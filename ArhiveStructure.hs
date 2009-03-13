----------------------------------------------------------------------------------------------------
---- Работа со структурой архивного файла.                                                    ------
---- Архив представляет из себя последовательность блоков, каждый из который может быть:      ------
----   * блоком данных                                                                        ------
----   * блоком каталога архива                                                               ------
----   * другим служебным блоком (recovery record, общая информация об архиве и т.д.)         ------
---- После каждого служебного блока (то есть любого, кроме блоков данных), записывается       ------
----   дескриптор, позволяющий разыскать и прочитать этот блок даже в случае сбоев            ------
----   в содержимом архива.                                                                   ------
---- Этот модуль содержит процедуры для:                                                      ------
----   * записи и чтения служебных блоков архива                                              ------
----   * записи, поиска и чтения дескрипторов служебных блоков                                ------
----------------------------------------------------------------------------------------------------
module ArhiveStructure where

import Prelude hiding (catch)
import Control.Monad
import Data.Word
import Data.Maybe
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Pool
import Foreign.Storable

import Utils
import Errors
import Files
import qualified ByteStream
import FileInfo
import Compression
import Encryption
import Options

-- |Сигнатура для поиска дескрипторов служебных блоков
aSIGNATURE = make4byte 65 114 67 1 :: Word32

-- |Сигнатура, записываемая в самое начало архива - по ней можно опознать архивный файл, + версия архиватора
aARCHIVE_SIGNATURE = (aSIGNATURE, aARCHIVE_VERSION)

-- |Сколько байт в конце архива сканировать в поиске сигнатуры, с которой начинается дескриптор последнего блока архива
aSCAN_MAX = 4096

-- Теги опциональных полей
aTAG_END = 0::Integer   -- ^тег окончания опциональных полей


----------------------------------------------------------------------------------------------------
---- Запись/чтение/поиск дескриптора служебного блока архива ---------------------------------------
----------------------------------------------------------------------------------------------------

-- |Записать в архив дескриптор служебного блока `block` и CRC этого дескриптора
archiveWriteBlockDescriptor block (receiveBuf,sendBuf) = do
  crc  <- ref aINIT_CRC
  let sendBuf_UpdatingCRC buf size len  =  do crc .<- updateCRC buf len;  sendBuf buf size len
  ByteStream.writeAll receiveBuf sendBuf_UpdatingCRC (return ())
    -- Структура блока каталога и любого другого управляющего блока - сначала идут упакованные данные, затем его дескриптор:
    --   сигнатура, тип блока, использованный компрессор, размер в распакованном и упакованном виде, CRC распакованных данных
    (aSIGNATURE, blType block, blCompressor block, blOrigSize block, blCompSize block, blCRC block)
  acrc <- val crc
  -- После дескриптора записывается CRC самого дескриптора
  ByteStream.writeAll receiveBuf sendBuf (return ())
    (finishCRC acrc)

-- |Прочитать из буфера дескриптор служебного блока и декодировать его с учётом того,
-- что в архиве этот дескриптор находится по смещению `arcpos`
archiveReadBlockDescriptor archive arcpos buf bufsize = do
  -- Проверим сначала CRC самого дескриптора
  right_crc  <- peekByteOff buf (bufsize - sizeOf (undefined::CRC))
  descriptor_crc <- calcCRC buf (bufsize - sizeOf (undefined::CRC))
  if (descriptor_crc/=right_crc)
    then return$ Left$ BROKEN_ARCHIVE (archiveName archive) ["0354 block descriptor at pos %1 is corrupted", show arcpos]
    else do
  -- Расшифруем содержимое дескриптора:
  -- сигнатуру, тип блока, использованный компрессор, размер в распакованном и упакованном виде, CRC распакованных данных
  (sign, block_type, compressor, origsize, compsize, crc)  <-  ByteStream.readMemory buf bufsize
  let pos   =  blDecodePosRelativeTo arcpos compsize  -- позиция в архиве начала блока
      block =  ArchiveBlock archive block_type compressor pos origsize compsize crc undefined (enc compressor)
  if (sign/=aSIGNATURE || pos<0)
    then return$ Left$ BROKEN_ARCHIVE (archiveName archive) ["0355 %1 is corrupted", block_name block]
    else return$ Right block

{-# NOINLINE archiveWriteBlockDescriptor #-}
{-# NOINLINE archiveReadBlockDescriptor #-}


----------------------------------------------------------------------------------------------------
---- Сканирование испорченного архива в поисках уцелевших блоков -----------------------------------
----------------------------------------------------------------------------------------------------

-- |Сканирование архива в поисках уцелевших блоков.
-- Создаёт их список и возвращает псевдо-FOOTER BLOCK, содержащий все найденные блоки.
-- Процедура читает архив с конца кусками по 8 мб, ищет дескрипторы блоков в этих кусках
-- и затем проверяет, что дескриптор и сам блок в порядке.
findBlocksInBrokenArchive arcname = do
  archive <- archiveOpen arcname
  arcsize <- archiveGetSize archive
  -- Выделяем буфер в 8 мб + 2 раза по 4 кбайт для упрощения реализации поиска дескриптора
  allocaBytes (aHUGE_BUFFER_SIZE+2*aSCAN_MAX) $ \buf -> do
  blocks <- withList $ scanArchiveSearchingDescriptors archive buf arcsize
  if null blocks
    then registerError$ BROKEN_ARCHIVE arcname ["0356 archive directory not found"]
    else do
  -- Возвратить нормальный FOOTER_BLOCK, если он нашёлся в архиве
  --if blType (head blocks) == FOOTER_BLOCK
  --  then return (archive, (head blocks) {ftBlocks=reverse blocks})
  --  else do
  let pseudo_footer = FooterBlock
        { ftBlocks   = reverse blocks
        , ftLocked   = False
        , ftComment  = ""
        , ftRecovery = ""
        , ftSFXSize  = minimum (map blPos blocks)   -- Размер SFX = позиции HEADER_BLOCK в архиве
        }
  return (archive, pseudo_footer)

-- Найти в архиве управляющие блоки, чьи дескрипторы начинаются
-- по адресам <pos, и занести эти дескрипторы в список found
scanArchiveSearchingDescriptors archive buf pos found =
  when (pos>0) $ do
    -- Для ускорения чтения архива округляем требуемый адрес вниз до 8-мбайтной границы
    -- и ищем дескриптор начиная с этой границы base_pos до позиции pos-1
    let base_pos = (pos-1) `roundDown` aHUGE_BUFFER_SIZE
    archiveSeek archive base_pos
    -- Читаем на 4 кбайта больше необходимого чтобы иметь возможность проверить
    -- дескриптор, начинающийся в самом конце буфера (длина дескриптора заведомо меньше 4 кбайт)
    len <- archiveReadBuf archive buf (i$ pos-base_pos+aSCAN_MAX)
    memset (buf+:len) 0 aSCAN_MAX  -- на всякий случай дополним прочитанное 4096-ю нулями
    -- scanMem возвращает позицию в архиве, после которой все дескрипторы уже обнаружены
    newpos <- scanMem archive base_pos found buf ((i$ pos-base_pos) `min` len)
    -- Вызываем функцию рекурсивно для поиска дескрипторов в предыдущем блоке
    scanArchiveSearchingDescriptors archive buf newpos found

-- Сканировать буфер buf в поисках дескрипторов, начинающихся в первых len байтах этого буфера
scanMem archive base_pos found buf len = do
  pos' <- ref base_pos
  whenRightM_ (archiveFindBlockDescriptor archive base_pos buf (len+aSCAN_MAX) len) $ \block -> do
    -- Найден дескриптор блока block. Значит, дальнейший поиск может быть ограничен
    -- его позицией, а если это блок каталога - то позицией первого блока данных в нём
    pos' =: blPos block
    -- Возможно, не стоит этого делать, поскольку архив может содержат "перепутавшиеся сектора"
    -- и соответственно, на месте данных вроде бы этого блока вполне могут оказаться совсем
    -- другие блоки. С другой же стороны, эти данные могут содержать просто другой архив в несжатом
    -- виде, и тогда мы включим его блоки в список блоков нашего архива :)
    -- Как вариант - "дотошность поиска" может регулироваться опцией
    -- when (blType block == DIR_BLOCK) $ do
    --   data_blocks <- archiveReadDir_OnlyBlocks
    --   pos' =: minpos data_blocks
    found <<= block
  -- Если буфер содержит ещё не просканированные данные, то продолжить поиск в нём,
  -- иначе - перейти к предыдущему куску архива
  pos <- val pos'
  if pos > base_pos
    then scanMem archive base_pos found buf (i$ pos-base_pos)
    else return pos

-- |Попытаться найти (последний) дескриптор блока в переданном буфере.
-- Общий объём данных в блоке - size, но при этом нас интересуют только дескрипторы,
-- начинающиеся в первых len байтах блока.
archiveFindBlockDescriptor archive base_pos buf size len =
  go ((size-sizeOf(aSIGNATURE)) `max` (len-1)) defaultError
    where
  go pos err | pos<0     = return$ Left err
             | otherwise = do
       x <- peekByteOff buf pos
       if x==aSIGNATURE
         then do -- Найдена сигнатура дескриптора по адресу pos, проверяем реальный ли это дескриптор
                 res <- archiveReadBlockDescriptor
                          archive            -- файл архива
                          (base_pos+i pos)   -- позиция в архивном файле декодируемого дескриптора
                          (buf+:pos)         -- адрес в памяти декодируемого дескриптора
                          (size-pos)         -- максимально возможный размер этого дескриптора
                 case res of
                   Left  err -> go (pos-1) err
                   Right _   -> return res
         else go (pos-1) err
  -- Сообщение об ошибке, возращаемое если в блоке вообще не найдено ни одного дескритора
  defaultError = BROKEN_ARCHIVE (archiveName archive) ["0357 archive signature not found at the end of archive"]

{-# NOINLINE findBlocksInBrokenArchive #-}
{-# NOINLINE scanArchiveSearchingDescriptors #-}
{-# NOINLINE scanMem #-}
{-# NOINLINE archiveFindBlockDescriptor #-}


----------------------------------------------------------------------------------------------------
---- Запись начального блока архива (HEADER_BLOCK) -------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Записать в начальный блок архива (HEADER_BLOCK) сигнатуру архива
archiveWriteHeaderBlock (receiveBuf,sendBuf) = do
  ByteStream.writeAll receiveBuf sendBuf (return ()) $
    aARCHIVE_SIGNATURE


----------------------------------------------------------------------------------------------------
---- Запись и чтение RECOVERY блока ----------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Записать RECOVERY блок
archiveWriteRecoveryBlock :: (ByteStream.BufferData a) =>  Maybe a -> Ptr CChar -> Int -> (ByteStream.RecvBuf, ByteStream.SendBuf) -> IO ()
archiveWriteRecoveryBlock moreinfo buf size (receiveBuf,sendBuf) = do
  stream <- ByteStream.create receiveBuf sendBuf (return ())
  case moreinfo of
    Just info -> ByteStream.write stream info
    Nothing   -> return ()
  ByteStream.writeBuf stream buf size
  ByteStream.closeOut stream


----------------------------------------------------------------------------------------------------
---- Запись и чтение финального блока архива (FOOTER_BLOCK) ------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Информация, содержащаяся в FOOTER BLOCK
data FooterBlock = FooterBlock
       { ftBlocks     :: ![ArchiveBlock]     -- список блоков в архиве (за исключением блоков данных)
       , ftLocked     :: !Bool               -- архив закрыт от изменений?
       , ftComment    :: !String             -- комментарий архива
       , ftRecovery   :: !String             -- настройка recovery info
       , ftSFXSize    :: !FileSize           -- размер SFX-модуля, предшествующего собственно архиву (вычисляется как объём данных, предшествующих первому блоку архива)
       }

-- |Записать FOOTER_BLOCK
archiveWriteFooterBlock control_blocks arcLocked arcComment (arcRecovery::String) arcpos (receiveBuf,sendBuf) = do
  stream <- ByteStream.create receiveBuf sendBuf (return ())
  let utf8comment  =  ByteStream.toUTF8List arcComment
  ByteStream.write        stream (map (blockToTuple arcpos) control_blocks)   -- запишем описания управляющих блоков,
  ByteStream.write        stream arcLocked                                    -- ... признак закрытия архива от изменений
  ByteStream.writeInteger stream 0                                            -- ... комментарий архива в старом формате - отсутствует
  ByteStream.write        stream arcRecovery                                  -- ... объём recovery инормации
  ByteStream.writeInteger stream (length utf8comment)                         -- ... комментарий архива (кодируем как список, поскольку при этом явно кодируется длина и комментарий таким образом может содержать нулевые символы)
  ByteStream.writeList    stream utf8comment                                  --     -.-
  ByteStream.closeOut     stream

-- |Прочитать информацию из FOOTER_BLOCK
archiveReadFooterBlock footer @ ArchiveBlock {
                                   blArchive  = archive
                                 , blType     = block_type
                                 , blPos      = pos
                                 , blOrigSize = origsize
                               }
                       decryption_info = do
  when (block_type/=FOOTER_BLOCK) $
    registerError$ BROKEN_ARCHIVE (archiveName archive) ["0358 last block of archive is not footer block"]
  withPool $ \pool -> do   -- используем пул памяти, чтобы автоматически освободить выделенные буферы при выходе
    (buf,size) <- archiveBlockReadAll pool decryption_info footer  -- поместим в буфер распакованные данные блока
    stream <- ByteStream.openMemory buf size
    control_blocks <- ByteStream.read stream      -- прочитаем описания управляющих блоков,
    locked         <- ByteStream.read stream      -- ... признак закрытия архива от изменений
    oldComment     <- ByteStream.readInteger stream >>= ByteStream.readList stream >>== map (toEnum.i :: Word32 -> Char)  -- ... и комментарий архива (читаем как список, поскольку при этом явно кодируется длина и комментарий таким образом может содержать нулевые символы)
    isEOF          <- ByteStream.isEOFMemory stream  -- Старая версия программы не записывала информацию о recovery record
    recovery       <- not isEOF &&& ByteStream.read stream  -- ... настройку RECOVERY информации, добавляемой к архиву
    isEOF          <- ByteStream.isEOFMemory stream
    comment        <- not isEOF &&& (ByteStream.readInteger stream >>= ByteStream.readList stream >>== ByteStream.fromUTF8)  -- ... и комментарий архива (читаем как список, поскольку при этом явно кодируется длина и комментарий таким образом может содержать нулевые символы)
    ByteStream.closeIn stream
    let blocks = map (tupleToBlock archive pos) control_blocks   -- сконструируем структуры ArchiveBlock из прочитаных данных
    return FooterBlock
             { ftBlocks   = blocks++[footer]
             , ftLocked   = locked
             , ftComment  = comment ||| oldComment
             , ftRecovery = recovery
             , ftSFXSize  = minimum (map blPos blocks)   -- Размер SFX = позиции HEADER_BLOCK в архиве
             }

{-# NOINLINE archiveWriteFooterBlock #-}
{-# NOINLINE archiveReadFooterBlock #-}


----------------------------------------------------------------------------------------------------
---- Блок архива (блок данных, каталог или служебный) ----------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Блок архива
data ArchiveBlock = ArchiveBlock
       { blArchive     :: !Archive      -- архив, к которому принадлежит данный блок
       , blType        :: !BlockType    -- тип блока
       , blCompressor  :: !Compressor   -- метод сжатия
       , blPos         :: !FileSize     -- позиция блока в файле архива
       , blOrigSize    :: !FileSize     -- размер блока в распакованном виде
       , blCompSize    :: !FileSize     -- размер блока в упакованном виде
       , blCRC         :: !CRC          -- CRC распакованных данных (только для служебных блоков)
       , blFiles       ::  Int          -- количество файлов (только для блоков данных)
       , blIsEncrypted ::  Bool         -- блок зашифрован?
       }

instance Eq ArchiveBlock where
  (==)  =  map2eq$ map2 (blPos, archiveName.blArchive)

-- |Вспомогательная функция для вычисления поля blIsEncrypted по blCompressor
enc = any isEncryption

-- |Для хранения в архиве информации об архивных блоках. Позиция блока записывается в архив
-- относительно `arcpos` - позиции в архиве того блока, в котором сохраняется эта информация
blockToTuple              arcpos (ArchiveBlock _ t c p o s crc f e) = (t,c,arcpos-p,o,s,crc)
tupleToBlock     archive arcpos (t,c,p,o,s,crc) = (ArchiveBlock archive t c (arcpos-p) o s crc undefined (enc c))
tupleToDataBlock archive arcpos   (c,p,o,s,f)   = (ArchiveBlock archive DATA_BLOCK c (arcpos-p) o s 0 f (enc c))

-- Отдельные функции для (де)кодирования позиции блока относительно другого места в архиве
blEncodePosRelativeTo arcpos arcblock  =  arcpos - blPos arcblock
blDecodePosRelativeTo arcpos offset    =  arcpos - offset

-- |Описание блока
block_name block  =  (case (blType block) of
                          DESCR_BLOCK    -> "block descriptor"
                          HEADER_BLOCK   -> "header block"
                          DATA_BLOCK     -> "data block"
                          DIR_BLOCK      -> "directory block"
                          FOOTER_BLOCK   -> "footer block"
                          RECOVERY_BLOCK -> "recovery block"
                          _              -> "block of unknown type"
                      ) ++ " at pos "++ show (blPos block)

-- |Тип блока архива (значения добавлять только в конец, поскольку они записываются в архив!)
data BlockType = DESCR_BLOCK       -- ^Тэг дескриптора блока архива  (записывается после каждого служебного блока, то есть всех, кроме DATABLOCK)
               | HEADER_BLOCK      -- ^Тэг начального блока архива   (используемого как сигнатура архивного файла)
               | DATA_BLOCK        -- ^Тэг блока данных
               | DIR_BLOCK         -- ^Тэг блока каталога
               | FOOTER_BLOCK      -- ^Тэг конечного блока архива    (содержащего список блоков в архиве)
               | RECOVERY_BLOCK    -- ^Тэг блока с recovery info
               | UNKNOWN_BLOCK     -- Реально неиспользуемые варианты для всех неподдерживаемых этой версией программы типов блока
               | UNKNOWN_BLOCK2
               | UNKNOWN_BLOCK3
     deriving (Eq,Enum)

instance ByteStream.BufferData BlockType  where
  write buf = ByteStream.writeInteger buf . fromEnum
  read  buf = ByteStream.readInteger  buf >>== toEnum

-- Операции с блоками архива
archiveBlockSeek    block pos       =  archiveSeek    (blArchive block) (blPos block + pos)
archiveBlockRead    block size      =  archiveRead    (blArchive block) size
archiveBlockReadBuf block buf size  =  archiveReadBuf (blArchive block) buf size

-- |Выделить буфер, прочитать в него содержимое блока и проверить CRC
archiveBlockReadAll pool
                    decryption_info
                    block @ ArchiveBlock {
                              blArchive     = archive
                            , blType        = block_type
                            , blCompressor  = compressor
                            , blPos         = pos
                            , blCRC         = right_crc
                          } = do
  let origsize = i$ blOrigSize block
      compsize = i$ blCompSize block
  (origbuf, decompressed_size)  <-  decompressInMemory pool compressor decryption_info archive pos compsize origsize
  crc <- calcCRC origbuf origsize
  when (crc/=right_crc || decompressed_size/=origsize) $ do
    registerError$ BROKEN_ARCHIVE (archiveName archive) ["0359 %1 failed decompression", block_name block]
  return (origbuf, origsize)

-- |Выделить буфер и прочитать в него содержимое блока. Не проверяет CRC и не распаковывает данные!
archiveBlockReadUnchecked pool block = do
  when (blCompressor block/=aNO_COMPRESSION) $ do
    registerError$ BROKEN_ARCHIVE (archiveName$ blArchive block) ["0360 %1 should be uncompressed", block_name block]
  archiveMallocReadBuf pool (blArchive block) (blPos block) (i$ blOrigSize block)

-- |Выделить буфер и прочитать в него данные из архива
archiveMallocReadBuf pool archive pos size = do
  buf         <- pooledMallocBytes pool (size+8)  -- +8 - из-за недоработок в ByteStream :(
  archiveSeek    archive pos
  archiveReadBuf archive buf size
  return buf

-- |Рпаспаковать в памяти блок, который может быть упакован несколькими алгоритмами и вдобавок зашифрован
decompressInMemory mainPool compressor decryption_info archive pos compsize origsize = do
  withPool $ \tempPool -> do
  let process srcbuf srcsize [] = return (srcbuf, srcsize)
      process srcbuf srcsize (algorithm:algorithms) = do
        let (dstsize, pool) = if null algorithms
                                then (origsize, mainPool)
                                else ((max compsize origsize)*2+100*kb, tempPool)
        dstbuf <- pooledMallocBytes pool (dstsize+8)  -- +8 - из-за недоработок в ByteStream :(
        decompressed_size <- decompressMem algorithm srcbuf srcsize dstbuf dstsize
        pooledReallocBytes tempPool srcbuf 0
        process dstbuf decompressed_size algorithms
  --
  if compressor==aNO_COMPRESSION
    then do compbuf <- archiveMallocReadBuf mainPool archive pos (compsize+8)
            return (compbuf, compsize)
    else do
  -- Дополнить алгоритмы шифрования ключами, необходимыми для расшифровки
  keyed_compressor <- generateDecryption compressor decryption_info
  when (any isNothing keyed_compressor) $ do
    registerError$ BAD_PASSWORD (archiveName archive) ""
  -- Прочитать исходный блок из архива
  compbuf <- archiveMallocReadBuf tempPool archive pos compsize
  process compbuf compsize (reverse (map fromJust keyed_compressor))


{-# NOINLINE archiveBlockReadAll #-}
{-# NOINLINE archiveMallocReadBuf #-}

