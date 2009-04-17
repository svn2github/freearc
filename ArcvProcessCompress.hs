----------------------------------------------------------------------------------------------------
---- Процесс упаковки данных и служебной информации архива, и записи упакованных данных в архив.----
---- Вызывается из ArcCreate.hs                                                                 ----
----------------------------------------------------------------------------------------------------
module ArcvProcessCompress where

import Prelude hiding (catch)
import Control.Monad
import Data.IORef
import Foreign.C.Types
import Foreign.Ptr

import Utils
import Files
import Errors
import Process
import FileInfo
import Compression
import Encryption
import Options           (opt_data_password, opt_headers_password, opt_encryption_algorithm)
import UI
import ArhiveStructure
import ArhiveDirectory
import ArcvProcessExtract
import ArcvProcessRead


-- |Процесс упаковки данных и служебной информации архива, и записи упакованных данных в архив.
-- Также возвращает через backdoor служебную информацию о блоках, созданных при записи архива
compress_AND_write_to_archive_PROCESS archive command backdoor pipe = do

  -- Процедура отображения в UI входных данных
  let display (FileStart fi)               =  uiStartFile      fi
      display (DataChunk buf len)          =  uiUnpackedBytes  (i len)
      display (CorrectTotals files bytes)  =  uiCorrectTotal   files bytes
      display (FakeFiles cfiles)           =  uiFakeFiles      (map cfFileInfo cfiles) 0
      display _                            =  return ()

  -- Процедура записи упакованных данных в архив
  let write_to_archive (DataBuf buf len) =  do uiCompressedBytes  (i len)
                                               archiveWriteBuf    archive buf len
                                               return len
      write_to_archive  NoMoreData       =  return 0

  -- Процедура копирования целиком солид-блока из входного архива в выходной без переупаковки
  let copy_block = do
        CopySolidBlock files <- receiveP pipe
        let block       = (cfArcBlock (head files))
        uiFakeFiles       (map cfFileInfo files)  (blCompSize block)
        archiveCopyData   (blArchive block) (blPos block) (blCompSize block) archive
        DataEnd <- receiveP pipe
        return ()

  repeat_while (receiveP pipe) (notTheEnd) $ \msg -> case msg of
    DebugLog str -> do   -- Напечатать отладочное сообщение
        debugLog str
    DebugLog0 str -> do
        debugLog0 str
    CompressData block_type compressor real_compressor just_copy -> mdo
        case block_type of             -- Сообщим UI какого типа данные сейчас будут паковаться
            DATA_BLOCK  ->  uiStartFiles (length real_compressor)
            DIR_BLOCK   ->  uiStartDirectory
            _           ->  uiStartControlData
        result <- ref 0   -- количество байт, записанных в последнем вызове write_to_archive

        -- Подсчёт CRC (только для служебных блоков) и количества байт в неупакованных данных блока
        crc      <- ref aINIT_CRC
        origsize <- ref 0
        let update_crc (DataChunk buf len) =  do when (block_type/=DATA_BLOCK) $ do
                                                     crc .<- updateCRC buf len
                                                 origsize += i len
            update_crc _                   =  return ()

        -- Выясним, нужно ли шифрование для этого блока
        let useEncryption = password>""
            password = case block_type of
                         DATA_BLOCK     -> opt_data_password command
                         DIR_BLOCK      -> opt_headers_password command
                         FOOTER_BLOCK   -> opt_headers_password command
                         DESCR_BLOCK    -> ""
                         HEADER_BLOCK   -> ""
                         RECOVERY_BLOCK -> ""
                         _              -> error$ "Unexpected block type "++show (fromEnum block_type)++" in compress_AND_write_to_archive_PROCESS"
            algorithm = command.$ opt_encryption_algorithm

        -- Если для этого блока нужно использовать шифрование, то добавить алгоритм шифрования
        -- к цепочке методов сжатия. В реально вызываемый алгоритм шифрования передаётся key и initVector,
        -- а в архиве запоминаются salt и checkCode, необходимый для быстрой проверки пароля
        (add_real_encryption, add_encryption_info) <- if useEncryption
                                                         then generateEncryption algorithm password   -- not thread-safe due to use of PRNG!
                                                         else return (id,id)

        -- Процесс упаковки одним алгоритмом
        let compressP = de_compress_PROCESS freearcCompress times
        -- Последовательность процессов упаковки, соответствующая последовательности алгоритмов `real_compressor`
        let real_crypted_compressor = add_real_encryption real_compressor
            processes = zipWith compressP real_crypted_compressor [1..]
            compressa = case real_crypted_compressor of
                          [_] -> storing_PROCESS |> last processes
                          _   -> storing_PROCESS |> foldl1 (|>) (init processes) |> last processes
        -- Процедура упаковки, вызывающая процесс упаковки со всеми необходимыми процедурами для получения/отправки данных
        let compress_block  =  runFuncP compressa (do x<-receiveP pipe; display x; update_crc x; return x)
                                                  (send_backP pipe)
                                                  (write_to_archive .>>= writeIORef result)
                                                  (val result)
        -- Выбрать между процедурой упаковки и процедурой копирования целиком солид-блока из входного архива
        let compress_f  =  if just_copy  then copy_block  else compress_block

        -- Упаковать один солид-блок
        pos_begin <- archiveGetPos archive
        ; times <- uiStartDeCompression "compression"              -- создать структуру для учёта времени упаковки
        ;   compress_f                                             -- упаковать данные
        ; uiFinishDeCompression times `on` block_type==DATA_BLOCK  -- учесть в UI чистое время операции
        ; uiUpdateProgressIndicator 0                              -- отметить, что прочитанные данные уже обработаны
        pos_end   <- archiveGetPos archive

        -- Возвратить в первый процесс информацию о только что созданном блоке
        -- вместе со списком содержащихся в нём файлов
        (Directory dir)  <-  receiveP pipe   -- Получим от первого процесса список файлов в блоке
        crc'             <-  val crc >>== finishCRC     -- Вычислим окончательное значение CRC
        origsize'        <-  val origsize
        putP backdoor (ArchiveBlock {
                           blArchive     = archive
                         , blType        = block_type
                         , blCompressor  = compressor .$(not just_copy &&& add_encryption_info) .$compressionDeleteTempCompressors
                         , blPos         = pos_begin
                         , blOrigSize    = origsize'
                         , blCompSize    = pos_end-pos_begin
                         , blCRC         = crc'
                         , blFiles       = error "undefined ArchiveBlock::blFiles"
                         , blIsEncrypted = error "undefined ArchiveBlock::blIsEncrypted"
                       }, dir)


{-# NOINLINE storing_PROCESS #-}
-- |Вспомогательный процесс, перекодирующий поток Instruction в поток CompressionData
storing_PROCESS pipe = do
  let send (DataChunk buf len)  =  failOnTerminated  >>  resend_data pipe (DataBuf buf len)  >>  send_backP pipe (buf,len)
      send  DataEnd             =  resend_data pipe NoMoreData >> return ()
      send _                    =  return ()

  -- По окончании сообщим следующему процессу, что данных больше нет
  ensureCtrlBreak "send DataEnd" (send DataEnd)$ do
    -- Цикл перекодирования инструкций
    repeat_while (receiveP pipe) (notDataEnd) (send)
  return ()

