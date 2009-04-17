----------------------------------------------------------------------------------------------------
---- Процесс формирования структуры архива и чтения упаковываемых данных.                       ----
---- Вызывается из ArcCreate.hs                                                                 ----
----------------------------------------------------------------------------------------------------
module ArcvProcessRead where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import Data.IORef
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Pool
import Foreign.Marshal.Utils

import Utils
import Files
import Process
import Errors
import FileInfo
import Compression
import Options
import UI
import ArhiveStructure
import ArhiveDirectory
import ArhiveFileList
import ArcvProcessExtract


-- |Инструкции, посылаемые процессом чтения входных данных процессу упаковки
data Instruction
  =   DebugLog  String                        --   Вывод отладочного сообщения с отметкой времени
  |   DebugLog0 String                        --                               без отметки
  |   CompressData BlockType Compressor Compressor Bool
                                              --   Начало блока архива
  |     FileStart FileInfo                    --     Начало очередного файла
  |       DataChunk (Ptr CChar) Int           --       Очередная порция упаковываемых данных
  |     CorrectTotals FileCount FileSize      --     Корректировка Total Files/Bytes для UI
  |     FakeFiles [FileToCompress]            --     Корректировка списка уже упакованных файлов для UI
  |   CopySolidBlock [CompressedFile]         --   Копирование солид-блока целиком из существующего архива
  |   DataEnd                                 --   Конец блока архива
  |   Directory [FileWithCRC]                 --   Запрос на получение служебных данных о последнем созданном блоке архива
  | TheEnd                                    -- Создание архива завершено

-- |Предикаты для использования в циклах: выполнять до конца блока архива, выполнять до конца архива
notDataEnd DataEnd = False
notDataEnd _       = True
notTheEnd  TheEnd  = False
notTheEnd  _       = True


-- |Процесс, создающий структуру архива - он разбивает файлы
--   по отдельным томам архива, каталогам файлов внутри архива и солид-блокам.
-- Этот же процесс собирает входные данные для упаковки - читает файлы с диска и
--   распаковывает данные из входных архивов.
create_archive_structure_AND_read_files_PROCESS command archive oldarc files processDir arcComment writeRecoveryBlocks results backdoor pipe = do
  initPos <- archiveGetPos archive
  -- При возникновении ошибки установим флаг для прерывания работы c_compress()
  handleCtrlBreak "operationTerminated =: True" (operationTerminated =: True) $ do
  -- Создадим процесс для распаковки файлов из входных архивов и гарантируем его корректное завершение
  bracket (runAsyncP$ decompress_PROCESS command doNothing)
          ( \decompress_pipe -> do sendP decompress_pipe Nothing; joinP decompress_pipe)
          $ \decompress_pipe -> do
  -- Создадим кеш для упреждающего чтения архивируемых файлов
  withPool $ \pool -> do
  bufOps <- makeFileCache (opt_cache command) pool pipe
  -- Параметры для writeControlBlock
  let params = (command,bufOps,pipe,backdoor)

  -- Запишем блок заголовка (HEADER_BLOCK) в начало архива
  header_block  <-  writeControlBlock HEADER_BLOCK aNO_COMPRESSION params $ do
                      archiveWriteHeaderBlock bufOps

  -- Заархивируем данные, разбивая файлы по dir-блокам
  directory_blocks <- foreach (splitToDirBlocks command files)
                              (createDirBlock archive processDir decompress_pipe params)

  -- Запишем финальный блок (FOOTER_BLOCK), содержащий каталог служебных блоков архива и комментарий архива
  let write_footer_block blocks arcRecovery = do
          footerPos <- archiveGetPos archive
          writeControlBlock FOOTER_BLOCK (dir_compressor command) params $ do
            let lock_archive = opt_lock_archive command   -- Закрыть создаваемый архив от изменений?
            archiveWriteFooterBlock blocks lock_archive arcComment arcRecovery footerPos bufOps
          return ()
  write_footer_block (header_block:directory_blocks) ""

  -- Напечатаем статистику выполнения команды и сохраним её для возврата в вызывающую процедуру
  uiDoneArchive  >>=  writeIORef results

  -- Если запись RECOVERY информации включена - запишем RECOVERY блоки и повторим FOOTER блок
  (recovery_blocks,recovery) <- writeRecoveryBlocks archive oldarc initPos command params bufOps
  unless (null recovery_blocks) $ do
    write_footer_block (header_block:directory_blocks++recovery_blocks) recovery

  -- Уведомим процесс записи в архив, что создание архива завершено
  sendP pipe TheEnd


-- |Записать в архив переданные файлы и dir-блок с их описанием
createDirBlock archive processDir decompress_pipe params@(command,bufOps,pipe,backdoor) files = do
  -- Разбить файлы по солид-блокам и обработать каждый подсписок отдельно. Для отладки - mapM (print.map (fpFullname.fiDiskName.cfFileInfo)) (splitToSolidBlocks files)
  solidBlocks <- foreach (splitToSolidBlocks command files)
                         (createSolidBlock command processDir bufOps pipe decompress_pipe)
  -- Получить от процесса write_to_archive информацию о созданных солид-блоках и содержащихся в них файлах.
  -- Выполнение этой команды форсирует завершение упаковки всех ранее посланных данных и запись упакованных данных в архив...
  blocks_info  <-  replicateM (length solidBlocks) (getP backdoor)
  -- ... после чего можно быть уверенным, что текущая позиция в архиве - это позиция, где начнётся блок каталога
  dirPos <- archiveGetPos archive
  -- Записать блок каталога и возвратить информацию о нём для формирования каталога каталогов
  writeControlBlock DIR_BLOCK (dir_compressor command) params $ do
    archiveWriteDir blocks_info dirPos bufOps


-- |Создать солид-блок, содержащий данные из переданных файлов
createSolidBlock command processDir bufOps pipe decompress_pipe (orig_compressor,files) = do
  let -- Выберем алгоритм сжатия для этого солид-блока
      -- и уменьшим словари его алгоритмов, ежели они больше объёма данных в блоке
      -- (+1%+512 потому что фильтры типа delta могут увеличить объём данных):
      compressor | copy_solid_block = cfCompressor (head files)
                 | otherwise        = orig_compressor.$limitDictionary (clipToMaxMemSize$ roundMemUp$ totalBytes+(totalBytes `div` 100)+512)
      -- Общий объём файлов в солид-блоке
      totalBytes = sum$ map (fiSize.cfFileInfo) files
      -- True, если это целый солид-блок из входного архива, который можно скопировать без изменений
      copy_solid_block = not(opt_recompress command)  &&  isWholeSolidBlock files
  -- Ограничить компрессор объёмом свободной памяти и значением -lc
  real_compressor <- limit_compressor command compressor
  opt_testMalloc command &&& testMalloc

  -- Сжать солид-блок данных и отослать в следующий процесс список помещённых в него файлов
  unless (null files) $ do
  printDebugInfo command pipe files totalBytes copy_solid_block compressor real_compressor
  writeBlock pipe DATA_BLOCK compressor real_compressor copy_solid_block $ do
    dir <- -- Если солид-блок передаётся из архива в архив целиком, то обойти излишнюю процедуру перепаковки
           if copy_solid_block then do
             sendP pipe (CopySolidBlock files)
             return$ map fileWithCRC files
           -- Если используется --nodata, то обойти чтение входных файлов
           else if isReallyFakeCompressor compressor then do
             sendP pipe (FakeFiles files)
             return$ map fileWithCRC files
           -- Обычное чтение файлов для всех прочих (более типичных) случаев
           else do
             mapMaybeM (read_file command pipe bufOps decompress_pipe) files
    processDir dir   -- дать процедуре, переданной сверху, пощупать список сархивированных файлов (используется для реализации опций -tl, -ac, -d[f])
    return dir


-- |Напечатать отладочную информацию
printDebugInfo command pipe files totalBytes copy_solid_block compressor real_compressor = do
  --print (clipToMaxInt totalBytes, compressor)
  --print$ map (diskName.cfFileInfo) files   -- debugging tool :)
  when (opt_debug command) $ do
    sendP pipe$ DebugLog$  "Compressing "++show_files3(length files)++" of "++show_bytes3 totalBytes
    sendP pipe$ DebugLog0$ if copy_solid_block then "  Copying "++join_compressor compressor  else "  Using "++join_compressor real_compressor
    unless (copy_solid_block) $ do
      sendP pipe$ DebugLog0$ "  Memory for compression "++showMem (getCompressionMem   real_compressor)
                                    ++", decompression "++showMem (getDecompressionMem real_compressor)


---------------------------------------------------------------------------------------------------
---- Процедура чтения данных упаковываемого файла -------------------------------------------------
---------------------------------------------------------------------------------------------------

{-# NOINLINE read_file #-}
-- Если это каталог, то пропускаем чтение данных
read_file command pipe _ _ file  | fi<-cfFileInfo file, fiIsDir fi = do
  sendP pipe (FileStart fi)
  return$ Just$ fileWithCRC file

-- Если это файл на диске, то прочитаем его по частям, отправляя прочитанные блоки на упаковку
read_file _ pipe (receiveBuf, sendBuf) _ (DiskFile old_fi) = do
  -- Операция информирования следующего процесса об изменении размера/количества файлов, которое он должен отослать в UI
  let correctTotals files bytes  =  when (files/=0 || bytes/=0) (sendP pipe (CorrectTotals files bytes)) >> return Nothing
  -- Проверяем возможность открыть файл - он может быть залочен или его за это время могли элементарно стереть :)
  tryOpen (diskName old_fi)  >>=  maybe (correctTotals (-1) (-fiSize old_fi))  (\file -> do
  ensureCtrlBreak "fileClose:read_file" (fileClose file) $ do  -- Гарантируем закрытие файла
  -- Перечитаем информацию о файле на случай, если он успел измениться
  rereadFileInfo old_fi file >>=  maybe (correctTotals (-1) (-fiSize old_fi))  (\fi -> do
  correctTotals 0 (fiSize fi - fiSize old_fi) -- Откорректируем показания UI, если размер файла успел измениться
  sendP pipe (FileStart fi)                   -- Проинформируем пользователя о начале упаковки файла
  let readFile crc bytes = do    -- Прочитаем в цикле файл, отправляя прочитанные блоки на упаковку:
        (buf, size) <- receiveBuf                -- Получим свободный буфер из очереди буферов
        len         <- fileReadBuf file buf size -- Прочитаем в него очередную порцию данных из файла
        newcrc      <- updateCRC buf len crc     -- Обновим CRC содержимым буфера
        sendBuf        buf size len              -- Отошлём данные процессу упаковки
        if len>0
          then readFile newcrc $! bytes+i len    -- Обновим счётчик прочитанных байт
          else return (finishCRC newcrc, bytes)  -- Выйдем из цикла, если файл окончился
  (crc,bytesRead) <- readFile aINIT_CRC 0     -- Прочитаем файл, получив его CRC и размер
  correctTotals 0 (bytesRead - fiSize fi)     -- Откорректируем показания UI, если размер файла отличается от возвращённого getFileInfo
  return$ Just$ FileWithCRC crc FILE_ON_DISK fi{fiSize=bytesRead} ))

-- Если это файл из уже существующего архива, то распакуем его, отправляя распакованные блоки на упаковку
read_file _ pipe (receiveBuf, sendBuf) decompress_pipe compressed_file = do
  crc  <-  ref aINIT_CRC                       -- Инициализируем значение CRC
  -- Операция "записи" распакованных данных путём копирования их в собственные буфера
  -- и отсылки этих буферов на последующую обработку
  let writer inbuf 0 = send_backP decompress_pipe ()  -- сообщим распаковщику, что теперь буфер свободен
      writer inbuf insize = do
        (buf, size) <- receiveBuf              -- получим свободный буфер из очереди буферов
        let len  = min insize size             -- определим сколько данных мы можем обработать
        crc    .<- updateCRC inbuf len         -- обновим CRC содержимым буфера
        copyBytes  buf inbuf len               -- скопируем данные в полученный буфер
        sendBuf    buf size len                -- пошлём их следующему процессу в транспортёре
        writer     (inbuf+:len) (insize-len)   -- обработаем оставшиеся данные, если есть
  let fi  =  cfFileInfo compressed_file
  sendP pipe (FileStart fi)                    -- Проинформируем пользователя о начале перепаковки файла
  decompress_file decompress_pipe compressed_file writer   -- Распаковать файл в отдельном треде
  crc'  <-  val crc >>== finishCRC            -- Вычислим окончательное значение CRC
  if cfCRC compressed_file == crc'            -- Если CRC в порядке
    then return$ Just$ fileWithCRC compressed_file  -- то возвратим информацию о файле
    else registerError$ BAD_CRC$ diskName fi        -- иначе вывалимся с ошибкой


---------------------------------------------------------------------------------------------------
---- Вспомогательные определения ------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Создать кеш для упреждающего чтения и возвратить процедуры receiveBuf и sendBuf
-- для получения свободного буфера из кеша и освобождения использованного буфера, соответственно
makeFileCache cache_size pool pipe = do
  -- Размер буферов, на которые будет разбит весь кеш
  let bufsize | cache_size>=aLARGE_BUFFER_SIZE*16  =  aLARGE_BUFFER_SIZE
              | otherwise                          =  aBUFFER_SIZE
  -- Выделить память под кеш и натравить memoryAllocator на выделенный блок памяти
  heap                     <-  pooledMallocBytes pool cache_size
  (getBlock, shrinkBlock)  <-  memoryAllocator   heap cache_size bufsize 256 (receive_backP pipe)
  let -- Операция получения свободного буфера
      receiveBuf            =  do buf <- getBlock
                                  failOnTerminated
                                  return (buf, bufsize)
      -- Операция отправления заполненного буфера следующему процессу
      sendBuf buf size len  =  do shrinkBlock buf len
                                  failOnTerminated
                                  when (len>0)$  do sendP pipe (DataChunk buf len)
  return (receiveBuf, sendBuf)

{-# NOINLINE writeBlock #-}
-- |Записать в архив блок данных/служебный/дескриптор блока
writeBlock pipe blockType compressor real_compressor just_copy action = do
  sendP pipe (CompressData blockType compressor real_compressor just_copy)
  directory <- action
  sendP pipe  DataEnd
  sendP pipe (Directory directory)

{-# NOINLINE writeControlBlock #-}
-- Записать в архив служебный блок вместе с его дескриптором и возвратить информацию об этом блоке
writeControlBlock blockType compressor (command,bufOps,pipe,backdoor) action = do
  if (opt_nodir command)   -- Опция "--nodir" отключает запись в архив всех служебных блоков - остаются только сами сжатые данные
    then return (error "Attempt to use value returned by writeControlBlock when \"--nodir\"")
    else do
  writeBlock pipe blockType compressor compressor False $ do  -- запишем в архив блок каталога
    action; return []
  (thisBlock, [])  <-  getP backdoor                      -- получим его дескриптор
  writeBlock pipe DESCR_BLOCK aNO_COMPRESSION aNO_COMPRESSION False $ do  -- запишем этот дескриптор в архив
    archiveWriteBlockDescriptor thisBlock bufOps; return []
  (_, [])  <-  getP backdoor                              -- оприходуем ненужный дескриптор дескриптора
  return thisBlock                                        -- возвратим дескриптор блока каталога

