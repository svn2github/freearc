----------------------------------------------------------------------------------------------------
---- Реализация команд распаковки и получения листинга архива                                   ----
----------------------------------------------------------------------------------------------------
module ArcExtract ( runArchiveExtract
                  , runArchiveList
                  , runCommentWrite
                  , formatDateTime
                  ) where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import Data.List
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Numeric
import System.IO.Unsafe
import System.Posix.Internals

import Process
import Utils
import Files
import FileInfo
import Charsets            (i18n)
import Errors
import Compression         (aINIT_CRC, updateCRC, finishCRC, join_compressor)
import Options
import UI
import ArhiveStructure
import ArhiveDirectory
import ArcvProcessExtract

-- |Обобщённая команда распаковки архива
runArchiveExtract pretestArchive
                  command@Command{ cmd_arcname        = arcname
                                 , cmd_archive_filter = archive_filter
                                 , opt_arc_basedir    = arc_basedir
                                 , opt_disk_basedir   = disk_basedir
                                 , opt_arccmt_file    = arccmt_file
                                 , opt_unParseFile    = unParseFile
                                 } = do
    -- Суперэкономия памяти: find_archives -> buffer 10_000 -> read_dir -> buffer 10_000 -> arcExtract
  doFinally uiDoneArchive2 $ do
  uiStartArchive command []  -- сообщить пользователю о начале обработки очередного архива
  uiStage "0249 Reading archive directory"
  command <- (command.$ opt_cook_passwords) command ask_passwords  -- подготовить пароли в команде к использованию
  let openArchive = archiveReadInfo command arc_basedir disk_basedir archive_filter (pretestArchive command)
  bracketCtrlBreak "arcClose:ArcExtract" (openArchive arcname) (arcClose)$ \archive -> do
    uiPrintArcComment (arcComment archive)            -- Напечатать комментарий
    when (arccmt_file/="-" && arccmt_file/="--") $    -- и записать его в файл, указанный опцией -z
      unParseFile 'c' arccmt_file (arcComment archive)
    arcExtract command archive
  uiDoneArchive  -- напечатать и вернуть в вызывающую процедуру статистику выполнения команды

-- |Распаковка архива
arcExtract command arcinfo = do
  -- Процедура, используемая для обработки каждого файла
  let process_file = case (cmd_name command) of
                       "t"  -> test_file
                       _    -> extract_file (fpFullname.fiDiskName) command
  -- Отобразить в UI общий объём распаковываемых файлов и объём уже распакованного каталога архива
  uiStartProcessing (map cfFileInfo (arcDirectory arcinfo))  (arcDataBytes arcinfo)  (arcDataCBytes arcinfo)
  uiStartDirectory
  uiUnpackedBytes   (arcDirBytes  arcinfo)
  uiCompressedBytes (arcDirCBytes arcinfo)
  uiStartFiles 0
  -- Создадим процесс для распаковки файлов и гарантируем его корректное завершение
  bracket (runAsyncP$ decompress_PROCESS command (uiCompressedBytes.i))
          ( \decompress_pipe -> do sendP decompress_pipe Nothing; joinP decompress_pipe)
          $ \decompress_pipe -> do
  -- Распаковать каждый распаковываемый файл и выругаться на нераспаковываемые
  let (filesToSkip, filesToExtract)  =  partition isCompressedFake (arcDirectory arcinfo)
  for filesToExtract (process_file decompress_pipe)   -- runP$ enum_files |> decompress |> write_files
  unless (null filesToSkip)$  do registerWarning$ SKIPPED_FAKE_FILES (length filesToSkip)

-- |Тестирование одного файла из архива
test_file decompress_pipe compressed_file = do
  uiStartFile (cfFileInfo compressed_file)
  run_decompress decompress_pipe compressed_file (\buf size -> return ())
  return ()

-- |Распаковка одного файла из архива
extract_file filename_func command decompress_pipe compressed_file = do
  let fileinfo  = cfFileInfo compressed_file
      filename  = filename_func fileinfo
  if (fiIsDir fileinfo)
    then do uiStartFile fileinfo
            createDirectoryHierarchy filename
    else do
  -- Продолжить при условии, что этот файл позволено распаковать
  whenM (can_be_extracted command filename fileinfo)$ do
    uiStartFile fileinfo
    buildPathTo filename
    outfile  <- fileCreate filename
    let closeOutfile ok = do   -- Процедура, выполняемая после распаковки файла или при выходе по ^Break
          fileClose outfile                              -- to do: если используется fileSetSize, то изменить размер файла в соответствии с количеством реально распакованных байт
          if ok || opt_keep_broken command
            then do setFileDateTimeAttr filename fileinfo   -- Распаковано успешно или нужно сохранять даже файлы, распакованные с ошибками
                    when (opt_clear_archive_bit command) $ do
                        clearArchiveBit filename            -- Опция -ac - очистить атрибут Archive после распаковки
            else fileRemove filename                     -- Удалить файл, распакованный с ошибками
    do  --fileSetSize outfile (fiSize fileinfo)  -- Приличная ОС при этом выделит на диске место для файла одним куском
        handleCtrlBreak "closeOutfile" (closeOutfile False) $ do
          ok <- run_decompress decompress_pipe compressed_file (fileWriteBuf outfile)
          closeOutfile ok


-- |Эта функция определяет - можно ли извлечь файл из архива?
-- Ответ зависит от 1) использованных опций (-u/-f/-sync)
--                  2) наличия на диске предыдущего файла
--                  3) того, какой из файлов свежее - на диске или в архиве
--                  4) значения опций "-o" и "y"
--                  5) ответа пользователя на запрос о перезаписи файла
--
can_be_extracted cmd filename arcfile = do
  diskfile_exist <- fileExist filename
  if not diskfile_exist                         -- Если файл на диске не существует
    then return (opt_update_type cmd /= 'f')    -- то извлечь файл из архива можно во всех случаях, кроме '-f'
    else do
  fileWithStatus "getFileInfo" filename $ \p_stat -> do
  diskFileIsDir  <-  stat_mode  p_stat  >>==  s_isdir
  diskFileTime   <-  stat_mtime p_stat
  diskFileSize   <-  if diskFileIsDir then return 0
                                      else stat_size p_stat
  let arcfile_newer  =  fiTime arcfile > diskFileTime   -- файл в архиве свежее, чем на диске?
  let overwrite = case (opt_update_type cmd) of
                    'f' -> arcfile_newer
                    'u' -> arcfile_newer
                    's' -> error "--sync can't be used on extract"
                    'a' -> True
  if not overwrite  then return False  else do
  askOverwrite filename diskFileSize diskFileTime arcfile (opt_overwrite cmd) arcfile_newer


{-# NOINLINE run_decompress #-}
-- |Распаковка файла из архива с проверкой CRC
run_decompress decompress_pipe compressed_file write_data = do
  crc <- ref aINIT_CRC                        -- Инициализируем значение CRC
  let writer buf len = do
        uiUnpackedBytes  (i len)              -- Информируем пользователя о ходе распаковки
        uiUpdateProgressIndicator (i len)     -- -.-
        crc          .<- updateCRC buf len    -- Обновим CRC содержимым буфера
        write_data       buf len              -- Запишем данные в файл
        send_backP       decompress_pipe ()   -- И возвратим использованный буфер
  decompress_file decompress_pipe compressed_file writer
  acrc  <-  val crc >>== finishCRC            -- Вычислим окончательное значение CRC
  when (cfCRC compressed_file /= acrc) $ do
    registerWarning$ BAD_CRC (fpFullname$ fiStoredName$ cfFileInfo compressed_file)
  return (cfCRC compressed_file == acrc)      -- Возвратить True, если всё ок


----------------------------------------------------------------------------------------------------
---- Запись комментария к архиву в файл (команда "cw")                                          ----
----------------------------------------------------------------------------------------------------

-- |Реализация команды "cw" - запись комментария к архиву в файл
runCommentWrite command@Command{ cmd_filespecs   = filespecs
                               , cmd_arcname     = arcname
                               , opt_unParseFile = unParseFile
                               } = do
  doFinally uiDoneArchive2 $ do
  when (length filespecs /= 1) $
    registerError$ CMDLINE_SYNTAX "cw archive outfile"
  let [outfile] = filespecs
  command <- (command.$ opt_cook_passwords) command ask_passwords  -- подготовить пароли в команде к использованию
  printLineLn$ "Writing archive comment of "++arcname++" to "++outfile
  bracket (archiveReadFooter command arcname) (archiveClose.fst) $ \(_,footer) -> do
    unParseFile 'c' outfile (ftComment footer)
  return (0,0,0,0)


----------------------------------------------------------------------------------------------------
---- Печать листинга архива:                                                                    ----
----    - для пользователя (команда "l")                                                        ----
----    - для создания файл-листов (команда "lb")                                               ----
----    - для других программ (команда "v")                                                     ----
---------------------------------------------------------------------------------------------------

-- |Обобщённая команда получения листинга архива
runArchiveList pretestArchive
               command@Command{ cmd_arclist        = arclist
                              , cmd_arcname        = arcname
                              , opt_arc_basedir    = arc_basedir
                              , cmd_archive_filter = archive_filter
                              } = do
  command <- (command.$ opt_cook_passwords) command ask_passwords  -- подготовить пароли в команде к использованию
  bracket (archiveReadInfo command arc_basedir "" archive_filter (pretestArchive command) arcname) (arcClose) $
      archiveList command (null$ tail arclist)

-- |Листинг архива
archiveList command @ Command{ cmd_name = cmd, cmd_arcname = arcname }
            show_empty
            arc @ ArchiveInfo{ arcDirectory = directory } = do
  let files = length directory
      bytes = sum$ map (fiSize.cfFileInfo) directory
  when (files>0 || show_empty) $ do
    doFinally uiDoneArchive2 $ do
    uiStartArchive command [] -- Сообщить пользователю о начале обработки очередного архива
    let list line1 line2 list_func linelast = do
                uiPrintArcComment (arcComment arc)
                myPutStrLn line1
                myPutStrLn line2
                compsize <- list_func
                myPutStrLn linelast
                myPutStr$   show3 files ++ " files, " ++ show3 bytes ++ " bytes, " ++ show3 compsize ++ " compressed"
    case cmd of
      "l" -> list "Date/time                  Size Filename"
                  "----------------------------------------"
                  (myMapM terse_list directory)
                  "----------------------------------------"

      "v" -> list "Date/time              Attr            Size          Packed      CRC Filename"
                  "-----------------------------------------------------------------------------"
                  (myMapM verbose_list directory)
                  "-----------------------------------------------------------------------------"

      "lb"-> myPutStr$ joinWith "\n"$ map filename directory

      "lt"-> list "              Pos            Size      Compressed   Files Method"
                  "-----------------------------------------------------------------------------"
                  (do mapM_ data_block_list (arcDataBlocks arc)
                      return (sum$ map blCompSize (arcDataBlocks arc)))
                  "-----------------------------------------------------------------------------"
  return (1, files, bytes, -1)


-- |Имя файла
filename = fpFullname . fiStoredName . cfFileInfo

-- |Добавляет к командам листинга информацию о сжатых размерах солид-блоков
myMapM f = go 0 True undefined
 where
  go total first lastSolidBlock [] = return total
  go total first lastSolidBlock (file:rest) = do
    let solidBlock = cfArcBlock file
    let compsize = if first  ||  blPos solidBlock /= blPos lastSolidBlock
                     then (blCompSize solidBlock)
                     else 0
    f file compsize
    (go $! total+compsize) False solidBlock rest


-- |Однострочный простой листинг файла
terse_list direntry compsize = do
  let fi = cfFileInfo direntry
  myPutStrLn$        (formatDateTime$ fiTime fi)
           ++ " " ++ right_justify 11 (if (fiIsDir fi) then ("-dir-") else (show3$ fiSize fi))
                  ++ (if (cfIsEncrypted direntry)  then "*"  else " ")
                  ++ filename direntry

-- |Однострочный подробный листинг файла
verbose_list direntry compsize = do
  let fi = cfFileInfo direntry
  myPutStrLn$        (formatDateTime$ fiTime fi)
           ++ " " ++ (if (fiIsDir fi)  then ".D....."  else ".......")
           ++ " " ++ right_justify 15 (show$ fiSize fi)
           ++ " " ++ right_justify 15 (show$ compsize)
           ++ " " ++ left_fill  '0' 8 (showHex (cfCRC direntry) "")
                  ++ (if (cfIsEncrypted direntry)  then "*"  else " ")
                  ++ filename direntry

{-
-- |Многострочный технический листинг файла
technical_list direntry = do
  let fi = (cfFileInfo direntry)
  timestr <- formatDateTime (fiTime fi)
  myPutStrLn$ ""
  myPutStrLn$ "Filename: "  ++ (fpFullname$ fiStoredName fi)
  myPutStrLn$ "Size: "      ++ (show$ fiSize fi)
  myPutStrLn$ "Date/time: " ++ timestr
  myPutStrLn$ "CRC: "       ++ showHex (cfCRC direntry) ""
  myPutStrLn$ "Type: "      ++ if (fiIsDir fi) then "directory" else "file"
-}

-- |Описание солид-блока
data_block_list bl = do
  myPutStrLn$        (if (blIsEncrypted bl)  then "*"  else " ")
           ++ " " ++ right_justify 15 (show3$ blPos      bl)
           ++ " " ++ right_justify 15 (show3$ blOrigSize bl)
           ++ " " ++ right_justify 15 (show3$ blCompSize bl)
           ++ " " ++ right_justify  7 (show3$ blFiles    bl)
           ++ " " ++ join_compressor (blCompressor bl)

