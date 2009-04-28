{-# OPTIONS_GHC -cpp #-}
---------------------------------------------------------------------------------------------------
---- Описание команд и опций, поддерживаемых FreeArc.                                          ----
---- Универсальный парсер командной строки.                                                    ----
---- Интерпретация Lua-скриптов.                                                               ----
---------------------------------------------------------------------------------------------------
module Options where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import Control.Concurrent
import Data.Array
import Data.Bits
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import Foreign.C
import Foreign.C.Types
import System.Environment
import System.IO.Unsafe
import System.Time
#if !defined(FREEARC_NO_LUA)
import qualified Scripting.Lua as Lua
#endif

import qualified CompressionLib
import Utils
import Files
import Charsets
import Errors
import FileInfo
import Compression


-- |Описание выполняемой команды
data Command = Command {
    cmd_args                 :: ![String]           -- Полный текст команды, разбитый на слова
  , cmd_additional_args      :: ![String]           -- Дополнительные опции, прочитанные из переменной среды и конфиг-файла
  , cmd_name                 :: !String             -- Название команды
  , cmd_arcspec              ::  String             -- Маска архивов
  , cmd_arclist              ::  [FilePath]         --   Имена всех найденных по этой маске (и возможно, рекурсивно) архивов
  , cmd_arcname              ::  FilePath           --   Имя обрабатываемого архива (из одной команды с wildcards в cmd_arcspec формируется несколько команд с конкретным именем обрабатываемого архива)
  , cmd_archive_filter       :: (FileInfo -> Bool)  -- Предикат отбора файлов из существующих архивов
  , cmd_filespecs            :: ![String]           -- Спецификации добавляемых архивов или файлов
  , cmd_added_arcnames       :: !(IO [FilePath])    --   Вычисление, возвращающее имена добавляемых архивов (команда "j")
  , cmd_diskfiles            :: !(IO [FileInfo])    --   Вычисление, возвращающее имена добавляемых файлов  (остальные команды создания/обновления архивов)
  , cmd_subcommand           :: !Bool               -- Подкоманда? (например, тестирование после архивации)
  , cmd_setup_command        :: !(IO ())            -- Действия, которые надо выполнить непосредственно перед началом отработки этой команды (один раз на все архивы)
                                                    -- Опции:
  , opt_scan_subdirs         :: !Bool               --   рекурсивный поиск файлов?
  , opt_add_dir              :: !Bool               --   добавить имя архива к имени каталога, куда происходит распаковка?
  , opt_add_exclude_path     :: !Int                --   исключить имя базового каталога / сохранить абсолютный путь (при ПОИСКЕ архивируемых файлов на диске)
  , opt_dir_exclude_path     :: !Int                --   исключить имя базового каталога / сохранить абсолютный путь (при чтении КАТАЛОГА АРХИВА)
  , opt_arc_basedir          :: !String             --   базовый каталог внутри архива
  , opt_disk_basedir         :: !String             --   базовый каталог на диске
  , opt_group_dir            :: ![Grouping]         --   группировка файлов для каталога архива
  , opt_group_data           :: ![Grouping]         --   группировка файлов для солид-блока
  , opt_data_compressor      :: !UserCompressor     --   методы сжатия для данных
  , opt_dir_compressor       :: !Compressor         --   метод сжатия для блоков каталога
  , opt_autodetect           :: !Int                --   уровень автодетекта типов файлов (0..9)
  , opt_arccmt_file          :: !String             --   файл, из которого читается (в который пишется) комментарий к архиву
  , opt_arccmt_str           :: !String             --   .. или сам комментарий в чистом виде
  , opt_include_dirs         :: !(Maybe Bool)       --   включить каталоги в обработку? (Да/Нет/По обстоятельствам)
  , opt_indicator            :: !String             --   тип индикатора прогресса ("0" - отсутствует, "1" - индикатор по умолчанию, "2" - вывод в отдельной строке имени каждого обрабатываемого файла)
  , opt_display              :: !String             --   внутренняя опция, описывающая какие строки выводить на экран
  , opt_overwrite            :: !(IORef String)     --   состояние запроса к пользователю о перезаписи файлов ("a" - перезаписывать все, "s" - пропускать все, любые другие - задавать вопросы)
  , opt_sfx                  :: !String             --   имя SFX-модуля, который надо присоединить к архиву ("-" - отсоединить, если уже есть, "--" - скопировать существующий)
  , opt_keep_time            :: !Bool               --   сохранить mtime архива после обновления содержимого?
  , opt_time_to_last         :: !Bool               --   установить mtime архива на mtime самого свежего файла в нём?
  , opt_keep_broken          :: !Bool               --   не удалять файлы, распакованные с ошибками?
  , opt_test                 :: !Bool               --   протестировать архив после упаковки?
  , opt_pretest              :: !Int                --   режим тестирования архивов _перед_ выполнением операции (0 - нет, 1 - только recovery info, 2 - recovery или full, 3 - full testing)
  , opt_lock_archive         :: !Bool               --   закрыть создаваемый архив от дальнейших изменений?
  , opt_match_with           :: !(PackedFilePath -> FilePath)  -- сопоставлять при фильтрации маски с fpBasename или fpFullname
  , opt_append               :: !Bool               --   добавлять новые файлы только в конец архива?
  , opt_recompress           :: !Bool               --   принудительно перепаковать все файлы?
  , opt_keep_original        :: !Bool               --   не перепаковывать ни одного файла?
  , opt_noarcext             :: !Bool               --   не добавлять стандартное расширение к имени архива?
  , opt_nodir                :: !Bool               --   не записывать в архив его оглавление (для бенчмарков)?
  , opt_update_type          :: !Char               --   алгоритм обновления файлов (a/f/u/s)
  , opt_x_include_dirs       :: !Bool               --   включить каталоги в обработку (для команд листинга/распаковки)?
  , opt_no_nst_filters       :: !Bool               --   TRUE, если в команде отсутствуют опции отбора файлов по имени/размеру/времени (-n/-s../-t..)
  , opt_file_filter          :: !(FileInfo -> Bool) --   сформированный опциями предикат отбора файлов по атрибутам/размеру/времени/имени (всё, кроме filespecs)
  , opt_sort_order           :: !String             --   порядок сортировки файлов в архиве
  , opt_reorder              :: !Bool               --   переупорядочить файлы после сортировки (поместив рядом одинаковые/близкие файлы)?
  , opt_find_group           :: !(FileInfo -> Int)  --   функция, определяющая по FileInfo к какой группе (из arc.groups) относится данный файл
  , opt_groups_count         :: !Int                --   количество групп (`opt_find_group` возвращает результаты в диапазоне 0..opt_groups_count-1)
  , opt_find_type            :: !(FileInfo -> Int)  --   функция, определяющая по FileInfo к какому типу данных (из перечисленных в `opt_data_compressor`) относится данный файл
  , opt_types_count          :: !Int                --   количество типов файлов (`opt_find_type` возвращает результаты в диапазоне 0..opt_types_count-1)
  , opt_group2type           :: !(Int -> Int)       --   преобразует номер группы из arc.groups а номер типа файла из opt_data_compressor
  , opt_logfile              :: !String             --   имя лог-файла или ""
  , opt_delete_files         :: !DelOptions         --   удалить файлы/каталоги после успешной архивации?
  , opt_workdir              :: !String             --   каталог для временных файлов или ""
  , opt_clear_archive_bit    :: !Bool               --   сбросить атрибут Archive у успешно упакованных файлов (и файлов, которые уже есть в архиве)
  , opt_language             :: !String             --   язык/файл локализации
  , opt_recovery             :: !String             --   величина Recovery блока (в процентах, байтах или секторах)
  , opt_broken_archive       :: !String             --   обрабатывать неисправный архив, полностью сканируя его в поисках оставшихся исправными блоков
  , opt_original             :: !String             --   перезагружать с указанного URL сбойные части архива
  , opt_save_bad_ranges      :: !String             --   записать в заданный файл список неисправных частей архива для их перевыкачки
  , opt_cache                :: !Int                --   размер буфера упреждающего чтения.
  , opt_limit_compression_memory   :: !MemSize      --   ограничение памяти при упаковке, байт
  , opt_limit_decompression_memory :: !MemSize      --   ограничение памяти при распаковке, байт

                                                    -- Настройки шифрования:
  , opt_encryption_algorithm :: !String             --   алгоритм шифрования.
  , opt_cook_passwords                              --   подготавливает команду к использованию шифрования, заправшивая у пользователя пароль и считывая keyfile (не должно выполняться прежде, чем начнётся выполнение самой команды, поэтому не может быть выполнено в parseCmdline)
                             :: !(Command -> (ParseDataFunc -> IO String, ParseDataFunc -> IO String, IO ()) -> IO Command)
  , opt_data_password        :: String              --   пароль, используемый для шифрования данных (включает в себя ввод с клавиатуры и содержимое keyfiles). "" - паролирование не нужно
  , opt_headers_password     :: String              --   пароль, используемый для шифрования заголовков (ditto)
  , opt_decryption_info                             --   информация, используемая процедурой подбора ключа дешифрации:
                             :: ( Bool              --     не запрашивать у пользователя новый пароль, даже если все известные для распаковки данных не подходят?
                                , MVar [String]     --     список "старых паролей", которыми мы пытаемся расшифровать распаковываемые данные
                                , [String]          --     содержимое keyfiles, добавляемых к паролям
                                , IO String         --     ask_decryption_password
                                , IO ()             --     bad_decryption_password
                                )
  -- Операции чтения/записи файлов в кодировке, настраиваемый опцией -sc
  , opt_parseFile   :: !(Domain -> FilePath -> IO [String])      -- процедура парсинга файла с настраиваемой в -sc кодировкой и ОС-независимым разбиением на строки
  , opt_unParseFile :: !(Domain -> FilePath -> String -> IO ())  -- процедура записи файла с настраиваемой в -sc кодировкой
  , opt_parseData   :: !(Domain -> String -> String)             -- процедура парсинга введённых данных с настраиваемой в -sc кодировкой
  , opt_unParseData :: !(Domain -> String -> String)             -- процедура депарсинга данных для вывода с настраиваемой в -sc кодировкой
  }

-- |Виртуальная опция --debug
opt_debug cmd = cmd.$opt_display.$(`contains_one_of` "$#")

-- |Включить тестирование памяти?
opt_testMalloc cmd = cmd.$opt_display.$(`contains_one_of` "%")

-- |Реально используемый компрессор отличается от записываемого в заголовок блока
-- вызовами "tempfile", вставленными между слишком прожорливыми алгоритмами
-- (память ограничивается до значения -lc и размера наибольшего блока свободной памяти,
-- если не задано -lc-)
limit_compressor command compressor = do
  let memory_limit = opt_limit_compression_memory command
  if memory_limit==CompressionLib.aUNLIMITED_MEMORY
    then return compressor
    else do maxMem <- getMaxMemToAlloc
            return$ limitCompressionMemoryUsage (memory_limit `min` maxMem) compressor


-- |Список опций, поддерживаемых программой
optionsList = sortOn (\(OPTION a b _) -> (a|||"zzz",b))
   [OPTION "--"    ""                   "stop processing options"
   ,OPTION "cfg"   "config"            ("use config FILE (default: " ++ aCONFIG_FILE ++ ")")
   ,OPTION "env"   ""                  ("read default options from environment VAR (default: " ++ aCONFIG_ENV_VAR ++ ")")
   ,OPTION "r"     "recursive"          "recursively collect files"
   ,OPTION "f"     "freshen"            "freshen files"
   ,OPTION "u"     "update"             "update files"
   ,OPTION ""      "sync"               "synchronize archive and disk contents"
   ,OPTION "o"     "overwrite"          "existing files overwrite MODE (+/-/p)"
   ,OPTION "y"     "yes"                "answer Yes to all queries"
   ,OPTION "x"     "exclude"            "exclude FILESPECS from operation"
   ,OPTION "n"     "include"            "include only files matching FILESPECS"
   ,OPTION "ep"    "ExcludePath"        "Exclude/expand path MODE"
   ,OPTION "ap"    "arcpath"            "base DIR in archive"
   ,OPTION "dp"    "diskpath"           "base DIR on disk"
   ,OPTION "m"     "method"             "compression METHOD (-m0..-m9, -m1x..-m9x)"
   ,OPTION "dm"    "dirmethod"          "compression METHOD for archive directory"
   ,OPTION "ma"    ""                   "set filetype detection LEVEL (+/-/1..9)"
   ,OPTION "md"    "dictionary"         "set compression dictionary to N mbytes"
   ,OPTION "mm"    "multimedia"         "set multimedia compression to MODE"
   ,OPTION "ms"    "StoreCompressed"    "store already compressed files"
   ,OPTION "mt"    "MultiThreaded"      "number of compression THREADS"
   ,OPTION "mc"    ""                   "disable compression algorithms (-mcd-, -mc-rep...)"
   ,OPTION "mx"    ""                   "maximum internal compression mode"
   ,OPTION "max"   ""                   "maximum compression using external precomp, ecm, ppmonstr"
   ,OPTION "ds"    "sort"               "sort files in ORDER"                      -- to do: сделать эту опцию OptArg
   ,OPTION ""      "groups"             "name of groups FILE"                      -- to do: сделать эту опцию OptArg
   ,OPTION "s"     "solid"              "GROUPING for solid compression"           -- to do: сделать эту опцию OptArg
   ,OPTION "p"     "password"           "encrypt/decrypt compressed data using PASSWORD"
   ,OPTION "hp"    "HeadersPassword"    "encrypt/decrypt archive headers and data using PASSWORD"
   ,OPTION "ae"    "encryption"         "encryption ALGORITHM (aes, blowfish, serpent, twofish)"
   ,OPTION "kf"    "keyfile"            "encrypt/decrypt using KEYFILE"
   ,OPTION "op"    "OldPassword"        "old PASSWORD used only for decryption"
   ,OPTION "okf"   "OldKeyfile"         "old KEYFILE used only for decryption"
   ,OPTION "w"     "workdir"            "DIRECTORY for temporary files"
   ,OPTION "sc"    "charset"            "CHARSETS used for listfiles and comment files"
   ,OPTION ""      "language"           "load localisation from FILE"
   ,OPTION "tp"    "pretest"            "test archive before operation using MODE"
   ,OPTION "t"     "test"               "test archive after operation"
   ,OPTION "d"     "delete"             "delete files & dirs after successful archiving"
   ,OPTION "df"    "delfiles"           "delete only files after successful archiving"
   ,OPTION "kb"    "keepbroken"         "keep broken extracted files"
   ,OPTION "ba"    "BrokenArchive"      "deal with badly broken archive using MODE"
#if defined(FREEARC_WIN)
   ,OPTION "ac"    "ClearArchiveBit"    "clear Archive bit on files succesfully (de)archived"
   ,OPTION "ao"    "SelectArchiveBit"   "select only files with Archive bit set"
#endif
   ,OPTION "sm"    "SizeMore"           "select files larger than SIZE"
   ,OPTION "sl"    "SizeLess"           "select files smaller than SIZE"
   ,OPTION "tb"    "TimeBefore"         "select files modified before specified TIME"
   ,OPTION "ta"    "TimeAfter"          "select files modified after specified TIME"
   ,OPTION "tn"    "TimeNewer"          "select files newer than specified time PERIOD"
   ,OPTION "to"    "TimeOlder"          "select files older than specified time PERIOD"
   ,OPTION "k"     "lock"               "lock archive"
   ,OPTION "rr"    "recovery"           "add recovery information of specified SIZE to archive"
   ,OPTION "sfx"   ""                  ("add sfx MODULE (\""++aDEFAULT_SFX++"\" by default)")  -- to do: сделать эту опцию OptArg
   ,OPTION "z"     "arccmt"             "read archive comment from FILE or stdin"  -- to do: сделать эту опцию OptArg
   ,OPTION ""      "archive-comment"    "input archive COMMENT in cmdline"
   ,OPTION "i"     "indicator"          "select progress indicator TYPE (0/1/2)"   -- to do: сделать эту опцию OptArg
   ,OPTION "ad"    "adddir"             "add arcname to extraction path"
   ,OPTION "ag"    "autogenerate"       "autogenerate archive name with FMT"       -- to do: сделать эту опцию OptArg
   ,OPTION ""      "noarcext"           "don't add default extension to archive name"
   ,OPTION "tk"    "keeptime"           "keep original archive time"
   ,OPTION "tl"    "timetolast"         "set archive time to latest file"
   ,OPTION "fn"    "fullnames"          "match with full names"
   ,OPTION ""      "append"             "add new files to the end of archive"
   ,OPTION ""      "recompress"         "recompress archive contents"
   ,OPTION ""      "dirs"               "add empty dirs to archive"
   ,OPTION "ed"    "nodirs"             "don't add empty dirs to archive"
   ,OPTION ""      "cache"              "use N mbytes for read-ahead cache"
   ,OPTION "lc"    "LimitCompMem"       "limit memory usage for compression to N mbytes"
   ,OPTION "ld"    "LimitDecompMem"     "limit memory usage for decompression to N mbytes"
   ,OPTION ""      "nodir"              "don't write archive directories"
   ,OPTION ""      "nodata"             "don't store data in archive"
   ,OPTION ""      "crconly"            "save/check CRC, but don't store data"
   ,OPTION "di"    "display"           ("control AMOUNT of information displayed: ["++aDISPLAY_ALL++"]*")
   ,OPTION ""      "logfile"            "duplicate all information displayed to this FILE"
   ,OPTION ""      "print-config"       "display built-in definitions of compression methods"
   ,OPTION ""      "proxy"              "setups proxy(s) for URL access"
   ,OPTION ""      "bypass"             "setups proxy bypass list for URL access"
   ,OPTION ""      "original"           "redownload broken parts of archive from the URL"
   ,OPTION ""      "save-bad-ranges"    "save list of broken archive parts to the FILE"
   ]

-- |Список опций, которым надо отдавать предпочтение при возникновении коллизий в разборе командной строки
aPREFFERED_OPTIONS = words "method sfx charset SizeMore SizeLess overwrite"

-- |Опции из предыдущего списка, имеющий максимальный приоритет :)
aSUPER_PREFFERED_OPTIONS = words "OldKeyfile"

-- |Скрыть пароли в командной строке (перед её выводом в лог)
hidePasswords args = map f args1 ++ args2 where
  (args1,args2)  =  break (=="--") args
  f "-p-"                                   =  "-p-"
  f ('-':'p':_)                             =  "-p"
  f "-op-"                                  =  "-op-"
  f ('-':'o':'p':_)                         =  "-op"
  f "-hp-"                                  =  "-hp-"
  f ('-':'h':'p':_)                         =  "-hp"
  f "--OldPassword-"                        =  "--OldPassword-"
  f x | "--OldPassword" `isPrefixOf` x      =  "--OldPassword"
  f "--HeadersPassword-"                    =  "--HeadersPassword-"
  f x | "--HeadersPassword" `isPrefixOf` x  =  "--HeadersPassword"
  f "--password-"                           =  "--password-"
  f x | "--password" `isPrefixOf` x         =  "--password"
  f x = x


-- |Описание команд, поддерживаемых программой
commandsList = [
    "a        add files to archive"
  , "c        add comment to archive"
  , "ch       modify archive (recompress, encrypt and so on)"
  , "create   create new archive"
  , "cw       write archive comment to file"
  , "d        delete files from archive"
  , "e        extract files from archive ignoring pathnames"
  , "f        freshen archive"
  , "j        join archives"
  , "k        lock archive"
  , "l        list files in archive"
  , "lb       bare list of files in archive"
  , "lt       technical archive listing"
  , "m        move files and dirs to archive"
  , "mf       move files to archive"
  , "r        recover archive using recovery record"
  , "rr       add recovery record to archive"
  , "s        convert archive to SFX"
  , "t        test archive integrity"
  , "u        update files in archive"
  , "v        verbosely list files in archive"
  , "x        extract files from archive"
  ]

-- |Список команд, поддерживаемых программой
aLL_COMMANDS = map (head.words) commandsList

-- |Список команд, которые просто копируют архив
is_COPYING_COMMAND ('r':'r':_) = True
is_COPYING_COMMAND ('s':_)     = True
is_COPYING_COMMAND x           = x `elem` words "c ch d j k"

-- |Команда, у которой НЕ ДОЛЖНО быть ни одного аргумента (помимо имени архива)
is_CMD_WITHOUT_ARGS x  =  is_COPYING_COMMAND x  &&  (x `notElem` words "d j")

-- |Классификация всех команд по четырём типам: команды упаковки, распаковки, тестирования и листинга
data CmdType = ADD_CMD | EXTRACT_CMD | TEST_CMD | LIST_CMD | RECOVER_CMD  deriving (Eq)
cmdType "t"  = TEST_CMD
cmdType "e"  = EXTRACT_CMD
cmdType "x"  = EXTRACT_CMD
cmdType "cw" = EXTRACT_CMD
cmdType "l"  = LIST_CMD
cmdType "lb" = LIST_CMD
cmdType "lt" = LIST_CMD
cmdType "v"  = LIST_CMD
cmdType "r"  = RECOVER_CMD
cmdType  _   = ADD_CMD
{-# NOINLINE cmdType #-}

-- |Версия архиватора, записываемая в HEADER BLOCK
aARCHIVE_VERSION = make4byte 0 0 5 1

{-# NOINLINE aARC_VERSION_WITH_DATE #-}
{-# NOINLINE aARC_HEADER_WITH_DATE #-}
{-# NOINLINE aARC_HEADER #-}
{-# NOINLINE aARC_VERSION #-}
{-# NOINLINE aARC_AUTHOR #-}
{-# NOINLINE aARC_EMAIL #-}
{-# NOINLINE aARC_WEBSITE #-}
-- |Краткое наименование программы, выводимое в начале работы
aARC_VERSION_WITH_DATE = aARC_VERSION ++ " ("++aARC_DATE++")"   -- aARC_VERSION
aARC_HEADER_WITH_DATE  = aARC_HEADER  ++ " ("++aARC_DATE++")"   -- aARC_HEADER
aARC_HEADER  = aARC_NAME++" "++aARC_VERSION++" "
aARC_VERSION = "0.51"                                  -- "0.50 alpha ("++aARC_DATE++")"
aARC_DATE    = "Apr 28 2009"
aARC_NAME    = "FreeArc"
aARC_AUTHOR  = "Bulat Ziganshin"
aARC_EMAIL   = "Bulat.Ziganshin@gmail.com"
aARC_WEBSITE = "http://freearc.org"

{-# NOINLINE aHELP #-}
-- |HELP, выводимый при вызове программы без параметров
aHELP = aARC_HEADER++" "++aARC_WEBSITE++"  "++aARC_DATE++"\n"++
        "Usage: Arc command [options...] archive [files... @listfiles...]\n" ++
        joinWith "\n  " ("Commands:":commandsList) ++ "\nOptions:\n" ++ optionsHelp

-- |Способы группировки файлов для солид-блока или оглавления архива
data Grouping = GroupNone                   -- каждый файл отдельно
                                            -- группировка по:
              | GroupByExt                  --   одинаковому расширению
              | GroupBySize      FileSize   --   минимальному объёму блока данных
              | GroupByBlockSize MemSize    --   максимальному объёму блока данных (для блочно-ориентированных алгоритмов, таких как BWT и ST)
              | GroupByNumber    FileCount  --   количеству файлов
              | GroupAll                    -- все файлы вместе

-- |Значение опции -d[f]: не удалять, удалять только файлы, удалять файлы и каталоги
data DelOptions = NO_DELETE | DEL_FILES | DEL_FILES_AND_DIRS  deriving (Eq)


---------------------------------------------------------------------------------------------------
-- ЗНАЧЕНИЯ, ИСПОЛЬЗУЕМЫЕ ПО УМОЛЧАНИЮ ------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Метод сжатия данных
aDEFAULT_COMPRESSOR = "4"

-- |Метод сжатия каталога архива
aDEFAULT_DIR_COMPRESSION = "lzma:bt4:1m"

-- |Размер солид-блоков (один солид-блок на всех)
aDEFAULT_DATA_GROUPING  =  ""

-- |Группировка для каталогов
aDEFAULT_DIR_GROUPING  =  GroupByNumber (20*1000)

-- |Алгоритм сжатия данных, используемый по умолчанию
aDEFAULT_ENCRYPTION_ALGORITHM = "aes"

-- |Если в командной строке не указаны имена обрабатываемых файлов - обрабатывать все, т.е. "*"
aDEFAULT_FILESPECS = [reANY_FILE]

-- |Расширение архивных файлов
aDEFAULT_ARC_EXTENSION = ".arc"

-- |Расширение SFX архивных файлов
#ifdef FREEARC_WIN
aDEFAULT_SFX_EXTENSION = ".exe"
#else
aDEFAULT_SFX_EXTENSION = ""
#endif

-- |Файл локализации
aLANG_FILE = "arc.language.txt"

-- |Файл с описанием порядка сортировки имён файлов при "-og"
aDEFAULT_GROUPS_FILE = "arc.groups"

-- |SFX-модуль, используемый по умолчанию
aDEFAULT_SFX = "freearc.sfx"

-- |Файл конфигурации (хранящий опции, используемые по умолчанию)
aCONFIG_FILE = "arc.ini"

-- |Переменная среды, содержащая опции, используемые по умолчанию
aCONFIG_ENV_VAR = "FREEARC"

-- |Порядок сортировки, используемый при solid-сжатии (для увеличения сжатия)
aDEFAULT_SOLID_SORT_ORDER = "gerpn"

-- |Объём информации, выводимой на экран - по умолчанию и при использовании опции "--display" без параметра.
-- По умолчанию на экран не выводятся "cmo" - доп. опции, режим сжатия и используемая память
aDISPLAY_DEFAULT = "hanwrftske"
aDISPLAY_ALL     = "hoacmnwrfdtske"

-- Секции arc.ini
compressionMethods = "[Compression methods]"
defaultOptions     = "[Default options]"
externalCompressor = "[External compressor:*]"

-- |Приведение имени секции к стандартизованной форме
cleanupSectionName  =  strLower . filter (not.isSpace)

-- |Проверка того, что это - заголовок секции
selectSectionHeadings  =  ("["==) . take 1 . trim


----------------------------------------------------------------------------------------------------
---- Универсальный парсер командной строки ---------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Описание опции - краткое имя, длинное имя, печатаемое описание
data Option = OPTION String String String

-- |Тип опции - короткие имеют префикс "-", длинные - префикс "--"
data OptType  =  SHORT | LONG

-- |Наличие параметра в опции: нет/обязательно/опционально
data ParamType  =  ParamNo | ParamReq | ParamOpt

-- |"Словарь" опций, содержащий их в удобной для разбора командной строки форме
optionsDict  =  concatMap compileOption optionsList
  where compileOption (OPTION short long description)  =  compile short ++ compile ('-':long)
          where -- Добавить в список описание опции с именем `name`, если оно непустое
                compile name  =  case (name, paramName description) of
                    ("",  _      )  ->  []                                -- нет имени - нет и опции :)
                    ("-", _      )  ->  []                                -- нет имени - нет и опции :)
                    (_,   Nothing)  ->  [(name, long|||short, ParamNo )]  -- опция без параметра
                    (_,   Just _ )  ->  [(name, long|||short, ParamReq)]  -- опция с параметром

-- |Описание опций для пользователя.
optionsHelp  =  init$ unlines table
  where (ss,ls,ds)     = (unzip3 . map fmtOpt) optionsList
        table          = zipWith3 paste (sameLen ss) (sameLen ls) ds
        paste x y z    = "  " ++ x ++ "  " ++ y ++ "  " ++ z
        sameLen xs     = flushLeft ((maximum . map length) xs) xs
        flushLeft n    = map (left_justify n)
          -- Возвращает формат "короткой опции", "длинной опции", и их описание
        fmtOpt (OPTION short long description)  =  (format short "" description, format ('-':long) "=" description, description)
          -- Возвращает формат опции `name` с учётом наличия у неё имени и параметра
        format name delim description  =  case (name, paramName description) of
                                            ("",   _         )  ->  ""
                                            ("-",  _         )  ->  ""
                                            ("--", _         )  ->  "--"
                                            (_,    Nothing   )  ->  "-"++name
                                            (_,    Just aWORD)  ->  "-"++name++delim++aWORD

-- |Возвращает имя параметра опции, извлекая его из строки её описания.
paramName descr =
  case filter (all isUpper) (words descr)
    of []      -> Nothing      -- Описание не содержит UPPERCASED слов
       [aWORD] -> Just aWORD   -- Описание включает UPPERCASED слово, обозначающее параметр опции
       _       -> error$ "option description \""++descr++"\" contains more than one uppercased word"

-- |Разбор командной строки, возвращающий список опций и список "свободных аргументов"
parseOptions []          options freeArgs  =  return (reverse options, reverse freeArgs)
parseOptions ("--":args) options freeArgs  =  return (reverse options, reverse freeArgs ++ args)

parseOptions (('-':option):args) options freeArgs = do
  let check (prefix, _, ParamNo)  =  (option==prefix)
      check (prefix, _, _)        =  (startFrom prefix option /= Nothing)
  let accept (prefix, name, haveParam)  =  return (name, tryToSkip "=" (tryToSkip prefix option))
      unknown                           =  registerError$ CMDLINE_UNKNOWN_OPTION ('-':option)
      ambiguous variants                =  registerError$ CMDLINE_AMBIGUOUS_OPTION ('-':option) (map (('-':).fst3) variants)
  newopt <- case (filter check optionsDict) of
              [opt] -> accept opt  -- принять опцию
              []    -> unknown     -- неизвестная опция.
              xs    -> -- При неоднозначности в разборе опции посмотрим на список предпочтительных опций
                       case (filter ((`elem` aPREFFERED_OPTIONS++aSUPER_PREFFERED_OPTIONS) . snd3) xs) of
                         [opt] -> accept opt        -- принять опцию
                         []    -> ambiguous xs      -- неоднозначная опция, которой нет в списке предпочтений
                         xs    -> -- Повторим трюк! :)
                                  case (filter ((`elem` aSUPER_PREFFERED_OPTIONS) . snd3) xs) of
                                    [opt] -> accept opt        -- принять опцию
                                    []    -> ambiguous xs      -- неоднозначная опция, которой нет в списке предпочтений
                                    xs    -> ambiguous xs      -- неоднозначный разбор даже в списке предпочтений!

  parseOptions args (newopt:options) freeArgs

parseOptions (arg:args) options freeArgs   =  parseOptions args options (arg:freeArgs)


-- |Вернуть список значений опции с названием `flag`. Пример вызова: findReqList opts "exclude"
findReqList ((name, param):flags) flag  | name==flag  =  param: findReqList flags flag
findReqList (_:flags) flag                            =  findReqList flags flag
findReqList [] flag                                   =  []

-- |Вернуть значение опции с названием `flag`, если её нет - значение по умолчанию `deflt`
findReqArg options flag deflt  =  last (deflt : findReqList options flag)

-- |Вернуть значение опции с необязательным параметром
findOptArg = findReqArg

-- |Вернуть значение опции с названием `flag`, если её нет - Nothing
findMaybeArg options flag  =  case findReqList options flag
                                of [] -> Nothing
                                   xs -> Just (last xs)

-- |Вернуть True, если в списке опций есть опция с названием `flag`
findNoArg options flag  =  case findReqList options flag
                                of [] -> False
                                   _  -> True

-- |Вернуть Just True, если в списке опций есть опция с названием `flag1`,
--          Just False, если в списке опций есть опция с названием `flag2`,
--          Nothing, если нет ни той, ни другой
findNoArgs options flag1 flag2  =  case filter (\(o,_) -> o==flag1||o==flag2) options
                                     of [] -> Nothing
                                        xs -> Just (fst (last xs) == flag1)

{-# NOINLINE optionsDict #-}
{-# NOINLINE optionsHelp #-}
{-# NOINLINE parseOptions #-}
{-# NOINLINE findReqList #-}
{-# NOINLINE findReqArg #-}
{-# NOINLINE findMaybeArg #-}
{-# NOINLINE findNoArg #-}
{-# NOINLINE findNoArgs #-}


---------------------------------------------------------------------------------------------------
---- Интерпретация Lua-скриптов                                                                ----
---------------------------------------------------------------------------------------------------

#if defined(FREEARC_NO_LUA)
-- Lua support disabled, just imitate it
type LuaState = ()
luaInit      = return ()
luaRun _ _ _ = return ()
#else

-- |Instance of Lua interpreter (separate instances may be created for every command executed)
type LuaState = Lua.LuaState

-- |Create new Lua instance
luaInit = do
  l <- Lua.newstate
  Lua.openlibs l
  -- Init event handler lists
  for luaEvents (addLuaEvent l)
  -- Execute configuration scripts, adding handlers for events
  places <- configFilePlaces "arc.*.lua"
  for places $ \place -> do
    scripts <- dirWildcardList place `catch` (\e->return [])
    for scripts (Lua.dofile l . (takeDirectory place </>))
  return l

-- |Execute Lua scripts assigned to the event cmd
luaRun l cmd params = do
  Lua.callproc l cmd params
  return ()

-- |Add support of event cmd to Lua instance
addLuaEvent l cmd = Lua.dostring l $ unlines
                      [ handlers++" = {}"
                      , "function on"++cmd++"(handler)"
                      , "  table.insert ("++handlers++", handler)"
                      , "end"
                      , "function "++cmd++"(params)"
                      , "  for _,handler in ipairs("++handlers++") do"
                      , "    handler(params)"
                      , "  end"
                      , "end"
                      ]        where handlers = "on"++cmd++"Handlers"

-- |Lua events list
luaEvents = words "ProgramStart ProgramDone CommandStart CommandDone"++
            words "ArchiveStart ArchiveDone Error Warning"

#endif


-- |The global Lua instance
{-# NOINLINE lua_state #-}
lua_state :: MVar LuaState
lua_state = unsafePerformIO $ do
   lua <- luaInit
   errorHandlers   ++= [\msg -> luaEvent "Error"   [("message", msg)]]
   warningHandlers ++= [\msg -> luaEvent "Warning" [("message", msg)]]
   newMVar lua

-- |Run Lua event in the global Lua instance
luaEvent  =  liftMVar3 luaRun lua_state

-- |Perform Start/Done procedures of givel level
luaLevel level params action = do
  luaEvent (level++"Start") params
  ensureCtrlBreak "luaDone" (luaEvent (level++"Done") [""]) action


----------------------------------------------------------------------------------------------------
---- System information ----------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Number of physical processors/cores in the system. Determines number of heavy-computations thread runned
foreign import ccall unsafe "Environment.h GetProcessorsCount"
  getProcessorsCount :: CInt

-- |Size of physical computer memory in bytes
foreign import ccall unsafe "Environment.h GetPhysicalMemory"
  getPhysicalMemory :: CUInt

-- |Size of maximum memory block we can allocate in bytes
foreign import ccall unsafe "Environment.h GetMaxMemToAlloc"
  getMaxMemToAlloc :: IO CUInt

-- |Size of physical computer memory that is currently unused
foreign import ccall unsafe "Environment.h GetAvailablePhysicalMemory"
  getAvailablePhysicalMemory :: CUInt

-- |Prints detailed stats about memory available
foreign import ccall unsafe "Environment.h TestMalloc"
  testMalloc :: IO ()

