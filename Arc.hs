{-# OPTIONS_GHC -cpp #-}
----------------------------------------------------------------------------------------------------
---- Основной модуль программы.                                                                 ----
---- Вызывает parseCmdline из модуля Cmdline для разбора командной строки и выполняет каждую    ----
----   полученную команду.                                                                      ----
---- Если команда должна обработать несколько архивов, то find_archives дублирует её            ----
----   для каждого из них.                                                                      ----
---- Затем каждая команда сводится к выполнению одной из следующих задач:                       ----
---- * изменение архива  с помощью  runArchiveCreate   из модуля ArcCreate   (команды a/f/m/u/j/d/ch/c/k/rr)
---- * распаковка архива         -  runArchiveExtract  -         ArcExtract  (команды t/e/x)    ----
---- * получение листинга архива -  runArchiveList     -         ArcList     (команды l/v)      ----
---- * восстановление архива     -  runArchiveRecovery -         ArcRecover  (команда r)        ----
---- которым передаются аргументы в соответствии со спецификой конкретной выполняемой команды.  ----
----                                                                                            ----
---- Эти процедуры в свою очередь прямо или косвенно обращаются к модулям:                      ----
----   ArhiveFileList   - для работы со списками архивируемых файлов                            ----
----   ArhiveDirectory  - для чтения/записи оглавления архива                                   ----
----   ArhiveStructure  - для работы со структурой архива                                       ----
----   ByteStream       - для превращения каталога архива в последовательность байтов           ----
----   Compression      - для вызова алгоритмов упаковки, распаковки и вычисления CRC           ----
----   UI               - для информирования пользователя о ходе выполняемых работ :)           ----
----   Errors           - для сигнализации о возникших ошибках и записи в логфайл               ----
----   FileInfo         - для поиска файлов на диске и получения информации о них               ----
----   Files            - для всех операций с файлами на диске и именами файлов                 ----
----   Process          - для разделения алгоритма на параллельные взаимодействующие процессы   ----
----   Utils            - для всех остальных вспомогательных функций                            ----
----------------------------------------------------------------------------------------------------
module Main where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.List
import System.Mem
import System.IO

import Utils
import Process
import Errors
import Files
import FileInfo
import Charsets
import Options
import Cmdline
import UI
import ArcCreate
import ArcExtract
import ArcRecover
#ifdef FREEARC_GUI
import FileManager
#endif


-- |Главная функция программы
main         =  (doMain =<< myGetArgs) >> shutdown "" aEXIT_CODE_SUCCESS
-- |Дублирующая главная функция для интерактивной отладки
arc cmdline  =  doMain (words cmdline)

-- |Превратить командную строку в набор команд и выполнить их
doMain args  = do
#ifdef FREEARC_GUI
  bg $ do                           -- выполняем в новом треде, не являющемся bound thread
#endif
  setUncaughtExceptionHandler handler
  setCtrlBreakHandler $ do          -- Организуем обработку ^Break
  ensureCtrlBreak "resetConsoleTitle" (resetConsoleTitle) $ do
  luaLevel "Program" [("command", args)] $ do
#ifdef FREEARC_GUI
  if length args < 2                -- При вызове программы без аргументов или с одним аргументом (именем каталога/архива)
    then myGUI run args             --   запускаем полноценный Archive Manager
    else do                         --   а иначе - просто отрабатываем команды (де)архивации
#endif
  uiStartProgram                    -- Открыть UI
  commands <- parseCmdline args     -- Превратить командную строку в список команд на выполнение
  mapM_ run commands                -- Выполнить каждую полученную команду
  uiDoneProgram                     -- Закрыть UI

 where
  handler ex  =
#ifdef FREEARC_GUI
    mapM_ doNothing $
#else
    registerError$ GENERAL_ERROR$
#endif
      case ex of
        Deadlock    -> ["0011 No threads to run: infinite loop or deadlock?"]
        ErrorCall s -> [s]
        other       -> [show ex]


-- |Диспетчеризует команду и организует её повторение для каждого подходящего архива
run command @ Command
                { cmd_name            = cmd
                , cmd_setup_command   = setup_command
                , opt_scan_subdirs    = scan_subdirs
                } = do
  performGC       -- почистить мусор после обработки предыдущих команд
  setup_command   -- выполнить настройки, необходимые перед началом выполнения команды
  luaLevel "Command" [("command", cmd)] $ do
  case (cmd) of
    "create" -> find_archives  False           run_add     command
    "a"      -> find_archives  False           run_add     command
    "f"      -> find_archives  False           run_add     command
    "m"      -> find_archives  False           run_add     command
    "mf"     -> find_archives  False           run_add     command
    "u"      -> find_archives  False           run_add     command
    "j"      -> find_archives  False           run_join    command
    "cw"     -> find_archives  False           run_cw      command
    "ch"     -> find_archives  scan_subdirs    run_copy    command
    's':_    -> find_archives  scan_subdirs    run_copy    command
    "c"      -> find_archives  scan_subdirs    run_copy    command
    "k"      -> find_archives  scan_subdirs    run_copy    command
    'r':'r':_-> find_archives  scan_subdirs    run_copy    command
    "r"      -> find_archives  scan_subdirs    run_recover command
    "d"      -> find_archives  scan_subdirs    run_delete  command
    "e"      -> find_archives  scan_subdirs    run_extract command
    "x"      -> find_archives  scan_subdirs    run_extract command
    "t"      -> find_archives  scan_subdirs    run_test    command
    "l"      -> find_archives  scan_subdirs    run_list    command
    "lb"     -> find_archives  scan_subdirs    run_list    command
    "lt"     -> find_archives  scan_subdirs    run_list    command
    "v"      -> find_archives  scan_subdirs    run_list    command
    _ -> registerError$ UNKNOWN_CMD cmd aLL_COMMANDS


-- |Ищет архивы, подходящие под маску arcspec, и выполняет заданную команду на каждом из них
find_archives scan_subdirs   -- искать архивы и в подкаталогах?
              run_command    -- процедура, которую нужно запустить на каждом найденном архиве
              command @ Command {cmd_arcspec = arcspec} = do
  uiStartCommand command   -- Отметим начало выполнения команды
  arclist <- if scan_subdirs || is_wildcard arcspec
               then find_files scan_subdirs arcspec >>== map diskName
               else return [arcspec]
  results <- foreach arclist $ \arcname -> do
    performGC   -- почистить мусор после обработки предыдущих архивов
    luaLevel "Archive" [("arcname", arcname)] $ do
    -- Если указана опция -ad, то добавить к базовому каталогу на диске имя архива (без расширения)
    let add_dir  =  opt_add_dir command  &&&  (</> takeBaseName arcname)
    run_command command { cmd_arcspec      = error "find_archives:cmd_arcspec undefined"  -- cmd_arcspec нам больше не понадобится.
                        , cmd_arclist      = arclist
                        , cmd_arcname      = arcname
                        , opt_disk_basedir = add_dir (opt_disk_basedir command)
                        }
  uiDoneCommand command results   -- доложить о результатах выполнения команды над всеми архивами


-- |Команды добавления в архив: create, a, f, m, u
run_add cmd = do
  msg <- i18n"0246 Found %1 files"
  let diskfiles =  find_and_filter_files (cmd_filespecs cmd) (uiScanning msg) find_criteria
      find_criteria  =  FileFind{ ff_ep             = opt_add_exclude_path cmd
                                , ff_scan_subdirs   = opt_scan_subdirs     cmd
                                , ff_include_dirs   = opt_include_dirs     cmd
                                , ff_no_nst_filters = opt_no_nst_filters   cmd
                                , ff_filter_f       = add_file_filter      cmd
                                , ff_group_f        = opt_find_group       cmd.$Just
                                , ff_arc_basedir    = opt_arc_basedir      cmd
                                , ff_disk_basedir   = opt_disk_basedir     cmd}
  runArchiveAdd cmd{ cmd_diskfiles      = diskfiles     -- файлы, которые нужно добавить с диска
                   , cmd_archive_filter = const True }  -- фильтр отбора файлов из открываемых архивов


-- |Команда слияния архивов: j
run_join cmd @ Command { cmd_filespecs = filespecs
                       , opt_noarcext  = noarcext
                       } = do
  msg <- i18n"0247 Found %1 archives"
  let arcspecs  =  map (addArcExtension noarcext) filespecs   -- добавим к именам расширение по умолчанию (".arc")
      arcnames  =  map diskName ==<< find_and_filter_files arcspecs (uiScanning msg) find_criteria
      find_criteria  =  FileFind{ ff_ep             = opt_add_exclude_path cmd
                                , ff_scan_subdirs   = opt_scan_subdirs     cmd
                                , ff_include_dirs   = Just False
                                , ff_no_nst_filters = opt_no_nst_filters   cmd
                                , ff_filter_f       = add_file_filter      cmd
                                , ff_group_f        = Nothing
                                , ff_arc_basedir    = ""
                                , ff_disk_basedir   = opt_disk_basedir     cmd}
  runArchiveAdd cmd{ cmd_added_arcnames = arcnames      -- дополнительные входные архивы
                   , cmd_archive_filter = const True }  -- фильтр отбора файлов из открываемых архивов


-- |Команды копирования архива с внесением изменений: ch, c, k. s, rr
run_copy    = runArchiveAdd                    . setArcFilter full_file_filter
-- |Команда удаления из архива: d
run_delete  = runArchiveAdd                    . setArcFilter ((not.).full_file_filter)
-- |Команды извлечения из архива: e, x
run_extract = runArchiveExtract pretestArchive . setArcFilter (test_dirs extract_file_filter)
-- |Команда тестирования архива: t
run_test    = runArchiveExtract pretestArchive . setArcFilter (test_dirs full_file_filter)
-- |Команды получения листинга архива: l, v
run_list    = runArchiveList pretestArchive    . setArcFilter (test_dirs full_file_filter)
-- |Команда записи архивного комментария в файл: cw
run_cw      = runCommentWrite
-- |Команда восстановления архива: r
run_recover = runArchiveRecovery

-- |Just shortcut
runArchiveAdd  =  runArchiveCreate pretestArchive writeRecoveryBlocks

{-# NOINLINE find_archives #-}
{-# NOINLINE run_add #-}
{-# NOINLINE run_join #-}
{-# NOINLINE run_copy #-}
{-# NOINLINE run_delete #-}
{-# NOINLINE run_extract #-}
{-# NOINLINE run_test #-}
{-# NOINLINE run_list #-}


----------------------------------------------------------------------------------------------------
---- Критерии отбора файлов, подлежащих обработке, для различных типов команд ----------------------
----------------------------------------------------------------------------------------------------

-- |Установить в cmd предикат выбора из архива обрабатываемых файлов
setArcFilter filter cmd  =  cmd {cmd_archive_filter = filter cmd}

-- |Отобрать файлы в соответствии с фильтром opt_file_filter, за исключением
-- обрабатываемых этой командой архивов и временных файлов, создаваемых при архивации
add_file_filter cmd      =  all_functions [opt_file_filter cmd, not.overwrite_f cmd]

-- |Отобрать файлы в соответствии с фильтром full_file_filter, за исключением
-- обрабатываемых этой командой архивов и временных файлов, создаваемых при архивации
extract_file_filter cmd  =  all_functions [full_file_filter cmd, not.overwrite_f cmd]

-- |Отбирает среди файлов, маски которых указаны в командной строке,
-- соответствующие фильтру opt_file_filter
full_file_filter cmd  =  all_functions
                           [  match_filespecs (opt_match_with cmd) (cmd_filespecs cmd) . fiFilteredName
                           ,  opt_file_filter cmd
                           ]

-- |Отбирает обрабатываемые архивы и временные файлы, создаваемые при архивации,
-- а также файлы, которые могут их перезаписать при распаковке
overwrite_f cmd  =  in_arclist_or_temparc . fiDiskName
  where in_arclist_or_temparc filename =
            fpFullname filename `elem` cmd_arclist cmd
            || all_functions [(temparc_prefix `isPrefixOf`), (temparc_suffix `isSuffixOf`)]
                             (fpBasename filename)

-- |Добавить в фильтр отбора файлов `filter_f` отбор каталогов в соответствии с опциями команды `cmd`
test_dirs filter_f cmd fi  =  if fiIsDir fi
                                then opt_x_include_dirs cmd
                                else filter_f cmd fi

