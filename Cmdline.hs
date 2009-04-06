{-# OPTIONS_GHC -cpp #-}
---------------------------------------------------------------------------------------------------
---- Превращение командной строки в набор команд/опций на выполнение.                          ----
---------------------------------------------------------------------------------------------------
module Cmdline where

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

import qualified CompressionLib
import Utils
import Files
import Charsets
import Errors
import FileInfo
import Compression
import Options
#if defined(FREEARC_WIN)
import System.Win32.File    (fILE_ATTRIBUTE_ARCHIVE)
#endif


-- |Разбирает командную строку и возвращает список заданных в ней команд в виде структур Command.
-- Каждая команда содержит имя команды, спецификацию архивов, список спецификаций файлов и опции.
-- Команды разделяются " ; ", например "a archive -r ; t archive ; x archive"
parseCmdline cmdline  =  (`mapMaybeM` split ";" cmdline) $ \args -> do
  -- Установить display_option в значение по умолчанию, поскольку другого случая может не представиться.
  display_option' =: aDISPLAY_DEFAULT
  let options = takeWhile (/="--") $ filter (match "-*") args
  -- Если командная строка не содержит ничего кроме опций - напечатать help/конфигурацию и выйти
  if args==options then do
      putStr $ if options `contains` "--print-config"
                 then unlines ("":";You can insert these lines into ARC.INI":compressionMethods:builtinMethodSubsts)
                 else aHELP
      return Nothing
    else do

  -- Прочитаем опции из переменной среды FREEARC или заданной в опции -env
  (o0, _) <- parseOptions options [] []
  let no_configs = findReqList o0 "config" `contains` "-"
  env_options <- case (findReqArg o0 "env" "--") of
                    "--" | no_configs -> return ""  -- Опция -cfg- в командной строке отключает использование И arc.ini, И %FREEARC
                         | otherwise  -> getEnv aCONFIG_ENV_VAR  `catch`  (\e -> return "")
                    "-"               -> return ""
                    env               -> getEnv env

  -- Прочитаем конфиг-файл arc.ini или указанный опцией -cfg
  (o1, _)  <- parseOptions (words env_options++options) [] []   -- опция -cfg может быть задана в командной строке или в переменной среды
  cfgfile <- case (findReqArg o1 "config" "--") of
               "--" -> findFile configFilePlaces aCONFIG_FILE
               "-"  -> return ""
               cfg  -> return cfg
  -- Обработаем опцию --charset/-sc, чтобы определить кодировку для чтения конфиг-файла
  let (_, parseFile1, _, _, _)  =  parse_charset_option (findReqList o1 "charset")
  -- Прочитаем опции из конфиг-файла, если он есть, и удалим из него пустые строки и комментарии
  config  <-  cfgfile  &&&  parseFile1 'i' cfgfile >>== map trim >>== deleteIfs [null, match ";*"]

  -- Эти определения превращают содержимое конфиг-файла в набор секций,
  -- содержимое которых может быть запрошено функцией configSection.
  -- К примеру, configSection "[Compression methods]" - список строк в секции "[Compression methods]"
  let configSections = map makeSection $ makeGroups selectSectionHeadings config
      makeSection (x:xs) = (cleanupSectionName x, xs)
      configSection name = lookup (cleanupSectionName name) configSections `defaultVal` []
      -- Декодировать метод сжатия/доп. алгоритмы, используя настройки из секции "[Compression methods]"
      decode_compression_method = decode_method (configSection compressionMethods)
      decode_methods s = ("0/"++s).$decode_compression_method.$lastElems (length (elemIndices '/' s) + 1)

  -- А эти определения позволяют вытащить из секции элемент с заданным именем,
  -- включая случаи, когда левая сторона определения содержит несколько слов,
  -- которым даётся одно и то же определение,
  -- и когда определение повторяется (в этом случае надо слить все строки).
  -- Пример:
  --   a create j = -m4x -ms
  --   a = --display
  -- В этом случае (configElement section "a") возвратит "-m4x -ms --display"
  let sectionElement name = unwords . map snd
                              . filter (strLowerEq name . fst)
                              . concatMap (\line -> let (a,b)  =  split2 '=' line
                                                    in  map (\w->(w,trim b)) (words$ trim a))
      configElement section element  =  configSection section .$ sectionElement element

  -- Если первая значащая строка конфиг-файла не является заголовком секции, то
  -- она описывает опции, общие для всех команд
  let config_1st_line  =  case (head1 config) of
                              '[' : _  -> ""    -- это заголовок секции
                              str      -> str

  -- Имя команды: "a", "create" и так далее. Опции по умолчанию для этой команды, заданные в конфиг-файле
  let cmd = head1$ filter (not.match "-*") args
      default_cmd_options = configElement defaultOptions cmd

  -- Добавим в начало командной строки опции по умолчанию для всех команд,
  -- опции по умолчанию для этой команды и содержимое переменной среды
  let additional_args  =  concatMap words [config_1st_line, default_cmd_options, env_options]

  -- Разберём командную строку, получив набор опций и список "свободных аргументов"
  (o, freeArgs)  <-  parseOptions (additional_args++args) [] []
  -- Сообщить об ошибке, если "свободных аргументов" меньше двух - отсутствует команда или имя архива
  case freeArgs of
    []     ->  registerError$ CMDLINE_NO_COMMAND args
    [cmd]  ->  registerError$ CMDLINE_NO_ARCSPEC args
    otherwise -> return ()
  let (cmd:pure_arcspec:pure_filespecs) = freeArgs

                               -- Аргументы:  название опции и значение по умолчанию
  let grouping              =  findReqArg   o "solid" aDEFAULT_DATA_GROUPING .$ parseSolidOption
      group_dir             =  fst3 grouping
      group_data            =  snd3 grouping
      defaultDirCompressor  =  thd3 grouping ||| aDEFAULT_DIR_COMPRESSION
      orig_dir_compressor   =  findReqArg   o "dirmethod"  defaultDirCompressor .$ decode_compression_method
      compression_options   =  findReqList  o "method"
      orig_sort_order       =  findMaybeArg o "sort"
      yes                   =  findNoArg    o "yes"
      autogenerate_arcname  =  findOptArg   o "autogenerate"  "--" ||| "%Y%m%d%H%M%S"
      indicator             =  findOptArg   o "indicator"     "1"  ||| "0"   -- по умолчанию -i1; -i эквивалентно -i0
      recovery              =  findOptArg   o "recovery"      (if take 2 cmd=="rr"  then drop 2 cmd  else "--")   -- команда "rr..." эквивалентна команде "ch -rr..."
                                                              .$  changeTo [("0.1%","0*4kb"), ("0.01%","0*64kb")]
      orig_workdir          =  findOptArg   o "workdir" "--"   ||| "%TEMP"
      pretest               =  findOptArg   o "pretest"       "1" .$  changeTo [("-","0"), ("+","2"), ("","2")]
      broken_archive        =  findReqArg   o "BrokenArchive" "-"  ||| "0"
      language              =  findReqArg   o "language"      "--"
      noarcext              =  findNoArg    o "noarcext"
      crconly               =  findNoArg    o "crconly"
      nodata                =  findNoArg    o "nodata"
      url_proxy             =  findOptArg   o "proxy"         "--"
      url_bypass            =  findOptArg   o "bypass"        ""
      exclude_path          =  findOptArg   o "ExcludePath"   "--"

      add_exclude_path  =  exclude_path .$ changeTo [("--", "9"), ("", "0")] .$ readInt
      dir_exclude_path  =  if        cmd=="e"                  then 0
                             else if cmdType cmd==EXTRACT_CMD  then add_exclude_path
                             else                                   3

  -- Список действий, которые надо выполнить непосредственно перед началом выполнения команды
  setup_command <- newList
  setup_command <<= (url_setup_proxy      .$ withCString (replace ',' ' ' url_proxy))
  setup_command <<= (url_setup_bypass_list.$ withCString (replace ',' ' ' url_bypass))

  -- Загрузить файл локализации
  setup_command <<= (setLocale language)
  setLocale language

  -- Вручную раскидать опции -o/-op
  let (op, o_rest) = partition is_op_option (findReqList o "overwrite")
      op_opt       = map  (tryToSkip "p") op
      overwrite    = last ("p":o_rest)
      is_op_option ('p':_:_) = True
      is_op_option _         = False

  -- Проверить, что опции принимают одно из допустимых значений
  testOption "overwrite"     "o"  overwrite      (words "+ - p")
  testOption "indicator"     "i"  indicator      (words "0 1 2")
  testOption "pretest"       "tp" pretest        (words "0 1 2 3")
  testOption "BrokenArchive" "ba" broken_archive (words "- 0 1")
  testOption "ExcludePath"   "ep" exclude_path   ([""]++words "1 2 3 --")

  -- Определить имя SFX-модуля, который будет добавлен в начало архива
  let sfxname  =  findOptArg o "sfx" (if take 1 cmd=="s"  then drop 1 cmd  else "--")   -- команда "s..." эквивалентна команде "ch -sfx..."
                    ||| aDEFAULT_SFX  -- при пустом параметре использовать модуль SFX по умолчанию (arc.sfx из стандартного каталога)
  sfx <- if sfxname `notElem` words "- --" && takeFileName sfxname == sfxname
           then findFile libraryFilePlaces sfxname   -- использовать модуль с заданным именем из стандартного каталога
           else return sfxname
  when (sfx=="") $
    registerError$ GENERAL_ERROR ["0342 SFX module %1 is not found", sfxname]

  -- Добавим к базовому имени архива штамп даты/времени, если указана опция -ag
  current_time <- getClockTime
  let add_ag  =  case autogenerate_arcname of
                   "--" -> id
                   _    -> updateBaseName (++ showtime autogenerate_arcname current_time)

  -- Добавим к имени архива расширение по умолчанию, если нет другого расширения и не используется опция --noarcext
  let arcspec  =  addArcExtension noarcext$ add_ag pure_arcspec

  -- Обработать список опций --charset/-sc, возвратив таблицу кодировок
  -- и процедуры чтения/записи файлов с её учётом
  let (charsets, parseFile, unParseFile, parseData, unParseData)  =  parse_charset_option (findReqList o "charset")
  setGlobalCharsets charsets
  setup_command <<= (setGlobalCharsets charsets)

  -- Вручную обработать список опций --display
  let orig_display = foldl f aDISPLAY_DEFAULT (findReqList o "display")
      -- Функция обработки опций --display
      f value ""       =  aDISPLAY_ALL     -- -di без параметров означает включить вывод всей информации
      f value "--"     =  aDISPLAY_DEFAULT -- -di-- означает восстановить значение по умолчанию
      f value ('+':x)  =  nub (value++x)   -- -di+x означает добавить x к флагам
      f value ('-':x)  =  nub value \\ x   -- -di-x означает убрать x из флагов
      f value x        =  nub x            -- иначе просто скопируем параметр в значение опции

  -- Для команды "lb" полностью отключать вывод доп. информации на экран,
  -- для прочих команд листинга включать вывод имени архива в принудительном порядке
  let display = case () of
                  _ | cmd=="lb"              ->  ""
                    | cmdType cmd==LIST_CMD  ->  orig_display++"a"
                    | otherwise              ->  orig_display
  -- Установить display_option, поскольку она нам может понадобиться при выводе warning о содержимом external compressor section
  display_option' =: display
  -- Перед началом выполнения команды восстановить значение display_option, поскольку оно могло быть изменено при парсинге/выполнении других команд
  setup_command <<= (display_option' =: display)

  -- Зарегистрируем описания внешних упаковщиков из секций [External compressor:...]
  let externalSections = filter (matchExternalCompressor.head) $ makeGroups selectSectionHeadings config
      matchExternalCompressor s = and[ head externalCompressor          ==    head s
                                     , init (tail externalCompressor) `match` init (tail s)
                                     , last externalCompressor          ==    last s]
  let registerExternalCompressors makeWarnings = do
        CompressionLib.clearExternalCompressorsTable
        for externalSections $ \section -> do
          result <- CompressionLib.addExternalCompressor (unlines section)
          when (result/=1 && makeWarnings) $ do
            registerWarning (BAD_CFG_SECTION cfgfile section)
  -- Зарегистрируем их сейчас для парсинга командной строки и перерегистриуем при выполнении для собственно сжатия.
  registerExternalCompressors True
  setup_command <<= (registerExternalCompressors False)


---------------------------------------------------------------------------------------------------
-- ОПРЕДЕЛЕНИЕ АЛГОРИТМА СЖАТИЯ -------------------------------------------------------------------
  -- Парсер объёмов памяти, воспринимающий записи типа "75%" (от объёма ОЗУ)
  -- Объём памяти округляется до величины, кратной 4 мб, чтобы исключить получение некруглых величин в результате действия различных Shadow BIOS options
  let parsePhysMem = parseMemWithPercents (toInteger getPhysicalMemory `roundTo` (4*mb))

  -- Парсер опции -md
  let parseDict dictionary  =  case dictionary of
          [c]       | isAlpha c     ->  Just$ 2^(16 + ord c - ord 'a')   -- опция задана одной буквой, -mda..-mdz
          s@(c:_)   | isDigit c     ->  Just$ parsePhysMem s             -- опция начинается c цифры: -md8, -md8m, -md10%
          otherwise                 ->  Nothing                          -- иначе - это не опция -md, а опция -m, начинающаяся с -md...

  -- Цикл, вручную обрабатывающий различные опции, начинающиеся на "-m"
  method <- ref "";    methods <- ref "";  mc' <- newList;  dict <- ref 0;
  mm'    <- ref "--";  threads <- ref 0 ;  ma' <- ref "--"
  for compression_options $ \option ->
    case option of
      -- Опция -mс позволяет быстро отключить отдельные алгоритмы сжатия (-mcd-, -mc-rep)
      'c':rest  | anyf [beginWith "-", endWith "-"] rest
                    ->  mc' <<= rest.$tryToSkip "-".$tryToSkipAtEnd "-"
                                    .$changeTo [("d","delta"), ("e","exe"),  ("l","lzp")
                                               ,("r","rep"),   ("z","dict")
                                               ,("a","$wav"),  ("c","$bmp"), ("t","$text")
                                               ]
      -- Опция -md устанавливает размер словаря как в старом добром RAR :)
      'd':rest  | Just md <- parseDict rest ->  dict =: md
      -- Опция -mm выбирает режим мультимедиа-сжатия.
      'm':rest  | mmflag <- rest.$tryToSkip "=",
                  mmflag `elem` ["","--","+","-","max","fast"]  ->  mm' =: mmflag
      -- Опция -ms задаёт использование быстрого метода сжатия для уже сжатых файлов
      "s"  ->  methods ++= "/$compressed="++join_compressor aCOMPRESSED_METHOD
      "s-" ->  mc' <<= "$compressed"
      -- Опция -ma выбирает режим автоопределения типов файлов
      'a':rest  | maflag <- rest.$tryToSkip "=".$changeTo [("+","--"), ("","--"), ("-","0")],
                  maflag `elem` ["--"]++map show [0..9]  ->  ma' =: maflag
      -- Опция -mt включает/выключает многотредовость и устанавливает количество тредов
      't':rest  | n <- rest.$tryToSkip "=".$changeTo [("-","1"), ("+","0"), ("","0"), ("--","0")],
                  all isDigit n  ->  threads =: readInt n
      -- Опции -m$type=method устанавливают алгоритмы сжатия для отдельных типов файлов
      '$':_ -> case (break (`elem` "=:.") option) of
                 (_type, '=':method) -> methods ++= '/':option                      -- -m$type=method: архивировать файлы этого типа заданным компрессором
                 -- (_type, ':':names)  -> types  ++= split ':' names               -- -m$type:name1:name2: добавить в список файлов этого типа заданные маски
                 -- (_type, ',':exts)   -> types  ++= map ("*."++) $ split '.' exts -- -m$type.ext1.ext2: добавить расширения в список типа
                 otherwise -> registerError$ CMDLINE_BAD_OPTION_FORMAT ("-m"++option)
      -- Все остальные опции, начинающиеся на -m0= или просто -m, задают основной метод сжатия.
      m  ->  method =: m.$tryToSkip "0="
  -- Прочитаем окончательные значения переменных
  dictionary  <- val dict       -- размер словаря (-md)
  cthreads    <- val threads    -- количество compression threads (-mt)
  mainMethod  <- val method     -- основной метод сжатия.
  userMethods <- val methods    -- дополнительные методы для конкретных типов файлов (-m$/-ms)
  mm          <- val mm'        -- мультимедиа-сжатие
  mc          <- listVal mc'    -- список алгоритмов сжатия, которые требуется отключить
  ma          <- val ma'        -- режим автоопределения типов файлов

  -- Уровень сжатия, 0..9
  let clevel = case mainMethod of
                 [d]     | isDigit d -> digitToInt d
                 [d,'p'] | isDigit d -> digitToInt d
                 [d,'x'] | isDigit d -> digitToInt d
                 ['x',d] | isDigit d -> digitToInt d
                 "mx"                -> 9
                 "max"               -> 9
                 _                   -> 4  -- default compression level
  -- Уровень автодетекта, 0..9
  let ma_opt = case ma of "--" -> clevel
                          _    -> readInt ma

  -- Перед началом выполнения команды передать в библиотеку упаковки количество тредов, которое она должна использовать
  setup_command <<= (CompressionLib.setCompressionThreads$  cthreads ||| i getProcessorsCount)   -- By default, use number of threads equal to amount of available processors/cores

  -- Ограничения на память при упаковке/распаковке
  let climit = parseLimit "75%"$ findReqArg o "LimitCompMem"   "--"
      dlimit = parseLimit d_def$ findReqArg o "LimitDecompMem" "--"
      d_def  = if cmdType cmd == ADD_CMD  then "1gb"  else "75%"
      parseLimit deflt x = case x of
        "--" -> parsePhysMem deflt  -- По умолчанию: ограничить использование памяти 75% её физического объёма при упаковке, и 1гб при распаковке
        "-"  -> CompressionLib.aUNLIMITED_MEMORY   -- Не ограничивать использование памяти
        s    -> parsePhysMem s      -- Ограничить использование памяти заданным объёмом

  -- Управление мультимедиа-сжатием
  let multimedia mm = case mm of
        "-"    -> filter ((`notElem` words "$wav $bmp").fst)    -- удалим группы $wav и $bmp из списка методов сжатия.
        "fast" -> (++decode_methods "$wav=wavfast/$bmp=bmpfast") . multimedia "-"
        "max"  -> (++decode_methods "$wav=wav/$bmp=bmp")         . multimedia "-"
        "+"    -> \m -> case () of
                          _ | m.$isFastDecompression  -> m.$multimedia "fast"
                            | otherwise               -> m.$multimedia "max"
        ""     -> multimedia "+"
        "--"   -> id

  -- Удаление заданного алгоритма сжатия.
  let method_change mc x = case mc of
        '$':_  -> -- удалим группу mc (например, "$bmp") из списка методов сжатия.
                  x.$ filter ((/=mc).fst)
        _      -> -- удалим группы, в которых mc - последний алгоритм сжатия (например -mc-tta приведёт к удалению групп, цепочки сжатия которых заканчиваются алгоритмом tta)
                  x.$ (\(x:xs) -> x:(xs.$ filter ((/=mc).method_name.last1.snd)))   -- Не трогаем основную группу сжатия (голову списка)
                  -- удалим алгоритм mc из остальных цепочек сжатия.
                   .$ map (mapSnd$ filter ((/=mc).method_name))

  -- Если задана опция "--nodata", то симулировать сжатие данных.
  -- Если задана опция "--crconly", то ограничиться подсчётом CRC архивируемых файлов.
  -- В противном случае обработать выбранные основной и дополнительные алгоритмы сжатия,
  -- настроив мультимедиа-сжатие и размер словаря, удалив отключенные алгоритмы,
  -- и ограничив потребление памяти
  let data_compressor = if      nodata   then [("", [aFAKE_COMPRESSION])]
                        else if crconly  then [("", [aCRC_ONLY_COMPRESSION])]
                        else ((mainMethod ||| aDEFAULT_COMPRESSOR) ++ userMethods)
                               .$ decode_compression_method
                               .$ multimedia mm
                               .$ applyAll (map method_change mc)
                               .$ setDictionary dictionary
                               .$ limitCompressionMem   climit
                               .$ limitDecompressionMem dlimit

  -- Ограничить сжатие каталога последним методом в основной цепочке и наличным объёмом памяти
  let dir_compressor = orig_dir_compressor.$ limitCompressionMem   climit
                                          .$ limitDecompressionMem dlimit
                                          .$ getMainCompressor
                                          .$ reverse .$ take 1

  -- Макс. размер блока в используемых блочных компрессорах или 0
  let maxBlockSize = getBlockSize data_compressor
  -- Память, требуемая для алгоритма сжатия.
  let compressionMem = getCompressionMem data_compressor

  -- Вычислить, сколько памяти нужно использовать под буфер упреждающего чтения файлов.
  -- Если размер кеша не задан явно опцией --cache, мы используем от 1 мб до 16 мб,
  -- стараясь сделать так, чтобы общее потребление памяти программой не превосходило
  -- половины от её физического объёма (не считая памяти, необходимой для распаковки данных
  -- в обновляемых архивах). Разумеется, при наличии параллельно выполняющихся memory-intensive
  -- tasks (и в частности, параллельно работающих копиях FreeArc) эта тактика не очень удачна.
  -- Лучше было бы смотреть на объём *свободного* физического ОЗУ в момент запуска программы
  let minCache  =  1*mb                             -- Мин. размер кеша  - 1  мб
      maxCache  =  (16*mb) `atLeast` maxBlockSize   -- Макс. размер кеша - 16 мб или размер блока для поблочных алгоритов (lzp/grzip/dict)
      availMem  =  if i(parsePhysMem "50%") >= compressionMem      -- "Свободно памяти" = 50% ОЗУ минус память, требуемая для сжатия.
                       then parsePhysMem "50%" - i compressionMem
                       else 0
      cache     =  clipToMaxInt $ atLeast aBUFFER_SIZE $  -- Кеш должен включать как минимум один буфер
                       case (findReqArg o "cache" "--") of
                           "--" -> availMem.$clipTo minCache maxCache
                           "-"  -> aBUFFER_SIZE
                           s    -> parsePhysMem s

  -- Автоматически включить опцию --recompress для команд, копирующих архив,
  -- если указаны опции -m../--nodata/--crconly
  let recompress = findNoArg o "recompress"
                   || (is_COPYING_COMMAND cmd  &&  (mainMethod>"" || nodata || crconly))
  -- Не перепаковывать существующие солид-блоки в архиве при --append
  -- и в командах копирования архива, если --recompress не задано явно
  let keep_original = findNoArg o "append"
                      || (is_COPYING_COMMAND cmd  &&  not recompress)


---------------------------------------------------------------------------------------------------
-- ПРЕДИКАТЫ ДЛЯ ОПРЕДЕЛЕНИЯ НОМЕРА ГРУППЫ (find_group) И ТИПА ФАЙЛА (find_type) ------------------
  -- Определить, какой файл со списком групп (типа arc.groups) будет использоваться.
  actual_group_file <- case (findReqArg o "groups" "--") of
      "--" -> findFile configFilePlaces aDEFAULT_GROUPS_FILE  -- использовать файл групп по умолчанию (arc.groups из каталога, где находится программа)
      "-"  -> return ""      -- файл групп отключен опцией    --groups-
      x    -> return x       -- файл групп указан явно опцией --groups=FILENAME

  -- Прочитать список групп из файла групп
  group_strings  <-  if actual_group_file > ""
                         then parseFile 'i' actual_group_file      -- распарсить файл групп с учётом кодировки символов и разделителей строк
                                >>== map translatePath             -- превратить все '\' в '/'
                                >>== deleteIfs [match ";*", null]  -- удалить строки комментариев и пустые
                         else return [reANY_FILE]     -- если файл групп не используется, то все файлы принадлежат одной общей группе
  -- Список предикатов, проверяющих вхождение в каждую группу
  let group_predicates  =  map (match_FP fpBasename) group_strings
  -- Группа по умолчанию, куда попадают все файлы, не совпадающие ни с одной из масок.
  -- Указывается псевдо-маской "$default", при её отсутствии считается, что эта маска добавлена в конец списка
  let lower_group_strings = (map strLower group_strings) ++ ["$default"]
      default_group = "$default" `elemIndex` lower_group_strings .$ fromJust
  -- Функция "PackedFilePath -> номер группы из arc.groups"
  let find_group    = findGroup group_predicates default_group

  -- Список типов файлов ($text, $exe и так далее), соответствующих каждой группе из arc.groups
  let group_type_names = go "$binary" lower_group_strings  -- начальная группа - "$binary"
      go t []     = []           -- пройти по списку групп, заменяя маски файлов
      go t (x:xs) = case x of    --   на предшествующие им имена типов файлов ("$text", "$rgb" и так далее)
                      '$':_ | x/="$default" -> x : go x xs
                      _                     -> t : go t xs
  -- Список номеров методов сжатия из списка `data_compressor`, соответствующих каждой группе из arc.groups
  let group_types =  map typeNum group_type_names
      typeNum t   =  t `elemIndex` (map fst data_compressor) `defaultVal` 0
  -- Список предикатов, проверяющих что файл принадлежит одному из типов, перечисленных в `data_compressor`
  let type_predicates  =  const False : map match_type [1..maximum group_types]
      match_type t     =  any_function$ concat$ zipWith (\a b->if a==t then [b] else []) group_types group_predicates
  -- Функция "PackedFilePath -> номер компрессора в списке `data_compressor`"
  let find_type  =  findGroup type_predicates 0


-------------------------------------------------------------------------------------
-- ФИЛЬТР ФАЙЛОВ
  let match_with            =  findNoArg    o "fullnames"          .$bool fpBasename fpFullname
      orig_include_list     =  findReqList  o "include"
      orig_exclude_list     =  findReqList  o "exclude"
      include_dirs          =  findNoArgs   o "dirs" "nodirs"
      clear_archive_bit     =  findNoArg    o "ClearArchiveBit"
      select_archive_bit    =  findNoArg    o "SelectArchiveBit"
      filesize_greater_than =  findReqArg   o "SizeMore"           "--"
      filesize_less_than    =  findReqArg   o "SizeLess"           "--"
      time_before           =  findReqArg   o "TimeBefore"         "--"
      time_after            =  findReqArg   o "TimeAfter"          "--"
      time_newer            =  findReqArg   o "TimeNewer"          "--"
      time_older            =  findReqArg   o "TimeOlder"          "--"

  -- Заменим ссылки на лист-файлы (@listfile/-n@listfile/-x@listfile) их содержимым
  listed_filespecs <- pure_filespecs   .$ replace_list_files parseFile >>== map translatePath
  include_list     <- orig_include_list.$ replace_list_files parseFile >>== map translatePath
  exclude_list     <- orig_exclude_list.$ replace_list_files parseFile >>== map translatePath

  -- Предикаты отбора включаемых (-n) и исключаемых (-x) файлов. Для -n проверяем orig_include_list, поскольку при пустом листфайле ни один файл не должен проходить фильтр
  let match_included  =  orig_include_list &&& [match_filespecs match_with include_list]
      match_excluded  =  exclude_list      &&& [match_filespecs match_with exclude_list]

#if defined(FREEARC_WIN)
  -- Отбор файлов по атрибутам
  let attrib_filter | select_archive_bit = [\attr -> attr.&.fILE_ATTRIBUTE_ARCHIVE /= 0]
                    | otherwise          = []
#else
  let attrib_filter = []
#endif

  -- Отбор файлов по размеру
  let size_filter _  "--"   = []
      size_filter op option = [`op` parseSize option]

  -- Отбор файлов по времени модификации, time в формате YYYYMMDDHHMMSS
  let time_filter _  "--" = []
      time_filter op time = [`op` (time.$makeCalendarTime.$toClockTime.$convert_ClockTime_to_CTime)]
      -- Преобразует строчку вида YYYY-MM-DD_HH:MM:SS в CalendarTime и выставляет корректное ctTZ в зависимости от её времени года (для этого toCalendarTime.toClockTime делается дважды)
      makeCalendarTime str = ct {ctTZ = ctTZ$ unsafePerformIO$ toCalendarTime$ toClockTime ct2}
          where        ct2 = ct {ctTZ = ctTZ$ unsafePerformIO$ toCalendarTime$ toClockTime ct}
                       ct = CalendarTime
                            { ctYear    = readInt (take 4 s)
                            , ctMonth   = readInt (take 2 $ drop 4 s) .$ (\x->max(x-1)0) .$ toEnum
                            , ctDay     = readInt (take 2 $ drop 6 s)
                            , ctHour    = readInt (take 2 $ drop 8 s)
                            , ctMin     = readInt (take 2 $ drop 10 s)
                            , ctSec     = readInt (take 2 $ drop 12 s)
                            , ctPicosec = 0
                            , ctWDay    = error "ctWDay"
                            , ctYDay    = error "ctYDay"
                            , ctTZName  = error "ctTZName"
                            , ctTZ      = 0
                            , ctIsDST   = error "ctIsDST"
                            }
                       s = filter isDigit str ++ repeat '0'

  -- Отбор файлов по "старости", time в формате [<ndays>d][<nhours>h][<nminutes>m][<nseconds>s]
  let oldness_filter _  "--" = []
      oldness_filter op time = [`op` (time.$calcDiff.$(`addToClockTime` current_time).$convert_ClockTime_to_CTime)]

      calcDiff  =  foldl updateTD noTimeDiff . recursive (spanBreak isDigit)
      updateTD td x = case (last x) of
                        'd' -> td {tdDay  = -readInt (init x)}
                        'h' -> td {tdHour = -readInt (init x)}
                        'm' -> td {tdMin  = -readInt (init x)}
                        's' -> td {tdSec  = -readInt (init x)}
                        _   -> td {tdDay  = -readInt x}

  -- Фильтр отбора файлов, включающий все критерии отбора,
  -- указанные в командной строке, кроме отбора по filespecs.
  -- Для последних используется отдельная функция,
  -- потому что они по-разному используются в командах разного типа.
  let file_filter = all_functions$
                      concat [                     attrib_filter          .$map (.fiAttr)
                             , map (not.)          match_excluded         .$map (.fiFilteredName)
                             , nst_filters
                             ]
      nst_filters =   concat [                     match_included         .$map (.fiFilteredName)
                             , size_filter    (>)  filesize_greater_than  .$map (.fiSize)
                             , size_filter    (<)  filesize_less_than     .$map (.fiSize)
                             , time_filter    (>=) time_after             .$map (.fiTime)
                             , time_filter    (<)  time_before            .$map (.fiTime)
                             , oldness_filter (>=) time_newer             .$map (.fiTime)
                             , oldness_filter (<)  time_older             .$map (.fiTime)
                             ]

  -- Если имена обрабатываемых файлов не указаны и команда не cw/d, то обрабатывать все файлы
  filespecs <- case listed_filespecs of
      [] | cmd `elem` (words "cw d")  ->  registerError$ CMDLINE_NO_FILENAMES args
         | otherwise                  ->  return aDEFAULT_FILESPECS
      _  | cmd.$is_CMD_WITHOUT_ARGS   ->  registerError$ CMDLINE_GENERAL ["0377 command \"%1\" shouldn't have additional arguments", cmd]
         | otherwise                  ->  return listed_filespecs

  -- Включать каталоги в обработку? Эта переменная используется только при листинге/распаковке
  let x_include_dirs  =  case include_dirs of
           Just x  -> x   -- в соответствии с оциями --dirs/--nodirs
           _       -> -- ДА, если обрабатываются все файлы, нет фильтров -n/-s*/-t* и команда не "e"
                      filespecs==aDEFAULT_FILESPECS && null nst_filters && cmd/="e"


-------------------------------------------------------------------------------------
-- ШИФРОВАНИЕ
  -- Алгоритм шифрования; проверка валидности и приведение к каноническому виду ("aes" -> "aes-256/ctr")
  let ea = findReqArg o "encryption" aDEFAULT_ENCRYPTION_ALGORITHM
  encryptionAlgorithm <- join_compressor ==<< (foreach (split_compressor ea) $ \algorithm -> do
    unless (isEncryption algorithm) $ do
      registerError$ CMDLINE_GENERAL ["0378 bad name or parameters in encryption algorithm %1", algorithm]
    return$ CompressionLib.canonizeCompressionMethod algorithm)

  -- Пароли для данных и заголовка архива
  let (dpwd,hpwd) = case (findReqArg o "password"        "--" .$changeTo [("-", "--")]
                         ,findReqArg o "HeadersPassword" "--" .$changeTo [("-", "--")])
                    of
                       (p,    "--")  ->  (p,  "--")    --  -p...
                       ("--", p   )  ->  (p,  p   )    --  -hp..,
                       (p,    ""  )  ->  (p,  p   )    --  -p[PWD] -hp
                       ("",   p   )  ->  (p,  p   )    --  -p -hpPWD
                       (p1,   p2  )  ->  (p1, p2  )    --  -pPWD1 -hpPWD2

  -- Запретить запрос паролей, необходимых для распаковки, если указано -op-/-p-/-hp-
  let dont_ask_passwords  =  last ("":op_opt) == "-" || findReqArg o "OldPassword" "" == "-"  ||  findReqArg o "password" "" == "-"  ||  findReqArg o "HeadersPassword" "" == "-"
  -- Список паролей, используемых при распаковке
  mvar_unpack_passwords  <-  newMVar$ deleteIfs [=="",=="?",=="-",=="--"]$ op_opt ++ findReqList o "OldPassword" ++ findReqList o "password" ++ findReqList o "HeadersPassword"
  -- Содержимое ключевых файлов, используемых при распаковке
  oldKeyfileContents     <-  mapM fileGetBinary (findReqList o "OldKeyfile" ++ findReqList o "keyfile")
  -- Содержимое ключевого файл, используемого при упаковке
  keyfileContents        <-  unlessNull fileGetBinary (findReqArg o "keyfile" "")
  -- Требуется ввод пароля с клавиатуры при -p? и при -p, если нет ключевого файла
  let askPwd pwd          =  pwd=="?" || (pwd=="" && keyfileContents=="")
  -- Рецепт подготовки команды к использованию шифрования, или Nothing до создания рецепта
  receipt                <-  newMVar Nothing

  -- Подготавливает command к использованию шифрования, при необходимости
  -- запрашивая пароль у пользователя и считывая keyfiles
  let cookPasswords command (ask_encryption_password, ask_decryption_password, bad_decryption_password) = do
        modifyMVar receipt $ \x -> do
          f <- x.$maybe makeReceipt return   -- создать рецепт подготовки команды к шифрованию, если его ещё нет
          return (Just f, f command)         -- применить рецепт к command и запомнить его для последующих применений
       where
        makeReceipt = do
          -- Запросим у пользователя пароль, если он потребуется нам дальше
          let ask_password | cmdType cmd==ADD_CMD = ask_encryption_password parseData
                           | otherwise            = ask_decryption_password parseData
          asked_password  <-  any askPwd [dpwd,hpwd]  &&&  ask_password
          -- Добавим в список паролей распаковки введённый пользователем пароль и пустой пароль, если для расшифровки может быть использован keyfile
          asked_password      &&&  modifyMVar_ mvar_unpack_passwords (return.(asked_password:))
          oldKeyfileContents  &&&  modifyMVar_ mvar_unpack_passwords (return.("":))
          -- Добавить к паролю содержимое keyfile и заменить обозначения "--"/"?"
          let cook "--"             = ""                                -- шифрование отключено
              cook pwd | askPwd pwd = asked_password++keyfileContents   -- пароль, введёный с клавиатуры + содержимое keyfile
                       | otherwise  = pwd++keyfileContents              -- пароль из командной строки + содержимое keyfile
          return$ \command ->
                   command { opt_data_password    = cook dpwd
                           , opt_headers_password = cook hpwd
                           , opt_decryption_info  = (dont_ask_passwords, mvar_unpack_passwords, oldKeyfileContents, ask_decryption_password parseData, bad_decryption_password)}


-------------------------------------------------------------------------------------
-- ПРОЧЕЕ ПО МЕЛОЧИ
  -- Алгоритм обновления архива
  let update_type = case cmd of
        "f"                       -> 'f'  -- команда f: обновить файлы более свежими версиями, новых файлов не добавлять
        "u"                       -> 'u'  -- команда u: обновить файлы более свежими версиями и добавить новые файлы
        _ | findNoArg o "freshen" -> 'f'  -- опция  -f: см. выше
          | findNoArg o "update"  -> 'u'  -- опция  -u: см. выше
          | findNoArg o "sync"    -> 's'  -- опция --sync: привести файлы в архиве в соответствие с файлами на диске
          | otherwise             -> 'a'  -- иначе: заменить файлы в архиве на взятые с диска и добавить новые файлы

  -- Закрыть архив от изменений, если использована опция "-k" или команда "k"
  let lock_archive  =  findNoArg o "lock" || cmd=="k"

  -- Удалить архивируемые файлы, если использована опция "-d[f]" или команда "m[f]"
  delete_files  <-  case (findNoArg o "delete"   || cmd=="m"
                         ,findNoArg o "delfiles" || cmd=="mf")
                      of
                         (False, False) -> return NO_DELETE
                         (False, True ) -> return DEL_FILES
                         (True , False) -> return DEL_FILES_AND_DIRS
                         (True , True ) -> registerError$ CMDLINE_INCOMPATIBLE_OPTIONS "m/-d" "mf/-df"

  -- Запретим использование несовместимых опций
  when (clear_archive_bit && delete_files/=NO_DELETE) $
      registerError$ CMDLINE_INCOMPATIBLE_OPTIONS "m[f]/-d[f]" "-ac"

  -- Каталог для временных файлов - может быть указан явно или через переменную среды
  workdir <- case orig_workdir of
               "--"       -> return ""    -- По умолчанию (означает создавать файлы сразу в выходном каталоге)
               '%':envvar -> getEnv envvar
               dir        -> return dir

  -- Определить порядок сортировки файлов в архиве
  let sort_order  =  case (orig_sort_order, group_data) of
        (Just "-", _)  -> ""                    -- Если порядок сортировки задан как "-", то отключить сортировку
        (Just  x,  _)  -> x                     -- Если порядок сортировки был явно указан, то использовать его
        (_, [GroupNone]) -> ""                  -- Если не используется solid-сжатие - отключить сортировку
        _  -> if getMainCompressor data_compressor
                 .$anyf [==aNO_COMPRESSION, isFakeCompressor, isVeryFastCompressor]
                then ""                         -- Если -m0/--nodata/--crconly/tor:1..4/lzp:h13..15 - также отключить сортировку
                else aDEFAULT_SOLID_SORT_ORDER  -- Иначе - использовать стандартный порядок сортировки для solid-архивов

  -- Проверим, что опция "-rr" принимает одно из допустимых значений
  let rr_ok = recovery `elem` ["","-","--"]
              || snd(parseNumber recovery 'b') `elem` ['b','%','p']
              || ';' `elem` recovery
              || '*' `elem` recovery
  unless rr_ok $ do
    registerError$ INVALID_OPTION_VALUE "recovery" "rr" ["MEM", "N", "N%", "MEM;SS", "N%;SS", "N*SS", "-", ""]

  -- Состояние запроса к пользователю о перезаписи файлов
  ref_overwrite  <-  newIORef$ case (yes,   overwrite) of
                                    (_,     "+")  ->  "a"
                                    (_,     "-")  ->  "s"
                                    (True,  _  )  ->  "a"
                                    (False, "p")  ->  " "

  -- Список действий, которые надо выполнить непосредственно перед началом выполнения команды
  setup_command'  <-  listVal setup_command >>== sequence_


------------------------------------------------------------------------------------------------
-- Занесём всё это в структуру, представляющую выполняемую команду в последующей части программы
  return$ Just$ Command {
      cmd_args                 = args
    , cmd_additional_args      = additional_args
    , cmd_name                 = cmd
    , cmd_arcspec              = arcspec
    , cmd_arclist              = error "Using uninitialized cmd_arclist"
    , cmd_arcname              = error "Using uninitialized cmd_arcname"
    , cmd_archive_filter       = error "Using uninitialized cmd_archive_filter"
    , cmd_filespecs            = filespecs
    , cmd_added_arcnames       = return []
    , cmd_diskfiles            = return []
    , cmd_subcommand           = False
    , cmd_setup_command        = setup_command'

    , opt_scan_subdirs         = findNoArg    o "recursive"
    , opt_add_dir              = findNoArg    o "adddir"
    , opt_add_exclude_path     = add_exclude_path
    , opt_dir_exclude_path     = dir_exclude_path
    , opt_arc_basedir          = findReqArg   o "arcpath"   "" .$ translatePath .$ dropTrailingPathSeparator
    , opt_disk_basedir         = findReqArg   o "diskpath"  "" .$ translatePath .$ dropTrailingPathSeparator
    , opt_no_nst_filters       = null nst_filters
    , opt_file_filter          = file_filter
    , opt_group_dir            = group_dir
    , opt_group_data           = group_data
    , opt_data_compressor      = data_compressor
    , opt_dir_compressor       = dir_compressor
    , opt_autodetect           = ma_opt
    , opt_include_dirs         = include_dirs
    , opt_indicator            = indicator
    , opt_display              = display
    , opt_overwrite            = ref_overwrite
    , opt_keep_time            = findNoArg    o "keeptime"
    , opt_time_to_last         = findNoArg    o "timetolast"
    , opt_test                 = findNoArg    o "test"
    , opt_pretest              = readInt pretest
    , opt_keep_broken          = findNoArg    o "keepbroken"
    , opt_match_with           = match_with
    , opt_append               = findNoArg    o "append"
    , opt_recompress           = recompress
    , opt_keep_original        = keep_original
    , opt_noarcext             = noarcext
    , opt_nodir                = findNoArg    o "nodir"
    , opt_cache                = cache
    , opt_update_type          = update_type
    , opt_x_include_dirs       = x_include_dirs
    , opt_sort_order           = sort_order
    , opt_reorder              = False
    , opt_find_group           = find_group . fiFilteredName
    , opt_groups_count         = length group_strings
    , opt_find_type            = find_type  . fiFilteredName
    , opt_types_count          = maximum group_types + 1
    , opt_group2type           = (listArray0 group_types!)
    , opt_arccmt_file          = findOptArg   o "arccmt"            (if cmd=="c"  then ""  else "--")   -- команда "c" эквивалентна команде "ch -z"
    , opt_arccmt_str           = findReqArg   o "archive-comment"   ""
    , opt_lock_archive         = lock_archive
    , opt_sfx                  = sfx
    , opt_logfile              = findReqArg   o "logfile"           ""
    , opt_delete_files         = delete_files
    , opt_workdir              = workdir
    , opt_clear_archive_bit    = clear_archive_bit
    , opt_language             = language
    , opt_recovery             = recovery
    , opt_broken_archive       = broken_archive
    , opt_original             = findOptArg   o "original"          "--"
    , opt_save_bad_ranges      = findReqArg   o "save-bad-ranges"   ""
    , opt_limit_compression_memory   = climit
    , opt_limit_decompression_memory = dlimit

    , opt_encryption_algorithm = encryptionAlgorithm
    , opt_cook_passwords       = cookPasswords
    , opt_data_password        = error "opt_data_password used before cookPasswords!"
    , opt_headers_password     = error "opt_headers_password used before cookPasswords!"
    , opt_decryption_info      = error "opt_decryption_info used before cookPasswords!"

    , opt_parseFile            = parseFile
    , opt_unParseFile          = unParseFile
    , opt_parseData            = parseData
    , opt_unParseData          = unParseData
    }


{-# NOINLINE testOption #-}
-- |Проверить, что опция принимает одно из разрешённых значений
testOption fullname shortname option valid_values = do
  unless (option `elem` valid_values) $ do
    registerError$ INVALID_OPTION_VALUE fullname shortname valid_values

{-# NOINLINE addArcExtension #-}
-- |Если имя архива не содержит расширения и не используется опция --noarcext,
-- то добавить к нему расширение по умолчанию
addArcExtension noarcext filespec =
  case (hasExtension filespec, noarcext) of
    (False, False)  ->  filespec ++ aDEFAULT_ARC_EXTENSION
    _               ->  filespec

{-# NOINLINE replace_list_files #-}
-- |Заменить ссылки на лист-файлы ("@listfile") их содержимым
replace_list_files parseFile  =  concatMapM $ \filespec ->
  case (startFrom "@" filespec) of
    Just listfile  ->  parseFile 'l' listfile >>== deleteIf null
    _              ->  return [filespec]

-- |Разбор параметров опции "-s"
parseSolidOption opt =
  case (split ';' opt) of
    []        ->  ([aDEFAULT_DIR_GROUPING], [GroupAll], "")   -- "-s" включает общий солид-блок для всех файлов в одном каталоге архива
    ["-"]     ->  ([aDEFAULT_DIR_GROUPING], [GroupNone], "")  -- "-s-" отключает солид-сжатие, для каталогов используется стандартная группировка
    ["7z"]    ->  ([GroupAll],  [GroupAll], "")               -- "-s=7z"  делает общий сжатый каталог и один солид-блок для всех файлов в архиве
    ["cab"]   ->  ([GroupAll],  [GroupAll],  "0")   --  -dm0  -- "-s=cab" делает общий несжатый каталог и один солид-блок для всех файлов в архиве
    ["zip"]   ->  ([GroupAll],  [GroupNone], "0")   --  -dm0  -- "-s=zip" делает отдельный солид-блок для каждого файла в архиве, и общий несжатый каталог
    ["arj"]   ->  ([GroupNone], [GroupNone], "0")   --  -dm0  -- "-s=arj" делает отдельный солид-блок и каталог для каждого файла в архиве
    [dat]     ->  ([aDEFAULT_DIR_GROUPING], parse dat, "")    -- "-sXXX" задаёт группировку только для солид-блоков, каталоги группируются стандартно
    [dir,dat] ->  (parse dir, parse dat, "")                  -- "-sXXX;YYY" задаёт группировку и для каталогов, и для солид-блоков
  where
    -- Разборщик описания группировки файлов:
    --   "-s/-se/-s10m/-s100f" - группировать все/по расширению/по 10 мб/по 100 файлов, соответственно.
    -- `parse1` обрабатывает одно описание группировки,
    -- а `parse` - их последовательность, например -se100f10m
    parse = map parse1 . recursive split
      where split ('e':xs) = ("e",xs)
            split xs       = spanBreak (anyf [isDigit, =='e']) xs
    parse1 s = case s of
                ""  -> GroupAll
                "e" -> GroupByExt
                _   -> case (parseNumber s 'f') of
                         (num, 'b') -> GroupBySize (i num)
                         (1,   'f') -> GroupNone
                         (num, 'f') -> GroupByNumber (i num)

