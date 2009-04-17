----------------------------------------------------------------------------------------------------
---- Защита и восстановление архивов.                                                           ----
---- Реализация функциональности опции -rr и команды r.                                         ----
---- Процедура writeRecoveryBlocks записывает в конец архива recovery info,                     ----
----   необходимую для восстановления сбойных участков в архиве.                                ----
---- Процедура pretestArchive проверяет, исправлен ли архив, используя recovery info            ----
----   и/или тестируя содержимое архива                                                         ----
---- Процедура runArchiveRecovery восстанавливает архив, используя recovery info.               ----
----------------------------------------------------------------------------------------------------
module ArcRecover where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

import Utils
import Files
import Charsets          (linesCRLF)
import Errors
import ByteStream
import Compression
import Options
import UI
import ArhiveStructure
import ArhiveDirectory
import ArcvProcessRead   (writeControlBlock)
import ArcCreate         (testArchive, writeSFX)

-- |Версии recovery info, которые мы умеем использовать
aREC_VERSIONS = words "0.36 0.39"

-- |Версия recovery info, которую мы записываем в архив, зависит от количества recovery sectors
aREC_VERSION 0 = "0.39"
aREC_VERSION _ = "0.36"

{-
recovery info записывается в архив следующим образом:

1. После выбора размера recovery сектора (может быть 512/1k/2k/4k/... байт)
   весь архив разбивается на сектора этого размера. Для каждого из них
   подсчитывается CRC32, затем сохраняемая в recovery info.
2. Одновременно с этим создаётся N recovery секторов, и каждый сектор архива
   (с номером i) отображается на (i `mod` N)-ый сектор recovery. Все сектора архива,
   отображённые на один recovery сектор, xor'ятся между собой, и в recovery info
   записывается результирующий сектор. Таким образом, recovery info включает в
   себя N recovery секторов, каждый из которых содержит "обобщённую" информацию о
   соответствующих ему секторах архива.

Проверка целостности архива сводится к подсчёту CRC секторов архива. CRC,
отличное от оригинального (сохранённого в recovery info), означает, что этот
сектор содержит сбои.

Восстановление архива возможно, если на один recovery сектор приходится не
более одного сбойного сектора архива. В этом случае правильное содержимое
сбойного сектора вычисляется xor'еньем содержимого recovery сектора и всех
остальных секторов архива, соответствующих этому recovery сектору.
-}

----------------------------------------------------------------------------------------------------
---- Запись recovery информации в конец архива -----------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Размер одного RECOVERY сектора в RAR (используется только для разбора опции -rr в RAR-совместимом виде)
aRAR_REC_SECTOR_SIZE = 512

-- |Записать в архив блок RECOVERY
writeRecoveryBlocks archive oldarc init_pos command params bufOps = do
  rrPos <- archiveGetPos archive   -- Позиция начала recovery info в архиве
  let -- Размер архива и 1% от него
      arcsize      = rrPos - init_pos
      arcsize_1p   = arcsize `divRoundUp` 100
      -- Используемый по умолчанию объём recovery info зависит от размера архива
      recommended_rr = if arcsize<3*10^5 then "4%" else
                       if arcsize<2*10^6 then "2%"
                                         else "1%"
      -- Старая настройка объёма recovery info, сохранённая в самом архиве
      old_recovery = ftRecovery (arcFooter oldarc)
      -- Новая настройка объёма recovery info, определяемая опцией -rr и старой настройки
      recovery = case opt_recovery command of
                   "-"     -> ""                                -- -rr-: отключить добавление к архиву recovery info
                   "--"    -> old_recovery                      -- по умолчанию: использовать прежнюю настройку опции, занесённую в сам архив
                   ""      -> old_recovery ||| recommended_rr   -- -rr: использовать прежнюю настройку или рекомендуемый объём, если раньше recovery info в архив не добавлялась
                   "+"     -> old_recovery ||| recommended_rr   -- -rr+: то же самое
                   "0.1%"  -> "0*4096"                          -- -rr0.1%: минимальный размер RR для восстановления только через internet
                   "0.01%" -> "0*65536"                         -- -rr0.01%: ещё меньшая RR
                   r       -> r                                 -- -rr...: добавить в архив указанный объём recovery info
  -- Сразу выйти, если звёзды показали, что никакой recovery info добавлять и не нужно
  if recovery==""  then return ([],"")  else do
      -- Расшифруем запись опции -rr в виде recovery_amount;sector_size или rec_sectors*sector_size,
      -- запоминая размер recovery сектора и/или их количество, если эти величины заданы явно
  let (recovery_amount, explicit_rec_size, explicit_sector_size) = case () of
        _ | ';' `elem` recovery -> let (r,ss)  = split2 ';' recovery .$ mapSnd (i.parseSize) in
                                   (r,  Nothing,       Just ss)
          | '*' `elem` recovery -> let (ns,ss) = split2 '*' recovery .$ mapFstSnd (i.parseSize) in
                                   ("", Just (ns*ss),  Just ss)
          | otherwise           -> (recovery, Nothing, Nothing)
      -- Пересчитаем размер recovery info в байты
      wanted_rec_size = (case parseNumber recovery_amount 's' of
                             (num,'b') -> num                        -- уже задан в байтах
                             (num,'s') -> num*aRAR_REC_SECTOR_SIZE   -- задан в секторах по 512 байт
                             (num,'%') -> arcsize_1p * num           -- задан в процентах
                             (num,'p') -> arcsize_1p * num           -- -.-
                        -- ... но должен быть не больше половины объёма ОЗУ 8-)
                        ) `minI` (getPhysicalMemory `div` 2)
      -- Размер recovery сектора зависит того, сколько процентов от размера архива занимает
      -- recovery info - чем она больше, тем меньший размер сектора можно сделать,
      -- не опасаясь, что crc секторов архива будут занимать слишком большую часть recovery info.
      -- Уменьшение размера recovery сектора увеличивает количество секторов, на которые
      -- разбивается архив, и следовательно уменьшает вероятность их пересечения на общем
      -- recovery секторе, то есть увеличивает шансы на восстановление архива.
      -- При маленьком относительном объёме recovery info (в частности, при большом размере архива),
      -- размер recovery сектора, наоборот, растёт до бесконечности.
      -- Зависимость "объём recovery info -> размер сектора" следующая: 4% -> 512, 2% -> 1024, 1% -> 2048...
      sector_size =  explicit_sector_size `defaultVal`
                     case wanted_rec_size of
                       0 -> 4096  -- При задании -rr0% запоминаются только CRC 4-килобайтных секторов, что составит 0.1% от размера архива
                       _ -> (2^lb(40*arcsize `div` wanted_rec_size)) `atLeast` 512
      -- Размер уже существующей части архива в секторах
      arc_sectors = i$ arcsize `divRoundUp` sector_size
      -- Сколько байт займёт запись CRC этих секторов
      crcs_size0  = arc_sectors * sizeOf (undefined::CRC)
      -- Реальный размер блока recovery
      rec_size    = explicit_rec_size `defaultVal`
                    max wanted_rec_size (crcs_size0+0*sector_size)  -- Блок recovery должен вмещать как минимум CRC секторов архива плюс 0 recovery-секторов
      -- Количество recovery секторов и их общий объём
      rec_sectors = (rec_size - crcs_size0) `divRoundUp` sector_size
      rec_sectors_size = rec_sectors*sector_size
      -- Окончательный размер буфера CRC, включающий CRC самих recovery секторов
      crcs_size   = crcs_size0 + rec_sectors * sizeOf (undefined::CRC)

  -- Все параметры определены, теперь - реальная работа
  condPrintLineLn "r"$ "Protecting archive with "++show3 rec_sectors++" recovery sectors ("++showMemory (i rec_sectors*i sector_size::Integer)++")..."
  uiStage              "0386 Protecting archive from damages"
  withPool $ \pool -> do
  sectors    <- pooledMallocBytes pool rec_sectors_size;   memset sectors 0 (i rec_sectors_size)
  buf        <- pooledMallocBytes pool sector_size
  crcbuf     <- pooledMallocBytes pool (crcs_size+1)
  crc_stream <- ByteStream.createMemBuf crcbuf (crcs_size+1)
  -- Начинаем i не с нуля для того, чтобы последний сектор в архиве отобразился на последний сектор
  -- в recovery info (это позволяет гарантировать восстановление архива при сбое в любых rec_sectors
  -- последовательных секторах, включая порчу последовательности секторов, приходящихся
  -- на конец данных и начало recovery info архива)
  i' <- ref ((-arc_sectors) `mod` rec_sectors)
  -- Цикл по секторам уже записанной части архива, вычисляющий CRC каждого из них,
  -- и xor-ящий каждый сектор в соответствующий ему сектор recovery info
  archiveSeek archive init_pos
  uiWithProgressIndicator command arcsize $ do
    doChunks arcsize sector_size $ \bytes -> do
      uiUpdateProgressIndicator (i bytes)
      failOnTerminated
      len <- archiveReadBuf archive buf bytes
      crc <- calcCRC buf bytes
      ByteStream.write crc_stream crc
      when (rec_sectors>0) $ do
        i <- val i';  i' =: (i+1) `mod` rec_sectors
        memxor (sectors +: i*sector_size) buf bytes
  -- Записать CRC самих recovery секторов
  for [0..rec_sectors-1] $ \i -> do
    crc <- calcCRC (sectors +: i*sector_size) sector_size
    ByteStream.write crc_stream crc
  -- Записать два отдельных блока recovery - с xor-секторами и с CRC блоков архива.
  -- Во второй блок также включается служебная информация, описывающая структуру recovery info
  -- (номер версии, размер и адрес начала в архиве защищаемой информации,
  --  количество секторов и размер сектора в каждом "отделении", на которые разбита recovery info).
  -- При этом для восстановления данных достаточно целостности только второго, меньшего блока
  -- (запоротые recovery сектора из первого блока просто фактически не будут использоваться,
  --  поскольку CRC секторов данных, "восстановленных" с их участием, просто будет неправильным).
  archiveSeek archive rrPos
  r0 <- writeControlBlock RECOVERY_BLOCK aNO_COMPRESSION params $ do
          archiveWriteRecoveryBlock (Nothing::Maybe Int) sectors rec_sectors_size bufOps
  curpos <- archiveGetPos archive
  let addinfo = (aREC_VERSION rec_sectors, arcsize::Integer, curpos-init_pos::Integer, [(toInteger sector_size, toInteger rec_sectors)])
  r1 <- writeControlBlock RECOVERY_BLOCK aNO_COMPRESSION params $ do
          archiveWriteRecoveryBlock (Just addinfo) crcbuf crcs_size bufOps
  return ([r0,r1],recovery)


-- |Чтение заголовка recovery info, описывающего все её параметры
readControlInfo crc_stream crcs_block = do
  ByteStream.rewindMemory crc_stream
  -- Второй recovery блок помимо CRC секторов архива также содержит мета-информацию.
  -- Для защиты от её неправильной интепретации она начинается с номера версии программы,
  -- совместимой с этим форматом мета-информации
  version <- ByteStream.read crc_stream
  if version `notElem` aREC_VERSIONS  then return$ Left version  else do
  -- Прочитаем заголовок второго recovery блока, содержащий все необходимые данные
  -- об этой recovery информации - начальный адрес защищённых данных (закодирован как
  -- смещение (offset) от начала второго recovery блока до начала защищённых данных),
  -- размер защищённых данных (arcsize), и наконец размер и количество recovery секторов
  -- в каждом "отделении" recovery информации
  (arcsize::Integer, offset::Integer) <- ByteStream.read crc_stream
  let init_pos = blPos crcs_block - offset
  (sector_size,rec_sectors):_ <- ByteStream.read crc_stream >>== mapFsts fromInteger >>== mapSnds fromInteger
  return$ Right (init_pos, arcsize, sector_size, rec_sectors)


----------------------------------------------------------------------------------------------------
---- Проверка архива при помощи recovery информации ------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Проверка целостности архива, содержащего recovery информацию,
-- и аварийный выход, если архив содержит сбои
pretestArchive command archive footer = do
  when (opt_pretest command>0) $ do
    result <- withPool$ scanArchive command archive footer False
    case result of
      Just (_, sector_size, bad_crcs)  |  bad_sectors <- genericLength bad_crcs, bad_sectors>0
              -> registerError$ BROKEN_ARCHIVE (archiveName archive) ["0352 found %1 errors (%2)", show3 bad_sectors, showMemory (bad_sectors*sector_size)]
      Just _  -> condPrintLineLn "r" "Archive integrity OK"
      _       -> return ()
    -- Полное тестирование архива только при -pt3 или при -pt2 и отсутствии recovery информации в архиве
    when (opt_pretest command==3 || (opt_pretest command==2 && isNothing result)) $ do
      w <- count_warnings $ do
               testArchive command (cmd_arcname command) doNothing3
      -- Продолжать работу только при отсутствии warning'ов
      when (w>0) $ do
        registerError$ BROKEN_ARCHIVE (archiveName archive) ["0353 there were %1 warnings due archive testing", show w]


-- |Просканировать архив и возвратить список сбойных секторов
-- (определяются сравнением CRC каждого сектора с его CRC, сохранённым во втором recovery блоке)
scanArchive command archive footer recovery pool = do
  -- Найдём recovery блоки в архиве. Нынешняя версия может обработать только одну пару recovery блоков
  let recovery_blocks  =  filter ((RECOVERY_BLOCK==).blType) (ftBlocks footer)
  if (length recovery_blocks < 2)  then return Nothing  else do
  let sectors_block:crcs_block:_ = recovery_blocks
  when (length recovery_blocks > 2) $ do
      registerWarning$ GENERAL_ERROR ["0344 only first of %1 recovery records can be processed by this program version. Please use newer versions to process the rest", show (length recovery_blocks `div` 2)]

  -- Прочитаем RECOVERY блоки (сектора+crcs)
  sectors <- if recovery  then archiveBlockReadUnchecked pool sectors_block
                          else return$ error "scanArchive:sectors undefined"
  (crcbuf, crcsize) <- archiveBlockReadAll pool (error "encrypted recovery block") crcs_block
  crc_stream <- ByteStream.openMemory crcbuf crcsize

  -- Прочитаем заголовок crc_stream, содержащий все необходимые данные об этой recovery информации
  info <- readControlInfo crc_stream crcs_block
  case info of
    Left version -> do registerWarning$ GENERAL_ERROR ["0345 you need FreeArc %1 or above to process this recovery info", version]
                       return Nothing
    Right (init_pos, arcsize, sector_size, rec_sectors) -> do
      -- От-xor-рить секторы архива с соответствующими секторами RECOVERY блока.
      -- Занести в bad_crcs список секторов архива, чьи CRC не совпадают с контрольными.
      condPrintLineLn "r"$ show3 rec_sectors++" recovery sectors ("++showMemory (i rec_sectors*i sector_size::Integer)++") present"
      condPrintLineLn "r"$ "Scanning archive for damages..."
      uiStage              "0385 Scanning archive for damages"
      archiveSeek archive init_pos
      buf <- pooledMallocBytes pool sector_size
      -- Размер защищённой части архива в секторах
      let arc_sectors = i$ arcsize `divRoundUp` sector_size
      -- i начинается не с нуля потому что (см. в writeRecoveryBlocks)
      i' <- ref ((-arc_sectors) `mod` rec_sectors);  n' <- ref 0
      bad_crcs <- withList $ \bad_crcs -> do
        -- Цикл по секторам архива с отображением индикатора прогресса
        uiWithProgressIndicator command arcsize $ do
          doChunks arcsize sector_size $ \bytes -> do
            uiUpdateProgressIndicator (i bytes)
            failOnTerminated
            len <- archiveReadBuf archive buf bytes
            -- Xor'им сектора, соответствующие одному recovery сектору, чтобы получить данные для восстановления сбойного сектора
            when (recovery && rec_sectors>0) $ do
              i <- val i';  i' =: (i+1) `mod` rec_sectors
              memxor (sectors +: i*sector_size) buf bytes
            -- Сохраняем номера сбойных секторов (чьи CRC не совпадают с контрольным)
            n <- val n';  n `seq` (n' =: n+1)
            crc          <- calcCRC buf bytes
            original_crc <- ByteStream.read crc_stream
            when (crc/=original_crc) $ do
              bad_crcs <<= n
      return$ Just ((crcs_block,crc_stream,sectors,buf), sector_size, bad_crcs)


----------------------------------------------------------------------------------------------------
---- Восстановление архива с помощью recovery информации -------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Команда восстановления запорченного архива
runArchiveRecovery command@Command{ cmd_filespecs       = filespecs
                                  , cmd_arcname         = arcname
                                  , opt_original        = opt_original
                                  , opt_save_bad_ranges = opt_save_bad_ranges
                                  } = do
  doFinally uiDoneArchive2 $ do
  uiStartArchive command []
  let arcname_fixed = arcname `replaceBaseName` ("fixed."++takeBaseName arcname)
  whenM (fileExist arcname_fixed) $ do
    registerError$ GENERAL_ERROR ["0346 file %1 already exists", arcname_fixed]
  command <- (command.$ opt_cook_passwords) command ask_passwords  -- подготовить пароли в команде к использованию
  withPool $ \pool -> do   -- используем пул памяти, чтобы автоматически освободить выделенные буферы при выходе
  bracketCtrlBreak "archiveClose1:ArcRecover" (archiveReadFooter command arcname) (archiveClose.fst) $ \(archive,footer) -> do
    -- Первый этап - сканирование архива и составление списка сбойных секторов
    result <- scanArchive command archive footer True pool
    if isNothing result
        then registerError$ GENERAL_ERROR ["0347 archive can't be recovered - recovery data absent or corrupt"]
        else do
    -- Переходим к восстановлению данных
    let Just ((crcs_block,crc_stream,sectors,buf),_,bad_crcs) = result
    if null bad_crcs  then condPrintLine "n"$ "Archive ok, no need to restore it!"  else do
    -- Прочитаем заголовок crc_stream, содержащий все необходимые данные об этой recovery информации
    Right (init_pos, arcsize, sector_size, rec_sectors) <- readControlInfo crc_stream crcs_block

    -- Составить список секторов, которые мы сможем восстановить, и тех, которые претендуют
    -- на один и тот же recovery сектор и потому не могут быть восстановлены
    let (recoverable,bad)  =  case rec_sectors of
           0 -> ([], bad_crcs)      -- если RR не содержит recovery sectors, то ни один сектор архива не может быть восстановлен с их помощью :D
           _ -> bad_crcs .$ sort_and_groupOn (`mod` rec_sectors)   -- сгруппировать вместе те сбойные сектора, которые приходятся на один сектор RECOVERY
                         .$ partition (null.tail)                  -- отделить группы, где только один элемент (сектор, который удастся однозначно восстановить), от других
                         .$ mapFst concat .$ mapSnd concat
        bad_sectors = genericLength bad
        recoverable_sectors = genericLength recoverable

    -- Эта процедура записывает в файл список запорченных диапазонов байт в архиве
    let arcPos sector = sector*sector_size+init_pos
    let save_bad_ranges bad_sectors = do
          when (opt_save_bad_ranges>"") $ do
            let byte_range sector = show start++"-"++show end
                                      where start = arcPos sector
                                            end   = start+sector_size-1
            filePutBinary opt_save_bad_ranges (joinWith "," $ map byte_range bad_sectors)

    -- Если мы ничего не можем восстановить - нам остаётся только застрелиться :)
    originalName <- originalURL opt_original arcname
    when (recoverable==[] && originalName=="") $ do
      save_bad_ranges bad
      registerError$ GENERAL_ERROR ["0348 %1 unrecoverable errors (%2) found, can't restore anything!",
                                    show3 bad_sectors, showMemory (bad_sectors*sector_size)]

    -- Скопируем файл, подставляя правильное содержимое вместо сбойных секторов из списка recoverable (bad сектора восстановить невозможно из-за отсутствия однозначности)
    condPrintLineLn "n"$ show3 recoverable_sectors++" recoverable errors ("++showMemory (recoverable_sectors*sector_size)++") "
                         ++(bad &&& "and "++show3 bad_sectors++" unrecoverable errors ("++showMemory (bad_sectors*sector_size)++") ")
                         ++"found"
    archiveFullSize <- archiveGetSize archive
    condPrintLineLn "n"$ "Recovering "++showMem archiveFullSize++" archive..."
    uiStage              "0387 Recovering archive"
    errors' <- ref bad
    -- Переходим к созданию архива с восстановленными данными
    handleCtrlBreak  "fileRemove arcname_fixed" (ignoreErrors$ fileRemove arcname_fixed) $ do
    bracketCtrlBreak "archiveClose2:ArcRecover" (archiveCreateRW arcname_fixed) (archiveClose) $ \new_archive -> do
    withJIT (fileOpen =<< originalURL originalName arcname) fileClose $ \original' -> do   -- Лениво откроем файл, откуда можно загрузить корректные данные
    writeSFX (opt_sfx command) new_archive (dirlessArchive archive footer)   -- Начнём создание архива с записи SFX-модуля
    archiveSeek archive init_pos
    -- Размер защищённой части архива в секторах
    let arc_sectors = i$ arcsize `divRoundUp` sector_size
    -- i начинается не с нуля потому что (см. в writeRecoveryBlocks)
    i' <- ref ((-arc_sectors) `mod` rec_sectors);  n' <- ref 0
    originalErr <- init_once

    -- Цикл по секторам восстанавливаемого архива с отображением индикатора прогресса
    uiWithProgressIndicator command arcsize $ do
      doChunks arcsize sector_size $ \bytes -> do
        uiUpdateProgressIndicator (i bytes)
        failOnTerminated
        i <- val i';  when (rec_sectors>0) $  do i `seq` (i' =: (i+1) `mod` rec_sectors)
        n <- val n';  n' =: n+1
        len <- archiveReadBuf archive buf bytes
        original_crc <- ByteStream.read crc_stream

        -- Если это один из восстанавливаемых секторов, то восстановим его содержимое,
        -- отксорив его с контрольным сектором, который сейчас содержит как раз
        -- необходимые для восстановления данные
        when (n `elem` recoverable) $ do
          let do_xor = memxor buf (sectors +: i*sector_size) bytes
          do_xor
          -- Если CRC и после этого не совпало (это возможно при ошибке в самом контрольном секторе),
          -- то восстановим исходное содержимое сектора и запомним,
          -- что в архиве остались невосстановленные сектора
          crc <- calcCRC buf bytes
          when (crc/=original_crc) $ do
            do_xor;  errors' .= (n:)

        -- Если это сбойный сектор, невосстановимый с помощью имеющейся информации,
        -- то просто выкачаем его заново (если указано --original)
        errors <- val errors'
        when (originalName>"" && n `elem` errors) $ do
          -- Прежде всего проверим, что original-файл удалось открыть
          eitherM_ (try$ valJIT original')
            ( \exception -> once originalErr$ registerWarning$ GENERAL_ERROR ["0349 can't open original at %1", originalName])
            $ \original  -> do
          -- Теперь проверим, что его размер совпадает с восстанавливаемым архивом
          dwnl_size <- fileGetSize original
          if dwnl_size /= archiveFullSize
            then once originalErr$ registerWarning$ GENERAL_ERROR
                      ["0350 %1 has size %2 so it can't be used to recover %3 having size %4",
                       originalName, show3 dwnl_size, arcname, show3 archiveFullSize]
            else do
          -- Читаем из original сбойный сектор
          allocaBytes bytes $ \temp -> do
          fileSeek    original (arcPos n)
          fileReadBuf original temp bytes
          -- Если прочитанный сектор имеет верную CRC - заменим им сектор, прочитанный из исходного архива
          crc <- calcCRC temp bytes
          when (crc==original_crc) $ do
            copyBytes buf temp bytes
            errors' .= delete n

        -- Записать [восстановленный] сектор в новый архив
        archiveWriteBuf new_archive buf bytes

    -- Скопировать блоки recovery (а точнее, вообще весь остаток старого архивного файла после защищённых данных)
    pos <- archiveGetPos archive
    archiveCopyData archive pos (archiveFullSize-pos) new_archive

    condPrintLineLn "n"$ "Recovered archive saved to "++arcname_fixed
    errors <- val errors'
    save_bad_ranges errors
    when (errors>[]) $ do
      let errnum = genericLength errors
      registerWarning$ GENERAL_ERROR ["0351 %1 errors (%2) remain unrecovered", show3 errnum, showMemory (errnum*sector_size)]
  return (1,0,0,0)



-- |Вычислить URL оригинала, исходя из содержимого опции --original и имени архива
originalURL opt_original arcname =
  case opt_original of
    "--"         -> return ""              -- отключено
    '?':command  -> run_command command    -- URL возвращается выполнением команды `command arcname`
    ""           -> auto_url               -- URL определяется автоматически из files.bbs/descript.ion
    url          -> return url             -- URL указан явно
 where

  -- Выполнить команду и вернуть её вывод в качестве URL
  run_command command  =  runProgram (command++" "++arcname)
                          >>== head.linesCRLF

  -- Автоматически подбираемый URL по описанию архива в files.bbs/descript.ion
  auto_url = mapMaybeM try_descr (words "files.bbs descript.ion") >>== catMaybes >>== listToMaybe >>== fromMaybe ""

  -- Поискать URL архива в файле описаний descr
  try_descr descr = do
    let descrname = takeDirectory arcname </> descr
        basename  = takeFileName  arcname
    fileExist descrname >>= bool (return Nothing) (do
    fileGetBinary descrname >>== linesCRLF
      -- Строки, начинающиеся с пробелов, надо прибавлять к предыдущим (это строки продолжения описаний)
      >>== joinContLines ""
      -- Искомая строка в files.bbs может начинаться с name.arc или с "The Name.arc", за которым идёт пробел
      >>== listToMaybe . filter (isSpace.head)
           . catMaybes . concatMap (\x -> [x.$startFrom basename
                                          ,x.$startFrom ("\""++basename++"\"")])
      -- Выделим URL из строки с описанием
      >>== fmap findURL
    )

  findURL s = firstJust$ map getURL$ strPositions s "://"
    where
      -- Выделить из строки s URL, чей "://" находится по смещению n
      getURL n = let (pre,post) = splitAt n s
                     prefix  = reverse$ takeWhile isURLPrefix$ reverse pre
                     postfix = takeWhile isURLChar$ drop 3 post
                 in
                     prefix &&& postfix &&& Just (prefix++"://"++postfix)

  -- Символы, которые могут встречаться в префиксе или теле URL
  isURLPrefix = anyf [isAsciiLower, isAsciiUpper]
  isURLChar   = anyf [flip elem "+-=._/*(),@'$:;&!?%", isDigit, isAsciiLower, isAsciiUpper]

  -- Слить строки продолжения (начинающиеся с пробелов) с предыдущими строками
  joinContLines prev (x@(c:_):xs) | isSpace c   =   joinContLines (prev++x) xs
  joinContLines prev (x:xs)                     =   prev : joinContLines x xs
  joinContLines prev []                         =   [prev]

