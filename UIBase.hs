{-# OPTIONS_GHC -cpp #-}
----------------------------------------------------------------------------------------------------
---- Информирование пользователя о ходе выполнения программы (CUI - Console User Interface).  ------
----------------------------------------------------------------------------------------------------
module UIBase where

import Prelude hiding (catch)
import Control.Monad
import Control.Concurrent
import Control.Exception
import Data.Char
import Data.IORef
import Foreign
import Foreign.C
import Numeric           (showFFloat)
import System.CPUTime    (getCPUTime)
import System.IO
import System.Time
#ifdef FREEARC_UNIX
import System.Posix.IO
import System.Posix.Terminal
#endif

import Utils
import Errors
import Files
import FileInfo
import Options


-- |Здесь хранится вся информация о команде и процессе её выполнения, требуемая для отображения
-- индикатора прогресса и вывода финальной статистики
data UI_State = UI_State {
    total_files     :: !FileCount   -- Кол-во файлов, которые она должна обработать
  , total_bytes     :: !FileSize    -- Общий объём этих файлов (в распакованном виде)
  , archive_total_bytes      :: !FileSize    -- Общий объём файлов в архиве - устанавливается только для команд распаковки
  , archive_total_compressed :: !FileSize    -- Общий объём файлов в архиве (в сжатом виде)
  , datatype        ::  DataType    -- Обрабатываемая в данный момент часть архива: файл/каталог/служебные данные
  , uiFileinfo      :: !(Maybe FileInfo)  -- Текущий обрабатываемый файл (если есть)
  -- В зависимости от того, какая часть архива сейчас обрабатывается, статистика заносится
  -- либо на счёт файлов:
  ,    files        :: !FileCount   -- Кол-во уже обработанных файлов
  ,    bytes        :: !FileSize    -- Объём уже обработанных данных в распакованном виде
  ,    cbytes       :: !FileSize    -- Объём уже обработанных данных в упакованном виде
  -- либо на счёт каталогов (служебная информация не подсчитывается):
  ,    dirs         :: !FileCount   -- Кол-во созданных каталогов и других служебных блоков
  ,    dir_bytes    :: !FileSize    -- Объём уже обработанных данных в распакованном виде
  ,    dir_cbytes   :: !FileSize    -- Объём уже обработанных данных в упакованном виде
  -- Кроме того, мы запоминаем, какая часть из этих данных - на самом деле не упаковывалась (это полезно для определения реальной скорости упаковки):
  ,    fake_files   :: !FileCount   -- Кол-во уже обработанных файлов
  ,    fake_bytes   :: !FileSize    -- Объём уже обработанных данных в распакованном виде
  ,    fake_cbytes  :: !FileSize    -- Объём уже обработанных данных в упакованном виде
  -- Информация о текущем солид-блоке
  ,    algorithmsCount :: Int       -- Кол-во алгоритмов в цепочке
  ,    rw_ops       :: [[UI_RW FileSize]] -- Последовательность операций чтения/записи с разбивкой по отдельным алгоритмам
  ,    r_bytes      :: FileSize     -- Объём уже обработанных данных на входе первого алгоритма сжатия
  ,    rnum_bytes   :: FileSize     -- Объём уже обработанных данных на входе последнего алгоритма сжатия
  }

-- |Обрабатываемая в данный момент часть архива: файл/каталог/служебные данные
data DataType = File | Dir | CData   deriving Eq

-- |Операции чтения и записи в списке операций
data UI_RW a = UI_Read a | UI_Write a

-- |Тип индикатора - только прценты или + файлы/...
data IndicatorType = INDICATOR_PERCENTS | INDICATOR_FULL   deriving Eq


-- Выполняемая сейчас команда
ref_command               =  unsafePerformIO$ newIORef$ error "undefined UI::ref_command"
-- Обрабатываемый архив (не совпадает с command.$cmd_arcname при тестировании временного архива после упаковки)
uiArcname                 =  unsafePerformIO$ newIORef$ error "undefined UI::uiArcname"
refStartArchiveTime       =  unsafePerformIO$ newIORef$ error "undefined UI::refStartArchiveTime"
refStartPauseTime         =  unsafePerformIO$ newIORef$ error "undefined UI::refStartPauseTime"
refArchiveProcessingTime  =  unsafePerformIO$ newIORef$ error "undefined UI::refArchiveProcessingTime"  :: IORef Double
ref_ui_state              =  unsafePerformIO$ newIORef$ error "undefined UI::ref_ui_state"
putHeader                 =  unsafePerformIO$ init_once
-- Текущая стадия выполнения команды или имя файла из uiFileinfo
uiMessage                 =  unsafePerformIO$ newIORef$ ""
-- |Счётчик просканированных файлов
files_scanned             =  unsafePerformIO$ newIORef$ (0::Integer)

-- |Глобальная переменная, хранящая состояние индикатора прогресса
aProgressIndicatorState    =  unsafePerformIO$ newIORef$ error "undefined UI::aProgressIndicatorState"
aProgressIndicatorEnabled  =  unsafePerformIO$ newIORef$ False
-- |Время начала отсчёта текущего индиатора
indicator_start_real_secs  =  unsafePerformIO$ newIORef$ (0::Double)

-- |Синхронизация доступа к UI
syncUI = withMVar mvarSyncUI . const;  mvarSyncUI = unsafePerformIO$ newMVar "mvarSyncUI"


-- |Тред, следящий за indicator, и выводящий время от времени его обновлённые значения
indicatorThread secs output =
  backgroundThread secs $ do
    whenM (val aProgressIndicatorEnabled) $ do
      operationTerminated' <- val operationTerminated
      (indicator, indType, arcname, direction, b, bytes', total') <- val aProgressIndicatorState
      when (indicator /= NoIndicator  &&  not operationTerminated') $ do
        bytes <- bytes' b;  total <- total'
        -- Отношение объёма обработанных данных к общему объёму
        let processed = total>0 &&& (fromIntegral bytes / fromIntegral total :: Double)
        secs <- return_real_secs
        sec0 <- val indicator_start_real_secs
        let remains  = if processed>0.001  then " "++showHMS(sec0+(secs-sec0)/processed-secs)  else ""
            winTitle = "{"++trimLeft p++remains++"}" ++ direction ++ takeFileName arcname
            p        = percents indicator bytes total
        output indicator indType winTitle b bytes total processed p

-- |Выполнять в бэкграунде action каждые secs секунд
backgroundThread secs action =
  forkIO $ do
    foreverM $ do
      threadDelay (round$ secs*1000000)
      syncUI $ do
        action

{-# NOINLINE indicatorThread #-}
{-# NOINLINE backgroundThread #-}

----------------------------------------------------------------------------------------------------
---- Индикатор прогресса ---------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Типы индикатора прогресса (молчаливый, проценты, десятые процента)
data Indicator = NoIndicator | ShortIndicator | LongIndicator   deriving (Eq)

bytes_per_sec = 1*mb  -- Typical (de)compression speed

-- |Выбрать индикатор прогресса, основываясь на показаниях свидетелей :)
select_indicator command total_bytes  =  case (opt_indicator command)
  of "0"                                    ->  NoIndicator      -- опция "-i" - отключить индикатор!
     _ | i total_bytes < bytes_per_sec*100  ->  ShortIndicator   -- индикатор в процентах, если общий объём данных меньше 100 мб (при этом в секунду обрабатывается больше одного процента данных)
       | otherwise                          ->  LongIndicator    -- индикатор в десятых долях процента, если данных больше 100 мб

-- |Вывести индикатор прогресса в соответствии с выбранной точностью
percents NoIndicator    current total  =  ""
percents ShortIndicator current total  =  right_justify 3 (ratio2 current total) ++ "%"
percents LongIndicator  current total  =  right_justify 5 (ratio3 current total) ++ "%"

-- |Создать место для индикатора прогресса
open_percents     =  flip replicate ' '  . indicator_len
-- |Вернуться назад на столько символов, сколько занимает индикатор прогресса
back_percents     =  flip replicate '\b' . indicator_len
-- |Напечатать пробелы поверх использовавшегося индикатора прогресса
clear_percents i  =  back_percents i ++ open_percents i

-- |Размер индикатора прогресса в символах
indicator_len NoIndicator    = 0
indicator_len ShortIndicator = 4
indicator_len LongIndicator  = 6

-- |Format percent ratio with 2 digits
ratio2 count 0     =  "0"
ratio2 count total =  show$ count*100 `div` total

-- |Format percent ratio with 2+1 digits
ratio3 count 0     =  "0.0"
ratio3 count total =  case (show$ count*1000 `div` total) of
                        [digit]  -> "0." ++ [digit]
                        digits   -> init digits ++ ['.', last digits]

-- |Вывести число, отделяя тысячи, миллионы и т.д.: "1.234.567"
show3 :: (Show a) => a -> [Char]
show3 = reverse.xxx.reverse.show
          where xxx (a:b:c:d:e) = a:b:c:'.': xxx (d:e)
                xxx a = a

{-# NOINLINE ratio2 #-}
{-# NOINLINE ratio3 #-}
{-# NOINLINE show3 #-}


----------------------------------------------------------------------------------------------------
---- Вспомогательные функции для форматирования чисел/строк и работы с временем --------------------
----------------------------------------------------------------------------------------------------

-- |Разница между двумя временами в секундах - использует особенности внутреннего представления!!!
diffTimes (TOD sa pa) (TOD sb pb)  =  i(sa - sb) + (i(pa-pb) / 1e12)

-- |Добавить секунды к времени
addTime (TOD sa pa) secs  = TOD (sa+sb+sc) pc
  where
    sb = i$ floor secs
    pb = round$ (secs-sb)*1e12
    (sc,pc) = (pa+pb) `divMod` (10^12)

-- |Возвратить время в юниксовом формае (секунд бог знает с какого времени)
getUnixTime = do
  (TOD seconds picoseconds) <- getClockTime
  return seconds

-- |Напечатать объём исходных и упакованных данных, и степень сжатия
show_ratio cmd bytes cbytes =
  ""        ++ show3       (if (cmdType cmd == ADD_CMD) then bytes else cbytes) ++
   " => "   ++ show_bytes3 (if (cmdType cmd == ADD_CMD) then cbytes else bytes) ++ ". " ++
   "Ratio " ++ ratio3 cbytes bytes ++ "%"

-- |Возвратить строку, описывающую заданное время
showTime secs  =  showFFloat (Just 2) secs " secs"

-- |Возвратить строку, описывающую заданную скорость
showSpeed bytes secs  =  show3(round$ i bytes/1000/secs) ++ " kB/s"

-- |Отформатировать время как H:MM:SS
showHMS secs  =  show hour++":"++left_fill '0' 2 (show min)++":"++left_fill '0' 2 (show sec)
  where
    s = round secs
    sec = (s `mod` 60)
    min = (s `div` 60) `mod` 60
    hour= (s `div` 3600)



-- |Отметить время, когда была достигнута определённая точка программы (чисто для внутренних бенчмарков)
debugLog label = do
  condPrintLine   "$" $  label   -- вычислим label и напечатаем её значение
  real_secs <- return_real_secs
  condPrintLineLn "$" $  ": " ++ showTime real_secs

-- |Вывести информацию о списке, если он содержит как минимум два элемента
debugLogList label list = do
  drop 1 list &&& debugLog (format label (show3$ length list))

-- |Добавить строчку в отладочный вывод программы
debugLog0 = condPrintLineLn "$"

-- |Время, реально прошедшее с начала выполнения команды над текущим архивом
return_real_secs = do
  start_time    <- val refStartArchiveTime
  current_time  <- getClockTime
  return$ diffTimes current_time start_time

-- Вычитаем время, проведённое в паузе, из реального времени выполнения команды
pause_real_secs = do
  refStartPauseTime =:: getClockTime

resume_real_secs = do
  start_time    <- val refStartPauseTime
  current_time  <- getClockTime
  let pause = diffTimes current_time start_time :: Double
  refStartArchiveTime .= (`addTime` pause)

pauseTiming = bracket_ pause_real_secs resume_real_secs

{-# NOINLINE diffTimes #-}
{-# NOINLINE show_ratio #-}
{-# NOINLINE debugLog #-}


----------------------------------------------------------------------------------------------------
---- Выбор сообщений, соответствующих выполняемой команде ------------------------------------------
----------------------------------------------------------------------------------------------------

msgStart cmd arcExist =
                case (cmdType cmd, arcExist) of
                  (ADD_CMD,     False)  ->  "Creating archive: "
                  (ADD_CMD,     True)   ->  "Updating archive: "
                  (LIST_CMD,    _)      ->  "Listing archive: "
                  (TEST_CMD,    _)      ->  "Testing archive: "
                  (EXTRACT_CMD, _)      ->  "Extracting archive: "
                  (RECOVER_CMD, _)      ->  "Recovering archive: "

msgDo cmd    =  case (cmdType cmd) of
                  ADD_CMD     -> "Compressing "
                  TEST_CMD    -> "Testing "
                  EXTRACT_CMD -> "Extracting "

msgFile      =  ("  "++).msgDo

msgDone cmd  =  case (cmdType cmd) of
                  ADD_CMD     -> "Compressed "
                  TEST_CMD    -> "Tested "
                  EXTRACT_CMD -> "Extracted "

msgStat cmd  =  case (cmdType cmd) of
                  ADD_CMD     -> "Compression "
                  TEST_CMD    -> "Testing "
                  EXTRACT_CMD -> "Extraction "

-- |Напечатать "file" или "files", в зависимости от кол-ва
show_files3 1 = "1 file"
show_files3 n = show3 n ++ " files"

-- |Напечатать "archive" или "archives", в зависимости от кол-ва
show_archives3 1 = "1 archive"
show_archives3 n = show3 n ++ " archives"

-- |Напечатать "byte" или "bytes", в зависимости от кол-ва
show_bytes3 1 = "1 byte"
show_bytes3 n = show3 n ++ " bytes"


{-
  Структура UI:
  - один процесс, получающий информацию от упаковки/распаковки и определяющий структуру
      взаимодействия с UI:
        ui_PROCESS pipe = do
          (StartCommand cmd) <- receiveP pipe
            (StartArchive cmd) <- receiveP pipe
              (StartFile fi fi) <- receiveP pipe
                (UnpackedData n) <- receiveP pipe
                (CompressedData n) <- receiveP pipe
            (EndArchive) <- receiveP pipe
          (EndCommand) <- receiveP pipe
         (EndProgram) <- receiveP pipe
      Этот процесс записывает текущее состояние UI в SampleVar
-}
