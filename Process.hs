---------------------------------------------------------------------------------------------------
---- "Взаимодействующие последовательные процессы", как описано в книге Хоара.                 ----
---------------------------------------------------------------------------------------------------
-- |
-- Module      :  Process
-- Copyright   :  (c) Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
-- License     :  Public domain
--
-- Maintainer  :  Bulat.Ziganshin@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-----------------------------------------------------------------------------

module Process where
{-
Процессы соединяются в цепочку операторами "|>" или "|>>>" и запускаются на выполнение функцией runP:
    runP( read_files |>>> compress |> write_data )
Процессы исполняются параллельно благодаря тому, что для их запуска используется функция forkOS.
Каждый процесс описывается обычной функцией, которая получает дополнительный параметр типа Pipe.
  С этой переменной можно выполнять операцию receiveP для получения данных от предыдущего
  процесса в списке, и операцию sendP для посылки данных следующему процессу в списке:
    compress pipe = foreverM (do data <- receiveP pipe; .....; sendP pipe compressed_data)
Данные от процесса к процессу передаются "слева направо". В зависимости от использованной
  при создании связи между процессами операции - "|>" или "|>>>" - в канал между этими процессами
  можно поместить только одно или неограниченное кол-во значений (реализуется с помощью MVar/Chan,
  соответственно).
Данные также можно посылать в обратную сторону ("справа налево") операциями send_backP и receive_backP.
  Канал обратной связи всегда имеет неограниченную ёмкость. Его можно использовать, например,
  для подтверждения выполнения операций, синхронизации, возвращения использованных ресурсов
  (например, буферов ввода/вывода):
    производитель: sendP pipe (buf,len); receive_backP pipe; теперь буфер свободен
    потребитель:   (buf,len) <- receiveP pipe; hPutBuf file buf len; send_backP pipe ()
Операция runP выполняется синхронно, она завершается по окончании выполнения последнего процесса
  в цепочке (даже если остальные процессы ещё не завершились). Если первый процесс в списке
  запускаемых пытается обмениваться с предыдущим (т.е. выполняет операции receiveP/send_backP) или
  последний процесс пытается обмениваться со следующим - то сигнализируется ошибка.
Операция runAsyncP запускает процесс или цепочку процессов асинхронно и возвращает Pipe для обмена
  с ним(и). В этом случае и первый процесс в цепочке может общаться с "предыдущим", и последний - со
  "следующим", хотя это и не обязательно:
    pipe <- runAsyncP compress; sendP pipe data; compressed_data <- receiveP pipe
    pipe <- runAsyncP( compress |> write_data ); sendP pipe data
    pipe <- runAsyncP( read_files |>>> compress ); compressed_data <- receiveP pipe
    runAsyncP( read_files |>>> compress |> write_data )
  Входная и выходная очереди асинхронно запущенного процесса - (пока) одноэлементные.
-}

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef

-- |Операция соединения двух последовательных процессов:
-- выходной канал первого становится входным каналом второго.
-- "|>" создаёт одноэлементную очередь, а "|>>>" - очередь неограниченной длины
infixl 1  |>, |>>>

p1 |>   p2 = createP p1 p2 newEmptyMVar
p1 |>>> p2 = createP p1 p2 newChan

createP p1 p2 create_inner (Pipe pid finished income income_back outcome outcome_back) = do
  inner       <- create_inner      -- Канал между p1 и p2 (MVar или Chan)
  inner_back  <- newChan           -- Обратный канал между p1 и p2
  p1_finished <- newEmptyMVar      -- Признак завершения выполнения p1

  -- Запустим первый процесс в отдельном треде, а второй исполним напрямую
  p1_id <- forkOS$ (p1 (Pipe pid finished income income_back inner inner_back) >> return ())
                       `finally` (putMVar p1_finished ())
  --
  p2 (Pipe (Just p1_id) (Just p1_finished) inner inner_back outcome outcome_back)
  takeMVar p1_finished
  return ()


-- |Запустить комбинированный процесс, созданный операциями "|>" и "|>>>"
runP p = do
  p (Pipe Nothing
          Nothing
          (error "First process in runP tried to receive")
          (error "First process in runP tried to send_back")
          (error "Last process in runP tried to send")
          (error "Last process in runP tried to receive_back"))

-- |Запустить процесс асинхронно и возвратить канал для обмена с ним
runAsyncP p = do
  income  <- newEmptyMVar
  outcome <- newEmptyMVar
  income_back  <- newChan
  outcome_back <- newChan
  parent_id    <- myThreadId
  p_finished   <- newEmptyMVar
  p_id         <- forkOS (p (Pipe Nothing Nothing income income_back outcome outcome_back)
                            `catch` (\e -> do killThread parent_id; throwIO e)
                            `finally` putMVar p_finished ())
  return (Pipe (Just p_id) (Just p_finished) outcome outcome_back income income_back)


-- |Канал обмена с соседними процессами, который получает в своё распоряжение каждый процесс.
-- Канал имеет 6 элементов - ИД предыдущего (запущенного асинхронно) процесса,
--                           MVar-переменная, сигнализирующая о его завершении,
--                           входные данные, отсылка подтверждений,
--                           выходные данные, получение подтверждений
data Pipe a b c d  =  Pipe (Maybe ThreadId) (Maybe (MVar ())) a b c d
killP    pipe@(Pipe (Just pid) _ _ _ _ _)                                  = killThread pid >> joinP pipe
joinP         (Pipe _ (Just finished) _ _ _ _)                             = takeMVar finished
receiveP      (Pipe pid finished income income_back outcome outcome_back)  = getP income
sendP         (Pipe pid finished income income_back outcome outcome_back)  = putP outcome
receive_backP (Pipe pid finished income income_back outcome outcome_back)  = getP outcome_back
send_backP    (Pipe pid finished income income_back outcome outcome_back)  = putP income_back

-- |Довольно странная операция - "возвращение" сообщений самому себе - так, как если бы это сделал
-- последующий процесс в очереди. Но она нужна для создания начального пула ресурсов, используемых
-- процессом
send_back_itselfP (Pipe pid finished income income_back outcome outcome_back)  =  putP outcome_back


-- |Элемент канала между процессами - может иметь тип как MVar, так и Chan
class PipeElement e where
  getP :: e a -> IO a
  putP :: e a -> a -> IO ()

instance PipeElement MVar where
  getP = takeMVar
  putP = putMVar

instance PipeElement Chan where
  getP = readChan
  putP = writeChan

-- |Псевдо-канал процесса - состоит из двух явно заданных функций для получения и посылки данных
data PairFunc a = PairFunc (IO a) (a -> IO ())

instance PipeElement PairFunc where
  getP (PairFunc get_f put_f) = get_f
  putP (PairFunc get_f put_f) = put_f

-- |Процедура запуска процесса с 4 функциями для эмуляции каналов ввода/вывода
runFuncP p receive_f send_back_f send_f receive_back_f  =
  p (Pipe Nothing
          Nothing
          (PairFunc receive_f      undefined)
          (PairFunc undefined      send_back_f)
          (PairFunc undefined      send_f)
          (PairFunc receive_back_f undefined))

{-# NOINLINE createP #-}
{-# NOINLINE runP #-}
{-# NOINLINE runAsyncP #-}
{-# NOINLINE runFuncP #-}


-- Пример использования:
{-
exampleP = do
  -- Demonstrates using of "runP"
  print "runP: before"
  runP( producer 5 |> transformer (++"*2") |> transformer (++"+1") |> printer "runP" )
  print "runP: after"

  -- Demonstrates using of "runAsyncP" to run computation as parallelly computed function
  pipe <- runAsyncP (transformer (++" modified"))
  sendP pipe "value"
  n <- receiveP pipe
  print n

  -- Demonstrates using of "runAsyncP" with "|>"
  pipe <- runAsyncP( transformer (++"*2") |> transformer (++"+1") )
  sendP pipe "7"
  n <- receiveP pipe
  print n

  -- Demonstrates using of "runAsyncP" to run asynchronous process
  print "runAsyncP: before"
  pipe <- runAsyncP( producer 7 |> printer "runAsyncP" )
  print "runAsyncP: after?"

producer n pipe = do
  mapM_ (sendP pipe.show) [1..n]
  sendP pipe "0"

transformer f pipe = do
  n <- receiveP pipe
  sendP pipe (f n)
  transformer f pipe

printer str pipe = do
  n <- receiveP pipe
  when (head n/='0')$  do print$ str ++ ": " ++ n
                          printer str pipe
-}

{- Design principles:
1. Процессы в runP должны запускаться слева направо. При небольшом объёме
обрабатываемых данных это приведёт к тому, что первый процесс в
транспортёре произведёт все необходимые данные и завершится прежде, чем
второй и последующие процессы вообще будут запущены
2. runP должен запустить все процессы в дополнительных тредах и дожидаться
их завершения. Выход из runP желательно производить только после
завершения работы всех процессов
3. При завершении процесса предыдущий процесс в транспортёре должен получить
исключение для того, чтобы завершиться как можно быстрее (после чего
предшествующий ему процесс должен получить исключение в свою очередь).
Следующий же процесс должен получить только информацию о завершении
входных данных при попытке их прочитать (tryReceiveP, eofP)
4. При возникновении необработанного исключения в одном из процессов все
остальные процессы в транспортёре должны быть прекращены (посылкой сигнала
KillThread) и это исключение перевозбуждено в основном процессе
5. runP (p1 |> p2 |> protectP p) защищает процесс `p` от возбуждения в нём исключений,
вместо этого возникающие ситуации только сигнализируются в состоянии канала
6. Для ожидания завершения процесса, запущенного по runAsyncP или
предыдущего процесса в транспортёре ввести операцию joinP pipe
7. "p |> yP p1 p2" посылает вывод одного процесса двум другим
8. killP pipe убивает все процессы из запущенного асинхронно транспортёра
9. Нужны удобные и эффективные средства для создания процессов, имеющих
несколько входных и/или выходных каналов (использовать getP/putP?)
10. new_pipe <- insertOnInputP old_pipe process - вставить новый процесс перед своим входом
    new_pipe <- insertOnOutputP old_pipe process - вставить новый процесс после своего выхода

p1 |> p2   -->  PChain p1 p2 ?

(p1 |> p2) pipe{ MainThreadId, ref_threads... }
  p2_threadId <- forkIO $ (p2 pipe2  >> writeIORef pipe.isEof True - по окончании второго процесса)
                          `catch` (throwTo MainThreadId)
  addToMVar ref_threads p2_threadId
  p1 pipe1

p1 |> (p2 |> p3)
  forkIO (forkIO p3; p2)
  p1

runP p =
  p_threadId <- forkIO$ p pipe{ MainThreadId = MyThreadId, ref_threads = newIORef [], ...}
  addToMVar ref_threads p_threadId
  wait them all `catch` (\e -> mapM killThread ref_threads; throw e)

11. Вокруг запущенного треда - катч, который убивает сына, дожидается его завершения и посылает
    полученное исключение отцу
-}


{-New design guidelines:
1. a|>b запускается как "fork a; b"
2. При завершении b дождаться завершения a
3. При возникновении в любом из процессов необработанного сигнала
   надо убивать все порцессы в транспортёре и перевозбуждать этот сигнал в основной порграмме
-}
