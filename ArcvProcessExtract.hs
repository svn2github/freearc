----------------------------------------------------------------------------------------------------
---- Процесс распаковки входных архивов.                                                        ----
---- Вызывается из ArcExtract.hs и ArcCreate.hs (при обновлении и слиянии архивов).             ----
----------------------------------------------------------------------------------------------------
module ArcvProcessExtract where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import Data.Int
import Data.IORef
import Data.Maybe
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Utils
import Foreign.Storable

import Utils
import Errors
import Process
import FileInfo
import CompressionLib
import Compression
import Encryption
import Options
import UI
import ArhiveStructure
import ArhiveDirectory

{-# NOINLINE decompress_file #-}
-- |Распаковка файла из архива с использованием переданного процесса декомпрессора
-- и записью распакованных данных с помощью функции `writer`
decompress_file decompress_pipe compressed_file writer = do
  -- Не пытаться распаковать каталоги/пустые файлы и файлы без данных, поскольку ожидать получение 0 байтов - воистину дзенское занятие ;)
  when (fiSize(cfFileInfo compressed_file) > 0  &&  not (isCompressedFake compressed_file)) $ do
    sendP decompress_pipe (Just compressed_file)
    repeat_while (receiveP decompress_pipe) ((>=0).snd) (uncurry writer)
    failOnTerminated

{-# NOINLINE decompress_PROCESS #-}
-- |Процесс, распаковывающий файлы из архивов
decompress_PROCESS command count_cbytes pipe = do
  cmd <- receiveP pipe
  case cmd of
    Nothing     -> return ()
    Just cfile' -> do
      cfile <- ref cfile'
      state <- ref (error "Decompression state is not initialized!")
      repeat_until $ do
        decompress_block command cfile state count_cbytes pipe
        operationTerminated' <- val operationTerminated
        when operationTerminated' $ do
          sendP pipe (error "Decompression terminated", aFREEARC_ERRCODE_OPERATION_TERMINATED)
        (x,_,_) <- val state
        return (x == aSTOP_DECOMPRESS_THREAD || operationTerminated')


{-# NOINLINE decompress_block #-}
-- |Распаковать один солид-блок
decompress_block command cfile state count_cbytes pipe = mdo
  cfile' <- val cfile
  let size        =  fiSize      (cfFileInfo cfile')
      pos         =  cfPos        cfile'
      block       =  cfArcBlock   cfile'
      compressor  =  blCompressor block .$ limitDecompressionMemoryUsage (opt_limit_decompression_memory command)
      startPos  | compressor==aNO_COMPRESSION  =  pos  -- для -m0 начинаем чтение напрямую с нужной позиции в блоке
                | otherwise                    =  0
  state =: (startPos, pos, size)
  archiveBlockSeek block startPos
  bytesLeft <- ref (blCompSize block - startPos)

  let reader buf size  =  do aBytesLeft <- val bytesLeft
                             let bytes   = minI size aBytesLeft
                             len        <- archiveBlockReadBuf block buf bytes
                             bytesLeft  -= i len
                             count_cbytes  len
                             return len

  let writer (DataBuf buf len)  =  decompress_step cfile state pipe buf len
      writer  NoMoreData        =  return 0

  -- Добавить ключ в запись алгоритма дешифрования
  keyed_compressor <- generateDecryption compressor (opt_decryption_info command)
  when (any isNothing keyed_compressor) $ do
    registerError$ BAD_PASSWORD (cmd_arcname command) (cfile'.$cfFileInfo.$storedName)

  -- Превратим список методов сжатия/шифрования в конвейер процессов распаковки
  let decompress1 = de_compress_PROCESS1 freearcDecompress reader times  -- первый процесс в конвейере
      decompressN = de_compress_PROCESS  freearcDecompress times         -- последующие процессы в конвейере
      decompressa [p]     = decompress1 p         0
      decompressa [p1,p2] = decompress1 p2        0 |> decompressN p1 0
      decompressa (p1:ps) = decompress1 (last ps) 0 |> foldl1 (|>) (map (\x->decompressN x 0) (reverse$ init ps)) |> decompressN p1 0

  -- И наконец процедура распаковки
  times <- uiStartDeCompression "decompression"  -- создать структуру для учёта времени распаковки
  ; result <- ref 0   -- количество байт, записанных в последнем вызове writer
  ; runFuncP (decompressa (map fromJust keyed_compressor)) (fail "decompress_block::runFuncP") (doNothing) (writer .>>= writeIORef result) (val result)
  uiFinishDeCompression times                    -- учесть в UI чистое время операции


{-# NOINLINE de_compress_PROCESS #-}
-- |Вспомогательный процесс перекладывания данных из буферов входного потока
-- во входные буфера процедуры упаковки/распаковки
--   comprMethod - строка метода сжатия с параметрами, типа "ppmd:o10:m48m"
--   num - номер процесса в цепочке процессов упаковки
de_compress_PROCESS de_compress times comprMethod num pipe = do
  -- Информация об остатке данных, полученных из предыдущего процесса, но ещё не отправленных на упаковку/распаковку
  remains <- ref$ Just (error "undefined remains:buf0", error "undefined remains:srcbuf", 0)
  let
    -- Процедура "чтения" входных данных. Важно, чтобы первый вызов с dstlen=0 не возвращал управление пока не поступит хотя бы один байт данных от предыдущего процесса
    read_data prevlen  -- сколько данных уже прочитано
              dstbuf   -- буфер, куда нужно поместить входные данные
              dstlen   -- размер буфера
              = do     -- -> процедура должна возвратить количество прочитанных байт или 0, если данные закончились
      remains' <- val remains
      case remains' of
        Just (buf0, srcbuf, srclen)                   -- Если ещё есть данные, полученные из предыдущего процесса
         | srclen>0  ->  copyData buf0 srcbuf srclen  --  то передать их упаковщику/распаковщику
         | otherwise ->  processNextInstruction       --  иначе получить новые
        Nothing      ->  return prevlen               -- Этот solid-блок закончился, данных больше нет
      where
        -- Скопировать данные из srcbuf в dstbuf и возвратить размер скопированных данных
        copyData buf0 srcbuf srclen = do
          let len = srclen `min` dstlen    -- определить - сколько данных мы можем прочитать
          copyBytes dstbuf srcbuf len
          uiReadData num (i len)           -- обновить индикатор прогресса
          remains =: Just (buf0, srcbuf+:len, srclen-len)
          case () of
           _ | len==srclen -> do send_backP pipe (srcbuf-:buf0+srclen)               -- возвратить размер буфера, поскольку все данные из него уже переданы упаковщику/распаковщику
                                 read_data (prevlen+len) (dstbuf+:len) (dstlen-len)  -- прочитать следующую инструкцию
             | len==dstlen -> return (prevlen+len)                                 -- буфер достаточно заполнен
             | otherwise   -> read_data (prevlen+len) (dstbuf+:len) (dstlen-len)   -- заполним остаток буфера содержимым следующих файлов

        -- Получить следующую инструкцию из потока входных данных и отработать её
        processNextInstruction = do
          instr <- receiveP pipe
          case instr of
            DataBuf srcbuf srclen  ->  copyData srcbuf srcbuf srclen
            NoMoreData             ->  do remains =: Nothing;  return prevlen

  -- Процедура чтения входных данных процесса упаковки/распаковки (вызывается лишь однажды, в отличие от рекурсивной read_data)
  let reader  =  read_data 0

  de_compress_PROCESS1 de_compress reader times comprMethod num pipe


{-# NOINLINE de_compress_PROCESS1 #-}
-- |de_compress_PROCESS с параметризуемой функцией чтения (может читать данные напрямую
-- из архива для первого процесса в цепочке распаковки)
de_compress_PROCESS1 de_compress reader times comprMethod num pipe = do
  total' <- ref ( 0 :: FileSize)
  time'  <- ref (-1 :: Double)
  let -- Процедура чтения входных данных процесса упаковки/распаковки
      callback "read" buf size = reader buf size
      -- Процедура записи выходных данных
      callback "write" buf size = do total' += i size
                                     uiWriteData num (i size)
                                     resend_data pipe (DataBuf buf size)
      -- "Квазизапись" просто сигнализирует сколько данных будет записано в результате сжатия
      -- уже прочитанных данных. Значение передаётся через int64* ptr
      callback "quasiwrite" ptr size = do bytes <- peek (castPtr ptr::Ptr Int64) >>==i
                                          uiQuasiWriteData num bytes
                                          return aFREEARC_OK
      -- Информация о чистом времени выполнения упаковки/распаковки
      callback "time" ptr 0 = do t <- peek (castPtr ptr::Ptr CDouble) >>==realToFrac
                                 time' =: t
                                 return aFREEARC_OK
      -- Прочие (неподдерживаемые) callbacks
      callback _ _ _ = return aFREEARC_ERRCODE_NOT_IMPLEMENTED

{-
      -- Debugging wrapper
      debug f what buf size = inside (print (comprMethod,what,size))
                                     (print (comprMethod,what,size,"done"))
                                     (f what buf size)
-}
      -- Non-debugging wrapper
      debug f what buf size = f what buf size

  -- СОБСТВЕННО УПАКОВКА ИЛИ РАСПАКОВКА
  result <- de_compress num comprMethod (debug callback)
  debug callback "finished" nullPtr result
  -- Статистика
  total <- val total'
  time  <- val time'
  uiDeCompressionTime times (comprMethod,time,total)
  -- Выйдем с сообщением, если произошла ошибка
  unlessM (val operationTerminated) $ do
    when (result `notElem` [aFREEARC_OK, aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED]) $ do
      registerThreadError$ COMPRESSION_ERROR [compressionErrorMessage result, comprMethod]
      operationTerminated =: True
  -- Сообщим предыдущему процессу, что данные больше не нужны, а следующему - что данных больше нет
  send_backP  pipe aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED
  resend_data pipe NoMoreData
  return ()


-- |Обработка очередной порции распакованных данных (writer для распаковщика).
-- Состояние (хранимое по ссылке state) содержит:
--   1) block_pos - текущую позицию в блоке данных
--   2) pos       - позицию, с которой начинается файл (или его оставшаяся часть)
--   3) size      - размер файла (или его оставшейся части)
-- Соответственно, получив от распаковщика данные по адресу buf длиной len, мы должны:
--   1) пропустить в начале буфера данные, предшествующие распаковываемому файлу (если есть)
--   2) передать на выход данные, относящиеся к этому файлу (если есть)
--   3) обновить состояние - позиция в блоке изменилась на размер полученного буфера,
--        а позиция и размер оставшихся данных файла - на размер переданных на выход данных
--   4) если файл распакован полностью - надо известить об этом принимающую сторону
--        и получить следующую команду на распаковку
--   5) если следующий распаковываемый файл оказался в другом блоке или в уже прошедшей части
--        текущего блока - надо прервать распаковку этого блока с тем, чтобы decompress_block
--        перешёл к распаковке того, что нужно (он читает эти данные из cfile)
--
decompress_step cfile state pipe buf len = do
  (block_pos, pos, size) <- val state
  if block_pos<0   -- похоже, что распаковщик не обратил внимание, что мы хотим перейти к другому блоку данных
    then return aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED   -- ничего, потерпим, пока он образумится. альтернатива: fail$ "Block isn't changed!!!"
    else do
  let skip_bytes = min (pos-block_pos) (i len)   -- пропустить данные предыдущих файлов в начале буфера
      data_start = buf +: skip_bytes             -- начало данных, принадлежащих распаковываемому файлу
      data_size  = min size (i len-skip_bytes)   -- кол-во байт, принадлежащих распаковываемому файлу
      block_end  = block_pos+i len               -- позиция в солид-блоке, соответствующая концу полученного буфера
  when (data_size>0) $ do    -- если в буфере нашлись данные, принадлежащие распаковываемому файлу
    sendP pipe (data_start, i data_size)  -- то выслать эти данные по каналу связи потребителю
    receive_backP pipe                    -- получить подтверждение того, что данные были использованы
  state =: (block_end, pos+data_size, size-data_size)
  if data_size<size     -- если файл ещё не распакован полностью
    then return len     -- то продолжаем распаковку блока
    else do             -- иначе переходим к следующему заданию на распаковку
  sendP pipe (error "End of decompressed data", aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED)
  old_block  <-  cfArcBlock ==<< val cfile
  cmd <- receiveP pipe
  case cmd of
    Nothing -> do  -- Это сообщение означает, что больше никаких файлов от треда распаковки не требуется и он должен быть завершён
      state =: (aSTOP_DECOMPRESS_THREAD, error "undefined state.pos", error "undefined state.size")
      cfile =: error "undefined cfile"
      return aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED

    Just cfile' -> do
      cfile =: cfile'
      let size   =  fiSize (cfFileInfo cfile')
          pos    =  cfPos      cfile'
          block  =  cfArcBlock cfile'
      if block/=old_block || pos<block_pos  -- если новый файл находится в другом блоке или в этом, но раньше
           || (pos>block_end && blCompressor block==aNO_COMPRESSION)   -- или мы распаковываем блок, сжатый с -m0, и у нас есть возможность пропустить часть файлов
        then do state =: (-1, error "undefined state.pos", error "undefined state.size")
                return aFREEARC_ERRCODE_NO_MORE_DATA_REQUIRED   -- признак того, что нужно завершить распаковку этого блока
        else do state =: (block_pos, pos, size)            -- снова рассмотрим переданный буфер,
                decompress_step cfile state pipe buf len   -- уже в контексте распаковки нового файла

-- |Сигнал, требующий завершения работы треда распаковки
aSTOP_DECOMPRESS_THREAD = -99


-- |Структура, используемая для передачи данных следующему процессу упаковки/распаковки
data CompressionData = DataBuf (Ptr CChar) Int
                     | NoMoreData

{-# NOINLINE resend_data #-}
-- |Процедура передачи выходных данных упаковщика/распаковщика следующей процедуре в цепочке
resend_data pipe x@DataBuf{}   =  sendP pipe x  >>  receive_backP pipe  -- возвратить количество потреблённых байт, возвращаемое из процесса-потребителя
resend_data pipe x@NoMoreData  =  sendP pipe x  >>  return 0

