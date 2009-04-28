{-# OPTIONS_GHC -cpp #-}
----------------------------------------------------------------------------------------------------
---- Кодирование структур данных в виде потока байтов и буферизация его записи/чтения --------------
----------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- |
-- Module      :  ByteStream
-- Copyright   :  (c) Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
-- License     :  Public domain
--
-- Maintainer  :  Bulat.Ziganshin@gmail.com
-- Stability   :  experimental
-- Portability :  GHC/Hugs on x86 processors
--
--  This module is like 'Binary' module from NHC - it supports writing data
--  structures to binary files or memory buffers and reading them back.
--
--  This module features:
--  * Compatibility with last versions of GHC and Hugs
--  * Lightning speed, especially for large strings and lists of Ints/Words
--      (i have seen a 10mb/s speed on my 1.2 ghz machine)
--  * Flexibility of input/output - data may be hold in files, memory buffers,
--      or reading/writing may be performed via callbacks
--
--  This module currently DON'T supports:
--  * Haskell'98 compatibility (because it uses "too complex" class scheme)
--  * Compatibility with MSB (most significant byte first) processors,
--      including Power PC, Motorola and Sparc
--  * Compatibility with processors, which require aligning of Ints on word
--      boundaries
--  * Bit-oriented compression (instead it uses byte-oriented compression,
--      which is faster and simplier)
--  * Writing strings which contains null chars
--  * Tell/Seek-like operations on streams and "freezing" streams
--  * Reading input streams via fixed-size buffer (buffering at this time
--      supported only for output streams, input streams must be placed
--      in one memory buffer containing all the data. MOREOVER, YOU MUST
--      ALLOCATE BUFFER WITH 8 ADDITIONAL BYTES AFTER END OF REAL DATA.
--      It's because Integer demarshalling can pre-read whole 9 bytes
--      even for values which use only 1 byte)
--
--  Example of simple usage you can see in the last section of this file,
--  and examples of defining functions to read/write values of some type -
--  in two preceding sections of file. If you need more explanations -
--  please write me.
--
-----------------------------------------------------------------------------

module ByteStream where

import Prelude hiding (read,readList)
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Data.Bits
import Data.Char
import Data.IORef
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Marshal.Utils
import Foreign.Storable
import System.IO  hiding (openFile)
import GHC.Base (unsafeChr)

import Files
import Utils

aTYPICAL_BUFFER = 64*1024

----------------------------------------------------------------------------------------------------
---- Выходной буфер для быстрой записи структурированных данных ------------------------------------
----------------------------------------------------------------------------------------------------

data OutStream = OutStream
  { ref_buf     :: (IORef (Ptr CChar))    -- буфер в памяти, используемый в настоящий момент
  , ref_size    :: (IORef Int)            -- его размер в байтах
  , ref_pos     :: (IORef Int)            -- текущая позиция записи в буфере
  , functions   :: ( RecvBuf              -- функции, обеспечивающие связь с внешним миром
                   , SendBuf              --   (см. описание create)
                   , Cleanup )
  }

type RecvBuf = IO (Ptr CChar, Int)
type SendBuf = Ptr CChar -> Int -> Int -> IO ()
type Cleanup = IO ()


-- |Записать выводимые данные в файл `filename`, буферизуя их в буфере размером `size` байт
createFile filename size = do
  file <- fileCreate filename
  createBuffered size (fileWriteBuf file) (fileClose file)

-- |Создать выходной поток, выделив для него буфер размером `size` байт.
-- Данные, накапливаемые в буфере, сплавлять наружу функцией `writer`
createBuffered size writer closer = do
  buf <- mallocBytes size
  let sendBuf buf size len  =  writer buf len
  create (return (buf,size)) sendBuf (free buf >> closer)

-- |Создать выходной поток, выделив для него буфер размером `size` байт.
-- Данные, накапливаемые в буфере, сплавлять наружу функцией `writer`
createMemBuf buf size = do
  create (return (buf,size)) (\buf size len -> fail "createMemBuf: Buffer overflow") (return ())

-- |Создать универсальный выходной поток, используя следующие функции:
-- receiveBuf : IO (buf,size)                - получить в своё распоряжение очередной буфер
-- sendBuf    : buf -> size -> len -> IO ()  - отправить буфер на выход с данными длиной `len`
-- cleanup    : IO ()                        - cleanup при завершении работы
create receiveBuf sendBuf cleanup = do
  (buf, size) <- receiveBuf   -- сразу получить первый в своей жизни буфер
  ref_buf  <- ref buf
  ref_size <- ref size
  ref_pos  <- ref 0
  return (OutStream ref_buf ref_size ref_pos (receiveBuf, sendBuf, cleanup))

-- |Получить очередной буфер для записи данных
receiveBuffer (OutStream ref_buf ref_size ref_pos (receiveBuf, _, _)) = do
  (buf, size) <- receiveBuf
  ref_buf  =: buf
  ref_size =: size
  ref_pos  =: 0

-- |Удостовериться, что в буфере ещё есть место для записи `bytes` байт.
-- Если нет - сплавить этот буфер перекупщикам и получить новый, чистенький, где места уж точно должно хватить!
ensureFreeSpaceInOutStream buffer@(OutStream _ ref_size ref_pos _) bytes = do
  size <- val ref_size
  pos  <- val ref_pos
  when (pos+bytes>size-1) $ do
    sendBuffer buffer
    receiveBuffer buffer
    size <- val ref_size
    pos  <- val ref_pos
    when (pos+bytes>size-1) $
      fail$ "OutStream: needs "++show bytes++" bytes, but entire new buffer contains only "++show size++" bytes"

-- |Отослать накопленное содержимое буфера через выходную функцию и прекратить его использование
sendBuffer (OutStream ref_buf ref_size ref_pos (_, sendBuf, _)) = do
  modifyIORefIO ref_buf $ \buf -> do
    size <- val ref_size
    pos  <- val ref_pos
    sendBuf buf size pos
    return (error "OutStream::buf undefined")

-- |Отослать накопленное содержимое буфера и закрыть поток
closeOut buffer@(OutStream _ _ _ (_, _, cleanup)) = do
  sendBuffer buffer
  cleanup

-- |All-in-one операция: создаёт выходной поток, записывает в него значение и закрывает поток.
-- Если вам нужно записать несколько значений - соберите их в tuple
writeAll :: (BufferData a) =>  RecvBuf -> SendBuf -> Cleanup -> a -> IO ()
writeAll receiveBuf sendBuf cleanup x =
  bracket (create receiveBuf sendBuf cleanup) (closeOut)
    (\buf -> write buf x)

-- |All-in-one операция: записывает значение в файл и закрывает его.
-- Если вам нужно записать несколько значений - соберите их в tuple
writeFile filename x =
  bracket (createFile filename aTYPICAL_BUFFER) (closeOut)
    (\buf -> write buf x)

{-# NOINLINE createFile #-}
{-# NOINLINE createBuffered #-}
{-# NOINLINE create #-}
{-# NOINLINE receiveBuffer #-}
{-# NOINLINE ensureFreeSpaceInOutStream #-}
{-# NOINLINE sendBuffer #-}
{-# NOINLINE closeOut #-}
{-# NOINLINE writeAll #-}


----------------------------------------------------------------------------------------------------
---- Входной буфер для быстрого чтения структурированных данных ------------------------------------
----------------------------------------------------------------------------------------------------

data InStream = InStream
  { iref_buf     :: (IORef (Ptr CChar))   -- буфер в памяти, используемый в настоящий момент
  , iref_size    :: (IORef Int)           -- его размер в байтах
  , iref_pos     :: (IORef Int)           -- текущая позиция записи в буфере
  , ifunctions   :: ( RecvBuf             -- функции, обеспечивающие связь с внешним миром
                    , SendBuf             --   (см. описание open)
                    , Cleanup )
  }

-- |to do: Декодировать данные из файла, читая их через буфер размером `size` байт
-- В настоящий момент файл читается в память целиком,
-- что вызвано отсутствием поддержки перехода к следующему буферу
openFile filename _size = do
  file     <- fileOpen filename
  filesize <- fileGetSize file   -- temporary solution
  let size  = 8 + i filesize     -- ditto
  buf      <- mallocBytes size
  let receiveBuf = do len <- fileReadBuf file buf size;  return (buf, len)
      sendBuf buf size len  =  return ()
  open receiveBuf sendBuf (free buf >> fileClose file)

-- |Декодировать данные, содержащиеся в буфере `buf` длиной `size`
openMemory buf size = do
  ref_bytes_read <- ref 0   -- сколько байт в буфере уже обработано
  let   -- receiveBuf возвращает (buf,size) без той части данных, которые уже были обработаны
      receiveBuf = do bytes_read <- val ref_bytes_read
                      return (buf+:bytes_read, size-bytes_read)
        -- sendBuf отмечает, что ещё `len` байтов было обработано
      sendBuf buf size len  =  ref_bytes_read += len
   -- Использовать универсальный `open`; при попытке перейти к следующему буферу просто возвращать
   -- остаток данных в `buf`
  open receiveBuf sendBuf (return ())

-- |to do: Создать универсальный входной поток, используя следующие функции:
-- receiveBuf : IO (buf,size)                - получить буфер `buf` с данными размером `size`
-- sendBuf    : buf -> size -> len -> IO ()  - освободить полученный буфер, из которого прочитано `len` байт
-- cleanup    : IO ()                        - cleanup при завершении работы
open receiveBuf sendBuf cleanup = do
  (buf, size) <- receiveBuf   -- сразу получить первый в своей жизни буфер
  ref_buf  <- ref buf
  ref_size <- ref size
  ref_pos  <- ref 0
  return (InStream ref_buf ref_size ref_pos (receiveBuf, sendBuf, cleanup))

-- |Закрыть входной поток и выполнить процедуру `cleanup`
closeIn (InStream _ _ _ (_, _, cleanup)) = do
  cleanup

-- |Возвращает указатель чтения в начало текущего буфера
rewindMemory buffer@(InStream _ _ pos _) = do
  pos =: 0

-- |Пропускает заданное число байт
skipBytes buffer@(InStream _ _ pos _) bytes = do
  pos += bytes

-- |Проверяет, что мы достигли конца текущего буфера
isEOFMemory buffer@(InStream _ size' pos' _) = do
  size <- val size'
  pos  <- val pos'
  return (pos==size)

-- |All-in-one операция: создаёт входной поток, читает значение и закрывает поток.
-- Если вам нужно прочитать несколько значений - соберите их в tuple
readMemory :: (BufferData a) =>  Ptr CChar -> Int -> IO a
readMemory buf size = do
  bracket (openMemory buf size) (closeIn) (read)

-- |All-in-one операция: открывает файл, читает значение и закрывает файл.
-- Если вам нужно прочитать несколько значений - соберите их в tuple
readFile filename = do
  bracket (openFile filename aTYPICAL_BUFFER) (closeIn) (read)

{-# NOINLINE openFile #-}
{-# NOINLINE openMemory #-}
{-# NOINLINE open #-}
{-# NOINLINE closeIn #-}
{-# NOINLINE readMemory #-}



----------------------------------------------------------------------------------------------------
---- Запись блока памяти ---------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

writeBuf :: OutStream -> Ptr a -> Int -> IO ()
writeBuf buffer@(OutStream ref_buf ref_size ref_pos _) dataptr datasize = do
  when (datasize>0) $ do
  ensureFreeSpaceInOutStream buffer 1
  buf  <- val ref_buf
  size <- val ref_size
  pos  <- val ref_pos
  let len = min datasize (size-pos)
  copyBytes (buf+:pos) dataptr len
  ref_pos =: pos+len
  writeBuf buffer (dataptr+:len) (datasize-len)


----------------------------------------------------------------------------------------------------
---- Классы типов данных, для которых реализовано чтение/запись в буфер ----------------------------
----------------------------------------------------------------------------------------------------

-- |Элементы этого класса могут записываться в выходной буфер и читаться из входного
class BufferData a where
  -- |Записать одно значение в выходной буфер
  write :: OutStream -> a -> IO ()

  -- |Записать в буфер целый список значений - реализация по умолчанию делает это медленно и печально :)
  writeList :: OutStream -> [a] -> IO ()
  writeList buffer list  =  mapM_ (write buffer) list

  -- |Прочитать одно значение из входного буфера
  read :: InStream -> IO a

  -- |Прочитать из входного буфера целый список значений - и тоже реализация по умолчанию слегка задумчива :)
  readList :: InStream -> Int -> IO [a]
  readList buffer length  =  replicateM length (read buffer)

  {-# NOINLINE read #-}
  {-# NOINLINE write #-}
  {-# NOINLINE readList #-}
  {-# NOINLINE writeList #-}


-- Утвердить на должности процедуры класса FastBufferData, выполняющие функции процедур класса BufferData :)
instance (FastBufferData a) => BufferData a where
  write     = writeFast
  writeList = writeListFast
  read      = readFast
  readList  = readListFast


-- |Элементы этого класса могут ОЧЕНЬ БЫСТРО записываться в выходной буфер и читаться из входного
class FastBufferData a where
  -- Для этого они должны предоставить следующие справки:
  --   Максимальное кол-во байт, которое может занимать одно значение (1 для CChar, 4 для Int32 и т.д.)
  maxSizeOf :: a -> Int
  --   Процедура, записывающая в буфер `buf` на позицию `pos` значение `x`, и возвращающая
  --   позицию в буфере после записанных данных (для типов, занимающих фиксированное число байт,
  --   это будет просто "pos+maxSizeOf x")
  writeUnchecked :: Ptr CChar -> a -> Int -> IO Int
  --   Процедура, читающая из буфера `buf` с позиции `pos` значение, возвращающая это значение,
  --   и обновляющая позицию в буфере
  readUnchecked :: Ptr CChar -> Int -> IO (a, Int)

  -- |Записать в буфер одно значение - и побыстрее
  writeFast :: OutStream -> a -> IO ()
  writeFast buffer@(OutStream ref_buf _ ref_pos _) x = do
    ensureFreeSpaceInOutStream buffer (maxSizeOf x)   -- проверить, что в буфере хватит места
    buf <- val ref_buf
    modifyIORefIO ref_pos (writeUnchecked buf x)   -- записать данные в буфер и обновить значение ref_pos

  -- |Быстро-быстро записать в буфер целый список!
  writeListFast :: OutStream -> [a] -> IO ()
  writeListFast buffer@(OutStream ref_buf _ ref_pos _)   list = do
    let aSIZE = 100
    -- Проверить, что в буфере хватит места на `aSIZE` значений данного типа
    ensureFreeSpaceInOutStream buffer (aSIZE * maxSizeOf (head list))
    buf <- val ref_buf
    pos <- val ref_pos

    -- Процедура "go list pos n" записывает без всяких проверок, начиная с позиции `pos`,
    -- данные из списка `list`, но не более `n` значений. Если список оказался
    -- длиннее - снова вызывается процедура `writeListFast`, которая проверит,
    -- что в буфере найдётся место для ещё 100 значений, и продолжит запись списка с того
    -- места, на котором мы остановились
    --
    let --go :: (FastBufferData a) => [a] -> Int -> Int -> IO ()
        go []     pos _  = ref_pos =: pos  -- Мы кончили! Надо только записать новую позицию в буфере!
        go list   pos 0  = do ref_pos =: pos             -- Записываем новую позицию в буфере
                              writeListFast buffer list  -- ... и вызываем функцию рекурсивно для остатка списка
        go (x:xs) pos n  = do new_pos <- writeUnchecked buf x pos    -- записать очередной элемент
                              go xs new_pos (n-1)                    -- ... и перейти к следующему
    go list pos aSIZE  -- записать список в память без проверок, но не более `aSIZE` значений


  -- |Быстрое чтение одного значения из входного буфера
  readFast :: InStream -> IO a
  readFast buffer@(InStream buf _ pos _) = do
    abuf <- val buf
    apos <- val pos
    (x, new_pos) <- readUnchecked abuf apos
    pos =: new_pos
    return x

  -- |Быстрое чтение целого списка из входного буфера
  readListFast :: InStream -> Int -> IO [a]
  readListFast buffer@(InStream buf _ pos _) length = do
    abuf <- val buf
    apos <- val pos
    let --go :: (FastBufferData a) => Int -> Int -> [a] -> IO [a]
        go apos 0 xs = do pos =: apos
                          return (reverse xs)
        go apos n xs = do (x, new_pos) <- readUnchecked abuf apos
                          go new_pos (n-1) (x:xs)
    go apos length []


  {-# NOINLINE readFast #-}
  {-# NOINLINE writeFast #-}
  {-# NOINLINE readListFast #-}
  {-# NOINLINE writeListFast #-}



----------------------------------------------------------------------------------------------------
---- Реализации для простых типов данных -----------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- Любая инстанция класса Storable автоматически становится инстанцией класса FastBufferData:
-- мы знаем, сколько данные такого типа занимают байт, и как записать их в память/прочитать из памяти
instance (Storable a) => FastBufferData a where
  maxSizeOf x = sizeOf x
  writeUnchecked buf x pos = do
    pokeByteOff buf pos x
    return (pos + sizeOf x)
  readUnchecked buf pos = do
    x <- peekByteOff buf pos
    return (x, pos + sizeOf x)


-- Символы записываются в UTF-8
instance BufferData Char where
  write buf c  =  writeList buf (toUTF8List [c])
  read  buffer@(InStream buf _ pos _) = do
    buf' <- val buf
    pos' <- val pos
    unpackCharUtf8 buf' pos' pos


-- Строка записывается как обычный список символов, но с нулевым символом в конце (в стиле Си)
instance BufferData String where
  write buf str  =  writeList buf (toUTF8List str)  >>  write buf (0::Word8)
  read  buffer@(InStream buf _ pos _) = do
    buf' <- val buf
    pos' <- val pos
    unpackCStringUtf8 buf' pos' pos


-- Целые числа неограниченной точности кодируются методом, являющимся улучшением используемого в 7-zip.
-- При этом величины вплоть до 2^64 требуют для записи переменное число байт: от 1 до 9
instance FastBufferData Integer where
  maxSizeOf x = 9   -- максимум: 1 байт из единичек и 8 байтов данных
  writeUnchecked buf x pos = do
    let write1  x pos  =  writeUnchecked buf (x::Word8)  pos
        write4  x pos  =  writeUnchecked buf (x::Word32) pos
        write_8 x pos  =  writeUnchecked buf (x::Word64) pos
    -- В память записывается сразу 4 или 8 байт, но указатель позиции изменяется только на нужное
    -- число байт. Кол-во младших битов-единичек в первом записанном байте определяет, сколько
    -- дополнительных байт нужно прочитать, чтобы получить всё число
    -- Эта реализация рассчитана только на машины, где первым в памяти идёт младший значащий байт!!!
    -- Кроме того, она оптимизирована для 32-разрядных машин, для 64-битных этот код будет неоптимален
    case () of
     _ | x<0       ->  fail$ "Sorry, FastBufferData.Integer.writeUnchecked don't support negative values like this: "++show x
       | x<128     ->  do write4  (i x*  2+  0) pos; return (pos+1)
       | x<128^2   ->  do write4  (i x*  4+  1) pos; return (pos+2)
       | x<128^3   ->  do write4  (i x*  8+  3) pos; return (pos+3)
       | x<128^4   ->  do write4  (i x* 16+  7) pos; return (pos+4)
       | x<128^5   ->  do write_8 (i x* 32+ 15) pos; return (pos+5)
       | x<128^6   ->  do write_8 (i x* 64+ 31) pos; return (pos+6)
       | x<128^7   ->  do write_8 (i x*128+ 63) pos; return (pos+7)
       | x<128^8   ->  do write_8 (i x*256+127) pos; return (pos+8)
       | x<256^8   ->  do write1 255 pos  >>=  write_8 (i x); return (pos+9)
       | otherwise ->  fail$ "Sorry, FastBufferData.Integer.writeUnchecked don't support numbers larger than 256^8, like this: "++show x

  readUnchecked buf pos = do
    -- Из памяти читаются сразу 4 байта, но из них используются только младшие байты, остальные маскируются
    (x::Word32,_)  <-  readUnchecked buf pos
    case () of
     _ | x .&.  1 ==   0  ->  return (i$ (x `mod` 256^1) `shiftR` 1, pos+1)
       | x .&.  3 ==   1  ->  return (i$ (x `mod` 256^2) `shiftR` 2, pos+2)
       | x .&.  7 ==   3  ->  return (i$ (x `mod` 256^3) `shiftR` 3, pos+3)
       | x .&. 15 ==   7  ->  return (i$ (x            ) `shiftR` 4, pos+4)
       | otherwise -> do
          -- Если значение занимает больше 4-х байт, то прочитать из памяти 8 байтов и опять же замаскировать старшие
          (x::Word64,_)  <-  readUnchecked buf pos
          case () of
           _ | x .&. 31 ==  15  ->  return (i$ (x `mod` 256^5) `shiftR` 5, pos+5)
             | x .&. 63 ==  31  ->  return (i$ (x `mod` 256^6) `shiftR` 6, pos+6)
             | x .&.127 ==  63  ->  return (i$ (x `mod` 256^7) `shiftR` 7, pos+7)
             | x .&.255 == 127  ->  return (i$ (x            ) `shiftR` 8, pos+8)
             | otherwise        ->  do
                 -- И последний вариант - байт из единичных битов плюс 8 байт собственно значения
                 (x::Word64, _) <- readUnchecked buf (pos+1); return (i x, pos+9)


-- Булевские величины поодиночке записываются как значения типа Word8 (т.е на каждое
-- расходуется целый байт), а при записи списка группируются по восемь значений на один байт
instance FastBufferData Bool where
  maxSizeOf x = maxSizeOf (undefined :: Word8)
  writeUnchecked buf x pos  =  writeUnchecked buf (toWord8 x) pos
  readUnchecked buf pos  =  do (x, new_pos) <- readUnchecked buf pos; return (fromWord8 x, new_pos)
{-
  writeListFast buffer  =  writeListFast buffer . makeBytes
   where
    makeBytes (a:b:c:d:e:f:g:h:xs) = (((((((n a*2+n b)*2+n c)*2+n d)*2+n e)*2+n f)*2+n g)*2+n h) : makeBytes xs
    makeBytes [a,b,c,d,e,f,g]      = (((((((n a*2+n b)*2+n c)*2+n d)*2+n e)*2+n f)*2+n g)*2) : []
    makeBytes [a,b,c,d,e,f]        = (((((((n a*2+n b)*2+n c)*2+n d)*2+n e)*2+n f)*2)*2) : []
    makeBytes [a,b,c,d,e]          = (((((((n a*2+n b)*2+n c)*2+n d)*2+n e)*2)*2)*2) : []
    makeBytes [a,b,c,d]            = (((((((n a*2+n b)*2+n c)*2+n d)*2)*2)*2)*2) : []
    makeBytes [a,b,c]              = (((((((n a*2+n b)*2+n c)*2)*2)*2)*2)*2) : []
    makeBytes [a,b]                = (((((((n a*2+n b)*2)*2)*2)*2)*2)*2) : []
    makeBytes [a]                  = (((((((n a*2)*2)*2)*2)*2)*2)*2) : []
    makeBytes []                   = []
    n = toWord8
-}



----------------------------------------------------------------------------------------------------
---- Реализации для составных типов данных ---------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Функции, позволяющие записывать значения любых целочисленных типов в формате с переменной длиной
writeInteger buf  =  write buf.toInteger
readInteger  buf  =  read buf >>= return.fromInteger

{-
-- |Записать список неотрицательных значений, ограниченных сверху величиной max'
-- Для эффективности процесса записи и большей сжимаемости представления все значения при этом
-- представляются 1/2/4/8 байтами
writeBoundList buf max =
  case () of
    _ | toInteger max <= toInteger (maxBound::Word8)   ->  writeList buf . map toWord8
      | toInteger max <= toInteger (maxBound::Word16)  ->  writeList buf . map toWord16
      | toInteger max <= toInteger (maxBound::Word32)  ->  writeList buf . map toWord32
      | toInteger max <= toInteger (maxBound::Word64)  ->  writeList buf . map toWord64
      | otherwise                                      ->  writeList buf

-- |Прочитать список неотрицательных значений, ограниченных сверху величиной max'
-- Для эффективности процесса записи и большей сжимаемости представления все значения при этом
-- представляются 1/2/4/8 байтами
readBoundList buf max n =
  case () of
    _ | toInteger max <= toInteger (maxBound::Word8)   ->  readList buf n >>= return.map fromWord8
      | toInteger max <= toInteger (maxBound::Word16)  ->  readList buf n >>= return.map fromWord16
      | toInteger max <= toInteger (maxBound::Word32)  ->  readList buf n >>= return.map fromWord32
      | toInteger max <= toInteger (maxBound::Word64)  ->  readList buf n >>= return.map fromWord64
      | otherwise                                      ->  readList buf n
-}

instance (BufferData a) => BufferData [a]  where
  write buf list  =  writeInteger buf (length list)  >>   writeList buf list
  read  buf       =  readInteger  buf                >>=  readList  buf

instance (BufferData a, BufferData b) => BufferData (a,b)  where
  write buf (a,b) = write buf a  >>  write buf b
  read  buf       = do a <- read buf; b <- read buf; return (a,b)

instance (BufferData a, BufferData b, BufferData c) => BufferData (a,b,c)  where
  write buf (a,b,c) = write buf ((a,b),c)
  read  buf         = do ((a,b),c) <- read buf; return (a,b,c)

instance (BufferData a, BufferData b, BufferData c, BufferData d) => BufferData (a,b,c,d)  where
  write buf (a,b,c,d) = write buf ((a,b),c,d)
  read  buf           = do ((a,b),c,d) <- read buf; return (a,b,c,d)

instance (BufferData a, BufferData b, BufferData c, BufferData d, BufferData e) => BufferData (a,b,c,d,e)  where
  write buf (a,b,c,d,e) = write buf ((a,b),c,d,e)
  read  buf             = do ((a,b),c,d,e) <- read buf; return (a,b,c,d,e)

instance (BufferData a, BufferData b, BufferData c, BufferData d, BufferData e, BufferData f) => BufferData (a,b,c,d,e,f)  where
  write buf (a,b,c,d,e,f) = write buf ((a,b),c,d,e,f)
  read  buf               = do ((a,b),c,d,e,f) <- read buf; return (a,b,c,d,e,f)

instance (BufferData a, BufferData b, BufferData c, BufferData d, BufferData e, BufferData f, BufferData g) => BufferData (a,b,c,d,e,f,g)  where
  write buf (a,b,c,d,e,f,g) = write buf ((a,b),c,d,e,f,g)
  read  buf                 = do ((a,b),c,d,e,f,g) <- read buf; return (a,b,c,d,e,f,g)

instance (BufferData a, BufferData b, BufferData c, BufferData d, BufferData e, BufferData f, BufferData g, BufferData h) => BufferData (a,b,c,d,e,f,g,h)  where
  write buf (a,b,c,d,e,f,g,h) = write buf ((a,b),c,d,e,f,g,h)
  read  buf                   = do ((a,b),c,d,e,f,g,h) <- read buf; return (a,b,c,d,e,f,g,h)

instance (BufferData a) => BufferData (Maybe a)  where
  write buf (Just  a) = write buf (True,a)
  write buf (Nothing) = write buf False
  read buf = do x <- read buf; if x  then (return.Just =<< read buf)  else (return Nothing)

instance (BufferData a, BufferData b) => BufferData (Either a b)  where
  write buf (Left  a) = write buf (True, a)
  write buf (Right b) = write buf (False,b)
  read buf = do x <- read buf; if x  then (return.Left =<< read buf)  else (return.Right =<< read buf)

{- Попытка сделать универсальный класс для чтения/записи данных
class DerivedBufferData a where
  toTuple   :: BufferData b => a -> b
  fromTuple :: BufferData b => b -> a

instance DerivedBufferData a => BufferData a where
  write buf a  =  write buf (toTuple a)
  read  buf    =  do a <- read buf; return (fromTuple a)

instance (BufferData a, BufferData b) => DerivedBufferData (Either a b)  where
  toTuple   (Left  a)  = (True,  a)
  toTuple   (Right b)  = (False, b)
  fromTuple (True,  a) = (Left   a)
  fromTuple (False, b) = (Right  b)

instance (Enum a) => BufferData a  where
  write buf a = writeInteger buf (fromEnum a)
  read  buf   = readInteger  buf >>= return.toEnum
-}

instance (FastBufferData a, FastBufferData b) => FastBufferData (a,b)  where
  maxSizeOf (a,b)               =  maxSizeOf a + maxSizeOf b
  writeUnchecked buf (a,b) pos  =  writeUnchecked buf a pos  >>=  writeUnchecked buf b
  readUnchecked buf pos         =  do
    (a, pos) <- readUnchecked buf pos
    (b, pos) <- readUnchecked buf pos
    return ((a,b), pos)
  {-# NOINLINE writeUnchecked #-}
  {-# NOINLINE readUnchecked #-}

instance (FastBufferData a, FastBufferData b, FastBufferData c) => FastBufferData (a,b,c)  where
  maxSizeOf (a,b,c)               =  maxSizeOf a + maxSizeOf b + maxSizeOf c
  writeUnchecked buf (a,b,c) pos  =
    writeUnchecked buf a pos
      >>=  writeUnchecked buf b
      >>=  writeUnchecked buf c
  readUnchecked buf pos           =  do
    (a, pos) <- readUnchecked buf pos
    (b, pos) <- readUnchecked buf pos
    (c, pos) <- readUnchecked buf pos
    return ((a,b,c), pos)
  {-# NOINLINE writeUnchecked #-}
  {-# NOINLINE readUnchecked #-}

instance (FastBufferData a, FastBufferData b, FastBufferData c, FastBufferData d) => FastBufferData (a,b,c,d)  where
  maxSizeOf (a,b,c,d)               =  maxSizeOf a + maxSizeOf b + maxSizeOf c + maxSizeOf d
  writeUnchecked buf (a,b,c,d) pos  =
    writeUnchecked buf a pos
      >>=  writeUnchecked buf b
      >>=  writeUnchecked buf c
      >>=  writeUnchecked buf d
  readUnchecked buf pos             =  do
    (a, pos) <- readUnchecked buf pos
    (b, pos) <- readUnchecked buf pos
    (c, pos) <- readUnchecked buf pos
    (d, pos) <- readUnchecked buf pos
    return ((a,b,c,d), pos)
  {-# NOINLINE writeUnchecked #-}
  {-# NOINLINE readUnchecked #-}



----------------------------------------------------------------------------------------------------
---- Вспомогательные функции -----------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Функции преобразования к заданным типам
toWord8  x = toEnum (fromEnum x) :: Word8
toWord16 x = toEnum (fromEnum x) :: Word16
toWord32 x = toEnum (fromEnum x) :: Word32
toWord64 x = i x                 :: Word64

-- |Функции преобразования из заданных типов
fromWord8  (x::Word8 ) = toEnum (fromEnum x)
fromWord16 (x::Word16) = toEnum (fromEnum x)
fromWord32 (x::Word32) = toEnum (fromEnum x)
fromWord64 (x::Word64) = i x

#define STRICT1(f) f a | a `seq` False = undefined
#define STRICT2(f) f a b | a `seq` b `seq` False = undefined
#define STRICT3(f) f a b c | a `seq` b `seq` c `seq` False = undefined
#define STRICT4(f) f a b c d | a `seq` b `seq` c `seq` d `seq` False = undefined
#define STRICT5(f) f a b c d e | a `seq` b `seq` c `seq` d `seq` e `seq` False = undefined
#define STRICT6(f) f a b c d e f | a `seq` b `seq` c `seq` d `seq` e `seq` f `seq` False = undefined

toUTF8List :: String -> [Word8]
STRICT1(toUTF8List)
toUTF8List [] = []
toUTF8List (x:xs)
  | ord x<=0x007f = fromIntegral (ord x):
                    toUTF8List xs
  | ord x<=0x07ff = fromIntegral (0xC0 .|. ((ord x `shiftR` 6) .&. 0x1F)):
                    fromIntegral (0x80 .|. (ord x .&. 0x3F)):
                    toUTF8List xs
  | ord x<=0xffff = fromIntegral (0xE0 .|. ((ord x `shiftR` 12) .&. 0x0F)):
                    fromIntegral (0x80 .|. ((ord x `shiftR` 6) .&. 0x3F)):
                    fromIntegral (0x80 .|. (ord x .&. 0x3F)):
                    toUTF8List xs
  | otherwise     = fromIntegral (0xF0 .|. (ord x `shiftR` 18)) :
                    fromIntegral (0x80 .|. ((ord x `shiftR` 12) .&. 0x3F)) :
                    fromIntegral (0x80 .|. ((ord x `shiftR` 6) .&. 0x3F)) :
                    fromIntegral (0x80 .|. (ord x .&. 0x3F)) :
                    toUTF8List xs


-- | Convert UTF-8 to Unicode.
fromUTF8 :: [Word8] -> String
fromUTF8 xs = fromUTF' (map fromIntegral xs) where
    fromUTF' [] = []
    fromUTF' (all@(x:xs))
	| x<=0x7F = (chr (x)):fromUTF' xs
	| x<=0xBF = err
	| x<=0xDF = twoBytes all
	| x<=0xEF = threeBytes all
	| otherwise   = fourBytes all
    twoBytes (x1:x2:xs) = chr  ((((x1 .&. 0x1F) `shift` 6) .|.
                                  (x2 .&. 0x3F))):fromUTF' xs
    twoBytes _ = error "fromUTF8: illegal two byte sequence"

    threeBytes (x1:x2:x3:xs) = chr ((((x1 .&. 0x0F) `shift` 12) .|.
                                     ((x2 .&. 0x3F) `shift` 6) .|.
                                      (x3 .&. 0x3F))):fromUTF' xs
    threeBytes _ = error "fromUTF8: illegal three byte sequence"

    fourBytes (x1:x2:x3:x4:xs) = chr ((((x1 .&. 0x0F) `shift` 18) .|.
                                       ((x2 .&. 0x3F) `shift` 12) .|.
                                       ((x3 .&. 0x3F) `shift` 6) .|.
                                        (x4 .&. 0x3F))):fromUTF' xs
    fourBytes _ = error "fromUTF8: illegal four byte sequence"

    err = error "fromUTF8: illegal UTF-8 character"


-- |Convert UTF8-encoded byte array to Char
STRICT3(unpackCharUtf8)
unpackCharUtf8 buf pos ref_pos = do
      let addr = castPtr buf :: Ptr Word8
      ch0 <- fromIntegral `liftM` peekElemOff addr pos
      case () of
         _ | ch0 <= 0x7F -> do
                ref_pos =: pos+1
                return $! (unsafeChr (fromIntegral ch0))
           | ch0 <= 0xDF -> do
                ref_pos =: pos+2
                ch1 <- fromIntegral `liftM` peekElemOff addr (pos+1)
                return $! (unsafeChr (((ch0 - 0xC0) `shiftL` 6) +
                                       (ch1 - 0x80)))
           | ch0 <= 0xEF -> do
                ref_pos =: pos+3
                ch1 <- fromIntegral `liftM` peekElemOff addr (pos+1)
                ch2 <- fromIntegral `liftM` peekElemOff addr (pos+2)
                return $! (unsafeChr (((ch0 - 0xE0) `shiftL` 12) +
                                      ((ch1 - 0x80) `shiftL` 6) +
                                       (ch2 - 0x80)))
           | otherwise -> do
                ref_pos =: pos+4
                ch1 <- fromIntegral `liftM` peekElemOff addr (pos+1)
                ch2 <- fromIntegral `liftM` peekElemOff addr (pos+2)
                ch3 <- fromIntegral `liftM` peekElemOff addr (pos+3)
                return $! (unsafeChr (((ch0 - 0xF0) `shiftL` 18) +
                                      ((ch1 - 0x80) `shiftL` 12) +
                                      ((ch2 - 0x80) `shiftL` 6) +
                                       (ch3 - 0x80)))


-- |Convert UTF8-encoded byte array to String
--unpackCStringUtf8 :: Ptr Word8 -> Int -> IO String
STRICT3(unpackCStringUtf8)
unpackCStringUtf8 buf pos ref_pos = do
  unpack pos
  where
    addr = castPtr buf :: Ptr Word8
    unpack nh = do
      ch0 <- fromIntegral `liftM` peekElemOff addr nh
      case () of
         _ | ch0 == 0 -> do
                ref_pos =: nh + 1
                return []
           | ch0 <= 0x7F -> do
                chs <- unpack (nh + 1)
                return $! (unsafeChr (fromIntegral ch0) : chs)
           | ch0 <= 0xDF -> do
                ch1 <- fromIntegral `liftM` peekElemOff addr (nh+1)
                chs <- unpack (nh + 2)
                return $! (unsafeChr (((ch0 - 0xC0) `shiftL` 6) +
                                       (ch1 - 0x80)) : chs)
           | ch0 <= 0xEF -> do
                ch1 <- fromIntegral `liftM` peekElemOff addr (nh+1)
                ch2 <- fromIntegral `liftM` peekElemOff addr (nh+2)
                chs <- unpack (nh + 3)
                return $! (unsafeChr (((ch0 - 0xE0) `shiftL` 12) +
                                      ((ch1 - 0x80) `shiftL` 6) +
                                       (ch2 - 0x80)) : chs)
           | otherwise -> do
                ch1 <- fromIntegral `liftM` peekElemOff addr (nh+1)
                ch2 <- fromIntegral `liftM` peekElemOff addr (nh+2)
                ch3 <- fromIntegral `liftM` peekElemOff addr (nh+3)
                chs <- unpack (nh + 4)
                return $! (unsafeChr (((ch0 - 0xF0) `shiftL` 18) +
                                      ((ch1 - 0x80) `shiftL` 12) +
                                      ((ch2 - 0x80) `shiftL` 6) +
                                       (ch3 - 0x80)) : chs)


----------------------------------------------------------------------------------------------------
---- Example of simple usage of in/out byte streams ------------------------------------------------
----------------------------------------------------------------------------------------------------
{-
test = do
  -- Writing and reading memory buffer as one operation
  --to do: (buf,bufsize)  <-  ByteStream.writeMemory (sign, block_type, crc)
  (sign::Word32, block_type::Int16, crc::Word64)  <-  ByteStream.readMemory buf bufsize

  -- Writing and reading file as one operation
  ByteStream.writeFile "test" [1..1000::Integer]
  (restored::[Integer]) <- ByteStream.readFile "test"

  -- Writing and reading file, divided to low-level operations
  stream <- ByteStream.createFile "test" 5000
  ByteStream.write stream  "asdfr"
  ByteStream.write stream  "12345"
  ByteStream.write stream  [10,20..500::Int]
  ByteStream.write stream  ([10,20..500] ++ [103*10^3, 106*10^6, 109*10^9, 112*10^12, 115*10^15::Integer])
  ByteStream.write stream  (concat$ replicate 100 [True,False,True])
  ByteStream.closeOut stream

  stream <- ByteStream.openFile "test" 5000
  (x::String)    <- ByteStream.read stream
  (y::String)    <- ByteStream.read stream
  (a::[Int])     <- ByteStream.read stream
  (b::[Integer]) <- ByteStream.read stream
  (c::[Bool])    <- ByteStream.read stream
  ByteStream.closeIn stream
  print [x,y]
  print a
  print b
  print c
-}
--Checklist:
--1. +получать буфера функцией receiveBuf = receiveP pipe
--2. +write для Storable по умолчанию - проверяет наличие свободного места и делает pokeByteOff elem
--3. +быстрая запись строк
--4. +использовать writeUnchecked
--5. +доделать кодирование Integer
--6. +переименовать WriteList в WriteListWithoutLength
--7. +упростить имена функций для квалифицированного импорта и сделать read/writeList=writeLength+writeListWithoutLength
--8. правильно переходить с одного буфера на другой
--9. читать более 100 элементов в списке
--10. восстановить кодирование [Bool]
--11. переделать чтение строк чтоб избавиться от reverse (в стёк помещать пормежуточные данные - без tail recursion)
--12. чтение FastBufferData организовать без возврашения tuple - pos сделать или IORef Int, или FastMutInt, или IORef (Ptr CChar)
