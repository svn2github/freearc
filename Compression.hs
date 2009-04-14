----------------------------------------------------------------------------------------------------
---- Упаковка, распаковка и вычисление CRC.                                                     ----
---- Типы данных CompressionMethod, Compressor, UserCompressor - описание метода сжатия.        ----
---- Интерфейс с написанными на Си процедурами, выполняющими всю реальную работу.               ----
----------------------------------------------------------------------------------------------------
module Compression (module Compression, CompressionLib.decompressMem) where

import Control.Concurrent
import Control.Monad
import Data.Bits
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Pool
import Foreign.Ptr
import System.IO.Unsafe

import qualified CompressionLib
import Utils
import Errors
import Files
import qualified ByteStream


-- |Метод сжатия или препроцессор и его параметры
type CompressionMethod  =  CompressionLib.Method

-- Методы "сжатия", поддерживаемые напрямую, а не через CompressionLib
aSTORING              = "storing"
aFAKE_COMPRESSION     = "fake"
aCRC_ONLY_COMPRESSION = "crc"

-- |Фейковые (нераспаковываемые) методы сжатия.
isFakeMethod             =  any_function [==aFAKE_COMPRESSION, ==aCRC_ONLY_COMPRESSION] . method_name
-- |LZP метод сжатия.
isLZP_Method     method  =  method_name method == "lzp"
-- |Tornado метод сжатия.
isTornado_Method method  =  method_name method == "tor"
-- |DICT метод сжатия.
isDICT_Method    method  =  method_name method == "dict"
-- |TTA метод сжатия.
isTTA_Method     method  =  method_name method == "tta"
-- |MM метод сжатия.
isMM_Method      method  =  method_name method == "mm"
-- |JPG метод сжатия.
isJPG_Method     method  =  method_name method == "jpg"
-- |GRZip метод сжатия.
isGRZIP_Method   method  =  method_name method == "grzip"
-- |Очень быстрый метод упаковки (>10 mb/s на 1ГГц процессоре)
isVeryFastMethod         =  CompressionLib.compressionIs "VeryFast?"
-- |Быстрый метод распаковки
isFastDecMethod          =  not . any_function [=="ppmd", =="ppmm", =="pmm", isEXTERNAL_Method] . method_name
-- |Метод сжатия, выполняемый внешней программой
isEXTERNAL_Method        =  CompressionLib.compressionIs "external?"
-- |Метод шифрования.
isEncryption             =  CompressionLib.compressionIs "encryption?"


-- |Последовательность алгоритмов сжатия, используемых для обработки данных
type Compressor = [CompressionMethod]

-- |Метод "storing" (-m0)
aNO_COMPRESSION = [aSTORING] :: Compressor

-- |Очень быстрое сжатие для уже сжатых файлов
aCOMPRESSED_METHOD = split_compressor "tor:8m:c3"

-- |Это - фейковый компрессор, если в нём ровно один метод сжатия и он - фейковый
isFakeCompressor (method:xs)  =  isFakeMethod method  &&  null xs

-- |Это - fake компрессор, если в нём ровно один метод сжатия и он - "fake"
isReallyFakeCompressor (method:xs)  =  method_name method == aFAKE_COMPRESSION  &&  null xs

-- |Это - LZP компрессор, если в нём ровно один метод сжатия и он - LZP
isLZP_Compressor (method:xs)  =  isLZP_Method method  &&  null xs

-- |Это - очень быстрый упаковщик, если в нём ровно один, очень быстрый метод сжатия.
isVeryFastCompressor (method:xs)  =  isVeryFastMethod method  &&  null xs

-- |Это - быстрый распаковщик, если он включает только быстрые методы распаковки
isFastDecompressor = all isFastDecMethod


-- |Выбор компрессора в зависимости от типа обрабатываемых данных.
-- Первый элемент списка безымянен и описывает компрессор, используемый
-- по умолчанию (для файлов всех прочих типов, не описанных в списке явно)
type UserCompressor = [(String,Compressor)]  -- список ассоциаций типа "$text->m3t, $exe->m3x, $compressed->m0"

getCompressors :: UserCompressor -> [Compressor]
getCompressors = map snd

getMainCompressor :: UserCompressor -> Compressor
getMainCompressor = snd.head

-- |Это - метод Storing, если в нём только один компрессор aNO_COMPRESSION для файлов всех типов
isStoring ((_,compressor):xs)  =  compressor==aNO_COMPRESSION  &&  null xs

-- |Это - fake compression, если в нём только один фейковый компрессор для файлов всех типов
isFakeCompression ((_,compressor):xs)  =  isFakeCompressor compressor  &&  null xs

-- |Это - LZP compression, если в нём только один LZP компрессор для файлов всех типов
isLZP_Compression ((_,compressor):xs)  =  isLZP_Compressor compressor  &&  null xs

-- |Это очень быстрая упаковка, если в ней используются только очень быстрые упаковщики для файлов всех типов
isVeryFastCompression = all (isVeryFastCompressor.snd)

-- |Это быстрая распаковка, если в ней используются только быстрые распаковщики для файлов всех типов
isFastDecompression = all (isFastDecompressor.snd)

-- |Найти компрессор, наиболее подходящий для данных типа `ftype`.
-- Если компрессор для файлов этого типа не описан в списке - возвратить компрессор
-- по умолчанию, записанный в первый элемент списка
findCompressor ftype list  =  lookup ftype list  `defaultVal`  snd (head list)

-- |Для записи в оглавление архива информации об использованных алгоритмах сжатия.
instance ByteStream.BufferData Compressor where
  write buf x  =  ByteStream.write buf (join_compressor x)
  read  buf    =  ByteStream.read  buf  >>==  split_compressor


----------------------------------------------------------------------------------------------------
----- Операции над алгоритмами сжатия                                                          -----
----------------------------------------------------------------------------------------------------

class Compression a where
  getCompressionMem              :: a -> Integer
  getDecompressionMem            :: a -> Integer
  getBlockSize                   :: a -> MemSize
  getDictionary                  :: a -> MemSize
  setDictionary                  :: MemSize -> a -> a
  limitCompressionMem            :: MemSize -> a -> a
  limitDecompressionMem          :: MemSize -> a -> a
  limitDictionary                :: MemSize -> a -> a
  limitCompressionMemoryUsage    :: MemSize -> a -> a
  limitDecompressionMemoryUsage  :: MemSize -> a -> a

-- |Превратить функцию из CompressionLib, изменяющую Method, в функцию, изменяющую CompressionMethod
liftSetter action  method | aSTORING ==  method   =  method
liftSetter action  method | isFakeMethod method   =  method
liftSetter action  method                         =  action method

-- |Превратить функцию из CompressionLib, опрашивающую Method, в функцию, опрашивающую CompressionMethod
liftGetter action  method | aSTORING ==  method   =  0
liftGetter action  method | isFakeMethod method   =  0
liftGetter action  method                         =  action method

instance Compression CompressionMethod where
  getCompressionMem              =i.liftGetter   CompressionLib.getCompressionMem
  getDecompressionMem            =i.liftGetter   CompressionLib.getDecompressionMem
  getBlockSize                   =  liftGetter   CompressionLib.getBlockSize
  getDictionary                  =  liftGetter   CompressionLib.getDictionary
  setDictionary                  =  liftSetter . CompressionLib.setDictionary
  limitCompressionMem            =  liftSetter . CompressionLib.limitCompressionMem
  limitDecompressionMem          =  liftSetter . CompressionLib.limitDecompressionMem
  limitDictionary                =  liftSetter . CompressionLib.limitDictionary
  limitCompressionMemoryUsage    =  limitCompressionMem
  limitDecompressionMemoryUsage  =  const id

instance Compression Compressor where
  getCompressionMem              =  calcMem getCompressionMem
  getDecompressionMem            =  calcMem getDecompressionMem
  getBlockSize                   =  maximum . map getBlockSize
  getDictionary                  =  maximum . map getDictionary
  setDictionary                  =  mapLast . setDictionary
  limitCompressionMem            =  map . limitCompressionMem
  limitDecompressionMem          =  map . limitDecompressionMem
  limitDictionary                =  compressionLimitDictionary
  limitCompressionMemoryUsage    =  compressionLimitMemoryUsage
  limitDecompressionMemoryUsage  =  genericLimitMemoryUsage getDecompressionMem

instance Compression UserCompressor where
  -- Определить максимальное потребление памяти / размер блока в заданном UserCompressor
  getCompressionMem              =  maximum . map (getCompressionMem   . snd)
  getDecompressionMem            =  maximum . map (getDecompressionMem . snd)
  getBlockSize                   =  maximum . map (getBlockSize        . snd)
  getDictionary                  =  maximum . map (getDictionary       . snd)
  -- Установить словарь / Ограничить используемую при сжатии/распаковке память
  -- сразу для всех методов, входящих в UserCompressor
  setDictionary                  =  mapSnds . setDictionary
  limitCompressionMem            =  mapSnds . limitCompressionMem
  limitDecompressionMem          =  mapSnds . limitDecompressionMem
  limitDictionary                =  mapSnds . limitDictionary
  limitCompressionMemoryUsage    =  mapSnds . limitCompressionMemoryUsage
  limitDecompressionMemoryUsage  =  mapSnds . limitDecompressionMemoryUsage


-- |Минимальный объём памяти, необходимый для упаковки/распаковки
compressorGetShrinkedCompressionMem    = maximum . map (compressionGetShrinkedCompressionMem . snd)
compressorGetShrinkedDecompressionMem  = maximum . map (compressionGetShrinkedDecompressionMem . snd)
compressionGetShrinkedCompressionMem    = maximum . map getCompressionMem
compressionGetShrinkedDecompressionMem  = maximum . map getDecompressionMem

-- |Ограничить словари для цепочки алгоритмов, прекратив это делать после первого алгоритма,
-- который может существенно раздуть данные (типа precomp). Среди внутренних алгоритмов
-- таких нет, но мы держим под подозрением все внешние :)
compressionLimitDictionary mem (x:xs) =  new_x : (not(isEXTERNAL_Method new_x)  &&&  compressionLimitDictionary mem) xs
                                             where new_x = limitDictionary mem x
compressionLimitDictionary mem []     =  []

-- |Уменьшает потребности в памяти каждого алгоритма в цепочке до mem
-- и затем вставляет между ними вызовы tempfile, если необходимо
compressionLimitMemoryUsage mem  =  genericLimitMemoryUsage getCompressionMem mem . map (limitCompressionMem mem)

-- |Вставляет вызовы tempfile между алгоритмами сжатия, разбивая их на "кластера",
-- умещающиеся в memory_limit+5% (при этом "маленькие" алгоритмы не должны начинать новых кластеров).
-- При этом для dict/dict+lzp используется особый учёт памяти (blocksize*2 на оба, blocksize/2 на выходе),
-- а external compressors обнуляют потребление памяти
genericLimitMemoryUsage getMem memory_limit = go (0::Double) ""
  where go _   _    []      =  []
        go mem prev (x:xs) | isEXTERNAL_Method x          =  x: go 0            x xs
                           | mem+newMem < memlimit*1.05   =  x: go (mem+newMem) x xs
                           | otherwise                    =  "tempfile":x: go newMem x xs

           where newMem | mem==0 && isDICT_Method x             =  realToFrac (getBlockSize x) / 2
                        | isDICT_Method prev && isLZP_Method x  =  0
                        | otherwise                             =  realToFrac$ getMem x
                 memlimit = realToFrac memory_limit

-- |Посчитать потребности в памяти цепочки алгоритмов сжатия с учётом их разбиения.
-- на кластеры по compressionIs "external?" и особым учётом dict/dict+lzp
calcMem getMem  = maximum . map getMemSum . splitOn isEXTERNAL_Method
  where getMemSum (x:y:xs) | isDICT_Method x && isLZP_Method y  =  max (i$ getMem x) (i(getBlockSize x `div` 2) + getMemSum xs)
        getMemSum (x:xs)   | isDICT_Method x                    =  max (i$ getMem x) (i(getBlockSize x `div` 2) + getMemSum xs)
        getMemSum (x:xs)                                        =  i(getMem x) + getMemSum xs
        getMemSum []                                            =  0::Integer

-- |Удаляет все упоминания о "tempfile" из записи алгоритма сжатия.
compressionDeleteTempCompressors = filter (/="tempfile")


----------------------------------------------------------------------------------------------------
----- (De)compression of data stream                                                           -----
----------------------------------------------------------------------------------------------------

{-
compress   method callback      - упаковать данные
decompress method callback      - распаковать данные

  method :: CompressionMethod - алгоритм упаковки
  callback "read" buf size - прочитать входные данные в буфер `buf` длиной `size`
                             Возвращает 0   - конец данных
                                        <0  - прервать (рас)паковку (ошибка или больше данных не нужно)
                                        >0  - кол-во прочитанных байт
  callback "write" buf size - записать выходные данные
                              Возвращает <0  - прервать (рас)паковку (ошибка или больше данных не нужно)
                                         >=0 - всё ок
                              При возвращении из этой функции данные должны быть "использованы", потому что
                                (рас)паковщик может начать запись новых данных на то же место
Входные и выходные буфера выделяются и освобождаются (рас)паковщиком
-}

-- |Процедуры упаковки для различных алгоритмов сжатия.
freearcCompress   num method | aSTORING ==  method =  copy_data
freearcCompress   num method | isFakeMethod method =  eat_data
freearcCompress   num method                       =  checkingCtrlBreak num (CompressionLib.compress method)

-- |Процедуры распаковки для различных алгоритмов сжатия.
freearcDecompress num method | aSTORING ==  method =  copy_data
freearcDecompress num method | isFakeMethod method =  impossible_to_decompress   -- эти типы сжатых данных не подлежат распаковке
freearcDecompress num method                       =  checkingCtrlBreak num (CompressionLib.decompress method)

-- |Поскольку Haskell'овский код, вызываемый из Си, не может получать
-- исключений, добавим к процедурам чтения/записи явные проверки
checkingCtrlBreak num action callback = do
  let checked_callback what buf size auxdata = do
        operationTerminated' <- val operationTerminated
        if operationTerminated'
          then return CompressionLib.aFREEARC_ERRCODE_OPERATION_TERMINATED   -- foreverM doNothing0
          else callback what buf size
  --
  res <- checked_callback "read" nullPtr 0 undefined   -- этот вызов позволяет отложить запуск следующего в цепочке алгоритма упаковки/распаковки до момента, когда предыдущий возвратит хоть какие-нибудь данные (а если это поблочный алгоритм - до момента, когда он обработает весь блок)
  if res<0  then return res
            else action (checked_callback)

-- |Копирование данных без сжатия (-m0)
copy_data callback = do
  allocaBytes aHUGE_BUFFER_SIZE $ \buf -> do  -- используем `alloca`, чтобы автоматически освободить выделенный буфер при выходе
    let go ptr = do
          len <- callback "read" ptr ((buf+:aHUGE_BUFFER_SIZE)-:ptr)
          if (len>0)
            then do let newptr = ptr+:len
                    if newptr < buf+:aHUGE_BUFFER_SIZE
                       then go newptr
                       else do result <- callback "write" buf (newptr-:buf)
                               if (result>=0)
                                 then go buf
                                 else return (result)  -- Возвратим отрицательное число, если произошла ошибка/больше данных не нужно
            else do if (len==0 && ptr>buf)
                      then do result <-  callback "write" buf (ptr-:buf)
                              return (if result>0 then 0 else result)
                      else return len  -- Возвратим 0, если данные кончились, и отрицательное число, если произошла ошибка/больше данных не нужно
    go buf -- возвратить результат

-- |Читаем всё, не пишем ничего, а CRC считается в другом месте ;)
eat_data callback = do
  allocaBytes aBUFFER_SIZE $ \buf -> do  -- используем `alloca`, чтобы автоматически освободить выделенный буфер при выходе
    let go = do
          len <- callback "read" buf aBUFFER_SIZE
          if (len>0)
            then go
            else return len   -- Возвратим 0, если данные кончились, и отрицательное число, если произошла ошибка/больше данных не нужно
    go  -- возвратить результат

impossible_to_decompress callback = do
  return CompressionLib.aFREEARC_ERRCODE_GENERAL   -- сразу возвратить ошибку, поскольку этот алгоритм (FAKE/CRC_ONLY) не подлежит распаковке

{-# NOINLINE checkingCtrlBreak               #-}
{-# NOINLINE copy_data                       #-}
{-# NOINLINE eat_data                        #-}


----------------------------------------------------------------------------------------------------
----- CRC calculation ------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |CRC файла
type CRC  = CUInt
aINIT_CRC = 0xffffffff  :: CRC
updateCRC addr len  =  c_UpdateCRC addr (i len)
finishCRC = xor aINIT_CRC

-- |Посчитать CRC данных в буфере
calcCRC addr len  =  updateCRC addr len aINIT_CRC  >>==  finishCRC

-- |Посчитать CRC не-unicode строки (символы с кодами 0..255)
crc32 str  =  unsafePerformIO$ withCStringLen str (uncurry calcCRC)

-- |Fast C routine for CRC-32 calculation
foreign import ccall safe "Environment.h UpdateCRC"
   c_UpdateCRC :: Ptr CChar -> CUInt -> CRC -> IO CRC


-------------------------------------------------------------------------------------------------------------
-- Encode/decode compression method for parsing options/printing info about selected compression method -----
-------------------------------------------------------------------------------------------------------------

-- |Parse command-line option that represents compression method.
-- Декодировать запись метода сжатия в виде текстовой строки, превратив его в список ассоциаций
-- "тип файла -> метод сжатия". Первый элемент этого списка описывает метод сжатия по умолчанию
decode_method configuredMethodSubsts str =
    str                       -- "3/$obj=2b/$iso=ecm+3b"
    .$ subst list             -- "3b/3t/$obj=2b/$iso=ecm+3b"
    .$ split_to_methods       -- [("","exe+3b"), ("$obj","3b"), ("$text","3t"), ("$obj","2b"), ("$iso","ecm+3b")]
    .$ keepOnlyLastOn fst     -- [("","exe+3b"), ("$text","3t"), ("$obj","2b"), ("$iso","ecm+3b")]
    .$ filter (not.null.snd)  -- "-m$bmp=" означает запретить использование специального алгоритма для группы $bmp
    .$ mapSnds (subst2 list)  -- [("",["exe","lzma"]), ("$text",["ppmd"]), ("$obj",["lzma"]), ("$iso",["ecm","lzma"])]

    where list = prepareSubsts (concatMap reorder [configuredMethodSubsts, builtinMethodSubsts])   -- сначала пользовательские замены, затем встроенные, чтобы дать первым приоритет
          reorder list = a++b  where (a,b) = partition (notElem '#') list                          -- внутри этих групп: сначала строчки, не содержащие #, затем с # (сначала конкретные, затем общие замены)

-- Замена по списку для метода сжатия (обобщённого обозначения для файлов всех типов)
subst list method  =  joinWith "/" (main_methods:group_methods++user_methods)
  where -- Из записи типа -m3/$obj=2b выделяем для расшифровки только первую часть, до слеша
        main:user_methods = split '/' method
        -- Расшифровка основных методов сжатия, типа 3x = 3xb/3xt
        main_methods = case (lookup main list) of
            Just x  -> subst list x   -- При успехе повторяем рекурсивно
            Nothing -> main           -- Больше подстановок нет
        -- Найдём в списке подстановок дополнительные методы сжатия для отдельных групп, типа 3x$iso = ecm+exe+3xb
        group_methods = list .$ keepOnlyFirstOn fst                      -- удалим повторные определения (не очень эффективно делать это именно здесь, зато по месту использования)
                             .$ mapMaybe (startFrom main . join2 "=")    -- оставим только определения, начинающиеся с 3x, удалив это 3x
                             .$ filter (("$"==).take 1)                  -- а из них - только начинающиеся с $

-- Замена по списку для алгоритма сжатия (посл-ти компрессоров для конкретного типа файлов)
subst2 list  =  concatMap f . split_compressor
    where f method = let (head,params)  =  break (==':') method
                     in case (lookup head list) of
                          Just new_head -> subst2 list (new_head++params)
                          Nothing       -> [decode_one_method method]

-- |Декодировать явно описанный метод сжатия.
decode_one_method method | isFakeMethod method = method
                         | otherwise           = CompressionLib.canonizeCompressionMethod method

-- Превращает длинную строку, описывающую методы сжатия для разных типов файлов,
-- в массив ассоциаций (тип файла, метод сжатия)
split_to_methods method = case (split '/' method) of
    [_]                 ->  [("",method)]   -- один метод для файлов всех типов
    x : xs@ (('$':_):_) ->  ("",x) : map (split2 '=') xs   -- m1/$type=m2...
    b : t : xs          ->  [("","exe+"++b), ("$obj",b), ("$text",t)] ++ map (split2 '=') xs   -- m1/m2/$type=m3...

-- Подготовить список замен к использованию в lookup
prepareSubsts x = x
    -- Удалить пустые строки, пробелы и комментарии
    .$ map (filter (not.isSpace) . fst . split2 ';') .$ filter (not.null)
    -- Заменить каждую строку с символом # на 9 строк, где # пробегает значения от 1 до 9
    .$ concatMap (\s -> if s `contains` '#'  then map (\d->replace '#' d s) ['1'..'9']  else [s])
    -- Преобразовать список строк вида "a=b" в список для lookup
    .$ map (split2 '=')

-- Встроенные описания методов сжатия в формате, аналогичном используемому в arc.ini
builtinMethodSubsts = [
      ";High-level method definitions"
    , "x  = 9            ;highest compression mode using only internal algorithms"
    , "ax = 9p           ;highest compression mode involving external compressors"
    , "0  = storing"
    , "1  = 1b  / $exe=exe+1b"
    , "1x = 1"
    , "#  = #rep+exe+#xb / $obj=#b / $text=#t"
    , "#x = #xb/#xt"
    , ""
    , ";Text files compression with slow decompression"
    , "1t  = 1b"
    , "2t  = grzip:m4:8m:32:h15"
    , "3t  = dict:p: 64m:85% + lzp: 64m: 24:h20        :92% + grzip:m3:8m:l"
    , "4t  = dict:p: 64m:80% + lzp: 64m: 65:d1m:s16:h20:90% + ppmd:8:96m"
    , "5t  = dict:p: 64m:80% + lzp: 80m:105:d1m:s32:h22:92% + ppmd:12:192m"
    , "6t  = dict:p:128m:80% + lzp:160m:145:d1m:s32:h23:92% + ppmd:16:384m"
    , "7t  = dict:p:128m:80% + lzp:320m:185:d1m:s32:h24:92% + ppmd:20:768m"
    , "8t  = dict:p:128m:80% + lzp:640m:225:d1m:s32:h25:92% + ppmd:24:1536m"
    , "9t  = dict:p:128m:80% + lzp:800m:235:d1m:s32:h26:92% + ppmd:25:2047m"
    , ""
    , ";Binary files compression with slow and/or memory-expensive decompression"
    , "1b  = 1xb"
    , "#b  = #rep+#bx"
    , "2rep  = rep:  96m"
    , "3rep  = rep:  96m"
    , "4rep  = rep:  96m"
    , "5rep  = rep: 128m"
    , "6rep  = rep: 256m"
    , "7rep  = rep: 512m"
    , "8rep  = rep:1024m"
    , "9rep  = rep:2047m"
    , ""
    , ";Text files compression with fast decompression"
    , "1xt = 1xb"
    , "2xt = 2xb"
    , "3xt = dict:  64m:80% + tor:7:96m:h64m"
    , "4xt = dict:  64m:75% + 4binary"
    , "#xt = dict: 128m:75% + #binary"
    , ""
    , ";Binary files compression with fast decompression"
    , "1xb = tor:3"
    , "2xb = tor:96m:h64m"
    , "#xb = delta + #binary"
    , ""
    , ";Binary files compression with fast decompression"
    , "1binary = tor:3"
    , "2binary = tor:  96m:h64m"
    , "3binary = lzma: 96m:h64m:fast  :mc8"
    , "4binary = lzma: 96m:h64m:normal:mc16"
    , "5binary = lzma: 16m:max"
    , "6binary = lzma: 32m:max"
    , "7binary = lzma: 64m:max"
    , "8binary = lzma:128m:max"
    , "9binary = lzma:255m:max"
    , ""
    , ";Synonyms"
    , "bcj = exe"
    , "#bx = #xb"
    , "#tx = #xt"
    , "x#  = #x"    -- принимаем опции типа "-mx7" для мимикрии под 7-zip
    , ""
    , ";Compression modes involving external PPMONSTR.EXE"
    , "#p  = #rep+exe+#xb / $obj=#pb / $text=#pt"
    , "5pt = dict:p: 64m:80% + lzp: 64m:32:h22:85% + pmm: 8:160m:r0"
    , "6pt = dict:p: 64m:80% + lzp: 64m:64:h22:85% + pmm:16:384m:r1"
    , "7pt = dict:p:128m:80% + lzp:128m:64:h23:85% + pmm:20:768m:r1"
    , "8pt = dict:p:128m:80% + lzp:128m:64:h23:85% + pmm:24:1536m:r1"
    , "9pt = dict:p:128m:80% + lzp:128m:64:h23:85% + pmm:25:2047m:r1"
    , "#pt = #t"
    , "#pb = #b"
    , ""
    , "#q  = #qb/#qt"
    , "5qt = dict:p:64m:80% + lzp:64m:64:d1m:24:h22:85% + pmm:10:160m:r1"
    , "5qb = rep: 128m      + delta                     + pmm:16:160m:r1"
    , "6qb = rep: 256m      + delta                     + pmm:20:384m:r1"
    , "7qb = rep: 512m      + delta                     + pmm:22:768m:r1"
    , "8qb = rep:1024m      + delta                     + pmm:24:1536m:r1"
    , "9qb = rep:2047m      + delta                     + pmm:25:2047m:r1"
    , "#qt = #pt"
    , "#qb = #pb"
    , ""
    , ";Sound wave files are compressed best with TTA"
    , "wav     = tta      ;best compression"
    , "wavfast = tta:m1   ;faster compression and decompression"
    , "1$wav  = wavfast"
    , "2$wav  = wavfast"
    , "#$wav  = wav"
    , "#x$wav = wavfast"
    , "#p$wav = wav"
    , ""
    , ";Bitmap graphic files are compressed best with GRZip"
    , "bmp        = mm    + grzip:m1:l:a  ;best compression"
    , "bmpfast    = mm    + grzip:m4:l:a  ;faster compression"
    , "bmpfastest = mm:d1 + tor:3:t0      ;fastest one"
    , "1$bmp  = bmpfastest"
    , "2$bmp  = bmpfastest"
    , "3$bmp  = bmpfast"
    , "#$bmp  = bmp"
    , "1x$bmp = bmpfastest"
    , "2x$bmp = bmpfastest"
    , "#x$bmp = mm+#binary"
    , "#p$bmp = bmp"
    , ""
    , ";Quick & dirty compression for data already compressed"
    , "4$compressed   = rep:96m + tor:c3"
    , "3$compressed   = rep:96m + tor:3"
    , "2$compressed   = rep:96m + tor:3"
    , "4x$compressed  = tor:8m:c3"
    , "3x$compressed  = rep:8m  + tor:3"
    , "2x$compressed  = rep:8m  + tor:3"
    ]

-- |Мультимедийный тип файлов?
isMMType x  =  x `elem` words "$wav $bmp"

-- |В некотором смысле обратная операция - угадывание типа файла по его компрессору
typeByCompressor c  =  case (map method_name c) of
  xs | xs `contains` "tta"        -> "$wav"
     | xs `contains` "mm"         -> "$bmp"
     | xs `contains` "grzip"      -> "$text"
     | xs `contains` "ppmd"       -> "$text"
     | xs `contains` "pmm"        -> "$text"
     | xs `contains` "dict"       -> "$text"
     | xs == aNO_COMPRESSION      -> "$compressed"
     | xs == ["rep","tor"]        -> "$compressed"
     | xs `contains` "ecm"        -> "$iso"
     | xs `contains` "precomp"    -> "$precomp"
     | xs == ["precomp","rep"]    -> "$jpgsolid"
     | xs `contains` "jpg"        -> "$jpg"
     | xs `contains` "exe"        -> "$binary"
     | xs `contains` "lzma"       -> "$obj"
     | xs `contains` "tor"        -> "$obj"
     | otherwise                  -> "default"

-- |Список всех типов файлов, обнаруживаемых подобным образом
typesByCompressor = words "$wav $bmp $text $compressed $iso $precomp $jpgsolid $jpg $obj $binary $exe"


-- |Human-readable description of compression method
encode_method uc  =  joinWith ", " (map encode_one_method uc)
encode_one_method (group,compressor)  =  between group " => " (join_compressor compressor)
join_compressor   =  joinWith "+"

-- |Opposite to join_compressor (used to read compression method from archive file)
split_compressor  =  split '+'

-- |Обработать алгоритмы в компрессоре императивной операцией process
process_algorithms process compressor = do
    return (split_compressor compressor)
       >>=  mapM process
       >>== join_compressor

-- |Разбить метод сжатия на заголовок и отдельные параметры
split_method = split ':'

-- |Имя метода сжатия.
method_name = head . split_method

-- |Строка, информирующая пользователя об используемом объёме памяти
showMem 0      = "0b"
showMem mem    = showM [(gb,"gb"),(mb,"mb"),(kb,"kb"),(b,"b"),error"showMem"] mem

showMemory 0   = "0 bytes"
showMemory mem = showM [(gb," gbytes"),(mb," mbytes"),(kb," kbytes"),(b," bytes"),error"showMemory"] mem

showM xs@( (val,str) : ~(nextval,_) : _) mem =
  if mem `mod` val==0 || mem `div` nextval>=4096
    then show((mem+val`div` 2) `div` val)++str
    else showM (tail xs) mem

-- |Округлить объём памяти вверх так, чтобы он приобрёл читабельность
roundMemUp mem | mem>=4096*kb = mem `roundUp` mb
               | otherwise    = mem `roundUp` kb

{-# NOINLINE builtinMethodSubsts #-}
{-# NOINLINE decode_method #-}
{-# NOINLINE showMem #-}

