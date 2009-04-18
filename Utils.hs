{-# OPTIONS_GHC -cpp -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction #-}
---------------------------------------------------------------------------------------------------
---- Вспомогательные функции: работа со строками, списками, регулярными выражениями,           ----
----   аллокатор памяти. упрощение манипуляций с IORef-переменными,                            ----
----   определение удобных операций и управляющих структур программы.                          ----
---------------------------------------------------------------------------------------------------
module Utils (module Utils, module CompressionLib) where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Array
import Data.Bits
import Data.Char
import Data.Either
import Data.IORef
import Data.List
import Data.Maybe
import Data.Word
import Debug.Trace
import Foreign.Marshal.Utils
import Foreign.Ptr

import CompressionLib (MemSize,b,kb,mb,gb,tb)

---------------------------------------------------------------------------------------------------
---- Проверим define's ----------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

#if !defined(FREEARC_WIN) && !defined(FREEARC_UNIX)
#error "You must define OS!"
#endif

#if !defined(FREEARC_INTEL_BYTE_ORDER) && !defined(FREEARC_MOTOROLA_BYTE_ORDER)
#error "You must define byte order!"
#endif


---------------------------------------------------------------------------------------------------
---- Разное :) ------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Собрать 4-байтовое число из отдельных байтов
#if defined(FREEARC_INTEL_BYTE_ORDER)
make4byte b0 b1 b2 b3 = b0+256*(b1+256*(b2+256*b3)) :: Word32
#else
make4byte b0 b1 b2 b3 = b3+256*(b2+256*(b1+256*b0)) :: Word32
#endif

-- |Разборщик чисел, указываемых во всяких опциях. Толкование числа определяется символами,
-- написанными после него (b/k/f/...), если их нет - используется `default_specifier`.
-- Результат возвращается в виде пары, второй элемент в которой - `b`, если результат выражен в байтах,
-- или другой символ, записанный после числа, или переданный как `default_specifier`.
parseNumber num default_specifier =
  case (span isDigit$ strLower$ num++[default_specifier]) of
    (digits, 'b':_)  ->  (readI digits     , 'b')
    (digits, 'k':_)  ->  (readI digits * kb, 'b')
    (digits, 'm':_)  ->  (readI digits * mb, 'b')
    (digits, 'g':_)  ->  (readI digits * gb, 'b')
    (digits, 't':_)  ->  (readI digits * tb, 'b')
    (digits, '^':_)  ->  (2 ^ readI digits , 'b')
    (digits,  c :_)  ->  (readI digits     ,  c )

-- |Расшифровать запись размера, по умолчанию - в байтах
parseSize memstr =
    case (parseNumber memstr 'b') of
        (bytes, 'b')  ->  bytes
        _             ->  error$ memstr++" - unrecognized size specifier"

-- |Расшифровать запись объёма памяти: "512b", "32k", "8m" и так далее. "24" означает 24mb
parseMem memstr =
    case (parseNumber memstr 'm') of
        (bytes, 'b')  ->  clipToMaxMemSize bytes
        _             ->  error$ memstr++" - unrecognized size specifier"

-- |Аналогично parseMem, но с добавлением поддержки записи в виде 75%/75p (от общего объёма памяти)
parseMemWithPercents memory memstr =
    case (parseNumber memstr 'm') of
        (bytes,    'b')  ->  clipToMaxMemSize$ bytes
        (percents, c) | c `elem` "%p"
                         ->  clipToMaxMemSize$ (memory * percents) `div` 100
        _                ->  error$ memstr++" - unrecognized size specifier"

-- Должна усекать к максимальному позволенному в MemSize числу или может вообще выдавать ошибку?
clipToMaxMemSize x | x < i(maxBound::MemSize) = i x
                   | otherwise                = i(maxBound::MemSize)

readI = foldl f 0
  where f m c | isDigit c  =  fromIntegral (ord c - ord '0') + (m * 10)
              | otherwise  =  error ("Non-digit "++[c]++" in readI")

readInt :: String -> Int
readInt = readI

readSignedInt ('-':xs) = - readInt xs
readSignedInt      xs  =   readInt xs

isSignedInt = all isDigit.tryToSkip "-"

lb :: Integral a =>  a -> Int
lb 0 = 0
lb 1 = 0
lb n = 1 + lb (n `div` 2)


{-# NOINLINE parseNumber          #-}
{-# NOINLINE parseSize            #-}
{-# NOINLINE parseMem             #-}
{-# NOINLINE parseMemWithPercents #-}
{-# NOINLINE readI                #-}
{-# NOINLINE readInt              #-}


-- |Некоторые операции для более изящной записи программ
infixl 9  .$
infixl 1  >>==, ==<<, =<<., .>>=, .>>, .>>==, ==<<.
a.$b         =  b a                -- вариант $ с перевёрнутым порядком аргументов
a>>==b       =  a >>= return.b     -- вариант >>=, в котором второй аргумент нуждаетcя в лифтинге
a==<<b       =  return.a =<< b     -- вариант =<<, в котором первый аргумент нуждаетcя в лифтинге
(a=<<.b) c   =  a =<< b c          -- вариант =<< для применения в mapM и тому подобных местах
(a.>>=b) c   =  a c >>= b          -- вариант >>= для применения в mapM и тому подобных местах
(a.>>b)  c   =  a c >> b           -- вариант >> для применения в mapM и тому подобных местах
(a==<<.b) c  =  return.a =<< b c   -- вариант ==<< для применения в mapM и тому подобных местах
(a.>>==b) c  =  a c >>= return.b   -- вариант >>== для применения в mapM и тому подобных местах

-- Типы данных, имеющие значения по умолчанию
class    Defaults a      where defaultValue :: a
instance Defaults ()     where defaultValue = ()
instance Defaults Bool   where defaultValue = False
instance Defaults [a]    where defaultValue = []
instance Defaults (a->a)               where defaultValue = id
instance Defaults (Maybe a)            where defaultValue = Nothing
instance Defaults (a->IO a)            where defaultValue = return
instance Defaults a => Defaults (IO a) where defaultValue = return defaultValue
instance Defaults Int                  where defaultValue = 0
instance Defaults Integer              where defaultValue = 0
instance Defaults Double               where defaultValue = 0

class    TestDefaultValue a       where isDefaultValue :: a -> Bool
instance TestDefaultValue Bool    where isDefaultValue = not
instance TestDefaultValue [a]     where isDefaultValue = null
instance TestDefaultValue Int     where isDefaultValue = (==0)
instance TestDefaultValue Integer where isDefaultValue = (==0)
instance TestDefaultValue Double  where isDefaultValue = (==0)

infixr 3  &&&
infixr 2  |||

-- |Дать величине значение по умолчанию
a ||| b | isDefaultValue a = b
        | otherwise        = a

-- |Возвратить второе значение, если первое не является значением по умолчанию
a &&& b | isDefaultValue a = defaultValue
        | otherwise        = b

-- |Применить функцию f к списку только если он не пустой
unlessNull f xs  =  xs &&& f xs

-- |Монадический вариант concatMap
concatMapM :: Monad io => (a -> io [b]) -> [a] -> io [b]
concatMapM f x  =  mapM f x  >>==  concat

-- |Условное выполнение
whenM cond action = do
  allow <- cond
  when allow
    action

unlessM = whenM . liftM not

-- |Выполнить `action` над значением, возвращённым `x`, если оно не Nothing
whenJustM  x action  =  x >>= (`whenJust` action)

whenJustM_ x action  =  x >>= (`whenJust_` action)

whenJust   x action  =  x .$ maybe (return Nothing) (action .>>== Just)

whenJust_  x action  =  x .$ maybe (return ()) (action .>> return ())

-- |Выполнить `action` над значением, возвращённым `x`, если оно "Right _"
whenRightM_ x action  =  x >>= either doNothing (action .>> return ())

-- |Выполнить onLeft/onRight над значением, возвращённым `x`
eitherM_ x onLeft onRight  =  x >>= either (onLeft  .>> return ())
                                           (onRight .>> return ())

-- |Выполнить для каждого элемента из списка и вовзратить результат как список
foreach = flip mapM

-- |Выполнить для каждого элемента из списка
for = flip mapM_

-- |Условное выполнение с условием в конце строки
infixr 0 `on`
on = flip (&&&)

-- |Удобный способ записать сначала то, что должно обязательно быть выполнено в конце :)
doFinally = flip finally

-- |Выполнить onError при ошибке в acquire, и action в противном случае
handleErrors onError acquire action = do
  x <- try acquire
  case x of
    Left  err -> onError
    Right res -> action res

-- |Записать в начале то, что нужно выполнить в конце
atExit a b = (b>>a)

-- |Выполнить действие только один раз, при var=True
once var action = do whenM (val var) action; var =: False
init_once       = ref True

-- |Заглушки на место команд, которые не должны ничего выполнять
doNothing0       = return ()
doNothing  a     = return ()
doNothing2 a b   = return ()
doNothing3 a b c = return ()

-- |Игнорировать исключени
ignoreErrors  =  handle doNothing

-- |Создать новый Channel и записать в него начальный список значений
newChanWith xs = do c <- newChan
                    writeList2Chan c xs
                    return c

-- |Константные функции
const2 x _ _ = x
const3 x _ _ _ = x
const4 x _ _ _ _ = x

-- |Нафига вам этот ThreadId??
forkIO_ action = forkIO action >> return ()

-- |Повторять бесконечно
foreverM action = do
  action
  foreverM action

-- |Управляющая структура, аналогичная циклу 'while' в обычных языках
repeat_while inp cond out = do
  x <- inp
  if (cond x)
    then do out x
            repeat_while inp cond out
    else return x

-- |Управляющая структура, аналогичная repeat-until в Паскале
repeat_until action = do
  done <- action
  when (not done) $ do
    repeat_until action

-- |Управляющая структура, разбивающая выполнение операции размером size
-- на отдельные операции размером не более chunk кажда
doChunks size chunk action =
  case size of
    0 -> return ()
    _ -> do let n = minI size chunk
            action (fromIntegral n)
            doChunks (size-n) chunk action

-- |Выполнить `action` над x, затем над каждым элементом списка, возвращённого из `action`, и так далее рекурсивно
recursiveM action x  =  action x >>= mapM_ (recursiveM action)

-- |Выполнить рекурсивно, если верно условие `cond`, и однократно в противном случае
recursiveIfM cond action x  =  if cond  then recursiveM action x  else (action x >> return ())

-- |Выполняет действие `action` над элементами списка `list` поочерёдно, возвращая
-- список результатов, возвращённых `action` - в общем, аналогично mapM.
-- Но дополнительно к этому проверяет обработанные данные по критерию `crit_f` и выходит из цикла,
-- если этот критерий удовлетворён. Поэтому дополнительно возвращается список необработанных
-- значений из `list`
mapMConditional (init,map_f,sum_f,crit_f) action list = do
  let go []     ys summary = return (reverse ys, [])     -- завершено из-за исчерпания списка
  let go (x:xs) ys summary = do
        y <- action x
        let summary  =  sum_f summary (map_f y)
        if (crit_f summary)
          then return (reverse$ y:ys, xs)                -- завершено согласно критерию
          else go xs (y:ys) summary
  go list [] init

-- |Execute action with background computation
withThread thread  =  bracket (forkIO thread) killThread . const

-- |Выполнить действие в другом треде и возвратить конечный результат
bg action = do
  resultVar <- newEmptyMVar
  forkIO (action >>= putMVar resultVar)
  takeMVar resultVar


{-# NOINLINE foreverM #-}
{-# NOINLINE repeat_while #-}
{-# NOINLINE repeat_until #-}
{-# NOINLINE mapMConditional #-}
{-# NOINLINE bg #-}


-- |Отфильтровать список с помощью монадического (выполняемого) предиката
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM p  =  go []
  where go accum []      =  return$ reverse accum
        go accum (x:xs)  =  p x  >>=  bool (go    accum  xs)
                                           (go (x:accum) xs)

-- |mapMaybe, перенесённая в класс Monad
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f  =  go []
  where go accum []      =  return$ reverse accum
        go accum (x:xs)  =  f x  >>=  maybe (      go    accum  xs)
                                            (\r -> go (r:accum) xs)

-- |@firstJust@ takes a list of @Maybes@ and returns the
-- first @Just@ if there is one, or @Nothing@ otherwise.
firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x  : ms) = Just x
firstJust (Nothing : ms) = firstJust ms

-- |Вернуть первый успешный (Just) результат применения f к списку или Nothing
firstMaybe :: (a -> Maybe b) -> [a] -> Maybe b
firstMaybe f  =  firstJust . map f

-- |Заменить Nothing на значение по умолчанию
defaultVal = flip fromMaybe

-- |Заменить Nothing на значение по умолчанию - для императивной операции
defaultValM = liftM2 defaultVal

-- |Выбрать одно из двух значений в зависимости от последнего аргумента
bool onFalse onTrue False  =  onFalse
bool onFalse onTrue True   =  onTrue

-- |if без синт. оверхеда
iif True  onTrue onFalse  =  onTrue
iif False onTrue onFalse  =  onFalse

-- Применить к списку одну из двух функций в зависимости от того, пуст ли он
list onNotNull onNull [] = onNull
list onNotNull onNull xs = onNotNull xs

-- |Возвратить True, если значение - не Nothing
maybe2bool (Just _) = True
maybe2bool Nothing  = False

-- |Проверка на Left
isLeft (Left _) = True
isLeft _        = False

-- |Удалить элементы, отвечающие заданному предикату
deleteIf p = filter (not.p)

-- |Удалить элементы, соответствующие любому из списка предикатов
deleteIfs = deleteIf.anyf

-- |Обновление lookup-списка
update list a@(key,value)  =  a : [x | x@(k,v)<-list, k/=key]

-- |Замена значений по списку
changeTo list value  =  lookup value list `defaultVal` value

-- |Напечатать и возвратить одно значение
trace2 s = trace (show s) s

-- |Evaluate list elements
evalList (x:xs) = x `seq` evalList xs
evalList []     = ()

{-
-- Cale Gibbard

A useful little higher order function. Some examples of use:

swing map :: forall a b. [a -> b] -> a -> [b]
swing any :: forall a. [a -> Bool] -> a -> Bool
swing foldr :: forall a b. b -> a -> [a -> b -> b] -> b
swing zipWith :: forall a b c. [a -> b -> c] -> a -> [b] -> [c]
swing find :: forall a. [a -> Bool] -> a -> Maybe (a -> Bool) -- applies each of the predicates to the given value, returning the first predicate which succeeds, if any
swing partition :: forall a. [a -> Bool] -> a -> ([a -> Bool], [a -> Bool])

-}

swing :: (((a -> b) -> b) -> c -> d) -> c -> a -> d
swing f = flip (f . flip ($))


-- |Map on functions instead of its' arguments!
map_functions []     x  =  []
map_functions (f:fs) x  =  f x : map_functions fs x

-- |Проверить, что все функции из списка дают True на (опущенном здесь) аргументе. Эффективней, чем swing all
allf x = all_functions x
all_functions []  = const True
all_functions [f] = f
all_functions fs  = and . map_functions fs

-- |Проверить, что хоть одна функция из списка даёт True на (опущенном здесь) аргументе. Эффективней, чем swing any
anyf x = any_function x
any_function []  = const False
any_function [f] = f
any_function fs  = or . map_functions fs

-- |Применить к аргументу последовательно все функции из списка
applyAll []     x = x
applyAll (f:fs) x = applyAll fs (f x)

(f>>>g) x = g(f x)


---------------------------------------------------------------------------------------------------
---- Операции над строками ------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Разбить строку на две подстроки, разделённые заданным символом
split2 :: (Eq a) => a -> [a] -> ([a],[a])
split2 c s  =  (chunk, drop 1 rest)
  where (chunk, rest) = break (==c) s

-- |Соединить их назад как было :)
join2 :: [a] -> ([a],[a]) -> [a]
join2 between (a,b) = a++between++b

-- |Разбить строку на подстроки, разделённые заданным символом
split :: (Eq a) => a -> [a] -> [[a]]
split c s =
  let (chunk, rest) = break (==c) s
  in case rest of  []     -> [chunk]
                   _:rest -> chunk : split c rest

-- |Соединить список строк в единый текст с разделителем: "one, two, three"
joinWith :: [a] -> [[a]] -> [a]
joinWith x  =  concat . intersperse x

-- |Соединить список строк в единый текст, используя два разных разделителя:
-- joinWith2 ", " " and " ["one","two","three","four"]  -->  "one, two, three and four"
joinWith2 :: [a] -> [a] -> [[a]] -> [a]
joinWith2 a b []    =  []
joinWith2 a b [x]   =  x
joinWith2 a b list  =  joinWith a (init list) ++ b ++ last list

-- |Поставить x между s1 и s2, если обе строки непустые
between s1 x [] = s1
between [] x s2 = s2
between s1 x s2 = s1++x++s2

-- |Добавить двойные кавычки вокруг строки
quote :: String -> String
quote str  =  "\"" ++ str ++ "\""

-- |Удалить двойные кавычки вокруг строки (если они есть)
unquote :: String -> String
unquote ('"':str) | str>"" && x=='"'  =  xs     where (x:xs) = reverse str
unquote str = str

contains = flip elem

-- |Удалить n элементов в конце спсика
dropEnd n  =  reverse . drop n . reverse

-- |Истина, если `s` содержит хотя бы один из элементов множества `set`
s `contains_one_of` set  =  any (`elem` set) s

-- |Последние n элементов
n `lastElems` xs  =  drop (length xs - n) xs

-- |Заменить n'й элемент (считая с 0) в списке `xs` на `x`
replaceAt n x xs  =  hd ++ x : drop 1 tl
    where (hd,tl) = splitAt n xs

-- |Изменить n'й элемент (считая с 0) в списке `xs` с `x` на `f x`
updateAt n f xs  =  hd ++ f x : tl
    where (hd,x:tl) = splitAt n xs

-- |Заменить в списке все вхождения элемента 'from' на 'to'
replace from to  =  map (\x -> if x==from  then to  else x)

-- |Если первая строка является префиксом второй - возвратить остаток второй строки, иначе Nothing
startFrom (x:xs) (y:ys) | x==y  =  startFrom xs ys
startFrom [] str                =  Just str
startFrom _  _                  =  Nothing

-- |Проверка, что строка начинается или заканчивается заданными символами
beginWith s = isJust . startFrom s
endWith   s = beginWith (reverse s) . reverse

-- |Попытаемся удалить строку substr в начале str
tryToSkip substr str  =  (startFrom substr str) `defaultVal` str

-- |Попытаться удалить строку substr в конце str
tryToSkipAtEnd substr str = reverse (tryToSkip (reverse substr) (reverse str))

-- | The 'isInfixOf' function takes two lists and returns 'True'
-- if the second list is contained, wholy and intact,
-- anywhere within the first.
substr haystack needle  =  any (needle `isPrefixOf`) (tails haystack)

-- |Список позиций подстроки в строке
strPositions haystack needle  =  elemIndices True$ map (needle `isPrefixOf`) (tails haystack)

-- |Заменить в строке `s` все вхождения `from` на `to`
replaceAll from to = repl
  where repl s      | Just remainder <- startFrom from s  =  to ++ repl remainder
        repl (c:cs)                                       =  c : repl cs
        repl []                                           =  []

-- |Заменить %1 на заданную строку
format msg s  =  replaceAll "%1" s msg

-- |Заменить %1..%9 на заданные строки
formatn msg s  =  go msg
  where go ('%':d:rest) | isDigit d = (s !! (digitToInt d-1)) ++ go rest
        go (x:rest)                 = x : go rest
        go ""                       = ""

-- |Заменить в строке `s` префикс `from` на `to`
replaceAtStart from to s =
  case startFrom from s of
    Just remainder  -> to ++ remainder
    Nothing         -> s

-- |Заменить в строке `s` суффикс `from` на `to`
replaceAtEnd from to s =
  case startFrom (reverse from) (reverse s) of
    Just remainder  -> reverse remainder ++ to
    Nothing         -> s

-- |Закодировать символы, запрещённые в URL
urlEncode = concatMap (\c -> if isReservedChar(ord c) then '%':encode16 [c] else [c])
  where
        isReservedChar x
            | x >= ord 'a' && x <= ord 'z' = False
            | x >= ord 'A' && x <= ord 'Z' = False
            | x >= ord '0' && x <= ord '9' = False
            | x <= 0x20 || x >= 0x7F = True
            | otherwise = x `elem` map ord [';','/','?',':','@','&'
                                           ,'=','+',',','$','{','}'
                                           ,'|','\\','^','[',']','`'
                                           ,'<','>','#','%', chr 34]

-- |Вернуть шестнадцатеричную запись строки символов с кодами <=255
encode16 (c:cs) | n<256 = [intToDigit(n `div` 16), intToDigit(n `mod` 16)] ++ encode16 cs
                             where n = ord c
encode16 "" = ""

-- |Декодировать шестнадцатеричную запись строки символов с кодами <=255
decode16 (c1:c2:cs) = chr(digitToInt c1 * 16 + digitToInt c2) : decode16 cs
decode16 ""         = ""

-- |Взять первых n элементов списка и добавить к ним more для индикации того, что что-то было опущено
takeSome n more s | (y>[])    = x ++ more
                  | otherwise = x
                  where  (x,y) = splitAt n s

-- |Выровнять строку влево/вправо, дополнив её до заданной ширины пробелами или чем-нибудь ещё
right_fill  c n s  =  s ++ replicate (n-length s) c
left_fill   c n s  =  replicate (n-length s) c ++ s
left_justify       =  right_fill ' '
right_justify      =  left_fill  ' '

-- Удалить пробелы в начале/конце строки или по обоим сторонам
trimLeft  = dropWhile (==' ')
trimRight = reverse.trimLeft.reverse
trim      = trimLeft.trimRight

-- |Перевести строку в нижний регистр
strLower = map toLower

-- |Сравнить две строки, игнорируя регистр
strLowerEq a b  =  strLower a == strLower b

-- |break начиная со второго элемента
break1 f (x:xs)  =  mapFst (x:) (break f xs)

-- |Возвратить значение по умолчанию вместо головы списка, если он пуст
head1 [] = defaultValue
head1 xs = head xs

-- Аналог tail, спокойно реагирующий на пустые списки
tail1 [] = []
tail1 xs = tail xs

-- Аналог init, спокойно реагирующий на пустые списки
init1 [] = []
init1 xs = init xs

-- Аналог last, спокойно реагирующий на пустые списки
last1 [] = defaultValue
last1 xs = last xs

-- |Map various parts of list
mapHead f []      =  []
mapHead f (x:xs)  =  f x : xs

mapTail f []      =  []
mapTail f (x:xs)  =  x : map f xs

mapInit f []      =  []
mapInit f xs      =  map f (init xs) : last xs

mapLast f []      =  []
mapLast f xs      =  init xs ++ [f (last xs)]

{-# NOINLINE replaceAll #-}
{-# NOINLINE replaceAtEnd #-}



---------------------------------------------------------------------------------------------------
---- Операции над списками ------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Sort list by function result (use Schwarznegian transform)
sortOn  f  =  map snd . sortOn' fst . map (keyval f)

-- |Sort list by function result (don't use Schwarznegian transform!)
sortOn' f  =  sortBy (map2cmp f)

-- |Group list by function result
groupOn f  =  groupBy (map2eq f)

-- |Sort and Group list by function result
sort_and_groupOn  f  =  groupOn f . sortOn  f
sort_and_groupOn' f  =  groupOn f . sortOn' f

-- |Сгруппировать все элементы (a.b) с одинаковым значением 'a'
groupFst :: (Ord a) =>  [(a,b)] -> [(a,[b])]
groupFst = map (\xs -> (fst (head xs), map snd xs)) . sort_and_groupOn fst

-- |Удаляет дубликаты из списка
removeDups = removeDupsOn id

-- |Оставляет только по одному элементу из каждой группы с одинаковым значением f
removeDupsOn f = map head . sort_and_groupOn f

-- |Проверить, что все последовательные значения в списке удовлетворяют заданному соотношению
isAll f []       = True
isAll f [x]      = True
isAll f (x:y:ys) = f x y  &&  isAll f (y:ys)

-- |Проверить, что хотя бы два каких-нибудь последовательных значения в списке удовлетворяют заданному соотношению
isAny f []       = False
isAny f [x]      = False
isAny f (x:y:ys) = f x y  ||  isAny f (y:ys)

-- |Check that list is sorted by given field/critery
isSortedOn f  =  isAll (<=) . map f

-- |Check that all elements in list are equal by given field/critery
isEqOn f      =  isAll (==) . map f

-- |Find maximum element by given comparison critery
maxOn f (x:xs) = go x xs
  where go x [] = x
        go x (y:ys) | f x > f y  =  go x ys
                    | otherwise  =  go y ys

-- |Merge two lists, sorted by `cmp`, in one sorted list
merge :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge cmp xs [] = xs
merge cmp [] ys = ys
merge cmp (x:xs) (y:ys)
 = case x `cmp` y of
        GT -> y : merge cmp (x:xs)   ys
        _  -> x : merge cmp    xs (y:ys)

-- |Разбить список на `numGroups` подсписков в соответствии со значением, возвращаемым `crit_f`
partitionList numGroups crit_f list =
  elems $ accumArray (flip (:)) [] (0, numGroups-1) (map (keyval crit_f) (reverse list))

-- partitionList numGroups crit_f list =
--   let xs = map (keyval crit_f) list
--       go 0 [] all = all
--       go n list prev = let (this, next) = partition (\(a,b) -> a==n-1) list
--                        in go (n-1) next (map snd this:prev)
--   in go numGroups xs []

-- |Разбить список на группы в соответствии с предикатами из списка `groups`:
--   splitList [(=='a'), (=='c')] 2 "cassa"  ->  ["aa","c","ss"]
--
splitList groups default_group filelist =
  let go [] filelist sorted  =  replaceAt default_group filelist (reverse sorted)
      go (group:groups) filelist sorted =
        let (found, notfound)  =  partition group filelist
        in go groups notfound (found:sorted)
  in go groups filelist []

-- |Найти номер первого предиката из списка `groups`, которому удовлетворяет значение `value`
findGroup groups default_group value  =  (findIndex ($ value) groups) `defaultVal` default_group

-- Utility functions for list operations
keyval  f x    =  (f x, x)                -- |Return pair containing computed key and original value
map2cmp f x y  =  (f x) `compare` (f y)   -- |Converts "key_func" to "compare_func"
map2eq  f x y  =  (f x) == (f y)          -- |Converts "key_func" to "eq_func"


-- |Рекурсивная обработка списка
recursive :: ([a]->(b,[a])) -> [a] -> [b]
recursive f list  =  list &&& (x:recursive f xs)   where (x,xs) = f list

-- |Разбить список на подсписки, длина которых определяется вызовом функции `len_f` на остатке списка
splitByLen :: ([a]->Int) -> [a] -> [[a]]
splitByLen len_f  =  recursive (\xs -> splitAt (len_f xs) xs)

-- |Эта функция получает список длин подсписков и разбивает `xs` в соответствии с ним
splitByLens (len:lens) list  =  (x:splitByLens lens xs)    where (x,xs) = splitAt len list
splitByLens []         []    =  []

-- |Возвращает длину начального сегмента списка, удовлетворяющего комбинированному условию,
-- например "groupLen (fiSize) (+) (<16*mb) files" возвращает длину начального сегмента списка,
-- содержащую файлы суммарным объёмом не более 16 мегабайт
groupLen mapper combinator tester  =  length . takeWhile tester . scanl1 combinator . map mapper

-- |Объединить результаты span и break: spanBreak isDigit "100a10b2c" = ("100a", "10b2c")
spanBreak crit xs  = let (s1,tail1) = span  crit xs
                         (s2,tail2) = break crit tail1
                     in (s1++s2, tail2)

-- |Разбить список на группы, заголовки которых - элементы, отвечающие критерию 'crit'
makeGroups              :: (a -> Bool) -> [a] -> [[a]]
makeGroups crit []      =  []
makeGroups crit (x:xs)  =  (x:ys) : makeGroups crit zs
                             where (ys,zs) = break crit xs

-- |Разбить список на группы, разделённые элементами, отвечающими критерию 'crit':
-- splitOn even [1,2,4,8,3,5,7] == [[1],[2],[4],[8],[3,5,7]]
splitOn crit []  =  []
splitOn crit xs  =  (not(null ys)  &&&  (ys :))
                    (not(null zs)  &&&  ([head zs] : splitOn crit (tail zs)))
                      where (ys,zs) = break crit xs

-- |Удалить в списке дубликаты по заданному критерию. O(n^2), зато сохраняет порядок элементов в списке
keepOnlyFirstOn f [] = []
keepOnlyFirstOn f (x:xs) = x : keepOnlyFirstOn f (filter (\a -> f x /= f a) xs)

-- |Оставить в списке только последний из дубликатов по заданному критерию
keepOnlyLastOn f = reverse . keepOnlyFirstOn f . reverse

-- |Удалить элементы с заданными номерами из списка
deleteElems = go 0
  where go n xs [] = xs  -- Удалять больше нечего
        go n (x:xs) iis@(i:is) | n<i  = x:go (n+1) xs iis  -- мы ещё не дошли до i-го элемента
                               | n==i =   go (n+1) xs is   -- дошли - удаляем!


{-# NOINLINE partitionList #-}
{-# NOINLINE splitList #-}


---------------------------------------------------------------------------------------------------
---- Операции с массивами -------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Превратить список в 0-based array
listArray0 list  =  listArray (0,length(list)-1) list

-- |Найти миним. и макс. индексы в списке пар и создать из них массив,
populateArray defaultValue castValue pairs =
  accumArray (\a b -> castValue b) defaultValue (minimum indexes, maximum indexes) pairs
  where indexes = map fst pairs


---------------------------------------------------------------------------------------------------
---- Операции с tuples ----------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- Операции над tuple/2
mapFst    f (a,b)  =  (f a,   b)
mapSnd    f (a,b)  =  (  a, f b)
mapFstSnd f (a,b)  =  (f a, f b)
map2      (f,g) a  =  (f a, g a)
mapFsts = map . mapFst
mapSnds = map . mapSnd
map2s   = map . map2

-- |Слить вторые элементы пар в списке и оставить один (общий) первый элемент
concatSnds xs = (fst (head xs), concatMap snd xs)

-- Операции над tuple/3
fst3 (a,_,_)    =  a
snd3 (_,a,_)    =  a
thd3 (_,_,a)    =  a
map3 (f,g,h) a  =  (f a, g a, h a)


---------------------------------------------------------------------------------------------------
---- Эмуляция обычных переменных ------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

infixl 0 =:, +=, -=, ++=, =::, .=, .<-, <<=, <=>

-- Simple variables
class Variable v a | v->a where
  new  :: a -> IO v
  val  :: v -> IO a
  (=:) :: v -> a -> IO ()
  (.=) :: v -> (a->a) -> IO ()
  (=::) :: v -> IO a -> IO ()
  (.<-) :: v -> (a->IO a) -> IO ()
  -- Default implementations
  a.=f = do x<-val a; a=:f x
  a=::b = (a=:) =<< b
  a.<-f = do x<-val a>>=f; a=:x

ref = newIORef
instance Variable (IORef a) a where
  new = newIORef
  val = readIORef
  a=:b = writeIORef a b
  a.=b = modifyIORef a b
  a.<-b = modifyIORefIO a b

mvar = newMVar
instance Variable (MVar a) a where
  new = newMVar
  val = readMVar
  a=:b = swapMVar a b >> return ()
  a.=b = modifyMVar_ a (return.b)
  a.<-b = modifyMVar_ a b

a+=b = a.=(\a->a+b)
a-=b = a.=(\a->a-b)
a++=b = a.=(\a->a++b)
a<=>b = do x <- val a; a =: b; return x
withRef init  =  with' (ref init) val


-- Accumulation lists
newtype AccList a = AccList [a]
newList   = ref$ AccList []
a<<=b     = a .= (\(AccList x) -> AccList$ b:x)
pushList  = (<<=)
listVal a = val a >>== (\(AccList x) -> reverse x)
withList  =  with' newList listVal


-- |Добавить значение в список, хранимый по ссылке IORef
addToIORef :: IORef [a] -> a -> IO ()
addToIORef var x  =  var .= (x:)

-- |Использовать значение, хранящееся по ссылке IORef, в процедуре,
-- и записать на его место новое значение, возвращённое этой процедурой
modifyIORefIO :: IORef a -> (a -> IO a) -> IO ()
modifyIORefIO var action = do
  readIORef var  >>=  action  >>=  writeIORef var

-- |Ещё одна полезная управляющая структура
with' init finish action  =  do a <- init;  action a;  finish a

-- |Выполнить операцию и возвратить её результат внутри обрамления init/finish операций
inside init finish action  =  do init;  x <- action;  finish; return x

-- |Выполнить "add key" c кешированием результатов
lookupMVarCache mvar add key = do
  modifyMVar mvar $ \assocs -> do
    case (lookup key assocs) of
      Just value -> return (assocs, value)
      Nothing    -> do value <- add key
                       return ((key,value):assocs, value)


-- JIT-переменные инициализируются только в момент их первого использовани
newJIT init        = ref (Left init)
delJIT a    finish = whenRightM_ (val a) finish
valJIT a           = do x <- val a
                        case x of
                          Left init -> do x<-init; a=:Right x; return x
                          Right x   -> return x

withJIT init finish action = do a <- newJIT init;  action a  `finally`  delJIT a finish


---------------------------------------------------------------------------------------------------
---- Ссылочная арифметика и операций над целыми ---------------------------------------------------
---------------------------------------------------------------------------------------------------

infixl 6 +:, -:
ptr+:n   = ptr `plusPtr` (fromIntegral n)
ptr-:buf = fromIntegral  (ptr `minusPtr` buf)
copyBytesI dst src len  =  copyBytes dst src (i len)
minI a b                =  i$ min (i a) (i b)
maxI a b                =  i$ max (i a) (i b)
clipToMaxInt            =  i. min (i (maxBound::Int))
atLeast                 =  max
i                       =  fromIntegral
clipTo low high         =  min high . max low
divRoundUp   x chunk    = ((x-1) `div` i chunk) + 1
roundUp      x chunk    = divRoundUp x chunk * i chunk
divRoundDown x chunk    = x `div` i chunk
roundDown    x chunk    = divRoundDown x chunk * i chunk
roundTo      x chunk    = i (((((toInteger(x)*2) `divRoundDown` chunk)+1) `divRoundDown` 2) * i chunk) `asTypeOf` x


---------------------------------------------------------------------------------------------------
---- Распределение памяти в циклическом буфере ----------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Распределение памяти в циклическом буфере с выравниванием выделяемых блоков
--   heapsize     - размер буфера
--   aBUFFER_SIZE - максимальный размер распределяемого блока
--   aALIGN       - все выделяемые блоки выравниваются на границу, кратную этому числу
--   returnBlock  - процедура получения буфера, освобождённого потребителем. Вызывается,
--                    когда памяти становится недостаточно для удовлетворения очередного запроса
--
allocator heapsize aBUFFER_SIZE aALIGN returnBlock = do
  let aHEAP_START = 0          -- начало буфера, должно быть = 0
      aHEAP_END   = heapsize   -- конец буфера

  start <- ref aHEAP_START     -- указатель начала свободного места в буфере
  end   <- ref aHEAP_END       -- указатель конца свободного места
                               -- если эти указатели равны, то свободного места нет
#if 0
  let debug = putStr         -- отладочная печать

  let printStats s = do      -- Напечатать состояние буфера при отладке
        astart <- val start
        aend <- val end
        debug$ left_justify 48 s++"STATE start:"++show astart++", end:"++show aend++", avail:"++show ((aend-astart) `mod` aHEAP_END)++"\n"

  debug "\n"
#else
  let debug      = return
      printStats = return
#endif

  -- Выравнять значение на ближайшую величину, кратную aALIGN
  let align n  =  (((n-1) `div` aALIGN) + 1) * aALIGN

  -- Возвращает адрес блока: >=n, выровненного на aALIGN и имеющего как минимум aBUFFER_SIZE байт до конца буфера
  let nextAvail n = if (aHEAP_END-aligned<aBUFFER_SIZE)
                      then aHEAP_END
                      else aligned
                    where aligned = align n

  -- Возвратить количество свободной памяти в буфере
  let available = do
        astart <- val start
        aend   <- val end
        if (astart<=aend) then
           return (aend-astart)
         else if (astart<aHEAP_END) then
           return (aHEAP_END-astart)
         else do
           -- Перевести указатель начала свободной памяти на начало буфера
           start =: aHEAP_START
           debug "===================================\n"
           printStats ""
           available

  -- Дождаться освобождения блока памяти и отметить его освобождение
  let waitReleasingMemory = do
        (addr,size) <- returnBlock
        astart <- val start
        aend   <- val end
        unless (addr == aend || (addr==aHEAP_START && (aHEAP_END-aend<aBUFFER_SIZE)))$  fail "addToAvail!"
        let new_end = nextAvail(addr+size)
        if new_end == astart
          then do start=:aHEAP_START; end=:aHEAP_END -- now all memory is free
          else end =: new_end
        printStats$ "*** returned buf:"++show addr++" size:"++show size++"   "

  -- Получить очередной блок размера aBUFFER_SIZE. Если свободных блоков нет - дождатьс
  --   возвращения необходимого количества выделенной прежде памяти
  let getBlock = do
        avail <- available
        if (avail >= aBUFFER_SIZE) then do
           block <- val start
           start =: error "Block not shrinked"
           return block
         else do
           waitReleasingMemory
           getBlock

  -- Укоротить выделенный блок до размера `size`. Должна быть обязательно вызвана после getBlock
  let shrinkBlock block size = do
        astart <- val start
        --unless (astart == block)$      fail "Tryed to shrink another block"
        unless (size <= aBUFFER_SIZE)$  fail "Growing instead of shrinking :)"
        start =: nextAvail(block+size)
        printStats$ "getBlock buf:"++show block++", size: "++show aBUFFER_SIZE++" --> "++show size++"    "

  -- Возвратить интерфейс к использованию циклического буфера
  return (getBlock, shrinkBlock)


-- |Циклический аллокатор, использующий блок памяти `heap`.
-- Преобразует функции, с которыми работает функция `allocator`
memoryAllocator heap size chunksize align returnBlock = do
  let returnBlock2            =  do (buf,len) <- returnBlock; return (buf-:heap, len)
  (getBlock2, shrinkBlock2)  <-  allocator size chunksize align returnBlock2
  let getBlock                =  do block <- getBlock2; return (heap+:block)
      shrinkBlock buf len     =  do shrinkBlock2 (buf-:heap) len
  return (getBlock, shrinkBlock)


---------------------------------------------------------------------------------------------------
---- Поддержка регулярных выражений.                                                           ----
---- todo: #define FULL_REGEXP включает использование расширенных рег. выражений: r[0-9][0-9]  ----
---------------------------------------------------------------------------------------------------

-- |Скомпилированное представление регулярного выражения                            ПРИМЕР
data RegExpr = RE_End                     -- конец маски                            ""
             | RE_Anything                -- любая строка                           "*"
             | RE_AnyStr  RegExpr         -- '*', после которой будут ещё '*'       '*':"bc*"
             | RE_FromEnd RegExpr         -- проверить соответствие RE конца строки '*':"bc"
             | RE_AnyChar RegExpr         -- любой символ, затем RE                 '?':"bc"
             | RE_Char    Char RegExpr    -- заданный символ, затем RE              'a':"bc"

-- |Проверить, что строка содержит один из символов,
-- имеющих специальное значение в регулярных выражениях
is_wildcard s  =  s `contains_one_of` "?*"

-- |Скомпилировать текстовое представление регулярного выражения в структуру RegExpr
compile_RE s  =  case s of
  ""                         -> RE_End
  "*"                        -> RE_Anything
  '*':cs | cs `contains` '*' -> RE_AnyStr   (compile_RE  cs)
         | otherwise         -> RE_FromEnd  (compile_RE$ reverse s)
  '?':cs                     -> RE_AnyChar  (compile_RE  cs)
  c  :cs                     -> RE_Char   c (compile_RE  cs)

-- |Проверить соответствие строки скомпилированному регулярному выражению
match_RE r = case r of
  RE_End        -> null
  RE_Anything   -> const True
  RE_AnyStr   r -> let re = match_RE r in \s -> any re (tails s)
  RE_FromEnd  r -> let re = match_RE r in re . reverse
  RE_AnyChar  r -> let re = match_RE r in \s -> case s of
                     ""   -> False
                     _:xs -> re xs
  RE_Char   c r -> let re = match_RE r in \s -> case s of
                     ""   -> False
                     x:xs -> x==c && re xs

-- |Проверить соответствие строки `s` регулярному выражению `re`
match re {-s-}  =  match_RE (compile_RE re) {-s-}

-- Perl-like names for matching routines
infix 4 ~=, !~
(~=)    = flip match
a !~ b  = not (a~=b)
