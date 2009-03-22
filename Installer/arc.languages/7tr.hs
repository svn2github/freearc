{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
import Data.Char
import Data.List
import Data.Maybe
import System.Environment

-- "7tr arc.russian.txt pl.txt" translates 7-zip language file pl.txt
--   into the FreeArc language file arc.polish.txt
-- "7tr arc.russian.txt pl.txt dir\arc.polish.txt" translates 7-zip language file pl.txt
--   and old FreeArc language file dir\arc.polish.txt into the new FreeArc language file arc.polish.txt
main = do (my:szip:old) <- getArgs
          dict    <-  readFile szip >>= return.makeDict
          oldLang <-  if null old then return []
                                  else readFile (head old) >>= return.makeOldLang
          let lang = dict .$lookup "00000000" .$fromMaybe "_New" .$replace ' ' '_' .$map toLower
              out  = "arc."++lang++".txt"
          readFile my >>= writeFile out.unlines.map (makeLine dict oldLang).lines

-- |Сообразить словарь из файла национализации 7-zip
makeDict x = ("copyright", c): xs
  where l  = x .$ lines
        xs = l .$ map (split2 '=')
               .$ map (\(key,str) -> (trim key, drop 1 $dropEnd 1 $trim str))
        c = l .$drop 2 .$takeWhile ((";"==).take 1) .$map (dropWhile isSpace.drop 1) .$filter (not.null) .$joinWith "\\n"

-- |Сообразить словарь из старого файла национализации FreeArc
makeOldLang x = x .$ lines
                  .$ filter (\s -> length s > 4  &&  s `contains` '=')
                  .$ filter (all isDigit.take 4)
                  .$ map    (split2 '=')
                  .$ filter (("??"/=).snd)
                  .$ map    (\(a,b) -> (take 4 a, b))

-- |Преобразовать строку x в язык, описанный в dict
makeLine dict oldLang x =
  case x.$ split2 '=' of
    (x,xs) | (n,eng) <- split2 ' ' x,
             length n==4, all isDigit n
       -> x++"="++(n `lookup` oldLang `defaultVal`
                   makeSubst dict n .$replaceAll "{0}" "%1" .$replaceAll "{1}" "%2")
    _  -> x

makeSubst dict n = case n of
  "0000" -> d "00000000"++" ("++d "00000001"++")"
  "0159" -> "Automatic translation from 7-Zip language file.\\nPlease edit it to finish translation.\\nOriginal copyrights:\\n"++d "copyright"
  "0022" ->(d "02000301".$replaceAll "{0}" "{1}")++", "++d "02000982"
  "0023" ->(d "02000302".$replaceAll "{0}" "{1}")++", "++d "02000982"
  "0020" -> d "03020215".$replaceAll "{0}" "{1}"
  "0019" -> d "03020215".$replaceAll "{0}" "{1}"
  "0114" -> d "02000D0B"++" %1"
  "0115" -> d "02000D0E"++" %2, "++d "02000C04"++" %1"
  "0116" -> d "02000D0F"++" %2, "++d "02000C04"++" %1"
  "0024" -> d "02000800"++" %3"
  "0152" -> d "02000109"++" %3"
  "0172" ->(d "03010302".$tryToSkipAtEnd ":" .$replaceAll "7-Zip" "FreeArc" .$replaceAll "7-zip" "FreeArc") ++ " .arc"
  "0165" -> "%1\\n"++(d "02000982".$replaceAll "{0}" "%2")++"\\n"++d "02000983"++" %3"
  _      -> d (trans .$lookup n .$fromMaybe ":(")
  where d n = dict .$lookup n .$fromMaybe "??"

-- |Трансляция номеров сообщений из FreeArc в 7-zip
trans = map (split2 ' ')
        ["0050 03000102"
        ,"0066 03000103"
        ,"0259 03000105"
        ,"0260 02000D07"
        ,"0261 03000106"

        ,"0262 02000103"
        ,"0263 03000330"
        ,"0008 03000333"
        ,"0009 03000334"
        ,"0037 03020250"
        ,"0038 03020251"
        ,"0264 03000332"
        ,"0039 03000440"
        ,"0036 03000260"

        ,"0030 03020400"
        ,"0040 02000108"
        ,"0033 03000233"
        ,"0034 03020402"
        ,"0044 0200010A"
        ,"0086 03020423"
        ,"0035 03020401"
        ,"0045 02000106"
        ,"0064 03010400"

        ,"0268 03020290"
        ,"0272 02000D10"


        ,"0015 02000204"
        ,"0016 02000207"
        ,"0017 0200020C"
        ,"0018 02000140"

        ,"0134 02000108"
        ,"0135 02000108"
        ,"0136 02000108"

        ,"0107 02000D0B"
        ,"0129 02000D04"
        ,"0108 02000D83"
        ,"0110 02000D82"
        ,"0111 02000D84"
        ,"0112 02000D85"
        ,"0137 02000D13"
        ,"0138 02000322"
        ,"0139 02000320"

        ,"0119 02000D10"
        ,"0120 02000D0A"
        ,"0121 02000D11"
        ,"0131 02000D01"

        ,"0160 03020213"
        ,"0161 03020213"

        ,"0005 02000820"
        ,"0001 02000821"
        ,"0002 02000822"
        ,"0051 02000823"
        ,"0004 02000801"
        ,"0021 02000881"

        ,"0088 02000320"
        ,"0089 02000C03"
        ,"0090 02000323"
        ,"0091 02000C06"
        ,"0099 02000D0E"
        ,"0100 02000D0F"
        ,"0105 02000D0C"
        ,"0156 02000D0A"
        ,"0097 02000D11"
        ,"0098 03020291"

        ,"0067 03010400"
        ,"0068 01000401"
        ,"0069 03000221"
        ,"0292 03000220"

        ,"0079 02000705"
        ,"0080 02000709"
        ,"0081 02000711"
        ,"0362 02000702"
        ,"0364 02000713"

        ,"0052 02000C10"
        ,"0053 02000C12"
        ,"0054 02000C13"

        ,"0056 02000320"
        ,"0058 02000C05"
        ,"0059 02000C03"
        ,"0252 02000323"
        ,"0060 02000C06"
        ,"0061 02000C04"
        ,"0062 02000C01"

        ,"0078 02000900"
        ,"0162 02000901"
        ,"0163 02000902"
        ,"0164 02000903"
        ,"0082 02000707"
        ,"0083 0200070B"

        ,"0076 02000B00"
        ,"0077 02000B00"
        ,"0074 02000B01"
        ,"0075 02000B03"

        ,"0173 02000321"
        ,"0184 02000D10"
        ,"0183 03080002"
        ,"0106 03080002"
        ,"0186 03020291"
        ,"0227 02000D08"
        ,"0199 03020290"
        ,"0221 02000830"

        ,"0194 02000D02"
        ,"0195 02000DA1"
        ,"0196 02000DA2"
        ,"0197 02000DA3"
        ,"0198 02000DA4"
        ]


---------------------------------------------------------------------------------------------------
---- Операции над строками ------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Соединить список строк в единый текст с разделителем: "one, two, three"
joinWith :: [a] -> [[a]] -> [a]
joinWith x  =  concat . intersperse x

-- |Разбить строку на две подстроки, разделённые заданным символом
split2 :: (Eq a) => a -> [a] -> ([a],[a])
split2 c s  =  (chunk, drop 1 rest)
  where (chunk, rest) = break (==c) s

contains a b = elem b a

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



-- |Заменить Nothing на значение по умолчанию
defaultVal = flip fromMaybe

infixl 9  .$
a.$b         =  b a                -- вариант $ с перевёрнутым порядком аргументов

