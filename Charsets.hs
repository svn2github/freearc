{-# OPTIONS_GHC -cpp #-}
----------------------------------------------------------------------------------------------------
---- Поддержка различных кодировок и локализации интерфейса программы                           ----
----------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- |
-- Module      :  Charsets
-- Copyright   :  (c) Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
-- License     :  Public domain
--
-- Maintainer  :  Bulat.Ziganshin@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-----------------------------------------------------------------------------

module Charsets where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Array
import Data.Char
import Data.IORef
import Data.List
import Foreign
import Foreign.C
import Foreign.Marshal.Alloc
import System.Posix.Internals
import System.Posix.Types
import System.IO
import System.IO.Error hiding (catch)
import System.IO.Unsafe
import System.Locale
import System.Time
import System.Process
import System.Directory
import System.Environment
#if defined(FREEARC_WIN)
import System.Win32
#endif

import Utils
import Files


---------------------------------------------------------------------------------------------------
---- Глобальные настройки перекодировки для использования в глубоко вложенных функциях ------------
---------------------------------------------------------------------------------------------------

-- |Translate string from internal to terminal encoding
str2terminal'     = unsafePerformIO$ newIORef$ unParseData (domainTranslation aCHARSET_DEFAULTS 't')
str2terminal s    = val str2terminal' >>== ($s)
-- |Translate string from terminal to internal encoding
terminal2str'     = unsafePerformIO$ newIORef$ parseData (domainTranslation aCHARSET_DEFAULTS 't')
terminal2str s    = val terminal2str' >>== ($s)
-- |Translate string from cmdline to internal encoding
cmdline2str'      = unsafePerformIO$ newIORef$ parseData (domainTranslation aCHARSET_DEFAULTS 'p')
cmdline2str s     = val cmdline2str' >>== ($s)
-- |Translate string from internal to logfile encoding
str2logfile'      = unsafePerformIO$ newIORef$ unParseData (domainTranslation aCHARSET_DEFAULTS 'i')
str2logfile s     = val str2logfile' >>== ($s)

-- |Операция, устанавливающая глобальные настройки перекодировки для использования в глубоко вложенных функциях
setGlobalCharsets charsets = do
  str2filesystem' =: unParseData (domainTranslation charsets 'f')
  str2terminal'   =: unParseData (domainTranslation charsets 't')
  str2logfile'    =: unParseData (domainTranslation charsets 'i')
  terminal2str'   =: parseData (domainTranslation charsets 't')
  filesystem2str' =: parseData (domainTranslation charsets 'f')
  cmdline2str'    =: parseData (domainTranslation charsets 'p')


-- Получение командной строки
#ifdef FREEARC_WIN
myGetArgs = do
   alloca $ \p_argc -> do
   p_argv_w <- commandLineToArgvW getCommandLineW p_argc
   argc     <- peek p_argc
   argv_w   <- peekArray (i argc) p_argv_w
   mapM peekTString argv_w >>== tail

foreign import stdcall unsafe "windows.h GetCommandLineW"
  getCommandLineW :: LPTSTR

foreign import stdcall unsafe "windows.h CommandLineToArgvW"
  commandLineToArgvW :: LPCWSTR -> Ptr CInt -> IO (Ptr LPWSTR)

#else
myGetArgs = getArgs >>= mapM cmdline2str
#endif


---------------------------------------------------------------------------------------------------
---- Парсер опции командной строки -sc/--charset --------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Тип функции, транслирующей входных данные заданного типа в Unicode
type ParseDataFunc  =  Domain -> String -> String

-- |Обработать список опций --charset/-sc, возвратив таблицу кодировок
-- и процедуры чтения/записи файлов с её учётом
parse_charset_option optionsList = (charsets
                                   ,parseFile   . domainTranslation charsets
                                   ,unParseFile . domainTranslation charsets
                                   ,parseData   . domainTranslation charsets
                                   ,unParseData . domainTranslation charsets)
  where
    -- Таблица кодировок
    charsets = foldl f aCHARSET_DEFAULTS optionsList
    -- Функция обработки опций --charset
    f value "--"      =  aCHARSET_DEFAULTS      -- -sc-- означает восстановить значения по умолчанию
    f value ('s':cs)  =  _7zToRAR value "l" cs  -- -scs... устанавливает кодировку для листфайлов
    f value ('l':cs)  =  _7zToRAR value "l" cs  -- -scl... does the same
    f value ('c':cs)  =  _7zToRAR value "c" cs  -- -scs... устанавливает кодировку для комментфайлов
    f value ('f':cs)  =  _7zToRAR value "f" cs  -- -scf... устанавливает кодировку для файловой системы
    f value ('d':cs)  =  _7zToRAR value "d" cs  -- -scd... устанавливает кодировку для каталога архива
    f value ('t':cs)  =  _7zToRAR value "t" cs  -- -sct... устанавливает кодировку для терминала (консоли)
    f value ('p':cs)  =  _7zToRAR value "p" cs  -- -scp... устанавливает кодировку для параметров ком. строки
    f value ('i':cs)  =  _7zToRAR value "i" cs  -- -sci... устанавливает кодировку для ini-файлов (arc.ini/arc.groups)
    f value (x:cs)    =  foldl Utils.update value [(c,x) | c<-cs|||"cl"]  -- установить в `x` элементы списка, перечисленные в cs (по умолчанию 'c' и 'l')
    -- Вспомогательные функции, преобразующие 7zip-овский формат опций в RAR'овский
    _7zToRAR value typ cs  =  f value (g (strLower cs):typ)
    g "utf-8"  = '8';  g "win"  = 'a'
    g "utf8"   = '8';  g "ansi" = 'a'
    g "utf-16" = 'u';  g "dos"  = 'o'
    g "utf16"  = 'u';  g "oem"  = 'o'


-- Процедура чтения файла, транслирующая его кодировку и разбивающая на отдельные строки
parseFile encoding file  =  fileGetBinary file >>== parseData encoding >>== linesCRLF

-- Процедура трансляции входных данных из encoding в Unicode
parseData encoding  =  aTRANSLATE_INPUT (charsetTranslation encoding)

-- Процедура записи файла, транслирующая данные в кодировку encoding
unParseFile encoding file  =  filePutBinary file . unParseData encoding

-- Процедура трансляции выходных данных из encoding из Unicode
unParseData encoding  =  aTRANSLATE_OUTPUT (charsetTranslation encoding)

-- |Разбиение на строки файла, ипользующего любое представление конца строки (CR, LF, CR+LF)
linesCRLF = recursive oneline  -- oneline "abc\n..." = ("abc","...")
              where oneline ('\r':'\n':s)  =  ("",'\xFEFF':s)
                    oneline ('\r':s)       =  ("",'\xFEFF':s)
                    oneline ('\n':s)       =  ("",'\xFEFF':s)
                    oneline ('\xFEFF':s)   =  oneline s
                    oneline (c:s)          =  (c:s0,s1)  where (s0,s1) = oneline s
                    oneline ""             =  ("","")


-- Будем считать, что все GUI конфиг-файлы хранятся в UTF-8
readConfigFile          = parseFile   '8'
saveConfigFile   file   = unParseFile '8' file . joinWith "\n"
modifyConfigFile file f = handle (\e->return []) (readConfigFile file) >>== f >>= saveConfigFile file


---------------------------------------------------------------------------------------------------
---- Поддержка различных кодировок для ввода/вывода -----------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Возвращает charset, используемый в domainCharsets для данных типа domain
domainTranslation domainCharsets domain =
  lookup domain domainCharsets `defaultVal` error ("Unknown charset domain "++quote [domain])

-- |Трансляция данных, заданных в кодировке charset
charsetTranslation charset =
  lookup charset aCHARSETS `defaultVal` error ("Unknown charset "++quote [charset])

-- |Трансляция данных из области domain (листфайлы, конфигфайлы, коммент-файлы...),
-- используя charset, заданный для неё в domainСharsets
translation domainCharsets domain =
  charsetTranslation $ domainTranslation domainCharsets domain

-- Типы, используемые для представления domain и charset
type Domain  = Char
type Charset = Char

-- |Each charset is represented by pair of functions: input translation (byte sequence into Unicode String) and output translation
data TRANSLATION = TRANSLATION {aTRANSLATE_INPUT, aTRANSLATE_OUTPUT :: String->String}

-- |Character sets and functions to translate texts from/to these charsets
aCHARSETS = [ ('0', TRANSLATION id               id)
            , ('8', TRANSLATION utf8_to_unicode  unicode2utf8)
            , ('u', TRANSLATION utf16_to_unicode unicode2utf16)
            ] ++ aLOCAL_CHARSETS


#ifdef FREEARC_UNIX

aLOCAL_CHARSETS = []

-- |Default charsets for various domains
aCHARSET_DEFAULTS = [ ('f','8')  -- filenames in filesystem: UTF-8
                    , ('d','8')  -- filenames in archive directory: UTF-8
                    , ('l','8')  -- filelists: UTF-8
                    , ('c','8')  -- comment files: UTF-8
                    , ('t','8')  -- terminal: UTF-8
                    , ('p','8')  -- program arguments: UTF-8
                    , ('i','8')  -- ini/group files: UTF-8
                    ]

#else

-- |Windows-specific charsets
aLOCAL_CHARSETS = [ ('o', TRANSLATION oem2unicode  unicode2oem)
                  , ('a', TRANSLATION ansi2unicode unicode2ansi)
                  ]

-- |Default charsets for various domains
aCHARSET_DEFAULTS = [ ('f','u')  -- filenames in filesystem: UTF-16
                    , ('d','8')  -- filenames in archive directory: UTF-8
                    , ('l','o')  -- filelists: OEM
                    , ('c','o')  -- comment files: OEM
                    , ('t','o')  -- terminal: OEM
                    , ('p','a')  -- program arguments: ANSI
                    , ('i','o')  -- ini/group files: OEM
                    ]

---------------------------------------------------------------------------------------------------
---- Windows-specific codecs ----------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Преобразовать виндовые коды символов \r и \n в человеческий вид
iHateWindows = replace (chr 9834) '\r' . replace (chr 9689) '\n'

-- |Translate string from Unicode to OEM encoding
unicode2oem s =
  if all isAscii s
    then s
    else unsafePerformIO $ do
           withCWStringLen s $ \(wstr,len) -> do
             allocaBytes len $ \cstr -> do
               c_WideToOemBuff wstr cstr (i len)
               peekCStringLen (cstr,len)

-- |Translate string from OEM encoding to Unicode
oem2unicode s =
  if all isAscii s
    then s
    else iHateWindows $
         unsafePerformIO $ do
           withCStringLen s $ \(cstr,len) -> do
             allocaBytes (len*2) $ \wstr -> do
               c_OemToWideBuff cstr wstr (i len)
               peekCWStringLen (wstr,len)

-- |Translate string from Unicode to ANSI encoding
unicode2ansi s =
  if all isAscii s
    then s
    else unsafePerformIO $ do
           withCWStringLen s $ \(wstr,len) -> do
             allocaBytes len $ \cstr -> do
               c_WideToOemBuff wstr cstr (i len)
               c_OemToAnsiBuff cstr cstr (i len)
               peekCStringLen (cstr,len)

-- |Translate string from ANSI encoding to Unicode
ansi2unicode s =
  if all isAscii s
    then s
    else iHateWindows $
         unsafePerformIO $ do
           withCStringLen s $ \(cstr,len) -> do
             allocaBytes (len*2) $ \wstr -> do
               c_AnsiToOemBuff cstr cstr (i len)
               c_OemToWideBuff cstr wstr (i len)
               peekCWStringLen (wstr,len)

foreign import stdcall unsafe "winuser.h CharToOemBuffW"
  c_WideToOemBuff :: CWString -> CString -> DWORD -> IO Bool

foreign import stdcall unsafe "winuser.h OemToCharBuffW"
  c_OemToWideBuff :: CString -> CWString -> DWORD -> IO Bool

foreign import stdcall unsafe "winuser.h OemToCharBuffA"
  c_OemToAnsiBuff :: CString -> CString -> DWORD -> IO Bool

foreign import stdcall unsafe "winuser.h CharToOemBuffA"
  c_AnsiToOemBuff :: CString -> CString -> DWORD -> IO Bool

#endif


---------------------------------------------------------------------------------------------------
---- UTF-8, UTF-16 codecs -------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Translate string from UTF-16 encoding to Unicode
utf16_to_unicode = tryToSkip [chr 0xFEFF] . map chr . fromUTF16 . map ord
 where
  fromUTF16 (c1:c2:c3:c4:wcs)
    | 0xd8<=c2 && c2<=0xdb  &&  0xdc<=c4 && c4<=0xdf =
      ((c1+c2*256 - 0xd800)*0x400 + (c3+c4*256 - 0xdc00) + 0x10000) : fromUTF16 wcs
  fromUTF16 (c1:c2:wcs) = c1+c2*256 : fromUTF16 wcs
  fromUTF16 [] = []

-- |Translate string from Unicode to UTF-16 encoding
unicode2utf16 = map chr . foldr utf16Char [] . map ord
 where
  utf16Char c wcs
    | c < 0x10000 = c `mod` 256 : c `div` 256 : wcs
    | otherwise   = let c' = c - 0x10000 in
                    ((c' `div` 0x400) .&. 0xFF) :
                    (c' `div` 0x40000 + 0xd8) :
                    (c' .&. 0xFF) :
                    (((c' `mod` 0x400) `div` 256) + 0xdc) : wcs

-- |Translate string from UTF-8 encoding to Unicode
utf8_to_unicode s =
  if all isAscii s
    then s
    else (tryToSkip [chr 0xFEFF] . fromUTF' . map ord) s  where
            fromUTF' [] = []
            fromUTF' (all@(x:xs))
                | x<=0x7F = chr x : fromUTF' xs
                | x<=0xBF = err
                | x<=0xDF = twoBytes all
                | x<=0xEF = threeBytes all
                | x<=0xFF = fourBytes all
                | otherwise = err
            twoBytes (x1:x2:xs) = chr  ((((x1 .&. 0x1F) `shift` 6) .|.
                                          (x2 .&. 0x3F))):fromUTF' xs
            twoBytes _ = error "fromUTF: illegal two byte sequence"

            threeBytes (x1:x2:x3:xs) = chr ((((x1 .&. 0x0F) `shift` 12) .|.
                                             ((x2 .&. 0x3F) `shift` 6) .|.
                                              (x3 .&. 0x3F))):fromUTF' xs
            threeBytes _ = error "fromUTF: illegal three byte sequence"

            fourBytes (x1:x2:x3:x4:xs) = chr ((((x1 .&. 0x0F) `shift` 18) .|.
                                               ((x2 .&. 0x3F) `shift` 12) .|.
                                               ((x3 .&. 0x3F) `shift` 6) .|.
                                                (x4 .&. 0x3F))):fromUTF' xs
            fourBytes _ = error "fromUTF: illegal four byte sequence"

            err = error "fromUTF: illegal UTF-8 character"

-- |Translate string from Unicode to UTF-8 encoding
unicode2utf8 s =
  if all isAscii s
    then s
    else go s
      where go [] = []
            go (x:xs) | ord x<=0x007f = chr (ord x) : go xs
                      | ord x<=0x07ff = chr (0xC0 .|. ((ord x `shiftR` 6) .&. 0x1F)):
                                        chr (0x80 .|. ( ord x .&. 0x3F)):
                                        go xs
                      | ord x<=0xffff = chr (0xE0 .|. ((ord x `shiftR` 12) .&. 0x0F)):
                                        chr (0x80 .|. ((ord x `shiftR`  6) .&. 0x3F)):
                                        chr (0x80 .|. ( ord x .&. 0x3F)):
                                        go xs
                      | otherwise     = chr (0xF0 .|. ( ord x `shiftR` 18)) :
                                        chr (0x80 .|. ((ord x `shiftR` 12) .&. 0x3F)) :
                                        chr (0x80 .|. ((ord x `shiftR`  6) .&. 0x3F)) :
                                        chr (0x80 .|. ( ord x .&. 0x3F)) :
                                        go xs


---------------------------------------------------------------------------------------------------
---- Internalization ------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

{-# NOINLINE locale #-}
-- |Локализация: отображение индекса в локализованную строку
locale :: IORef (Array Int (Maybe String))
locale = unsafePerformIO $ ref$ array (0,-1) []

{-# NOINLINE setLocale #-}
-- |Установить локализацию по файлу
setLocale "--"       = return ()
setLocale localeFile = do
  localeInfo <- parseLocaleFile localeFile
  locale =: localeInfo

-- |Переводит строку/список строк на местный язык
i18ns = mapM i18n
i18n  = i18n' .>>== fst
i18n' = i18n_general (val locale)

{-# NOINLINE i18fmt #-}
-- |Отформатировать список строк, используя первую как требущий локализации шаблон,
-- а остальные - как его аргументы
i18fmt (x:xs)  =  i18n x >>== (`formatn` xs)


{-# NOINLINE parseLocaleFile #-}
-- |Прочитать из файла список строк локализации
parseLocaleFile localeFile = do
  -- Прочитаем файл локализации или возвратим пустую болванку
  localeInfo <- readConfigFile localeFile `catch` \e -> return ["0000=English"]
  -- Отбираем строки, начинающиеся на "dddd", и создаём из них массив: dddd -> текст после знака '='
  -- Если текст после '=' заключён в двойные кавычки - избавимся от них
  -- Символы '&' заменяются на '_' (различие в акселераторах 7-zip и Gtk)
  -- \" заменяется на просто ", запись "\\n" на сам символ \n
  return$ localeInfo .$ filter   (\s -> length s > 4  &&  s `contains` '=')
                     .$ filter   (all isDigit.take 4)
                     .$ map      (split2 '=')
                     .$ deleteIf (("??"==).snd)
                     .$ mapFsts  (readInt.take 4)
                     .$ mapSnds  (\s -> s.$ (s.$match "\"*\"" &&& (reverse.drop 1.reverse.drop 1)))
                     .$ mapSnds  (replace '&' '_'. replaceAll "\\\"" "\"". replaceAll "\\n" "\n")
                     .$ populateArray Nothing Just

{-# NOINLINE i18n_general #-}
-- |Возвращает локализованный текст надписи и её всплывающую подсказку
i18n_general getLocale text = do
  -- Если текст содержит в начале "dddd ", то вернём вместо него строку локализации за номером dddd
  -- Если такой строки не найдено - возвратим переданный текст за вычетом "dddd "
  -- Кроме того, тексты вида "  *  " локализуются в аналогичный вид
  case splitAt 4 text of
    (d,' ':engText) | all isDigit d -> do
         let f = (engText.$match "  *  ")  &&&  (("  "++).(++"  "))
         arr <- getLocale
         let n = readInt d
             g i def = if i.$inRange (bounds arr)
                         then fmap f (arr!i) `defaultVal` def
                         else def
         return (g n engText, g (n+1000) "")
    _ -> return (text, "")

