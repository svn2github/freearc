{-# OPTIONS_GHC -cpp #-}
----------------------------------------------------------------------------------------------------
---- Операции с именами файлов, манипуляции с файлами на диске, ввод/вывод.                     ----
----------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- |
-- Module      :  Files
-- Copyright   :  (c) Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
-- License     :  Public domain
--
-- Maintainer  :  Bulat.Ziganshin@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-----------------------------------------------------------------------------

module Files (module Files, module FilePath) where

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

import Utils
import FilePath
#if defined(FREEARC_WIN)
import Win32Files
import System.Win32
#else
import System.Posix.Files hiding (fileExist)
#endif

-- |Размер одного буфера, используемый в различных операциях
aBUFFER_SIZE = 64*kb

-- |Количество байт, которые должны читаться/записываться за один раз в быстрых методах и при распаковке асимметричных алгоритмов
aLARGE_BUFFER_SIZE = 256*kb

-- |Количество байт, которые должны читаться/записываться за один раз в очень быстрых методах (storing, tornado и тому подобное)
-- Этот объём минимизирует потери на disk seek operations - при условии, что одновременно не происходит в/в в другом потоке ;)
aHUGE_BUFFER_SIZE = 8*mb


----------------------------------------------------------------------------------------------------
---- Filename manipulations ------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |True, если file находится в каталоге `dir`, одном из его подкаталогов, или совпадает с ним
dir `isParentDirOf` file =
  case (startFrom dir file) of
    Just ""    -> True
    Just (x:_) -> isPathSeparator x
    Nothing    -> False

-- |Имя файла за минусом каталога dir
file `dropParentDir` dir =
  case (startFrom dir file) of
    Just ""    -> ""
    Just (x:xs) | isPathSeparator x -> xs
    _          -> error "Utils::dropParentDir: dir isn't prefix of file"


#if defined(FREEARC_WIN)
-- |Для case-insensitive файловых систем
filenameLower = strLower
#else
-- |Для case-sensitive файловых систем
filenameLower = id
#endif

-- |Return False for special filenames like "." and ".." - used to filtering results of getDirContents
exclude_special_names s  =  (s/=".")  &&  (s/="..")

-- Strip "drive:/" at the beginning of absolute filename
stripRoot = dropDrive

-- |Replace all '\' with '/'
translatePath = map (\c -> if isPathSeparator c  then '/'  else c)

-- |Filename extension, "dir/name.ext" -> "ext"
getFileSuffix = snd . splitFilenameSuffix

splitFilenameSuffix str  =  (name, drop 1 ext)
                               where (name, ext) = splitExtension str

-- "foo/bar/xyzzy.ext" -> ("foo/bar", "xyzzy.ext")
splitDirFilename :: String -> (String,String)
splitDirFilename str  =  case splitFileName str of
                           x@([d,':',s], name) -> x   -- оставляем ("c:\", name)
                           (dir, name)         -> (dropTrailingPathSeparator dir, name)

-- "foo/bar/xyzzy.ext" -> ("foo/bar", "xyzzy", "ext")
splitFilename3 :: String -> (String,String,String)
splitFilename3 str
   = let (dir, rest) = splitDirFilename str
         (name, ext) = splitFilenameSuffix rest
     in  (dir, name, ext)

-- | Modify the base name.
updateBaseName :: (String->String) -> FilePath -> FilePath
updateBaseName f pth  =  dir </> f name <.> ext
    where
          (dir, name, ext) = splitFilename3 pth


----------------------------------------------------------------------------------------------------
---- Поиск конфиг-файлов программы и SFX модулей ---------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Найти конфиг-файл с заданным именем или возвратить ""
findFile = findName fileExist
findDir  = findName dirExist
findName exist possibleFilePlaces cfgfilename = do
  found <- possibleFilePlaces cfgfilename >>= Utils.filterM exist
  case found of
    x:xs -> return x
    []   -> return ""

-- |Найти конфиг-файл с заданным именем или возвратить имя для создания нового файла
findOrCreateFile possibleFilePlaces cfgfilename = do
  variants <- possibleFilePlaces cfgfilename
  found    <- Utils.filterM fileExist variants
  case found of
    x:xs -> return x
    []   -> return (head variants)


#if defined(FREEARC_WIN)
-- Под Windows все дополнительные файлы по умолчанию лежат в одном каталоге с программой
libraryFilePlaces = configFilePlaces
configFilePlaces filename  =  do -- dir1 <- getAppUserDataDirectory "FreeArc"
                                 exe  <- getExeName
                                 return [-- dir1              </> filename,
                                         takeDirectory exe </> filename]

-- |Имя исполняемого файла программы
getExeName = do
  allocaBytes (long_path_size*4) $ \pOutPath -> do
    c_GetExeName pOutPath (fromIntegral long_path_size*2) >>= peekCWString

foreign import ccall unsafe "Environment.h GetExeName"
  c_GetExeName :: CWFilePath -> CInt -> IO CWFilePath

#else
-- |Места для поиска конфиг-файлов
configFilePlaces  filename  =  do dir1 <- getAppUserDataDirectory "FreeArc"
                                  return [dir1   </> filename
                                         ,"/etc/FreeArc" </> filename]

-- |Места для поиска sfx-модулей
libraryFilePlaces filename  =  return ["/usr/lib/FreeArc"       </> filename
                                      ,"/usr/local/lib/FreeArc" </> filename]
#endif


----------------------------------------------------------------------------------------------------
---- Запуск внешних программ и работа с Windows registry -------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Запустить команду через shell и возвратить её stdout
runProgram cmd = do
    (_, stdout, stderr, ph) <- runInteractiveCommand cmd
    forkIO (hGetContents stderr >>= evaluate.length >> return ())
    result <- hGetContents stdout
    evaluate (length result)
    waitForProcess ph
    return result

-- |Execute file `filename` in the directory `curdir` optionally waiting until it finished
runFile filename curdir wait_finish = do
  withCFilePath filename $ \c_filename -> do
  withCFilePath curdir   $ \c_curdir   -> do
  c_RunFile c_filename c_curdir (i$fromEnum wait_finish)

foreign import ccall safe "Environment.h RunFile"
  c_RunFile :: CFilePath -> CFilePath -> CInt -> IO ()


#if defined(FREEARC_WIN)
-- |Создать HKEY и прочитать из Registry значение типа REG_SZ
registryGetStr root branch key =
  bracket (regCreateKey root branch) regCloseKey
    (\hk -> registryGetStringValue hk key)

-- |Создать HKEY и записать в Registry значение типа REG_SZ
registrySetStr root branch key val =
  bracket (regCreateKey root branch) regCloseKey
    (\hk -> registrySetStringValue hk key val)

-- |Прочитать из Registry значение типа REG_SZ
registryGetStringValue :: HKEY -> String -> IO (Maybe String)
registryGetStringValue hk key = do
  (regQueryValue hk (Just key) >>== Just)
    `catch` (\e -> return Nothing)

-- |Записать в Registry значение типа REG_SZ
registrySetStringValue :: HKEY -> String -> String -> IO ()
registrySetStringValue hk key val =
  withTString val $ \v ->
  regSetValueEx hk key rEG_SZ v (length val*2)
#endif


#if defined(FREEARC_WIN)
-- |OS-specific thread id
foreign import stdcall unsafe "windows.h GetCurrentThreadId"
  getOsThreadId :: IO DWORD
#else
foreign import stdcall unsafe "pthread.h pthread_self"
  getOsThreadId :: IO Int
#endif


----------------------------------------------------------------------------------------------------
---- Операции с неоткрытыми файлами и каталогами ---------------------------------------------------
----------------------------------------------------------------------------------------------------

#if defined(FREEARC_WIN)
-- |Список дисков в системе с их типами
getDrives = getLogicalDrives >>== unfoldr (\n -> Just (n `mod` 2, n `div` 2))
                             >>== zipWith (\c n -> n>0 &&& [c:":"]) ['A'..'Z']
                             >>== concat
                             >>=  mapM (\d -> do t <- withCString d c_GetDriveType; return (d++"\t"++(driveTypes!!i t)))

driveTypes = ["", ""]++split ',' "Removable,Fixed,Network,CD/DVD,Ramdisk"

foreign import stdcall unsafe "windows.h GetDriveTypeA"
  c_GetDriveType :: LPCSTR -> IO CInt
#endif


-- |Create a hierarchy of directories
createDirectoryHierarchy :: FilePath -> IO ()
createDirectoryHierarchy dir = do
  let d = stripRoot dir
  when (d/= "" && exclude_special_names d) $ do
    unlessM (dirExist dir) $ do
      createDirectoryHierarchy (takeDirectory dir)
      dirCreate dir

-- |Создать недостающие каталоги на пути к файлу
buildPathTo filename  =  createDirectoryHierarchy (takeDirectory filename)

-- |Return current directory
getCurrentDirectory = myCanonicalizePath "."

-- | Given path referring to a file or directory, returns a
-- canonicalized path, with the intent that two paths referring
-- to the same file\/directory will map to the same canonicalized
-- path. Note that it is impossible to guarantee that the
-- implication (same file\/dir \<=\> same canonicalizedPath) holds
-- in either direction: this function can make only a best-effort
-- attempt.
myCanonicalizePath :: FilePath -> IO FilePath
myCanonicalizePath fpath | isURL fpath = return fpath
                         | otherwise   =
#if defined(FREEARC_WIN)
  withCFilePath fpath $ \pInPath ->
  allocaBytes (long_path_size*4) $ \pOutPath ->
  alloca $ \ppFilePart ->
    do c_GetFullPathName pInPath (fromIntegral long_path_size*2) pOutPath ppFilePart
       peekCFilePath pOutPath >>== dropTrailingPathSeparator

foreign import stdcall unsafe "GetFullPathNameW"
            c_GetFullPathName :: CWString
                              -> CInt
                              -> CWString
                              -> Ptr CWString
                              -> IO CInt
#else
  withCFilePath fpath $ \pInPath ->
  allocaBytes (long_path_size*4) $ \pOutPath ->
    do c_realpath pInPath pOutPath
       peekCFilePath pOutPath >>== dropTrailingPathSeparator

foreign import ccall unsafe "realpath"
                   c_realpath :: CString
                              -> CString
                              -> IO CString
#endif

-- |Максимальная длина имени файла
long_path_size  =  i c_long_path_size :: Int
foreign import ccall unsafe "Environment.h long_path_size"
  c_long_path_size :: CInt


#if defined(FREEARC_WIN)
-- |Clear file's Archive bit
clearArchiveBit filename = do
    attr <- getFileAttributes filename
    when (attr.&.fILE_ATTRIBUTE_ARCHIVE /= 0) $ do
        setFileAttributes filename (attr - fILE_ATTRIBUTE_ARCHIVE)
#else
clearArchiveBit _ = return ()
#endif


-- |Минимальное datetime, которое только может быть у файла. Соответствует 1 января 1970 г.
aMINIMAL_POSSIBLE_DATETIME = 0 :: CTime

-- |Get file's date/time
getFileDateTime filename  =  fileWithStatus "getFileDateTime" filename stat_mtime

-- |Set file's date/time
setFileDateTime filename datetime  =  withCFilePath filename (`c_SetFileDateTime` datetime)

foreign import ccall unsafe "Environment.h SetFileDateTime"
   c_SetFileDateTime :: CFilePath -> CTime -> IO ()

-- |Пребразование CTime в ClockTime. Используется информация о внутреннем представлении ClockTime в GHC!!!
convert_CTime_to_ClockTime ctime = TOD (realToInteger ctime) 0
  where realToInteger = round . realToFrac :: Real a => a -> Integer

-- |Пребразование ClockTime в CTime
convert_ClockTime_to_CTime (TOD secs _) = i secs

-- |Текстовое представление времени
showtime format t = formatCalendarTime defaultTimeLocale format (unsafePerformIO (toCalendarTime t))

-- |Отформатировать CTime в строку с форматом "%Y-%m-%d %H:%M:%S"
formatDateTime t  =  unsafePerformIO $ do
                       allocaBytes 100 $ \buf -> do
                       c_FormatDateTime buf 100 t
                       peekCString buf

foreign import ccall unsafe "Environment.h FormatDateTime"
  c_FormatDateTime :: CString -> CInt -> CTime -> IO ()


#if defined(FREEARC_UNIX)
executeModes         =  [ownerExecuteMode, groupExecuteMode, otherExecuteMode]
removeFileModes a b  =  a `intersectFileModes` (complement b)
#endif


----------------------------------------------------------------------------------------------------
---- Операции с открытыми файлами ------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

--withMVar  mvar action     =  bracket (takeMVar mvar) (putMVar mvar) action
liftMVar1  action mvar     =  withMVar mvar action
liftMVar2  action mvar x   =  withMVar mvar (\a -> action a x)
liftMVar3  action mvar x y =  withMVar mvar (\a -> action a x y)
returnMVar action          =  action >>= newMVar

-- |Архивный файл, заворачивается в MVar для реализации параллельного доступа из разных тредов ко входным архивам
data Archive = Archive { archiveName :: FilePath
                       , archiveFile :: MVar File
                       }
archiveOpen     name = do file <- fileOpen name >>= newMVar; return (Archive name file)
archiveCreate   name = do file <- fileCreate name >>= newMVar; return (Archive name file)
archiveCreateRW name = do file <- fileCreateRW name >>= newMVar; return (Archive name file)
archiveGetPos        = liftMVar1 fileGetPos   . archiveFile
archiveGetSize       = liftMVar1 fileGetSize  . archiveFile
archiveSeek          = liftMVar2 fileSeek     . archiveFile
archiveRead          = liftMVar2 fileRead     . archiveFile
archiveReadBuf       = liftMVar3 fileReadBuf  . archiveFile
archiveWrite         = liftMVar2 fileWrite    . archiveFile
archiveWriteBuf      = liftMVar3 fileWriteBuf . archiveFile
archiveClose         = liftMVar1 fileClose    . archiveFile

-- |Скопировать данные из одного архива в другой и затем восстановить позицию в исходном архиве
archiveCopyData srcarc pos size dstarc = do
  withMVar (archiveFile srcarc) $ \srcfile ->
    withMVar (archiveFile dstarc) $ \dstfile -> do
      restorePos <- fileGetPos srcfile
      fileSeek      srcfile pos
      fileCopyBytes srcfile size dstfile
      fileSeek      srcfile restorePos

-- |При работе с одним физическим диском (наиболее частый вариант)
-- нет смысла выполнять несколько I/O операций параллельно,
-- поэтому мы их все проводим через "угольное ушко" одной-единственной MVar
oneIOAtTime = unsafePerformIO$ newMVar "oneIOAtTime value"
fileReadBuf  file buf size = withMVar oneIOAtTime $ \_ -> fileReadBufSimple  file buf size
fileWriteBuf file buf size = withMVar oneIOAtTime $ \_ -> fileWriteBufSimple file buf size


----------------------------------------------------------------------------------------------------
---- URL access ------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

data File = FileOnDisk FileOnDisk | URL URL

fileOpen           = choose0 fOpen           url_open
fileCreate         = choose0 fCreate         (\_ -> err "url_create")
fileCreateRW       = choose0 fCreateRW       (\_ -> err "url_create_rw")
fileAppendText     = choose0 fAppendText     (\_ -> err "url_append_text")
fileGetPos         = choose  fGetPos         (url_pos  .>>==i)
fileGetSize        = choose  fGetSize        (url_size .>>==i)
fileSeek           = choose  fSeek           (\f p -> url_seek f (i p))
fileReadBufSimple  = choose  fReadBufSimple  url_read
fileWriteBufSimple = choose  fWriteBufSimple (\_ _ _ -> err "url_write")
fileFlush          = choose  fFlush          (\_     -> err "url_flush")
fileClose          = choose  fClose          url_close

-- |Проверяет существование файла/URL
fileExist name | isURL name = do url <- withCString name url_open
                                 url_close url
                                 return (url/=nullPtr)
               | otherwise  = fExist name

-- |Проверяет, является ли имя url
isURL name = "://" `isInfixOf` name

{-# NOINLINE choose0 #-}
choose0 onfile onurl name | isURL name = do url <- withCString name onurl
                                            when (url==nullPtr) $ do
                                              fail$ "Can't open url "++name   --registerError$ CANT_OPEN_FILE name
                                            return (URL url)
                          | otherwise  = onfile name >>== FileOnDisk

choose _ onurl  (URL        url)   = onurl  url
choose onfile _ (FileOnDisk file)  = onfile file

{-# NOINLINE err #-}
err s  =  fail$ s++" isn't implemented"    --registerError$ GENERAL_ERROR ["0343 %1 isn't implemented", s]


type URL = Ptr ()
foreign import ccall safe "URL.h"  url_setup_proxy         :: Ptr CChar -> IO ()
foreign import ccall safe "URL.h"  url_setup_bypass_list   :: Ptr CChar -> IO ()
foreign import ccall safe "URL.h"  url_open   :: Ptr CChar -> IO URL
foreign import ccall safe "URL.h"  url_pos    :: URL -> IO Int64
foreign import ccall safe "URL.h"  url_size   :: URL -> IO Int64
foreign import ccall safe "URL.h"  url_seek   :: URL -> Int64 -> IO ()
foreign import ccall safe "URL.h"  url_read   :: URL -> Ptr a -> Int -> IO Int
foreign import ccall safe "URL.h"  url_close  :: URL -> IO ()


----------------------------------------------------------------------------------------------------
---- Под Windows мне пришлось реализовать библиотеку в/в самому для поддержки файлов >4Gb и Unicode имён файлов
----------------------------------------------------------------------------------------------------
#if defined(FREEARC_WIN)

type FileOnDisk      = FD
type CFilePath       = CWFilePath
type FileAttributes  = FileAttributeOrFlag
withCFilePath        = withCWFilePath
peekCFilePath        = peekCWString
fOpen       name     = wopen name (read_flags  .|. o_BINARY) 0o666
fCreate     name     = wopen name (write_flags .|. o_BINARY .|. o_TRUNC) 0o666
fCreateRW   name     = wopen name (rw_flags    .|. o_BINARY .|. o_TRUNC) 0o666
fAppendText name     = wopen name (append_flags) 0o666
fGetPos              = wtell
fGetSize             = wfilelength
fSeek   file pos     = wseek file pos sEEK_SET
fReadBufSimple       = wread
fWriteBufSimple      = wwrite
fFlush  file         = return ()
fClose               = wclose
fExist               = wDoesFileExist
fileRemove           = wunlink
fileRename           = wrename
fileWithStatus       = wWithFileStatus
fileStdin            = 0
stat_mode            = wst_mode
stat_size            = wst_size
stat_mtime           = wst_mtime
dirCreate            = wmkdir
dirExist             = wDoesDirectoryExist
dirRemove            = wrmdir
dirList dir          = dirWildcardList (dir </> "*")
dirWildcardList wc   = withList $ \list -> do
                         wfindfiles wc $ \find -> do
                           name <- w_find_name find
                           list <<= name

#else

type FileOnDisk      = Handle
type CFilePath       = CString
type FileAttributes  = Int
withCFilePath s a    = (`withCString` a) =<< str2filesystem s
peekCFilePath ptr    = peekCString ptr >>= filesystem2str
fOpen                = (`openBinaryFile` ReadMode     ) =<<. str2filesystem
fCreate              = (`openBinaryFile` WriteMode    ) =<<. str2filesystem
fCreateRW            = (`openBinaryFile` ReadWriteMode) =<<. str2filesystem
fAppendText          = (`openFile`       AppendMode   ) =<<. str2filesystem
fGetPos              = hTell
fGetSize             = hFileSize
fSeek                = (`hSeek` AbsoluteSeek)
fReadBufSimple       = hGetBuf
fWriteBufSimple      = hPutBuf
fFlush               = hFlush
fClose               = hClose
fExist               = doesFileExist =<<. str2filesystem
fileGetStatus        = getFileStatus =<<. str2filesystem
fileSetMode name mode= (`setFileMode` mode) =<< str2filesystem name
fileRemove name      = removeFile    =<<  str2filesystem name
fileRename a b       = do a1 <- str2filesystem a; b1 <- str2filesystem b; renameFile a1 b1
fileSetSize          = hSetFileSize
fileStdin            = stdin
stat_mode            = st_mode
stat_size            = st_size  .>>== i
stat_mtime           = st_mtime
dirCreate            = createDirectory     =<<. str2filesystem
dirExist             = doesDirectoryExist  =<<. str2filesystem
dirRemove            = removeDirectory     =<<. str2filesystem
dirList dir          = str2filesystem dir >>= getDirectoryContents >>= mapM filesystem2str
dirWildcardList wc   = dirList (takeDirectory wc)  >>==  filter (match$ takeFileName wc)

-- kidnapped from System.Directory :)))
fileWithStatus :: String -> FilePath -> (Ptr CStat -> IO a) -> IO a
fileWithStatus loc name f = do
  modifyIOError (`ioeSetFileName` name) $
    allocaBytes sizeof_stat $ \p ->
      withCFilePath name $ \s -> do
        throwErrnoIfMinus1Retry_ loc (c_stat s p)
	f p

#endif

fileRead      file size = allocaBytes size $ \buf -> do fileReadBuf file buf size; peekCStringLen (buf,size)
fileWrite     file str  = withCStringLen str $ \(buf,size) -> fileWriteBuf file buf size
fileGetBinary name      = bracket (fileOpen   name) fileClose (\file -> fileGetSize file >>= fileRead file.i)
filePutBinary name str  = bracket (fileCreate name) fileClose (`fileWrite` str)

-- |Скопировать заданное количество байт из одного открытого файла в другой
fileCopyBytes srcfile size dstfile = do
  allocaBytes aHUGE_BUFFER_SIZE $ \buf -> do        -- используем `alloca`, чтобы автоматически освободить выделенный буфер при выходе
    doChunks size aHUGE_BUFFER_SIZE $ \bytes -> do  -- Скопировать size байт кусками по aHUGE_BUFFER_SIZE
      bytes <- fileReadBuf srcfile buf bytes        -- Проверим, что прочитано ровно столько байт, сколько затребовано
      fileWriteBuf dstfile buf bytes

-- |True, если существует файл или каталог с заданным именем
fileOrDirExist f  =  mapM ($f) [fileExist, dirExist] >>== or


---------------------------------------------------------------------------------------------------
---- Глобальные настройки перекодировки для использования в глубоко вложенных функциях ------------
---------------------------------------------------------------------------------------------------

-- |Translate filename from filesystem to internal encoding
filesystem2str'   = unsafePerformIO$ newIORef$ id   -- 'id' means that inifiles can't have non-English names
filesystem2str s  = val filesystem2str' >>== ($s)
-- |Translate filename from internal to filesystem encoding
str2filesystem'   = unsafePerformIO$ newIORef$ id
str2filesystem s  = val str2filesystem' >>== ($s)


---------------------------------------------------------------------------------------------------
---- Utility functions ----------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

foreign import ccall unsafe "string.h"
    memset :: Ptr a -> Int -> CSize -> IO ()

foreign import ccall unsafe "Environment.h memxor"
    memxor :: Ptr a -> Ptr a -> Int -> IO ()

