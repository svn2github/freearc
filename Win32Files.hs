{-# OPTIONS_GHC -cpp -fvia-C #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Win32Files
-- Copyright   :  (c) Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
-- License     :  Public domain
--
-- Maintainer  :  Bulat.Ziganshin@gmail.com
-- Stability   :  experimental
-- Portability :  GHC/mingw32
--
-----------------------------------------------------------------------------

module Win32Files where

import Data.Bits
import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.C.Error
import Foreign.Marshal.Alloc
import Foreign.Ptr
import System.IO.Error
import System.Posix.Internals (s_isdir, o_NONBLOCK, o_NOCTTY, o_CREAT, o_RDONLY, o_WRONLY, o_RDWR, o_APPEND)
import System.Posix.Types
import System.Win32
import System.Win32.File

import Utils


type FD = CInt                -- handle of open file

#if 1
type CWFilePath   = LPCTSTR   -- filename in C land
type CWFileOffset = Int64     -- filesize or filepos in C land
withCWFilePath = withTString  -- FilePath->CWFilePath conversion
peekCWFilePath = peekTString  -- CWFilePath->FilePath conversion
#else
type CWFilePath   = CString
type CWFileOffset = COff
withCWFilePath = withCString
peekCWFilePath = peekCString
#endif


waccess :: String -> CMode -> IO Int
waccess name mode =
  modifyIOError (`ioeSetFileName` name) $
  withCWFilePath name $ \ p_name ->
  c_waccess p_name mode
foreign import ccall unsafe "Win32Files.h _waccess"
  c_waccess :: CWFilePath -> CMode -> IO Int

wchmod :: String -> CMode -> IO ()
wchmod name mode =
  modifyIOError (`ioeSetFileName` name) $
  withCWFilePath name $ \ p_name ->
  throwErrnoIfMinus1Retry_ "chmod" $ c_wchmod p_name mode
foreign import ccall unsafe "Win32Files.h _wchmod"
  c_wchmod :: CWFilePath -> CMode -> IO Int

wunlink :: String -> IO ()
wunlink name =
  modifyIOError (`ioeSetFileName` name) $
  withCWFilePath name $ \ p_name ->
  throwErrnoIfMinus1Retry_ "unlink" $ c_wunlink p_name
foreign import ccall unsafe "Win32Files.h _wunlink"
  c_wunlink :: CWFilePath -> IO Int

wrename :: String -> String -> IO ()
wrename oldname newname =
  modifyIOError (`ioeSetFileName` oldname) $
  withCWFilePath oldname $ \ p_oldname ->
  withCWFilePath newname $ \ p_newname ->
  throwErrnoIfMinus1Retry_ "rename" $ c_wrename p_oldname p_newname
foreign import ccall unsafe "Win32Files.h _wrename"
  c_wrename :: CWFilePath -> CWFilePath -> IO Int

wcreat :: String -> Int -> IO FD
wcreat name mode =
  modifyIOError (`ioeSetFileName` name) $
  withCWFilePath name $ \ p_name ->
  throwErrnoIfMinus1Retry "creat" $ c_wcreat p_name mode
foreign import ccall unsafe "Win32Files.h _wcreat"
  c_wcreat :: CWFilePath -> Int -> IO FD

wopen :: String -> CInt -> CMode -> IO FD
wopen name access mode =
  modifyIOError (`ioeSetFileName` name) $
  withCWFilePath name $ \ p_name ->
  throwErrnoIfMinus1Retry "open" $ c_wopen p_name access mode
foreign import ccall safe "Win32Files.h _wopen"
  c_wopen :: CWFilePath -> CInt -> CMode -> IO FD

wclose :: FD -> IO ()
wclose h =
  throwErrnoIfMinus1Retry_ "wclose" $ c_close h
foreign import ccall safe "Win32Files.h _close"
  c_close :: FD -> IO Int

wread :: Integral int => FD -> Ptr a -> int -> IO int
wread h buf size =
  (throwErrnoIfMinus1Retry "read" $ c_read h (castPtr buf) (i size)) >>== i
foreign import ccall safe "Win32Files.h _read"
  c_read :: FD -> Ptr a -> Int -> IO Int

wwrite :: Integral int => FD -> Ptr a -> int -> IO ()
wwrite h buf size =
  throwErrnoIfMinus1Retry_ "write" $ c_write h (castPtr buf) (i size)
foreign import ccall safe "Win32Files.h _write"
  c_write :: FD -> Ptr a -> Int -> IO Int

wtell :: Integral int => FD -> IO int
wtell h =
  (throwErrnoIfMinus1Retry "tell" $ c_telli64 h) >>== i
foreign import ccall unsafe "Win32Files.h _telli64"
  c_telli64 :: FD -> IO CWFileOffset

wseek :: Integral int => FD -> int -> CInt -> IO ()
wseek h offset direction =
  throwErrnoIfMinus1Retry_ "seek" $ c_lseeki64 h (i offset) direction
foreign import ccall unsafe "Win32Files.h _lseeki64"
  c_lseeki64 :: FD -> CWFileOffset -> CInt -> IO CWFileOffset

wfilelength :: Integral int => FD -> IO int
wfilelength h =
  (throwErrnoIfMinus1Retry "filelength" $ c_filelengthi64 h) >>== i
foreign import ccall safe "Win32Files.h _filelengthi64"
  c_filelengthi64 :: FD -> IO CWFileOffset

wmkdir :: FilePath -> IO ()
wmkdir name = do
  modifyIOError (`ioeSetFileName` name) $ do
  withCWFilePath name $ \ p_name -> do
  throwErrnoIfMinus1Retry_ "mkdir" $ c_wmkdir p_name
foreign import ccall unsafe "Win32Files.h _wmkdir"
   c_wmkdir :: CWFilePath -> IO CInt

wrmdir :: FilePath -> IO ()
wrmdir name = do
  modifyIOError (`ioeSetFileName` name) $ do
  withCWFilePath name $ \ p_name -> do
  throwErrnoIfMinus1Retry_ "rmdir" (c_wrmdir p_name)
foreign import ccall unsafe "Win32Files.h _wrmdir"
   c_wrmdir :: CWFilePath -> IO CInt



type CWFindData = ()

foreign import ccall unsafe "Win32Files.h __w_find_sizeof"      w_find_sizeof      :: Int
foreign import ccall unsafe "Win32Files.h __w_find_time_create" w_find_time_create :: Ptr CWFindData -> IO CTime
foreign import ccall unsafe "Win32Files.h __w_find_time_access" w_find_time_access :: Ptr CWFindData -> IO CTime
foreign import ccall unsafe "Win32Files.h __w_find_time_write"  w_find_time_write  :: Ptr CWFindData -> IO CTime
foreign import ccall unsafe "Win32Files.h __w_find_attrib"      c_w_find_attrib    :: Ptr CWFindData -> IO Word
foreign import ccall unsafe "Win32Files.h __w_find_size"        c_w_find_size      :: Ptr CWFindData -> IO CWFileOffset
foreign import ccall unsafe "Win32Files.h __w_find_name"        c_w_find_name      :: Ptr CWFindData -> IO CWFilePath

w_find_name   =  c_w_find_name   .>>=  peekCWFilePath
w_find_size   =  c_w_find_size   .>>== i
w_find_attrib =  c_w_find_attrib .>>== i
w_find_isDir  =  w_find_attrib   .>>== (\a -> a.&.fILE_ATTRIBUTE_DIRECTORY /= 0)

wfindfirst :: String -> Ptr CWFindData -> IO (Maybe Int)
wfindfirst name p_find = do
  modifyIOError (`ioeSetFileName` name) $ do
  withCWFilePath name $ \ p_name -> do
  res <- c_wfindfirsti64 p_name p_find
  case res of
    -1 -> do err <- getErrno
             if err `elem` [eMFILE, eNOENT]
               then return Nothing  -- no files found
               else throwErrno "findfirst"
    _  -> return (Just res)
foreign import ccall unsafe "Win32Files.h _wfindfirsti64"
  c_wfindfirsti64 :: CWFilePath -> Ptr CWFindData -> IO Int

wfindnext :: Int -> Ptr CWFindData -> IO Bool
wfindnext h p_find = do
  res <- c_wfindnexti64 h p_find
  case res of
    -1 -> do err <- getErrno
             if err `elem` [eMFILE, eNOENT]
               then return True  -- no more files
               else throwErrno "findnext"
    _  -> return False
foreign import ccall unsafe "Win32Files.h _wfindnexti64"
  c_wfindnexti64 :: Int -> Ptr CWFindData -> IO Int

findclose :: Int -> IO ()
findclose h = do
  c_findclose h
  return ()
foreign import ccall unsafe "Win32Files.h _findclose"
  c_findclose :: Int -> IO Int

wfindfiles :: String -> (Ptr CWFindData -> IO ()) -> IO ()
wfindfiles name action = do
  allocaBytes w_find_sizeof $ \ p_find -> do
  res <- wfindfirst name p_find
  case res of
    Nothing -> return ()
    Just h ->  do repeat_until $ do
                    action p_find
                    wfindnext h p_find
                  findclose h



type CWStat = ()

foreign import ccall unsafe "Win32Files.h __w_stat_sizeof"  wst_sizeof  :: Int
foreign import ccall unsafe "Win32Files.h __w_stat_mode"    wst_mode    :: Ptr CWStat -> IO CMode
foreign import ccall unsafe "Win32Files.h __w_stat_ctime"   wst_ctime   :: Ptr CWStat -> IO CTime
foreign import ccall unsafe "Win32Files.h __w_stat_atime"   wst_atime   :: Ptr CWStat -> IO CTime
foreign import ccall unsafe "Win32Files.h __w_stat_mtime"   wst_mtime   :: Ptr CWStat -> IO CTime
foreign import ccall unsafe "Win32Files.h __w_stat_size"    c_wst_size  :: Ptr CWStat -> IO CWFileOffset

wst_size  =  c_wst_size .>>== i

wfstat :: FD -> Ptr CWStat -> IO ()
wfstat h p_stat =
  throwErrnoIfMinus1Retry_ "wfstat" $ c_fstati64 h p_stat
foreign import ccall unsafe "Win32Files.h _fstati64"
  c_fstati64 :: FD -> Ptr CWStat -> IO Int

wstat :: String -> Ptr CWStat -> IO ()
wstat name p_stat =
  modifyIOError (`ioeSetFileName` name) $
  withCWFilePath name $ \ p_name ->
  throwErrnoIfMinus1Retry_ "stat" $ c_wstati64 p_name p_stat
foreign import ccall unsafe "Win32Files.h _wstati64"
  c_wstati64 :: CWFilePath -> Ptr CWStat -> IO Int

{- |The operation 'doesDirectoryExist' returns 'True' if the argument file
exists and is a directory, and 'False' otherwise.
-}

wDoesDirectoryExist :: FilePath -> IO Bool
wDoesDirectoryExist name =
 catch
   (wWithFileStatus "doesDirectoryExist" name $ \st -> wIsDirectory st)
   (\ _ -> return False)

{- |The operation 'doesFileExist' returns 'True'
if the argument file exists and is not a directory, and 'False' otherwise.
-}

wDoesFileExist :: FilePath -> IO Bool
wDoesFileExist name = do
 catch
   (wWithFileStatus "wDoesFileExist" name $ \st -> do b <- wIsDirectory st; return (not b))
   (\ _ -> return False)

wWithFileStatus :: String -> FilePath -> (Ptr CWStat -> IO a) -> IO a
wWithFileStatus loc name f = do
  modifyIOError (`ioeSetFileName` name) $
    allocaBytes wst_sizeof $ \p_stat ->
      withCWFilePath name $ \p_name -> do
        throwErrnoIfMinus1Retry_ loc $
          c_wstati64 p_name p_stat
        f p_stat

wIsDirectory :: Ptr CWStat -> IO Bool
wIsDirectory stat = do
  mode <- wst_mode stat
  return (s_isdir mode)




foreign import ccall unsafe "HsBase.h __hscore_seek_cur" sEEK_CUR :: CInt
foreign import ccall unsafe "HsBase.h __hscore_seek_set" sEEK_SET :: CInt
foreign import ccall unsafe "HsBase.h __hscore_seek_end" sEEK_END :: CInt

std_flags    = o_NONBLOCK   .|. o_NOCTTY
output_flags = std_flags    .|. o_CREAT
read_flags   = std_flags    .|. o_RDONLY
write_flags  = output_flags .|. o_WRONLY
rw_flags     = output_flags .|. o_RDWR
append_flags = write_flags  .|. o_APPEND


{-# NOINLINE waccess #-}
{-# NOINLINE wchmod #-}
{-# NOINLINE wunlink #-}
{-# NOINLINE wrename #-}
{-# NOINLINE wcreat #-}
{-# NOINLINE wopen #-}
{-# NOINLINE wclose #-}
{-# NOINLINE wread #-}
{-# NOINLINE wwrite #-}
{-# NOINLINE wtell #-}
{-# NOINLINE wseek #-}
{-# NOINLINE wfilelength #-}
{-# NOINLINE wmkdir #-}
{-# NOINLINE wrmdir #-}
{-# NOINLINE wfindfirst #-}
{-# NOINLINE wfindnext #-}
{-# NOINLINE findclose #-}
{-# NOINLINE wfindfiles #-}
{-# NOINLINE wfstat #-}
{-# NOINLINE wstat #-}
{-# NOINLINE wDoesDirectoryExist #-}
{-# NOINLINE wDoesFileExist #-}
{-# NOINLINE wWithFileStatus #-}
{-# NOINLINE wIsDirectory #-}
