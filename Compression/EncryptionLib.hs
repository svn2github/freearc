{-# OPTIONS_GHC -cpp -fno-monomorphism-restriction #-}
----------------------------------------------------------------------------------------------------
---- (ƒе)шифрование данных.                                                                     ----
---- »нтерфейс с написанными на —и процедурами, выполн€ющими всю реальную работу.               ----
----------------------------------------------------------------------------------------------------
module EncryptionLib where

import Control.Monad
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import System.IO.Unsafe

import CompressionLib

----------------------------------------------------------------------------------------------------
----- Encryption routines --------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Query encryption method for some parameter
encryptionGet = compressionGet

-- |Generate key based on password and salt using given number of hashing iterations
pbkdf2Hmac :: String -> String -> Int -> Int -> String
pbkdf2Hmac password salt iterations keySize = unsafePerformIO $
  withCStringLen   password $ \(c_password, c_password_len) -> do
    withCStringLen salt     $ \(c_salt,     c_salt_len) -> do
    allocaBytes    keySize  $ \c_key -> do
      c_Pbkdf2Hmac c_password (ii c_password_len) c_salt (ii c_salt_len) (ii iterations) c_key (ii keySize)
      peekCStringLen (c_key, keySize)


----------------------------------------------------------------------------------------------------
----- External encryption/PRNG routines ------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Generate key based on password and salt using given number of hashing iterations
foreign import ccall unsafe  "Compression.h  Pbkdf2Hmac"
   c_Pbkdf2Hmac :: Ptr CChar -> CInt -> Ptr CChar -> CInt -> CInt -> Ptr CChar -> CInt -> IO ()

-- PRNG
foreign import ccall unsafe  "Compression.h  fortuna_size"
   prng_size :: CInt
foreign import ccall unsafe  "Compression.h  fortuna_start"
   prng_start :: Ptr CChar -> IO CInt
foreign import ccall unsafe  "Compression.h  fortuna_add_entropy"
   prng_add_entropy :: Ptr CChar -> CULong -> Ptr CChar -> IO CInt
foreign import ccall unsafe  "Compression.h  fortuna_ready"
   prng_ready :: Ptr CChar -> IO CInt
foreign import ccall unsafe  "Compression.h  fortuna_read"
   prng_read :: Ptr CChar -> CULong -> Ptr CChar -> IO CULong

