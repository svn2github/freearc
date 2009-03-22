----------------------------------------------------------------------------------------------------
---- Шифрование, дешифрование и криптографический PRNG.                                         ----
---- Процедура generateEncryption добавляет к цепочке алгоритмов сжатия алгоритм(ы) шифрования. ----
---- Процедура generateDecryption добавляет ключи к записи алгоритма сжатия+шифрования,         ----
---- Процедура generateRandomBytes возвращает последовательность крипт. случайных байт          ----
----------------------------------------------------------------------------------------------------
module Encryption (generateEncryption, generateDecryption, generateRandomBytes) where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Monad
import Data.Char
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import System.IO.Unsafe

import EncryptionLib
import Utils
import Errors
import Compression

---------------------------------------------------------------------------------------------------
---- Шифрование и дешифрование --------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- |Возвращает две функции, добавляющие к методу сжатия алгоритм шифрования:
-- первая включает ключ шифрования (для реального использования)
-- вторая только вспомогательные данные (для сохранения в архиве)
generateEncryption encryption password = do
    addRandomness
    result <- foreach (split_compressor encryption) $ \algorithm -> do
        initVector <- generateRandomBytes (encryptionGet "ivSize"  algorithm)
        salt       <- generateRandomBytes (encryptionGet "keySize" algorithm)
        let numIterations = encryptionGet "numIterations" algorithm
            checkCodeSize = 2
            (key, checkCode) = deriveKey algorithm password salt numIterations checkCodeSize
        return (algorithm++":k"++encode16 key++":i"++encode16 initVector
               ,algorithm++":s"++encode16 salt++":c"++encode16 checkCode
                         ++":i"++encode16 initVector)
    return ((++map fst result), (++map snd result))


-- |Обработать compressor, прочитанный из архива, добавив к нему информацию,
-- необходимую для расшифровки
generateDecryption compressor decryption_info  =  mapM addKey compressor   where
  -- Обработать алгоритм шифрования, добавив к нему key, выведенный
  -- из заданного пользователем пароля и salt, сохранённого в параметрах алгоритма
  addKey algorithm | not (isEncryption algorithm)
                               = return (Just algorithm)    -- non-encryption algorithm stays untouched
                   | otherwise = do
    -- Выделим из записи алгоритма параметры, необходимые для вычисления и проверки ключа
    let name:params   = split_method algorithm
        param c       = params .$map (splitAt 1) .$lookup c   -- найти значение параметра с
        salt          = param "s" `defaultVal` (error$ algorithm++" doesn't include salt") .$decode16
        checkCode     = param "c" `defaultVal` "" .$decode16
        numIterations = param "n" `defaultVal` (error$ algorithm++" doesn't include numIterations") .$readInt

    -- Воспользуемся списком паролей распаковкии и, если придётся,
    -- добавим в него новые пароли, введённые пользователем
    let (dont_ask_passwords, mvar_passwords, keyfiles, ask_decryption_password, bad_decryption_password) = decryption_info
    modifyMVar mvar_passwords $ \passwords -> do
      passwords_list <- ref passwords

      -- Попробовать расшифровку со всеми возможными keyfiles и затем без них
      let checkPwd password  =  firstJust$ map doCheck (keyfiles++[""])
            where -- Если верификация по checkCode успешна, то возвращаем найденный ключ алгоритма шифрования, иначе - Nothing
                  doCheck keyfile  =  recheckCode==checkCode  &&&  Just key
                    -- Вычислим ключ расшифровки key и recheckCode, используемый для быстрой проверки правильности пароля
                    where (key, recheckCode) = deriveKey algorithm (password++keyfile) salt numIterations (length checkCode)

      -- Процедура подбора пароля для расшифровки блока.
      -- Каждый вероятный пароль проверяется с помощью checkPwd.
      -- Если ни один из уже известных паролей не подошёл, то мы запрашиваем у пользователя новые
      let findDecryptionKey (password:pwds) = do
            case (checkPwd password) of            -- Если верификация в checkPwd прошла успешно
              Just key -> return (Just key)        --   то возвращаем найденный ключ алгоритма шифрования
              Nothing  -> findDecryptionKey pwds   --   иначе - пробуем следующие пароли

          findDecryptionKey [] = do   -- Сюда мы попадаем если ни один старый пароль не подошёл
            -- Если использована опция -p-/-op- или пользователь введёт пустую строку -
            -- значит, этот блок расшифровать нам не удастся
            if dont_ask_passwords  then return Nothing   else do
            password <- ask_decryption_password
            if password==""        then return Nothing   else do
            -- Добавим новый пароль в список паролей, проверяемых при распаковке
            passwords_list .= (password:)
            case (checkPwd password) of               -- Если верификация в checkPwd прошла успешно
              Just key -> return (Just key)           --   то возвращаем найденный ключ алгоритма шифрования
              Nothing  -> do bad_decryption_password  --   иначе - запрашиваем другой пароль
                             findDecryptionKey []
      --
      key <- findDecryptionKey passwords
      pl  <- val passwords_list
      return (pl, key.$fmap (\key -> algorithm++":k"++encode16 key))


-- |Вывести из password+salt ключ шифрования и код проверки
deriveKey algorithm password salt numIterations checkCodeSize =
    splitAt keySize $ pbkdf2Hmac password salt numIterations (keySize+checkCodeSize)
    where   keySize = encryptionGet "keySize" algorithm


-- |Check action result and abort on (internal) error
check test msg action = do
  res <- action
  unless (test res) (fail$ "Error in "++msg)

-- |OK return code for LibTomCrypt library
aCRYPT_OK = 0


---------------------------------------------------------------------------------------------------
---- Криптографический генератор случайных последовательностей байт -------------------------------
---------------------------------------------------------------------------------------------------

-- |Добавить случайную информацию, полученную от ОС, в PRNG
addRandomness = withMVar prng_state addRandomnessTo
addRandomnessTo prng = do
  let size = 4096
  allocaBytes size $ \buf -> do
    bytes <- systemRandomData buf (i size)
    check (==aCRYPT_OK) "prng_add_entropy" $
      prng_add_entropy buf (i bytes) prng

-- |Сгенерить случайную последовательность байт указанной длины
generateRandomBytes bytes = do
  withMVar prng_state $ \prng -> do
    allocaBytes bytes $ \buf -> do
      check (==i bytes) "prng_read" $
        prng_read buf (i bytes) prng
      peekCAStringLen (buf, bytes)

-- |Переменная, хранящая состояние PRNG
{-# NOINLINE prng_state #-}
prng_state :: MVar (Ptr CChar)
prng_state = unsafePerformIO $ do
   prng <- mallocBytes (i prng_size)
   prng_start prng
   addRandomnessTo prng
   newMVar prng

-- |Fill buffer with system-generated pseudo-random data
foreign import ccall unsafe "Environment.h systemRandomData"
  systemRandomData :: Ptr CChar -> CInt -> IO CInt

