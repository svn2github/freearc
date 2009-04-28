{-# OPTIONS_GHC -cpp #-}
----------------------------------------------------------------------------------------------------
---- Имена файлов, храняющиеся в UTF8+\0 представлении.                                         ----
---- Имена сортируются/сравниваются с учётом или без учёта регистра в зависимости от того,      ----
----   откомпилирована ли программа под Windows или Unix                                        ----
----------------------------------------------------------------------------------------------------
-- |
-- Module      :  UTF8Z
-- Copyright   :  (c) Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
-- License     :  Public domain
--
-- Maintainer  :  Bulat.Ziganshin@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-----------------------------------------------------------------------------

module UTF8Z where

import Data.Bits
import Data.Char
import Data.Word
import GHC.Exts
import GHC.IOBase
import GHC.Word
import Foreign.C.Types


data PackedString = PS ByteArray#

instance Eq PackedString where
  a==b  =  a `comparePS` b == EQ
  {-# INLINE (==) #-}

instance Ord PackedString where
  compare  =  comparePS
  {-# INLINE compare #-}

packString :: String -> PackedString
packString = inlinePerformIO.packStr

packStr :: String -> IO PackedString
packStr str = IO $ \s ->
              case newByteArray# (utfCount str +# 1#) s of { (# s, arr #) ->
              case go arr 0# str s of { s ->
              case unsafeFreezeByteArray# arr s of { (# s, arr #) ->
              (# s, PS arr #) } } }
 where
  go p i []     s#  =      writeWord8Array# p i (u 0) s#
  go p i (x:xs) s#
    | ord x<=0x07f  = case writeWord8Array# p i (u (ord x)) s# of { s# ->
                           go p (i +# 1#) xs s# }
    | ord x<=0x07ff = case writeWord8Array# p i         (u (0xC0 .|. ((ord x `shiftR` 6) .&. 0x1F))) s# of { s# ->
                      case writeWord8Array# p (i +# 1#) (u (0x80 .|. (ord x .&. 0x3F))) s# of { s# ->
                           go p (i +# 2#) xs s# } }
    | ord x<=0xffff = case writeWord8Array# p i         (u (0xE0 .|. ((ord x `shiftR` 12) .&. 0x0F))) s# of { s# ->
                      case writeWord8Array# p (i +# 1#) (u (0x80 .|. ((ord x `shiftR` 6) .&. 0x3F))) s# of { s# ->
                      case writeWord8Array# p (i +# 2#) (u (0x80 .|. (ord x .&. 0x3F))) s# of { s# ->
                           go p (i +# 3#) xs s# } } }
    | otherwise     = case writeWord8Array# p i         (u (0xF0 .|. (ord x `shiftR` 18))) s# of { s# ->
                      case writeWord8Array# p (i +# 1#) (u (0x80 .|. ((ord x `shiftR` 12) .&. 0x3F))) s# of { s# ->
                      case writeWord8Array# p (i +# 2#) (u (0x80 .|. ((ord x `shiftR` 6) .&. 0x3F))) s# of { s# ->
                      case writeWord8Array# p (i +# 3#) (u (0x80 .|. (ord x .&. 0x3F))) s# of { s# ->
                           go p (i +# 4#) xs s# } } } }
  u n = case fromIntegral n of { (W8# w) -> w }


unpackPS :: PackedString -> String
unpackPS (PS ba) = unpackFoldrUtf8# ba f [] where
    f ch r = C# ch : r


{-# INLINE comparePS #-}
-- Выберем между strcmp и strcasecmp в зависмости от ОС
#if defined(FREEARC_WIN)
comparePS (PS x) (PS y) = case inlinePerformIO$ strcasecmp (unsafeCoerce# x) (unsafeCoerce# y) of
#else
comparePS (PS x) (PS y) = case inlinePerformIO$ strcmp (unsafeCoerce# x) (unsafeCoerce# y) of
#endif
                           x | x<0       -> LT
                             | x>0       -> GT
                             | otherwise -> EQ

-- C functions that compare strings either case-sensitive or case-ignoring
foreign import ccall unsafe
    strcmp :: ByteArray# -> ByteArray# -> IO CInt

foreign import ccall unsafe
    strcasecmp :: ByteArray# -> ByteArray# -> IO CInt


-- -----------------------------------------------------------------------------
-- Local utility functions

{-# INLINE utfCount #-}
utfCount :: String -> Int#
utfCount cs = uc 0# cs where
    uc n []  = n
    uc n (x:xs)
        | ord x <= 0x7f = uc (n +# 1#) xs
        | ord x <= 0x7ff = uc (n +# 2#) xs
        | ord x <= 0xffff = uc (n +# 3#) xs
        | ord x <= 0x1fffff = uc (n +# 4#) xs
        | ord x <= 0x3ffffff = uc (n +# 5#) xs
        | ord x <= 0x7fffffff = uc (n +# 6#) xs
        | otherwise = error "invalid string"


-- | Convert Unicode characters to UTF-8.
utfList :: String -> [Word8]
utfList [] = []
utfList (x:xs)
  | ord x<=0x007f = fromIntegral (ord x) : utfList xs
  | ord x<=0x07ff = fromIntegral (0xC0 .|. ((ord x `shiftR` 6) .&. 0x1F)):
                    fromIntegral (0x80 .|. (ord x .&. 0x3F)):
                    utfList xs
  | ord x<=0xffff = fromIntegral (0xE0 .|. ((ord x `shiftR` 12) .&. 0x0F)):
                    fromIntegral (0x80 .|. ((ord x `shiftR` 6) .&. 0x3F)):
                    fromIntegral (0x80 .|. (ord x .&. 0x3F)):
                    utfList xs
  | otherwise     = fromIntegral (0xF0 .|. (ord x `shiftR` 18)) :
                    fromIntegral (0x80 .|. ((ord x `shiftR` 12) .&. 0x3F)) :
                    fromIntegral (0x80 .|. ((ord x `shiftR` 6) .&. 0x3F)) :
                    fromIntegral (0x80 .|. (ord x .&. 0x3F)) :
                    utfList xs


{-# INLINE unpackFoldrUtf8# #-}
unpackFoldrUtf8# :: ByteArray# -> (Char# -> b -> b) -> b -> b
unpackFoldrUtf8# addr f e = unpack 0# where
    unpack nh
      | ch `eqChar#` '\x00'# =  e
      | ch `leChar#` '\x7F'# =  ch `f` unpack (nh +# 1#)
      | ch `leChar#` '\xDF'# =
           (chr# (((ord# ch                                  -# 0xC0#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexCharArray# addr (nh +# 1#)) -# 0x80#))) `f`
          unpack (nh +# 2#)
      | ch `leChar#` '\xEF'# =
           (chr# (((ord# ch                                  -# 0xE0#) `uncheckedIShiftL#` 12#) +#
                    ((ord# (indexCharArray# addr (nh +# 1#)) -# 0x80#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexCharArray# addr (nh +# 2#)) -# 0x80#))) `f`
          unpack (nh +# 3#)
      | otherwise            =
           (chr# (((ord# ch                                  -# 0xF0#) `uncheckedIShiftL#` 18#) +#
                    ((ord# (indexCharArray# addr (nh +# 1#)) -# 0x80#) `uncheckedIShiftL#` 12#) +#
                    ((ord# (indexCharArray# addr (nh +# 2#)) -# 0x80#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexCharArray# addr (nh +# 3#)) -# 0x80#))) `f`
          unpack (nh +# 4#)
      where
        ch = indexCharArray# addr nh


{-# INLINE unpackFoldlUtf8# #-}
unpackFoldlUtf8# ::  (a -> Char# -> a) -> a -> ByteArray# -> Int# -> a
unpackFoldlUtf8# f e addr count = unpack 0# e where
    unpack nh e
      | nh ==# count  = e
      | ch `leChar#` '\x7F'# = let n = (f e ch) in n `seq` unpack (nh +# 1#) n
      | ch `leChar#` '\xDF'# =
           let n = f e (chr# (((ord# ch                                  -# 0xC0#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexCharArray# addr (nh +# 1#)) -# 0x80#))) in n `seq` unpack (nh +# 2#) n
      | ch `leChar#` '\xEF'# =
         let n = f e (chr# (((ord# ch                        -# 0xE0#) `uncheckedIShiftL#` 12#) +#
                    ((ord# (indexCharArray# addr (nh +# 1#)) -# 0x80#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexCharArray# addr (nh +# 2#)) -# 0x80#))) in n `seq` unpack (nh +# 3#) n
      | otherwise            =
         let n = f e (chr# (((ord# ch                        -# 0xF0#) `uncheckedIShiftL#` 18#) +#
                    ((ord# (indexCharArray# addr (nh +# 1#)) -# 0x80#) `uncheckedIShiftL#` 12#) +#
                    ((ord# (indexCharArray# addr (nh +# 2#)) -# 0x80#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexCharArray# addr (nh +# 3#)) -# 0x80#))) in n `seq` unpack (nh +# 4#) n
      where
        ch = indexCharArray# addr nh


-- Just like unsafePerformIO, but we inline it.
{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #)   -> r

