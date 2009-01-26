{- Multithreading compression with overlapped I/O example
4x4 version 0.2 - bigger, longer, stronger ;)
* fast compression modes made even faster (direct i/o to compression buffers; larger blocks to avoid disk trashing; MM tables disabled in -2/-3 modes)
* high compression modes made even higher (256mb dictionary, saving output to temp. files)
* multi-threaded decompression with direct i/o  (lzma, увел. кол-во тредов декомпрессии при малом blocksize)
* fast text compression modes 1t..4t using grzip - now 4x4 is 10x faster than lzturbo on ENWIK9!
-}

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Array
import Data.Char
import Data.Either
import Data.IORef
import Data.List
import Data.Maybe
import Data.Word
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import System.Environment
import System.Exit
import System.IO

import CompressionLib
import Utils

-- |Abbreviations for useful compression methods
profiles = [("1t", "grzip:m4")
           ,("2t", "grzip:m3")
           ,("3t", "grzip:m2")
           ,("4t", "grzip:m1:h18")

           ,( "1", "tor:1:4m")
           ,( "2", "tor:2:8m")
           ,( "3", "tor:3:8m")
           ,( "4", "tor:4:16m")
           ,( "5", "tor:5:16m")
           ,( "6", "tor:6:32m")
           ,( "7", "tor:7:64m")

           ,( "8", "lzma:fast:128m:ht4:mc8")
           ,( "9", "lzma:128m:ht4:mc16")
           ,("10", "lzma:128m:ht4:mc32")
           ,("11", "lzma:128m:ht4:mc64")
           ,("12", "lzma:128m:ht4:mc128")
           ]

-- |Describe builtin compression profile for help display
describe (level, method)  =  "  "++level++": "++method


data Mode = COMPRESS | DECOMPRESS  deriving (Eq)

data Options = Options { workThreads :: Int
                       , ioThreads   :: Int
                       }

-- |Parse options and return Options structure and rest of cmdline arguments
parseOpt (('-':'p':n):args) opts  =  parseOpt args opts{workThreads=readInt n}
parseOpt (('-':'i':n):args) opts  =  parseOpt args opts{ioThreads=readInt n}
parseOpt ("--":args)        opts  =  (args, opts)
parseOpt (arg:args)         opts  =  parseOpt args opts.$ mapFst (arg:)
parseOpt []                 opts  =  ([], opts)


main = do
  poke compress_all_at_once 1     -- enables block-to-block compression mode in tornado and lzma
  origArgs <- getArgs
  let (args,options)  =  parseOpt origArgs Options {workThreads=0, ioThreads = -1}
  case args of
    [level] ->  do let method = level.$ changeTo profiles
                   withStdio $ compressionDriver method options

    [level,infile,outfile] ->  do
                   let method = level.$ changeTo profiles
                   withFiles infile outfile $ compressionDriver method options

    _ -> putStr ("4x4 ver. 0.3 alpha - multithreaded compressor, Nov 15 2008  (http://haskell.org/bz)\n"++
                 "\n"++
                 "Compression:   4x4 M [options] [infile outfile]\n"++
                 "                 where M is compression profile: 1..12 or 1t..4t\n"++
                 "                         or compression method: lzma:../grzip:../tor:..\n"++
                 "\n"++
                 "Decompression: 4x4 d [options] [infile outfile]\n"++
                 "\n"++
                 "Options:\n"++
                 "  -p#: number of (de)compression threads (default 4)\n"++
                 "  -i#: number of I/O threads\n"++
                 "\n"++
                 "Built-in compression profiles:\n" ++ unlines (map describe profiles))
  --
  let result=0
  exitWith (if result==0  then ExitSuccess  else ExitFailure result)


----------------------------------------------------------------------------------------------------
-- Compression/decompression driver ----------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Compression/decompression driver. Creates a pool of (de)compression threads and one writing thread
compressionDriver method Options {workThreads=workThreads, ioThreads=ioThreads} fin fout = do
  let mode       =  if method=="d"  then DECOMPRESS  else COMPRESS
      blockSize  =  getDictionary method .$i
  numThreads <- case workThreads of
                  0 -> return 4  -- todo: autodetect
                  _ -> return workThreads
  let numBufs  =  numThreads + case () of  -- determine number of additional threads used only for i/o operations
                    _ | ioThreads >= 0                     ->  ioThreads   -- explicitly specified by -i# option
                      | mode == DECOMPRESS                 ->  2           -- maximum that can be used in decompression mode without overflowing 2gb memory when 128 mb data blocks are used
                      | getCompressionMem method > 200*mb  ->  1           -- it's the maximum we can use with highest compresssion modes
                      | otherwise                          ->  4           -- for smaller blocks use more i/o threads
  seqChan <- newChan   -- used to order write operations in the same order as data was read
  fin <- newMVar fin   -- fin locked until write operation slot will be sent into seqChan
  compressionThreadsSem <- newQSemN 0     -- used to postpone program finishing until all compression threads (and C compression functions) will be finished
  compressorsSem <- newQSem numThreads    -- number of threads that are allowed to perform actual (de)compression simultaneously
  replicateM_ numBufs (forkOS$ compressionThread mode method blockSize fin fout seqChan compressorsSem compressionThreadsSem)
  writingThread seqChan
  waitQSemN compressionThreadsSem numBufs


-- |Single compression thread: reads input blocks, compress them and sends compressed data to outfile
compressionThread COMPRESS  method blockSize fin fout seqChan compressorsSem compressionThreadsSem = do
  chan <- newChan   -- channel used to send write commands to the writing thread
  bytes' <- ref 0   -- size of last input block
  repeat_until $ do   -- repeat until EOF (bytes==0)
    let reader buf size = do bytes <- withMVar fin $ \fin -> do
                               bytes <- hGetBuf fin buf (min size blockSize)
                               writeChan seqChan (chan,bytes)
                               return bytes
                             bytes' =: bytes
                             when (bytes>0) $ waitQSem compressorsSem    -- wait for signal to start compression itself
                             return (bytes ||| -1)    -- on EOF returns -1 in order to immediately stop compression
    --
    let writer buf size = do signalQSem compressorsSem  -- signal that compression finished
                             sem <- newQSem 0  -- used to pause execution until data will be written
                             writeChan chan $ \origsize -> do
                               -- Put header before compressed data: origsize+compsize+method
                               let compsize  =  length method + 1 + size
                               with (i origsize::Word32) $ \header -> hPutBuf fout header 4
                               with (i compsize::Word32) $ \header -> hPutBuf fout header 4
                               hPutStr0 fout method
                               --
                               hPutBuf fout buf size
                               signalQSem sem
                             waitQSem sem
                             return size
    --
    compress method (makeCallback reader writer)
    val bytes' >>== (==0)
  signalQSemN compressionThreadsSem 1


-- |Single decompression thread: reads compressed blocks, decompress them and sends decompressed data to outfile
compressionThread DECOMPRESS method blockSize finMVar fout seqChan compressorsSem compressionThreadsSem = do
  chan <- newChan   -- channel used to send write commands to the writing thread
  repeat_until $ do   -- repeat until EOF (x==0)
    fin <- takeMVar finMVar
    allocaArray 2 $ \header -> do
    x <- hGetBuf fin header 8
    when (x>0) $ do
      [origsize,compsize]  <-  peekArray 2 header
      method <- hGetStr0 fin
      remainder' <- ref (compsize-length method-1)
      --
      let reader buf size = do remainder <- val remainder'
                               bytes <- hGetBuf fin buf (min remainder size)
                               when (remainder>0 && remainder==bytes) $ do  -- Now we've read all input data
                                 writeChan seqChan (chan,13)   -- this should be done before releasing fin!
                                 putMVar finMVar fin           -- now fin may be used in other threads
                                 waitQSem compressorsSem       -- no more than T threads should be busy in actual compression
                               remainder' -= bytes
                               return bytes
      --
      let writer buf size = do signalQSem compressorsSem   -- signal that this thread was finished compression
                               sem <- newQSem 0            -- used to pause execution until buffer contents will be written to disk
                               writeChan chan $ \13 -> do
                                 hPutBuf fout buf size
                                 signalQSem sem
                               waitQSem sem
                               return size
      --
      decompress method (makeCallback reader writer)
      return ()
    when   (x==0) $ putMVar finMVar fin
    return (x==0)
  writeChan seqChan (chan,0)
  signalQSemN compressionThreadsSem 1


-- |The writing thread sequences write operations so they are performed in the same order as data was read from input stream
writingThread seqChan = do
  repeat_until $ do   -- repeat until EOF
    (chan,origsize) <- readChan seqChan
    when (origsize>0) $ do
      cmd <- readChan chan
      cmd origsize
    return (origsize==0)


----------------------------------------------------------------------------------------------------
-- Auxiliary functions -----------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- |Perform action with stdin/stdout handles set to binary mode
withStdio action = do
  hSetBinaryMode stdin  True
  hSetBinaryMode stdout True
  action stdin stdout

-- |Perform action with files specified
withFiles infile outfile action = do
  fin  <- openBinaryFile infile  ReadMode
  fout <- openBinaryFile outfile WriteMode
  action fin fout

-- |Make callback function from reader & writer funcs
makeCallback reader writer "read"  buf size _  =  reader buf size
makeCallback reader writer "write" buf size _  =  writer buf size
makeCallback reader writer _       _   _    _  =  return 0

-- |Write string + NULL char
hPutStr0 h str  =  hPutStr h (str++[chr 0])

-- |Read NULL-terminated string
hGetStr0 h  =  go ""
  where go cs = do c <- hGetChar h
                   case c of
                     '\0' -> return (reverse cs)
                     _    -> go (c:cs)

