import System.Environment
import System.Exit
import System.IO
import CompressionLib

main = do
  hSetBinaryMode stdin  True
  hSetBinaryMode stdout True
  let write_stdout buf size  =  hPutBuf stdout buf size  >>  return size
  args <- getArgs
  (result,time) <- case args of
    ["d"]    -> decompressWithHeader        (hGetBuf stdin) write_stdout
    [method] ->   compressWithHeader method (hGetBuf stdin) write_stdout
    _        -> putStrLn ("Usage: Compressor method <infile >outfile\n"++
                          "       Compressor d      <infile >outfile")  >> return (1,0)
  exitWith (if result==0  then ExitSuccess  else ExitFailure result)
