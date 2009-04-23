-- Build list of files/dirs in FreeArc installation for uninstall/clear before install procedures

import Data.List
import System.Cmd
import System.Directory
import System.Environment

main = do
  args <- getArgs
  let dir = case args of
              []    -> "C:\\Program Files (x86)\\FreeArc"
              dir:_ -> dir
  system$ "dir/s/b/a:-d \""++dir++"\" >files"
  system$ "dir/s/b/a:d  \""++dir++"\" >dirs"
  files <- readFile "files"
  dirs  <- readFile "dirs"
  let nobase = drop (length dir+1)
  writeFile "FreeArc-delete.nsh"$
    (unlines$ map (\f -> "Delete   \"$INSTDIR\\"++nobase f++"\"") $ sort$ lines files)++
    (unlines$ map (\f -> "RMDir    \"$INSTDIR\\"++nobase f++"\"") $ reverse$ sort$ lines dirs)
  return $! last dirs
  removeFile "files"
  removeFile "dirs"
