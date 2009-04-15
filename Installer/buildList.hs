-- Build list of files/dirs in FreeArc installation for uninstall/clear before install procedures

import Data.List
import System.Cmd
import System.Directory

main = do
  system "dir/s/b/a:-d \"C:\\Program Files (x86)\\FreeArc\" >files"
  system "dir/s/b/a:d  \"C:\\Program Files (x86)\\FreeArc\" >dirs"
  files <- readFile "files"
  dirs  <- readFile "dirs"
  let nobase = drop (length "\"C:\\Program Files (x86)\\FreeArc")
  writeFile "FreeArc-delete.nsh"$
    (unlines$ map (\f -> "Delete   \"$INSTDIR\\"++nobase f++"\"") $ sort$ lines files)++
    (unlines$ map (\f -> "RMDir    \"$INSTDIR\\"++nobase f++"\"") $ reverse$ sort$ lines dirs)
  return $! last dirs
  removeFile "files"
  removeFile "dirs"
