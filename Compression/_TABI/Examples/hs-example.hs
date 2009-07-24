{-# OPTIONS -fglasgow-exts #-}
import Foreign
import Foreign.C

import qualified TABI
infixr 7  ==>
a ==> b = TABI.Pair a b


-- |Server function implemented in C++
foreign import ccall safe ""  server :: TABI.C_FUNCTION

-- |Client function calling server()
client = TABI.call server ["i" ==> (1::Int), "s" ==> "str", "callback" ==> (callback :: TABI.FUNCTION)]

-- |Functiom called back from server()
callback p = do
  n       <- TABI.required p "n"
  name    <- TABI.optional p "name"    "-"
  missing <- TABI.optional p "missing" "-"
  print (n::Int, name::String, missing::String)
  return 0


main = client

