
{-# OPTIONS_GHC -fglasgow-exts #-}

module Main where
import qualified Scripting.Lua as Lua
import Foreign.C.Types
import qualified Foreign.Storable as F
import qualified Foreign.StablePtr as F
import qualified Foreign.Ptr as F
import Data.List
import Control.Monad
import Data.IORef
import Data.Maybe
import Scripting.Lua

foreign export ccall hs_lua_c_func :: Lua.LuaState -> IO CInt
foreign import ccall "&hs_lua_c_func" hs_lua_c_func_addr :: F.FunPtr Lua.LuaCFunction

hs_lua_c_func l = do
    putStrLn "from haskell"
    return 0

test1 :: Int -> String -> IO Int
test1 a b = do
    putStrLn ("Got " ++ show a ++ " " ++ b)
    return 15

test2 :: IO Int
test2 = do
    fail "expected error"
    return 15


main = do
    l <- Lua.newstate
    Lua.openlibs l
    Lua.getglobal l "print"
    Lua.pushstring l "from print"
    putStrLn "NEXT: from print"
    Lua.call l 1 0
    Lua.registercfunction l "kill" hs_lua_c_func_addr
    Lua.getglobal l "kill"
    putStrLn "NEXT: from haskell"
    Lua.call l 0 0
    c <- Lua.loadstring l "print \"preparation\"; function f () print \"in lua def fun\"; end" "inline code"
    putStrLn "NEXT: result 0"
    putStrLn ("result " ++ show c)
    putStrLn "NEXT: preparation"
    Lua.call l 0 Lua.multret
    Lua.getglobal l "f"
    putStrLn "NEXT: in lua def fun"
    Lua.call l 0 0
    Lua.getglobal l "print"
    Lua.push l (42::Int)
    putStrLn "NEXT: 42"
    Lua.call l 1 0
    Lua.push l (42::Int)
    putStrLn "NEXT: number 42"
    Just (x::Int) <- Lua.peek l (-1)
    putStrLn ("number " ++ show x)
    putStrLn "NEXT: string 42"
    Just (x::String) <- Lua.peek l (-1)
    putStrLn ("string " ++ x)
    Lua.pop l 1     

    putStrLn "NEXT: "
    Lua.callproc l "print"
    putStrLn "NEXT: arg1"
    Lua.callproc l "print"  "arg1"
    putStrLn "NEXT: arg1    arg2"
    Lua.callproc l "print" "arg1" "arg2"

    putStrLn $ "NEXT: " ++ show (sin 2)
    Lua.getglobal l "math"
    Lua.getfield l (-1) "sin"
    Lua.gettop l >>= \n -> Lua.remove l (n-1)
    Lua.push l (2.0::Double)
    Lua.call l 1 1
    Just (x::Double) <- Lua.peek l (-1)
    print x
    Lua.pop l 1

    putStrLn $ "NEXT: " ++ show (sin 3)
    Lua.getglobal2 l "math.sin"
    Lua.push l (3.0::Double)
    Lua.call l 1 1
    Just (x::Double) <- Lua.peek l (-1)
    print x
    Lua.pop l 1

    putStrLn $ "NEXT: " ++ show (sin 1)
    (x::Double) <- Lua.callfunc l "math.sin" (1.0::Double)
    print x

    putStrLn $ "NEXT: " ++ show (sin 5)
    (x::Double) <- Lua.callfunc l "math.sin" (5.0::Double) (6.0::Double) (7.0::Double)
    print x

    v <- Lua.newcfunction test1
    Lua.push l v
    Lua.setglobal l "test1"

    putStrLn "NEXT: Got 1 test1"
    Lua.callproc l "test1"  (1.0::Double) "test1"

    Lua.freecfunction v

    Lua.pushfunction l test1
    Lua.setglobal l "test2"

    putStrLn "NEXT: Got 2 test2 ex"
    (m :: Int) <- Lua.callfunc l "test2"  (2.0::Double) "test2 ex"
    putStrLn "NEXT: 15"
    print m

    putStrLn "NEXT: Error ignored"
    Lua.callproc l "math.sin" "notanumber" `catch` (\e -> putStrLn ("Error caught " ++ show e))

    putStrLn "NEXT: Error ignored 2"
    Lua.callproc l "test2" "notanumber" "notanumber" `catch` (\e -> putStrLn ("Error caught 2 " ++ show e))

    Lua.pushfunction l test2
    Lua.setglobal l "testexception"
    putStrLn "NEXT: user error (user error (expected error))"
    Lua.callproc l "testexception" `catch` (\e -> print e)

    putStrLn "NEXT: 123"
    Lua.dostring l "return 123"
    Just (k::Int) <- Lua.peek l (-1)
    Lua.pop l 1
    print k

    putStrLn "NEXT: [123,345]"
    Lua.push l [123::Int,345]
    Just (k::[Int]) <- Lua.peek l (-1)
    Lua.pop l 1
    print k

    putStrLn "NEXT: [123,345]"
    Lua.dostring l "return {123,345}"
    Just (k::[Int]) <- Lua.peek l (-1)
    Lua.pop l 1
    print k

    putStrLn "NEXT: [(1,123),(2,345),(42,42)]"
    Lua.dostring l "return {123,345,[42]=42}"
    Just (k::[(Int,Int)]) <- Lua.peek l (-1)
    Lua.pop l 1
    print k

    putStrLn "NEXT: [(1,123),(2,345),(42,42)]"
    Lua.push l [(1::Int,123::Int),(2,345),(42,42)]
    Just (k::[(Int,Int)]) <- Lua.peek l (-1)
    Lua.pop l 1
    print k

    putStrLn "NEXT: 0"
    k <- Lua.gettop l
    print k


    Lua.close l

