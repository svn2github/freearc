::set LUA_PATH=..\?.lua
::E:\temp\sources\lua-5.1.4\src\lua.exe example.lua

C:\Base\Compiler\MinGW\bin\g++.exe -I.. cpp-example.cpp -ocpp-example.exe && cpp-example.exe

cd ..
hsc2hs tabi.hsc
cd Examples
ghc -i.. -optc-I.. -lstdc++ --make hs-example.hs cpp-server.cpp && hs-example.exe
