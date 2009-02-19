@echo off
t C:\Base\Compiler\MinGW\bin\g++.exe -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -O3 -funroll-loops -fno-exceptions -fno-rtti -fomit-frame-pointer -fstrict-aliasing -ffast-math -fforce-addr %1 %2 %3 main.cpp -otor.exe  -s -Xlinker --large-address-aware
:: -DSTAT -DDEBUG -DFREEARC_NO_TIMING
