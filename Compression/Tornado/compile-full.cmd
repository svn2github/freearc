@echo off
t C:\Base\Compiler\MinGW\bin\g++.exe -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -DFULL_COMPILE -O3 --param inline-unit-growth=999 -funroll-loops -fno-exceptions -fno-rtti -march=i486 -mtune=pentiumpro -fomit-frame-pointer -fstrict-aliasing -ffast-math -fforce-addr %1 %2 %3 main.cpp -otor-full.exe  -s -Xlinker --large-address-aware
:: -DSTAT -DDEBUG -DFREEARC_NO_TIMING

