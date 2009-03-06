@echo off
t C:\Base\Compiler\MinGW\bin\g++.exe -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER %1 %2 %3 main.cpp -otor.exe  -Xlinker --large-address-aware
:: -DSTAT -DDEBUG -DFULL_COMPILE -DFREEARC_NO_TIMING
