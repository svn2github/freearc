@echo off
set include=C:\Base\Compiler\WATCOM11\h
set lib=C:\Base\Compiler\WATCOM11\lib
set watcom=C:\Base\Compiler\WATCOM11
set path=C:\Base\Compiler\WATCOM11\BINNT;%path%
::wcl386 -mf -5r -oneatxh   -oi+  -DUSE_INDEX1 DICT4.CPP

C:\Base\Compiler\MinGW\bin\g++.exe -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -O2 -funroll-loops -march=i486 -mtune=pentiumpro -fomit-frame-pointer -fstrict-aliasing -ffast-math -fforce-addr %1 %2 %3 dict.cpp -odict.exe
:: -DPPMD_VERSION -DDEBUG
