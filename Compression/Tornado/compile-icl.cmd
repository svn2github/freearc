@echo off
@call "C:\Program Files (x86)\Intel\Compiler\11.0\066\cpp\bin\iclvars.bat" ia32
t icl -W0 -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -O3 -Gy -Gr -GL -Qipo -Qinline-factor999 %1 %2 %3 main.cpp user32.lib /Fetor-icl.exe /link  /LARGEADDRESSAWARE
:: -DSTAT -DDEBUG -DFULL_COMPILE -DFREEARC_NO_TIMING
