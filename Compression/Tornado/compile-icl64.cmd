@echo off
@call "C:\Program Files (x86)\Intel\Compiler\11.0\066\cpp\bin\iclvars.bat" intel64
t icl -DFREEARC_64BIT -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -O2 -Gy %1 %2 %3 main.cpp user32.lib /Fetor-icl64.exe /link  /LARGEADDRESSAWARE
:: -DSTAT -DDEBUG -DFULL_COMPILE -DFREEARC_NO_TIMING
