@echo off
@call "C:\Program Files (x86)\Microsoft Visual Studio 9.0\VC\bin\amd64\vcvarsamd64.bat"
t cl -DFREEARC_64BIT -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -O2 -Gy %1 %2 %3 main.cpp user32.lib /Fetor-msvc64.exe /link  /LARGEADDRESSAWARE
:: -DSTAT -DDEBUG -DFULL_COMPILE -DFREEARC_NO_TIMING
