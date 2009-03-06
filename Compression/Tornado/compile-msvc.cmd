@echo off
@call "C:\Program Files (x86)\Microsoft Visual Studio 9.0\VC\vcvarsall.bat"
t cl -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -Ox -GL -Gy %1 %2 %3 main.cpp user32.lib /Fetor-msvc.exe /link  /LARGEADDRESSAWARE
:: -DSTAT -DDEBUG -DFULL_COMPILE -DFREEARC_NO_TIMING
