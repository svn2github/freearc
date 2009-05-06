@echo off
@call "C:\Program Files (x86)\Microsoft Visual Studio 9.0\VC\vcvarsall.bat"
nmake clean
nmake
regsvr32 /s /c ArcShellExt.dll
::start C:\Base\Tools\ARC\7-ZIP4_58\7zFM.exe
