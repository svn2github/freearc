@echo off
@call "C:\Program Files (x86)\Microsoft Visual Studio 9.0\VC\bin\amd64\vcvarsamd64.bat"
nmake XVER=-64 clean
nmake XVER=-64
