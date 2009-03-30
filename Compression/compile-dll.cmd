@echo off
@call "C:\Program Files (x86)\Microsoft Visual Studio 9.0\VC\vcvarsall.bat"
@call "C:\Program Files (x86)\Intel\Compiler\11.0\066\cpp\bin\iclvars.bat" ia32
@make -fmakefile-dll
@del facompress.exp facompress.lib