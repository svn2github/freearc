@set exe=4x4.exe
@set ctempdir=c:\temp\out\FreeArc
@set gcc=C:\Base\Compiler\ghc\gcc -BC:\Base\Compiler\ghc\gcc-lib -IC:\Base\Compiler\ghc\include\mingw
@set defines=-DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER
@rem ******** -DFREEARC_UNIX -DFREEARC_MOTOROLA_BYTE_ORDER -DFREEARC_ONLY_ALIGNED_ACCESS -DFREEARC_PACKED_STRINGS *******
@mkdir %ctempdir%
@cd ..
@call compile
@cd _Examples
@set c_modules=%ctempdir%/Environment.o %ctempdir%/Common.o %ctempdir%/CompressionLibrary.o %ctempdir%/C_PPMD.o %ctempdir%/C_LZP.o %ctempdir%/C_LZMA.o %ctempdir%/C_BCJ.o %ctempdir%/C_GRZip.o %ctempdir%/C_Dict.o %ctempdir%/C_REP.o %ctempdir%/C_MM.o %ctempdir%/C_TTA.o %ctempdir%/C_Tornado.o %ctempdir%/C_Delta.o %ctempdir%/C_External.o
@set options=-lstdc++ -s -Xlinker --large-address-aware
%gcc% %1 4x4.cpp %options% %defines% %c_modules% -o %exe%

