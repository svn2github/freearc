@set exe=4x4.exe
@set ctempdir=c:\temp\out\FreeArc
@set defines=-DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -optc-DFREEARC_WIN -optc-DFREEARC_INTEL_BYTE_ORDER
@rem ******** -DFREEARC_UNIX -DFREEARC_MOTOROLA_BYTE_ORDER -DFREEARC_ONLY_ALIGNED_ACCESS -DFREEARC_PACKED_STRINGS *******
@mkdir %ctempdir% >nul 2>nul
@cd ..
@call compile
@cd _Examples
@set c_modules=%ctempdir%/Common.o %ctempdir%/CompressionLibrary.o %ctempdir%/C_LZMA.o %ctempdir%/C_GRZip.o %ctempdir%/C_Tornado.o %ctempdir%/C_External.o -lstdc++ -optl-s -optl-Xlinker -optl--large-address-aware
@set options=-fglasgow-exts -cpp -i.. -i../.. -threaded
ghc.exe --make %1 4x4.hs %options% %defines% %c_modules% -odir %ctempdir%%1 -hidir %ctempdir%%1 -o %exe% -H20m
@del ..\CompressionLib_stub.? >nul 2>nul
