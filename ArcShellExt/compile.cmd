path C:\Base\Compiler\ghc;C:\Base\Compiler\MinGW\bin;%PATH%
set GHCDIR=C:\Base\Compiler\ghc
set LIBDIR=%GHCDIR%\gcc-lib
set INCDIR=%GHCDIR%\include\mingw
set GCC=%GHCDIR%\gcc.exe -B%LIBDIR% -I%INCDIR% -D_UNICODE -DUNICODE
set DLLWRAP=%GHCDIR%\gcc-lib\dllwrap.exe -B%LIBDIR%
set WINDRES=%GHCDIR%\windres.exe
set LIBS=-lcomctl32 -lshell32 -lole32 -luuid
%gcc% -Os -c %* ArcShellExt.cpp
%WINDRES% -I..\Unarc\gui ArcShellExt.rc ArcShellExtRes.o
%DLLWRAP% --driver-name g++ ArcShellExt.o ArcShellExtRes.o Lua\liblua.a -lstdc++ %LIBS% -def ArcShellExt.def -s -o ArcShellExt.dll

