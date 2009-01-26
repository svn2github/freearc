windres -I. main.rc res.o
C:\Base\Compiler\ghc\g++.exe -IC:\Base\Compiler\ghc\include\mingw -BC:\Base\Compiler\ghc\gcc-lib -s -mwindows -DWIN32 -D_WINDOWS -D_UNICODE -DUNICODE gui.cpp main.cpp res.o -lcomctl32 -lole32 -lstdc++ -o gui.exe
