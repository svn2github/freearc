@echo off
C:\Base\Compiler\Dev-Cpp\bin\gcc.exe -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -O3 -funroll-loops -march=i486 -mtune=pentiumpro -fomit-frame-pointer -fstrict-aliasing -ffast-math -fforce-addr tta.cpp entropy.cpp filters.cpp -DMMD_LIBRARY mmdet.cpp -otta.exe


