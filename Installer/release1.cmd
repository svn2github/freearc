::Compile all components
cd ..\Unarc
make
::upx -d *.sfx
::upx --ultra-brute *.sfx
upx --lzma -9 *.sfx
cd ..

call compile-O2.cmd
call compile-GUI-O2.cmd

cd Compression
del *.obj facompress.dll
call compile-dll.cmd

cd ..\ArcShellExt
call compile.cmd
call compile-64.cmd

cd ..\Installer
