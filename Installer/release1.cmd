cd ..\Unarc
make
upx -9 --lzma arc*.linux.sfx
cd ..
call compile-O2.cmd
call compile-GUI-O2.cmd
cd Compression
call compile-dll.cmd
cd ..\Installer
