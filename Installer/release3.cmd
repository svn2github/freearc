set VER=0.51
::set V=-alpha-2009-04-19

del FreeArc*.exe FreeArc*.zip FreeArc*.bz2 FreeArc*.arc
arc a FreeArc-console FreeArc.url Addons\ -xFreeArc.reg bin\arc.* bin\facompress.dll bin\*.sfx bin\unarc.* bin\empty.arc bin\scripts\ Documentation\ -xFreeArc-GUI*.* -xDocumentation\readme.txt -z=Documentation\readme.txt -m5x -mm- -sfx=bin\freearc-tiny.sfx

"C:\Program Files (x86)\NSIS\makensis.exe"       FreeArc.nsi
"C:\Program Files (x86)\NSIS\makensis.exe" -DGTK FreeArc.nsi

ren FreeArc-install.exe FreeArc-%VER%-win32%V%.exe
ren FreeArc-update.exe  FreeArc-update-%VER%-win32%V%.exe
ren FreeArc-console.exe FreeArc-console-%VER%-win32%V%.exe

7z -mx a FreeArc-portable-%VER%-win32%V%.zip  -x!GTK* -x!FreeArc-* -x!*.nsi -x!*.nsh -x!*.cmd
cd gtk2-themes
7z -mx a ..\FreeArc-portable-%VER%-win32%V%.zip
cd ..
copy /b FreeArc-portable-%VER%-win32%V%.zip FreeArc-portable-update-%VER%-win32%V%.zip
cd GTK
7z -mx a ..\FreeArc-portable-%VER%-win32%V%.zip
cd ..

::cd ..\..\_darcs\current
::7z -mx a ..\..\Installer\!Release\FreeArc-%VER%-sources%V%.zip
rmdir FreeArc-%VER%-sources%V% /s/q
mkdir FreeArc-%VER%-sources%V%
xcopy ..\..\_darcs\current  FreeArc-%VER%-sources%V%  /e
7z a -so a.tar FreeArc-%VER%-sources%V% | 7z -mx a FreeArc-%VER%-sources%V%.tar.bz2 -siFreeArc-%VER%-sources%V%.tar
rmdir FreeArc-%VER%-sources%V% /s/q
