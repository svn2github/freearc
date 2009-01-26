set V=-alpha-2008-06-23

del FreeArc*.exe FreeArc*.zip FreeArc*.bz2 FreeArc*.arc
arc a FreeArc-console FreeArc.url Addons\ -xFreeArc.reg bin\arc.* bin\*.sfx bin\unarc.* bin\empty.arc bin\scripts\ Documentation\ -xFreeArc-GUI*.* -xDocumentation\readme.txt -z=Documentation\readme.txt -m5x -mm- -sfx=bin\freearc-tiny.sfx

"C:\Program Files\NSIS\makensis.exe"       FreeArc.nsi
"C:\Program Files\NSIS\makensis.exe" -DGTK FreeArc.nsi

ren FreeArc-install.exe FreeArc-0.50-win32%V%.exe
ren FreeArc-update.exe  FreeArc-update-0.50-win32%V%.exe
ren FreeArc-console.exe FreeArc-console-0.50-win32%V%.exe

7z -mx a FreeArc-portable-0.50-win32%V%.zip  -x!GTK* -x!FreeArc-* -x!*.nsi -x!*.nsh -x!*.cmd
cd gtk2-themes
7z -mx a ..\FreeArc-portable-0.50-win32%V%.zip
cd ..
copy /b FreeArc-portable-0.50-win32%V%.zip FreeArc-portable-update-0.50-win32%V%.zip
cd GTK
7z -mx a ..\FreeArc-portable-0.50-win32%V%.zip
cd ..

::cd ..\..\_darcs\current
::7z -mx a ..\..\Installer\!Release\FreeArc-0.50-sources%V%.zip
rmdir FreeArc-0.50-sources%V% /s/q
mkdir FreeArc-0.50-sources%V%
xcopy ..\..\_darcs\current  FreeArc-0.50-sources%V%  /e
7z a -so a.tar FreeArc-0.50-sources%V% | 7z -mx a FreeArc-0.50-sources%V%.tar.bz2 -siFreeArc-0.50-sources%V%.tar
rmdir FreeArc-0.50-sources%V% /s/q
