cd arc.languages
call tr.cmd
cd ..

del !Release /s/q
mkdir !Release
mkdir !Release\Addons
mkdir !Release\bin
mkdir !Release\bin\arc.languages
mkdir !Release\Documentation
mkdir !Release\GTK
mkdir !Release\gtk2-themes

xcopy Addons        !Release\Addons        /e
xcopy bin           !Release\bin           /e
xcopy Documentation !Release\Documentation /e
xcopy GTK           !Release\GTK           /e
xcopy gtk2-themes   !Release\gtk2-themes   /e
xcopy arc.languages\arc.languages  !Release\bin\arc.languages        /e

xcopy ..\Documentation\History.txt          !Release\Addons
xcopy ..\Unarc\FreeArc.fmt                 "!Release\Addons\FAR MultiArc plugin"
xcopy ..\Documentation\FreeArc036-eng.htm   !Release\Documentation
xcopy ..\Documentation\FreeArc040-rus.htm   !Release\Documentation
xcopy ..\Documentation\FreeArc-GUI-Eng.htm  !Release\Documentation
xcopy ..\Documentation\FreeArc-GUI-Rus.htm  !Release\Documentation
xcopy ..\Tests\Arc.exe                      !Release\bin
xcopy ..\Tests\FreeArc.exe                  !Release\bin
xcopy ..\Unarc\unarc.exe                    !Release\bin
xcopy ..\Unarc\*.sfx                        !Release\bin
xcopy ..\Compression\facompress.dll         !Release\bin
xcopy release3.cmd                          !Release
xcopy *.nsi                                 !Release
xcopy *.nsh                                 !Release
xcopy FreeArc.url                           !Release

cd    !Release
