:: Directory where 7-zip is installed
set szip=C:\!\FreeArchiver\Installer\arc.languages

ghc -O2 --make -optl-s 7tr.hs
upx 7tr.exe
mkdir arc.languages
del /q arc.languages\*.*
cd arc.languages
for %%a in (%szip%\Lang\*.txt) do ..\7tr ..\arc.russian.txt %%a
..\7tr.exe ..\arc.russian.txt %szip%\Lang\de.txt ..\arc.german.txt
..\7tr.exe ..\arc.russian.txt %szip%\Lang\cs.txt ..\arc.czech.txt
..\7tr.exe ..\arc.russian.txt %szip%\Lang\fr.txt ..\arc.french.txt
..\7tr.exe ..\arc.russian.txt %szip%\Lang\it.txt ..\arc.italian.txt
..\7tr.exe ..\arc.portuguese_brazil.txt %szip%\Lang\pt-br.txt ..\arc.portuguese_brazil.txt
..\7tr.exe ..\arc.russian.txt %szip%\Lang\zh-cn.txt ..\arc.chinese_simplified.txt
..\7tr.exe ..\arc.russian.txt %szip%\Lang\zh-tw.txt ..\arc.chinese_traditional.txt
..\7tr.exe ..\arc.russian.txt %szip%\Lang\nl.txt ..\arc.dutch.txt
..\7tr.exe ..\arc.russian.txt %szip%\Lang\es.txt ..\arc.spanish.txt
..\7tr.exe ..\arc.russian.txt %szip%\Lang\uk.txt ..\arc.ukrainian.txt
..\7tr.exe ..\arc.russian.txt %szip%\Lang\pl.txt ..\arc.polish.txt
..\7tr.exe ..\arc.russian.txt %szip%\Lang\hu.txt ..\arc.hungarian.txt
..\7tr.exe ..\arc.russian.txt nul ..\arc._New.txt
copy ..\arc.russian.txt
copy ..\arc.english.txt
:: Temporary - it has more tooltips than english version!
::copy ..\arc.german.txt
cd ..
