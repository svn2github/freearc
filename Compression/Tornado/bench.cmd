@rem Please note that mode 8 requires 2gb RAM and modes 9..11 require 4gb RAM and 64-bit Windows
@echo %1 %2 %3
@copy /b %1 nul >nul
@copy /b %2 nul >nul
@copy /b %3 nul >nul
@for %%a in (1 2 3 4 5 6 7 8 9 10 11) do @C:\!\FreeArchiver\Compression\TORNADO\tor.exe -%%a -o -qh -cpu %1 %2 %3
:: >>d:\res