TARGETOS = BOTH
!include <win32.mak>
# $Id: Makefile 497 2008-12-07 03:17:37Z andre $

lflags  = /INCREMENTAL:NO /RELEASE /NOLOGO
dlllflags = $(lflags) -entry:_DllMainCRTStartup$(DLLENTRY) -dll

all: ArcShellExt.dll

ArcShellExt.dll: ArcShellExt.obj ArcShellExt.res
	$(implib) -machine:$(CPU) -def:ArcShellExt.def $** -out:ArcShellExt.lib
	$(link) $(dlllflags) -base:0x1C000000 -out:$*.dll $** $(olelibsdll) shell32.lib ArcShellExt.lib comctl32.lib ArcShellExt.exp Lua\lua5.1.lib

.cpp.obj:
	$(cc) $(cflags) $*.cpp

ArcShellExt.res: ArcShellExt.rc
	$(rc) $(rcflags) $(rcvars) ArcShellExt.rc

clean:
	-1 del ArcShellExt.dll ArcShellExt.lib ArcShellExt.dll.manifest ArcShellExt.obj ArcShellExt.exp ArcShellExt.res

zip:
	-1 del *.zip
	perl abpack.pl