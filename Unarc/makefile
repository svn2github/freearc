include ../common.mak

windows: unarc.exe arc.sfx freearc.sfx freearc-installer.sfx FreeArc.fmt
linux: unarc arc.linux.sfx

Extractor_DEFINES = -DFREEARC_DECOMPRESS_ONLY -D_NO_EXCEPTIONS -DUNARC
OBJDIR  = $(TEMPDIR)-unarc
LINKOBJ_FAR_PLUGIN = $(OBJDIR)/Environment.o $(OBJDIR)/Common.o $(OBJDIR)/CompressionLibrary.o \
                     $(OBJDIR)/C_LZMA.o
LINKOBJ_TINY = $(LINKOBJ_FAR_PLUGIN) $(OBJDIR)/C_BCJ.o $(OBJDIR)/C_Dict.o $(OBJDIR)/C_Delta.o
LINKOBJ_MINI = $(LINKOBJ_TINY) $(OBJDIR)/C_REP.o $(OBJDIR)/C_LZP.o $(OBJDIR)/C_PPMD.o
LINKOBJ = $(LINKOBJ_MINI) $(OBJDIR)/C_MM.o $(OBJDIR)/C_TTA.o \
          $(OBJDIR)/C_Tornado.o $(OBJDIR)/C_External.o $(OBJDIR)/C_GRZip.o
HEADERS =  ../Compression/Compression.h ../Compression/Common.h
#makefile
LINK_FLAGS = -L$(LIBDIR) -Xlinker --large-address-aware -lstdc++ -s
UNIX_LINK_FLAGS = -L$(LIBDIR) -lstdc++ -lrt -lpthread -s


CODE_FLAGS  = -fno-exceptions -fno-rtti -Wall \
              -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion
OPT_FLAGS   = -march=i486 -mtune=pentiumpro -ffast-math \
              -fomit-frame-pointer -fstrict-aliasing \
              -fforce-addr
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) -O2 $(DEBUG_FLAGS) $(DEFINES) $(Extractor_DEFINES)
CFLAGS1= $(CODE_FLAGS) $(OPT_FLAGS) -O1 $(DEBUG_FLAGS) $(DEFINES) $(Extractor_DEFINES)


unarc.exe: $(OBJDIR)/unarc.o $(LINKOBJ)
	$(GCC) $< $(LINKOBJ) $(LINK_FLAGS) -o unarc.exe

arc.sfx: $(OBJDIR)/ArcSFX.o $(LINKOBJ)
	$(GCC) $< $(LINKOBJ)      $(LINK_FLAGS) -o arc.sfx
	$(GCC) $< $(LINKOBJ_MINI) $(LINK_FLAGS) -o arc-mini.sfx
	$(GCC) $< $(LINKOBJ_TINY) $(LINK_FLAGS) -o arc-tiny.sfx
	upx.exe -9 --lzma --compress-icons=2 arc.sfx arc-mini.sfx arc-tiny.sfx

freearc.sfx: $(OBJDIR)/FreeArcSFX.o $(LINKOBJ)
	$(WINDRES) -Igui gui\main.rc gui\res.o
	$(GCC) $< $(LINKOBJ)      $(LINK_FLAGS) gui\res.o -lcomctl32 -mwindows -o freearc.sfx
	$(GCC) $< $(LINKOBJ_MINI) $(LINK_FLAGS) gui\res.o -lcomctl32 -mwindows -o freearc-mini.sfx
	$(GCC) $< $(LINKOBJ_TINY) $(LINK_FLAGS) gui\res.o -lcomctl32 -mwindows -o freearc-tiny.sfx
	upx.exe -9 --lzma --compress-icons=2 freearc.sfx freearc-mini.sfx freearc-tiny.sfx

freearc-installer.sfx: $(OBJDIR)/FreeArcInstaller.o $(LINKOBJ)
	$(WINDRES) -Igui gui\main.rc gui\res.o
	$(GCC) $< $(LINKOBJ)      $(LINK_FLAGS) gui\res.o -lcomctl32 -mwindows -o freearc-installer.sfx
	upx.exe -9 --lzma --compress-icons=2 freearc-installer.sfx

FreeArc.fmt: $(OBJDIR)/FarPlugin.o FarPlugin.def $(LINKOBJ_FAR_PLUGIN)
	$(DLLWRAP) --driver-name g++ $< $(LINKOBJ_FAR_PLUGIN) -lstdc++ -def FarPlugin.def -s -o FreeArc.fmt


unarc: $(OBJDIR)/unarc.o $(LINKOBJ)
	$(GCC) $< $(LINKOBJ) $(UNIX_LINK_FLAGS) -o unarc

arc.linux.sfx: $(OBJDIR)/ArcSFX.o $(LINKOBJ)
	$(GCC) $< $(LINKOBJ)      $(UNIX_LINK_FLAGS) -o arc.linux.sfx
	$(GCC) $< $(LINKOBJ_MINI) $(UNIX_LINK_FLAGS) -o arc-mini.linux.sfx
	$(GCC) $< $(LINKOBJ_TINY) $(UNIX_LINK_FLAGS) -o arc-tiny.linux.sfx
#	upx -9 --lzma arc*.linux.sfx


$(OBJDIR)/FarPlugin.o: FarPlugin.cpp ArcStructure.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -IHeaders.c -o $*.o $<

$(OBJDIR)/unarc.o: unarc.cpp ArcStructure.h CUI.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/ArcSFX.o: unarc.cpp ArcStructure.h CUI.h $(HEADERS)
	$(GCC) -c $(CFLAGS1) -DFREEARC_SFX -o $*.o $<

$(OBJDIR)/FreeArcSFX.o: unarc.cpp ArcStructure.h gui\gui.h gui\gui.cpp $(HEADERS)
	$(GCC) -c $(CFLAGS1) -DFREEARC_SFX -DFREEARC_GUI -Igui -o $*.o $<

$(OBJDIR)/FreeArcInstaller.o: unarc.cpp ArcStructure.h gui\gui.h gui\gui.cpp $(HEADERS)
	$(GCC) -c $(CFLAGS1) -DFREEARC_SFX -DFREEARC_INSTALLER -DFREEARC_GUI -Igui -o $*.o $<


$(OBJDIR)/Environment.o: ../Environment.cpp ../Environment.h $(HEADERS)
	$(GCC) -c $(CFLAGS1) -o $*.o $<

$(OBJDIR)/CompressionLibrary.o: ../Compression/CompressionLibrary.cpp $(HEADERS)
	$(GCC) -c $(CFLAGS1) -o $*.o $<

$(OBJDIR)/Common.o: ../Compression/Common.cpp $(HEADERS)
	$(GCC) -c $(CFLAGS1) -o $*.o $<


$(OBJDIR)/C_BCJ.o: ../Compression/LZMA/C_BCJ.cpp ../Compression/LZMA/C_BCJ.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_LZMA.o: ../Compression/LZMA/C_LZMA.cpp ../Compression/LZMA/C_LZMA.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_Dict.o: ../Compression/Dict/C_Dict.cpp ../Compression/Dict/C_Dict.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_Delta.o: ../Compression/Delta/C_Delta.cpp ../Compression/Delta/C_Delta.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_REP.o: ../Compression/REP/C_REP.cpp ../Compression/REP/C_REP.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_LZP.o: ../Compression/LZP/C_LZP.cpp ../Compression/LZP/C_LZP.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_PPMD.o: ../Compression/PPMD/C_PPMD.cpp ../Compression/PPMD/C_PPMD.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -O1 -o $*.o $<

$(OBJDIR)/C_MM.o: ../Compression/MM/C_MM.cpp ../Compression/MM/C_MM.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_TTA.o: ../Compression/MM/C_TTA.cpp ../Compression/MM/C_TTA.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_Tornado.o: ../Compression/Tornado/C_Tornado.cpp ../Compression/Tornado/C_Tornado.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_External.o: ../Compression/External/C_External.cpp ../Compression/External/C_External.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_GRZip.o: ../Compression/GRZip/C_GRZip.cpp ../Compression/GRZip/C_GRZip.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<
