include ../../common.mak

ALL: $(TEMPDIR)/C_LZMA.o $(TEMPDIR)/C_BCJ.o

CODE_FLAGS = -fno-rtti -Wall \
                -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion
OPT_FLAGS   = -O2 -march=i486 -mtune=pentiumpro \
              -fomit-frame-pointer -fstrict-aliasing \
              -ffast-math -fforce-addr -funroll-loops
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(DEFINES)

$(TEMPDIR)/C_LZMA.o: C_LZMA.cpp C_LZMA.h makefile 7zip/Compress/LZ/BinTree/BinTreeMain.h 7zip/Compress/LZMA/LZMAEncoder.cpp
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(TEMPDIR)/C_BCJ.o: C_BCJ.cpp C_BCJ.h makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<
