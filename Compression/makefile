include ../common.mak

ALL: $(TEMPDIR)/Common.o $(TEMPDIR)/CompressionLibrary.o

CODE_FLAGS = -fno-exceptions -fno-rtti \
                -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion
OPT_FLAGS = -Os -march=i486 -mtune=pentiumpro -fomit-frame-pointer \
                 -ffast-math -fforce-addr -fstrict-aliasing
DEBUG_FLAGS = -g0
CFLAGS = -c $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(DEFINES)

$(TEMPDIR)/Common.o: Common.cpp Common.h Compression.h makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(TEMPDIR)/CompressionLibrary.o: CompressionLibrary.cpp Common.h Compression.h makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<
