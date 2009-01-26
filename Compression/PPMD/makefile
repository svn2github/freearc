include ../../common.mak

ALL: $(TEMPDIR)/C_PPMD.o

CODE_FLAGS  = -fno-exceptions -fno-rtti -Wall \
              -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion
OPT_FLAGS   = -O1 -march=i486 -mtune=pentiumpro \
              -fomit-frame-pointer -fstrict-aliasing \
              -fforce-addr -funroll-all-loops -floop-optimize
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(DEFINES)

$(TEMPDIR)/C_PPMD.o: C_PPMD.cpp C_PPMD.h PPMdType.h PPMd.h SubAlloc.hpp Coder.hpp Model.cpp makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<
