include ../../common.mak

ALL: $(TEMPDIR)/C_Delta.o

CODE_FLAGS  = -fno-exceptions -fno-rtti -Wall \
              -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion
OPT_FLAGS   = -O3 -march=i486 -mtune=pentiumpro \
              -fomit-frame-pointer -fstrict-aliasing \
              -fforce-addr -funroll-loops
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(DEFINES)

$(TEMPDIR)/C_Delta.o: C_Delta.cpp C_Delta.h Delta.cpp makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<
