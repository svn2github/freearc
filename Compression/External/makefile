include ../../common.mak

ALL: $(TEMPDIR)/C_External.o

CODE_FLAGS = -fno-rtti -Wall \
                -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion
OPT_FLAGS   = -Os -march=i486 -mtune=pentiumpro \
              -fomit-frame-pointer -fstrict-aliasing \
              -ffast-math -fforce-addr
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(DEFINES)

$(TEMPDIR)/C_External.o: C_External.cpp C_External.h makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<
