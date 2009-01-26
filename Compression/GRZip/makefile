include ../../common.mak

ALL: $(TEMPDIR)/C_GRZip.o

CODE_FLAGS = -fno-exceptions -fno-rtti \
                -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion
OPT_FLAGS   = -O2 -march=i486 -mtune=pentiumpro \
              -fomit-frame-pointer -fforce-addr \
              -ffast-math -fstrict-aliasing -funroll-loops
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(DEFINES)

$(TEMPDIR)/C_GRZip.o: C_GRZip.cpp C_GRZip.h LZP.c libGRZip.h Rec_Flt.c makefile ../MultiThreading.h
	$(GCC) -c $(CFLAGS) -o $*.o $<
