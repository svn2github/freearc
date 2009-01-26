include ../../common.mak

ALL: $(TEMPDIR)/C_Dict.o

CODE_FLAGS = -fno-rtti -Wall \
                -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion
OPT_FLAGS   = -O3 -march=i486 -mtune=pentiumpro \
              -fomit-frame-pointer -fstrict-aliasing \
              -ffast-math -fforce-addr
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(DEFINES)

$(TEMPDIR)/C_Dict.o: C_Dict.cpp C_Dict.h dict.cpp makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<
