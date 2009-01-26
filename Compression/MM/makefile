include ../../common.mak

ALL: $(TEMPDIR)/C_MM.o $(TEMPDIR)/C_TTA.o

CODE_FLAGS  = -fno-exceptions -fno-rtti -Wall \
              -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion
OPT_FLAGS   = -O3 -march=i486 -mtune=pentiumpro \
              -fomit-frame-pointer -fstrict-aliasing \
              -fforce-addr -funroll-loops
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(DEFINES)

$(TEMPDIR)/C_MM.o: C_MM.cpp C_MM.h mm.cpp mmdet.cpp makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(TEMPDIR)/C_TTA.o: C_TTA.cpp C_TTA.h tta.cpp entropy.cpp filters.cpp makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<
