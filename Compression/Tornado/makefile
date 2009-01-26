include ../../common.mak

ALL: $(TEMPDIR)/C_Tornado.o

CODE_FLAGS  = -fno-exceptions -fno-rtti \
              -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion
OPT_FLAGS   = -O3 -march=i486 -mtune=pentiumpro \
              -fomit-frame-pointer -fstrict-aliasing \
              -fforce-addr -ffast-math -funroll-loops --param inline-unit-growth=999
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(DEFINES)

$(TEMPDIR)/C_Tornado.o: C_Tornado.cpp C_Tornado.h Tornado.cpp MatchFinder.cpp LZ77_Coder.cpp EntropyCoder.cpp DataTables.cpp makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<
