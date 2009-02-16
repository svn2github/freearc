include common.mak

ALL: $(TEMPDIR)/Environment.o $(TEMPDIR)/GuiEnvironment.o $(TEMPDIR)/URL.o

CODE_FLAGS = -fno-exceptions -fno-rtti -Wall \
                -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion
OPT_FLAGS   = -O3 -march=i486 -mtune=pentiumpro \
              -fomit-frame-pointer -fstrict-aliasing \
              -ffast-math -fforce-addr
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(DEFINES)

$(TEMPDIR)/Environment.o:  Environment.cpp Environment.h Compression/Common.h makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(TEMPDIR)/GuiEnvironment.o:  GuiEnvironment.cpp Environment.h Compression/Common.h makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(TEMPDIR)/URL.o:  URL.cpp URL.h Compression/Common.h makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<

clean:
	rm -rf $(TEMPDIR)-unarc/*.o
	rm -rf $(TEMPDIR)/*.o
	rm -rf $(TEMPDIR)/*.hi
	rm -rf $(TEMPDIR)-O2/*.o
	rm -rf $(TEMPDIR)-O2/*.hi
	rm -rf $(TEMPDIR)-DFREEARC_GUI/*.o
	rm -rf $(TEMPDIR)-DFREEARC_GUI/*.hi
	rm -rf $(TEMPDIR)-O2-DFREEARC_GUI/*.o
	rm -rf $(TEMPDIR)-O2-DFREEARC_GUI/*.hi

winclean:
	cmd /c del /Q /S $(subst /,\,$(TEMPDIR))-unarc\*.o
	cmd /c del /Q /S $(subst /,\,$(TEMPDIR))\*.o
	cmd /c del /Q /S $(subst /,\,$(TEMPDIR))\*.hi
	cmd /c del /Q /S $(subst /,\,$(TEMPDIR))-O2\*.o
	cmd /c del /Q /S $(subst /,\,$(TEMPDIR))-O2\*.hi
	cmd /c del /Q /S $(subst /,\,$(TEMPDIR))-DFREEARC_GUI\*.o
	cmd /c del /Q /S $(subst /,\,$(TEMPDIR))-DFREEARC_GUI\*.hi
	cmd /c del /Q /S $(subst /,\,$(TEMPDIR))-O2-DFREEARC_GUI\*.o
	cmd /c del /Q /S $(subst /,\,$(TEMPDIR))-O2-DFREEARC_GUI\*.hi
