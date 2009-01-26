It should be easy to compile FreeArc:

== ON WINDOWS ===========================================================================

1. Download GHC 6.6.1 or 6.8.2 bundled with C++ compiler (33 mb):
     http://haskell.org/ghc/dist/6.6.1/ghc-6.6.1-i386-windows.exe
     OR http://haskell.org/ghc/dist/6.8.2/ghc-6.8.2-i386-windows.exe
2. Install it into C:\Base\Compiler\ghc directory
3. Make sure that make.exe is available via your PATH. If you yet not have make,
     download http://sourceforge.net/project/showfiles.php?group_id=2435
     and rename mingw32-make.exe to make.exe
4. Install Gtk2Hs: http://sourceforge.net/project/showfiles.php?group_id=49207&package_id=42440
     (for ghc 6.8.2 download http://haskell.org/~duncan/gtk2hs/gtk2hs-0.9.12.1.exe )
5. Install HsLua:
     cd HsLua
     make
     cd ..
6. For compiling console version (Arc.exe):
     compile-O2.cmd
7. For compiling GUI version (WinArc.exe):
     compile-GUI-O2.cmd
8. When compilation will be finished, you will find Arc.exe/WinArc.exe in Tests subdirectory
9. For compiling SFX modules, Unarc and FAR plugin:
     cd Unarc
     make


== ON UNIX (tested on Fedora7) ==========================================================

1. Install GHC 6.6.1 (http://haskell.org/ghc/download_ghc_661.html) or 6.8.2, GCC, make and
     Gtk2Hs (http://darcs.haskell.org/gtk2hs/download)
2. Run in the directory where you've extracted FreeArc sources:
     chmod +x compile*
3. Install HsLua:
     cd HsLua && ghc --make Setup.hs && ./Setup configure && ./Setup build && ./Setup install && cd ..
4. For compiling console version (arc):
     ./compile-O2
5. For compiling GUI version (winarc):
     ./compile-GUI-O2
6. When compilation will be finished, you will find arc/winarc executables in Tests subdirectory
7. For compiling SFX modules and Unarc:
     cd Unarc
     make linux

