Welcome to FreeArc 0.51 (April 28 2009).

FreeArc is free, open-source archiver for Windows and Linux. It features solid archives with smart updating, AES+Twofish+Serpent encryption, recovery record, self-extracting archives and wide range of compression profiles.

FreeArc works 1.5-3 times faster than best other compression programs (zip, RAR, 7-zip, WinRK) while providing the same compression ratio.

For up-to-date information about FreeArc look at http://freearc.org

===========================================================================

Run "make install" to install Arc, FreeArc, Unarc and support files on your system.
Or, run "make local" to install with per-user settings.
Run "make uninstall" to remove all files installed.
FreeArc GUI requires Gtk+ to work.

===========================================================================

Main changes in this version:

    * Significant improvements in GUI and lots of fixed problems, result in that now GUI version is more stable, feature-rich and may be used for daily work
    * -m2/-m3/-m4 modes now use 96mb dictionary by default. Speed/compresion ratio was also improved in many other ways
    * Fixed three (de)compression bugs - while these are not critical, I recommend you to upgrade

Also
    * GUI: Skin changer provides more themes; default theme was also improved
    * GUI: You can change menu/toolbar (just copy freearc.menu.example to freearc.menu and edit it as you wish)
    * GUI: Run any program/document just by double-click (or Enter) on it
    * GUI: Single-click on the empty space at the right clears selection, double-click selects all files
    * GUI: If you move/resize any window, dialog or filemanager column - its size and position are preserved
    * GUI: You can sort columns by clocking on column header, second click reverses sorting; sorting order is preserved too
    * GUI: FAR-like keys for marking files: Shift+ Shift- Ctrl+ Ctrl- Alt+ Alt-
    * GUI: You can start a file search by typing the first letters of its name. *? wildcards are also supported
    * GUI: Every day FreeArc automatically checks for program updates via Internet
    * FAR/TC plugins: support for copy/move of empty directories to/from archive
    * Windows: unicode (utf-16) cmdline processing
    * Encoding selection options: -sct for console; -sci for logfile; -scf for filenames on disk (Linux only)
    * "-ms-" option: disable fast compression of already compressed files
    * "lt" command: technical archive listing
    * --language=LANGFILE option provides translation of error/warning messages for console version
    * -rr0.1%, -rr0.01% options support (translated to -rr0*4kb, -rr0*64kb)
    * -m1xx..-m4xx: very fast decompression modes (by default, require 1gb of free memory to decompress!)
    * Tornado: support for :t :ah :al options, :l that's not power of 2; tor:7:c1 .. tor:11:c3; checks at decoding in order to prevent segfaults
    * Installer SFX-es: freearc-installer*.sfx: extracts into tempdir, runs setup.exe and then optionally deletes extracted files
    * External compressors: Copies data intact when compression program returned non-zero error code
    * External compressors: Raises error when decompression program returned non-zero error code
    * External compressors: Improved syntax for description of external compressors
    * arc.ini: large 40kb variant with support for External Compressors PowerPack
    * Initial CLS support (external compressors in cls-*.dll), see Addons\CLS directory
    * Error code 21 is returned when archive/file cannot be decrypted using supplied password
    * Logfile: hiding passwords used in cmdline/cfgfile

Building FreeArc users community:
    * New FreeArc forum and Wiki are available via the program menu
    * Send bug reports and enhancement requests via Community page at program site
    * Source repository is at https://freearc.svn.sourceforge.net/svnroot/freearc
    * And yes, now we accept donations via PayPal ;)


Main changes in FreeArc 0.50 alpha (June 23 2008):

    * GUI: WinArc executable renamed to FreeArc; a lot of usability improvements
    * Windows: added "Extract here" to .arc files context menu; added "FreeArc archive" to Explorer New command; added "Change skin" shortcut; config files are searched first in user-specific "Application Data\FreeArc" directory
    * Linux: fixed archive navigation & Unicode filenames support
    * SFX: support for non-English filenames; WinRAR-compatible options with help displayed by "/?"
    * -sfx option by default adds GUI module freearc.sfx (instead of arc.sfx)
    * Total Commander addon: support for filenames with spaces and SFX archives
    * FAR Plugin: added "Convert to SFX" command
    * Improved filetype detection for large heterogenous files
    * Fixed bug in REP that was preventing decompression of some archives created in -mx mode


Main changes in FreeArc 0.50 alpha (June 3 2008):

    * Creation of self-extracting archives with Windows GUI, Windows console and Linux console interfaces
    * Support of RTF files used as comments in GUI self-extractors
    * WinArc: message combobox at the window bottom
    * WinArc: registers icon for .arc files
    * New format of archive comments with ability to read old comments
    * Linux: multithreading for LZMA (1.5x faster compression)
    * Fixed bug in -m1 mode implementation; old archives created in this mode need to be recompressed to -m2 before upgarding to new program version


Main changes in FreeArc 0.50 alpha (May 15 2008):

    * Improved automatic filetype detection
    * Improved -m3/m4 compression methods
    * Initial support for scripting FreeArc using Lua (see scripts directory)
    * -di+% for displaying memory status
    * lzma: changed default settings; :h parameter allows to change size of hash; new HT4 matchfinder allows to use dictionary up to 1gb
    * GUI: BackSpace jumps to the parent directory


Main changes in FreeArc 0.50 alpha (Feb 8 2008):

    * Improved automatic filetype detection and ability to disable it using -ma- option and in GUI
    * Ability to create Compression/Encryption profiles
    * Significantly improved Add dialog, it now supports almost all features of cmdline version
    * Localization for names of standard settings/profiles
    * Tooltips can be assigned to any GUI control using the same number+1000. Please tell me if some controls don't appreciate tooltips


Main changes in FreeArc 0.50 alpha (Jan 28 2008):

    * GUI version, localized to 70 languages ;)
    * Automatic detection of filetype by file contents - still unfinished
    * Fixed all the memory-allocation problems (thanks to Egor!)
    * By default, -ld=1gb (limit decompression memory to 1gb) is used
    * With "-lc- -ld-" options and 64-bit Windows, algorithm limits now are: lzma:255m, lzma:fast:511m, ppmd:2047m, rep:2047m, lzp:1675m
    * Archive listing commands now include "Compressed" sizes and mark encrypted files with "*"
    * By default, solid blocks now have unlimited size (in 0.40, 1gb was the default)
