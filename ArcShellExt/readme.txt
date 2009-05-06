========================================================================
Context Menu Extension for SciTE (Win32)
Version: 1.4.0
Date: 12/06/2008
File Name: wscitecm.dll
$Id: readme.txt 497 2008-12-07 03:17:37Z andre $
========================================================================

Overview
--------
wscitecm.dll is a companion for SciTE on Windows (32-bit and 64-bit). Installing wscitecm.dll creates a new command "Edit with SciTE" in the context menu of Windows Explorer. You can quickly open one or several selected files in Windows Explorer: right click on the selection and click on the command "Edit with SciTE". For a friendly installation, I recommend to use the SciTE setup provided by Bruce Dodson (http://gisdeveloper.tripod.com/scite.html) and to check the choice "Register shell extension" during the install process. The manual installation is described in the following sections.

Uninstallation
--------------
To uninstall a version prior to 1.2:
- Double click on the file wscitecm-remove.reg. This will clean-up the old registry keys. This script was provided by Bruce Dodson.

To uninstall any version newer than 1.2:
- In SciTE directory installation, type the command "regsvr32 /u wscitecm.dll".

Installation
------------
1) Copy wscitecm.dll in SciTE directory. Refer to the paragraph unload the dll if you have the error: "Access is denied."
2) In SciTE directory installation, type the  command "regsvr32 wscitecm.dll". This will register the dll.

If everything goes well, you should have "Edit with SciTE" when you right click on selected file(s) in Windows Explorer.

Unload the dll
--------------
If you try to delete or override the dll file and you get the error "Access is denied.", the library is already loaded. There are several options to workaround this issue:

Solution 1:
- Close all the Windows Explorer instances open on your desktop and copy wsctecm.dll using the command line (Example: "C:/>cp wscitecm.dll <scite_directory>").

Solution 2:
- Reboot the computer and delete or override wscitecm.dll (with the command line) before starting Windows Explorer and using the context menu (right-click).

Solution 3:
- Open a command line window
- Type CTRL+ALT+DEL to display the Windows Task Manager, display the Process tab and "kill" the explorer.exe process.
- If Windows Exlorer does not restart automatically, start it manually from  the command line window (c:/>explorer).
- Delete or override  wscitecm.dll before using the context menu (Example: "C:/>cp wscitecm.dll <scite_directory>").

Build
-----
Until version 1.2.1, wscitecm was built with Visual C++ 6.0. Version 1.3.0 (32-bit and 64-bit) was built with Visual Studio 2005. A Makefile is provided with the sources: in the source directory, type "nmake". Ensure that  all the environment variables and paths are set correctly. To do so, use the command file "VCVARS32.BAT" available in the bin directory of Visual C++ installation.

History
-------
Version 1.4.0 (12/06/2008):
- Fixed an issue with the manifest file introduced with VS 2005 compilation
- Modified the GUID to avoid conflict with Notepad++ reusing wscitecm code with the same GUID

Version 1.3.0 (05/26/2008):
- Support for Vista 64-bit
- Build with Visual Studio 2005
- Fixed warnings related to string functions

Version 1.2.1 (01/21/2003):
- Released under MIT license and packaged with the source code

Version 1.2:
- Registration and unregistration of the Shell Extension included in the code
- SciTE icon displayed in the context menu

Version 1.x:
- Initial version

License
-------
MIT License
Copyright 2002-2008 Andre Burgaud <andre@burgaud.com>
See license.txt