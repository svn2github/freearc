// Definitions for use of unarc.dll

typedef int __stdcall cbtype (char *what, int int1, int int2, char *str);

extern "C" int __cdecl FreeArcExtract (cbtype *callback, ...);
