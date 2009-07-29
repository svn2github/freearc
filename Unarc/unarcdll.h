// Definitions for use of unarc.dll

typedef int Number;
typedef int __stdcall cbtype (char *what, Number int1, Number int2, char *str);

extern "C" int __cdecl FreeArcExtract (cbtype *callback, ...);
