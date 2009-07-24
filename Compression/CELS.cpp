#include <stdio.h>
#include "CELS.h"

namespace CELS
{

int COMPRESSION_METHOD::server()
{
  char *service = p._str("service");

  if (strequ (service, "encryption?"))    return 1;               // to do: ---

  // Global services
  if (strequ (service, "register"))               return FREEARC_OK; //to do: Register();

  // Invocation-specific services
  if (strequ (service, "decompress"))             {parse_method(); return decompress (p._callback("callback"), p);}
#ifndef FREEARC_DECOMPRESS_ONLY
  if (strequ (service, "compress"))               {parse_method(); return compress   (p._callback("callback"), p);}
  if (strequ (service, "canonize"))               {parse_method();                                     char a[1000]; ShowCompressionMethod(a); return p._return(a);}
  if (strequ (service, "SetCompressionMem"))      {parse_method(); SetCompressionMem  (p._int("mem")); char a[1000]; ShowCompressionMethod(a); return p._return(a);}
  if (strequ (service, "SetDictionary"))          {parse_method(); SetDictionary      (p._int("mem")); char a[1000]; ShowCompressionMethod(a); return p._return(a);}
  if (strequ (service, "SetBlockSize"))           {parse_method(); SetBlockSize       (p._int("mem")); char a[1000]; ShowCompressionMethod(a); return p._return(a);}
  if (strequ (service, "SetDecompressionMem"))    {parse_method(); SetDecompressionMem(p._int("mem")); char a[1000]; ShowCompressionMethod(a); return p._return(a);}
  if (strequ (service, "GetCompressionMem"))      {parse_method(); return p._return (GetCompressionMem());}
  if (strequ (service, "GetDictionary"))          {parse_method(); return p._return (GetDictionary());}
  if (strequ (service, "GetBlockSize"))           {parse_method(); return p._return (GetBlockSize());}
#endif
  if (strequ (service, "GetDecompressionMem"))    {parse_method(); return p._return (GetDecompressionMem());}

  return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}

}

// ****************************************************************************************************************************
// ПОДДЕРЖКА ТАБЛИЦЫ ЗАРЕГИСТРИРОВАННЫХ МЕТОДОВ СЖАТИЯ И ПОИСК В ЭТОЙ ТАБЛИЦЕ РЕАЛИЗАЦИИ ЧИСТО КОНКРЕТНОГО МЕТОДА *************
// ****************************************************************************************************************************

// Кол-во зарегистрированных методов сжатия и таблица, куда они заносятся
int methodsCount = 0;
TABI_FUNCTION* methodsTable[MAX_COMPRESSION_METHODS];

// Compression method registration
int CELS_Register (TABI_FUNCTION *method)
{
  CHECK (methodsCount < elements(methodsTable), (s,"INTERNAL ERROR: Overflow of compression methods table"));
  int result = (*method)(TABI_DYNAMAP("service","register"));
  if (result==FREEARC_OK)
    methodsTable[methodsCount++] = method;
  return result;
}

// Central CELS function that provides all CELS services
int CELS_Call (TABI_ELEMENT* params)
{
  TABI_MAP p(params); p.dump();
  for (int i=0; i<methodsCount; i++)
  {
    int x = methodsTable[i](params);
    // some auto-actions
    //SetDecompressionMem {if (mem>0)   ...;}
    //SetDictionary       {if (dict>0)  ...;}
    //GetBlockSize        {return 0;}
    //SetBlockSize        {}
    //Limit               get & set

    if (x!=FREEARC_ERRCODE_NOT_IMPLEMENTED)
      return x;
  }
  return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}

