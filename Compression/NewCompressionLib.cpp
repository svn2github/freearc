#include <stdio.h>
#include <tabi.h>

struct COMPRESSION_METHOD
{
  // Call parameters
  TABI_MAP p;

  // Methods
  COMPRESSION_METHOD (TABI_ELEMENT* params) : p(params) {};

  void parse_method()
  {
    char *paramstr = p._str("method");
  }

  int server (char *service)
  {
    // Global services
    if (strequ (service, "register"))               return Register();

    // Private services
    if (strequ (service, "decompress"))             {parse_method(); return Decompress (p._callback("callback"));}  //auxdata?
#ifndef FREEARC_DECOMPRESS_ONLY
    if (strequ (service, "compress"))               {parse_method(); return Compress   (p._callback("callback"));}
    if (strequ (service, "GetCompressionMem"))      {parse_method(); return GetCompressionMem();}
    if (strequ (service, "GetDictionary"))          {parse_method(); return GetDictionary();}
    if (strequ (service, "GetBlockSize"))           {parse_method(); return GetBlockSize();}
    if (strequ (service, "SetCompressionMem"))      {parse_method(); SetCompressionMem  (p._int("mem"));  ShowCompressionMethod(p._str("result")); return FREEARC_OK;}
    if (strequ (service, "SetDecompressionMem"))    {parse_method(); SetDecompressionMem(p._int("mem"));  ShowCompressionMethod(p._str("result")); return FREEARC_OK;}
    if (strequ (service, "SetDictionary"))          {parse_method(); SetDictionary      (p._int("dict")); ShowCompressionMethod(p._str("result")); return FREEARC_OK;}
    if (strequ (service, "SetBlockSize"))           {parse_method(); SetBlockSize       (p._int("dict")); ShowCompressionMethod(p._str("result")); return FREEARC_OK;}
#endif
    if (strequ (service, "GetDecompressionMem"))    {parse_method(); return GetDecompressionMem();}

    return FREEARC_ERRCODE_NOT_IMPLEMENTED;
  }

};



int RegisterCompressionMethod (TABI_SERVICE server)
{
  if (server("register")==FREEARC_OK)
    *ptr++ = server;
  return FREEARC_OK;
}

int cels_call (char *service, TABI_ELEMENT* params)
{
  for (p=table; p<ptr; p++)
  {
    int x = (*p)(service,params);
    // some auto-actions
    //SetDecompressionMem {if (mem>0)   ...;}
    //SetDictionary       {if (dict>0)  ...;}
    //GetBlockSize        {return 0;}
    //SetBlockSize        {}

    if (x!=FREEARC_ERRCODE_NOT_IMPLEMENTED)
      return x;
  }
  return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}






