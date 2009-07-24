#include <stdio.h>
#include <tabi.h>

virtual int COMPRESSION_METHOD::server()
{
  char *service = p._str("service");

  // Global services
  if (strequ (service, "register"))               return Register();

  // Invocation-specific services
  if (strequ (service, "decompress"))             {parse_method(); return decompress (p._callback("callback"), p);}
#ifndef FREEARC_DECOMPRESS_ONLY
  if (strequ (service, "compress"))               {parse_method(); return compress   (p._callback("callback"), p);}
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


int RegisterCompressionMethod (TABI_SERVICE server)
{
  if (server("register")==FREEARC_OK)
    *ptr++ = server;
  return FREEARC_OK;
}

int cels_call (TABI_ELEMENT* params)
{
  for (p=table; p<ptr; p++)
  {
    int x = (*p)(params);
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






