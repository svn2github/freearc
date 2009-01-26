/*
 *  Example program of using Compression-2005 library with C/C++
 *
 *  This program implements simple file-to-file compressor and decompressor
 *  Run program without parameters to see it's syntax
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../Compression.h"

#if defined(WIN32) || defined(OS2) || defined(MSDOS)
#include <fcntl.h>
#include <io.h>
#define set_binary_mode(file)                   setmode(fileno(file),O_BINARY)
#else
#define set_binary_mode(file)
#endif

// Input/output files and callbacks for (de)compression functions
FILE *infile, *outfile;
int read_func (void *buf, int size)
{
  return fread(buf,1,size,infile);
}
int write_func (void *buf, int size)
{
  return fwrite(buf,1,size,outfile)==size? size : FREEARC_ERRCODE_IO;
}

int process (char *method)
{
  int result;  double t;
  if (strcmp (method, "d"))
    result = CompressWithHeader (method, read_func, write_func, &t);
  else
    result = DecompressWithHeader (read_func, write_func, &t);
  if (result) {fprintf (stderr, "(De)compression error %d!\n", result); return EXIT_FAILURE;}
  return EXIT_SUCCESS;
}

int main(int argc, char *argv[])
{
  if (argc==2)
  {
    infile  = stdin;   set_binary_mode(infile);
    outfile = stdout;  set_binary_mode(outfile);
    int result = process (argv[1]);
    return result;
  }
  else if (argc==4)
  {
    infile  = fopen (argv[2], "rb");   if (infile==NULL)  {printf ("Can't open input file %s!\n", argv[2]); return EXIT_FAILURE;}
    outfile = fopen (argv[3], "wb");   if (outfile==NULL) {printf ("Can't create output file %s!\n", argv[3]); return EXIT_FAILURE;}
    int result = process (argv[1]);
    fclose (infile);
    fclose (outfile);
    return result;
  }
  else
  {
    puts("Usage, compression: Compressor method <infile >outfile\n"
         "                 or Compressor method infile outfile\n"
         "       decompression: Compressor d <infile >outfile\n"
         "                   or Compressor d infile outfile\n"
         "Supported methods are \"ppmd:...\", \"lzma:...\", \"grzip:...\"");
    return argc==1? EXIT_SUCCESS : EXIT_FAILURE;
  }
}
