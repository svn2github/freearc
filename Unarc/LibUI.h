#include "../Compression/MultiThreading.h"
//#include "LZMA/Windows/Synchronization.cpp"

Event DoEvent, EventDone;

class UI
{
private:
  char outdir[MY_FILENAME_MAX*4];  //unicode: utf-8 encoding
  uint64 totalBytes;
public:
  char *event; int int1,int2; char *str;

  UI();
  ~UI();
  void DisplayHeader (char* header);
  bool AllowProcessing (char cmd, int silent, FILENAME arcname, char* comment, int cmtsize, FILENAME outdir);
  FILENAME GetOutDir();
  void BeginProgress (uint64 totalBytes);
  bool ProgressRead  (uint64 readBytes);
  bool ProgressWrite (uint64 writtenBytes);
  bool ProgressFile  (bool isdir, const char *operation, FILENAME filename, uint64 filesize);
  void EndProgress();
  char AskOverwrite (FILENAME filename, uint64 size, time_t modified);
};

UI::UI()
{
}

UI::~UI()
{
}

void UI::DisplayHeader (char* header)
{
}

void UI::BeginProgress (uint64 totalBytes)
{
  this->totalBytes = totalBytes;
}

bool UI::ProgressRead (uint64 readBytes)
{
  event = "progress";
  int1 = readBytes>>20;
  int2 = totalBytes>>20;

  DoEvent.Signal();
  EventDone.Lock();
  return TRUE;
}

bool UI::ProgressWrite (uint64 writtenBytes)
{
  return TRUE;
}

bool UI::ProgressFile (bool isdir, const char *operation, FILENAME filename, uint64 filesize)
{
//  printf (isdir?  "%s %s" STR_PATH_DELIMITER "\n"  :  "%s %s (%llu bytes)\n",
//          operation, filename, filesize);

  event = "filename";
  str = filename;

  DoEvent.Signal();
  EventDone.Lock();
  return TRUE;
}

void UI::EndProgress()
{
  printf ("All OK");
}

FILENAME UI::GetOutDir()
{
  return outdir;
}

bool UI::AllowProcessing (char cmd, int silent, FILENAME arcname, char* comment, int cmtsize, FILENAME _outdir)
{
  strcpy (outdir, _outdir);
  return TRUE;
}

char UI::AskOverwrite (FILENAME filename, uint64 size, time_t modified)
{
  return 'n';
}

