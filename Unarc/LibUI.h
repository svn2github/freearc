#include <shlobj.h>
#include <windows.h>
#include <richedit.h>

typedef int __cdecl cbtype (char *what, int int1, int int2, char *str);

class UI
{
private:
  char outdir[MY_FILENAME_MAX*4];  //unicode: utf-8 encoding
  uint64 totalBytes;
public:
  cbtype *callback;
  HANDLE hWnd, hpb, hst;

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
//  callback ("progress", readBytes>>10, totalBytes>>10, "");
  // Show progress indicator
  int MAX_PROGRESS_VALUE=100;
  int normalizedReadBytes = (totalBytes == 0)? 0 : int((double(readBytes) * MAX_PROGRESS_VALUE)/totalBytes);
  //SendNotifyMessage ((HWND)hpb, PBM_SETPOS, normalizedReadBytes, 0);
  return TRUE;
}

bool UI::ProgressWrite (uint64 writtenBytes)
{
  return TRUE;
}

bool UI::ProgressFile (bool isdir, const char *operation, FILENAME filename, uint64 filesize)
{
//  SetWindowTextA((HWND)hWnd, filename);
//  SetWindowTextA((HWND)hst, filename);
//  callback ("filename", 0, 0, filename);
//  printf (isdir?  "%s %s" STR_PATH_DELIMITER "\n"  :  "%s %s (%llu bytes)\n",
//          operation, filename, filesize);
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

