// Доступ к структуре архива
#include "ArcStructure.h"

#include "../Compression/MultiThreading.h"
#include "unarcdll.h"

// Доступ к парсингу командной строки и выполнению операций над архивом
#include "ArcCommand.h"
#include "ArcProcess.h"


/******************************************************************************
** Описание интерфейса с программой, использующей DLL *************************
******************************************************************************/
class DLLUI : public BASEUI
{
private:
  char outdir[MY_FILENAME_MAX*4];  //unicode: utf-8 encoding
  uint64 totalBytes;
public:
  char *event; int int1,int2; char *str;

  bool AllowProcessing (char cmd, int silent, FILENAME arcname, char* comment, int cmtsize, FILENAME outdir);
  FILENAME GetOutDir();
  void BeginProgress (uint64 totalBytes);
  bool ProgressRead  (uint64 readBytes);
  bool ProgressFile  (bool isdir, const char *operation, FILENAME filename, uint64 filesize);
  void EndProgress();
  char AskOverwrite (FILENAME filename, uint64 size, time_t modified);
} UI;


/******************************************************************************
** Реализация интерфейса с программой, использующей DLL ***********************
******************************************************************************/
Event DoEvent, EventDone;

void DLLUI::BeginProgress (uint64 totalBytes)
{
  this->totalBytes = totalBytes;
}

bool DLLUI::ProgressRead (uint64 readBytes)
{
  event = "progress";
  int1 = readBytes>>20;
  int2 = totalBytes>>20;

  DoEvent.Signal();
  EventDone.Lock();
  return TRUE;
}

bool DLLUI::ProgressFile (bool isdir, const char *operation, FILENAME filename, uint64 filesize)
{
//  printf (isdir?  "%s %s" STR_PATH_DELIMITER "\n"  :  "%s %s (%llu bytes)\n",
//          operation, filename, filesize);

  event = "filename";
  str = filename;

  DoEvent.Signal();
  EventDone.Lock();
  return TRUE;
}

void DLLUI::EndProgress()
{
  printf ("All OK");
}

FILENAME DLLUI::GetOutDir()
{
  return outdir;
}

bool DLLUI::AllowProcessing (char cmd, int silent, FILENAME arcname, char* comment, int cmtsize, FILENAME _outdir)
{
  strcpy (outdir, _outdir);
  return TRUE;
}

char DLLUI::AskOverwrite (FILENAME filename, uint64 size, time_t modified)
{
  return 'n';
}


/******************************************************************************
** Реализация функционала DLL *************************************************
******************************************************************************/
static DWORD WINAPI decompress_thread (void *paramPtr)
{
  COMMAND *command = (COMMAND*) paramPtr;
  PROCESS (*command, UI);      //   Выполнить разобранную команду
  UI.event = "quit";
  DoEvent.Signal();
  return 0;
}

int __cdecl FreeArcExtract (cbtype *callback, ...)
{
  va_list argptr;
  va_start(argptr, callback);

  int argc=0;
  char *argv[1000] = {"c:\\unarc.dll"};  //// Здесь будет искаться arc.ini!

  for (int i=1; i<1000; i++)
  {
    argc = i;
    argv[i] = va_arg(argptr, char*);
    if (argv[i]==NULL || argv[i][0]==0)
      {argv[i]=NULL; break;}
  }
  va_end(argptr);



  CThread thread;

  COMMAND command (argc, argv);    // Распарсить команду
  if (command.ok) {                // Если парсинг был удачен и можно выполнить команду
    thread.Create (decompress_thread, &command);   //   Выполнить разобранную команду

    for(;;)
    {
      DoEvent.Lock();
      if (strequ(UI.event, "quit"))
        break;
      int result = callback (UI.event, UI.int1, UI.int2, UI.str);
      if (result < 0)
        return result;
      EventDone.Signal();
    }
    thread.Wait();
  }
  return command.ok? FREEARC_OK : FREEARC_ERRCODE_GENERAL;
}

