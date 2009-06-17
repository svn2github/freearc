// Обработка сбоев при распаковке архива
#undef  ON_CHECK_FAIL
#define ON_CHECK_FAIL()   UnarcQuit()
void UnarcQuit();

// Доступ к структуре архива
#include "ArcStructure.h"

#include "../Compression/MultiThreading.h"
#include "unarcdll.h"

// Доступ к парсингу командной строки и выполнению операций над архивом
#include "ArcCommand.h"
#include "ArcProcess.h"

// Экстренный выход из программы в случае ошибки
void UnarcQuit()
{
  CurrentProcess->quit();
}


/******************************************************************************
** Описание интерфейса с программой, использующей DLL *************************
******************************************************************************/
class DLLUI : public BASEUI
{
private:
  char outdir[MY_FILENAME_MAX*4];  //unicode: utf-8 encoding
  uint64 totalBytes;
public:
  Event DoEvent, EventDone;
  char *what; int int1, int2; char *str;
  void event (char *_what, int _int1, int _int2, char *_str);

  bool AllowProcessing (char cmd, int silent, FILENAME arcname, char* comment, int cmtsize, FILENAME outdir);
  FILENAME GetOutDir ();
  void BeginProgress (uint64 totalBytes);
  bool ProgressRead  (uint64 readBytes);
  bool ProgressFile  (bool isdir, const char *operation, FILENAME filename, uint64 filesize);
  char AskOverwrite  (FILENAME filename, uint64 size, time_t modified);
  void Abort         (COMMAND *cmd);
} UI;


/******************************************************************************
** Реализация интерфейса с программой, использующей DLL ***********************
******************************************************************************/
void DLLUI::event (char *_what, int _int1, int _int2, char *_str)
{
  what = _what;
  int1 = _int1;
  int2 = _int2;
  str  = _str;

  DoEvent.Signal();
  EventDone.Lock();
}

void DLLUI::BeginProgress (uint64 totalBytes)
{
  this->totalBytes = totalBytes;
}

bool DLLUI::ProgressRead (uint64 readBytes)
{
  event ("progress", readBytes>>20, totalBytes>>20, "");
  return TRUE;
}

bool DLLUI::ProgressFile (bool isdir, const char *operation, FILENAME filename, uint64 filesize)
{
  event ("filename", 0, 0, filename);
  return TRUE;
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

void DLLUI::Abort (COMMAND *cmd)
{
  event ("quit", 0, 0, "");
}


/******************************************************************************
** Реализация функционала DLL *************************************************
******************************************************************************/
static DWORD WINAPI decompress_thread (void *paramPtr)
{
  COMMAND *command = (COMMAND*) paramPtr;
  PROCESS (*command, UI);      //   Выполнить разобранную команду
  UI.what = "quit";
  UI.DoEvent.Signal();
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




  COMMAND command (argc, argv);    // Распарсить команду
  if (command.ok) {                // Если парсинг был удачен и можно выполнить команду
    CThread thread;
    thread.Create (decompress_thread, &command);   //   Выполнить разобранную команду

    for(;;)
    {
      UI.DoEvent.Lock();
      if (strequ (UI.what, "quit"))
        break;
      int result = callback (UI.what, UI.int1, UI.int2, UI.str);
      if (result < 0)
        return result;
      UI.EventDone.Signal();
    }
    thread.Wait();
  }
  return command.ok? FREEARC_OK : FREEARC_ERRCODE_GENERAL;
}

