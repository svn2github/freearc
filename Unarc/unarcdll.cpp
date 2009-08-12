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
  CurrentProcess->quit(FREEARC_ERRCODE_GENERAL);
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
  COMMAND *command;
  Mutex mutex;
  Event DoEvent, EventDone;

  char *what; Number n1, n2; int result; char *str;
  bool event (char *_what, Number _n1, Number _n2, char *_str);

  DLLUI (COMMAND *_command) : command(_command) {}
  bool AllowProcessing (char cmd, int silent, FILENAME arcname, char* comment, int cmtsize, FILENAME outdir);
  FILENAME GetOutDir ();
  void BeginProgress (uint64 totalBytes);
  bool ProgressRead  (uint64 readBytes);
  bool ProgressWrite (uint64 writtenBytes);
  bool ProgressFile  (bool isdir, const char *operation, FILENAME filename, uint64 filesize);
  char AskOverwrite  (FILENAME filename, uint64 size, time_t modified);
  void Abort         (COMMAND *cmd, int errcode);
};


/******************************************************************************
** Реализация интерфейса с программой, использующей DLL ***********************
******************************************************************************/
bool DLLUI::event (char *_what, Number _n1, Number _n2, char *_str)
{
  Lock _(mutex);
  what = _what;
  n1   = _n1;
  n2   = _n2;
  str  = _str;

  DoEvent.Signal();
  EventDone.Lock();
  return result>=0;
}

void DLLUI::BeginProgress (uint64 totalBytes)
{
  this->totalBytes = totalBytes;
}

bool DLLUI::ProgressRead (uint64 readBytes)
{
  return event ("read", readBytes>>20, totalBytes>>20, "");
}

bool DLLUI::ProgressWrite (uint64 writtenBytes)
{
  return event ("write", writtenBytes>>20, 0, "");
}

bool DLLUI::ProgressFile (bool isdir, const char *operation, FILENAME filename, uint64 filesize)
{
  return event ("filename", 0, 0, filename);
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

void DLLUI::Abort (COMMAND *cmd, int errcode)
{
  event ("quit", errcode, 0, "");
}


/******************************************************************************
** Реализация функционала DLL *************************************************
******************************************************************************/
static DWORD WINAPI timer_thread (void *paramPtr)
{
  DLLUI *ui = (DLLUI*) paramPtr;
  for(;;)
  {
    Sleep(10);
    ui->event ("timer", 0, 0, "");
  }
}

static DWORD WINAPI decompress_thread (void *paramPtr)
{
  uint64 total_files, origsize, compsize;
  DLLUI *ui = (DLLUI*) paramPtr;
  // Выполнить разобранную команду
  if (ui->command->cmd=='l')
  {
    PROCESS (ui->command, ui, total_files, origsize, compsize);
    ui->event ("total_files", total_files,  0, "");
    ui->event ("origsize",    origsize>>20, 0, "");
    ui->event ("compsize",    compsize>>20, 0, "");
  }
  else
    PROCESS (ui->command, ui);
  ui->what = "quit";
  ui->n1   = FREEARC_OK;
  ui->DoEvent.Signal();
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
    DLLUI *ui = new DLLUI(&command);
    thread.Create (timer_thread,      ui);   //   Спец. тред, вызывающий callback 100 раз в секунду
    thread.Create (decompress_thread, ui);   //   Выполнить разобранную команду

    for(;;)
    {
      ui->DoEvent.Lock();
      if (strequ (ui->what, "quit"))
        return ui->n1;  // error code of command
      ui->result = callback (ui->what, ui->n1, ui->n2, ui->str);
      ui->EventDone.Signal();
    }
    thread.Wait();
  }
  return command.ok? FREEARC_OK : FREEARC_ERRCODE_GENERAL;
}

