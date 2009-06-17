// to do: отбор файлов по именам ("name" или "dir/name"),
//        дешифрование данных/заголовка
//        добавление ".arc", listfiles/-ap/-kb
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <wchar.h>

// External compressors support
extern "C" {
#include "../Compression/External/C_External.h"
}

// SFX module is just unarc.cpp compiled with FREEARC_SFX defined
#ifdef FREEARC_SFX
#define NAME           "SFX"
#else
#define NAME           "unpacker"
#endif

#define HEADER1        "FreeArc 0.52 "
#define HEADER2        "  http://freearc.org  2009-06-15\n"

// Доступ к структуре архива
#include "ArcStructure.h"
#include "ArcCommand.h"
#include "ArcProcess.h"


// Весь диалог с пользователем описан в сменных модулях, включаемых здесь
#ifdef FREEARC_GUI
#include "gui\gui.h"
#include "gui\gui.cpp"
#else

class CUI : public BASEUI
{
private:
  char outdir[MY_FILENAME_MAX*4];  //unicode: utf-8 encoding
public:
  void DisplayHeader (char* header);
  bool ProgressFile  (bool isdir, const char *operation, FILENAME filename, uint64 filesize);
  void EndProgress   ();
  bool AllowProcessing (char cmd, int silent, FILENAME arcname, char* comment, int cmtsize, FILENAME outdir);
  FILENAME GetOutDir();
  char AskOverwrite (FILENAME filename, uint64 size, time_t modified);
};

void CUI::DisplayHeader (char* header)
{
  printf ("%s", header);
}

bool CUI::ProgressFile (bool isdir, const char *operation, FILENAME filename, uint64 filesize)
{
  printf (isdir?  "%s %s" STR_PATH_DELIMITER "\n"  :  "%s %s (%llu bytes)\n",
          operation, filename, filesize);
  return TRUE;
}

void CUI::EndProgress()
{
  printf ("All OK");
}

FILENAME CUI::GetOutDir()
{
  return outdir;
}

bool CUI::AllowProcessing (char cmd, int silent, FILENAME arcname, char* comment, int cmtsize, FILENAME _outdir)
{
  strcpy (outdir, _outdir);
  printf (". %s archive: %s\n",                       // Выведем имя обрабатываемого архива
    cmd=='l'||cmd=='v'? "Listing" : cmd=='t' ? "Testing" : "Extracting", drop_dirname(arcname));
  if (cmtsize>0)                                      // Выведем архивный комментарий
#ifdef FREEARC_WIN
{
    // Convert comment from UTF-8 to OEM encoding before printing
    char *oemname = (char*) malloc(cmtsize+1);
    strncpy (oemname, comment, cmtsize);
    oemname[cmtsize] = 0;
    utf8_to_oem (oemname, oemname);
    printf ("%s\n", oemname);
    free (oemname);
}
#else
    printf("%*.*s\n", cmtsize, cmtsize, comment);
#endif

#ifdef FREEARC_SFX
  // В SFX необходимо запросить согласие пользователя перед началом распаковки
  if (!silent)
  {
    char answer[256];
    printf ("Continue extraction (y/n)? ");
    gets (answer);
    if (! (strequ(answer,"y") || strequ(answer,"Y")))
    {
      printf ("Extraction aborted!\n");
      return FALSE;
    }
    printf("\n");
  }
#endif
  return TRUE;
}

char CUI::AskOverwrite (FILENAME filename, uint64 size, time_t modified)
{
  char help[] = "Valid answers: Y - yes, N - no, A - overwrite all, S - skip all, Q - quit\n";
  again: printf ("Overwrite %s (y/n/a/s/q) ? ", filename);
  char answer[256];  gets (answer);  *answer = tolower(*answer);
  if (strlen(answer)!=1 || !strchr("ynasq", *answer))  {printf (help);  goto again;}
  if (*answer=='q') {printf ("Extraction aborted\n");  exit(1);}
  return *answer;
}

CUI UI;

#endif


#ifdef FREEARC_INSTALLER
// Wipes entire directory with all its subdirs
void wipedir(TCHAR *dir)
{
    // List all entries in this directory
    CFILENAME dirstar  = (TCHAR*) malloc (MY_FILENAME_MAX * sizeof(TCHAR));
    CFILENAME fullname = (TCHAR*) malloc (MY_FILENAME_MAX * sizeof(TCHAR));
    _stprintf (dirstar, _T("%s%s*"), dir, _T(STR_PATH_DELIMITER));
    WIN32_FIND_DATA FindData[1];
    HANDLE h = FindFirstFile (dirstar, FindData);
    if (h) do {
        // For every entry except for "." and "..", remove entire subdir (if it's a directory) or remove just file itself
        if (_tcscmp(FindData->cFileName,_T("."))  &&  _tcscmp(FindData->cFileName,_T("..")))
        {
            _stprintf (fullname, _T("%s%s%s"), dir, _T(STR_PATH_DELIMITER), FindData->cFileName);
            if (FindData->dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
                wipedir (fullname);
            else
                DeleteFile (fullname);
        }
    } while (FindNextFile(h,FindData));
    FindClose(h);
    RemoveDirectory (dir);
    free(fullname); free(dirstar);
}
#endif


/******************************************************************************
** Реализация команды получения листинга архива *******************************
******************************************************************************/
#ifdef FREEARC_GUI
void ListHeader (COMMAND &) {}
void ListFooter (COMMAND &) {}
void ListFiles (DIRECTORY_BLOCK *, COMMAND &) {}
#else

uint64 total_files, total_bytes, total_packed;

void ListHeader (COMMAND &command)
{
  if (command.cmd=='l')
      printf ("Date/time                  Size Filename\n"
              "----------------------------------------\n");
  else
      printf ("Date/time              Attr            Size          Packed      CRC Filename\n"
              "-----------------------------------------------------------------------------\n");
  total_files=total_bytes=total_packed=0;
}

void ListFooter (COMMAND &command)
{
  if (command.cmd=='l')
      printf ("----------------------------------------\n");
  else
      printf ("-----------------------------------------------------------------------------\n");
  printf ("%.0lf files, %.0lf bytes, %.0lf compressed", double(total_files), double(total_bytes), double(total_packed));
}

void ListFiles (DIRECTORY_BLOCK *dirblock, COMMAND &command)
{
  int  b=0;                // current_data_block
  bool Encrypted = FALSE;  // текущий солид-блок зашифрован?
  uint64 packed=0;
  iterate_var (i, dirblock->total_files) {
    // Увеличим номер солид-блока если мы вышли за последний принадлежащий ему файл
    if (i >= dirblock->block_end(b))
      b++;
    // Если это первый файл в солид-блоке - соберём block-related информацию
    if (i == dirblock->block_start(b))
    { // Запишем на первый файл в блоке весь его упакованный размер
      packed = dirblock->data_block[b].compsize;
      // Запомним информацию о солид-блоке для использования её со всеми файлами из этого солид-блока
      char *c = dirblock->data_block[b].compressor;
      Encrypted = strstr (c, "+aes-")!=NULL || strstr (c, "+serpent-")!=NULL || strstr (c, "+blowfish-")!=NULL || strstr (c, "+twofish-")!=NULL;
    }


    if (command.accept_file (dirblock, i)) { //   Если этот файл требуется обработать
      unsigned long long filesize = dirblock->size[i];
      char timestr[100];  FormatDateTime (timestr, 100, dirblock->time[i]);

      if (command.cmd=='l')
          printf (dirblock->isdir[i]? "%s       -dir-" : "%s %11.0lf", timestr, double(filesize));
      else
          printf ("%s %s %15.0lf %15.0lf %08x", timestr, dirblock->isdir[i]? ".D.....":".......", double(filesize), double(packed), dirblock->crc[i]);
      printf ("%c", Encrypted? '*':' ');

      // Print filename using console encoding
      static char filename[MY_FILENAME_MAX*4];
      dirblock->fullname (i, filename);
      static MYFILE file;  file.setname (filename);
      printf ("%s\n", file.displayname());

      total_files++;
      total_bytes  += filesize;
      total_packed += packed;    packed = 0;
    }
  }
}
#endif


/******************************************************************************
** Основная программа *********************************************************
******************************************************************************/

// Run setup.exe after unpacking
void RunSetup (COMMAND &command)
{
#ifdef FREEARC_INSTALLER
  if (command.runme)
  {
      CFILENAME tmp  = (TCHAR*) malloc (MY_FILENAME_MAX * 4);
      CFILENAME tmp2 = (TCHAR*) malloc (MY_FILENAME_MAX * 4);

      // Execute command.runme in the directory command.outpath
      RunProgram (utf8_to_utf16 (command.runme, tmp), utf8_to_utf16 (command.outpath, tmp2), command.wipeoutdir);

      // Wipe outdir after installation was completed
      if (command.wipeoutdir)
          wipedir (utf8_to_utf16 (command.outpath, tmp));

      free(tmp); free(tmp2);
  }
#endif
}

int main (int argc, char *argv[])
{
  SetCompressionThreads (GetProcessorsCount());
  UI.DisplayHeader (HEADER1 NAME);
  COMMAND command (argc, argv);    // Распарсить команду
  if (command.ok)                  // Если парсинг был удачен и можно выполнить команду
    PROCESS (command, UI),         //   Выполнить разобранную команду
    RunSetup (command);            //   Выполнить setup.exe для инсталятора
  printf ("\n");
  return command.ok? EXIT_SUCCESS : FREEARC_EXIT_ERROR;
}

