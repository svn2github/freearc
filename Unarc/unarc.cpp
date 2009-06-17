// to do: отбор файлов по именам ("name" или "dir/name"),
//        дешифрование данных/заголовка
//        добавление ".arc", listfiles/-ap/-kb

// Обработка сбоев при распаковке архива
#undef  ON_CHECK_FAIL
#define ON_CHECK_FAIL()   UnarcQuit()
void UnarcQuit();

// Доступ к структуре архива, парсингу командной строки и выполнению операций над архивом
#include "ArcStructure.h"
#include "ArcCommand.h"
#include "ArcProcess.h"

// Экстренный выход из программы в случае ошибки
void UnarcQuit()
{
  CurrentProcess->quit();
}


// Весь диалог с пользователем описан в сменных модулях, включаемых здесь
#ifndef FREEARC_GUI
#include "CUI.h"
CUI UI;
#else
#include "gui\gui.h"
#include "gui\gui.cpp"
#ifndef FREEARC_INSTALLER
GUI UI;
#else

/******************************************************************************
** Installer support **********************************************************
******************************************************************************/

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

class INSTALLER_GUI : public GUI
{
    // Run setup.exe after unpacking
    void EndProgress (COMMAND *cmd)
    {
    	GUI::EndProgress (cmd);
        if (cmd->runme)
        {
            CFILENAME tmp  = (TCHAR*) malloc (MY_FILENAME_MAX * 4);
            CFILENAME tmp2 = (TCHAR*) malloc (MY_FILENAME_MAX * 4);

            // Execute cmd->runme in the directory cmd->outpath
            RunProgram (utf8_to_utf16 (cmd->runme, tmp), utf8_to_utf16 (cmd->outpath, tmp2), cmd->wipeoutdir);

            // Wipe outdir after installation was completed
            if (cmd->wipeoutdir)
                wipedir (utf8_to_utf16 (cmd->outpath, tmp));

            free(tmp); free(tmp2);
        }
    }


    // Wipe temporary outdir on unsuccesful extraction
    virtual void Abort (COMMAND *cmd)
    {
        if (cmd->tempdir)
        {
            CFILENAME tmp  =  (TCHAR*) malloc (MY_FILENAME_MAX * 4);
            wipedir (utf8_to_utf16 (cmd->outpath, tmp));
            free(tmp);
        }
        GUI::Abort (cmd);
    }
} UI;
#endif
#endif


/******************************************************************************
** Основная программа *********************************************************
******************************************************************************/

int main (int argc, char *argv[])
{
  UI.DisplayHeader (HEADER1 NAME);
  COMMAND command (argc, argv);    // Распарсить команду
  if (command.ok)                  // Если парсинг был удачен и можно выполнить команду
    PROCESS (command, UI);         //   Выполнить разобранную команду
  printf ("\n");
  return command.ok? EXIT_SUCCESS : FREEARC_EXIT_ERROR;
}

