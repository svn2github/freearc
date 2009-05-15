#include <stdio.h>
#include <sys/stat.h>
#include <utime.h>
#include <limits.h>
#include <memory.h>
#include "Environment.h"
#include "Compression/Compression.h"

#ifdef FREEARC_WIN

#include <shlobj.h>

static int CALLBACK BrowseCallbackProc(HWND hwnd, UINT uMsg, LPARAM lParam, LPARAM lpData)
{
  if(uMsg == BFFM_INITIALIZED)
    PostMessage(hwnd, BFFM_SETSELECTION, TRUE, lpData);

  return 0;
}

// Дать пользователю выбрать каталог
int BrowseForFolder(TCHAR *prompt, TCHAR *in_filename, TCHAR *out_filename)
{
  BROWSEINFO bi;
  bi.hwndOwner = GetActiveWindow();
  bi.lParam = (LONG)in_filename;
  bi.lpszTitle = prompt;
  bi.lpfn = BrowseCallbackProc;
  bi.pidlRoot = NULL;
  bi.pszDisplayName = out_filename;
  bi.ulFlags = BIF_RETURNONLYFSDIRS | BIF_USENEWUI;

  LPITEMIDLIST pItemIdList = SHBrowseForFolder(&bi);

  int result = 0;
  if(pItemIdList != NULL)
  {
    if (SHGetPathFromIDList(pItemIdList, out_filename))
      result = 1;

    IMalloc *iMalloc = 0;
    if(SUCCEEDED(SHGetMalloc(&iMalloc)))
    {
      iMalloc->Free(pItemIdList);
      iMalloc->Release();
    }
  }
  return result;
}


// Дать пользователю выбрать файл
int BrowseForFile(TCHAR *prompt, TCHAR *filters, TCHAR *in_filename, TCHAR *out_filename)
{
  OPENFILENAME ofn;
  ZeroMemory (&ofn, sizeof(ofn));
  ofn.lStructSize = sizeof(ofn);
  ofn.hwndOwner   = GetActiveWindow();
  ofn.lpstrFilter = filters;
  ofn.lpstrFile   = out_filename;
  ofn.nMaxFile    = MY_FILENAME_MAX*2;
  ofn.lpstrTitle  = prompt;

  // If "initial filename" looks like "something\." or "something\", it's just initial dir, really
  if (in_filename[_tcslen(in_filename)-1] == _T('.')  &&
      in_filename[_tcslen(in_filename)-2] == _T('\\'))
  {
    in_filename[_tcslen(in_filename)-1] == _T('\0');
    ofn.lpstrInitialDir = in_filename;
    _tcscpy (out_filename, _T(""));
  }
  else if (in_filename[_tcslen(in_filename)-1] == _T('\\'))
  {
    ofn.lpstrInitialDir = in_filename;
    _tcscpy (out_filename, _T(""));
  }
  else
  {
    _tcscpy (out_filename, in_filename);
  }

  return GetOpenFileName(&ofn)? 1 : 0;
}

// Превратить время/дату файла в строку в соответствии с настройками locale или заданными форматами времени и даты
void GuiFormatDateTime (time_t t, char *buf, int bufsize, char *date_format, char *time_format)
{
  if (t==-1)  t=0;  // Иначе получим вылет :(

  FILETIME ft1, ft2;
  UnixTimeToFileTime (t, &ft1);
  FileTimeToLocalFileTime (&ft1, &ft2);
  SYSTEMTIME datetime;
  FileTimeToSystemTime (&ft2, &datetime);

  GetDateFormatA(LOCALE_USER_DEFAULT, 0, &datetime, date_format, buf, bufsize);
  char *p = str_end(buf);
  *p++ = ' ';
  GetTimeFormatA(LOCALE_USER_DEFAULT, 0, &datetime, time_format, p, bufsize - (p-buf));
}

// Delete entrire subtree from Windows Registry
DWORD RegistryDeleteTree(HKEY hStartKey, LPTSTR pKeyName)
{
   const int MAX_KEY_LENGTH = MAX_PATH;
   DWORD   dwRtn, dwSubKeyLength;
   TCHAR   szSubKey[MAX_KEY_LENGTH];
   HKEY    hKey;

   // Do not allow NULL or empty key name
   if ( pKeyName &&  lstrlen(pKeyName))
   {
      if( (dwRtn=RegOpenKeyEx(hStartKey,pKeyName,
         0, KEY_ENUMERATE_SUB_KEYS | DELETE, &hKey )) == ERROR_SUCCESS)
      {
         while (dwRtn == ERROR_SUCCESS )
         {
            dwSubKeyLength = MAX_KEY_LENGTH;
            dwRtn=RegEnumKeyEx(
                           hKey,
                           0,       // always index zero
                           szSubKey,
                           &dwSubKeyLength,
                           NULL,
                           NULL,
                           NULL,
                           NULL
                         );

            if(dwRtn == ERROR_NO_MORE_ITEMS)
            {
               dwRtn = RegDeleteKey(hStartKey, pKeyName);
               break;
            }
            else if(dwRtn == ERROR_SUCCESS)
               dwRtn=RegistryDeleteTree(hKey, szSubKey);
         }
         RegCloseKey(hKey);
         // Do not save return code because error
         // has already occurred
      }
   }
   else
      dwRtn = ERROR_BADKEY;

   return dwRtn;
}

#endif // Windows/Unix

