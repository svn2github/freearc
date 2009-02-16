#include <stdio.h>
#include <sys/stat.h>
#include <utime.h>
#include <limits.h>
#include <memory.h>
#include "Environment.h"
#include "Compression/Compression.h"

#ifdef FREEARC_WIN

#include <shlobj.h>
#include <commdlg.h>

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
  bi.ulFlags = BIF_RETURNONLYFSDIRS;

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
int BrowseForFile(TCHAR *prompt, TCHAR *in_filename, TCHAR *out_filename)
{
  OPENFILENAME ofn;
  ZeroMemory (&ofn, sizeof(ofn));
  ofn.lStructSize = sizeof(ofn);
  ofn.hwndOwner   = GetActiveWindow();
  ofn.lpstrFile   = out_filename;
  ofn.nMaxFile    = MY_FILENAME_MAX;

  _tcscpy (out_filename, in_filename);

  return GetOpenFileName(&ofn)? 1 : 0;
}

#endif // Windows/Unix

