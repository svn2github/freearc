//---------------------------------------------------------------------------
// Copyright 2002-2008 Andre Burgaud <andre@burgaud.com>
// See license.txt
// $Id: ArcShellExt.cpp 497 2008-12-07 03:17:37Z andre $
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// ArcShellExt.cpp
// Defines the entry point for the DLL application.
//---------------------------------------------------------------------------

#define _CRT_SECURE_NO_WARNINGS

#ifndef STRICT
#define STRICT
#endif

#define INC_OLE2

#include <windows.h>
#include <windowsx.h>
#include <shlobj.h>
#include <io.h>
#include <fcntl.h>
#include <tchar.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "Lua/lua.hpp"

#define GUID_SIZE 128
#define MY_FILENAME_MAX 65536  /* maximum filename size */
#define MY_CMDLINE_MAX 32000  /* maximum cmdline size */
#define ResultFromShort(i) ResultFromScode(MAKE_SCODE(SEVERITY_SUCCESS, 0, (USHORT)(i)))
#define MYARRAYSIZE(arr) (sizeof(arr)/sizeof(*(arr)))

#pragma data_seg(".text")
#define INITGUID
#include <initguid.h>
#include <shlguid.h>
#include "resource.h"
#include "ArcShellExt.h"
#pragma data_seg()

//---------------------------------------------------------------------------
//  Global variables
//---------------------------------------------------------------------------
UINT _cRef = 0; // COM Reference count.
HINSTANCE _hModule = NULL; // DLL Module.

typedef struct{
  HKEY  hRootKey;
  LPTSTR szSubKey;
  LPTSTR lpszValueName;
  LPTSTR szData;
} DOREGSTRUCT, *LPDOREGSTRUCT;

#ifdef ISS_JOINER
#define szShellExtensionTitle _T("IssJoiner")
#else
#define szShellExtensionTitle _T("FreeArc")
#endif

BOOL RegisterServer(CLSID, LPTSTR);
BOOL UnregisterServer(CLSID, LPTSTR);
void MsgBox(LPTSTR);
void MsgBoxDebug(LPTSTR);
void MsgBoxError(LPTSTR);

//---------------------------------------------------------------------------
// DllMain
//---------------------------------------------------------------------------
extern "C" int APIENTRY
DllMain(HINSTANCE hInstance, DWORD dwReason, LPVOID lpReserved) {
  if (dwReason == DLL_PROCESS_ATTACH) {
    _hModule = hInstance;
  }
  return 1;
}

//---------------------------------------------------------------------------
// DllCanUnloadNow
//---------------------------------------------------------------------------
STDAPI DllCanUnloadNow(void) {
  return (_cRef == 0 ? S_OK : S_FALSE);
}

//---------------------------------------------------------------------------
// DllGetClassObject
//---------------------------------------------------------------------------
STDAPI DllGetClassObject(REFCLSID rclsid, REFIID riid, LPVOID *ppvOut) {
  *ppvOut = NULL;
  if (IsEqualIID(rclsid, CLSID_ShellExtension)) {
    CShellExtClassFactory *pcf = new CShellExtClassFactory;
    return pcf->QueryInterface(riid, ppvOut);
  }
  return CLASS_E_CLASSNOTAVAILABLE;
}

//---------------------------------------------------------------------------
// DllRegisterServer
//---------------------------------------------------------------------------
STDAPI DllRegisterServer() {
  return (RegisterServer(CLSID_ShellExtension, szShellExtensionTitle) ? S_OK : E_FAIL);
}

//---------------------------------------------------------------------------
// DllUnregisterServer
//---------------------------------------------------------------------------
STDAPI DllUnregisterServer(void) {
  return (UnregisterServer(CLSID_ShellExtension, szShellExtensionTitle) ? S_OK : E_FAIL);
}

//---------------------------------------------------------------------------
// RegisterServer
//---------------------------------------------------------------------------
BOOL RegisterServer(CLSID clsid, LPTSTR lpszTitle) {
  int      i;
  HKEY     hKey;
  LRESULT  lResult;
  DWORD    dwDisp;
  TCHAR    szSubKey[MAX_PATH];
  TCHAR    szData[MAX_PATH];
  TCHAR    szCLSID[MAX_PATH];
  TCHAR    szModule[MAX_PATH];
  LPWSTR   pwsz;

  StringFromIID(clsid, &pwsz);
  if(pwsz) {
#ifdef UNICODE
    lstrcpy(szCLSID, pwsz);
#else
    WideCharToMultiByte(CP_ACP, 0, pwsz, -1, szCLSID, MYARRAYSIZE(szCLSID), NULL, NULL);
#endif
    //free the string
    LPMALLOC pMalloc;
    CoGetMalloc(1, &pMalloc);
    pMalloc->Free(pwsz);
    pMalloc->Release();
  }

  //If running on NT, register the extension as approved.
  OSVERSIONINFO  osvi;

  osvi.dwOSVersionInfoSize = sizeof(osvi);
  GetVersionEx(&osvi);

  //get this app's path and file name
  GetModuleFileName(_hModule, szModule, MAX_PATH);

  DOREGSTRUCT ClsidEntries[] = {
    HKEY_CLASSES_ROOT,   TEXT("CLSID\\%s"),                                                                NULL,                   lpszTitle,
    HKEY_CLASSES_ROOT,   TEXT("CLSID\\%s\\InprocServer32"),                                                NULL,                   szModule,
    HKEY_CLASSES_ROOT,   TEXT("CLSID\\%s\\InprocServer32"),                                                TEXT("ThreadingModel"), TEXT("Apartment"),
    HKEY_CLASSES_ROOT,   TEXT("*\\shellex\\ContextMenuHandlers\\" szShellExtensionTitle),                  NULL,                   szCLSID,
    HKEY_CLASSES_ROOT,   TEXT("Folder\\shellex\\ContextMenuHandlers\\" szShellExtensionTitle),             NULL,                   szCLSID,
    HKEY_LOCAL_MACHINE,  TEXT("Software\\Microsoft\\Windows\\CurrentVersion\\Shell Extensions\\Approved"), szCLSID,                lpszTitle,
    NULL,                NULL,                                                                             NULL,                   NULL
  };

  // Register the CLSID entries
  for(i = 0; ClsidEntries[i].hRootKey; i++) {
    // NT needs to have shell extensions "approved".
    if (ClsidEntries[i].hRootKey==HKEY_CLASSES_ROOT  ||  VER_PLATFORM_WIN32_NT==osvi.dwPlatformId)
    {
      // Create the sub key string - for this case, insert the file extension
      wsprintf(szSubKey, ClsidEntries[i].szSubKey, szCLSID);
      lResult = RegCreateKeyEx(ClsidEntries[i].hRootKey, szSubKey, 0, NULL, REG_OPTION_NON_VOLATILE, KEY_WRITE, NULL, &hKey, &dwDisp);
      if(NOERROR == lResult) {
        // If necessary, create the value string
        wsprintf(szData, ClsidEntries[i].szData, szModule);
        lResult = RegSetValueEx(hKey, ClsidEntries[i].lpszValueName, 0, REG_SZ, (LPBYTE)szData, (lstrlen(szData) + 1) * sizeof(TCHAR));
        RegCloseKey(hKey);
      }
      else
        return FALSE;
    }
  }

  SHChangeNotify (SHCNE_ASSOCCHANGED, SHCNF_IDLIST, NULL, NULL);
  return TRUE;
}

//---------------------------------------------------------------------------
// UnregisterServer
//---------------------------------------------------------------------------
BOOL UnregisterServer(CLSID clsid, LPTSTR lpszTitle) {
  TCHAR szCLSID[GUID_SIZE + 1];
  TCHAR szCLSIDKey[GUID_SIZE + 32];
  TCHAR szKeyTemp[MAX_PATH + GUID_SIZE];
  LPWSTR pwsz;

  StringFromIID(clsid, &pwsz);
  if(pwsz) {
#ifdef UNICODE
    lstrcpy(szCLSID, pwsz);
#else
    WideCharToMultiByte(CP_ACP, 0, pwsz, -1, szCLSID, MYARRAYSIZE(szCLSID), NULL, NULL);
#endif
    //free the string
    LPMALLOC pMalloc;
    CoGetMalloc(1, &pMalloc);
    pMalloc->Free(pwsz);
    pMalloc->Release();
  }

  lstrcpy(szCLSIDKey, TEXT("CLSID\\"));
  lstrcat(szCLSIDKey, szCLSID);

  HKEY     hKey;
  LRESULT  lResult = RegOpenKeyEx(HKEY_LOCAL_MACHINE, TEXT("Software\\Microsoft\\Windows\\CurrentVersion\\Shell Extensions\\Approved"), 0, KEY_SET_VALUE, &hKey);
  if(NOERROR == lResult)
  {
    RegDeleteValue(hKey, szCLSID);
    RegCloseKey(hKey);
  }

  wsprintf(szKeyTemp, TEXT("Folder\\shellex\\ContextMenuHandlers\\%s"), lpszTitle);
  RegDeleteKey(HKEY_CLASSES_ROOT, szKeyTemp);

  wsprintf(szKeyTemp, TEXT("*\\shellex\\ContextMenuHandlers\\%s"), lpszTitle);
  RegDeleteKey(HKEY_CLASSES_ROOT, szKeyTemp);

  wsprintf(szKeyTemp, TEXT("%s\\%s"), szCLSIDKey, TEXT("InprocServer32"));
  RegDeleteKey(HKEY_CLASSES_ROOT, szKeyTemp);
  RegDeleteKey(HKEY_CLASSES_ROOT, szCLSIDKey);

  SHChangeNotify (SHCNE_ASSOCCHANGED, SHCNF_IDLIST, NULL, NULL);
  return TRUE;
}

//---------------------------------------------------------------------------
// MsgBoxDebug
//---------------------------------------------------------------------------
void MsgBoxDebug(LPCSTR lpszMsg) {
  wchar_t msgW[MAX_PATH];
  MultiByteToWideChar (CP_UTF8, 0, lpszMsg, -1, msgW, MAX_PATH);

  MessageBoxW (NULL,
               msgW,
               L"DEBUG: FreeArc Shell Extension",
               MB_OK);
}

//---------------------------------------------------------------------------
// MsgBox
//---------------------------------------------------------------------------
void MsgBox(HWND hParent, LPCSTR lpszMsg) {
  wchar_t msgW[MAX_PATH];
  MultiByteToWideChar (CP_UTF8, 0, lpszMsg, -1, msgW, MAX_PATH);

  MessageBoxW (hParent,
               msgW,
               L"FreeArc Shell Extension",
               MB_OK);
}

//---------------------------------------------------------------------------
// MsgBoxError
//---------------------------------------------------------------------------
void MsgBoxError(HWND hParent, LPCSTR lpszMsg) {
  wchar_t msgW[MAX_PATH];
  MultiByteToWideChar (CP_UTF8, 0, lpszMsg, -1, msgW, MAX_PATH);

  MessageBoxW (hParent,
               msgW,
               L"FreeArc Shell Extension",
               MB_OK | MB_ICONSTOP);
}

//---------------------------------------------------------------------------
// CShellExtClassFactory
//---------------------------------------------------------------------------
CShellExtClassFactory::CShellExtClassFactory() {
  m_cRef = 0L;
  _cRef++;
}

CShellExtClassFactory::~CShellExtClassFactory() {
  _cRef--;
}

STDMETHODIMP CShellExtClassFactory::QueryInterface(REFIID riid, LPVOID FAR *ppv) {
  *ppv = NULL;
  if (IsEqualIID(riid, IID_IUnknown) || IsEqualIID(riid, IID_IClassFactory)) {
    *ppv = (LPCLASSFACTORY)this;
    AddRef();
    return NOERROR;
  }
  return E_NOINTERFACE;
}

STDMETHODIMP_(ULONG) CShellExtClassFactory::AddRef() {
  return ++m_cRef;
}

STDMETHODIMP_(ULONG) CShellExtClassFactory::Release()
{
  if (--m_cRef)
    return m_cRef;
  delete this;
  return 0L;
}

STDMETHODIMP CShellExtClassFactory::CreateInstance(LPUNKNOWN pUnkOuter, REFIID riid, LPVOID *ppvObj) {
  *ppvObj = NULL;
  if (pUnkOuter)
    return CLASS_E_NOAGGREGATION;
  LPCSHELLEXT pShellExt = new CShellExt();
  if (NULL == pShellExt)
    return E_OUTOFMEMORY;
  return pShellExt->QueryInterface(riid, ppvObj);
}

STDMETHODIMP CShellExtClassFactory::LockServer(BOOL fLock) {
  return NOERROR;
}

//---------------------------------------------------------------------------
// CShellExt
//---------------------------------------------------------------------------
static void *l_alloc (void *ud, void *ptr, size_t osize, size_t nsize) {
  (void)ud;
  (void)osize;
  if (nsize == 0) {
    free(ptr);
    return NULL;
  }
  else
    return realloc(ptr, nsize);
}

static int Lua_add_menu_item (lua_State *L) {
  CShellExt *obj;
  lua_getallocf (L, (void**)&obj);
  return obj->add_menu_item();
}

// Lua function that reads chunk of data from file with UTF8-encoded filename
// Call: read_from_file (filename, origin, offset, bufsize)
static int Lua_read_from_file (lua_State *L) {
  const char *filename = luaL_checkstring(L, 1);
  if (!filename)
    return 0;  ////lua_pushstring (L, errormsg); luaL_error / lua_error

  // Convert filename into UTF-16
  TCHAR *filenameW = (TCHAR*) malloc (sizeof(TCHAR) * MY_FILENAME_MAX);
  if (!filenameW)
    return 0;  ////lua_pushstring (L, errormsg); luaL_error / lua_error
  MultiByteToWideChar (CP_UTF8, 0, filename, -1, filenameW, MY_FILENAME_MAX);

  // Where and how many bytes to read
  int origin = luaL_checkinteger(L, 2);
  int offset = luaL_checkinteger(L, 3);
  int size   = luaL_checkinteger(L, 4);

  // Open file
  int f = _wopen (filenameW, _O_BINARY | _O_RDONLY);
  free(filenameW);
  if (f<0)
    return 0;  ////lua_pushstring (L, errormsg); luaL_error / lua_error

  // Alloc buffer
  void *buf = malloc(size);
  if (!buf)
    return 0;  ////lua_pushstring (L, errormsg); luaL_error / lua_error

  // Seek to and read data requested
  _lseek (f, offset, origin);
  int len = _read (f, buf, size);
  _close(f);
  if (len<0)
    return 0;  ////lua_pushstring (L, errormsg); luaL_error / lua_error

  // Return the data read
  lua_pushlstring(L, (const char*)buf, len);
  free(buf);
  return 1;
}

// Lua function that checks directory existance using UTF8-encoded filename
// Call: dir_exists (filename)
static int Lua_dir_exists (lua_State *L) {
  const char *filename = luaL_checkstring(L, 1);
  if (!filename)
    return 0;  ////lua_pushstring (L, errormsg); luaL_error / lua_error

  // Convert filename into UTF-16
  TCHAR *filenameW = (TCHAR*) malloc (sizeof(TCHAR) * MY_FILENAME_MAX);
  MultiByteToWideChar (CP_UTF8, 0, filename, -1, filenameW, MY_FILENAME_MAX);

  struct _stat st;
  _wstat (filenameW, &st);
  if ((st.st_mode & S_IFDIR) != 0)
       lua_pushnumber (L, 1);
  else lua_pushnil (L);
  free(filenameW);
  return 1;
}

CShellExt::CShellExt() {
  m_cRef = 0L;
  m_pDataObj = NULL;
  _cRef++;
  m_hFreeArcBmp = LoadBitmap(_hModule, MAKEINTRESOURCE(IDB_SCITE));
  HRESULT hr;
  hr = SHGetMalloc(&m_pAlloc);
  if (FAILED(hr))
    m_pAlloc = NULL;
  listfile_data = NULL;

  // Create Lua interpreter, register C functions and run Lua scripts to define Lua functions
  L = lua_newstate (l_alloc, this);  luaL_openlibs(L);
  lua_register (L, "add_menu_item",  ::Lua_add_menu_item);
  lua_register (L, "read_from_file", ::Lua_read_from_file);
  lua_register (L, "dir_exists",     ::Lua_dir_exists);
  load_user_funcs();
}

CShellExt::~CShellExt() {
  lua_close(L);

  free(listfile_data);
  if (m_pDataObj)
  m_pDataObj->Release();
  _cRef--;
  m_pAlloc->Release();
}

STDMETHODIMP CShellExt::QueryInterface(REFIID riid, LPVOID FAR *ppv) {
  *ppv = NULL;
  if (IsEqualIID(riid, IID_IShellExtInit) || IsEqualIID(riid, IID_IUnknown)) {
    *ppv = (LPSHELLEXTINIT)this;
  }
  else if (IsEqualIID(riid, IID_IContextMenu)) {
    *ppv = (LPCONTEXTMENU)this;
  }
  if (*ppv) {
    AddRef();
    return NOERROR;
  }
  return E_NOINTERFACE;
}

STDMETHODIMP_(ULONG) CShellExt::AddRef() {
  return ++m_cRef;
}

STDMETHODIMP_(ULONG) CShellExt::Release() {
  if (--m_cRef)
    return m_cRef;
  delete this;
  return 0L;
}

STDMETHODIMP CShellExt::Initialize(LPCITEMIDLIST pIDFolder, LPDATAOBJECT pDataObj, HKEY hRegKey) {
  HRESULT hres = 0;
  if (m_pDataObj)
    m_pDataObj->Release();
  if (pDataObj) {
    m_pDataObj = pDataObj;
    pDataObj->AddRef();
  }
  return NOERROR;
}


//---------------------------------------------------------------------------
// Worker code
//---------------------------------------------------------------------------

void CShellExt::load_user_funcs() {
  TCHAR *szModuleFullNameW = (TCHAR*) malloc (sizeof(TCHAR) * MY_FILENAME_MAX);;
  char  *szModuleFullName  = ( char*) malloc (            4 * MY_FILENAME_MAX);  ;

  GetModuleFileName(_hModule, szModuleFullNameW, MAX_PATH);
  WideCharToMultiByte (CP_UTF8, 0, szModuleFullNameW, -1, szModuleFullName, MY_FILENAME_MAX, NULL, NULL);

  char *filename = strrchr(szModuleFullName, '\\') + 1;

  strcpy (filename, "ArcShellExt-system.lua");
  luaL_dofile (L, szModuleFullName);                           //unicode!

  strcpy (filename, "ArcShellExt-config.lua");
  luaL_dofile (L, szModuleFullName);                           //unicode!

  strcpy (filename, "ArcShellExt-user.lua");
  luaL_dofile (L, szModuleFullName);                           //unicode!

  free(szModuleFullName);
  free(szModuleFullNameW);
}

int CShellExt::add_menu_item() {
  const char *text = luaL_checkstring(L, 1);
  if (!text)
    return 0;  ////lua_pushstring (L, errormsg); luaL_error / lua_error

  wchar_t textW[MAX_PATH];
  MultiByteToWideChar (CP_UTF8, 0, text, -1, textW, MAX_PATH);

  // Submenu flag
  int menu_down = luaL_checkinteger(L, 2);

  // Return from submenu
  int menu_up = luaL_checkinteger(L, 3);
  if (menu_up)
  {
    menu_level -= menu_up;
    hMenu  = menu_stack [menu_level];
    nIndex = index_stack[menu_level];
  }

  HMENU hSubMenu;
  if (menu_down)
  {
     hSubMenu = ::CreateMenu();   ////if (!HMENU)  error
     InsertMenuW(hMenu, nIndex, MF_POPUP|MF_BYPOSITION, (UINT)hSubMenu, textW);
  }
  else
  {
     InsertMenuW(hMenu, nIndex, MF_STRING|MF_BYPOSITION, idCmd, textW);
  }

  if (m_hFreeArcBmp) {
    SetMenuItemBitmaps (hMenu, nIndex, MF_BYPOSITION, m_hFreeArcBmp, m_hFreeArcBmp);
  }

  nIndex++;

  if (menu_down)
  {
     menu_stack[menu_level]  = hMenu;
     index_stack[menu_level] = nIndex;
     menu_level++;
     hMenu = hSubMenu;
     nIndex = 0;
  }

  lua_pushnumber (L, idCmd);
  idCmd++;
  return 1;
}

// Insert items into context menu
STDMETHODIMP CShellExt::QueryContextMenu(HMENU _hMenu, UINT _nIndex, UINT _idCmdFirst, UINT idCmdLast, UINT uFlags) {

  // If the flags include CMF_DEFAULTONLY then we shouldn't do anything.
  if ( uFlags & CMF_DEFAULTONLY )
    return MAKE_HRESULT (SEVERITY_SUCCESS, FACILITY_NULL, 0);

  // Init class variables
  hMenu  = _hMenu;
  nIndex = _nIndex;
  idCmd  = idCmdFirst = _idCmdFirst;
  menu_level = 0;

  FORMATETC fmte = {CF_HDROP, (DVTARGETDEVICE FAR *)NULL, DVASPECT_CONTENT, -1, TYMED_HGLOBAL};
  HRESULT hres = m_pDataObj->GetData(&fmte, &m_stgMedium);

  if (SUCCEEDED(hres) && m_stgMedium.hGlobal)
  {
    // Number of files selected
    int numFiles = DragQueryFile((HDROP)m_stgMedium.hGlobal, (UINT)-1, 0, 0);

    if (numFiles)
    {
      // Call build_menu, passing list of files selected
      lua_getglobal (L, "build_menu");

      if (!lua_checkstack (L, numFiles))
        return 0;////error

      // Push UTF8 names of files selected to the Lua stack and save them to filenames[]
      TCHAR *WSelectedFilename = (TCHAR*) malloc (sizeof(TCHAR) * MY_FILENAME_MAX);
      int    total_size = 0;
      for (UINT i = 0; i < numFiles; i++)
      {
        DragQueryFile ((HDROP)m_stgMedium.hGlobal, i, WSelectedFilename, MY_FILENAME_MAX);
        WideCharToMultiByte (CP_UTF8, 0, WSelectedFilename, -1, SelectedFilename, MY_FILENAME_MAX, NULL, NULL);
        lua_pushstring (L, SelectedFilename);
        total_size += strlen(strrchr(SelectedFilename,'\\'));
      }

      // Concat all filenames to one buffer
      char *p = listfile_data = (char*) malloc (sizeof(char) * (total_size+1));
      for (UINT i = 0; i < numFiles; i++)
      {
        DragQueryFile ((HDROP)m_stgMedium.hGlobal, i, WSelectedFilename, MY_FILENAME_MAX);
        WideCharToMultiByte (CP_UTF8, 0, WSelectedFilename, -1, SelectedFilename, MY_FILENAME_MAX, NULL, NULL);
        strcpy (p, strrchr(SelectedFilename,'\\')+1);
        if (i<numFiles)  strcat (p, "\n");
        p = strchr(p, '\0');
      }

      if (lua_pcall(L, numFiles, 0, 0) != 0)
        return 0;////error
        ;//error(L, "error running function `f': %s",
         //        lua_tostring(L, -1));
    }
  }

  return ResultFromShort(idCmd-idCmdFirst);
}

// Return help to show in the status line
STDMETHODIMP CShellExt::GetCommandString(UINT_PTR idCmd, UINT uFlags, UINT FAR *reserved, LPSTR pszName, UINT cchMax) {

  if (uFlags == GCS_HELPTEXTA || uFlags == GCS_HELPTEXTW)
  {
    lua_getglobal  (L, "get_help");
    lua_pushnumber (L, idCmdFirst+idCmd);

    if (lua_pcall(L, 1, 1, 0) != 0)
      return NOERROR;////error

    if (!lua_isstring(L, -1))
      return NOERROR;//error(L, "function `f' must return a string");
    const char *z = lua_tostring(L, -1);

    if (uFlags == GCS_HELPTEXTA) {
      wchar_t buf[MAX_PATH];
      MultiByteToWideChar (CP_UTF8, 0, z,   -1, buf, MAX_PATH);
      WideCharToMultiByte (CP_ACP,  0, buf, -1, pszName, cchMax, NULL, NULL);
    }

    if (uFlags == GCS_HELPTEXTW)
      MultiByteToWideChar (CP_UTF8, 0, z, -1, (LPWSTR)pszName, cchMax);

    lua_pop(L, 1);  /* pop returned value */
  }
  return NOERROR;
}

// Perform selected command
STDMETHODIMP CShellExt::InvokeCommand(LPCMINVOKECOMMANDINFO lpcmi) {
  HRESULT hr = E_INVALIDARG;

  if (!HIWORD(lpcmi->lpVerb)) {
    UINT idCmd = LOWORD(lpcmi->lpVerb);

    lua_getglobal  (L, "get_command");
    lua_pushnumber (L, idCmdFirst+idCmd);

    if (lua_pcall(L, 1, 1, 0) != 0)
      return hr;//error(L, "error running function `f': %s",
                //        lua_tostring(L, -1));

    if (!lua_isstring(L, -1))
      return hr;//error(L, "function `f' must return a string");
    const char *z = lua_tostring(L, -1);

    hr = RunProgram (lpcmi->hwnd, lpcmi->lpDirectory, z, lpcmi->lpParameters, lpcmi->nShow);

    lua_pop(L, 1);  /* pop returned value after use */
  }
  return hr;
}


// ****************************************************************************************
// Execution of command that user has been selected ***************************************
// ****************************************************************************************

// Save buffer contents to tempfile and return its name
TCHAR *SaveDataToTempFile (char *buffer, char *message)
{
  HANDLE hTempFile;
  DWORD  dwRetVal;
  DWORD  dwBytesWritten;
  UINT   uRetVal;
  TCHAR *lpPathBuffer = (TCHAR*) malloc (sizeof(TCHAR) * MY_FILENAME_MAX);
  TCHAR *szTempName   = (TCHAR*) malloc (sizeof(TCHAR) * MY_FILENAME_MAX);
  BOOL   fSuccess;

  // Check mallocs
  if (!lpPathBuffer || !szTempName)
  {
    sprintf (message, "malloc failed\n");
    return NULL;
  }

   // Get the temp path
  dwRetVal = GetTempPath(MY_FILENAME_MAX,     // length of the buffer
                         lpPathBuffer);       // buffer for path
  if (dwRetVal > MY_FILENAME_MAX  ||  dwRetVal == 0)
  {
    sprintf (message, "GetTempPath failed (%d)\n", GetLastError());
    return NULL;
  }

  // Ask for a unique temporary file name
  uRetVal = GetTempFileName(lpPathBuffer,      // directory for tmp files
                            TEXT("filelist"),  // temp file name prefix
                            0,                 // create unique name
                            szTempName);       // buffer for name
  if (uRetVal == 0)
  {
    sprintf (message, "GetTempFileName failed (%d)\n", GetLastError());
    return NULL;
  }
  free(lpPathBuffer);


  // Create the tempfile
  hTempFile = CreateFile(szTempName,           // file name
                         GENERIC_WRITE,        // open for writing
                         0,                    // do not share
                         NULL,                 // default security
                         CREATE_ALWAYS,        // overwrite existing
                         FILE_ATTRIBUTE_NORMAL,// normal file
                         NULL);                // no template
  if (hTempFile == INVALID_HANDLE_VALUE)
  {
    sprintf (message, "CreateFile failed (%d)\n", GetLastError());
    return NULL;
  }

  // Save data to file
  fSuccess = WriteFile(hTempFile,
                       buffer,
                       strlen(buffer),
                       &dwBytesWritten,
                       NULL);
  if (!fSuccess || dwBytesWritten!=strlen(buffer))
  {
    sprintf (message, "WriteFile failed (%d)\n", GetLastError());
    return NULL;
  }

  // Close the handle to the file.
  fSuccess = CloseHandle (hTempFile);
  if (!fSuccess)
  {
    sprintf (message, "CloseHandle failed (%d)\n", GetLastError());
    return NULL;
  }

  return szTempName;
}

// Replace substring one time
TCHAR *str_replace (TCHAR *str, TCHAR *from, const TCHAR *to)
{
  TCHAR *s = (TCHAR*) malloc (sizeof(TCHAR) * (_tcslen(str)+_tcslen(to)+1));
  if (!s)  return NULL;
  TCHAR *p = _tcsstr(str, from);
  if (p) {
    _stprintf(s, _T("%.*s%s%s"), p-str, str, to, p+_tcslen(from));
  } else {
    _tcscpy(s,str);
  }
  return s;
}

struct Param
{
  HANDLE hProcess;
  TCHAR *listfile;
};

// Wait for program finish and then delete the list file
static DWORD WINAPI WaitProgramFinish (void *paramPtr)
{
  Param *param = (Param*) paramPtr;
  WaitForSingleObject (param->hProcess, INFINITE);
  CloseHandle (param->hProcess);
  DeleteFile (param->listfile),
  free (param->listfile);
  free (param);
  return 0;
}

STDMETHODIMP CShellExt::RunProgram (HWND hParent, LPCSTR pszWorkingDir, LPCSTR cmd0, LPCSTR pszParam, int iShowCmd)
{
  char message[MAX_PATH], cmdBuf[MAX_PATH+100];

  // Find current directory - it's the name of any file selected minus last part
  char* pDest = strrchr(SelectedFilename, '\\');
  if (pDest==SelectedFilename || pDest[-1]==':')  pDest++;
  pDest[0] = 0;
  TCHAR *CurrentDirW = (TCHAR*) malloc (sizeof(TCHAR) * MY_FILENAME_MAX);
  MultiByteToWideChar (CP_UTF8, 0, SelectedFilename, -1, CurrentDirW, MY_FILENAME_MAX);

  // Данные, которые нужно записать во временный файл
  char *data_to_write = NULL;
  if (strlen(cmd0) > MY_CMDLINE_MAX)
  {
    char *p = cmd0[0]=='"'? strchr((char*)cmd0+1, '"')+1 : strchr((char*)cmd0, ' ');
    // Превратить команду в "freearc @cmd"
    sprintf (cmdBuf, "%.*s @{listfile}", p-cmd0, cmd0);
    cmd0 = cmdBuf;
    // Записать остаток команды во временный файл
    while (*p==' ')  p++;
    data_to_write = p;
  }
  else if (strstr(cmd0, "{listfile}"))
  {
    data_to_write = listfile_data;
  }

  // Save listfile data to tempfile
  TCHAR *listfile = data_to_write? SaveDataToTempFile (data_to_write, message) : NULL;
  if (data_to_write && !listfile) {
    MsgBoxError (hParent, message);
    return NOERROR;////error
  }

  // Convert cmd from UTF-8 to UTF-16
  TCHAR *cmdW0 = (TCHAR*) malloc ((strlen(cmd0)+1) * sizeof(*cmdW0));
  if (!cmdW0) {
    MsgBoxError (hParent, "Memory allocation error");
    return NOERROR;////error
  }
  MultiByteToWideChar (CP_UTF8, 0, cmd0, -1, cmdW0, strlen(cmd0)+1);

  // Substitute listfile name in cmd
  TCHAR *cmdW = str_replace(cmdW0, _T("{listfile}"), listfile);
  free(cmdW0);
  if (!cmdW) {
    MsgBoxError (hParent, "Memory allocation error");
    return NOERROR;////error
  }

  STARTUPINFOW si;
  PROCESS_INFORMATION pi;
  ZeroMemory(&si, sizeof(si));
  si.cb = sizeof(si);
  si.dwFlags = STARTF_USESHOWWINDOW;
  si.wShowWindow = SW_RESTORE;
  if (CreateProcessW (NULL, cmdW, NULL, NULL, FALSE, 0, NULL, CurrentDirW, &si, &pi)) {
    CloseHandle (pi.hThread);

    // Если есть файл, который нужно стереть после выполнения команды
    if (listfile)
    {
      Param *param = (Param*) malloc(sizeof(Param));
      param->hProcess = pi.hProcess;
      param->listfile = listfile;

      CreateThread(
              NULL,              // default security attributes
              0,                 // use default stack size
              WaitProgramFinish, // thread function
              param,             // argument to thread function
              0,                 // use default creation flags
              NULL);             // returns the thread identifier (Win9x-incompatible)
    } else {
      CloseHandle (pi.hProcess);
    }
  } else {
    sprintf(message, "Cannot run program: %.*s", MAX_PATH-100, cmd0);
    MsgBoxError (hParent, message);
  }
  free(CurrentDirW);
  free(cmdW);

  return NOERROR;
}

//to do
// arbitrary actions
// icons
// multiple *-user.lua files
// persistent Lua_state auto-reloaded on ArcShell*.lua changes
// GCS_VERB
// memory management - use SHMalloc

// "unicode!" - labeled things that are unicode-incompatible
