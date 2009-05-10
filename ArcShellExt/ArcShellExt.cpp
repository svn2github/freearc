//---------------------------------------------------------------------------
// Copyright 2002-2008 Andre Burgaud <andre@burgaud.com>
// See license.txt
// $Id: ArcShellExt.cpp 497 2008-12-07 03:17:37Z andre $
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// ArcShellExt.cpp
// Defines the entry point for the DLL application.
//---------------------------------------------------------------------------

#ifndef STRICT
#define STRICT
#endif

#define INC_OLE2

#include <windows.h>
#include <windowsx.h>
#include <shlobj.h>

#include "Lua\lua.hpp"

#define GUID_SIZE 128
#define MAX_FILES 10
#define MAX_CMDSTR (MAX_PATH * MAX_FILES)
#define ResultFromShort(i) ResultFromScode(MAKE_SCODE(SEVERITY_SUCCESS, 0, (USHORT)(i)))

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

char szShellExtensionTitle[] = "FreeArc";

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
    WideCharToMultiByte(CP_ACP, 0, pwsz, -1, szCLSID, ARRAYSIZE(szCLSID), NULL, NULL);
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
    HKEY_CLASSES_ROOT,   TEXT("*\\shellex\\ContextMenuHandlers\\FreeArc"),                                 NULL,                   szCLSID,
    HKEY_CLASSES_ROOT,   TEXT("Folder\\shellex\\ContextMenuHandlers\\FreeArc"),                            NULL,                   szCLSID,
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
    WideCharToMultiByte(CP_ACP, 0, pwsz, -1, szCLSID, ARRAYSIZE(szCLSID), NULL, NULL);
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

  return TRUE;
}

//---------------------------------------------------------------------------
// MsgBoxDebug
//---------------------------------------------------------------------------
void MsgBoxDebug(LPTSTR lpszMsg) {
  MessageBox(NULL,
             lpszMsg,
             "DEBUG",
             MB_OK);
}

//---------------------------------------------------------------------------
// MsgBox
//---------------------------------------------------------------------------
void MsgBox(LPTSTR lpszMsg) {
  MessageBox(NULL,
             lpszMsg,
             "FreeArc Extension",
             MB_OK);
}

//---------------------------------------------------------------------------
// MsgBoxError
//---------------------------------------------------------------------------
void MsgBoxError(LPTSTR lpszMsg) {
  MessageBox(NULL,
             lpszMsg,
             "FreeArc Extension",
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

static int add_menu_item (lua_State *L) {
  CShellExt *obj;
  lua_getallocf (L, (void**)&obj);
  return obj->add_menu_item();
}

CShellExt::CShellExt() {
  m_cRef = 0L;
  m_pDataObj = NULL;
  _cRef++;
  m_hSciteBmp = LoadBitmap(_hModule, MAKEINTRESOURCE(IDB_SCITE));
  HRESULT hr;
  hr = SHGetMalloc(&m_pAlloc);
  if (FAILED(hr))
    m_pAlloc = NULL;

  // 0. создаём интерпретатор Lua
  L = lua_newstate (l_alloc, this);  luaL_openlibs(L);
  // 1. регистрируем функцию, которая добавляет пункт в меню
  lua_register (L, "add_menu_item", ::add_menu_item);
  // 2. втягиваем Lua scripts
  load_user_funcs();
}

CShellExt::~CShellExt() {
  lua_close(L);

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
  TCHAR szModuleFullName[MAX_PATH];
  TCHAR* pDest;

  GetModuleFileName(_hModule, szModuleFullName, MAX_PATH);
  pDest = strrchr(szModuleFullName, '\\' );
  pDest++;

  pDest[0] = 0;
  strcat_s (szModuleFullName, MAX_PATH, "ArcShellExt-system.lua");
  luaL_dofile (L, szModuleFullName);

  pDest[0] = 0;
  strcat_s (szModuleFullName, MAX_PATH, "ArcShellExt-user.lua");
  luaL_dofile (L, szModuleFullName);
}

int CShellExt::add_menu_item() {
  const char *text = luaL_checkstring(L, 1);
  if (!text)
    return 0;  ////lua_pushstring (L, errormsg); luaL_error / lua_error

  // Submenu flag
  int menu_down = luaL_checknumber(L, 2);

  // Return from submenu (menu_level -= menu_up)
  int menu_up = luaL_checknumber(L, 3);


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
     InsertMenu(hMenu, nIndex, MF_POPUP|MF_BYPOSITION, (UINT)hSubMenu, text);
  }
  else
  {
     InsertMenu(hMenu, nIndex, MF_STRING|MF_BYPOSITION, idCmd, text);
  }

  if (m_hSciteBmp && menu_level==0) {
    SetMenuItemBitmaps (hMenu, nIndex, MF_BYPOSITION, m_hSciteBmp, m_hSciteBmp);
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



  hMenu  = _hMenu;
  nIndex = _nIndex;
  idCmd  = idCmdFirst = _idCmdFirst;
  menu_level = 0;




  FORMATETC fmte = {CF_HDROP, (DVTARGETDEVICE FAR *)NULL, DVASPECT_CONTENT, -1, TYMED_HGLOBAL};

  HRESULT hres = m_pDataObj->GetData(&fmte, &m_stgMedium);

  if (SUCCEEDED(hres)) {
    if (m_stgMedium.hGlobal)
      m_cbFiles = DragQueryFile((HDROP)m_stgMedium.hGlobal, (UINT)-1, 0, 0);
  }




  // 3. вызываем build_menu, передав ему список имён файлов
  lua_getglobal (L, "build_menu");

  if (!lua_checkstack (L, m_cbFiles))
    ;//error

  // push filenames of files selected
  for (UINT i = 0; i < m_cbFiles; i++) {
    DragQueryFile((HDROP)m_stgMedium.hGlobal, i, SelectedFilename, MAX_PATH);
    lua_pushstring (L, SelectedFilename);
  }

  if (lua_pcall(L, 1, 0, 0) != 0)
    ;//error(L, "error running function `f': %s",
     //        lua_tostring(L, -1));

  return ResultFromShort(idCmd-idCmdFirst);
}

// Return help to show in the status line
STDMETHODIMP CShellExt::GetCommandString(UINT_PTR idCmd, UINT uFlags, UINT FAR *reserved, LPSTR pszName, UINT cchMax) {

  if (uFlags == GCS_HELPTEXTA || uFlags == GCS_HELPTEXTW)
  {
    lua_getglobal  (L, "get_help");
    lua_pushnumber (L, idCmdFirst+idCmd);

    if (lua_pcall(L, 1, 1, 0) != 0)
      ;//error(L, "error running function `f': %s",
       //        lua_tostring(L, -1));

    if (!lua_isstring(L, -1))
      ;//error(L, "function `f' must return a string");
    const char *z = lua_tostring(L, -1);

    if (uFlags == GCS_HELPTEXTA)
      strcpy_s(pszName, cchMax, z);

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
      ;//error(L, "error running function `f': %s",
       //        lua_tostring(L, -1));

    if (!lua_isstring(L, -1))
      ;//error(L, "function `f' must return a string");
    const char *z = lua_tostring(L, -1);

    hr = InvokeSciTE(lpcmi->hwnd, lpcmi->lpDirectory, z, lpcmi->lpParameters, lpcmi->nShow);

    lua_pop(L, 1);  /* pop returned value after use */
  }
  return hr;
}

STDMETHODIMP CShellExt::InvokeSciTE(HWND hParent, LPCSTR pszWorkingDir, LPCSTR cmd, LPCSTR pszParam, int iShowCmd) {

  TCHAR* pDest = strrchr(SelectedFilename, '\\');
  if (pDest==SelectedFilename || pDest[-1]==':')  pDest++;
  pDest[0] = 0;
  TCHAR* CurrentDir = SelectedFilename;


  STARTUPINFO si;
  PROCESS_INFORMATION pi;
  ZeroMemory(&si, sizeof(si));
  si.cb = sizeof(si);
  si.dwFlags = STARTF_USESHOWWINDOW;
  si.wShowWindow = SW_RESTORE;
  if (!CreateProcess (NULL, (LPSTR)cmd, NULL, NULL, FALSE, 0, NULL, CurrentDir, &si, &pi)) {
    MessageBox(hParent,
               "Error creating process: ArcShellExt.dll needs to be in the same directory as FreeArc.exe",
               "FreeArc Extension",
               MB_OK);
  }

  return NOERROR;
}

//to do
// unicode
// arbitrary actions
// multiple selection
// paths are included in archive created

// icons
// multiple user.lua files
// persistent Lua_state auto-reloaded on *user.lua changes
// GCS_VERB
// memory management - use SHMalloc

// WideCharToMultiByte
// MultiByteToWideChar

