//---------------------------------------------------------------------------
// Copyright 2002-2008 Andre Burgaud <andre@burgaud.com>
// See license.txt
// $Id: ArcShellExt.h 497 2008-12-07 03:17:37Z andre $
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// ArcShellExt.h
//---------------------------------------------------------------------------

// GUID
// {1539B32D-3A75-4a4d-9B38-5A6000EA7045}
DEFINE_GUID(CLSID_ShellExtension,
            0x1539b32d, 0x3a75, 0x4a4d, 0x9b, 0x38, 0x5a, 0x60, 0x0, 0xea, 0x70, 0x45);

class CShellExtClassFactory : public IClassFactory {
protected:
  ULONG m_cRef;

public:
  CShellExtClassFactory();
  ~CShellExtClassFactory();

  STDMETHODIMP QueryInterface(REFIID, LPVOID FAR *);
  STDMETHODIMP_(ULONG) AddRef();
  STDMETHODIMP_(ULONG) Release();
  STDMETHODIMP CreateInstance(LPUNKNOWN, REFIID, LPVOID FAR *);
  STDMETHODIMP LockServer(BOOL);
};
typedef CShellExtClassFactory *LPCSHELLEXTCLASSFACTORY;

  HBITMAP m_hFreeArcBmp;
class CShellExt : public IContextMenu, IShellExtInit {
public:
protected:
  ULONG m_cRef;
  UINT m_cbFiles;
  STGMEDIUM m_stgMedium;
  LPDATAOBJECT m_pDataObj;
  LPMALLOC m_pAlloc;
  TCHAR m_szDllDir [MAX_PATH];

  lua_State *L;
  HMENU hMenu;
  UINT nIndex;
  UINT idCmdFirst;
  UINT idCmd;
  HMENU menu_stack [100];
  UINT  index_stack[100];
  int   menu_level;
  TCHAR SelectedFilename[MAX_PATH];

  STDMETHODIMP RunProgram (HWND hParent,
                           LPCSTR pszWorkingDir,
                           LPCSTR pszCmd,
                           LPCSTR pszParam,
                           int iShowCmd);

public:
  CShellExt();
  ~CShellExt();

  void load_user_funcs();
  int  add_menu_item();

  STDMETHODIMP QueryInterface(REFIID, LPVOID FAR *);
  STDMETHODIMP_(ULONG) AddRef();
  STDMETHODIMP_(ULONG) Release();

  STDMETHODIMP QueryContextMenu(HMENU hMenu,
    UINT nIndex,
    UINT idCmdFirst,
    UINT idCmdLast,
    UINT uFlags);

  STDMETHODIMP InvokeCommand(LPCMINVOKECOMMANDINFO lpcmi);

  STDMETHODIMP GetCommandString(UINT_PTR idCmd,
    UINT uFlags,
    UINT FAR *reserved,
    LPSTR pszName,
    UINT cchMax);

  STDMETHODIMP Initialize(LPCITEMIDLIST pIDFolder,
    LPDATAOBJECT pDataObj,
    HKEY hKeyID);
};

typedef CShellExt *LPCSHELLEXT;
