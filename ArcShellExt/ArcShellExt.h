//---------------------------------------------------------------------------
// Copyright 2002-2008 Andre Burgaud <andre@burgaud.com>
// See license.txt
// $Id: wscitecm.h 497 2008-12-07 03:17:37Z andre $
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// wscitecm.h
//---------------------------------------------------------------------------

// Old GUID
// {120b94b5-2e6a-4f13-94d0-414bcb64fa0f}
//DEFINE_GUID(CLSID_ShellExtension, 0x120b94b5, 0x2e6a, 0x4f13, 0x94, 0xd0, 0x41, 0x4b, 0xcb, 0x64, 0xfa, 0x0f);

// New GUID since version 1.3.1
// Created to avoid conflict with Notpad++ reusing this code with the GUID above
// {C4ACBD3E-6114-4618-904C-B206ABA9DEB0}
DEFINE_GUID(CLSID_ShellExtension,
            0xc4acbd3e, 0x6114, 0x4618, 0x90, 0x4c, 0xb2, 0x6, 0xab, 0xa9, 0xde, 0xb0);

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

class CShellExt : public IContextMenu, IShellExtInit {
public:
protected:
  ULONG m_cRef;
  UINT m_cbFiles;
  STGMEDIUM m_stgMedium;
  LPDATAOBJECT m_pDataObj;
  HBITMAP m_hSciteBmp;
  LPMALLOC m_pAlloc;
  TCHAR m_szDllDir [MAX_PATH];

  STDMETHODIMP InvokeSciTE(HWND hParent,
    LPCSTR pszWorkingDir,
    LPCSTR pszCmd,
    LPCSTR pszParam,
    int iShowCmd);

public:
  CShellExt();
  ~CShellExt();

  STDMETHODIMP QueryInterface(REFIID, LPVOID FAR *);
  STDMETHODIMP_(ULONG) AddRef();
  STDMETHODIMP_(ULONG) Release();

  STDMETHODIMP QueryContextMenu(HMENU hMenu,
    UINT indexMenu,
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
