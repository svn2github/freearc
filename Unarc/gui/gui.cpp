#undef  _WIN32_WINNT
#define _WIN32_WINNT    0x501
#define _WIN32_IE	0x0300
#include <shlobj.h>
#include <io.h>
#include <stdio.h>
#include <stdlib.h>
#include <tchar.h>
#include <wchar.h>
#include <fcntl.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <richedit.h>
#include "gui.h"
#include "resource.h"

#pragma warning(disable: 4996)

static const UINT WM_SETOBJECT = WM_USER + 1;
static const UINT WM_CONFIRM_REPLACE = WM_USER + 2;
static const WORD MAX_PROGRESS_VALUE = 65535U;
static const size_t MAX_MESSAGE_LENGTH = MY_FILENAME_MAX+256;
bool UI::isInitialized = false;
static TCHAR archiveFileName[MY_FILENAME_MAX];
static TCHAR cancelConfirmCaption[MAX_MESSAGE_LENGTH];
static TCHAR cancelConfirmText[MAX_MESSAGE_LENGTH];
static TCHAR selectFolderToExtract[MAX_MESSAGE_LENGTH];
static TCHAR msgBytes[MAX_MESSAGE_LENGTH];
static TCHAR msgModified[MAX_MESSAGE_LENGTH];
static TCHAR formatTime[MAX_MESSAGE_LENGTH];
static TCHAR formatSpeed[MAX_MESSAGE_LENGTH];
static TCHAR formatProgressCaption[MAX_MESSAGE_LENGTH];
static TCHAR formatMainCaption[MAX_MESSAGE_LENGTH];
static HCURSOR hCursorHand;
static WNDPROC defaultStaticProc = 0;
static UI *globalGUI = 0;

static BOOL CALLBACK DialogProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
static BOOL CALLBACK ProgressProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
static BOOL CALLBACK ConfirmReplaceProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
static void ConvertSecondsToHMS(int seconds, int *h, int *m, int *s);
static void InitConfirmReplaceDialog(HWND hWnd, TCHAR *filename, uint64 size, time_t modified, HICON *hIcon);
static void CenterWindow(HWND hWnd);
static void BrowseForFolder(HWND hWndOwner, TCHAR *destinationDirectory);
static int CALLBACK BrowseCallbackProc(HWND hwnd, UINT uMsg, LPARAM lParam, LPARAM lpData);
static DWORD ProgressThread(LPVOID);

#ifndef FREEARC_WIN
// Converts UTF-8 string to UTF-16
void utf8_to_utf16 (char *utf8, WCHAR *utf16)
{
  do {
    BYTE c = utf8[0];   UINT c32;
         if (c<=0x7F)   c32 = c;
    else if (c<=0xBF)   c32 = '?';
    else if (c<=0xDF)   c32 = ((c&0x1F) << 6) +  (utf8[1]&0x3F),  utf8++;
    else if (c<=0xEF)   c32 = ((c&0x0F) <<12) + ((utf8[1]&0x3F) << 6) +  (utf8[2]&0x3F),  utf8+=2;
    else                c32 = ((c&0x0F) <<18) + ((utf8[1]&0x3F) <<12) + ((utf8[2]&0x3F) << 6) + (utf8[3]&0x3F),  utf8+=3;

    // Now c32 represents full 32-bit Unicode char
    if (c32 <= 0xFFFF)  *utf16++ = c32;
    else                c32-=0x10000, *utf16++ = c32/0x400 + 0xd800, *utf16++ = c32%0x400 + 0xdc00;

  } while (*utf8++);
}

// Converts UTF-16 string to UTF-8
void utf16_to_utf8 (WCHAR *utf16, char *utf8)
{
  do {
    UINT c = utf16[0];
    if (0xd800<=c && c<=0xdbff && 0xdc00<=utf16[1] && utf16[1]<=0xdfff)
      c = (c - 0xd800)*0x400 + (UINT)(*++utf16 - 0xdc00) + 0x10000;

    // Now c represents full 32-bit Unicode char
         if (c<=0x7F)   *utf8++ = c;
    else if (c<=0x07FF) *utf8++ = 0xC0|(c>> 6)&0x1F,  *utf8++ = 0x80|(c>> 0)&0x3F;
    else if (c<=0xFFFF) *utf8++ = 0xE0|(c>>12)&0x0F,  *utf8++ = 0x80|(c>> 6)&0x3F,  *utf8++ = 0x80|(c>> 0)&0x3F;
    else                *utf8++ = 0xF0|(c>>18)&0x0F,  *utf8++ = 0x80|(c>>12)&0x3F,  *utf8++ = 0x80|(c>> 6)&0x3F,  *utf8++ = 0x80|(c>> 0)&0x3F;

  } while (*utf16++);
}
#endif

UI::UI()
{
	if(!isInitialized)
	{
		LoadLibrary(_T("Riched20.dll"));
		INITCOMMONCONTROLSEX icce;
		icce.dwSize = sizeof(icce);
		icce.dwICC = ICC_ANIMATE_CLASS | ICC_BAR_CLASSES | ICC_LINK_CLASS | ICC_PROGRESS_CLASS | ICC_STANDARD_CLASSES;
		InitCommonControlsEx(&icce);

		// Load string resources
		HINSTANCE currentModule = GetModuleHandle(NULL);
		LoadString(currentModule, IDS_STRING_CANCEL_CONFIRM_CAPTION, cancelConfirmCaption, MAX_MESSAGE_LENGTH);
		LoadString(currentModule, IDS_STRING_CANCEL_CONFIRM_TEXT, cancelConfirmText, MAX_MESSAGE_LENGTH);
		LoadString(currentModule, IDS_STRING_SELECT_FOLDER, selectFolderToExtract, MAX_MESSAGE_LENGTH);
		LoadString(currentModule, IDS_STRING_BYTES, msgBytes, MAX_MESSAGE_LENGTH);
		LoadString(currentModule, IDS_STRING_MODIFIED, msgModified, MAX_MESSAGE_LENGTH);
		LoadString(currentModule, IDS_STRING_TIME, formatTime, MAX_MESSAGE_LENGTH);
		LoadString(currentModule, IDS_STRING_SPEED, formatSpeed, MAX_MESSAGE_LENGTH);
		LoadString(currentModule, IDS_STRING_PROGRESS_CAPTION, formatProgressCaption, MAX_MESSAGE_LENGTH);
		LoadString(currentModule, IDS_STRING_MAIN_CAPTION, formatMainCaption, MAX_MESSAGE_LENGTH);
		isInitialized = true;

		// Load hand cursor
		hCursorHand = LoadCursor(NULL, MAKEINTRESOURCE(IDC_HAND));
	}

	button = 0;
	*destinationDirectory = 0;
	hWndProgress = 0;
	progressResult = true;
	hIcon = LoadIcon(GetModuleHandle(NULL), MAKEINTRESOURCE(IDI_ICON_FREEARC));
	thread = 0;
	totalBytes = 0;
	readBytes = 0;
	writtenBytes = 0;
	lastWrittenBytes = 0;
	hEvent = 0;
	*filename = 0;
	msg = 0;
}

UI::~UI()
{
	if(hIcon != 0)
	{
		DestroyIcon(hIcon);
		hIcon = 0;
	}

	if(hEvent != 0)
	{
		CloseHandle(hEvent);
		hEvent = 0;
	}
}

void UI::DisplayHeader(char* header)
{
}

static void SplitFilename(TCHAR *filename, TCHAR *dirname, TCHAR *basename)
{
	TCHAR *p  = _tcsrchr(filename, _T('\\'));
	TCHAR *p2 = _tcsrchr(filename, _T('/'));
	TCHAR *p3 = _tcsrchr(filename, _T(':'));
	if (p2 && (!p || p2>p))  p=p2;
	if (p3 && (!p || p3>p))  p=p3;

	TCHAR *p0  =  (p && p>filename && p[-1]==_T(':'))? p+1 : p;  // for c:\name directory is c:\ not c:

	if(p == 0)
		dirname  && (_tcscpy (dirname,  _T(""))),
		basename && (_tcscpy (basename, filename));
	else
		dirname  && (_tcscpy (dirname,  filename),  dirname[p0-filename] = 0),
		basename && (_tcscpy (basename, p+1));

}

static void SetComment(HWND hWnd, char *comment, int commentSize)
{
	char *src = 0;
	size_t srcSize = 0;

	if(comment != 0 && commentSize > 0)
	{
		src = comment;
		srcSize = (size_t)commentSize;
	}
	else
	{
		HRSRC hResInfo = FindResource(GetModuleHandle(NULL), MAKEINTRESOURCE(IDR_RTF_COMMENTS), _T("RT_RTF"));

		if(hResInfo != NULL)
		{
			HGLOBAL hComment = LoadResource(GetModuleHandle(NULL), hResInfo);

			if(hComment != NULL)
			{
				src = (char *)LockResource(hComment);
				srcSize = (size_t)SizeofResource(GetModuleHandle(NULL), hResInfo);
			}
		}
	}

	SETTEXTEX ste;
	ste.flags = ST_DEFAULT;
	void *data = 0;

	if(src != 0 && srcSize > 0)
	{
		if(srcSize >= 2 && ((src[0] == '\xff' && src[1] == '\xfe') | (src[0] == '\xfe' && src[1] == '\xff')))
		{
			// UTF-16 detected
			size_t utf16Size = srcSize/2;
			wchar_t *buffer = (wchar_t *) malloc ((utf16Size+1) * sizeof(wchar_t));
			memcpy(buffer, src, srcSize);
			buffer[utf16Size] = L'\0';
			data = buffer;
			ste.codepage = 1200;
		}
		else if(srcSize >= 5 && src[0] == '{' && src[1] == '\\' && src[2] == 'r' && src[3] == 't' && src[4] == 'f')
		{
			// RTF detected
			char *buffer = (char *) malloc (srcSize + 1);
			memcpy(buffer, src, srcSize);
			buffer[srcSize] = '\0';
			data = buffer;
			ste.codepage = CP_ACP;
		}
		else
		{
			// UTF-8 by default: encode to UTF-16
			char *tmpBuffer = (char *) malloc (srcSize + 1);
			memcpy(tmpBuffer, src, srcSize);
			tmpBuffer[srcSize] = '\0';
			wchar_t *buffer = (wchar_t *) malloc ((srcSize+1) * sizeof(wchar_t));
			utf8_to_utf16(tmpBuffer, buffer);
			free(tmpBuffer);
			data = buffer;
			ste.codepage = 1200;
		}
	}

	if(data != 0)
	{
		SendDlgItemMessage(hWnd, IDC_RICHEDIT, EM_SETTEXTEX, (WPARAM)&ste, (LPARAM)data);
		free(data);
	}
}

bool UI::AllowProcessing(char cmd, int _silent, TCHAR *arcname, char* utf8comment, int cmtsize, char* utf8outdir)
{
	SplitFilename(arcname, NULL, archiveFileName);
	msg = (TCHAR *) malloc((_tcslen(archiveFileName)+1000) * sizeof(*msg));

	if(*utf8outdir)
		utf8_to_utf16(utf8outdir, destinationDirectory);
	else
	{
		_tcscpy(destinationDirectory, arcname);
		TCHAR *ext = _tcsrchr(destinationDirectory, _T('.'));
		if(ext)
			*ext = _T('\0');
	}

        // Don't ask user in silent mode
        silent = _silent;  if (silent)  return TRUE;

	HWND hWnd = CreateDialog(GetModuleHandle(NULL), MAKEINTRESOURCE(IDD_DIALOG_MAIN), 0, DialogProc);
	SetClassLong(hWnd, GCL_HICON, (LONG)hIcon);
	SetWindowLong(hWnd, GWL_USERDATA, (LONG)this);
	PostMessage(hWnd, WM_SETOBJECT, 0, 0);
   	SetDlgItemText(hWnd, IDC_EDIT_PATH, destinationDirectory);
	SetComment(hWnd, utf8comment, cmtsize);

	_stprintf(msg, formatMainCaption, archiveFileName);
	SetWindowText(hWnd, msg);

	MSG message;
	while(GetMessage(&message, NULL, 0, 0) != 0)
	{
		if(IsDialogMessage(hWnd, &message) == 0)
		{
			TranslateMessage(&message);
			DispatchMessage(&message);
		}
	}

	return (button == IDOK);
}

char* UI::GetOutDir()
{
	utf16_to_utf8(destinationDirectory, destinationDirectory_utf8);
	return destinationDirectory_utf8;
}

void UI::BeginProgress(uint64 totalBytes)
{
	this->totalBytes = totalBytes;
	hEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
	if (silent==1)  return;
	thread = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)ProgressThread, this, 0, NULL);
	WaitForSingleObject(hEvent, INFINITE);
	tickCountOfBegin = GetTickCount();
}

bool UI::ProgressFile(bool isdir, const char *operation, TCHAR *_filename, uint64 filesize)
{
	_tcscpy(filename, _filename);
	return progressResult;
}

bool UI::ProgressWrite(uint64 _writtenBytes)
{
	writtenBytes = _writtenBytes;
	if (writtenBytes - lastWrittenBytes >= 65536)
		ShowProgress();
	return progressResult;
}

bool UI::ProgressRead(uint64 _readBytes)
{
	readBytes = _readBytes;
	ShowProgress();
	return progressResult;
}

bool UI::ShowProgress()
{
	if (silent==1)  return progressResult;
	WaitForSingleObject(hEvent, INFINITE);
	lastWrittenBytes = writtenBytes;

	// Show name of file currently extracted
	SetDlgItemText(hWndProgress, IDC_STATIC_FILENAME, filename);

	// Show progress window caption
	int percents = (totalBytes == 0)? 0 : (int)(double(readBytes) * 100/totalBytes);
	_stprintf(msg, formatProgressCaption, percents, archiveFileName);
	SetWindowText(hWndProgress, msg);

	// Show elapsed and estimated times
	double elapsedSeconds = ((GetTickCount() - tickCountOfBegin)/1000);

	if(readBytes > 0  &&  elapsedSeconds >= 1)
	{
		int elaH, elaM, elaS;
		ConvertSecondsToHMS((int)elapsedSeconds, &elaH, &elaM, &elaS);

		double remainingSeconds = 1+(((totalBytes - readBytes) * elapsedSeconds)/readBytes);
		int remH, remM, remS;
		ConvertSecondsToHMS((int)remainingSeconds, &remH, &remM, &remS);
		_stprintf(msg, formatTime, elaH, elaM, elaS, remH, remM, remS);
		SetDlgItemText(hWndProgress, IDC_STATIC_TIME, msg);
	}

	// Show extraction speed
	if(writtenBytes > 0  &&  elapsedSeconds >= 1)
	{
		_stprintf(msg, formatSpeed, uint64(writtenBytes/elapsedSeconds/1000));
		SetDlgItemText(hWndProgress, IDC_STATIC_SPEED, msg);
	}

	// Show progress indicator
	int normalizedReadBytes = (totalBytes == 0)? 0 : int((double(readBytes) * MAX_PROGRESS_VALUE)/totalBytes);
	SendDlgItemMessage(hWndProgress, IDC_PROGRESS, PBM_SETPOS, normalizedReadBytes, 0);

	return progressResult;
}

static void ConvertSecondsToHMS(int seconds, int *h, int *m, int *s)
{
	*s = seconds % 60;
	*m = (seconds/60) % 60;
	*h = seconds/3600;
}

void UI::EndProgress()
{
	if(thread != 0)
	{
		CloseHandle(thread);
		thread = 0;
	}
}

char UI::AskOverwrite(TCHAR *filename, uint64 size, time_t modified)
{
	_tcscpy (replacedFileName, filename);
	replacedFileSize = size;
	replacedFileModified = modified;
	ResetEvent(hEvent);

	//Silent mode: duplicated DialogBox call code
	globalGUI = this;
	if (silent==1)
	{
		replaceDialogResult = (char)DialogBox(GetModuleHandle(NULL), MAKEINTRESOURCE(IDD_DIALOG_CONFIRM), NULL, ConfirmReplaceProc);
		SetEvent(hEvent);
	} else
	{
		PostMessage(hWndProgress, WM_CONFIRM_REPLACE, 0, 0);
	}

	WaitForSingleObject(hEvent, INFINITE);

	return replaceDialogResult;
}

static void InitConfirmReplaceDialog(HWND hWnd, TCHAR *filename, uint64 size, time_t modified, HICON *hIcon)
{
	static TCHAR msg[MAX_MESSAGE_LENGTH];
	static TCHAR path[MY_FILENAME_MAX], basename[MY_FILENAME_MAX];

	// Display file name and path
	SplitFilename(filename, path, basename);
	SetDlgItemText(hWnd, IDC_STATIC_FOLDER, path);
	SetDlgItemText(hWnd, IDC_STATIC_FILENAME, basename);

	// Display file's icon
	SHFILEINFO shFileInfo;

	SHGetFileInfo(filename, 0, &shFileInfo, sizeof(shFileInfo), SHGFI_ICON);
	*hIcon = shFileInfo.hIcon;
	SendDlgItemMessage(hWnd, IDC_STATIC_ICON1, STM_SETICON, (WPARAM)*hIcon, 0);
	SendDlgItemMessage(hWnd, IDC_STATIC_ICON2, STM_SETICON, (WPARAM)*hIcon, 0);

	// Display source size
	_stprintf(msg, _T("%I64u %s"), size, msgBytes);
	SetDlgItemText(hWnd, IDC_STATIC_SIZE2, msg);

	// Display source modified date/time
	TCHAR *dateTime = _tctime(&modified);
	_stprintf(msg, _T("%s %s"), msgModified, dateTime);
	SetDlgItemText(hWnd, IDC_STATIC_DATETIME2, msg);

	// Display destination size
	struct _stati64 s;
	_tstati64(filename, &s);
	_stprintf(msg, _T("%I64u %s"), s.st_size, msgBytes);
	SetDlgItemText(hWnd, IDC_STATIC_SIZE1, msg);

	// Display destination modified date/time
	dateTime = _tctime(&(s.st_mtime));
	_stprintf(msg, _T("%s %s"), msgModified, dateTime);
	SetDlgItemText(hWnd, IDC_STATIC_DATETIME1, msg);
}

static BOOL CALLBACK UrlProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch(uMsg)
	{
	case WM_SETCURSOR:
		SetCursor(hCursorHand);
		return TRUE;
	}

	return defaultStaticProc(hWnd, uMsg, wParam, lParam);
}

static void ConvertStaticTextToHyperLink(HWND hWnd)
{
	HFONT hFont = (HFONT)SendMessage(hWnd, WM_GETFONT, 0, 0);
	LOGFONT lf;
	GetObject(hFont, sizeof(lf), &lf);
	lf.lfUnderline = TRUE;
	hFont = CreateFontIndirect(&lf);
	HWND hStaticWindow = GetDlgItem(hWnd, IDC_STATIC_URL);
	SendMessage(hStaticWindow, WM_SETFONT, (WPARAM)hFont, MAKELPARAM(TRUE, 0));


	if(defaultStaticProc == 0)
		defaultStaticProc = (WNDPROC)GetWindowLong(hStaticWindow, GWL_WNDPROC);

	SetWindowLong(hStaticWindow, GWL_WNDPROC, (LONG)UrlProc);
}

static void UnitializeHyperLink(HWND hWnd)
{
	HFONT hFont = (HFONT)SendDlgItemMessage(hWnd, IDC_STATIC_URL, WM_GETFONT, 0, 0);
	DeleteObject(hFont);
}

static BOOL CALLBACK DialogProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	UI *gui = (UI*)GetWindowLong(hWnd, GWL_USERDATA);

	switch(uMsg)
	{
	case WM_INITDIALOG:
		CenterWindow(hWnd);
		SendDlgItemMessage(hWnd, IDC_EDIT_PATH, WM_SETFOCUS, 0, 0);
		ConvertStaticTextToHyperLink(hWnd);
		return TRUE;
	case WM_CTLCOLORSTATIC:
		if(GetDlgItem(hWnd, IDC_STATIC_URL) == (HWND)lParam)
		{
			HDC hDC = (HDC)wParam;
			SetTextColor(hDC, RGB(0, 0, 255));
			SetBkMode(hDC, TRANSPARENT);
			return (BOOL)GetSysColorBrush(COLOR_BTNFACE);
		}
		return FALSE;
	case WM_COMMAND:
		switch(LOWORD(wParam))
		{
		case IDOK:
			GetDlgItemText(hWnd, IDC_EDIT_PATH, gui->destinationDirectory, MY_FILENAME_MAX);
		case IDCANCEL:
			gui->button = LOWORD(wParam);
			DestroyWindow(hWnd);
			return TRUE;
		case IDBROWSE:
			GetDlgItemText(hWnd, IDC_EDIT_PATH, gui->destinationDirectory, MY_FILENAME_MAX);
			BrowseForFolder(hWnd, gui->destinationDirectory);
			return TRUE;
		case IDC_STATIC_URL:
			ShellExecute(hWnd, _T("open"), _T("http://freearc.org"), NULL, NULL, SW_SHOWNORMAL);
			return TRUE;
		}
		return FALSE;
	case WM_CLOSE:
		DestroyWindow(hWnd);
		return TRUE;
	case WM_DESTROY:
		UnitializeHyperLink(hWnd);
		PostQuitMessage(0);
		return TRUE;
	}

	return FALSE;
}

static BOOL CALLBACK ProgressProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	UI *gui = (UI*)GetWindowLong(hWnd, GWL_USERDATA);

	switch(uMsg)
	{
	case WM_SETOBJECT:
		SetWindowLong(hWnd, GWL_USERDATA, lParam);
		return TRUE;
	case WM_CONFIRM_REPLACE:
		gui->replaceDialogResult = (char)DialogBox(GetModuleHandle(NULL), MAKEINTRESOURCE(IDD_DIALOG_CONFIRM), hWnd, ConfirmReplaceProc);
		SetEvent(gui->hEvent);
		return TRUE;
	case WM_INITDIALOG:
		CenterWindow(hWnd);
		Animate_OpenEx(GetDlgItem(hWnd, IDC_ANIMATE_COPY), GetModuleHandle(_T("shell32.dll")), MAKEINTRESOURCE(161));
		return TRUE;
	case WM_COMMAND:
		if(LOWORD(wParam) == IDCANCEL)
		{
			ResetEvent(gui->hEvent);
			if(MessageBox(hWnd, cancelConfirmText, cancelConfirmCaption, MB_YESNO | MB_ICONQUESTION | MB_DEFBUTTON2) == IDYES)
				DestroyWindow(hWnd);
			SetEvent(gui->hEvent);
			return TRUE;
		}
		return FALSE;
	case WM_CLOSE:
		DestroyWindow(hWnd);
		return TRUE;
	case WM_DESTROY:
		gui->progressResult = false;
		PostQuitMessage(0);
		return TRUE;
	}

	return FALSE;
}

static BOOL CALLBACK ConfirmReplaceProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	//Silent mode: Replaced UI *gui = (UI*)GetWindowLong(hWnd, GWL_USERDATA);
	UI *gui = globalGUI;

	switch(uMsg)
	{
	case WM_INITDIALOG:
		CenterWindow(hWnd);
		//Silent mode: Removed SetWindowLong(hWnd, GWL_USERDATA, GetWindowLong(GetParent(hWnd), GWL_USERDATA));
		//Silent mode: Removed gui = (UI*)GetWindowLong(hWnd, GWL_USERDATA);
		InitConfirmReplaceDialog(hWnd, gui->replacedFileName, gui->replacedFileSize, gui->replacedFileModified, &(gui->hFileIcon));
		return TRUE;
	case WM_COMMAND:
		switch(LOWORD(wParam))
		{
		case IDC_BUTTON_YES:
			EndDialog(hWnd, (INT_PTR)'y');
			return TRUE;
		case IDC_BUTTON_YESALL:
			EndDialog(hWnd, (INT_PTR)'a');
			return TRUE;
		case IDC_BUTTON_NO:
			EndDialog(hWnd, (INT_PTR)'n');
			return TRUE;
		case IDC_BUTTON_NOALL:
			EndDialog(hWnd, (INT_PTR)'s');
			return TRUE;
		case IDCANCEL:
			EndDialog(hWnd, (INT_PTR)'q');
			return TRUE;
		}
		return FALSE;
	case WM_CLOSE:
		EndDialog(hWnd, (INT_PTR)'q');
		return TRUE;
	case WM_DESTROY:
		DestroyIcon(gui->hFileIcon);
		return true;
	}

	return FALSE;
}

static void CenterWindow(HWND hWnd)
{
	RECT desktop;
	GetWindowRect(GetDesktopWindow(), &desktop);
	RECT window;
	GetWindowRect(hWnd, &window);

	int desktopWidth = desktop.right - desktop.left;
	int desktopHeight = desktop.bottom - desktop.top;
	int windowWidth = window.right - window.left;
	int windowHeight = window.bottom - window.top;

	int newLeft = (desktopWidth - windowWidth)/2;
	int newTop = (desktopHeight - windowHeight)/2;
	SetWindowPos(hWnd, 0, newLeft, newTop, 0, 0, SWP_NOSIZE);
}

static void BrowseForFolder(HWND hWndOwner, TCHAR *destinationDirectory)
{
	BROWSEINFO bi;
	static TCHAR displayName[MY_FILENAME_MAX];

	bi.hwndOwner = hWndOwner;
	bi.lParam = (LONG)destinationDirectory;
	bi.lpszTitle = selectFolderToExtract;
	bi.lpfn = BrowseCallbackProc;
	bi.pidlRoot = NULL;
	bi.pszDisplayName = displayName;
	bi.ulFlags = BIF_RETURNONLYFSDIRS;

	LPITEMIDLIST pItemIdList = SHBrowseForFolder(&bi);

	if(pItemIdList != NULL)
	{
		if(SHGetPathFromIDList(pItemIdList, destinationDirectory))
			SetDlgItemText(hWndOwner, IDC_EDIT_PATH, destinationDirectory);

		IMalloc *iMalloc = 0;
		if(SUCCEEDED(SHGetMalloc(&iMalloc)))
		{
			iMalloc->Free(pItemIdList);
			iMalloc->Release();
		}
	}
}

static int CALLBACK BrowseCallbackProc(HWND hwnd, UINT uMsg, LPARAM lParam, LPARAM lpData)
{
	if(uMsg == BFFM_INITIALIZED)
		PostMessage(hwnd, BFFM_SETSELECTION, TRUE, lpData);

	return 0;
}

static DWORD ProgressThread(LPVOID parameter)
{
	UI *gui = (UI*)parameter;

	gui->hWndProgress = CreateDialog(GetModuleHandle(NULL), MAKEINTRESOURCE(IDD_DIALOG_PROGRESS), 0, ProgressProc);
	SetClassLong(gui->hWndProgress, GCL_HICON, (LONG)gui->hIcon);
	PostMessage(gui->hWndProgress, WM_SETOBJECT, 0, (LONG)gui);
	SendDlgItemMessage(gui->hWndProgress, IDC_PROGRESS, PBM_SETRANGE, 0, MAKELPARAM(0, MAX_PROGRESS_VALUE));
	SetEvent(gui->hEvent);

	MSG msg;
	while(GetMessage(&msg, NULL, 0, 0) != 0)
	{
		if(IsDialogMessage(gui->hWndProgress, &msg) == 0)
		{
			TranslateMessage(&msg);
			DispatchMessage(&msg);
		}
	}

	return 0;
}
