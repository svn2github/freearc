#include <stdio.h>
#include "gui.h"

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
	UI *gui = new UI();

	char comment[20000];
	FILE *fcmt=fopen("comments.rtf","rb");
	int cmtsize = (fcmt==NULL)? 0 : fread(comment, 1, sizeof(comment), fcmt);
        bool result = gui->AllowProcessing ('x', _T("c:\\temp\\archive.exe"), comment, cmtsize, _T("C:\\TEMP"));

	if(result)
	{
		TCHAR *filenames[] = {_T("aaa.txt"), _T("bbb.txt"), _T("ccc.txt"), _T("ddd.txt"), _T("eee.txt"), _T("fff.txt"), _T("ggg.txt")};
		gui->BeginProgress(7000000);

		for(int i = 0; i < 7000; i += 1)
		{
			if(i%1000==0 && !gui->ProgressFile(FALSE, "Extracting", filenames[i/1000], i))
        			break;

        		Sleep(1);

			if(!gui->ProgressRead(i*1000))
				break;

			if(!gui->ProgressWrite(i*1000))
				break;

			if(i == 1000)
				gui->AskOverwrite(_T("C:\\AUTOEXEC.BAT"), 3240, 176000L);
		}

		gui->EndProgress();
	}

	delete gui;

	return 0;
}