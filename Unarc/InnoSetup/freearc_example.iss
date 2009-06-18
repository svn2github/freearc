;[English]
;Example of using unarc.dll for decompression of FreeArc archives with displaying of progress indicator in Inno Setup window
;
;If you have just one archive, you can append it to the end of installer using command "copy /b setup.exe+xxx.arc newsetup.exe"
;In that case you should use {srcexe} as the name of archive to extract.
;
;[Russian]
;Пример распаковки FreeArc архива, с отображением прогресс бара в окне Inno Setup при помощи unarc.dll
;
;Один архив можно просто пристегнуть в конец инсталятора (через copy /b setup.exe+xxx.arc newsetup.exe) и указать в команде распаковки {srcexe}.
;одна из недоработок - не показываются руские имена файлов (нужно сделать перекодировку строки из utf8 в ansi code page или utf-16).

[Setup]
AppName=FreeArc Example
AppVerName=FreeArc Example 1.2
DefaultDirName={pf}\FreeArc Example
UsePreviousAppDir=false
DirExistsWarning=no
ShowLanguageDialog=auto
OutputBaseFilename=FreeArcExample
OutputDir=.
VersionInfoCopyright=Bulat Ziganshin & Victor Dobrov

[Languages]
Name: eng; MessagesFile: compiler:Default.isl
Name: rus; MessagesFile: compiler:Languages\Russian.isl

[CustomMessages]
eng.ArcCancel=Cancel installation
eng.ArcBreak=Installation cancelled!
eng.ArcInfo=Extracted %1 of %2 (%3%%). Archive: %4 of %5.
eng.ArcTitle=Extracting FreeArc archive
eng.ArcError=Decompression failed with error code %1
eng.ArcFail=Decompression failed!
eng.AllProgress=Overall extraction progress: %1%%
eng.ArcBroken=Archive %1 is damaged%nor not enough disk space on desctination drive.

rus.ArcCancel=Отменить распаковку
rus.ArcBreak=Установка прервана!
rus.ArcInfo=Распаковано %1Мб из %2Мб (%3%%). Архив: %4 из %5.
rus.ArcTitle=Распаковка архивов FreeArc
rus.ArcError=Распаковщик FreeArc вернул код ошибки: %1
rus.ArcFail=Распаковка не завершена!
rus.AllProgress=Общий прогресс распаковки: %1%%
rus.ArcBroken=Архив %1 повреждён%nили недостаточно места на диске назначения.

[Files]
Source: *.arc; DestDir: {app}; Flags: nocompression
Source: unarc.dll; DestDir: {tmp}; Flags: dontcopy deleteafterinstall
Source: InnoCallback.dll; DestDir: {tmp}; Flags: dontcopy

[Code]
const
    // [English] Filenames of FreeArc archives; names of external archives shouldn't be added to Files section
    // [Russian] укажите расположение архивов FreeArc; для внешних файлов строку в Files добавлять необязательно
    Archives = '{app}\*.arc';    

type
    PAnsiChar=PChar;     // Remove this line if you use Inno Setup 5.3.0 or higher (Удалите строку, если установлен Inno Setup версии 5.3.0 и выше)

    TMyMsg = record
     hwnd: HWND;
     message: UINT;
     wParam: Longint;
     lParam: Longint;
     time: DWORD;
     pt: TPoint;
    end;

    TFreeArcCallback = function (what: PAnsiChar; int1, int2: Integer; str: PAnsiChar): Integer;
    TArc = record Path: string; Size: Extended; end;

function PeekMessage(var lpMsg: TMyMsg; hWnd: HWND; wMsgFilterMin, wMsgFilterMax, wRemoveMsg: UINT): BOOL; external 'PeekMessageA@user32.dll stdcall';
function TranslateMessage(const lpMsg: TMyMsg): BOOL; external 'TranslateMessage@user32.dll stdcall';
function DispatchMessage(const lpMsg: TMyMsg): Longint; external 'DispatchMessageA@user32.dll stdcall';

const
    PM_REMOVE = 1;

procedure AppProcessMessage;
var
    Msg: TMyMsg;
begin
    while PeekMessage(Msg, WizardForm.Handle, 0, 0, PM_REMOVE) do begin
        TranslateMessage(Msg);
        DispatchMessage(Msg);
    end;
end;

function WrapFreeArcCallback (callback: TFreeArcCallback; paramcount: integer):longword; external 'wrapcallback@files:innocallback.dll stdcall';
function FreeArcExtract (callback: longword; cmd1,cmd2,cmd3,cmd4,cmd5,cmd6,cmd7,cmd8,cmd9,cmd10: PAnsiChar): integer; external 'FreeArcExtract@files:unarc.dll cdecl';

var
    ProgressBar: TNewProgressBar;
    ExtractFile: TNewStaticText;
    Button1:     TButton;
    Cancel:      Integer;
    n: Integer; Arcs: array of TArc; m: Extended;

function cm(Message: String): String; Begin Result:= ExpandConstant('{cm:'+ Message +'}') End;

{Перевод числа в строку с точностью 3 знака (%.3n) с округлением дробной части, если она есть}
Function NumToStr(Float: Extended): String; 
Begin
    Result:= Format('%.3n', [Float]); StringChange(Result, ',', '.');
    while (Pos('.', Result) > 0) and ((Result[Length(Result)] = '0') or (Result[Length(Result)] = '.'))
      do SetLength(Result, Length(Result)-1);
End;

Function Size64(Hi, Lo: Integer): Extended; Begin
    Result:= Lo; if Lo< 0 then Result:= Result + $7FFFFFFF + $7FFFFFFF + 2; for Hi:= Hi-1 Downto 0 do Result:= Result + $7FFFFFFF + $7FFFFFFF + 2;
End;

procedure Button1OnClick(Sender: TObject);
begin
    Cancel := -127;  //error code for Cancel button
end;

function FindArcs(dir: string): Extended;
var
    FSR: TFindRec;
Begin
    if FindFirst(ExpandConstant(dir), FSR) then
      try
        repeat
            if FSR.Attributes and FILE_ATTRIBUTE_DIRECTORY > 0 then CONTINUE
            n:= GetArrayLength(Arcs)
            SetArrayLength(Arcs, n +1)
            Arcs[n].Path:= ExtractFilePath(ExpandConstant(Archives)) + FSR.Name
            Arcs[n].Size:= Size64(FSR.SizeHigh, FSR.SizeLow)
            Result:= Result + Arcs[n].Size;
        until not FindNext(FSR);
      finally
        FindClose(FSR);
      end;
End;

function FreeArcCallback (what: PAnsiChar; Mb, sizeArc: Integer; str: PAnsiChar): Integer;
    var percents: Integer;
begin
    if string(what)='filename' then
//      ExtractFile.Caption:= str
//    желательно сделать перекодировку строки из utf8 в ansi например, при помощи WinAPI MultiByteToWideChar
    else if (string(what)='progress') and (sizeArc>0) then begin
            percents:= (Mb*1000) div sizeArc;
            ProgressBar.Position:= percents;
            ExtractFile.Caption:= FmtMessage(cm('ArcInfo'), [IntToStr(Mb), IntToStr(sizeArc), Format('%.1n', [Abs(percents/10)]), IntToStr(n+1), IntToStr(GetArrayLength(Arcs)) ]);
            WizardForm.ProgressGauge.Position:= WizardForm.ProgressGauge.Tag + round(ProgressBar.Position * m)
            percents:= (WizardForm.ProgressGauge.Position-WizardForm.ProgressGauge.Min)/((WizardForm.ProgressGauge.Max - WizardForm.ProgressGauge.Min)/1000)
            WizardForm.FileNameLabel.Caption:= FmtMessage(cm('AllProgress'), [Format('%.1n', [Abs(percents/10)])]);
    end;
    AppProcessMessage;
    Result := Cancel;
end;

function ExtractFreeArcArchive(arcname, destpath: String): Integer;
  var callback: longword;
begin
    Cancel:= 0;
    AppProcessMessage;
    callback:= WrapFreeArcCallback(@FreeArcCallback,4);   //FreeArcCallback has 4 arguments
    try
        Result:= FreeArcExtract (callback, 'x', '-o+', '-dp'+destpath, '--', arcname, '', '', '', '', '');
        if Result = 0 then Result:= Cancel;
    except
        Result:= -63;  //error code for ArcFail
    end;
end;

function UnPack(Archives: string): Integer;
  var allSize: Extended; FreeMB, TotalMB: Cardinal; mes: string;
begin
  Button1.Show
  WizardForm.ProgressGauge.Position:= 0;
  WizardForm.ProgressGauge.Max:= 1000
  allSize:= FindArcs(Archives)
  for n:= 0 to GetArrayLength(Arcs) -1 do begin
    m:= Arcs[n].Size/allSize    //current archive size
    WizardForm.ProgressGauge.Tag:= WizardForm.ProgressGauge.Position
    Result:= ExtractFreeArcArchive(Arcs[n].Path, ExpandConstant('{app}'));
    if Result <> 0 then begin
        mes:= FmtMessage(cm('ArcError'), [IntToStr(Result)]);
        GetSpaceOnDisk(ExtractFileDrive(ExpandConstant('{app}')), True, FreeMB, TotalMB);
        case Result of
          -1: if FreeMB < 32 {megabytes of free disk space} then mes:= SetupMessage(msgDiskSpaceWarningTitle)
              else mes:= mes + #13#10 + FmtMessage(cm('ArcBroken'), [ExtractFileName(Arcs[n].Path)]);
          -127: mes:= cm('ArcBreak');    //Cancel button
          -63:  mes:= cm('ArcFail');
        end;
        MsgBox(mes, mbInformation, MB_OK);
        Log(mes);
        Break;    //leave extraction loop
    end;
  end;
  Button1.visible:= false;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
    if CurStep = ssPostInstall then UnPack(Archives);
end;

procedure InitializeWizard();
begin
    ProgressBar := TNewProgressBar.Create(WizardForm);
    ExtractFile := TNewStaticText.Create(WizardForm);
    with WizardForm.ProgressGauge do
      begin
        ProgressBar.SetBounds(Left, Top + ScaleX(55), Width, Height)
        ProgressBar.Parent := WizardForm.InstallingPage;
        ProgressBar.max := 1000;
        ProgressBar.Position := 0;
        ExtractFile.parent:=WizardForm.InstallingPage;
        ExtractFile.autosize:=false;
        ExtractFile.Width := Width;
        ExtractFile.top:=Top + ScaleX(35);
        ExtractFile.caption:=cm('ArcTitle');
      end;
    Button1:=TButton.create(WizardForm);
    Button1.parent:=WizardForm;
    Button1.SetBounds(260, WizardForm.cancelbutton.top, 135, WizardForm.cancelbutton.Height);
    Button1.caption:=cm('ArcCancel');
    Button1.OnClick:=@Button1OnClick;
    Button1.Hide;
end;
