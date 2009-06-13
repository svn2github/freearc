;[English]
;Example of using unarc.dll for decompression of FreeArc archives with displaying of progress indicator in Inno Setup window
;
;[Russian]
;Пример распаковки FreeArc архива, с отображением прогресс бара в окне Inno Setup при помощи unarc.dll
[Setup]
AppName=My Program
AppVerName=My Program version 1.5
DefaultDirName={pf}\My Program
DefaultGroupName=My Program
Compression=zip

[Languages]
;Name: rus; MessagesFile: compiler:Languages\Russian.isl

[Files]
Source: *.arc; DestDir: {app}; Flags: ignoreversion
Source: ..\unarc.dll; DestDir: {tmp}; Flags: dontcopy
Source: InnoCallback.dll; DestDir: {tmp}; Flags: dontcopy

[Icons]
Name: {group}\Uninstall; IconFilename: {app}\unins000.exe; Filename: {app}\unins000.exe

[Code]
type
  TMsg = record
    hwnd: HWND;
    message: UINT;
    wParam: Longint;
    lParam: Longint;
    time: DWORD;
    pt: TPoint;
  end;

const
  PM_REMOVE      = 1;

function PeekMessage(var lpMsg: TMsg; hWnd: HWND; wMsgFilterMin, wMsgFilterMax, wRemoveMsg: UINT): BOOL; external 'PeekMessageA@user32.dll stdcall';
function TranslateMessage(const lpMsg: TMsg): BOOL; external 'TranslateMessage@user32.dll stdcall';
function DispatchMessage(const lpMsg: TMsg): Longint; external 'DispatchMessageA@user32.dll stdcall';

procedure AppProcessMessage;
var
  Msg: TMsg;
begin
  while PeekMessage(Msg, WizardForm.Handle, 0, 0, PM_REMOVE) do begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;
end;



type
  PAnsiChar=PChar;     // Remove this line if you run InnoSetup 5.3.0+
  TFreeArcCallback = function (what: PAnsiChar; int1, int2: Integer; str: PAnsiChar): Integer;

function WrapFreeArcCallback (callback: TFreeArcCallback; paramcount: integer):longword;
  external 'wrapcallback@files:innocallback.dll stdcall';

function FreeArcExtract (callback: longword; cmd1,cmd2,cmd3,cmd4,cmd5,cmd6,cmd7,cmd8,cmd9,cmd10: PAnsiChar): integer; external 'FreeArcExtract@files:unarc.dll cdecl';



var
 ProgressBar: TNewProgressBar;
 ExtractFile: TNewStaticText;
 Button1:     TButton;
 Cancel:      Integer;

procedure InitializeWizard();
begin
  ProgressBar := TNewProgressBar.Create(WizardForm);
  ExtractFile:=TNewStaticText.Create(WizardForm);
  with WizardForm.ProgressGauge do
    begin
      ProgressBar.Left := Left;
      ProgressBar.Top := Top + ScaleX(55);
      ProgressBar.Width := Width;
      ProgressBar.Height := Height;
      ProgressBar.Parent := WizardForm.InstallingPage;
      ProgressBar.max := 1000;
      ProgressBar.Position := 0;
      ExtractFile.parent:=WizardForm.InstallingPage;
      ExtractFile.autosize:=false;
      ExtractFile.Width := Width;
      ExtractFile.top:=Top + ScaleX(35);
      ExtractFile.caption:='Распаковка архива FreeArc';
    end;
end;

procedure Button1OnClick(Sender: TObject);
begin
  Cancel := -1;
end;

function FreeArcCallback (what: PAnsiChar; int1, int2: Integer; str: PAnsiChar): Integer;
var percents: Integer;
begin
  if string(what)='filename' then
      //ExtractFile.Caption:=str
  else if (string(what)='progress') and (int2>0) then begin
      percents := (int1*1000) div int2;
      ProgressBar.Position := percents;
      ExtractFile.Caption:='Распаковано '+IntToStr(int1)+' из '+IntToStr(int2)+' мб ('+FloatToStr(percents/10)+'%)';
  end;
  AppProcessMessage;
  Result := Cancel;
end;

procedure ExtractFreeArcArchive(arcname: String; destpath: String);
var callback: longword;
    res: Integer;
begin
  AppProcessMessage;
  callback:=WrapFreeArcCallback(@FreeArcCallback,4);   //FreeArcCallback has 4 arguments
  Cancel := 0;
  try
   res := FreeArcExtract (callback, 'x', '-o+', '-dp'+destpath, '--', arcname, '', '', '', '', '');
   if cancel<0 then
     MsgBox('Installation cancelled', mbInformation, MB_OK);
   if res<0 then
     MsgBox('Decompression failed with error code '+IntToStr(res)+'!', mbError, MB_OK);
   Button1.visible:=false;
  except
   MsgBox('Decompression failed!', mbError, MB_OK);
   Button1.visible:=false;
  end;
  DeleteFile(arcname);
end;

procedure CurStepChanged(CurStep: TSetupStep);
var app: String;
begin
  If CurStep=ssPostInstall then
   begin
    Button1:=TButton.create(WizardForm);
    Button1.parent:=WizardForm;
    Button1.width:=135;
    Button1.caption:='Cancel installation';
    Button1.left:=260;
    Button1.top:=WizardForm.cancelbutton.top;
    Button1.OnClick:=@Button1OnClick;
    
    app := ExpandConstant('{app}');
    ExtractFreeArcArchive(app+'\1.arc', app);
    ExtractFreeArcArchive(app+'\2.arc', app);
   end;
end;
