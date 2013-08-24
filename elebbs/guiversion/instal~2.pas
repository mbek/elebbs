unit InstallForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, IniFiles, DdeMan, ShlObj, RxShell, StUtils,
  ProgIcon;

type
  TInstallFormForm = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    RadioGroup1: TRadioGroup;
    Timer1: TTimer;
    DdeClientConv1: TDdeClientConv;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  InstallFormForm: TInstallFormForm;

implementation

uses SelGroup;

{$R *.DFM}

procedure TInstallFormForm.Button2Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TInstallFormForm.Button1Click(Sender: TObject);
var SystemIni: TIniFile;
    StartUpName: String;
begin
  ProgramGroupForm.ShowModal;
  StartupName := ProgramGroupForm.SelectedName;
  if MessageDlg('Add ELESTART to group "'+StartupName+'"', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then EXIT;

   if CreateProgManGroup(DdeClientConv1, StartupName) then
     begin
       CreateProgManItem(DdeClientConv1, StartupName, 'EleStart', 'C:\Bbs\Elstart.exe '+FStr(RadioGroup1.ItemIndex));
     end
      else MessageDlg('Unable to create group!', mtError, [mbOk], 0);
      
  if RadioGroup1.ItemIndex=0 then
    begin
      MessageDlg('You need to select a comport to which your modem is attached', mtError, [mbOk], 0);
      EXIT;
    end; { if }

  SystemIni := TIniFile.Create('System.Ini');
  SystemIni.WriteString('386enh', 'Com'+IntToStr(RadioGroup1.ItemIndex)+'AutoAssign', '0');
  SystemIni.Free;

  ExitWindowsEx(EWX_REBOOT, 0);
end;

procedure TInstallFormForm.Timer1Timer(Sender: TObject);
begin
  Button1.Enabled := true;
  Button2.Enabled := true;
end;

end.
