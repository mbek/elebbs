unit Win_main;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Console, StdCtrls, ExtCtrls, Buttons,
  EditMain, Security, Menus, Tabs, Spin, FileRout,
  RXCtrls, Placemnt, SpeedBar, TB97, ComCtrls,
  StUtils, Cases, Global,  ElLog_U, WFC,
  StrPath, RXShell, MPlayer, 
  Support, Mailer, Startup;


type
  TForm1 = class(TForm)
    PrintDialog1: TPrintDialog;
    FontDialog1: TFontDialog;
    FormStorage1: TFormStorage;
    Dock971: TDock97;
    Toolbar972: TToolbar97;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Toolbar973: TToolbar97;
    ChatButton: TToolbarButton97;
    HangupButton: TToolbarButton97;
    LockoutButton: TToolbarButton97;
    EditUserButton: TToolbarButton97;
    PagingButton: TToolbarButton97;
    PagingPopupMenu: TPopupMenu;
    Item1: TMenuItem;
    Item2: TMenuItem;
    Item3: TMenuItem;
    GarbageButton: TToolbarButton97;
    SecurityButton: TToolbarButton97;
    FontButton: TToolbarButton97;
    CopyButton: TToolbarButton97;
    PrintButton: TToolbarButton97;
    ToolbarSep971: TToolbarSep97;
    CheckBox1: TCheckBox;
    WantChatTrayIcon: TRxTrayIcon;
    ColorConsole1: TColorConsole;
    Panel3: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    TabSet1: TTabSet;
    SpinEdit1: TSpinEdit;
    Timer1: TTimer;
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure TabSet1Change(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure SpinEdit1Change(Sender: TObject);
    procedure PrintButtonClick(Sender: TObject);
    procedure CopyButtonClick(Sender: TObject);
    procedure FontButtonClick(Sender: TObject);
    procedure ChatButtonClick(Sender: TObject);
    procedure HangupButtonClick(Sender: TObject);
    procedure LockoutButtonClick(Sender: TObject);
    procedure EditUserButtonClick(Sender: TObject);
    procedure Item1Click(Sender: TObject);
    procedure Item2Click(Sender: TObject);
    procedure Item3Click(Sender: TObject);
    procedure PagingPopupMenuPopup(Sender: TObject);
    procedure GarbageButtonClick(Sender: TObject);
    procedure SecurityButtonClick(Sender: TObject);
    procedure StatusbarBoxClick(Sender: TObject);
    procedure NewHandleException(Sender: TObject; E: Exception);
    procedure CheckBox1Click(Sender: TObject);
    procedure WantChatTrayIconClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses Main, Menu, Chat, SysKey,
      InOut_U, Control, Transfer, CfgRec, Limit_U,
       StatusB, ExitProg, TagUnit, waitingforcaller_u;

procedure TForm1.SpeedButton5Click(Sender: TObject);
begin
 If FontDialog1.Execute then
  begin
   ColorConsole1.Font.Assign(FontDialog1.Font);
  end; { if }
end;

procedure TForm1.SpeedButton7Click(Sender: TObject);
begin
  ShowGarbage;
end;

procedure TForm1.SpeedButton9Click(Sender: TObject);
begin
  Snooping := NOT Snooping;
end;

procedure TForm1.SpeedButton10Click(Sender: TObject);
begin
  Exitinfo^.SysOpNext := NOT ExitInfo^.SysOpNext;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CheckBox2.Checked := PrinterLogging;
  CheckBox3.Checked := Snooping;
  StatusBarBox.Checked := True;
  Form1.Caption := Application.Title;
  Timer1.Enabled := false;

  (*
  TabSet1.TabIndex := Exitinfo^.StatusLine;
  *)
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  PrinterLogging := CheckBox2.Checked;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  Snooping := CheckBox3.Checked;
  ColorConsole1.Visible := Snooping;
end;

procedure TForm1.TabSet1Change(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
begin
  If NewTab in [0..8] then
   begin
     AllowChange := True;

     if Form1.TabSet1.Tag = 0 then
       StatusDisplay(Byte(NewTab)+1);
   end
     else AllowChange := False;

end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  if SpinEdit1.Text = '' then EXIT;
  if SpinEdit1.Tag = 1 then EXIT;

  SpinEdit1.Tag := 1;

  ChangeTime(TSpinEdit(Sender).Value - Exitinfo^.TimeLimit, true);
  StatusDisplay(11);
  
  SpinEdit1.Tag := 0;
end;


procedure TForm1.PrintButtonClick(Sender: TObject);
begin
  try
    If PrintDialog1.Execute then
      ColorConsole1.PrintThisScreen;
  except
    Messagedlg('Error occured while printing', mtError, [mbCancel], 0);
  end;
end;

procedure TForm1.CopyButtonClick(Sender: TObject);
begin
  ColorConsole1.CopyToClipBoard;
end;

procedure TForm1.FontButtonClick(Sender: TObject);
begin
 If FontDialog1.Execute then
  begin
   ColorConsole1.Font.Assign(FontDialog1.Font);
  end; { if }
end;

procedure TForm1.ChatButtonClick(Sender: TObject);
begin
   ChatButton.Enabled := False;
   If NOT NowChatting then
      BeginChatting;
   ChatButton.Enabled := True;
end;

procedure TForm1.HangupButtonClick(Sender: TObject);
begin
  HangUpButton.Enabled := False;
  ChatButton.Enabled := False;
  LockoutButton.Enabled := False;
  EdituserButton.Enabled := False;

  ProgTerminated := true;
end;

procedure TForm1.LockoutButtonClick(Sender: TObject);
begin
  HangUpButton.Enabled := False;
  ChatButton.Enabled := False;
  LockoutButton.Enabled := False;
  EdituserButton.Enabled := False;

  LockoutUsers;
end;

procedure TForm1.EditUserButtonClick(Sender: TObject);
begin
  Move(Exitinfo^.Userinfo, UserEdit.UserInf, SizeOf(UsersRecord));

  UserEdit.CurOnline := True;
  UserEdit.CurTimeLimit := Exitinfo^.TimeLimit;

  UserEdit.ShowModal;

  repeat
    Application.HandleMessage;
  until NOT Useredit.Visible;

  Move(UserEdit.UserInf, Exitinfo^.Userinfo, SizeOf(UsersRecord));
  Exitinfo^.TimeLimit := UserEdit.CurTimeLimit;
  SpinEdit1.Value := Exitinfo^.TimeLimit;
  StatusDisplay(11);
  (*
  SetTime(Exitinfo^.TimeLimit);
  *)
end;

procedure TForm1.Item1Click(Sender: TObject);
begin
  Item1.Checked := True;
  Item2.Checked := False;
  Item3.Checked := False;
  RaSetPageStat(1);
end;

procedure TForm1.Item2Click(Sender: TObject);
begin
  Item1.Checked := False;
  Item2.Checked := True;
  Item3.Checked := False;

  RaSetPageStat(2);
end;

procedure TForm1.Item3Click(Sender: TObject);
begin
  Item1.Checked := False;
  Item2.Checked := False;
  Item3.Checked := True;

  RaSetPageStat(3);
end;

procedure TForm1.PagingPopupMenuPopup(Sender: TObject);
begin
  Item1.Checked := False;
  Item2.Checked := False;
  Item3.Checked := False;

   Case RaPageStat of
     01 : Item1.Checked := True;
     02 : Item2.Checked := True;
     03 : Item3.Checked := True;
   end; { case }
end;

procedure TForm1.GarbageButtonClick(Sender: TObject);
begin
 Showgarbage;
end;

procedure TForm1.SecurityButtonClick(Sender: TObject);
begin
  UserLevel.SpinEdit1.Value := Exitinfo^.Userinfo.Security;
  UserLevel.ShowModal;

  Exitinfo^.Userinfo.Security := UserLevel.SpinEdit1.Value;

  {$ifdef With_Full}
  GetLevelLimitsInfo(Exitinfo^.Userinfo.Security, True);
  UpdateUserRecord;
  {$Endif}

  StatusDisplay(11);
end;

procedure TForm1.StatusbarBoxClick(Sender: TObject);
begin
  Panel3.Visible := StatusBarBox.Checked;
  FormResize(Self);
end;

{$IFDEF WIN32}
procedure TForm1.NewHandleException(Sender: TObject; E: Exception);
var S: String;
begin
  S := Format('Invalid exception: %s"%s" at address %p.%sProgram terminated', [#13+#10, E.Message, ExceptAddr, #13+#10]);

  WriteLn;
  Writeln(S);

  RaLog('!', S);

  {$IFDEF WITH_DEBUG}
    DebugLog(logErrors, S);
  {$ENDIF}

  Halt(255);
  Abort;
end; { proc. NewHandleException }
{$ENDIF}

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if (Sender as TCheckBox).Checked then
    ColorConsole1.ShowScrollBars := true
     else ColorConsole1.ShowScrollBars := false;
end;

procedure TForm1.WantChatTrayIconClick(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Form1.Show;
  Form1.BringToFront;
  Form1.WindowState := wsNormal;
end;

procedure TForm1.FormResize(Sender: TObject);
var StatusPanelSize: Longint;
begin
  StatusPanelSize := Panel3.Height;
  if NOT StatusBarBox.Checked then StatusPanelSize := 0;

  ColorConsole1.Height := Form1.Height - (Dock971.Height + StatusPanelSize + 65);
  ColorConsole1.Width := Form1.Width - (30);
  Panel3.Top := (Form1.Height - (StatusPanelSize + 40));
end;


procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caNone;

  if MessageDlg('Are you sure you want to terminate?', mtWarning, [mbYes, mbNo], 0) = mrYes then
    begin
      HangUpButtonClick(Sender);
      Action := caFree;
    end; { if }
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  FormResize(self);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;

  SetTimeout;

  if Form1.Tag <> 00 then EXIT;
  if (ProgTerminated) OR (Application.Terminated) then
    begin
      Application.Terminate;
      EXIT;
    end; { if }  

  Form1.Tag := 01;
  WaitingForCallerForm.WaitingForCallerTimer.Enabled := false;
  SpinEdit1.Value := Exitinfo^.TimeLimit;

  StartupBBS;
  if NOT Application.Terminated then
   if NOT ProgTerminated then
    Start_This_Program;
  PostQuitMessage(0);

end;

end.
