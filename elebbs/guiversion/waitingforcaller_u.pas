unit waitingforcaller_u;
{$I C:\BBS\COMPILER.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, TB97, Menus, Chat, Global, Control, ApFossil,
  Wfc, GenFile, FileROut, ExtCtrls, LongStr, Cases, ComUnit;

type
  TWaitingForCallerForm = class(TForm)
    GroupBox2: TGroupBox;
    ListBox2: TListBox;
    GroupBox3: TGroupBox;
    StatusBar1: TStatusBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    PagingPopupMenu: TPopupMenu;
    Item1: TMenuItem;
    Item2: TMenuItem;
    Item3: TMenuItem;
    WaitingForCallerTimer: TTimer;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Button1: TButton;
    Button2: TButton;
    ListBox1: TListBox;
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure WaitingForCallerTimerTimer(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure DoWait;
    procedure UpdateModemDisplay(S: String);
    procedure UpdateStatus(S: String);
  end;

var DidWaitForCaller : Boolean = false;
    WFC_DoExit: Boolean = false;
    ModemInitted: Boolean = false;
    Timer_AlreadyActive: Boolean = false;

var
  WaitingForCallerForm: TWaitingForCallerForm;

implementation

uses Win_main;

{$R *.DFM}

procedure TWaitingForCallerForm.CheckBox1Click(Sender: TObject);
begin
  Snooping := CheckBox1.Checked;
end;

procedure TWaitingForCallerForm.CheckBox2Click(Sender: TObject);
begin
  AnswerBBS := CheckBox2.Checked;
end;

procedure TWaitingForCallerForm.UpdateStatus(S: String);
begin
  StatusBar1.Panels[0].Text := S;
  WaitingForCallerform.Caption := 'Waiting for caller ['+S+']';
end; { proc. UpdateStatus }

procedure TWaitingForCallerForm.UpdatemodemDisplay(S: String);
begin
  ListBox2.Items.Add(s);
end; { proc. UpdatemodemDisplay }

procedure TWaitingForCallerForm.DoWait;
var CH        : Char;
    TempBool  : Boolean;
    SavePort  : Byte;
    InitCount : Byte;
    TempStr   : String;
    p         : pointer;
begin
  AnswerBBS := Modem^.AnswerPhone;
  CheckBox2.Checked := AnswerBBS;
  InitCount := 00;
  TempBool := true;

  If NOT ModemInitted then
   repeat
    Application.ProcessMessages;
    Sleep(0);

    if InitCount>00 then
       UpdateStatus('Initialise failure (#'+FStr(InitCount)+')')
        else UpdateStatus('Initialising the modem');

    TempBool := TryInitModem;
    if TempBool then
      begin
         UpdateModemDisplay(Modem^.InitResp);
         {$IFDEF WITH_FULL}
           if ComObj <> nil then
             ComObj^.Com_PurgeInBuffer;
         {$ENDIF}
      end; { if }
    Inc(InitCount);
  until (TempBool) OR (InitCount > Modem^.InitTries)
          OR (Application.Terminated) OR (LocalLogon);

  if NOT TempBool then Halt(1);
  ModemInitted := true;
  UpdateStatus('Waiting for a caller');

  if (NOT LocalLogon) then
   if (NOT Application.Terminated) then
    if (InitCount <= Modem^.InitTries) then
     repeat
       Application.ProcessMessages;
       Sleep(0);

       if ProgTerminated then EXIT;
       if ComObj = NIL then EXIT;

       if ComObj <> NIL then
        if ComObj^.Com_CharAvail then
         begin
           TempStr := GetModemStr;
           UpdateModemdisplay(TempStr);

           if Pos(SUpCase(Modem^.RingStr), SUpCase(TempStr))>00 then
            if AnswerBBS then
              SendModem(Modem^.AnswerStr);

            if (CheckStr(Modem^.ConnectFax, -1, TempStr)) OR
                (CheckStr(Modem^.Connect115k, 11520, TempStr)) OR
                 (CheckStr(Modem^.Connect64k, 64000, TempStr)) OR
                  (CheckStr(Modem^.Connect57k, 57600, TempStr)) OR
                   (CheckStr(Modem^.Connect38k, 38400, TempStr)) OR
                    (CheckStr(Modem^.Connect33k, 33600, TempStr)) OR
                     (CheckStr(Modem^.Connect31k, 31200, TempStr)) OR
                      (CheckStr(Modem^.Connect28k, 28800, TempStr)) OR
                       (CheckStr(Modem^.Connect26k, 26400, TempStr)) OR
                        (CheckStr(Modem^.Connect24k, 24400, TempStr)) OR
                         (CheckStr(Modem^.Connect21k, 21600, TempStr)) OR
                          (CheckStr(Modem^.Connect19k, 19200, TempStr)) OR
                           (CheckStr(Modem^.Connect16k, 16800, TempStr)) OR
                            (CheckStr(Modem^.Connect14k, 14400, TempStr)) OR
                             (CheckStr(Modem^.Connect12k, 12000, TempStr)) OR
                              (CheckStr(Modem^.Connect9600, 9600, TempStr)) OR
                               (CheckStr(Modem^.Connect7200, 7200, TempStr)) OR
                                (CheckStr(Modem^.Connect4800, 4800, TempStr)) OR
                                 (CheckStr(Modem^.Connect2400, 2400, TempStr)) OR
                                  (CheckStr(Modem^.Connect1200, 1200, TempStr)) OR
                                   (CheckStr(Modem^.Connect300, 300, TempStr)) then
                                  begin
                                    Exitinfo^.ErrorFreeConnect := GetOk(Modem^.ErrorFreeString);
                                    WFC_DoExit := true;
                                    EXIT;
                                  end; { if }
       end; { if }

      if CheckEventStatus then
        begin
          Halt(defExitCode);
        end; { Event Busy }

      if NOT CheckRaExit then ;
    until (true=false) OR (application.terminated) OR (LocalLogon) OR (wfc_Doexit);
end; { proc. DoWait }

procedure TWaitingForCallerForm.FormShow(Sender: TObject);
var TempEvent: String;
begin
  SearchNextEvent(Exitinfo^.EventInfo);
  ReadSysInfoBBS(Exitinfo^.Sysinfo);
  If Exitinfo^.EventInfo.StartTime='00:00' then TempEvent := 'none'
    else TempEvent := Exitinfo^.EventInfo.StartTime;

  CheckBox1.Checked := Snooping;
  CheckBox2.Checked := AnswerBBS;

  Label2.Caption := IntToStr(RaNodeNr);
  Label4.Caption := 'COM' + IntToStr(Modem^.Comport);
  Label6.Caption := TempEvent;
  Label8.Caption := Exitinfo^.Sysinfo.Lastcaller;
  Label10.Caption := IntToStr(Exitinfo^.SysInfo.TotalCalls);
  ListBox1.ItemIndex := RaPageStat - 1;
end;

procedure TWaitingForCallerForm.WaitingForCallerTimerTimer(Sender: TObject);
begin
  if Timer_AlreadyActive then EXIT;
  Timer_AlreadyActive := true;

  if NOT WFC_DoExit then
    begin
      DoWait;
      WaitingForCallerForm.Close;
    end { if }
      else begin
              WaitingForCallerTimer.Enabled := false;
              WaitingForCallerForm.Close;
           end; { if }
end;

procedure TWaitingForCallerForm.ListBox1Click(Sender: TObject);
begin
  RaSetPageStat(Listbox1.ItemIndex + 01);
end;

procedure TWaitingForCallerForm.Button1Click(Sender: TObject);
begin
  LocalLogon := true;
  Exitinfo^.Baud := 00;
  CarrierCheck := false;
  InheritedHandle := -1;
  TakeModemOffHook;
  Progterminated := false;
  Wfc_DoExit:= true;
  CloseComport;

  WaitingForCallerForm.Close;
end;

procedure TWaitingForCallerForm.Button2Click(Sender: TObject);
begin
  WaitingForCallerTimer.Enabled := false;
  LocalLogon := true;
  Exitinfo^.Baud := 00;
  CarrierCheck := false;
  InheritedHandle := -1;
  TakeModemOffHook;
  ProgTerminated := true;
  Wfc_DoExit := true;
  defExitCode := 0;
  CloseComport;

  WaitingForCallerForm.Close;
end;

procedure TWaitingForCallerForm.FormActivate(Sender: TObject);
begin
  if DidWaitForCaller then EXIT;
  DidWaitForCaller := true;
  DoWait;
end;

begin
  DidWaitForCaller := true;
  WFC_DoExit := false;
  ModemInitted := false;
end.
