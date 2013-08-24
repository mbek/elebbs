unit Chat;
(*
**
** Copyright (C) 1999-2003 Maarten Bekers. All rights reserved.
**
** This file is part of EleBBS, EleXer and EleWEB.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
** This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
** WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
**
*)
{$I COMPILER.INC}
(*
**
** Chatter routines for EleBBS
**
** Copyright (c) 1996 by Maarten Bekers
**
** Created : 29-Sep-1996
** Last update : 29-Sep-1996
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF WITH_FULL}
  This unit shouldn''t be compiled for an utility program
{$ENDIF}

{$IFDEF ISCGI}
  This unit should not be compiled into EleWEB
{$ENDIF}

type tChatterObj = Object
         ChatLogging : Boolean; { default false; }
         LogName     : String[128]; { default: CHAT.LOG }
         OldChatColor  : Byte;
         Save_SysOp    : record
                            X,Y: Byte;
                         end;
         Save_user     : record
                           X,Y: Byte;
                         end;
         UsingIemsiChat: Boolean;
         Log_F         : Text;
         LogStr        : String[80];

         {-- Constructors and destructors ----------------------------------}
         constructor Init;
         destructor Done;

         {-- Generic routines ----------------------------------------------}
         procedure RaSetPageStat (Status : Byte);           { Set pageStatus }
         function  DorPage (SysAbort,UsrAbort,PageFile : String) : Char; { Page sysop }
         function  RaPageStat: Byte;                       { Get page status }
         procedure PickPaging;                  { Pick paging (On/Off/Hours) }
         procedure BeginChatting;
         procedure HandleChatInput(CH: Char; SysOpkey: Boolean; var TempStr, LastStr: String);

         {-- Private routines ----------------------------------------------}
         private
           procedure OpenLogFile;
           procedure CloseLogFile;
           function  IEMSI_Chat(Start: Boolean): Boolean;
           procedure CheckToScroll;
     end; { tChatterObj }

type
  pChatterObj = ^tChatterObj;


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses {$IFDEF WITH_DEBUG}
       Debug_U,
     {$ENDIF}

     {$IFDEF VirtualPascal}
       VpSysLow,
     {$ENDIF}

     {$IFDEF WIN32}
       Windows,
     {$ENDIF}

     {$IFDEF WINGUI}
       Win_Main,
     {$ELSE}
       Crt,
       ScrnU,
     {$ENDIF}
       EditUser, Global, FastScrn, StatusB, ApTimer, GenFile, Avatar,
       ElLog_U, StrPath, Support, Cases, WordStr, Sound_U, LongStr,
       InOut_U, CfgFile, Colors, StUtils, Input_U, CfgRec,
       LineEd, JDates, Outblock, ComUnit, EMSI, ObjDec,
       CentrStr, DispAns, RAL, ExecFunc, Control,
       MultiLn, ApFossil, FileObj;


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tChatterObj.Init;
begin
  ChatLogging := false;
  LogName := 'chat.log';
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tChatterObj.done;
begin
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tChatterObj.PickPaging;                { Pick paging (On/Off/Hours) }
var CH      : Char;
    HiLight : Byte;
    SaveScrn: Pointer;
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logChat, 'PickPaging');{$endif}
  SaveScreen(SaveScrn);
  LineCfg^.LocalOnly := True;

  BoxWindow(35, 12, 56, 16, LightCyan, Black, '');
  FastWrite(38, 12, Yellow, ' Paging options ');
  HiLight := 01;
  Repeat;
   FastWrite(36, 13, Cyan, '  Use paging hours  ');
   FastWrite(36, 14, Cyan, '  Turn paging OFF   ');
   FastWrite(36, 15, Cyan, '  Turn paging ON    ');
   Case HiLight of
    01 : FastWrite(36, 13, 112, '  Use paging hours  ');
    02 : FastWrite(36, 14, 112, '  Turn paging OFF   ');

03 : FastWrite(36, 15, 112, '  Turn paging ON    ');
   End; { Case }

   CH := UserEditObj^.GetLocalKey;

   if CH in [#0] then
     begin
       CH := UserEditObj^.GetLocalKey;

       Case Ord(CH) of
         72 : Dec(HiLight);
         80 : Inc(HiLight);
       End; { Case }

                    If HiLight<1 then HiLight:= 3;
                    If HiLight>3 then HiLight:= 1;
                  end; { case }

  Until CH in [#13, #27];

  If CH=#13 then RaSetPageStat(HiLight);              { Set selected pagestat }

  LineCfg^.LocalOnly:=False;
  RestoreScreen(SaveScrn);
  StatusDisplay(11, false);
end; { proc. PickPaging }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tChatterObj.DorPage (SysAbort,UsrAbort,PageFile : String) : Char; { Page sysop }
var SysAbortSet,
    UsrAbortSet: CfgRec.CharSet;
    PageID: String;
    Page_F: Text;
    PageStr: String;
    AbortPaging: Boolean;
    CH: Char;
    Timer: EventTimer;
    NoPage: Byte;
begin
 DorPage := #00;
 {$ifdef With_Debug} DebugObj.DebugLog(logChat, 'Dorpage');{$endif}

 Str2Set(SysAbort, SysAbortSet);
 Str2Set(UsrAbort, UsrAbortSet);

 If (PageFile='') OR (NOT FileExist(PageFile)) then
  PageFile := PageFileName;

 Assign(Page_F, PageFile);
 {$i-} System.Reset(Page_F); {$I+}
 If IOResult>00 then
    begin
       RaLog('!', JustName(PageFileName)+' not found!');
    end; { if }

 AbortPaging := False;

 NewTimer(Timer, Secs2Tics(GlobalCfg^.RaConfig^.PageLength));

 NoPage := 0;

 repeat
  {$i-} ReadLn(Page_F, PageStr); {$I+}
  If IOResult>00 then
   begin
     Case NoPage of
       0 : begin
             NoPage := 01;
             PageStr := 'TONE 426 20';
           end;
       1 : begin
             NoPage := 02;
             PageStr := 'TONE 716 20';
           end;
       2 : begin
             NoPage := 00;
             PageStr := 'TONE 426 20';
           end;
     end; { case }
   end; { if }

  {$IFDEF WITH_FULL}
  {$IFDEF MSDOS}
    Support.Delay(1);
  {$ELSE}
    {$IFNDEF VirtualPascal}
     {$IFNDEF FPC}
      Sleep(0);
     {$ENDIF}
    {$ELSE}
      SysCtrlSleep(0);
    {$ENDIF}
  {$ENDIF}
  {$ENDIF}

  PageID := SUpcase(FirstWord(PageStr, defExtractWord, false));

  If PageId='TONE' then
    begin
      if NOT ScrollLockOn then
        Sound(FVal(FirstWord(PageStr, defExtractWord, false)),  { ScrollLock active, no sound }
              FVal(FirstWord(PageStr, defExtractWord, false)) * 10);
      NoSound;
    end; { If Tone }

  If PageId='WAIT' then
    begin
      NoSound;
      {$IFNDEF VIRTUALPASCAL}
        Support.Delay(FVal(FirstWord(PageStr, defExtractWord, false)) * 10);
      {$ELSE}
        SysCtrlSleep(FVal(FirstWord(PageStr, defExtractWord, false)) * 10);
      {$ENDIF}
    end; { If Tone }

  {$i-}
    If Eof(Page_F) then System.Reset(Page_F);
  {$i+}
  if IOREsult > 00 then ;

  If InputObj^.Keypressed then
   begin;
     If InputObj^.KeyPressed then
      CH := InputObj^.ReadKey;

     If (LineCfg^.SysOpKey) then
      If CH in SysAbortSet then
       begin;
        DorPage := CH;
        AbortPaging := True;
       End; { Sysop pressed abort key }

     If (NOT LineCfg^.SysOpKey) then
      If CH in UsrAbortSet then
       begin;
        DorPage := CH;
        AbortPaging := True;
       End; { User pressed abort key }
   End; { if OutputObj^.Keypressed }

  If TimerExpired(Timer) then AbortPaging := True;

 Until (AbortPaging);

 {$i-} Close(Page_F); {$I+}
 If IOResult>00 then;
end; { func. DorPage }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tChatterObj.RaSetPageStat (Status : Byte);                    { Set pageStatus }
var RaPage    : String;
    PageStat_F: pFileObj;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logChat, 'RaSetPageStat');
  {$ENDIF}

  New(PageStat_F, Init);
  PageStat_F^.Assign(PageStatFileName);
  PageStat_F^.FileMode := ReadWriteMode + DenyNone;
  if PageStat_F^.Create(1) then
    begin
      RaPage := FStr(Status);
      PageStat_F^.BlkWrite(RaPage[1], 1);
    end; { if }

  Dispose(PageStat_F, Done);
end; { proc. RaSetPageStat }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tChatterObj.RaPageStat : Byte;                               { Get page status }
var RaPage    : Char;
    PageStat_F: pFileObj;
    OldFMode  : Longint;
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logChat, 'RaPageStat');{$endif}

  if NOT FileExist(PageStatFileName) then RaSetPageStat(0);

  if Config_OpenFile(PageStat_F, PageStatFileName, 01, ReadMode + DenyNone, False, False)>00 then EXIT;

  {$i-}
  if NOT PageStat_F^.EOF then
    PageStat_F^.BlkRead(RaPage, 01);
  {$i+}
  if PageStat_F^.IOresult>00 then ;

  Config_DoneFile(PageStat_F);

  RaPageStat := Fval(RaPage);
end; { func. RaPageStat }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tChatterObj.OpenLogFile;
var OldA    : Byte;
    SaveScrn: Pointer;
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logChat, 'OpenLogFile'); {$endif}
  If ChatLogging then EXIT;                                 { Already logging }

  ChatLogging := False;
  OldA := {$IFDEF WINGUI}Form1.ColorConsole1.{$ENDIF}TextAttr;

  SaveScreen(SaveScrn);

  if EditBox(Logname, 02, 10, 76, 14, 'Capture filename', 70, true, false) = #13 then
    begin
      ChatLogging := True;

      Assign(Log_F, LogName);
      {$i-}
        Append(Log_F);
        If IOResult>00 then System.ReWrite(Log_F);
      {$I+}
      if IOResult>00 then begin;
                            ChatLogging := False;
                            exit;
                          end;

      WriteLn(Log_F);
      WriteLn(Log_F, '** Capture log file opened at '+TimeStr(False,False)+' on '+DateStr);
      WriteLn(Log_F, '** Chatting with '+lineCfg^.Exitinfo^.Userinfo.Name+' of '+lineCfg^.Exitinfo^.Userinfo.Location);
      WriteLn(Log_F);
    end; { LogName }
  RestoreScreen(SaveScrn);

  {$IFNDEF WINGUI}
  Crt.TextAttr := OldA;
  {$ELSE}
  Form1.ColorConsole1.TextAttr := OldA;
  {$ENDIF}
end; { proc. OpenlogFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tChatterObj.CloseLogFile;
var SaveScrn: pointer;
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logChat, 'CloseLogFIle');{$endif}
  If NOT ChatLogging then EXIT;

  {$i-}
    WriteLn(Log_F, LogStr);
    Close(Log_F);
  {$I+}
  If IOResult>00 then;

  SaveScreen(SaveScrn);
  BoxWindow(30, 12, 50, 16, GlobalCfg^.RaConfig^.BorderFore, GlobalCfg^.RaConfig^.BorderBack, '');
  FastWrite(32, 14, MakeAttr(GlobalCfg^.RaConfig^.WindFore, GlobalCfg^.RaConfig^.WindBack), 'Capturelog closed');

  {$IFNDEF VirtualPascal}
    Support.Delay(1500);
  {$ELSE}
    SysCtrlSleep(1500);
  {$ENDIF}
  RestoreScreen(SaveScrn);

  ChatLogging := False;
end; { proc. CloseLogFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tChatterObj.IEMSI_Chat(Start: Boolean): Boolean;
Const IEMSI_StartChat = '**EMSI_CHTF5D4';
      IEMSI_EndChat   = '**EMSI_TCH3C60';

      IEMSI_ACK       = '**EMSI_ACKA490';

var ChatString : String;
    Buffer     : Array[1..Length(IEMSI_ACK)] of Char;
begin
  IEMSI_Chat := false;
  if (NOT lineCfg^.Exitinfo^.Emsi_Session) OR (Pos('CHT', LineCfg^.EMSI_User^.Capabilities)=0)
    then EXIT;

  If Start then ChatString := IEMSI_Startchat
    else ChatString := IEMSI_EndChat;

  OutBlockObj^.DumpBlock;
  if ComObj <> NIL then
    ComObj^.Com_SendString(ChatString + #13);

  FillChar(Buffer, SizeOf(Buffer), #00);

  if (NOT GetFossilBuffer(Buffer, 00, Length(IEMSI_ACK), 10)) OR
      (Buffer[1] <> '*') OR (Buffer[8] <> 'A') OR (Buffer[9] <> 'C')  then
        begin
          OutBlockObj^.DumpBlock;

          if ComObj <> NIL then
            ComObj^.Com_SendString(ChatString + #13);
        end; {  if }

  Delay(100);                                        { Let the buffer rest }
  IEMSI_Chat := true;
end; { func. Iemsi_Chat }


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tChatterObj.CheckToScroll;
var TempC   : Char;
    TempA   : Byte;
    XCounter: Byte;
    YCounter: Byte;
begin
  if Save_SysOp.Y > (((LineCfg^.actScrnLen) div 2) - 1) then
    begin
      For YCounter := 2 to (((LineCfg^.actScrnLen) div 2) - 1) do
       For XCounter := 1 to (mnuScrnWidth) do
         begin
           GetScreen(XCounter, YCounter, TempC, TempA);
           PutScreen(XCounter, YCounter - 01, TempC, TempA);
         end; { for }

      Dec(Save_SysOp.Y);

      {$IFNDEF WINGUI}
        AvtObj^.StatusAdjustXY[1] := Save_SysOp.X;
        AvtObj^.StatusAdjustXY[2] := Save_SysOp.Y;
      {$ELSE}
        Form1.ColorConsole1.CursorTO(Save_SysOp.X, Save_SysOp.y);
      {$ENDIF}

      FastWrite(Save_Sysop.X, Save_Sysop.Y, 07, Dup(#32, 80));
    end; { if }


  if Save_User.Y > (LineCfg^.actScrnLen - 1) then
    begin
      For YCounter := (((LineCfg^.actScrnLen - 1) div 2) + 02) to (LineCfg^.actScrnLen - 1) do
       For XCounter := 1 to (mnuScrnWidth) do
         begin
           GetScreen(XCounter, YCounter, TempC, TempA);
           PutScreen(XCounter, YCounter - 01, TempC, TempA);
         end; { for }

      Dec(Save_User.Y);

      {$IFNDEF WINGUI}
        AvtObj^.StatusAdjustXY[1] := Save_User.X;
        AvtObj^.StatusAdjustXY[2] := Save_User.Y;
      {$ELSE}
        Form1.ColorConsole1.CursorTO(Save_User.X, Save_User.y);
      {$ENDIF}

      FastWrite(Save_User.X, Save_User.Y, 07, Dup(#32, 80));
    end; { if }
end; { proc. CheckToScroll }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tChatterObj.HandleChatInput(CH: Char; SysOpkey: Boolean; var TempStr, LastStr: String);

procedure DoWrite(var X, Y: Byte; S: String; A: Byte; AddCr, Sysop: Boolean);
begin
  {$IFNDEF WINGUI}
    AvtObj^.StatusAdjustXY[1] := X;
    AvtObj^.StatusAdjustXY[2] := Y;
    TextAttr := A;
  {$ENDIF}

  {$IFDEF WINGUI}
    Form1.ColorConsole1.CursorTo(X, Y);
    Form1.ColorConsole1.TextAttr := A;
  {$ENDIF}

  LocalScreen(S);
  if AddCr then LocalScreenLn('');

  if Sysop then
   begin
     OutputObj^.SendString(s);
     if AddCr then
      begin
        OutputObj^.SendString(#13#10);
        InputObj^.UpdateScrn;
      end; { if }
   end; { if }

  {$IFNDEF WINGUI}
    X := AvtObj^.StatusAdjustXY[1];
    Y := AvtObj^.StatusAdjustXY[2];
  {$ELSE}
    X := Form1.ColorConsole1.WhereX;
    Y := Form1.ColorConsole1.WhereY;
  {$ENDIF}
end; { proc. DoWrite }

procedure DoOutput(S: String; DoLn: Boolean; Color: Byte);
begin
  if Color <> OldChatColor then
    if NOT UsingIEMSIChat then
     Write('`A', Color, ':');

  if (NOT UsingIEMSIChat) then
   begin
     Write(s);
     if DoLn then WriteLn;
   end; { if }

  if (UsingIEMSIChat) then
    begin
      if SysOpKey then
        begin
          CheckToScroll;

          DoWrite(Save_Sysop.X, Save_Sysop.Y, S, ChatSysopColor, DoLn, true);
        end; { if }

      if NOT SysOpKey then
        begin
          CheckToScroll;

          DoWrite(Save_User.X, Save_User.Y, S, ChatUserColor, DoLn, false);
        end;
    end; { if }
end; { proc. DoOutput }

var SaveRadu       : Boolean;
begin
  case CH of
    #13 : begin
            TempStr := '';
            DoOutput('', True, OldChatColor);

            if ChatLogging then
              begin
                {$i-} WriteLn(Log_F, LogStr); {$I+}
                if IOResult>00 then;
              end; { ChatLogging }

            LogStr := '';
            InputObj^.UpdateScrn;
          end; { Enter }
    #27 : TempStr := '';
    #127, { linux backspace }
    #08 : begin
            TempStr := #08 + #32 + #08;

            if ChatLogging then
              if Length(LogStr)>00 then
               Dec(LogStr[0]);
          end; { backspace }
  end; { case }

  if UsingIemsiChat then
    CheckToScroll;

  if SysOpkey then
   if CH in [#08, #32..#254] then
     begin
       if (OldChatColor <> ChatSysOpColor) then
         begin
           DoOutput('', false, ChatSysOpColor);
           Flush(Output);
           OldChatColor := ChatSysOpColor;
           LastStr := '';
         end; { OldChatColor }

       SaveRadu := LineCfg^.RaduCodes;
       LineCfg^.RaduCodes := False;
       DoOutput(TempStr, false, OldChatColor);
       if (Pos('`', TempStr) > 00) then Flush(Output);
       LineCfg^.RaduCodes := SaveRadu;
     end; { OldChatColor }

  if NOT SysOpkey then
    begin
      if OldChatColor<>ChatUserColor then
       begin
         DoOutput('', false, ChatUserColor);
         Flush(Output);
         OldChatColor := ChatUserColor;
         LastStr := '';
       end; { OldColor }

      SaveRadu := LineCfg^.RaduCodes;
      LineCfg^.RaduCodes := False;
      DoOutput(TempStr, false, OldChatColor);
      if (Pos('`', TempStr) > 00) then Flush(Output);

      LineCfg^.RaduCodes := SaveRadu;
    end; { OldColor }
end; { proc. HandleChatInput }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tChatterObj.BeginChatting;
const AbortWrapSet   : CharSet = [#13,#32,#10,'.',',',']',')','/','?','!'];

var CH             : Char;
    ExitCode       : Word;
    TempStr        : String;
    Success        : Boolean;
    LastStr        : String;
    SaveInactivity : Boolean;
    IemsiSave      : Pointer;


procedure StartIEMSIScreen(Start: Boolean);
begin
  if Start then
    begin
      Save_User.X := 01;
      Save_User.Y := (LineCfg^.actScrnLen DIV 2);
      Save_SysOp.X := 01;
      Save_SysOp.Y := 01;

      SaveScreen(IemsiSave);
      FillArea(1, 1, mnuScrnWidth, LineCfg^.actScrnLen, #32, 07); { Clear the screen }
      FastWrite(1, (LineCfg^.actScrnLen) div 2,
                MakeAttr(GlobalCfg^.RaConfig^.WindFore, GlobalCfg^.RaConfig^.WindBack),
                Dup(#196, mnuScrnWidth));
      FastWrite(20, (LineCfg^.actScrnlen) div 2,
                MakeAttr(GlobalCfg^.RaConfig^.WindFore, GlobalCfg^.RaConfig^.WindBack),
                ' Local '+#25);
      FastWrite(50, (LineCfg^.actScrnLen) div 2,
                MakeAttr(GlobalCfg^.RaConfig^.WindFore, GlobalCfg^.RaConfig^.WindBack),
                ' Remote '+#24);
    end else RestoreScreen(IEMSISave);
end; { proc. StartIEMSIScreen }

var OrigRadu: Boolean;
    SaveDos : Byte;
    SaveOLM : Boolean;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logChat, 'BegingChatting');
  {$ENDIF}

  OrigRadu := LineCfg^.RaduCodes;
  LineCfg^.RaduCodes := true;
  SaveOLM := contrObj^.TimeInfo.InterNodeChecking;
  contrObj^.TimeInfo.InterNodeChecking := FALSE;
  SaveInactivity := LineCfg^.CheckInactivity;
  LineCfg^.CheckInactivity := false;
  LineCfg^.TimeFrozen := GlobalCfg^.RaConfig^.FreezeChat;
  lineCfg^.Exitinfo^.WantChat := False;
  {$IFDEF WINGUI}
    Form1.WantChatTrayIcon.Active := false;
  {$ENDIF}
  LastStr := '';
  StatusDisplay(11, false);
  lineCfg^.Exitinfo^.NumberPages := 00;
  LineCfg^.NowChatting := True;
  SaveDos := LineCfg^.DosShellMsgTyp;

  WriteLn; WriteLn; WriteLn;                        { Scroll down a few lines }
  If DisplayHotFile('STARTCHT', [])=#01                      { File not found }
   then WriteLn(LangObj^.ralget(ralStartCht));

  LogStr := '';

  If (GlobalCfg^.RaConfig^.ChatCommand<>'') AND (LineCfg^.LoggedON) then RaExec(GlobalCfg^.RaConfig^.ChatCommand, False, False,
       False,False,ExitCode, Success)
   else begin
          WriteLn;

          if lineCfg^.Exitinfo^.Baud=00 then WriteLn('`A14:', LangObj^.ralGet(ralNoUser1));
          if lineCfg^.Exitinfo^.Baud>00 then
            if GlobalCfg^.RaConfig^.AutoChatCapture then OpenLogFile;

          if lineCfg^.ExitInfo^.Baud>00 then
           begin
            UsingIEMSIChat := IEMSI_Chat(true);
            if UsingIEMSIChat then
             begin
               StartIemsiScreen(true);
               LineCfg^.DosShellMsgTyp := 01;
             end { if }
               else LineCfg^.DosShellMSgTyp := 00;

            repeat
              repeat
                CH := InputObj^.ReadKey;

                if (CH=#01) AND (LineCfg^.SysopKey) then
                 if ChatLogging then CloseLogFile else
                   OpenLogFile;

                if CH=#27 then
                 if NOT LineCfg^.SysOpKey then CH := #00;
              until ch in [#27, #08, #13, #32..#254];

              TempStr := CH;
              LogStr := LogStr + CH;

              if CH in AbortWrapSet then
                LastStr := ''
                 else LastStr := LastStr + CH;

              if OutputObj^.WhereX=79 then
               if CH <> #13 then
                if Length(LastStr) < 70 then
                 if NOT UsingIEMSIChat then
                  begin
                    Write('`X', OutputObj^.WhereX - Length(LastStr),':');
                    Write(Dup(#32, 80 - OutputObj^.WhereX));
                    WriteLn;
                    Delete(LastStr, Length(LastStr), 1);
                    Write(LastStr);
                  end; { if }

              HandleChatInput(CH, LineCfg^.SysOpKey, TempStr, LastStr);
            Until (CH in [#27]){ or (Exitinfo^.Baud=00)} OR (ProgTerminated);

            IEMSI_Chat(false);
            if UsingIEMSIChat then
              StartIemsiScreen(false);

           end; { if }

          WriteLn;
          WriteLn;
        End; { Else begin }

  MultiLnObj^.WriteUserOn('', uonBrowsing);

  LineCfg^.DosShellMsgTyp := SaveDos;
  CloseLogFile;
  ChatLogging := False;

  If DisplayHotFile('ENDCHT', [])=#01 then               { File not found }
   WriteLn('`A7:', LangObj^.ralGet(ralEndCht));

  InputObj^.PutInBuffer(#255, false);
  LineCfg^.NowChatting := False;
  LineCfg^.TimeFrozen := False;
  contrObj^.TimeInfo.InterNodeChecking := SaveOLM;
  LineCfg^.CheckInactivity := SaveInactivity;
  LineCfg^.RaduCodes := OrigRadu;

  {$ifdef With_Debug} DebugObj.DebugLog(logChat, 'Exit beginchatting');{$endif}
end; { proc. BeginChatting }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit Chat. }
