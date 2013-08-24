program EleMON;
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
** EleMON - Line (screen and sysops key overtaker) program for EleBBS.
**
** Copyright (c) 1996 - 2003 by Maarten Bekers
**
** Created : 10-Jul-1999
** Last update : 20-Feb-2003
**
**
*)

uses {$IFDEF ELEUNIX}
       Crt,
     {$ELSE}
       Crt,
     {$ENDIF}
      Global,
       CfgRec,
        ScrnU,
         Memman,
          RemScrn,
           Multi,
            ApTimer,
             Readcfg,
              WinTitle,
               GenFile,
                BbsKey,
                 Filemgr,
                  StrEdit,
                   GenCfg,
                    ListSys,
                     Area_Lst,
                      FileObj,
                       LongStr,
                        Editor,
                         Colors,
                          FileRout,
                           Debug_U,
                            ObjDec,
                             SysVars;


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

var RemScrnObj     : pRemoteScrnObj;
    Useron_F       : pFileObj;
    ScrnTimer      : EventTimer;               { Timer to update the screen }
    BlankTimer     : EventTimer;                      { "Screensaver" timer }
    DoShowStats    : Boolean;
    ServerName     : ShortString;
    DoShowStatusBar: Boolean;

    StatLine1,
    StatLine2      : ShortString;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowHeader;
begin
  New(LineCfg);
  New(GlobalCfg);
  InitBasicGlobalVars;
  InitBasicLineVars(LineCfg^);

  if NOT MemMan.AllocMem(GlobalCfg^.RaConfig, SizeOf(ConfigRecord), 'ConfigRecord', 'ShowHeader') then
    begin
      WriteLn;
      Writeln('Sorry you need at least ', SizeOf(ConfigRecord) div 1024, 'k more memory to run');
      Halt(255);
    end; { Not enough memory }

  if NOT MemMan.AllocMem(GlobalCfg^.ElConfig, SizeOf(EleConfigRecord), 'EleConfigRecord', 'ShowHeader') then
    begin
      WriteLn;
      Writeln('Sorry you need at least ', SizeOf(EleConfigRecord) div 1024, 'k more memory to run');
      Halt(255);
    end; { Not enough memory }

  ReadConfigRa;
  DirectScrnUpdate := false;
  CursorOff;

  TextAttr := LightGray;

  ScreenClear(mnuBackGrCol, mnuBackGrChar);         { Initialize the screen area }
  PartClear(1, 1, mnuScrnWidth, 1, LightGray, ' ');
  PartClear(1, 2, mnuScrnWidth, 2, mnuTopBar, mnuDblWide);
  PartClear(1, mnuScrnLen-1, mnuScrnWidth, mnuScrnLen-1, mnuTopBar, mnuSngWide);
  PartClear(1, mnuScrnLen, mnuScrnWidth, mnuScrnLen, LightGray, ' ');

  WriteAT(01, 01, Yellow, FullProgName + ' CLIENT MONITOR v'+VersionID);
  DirectScrnUpdate := true;
  WriteRight(mnuScrnWidth - 01, 01, Yellow, 'Copyright 1997-2003 Maarten Bekers');

  UpdateScreenBuffer(DirectScrnUpdate);
end; { proc. ShowHeader }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure InitBlankTimer;
begin
  NewTimer(BlankTimer, Secs2Tics(GlobalCfg^.RaConfig^.BlankSecs));
end; { proc. BlankTimer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowStatusBar;
begin
  WriteAT(01, mnuScrnlen -1, Makeattr(Black, LightGray), StatLine1);
  WriteAT(01, mnuScrnlen -0, Makeattr(Black, LightGray), StatLine2);
end; { proc. ShowStatusBar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


procedure MessageBox(Title, S: String; WaitSecs: Longint; Remove: Boolean);
var XLen    : Byte;
    XStart  : Byte;
    SaveScrn: Pointer;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'Mgr_MessageBox (S="'+S+'") (begin)');
  {$ENDIF}

  if Remove then SaveScreen(SaveScrn);

  if Length(S) > 75 then S[0] := #76;
  XLen := 02 + Length(S) + 02;
  XStart := (80 div 2) - (XLen div 2);

  ShadFillBoxTitle(XStart, 10, (XStart + Xlen) - 01, 14, Makeattr(LightBlue, Blue), mnuStyle, True, Title);
  WriteAT(XStart + 02, 12, Makeattr(Lightgray, Blue), S);

  if WaitSecs = 0 then
    begin
      Delay(500);
      GetInput;
    end
      else KeyDelay(WaitSecs, true);

  if Remove then RestoreScreen(SaveScrn);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'Mgr_MessageBox (S="'+S+'") ( end )');
  {$ENDIF}
end; { proc. MessageBox }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ProcessKey(var CH:Char);
begin
 { ALT-Z } If Ch=#44 then DoDosShell('Type "EXIT" to return to '+FullProgName+ ' Manager.');
 { ALT-J } if CH=#36 then DoDosShell('Type "EXIT" to return to '+FullProgName+ ' Manager.')
end; { proc. Processkey }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowStatistics;
var SysInfo       : SysInfoRecord;
    LastcallSize  : Longint;
begin
  ReadSysInfoBBS(SysInfo);
  LastCallSize := GetFileSize(LastcallFileName, SizeOf(Lastcallrecord));

  ShadFillBoxTitle(03, 18, 78, 22, mnuBoxColor, mnustyle, true, ' Total ');

  WriteAT(05, 19, mnuNormColor, 'Total calls   : ' + Fstr(SysInfo.TotalCalls));
  WriteAT(05, 20, mnuNormColor, 'Last caller   : ' + SysInfo.LastCaller);
  WriteAT(05, 21, mnuNormColor, 'Todays calls  : ' + Fstr(LastcallSize));
end; { proc. ShowStatistics }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure UnknownCode(const Code   : byte;
                      var   TmpBuf : Array of Char;
                      const Length : Byte;
                      var   Counter: Longint);

type WordRec = record
                 Lo : Byte;
                 Hi : Byte;
               end; { WordRec }

var ScrnSize : SmallWord;
    Thisline : ScreenLineType;
    PwdStr   : String;
begin
  Case Code of
    pipe_cntrl_ScrnUpdate  : begin
                               WordRec(ScrnSize).Hi := Ord(TmpBuf[Counter + 1]);
                               WordRec(ScrnSize).Lo := Ord(TmpBuf[Counter + 2]);

                               RemScrnObj^.rem_RecvScreen(ScrnU.ScrPtr^, ScrnSize);
                               UpdateScreenBuffer(true);
                             end; { if }
    pipe_cntrl_SendLine    : begin
                               WordRec(ScrnSize).Hi := Ord(TmpBuf[Counter + 1]);
                               WordRec(ScrnSize).Lo := Ord(TmpBuf[Counter + 2]);

                               RemScrnObj^.rem_RecvLine(ThisLine, ScrnSize);
                               WriteScrnLine(01, Ord(TmpBuf[Counter + 3]), ScrnSize div 2, ThisLine);
                             end; { if }
    pipe_cntrl_IsStatus    : begin
                               RemScrnObj^.rem_RecvStatusBar(StatLine1, StatLine2, Ord(TmpBuf[Counter + 2]), Ord(TmpBuf[Counter + 3]));

                               if DoShowStatusBar then
                                 ShowStatusBar;
                             end; { if }
    pipe_cntrl_SetCursor   : begin
                               GotoXY(Ord(TmpBuf[Counter + 1]),
                                       Ord(TmpBuf[Counter + 2]));
                               TextAttr := Ord(TmpBuf[Counter + 3]);
                             end; { SetCursor }
    pipe_cntrl_UpdateDone  :  begin
                               UpdateScreenBuffer(true);
                             end; { UpdateDone }
    pipe_cntrl_GivePassword: begin
                               PwdStr := '';
                               Mgr_EditBox(PwdStr, 'Password: ', '', 12, 0, 12, '', false, false, true);
                               RemScrnObj^.rem_SendPassword(PwdStr);
                               RemScrnObj^.rem_RequestScrnUpdate;
                             end; { Password }
    pipe_cntrl_PlayBeep    : begin
                               Write(^G);
                             end; { PlayBeep }
    pipe_cntrl_NAK         :  ;
  end; { case }
end; { proc. UnknownCode }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure IdleProc;
var BlankScrn: Pointer;
begin
  if GlobalCfg^.RaConfig^.BlankSecs > 0 then
    if TimerExpired(BlankTimer) then
      begin
        {-- Savescreen --------------------------------------------------}
        SaveScreen(BlankScrn);

        {-- Clear the screen --------------------------------------------}
        Partclear(01, 01, mnuScrnWidth, mnuScrnLen, Cyan, #32);

        {-- Idle --------------------------------------------------------}
        REPEAT
          DoSlice;
        UNTIL KeyPressed;

        {-- reset blanking timer ----------------------------------------}
        InitBlankTimer;

        {-- restore screen ----------------------------------------------}
        RestoreScreen(BlankScrn);
      end; { if }

  if (NOT DoShowStats) then EXIT;

  if TimerExpired(ScrnTimer) then
    begin
      if NOT ExtraWaiting then
        begin
          ExtraWaiting := true;
          ExtraKey := #254;

          ShowStatistics;
          NewTimer(ScrnTimer, Secs2Tics(3));
        end; { if }
    end; { if }
end; { proc. IdleProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SendMsgToNode(NodeNr: Longint);
var EditX   : Longint;
    EditY   : Longint;
    SaveScrn: Pointer;
    Counter : Longint;
    MsgFile : pFileObj;
    MsgPath : String;
begin
  EditX := 01;
  EditY := 01;

  WindowColor := MakeAttr(LightBlue, blue);
  NormColor := MakeAttr(Lightgray, Blue);

  SaveScreen(SaveScrn);

  DirectScrnUpdate := true;
  if NOT Editor_InitEditor(02, 02, mnuScrnWidth - 1, mnuScrnLen - 1, true) then
    begin
      RestoreScreen(SaveScrn);
      EXIT;
    end; { if }

  Editor_Redraw;
  Editor_Edit(EditX, EditY);

  DirectScrnUpdate := true;

  if Editor_Modified then
    if DoSaveChanges('Send this message (Y/n)? °', true, false) then
     begin
       New(MsgFile, Init);

       if GlobalCfg^.RaConfig^.SemPath <> '' then MsgPath := GlobalCfg^.RaConfig^.SemPath
         else MsgPath := GlobalCfg^.RaConfig^.SysPath;

       MsgFile^.Assign(MsgPath + 'node'+ FStr(NodeNr)+'.ra');
       MsgFile^.FileMode := ReadWriteMode + Denynone;
       if MsgFile^.OpenOrCreate(1) then
         begin
           for Counter := 01 to LastString do
             MsgFile^.WriteLn(Txt^[Counter]);

           MsgFile^.WriteLn('');
           MsgFile^.WriteLn(^K + ']258' + ^A);
         end; { if }

       Dispose(MsgFile, Done);
     end; { if }

  Editor_DoneEditor(EditX, EditY);
  RestoreScreen(SaveScrn);
end; { proc. SendMsgToNode }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoShowSent;
begin
  ShadFillBoxTitle(58, 03, 78, 06, mnuBoxColor, mnustyle, true, ' Statistics ');

  WriteAT(60, 4, mnuNormColor, 'TxBytes: ' + FStr(RemScrnObj^.BytesSent));
  WriteAT(60, 5, mnuNormColor, 'RxBytes: ' + FStr(RemScrnObj^.BytesRead));
end; { proc. DoShowSent }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TakeOverScreen(NodeNr: Longint): Boolean;
var Connected      : Boolean;
    InputCH        : Char;
    ShowSent       : Boolean;
begin
  TakeOverScreen := false;
  DoShowstats := false;
  DoShowStatusBar := TRUE;
  ShowSent := false;

  Connected := RemScrnObj^.ConnectPipe(ServerName, NodeNr);
  if NOT Connected then
    begin
      DoShowStats := true;
      EXIT;
    end; { if }

  RemScrnObj^.UnknownCode := {$IFDEF FPC}@{$ENDIF}UnknownCode;
  TakeOverScreen := true;

  InputCH := #13;
  RemScrnObj^.rem_RequestScrnUpdate;
  RemScrnObj^.rem_AskStatusBar(1);

  REPEAT
    if KeyPressed then
      begin
        InputCH := ReadKey;

        if InputCH = #00 then
          begin
            InputCH := ReadKey;

            Case InputCH of
              { Up ..  } #75, #77, #72, #80,
              { End }    #82, #83, #71, #79   : begin
                                                  RemScrnObj^.rem_SendKeyPress(#00);
                                                  RemScrnObj^.rem_SendKeyPress(InputCH);
                                                end; { if }

              { ALT-1 } #120 : ShowSent := NOT ShowSent;
              { ALT-J } #36  : begin
                                 { ALT-J is disabled }
                               end; { ALT-J }
              { F1..F10 }
                   #59..#68  : begin
                                 RemScrnObj^.rem_AskStatusBar(Ord(InputCH) - 58);
                               end; { F1 .. F10 }
               else RemScrnObj^.rem_SendSysCommand(InputCH);
            end; { case }
          end
            else RemScrnObj^.rem_SendKeyPress(InputCH);
      end; { if }

    if ShowSent then
      DoShowSent;

    if InputCH = #27 then
      begin
        if NOT DoSaveChanges('Terminate session? (Y/n)? °', true, false) then
          InputCH := #00;
      end; { if }

    RemScrnObj^.rem_GetInputBuffer;

    DoSlice;
  UNTIL (InputCH = #27) OR (NOT RemScrnObj^.Connected);

  DoShowstats := true;

  {-- Now disconnect the remotepipe ----------------------------------------}
  RemScrnObj^.PipeHandle.Disconnect;
end; { func. TakeOverScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TakeNodeDown(NodeNr: Longint);
var MsgFile: pFileObj;
    MsgPath: String;
    EditStr: String;
begin
  if GlobalCfg^.RaConfig^.SemPath <> '' then MsgPath := GlobalCfg^.RaConfig^.SemPath
    else MsgPath := GlobalCfg^.RaConfig^.SysPath;

  EditStr := '0';
  if Mgr_EditBox(EditStr, 'Errorlevel: ', '', 3, 3, 3, '', false, true, false) = #27 then EXIT;

  New(MsgFile, Init);

  MsgFile^.Assign(MsgPath + 'raxit'+FStr(NodeNr) + '.' + EditStr);
  MsgFile^.Create(1);

  Dispose(MsgFile, Done);

  MessageBox(' NOTICE ', 'Force down command sent ', 1500, true);
end; { proc. TakeNodeDown }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SpecifyPort;
var EditStr: String;
begin
  EditStr := FStr(RemScrnObj^.remStartPort);

  Mgr_EditBox(EditStr, 'TCP/IP start port: ', '', 5, 5, 5, '', false, true, false);

  RemScrnObj^.remStartPort := Fval(EditStr);
end; { proc. SpecifyPort }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SpecifyServer;
var EditStr: String;
begin
  Mgr_EditBox(ServerName, 'TCP/IP server: ', '', 45, 5, 45, '', false, false, false);
end; { proc. SpecifyServer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Useron_GetInfo(var Info1, Info2, Info3, Info4, Info5, Info6, Info7, Info8, Info9: List_StringType;
                        ItemNr: Longint; IsHiLight: Longint);
var UseronInf: UseronRecord;
    LineNr   : Longint;
begin
  Useron_F^.Seek(Pred(ItemNr) * SizeOf(UseronRecord));         { FileSize checking is done bij 'DoList' }
  Useron_F^.BlkRead(UseronInf, SizeOf(UseronRecord));

  if UserOnInf.Line <> 0 then
    LineNr := UserOnInf.Line
      else LineNr := UserOnInf.Nodenumber;

  if (UseronInf.Name = '') OR (LineNr = 0) then
    begin
      Info1 := FStr(ItemNr);
      Info2 := '(Inactive)';
      Info3 := '';
      Info4 := '';
      Info5 := '';

      EXIT;
    end; { if }

  Info1 := FStr(LineNr);
  Info2 := UseronInf.Name;
  Info3 := UseronInf.City;
  Info4 := FStr(UseronInf.NoCalls);
  Info5 := UseronInf.StatDesc;

  Case UseronInf.Status of
    0 : Info5 := 'Browsing';
    1 : Info5 := 'Up/downloading';
    2 : Info5 := 'Read/Writing msgs';
    3 : Info5 := 'Door';
    4 : Info5 := 'Chatting';
    5 : Info5 := 'Questionnaire';
    6 : Info5 := 'IRC';
    7 : Info5 := 'Newuser logon';
  end; { case }
end; { proc. Useron_GetInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Useron_Activate(HiLight: Longint; HiBarPos: Longint): Longint;
begin
  Useron_Activate := -1;

  {-- reset "screensaver" timer --------------------------------------------}
  InitBlankTimer;
end; { func. Useron_Activate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Useron_AbortCheck(CH: Char): Boolean;
begin
  if (Ch in [#27]) then Useron_AbortCheck := true
   else Useron_AbortCheck := false;
end; { func. Useron_AbortCheck }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Useron_Seek(S: String): Longint;
begin
  Useron_Seek := -1;

  {-- reset "screensaver" timer --------------------------------------------}
  InitBlankTimer;
end; { func. Useron_Seek }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Useron_KeyPress(CH: Char; HiLight: LongInt; var HiBarPos, TopOfScrn: Longint;
                         var StartRange, EndRange: LongInt): LongInt;
var SaveScrn: Pointer;
begin
  {-- reset "screensaver" timer --------------------------------------------}
  InitBlankTimer;

  Useron_Keypress := -1;

  Case CH of
     { ALT-T } #20 : begin
                       SaveScreen(SaveScrn);
                       PartClear(1, 1, mnuScrnWidth, mnuScrnLen, mnuNormColor, #32);
                       MessageBox(' NOTICE ', 'Contacting node '+FStr(HiLight), 1, false);

                       if NOT TakeOverScreen(HiLight) then
                         begin
                           MessageBox(' ERROR ', 'Unable to contact node '+FStr(HiLight), 0, true);
                         end; { TakeOver }

                       RestoreScreen(SaveScrn);
                     end; { AlT-T }
     { ALT-P } #25 : begin
                       SpecifyPort;
                     end; { ALT-P }
     { ALT-S } #31 : begin
                       SpecifyServer;
                     end; { ALT-S }
     { ALT-D } #32 : begin
                       TakeNodeDown(HiLight);
                     end; { ALT-D }
     { ALT-M } #50 : begin
                       SendMsgToNode(HiLight);
                     end; { ALT-M }

  end; { case }

  {-- reset "screensaver" timer --------------------------------------------}
  InitBlankTimer;
end; { func. Useron_Keypress }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Useron_GetItems: Longint;
begin
  Useron_GetItems := Useron_F^.FileSize DIV SizeOf(UseronRecord);
end; { func. Useron_GetItems }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Useron_CreateNewFile: Boolean;
begin
  Useron_CreateNewFile := false;
end; { func. Useron_CreateNewFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowCurrentNodes;
var SaveScrn: Pointer;
begin
  CursorOff;
  SaveScreen(SaveScrn);

  New(Useron_F, Init);
  Useron_F^.Assign(GlobalCfg^.RaConfig^.Syspath + 'useron.bbs');
  Useron_F^.FileMode := ReadWriteMode + DenyNone;
  Useron_F^.Open(01);


  ShowStatistics;
  DoList(true,
         04,
         5, 29, 18, 6, 15, 0, 0, 0, 0,
         03, 04, 78, 16,
         ' Nodes available ',
         'ALT: T - Attach  M - Send message  D - Shut down  P - Specify port  S - Server',
         {$IFDEF FPC}@{$ENDIF}Useron_GetInfo,
         {$IFDEF FPC}@{$ENDIF}Useron_Activate,
         {$IFDEF FPC}@{$ENDIF}Useron_Seek,
         {$IFDEF FPC}@{$ENDIF}Useron_AbortCheck,
         {$IFDEF FPC}@{$ENDIF}Useron_Keypress,
         {$IFDEF FPC}@{$ENDIF}Useron_GetItems,
         {$IFDEF FPC}@{$ENDIF}Useron_CreateNewFile,
         {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
         false,
         01, 01,
         true, false);

  Dispose(Useron_F, Done);

  RestoreScreen(SaveScrn);
  CursorOn;
end; { proc. Useron_List }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

begin
  SetWindowTitle(FullProgName + ' CLIENT MONITOR');
  RunningBBS := false;

  SetupScrnU;
  ShowHeader;
  InitSystemNames;
  CheckRegistered;
  KeyProc := {$IFDEF FPC}@{$ENDIF}ProcessKey;
  StrEdit.IdleProc := {$IFDEF FPC}@{$ENDIF}IdleProc;
  NewTimer(ScrnTimer, Secs2Tics(3));
  InitBlankTimer;
  New(RemScrnObj, Init);

  if NOT AskForPassword(GlobalCfg^.Raconfig^.KeyBoardPwd) then
   begin
     TextAttr := 07;
     CursorOn;
     ReleaseMem(GlobalCfg^.RaConfig, SizeOf(ConfigRecord));
     Partclear(01, 01, mnuScrnWidth, mnuScrnLen, White, #32);
     EXIT;
   end; { wrong password }

  DoShowStats := true;
  ServerName := 'localhost';
  ShowCurrentNodes;
  Dispose(RemScrnobj, Done);

  TextAttr := 07;
  CursorOn;
  Partclear(01, 01, mnuScrnWidth, mnuScrnLen, White, #32);
end. { progam ELEMON }

