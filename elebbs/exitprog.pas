unit ExitProg;
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
** ExitProg (Hangup etc) routines for EleBBS
**
** Copyright (c) 1996, 1997, 1998 by Maarten Bekers
**
** Created : 11-Apr-1998
** Last update : 11-Apr-1998
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

procedure DoExitProcedures;
procedure HangUp;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Debug_U,
     Global,
     Colors,
     LongStr,
     {$IFDEF VirtualPascal}
       VpSysLow,
     {$ENDIF}

     Aptimer,
     elx_Bbs,
     StrPath,
     ObjDec,

     {$IFDEF WIN32}
       Support,
      {$IFDEF WINGUI}
         Win_Main,
         Forms,
         Windows,
      {$ENDIF}

      {$IFNDEF WINGUI}
       Crt,
       Wfc,
      {$ENDIF}
     {$ENDIF}

     {$IFDEF MSDOS}
       Crt,
       Wfc,
     {$ENDIF}

     {$IFDEF GO32V2}
       Crt,
       Wfc,
     {$ENDIF}

     {$IFDEF OS2}
       Crt,
       Wfc,
     {$ENDIF}

     {$IFDEF ELEUNIX}
       Crt,
       Wfc,
     {$ENDIF}

     ComUnit, ElLog_U, FileRout, FastScrn, OutBlock, Input_U, Multi,
     CfgRec, JDates, Cases, Cfgfile, MultiLn, Limit_U, Genfile,
     BitWise, User_u, ApFossil, FileObj;

procedure HangUp;
var SysopNextTimer: EventTimer;
    CH            : Char;
    CarrierTimer  : EventTimer;
begin
  if RunHangupProc then EXIT;
  RunHangupProc := TRUE;
  ProgTerminated := true;


  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logSupport, 'Terminate call (hangup)');
  {$ENDIF}


{$IFDEF WITH_FULL}
  if lineCfg^.Exitinfo^.Baud>00 then
   if ComObj <> NIL then
    if NOT ComObj^.Com_Carrier then
       begin
         RaLog('>', 'Lost carrier');
       end; { if }
{$ENDIF}

  RaLog('!', 'User off-line');

  if (LineCfg^.GuestUser) AND (lineCfg^.Exitinfo <> nil) then
   With lineCfg^.Exitinfo^.Userinfo, lineCfg^  do
      begin
        Comment := SaveGuestRecord^.Comment;
        Attribute := SaveGuestRecord^.Attribute;
        Attribute2 := SaveGuestRecord^.Attribute2;
        Flags := SaveGuestRecord^.Flags;
        Credit := SaveGuestRecord^.Credit;
        Pending := SaveGuestRecord^.Pending;
        Security := SaveGuestRecord^.Security;
        CombinedInfo := SaveGuestRecord^.CombinedInfo;
        LineCfg^.Exitinfo^.Userinfo.Language := SaveGuestRecord^.Language;
        DateFormat := SaveGuestRecord^.DateFormat;
        ForwardTo := SaveGuestRecord^.ForwardTo;
        MsgArea := SaveGuestRecord^.MsgArea;
        FileArea := SaveGuestRecord^.FileArea;
        DefaultProtocol := SaveGuestRecord^.DefaultProtocol;
        FileGroup := SaveGuestRecord^.FileGroup;
        Sex := SaveGuestRecord^.Sex;
        MsgGroup := SaveGuestRecord^.MsgGroup;
        Attribute3 := SaveGuestRecord^.Attribute3;
        PassWord := SaveGuestRecord^.PassWord;
      end; { if }

  UpdateUserRecord;                                 { Update user information }

  if GlobalCfg^.RaConfig <> nil then
    begin
      BoxWindow(30, 12, 50, 16, GlobalCfg^.RaConfig^.BorderFore, GlobalCfg^.RaConfig^.BorderBack, '');
      FastWrite(33, 14, MakeAttr(GlobalCfg^.RaConfig^.WindFore, GlobalCfg^.RaConfig^.WindBack), 'Terminating call');
    end; { if }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logSupport, 'Mike debugging part 1 of 500');
  {$ENDIF}

  {$IFDEF WITH_FULL}
    OutBlockObj^.DumpBlock;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logSupport, 'Mike debugging part 2 of 500');
  {$ENDIF}
    if ComObj <> NIL then
      {$IFDEF FPC}
        ComObj^.Com_FlushOutBuffer(@DoSlice);
      {$ELSE}
        ComObj^.Com_FlushOutBuffer(DoSlice);
      {$ENDIF}
  {$ENDIF}

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logSupport, 'Mike debugging part 3 of 500');
  {$ENDIF}

  if lineCfg^.Exitinfo <> nil then
   begin
   If lineCfg^.Exitinfo^.SysOpNext then
    begin;
     BoxWindow(30, 12, 50, 16, GlobalCfg^.RaConfig^.BorderFore, GlobalCfg^.RaConfig^.BorderBack, '');
     FastWrite(32, 14, MakeAttr(GlobalCfg^.RaConfig^.WindFore, GlobalCfg^.RaConfig^.WindBack), 'SYSOP: Press (CR)');
     LineCfg^.LocalOnly := True;

     NewTimer(SysopNextTimer, Secs2Tics(120));
     Repeat;
     {$IFNDEF WINGUI}
       If Crt.KeyPressed then CH := Crt.ReadKey;
     {$ELSE}
       If Form1.COlorConsole1.KeyPressed then CH := Form1.ColorConsole1.ReadKey;
     {$ENDIF}
       {$IFNDEF WIN32}
        {$IFNDEF OS2}
          Crt.Sound(800); Crt.Delay(150);
          Crt.Sound(600); Crt.Delay(100);
          Crt.Sound(800); Crt.Delay(150);
          Crt.Sound(600); Crt.Delay(100);
          Crt.Nosound;
        {$ELSE}
          PlaySound(800, 150);
          Playsound(600, 100);
          PlaySound(800, 150);
          Playsound(600, 100);
        {$ENDIF}
       {$ENDIF}
     Until (CH=#13) OR (TimerExpired(SysopNextTimer));

     LineCfg^.LocalOnly := False;
    end; { SysOpNext }
   end; { if }


{$IFDEF WITH_FULL}

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logSupport, 'Mike debugging part 4 of 500');
  {$ENDIF}

{$IFNDEF ELEUNIX}
{$IFNDEF WINGUI}
 if ComObj <> NIL then
  if lineCfg^.Exitinfo^.Baud <> 00 then
   begin
      NewTimer(CarrierTimer, Secs2Tics(4));

      ComObj^.Com_SetDtr(False);
      if ComObj^.Com_Carrier then Crt.Delay(100);
      ComObj^.Com_SetDtr(True);
      if ComObj^.Com_Carrier then Crt.Delay(100);
      if ComObj^.Com_Carrier then Crt.Delay(100);

      While (NOT (TimerExpired(CarrierTimer))) AND (ComObj^.Com_Carrier) do
        begin
          ComObj^.Com_SetDtr(false);

          DoSlice;

          ComObj^.Com_SetDtr(true);
        end; { while }

      if ComObj^.Com_Carrier then ComObj^.Com_SetDtr(true);
      if ComObj^.Com_Carrier then Crt.Delay(50);
      if ComObj^.Com_Carrier then ComObj^.Com_SetDtr(false);
      if ComObj^.Com_Carrier then Crt.Delay(50);
      ComObj^.Com_SetDtr(false);

      if lineCfg^.Modem^.OffHook then
        WfcObj^.SendModem(lineCfg^.Modem^.BusyStr);
   end; { if }
{$ENDIF}
{$ENDIF}
  {$ENDIF}

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logSupport, 'Mike debugging part 5 of 500');
  {$ENDIF}

 {$ifndef WinGui}
   System.Halt(DefExitCode);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logSupport, 'Mike debugging part 6 of 500');
  {$ENDIF}

   RunHangupProc := TRUE;
   ProgTerminated := TRUE;
 {$Else}
   DoExitProcedures;
   Application.Terminate;
   ExitCode := DefExitCode;

   repeat;
     Application.HandleMessage;
     Postquitmessage(00);
   until (Application.Terminated);
   RunHangupProc := TRUE;
   ProgTerminated := TRUE;
 {$endif}
end; { hangup }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoExitProcedures;
var OldFMode: Longint;
    Event_F : pFileObj;
    elxObj  : pElxBbsObj;
begin
 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logSupport, 'DoExitProcedures');
 {$ENDIF}
 If NOT RunningBBS then EXIT;
 If lineCfg^.Exitinfo=nil then EXIT;

  if (TimeStr(false, false) = lineCfg^.Exitinfo^.EventInfo.StartTime) then
    EventStarted := True;

  UpdateStatistics;
  if EventStarted then
   if (EventRecNr <> 0) then
    if (Supcase(lineCfg^.Exitinfo^.EventInfo.StartTime) <> 'NONE') then
      begin
        if NOT CmdEventSet then
          DefExitCode := lineCfg^.Exitinfo^.EventInfo.ErrorLevel;

        WriteLn(SystemMsgPrefix, 'Executing event; exiting at errorlevel ',DefExitCode);
        lineCfg^.Exitinfo^.EventInfo.LastTimeRun := DateStr;

        if NOT CmdEventSet then
         if Config_OpenFile(Event_F, EventsFileName, 01, WriteMode+DenyNone, True, False)=00 then
           begin
             Config_SeekFile(Event_F, ((EventRecNr-1) * SizeOf(EventRecord)));
             Config_WriteFile(Event_F, lineCfg^.Exitinfo^.EventInfo, SizeOf(EventRecord));
             Config_DoneFile(Event_F);
           end; { if }
      end; { Start Event }

  if (lineCfg^.Exitinfo^.Userinfo.name = '') then EXIT;
  if InExitProcedure then EXIT;
  if RunExitProc then EXIT;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logSupport, 'DoExitProcedures do finish (no test proves it already is running)');
  {$ENDIF}

  InExitProcedure := True;
  RunExitProc := true;

  {$IFDEF WITH_FULL}
    MultiLnObj^.KillFromUserOn;
  {$ENDIF}

  if LineCfg^.LoggedOn then
   begin
     lineCfg^.Exitinfo^.Userinfo.LastDate := lineCfg^.Exitinfo^.LoginDate;
     lineCfg^.Exitinfo^.Userinfo.LastTime := lineCfg^.Exitinfo^.LoginTime;

     if Longint(Longint(lineCfg^.Exitinfo^.Userinfo.Elapsed) - Longint(LineCfg^.EventDeducted)) < -32000 then
       LineCfg^.EventDeducted := 00;
     Dec(lineCfg^.Exitinfo^.Userinfo.Elapsed, LineCfg^.EventDeducted);
   end; { if }

  if LineCfg^.LoggedOn then ChangeTime(00, false);
  if LineCfg^.LoggedOn then UpdateUserRecord;

  EraseFile('exitinfo.bbs');

  New(elxObj, Init);
  if NOT elxObj^.RunElexerScript('.' + BsChar + 'exitbbs', '', false) then
    begin
      { unable to run exitbbs }
    end; { if }
  Dispose(elxObj, Done);

  InExitProcedure := False;
end; { proc. DoExitprocedures }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
