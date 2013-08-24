unit Control;
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
** Control (timelimit, inactivity, CD) Routines for EleBBS
**
** Copyright (c) 1996-2001 by Maarten Bekers
**
** Created : 15-Nov-1997
** Last update : 13-Oct-2001
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF WITH_FULL}
  This unit should not be compiled for an utility program
{$ENDIF}

uses ApTimer;

Type UserInfoType = Record
       StartTime        : LongInt;
       CurrLines        : Word;
       StopMore         : Boolean;
       TwoMinWarn       : Boolean;
       OneMinWarn       : Boolean;
       TimeOverWarn     : Boolean;
       IdleLimit        : LongInt;
       IdleWarn         : Boolean;
       IdleTime         : Word;
       InterNodeTime    : Longint;
       InterNodeChecking: Boolean;
       SecondsTimer     : Longint;
       Charcounter      : Word;
       KeyBuffer        : Array[1..250] of record
                                             TempC   : Char;
                                             IsSysOp : Boolean;
                                             Emulated: Boolean;
                                           end; { if }
       KeyStart         : Word;
     end; { UserinfoRecord }


type tControlObj = Object
         InCheckAll     : Boolean;       { default: false }
         ControlTimer   : EventTimer;
         ScrnUpdateTimer: EventTimer;
         TimeInfo       : UserinfoType;

         {-- Constructors and destructors ----------------------------------}
         constructor Init;
         destructor Done;

         {-- Generic routines ----------------------------------------------}
         function  CheckAll: Boolean;
         function  TimeRemain: Longint;
         function  CheckEventStatus: Boolean;
         function  CheckRaExit: Boolean;
         function  CheckIdle: Boolean;

         procedure TimeWarn;
         procedure CheckForEleMon;
         procedure TimeOver;
         procedure SetTimeOut;
         procedure SetIdleTimeLimit(T: Word);
         procedure SetStartTime;

         {-- Private routines ----------------------------------------------}
         private
     end; { tControlobj }

type
  pControlObj = ^tControlObj;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses DOS, Multi, Global, DispAns, GenDos, StatusB, ObjDec, Input_U,
     RAL, ElLog_U, Support, StrPath, JDates, ExitProg,
     CurTime, FileRout, ApFossil, FileObj, RemScrn, longStr,
     CfgRec;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tControlObj.Init;
begin
  InCheckAll := FALSE;
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tControlObj.Done;
begin
end; { Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tControlObj.CheckRaExit: Boolean;
var MsgPath  : String;
    Temp     : String;
    TempNr   : Word;
    Code     : NumreadType;
    Temp_F   : pFileObj;
    DirInfo  : SearchRec;
begin
  CheckRaExit := true;

   If GlobalCfg^.RaConfig^.SemPath <> '' then MsgPath := GlobalCfg^.RaConfig^.SemPath
     else MsgPath := GlobalCfg^.RaConfig^.SysPath;

  FindFirst(MsgPath + 'RAXIT*.*', Anyfile - VolumeID, DirInfo);

  While (DosError=0) do
    begin
       if Dirinfo.Attr AND Directory = 0 then
        begin
          Temp := Copy(Dirinfo.Name, Length('RAXIT') + 01, Pos('.', Dirinfo.Name) - Succ(Length('RAXIT')));
          Val(Temp, TempNr, Code);

          if TempNr = LineCfg^.RaNodeNr then
            begin
              New(Temp_F, Init);

              Temp_F^.Assign(MsgPath + Dirinfo.Name);
              Temp_F^.FileMode := ReadWriteMode + DenyNone;
              Temp_F^.Erase;

              Dispose(Temp_F, Done);

              TempNr := FVal(JustExtension(Dirinfo.Name));

              LineCfg^.ForcedExit := true;
              defExitCode := TempNr;
              Halt(TempNr);
            end; { if }
        end; { if }

      FindNext(DirInfo);
    end; { While DosError=0 }

  FindClose(Dirinfo);
end; { func. CheckRaExit }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tControlObj.CheckForEleMon;
begin
  if LineCfg^.DoMonitor then
   if TimerExpired(ScrnUpdateTimer) then
     begin
       if GlobalCfg^.ElConfig^.AttachPassword = '' then
         begin
           Ralog('!', 'EleMON server module disabled, no password set');
           LineCfg^.DoMonitor := false;
         end; { if }

       NewTimer(ScrnUpdateTimer, Round((0.5 * TicsFreq)) div SecsFreq);

       if LineCfg^.DoMonitor then
        if RemScrnObj <> nil then
         begin
           if NOT RemScrnObj^.Connected then
             begin
               RemScrnObj^.ListenPipe;

               if RemScrnObj^.Connected then
                 begin
                   RemScrnObj^.InitVars;
                   RemScrnObj^.PwdWeWant := GlobalCfg^.ElConfig^.AttachPassword;
                   RemScrnObj^.rem_AskForPassword;
                 end; { if }
             end
                else RemScrnObj^.rem_GetInputBuffer;
         end; { if }
     end; { if }
end; { proc. CheckForEleMon }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tControlObj.CheckAll: Boolean;
var TempTime             : LongInt;
    SaveInterNodeChecking: Boolean;
    MsgPath              : String;
    RaNodeStr            : String;
    Temp_F               : pFileObj;

{$IFNDEF FPC}
{$IFNDEF WIN32}
 {$IFNDEF OS2}
    Result               : Boolean;
 {$ENDIF}
{$ENDIF}
{$ENDIF}
    Temp                 : Longint;
begin
  if ProgTerminated then
     begin
      CheckAll := false;
      EXIT;
    end; { Terminated }

  Result := true;
  CheckAll := true;

  {$IFDEF WITH_FULL}
    if LineCfg^.Exitinfo^.Baud <> 00 then
     if ComObj <> Nil then
      if NOT ComObj^.Com_Carrier then Result := FALSE;
    CheckAll := Result;
    ProgTerminated := NOT Result;
  {$ENDIF}
  if (InCheckAll) OR (NOT Result) then EXIT;

  if NOT TimerExpired(ControlTimer) then EXIT;
  NewTimer(ControlTimer, Round((0.5 * TicsFreq)) div SecsFreq);       { Each second check for carrier }

  Inc(OutputObj^.CheckAllCounter);

  CheckForEleMon;

{$IFDEF WITH_FULL}
  if NOT CheckIdle then Result := false;

  TempTime := TimeRemain;
  If TempTime>00 then TimeWarn
    else begin
           TimeOver;
         end; { if }

   InCheckAll := TRUE;

   if Progterminated then Result := false;
   if NOT CheckRaExit then Result := false;

   If ((TimeInfo.InterNodeTime + uonWaitTime) <= CurrentTime) then
    begin
      TimeInfo.InterNodeTime := CurrentTime;
      ReadSysInfoBBS(LineCfg^.Exitinfo^.Sysinfo);

      if TimeInfo.InterNodeChecking then
       begin
         if GlobalCfg^.RaConfig^.SemPath <> '' then MsgPath := GlobalCfg^.RaConfig^.SemPath
           else MsgPath := GlobalCfg^.RaConfig^.SysPath;

         Str(LineCfg^.RaNodeNr, RaNodeStr);

         New(Temp_F, Init);
         Temp_F^.Assign(MsgPath + 'NODE' + RaNodeStr + '.RA');
         Temp_F^.FileMode := ReadMode + DenyNone;
         if Temp_F^.Open(1) then
           begin
             if Temp_F^.FileSize > 00 then
              begin
                Dispose(Temp_F, Done);
                Temp_F := nil;

                SaveInterNodeChecking := TimeInfo.InterNodeChecking;
                TimeInfo.InterNodeChecking := False;

                DisplayHotFile(MsgPath+'NODE'+RaNodeStr+'.RA', []);

                {----------------- Erase the NODE<X>.RA file ------------------}
                New(Temp_F, Init);
                Temp_F^.Assign(MsgPath + 'NODE' + RaNodeStr + '.RA');
                Temp_F^.FileMode := ReadMode + DenyNone;

                if NOT Temp_F^.Erase then
                  Temp_F^.Create(1);

                Dispose(Temp_F, Done);
                Temp_F := nil;
                {--------------------------------------------------------------}


                TimeInfo.InterNodeChecking := SaveInterNodeChecking;
              end; { InterNode message checking }
           end; { if }

         if Temp_F <> nil then Dispose(Temp_F, Done);
         Temp_F := nil;
       end; { if }
    end; { if }

  if NOT Result then HangUp;

  InCheckAll := FALSE;
  CheckAll := RESULT;
{$ENDIF}

 ProgTerminated := NOT Result;
 CheckAll := Result;
end; { func. CheckAll }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tControlObj.CheckEventStatus: Boolean;
var Hours    : Word;
    Minutes  : Word;
    Secs     : Word;
    MSecs    : Word;
    HoursStr : String;
    MinutStr : String;
begin
  GetTime(Hours, Minutes, Secs, MSecs);
  Str(Hours, HoursStr);
  Str(Minutes, MinutStr);

  if length(HoursStr) < 2 then HoursStr := '0' + HoursStr;
  if length(MinutStr) < 2 then MinutStr := '0' + MinutStr;

  CheckEventStatus := false;

  if HoursStr + ':' + MinutStr =
       LineCfg^.Exitinfo^.EventInfo.StartTime then
        begin
          DefExitCode := LineCfg^.Exitinfo^.EventInfo.ErrorLevel;
          CheckEventStatus := true;
        end; { if }
end; { func. CheckEventStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function tControlObj.TimeRemain: LongInt;                 { Seconds remaining }
var TempTime : LongInt;
begin
  TimeRemain := 00;
  if LineCfg^.Exitinfo^.TimeLimit <= 00 then EXIT;

  TempTime := CurrentTime;
  TempTime := (Longint(LineCfg^.Exitinfo^.TimeLimit) * 60);

  If (TimeInfo.SecondsTimer - CurrentTime) > 00 then
    Inc(TempTime, TimeInfo.SecondsTimer - CurrentTime);
(*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  + Longint(TimeInfo.StartTime - TempTime);
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*)

  TimeRemain := TempTime;
  if CheckEventStatus then
    EventStarted := true;


  if NOT LineCfg^.TimeFrozen then
   if (TimeInfo.SecondsTimer - CurrentTime)<=00 then
     begin
       if LineCfg^.LimitsInfo^.lTime <> UnlimitedValue then
         Inc(LineCfg^.Exitinfo^.Userinfo.Elapsed, 1)
          else LineCfg^.Exitinfo^.Userinfo.Elapsed := LineCfg^.EventDeducted;
       LineCfg^.Exitinfo^.TimeLimit := SmallWord(LineCfg^.LimitsInfo^.LTime - LineCfg^.Exitinfo^.UserInfo.Elapsed);

       TimeInfo.SecondsTimer := CurrentTime + 60;
       StatusDisplay(99, false);
       Inc(LineCfg^.OnlineTime);
     end; { if }
end; { func. TimeRemain }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tControlObj.TimeWarn;
var TempTime : Longint;
    TempStr  : String;
begin
  TempTime := TimeRemain;
  Str(LineCfg^.Exitinfo^.TimeLimit, TempStr);

  If TempTime < 60 then
    begin
     If NOT TimeInfo.OneMinWarn then      { Not already warned for 1 minute? }
      begin
        TimeInfo.OneMinWarn := True;        { Already warned for 1 min. left }
        LineCfg^.RaduCodes := true;

        OutputObj^.DisplayNewLine([]);
        OutputObj^.DisplayNewLine([]);
        OutputObj^.DisplayStrLn('`A12:' + LangObj^.ralGet(ralTimeRem) + #32 + TempStr +
                     #32 + LangObj^.ralGet(ralMinutes)+'!', []);
        InputObj^.PressEnter(True, True);
      end; { If Not warned already }
    end { If TempTime less than one minute }
   else If TempTime < 120 then
         If NOT TimeInfo.TwoMinWarn then
          begin
            TimeInfo.TwoMinWarn := True;    { Already warned for 2 min. left }
            LineCfg^.RaduCodes := true;

            OutputObj^.DisplayNewLine([]);
            OutputObj^.DisplayNewLine([]);
            OutputObj^.DisplayStrLn('`A12:' + LangObj^.ralGet(ralTimeRem) + #32 + TempStr +
                         #32 + LangObj^.ralGet(ralMinutes)+'!', []);
            InputObj^.PressEnter(True, True);
          end; { If Not warned already }
end; { proc. TimeWarn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tControlObj.CheckIdle: Boolean;
var SaveCheck: Boolean;
begin
  CheckIdle := true;
  if LineCfg^.Exitinfo^.Baud=00 then EXIT;
  if NOT LineCfg^.CheckInactivity then EXIT;

  SaveCheck := LineCfg^.CheckInactivity;
  LineCfg^.CheckInactivity := False;

  If TimeInfo.IdleTime <> 00 then
   begin
      if TimeInfo.IdleTime > 30 then
       If ((NOT TimeInfo.IdleWarn) AND
          ((TimeInfo.IdleLimit - CurrentTime) < 30)) then
            begin;                             { 30 seconds before logged off }
              TimeInfo.IdleWarn := True;                     { Already warned }
              OutputObj^.ResetLines(01);

              OutputObj^.DisplayNewLine([]);
              OutputObj^.DisplayStrLn('`A12:', []);
              OutputObj^.ResetLines(01);
              OutputObj^.DisplayStrLn(LangObj^.ralGet(ralInActive2)+#7#7, []);
              InputObj^.UpdateScrn;
            end; { If Check etc }

      If (TimeInfo.IdleLimit <= CurrentTime) then
       begin
         OutputObj^.ResetLines(01);
         OutputObj^.DisplayNewLine([]);
         OutputObj^.DisplayNewLine([]);
         OutputObj^.ResetLines(01);
         OutputObj^.DisplayStrLn(langObj^.ralGet(ralInActive1), []);

         RaLog('!', 'Inactivity timeout');
         Progterminated := true;
         CheckIdle := false;
         HangUp;
       end; { While Ok }

   end; { If Idle Time set }

  LineCfg^.CheckInactivity := SaveCheck;
end; { proc. CheckIdle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tControlObj.SetTimeOut;
begin
  TimeInfo.IdleLimit := CurrentTime + TimeInfo.IdleTime;
end; { proc. SetTimeOut }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tControlObj.SetIdleTimeLimit(T: Word);
begin
  TimeInfo.IdleTime := T;
end; { proc. SetIdleTimeLimit }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tControlObj.TimeOver;
begin
  If NOT TimeInfo.TimeOverWarn then                 { If not already notified }
   begin
     TimeInfo.TimeOverWarn := True;                    { Set already notified }

     OutputObj^.DisplayNewLine([]);
     OutputObj^.DisplayNewLine([]);
     OutputObj^.DisplayNewLine([]);
     OutputObj^.DisplayStrLn('`A12:' + langObj^.ralGet(ralExceeded), []);
     RaLog('!', 'Daily timelimit exceeded');
     HangUp;
   end; { if not already notified }
end; { proc. TimeOver }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tControlObj.SetStartTime;
var Year   : Word;
    Month  : Word;
    Day    : Word;
    Dow    : Word;
begin
  GetDate(Year, Month, Day, Dow);
  LineCfg^.StartTimingDay := Day;

  TimeInfo.StartTime := CurrentTime;
  Timeinfo.SecondsTimer := CurrentTime;
end; { proc. SetStartTime }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


end. { control }
