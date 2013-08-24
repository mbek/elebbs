unit WFC;
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
** Waiting for Caller routines for EleBBS
**
** Copyright (c) 1997-2001 by Maarten Bekers
**
** Created: 22-Dec-1997
** Last update: 14-Oct-2001
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 uses ApTimer;

{$IFNDEF WITH_FULL}
  This  unit shouldn''t be compiled for an utility program
{$ENDIF}


type tWfcObj = Object
         AnswerBBS     : Boolean;
         SaveModemY    : Byte;    { default: 15 }
         OldTimeStr    : String;  { default: <nothing> }
         BlankTimer    : Eventtimer;

         {-- Constructors and destructors ----------------------------------}
         constructor Init;
         destructor Done;

         {-- Generic routines ----------------------------------------------}
         procedure SendModem(S: String);
         procedure WaitingForCaller;
         procedure TakeModemOffHook;
         procedure UpdateModemDisplay(TempStr: String);
         function  GetModemStr: String;
         function  GetOk(OkStr: String): Boolean;
         function  TryInitModem: Boolean;
         function  CheckStr(S: String; BpsRate: Longint; TempStr: String): Boolean;

         {$IFDEF MSDOS}
           function  GetFossilString(Full: Boolean): String;
         {$ENDIF}

         {-- Private routines ----------------------------------------------}
         private
           function LocReadKey: Char;
           function LocKeyPressed: Boolean;
           procedure CreateRestOfScreen;
           procedure BuildScreen;
           procedure UpdateStatus(S: String);
           procedure UpdateRestOfScreen;
           procedure DoWait;
     end; { tWfcObj }

type pWfcObj = ^tWfcObj;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses {$IFDEF OS2}
       Os2Base,
     {$ENDIF}

       Dos,
       Avatar,
       Strings,
       Multi,
       Crt,
       ScrnU,

     {$IFDEF VirtualPascal}
       VpSysLow,
     {$ENDIF}

     ApFossil, CfgRec, ObjDec,
     Global, StUtils, FastScrn, Chat, Syskey, Main, Input_U,
     Jdates, Support, ElLog_U, FileRout, Sound_U, Debug_U, Ranges,
     Ral, Cases, LongStr, ComUnit, CentrStr, WordStr, GenFile, Control,
     RemScrn;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tWfcObj.Init;
begin
  SaveModemY := 15;
  OldTimeStr := '';
end; { constructor init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tWfcObj.Done;
begin
end; { constructor done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tWfcObj.SendModem(S: String);
var Counter   : Byte;
    Temp      : Char;

procedure Delay(MS: Longint);
begin
  Crt.Delay(ms);
end; { proc. Delay }

begin
  if ComObj = NIL then EXIT;
  ComObj^.Com_PurgeInBuffer;

  For Counter := 01 to Length(s) do
    Case S[Counter] of
      '^' : ComObj^.Com_SetDtr(True);
      'v' : ComObj^.Com_SetDtr(False);
      '~' : Delay(250);
      '|' : begin
              ComObj^.Com_SendChar(^M);
              ComObj^.Com_SendChar(^J);
              Delay(lineCfg^.Modem^.ModemDelay);
            end; { enter }
     else begin
            ComObj^.Com_SendChar(S[Counter]);
            Delay(lineCfg^.Modem^.ModemDelay);
          end; { if }
    end; { case }
end; { proc. SendModem }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tWfcObj.GetModemStr: String;
var Temp     : Char;
    TempStr  : String;
    ChrCount : Longint;
begin
  TempStr := '';
  Temp := #00;
  if ComObj = NIL then EXIT;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logInOut, 'GetModemStr (begin)');
  {$ENDIF}

      While (ComObj^.Com_CharAvail) AND (Temp<>#13) AND (Length(TempStr)<18) do
        begin
          if ComObj = nil then EXIT;

          Temp := ComObj^.Com_GetChar;

          if NOT (Temp in [#13, #10]) then
              TempStr := TempStr + Temp;

          ChrCount := 0;
          While (ChrCount < 50) AND (NOT ComObj^.Com_CharAvail) do
            begin
              Crt.Delay(Max(LineCfg^.Modem^.ModemDelay, 10));
              Inc(ChrCount);
            end; { while }
         end; { if }

  TempStr := Trim(TempStr);
  GetModemStr := TempStr;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logInOut, 'GetModemStr (end), received: "'+TempStr+'"');
  {$ENDIF}
end; { func. GetModemStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tWfcObj.CheckStr(S: String; BpsRate: Longint; TempStr: String): Boolean;
begin
  CheckStr := false;

  Replace('|', #13, S);

  if Pos(SUpCase(S), SUpCase(TempStr))>00 then
    begin
      LineCfg^.Exitinfo^.Baud := SmallWord(BpsRate);
      if LineCfg^.Exitinfo^.Baud = -1 then Halt(GlobalCfg^.RaConfig^.ExitFax);

      CheckStr := true;
    end; { if }

end; { func. CheckStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tWfcObj.TakeModemOffHook;
begin
  if LineCfg^.Modem^.OffHook then
    begin
      SendModem(LineCfg^.Modem^.BusyStr);
      UpdateModemDisplay('Taking modem off-hook');
    end; { if }
end; { proc. TakeModemOffHook }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF MSDOS}
function tWfcObj.GetFossilString(Full: Boolean): String;
type
  {Holds driver information from FOSSIL GetDriverInfo call}
  DriverInfo = record
    diSize      : Word;
    diSpec      : Byte;
    diRev       : Byte;
    diID        : PChar;
    diInSize    : Word;
    diInFree    : Word;
    diOutSize   : Word;
    diOutFree   : Word;
    diSWidth    : Byte;
    diSHeight   : Byte;
    diBaudMask  : Byte;
    diJunk      : Word;
  end;


var Regs: Registers;
    Info: DriverInfo;
    Temp: String;
begin
  FillChar(Info, SizeOf(DriverInfo), 00);

  Regs.ES := Seg(Info);
  Regs.DI := Ofs(Info);
  Regs.AH := $1B;
  Regs.CX := SizeOf(Info);
  Regs.DX := LineCfg^.Modem^.Comport - 01;
  Intr($14, Regs);

  Temp := StrPas(Info.diID);

  if NOT Full then
   if SUpCase(Copy(Temp, 01, Length('Ray Gwinn'))) = ('RAY GWINN') then
     begin
       Temp := Copy(Temp, Pos(',', Temp)+01, 255);
     end; { if }

  GetFossilString := Trim(Temp);
end; { func. GetFossilString }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tWfcObj.GetOk(OkStr: String): Boolean;
var Count: Byte;
    Temp : String;
begin
   Count := 60;
   GetOk := False;

   While Count>0 do
    begin
      Temp := GetModemStr;

      {$IFDEF WITH_DEBUG}
        DebugObj.DebugLog(logInOut, 'GetOk, tempstr: "'+Temp+'", expected:');
        DebugObj.DebugLog(logInOut, '                "'+OkStr+'", count='+FStr(Count));
      {$ENDIF}

      if Temp=OkStr then
        begin
          GetOk := true;
          EXIT;
        end; { if }

      Dec(Count);
      Crt.Delay(LineCfg^.Modem^.ModemDelay);
    end; { while }
end; { func. GetOk }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tWfcObj.TryInitModem: Boolean;
begin
  BoxWindow(05, 10, 33, 16, LightCyan, Black, 'Modem');

  TryInitModem := false;


  if LineCfg^.Modem^.InitStr <> '' then
    begin
      SendModem(LineCfg^.Modem^.InitStr);

      if NOT getOk(LineCfg^.Modem^.InitResp) then EXIT;
    end; { if }

  if LineCfg^.Modem^.InitStr2 <> '' then
    begin
      Delay(100);
      SendModem(LineCfg^.Modem^.InitStr2);
      if NOT getOk(LineCfg^.Modem^.InitResp) then EXIT;
    end; { if }

  TryInitModem := true;
end; { proc. TryInitModem }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tWfcObj.BuildScreen;
var SaveDirect: Boolean;
begin
  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;

  FastWrite(01, 01, Yellow, CenterJust(PidName + RegAddStrFull + ' - ' + GlobalCfg^.RaConfig^.SystemName, mnuScrnWidth, True));
  FillArea(01, 02, mnuScrnWidth, 02, mnuDblWide, Blue);
  FillArea(01, 03, mnuScrnwidth, mnuScrnLen - 2, mnuBackGrChar, mnuBackGrCol);
  FillArea(01, mnuScrnLen - 1, mnuScrnWidth, mnuScrnLen - 1, mnuSngWide, Blue);
  FastWrite(01, mnuScrnLen, White, ' L-Logon  Esc-Exit     ALT: O-Paging  J-DOS shell  P-Printer  D-Snoop  A-Answer');

  DirectScrnUpdate := SaveDirect;
  UpdateScreenBuffer(TRUE);
end; { proc. BuildScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tWfcObj.UpdateStatus(S: String);
var SaveDirect: Boolean;
begin
  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;

  BoxWindow(05, 04, 33, 08, LightCyan, Black, 'Status');
  FastWrite(07, 06, LightGray, S);

  DirectScrnUpdate := SaveDirect;
  UpdateScreenBuffer(TRUE);
end; { proc. UpdateStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tWfcObj.UpdateModemDisplay(TempStr: String);
begin
  DirectScrnUpdate := false;

  if TempStr <> '' then
    begin
      AvtObj^.ScrollScrnRegionUp(06, 12, 31, 15, 1, false);

      Crt.GotoXY(07, SaveModemY);
      FastWrite(07, SaveModemY, Lightgray, CenterJust(TempStr, 25, true));
    end; { if }

  DirectScrnUpdate := TRUE;
  UpdateScreenBuffer(DirectScrnUpdate);
end; { proc. UpdateModemDisplay }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tWfcObj.LocReadKey: Char;
begin
  if RemScrnObj <> nil then
    begin
      if RemScrnObj^.rem_Keypressed then
        LocReadKey := RemScrnObj^.rem_ReadKey
          else LocReadKey := Crt.ReadKey;
    end
      else LocReadKey := Crt.ReadKey;
end; { func. LocReadKey }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tWfcObj.LocKeyPressed: Boolean;
begin
  if RemScrnObj <> nil then
    begin
      if RemScrnObj^.rem_Keypressed then
        LockeyPressed := TRUE
          else LocKeyPressed := Crt.KeyPressed;
    end
      else LocKeyPressed := Crt.KeyPressed;
end; { func. LocKeyPressed }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tWfcObj.CreateRestOfScreen;
var SaveDirect: Boolean;
begin
  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;

  BoxWindow(5, 18, 33, 22, LightCyan, Black, 'Time');

  BoxWindow(38, 04, 76, 22, LightCyan, Black, 'System');
  FastWrite(40, 06, Lightgray, 'Environment : ');
  FastWrite(40, 07, Lightgray, 'Free memory : ');
  FastWrite(40, 08, Lightgray, 'Drive space : ');
  FastWrite(40, 09, Lightgray, 'Driver      : ');

  FastWrite(40, 11, Lightgray, 'Node        : ');
  FastWrite(40, 12, Lightgray, 'Port        : COM');
  FastWrite(40, 13, Lightgray, 'Next event  : ');
  FastWrite(40, 14, Lightgray, 'Last caller : ');
  FastWrite(40, 15, Lightgray, 'All nodes   : ');

  FastWrite(40, 17, Lightgray, 'Snoop       : ');
  FastWrite(40, 18, Lightgray, 'Printer     : ');
  FastWrite(40, 19, Lightgray, 'Paging      : ');
  FastWrite(40, 20, Lightgray, 'Answer      : ');

  DirectScrnUpdate := SaveDirect;
  UpdateScreenBuffer(DirectScrnUpdate);
end; { proc. CreateRestOfScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tWfcObj.UpdateRestOfScreen;
var TempEvent: String;

function Bool2Str(B: Boolean): String;
begin
  if B then Bool2Str := 'ON ' else Bool2Str := 'OFF';
end; { func. Bool2Str }

function DayOfWeek(Full: Boolean): String;
var DT  : DateTime;
    Temp: String;
    DOW : Word;
begin
  GetDate(DT.Year, DT.Month, DT.Day, Dow);

   Case Dow of
     00 : Temp := 'Sunday';
     01 : Temp := 'Monday';
     02 : Temp := 'Tuesday';
     03 : Temp := 'Wednesday';
     04 : Temp := 'Thursday';
     05 : Temp := 'Friday';
     06 : Temp := 'Saturday';
   end; { case }

   If Full then DayOfWeek := Temp
     else DayOfWeek := Copy(Temp, 1, 3);
end; { func. DayOfWeek }

var SaveUseFile: Boolean;
    SaveDirect : Boolean;
begin
  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;

  SearchNextEvent(LineCfg^.Exitinfo^.EventInfo);
  ReadSysInfoBBS(LineCfg^.Exitinfo^.Sysinfo);
  If LineCfg^.Exitinfo^.EventInfo.StartTime='00:00' then TempEvent := 'none'
    else TempEvent := LineCfg^.Exitinfo^.EventInfo.StartTime;

  SaveuseFile := LangObj^.UsingFile;
  LangObj^.UsingFile := FALSE; { we dont want localized dates on the wfc }

  FastWrite(08, 20, LightGray, TimeStr(False, False) + #32#32 + '(' + DayOfWeek(False) + ')' + #32 +
                               LangObj^.RaFormatDate(DateStr, 8, y2k_SysDate, 0));

  LangObj^.UsingFile := SaveUseFile;

{$IFDEF MSDOS}
  FastWrite(54, 06, Lightgray, MultiString[MultiTasker]);
{$ENDIF}

{$IFDEF WIN32}
  FastWrite(54, 06, Lightgray, 'Windows9x/NT');
{$ENDIF}

{$IFDEF OS2}
  FastWrite(54, 06, Lightgray, 'OS/2');
{$ENDIF}

{$IFDEF GO32V2}
  FastWrite(54, 06, Lightgray, 'DOS-386');
{$ENDIF}

  {$IFDEF MSDOS}
  	FastWrite(54, 07, Lightgray, CommaStr(MemAvail) + ' bytes');
  {$ELSE}
  	FastWrite(54, 07, Lightgray, '(unknown)');
  {$ENDIF}
  FastWrite(54, 08, Lightgray, CommaStr(GetDiskFree(GlobalCfg^.RaConfig^.SysPath[1])) + ' bytes');
 {$IFDEF MSDOS}
  FastWrite(54, 09, Lightgray, Copy(GetFossilString(False), 1, 21));
 {$ENDIF}

  FastWrite(54, 11, Lightgray, FStr(LineCfg^.RaNodeNr));
  FastWrite(54, 12, Lightgray, 'COM'+FStr(LineCfg^.Modem^.Comport));
  FastWrite(54, 13, Lightgray, TempEvent);
  FastWrite(54, 14, Lightgray, MakeLen(LineCfg^.Exitinfo^.Sysinfo.LastCaller, 17, space, false, false));
  FastWrite(54, 15, Lightgray, FStr(LineCfg^.Exitinfo^.Sysinfo.TotalCalls));

  FastWrite(54, 17, Lightgray, Bool2Str(LineCfg^.Snooping));
  FastWrite(54, 18, Lightgray, Bool2Str(LineCfg^.PrinterLogging));
  FastWrite(54, 19, Lightgray, Bool2Str(RaYell));
  FastWrite(54, 20, Lightgray, Bool2Str(AnswerBBS));

  TextAttr := 00;
  GotoXY(1,1);

  DirectScrnUpdate := SaveDirect;
  UpdateScreenBuffer(DirectScrnUpdate);
end; { proc. UpdateRestOfScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tWfcObj.DoWait;
var CH        : Char;
    TempBool  : Boolean;
    SavePort  : Byte;
    InitCount : Byte;
    TempStr   : String;
    SaveScrn  : pointer;
    CheckTimer: EventTImer;
begin
  BuildScreen;
  AnswerBBS := LineCfg^.Modem^.AnswerPhone;
  if ComObj = NIL then Halt(1);
  InitCount := 00;

  repeat
    if InitCount>00 then
       UpdateStatus('Initialise failure (#'+FStr(InitCount)+')')
        else UpdateStatus('Initialising the modem');

    TempBool := TryInitModem;
    if TempBool then
      begin
         UpdateModemDisplay(LineCfg^.Modem^.InitResp);
         {$IFDEF WITH_FULL}
           ComObj^.Com_PurgeInbuffer;
         {$ENDIF}
      end; { if }

    Inc(InitCount);

    if LocKeypressed then
     if LocReadkey=#27 then InitCount := LineCfg^.Modem^.InitTries + 12;
  until (TempBool) OR (InitCount > LineCfg^.Modem^.InitTries);
  if NOT TempBool then System.Halt(1);                      { Init-failure }

  CreateRestOfScreen;

  if GlobalCfg^.RaConfig^.BlankSecs>00 then
    NewTimer(BlankTimer, Secs2Tics(GlobalCfg^.RaConfig^.BlankSecs));
  NewTimer(CheckTimer, Secs2tics(1));

  UpdateStatus('Waiting for caller');

  repeat
    if OldTimeStr <> TimeStr(False, False) then
      UpdateRestOfScreen;
    OldTimeStr := TimeStr(False, False);

    DoSlice;

    if ComObj^.Com_CharAvail then
     with LineCfg^ do
     begin
       TempStr := GetModemStr;
       UpdateModemdisplay(TempStr);

       if Pos(SUpCase(LineCfg^.Modem^.RingStr), SUpCase(TempStr))>00 then
        if AnswerBBS then
          SendModem(LineCfg^.Modem^.AnswerStr);

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
                                   LineCfg^.Exitinfo^.ErrorFreeConnect := GetOk(Modem^.ErrorFreeString);
                                   EXIT;
                                 end; { if }

     end; { if }

    if GlobalCfg^.RaConfig^.BlankSecs>00 then
     if TimerExpired(BlankTimer) then
       begin
         SaveScreen(SaveScrn);
         TextAttr := $00;
         Crt.ClrScr;
         Partclear(01, 01, 80, mnuScrnLen, White, #32);
         repeat
           DoSlice;
         until (Lockeypressed) OR (ComObj^.Com_CharAvail) OR
                (contrObj^.CheckEventStatus) OR (NOT contrObj^.CheckRaExit);
         RestoreScreen(SaveScrn);

         if GlobalCfg^.RaConfig^.BlankSecs>00 then
            NewTimer(BlankTimer, Secs2Tics(GlobalCfg^.RaConfig^.BlankSecs));
       end; { if }

    if LocKeypressed then
     begin
       CH := UpCase(LocReadKey);

       Case CH of
         'L' : begin
                 LineCfg^.LocalLogon := true;
                 LineCfg^.Exitinfo^.Baud := 00;
                 LineCfg^.CarrierCheck := false;
                 TakeModemOffHook;
                 CloseComport;
                 BREAK;                           { Local login is default }
               end; { Local }
         #27 : begin
                 TakeModemOffHook;
                 defExitCode := 00;
                 Halt(0);
               end; { Escape }
         #00 : begin
                 CH := LocReadkey;

                 Case CH of
           { Alt-O }  #24 : begin
                              SaveScreen(SaveScrn);
                              LocalCommand(#24, 03, false);
                              RestoreScreen(SaveScrn);
                            end; { alt-o }
           { Alt-J }  #36 : begin
                              SaveScreen(SaveScrn);

                              ScreenClear(07, #32);
                              SavePort := LineCfg^.Modem^.Comport;
                              LocalCommand(#36, 03, false);
                              LineCfg^.Modem^.Comport := SavePort;

                              RestoreScreen(SaveScrn);
                            end; { if }
           { Alt-P }  #25 : LineCfg^.PrinterLogging := NOT LineCfg^.PrinterLogging;
           { Alt-D }  #32 : LineCfg^.Snooping := NOT LineCfg^.Snooping;
           { Alt-A }  #30 : AnswerBBS := NOT AnswerBBS;
                 end; { case }

               end; { extended key }
       end; { case }

        if CH in ['L', #27, #24, #36, #25, #32, #30] then
          UpdateRestOfScreen;

       if GlobalCfg^.RaConfig^.BlankSecs > 00 then
         NewTimer(BlankTimer, Secs2Tics(GlobalCfg^.RaConfig^.BlankSecs));
     end; { if keypressed }

    if TimerExpired(CheckTimer) then
      begin
        contrObj^.CheckForEleMon;
        NewTimer(CheckTimer, Secs2tics(3));

        if contrObj^.CheckEventStatus then
          begin
            Support.Halt(defExitCode);
          end; { Event Busy }

        if NOT contrObj^.CheckRaExit then ;
      end; { if }

  until (true=false);
end; { proc. DoWait }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tWfcObj.WaitingForCaller;
begin
  LangObj^.RalFile^.Close;
  if LangObj^.RalFile^.IOResult>00 then ;

  CursorOff;
  DoWait;

  LoadDefaultLanguage;

  CursorOn;
  Crt.ClrScr;
  FillArea(01, 01, mnuScrnWidth, mnuScrnLen, #32, 03);
end; { proc. WaitingForCaller }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { wfc }
