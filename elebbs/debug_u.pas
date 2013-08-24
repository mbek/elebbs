unit Debug_U;
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
** DEBUG_U, Debugging routines for EleBBS
**
** Copyright (c) 1997 by Maarten Bekers
**
** Created : 19-Sep-1997
** Last update : 14-Oct-2001
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
uses FileObj;

type
  LoggingType = (logString, logDateTime, logElLog, logFileIO, logCrc,
                 logChat, logMenuFunc, logMailInt, logMailSys, logAreas,
                 logTransfer, logFileSys, logSecurity,
                 logRaExec, logSupport, logEditor, logFastScrn,
                 logRestoreSave, logStatus, logAccess, logIEMSI,
                 logMenu, logMain, logLogon, logQuest, logFileRout,
                 logInOut, logRAL, logALL, logKey, logAsync, logTcpIp,
                 logUserBase);
  LoggingSetType = Set of LoggingType;

type tDebugObj = Object
         LoggingEnabled: LoggingSettype;      { default: logAll }
         DebugLogging  : Boolean;             { default: false }
         InDebug_Log   : Boolean;             { default: InDebug_Log }
         DebugLog_F    : pFileObj;
         SaveIO        : Longint;
         IoStr         : String;

         {-- Constructors and destructors ----------------------------------}
         constructor Init;
         destructor Done;

         {-- Generic routines ----------------------------------------------}
         procedure DebugLog(const LogType: LoggingType;
                            const LogStr : String);

         {-- Private routines ----------------------------------------------}
         private
           function LoggingStr(Nr: LoggingType): String;
           function FStr (N : LongInt) : String;   { Convert integer to string }
           function LeadingZero(L: Longint; Len: Byte): String;
           function TimeStr (WithSec,WithHS : Boolean) : String;
           function Makelen(S: String; Len: Byte): String;
     end; { tDebugObj }

type pDebugObj = ^tDebugObj;

var DebugObj: tDebugObj;                           { Debug-logging routines }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF WINGUI}
uses SysUtils, GenDos, Dos{, ElLog_u}
       {$IFDEF VirtualPascal}, VpUtils {$ENDIF} ;
{$ELSE}
uses SysUtils, GenDos, ElLog_U;
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tDebugObj.Init;
begin
  LoggingEnabled := [logAll];
  DebugLogging := FALSE;
  InDebug_Log := FALSE;
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tDebugObj.Done;
begin
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tDebugObj.LoggingStr(Nr: LoggingType): String;
begin
  { a table might be faster, but more resource usage ... :( }
  case Nr of
    logString      : LoggingStr := 'logString';
    logDateTime    : LoggingStr := 'logDateTime';
    logElLog       : LoggingStr := 'logElLog';
    logFileIO      : LoggingStr := 'logFileIO';
    logCrc         : LoggingStr := 'logCRC';
    logChat        : LoggingStr := 'logChat';
    logMenuFunc    : LoggingStr := 'logMenuFunc';
    logMailInt     : LoggingStr := 'logMailInt';
    logMailSys     : LoggingStr := 'logMailSys';
    logAreas       : LoggingStr := 'logAreas';
    logTransfer    : LoggingStr := 'logTransfer';
    logFileSys     : LoggingStr := 'logFileSys';
    logSecurity    : LoggingStr := 'logSecurity';
    logRaExec      : LoggingStr := 'logRaExec';
    logSupport     : LoggingStr := 'logSupport';
    logEditor      : LoggingStr := 'logEditor';
    logFastScrn    : LoggingStr := 'logFastScrn';
    logRestoreSave : LoggingStr := 'logRestoreSave';
    logStatus      : LoggingStr := 'logStatus';
    logAccess      : LoggingStr := 'logAccess';
    logIEMSI       : LoggingStr := 'logIEMSI';
    logMenu        : LoggingStr := 'logMenu';
    logMain        : LoggingStr := 'logMain';
    logLogon       : LoggingStr := 'logLogon';
    logQuest       : LoggingStr := 'logQuest';
    logfileRout    : LoggingStr := 'logFileRout';
    logInOut       : LoggingStr := 'logInOut';
    logRal         : LoggingStr := 'logRAL';
    logAll         : LoggingStr := 'logAll';
    logKey         : LoggingStr := 'logKey';
    logAsync       : LoggingStr := 'logAsync';
    logTcpIp       : LoggingStr := 'logTcpIp';
    logUserBase    : LoggingStr := 'logUserbase';
  end; { case }
end; { func. LoggingStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function tDebugObj.FStr (N : LongInt) : String;   { Convert integer to string }
var Temp: String;
begin
  Str(n,temp);
  FStr:=Temp;
end; { func. FStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{ dup.procedures making overlaying possible }
Function tDebugObj.LeadingZero(L: Longint; Len: Byte): String;
var Tmp: String;
begin
 Tmp := FStr(L);

 While Length(Tmp)<Len do Tmp := '0' + Tmp;
 LeadingZero := Tmp;
end; { func. LeadingZero }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tDebugObj.TimeStr (WithSec,WithHS : Boolean) : String;      { Time to string }
var Hours,
    Minutes,
    Secs,
    MSecs   : Word;
    TempStr : String;
begin
 GetTime(Hours, Minutes, Secs, MSecs);

 TempStr := LeadingZero(Hours, 02) + ':' + LeadingZero(Minutes, 02);

 If WithSec then TempStr := TempStr + ':' + LeadingZero(Secs, 02);
 If WithHS then TempStr := TempStr + '.' + LeadingZero(MSecs, 02);

 TimeStr := TempStr;
end; { func. TimeStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tDebugObj.Makelen(S: String; Len: Byte): String;
begin
  if length(s) > len then SetLength(S, Len);

  if Len>00 then
   While Length(s) < Len do
     S := S + #32;

  MakeLen := S;
end; { func. MakeLen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tDebugObj.DebugLog(const LogType: LoggingType; const LogStr: String);
begin
  {$IFDEF RELEASE}
    EXIT;
  {$ENDIF}

  {$IFDEF WITH_DEBUG}
    If (NOT DebugLogging) then EXIT;
    if (InDebug_Log) then EXIT;
    if NOT ((logType in loggingEnabled) OR (logALL in loggingEnabled)) then EXIT;

    INDEBUG_LOG := TRUE;
    SaveIO := IoResult;

    if SaveIO <> 0 then IoStr := #32 + '#' + FStr(SaveIo)+ #32
      else IoStr := '';

    New(DebugLog_F, Init);
    {$IFNDEF ELEUNIX}
      DebugLog_F^.Assign('C:\DEBUG.TXT');
    {$ELSE}
      DebugLog_F^.Assign('./DEBUG.TXT');
    {$ENDIF}

    DebugLog_F^.FileMode := 2 + $40;
    if NOT DebugLog_F^.OpenOrCreate(1) then EXIT;

    if LogStr = '' then DebugLog_F^.WriteLn('')
      else DebugLog_F^.WriteLn(TimeStr(True, True) + ' - ' + IoStr +
                             {$IFNDEF VirtualPascal}
                              {$IFNDEF DELPHI}
                               {$IFNDEF FPC}
                               	MakeLen(FStr(MemAvail), 6) + 'b.' +
                               {$ENDIF}
                              {$ENDIF}
                             {$ELSE}
                               MakeLen(FStr(MemUsed), 6) + 'b. (used!) ' +
                             {$ENDIF}
                             ' -- [' + MakeLen(LoggingStr(logType), 10) + '] ' +
                             logStr);

    Dispose(DebugLog_F, Done);

    InDebug_Log := FALSE;
  {$ENDIF}
end; { proc. DebugLog }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_DEBUG}
begin
  DebugObj.Init;
{$ENDIF}
end. { unit DEBUG_U }
