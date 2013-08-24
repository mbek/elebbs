UNIT IORes_Un;
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
** IORES_UNIT.PAS, Criticial error handler for EleBBS and sort.
**
** Copyright (c) 1994, 1995, 1997 by Maarten Bekers
**
** Created : 22-Sep-1995
** Last Update : 05-May-2000
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 const
   HandlerInitted : Boolean = FALSE;

 type
   LogProcType = procedure(const TempStr: String);
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  MapIoErrorToStr(IoError: Longint): String;
{$IFDEF OVERLAY}
  procedure InitErrorHandler;
{$ENDIF}

var LogProc: LogProcType;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

 uses {$IFNDEF ISCGI}
        {$IFNDEF NOLINLOCAL}
          {$IFDEF DELCRT}
            Crt32,
          {$ELSE}
            Crt,
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
      {$IFDEF DELPHI}
        GenDos,
      {$ENDIF}
      {$IFNDEF MSDOS}
        FileObj,
        SysUtils,
      {$ENDIF}

      Dos, WinTitle, ElLog_U, Global;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{-- All variables are global to preserve stack --------------------------------}
const
  Hexchars : Array[0..$F] of Char = '0123456789ABCDEF';

var OldExit    : Pointer;
    TempLog_F  : Text;
    RunErrorStr: String;
    Year       : Word;               { Variables used for GetDate and GetTime }
    Month      : Word;
    Day        : Word;
    Dow        : Word;
    LineInfoLne: Longint;
    LineInfoStr: String;
    TempConv   : String;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FStr (N: LongInt): String;               { Convert integer to string }
begin
  Str(N, TempConv);
  FStr := TempConv;
end; { func. FStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Leadingzero(L: Longint):String;
begin
  Str(L, TempConv);

  While Length(TempConv) < 02 do
    TempConv := '0' + TempConv;

  LeadingZero := TempConv;
end; { func. LeadingZero }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function DateStr:String;
begin
  GetDate(Year, Month, Day, Dow);
  DateStr := LeadingZero(Month) + '-' + LeadingZero(Day) + '-' + LeadingZero(Year);
end; { func. DateStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TimeStr: String;
begin
  GetTime(Year, Month, Day, Dow);

  TimeStr := LeadingZero(year) + ':' + LeadingZero(Month) + ':' + LeadingZero(Day);
end; { func. TimeStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF MSDOS}
function HexWord(W:Word) : String;
begin
 HexWord := HexChars[Hi(W) SHR 4]  +
            HexChars[Hi(W) AND $F] +
            HexChars[Lo(W) SHR 4]  +
            HexChars[Lo(W) AND $F];
end; { func. HexWord }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function MapIoErrorToStr(IoError: Longint): String;
begin
  MapIoErrorToStr := '';

  Case IoError of
      1 : MapIoErrorToStr := 'Invalid function number';
      2 : MapIoErrorToStr := 'File not found';
      3 : MapIoErrorToStr := 'Path not found';
      4 : MapIoErrorToStr := 'Too many open files';
      5 : MapIoErrorToStr := 'File access denied';
      6 : MapIoErrorToStr := 'Invalid file handle';
     12 : MapIoErrorToStr := 'Invalid file access code';
     15 : MapIoErrorToStr := 'Invalid drive number';
     16 : MapIoErrorToStr := 'Cannot remove current directory';
     17 : MapIoErrorToStr := 'Cannot rename across drives';
     18 : MapIoErrorToStr := 'No More Files';
    100 : MapIoErrorToStr := 'Disk read error';
    101 : MapIoErrorToStr := 'Disk write error';
    102 : MapIoErrorToStr := 'File not assigned';
    103 : MapIoErrorToStr := 'File not open';
    104 : MapIoErrorToStr := 'File not open for input';
    105 : MapIoErrorToStr := 'File not open for output';
    106 : MapIoErrorToStr := 'Invalid numeric format';
    150 : MapIoErrorToStr := 'Disk is write-protected';
    151 : MapIoErrorToStr := 'Unknown unit';
    152 : MapIoErrorToStr := 'Drive not ready';
    153 : MapIoErrorToStr := 'Unknown command';
    154 : MapIoErrorToStr := 'CRC error in data';
    155 : MapIoErrorToStr := 'Bad drive request structure length';
    156 : MapIoErrorToStr := 'Disk seek error';
    157 : MapIoErrorToStr := 'Unknown media type';
    158 : MapIoErrorToStr := 'Sector not found';
    159 : MapIoErrorToStr := 'Printer out of paper';
    160 : MapIoErrorToStr := 'Device write fault';
    161 : MapIoErrorToStr := 'Device read fault';
    162 : MapIoErrorToStr := 'Hardware failure';
    200 : MapIoErrorToStr := 'Division by zero';
    201 : MapIoErrorToStr := 'Range check error';
    202 : MapIoErrorToStr := 'Stack overflow error';
    203 : MapIoErrorToStr := 'Heap overflow error';
    204 : MapIoErrorToStr := 'Invalid pointer operation';
    205 : MapIoErrorToStr := 'Floating point overflow';
    206 : MapIoErrorToStr := 'Floating point underflow';
    207 : MapIoErrorToStr := 'Invalid floating point operation';
    208 : MapIoErrorToStr := 'Overlay manager not installed';
    209 : MapIoErrorToStr := 'Overlay file read error';
    210 : MapIoErrorToStr := 'Object not initialized';
    211 : MapIoErrorToStr := 'Call to abstract method';
    212 : MapIoErrorToStr := 'Stream registration error';
    213 : MapIoErrorToStr := 'Collection index out of range';
    214 : MapIoErrorToStr := 'Collection overflow error';
    215 : MapIoErrorToStr := 'Arithmetic overflow error';
    216 : MapIoErrorToStr := 'General Protection fault'
     else MapIoErrorToStr := 'Unknown error, code '+FStr(IoError);
  end; { Case }
end; { func. MapIoErrorToStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetLocationInfo: String;
begin
  LineInfoStr := '';

  {$IFDEF VirtualPascal}
    if System.GetLocationInfo(ErrorAddr, LineInfoStr, LineInfoLne) <> nil then
      LineInfoStr := 'Error occured in file ' + LineInfoStr + '#' + FStr(LineInfoLne)
       else LineInfoStr := '';
  {$ENDIF}

  {$IFDEF FPC}
  //  LineInfoStr := BackTraceStrFunc();
  {$ENDIF}

  GetLocationInfo := LineInfoStr;
end; { func. GetLocationInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EmptyLogproc(const TempStr: String);
begin
  {-- Open the error file, and write the error to it --------------------------}
  Assign(TempLog_F, 'fatal.err');
  {$i-} System.Append(TempLog_F); {$i+}
  if IoResult > 0 then
    begin
       {$i-} System.Rewrite(TempLog_F); {$i+}
       if IoResult > 0 then EXIT;
    end; { if }

  {$i-}
    WriteLn(TempLog_F, TempStr);
    Close(TempLog_F);
  {$i+}
  if IoResult > 0 then ;

  {-- Write the error to the generic log --------------------------------------}
  RaLog('!', TempStr);
end; { proc. EmptyLogProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CreateBugreport(IoError: Longint);
var Counter: Longint;
begin
  {-- Open the bug-logfile, and disable IO checking ---------------------------}
  Assign(TempLog_F, 'bugrep.frm');
  {$i-} System.ReWrite(TempLog_F); {$i+}
  if IoResult > 0 then EXIT;

  {-- Start creating the actual error log -------------------------------------}
  {$i-}
    {-- Write some generic info about which program and which error -----------}
    WriteLn(TempLog_F);
    WriteLn(TempLog_F, '--------------------------------------------------------------------------------');
    WriteLn(TempLog_F, ' ELEBBS ERROR REPORT LOG ');
    WriteLn(TempLog_F, '--------------------------------------------------------------------------------');
    WriteLn(TempLog_F);
    WriteLn(TempLog_F, 'The program is part of...................: ', FullProgName, #32, VersionID);
    WriteLn(TempLog_F, 'Program name.............................: ', ParamStr(0));
    WriteLn(TempLog_F, 'Error....................................: ', IoError, ' / ', MapIoErrorToStr(IoError));
    WriteLn(TempLog_F, 'Time and Date............................: ', TimeStr, ' / ', DateStr);
    Writeln(TempLog_F, 'defExitCode..............................: ', DefExitCode);

    {-- Now write the RACONFIG info if we have it -----------------------------}
    if GlobalCfg^.RaConfig <> nil then
      begin
        WriteLn(TempLog_F, 'System name..............................: ', GlobalCfg^.RaConfig^.SystemName);
        WriteLn(TempLog_F, 'System operator..........................: ', GlobalCfg^.RaConfig^.SysOp);
        WriteLn(TempLog_F, 'Location.................................: ', GlobalCfg^.RaConfig^.Location);
        WriteLn(TempLog_F, 'System path..............................: ', GlobalCfg^.RaConfig^.SysPath);
        WriteLn(TempLog_F, 'Multiline system.........................: ', GlobalCfg^.RaConfig^.MultiLine);
      end { if }
        else Writeln(TempLog_F, 'Configuration not loaded.................: <nil>');

    {-- Now dump all open files to this file ----------------------------------}
    WriteLn(TempLog_F);
    WriteLn(TempLog_F);
    WriteLn(TempLog_F, '-- Open files ------------------------------------------------------------------');
    WriteLn(TempLog_F);

    {-- And close the file properly -------------------------------------------}
    Close(TempLog_F);
  {$i+}
  if IoResult > 0 then ;
end; { proc. CreateBugReport }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowError(TempStr: ShortString);
begin
  {-- Clear the screen if running EleBBS --------------------------------------}
  {$IFNDEF WINGUI}
    {$IFNDEF ISCGI}
      {$IFNDEF NOLINLOCAL}
        if RunningBBS then ClrScr;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

  {-- Clear any pending I/O Errors which could block output -------------------}
  if IoResult > 0 then ;

  {-- Make sure this error isnt seen by the user ------------------------------}
  if RunningBBS then
    Move(CrtOutput, Output, SizeOf(Output));

  {-- Now start showing the error ---------------------------------------------}
  WriteLn(^G);
  WriteLn(SystemMsgPrefix, 'A fatal error occured in this program:');

  {-- Now show the date/time this error occured -------------------------------}
  WriteLn(SystemMsgPrefix, 'This program halted at ', DateStr, ' on ', TimeStr);

  {-- Show the actual error code, depends on OS how to show it ----------------}
  {$IFDEF MSDOS}
    WriteLn(SystemMsgPrefix, 'Runtime error [', Exitcode, '] at [',HexWord(Seg(ErrorAddr^)),':',
          HexWord(Ofs(ErrorAddr^)),'].');
  {$ELSE}
    WriteLn(SystemMsgPrefix, 'Runtime error [', Exitcode, '] at [', format('%p', [ErrorAddr]), '].');
  {$ENDIF}

  {-- Now show the explanation of this error ----------------------------------}
  WriteLn(SystemMsgPrefix, TempStr);

  {-- Get an nice error string (location info) --------------------------------}
  RunErrorStr := GetLocationInfo;

  if RunErrorStr <> '' then
    begin
      WriteLn(SystemMsgPrefix, RunErrorStr);
      LogProc(RunErrorStr);
    end; { if }


  {-- Write all this stuff to the logfile -------------------------------------}
  LogProc('A fatal error occurred in this program');
  LogProc('Program halted at ' + DateStr + ' on ' + TimeStr);
  {$IFDEF MSDOS}
    LogProc('Runtime error [' + FStr(Exitcode) + '] at [' + HexWord(Seg(ErrorAddr^)) + ':' +
            HexWord(Ofs(ErrorAddr^))  + '].');
  {$ELSE}
    LogProc('Runtime error [' + FStr(Exitcode) + '] at [' + format('%p', [ErrorAddr]) + '].');
  {$ENDIF}
  LogProc(TempStr);
  LogProc('Please fill in and send form "bugrep.frm" to Maarten Bekers (maarten@elebbs.com).');
  LogProc('');
  LogProc('');


  {-- Now create the BUGREP.FRM file ------------------------------------------}
  CreateBugReport(ExitCode);
end; { proc. ShowError }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure NewExitHandler;
begin
  {-- Save old exit pointer ---------------------------------------------------}
  ExitProc := OldExit;

  {-- Show the error ----------------------------------------------------------}
  if (ErrorAddr <> NIL) then
    ShowError(MapIoErrorToStr(ExitCode));

  {-- Restore the DOS-box title -----------------------------------------------}
  RestoreTitle;
end; { NewExitHandler }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF OVERLAY}
  procedure InitErrorHandler;
{$ENDIF}
begin
  {-- Dont let this procedure be called twice! --------------------------------}
  if HandlerInitted then EXIT;
  HandlerInitted := TRUE;

{$IFNDEF ISCGI} { we dont run for cgi's - no added value in this processing }
  {-- Make sure at least one log procedure is available -----------------------}
  {$IFNDEF FPC}
    LogProc := EmptyLogProc;
  {$ELSE}
    LogProc := @EmptyLogProc;
  {$ENDIF}

  {-- Install the new exit handle ---------------------------------------------}
  OldExit := ExitProc;                                    { Save old handler }
  ExitProc := @NewExitHandler;                            { New exit handler }
{$ENDIF}
{$IFDEF OVERLAY}
end;
{$ENDIF}

end. { unit }
