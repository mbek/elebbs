unit OpenMDM;
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
** Open the modem routines for EleBBS
**
** Copyright (c) 1996-1999 by Maarten Bekers
**
** Created : 07-Mar-1999
** Last update : 07-Mar-1999
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF ISCGI}
  This unit should not be compiled into EleWEB
{$ENDIF}

procedure OpenModem;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Dos, Crt, GenDos, Global, ApFossil, ComUnit, Apport,
     ApMisc, ElLog_U, Debug_U, FastScrn, Input_U, ObjDec

     {$IFDEF WIN32}
       ,Windows
       ,Support
     {$ENDIF};

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  FStr (N : LongInt) : String;           { Convert integer to string }
var Temp: String;
begin
  Str(n,temp);
  FStr:=Temp;
end; { func. FStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure OpenModem;
 {$IFDEF MSDOS}
   var Info: DriverInfo;
       Regs: Registers;
  {$ENDIF}
begin
 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logFileIO, 'OpenModem (begin)');
   if LineCfg^.LocalLogon then
     DebugObj.DebugLog(logFileIO, 'LocalLogon : TRUE')
      else DebugObj.DebugLog(logFileIO, 'LocalLogon: FALSE');
 {$ENDIF}

  ComObj := nil;

  if LineCfg^.LocalLogon then EXIT;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileIO, 'OpenModem (-02-)');
  {$ENDIF}

  ComDevice := Com1;
  ComDevice := ComNameType(LineCfg^.Modem^.Comport - 01);
  if OpenComDelay > 0 then Delay(OpenComDelay);

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileIO, 'OpenModem (-03-)');
  {$ENDIF}

  InitFossil(LineCfg^.Modem^.LockModem, LineCfg^.Exitinfo^.Baud);

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileIO, 'OpenModem (-04-)');
  {$ENDIF}

  if (ComObj = nil) OR (AsyncStatus <> ecOk) then
    begin
      LocalScreenLn(SystemMsgPrefix + 'Unable to open comport: COM' + FStr(LineCfg^.Modem^.ComPort));
      LineCfg^.Exitinfo^.Baud := 00;
      if AsyncStatus <> ecOk then
        LocalScreenLn(SystemMsgPrefix + StatusStr(AsyncStatus));

      RaLog('!', StatusStr(AsyncStatus));
      InputObj^.UpdateScrn;

      {$IFNDEF WINGUI}
        Crt.Delay(2000);                                   { Wait for 2 seconds }
      {$ELSE}
        Support.Delay(2000);
      {$ENDIF}

      RaLog('!', 'Unable to open communications port, aborting!');
      ProgTerminated := true;
      System.Halt(01);          { Default exitcode when not modem initialized }
    end; { if }

 {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileIO, 'Comport opened...');
    if LineCfg^.Modem^.LockModem then
     DebugObj.DebugLog(logFileIO, 'Locked       : Yes')
      else DebugObj.DebugLog(logFileIO, 'Locked       : No');
    DebugObj.DebugLog(logFileIO, 'Baudrate     : '+FStr(LineCfg^.Exitinfo^.Baud));
    DebugObj.DebugLog(logFileIO, 'Comnumber    : '+FStr(LineCfg^.Modem^.Comport));
    DebugObj.DebugLog(logFileIO, 'Output Buffer: '+FStr(OutSize));
    DebugObj.DebugLog(logFileIO, 'Input Buffer : '+FStr(InSize));
 {$ENDIF}

 {$IFDEF MSDOS}
    FillChar(Info, SizeOf(DriverInfo), 00);

    Regs.ES := Seg(Info);
    Regs.DI := Ofs(Info);
    Regs.AH := $1B;
    Regs.CX := SizeOf(Info);
    Regs.DX := LineCfg^.Modem^.Comport - 01;
    Intr($14, Regs);

    if Info.diInSize < 2048 then
      RaLog('!', 'Small FOSSIL input buffer detected, increase is recommended! (at least 2048 bytes)');

    if Info.diOutSize < 2048 then
      RaLog('!', 'Small FOSSIL output buffer detected, increase is recommended! (at least 2048 bytes)');
 {$ENDIF}
end; { proc. OpenModem }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit Openmdm }
