UNIT SigBBS;
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
{$IFDEF FPC}
  {$R-}
{$ENDIF}
(*
**
** Signal handler for EleBBS.
** Currently responds to SIGHUP
**
** Copyright (c) 2001 by Maarten Bekers
**
** Created : 06-Sep-2001
** Last update : 06-Sep-2001
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
uses {$IFDEF VER1_0}
       Linux;
     {$ELSE}
       OldLinux,
       Unix;
     {$ENDIF}

type tSignalObj = Object
        OldAction : PSigActionRec;
        NewAction : PSigActionRec;

        constructor Init;
        destructor Done;

        procedure InstallSignalHandler;
     end; { tSignalObj }

type
  pSignalobj = ^tSignalObj;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 uses ExitProg,
       ElLog_u,
        Debug_U;
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tSignalobj.Init;
begin
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tSignalObj.Done;
begin
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure HandleSig(Sig: Longint); cdecl;
begin
  Case Sig of
    SIGHUP : begin
               RaLog('!', 'HUP signal caught, terminating session');
               Hangup;
             end; { SIGHUP }
  end; { case }

end; { proc. HandleSig }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSignalObj.InstallSignalHandler;
var MySigSet: SigSet;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'InstallSignalHandler (BEGIN)');
  {$ENDIF}

  New(NewAction);
  New(OldAction);

  {-- initialize some stuff -------------------------------------------------}
  FillChar(MySigSet, SizeOf(MySigSet), 0);

  {-- setup the record for the signal handler for this signal ---------------}
  NewAction^.Handler.SH := @HandleSig;
  NewAction^.Sa_Mask := MySigSet;
  NewAction^.Sa_Flags := 0;
  {$IFNDEF FREEBSD}
    NewAction^.Sa_Restorer := NIL;
  {$ENDIF}

  {-- install the new signal handler ----------------------------------------}
  SigAction(SigHup, NewAction, OldAction);

  if LinuxError <> 0 then
    RaLog('!', 'Unable to install SIGHUP signal handler');

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'InstallSignalHandler ( END )');
  {$ENDIF}
end; { proc. InstallSignalHandler }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { SigBBS }
