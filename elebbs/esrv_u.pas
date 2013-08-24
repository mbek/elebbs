unit esrv_u;
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
** ESRV_U, supporting unit fr EleServ
**
** Copyright (c) 1999 - 2001 by Maarten Bekers
**
** Created : 17-Dec-2001
** Last update : 17-Dec-2001
**
*)

{$IFDEF WITH_FULL}
{$ENDIF}

{$IFDEF ISCGI}
  Wont compile
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 uses Threads;
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FatalError(ErrorStr: String);
procedure GenericSleep(Ms: Longint);

function findEmptySlot: Longint;
function FindEmptyNode: Longint;


type
  SrvTypeType = (srvNews, srvIdent, srvTelnet, srvFTP);

const
  Intern_MaxSessions  = 250;
  SecsTimeOut         = 180;
  TermProgram         : Boolean = false;         { Terminate whole program }
  HaltScrn            : Boolean = false;        { Dont update the scrn atm }
  NoDnsLookup         : Boolean = false;           { Dont lookup DNS names }
  srvUpdateScreen     : Boolean = false;     { Something changed, upd scrn }

  CurrentSessions     : Longint = 00;
  TotalSessions       : Longint = 00;
  TotalRefused        : Longint = 00;

  StackSize           = 1024 * 64;
  ExecEleStack        = 1024 * 32;

var
  Connections : Array[1..Intern_MaxSessions] of
                                  record
                                    SrvType     : SrvTypeType;
                                    Name        : ShortString;
                                    IpAddr      : String[16];
                                    StartTimeStr: ShortString;
                                    ClientRC    : Longint;
                                    ServerThread: PThreadsObj;
                                    SrvData     : Pointer;
                                  end; { record }


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
  uses Debug_U, Crt, SockFunc, ScrnU, Global

        {$IFDEF WIN32}
          , Windows
        {$ENDIF}

        {$IFDEF OS2}
         , Os2base
        {$ENDIF}
        ;
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FatalError(ErrorStr: String);
begin
  {-- Log it to the logs ---------------------------------------------------}
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, '(EleSrv) FatalError(s='+ErrorStr+');');
  {$ENDIF}

  {-- Clear the screen using primitives to avoid any extra errors ----------}
  TextAttr := Lightgray;
  ClrScr;

  {-- Now write the error to the screen ------------------------------------}
  WriteLn(SystemMsgPrefix, 'Fatal error occured!');
  WriteLn(SystemMsgPrefix, ErrorStr);

  {-- Show the last socket error that occured ------------------------------}
  WriteLn(SystemMsgPrefix, '#', SockErrorNo, ', ', SockGetErrStr(SockErrorNo));

  {-- Make sure we exit with an cursor -------------------------------------}
  CursorOn;

  {-- And terminate the program --------------------------------------------}
  TermProgram := true;
  Halt(255);
end; { proc. FatalError }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GenericSleep(Ms: Longint);
begin
  {$IFDEF WIN32} Sleep(Ms); {$ENDIF}

  {$IFDEF OS2} DosSleep(Ms); {$ENDIF}
end; { proc. GenericSleep }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function findEmptySlot: Longint;
var Counter: Longint;
begin
  FindEmptySlot := 0;

  for Counter := 01 to intern_MaxSessions do
    if Connections[Counter].Name = '' then
      begin
        FindEmptySlot := Counter;
        BREAK;
      end; { if }
end; { func. findEmptySlot }


function NodeInUse(Node: Longint): Boolean;
var Counter: Longint;
begin
  NodeInUse := False;

  for Counter := 01 to Intern_MaxSessions do
    if (Connections[Counter].Name <> '') and (Connections[Counter].srvType = srvTelnet) and (Longint(Connections[Counter].srvData) = Node) then
      begin
        NodeInUse := True;
        BREAK;
      end; { if }
end; { func. NodeInuse }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FindEmptyNode: Longint;
var Counter: Longint;
begin
  FindEmptyNode := 0;

  for Counter := LineCfg^.Telnet^.StartNodeWith to (LineCfg^.Telnet^.StartNodeWith + LineCfg^.Telnet^.MaxSessions - 1) do
    if not NodeInUse(Counter) then
      begin
        FindEmptyNode := Counter;
        BREAK;
      end; { if }
end; { func. FindEmptyNode }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
end. { esrv_u }
