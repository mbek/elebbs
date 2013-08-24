unit TCPSRV;
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
{&Delphi+}
(*
**
** General TCP server component
**
** Copyright (c) 1998, 99 by Maarten Bekers
**
** Created : 21-Feb-1999
** Last update : 09-Jul-1999
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses SockFunc, SockDef, TcpCli;

type DataAbortProc = function: Boolean;

type tTcpServer = class(tTcpClient)
       protected
         ServerAddr   : PSockAddr;
         fServerHandle: Longint;

         fKeepAlive  : Boolean;
         fReUseAddr  : Boolean;
         fBlocking   : Boolean;

         procedure SetBlocking(B: Boolean);
       public
         constructor Create;
         destructor  Destroy;

         property KeepAlive: boolean READ fKeepAlive WRITE fKeepAlive;
         property ReUseAddr: Boolean READ fReUseAddr WRITE fReuseAddr DEFAULT true;
         property ServerHandle: Longint READ fServerHandle WRITE fServerHandle DEFAULT -1;
         property Blocking: Boolean READ fBlocking WRITE SetBlocking;

         procedure SockReuse;
         procedure SockKeepAlive;

         function SetupServer(Port: Word; var ErrorStr: String; BindTo: Longint): Boolean;
         function Accept(DataAbort: DataAbortProc): Boolean;
     end; { tTcpClient }


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 uses longStr,  WordStr
        {$IFDEF WITH_DEBUG}
          ,Debug_u
        {$ENDIF};

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tTcpServer.Create;
begin
  inherited Create;

  New(ServerAddr);
  ServerHandle := -1;
end; { constructor Create }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tTcpServer.Destroy;
begin
  if ServerAddr <> nil then Dispose(ServerAddr);
  if ServerHandle <> -1 then
    if DoDisconnect then SockShutDown(ServerHandle, 2);
  if ServerHandle <> -1 then
    if DoDisconnect then SockClose(ServerHandle);

  inherited Destroy;
end; { destructor Destroy }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTcpServer.SockReuse;
var BufFlag: Longint;
begin
  BufFlag := -1;                                                { Do re-use }
  SockSetSockOpt(ServerHandle, SOL_SOCKET, SO_REUSEADDR, @BufFlag, SizeOf(BufFlag));
end; { proc. SockReuse }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTcpServer.SockKeepAlive;
var BufFlag: Longint;
    RC     : Longint;
begin
  BufFlag := -1;                                              { Do keep alive }
  RC := SockSetSockOpt(ServerHandle, SOL_SOCKET, SO_KEEPALIVE, @BufFlag, SizeOf(BufFlag));
end; { proc. SockReuse }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tTcpServer.SetupServer(Port: Word; var ErrorStr: String; BindTo: Longint): Boolean;
var ReturnCode: Longint;
begin
  ErrorStr := '';
  FillChar(ServerAddr^, SizeOf(ServerAddr^), #00);

  ReturnCode := SockSocket(AF_INET, SOCK_STREAM, IPPROTO_NULL);
  if (ReturnCode <> -1) then
   begin
     ServerHandle := ReturnCode;

     if KeepAlive then SockKeepAlive;
     if ReUseAddr then SockReuse;

     DebugLog('!', '> Sin_port = ' + FStr(Port) + ' / ' + FStr(Sockhtons(port)));

     { Setup server site }
     ServerAddr^.Sin_Addr.IPAddr := BindTo; { InAddr_Any; }
     ServerAddr^.Sin_Port        := Sockhtons(Port);
     ServerAddr^.Sin_Family      := AF_INET;
     ReturnCode := SockBind(ServerHandle, ServerAddr^);

     if ReturnCode = -1 then
       ErrorStr := 'Unable to bind to socket, RC='+FStr(ReturnCode);

     DebugLog('!', '> Binded, ReturnCode = ' + FStr(ReturnCode) + ' / SockErrorNo = ' + FStr(SockErrorNo));

   end
     else ErrorStr := 'Unable to open socket, RC='+FStr(ReturnCode);

  if ErrorStr = '' then
    begin
      ReturnCode := SockListen(ServerHandle, 5);
      if ReturnCode = -1 then
        begin
          ErrorStr := 'Unable to listen on socket, RC='+FStr(ReturnCode) + ' / SockErrorNo='+FStr(SockErrorNo);
        end; { if }
     end; { if }

  Result := ErrorStr = '';
end; { proc. Init_Server }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTcpServer.SetBlocking(B: Boolean);
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'SetBlockIng = ' + FStr(Longint(b)));
  {$ENDIF}

  SocksetBlockingIO(ServerHandle, b);
  fBlocking := b;
end; { proc. SetBlocking }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tTcpServer.Accept(DataAbort: DataAbortProc): Boolean;
var Temp: Longint;
begin
  Temp := SockAddr_len;
  New(ClientAddr);
  FillChar(ClientAddr^, SizeOf(ClientAddr^), #00);

  repeat
    SockHandle := SockAccept(ServerHandle,
                             ClientAddr,
                             Temp);
    DoSleep(10);
  until (SockHandle <> -1) OR (DataAbort{$IFDEF FPC}(){$ENDIF});

  Accept := SockHandle <> -1;
end; { func. Accept }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { tcpsrv }
