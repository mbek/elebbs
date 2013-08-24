unit UDPCLI;
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
** General UDP client component (based on TCPCLI)
**
** Copyright (c) 1998, 99 by Maarten Bekers
**
** Created : 13-Nov-1998
** Last update : 11-Jul-1999
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses SockFunc, SockDef, TcpCli;

type DebugLogProc = procedure(C: Char; S: ShortString);

type tUdpClient = class(tTcpClient)
       protected

       public
         function ConnectToServer(Hostname: String; Port: Longint;
                                    var ErrorStr: String): Boolean;
     end; { tTcpClient }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses LongStr;

function tUdpClient.ConnectToServer(Hostname: String; Port: Longint;
                                    var ErrorStr: String): Boolean;
var ReturnCode: Longint;
    IpAddr    : tIn_Addr;
begin
  DebugLog('0', 'ConnectToServer, hostname='+hostname);
  DebugLog('0', 'ConnectToServer, port='+FStr(Port));
   DebugLog('0', 'ConnectToServer, port(2)='+FStr(SockHtons(Port)));

  New(ClientAddr);
  ErrorStr := '';

  FillChar(ClientAddr^, SizeOf(ClientAddr^), #00);     {null}
  ReturnCode := SockSocket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);

  if ReturnCode <> -1 then
    begin
      SockHandle := ReturnCode;

      IpAddr := SockInetAddr(HostName);
      if IpAddr.IpAddr = INADDR_NONE then
        begin
          IpAddr.IpAddr := SockGetHostAddrByName(HostName);
        end; { if }

      if ReturnCode <> -1 then
        begin
          ClientAddr^.Sin_Addr.IPAddr := IpAddr.IpAddr;
          ClientAddr^.Sin_Port        := sockhtons(Port);
          ClientAddr^.Sin_Family      := AF_INET;

          ReturnCode := SockConnect(SockHandle,
                                    ClientAddr^);
          if ReturnCode = -1 then
            ErrorStr := 'Unable to connect (' + SockGetErrStr(SockErrorNo) + ')';
        end
         else ErrorStr := 'Unable to get hostname (' + SockGetErrStr(SockErrorNo) + ')';
    end
     else ErrorStr := 'Unable to intiailize socket (' + SockGetErrStr(SockErrorNo) + ')';

  SocksetBlockingIO(SockHandle, false);

  Result := (ErrorStr = '');
  HaveConnection := Result;

  DebugLog('0', 'ConnectToServer, ErrorStr='+ErrorStr);
end; { func. ConnectToServer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { udpcli }
