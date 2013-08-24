{
    $Id: fbsdsck.pas,v 1.1.1.1 2007/08/23 02:24:05 elebbs Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FBsdSck;
{$i C:\bbs\compiler.inc}
{$MODE objfpc}

Interface

{$ifdef dolibc}
  {$LINKLIB c}
{$endif}

uses SockDef,
       {$IFDEF VER1_0}
          Linux;
       {$ELSE}
          Unix;
       {$ENDIF}

Const
  { Socket Types }
  SOCK_STREAM     = 1;               { stream (connection) socket   }
  SOCK_DGRAM      = 2;               { datagram (conn.less) socket  }
  SOCK_RAW        = 3;               { raw socket                   }
  SOCK_RDM        = 4;               { reliably-delivered message   }
  SOCK_SEQPACKET  = 5;               { sequential packet socket     }

  AF_UNSPEC       = 0;
  AF_UNIX         = 1;      { Unix domain sockets          }
  AF_INET         = 2;      { Internet IP Protocol         }

  {  Protocol Families }
  PF_UNSPEC       = AF_UNSPEC;
  PF_UNIX         = AF_UNIX;
  PF_INET         = AF_INET;

const
 {BSD}
  AF_LOCAL        =1;              { local to host (pipes, portals) }
  AF_IMPLINK      =3;               { arpanet imp addresses }
  AF_PUP          =4;              { pup protocols: e.g. BSP }
  AF_CHAOS        =5;               { mit CHAOS protocols }
  AF_NS           =6;              { XEROX NS protocols }
  AF_ISO          =7;              { ISO protocols }
  AF_OSI          =AF_ISO;
  AF_ECMA         =8;              { European computer manufacturers }
  AF_DATAKIT      =9;              { datakit protocols }
  AF_CCITT        =10;             { CCITT protocols, X.25 etc }
  AF_SNA          =11;             { IBM SNA }
  AF_DECnet       =12;             { DECnet }
  AF_DLI          =13;             { DEC Direct data link interface }
  AF_LAT          =14;             { LAT }
  AF_HYLINK       =15;             { NSC Hyperchannel }
  AF_APPLETALK    =16;             { Apple Talk }
  AF_ROUTE        =17;             { Internal Routing Protocol }
  AF_LINK         =18;             { Link layer interface }
  pseudo_AF_XTP   =19;             { eXpress Transfer Protocol (no AF) }
  AF_COIP         =20;             { connection-oriented IP, aka ST II }
  AF_CNT          =21;             { Computer Network Technology }
  pseudo_AF_RTIP  =22;             { Help Identify RTIP packets }
  AF_IPX          =23;             { Novell Internet Protocol }
  AF_SIP          =24;             { Simple Internet Protocol }
  pseudo_AF_PIP   =25;             { Help Identify PIP packets }
  AF_ISDN         =26;             { Integrated Services Digital Network}
  AF_E164         =AF_ISDN;        { CCITT E.164 recommendation }
  pseudo_AF_KEY   =27;             { Internal key-management function }
  AF_INET6        =28;             { IPv6 }
  AF_NATM         =29;             { native ATM access }
  AF_ATM          =30;             { ATM }
  pseudo_AF_HDRCMPLT=31;           { Used by BPF to not rewrite headers
                                    in interface output routine}
  AF_NETGRAPH     =32;             { Netgraph sockets }
  AF_MAX          =33;

  SOCK_MAXADDRLEN =255;             { longest possible addresses }

{
* Protocol families, same as address families for now.
}
  PF_LOCAL        =AF_LOCAL;
  PF_IMPLINK      =AF_IMPLINK;
  PF_PUP          =AF_PUP;
  PF_CHAOS        =AF_CHAOS;
  PF_NS           =AF_NS;
  PF_ISO          =AF_ISO;
  PF_OSI          =AF_ISO;
  PF_ECMA         =AF_ECMA;
  PF_DATAKIT      =AF_DATAKIT;
  PF_CCITT        =AF_CCITT;
  PF_SNA          =AF_SNA;
  PF_DECnet       =AF_DECnet;
  PF_DLI          =AF_DLI;
  PF_LAT          =AF_LAT;
  PF_HYLINK       =AF_HYLINK;
  PF_APPLETALK    =AF_APPLETALK;
  PF_ROUTE        =AF_ROUTE;
  PF_LINK         =AF_LINK;
  PF_XTP          =pseudo_AF_XTP;  { really just proto family, no AF }
  PF_COIP         =AF_COIP;
  PF_CNT          =AF_CNT;
  PF_SIP          =AF_SIP;
  PF_IPX          =AF_IPX;         { same format as AF_NS }
  PF_RTIP         =pseudo_AF_RTIP; { same format as AF_INET }
  PF_PIP          =pseudo_AF_PIP;
  PF_ISDN         =AF_ISDN;
  PF_KEY          =pseudo_AF_KEY;
  PF_INET6        =AF_INET6;
  PF_NATM         =AF_NATM;
  PF_ATM          =AF_ATM;
  PF_NETGRAPH     =AF_NETGRAPH;
  PF_MAX          =AF_MAX;

const
        SOL_SOCKET = $FFFF;
        SO_DEBUG         =$0001;        { turn on debugging info recording }
        SO_ACCEPTCONN    =$0002;        { socket has had listen() }
        SO_REUSEADDR     =$0004;        { allow local address reuse }
        SO_KEEPALIVE     =$0008;        { keep connections alive }
        SO_DONTROUTE     =$0010;        { just use interface addresses }
        SO_BROADCAST     =$0020;        { permit sending of broadcast msgs }
        SO_USELOOPBACK   =$0040;        { bypass hardware when possible }
        SO_LINGER        =$0080;        { linger on close if data present }
        SO_OOBINLINE     =$0100;        { leave received OOB data in line }
        SO_REUSEPORT     =$0200;        { allow local address & port reuse }
        SO_TIMESTAMP     =$0400;        { timestamp received dgram traffic }

{
 * Additional options, not kept in so_options.
 }
        SO_SNDBUF        =$1001;        { send buffer size }
        SO_RCVBUF        =$1002;        { receive buffer size }
        SO_SNDLOWAT      =$1003;        { send low-water mark }
        SO_RCVLOWAT      =$1004;        { receive low-water mark }
        SO_SNDTIMEO      =$1005;        { send timeout }
        SO_RCVTIMEO      =$1006;        { receive timeout }
        SO_ERROR         =$1007;        { get error status and clear }
        SO_TYPE          =$1008;        { get socket type }


        SHUT_RD         =0;             { shut down the reading side }
        SHUT_WR         =1;             { shut down the writing side }
        SHUT_RDWR       =2;             { shut down both sides }

type
  TUnixSockAddr = packed Record
    family:word; { was byte, fixed }
    path:array[0..108] of char;
    end;


const
  { Two constants to determine whether part of soket is for in or output }
  S_IN = 0;
  S_OUT = 1;

Type
  TSockAddr = packed Record
    {$ifdef BSD}
     len : byte;
     family:byte;
    {$ELSE}
     family:word;  { was byte, fixed }
    {$ENDIF}
    data  :array [0..13] of char;
    end;

  TInetSockAddr = packed Record
    family:Word;
    port  :Word;
    addr  :Cardinal;
    pad   :array [1..8] of byte; { to get to the size of sockaddr... }
    end;

  TSockArray = Array[1..2] of Longint;

{--------------------------------------------------------------------------}
{$ifdef dolibc}
  function getservbyname (name : pchar  ; protocol : pchar) : PServEnt;cdecl;
  function gethostbyname ( HostName : Pchar) : PHostEnt;cdecl;
  function gethostbyaddr ( Addr : pointer; Len : Longint; HType : Longint) : PHostEnt;cdecl;
  function inet_addr(ipaddr: pchar): ULONG; cdecl;
{$else}  
  function getservbyname (name : pchar  ; protocol : pchar) : PServEnt;
  function GetHostByName ( HostName : Pchar) : PHostEnt;
  function GetHostByAddr ( Addr : pointer; Len : Longint; HType : Longint) : PHostEnt;
  function inet_addr(ipaddr: pchar): ULONG;
{$endif}
  Function hton(Host : integer) : integer;
  Function ntohs(Net : integer) : integer;
{--------------------------------------------------------------------------}

Var
  SocketError:Longint;


{Basic Socket Functions}
Function Socket(Domain,SocketType,Protocol:Longint):Longint;
Function Send(Sock:Longint;Const Buf;BufLen,Flags:Longint):Longint;
Function Recv(Sock:Longint;Var Buf;BufLen,Flags:Longint):Longint;
Function Bind(Sock:Longint;Const Addr;AddrLen:Longint):Boolean;
Function Listen (Sock,MaxConnect:Longint):Boolean;
Function Accept(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
Function Connect(Sock:Longint;Const Addr;Addrlen:Longint):longint;
Function Shutdown(Sock:Longint;How:Longint):Longint;
Function GetSocketName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
Function GetPeerName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
Function SetSocketOptions(Sock,Level,OptName:Longint;Const OptVal;optlen:longint):Longint;
Function GetSocketOptions(Sock,Level,OptName:Longint;Var OptVal;Var optlen:longint):Longint;
Function SocketPair(Domain,SocketType,Protocol:Longint;var Pair:TSockArray):Longint;


Implementation

{******************************************************************************
                          Kernel Socket Callings
******************************************************************************}

{---------------------------------------------------------------------------}
{$ifdef dolibc}
  function getservbyname (name : pchar  ; protocol : pchar) : PServEnt; cdecl; external;
  function gethostbyname ( HostName : Pchar) : PHostEnt; cdecl; external;
  function gethostbyaddr ( Addr : Pointer; Len : Longint; HType : Longint) : PHostent ; cdecl; external;
  function inet_addr(ipaddr: pchar): ULONG; cdecl; external;
{$else}
  function getservbyname (name : pchar  ; protocol : pchar) : PServEnt;
  begin
    WriteLn('Warning - Dummy call issued (getservbyname)');

    GetServByname := nil;
  end; { func. GetServByName }
  
  function GetHostByName ( HostName : Pchar) : PHostEnt;
  begin
    WriteLn('Warning - Dummy call issued (gethostbyname)');

    GetHostByName := nil;
  end; { func. GetHostByName }
  
  function GetHostByAddr ( Addr : pointer; Len : Longint; HType : Longint) : PHostEnt;
  begin
    WriteLn('Warning - Dummy call issued (gethostbyaddr)');

    GetHostByAddr := nil;
  end; { func. GetHostbyAddr }

  function inet_addr(Ipaddr: pchar):ulong;
  begin
    WriteLn('Warning - Dummy call issued (inet_addr)');

    inet_addr:=0;
  end; { func. inet_addr }
{$endif}
Function hton (Host : integer) : integer;
 begin
  hton:=lo(host)*256+Hi(Host);
end;
Function ntohs(Net : integer) : integer;
begin
  ntohs:=lo(Net)*256+Hi(Net);
end;

{---------------------------------------------------------------------------}


{******************************************************************************
                          Basic Socket Functions
******************************************************************************}

Function socket(Domain,SocketType,Protocol:Longint):Longint;
begin
  Socket:=Do_Syscall(syscall_nr_socket,Domain,SocketType,Protocol);
  if Result < 0 then
   SocketError:=Errno
  else
   SocketError:=0;
end;

Function Send(Sock:Longint;Const Buf;BufLen,Flags:Longint):Longint;
begin
  Send:=do_syscall(syscall_nr_sendto,Sock,Longint(@Buf),BufLen,Flags,0,0);
  if Result < 0 then
   SocketError:=Errno
  else
   SocketError:=0;
end;

Function Recv(Sock:Longint;Var Buf;BufLen,Flags:Longint):Longint;
begin
  Recv:=do_syscall(syscall_nr_Recvfrom,Sock,Longint(@Buf),BufLen,Flags,0,0);
  if Result < 0 then
   SocketError:=Errno
  else
   SocketError:=0;
end;

Function Bind(Sock:Longint;Const Addr;AddrLen:Longint):Boolean;
begin
  Bind:=(do_syscall(syscall_nr_Bind,Sock,Longint(@Addr),AddrLen)=0);
  if NOT Result then
   SocketError:=Errno
  else
   SocketError:=0;
end;

Function Listen(Sock,MaxConnect:Longint):Boolean;
begin
  Listen:=(do_syscall(syscall_nr_Listen,Sock,MaxConnect,0)=0);
  if NOT Result then
   SocketError:=Errno
  else
   SocketError:=0;
end;

Function Accept(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
begin
  Accept:=do_syscall(syscall_nr_accept,Sock,longint(@Addr),longint(@AddrLen));
  If Accept<0 Then
    begin
      Accept:=-1;
      SocketError:=errno;
    end
      else SocketError := 0;
end;

Function Connect(Sock:Longint;Const Addr;Addrlen:Longint): Longint;

begin
  Connect:=do_syscall(syscall_nr_connect,Sock,longint(@Addr),AddrLen);
  if Result < 0 then
   SocketError:=Errno
  else
   SocketError:=0;
end;


Function Shutdown(Sock:Longint;How:Longint):Longint;
begin
  ShutDown:=do_syscall(syscall_nr_shutdown,Sock,How);
  if Result < 0 then
   SocketError:=Errno
  else
   SocketError:=0;
end;


Function GetSocketName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
begin
  GetSocketName:=do_syscall(syscall_nr_GetSockName,Sock,longint(@Addr),longint(@AddrLen));
  if Result < 0 then
   SocketError:=Errno
  else
   SocketError:=0;
end;



Function GetPeerName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
begin
  GetPeerName:=do_syscall(syscall_nr_GetPeerName,Sock,longint(@Addr),longint(@AddrLen));
  if Result < 0 then
   SocketError:=Errno
  else
   SocketError:=0;
end;



Function SetSocketOptions(Sock,Level,OptName:Longint;Const OptVal;optlen:longint):Longint;
begin
  SetSocketOptions:=do_syscall(syscall_nr_SetSockOpt,Sock,Level,OptName,Longint(@OptVal),OptLen,0);
  if Result < 0 then
   SocketError:=Errno
  else
   SocketError:=0;
end;



Function GetSocketOptions(Sock,Level,OptName:Longint;Var OptVal;Var optlen:longint):Longint;
begin
  GetSocketOptions:=do_syscall(syscall_nr_GetSockOpt,Sock,Level,OptName,Longint(@OptVal),OptLen,0);
  if Result < 0 then
   SocketError:=Errno
  else
   SocketError:=0;
end;



Function SocketPair(Domain,SocketType,Protocol:Longint;var Pair:TSockArray):Longint;
begin
  SocketPair:=do_syscall(syscall_nr_SocketPair,Domain,SocketType,Protocol,longint(@Pair),0,0);
  if Result < 0 then
   SocketError:=Errno
  else
   SocketError:=0;
end;

{******************************************************************************
                               UnixSock
******************************************************************************}

Procedure Str2UnixSockAddr(const addr:string;var t:TUnixSockAddr;var len:longint);
begin
  Move(Addr[1],t.Path,length(Addr));
  t.Family:=AF_UNIX;
  t.Path[length(Addr)]:=#0;
  Len:=Length(Addr)+3;
end;


Function DoAccept(Sock:longint;var addr:string):longint;
var
  UnixAddr : TUnixSockAddr;
  AddrLen  : longint;
begin
  AddrLen:=length(addr)+3;
  DoAccept:=Accept(Sock,UnixAddr,AddrLen);
  Move(UnixAddr.Path,Addr[1],AddrLen);
  SetLength(Addr,AddrLen);
end;



end.
