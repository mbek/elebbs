{
    $Id: linsock.pas,v 1.1.1.1 2007/08/23 02:24:05 elebbs Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit LinSock;
{$i compiler.inc}
{$MODE objfpc}

Interface

{$ifdef dolibc}
  {$LINKLIB c}
{$endif}

uses SockDef,
       {$IFDEF VER1_0}
          Linux;
       {$ELSE}
	  BaseUnix,
          Unix,
          OldLinux;
       {$ENDIF}

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

Const
 { Socket Types }
  SOCK_STREAM     = 1;               { stream (connection) socket   }
  SOCK_DGRAM      = 2;               { datagram (conn.less) socket  }
  SOCK_RAW        = 3;               { raw socket                   }
  SOCK_RDM        = 4;               { reliably-delivered message   }
  SOCK_SEQPACKET  = 5;               { sequential packet socket     }
  SOCK_PACKET     =10;

  { Adress families }
  AF_UNSPEC       = 0;
  AF_UNIX         = 1;      { Unix domain sockets          }
  AF_INET         = 2;      { Internet IP Protocol         }
  AF_AX25         = 3;      { Amateur Radio AX.25          }
  AF_IPX          = 4;      { Novell IPX                   }
  AF_APPLETALK    = 5;      { Appletalk DDP                }
  AF_NETROM       = 6;      { Amateur radio NetROM         }
  AF_BRIDGE       = 7;       { Multiprotocol bridge         }
  AF_AAL5         = 8;       { Reserved for Werner's ATM    }
  AF_X25          = 9;       { Reserved for X.25 project    }
  AF_INET6        = 10;      { IP version 6                 }
  AF_MAX          = 12;

 {  Protocol Families }

  PF_UNSPEC       = AF_UNSPEC;
  PF_UNIX         = AF_UNIX;
  PF_INET         = AF_INET;
  PF_AX25         = AF_AX25;
  PF_IPX          = AF_IPX;
  PF_APPLETALK    = AF_APPLETALK;
  PF_NETROM       = AF_NETROM;
  PF_BRIDGE       = AF_BRIDGE;
  PF_AAL5         = AF_AAL5;
  PF_X25          = AF_X25;
  PF_INET6        = AF_INET6;

  PF_MAX          = AF_MAX;

const
  { Two constants to determine whether part of soket is for in or output }
  S_IN = 0;
  S_OUT = 1;

Type
  TSockAddr = packed Record
    family:word;  { was byte, fixed }
    data  :array [0..13] of char;
    end;

  TUnixSockAddr = packed Record
    family:word; { was byte, fixed }
    path:array[0..108] of char;
    end;

  TInetSockAddr = packed Record
    family:Word;
    port  :Word;
    addr  :Cardinal;
    pad   :array [1..8] of byte; { to get to the size of sockaddr... }
    end;

  TSockArray = Array[1..2] of Longint;

Var
  SocketError:Longint;

{Basic Socket Functions}
Function Socket(Domain,SocketType,Protocol:Longint):Longint;
Function Send(Sock:Longint;var Addr;AddrLen,Flags:Longint):Longint;
Function Recv(Sock:Longint;var Addr;AddrLen,Flags:Longint):Longint;
Function Bind(Sock:Longint;var Addr;AddrLen:Longint):Boolean;
Function Listen (Sock,MaxConnect:Longint):Boolean;
Function Accept(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
Function Connect(Sock:Longint;Var Addr;Addrlen:Longint):Longint;
Function Shutdown(Sock:Longint;How:Longint):Longint;
Function GetSocketName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
Function GetPeerName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
Function SetSocketOptions(Sock,Level,OptName:Longint;Var OptVal;optlen:longint):Longint;
Function GetSocketOptions(Sock,Level,OptName:Longint;Var OptVal;optlen:longint):Longint;
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


Const
  {
    Arguments to the Linux Kernel system call for sockets. All
    Socket Connected calls go through the same system call,
    with an extra argument to determine what action to take.
  }
  Socket_Sys_SOCKET      = 1;
  Socket_Sys_BIND        = 2;
  Socket_Sys_CONNECT     = 3;
  Socket_Sys_LISTEN      = 4;
  Socket_Sys_ACCEPT      = 5;
  Socket_Sys_GETSOCKNAME = 6;
  Socket_Sys_GETPEERNAME = 7;
  Socket_Sys_SOCKETPAIR  = 8;
  Socket_Sys_SEND        = 9;
  Socket_Sys_RECV        = 10;
  Socket_Sys_SENDTO      = 11;
  Socket_Sys_RECVFROM    = 12;
  Socket_Sys_SHUTDOWN    = 13;
  Socket_Sys_SETSOCKOPT  = 14;
  Socket_Sys_GETSOCKOPT  = 15;
  Socket_Sys_SENDMSG     = 16;
  Socket_Sys_RECVMSG     = 17;


Function SocketCall(SockCallNr,a1,a2,a3,a4,a5,a6:longint):longint;
var
  Regs:SysCallRegs;
  Args:array[1..6] of longint;
begin
  args[1]:=a1;
  args[2]:=a2;
  args[3]:=a3;
  args[4]:=a4;
  args[5]:=a5;
  args[6]:=a6;
  regs.reg2:=SockCallNr;
  regs.reg3:=Longint(@args);
  SocketCall:=Syscall(syscall_nr_socketcall,regs);
  if Result < 0 then
   SocketError:=Errno
  else
   SocketError:=0;
end;



Function SocketCall(SockCallNr,a1,a2,a3:longint):longint;
begin
  SocketCall:=SocketCall(SockCallNr,a1,a2,a3,0,0,0);
end;


{******************************************************************************
                          Basic Socket Functions
******************************************************************************}

Function socket(Domain,SocketType,Protocol:Longint):Longint;
begin
  Socket:=SocketCall(Socket_Sys_Socket,Domain,SocketType,ProtoCol);
end;



Function Send(Sock:Longint;var Addr;AddrLen,Flags:Longint):Longint;
begin
  Send:=SocketCall(Socket_Sys_Send,Sock,Longint(@Addr),AddrLen,Flags,0,0);
end;



Function Recv(Sock:Longint;var Addr;AddrLen,Flags:Longint):Longint;
begin
  Recv:=SocketCall(Socket_Sys_Recv,Sock,Longint(@Addr),AddrLen,Flags,0,0);
end;



Function Bind(Sock:Longint;Var Addr;AddrLen:Longint):Boolean;
begin
  Bind:=(SocketCall(Socket_Sys_Bind,Sock,Longint(@Addr),AddrLen)=0);
end;



Function Listen(Sock,MaxConnect:Longint):Boolean;
begin
  Listen:=(SocketCall(Socket_Sys_Listen,Sock,MaxConnect,0)=0);
end;



Function Accept(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
begin
  Accept:=SocketCall(Socket_Sys_Accept,Sock,longint(@Addr),longint(@AddrLen));
  If Accept<0 Then 
    Accept:=-1;
end;



Function Connect(Sock:Longint;Var Addr;Addrlen:Longint):Longint;

begin
  Connect:=SocketCall(Socket_Sys_Connect,Sock,longint(@Addr),AddrLen);
  If Connect<0 Then
    Connect:=-1;
end;



Function Shutdown(Sock:Longint;How:Longint):Longint;
begin
  ShutDown:=SocketCall(Socket_Sys_ShutDown,Sock,How,0);
end;



Function GetSocketName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
begin
  GetSocketName:=SocketCall(Socket_Sys_GetSockName,Sock,longint(@Addr),longint(@AddrLen));
end;



Function GetPeerName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
begin
  GetPeerName:=SocketCall(Socket_Sys_GetPeerName,Sock,longint(@Addr),longint(@AddrLen));
end;



Function SetSocketOptions(Sock,Level,OptName:Longint;Var OptVal;optlen:longint):Longint;
begin
  SetSocketOptions:=SocketCall(Socket_Sys_SetSockOpt,Sock,Level,OptName,Longint(@OptVal),OptLen,0);
end;



Function GetSocketOptions(Sock,Level,OptName:Longint;Var OptVal;optlen:longint):Longint;
begin
  GetSocketOptions:=SocketCall(Socket_Sys_GetSockOpt,Sock,Level,OptName,Longint(@OptVal),OptLen,0);
end;



Function SocketPair(Domain,SocketType,Protocol:Longint;var Pair:TSockArray):Longint;
begin
  SocketPair:=SocketCall(Socket_Sys_SocketPair,Domain,SocketType,Protocol,longint(@Pair),0,0);
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


{******************************************************************************
                               InetSock
******************************************************************************}



Function DoAccept(Sock:longint;Var addr:TInetSockAddr):longint;

Var AddrLen : Longint;

begin
  AddrLEn:=SizeOf(Addr);
  DoAccept:=Accept(Sock,Addr,AddrLen);
end;




end.

{
  $Log: linsock.pas,v $
  Revision 1.1.1.1  2007/08/23 02:24:05  elebbs
  Initial import after messing up repo :(

  Revision 1.2  2006/02/18 11:49:23  elebbs
  Used hardcoded path to the compiler include file

  Revision 1.1.1.1  2006/01/19 20:30:35  elebbs
  Initial import

  Revision 1.2  2003/12/26 16:13:56  ele
  New and updated EleWEB scripts
  Some FPC 1.0.10/Linux fixes

  Revision 1.1.1.1  2003/04/06 13:26:27  ele
  new import

  Revision 1.1.1.1  2003/04/05 21:52:32  ele
  Initial import

  Revision 1.7  1999/06/08 18:35:24  michael
  + Beter return values for connect and accept

  Revision 1.6  1999/06/08 18:19:24  michael
  + Fixes for connect calls

  Revision 1.5  1999/06/08 16:08:33  michael
  + completed (hopefully) Fix by stian (my_wave@ypsilonia.net)

  Revision 1.4  1999/06/08 16:05:08  michael
  + Fix by stian (my_wave@ypsilonia.net)

  Revision 1.3  1998/11/16 10:21:30  peter
    * fixes for H+

  Revision 1.2  1998/07/16 10:36:45  michael
  + added connect call for inet sockets

  Revision 1.1.1.1  1998/03/25 11:18:43  root
  * Restored version

  Revision 1.1  1998/02/13 08:35:05  michael
  + Initial implementation

}
