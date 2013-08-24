unit TCPCLI;
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
** General TCP client component
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

uses SockFunc, SockDef, BufUnit, Sysutils;

type DebugLogProc = procedure(C: Char; S: ShortString);

const
  MaxBufSize = 32768;


type
  TRecvBuf = Array[0..MaxBufSize + 1] of Char;
  PRecvBuf = ^TRecvBuf;

type tTcpClient = class
       protected
         fReadBuf      : ^BufArrayObj;

         fSockHandle   : Longint;
         fDebugLog     : DebugLogProc;
         ClientAddr    : PSockAddr;

         function  GetDataAvailable: Boolean;
         function  GetConnection: Boolean;
         procedure SockBlockRead(var Buf; Blocklen: longint; var DidRead: LongInt);

       public
         DoDisconnect  : Boolean;
         HaveConnection: Boolean;

         constructor Create;
         destructor  Destroy; override;

         function ConnectToServer(Hostname: String; Port: Longint;
                                    var ErrorStr: String): Boolean;

         property SockHandle: Longint READ fSockHandle WRITE fSockHandle;
         property DataAvailable: Boolean READ GetDataAvailable;
         property ConnectionAlive: Boolean READ GetConnection;
         property DebugLog: DebugLogProc READ fDebugLog WRITE fDebugLog;

         procedure SendStrLn(S: String);
         procedure SendStr(S: String);
         procedure SendAnsiStrLn(S: AnsiString);
         procedure SendAnsiStr(S: AnsiString);
         procedure SendBuf(var Buf; Len: Longint; var DidSent: Longint);
         procedure RecvBuf(var Buf; Len: Longint; var DidRead: Longint);
         procedure Disconnect;
         procedure WaitForData;
         procedure FlushData;
         procedure DoSleep(MS: Longint);

         function  RecvStrLn(var S: AnsiString; DoPeek: Boolean): Boolean;
         function  RecvString(var S: AnsiString; DoPeek: Boolean): Boolean;
         function  ResolveAddr(S: String; var Dest: String): Boolean;
         function  IpToStr(Addr: tIn_Addr): String;
         function  ConnectedIp: String;
     end; { tTcpClient }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 uses longStr, WordStr
        {$IFDEF WIN32}
          ,W32Sock
          ,Windows
        {$ENDIF}

       {$IFDEF ELEUNIX}
         {$IFDEF VER1_0}
            ,Linux
         {$ELSE}
            ,Unix
            ,OldLinux
         {$ENDIF}
       {$ENDIF}

        {$IFDEF VirtualPascal}
          ,VpSysLow
        {$ENDIF}

        {$IFDEF WITH_DEBUG}
          ,Debug_u
        {$ENDIF};

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Bool2Str(B: Boolean): String;
begin
  if B then Result := 'Yes'
    else Result := 'No ';
end; { func. Bool2str }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EmptyDebugLog(C: Char; S: ShortString);
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, C + #32#32#32 + '"' + S + '"');
  {$ENDIF}
end; { proc. EmptyDebugLog }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tTcpClient.Create;
begin
  inherited Create;

  SockInit;
  SockHandle := -1;
  ClientAddr := nil;
  DoDisconnect := TRUE;
  HaveConnection := true;

  New(fReadBuf, Init(MaxBufSize));

  {$IFDEF FPC}
    DebugLog := @EmptyDebugLog;
  {$ELSE}
    DebugLog := EmptyDebugLog;
  {$ENDIF}
end; { constructor Create }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTcpClient.FlushData;
var Count  : Longint;
    Tempstr: AnsiString;
begin
  fReadBuf^.Clear;

  if NOT RecvString(TempStr, false) then  { was: RecvStrLn }
     Count := 40
      else Count := 80;

  While (ConnectionAlive) AND (Count >= 0) do
    begin
      if NOT RecvString(TempStr, false) then { was: RecvStrLn }
        begin
          Dec(Count);

          DoSleep(10);
        end; { while }
    end; { while }
end; { proc. FlushData }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tTcpClient.Destroy;
begin
  inherited Destroy;

  Dispose(fReadBuf, Done);
  DebugLog('0', 'Destroy class');
end; { destructor Destroy }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTcpClient.Disconnect;
begin
  {--------------------------------------------------------------------------}
  if SockHandle <> -1 then
   if DoDisconnect then
    begin
      SockShutDown(SockHandle, 2);
      SockClose(SockHandle);
    end; { if }

  if ClientAddr <> nil then Dispose(ClientAddr);

  ClientAddr := nil;
  SockHandle := -1;
  HaveConnection := true;
end; { proc. Disconnect }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tTcpClient.ConnectToServer(Hostname: String; Port: Longint;
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
  ReturnCode := SockSocket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

  if ReturnCode <> -1 then
    begin
      SockHandle := ReturnCode;

      IpAddr := SockInetAddr(HostName);
      {$IFDEF ELEUNIX}
      if IpAddr.IpAddr = ULONG(INADDR_NONE) then
      {$ELSE}
      if IpAddr.IpAddr = INADDR_NONE then
      {$ENDIF}
        begin
          IpAddr.IpAddr := SockGetHostAddrByName(HostName);
        end; { if }

      {$IFDEF ELEUNIX}
      if IpAddr.IpAddr <> ULONG(INADDR_NONE) then
      {$ELSE}
      if IpAddr.IpAddr <> INADDR_NONE then
      {$ENDIF}
        begin
          ClientAddr^.Sin_Addr.IPAddr := IpAddr.IpAddr;
          ClientAddr^.Sin_Port        := sockhtons(Port);
          ClientAddr^.Sin_Family      := AF_INET;

          ReturnCode := SockConnect(SockHandle,
                                    ClientAddr^);
          if ReturnCode = -1 then
            ErrorStr := 'Unable to connect (' + SockGetErrStr(SockErrorNo) + ')';
        end
         else ErrorStr := 'Unable to get ipaddress for hostname (' + SockGetErrStr(SockErrorNo) + ')';
    end
     else ErrorStr := 'Unable to intiailize socket (' + SockGetErrStr(SockErrorNo) + ')';

  SocksetBlockingIO(SockHandle, false);
  Result := (ErrorStr = '');
  HaveConnection := Result;

  DebugLog('0', 'ConnectToServer, ErrorStr='+ErrorStr);
  DebugLog('0', 'ConnectToServer, result='+Bool2Str(Result));
end; { func. ConnectToServer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tTcpClient.GetDataAvailable: boolean;
begin
  Result := fReadBuf^.BufUsed > 0;

  if NOT Result then
    begin
      Result := SockDataAvail(SockHandle);
    end; { if }
end; { func. GetDataAvailable }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tTcpClient.GetConnection: Boolean;
var TempCH     : Char;
    Returncode : longint;
    TempError  : longint;
{$IFDEF ELEUNIX}
  var WriteFDS : FDSet;
      Temp     : Longint;
{$ENDIF}
begin
  if NOT HaveConnection then
    begin
      Result := false;
      EXIT;
    end; { if }

  Result := true;

  {$IFNDEF ELEUNIX}
    ReturnCode := SockRecv(SockHandle, @TempCH, SizeOf(TempCH), MSG_PEEK);
    TempError := SockErrorNo;

    if ((TempError <> WSAEWOULDBLOCK) AND (TempError <> 00)) OR (ReturnCode=0) then
      begin
        Result := false;
        HaveConnection := false;
      end; { if }
  {$ELSE}
     fd_Zero(WriteFDS);
     fd_Set(SockHandle, WriteFDS);

     Temp := Select(SockHandle + 1, nil, @WriteFDS, nil, 0);
     if (Temp > 0) then
       begin
         Result := FD_ISSET(Sockhandle, WriteFDS);
         HaveConnection := Result;
       end { if }
         else begin
                Result := false;
                HaveConnection := false;
              end; { else }
  {$ENDIF}
end; { func. GetConnection }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTcpClient.SockBlockRead(var Buf; Blocklen: longint; var DidRead: LongInt);
var ReadLen : Longint;
    WasEmpty: Boolean;
begin
  DebugLog('^', 'SockBlockRead(BlockLen='+FStr(BlockLen)+'); (begin)');
  DebugLog('^', 'SockDataAvailable = '+Bool2Str(DataAvailable));

  {-- Initialize variables -------------------------------------------------}
  DidRead := 00;

  {-- if no data is to be expected, exit -----------------------------------}
  if NOT DataAvailable then EXIT;

  {-- try reading 'x' bytes ------------------------------------------------}
  ReadLen := SockRecv(SockHandle, @Buf, BlockLen, 00);
  if ReadLen > 0 then Inc(DidRead, ReadLen);

  DebugLog('^', 'SockBlockRead (errno='+FStr(SockErrorNo)+')');
  DebugLog('^', 'SockBlockRead(BlockLen='+FStr(BlockLen)+'); ( end ), read='+FStr(DidRead)+' bytes');
end; { proc. BlockRead }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tTcpClient.RecvStrLn(var S: AnsiString; DoPeek: Boolean): Boolean;

function ScanFor(var TmpChar: BufArrayObj; Ch: Char): Longint;
var Counter: Longint;
begin
  ScanFor := -1;
  Counter := 0;

  while (Counter < TmpChar.BufUsed) do
    begin
{!!!!!      if (TmpChar.TxtArr^[Counter + TmpChar.TxtStartPtr] = CH) then }
      if (TmpChar.TxtArr[Counter + TmpChar.TxtStartPtr] = CH) then
        begin
          ScanFor := Counter + 1;
          BREAK;
        end
          else Inc(Counter);
    end; { while }
end; { func. ScanFor }

var TmpChar    : PRecvBuf;
    LfPos      : Longint;
    TmpShortStr: String[250];
    BytesRead  : Longint;
    RecvCount  : Longint;
    TempCh     : Char;
begin
  {-- initialize some variables --------------------------------------------}
  RecvStrLn  := FALSE;
  S := '';
  New(TmpChar);

  {-- log some info --------------------------------------------------------}
  DebugLog('<', 'RecvStrLn. DoPeek='+Bool2Str(DoPeek));

  {-- first check if we dont have a #10 in our buffer ----------------------}
  LfPos := ScanFor(fReadBuf^, #10);

  {-- log some info --------------------------------------------------------}
  DebugLog('<', 'RecvStrLn. LfPos (01)='+FStr(LfPos));

  {-- if we are about to exceed the maximum buffer, and still no LF found --}
  {-- we just return the first 32768 characters ----------------------------}
  if fReadBuf^.BufUsed >= (MaxBufSize - 512) then
    LfPos := fReadBuf^.BufUsed;

  {-- log some info --------------------------------------------------------}
  DebugLog('<', 'RecvStrLn. LfPos (02)='+FStr(LfPos));

  {-- if no enter found, and our buffer is far from full, read some more ---}
  {-- (the buffer full handling is  done earlier! --------------------------}
  if LfPos <= 0 then
    begin
      {-- loop a max of 15 timess ------------------------------------------}
      RecvCount := 15;
      BytesRead := 1;

      while (RecvCount > 0) AND (BytesRead > 0) do
        begin
          {-- read some 150 bytes --------------------------------------------}
          SockBlockRead(TmpShortStr[1], 150, BytesRead);
          TmpShortStr[0] := Chr(BytesRead);

          {-- now add this ---------------------------------------------------}
          if BytesRead > 0 then
            begin
             fReadBuf^.Put(TmpShortStr[1], BytesRead);

             {-- and rescan for an enter -------------------------------------}
             LfPos := ScanFor(fReadBuf^, #10);
           end { if }
             else DoSleep(150);                                { give up timeslice }

          Dec(RecvCount);
        end; { while }
    end; { if }

  {-- log some info --------------------------------------------------------}
  DebugLog('<', 'RecvStrLn. LfPos (03)='+FStr(LfPos));

  {-- and -if found- return the string with cr/lf --------------------------}
  if LfPos > 0 then
    begin
      {-- we have a #10 in our buffer --------------------------------------}
      {-- now copy the string ----------------------------------------------}
      fReadBuf^.Get(TmpChar^[0], LfPos, NOT DoPeek);

      {-- delete the #10 itself --------------------------------------------}
      TmpChar^[LfPos - 1] := #0;


      {-- and convert it to an string --------------------------------------}
      S := SysUtils.StrPas(TmpChar^);

      {-- if the last character is #13, kill it ----------------------------}
      if Pos(#13, S) = Length(s) then
        Delete(S, Length(s), 1);

      {-- if the next character is #13, kill it too ------------------------}
      fReadBuf^.Get(TempCH, 1, FALSE);

      if TempCH = #13 then
        fReadBuf^.Get(TempCH, 1, TRUE);

      {-- were done --------------------------------------------------------}
      RecvStrLn := TRUE;
    end; { if }

  {-- log some info --------------------------------------------------------}
  DebugLog('<', 'RecvStrLn. End (TxtStartPtr)='+FStr(fReadBuf^.TxtStartPtr));
  DebugLog('<', 'RecvStrLn. End (BufUsed)='+FStr(fReadBuf^.BufUsed));

 if NOT DoPeek then DebugLog('<', 'RecvStrLn = "'+S+'"')
    else DebugLog('#', 'RecvStrLn = "'+S+'"');

  {-- and clean up ---------------------------------------------------------}
  Dispose(TmpChar);
end; { proc. RecvStrLn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tTcpClient.RecvString(var S: AnsiString; DoPeek: Boolean): Boolean;
var BytesRead  : Longint;
    TmpChar    : PRecvBuf;
    LfPos      : Longint;
begin
  DebugLog('<', 'RecvString. DoPeek='+Bool2Str(DoPeek));

  New(TmpChar);
  if fReadBuf^.BufUsed > 0 then
    begin
      DebugLog('<', 'RecvString. using freadBuf');

      {-- now copy the string ----------------------------------------------}
      LfPos := fReadBuf^.Get(TmpChar^, 200, NOT DoPeek);
      TmpChar^[LfPos] := #0;

      {-- and convert it to an string --------------------------------------}
      S := SysUtils.StrPas(TmpChar^);
    end
     else begin
            SetLength(S, 200);
            SockBlockRead(S[01], 200, BytesRead);
            SetLength(S, BytesRead);
          end; { if }

  Result := (S <> '');

  if NOT DoPeek then DebugLog('<', S);

  {-- and clean up ---------------------------------------------------------}
  Dispose(TmpChar);
end; { func. RecvString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTcpClient.SendStr(S: String);
var BytesSnt: Longint;
    TmpByte : Longint;
    BufFlag : Longint;
    TmpError: Longint;
begin
  BufFlag := 00;
  TmpByte := 01;

  REPEAT
    BytesSnt := SockSend(SockHandle,
                         @S[TmpByte],
                         Length(s),
                         BufFlag);

   { A non-blocking socket operation could not be completed immediately. }
   TmpError := SockErrorNo;
   if TmpError > 0 then
     begin
       if TmpError <> 10035 then
         TmpByte := Length(s) + 1;
     end; { if }


   if BytesSnt > 0 then
     Inc(TmpByte, BytesSnt);

   if BytesSnt <= 0 then        { if no bytes could be sent, wait for a while }
     DoSleep(10);

  UNTIL (TmpByte > Length(s));

  {$IFDEF FreeBSD}
    if TmpError > 0 then
      if TmpError <> 10035 then
        HaveConnection := false;
  {$ENDIF}

  DebugLog('>', S);
end; { proc. SendStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTcpClient.SendAnsiStrLn(S: AnsiString);
begin
  S := S + #13#10;
  SendAnsiStr(S);
end; { proc. SendAnsiStrLn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTcpClient.SendAnsiStr(S: AnsiString);
var BytesSnt: Longint;
    TmpByte : Longint;
    BufFlag : Longint;
    TmpError: Longint;
begin
  BufFlag := 00;
  TmpByte := 01;

  REPEAT
    BytesSnt := SockSend(SockHandle,
                         @S[TmpByte],
                         Length(s),
                         BufFlag);

   { A non-blocking socket operation could not be completed immediately. }
   TmpError := SockErrorNo;
   if TmpError > 0 then
     begin
       if TmpError <> 10035 then
         TmpByte := Length(s) + 1;
     end; { if }

   if BytesSnt > 0 then
     Inc(TmpByte, BytesSnt);

   if BytesSnt <= 0 then        { if no bytes could be sent, wait for a while }
     DoSleep(10);
  UNTIL (TmpByte > Length(s));

  {$IFDEF FreeBSD}
    if TmpError > 0 then
      if TmpError <> 10035 then
        HaveConnection := false;
  {$ENDIF}

  DebugLog('>', S);
end; { proc. SendAnsiStrLn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTcpClient.SendStrLn(S: String);
var BytesSnt: Longint;
    TmpByte : Longint;
    BufFlag : Longint;
    TmpError: Longint;
begin
  TmpByte := 01;
  BufFlag := 00;
  S := S + #13#10;

  REPEAT
    BytesSnt := SockSend(SockHandle,
                         @S[TmpByte],
                         Length(s),
                         BufFlag);

   { A non-blocking socket operation could not be completed immediately. }
   TmpError := SockErrorNo;
   if TmpError > 0 then
     begin
       if TmpError <> 10035 then
         TmpByte := Length(s) + 1;
     end; { if }

   if BytesSnt > 0 then
     Inc(TmpByte, BytesSnt);

   if BytesSnt <= 0 then        { if no bytes could be sent, wait for a while }
     DoSleep(10);

  UNTIL (TmpByte > Length(s));

  {$IFDEF FreeBSD}
    if TmpError > 0 then
      if TmpError <> 10035 then
        HaveConnection := false;
  {$ENDIF}

  DebugLog('>', S);
end; { proc. SendStrLn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTcpClient.SendBuf(var Buf; Len: Longint; var DidSent: Longint);
var BufFlag: Longint;
begin
  BufFlag := 0;

  DidSent := SockSend(SockHandle,
                      @Buf,
                      Len,
                      BufFlag);
end; { proc. SendBuf }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTcpClient.RecvBuf(var Buf; Len: Longint; var DidRead: Longint);
var BufFlag: Longint;
begin
  BufFlag := 0;

  DidRead := SockRecv(SockHandle,
                      @Buf,
                      Len,
                      BufFlag);
end; { proc. RecvBuf }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTcpClient.WaitForData;
var Count    : Longint;
    TempStr  : AnsiString;
begin
  Count := 6000;
  TempStr := '';

  While (Count > 0) AND (TempStr = '') AND (ConnectionAlive) do
    begin
      Dec(Count);

      if NOT RecvStrLn(TempStr, true) then TempStr := '';

      if TempStr = '' then
        DoSleep(10);
    end; { while }
end; { proc. WaitForData }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tTcpClient.ResolveAddr(S: String; var Dest: String): Boolean;
var TempAddr: tIn_Addr;
begin
  Dest := '';
  Result := false;

  if S[1] in ['0'..'9'] then
   begin
     TempAddr := SockInetAddr(s);

     Dest := SockGetHostNameByAddr(@TempAddr);
   end
     else begin
            TempAddr.IpAddr := SockGetHostAddrByName(s);
            Dest := IpToStr(TempAddr);
          end; { if }

  if (Dest <> 'amnesiac') AND (Dest <> '') then
    Result := true;
end; { func. ResolveAddr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tTcpClient.IpToStr(Addr: tIn_Addr): String;
var ValStr : String;
    TempStr: String;
begin
  TempStr := '';

  Str(Addr.ClassA, ValStr);
  TempStr := TempStr + ValStr + '.';
  Str(Addr.ClassB, ValStr);
  TempStr := TempStr + ValStr + '.';
  Str(Addr.ClassC, ValStr);
  TempStr := TempStr + ValStr + '.';
  Str(Addr.ClassD, ValStr);
  TempStr := TempStr + ValStr;

  Result := TempStr;
end; { func. IpToStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tTcpClient.ConnectedIp: String;
begin
  if ClientAddr <> nil then
    ConnectedIp := IpToStr(ClientAddr^.sIn_Addr)
      else ConnectedIp := '(n/a)';
end; { func. ConnectedIp }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTcpClient.DoSleep(MS: Longint);
begin
  {$IFDEF FPC}
    {$IFDEF WIN32}
      Windows.Sleep(ms);
    {$ENDIF}

    {$IFDEF OS2}
      DosSleep(ms);
    {$ENDIF}
  {$ENDIF}

  {$IFDEF VirtualPascal}
    SysCtrlSleep(Ms);
  {$ENDIF}

  {$IFDEF WINGUI}
    Sleep(ms);
  {$ENDIF}

  {$IFDEF ELEUNIX}
    Select(0, nil, nil, nil, MS);
  {$ENDIF}
end; { proc. DoSleep }


end. { tcpcli }
