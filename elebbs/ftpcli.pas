unit FTPCLI;
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
{$H-}
(*
**
** FTP - File Transfer protocol component (RFC 959)
**
** Copyright (c) 1998, 99 by Maarten Bekers
**
** Created : 19-Dec-1998
** Last update : 23-May-1998
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses TcpCli, FileObj;

Const ConnectWaitSecs = 8;
      DataWaitSecs = 60;

type transfermodetype = (transAscii, transBinary);
     ftpTransferStatus= (transStart, transNormal, transDone);

     ShowLineProc     = procedure(S: ShortString);
     ReceiveDataProc  = procedure(FName: ShortString; S: ShortString; Status: ftpTransferStatus;
                                  var Dest_F: pFileObj;
                                  var DestPath: String);


type tFTPclient = class(tTcpClient)
       protected
        fUserName    : String;
        fPassword    : String;
        fLastError   : Longint;
        fTransfermode: transfermodetype;
        fShowLine    : ShowLineProc;
        fListData    : ShowLineProc;
        fRecvData    : ReceiveDataProc;

        procedure SetTransfermode(T: TransferModeType);
       public
        constructor Create;
        destructor  Destroy; override;

        procedure ShowAllText(Require: Boolean);

        property Username : String READ fUserName WRITE fUserName;
        property Password : String READ fPassword WRITE fPassword;
        property TransType: transfermodetype READ fTransferMode WRITE SetTransferMode;
        property ShowLine : ShowLineProc READ fShowLine WRITE fShowLine;
        property ListData : ShowLineProc READ fListData WRITE fListData;
        property RecvData : ReceiveDataProc READ fRecvdata WRITE fRecvData;

        function Logon: Boolean;


        function Cwd(Dir: String): Boolean;
        function CdUp: Boolean;
        function Quit: Boolean;
        function Rein: Boolean;
        function Dele(Filename: String): Boolean;
        function Rmd(Pathname: String): Boolean;
        function Mkd(Pathname: String): Boolean;
        function Pwd: Boolean;
        function Site(S: String): Boolean;
        function Syst: Boolean;
        function Stat(Pathname: String): Boolean;
        function Help(Cmd: String): Boolean;
        function Noop: Boolean;
        function Rename(OrigName, DestName: String): Boolean;

        function List(Pathname: String): Boolean;
        function Retr(PathName: String; var Dest_F: pFileObj; DestPath: String): Boolean;
        function FtpPort(ServerHandle: Longint): Longint;

        function CheckError(TestSet: Array of Word; var LastErr: Longint): Boolean;
     end; { tIRC client }


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 uses LongStr, WordStr, SysUtils,
       SockDef, SockFunc, StUtils, Global,
        TcpSrv, CurTime, ApTimer, ObjDec

        {$IFDEF WIN32}
          ,Windows
        {$ENDIF}

        {$IFDEF OS2}
          ,Os2Base
        {$ENDIF}  ;

var DataEndtime: Longint;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tFtpClient.Create;
begin
  inherited Create;

  UserName := '';
  Password := '';
  {$IFDEF WITH_FULL}
  contrObj^.SetStartTime;            { We use CurrentTime and it compares this date! }
  {$ENDIF}

  Showline := nil;
  Listdata := nil;
  Recvdata := nil;
end; { constructor Create }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tFtpClient.Destroy;
begin
  inherited Destroy;
end; { destructor Destroy }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFtpClient.ShowAllText(Require: Boolean);
var RecvStr  : AnsiString;
    Count    : Byte;
begin
  if Require then
    begin
      Count := 40;

      While (Count > 0) AND (NOT DataAvailable) AND (ConnectionAlive) do
        begin
          DoSleep(100);

          Dec(Count);
        end; { while }
    end; { require }

  while (DataAvailable) AND (ConnectionAlive) do
    begin
      RecvStrLn(RecvStr, false);

      if Assigned(fShowLine) then
        fShowLine(RecvStr);

      if Require then BREAK;
    end; { while }

end; { proc. ShowAllText }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFtpClient.Logon: Boolean;
var AbortLogon: Boolean;
begin
  AbortLogon := false;
  Result := true;

  {-- Clear all pending input -------------------------------------------------}
  ShowAllText(true);

  {-- Send the username -------------------------------------------------------}
  if UserName <> '' then
     begin
       SendStrLn('USER '+UserName);

       {$IFNDEF VER1_1_0}
         if CheckError([530, 500, 501], fLastError) then
           AbortLogon := true;
       {$ENDIF}

       ShowAllText(true);
     end; { if }

  {-- Send the password -------------------------------------------------------}
  if PassWord <> '' then
     begin
       SendStrLn('PASS '+Password);

       {$IFNDEF VER1_1_0}
         if CheckError([530, 500, 501, 503], fLastError) then
           Abortlogon := true;
       {$ENDIF}

       ShowAllText(true);
     end; { if }

  if AbortLogon then Result := false;
  ShowAllText(true);
  FlushData;                                      { Flush the banner text }
end; { func. Logon }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFtpClient.Cwd(Dir: String): Boolean;
var TempStr: AnsiString;
begin
  {-- Remove the trailing (back)space -}
  if Dir[Length(Dir)] in ['/', '\'] then
    Delete(Dir, Length(Dir), 1);

  SendStrLn('CWD ' + Dir);

  {$IFNDEF VER1_1_0}
    if CheckError([501, 501, 502, 530, 550], fLastError) then Result := false
      else Result := true;
  {$ENDIF}

  RecvStrLn(TempStr, false);
end; { func. Cwd }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFtpClient.CdUp: Boolean;
begin
  SendStrLn('CDUP');

  {$IFNDEF VER1_1_0}
    if CheckError([501, 501, 502, 530, 550], fLastError) then Result := false
      else Result := true;
  {$ENDIF}

  ShowAllText(true);
end; { func. CdUp }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFtpClient.Rein: Boolean;
begin
  SendStrLn('REIN');

  {$IFNDEF VER1_1_0}
    if CheckError([500, 502], fLastError) then Result := false
      else Result := true;
  {$ENDIF}

  ShowAllText(true);
end; { func. Rein }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFtpClient.Quit: Boolean;
begin
  SendStrLn('QUIT');

  {$IFNDEF VER1_1_0}
    if CheckError([500], fLastError) then Result := false
      else Result := true;
  {$ENDIF}

  ShowAllText(true);
end; { func. Quit }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFtpClient.SetTransfermode(T: TransferModeType);
begin
   Case T of
     transAscii  : SendStrLn('TYPE A');
     transBinary : SendStrLn('TYPE I');
   end; { case }

  fTransfermode := T;
  ShowAllText(true);
end; { func. TransType }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFtpClient.Dele(Filename: String): Boolean;
begin
  SendStrLn('DELE '+FileName);

  {$IFNDEF VER1_1_0}
    if CheckError([500, 501, 502, 530], fLastError) then Result := false
      else Result := true;
  {$ENDIF}

  ShowAllText(true);
end; { func. Dele }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFtpClient.Rmd(Pathname: String): Boolean;
begin
  SendStrLn('RMD '+PathName);

  if CheckError([500, 501, 502, 530, 550], fLastError) then Result := false
    else Result := true;

  ShowAllText(true);
end; { func. Rmd }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFtpClient.Mkd(Pathname: String): Boolean;
begin
  SendStrLn('MKD '+PathName);

  {$IFNDEF VER1_1_0}
    if CheckError([500, 501, 502, 530, 550], fLastError) then Result := false
      else Result := true;
  {$ENDIF}

  ShowAllText(true);
end; { func. Mkd }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFtpClient.Pwd: Boolean;
begin
  SendStrLn('PWD');

  {$IFNDEF VER1_1_0}
    if CheckError([500, 501, 502, 550], fLastError) then Result := false
      else Result := true;
  {$ENDIF}

  ShowAllText(true);
end; { func. Pwd }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFtpClient.Site(S: String): Boolean;
begin
  SendStrLn('SITE '+ S);

  {$IFNDEF VER1_1_0}
    if CheckError([500, 501, 530], fLastError) then Result := false
      else Result := true;
  {$ENDIF}

  ShowAllText(true);
end; { func. Site }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFtpClient.Syst: Boolean;
begin
  SendStrLn('SYST');

  {$IFNDEF VER1_1_0}
    if CheckError([500, 501, 502], fLastError) then Result := false
      else Result := true;
  {$ENDIF}

  ShowAllText(true);
end; { func. Syst }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFtpClient.Stat(PathName: String): Boolean;
begin
  SendStrLn('STAT '+PathName);

  {$IFNDEF VER1_1_0}
    if CheckError([500, 501, 502], fLastError) then Result := false
      else Result := true;
  {$ENDIF}

  ShowAllText(true);
end; { func. Stat }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFtpClient.Help(Cmd: String): Boolean;
begin
  SendStrLn('HELP '+Cmd);

  {$IFNDEF VER1_1_0}
    if CheckError([500, 501, 502], fLastError) then Result := false
      else Result := true;
  {$ENDIF}

  ShowAllText(true);
end; { func. Help }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFtpClient.Noop: Boolean;
begin
  SendStrLn('NOOP');

  {$IFNDEF VER1_1_0}
    if CheckError([500], fLastError) then Result := false
      else Result := true;
  {$ENDIF}

  ShowAllText(true);
end; { func. Noop }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFtpClient.FtpPort(ServerHandle: Longint): Longint;
var sAddr       : tSockAddr;
    DataPort    : Longint;
begin
  SockGetSockAddr(ServerHandle, sAddr);
  DataPort := SockHToNs(sAddr.sin_port);
  Result := DataPort;

  SockGetSockAddr(SockHandle, sAddr);

  With sAddr do
    SendStrLn(Format('PORT %d,%d,%d,%d,%d,%d',
                     [Sin_Addr.ClassA,
                      Sin_Addr.ClassB,
                      Sin_Addr.ClassC,
                      Sin_Addr.ClassD,
                      Hi(Word(DataPort)),
                      Lo(Word(DataPort))]));
  ShowAllText(true);
end; { proc. FtpPort }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function DataAbortFunc: Boolean;
begin
  DataAbortFunc := CurrentTime > DataEndTime;
end; { func. DataAbortFunc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFtpClient.List(Pathname: String): Boolean;
var Savetransfer: TransferModeType;
    DataPort    : Longint;
    DataServer  : TTcpServer;
    ConnectStr  : String;
    ErrorStr    : AnsiString;
begin
  ShowAllText(false);
  ErrorStr := '';

  SaveTransfer := fTransferMode;
  if SaveTransfer <> transAscii then
    TransType := transAscii;

  DataServer := TTcpServer.Create;
  DataServer.ReuseAddr := true;

  if DataServer.SetupServer(0, ConnectStr, InAddr_Any) then
    begin
      DataPort := FtpPort(DataServer.ServerHandle);

      SendStrLn('LIST '+PathName);
      ShowAllText(true);        { Expect information about data connection }

      DataEndTime := CurrentTime + ConnectWaitSecs;
      DataServer.Blocking := false;

      if DataServer.Accept({$IFDEF FPC}@{$ENDIF}DataAbortFunc) then
       begin
         DoSleep(25);
         if NOT DataServer.DataAvailable then
           DoSleep(25);

         While (DataServer.DataAvailable) do
           begin

              if DataServer.RecvStrLn(ErrorStr, false) then
                Listdata(ErrorStr)
                  else begin
                         if NOT DataServer.ConnectionAlive then BREAK;

                         DoSleep(15);
                       end; { if }

           end; { while }
       end { if }
         else begin
                ErrorStr := 'Accept() call failed: '+ConnectStr;
              end;
    end { if }
      else ErrorStr := 'SetupServer() call failed: '+ConnectStr;

  DataServer.Destroy;

  if SaveTransfer <> TransType then
    TransType := SaveTransfer;

  List := (ErrorStr = '');

  if ErrorStr <> '' then
    DebugLog('!', 'ErrorStr = '+ErrorStr);
end; { func. List }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFtpClient.Retr(Pathname: String; var Dest_F: pFileObj; DestPath: String): Boolean;
var Savetransfer: TransferModeType;
    DataPort    : Longint;
    DataServer  : TTcpServer;
    DataTimer   : EventTimer;
    ErrorStr    : AnsiString;
    TempStr     : AnsiString;
    Connectstr  : ShortString;
begin
  ShowAllText(false);
  ErrorStr := '';

  SaveTransfer := fTransferMode;
  if SaveTransfer <> transAscii then
    TransType := transAscii;

  DataServer := TTcpServer.Create;
  DataServer.ReuseAddr := true;

  if DataServer.SetupServer(0, ConnectStr, InAddr_Any) then
    begin
      DataPort := FtpPort(DataServer.ServerHandle);

      SendStrLn('RETR '+PathName);
      ShowAllText(true);        { Expect information about data connection }

      DataEndTime := CurrentTime + ConnectWaitSecs;
      DataServer.Blocking := false;

      if DataServer.Accept({$IFDEF FPC}@{$ENDIF}DataAbortFunc) then
       begin
         RecvData(Pathname, TempStr, transStart, Dest_F, DestPath);
         NewTimer(DataTimer, Secs2Tics(DataWaitSecs));

         While (DataServer.ConnectionAlive) AND (NOT TimerExpired(DataTimer)) do
           begin
             if DataServer.RecvString(TempStr, false) then
               begin
                  NewTimer(DataTimer, Secs2Tics(DataWaitSecs));

                  RecvData(Pathname, TempStr, transNormal, Dest_F, DestPath);

                  if Length(TempStr) > 512 then DoSleep(10);
               end { if }
                else DoSleep(50);
           end; { while }

         RecvData(Pathname, TempStr, transDone, Dest_F, DestPath);
       end { if }
         else begin
                ErrorStr := 'Accept() call failed';
              end;
    end { if }
      else ErrorStr := 'SetupServer() call failed';

  DataServer.Destroy;

  if SaveTransfer <> TransType then
    TransType := SaveTransfer;

  Retr := (ErrorStr = '');

  if ErrorStr <> '' then
    DebugLog('!', 'ErrorStr = '+ErrorStr);
end; { func. Retr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFtpClient.Rename(OrigName, DestName: String): Boolean;
begin
  Result := false;
  if (OrigName = '') OR (DestName = '') then EXIT;

  SendStrLn('RNFR ' + OrigName);

  {$IFNDEF VER1_1_0}
    if CheckError([450, 550, 500, 501, 502, 530, 421], fLastError) then Result := false
      else Result := true;
  {$ENDIF}

  if Result then ShowAllText(true);

  if Result then
    begin
      SendStrLn('RNTO ' + DestName);

      {$IFNDEF VER1_1_0}
        if CheckError([532, 553, 500, 501, 502, 503, 421, 530], fLastError) then Result := false
         else Result := true;
      {$ENDIF}

      ShowAllText(true);
    end; { if }
end; { func. Rename }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFtpClient.CheckError(TestSet: Array of Word; var LastErr: Longint): Boolean;
var Counter       : Longint;
    FoundResponse : Longint;
    TempStr       : AnsiString;
begin
  WaitForData;

  RecvStrLn(TempStr, true);
  Result := false;

  FoundResponse := FVal(ExtractWord(TempStr, 1, defExtractWord, false, false));

  for Counter := 00 to High(TestSet) do
    if FoundResponse = TestSet[Counter] then
     begin
       LastErr := TestSet[Counter];
       Result := true;
       RecvStrLn(TempStr, false);

       EXIT;
     end; { CheckReturn }

end; { func. CheckReturn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { ftpcli }
