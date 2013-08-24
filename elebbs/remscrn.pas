unit REMSCRN;
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
** Remote screen taking over routines for EleBBS
**
** Copyright (c) 1996-1999 by Maarten Bekers
**
** Created : 02-Jul-1999
** Last update : 02-Jul-1999
**
** (how to f*ck up OOP :-)
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

{$IFDEF MSDOS}
  {$DEFINE USEFILE}
{$ENDIF}

{$IFDEF WIN32}
  {$DEFINE PIPES}
  {$UNDEF PIPES}
  {$DEFINE TCPIP}
{$ENDIF}

{$IFDEF ELEUNIX}
  {$DEFINE TCPIP}
{$ENDIF}

uses Global,
      CfgRec

       {$IFNDEF VirtualPascal}
         ,Use32
       {$ENDIF}

       {$IFDEF TCPIP}
         ,SockFunc
         ,SockDef
         ,TcpSrv
       {$ENDIF};

Const
      pipe_cntrl_ScrnUpdate  = 01;   { Notify that theres been a scrn update }
      pipe_cntrl_ReadKey     = 02;       { Treat following char as a readkey }
      pipe_cntrl_SetCursor   = 03;    { Set the cursor position as following }
      pipe_cntrl_PlayBeep    = 04;           { Play a beep on the remote end }
      pipe_cntrl_StatusBar   = 05;             { Ask for a certain statusbar }
      pipe_cntrl_IsStatus    = 06;                     { Sending a statusbar }
      pipe_cntrl_SysCommand  = 07;               { ALT-code send from remote }
      pipe_cntrl_SendLine    = 08;                   { Send a line at a time }
      pipe_cntrl_UpdateDone  = 09;               { The lines are all updated }
      pipe_cntrl_GivePassword= 10;          { Request for password from host }
      pipe_cntrl_IsPassword  = 11;                    { This is the password }
      pipe_cntrl_RefreshScrn = 12;  { Request from client for screen refresh }
      pipe_cntrl_NAK         = 255;               { NAK - abort all commands }


type RemoteUnknownProc = procedure(const Code   : Byte;
                                   var   TmpBuf : Array of Char;
                                   const Length : Byte;
                                   var   Counter: Longint);
type TRemoteScrnObj = Object
                        RemStartPort : Longint;
                        MaxPwdTries  : Longint;

                        StatRec    : record
                                       Number : Byte;
                                       Line1,
                                       Line2  : String[80];
                                     end; { record }

                        CmdBuf     : Array[0..3] of Char;
                        PwdWeWant  : String;
                        PwdEntered : String;
                        PwdTries   : Byte;
                        NodeNr     : Longint;
                        BytesRead,
                        BytesSent  : Longint;
                        IsValidated: Boolean;

                        {$IFDEF PIPES}
                          PipeInput  : Longint;
                          PipeOutput : Longint;
                          PipeHandle : Longint;
                        {$ENDIF}

                        {$IFDEF USEFILE}
                          PipeInput  : File;
                          PipeOutput : File;

                          InputPtr   : Longint;
                          OutputPtr  : Longint;
                        {$ENDIF}

                        {$IFDEF TCPIP}
                          PipeHandle : TTcpServer;
                        {$ENDIF}

                        InputCnt   : Longint;
                        InputBuf   : Array[0..99] of Char;
                        Connected  : Boolean;
                        WantRedraw : Boolean;
                        ScrnChanged: Boolean;
                        UnknownCode: RemoteUnknownProc;

                        constructor Init;
                        destructor Done;

                        procedure CreatePipe(UseNr: Longint);
                        function ListenPipe: Boolean;
                        function ConnectPipe(Server: String; UseNr: Longint): Boolean;
                        procedure SendToPipe(var Buf; Len: Longint);
                        procedure ReadFromPipe(var Buf; Len: Longint; var DidRead: NumreadType);
                        procedure InitVars;

                        procedure rem_GotoXY(X, Y, A: Byte);
                        procedure rem_SendKeypress(C: Char);
                        procedure rem_SendSysCommand(C: Char);
                        function rem_ReadKey: Char;
                        function rem_KeyPressed: Boolean;
                        procedure rem_GetInputBuffer;
                        procedure rem_DoBeep;
                        procedure rem_UpdateDone;
                        procedure rem_SendScreen(var Buf; Size: SmallWord);
                        procedure rem_RecvScreen(var Buf; Size: SmallWord);
                        procedure rem_SendLine(var Buf; Row, Size: SmallWord);
                        procedure rem_RecvLine(var Buf; Size: SmallWord);
                        procedure rem_GetInputSize(var Used: Numreadtype);
                        procedure rem_AskStatusBar(Status: Byte);
                        procedure rem_SendStatusBar(Status: Byte; Line1, Line2: String);
                        procedure rem_RecvStatusBar(var Line1, Line2: String; Len1, Len2: Byte);
                        procedure rem_AskForPassword;
                        procedure rem_ReceivePassword(Len: Byte);
                        procedure rem_SendPassword(Pwd: String);
                        procedure rem_RequestScrnUpdate;

                        procedure rem_CheckPurge;
                      end; { object }

     PRemoteScrnObj = ^TRemoteScrnObj;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

{$IFDEF MSDOS}
uses GenFile, Dos, LongStr, Debug_U
      {$IFDEF WITH_FULL}
        , Main
        , ScrnU
        , ElLog_U
        , StatusB
        , SysKey
      {$ENDIF};
{$ENDIF}

{$IFDEF GO32V2}
uses GenFile, Dos, LongStr, Debug_U
      {$IFDEF WITH_FULL}
        , Main
        , ScrnU
        , ElLog_U
        , StatusB
        , SysKey
      {$ENDIF};
{$ENDIF}

{$IFDEF WIN32}
uses Windows, LongStr, Debug_U
      {$IFDEF WITH_FULL}
        , Main
        , ScrnU
        , ElLog_U
        , StatusB
        , SysKey
      {$ENDIF};
{$ENDIF}

{$IFDEF OS2}
uses LongStr
     , Os2base, Debug_U
      {$IFDEF WITH_FULL}
        , Main
        , ScrnU
        , ElLog_U
        , StatusB
        , SysKey
      {$ENDIF};
{$ENDIF}

{$IFDEF ELEUNIX}
uses LongStr, Debug_U
      {$IFDEF WITH_FULL}
        , Main
        , ScrnU
        , ElLog_U
        , StatusB
        , SysKey
      {$ENDIF};
{$ENDIF}

const MaxPipeInstances = 10;
      DefaultPipeWait  = 1000;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

constructor TRemoteScrnObj.Init;
begin
  MaxPwdtries := 3;
  remStartPort := 8999;

  {$IFDEF PIPES}
    PipeInput := -1;
    PipeOutput := -1;
  {$ENDIF}

  {$IFDEF USEFILE}
    InputPtr := 0;
    OutputPtr := 0;
  {$ENDIF}

  {$IFDEF TCPIP}
    PipeHandle := TTcpServer.Create;
  {$ENDIF}

  InitVars;
  Connected := false;
  PwdTries := 0;
  PwdEntered := '';
end; { init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure TRemoteScrnObj.InitVars;
begin
  FillChar(StatRec, SizeOf(StatRec), #00);
  InputCnt := -1;
  WantRedraw := false;
  Unknowncode := nil;
  ScrnChanged := false;
  BytesRead := 0;
  BytesSent := 0;
  PwdWeWant := '';
  IsValidated := false;
end; { proc. rem_InitVars }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure TRemoteScrnObj.rem_GetInputSize(var Used: NumreadType);
begin
  Used := 0;
  if (NOT Connected) then EXIT;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'rem_GetInputSize (begin)');
  {$ENDIF}

  {$IFDEF PIPES}
    PeekNamedPipe(PipeInput,
                  nil,
                  0,
                  nil,
                  @Used,
                  nil);
  {$ENDIF}

  {$IFDEF USEFILE}
    InputPtr := FilePos(PipeInput);
    Used := (FileSize(PipeInput) - InputPtr);
  {$ENDIF}

  {$IFDEF TCPIP}

    if PipeHandle.DataAvailable then
      Used := 4
       else begin
              Used := 0;

              {$IFDEF WITH_DEBUG}
                 DebugObj.DebugLog(logTcpIp, 'rem_GetInputSize (before conn)');
              {$ENDIF}

              Connected := PipeHandle.ConnectionAlive;
            end; { if }
  {$ENDIF}

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'rem_GetInputSize (end)');
  {$ENDIF}
end; { proc. rem_GetInputSize }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure TRemoteScrnObj.CreatePipe(UseNr: Longint);
var TempStr    : String;
  {$IFDEF PIPES}
    Security   : TSECURITYATTRIBUTES;
  {$ENDIF}
begin
  NodeNr := UseNr;

  {$IFDEF PIPES}
    FillChar(Security, SizeOf(TSECURITYATTRIBUTES), 0);
    Security.nLength := SizeOf(TSECURITYATTRIBUTES);
    Security.lpSecurityDescriptor := nil;
    Security.bInheritHandle := true;

    TempStr := '\\.\PIPE\ELEBBS_NODE_INPUT_'+FStr(NodeNr) + #00;
    PipeInput :=
      CreateNamedPipe(@TempStr[1],
                      PIPE_ACCESS_DUPLEX OR FILE_FLAG_WRITE_THROUGH,
                      PIPE_TYPE_BYTE OR PIPE_READMODE_BYTE OR PIPE_NOWAIT,
                      MaxPipeInstances,
                      32768,
                      32768,
                      DefaultPipeWait,
                      @Security);

    TempStr := '\\.\PIPE\ELEBBS_NODE_OUTPUT_'+FStr(NodeNr) + #00;
    PipeOutput :=
      CreateNamedPipe(@TempStr[1],
                      PIPE_ACCESS_DUPLEX OR FILE_FLAG_WRITE_THROUGH,
                      PIPE_TYPE_BYTE OR PIPE_READMODE_BYTE OR PIPE_NOWAIT,
                      MaxPipeInstances,
                      32768,
                      32768,
                      DefaultPipeWait,
                      @Security);
  {$ENDIF}

  {$IFDEF USEFILE}
    Assign(PipeInput, '$ELE$INP.' + FStr(NodeNr));
    Assign(PipeOutput, '$ELE$OUT.' + FStr(NodeNr));

    {$i-} ReWrite(PipeInput, 1); {$i+}
    if IoResult > 0 then ;

    {$i-} ReWrite(PipeOutput, 1); {$i+}
    if IoResult > 0 then ;
  {$ENDIF}

  {$IFDEF TCPIP}
    PipeHandle.Blocking := false;
    PipeHandle.ReUseAddr := true;
    PipeHandle.SetupServer(RemStartPort + NodeNr, TempStr, InAddr_Any);
    PipeHandle.Blocking := true;
    PipeHandle.Blocking := false;
  {$ENDIF}
end; { proc. CreatePipe }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

function DefAbort: Boolean;
begin
  DefAbort := true;
end; { func. defAbort }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

function TRemoteScrnObj.ListenPipe: Boolean;
var LastError: Longint;
begin
  ListenPipe := true;
  if Connected then EXIT;
  ListenPipe := false;
  if PwdTries > MaxPwdTries then EXIT;
  IsValidated := false;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'rem_ListenPipe (begin)');
  {$ENDIF}

  {$IFDEF PIPES}
    ConnectNamedPipe(PipeInput, nil);
    LastError := GetLastError;

    Connected := (LastError = 535);
    ListenPipe := Connected;
  {$ENDIF}

  {$IFDEF USEFILE}
    if FileExist('$ELE$LST.'+FStr(NodeNr)) then Connected := true;
    ListenPipe := Connected;
  {$ENDIF}

  {$IFDEF TCPIP}
    Connected := PipeHandle.Accept({$IFDEF FPC}@{$ENDIF}DefAbort);
    Result := Connected;
  {$ENDIF}

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'rem_ListenPipe (end)');
  {$ENDIF}
end; { proc. ListenPipe }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

destructor TRemoteScrnObj.Done;
begin
  {$IFDEF PIPES}
    DisconnectNamedPipe(PipeHandle);
    if PipeHandle <> -1 then
      CloseHandle(PipeHandle);
  {$ENDIF}

  {$IFDEF USEFILE}
    {$i-}
      Close(PipeInput);
      Close(PipeOutput);

      Assign(PipeInput, '$ELE$LST.'+Fstr(NodeNr));
      Erase(PipeInput);

      Assign(PipeInput, '$ELE$INP.'+Fstr(NodeNr));
      Erase(PipeInput);

      Assign(PipeInput, '$ELE$OUT.'+Fstr(NodeNr));
      Erase(PipeInput);
    {$i+}
    if IoResult > 0 then ;
  {$ENDIF}

  {$IFDEF TCPIP}
    if PipeHandle.DoDisconnect then
      PipeHandle.Disconnect;

    PipeHandle.Free;
  {$ENDIF}
end; { done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

function TRemoteScrnObj.ConnectPipe(Server: String; Usenr: Longint): Boolean;
var TempStr : String;
  {$IFDEF PIPES}
    Security: TSECURITYATTRIBUTES;
  {$ENDIF}
begin
  NodeNr := UseNr;
  ConnectPipe := false;

  {$IFDEF PIPES}
    TempStr := '\\'+ Server + '\PIPE\ELEBBS_NODE_'+FStr(NodeNr) + #00;

    FillChar(Security, SizeOf(TSECURITYATTRIBUTES), 0);
    Security.nLength := SizeOf(TSECURITYATTRIBUTES);
    Security.lpSecurityDescriptor := nil;
    Security.bInheritHandle := true;

    PipeHandle := CreateFile(@TempStr[1],
                             GENERIC_READ or GENERIC_WRITE,
                             0,
                             @Security,
                             OPEN_EXISTING,
                             FILE_ATTRIBUTE_NORMAL OR FILE_FLAG_WRITE_THROUGH,
                             0);

    Result := (PipeHandle >= 0);

    if Result then Connected := true;
  {$ENDIF}

  {$IFDEF USEFILE}
    Connected := true;

    Assign(PipeInput, '$ELE$LST.' + FStr(NodeNr));
    ReWrite(PipeInput, 1);
    Close(Pipeinput);

    Assign(PipeInput, '$ELE$OUT.' + FStr(NodeNr));
    {$i-} Reset(PipeInput, 1); {$i+}
    if IoResult > 0 then Connected := false;

    Assign(PipeOutput, '$ELE$INP.' + FStr(NodeNr));
    {$i-} Reset(PipeOutput, 1); {$i+}
    if IoResult > 0 then Connected := false;

    ConnectPipe := Connected;
  {$ENDIF}

  {$IFDEF TCPIP}
     Connected := PipeHandle.ConnectToServer(Server, RemStartPort + NodeNr,
                                            TempStr);
     Connectpipe := Connected;
     IsValidated := true;
  {$ENDIF}
end; { func. ConnectPipe }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure TRemoteScrnObj.SendToPipe(var Buf; Len: Longint);
type BufType = Array[0..(10*1024) - 1] of Char;

var DidWrite : NumReadType;
    StartPos : Longint;
    LastError: Longint;
    CharBuf  : ^Buftype;
    IncCount : Longint;
begin
  if (NOT Connected) then EXIT;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'rem_SendToPipe (begin)');
  {$ENDIF}

  if Len = 0 then EXIT;
  Inc(BytesSent, Len);
  StartPos := 0;
  IncCount := 0;

  New(CharBuf);
  Move(Buf, CharBuf^, Len);

  {$IFDEF PIPES}
    repeat
      WriteFile(PipeHandle,
                CharBuf^[StartPos],
                Len,
                DidWrite,
                nil);

      LastError := Windows.GetLastError;
      Dec(Len, DidWrite);
      Inc(StartPos, DidWrite);
    until (Len <= 0) OR (LastError = 232);
  {$ENDIF}

  {$IFDEF USEFILE}
    BlockWrite(PipeOutput, CharBuf^, Len, DidWrite);
  {$ENDIF}

  {$IFDEF TCPIP}
    repeat
      PipeHandle.SendBuf(CharBuf^[StartPos], Len, DidWrite);

      if DidWrite > 0 then
        begin
          Dec(Len, DidWrite);
          Inc(StartPos, DidWrite);
        end; { if }

      if DidWrite < 0 then
        begin
          Inc(IncCount);
        end; { if }

    until (Len <= 0) OR (NOT Connected) OR (IncCount > 200);
  {$ENDIF}

  Dispose(CharBuf);


  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logTcpIp, 'rem_SendToPipe (end)');
  {$ENDIF}
end; { proc. SendToPipe }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure TRemoteScrnObj.ReadFromPipe(var Buf; Len: Longint; var DidRead: NumreadType);
type BufType = Array[0..(10*1024) - 1] of Char;

var StartPos : Longint;
    LastError: Longint;
    CharBuf  : ^Buftype;
    IncCount : Longint;
begin
  StartPos := 0;
  if (NOT Connected) then EXIT;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logTcpIp, 'rem_ReadFromPipe (begin)');
  {$ENDIF}

  New(CharBuf);
  Inc(BytesRead, Len);

 {$IFDEF PIPES}
   REPEAT
     ReadFile(PipeHandle,
              CharBuf^[StartPos],
              Len,
              DidRead,
              nil);

     LastError := GetLastError;
     Dec(Len, DidRead);

     Inc(StartPos, DidRead);
   UNTIL (Len <= 0) OR (LastError = 232);

   Move(CharBuf^, Buf, StartPos);
 {$ENDIF}

 {$IFDEF USEFILE}
   repeat
     BlockRead(PipeInput, CharBuf^[StartPos], Len, DidRead);

     Dec(Len, DidRead);
     Inc(StartPos, DidRead);
   until (Len <= 0);

   Move(CharBuf^, Buf, StartPos);
 {$ENDIF}

 {$IFDEF TCPIP}
   IncCount := 0;

   repeat
     PipeHandle.RecvBuf(CharBuf^[Startpos], Len, DidRead);

     if DidRead > 0 then
       begin
         Dec(Len, DidRead);
         Inc(StartPos, DidRead);

         IncCount := 0;
       end; { if }

     if DidRead <= 0 then
       begin
         Inc(IncCount);

         Connected := PipeHandle.ConnectionAlive;
         if NOT Connected then
           if PipeHandle.DoDisconnect then PipeHandle.Disconnect;

         PipeHandle.DoSleep(25);
       end; { if }

   until (Len <= 0) OR (NOT Connected) OR (IncCount > 200);

   Move(CharBuf^, Buf, StartPos);
 {$ENDIF}


  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logTcpIp, 'rem_ReadFromPipe (end)');
  {$ENDIF}

 Dispose(CharBuf);
end; { proc. ReadFromPipe }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure TRemoteScrnObj.rem_SendScreen(var Buf; Size: SmallWord);
var Counter: Longint;
begin
  if (NOT Connected) OR (NOT IsValidated) then EXIT;
  FillChar(CmdBuf, SizeOf(CmdBuf), #00);

  CmdBuf[0] := Chr(pipe_cntrl_ScrnUpdate);
  CmdBuf[1] := Chr(Hi(Size));
  CmdBuf[2] := Chr(Lo(Size));

  SendToPipe(CmdBuf, SizeOf(CmdBuf));
  SendToPipe(Buf, Size);
end; { proc. rem_SendScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure TRemoteScrnObj.rem_RecvScreen(var Buf; Size: SmallWord);
var DidRead: NumReadType;
begin
  if (NOT Connected) OR (NOT IsValidated) then EXIT;

  ReadFromPipe(Buf, Size, DidRead);
end; { proc. rem_RecvScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure TRemoteScrnObj.rem_SendLine(var Buf; Row, Size: SmallWord);
var Counter: Longint;
begin
  if (NOT Connected) OR (NOT IsValidated) then EXIT;
  FillChar(CmdBuf, SizeOf(CmdBuf), #00);

  CmdBuf[0] := Chr(pipe_cntrl_SendLine);
  CmdBuf[1] := Chr(Hi(Size));
  CmdBuf[2] := Chr(Lo(Size));
  CmdBuf[3] := Chr(Row);

  SendToPipe(CmdBuf, SizeOf(CmdBuf));
  SendToPipe(Buf, Size);
end; { proc. rem_SendLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure TRemoteScrnObj.rem_RecvLine(var Buf; Size: SmallWord);
var DidRead: NumReadType;
begin
  if (NOT Connected) OR (NOT IsValidated) then EXIT;

  ReadFromPipe(Buf, Size, DidRead);
end; { proc. rem_RecvScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure TRemoteScrnObj.rem_SendKeypress(C: Char);
begin
  if (NOT Connected) OR (NOT IsValidated) then EXIT;

  FillChar(CmdBuf, SizeOf(CmdBuf), #00);
  CmdBuf[0] := Chr(pipe_cntrl_ReadKey);
  CmdBuf[1] := C;

  SendToPipe(CmdBuf, SizeOf(CmdBuf));
end; { proc. rem_SendKeyPress }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure TRemoteScrnObj.rem_SendSysCommand(C: Char);
begin
  if (NOT Connected) OR (NOT IsValidated) then EXIT;

  FillChar(CmdBuf, SizeOf(CmdBuf), #00);
  CmdBuf[0] := Chr(pipe_cntrl_SysCommand);
  CmdBuf[1] := C;

  SendToPipe(CmdBuf, SizeOf(CmdBuf));
end; { proc. rem_SendSysCommand }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure TRemoteScrnObj.rem_GotoXY(X, Y, A: Byte);
begin
  if (NOT Connected) OR (NOT IsValidated) then EXIT;

  FillChar(CmdBuf, SizeOf(CmdBuf), #00);
  CmdBuf[0] := Chr(pipe_cntrl_SetCursor);
  CmdBuf[1] := Chr(X);
  CmdBuf[2] := Chr(Y);
  CmdBuf[3] := Chr(A);

  SendToPipe(CmdBuf, SizeOf(CmdBuf));
end; { proc. rem_GotoXY }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

function TRemoteScrnObj.rem_Keypressed: Boolean;
begin
  rem_KeyPressed := (InputCnt >= 0);
end; { func. rem_Keypressed }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

function TRemoteScrnObj.rem_ReadKey: Char;
begin
  if NOT rem_Keypressed then
    repeat
      {$IFDEF WIN32}
        Sleep(5);
      {$ENDIF}

      {$IFDEF OS2}
        DosSleep(5);
      {$ENDIF}

      {$IFDEF ELEUNIX}
        { !! Delay(5); }
      {$ENDIF}
    until rem_Keypressed;

  rem_ReadKey := InputBuf[0];
  Move(InputBuf[1], InputBuf[0], SizeOf(InputBuf) - 2);
  InputBuf[SizeOf(InputBuf) - 1] := #00;

  Dec(InputCnt);
end; { func. rem_ReadKey }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure TRemoteScrnObj.rem_GetInputBuffer;
var TmpBuf  : Array[0..3] of Char;
    DidRead : NumreadType;
    Counter : Longint;
begin
  if (NOT Connected) then EXIT;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logTcpIp, 'rem_GetInputBuffer (begin)');
  {$ENDIF}

  FillChar(TmpBuf, SizeOf(TmpBuf), pipe_cntrl_nak);

  rem_GetInputSize(DidRead);
  if DidRead < SizeOf(TmpBuf) then EXIT;

  ReadFromPipe(TmpBuf, SizeOf(TmpBuf), DidRead);
  if DidRead = 0 then EXIT;

  Counter := 0;
  While (Counter < DidRead) AND (Connected) do
    begin
      Case Ord(TmpBuf[Counter]) of
      {$IFDEF WITH_FULL}
        pipe_cntrl_StatusBar : if IsValidated then
                                 begin
                                   StatusDisplay(Ord(TmpBuf[Counter + 1]), false);
                                   rem_SendStatusBar(StatRec.Number,
                                                     StatRec.Line1,
                                                     StatRec.Line2);
                                 end; { statusbar }
        pipe_cntrl_SysCommand: if IsValidated then
                                 begin
                                   LocalCommand(TmpBuf[Counter + 1], LineCfg^.DosShellMsgTyp, true);
                                 end; { SysCommand }
      {$IFNDEF MSDOS}
       {$IFNDEF GO32V2}
        pipe_cntrl_RefreshScrn: if IsValidated then
                                  begin
                                    FillChar(OldScreen^, SizeOf(OldScreen^), #00);
                                    ScrnU.UpdateScreenBuffer(true);
                                  end; { Refresh the screen }
       {$ENDIF}
      {$ENDIF}
      {$ENDIF}
        pipe_cntrl_IsPassword: rem_ReceivePassword(Ord(TmpBuf[Counter + 1]));
        pipe_cntrl_Readkey   : if IsValidated then
                                 begin
                                   if InputCnt >= SizeOf(InputBuf) then
                                     if rem_ReadKey = #00 then
                                      rem_Readkey;

                                   Inc(InputCnt);
                                   InputBuf[InputCnt] := TmpBuf[Counter + 1];
                                 end; { if }
          else if @UnknownCode <> nil then
                    UnknownCode(Ord(TmpBuf[Counter]),
                                TmpBuf,
                                DidRead,
                                Counter);
      end; { case }

      Inc(Counter, 4);
    end; { while }

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logTcpIp, 'rem_GetInputBuffer (end)');
  {$ENDIF}

end; { proc. rem_GetinputBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure TRemoteScrnObj.rem_DoBeep;
begin
  if (NOT Connected) OR (NOT IsValidated) then EXIT;

  FillChar(CmdBuf, SizeOf(CmdBuf), #00);
  CmdBuf[0] := Chr(pipe_cntrl_Playbeep);

  SendToPipe(CmdBuf, SizeOf(CmdBuf));
end; { proc. rem_DoBeep }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure TRemoteScrnObj.rem_UpdateDone;
begin
  if (NOT Connected) OR (NOT IsValidated) then EXIT;

  FillChar(CmdBuf, SizeOf(CmdBuf), #00);
  CmdBuf[0] := Chr(pipe_cntrl_UpdateDone);

  SendToPipe(CmdBuf, SizeOf(CmdBuf));
end; { proc. rem_UpdateDone }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure TRemoteScrnObj.rem_CheckPurge;
begin
 {$IFDEF USEFILE}
   if Eof(PipeInput) then ReWrite(PipeInput, 1);
   if Eof(PipeOutput) then Rewrite(PipeOutput, 1);
 {$ENDIF}
end; { proc. rem_CheckPurge }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure TRemoteScrnObj.rem_AskStatusBar(Status: Byte);
begin
  if (NOT Connected) OR (NOT IsValidated) then EXIT;

  FillChar(CmdBuf, SizeOf(CmdBuf), #00);
  CmdBuf[0] := Chr(pipe_cntrl_StatusBar);
  CmdBuf[1] := Chr(Status);

  SendToPipe(CmdBuf, SizeOf(CmdBuf));
end; { proc. rem_AskStatusBar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure TRemoteScrnObj.rem_SendStatusBar(Status: Byte; Line1, Line2: String);
begin
  if (NOT Connected) OR (NOT IsValidated) then EXIT;

  FillChar(CmdBuf, SizeOf(CmdBuf), #00);
  CmdBuf[0] := Chr(pipe_cntrl_IsStatus);
  CmdBuf[1] := Chr(Status);
  CmdBuf[2] := Chr(Length(Line1));
  CmdBuf[3] := Chr(Length(Line2));

  SendToPipe(CmdBuf, SizeOf(CmdBuf));
  SendToPipe(Line1[1], Length(Line1));
  SendToPipe(Line2[1], Length(Line2));
end; { proc. rem_SendStatusBar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure TRemoteScrnObj.rem_RecvStatusBar(var Line1, Line2: String; Len1, Len2: Byte);
var DidRead: NumReadType;
begin
  if (NOT Connected) OR (NOT IsValidated) then EXIT;

  ReadFromPipe(Line1[1], Len1, DidRead);
  Line1[0] := Chr(DidRead);

  ReadFromPipe(Line2[1], Len2, DidRead);
  Line2[0] := Chr(DidRead);

end; { proc. rem_RecvStatusBar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure TRemoteScrnObj.rem_AskForPassword;
begin
  if (NOT Connected) then EXIT;

  FillChar(CmdBuf, SizeOf(CmdBuf), #00);
  CmdBuf[0] := Chr(pipe_cntrl_GivePassword);

  SendToPipe(CmdBuf, SizeOf(CmdBuf));
end; { proc. rem_AskForPassword }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure TRemoteScrnObj.rem_ReceivePassword(Len: Byte);
var DidRead: NumReadType;
begin
  if (NOT Connected) then EXIT;

  if Len > 0 then
    ReadFromPipe(PwdEntered[1], Len, DidRead)
      else DidRead := 0;
  PwdEntered[0] := Chr(DidRead);

  if PwdEntered <> PwdWeWant then
    begin
      Inc(PwdTries);

      {$IFDEF WITH_FULL}
       {$IFDEF TCPIP}
         RaLog('!', 'EleMON connection attempt from ' + PipeHandle.ConnectedIp);
         RaLog('!', 'Used password "'+PwdEntered+'" - incorrect, try #'+FStr(PwdTries));

         if PwdTries > MaxPwdTries then
           RaLog('!', 'Maximum password attempts for this session exceeded, shutting down EleMON module');
       {$ENDIF}
      {$ENDIF}

      {$IFDEF TCPIP}
        PipeHandle.Disconnect;
      {$ENDIF}
    end { if }
      else IsValidated := true;
end; { proc. rem_ReceivePassword }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure TRemoteScrnObj.rem_SendPassword(Pwd: String);
begin
  if (NOT Connected) then EXIT;

  FillChar(CmdBuf, SizeOf(CmdBuf), #00);
  CmdBuf[0] := Chr(pipe_cntrl_IsPassword);
  CmdBuf[1] := Chr(Length(Pwd));

  SendToPipe(CmdBuf, SizeOf(CmdBuf));
  SendToPipe(Pwd[1], Length(Pwd));
end; { proc. rem_SendPassword }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure TRemoteScrnObj.rem_RequestScrnUpdate;
begin
  FillChar(CmdBuf, SizeOf(CmdBuf), #00);
  CmdBuf[0] := Chr(pipe_cntrl_RefreshScrn);

  SendToPipe(CmdBuf, SizeOf(CmdBuf));
end; { proc. rem_RequestScrnUpdate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

end. { unit REMSCRN }
