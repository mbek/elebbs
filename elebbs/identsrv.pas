unit IDENTSRV;
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
** IDENTD, Identification protocol server for EleBBS/W32 and EleBBS/2
**
** Copyright (c) 1998 - 2001 by Maarten Bekers
**
** Created : 17-Mar-2001
** Last update : 12-Dec-2001
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
uses TcpSrv;

var
  ident_ListenThreadEnded : BOOLEAN;
  IdentServer : TTcpServer;

procedure ident_InitServer;
procedure ident_SetupServer;

function ident_ExecuteConnection(P: pointer): Longint;
function ident_ServerThread(P: pointer): Longint;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Global,                     { needed for things like version# etc }
      Debug_U,                   { Debug-logging routines }
       Cases,                    { String routines }
        LongStr,                 { String/Integer routines }
         Crt,                    { Standard console i/o }
           Threads,              { multi-threaded support }
            ApTimer,             { Timing support }
             MemMan,             { memory allocation routines }
              ScrnU,             { Configuration screen routines }
               TcpCli,           { TCP/IP client routines }
                SockDef,         { TCP/IP definitions }
                 SockFunc,       { Standard TCP/IP routines }
                  GenCfg,        { Standard screen i/o routines }
                   ElLog_U,      { Standard logging }
                    WinTitle,    { Change the console window }
                     StUtils,    { String routines }
                      SysUtils,  { SysUtils }
                       JDates,   { Time and Date routines }
                        ReadCfg, { Configuration files }
                         CfgRec, { Configuration records }
                          WordStr,
                           SysVars,
                            esrv_u    { EleBBS server primitives }


                    {$IFDEF WIN32}
                      ,Windows   { Support routines (Sleep, etc) }
                    {$ENDIF}

                    {$IFDEF OS2}
                      ,Os2Base   { Support routines (Sleep, etc) }
                    {$ENDIF} ;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

const
  IdentOS           = 'UNIX';
  IdentUserName     = 'EleBBS';
  IdentSecsTimeOut  = 180;
  IdentSessions     : Longint = 0;

var
  IdentInfo   : record
                  MaxSessions : Longint;
                  ServerPort  : Longint;
                end; { record }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ident_SetupServer;
var ErrorStr: String;
begin
  {-- Create a server isntance ---------------------------------------------}
  IdentServer := TTcpServer.Create;

  {-- Make sure this server can be restarted multiple times ----------------}
  IdentServer.ReUseAddr := TRUE;

  {-- Now setup the server to listen on any address ------------------------}
  if NOT IdentServer.SetupServer(IdentInfo.ServerPort,
                                 ErrorStr,
                                 InAddr_Any) then FatalError(ErrorStr);

  {-- Make sure our server is not blocking ---------------------------------}
  IdentServer.Blocking := false;
end; { proc. ident_SetupServer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ident_InitServer;
begin
  {-- Setup the server info ------------------------------------------------}
  IdentInfo.MaxSessions := 200;
  IdentInfo.ServerPort := 113;
end; { proc. ident_InitServer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ident_ExecuteConnection(P: pointer): Longint;
var ReturnCode  : Longint;
    CurSlot     : Longint;
    IdentClient : TTcpClient;
    StartTime   : EventTimer;
    ATempStr    : AnsiString;
    TempStr     : String;

    PortOnServer: Longint;
    PortOnClient: Longint;
begin
  {-- Retrieve the current slot we got -------------------------------------}
  CurSlot := Longint(p);

  {-- Increase the session count -------------------------------------------}
  Inc(TotalSessions);
  Inc(CurrentSessions);
  Inc(IdentSessions);

  {-- Setup a client -------------------------------------------------------}
  IdentClient := TTcpClient.Create;
  IdentClient.SockHandle := Connections[CurSlot].ClientRC;

  {-- Update the server screen to tell were updated ------------------------}
  srvUpdateScreen := TRUE;

  {-- Now just answer all texts that come in -------------------------------}
  NewTimer(StartTime, Secs2Tics(SecsTimeOut));

  REPEAT
    {-- If we received a string, reset the timer ---------------------------}
    if IdentClient.RecvStrLn(ATempStr, false) then
     begin
       GenericSleep(0);
       TempStr := ATempStr;

       if TempStr <> '' then
        begin
          NewTimer(StartTime, Secs2Tics(SecsTimeOut));

          {$IFDEF WITH_DEBUG}
            DebugObj.DebugLog(logString, 'Command: '+TempStr);
          {$ENDIF}

          {-- Disassemble the ident daemon ---------------------------------}
          PortOnServer := FVal(ExtractWord(TempStr, 1, [','] + defExtractWord, false, false));
          PortOnClient := FVal(ExtractWord(TempStr, 2, [','] + defExtractWord, false, false));
          RaLog('>', 'Ident request for port ' + Fstr(PortOnServer) + ' / ' + FStr(PortOnClient));

          {-- We always know eachother -------------------------------------}
          IdentClient.SendStrLn(Format('%d, %d : USERID : %s : %s',
                                       [PortOnClient,
                                        PortOnServer,
                                        IdentOS,
                                        IdentUsername]));
        end { if }
     end
      else GenericSleep(10);

  UNTIL (NOT IdentClient.ConnectionAlive)
          OR (TimerExpired(StartTime));

  {-- Now log (if) that the connectioned timeout ---------------------------}
  if (TimerExpired(StartTime)) then
    begin
      RaLog('>', Format('[IDENTSRV] [%s] Connection timedout', [Connections[CurSlot].IpAddr]));
    end; { if }

  {-- Close down the connection --------------------------------------------}
  try
    ReturnCode:= SockShutdown(Connections[CurSlot].ClientRC, 01);
    ReturnCode:= SockClose(Connections[CurSlot].ClientRC);
  except
  end;

  {-- If it failed, this is fatal ------------------------------------------}
  if (ReturnCode <> 00) then
    begin
      FatalError('Shutdown of socket failed!');
    end; { if }


  {-- And log to the generic logging ---------------------------------------}
  RaLog('>', Format('[IDENTSRV] [%s] Connection closed', [Connections[CurSlot].IpAddr]));

  {-- Remove the slot entry ------------------------------------------------}
  Connections[CurSlot].IpAddr := '';
  Connections[CurSlot].StartTimeStr := '';
  Connections[CurSlot].ClientRC := -1;
  Connections[CurSlot].Name := '';

  {-- Update the current server screen -------------------------------------}
  Dec(CurrentSessions);
  Dec(IdentSessions);

  {-- Update the server screen to tell were updated ------------------------}
  srvUpdateScreen := TRUE;

  {-- And exit this thread -------------------------------------------------}
  ExitThisThread;
end; { func. ident_ExecuteConnection }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ident_ServerThread(P: pointer): Longint;
var ClientAddr  : PSockAddr;
    ClientRC    : Longint;

    ReturnCode  : Longint;
    Counter     : Longint;
    SelectedSlot: Longint;

    Temp        : Longint;
    Client      : String;
    TempStr     : String;
begin
  {-- And lets log to the server we have initialized -----------------------}
  RaLog('>', Format('EleBBS IDENTSRV %s firing up', [VersionID]));

  {-- setup this thread ----------------------------------------------------}
  ident_ListenThreadEnded := FALSE;

  {-- Currently we have no connections at all ------------------------------}
  for Counter := 01 to IdentInfo.MaxSessions do
    Connections[Counter].ServerThread := nil;

  {-- Setup the server -----------------------------------------------------}
  New(ClientAddr);
  FillChar(ClientAddr^, SizeOf(ClientAddr^), #0);

  {-- Set the variable -----------------------------------------------------}
  Temp := SockAddr_len;

  {-- And loop through it --------------------------------------------------}
  REPEAT
    {-- Make sure we can process all message queues ------------------------}
    IdentServer.DoSleep(1);

    REPEAT
      {-- Accept this connection -------------------------------------------}
      ClientRC := SockAccept(IdentServer.ServerHandle,
                             ClientAddr,
                             Temp);

      {-- Sleep ------------------------------------------------------------}
      Identserver.DoSleep(10);
    UNTIL (ClientRC <> -1) OR (TermProgram);

    {-- give up some time so we dont hog the cpu --------------------------}
    IdentServer.DoSleep(1);

    {-- Make sure that the calls aren't blocking --------------------------}
    SocksetBlockingIO(ClientRC, false);

    {-- now get a slot for our program to work in -------------------------}
    SelectedSlot := FindEmptySlot;

    {-- make sure we dont exceed the max. number of sessions --------------}
    if IdentSessions >= IdentInfo.MaxSessions then
      SelectedSlot := 0;

    {-- now act upon what we found ----------------------------------------}
    if NOT TermProgram then
      begin
        {-- Now get the client name ---------------------------------------}
        if NOT NoDnsLookup then
          Client := SockGetHostNameByAddr(@ClientAddr^.Sin_Addr)
            else Client := IdentServer.IpToStr(ClientAddr^.sIn_Addr);

        if ReturnCode = -1 then
          Client := 'unknown host';

        {-- do we have room for this user anyway? -------------------------}
        if SelectedSlot > 00 then
          begin
            {-- Dispose of the current thread if necessary -----------------}
            if Connections[SelectedSlot].ServerThread <> nil then
              Dispose(Connections[SelectedSlot].ServerThread, Done);

            {-- Setup the slot ---------------------------------------------}
            Connections[SelectedSlot].SrvType := srvIdent;
            Connections[SelectedSlot].Name := Client;
            Connections[SelectedSlot].IpAddr := IdentServer.IpToStr(ClientAddr^.sIn_Addr);
            Connections[SelectedSlot].StartTimeStr := TimeStr(true, false);
            Connections[SelectedSlot].ClientRC := ClientRC;
            Connections[SelectedSlot].SrvData := NIL;

            {-- Make sure its logged ---------------------------------------}
            RaLog('>', Format('[IDENTSRV] [%s] Connection opened', [Connections[SelectedSlot].IpAddr]));

            {-- Setup a new connection -------------------------------------}
            New(Connections[SelectedSlot].ServerThread, Init);

            {-- and fire up the handling thread ----------------------------}
            Connections[SelectedSlot].ServerThread.CreateThread(ExecEleStack,
                                                                @Ident_ExecuteConnection,
                                                                Pointer(SelectedSlot),
                                                                0);
          end
            else begin
                   {-- We are booked full - refuse this connection ---------}
                   Inc(TotalRefused);

                   {-- Default string --------------------------------------}
                   TempStr := 'Sorry, server too busy';

                   {-- Sorry, were full ------------------------------------}
                   ReturnCode := SockSend(ClientRC,
                                          @TempStr[01],
                                          Length(TempStr),
                                          0);

                   {-- Forcefully disconnect the client --------------------}
                   ReturnCode:= SockShutdown(ClientRC, 2);
                   SockClose(ClientRc);

                   {-- if we cant close it, sigabort the server ------------}
                   if (ReturnCode <> 00) then
                     begin
                       FatalError('Shutdown of socket failed!');
                     end; { if }
                 end; { if busy }
      end; { if not program Terminated }

  UNTIL (ReturnCode = -1) OR (TermProgram);

  {-- Dispose all memory ---------------------------------------------------}
  Dispose(ClientAddr);

  {-- The listener thread has exitted --------------------------------------}
  ident_ListenThreadEnded := true;

  {-- and exit this thread -------------------------------------------------}
  ExitThisThread;
end; { func. ident_ServerThread }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { identsrv }
