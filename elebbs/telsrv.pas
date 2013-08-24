unit TELSRV;
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
** TELSRV, Telnet server for EleBBS/W32 and EleBBS/2
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
  telnet_ListenThreadEnded : BOOLEAN;
  TelnetServer : TTcpServer;

procedure telnet_InitServer;
procedure telnet_SetupServer;

function telnet_ExecuteConnection(P: pointer): Longint;
function telnet_ServerThread(P: pointer): Longint;

const
  AllowEleMON       : Boolean = false;
  TelnetSessions    : Longint = 0;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 uses {$IFDEF Win32}
        Windows,
      {$ENDIF}

      {$IFDEF OS2}
        Os2Def,
        Os2Base,
      {$ENDIF}

       Dos, GenDos, CfgRec, SysVars, objDec, Global, SockDef, SockFunc,
       eSrv_U, SysUtils, Bitwise, StrPath, WordStr, LongStr, JDates,
        Debug_U, elLog_U, Threads, GenFile, ScrnU, StrEdit,
         ReadCfg, Memman;
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

const
  BusyStr           = 'BUSY' + #10#13;
  ConnectStr        = 'CONNECT 115200/TELNET'+#10#13#10#13;
  Elepath           : String = 'ELEBBS.EXE';

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure telnet_SetupServer;
var ErrorStr: String;
begin
  TelnetServer := TTcpServer.Create;
  TelnetServer.ReUseAddr := true;

  if NOT TelnetServer.SetupServer(LineCfg^.Telnet^.ServerPort, ErrorStr, InAddr_Any) then
    FatalError(ErrorStr);
  TelnetServer.Blocking := false;
end; { proc. telnet_SetupServer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{$IFDEF WIN32}
function SetDosError(ErrCode: Integer): Integer;
begin
  DosError := ErrCode;
  Result := ErrCode;
end;

function SetResult(Success: Boolean): Longint;
begin
  Result := 0;
  if not Success then
    Result := GetLastError;
end;

function SysExecute(Path,CmdLine,Env: PChar; Async: Boolean; PID: PLongint; StdIn,StdOut,StdErr: Longint): Longint;
{$IFNDEF VER1_0_6}
var
  ProcessInfo: TProcessInformation;
  LastAsync: Boolean;
{$ELSE}
var
  ProcessInfo: TProcessInformation;
  LastAsync: Boolean;
{$ENDIF}

var
  P: PChar;
  Flags: Longint;
  StartupInfo: TStartupInfo;
  CmdLineBuf: array [0..1023] of Char;
begin
{$IFNDEF VER0_99_11}
  P := CmdLineBuf;
  P^ := '"';                   // Quotes to support spaces
  inc(P);
  P := StrECopy(P, Path);      // 'Path'#0
  P^ := '"';
  inc(P);
  P^ := ' ';
  StrCopy(P+1, CmdLine);                // 'Path CommandLine'#0
  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  with StartupInfo do
  begin
    cb := SizeOf(TStartupInfo);
    dwFlags := startf_UseShowWindow;

    if ReadBit(LineCfg^.Telnet^.Attrib, 1) then
     wShowWindow := SW_MINIMIZE
      else wShowWindow := SW_SHOWNORMAL;

    if ReadBit(LineCfg^.Telnet^.Attrib, 2) then
     wShowWindow := SW_HIDE;

    if StdIn = -1 then
      hStdInput := GetStdHandle(STD_INPUT_HANDLE)
    else
      hStdInput := StdIn;
    if StdOut = -1 then
      hStdOutput := GetStdHandle(STD_OUTPUT_HANDLE)
    else
      hStdOutput := StdOut;
    if StdErr = -1 then
      hStdError := GetStdHandle(STD_ERROR_HANDLE)
    else
      hStdError := StdErr;
    if (StdIn <> - 1) or (StdOut <> -1) or (StdErr <> -1) then
      Inc(dwFlags, startf_UseStdHandles);
  end;
  Flags := normal_Priority_Class OR CREATE_NEW_CONSOLE;
  LastAsync := Async;
  Result := SetResult(CreateProcess(Path, CmdLineBuf, nil, nil, True, Flags, Env, nil, {$IFDEF FPC}@{$ENDIF}StartupInfo,
                      {$IFDEF FPC}@{$ENDIF}ProcessInfo));
  if Result = 0 then
    if Async then
      begin
        if PID <> nil then
          PID^ := ProcessInfo.hProcess;
      end
    else
      WaitForSingleObject(ProcessInfo.hProcess, Infinite);
{$ENDIF}
end;
{$ENDIF}

{$IFDEF OS2}
function StartNewWindow(Title, FileName, Parameters, QueueName: String; var RC: Longint): Longint;
{ Converted from example code of "The OS/2 API Project - DosStartsession" }
{ Original C-code written by Oscar Gustafsson - oscar@lysator.liu.se }
type rcStruct = record
                  SessionID  : uShort;
                  ReturnCode : uShort;
                end; { rcStruct }

var IdSession : ULong;
    Pid1      : Pid;
    StData    : StartData;
    hqQueue   : HQUEUE;
    rdRequest : REQUESTDATA;
    ulSzData  : ULong;
    bPriority : Byte;
    pvData    : ^RcStruct;

    CmdBuf    : Array[0..259] of Char;
    ParamBuf  : Array[0..259] of Char;
    QBUf      : Array[0..259] of Char;
    TitleBuf  : Array[0..259] of Char;
begin
  Result := 0;                                                    { Success }
  RC := -1;

  FillChar(StData, SizeOf(StartData), 00);
  FillChar(CmdLine, SizeOf(CmdLine), #00);
  FillChar(ParamBuf, SizeOf(ParamBuf), #00);
  FillChar(QBuf, SizeOf(QBuf), #00);
  FillChar(TitleBuf, SizeOf(TitleBuf), #00);

  StData.Length := SizeOf(StartData);
  StData.Related := ssf_Related_Child;
  StData.FgBg := ssf_FgBg_Back;
  StData.TraceOpt := ssf_TraceOpt_None;
  StData.PgmTitle := StrPCopy(TitleBuf, Title);
  StData.PgmName := StrPCopy(CmdBuf, FileName);
  StData.PgmInputs := StrPCopy(ParamBuf, Parameters);
  StData.TermQ := StrPCopy(QBuf, QueueName);
  StData.Environment := nil;
  StData.InheritOpt := ssf_InhertOpt_Parent;
  StData.SessionType := ssf_Type_Default;
{?}  StData.IconFile := nil;
{?}  StData.PgmHandle := 0;
{?}  StData.PgmControl := ssf_Control_Visible;
  StData.InitXPos := ssf_control_setpos;
  StData.InitYPos := ssf_control_setpos;
  StData.InitXSize := ssf_control_setpos;
  StData.InitYSize := ssf_control_setpos;
{?}  StData.ObjectBuffer := nil;
{?}  StData.ObjectBuffLen := 0;

  if DosCreateQueue(hqQueue, QUE_FIFO AND QUE_CONVERT_ADDRESS, StrPCopy(QBuf, QueueName)) = 0 then
   begin
     if DosStartSession(StData, IdSession, Pid1) = 0 then
      begin
        if DosReadQueue(hqQueue, rdRequest, ulSzData, pvData, 0, 0, bPriority, 0) = 0 then
          begin
            Rc := pvData^.Returncode;
            DosFreeMem(pvData);
          end
            else Result := -1;
      end
        else Result := -1;

     DosCloseQueue(hqQueue);
   end
    else Result := -1;
end; { func. StartNewWindow }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure NewExec(const Path: PathStr; const ComLine: ComStr; const Title: String; SlotNr: longint);
{$IFDEF FPC}
 const ExecFlags: Longint = 0;

 const
   efAsync = 1;
{$ENDIF}

var
  PathBuf:    array [0..255] of Char;
  CmdLineBuf: array [0..255] of Char;
  ReturnCode: Longint;
begin
  {$IFDEF WIN32}
    SetDosError(SysExecute(StrPCopy(PathBuf, Path), StrPCopy(CmdLineBuf, ComLine), nil, ExecFlags = efAsync, nil, -1, -1, -1));
  {$ENDIF}
  
  {$IFDEF OS2}
  	if StartNewWindow(Title, Path, ComLine, '\\QUEUES\ELESES$.' + FStr(SlotNr), ReturnCode) = 0 then
   	begin
       //SetDosError(ReturnCode);
   	end
     	else //SetDosError(2);
  {$ENDIF}
  
  {$IFNDEF OS2}
   {$IFNDEF WIN32}
     {$ERROR Routine not supported on this platform}
   {$ENDIF}
  {$ENDIF}  
end; { proc. NewExec }
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


function telnet_ExecuteConnection(P: pointer): Longint;
var TempStr    : String;
    TempPath   : String;
    OldDir     : String;
    EleMonStr  : String;
    BufFlag    : Longint;
    ReturnCode : Longint;

    CurSlot    : Longint;
    DupClientRC: Longint;
    TitleStr   : String;
begin
  {-- Update the screen ---------------------------------------------------}
  srvUpdateScreen := TRUE;

  {-- get current slot ----------------------------------------------------}
  CurSlot := Longint(p^);

  {-- Update the total number of connections weve gotten ------------------}
  Inc(TotalSessions);
  Inc(CurrentSessions);
  Inc(TelnetSessions);

  {-- Return a string showing that we connected (to the user --------------}
  TempStr := ConnectStr;
  BufFlag  := 00;
  ReturnCode := SockSend(Connections[CurSlot].ClientRC,
                         @TempStr[01],
                         Length(TempStr),
                         BufFlag);


  {-- Now actually start executing EleBBS ---------------------------------}
  {$IFDEF WIN32}
    if NOT DuplicateHandle(GetCurrentProcess,
                           Connections[CurSlot].ClientRC,
                           GetCurrentProcess,
                           @DupClientRC,
                           0,
                           true,
                           DUPLICATE_SAME_ACCESS) then EXIT;
  {$ELSE}
    DupClientRC := Connections[CurSlot].ClientRC;
  {$ENDIF}

  {$IFNDEF FPC}
    ExecFlags := efsync;
  {$ENDIF}
  TitleStr := FullProgName + '/' + PlatformIDStr + ' - TELNET node %d';

  TempStr := ForceBack(LineCfg^.Telnet^.ProgramPath) + ElePath;
  TempPath := ForceBack(LineCfg^.Telnet^.NodeDirectories);
  While Pos('*N', TempPath) > 00 do
     Replace('*N', FStr(Longint(Connections[CurSlot].SrvData)), TempPath);


  GetDir(0, OldDir);
  {$i-}
    MkDir(NoBackSlash(TempPath));
    if IoResult > 0 then ;
    ChDir(NoBackSlash(TempPath));
  {$i+}
  if IOResult > 00 then ;

  if AllowEleMON then EleMonstr := ' -XM'
    else EleMonStr := '';
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, '(TelSrv) ExecuteEleBBS(TempStr='+TempStr+');');
    DebugObj.DebugLog(logTcpIp, '(TelSrv) ExecuteEleBBS(TempStr='+'-C3 -XC -XT -B65529 -H'+FStr(DupClientRC) +
          ' -N'+FStr(Longint(Connections[CurSlot].SrvData)) + EleMonStr+');');
  {$ENDIF}


  DosError := 00;
  NewExec(TempStr, '-C3 -XC -XT -B65529 -H'+FStr(DupClientRC) +
          ' -N'+FStr(Longint(Connections[CurSlot].SrvData)) +
          ' -XI' + Connections[CurSlot].IpAddr + #32 +
          EleMonStr,
          Format(TitleStr, [Longint(Connections[CurSlot].SrvData)]), CurSlot);

  if DosError <> 00 then
    RaLog('!', '[TELSRV ] Error executing "'+TempStr+'" from within telnet server');

  ChDir(NoBackSlash(OldDir));

  {$IFDEF WIN32}
    try
      ReturnCode := SockShutdown(DupClientRC, 01);
      ReturnCode:= SockClose(DupClientRC);
    except
    end;
  {$ENDIF}
  try
    ReturnCode:= SockShutdown(Connections[CurSlot].ClientRC, 01);
    ReturnCode:= SockClose(Connections[CurSlot].ClientRC);
  except
  end;

  if (ReturnCode <> 00) then
    begin
      FatalError('Shutdown of socket failed!');
    end; { if }

  {-- Show that this session has been disconnected ------------------------}
  RaLog('>', Format('[TELSRV ] [%s] Connection closed', [Connections[CurSlot].IpAddr]));

  {-- and remove this session from the slot -------------------------------}
  Connections[CurSlot].IpAddr := '';
  Connections[CurSlot].StartTimeStr := '';
  Connections[CurSlot].ClientRC := -1;
  Connections[CurSlot].Name := '';

  {-- decrease the total number of sessions -------------------------------}
  Dec(CurrentSessions);
  Dec(TelnetSessions);

  {-- and force a screen update -------------------------------------------}
  srvUpdateScreen := true;

  ExitThisThread;
end; { func. telnet_ExecuteConnection }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function telnet_ServerThread(P: pointer): Longint;
var ClientAddr  : PSockAddr;
    ClientRC    : Longint;
    BufFlag     : Longint;

    ReturnCode  : Longint;
    Counter     : Longint;
    SelectedSlot: Longint;
    SelectedNode: Longint;

    Temp        : Longint;
    Client      : String;
    TempStr     : String;
begin
  {-- Initialize basic variables ------------------------------------------}
  New(ClientAddr);
  FillChar(ClientAddr^, SizeOf(ClientAddr^), #00);
  Temp := SockAddr_len;

  {-- Now lets keep looping till we get terminated ------------------------}
  REPEAT
    TelnetServer.DoSleep(1);

    {-- repeat until we get at least one connection -----------------------}
    REPEAT
      ClientRC := SockAccept(TelnetServer.ServerHandle,
                             ClientAddr,
                             Temp);

      Telnetserver.DoSleep(10);
    UNTIL (ClientRC <> -1) OR (TermProgram);

    TelnetServer.DoSleep(1);

    {-- Make sure that the calls aren't blocking --------------------------}
    if (NOT TermProgram) then 
      begin
        SocksetBlockingIO(ClientRC, false);
      end; { if }

    {-- now get a slot for our program to work in -------------------------}
    SelectedSlot := FindEmptySlot;

    {-- and a node number -------------------------------------------------}
    SelectedNode := FindEmptyNode;

    {-- make sure we dont exceed the max. number of sessions --------------}
    if (TelnetSessions >= LineCfg^.Telnet^.MaxSessions) or
        (SelectedNode = 0) or
         (SelectedSlot = 0) then
      SelectedSlot := 0;

    {-- now act upon what we found ----------------------------------------}
    if NOT TermProgram then
      begin
        {-- Now get the client name ---------------------------------------}
        if NOT NoDnsLookup then
          Client := SockGetHostNameByAddr(@ClientAddr^.Sin_Addr)
            else Client := TelnetServer.IpToStr(ClientAddr^.sIn_Addr);

        if ReturnCode = -1 then
          Client := 'unknown host';

        {-- do we have room for this user anyway? -------------------------}
        if SelectedSlot > 00 then
          begin
            {-- clear any pending thread ----------------------------------}
            if Connections[SelectedSlot].ServerThread <> nil then
              Dispose(Connections[SelectedSlot].ServerThread, Done);

            {-- and initialize the new section as soon as possible --------}
            Connections[SelectedSlot].SrvType := srvTelnet;
            Connections[SelectedSlot].Name := Client;
            Connections[SelectedSlot].IpAddr := TelnetServer.IpToStr(ClientAddr^.sIn_Addr);
            Connections[SelectedSlot].StartTimeStr := TimeStr(true, false);
            Connections[SelectedSlot].ClientRC := ClientRC;
            Longint(Connections[SelectedSlot].SrvData) := SelectedNode;

            {-- now initialize the new server instance --------------------}
            New(Connections[SelectedSlot].ServerThread, Init);

            {-- low some info ---------------------------------------------}
            RaLog('>', Format('[TELSRV ] [%s] Connection opened', [Connections[SelectedSlot].IpAddr]));

            {-- and actually execute this thread --------------------------}
            Connections[SelectedSlot].ServerThread.CreateThread(ExecEleStack,
                                                                @telnet_ExecuteConnection,
                                                                @SelectedSlot,
                                                                0);
          end
            else begin
                   {-- no more sessions left, exit ------------------------}
                   TempStr := BusyStr;
                   Inc(TotalRefused);

                   BufFlag  := 00;
                   ReturnCode := SockSend(ClientRC,
                                          @TempStr[01],
                                          Length(TempStr),
                                          BufFlag);

                   BufFlag := 02;
                   ReturnCode:= SockShutdown(ClientRC, BufFlag);
                   SockClose(ClientRc);

                   if (ReturnCode <> 00) then
                     begin
                       FatalError('Shutdown of socket failed!');
                     end; { if }
                 end; { if busy }
      end; { if not program Terminated }

  UNTIL (ReturnCode = -1) OR (TermProgram);

  {-- and remove unused memory --------------------------------------------}
  Dispose(ClientAddr);
  telnet_ListenThreadEnded := true;
  ExitThisThread;
end; { func. telnet_ServerThread }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CheckEnvironments;
var ElePath : String;
    SaveScrn: Pointer;
begin
  ElePath := GetSysEnv;

  if ElePath = '' then
    begin
      SaveScreen(SaveScrn);

      ShadFillBoxTitle(04, 10, 78, 14, mnuNormColor, mnuStyle, true, ' WARNING! ');
      WriteCenterPos(06, 12, 76, 15, ' ELEBBS environment not found, EleBBS may not function properly!');
      GetInput;

      RestoreScreen(SaveScrn);
    end; { if }
end; { proc. CheckEnvironments }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure telnet_InitServer;
begin
  if NOT MemMan.AllocMem(LineCfg^.Telnet, SizeOf(TelnetRecord), 'TelnetRecord', 'ShowHeader') then
    begin
      WriteLn;
      Writeln('Sorry you need at least ', SizeOf(TelnetRecord) div 1024, 'k more memory to run');
      Halt(255);
    end; { Not enough memory }

  {-- Initialize some objects ---------------------------------------------}
  ReadTelnetELE;
  CheckEnvironments;
end; { proc. telnet_InitServer }

end. { TELSRV }
