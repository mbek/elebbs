unit TELUNIT;
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
** Telnet client component
**
** Copyright (c) 1998, 99 by Maarten Bekers
**
** Created : 15-Jan-1999
** Last update : 15-Jan-1999
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses CfgRec, TelCli, Global;

type tTelInterfaceObj = Object
         TelnetHostName: String;
         TelnetPort    : Longint;
         TelnetClient  : TTelnetClient;
         RecvStr       : AnsiString;
         DoAbortTelnet : Boolean;

         {-- Constructors and destructors ----------------------------------}
         constructor Init;
         destructor Done;

         {-- Generic routines ----------------------------------------------}
         procedure Telnet_System(MiscData: String);

         {-- Private routines ----------------------------------------------}
         private
           procedure CloseDownTelnet;
           procedure AskTelnetServer;
           procedure ShowTelnetMenu;
           function SetupTCPIP(Port: Word; HostName: String): Boolean;
     end; { tTelInterfaceObj }


type pTelInterfaceObj = ^tTelInterfaceObj;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses {$IFDEF VIRTUALPASCAL}
       VpSysLow,
       Crt,
     {$ENDIF}

     {$IFDEF WIN32}
       Windows,
     {$ENDIF}
         SockFunc, TcpCli, Sockdef, LongStr, InOut_U, Multi,
          ElLog_U, LineEd, RAL, Control, Cases, Input_U,
           GenFile, WordStr, CentrStr, SysUtils, Outblock,
            ObjDec, elx_BBS;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Telnet_InitTerm;
begin
  OutputObj^.ClearScreen;
end; { proc. Telnet_InitTerm }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


constructor tTelInterfaceObj.Init;
begin
  TelnetHostName := '127.0.0.1';
  TelnetPort := 23;
  Telnetclient := nil;
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tTelInterfaceObj.Done;
begin
  CloseDownTelnet;
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTelInterfaceObj.CloseDownTelnet;
begin
  if TelnetClient <> nil then
    try
      TelnetClient.Free;
    except
    end;

  TelnetClient := nil;
end; { proc. CloseDownTelnet }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTelInterfaceObj.AskTelnetServer;
var TempStr: String;
begin
  TempStr := '';

  Write('`A3:', langObj^.ralGet(ralAskTelnet));
  GetString(TempStr, 78 - Length(langObj^.ralGet(ralAskTelnet)), [#32..#254], false, false, false, false);
  WriteLn;

  TelnetHostName := TempStr;
end; { proc. AskTelnetServer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tTelInterfaceObj.SetupTCPIP(Port: Word; HostName: String): Boolean;

procedure FatalError(S: String);
begin
  SetupTCPIP := false;
  RaLog('!', 'TELNET-CLIENT: '+S);
end; { proc. FatalError }

var ErrorStr: String;
begin
  Result := TelnetClient.ConnectToServer(HostName, Port, ErrorStr);

  if NOT Result then
    FatalError(ErrorStr);
end; { proc. SetupTCPIP }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTelInterfaceObj.ShowTelnetMenu;
var elxObj  : pElxBbsObj;
    TmpStr  : String;
begin
  {-- Run the EleXer script ------------------------------------------------}
  New(elxObj, Init);
  if NOT elxObj^.RunElexerScript('telnmnu', '', false) then
    begin
      RaLog('!', 'Error occured while executing TELNMNU EleXer script');
      TmpStr := 'QUIT';
    end { if }
      else TmpStr := SupCase(elxObj^.GetElxReturnString);
  Dispose(elxObj, Done);

  {-- and depending on the result we act -----------------------------------}
  if TmpStr = 'QUIT' then DoAbortTelnet := TRUE;
end; { proc. ShowTelnetMenu }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTelInterfaceObj.Telnet_System(MiscData: String);
var TempCH       : Char;
begin
  {-- Start initialisation of IRC parameters -------------------------------}
  TempCH := #0;
  DoAbortTelnet := FALSE;
  TelnetClient := nil;
  TelnetHostName := GetValue('/SERVER=', MiscData, true);

  {-- Setup the telnet client ----------------------------------------------}
  TelnetClient := tTelnetClient.Create;
  TelnetClient.fInitTerm := {$IFDEF FPC}@{$ENDIF}Telnet_InitTerm;

  {-- Now ask for the server to connect to ---------------------------------}
  WriteLn;
  WriteLn;
  if TelnetHostName = '' then
    AskTelnetServer;

  if Pos(':', TelnetHostName) > 0 then
    begin
      TelnetPort := FVal(Copy(TelnetHostName, Pos(':', TelnetHostName) + 1, 255));
      TelnetHostName := Copy(TelnetHostName, 1, Pos(':', TelnetHostName) - 1);
    end; { if }

  if TelnetHostName = '' then
    begin
      CloseDownTelnet;
      EXIT;
    end; { if }

  {-- Now try to connect to this -------------------------------------------}
  WriteLn(Format(LangObj^.ralGet(ralTelConn), [TelnetHostName]));
  OutBlockObj^.DumpBlock;
  InputObj^.UpdateScrn;

  {-- log some information -------------------------------------------------}
  RaLog('>', 'Opening telnet connection to ' + TelnethostName + ' (port ' + FStr(TelnetPort) + ')');

  {-- Try to connect to the server -----------------------------------------}
  if NOT SetupTcpIp(TelnetPort, TelnetHostName) then
    begin
      Write('`A12:', Format(LangObj^.ralGet(ralNoConnect), [TelnetHostName]));
      InputObj^.PressEnter(false, true);
      CloseDownTelnet;
      EXIT;
    end; { if }

  {-- Try to login into the server -----------------------------------------}
  TelnetClient.SendDo(TELNETOPT_ECHO);
  TelnetClient.TerminalType := 'ansi';

  {-- Now loop through this. We get all keys and send it to the remote -----}
  REPEAT
    {-- If a key was pressed, send it --------------------------------------}
    if InputObj^.KeyPressed then
     begin
       TempCH := InputObj^.ReadKey;

       if TempCH = #29 then                                        { Ctrl-] }
         begin
           ShowTelnetMenu;
         end
           else TelnetClient.SendStr(TempCH);
     end; { if }

    {-- Receive the string, and update the remote/local screen -------------}
    if TelnetClient.DataAvailable then
      begin
        TelnetClient.RecvString(RecvStr, false);
        Write(RecvStr);

        OutBlockObj^.DumpBlock;
        InputObj^.UpdateScrn;
      end; { if }

    {-- Reset the line counter ---------------------------------------------}
    OutputObj^.ResetLines(01);

    {-- Make sure we dont hug too much CPU ---------------------------------}
    DoSlice;
  UNTIL (NOT TelnetClient.ConnectionAlive) OR (NOT ContrObj^.CheckAll) OR
          (DoAbortTelnet);

  {-- Now disconnect and show the user were through ------------------------}
  if NOT TelnetClient.ConnectionAlive then
    begin
      Write('`A12:`X1:`E:', Format(LangObj^.ralGet(ralIrcDiscon), [TelnetHostName]));
      InputObj^.PressEnter(false, true);
    end; { if }

  CloseDownTelnet;
end; { proc. Telnet_System }


end. { unit TELUNIT }
