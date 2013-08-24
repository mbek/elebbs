unit TELCLI;
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

uses TcpCli;

Const
  { Telnet Options }
  TELNET_IAC   = #255;  { Interpret as Command }
  TELNET_EXOPL = #255;  { Extended options list }
  TELNET_DONT  = #254;  { Stop performing, or not expecting him to perform }
  TELNET_DO    = #253;  { Perform, or expect him to perform }
  TELNET_WONT  = #252;  { Refusal to perform }
  TELNET_WILL  = #251;  { Desire to perform }
  TELNET_SB    = #250;  { What follow is sub-negotiation of indicated option }
  TELNET_GA    = #249;  { Go ahead signal }
  TELNET_EL    = #248;  { Erase Line function }
  TELNET_EC    = #247;  { Erase Character function }
  TELNET_AYT   = #246;  { Are You There function }
  TELNET_AO    = #245;  { Abort Output function }
  TELNET_IP    = #244;  { Interrupt Process function }
  TELNET_BRK   = #243;  { NVT break character }
  TELNET_DM    = #242;  { Data stream portion of a Synch (DATAMARK) }
  TELNET_NOP   = #241;  { No operation }
  TELNET_SE    = #240;  { End of sub-negotiation parameters }
  TELNET_EOR   = #239;  { End of record }
  TELNET_ABORT = #238;  { Abort process }
  TELNET_SUSP  = #237;  { Suspend current process }
  TELNET_EOF   = #236;  { End of file }

  TELNETOPT_BINARY  = #0;     { Transmit binary }
  TELNETOPT_ECHO    = #1;     { Echo mode }
  TELNETOPT_SUPGA   = #3;     { Suppress Go-Ahead }
  TELNETOPT_TERM    = #24;    { Terminal Type }
  TELNETOPT_SPEED   = #32;    { Terminal Speed }
  TELNETOPT_XDISPLOC= #35;    { X-Display location }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

type InitTermProc = Procedure;

type tTelnetclient = class(tTcpClient)
       protected

       public
         fTerminalType: String;
         fInitTerm: InitTermProc;

         constructor create;
         destructor destroy;

         property TerminalType: String READ fTerminalType WRITE fTerminalType;

         procedure SendStr(S: String);
         function  RecvString(var S: AnsiString; DoPeek: Boolean): Boolean;
         procedure SendTerminalType;

         procedure SendWill(Option: Char);
         procedure SendWont(Option: Char);
         procedure SendDont(Option: Char);
         procedure SendDo(Option: Char);
         procedure HandleSB(TelCmd, TempStr: String; var Counter: Longint);

         procedure HandleBinary(TelCmd: String);
         procedure HandleEcho(TelCmd: String);
         procedure HandleSpeed(TelCmd: String);
         procedure HandleTerminalType(TelCmd: String);
     end; { tTelnet client }


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 uses IrcDef, LongStr, WordStr, Support, SysUtils, Ellog_U,
       StUtils, Global {$IFDEF VirtualPascal}, VpSysLow{$ENDIF};
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


constructor tTelnetClient.Create;
begin
  inherited Create;
end; { constructor Create }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tTelnetClient.Destroy;
begin
  inherited Destroy;
end; { destructor Destroy }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTelnetClient.SendStr(S: String);
var TempStr: String;
    Counter: Longint;
begin
  TempStr := '';
  Counter := 01;

  while Counter <= Length(s) do
    begin
      Case S[Counter] of
         TELNET_IAC : begin
                         TempStr := TempStr + S[Counter] + S[Counter];
                      end;
         else TempStr := TempStr + S[Counter];
      end; { case }

      Inc(counter);
    end; { while }

  inherited SendStr(TempStr);
end; { proc. SendStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetClient.SendWill(Option: Char);
var Buf   : String;
    Return: Integer;
begin
  Buf[0] := #03;
  Buf[1] := TELNET_IAC;
  Buf[2] := TELNET_WILL;
  Buf[3] := Option;

  inherited SendStr(Buf);
end; { proc. SendWill }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetClient.SendWont(Option: Char);
var Buf   : String;
    Return: Integer;
begin
  Buf[0] := #03;
  Buf[1] := TELNET_IAC;
  Buf[2] := TELNET_WONT;
  Buf[3] := Option;

  inherited SendStr(buf);
end; { proc. SendWont }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetClient.SendDo(Option: Char);
var Buf   : String;
    Return: Integer;
begin
  Buf[0] := #03;
  Buf[1] := TELNET_IAC;
  Buf[2] := TELNET_DO;
  Buf[3] := Option;

  inherited SendStr(buf);
end; { proc. SendDo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTelnetClient.SendDont(Option: Char);
var Buf   : String;
    Return: Integer;
begin
  Buf[0] := #03;
  Buf[1] := TELNET_IAC;
  Buf[2] := TELNET_DONT;
  Buf[3] := Option;

  inherited SendStr(Buf);
end; { proc. SendDont }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTelnetClient.HandleSB(TelCmd, TempStr: String; var Counter: Longint);
begin
  {-- We will dis-assemble the Subnegotiation here ----------------------------}
  Case TempStr[Counter] of
    TELNETOPT_TERM : begin
                       if TempStr[Counter + 1] = #1 then
                         begin
                           SendTerminalType;
                           fInitTerm;
                         end; { if }
                     end; { else }
  end; { case }

  {-- Now skipp the sub-option type, and the _SE ------------------------------}
  Inc(Counter, 3);
end; { proc. HandleSB }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTelnetClient.HandleBinary(TelCmd: String);
begin
  if TelCmd[1] = TELNET_DONT then
    SendWont(TELNETOPT_BINARY)
      else SendWill(TELNETOPT_BINARY);
end; { proc. HandleBinary }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTelnetClient.HandleEcho(TelCmd: String);
begin
  SendDo(TELNETOPT_ECHO);
end; { proc. HandleEcho }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTelnetClient.HandleSpeed(TelCmd: String);
begin
  SendWont(TELNETOPT_SPEED);
end; { proc. HandleSpeed }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTelnetClient.HandleTerminalType(TelCmd: String);
begin
  SendWill(TELNETOPT_TERM);
  SendTerminalType;
end; { proc. HandleTerminalType }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tTelnetClient.RecvString(var S: AnsiString; DoPeek: Boolean): Boolean;
var TempStr: AnsiString;
    Counter: Longint;
    TelCmd : String;
begin
  {-- Initialize some variables -----------------------------------------------}
  Counter := 01;
  S := '';
  TelCmd := '';

  {-- Get the string ----------------------------------------------------------}
  inherited RecvString(TempStr, doPeek);

  {-- Now handle it -----------------------------------------------------------}
  While Counter <= Length(TempStr) do
    begin
      Case TempStr[Counter] of
         {-- TELNET_IAC -------------------------------------------------------}
         TELNET_IAC :
            begin
              {-- Get the TELNET command --------------------------------------}
              Inc(Counter);
              TelCmd := TempStr[Counter];

              {-- Get the next character --------------------------------------}
              Inc(Counter);

              {-- Get the type of command to handle ---------------------------}
              Case TelCmd[1] of
                {-- Sub-negotiation -------------------------------------------}
                TELNET_SB : begin
                              HandleSB(TelCmd, TempStr, Counter);
                            end; { SubNegotiation }

                {-- Erase character -------------------------------------------}
                TELNET_EC : begin
                               {-- We enforce the erase character -------------}
                               SendWill(TelCmd[1]);
                            end; { Erase Character }

                {-- Now handle the DONT, DO -----------------------------------}
                TELNET_DONT,
                TELNET_DO : begin
                              Case TempStr[Counter] of
                                TELNETOPT_BINARY : HandleBinary(TelCmd);
                                TELNETOPT_ECHO   : HandleEcho(TelCmd);
                                TELNETOPT_TERM   : HandleTerminalType(TelCmd);
                                TELNETOPT_SPEED  : HandleSpeed(TelCmd);
                                  else SendWont(TempStr[Counter]);
                              end; { case }
                            end; { TELNET_DO, TELNET_DONT }

                {-- We do not support: ----------------------------------------}
                {-- Go Ahead --------------------------------------------------}
                {-- Erase Line (could be supported by CTRL-Y) -----------------}
                {-- Are you there ---------------------------------------------}
                {-- Abort output (could be supported by Yes/No/More) ----------}
                {-- Interrupt process -----------------------------------------}
                {-- TELNET_BRK (break or attention key) -----------------------}
                {-- TELNET_DM -------------------------------------------------}
                {-- TELNET_NOP ------------------------------------------------}
                  else begin
                         SendWont(TempStr[Counter]);
                       end; { else }
              end; { case }
            end; { TELNET_IAC }
          else S := S + TempStr[Counter];
      end; { case }

      Inc(Counter);
    end; { while }
end; { func. RecvString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTelnetClient.SendTerminalType;
var TempStr: String;
begin
  TempStr := TELNET_IAC +
             TELNET_SB  +
             TELNETOPT_TERM +
             #00 +        { Terminal-type is }
             TerminalType +
             TELNET_IAC +
             TELNET_SE;

  inherited SendStr(TempStr);
end; { proc. SendTerminal }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { telcli }

