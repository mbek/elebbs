unit SMTPCLI;
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
** SMTP - Mail sending client component
** Written to follow RFC 821
**
** Copyright (c) 1998-2000 by Maarten Bekers
**
** Created : 23-Apr-2000
** Last update : 23-Apr-2000
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses TcpCli, Articles;

Const ArticleWaitSecs   = 120;
      KeyboardCheckSecs = 2;

type  ShowLineProc     = procedure(const S: ShortString);

type tSMTPclient = class(tTcpClient)
       protected
        fLastError   : Longint;
        fShowLine    : ShowLineProc;

       public
        fErrorStr    : AnsiString;

        constructor Create;
        destructor  Destroy; override;

        property ShowLine : ShowLineProc READ fShowLine WRITE fShowLine;

        function Quit: Boolean;
        function Helo(Domain: String; sendEnhanced: boolean): Boolean;
        function base64_encode(strIn: String): String;
        function Auth(SmtpUsername, SmtpPassword: String): Boolean;
        function Mail(MsgFrom: String): Boolean;
        function Rcpt(MsgTo: String): Boolean;
        function Rset: Boolean;
        function Vrfy(Mailbox: String): Boolean;
        function Noop: Boolean;
        function Data(var MsgText: MessageTextType; var ReplyTo: String): Boolean;

        procedure ShowAllText(Require: Boolean);
        function CheckError(TestSet: Array of Word; var LastErr: Longint): Boolean;
     end; { tSMTP client }


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 uses ApTimer, LongStr, WordStr, SysUtils,
       SockDef, SockFunc, StUtils, Global;
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tSMTPClient.Create;
begin
  inherited Create;

  fErrorStr:= '';
end; { constructor Create }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tSMTPClient.Destroy;
begin
  inherited Destroy;
end; { destructor Destroy }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tSMTPClient.ShowAllText(Require: Boolean);
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

function tSMTPClient.Quit: Boolean;
begin
  SendStrLn('QUIT');

  Result := CheckError([221], fLastError);
  ShowAllText(false);
end; { func. Quit }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSMTPClient.Helo(Domain: String; sendEnhanced: boolean): Boolean;
begin
  if (sendEnhanced) then
	  SendStrLn('HELO ' + Domain)
	   else SendStrLn('EHLO ' + Domain);

  Result := CheckError([250], fLastError);

	if (sendEnhanced) AND (Result) then
	  begin
	    Result := false;
	    
	 		While(RecvStrLn(fErrorStr, false)) do
				if (Pos('AUTH LOGIN PLAIN', fErrorStr) > 0) then Result := true;
		end; { if }
	
  ShowAllText(false);
end; { func. Helo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSMTPClient.base64_encode(strIn: String):String;
const base64table :String = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var len, cont :Integer;
    strInput, strOutput :String;
    c1, c2, c3, c4 :Char;
begin
	len := Ord(strIn[0]);
	
	{Example:
	M        a         n
	77       97        110
	01001101 01100001  01101110
	010011 010110 000101 101110
	19     22     5      46
	T      W      F      u
	}
	
	{Make the String multiple of 3 size adding \0 at the end}
	
	case (len mod 3) of
		1: strInput := strIn + chr(0) + chr(0);
		2: strInput := strIn + chr(0);
		else
		  strInput := strIn;
	end;

	len := Ord(strInput[0]);
	
	{We are not concerned about adding new lines afte 76 chars}
	
	{Loops in 3 by 3 chars}
	strOutput := '';
	cont := 1;
	while cont <= len do
		begin
			{Turns 3 8-bits chars into 4 6-bits chars}
			{
	010011 01  0110 0001  01 101110
	00.010011  00.01.0000 | 00000110   00000100 | 00000001  01101110
	010011 010110 000101 101110
	
	00000011 = 1+2 = 3
	00001111 = 1+2+4+8 = 15
	00111111 = 1+2+4+8+16+32 = 63
			}
			
			c1 := base64table[(      ord(strInput[cont])                  shr 2)+1];		
			c2 := base64table[( ( ( (ord(strInput[cont])   and 3 ) shl 6) shr 2) or (ord(strInput[cont+1]) shr 4))+1];
			c3 := base64table[( ( ( (ord(strInput[cont+1]) and 15) shl 4) shr 2) or (ord(strInput[cont+2]) shr 6))+1];
			c4 := base64table[(   ( (ord(strInput[cont+2]) and 63) shl 2) shr 2)+1];
			
	    strOutput := strOutput+c1+c2+c3+c4;		
	    
	    cont := cont + 3;
		end;
		
	base64_encode := strOutput;

end; { func. base64_encode }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSMTPClient.Auth(SmtpUsername, SmtpPassword: String): Boolean;
begin
	{ The correct form of the AUTH PLAIN value is 'authid\0userid\0passwd'
    where '\0' is the null byte. }
	
  SendStrLn('AUTH PLAIN '+base64_encode(Chr(0)+SmtpUsername+Chr(0)+SmtpPassword));

 	Result := CheckError([235], fLastError);
  ShowAllText(false);
end; { func. Auth }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSmtpClient.Mail(MsgFrom: String): Boolean;
begin
  SendStrLn('MAIL FROM:<' + MsgFrom + '>');

  Result := CheckError([250], fLastError);
  ShowAllText(false);
end; { func. Mail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSmtpClient.Vrfy(Mailbox: String): Boolean;
begin
  SendStrLn('VRFY ' + MailBox);

  Result := CheckError([250, 251], fLastError);
  ShowAllText(false);
end; { func. Vrfy }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSmtpClient.Rcpt(MsgTo: String): Boolean;
begin
  SendStrLn('RCPT TO:<' + Trim(MsgTo) + '>');

  Result := CheckError([250, 251], fLastError);
  ShowAllText(false);
end; { func. Rcpt }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSmtpClient.Rset: Boolean;
begin
  SendStrLn('RSET');

  Result := CheckError([250], fLastError);
  ShowAllText(false);
end; { func. Rset }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSmtpClient.Noop: Boolean;
begin
  SendStrLn('NOOP');

  Result := CheckError([250], fLastError);
  ShowAllText(false);
end; { func. Noop }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSmtpClient.Data(var MsgText: MessageTextType; var ReplyTo: String): Boolean;
var TempStr: AnsiString;
    TxtPtr : Longint;
    CheckKb: EventTimer;
begin
  Result := true;
  if DataAvailable then FlushData;       { Make sure previous data is flushed }

  {-- Send command to initiate message transfer ----------------------------}
  SendStrLn('DATA');
  if NOT CheckError([354], fLastError) then
    begin
      Result := false;
      EXIT;
    end; { if }

  {-- Initialize some timers before sending --------------------------------}
  TxtPtr := 00;
  TempStr := '';

  NewTimer(CheckKB, Secs2Tics(KeyboardCheckSecs));

  {-- IORAM Adding the Reply-to header -------------------------------------}
  if (ReplyTo <> '') then
  	SendStrLn('Reply-To: <' + ReplyTo + '>');

  {-- Now actually send the message ----------------------------------------}
  While (TxtPtr <= (StrLen(MsgText) + 01)) do
    begin
      if (NOT (MsgText[TxtPtr] in [#13, #10])) then
        TempStr := TempStr + MsgText[TxtPtr]
          else begin
                 Inc(TxtPtr);

                 if TempStr = '.' then TempStr := '..';
                   SendStrLn(TempStr);

                 TempStr := '';

                 if TimerExpired(CheckKB) then
                   begin
                     NewTimer(CheckKB, Secs2Tics(KeyboardCheckSecs));

                     if NOT ConnectionAlive then BREAK;
                   end; { if }
               end; { if }

      Inc(TxtPtr);
    end; { while }

  {-- End the message ------------------------------------------------------}
  SendStrLn('.');
  if CheckError([250], fLastError) then
    begin
      Result := true;
      { Article posted correctly }
    end
      else begin
             Result := false;
           end; { not posted }
end; { func. Data }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tSMTPClient.CheckError(TestSet: Array of Word; var LastErr: Longint): Boolean;
var Counter       : Longint;
    FoundResponse : Longint;
begin
  Result := false;
  if NOT ConnectionAlive then EXIT;
  WaitForData;

  RecvStrLn(fErrorStr, false);

  FoundResponse := FVal(ExtractWord(fErrorStr, 1, defExtractWord, false, false));

  for Counter := 00 to High(TestSet) do
    if FoundResponse = TestSet[Counter] then
     begin
       LastErr := TestSet[Counter];
       Result := true;
       EXIT;
     end; { CheckError }

end; { func. CheckError }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { smtpcli }
