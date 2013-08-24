unit POP3CLI;
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
** POP3 - Mail retrieving client component
**
** Copyright (c) 1998, 99 by Maarten Bekers
**
** Created : 19-Apr-1999
** Last update : 19-Apr-1999
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses TcpCli, Articles;

Const ArticleWaitSecs   = 120;
      KeyboardCheckSecs = 2;

type ShowLineProc     = procedure(const S: ShortString);
     AbortSessionType = function: boolean;
     TopFuncType      = procedure(const S: ShortString);

type tPOP3client = class(tTcpClient)
       protected
        fUserName    : String;
        fPassword    : String;
        fLastError   : Longint;
        fErrorStr    : String;
        fShowLine    : ShowLineProc;
        fAbortFunc   : AbortSessionType;
        fTopFunc     : TopFuncType;

       public
        procedure ShowAllText;

        constructor Create;
        destructor  Destroy; override;

        property Username : String READ fUserName WRITE fUserName;
        property Password : String READ fPassword WRITE fPassword;
        property ShowLine : ShowLineProc READ fShowLine WRITE fShowLine;
        property AbortFunc: AbortSessionType READ fAbortFunc WRITE fAbortFunc;
        property TopFunc  : TopFuncType READ fTopFunc WRITE fTopFunc;

        function Logon: Boolean;
        function PositiveResponse(NeedError: Boolean): Boolean;

        procedure Quit;
        procedure Dele(Nr: Longint);
        function GetStat(var NrMsgs, NrOctets: Longint): Boolean;
        function Top(MsgNr: Longint; NrLines: Longint): Boolean;
        function Retr(MsgNr: Longint; var MsgText: MessageTextType): Boolean;
        function Noop: Boolean;
        function List(MsgNr: Longint): Longint; { Get message size }
        procedure Rset;
     end; { tPOP3 client }


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 uses ApTimer, LongStr, WordStr, SysUtils,
       SockDef, SockFunc, StUtils, Global;
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tPOP3Client.Create;
begin
  inherited Create;

  UserName := '';
  Password := '';
  fErrorStr:= '';
end; { constructor Create }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tPOP3Client.Destroy;
begin
  inherited Destroy;
end; { destructor Destroy }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tPOP3Client.ShowAllText;
var RecvStr  : AnsiString;
    Count    : Longint;
begin
  Count := 800;

  While (Count > 0) AND (NOT DataAvailable) AND (ConnectionAlive) do
    begin
      DoSleep(10);
      Dec(Count);
    end; { while }

  while (DataAvailable) AND (ConnectionAlive) do
    begin
      RecvStrLn(RecvStr, false);
      fShowLine(RecvStr);
    end; { while }

end; { proc. ShowAllText }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tPOP3Client.Logon: Boolean;
var AbortLogon: Boolean;
    TempStr   : AnsiString;
begin
  AbortLogon := false;
  Result := true;
  FlushData;

  if UserName <> '' then
     begin
       SendStrLn('USER '+UserName);

       if NOT PositiveResponse(true) then AbortLogon := true
         else RecvStrLn(TempStr, false);
     end; { if }

  if PassWord <> '' then
     begin
       SendStrLn('PASS '+Password);

       if NOT PositiveResponse(true) then AbortLogon := true
          else RecvStrLn(TempStr, False);
     end; { if }

  if AbortLogon then Result := false;
  FlushData;
end; { func. Logon }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tPOP3Client.PositiveResponse(NeedError: Boolean): Boolean;
var TempStr: AnsiString;
begin
  if NeedError then WaitForData;

  RecvStrLn(TempStr, true);
  fErrorStr := TempStr;

  if Length(TempStr) >= 1 then Result := (TempStr[1] = '+')
    else Result := FALSE;
end; { func. PositiveResponse }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tPOP3Client.Quit;
begin
  SendStrLn('QUIT');
  if PositiveResponse(True) then ;
end; { proc. Quit }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tPOP3Client.GetStat(var NrMsgs, NrOctets: Longint): Boolean;
begin
  Result := false;
  NrMsgs := 0;
  NrOctets := 0;
  FlushData;

  SendStrLn('STAT');

  if PositiveResponse(true) then
    begin
      Result := true;

      NrMsgs := FVal(ExtractWord(fErrorStr, 2, defExtractWord, false, false));
      NrOctets := FVal(ExtractWord(fErrorStr, 3, defExtractWord, false, false));
    end; { if }

  FlushData;
end; { func. GetStat }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tPOP3Client.Dele(Nr: Longint);
begin
  SendStrLn('DELE '+FStr(nr));
end; { proc. DELE }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tPop3Client.Retr(MsgNr: Longint; var MsgText: MessageTextType): Boolean;
var TempStr: AnsiString;
    Timer  : EventTimer;
    TxtPtr : Longint;
    CheckKb: EventTimer;
begin
  FillChar(MsgText, SizeOf(MessageTextType), #00);
  TxtPtr := 00;
  Result := false;

  if DataAvailable then FlushData; { was: FlushData; }

  SendStrLn(Format('RETR %d', [MsgNr]));

  if PositiveResponse(true) then
    begin
      RecvStrLn(TempStr, false);
      Result := true;
      NewTimer(Timer, Secs2Tics(ArticleWaitSecs));
      NewTimer(CheckKB, Secs2Tics(KeyboardCheckSecs));

      While (TempStr <> '.') AND (NOT TimerExpired(Timer)) do
         begin
           if TimerExpired(CheckKb) then
             begin
               NewTimer(CheckKb, Secs2Tics(KeyboardCheckSecs));

               if fAbortFunc{$IFDEF FPC}(){$ENDIF} then BREAK;
               if NOT (ConnectionAlive) then BREAK;
             end; { if }

           if NOT DataAvailable then DoSleep(50);

           if RecvStrLn(TempStr, false) then
             if TempStr <> '.' then
               begin
                 NewTimer(Timer, Secs2Tics(ArticleWaitSecs));
                 TempStr := TempStr + #13#10;

                 if (TxtPtr + Length(TempStr)) < SizeOf(MessageTextType) then
                   begin
                     Move(TempStr[1], MsgText[TxtPtr], Length(TempStr));
                     Inc(TxtPtr, Length(TempStr));
                   end
                     else BREAK;
               end; { if }
         end; { while }
    end; { if }
end; { func. Retr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tPop3Client.Noop: Boolean;
var TempStr: AnsiString;
begin
  if DataAvailable then FlushData;

  SendStrLn('NOOP');
  RecvStrLn(TempStr, false);
  Result := (TempStr[1] = '+');
end; { func. Noop }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tPop3Client.List(MsgNr: Longint): Longint; { Get message size }
var TempStr: AnsiString;
begin
  Result := -1;
  if DataAvailable then FlushData;

  SendStrLn(Format('LIST %d', [MsgNr]));
  if PositiveResponse(true) then
    begin
       RecvStrLn(TempStr, false);

       Result := FVal(ExtractWord(fErrorStr, 3, defExtractWord, false, false));
    end; { if }
end; { func. tPop3Client }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tPop3client.rset;
var TempStr: AnsiString;
begin
  SendStrLn('RSET');
  RecvStrLn(TempStr, false);
end; { proc. rset }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tPOP3Client.Top(MsgNr: Longint; NrLines: Longint): Boolean;
var Timer  : EventTimer;
    TempStr: AnsiString;
begin
  Result := false;
  FlushData;

  SendStrLn(Format('TOP %d %d', [MsgNr, NrLines]));

  if PositiveResponse(true) then
    begin
      RecvStrLn(TempStr, false);
      NewTimer(Timer, Secs2Tics(ArticleWaitSecs));
      TempStr := '';

      TopFunc(Format('Size: %d', [FVal(ExtractWord(fErrorStr, 2, defExtractWord, false, false))]));

      While (TempStr <> '.') AND (NOT TimerExpired(Timer)) AND (ConnectionAlive) do
        begin
          if fAbortFunc{$IFDEF FPC}(){$ENDIF} then BREAK;
          if NOT DataAvailable then DoSleep(1);

          if RecvStrLn(TempStr, false) then
            begin
              NewTimer(Timer, Secs2Tics(ArticleWaitSecs));
              Result := true;

              if TempStr <> '.' then
                TopFunc(TempStr);
            end; { if }
         end; { while }
    end; { if }

end; { func. TOP }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { pop3cli }
