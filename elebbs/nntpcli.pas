unit NNTPCLI;
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
** NNTP - NEWS client component
**
** Copyright (c) 1998, 99 by Maarten Bekers
**
** Created : 19-Dec-1998
** Last update : 19-Dec-1998
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses TcpCli, Articles;

Const ArticleWaitSecs   = 120;
      KeyboardCheckSecs = 10;

type ShowLineProc     = procedure(const S: ShortString);
     HandleXOverType  = procedure(const S: ShortString);
     HandleListType   = procedure(const S: ShortString);
     HandleHeaderType = procedure(const S: ShortString);
     AbortSessionType = function: boolean;

type tNNTPclient = class(tTcpClient)
       protected
        fUserName    : String;
        fPassword    : String;
        fLastError   : Longint;
        fErrorStr    : String;
        fShowLine    : ShowLineProc;
        fAbortFunc   : AbortSessionType;

       public
        CanPost      : Boolean;

        procedure ShowAllText;

        constructor Create;
        destructor  Destroy; override;

        property Username : String READ fUserName WRITE fUserName;
        property Password : String READ fPassword WRITE fPassword;
        property ShowLine : ShowLineProc READ fShowLine WRITE fShowLine;
        property AbortFunc: AbortSessionType READ fAbortFunc WRITE fAbortFunc;

        function Logon: Boolean;

        function NntpMode(S: String): Boolean;
        function NewsGroupName(GroupPat: String; var Groupname, Description: String): Boolean;
        function XOVER(range: String; XoverStr: HandleXOVERtype): Boolean;
        function NewsGroupList(GroupList: HandleListtype): Boolean;
        function CheckError(const TestSet: Array of Word; var LastErr: Longint; NeedError, EatLine: Boolean): Boolean;
        function GetNewsGroup(GroupName: String; var FirstMsg, HighMsg: Longint; var ErrorStr: String): Boolean;
        function GetHeader(ArticleNr: Longint; HeaderStr: HandleHeaderType): Boolean;
        function GetArticle(ArticleNr: Longint; var MsgText: MessageTextType): Boolean;
        function PostArticle(GroupName: String; var MsgText: MessageTextType; var ErrorStr: String): Boolean;
        function Quit: Boolean;
     end; { tNNTP client }


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 uses ApTimer, LongStr, WordStr, SysUtils,
       SockDef, SockFunc, StUtils, Global,
        JDates,

          ellog_u;
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tNNTPClient.Create;
begin
  inherited Create;

  UserName := '';
  Password := '';
  CanPost  := false;
  fErrorStr:= '';
end; { constructor Create }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tNNTPClient.Destroy;
begin
  inherited Destroy;
end; { destructor Destroy }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tNntpClient.ShowAllText;
var RecvStr  : AnsiString;
    Count    : Longint;
begin
  Count := 800;

  While (Count > 0) AND (NOT DataAvailable) do
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

function tNNTPClient.Logon: Boolean;
var AbortLogon: Boolean;
begin
  { Expect the newsserver header, wait for it }
  FlushData;

  AbortLogon := false;
  Result := true;

  if UserName <> '' then
     begin
       SendStrLn('AUTHINFO USER '+UserName);

       if CheckError([482, 502], fLastError, true, true) then
         AbortLogon := true;
     end; { if }

  if PassWord <> '' then
     begin
       SendStrLn('AUTHINFO PASS '+Password);

       if CheckError([482, 502], fLastError, true, true) then
         Abortlogon := true;
     end; { if }

  if AbortLogon then Result := false;
end; { func. Logon }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tNNTPclient.NntpMode(S: String): Boolean;
begin
  NntpMode := true;

  if S <> '' then
    begin
      SendStrLn('MODE ' + S);

      if CheckError([200], fLastError, true, true) then
        begin
          CanPost := true;
        end; { if }
    end; { if }

end; { func. NntpMode }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tNNTPClient.NewsGroupName(GroupPat: String; var Groupname, Description: String): Boolean;
var TempStr: AnsiString;
begin
  GroupName := '';
  Description := '';
  Result := false;

  if GroupPat <> '' then
    begin
      SendStrLn('LIST NEWSGROUPS ' + GroupPat);

      if CheckError([215], fLastError, true, false) then
        begin
          if RecvStrLn(TempStr, false) then
            begin
              Result := true;
              GroupName := ExtractWord(TempStr, 1, defExtractWord, false, false);
              Description := Copy(TempStr, Length(GroupName) + 01, 255);
            end; { if }
        end; { if }
    end; { if }

end; { func. NewsGroupName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tNNTPClient.XOVER(range: String; XoverStr: HandleXOVERtype): Boolean;
var TempStr: AnsiString;
begin
  Result := false;

  if Range <> '' then
    begin
      SendStrLn('XOVER '+Range);

      if CheckError([224], fLastError, true, true) then
        begin
          TempStr := '';

          While (TempStr <> '.') AND (ConnectionAlive) do
           begin
             if RecvStrLn(TempStr, false) then
               begin
                 Result := true;
                 XoverStr(TempStr);
               end { if }
                else TempStr := '.';
           end; { while }
        end; { if }
    end; { if }

end; { func. XOVER }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tNNTPClient.NewsGroupList(GroupList: HandleListtype): Boolean;
var TempStr: AnsiString;
    Timer  : EventTimer;
    CheckKB: EventTimer;
begin
  if DataAvailable then FlushData;

  SendStrLn('LIST');

  if CheckError([215], fLastError, true, true) then
    begin
      TempStr := '';
      WaitForData;
      NewTimer(Timer, Secs2Tics(ArticleWaitSecs));
      NewTimer(CheckKB, Secs2Tics(KeyboardCheckSecs));

      While (TempStr <> '.') AND (NOT TimerExpired(Timer))  do
        begin
          if TimerExpired(CheckKB) then
            begin
              NewTimer(CheckKB, Secs2Tics(KeyboardCheckSecs));

              if fAbortFunc{$IFDEF FPC}(){$ENDIF} then BREAK;
              if (NOT ConnectionAlive) then BREAK;
            end; { if }

          if RecvStrLn(TempStr, false) then
            begin
              NewTimer(Timer, Secs2Tics(ArticleWaitSecs));
              Result := true;

              if TempStr <> '.' then
               GroupList(TempStr);
            end; { if }

          {$IFDEF WITH_DEBUG}
            DebugLog('>', 'TempStr= "'+TempStr+'"');
          {$ENDIF}
         end; { while }
    end { if }
      {$IFDEF WITH_DEBUG}
        else DebugLog('>', 'NewsGroupList -- Not rreturned 215 :-(')
      {$ENDIF} ;

end; { func. NewsGroupList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tNntpClient.CheckError(const TestSet: Array of Word; var LastErr: Longint; NeedError, EatLine: Boolean): Boolean;
var Counter       : Longint;
    FoundResponse : Longint;
    TempStr       : AnsiString;
    Value         : Longint;
begin
  if NeedError then WaitForData;

  RecvStrLn(TempStr, NOT EatLine);
  Result := false;

  FoundResponse := FVal(ExtractWord(TempStr, 1, defExtractWord, false, false));
  fErrorStr := TempStr;

  {$IFDEF WITH_DEBUG}
    DebugLog('>', 'CheckError - TempStr = "'+TempStr+'"');
    DebugLog('>', 'CheckError - High(testSet) = ' + FStr(High(TestSet)) + '"');
    DebugLog('>', 'CheckError - FoundResponse = ' + FStr(FoundResponse));
  {$ENDIF}

  for Counter := 00 to High(TestSet) do
    begin
      Value := TestSet[Counter];

      if FoundResponse = Value then
       begin
         LastErr := Value;
         Result := true;

         EXIT;
       end; { CheckReturn }
    end; { for }

end; { func. CheckReturn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tNntpClient.Quit: Boolean;
begin
  SendStrLn('QUIT');

  WaitForData;
  Result := TRUE;
end; { func. Quit }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tNNTPClient.GetNewsGroup(GroupName: String; var FirstMsg, HighMsg: Longint; var ErrorStr: String): Boolean;
var TempStr: AnsiString;
begin
  Result := false;
  FirstMsg := -1;
  Highmsg := -1;
  ErrorStr := '';

  if DataAvailable then FlushData;

  SendStrLn('GROUP '+GroupName);
  WaitForData;

  {$IFDEF WITH_DEBUG}
    DebugLog('>', 'GetNewsGroup - Going to call CheckError');
  {$ENDIF}

  if CheckError([211], fLastError, true, false) then
   begin
     if RecvStrLn(TempStr, false) then
       begin
         Result := true;

         FirstMsg := FVal(ExtractWord(TempStr, 3, defExtractWord, false, false));
         HighMsg := FVal(ExtractWord(TempStr, 4, defExtractWord, false, false));
       end; { if }
   end { if }
     else begin
            RecvStrLn(TempStr, false);
            Result := FALSE;
            ErrorStr := TempStr;                   { Newsgroup does not exist }
          end; { error }

  if NOT ConnectionAlive then GetNewsGroup := false;
end; { func. GetNewsGroup }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tNNTPClient.GetHeader(ArticleNr: Longint; HeaderStr: HandleHeaderType): Boolean;
var TempStr: AnsiString;
begin
  Result := false;

  SendStrLn('HEAD ' + FStr(ArticleNr));

  if CheckError([221], fLastError, true, true) then
    begin
      TempStr := '';

      While (TempStr <> '.') AND (ConnectionAlive) do
        begin
          if RecvStrLn(TempStr, false) then
            begin
              Result := true;
              HeaderStr(TempStr);
            end { if }
              else TempStr := '.';
        end; { while }
    end; { if }

end; { func. GetHeader }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tNNTPClient.GetArticle(ArticleNr: Longint; var MsgText: MessageTextType): Boolean;
var TempStr: AnsiString;
    TempLen: Longint;
    Timer  : EventTimer;
    CheckKb: EventTimer;
    TxtPtr : Longint;
begin
  FillChar(MsgText, SizeOf(MessageTextType), #00);
  TxtPtr := 00;

  Result := true;
  if DataAvailable then FlushData; { was: FlushData; }

  SendStrLn('ARTICLE '+FStr(ArticleNr));

  if CheckError([220], fLastError, true, true) then     { 220 successfull }
    begin
      NewTimer(Timer, Secs2Tics(ArticleWaitSecs));
      NewTimer(CheckKb, Secs2Tics(KeyboardCheckSecs));

      While (TempStr <> '.') AND (NOT TimerExpired(Timer)) do
        begin
          if TimerExpired(CheckKb) then
            begin
              NewTimer(CheckKb, Secs2Tics(KeyboardCheckSecs));

              if fAbortFunc{$IFDEF FPC}(){$ENDIF} then
                begin
                  BREAK;
                end; { if }
              if NOT (ConnectionAlive) then
                begin
                  BREAK;
                end; { if }
            end; { if }

          if NOT DataAvailable then DoSleep(0);

          if RecvStrLn(TempStr, false) then
           if TempStr <> '.' then
             begin
               NewTimer(Timer, Secs2Tics(ArticleWaitSecs));
               TempStr := TempStr + #13#10;
               TempLen := Length(TempStr);

               if (TxtPtr + TempLen) < SizeOf(MessageTextType) then
                 begin
                   Move(TempStr[1], MsgText[TxtPtr], TempLen);
                   Inc(TxtPtr, TempLen);
                 end
                  else begin
                         BREAK;
                       end; { else }
             end; { if }
        end; { while }
    end
      else begin                                                      { Error }
              Result := FALSE;
              FlushData;
           end; { else }
end; { func. GetArticle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tNntpClient.PostArticle(GroupName: String; var MsgText: MessageTextType; var ErrorStr: String): Boolean;
var TempStr: AnsiString;
    TxtPtr : Longint;
    CheckKb: EventTimer;
begin
  Result := true;
  ErrorStr := '';
  if DataAvailable then FlushData;       { Make sure previous data is flushed }

  SendStrLn('GROUP '+ GroupName);
  if CheckError([411], fLastError, true, false) then
    begin
      RecvStrLn(TempStr, false);
      ErrorStr := TempStr;                        { Newsgroup does not exist }
      Result := false;
      EXIT;
    end; { if }

  RecvStrLn(TempStr, false);                      { Get actual errorstring }

  SendStrLn('POST');
  if CheckError([340], fLastError, true, false) then
    begin
      RecvStrLn(TempStr, false);                  { Get actual errorstring }

      TxtPtr := 00;
      TempStr := '';

      NewTimer(CheckKB, Secs2Tics(KeyboardCheckSecs));

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

      SendStrLn('.');
      if CheckError([240], fLastError, true, true) then
        begin
          { Article posted correctly }
        end
          else begin
                 ErrorStr := fErrorStr;
                 Result := false;
               end; { not posted }
    end { if }
     else begin
            Result := false;
            ErrorStr := fErrorStr;
          end; { if }
end; { func. PostArticle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { nntpcli }
