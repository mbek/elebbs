unit WEB_ABS;
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
{$I Compiler.inc}
{$IFNDEF MSDOS}
  {$H-}
{$ENDIF}
(*
**
** WEB_CGI - CGI routines for EleWEB
**
** Copyright (c) 1998-2001 by Maarten Bekers
**
** Created : 16-Apr-2001
** Last update : 16-Apr-2001
**
** Note: (c)1996 by Gerhard Hoogterp
**
** Notes:
**   The program automaticly pastes the data from a GET and a POST together.
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Dos, LongStr;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

type
  tFormField    = Record
     Name        : String[60];
     FieldData   : Pointer;
     FieldSize   : Longint;
  end; { tFormField }

  pFormField    = ^tFormField;

  {-- List with all fields ----------------------------------------------------}
  {$IFNDEF MSDOS}
    tFieldList    = Array[0..$FFFF] Of pFormField;
    pFieldList    = ^tFieldList;

    {-- Buffer, used later on to store temporarily data in --------------------}
    tBuffer       = Array[0..$FFFFF] of Char;
  {$ELSE}
    tFieldList    = Array[0..$FFF] Of pFormField;
    pFieldList    = ^tFieldList;

    {-- Buffer, used later on to store temporarily data in --------------------}
    tBuffer       = Array[0..$FFF] of Char;
  {$ENDIF}

  {-- Actual base object ------------------------------------------------------}
  web_SrvObject = object
    Field       : pFieldList;                             { The actual fields }
    Fields      : LongInt;                                 { Number of fields }

    CurQueryPos : Longint;                           { Position in the string }
    CurQueryStr : String;                        { Data passed as parameter }
    CurBuffer   : ^tBuffer;              { Buffer to hold temporarily data in }

    cgi_buffer  : Array[0..(180 * 1024)] of Char;
    cgi_BufLen  : Cardinal;
    ContentInitialized: Boolean;               { webOpenOutput already called }

    {-- Constructor and destructors -------------------------------------------}
    constructor Init(QueryAdd: String);
    destructor  Done; virtual;

    {-- Field (POST and GET) types --------------------------------------------}
    function  FoundField(Name: String): Longint; virtual;
    function  GetFieldByName(Name: String; ConvertField: Boolean): String; virtual;
    function  BigFieldByName(Name: String; var Size: LongInt): Pointer; virtual;

    {-- Special environment variable ------------------------------------------}
    function GetEnv(Env: String): String; virtual;

    {-- Get some miscellaenous data -------------------------------------------}
    function  GetServerName: String; virtual;

    {-- Data send routines ----------------------------------------------------}
    {$IFNDEF MSDOS}
      procedure webSend(const MsgStr: AnsiString); virtual;
      procedure webSendLn(const MsgStr: AnsiString); virtual;
      procedure webOpenOutput(MimeType: AnsiString); virtual;
    {$ELSE}
      procedure webSend(const MsgStr: String); virtual;
      procedure webSendLn(const MsgStr: String); virtual;
      procedure webOpenOutput(MimeType: String); virtual;
    {$ENDIF}

    {-- webopenoutput ---------------------------------------------------------}
    procedure FlushOutBuffer; virtual;
    {$IFNDEF MSDOS}
      procedure WebSendCookie(CookieStr: AnsiString); virtual;
      procedure WebSendHeader(Content,Value: AnsiString); virtual;
    {$ELSE}
      procedure WebSendCookie(CookieStr: String); virtual;
      procedure WebSendHeader(Content,Value: String); virtual;
    {$ENDIF}


    {-- Data retrieval routines -----------------------------------------------}
    procedure RecvChar(var Ch: Char);virtual;        { Actually read the data }

    procedure dta_RecvChar(var Ch: Char); virtual;
                                           { Wrapper routine to transparently }
                                                      { handle POSTS and GETS }
  end; { web_SrvObject }

  pWeb_SrvObject = ^web_SrvObject;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses
   Cases

  {$IFDEF WIN32}
    ,SysUtils
    ,Windows
  {$ENDIF}

  {$IFDEF ELEUNIX}
    ,Sysutils
    {$IFDEF VER1_0_10}
      ,Linux
    {$ELSE}
      ,Unix
    {$ENDIF}
  {$ENDIF}

  {$IFDEF OS2}
    ,SysUtils
    ,Os2Base
  {$ENDIF}

  ;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{ -- data-recvhcar }
{ -- done }
{ -- bigfieldbyname }
{ -- getfieldbyname }
{ -- foundfield }
{ -- websendln }

procedure web_SrvObject.dta_RecvChar(var CH: Char);
begin
  {-- This routine transparently handles POST and GETS data and pastes -------}
  {-- it together ------------------------------------------------------------}
  if CurQueryPos <= Length(CurQueryStr) then
    begin
      CH := CurQueryStr[CurQueryPos];
      Inc(CurQueryPos)
    end
      else RecvChar(CH);
end; { proc. dta_RecvChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure web_SrvObject.RecvChar(var CH: Char);
begin
end; { proc. RecvChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor web_SrvObject.Init(QueryAdd: String);
begin
end; { constructor INIT }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor web_SrvObject.Done;
var Counter: Longint;
begin
  {-- Dispose of all data ----------------------------------------------------}
  for Counter := 0 to Fields do
    begin
      With Field^[Counter]^ do
        FreeMem(FieldData, FieldSize);

      Dispose(Field^[Counter]);
      Field^[Counter] := NIL;
    end; { for }

  {-- Dispose the fields -----------------------------------------------------}
  if Field <> NIL then Dispose(Field);
  if CurBuffer <> NIL then Dispose(CurBuffer);
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function web_SrvObject.GetServerName: String;
begin
end; { func. GetServerName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function web_SrvObject.BigFieldByName(Name: String; var Size: LongInt): Pointer;
var FieldID: Longint;
begin
  {-- Lookup the field -------------------------------------------------------}
  FieldID := FoundField(Name);
  if FieldID < 0 then
    begin
      BigFieldByName := nil;
      EXIT;
    end; { if }

  {-- Return the data --------------------------------------------------------}
  BigFieldByName := Field^[FieldID]^.FieldData;
  Size := Field^[FieldID]^.FieldSize;
end; { func. BigFieldByName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TranslateHex(S: String): String;
var Temp : Longint;
    Nr   : Longint;
    {$IFNDEF MSDOS}
      Error: LongInt;
    {$ELSE}
      Error: Integer;
    {$ENDIF}
begin
  Temp := 01;

  while Temp < Length(s) do
    begin
      if S[Temp]='%' then
        begin
          if S[Temp + 1] in ['0'..'9'] then
            begin
              Val('$' + S[Temp+1] + S[Temp+2], Nr, Error);
              S[Temp] := Chr(Nr);

              Delete(S, Temp+1, 2);
            end; { if }
        end; { if }

      Inc(Temp);
    end; { while }

  TranslateHex := S;
end; { func. TranslateHex }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function web_SrvObject.GetFieldByName(Name: String; ConvertField: Boolean): String;
var FieldID: Longint;
    TempStr: String;
begin
  {-- Lookup the field -------------------------------------------------------}
  FieldID := FoundField(Name);
  if FieldID < 0 then
    begin
      GetFieldByName := '';
      EXIT;
    end; { if }

  {-- Make sure we can handle these sorts of fields --------------------------}
  if Field^[FieldID]^.FieldSize > 255 then
    begin
      GetFieldByName := 'Field to big for string!';
      EXIT;
    end; { if }

  {-- Return the string ------------------------------------------------------}
  Move(Field^[FieldID]^.FieldData^, TempStr[1], Field^[FieldID]^.FieldSize);
  {$IFDEF TP70}
    SetLength(TempStr, Field^[FieldID]^.FieldSize);
  {$ELSE}
    TempStr[0] := Chr(Field^[FieldID]^.FieldSize);
  {$ENDIF}

  {-- Should we convert this string? -----------------------------------------}
  if ConvertField then
    TempStr := TranslateHex(TempStr);

  {-- Return the string ------------------------------------------------------}
  GetFieldByName := TempStr;
end; { func. GetFieldByName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function web_SrvObject.FoundField(Name: String): Longint;
var FieldID: Longint;
begin
  {-- Initialize some variables -----------------------------------------------}
  FoundField := -1;
  FieldID := -1;
  Name := SUpCase(Name);
  if Fields < 0 then EXIT;

  {-- Lets see if the we know this field --------------------------------------}
  REPEAT
    Inc(FieldID);
  UNTIL (FieldID > Fields) OR (Name = Field^[FieldID]^.Name);

  {-- If we did not find it exit else return the value ------------------------}
  if FieldID > Fields then EXIT
    else FoundField := FieldID;
end; { func. FoundField }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF MSDOS}
procedure web_SrvObject.webSendLn(const MsgStr: String);
{$ELSE}
procedure web_SrvObject.webSendLn(const MsgStr: AnsiString);
{$ENDIF}
begin
  {$IFNDEF ELEUNIX}
    webSend(MsgStr + #13#10);
  {$ELSE}
    webSend(MsgStr + #10);
  {$ENDIF}
end; { proc. webSendLn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF MSDOS}
procedure web_SrvObject.webSend(const MsgStr: String);
{$ELSE}
procedure web_SrvObject.webSend(const MsgStr: AnsiString);
{$ENDIF}
begin
end; { proc. webSend }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure web_SrvObject.FlushOutBuffer;
begin
end; { proc. FlushOutBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF MSDOS}
procedure web_SrvObject.webOpenOutput(MimeType: AnsiString);
{$ELSE}
procedure web_SrvObject.webOpenOutput(MimeType: String);
{$ENDIF}
begin
end; { proc. webOpenOutput }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF MSDOS}
procedure web_SrvObject.WebSendHeader(Content, Value: AnsiString);
{$ELSE}
procedure web_SrvObject.WebSendHeader(Content, Value: String);
{$ENDIF}
begin
end; { proc. WebSendHeader }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF MSDOS}
procedure web_SrvObject.WebSendCookie(CookieStr: AnsiString);
{$ELSE}
procedure web_SrvObject.WebSendCookie(CookieStr: String);
{$ENDIF}
begin
end; { proc. WebSendCookie }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function web_SrvObject.GetEnv(Env: String): String;
begin
  GetEnv := Dos.GetEnv(Env);
end; { func. GetEnv }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit WEB_ABS }
