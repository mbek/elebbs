unit WEB_CGI;
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

uses Dos, LongStr, web_abs;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

const
  cgi_Buffering: Boolean = true; { Buffer output before writing it }

type
  {-- Actual CGI object -------------------------------------------------------}
  web_CgiObject = object(web_SrvObject)
    cgi_StdOut  : Cardinal;

    {-- Constructor and destructors -------------------------------------------}
    constructor Init(QueryAdd: String);

    {-- Get some miscellaenous data -------------------------------------------}
    function  GetServerName: String; virtual;

    {-- Data send routines ----------------------------------------------------}
    {$IFNDEF MSDOS}
      procedure webSend(const MsgStr: AnsiString); virtual;
    procedure webOpenOutput(MimeType: AnsiString); virtual;
    {$ELSE}
      procedure webSend(const MsgStr: String); virtual;
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
    procedure RecvChar(var Ch: Char); virtual;       { Actually read the data }
  end; { web_CgiObject }


  pWeb_CgiObject = ^web_CgiObject;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses
   web_sup,
   Cases,
   GenDos

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

procedure web_CGIObject.RecvChar(var CH: Char);
{$IFDEF WIN32}
var BytesRead: ULONG;
    Temp     : Char;
{$ENDIF}

{$IFDEF OS2}
var BytesRead: Longint;
{$ENDIF}
begin
  {$IFDEF WIN32}
    Temp := CH;
    ReadFile(GetStdHandle(std_input_handle),
             Temp,
             SizeOf(Temp),
             BytesRead,
             nil);
    CH := Temp;
  {$ENDIF}

  {$IFDEF ELEUNIX}
    Read(Input, CH);
  {$ENDIF}

  {$IFDEF OS2}
    DosRead(0, CH, SizeOf(CH), BytesRead);
  {$ENDIF}

  {$IFNDEF WIN32}
   {$IFNDEF ELEUNIX}
    {$IFNDEF OS2}
      {$ERROR Hallo?}
    {$ENDIF}
   {$ENDIF}
  {$ENDIF}
end; { proc. RecvChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor web_CGIObject.Init(QueryAdd: String);
var Dummy   : LongInt;
    DataLen : LongInt;
    TempCH  : Char;
    Counter : Longint;
begin
  {-- Initialize some variables ----------------------------------------------}
  FillChar(Cgi_Buffer, SizeOf(cgi_Buffer), #0);
  ContentInitialized := FALSE;
  Field      := NIL;
  CurBuffer  := NIL;
  DataLen    := FVal(GetEnv('CONTENT_LENGTH'));
  Fields     := -1;
  Dummy      := 0;
  CurQueryPos:= 01;
  Cgi_BufLen := 0;

  {$IFDEF WIN32}
    cgi_StdOut := GetStdHandle(std_output_handle);
  {$ENDIF}

  {-- Get memory for the buffers ---------------------------------------------}
  New(CurBuffer);
  New(Field);

  {-- Initialize the GET fields ----------------------------------------------}
  if QueryAdd = '' then
    CurQueryStr := GetEnv('QUERY_STRING')
      else begin
             CurQueryStr := GetEnv('QUERY_STRING');

             if CurQueryStr <> '' then
               CurQueryStr := CurQueryStr + '&' + QueryAdd
                 else CurQueryStr := QueryAdd;
           end; { else }

  if GetEnv('REDIRECT_QUERY_STRING') <> '' then
    CurQueryStr := CurQueryStr + '&' + GetEnv('REDIRECT_QUERY_STRING');

  if CurQueryStr <> '' then
    CurQueryStr := CurQueryStr + '&';

  {-- Increase data length ---------------------------------------------------}
  Inc(DataLen, Length(CurQueryStr));

  {-- Now get all the data and divide it into fields -------------------------}
  REPEAT
    {-- Get this field -------------------------------------------------------}
    Inc(Fields);
    New(Field^[Fields]);
    FillChar(Field^[Fields]^, SizeOf(Field^[Fields]^), #00);

    {-- Now get all data for this field --------------------------------------}
    REPEAT
      {-- Fill the fields ----------------------------------------------------}
      with Field^[Fields]^ do
        begin
          if Succ(Dummy) < DataLen then
            dta_RecvChar(TempCH);                 { Read character from input }
          Inc(Dummy);

          {-- Get the field name ---------------------------------------------}
          While (TempCH <> '=') AND (Succ(Dummy) < DataLen) do
            begin
              {-- Convert plusses to whitespace ------------------------------}
              if TempCH = '+' then
                TempCH := #32;

              {-- All field names should be uppercase ------------------------}
              Name := Name + UpCase(TempCH);

              {-- Get the actual data ----------------------------------------}
              if Succ(Dummy) < DataLen then
                 dta_RecvChar(TempCH);
              Inc(Dummy);
            end; { while }

          {-- And enter the field name ---------------------------------------}
          Field^[Fields]^.Name := Name;

          {-- Now get the data we want ---------------------------------------}
          FieldSize := 00;
          FillChar(CurBuffer^, SizeOf(CurBuffer^), #00);

{  was (27-4-2002):        if Succ(Dummy) < DataLen then }
          if Succ(Dummy) <= DataLen then
            dta_RecvChar(TempCH);
          Inc(Dummy);

          While (TempCH <> '&') AND (Dummy <= DataLen) do
            begin
              {-- Convert plusses to whitespace ------------------------------}
              if TempCH = '+' then
                TempCH := #32;

              {-- Get this data ----------------------------------------------}
              CurBuffer^[FieldSize] := TempCH;
              Inc(FieldSize);

              {-- Make sure we dont overflow this buffer ---------------------}
              if (FieldSize + 1) > (SizeOf(CurBuffer^) - 1) then BREAK;

              {-- While possible ---------------------------------------------}
              if Succ(Dummy) <= DataLen then
                begin
                  dta_RecvChar(TempCH);
                end; { if }

              Inc(Dummy);
            end; { while }

          {-- Now move the data to a safe spot -------------------------------}
          GetMem(FieldData, FieldSize);
          Move(CurBuffer^, FieldData^, FieldSize);
        end; { with }

    {-- Until field terminator or end of data --------------------------------}
    UNTIL (TempCH = '&') OR (Dummy >= DataLen);

  UNTIL (Dummy >= DataLen);
end; { constructor INIT }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function web_CgiObject.GetServerName: String;
begin
  GetServerName := GetEnv('SERVER_NAME');
end; { func. GetServerName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF MSDOS}
procedure web_CgiObject.webSend(const MsgStr: String);
{$ELSE}
procedure web_CgiObject.webSend(const MsgStr: AnsiString);
var TmpChar: ^Char;
{$ENDIF}
{$IFDEF Win32}
var StartPos: DWORD;
{$ENDIF}
begin
  if NOT cgi_Buffering then
    begin
      Write(MsgStr);
    end
     else begin
            if MsgStr <> '' then
              begin
                if Length(MsgStr) < ((80 * 1024) - cgi_BufLen - 1) then
                  begin
                    StrCat(cgi_Buffer, pChar(MsgStr));

                    Inc(cgi_BufLen, Length(MsgStr));

                    {-- flush the output buffer if necessary ------------------}
                    if cgi_BufLen > (1024 * 40) then
                      FlushOutBuffer;
                  end
                    else begin
                           {-- this snippet would crash the buffer -----------}
                           FlushOutBuffer;

                           {-- and write the whole snippet at once -----------}
                           {$IFDEF WIN32}
                              WriteLn(MsgStr);
                           {$ELSE}
                              Write(MsgStr);     { Actually display }
                           {$ENDIF}
                         end; { else }
              end; { if }
          end; { else }
end; { proc. webSend }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure web_CgiObject.FlushOutBuffer;
{$IFDEF Win32}
var BytesRead: DWORD;
{$ENDIF}
begin
  {$IFDEF WIN32}
    WriteFile(cgi_StdOut,
              cgi_Buffer,
              cgi_BufLen,
              BytesRead,
              nil);
  {$ELSE}
    Write(Cgi_Buffer);                                   { Actually display }
  {$ENDIF}

  cgi_Buffer[0] := #0;                                   { reset the buffer }
  cgi_BufLen := 0;
end; { proc. FlushOutBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF MSDOS}
procedure web_CgiObject.webOpenOutput(MimeType: String);
{$ELSE}
procedure web_CgiObject.webOpenOutput(MimeType: AnsiString);
{$ENDIF}
begin
  if NOT ContentInitialized then
    begin
      ContentInitialized := TRUE;

      WebSendLn('Content-type: ' + MimeType);
      WebSendLn('');

      if cgi_Buffering then
        FlushOutbuffer;
    end; { if }
end; { proc. webOpenOutput }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF MSDOS}
procedure web_CgiObject.WebSendHeader(Content, Value: AnsiString);
{$ELSE}
procedure web_CgiObject.WebSendHeader(Content, Value: String);
{$ENDIF}
begin
  WebSendLn(Content + ' ' + Value);

  if cgi_Buffering then
    FlushOutBuffer;
end; { proc. WebSendHeader }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF MSDOS}
procedure web_CgiObject.WebSendCookie(CookieStr: AnsiString);
{$ELSE}
procedure web_CgiObject.WebSendCookie(CookieStr: String);
{$ENDIF}
begin
  WebSendHeader('Set-Cookie:', CookieStr);
end; { proc. WebSendCookie }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit WEB_CGI }
