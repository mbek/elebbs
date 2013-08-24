unit WEB_FastCGI;
(*
**
** Copyright (C) 1999-2004 Maarten Bekers. All rights reserved.
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
** WEB_FASTCGI - FastCGI interface routines
** Implements the basic routines into this program.
** Consult www.fastcgi.com for infomation about the specific API
**
** Copyright (c) 2004 by Maarten Bekers
**
** Created : 06-Mar-2003
** Last update : 06-Mar-2003
**
** Notes:
**   The program automaticly pastes the data from a GET and a POST together.
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses fcgiapp, web_abs;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

const
  cgi_Buffering : boolean = false;

type
  {-- Actual CGI object -------------------------------------------------------}
  web_FastCgiObject = object(web_SrvObject)
    fcgi_in,
    fcgi_out,
    fcgi_err      : PFCGX_Stream;  
    EnvironList   : FCGX_ParamArrayType;
    
    {-- Constructor and destructors -------------------------------------------}
    constructor Init(QueryAdd: String;
                     src_in,
                     src_out,
                     src_err    : PFCGX_Stream;
                     src_environ: FCGX_ParamArrayType);

    {-- Get some miscellaenous data -------------------------------------------}
    function  GetServerName: String; virtual;

    {-- Data send routines ----------------------------------------------------}
    procedure webSend(const MsgStr: AnsiString); virtual;
    procedure webSendLn(const MsgStr: AnsiString); virtual;

    function GetEnv(Env: String): String; virtual;
    
    {-- webopenoutput ---------------------------------------------------------}
    procedure webOpenOutput(MimeType: AnsiString); virtual;
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
  end; { web_FastCgiObject }

  pWeb_FastCgiObject = ^web_FastCgiObject;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses
  web_Glob,
  LongStr,
  Dos,

  {$IFDEF WIN32}
    SysUtils
    ,Windows
  {$ENDIF}

  {$IFDEF ELEUNIX}
    Sysutils
    ,Unix
  {$ENDIF}

  {$IFDEF OS2}
    SysUtils
    ,Os2Base
  {$ENDIF}

  ;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure web_FastCgiObject.RecvChar(var CH: Char);
begin
  // return the character 
  CH := char(FCGX_GetChar(fcgi_in^));
end; { proc. RecvChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor web_FastCgiObject.Init(QueryAdd: String;
		     src_in,
                     src_out,
                     src_err    : PFCGX_Stream;
                     src_environ: FCGX_ParamArrayType);
var QueryValue: PChar;
   
    Dummy     : LongInt;
    DataLen   : LongInt;
    TempCH    : Char;
    Counter   : Longint;
begin
  {-- Take over variables ----------------------------------------------------}
  fcgi_in := src_in;
  fcgi_out := src_out;
  fcgi_err := src_err;
  EnvironList := src_environ;
  	
  {-- Initialize some variables ----------------------------------------------}
  ContentInitialized := FALSE;
  Field      := NIL;
  CurBuffer  := NIL;

  {-- Get the content length if specified ------------------------------------}
  QueryValue := FCGX_GetParam('QUERY_STRING', EnvironList);
  if QueryValue <> nil then
    DataLen := FVal(StrPas(QueryValue));
  
  Fields     := -1;
  Dummy      := 0;
  CurQueryPos:= 01;
  Cgi_BufLen := 0;

  {-- Get memory for the buffers ---------------------------------------------}
  New(CurBuffer);
  New(Field);

  {-- Initialize the GET fields ----------------------------------------------}
  if QueryAdd = '' then
    CurQueryStr := FCGX_GetParam('QUERY_STRING', EnvironList)
      else begin
             CurQueryStr := FCGX_GetParam('QUERY_STRING', EnvironList);

             if CurQueryStr <> '' then
               CurQueryStr := CurQueryStr + '&' + QueryAdd
                 else CurQueryStr := QueryAdd;
           end; { else }

  if CurQueryStr <> '' then
    CurQueryStr := CurQueryStr + '&';

  {-- Increase data length ---------------------------------------------------}
  Inc(DataLen, Length(CurQueryStr));

  {-- Now get all the data and divide it into fields -------------------------}
  REPEAT
    {-- Get this field -------------------------------------------------------}
    Inc(Fields);
    New(Field^[Fields]);
    Field^[Fields]^.Name := '';

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
          CurBuffer^[FieldSize + 1] := #0;
          Move(CurBuffer^, FieldData^, FieldSize);
        end; { with }

    {-- Until field terminator or end of data --------------------------------}
    UNTIL (TempCH = '&') OR (Dummy >= DataLen);

  UNTIL (Dummy >= DataLen);
end; { constructor INIT }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function web_FastCgiObject.GetServerName: String;
begin
  GetServerName := FCGX_GetParam('SERVER_NAME', EnvironList);
end; { func. GetServerName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure web_FastCgiObject.webSend(const MsgStr: AnsiString);
var f: text;
begin
// opening output conflicts with stuff as setting cookies etc	
//  if NOT ContentInitialized then
//    webOpenOutput(web_DefMimeType);

  FCGX_PutS(PChar(MsgStr), fcgi_out^);
end; { proc. webSend }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure web_FastCgiObject.FlushOutBuffer;
begin
end; { proc. FlushOutBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure web_FastCgiObject.webOpenOutput(MimeType: AnsiString);
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

procedure web_FastCgiObject.WebSendHeader(Content, Value: AnsiString);
begin
  WebSendLn(Content + ' ' + Value);

  if cgi_Buffering then
    FlushOutBuffer;
end; { proc. WebSendHeader }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure web_FastCgiObject.WebSendCookie(CookieStr: AnsiString);
begin
  WebSendHeader('Set-Cookie:', CookieStr);
end; { proc. WebSendCookie }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure web_FastCgiObject.webSendLn(const MsgStr: AnsiString);
begin
  {$IFNDEF ELEUNIX}
    webSend(MsgStr + #13#10);
  {$ELSE}
    webSend(MsgStr + #10);
  {$ENDIF}
end; { proc. webSendLn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function web_FastCgiObject.GetEnv(Env: String): String;
begin
  // First ttry the basic environment variabeles
  Result := Dos.GetEnv(Env);
  
  // if necessary we fall back
  if Result = '' then
    begin
      Result := FCGX_GetParam(PChar(AnsiString(Env)), EnvironList);
    end; { if }  
end; { func. GetEnv }
    
end. { unit web_fastcgi }
