unit WEB_APACHE1;
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
** WEB_APACHE1 - Apache Moduel (v1.3x) routines.
** Implements the basic routines into this program.
**
** Copyright (c) 2003 by Maarten Bekers
**
** Created : 08-Feb-2003
** Last update : 08-Feb-2003
**
** Notes:
**   The program automaticly pastes the data from a GET and a POST together.
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Ap1_Base, web_abs;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

const
  cgi_Buffering : boolean = false;

type
  {-- Actual CGI object -------------------------------------------------------}
  web_ApacheObject = object(web_SrvObject)
    RequestRec  : pRequest_Rec; 	         { request record from apache }
    
    {-- Constructor and destructors -------------------------------------------}
    constructor Init(QueryAdd: String; var r: prequest_rec);

    {-- Get some miscellaenous data -------------------------------------------}
    function  GetServerName: String; virtual;

    {-- Data send routines ----------------------------------------------------}
    procedure webSend(const MsgStr: AnsiString); virtual;

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
  end; { web_ApacheObject }

  pWeb_ApacheObject = ^web_ApacheObject;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses
  web_Glob,

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

procedure web_ApacheObject.RecvChar(var CH: Char);
begin
  if boolean(ap_should_client_block(RequestRec)) then
    begin
      ap_get_client_block(RequestRec, @CH, SizeOf(CH));
    end; { if }      
end; { proc. RecvChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor web_ApacheObject.Init(QueryAdd: String; var r: prequest_rec);
var Dummy   : LongInt;
    DataLen : LongInt;
    TempCH  : Char;
begin
  {-- setup reading of post data ---------------------------------------------}
  RequestRec := r;
  ap_setup_client_block(RequestRec, REQUEST_CHUNKED_DECHUNK);
	
  {-- Initialize some variables ----------------------------------------------}
  ContentInitialized := FALSE;
  Field      := NIL;
  CurBuffer  := NIL;

(*
  if RequestRec^.SubProcess_Env <> nil then
    DataLen    := StrToInt(ap_table_get(RequestRec^.SubProcess_Env, 'CONTENT_LENGTH'))
     else*) Datalen := 0;

  Fields     := -1;
  Dummy      := 0;
  CurQueryPos:= 01;
  Cgi_BufLen := 0;

  {-- Get memory for the buffers ---------------------------------------------}
  New(CurBuffer);
  New(Field);

  {-- Initialize the GET fields ----------------------------------------------}
  if QueryAdd = '' then
    CurQueryStr := RequestRec^.Args
      else begin
             CurQueryStr := RequestRec^.Args;

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

function web_ApacheObject.GetServerName: String;
begin
  GetServerName := RequestRec^.Server^.server_Hostname;
end; { func. GetServerName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure web_ApacheObject.webSend(const MsgStr: AnsiString);
begin
  if NOT ContentInitialized then
    webOpenOutput(web_DefMimeType);

  ap_rputs(PChar(MsgStr), RequestRec);
end; { proc. webSend }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure web_ApacheObject.FlushOutBuffer;
begin
end; { proc. FlushOutBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure web_ApacheObject.webOpenOutput(MimeType: AnsiString);
begin
  if NOT ContentInitialized then
    begin
      RequestRec^.Content_Type := PChar(MimeType);
      ap_send_http_header(RequestRec);
       	
      ContentInitialized := TRUE;
    end; { if }
end; { proc. webOpenOutput }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure web_ApacheObject.WebSendHeader(Content, Value: AnsiString);
begin
  ap_table_set(RequestRec^.headers_out,
                PChar(AnsiString(Content)),
                PChar(AnsiString(Value)));
end; { proc. WebSendHeader }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure web_ApacheObject.WebSendCookie(CookieStr: AnsiString);
begin
  WebSendHeader('Set-Cookie:', CookieStr);
end; { proc. WebSendCookie }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit WEB_APACHE1 }
