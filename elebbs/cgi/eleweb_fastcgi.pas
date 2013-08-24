program eleweb_fastcgi;
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

{.$DEFINE DEBUGCGI}

{$IFNDEF ISCGI}
  Wont compile!!
{$ENDIF}
{$R-,S-,W-}
{&LocInfo+}

(*
**
** EleWEB FastCGI executable
**
** Copyright (c) 2001-2004 by Maarten Bekers
**
** Created: 06-Mar-2004
** Last update: 06-Mar-2004
**
**
*)
uses
    dos,                                    { environment variable handling }
{$IFDEF DEBUGCGI}    
    web_abs,
    web_cgi,
{$ENDIF}    
     web_fastcgi,                                   { CGI handling routines }
      fcgiapp,    
       web_gen,
        web_glob,                                               { global data }
         web_sup,                               { Support routines for EleWEB }
          web_scr,                                       { Scripting routines }
           longStr,                     { Converting strings to ints and v.v. }
            user_u,                          { User search routines and alike }
             filerout,                                { EleBBS-files routines }
              global,                                      { Global variables }
               crc_Unit,                                     { CRC32 routines }
                multiln,                                { Free nodenumber etc }
                 jdates,                              { Date and age routines }
                  terminal,                             { System / user codes }
                   CfgRec,                            { Data type definitions }
                    objDec,                            { several object decls }
                     strpath,
                      genfile,
                       SysUtils;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

const
  RequestPerExe = 150;  

var fcgi_in,
    fcgi_out,
    fcgi_err   : PFCGX_Stream;
    Environ    : FCGX_ParamArrayType;
    
    AllOk      : Boolean;

    web_Script : ShortString;
    elxError   : Integer;
    elxErrorStr: AnsiString; 
    
    ReqsHandled: Integer;

{$IFDEF DEBUGCGI}    
    counter: integer;
{$ENDIF}    

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
                 
function OutCgiWrite(Var F: TextRec): Integer;
var Temp: ShortString;
begin
  if F.BufPos > 0 then
    begin
      Move(F.Buffer[0], Temp[1], F.BufPos);
      Temp[0] := Chr(F.BufPos);

      FCGX_PutS(PChar(AnsiString(Temp)), fcgi_out^);
    end; { if }

  F.BufPos := 0;
  Result := 0;
end; { func. OutCgiWrite }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ErrCgiWrite(Var F: TextRec): Integer;
var Temp: ShortString;
begin
  if F.BufPos > 0 then
    begin
      Move(F.Buffer[0], Temp[1], F.BufPos);
      Temp[0] := Chr(F.BufPos);

      FCGX_PutS(PChar(AnsiString(Temp)), fcgi_out^);
    end; { if }

  F.BufPos := 0;
  Result := 0;
end; { func. ErrCgiWrite }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function OutFuncOpen(Var F: TextRec): Integer;
begin
  Result := 0
end; { func. OutFuncOpen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
                 
begin
  {-- Initialize some variables -----------------------------------------------}
  web_Initted := false;
  ReqsHandled := 0;

  {$IFDEF DEBUGCGI}
     Counter := 4000;
//counter := 1;
  {$ENDIF}
  
  {$IFNDEF DEBUGCGI}
  while (ReqsHandled < RequestPerExe) AND (FCGX_Accept(fcgi_in, fcgi_out, fcgi_err, environ) >= 0) do 
  {$ELSE}
  while (Counter > 0) do
  {$ENDIF}
    begin
      {-- and decrease the loop count -----------------------------------------}
      ReqsHandled := ReqsHandled + 1;
      
      {-- and run the whole mess ----------------------------------------------}
      {$IFDEF DEBUGCGI}
        Counter := Counter - 1;
        CgiObj := New(pWeb_CgiObject, Init(''));
      {$ELSE}
        CgiObj := New(pWeb_FastCgiObject, Init('', fcgi_in, fcgi_out, fcgi_err, environ));
      {$ENDIF}

CgiObj^.webopenoutput('text/html;');
    	
      {-- if this is the first client request, lets initialize stuff ----------}
      if NOT web_Initted then
        begin
  	  RunningWEB := TRUE;

          {-- first we re assign standard output and error to the cgi system --}
{$IFNDEF DEBUGCGI}          
	  with TextRec(Output) do
	    begin
	      Handle := $FFFF;
    	      Mode := fmClosed;
              BufSize := sizeof(buffer);
              BufPtr := @Buffer;
              InOutFunc := @OutCgiWrite;
              FlushFunc := @OutCgiWrite;
              OpenFunc := @OutFuncOpen;
              Name[0] := #0;
            end; { Output }
          System.Rewrite(output);

          {-- first we re assign standard output and error to the cgi system --}
	  with TextRec(StdErr) do
	    begin
	      Handle := $FFFF;
    	      Mode := fmClosed;
              BufSize := sizeof(buffer);
              BufPtr := @Buffer;
              InOutFunc := @ErrCgiWrite;
              FlushFunc := @ErrCgiWrite;
              OpenFunc := @OutFuncOpen;
              Name[0] := #0;
            end; { StdErr }
          System.Rewrite(StdErr);
{$ENDIF}

          {-- First initialize some variables ---------------------------------}
          web_Access := FALSE;

          {-- and skip along --------------------------------------------------}
          web_InitMemory;                    { Allocate memory for the globals }
          web_RunLocal := false;

          {-- read config.ra and stuff ----------------------------------------}
                                                   { read all the system files }
          web_Initted := web_InitSystemFiles(NOT web_RunLocal); 
        end; { if }
 
      {-- Default to success --------------------------------------------------}
      AllOk := true;
 
      {-- Now get the default fields we always get ----------------------------}
      {$IFNDEF DEBUGCGI}
      if NOT RequireLongField(web_Action,
                             'action',
                             'Unable to retrieve "action" field - aborting EleWEB') then
       AllOk := false;

      {-- Now get the default fields we always get -----------------------------}
      if AllOk then
         RequireStringField(web_Script,
                            'script',
                            'Unable to retrieve "script" field - aborting EleWEB');
      {$ELSE}
        web_Script := 'eleforum';
      {$ENDIF}

      {-- run any script we want to --------------------------------------------}
      if AllOk then
        begin
           elxCache_RunModule('C:\system\web\script\' + web_Script,
                              '',
                              ElxError,
                              ElxErrorStr);
          if ElxError > 0 then
             begin
               web_FatalError(web_Err_ScrNtFnd, web_Script + ' (#' + FStr(ElxError) + ') - '  + ElxErrorStr + ')');               
               
               AllOk := false;
             end; { if }
        end; { if }

     {-- dispose of our CGI program again --------------------------------------}
     Dispose(CgiObj, Done);
    end; { while }    
end. { EleWEB_fastcgi }
