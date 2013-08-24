program EleWEB;
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

{$IFNDEF ISCGI}
  Wont compile!!
{$ENDIF}

{$R+,S+,W+}

{$IFDEF FPC}
  {$M 85385499, 1955000}
{$ELSE}
  {$M 853849, 1955000}
{$ENDIF}
{&LocInfo+}
(*
**
** EleWEB main executable
**
** Copyright (c) 2001-2003 by Maarten Bekers
**
** Created: 21-Apr-2001
** Last update: 21-Jan-2003
**
**
*)
uses
   {$IFDEF OS2}
     Crt,
   {$ENDIF}

   {$IFDEF ELX_MYSQL}
     MySql,
   {$ENDIF}
    web_gen,                                       { General EleWEB routines }
     dos,                                    { environment variable handling }
      web_cgi,                                       { CGI handling routines }
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
                   objDec;                            { several object decls }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure RunLocal;
var elxError   : Integer;
    elxErrorStr: AnsiString;
begin
  {-- Disable buffering to get a more instant response ---------------------}
  cgi_Buffering := false;

  {-- display an header ----------------------------------------------------}
  WriteLn;
  WriteLn;
  WriteLn('ELEWEB; CGI interface to EleBBS, Version '+VersionID);
  WriteLn('        Copyright 1997-2003 Maarten Bekers, All rights reserved.');
  WriteLn;
  WriteLn;

  {-- now check if there were parameters given -----------------------------}
  if ParamCount < 1 then
    begin
       WriteLn('Missing scriptname on command line!');
       WriteLn;
       WriteLn('Usage: EleWEB.EXE <scriptname> [parameters]');
       WriteLn;
       WriteLn('example: EleWEB.EXE initevent eventid=1&runtime=12&tt=12');
       WriteLn;
       Flush(Output);
       Halt(255);
    end; { if }

  {-- now re-initialize the cgi object to pass our own parameters ----------}
  {$IFNDEF VER1_1_0} Dispose(CgiObj, Done); {$ENDIF}

  {-- initting the actual cgi object ---------------------------------------}
  CgiObj := New(pWeb_CgiObject, Init(ParamStr(2)));

  {-- clear the script path ------------------------------------------------}
  LineCfg^.Language^.QuesPath := '';

  {-- and execute the script -----------------------------------------------}
  if NOT RunElexerScripts(ParamStr(1), '', false, false, ElxError, ElxErrorStr) then
    begin
      WriteLn('Unable to run ', ParamStr(1), ' (#', ElxError, ' - ', ElxErrorStr,')');
    end; { if }
end; { proc. RunLocal }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

var InitIsFatal: Boolean;
begin
  {-- Trying to initialize the several modules -----------------------------}
  {$IFDEF ELX_MYSQL}
    {$IFDEF Win32}
       {!InitializeMySQL;}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF OS2}
    Assign(Output, '');
    Rewrite(Output);

    Assign(Input, '');
    Reset(Input);
  {$ENDIF}

  {-- let the system know were running EleWEB ------------------------------}
  RunningWEB := TRUE;

  {-- First initialize some variables --------------------------------------}
  web_Access := FALSE;

  {-- initting the actual cgi object ---------------------------------------}
  CgiObj := New(pWeb_CgiObject, Init(''));

  {-- and skip along -------------------------------------------------------}
  web_InitMemory;                         { Allocate memory for the globals }
  web_RunLocal := NOT web_ServerRunning;

  if (FVal(CgiObj^.GetFieldByName('action', true)) <> 11) then
    begin
      InitIsFatal := TRUE;
      if web_RunLocal then{ If a local run, config.ra etc are not necessary }
        InitIsFatal := FALSE;

      {-- if none of the EleBBS and EleWEB environment variables are -------}
      {-- are defined, we wont display a warning anymore. ------------------}
      if (GetEnv('ELEBBS') = '') AND (GetEnv('ELEWEB') = '') then
        InitIsFatal := false;

      {-- and actually initialize ------------------------------------------}
      web_Initted := web_InitSystemFiles(InitIsFatal)  { read all the system files }
    end
      else web_Initted := true;

  {-- if we have succesfully initialized EleWEB run the rest ---------------}
  if web_Initted then
    begin
      if NOT web_RunLocal then
        RunEleWeb
         else RunLocal;
    end; { if }

  {-- Flush the cgi buffer -------------------------------------------------}
  cgiObj^.FlushOutBuffer;

  {-- flush output buffer --------------------------------------------------}
  Flush(Output);

  {-- Dispose of variables -------------------------------------------------}
  {$IFNDEF VER1_1_0} Dispose(CgiObj, Done); {$ENDIF}
end. { EleWEB }
