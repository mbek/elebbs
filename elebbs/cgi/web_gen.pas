unit WEB_GEN;
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
(*
**
** WEB_GEN, General routines for EleWEB and modules
** Copyright (c) 1999-2003 by Maarten Bekers
**
**
** Created: 16-Apr-2001
** Last update : 09-Feb-2003
**
*)

{$IFNDEF ISCGI}
  You need to define "ISCGI" in order to compile the CGIs
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure WebLogin;
procedure WebLogout;
procedure WebRunAny(WebAction: Longint);
procedure webRunChecks;
procedure RunEleWeb;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
uses
    web_abs,			   	    { base abstraction cgi obj class }
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
                   objDec,                            { several object decls }
                    strpath,              { filename/directory manipulations }
                     gendos;           { Unit for Delphi (dos) compatibility }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure WebLogin;
var web_UserName   : String;               { Username passed to this script }
    web_PasswordCRC: Longint;
    elxErrorStr    : AnsiString;
    elxError       : Integer;
begin
  {-- initialize password CRC ----------------------------------------------}
  web_PasswordCRC := -1;

  {-- First get the fields we want -----------------------------------------}
  if CgiObj^.FoundField('ele_userrec') < 0 then
    begin
      if NOT RequireStringField(web_Username,
                                'ele_username',
                                'Unable to retrieve "ele_username" field - aborting EleWEB')
        then EXIT;
    end; { if }

  {-- First get the fields we want -----------------------------------------}
  web_passwordCrc := RaCrc(CgiObj^.GetFieldByName('ele_password', true), true);
  if CgiObj^.FoundField('ele_passwordcrc') >= 0 then
    web_PasswordCrc := FVal(CgiObj^.GetFieldByName('ele_passwordcrc', true));

  {-- First make sure we know this user ------------------------------------}
  if CgiObj^.FoundField('ele_userrec') < 0 then
    begin
      web_UIR := SearchUser(web_UserName);
    end
      else web_UIR := FVal(CgiObj^.GetFieldByName('ele_userrec', true));

  {-- if this is an unknown user, error and exit ---------------------------}
  if web_UIR < 0 then
    begin
      {-- Run the script to show this error --------------------------------}
      Web_FatalError(web_err_UnkUser, web_UserName);

      {-- abort logon procedure --------------------------------------------}
      EXIT;
    end; { if }

  {-- user is known, lets lookup the record --------------------------------}
  GetUserRecord(web_UIR,
                LineCfg^.Exitinfo^.Userinfo,
                LineCfg^.UserExtension^,
                true);                        { update ANSI and userage etc }
                                         { Very important else the 0 record }
  LineCfg^.Exitinfo^.UserRecord := web_UIR;          { will be overwritten! }

  {-- now validate the password --------------------------------------------}
  if (web_PasswordCrc <> LineCfg^.Exitinfo^.Userinfo.PasswordCRC) then
    begin
      {-- Run the script to show this error --------------------------------}
      Web_FatalError(web_err_InvPwd, '');

      {-- abort logon procedure --------------------------------------------}
      EXIT;
    end; { if }

  {-- update the password hash ---------------------------------------------}
  web_UIP := LineCfg^.Exitinfo^.Userinfo.PasswordCRC;

  {-- Actually log the user in ---------------------------------------------}
  {-- this is a completely silent login ------------------------------------}
  web_PerformSilentLogin;

  {-- now run the TOP script -----------------------------------------------}
  if NOT RunElexerScripts(InternalScripts[web_Login], '', true, true, ElxError, ElxErrorstr) then { Script to run }
    begin
      web_FatalError(web_Err_ScrNtFnd, InternalScripts[web_Login]);
      EXIT;
    end; { if }
end; { proc. WebLogin }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure WebLogout;
var elxError    : Integer;
    elxErrorStr : AnsiString;
begin
  {-- run this script to get user credentials ------------------------------}
  RunEleXerScripts(InternalScripts[web_GetCreds], '', true, true, ElxError, elxErrorStr);

  {-- Run the script to show this error ------------------------------------}
  if NOT web_Access then
    begin
      Web_FatalError(web_err_AccDnd, '');
      EXIT;
    end; { if }

  {-- Update the lastcall etc settings -------------------------------------}
  web_DoLogOut;

  {-- now remove it from useron.bbs ----------------------------------------}
  web_DeleteOldLogons;

  {-- now run the TOP script -----------------------------------------------}
  RunElexerScripts(InternalScripts[web_LogOut], '', true, true, ElxError, elxErrorStr);  { Script to run }
end; { proc. WebLogOut }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure WebRunAny(WebAction: Longint);
var web_Script : String;
    elxError   : Integer;
    elxErrorStr: AnsiString;

    TmpPos     : Integer;
    TmpStr     : String;
begin
  {-- Now get the default fields we always get -----------------------------}
  if CgiObj^.FoundField('script') >= 0 then
    begin
      RequireStringField(web_Script,
                         'script',
                         'Unable to retrieve "script" field - aborting EleWEB');
    end
      else begin
             TmpStr := GetEnv('SCRIPT_FILENAME');
             TmpPos := Pos('?', TmpStr);

             if TmpPos = 0 then
               TmpPos := Length(TmpStr);

             web_Script := Copy(TmpStr, 1, TmpPos);
           end; { else }

  {-- strip the path -------------------------------------------------------}
  {-- this is specially for when EleWEB is associated with an filetype in --}
  {-- Apache - it will default to passing the full path, which we strip ----}
  if webAction = web_runAnyPth then
    web_Script := NoExtension(JustName(web_Script));

  {-- run any script we want to --------------------------------------------}
  if NOT RunElexerScripts(web_Script, '', true, true, ElxError, ElxErrorStr) then
    begin
      if NOT RunElexerScripts(JustPath(ParamStr(0)) + PathFilter(web_Script), '', false, true, elxError, elxErrorStr) then
        begin
          web_FatalError(web_Err_ScrNtFnd, web_Script + ' (#' + FStr(ElxError) + ' - ' + ElxErrorStr + ')');
          EXIT;
        end; { if }
    end; { if }
end; { proc. WebRunAny }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure webRunChecks;
var TmpStr : String;
    Counter: Longint;
begin
  {-- open the output ------------------------------------------------------}
  CgiObj^.webOpenOutput(web_DefMimetype);

  {-- first make sure we have access to all the files we need access to ----}
  if NOT web_CheckFileAccess(TmpStr) then
    begin
      CgiObj^.webSendLn('Unable to open all files succesfully (failed on: ' + TmpStr + ')<BR><HR>');
    end { if }
      else CgiObj^.webSendLn('Succesfully opened all files <BR><HR>');

  {-- then display the HTML files path -------------------------------------}
  CgiObj^.webSendLn('web_Htmpath: '+ web_HtmPath + '<BR>');
  CgiObj^.webSendLn('<HR>');

  {-- then display the HTML files path -------------------------------------}
  CgiObj^.webSendLn('web_ELMpath: '+ web_ScrPath + '<BR>');
  CgiObj^.webSendLn('<HR>');

  {-- then display the timelog.bbs filepath --------------------------------}
  CgiObj^.webSendLn('web_TimeLogBBS: '+ TimeLogFileName + '<BR>');
  CgiObj^.webSendLn('<HR>');

  {-- then display all environment variables -------------------------------}
  for Counter := 01 to EnvCount do
    CgiObj^.webSendLn(EnvStr(Counter) + '<BR>');
end; { proc. webRunChecks }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure RunEleWeb;
var ErrorNr    : Longint;
    elxError   : Integer;
    elxErrorStr: AnsiString;
begin
  {-- Now get the default fields we always get -----------------------------}
  if CgiObj^.FoundField('action') < 0 then
    begin
      web_Action := web_RunAnyPth;
    end { if }
      else begin
             RequireLongField(web_Action,
                              'action',
                              'Unable to retrieve "action" field - aborting EleWEB');
           end; { if }


  {-- Now execute something depending on the action ------------------------}
  Case web_Action of
   { login }   web_login    : WebLogin;
   { logout }  web_logout   : WebLogout;
   { run }     web_RunAny,
   { strippath}web_RunAnyPth: WebRunAny(web_Action);
   { checks }  web_RunChecks: WebRunChecks;
  end; { case }
end; { proc. RunEleWeb }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { WEB_GEN }
