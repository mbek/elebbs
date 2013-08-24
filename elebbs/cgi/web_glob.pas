unit WEB_GLOB;
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
** WEBGLOB, Global variables for all EleWEB scripts
** Copyright (c) 1999-2001 by Maarten Bekers
**
**
** Created: 16-Apr-2001
** Last update : 16-Apr-2001
**
*)

{$IFNDEF ISCGI}
  You need to define "ISCGI" in order to compile the CGIs
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
  uses web_Abs;
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{-- Messagebase records ----------------------------------------------------}
type
  {$IFNDEF MSDOS}
    web_MsgTextRecord = Array[0..(1024 * 72)] of Char;
  {$ELSE}
    web_MsgTextRecord = Array[0..(1024 * 1)] of Char;
  {$ENDIF}

const
  MaxInternScripts = 5;

  InternalScripts : Array[1..MaxInternScripts] of String =
                      (
                       'login',                                    { login }
                       'logout',                                  { logout }
                       '',                                    { any script }
                       'errors',                 { error displaying script }
                       'getcreds'   { get user credentials for logging out }
                      );

  {-- time a useron.bbs record for EleWEB is valid ------------------------}
  web_ExpireMins  = 15;                       { Mins a login expires after }

  {-- action codes that are matched to script actions ---------------------}
  web_Login       = 01;                                            { login }
  web_LogOut      = 02;                                           { logout }
  web_RunAny      = 03;                                   { run any script }
  web_RunAnyPth   = 08;                { run any script, but strp the path }
  web_Errors      = 04;                            { Display an error code }
  web_GetCreds    = 05;             { Get credentials for logout procedure }
  web_NewOne      = 09;                           { First part of new user }
  web_NewTwo      = 10;   { The actual validation and adding of a new user }
  web_RunChecks   = 11;                   { run several file/access checks }

  {-- Error codes that can be returned by EleWEB --------------------------}
  web_err_UnkUser = 01;                                     { Unknown user }
  web_err_InvPwd  = 02;                                 { Invalid password }
  web_err_CfgFile = 03;               { Unable to read configuration files }
  web_err_AccDnd  = 04;                                    { Access denied }
  web_err_ScrNtFnd= 05;                  { Unable to find specified script }

  {-- default we use when opening output ----------------------------------}
  web_DefMimeType = 'text/html';

var CgiObj       : pweb_SrvObject;  { General CGI object we use everywhere }
    web_Access   : Boolean;                { Wether or not user has access }
    web_HtmPath  : String;                                    { HTML files }
    web_ScrPath  : String;                   { EleXer binaries script path }
    web_Initted  : Boolean;                     { Did we succesfully init? }
    web_RunLocal : Boolean;     { Is EleWEB ran local (not from webserver) }
    web_UIP      : Longint;                             { Password (CRC32) }
    web_UIR      : Longint;                       { users.bbs Recordnumber }
    web_UIN      : Longint;                                   { Nodenumber }
    web_AlwSilent: Boolean;                  { Allow silent login of user? }
    web_Action   : Longint;                           { Action to perform: }
                                                               { 1 = login }
                                                              { 2 = logout }
                                                             { 3 = run any }


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


end. { WEBGLOB }
