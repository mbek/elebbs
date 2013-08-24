unit WEB_SUP;
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
** Support routines for EleWEB
** Copyright (c) 1999-2001 by Maarten Bekers
**
**
** Created: 01-May-2001
** Last update : 01-May-2002
**
*)

{$IFNDEF ISCGI}
  You need to define "ISCGI" in order to compile the CGIs
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
  uses web_Abs, web_Glob, CfgRec;
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure web_InitMemory;
procedure web_DeleteOldLogons;
function  web_InitSystemFiles(IsFatal: Boolean): Boolean;
procedure web_GetUserData;
procedure web_DoLogout;
function  web_ServerRunning: Boolean;
function  web_MatchCredentials(web_UIR,
                               web_UIP,
                               web_UIN      : Longint;
                               web_AlwSilent: Boolean;
                               var ErrorNr  : Longint): Boolean;
function  web_ShowHtmlFile(FName: String): Boolean;
function  web_CheckFileAccess(var TmpStr: String): Boolean;
function  web_ErrToString(Error: Longint): String;
procedure web_PerformSilentLogin;

procedure FatalParamError(const MsgStr: String);
function  RequireLongField(var   LongInput: Longint;
                           const FieldName: String;
                           const MsgStr   : String): Boolean;
function  RequireStringField(var   StrInput : String;
                             const FieldName: String;
                             const MsgStr   : String): Boolean;
procedure web_FatalError(const WebErr: Longint;
                         const MsgStr: String);
{$IFNDEF MSDOS}
function  web_GetCookies(const Value, CookieList: AnsiString): String;
function  web_CookieExist(const Value, CookieList: AnsiString): Boolean;
{$ENDIF}

function  ToWWW(S: String): String;
function  TransString(TempStr: String; KeepLinks, KeepAmp: Boolean): String;
function  TransSpaces(TempStr: String): String;
function  TransLink(TempStr: String): String;
function  TranslateHex(S: String): String;
procedure InvalidateHtml(var TmpStr: AnsiString);
function  EscapedToNonEscaped(TempStr: String): String;
procedure ConvertBuffer(var MsgText: web_MsgTextRecord; var MaxLen: Longint);
function  LegalAddress(Addr: String):Boolean;
procedure GetNextLine(var MsgText: web_MsgTextRecord; var oldCounter: Longint; var TmpStr: AnsiString);

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 uses LongStr, Global, FileObj, StrPath, GenFile,
       FileRout, User_U, MultiLn, UnixDate, Terminal,
        web_Scr, JDates, Limit_U, Cases, Strings, SysUtils,
         StUtils, Ranges, WordStr, Dos, ElLog_U, usrExt,
          ObjDec, SysVars, GenDos, Strunit, Ral;
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Type HTMLCharRec = Record
       TempC      : Char;
       HTMLCode   : String[10];
     end; { HTMLcharRec }

Const HtmlCharCount  = 39;
      Charlist : Array[1..HtmlCharCount] Of HtmlCharRec =
         (
         (TempC: '<'; HTMLCode: '&lt;'),      (TempC: '>'; HTMLCode: '&gt;'),
         (TempC: '&'; HTMLCode: '&amp;'),     (TempC: '"'; HTMLCode: '&quot;'),
         (TempC: 'Ä'; HTMLCode: '&Ccedil;'),  (TempC: 'Å'; HTMLCode: '&uuml;'),
         (TempC: 'Ç'; HTMLCode: '&eacute;'),  (TempC: 'É'; HTMLCode: '&acirc;'),
         (TempC: 'Ñ'; HTMLCode: '&auml;'),    (TempC: 'Ö'; HTMLCode: '&agrave;'),
         (TempC: 'Ü'; HTMLCode: '&aring;'),   (TempC: 'á'; HTMLCode: '&ccedil;'),
         (TempC: 'à'; HTMLCode: '&ecirc;'),   (TempC: 'â'; HTMLCode: '&euml;'),
         (TempC: 'ä'; HTMLCode: '&egrave;'),  (TempC: 'ã'; HTMLCode: '&iuml;'),
         (TempC: 'å'; HTMLCode: '&icirc;'),   (TempC: 'ç'; HTMLCode: '&igrave;'),
         (TempC: 'é'; HTMLCode: '&Aumnl;'),   (TempC: 'è'; HTMLCode: '&Aring;'),
         (TempC: 'ê'; HTMLCode: '&Eacute;'),  (TempC: 'ë'; HTMLCode: '&aelig;'),
         (TempC: 'í'; HTMLCode: '&AEligl;'),  (TempC: 'ì'; HTMLCode: '&ocirc;'),
         (TempC: 'î'; HTMLCode: '&ouml;'),    (TempC: 'ï'; HTMLCode: '&ograve;'),
         (TempC: 'ñ'; HTMLCode: '&ucirc;'),   (TempC: 'ó'; HTMLCode: '&ugrave;'),
         (TempC: 'ò'; HTMLCode: '&yuml;'),    (TempC: 'ô'; HTMLCode: '&Ouml;'),
         (TempC: 'ö'; HTMLCode: '&Uuml;'),    (TempC: 'õ'; HTMLCode: '&Cslash;'),
         (TempC: '†'; HTMLCode: '&aacute;'),  (TempC: '°'; HTMLCode: '&iacute;'),
         (TempC: '¢'; HTMLCode: '&oacute;'),  (TempC: '§'; HTMLCode: '&ntilde;'),
         (TempC: '£'; HTMLCode: '&uacute;'),  (TempC: '•'; HTMLCode: '&Ntilde;'),
         (TempC: '·'; HTMLCode: '&szlig;')
         );

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FatalParamError(const MsgStr: String);
begin
  {-- make sure we have opened the output ---------------------------------}
  CgiObj^.webOpenOutput(web_DefMimetype);

  {-- We can replace this later by an script -------------------------------}
  CgiObj^.webSendLn('');
  CgiObj^.webSendLn('');
  CgiObj^.webSendLn('<HTML>');
  CgiObj^.webSendLn('  <BODY>');
  CgiObj^.webSendLn('    <B>');
  CgiObj^.webSendLn('      ' + MsgStr);
  CgiObj^.webSendLn('    </B>');
  CgiObj^.webSendLn('  </BODY>');
  CgiObj^.webSendLn('</HTML>');
end; { proc. FatalParamError }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure web_PerformSilentLogin;
begin
  {-- We assume the user has been validated already. In this routine -------}
  {-- we only check wether a node number has been assigned yet, if not -----}
  {-- we assign it one. After that, we update the user record, update the --}
  {-- useron.bbs table, and enable the logon flag. -------------------------}

  {-- delete old records from the web --------------------------------------}
  {!!web_DeleteOldLogons;}

  {-- make sure user isn't already logged into EleWEB ----------------------}
  LineCfg^.RaNodeNr := MultiLnObj^.GetUserOn(LineCfg^.Exitinfo^.Userinfo.Name, true, true); { lookup EleWEB logins }
  if LineCfg^.RaNodeNr < 0 then
    LineCfg^.RaNodeNr := MultiLnObj^.GetUserOn(LineCfg^.Exitinfo^.Userinfo.Handle, true, true); { Lookup handle }

  {-- if user isnt logged in, create a new nodenumber ----------------------}
  if LineCfg^.RaNodeNr < 0 then
    begin
      LineCfg^.RaNodeNr := MultiLnObj^.EmptyNodeNr(true);
    end; { if }

  {-- get user data --------------------------------------------------------}
  web_GetUserData;

  {-- signal as logged on --------------------------------------------------}
  LineCfg^.LoggedOn := true;

  {-- Now update the useron record -----------------------------------------}
  MultiLnObj^.WriteUserOn('EleWEB', 255);

  {-- set the global variables we need -------------------------------------}
  web_UIN := LineCfg^.RaNodeNr;

  {-- Access granted -------------------------------------------------------}
  web_Access := TRUE;

  {-- remove all old logons from useron.bbs --------------------------------}
  web_DeleteOldLogons;
end; { proc. web_PerformSilentLogin }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function RequireLongField(var   LongInput: Longint;
                          const FieldName: String;
                          const MsgStr   : String): Boolean;
var TmpStr: String;
begin
  {-- Get the actual value -------------------------------------------------}
  TmpStr := CgiObj^.GetFieldByName(FieldName, true);

  {-- now make sure we get a value, else show an error ---------------------}
  if TmpStr = '' then
    begin
      RequireLongField := FALSE;
      FatalParamError(MsgStr);
    end { not found }
      else begin
             LongInput := FVal(TmpStr);
             RequireLongField := TRUE;
           end; { else }
end; { proc. RequireLongField }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function RequireStringField(var   StrInput : String;
                            const FieldName: String;
                            const MsgStr   : String): Boolean;
begin
  {-- Get the actual value -------------------------------------------------}
  StrInput := CgiObj^.GetFieldByName(FieldName, true);

  {-- now make sure we get a value, else show an error ---------------------}
  if StrInput = '' then
    begin
      FatalParamError(MsgStr);
      RequireStringField := FALSE;
    end { not found }
      else RequireStringField := TRUE;
end; { proc. RequireStringField }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure web_FatalError(const WebErr: Longint;
                         const MsgStr: String);
var ElxError: Integer;
    ScriptErrorStr: AnsiString;
begin
  {-- make sure we have opened the output ---------------------------------}
  CgiObj^.webOpenOutput(web_DefMimetype);

  {-- Run the script ------------------------------------------------------}
  if NOT RunElexerScripts(InternalScripts[web_Errors],
                          FStr(WebErr) + ' ' + MsgStr, true, true, ElxError,
                          ScriptErrorStr) then   { parameters }
    begin
      {-- When the execution of the error script fails -------------------------}
      CgiObj^.webSendLn('');
      CgiObj^.webSendLn('');
      CgiObj^.webSendLn('');
      CgiObj^.webSendLn('<HTML>');
      CgiObj^.webSendLn('  <BODY>');
      CgiObj^.webSendLn('    <B>');
      CgiObj^.webSendLn('      Error-handler script is not available!<BR><BR>');
      CgiObj^.webSendLn('      EleWEB error: #' + FStr(WebErr) + ' - ' + web_ErrToString(WebErr) +'<BR>');
      CgiObj^.webSendLn('      Extra error msg: ' + MsgStr + '<BR>');
      CgiObj^.webSendLn('      EleXer script path: "' + web_ScrPath + '"<BR>');
      CgiObj^.webSendLn('      ElxError = ' + FStr(ElxError) + '<BR>');
      CgiObj^.webSendLn('      ElxErrorStr = ' + ScriptErrorStr + '<BR>');

      CgiObj^.webSendLn('    </B>');
      CgiObj^.webSendLn('  </BODY>');
      CgiObj^.webSendLn('</HTML>');
    end; { if }
end; { proc. web_FatalError }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure web_InitMemory;
begin
  {-- initialize the memory so we can initialize the memory soon ------------}
  New(GlobalCfg);               { Initialize memory for BBS global variables }
  New(LineCfg);                     { intialize session specific global vars }
  InitBasicGlobalVars;
  InitBasicLineVars(LineCfg^);

  {-- Get some memory ------------------------------------------------------}
  New(GlobalCfg^.ElConfig);
  New(GlobalCfg^.RaConfig);

  {-- Get some memory ------------------------------------------------------}
  New(LineCfg^.Exitinfo);
  New(LineCfg^.UserExtension);
  New(LineCfg^.SaveGuestRecord);
  New(LineCfg^.Language);
  New(LineCfg^.UploadInfo);
  New(LineCfg^.TagPaths, Init(100));
  New(LineCfg^.Modem);
  New(LineCfg^.Telnet);
  New(LineCfg^.LimitsInfo);
  New(LineCfg^.MenuContents);
  New(LineCfg^.LBarContents);
  New(LineCfg^.HeaderList);
  New(LineCfg^.MarkedMsgArray);
  New(LineCfg^.CurFilesRecord);
  New(LineCfg^.CurMessageRecord);
  New(LineCfg^.CurEleMsgRecord);
  New(LineCfg^.EMSI_user);

  {-- Initialized object -------------------------------------------------}
  New(termObj, Init);
  New(LangObj, Init);
  New(MultiLnObj, Init);
end; { proc. web_InitMemory }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function web_ReadBbsConfig(IsFatal: Boolean): Boolean;
var Config_F : pFileObj;
    TempPath : String;
    TempError: Longint;
begin
  {-- Initialize some variables -------------------------------------------}
  GlobalCfg^.ElConfig^.webElmPath := '';

  web_ReadBbsConfig := FALSE;
  TempPath := ForceBack(CgiObj^.GetEnv('ELEWEB'));
  if TempPath = '' then
    begin
      TempPath := ForceBack(CgiObj^.GetEnv('ELEBBS'));
      
      if TempPath = '' then
         TempPath := ForceBack(GetSysEnv);
    end; { if }
    
  {-- override the language path ------------------------------------------}
  {-- we dont know it yet, so we set it to a default nul-value ------------}
  if IsFatal then
    LineCfg^.Language^.QuesPath := '/nul/'
      else LineCfg^.Language^.QuesPath := '';

  {-- Now start reading the files -----------------------------------------}
  New(Config_F, Init);
  Config_F^.Assign(TempPath + 'config.ra');
  Config_F^.FileMode := ReadMode + DenyWrite;

  {-- Now try if we can open this file ------------------------------------}
  if NOT Config_F^.Open(SizeOf(ConfigRecord)) then
    begin
      if IsFatal then
        begin
          CgiObj^.webOpenOutput(web_DefMimetype);

          TempError := Config_F^.IoResult;
          {$IFNDEF MSDOS}
          web_FatalError(web_err_CfgFile, '"' + TempPath + 'config.ra' + '" (' + FStr(TempError) +
                       ' / OS sais: "' + SysErrorMessage(TempError) + '")');
          {$ENDIF}
          EXIT;
       end
         else CgiObj^.webSendLn('WARNING: Unable to open config.ra');
    end; { if }

  {-- Read the file, and dispose of it ------------------------------------}
  Config_F^.BlkRead(GlobalCfg^.RaConfig^, 1);
  Dispose(Config_F, Done);

  {-- Now try to open config.ele ------------------------------------------}
  New(Config_F, Init);
  Config_F^.Assign(TempPath + 'config.ele');
  Config_F^.FileMode := ReadMode + DenyWrite;

  {-- Now try if we can open this file ------------------------------------}
  if NOT Config_F^.Open(SizeOf(EleconfigRecord)) then
    begin
      if IsFatal then
        begin
          CgiObj^.webOpenOutput(web_DefMimetype);

          TempError := Config_F^.IoResult;
          {$IFNDEF MSDOS}
            web_FatalError(web_err_CfgFile, '"' + TempPath + 'config.ele' + '" (' + FStr(TempError) +
                        ' / OS sais: "' + SysErrorMessage(TempError) + '")');
          {$ENDIF}
          EXIT;
        end
         else CgiObj^.webSendLn('WARNING: Unable to open config.ele');
    end; { if }

  {-- Read the file, and dispose of it ------------------------------------}
  Config_F^.BlkRead(GlobalCfg^.ElConfig^, 1);
  Dispose(Config_F, Done);

  {-- now set the EleWEB paths ccording to ELCONFIG values ----------------}
  web_ScrPath := GlobalCfg^.ElConfig^.webElmPath;
  web_HtmPath := GlobalCfg^.ElConfig^.webHtmlPath;

  {-- fix it up -----------------------------------------------------------}
  if web_ScrPath = '' then
    begin
      web_ScrPath := JustPath(CgiObj^.GetEnv('SCRIPT_FILENAME'));
    end; { if }

  {-- and exit if all went well -------------------------------------------}
  web_ReadBbsConfig := TRUE;

  {-- override the language path ------------------------------------------}
  if web_ScrPath <> '' then
    LineCfg^.Language^.QuesPath := web_ScrPath;
end; { func. web_ReadBbsConfig }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function web_InitSystemFiles(IsFatal: Boolean): Boolean;
begin
  web_InitSystemFiles := web_ReadBbsConfig(IsFatal); { Read the configuration file }
  InitSystemNames;                          { Get all filenames and stuff }
end; { func. web_InitSystemFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function web_MatchCredentials(web_UIR,
                              web_UIP,
                              web_UIN      : Longint;
                              web_AlwSilent: Boolean;
                              var ErrorNr  : Longint): Boolean;
var TempNodeNr: Longint;
begin
  {-- initialize some variables ---------------------------------------------}
  ErrorNr := 0;
  web_MatchCredentials := TRUE;
  LineCfg^.UserExtensionSize := usrext_GetSize;

  {-- First get the user record ---------------------------------------------}
  GetUserRecord(web_UIR,
                LineCfg^.Exitinfo^.Userinfo,
                LineCfg^.UserExtension^,
                true);                        { update ANSI and userage etc }
                                         { Very important else the 0 record }
  LineCfg^.Exitinfo^.UserRecord := web_UIR;          { will be overwritten! }

  {-- check if the user has access ------------------------------------------}
  if (web_UIP <> LineCfg^.Exitinfo^.Userinfo.PasswordCRC) then
    begin
      web_MatchCredentials := FALSE;
      ErrorNr := 1;
      EXIT;
    end; { if }

  {-- get the node information ----------------------------------------------}
  TempNodeNr := MultiLnObj^.GetUserOn(LineCfg^.Exitinfo^.Userinfo.Name, true, true);
  if TempNodeNr < 0 then
    TempNodeNr := MultiLnObj^.GetUserOn(LineCfg^.Exitinfo^.Userinfo.Handle, true, true);

  {-- now make sure its this user -------------------------------------------}
  if TempNodeNr <> web_UIN then
    begin
      if web_AlwSilent then
        begin
          web_PerformSilentLogin;        { try auto-assigning of nodenumber }
          TempNodeNr := LineCfg^.RaNodeNr;
        end { if }
          else begin
                 web_MatchCredentials := FALSE;
                 ErrorNr := 2;
                 EXIT;
               end; { else }
    end; { if }

  {-- and make sure a valid node# is supplied -------------------------------}
  if TempNodeNr < 0 then
    begin
      web_MatchCredentials := FALSE;
      ErrorNr := 3;
      EXIT;
    end; { if }

  {-- now update RaNodeNr ---------------------------------------------------}
  LineCfg^.RaNodeNr := TempNodeNr;

  {-- now make sure this record is not expired ------------------------------}
  if NowAsUnixDate > (MultiLnObj^.UserOn^.LastUpdate + (web_ExpireMins * 60)) then
    begin
      web_MatchCredentials := FALSE;
      ErrorNr := 4;
      EXIT;
    end; { if }
end; { web_MatchCredentials }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function web_ShowHtmlFile(FName: String): Boolean;
var TempFile: pFileObj;
    TempStr : AnsiString;
    o:shortstring;
begin
  {-- Initialize some variables --------------------------------------------}
  web_ShowHtmlFile := FALSE;

  {-- Initialize the file object -------------------------------------------}
  New(TempFile, Init);

  {-- adjust the path when we want -----------------------------------------}
  if JustPath(FName) = '' then
    FName := ForceBack(web_Htmpath) + FName;

  {-- Open the file --------------------------------------------------------}
  TempFile^.Assign(FName);
  if NOT TempFile^.Open(1) then
    begin
      Dispose(TempFile, Done);
      EXIT;
    end; { if }

{!! writeln('hihihih'); }

  {-- Now display the file -------------------------------------------------}
  While (NOT TempFile^.EOF) AND (TempFile^.IoResult = 0) do
    begin
{!! writeln('going to read...', longint(tempfile)); }

      {-- Read a line from the file ----------------------------------------}
      TempFile^.AnsiReadLn(TempStr);

{!! writeln('going to parse'); }

      {-- convert the EleBBS macro codes -----------------------------------}
      termObj^.ParseRaCodeStr(TempStr);

{!! writeln('parsed'); }

      {-- and display the string to output ---------------------------------}
      cgiObj^.WebSend(TempStr);

{!! writeln('and displayed'); }
    end; { while }

{!! writeln('ok dan'); }

  {-- Dispose the file object ----------------------------------------------}
  Dispose(TempFile, Done);
  web_ShowHtmlFile := TRUE;
end; { web_ShowHtmlFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF MSDOS}
function web_GetCookies(const Value, CookieList: AnsiString): String;
var TmpPos: Longint;
    ValueLen: Longint;
    TmpStr: AnsiString;
begin
  web_GetCookies := '';

  {-- lets locate where this value is stored -------------------------------}
  TmpPos := Pos(#32 + Value + '=', #32 + CookieList);
  if TmpPos <= 0 then EXIT;
  Dec(TmpPos); { we fake an leading space  }

  {-- now copie the value --------------------------------------------------}
  ValueLen := Pos(';', Copy(CookieList, TmpPos, Length(CookieList)));
  if ValueLen = 0 then
    ValueLen := Length(CookieList) + 1;

  {-- store the name=value pair --------------------------------------------}
  TmpStr := Copy(CookieList, TmpPos, ValueLen - 1);

  {-- and extract the value out of it --------------------------------------}
  web_GetCookies := Copy(TmpStr, Pos('=', TmpStr) + 1, Length(TmpStr));
end; { func. web_GetCookies }

function web_CookieExist(const Value, CookieList: AnsiString): Boolean;
var TmpPos: Longint;
begin
  web_CookieExist := false;

  {-- lets locate where this value is stored -------------------------------}
  TmpPos := Pos(#32 + Value + '=', #32 + CookieList);
  if TmpPos <= 0 then EXIT;
  Dec(TmpPos); { we fake an leading space  }

  {-- the cookie exists ----------------------------------------------------}
  web_CookieExist := true;
end; { func. web_CookieExist }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure web_DeleteOldLogons;
var Orig_F   : pFileObj;
    Dest_F   : pFileObj;
    UserOnInf: UseronRecord;
    UserNr   : Longint;
    UserInf  : UsersRecord;
    UserExt  : UserExtensionRecord;
    TmpDT    : DateTime;
    TmpL     : Longint;
begin
  {-- initialize the files -------------------------------------------------}
  New(Orig_F, Init);
  New(Dest_F, Init);

  {-- open the file --------------------------------------------------------}
  Orig_F^.Assign(GlobalCfg^.RaConfig^.Syspath + 'useron.bbs');
  Orig_F^.FileMode := ReadMode + DenyNone;
  if Orig_F^.Open(1) then
    begin
      Dest_F^.Assign(GlobalCfg^.RaConfig^.Syspath + 'USER' + FStr(LineCfg^.RaNodeNr) + '.BB$');
      Dest_F^.FileMode := ReadWriteMode + DenyAll;

      {-- create the new temporarily file ----------------------------------}
      if Dest_F^.Create(1) then
        begin
          {-- Loop through all records -------------------------------------}
          While NOT Orig_F^.EOF do
            begin
               {-- get the record ------------------------------------------}
               Orig_F^.BlkRead(UserOnInf, SizeOf(UserOnRecord));

               {-- if deleted ----------------------------------------------}
               if UserOnInf.NodeNumber < 0 then
                 begin
                   FillChar(UserOnInf, SizeOf(UserOnInf), #00);
                 end; { if }

               {-- if EleWEB item ------------------------------------------}
               if UserOnInf.NodeNumber >= WebNodeBase then
                 if UserOnInf.LastUpdate > 0 then
                   begin
                     {-- if this user has expired --------------------------}
                     if (UserOnInf.LastUpdate + (web_ExpireMins * 60)) <= NowAsUnixDate then
                       begin
                         {-- Get the user record ---------------------------}
                         UserNr := SearchUser(UseronInf.Name);

                         if UserNr >= 0 then
                           begin
                             {-- get the user record -----------------------}
                             GetUserRecord(UserNr, UserInf, UserExt, false);

                             {-- Update last logon date and time etc -------}
                             UnixToDt(UseronInf.LastUpdate, TmpDt);
                             PackTime(TmpDt, TmpL);

                             Userinf.LastTime := Time2Str(TmpL);
                             Userinf.LastDate := Date2Str(TmpL);
                             Inc(UserInf.NoCalls);

                             {-- Update statistics -------------------------}
                             UpdateStatistics;

                             {-- and write it back to disk -----------------}
                             WriteUserRecord(UserNr, Userinf, userExt)
                           end; { if }

                         {-- and kill of the record ------------------------}
                         FillChar(UserOnInf, SizeOf(UserOnInf), #00);
                       end; { if }
                   end; { if }

               {-- write back the record -----------------------------------}
               Dest_F^.BlkWrite(UserOnInf, SizeOf(UserOnRecord));
            end; { while }

        end; { if }
    end; { if }

  Dispose(Dest_F, Done);

  Orig_F^.Erase;
  Dispose(Orig_F, Done);

  RenameFile(GlobalCfg^.RaConfig^.SysPath + 'USER' + FStr(LineCfg^.RaNodeNr) + '.BB$',
             GlobalCfg^.RaConfig^.Syspath + 'useron.bbs');
end; { proc. web_DeleteOldLogons }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure web_DoLogout;
begin
  {-- Increase the call count ------------------------------------------}
  LineCfg^.Exitinfo^.Userinfo.LastTime := TimeStr(false, false);
  LineCfg^.Exitinfo^.Userinfo.LastDate := DateStr;
  Inc(LineCfg^.Exitinfo^.UserInfo.NoCalls);

  {-- Update statistics ------------------------------------------------}
  UpdateStatistics;

  {-- Update the user record -------------------------------------------}
  UpdateUserRecord;

  {-- delete ourselves from useron.bbs ---------------------------------}
  MultiLnObj^.KillFromUserOn;
end; { proc. web_DoLogout }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure web_GetUserData;
var TmpStr: String;
begin
  {-- now update the last call information etc. ----------------------------}
  GetExitinfoInfo(LineCfg^.Exitinfo^);    { Update number of calls etcetera }
  GetUserAge(LineCfg^.Exitinfo^.Userinfo.BirthDate, true); { Make sure we dont get cached settings that are invalid }
  LineCfg^.LoggedOn := true;

  {-- get user limits and stuff --------------------------------------------}
  GetLevelLimitsInfo(LineCfg^.Exitinfo^.Userinfo.Security,
                     false,                    { Warning message on console }
                     false);                          { Ignore time warning }

  {-- get the defualt language info ----------------------------------------}
  ReadLanguageRa(LineCfg^.Exitinfo^.Userinfo.Language);

  {-- update last logon time and date etc ----------------------------------}
  LineCfg^.Exitinfo^.LoginTime := TimeStr(false, false);

  {-- update some data paths to override other settings --------------------}
  if web_ScrPath <> '' then
    LineCfg^.Language^.QuesPath := web_ScrPath;
end; { proc. web_GetUserData }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function web_ErrToString(Error: Longint): String;
begin
  web_ErrToString := '(unknown error string)';

  {$IFNDEF MSDOS}
  case Error of
    web_err_UnkUser : Result := 'Unknown user';
    web_err_InvPwd  : Result := 'Invalid password';
    web_err_CfgFile : Result := 'Unable to read configuration files';
    web_err_AccDnd  : Result := 'Access denied';
    web_err_ScrNtFnd: Result := 'Unable to find specified script';
  end; { case }
  {$ENDIF}
end; { func. web_ErrToString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GrabURL(S: String; var Start: Longint):String;
var TempStr: String;
    TempPos: Longint;
begin
  TempPos := Start;
  Dec(TempPos);

  While (TempPos > 00) AND (S[TempPos] <> ':') AND
         (UpCase(S[TempPos]) in ['A'..'Z','_','.','-']) do
          Dec(TempPos);

  Inc(TempPos);
  Start := TempPos;

  TempStr := '';
  while (TempPos <= Length(S)) AND (Pos(S[TempPos],' ^&*(){}<>[]|;"''') =00) do
    begin
      TempStr := TempStr + S[TempPos];
      Inc(TempPos);
    end; { while }

  GrabURL := TempStr;
end; { func. GrabURL }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function LegalAddress(Addr: String):Boolean;
var Name   : String;
    Server : String;
begin
  LegalAddress := false;

  Name   := Copy(Addr, 1, Pos('@', Addr));
  Server := Addr;

  Delete(Server, 1, Length(Name) + 01);

  if (Server = '') OR (Name = '') OR (Name = '@') then EXIT;
  if Pos('@', Server) > 00 then EXIT;
  if Pos(#32, Name) > 00 then EXIT;
  if Pos(#32, Server) > 00 then EXIT;
  if Pos('.', Server) = 0 then EXIT;

  LegalAddress := true;
end; { func. LegalAddress }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ToWWW(S: String): String;
var Start : Longint;
    PosCnt: Longint;
    URL   : String;
begin
  Url := SUpCase(s);

  PosCnt := Pos('@', S);

  if PosCnt > 00 then
    begin
      URL := GrabURL(S, PosCnt);
      Delete(S, PosCnt, length(Url));

      if LegalAddress(url) then
        begin
          Url := '<A HREF="mailto:' + Url + '">' + Url + '</A>';
        end; { if }

      Insert(Url, S, PosCnt);
    end; { if }

  PosCnt := Pos('://', Url);
  if PosCnt > 00 then
    begin
      Url := GrabURL(S, PosCnt);
      Delete(S, PosCnt, Length(Url));

      Url :='<A HREF="' + Url + '">' + Url + '</A>';
      Insert(Url, S, PosCnt);
    end; { if }

  ToWWW := S;
end; { func. ToWww }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TransChar(TempC: Char): String;
var Temp: Longint;
begin
  Temp := Pos(TempC, '<>&"ÄÅÇÉÑÖÜáàâäãåçéèêëíìîïñóòôöõ†°¢§£•·');

  if Temp > 00 then
    TransChar := CharList[Temp].HtmlCode
     else TransChar := TempC;
end; { func. Transchar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function EscapedToNonEscaped(TempStr: String): String;
var Counter: Longint;
    Count2 : Longint;
begin
  for Counter := 1 to HtmlCharCount do
   if Pos('&', TempStr) > 0 then
    begin
      for Count2 := 1 to 50 do
        begin
          if Pos('&', TempStr) = 0 then BREAK;

          Replace(CharList[Counter].HtmlCode,
                  CharList[Counter].TempC,
                  TempStr);
        end; { for }
    end; { for }

  EscapedToNonEscaped := TempStr;
end; { func. EscapedToNonEsacped }

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
          Val('$' + S[Temp+1] + S[Temp+2], Nr, Error);
          S[Temp] := Chr(Nr);

          Delete(S, Temp+1, 2);
        end; { if }

      Inc(Temp);
    end; { while }

  TranslateHex := S;
end; { func. TranslateHex }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  TransSpaces(TempStr: String): String;
var Counter    : Longint;
    DoTranslate: Boolean;
    Temp       : String;
begin
  DoTransLate := true;
  Temp := '';
  Counter := 1;

  TempStr := TranslateHex(TempStr);     { Convert all %xx to the actual chars }

  While Counter <= Length(TempStr) do
    begin
      if TempStr[Counter] = '<' then
        begin
          DoTranslate := (Pos('>', Copy(TempStr, Counter + 01, Length(TempStr))) = 0);
        end; { if }

      if DoTranslate then
        begin
          if TempStr[Counter] = #32 then { Space }
            Temp := Temp + '&nbsp;'
              else Temp := Temp + TempStr[Counter];
        end
          else Temp := Temp + TempStr[Counter];

      if TempStr[Counter] = '>' then DoTranslate := True;

      Inc(Counter);
    end; { while }

  TransSpaces := Temp;
end; { func. TransSpaces }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  TransString(TempStr: String; KeepLinks, KeepAmp: Boolean): String;
var Counter    : Longint;
    DoTranslate: Boolean;
    Temp       : String;
begin
  DoTransLate := true;
  Temp := '';
  Counter := 1;

  TempStr := TranslateHex(TempStr);     { Convert all %xx to the actual chars }

  While Counter <= Length(TempStr) do
    begin
      if TempStr[Counter] = '<' then
        begin
          DoTranslate := (Pos('>', Copy(TempStr, Counter + 01, Length(TempStr))) = 0);

          if NOT KeepLinks then
            DoTranslate := TRUE;
        end; { if }

      if DoTranslate then
        begin
          if (TempStr[Counter] <> '&') OR (NOT KeepAmp) then
            Temp := Temp + TransChar(TempStr[Counter])
              else Temp := Temp + TempStr[Counter];
        end
          else Temp := Temp + TempStr[Counter];

      if TempStr[Counter] = '>' then DoTranslate := True;

      Inc(Counter);
    end; { while }

  TransString := Temp;
end; { func. TransString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ConvertBuffer(var MsgText: web_MsgTextRecord; var MaxLen: Longint);
var Counter   : Longint;
    TmpCounter: Longint;
    SaveCnt   : Longint;

    newTxtBuf : ^web_MsgTextRecord;
    newSize   : Longint;
begin
  {-- initialize variables -------------------------------------------------}
  New(NewTxtBuf);
  FillChar(newTxtBuf^, sizeOf(web_MsgTextRecord), #0);
  Counter := 0;
  NewSize := 0;

  While (Counter < MaxLen) do
    begin
      if MsgText[Counter] = '%' then
        begin
          if (Counter + 2) < MaxLen then
            begin
              newTxtBuf^[newSize] := FirstChar(TranslateHex('%' + MsgText[Counter + 1] + MsgText[Counter + 2]));
              Inc(Counter, 2);
            end; { if }
        end { if }
          else newTxtBuf^[NewSize] := MsgText[Counter];

      Inc(Counter);
      Inc(NewSize);
    end; { while }

  MaxLen := NewSize;
  MsgText := newTxtBuf^;
  Dispose(NewTxtBuf);
end; { proc. ConvertBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GetNextLine(var MsgText: web_MsgTextRecord; var oldCounter: Longint; var TmpStr: AnsiString);
var MaxLen : Longint;
    Counter: Longint;
begin
  TmpStr := '';
  MaxLen := StrLen(MsgText);
  Counter := oldCounter;

  while (MsgText[Counter] <> #13) AND (Counter < MaxLen) do
    begin
      TmpStr := TmpStr + MsgText[Counter];
      Inc(Counter);
    end; { while }

  if Counter < MaxLen then
   if MsgText[Counter] = #13 then
     Inc(Counter);

  if Counter < MaxLen then
   if MsgText[Counter] = #10 then
     Inc(Counter);

  oldCounter := Counter;
end; { proc. GetNextline }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TransLink(TempStr: String): String;
var Counter: Longint;
    NewStr : String;
begin
  Counter := 01;
  NewStr := '';

  While Counter <= Length(TempStr) do
    begin
       if TempStr[Counter] in ['A'..'Z', 'a'..'z', '0'..'9', '?', '&', '.',
                               '\', '/', '_'] then
        NewStr := NewStr + TempStr[Counter]
         else NewStr := NewStr + '%' + ToHex(Ord(TempStr[Counter]));

      Inc(Counter);
    end; { while }

  TransLink := NewStr;
end; { func. TransLink }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function web_ServerRunning: Boolean;
begin
  {-- Default to not running -----------------------------------------------}
  web_ServerRunning := FALSE;

  {-- now check if it might be running -------------------------------------}
  if GetEnv('HTTP_HOST') <> '' then web_ServerRunning := TRUE
   else if GetEnv('CONTENT_LENGTH') <> '' then web_ServerRunning := TRUE
    else if GetEnv('HTTP_CONNECTION') <> '' then web_ServerRunning := TRUE
     else if GetEnv('SERVER_NAME') <> '' then web_ServerRunning := TRUE
      else if GetEnv('SERVER_PORT') <> '' then web_ServerRunning := TRUE
       else if GetEnv('SERVER_SOFTWARE') <> '' then web_ServerRunning := TRUE;
end; { func. web_ServerRunning }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure InvalidateHtml(var TmpStr: AnsiString);
var Counter: Longint;
    NewStr : AnsiString;
begin
  NewStr := '';

  for Counter := 01 to Length(TmpStr) do
    if TmpStr[Counter] in ['<'{, '>'}] then
      NewStr := NewStr + TransChar(TmpStr[Counter])
       else NewStr := NewStr + TmpStr[Counter];

  TmpStr := NewStr;
end; { proc. InvalidateHtml }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function web_CheckFileAccess(var TmpStr: String): Boolean;

function fileHasAccess(FName: String; AccessMode: Longint): Boolean;
var FileF: tFileObj;
begin
  FileF.Init;
  FileF.Assign(FName);
  FileF.FileMode := AccessMode;
  if FileF.Open(1) then
    begin
      fileHasAccess := TRUE;
    end
      else begin
             TmpStr := FName;
             fileHasAccess := FALSE;
           end; { if }
  FileF.Done;
end; { func. fileHasAccess }

begin
  {-- initialize defaults --------------------------------------------------}
  TmpStr := '';
  web_CheckfileAccess := false;

  {-- now loop through all the files we need to make sure we have access ---}
  if NOT fileHasAccess(GlobalCfg^.RaConfig^.MsgBasePath + 'users.bbs', ReadMode + DenyNone) then EXIT;
  if NOT fileHasAccess(GlobalCfg^.RaConfig^.MsgBasePath + 'usersidx.bbs', ReadMode + DenyNone) then EXIT;
  if NOT fileHasAccess(GlobalCfg^.RaConfig^.MsgBasePath + 'usersele.bbs', ReadMode + DenyNone) then EXIT;
  if NOT fileHasAccess(GlobalCfg^.RaConfig^.MsgBasePath + 'usersxi.bbs', ReadMode + DenyNone) then EXIT;
  if NOT fileHasAccess(GlobalCfg^.RaConfig^.MsgBasePath + 'lastread.bbs', ReadMode + DenyNone) then EXIT;
  if NOT fileHasAccess(LanguageFileName, ReadMode + DenyNone) then EXIT;
  if NOT fileHasAccess(LimitsFileName, ReadMode + DenyNone) then EXIT;
  if NOT fileHasAccess(EventsFilename, ReadMode + DenyNone) then EXIT;
  if NOT fileHasAccess(FGroupsFileName, ReadMode + DenyNone) then EXIT;
  if NOT fileHasAccess(MGroupsFileName, ReadMode + DenyNone) then EXIT;
  if NOT fileHasAccess(FilesFileName, ReadMode + DenyNone) then EXIT;
  if NOT fileHasAccess(MessagesFileName, ReadMode + DenyNone) then EXIT;
  if NOT fileHasAccess(AddressFileName, ReadMode + DenyNone) then EXIT;
  if NOT fileHasAccess(TelnetFileName, ReadMode + DenyNone) then EXIT;
  if NOT fileHasAccess(LastcallFileName, ReadMode + DenyNone) then EXIT;
  if NOT fileHasAccess(SysinfoFileName, ReadMode + DenyNone) then EXIT;
  if NOT fileHasAccess(TimeLogFileName, ReadMode + DenyNone) then EXIT;
  if NOT fileHasAccess(MessageEleFileName, ReadMode + DenyNone) then EXIT;
  if NOT fileHasAccess(EleFilesFileName, ReadMode + DenyNone) then EXIT;

  {-- and eventually, well succeed -----------------------------------------}
  web_CheckfileAccess := true;
end; { func. web_CheckFileAccess }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { WEBSUP }
