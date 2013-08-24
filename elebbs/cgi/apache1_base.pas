unit Apache1_Base;
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
** Apache1_Base.
** Apache 1.3x.x module for EleWEB.
**
** Copyright (c) 1999-2003 by Maarten Bekers
**
**
** Created: 10-Feb-2003
** Last update : 10-Feb-2003
**
*)

{$IFNDEF ISCGI}
  You need to define "ISCGI" in order to compile the CGIs
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 uses SysUtils,
       Classes,
        Ap1_Base,
         web_Sup,
          elx_bbs,
           elx_glob,
            Unixdate;


{-- functions exported -------------------------------------------------------}
procedure apache_init(server: Pserver_rec; pool: Ppool); cdecl;

{-- and functions we use internally ------------------------------------------}
function apache_handlers(r: Prequest_rec): integer; cdecl;
procedure ApacheInitialize; cdecl;


type
  HandlersRec = Array[0..1] of Handler_Rec;
  
var
  Request_Handlers : HandlersRec;
  Apache_Module    : Module;                     { This is exported to Apache }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses
  web_scr,
  web_apache1,
  web_glob
{$IFDEF MSWINDOWS}
  ,Windows
{$ENDIF} ;

// Apache dispatch interface
function apache_handlers(r: Prequest_rec): integer; cdecl;
var web_Script : ShortString;
    elxError   : Integer;
    elxErrorStr: AnsiString;
    AllOk      : Boolean;
begin
  {-- were the positive kind -------------------------------------------------}
  AllOk := true;

  {-- we set the options that we like ----------------------------------------}
  r^.allowed := M_GET OR M_POST;

  {-- now see if this is any thing we like -----------------------------------}
  if (r^.method_number = M_GET) OR (r^.method_number = M_POST) then
    begin
      {-- default to a proper working of it all ------------------------------}
      Result := AP_OK;

      {-- if we only want a header, just invalidate the AllOk boolean --------}
      if (boolean(R^.header_only)) then
        begin
          AllOk := false;
        end; { if }

     {-- initialize the new apache object ------------------------------------}
     CgiObj := New(pWeb_ApacheObject, Init('', r));

     {-- Now get the default fields we always get ----------------------------}
     if NOT RequireLongField(web_Action,
                            'action',
                            'Unable to retrieve "action" field - aborting EleWEB') then
       AllOk := false;

     {-- Now get the default fields we always get ----------------------------}
     if AllOk then
        RequireStringField(web_Script,
                           'script',
                           'Unable to retrieve "script" field - aborting EleWEB');

     {-- run any script we want to -------------------------------------------}
     if AllOk then
       begin
          elxCache_RunModule(web_Script,
                             '',
                             ElxError,
                             ElxErrorStr);
          if ElxError > 0 then
            begin
              web_FatalError(web_Err_ScrNtFnd, web_Script + ' (' + ElxErrorStr + ')');
              AllOk := false;
            end; { if }
       end; { if }

     {-- dispose of our CGI program again ------------------------------------}
     Dispose(CgiObj, Done);
   end { if }
     else Result := AP_DECLINED;
end; { proc. apache_handlers }

procedure apache_init(server: Pserver_rec; pool: Ppool); cdecl;
begin
//  with (Application as TApacheApplication) do
//  if Assigned(ApacheOnInit) then
//    ApacheOnInit(server, pool);
end;

procedure apache_child_init(Server: Pserver_rec; Pool: Ppool); cdecl;
begin
//  with (Application as TApacheApplication) do
//  if Assigned(ApacheOnChildInit) then
//    ApacheOnChildInit(server, pool);
end;

procedure apache_child_exit(server: Pserver_rec; pool: Ppool); cdecl;
begin
//  with (Application as TApacheApplication) do
//  if Assigned(ApacheOnChildExit) then
//    ApacheOnChildExit(server, pool);
end;

function apache_create_dir_config(pool: Ppool; szDir: pchar): pointer; cdecl;
begin
  result := nil;
//  with (Application as TApacheApplication) do
//  if Assigned(ApacheOnCreateDirConfig) then
//    result := ApacheOnCreateDirConfig(pool, szDir);
end;

function apache_merge_dir_config(pool: Ppool; base_conf, new_conf: pointer): pointer; cdecl;
begin
  result := base_conf;
//  with (Application as TApacheApplication) do
//  if Assigned(ApacheOnMergeDirConfig) then
//    result := ApacheOnMergeDirConfig(pool, base_conf, new_conf);
end;

function apache_create_server_config(pool: Ppool; server: Pserver_rec): pointer; cdecl;
begin
  result := server;
//  with (Application as TApacheApplication) do
//  if Assigned(ApacheOnCreateServerConfig) then
//    result := ApacheOnCreateServerConfig(pool, server);
end;

function apache_merge_server_config(pool: Ppool; base_conf, new_conf: pointer): pointer; cdecl;
begin
  result := base_conf;
//  with (Application as TApacheApplication) do
//  if Assigned(ApacheOnMergeServerConfig) then
//    result := ApacheOnMergeServerConfig(pool, base_conf, new_conf);
end;

function apache_logger(r: Prequest_rec): integer;  cdecl;
begin
  result := 0;
//  with (Application as TApacheApplication) do
//  if Assigned(ApacheOnLogger) then
//    result := ApacheOnLogger(r);
end;

function apache_fixer_upper(r: Prequest_rec): integer;  cdecl;
begin
  result := 0;
//  with (Application as TApacheApplication) do
//  if Assigned(ApacheOnFixUps) then
//    result := ApacheOnFixUps(r);
end;

function apache_type_checker(r: Prequest_rec): integer;  cdecl;
begin
  result := 0;
//  with (Application as TApacheApplication) do
//  if Assigned(ApacheOnTypeChecker) then
//    result := ApacheOnTypeChecker(r);
end;

function apache_auth_checker(r: Prequest_rec): integer;  cdecl;
begin
  result := 0;
//  with (Application as TApacheApplication) do
//  if Assigned(ApacheOnAuthChecker) then
//    result := ApacheOnAuthChecker(r);
end;

function apache_check_user_id(r: Prequest_rec): integer;  cdecl;
begin
  result := 0;
//  with (Application as TApacheApplication) do
//  if Assigned(ApacheOnCheckUserId) then
//    result := ApacheOnCheckUserId(r);
end;

function apache_header_parser(r: Prequest_rec): integer;  cdecl;
begin
  result := 0;
//  with (Application as TApacheApplication) do
//  if Assigned(ApacheOnHeaderParser) then
//    result := ApacheOnHeaderParser(r);
end;

function apache_access_checker(r: Prequest_rec): integer;  cdecl;
begin
  result := 0;
//  with (Application as TApacheApplication) do
//  if Assigned(ApacheOnAccessChecker) then
//    result := ApacheOnAccessChecker(r);
end;

function apache_post_read_request(r: Prequest_rec): integer;  cdecl;
begin
  result := 0;
//  with (Application as TApacheApplication) do
//  if Assigned(ApacheOnPostReadRequest) then
//    result := ApacheOnPostReadRequest(r);
end;

function apache_translate_handler(r: Prequest_rec): integer;  cdecl;
begin
  result := 0;
//  with (Application as TApacheApplication) do
//  if Assigned(ApacheOnTranslateHandler) then
//    result := ApacheOnTranslateHandler(r);
end;


procedure ApacheInitialize; cdecl;
begin
  with Apache_Module do
  begin
    // STANDARD_MODULE_STUFF
    // must appear in all module records.
    version := MODULE_MAGIC_NUMBER_MAJOR;
    minor_version := MODULE_MAGIC_NUMBER_MINOR;
    module_index := -1;
    name := 'mod_ick';
    dynamic_load_handle := nil;
    next := nil;
    magic := MODULE_MAGIC_COOKIE;
    cmds := nil;
    
    request_handlers[0].content_type := 'mod_ick-handler';
    request_handlers[0].handler := @apache_handlers;
    request_handlers[1].content_type := nil;
    request_handlers[1].handler := nil;
    handlers := @request_handlers;

    init := @apache_init;
    create_dir_config := nil;
    merge_dir_config := nil;
    create_server_config := nil;
    merge_server_config := nil;
    translate_handler := nil;
    ap_check_user_id := nil;
    auth_checker := nil;
    access_checker := nil;
    type_checker := nil;
    fixer_upper := nil;
    logger := nil;
    header_parser := nil;
    child_init := nil;
    child_exit := nil;
    post_read_request := nil;
  end;
end;


initialization
  {-- run the initialisation of the program --------------------------------}
  ApacheInitialize;
end.
