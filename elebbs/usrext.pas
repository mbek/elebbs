unit USREXT;
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
** Userbase extension routines for EleBBS
**
** Copyright (c) 1996-2001 by Maarten Bekers
**
** Created : 30-Jun-2001
** Last update : 30-Jun-2001
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Global;

function  usrext_GetSize: Longint;
function  usrext_SetDefaults(var UserExt: UserExtensionRecord): Boolean;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 uses elx_Bbs,
       longstr,
        ellog_u,
         strpath,
          genfile;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

const
  usrExt_GetSizeSave : Longint = -1;

function usrext_GetSize: Longint;
var elxObj    : pElxBbsObj;
    TempPath  : String;
begin
  if usrExt_GetSizeSave = -1 then
    begin
      New(elxObj, Init);

      if NOT RunningWEB then TempPath := GlobalCfg^.RaConfig^.Syspath
        else TempPath := LineCfg^.Language^.QuesPath;

      if elxObj^.RunElexerScript(TempPath + 'usrsup', '0 0', false) then
        begin
          usrExt_GetSizeSave := FVal(elxObj^.GetElxReturnString);
        end { if }
          else RaLog('!', 'Unable to find/run "usrsup.elm"');

      Dispose(elxObj, Done);
    end; { if }

  usrExt_GetSize := usrExt_GetSizeSave;
end; { func. usrext_GetSize }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  usrext_SetDefaults(var UserExt: UserExtensionRecord): Boolean;
var SaveExt : UserExtensionRecord;
    elxObj  : pElxBbsObj;
    TempPath: String;
begin
  SaveExt := LineCfg^.UserExtension^;
  usrExt_SetDefaults := TRUE;

  New(elxObj, Init);

  if NOT RunningWEB then TempPath := GlobalCfg^.RaConfig^.Syspath
    else TempPath := LineCfg^.Language^.QuesPath;

  if elxObj^.RunElexerScript(TempPath + 'usrsup', '0 2', false) then
    begin
      UserExt := LineCfg^.UserExtension^;
    end { if }
      else RaLog('!', 'Unable to find/run "usrsup.elm"');

  Dispose(elxObj, Done);
  LineCfg^.UserExtension^ := SaveExt;
end; { proc. usrext_SetDefaults }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
