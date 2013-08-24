unit LoadOVR;
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
** Open the modem routines for EleBBS
**
** Copyright (c) 1996-1999 by Maarten Bekers
**
** Created : 07-Mar-1999
** Last update : 07-Mar-1999
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure LoadOverlay;
function  GetOvrEnv: String;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF MSDOS}
uses Dos, Global, StrPath, Overlay, CmdLine, OvrXMS, FileObj, FastScrn, CfgRec;
{$ENDIF}

function FileExist(Fname: String): Boolean;
{$IFDEF MSDOS}
var F: pFileObj;
{$ENDIF}
begin
  FileExist := TRUE;

 {$IFDEF MSDOS}
    New(F, Init);
    F^.Assign(Fname);
    F^.FileMode := ReadMode + DenyNone;
    FileExist := F^.Exists;
    Dispose(f, Done);
  {$ENDIF}
end; { func. FileExist }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  GetOvrEnv: String;
{$IFDEF MSDOS}
var TempStr: String;
{$ENDIF}
begin
  GetOvrEnv := '';

{$IFDEF MSDOS}
  TempStr := GetEnv('ELEOVR');
  if TempStr = '' then TempStr := GetEnv('RAOVR');

  GetOvrEnv := TempStr;
{$ENDIF}
end; { func. GetOvrEnv }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  JustPath(S: String): String;                   { Returns just path }
{$IFDEF MSDOS}
var Dir, Name, Ext: String;
{$ENDIF}
begin
  JustPath := '';

{$IFDEF MSDOS}
  {$IFNDEF WIN32}
    FSplit(S, Dir, Name, Ext);
    JustPath := Dir;
  {$ELSE}
    JustPath := ExtractFilepath(S);
  {$ENDIF}
{$ENDIF}
end; { func. JustPath }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure LoadOverlay;
{$IFDEF MSDOS}
var SaveRes     : Integer;
    TempDir     : String;
{$ENDIF}
begin
{$IFDEF MSDOS}
  TempDir := GetOvrEnv;

  if TempDir='' then
    TempDir := JustPath(FSearch('ELEBBS.OVR', '.'+BSChar+';' + JustPath(ParamStr(0)) + ';' + GetEnv('PATH')));

  if not (TempDir[Length(TempDir)] in [BsChar,':']) AND (TempDir<>'') then
      TempDir := TempDir + BsChar;

  if NOT FileExist(TempDir + 'ELEBBS.OVR') then
      begin
        LocalScreenLn(SystemMsgPrefix + 'Unable to find ELEBBS.OVR, program halted.');
        System.Halt(0);
      end; { if }

  OvrInit(TempDir+'ELEBBS.OVR');
  ParseCommandLine(false);

  if OvrUseXMS then
    OvrInitXMS;

  SaveRes := OvrResult;

  if (SaveRes <> ovrOk) OR (NOT OvrUseXMS) then
    if OvrUseEMS then
      OvrInitEMS;

  if (NOT (OvrExtraMemory)) then ExtraOvrSize := ExtraOvrSize div 2;

  OvrSetBuf(OvrGetBuf + ExtraOvrSize);
  OvrSetRetry(OvrGetBuf div 3);
{$ENDIF}
end; { proc. LoadOverlay }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit LOADOVR }
