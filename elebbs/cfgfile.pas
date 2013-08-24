unit CfgFile;
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
** CFGFILE.TPU, File routines for EleBBS
**
**
** Copyright (c) 1998 by Maarten Bekers
**
** Created : 01-May-1998
** Last update : 01-May-1998
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Global, CfgRec, FileObj;

procedure Config_DoneFile(var F: pFileObj);
procedure Config_ShowError(Msg, FName: String; Error: Word);
function  Config_OpenFile(var F: pFileObj; Name: String; RecSize, ReqMode: Word; Create, Fatal: Boolean): Word;
function  Config_ReadFile(var F: pFileObj; var buf; NrRead: Word): Word;
function  Config_SeekFile(var F: pFileObj; FPos: Longint): Word;
function  Config_WriteFile(var F: pFileObj; var buf; NrWrite: Word): Word;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses ElLog_U, Debug_U, IORes_Un, CentrStr, GenFile, SysUtils,
     LongStr,
     {$IFDEF WITH_FULL}
       FastScrn,
     {$ENDIF}
     GenDos, Dos, Strings,
     ObjDec;

procedure Config_ShowError(Msg, FName: String; Error: Word);
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileIo, 'ShowError (fatal): '+Msg+' / '+FName);
  {$ENDIF}

  RaLog('!', Msg + ' "'+ FName + '"');
  RaLog('!', 'Last error: '+MapIoErrorToStr(Error));
  RaLog('!', 'Terminated '+FullProgName);

  WriteLn;
  ClearOnExit := false;

  {$IFDEF WITH_FULL}
    if RunningBBS then Fillarea(01, 01, 80, 03, #32, 07);
    LocalScreenLn(SystemMsgPrefix + 'Error #'+FStr(Error)+ ' occured while accessing '+FName);
    LocalScreenLn(SystemMsgPrefix + Msg);
  {$ELSE}
    WriteLn(SystemMsgPrefix + 'Error #'+FStr(Error)+ ' occured while accessing '+FName);
    WriteLn(SystemMsgPrefix + Msg);
  {$ENDIF}

  {$IFDEF ELEUNIX}
    WriteLn(StdErr, #13#10#13#10);
    WriteLn(StdErr, SystemMsgPrefix + 'Error #'+FStr(Error)+ ' occured while accessing '+FName+#13#10);
    WriteLn(StdErr, SystemMsgPrefix + Msg+#13#10);
    WriteLn(StdErr, #13#10#13#10);
  {$ENDIF}

  {$IFNDEF WINGUI}
    System.Halt(defExitCode);
  {$ELSE}
    Progterminated := true;
    Application.Terminate;
  {$ENDIF}
end; { proc. Config_ShowError }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Config_OpenFile(var f:pFileObj; Name: String; RecSize, ReqMode: Word; Create, Fatal: Boolean): Word;
var Tries    : Word;
    TempError: Word;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileIo, 'Opening file: '+Name);
  {$ENDIF}

  Tries := FileRetries + 1;
  Config_OpenFile := 00;

  if Trim(Name)='' then
    begin
      Config_OpenFile := 02;
      EXIT;
    end; { if }

  New(F, Init);
  F^.Assign(Name);

  repeat
    F^.FileMode := ReqMode;

    {$i-}
      F^.Open(RecSize);
    {$i+}
    TempError := F^.IOResult;

    if (Create) AND ( (TempError = 02) OR (NOT FileExist(Name)) )then
      begin
        {$i-} F^.Create(RecSize); {$i+}
        TempError := F^.IOResult;
      end; { if }

    Dec(Tries);
  until (TempError = 00) OR (Tries=00);

  if Fatal then
   if TempError <> 00 then
    Config_ShowError('Unable to open file.', Name, TempError);

  Config_OpenFile := TempError;
end; { func. Open_ConfigFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Config_ReadFile(var F: pFileObj; var buf; NrRead: Word): Word;
var Tries     : Word;
    TempError : Word;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(LogFileIo, 'Reading file: '+F^.FileName);
  {$ENDIF}

  Tries := FileRetries + 1;


  repeat;
    {$i-}
      if F^.BlkRead(Buf, NrRead) = 0 then
       if NrRead > 0 then
         F^.SetError(100);
    {$i+}
    TempError := F^.IOResult;

    Dec(Tries);
  until (TempError = 00) OR (Tries <= 00);

  Config_ReadFile := TempError;
end; { proc. Config_ReadFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Config_WriteFile(var F: pFileObj; var buf; NrWrite: Word): Word;
var Tries     : Word;
    TempError : Word;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileIo, 'Writing file: '+F^.FileName);
  {$ENDIF}
  Tries := FileRetries + 1;

  repeat;
    {$i-}
      F^.BlkWrite(Buf, NrWrite);
    {$i+}
    TempError := F^.IOResult;

    Dec(Tries);
  until (TempError = 00) OR (Tries <= 00);

  Config_WriteFile := TempError;
end; { proc. Config_WriteFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Config_DoneFile(var F: pFileObj);
var Tries     : Word;
    TempError : Word;
begin
  if F = nil then EXIT;
  if F^.FileName = '' then EXIT;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileIo, 'Closing file: '+F^.FileName);
  {$ENDIF}

  if F = nil then EXIT;
  Tries := FileRetries + 1;

  repeat
    {$i-}
      F^.Close;
    {$i+}
    TempError := F^.IOResult;

    Dec(Tries);
  until (TempError = 00) OR (Tries <= 00);

  if TempError > 00 then
     Config_ShowError('Unable to close', F^.FileName, TempError);
  Dispose(F, Done);
end; { proc. Config_DoneFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Config_SeekFile(var F: pFileObj; FPos: Longint): Word;
var Tries     : Word;
    TempError : Word;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileIo, 'Seeking file: '+F^.FileName);
  {$ENDIF}
  Tries := FileRetries + 1;

  repeat;
    {$i-}
      F^.Seek(FPos);
    {$i+}
    TempError := F^.IOResult;

    Dec(Tries);
  until (TempError = 00) OR (Tries <= 00);

  Config_SeekFile := TempError;
end; { proc. Config_SeekFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit CFGFILE }
