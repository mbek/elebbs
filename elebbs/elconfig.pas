program ELCONFIG;
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
** ElConfig, Configuration program for "EleBBS", a RA v2.50
** compatible BBS system!
**
** Copyright (c) 1996, 97, 98 by Maarten Bekers
**
** Created : 16-Oct-1996
** Last update : 25-Apr-1999
**
**
*)
{$IFNDEF USINGMGR}
  You must enable USINGMGR else ELCONFIG will use the incorrect screenlength
{$ENDIF}

uses {$IFDEF ELEUNIX}
       Crt,
     {$ELSE}

       {$IFDEF DELCRT}
         Crt32,
       {$ELSE}
         Crt,
       {$ENDIF}
     {$ENDIF}
  ScrnU,
  GenDos,
  GenCfg,
  {$IFNDEF MSDOS}
    SysUtils,
  {$ENDIF}

{$IFNDEF WINGUI}
     Dos,
      Extend,
{$ELSE}
     {$IFNDEF VirtualPascal}
        Dialogs,
     {$ELSE}
       Crt,
     {$ENDIF}
{$ENDIF}
         MiscCfg,
{$IFDEF OVERLAY}
          Overlay,
{$ENDIF}
        {$IFDEF WITH_DEBUG}
          Debug_U,
        {$ENDIF}
           Global,
            CfgScrn,
              Area_lst,
               RalDef,
                CfgDef,
                 FileRout,
                  IoRes_Un,
                    GenFile,
                     BBSkey,
                       StrPath,
                        StrEdit,
                         LongStr,
                          WinTitle,
                           RemScrn,
                            Cases,
                             RemSup,
                              ObjDec,
                               SysVars;

{$IFDEF VPDEMO} {&Dynamic VP11DEMO.LIB} {$ENDIF}

{$IFDEF OVERLAY}
 {$O MISCCFG}
 {$O AREA_LST}
 {$O RALDEF}
 {$O CFGDEF}
 {$O MNUMODEM}
 {$O CFGSCRN}
 {$O LIMLST}
 {$O DISPMNU}
{$ENDIF}

function  GetOvrEnv: String;
var TempStr: String;
begin
  TempStr := GetEnv('ELEOVR');
  if TempStr = '' then TempStr := GetEnv('RAOVR');

  GetOvrEnv := TempStr;
end; { func. GetOvrEnv }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowHelpScreen;
var CH: Char;
begin
  {$IFNDEF ELEUNIX}
   {$IFNDEF DELPHI}
     Crt.ClrScr;
   {$ELSE}
     Crt32.ClrScr;
   {$ENDIF}
  {$ELSE}
    Crt.ClrScr;
  {$ENDIF}
  WriteLn(Output, #254, #32, 'EleBBS CONFIG - Command line parameters');
  WriteLn(Output);
  WriteLn(Output, '-L                     - Fire up the language editor directly');
  WriteLn(Output, '-M                     - Start directly into the menu editor');
  WriteLn(Output, '   [LangNr] [Menu]        - Specify the language number and menu name to edit');
  WriteLn(Output, '-SIMPLE                - Use a very basic characet set for the screen');
  WriteLn(Output, '-B                     - Use black & white color set');
  WriteLn(Output, '-N                     - Don''t check for dupe areas');
  WriteLn(Output);

  CH := UpCase(ReadKey);
  if CH = 'Q' then EXIT;
  if CH = #00 then ReadKey;
end; { proc. ShowHelpScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  JustPath(S: String): String;                   { Returns just path }
var Dir, Name, Ext: String;
begin
{$ifdef With_Debug}   DebugObj.DebugLog(logString, 'Justpath: '+S);{$endif}
  {$IFNDEF WINGUI}
  FSplit(S, Dir, Name, Ext);
  JustPath := Dir;
  {$ELSE}
  JustPath := ExtractFilepath(S);
  {$ENDIF}
end; { func. JustPath }

Const ExtraSize = 1024 * 0; { 20k }

var TempDir: String;
begin
  SetWindowTitle('EleBBS/'+PlatformIdStr+' CONFIG');

  {$IFNDEF DELPHI}
    AssignCrt(CrtOutput);
    System.Rewrite(Crtoutput);
  {$ELSE}
     Move(Output, Crtoutput, SizeOf(Crtoutput)); {  Save this for later use }
  {$ENDIF}

{$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, '');
  DebugObj.DebugLog(logFastScrn, 'ElCONFIG fired up.');
 {$ENDIF}
  RunningBBS := False;

{$IFDEF OVERLAY}
  TempDir := GetOvrEnv;
 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, '.Got environment: '+GetOvrEnv);
 {$ENDIF}

  If (not (TempDir[Length(TempDir)] in [BsChar,':'])) AND (TempDir<>'') then
        TempDir := TempDir + BsChar;
  if NOT (FileExist(TempDir + 'ELCONFIG.OVR')) then
      TempDir := '';

  if TempDir='' then TempDir := JustPath(FSearch('ELCONFIG.OVR', '.'+BsChar+';'+ JustPath(ParamStr(0))+';' + GetEnv('PATH')));
  If not (TempDir[Length(TempDir)] in [BsChar,':']) AND (TempDir<>'') then
        TempDir := TempDir + BsChar;

  if NOT FileExist(TempDir + 'ELCONFIG.OVR') then
    begin
      Writeln('þ Unable to find ELCONFIG.OVR, program halted.');
      Halt(0);
    end; { if }

 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, '.Initting overlay: '+TempDir+'ELCONFIG.OVR');
 {$ENDIF}
  OvrInit(TempDir+'ELCONFIG.OVR');
 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, '.Setting overlay buffer to: '+FStr(OvrGetBuf+ExtraSize));
 {$ENDIF}
  OvrSetBuf(OvrGetBuf + ExtraSize);

  OvrSetRetry(OvrGetBuf div 4);
 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, '.Setting retry buffer to: '+FStr(OvrGetBuf div 4));
 {$ENDIF}

  OvrInitEMS;
 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, '.Initted to EMS: '+FStr(OvrGetBuf+ExtraSize));
 {$ENDIF}
{$ENDIF}

  New(LineCfg);
  New(GlobalCfg);
  InitBasicGlobalVars;
  InitBasicLineVars(LineCfg^);


{$IFDEF OVERLAY}
  InitErrorHandler;
  GenCfgInit;
  MiscInit;                                              { Initialize MiscCFG }
{$ENDIF}

  New(LangObj, Init);

 SetupScrnU;

 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, 'Creating screen.');
 {$ENDIF}

 {-- Test if we are executed with EleMON enabled ---------------------------}
 remUtilitySetup;

 if (Pos('-L', ParamString)=00) AND (Pos('/L', ParamString)=00)
     AND (Pos('-M', ParamString)=00) AND (Pos('/M', ParamString)=00) then
      CreateScreen;


 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, 'Starting of screen.');
  DebugObj.DebugLog(logFastScrn, 'Command-line parameters: "'+ParamString+'"');
  DebugObj.DebugLog(logFastScrn, 'Start initting');
 {$ENDIF}

  StartInitting;
  CheckRegistered;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, 'Cleared');
  {$ENDIF}

 if NOT AskForPassword(GlobalCfg^.Raconfig^.KeyBoardPwd) then
  begin
    TextAttr := 07;
    ClrScr;
    Exit;
  end; { wrong password }

  CursorOff;
  {$IFNDEF FPC}
    KeyProc := ProcessKey;
  {$ELSE}
    KeyProc := @ProcessKey;
  {$ENDIF}

  {$IFDEF ELEUNIX}
    while KeyPressed do ReadKey;
  {$ENDIF}

  if (Pos('?', ParamString) > 00) then ShowHelpScreen else
  if (Pos('-L', ParamString)>00) OR (Pos('/L', ParamString)>00) then
      DoEditLanguage else
  if (Pos('-M', ParamString)>00) OR (Pos('/M', ParamString)>00) then
    DoEditMenus(ParamString) else StartMenu;

  CursorOn;
  Dispose(LangObj, Done);
end. { program ElConfig }


