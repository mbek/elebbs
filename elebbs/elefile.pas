program EleFILE;
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
{$IFDEF WIN32}
 {$IFNDEF VIRTUALPASCAL}
   {$APPTYPE CONSOLE}
 {$ENDIF}
{$ENDIF}
(*
**
** EleFILE, RemoteAccess v2.50 filebase maintenance program (for EleBBS)
**
** Copyright (c) 1996, 97 by Maarten Bekers
**
** Created : 01-Aug-1997
** Last update : 20-May-1998
**
** Note: Started on 25 januari the OS/2 port of EleBBS. The same day
**       EleFILE, EleUSER, EleNODE, ElCONFIG, EleMGR were all up and running.
**
*)
{$IFDEF WITH_FULL}
  To be able to compile EleFile/EleUser/EleNode the WITH_FULL conditional flag has to be disabled
{$ENDIF}

{$IFNDEF ISCGI}
 { !! You need to define this else QASYS won''t work }
{$ENDIF}

uses {$IFDEF DELCRT}
       Crt32,
     {$ENDIF}

     MemMan,
      GenDos,
      IORes_Un,
        CfgRec,
         Global,
          LongStr,
           WordStr,
            Cases,
             StUtils,
              CentrStr,
               ElLog_U,
                StrPath,
                 GenFile,
                   BitWise,
                    ReadCfg,
                     FileObj,
                      ElFile_U,
                       FileRout,
                        FileSys,
                         Desc_U,
                          Ra2Fdb,
                           MgrFdb,
                            BBSkey,
                             Debug_U,
                              Ranges,
                               WinTitle,
                                ObjDec,
                                 elx_bbs,
                                  JDates,
                                   SysVars,


{$IFDEF OS2}
          Dos,
           Crt
{$ENDIF}

{$IFDEF MSDOS}
          Dos,
           Crt
{$ENDIF}

{$IFDEF WIN32}
           SysUtils,
          {$IFNDEF VIRTUALPASCAL}
          {$ELSE}
            Crt, Dos
          {$ENDIF}

{$ENDIF}

{$IFDEF DELPHI}
  Dos,
   Crt
{$ENDIF}

{$IFDEF FPC}
    Dos,
     Crt
{$ENDIF};

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Const CommandOption  : String = '';
      UpdatingName   : String = '';

      AreaSpec       : String = '';
      RestParams     : Array[1..10] of String = ('', '', '', '', '',
                                                 '', '', '', '', '');

      DeleteSema     : Boolean = false;

Type  paramType      = (paramAdd, paramIndex, paramClean, paramComp, paramExp,
                        paramImport, paramMaint, paramFileList, paramSort,
                        paramReArc, paramAdopt, paramDizImport,
                        paramHtmlList);

var Area_F      : pFileObj;                   { FILES.RA buffered file object }
    AreaItems   : Longint;                                      { Total items }
    AreaNow     : Longint;                       { Item we are now processing }
    FilesInf    : FilesRecord;                           { Info for this area }
    ParamString : String;
    SaveAttr    : Byte;
    SaveExitProc: Pointer;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FatalError(S: String);
begin
  AddStatusMsg('');
  AddStatusMsg(#32 + SystemMsgPrefix + S + ^G);
  AddStatusMsg('');

  {$IFNDEF WINGUI}
    TextAttr := SaveAttr;
  {$ENDIF}

  Halt(255);
end; { proc. FatalError }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowHeader;
begin
  {$IFNDEF WINGUI}
  SaveAttr := TextAttr;
  {$ENDIF}

  ClrScr;

  {$IFNDEF WINGUI}
    TextAttr := LightGray;
  {$ENDIF}

  WriteLn('ELEFILE; EleBBS File Database maintenance utility, Version '+VersionID);
  WriteLn('         Copyright 1997-2003 Maarten Bekers, All rights reserved.');
  WriteLn;

  New(LineCfg);
  New(GlobalCfg);
  InitBasicGlobalVars;
  InitBasicLineVars(LineCfg^);

  if NOT MemMan.AllocMem(GlobalCfg^.RaConfig, SizeOf(ConfigRecord), 'ConfigRecord', 'ShowHeader') then
     FatalError('Could not allocate enough memory!');

  if NOT MemMan.AllocMem(GlobalCfg^.ElConfig, SizeOf(EleConfigRecord), 'EleConfigRecord', 'ShowHeader') then
     FatalError('Could not allocate enough memory!');

  if NOT MemMan.AllocMem(LineCfg^.Exitinfo, SizeOf(ExitinfoRecord), 'ExitinfoRecord', 'ShowHeader') then
     FatalError('Could not allocate enough memory!');
end; { proc. ShowHeader }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CloseDownExitProc;
begin
  ExitProc := SaveExitProc;

  if DeleteSema then
    RemoveSema(EleFileSemaphore);
end; { proc. CloseDownExitProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


function MakeCommandLine(CmdType: ParamType; MinParam: Byte): Boolean;

function FileArea(S: String): Boolean;
begin
  FileArea := true;

  if (FVal(AreaSpec)=00) AND (NOT (UpCase(AreaSpec[1]) in ['@', 'G'])) then
    FileArea := false;

  if FVal(AreaSpec)=00 then
   if (AreaSpec[1] = 'G') then
     If (FVal(Copy(AreaSpec, 2, 255)) = 00) then FileArea := false;
end; { func. FileArea }

begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, 'ParamString = '+ParamString);
  {$ENDIF}

  MakeCommandLine := false;

  if ParamCount < Minparam then
    begin
      AddStatusMsg(SystemMsgPrefix + 'Incorrect number of parameters, aborting. Type EleFILE ? for help');
      EXIT;
    end; { if }

  FirstWord(Paramstring, defExtractWord, false); { Remove the command-line option }

  Case CmdType of
     paramAdd  : begin
                   AreaSpec := FirstWord(ParamString, defExtractWord, false);  { Get area number }
                   if FVal(AreaSpec)=00 then
                     begin
                       AddStatusMsg(SystemMsgPrefix + 'Must specify a valid target area number!');
                       EXIT;
                     end; { if }

                   { Gather filename and upload-name }
                   RestParams[01] := SUpCase(FirstWord(ParamString, defExtractWord, false));
                   RestParams[02] := Under2Norm(FirstWord(ParamString, defExtractWord, false));

                   if RestParams[02]='' then RestParams[02] := GlobalCfg^.RaConfig^.SysOp;
                 end; { paramater: ADD }
     paramIndex: AreaSpec := FirstWord(ParamString, defExtractWord, false);
     paramClean: begin
                   AreaSpec := FirstWord(ParamString, defExtractWord, false);

                   if SUpCase(AreaSpec) = '/KM' then
                    begin
                      RestParams[01] := SupCase(AreaSpec);
                      AreaSpec := '';
                    end else RestParams[01] := SUpCase(FirstWord(ParamString, defExtractWord, false));
                 end; { parameter: CLEAN }
     paramComp : AreaSpec := FirstWord(ParamString, defExtractWord, false);
     paramExp  : begin
                   If Pos('/RA', SUpCase(ParamString))>00 then
                     begin
                       RestParams[02] := '/RA';
                       Replace('/RA', '', ParamString);
                     end else RestParams[02] := '';

                   if WordCount(ParamString, defExtractWord, false) > 0 then
                     begin
                       AreaSpec := SUpCase(FirstWord(ParamString, defExtractWord, false));

                       if NOT FileArea(AreaSpec) then
                           begin
                             AddStatusMsg(SystemMsgPrefix + 'You must specify an area when you specify an explicit output name!');
                             EXIT;
                           end; { if }
                     end; { if }

                   RestParams[01] := SUpCase(FirstWord(ParamString, defExtractWord, false));
                 end; { parameter: EXPORT }
    paramImport: begin
                   if Pos('/U', SUpCase(ParamString))>00 then
                     RestParams[02] := Under2Norm(GetValue('/U', ParamString, True))
                      else RestParams[02] := '';

                   If Pos('/ERASE', SUpCase(ParamString))>00 then
                     begin
                       RestParams[03] := '/ERASE';
                       Replace('/ERASE', '', ParamString);
                     end else RestParams[03] := '';

                   If Pos('/MISSING', SUpCase(ParamString))>00 then
                     begin
                       RestParams[04] := '/MISSING';
                       Replace('/MISSING', '', ParamString);
                     end else RestParams[04] := '';

                   If Pos('/C', SUpCase(ParamString))>00 then
                     begin
                       RestParams[05] := GetValue('/C', ParamString, True);
                     end else RestParams[05] := '';

                    AreaSpec := FirstWord(ParamString, defExtractWord, false);
                    if NOT FileArea(AreaSpec) then
                      begin
                        RestParams[01] := AreaSpec;
                        AreaSpec := FirstWord(ParamString, defExtractWord, false);
                      end; { if }

                    RestParams[01] := FirstWord(ParamString, defExtractWord, false);
                    if RestParams[01] <> '' then
                     if (NOT FileArea(AreaSpec)) then
                      begin
                        AddStatusMsg(SystemMsgPrefix + 'You must specify an area when you specify an explicit output name!');
                        EXIT;
                      end; { if }
                 end; { parameter: IMPORT }
    paramMaint : begin
                   If Pos('TOUCHMOD', SUpCase(ParamString))>00 then
                     begin
                       RestParams[02] := 'TOUCHMOD';
                       Replace('TOUCHMOD', '', ParamString);
                     end else RestParams[02] := '';

                   If Pos('TOUCH', SUpCase(ParamString))>00 then
                     begin
                       RestParams[02] := 'TOUCH';
                       Replace('TOUCH', '', ParamString);
                     end;

                   RestParams[01] := FirstWord(ParamString, defExtractWord, false);
                   AreaSpec := FirstWord(ParamString, defExtractWord, false);
                 end; { parameter: MAINTENANCE (Kill, Lock, Unlock, Update) }

  paramFileList: begin
                   RestParams[01] := FirstWord(ParamString, defExtractWord, false);

                   if Pos('/NOHDR', SUpCase(ParamString))>00 then
                     begin
                       RestParams[02] := '/NOHDR';
                       Replace('/NOHDR', '', ParamString);
                     end; { if }

                   if Pos('/7BIT', SUpCase(ParamString))>00 then
                     begin
                       RestParams[03] := '/7BIT';
                       Replace('/7BIT', '', ParamString);
                     end; { if }

                   if Pos('/FORMF', SUpCase(ParamString))>00 then
                     begin
                       RestParams[04] := '/FORMF';
                       Replace('/FORMF', '', ParamString);
                     end; { if }

                   RestParams[05] := GetValue('/S', ParamString, True);
                   RestParams[06] := GetValue('/D', ParamString, True);
                   RestParams[07] := GetValue('/B', ParamString, True);
                   RestParams[08] := GetValue('/F', ParamString, True);

                   AreaSpec := FirstWord(ParamString, defExtractWord, false);
                 end; { parameter: FILELIST }
  paramHtmlList: begin
                   RestParams[01] := GetValue('/S', ParamString, True);
                   RestParams[02] := GetValue('/D', ParamString, True);

                   AreaSpec := FirstWord(ParamString, defExtractWord, false);
                 end; { parameter: HTMLLIST }
     paramSort : begin
                   if Pos('DATE', SUpCase(ParamString))>00 then
                     begin
                       RestParams[01] := 'DATE';
                       Replace('DATE', '', ParamString);
                     end; { if }

                   if Pos('REVERSE', SUpCase(ParamString))>00 then
                     begin
                       RestParams[02] := 'REVERSE';
                       Replace('REVERSE', '', ParamString);
                     end; { if }

                   AreaSpec := FirstWord(ParamString, defExtractWord, false);
                 end; { parameter: SORT }
     paramReArc: AreaSpec := FirstWord(ParamString, defExtractWord, false);
     paramAdopt: begin
                   RestParams[01] := FirstWord(ParamString, defExtractWord, false);
                   AreaSpec := FirstWord(ParamString, defExtractWord, false);
                 end; { parameter: ADOPT }
 paramDizImport: begin
                   AreaSpec := FirstWord(ParamString, defExtractWord, false);
                   RestParams[01] := FirstWord(ParamString, defExtractWord, false);
                 end; { DizImport }
  end; { case }

  MakeCommandLine := true;
end; { proc. MakeCommandLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ParseCommandLine;
var Invalid  : Boolean;
    TempParam: String;
begin
  Invalid := False;

  TempParam := SupCase(Trim(ParamStr(01)));

  If (TempParam='ADD') OR (TempParam='INDEX') OR (TempParam='CLEAN') OR
      (TempParam='COMPRESS') OR (TempParam='EXPORT') OR (TempParam='IMPORT') OR
       (TempParam='KILL') OR (TempParam='LOCK') OR (TempParam='UNLOCK') OR
        (TempParam='FILELIST') OR (TempParam='SORT') OR (TempParam='ADOPT') OR
         (TempParam='UPDATE') OR (TempParam='REARC') OR (TempParam='DESCRIBE') OR
          (TempParam='HTMLIST') then
            CommandOption := TempParam;

  If Pos('?', ParamStr(1)) > 00 then
    begin
       Writeln('  ADD      <area#> <name> [uploader_name] [description]');
       WriteLn('  INDEX    [area#|@arealist]');
       WriteLn('  CLEAN    [area#|@arealist] [/KM]');
       WriteLn('  COMPRESS [area#|@arealist]');
       WriteLn('  EXPORT   [area#|@arealist] [output file] [/RA]');
       WriteLn('  IMPORT   [area#|@arealist] [input file] [/ERASE] [/Uuploader_name] [/MISSING] [/Cxx]');
       WriteLn('  KILL     <filespec> [area#|@arealist]');
       WriteLn('  LOCK     <filespec> [area#|@arealist]');
       WriteLn('  UNLOCK   <filespec> [area#|@arealist]');
       WriteLn('  FILELIST <output file> [area#|@arealist] [/Ssecurity] [/Ddays old]');
       WriteLn('                         [/Bbanner] [/Ffooter] [/NOHDR] [/7BIT] [/FORMF]');
       WriteLn('  SORT     [area#|@areallist] [DATE] [REVERSE]  (Default=NAME,FORWARD)');
       WriteLn('  ADOPT    <filespec> [area#|@arealist]');
       WriteLn('  UPDATE   <filespec> [area#|@arealist] [TOUCH|TOUCHMOD]');
       WriteLn('  REARC    [area#|@arealist]');
       WriteLn('  DESCRIBE [area#|@arealist] [filespec]');
       WriteLn('  HTMLIST  [area#|@arealist] [/Ssecurity] [/Ddays old]');
       WriteLn;
       WriteLn(' þ [] Parameters are optional, <> Parameters are mandatory.');
       WriteLn('   (Area#=0) means all areas, wildcards are valid. Please refer to the');
       WriteLn('   documentation for a complete command summary.');

       MemMan.ReleaseMem(GlobalCfg^.RaConfig, SizeOf(ConfigRecord));
       MemMan.ReleaseMem(Linecfg^.Exitinfo, SizeOf(ExitinfoRecord));

       {$IFNDEF WINGUI}
         TextAttr := SaveAttr;
       {$ENDIF}
       Halt(255);
    end; { if }

  if (ParamCount = 00) OR (CommandOption='') then
    begin
      WriteLn;
      Write(Dup(#205, 80));
      WriteLn(SystemMsgPrefix, 'Nothing to do! Type ELEFILE ? for command-line help.');
      WriteLn;

      MemMan.ReleaseMem(GlobalCfg^.RaConfig, SizeOf(ConfigRecord));
      MemMan.ReleaseMem(LineCfg^.Exitinfo, SizeOf(ExitinfoRecord));
      {$IFNDEF WINGUI}
        TextAttr := SaveAttr;
      {$ENDIF}
      Halt(255);
    end; { Invalid }

  Writeln(CommandOption);
  Write(Dup(#205, 80));
end; { proc. ParseCommandLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function UpdateFunc(Name: DelStr; CurPos: Word): Boolean;
begin
  if SUpCase(Trim(Name)) = UpdatingName then
    UpdateFunc := True
     else UpdateFunc := False;
end; { func. UpdateFunc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CreateShowWindow;
begin
 {$IFNDEF WIN32}
    Window(01, 06, 80, 25);
 {$ENDIF}

 {$IFDEF WIN32}
   {$IFDEF VirtualPascal}
     Window(01, 06, 80, 25);
   {$ENDIF}
 {$ENDIF}
end; { proc. CreateShowWindow }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowAreaName(FilesInf: FilesRecord);
var OldX,
    OldY           : Byte;
    OldMin, OldMax : Word;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, 'ShowAreaName (' + FilesInf.Name + ')');
  {$ENDIF}

  {$IFNDEF WINGUI}
    OldY := WhereY;
    OldX := WhereX;
    OldMin := WindMin;
    OldMax := WindMax;

    {$IFNDEF WIN32}
      Window(01, 01, 80, 25);
    {$ENDIF}
    {$IFDEF VIRTUALPASCAL}
      Window(01, 01, 80, 25);
    {$ENDIF}
    GotoXY(13, 04);
    ClrEol;

    WriteLn('Area ', Filesinf.AreaNum:5, ' - ', FilesInf.Name);

    CreateShowWindow;
    GotoXY(OldX, OldY);
  {$ENDIF}
end; { proc. ShowareaName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function OpenFileAreaScanner: Boolean;
begin
  OpenFileAreaScanner := false;

  New(Area_F, Init);
  Area_F^.Assign(FilesFileName);
  Area_F^.FileMode := ReadMode + DenyNone;
  if NOT Area_F^.Open(1) then
    begin
      Dispose(Area_F, Done);
      EXIT;
    end; { if }

  AreaItems    := Area_F^.FileSize DIV SizeOf(FilesRecord);
  AreaNow      := 00;
  OpenFileAreaScanner := true;
end; { func. OpenFileAreaScanner }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CloseFileAreaScanner;
begin
  Dispose(Area_F, Done);
end; { proc. CloseFileAreaScanner }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function IsZero(S: String): Boolean;
var Counter: Longint;
begin
  IsZero := true;

  for Counter := 01 to Length(s) do
   if (NOT (S[Counter] in ['0', #32])) then IsZero := false;
end; { func. iszero }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function IncludeAreaInList(FilesInf: FilesRecord): Boolean;

function DoEval(Eval: String): Boolean;
var TempGroup: Word;
    PartOne  : Word;
    PartTwo  : Word;
begin
  DoEval := false;
  Eval := SUpCase(Eval);

  if (Eval='') OR (IsZero(Eval)) then
    begin
      DoEval := true;
      exit;
    end; { if }

  Case Eval[1] of
         'G' : begin
                 if Pos('-', Eval)=00 then
                    begin
                      TempGroup := FVal(Copy(Eval, 2, 255));

                      if (FilesInf.Group=TempGroup) OR
                          (FilesInf.AltGroup[1] = TempGroup) OR
                           (FilesInf.AltGroup[2] = TempGroup) OR
                            (FilesInf.AltGroup[3] = TempGroup) then DoEval := true;
                    end
                      else begin
                             PartOne := FVal(Copy(Eval, 2, Pos('-', Eval) - 02));
                             PartTwo := FVal(Copy(Eval, Pos('-', Eval) + 01, 255));

                             if (InRange(FilesInf.Group, PartOne, PartTwo)) OR
                                 (InRange(FilesInf.AltGroup[1], PartOne, PartTwo)) OR
                                  (InRange(FilesInf.AltGroup[2], PartOne, PartTwo)) OR
                                   (InRange(FilesInf.AltGroup[3], PartOne, PartTwo)) then DoEval := true;
                           end; { else }
               end; { if (group) }
    '0'..'9' : begin
                 if Pos('-', Eval)=00 then
                   DoEval := (FilesInf.AreaNum = FVal(Eval)) else
                    begin
                      PartOne := FVal(Copy(Eval, 1, Pos('-', Eval) - 01));
                      PartTwo := FVal(Copy(Eval, Pos('-', Eval) + 01, 255));

                      if InRange(FilesInf.AreaNum, PartOne, PartTwo) then
                        DoEval := true;
                    end; { else }

               end; { area-number }
  end; { case }
end; { func. DoInclude }

var Temp_F : Text;
    Counter: Word;
    TempStr: String;
begin
  IncludeAreaInList := false;
  if (FilesInf.Name='') OR (FilesInf.AreaNum=00) then EXIT;

  If AreaSpec[1] <> '@' then IncludeAreaInList := DoEval(AreaSpec)
   else begin
          if Trim(Copy(AreaSpec, 2, 250))='' then EXIT;

          Assign(Temp_F, Copy(AreaSpec, 2, 250));
          {$i-} System.Reset(Temp_F); {$i+}
          if IOresult>00 then
            begin
              AddStatusMsg(SystemMsgPrefix + 'Cannot find include file ('+Copy(AreaSpec, 2, 250)+').');
              EXIT;
            end; { if }

          While NOT Eof(Temp_F) do
            begin
              ReadLn(Temp_F, TempStr);

              While Pos(',', TempStr) > 00 do
               Replace(',', #32, TempStr);

              For Counter := 01 to WordCount(TempStr, defExtractWord, false) do
               if DoEval(ExtractWord(TempStr, Counter, defExtractWord, false, false)) then
                 begin
                   IncludeAreaInList := true;
                   break;
                 end; { if }
            end; { while }

          Close(Temp_F);
        end; { if }
end; { func. IncludeAreaInList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetNextfileArea: Boolean;
var NumRead: NumReadType;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, 'GetNextfileArea (BEGIN)');
  {$ENDIF}

  GetNextFileArea := false;

  While (AreaNow < AreaItems) AND (AreaItems>00) do
    begin
      NumRead := Area_F^.BlkRead(FilesInf, SizeOf(FilesRecord));
      Inc(AreaNow);

      if IncludeAreaInList(FilesInf) then
        begin
          FilesInf.FilePath := ForceBack(FilesInf.FilePath);

          GetNextfileArea := true;
          BREAK;
        end; { if }

    end; { while }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, 'GetNextFileArea (END)');
  {$ENDIF}
end; { func. GetNextFileArea }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function AddFiles: Boolean;
var HdrInf     : FilesHdrRecord;
    TxtInf     : TxtRec;
    Counter    : Byte;
    FileMgr    : MgrFileObj;
    TempStr    : String;
    FilePath   : String;
    Dirinfo    : SearchRec;
begin
  AddFiles := false;

  CreateShowWindow;
  if NOT MakeCommandLine(paramADD, 03) then EXIT;
  GetFilesRecord(FilesInf, FVal(AreaSpec), true);          { Get area number }

  {-- Extract the filepath from this ----------------------------------------}
  FilePath := FilesInf.FilePath;
  if JustPath(RestParams[01]) <> '' then
    begin
      FilePath := JustPath(Restparams[01]);
      RestParams[01] := JustName(RestParams[01]);
    end; { if }


  {--------------------- Make sure file really exists -----------------------}
  if NOT FileExist(FilePath + RestParams[01]) then
     begin
        RaLog('!', 'Cannot find file: '+FilePath + RestParams[01] + ', aborting!');
        AddStatusMsg('Cannot find file: '+FilePath + RestParams[01] + ', aborting.');
        EXIT;
     end; { file does not exist }

  {--------------------- Let viewer know where we are -----------------------}
  ShowAreaName(FilesInf);                    { Display area where file to add }

  {--------------------- Start searching for the wildcard -------------------}
  FindFirst(FilePath + RestParams[01], AnyFile - VolumeID, DirInfo);

  {------------------------ Initialize records ------------------------------}
  While DosError = 00 do
   begin
     {-- Copy the file ------------------------------------------------------}
     if (Supcase(FilePath) <> Supcase(FilesInf.FilePath)) then
       begin
         FileCopy(FilePath + DirInfo.Name,
                  FilesInf.FilePath + DirInfo.Name, TRUE, FALSE);
       end; { if }

     {-- Add the file -------------------------------------------------------}
     FillChar(HdrInf, SizeOf(HdrInf), #00);
     FillChar(TxtInf, SizeOf(TxtInf), #00);

     HdrInf.Name       := DirInfo.Name;
     HdrInf.Size       := GetFileSize(Filesinf.FilePath + DirInfo.Name, 1);
     HdrInf.Crc32      := -01;
     HdrInf.Uploader   := RestParams[02];
     HdrInf.UploadDate := GetDosDate;
     HdrInf.FileDate   := GetPackedFileTime(FilesInf.FilePath + DirInfo.Name);
     HdrInf.LastDL     := GetDosDate;
     HdrInf.TimesDL    := 00;
     HdrInf.Attrib     := 00;
     HdrInf.Password   := '';
     HdrInf.Cost       := 00;
     HdrInf.LongDescPtr:= -1;
     FileMgr.Init(FilesInf.AreaNum, true);
     HdrInf.LfnPtr     := FileMgr.AddLfnPtr(Dirinfo.Name, FilesInf.FilePath + Dirinfo.Name, HdrInf.Name);
     FileMgr.Done;

     {-------------------- Check wether the file isn't duplicate ---------------}
     if SearchNameInArea(FilesInf.AreaNum, HdrInf.Name, false) >= 00 then
       begin
         AddStatusMsg(MakeLen(HdrInf.Name, 13, Space, false, false) + '  - Duplicate, rejected.');
         RaLog('!', 'File "'+HdrInf.Name+' rejected, already exists in area #'+FStr(FilesInf.AreaNum));
       end { if }
        else
         {$IFNDEF DELPHI}
          if Dirinfo.Attr AND Dos.Directory = 0 then
         {$ELSE}
          if DirInfo.Attr AND Sysutils.faDirectory = 0 then
         {$ENDIF}
             begin
               FileMgr.Init(FilesInf.AreaNum, True);

               {---------------------- Build description line ------------------}
               TempStr := '';

               if ParamCount > 4 then
                 begin
                   For Counter := 05 to ParamCount do
                     TempStr := TempStr + ParamStr(Counter) + #32;

                   TempStr := Trim(TempStr);

                   Move(TempStr[1], TxtInf, Length(TempStr));
                 end; { if }

               {------------- And finally really add the file ------------------}
               if TempStr <> '' then
                 HdrInf.LongDescPtr := FileMgr.AddDescription(TxtInf);
               FileMgr.AddHeader(HdrInf);
               FileMgr.Read(FileMgr.TotalRecords - 1);

               AddStatusMsg(FileMgr.GetFileName + '  - Added succesfully.');
               RaLog('>', 'Added '+HdrInf.Name+' to filebase.');

               FileMgr.Done;

               AddFiles := True;
          end; { else }

     FindNext(DirInfo);
   end; { while }

  FindClose(Dirinfo);
end; { proc. AddFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function IndexFiles: Boolean;
var FileMgr : MgrFileObj;
begin
  IndexFiles := false;
  CreateShowWindow;

  if NOT MakeCommandLine(paramIndex, 01) then EXIT;
  if NOT OpenFileAreaScanner then EXIT;

  While GetNextFileArea do
    begin
      ShowAreaName(FilesInf);

      FileMgr.Init(FilesInf.AreaNum, True);
      FileMgr.FileBaseMaintenance(FilesInf.AreaNum,
                                  {$IFDEF FPC}@{$ENDIF}CreateIdxBaseProc,
                                  true);
      FileMgr.Done;
    end; { While GetNextFileArea }

  CloseFileAreaScanner;
  IndexFiles := true;
end; { func. IndexFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CleanFiles: Boolean;
var TotalMoved: Word;
begin
  CleanFiles := false;
  CreateShowWindow;

  if NOT MakeCommandLine(paramClean, 01) then EXIT;
  if NOT OpenFileAreaScanner then EXIT;

  While GetNextFileArea do
    begin
      ShowAreaName(FilesInf);

      Case CleanFileBase(FilesInf.AreaNum,
                         (Pos('/KM', RestParams[01]) > 00), TotalMoved) of
         fdb_Ok      : ;
         fdb_NoHdr   : AddStatusMsg('Area '+ FStr(FilesInf.AreaNum) + ' does not exist!');
         fdb_NoLock  : AddStatusMsg('Filebase in-use by another application');
         fdb_TooLarge: AddStatusMsg('Filebase too large to process (not enough memory/diskspace)');
         fdb_NoParam : AddStatusMsg('Incorrect (number of) parameters');
      end; { case }

      CompressFileBase(FilesInf.AreaNum);

      if TotalMoved>00 then
       begin
         RaLog('>', '  Area '+MakeLen(Fstr(FilesInf.AreaNum), 5, Space, True, false) + ' - ' + FilesInf.Name);

         if TotalMoved>1 then
          RaLog('!', '  Removed '+FStr(TotalMoved)+' entries') else
           RaLog('!', '  Removed 1 entry');
       end; { if totalmoved }
    end; { While }

  CloseFileAreaScanner;
  CleanFiles := True;
end; { func. CleanFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CompressFiles: Boolean;
begin
  CompressFiles := false;
  CreateShowWindow;
  if NOT MakeCommandLine(paramComp, 01) then EXIT;
  if NOT OpenFileAreaScanner then EXIT;

  While GetNextFileArea do
    begin
      ShowAreaName(FilesInf);

      Case CompressFileBase(FilesInf.AreaNum) of
             fdb_Ok      : ;
             fdb_NoHdr   : AddStatusMsg('Area '+ FStr(FilesInf.AreaNum)+' does not exist!');
             fdb_NoLock  : AddStatusMsg('Filebase in-use by another application');
             fdb_TooLarge: AddStatusMsg('Filebase too large to process (not enough memory/diskspace)');
      end; { case }
    end; { while }

  CompressFiles := true;
  CloseFileAreaScanner;
end; { func. CompressFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ExportFiles: Boolean;
var NrExported: Word;
    RaExport  : Boolean;
begin
  ExportFiles := false;
  CreateShowWindow;
  if NOT MakeCommandLine(paramExp, 01) then EXIT;
  if NOT OpenFileAreaScanner then EXIT;

  if RestParams[1]<> '' then EraseFile(RestParams[1]);
  RaExport := (RestParams[2] = '/RA');

  While GetNextFileArea do
    begin
      if ( (NOT ReadBit(FilesInf.Attrib, 3)) OR (RestParams[1] <> '')) then
         begin
           ShowAreaName(FilesInf);

           Case ExportFileBase(FilesInf.AreaNum, RestParams[1], NrExported, false, RaExport) of
              fdb_Ok      : ;
              fdb_NoHdr   : AddStatusMsg('Area '+ FStr(FilesInf.AreaNum) + ' does not exist!');
              fdb_NoLock  : AddStatusMsg('Filebase in-use by another application');
              fdb_NoOutput: AddStatusMsg('Unable to create output file');
              fdb_TooLarge: AddStatusMsg('Filebase too large to process (not enough memory/diskspace)');
           end; { case }

           RaLog('>', '  Area '+MakeLen(FStr(FilesInf.AreaNum), 5, Space, True, false) + ' - '+FilesInf.Name);
           RaLog('>', '  Exported '+FStr(NrExported)+' entries');

        end; { if CheckAreaInList }

    end; { While }

  ExportFiles := true;
  CloseFileAreaScanner;
end; { func. ExportFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function MaintenanceFile(Option: String): Boolean;
var TouchMethod  : Byte;
    NrDeleted    : Word;
    RecNum       : Longint;
begin
  MaintenanceFile := false;
  CreateShowWindow;
  if NOT MakeCommandLine(paramMaint, 02) then EXIT;
  if NOT OpenFileAreaScanner then EXIT;

  TouchMethod := 00;
  if Pos('TOUCH', RestParams[02]) > 00 then TouchMethod := 02;
  if Pos('TOUCHMOD', RestParams[02]) > 00 then TouchMethod := 01;

  While GetNextFileArea do
    begin
      ShowAreaName(FilesInf);
      NrDeleted := 00;

      if (Option='KILL') then
        begin
          RecNum := SearchNameInArea(FilesInf.AreaNum,
                                     RestParams[1],
                                     true);
          ElFile_U.DeleteFile(FilesInf.AreaNum, RestParams[01], RecNum, true, NrDeleted, del_Normal, false, false);
          CompressFileBase(FilesInf.AreaNum);

          if NrDeleted>00 then
            begin
              RaLog('!', '  Area '+MakeLen(FStr(FilesInf.AreaNum), 5, Space, True, false) + ' - '+FilesInf.Name);
              RaLog('!', '  Killed '+FStr(NrDeleted)+' entries');
            end; { if }
        end; { if Option=KILL }

      if (Option='LOCK') or (Option='UNLOCK') then
         Case LockUnlockFileBase(FilesInf.AreaNum, Option, RestParams[01]) of
           fdb_Ok      : ;
           fdb_NoHdr   : AddStatusMsg('Area '+ FStr(FilesInf.AreaNum) + ' does not exist!');
           fdb_NoLock  : AddStatusMsg('Filebase in-use by another application');
           fdb_NoOutput: AddStatusMsg('Unable to create output file');
           fdb_TooLarge: AddStatusMsg('Filebase too large to process (not enough memory/diskspace)');
         end; { case }

       if (Option='UPDATE') then
         UpdateFile(FilesInf.AreaNum, RestParams[01], TouchMethod);
    end; { While }

  CloseFileAreaScanner;
  MaintenanceFile := true;
end; { func. MaintenanceFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AddText(FName, Include: String; TextStr: String);
var Write_F: Text;
    Read_F : Text;
    TempStr: String;
begin
  if FName = '' then EXIT;

  if TextStr = '' then
   if NOT FileExist(Include) then EXIT;

  Assign(Write_F, FName);
  {$i-} Append(Write_F); {$I+}
  if IOResult>00 then
    begin
      {$i-} System.ReWrite(Write_F); {$I+}
      if IOResult>00 then EXIT;
    end; { if }

  if TextStr = '' then
   begin
     Assign(Read_F, Include);
     {$i-} System.Reset(Read_F); {$I+}
     if IOResult>00 then
        begin
          Close(Write_F);
          EXIT;
        end; { if }
   end; { If }

  if TextStr='' then
  begin
  {$i-}
    While NOT Eof(Read_F) do
      begin
        ReadLn(Read_F, Tempstr);
        WriteLn(Write_F, TempStr);
      end; { while }
  end { if }
   else WriteLn(Write_F, TextStr);

    Close(Write_F);
    if TextStr='' then Close(Read_F);
  {$i+}
end; { proc. AddText }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FileListFiles: Boolean;
var Security  : Word;
    DaysOld   : Word;
    Banner    : String;
    Footer    : String;
    NoHdr     : Boolean;
    SevenBit  : Boolean;
    FormFeeds : Boolean;
    NrFiles,
    NrkBytes  : Longint;
begin
  FileListFiles := false;
  CreateShowWindow;

  if NOT MakeCommandLine(paramFileList, 02) then EXIT;
  if NOT OpenFileAreaScanner then EXIT;

                 { Volgorde is belangrijk, andres krijg je /FORMF ipv /FOOTER }
  NoHdr     := RestParams[02] = '/NOHDR';
  SevenBit  := RestParams[03] = '/7BIT';
  FormFeeds := RestParams[04] = '/FORMF';
  Security  := FVal(RestParams[05]);
  DaysOld   := FVal(RestParams[06]);
  Banner    := RestParams[07];
  Footer    := RestParams[08];

  NrKBytes := 00;
  NrFiles := 00;

  if FileExist(RestParams[01]) then EraseFile(RestParams[01]);
  if (NOT NoHdr) then AddText(RestParams[01], Banner, '');

  While GetNextFileArea do
   If (FilesInf.ListSecurity <= Security) OR (Security=00) then
     begin
       ShowAreaName(FilesInf);

       Case MakeFileList(FilesInf.AreaNum, RestParams[01], DaysOld, NoHdr, SevenBit, FormFeeds, NrFiles, NrKBytes) of
          fdb_Ok      : ;
          fdb_NoIdx   : AddStatusMsg('Area '+ FStr(FilesInf.AreaNum) + ' does not exist!');
          fdb_NoLock  : AddStatusMsg('Filebase in-use by another application');
          fdb_NoOutput: AddStatusMsg('Unable to create output file');
          fdb_TooLarge: AddStatusMsg('Filebase too large to process (not enough memory/diskspace)');
       end; { case }
     end; { while }

  If NOT NoHdr then
    AddText(RestParams[01], '', MakeAreaTopic('Listed '+FStr(NrFiles)+ ' files,  '+FStr(NrkBytes) + 'k', SevenBit));

  AddText(RestParams[01], Footer, '');
  CloseFileAreaScanner;

  FileListFiles := True;
end; { proc. FileListfiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure LoadCharMap(var CharMap: Array of Char);
var Counter: Byte;
    Char_F : pFileObj;
    TempStr: String;
begin
  for Counter := 0 to 255 do
    Charmap[Counter] := Chr(Counter);

  {--------------------------- Open the character file ----------------------}
  New(Char_F, Init);
  Char_F^.Assign('charmap.ctl');
  Char_F^.FileMode := ReadMode + DenyNone;
  if NOT Char_F^.Open(1) then
    begin
      RaLog('>', 'File "charmap.ctl" not found - char filtering disabled');

      Dispose(Char_F, Done);
      EXIT;
    end; { if }

  While NOT Char_F^.EOF do
    begin
      Char_F^.ReadLn(TempStr);

      if TempStr[1] <> ';' then
        begin
          Counter := FVal(Copy(TempStr, 1, 3));

          CharMap[Counter] := Chr(FVal(Copy(TempStr, 8, 3)));
        end; { if }

    end; { while }

  Dispose(Char_F, Done);
end; { proc. LoadCharMap }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function HtmlScriptFiles: Boolean;
var Security   : Word;
    DaysOld    : Word;
    ParamString: String;
    FirstArea  : Boolean;
    elxObj     : pElxBbsObj;
begin
  {-- initialize some variables --------------------------------------------}
  HtmlScriptFiles := false;
  FirstArea := TRUE;
  CreateShowWindow;

  {- EleXer uses the language path for its ELM files -----------------------}
  New(lineCfg^.Language);
  LineCfg^.Language^.QuesPath := '';

  {-- make sure we have a correct commandline and we can open the fb -------}
  if NOT MakeCommandLine(paramHtmlList, 02) then EXIT;
  if NOT OpenFileAreaScanner then EXIT;

  {-- get the parameters ---------------------------------------------------}
  Security  := FVal(RestParams[01]);
  DaysOld   := FVal(RestParams[02]);

  {-- warn if this is MSDOS ------------------------------------------------}
  {$IFDEF MSDOS}
    AddStatusMsg('Warning - Running this command under DOS will likely run');
    AddStatusMsg('          out of memory!');
  {$ENDIF}

  {-- we first run the script for the header -------------------------------}
  ParamString := FStr(DaysOld) + ' ' + FStr(Security);

  {-- now loop through all areas -------------------------------------------}
  While GetNextFileArea do
   if (FilesInf.ListSecurity <= Security) OR (Security=00) then
     begin
       {-- display the header ----------------------------------------------}
       if FirstArea then
         begin
           New(elxObj, Init);
            elxObj^.RunElexerScript('fdbhtml', ParamString + ' ' + FStr(FilesInf.AreaNum) + ' ' + FStr(0), false);
           Dispose(elxobj, Done);

           FirstArea := FALSE;
         end; { if }

       {-- show the area name ----------------------------------------------}
       ShowAreaName(FilesInf);

       New(elxObj, Init);
       if NOT elxObj^.RunElexerScript('fdbhtml', ParamString + ' ' + FStr(FilesInf.AreaNum) + ' 2', false) then
          AddStatusMsg('Unable to load "fdbhtml.elm"');
       Dispose(elxObj, Done);
     end; { while }

  {-- now we run the script for the footer ---------------------------------}
  New(elxObj, Init);
    elxobj^.RunElexerScript('fdbhtml', FStr(DaysOld) + ' ' + FStr(Security) +  ' ' +
                               FStr(FilesInf.AreaNum) + ' ' + FStr(1), false);
  Dispose(elxObj, Done);

  CloseFileAreaScanner;
  HtmlScriptFiles := True;

  Dispose(LineCfg^.Language);
end; { func. HtmlScriptFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ImportFiles: Boolean;
var NrImported   : Word;
    DoMissing    : Boolean;
    KillFile     : Boolean;
begin
  ImportFiles := false;
  CreateShowWindow;
  if NOT MakeCommandLine(paramImport, 01) then EXIT;
  if NOT OpenFileAreaScanner then EXIT;

  KillFile := (Pos('/ERASE', SUpCase(RestParams[3])) > 00);
  DoMissing := (Pos('/MISSING', SUpCase(RestParams[4])) > 00);

  While GetNextFileArea do
    begin
      ShowAreaName(FilesInf);

      Case ImportFileBase(FilesInf.AreaNum, RestParams[01], RestParams[02],
                          FVal(RestParams[05]), KillFile, DoMissing, NrImported) of
         fdb_Ok      : ;
         fdb_NoHdr   : AddStatusMsg('Area '+ FStr(FilesInf.AreaNum) + ' does not exist!');
         fdb_NoLock  : AddStatusMsg('Filebase in-use by another application');
         fdb_NoInput : AddStatusMsg('Input file does not exists!');
         fdb_TooLarge: AddStatusMsg('Filebase too large to process (not enough memory/diskspace)');
      end; { case }

      if NRImported>00 then
        begin
          RaLog('>', '  Area '+MakeLen(FStr(FilesInf.AreaNum), 5, Space, True, false) + ' - '+FilesInf.Name);
          RaLog('>', '  Imported '+FStr(NrImported)+' entries');
        end; { if }

      if KillFile then
        begin
          if RestParams[01] <> '' then EraseFile(RestParams[01])
           else EraseFile(FilesInf.FilePath + 'files.bbs');
         end; { if }
    end; { while }

  ImportFiles := true;
  CloseFileAreaScanner;
end; { func. ImportFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function SortFiles: Boolean;
var DateSort     : Boolean;
    ReverseSort  : Boolean;
begin
  SortFiles := false;
  CreateShowWindow;
  if NOT MakeCommandLine(paramSort, 01) then EXIT;
  if NOT OpenFileAreaScanner then EXIT;

  DateSort    := RestParams[01] = 'DATE';
  ReverseSort := RestParams[02] = 'REVERSE';

  While GetNextFileArea do
    begin
      ShowAreaName(FilesInf);

      Case SortFileBase(FilesInf.AreaNum, DateSort, ReverseSort, -1, -1, True) of
         fdb_Ok      : ;
         fdb_NoHdr   : AddStatusMsg('Area '+ FStr(FilesInf.AreaNum) + ' does not exist!');
         fdb_NoLock  : AddStatusMsg('Filebase in-use by another application');
         fdb_TooLarge: AddStatusMsg('Filebase too large to process (not enough memory/diskspace)');
      end; { case }
    end; { While }

  CloseFileAreaScanner;
  SortFiles := true;
end; { func. SortFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ReArchiveFiles: Boolean;
var NrReArced  : Word;
begin
  ReArchiveFiles := false;
  CreateShowWindow;
  if NOT MakeCommandLine(paramReArc, 01) then EXIT;
  if NOT OpenFileAreaScanner then EXIT;

  While GetNextFileArea do
    begin
      ShowAreaName(FilesInf);

      Case ReArcFileBase(FilesInf.AreaNum, nrRearced) of
         fdb_Ok        : ;
         fdb_NoHdr     : AddStatusMsg('Area '+ FStr(FilesInf.AreaNum) + ' does not exist!');
         fdb_NoLock    : AddStatusMsg('Filebase in-use by another application');
         fdb_TooLarge  : AddStatusMsg('Filebase too large to process (not enough memory/diskspace)');
         fdb_NoTempPath: AddStatusMsg('Temporarily path for archive conversions does not exist');
      end; { case }

      if NrRearced>00 then
       begin;
         RaLog('>', '  Area '+MakeLen(FStr(FilesInf.AreaNum), 5, Space, True, false) + ' - '+FilesInf.Name);
         RaLog('>', '  Rearchived '+FStr(NrRearced)+' entries');
       end; { if }
    end; { While }

  ReArchiveFiles := false;
  CloseFileAreaScanner;
end; { proc. ReArchiveFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function AdoptFiles: Boolean;
var HdrInf      : FilesHdrRecord;
    NrAdded     : Longint;
    FileMgr     : MgrFileObj;
    DirInfo     : SearchRec;
begin
  AdoptFiles := false;
  CreateShowWindow;
  if NOT MakeCommandLine(paramAdopt, 01) then EXIT;
  if NOT OpenFileAreaScanner then EXIT;

  While GetNextFileArea do
    begin
      ShowAreaName(FilesInf);

      NrAdded := 00;

      FindFirst(FilesInf.FilePath + RestParams[01], AnyFile - VolumeID, DirInfo);

      While DosError=00 do
        begin
          if Dirinfo.Attr AND Directory = 00 then
              If (NOT SearchNameInArea(FilesInf.AreaNum, DirInfo.Name, false) >= 00) then
                begin
                  AddStatusMsg(Dirinfo.Name);

                  FillChar(HdrInf, SizeOf(HdrInf), #00);
                  HdrInf.Name       := SupCase(Trim(DirInfo.Name));
                  HdrInf.Size       := GetFileSize(Filesinf.FilePath +  DirInfo.Name, 1);
                  HdrInf.Crc32      := -1;
                  HdrInf.Uploader   := '';
                  HdrInf.UploadDate := GetDosDate;
                  HdrInf.FileDate   := GetPackedFileTime(FilesInf.FilePath + DirInfo.Name);
                  HdrInf.LastDL     := GetDosDate;
                  HdrInf.TimesDL    := 00;
                  HdrInf.Attrib     := 00;
                  HdrInf.Password   := '';
                  HdrInf.Cost       := 00;
                  HdrInf.LongDescPtr:= -1;

                  FileMgr.Init(FilesInf.AreaNum, true);
                  HdrInf.LfnPtr := FileMgr.AddLfnPtr(Dirinfo.Name, FilesInf.FilePath + Dirinfo.Name, Hdrinf.Name);
                  FileMgr.AddHeader(HdrInf);
                  FileMgr.Done;

                  Inc(NrAdded);

                  {-- Log that the file is added ------------------------------}
                  RaLog('>', '     Adopted ''' + FilesInf.FilePath + Dirinfo.Name + '''.');
                end; { if not }

           FindNext(DirInfo);
        end; { while }

      FindClose(DirInfo);

      if NrAdded>00 then
        begin
          RaLog('>', '   Area '+MakeLen(Fstr(FilesInf.AreaNum), 5, Space, True, false)+ ' - ' +FilesInf.name);
          RaLog('>', '   Adopt '+FStr(NrAdded)+' entries');
        end; { if }
    end; { while }

  AdoptFiles := true;
  CloseFileAreaScanner;
end; { proc. AdoptFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ImportDizFiles: Boolean;
begin
  ImportDizFiles := false;
  CreateShowWindow;
  if NOT MakeCommandLine(paramDizImport, 01) then EXIT;
  if NOT OpenFileAreaScanner then EXIT;

  While GetNextFileArea do
    begin
      ShowAreaName(FilesInf);

      Case DescribeFileBase(FilesInf.AreaNum, RestParams[01]) of
         fdb_Ok        : ;
         fdb_NoHdr     : AddStatusMsg('Area '+ FStr(FilesInf.AreaNum) + ' does not exist!');
         fdb_NoLock    : AddStatusMsg('Filebase in-use by another application');
         fdb_NoInput   : AddStatusMsg('Input file does not exists!');
         fdb_TooLarge  : AddStatusMsg('Filebase too large to process (not enough memory/diskspace)');
         fdb_NoMemory  : AddStatusMsg('Not enough memory for extraction');
         fdb_NoTempPath: AddStatusMsg('No temp path available');
      end; { case }
    end; { while }

  ImportDizFiles := true;
  CloseFileAreaScanner;
end; { func. ImportDizFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

var Counter  : Byte;
    TempBool : Boolean;
    TempPath : String;
    TempByte : Byte;
    TmpString: String;
begin
  SetWindowTitle('EleFILE');

  {$IFNDEF DELPHI}
    AssignCrt(CrtOutput);
    System.Rewrite(Crtoutput);
  {$ELSE}
     Move(Output, Crtoutput, SizeOf(Crtoutput)); {  Save this for later use }
  {$ENDIF}

  ParamString := '';
  TempByte := ParamCount;

  for Counter := 01 to TempByte do
   if SUpCase(ParamStr(Counter)) <> '/XDEBUGLOG' then
     begin
       if Counter <> TempByte then TmpString := ParamStr(Counter) + #32
         else TmpString := ParamStr(Counter);
       ParamString := ParamString + TmpString;
     end; { if }

  RunningBBS := False;

  for Counter:=01 to paramcount do
   if pos('/XDEBUGLOG', supcase(ParamStr(counter)))>00 then DebugObj.DebugLogging := True;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'EleFILE - DebugLog - ' + ParamString);
  {$ENDIF}

  SaveExitProc := ExitProc;
  ExitProc := @CloseDownExitproc;
  ShowHeader;
  ReadConfigRa;
  InitSystemNames;
  CheckRegistered;
  ParseCommandLine;

  RaLog('>', '');
  RaLog('>', 'EleFILE; File database maintenance utility fired up');
  RaLog('>', CommandOption);

  if (SemaExist(EleFileSemaphore)) OR
      (SemaExist(EleFMgrSemaphore)) then
    begin
      if SemaExist(EleFileSemaphore) then
        begin
          RaLog('!', 'EleFILE is already running ('+EleFileSemaPhore + ' exists), aborting.');
          WriteLn(#32, SystemMsgPrefix, 'EleFILE is already running ('+EleFileSemaPhore + ' exists), aborting');
        end
          else begin
                 RaLog('!', 'EleMGR is running ('+EleFMGRSemaPhore + ' exists), aborting.');
                 WriteLn(#32, SystemMsgPrefix, 'EleMGR is running ('+EleFMGRSemaPhore + ' exists), aborting');
               end; { else }
    end { if }
     else begin
            CreateSema(EleFileSemaphore);
            DeleteSema := true;

            if (NOT UpToDateFile(FilesFileName, RdxName(FilesFileName))) then
              begin
                WriteLn('Area index file ('+RdxName(FilesFileName)+') is not up to date!');
                {$IFNDEF DELPHI}
                  TextAttr := SaveAttr;
                {$ENDIF}
                Halt(255);
              end; { if }

            TempBool := false;
            if CommandOption='ADD' then TempBool := AddFiles;
            if CommandOption='INDEX' then TempBool := IndexFiles;
            if CommandOption='CLEAN' then TempBool := CleanFiles;
            if CommandOption='COMPRESS' then TempBool := CompressFiles;
            if CommandOption='EXPORT' then TempBool := ExportFiles;
            if CommandOption='IMPORT' then TempBool := ImportFiles;
            if CommandOption='KILL' then TempBool := MaintenanceFile('KILL');
            if CommandOption='LOCK' then TempBool := MaintenanceFile('LOCK');
            if CommandOption='UNLOCK' then TempBool := MaintenanceFile('UNLOCK');
            if CommandOption='FILELIST' then TempBool := FileListFiles;
            if CommandOption='SORT' then TempBool := SortFiles;
            if CommandOption='ADOPT' then TempBool := AdoptFiles;
            if CommandOption='UPDATE' then TempBool := MaintenanceFile('UPDATE');
            if CommandOption='REARC' then TempBool := RearchiveFiles;
            if CommandOption='DESCRIBE' then TempBool := ImportDizFiles;
            if CommandOption='HTMLIST' then TempBool := HtmlScriptFiles;

            if TempBool then
              begin
                AddStatusMsg('');
                AddStatusMsg(SystemMsgPrefix + 'Processing completed successfully');
                AddStatusMsg('');
              end; { if }
          end; { else }

  RestoreTitle;

  {$IFNDEF DELPHI}
  TextAttr := SaveAttr;
  {$ENDIF}
end. { EleFile }
