unit DispAns;
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
** ANSI/AVATAR/RIP displaying routines for EleBBS
**
** Copyright (c) 1996, 1997 by Maarten Bekers
**
** Created : 15-Nov-1997
** Last update : 15-Nov-1997
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF WITH_FULL}
  This unit shouldn''t be compiled for an utility program
{$ENDIF}

uses CfgRec;

function  OpenTextFile(S: String): String;
function  DisplayHotFile(FileName: String; const HotKeys: CharSet):Char;
procedure DisplayWithCr(FName:String);

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses {$IFDEF WITH_DEBUG}
       Debug_U,
     {$ENDIF}

     {$IFDEF WINGUI}
       SysUtils,
       Win_Main,
       Dialogs,
     {$ELSE}
       ScrnU,
     {$ENDIF}

     {$IFDEF VirtualPascal}
       Dos,
     {$ENDIF}

     {$IFDEF MSDOS}
       Dos,
     {$ENDIF}

     {$IFDEF FPC}
       Dos,
     {$ENDIF}

     ApTimer,
     Global,

      {$IFDEF WITH_FULL}
        FastScrn,
        Avatar,
        Input_U,
        InOut_U,
        ComUnit,
      {$ENDIF}

      ApFossil, WordStr, Cases, StrPath, Control, CentrStr,  Outblock,
      GenDos, FileObj, ObjDec, ellog_u, LongStr;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function DisplayHotFile(FileName: String; const HotKeys: CharSet): Char;
var AFile     : pFileObj;
    TempChar  : Char;
    Buffer    : String[100];
    Counter   : Word;
    NumRead   : NumReadType;
    IChr      : Char;
    MakeStr   : String;
    OldFMode  : Longint;
    Name      : String;
    OldCaption: String;
    TextName  : String;
    SaveDisp  : Boolean;
    SaveRadu  : Boolean;
    SaveBeep  : Boolean;
    SaveContr : Boolean;
    SaveDoBinary: Boolean;
    SaveDirect: Boolean;
    SaveScrn  : Pointer;

    Temp      : Longint;
begin
{$IFDEF WITH_FULL}
  DisplayHotFile := #00;
  If FileName='' then EXIT;
  Name           := FirstWord(FileName, defExtractWord, true);
  TextName       := OpenTextFile(Name);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logInOut, 'DisplayHotFile = ' + TextName + ' / ' + Name);
  {$ENDIF}

  SaveDisp       := LineCfg^.DispMorePrompt;
  SaveBeep       := AvtObj^.AnsiDoBeep;
  SaveDoBinary   := AvtObj^.DoingBinary;
  SaveContr      := LineCfg^.ContrCodes;
  {$IFNDEF WINGUI}
    SaveDirect     := DirectScrnUpdate;
  {$ENDIF}
  OutputObj^.ResetLines(01);

  If LineCfg^.RaduCodes then
    Write('`F', GlobalCfg^.RaConfig^.NormFore, ':`B', GlobalCfg^.RaConfig^.NormBack, ':');

  FileName := Trim(FileName);
  If SUpcase(FileName)='/BINARY' then
     begin
       LineCfg^.ContrCodes := False;
       AvtObj^.AnsiDoBeep := false;
       AvtObj^.DoingBinary := TRUE;
     end; { if }

 If NOT LineCfg^.Snooping then
    begin
      SaveScreen(SaveScrn);
      BoxWindow(23, 10, 57, 14, GlobalCfg^.RaConfig^.BorderFore, GlobalCfg^.RaConfig^.BorderBack, '');
      FastWrite(24, 12, (GlobalCfg^.RaConfig^.WindBack SHL 4) OR GlobalCfg^.RaConfig^.WindFore,
                CenterJust('Displaying '+SUpCase(Name), 30, False));
      {$IFDEF WINGUI}
        OldCaption := Form1.Panel3.Caption;
        Form1.Panel3.Caption := 'Currently showing (AVT/ANS/ASC): '+SUpcase(Name);
      {$ENDIF}
    end; { NOT Snooping }

  New(aFile, Init);
  aFile^.Assign(TextName);
  aFile^.FileMode := ReadMode + DenyNone;
  if aFile^.Open(1) then
   begin
     WriteLn;

     SaveRadu := LineCfg^.RaduCodes;
     LineCfg^.RaduCodes := false;

     While (NOT aFile^.EOF) AND (NOT ProgTerminated) AND (NOT OutputObj^.StopMore) do
              begin
                NumRead := aFile^.BlkRead(Buffer[1], SizeOf(Buffer) - 01);

                Buffer[0] := Chr(NumRead);
                OutputObj^.DisplayStr(Buffer, LineCfg^.PauseChecking, Hotkeys);
              end; { while }

     LineCfg^.RaduCodes := SaveRadu;

     Dispose(aFile, Done);
     aFile := nil;

     if ComObj <> nil then
      if OutputObj^.StopMore then
        begin
          ComObj^.Com_PurgeOutBuffer;

          OutBlockObj^.DumpBlock;
          OutBlockObj^.ClearBlock;
        end; { if }

      If OutputObj^.StopMore then
        DisplayHotFile := LineCfg^.DispAbortChar
          else DisplayHotFile := #00;
      if (AvtObj^.InAvatar) OR (lineCfg^.ProcessCharType<>chrDisplay) then
        WriteLn;
   end else
    begin
      DisplayHotFile := #01;                                 { File not found }

      Dispose(aFile, Done);
      aFile := nil;
    end; { if }

  If NOT LineCfg^.Snooping then
    begin
       {$IFDEF WINGUI}
          Form1.Panel3.Caption := OldCaption;
       {$ENDIF}
       RestoreScreen(SaveScrn);
    end; { if NOT snooping }

  OutputObj^.SetStopMore(False);

  LineCfg^.DispMorePrompt:= SaveDisp;
  LineCfg^.ContrCodes    := SaveContr;
  AvtObj^.AnsiDoBeep     := SaveBeep;
  AvtObj^.DoingBinary    := SaveDoBinary;
{$IFNDEF WINGUI}
  DirectScrnUpdate:= SaveDirect;
{$ENDIF}
{$ENDIF}
end; { proc. DisplayHotFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function OpenTextFile(S: String): String;

function DoesExist(Path: String; var Name: String): Boolean;
var DirInfo : SearchRec;
    TempRec : Array[1..6] of String;  { 1=RI2, 2=RIP, 3=AVT, 4=ANS, 5=ASC, 6=. }
    Ext     : String[4];
begin
  DoesExist := false;
  FillChar(TempRec, SizeOf(TempRec), #00);

  FindFirst(Path + Name + '.*', AnyFile - VolumeID, Dirinfo);
  While DosError=00 do
    begin
      Ext := SUpCase(JustExtension(Dirinfo.Name));

      {$IFDEF WITH_DEBUG}
        DebugObj.DebugLog(logInout, 'DoesExist('+Path+Name+'.*'+') = ' + DirInfo.Name + ' / ' + Ext);
      {$ENDIF}

      if Ext='RI2' then TempRec[01] := DirInfo.Name else
       if Ext='RIP' then TempRec[02] := Dirinfo.Name else
        if Ext='AVT' then TempRec[03] := Dirinfo.Name else
         if Ext='ANS' then TempRec[04] := Dirinfo.Name else
          if Ext='ASC' then TempRec[05] := Dirinfo.Name else
           if Ext='' then TempRec[06] := Dirinfo.Name;

      FindNext(Dirinfo);
    end; { while }

  FindClose(DirInfo);

  if lineCfg^.RipOn then
   If TempRec[01] <> '' then
      begin
        Name := Path + Name + '.ri2';
        DoesExist := True;
        EXIT;
      end; { if }

  if lineCfg^.RipOn then
   If TempRec[02] <> '' then
      begin
        Name := Path + Name + '.rip';
        DoesExist := True;
        EXIT;
      end; { if }

  if lineCfg^.AvatarOn then
   If TempRec[03] <> '' then
      begin
        Name := Path + Name + '.avt';
        DoesExist := true;
        EXIT;
      end; { if }

  {$IFDEF WITH_DEBUG}
    if lineCfg^.AnsiOn then DebugObj.DebugLog(logInOut, 'DoesExist - AnsiOn')
      else DebugObj.DebugLog(logInOut, 'DoexExist - AnsiOff');
  {$ENDIF}

 if lineCfg^.AnsiOn then
   If TempRec[04] <> '' then
      begin
        Name := Path + Name + '.ans';
        DoesExist := true;
        EXIT;
      end; { if }

   If TempRec[05] <> '' then
    begin;
      Name := Path + Name + '.asc';
      DoesExist := true;
      EXIT;
    end; { if }

  {$IFNDEF ELEUNIX}
   If TempRec[06] <> '' then
    begin
      Name := Path + Name + '.';
      DoesExist := true;
      EXIT;
    end; { if }
  {$ENDIF}

  DoesExist := false;
end; { func. DoesExist }

var Temppath: String;
    TmpStr  : String;
begin
 s := Slowcase(s);
 OpenTextFile := s;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logInOut, 'OpenTextFile() = ' + S);
    DebugObj.DebugLog(logInOut, 'OpenTextFile() TextPath = ' + LineCfg^.Language^.TextPath);
  {$ENDIF}

 If (Pos('\', S)>00) OR (Pos('/', S)>00) OR (Pos('.', S)>00) then
          begin
            TempPath := JustPath(s);
            TmpStr := NoExtension(Justname(s));

            if DoesExist(TempPath, TmpStr) then
              S := TmpStr;

            OpenTextFile := S;
            Exit;
          end; { No extra path }

 if DoesExist(LineCfg^.Language^.TextPath, S) then
      begin
        OpenTextfile := S;
        exit;
      end; { Language.TextFiles }

 if DoesExist(GlobalCfg^.RaConfig^.TextPath, S) then
      begin
        OpenTextfile := S;
        exit;
      end; { RaConfig.TextFiles }

 if DoesExist('', S) then
      begin
        OpenTextfile := S;
        exit;
      end; { RaConfig.TextFiles }

                 {12345678}
 OpenTextFile := 'File not';
end; { func. OpenTextFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DisplayWithCr(FName:String);
begin
  {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logSupport, 'DisplayWithCr: '+FName);
  {$ENDIF}

  {$IFDEF WITH_FULL}
    OutputObj^.ResetLines(01);
    DisplayHotFile(Fname, []);
    InputObj^.PressEnter(True, True);
  {$ENDIF}
end; { proc. DisplayWithCr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)


end.
