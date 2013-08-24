unit GenFile;
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
** General file routines for EleBBS
**
** Copyright (c) 1998 by Maarten Bekers
**
** Created : 01-May-1998
** Last update : 16-Aug-1999
**
** Note: Text Seeking procedures are (c) Arne de Bruijn
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses FileObj, CfgRec;

procedure Reset(var F: File; RecSize: Word);
procedure ReWrite(var F: File; RecSize: Word);

procedure MakeDir(Pathname: String);
procedure KillCompleteDir(PathName: String; ThisAlso: Boolean);
procedure EraseDir(S: String; ThisAlso: Boolean);       { Erase the complete dir (no sub dirs!) }
procedure SetFileTime(FName: String);         { Set the current time to file }
procedure RenameWithDrive(var F: File; NewName: String);
procedure TextSeek(Var f : Text; n : LongInt);

function  RenameObjWithDrive(var F: pFileObj; NewName: String): Boolean;
function  GetDiskFree(C:Char): Longint;    { Get the bytes free for disk '?' }
function  RenameFile(OldName, NewName: String): Boolean;     { Rename a file }
function  GetCurDir: String;                     { Get the current directory }
function  GetFileSize(Name: String; RecSize: SmallWord): Longint;
function  FileCount(S: String): Longint;    { Count number of files in a dir }
function  FileExist(FName: String): Boolean;         { Does this file exist? }
function  GetPackedFileTime(FName: String): Longint;
function  GetFileDate(S: String): String; { Returns filedate in MM-DD-YY form }
function  IsWildCard(WildCard, TestName: String): Boolean;
function  OpenFile(S: String; Mode: SmallWord): Boolean;
function  EraseFile(FName: String): Boolean;
function  TextFilePos(Var f : Text) : LongInt;
function  TextFileSize(Var f : Text) : LongInt;

function  GetVolumeLabel(Drive: String): String;
function  GetSysEnv: String;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Uses
     {$IFDEF WIN32}
       Windows,
     {$ENDIF}

     {$IFDEF WITH_DEBUG}
       Debug_U,
     {$ENDIF}

     SysUtils,
      Dos,
       GenDos,
        CentrStr,
         StrPath,
          Strings,
           Cases,
            LongStr,
             Global,
              ObjDec,
               Wildcards;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Rewrite(var F: File; RecSize: Word);
{$IFDEF WIN32}
var TempRec: File;
{$ENDIF}
begin
  {$IFNDEF WIN32}
   {$i-} System.Rewrite(F, RecSize); {$i+}
  {$ELSE}
    {$IFNDEF FPC}
      {$IFDEF DELPHI}
        If SysUtils.FileExists(System.TFileRec(F).Name) then
          SysUtils.DeleteFile(System.TFileRec(F).Name);
      {$ELSE}
        If SysUtils.FileExists(SysUtils.TFileRec(F).Name) then
          SysUtils.DeleteFile(SysUtils.TFileRec(F).Name);
      {$ENDIF}
    {$ELSE}
      If SysUtils.FileExists(StrPas(Dos.FileRec(F).Name)) then
        SysUtils.DeleteFile(StrPas(Dos.FileRec(F).Name));
    {$ENDIF}

   {$i-} System.Rewrite(F, RecSize);

    Move(F, TempRec, SizeOf(TempRec));
    Close(F);
    Move(TempRec, F, SizeOf(TempRec));

    {$IFNDEF FPC}
      {$IFDEF DELPHI}
        System.TFileRec(F).Handle := FileOpen(System.TFileRec(F).Name, FileMode);
      {$ELSE}
        Sysutils.TFileRec(F).Handle := FileOpen(SysUtils.TFileRec(F).Name, FileMode);
      {$ENDIF}
    {$ELSE}
      Dos.FileRec(F).Handle := FileOpen(StrPas(Dos.FileRec(F).Name), FileMode);
    {$ENDIF}

   {$i+}
  {$ENDIF}
end; { proc. ReWrite }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Reset(var F: File; RecSize: Word);
{$IFDEF WIN32}
var TempRec: File;
{$ENDIF}
begin
  {$IFNDEF WIN32}
   {$i-} System.Reset(F, RecSize); {$i+}
  {$ELSE}
   {$i-} System.Reset(F, RecSize);

    Move(F, TempRec, SizeOf(TempRec));
    Close(F);
    Move(TempRec, F, SizeOf(TempRec));

    {$IFNDEF FPC}
      {$IFDEF DELPHI}
        System.TFileRec(F).Handle := FileOpen(System.TFileRec(F).Name, FileMode);
      {$ELSE}
        Sysutils.TFileRec(F).Handle := FileOpen(SysUtils.TFileRec(F).Name, FileMode);
      {$ENDIF}
    {$ELSE}
      Dos.FileRec(F).Handle := FileOpen(StrPas(Dos.FileRec(F).Name), FileMode);
    {$ENDIF}
   {$i+}
  {$ENDIF}
end; { proc. Reset }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure KillCompleteDir(PathName: String; ThisAlso: Boolean);
var DirInfo : SearchRec;
    F       : pFileObj;
begin
  PathName := Trim(PathName);                              { Some precautions }
  PathName := ForceBack(PathName);        { Making a dir called \A not abort. }

  if PathName = '' then EXIT;
  if PathName = BsChar then EXIT;
  if (Length(PathName)=3) and (PathName[2] = ':') then EXIT;

  FindFirst(PathName + '*.*', AnyFile - VolumeID, DirInfo);

  While DOSError=00 do
    begin
      if (DirInfo.Name <> '..') AND (DirInfo.Name <> '.') then
        begin
          if Dirinfo.Attr AND Directory = 0 then
             begin
               New(F, Init);
               F^.Assign(PathName + Dirinfo.Name);
               F^.FileMode := ReadWriteMode + DenyNone;
               F^.SetAttr(0);
               F^.Erase;
               Dispose(F, Done);
             end { if }
               else KillCompleteDir(PathName + DirInfo.Name, True);
        end; { if }

      FindNext(DirInfo);
    end; { while }

  FindClose(Dirinfo);

  {$i-}
    if ThisAlso then RmDir(NoBackSlash(PathName));
  {$i+}
  if IOResult>00 then ;
end; { proc. KillCompleteDir }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


procedure EraseDir(S: String; ThisAlso: Boolean);
var DirInfo   : SearchRec;
    F         : pFileObj;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileRout, 'EraseDir('+S+') (begin)');
  {$ENDIF}

  FindFirst(S, AnyFile - VolumeID, Dirinfo);
  S := JustPath(s);

  While (DosError=0) do
    begin
       If Dirinfo.Attr AND Directory = 0 then
        begin
          New(F, Init);
          F^.Assign(S + Dirinfo.Name);
          F^.FileMode := ReadWriteMode + DenyNone;
          F^.Erase;
          Dispose(F, Done);

          {$IFDEF WITH_DEBUG}
            DebugObj.DebugLog(logFileRout, 'EraseDir('+S+'), deleting: '+DirInfo.Name);
          {$ENDIF}
        end; { if }

      FindNext(DirInfo);
    end; { While DosError=0 }

  FindClose(DirInfo);

  if ThisAlso then
    begin
      {$i-} RmDir(NoBackSlash(s)); {$i+}
      if IOResult>00 then ;
    end; { if }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileRout, 'EraseDir('+S+') ( end )');
  {$ENDIF}
end; { proc. EraseDir }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetDiskFree(C:Char): Longint;
var TempBool: Boolean;
    TmpLong : Longint;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileRout, 'Gettting FreeDiskSpace from letter ['+C+']');
  {$ENDIF}

  TempBool := UpCase(C) in ['A'..'Z'];

  If (NOT TempBool) then TmpLong := Longint(DiskFree(0))
    else TmpLong := Longint(DiskFree(Ord(UpCase(C))-64));

  if TmpLong < 0 then TmpLong := TmpLong - (TmpLong * 2);
  GetDiskFree := TmpLong;
end; { func. GetDiskFree }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure RenameWithDrive(var F: File; NewName: String);
var OldName: String;
    Temp_F : pFileObj;

function CopyThisFile(Orig, Dest: String; KeepDates: Boolean): Boolean;
var Orig_F,
    Dest_F  : pFileObj;
    Buffer  : Array[0..1023] of Byte;
    FTime   : Longint;
    NumRead,
    NumWrite: NumReadType;
begin
  CopyThisFile := false;

  New(Orig_F, Init);
  Orig_F^.Assign(Orig);
  Orig_F^.FileMode := ReadMode + DenyNone;
  if NOT Orig_F^.Open(1) then
    begin
      Dispose(Orig_F, Done);
      EXIT;
    end; { if }

  New(Dest_F, Init);
  Dest_F^.Assign(Dest);
  Dest_F^.FileMode := WriteMode + DenyNone;
  if NOT Dest_F^.Create(1) then
    begin
      Dispose(Orig_F, Done);
      Dispose(Dest_F, Done);
      EXIT;
    end; { if }

  NumRead := 00;
  NumWrite := 00;

  While (NOT Orig_F^.EOF) AND (NumRead=NumWrite) do
    begin
      NumRead := Orig_F^.BlkRead(Buffer, SizeOf(Buffer));
      NumWrite := Dest_F^.BlkWrite(Buffer, NumRead);
    end; { while }

  If KeepDates then
    begin
      Dest_F^.SetFileTime(Orig_F^.GetFileTime);
    end; { Set filestamps }

  Dispose(Orig_F, Done);
  Dispose(Dest_F, Done);

  CopyThisFile := true;
end; { func. CopyThisFile }

begin
  OldName := StrPas(FileRec(F).Name);

  OldName := FExpand(OldName);
  NewName := FExpand(NewName);

  if SUpCase(Copy(OldName, 1, 2)) <> SUpCase(Copy(NewName, 1, 2)) then
    begin
      if NOT FileExist(NewName) then
        begin

          if CopyThisFile(OldName, NewName, True) then
            begin
              Erase(File(F));
            end; { if successfully copied }
        end; { if }
    end { if }
     else begin
            New(Temp_F, Init);

            Temp_F^.Assign(OldName);
            Temp_F^.FileMode := ReadWriteMode + DenyNone;
            Temp_F^.Rename(NewName);

            Dispose(TemP_F, Done);
          end; { else }

  NewName := NewName + #00;
  Move(NewName[1], FileRec(F).Name, Length(NewName));
end; { proc. RenameWithDrive }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function RenameObjWithDrive(var F: pFileObj; NewName: String): Boolean;
var OldName: String;
    Temp_F : pFileObj;

function CopyThisFile(Orig, Dest: String; KeepDates: Boolean): Boolean;
var Orig_F,
    Dest_F  : pFileObj;
    Buffer  : Array[0..1023] of Byte;
    FTime   : Longint;
    NumRead,
    NumWrite: NumReadType;
begin
  CopyThisFile := false;

  New(Orig_F, Init);
  Orig_F^.Assign(Orig);
  Orig_F^.FileMode := ReadMode + DenyNone;
  if NOT Orig_F^.Open(1) then
    begin
      Dispose(Orig_F, Done);
      EXIT;
    end; { if }

  New(Dest_F, Init);
  Dest_F^.Assign(Dest);
  Dest_F^.FileMode := WriteMode + DenyNone;
  if NOT Dest_F^.Create(1) then
    begin
      Dispose(Orig_F, Done);
      Dispose(Dest_F, Done);
      EXIT;
    end; { if }

  NumRead := 00;
  NumWrite := 00;

  While (NOT Orig_F^.EOF) AND (NumRead=NumWrite) do
    begin
      NumRead := Orig_F^.BlkRead(Buffer, SizeOf(Buffer));
      NumWrite := Dest_F^.BlkWrite(Buffer, NumRead);
    end; { while }

  If KeepDates then
    begin
      Dest_F^.SetFileTime(Orig_F^.GetFileTime);
    end; { Set filestamps }

  Dispose(Orig_F, Done);
  Dispose(Dest_F, Done);

  CopyThisFile := true;
end; { func. CopyThisFile }

var DidWork: Boolean;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileIO, 'RenameObjWithDrive: (BEGIN): '+NewName);
  {$ENDIF}

  RenameObjWithDrive := true;
  DidWork := true;

  if F = nil then EXIT;

  OldName := F^.FileName;

  OldName := FExpand(OldName);
  NewName := FExpand(NewName);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileIO, 'RenameObjWithDrive: (BEGIN): '+OldName + ' / ' + NewName);
  {$ENDIF}

  if SUpCase(Copy(OldName, 1, 2)) <> SUpCase(Copy(NewName, 1, 2)) then
    begin
      {$IFDEF WITH_DEBUG}
        DebugObj.DebugLog(logFileIO, 'RenameObjWithDrive: Difficult copy');
      {$ENDIF}

      if NOT FileExist(NewName) then
        begin
          {$IFDEF WITH_DEBUG}
            DebugObj.DebugLog(logFileIO, 'RenameObjWithDrive: Difficult: It does NOT exist!');
          {$ENDIF}

          if CopyThisFile(OldName, NewName, True) then
            begin
              F^.Erase;
            end { if successfully copied }
              else begin
                     DidWork := false;
                     {$IFDEF WITH_DEBUG}
                       DebugObj.DebugLog(logFileIO, 'RenameObjWithDrive: Difficult: Copy failed');
                     {$ENDIF}
                   end; { else }


        end { if }
          else DidWork := false;

    end { if }
     else begin
            {$IFDEF WITH_DEBUG}
              DebugObj.DebugLog(logFileIO, 'RenameObjWithDrive: Easy copy');
            {$ENDIF}

            New(Temp_F, Init);

            Temp_F^.Assign(OldName);
            Temp_F^.FileMode := ReadWriteMode + DenyNone;
            DidWork := Temp_F^.Rename(NewName);

            {$IFDEF WITH_DEBUG}
              DebugObj.DebugLog(logFileIO, 'RenameObjWithDrive: Result = ' + FStr(Temp_F^.IoResult));
            {$ENDIF}

            Dispose(Temp_F, Done);
          end; { else }

  if NOT DidWork then NewName := OldName;

  F^.Close;
  F^.Assign(NewName);

  RenameObjWithDrive := DidWork;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileIO, 'RenameObjWithDrive: DidWork = '+fStr(Byte(DidWork)));
    DebugObj.DebugLog(logFileIO, 'RenameObjWithDrive: ( END )');
  {$ENDIF}
end; { proc. RenameObjWithDrive }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function RenameFile(OldName, NewName: String): Boolean;
var Orig_F    : pFileObj;
    TempError : SmallWord;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileIO, 'RenameFile: Old/New: '+OldName+' / '+NewName);
  {$ENDIF}

  New(Orig_F, Init);
  Orig_F^.Assign(OldName);
  if RenameObjWithDrive(Orig_F, NewName) then TempError := 0
    else TempError := 1;
  Dispose(Orig_F, Done);

  RenameFile := true;
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileio, 'TempError (Rename): '+Fstr(TempError));
  {$ENDIF}
end; { proc. RenameFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  FileCount(S: String): Longint;
var NrFiles : Longint;
    DirInfo : SearchRec;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileRout, 'FileCount: ('+S+') (begin)');
  {$ENDIF}
  NrFiles := 00;
  S := ForceBack(S);

  FindFirst(S + '*.*', AnyFile - VolumeID, DirInfo);

  While (DosError=0) do
    begin
      if Dirinfo.Attr AND Directory = 0 then
        Inc(nrFiles);

      FindNext(Dirinfo);
    end; { While DosError=0 }

  FindClose(DirInfo);

  FileCount := NrFiles;
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileRout, 'FileCount: ('+S+') ( end )');
  {$ENDIF}
end; { func. FileCount }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetFileSize(Name: String; RecSize: SmallWord): Longint;
var Temp_F    : pFileObj;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileIo, 'GetFileSize: ('+ Name + ') - (begin)');
  {$ENDIF}

  New(Temp_F, Init);

  Temp_F^.Assign(Name);
  Temp_F^.FileMode := ReadMode + DenyNone;
  if Temp_F^.Open(RecSize) then
    GetFileSize := Temp_F^.FileSize
     else GetFileSize := 0;

  Dispose(Temp_F, Done);

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileIo, 'GetFileSize: ('+ Name + ') - ( end )');
  {$ENDIF}
end; { func. GetFileSize }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function  GetCurDir: String;
var Temp: String;
begin
  Getdir(0, Temp);
  GetCurDir := Temp;
end; { func. GetCurDir }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function FileExist(FName: String): Boolean;
var SR         : SearchRec;
    WinTmpError: SmallWord;
    F          : File;
    Attr       : Word;
begin
  if IOResult>00 then ;

  if FName[Length(Fname)]=BsChar then
    begin
      {$IFNDEF WIN32}
        if (Length(FName)=1) or (Length(FName)=3) then FName := FName + BsChar;

        DosError := 0;
        Assign(F, Copy(FName, 1, Length(FName) - 01));
        GetFAttr(F, Attr);
        FileExist := (DosError = 00);

(* Old routine did not work (does not) with CD-ROM's (r/o devices)
        Assign(F, FName + 'TEMPDIR$.$$$');
        FileMode := 00;
        {$i-}
          System.ReWrite(F);
          Close(f);
        {$i+}
        FileExist := (IOResult=00);

        {$i-}
          Erase(F);           { If you don't have delete-access to the drive! }
        {$i+}
        if IOResult>00 then ;
*)
      {$ENDIF}

      {$IFDEF WIN32}
        FileExist := SysUtils.FileGetAttr(FName) <> -1;
      {$ENDIF}

      EXIT;
    end; { if }

  FindFirst(FName, AnyFile - VolumeID, SR);
  WinTmpError := DosError;
  FindClose(Sr);

  FileExist := (WinTmpError = 00);
  If IoResult <> 0 Then;
end; { func. FileExist }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure MakeDir(PathName: String);
begin
  {$i-}
    if IoResult > 0 then ;
    MkDir(ForceBack(PathName));
  {$i+}
  if IOResult > 00 then ;
end; { proc. MakeDir }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  GetPackedFileTime(FName: String): Longint;
var F   : pFileObj;
    Time: LongInt;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileIo, 'GetPackedFileTime (begin)');
  {$ENDIF}

  Time := 2201600;                                              { 01 Jan 1980 }
  GetPackedFileTime := Time;

  New(F, Init);

  F^.Assign(FName);
  F^.FileMode := ReadMode + DenyNone;
  if F^.Open(1) then Time := F^.GetFileTime;

  Dispose(f, Done);

  GetPackedFiletime := Time;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileIo, 'GetPackedFileTime ( end )');
  {$ENDIF}
end; { func. GetPackedFileTime }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure SetFileTime(FName: String);
var F   : pFileObj;
    Time: LongInt;
    DOW : Word;
    DT  : DateTime;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileIo, 'SetFileTime (begin)');
  {$ENDIF}

  FillChar(Dt, SizeOf(Dt), 0);
  GetDate(DT.Year, Dt.Month, DT.Day, Dow);
  GetTime(DT.Hour, DT.Min, DT.Sec, Dow);
  PackTime(DT, Time);

  New(F, Init);
  F^.Assign(FName);
  F^.FileMode := ReadMode + DenyNone;
  if F^.Open(1) then
    begin
      F^.SetFileTime(Time);
    end; { if }

  Dispose(F, Done);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileIo, 'SetFileTime ( end )');
  {$ENDIF}
end; { proc. SetFileTime }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)


function GetFileDate(S: String): String; { Returns filedate in MM-DD-YY form }
var Time   : LongInt;
    DT     : DateTime;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileIo, 'Getfiledate ('+S+') - (begin)');
  {$ENDIF}

  GetFileDate := '01-01-80';
  if NOT FileExist(S) then Exit;

  Time := GetPackedFileTime(s);
  UnpackTime(Time, DT);

  {$IFNDEF WIN32}
   With DT do
     GetFileDate := LeadingZero(DT.Month, 2) + '-' + LeadingZero(DT.Day, 2) + '-' +
                    Copy(LeadingZero(Dt.Year, 2), 3, 2);
  {$ELSE}
     GetFileDate := FormatDateTime('mm"-"dd"-"yy', FileDateToDateTime(FileAge(S)));
  {$ENDIF}

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileIo, 'Getfiledate ('+S+') - ( end )');
  {$ENDIF}
end; { func. GetFileDate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF USEOLDWILDCARD}
Function IsWildCard(WildCard, TestName: String): Boolean;
var KeyPos,
    LinPos  : Integer;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileRout, 'IsWildCard: '+WildCard+' / '+TestName);
  {$ENDIF}

  WildCard := SUpCase(Trim(WildCard));
  TestName := SUpCase(Trim(TestName));

  IsWildCard := True;

  If (WildCard='*.*') OR (WildCard='*') then EXIT;
  if (WildCard=TestName) then EXIT;

  if (Pos('*', WildCard)=00) AND (Pos('?', WildCard)=00) then
    begin
      IsWildCard := (WildCard = TestName);
      EXIT;
    end; { if }

  if WildCard[Length(WildCard)] = '*' then
   begin
     if WildCard[Length(WildCard) - 1] <> '.' then
       WildCard := WildCard + '.*';
   end; { if }

  KeyPos := 01;
  LinPos := 01;

  While True do
   begin
      If KeyPos> Length(WildCard) then { End Of WildCard? Yes? We might have a match }
        begin
         IsWildCard := (LinPos >= Length(TestName)) OR (TestName[LinPos] = ' ');
         EXIT;
        end { If End }
         else If LinPos>Length(TestName) then
          begin
            IsWildCard := False;
            exit;
          end { no wildcard }
         else If (UpCase(WildCard[Keypos]) = Upcase(TestName[Linpos])) OR
                 (WildCard[KeyPos] = '?') then
                  begin
                    Inc(Keypos);
                    Inc(linpos);
                  end
         else If WildCard[KeyPos] = '*' then
                  begin
                    if KeyPos = Length(WildCard) then
                      begin
                        EXIT;
                      end; { if }

                    While (TestName[Linpos]<>'.')
                         AND (LInPos < Length(TestName)) do Inc(LinPos);

                   Inc(Keypos);
                  end
         else begin;
                IsWildCard := False;
                Exit;
              end; { else }
   end; { While }
end; { func. IsWildCard }
{$ENDIF}

{$IFNDEF USEOLDWILDCARD}
function IsWildCard(WildCard, TestName: String): Boolean;
var TmpBool: Boolean;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileRout, 'IsWildCard (begin): '+WildCard+' / '+TestName);
  {$ENDIF}
  
  TmpBool := Wild(TestName, WildCard, True); // true == make upercase
  IsWildCard := TmpBool;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileRout, 'IsWildCard (end): '+WildCard+' / '+TestName + ' -> ' + FStr(Byte(TmpBool)));
  {$ENDIF}  
end; { func. IsWildCard }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  OpenFile(S: String; Mode: SmallWord): Boolean;
var F: pFileObj;
begin
  New(F, Init);
  F^.Assign(S);
  F^.FileMode := Mode;

  OpenFile := (F^.Open(1));

  Dispose(f, Done);
end; { func. OpenFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  EraseFile(FName: String): Boolean;
var F: pFileObj;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileRout, 'EraseFile (begin) ('+FName+')');
  {$ENDIF}

  New(F, Init);
  F^.Assign(FName);
  F^.FileMode := ReadWriteMode + Denynone;
  EraseFile := F^.Erase;
  Dispose(f, Done);

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileRout, 'EraseFile ( end ) ('+FName+')');
  {$ENDIF}
end; { func. EraseFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

Function  TextFilePos(Var f : Text) : LongInt;        { (c) by Arne de Bruijn }
var SaveMode,
    SaveRecSize : Word;
    Temp_F      : File ABSOLUTE F;
begin
 SaveMode := FileRec((@F)^).Mode;
 SaveRecSize := FileRec((@F)^).RecSize;

 FileRec((@F)^).Mode:=fmInOut;                           { Set to binary mode }
        { (The (@F)^ part is to let TP 'forget' the type of the structure, so }
      { you can type-caste it to everything (note that with and without (@X)^ }
       { can give a different value, longint(bytevar) gives the same value as }
                      { bytevar, while longint((@bytevar)^) gives the same as }
         { longint absolute Bytevar (i.e. all 4 bytes in a longint are readed }
                              { from memory instead of 3 filled with zeros))) }

 FileRec((@F)^).RecSize := 01;                { Set record size to 1 (a byte) }

 TextFilePos := (FilePos(File((@F)^)) -
                 TextRec(F).BufEnd) +
                 TextRec(F).BufPos;
      { Get the fileposition, subtract the already readed buffer, and add the }
                                                    { position in that buffer }

 TextRec(F).Mode := SaveMode;                         { Set back to text mode }
 TextRec(F).BufSize:= SaveRecSize;           { BufSize overwritten by RecSize }
end; { func. TextFilePos }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  TextFileSize(Var f : Text) : LongInt;
var SaveMode,
    SaveRecSize : Word;
    Temp_F      : File ABSOLUTE F;
begin
 SaveMode := FileRec((@F)^).Mode;
 SaveRecSize := FileRec((@F)^).RecSize;

 FileRec((@F)^).Mode:=fmInOut;                           { Set to binary mode }
        { (The (@F)^ part is to let TP 'forget' the type of the structure, so }
      { you can type-caste it to everything (note that with and without (@X)^ }
       { can give a different value, longint(bytevar) gives the same value as }
                      { bytevar, while longint((@bytevar)^) gives the same as }
         { longint absolute Bytevar (i.e. all 4 bytes in a longint are readed }
                              { from memory instead of 3 filled with zeros))) }

 FileRec((@F)^).RecSize := 01;                { Set record size to 1 (a byte) }

 TextFileSize := FileSize(File((@F)^));

 TextRec(F).Mode := SaveMode;                         { Set back to text mode }
 TextRec(F).BufSize:= SaveRecSize;           { BufSize overwritten by RecSize }
end; { func. TextFileSize }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TextSeek(Var f : Text; n : LongInt);
var SaveMode,
    SaveRecSize : Word;
    Temp_F      : File ABSOLUTE F;
begin
 SaveMode := FileRec((@F)^).Mode;
 SaveRecSize := FileRec((@F)^).RecSize;

 FileRec((@F)^).Mode:=fmInOut;                           { Set to binary mode }
        { (The (@F)^ part is to let TP 'forget' the type of the structure, so }
      { you can type-caste it to everything (note that with and without (@X)^ }
       { can give a different value, longint(bytevar) gives the same value as }
                      { bytevar, while longint((@bytevar)^) gives the same as }
         { longint absolute Bytevar (i.e. all 4 bytes in a longint are readed }
                              { from memory instead of 3 filled with zeros))) }

  FileRec((@F)^).RecSize := 01;               { Set record size to 1 (a byte) }
  {$i-}
    Seek(File((@F)^), N);
  {$i-}

 TextRec(F).Mode := SaveMode;                         { Set back to text mode }
 TextRec(F).BufSize:= SaveRecSize;           { BufSize overwritten by RecSize }
 TextRec(F).BufEnd := TextRec(F).BufPos;
end; { func. TextSeek }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  GetSysEnv: String;
var TempStr: String;
begin
   if OverrideSysPath = '' then
     begin
       TempStr := GetEnv('ELEBBS');
       if TempStr = '' then TempStr := GetEnv('RA');
       if TempStr = '' then TempStr := GetEnv('ELE');
       if TempStr = '' then TempStr := GetEnv('elebbs'); { Lnx - lowercase }
       if TempStr = '' then TempStr := GetEnv('ele');    { Lnx - lowercase }
     end
       else TempStr := OverrideSysPath;

   GetSysEnv := TempStr;
end; { func. GetSysEnv }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


function GetVolumeLabel(Drive: String): String;
{$IFDEF WIN32}
 const Root     : Array[0..4] of char = 'C:\'#0;
 var   VolLabel : Array[0..256] of char;
       MaxLength: DWORD;
       FSFlags  : DWORD;
{$ENDIF}

{$IFDEF MSDOS}
var Dirinfo: SearchRec;
{$ENDIF}

{$IFDEF GO32V2}
var DirInfo: Searchrec;
{$ENDIF}
begin
  GetVolumeLabel := '[unk]';

  {$IFDEF WIN32}
    Root[0] := Drive[1];
    {$IFDEF VER0_99_15}
      if GetVolumeInformation(Root, VolLabel, Sizeof(VolLabel),
                              NIL, @MaxLength, @FSFlags, NIL, 0) then
    {$ELSE}
      if GetVolumeInformation(Root, VolLabel, Sizeof(VolLabel),
                              NIL, MaxLength, FSFlags, NIL, 0) then
    {$ENDIF}


      Result := StrPas(VolLabel) else Result := '';
  {$ENDIF}

  {$IFDEF MSDOS}
    FindFirst('*.*', VolumeID, DirInfo);

    if Pos(DirInfo.Name, '.') > 00 then
      DirInfo.Name := Copy(Dirinfo.Name, 1, Pos('.', Dirinfo.Name));

    if DosError = 0 then
      GetVolumeLabel := Dirinfo.Name
       else GetVolumeLabel := '';

    FindClose(Dirinfo);
  {$ENDIF}

  {$IFDEF Go32V2}
    FindFirst('*.*', VolumeID, DirInfo);

    if Pos(DirInfo.Name, '.') > 00 then
      DirInfo.Name := Copy(Dirinfo.Name, 1, Pos('.', Dirinfo.Name));

    if DosError = 0 then
      GetVolumeLabel := Dirinfo.Name
       else GetVolumeLabel := '';

    FindClose(Dirinfo);
  {$ENDIF}
end; { func. GetVolumeLabel }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
