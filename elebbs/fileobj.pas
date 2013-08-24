unit FileOBJ;
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
{$IFDEF FPC} {$R-} {$ENDIF}
(*
**
** Fileroutines for EleBBS
**
** Copyright (c) 1996-1999 by Maarten Bekers
**
** Created : 08-Apr-1999
** Last update : 08-Apr-1999
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)


{$IFDEF MSDOS}
 {$DEFINE DOSMODE}
{$ENDIF}

{$IFDEF GO32V2}
 {$DEFINE DOSMODE}
{$ENDIF}

{$IFDEF ELEUNIX}
  {$DEFINE DOSMODE}
{$ENDIF}


{$IFDEF DOSMODE}
uses Dos;
  {$IFNDEF FPC}
    type ShortString = String;
  {$ENDIF}
  type
       LongType    = Integer;
{$ENDIF}

{$IFDEF WIN32}
 uses Windows, SysUtils;

 type LongType    = Longint;
{$ENDIF}

{$IFDEF OS2}
  uses SysUtils, Dos, Os2Base;

  type LongType    = Longint;
{$ENDIF}


{$IFNDEF DOSMODE}
 {$IFNDEF WIN32}
  {$IFNDEF OS2}
  {$ENDIF}
 {$ENDIF}
{$ENDIF}


type tFileObj = Object
                  {$IFDEF DOSMODE}
                    FileVar   : File;
                  {$ENDIF}
                  {$IFDEF WIN32}
                   {$IFNDEF DOSMODE}
                    FileVar   : Integer;
                   {$ENDIF}
                  {$ENDIF}
                  {$IFDEF OS2}
                    FileVar   : Longint;
                  {$ENDIF}
                  FileHandle: Longint;
                  FileMode  : Longint;
                  FileName  : ShortString;
                  ZeroName  : ShortString;
                  RecordSize: Longint;
                  Opened    : Boolean;
                  ErrorCode : Longint;

                  constructor Init;
                  destructor Done;

                  procedure Assign(FName: String);
                  function  Create(RecSize: Longint): Boolean;
                  function  Open(RecSize: Longint): Boolean;
                  function  OpenOrCreate(RecSize: Longint): Boolean;
                  function  Close: Boolean;
                  function  Erase: Boolean;
                  procedure Seek(FPos: Longint);
                  function  FilePos: Longint;
                  function  FileSize: Longint;
                  function  EOF: Boolean;
                  function  IoResult: Longint;
                  function  Exists: Boolean;
                  procedure SetError(Error: Longint);
                  procedure FlushFile;

                  function  BlkRead(var Buf; BytesToRead: Longint): Longint;
                  function  BlkWrite(var Buf; BytesToWrite: Longint): Longint;

                  procedure WriteLn(S: ShortString);
                  procedure Write(S: ShortString);
                  procedure ReadLn(var S: ShortString);
                  procedure Read(var S: ShortString);
                  procedure SetFiletime(L: Longint);
                  function  GetFileTime: Longint;

                  {$IFNDEF MSDOS}
                    procedure AnsiWriteLn(S: AnsiString);
                    procedure AnsiWrite(S: AnsiString);
                    procedure AnsiReadLn(var S: AnsiString);
                    procedure AnsiRead(var S: AnsiString);
                  {$ENDIF}

                  function  Rename(NewName: String): Boolean;
                  procedure SetAttr(Attr: Longint);
                end; { tFileObj }

type pFileObj = ^tFileObj;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

constructor tFileObj.Init;
begin
  FileName := '';
  Opened := false;
  Errorcode := 0;
  FileMode := $42;
  RecordSize := 1;
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

destructor tFileObj.Done;
begin
  if Opened then Close;
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure tFileObj.Assign(FName: String);
begin
  FileName := FName;
  ZeroName := FileName + #00;

  {$IFDEF DOSMODE}
    System.Assign(FileVar, FName);
  {$ENDIF}
end; { proc. Assign }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

function tFileObj.Create(RecSize: Longint): Boolean;
{$IFDEF OS2}
var ApiFlags    : Longint;
    Temp        : Longint;
{$ENDIF}

{$IFDEF WIN32}
const
  AccessMode: array[0..2] of THandle = (
    generic_Read, generic_Write, generic_Read or generic_Write);
  ShareMode: array[0..4] of Integer = (
    0, 0, file_share_Read, file_share_Write, file_share_Read or file_share_Write);
var SecAttr     : TSecurityAttributes;
{$ENDIF}

var Created     : Boolean;
begin
  Created := false;
  Create := false;
  RecordSize := RecSize;
  if FileName = '' then EXIT;
  if Opened then Close;

  {$IFDEF DOSMODE}
    System.FileMode := FileMode;
    {$i-} ReWrite(FileVar, 1); {$i+}
    SetError(System.IoResult);
    Created := (ErrorCode = 0);

    {$IFDEF FPC}
      if ErrorCode = 0 then Reset(FileVar, 1);
      if ErrorCode = 0 then Seek(0);
    {$ENDIF}
  {$ENDIF}

  {$IFDEF WIN32}
     {-- Setup the security attributes --------------------------------------}
     FillChar(SecAttr, SizeOf(SecAttr), #0);
     SecAttr.nLength := SizeOf(SecAttr);
     SecAttr.lpSecurityDescriptor := NIL;
     SecAttr.bInheritHandle := TRUE;


     SetLastError(0);
     FileVar := CreateFile(@ZeroName[1],
                           DWORD(AccessMode[FileMode AND 3]),
                           DWORD(ShareMode[(FileMode and $F0) shr 4]),
                           @SecAttr,
                           Create_Always,
                           file_attribute_normal,
                           0);

     if (FileVar <> INVALID_HANDLE_VALUE) then ErrorCode := 0
       else begin
              ErrorCode := GetLastError;
              if ErrorCode = 0 then ErrorCode := 1;             { Just.. }
            end; { else }

     Created := (ErrorCode = 0);
  {$ENDIF}

  {$IFDEF OS2}
    APIFlags := file_create + file_truncate;
    ErrorCode := DosOpen(@ZeroName[1],                             { Filename }
                         FileVar,                                { Filehandle }
                         Temp,                        { Action actually taken }
                         0,                                        { cbFile ? }
                         0,                                       { Attribute }
                         ApiFlags,                                { OpenFlags }
                         FileMode,                         { Filemode we want }
                         nil);

    Created := (ErrorCode = 0);
  {$ENDIF}

  Opened := Created;
  Create := Created;
  {$IFDEF DOSMODE}
    FileHandle := FileRec(FileVar).Handle;
  {$ELSE}
    FileHandle := FileVar;
  {$ENDIF}
end; { func. Create }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

function tFileObj.Open(RecSize: Longint): Boolean;
{$IFDEF OS2}
var ApiFlags    : Longint;
    Temp        : Longint;
{$ENDIF}
begin
  Open := false;
  RecordSize := RecSize;
  if FileName = '' then EXIT;

  {$IFDEF DOSMODE}
    System.FileMode := FileMode;
    {$i-} Reset(FileVar, 1); {$i+}
    SetError(System.IoResult);
    Opened := (ErrorCode = 0);

    {$IFDEF FPC}
      if Errorcode=0 then  Seek(0);
    {$ENDIF}
  {$ENDIF}

  {$IFDEF WIN32}
    FileVar := FileOpen(FileName, FileMode);
    ErrorCode := GetLastError;

    Opened := (ErrorCode = 0);
  {$ENDIF}

  {$IFDEF OS2}
    APIFlags := open_action_open_if_exists or open_action_fail_if_new;
    ErrorCode := DosOpen(@ZeroName[1], FileVar, Temp, 0, 0, ApiFlags, FileMode, nil);
    Opened := (ErrorCode = 0);
  {$ENDIF}

  Open := Opened;
  {$IFDEF DOSMODE}
    FileHandle := FileRec(FileVar).Handle;
  {$ELSE}
    FileHandle := FileVar;
  {$ENDIF}
end; { func. Open }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

function tFileObj.OpenOrCreate(RecSize: Longint): Boolean;
begin
  OpenOrCreate := true;

  if NOT Open(RecSize) then
    OpenOrCreate := Create(RecSize);
end; { func. OpenOrCreate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

function tFileObj.BlkRead(var Buf; BytesToRead: Longint): Longint;
var BytesRead: LongType;
begin
  {$IFDEF DOSMODE}
    if IoResult > 0 then ;
    {$i-} System.BlockRead(FileVar, Buf, BytesToRead * RecordSize, BytesRead); {$i+}
    SetError(System.IoResult);
  {$ENDIF}

  {$IFDEF WIN32}
    SetLastError(0);
    BytesRead := FileRead(FileVar, Buf, BytesToRead * RecordSize);
    SetError(GetLastError);
  {$ENDIF}

  {$IFDEF OS2}
    DosRead(FileVar, Buf, BytesToRead * RecordSize, BytesRead);
  {$ENDIF}

(* Breaks MKMSGJAM.PAS
  if BytesRead < (BytesToRead * RecordSize) then
   if ErrorCode = 0 then
     SetError(100);
*)
  if ErrorCode = 0 then SetError(0);

  if BytesRead = 0 then
   if BytesToRead > 0 then
    if ErrorCode = 0 then
      SetError(100);

  if RecordSize > 0 then
   BlkRead := BytesRead div RecordSize
    else BlkRead := 0;
end; { func. BlkRead }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

function tFileObj.BlkWrite(var Buf; BytesToWrite: Longint): Longint;
var BytesWritten: LongType;
begin
  {$IFDEF DOSMODE}
    {$i-} System.BlockWrite(FileVar, Buf, BytesToWrite * RecordSize, BytesWritten); {$i+}
    SetError(System.IoResult);
  {$ENDIF}

  {$IFDEF WIN32}
    SetLastError(0);
    BytesWritten := FileWrite(FileVar, Buf, BytesToWrite * RecordSize);
    SetError(GetLastError);
  {$ENDIF}

  {$IFDEF OS2}
    DosWrite(FileVar, Buf, BytesToWrite * RecordSize, BytesWritten);
  {$ENDIF}

  if BytesWritten < (BytesToWrite * RecordSize) then
   if ErrorCode = 0 then
     SetError(101);


  if RecordSize > 0 then
    BlkWrite := BytesWritten div RecordSize
     else BlkWrite := 0;
end; { func. BlkWrite }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure tFileObj.Seek(FPos: Longint);
{$IFDEF OS2}
var Actual: Longtype;
{$ENDIF}
begin
  if FPos < 0 then FPos := 0;

  {$IFDEF DOSMODE}
    {$i-} System.Seek(FileVar, FPos * RecordSize); {$i+}
    SetError(System.IoResult);
  {$ENDIF}

  {$IFDEF WIN32}
    SetLastError(0);
    FileSeek(FileVar, FPos * RecordSize, 0);
    SetError(GetLastError);
  {$ENDIF}

  {$IFDEF OS2}
    DosSetFilePtr(FileVar, FPos * RecordSize, file_begin, Actual);
  {$ENDIF}
end; { proc. Seek }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

function tFileObj.FilePos: Longint;
var Temp: Longint;
begin
  {$IFDEF DOSMODE}
    {$i-} Temp := System.FilePos(FileVar); {$i+}
    SetError(System.IoResult);
  {$ENDIF}

  {$IFDEF WIN32}
    SetLastError(0);
    Temp := FileSeek(FileVar, 0, 1);
    SetError(GetLastError);
  {$ENDIF}

  {$IFDEF OS2}
    DosSetFilePtr(FileVar, 0, file_current, Temp);
  {$ENDIF}

  if RecordSize > 0 then
    FilePos := Temp DIV RecordSize
     else FilePos := 0;
end; { func. FilePos }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

function tFileObj.FileSize: Longint;
var SavePos: Longint;
    Temp   : Longint;
begin
  SavePos := FilePos;

  {$IFDEF DOSMODE}
    {$i-} Temp := System.FileSize(FileVar); {$i+}
    SetError(System.IoResult);
  {$ENDIF}

  {$IFDEF WIN32}
    SetLastError(0);
    Temp := FileSeek(FileVar, 0, 2);
    SetError(GetLastError);
  {$ENDIF}

  {$IFDEF OS2}
    DosSetFilePtr(FileVar, 0, file_end, Temp);
  {$ENDIF}

  Seek(SavePos);

  if RecordSize > 0 then FileSize := Temp DIV RecordSize
    else FileSize := 0;
end; { func. FileSize }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

function tFileObj.EOF: Boolean;
begin
  EOF := FileSize <= FilePos;
end; { func. EOF }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

function tFileObj.Erase: Boolean;
begin
  if Opened then Close;

  {$IFDEF DOSMODE}
    {$i-} System.Erase(FileVar); {$i+}
    Erase := (System.IoResult = 0);
  {$ENDIF}

  {$IFDEF WIN32}
    Erase := DeleteFile(FileName);
    ErrorCode := 0;
  {$ENDIF}

  {$IFDEF OS2}
    Erase := (DosDelete(@ZeroName[1]) = 0);
  {$ENDIF}
end; { func. Erase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure tFileObj.WriteLn(S: ShortString);
begin
  Seek(FileSize);

  {$IFNDEF ELEUNIX}
    Write(S + #13#10);
  {$ELSE}
    Write(S + #10);
  {$ENDIF}
end; { proc. WriteLn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

{$IFNDEF MSDOS}
procedure tFileObj.AnsiWriteLn(S: AnsiString);
begin
  Seek(FileSize);

  {$IFNDEF ELEUNIX}
    AnsiWrite(S + #13#10);
  {$ELSE}
    AnsiWrite(S + #10);
  {$ENDIF}
end; { proc. AnsiWriteLn }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

{$IFNDEF MSDOS}
procedure tFileObj.AnsiWrite(S: AnsiString);
begin
  BlkWrite(S[1], Length(s));
end; { proc. AnsiWrite }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure tFileObj.Write(S: ShortString);
begin
  BlkWrite(S[1], Length(s));
end; { proc. Write }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure tFileObj.ReadLn(var S: ShortString);
var BytesRead: Longint;
    TempPos  : Longint;
begin
  BytesRead := BlkRead(S[1], SizeOf(S) - 1);
  S[0] := Chr(BytesRead);

  TempPos := Pos(#10, S);
  if TempPos = 0 then
    TempPos := Pos(#13, S);

  if TempPos > 0 then
    begin
      Seek((FilePos - (BytesRead - 1)) + (TempPos - 1));
      S[0] := Chr(TempPos);

      if S[Length(s)] = #13 then Dec(S[0]);
      if S[Length(s)] = #10 then Dec(S[0]);
      if S[Length(s)] = #13 then Dec(S[0]);
    end; { if }
end; { proc. ReadLn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure tFileObj.Read(var S: ShortString);
begin
  ReadLn(S);
end; { proc. ReadLn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

{$IFNDEF MSDOS}
procedure tFileObj.AnsiReadLn(var S: AnsiString);
type MyBuf = Array[0..(1024 * 65)] of Char;
var BytesRead   : Longint;
    TempPos     : Longint;
    EolLen      : Longint;
    SavePos     : Longint;
    AnsiReadSize: Longint;
    TmpBuf      : MyBuf;
begin
  AnsiReadSize := SizeOf(TmpBuf) - 1;
  SavePos := FilePos;
  TmpBuf[0] := #0;
  BytesRead := BlkRead(TmpBuf[0], AnsiReadSize - 1);

  TempPos := 0;
  EolLen := 0;

  while (TempPos <= BytesRead) do
    begin
      if TmpBuf[TempPos] in [#10] then
        begin
          EolLen := 1;
          TmpBuf[TempPos] := #0;
          BREAK;
        end; { if }

      if TmpBuf[TempPos] in [#13] then
        begin
          TmpBuf[TempPos] := #0;

          if TempPos < BytesRead then
            if TmpBuf[TempPos + 1] = #10 then
              begin
                TempPos := TempPos + 1;
                EolLen := 2;
              end { if }
                else EolLen := 1;

          BREAK;
        end; { if }

      Inc(TempPos);
    end; { while }

  S := StrPas(TmpBuf);

  if BytesRead > 0 then
    Seek(SavePos + Length(S) + EolLen);
end; { proc. AnsiReadLn }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

{$IFNDEF MSDOS}
procedure tFileObj.AnsiRead(var S: AnsiString);
const
  maxReadSize = (64 * 1024);

var BytesRead   : Longint;
    TempPos     : Longint;
    AnsiReadSize: Longint;
begin
  AnsiReadSize := FileSize;
  if AnsiReadSize > maxReadSize then
    AnsiReadsize := MaxReadSize;
  SetLength(S, AnsiReadSize);

  BytesRead := BlkRead(S[1], AnsiReadSize);
end; { proc. AnsiRead }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

function tFileObj.Close: Boolean;
begin
  Close := true;
  if NOT Opened then EXIT;

  {$IFDEF DOSMODE}
    {$i-} System.Close(FileVar); {$i+}
    Close := (System.IoResult = 0);
  {$ENDIF}

  {$IFDEF WIN32}
    if FileVar > 0 then
      begin
        FileClose(FileVar);
        FileVar := 0;
      end; { if }
  {$ENDIF}

  {$IFDEF OS2}
    if FileVar > 0 then
      begin
        Os2Base.DosClose(FileVar);
        FileVar := -1;
      end; { if }
  {$ENDIF}

  Opened := false;
  FileHandle := -1;
end; { func. Close }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

function tFileObj.IoResult: Longint;
begin
  IoResult := ErrorCode;
  ErrorCode := 0;
end; { func. IoResult }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure tFileObj.SetError(Error: Longint);
begin
  ErrorCode := Error;

  {$IFDEF WIN32}
    SetLastError(0);
  {$ENDIF}
end; { proc. SetError }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure tFileObj.FlushFile;
{$IFDEF DOSMODE}
 {$IFNDEF ELEUNIX}
   var Regs: Registers;
 {$ENDIF}
{$ENDIF}
begin
 {$IFDEF DOSMODE}
   {$IFNDEF ELEUNIX}
     Regs.AH := $45;
     Regs.BX := FileRec(FileVar).Handle;
     Intr($21, Regs);
     if (Regs.Flags and 1) = 0 Then   {carry}
       begin
         Regs.BX := Regs.AX;
         Regs.AH := $3e;
         Intr($21, regs);
       end; { if }
   {$ENDIF}
 {$ENDIF}

 {$IFDEF WIN32}
    Windows.FlushFileBuffers(FileVar);
 {$ENDIF}

 {$IFDEF OS2}
    DosResetBuffer(FileVar);
 {$ENDIF}
end; { proc. FlushFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure tFileObj.SetFiletime(L: Longint);
{$IFDEF OS2}

type
  TDateTimeRec = record
    FTime,FDate: SmallWord;
  end;
var Info: FileStatus3;
    FDateTime: TDateTimeRec absolute L;
{$ENDIF}
begin
  {$IFDEF DOSMODE}
    SetFTime(FileVar, L);
  {$ENDIF}

  {$IFDEF WIN32}
    FileSetDate(FileVar, L);
  {$ENDIF}

  {$IFDEF OS2}
    if DosQueryFileInfo(FileVar, fil_Standard, Info, SizeOf(Info)) = 0 then
      with FDateTime do
        begin
          Info.ftimeLastWrite := FTime;
          Info.fdateLastWrite := FDate;
          DosSetFileInfo(FileVar, fil_Standard, Info, SizeOf(Info));
        end; { with }
  {$ENDIF}
end; { proc. SetFileTime }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

function tFileObj.GetFileTime: Longint;
var L: Longint;
begin
  {$IFDEF DOSMODE}
    GetFTime(FileVar, L);
  {$ENDIF}

  {$IFDEF WIN32}
    L := FileGetDate(FileVar);
  {$ENDIF}

  {$IFDEF OS2}
    GetFTime(FileVar, L); { !!!! }
  {$ENDIF}

  GetFileTime := l;
end; { func. GetFileTime }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

function tFileObj.Exists: Boolean;
begin
  Exists := true;
  if Opened then EXIT;
  Exists := false;

  if Open(1) then Exists := true;
  Close;
end; { func. Exists }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

function tFileObj.Rename(NewName: String): Boolean;
begin
  {$IFDEF DOSMODE}
    {$i-} System.Rename(FileVar, NewName); {$i+}
    SetError(System.IoResult);

    Rename := (ErrorCode = 0);
  {$ENDIF}

  {$IFDEF WIN32}
    NewName := NewName + #00;

    Rename := MoveFile(@ZeroName[1], @NewName[1]);
    SetError(GetLastError);

    FileName := Copy(NewName, 1, Length(NewName) - 1);
    ZeroName := FileName + #00;
  {$ENDIF}

  {$IFDEF OS2}
    NewName := NewName + #00;

    Rename := (DosMove(@ZeroName[1], @NewName[1]) = 0);

    FileName := Copy(NewName, 1, Length(NewName) - 1);
    ZeroName := FileName + #00;
  {$ENDIF}
end; { proc. Rename }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+*)

procedure tFileObj.SetAttr(Attr: Longint);
{$IFDEF OS2}
var Info: FileStatus3;
    Temp: Longint;
{$ENDIF}
begin
  {$IFDEF DOSMODE}
    SetfAttr(FileVar, Attr);
  {$ENDIF}

  {$IFDEF WIN32}
    SetFileAttributes(@ZeroName[1], Attr);
  {$ENDIF}

  {$IFDEF OS2}
    Temp := DosQueryPathInfo(@ZeroName[1], fil_Standard, Info, SizeOf(Info));
    if Temp = 0 then
      begin
        Info.attrFile := Attr;
        DosSetPathInfo(@ZeroName[1], fil_Standard, Info, SizeOf(Info), dspi_WrtThru);
      end; { if }
  {$ENDIF}
end; { proc. SetAttr }


end. { unit FILEOBJ }
