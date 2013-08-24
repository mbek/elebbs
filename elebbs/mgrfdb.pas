unit MgrFDB;
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
{$I-}
(*
**
** EleMGR FDB routines for EleBBS
**
** Copyright (c) 1998 by Maarten Bekers
**
** Created : 14-Jun-1998
** Last update : 02-Jul-1998
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
uses CfgRec, Global, Ra2Fdb, FileObj,
     ObjDec;

{ This objects extends "FdbFileObj" with writing. This writing is not }
{ buffered to forecom too much complexity }

Type DoBaseProc = procedure(var CurHDR: CfgRec.FilesHdrRecord;
                            var CurIDX: CfgRec.FilesIdxRecord;
                            var HdrF: pFileObj;
                            var IdxF: pFileObj;
                            LfnName: String;
                            RecNum : Longint);

type MgrFileObj = Object(FdbFileObj)
       constructor Init(AreaNum: Longint; Create: Boolean);
       destructor  Done;

       function    AddDescription(var TxtInf: TxtRec): Longint;
       function    AddHeader(var HDR: FilesHdrRecord): Boolean;

       function    LockFileBase(var HDR: pFileObj): Boolean;
       procedure   UnLockFileBase(var HDR: pFileObj);

       procedure   Header2Idx(const Hdr: FilesHdrRecord; var IDX: FilesIdxRecord);

       procedure   WriteHdr(HdrRec: Longint; Hdr: FilesHdrRecord);
       procedure   WriteIdx(IdxRec: Longint; Idx: FilesIdxRecord);
       procedure   WriteBoth(HdrRec: Longint; Hdr: FilesHdrRecord);

       function    AddLfnPtr(LongFName: String; Path: String; var ShortName: String): longint;
       function    FileBaseMaintenance(AreaNr   : Longint;
                                       BaseProc : DoBaseProc;
                                       IgnoreIDX: Boolean): FDB_ResultCodeType;
       function UniqueName(Sort: String; AreaNr: Longint): String;
     end; { fdbFileObj }


function IsLfn(S: String): Boolean;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WIN32}
uses Sysutils, StrPath, Windows, MemMan, MkFile, Crc_Unit, GenFile
{$ENDIF}

{$IFDEF OS2}
uses Strings, MemMan, MkFile, Crc_Unit, GenFile
{$ENDIF}

{$IFDEF MSDOS}
uses Strings, MemMan, MkFile, Crc_Unit, GenFile
{$ENDIF}

{$IFDEF GO32V2}
uses Strings, MemMan, MkFile, Crc_Unit, GenFile
{$ENDIF}

{$IFDEF ELEUNIX}
uses Strings, MemMan, MkFile, Crc_Unit, GenFile
{$ENDIF}

  {$IFDEF WITH_DEBUG}
    , LongStr, Debug_U
  {$ENDIF}

  {$IFDEF WITH_FULL}
    ,RAL
  {$ENDIF};

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function IsLfn(S: String): Boolean;
begin
  IsLfn := (Length(s) > 12);       { Check if its larger than 12 characters }

  if Pos('.', S) > 0 then               { More than 3 chars behind the dot? }
   if Length(Copy(S, Pos('.', S) + 1, 255)) > 3 then IsLfn := true;

  if Pos(',', S) > 0 then                                { Contains a comma }
     IsLfn := TRUE;

  if Pos(' ', S) > 0 then                             { Contains whitespace }
     IsLfn := TRUE;

  if Pos('.', S) > 0 then                          { Contains multiple dots }
   if Pos('.', Copy(S, Pos('.', S) + 1, 255)) > 0 then
     IsLfn := TRUE;
end; { func. IsLfn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor MgrFileObj.Init(AreaNum: Longint; Create: Boolean);
begin
  SetFileMode(ReadWriteMode + DenyNone);

  inherited init(AreaNum, Create);
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor MgrFileObj.Done;
begin
  inherited Done;
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function MgrFileObj.AddLfnPtr(LongFName: String; Path: String; var ShortName: String): longint;
var TxtInf : TxtRec;
    Temp   : Longint;
{$IFDEF Win32}
    TempLen: Longint;
    TempStr: String[255];
{$ENDIF}
begin
  AddLfnPtr := -1;

  if NOT IsLfn(LongFName) then
    begin
      ShortName := LongFname;
      EXIT;
    end; { if }

  LongFname := LongFname + #00;
  Move(LongFName[1], TxtInf, Length(LongFname));
  Temp := AddDescription(TxtInf);

  {$IFDEF WIN32}
    Path := Path + #00;
    TempLen := GetShortPathName(@Path[1], @TempStr[1], SizeOf(TempStr));
    TempStr[0] := Chr(TempLen);

    if Length(TempStr) > 00 then ShortName := JustName(TempStr)
      else ShortName := JustName(Path);
  {$ENDIF}

  if Temp = 0 then
    Temp := AddDescription(TxtInf);

  AddLfnPtr := Temp;
end; { func. AddLfnptr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function MgrFileObj.AddDescription(var TxtInf: TxtRec): Longint;
begin
  if StrLen(TxtInf) = 00 then
    begin
      AddDescription := -1;
      EXIT;
    end; { if }

  AddDescription := TxtF^.FileSize;                       { Did not succeed }

  if StrLen(TxtInf) = SizeOf(TxtInf) then
    TxtInf[SizeOf(TxtInf) - 1] := #00;     { Make sure string is terminated }

  {$i-}
    TxtF^.Seek(TxtF^.FileSize);               { Seek to the end of the desc }
    TxtF^.BlkWrite(TxtInf, StrLen(TxtInf) + 01);
  {$i+}
  SetError(IOResult);

  if GetError>00 then AddDescription := -1;              { Operation failed }

  ResetCache;        { Make sure the object is using the latest information }
end; { func. AddDescription }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function MgrFileObj.AddHeader(var HDR: FilesHdrRecord): Boolean;
var IDX: FilesIdxRecord;
begin
  if IOResult>00 then; { Clear the error }

  AddHeader := false;

  {$i-}
    HdrF^.Seek(HdrF^.FileSize);
    IdxF^.Seek(IdxF^.FileSize);

     if LockFileBase(HdrF) then
      begin
        Header2Idx(Hdr, IDX);                         { Convert Hdr to Idx }

        HdrF^.BlkWrite(HDR, SizeOf(FilesHdrRecord));
        IdxF^.BlkWrite(IDX, SizeOf(FilesIdxRecord));

        UnLockFileBase(HdrF);
      end; { if }
  {$i+}
  AddHeader := (IOResult=00);
end; { func. AddHeader }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure MgrFileObj.Header2Idx(const Hdr: FilesHdrRecord; var IDX: FilesIdxRecord);
var Counter: Byte;
begin
  FillChar(Idx, SizeOf(FilesIdxRecord), #00);

  Idx.Name := Hdr.Name;
  Idx.UploadDate := Hdr.UploadDate;

  for counter := 01 to 05 do
    Idx.KeyWordCRC[counter] := RaCrc(Hdr.KeyWord[counter], true);

  Idx.LongDescPtr := Hdr.LongDescPtr;
end; { proc. Header2Idx }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure MgrFileObj.WriteHdr(HdrRec: Longint; Hdr: FilesHdrRecord);
begin
  {$i-}
    HdrF^.Seek(HdrRec * SizeOf(FilesHdrRecord));

    if LockFileBase(HdrF) then
      begin
        HdrF^.BlkWrite(HDR, SizeOf(FilesHdrRecord));

        UnLockFileBase(HdrF);
      end; { if }
  {$i+}
  SetError(IOResult);

  ResetCache;
end; { proc. Writehdr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure MgrFileObj.WriteIdx(IdxRec: Longint; Idx: FilesIdxRecord);
begin
  {$i-}
    IdxF^.Seek(IdxRec * SizeOf(FilesIdxRecord));

    if LockFileBase(HdrF) then
      begin
        IdxF^.BlkWrite(IDX, SizeOf(FilesIDXRecord));

        UnLockFileBase(HdrF);
      end; { if }
  {$i+}
  SetError(IOResult);

  ResetCache;
end; { proc. WriteIDX }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure MgrFileObj.WriteBoth(HdrRec: Longint; Hdr: FilesHdrRecord);
var IDX: FilesIdxRecord;
begin
  WriteHdr(HdrRec, Hdr);

  Header2Idx(Hdr, Idx);
  WriteIDX(HdrRec, Idx);
end; { proc. WriteBoth }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function MgrFileObj.LockFileBase(var Hdr: pFileObj): Boolean;
var Temp      : Integer;
    UserAbort : Boolean;
    Count     : Longint;
begin
  Count := 00;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'LockFileBase (begin)');
  {$ENDIF}

  repeat
    Temp := ShLock(Hdr, 0, SizeOf(FilesHdrRecord));

    UserAbort := False;
    Inc(Count);

    If NOT RunningBBS then UserAbort := True;

    If (Temp=5) then
      begin
       {$IFDEF WITH_FULL}
        If RunningBBS then
            If NOT InputObj^.ralStrYesNoAsk(ralbaseIUse) then
             { Locking violation error }
               UserAbort := True;
       {$ENDIF}

        If NOT RunningBBS then UserAbort := True;
      end; { Locking violation error }

  Until (Temp=00) OR (UserAbort) OR (Count>10);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'LockFileBase ( end ) Temp='+FStr(Temp));
  {$ENDIF}

  LockFileBase := (Temp=00);
end; { func. LockFileBase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure MgrFileObj.UnLockFileBase(var Hdr: pFileObj);
begin
  MkFile.ShUnLock(Hdr, 00, SizeOf(FilesHdrRecord));
end; { proc. UnlockFileBase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function MgrFileObj.UniqueName(Sort: String; AreaNr: Longint): String;
var NewName: String;
    AreaStr: String;
    TempF  : pFileObj;
begin
  Str(AreaNr, AreaStr);

  NewName := '';
  UniqueName := '';

  if NOT FileExist(Sort + '$' + AreaStr + '.000') then
    NewName := Sort + '$' + AreaStr + '.000' else
   if NOT FileExist(Sort + '$' + AreaStr + '.001') then
     NewName := Sort + '$' + AreaStr + '.001' else
    if NOT FileExist(Sort + '$' + AreaStr + '.002') then
      NewName := Sort + '$' + AreaStr + '.002' else
    if NOT FileExist(Sort + '$' + AreaStr + '.003') then
      NewName := Sort + '$' + AreaStr + '.003'
       else NewName := '';

  if NewName = '' then EXIT;

  New(TempF, Init);
  TempF^.Assign(NewName);
  TempF^.FileMode := ReadWriteMode + DenyAll;
  if NOT TempF^.Create(1) then NewName := '';
  Dispose(TempF, Done);

  UniqueName := NewName;
end; { func. UniqueName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function MgrFileObj.FileBaseMaintenance(AreaNr   : Longint;
                                        BaseProc : DoBaseProc;
                                        IgnoreIDX: Boolean): FDB_ResultCodeType;
const HdrFile = 00;
      IdxFile = 01;

var NewHdrName : String;
    NewIdxName : String;

    OldFiles   : Array[HdrFile..IdxFile] of pFileObj;
    NewFiles   : Array[HdrFile..IdxFile] of pFileObj;
    LfnFile    : pFileObj;

    Hdr        : FilesHdrRecord;
    Idx        : FilesIdxRecord;
    Lfn        : TxtRec;

    ReadIDX    : NumreadType;
    ReadHDR    : NumReadType;

    LfnName    : String;

procedure CloseAll;           { We cannot risk the loose of 4 file handles }
begin
  {$i-}
    OldFiles[HdrFile]^.Close;
    OldFiles[IdxFile]^.Close;

    NewFiles[HdrFile]^.Close;
    NewFiles[IdxFile]^.Close;

    LfnFile^.Close;
  {$i+}
  if IOResult > 00 then ;
end; { proc. CloseAll }


function OpenBase(var F: pFileObj; FName: String): Boolean;
begin
  F^.Assign(FName);
  F^.FileMode := ReadWriteMode + DenyWrite;
  {$i-}
    F^.Open(1);
  {$i+}
  OpenBase := (F^.IOResult=00) AND (FName<>'');
end; { func. OpenBase }

begin
  {-------------------- Initialize all those pointers -------------------}
  New(OldFiles[HdrFile], Init);
  New(OldFiles[IdxFile], Init);
  New(NewFiles[HdrFile], Init);
  New(NewFiles[IdxFile], Init);
  New(LfnFile, Init);

  CloseAreaFiles; { Close all open files so they are available to this one }
  FileBaseMaintenance := FDB_OK;

  {----------------- Start opening all the original files ------------------}
  if NOT OpenBase(OldFiles[HdrFile], GetFileBasePath('hdr', AreaNr)) then
   begin
     FileBaseMaintenance := fdb_NoHdr;
     CloseAll;
     EXIT;
   end; { NO HDR }

  if NOT OpenBase(OldFiles[IdxFile], GetFileBasePath('idx', AreaNr)) then
   begin
     if NOT IgnoreIDX then
      begin
        FileBaseMaintenance := fdb_NoIDX;
        CloseAll;
        EXIT;
      end; { if }
   end; { NO IDX }

  {------------------------ Open the LFN file we need ----------------------}
  LfnFile^.Assign(GetFileBasePath('txt', Areanr));
  LfnFile^.FileMode := ReadMode + DenyNone;
  {$i-} LfnFile^.Open(1); {$i+}
  if LfnFile^.IoResult > 00 then
    begin
      FileBaseMaintenance := fdb_NoHdr;
      CloseAll;
      EXIT;
    end; { if }

  {------------------ Creating temporarily unique files --------------------}
  NewHdrName := UniqueName('hdr', AreaNr);
  NewIdxName := UniqueName('idx', AreaNr);

  if (NewHdrName='') OR ((NewIdxName='') AND (NOT IgnoreIDX)) then
    begin
      FileBaseMaintenance := fdb_NoTempPath;
      CloseAll;
      EXIT;
    end; { if }

  {----------------- Start opening all the new files -----------------------}
  if NOT OpenBase(NewFiles[HdrFile], NewHdrName) then
   begin
     FileBaseMaintenance := fdb_NoTempPath;
     CloseAll;
     EXIT;
   end; { NO HDR }

  if NOT OpenBase(NewFiles[IdxFile], NewIdxName) then
   begin
     FileBaseMaintenance := fdb_NoTempPath;
     CloseAll;
     EXIT;
   end; { NO IDX }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'FDBmaintenance: (begin while)');
  {$ENDIF}

  {-------------------- Run through all the headers ------------------------}
  if LockFileBase(OldFiles[HdrFile]) then
   begin
     While (NOT OldFiles[HdrFile]^.EOF) do
       begin
         {$i-}
           ReadHDR := OldFiles[HdrFile]^.BlkRead(Hdr, SizeOf(FilesHdrRecord));

           if NOT IgnoreIDX then
             ReadIDX := OldFiles[IdxFile]^.BlkRead(Idx, SizeOf(FilesIdxRecord))
              else ReadIDX := SizeOf(FilesIdxRecord);

           if Hdr.LfnPtr > 00 then
             begin
                FillChar(Lfn, SizeOf(Txtrec), #00);
                LfnFile^.Seek(Hdr.LfnPtr);
                LfnFile^.BlkRead(Lfn, SizeOf(TxtRec));

                LfnName := StrPas(Lfn);
             end { has a long filename }
               else LfnName := Hdr.Name;
         {$i+}
         if (IOResult>00) OR (ReadHDR <> SizeOf(FilesHdrRecord))
             OR (ReadIDX <> SizeOf(FilesIdxRecord)) then BREAK;

         CurHDR^ := HDR;
         CurIDX^ := IDX;

         BaseProc(Hdr,
                  Idx,
                  NewFiles[HdrFile],
                  NewFiles[IdxFile],
                  LfnName,
                  (OldFiles[HdrFile]^.FilePos DIV SizeOf(FilesHdrRecord)) - 1);
       end; { while }

     UnlockfileBase(OldFiles[HdrFile]);
  end  { with }
   else begin
          EraseFile(NewHdrName);
          EraseFile(NewIdxName);

          NewHdrName := '';
          NewIdxName := '';
          FileBaseMaintenance := fdb_NoLock;
        end; { if }

  {---------------------- Close all files and save -------------------------}
  CloseAll;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'FDBmaintenance: (end   while)');
    DebugObj.DebugLog(logFileSys, 'NewHdrName      = ' + NewHdrName);
    DebugObj.DebugLog(logFileSys, 'NewIdxName      = ' + NewIdxName);
    DebugObj.DebugLog(logFileSys, 'GetFileBase(HDR)= ' + GetFileBasePath('hdr', AreaNr));
    DebugObj.DebugLog(logFileSys, 'GetFileBase(IDX)= ' + GetFileBasePath('idx', AreaNr));
  {$ENDIF}

  if (NewHdrName <> '') AND (NewIdxName <> '') then
    begin
     {$i-}
       EraseFile(GetFileBasePath('hdr', AreaNr));
       EraseFile(GetFileBasePath('idx', AreaNr));

       RenameFile(NewHdrName, GetFileBasePath('hdr', AreaNr));
       RenameFile(NewIdxName, GetFileBasePath('idx', AreaNr));
     {$i+}
     if IOResult>00 then ;

     FileBaseMaintenance := fdb_OK;
    end; { if }

  OpenAreaFiles(True);

  Dispose(OldFiles[HdrFile], Done);
  Dispose(OldFiles[IdxFile], Done);
  Dispose(NewFiles[HdrFile], Done);
  Dispose(NewFiles[IdxFile], Done);
  Dispose(LfnFile, Done);
end; { func. FileBaseMaintenance }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
