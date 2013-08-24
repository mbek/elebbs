unit Ra2Fdb;
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
** RA2 FDB routines for EleBBS
*
** Copyright (c) 1998 by Maarten Bekers
**
** Created : 20-Feb-1998
** Last update : 16-Dec-2001
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
uses CfgRec, Global, StrPath, FileObj, Cases;

Const HdrBuf = 20;

type IdxRec = Array[1..hdrBuf] of FilesIdxRecord;
     HdrRec = Array[1..hdrBuf] of FilesHdrRecord;
     TxtRec = Array[0..(10240 - 01)] of Char;

type FdbFileObj = Object
          TempError    : Integer;
          FileBaseFileMode: Word;

          IdxInf       : record
                            StartNr: Longint;
                            EndNr  : Longint;
                            Idx    : ^IdxRec;
                         end; { idx }
          HdrInf       : ^HdrRec;
          CurHdr       : ^FilesHdrRecord;
          Curidx       : ^FilesIdxRecord;
          CurTxt       : ^TxtRec;
          TxtPtr       : Longint;
          TxtSize      : Longint;
          CurRec       : Longint;
          HdrF         : pFileObj;
          IdxF         : pFileObj;
          TxtF         : pFileObj;
          LfnF         : pFileObj;
          InDescPtr    : Longint;
          SaveAreaNum  : Word;
          TotalCached  : Word;
          AbortStrChars: Set of Char;

          constructor Init(AreaNum: Longint; Create: Boolean);
          destructor Done;

          procedure SetFileMode(FMode: Word);
          function GetFileMode: Word;

          procedure Read(HdrRec: Longint);
          function TotalRecords: Longint;
          function CurrentRecord: Longint;
          function GetAreaNum: Word;

{
                     LongDescPtr    : LongInt;
}

          function GetFileName: String;
          function GetShortName: String;
          function GetSize: Longint;
          function GetSizeStr: String;
          function GetUploader: String;
          function GetTimesDL: Word;
          function GetPassword: String;
          function GetKeyword(Index: Byte): String;
          function GetCost: Word;
          function GetCrc32: Longint;
          function GetUploadDate: Longint;
          function GetFileDate: Longint;
          function GetLastDL: Longint;
          function IsDeleted: Boolean;
          function IsUnlisted: Boolean;
          function IsFree: Boolean;
          function IsNotAvail: Boolean;
          function IsLocked: Boolean;
          function IsMissing: Boolean;
          function IsNoTime: Boolean;
          function GetLongDescPtr: Longint;
          function GetLfnPtr: Longint;
          function GetAttrib: Longint;
          function IsComment: Boolean;
          procedure GetDescBlock(var Txt: TxtRec);

          function GetError: Integer;
          procedure SetError(I: Integer);
          procedure ResetCache;
          procedure ReadDescription(var TextFile: pFileObj; LongDescPtr: Longint);
          procedure ResetDescription;
          function GetDescLine(MaxLen: Byte): String;
          function EndOfDesc: Boolean;
          function GetPrevWord(ThisDescPtr:Longint; Const MinValue: Longint): Longint;
          function GetFileBasePath(S: String; AreaNum: Word): String;

          function MakeUpFName(S: String): String;

          procedure CreateNewTxt(AreaNum: Word);
          procedure CreateNewIdx(AreaNum: Word);
          procedure CreateNewHdr(AreaNum: Word);

          procedure CreateFileBase(AreaNum: Word);
          procedure ClearError;
          procedure OpenAreaFiles(Create: Boolean);
          procedure CloseAreaFiles;
     end; { fdbFileObj }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WIN32}
uses Sysutils, MemMan;
{$ENDIF}

{$IFDEF OS2}
uses Strings, MemMan;
{$ENDIF}

{$IFDEF MSDOS}
uses Strings, MemMan;
{$ENDIF}

{$IFDEF GO32V2}
uses Strings, MemMan;
{$ENDIF}

{$IFDEF ELEUNIX}
uses Strings, MemMan;
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor FdbFileObj.Init(AreaNum: Longint; Create: Boolean);
begin
  if FileBaseFileMode <> ReadWriteMode + DenyNone then
    FileBaseFileMode := ReadMode + DenyNone;

  IdxInf.StartNr := -1;
  IdxInf.EndNr := -1;
  InDescPtr := -1;
  TxtPtr := -1;
  TxtSize := -1;
  TotalCached := 00;

  ClearError;

  IdxInf.Idx := nil;
  HdrInf := nil;
  CurHdr := nil;
  CurTxt := nil;
  CurIdx := nil;

  if NOT AllocMem(IdxInf.Idx, SizeOf(IdxRec), 'IdxRec', 'FdbFileObj.Init') then SetError(900);
  if NOT AllocMem(HdrInf, SizeOf(HdrRec), 'HdrRec', 'FdbFileObj.Init') then SetError(901);
  if NOT AllocMem(CurHdr, SizeOf(FilesHdrRecord), 'FilesHdrRecord', 'FdbFileObj.Init') then SetError(902);
  if NOT AllocMem(CurIdx, SizeOf(FilesIdxRecord), 'FilesIdxRecord', 'FdbFileObj.Init') then SetError(903);
  if NOT AllocMem(CurTxt, SizeOf(TxtRec), 'TxtRec', 'FdbFileObj.Init') then SetError(904);

  SaveAreaNum := AreaNum;
  FileMode := FileBaseFileMode;

  New(HdrF, Init);
  New(IdxF, Init);
  New(TxtF, Init);
  New(LfnF, Init);

  OpenAreaFiles(Create);
  AbortStrChars := [',', '.', ')', ']', '_', '}', '!', '?','*', #10, #00, #32];
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor FdbFileObj.Done;
begin
  if IdxInf.Idx <> nil then ReleaseMem(IdxInf.Idx, SizeOf(IdxRec));
  if HdrInf <> nil then ReleaseMem(HdrInf, SizeOf(HdrRec));
  if CurHdr <> nil then ReleaseMem(CurHdr, SizeOf(FilesHdrRecord));
  if CurIdx <> nil then ReleaseMem(CurIdx, SizeOf(FilesIdxRecord));
  if CurTxt <> nil then ReleaseMem(CurTxt, SizeOf(TxtRec));

  CloseAreaFiles;

  Dispose(HdrF, Done);
  Dispose(IdxF, Done);
  Dispose(TxtF, Done);
  Dispose(LfnF, Done);

  IdxInf.StartNr := -1;
  IdxInf.EndNr := -1;

  ResetDescription;

  IdxInf.Idx := nil;
  HdrInf := nil;
  CurHdr := nil;
  CurTxt := nil;
  CurIdx := nil;
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FdbFileObj.OpenAreaFiles(Create: Boolean);
begin
  if Create then
   CreateFileBase(GetAreaNum); { Creates the filebase if it doesn't exist }

  IdxF^.Assign(GetFileBasePath('idx', GetAreaNum));
  IdxF^.FileMode := FileBaseFileMode;
  IdxF^.Open(1);
  if IdxF^.IOResult>00 then
    if Create then CreateNewIDX(GetAreaNum);

  HdrF^.Assign(GetFileBasePath('hdr', GetAreaNum));
  HdrF^.FileMode := FileBaseFileMode;
  HdrF^.Open(1);
  if HdrF^.IOResult>00 then
    if Create then CreateNewHDR(GetAreaNum);

  LfnF^.Assign(GetFileBasePath('txt', GetAreaNum));
  LfnF^.FileMode := FileBaseFileMode;
  LfnF^.Open(1);
  if LfnF^.IOResult>00 then
    if Create then CreateNewTXT(GetAreaNum);

  TxtF^.Assign(GetFileBasePath('txt', GetAreaNum));
  TxtF^.FileMode := FileBaseFileMode;
  TxtF^.Open(1);
  if TxtF^.IOResult>00 then
    if Create then CreateNewTXT(GetAreaNum);

  SetError(IOresult);
end; { proc. OpenAreaFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FdbFileObj.CloseAreaFiles;
begin
  IdxF^.Close;
  HdrF^.Close;
  TxtF^.Close;
  LfnF^.Close;

  SetError(IoResult);
end; { proc. CloseAreaFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FdbFileObj.SetFileMode(FMode: Word);
begin
  FileBaseFileMode := FMode;
  FileMode := FMode;
end; { proc. SetFileMode }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.GetFileMode: Word;
begin
  GetFileMode := FileBaseFileMode;
end; { func. GetFileMode }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.GetFileBasePath(S: String; AreaNum: Word): String;
var AreaStr: String;
begin
  Str(AreaNum, AreaStr);

  GetFileBasePath := GlobalCfg^.RaConfig^.FileBasePath + Slowcase(S) + BsChar + 'fdb' + AreaStr + '.' + SlowCase(s);
end; { func. FileBasepath }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FdbFileObj.Read(HdrRec: Longint);
begin
  if (HdrRec >= IdxInf.StartNr) AND
      (HdrRec <= IdxInf.EndNr) then
        begin
          CurIdx^ := IdxInf.Idx^[Succ(HdrRec) - IdxInf.StartNr];
          CurHdr^ := HdrInf^[Succ(HdrRec) - IdxInf.StartNr];
          CurRec  := HdrRec;
        end
         else begin
                HdrF^.Seek(HdrRec * SizeOf(FilesHdrRecord));
                HdrF^.BlkRead(HdrInf^, HdrBuf * SizeOf(FilesHdrRecord));
                SetError(HdrF^.IOresult);

                IdxF^.Seek(HdrRec * SizeOf(FilesIdxRecord));
                IdxF^.BlkRead(IdxInf.Idx^, HdrBuf * SizeOf(FilesIdxRecord));
                SetError(IdxF^.IOresult);

                IdxInf.StartNr := HdrRec;
                IdxInf.EndNr := Pred(HdrRec + HdrBuf);

                CurIdx^ := IdxInf.Idx^[1];
                CurHdr^ := HdrInf^[1];
                CurRec  := HdrRec;
              end; { not in cache }

end; { proc. FdbFileObj }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FdbFileObj.ClearError;
begin
  TempError := 00;
end; { proc. ClearError }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FdbFileObj.SetError(I: Integer);
begin
  TempError := I;

  if I>00 then ResetCache;
end; { proc. SetError }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.GetError: Integer;
begin
  GetError := TempError;
end; { func. GetError }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.GetSize: Longint;
begin
  GetSize := CurHdr^.Size;
end; { func. GetSize }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.GetAreaNum: Word;
begin
  GetAreaNum := SaveAreaNum;
end; { func. GetAreaNum }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.GetUploader: String;
begin
  GetUploader := CurHdr^.Uploader;
end; { func. GetUploader }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.GetSizeStr: String;
var S: String;
begin
  Str(CurHdr^.Size, S);

  GetSizeStr := S;
end; { func. GetSizeStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.GetPassword: String;
begin
  GetPassword := CurHdr^.PassWord;
end; { func. GetPassword }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.GetKeyword(Index: Byte): String;
begin
  GetKeyword := Curhdr^.KeyWord[Index];
end; { func. GetKeyWord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.GetCrc32: Longint;
begin
  GetCrc32 := CurHdr^.Crc32;
end; { func. GetCrc32 }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.GetCost: Word;
begin
  GetCost := CurHdr^.Cost;
end; { func. GetCost }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.GetUploadDate: Longint;
begin
  GetUploadDate := CurHdr^.UploadDate;
end; { func. GetUploadDate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.GetFileDate: Longint;
begin
  GetFileDate := CurHdr^.FileDate;
end; { func. GetFileDate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.IsDeleted: Boolean;
begin
  IsDeleted := (CurHdr^.Attrib AND (1 SHL 0)) <> 00;
end; { func. IsDeleted }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.IsUnlisted: Boolean;
begin
  IsUnlisted := (CurHdr^.Attrib AND (1 SHL 1)) <> 00;
end; { func. IsUnlisted }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.IsFree: Boolean;
begin
  IsFree := (CurHdr^.Attrib AND (1 SHL 2)) <> 00;
end; { func. IsFree }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.IsNotAvail: Boolean;
begin
  IsNotAvail := (CurHdr^.Attrib AND (1 SHL 3)) <> 00;
end; { func. IsNotAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.IsLocked: Boolean;
begin
  IsLocked := (CurHdr^.Attrib AND (1 SHL 4)) <> 00;
end; { func. IsLocked }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.IsMissing: Boolean;
begin
  IsMissing := (CurHdr^.Attrib AND (1 SHL 5)) <> 00;
end; { func. IsMissing }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.IsNoTime: Boolean;
begin
  IsNoTime := (CurHdr^.Attrib AND (1 SHL 6)) <> 00;
end; { func. IsNoTime }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.GetLastDL: Longint;
begin
  GetLastDL := CurHdr^.LastDL;
end; { func. GetLastDL }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.GetTimesDL: Word;
begin
  GetTimesDL := CurHdr^.TimesDL;
end; { func. GetTimesDL }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.GetLongDescPtr: Longint;
begin
  GetLongDescPtr := CurHdr^.LongDescPtr;
end; { func. GetLongDescPtr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.GetShortName: String;
begin
  GetShortName := CurHdr^.Name;
end; { func. GetShortName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.GetFileName: String;
var TempStr : String;
    NumRead : NumReadType;
begin
  if IsComment then
    begin
      GetFileName := '';
      EXIT;
    end; { if }

  if CurHdr^.LfnPtr > 0 then
    begin
      LfnF^.Seek(CurHdr^.LfnPTR);
      NumRead := LfnF^.BlkRead(TempStr[1], SizeOf(TempStr) - 01);
      if LfnF^.IOResult > 00 then NumRead := 00;

      Tempstr[0] := Chr(NumRead);
      TempStr := TempStr + #00;

      GetFilename := Copy(TempStr, 1, Pos(#00, TempStr) - 01);
    end
     else GetFileName := CurHdr^.Name;
end; { func. GetFileName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.GetLfnPtr: Longint;
begin
  GetLfnPtr := CurHdr^.LfnPtr;
end; { func. GetLfnPtr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FdbFileObj.ReadDescription(var TextFile: pFileObj; LongDescPtr: Longint);
var NumRead    : NumReadtype;
    DidCache   : Boolean;
begin
  if LongDescPtr <> -1 then
    begin
      DidCache := false;

      if (LongDescPtr > TxtPtr) AND
          (LongDescPtr <= TxtPtr + (TxtSize)) AND
           (TxtPtr <> -1) AND (LineCfg^.CacheDesc) then
             begin
                Move(CurTxt^[LongDescPtr - TxtPtr], CurTxt^[0], SizeOf(CurTxt^) - (LongDescPtr - TxtPtr));

                Dec(TxtSize, (LongDescPtr - TxtPtr));
                Inc(TotalCached, LongDescPtr - TxtPtr);

                NumRead := TxtSize;
                TxtPtr := LongDescptr;

                if StrLen(CurTxt^) >= TxtSize then DidCache := false
                 else DidCache := true;
             end; { if }

      if NOT DidCache then
          begin
            TextFile^.Seek(LongDescPtr);
            NumRead := TextFile^.BlkRead(CurTxt^[0], SizeOf(TxtRec) - 01);

            TxtSize := NumRead;
            TxtPtr := LongDescPtr;
            TotalCached := 00;
          end; { if not cached }
    end else NumRead := 00;

  if NumRead>0 then Inc(NumRead);
  if NumRead>Pred(Sizeof(Txtrec)) then NumRead := SizeOf(TxtRec) - 01;
  if NumRead < 0 then NumRead := 0;

  CurTxt^[NumRead] := #00;

  ResetDescription;
end; { proc. ReadDescription }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FdbFileObj.ResetCache;
begin
  IdxInf.StartNr := -1;
  IdxInf.EndNr := -1;
  TxtPtr := -1;
end; { proc. FdbFileObj }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.GetPrevWord(ThisDescPtr: Longint; const MinValue: Longint): Longint;
var Start: Word;
begin
  GetPrevWord := 00;
  Start := ThisDescPtr;

  While (ThisDescPtr > MinValue) do
    begin
      if CurTxt^[ThisDescPtr] in AbortStrChars then
        begin
           GetPrevWord := Word(Start - ThisDescPtr);
           BREAK;
        end; { if }

      Dec(ThisDescPtr);
    end; { while }
end; { func. GetPrevWord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.EndOfDesc: Boolean;
begin
  if (InDescPtr <= SizeOf(CurTxt^)) AND (InDescPtr >= 0) then
    EndOfDesc := (CurTxt^[InDescPtr] = #00)
     else EndOfDesc := false;
end; { func. EndOfDesc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.GetDescLine(MaxLen: Byte): String;
var TempStr: String;
    PrevPtr: Word;
    SetBack: Word;
begin
  if MaxLen = 00 then MaxLen := 255;

  If IndescPtr < 00 then
    InDescPtr := 00;

  PrevPtr := InDescPtr;
  TempStr := '';

  if IsComment then
     begin
       TempStr := StrPas(CurTxt^);
       Inc(InDescPtr, StrLen(CurTxt^));
     end else
   begin
     While NOT (CurTxt^[InDescPtr] in [#10, #00]) do
      begin
        if CurTxt^[InDescPtr] in [#32..#255] then
          TempStr := TempStr + CurTxt^[InDescPtr];
        Inc(InDescPtr);

        If Length(TempStr) > MaxLen then
          begin
            SetBack := GetPrevWord(InDescPtr, PrevPtr);

            if SetBack > 00 then
              begin
                Delete(TempStr, Succ(Length(TempStr) - SetBack), 255);
                Dec(InDescPtr, SetBack + 01);

                if CurTxt^[InDescPtr + 01] = #32 then Inc(InDescPtr);
              end; { if }

            BREAK;
          end; { if }

      end; { while }

     if InDescPtr <> PrevPtr then
      if (NOT (CurTxt^[InDescPtr] in [#00])) then Inc(InDescPtr);

     if (CurTxt^[InDescPtr] = #10) then Inc(InDescPtr);
   end; { if not comment }

  GetDescLine := TempStr;
end; { func. GetDescLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.TotalRecords: Longint;
begin
  TotalRecords := HdrF^.FileSize div SizeOf(FilesHdrRecord);
end; { func. TotalRecords }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.GetAttrib: Longint;
begin
  GetAttrib := CurHdr^.Attrib;
end; { func. FdbFileObj.GetAttrib }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.CurrentRecord: Longint;
begin
  CurrentRecord := CurRec + 01;
end; { func. CurrentRecord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.IsComment: Boolean;
begin
  IsComment := CurHdr^.Name[0] = #00;
end; { func. IsComment }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FdbFileObj.GetDescBlock(var Txt: TxtRec);
var Size: Word;
begin
  Size := SizeOf(Txt);
  if SizeOf(Txt) > SizeOf(CurTxt^) then Size := SizeOf(CurTxt^);

  Move(CurTxt^, Txt, Size);
end; { proc. GetDescBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FdbFileObj.ResetDescription;
begin
  InDescPtr := -1;
end; { proc. ResetDescription }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FdbFileObj.CreateNewTxt(AreaNum: Word);
var TempF: pFileObj;
begin
  New(TempF, Init);

  TempF^.Assign(GetFileBasePath('txt', AreaNum));
  TempF^.FileMode := ReadWriteMode + DenyNone;
  TempF^.OpenOrCreate(1);

  Dispose(TempF, Done);

  FileMode := FileBaseFileMode;
end; { proc. CreateNewTxt }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FdbFileObj.CreateNewIdx(AreaNum: Word);
var TempF: pFileObj;
begin
  New(TempF, Init);

  TempF^.Assign(GetFileBasePath('idx', AreaNum));
  TempF^.FileMode := ReadWriteMode + DenyNone;
  TempF^.OpenOrCreate(1);

  Dispose(TempF, Done);

  FileMode := FileBaseFileMode;
end; { proc. CreateNewIdx }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FdbFileObj.CreateNewHdr(AreaNum: Word);
var TempF: pFileObj;
begin
  New(TempF, Init);

  TempF^.Assign(GetFileBasePath('hdr', AreaNum));
  TempF^.FileMode := ReadWriteMode + DenyNone;
  TempF^.OpenOrCreate(1);

  Dispose(TempF, Done);

  FileMode := FileBaseFileMode;
end; { proc. CreateNewHdr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FdbFileObj.CreateFileBase(AreaNum: Word);
begin
  CreateNewHdr(AreaNum);
  CreateNewTxt(AreaNum);
  CreateNewIdx(AreaNum);
end; { proc. CreateFileBase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FdbFileObj.MakeUpFName(S: String): String;
var Counter: Byte;
begin
  For Counter := 01 to Length(s) do
    S[Counter] := UpCase(S[Counter]);

  While (Length(S)>00) AND (S[1]=#32) do Delete(S, 1, 1);
  While (Length(S)>00) AND (S[Length(S)]=#32) do Delete(S, Length(S), 1);

  MakeUpFName := S;
end; { func. MakeUpName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.  { unit RA2FDB }
