unit TagObj;
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
** Tagfile routines for EleBBS
**
** Copyright (c) 1998 by Maarten Bekers
**
** Created : 26-Apr-1998
** Last update : 26-Apr-1998
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses CfgRec, FileObj;

Const MaxTagged =  100;

type tTagFileObj = Object
          TagFileF   : pFileObj;

          constructor Init;
          destructor Done;

          procedure OpenTagFile;
          procedure CloseTagFile;
          procedure ClearTagList;

          procedure GetTagHeader(RecNr: Longint; var TagInfo: TagFileRecord);
          procedure SetTagHeader(RecNr: Longint; var TagInfo: TagFileRecord);
          procedure MakeUpName(var S: String);
          function CurrentTagged: Longint;
          function GetTotalKbsTagged: LongInt;
          function IsTaggedFile(FName: String; const AreaNum: Word): Boolean;
          function AddToTagFile(HdrInf   : FilesHdrRecord;
                                AreaNum  : Word;
                                RecordNum: Word;
                                Area_Attr: Byte;
                                FileName : String): Boolean;

          procedure DeleteFileFromTagged(FName: String);
          procedure DeleteTagNumber(RecNr: Longint);
     end; { tTagFileObj }

type pTagFileObj = ^tTagFileObj;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 uses Global, Cases, CentrStr, FileRout, BitWise, RAL, GenFile,
       TagUnit, Ra2Fdb, LongStr, CfgFile, ObjDec,
       {$IFDEF WITH_FULL}
         Support,
         Transfer,
       {$ENDIF}

       {$IFDEF WITH_DEBUG}
         Debug_U,
       {$ENDIF}

       {$IFDEF WIN32}
         SysUtils
       {$ENDIF}

       {$IFDEF MSDOS}
         Dos
       {$ENDIF}

       {$IFDEF OS2}
         Dos
       {$ENDIF}

       {$IFDEF ELEUNIX}
         Dos
       {$ENDIF}

       {$IFDEF GO32V2}
         Dos
       {$ENDIF};
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tTagFileObj.Init;
begin
  TagFileF := NIL;

  OpenTagFile;
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tTagFileObj.Done;
begin
  CloseTagFile;
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tTagFileObj.CurrentTagged: Longint;
begin
  CurrentTagged := TagFileF^.FileSize DIV SizeOf(TagFileRecord);
end; { func. CurrentTagged }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTagFileObj.MakeUpName(var S: String);
begin
  S := SUpCase(S);
  S := Trim(S);
end; { proc. MakeUpName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTagFileObj.OpenTagFile;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'tTagFileObj - OpenTagFile');
  {$ENDIF}

  if TagFileF <> NIL then
    CloseTagFile;

  New(TagFileF, Init);
  TagFileF^.Assign(TagListFileName);
  TagFileF^.FileMode := ReadWriteMode + DenyNone;
  if NOT TagFileF^.OpenOrCreate(1) then
    begin
      Config_ShowError('Unable to open/create file', TagListFileName, 2);
    end; { if }
end; { proc. OpenTagFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTagFileObj.CloseTagFile;
begin
  if TagFileF <> nil then
    Dispose(TagFileF, Done);
  TagFileF := NIL;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'tTagFileObj - CloseTagFile');
  {$ENDIF}
end; { proc. CloseTagFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTagFileObj.ClearTagList;
begin
  CloseTagFile;
  EraseFile(TagListFileName);
  OpenTagFile;
end; { proc. tTagFileObj }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTagFileObj.DeleteFileFromTagged(FName: String);
var WildSearch: Boolean;
    Counter   : Longint;
    TagInfo   : TagFileRecord;
    TempStr   : String;
begin
  if FName='' then EXIT;
  MakeUpName(FName);

  WildSearch := FALSE;
  If Pos('?', FName) > 00 then WildSearch := TRUE;
  If Pos('*', FName) > 00 then WildSearch := TRUE;

  For Counter := 01 to CurrentTagged do
   begin
     GetTagHeader(Pred(Counter), TagInfo);

     TempStr := GetLfnName(TagInfo.Name, TagInfo.RecordNum, TagInfo.AreaNum);
     MakeUpName(TempStr);

     {$IFDEF WITH_DEBUG}
        DebugObj.DebugLog(logTransfer, 'DeleteFromTagged = ' + TagInfo.Name);
        DebugObj.DebugLog(logTransfer, 'DeleteFromtagged (2) = ' + TempStr);
     {$ENDIF}

     if WildSearch then
      if IsWildCard(FName, TempStr) then
        begin
          DeleteTagNumber(Pred(Counter));
          BREAK;
        end; { Include }

     if NOT WildSearch then
      if FName = TempStr then
       begin
          DeleteTagNumber(Pred(Counter));
          BREAK;
       end; { Include }
   end; { for }
end; { proc. DeleteFileFromTagged }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTagFileObj.GetTagHeader(RecNr: Longint; var TagInfo: TagFileRecord);
var NumRead: NumReadType;
    DidRead: Boolean;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logTransfer, 'GetTagHeader - RecNr = '+FStr(RecNr));
     DebugObj.DebugLog(logTransfer, 'GetTagHeader - Curr  = '+FStr(TagFileF^.FileSize));
     DebugObj.DebugLog(logTransfer, 'GetTagHeader - Grmph = '+FStr(SizeOf(TagFileRecord)));
  {$ENDIF}

  if (Longint(RecNr) * SizeOf(TagFileRecord)) > TagFileF^.FileSize then
    DidRead := false
     else begin
            {$IFDEF WITH_DEBUG}
              DebugObj.DebugLog(logTransfer, 'GetTagHeader - whileloop= true');
              DebugObj.DebugLog(logTransfer, 'GetTagHeader - SeekFile = ' + FStr(Longint(RecNr) * SizeOf(TagFileRecord)));
            {$ENDIF}

            TagFileF^.Seek(Longint(RecNr) * SizeOf(TagFileRecord));
            {$IFDEF WITH_DEBUG}
              DebugObj.DebugLog(logTransfer, 'GetTagHeader - FilePos  = ' + FStr(TagFileF^.FilePos));
            {$ENDIF}

            NumRead := TagFileF^.BlkRead(TagInfo, SizeOf(TagFileRecord));
            DidRead := (NumRead <> 0);
          end; { if }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'GetTagHeader - IORes   = ' + FStr(TagFileF^.IoResult));
    DebugObj.DebugLog(logTransfer, 'GetTagHeader - TagInfo = ' + TagInfo.Name);
    DebugObj.DebugLog(logTransfer, 'GetTagHeader - NumRead = ' + FStr(NumRead));
  {$ENDIF}


  if (NumRead <> SizeOf(TagFileRecord)) OR (NOT DidRead) then
   begin
     FillChar(TagInfo, SizeOf(TagFileRecord), #00);
     Taginfo.FoundFirst := false;
   end; { if }
end; { proc. GetTagHeader }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTagFileObj.SetTagHeader(RecNr: Longint; var TagInfo: TagFileRecord);
begin
  TagFileF^.Seek(Longint(RecNr) * SizeOf(TagFileRecord));
  TagFileF^.BlkWrite(TagInfo, SizeOf(TagFileRecord));
end; { proc. SetTagHeader }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTagFileObj.DeleteTagNumber(RecNr: Longint);
var TempF    : pFileObj;
    TagInfo  : TagFileRecord;
    RaNodeStr: String;
    Counter  : Longint;
begin
  Str(LineCfg^.RaNodeNr, RaNodeStr);

  New(TempF, Init);
  TempF^.Assign('TAGTMP$.' + RaNodeStr);
  TempF^.FileMode := ReadWriteMode + DenyAll;

  if NOT TempF^.Create(SizeOf(TagFileRecord)) then EXIT;

  if CurrentTagged > 00 then
   For Counter := 01 to CurrentTagged do
     begin
        GetTagHeader(Pred(Counter), TagInfo);

        if (Counter-01) <> RecNr then
         if TagInfo.Name <> '' then
           TempF^.BlkWrite(TagInfo, 1);
      end; { while }

  TempF^.Close;

  CloseTagFile;
  EraseFile(TagListFileName);
  RenameObjWithDrive(TempF, TagListFileName);

  Dispose(TempF, Done);

  OpenTagFile;
end; { proc. tTagFileObj }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tTagFileObj.AddToTagFile(HdrInf   : FilesHdrRecord;
                                 AreaNum  : Word;
                                 RecordNum: Word;
                                 Area_Attr: Byte;
                                 FileName : String): Boolean;
var TagInfo: TagFileRecord;
begin
  AddToTagFile := true;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'AddToTagfile - FileName = '+FileName);
    DebugObj.DebugLog(logTransfer, 'AddToTagFile - HdrInf   = '+HdrInf.Name);
    DebugObj.DebugLog(logTransfer, 'AddToTagfile - Record#  = '+FStr(RecordNum));
  {$ENDIF}

  MakeUpName(HdrInf.Name);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'AddToTagFile - HdrInf.Name (2) = '+HdrInf.Name);
    DebugObj.DebugLog(logTransfer, 'AddToTagFile - MaxTagged       = '+FStr(MaxTagged));
    DebugObj.DebugLog(logTransfer, 'AddToTagFile - CurrentTagged   = '+FStr(CurrentTagged));
  {$ENDIF}

  if CurrentTagged > MaxTagged then        { Maximum number of tags allowed }
    begin
      AddToTagFile := false;
      EXIT;
    end; { if }

  if HdrInf.Name = '' then EXIT;
  if IsTaggedFile(FileName, AreaNum) then EXIT;

  if ReadBit(HdrInf.Attrib, 03) then
    begin
      {$IFDEF WITH_FULL}
        Write('`A15:', Filename, #32, LangObj^.ralGet(ralNotAvail3));
        Delay(1000);
        Write('`X1:');
      {$ENDIF}
      EXIT;
    end; { File is not available }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'AddToTagFile - Final Stage='+HdrInf.Name);
  {$ENDIF}

  Fillchar(TagInfo, SizeOf(TagFileRecord), #00);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'AddToTagFile - After Final='+HdrInf.Name);
  {$ENDIF}

  TagInfo.Name       := HDRInf.Name;
  TagInfo.Password   := Trim(HDRInf.PassWord);
  TagInfo.Attrib     := HDRInf.Attrib;
  TagInfo.AreaNum    := AreaNum;
  TagInfo.RecordNum  := RecordNum;
  TagInfo.Size       := HDRInf.Size;
  TagInfo.FileDate   := HDRInf.FileDate;
  TagInfo.Cost       := HDRInf.Cost;
  TagInfo.CDRom      := ReadBit(Area_Attr, 3);
  TagInfo.FoundFirst := false;
  TagInfo.XFerTime   := 00;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'AddToTagFile - FterFinal = '+TagInfo.Name);
  {$ENDIF}

  {$IFDEF WITH_FULL}
    If NOT DownloadRestrictions(true) then EXIT;
  {$ENDIF}

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'AddToTagFile - InFinal   = '+TagInfo.Name);
  {$ENDIF}

  TagFileF^.Seek(Longint(CurrentTagged) * SizeOf(TagFileRecord));
  TagFileF^.BlkWrite(TagInfo, SizeOf(TagFileRecord));
end; { func. AddToTagFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tTagFileObj.IsTaggedFile(FName: String; const AreaNum: Word): Boolean;
var Counter  : Longint;
    TagInfo  : TagFileRecord;
    TempStr  : String;
begin
  MakeUpName(FName);

  IsTaggedFile := false;

  for Counter := 01 to CurrentTagged do
    begin
      GetTagHeader(Pred(Counter), TagInfo);

      TempStr := GetLfnName(TagInfo.Name, TagInfo.RecordNum, TagInfo.AreaNum);
      MakeUpName(TempStr);


      if TempStr = FName then
       if TagInfo.AreaNum = AreaNum then
        begin
          IsTaggedFile := true;
          BREAK;
        end; { if }
    end; { for }

end; { func. IsTaggedFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tTagFileObj.GetTotalKbsTagged: LongInt;
var TotalSize: Longint;
    Counter  : Longint;
    TagInfo  : TagFileRecord;
begin
  TotalSize := 00;

  for Counter := 01 to CurrentTagged do
   begin
      GetTagHeader(Pred(Counter), TagInfo);

      If NOT ReadBit(TagInfo.Attrib, 2) then
           Inc(TotalSize, TagInfo.Size);
   end; { for }

  GetTotalKbsTagged := TotalSize div 1024;
end; { func. GetTotalKbsTagged }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit TabObj }
