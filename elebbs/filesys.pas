unit FileSys;
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
** The FileSystem of EleBBS
**
** Copyright (c) 1996-1999 by Maarten Bekers
**
** Created : 06-Nov-1996
** Last update : 30-Dec-1998
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 Uses Global, CfgRec, FileObj;
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

type AreaEditActivateProc= procedure(FName: DelStr; Row: Integer; var Close: Boolean; var Hdr_F, Txt_F: File; AreaNum: Word);

procedure CreateIdxBaseProc(var CurHDR: CfgRec.FilesHdrRecord;
                            var CurIDX: CfgRec.FilesIdxRecord;
                            var HdrF: pFileObj;
                            var IdxF: pFileObj;
                            LfnName: String;
                            RecNum : Longint);
procedure MakeSpecificTagList(MiscData: String; FreeFile: Boolean);
procedure UnLockFileBase(var Hdr: pFileObj);
procedure DirectDirectoryListing(MiscData: String; fattach: Boolean);
procedure DumpBinaryFileToScreen(Path: String);
function  AddToFdb(FileName: String; FileSize: Longint; AreaNum: Word; var FirstTime: Boolean): Boolean;

function  LockFileBase(var Hdr: pFileObj): Boolean;
function  SearchNameInArea(AreaNum: Longint; FName: String; DoWild: Boolean): Longint;
function  InsertInHdr(const AreaNum, Row, NrRecs: Word;
                      const HdrInfs: Array of FilesHdrRecord): fdb_ResultCodeType;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses BitWise,
       GenFile,
        Ra2Fdb,
         Debug_U,
          LongStr,
           Cases,
            Crc_Unit,
             MkFile,
              FileRout,
               Ral,
                StrPath,
                 ElLog_U,
                  Desc_U,
                   ElFIle_U,
                    MgrFdb,
                     GenDos,
                      Ranges,
                       StrUnit,
                        ObjDec,
                         StUtils,

             {$IFDEF WITH_FULL}
               TagUnit,
               Input_U,
               LineEd,
               Control,
               Transfer,
               IntEdit,
               Centrstr,
             {$ENDIF}

             {$IFDEF MSDOS}
               Dos,
             {$ENDIF}

             {$IFDEF GO32V2}
              Dos,
             {$ENDIF}

             {$IFDEF ELEUNIX}
              Dos,
             {$ENDIF}

             {$IFDEF WIN32}
               SysUtils,
               {$IFNDEF DELPHI}
                 Dos,
               {$ELSE}
                 JDates,
               {$ENDIF}
             {$ENDIF}


             {$IFDEF OS2}
               Dos,
             {$ENDIF} WordStr, Memman;

Type HdrRecArray = Array[1..101] of FilesHdrRecord;

var Ins_HdrInf   : ^HdrRecArray;
    Ins_NrRecs   : Word;
    Ins_InsertPos: Word;
    Ins_DidCopy  : Boolean;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CreateIdxBaseProc(var CurHDR: CfgRec.FilesHdrRecord;
                            var CurIDX: CfgRec.FilesIdxRecord;
                            var HdrF: pFileObj;
                            var IdxF: pFileObj;
                            LfnName: String;
                            RecNum : Longint);
var Mgr: MgrFileObj;
begin
  Mgr.Header2Idx(CurHDR, CurIDX);

  {$i-}
    HdrF^.BlkWrite(CurHDR, SizeOf(FilesHdrRecord));
    IdxF^.BlkWrite(CurIDX, SizeOf(FilesIdxRecord));
  {$i+}
  if IOResult>00 then ;
end; { proc. CreateIdxBaseProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure MakeSpecificTaglist(MiscData: String; FreeFile: Boolean);
var AreaNum    : Word;
    AreaObj    : FdbFileObj;
    Counter    : Longint;
    FilesInf   : FilesRecord;
    TempHDR    : FilesHdrRecord;

    TempName   : String;
    TempPath   : String;

    TotalItems : Longint;
    ItemCounter: Longint;

    DirInfo    : SearchRec;

    FilesAdded : Longint;
    OldAdded   : Longint;
    ShowedHDR  : Boolean;
    TempStr    : String;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileSys, 'MakeSpecificTagList (MiscData='+MiscData+'), (begin)');
  {$ENDIF}

  OldAdded := 1;
  ShowedHDR := true;
  FilesAdded := 0;
  TempStr := '';

  {--------------- An area number specified on the commandline ----------------}
  AreaNum := FVal(GetValue('/A=', MiscData, False));
  if AreaNum > 00 then
    begin
      GetFilesRecord(FilesInf, AreaNum, false);
      AreaObj.Init(AreaNum, false);

      TotalItems  := AreaObj.TotalRecords;
      ItemCounter := 00;

      While (ItemCounter < TotalItems) AND (TotalItems>00) do
        begin
          AreaObj.Read(ItemCounter);               { Read header sequentially }
          Inc(ItemCounter);

          for Counter := 01 to WordCount(MiscData, DefExtractWord, true) do
            begin
              TempName := ExtractWord(MiscData, Counter, defExtractWord, true, false);

              {$IFDEF WITH_DEBUG}
                DebugObj.DebugLog(logTransfer, 'MakeSpecificTagList - CDROM part (begin)');
                DebugObj.DebugLog(logTransfer, 'MakeSpecificTagList - FilesInf.Name = '+FilesInf.Name);
                DebugObj.DebugLog(logTransfer, 'MakeSpecificTagList - FilesInf.CDROM= '+
                  Bool2Str(ReadBit(FilesInf.Attrib, 3)));
                DebugObj.DebugLog(logTransfer, 'MakeSpecificTagList - CDROM part ( end )');
              {$ENDIF}

              {$IFDEF WITH_FULL}
               if IsWildCard(TempName, AreaObj.GetFileName) then
                if TaggingObj^.AddToTagFile(AreaObj.CurHdr^, AreaNum, AreaObj.CurrentRecord,
                                         FilesInf.Attrib, AreaObj.GetFileName) then ;

(*
                if NOT AskToAdd(FilesInf, AreaObj.CurHdr^,
                                ShowedHDR, AreaObj.CurrentRecord - 1, AreaObj.GetFileName,
                                0, TempStr, AreaNum, AreaObj.GetFileName, FilesAdded) then BREAK;
*)
              {$ENDIF}
            end; { for }
        end; { while }

      AreaObj.Done;
    end; { if }

  {------------- No area number was specified on the commandline --------------}
  if AreaNum = 00 then
   While (MiscData <> '') do
    begin
      TempName := FirstWord(MiscData, defExtractWord, true);
      TempPath := JustPath(TempName);

      if TempPath = '' then TempPath := GetCurDir;

      FindFirst(TempName, AnyFile - VolumeID, DirInfo);

      While (DosError = 00) do
        begin
          if Dirinfo.Attr AND Directory = 0 then
             begin
               FillChar(TempHDR, SizeOf(FilesHdrRecord), #00);

               TempHDR.Name := Dirinfo.Name;
               TempHDR.Size := Dirinfo.Size;
               if FreeFile then
                  SetBit(TempHDR.Attrib, 2);                { Set to free-file }

              {$IFDEF WITH_DEBUG}
                DebugObj.DebugLog(logTransfer, 'MakeSpecificTagList - CDROM part (begin)');
                DebugObj.DebugLog(logTransfer, 'MakeSpecificTagList - FilesInf.Name = '+FilesInf.Name);
                DebugObj.DebugLog(logTransfer, 'MakeSpecificTagList - FilesInf.CDROM= '+
                   Bool2Str(ReadBit(FilesInf.Attrib, 3)));
                DebugObj.DebugLog(logTransfer, 'MakeSpecificTagList - CDROM part ( end )');
              {$ENDIF}

               {$IFDEF WITH_FULL}
                OldAdded := FilesAdded;

                if NOT AskToAdd(FilesInf, TempHDR,
                                ShowedHDR, 0, '',
                                0, TempStr, 0, Dirinfo.Name, FilesAdded) then BREAK;

                 if OldAdded <> FilesAdded then
                   LineCfg^.TagPaths^.Put(TaggingObj^.CurrentTagged, ForceBack(TempPath));

                 OldAdded := FilesAdded;
               {$ENDIF}
             end; { if not ReadBit }

          FindNext(Dirinfo);
        end; { while }

      FindClose(DirInfo);
    end; { AreaNum = 00 }



  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileSys, 'MakeSpecificTagList (MiscData='+MiscData+'), ( end )');
  {$ENDIF}
end; { proc. MakeSpecificTagList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure InsertFilesProc(var CurHDR: CfgRec.FilesHdrRecord;
                          var CurIDX: CfgRec.FilesIdxRecord;
                          var HdrF: pFileObj;
                          var IdxF: pFileObj;
                          LfnName: String;
                          RecNum : Longint);
var Counter: Byte;
    TmpIDX : CfgRec.FilesIdxRecord;
    MgrObj : MgrFileObj;
begin
  if Pred(Ins_InsertPos) = (HdrF^.FilePos div SizeOf(FilesHdrRecord)) then
   begin
     Ins_DidCopy := true;

     {$i-}
       HdrF^.BlkWrite(Ins_HdrInf^[1], SizeOf(FilesHdrRecord) * Ins_NrRecs);
       if IOResult>00 then ;

       for Counter := 1 to Ins_NrRecs do
         begin
           MgrObj.Header2Idx(Ins_HdrInf^[Counter], TmpIDX);

           {$I-} IdxF^.BlkWrite(TmpIDX, SizeOf(FilesIDXRecord)); {$i+}
           if IOresult>00 then ;
         end; { for }

     {$i+}
     if IOResult>00 then ;
   end; { if }

  {$i-}
    HdrF^.BlkWrite(CurHDR, SizeOf(FilesHdrRecord));
    IdxF^.BlkWrite(CurIDX, SizeOf(FilesIdxRecord));
  {$i+}
  if IOResult>00 then ;
end; { proc. InsertFilesProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function InsertInHdr(const AreaNum, Row, NrRecs: Word;
                     const HdrInfs: Array of FilesHdrRecord): fdb_ResultCodeType;
var FileMgr: MgrFileObj;
    Counter: Byte;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'InsertInHdr (begin)');
  {$ENDIF}

  InsertInHdr := FDB_OK;

  GetFilesRecord(GlobalAreaFileInfo, AreaNum, false);

  if NOT AllocMem(Ins_HdrInf, SizeOf(HdrRecArray), 'HdrRecArray', 'InsertInHdr') then
    begin
      InsertInHdr := FDB_NoMemory;
      EXIT;
    end; { if }


  Move(HdrInfs, Ins_HdrInf^, SizeOf(HdrInfs));
  Ins_NrRecs    := NrRecs;
  Ins_InsertPos := Row;
  Ins_DidCopy   := false;

  FileMgr.Init(AreaNum, True);

  InsertInHdr := FileMgr.FileBaseMaintenance(AreaNum,
                                             {$IFDEF FPC}@{$ENDIF}InsertFilesProc,
                                             false);

  if NOT Ins_DidCopy then
    begin
      for Counter := 01 to Ins_NrRecs do
        FileMgr.AddHeader(Ins_HdrInf^[Counter]);
    end; { if }

  FileMgr.Done;

  ReleaseMem(Ins_HdrInf, SizeOf(HdrRecArray));

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'InsertInHdr ( end )');
  {$ENDIF}
end; { proc. InsertInHdr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function SearchNameInArea(AreaNum: Longint; FName: String; DoWild: Boolean): Longint;
var AreaObj    : FdbFileObj;

    TotalItems : Longint;
    ItemCounter: Longint;

    WildSearch : Boolean;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'Searching for filename in area: '+FStr(AreaNum)+'#: '+FName + ' (begin)');
  {$ENDIF}

  SearchNameInArea := -1; 
  FName := SUpCase(FName);
  WildSearch := FALSE;
  
  if DoWild then 
    begin
      if (Pos('?', FName) > 0) then 
        begin
          WildSearch := TRUE;
        end
          else if Pos('*', FName) > 00 then 
            begin
              WildSearch := TRUE;
            end; { if }
    end; { if }

  AreaObj.Init(AreaNum, false);
  if AreaObj.GetError <> 00 then
    begin
      AreaObj.done;
      EXIT;
    end; { if }

  TotalItems := AreaObj.TotalRecords;
  ItemCounter:= 00;

  While (ItemCounter < TotalItems) AND (TotalItems > 00) do
    begin
      AreaObj.Read(ItemCounter);
      Inc(ItemCounter);

      Case WildSearch of
        TRUE : if IsWildCard(FName, SUpCase(AreaObj.GetFileName)) or (IsWildCard(FName, SUpCase(AreaObj.GetShortName))) then
                begin
                  SearchNameInArea := ItemCounter;
                  BREAK;
                end; { if }
        FALSE: if (FName = SUpCase(AreaObj.GetFileName)) or (FName = SUpCase(AreaObj.GetShortName)) then
                begin
                  SearchNameInArea := ItemCounter;
                  BREAK;
                end; { if }
      end; { case }
    end; { while }

  AreaObj.Done;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'Searching for filename in area: '+FStr(AreaNum)+'#: '+FName + ' ( end )');
  {$ENDIF}
end; { func. SearchNameInArea }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function LockFileBase(Var Hdr: pFileObj): Boolean;
var Temp      : Integer;
    UserAbort : Boolean;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'LockFileBase (begin)');
  {$ENDIF}

  repeat
    Temp := ShLock(Hdr, 0, SizeOf(FilesHdrRecord));

    UserAbort := False;

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

  until (Temp=00) OR (UserAbort);

  LockFileBase := (Temp=00);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'LockFileBase ( end )');
  {$ENDIF}
end; { func. LockFileBase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure UnLockFileBase(var Hdr: pFileObj);
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'Unlocking filebase (begin)');
  {$ENDIF}

  MkFile.shUnlock(Hdr, 00, SizeOf(FilesHdrRecord));

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'Unlocking filebase ( end )');
  {$ENDIF}
end; { proc. UnlockFileBase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DirectDirectoryListing(MiscData: String; fattach: Boolean);
var TempStr   : String;
    FilesInf  : FilesRecord;
    DirInfo   : SearchRec;
    NrFiles,
    NrBytes   : LongInt;
    DT        : DateTime;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'Direct directory listing (begin)');
  {$ENDIF}

{$IFDEF WITH_FULL}
  if NOT FAttach then
    begin
      WriteLn;
      Writeln;
      Write('`A10:', LangObj^.ralGet(ralPattern));
    end;

  if Fattach then
    begin
      OutputObj^.ClearScreen;
      WriteLn('`A10:', langObj^.ralget(ralFilesAtt));
      WriteLn('`A15:');
    end; { if }

  if Pos('/F', SUpcase(MiscData))>00 then
      GetFilesRecord(FilesInf, LineCfg^.Exitinfo^.Userinfo.FileArea, True)
       else GetFilesRecord(FilesInf, Word(FVal(MiscData)), True);

  if NOT FAttach then
    begin
      TempStr := '';
      GetString(TempStr, 12, [#32..#254], False, False, False, False);
   end
    else TempStr := ForceBack(MiscData) + '*.*';

  If TempStr='' then TempStr := '*.';

  if NOT FAttach then
    TempStr := FilesInf.FilePath+TempStr+'*';

  NrFiles := 00;
  NrBytes := 00;

  if NOT FAttach then OutputObj^.ClearScreen;
  Writeln;
  WriteLn('`A14:', LangObj^.ralGet(ralFileHdr));
  Writeln('`A14:-------------- ----------- -----------');

  FindFirst(TempStr, AnyFile - VolumeID, DirInfo);

  While (DosError = 00) AND (NOT OutputObj^.StopMore) do
    begin
      Inc(NrFiles);
      Inc(NrBytes, DirInfo.Size);

      UnpackTime(DirInfo.Time, DT);

      if Dirinfo.Attr AND Directory = 0 then
          begin
            With DT, DirInfo do
              Writeln('`A11:',      MakeLen(Name, 14, Space, False, false), #32,
                      '`A10:`X16:', MakeLen(FStr(Size),11, Space, False, false),
                      '`A9:`X28:',  LangObj^.RaFormatDate(LeadingZero(Month, 2) + '-' +
                                                 LeadingZero(Day, 2) + '-' +
                                                 Copy(LeadingZero(Year, 4), 3, 2), 0, y2k_FileDate,
                                                 LineCfg^.Exitinfo^.Userinfo.DateFormat));
          end; { No Directory }

      FindNext(DirInfo);
    end; { DosError = 00 }

  FindClose(DirInfo);

  Writeln('`A14:-------------- ----------- ---------');
  WriteLn('`A15:',NrFiles,' ',LangObj^.ralGet(ralFiles1), '`X16:', NrBytes,' ', LangObj^.ralGet(ralBytes));
  WriteLn;
  InputObj^.PressEnter(False, True);
  OutputObj^.SetStopMore(False);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'Direct directory listing ( end )');
  {$ENDIF}
{$ENDIF}
end; { proc. DirectDirectoryListing }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DumpBinaryFileToScreen(Path: String);
var Tmp_F  : pFileObj;
    TmpBuf : ShortString;
    NumRead: NumReadType;
begin
  {-- Open the file -------------------------------------------------------}
  New(Tmp_F, Init);
  Tmp_F^.Assign(Path);
  Tmp_F^.FileMode := ReadMode + DenyNone;
  Tmp_F^.Open(1);

  {-- and actually dump the fiel to disk ----------------------------------}
  while (NOT Tmp_F^.EOF) AND (Tmp_F^.IoResult = 0) do
    begin
      NumRead := Tmp_F^.BlkRead(TmpBuf[1], 250);
      TmpBuf[0] := Chr(NumRead);
      Write(TmpBuf);
    end; { while }

  {-- and close the file --------------------------------------------------}
  Dispose(Tmp_F, Done);
end; { proc. DumpBinaryFileToScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function AddToFdb(FileName: String; FileSize: Longint; AreaNum: Word; var FirstTime: Boolean): Boolean;
var FilesInf   : FilesRecord;
    EditorInfo : pStringArrayObj;
    TempStr    : String;
    TotLines   : Longint;
    PwdStr     : String;
    HdrInf     : FilesHdrRecord;
    DT         : DateTime;
    Counter    : longint;
    DOW        : Word;
{$IFDEF WITH_FULL}
    AreaObj    : MgrFileObj;
{$ENDIF}
    TxtPtr     : Longint;
begin
{$IFDEF WITH_FULL}
  AddToFdb := false;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileSys, 'ADDTOFDB (begin)');
   {$ENDIF}

  if NOT AcceptUploadedFile(JustName(FileName)) then
       EXIT;

  {-------------- Initialize all the variables we need ------------------------}
  New(EditorInfo, Init(MaxMsgLines));
  GetFilesRecord(FilesInf, AreaNum, True);
  PwdStr   := '';

  {---------------------- Ask the user for a description ----------------------}
  WriteLn('`A12:');

  if FirstTime then
    begin
      WriteLn(langObj^.ralGet(ralDescr));
      InputObj^.PressEnter(False, False);
      FirstTime := true;
    end; { if }

  repeat
    TotLines := 01;

    if ReadBit(FilesInf.Attrib, 2) then             { Allow long descriptions }
      begin
        repeat
          TotLines := 20;
          RaAlikeEditor(LangObj^.ralGet(ralPlsDesc) + #32 + FileName,
                        TotLines, EditorInfo^, 00)
        until (TotLines > 0) AND (Trim(EditorInfo^.Get(01)) <> '');
      end; { if }

    if NOT ReadBit(FilesInf.Attrib, 2) then
      begin
        repeat
          WriteLn;

          Write('`A2:', LangObj^.ralGet(ralPlsDesc) + #32 + '`A3:',
                MakeLen(FileName, 14, Space, false, false), '`A15::');

          TempStr := EditorInfo^.Get(01);
          GetString(TempStr, 40, [#32..#255], false, false, false, false);
          EditorInfo^.Put(1, TempStr);

        until Trim(EditorInfo^.Get(01)) <> '';
      end; { ShortDesc }

  until (TotLines > 00);

  {--------------------- Ask the user for a password --------------------------}
  if ReadBit(FilesInf.Attrib, 6) then
    if InputObj^.ralStrYesNoAsk(ralPswProtF) then
      begin
        if ProgTerminated then EXIT;
        WriteLn;
        Write(LangObj^.ralGet(ralPassword));

        GetString(PwdStr, 15, [#32..#254], False, True, False, False);
      end; { PassWord Protect this file }


  Inc(LineCfg^.Exitinfo^.Userinfo.Uploads);
  Inc(LineCfg^.Exitinfo^.Userinfo.UploadsK, FileSize DIV 1024);

  {--------------------- Actually add the file to the filedatabase ------------}
  FillChar(HdrInf, SizeOf(FilesHdrRecord), #00);

  if GlobalCfg^.RaConfig^.FixUploadDates then
    SetFileTime(FilesInf.FilePath+FileName);

  GetDate(DT.Year, Dt.Month, DT.Day, Dow);
  GetTime(DT.Hour, DT.Min, DT.Sec, Dow);
  PackTime(DT, HdrInf.LastDl);
  PackTime(Dt, HdrInf.UploadDate);

  HdrInf.Name       := FileName;
  HdrInf.Size       := FileSize;
  HdrInf.Crc32      := -1;
  HdrInf.Uploader   := LineCfg^.Exitinfo^.Userinfo.Name;
  HdrInf.FileDate   := GetPackedFileTime(FilesInf.FilePath + FileName);
  HdrInf.Password   := PwdStr;
  HdrInf.Cost       := FilesInf.DefCost;
  HdrInf.LongDescPtr:= -1;
  AreaObj.Init(AreaNum, true);
  HdrInf.LfnPtr     := AreaObj.AddLfnPtr(Filename, FilesInf.FilePath + FileName, HdrInf.Name);
  AreaObj.Done;

  TempStr := Editorinfo^.Get(1);
  if TempStr[1] = '/' then                                   { SysOpOnly File }
    begin
      SetBit(HdrInf.Attrib, 1);                                  { Unlisted }
      SetBit(HdrInf.Attrib, 3);                             { Not Available }

      TempStr := EditorInfo^.Get(1);
      Delete(TempStr, 1, 1);                           { Remove the 1st slash }
      EditorInfo^.Put(1, TempStr);
    end; { SysOp Only }

  {----------------- Set the description and add the file ---------------------}
  AreaObj.Init(AreaNum, true);

  FillChar(AreaObj.CurTxt^, SizeOf(AreaObj.CurTxt^), #00);
  TxtPtr := 00;
  for Counter := 01 to TotLines do
    begin
      TempStr := EditorInfo^.Get(Counter);

      Move(TempStr[1], AreaObj.CurTxt^[TxtPtr], Length(TempStr));
      Inc(TxtPtr, Length(TempStr));
    end; { for }

  HdrInf.LongDescPtr := AreaObj.AddDescription(AreaObj.CurTxt^);
  AreaObj.AddHeader(HdrInf);
  AreaObj.Done;


  Dispose(EditorInfo, Done);
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileSys, 'ADDTOFDB ( end )');
   {$ENDIF}

{$ENDIF}
  AddToFdb := true;
end; { proc. AddToFdb }

end. { unit. FileSys }

