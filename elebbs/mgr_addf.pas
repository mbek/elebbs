unit MGR_ADDF;
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
** MGR_ADDF, Adding files to EleMGR (filebase) part of EleMGR/DOS.
**
** Copyright (c) 1996, 97 by Maarten Bekers
**
** Created : 07-Nov-1997
** Last update : 07-Nov-1997
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
  This unit should not be compiled in EleBBS
{$ENDIF}

{$IFDEF ISCGI}
  This unit should not be compiled into EleWEB
{$ENDIF}

{$IFNDEF WINGUI}
procedure Do_AddFiles(AreaNum: Word; Const Path, UlName: String; Entry: Longint; CopyThem: Boolean);
{$ENDIF}
procedure MgrInsert_Files(Const AreaNum: Word; Const Entry: Longint; Const UlName, Filepath: String; CopyThem: Boolean);

{$IFDEF MSDOS}
Const MaxFilesToAdd = 100;
{$ELSE}
Const MaxFilesToAdd = 10000;
{$ENDIF}

var FilesList: Array[1..MaxFilesToAdd] of record
                               {$IFDEF MSDOS}
                                  FName    : String[12];
                               {$ELSE}
                                  FName    : String[255];
                               {$ENDIF}
                                  FileDate : Longint;
                                  FileSize : Longint;
                                  Tagged   : Boolean;
                                end; { record }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses CfgRec, Debug_U, MgrFdb, GenDos, LongStr, FileSys,
      FileRout, GenFile, WordStr, CentrStr
     {$IFNDEF WINGUI}
     ,StUtils, Global, Area_lst, GenCfg,
     {$IFDEF MSDOS}
       Memory,
       Exec,
       Crt,
     {$ELSE}
       {$IFDEF TCPIP}
         FtpUnit,
       {$ENDIF}
     {$ENDIF}
      Dos, Strings, ElFile_U, FileMgr, ListSys, StrEdit,
       Ral, JDates, StrPath, scrnU, Cases, ObjDec,
        Colors
     {$ENDIF};
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

var NrFiles    : Longint;
    AddareaNum : Word;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure MgrInsert_Files(Const AreaNum: Word; Const Entry: Longint; Const UlName, Filepath: String; CopyThem: Boolean);
var Counter  : Longint;
    HdrInf   : FilesHdrRecord;
    FilesInf : FilesRecord;
    FileMgr  : MgrFileObj;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'InsertFiles - Insert_Files (AreaNum='+FStr(AreaNum)+') ( begin )');
  {$ENDIF}


  GetFilesRecord(filesinf, AreaNum, true);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'InsertFiles - Insert_Files (AreaName='+FilesInf.Name+')');
  {$ENDIF}

  if FilesInf.Name = '' then EXIT;

  FillChar(HdrInf, SizeOf(HdrInf), #00);

  for Counter := 01 to MaxFilesToAdd do
   if FilesList[Counter].Tagged then
    begin
      if NOT FileExist(FilesInf.FilePath + FilesList[Counter].Fname) then
       if CopyThem then
        begin
          FileCopy(FilePath + FilesList[Counter].FName,
                   Filesinf.FilePath + FilesList[Counter].FName, true, false);
        end; { if }

      HdrInf.Name       := FilesList[Counter].FName;
      HdrInf.Size       := FilesList[Counter].FileSize;
      HdrInf.Crc32      := -01;
      HdrInf.Uploader   := UlName;
      HdrInf.UploadDate := GetDosDate;
      HdrInf.FileDate   := GetPackedFileTime(FilesList[Counter].FName);
      HdrInf.LastDL     := GetDosDate;
      HdrInf.FileDate   := FilesList[Counter].FileDate;
      HdrInf.LastDL     := GetDosDate;
      HdrInf.LongDescPtr:= -1;

      FileMgr.Init(FilesInf.AreaNum, true);

      HdrInf.LfnPtr     := FileMgr.AddLfnPtr(FilesList[Counter].FName, FilesInf.FilePath +
                     FilesList[Counter].FName, HdrInf.Name);
      FileMgr.Done;

      InsertInHdr(AreaNum, Entry, 1, HdrInf);
    end; { if }
end; { proc. Insert_Files }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF WINGUI}
procedure LoadPathBox(const AreaNum: Word; Const Path: String);
var DirInfo : SearchRec;
begin
  FindFirst(Path, AnyFile - VolumeID, DirInfo);
  While DosError=00 do
    begin
       if Dirinfo.Attr AND Directory = 0 then
        begin
          if NrFiles>=MaxFilesToAdd then
            begin
              BREAK;
            end; { if }

          if SearchNameInArea(AreaNum, Dirinfo.Name, false) = -1 then
            begin
              Inc(NrFiles);
              Fileslist[NrFiles].FName   := Dirinfo.Name;
              FilesList[NrFiles].FileDate:= Dirinfo.Time;
              FilesList[NrFiles].FileSize:= Dirinfo.Size;
              FilesList[NrFiles].Tagged  := false;
            end; { if }

        end; { if }

     FindNext(DirInfo);
    end; { while }

  FindClose(DirInfo);
end; { proc. LoadPathBox }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DirListingProc(DirStr: ShortString);
var IsDirectory : Boolean;
    DateStr,
    TimeStr,
    SizeStr,
    NameStr  : String;
{$IFDEF TCPIP}
    FtpObj   : pFtpInterfaceObj;
{$ENDIF}
begin
{$IFDEF TCPIP}
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'DirListingProc() -> ');
    DebugObj.DebugLog(logTcpIp, '    ' + DirStr);
    DebugObj.DebugLog(logTcpIp, 'DirListingProc() <- ');
  {$ENDIF}

  New(ftpObj, Init);
  ftpObj^.FtpListToStr(IsDirectory, DirStr, TimeStr, DateStr, NameStr, SizeStr);
  Dispose(ftpObj, done);

  if NrFiles >= MaxFilesToAdd then EXIT;

  if NOT IsDirectory then
   if SearchNameInArea(AddAreaNum, NameStr, false) = -1 then
    if NameStr <> '' then
      begin
        Inc(NrFiles);
        Fileslist[NrFiles].FName   := Trim(NameStr);
        FilesList[NrFiles].FileDate:= PackTimeStr(DateStr, TimeStr);
        FilesList[NrFiles].FileSize:= FVal(SizeStr);
        FilesList[NrFiles].Tagged  := false;
      end; { if }
{$ENDIF}
end; { proc. DirListingProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure LoadFtpBox(const AreaNum: Word; Const Path: String);
var SaveScrn: Pointer;
    UsePath : String;
{$IFDEF TCPIP}
    ftpObj  : pFtpInterfaceObj;
{$ENDIF}
begin
{$IFDEF TCPIP}
  UsePath := Path;
  UsePath[Length(usePath) + 1] := '/';
  AddAreaNum := AreaNum;

  New(ftpObj, Init);
  if NOT ftpObj^.StartUpFtp(UsePath, '', '') then
    begin
      OneMomentBox(SaveScrn, 'Unable to connect/logon to FTP server..', false);
      GetInput;
      OneMomentBox(SaveScrn, '', true);

      ftpObj^.DoneFtp;

      EXIT;
    end; { if }

  ftpObj^.GetFtpDirList(UsePath, {$IFDEF FPC}@{$ENDIF}DirListingProc);
  ftpObj^.DoneFtp;
  Dispose(ftpObj, Done);
{$ENDIF}
end; { proc. LoadFtpBox }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure LoadAddBox(Const AreaNum: Word; Const Path: String);
var SaveScrn: Pointer;
begin
  FillChar(FilesList, SizeOf(FilesList), #00);
  NrFiles := 00;

  OneMomentBox(SaveScrn, 'One moment ..', false);

{$IFDEF TCPIP}
  if IsFtpUrl(Path) then LoadFtpBox(AreaNum, Path)
    else {$ENDIF} LoadPathBox(AreaNum, Path);

  OneMomentBox(SaveScrn, '', true);
end; { proc. LoadAddBox }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure FilesAdd_GetInfo(var Info1, Info2, Info3, Info4, Info5, Info6, Info7, Info8, Info9: List_StringType;
                           ItemNr: LongInt; HiLight: LongInt);
begin
  if FilesList[ItemNr].Tagged then Info1 := #254 else Info1 := #32;
  Info2 := FilesList[ItemNr].FName;
  Info3 := StUtils.MakeLen(FStr(FilesList[ItemNr].FileSize), 8, Space, True, false);
  Info4 := LangObj^.RaFormatDate(Date2Str(FilesList[ItemNr].Filedate), 8, y2k_filedate,
      0);
  Info5 := '';
  Info6 := '';
  Info7 := '';
  Info8 := '';
  Info9 := '';

end; { proc. FilesAdd_GetInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FilesAdd_Activate(HiLight: LongInt; HiBarPos: LongInt): Longint;
begin;
  FilesAdd_Activate := -1;
end; { func. FilesAdd_Activate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FilesAdd_Seek(Str: String): LongInt;
var Counter: Byte;
begin
  FilesAdd_Seek := -1;

  For Counter := 01 to NrFiles do
   If SUpcase(Str) = SupCase(Copy(FilesList[Counter].FName, 1, Length(Str)))
      then begin;
              FilesAdd_Seek := Counter;
              Break;
           end; { Found one }

end; { func. FilesAdd_Seek }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function FilesAdd_AbortCheck(CH: Char): Boolean;
begin
  FilesAdd_AbortCheck := CH in [#27, #13];
end; { func. FilesAdd_AbortCheck }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Filesadd_KeyPress(CH: Char; HiLight: LongInt;
                           var HiBarPos, TopOfScrn: Longint;
                           var StartRange, EndRange: LongInt): LongInt;
var Counter: Byte;
begin
  FilesAdd_Keypress := -1;

  Case CH of
   #22, #20 : For Counter := 01 to NrFiles do
            FilesList[Counter].Tagged := (CH=#20);
  end; { case }
end; { func. FilesAdd_keypress }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function FilesAdd_GetItems: LongInt;
begin
  FilesAdd_GetItems := NrFiles;
end; { func. FilesAdd_GetItems }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FilesAdd_CreateNew: Boolean;
begin
  FilesAdd_CreateNew := True;
end; { proc. FilesAdd_CreateNew }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TagAllCharHook(CH: Char; HiLight: Longint): Longint;
begin
  TagAllCharHook := -1;

  Case CH of
    #32 : begin
            FilesList[HiLight].Tagged := NOT FilesList[HiLight].Tagged;
            TagAllCharHook := HiLight + 01;
          end; { if }
  end; { case }

end; { proc. TagAllCharHook }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Do_AddFiles(AreaNum: Word; Const Path, UlName: String; Entry: Longint; CopyThem:Boolean);
var SaveNorm: Byte;
    SavePull: Byte;
    SaveBox : Byte;
    TitleStr: String;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'InsertFiles - Do_AddFiles(AreaNum='+FStr(AreaNum)+') (begin)');
  {$ENDIF}

  LoadAddBox(AreaNum, Path);

  if NrFiles = 00 then
    begin
      Mgr_MessageBox('Could not find any files which are not already in the database.', 0);
      EXIT;
    end; { if }

  SaveNorm := mnuNormColor;
  SavePull := mnuPullHiColor;
  SaveBox := mnuBoxColor;

  mnuNormColor := MakeAttr(Lightgray, Blue);
  mnuPullHiColor := 15;
  mnuBoxColor := Makeattr(LightBlue, Blue);

  if CopyThem then TitleStr := ' Add files '
   else TitleStr := ' Adopt orphaned files ';

{$IFDEF MSDOS}
  DoList(True, 18, 1, 14, 9, 13, 00, 00, 00, 00, 00,                                   { ShowLen }
         17, 04, 56, mnuScrnLen - 4,            { Window Coordinates }
{$ELSE}
  DoList(True, 04, 1, 50, 9, 13, 00, 00, 00, 00, 00,                                   { ShowLen }
         03, 04, 78, mnuScrnLen - 4,
{$ENDIF}
         TitleStr,
         '(SPACE) tag, (ALT-U/T) un/tag all, (ENTER) continue, (ESCAPE) quit',
         {$IFDEF FPC}@{$ENDIF}FilesAdd_GetInfo,
         {$IFDEF FPC}@{$ENDIF}FilesAdd_Activate,
         {$IFDEF FPC}@{$ENDIF}FilesAdd_Seek,
         {$IFDEF FPC}@{$ENDIF}FilesAdd_AbortCheck,
         {$IFDEF FPC}@{$ENDIF}FilesAdd_KeyPress,
         {$IFDEF FPC}@{$ENDIF}FilesAdd_GetItems,
         {$IFDEF FPC}@{$ENDIF}FilesAdd_CreateNew,
         {$IFDEF FPC}@{$ENDIF}TagAllCharHook,
         False,
         00, 00,
         true, false);

  mnuNormColor := SaveNorm;
  mnuPullHiColor := SavePull;
  mnuBoxColor := SaveBox;

  if rdLastKey=#13 then
    begin
      MgrInsert_Files(AreaNum, Entry, UlName, JustPath(Path), CopyThem);
    end; { if }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'InsertFiles - Do_AddFiles(AreaNum='+FStr(AreaNum)+') ( end )');
  {$ENDIF}
end; { proc. Do_AddFiles }
{$ENDIF - NOT WINGUI}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
