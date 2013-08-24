unit ELFILE_U;
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
** ELEFILE routines for EleBBS
**
** Copyright (c) 1996, 1997, 1998 by Maarten Bekers
**
** Created : 01-Nov-1997
** Last update : 02-Jul-1998
**
** note: New filemgr object introduced at the end of June 1998
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Global, CfgRec, MgrFdb, Ra2fdb, FileObj;

{-------------------------------------------------------------------------}
procedure MaintFileBaseProc(var CurHDR: CfgRec.FilesHdrRecord;
                            var CurIDX: CfgRec.FilesIdxRecord;
                            var HdrF: pFileObj;
                            var IdxF: pFileObj;
                            LfnName: String;
                            RecNum : Longint);
{-------------------------------------------------------------------------}

function  ImportFileBase(AreaNum       : Word;
                         FileName,
                         UploaderName  : String;
                         ColumnPos     : Longint;
                         EraseFile     : Boolean;
                         DoMissing     : Boolean;
                         var NrImported: Word): fdb_ResultCodeType;


procedure DeleteFile(const AreaNum   : Word;
                     const FName     : String;
                     const RecNum    : Longint;
                     const FullDelete: Boolean;
                     var   NrDeleted  : Word;
                     const DeleteOpt: DeleteFileOpt;
                     const AlsoLocked: Boolean;
                     const UseRecNum : Boolean);
function  CleanFileBase(AreaNum       : Word;
                        KillMissing   : Boolean;
                        var TotalMoved: Word): fdb_ResultCodeType;
function  CompressFileBase(AreaNum: Word): fdb_ResultCodeType;
function  ExportFileBase(AreaNum     : Word;
                         ExportName  : String;
                         var ExportNr: Word;
                         AppendF     : Boolean;
                         RaCompat    : Boolean):fdb_ResultCodeType;
function  MakeAreaTopic(Name: String; SevenBit: Boolean): String;
function  LockUnlockFileBase(AreaNum : Word;
                             Option  : String;
                             FileName: String): fdb_ResultCodeType;
procedure AddStatusMsg(S: String);
procedure UpdateFile(AreaNum    : Word;
                     FileName   : String;
                     TouchMethod: Byte);
function  MakeFileList(AreaNum      : Word;
                       FileName     : String;
                       DaysOld      : Word;
                       NoHdr,
                       SevenBit,
                       FormFeeds    : Boolean;
                       var NrFiles,
                           NrKBytes : Longint): fdb_ResultCodeType;
function  SortFileBase(AreaNum          : Word;
                       DateSort         : Boolean;
                       ReverseSort      : Boolean;
                       Glob_StartRange,
                       Glob_EndRange    : Longint;
                       EleFile          : Boolean): fdb_ResultCodeType;
function  ReArcFileBase(AreaNum      : Word;
                        var NrRearced: Word): fdb_ResultCodeType;
function ExtractDiz(const FileInfo: FilesRecord;
                    const FileName: String;
                    var   Txt: TxtRec;
                    ForcedExtension : Byte): Boolean;
function WriteDiz(const FileInfo : FilesRecord;
                  const FileName : String;
                  var   Txt      : TxtRec;
                  ForcedExtension: Byte): Boolean;
function ConvertToExtensionNum(S: String): Byte;
function DescribeFileBase(AreaNum: Word; FileName: String): fdb_ResultCodeType;

    {--------------- Global info used for CleanFileBaseProc ----------------}
var TotalMoved           : Word;
    KillMissing          : Boolean;
    FullDelete           : Boolean;
    GlobalFileName       : String;
    GlobalRecNum         : Longint;
    OverrideRecNum       : Boolean;
    MaintOption          : String;
    UpdateOption         : Byte;
    GlobalAreaFileInfo   : FilesRecord;
    GlobalEleFileInfo    : EleFilesRecord;
    AreaObj              : fdbFileObj;

    Glob_DaysOld         : Longint;

    CharMapArray         : Array[0..255] of Char;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses
      JDates,
      MkFile,
      FileRout,
      Desc_U,
      StUtils,
      ObjDec,
      Ranges,
      {$IFDEF ELEUNIX}
        {$IFDEF VER1_0}
          Linux,
        {$ELSE}
          Unix,
	  OldLinux,
        {$ENDIF}
      {$ENDIF}

      FileSys,
      Formlst,
      Strunit,

      Colors,
      Cases,
      StrPath,
      LongStr,
      CentrStr,
      WordStr,
      GenFile,
      {$IFDEF TCPIP}
        FtpUnit,
      {$ENDIF}

     {$IFNDEF WIN32}
      {$IFNDEF USE32BIT}
        Exec, Memory,
      {$ENDIF}

       Dos, Strings,
     {$ELSE}
       {$IFDEF VirtualPascal}
         Dos,
       {$ENDIF}
       {$IFDEF FPC}
         Dos,
       {$ENDIF}
       SysUtils,
     {$ENDIF}

        Sort_Un, ElLog_U, BitWise,
         MemMan, Debug_U;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Const MaxSortings    = (65535 div SizeOf(Longint)) - 2;

Type SortListType    = Longint;
     SortListRec     = Array[1..MaxSortings] of SortListType;
     FileSortType    = pFileObj;

var Sort_Glob_DateSort   : Boolean;
    Sort_Glob_ReverseSort: Boolean;
    SortHdr_F            : FileSortType;
    SortTxt_F            : FileSortType;
    SortIDX              : ^SortListRec;

    FileMgr              : MgrFileObj;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AddStatusMsg(S: String);
begin
  WriteLn(s);
end; { proc. AddStatusMsg }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


procedure GetFileBaseSizeAndTime(Path: String; var Size, Time: Longint);
var DateStr,
    TimeStr,
    SizeStr   : String;
    {$IFDEF TCPIP}
      ftpObj    : pFtpInterfaceObj;
    {$ENDIF}
begin
  if NOT IsFtpUrl(Path) then
    begin
      Size := GetFileSize(Path, 1);
      Time := GetPackedFileTime(Path);
    end
      else begin
             {$IFDEF TCPIP}
               New(ftpObj, Init);
               if ftpObj^.StartUpFtp(Path, '', '') then
                 begin
                   ftpObj^.FtpGetList(Path, DateStr, TimeStr, SizeStr);

                   Size := FVal(SizeStr);
                   Time := PackTimeStr(DateStr, TimeStr);
                 end; { if }

               ftpObj^.DoneFTP;
               Dispose(ftpObj, Done);
             {$ENDIF}
           end; { if }
end; { func. GetFileBaseSize }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EraseFileBaseFile(URL: String);
var DateStr,
    TimeStr,
    SizeStr   : String;
    {$IFDEF TCPIP}
      ftpObj    : pftpInterfaceObj;
    {$ENDIF}
begin
  if NOT IsFtpUrl(URL) then
    begin
      EraseFile(url);
    end
      else begin
             {$IFDEF TCPIP}
               New(ftpObj, Init);
               if ftpObj^.StartUpFtp(Url, '', '') then
                 begin
                   ftpObj^.FtpDeleteFile(url);
                 end; { if }

               ftpOBj^.DoneFTP;
               Dispose(ftpObj, Done);
             {$ENDIF}
           end; { if }
end; { func. GetFileBaseSize }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function MakeAreaTopic(Name: String; SevenBit: Boolean): String;
const StyleStr: Array[1..06] Of String[10] = ('ÚÄ¿³´ÙÀÃ³Ä',  { 1 }
                                              'ÉÍ»º¹¼ÈÌºÍ',  { 2 }
                                              'ÖÄ·º¶½ÓÇºÄ',  { 3 }
                                              'ÕÍ¸³µ¾ÔÆ³Í',  { 4 }
                                              'ÚÄ·³µ¼ÔÆºÍ',  { 5 }
                                              '+-+||++||-'); { 6 }

var Style: Byte;
    TempStr: String;
    LeftStr: String;
    RightStr: String;
begin
  if SevenBit then Style := 06 else Style := 01;
  TempStr := '';

  LeftStr := '°°°±±±²²²' + #32;
  RightStr := #32 + '²²²±±±°°°';

  if SevenBit then
    begin
      LeftStr := Dup(#32, 10);
      RightStr := Dup(#32, 10);
    end;

  TempStr := TempStr + #32 + StyleStr[Style, 1] + Dup(StyleStr[Style, 2], 75) + StyleStr[Style, 3] + #13 + #10;
  TempStr := TempStr + #32 + StyleStr[Style, 4] + LeftStr + CenterJust(Name, 55, True) + RightStr + StyleStr[Style, 4] +#13#10;
  TempStr := TempStr + #32 + StyleStr[Style, 7] + Dup(StyleStr[Style, 2], 75) + StyleStr[Style, 6] + #13 + #10;

  MakeAreaTopic := #13 + #10 + #13 + #10 + TempStr;
end; { func. MakeAreaTopic }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function AllowInclude(HdrInf: FilesHdrRecord; DaysOld: Word): Boolean;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'AllowInclude (BEGIN). Name    = "' + HdrInf.Name + '"');
    DebugObj.DebugLog(logString, 'AllowInclude          DaysOld = "' + FStr(DaysOld) + '"');
    DebugObj.DebugLog(logString, 'AllowInclude          UpDate  = "' + Date2Str(HdrInf.UploadDate) + '"');
    DebugObj.DebugLog(logString, 'AllowInclude          DaysAgo = "' + FStr(DaysAgo(Date2Str(HdrInf.UploadDate))) + '"');
    DebugObj.DebugLog(logString, 'AllowInclude ( END )  Attrib  = "' + FStr(HdrInf.Attrib) + '"');
  {$ENDIF}

  AllowInclude := True;

  if DaysOld>00 then
   if DaysAgo(Date2Str(HdrInf.UploadDate)) > DaysOld then
     AllowInclude := False;

  if ReadBit(HdrInf.Attrib, 1) then
    begin
      if HdrInf.Name[0] <> #0 then      { Unlisted for comments dont exist }
        AllowInclude := false;
    end; { if }
end; { func. AllowInclude }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  MakeFileList(AreaNum      : Word;
                       FileName     : String;
                       DaysOld      : Word;
                       NoHdr,
                       SevenBit,
                       FormFeeds    : Boolean;
                       var NrFiles,
                           NrKBytes  : Longint): fdb_ResultCodeType;


var TotalItems    : Longint;
    ItemCounter   : Longint;
    FilesInf      : FilesRecord;
    TextBuffer    : Array[0..16383] of Char;
    UseCR_Norm    : Boolean;
    UseCR_Missing : Boolean;
    MissingLine   : String;
    AvailLine     : String;
    MissingPos    : Byte;
    AvailPos      : Byte;
    DescPos       : Byte;
    DisplayStr    : String;

    ExportFile    : Text;
    HdrLines      : DisplayLinesArray;
    HeaderLines   : Byte;
    Counter       : Byte;
    ShownHeader   : Boolean;
begin
{------------------ Initialize all fields and get area info -------------------}
  MakeFileList := fdb_OK;
  ShownHeader := False;
  if FileName='' then EXIT;

  GetFilesRecord(FilesInf, AreaNum, true);

{----------------------- Open and Create output file --------------------------}
  Assign(ExportFile, FileName);
  FileMode := ReadWriteMode + DenyNone;
  SetTextBuf(ExportFile, TextBuffer);
  {$i-} Append(ExportFile); {$I+}

  if (IOResult > 00) then
   {$i-} System.Rewrite(ExportFile); {$I+}

  if IOResult>00 then
       begin
          MakeFileList := fdb_NoOutput;
          {$i-} Close(Exportfile); {$I+}
          if IOResult>00 then ;
          EXIT;
       end; { couldn't create export }

{------------------------- Open the area info ---------------------------------}
  AreaObj.Init(FilesInf.AreaNum, False);
  if AreaObj.GetError <> 00 then
     begin
       MakeFileList := fdb_NoIDX;
       {$i-} Close(ExportFile); {$I+}
       if IOResult>00 then ;

       Areaobj.Done;
       EXIT;
     end; { if }

  TotalItems := AreaObj.TotalRecords;
  ItemCounter := 00;

{------------------------- Set the description --------------------------------}
  MissingLine := GlobalCfg^.RaConfig^.FileMissingLine;
  AvailLine := GlobalCfg^.RaConfig^.FileLine;

  GetDescriptionPos(MissingPos, MissingLine, False, UseCR_NORM);
  GetDescriptionPos(AvailPos, AvailLine, False, UseCR_MISSING);

  While (ItemCounter < TotalItems) AND (TotalItems>00) do
    begin
      AreaObj.Read(ItemCounter);                   { Read header sequentially }
      Inc(ItemCounter);

      AreaObj.ReadDescription(AreaObj.TxtF,
                              AreaObj.GetLongDescPtr); { Read the description }

      {------------------------ Check to see this file -----------------------}
      if AllowInclude(AreaObj.CurHdr^, DaysOld) then
        begin
          {--------------------- Create listing header -----------------------}
          if NOT ShownHeader then
            begin
              {$I-}
                If NOT NoHdr then
                  WriteLn(ExportFile, MakeAreaTopic('Area '+FStr(FilesInf.AreaNum) + ' - ' + FilesInf.Name, SevenBit));

                WriteLn(ExportFile);
              {$I+}
              if IOResult>00 then ;
              ShownHeader := True;
            end; { if }

          {----------------- Check to see this file is missing ----------------}
          if AreaObj.IsMissing then
           begin
             DescPos := MissingPos;
             DisplayStr := MissingLine;
           end else begin
                      DescPos := AvailPos;
                      DisplayStr := AvailLine;
                    end; { if }

          {----------------------- Create display str ------------------------}
          If NOT AreaObj.IsComment then
             DisplayListFile(AreaObj.CurHdr^,
                             DisplayStr, HdrLines,
                             AreaObj.GetDescLine(78 - DescPos), False, HeaderLines,
                             FilesInf, AreaObj.GetFileName, true);

          If NOT AreaObj.IsComment then
            begin
              RemoveRaColors(HdrLines[00]);

              {$i-}
                WriteLn(ExportFile, HdrLines[00]);
              {$i+}
              If IOresult>00 then ;

              if HeaderLines>00 then
                For Counter := 01 to Min(HeaderLines, 25) do
                 begin
                   RemoveRaColors(HdrLines[Counter]);

                   {$i-}
                     WriteLn(ExportFile, HdrLines[Counter]);
                   {$i+}
                   If IOresult>00 then ;
                 end; { for }
            end; { for }

          {----------------------- Is comment, display it --------------------}
          {$i-}
          If AreaObj.IsComment then
            WriteLn(ExportFile, AreaObj.GetDescLine(0));
          {$i+}
          if IOResult>00 then ;

          {------------------ More than one line? Display all -----------------}
          While NOT AreaObj.EndOfDesc do
           begin
              {$i-}
               Write(ExportFile, Dup(#32, DescPos-1), AreaObj.GetDescLine(78 - DescPos));
               WriteLn(ExportFile);
              {$i+}
              if IOResult>00 then ;
            end; { while }

          {----------------------- Update statistics --------------------------}
          if NOT AreaObj.IsComment then
            begin
              Inc(NrFiles);
              Inc(NrKBytes, (AreaObj.GetSize DIV 1024));
            end; { if }

          {$IFDEF WITH_DEBUG}
            DebugObj.DebugLog(logString, 'FileList() = '+FStr(nrkBytes)+ ' / CurFile = '+FStr(AreaObj.GetSize));
          {$ENDIF}
        end; { if }
    end; { while }

  AreaObj.Done;

  {$i-}
  if FormFeeds then Write(ExportFile, #12);         { Add a FormFeed character }
  {$i+}
  if IOResult>00 then ;

  {$i-} Close(ExportFile); {$i+}
  If IOResult>00 then;
end; { func. MakeFileList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FileSortFunc(El1, El2: Pointer): Boolean;

function GetLfn(var HdrInf: FilesHdrRecord): String;
var TempStr: String;
    Numread: Longint;
begin
  SortTxt_F^.Seek(Hdrinf.LfnPTR);
  NumRead := SortTxt_F^.BlkRead(TempStr[1], SizeOf(TempStr) - 01);
  if SortTxt_F^.IOResult > 00 then NumRead := 00;

  Tempstr[0] := Chr(NumRead);
  TempStr := TempStr + #00;

  GetLfn := Copy(TempStr, 1, Pos(#00, TempStr) - 01);
end; { func. GetLfn }

var HdrInf   : Array[1..2] of FilesHdrRecord;
    Temp     : Array[1..2] of String;
    TempRes  : Boolean;
    Numread  : NumreadType;
begin
  SortHdr_F^.Seek((Longint(El1^) - 1) * SizeOf(FilesHdrRecord));
  if SortHdr_F^.IoResult = 0 then
    NumRead := SortHdr_F^.BlkRead(HdrInf[1], SizeOf(FilesHdrRecord))
      else FillChar(HdrInf[1], SizeOf(HdrInf[1]), #0);

  SortHdr_F^.Seek((Longint(El2^) - 1) * SizeOf(FilesHdrRecord));
  if SortHdr_F^.IoResult = 0 then
    NumRead := SortHdr_F^.BlkRead(HdrInf[2], SizeOf(FilesHdrRecord))
     else FillChar(HdrInf[2], SizeOf(HdrInf[2]), #0);

  Temp[1] := SUpcase(HdrInf[1].Name);
  Temp[2] := SUpcase(HdrInf[2].Name);

  if HdrInf[1].LfnPtr > 0 then
    Temp[1] := GetLfn(HdrInf[1]);
  if HdrInf[2].LfnPtr > 0 then
    Temp[2] := GetLfn(HdrInf[2]);


  if NOT Sort_Glob_ReverseSort then
    begin
      if (Sort_Glob_DateSort) then
        begin
          TempRes := HdrInf[1].FileDate > HdrInf[2].FileDate;

          if (HdrInf[1].FileDate = HdrInf[2].FileDate) then
            TempRes := SUpCase(Temp[1]) > SUpCase(Temp[2]);
        end; { if }

      if (NOT Sort_Glob_DateSort) then
        begin
          TempRes := SUpCase(Temp[1]) > SUpCase(Temp[2]);

          if (SUpCase(Temp[1]) = SUpCase(Temp[2])) then
            TempRes := HdrInf[1].FileDate > HdrInf[2].FileDate;
        end; { if }
    end; { if }

  if Sort_Glob_ReverseSort then
    begin
      if (Sort_Glob_DateSort) then
        begin
          TempRes := HdrInf[1].FileDate < HdrInf[2].FileDate;

          if (HdrInf[1].FileDate = HdrInf[2].FileDate) then
            TempRes := SUpCase(Temp[1]) < SUpCase(Temp[2]);
        end; { if }

      if (NOT Sort_Glob_DateSort) then
        begin
          TempRes := SUpCase(Temp[1]) < SUpCase(Temp[2]);

          if (SUpCase(Temp[1]) = SUpCase(Temp[2])) then
            TempRes := HdrInf[1].FileDate < HdrInf[2].FileDate;
        end; { if }
    end; { if }

  FileSortFunc := TempRes;
end; { func. FileSortFunc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  SortFileBase(AreaNum          : Word;
                       DateSort         : Boolean;
                       ReverseSort      : Boolean;
                       Glob_StartRange,
                       Glob_EndRange    : Longint;
                       EleFile          : Boolean): fdb_ResultCodeType;

function BlockingFound(var F: FileSortType; StartRange: Longint; var OnlyComment: Boolean): Longint;
var HdrInf     : FilesHdrRecord;
    PrevHDR    : FilesHdrRecord;

    NumRead    : NumreadType;
begin
  BlockingFound := -1;

  F^.Seek(StartRange * SizeOf(FilesHdrRecord));
  if F^.FileSize = F^.FilePos then EXIT;


  While (F^.FileSize > F^.FilePos) do
    begin
      NumRead := F^.BlkRead(HdrInf, SizeOf(FilesHdrRecord));
      if (StartRange + 01) = (F^.Filepos DIV SizeOf(FilesHdrRecord)) then
           PrevHDR := HdrInf;

      if PrevHdr.Name[0] = #00 then
       if HdrInf.Name[0] <> #00 then
         BREAK;

      if PrevHdr.Name[0] <> #00 then
       If HdrInf.Name[0] = #00 then
         begin
           F^.Seek(F^.FilePos - SizeOf(FilesHdrRecord));
           BREAK;
         end; { if }
    end; { while }

  OnlyComment := (PrevHdr.Name[0] = #00);
  BlockingFound := F^.Filepos DIV SizeOf(FilesHdrRecord);
end; { func. BlockingFound }


var HdrInf     : FilesHdrRecord;
    PrevHdr    : FilesHdrRecord;
    TotalSize  : Longint;
    Counter    : Longint;
    NewHdr_F   : FileSortType;

    StartRange : Longint;
    EndRange   : Longint;
    OnlyComment: Boolean;
    SavePos    : Longint;
    NumRead    : NumreadType;
    FileMgr    : MgrFileObj;
    Temp       : Longint;

procedure CleanUp;
begin
  if SortHdr_F <> nil then
    Dispose(SortHdr_F, Done);
  if SortTxt_F <> nil then
    Dispose(SortTxt_F, Done);

  if NewHdr_F <> nil then
    begin
      NewHdr_F^.Erase;
      Dispose(NewHdr_F, Done);
    end; { if }

  if SortIdx <> nil then
    ReleaseMem(SortIdx, SizeOf(SortListRec));
  SortIdx := nil;
end; { proc. CleanUp }

begin
  SortTxt_F := nil;
  SortHdr_F := nil;
  NewHdr_F := nil;

  FileMgr.Init(AreaNum, True);
  FileMgr.FileBaseMaintenance(AreaNum,
                              {$IFDEF FPC}@{$ENDIF}CreateIdxBaseProc,
                              true);
  FileMgr.Done;

  SortFileBase := fdb_OK;

  Sort_Glob_DateSort := DateSort;
  Sort_Glob_ReverseSort := ReverseSort;

  if FileExist('TEMP'+FStr(AreaNum)+'.HDR') then
   if NOT EraseFile('TEMP'+FStr(AreaNum)+'.HDR') then
     begin
       EXIT;
     end; { if }

  SortIDX := nil;
  TotalSize := 00;
  TotalSize := TotalSize + GetFileSize(GlobalCfg^.RaConfig^.FileBasePath + 'hdr'+BsChar+'fdb'+FStr(AreaNum)+'.hdr', 1);
  TotalSize := (TotalSize * 02) + 1024 * 30;
  If TotalSize > GetDiskFree(GlobalCfg^.RaConfig^.FileBasePath[1]) then
   begin
     SortFileBase:= fdb_TooLarge;
     EXIT;
   end; { if }
  if GetFileSize(GlobalCfg^.RaConfig^.FileBasePath + 'hdr'+BsChar+'fdb'+FStr(AreaNum)+'.hdr', 1) = SizeOf(FilesHdrRecord) then
    begin
      EXIT;
    end; { if }

  New(SortHdr_F, Init);
  SortHdr_F^.Assign(GlobalCfg^.RaConfig^.FileBasePath + 'hdr' + BsChar + 'fdb' + FStr(AreaNum) + '.hdr');
  SortHdr_F^.FileMode := ReadWriteMode + DenyAll;
  if NOT SortHdr_F^.Open(1) then
    begin
      SortFileBase := fdb_NoHdr;
      Dispose(SortHdr_F, Done);
      EXIT;
     end; { if }

  New(SortTxt_F, Init);
  SortTxt_F^.Assign(GlobalCfg^.RaConfig^.FileBasePath + 'txt' + BsChar + 'fdb' + FStr(AreaNum) + '.txt');
  SortTxt_F^.FileMode := ReadWriteMode + DenyAll;
  if NOT SortTxt_F^.Open(1) then
    begin
      SortFileBase := fdb_NoHdr;
      Dispose(SortHdr_F, Done);
      Dispose(SortTxt_F, Done);
      EXIT;
     end; { if }

  New(NewHdr_F, Init);
  NewHdr_F^.Assign('temp' + FStr(AreaNum) + '.hdr');
  NewHdr_F^.FileMode := ReadWriteMode + DenyAll;
  if NOT NewHdr_F^.Create(1) then
    begin
      SortFileBase:= fdb_NoHdr;
      CleanUp;
      EXIT;
    end; { if }

  if ((SortHDR_F^.FileSize div SizeOf(FilesHdrRecord)) > MaxSortings) 
   {$IFDEF MSDOS}OR (MemAvail < SizeOf(SortIdx)) {$ENDIF}
    then begin
           CleanUp;

           SortFileBase := fdb_TooLarge;
           EXIT;
         end; { if }

  EndRange := Glob_EndRange;
  StartRange := Glob_StartRange;

  if StartRange > EndRange then
    begin
      Temp := StartRange;

      StartRange := EndRange;
      EndRange := Temp;
    end; { if }

  if (EndRange < 0) OR (StartRange < 00) then
    begin
      StartRange := 1;
      EndRange := SortHdr_F^.FileSize DIV SizeOf(FilesHdrRecord);
    end; { if }

  IF NOT AllocMem(SortIDX, SizeOf(SortListRec), 'SortListRec', 'SortFileBase') then
      begin
        CleanUp;

        SortFileBase := fdb_NoTempPath;
        EXIT;
      end; { if }

  if StartRange > (SortHdr_F^.FileSize DIV SizeOf(FilesHdrRecord)) then
       StartRange := SortHdr_F^.FileSize div SizeOf(FilesHdrRecord);
  if EndRange > (SortHdr_F^.FileSize DIV SizeOf(FilesHdrRecord)) then
    begin
      if (SortHdr_F^.FileSize DIV SizeOf(FilesHdrRecord)) = 1 then
        EndRange := 2
         else EndRange := SortHdr_F^.FileSize div SizeOf(FilesHdrRecord);
    end; { if }

  if (StartRange = EndRange) then
    begin
      CleanUp;

      EXIT;
    end; { if }

  FillChar(SortIdx^, SizeOf(SortListRec), 00);
  For Counter := 01 to (SortHDR_F^.FileSize DIV SizeOf(FilesHdrRecord)) do
   SortIdx^[Counter] := Counter;

  if NOT EleFile then
   QuickSort(SortIdx^[StartRange], (EndRange - StartRange) + 01, SizeOf(SortListType), {$IFDEF FPC}@{$ENDIF}FileSortFunc)
    else begin
            SortHDR_F^.Seek(0);
            StartRange := 00;

            While BlockingFound(SortHdr_F, StartRange, OnlyComment) >= 00 do
             begin
               Savepos := SortHdr_F^.FilePos div SizeOf(FilesHdrRecord);

               if NOT OnlyComment then
                QuickSort(SortIdx^[Max(StartRange, 01)],
                          ( (SortHDR_F^.FilePos DIV SizeOf(FilesHdrRecord)) - Max(StartRange-1, 0)),
                          SizeOf(SortListType), {$IFDEF FPC}@{$ENDIF}FileSortFunc);

               SortHdr_F^.Seek(SavePos * SizeOf(FilesHdrRecord));
               StartRange := SortHdr_F^.FilePos DIV SizeOf(FilesHdrRecord);
             end; { while }

            SortHDR_F^.Seek(0);
         end; { while }

  LockFileBase(NewHdr_F);
  Counter := (SortHDR_F^.FileSize DIV SizeOf(FilesHdrRecord)); {!}

  For Counter := 01 to (SortHDR_F^.FileSize DIV SizeOf(FilesHdrRecord)) do
   begin
     SortHdr_F^.Seek((SortIdx^[Counter]-1) * SizeOf(FilesHdrRecord));

     NumRead := SortHdr_F^.BlkRead(HdrInf, SizeOf(FilesHdrRecord));
     NewHdr_F^.BlkWrite(HdrInf, SizeOf(FilesHdrRecord));
   end; { for }

  UnlockFileBase(NewHdr_F);

  Dispose(SortHdr_F, Done);
  Dispose(SortTxt_F, Done);
  Dispose(NewHdr_F, Done);

  EraseFile(GlobalCfg^.RaConfig^.FileBasePath + 'hdr'+BsChar+'fdb'+FStr(AreaNum)+'.hdr');
  if NOT RenameFile('temp'+FStr(AreaNum)+'.hdr', GlobalCfg^.RaConfig^.FileBasePath +
                    'hdr'+BsChar+'fdb'+FStr(AreaNum)+'.hdr') then
    begin
      SortFileBase := fdb_NoIdx;
      CleanUp;
      EXIT;
    end; { if }

  ReleaseMem(SortIdx, SizeOf(SortListRec));
  SortIdx := nil;

  FileMgr.Init(AreaNum, True);
  FileMgr.FileBaseMaintenance(AreaNum,
                              {$IFDEF FPC}@{$ENDIF}CreateIdxBaseProc,
                              true);
  FileMgr.Done;
end; { func. SortFileBase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CleanFileBaseProc(var CurHDR: CfgRec.FilesHdrRecord;
                            var CurIDX: CfgRec.FilesIdxRecord;
                            var HdrF: pFileObj;
                            var IdxF: pFileObj;
                            LfnName: String;
                            Recnum : Longint);

function FileExpired(FDate, DLDate, Attrib: Longint; FilesInf: FilesRecord): Boolean;
begin
  FileExpired := False;

  If FilesInf.KillDaysFD >00 then
   if (DaysAgo(Date2Str(FDate)) > FilesInf.KillDaysFD) then FileExpired := True;

  If FilesInf.KillDaysDL >00 then
   if (DaysAgo(Date2Str(DLDate)) > FilesInf.KillDaysDL) then FileExpired := True;

  if ReadBit(Attrib, 4) { Locked } then FileExpired := False;
end; { func. FileExpired }

var DoDelete   : Boolean;
    DestAreaObj: MgrFileObj;
    TxtInf     : TxtRec;
    DestArea   : FilesRecord;
begin
  {--------- Check to see if it's not an comment and if file expired -------}
  if NOT FileMgr.IsComment then
   begin
     if FileExpired(FileMgr.GetFileDate,
                    FileMgr.GetLastDL,
                    FileMgr.GetAttrib,
                    GlobalAreaFileInfo) then
       begin
         Inc(TotalMoved);

         {----------------------- MoveArea is entered ----------------------}
         if GlobalAreaFileInfo.MoveArea > 00 then
           begin
              GetFilesRecord(DestArea, GlobalAreaFileInfo.MoveArea, true);

              FileMgr.OpenAreaFiles(True);
              FileMgr.ReadDescription(FileMgr.TxtF, FileMgr.GetLongDescPtr);
              FileMgr.CloseAreaFiles;
              FileMgr.GetDescBlock(TxtInf);

              DestAreaObj.Init(DestArea.AreaNum, True);
              FileMgr.CurHdr^.LongDescPtr := DestAreaObj.AddDescription(TxtInf);
              DestAreaObj.AddHeader(FileMgr.CurHdr^);
              DestAreaObj.Done;

              FileCopy(GlobalAreaFileInfo.FilePath + FileMgr.GetFileName,
                       DestArea.FilePath + FileMgr.GetFileName, True, False);
              EraseFile(GlobalAreaFileInfo.FilePath + FileMgr.GetFileName);
           end { if movearea > 00 }

           {-------------------- MoveArea is empty -------------------------}
            else begin
                   WriteLn(#32, #32, FileMgr.GetFileName:12, ' removed');
                   EraseFile(GlobalAreaFileInfo.FilePath + FileMgr.GetFileName);
                 end; { if MoveArea = 00 }

       end { if fileexpired }
        else begin
               DoDelete := false;
               if KillMissing then
                 DoDelete := FileMgr.IsMissing;

               if FileMgr.IsLocked then
                 DoDelete := false;                       { File is locked }

               if DoDelete then
                 begin
                   WriteLn(#32, #32, FileMgr.GetFileName:12, ' removed');
                   EraseFile(GlobalAreaFileInfo.FilePath + FileMgr.GetFileName);
                 end; { if }

               if NOT DoDelete then
                 begin
                   HdrF^.BlkWrite(CurHDR, SizeOf(FilesHdrRecord));
                   IdxF^.BlkWrite(CurIDX, SizeOf(FilesIdxRecord));
                 end; { if }
             end; { not expired or comment }

   end { Isn't a comment }
     else begin
            HdrF^.BlkWrite(CurHDR, SizeOf(FilesHdrRecord));
            IdxF^.BlkWrite(CurIDX, SizeOf(FilesIdxRecord));
          end; { is a comment }
end; { proc. CleanFileBaseProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CleanFileBase(AreaNum       : Word;
                       KillMissing   : Boolean;
                       var TotalMoved: Word): fdb_ResultCodeType;
begin
  GetFilesRecord(GlobalAreaFileInfo, AreaNum, true);

  CleanFileBase := fdb_OK;
  ElFile_U.KillMissing := KillMissing;

  FileMgr.Init(AreaNum, True);
  CleanFileBase := FileMgr.FileBaseMaintenance(AreaNum,
                                               {$IFDEF FPC}@{$ENDIF}CleanFileBaseProc,
                                               false);
  FileMgr.Done;

  TotalMoved := ElFile_U.TotalMoved;
end; { proc. CleanFileBase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CompressFileBase(AreaNum: Word): fdb_ResultCodeType;
Type TextArrayType = Array[0..19999] of Char;
var TempHdr_F     : pFileObj;
    TempTXT_F     : pFileObj;
    HdrF          : pFileObj;
    TxtF          : pFileObj;

    HdrInf        : FilesHdrRecord;
    FilesInf      : FilesRecord;
    TextArray     : TextArrayType;
    DeleteThisFile: Boolean;
    NumRead       : NumReadType;
    TotalSize     : Longint;

    TotalItems    : Longint;
    ItemCounter   : Longint;

function OpenTempBase(var HDRF, TXTF: pFileObj; FMode: Word; HDRName, TxtName: String; Create: Boolean): Boolean;
begin
  OpenTempBase := FALSE;

  New(HdrF, Init);
  HdrF^.Assign(HdrName);
  HdrF^.FileMode := fMode;
  if Create then
    begin
      if NOT HdrF^.OpenOrCreate(1) then
        begin
          Dispose(HdrF, Done);
          EXIT;
        end; { if }
    end
      else begin
             if NOT HdrF^.Open(1) then
               begin
                 Dispose(HdrF, Done);
                 EXIT;
               end; { if }
           end; { if }

  New(TxtF, Init);
  TxtF^.Assign(TxtName);
  TxtF^.FileMode := fMode;
  if Create then
    begin
      if NOT TxtF^.OpenOrCreate(1) then
        begin
          Dispose(TxtF, Done);
          EXIT;
        end; { if }
    end
      else begin
             if NOT TxtF^.Open(1) then
               begin
                 Dispose(TxtF, Done);
                 EXIT;
               end; { if }
           end; { if }

  OpenTempBase := TRUE;
end; { func. OpenTempbase }

begin
  CompressFileBase := fdb_OK;

  {-------------------- Try erasing the temporarily files ------------------}
  if FileExist('temphdr.$$$') then if NOT EraseFile('temphdr.$$$') then EXIT;
  if FileExist('temptxt.$$$') then if NOT EraseFile('temptxt.$$$') then EXIT;

  GetFilesRecord(FilesInf, AreaNum, true);

  {--------------- Make sure were not short on diskspace -------------------}
  TotalSize := 00;
  TotalSize := TotalSize + GetFileSize(GlobalCfg^.RaConfig^.FileBasePath + 'hdr'+BsChar+'fdb'+FStr(AreaNum)+'.hdr', 1);
  TotalSize := TotalSize + GetFileSize(GlobalCfg^.RaConfig^.FileBasePath + 'txt'+BsChar+'fdb'+FStr(AreaNum)+'.txt', 1);
  TotalSize := (TotalSize * 02) + 1024 * 30;

  If TotalSize > GetDiskFree(GlobalCfg^.RaConfig^.FileBasePath[1]) then
   begin
     CompressFileBase := fdb_TooLarge;
     EXIT;
   end; { if }

  {------------------------ Rename original base ---------------------------}
  if NOT RenameFile(GlobalCfg^.RaConfig^.FileBasePath + 'hdr'+BsChar+'fdb'+FStr(AreaNum)+'.hdr', 'temphdr.$$$') then
      begin
        CompressFileBase := fdb_NOHDR;
        EXIT;
      end; { if }

  if NOT RenameFile(GlobalCfg^.RaConfig^.FileBasePath + 'txt'+BsChar+'fdb'+FStr(AreaNum)+'.txt', 'temptxt.$$$') then
      begin
        CompressFileBase := fdb_NOIDX;
        EXIT;
      end; { if }

  {------------------------ Open the new filebase --------------------------}
  if NOT OpenTempBase(HDRF, TXTF, ReadWriteMode + DenyAll,
                      GlobalCfg^.RaConfig^.FileBasePath + 'hdr'+BsChar+'fdb'+FStr(AreaNum)+'.hdr',
                      GlobalCfg^.RaConfig^.FileBasePath + 'txt'+BsChar+'fdb'+FStr(AreaNum)+'.txt',
                      True)
                       then begin
                              CompressFileBase := fdb_NOHDR;
                              EXIT;
                            end; { if }

  if NOT OpenTempBase(TempHDR_F, TempTXT_F, ReadMode + DenyAll,
                      'temphdr.$$$', 'temptxt.$$$', True)
                       then begin
                              CompressFileBase := fdb_NOHDR;
                              EXIT;
                            end; { if }

  {--------------------------- Lock the filebase ---------------------------}
  if NOT LockFileBase(HdrF) then
     begin
       CompressFileBase := fdb_NoLock;
       EXIT;
     end; { if }

  TotalItems := TempHDR_F^.FileSize DIV SizeOf(FilesHdrRecord);
  ItemCounter := 00;
  NumRead := 00;

  While (ItemCounter < TotalItems) AND (TotalItems > 00) do
    begin
      NumRead := TempHDR_F^.BlkRead(HdrInf, SizeOf(FilesHdrRecord));

      if HdrInf.LfnPtr > 0 then
        begin
          TempTXT_F^.Seek(HdrInf.LfnPtr);
          NumRead := TempTXT_F^.BlkRead(TextArray, Pred(SizeOf(TextArray)));
          if TxtF^.FilePos = 0 then
            TxtF^.Seek(1);

          HdrInf.LfnPtr := TXTF^.FilePos;

          if HdrInf.LfnPtr >= 00 then
             TXTF^.BlkWrite(TextArray[0], StrLen(TextArray) + 01);
        end; { if }

      if HdrInf.LongDescPtr <> -1 then
       begin
         TempTXT_F^.Seek(HdrInf.LongDescptr);
         Numread := TempTXT_F^.BlkRead(TextArray, Pred(SizeOf(TextArray)));
       end; { if }


      TextArray[NumRead] := #00;
      DeleteThisFile := ReadBit(HdrInf.Attrib, 00);
      Inc(ItemCounter);

      if NOT DeleteThisFile then
         begin
           if HdrInf.LongDescPtr >= 00 then
             HdrInf.LongDescPtr := TXTF^.FilePos
              else HdrInf.LongDescPtr := -1;

           HDRF^.BlkWrite(HdrInf, SizeOf(FilesHdrRecord));
           if HdrInf.LongDescPtr >= 00 then
               TXTF^.BlkWrite(TextArray[0], StrLen(TextArray) + 01);
         end; { if }
    end; { while }

  UnlockFileBase(HdrF);

  {--------------------------- Erase all the files -------------------------}
  Dispose(HdrF, Done);
  Dispose(TxtF, Done);
  Dispose(TempHDR_F, Done);
  Dispose(TempTXT_F, Done);

  EraseFile('temphdr.$$$');
  EraseFile('temptxt.$$$');
end; { func. CompressFileBase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure MaintFileBaseProc(var CurHDR: CfgRec.FilesHdrRecord;
                            var CurIDX: CfgRec.FilesIdxRecord;
                            var HdrF: pFileObj;
                            var IdxF: pFileObj;
                            LfnName: String;
                            RecNum : Longint);
var DoDelete : Boolean;
    OldHdr   : FilesHdrRecord;
    Txt      : ^TxtRec;
    CheckPath: String;
begin
  DoDelete := false;

  if OverrideRecNum then                { Should only be used by deletefile!! }
    if IsWildCard(GlobalfileName, LfnName) then
      GlobalRecNum := RecNum;

  if (RecNum = GlobalRecNum) then
    begin
       {-- We *have* to use KILL and KILL_LOCKED on recordnum rather than on -}
       {-- filename cause else the move/copy operations in EleMGR will -------}
       {-- malfunction completely --------------------------------------------}
       if (MaintOption = 'KILL') OR (MaintOption='KILL_LOCKED') then
         begin
           DoDelete := TRUE;

           if MaintOption = 'KILL' then
            if ReadBit(CurHdr.Attrib, 4) then                      { Locked }
              DoDelete := false;

           if FullDelete then
            if DoDelete then
             if (FileExist(GlobalAreaFileInfo.FilePath + LfnName)) OR
                 (IsFtpUrl(GlobalAreaFileInfo.FilePath)) then
                   EraseFileBaseFile(GlobalAreaFileInfo.FilePath + LfnName);

            Inc(TotalMoved);
            {------------------------- File deleted ---------------------------}
         end; { if kill }
     end; { we kill on recnum }

   if IsWildCard(GlobalfileName, LfnName) then
     begin
       if MaintOption = 'LOCK' then
        if CurHdr.Name[1] <> #00 then
          begin
            DoDelete := false;
            SetBit(CurHdr.Attrib, 4);
          end; { if }

       if MaintOption = 'UNLOCK' then
        if CurHdr.Name[1] <> #00 then
          begin
            DoDelete := false;
            ClearBit(CurHdr.Attrib, 4);
          end; { if }

       if MaintOption = 'UPDATE' then
        if CurHdr.Name[1] <> #00 then
          begin
              OldHdr := CurHdr;
              DoDelete := false;
              CheckPath := GlobalAreaFileInfo.FilePath;
              if IsFtpUrl(CheckPath) then
                CheckPath := GlobalEleFileInfo.FtpPath;

              GetFileBaseSizeAndTime(CheckPath + LfnName,
                                     CurHdr.Size, CurHdr.FileDate);

              if CurHdr.Size = 0 then
                SetBit(CurHdr.Attrib, 5)
                 else ClearBit(CurHdr.Attrib, 5);

              Case UpdateOption of
                00 : ; { don't do anything }
                01 : if (OldHdr.Size <> CurHdr.Size) OR
                         (OldHdr.FileDate <> CurHdr.FileDate) then
                          CurHdr.UploadDate := GetDosDate;
                02 : CurHdr.UploadDate := GetDosDate;
                03 : CurHdr.UploadDate := CurHdr.FileDate;
              end; { case }

              CurIdx.UploadDate := CurHdr.UploadDate;
          end; { if }
     end; { if matches }

  if NOT DoDelete then
    begin
      HdrF^.BlkWrite(CurHDR, SizeOf(FilesHdrRecord));
      IdxF^.BlkWrite(CurIDX, SizeOf(FilesIdxRecord));
    end; { if }

end; { proc. MaintFileBaseProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DeleteFile(const AreaNum   : Word;
                     const FName     : String;
                     const RecNum    : Longint;
                     const FullDelete: Boolean;
                     var   NrDeleted  : Word;
                     const DeleteOpt: DeleteFileOpt;
                     const AlsoLocked: Boolean;
                     const UseRecNum : Boolean);
var FileMgr: ^MgrFileObj;
begin
  GlobalFileName := FName;
  GlobalRecNum := RecNum;
  OverrideRecNum := (NOT UseRecnum);
  ElFile_U.FullDelete := FullDelete;
  GetFilesRecord(GlobalAreaFileInfo, AreaNum, true);

  if AlsoLocked then MaintOption := 'KILL_LOCKED' else
    MaintOption := 'KILL';
  TotalMoved := 00;

  New(FileMgr, Init(AreaNum, True));
  FileMgr^.FileBaseMaintenance(AreaNum,
                              {$IFDEF FPC}@{$ENDIF}MaintFileBaseProc,
                              false);
  Dispose(FileMgr, Done);

  NrDeleted := TotalMoved;
end; { proc. DeleteFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ExportFileBase(AreaNum     : Word;
                        ExportName  : String;
                        var ExportNr: Word;
                        AppendF     : Boolean;
                        RaCompat    : Boolean):fdb_ResultCodeType;
var FilesInf   : FilesRecord;
    TempBuf    : Array[0..16383] of Char;
    Exportfile : Text;

    TotalItems : Longint;
    ItemCounter: Longint;

    Counter    : Byte;
    NrLines    : Byte;
begin
  ExportFileBase := fdb_OK;
  ExportNr := 00;

{-------------------------------- Fetch filerecord ----------------------------}
  GetFilesRecord(FilesInf, AreaNum, true);

  {------------------------- Create output file -------------------------------}
  if ExportName='' then
    ExportName := FilesInf.FilePath + 'FILES.BBS';

  Assign(ExportFile, ExportName);
  SetTextBuf(ExportFile, TempBuf);
  if AppendF then
   {$i-} Append(ExportFile); {$I+}
  if (IOResult <> 00) OR (NOT AppendF) then
   {$i-} System.Rewrite(ExportFile); {$I+}

  if IOResult>00 then
       begin
          ExportFileBase := fdb_NoOutput;
          EXIT;
       end; { couldn't create export }

  {----------------------- Open the area info ---------------------------------}
  AreaObj.Init(FilesInf.AreaNum, false);
  if AreaObj.GetError <> 00 then
      begin
        ExportFileBase := fdb_NOIDX;
        {$i-} Close(Exportfile); {$i+}
        if IOresult>00 then ;

        AreaObj.Done;
        EXIT;
      end; { if }

  TotalItems := AreaObj.TotalRecords;
  ItemCounter := 00;

  {----------------------- Print all area items -------------------------------}
  While (ItemCounter < TotalItems) AND (TotalItems > 00) do
    begin
      AreaObj.Read(ItemCounter);                   { Read header sequentially }
      Inc(ItemCounter);
      Inc(ExportNr);

      AreaObj.ReadDescription(AreaObj.TxtF,
                              AreaObj.GetLongDescPtr); { Read the description }

      {--------------------- Print the information ----------------------------}
      if AreaObj.IsComment then
        begin
          While NOT AreaObj.EndOfDesc do
            WriteLn(ExportFile, AreaObj.GetDescLine(0));
        end; { is comment }

      if NOT AreaObj.IsComment then
        begin
          WriteLn(ExportFile, MakeLen(AreaObj.GetFileName, 13, Space, False, false),
                              AreaObj.GetDescLine(65));

          if RaCompat then
            begin
              While NOT AreaObj.EndOfDesc do
                WriteLn(ExportFile, '             ', AreaObj.GetDescLine(70));
            end
              else begin
                      While NOT AreaObj.EndOfDesc do
                         WriteLn(ExportFile, '+', AreaObj.GetDescLine(70));
                   end; { if }

        end; { if }
    end; { while }

  {$i-}
    AreaObj.Done;
    Close(ExportFile);
  {$i+}
  If IOResult>00 then;
end; { func. ExportFileBase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  LockUnlockFileBase(AreaNum : Word;
                             Option  : String;
                             FileName: String): fdb_ResultCodeType;
var FileMgr: MgrFileObj;
begin
  MaintOption := Option;
  GlobalFileName := FileName;
  OverrideRecNum := FALSE;

  FileMgr.Init(AreaNum, True);
  LockUnLockFileBase := FileMgr.FileBaseMaintenance(AreaNum,
                                                    {$IFDEF FPC}@{$ENDIF}MaintFileBaseProc,
                                                    false);
  FileMgr.Done;
end; { func. LockUnlockFileBase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure UpdateFile(AreaNum    : Word;
                     FileName   : String;
                     TouchMethod: Byte);
var FileMgr: MgrFileObj;
begin
  MaintOption := 'UPDATE';
  GlobalFileName := FileName;
  OverrideRecNum := FALSE;
  UpdateOption := TouchMethod;
  GetFilesRecord(GlobalAreaFileInfo, AreaNum, true);
  GetEleFilesRecord(GlobalEleFileInfo, AreaNum, true);

  FileMgr.Init(AreaNum, True);
  FileMgr.FileBaseMaintenance(AreaNum,
                              {$IFDEF FPC}@{$ENDIF}MaintFileBaseProc,
                              false);
  FileMgr.Done;
end; { proc. UpdateFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ConvertToExtensionNum(S: String): Byte;
var Counter: Byte;
begin
  ConvertToExtensionNum := 00;
  S := SUpCase(Trim(S));

  for Counter := 01 to 10 do
    if IsWildCard(SUpCase(Trim(GlobalCfg^.RaConfig^.ArcInfo[Counter].Extension)), S) then
      begin
        ConvertToExtensionNum := Counter;
        break;
      end; { if }
end; { func. ConvertToExtensionNum }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ExecArchive(Cmd: String; UnpackName: String): Word;
var Memory_To_use: Integer;
    MemorySwap   : Boolean;
    ExitCode     : Word;
    DoSwap       : Word;
    Return       : Integer;
    Path         : String;
    OldHeapEnd   : Pointer;
    Bat_F        : Text;
    TempNum      : Longint;
begin
 MemorySwap := Pos('*M', SUpCase(Cmd)) > 00;
 if CMd='' then exit;

{$IFNDEF WIN32}
 {$IFNDEF USE32BIT}
   Memory_To_use := HIDE_FILE or CHECK_NET or USE_FILE;
   If GlobalCfg^.RaConfig^.UseXMS then
    Memory_To_Use := Memory_To_Use or USE_XMS;
   If GlobalCfg^.RaConfig^.UseEMS then
    Memory_To_Use := Memory_To_Use or USE_EMS;

   If MemorySwap then DoSwap := $ffff else DoSwap := $0000;
 {$ENDIF}
{$ENDIF}

 Replace('@', UnpackName, Cmd);
 Replace('*M', '', Cmd);

 {$IFNDEF USE32BIT}
 If MemorySwap then
  begin;
   Path := FirstWord(Cmd, defExtractWord, false);

   Return := Do_Exec(Path, Cmd, Memory_To_Use, DoSwap, false);

   ExitCode := Lo(Return);

   Case Return of
    $0101 : RALog('!', 'SwapError: No space for swapping ...');
    $0102 : RALog('!', 'SwapError: Not enough memory to swap ...');
    $0200 : RALog('!', 'SwapError: File not found');
    $0201 : RALog('!', 'SwapError: Invalid drive');
    $0202 : RALog('!', 'SwapError: Invalid path');
    $0203 : RALog('!', 'SwapError: Invalid name');
    $0204 : RALog('!', 'SwapError: Invalid driveletter');
    $0205 : RALog('!', 'SwapError: Path too long..');
    $0206 : RALog('!', 'SwapError: Drive not ready');
    $0207 : RALog('!', 'SwapError: COMMAND.COM not found!');
    $0208 : RALog('!', 'SwapError: Error allocating temporary buffer');
    $0400 : RALog('!', 'SwapError: Error allocating environment buffer');
    $0500 : RALog('!', 'SwapError: Swap requested, but prep_swap not called/returned an error!!');
    $0501 : RALog('!', 'SwapError: MCBS don''t match expected setup!');
    $0502 : RALog('!', 'SwapError: Error while swapping out ...');
    $0600 : RALog('!', 'SwapError: Redirection syntax error');
   End; { Case }
  End  { If Swap }
   else begin;
          OldHeapEnd := HeapEnd;
          HeapEnd := HeapPtr;
          SetMemTop(HeapEnd);
 {$ENDIF}
          Path := FirstWord(Cmd, defExtractWord, true);

    {$IFNDEF DELPHI}
          SwapVectors;

          {$IFNDEF ELEUNIX}
            DOS.Exec(GetEnv('COMSPEC'), '/C '+ Path + #32 + Cmd);
          {$ELSE}
            SwapVectors;
            Randomize;
            TempNum := Random(1200);

            {----------------- Create a .SH file to execute this --------------}
            if FileExist(GlobalCfg^.RaConfig^.SysPath + 'x_tmp'+FStr(TempNum)+'.sh') then
              EraseFile(GlobalCfg^.RaConfig^.SysPath + 'x_tmp'+FStr(TempNum)+'.sh');

            Assign(Bat_F, GlobalCfg^.RaConfig^.SysPath + 'x_tmp'+FStr(Tempnum)+'.sh');
            {$i-}
              System.ReWrite(Bat_F);
              Writeln(Bat_F, '#!/bin/bash');
              Writeln(Bat_F, Path, #32, Cmd);
              Close(Bat_F);
              ChMod(GlobalCfg^.RaConfig^.SysPath + 'x_tmp' + FStr(TempNum) + '.sh', 493);
            {$i+}
            if IOResult>00 then ;

            Dos.Exec(GetEnv('SHELL'), GlobalCfg^.RaConfig^.SysPath + 'x_tmp' + FStr(TempNum) + '.sh');

            Assign(Bat_F, GlobalCfg^.RaConfig^.SysPath + 'x_tmp'+FStr(TempNum)+'.sh');
            {$i+} Erase(Bat_F); {$i+}
            if IoResult > 0 then ;

            SwapVectors;
          {$ENDIF}

          SwapVectors;

          ExitCode := Dos.DosExitCode;
    {$ENDIF}

 {$IFNDEF USE32BIT}
          HeapEnd := OldHeapEnd;
          SetMemTop(HeapEnd);
        end; { Don't swap }
{$ENDIF}

 ExecArchive := ExitCode;
end; { func. ExecArchive }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ReArcFileBaseProc(var CurHDR: CfgRec.FilesHdrRecord;
                            var CurIDX: CfgRec.FilesIdxRecord;
                            var HdrF: pFileObj;
                            var IdxF: pFileObj;
                            LfnName: String;
                            RecNum : Longint);
var ThisExtension: String;
    ThisArc      : ArcRecord;
    Curdir       : PathStr;

    OldName      : String;
    Temp_F       : pFileObj;
    FTime        : Longint;

    TempMgr      : MgrFileObj;
begin
  ThisExtension := JustExtension(LfnName);

  if ThisExtension <> GlobalCfg^.RaConfig^.ArcInfo[GlobalAreaFileInfo.ConvertExt].Extension then
   if (NOT ReadBit(CurHdr.Attrib, 5)) then
    begin
      if ConvertToExtensionNum(ThisExtension) <> 00 then
        begin
          KillCompleteDir(GlobalCfg^.RaConfig^.TempCDFilePath, false);
          ThisArc := GlobalCfg^.RaConfig^.ArcInfo[ConvertToExtensionNum(ThisExtension)];

          GetDir(0, CurDir);
          ChDir(NoBackSlash(GlobalCfg^.RaConfig^.TempCDFilePath));

          if ExecArchive(ThisArc.UnpackCmd, GlobalAreaFileInfo.FilePath + LfnName)=00 then
           with GlobalCfg^ do
            begin
              OldName := LfnName;
              CurHdr.Name := NoExtension(LfnName) + '.' + RaConfig^.ArcInfo[GlobalAreaFileInfo.ConvertExt].Extension;
              LfnName := NoExtension(LfnName) + '.' + RaConfig^.ArcInfo[GlobalAreaFileInfo.ConvertExt].Extension;

              if ExecArchive(GlobalCfg^.RaConfig^.ArcInfo[GlobalAreaFileInfo.ConvertExt].PackCMD, GlobalAreaFileInfo.FilePath +
                    LfnName)= 00 then
                      begin
                        { -------------- Get the original filetime ----------- }
                        New(Temp_F, Init);
                        Temp_F^.Assign(GlobalAreaFileinfo.FilePath + OldName);
                        Temp_F^.Open(1);

                        FTime := Temp_F^.GetFileTime;
                        Temp_F^.Close;
                        Temp_F^.Erase;
                        Dispose(Temp_F, Done);

                        Inc(TotalMoved);

                        { -------------- Set the original filename ----------- }
                        New(Temp_F, Init);

                        Temp_F^.Assign(GlobalAreaFileInfo.FilePath + LfnName);
                        Temp_F^.Open(1);
                        Temp_F^.SetFileTime(FTime);

                        Dispose(Temp_F, Done);
                      end else begin
                                 CurHdr.Name := OldName;
                                 LfnName := OldName;
                               end; { else }
               end; { if ExecArchive }

               ChDir(NoBackSlash(CurDir));
               KillCompleteDir(GlobalCfg^.RaConfig^.TempCDFilePath, False);
        end; { if recognized extension }
    end; { if }

  CurIdx.Name := CurHdr.Name;

  TempMgr.Init(GlobalAreaFileInfo.AreaNum, true);
  Curhdr.LfnPtr := TempMgr.AddLfnPtr(LfnName, GlobalAreaFileInfo.FilePath + LfnName, CurHdr.Name);
  TempMgr.Done;

  HdrF^.BlkWrite(CurHDR, SizeOf(FilesHdrRecord));
  IdxF^.BlkWrite(CurIDX, SizeOf(FilesIdxRecord));
end; { proc. ReArcFileBaseProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  ReArcFileBase(AreaNum      : Word;
                        var NrRearced: Word): fdb_ResultCodeType;
var FileMgr: MgrFileObj;
begin
  GetFilesRecord(GlobalAreaFileInfo, AreaNum, true);
  NrReArced := 00;

  {----------------- Check to see if everything is OK to cont.? ------------}
  if (NOT FileExist(GlobalCfg^.RaConfig^.TempCDFilePATH)) OR (Trim(GlobalCfg^.RaConfig^.TempCDFIlePath)='') then
     begin
       ReArcFileBase := fdb_NoTempPath;
       EXIT;
     end; { if }
  if NOT (GlobalAreaFileInfo.ConvertExt in [1..10]) then EXIT;
  if Trim(GlobalCfg^.RaConfig^.ArcInfo[GlobalAreaFileInfo.ConvertExt].Extension) = '' then EXIT;

  TotalMoved := 00;

  FileMgr.Init(AreaNum, True);
  ReArcFileBase := FileMgr.FileBaseMaintenance(AreaNum,
                                               {$IFDEF FPC}@{$ENDIF}ReArcFileBaseProc,
                                               false);
  FileMgr.Done;

  NrReArced := TotalMoved;
end; { func. ReArcFileBase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ImportFileBase(AreaNum: Word; FileName, UploaderName: String;
                        ColumnPos: Longint;
                        EraseFile, DoMissing: Boolean; var NrImported: Word):
         fdb_ResultCodeType;

var FileMgr   : MgrFileObj;

    HdrInf    : FilesHdrRecord;
    FilesInf  : FilesRecord;

    TmpName   : String;

    Text_F    : Text;
    TmpBuf    : Array[0..8191] of Char;

    SavePos   : Longint;

    DescArray : TxtRec;
    TempStr   : String;
    DescIDX   : Word;
    Counter   : Word;
begin
  ImportFileBase := fdb_OK;
  NrImported := 00;
  if ColumnPos = 0 then
    ColumnPos := 14;

  GetFilesRecord(FilesInf, AreaNum, true);
  if FileName = '' then FileName := FilesInf.FilePath + 'files.bbs';

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'ImportFileBase - FileName = '+FileName);
  {$ENDIF}

  Assign(Text_F, FileName);
  SetTextBuf(Text_F, TmpBuf);
  {$i-} System.Reset(Text_F); {$i+}
  if IOResult>00 then
       begin
         ImportFileBase := fdb_NoInput;
         EXIT;
       end; { if }

  SavePos := 00;
  FileMgr.Init(AreaNum, True);

  While NOT Eof(Text_F) do
    begin
      {$i-} ReadLn(Text_F, TempStr); {$i+}
      if IOResult > 00 then BREAK;

      if (FirstChar(Trim(TempStr)) <> '+') AND (Copy(TempStr, 1, 12) <> Dup(#32, 12)) then
        begin
           FillChar(HdrInf, SizeOf(FilesHdrRecord), #00);
           FillChar(DescArray, SizeOf(DescArray), #00);
           DescIDX := 00;

           {-- if the first character of the filename is an ", its considered-}
           {-- a long filename, lets see if we can extract it ----------------}
           if Copy(TempStr, 1, 1) = '"' then
             begin
               {-- extract the longfilename ----------------------------------}
               TmpName := ExtractWord(TempStr, 1, defExtractWord, true, false);

               {-- add this as a real long filename --------------------------}
               HdrInf.LfnPtr := FileMgr.AddLfnPtr(TmpName,
                                                  FilesInf.FilePath + TmpName,
                                                  HdrInf.Name);

               {-- now we fix this string to contain the shortname ----------}
               Replace('"' + TmpName + '"',
                       HdrInf.Name,
                       TempStr);

writeln('display: ', TempStr, ' / ', tmpname);
             end; { if }

           {-- update the header ---------------------------------------------}
           HdrInf.Name       := Trim(Copy(TempStr, 1, 12));

           GetFileBaseSizeAndTime(FilesInf.FilePath + HdrInf.Name,
                                  HdrInf.Size, HdrInf.FileDate);

           HdrInf.Crc32      := -1;
           HdrInf.Uploader   := UploaderName;
           HdrInf.LongDescPtr:= -1;                 { Set to no description }
           HdrInf.UploadDate := GetDosDate;
           HdrInf.LastDl     := HdrInf.FileDate;
           HdrInf.TimesDL    := 00;
           HdrInf.Attrib     := 00;
           HdrInf.Password   := '';
           HdrInf.Cost       := 00;

           TempStr := Copy(TempStr, ColumnPos, 255);
           for Counter := 01 to Length(TempStr) do
              DescArray[DescIDX + Pred(Counter)] := TempStr[Counter];
           Inc(DescIDX, Length(TempStr));

           if ((FileExist(FilesInf.FilePath + HdrInf.Name)) OR (DoMissing)) AND (HdrInf.Name <> '') then
             begin
                While NOT Eof(Text_F) do
                  begin
                    SavePos := TextFilePos(Text_F);

                    ReadLn(Text_F, TempStr);

                    if (FirstChar(TrimLeft(TempStr)) in ['+','|']) OR ((Copy(TempStr, 1, 12) = Dup(#32, 12))) then
                      begin
                         TempStr := TrimLeft(TempStr);

                         if (FirstChar(TrimLeft(TempStr)) in ['+','|']) then TempStr := #13 + #10 + Copy(TempStr, 2, 255)
                           else TempStr := #13 + #10 + TempStr;

                         for Counter := 01 to Length(TempStr) do
                           DescArray[DescIDX + Pred(Counter)] := TempStr[Counter];
                         Inc(DescIDX, Length(TempStr));
                      end
                        else begin
                                TextSeek(Text_F, SavePos);
                                BREAK;
                             end; { else }
                  end; { while }

                if SearchNameInArea(FilesInf.AreaNum, HdrInf.Name, false) < 00 then
                  begin
                    HdrInf.LongDescPtr := FileMgr.AddDescription(DescArray);
                    FileMgr.AddHeader(HdrInf);

                    Inc(NrImported);
                    AddStatusMsg(MakeLen(HdrInf.Name, 12, Space, False, false));
                  end; { if }

             end { if fileexist }
               else begin
                      if HdrInf.Name <> '' then
                        AddStatusMsg(MakeLen(HdrInf.Name, 12, Space, False, false) + ' (Missing/Offline)');
                     end; { else }
        end; { if TempStr[1] <> '+' }
    end; { while }

  FileMgr.Done;
  {$i-} Close(Text_F); {$i+}
  if IOResult>00 then ;
end; { func. ImportFileBase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ExtractDiz(const FileInfo : FilesRecord;
                    const FileName : String;
                    var   Txt      : TxtRec;
                    ForcedExtension: Byte): Boolean;

procedure ReadFile(FName: String; var Txt: TxtRec);
var TempF   : Text;
    TempStr : String;
begin
  FillChar(Txt, SizeOf(TxtRec), #00);

  Assign(TempF, FName);
  {$i-} System.Reset(TempF); {$I+}
  if IOResult = 00 then
   While NOT Eof(TempF) do
    begin
      {$i-} ReadLn(TempF, TempStr); {$i+}
      if IOResult>00 then BREAK;

      TempStr := TempStr + #13#10;
      Move(TempStr[1], Txt[StrLen(Txt)], Length(TempStr));
    end; { if }

  {$i-} Close(TempF); {$i+}
  if IOresult > 00 then ;
end; { proc. ReadFile }

var ExtNum       : Byte;
    ThisArc      : ArcRecord;
    CurDir       : String;
begin
  ExtractDiz := false;
  ExtNum := ConvertToExtensionNum(JustExtension(FileName));

  if ForcedExtension in [1..10] then
    ExtNum := ForcedExtension;

  if ExtNum > 00 then
   if GlobalCfg^.RaConfig^.TempCDFilePath <> '' then
    begin
      ThisArc := GlobalCfg^.RaConfig^.ArcInfo[ExtNum];

      if ThisArc.UnpackCmd <> '' then
        begin
          {$i-}
            GetDir(0, CurDir);
            ChDir(NoBackSlash(GlobalCfg^.RaConfig^.TempCDFilePath));
          {$i+}
          if IOResult > 00 then EXIT;

          EraseFile('file_id.diz');
          EraseFile('desc.sdi');

          {-- filenames will be quoted on non-DOS platforms for LFN compatibility --}
          {$IFDEF MSDOS}
          	ExecArchive(ThisArc.UnpackCmd, FileInfo.FilePath + FileName + ' file_id.diz desc.sdi');
          {$ELSE}
          	ExecArchive(ThisArc.UnpackCmd, '"' + FileInfo.FilePath + FileName + '" file_id.diz desc.sdi');
          {$ENDIF}          

          {---------------------- Actually read the files ---------------------}
          if FileExist('file_id.diz') then
            begin
              ReadFile('file_id.diz', Txt);
              ExtractDiz := true;
            end { if }
              else if FileExist('desc.sdi') then
                     begin
                       ReadFile('file_id.diz', Txt);
                       ExtractDiz := true;
                     end; { if }

          ChDir(NoBackSlash(CurDir));
          KillCompleteDir(GlobalCfg^.RaConfig^.TempCDFilePath, False);
        end; { if }
    end; { if }
end; { proc. ExtractDiz }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function WriteDiz(const FileInfo : FilesRecord;
                  const FileName : String;
                  var   Txt      : TxtRec;
                  ForcedExtension: Byte): Boolean;

procedure WriteFile(FName: String; var Txt: TxtRec);
var Temp_F: pFileObj;
begin
  New(Temp_F, Init);

  Temp_F^.Assign(FName);
  Temp_F^.Create(1);
  Temp_F^.BlkWrite(Txt, StrLen(Txt));

  Dispose(Temp_F, Done);
end; { proc. WriteFile }

var ExtNum       : Byte;
    ThisArc      : ArcRecord;
    CurDir       : String;
begin
  WriteDiz := false;
  ExtNum := ConvertToExtensionNum(JustExtension(FileName));

  if ForcedExtension in [1..10] then
    ExtNum := ForcedExtension;

  if ExtNum > 00 then
   if GlobalCfg^.RaConfig^.TempCDFilePath <> '' then
    begin
      ThisArc := GlobalCfg^.RaConfig^.ArcInfo[ExtNum];

      if ThisArc.PackCmd <> '' then
        begin
          {$i-}
            GetDir(0, CurDir);
            ChDir(NoBackSlash(GlobalCfg^.RaConfig^.TempCDFilePath));
          {$i+}
          if IOResult > 00 then EXIT;

          EraseFile('file_id.diz');
          EraseFile('desc_sdi');
          WriteFile('file_id.diz', Txt);

          if ExecArchive(ThisArc.PackCmd, FileInfo.FilePath + FileName + ' file_id.diz')=00 then
            begin
              WriteDiz := true;
            end; { if }

          ChDir(NoBackSlash(CurDir));
          KillCompleteDir(GlobalCfg^.RaConfig^.TempCDFilePath, False);
        end; { if }
    end; { if }
end; { proc. WriteDiz }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function DescribeFileBase(AreaNum: Word; FileName: String): fdb_ResultCodeType;
var FileMgr    : MgrFileObj;
    TotalItems : Longint;
    ItemCounter: Longint;
    Txt        : ^TxtRec;
begin
  DescribeFileBase := fdb_OK;
  GetFilesRecord(GlobalAreaFileInfo, AreaNum, true);

  {----------------- Check to see if everything is OK to cont.? ------------}
  if (NOT FileExist(GlobalCfg^.RaConfig^.TempCDFilePATH)) OR (Trim(GlobalCfg^.RaConfig^.TempCDFIlePath)='') then
     begin
       DescribeFileBase := fdb_NoTempPath;
       EXIT;
     end; { if }

  {-------------------- Get memory for the text buffer ----------------------}
  if NOT AllocMem(Txt, SizeOf(TxtRec), 'TxtRec', 'DescribeFileBase') then
    begin
      DescribeFileBase := fdb_NoMemory;
      EXIT;
    end; { if }

  if FileName = '' then FileName := '*.*';

  FileMgr.Init(AreaNum, True);

  TotalItems := FileMgr.TotalRecords;
  ItemCounter := 00;

   While (ItemCounter < TotalItems) AND (TotalItems>00) do
      begin
        FileMgr.Read(ItemCounter);
        Inc(ItemCounter);

        if FileMgr.GetLongDescPtr < 0 then
         if NOT FileMgr.IsComment then
          if IsWildCard(FileName, FileMgr.GetFileName) then
            begin
              if ExtractDiz(GlobalAreaFileInfo,
                            FileMgr.GetShortName,
                            Txt^,
                            0) then
                begin
                  FileMgr.CurHdr^.LongDescPtr := FileMgr.AddDescription(Txt^);

                  FileMgr.WriteBoth(Pred(FileMgr.CurrentRecord), FileMgr.CurHdr^);
                end; { if }
           end; { IsWildCard }
      end; { while }

  FileMgr.Done;
  ReleaseMem(Txt, SizeOf(TxtRec));
end; { proc. DescribeFileBase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
