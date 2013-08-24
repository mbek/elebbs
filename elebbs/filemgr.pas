unit FileMgr;
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
** FileMGR, RA v2.50 compatible FileMGR, part of EleMGR.
**
** Copyright (c) 1996, 97, 98 by Maarten Bekers
**
** Created : 13-Aug-1997
** Last update : 03-Jul-1998
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF ISCGI}
  This unit should not be compiled into EleWEB
{$ENDIF}

procedure OneMomentBox(var SaveScrn: Pointer; S: String; Restore: Boolean);
procedure FileMgr_Start;
procedure Mgr_MessageBox(S: String; WaitSecs: Longint);
procedure ShowMgrProgressBar(Cur, Total: Longint);
procedure CopyMoveFile(Dest, OrigNum: Word; StartRow, EndRow: Word; Moving,SearchDouble:Boolean; InsertRow: Word;
                       Mgr: Boolean);
function Mgr_EditBox(var EditStr: String; BeforeStr, TitleStr: String; MaxLen, AddLen,EditLen: Byte; Descrip: String;
                     AllUpper, OnlyNums, IsPw: Boolean): Char;
function ShortToLong(Path, FName: String): String;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses  Crt, LongStr, Multi,
      {$IFDEF MSDOS}
        Memory, Exec,
      {$ENDIF}

      {$IFDEF TCPIP}
        FtpUnit,
      {$ENDIF}

      {$IFDEF VirtualPascal}
        VpUtils,
      {$ENDIF}

      {$IFDEF ELEUNIX}
        {$IFDEF VER1_0}
          Linux,
        {$ELSE}
          OldLinux,
          Unix,
        {$ENDIF}
      {$ENDIF}

      {$IFDEF OS2}
        Os2Base,
      {$ENDIF}

      {$IFDEF WIN32}
        Windows,
      {$ENDIF}

      Dos, Ra2Fdb, MgrFdb, CfgRec, Debug_U, RAL, ScrnU, Colors,
      StrEdit, StUtils, Global, StrPath, Ranges, ElFile_U,
      FileRout, BitWise, ListSys, Strings, SysUtils, JDates,
      Cases, Editor, GenCfg, MemMan, MenuSys, GenFile,
      CentrStr, RecDif, Area_Lst, FileSys, Desc_U, Mgr_Addf,
      WordStr, ElLog_U, CfgScrn, FileObj, FormLst, ObjDec;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Const Global_EndRange   : Longint = -1;
      Global_StartRange : Longint = -1;
      Global_CurHilight : Longint = -1;

      DirectAbort       : Boolean = False;
      CurMoving         : Boolean = False;

      KeyWordToSearch   : String = '';
      FileNameToSearch  : String = '';

      AreaToStartSearch : Longint = 0;
      RecToStartSearch  : Longint = 0;

      {---------------------------------------------------------------------}
      GlobalOption      : String = '';
      Global_Bool       : Boolean = false;
      GlobalCount       : Longint = 00;
      GlobalPath        : String = '';
      {---------------------------------------------------------------------}

type DispModeSet = (disp_EleMGR, disp_Usermode);

var Area_F      : pFileObj;
    AreaObj     : MgrFileObj;

    GrpInf      : GroupRecord;
    FilesInf    : FilesRecord;
    EdittingArea: FilesRecord;
    EdittingEle : EleFilesRecord;
    HdrInf      : FilesHdrRecord;
    DisplayMode : DispModeSet;
{    CurHDR      : FilesHdrRecord; }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure OneMomentBox(var SaveScrn: Pointer; S: String; Restore: Boolean);
var XLen  : Byte;
    XStart: Byte;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'OneMomentBox (S="'+S+'", Restore='+Bool2Str(Restore)+') (begin)');
  {$ENDIF}

  if Restore then RestoreScreen(SaveScrn);

  if NOT Restore then
    begin
      SaveScreen(SaveScrn);

      if Length(S) > 75 then S[0] := #76;
      XLen := 02 + Length(S) + 02;
      XStart := (80 div 2) - (XLen div 2);

      ShadFillBoxTitle(XStart, 10, (XStart + Xlen) - 01, 14, Makeattr(LightBlue, Blue), mnuStyle, True, '');
      WriteAT(XStart + 02, 12, Makeattr(Lightgray, Blue), S);
    end; { if }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'OneMomentBox (S="'+S+'", Restore='+Bool2Str(Restore)+') ( end )');
  {$ENDIF}
end; { proc. OneMoment }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure LowToHigh(var StartRange, EndRange: Longint);
var Temp: Longint;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'LowToHigh (StartRange/EndRange='+FStr(StartRange) + ' / ' + FStr(EndRange) + ' (begin)');
  {$ENDIF}

  if StartRange > EndRange then
    begin
      Temp := StartRange;

      StartRange := EndRange;
      EndRange := Temp;
    end; { if }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'LowToHigh (StartRange/EndRange='+FStr(StartRange) + ' / ' + FStr(EndRange) + ' ( end )');
  {$ENDIF}
end; { proc. LowToHigh }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Mgr_MessageBox(S: String; WaitSecs: Longint);
var XLen    : Byte;
    XStart  : Byte;
    SaveScrn: Pointer;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'Mgr_MessageBox (S="'+S+'") (begin)');
  {$ENDIF}

  SaveScreen(SaveScrn);

  if Length(S) > 75 then S[0] := #76;
  XLen := 02 + Length(S) + 02;
  XStart := (80 div 2) - (XLen div 2);

  ShadFillBoxTitle(XStart, 10, (XStart + Xlen) - 01, 14, Makeattr(LightBlue, Blue), mnuStyle, True, ' ERROR ');
  WriteAT(XStart + 02, 12, Makeattr(Lightgray, Blue), S);

  if WaitSecs = 0 then
    begin
      Delay(500);
      GetInput;
    end
      else KeyDelay(WaitSecs, true);

  RestoreScreen(SaveScrn);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'Mgr_MessageBox (S="'+S+'") ( end )');
  {$ENDIF}
end; { proc. Mgr_MessageBox }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ShortToLong(Path, FName: String): String;
{$IFDEF WIN32}
  var FindHandle: THandle;
    {$IFDEF FPC}
      FindData  : WIN32_FIND_DATA;
    {$ELSE}
      FindData  : TWin32FindData;
    {$ENDIF}
{$ENDIF}
begin
  ShortToLong := FName;

  {$IFDEF WIN32}
     if NOT IsLfn(FName) then   { if this already is an LFN, skip it }
       begin
         { try to convert the shortnamed variant, to a longname variant }
         Path := ForceBack(Path) + FName + #0;

         FindHandle := FindFirstFile(PChar(AnsiString(Path)), FindData);
         if FindHandle <> INVALID_HANDLE_VALUE then
           begin
             Windows.FindClose(FindHandle);
             ShortToLong := StrPas(FindData.cFileName);
           end; { if }
       end; { if }
  {$ENDIF}
end; { func. ShortToLong }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Mgr_EditBox(var EditStr: String; BeforeStr, TitleStr: String; MaxLen, AddLen,EditLen: Byte; Descrip: String;
                     AllUpper, OnlyNums, IsPw: Boolean): Char;
var SaveScrn: Pointer;
    XLen    : Byte;
    XStart  : Byte;
    CH      : Char;
    Chars   : CharSet;
begin
  SaveScreen(SaveScrn);
  if OnlyNums then Chars := ['0'..'9']
    else Chars := [#32..#254];

  if MaxLen > 70 then MaxLen := 70;
  XLen := 02 + MaxLen + AddLen + Length(BeforeStr) + 02;
  XStart := (80 div 2) - (XLen div 2);

  if Descrip = '' then
      ShadFillBoxTitle(XStart, 10, (XStart + Xlen) - 01, 14, Makeattr(LightBlue, Blue), mnuStyle, True, TitleStr)
       else ShadFillBoxTitle(XStart, 08, (XStart + Xlen) - 01, 14, Makeattr(LightBlue, Blue), mnuStyle, True, TitleStr);

  If Descrip <> '' then
    WriteAT(XStart + 02, 12, MakeAttr(Lightgray, Blue), Descrip);
  if (BeforeStr <> '') then
   begin;
     if Descrip = '' then
       WriteAT(XStart + 02, 12, MakeAttr(Yellow, Blue), BeforeStr)
        else WriteAT(XStart + 02, 10, MakeAttr(Yellow, Blue), BeforeStr);
   end; { if }

  CursorOn;
  if EditLen = 00 then
   if MaxLen <> 00 then EditLen := MAxLen;

  if Descrip = '' then
    GetString(XStart + 02 + Length(BeforeStr), 12,MakeAttr(LightGray, Blue), EditStr, Chars, [#13, #27], EditLen, MaxLen,
              AllUpper, False, True, IsPw, 0)
     else GetString(XStart + 02 + Length(BeforeStr), 10, MakeAttr(Lightgray, Blue), EditStr, Chars, [#13, #27], EditLen,
                    MaxLen, AllUpper,False, True, IsPw, 0);

  Mgr_EditBox := rdLastkey;

  RestoreScreen(SaveScrn);
end; { func. Mgr_EditBox }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Mgr_QuestionBox(S: String; Title, HeadString: String; Chars: CharSet; EnterTo: Char): Char;
var SaveScrn: Pointer;
    XLen    : Byte;
    XStart  : Byte;
    CH      : Char;
begin
  SaveScreen(SaveScrn);

  if Length(S) > 75 then S[0] := #76;
  if Length(HeadString) > 75 then HeadString[0] := #75;

  if Length(S) > Length(HeadString) then XLen := 02 + Length(S) + 02
   else XLen := 02 + Length(HeadString) + 02;

  XStart := (80 div 2) - (XLen div 2);

  if HeadString = '' then
    ShadFillBoxTitle(XStart, 10, (XStart + Xlen) - 01, 14, Makeattr(LightBlue, Blue), mnuStyle, True, Title)
     else ShadFillBoxTitle(XStart, 09, (XStart + Xlen) - 01, 15, MakeAttr(LightBlue, Blue), mnuStyle, True, Title);
  if HeadString='' then WriteAT(XStart + 02, 12, MakeAttr(Lightgray, Blue), S)
   else begin
          WriteCenterPos(XStart, 11, XLen, MakeAttr(LightGray, Blue), HeadString);
          WriteAT(XStart + 02, 13, MakeAttr(LightGray, Blue), S);
        end; { else }

  repeat
    If HeadString = '' then
        WriteAT(XStart + (Length(S) + 01), 12, MakeAttr(Yellow, Blue), LastChar(S))
         else WriteAT(XStart + (Length(S) + 01), 13, MakeAttr(Yellow, Blue), LastChar(S));

    CursorOn;
    if HeadString <> '' then GotoXY(XStart + (Length(S) + 01), 13)
     else GotoXY(XStart + (XLen - 03), 12);

    CH := UpCase(StrEdit.Readkey);
    CursorOff;

    If HeadString = '' then
      WriteAT(XStart + (XLen - 03), 12, MakeAttr(Yellow, Blue), CH)
       else WriteAT(XStart + (Length(S) + 01), 13, MakeAttr(Yellow, Blue), CH);

    if CH=#13 then CH := EnterTo;
  until ((CH <> #00) AND (CH in Chars));

  Mgr_QuestionBox := CH;

  RestoreScreen(SaveScrn);
end; { func. Mgr_QuestionBox }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CreateNothing: Boolean;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'CreateNothing ('+FilesFileName+') (begin)');
  {$ENDIF}

  Mgr_MessageBox('Error occured while reading '+JustName(FilesFileName), 0);
  CreateNothing := False;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'CreateNothing ('+FilesFileName+') ( end )');
  {$ENDIF}
end; { proc. CreateNothing }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GlobalDoProc(var CurHDR: CfgRec.FilesHdrRecord;
                       var CurIDX: CfgRec.FilesIdxRecord;
                       var HdrF: pFileObj;
                       var IdxF: pFileObj;
                       LfnName: String;
                       RecNum : Longint);
var IsInRange: Boolean;
    FileMgr  : MgrFileObj;
    NameToUse: String;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'GlobalDoProc (Name='+CurHdr.Name+') (Inrange='+Bool2Str(InRange(GlobalCount+1,
                          Global_StartRange, Global_EndRange))+' GlobalOption='+GlobalOption+' (begin)');
  {$ENDIF}

  Inc(GlobalCount);   { This count is indepent of the real number of records }

  IsInRange := InRange(GlobalCount, Global_StartRange, Global_EndRange);

  if IsInRange then
    begin
      {------------------ Delete function ----------------------------------}
      if (GlobalOption = 'KILL') OR (GlobalOption = 'KILL_LOCKED') then
        begin
          GlobalFileName := LfnName;
          GlobalRecNum := RecNum;
          MaintOption := GlobalOption;
          FullDelete := Global_Bool;

          MaintFileBaseProc(CurHDR,
                            CurIDX,
                            HdrF,
                            IdxF,
                            LfnName,
                            RecNum);
        end; { if }

      {------------------------ Update function ----------------------------}
      if GlobalOption = 'UPDATE' then
        begin
          {$IFNDEF MSDOS}
          {$IFNDEF GO32V2}
          if SUpCase(ShortToLong(EdittingArea.FilePath, CurHdr.Name)) <> SUpCase(LfnName) then
            begin
              if mgr_QuestionBox('Convert shortname to long (Y,n)? °', CurHdr.Name, '', ['Y', 'N'], 'Y') = 'Y' then
                begin
                  LfnName := ShortToLong(EdittingArea.FilePath, CurHdr.Name);

                  FileMgr.Init(EdittingArea.AreaNum, True);
                  FileMgr.Read(RecNum);

                  CurHdr.LfnPtr := FileMgr.AddLfnPtr(LfnName,
                                                     EdittingArea.FilePath + LfnName,
                                                     CurHdr.Name);

                  FileMgr.Done;
                end; { if }
            end; { if }
          {$ENDIF}
          {$ENDIF}

          GlobalFileName := LfnName;
          MaintOption := 'UPDATE';
          UpdateOption := 00;

          MaintFileBaseProc(CurHDR,
                            CurIDX,
                            HdrF,
                            IdxF,
                            LfnName,
                            RecNum);
        end; { if }

      {-------------------------- Touch Function ---------------------------}
      if GlobalOption = 'TOUCH' then
        begin
          GlobalFileName := LfnName;
          MaintOption := 'UPDATE';

          if Global_Bool then UpdateOption := 03
           else UpdateOption := 02;

          MaintFileBaseProc(CurHDR,
                            CurIDX,
                            HdrF,
                            IdxF,
                            LfnName,
                            Recnum);
        end; { if }

      {-------------------------- Copy to path -----------------------------}
      if GlobalOption = 'COPYPATH' then
        begin
           NameToUse := ShortToLong(Edittingarea.FilePath, CurHdr.Name);

           if NOT FileCopy(EdittingArea.FilePath + NameToUse,
                           GlobalPath + NameToUse, true, false) then
             begin
               Mgr_MessageBox('Failed to copy '+NameToUse, 0);
             end; { if }

           IsInRange := false;                    { Let EleMGR copy itself }
        end; { if }
    end; { if in range }

  if NOT IsInRange then
    begin
      {$i-}
        HdrF^.BlkWrite(CurHDR, SizeOf(FilesHdrRecord));
        IdxF^.BlkWrite(CurIDX, SizeOf(FilesIdxRecord));
      {$i+}
      if IOResult>00 then ;
    end; { if }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'GlobalDoProc (Name='+CurHdr.Name+') (Inrange='+
             Bool2Str(InRange(GlobalCount, Global_StartRange, Global_EndRange))+' GlobalOption='+GlobalOption+' ( end )');
  {$ENDIF}
end; { proc. GlobalDoProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function OpenFileBase(AreaNum: Word): Boolean;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'OpenFileBase ('+FStr(AreaNum)+') (begin)');
  {$ENDIF}
  OpenFileBase := true;

  AreaObj.SaveAreaNum := AreaNum;
  AreaObj.OpenAreaFiles(true);

  if AreaObj.GetError > 00 then
    begin
      OpenFileBase := False;
      DirectAbort := True;
    end; { if }

  AreaObj.ResetCache;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'OpenFileBase ('+FStr(AreaNum)+') ( end )');
  {$ENDIF}
end; { func. OpenFileBase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CloseFileBase: Boolean;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'CloseFileBase (begin)');
  {$ENDIF}

  AreaObj.CloseAreaFiles;
  CloseFileBase := true;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'CloseFileBase ( end )');
  {$ENDIF}
end; { func. CloseFileBase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GlobalDo(Option     : String;
                   StartRange : Longint;
                   EndRange   : Longint;
                   HiLight    : Longint;
                   EmptyBool  : Boolean);
var SaveError: FDB_ResultCodeType;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'GlobalDo (Option='+Option+') (begin)');
  {$ENDIF}

  GlobalOption := Option;
  GlobalCount := 00;
  GlobalAreaFileInfo := EdittingArea;

  if (StartRange < 0) AND (EndRange < 0) then
    begin
      StartRange := HiLight;
      EndRange := HiLight;
    end; { if }

  Global_StartRange := StartRange;
  Global_EndRange := EndRange;
  Global_Bool := EmptyBool;

  LowToHigh(Global_StartRange, Global_EndRange);

  SaveError := AreaObj.FileBaseMaintenance(EdittingArea.AreaNum,
                              {$IFDEF FPC}@{$ENDIF}GlobalDoProc,
                              True);
  if( SaveError = fdb_OK) then
    begin
      SaveError := AreaObj.FileBaseMaintenance(EdittingArea.AreaNum,
                                  {$IFDEF FPC}@{$ENDIF}CreateIdxBaseProc,
                                  true);
    end; { if }

  AreaObj.ResetCache;

  { Now determine what the result was of the last filebase action }
  case SaveError of
    fdb_Ok        : ;
    fdb_NoHdr     : Mgr_MessageBox('Unable to properly open filedatabase', 0);
    fdb_NoTxt     : Mgr_MessageBox('Unable to open the textfiledatabase', 0);
    fdb_NoIdx     : Mgr_MessageBox('Unable to open index for filedatabase', 0);
    fdb_NoLock    : Mgr_MessageBox('Unable to acquire exclusive filebase lock', 0);
    fdb_NoTempPath: Mgr_MessageBox('Temp path does not exist or is invalid', 0);
    fdb_NoMemory  : Mgr_MessageBox('Not enough memory to proceed with this operation', 0);
  end; { case }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'GlobalDo (Option='+Option+') ( end )');
  {$ENDIF}
end; { proc. GlobalDO }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function AreaList_GetItems: LongInt;
begin
  AreaList_GetItems := AreaObj.TotalRecords + 01;
end; { func. AreaList_GetItems }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function HdrAttrib2Str(B: Byte): String;
{ ALSO UPDATE ELEMGR/WIN ! !!!! ! !! ! ! }
var TempStr: String;
begin
             {1234567}
  TempStr := 'FMDLNUT';

  if not ReadBit(B, 0) then TempStr[3] := '-';
  if not ReadBit(B, 1) then TempStr[6] := '-';
  if not ReadBit(B, 2) then TempStr[1] := '-';
  if not ReadBit(B, 3) then TempStr[5] := '-';
  if not ReadBit(B, 4) then TempStr[4] := '-';
  if not ReadBit(B, 5) then TempStr[2] := '-';
  if not ReadBit(B, 6) then TempStr[7] := '-';

  HdrAttrib2Str := TempStr;
end; { func. HdrAttrib2Str }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AreaList_GetInfo(var Info1, Info2, Info3, Info4, Info5, Info6, Info7, Info8, Info9: List_StringType;
                           ItemNr: LongInt; HiLight: LongInt);
var TempStr       : String;
    Counter       : Byte;
    HdrLines      : DisplayLinesArray;
    HeaderLines   : Byte;
    DisplayStr    : String;
    MissingPos    : Byte;
    AvailPos      : Byte;
    DescPos       : Byte;
    MissingLine   : String;
    AvailLine     : String;
    UseCR_Norm    : Boolean;
    UseCR_Missing : Boolean;
begin
  if ItemNr <> AreaList_GetItems then
    AreaObj.Read(Pred(ItemNr));

  if ItemNr = HiLight then
    begin
      PartClear(01, mnuScrnLen - 04, mnuScrnWidth, mnuScrnLen - 2, Lightgray, #32);
      PartClear(01, mnuScrnLen - 05, mnuScrnWidth, mnuScrnLen - 5, Blue, mnuSngWide);
      PartClear(01, mnuScrnLen - 01, mnuScrnWidth, mnuScrnLen - 1, Blue, mnuSngWide);
      WriteCenterPos(02, mnuScrnLen - 05, 68, Yellow, 'Area '+FStr(EdittingArea.AreaNum)+ ': '+EdittingArea.Name+
                     ' ('+FStr(AreaList_GetItems - 01)+' entries)');

      WriteAT(73, mnuScrnLen - 05, Blue, #180);
      WriteAT(79, mnuScrnLen - 05, Blue, #195);
      WriteAT(74, mnuScrnLen - 05, Yellow, StUtils.MakeLen(FStr(HiLight), 05, Space, True, false));

      if IsFtpUrl(EdittingArea.Filepath) then
        WriteAT(03, mnuScrnLen - 1, Yellow, ' (FTP) ');


      if NOT CurMoving then
        WriteCenter(mnuScrnLen, Lightgray, '(F1) Command summary')
         else WriteCenter(mnuScrnLen, Yellow, 'Select position to move to, and press (ENTER) or (ESC) to exit');

      if (ItemNr <> AreaList_GetItems) AND (NOT (AreaObj.IsComment)) then
       begin
         if AreaObj.GetLfnPtr > 0 then
           begin
             AreaObj.ReadDescription(AreaObj.LfnF, AreaObj.GetLfnPtr);
             TempStr := StrPas(AreaObj.CurTxt^);
             if Length(TempStr) > 60 then
               TempStr := Copy(TempStr, 1, 57) + '...';

             WriteAT(79 - (Length(TempStr) + 1), mnuScrnLen - 01, Blue, '(');
             WriteAT(79 - (Length(Tempstr)), mnuScrnLen - 01, Yellow, TempStr);
             WriteAT(79, mnuScrnLen - 01, Blue, ')');
           end; { if }


         AreaObj.ReadDescription(AreaObj.TxtF, AreaObj.GetLongDescPtr);

         for Counter := 01 to 03 do
           begin
             WriteAT(01, (mnuScrnLen - 05) + Counter, Lightgray, AreaObj.GetDescLine(80));
           end; { for }
       end; { if }
    end; { if hilight }

  if DisplayMode = disp_EleMGR then
    begin
      Info1 := #32 + AreaObj.GetShortName;
      Info2 := StUtils.MakeLen(AreaObj.GetSizeStr, 8, Space, True, false);
      Info3 := LangObj^.RaFormatDate(JDates.Date2Str(AreaObj.GetFileDate), 8, y2k_filedate, 0);
      Info4 := StUtils.MakeLen(FStr(AreaObj.GetTimesDL), 4, Zero, True, false);
      Info5 := LangObj^.RaFormatDate(JDates.Date2Str(AreaObj.GetLastDL), 8, y2k_sysdate, 0);
      Info6 := HdrAttrib2Str(AreaObj.GetAttrib);
      Info7 := AreaObj.GetKeyword(1);
      Info8 := '';
      Info9 := '';

      If Info1='' then Info1 := '[Unused]';
    end; { if }

  if DisplayMode = disp_Usermode then
    begin
      MissingLine := GlobalCfg^.RaConfig^.FileMissingLine;
      AvailLine := GlobalCfg^.RaConfig^.FileLine;

      GetDescriptionPos(MissingPos, MissingLine, False, UseCR_NORM);
      GetDescriptionPos(AvailPos, AvailLine, False, UseCR_MISSING);

      if AreaObj.IsMissing then
        begin
          DescPos := MissingPos;
          DisplayStr := MissingLine;
        end else begin
                   DescPos := AvailPos;
                   DisplayStr := AvailLine;
                 end; { if }

      AreaObj.ReadDescription(AreaObj.TxtF, AreaObj.GetLongDescPtr);
      DisplayListFile(AreaObj.CurHdr^,
                      DisplayStr, HdrLines,
                      AreaObj.GetDescLine(78 - DescPos), False, HeaderLines,
                      FilesInf, AreaObj.GetFileName, true);
      TempStr := HdrLines[0];
      RemoveRaColors(TempStr);

      Info1 := Copy(TempStr, 1, 14);
      Info2 := Copy(TempStr, 15, 9);
      Info3 := Copy(TempStr, 24, 12); { 10 }
      Info4 := Copy(TempStr, 36, 5);
      Info5 := Copy(TempStr, 41, 12); { 10 }
      Info6 := Copy(TempStr, 53, 8);
      Info7 := Copy(TempStr, 61, 25);
    end; { if }

  if AreaObj.IsComment then
    begin
      AreaObj.ReadDescription(AreaObj.TxtF, AreaObj.GetLongDescPtr);
      TempStr := AreaObj.GetDescLine(250);

      Info1 := Copy(TempStr, 1, 14);
      Info2 := Copy(TempStr, 15, 9);
      Info3 := Copy(TempStr, 24, 12); { 10 }
      Info4 := Copy(TempStr, 36, 5);
      Info5 := Copy(TempStr, 41, 12); { 10 }
      Info6 := Copy(TempStr, 53, 8);
      Info7 := Copy(TempStr, 61, 25);
    end; { if }

  if ItemNr = AreaList_GetItems then
    begin
      Info1 := '';
      Info2 := '';
      Info3 := '';
      Info4 := '';
      Info5 := '';
      Info6 := '';
      Info7 := '';
    end; { if }
end; { proc. AreaList_GetInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function AreaList_Seek(Str: String): LongInt;
var CurItem  : Longint;
    TotItems : Longint;

    ComparStr: String;
begin
  AreaList_Seek := -1;
  Str := SUpCase(Str);

  TotItems := AreaObj.TotalRecords;
  CurItem := 00;

  While (CurItem < TotItems) do
    begin
      Inc(CurItem);
      AreaObj.Read(Pred(CurItem));

      ComparStr := '';

      if AreaObj.IsComment then
        begin
          AreaObj.ReadDescription(AreaObj.TxtF, AreaObj.GetLongDescPtr);
          ComparStr := AreaObj.GetDescLine(80);
        end
          else begin
                 ComparStr := AreaObj.GetFileName;
                end; { else }

      if Str = SUpCase(Copy(ComparStr, 1, Length(Str))) then
        begin
          AreaList_Seek := AreaObj.CurrentRecord;
          BREAK
        end; { if }
    end; { while }
end; { func. AreaList_Seek }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditDescription(HiLight: Longint);
var EditX   : Longint;
    EditY   : Longint;
    SaveScrn: Pointer;
    Counter : Longint;
    TxtInf  : ^TxtRec;
    CH: Char;
begin
  EditX := 01;
  EditY := 01;

  WindowColor := MakeAttr(LightBlue, blue);
  NormColor := MakeAttr(Lightgray, Blue);

  SaveScreen(SaveScrn);

  DirectScrnUpdate := true;
  if NOT Editor_InitEditor(03, 07, 77, 20, true) then
    begin
      RestoreScreen(SaveScrn);
      EXIT;
    end; { if }

  LastString := 00;
  AreaObj.Read(Pred(HiLight));
  AreaObj.ReadDescription(AreaObj.TxtF, AreaObj.GetLongDescPtr);

  While (NOT AreaObj.EndOfDesc) AND (LastString < NumOfStrings) do
    begin
      Inc(Laststring);
      Txt^[LastString] := AreaObj.GetDescline(MaxStrLen);
    end; { while }

  Editor_Redraw;
  WriteAT(45 + 03, 06, MakeAttr(LightBlue, Blue), 'Â');
  Editor_Edit(EditX, EditY);

  DirectScrnUpdate := true;

  if Editor_Modified then
    if DoSaveChanges('Save changes (Y/n)? °', true, false) then
     begin
       if AllocMem(TxtInf, SizeOf(TxtRec), 'TxtRec', 'EditDescription') then
         begin
           FillChar(TxtInf^, SizeOf(TxtRec), #00);

           for Counter := 01 to LastString do
             begin
                Txt^[Counter] := Txt^[Counter] + #13#10;
                Move(Txt^[Counter][1], TxtInf^[StrLen(TxtInf^)], Length(Txt^[Counter]));
             end; { for }

           AreaObj.CurHdr^.LongDescPtr := AreaObj.AddDescription(TxtInf^);
           AreaObj.WriteBoth(Pred(AreaObj.CurrentRecord), AreaObj.CurHdr^);

           ReleaseMem(TxtInf, SizeOf(TxtRec));
         end; { if }
     end; { if }

  Editor_DoneEditor(EditX, EditY);
  RestoreScreen(SaveScrn);
end; { proc. EditDescription }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function SelectArcExtension: Byte;
var Menu     : PullRecord;
    Choice   : Word;
    CH       : Char;
    Items    : Byte;
    Counter  : Byte;
begin
  AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'SelectArcExtension');
  InitPullMenu(Menu);

  Items := 00;
  SelectArcExtension := 00;

  for Counter := 01 to 10 do
    With GlobalCfg^.RaConfig^.ArcInfo[Counter] do
     if (Extension <> '') AND (UnpackCmd <> '') then
       begin
         AddPullItem(Menu.PullInf^[Items + 01], ' ' +
            MakeLen(Extension, 3, Space, false, false) + ' ', 101 + Items , #00, '', 1);
         Inc(Items);
       end; { with }

  if Items > 00 then
   begin
     Menu.Items   := Items;
     Menu.Width   := 8;
     Menu.Length  := Items;
     Menu.X       := 01;
     Menu.Y       := 01;
     Menu.HiLight := 01;
     Menu.AddSpace:= False;
     Menu.Title   := ' EXT ';
     Menu.PosArray[1].XInc := 00;
     Menu.PosArray[1].YDec := 00;

     ShowMenu(Menu, False);

     Choice := DoPullMenu(Menu, CH, False, False);

     if Choice in [101..110] then
       SelectArcExtension := Choice - 100
        else SelectArcExtension := 00;
   end; { else }

  RemoveMenu(Menu);
  ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { func. SelectArcExtension }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ImportDizFile(HiLight: Longint; StartRange, EndRange: Longint);
var TxtInf  : ^TxtRec;
    Temp    : Byte;
    Counter : Longint;
    SaveScrn: Pointer;
begin
  SaveScreen(SaveScrn);
  LowToHigh(StartRange, EndRange);
  if (StartRange = -1) OR (EndRange = -1) then
    begin
      StartRange := HiLight;
      EndRange := HiLight;
    end; { if }

  for Counter := StartRange to EndRange do
    begin
      AreaObj.Read(Pred(Counter));

      if AllocMem(TxtInf, SizeOf(TxtRec), 'TxtRec', 'EditDescription') then
        begin
          PartClear(01, 01, mnuScrnWidth, mnuScrnLen, mnuNormColor, #32);

          if ExtractDiz(EdittingArea, AreaObj.GetFileName, TxtInf^, 0) then
            begin
              AreaObj.CurHdr^.LongDescPtr := AreaObj.AddDescription(TxtInf^);
              AreaObj.WriteBoth(Pred(AreaObj.CurrentRecord), AreaObj.CurHdr^);
            end { if }
             else begin
                    if ConvertToExtensionNum(JustExtension(AreaObj.GetFileName)) = 00 then
                      begin
                        Temp := SelectArcExtension;

                        if Temp in [1..10] then
                          begin
                            PartClear(01, 01, mnuScrnWidth, mnuScrnLen, mnuNormColor, #32);

                            if ExtractDiz(EdittingArea, AreaObj.GetFileName, TxtInf^, Temp) then
                              begin
                                AreaObj.CurHdr^.LongDescPtr := AreaObj.AddDescription(TxtInf^);
                                AreaObj.WriteBoth(Pred(AreaObj.CurrentRecord), AreaObj.CurHdr^);
                              end; { if }
                          end; { if }
                      end { if }
                        else begin
                               if (GlobalCfg^.RaConfig^.TempCDFilePath = '') OR
                                (NOT FileExist(ForceBack(GlobalCfg^.RaConfig^.TempCdFilePath))) then
                                 Mgr_MessageBox('CD-ROM temp path does not exist!', 0)
                                   else Mgr_MessageBox('Error occured (no FILE_ID?)', 2000);
                             end; { else }
                  end; { if }

          ReleaseMem(TxtInf, SizeOf(TxtRec));
        end; { if }
    end; { for counter }

  RestoreScreen(SaveScrn);
  CursorOff;
end; { proc. ImportDizFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure PutDizInArchive(var HiLight: Longint);
var Temp     : Byte;
    Temp_F   : pFileObj;
    SaveTime : Longint;
begin
  AreaObj.Read(Pred(HiLight));

  if AreaObj.GetLongDescPtr = -1 then
    begin
      MGR_MessageBox('Description is empty', 0);
      EXIT;
    end; { if }

  if (GlobalCfg^.RaConfig^.TempCdFilepath = '') OR (NOT FileExist(ForceBack(GlobalCfg^.RaConfig^.TempCdFilepath))) then
    begin
      MGR_MessageBox('CD-ROM temp path does not exist!', 0);
      EXIT;
    end; { if }

  if Mgr_QuestionBox('Overwrite these (y,N,esc)? °', ' CONFIRM ', 'Current archive might already contain an FILE_ID.DIZ',
                     ['Y', 'N', #27], 'N') <> 'Y' then
    begin
      EXIT;
    end; { if }

  PartClear(01, 01, mnuScrnWidth, mnuScrnLen, mnuNormColor, #32);
  SaveTime := GetPackedFileTime(EdittingArea.FilePath + AreaObj.GetFileName);

  if NOT WriteDiz(EdittingArea, AreaObj.GetFileName, AreaObj.CurTxt^, 0) then
    begin
      if ConvertToExtensionNum(JustExtension(AreaObj.GetFileName)) = 00 then
        begin
          Temp := SelectArcExtension;

          if Temp in [1..10] then
            begin
              PartClear(01, 01, mnuScrnWidth, mnuScrnLen, mnuNormColor, #32);

              WriteDiz(EdittingArea, AreaObj.GetFileName, AreaObj.CurTxt^, Temp);
            end; { if }
        end; { if }
    end; { if }


  if Mgr_QuestionBox('Keep filedate (Y,n)? °', ' CONFIRM ', '',
                     ['Y', 'N'], 'Y') = 'Y' then
    begin
      New(Temp_F, Init);

      Temp_F^.Assign(EdittingArea.FilePath + AreaObj.GetFileName);
      Temp_F^.FileMode := ReadMode + DenyNone;
      Temp_F^.Open(1);
      Temp_F^.SetFileTime(SaveTime);

      Dispose(Temp_F, Done)
    end; { if }

  CursorOff;
end; { proc. PutDizInArchive }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditComment(HiLight, HiBarPos: Longint);
var TempStr: String;
    TxtInf : TxtRec;
begin
  AreaObj.Read(Pred(HiLight));
  AreaObj.ReadDescription(AreaObj.TxtF, AreaObj.GetLongDescPtr);
  TempStr := AreaObj.GetDescLine(120);

  GetString(01, HiBarPos + 02, mnuEditColor, TempStr, [#32..#254], [#13,#27], 120, 80, false, false,True,false,0);

  FillChar(TxtInf, SizeOf(TxtRec), #00);
  Move(TempStr[1], TxtInf, Length(TempStr));

  AreaObj.CurHdr^.LongDescPtr := AreaObj.AddDescription(TxtInf);
  AreaObj.WriteBoth(Pred(HiLight), AreaObj.CurHdr^);
end; { proc. EditComment }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditFileHdr(var HdrInf: FilesHdrRecord; HiLight, HibarPos: Longint; ShiftState: Boolean);
var FieldEdit  : Integer;
    NrLines    : Byte;

procedure PaddingWrite(X,Y,Attr,Len: Byte; S: String);
begin
  WriteAT(X,Y,Attr, Dup(mnuBlockChar, Len));
  WriteAT(X,Y,Attr, S);
end; { proc. PaddingWrite }

procedure WriteAbove(S: String);
begin
  PartClear(01, 01, mnuScrnWidth, 01, Yellow, #32);
  WriteCenter(01, Yellow, S);
end; { proc. WriteAbove }

procedure EditWord(X,Y,A: Byte; var W: SmallWord);
var Temp: String;
begin
   Temp := FStr(W);
   GetString(X,Y,A, Temp, ['0'..'9'], [#27, #13, #72, #80, #60], 5, 5, False, False, False, false,0);
   W := SmallWord((FVal(Temp)));
end; { proc. EditWord }

procedure EditDate(X,Y,A: Byte; var Date: Longint);
var TempStr  : String;
    FieldEdit: Byte;
    DateStr  : String;
    TempDT   : DateTime;
begin
  TempStr := JDates.Date2Str(Date);

  if rdLastKey = #72 then FieldEdit := 03 { going up }
    else FieldEdit := 01;

  repeat;
    WriteAT(X,Y, Lightgray, TempStr);

    Case FieldEdit of
      01 : DateStr := Copy(TempStr, 1, 2);
      02 : DateStr := Copy(TempStr, 4, 2);
      03 : DateStr := Copy(TempStr, 7, 2);
    end; { case }

    Case FieldEdit of
      01 : GetString(X, Y, A, DateStr, ['0'..'9'], [#27, #13, #72, #80, #60], 2, 2, False, False, False, false,0);
      02 : GetString(X + 03 , Y, A, DateStr, ['0'..'9'], [#27, #13, #72, #80, #60], 2, 2, False, False, False, false,0);
      03 : GetString(X + 06, Y, A, DateStr, ['0'..'9'], [#27, #13, #72, #80, #60], 2, 2, False, False, False, false,0);
    end; { case }

    DateStr := StUtils.MakeLen(DateStr, 02, Zero, True, false);

    Case FieldEdit of
     01 : begin
            TempStr[01] := DateStr[1];
            TempStr[02] := DateStr[2];
          end; { 01 }
     02 : begin
            TempStr[04] := DateStr[1];
            TempStr[05] := DateStr[2];
          end; { 02 }
     03 : begin
            TempStr[07] := DateStr[1];
            TempStr[08] := DateStr[2];
          end; { 03 }
    end; { case }

    Case rdLastKey of
     { Enter }  #13 : Inc(FieldEdit);
     { Up }     #72 : Dec(FieldEdit);
     { Down }   #80 : Inc(FieldEdit);
     { Escape } #27 : FieldEdit := 00;
     { F2 }     #60 : FieldEdit := 00;
    end; { case }

    if rdLastKey in [#80, #13] then
     if FieldEdit > 3 then FieldEdit := 0;

  until FieldEdit = 00;

  UnpackTime(Date, TempDt);
  TempDT.Month := FVal(Copy(TempStr, 1, 2));
  TempDT.Day := FVal(Copy(TempStr, 4, 2));
  TempDT.Year := FVal(Copy(TempStr, 7, 2));

  if TempDt.Year < 80 then Inc(TempDt.Year, 2000)
   else Inc(TempDT.Year, 1900);

  PackTime(TempDT, Date);
end; { proc. EditDate }

procedure EditYesNo(X,Y,A: Byte; var Attrib: Byte; B: Byte);
var CH: Char;
begin
  repeat
    if ReadBit(Attrib, B) then WriteAT(X,Y,A, 'Y')
     else WriteAT(X,Y,A, 'N');
    UpdateScreenBuffer(true);

    CursorOn;
    GotoXY(X,Y);
    CH := UpCase(ReadKey);
    if Ch=#00 then Ch := ReadKey;

    if CH = 'Y' then SetBit(Attrib, B);
    if CH = 'N' then ClearBit(Attrib, B);

  until (CH in [#13, #72, #80, #27, #60, 'Y', 'N']);

  if CH in ['Y', 'N'] then rdLastkey := #80;
end; { proc. EditYesNo }

function YNChar(Attrib,B: Byte): Char;
begin
  if ReadBit(Attrib, B) then YnChar := 'Y'
   else YnChar := 'N';
end; { func. YnChar }

procedure ShowPaddings;
begin
  PaddingWrite(15, 06, Lightgray, 35, HdrInf.Uploader);
  PaddingWrite(15, 07, Lightgray, 15, HdrInf.Password);
  PaddingWrite(15, 08, Lightgray, 15, HdrInf.Keyword[1]);
  PaddingWrite(15, 09, Lightgray, 15, HdrInf.Keyword[2]);
  PaddingWrite(15, 10, Lightgray, 15, HdrInf.Keyword[3]);
  PaddingWrite(15, 11, Lightgray, 15, HdrInf.Keyword[4]);
  PaddingWrite(15, 12, Lightgray, 15, HdrInf.Keyword[5]);
  PaddingWrite(15, 13, Lightgray, 05, FStr(HdrInf.TimesDL));
  PaddingWrite(15, 14, Lightgray, 05, FStr(HdrInf.Cost));

  PaddingWrite(15, 16, Lightgray, 8, JDates.Date2Str(HdrInf.UploadDate));
  PaddingWrite(15, 17, Lightgray, 8, JDates.Date2Str(HdrInf.LastDL));

  PaddingWrite(71, 06, Lightgray, 1, YNChar(HdrInf.Attrib, 0));
  PaddingWrite(71, 07, Lightgray, 1, YNChar(HdrInf.Attrib, 1));
  PaddingWrite(71, 08, Lightgray, 1, YNChar(HdrInf.Attrib, 2));
  PaddingWrite(71, 09, Lightgray, 1, YNChar(HdrInf.Attrib, 4));
  PaddingWrite(71, 10, Lightgray, 1, YNChar(HdrInf.Attrib, 3));
  PaddingWrite(71, 11, Lightgray, 1, YNChar(HdrInf.Attrib, 5));
  PaddingWrite(71, 12, Lightgray, 1, YNChar(HdrInf.Attrib, 6));
end; { proc. ShowPaddings }

var SaveDirect: Boolean;
    TempStr   : String;
    TxtInf    : TxtRec;
    Counter   : Byte;
begin
  if (HdrInf.Name[0]=#00) AND (NOT ShiftState) then
    begin
      EditComment(HiLight, HibarPos);
      EXIT;
    end; { isComment }

  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;

  Partclear(01, 01, mnuScrnWidth, mnuScrnLen, White, #32);
  PartClear(01, 02, mnuScrnWidth, 02, Blue, mnuDblWide);
  PartClear(01, mnuScrnLen - 01, mnuScrnWidth, mnuScrnLen - 01, Blue, mnuSngWide);
  WriteAT(02, 04, Yellow, 'File: '+HdrInf.Name+', '+Fstr(HdrInf.Size)+' bytes');
  WriteAT(40, 04, Yellow, 'Date: '+LangObj^.RaFormatDate(Jdates.Date2Str(HdrInf.FileDate), 8, y2k_FileDate, 0));

  WriteAT(02, 06, Lightgray, 'Uploader   : ');
  WriteAT(02, 07, Lightgray, 'Password   : ');
  WriteAT(02, 08, Lightgray, 'Keyword #1 : ');
  WriteAT(02, 09, Lightgray, 'Keyword #2 : ');
  WriteAT(02, 10, Lightgray, 'Keyword #3 : ');
  WriteAT(02, 11, Lightgray, 'Keyword #4 : ');
  WriteAT(02, 12, Lightgray, 'Keyword #5 : ');
  WriteAT(02, 13, Lightgray, 'Downloads  : ');
  WriteAT(02, 14, Lightgray, 'Cost       : ');

  WriteAT(02, 16, Lightgray, 'UL Date  : ');
  WriteAT(02, 17, Lightgray, 'Last DL  : ');


  WriteAT(60, 06, Lightgray, 'Deleted  : ');
  WriteAT(60, 07, Lightgray, 'Unlisted : ');
  WriteAT(60, 08, Lightgray, 'Free     : ');
  WriteAT(60, 09, Lightgray, 'Locked   : ');
  WriteAT(60, 10, Lightgray, 'NotAvail : ');
  WriteAT(60, 11, Lightgray, 'Missing  : ');
  WriteAT(60, 12, Lightgray, 'NoTime   : ');

  WriteCenter(mnuScrnLen, White, '(ESC) Exit');

  FieldEdit := 01;
  CursorOn;

  PartClear(01, mnuScrnLen - 05, mnuScrnWidth, mnuScrnLen - 05, Blue, mnuSngWide);
  WriteAT(02, 20, Yellow, 'Description (F2 to edit)');

  AreaObj.ReadDescription(AreaObj.TxtF, AreaObj.GetLongDescPtr);
  For Counter := 01 to 3 do
    WriteAT(01, (mnuScrnlen - 05) + Counter, Lightgray, AreaObj.GetDescLine(80));

  repeat
    DirectScrnUpdate := false;
    ShowPaddings;

    Case FieldEdit of
      01 : WriteAbove('Full name of the person who uploaded this file');
      02 : WriteAbove('Password to download this file');
      03 : WriteAbove('First keyword checked in keyword search');
      04 : WriteAbove('Second keyword checked in keyword search');
      05 : WriteAbove('Third keyword checked in keyword search');
      06 : WriteAbove('Fourth keyword checked in keyword search');
      07 : WriteAbove('Fifth keyword checked in keyword search');
      08 : WriteAbove('Number of times this file has been downloaded');
      09 : WriteAbove('Total cost to user to download this file');
      10 : WriteAbove('Date this file was uploaded');
      11 : WriteAbove('Date this file was last downloaded');
      12 : WriteAbove('Mark this file to be deleted');
      13 : WriteAbove('File should not be listed');
      14 : WriteAbove('This file is free and does not affect download ratios');
      15 : WriteAbove('Locked (cannot be moved/deleted)');
      16 : WriteAbove('Not available (no download allowed)');
      17 : WriteAbove('This file is temporarily offline - still displayed');
      18 : WriteAbove('No time restrictions on this file - always allow DL - zero-time transfer');
    end; { case }

    DirectScrnUpdate := false;
    UpdateScreenBuffer(true);

    Case FieldEdit of
      01 : GetString(15, 06, Yellow, HdrInf.Uploader, [#32..#254], [#27, #13, #72, #80, #60],35,35,False,False,False,false,0);
      02 : GetString(15, 07, Yellow, HdrInf.Password, [#32..#254], [#27, #13, #72, #80, #60],15,15,False,False,False,false,0);
      03 : GetString(15, 08, Yellow, HdrInf.KeyWord[1], [#32..#254],[#27, #13, #72, #80, #60],15,15,False,False,False,false,0);
      04 : GetString(15, 09, Yellow, HdrInf.KeyWord[2], [#32..#254],[#27, #13, #72, #80, #60],15,15,False,False,False,false,0);
      05 : GetString(15, 10, Yellow, HdrInf.KeyWord[3], [#32..#254],[#27, #13, #72, #80, #60],15,15,False,False,False,false,0);
      06 : GetString(15, 11, Yellow, HdrInf.KeyWord[4], [#32..#254],[#27, #13, #72, #80, #60],15,15,False,False,False,false,0);
      07 : GetString(15, 12, Yellow, HdrInf.KeyWord[5], [#32..#254],[#27, #13, #72, #80, #60],15,15,False,False,False,false,0);
      08 : EditWord(15, 13, Yellow, HdrInf.TimesDL);
      09 : EditWord(15, 14, Yellow, HdrInf.Cost);

      10 : EditDate(15, 16, Yellow, HdrInf.UploadDate);
      11 : EditDate(15, 17, Yellow, HdrInf.LastDL);

      12 : EditYesNo(71, 06, Yellow, Hdrinf.Attrib, 0);
      13 : EditYesNo(71, 07, Yellow, Hdrinf.Attrib, 1);
      14 : EditYesNo(71, 08, Yellow, Hdrinf.Attrib, 2);
      15 : EditYesNo(71, 09, Yellow, Hdrinf.Attrib, 4);
      16 : EditYesNo(71, 10, Yellow, Hdrinf.Attrib, 3);
      17 : EditYesNo(71, 11, Yellow, Hdrinf.Attrib, 5);
      18 : EditYesNo(71, 12, Yellow, Hdrinf.Attrib, 6);
    end; { case }

    DirectScrnUpdate := false;
    ShowPaddings;

    Case rdLastKey of
     { Enter }  #13 : Inc(FieldEdit);
     { Up }     #72 : Dec(FieldEdit);
     { Down }   #80 : Inc(FieldEdit);
     { Escape } #27 : FieldEdit := -1;
     { F2 }     #60 : begin;
                         EditDescription(HiLight);

                         AreaObj.ReadDescription(AreaObj.TxtF, AreaObj.GetLongDescPtr);
                         PartClear(01, mnuScrnLen - 04, mnuScrnWidth, mnuScrnLen - 2, Lightgray, #32);
                         for Counter := 01 to 3 do
                           WriteAT(01, (mnuScrnlen - 05) + Counter, Lightgray, AreaObj.GetDescLine(80));
                      end; { if F2 }
    end; { case }

    if FieldEdit > 18 then FieldEdit := 01;
    if FieldEdit = 00 then FieldEdit := 18;

  until FieldEdit < 00;

  CursorOff;
  DirectScrnUpdate := true;
end; { proc. EditFileHdr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function AreaList_Activate(HiLight: LongInt; HiBarPos: LongInt): Longint;
var OldHdr    : FilesHdrRecord;
    SaveScrn  : Pointer;
    ShiftState: Boolean;
begin
  AreaList_Activate := -1;
  if HiLight = AreaList_GetItems then EXIT;

  AreaObj.Read(Pred(HiLight));

  SaveScreen(SaveScrn);

  OldHdr := AreaObj.CurHdr^;
  ShiftState := StrEdit.ShiftState;
  EditFileHdr(AreaObj.CurHdr^, HiLight, HiBarPos, ShiftState);

  if (NOT AreaObj.IsComment) OR (ShiftState) then
   if RecordsDifferent(AreaObj.CurHdr^, OldHdr, SizeOf(FilesHdrRecord)) then
    if DoSaveChanges('Save changes (Y/n)? °', True, false) then
      AreaObj.WriteBoth(Pred(HiLight), AreaObj.CurHdr^);

  RestoreScreen(SaveScrn);
end; { proc. AreaList_Activate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Show2ndHelpScreen;
var Counter: Longint;
begin
  ShadFillBoxTitle(01, 01, 80, mnuScrnLen, Makeattr(LightBlue, Blue), mnuStyle, True, ' Command summary ');

  WriteAT(03, 02, MakeAttr(Lightgray, Blue), '(ALT-W)        Write description to archive');
  WriteRight(79, mnuScrnLen, MakeAttr(Yellow, Blue), ' Press any key to return ');

  GetInput;
end; { proc. Show2ndHelpScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowHelpScreen;
var SaveScrn: Pointer;
begin
  SaveScreen(SaveScrn);
  ShadFillBoxTitle(01, 01, 80, mnuScrnLen, Makeattr(LightBlue, Blue), mnuStyle, True, ' Command summary ');

  WriteAT(03, 02, MakeAttr(Lightgray, Blue), '(ESC)          Escape back to file area selection list');
  WriteAT(03, 03, MakeAttr(Lightgray, Blue), '(ENTER)        Edit the highlighted file or comment');
  WriteAT(03, 04, MakeAttr(Lightgray, Blue), '(F2)           Edit the description of the highlighted file');
  WriteAT(03, 05, MakeAttr(Lightgray, Blue), '(SPACE)        Drop anchor for file block');
  WriteAT(03, 06, MakeAttr(Lightgray, Blue), '(DEL)          Delete the highlighted file(s)');
  WriteAT(03, 07, MakeAttr(Lightgray, Blue), '(INSERT)       Insert a new file entry at the highlight position');
  WriteAT(03, 08, MakeAttr(Lightgray, Blue), '(SHIFT-INSERT) Insert a new comment at the highlight position');
  WriteAT(03, 09, MakeAttr(Lightgray, Blue), '(ALT-A)        Adopt orphaned files');
  WriteAT(03, 10, MakeAttr(Lightgray, Blue), '(ALT-B)        Buffer operations (copy, paste, edit, import, export)');
  WriteAT(03, 11, MakeAttr(Lightgray, Blue), '(ALT-C)        Copy highlighted file(s) to another area');
  WriteAT(03, 12, MakeAttr(Lightgray, Blue), '(ALT-D)        Toggle file listing display format (user mode is slower)');
  WriteAT(03, 13, MakeAttr(Lightgray, Blue), '(ALT-F)        Find a file (wildcards valid)');
  WriteAT(03, 14, MakeAttr(Lightgray, Blue), '(ALT-G)        Generate a text file from the current area (FILES.BBS)');
  WriteAT(03, 15, MakeAttr(Lightgray, Blue), '(ALT-H)        Hurl (move) highlighted file(s) to another area');
  WriteAT(03, 16, MakeAttr(Lightgray, Blue), '(ALT-I)        Import a text file as comment lines');
  WriteAT(03, 17, MakeAttr(Lightgray, Blue), '(ALT-K)        Find a file (keyword description search)');
  WriteAT(03, 18, MakeAttr(Lightgray, Blue), '(ALT-M)        Move highlighted file(s) within the current area');
  WriteAT(03, 19, MakeAttr(Lightgray, Blue), '(ALT-P)        Put a copy of the highlighted file(s) in any directory');
  WriteAT(03, 20, MakeAttr(Lightgray, Blue), '(ALT-R)        Rename the highlighted file');
  WriteAT(03, 21, MakeAttr(Lightgray, Blue), '(ALT-S)        Sort highlighted files');
  WriteAT(03, 22, MakeAttr(Lightgray, Blue), '(ALT-T)        Touch upload date on highlighted files');
  WriteAT(03, 23, MakeAttr(Lightgray, Blue), '(ALT-U)        Update highlighted entries from OS information');
  WriteAT(03, 24, MakeAttr(Lightgray, Blue), '(ALT-V)        Extract description from archive');
  WriteRight(79, mnuScrnLen, MakeAttr(Yellow, Blue), ' Press any key to return ('+#25+' for more)');

  if Readkey = #00 then
   if ReadKey = #80 then
    begin
      Show2ndHelpScreen;
    end; { if }

  RestoreScreen(SaveScrn);
end; { proc. ShowHelpScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DeleteFiles(var HiLight: Longint; var StartRange, EndRange: Longint);
var TempCH   : Char;
    SaveScrn : Pointer;
begin
  AreaObj.Read(HiLight - 01);
  TempCH := 'Y';

  if (AreaObj.IsLocked) AND (NOT AreaObj.IsComment) then
    begin
      Mgr_MessageBox('Cannot DELETE a LOCKED file!', 0);
      EXIT;
    end; { if }

  if ((NOT FileExist(EdittingArea.FilePath + AreaObj.GetFileName)) AND (NOT IsFtpUrl(EdittingArea.FilePath))
       OR (ReadBit(EdittingArea.Attrib, 3)))
        OR (AreaObj.isComment) OR (AreaObj.IsMissing) then TempCH := 'N' else
    begin
      TempCH := Mgr_QuestionBox('Delete files as well as database entries (y,N,esc)? °',
                                ' CONFIRM ', '', ['Y', 'N', #27], 'N');
    end; { if }

  OneMomentBox(SaveScrn, 'One moment ..', False);

  if TempCH in ['Y', 'N'] then
    begin
      GlobalDo('KILL_LOCKED',
               StartRange,
               EndRange,
               HiLight,
               (TempCH='Y'));
    end; { if }

  OneMomentBox(SaveScrn, '', True);

  HiLight := -1;
  Global_Startrange := -1;
  Global_EndRange   := -1;
  Global_CurHilight := -1;
  StartRange := -1;
  EndRange := -1;
end; { proc. DeleteFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure UpdateFiles(HiLight: Longint; var StartRange, EndRange: Longint);
var SaveScrn : Pointer;
begin
  OneMomentBox(SaveScrn, 'One moment ..', False);

  GlobalDo('UPDATE',
           StartRange,
           EndRange,
           HiLight,
           false);

  OneMomentBox(SaveScrn, 'One moment ..', True);

  Global_Startrange := -1;
  Global_EndRange   := -1;
  Global_CurHilight := -1;
end; { proc. UpdateFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TouchFiles(HiLight: Longint; var StartRange, EndRange: Longint);
var SaveScrn   : Pointer;
    TempBool   : Boolean;
    TempChar   : Char;
begin
  TempChar := Mgr_QuestionBox('(F)ile or (T)oday''s date? °', ' Touch upload date ', '', ['F', 'T', #27], #00);

  Case TempChar of
    'F' : TempBool := true;   { 03 }
    'T' : TempBool := false;  { 02 }
  end; { case }

  if TempChar in ['F', 'T'] then
     begin
       OneMomentBox(SaveScrn, 'One moment ..', False);

       GlobalDo('TOUCH',
                StartRange,
                EndRange,
                HiLight,
                TempBool);

        OneMomentBox(SaveScrn, 'One moment ..', True);
     end; { if }

  Global_Startrange := -1;
  Global_EndRange   := -1;
  Global_CurHilight := -1;
end; { proc. TouchFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function RenameFileBaseFile(Orig, Dest: String): Boolean;
var Temp_F : pFileObj;
{$IFDEF TCPIP}
    ftpObj: pFtpInterfaceObj;
{$ENDIF}
begin
  if NOT IsFtpUrl(Orig) then
    begin
      New(Temp_F, Init);
      Temp_F^.Assign(Orig);
      RenameFileBaseFile := RenameObjWithDrive(Temp_F, Dest);
      Dispose(Temp_F, Done);
    end { if }
      else begin
             RenamefileBaseFile := false;

             {$IFDEF TCPIP}
               New(ftpObj, Init);
               if ftpObj^.StartUpFtp(Orig, '', '') then
                 begin
                   if ftpObj^.FtpRenameFile(JustPath(Orig),
                                    JustName(Orig),
                                    JustName(Dest)) then RenameFileBaseFile := true;
                 end; { if }

               ftpObj^.DoneFTP;
               Dispose(ftpObj, Done);
             {$ENDIF}
           end; { if }
end; { func. RenamefileBaseFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure RenameFile(HiLight: Longint);
var TempStr  : String;
    UpdateHdr: Boolean;
    SaveScrn : Pointer;
    FnameStr : String;
    EditPath : String;
begin
  AreaObj.Read(Pred(HiLight));
  UpdateHDR := True;

  TempStr := AreaObj.GetFileName;
  if Length(TempStr) > 25 then
    FNameStr := Copy(TempStr, 1, 22) + '...'
     else FNameStr := TempStr;

  if (Mgr_EditBox(TempStr, 'Rename "'+FNameStr+'" to: ', ' Rename file ', 35, 01, 250, '', false, false, false) = #27)
       OR (SUpCase(Trim(AreaObj.GetFileName)) = SUpCase(Trim(TempStr))) then UpdateHDR := False;

  If UpdateHDR then
    begin
      if NOT IsFtpUrl(EdittingArea.FilePath) then
        EditPath := EdittingArea.FilePath
         else EditPath := EdittingEle.FtpPath;

      if NOT RenameFileBaseFile(EditPath + AreaObj.GetFileName,
                                EditPath + TempStr) then
        begin
          Write(^G);                                                       { Beep }

          Case Mgr_QuestionBox('DOS could not rename the file. Update the database anyway (y,N,esc)? °', ' Rename file ', '',
                               ['Y', 'N', #27], 'N') of
            #27 : UpdateHDR := False;
            'N' : UpdateHDR := False;
          end; { case }

        end; { if }
    end; { if }

  if UpdateHDR then
    begin
       AreaObj.CurHdr^.LfnPtr := AreaObj.AddLfnPtr(TempStr,
                                                   EdittingArea.FilePath + TempStr,
                                                   AreaObj.CurHdr^.Name);

      OneMomentBox(SaveScrn, 'One moment ..', False);
        AreaObj.WriteBoth(Pred(HiLight), AreaObj.CurHdr^);
      OneMomentBox(SaveScrn, 'One moment ..', True);
    end; { if }

end; { proc. RenameFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CopyAnyPath(HiLight: Longint; var StartRange, EndRange: Longint);
var SaveScrn   : Pointer;
    TargetPath : String;
begin
  TargetPath := '';

  REPEAT
    if Mgr_EditBox(TargetPath, '', ' Target path ', 70, 0, 00, '', false, false, false)=#27 then EXIT;
    if Targetpath = '' then EXIT;
    TargetPath := ForceBack(Targetpath);

    if NOT FileExist(Targetpath) then
      begin
        Write(^G);
        Mgr_MessageBox('Target path does not exist!', 0);
        Targetpath := '';
      end; { if }

  UNTIL (TargetPath <> '');

  GlobalPath := TargetPath;

  OneMomentBox(SaveScrn, 'Copying...', false);
  GlobalDo('COPYPATH',
           StartRange,
           EndRange,
           HiLight,
           false);
  OneMomentBox(SaveScrn, 'Copying...', false);

  Global_Startrange := -1;
  Global_EndRange   := -1;
  Global_CurHilight := -1;
end; { proc. CopyAnyPath }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SortEntries(var HiLight, StartRange, EndRange: Longint);
var SaveScrn: Pointer;
    NameChar: Char;
    RevChar : Char;
begin
  if (StartRange=-1) OR (EndRange=-1) then
    begin
      Mgr_MessageBox('Cannot sort a single entry!', 0);
      EXIT;
    end; { if }

  NameChar := Mgr_QuestionBox('Sort by (N)ame or (d)ate? °', ' Sort entries ', '', ['N', 'D', #27], 'N');
  if NameChar=#27 then EXIT;
  RevChar := Mgr_QuestionBox('(F)orward or (r)everse? °', ' Sort entries ', '', ['F', 'R', #27], 'F');
  if RevChar=#27 then EXIT;

  OneMomentBox(SaveScrn, 'One moment ..', False);

  CloseFileBase;
  SortFileBase(EdittingArea.AreaNum, (UpCase(NameChar)='D'), (UpCase(RevChar)='R'), StartRange, EndRange, False);
  OpenFileBase(EdittingArea.AreaNum);

  OneMomentBox(SaveScrn, 'One moment ..', True);

  StartRange := -1;
  EndRange := -1;
end; { proc. SortEntries }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function AreaList_CreateFiles: Boolean;
begin
  AreaList_CreateFiles := true;
end; { proc. AreaList_CreateFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure MoveFileLocation(var HiLight: Longint; var StartRange, EndRange: longint;
                           var TopOfScrn, HiBarPos: Longint);
type HdrNamesRec = Array[0..99] of String[250];

var SaveScrn   : Pointer;
    NrRecs     : Word;
    HdrInfs    : Array[0..99] of FilesHdrRecord;
    HdrNames   : ^HdrNamesRec;
    HdrRecNums : Array[0..99] of SmallWord;
    NrDeleted  : Word;
    TempL      : Longint;
    NrDeletes  : Word;
    Counter    : Byte;
begin
  Global_Startrange := StartRange;
  Global_EndRange   := EndRange;
  Global_CurHilight := Hilight;

  CurMoving := True;
  SaveScreen(SaveScrn);

  {----------------- Start list again with another purpose ------------------}
  DoList(False, 01, 14, 9, 12, 05, 12, 08, 20, 00, 00,                                  { ShowLen }
               {01, 14, 9, 10, 05, 10, 08, 24, 00, 00 }
         01, 02, 80, mnuScrnLen - 5,                                { Window Coordinates }
         '', '',
         {$IFDEF FPC}@{$ENDIF}AreaList_GetInfo,
         {$IFDEF FPC}@{$ENDIF}Copy_Activate,
         {$IFDEF FPC}@{$ENDIF}AreaList_Seek,
         {$IFDEF FPC}@{$ENDIF}Copy_AbortCheck,
         {$IFDEF FPC}@{$ENDIF}Copy_Keypress,
         {$IFDEF FPC}@{$ENDIF}AreaList_GetItems,
         {$IFDEF FPC}@{$ENDIF}AreaList_CreateFiles,
         {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
         True,
         HiBarPos,
         TopOfScrn,
         False, true);
  CurMoving := False;
  RestoreScreen(SaveScrn);

  if (HiLight = CopyPoint) then EXIT;

  {-------------------- Show box that we are moving the files ---------------}
  OneMomentBox(SaveScrn, 'One moment ..', False);

  {---------- Wrap the selection the other way around if it's wrong --------}
  if Global_StartRange > Global_EndRange then
   begin
     TempL := Global_StartRange;
     Global_StartRange := Global_EndRange;
     Global_EndRange := TempL;
     Inc(HiLight, Global_EndRange - Global_StartRange);
   end; { if }

  {------------------ Calculate the number of records to copy --------------}
  NrRecs := Succ(Global_EndRange - Global_StartRange);
  if (Global_EndRange = -1) OR (Global_StartRange = -1) then
    NrRecs := 01;

  if (Global_EndRange = -1) OR (Global_EndRange = -1) then
    begin
      Global_StartRange := HiLight;
      Global_EndRange := HiLight;
    end { if }
     else begin
{            Dec(Global_StartRange);
            Dec(Global_EndRange); }
          end; { if }

  {-------------------------- Start copying the records --------------------}
  New(HdrNames);

  if (CopyPoint <> HiLight) AND (CopyPoint > -1) AND (NOT (InRange(CopyPoint, Global_StartRange, Global_EndRange))) then
    begin
      if NrRecs > 99 then
        begin
          Mgr_MessageBox('Maximum number of records to move in once is 99. Number adjusted!', 0);
          NrRecs := 99;
        end; { for }

      {----------------------- Read in all records --------------------------}
      FillChar(HdrInfs, SizeOf(HdrInfs), #00);
      for Counter := 00 to Pred(NrRecs) do
        begin
          AreaObj.Read(Pred(Global_StartRange) + Counter);

          HdrInfs[Counter] := AreaObj.CurHdr^;
          HdrNames^[Counter] := AreaObj.GetFileName;
          HdrRecNums[Counter] := AreaObj.CurrentRecord;
        end; { for }

      {----------------- Close the filebase and delete those files ----------}
      ClosefileBase;

      for Counter := 00 to Pred(NrRecs) do
        begin
          ElFile_U.DeleteFile(EdittingArea.AreaNum,
                              HdrNames^[Counter],
                              HdrRecNums[Counter] - 1,
                              false,
                              NrDeletes,
                              del_Normal,
                              true,
                              true);
        end; { for }

        {------------------------- Make sure we use correct position ----------}
        if CopyPoint > Pred(HiLight) then
           Dec(Copypoint, NrRecs);

        InsertInHdr(EdittingArea.AreaNum, CopyPoint, NrRecs, HdrInfs);
        OpenFileBase(EdittingArea.AreaNum);
    end; { if }

  OneMomentBox(SaveScrn, 'One moment ..', True);

  Dispose(HdrNames);
  StartRange := -1;
  EndRange   := -1;
  HiLight := Copypoint;
end; { proc. MoveFileLocation }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function FileMgr_Seek(Str: String): LongInt;
begin
  FileArea_F^.Seek(0);

  FileMgr_Seek := -1;

  While NOT FileArea_F^.EOF do
   begin
     FileArea_F^.BlkRead(FilesInf, SizeOf(FilesInf));

     if Str = Copy(FStr(FilesInf.AreaNum), 1, Length(Str)) then
       begin
         FileMgr_Seek := FileArea_F^.FilePos div SizeOf(FilesRecord);
         BREAK;
       end; { Found one }

   end; { While NOT eof }

end; { func. FileMgr_Seek }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FileMGR_GetItems: LongInt;
begin
  FileMGR_GetItems := (FileArea_F^.FileSize div SizeOf(FilesRecord));
end; { func. FileMGR_GetItems }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure HurlFile(var HiLight: Longint;
                   var StartRange,
                       EndRange    : Longint;
                       RealHurl    : Boolean);
var SaveScrn   : Pointer;
    NrRecs     : Word;
    TempL      : Longint;
    MoveArea   : Word;
    FilesInf   : FilesRecord;
    EleFilesInf: EleFilesRecord;
    SaveArea   : FilesRecord;
    SaveEle    : EleFilesRecord;
begin
  Global_Startrange := StartRange;
  Global_EndRange   := EndRange;
  Global_CurHilight := Hilight;

  if ReadBit(EdittingArea.Attrib, 3) then
   if RealHurl then
    begin
      Mgr_MessageBox('Cannot HURL files from a CD-ROM area!', 0);
      EXIT;
    end; { if }

  if AreaObj.IsLocked then
   if RealHurl then
    begin
      Mgr_MessageBox('Cannot HURL a LOCKED file!', 0);
      EXIT;
    end; { if }

  {----------------------- List all available file-areas -------------------}
  DoList(True, 16, 41, 6, 0, 0,  00, 00, 00, 00, 00,                           { ShowLen }
         15, 04, 64, mnuScrnLen - 4,                                { Window Coordinates }
         ' File areas ',
         'Select an filearea',
         {$IFDEF FPC}@{$ENDIF}FileArea_GetInfo,
         {$IFDEF FPC}@{$ENDIF}Copy_Activate,
         {$IFDEF FPC}@{$ENDIF}FileMgr_Seek,
         {$IFDEF FPC}@{$ENDIF}Copy_AbortCheck,
         {$IFDEF FPC}@{$ENDIF}Copy_Keypress,
         {$IFDEF FPC}@{$ENDIF}FileMGR_GetItems,
         {$IFDEF FPC}@{$ENDIF}CreateNothing,
         {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
         False,
         0, 0,
         True, false);

  {------------------ Make sure selected area is valid ---------------------}
  if CopyPoint <> -1 then
   begin
     GetFilesRecord(FilesInf, CopyPoint, false);
     GetEleFilesRecord(EleFilesInf, CopyPoint, false);
   end
     else EXIT;

  MoveArea := FilesInf.AreaNum;

  if MoveArea=EdittingArea.AreaNum then
    begin
      Mgr_MessageBox('Unable to MOVE/COPY to the same area!', 0);
      EXIT;
    end; { if }

  if ReadBit(FilesInf.Attrib, 3) then
    begin
      Mgr_MessageBox('Cannot MOVE/COPY files to a CD-ROM area!', 0);
      EXIT;
    end; { if }

  if IsFtpUrl(FilesInf.FilePath) then
    begin
      Mgr_MessageBox('Cannot MOVE/COPY files to a FTP area!', 0);
      EXIT;
    end; { if }

  if NOT FileExist(FilesInf.FilePath) then
    begin
      Mgr_MessageBox('Target filepath does not exist!', 0);
      EXIT;
    end; { if }

  {------------------- Choose fileposition to insert file at ----------------}
  SaveArea := EdittingArea;
  SaveEle := EdittingEle;
  EdittingArea := FilesInf;
  EdittingEle := EleFilesInf;

  CurMoving := True;
  SaveScreen(SaveScrn);

  CloseFileBase;
  OpenFileBase(FilesInf.AreaNum);

  DoList(False, 01, 14, 9, 12, 05, 12, 08, 20, 00, 00,                                  { ShowLen }
               {01, 14, 9, 10, 05, 10, 08, 24, 00, 00 }
         01, 02, 80, mnuScrnLen - 5,                                { Window Coordinates }
         '', '',
         {$IFDEF FPC}@{$ENDIF}AreaList_GetInfo,
         {$IFDEF FPC}@{$ENDIF}Copy_Activate,
         {$IFDEF FPC}@{$ENDIF}AreaList_Seek,
         {$IFDEF FPC}@{$ENDIF}Copy_AbortCheck,
         {$IFDEF FPC}@{$ENDIF}Copy_Keypress,
         {$IFDEF FPC}@{$ENDIF}AreaList_GetItems,
         {$IFDEF FPC}@{$ENDIF}AreaList_CreateFiles,
         {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
         True,
         00, 00,
         False, false);
  CurMoving := False;
  RestoreScreen(SaveScrn);

  {---------------------------- Reopen the filebase ------------------------}
  EdittingArea := SaveArea;
  EdittingEle := SaveEle;
  CloseFileBase;
{   OpenFileBase(EdittingArea.AreaNum);}

  {---------- Wrap the selection the other way around if it's wrong --------}
  if Global_StartRange > Global_EndRange then
   begin
     TempL := Global_StartRange;
     Global_StartRange := Global_EndRange;
     Global_EndRange := TempL;
     Inc(HiLight, Global_EndRange - Global_StartRange);
   end; { if }

  {------------------ Calculate the number of records to copy --------------}
  NrRecs := Succ(Global_EndRange - Global_StartRange);
  if (Global_EndRange = -1) OR (Global_StartRange = -1) then
    NrRecs := 01;

  Dec(HiLight);

  if (Global_EndRange = -1) OR (Global_EndRange = -1) then
    begin
      Global_StartRange := HiLight;
      Global_EndRange := HiLight;
    end { if }
     else begin
            Dec(Global_StartRange);
            Dec(Global_EndRange);
          end; { if }

  {------------------ Make sure we don't copy more than 99 ------------------}
  if (CopyPoint > -1) then
    begin
      if NrRecs > 199 then
        begin
          Mgr_MessageBox('Maximum number of records to move in once is 199. Number adjusted!', 0);
          NrRecs := 199;
        end; { for }

      CopyMoveFile(MoveArea, EdittingArea.AreaNum,
                   Global_StartRange, Global_EndRange,
                   RealHurl, True, CopyPoint, true);
    end; { if }

  {---------- Do we 'stay' in the original area or return to the new? -------}
  if (CopyPoint > -1) then
   if Mgr_QuestionBox('Stay in original area (Y/n)? °',
                      ' CONFIRM ', '', ['Y', 'N'], 'Y') = 'N' then
     begin
       EdittingArea := FilesInf;
       EdittingEle := EleFilesInf;

       CloseFileBase;
       HiLight := CopyPoint;
     end { if }
       else HiLight := Global_CurHiLight;

  OpenFileBase(EdittingArea.AreaNum);

  StartRange := -1;
  EndRange   := -1;
end; { proc. HurlFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure KeyWordSearch(var HiLight: Longint; RealKeyWord: Boolean);

function KeyWordSearch_Do(    AreaNum  : Word;
                              KeyWord  : String;
                          var HiLight  : longint;
                          var FoundName: String;
                              UseHigh  : Boolean): Boolean;
var SearchObj   : MgrFileObj;
    CurrentItem : Longint;
    TotalItems  : Longint;
begin
  KeyWordSearch_DO := False;
  FoundName := '';

  SearchObj.Init(AreaNum, true);

  If UseHigh then CurrentItem := HiLight
    else CurrentItem := 00;
  TotalItems := SearchObj.TotalRecords;

  While (CurrentItem < TotalItems) do
    begin
      SearchObj.Read(CurrentItem);
      Inc(CurrentItem);

      if RealKeyword then
        begin
          SearchObj.ReadDescription(SearchObj.TxtF, SearchObj.GetLongDescPtr);

          if ShowFile(fsKeyWord,
                      SearchObj.CurIdx^,
                      SearchObj.CurTxt^,
                      KeyWord,
                      false,
                      SearchObj.GetFileName) then
             begin
               HiLight := CurrentItem;
               FoundName := SearchObj.GetFileName;
               KeyWordSearch_Do := True;
               BREAK;
             end; { if }
        end; { if RealKeyWord }

      if NOT RealKeyword then
        begin
          if ShowFile(fsFileName,
                      SearchObj.CurIdx^,
                      SearchObj.CurTxt^,
                      KeyWord,
                      false,
                      SearchObj.GetFileName) then
             begin
               HiLight := CurrentItem;
               FoundName := SearchObj.GetFileName;
               KeyWordSearch_Do := True;
               BREAK;
             end; { if }
        end; { if }
    end; { while }

  SearchObj.Done;
end; { proc. KeyWordSearch_Do }


var ToSearch     : String;
    SaveScrn     : Pointer;
    SaveHi       : Longint;
    AreaInf      : FilesRecord;
    EleInf       : EleFilesRecord;
    TempScrn     : Pointer;
    TempCH       : Char;
    FoundName    : String;
    AbortSearch  : Boolean;

    Keyinput     : Boolean;
    DoWholeSearch: Boolean;
begin
  SaveHi := HiLight;
  KeyInput := true;
  TempCH := #00;

  SaveScreen(SaveScrn);

  if (AreaToStartSearch > 0) OR (RecToStartSearch > 0) then
    begin
      if Mgr_QuestionBox('Continue previous search (Y,n)? °', ' Find file (keyword search) ', '',
                        ['Y', 'N'],'Y')='N' then
        begin
          AreaToStartSearch := 0;
          RecToStartSearch := 0;
        end { if }
          else begin
                 KeyInput := false;
                 HiLight := RecToStartSearch + 1;
               end; { if }
    end; { if }

  if RealKeyword then
   if KeyInput then
    if Mgr_EditBox(KeyWordToSearch, 'KeyWord: ', ' Find file (keyword search) ', 35, 25, 00,
                  'Enter the keyword or partial keyword to search on',
                  false, false, false)=#27 then
                   begin
                     RestoreScreen(SaveScrn);
                     EXIT;
                   end; { if }

  if NOT RealKeyword then
   if KeyInput then
    if Mgr_EditBox(FileNameToSearch, 'FileMask: ', ' Find file ', 60, 5, 60,

 { MaxLen, AddLen,EditLen }

                   'Enter the name of the file to find (wildcards are valid)',
                   false, false,false )=#27 then
                    begin
                      RestoreScreen(SaveScrn);
                      EXIT;
                    end; { if }

  if RealKeyWord then ToSearch := KeyWordToSearch
    else ToSearch := FileNameToSearch;

  if KeyWordSearch_Do(EdittingArea.AreaNum, ToSearch, HiLight, FoundName, True) then
   begin
     RecToStartSearch := HiLight;

     RestoreScreen(SaveScrn);
     EXIT;
   end; { if }

  if KeyInput then
    begin
      TempCH := Mgr_QuestionBox('Not found. Search all areas (y,c,N,esc)? °', ' Find file (keyword search) ', '',
                        ['Y', 'N', 'C', #27], 'N');

      if (TempCH = 'Y') then DoWholeSearch := true;
      if (TempCH = 'C') then
        begin
          DoWholeSearch := true;
          AreaToStartSearch := Pred(Ra250Area(EdittingArea.Areanum));
        end; { if }

    end
      else DoWholesearch := true;

  if DoWholeSearch then
    begin
      HiLight := 01;

      New(Area_F, Init);
      Area_F^.Assign(FilesFileName);
      Area_F^.FileMode := ReadMode + DenyNone;
      if NOT Area_F^.Open(1) then
        begin
          Mgr_Messagebox('Unable to find/open '+JustName(FilesFileName), 0);
          EXIT;
        end; { if }

      Area_F^.Seek(Succ(AreaToStartSearch) * Sizeof(FilesRecord));

      While NOT Area_F^.EOF do
        begin
           Hilight := 00;

           Area_F^.BlkRead(Areainf, Sizeof(FilesRecord));
           if Area_F^.IoResult > 00 then BREAK;

           if AreaInf.Name <> '' then
            begin
               AreaInf.FilePath := ForceBack(AreaInf.FilePath);
               OneMomentBox(TempScrn, AreaInf.Name, False);

               repeat
                 AbortSearch := NOT KeyWordSearch_Do(AreaInf.AreaNum, ToSearch, HiLight, FoundName, true);

                 if NOT AbortSearch then
                  begin
                    TempCH := Mgr_QuestionBox('(G)o to new area, (C)ontinue search, (Esc) to quit: °',
                    ' Find file (keyword search) ', 'Found '+SUpCase(FoundName)+' in area '+
                    FStr(AreaInf.AreaNum) + ' - '+AreaInf.Name, ['G', 'C', #27], #00);

                    if TempCH in ['G', #27] then
                     begin
                       AbortSearch := true;
                       if TempCH <> #27 then
                         begin
                           GetEleFilesRecord(EleInf, AreaInf.AreaNum, true);

                           EdittingArea := AreaInf;
                           EdittingEle := EleInf;

                           ClosefileBase;
                           OpenFileBase(EdittingArea.AreaNum);
                         end; { if }

                       if TempCH = #27 then HiLight := SaveHI;
                       AreaToStartSearch := (Area_F^.FilePos div SizeOf(FilesRecord)) - 01;
                       RecToStartSearch := HiLight;

                       Dispose(Area_F, Done);
                       RestoreScreen(SaveScrn);
                       OneMomentBox(TempScrn, AreaInf.Name, True);
                       EXIT;
                     end; { if }
                  end; { if KeyWordSearch }
               until (AbortSearch);

               if (AbortSearch) AND (TempCH in ['G', #27]) then
                  EXIT;

               OneMomentBox(TempScrn, AreaInf.Name, True);
            end; { if }
        end; { while }

      AreaToStartSearch := 0;
      RecToStartSearch := 0;
      HiLight := 0;

      Dispose(Area_F, Done);

      Mgr_MessageBox('Scanned all areas; Press any key..', 0);
    end { if }
      else begin
             AreaToStartSearch := 0;
             RecToStartSearch := 0;
             HiLight := 0;
           end; { not Whole search }

  if TempCH <> 'G' then HiLight := SaveHi;
  RestoreScreen(SaveScrn);
end; { proc. KeyWordSearch }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Insertfiles(CurHiLight: Longint;
                      Comment   : Boolean;
                      CopyThem  : Boolean);
var InsertName  : String;
    UploaderName: String;
    SaveScrn    : Pointer;
    HdrInfs     : Array[0..1] of FilesHdrRecord;
    AddPath     : String;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'InsertFiles (CurHiLight='+FStr(CurHiLight)+') (begin)');
    DebugObj.DebugLog(logFileSys, 'InsertFiles (InsertPath='+EdittingArea.FilePath+')');
    DebugObj.DebugLog(logFileSys, 'InsertFiles (InsertName='+EdittingArea.Name+')');
  {$ENDIF}

  if Comment then
    begin
      CloseFileBase;

      FillChar(HdrInfs, SizeOf(HdrInfs), #00);
      HdrInfs[0].LongDescPtr := -1;
      HdrInfs[0].Lfnptr := -1;

      InsertInHdr(EdittingArea.AreaNum, CurHiLight, 1, HdrInfs);

      OpenFileBase(EdittingArea.AreaNum);

      EXIT;
    end; { if }

  AddPath := EdittingArea.FilePath;
  if IsFtpUrl(AddPath) then
    AddPath := EdittingEle.FtpPath;

  if IsFtpUrl(AddPath) then
   if LastChar(AddPath) <> '/' then
     AddPath := AddPath + '/';

  {$IFNDEF ELEUNIX}
    InsertName := AddPath + '*.*';
  {$ELSE}
    InsertName := AddPath + '*';
  {$ENDIF}
  UploaderName := GlobalCfg^.RaConfig^.SysOp;

  SaveScreen(SaveScrn);

  if CopyThem then
   begin
     if Mgr_EditBox(InsertName, '', ' Filename (wildcards valid) ', 70, 01, 250,
                    'The "@" prefix is not supported by EleMGR nor by EleBBS.',
                    false, false, false)=#27 then
                     begin
                       RestoreScreen(SaveScrn);
                       EXIT;
                     end; { if }

     if Mgr_EditBox(UploaderName, '', ' Uploader ', 35, 35, 00,
                    'Enter the name of the uploader.',
                    false, false, false)=#27 then
                     begin
                       RestoreScreen(SaveScrn);
                       EXIT;
                     end; { if }
   end; { if CopyThem }

  if NOT CopyThem then
    begin
      {$IFNDEF ELEUNIX}
        InsertName := '*.*';
      {$ELSE}
        InsertName := '*';
      {$ENDIF}

      if Mgr_EditBox(InsertName, 'Filemask: ', ' Adopt orphans ', 40, 14, 12,
                     '', false, false, false)=#27 then
                     begin
                       RestoreScreen(SaveScrn);
                       EXIT;
                     end; { if }

      InsertName := AddPath + InsertName;
    end; { if }

  CloseFileBase;

  Do_AddFiles(EdittingArea.AreaNum, InsertName, UploaderName, CurHiLight, CopyThem);

  OpenFileBase(EdittingArea.AreaNum);

  RestoreScreen(SaveScrn);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'InsertFiles (CurHiLight='+FStr(CurHiLight)+') ( end )');
  {$ENDIF}
end; { proc. InsertFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Import_Comment_File(var HiLight: Longint);
var SaveScrn    : Pointer;
    CommentFile : String;
    TxtInf      : TxtRec;
    HdrInfs     : Array[0..100] of FilesHdrRecord;
    CountLines  : Byte;
    TempStr     : String;
    Text_F      : Text;
begin
  SaveScreen(SaveScrn);

  CommentFile := '';
  if Mgr_EditBox(CommentFile, '', ' Import textfile ', 70, 0, 00, '', false, false, false)=#27 then
    begin
      RestoreScreen(SaveScrn);
      EXIT;
    end; { EditBox }

  if NOT FileExist(CommentFile) then EXIT;

  CountLines := 00;
  FillChar(HdrInfs, SizeOf(HdrInfs), #00);

  {------------------------ Open up the comment file -----------------------}
  Assign(Text_F, CommentFile);
  {$i-} System.Reset(Text_F); {$I+}
  If IOResult>00 then
    begin
      RestoreScreen(SaveScrn);
      EXIT;
    end; { if }

  {-------------------- Read up to a maximum of 100 lines ------------------}
  While NOT Eof(Text_F) do
    begin
      Inc(CountLines);
      {$i-} ReadLn(Text_F, TempStr); {$I+}
      if IOResult>00 then BREAK;

      FillChar(HdrInfs[CountLines-01], SizeOf(FilesHdrRecord), #00);
      FillChar(TxtInf, SizeOf(TxtRec), #00);

      Move(TempStr[1], TxtInf, Length(TempStr));
      HdrInfs[CountLines-01].LfnPtr := 01;
      HdrInfs[CountLines-01].LongDescPtr := AreaObj.AddDescription(TxtInf);

      if CountLines>99 then BREAK;
    end; { While }

  {$i-} Close(Text_F); {$I+}
  If IOResult>00 then ;

  {---------------- Close filebase, add comment, open fbase ----------------}
  CloseFileBase;

  InsertInHdr(EdittingArea.AreaNum, HiLight, CountLines, HdrInfs);

  OpenFileBase(EdittingArea.AreaNum);

  RestoreScreen(SaveScrn);
end; { proc. Import_Comment_File }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ExportFiles;
var SaveScrn    : Pointer;
    ExportFile  : String;
    TempCH      : Char;
    ExportNr    : Word;
begin
  SaveScreen(SaveScrn);

  ExportFile := EdittingArea.FilePath + 'files.bbs';
  if Mgr_EditBox(ExportFile, '', ' Generate file list ', 70, 0, 00, '', false, false, false)=#27 then
    begin
      RestoreScreen(SaveScrn);
      EXIT;
    end; { EditBox }

  if IsFtpUrl(ExportFile) then
    begin
      Mgr_MessageBox('Cannot export to FTP paths', 3000);
      RestoreScreen(SaveScrn);
      EXIT;
    end; { if }

  if FileExist(ExportFile) then
    begin
      TempCH := Mgr_QuestionBox('File already exists! (A)ppend or (O)verwrite (a,O,esc)? °', ' Generate file list ',
                                '', ['A', 'O', #27], 'O');
      if TempCH=#27 then
        begin
          RestoreScreen(SaveScrn);
          EXIT;
        end; { if }
    end; { if }

  ExportFileBase(EdittingArea.AreaNum, ExportFile, ExportNr, TempCH='A', false);

  RestoreScreen(SaveScrn);
end; { proc. ExportFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure PasteToComment(HiLight: Longint);
var Text_F   : Text;
    HdrInfs  : Array[1..100] of FilesHdrRecord;
    Lines    : Word;
    TempStr  : String;
    TxtInf   : TxtRec;
begin
  if GetFileSize('ramgrbuf.$00', 01) = 00 then
    begin
      Mgr_MessageBox('Edit buffer is empty!', 0);
      EXIT;
    end; { if }

  if MGR_QuestionBox('Insert the edit buffer at the hilight bar entry (Y,n,esc)? °',
                     ' Paste buffer ',
                     '',
                     ['Y', 'N', #27],
                     'Y') <> 'Y' then EXIT;



  Assign(Text_F, 'ramgrbuf.$00');
  {$i-} System.Reset(Text_F); {$I+}
  if IOResult>00 then EXIT;

  Lines := 00;

  While (NOT Eof(Text_F)) AND (Lines < 100) do
    begin
      Inc(Lines);

      ReadLn(Text_F, TempStr);

      FillChar(HdrInfs[Lines], SizeOf(FilesHdrRecord), #00);
      FillChar(TxtInf, SizeOf(TxtRec), #00);
      Move(TempStr[1], TxtInf, Length(TempStr));

      HdrInfs[Lines].LongDescPtr := AreaObj.AddDescription(TxtInf);
      HdrInfs[Lines].LfnPtr := -1;
    end; { while }

  {$i-} Close(Text_F); {$I+}
  if IOResult>00 then ;

  CloseFileBase;

  InsertInHdr(EdittingArea.AreaNum, HiLight, Lines, Hdrinfs);
  OpenFileBase(EdittingArea.AreaNum);
end; { proc. PasteToComment }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditBuffer;
var Name      : String;
    SaveMnuBox: Byte;
    Menu      : PullRecord;
begin
  name := 'ramgrbuf.$00';

  WindowColor := MakeAttr(LightBlue, blue);
  NormColor := MakeAttr(Lightgray, Blue);

  Text_Editor('ramgrbuf.$00', Menu, false);
end; { proc. EditBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure PasteToDescription(HiLight: Longint);
var Text_F  : Text;
    TempStr : String;
    HdrInf  : FilesHdrRecord;
    TxtInf  : TxtRec;
    TempCH  : Char;
    NrLines : Byte;
begin
  if GetFileSize('ramgrbuf.$00', 01) = 00 then
    begin
      Mgr_MessageBox('Edit buffer is empty!', 0);
      EXIT;
    end; { if }

  if MGR_QuestionBox('Paste the buffer into the description of the hilighted file (Y,n,esc)? °',
                     ' Paste buffer to description ', '', ['Y', 'N', #27], 'Y') <> 'Y' then
    EXIT;


  AreaObj.Read(Pred(HiLight));
  if AreaObj.GetLongDescPtr <> -1 then
    begin
      TempCH :=  MGR_QuestionBox('File already has a description. (A)ppend or (O)verwrite (a,O,esc)? °',
                                 ' Paste buffer to description ', '', ['A', 'O', #27], 'O');
      if TempCH=#27 then EXIT;
    end; { if }


  FillChar(HdrInf, SizeOf(FilesHdrRecord), #00);
  FillChar(TxtInf, SizeOf(TxtRec), #00);

  Assign(Text_F, 'ramgrbuf.$00');
  {$i-} System.Reset(Text_F); {$I+}
  if IOResult>00 then EXIT;

  NrLines := 00;
  if TempCH = 'A' then
    begin
      AreaObj.ReadDescription(AreaObj.TxtF, AreaObj.GetLongDescPtr);

      While (NOT AreaObj.EndOfDesc) AND (NrLines < 100) do
        begin
           TempStr := AreaObj.GetDescLine(80);
           TempStr := TempStr + #13#10;

           Move(TempStr[1], TxtInf[StrLen(TxtInf)], Length(TempStr));
           Inc(NrLines);
        end; { while }
    end; { if Append }

  While (NOT Eof(Text_F)) AND (NrLines<100) do
    begin
      ReadLn(Text_F, TempStr);
      Inc(NrLines);

      TempStr := TempStr + #13#10;

      Move(TempStr[1], TxtInf[StrLen(TxtInf)], Length(TempStr));
    end; { while }

  {$i-} Close(Text_F); {$I+}
  if IOResult>00 then ;

  AreaObj.CurHdr^.LongDescPtr := AreaObj.AddDescription(TxtInf);
  AreaObj.WriteBoth(Pred(AreaObj.CurrentRecord), AreaObj.CurHdr^);
end; { proc. PasteToDescription }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure BufferMenu(var HiLight: Longint);

function OpenBuffer(var Text_F: Text; TitleName: String; AskAppend: Boolean): Boolean;
var TempCH: Char;
begin
  OpenBuffer := false;

  if GetFileSize('ramgrbuf.$00', 1) = 00 then
      EraseFile('ramgrbuf.$00');

  Assign(Text_F, 'ramgrbuf.$00');

  if AskAppend then
   if FileExist('ramgrbuf.$00') then
     begin
       TempCH := UpCase(Mgr_QuestionBox('Buffer is not empty! (A)ppend or (O)verwrite (a,O,esc)? °',
                                        TitleName, '', ['A', 'O', #27], 'O'));

       {$i-}
       Case TempCH of
         #27 : EXIT;
         'O' : System.Rewrite(Text_F);
         'A' : Append(Text_F);
       end; { case }
       {$i+}
       if IOResult>00 then EXIT;
     end { if FileExist }
      else begin
             {$i-} System.ReWrite(Text_F); {$i+}
             if IOresult>00 then ;
           end; { if }

  if NOT AskAppend then
    begin
      {$i-} Append(Text_F); {$I+}
      if IOResult>00 then System.ReWrite(Text_F);
    end; { if }

  OpenBuffer := true;
end; { func. OpenBuffer }

procedure CopyToBuffer(var HiLight: Longint);
var Text_F     : Text;
    HdrInf     : FilesHdrRecord;
    Counter    : Byte;
    NrLines    : Byte;
begin
  if NOT OpenBuffer(Text_F, ' Copy to buffer ', True) then EXIT;

  AreaObj.Read(Pred(HiLight));
  AreaObj.ReadDescription(AreaObj.TxtF, AreaObj.GetLongDescPtr);

  While NOT AreaObj.EndOfDesc do
    begin
      {$i-}
        WriteLn(Text_F, AreaObj.GetDescLine(80));
      {$i+}
      if IOResult>00 then BREAK;
    end; { for }

  {$I-} Close(Text_F); {$I+}
  if IOResult>00 then ;
end; { proc. CopyToBuffer }


procedure ImportFromFile;
var Temp_F     : pFileObj;
    Text_F     : Text;
    ImportName : String;
    NumRead    : Word;
    TempStr    : String[127];
begin
  ImportName := '';
  if Mgr_EditBox(ImportName, '', ' Import file to buffer ', 70, 00, 00, '', false, false, false) = #27 then EXIT;

  if NOT OpenBuffer(Text_F, ' Import file to buffer ', True) then EXIT;

  New(Temp_F, Init);
  Temp_F^.Assign(ImportName);
  if NOT Temp_F^.Open(1) then
      begin
        Mgr_MessageBox('Could not open file for read!', 0);
        EXIT;
      end; { box }

  While NOT Temp_F^.EOF do
    begin
       NumRead := Temp_F^.BlkRead(TempStr[1], SizeOf(TempStr) - 01);
       TempStr[0] := Chr(NumRead);
       Write(Text_F, TempStr);
    end; { while }

 Dispose(Temp_F, Done);

 {$i-}
   Close(Text_F);
 {$i+}
 if IOResult>00 then ;
end; { proc. ImportFromFile }

procedure ExportToFile;
var Temp_F     : pFileObj;
    Text_F     : Text;
    ExportName : String;
    NumRead    : Word;
    TempStr    : String[127];
begin
  if GetFileSize('ramgrbuf.$00', 01) = 00 then
    begin
      Mgr_MessageBox('Edit buffer is empty!', 0);
      EXIT;
    end; { if }

  ExportName := '';
  if Mgr_EditBox(ExportName, '', ' Export buffer to file ', 70, 00, 00, '', false, false, false) = #27 then EXIT;

  if NOT OpenBuffer(Text_F, ' Export buffer to file ', false) then EXIT;
  System.Reset(Text_F);

  New(Temp_F, Init);
  Temp_F^.Assign(ExportName);
  Temp_F^.FileMode := ReadWriteMode + DenyNone;

  if NOT Temp_F^.Open(1) then
    Temp_F^.Create(1)
     else begin
            Case  Mgr_QuestionBox('File already exists! (A)ppend or (O)verwrite (a,O,esc)? °', ' Export buffer to file ', '',
                    ['A', 'O', #27], 'O') of
                   'A' : Temp_F^.Seek(Temp_F^.FileSize);
                   'O' : Temp_F^.Create(1);
                   #27 : begin
                           Dispose(Temp_F, Done);
                           Close(Text_F);
                           EXIT;
                         end; { if }
          end; { case }
        end; { box }

  While NOT Eof(Text_F) do
    begin
       ReadLn(Text_F, TempStr);
       TempStr := Tempstr + #13 + #10;

       Temp_F^.Blkwrite(TempStr[1], Length(TempStr));
    end; { while }

 Dispose(Temp_F, Done);

 {$i-}
   Close(Text_F);
 {$i+}
 if IOResult>00 then ;
end; { proc. ExportToFile }

var Menu     : PullRecord;
    Choice   : Word;
    CH       : Char;
begin
  AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'BufferMenu');
  InitPullMenu(Menu);
  AddPullItem(Menu.PullInf^[01], ' Copy to buffer   ', 101, 'C', '', 1);
  AddPullItem(Menu.PullInf^[02], ' Edit buffer      ', 102, 'E', '', 1);
  AddPullItem(Menu.PullInf^[03], ' Paste to comment ', 103, 'P', '', 1);
  AddPullItem(Menu.PullInf^[04], ' ÀÄto description ', 104, #00, '', 1);
  AddPullItem(Menu.PullInf^[05], ' Import from file ', 105, 'I', '', 1);
  AddPullItem(Menu.PullInf^[06], ' Export to file   ', 106, 'X', '', 1);

  Menu.Items   := 6;
  Menu.Width   := 19;
  Menu.Length  := 6;
  Menu.X       := 01;
  Menu.Y       := 01;
  Menu.HiLight := 01;
  Menu.AddSpace:= False;
  Menu.Title   := ' Buffer ';
  Menu.PosArray[1].XInc := 00;
  Menu.PosArray[1].YDec := 00;

  ShowMenu(Menu, False);

  Choice := DoPullMenu(Menu, CH, False, False);
  RemoveMenu(Menu);
  ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));

  if Choice in [101..106] then                      { Try to fix GET-DIZ }
   if RdLastkey in ['C', 'E', 'P', 'I', 'X'] then
    if Keypressed then
     begin
       CH := ReadKey;

       if CH <> #13 then
          begin
            ExtraWaiting := true;
            ExtraKey := CH;
          end; { if }
     end; { if }

  Case Choice of
    101 : CopyToBuffer(HiLight);
    102 : EditBuffer;
    103 : PasteToComment(HiLight);
    104 : PasteToDescription(HiLight);
    105 : ImportFromFile;
    106 : ExportToFile;
  end; { Case }

 HiLight := -1;
end; { proc. BufferMenu }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure RaMgrExecute(S: String; HdrInf: FilesHdrRecord; var AreaObj: MgrFileObj);
var OldHeapEnd   : Pointer;
    SaveScrn     : Pointer;
    DoSwap       : Word;
    Memory_To_Use: Integer;
    Swapping     : Boolean;
    HdrPath      : String;
    TempStr      : String;
    TempLen      : Longint;
    Bat_F        : Text;
    TempNum      : Longint;
begin
   SaveScreen(SaveScrn);

   TextColor(LightGray);
   TextBackGround(Black);
   Partclear(01, 01, mnuScrnWidth, mnuScrnLen, White, #32);

   if S='' then
    begin
      WriteLn;
      WriteLn('Type "EXIT" to return to EleMGR.');
    end; { if }

   CursorOn;                                               { Do the cursor on }

   Swapping := (Pos('*M', SUpCase(S))>0);
   Replace('*M', '', S);                                    { Replace *M code }

   HdrPath := EdittingArea.FilePath + AreaObj.GetFileName;
   Replace('@', HdrPath, S);

   {$IFDEF WIN32}
     HdrPath := HdrPath + #00;
     TempLen := GetShortPathName(@HdrPath[1], @TempStr[1], SizeOf(TempStr));
     TempStr[0] := Chr(TempLen);

     if Length(TempStr) > 00 then HdrPath := TempStr
       else HdrPath := HdrPath;
   {$ENDIF}

   {$IFDEF OS2}
     { short filename in os/2 == long filename }
   {$ENDIF}

   Replace('*X', HdrPath, S);
   if S <> '' then
     S := '/C '+S;

   {$IFNDEF MSDOS}
     Swapping := false;
   {$ENDIF}

   if NOT Swapping then
    begin
     {$IFDEF MSDOS}
       OldHeapEnd := HeapEnd;                               { Save old HeapEnd }
       HeapEnd := HeapPtr;                      { Set the HeapEnd to Heap-Used }
       SetMemTop(HeapEnd);
     {$ENDIF}

      {$IFNDEF ELEUNIX}
        Dos.Exec(GetEnv('COMSPEC'), S);                         { Do Dos Shell }
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
          Writeln(Bat_F, S);
          Close(Bat_F);
          ChMod(GlobalCfg^.RaConfig^.SysPath + 'x_tmp' + FStr(TempNum) + '.sh', 493);
        {$i+}
        if IOResult>00 then ;

        Dos.Exec(GetEnv('SHELL'), GlobalCfg^.RaConfig^.SysPath + 'x_tmp' + FStr(TempNum) + '.sh');

        Assign(Bat_F, GlobalCfg^.RaConfig^.SysPath + 'x_tmp'+FStr(TempNum)+'.sh');
        {$i+} Erase(Bat_F); {$i+}
        if IoResult > 0 then ;

        SwapVectors;
        FillChar(RealScrnBuf^, SizeOf(ScreenRec), #0); { Make sure we redraw }
      {$ENDIF}

     {$IFDEF MSDOS}
       HeapEnd := OldHeapEnd;                                { Restore heapend }
       SetMemTop(HeapEnd);
     {$ENDIF}
    end; { if }

   if Swapping then
     begin
      {$IFDEF MSDOS}
        Memory_To_use := HIDE_FILE or CHECK_NET or USE_FILE;
        If GlobalCfg^.RaConfig^.UseXMS then
           Memory_To_Use := Memory_To_Use or USE_XMS;
        If GlobalCfg^.RaConfig^.UseEMS then
           Memory_To_Use := Memory_To_Use or USE_EMS;
        DoSwap := $ffff;
        Do_Exec(GetEnv('COMSPEC'), S, Memory_To_Use, DoSwap, false);
        FillChar(RealScrnBuf^, SizeOf(ScreenRec), #0);
      {$ENDIF}
     end; { if Swapping }

   RestoreScreen(SaveScrn);
   CursorOff;                                             { Do the cursor off }
end; { proc. RaMgrExecute }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function NotFtp(DoMsg: Boolean): Boolean;
begin
  if IsFtpUrl(EdittingArea.FilePath) then
    begin
      if DoMsg then
         Mgr_MessageBox('Not available on FTP areas', 0);
      NotFtp := false
    end
      else NotFtp := true;
end; { func. NotFtp }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function AreaList_KeyPress(CH: Char; HiLight: LongInt;
                           var HiBarPos, TopOfScrn: Longint;
                           var StartRange, EndRange: LongInt): LongInt;
var Temp_EndRange: Longint;
    TempL        : Longint;
    OldHiLight   : Longint;
    TotalItems   : longint;
    OldDispMode  : dispModeSet;
begin
  AreaList_Keypress := -1;
  OldHiLight := HiLight;

  Temp_EndRange := EndRange;
  if EndRange = AreaList_GetItems then
    EndRange := AreaList_GetItems - 01;

  AreaObj.Read(Pred(HiLight));

  TotalItems := AreaList_GetItems;
  OldDispMode := Displaymode;

  Case CH of
    { F1 - Help }    #59 : ShowHelpScreen;
    { F2 - Edit Dsc} #60 : if HiLight <> TotalItems then
                             if (NOT AreaObj.IsComment) then EditDescription(HiLight)
                               else EditComment(HiLight, HiBarPos);
    { Delete }       #83 : if HiLight <> TotalItems then DeleteFiles(HiLight, StartRange, EndRange);
    { Insert }       #82 : InsertFiles(HiLight, StrEdit.ShiftState, true);
    { Insert }       #05 : InsertFiles(HiLight, StrEdit.ShiftState, false);
    { ALT - A }      #30 : if NotFtp(true) then InsertFiles(HiLight, StrEdit.ShiftState, false);
    { ALT - U }      #22 : if HiLight <> TotalItems then
                            if (NOT AreaObj.IsComment) then UpdateFiles(HiLight, StartRange, EndRange);
    { ALT - T }      #20 : if HiLight <> TotalItems then
                            if (NOT AreaObj.IsComment) then TouchFiles(HiLight, StartRange, EndRange);
    { ALT - R }      #19 : if HiLight <> TotalItems then
                             if (NOT AreaObj.IsComment) then RenameFile(HiLight);
    { ALT - P }      #25 : if NotFtp(true) then
                            if HiLight <> TotalItems then
                             if (NOT AreaObj.IsComment) then CopyAnyPath(HiLight, StartRange, EndRange);
    { ALT - S }      #31 : if HiLight <> TotalItems then SortEntries(HiLight, StartRange, EndRange);
    { ALT - M }      #50 : if HiLight <> TotalItems then MoveFileLocation(HiLight, StartRange, EndRange, TopOfScrn, HiBarPos);
    { ALT - K }      #37 : KeyWordSearch(HiLight, True);
    { ALT - I }      #23 : Import_Comment_File(HiLight);
    { ALT - H }      #35 : if NotFtp(true) then
                             if HiLight <> TotalItems then HurlFile(HiLight, StartRange, EndRange, True);
    { ALT - G }      #34 : ExportFiles;
    { ALT - F }      #33 : KeyWordSearch(HiLight, False);
    { ALT - D }      #32 : begin
                             if DisplayMode = disp_EleMGR then
                               begin
                                 DisplayMode := disp_Usermode
                               end
                                 else begin
                                        DisplayMode := disp_EleMGR;
                                      end; { if }
                           end; { ALT-D }
    { ALT - C }      #46 : if NotFtp(true) then
                             if HiLight <> TotalItems then HurlFile(HiLight, StartRange, EndRange, False);
    { ALT - V }      #47 : if NotFtp(true) then
                             if HiLight <> TotalItems then
                              if (NOT AreaObj.IsComment) then ImportDizFile(HiLight, StartRange, EndRange);
    { ALT - W }      #17 : if NotFtp(true) then
                             if HiLight <> TotalItems then
                              if (NOT AreaObj.IsComment) then PutDizInArchive(HiLight);
    { ALT - B }      #48 : begin
                             BufferMenu(HiLight);
                           end; { if }
    { ALT - F1 }    #104 : RaMgrExecute(GlobalCfg^.RaConfig^.RaMgrAltFkeys[1], AreaObj.CurHdr^, AreaObj);
    { ALT - F2 }    #105 : RaMgrExecute(GlobalCfg^.RaConfig^.RamgrAltFkeys[2], AreaObj.CurHdr^, AreaObj);
    { ALT - F3 }    #106 : RaMgrExecute(GlobalCfg^.RaConfig^.RamgrAltFkeys[3], AreaObj.CurHdr^, AreaObj);
    { ALT - F4 }    #107 : RaMgrExecute(GlobalCfg^.RaConfig^.RamgrAltFkeys[4], AreaObj.CurHdr^, AreaObj);
    { ALT - F5 }    #108 : RaMgrExecute(GlobalCfg^.RaConfig^.RamgrAltFkeys[5], AreaObj.CurHdr^, AreaObj);
      else begin
             EndRange := Temp_EndRange;
             HiLight := -1;
           end; { else }
  end; { case }


  if OldDispMode <> DisplayMode then
   begin
      PartClear(01, 01, mnuScrnWidth, 01, Yellow, #32);

      Case DisplayMode of
         disp_EleMGR  : WriteAT(01, 01, Yellow, ' ÚÄÄ Name ÄÄ¿    Size    Date    ' +
                                                'DL#  Last DL   Flags   ÚÄÄ Keyword ÄÄ¿');
         disp_UserMode: WriteAT(01, 01, Yellow, 'User display mode.');
      end; { case }
   end; { if }

  if OldHiLight <> HiLight then
    AreaList_Keypress := HiLight
     else AreaList_KeyPress := -1;
end; { func. AreaList_KeyPress }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditFileArea(FilesInf: FilesRecord;
                       EleFilesInf: EleFilesRecord);
var SaveScrn:Pointer;
begin
  if NOT FileExist(FilesInf.Filepath) then
   if ((Pos('-N', ParamString) = 00) AND (Pos('/N', ParamString)=00)) then
    if (NOT IsFtpUrl(FilesInf.FilePath)) then
      begin
        Mgr_MessageBox('Path for this area does not exist!', 0);
        EXIT;
      end; { if }

  SaveScreen(SaveScrn);

  DirectAbort := False;

  EdittingArea := FilesInf;
  EdittingEle := EleFilesInf;
  Partclear(01, 01, mnuScrnWidth, mnuScrnLen, White, #32);
  WriteAT(01, 01, Yellow, ' ÚÄÄ Name ÄÄ¿    Size    Date      DL#  Last DL     Flags   ÚÄÄ Keyword ÄÄ¿');
  PartClear(01, 02, mnuScrnWidth, 02, Blue, mnuDblWide);
  PartClear(01, mnuScrnLen - 01, mnuScrnWidth, mnuScrnLen - 01, Blue, mnuSngWide);

  if IsFtpUrl(FilesInf.Filepath) then
    WriteAT(03, mnuScrnLen - 1, Yellow, ' (FTP) ');

  DoList(False, 01, 14, 9, 12, 05, 12, 08, 20, 00, 00,                                  { ShowLen }
               {01, 14, 9, 10, 05, 10, 08, 24, 00, 00 }
         01, 02, 80, mnuScrnLen - 05,                                { Window Coordinates }
         '', '',
         {$IFDEF FPC}@{$ENDIF}AreaList_GetInfo,
         {$IFDEF FPC}@{$ENDIF}AreaList_Activate,
         {$IFDEF FPC}@{$ENDIF}AreaList_Seek,
         {$IFDEF FPC}@{$ENDIF}General_AbortCheck,
         {$IFDEF FPC}@{$ENDIF}AreaList_Keypress,
         {$IFDEF FPC}@{$ENDIF}AreaList_GetItems,
         {$IFDEF FPC}@{$ENDIF}AreaList_CreateFiles,
         {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
         True,
         00, 00,
         False, false);

  RestoreScreen(SaveScrn);
end; { proc. EditFileArea }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function FileMgr_AbortCheck(CH: Char): Boolean;
begin
  If Ch=#27 then Filemgr_AbortCheck := True
   else FileMgr_AbortCheck := False;

  if DirectAbort then FileMgr_AbortCheck := True;
end; { func. FileMgr_AbortCheck }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FileMgr_Activate(HiLight: LongInt; HibarPos: LongInt): Longint;
var FilesInf   : FilesRecord;
    EleFilesinf: EleFilesRecord;
    FileMgr    : MgrFileObj;
begin
  FileMgr_Activate := -1;

  FileArea_F^.Seek(Pred(Hilight) * SizeOf(FilesRecord));
  FileArea_F^.BlkRead(FilesInf, SizeOf(FilesRecord));
  FilesInf.Filepath := ForceBack(FilesInf.FilePath);

  AreaObj.Init(FilesInf.AreaNum, True);

  if (AreaObj.HdrF^.FileSize DIV SizeOf(FilesHdrRecord)) <>
      (AreaObj.IdxF^.FileSize DIV SizeOf(FilesIdxRecord)) then
        begin
          AreaObj.Done;

          FileMgr.Init(FilesInf.AreaNum, True);
          FileMgr.FileBaseMaintenance(FilesInf.AreaNum,
                                      {$IFDEF FPC}@{$ENDIF}CreateIdxBaseProc,
                                      true);
          FileMgr.Done;

          AreaObj.Init(FilesInf.AreaNum, True);
        end; { if }

  GetEleFilesRecord(EleFilesInf, FilesInf.AreaNum, true);
  EditFileArea(FilesInf, EleFilesInf);

  AreaObj.Done;

  FIleMgr_Activate := -1;
end; { func. FileMgr_Activate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FileMGR_GetInfo(var Info1, Info2, Info3, Info4, Info5, Info6, Info7, Info8, Info9: List_StringType;
                           ItemNr: LongInt; HiLight: LongInt);
begin
 {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileIO, 'FileArea_GetInfo: ItemNr='+FStr(ItemNr));
 {$ENDIF}

  FileArea_F^.Seek(Pred(ItemNr) * SizeOf(FilesRecord));
  FileArea_F^.BlkRead(FilesInf, SizeOf(FilesRecord));

  Info1 := FilesInf.Name;
  Info2 := FStr(FilesInf.AreaNum);
  If Info1='' then Info1 := '[Unused]';
  Info3 := '';
  Info4 := '';
  Info5 := '';
  Info6 := '';
  Info7 := '';
  Info8 := '';
  Info9 := '';

  if ItemNr = HiLight then
    WriteDescrip(mnuMsgColor, FilesInf.FilePath);
end; { proc. FileMGR_GetInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FileMgr_Start;
var SaveScrn: Pointer;
    DoCreate: Boolean;
begin
  {-- Make sure EleFILE is not running -------------------------------------}
  if SemaExist(EleFileSemaphore) then
    begin
      OneMomentBox(SaveScrn, 'EleFILE is running (' + EleFileSemaphore + ' exists)', false);
      GetInput;
      OneMomentBox(SaveScrn, 'EleFILE is running (' + EleFileSemaphore + ' exists)', true);
      EXIT;
    end; { if }

  {-- Make sure EleMGR/file isnt already running ---------------------------}
  if SemaExist(EleFMgrSemaphore) then
    begin
      OneMomentBox(SaveScrn, 'EleMGR (file) is already running (' + EleFMgrSemaphore + ' exists)', false);
      GetInput;
      OneMomentBox(SaveScrn, 'EleMGR (file) is already running (' + EleFMgrSemaphore + ' exists)', true);
      EXIT;
    end; { if }

  {-- now create that semaphore --------------------------------------------}
  CreateSema(EleFMgrSemaphore);

  {-- Check to make sure that HDR\, IDX\ and TXT\ exist --------------------}
  DoCreate := false;
  if NOT FileExist(GlobalCfg^.RaConfig^.FileBasePath) then DoCreate := TRUE;
  if NOT FileExist(GlobalCfg^.RaConfig^.FileBasePath + 'hdr' + BsChar) then DoCreate := TRUE;
  if NOT FileExist(GlobalCfg^.RaConfig^.FileBasePath + 'idx' + BsChar) then DoCreate := TRUE;
  if NOT FileExist(GlobalCfg^.RaConfig^.FileBasePath + 'txt' + BsChar) then DoCreate := TRUE;

  if DoCreate then
    begin
      OneMomentBox(SaveScrn, 'Creating subdirectories', false);

      if NOT FileExist(GlobalCfg^.RaConfig^.FileBasePath) then
        MakeDirectory(GlobalCfg^.RaConfig^.FileBasePath);
      if NOT FileExist(GlobalCfg^.RaConfig^.FileBasePath + 'hdr' + BsChar) then
        MakeDirectory(GlobalCfg^.RaConfig^.FileBasePath + 'hdr' + BsChar);
      if NOT FileExist(GlobalCfg^.RaConfig^.FileBasePath + 'idx' + BsChar) then
        MakeDirectory(GlobalCfg^.RaConfig^.FileBasePath + 'idx' + BsChar);
      if NOT FileExist(GlobalCfg^.RaConfig^.FileBasePath + 'txt' + BsChar) then
        MakeDirectory(GlobalCfg^.RaConfig^.FileBasePath + 'txt' + BsChar);

      OneMomentBox(SaveScrn, 'Creating subdirectories', true);
    end; { if }

  {-- Show the area files --------------------------------------------------}
   if NOT FileExist(FilesFileName) then
    begin
      Mgr_MessageBox('Unable to find '+JustName(FilesFileName), 0);
      EXIT;
    end; { if }

   New(FileArea_F, Init);
   FileArea_F^.Assign(FilesFileName);
   FileArea_F^.FileMode := ReadMode + DenyNone;
   if NOT FileArea_F^.Open(1) then
      begin
        Mgr_MessageBox('Unable to open '+JustName(FilesFileName), 0);
        EXIT;
      end; { if }


   SaveScreen(SaveScrn);
   DoList(True, 16, 41, 6, 0, 0,  00, 00, 00, 00, 00,                                  { ShowLen }
          15, 04, 64, mnuScrnLen-4,                                { Window Coordinates }
          ' File areas ',
          '',
          {$IFDEF FPC}@{$ENDIF}FileMGR_GetInfo,
          {$IFDEF FPC}@{$ENDIF}FileMGR_Activate,
          {$IFDEF FPC}@{$ENDIF}FileMGR_Seek,
          {$IFDEF FPC}@{$ENDIF}FileMGR_AbortCheck,
          {$IFDEF FPC}@{$ENDIF}Copy_Keypress,
          {$IFDEF FPC}@{$ENDIF}FileMGR_GetItems,
          {$IFDEF FPC}@{$ENDIF}CreateNothing,
          {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
          False,
          00, 00,
          True, false);
   RestoreScreen(SaveScrn);

   Dispose(FileArea_F, Done);

  {-- now remove that semaphore --------------------------------------------}
  RemoveSema(EleFMgrSemaphore);
end; { proc. FileMgr_Start }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowMgrProgressBar(Cur, Total: Longint);
var BarLen    : Byte;
    BlockValue: Real;
    Percent   : Real;
    Counter   : Longint;
    Filled    : Longint;
    TmpStr    : String;
    SaveUpd   : Boolean;
begin
  {-- Save the current update setting --------------------------------------}
  SaveUpd := DirectScrnUpdate;


  {-- Create a box ---------------------------------------------------------}
  BoxTitle(12, (mnuScrnLen DIV 2) - 3, 68, (mnuScrnLen DIV 2) + 1,
           MakeAttr(LightBlue, Blue), mnuStyle, true,
           ' Progress ');

  {-- Calc. the percent of this setting ------------------------------------}
  Percent := (Cur / Total);

  {-- Bar length -----------------------------------------------------------}
  BarLen := (68 - 12) - 4;

  {-- Calculate the block value (one block, is so much) --------------------}
  BlockValue := 100.0 / BarLen;

  {-- Filled ---------------------------------------------------------------}
  if BlockValue > 0 then Filled := Trunc((Percent * 100) / BlockValue)
    else Filled := 0;

  {-- Write the numbers of characters --------------------------------------}
  TmpStr := '';
  for Counter := 01 to Filled do
    TmpStr := TmpStr + #219;

  {-- Correct any rounding errors ------------------------------------------}
  if Filled < 0 then Filled := 0;
  if Filled > BarLen then Filled := BarLen;

  {-- Fill up with empty characters ----------------------------------------}
  if Filled < BarLen then
    for Counter := Filled to BarLen do
      TmpStr := TmpStr + mnuBlockChar;

  {-- Actually display the progressbar -------------------------------------}
  WriteAT(14, (mnuScrnLen DIV 2) - 1, MakeAttr(LightBlue, Blue), TmpStr);

  {-- Restore the update setting and update screen -------------------------}
  DirectScrnUpdate := SaveUpd;
  UpdateScreenBuffer(true);
end; { proc. ShowMgrProgressBar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CopyMoveFile(Dest, OrigNum: Word;
                       StartRow, EndRow: Word;
                       Moving, SearchDouble:Boolean;
                       InsertRow: Word;
                       Mgr: Boolean);
var TotalRows  : Word;
    Counter    : Word;
    AreaObj    : ^MgrFileObj;
    FileMgr    : MgrFileObj;
    DoCopy     : Boolean;
    LongName   : String;

    OrigInf    : FilesRecord;
    DestInf    : FilesRecord;

    HdrInfs    : Array[1..2] of FilesHdrRecord;
    NrDeleted  : Word;
    SaveScrn   : Pointer;
    TempName   : String;
    IsDuplicate: Boolean;
    FoundRow   : Longint;
    SelectRow  : Longint;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileSys, 'CopyMovefile (begin)');
  {$ENDIF}

  SaveScreen(SaveScrn);
  AreaObj := nil;
  TotalRows := (EndRow - StartRow) + 01;
  if TotalRows>199 then
      begin
        TotalRows := 199;
      end; { if TotalRows }
  ShowMgrProgressBar(0, TotalRows + 3);                   { +3 for reindexing }

  {-- Get filearea information ------------------------------------------------}
  GetFilesRecord(OrigInf, OrigNum, true);
  GetFilesRecord(DestInf, Dest, true);

  {-- Update the statusbar ----------------------------------------------------}
  ShowMgrProgressBar(1, TotalRows + 3);                   { +3 for reindexing }

  {-- Start the copying/moving ------------------------------------------------}
  for counter := EndRow downto StartRow do
   begin
     IsDuplicate := false;
     FoundRow := -1;

     New(AreaObj, Init(OrigInf.AreaNum, false));
     if AreaObj^.GetError <> 00 then
       begin
         EXIT;
       end;

     AreaObj^.Read(Counter);                     { Read header sequentially }

     FileMgr.Init(DestInf.AreaNum, True);
     FileMgr.FileBaseMaintenance(DestInf.AreaNum,
                                 {$IFDEF FPC}@{$ENDIF}CreateIdxBaseProc,
                                 true);
     FileMgr.Done;

     DoCopy := true;
     if SearchDouble then
      if NOT AreaObj^.IsComment then
       begin
         FoundRow := SearchNameInArea(DestInf.AreaNum, AreaObj^.GetFileName, false);
         DoCopy := FoundRow < 0;
       end; { if }

     SelectRow := InsertRow;

     if Mgr then
      if (NOT DoCopy) OR (FileExist(DestInf.FilePath + AreaObj^.GetFileName)) then
       if NOT AreaObj^.IsComment then
       begin
         if Mgr_QuestionBox(TrimRight(CenterJust('(R)eplace it, or (S)kip? °', 41 + Length(AreaObj^.GetFIleName), false)), '',
                            'File '+AreaObj^.GetFileName+' already exists in the target area!', ['R', 'S'], 'S') = 'S' then
           begin
             DoCopy := false;

             Dispose(AreaObj, Done);
             AreaObj := nil;
           end { if }
            else begin
                   DoCopy := true;
                   if FoundRow <> -1 then IsDuplicate := true;
                 end; { if }
       end; { if }

     if DoCopy then
      begin
        AreaObj^.ReadDescription(AreaObj^.TxtF, AreaObj^.GetLongDescPtr);

        {----------------- Actually copy the file -----------------------------}
        if NOT AreaObj^.IsComment then
          begin
            TempName := ShortToLong(OrigInf.FilePath, AreaObj^.GetFileName);
            if TempName <> AreaObj^.GetFileName then
              begin
                Mgr_MessageBox('Long filename encountered, converting to long name', 0);

                AreaObj^.CurHdr^.LfnPtr := AreaObj^.AddLfnPtr(TempName,
                                                             OrigInf.FilePath + TempName,
                                                             AreaObj^.CurHdr^.Name);
              end; { if }

            if NOT FileCopy(OrigInf.FilePath + TempName,
                            DestInf.FilePath + TempName, True, False) then
               begin
                 Mgr_MessageBox('Failed to copy '+AreaObj^.GetfileName, 0);
               end; { if }
          end; { if }

        {---- Convert AreaObj to correct format -------------------------------}
        FillChar(HdrInfs, SizeOf(HdrInfs), #00);
        HdrInfs[1] := AreaObj^.CurHdr^;

        {---- Insert into filebase and set description ------------------------}
        if NOT IsDuplicate then
          InsertInHdr(Dest, SelectRow, 1, HdrInfs)
           else SelectRow := FoundRow;

        FileMgr.Init(Dest, true);
        FileMgr.Read(SelectRow - 01);
        FileMgr.CurHdr^.LongDescPtr := FileMgr.AddDescription(AreaObj^.CurTxt^);

        if NOT AreaObj^.IsComment then
          FileMgr.CurHdr^.LfnPtr := FileMgr.AddLfnPtr(AreaObj^.GetFileName, OrigInf.FilePath + AreaObj^.GetFileName, TempName);

        if NOT AreaObj^.IsComment then
          if FoundRow <> -1 then
            begin
              FileMgr.CurHdr^.Size     := GetFileSize(DestInf.FilePath + AreaObj^.GetFileName, 1);
              FileMgr.CurHdr^.FileDate := GetPackedFileTime(DestInf.FilePath + AreaObj^.GetFileName);
            end; { if }

        FileMgr.WriteHdr(SelectRow - 01, FileMgr.CurHdr^);
        FileMgr.Done;

        if NOT Mgr then
          if NOT AreaObj^.IsComment then
            RaLog('>', 'Moved '+ AreaObj^.GetFileName + ' to area '+FStr(DestInf.AreaNum));
        LongName := AreaObj^.GetFileName;

        Dispose(AreaObj, Done);
        AreaObj := nil;

        if Moving then
         begin
           NrDeleted := 00;

           ElFile_U.DeleteFile(OrigNum, Longname, Counter,
                               True, NrDeleted, del_Normal, true, true);
         end; { if }
      end; { if }

     ShowMgrProgressBar(StartRow + (EndRow - Counter), TotalRows + 3);       { +3 for reindexing }
   end; { for }

  if AreaObj <> nil then Dispose(AreaObj, Done);
  AreaObj := nil;

  {-- Update the statusbar ----------------------------------------------------}
  ShowMgrProgressBar(StartRow + 2, TotalRows + 3);         { +3 for reindexing }

  {-- Reindex the filedatabase ------------------------------------------------}
  FileMgr.Init(OrigNum, True);
  FileMgr.FileBaseMaintenance(OrigNum,
                              {$IFDEF FPC}@{$ENDIF}CreateIdxBaseProc,
                              true);
  FileMgr.Done;

  {-- Update the statusbar ----------------------------------------------------}
  ShowMgrProgressBar(StartRow + 3, TotalRows + 3);         { +3 for reindexing }

  {-- Reindex the filedatabase ------------------------------------------------}
  FileMgr.Init(Dest, True);
  FileMgr.FileBaseMaintenance(Dest,
                              {$IFDEF FPC}@{$ENDIF}CreateIdxBaseProc,
                              true);
  FileMgr.Done;
  RestoreScreen(SaveScrn);
end; { proc. CopyMoveFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

begin
  displaymode := disp_EleMGR;
end. { unit FileMgr }
