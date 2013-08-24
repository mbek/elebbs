unit Tagunit;
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
** Tagging Routines for EleBBS
**
** Copyright (c) 1998 by Maarten Bekers
**
** Created : 26-Apr-1998
** Last update : 26-Apr-1998
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses TagObj, Global, CfgRec;

type haha2=string[1];

{$IFDEF WITH_FULL}
procedure UserAddToTag(AnyFile, Global, Group: Boolean; AreaNr: Longint);
procedure TagFiles(var ListRec: TagHeaderList);
procedure EditTagList(FromListing, AnyFile, Global, Group: Boolean; AreaNr: Longint);
procedure DeleteFromTag;
function  ShowTaggedFiles(FromListing: Boolean): Boolean;
function  AskToAddTag(FNames: String; AreaNum: Word; IsAnyFile, Global, Group: Boolean): Boolean;
function AskToAdd(FilesInf: FilesRecord; HdrInf: FilesHdrRecord;
                   var ShowedHdr: Boolean; RecNum: Word; FName: String;
                   AddNr: Word; var FNames: String; AreaNum: Word;
                   FileName: String;
                   var FilesAdded: Longint): Boolean;
{$ENDIF}
function  GetLfnname(Name: String; RecordNum, AreaNum: Word): String;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Debug_U, RAL, WordStr, CentrStr,
      LongStr, FileRout, BitWise, Cases, StUtils, GenFile,
      ObjDec,


       {$IFDEF WITH_FULL}
          InOut_U,
          Input_U,
          Control,
          FileSys,
          LineEd,
       {$ENDIF}

       {$IFDEF WIN32}
         Sysutils,
        {$IFNDEF VirtualPascal}
          Use32
        {$ELSE}
          Dos
        {$ENDIF}

        {$IFDEF FPC}
          ,Dos
         {$ENDIF}
       {$ENDIF}

       {$IFDEF MSDOS}
         Dos
       {$ENDIF}

       {$IFDEF ELEUNIX}
        Dos, Use32
       {$ENDIF}

       {$IFDEF Go32V2}
         Dos, Use32
       {$ENDIF}

       {$IFDEF OS2}
         Dos
       {$ENDIF}, Access_U, ElLog_U, StrPath, RA2FDB, GenDos, FileObj;

{$IFDEF WITH_FULL}
procedure UserAddToTag(AnyFile, Global, Group: Boolean; AreaNr: Longint);
var TempStr: String;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileSys, 'UserAddToTag (begin)');
  {$ENDIF}

  WriteLn('`A3:');
  Write(LangObj^.ralGet(ralFile1));

  TempStr := '';
  GetString(TempStr, 60, [#32..#254], false, false, false, False);
  AskToAddTag(TempStr, AreaNr, AnyFile, Global, Group);

  Write('`X1:`E:');

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileSys, 'UserAddToTag ( end )');
  {$ENDIF}
end; { proc. UserAddToTag }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditTagList(FromListing, AnyFile, Global, Group: Boolean; AreaNr: Longint);
var CH       : Char;
    KeysStr  : String;
    AddKey   : Char;
    DeleteKey: Char;
    ClearKey : Char;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'EditTagList (start)');
  {$ENDIF}

  repeat
    if NOT ShowTaggedFiles(FromListing) then
     begin
       { List is empty - if it's from the filelist, abort, else give the }
       { user a chance to add files to the taglist }

       if FromListing then EXIT
         else begin
                UserAddToTag(AnyFile, Global, Group, AreaNr);
                If NOT ShowTaggedFiles(FromListing) then EXIT;
              end; { if }
     end; { When no files are tagged }

    if TaggingObj^.CurrentTagged=00 then EXIT; { After adding nothing? Abort }
    WriteLn;

    If FromListing then Write('`A11:', LangObj^.ralGetStr(ralDc))
      else Write('`A11:', LangObj^.ralGetStr(ralAdc));

    If FromListing then KeysStr := LangObj^.ralGetKeys(ralDc)
      else KeysStr := langObj^.ralGetKeys(ralAdc);

    DeleteKey := KeysStr[1];
    ClearKey := KeysStr[2];

    If NOT FromListing then
      begin
        AddKey := KeysStr[1];
        DeleteKey := KeysStr[2];
        ClearKey := KeysStr[3];
      end; { NOT fromlisting }

    repeat
      CH := UpCase(InputObj^.ReadKey);
    until (CH in [DeleteKey, AddKey, ClearKey, #13]) OR (ProgTerminated);

    if CH = AddKey then
      begin
        if NOT FromListing then
          begin
            WriteLn;
            UserAddToTag(AnyFile, Global, Group, AreaNr);
          end; { NOT FromListing }
      end; { (A)dd }

    if CH = DeleteKey then
      begin
        DeleteFromTag;
      end; { (D)elete }

    if CH = ClearKey then
      begin
        WriteLn('`A12:');
        WriteLn;

        if InputObj^.ralStrYesNoAsk(ralCTag) then
           begin
             TaggingObj^.ClearTagList;
           end; { Clear All }
      end; { (C)lear }

  until (UpCase(CH) in [#13]) OR (Progterminated);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'EditTagList ( end )');
  {$ENDIF}
end; { proc. EditTagList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DeleteFromTag;
var TempStr    : String;
{$IFDEF MSDOS}
    DeleteNames: Array[1..100] of String[12];
{$ELSE}
    DeleteNames: Array[1..100] of String;
{$ENDIF}
    FileName   : String;
    TagInfo    : TagFileRecord;
    DelCount   : Longint;
    Counter    : Longint;
    FNameStr   : String;

    StartNr    : Longint;
    EndNr      : Longint;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileSys, 'DeleteFromTag (begin)');
  {$ENDIF}

  FillChar(DeleteNames, SizeOf(DeleteNames), #00);
  TempStr := '';

  WriteLn;
  WriteLn;
  Write('`A12:', LangObj^.ralGet(ralDelete));
  GetString(TempStr, 80, [#32..#254], False, False, False, False);

  Writeln;
  While Pos(',', TempStr) > 00 do
     Replace(',', #32, TempStr);

  DelCount := 00;
  While Length(Trim(TempStr))>00 do
    begin
      FileName := FirstWord(TempStr, defExtractWord, true);
      if DelCount > 99 then BREAK;

      Inc(DelCount);
      If FVal(FileName)>00 then
        begin
          TaggingObj^.GetTagHeader(Pred(FVal(FileName)), TagInfo);

          FNameStr := GetLfnName(TagInfo.Name, TagInfo.Recordnum, TagInfo.AreaNum);

          DeleteNames[DelCount] := FNameStr;
        end { if }
          else begin
                 if (Pos('-', FileName) > 0) AND (Pos('.', FileName) = 0) then
                    begin
                      StartNr := FVal(FirstWord(FileName, defExtractWord + ['-'], true));
                      EndNr := FVal(FirstWord(FileName, defExtractWord + ['-'], true));

                      if EndNr >= StartNr then
                        begin
                          Dec(DelCount);

                          for Counter := StartNr to EndNr do
                            begin
                              if Counter > 99 then BREAK;
                              if DelCount > 99 then BREAK;

                              Inc(DelCount);
                              TaggingObj^.GetTagHeader(Pred(Counter), TagInfo);
                              FNameStr := GetLfnName(TagInfo.Name, TagInfo.Recordnum, TagInfo.AreaNum);
                              DeleteNames[DelCount] := FNameStr;
                            end; { for }
                        end; { if }
                    end
                      else begin
                             DeleteNames[DelCount] := FileName;
                           end; { if }
               end; { if }
    end; { While }

  for Counter := 01 to DelCount do
   begin
     TaggingObj^.DeleteFileFromTagged(DeleteNames[Counter]);
   end; { for }

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileSys, 'DeleteFromTag ( end )');
  {$ENDIF}
end; { proc. DeleteFromTag }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ShowTaggedFiles(FromListing: Boolean): Boolean;
var Counter     : Byte;
    PrevArea    : Word;
    FilesInf    : FilesRecord;
    TotalBytes  : LongInt;
    FreeBytes   : LongInt;
    TagInfo     : TagFileRecord;
    TempStr     : String;
    FilesTagged,
    FreeFiles   : LongInt;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'ShowTaggedFiles (begin)');
  {$ENDIF}

  OutputObj^.ClearScreen;
  ShowTaggedFiles := false;

  if TaggingObj^.CurrentTagged=00 then
   begin
     if FromListing then
       begin
         WriteLn('`A12:',LangObj^.ralGet(ralNoTagged1));
         InputObj^.PressEnter(False, True);
       end
        else WriteLn('`A12:', LangObj^.ralGet(ralNoTagged2));

      EXIT;
   end; { No files tagged }

  WriteLn('`A3:', LangObj^.ralGet(ralTagList));
  WriteLn;

  FillChar(FilesInf, SizeOf(FilesRecord), #00);
  PrevArea   := 00;
  TotalBytes := 00;
  FreeBytes  := 00;
  Freefiles  := 00;
  FilesTagged:= 00;

  for Counter := 01 to TaggingObj^.CurrentTagged do
    begin
      TaggingObj^.GetTagHeader(Pred(Counter), TagInfo);

      if TagInfo.AreaNum <> PrevArea then
        begin
          GetFilesRecord(FilesInf, TagInfo.AreaNum, True);

          Writeln;
          Writeln('`A14:', FilesInf.Name);
        end; { An other area }

      TempStr := GetLfnName(TagInfo.Name, TagInfo.RecordNum, TagInfo.AreaNum);

      PrevArea := TagInfo.AreaNum;
      Write (' `A7:',LeadingZero(Counter, 2),' `A3:',
                     TempStr, '`X48:',
                     TagInfo.Size DIV 1024, 'k`X24:');

      if (FilesInf.PassWord<>'') then
       if TagInfo.Password='' then
         TagInfo.PassWord := FilesInf.Password;

      if (ReadBit(FilesInf.Attrib, 04)) then
           SetBit(TagInfo.Attrib, 02);

      if (ReadBit(TagInfo.Attrib, 02)) then
        begin
          Write(' ', LangObj^.ralGet(ralFreeDL));
          Inc(FreeBytes, TagInfo.Size);
          Inc(FreeFiles);
        end; { if }

      TaggingObj^.SetTagHeader(Pred(Counter), TagInfo);

      Inc(TotalBytes, TagInfo.Size);
      Inc(FilesTagged);

      if (TagInfo.PassWord<>'') then
        Write(' ', LangObj^.ralGet(ralPassWord2));

      WriteLn;
      if OutputObj^.StopMore then BREAK;
    end; { for counter }

  WriteLn;
  Write('`A11:',FilesTagged, #32, LangObj^.ralGet(ralFiles1), ', ', TotalBytes DIV 1024, 'k');
  If FreeBytes>00 then
    Write(' / ', FreeFiles, #32, LangObj^.ralGet(ralFiles1), ', ', FreeBytes DIV 1024, 'k ',LangObj^.ralGet(ralFreeDL));
  WriteLn;

  ShowTaggedFiles := true;
  OutputObj^.SetStopMore(false);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'ShowTaggedFiles ( end )');
  {$ENDIF}
end; { func. ShowTaggedFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TagFiles(var ListRec: TagHeaderList);
var TagNrStr   : String;
    Part       : String;
    Nr         : Byte;
    HdrInf     : FilesHdrRecord;
    Counter    : Longint;
    AreaObj    : FdbFileObj;
    TempStr    : String;
    EndNR      : Longint;
    StartNr    : Longint;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileSys, 'TagFiles (begin)');
  {$ENDIF}

  TagNrStr := '';

  Write('`X1:`A15:`E:', LangObj^.ralGet(ralTagged), ^K, '&, ', ^K, '''k; ',
        LangObj^.ralGet(ralTagNo));

  GetString(TagNrStr, 80, [#32..#254], false, false, false, false);
  Write('`X1:`A15:`E:');

  While Pos(',', TagNrStr) > 00 do
     Replace(',', #32, TagNrStr);

  While Length(TagNrStr)>00 do
    begin
      Part := FirstWord(TagNrStr, defExtractWord, true);
      Nr := Byte(FVal(Part));

      if (Nr=00) AND (Pos('-', Part) > 0) then
       if Pos('.', Part) = 0 then
         begin
           StartNr := FVal(FirstWord(Part, defExtractWord + ['-'], true));
           EndNr := FVal(FirstWord(Part, defExtractWord + ['-'], true));

           if EndNr >= StartNr then
             begin
               for Counter := StartNr to EndNr do
                 begin
                   if Counter > 99 then BREAK;

                   if ListRec[Counter].Name <> '' then
                     With ListRec[Counter] do
                       begin
                         AreaObj.Init(AreaNum, false);
                         AreaObj.Read(RecordNum - 01);

                         HdrInf := AreaObj.CurHdr^;
                         TempStr := AreaObj.GetFileName;

                         AreaObj.Done;

                         if NOT TaggingObj^.AddToTagFile(HdrInf, AreaNum,
                                         RecordNum, AreaAttrib,
                                         TempStr) then BREAK;
                       end; { with }
                 end; { for }
             end; { if }
         end; { if }

      if (Nr=00) AND ((Pos('-', Part) = 0) OR (Pos('.', Part) > 0)) then
        begin
          for Counter := 01 to 99 do
           if ListRec[Counter].Name <> '' then
            if IsWildCard(Part, SUpCase(Trim(ListRec[Counter].Name))) then
             With ListRec[Counter] do
              begin
                AreaObj.Init(AreaNum, false);
                AreaObj.Read(RecordNum - 01);

                HdrInf := AreaObj.CurHdr^;
                TempStr := AreaObj.GetFileName;

                {$IFDEF WITH_DEBUG}
                  DebugObj.DebugLog(logTransfer, 'TagFiles - CDROM part (begin)');
                  DebugObj.DebugLog(logTransfer, 'TagFiles - FilesInf.CDROM= '+Bool2Str(ReadBit(AreaAttrib, 3)));
                  DebugObj.DebugLog(logTransfer, 'TagFiles - TempStr = '+ TempStr);
                  DebugObj.DebugLog(logTransfer, 'TagFiles - CurHdr  = '+ AreaObj.CurHdr^.Name);
                  DebugObj.DebugLog(logTransfer, 'TagFiles - CDROM part ( end )');
                {$ENDIF}

                AreaObj.Done;
                if NOT TaggingObj^.AddToTagFile(HdrInf, AreaNum,
                                             RecordNum, AreaAttrib,
                                             TempStr) then BREAK;
              end; { adding }
        end; { if }

      if (Nr in [1..99]) then
       if ListRec[Nr].Name <> '' then
        With ListRec[Nr] do
          begin
            AreaObj.Init(AreaNum, false);
            AreaObj.Read(RecordNum - 01);

            HdrInf := AreaObj.CurHdr^;
            TempStr := AreaObj.GetFileName;

            AreaObj.Done;

            {$IFDEF WITH_DEBUG}
              DebugObj.DebugLog(logTransfer, 'TagFiles - CDROM part (begin)');
              DebugObj.DebugLog(logTransfer, 'TagFiles - FilesInf.CDROM= '+Bool2Str(ReadBit(AreaAttrib, 3)));
              DebugObj.DebugLog(logTransfer, 'TagFiles - CDROM part ( end )');
            {$ENDIF}

            if NOT TaggingObj^.AddToTagFile(HdrInf, AreaNum,
                                         RecordNum, AreaAttrib,
                                         TempStr) then BREAK;
          end; { with }
    end; { While }

  Write('`X1:`E:');

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'TagFiles ( end )');
  {$ENDIF}
end; { proc. TagFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function AskToAdd(FilesInf: FilesRecord; HdrInf: FilesHdrRecord;
                   var ShowedHdr: Boolean; RecNum: Word; FName: String;
                   AddNr: Word; var FNames: String; AreaNum: Word;
                   FileName: String;
                   var FilesAdded: Longint): Boolean;
var GoodKeys  : CharSet;
    TempStr   : String;
    CH        : char;
begin
  AskToAdd := true;
  If NOT ShowedHdr then
    begin;
     WriteLn('`A14:`X1:`E:', FilesInf.Name);        { Show filearea-name }
     ShowedHdr := True;
    end;

  Write('`X1:`E:`A15:', FileName,'`A3:`X+2:');

  If NOT ReadBit(HdrInf.Attrib, 3) then
    begin
       If TaggingObj^.IsTaggedFile(FileName, AreaNum) then
        WriteLn('`A3:', LangObj^.ralGet(ralATagged)) else
         begin
           Write(LangObj^.ralGetStr(ralAddTag));
           Str2Set(LangObj^.ralGetKeys(ralAddTag), GoodKeys);
           TempStr := langObj^.ralGetKeys(ralAddTag);

           OutputObj^.ResetLines(01);

           repeat;
             CH := Upcase(InputObj^.ReadKey);

             If CH=#13 then CH := UpCase(TempStr[1]);
           Until (CH in GoodKeys) OR (ProgTerminated);

           WriteLn(CH);

           {$IFDEF WITH_DEBUG}
             DebugObj.DebugLog(logTransfer, 'AskToAdd - CDROM part (begin)');
             DebugObj.DebugLog(logTransfer, 'AskToAdd - FilesInf.Name = '+FilesInf.Name);
             DebugObj.DebugLog(logTransfer, 'AskToAdd - CDROM part ( end )');
           {$ENDIF}

           Case Pos(CH, LangObj^.ralGetKeys(ralAddTag)) of
 { Yes }     01 : If TaggingObj^.AddToTagFile(HdrInf, AreaNum, RecNum, FilesInf.Attrib, FileName)
                    then begin
                          Inc(FilesAdded);

                          If (SUpcase(Trim(HDRInf.Name))=SUpcase(Trim(FName))) then
                            If NOT InputObj^.ralStrYesNoAsk(ralCntSearch) then
                               begin
                                 if ProgTerminated then EXIT;
                                 if WordCount(FNames, DefExtractWord, true) = 01 then FNames := ''
                                      else RemoveWordNr(FNames, AddNr, DefExtractWord, true);

                                 AskToAdd := false;
                               end; { if }
                         end; { RemoveWord }

 { No }      02 : ;
 { Quit }    03 : begin
                   AskToAdd := False;
                   FNames :='';
                  end; { Quit }
           end; { case }
         end; { Not tagged }

    end { if not readbit }
      else WriteLn('`A3:', LangObj^.ralGet(ralNotAvail3)); { Not avail for down }
end; { func. AskToAdd }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function AskToAddTag(FNames: String; AreaNum: Use32.Word; IsAnyFile, Global, Group: Boolean): Boolean;
var FilesAdded: LongInt;


function Add_ScanFileBase(AreaNum: Word; var ShowMoment: Boolean; Global: Boolean;
                          var FNames: String): Boolean;
var FilesInf    : FilesRecord;
    ShowedHdr   : Boolean;
    FName       : String;
    LastChar    : Char;
    Counter     : Byte;
    AreaObj     : FdbFileObj;
    TotalItems  : Longint;
    ItemCounter : Longint;
    DirInfo     : SearchRec;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileSys, 'Add_ScanFileBase (begin)');
  {$ENDIF}

  GetFilesRecord(FilesInf, Areanum, true);
  Add_ScanFileBase := True;
  ShowedHdr := False;

  If NOT CheckFileAreaAccess(FilesInf, True, Group, False, False, 00, LineCfg^.Exitinfo^) then
    begin
      if NOT Global then
       begin
         WriteLn('`A12:', LangObj^.ralGet(ralNoDownAcc));
         EXIT;
       end { no access }
        else EXIT;
    end; { if not }

  AreaObj.Init(FilesInf.AreaNum, false);
  if AreaObj.GetError <> 00 then
      begin
        EXIT
      end
       else begin
              TotalItems := AreaObj.TotalRecords;
              ItemCounter := 00;
            end;

  While (ItemCounter < TotalItems) AND (TotalItems>00) AND (NOT ProgTerminated) do
    begin
      contrObj^.SetTimeOut;
      AreaObj.Read(ItemCounter);      { Read header sequentially }
      Inc(ItemCounter);

      if InputObj^.Key_HasPressed(LineCfg^.DispAbortSet, LastChar) then
        begin
           InputObj^.DorCleanKeys;
           OutputObj^.SetStopMore(True);
           LineCfg^.DispAbortChar := LastChar;
           BREAK;
        end; { if }

      if ShowMoment then
        Write('`X1:`A7:',LangObj^.ralGet(ralOneMoment));
      ShowMoment := FALSE;

      if NOT AreaObj.IsComment then
        begin
          for Counter := 01 to WordCount(FNames, DefExtractWord, true) do
            begin
              FName := ExtractWord(FNames, Counter, defExtractWord, true, false);

              if FName <> '' then
               if IsWildCard(FName, AreaObj.GetFileName) then
                 begin
                   ShowMoment := True;

                   if not AskToAdd(FilesInf, AreaObj.CurHdr^, ShowedHdr,
                                   ItemCounter,
                                   FName, Counter, FNames, FilesInf.AreaNum,
                                   AreaObj.GetFileName, FilesAdded) then
                                    begin
                                      Add_ScanFileBase := false;
                                      BREAK;
                                    end; { if }
                 end; { if IsWildCard }
            end; { for counter }
        end; { when not a comment }
    end; { While NOT Eof }

  if FNames <> '' then
   if ReadBit(FilesInf.Attrib, 5) then                    { Allow DirectDL }
     begin
       for Counter := 01 to WordCount(FNames, DefExtractWord, true) do
         if NOT TaggingObj^.IsTaggedFile(ExtractWord(FNames, Counter, defExtractWord, true, false), FilesInf.AreaNum) then
           begin
             FindFirst(FilesInf.FilePath + ExtractWord(Fnames, Counter, defExtractWord, true, false), Anyfile -
                       VolumeID, Dirinfo);

             While DosError=00 do
              begin
                Fillchar(AreaObj.CurHdr^, SizeOf(FilesHdrRecord), #00);
                AreaObj.CurHdr^.Name := Dirinfo.Name;
                AreaObj.CurHdr^.Size := DirInfo.Size;
                AreaObj.CurHdr^.Crc32 := -1;
                AreaObj.CurHdr^.Uploader := GlobalCfg^.RaConfig^.SysOp;
                AreaObj.CurHdr^.UploadDate := 00;
                AreaObj.CurHdr^.FileDate := Dirinfo.Time;
                AreaObj.CurHdr^.Password := '';
                AreaObj.CurHdr^.Cost := FilesInf.Defcost;

                ShowMoment := true;
                if (DirInfo.Name <> '.') AND (Dirinfo.Name <> '..') then
                 if Dirinfo.Attr AND Directory = 0 then
                   if NOT AskToAdd(FilesInf, AreaObj.CurHdr^, ShowedHdr, 0, Dirinfo.Name,
                                 Counter, FNames, FilesInf.AreaNum, DirInfo.name, FilesAdded) then
                                   begin
                                     Add_ScanFileBase := false;
                                     BREAK;
                                   end; { if }

                FindNext(Dirinfo);
              end; { while }

             FindClose(Dirinfo);
           end; { for counter }
       end; { if }

  AreaObj.Done;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileSys, 'Add_ScanFileBase ( end )');
  {$ENDIF}
end; { func. Add_ScanFileBase }

var Files_F   : pFileObj;
    FilesInf  : FilesRecord;
    ShowMoment: Boolean;
begin
   {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logFileSys, 'AskToAddTag (begin) ('+Fnames+')');
   {$ENDIF}
   if FNames='' then EXIT;

   AskToAddTag := True;
   ShowMoment := True;
   FilesAdded := 00;

   If NOT IsAnyFile then
    begin
      If Global then
        begin
          WriteLn;
          WriteLn;

          New(Files_F, Init);
          Files_F^.Assign(FilesFileName);
          Files_F^.FileMode := ReadMode + DenyNone;
          Files_F^.Open(1);

          While (NOT Files_F^.EOF) AND (FNames <> '') AND
                 (NOT OutputObj^.StopMore) do
                  begin
                    Files_F^.BlkRead(FilesInf, SizeOf(FilesRecord));

                    FilesInf.FilePath := ForceBack(FilesInf.FilePath);
                    If Group then
                     With LineCfg^.Exitinfo^.Userinfo do
                      if (FilesInf.Group=FileGroup) OR
                          (FilesInf.AltGroup[1]=FileGroup) OR
                           (FilesInf.AltGroup[2]=FileGroup) OR
                            (FilesInf.AltGroup[3]=FileGroup) OR
                             (ReadBit(FilesInf.Attrib2, 0))
                              then
                               If NOT Add_ScanFileBase(FilesInf.AreaNum, ShowMoment, Global, FNames) then break;

                   IF NOT Group then
                    If NOT Add_ScanFileBase(FilesInf.AreaNum, ShowMoment, Global, FNames) then Break;
                  end; { While }


          Dispose(Files_F, Done);
        end { if global }
         else begin
                WriteLn;
                WriteLn;
                Add_ScanFileBase(AreaNum, ShowMoment, Global, Fnames);
              end; { if }

       OutputObj^.SetStopMore(false);
       WriteLn;
       WriteLn('`A14:',filesAdded, #32, LangObj^.ralGet(ralFilesAdd));
       InputObj^.PressEnter(False, False);
    end
     else MakeSpecificTagList(FNames, True);

   {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logFileSys, 'AskToAddTag ( end )');
   {$ENDIF}
end; { func. AskToAddTag }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
{$ENDIF}

function  GetLfnname(Name: String; RecordNum, AreaNum: Word): String;
{$IFNDEF MSDOS}
var AreaObj : FdbFileObj;
{$ENDIF}
begin
  GetLfnName := '';
  if (Name = '') then EXIT;

  if RecordNum = 0 then
    begin
      GetLfnName := Name;
      EXIT;
    end; { if }

  {$IFDEF MSDOS}
    GetLfnName := Name;
  {$ELSE}
    AreaObj.Init(AreaNum, false);
    AreaObj.Read(Recordnum - 01);
    GetLfnName := AreaObj.GetFileName;

    {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logTransfer, 'GetLfnName - Name(2) = '+AreaObj.GetFileName);
    {$ENDIF}

    AreaObj.Done;
  {$ENDIF}
end; { func. GetLfnName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
