unit Listfile;
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
** The filelisting system for EleBBS's menufunctions.
**
** Copyright (c) 1997,1998 by Maarten Bekers
**
** Created : 01-Mar-1998
** Last update : 01-Mar-1998
**
** [Completely revised at 5 March 1998 and following week]
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 Uses Global, CfgRec, MgrFdb;

function NoMissingFile(var HdrInf: FilesHdrRecord): Boolean;
procedure AddToHeaderList(TagNumber: Byte;
                          RaFdb    : MgrFileObj;
                          Attrib   : Byte);
{$IFDEF WITH_FULL}
procedure ListFiles(MiscData: String; ShowType: FileShowType; UserStart: Boolean);
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
uses RAL,
      LineEd,
       StUtils,
        Control,
         FileRout,
          Debug_U,
           InOut_u,
            BitWise,
             Desc_U,
              Colors,
               ElLog_U,
                Access_U,
                 FileSys,
                  Support,
                   Ra2fdb,
                    Formlst,
                     Cases,
                      StrPath,
                       LongStr,
                         CentrStr,
                          WordStr,
                           Memman,
                            TagUnit,
                             ViewFile,
                              Mnuser,
                               Input_U,
                                FileObj,
                                 objDec;
{$ELSE}
  uses BitWise;
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
procedure AskParameters(var KeyWordStr: String; const ShowType: fileShowType; Const UserStart: Boolean);
begin;
  KeyWordStr := '';

  WriteLn;
  WriteLn;

  case Byte(ShowType) of
    Byte(fsNormal)   : ;
    Byte(fsKeyWord)  : begin
                         Write('`A12:',LangObj^.ralGet(ralKeyw));
                         GetString(KeyWordStr, 80, [#32..#254], False, False, False, False);
                         KeyWordStr := Trim(KeyWordStr);
                       end; { keyword }
    Byte(fsFileName) : begin
                         Write('`A12:',LangObj^.ralGet(ralWildcard));
                         GetString(KeyWordStr, 80, [#32..#254], False, False, False, False);
                       end; { FileName Search }
    Byte(fsNewFiles) : begin
                         if UserStart then
                          begin
                             WriteLn('`A11:');
                             If InputObj^.ralStrYesNoAsk(ralSinceLast) then
                              KeyWordStr := LineCfg^.Exitinfo^.Userinfo.LastDate
                               else begin
                                       if ProgTerminated then EXIT;
                                       GetDateStr(KeyWordStr, false, LangObj^.ralGet(ralNewerThan)+#32, false,true);
                                    end
                          end
                           else KeyWordStr := LineCfg^.Exitinfo^.UserInfo.LastDate;
                       end; { newfiles }
  end; { Case }
end; { proc. Askparameters }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowFileListLn(S: String; ClearLn: Boolean);
var Counter: Byte;
begin
  if OutputObj^.Last50COunter > 50 then
    OutputObj^.Last50Counter := 50;

  if OutputObj^.Last50Counter >= LineCfg^.Exitinfo^.Userinfo.ScreenLength then
   with OutputObj^ do
     begin
       For Counter := 02 to Last50Counter do
         Last50Lines^[Counter - 01] := Last50Lines^[Counter];

       Last50Lines^[Last50Counter] := '';
     end; { if }

  if ClearLn then
    With OutputObj^ do
      Last50Lines^[Last50Counter] := '';

  with OutputObj^ do
    Last50Lines^[Last50Counter] := Last50Lines^[Last50Counter] + S;

  if OutputObj^.Last50Counter < LineCfg^.Exitinfo^.Userinfo.ScreenLength then
   Inc(OutputObj^.Last50Counter);

  OutputObj^.DisplayStrLn(S, []);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'ShowFileListLn('''+S+'''''), DispMore='+Bool2Str(LineCfg^.DispMorePrompt)+', CurrLines='+
      FStr(contrObj^.TimeInfo.CurrLines)+', ScreenLen='+FStr(LineCfg^.Exitinfo^.Userinfo.ScreenLength));
  {$ENDIF}
end; { proc. ShowFileListLn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowFileList(S: String; ClearLn: Boolean);
begin
  with OutputObj^ do
    begin
      if Last50COunter > 50 then Last50Counter := 50;

      if ClearLn then Last50Lines^[Last50Counter] := '';
      Last50Lines^[Last50Counter] := Last50Lines^[Last50Counter] + S;
    end; { with }

  OutputObj^.DisplayStr(S, LineCfg^.PauseChecking, []);
end; { proc. ShowFileList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowListingHeader(ShowType: FileShowType);
begin
  OutputObj^.ClearScreen;

  If ShowType in [fsNormal] then
    begin;
      ShowFileListLn('`A10:' + LangObj^.ralGet(ralPauseStop), false);
      ShowFileListLn('`A03:' + OutputObj^.AvatarDupl(#196, NoColorLength(LangObj^.ralGet(ralPauseStop))), false);
    end; { ShowType }

  If Byte(ShowType) in [Byte(fsKeyWord),
       Byte(fsNewFiles), Byte(fsFileName)] then
    begin;
       ShowFileList('`A15:', false);

       Case Byte(ShowType) of
         Byte(fsKeyWord)  : ShowFileList(LangObj^.ralGet(ralSrcKeyW) + #32, false);
         Byte(fsNewFiles) : ShowFileList(LangObj^.ralGet(ralSrcByDate) + #32, false);
         Byte(fsFileName) : ShowFileList(LangObj^.ralGet(ralSrcName) + #32, false);
       End; { case }

      ShowFileListLn(LangObj^.ralGet(ralPauseStop), false);
    end; { ShowType }

  ShowFileListLn('', false);
end; { proc. ShowListingHeader }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function DoRealDisplay(const RaFdb      : FdbFileObj;
                       const ListFormat : String;
                       TaggingAllowed   : Boolean;
                       const DescPos,
                             MissingPos : Byte;
                       var   FirstFile  : Boolean;
                       var   NoTagAtAll : Boolean;
                       const ShowType   : FileShowType;
                       var   Tagnumber  : Byte;
                       const FilesInf   : FilesRecord): Boolean;
var DisplayStr : String;
    TagTemp    : String[1];
    Counter    : Byte;
    MaxDescLen : Byte;
    TempStr    : String;

    HdrLineAr  : DisplayLinesArray;
    HdrLinesNr : Byte;
begin
   DoRealDisplay := true;

{----------------------- Check if file isn't missing -----------------------}
   If RaFdb.IsMissing then                                       { Missing }
     begin
       DisplayStr := GlobalCfg^.RaConfig^.FileMissingLine;
       TaggingAllowed := false;
     end
      else DisplayStr := ListFormat;                { is file not missing? }

{------------------- Calculate the maximum width of descrip. ---------------}
   if TaggingAllowed then
     MaxDescLen := Abs(79 - DescPos)      { Determine max. length of desc. }
      else MaxDescLen := Abs(79 - Missingpos);

{---------------------- Initialize description array -----------------------}
   If NOT RaFdb.IsComment then
     DisplayListFile(RaFdb.CurHdr^, DisplayStr, HdrLineAr, RaFdb.GetDescLine(MaxDescLen), True, HdrLinesNr,
                     FilesInf, RaFdb.GetFileName, true);

{------------------ Check if first file, and update screen if so -----------}
   If Byte(ShowType) in [Byte(fsKeyWord), Byte(fsNewFiles), Byte(fsFileName)] then
    If FirstFile then
       begin
         ShowFileListLn('', false);
         ShowFileListLn('', false);
       end; { Skip the header-listing prompt }
   FirstFile := False;                       { Not the first file anymore! }

{--------------------- Add this file to the tagging list -------------------}
   If TaggingAllowed then
     begin
       if TagNumber > 100 then TagNumber := 100;

       AddToHeaderList(TagNumber, MgrFileObj(RaFdb), FilesInf.Attrib);

       NoTagAtAll := false;             { There are files tagged, so false }
       LineCfg^.Global_Tagging := true;    { We are tagging in more-prompt }
      end; { Tagging Files }

{--------------------- Check if this file is a comment ---------------------}
   if NOT RaFdb.IsComment then
     begin
       TagTemp := #32;            { Default is white-space, not tagged yet }

       {----------------- Check if file is already tagged ------------------}
       if TaggingObj^.IsTaggedFile(RaFdb.GetFileName, RaFdb.GetAreaNum) then
          begin
            TagTemp := '+';
          end; { if }

       {------------------------- Create color string ----------------------}
       With GlobalCfg^.RaConfig^ do
        if ((TagFore = 00) AND (TagBack = 00)) then
          TempStr := '`A07:'
           else TempStr := '`A' + FStr(MakeAttr(GlobalCfg^.RaConfig^.TagFore, GlobalCfg^.RaConfig^.TagBack)) + ':';

       if TaggingAllowed then
         TempStr := TempStr + LeadingZero(TagNumber, 02) + TagTemp
           else TempStr := TempStr + Dup(#32, 3);

        If TaggingAllowed then Inc(TagNumber);

       {--------------- Display the rest of all the format lines -----------}
        ShowFileListLn(TempStr + HdrLineAr[00], false);

        if HdrLinesNr>00 then
         for Counter := 01 to HdrLinesNr do
          if Counter < 25 then
           ShowFileListLn(HdrLineAr[Counter], false);
     end; { HeaderInf.Name }

{--------------------- Display the comment appropriatly --------------------}
   if RaFdb.IsComment then
     begin
       ShowFileListLn('`A15:' + RaFdb.GetDescLine(250), false);
     end; { if IsComment }

{--------------------- Display the desription lines ------------------------}
   While NOT RaFdb.EndOfDesc do
     begin
       if (TaggingAllowed) OR (NOT RaFdb.IsMissing) then
         TempStr := '`X' + FStr(DescPos) + ':' + RaFdb.GetDescLine(MaxDescLen)
           else TempStr := '`X' + FStr(MissingPos) + ':' + RaFdb.GetDescLine(MaxDescLen);

          ShowFileListLn(TempStr, false);
          if OutputObj^.StopMore then
            begin
              DoRealDisplay := FALSE;
              BREAK;
            end; { if }
       end; { for }
end; { proc. DoRealDisplay }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ListFiles(MiscData: String; ShowType: FileShowType; UserStart: Boolean);
label EndMemory;


procedure ProcessMiscData(var TaggingOn, CheckGroup, DoQuickScan: Boolean; var ListFormat: String;
                          var FilesInf: FilesRecord; var IndividArea: Boolean);
var TempArea: Longint;
begin
  ListFormat := '';
  IndividArea := false;

  If Pos('/A=', MiscData)>00 then
    ListFormat := Copy(MiscData, Pos('/A=', SUpcase(MiscData))+3, 250);
  If ListFormat='' then ListFormat := GlobalCfg^.RaConfig^.FileLine;

  TaggingOn := (Pos('/T', SUpcase(MiscData))>00);
  CheckGroup := FVal(GetValue('/FG=', MiscData, True)) > 00;
  if NOT CheckGroup then
   If Pos('/FG', SUpCase(MiscData)) > 00 then
     CheckGroup := true;
  DoQuickScan := (Pos('/Q', SUpcase(MiscData))>00);
  Replace('/T', '', MiscData);                              { Remove /T slash }
  Replace('/FG', '', MiscData);                            { Remove /FG slash }
  Replace('/Q', '', MiscData);

  If Pos('/F', SUpcase(MiscData))>00 then
    GetFilesRecord(FilesInf, LineCfg^.Exitinfo^.Userinfo.FileArea, True)
      else begin
             TempArea := FVal(FirstWord(MiscData, defExtractWord, false));

             GetFilesRecord(FilesInf, TempArea, True);

             if TempArea <> 0 then
               if (NOT (ShowType in [fsNormal])) then
                 IndividArea := true;
           end; { if }

  if Pos('/F', SUpCase(MiscData))>00 then
   begin
     if (NOT (Showtype in [fsNormal])) then
       IndividArea := True;

       MiscData := MiscData + #32 + FStr(FilesInf.AreaNum);
     Replace('/F', '', MiscData); {!!}
   end; { if }
end; { proc. ProcessMiscData }

var AreaObj        : FdbFileObj;
    AreaObjInitted : Boolean;
    TempCH         : Char;

    RaFileInitted  : Boolean;
    RaFile         : pFileObj;                { File-handle for FILES name }

    UseCR_Norm     : Boolean;
    UseCR_Missing  : Boolean;
    OldPause       : Boolean;
    SearchAborted  : Boolean;

    DescPos        : Byte; { Screen (X) position where to display description }
    MissingPos     : Byte;{ Screen (X) position where to display missing file }
    TagNumber      : Byte;             { Number currently shown as tag-number }
    PrevArea       : Word;                            { Previous area reading }
    IndividArea    : Boolean;   { (SEARCHING): Is this the only area to srch? }
    FirstFile      : Boolean;    { (SEARCHING): First file after header-show? }
    NoTagAtAll     : Boolean;                { There Are no files for tagging }
    TaggingFiles   : Boolean;               { Allowed downloads in this area? }
    ShowThisArea   : Boolean;                    { Show current area to user? }

    KeyWordStr     : String;
    TaggingOn,
    GroupChecking,
    QuickScan      : Boolean;
    ListFormat     : String;

    FilesInf       : FilesRecord;     { Current area files-record information }

    KeysStr        : String;                  { String for all available keys }
    CH             : Char;
    NumRead        : NumReadType;

    TotalItems     : Longint;
    ItemCounter    : Longint;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'Listfiles (begin)');
  {$ENDIF}

{------------------ Ask the parameters like Keyword etc --------------------}
  AskParameters(KeyWordStr, ShowType, UserStart);    { Ask for keyword etc }
  ProcessMiscData(TaggingOn, GroupChecking,    { Process Misc. Data string }
                  QuickScan, ListFormat, FilesInf, IndividArea);

  If Byte(ShowType) <> Byte(fsNormal) then
   If KeyWordStr = '' then EXIT;                     { No search-string given }

{--------------- Setup scrollback buffer used after viewing etc ------------}
  with OutputObj^ do
    begin
      FilLChar(Last50Lines^, SizeOf(Last50LinesRec), #00);
      Last50Counter := 01;
    end; { with }
  LineCfg^.DispMorePrompt := true;
  LineCfg^.IsSearching := (Byte(ShowType) <> Byte(fsNormal));

{---------------- Show the header like "Press P to pause" ------------------}
  ShowListingHeader(ShowType);

{------------- Allocate and "clean up" memory used for tagging -------------}
  {$IFDEF MSDOS}
    if MemAvail < SizeOf(TagHeaderList) then
        begin
          RaLog('!', 'Not enough memory to perform filearea browsing operation.');
          GOTO EndMemory
        end; { if }
  {$ENDIF}
  
  AllocMem(LineCfg^.Headerlist, SizeOf(TagHeaderList), 'TagHeaderList', 'ListFiles');
  FillChar(LineCfg^.HeaderList^, SizeOf(TagHeaderList), #00);


{-------------------- Initialize some of the variables ---------------------}
  TagNumber    := 01;
  PrevArea     := FVal(FirstWord(MiscData, defExtractWord, false));
  if NOT IndividArea then
    IndividArea := (PrevArea <> 00);
  NoTagAtAll   := true;
  FirstFile    := true;
  TaggingFiles := NOT NoTagAtAll;
  RaFileInitted:= false;
  AreaObjInitted := false;
  SearchAborted := false;


{------------------------ Initialize the area list -------------------------}
  New(RaFile, Init);
  RaFile^.Assign(FilesFileName);
  RaFile^.FileMode := ReadMode + DenyNone;
  RaFileInitted := TRUE;

{------------------------------ Open the file ------------------------------}
  if NOT RaFile^.Open(1) then
    begin
      Dispose(RaFile, Done);
      RAFileInitted := False;

      ShowFileListLn('', false);
      ShowFileListLn('`A12:' + LangObj^.ralGet(ralSrcAbrt), false);

      InputObj^.PressEnter(true, true);
      GOTO EndMemory;
    end; { FILES-file not found }

{---------------------- Make sure pausechecking is off ---------------------}
  OldPause := LineCfg^.PauseChecking;
  LineCfg^.PauseChecking := false;


{--------------------- Log the current activity ----------------------------}
  if ShowType in [fsNormal] then
     RaLog('>', 'Browsing file area #'+FStr(FilesInf.AreaNum)+' : '+FilesInf.Name);

{----------------- Get the column coordinate to show the desc --------------}
  GetDescriptionPos(DescPos, ListFormat, True, UseCR_Norm);
  GetDescriptionPos(MissingPos, GlobalCfg^.RaConfig^.FileMissingLine, True, UseCR_Missing);
  if NOT UseCR_Norm then Inc(DescPos, 3);
  if NOT UseCR_Missing then Inc(MissingPos, 3);

  repeat
    {------------------- Seeking to area position --------------------------}
    if ShowType in [fsNormal] then         { Seek to area listing files in }
       RAFILE^.Seek((Ra250Area(FilesInf.AreaNum) - 1) * SizeOf(FilesRecord));

    if (IndividArea) AND (PrevArea>00) then { Specified area# on miscdata-line }
      RAFILE^.Seek((Ra250Area(PrevArea) - 01) * SizeOf(FilesRecord));

    {------------ Read in area record and check for correctnes -------------}
    NumRead := RAFILE^.BlkRead(FilesInf, SizeOf(FilesRecord));
    if NumRead <> SizeOf(FilesRecord) then BREAK;

    FilesInf.Filepath := ForceBack(FilesInf.FilePath);
    LineCfg^.Search_Skip := false;

    {------------ Check wether the user wants to abort or not --------------}
    If InputObj^.Keypressed then
      begin
        TempCH := UpCase(InputObj^.ReadKey);
        if TempCH in ['S'] then
          begin
            SearchAborted := true;
            BREAK;
          end; { if }
        if TempCH in ['P'] then InputObj^.ReadKey;
      end; { if }
    if Progterminated then GOTO EndMemory;
    contrObj^.SetTimeOut;        { Need to set this so search won't abort! }

    {---------------- Check wether user has download access ----------------}
    If TaggingOn then
     TaggingFiles := CheckFileAreaAccess(FilesInf, True, GroupChecking, False, False, 00,
                                          LineCfg^.Exitinfo^)
      else TaggingFiles := False;

    {---------------- Check to see if we need to show this area ------------}
    ShowThisArea := true;
    If ShowType in [fsNewFiles] then                  { Searching on date? }
     begin
       ShowThisArea := ReadBit(FilesInf.Attrib, 0);

       If ReadBit(FilesInf.Attrib, 3) then             { Area is on CD-ROM }
        If NOT GlobalCfg^.RaConfig^.IncludeNewCDareas then
         ShowThisArea := False;
     end; { Datesearcher }

    {-------------------- First file in this area --------------------------}
    FirstFile := True;                          { First file from this area }
    if NOT CheckFileAreaAccess(FilesInf, False, GroupChecking, False, True, 00,
                                LineCfg^.Exitinfo^) then
       ShowThisArea := False;
    if OutputObj^.StopMore then ShowThisArea := false;

    {------------------ Update the user screen accordingly -----------------}
    if ShowThisArea then
     If Byte(ShowType) in [Byte(fsKeyWord), Byte(fsNewFiles), Byte(fsFileName)] then
          begin                                                { Searching }
            If FirstFile then ShowFileList('`X1:', false)
              else begin
                     ShowFileListLn('', false);
                   end; { else }

            ShowFileList('`A15:' + MakeLen(MakeLen(FStr(FilesInf.AreaNum), 4, Space, True, false) + '  ... `A14:'+
                           FilesInf.Name, 60, Space, False, false), true);
            InputObj^.UpdateScrn;
          end; { Currently in search }

    {---------------------- Initialize the area ----------------------------}
    if ShowThisArea then
      begin
        AreaObj.Init(FilesInf.AreaNum, false);
        if AreaObj.GetError <> 00 then
          begin
            ShowThisArea := false
          end
           else begin
                  TotalItems := AreaObj.TotalRecords;
                  ItemCounter := 00;
                end; { if }

        AreaObjInitted := true;
      end; { if ShowThisArea }

    {--------------------- Start displaying those fiels --------------------}
    If ShowThisArea then
      While (ItemCounter < TotalItems) AND
             (NOT OutputObj^.StopMore) AND (TotalItems>00) AND (NOT ProgTerminated)
              AND (NOT LineCfg^.Search_Skip) do
              begin
                {-- Check wether the user wants to abort or not -----------}
                if InputObj^.Keypressed then
                  begin
                    TempCH := UpCase(InputObj^.ReadKey);
                    if TempCH in ['S'] then
                      begin
                        SearchAborted := true;
                        BREAK;
                      end; { if }
                    if TempCH in ['P'] then InputObj^.ReadKey;
                  end; { if }


                AreaObj.Read(ItemCounter);      { Read header sequentially }
                Inc(ItemCounter);

                if ShowType = fsKeyWord then        { Read the description }
                  AreaObj.ReadDescription(AreaObj.TxtF, AreaObj.GetLongDescPtr);

                {--------------------- Setup tagging -----------------------}
                LineCfg^.Global_Tagging := NOT NoTagAtAll;
                If TagNumber<01 then TagNumber := 01;    { Wrap tag-buffer }
                If TagNumber>99 then TagNumber := 01;


                {------------------------ Last Check! ----------------------}
                if ShowFile(ShowType, AreaObj.CurIdx^,
                            AreaObj.CurTxt^, KeyWordStr, QuickScan,
                            AreaObj.GetFileName) then
                    begin
                     if ShowType <> fsKeyWord then   { Read the description }
                        AreaObj.ReadDescription(AreaObj.TxtF, AreaObj.GetLongDescPtr);

                     if NoMissingFile(AreaObj.CurHdr^) then
                      if ((ShowType in [fsNormal]) OR (NOT AreaObj.IsComment)) then
                       begin
                         if NOT DoRealDisplay(AreaObj, ListFormat,
                                              TaggingFiles, DescPos,
                                              MissingPos, FirstFile,
                                              NoTagAtAll, ShowType, TagNumber,
                                              FilesInf)
                                               then BREAK;
                       end; { Show File }
                    end; { if ShowFile }
              end; { until end of area }

    {------------------ Area is completely checkred and disp. --------------}
    if OutputObj^.StopMore then BREAK;

    {------------------------ Close area-files -----------------------------}
    if ShowThisArea then
      begin
        if AreaObjInitted then AreaObj.Done;
        AreaObjInitted := false;

        If IOResult>00 then Goto EndMemory;
      end; { if }

    {------------------- Check to see if at end of arealist ----------------}
    If ShowType in [fsNormal] then BREAK;           { Only show this area! }

    If IndividArea then
      begin
        If Length(Trim(MiscData))=00 then Break;

        PrevArea := FVal(FirstWord(MiscData, defExtractWord, false));
        If (PrevArea=00) then Break;
      End; { Individual areas search }

    {------------------ Update the user screen accordingly -----------------}
    if NOT FirstFile then
      ShowFileListLn('', true);

  until (True=False) OR (ProgTerminated) OR (SearchAborted);


  {-------------------- Check wether tag-prompt or no tag prompt -----------}
  TaggingFiles := TaggingOn;
  ShowFileList('`X:`E:', true);

  If (NOT TaggingFiles) OR (NoTagAtAll) then         { Tagging not enabled }
      InputObj^.PressEnter(True, True);

  {---------------------- Do show an tag more prompt -----------------------}
  If (TaggingFiles) AND (NOT NoTagAtAll) AND (NOT ProgTerminated) then
   if (NOT OutputObj^.StopMore) then
    begin;
      KeysStr := LangObj^.ralGetKeys(ralTVE);

      LineCfg^.DispMorePrompt := False;
      ShowFileListLn('', false);
      LineCfg^.DispMorePrompt := True;

      repeat;
        ShowFileList('`X1:`E:`F' + FStr(GlobalCfg^.RaConfig^.CRFore) + ':`B' + FStr(GlobalCfg^.RaConfig^.CRBack) + ':' +
                     LangObj^.ralGetStr(ralTVE), false);
        CH := UpCase(InputObj^.ReadKey);
        if ProgTerminated then Goto EndMemory;

        If UpCase(CH) = KeysStr[1] then TagFiles(LineCfg^.HeaderList^);
        If UpCase(CH) = KeysStr[2] then ViewTagFile(LineCfg^.HeaderList^, LineCfg^.Exitinfo^);
        If UpCase(CH) = KeysStr[3] then begin
                                             EditTagList(True, False, False, False, FilesInf.AreaNum);
                                             OutputObj^.DoRefresh;
                                             Write('`X1:');
                                             Write(OutputObj^.MakeClrEolstr);
                                           end; { EditTagList }

       Until (CH in [#13]) OR (ProgTerminated);

       WriteLn;                                           { For non-ansi menus }
       WriteLn;
    end; { If Tagging }

{--------------------------- Eventually end up here ---------------------------}
EndMemory:
  OutputObj^.SetStopMore(false);

  LineCfg^.PauseChecking := OldPause;
  LineCfg^.Global_tagging := false;

  ReleaseMem(LineCfg^.HeaderList, SizeOf(TagHeaderList));

  {$i-}
    if RaFileInitted then Dispose(RaFile, Done);
    if AreaObjInitted then AreaObj.Done;
  {$I+}
  If IOResult>00 then;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'Listfiles ( end )');
  {$ENDIF}
end; { proc. ListFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
{$ENDIF}

function NoMissingFile(var HdrInf: FilesHdrRecord): Boolean;
begin
  NoMissingFile := FALSE;

  if HdrInf.Name[0] = #0 then           { If its a comment, always show it }
    begin
      NoMissingFile := TRUE;
      EXIT;
    end; { if }

  if ReadBit(HdrInf.Attrib, 5) then                              { Missing }
    if NOT GlobalCfg^.RaConfig^.ShowMissingFiles then EXIT;
  If ReadBit(HdrInf.Attrib, 0) then EXIT;                        { Deleted }
  If ReadBit(HdrInf.Attrib, 1) then EXIT;                       { Unlisted }

  NoMissingFile := TRUE;
end; { func. NoMissingFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AddToHeaderList(TagNumber: Byte;
                          RaFdb    : MgrFileObj;
                          Attrib   : Byte);
begin
  LineCfg^.HeaderList^[TagNumber].Name      := RaFdb.GetShortName;
  LineCfg^.HeaderList^[TagNumber].AreaNum   := RaFdb.GetAreaNum;
  LineCfg^.HeaderList^[TagNumber].RecordNum := RaFdb.CurrentRecord;
  LineCfg^.HeaderList^[TagNumber].Attrib    := RaFdb.GetAttrib;
  LineCfg^.HeaderList^[TagNumber].AreaAttrib:= Attrib;
  LineCfg^.HeaderList^[TagNumber].Password  := RaFdb.GetPassword;
end; { proc. AddToheaderList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
