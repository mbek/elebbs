program ListFile;
(*
**
** EleWEB EleXer source.
** LISTFILE - Show file areas
**
** Created: 07-October-2001
** Last update: 07-October-2001
**
*)

const
  MaxDescLen     = 41;           { maximum characters in one description line }

const
  maxCacheFiles = 15;


{-- Include standard type definitions ---------------------------------------}
{$I stdrec.inc}

{-- Include area records ----------------------------------------------------}
{$i arearec.inc}

{-- Include file header records ---------------------------------------------}
{$i fhdrrec.inc}


var AreaInf     : FilesRecord;
    EleAreaInf  : EleFilesRecord;
    FileHdr     : FilesHdrRecord;
    FileNameStr : String;
    RecordNum   : Integer;
    DescStr     : String;
    CharMap     : Array[0..255] of Char;
    DaysOld     : Integer;
    Security    : Integer;
    AreaNum     : Integer;
    SubAction   : Integer;
    KeyWord     : String;
    CheckGroup  : String;
    WildCard    : String;
    TmpBool     : Boolean;
    cache_Record: array[1..maxCacheFiles] of
                                     record
                                       FName     : String;
                                       Data      : String;
                                     end; { record }


    timing_StartTime : Real;
    timing_Finished  : Real;

{-- Include timing routines --------------------------------------------------}
{$I timefnc.inc}

{-- Include header and getuir routines ---------------------------------------}
{$I header.inc}
{$i getuir.inc}

procedure LoadCharMap;
var Counter: Integer;
    Char_F : Integer;
    TempStr: String;
begin
  {-- initialize the default charmap ----------------------------------------}
  for Counter := 0 to 255 do
    Charmap[Counter] := Chr(Counter);

  {-- Open the character file -----------------------------------------------}
  Char_F := FileAssign(GetPath_HtmlPath + 'charmap.ctl', 0, 3);
  FileOpen(Char_F);

  if FileGetError(Char_F) = 0 then
    begin
      While (FileGetError(Char_F) = 0) do
        begin
          FileReadStringLn(Char_F, TempStr);

          if Copy(TempStr, 1, 1) <> ';' then
            begin
              Counter := FVal(Copy(TempStr, 1, 3));

              CharMap[Counter] := Chr(FVal(Copy(TempStr, 8, 3)));
            end; { if }
        end; { while }
    end; { if }

  {-- close down the file ---------------------------------------------------}
  FileClose(Char_F);
  FileDone(Char_F);
end; { proc. LoadCharMap }

procedure FilterString(var TempStr: String);
var Counter: Integer;
    NewStr : String;
    TmpChar: Char;
begin
  NewStr := '';

  for Counter := 01 to Length(TempStr) do
    begin
      TmpChar := Copy(TempStr, Counter, 1);
      NewStr := NewStr + CharMap[Ord(TmpChar)];
    end; { for }

  TempStr := NewStr;
end; { proc. FilterString }


procedure DisplayDescText(TmpStr: String);
var PartStr : String;
begin
  {-- if the string is empty, at least show something -----------------------}
  if TmpStr = '' then
    TmpStr := #32;

  {-- now convert e-mail addresses to links, URL's to links -----------------}
  {-- we borrow an messagebase function for this ----------------------------}
  TmpStr := mb_HtmlString(TmpStr);

  {-- strings can have a maximum length of 254 characters. Because all ------}
  {-- special characters gets converted to &????; codes, theoretically a ----}
  {-- string with 255 characters can actually need 255 * 8 characters -------}
  {-- which is 2040 characters ----------------------------------------------}
  {-- what we do here, is divide the string into parts of 255 / 8 characters-}
  {-- (31 characters). We feed this to our convert string, and then display -}
  {-- the string. This is the most safest way to do this, but not the most --}
  {-- efficient. ------------------------------------------------------------}
  while TmpStr <> '' do
    begin
      {-- Copy the first part of the string ---------------------------------}
      PartStr := Copy(TmpStr, 1, 31);

      {-- delete this part of the string ------------------------------------}
      Delete(TmpStr, 1, 31);

      {-- now convert all high ascii codes according to the map supplied ----}
      {-- by the file CHARMAP.CTL -------------------------------------------}
      FilterString(PartStr);

      {-- and display the converted string ----------------------------------}
      Write(PartStr);
    end; { while }
end; { proc. DisplayDescText }


procedure ConvertMacros(var DataStr: String);
var Count     : Integer;
    TmpInt    : Integer;

    TmpChar   : Char;
    SaveMsg   : Integer;
    TmpStr    : String;
    NewStr    : String;
begin
  Count := 1;
  NewStr := '';
  
  if Pos('|', DataStr) > 0 then
    begin
      while DataStr <> ''do
        begin
          {-- find the first macro ------------------------------------------}
          TmpInt := Pos('|', DataStr);
          if TmpInt = 0 then
            Tmpint := Length(DataStr);

          {-- add everything that does not need concatenating ---------------}
          Concatenate(NewStr, Copy(DataStr, 1, TmpInt - 1));

          {-- and delete the non-macro code ---------------------------------}
          Delete(DataStr, 1, TmpInt);

          {-- now lets find the macro ---------------------------------------}
          if Length(DataStr) > 1 then
            begin
              TmpChar := DataStr[2];

              if DataStr[1] = 'Y' then
                begin
                   if TmpChar = 'A' then Concatenate(NewStr, AreaInf.Name)
                    else if TmpChar = 'B' then Concatenate(NewStr, FStr(AreaInf.Areanum))
                    else if TmpChar = 'C' then 
                           begin
                             Concatenate(NewStr, mb_HtmlString(DescStr));	
                           end { 'C' }
                    else if TmpChar = 'D' then Concatenate(NewStr, FStr(FileHdr.Size DIV 1024))
                    else if TmpChar = 'E' then 
                           begin
                              {-- make sure it ends with a trailing slash --------}
                              TmpStr := EleAreaInf.ExportURL;
                              if Copy(TmpStr, Length(TmpStr), 1) <> '/' then
                                TmpStr := TmpStr + '/';

                              {-- now add the filename ---------------------------}
                              TmpStr := TmpStr + fb_GetFileName;

                              {-- and now convert this to valid link -------------}
                              {-- (this replaces spaces with %20 etc -------------}
                              TmpStr := web_ConvertLink(TmpStr);

                              {-- and add this string ----------------------------}
                              Concatenate(NewStr, TmpStr);
                           end; { if }
                end
              else if DataStr[1] = 'Z' then
                    begin
                      if TmpChar = 'A' then Concatenate(NewStr, FileNameStr)
                       else if TmpChar = 'B' then Concatenate(NewStr, FStr(FileHdr.Size))
                       else if TmpChar = 'C' then Concatenate(NewStr, FileHdr.Uploader)
                       else if TmpChar = 'D' then Concatenate(NewStr, Date2Str(FileHdr.UploadDate))
                       else if TmpChar = 'E' then Concatenate(NewStr, Date2Str(FileHdr.FileDate))
                       else if TmpChar = 'F' then Concatenate(NewStr, Date2Str(FileHdr.LastDL))
                       else if TmpChar = 'G' then Concatenate(NewStr, FStr(FileHdr.TimesDL))
                       else if TmpChar = 'H' then Concatenate(NewStr, FileHdr.Password)
                       else if TmpChar = 'I' then Concatenate(NewStr, FileHdr.KeyWord[1])
                       else if TmpChar = 'J' then Concatenate(NewStr, FileHdr.KeyWord[2])
                       else if TmpChar = 'K' then Concatenate(NewStr, FileHdr.KeyWord[3])
                       else if TmpChar = 'L' then Concatenate(NewStr, FileHdr.KeyWord[4])
                       else if TmpChar = 'M' then Concatenate(NewStr, FileHdr.KeyWord[5])
                       else if TmpChar = 'N' then Concatenate(NewStr, FStr(FileHdr.Cost))
                       else if TmpChar = 'O' then Concatenate(NewStr, FStr(RecordNum))
                     end; { else }

              {-- and remove those two characters too -----------------------}
              if (DataStr[1] = 'Y') OR (DataStr[1] = 'Z') then
                Delete(DataStr, 1, 2)
                  else Concatenate(NewStr, '|');
            end; { if }
        end; { while }
    end
      else NewStr := DataStr;

  {-- Convert any EleBBS codes to user data ---------------------------------}
  DataStr := EleCodeStr(NewStr);
end; { proc. ConvertMacros }


procedure ShowHtmlFile(HtmlFile: String);
var HtmlHandle: Integer;
    Error     : Integer;
    DataStr   : String;
    Counter   : Integer;

    CachePtr  : Integer;
    CacheLne  : Integer;
    DidCache  : Boolean;
begin
  {-- Initialize variable ---------------------------------------------------}
  DidCache := false;

  {-- Is this file cached? --------------------------------------------------}
  for Counter := 1 to maxCacheFiles do
    if cache_Record[Counter].FName = HtmlFile then
      begin
        DidCache := true;

        DataStr := cache_Record[Counter].Data;
        ConvertMacros(DataStr); 
        Write(DataStr);
      end; { if }

  {-- initialize variables --------------------------------------------------}
  CachePtr := -1;
  CacheLne := 0;

  if NOT DidCache then
    begin
      {-- find an empty cached entry ----------------------------------------}
      for Counter := 1 to maxCacheFiles do
        if cache_Record[Counter].fName = '' then CachePtr := Counter;

      {-- and assign the data -----------------------------------------------}
      if CachePtr > 0 then
        cache_Record[CachePtr].FName := HtmlFile;

      {-- Assign the file to the proper handle etc --------------------------}
      HtmlHandle := FileAssign(HtmlFile, 0, 3);
      Error := FileGetError(HtmlHandle);

      {-- Now open the file -------------------------------------------------}
      if Error = 0 then
        FileOpen(HtmlHandle);
      Error := FileGetError(HtmlHandle);

      if (Error = 0) then
       begin
         {-- we try to read the whole string in once to prevent an expensive -}
         {-- loop. The FileReadString() routine maxes out at 64k which should-}
         {-- be more than enough ---------------------------------------------}
         FileReadString(HtmlHandle, DataStr);
         Error := FileGetError(HtmlHandle);

         {-- Add it to the cache ---------------------------------------------}
         cache_Record[CachePtr].Data := DataStr;

         {-- now convert the macros ------------------------------------------}
         if Error = 0 then
           ConvertMacros(DataStr);

         {-- and display it to the user --------------------------------------}
         if Error = 0 then
           Write(DataStr);
       end; { if }

    {-- Close the file -------------------------------------------------------}
    FileClose(HtmlHandle);
    FileDone(HtmlHandle);
  end; { if }
end; { proc. ShowHtmlFile }


function AccessToArea(AreaNum: Integer; CheckGroup, IsDownload: Boolean): Boolean;
var TmpBool: Boolean;
begin
  {-- Initialize variables --------------------------------------------------}
  TmpBool := TRUE;

  {-- make sure we can list this area ---------------------------------------}
  if NOT GetFilesRecord(AreaInf, AreaNum, TRUE) then
    TmpBool := FALSE;

  {-- Now actually check the access -----------------------------------------}
  if TmpBool then
    if NOT CheckFileAreaAccess(AreaInf,
                               IsDownload,
                               CheckGroup,
                               FALSE,
                               TRUE,
                               FVal(EleCodeStr('|F*'))) then TmpBool := FALSE;

  {-- and return the value --------------------------------------------------}
  AccessToArea := TmpBool;
end; { func. AccessToArea }

procedure ListArea(AreaNum, DaysOld: Longint;
                   KeyWord, Wildcard: String;
                   OnlyThisArea: Boolean;
                   var OneDisplayed: Boolean);
var CurItem     : Longint;
    TotalItems  : Longint;
    ShowArea    : Boolean;
    ShowFile    : Boolean;
begin
  {-- make sure we can list this area -----------------------------------------}
  if GetFilesRecord(AreaInf, AreaNum, TRUE) then
   if GetEleFilesRecord(EleAreaInf, AreaNum, TRUE) then
     ShowArea := TRUE;

  {-- Get the area record -----------------------------------------------------}
  if ShowArea then
    begin
      {-- and after the header, we open the filebase --------------------------}
      if fb_OpenFileBase(AreaInf.AreaNum) then
        begin
          {-- Initialize some variables ---------------------------------------}
          CurItem := 0;
          TotalItems := fb_TotalRecords;

          {-- Loop ------------------------------------------------------------}
          While (CurItem < TotalItems) do
            begin
              {-- Read the file and description -------------------------------}
              if fb_Read(CurItem) then ;
              fb_DoReadDescription;

              {-- default to showing this file --------------------------------}
              ShowFile := TRUE;

              {-- now check if we should display this file --------------------}
              if (KeyWord <> '') then
                ShowFile := fb_MatchShowing(1, KeyWord);

              if (DaysOld > 0) then
               if ShowFile then
                ShowFile := fb_MatchShowing(2, HistoryDate(DaysOld));

              if (WildCard <> '') then
               if ShowFile then
                ShowFile := fb_MatchShowing(3, Wildcard);

              {-- first display the header -------------------------------------}
              if ShowFile then
               if NOT OneDisplayed then
                 begin
                   ShowHtmlFile(GetPath_HtmlPath + 'fl_ahead.htm');
                   OneDisplayed := TRUE;
                 end; { if }

              {-- Now display the file ----------------------------------------}
              if NOT fb_IsComment then
                begin
                  {-- get the whole header record -----------------------------}
                  fb_GetHdrRecord(FileHdr);

                  {-- Fix up the filename -------------------------------------}
                  FileNameStr := fb_GetFileName;
                  RecordNum := CurItem;

                  {-- Get the first line of descriptions ----------------------}
                  DescStr := fb_GetDescLine(maxDescLen);

                  {-- if the filename is longer than 30 chars, cut off --------}
                  if Length(FileNameStr) > 30 then
                    FileNameStr := Copy(FileNameStr, 1, 27) + '...';

                  {-- now actually display the file ---------------------------}
                  if ShowFile then
                    if NOT fb_UnlistedFile then
                      begin
                        if NOT fb_IsMissing then
                          begin
                            ShowHtmlFile(GetPath_HtmlPath + 'fl_data.htm');
                          end
                            else begin
                                   ShowHtmlFile(GetPath_HtmlPath + 'fl_miss.htm');
                                 end; { else }
                      end; { unlisted }
                end { if not comment }
                  else begin
                         {-- is a comment -------------------------------------}
                         if ShowFile then
                           ShowHtmlFile(GetPath_HtmlPath + 'fl_comnt.htm');
                       end; { else }

              {-- then display the description --------------------------------}
              if NOT fb_UnlistedFile then
               if ShowFile then
                  while (NOT fb_EndOfDesc) do
                    begin
                      {-- Get the next line of descriptions -------------------}
                      DescStr := fb_GetDescLine(maxDescLen);

                      {-- display the description -----------------------------}
                      ShowHtmlFile(GetPath_HtmlPath + 'fl_data2.htm');
                    end; { while }

              {-- Make sure we get the next entry :-) -------------------------}
              CurItem := CurItem + 1;
            end; { while }
        end
          else MakeLogEntry('!', 'Unable to open filebase');

      {-- and display the header ----------------------------------------------}
      if OneDisplayed then
        ShowHtmlFile(GetPath_HtmlPath + 'fl_afoot.htm');
    end { if }
      {-- Unable to get area information --------------------------------------}
      else begin
             MakeLogEntry('!', 'Unable to retrieve area information. Exitting');
           end; { else }

  {-- If we only searched this area, and no files were found display an error -}
  if NOT OneDisplayed then
   if OnlyThisArea then
     begin
       {-- Show the area header ----------------------------------------------}
       ShowHtmlFile(GetPath_HtmlPath + 'fl_ahead.htm');

       {-- Show no files found -----------------------------------------------}
       ShowHtmlFile(GetPath_HtmlPath + 'fl_anone.htm');

       {-- Show the area footer ----------------------------------------------}
       ShowHtmlFile(GetPath_HtmlPath + 'fl_afoot.htm');
     end; { if }

  {-- close the filebase ------------------------------------------------------}
  fb_CloseFileBase;
end; { proc. ListArea }


procedure LoopAreas(Days: Integer; KeyWord, FName: String; CheckGroup: Boolean);
var FileHandle  : Integer;
    TmpNum      : Integer;
    DoShow      : Boolean;
    NoFilesAtAll: Boolean;
    Error       : Integer;
    FilesInf    : FilesRecord;
    DidDisplay  : Boolean;
begin
  {-- Initialize some variables ---------------------------------------------}
  NoFilesAtAll := TRUE;

  {-- we use a small trick to get the users file group-number, if we dont ---}
  {-- we would have to retrieve the whole users record - its a waste of -----}
  {-- resources -------------------------------------------------------------}
  TmpNum := FVal(EleCodeStr('|F*'));

  {-- Show the footer -------------------------------------------------------}
  ShowHtmlFile(GetPath_HtmlPath + 'fl_head.htm');

  {-- Assign the file to the proper handle etc ------------------------------}
  {-- (we dont need files.ele here, so we ignore it -------------------------}
  FileHandle := FileAssign(GetPath_FilesRa, 0, 3);

  {-- Now open the file -----------------------------------------------------}
  Error := FileGetError(FileHandle);
  if Error = 0 then
    FileOpen(FileHandle);
  Error := FileGetError(FileHandle);

  if Error = 0 then
   begin
     {-- Lets loop through the whole list of users --------------------------}
     While (Error = 0) do
       begin
         {-- Get the next record --------------------------------------------}
         FileRead(FileHandle, FilesInf);
         Error := FileGetError(FileHandle);

         {-- And display the users name -------------------------------------}
         if Error = 0 then
           begin
             {-- Make sure this user should be shown ------------------------}
             DoShow := CheckFileAreaAccess(FilesInf,
                                           FALSE,     { Check for download xs }
                                           CheckGroup,     { Check for groups }
                                           FALSE, { Chck if uplds are allowed }
                                           TRUE,              { Allow listing }
                                           TmpNum);   { Group number to check }

             {-- and show the entry -----------------------------------------}
             DidDisplay := FALSE;

             if DoShow then
               ListArea(FilesInf.AreaNum,
                        Days,
                        KeyWord,
                        WildCard,
                        FALSE,
                        DidDisplay);

             if DidDisplay then
               NoFilesAtAll := FALSE;
           end; { if }
       end; { while }
   end
     else begin
            WriteLn('<HTML> <BODY> <B> <H1>');
            WriteLn('Unable to display file!');
            WriteLn('</H> </B> </BODY> </HTML>');
          end; { else }


  {-- Close the file ---------------------------------------------------------}
  FileClose(FileHandle);
  FileDone(FileHandle);

  {-- if no files at all were displayed, we failed ---------------------------}
  if NoFilesAtAll then
    ShowHtmlFile(GetPath_HtmlPath + 'fl_none.htm');

  {-- Show the area header ---------------------------------------------------}
  ShowHtmlFile(GetPath_HtmlPath + 'fl_foot.htm');
end; { proc. LoopAreas }

begin
  {-- we begin by getting the actual logon information, and opening output ---}
  if web_GetFormData('newsubaction') = '' then
    begin
      GetUir;
      ShowHeader;
    end; { if }

  {-- Save the current time --------------------------------------------------}
  timing_Initialize(timing_StartTime);

  {-- Make sure this is not run on an anonymous logon ------------------------}
  if NOT web_IsLoggedOn then
    begin
      web_RunErrorScript(4, '');              { Show an "Access denied" error }
      Halt;                                           { Terminate this script }
    end; { if }

  {-- First get some variables -----------------------------------------------}
  TmpBool := FALSE;

  if web_GetFormData('newsubaction') = '' then
    SubAction := FVal(web_GetFormData('sub-action'))
     else SubAction := FVal(web_GetFormData('newsubaction'));

  DaysOld := FVal(web_GetFormData('days'));
  AreaNum := FVal(web_GetFormData('areanum'));
  Keyword := web_GetFormData('keyword');
  CheckGroup := web_GetFormData('checkgroup');
  WildCard := web_GetFormData('wildcard');

  {-- load character conversion table ----------------------------------------}
  LoadCharMap;

  {-- List area --------------------------------------------------------------}
  Case SubAction of
    00 : if AccessToArea(AreaNum, false, false) then
           ListArea(AreaNum,
                    DaysOld,
                    KeyWord,
                    WildCard,
                    true,                                    { only this area }
                    TmpBool);
    01 : LoopAreas(DaysOld,
                   Keyword,
                   WildCard,
                   (sLowCase(CheckGroup) = 'true'));
      else ;
  end; { case }


  {-- and stop the timer -----------------------------------------------------}
  timing_Done(timing_Finished);
  timing_ShowTimeFooter(timing_StartTime, timing_Finished);
end. { listfile }
