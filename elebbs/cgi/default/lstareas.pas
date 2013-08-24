program LstAreas;
(*
**
** EleWEB EleXer source.
** List all areas/groups
**
** Created: 24-may-2001
** Last update: 14-Jul-2002
** Written by: Maarten Bekers
**
*)


{-- Include standard type definitions ---------------------------------------}
{$I stdrec.inc}

{-- Include area records ----------------------------------------------------}
{$i arearec.inc}

{-- Special index records for the forum style. ------------------------------}
{$I msgidx.inc}

{-- Web configuration record ------------------------------------------------}
{$I cfgrec.inc}


var FileHandle  : Integer;
    Error       : Integer;
    DoShow      : Boolean;
    SubAction   : Integer;
    GroupInf    : GroupRecord;
    webCfgInf   : webCfgRecord;
    FilesInf    : FilesRecord;
    MessageInf  : MessageRecord;
    ForumInf    : ForumInfoRecord;
    TopicInf    : TopicIdxRecord;
    TopicHdr    : TopicHdrRecord;

    LoginUnix   : Integer[4];       { Unix timestamp of last login time/date }

    timing_StartTime : Real;
    timing_Finished  : Real;


{-- Include header and getuir routines ---------------------------------------}
{$I header.inc}
{$i getuir.inc}

procedure ShowError(TmpStr: String);
begin
  WriteLn;
  WriteLn;
  WriteLn('<HTML> <BODY> <B> <H1>');
  WriteLn('Unable to display file!<BR><BR><BR>');
  WriteLn(TmpStr, '<BR><BR><BR>');
  WriteLn('</H> </B> </BODY> </HTML>');
end; { proc. ShowError }

{-- Include some of the IDX support routines --------------------------------}
{$I idxsup.inc}

{-- Include some basic date/time routines -----------------------------------}
{$I datefnc.inc}

{-- Include web configuration record functions -------------------------------}
{$i cfgfnc.inc}

{-- Include timing routines --------------------------------------------------}
{$I timefnc.inc}

procedure ShowGroupEntry(IsMessage, IsForum: Boolean);
var HtmlHandle: Integer;
    Error     : Integer;
    Count     : Integer;
    TmpInt    : Integer;
    TmpChar   : Char;
    DataStr   : String;
begin
  {-- Assign the file to the proper handle etc ------------------------------}
  if IsMessage then
    begin
      if IsForum then
        HtmlHandle := FileAssign(GetPath_HtmlPath + 'frm_grp.htm', 0, 3)
          else HtmlHandle := FileAssign(GetPath_HtmlPath + 'mgrpdata.htm', 0, 3)
    end { if }
      else begin
              if IsForum then
                HtmlHandle := FileAssign(GetPath_HtmlPath + 'frm_grp.htm', 0, 3)
                  else HtmlHandle := FileAssign(GetPath_HtmlPath + 'fgrpdata.htm', 0, 3);
           end; { else }

  Error := FileGetError(HtmlHandle);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileOpen(HtmlHandle);
  Error := FileGetError(HtmlHandle);

  if (Error = 0) AND (DoShow) then
   begin
     {-- Loop through the whole data file -----------------------------------}
     while (error = 0) do
       begin
         {-- Read the next line ---------------------------------------------}
         FileReadStringLn(HtmlHandle, DataStr);
         Error := FileGetError(HtmlHandle);

         {-- now convert all "internal" codes to the data we have -------}
         Count := 0;
         TmpInt := Pos('|Z', DataStr);

         while (TmpInt > 0) AND (Count < 20) AND (Error = 0) do
           begin
             if ((TmpInt + 1) < Length(DataStr)) then
               begin
                 TmpChar := Copy(DataStr, TmpInt + 2, 1);

                 Case TmpChar of
                   'A' : Replace('|ZA', GroupInf.Name, DataStr, false);
                   'B' : Replace('|ZB', FStr(GroupInf.AreaNum), DataStr, false);
                   'C' : Replace('|ZC', FStr(GroupInf.Security), DataStr, false);
                     {-- and you can add more ;-) }
                     else Replace('|Z' + TmpChar, '[Unknown macro]', DataStr, false);
                 end; { case }
               end; { if }

             Count := Count + 1;
             TmpInt := Pos('|Z', DataStr);
           end; { while }

         {-- Convert any EleBBS codes to user data ----------------------}
         DataStr := EleCodeStr(DataStr);
    
         {-- and display it to the user ---------------------------------}
         WriteLn(DataStr);
       end; { while }
   end; { if }

  {-- Close the file --------------------------------------------------------}
  FileClose(HtmlHandle);
  FileDone(HtmlHandle);
end; { proc. ShowGroupEntry }



procedure ShowFilesEntry;
var HtmlHandle: Integer;
    Error     : Integer;
    Count     : Integer;
    TmpInt    : Integer;
    TmpChar   : Char;
    DataStr   : String;
begin
  {-- Assign the file to the proper handle etc ------------------------------}
  HtmlHandle := FileAssign(GetPath_HtmlPath + 'faredata.htm', 0, 3);
  Error := FileGetError(HtmlHandle);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileOpen(HtmlHandle);
  Error := FileGetError(HtmlHandle);

  if (Error = 0) AND (DoShow) then
   begin
     {-- Loop through the whole data file -----------------------------------}
     while (error = 0) do
       begin
         {-- Read the next line ---------------------------------------------}
         FileReadStringLn(HtmlHandle, DataStr);
         Error := FileGetError(HtmlHandle);

         {-- now convert all "internal" codes to the data we have -------}
         Count := 0;
         TmpInt := Pos('|Z', DataStr);

         while (TmpInt > 0) AND (Count < 20) AND (Error = 0) do
           begin
             if ((TmpInt + 1) < Length(DataStr)) then
               begin
                 TmpChar := Copy(DataStr, TmpInt + 2, 1);

                 Case TmpChar of
                   'A' : Replace('|ZA', FilesInf.Name, DataStr, false);
                   'B' : Replace('|ZB', FStr(FilesInf.AreaNum), DataStr, false);
                   'C' : Replace('|ZC', FStr(FilesInf.Security), DataStr, false);
                     {-- and you can add more ;-) }
                     else Replace('|Z' + TmpChar, '[Unknown macro]', DataStr, false);
                 end; { case }
               end; { if }

             Count := Count + 1;
             TmpInt := Pos('|Z', DataStr);
           end; { while }

         {-- Convert any EleBBS codes to user data ----------------------}
         DataStr := EleCodeStr(DataStr);
    
         {-- and display it to the user ---------------------------------}
         WriteLn(DataStr);
       end; { while }
   end; { if }

  {-- Close the file --------------------------------------------------------}
  FileClose(HtmlHandle);
  FileDone(HtmlHandle);
end; { proc. ShowFilesEntry }


procedure ReadForumInfo(JamPath: String; AreaNum: Integer);
var InfoFile: Integer;
    TmpStr  : String;
    Count   : Integer;
    Error   : Integer;
begin
  {-- Initialize variables --------------------------------------------------}
  Count := 0;
  ForumInf.Moderator := 'No forum' + FStr(AreaNum) +'.ini found';
  ForumInf.Description := '';
  ForumInf.PruneDays := 30;

  {-- Open the forum info file ----------------------------------------------}
  InfoFile := FileAssign(JamPath + 'forum' + FStr(AreaNum) + '.ini', 0, 3);
  Error := FileGetError(InfoFile);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileOpen(InfoFile);
  Error := FileGetError(InfoFile);

  if (Error = 0) AND (DoShow) then
   begin
     {-- Loop through the whole data file -----------------------------------}
     while (error = 0) do
       begin
         {-- Read the next line ---------------------------------------------}
         FileReadStringLn(InfoFile, TmpStr);
         Error := FileGetError(InfoFile);

         {-- get it ---------------------------------------------------------}
         if (FirstChar(TmpStr) <> ';') AND (TmpStr <> '') then
           begin
             if Count = 0 then
               ForumInf.Moderator := TmpStr
                else if Count = 1 then ForumInf.Description := TmpStr
                else if Count = 2 then ForumInf.PruneDays := FVal(TmpStr);

             Count := Count + 1;
           end; { if }
       end; { while }
   end; { while }

  {-- close te file ---------------------------------------------------------}
  FileClose(InfoFile);
  FileDone(InfoFile);
end; { proc. ReadForumInfo }


procedure ShowMessageEntry(IsForum: Boolean);
var HtmlHandle: Integer;
    Error     : Integer;
    Count     : Integer;
    TmpInt    : Integer;
    TmpChar   : Char;
    DataStr   : String;
begin
  {-- Read the index file for this area -------------------------------------}
  ReadIdxHdr(JustPath(MessageInf.JamBase), MessageInf.AreaNum);

  {-- Assign the file to the proper handle etc ------------------------------}
  if IsForum then
    begin
      if HasNewMessages(TopicHdr.LastPostDate) then
        HtmlHandle := FileAssign(GetPath_HtmlPath + 'frm_aron.htm', 0, 3)
          else HtmlHandle := FileAssign(GetPath_HtmlPath + 'frm_arof.htm', 0, 3);
    end
     else HtmlHandle := FileAssign(GetPath_HtmlPath + 'maredata.htm', 0, 3);
  Error := FileGetError(HtmlHandle);

  {-- if this is forum, get the area info -----------------------------------}
  if IsForum then
    ReadForumInfo(JustPath(MessageInf.JamBase), MessageInf.Areanum);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileOpen(HtmlHandle);
  Error := FileGetError(HtmlHandle);

  if (Error = 0) AND (DoShow) then
   begin
     {-- Loop through the whole data file -----------------------------------}
     while (error = 0) do
       begin
         {-- Read the next line ---------------------------------------------}
         FileReadStringLn(HtmlHandle, DataStr);
         Error := FileGetError(HtmlHandle);

         {-- now convert all "internal" codes to the data we have -----------}
         Count := 0;
         TmpInt := Pos('|Z', DataStr);

         while (TmpInt > 0) AND (Count < 20) AND (Error = 0) do
           begin
             if ((TmpInt + 1) < Length(DataStr)) then
               begin
                 TmpChar := Copy(DataStr, TmpInt + 2, 1);

                 Case TmpChar of
                   'A' : Replace('|ZA', MessageInf.Name, DataStr, false);
                   'B' : Replace('|ZB', FStr(MessageInf.AreaNum), DataStr, false);
                   'C' : Replace('|ZC', ForumInf.Moderator, DataStr, false);
                   'D' : Replace('|ZD', ForumInf.Description, DataStr, false);
                   'E' : Replace('|ZE', FStr(TopicHdr.NumMsgs), DataStr, false);
                   'F' : Replace('|ZF', FStr(TopicHdr.NumThreads), DataStr, false);
                   'G' : Replace('|ZG', TopicHdr.LastPoster, DataStr, false);
                   'H' : Replace('|ZH', Date2Str(Unix2Date(TopicHdr.LastPostDate)), DataStr, false);
                   'I' : Replace('|ZI', FStr(ForumInf.PruneDays), DataStr, false);
                     {-- and you can add more ;-) }
                     else Replace('|Z' + TmpChar, '[Unknown macro]', DataStr, false);
                 end; { case }
               end; { if }

             Count := Count + 1;
             TmpInt := Pos('|Z', DataStr);
           end; { while }

         {-- Convert any EleBBS codes to user data ----------------------}
         DataStr := EleCodeStr(DataStr);

         {-- and display it to the user ---------------------------------}
         WriteLn(DataStr);
       end; { while }
   end; { if }

  {-- Close the file --------------------------------------------------------}
  FileClose(HtmlHandle);
  FileDone(HtmlHandle);
end; { proc. ShowMessageEntry }



procedure ListGroups(IsMessage: Boolean);
begin
  {-- Assign the file to the proper handle etc ------------------------------}
  if IsMessage then
    begin
      FileHandle := FileAssign(GetPath_MGroupsRa, 0, 3);
      if NOT web_ShowHtmlFile('mgrphead.htm') then
        ShowError('Unable to display mgrphead.htm');
    end
     else begin
            FileHandle := FileAssign(GetPath_FGroupsRa, 0, 3);
            if NOT web_ShowHtmlFile('fgrphead.htm') then
              ShowError('Unable to display fgrphead.htm');
          end; { else }

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
         FileRead(FileHandle, GroupInf);
         Error := FileGetError(FileHandle);

         {-- And display the users name -------------------------------------}
         if Error = 0 then
           begin
             {-- Make sure this user should be shown ------------------------}
             DoShow := TRUE;

             if NOT CheckGroupAccess(GroupInf) then     { do we have access? }
               DoShow := FALSE;

             {-- Do not show forums in the regular listing ------------------}
             if GroupInf.Typ = 1 then                                { Forum }
               begin
                 DoShow := false;
               end; { if }


             {-- and show the entry -----------------------------------------}
             if DoShow then
               ShowGroupEntry(IsMessage, false);
           end; { if }
       end; { while }

     {-- Show the online list footer ----------------------------------------}
     if IsMessage then
       begin
         if NOT web_ShowHtmlFile('mgrpfoot.htm') then
           ShowError('Unable to display mgrpfoot.htm');
       end
         else begin
                if NOT web_ShowHtmlFile('fgrpfoot.htm') then
                  ShowError('Unable to display fgrpfoot.htm');
              end; { else }
   end
     else begin
            WriteLn('<HTML> <BODY> <B> <H1>');
            WriteLn('Unable to display file!');
            WriteLn('</H> </B> </BODY> </HTML>');
          end; { else }


   {-- Close the file -------------------------------------------------------}
   FileClose(FileHandle);
   FileDone(FileHandle);
end; { proc. ListGroups }



procedure ListFileAreas(CheckGroups: Boolean);
var TmpNum: Integer;
begin
  {-- we use a small trick to get the users file group-number, if we dont ---}
  {-- we would have to retrieve the whole users record - its a waste of -----}
  {-- resources -------------------------------------------------------------}
  TmpNum := FVal(EleCodeStr('|F*'));

  {-- Assign the file to the proper handle etc ------------------------------}
  {-- (we dont need files.ele here, so we ignore it -------------------------}
  FileHandle := FileAssign(GetPath_FilesRa, 0, 3);
  if NOT web_ShowHtmlFile('farehead.htm') then
    ShowError('Unable to display farehead.htm');

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
                                           CheckGroups,    { Check for groups }
                                           FALSE, { Chck if uplds are allowed }
                                           TRUE,              { Allow listing }
                                           TmpNum);   { Group number to check }

             {-- Do not show forums in the regular listing ------------------}
             if GroupInf.Typ = 1 then                                { Forum }
               begin
                 DoShow := false;
               end; { if }

             {-- and show the entry -----------------------------------------}
             if DoShow then
               ShowFilesEntry;
           end; { if }
       end; { while }

     {-- Show the online list footer ----------------------------------------}
     if NOT web_ShowHtmlFile('farefoot.htm') then
       ShowError('Unable to display farefoot.htm');
   end
     else begin
            WriteLn('<HTML> <BODY> <B> <H1>');
            WriteLn('Unable to display file!');
            WriteLn('</H> </B> </BODY> </HTML>');
          end; { else }


   {-- Close the file -------------------------------------------------------}
   FileClose(FileHandle);
   FileDone(FileHandle);
end; { proc. ListFileAreas }


procedure ListMessageAreas(CheckGroups, IsForum: Boolean; TmpNum: Integer);
var IsFrontPage: Boolean;
begin
  {-- Assign the file to the proper handle etc ------------------------------}
  {-- (we dont need files.ele here, so we ignore it -------------------------}
  FileHandle := FileAssign(GetPath_MessagesRa, 0, 3);

  if NOT IsForum then
    begin
      if NOT web_ShowHtmlFile('marehead.htm') then
        ShowError('Unable to display marehead.htm');
    end; { if }

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
         FileRead(FileHandle, MessageInf);
         Error := FileGetError(FileHandle);

         {-- And display the users name -------------------------------------}
         if Error = 0 then
           begin
             {-- Make sure this user should be shown ------------------------}
             DoShow := TRUE;

             DoShow := CheckMsgAreaAccess
                                   (MessageInf,
                                    Checkgroups,     { Check for group access }
                                    true,{ Chck for read xs (else its write!) }
                                    TmpNum);                   { Group number }

             {-- Do not show netmails in the regular listing ----------------}
             if MessageInf.Typ = 1 then                            { Netmail }
               begin
                 DoShow := false;
               end; { if }

             {-- Is this a frontpage? ---------------------------------------}
             {-- if it is, do not show it to users below SysOP level access -}
             IsFrontPage := (MessageInf.AreaNum = webCfgInf.FrontPageNum);
             if IsFrontPage then
               begin
                 if NOT web_IsLoggedOn then
                    DoShow := false;

                 if NOT CheckMsgSysopAccess(MessageInf) then
                   DoShow := false;
               end; { if }

             {-- and show the entry -----------------------------------------}
             if DoShow then
               begin
                 ShowMessageEntry(IsForum);
               end; { if }
           end; { if }
       end; { while }

     {-- Show the online list footer ----------------------------------------}
     if NOT IsForum then
       begin
         if NOT web_ShowHtmlFile('marefoot.htm') then
           ShowError('Unable to display marefoot.htm');
       end; { if }
   end
     else begin
            WriteLn('<HTML> <BODY> <B> <H1>');
            WriteLn('Unable to display file!');
            WriteLn('</H> </B> </BODY> </HTML>');
          end; { else }


   {-- Close the file -------------------------------------------------------}
   FileClose(FileHandle);
   FileDone(FileHandle);
end; { proc. ListMessageAreas }


procedure ListForums;
var TmpNum    : Integer;
    FileHandle: Integer;
    Error     : Integer;
begin
  {-- Assign the file to the proper handle etc ------------------------------}
  {-- (we dont need files.ele here, so we ignore it -------------------------}
  FileHandle := FileAssign(GetPath_MGroupsRa, 0, 3);
  if NOT web_ShowHtmlFile('frm_head.htm') then
    ShowError('Unable to display frm_head.htm');

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
         FileRead(FileHandle, GroupInf);
         Error := FileGetError(FileHandle);

         {-- And display the forum name -------------------------------------}
         if Error = 0 then
           begin
             {-- Make sure this user should be shown ------------------------}
             DoShow := CheckGroupAccess(GroupInf);

             {-- and show the entry -----------------------------------------}
             if GroupInf.Typ = 1 then                                { Forum }
               begin
                 {-- first show the head group header -----------------------}
                 ShowGroupEntry(true, true);

                 {-- Show all areas in this head group ----------------------}
                 ListMessageAreas(true, true, GroupInf.AreaNum);
               end; { if }
           end; { if }
       end; { while }

     {-- Show the online list footer ----------------------------------------}
     if NOT web_ShowHtmlFile('frm_foot.htm') then
       ShowError('Unable to display frm_foot.htm');
   end
     else begin
            WriteLn('<HTML> <BODY> <B> <H1>');
            WriteLn('Unable to display file!');
            WriteLn('</H> </B> </BODY> </HTML>');
          end; { else }


   {-- Close the file -------------------------------------------------------}
   FileClose(FileHandle);
   FileDone(FileHandle);
end; { proc. ListForums }

begin
  {-- we begin by getting the actual logon information, and opening output ---}
  if web_GetFormData('newsubaction') = '' then
    begin
      GetUir;
      ShowHeader;
    end; { if }


  {-- Save the current time --------------------------------------------------}
  timing_Initialize(timing_StartTime);

  {-- Make sure the last login date/time is available in unix format ---------}
  SetLoginUnixTimeStamp;

  {-- Load configuration -----------------------------------------------------}
  LoadWebConfig;

  {-- get the sub action of what we should do --------------------------------}
  { subaction: 0 = message groups }
  {            1 = file groups }
  {            2 = message areas }
  {            3 = file areas }
  {            4 = message areas, no group checking }
  {            5 = file areas, no group checking }
  {            6 = list all forums }
  if web_GetFormData('newsubaction') = '' then
    SubAction := FVal(web_GetFormData('sub-action'))
     else SubAction := FVal(web_GetFormData('newsubaction'));

  {-- Make sure this is not run on an anonymous logon ------------------------}
  if NOT web_IsLoggedOn then
    begin
      if SubAction <> 6 then
        begin
          web_RunErrorScript(4, '');          { Show an "Access denied" error }
          Halt;                                       { Terminate this script }
        end; { if }
    end; { if }


  {-- now get wether we want file or area listing, or perhaps group listing?--}
  case SubAction of
    0 : ListGroups(true);
    1 : ListGroups(false);
    2 : ListMessageAreas(true, false, FVal(EleCodeStr('|F+')));
    3 : ListFileAreas(true);
    4 : ListMessageAreas(false, false, FVal(EleCodeStr('|F+')));
    5 : ListFileAreas(false);
    6 : ListForums; { we only support message forums }
      else ;
  end; { case }

  {-- and stop the timer -----------------------------------------------------}
  timing_Done(timing_Finished);
  timing_ShowTimeFooter(timing_StartTime, timing_Finished);
end.
