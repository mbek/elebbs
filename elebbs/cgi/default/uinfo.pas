program uinfo;
(*
**
** EleWEB EleXer source.
** Shows user info for user
**
** Created: 7-Jul-2002
** Last update: 08-Aug-2002
** Written by: Maarten Bekers
**
*)


{-- Include standard type definitions ---------------------------------------}
{$I stdrec.inc}

{-- User structures data type -----------------------------------------------}
{$I usrrec.inc}


var FileHandle: Integer;
    Error     : Integer;
    UserInf   : UsersRecord;
    UserRec   : Integer;
    TmpStr    : String;
    TmpDir    : String;
    usrExtra  : usrExtraInfo;


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


{-- Include user extension routines ---------------------------------------}
{$i usrfunc.inc}

procedure ShowUserEntry;
var HtmlHandle: Integer;
    Error     : Integer;
    Count     : Integer;
    TmpInt    : Integer;
    TmpChar   : Char;
    DataStr   : String;
    DoShow    : Boolean;
begin
  {-- Assign the file to the proper handle etc ------------------------------}
  HtmlHandle := FileAssign(GetPath_HtmlPath + 'uinfdata.htm', 0, 3);
  Error := FileGetError(HtmlHandle);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileOpen(HtmlHandle);
  Error := FileGetError(HtmlHandle);

  {-- display an error if we cant find this html-file -----------------------}
  if Error > 0 then
    begin
      ShowError('Unable to find uinfdata.htm');
    end; { if }

  if Error = 0 then
   begin
     {-- Show the list-user header ------------------------------------------}
     while (error = 0) do
       begin
         {-- Now loop through the whole file --------------------------------}
         FileReadStringLn(HtmlHandle, DataStr);
         Error := FileGetError(HtmlHandle);

         {-- Is it allowed to show this user --------------------------------}
         if Error = 0 then
           begin
             DoShow := TRUE;

             if ReadBit(UserInf.Attribute2, 3) then   { Hidden from userlist }
               DoShow := FALSE;
           end
             else DoShow := FALSE;

         {-- can we show this user? -----------------------------------------}
         if DoShow then
           begin
             {-- now convert all "internal" codes to the data we have -------}
             Count := 0;
             TmpInt := Pos('|Z', DataStr);

             while (TmpInt > 0) AND (Count < 20) AND (Error = 0) do
               begin
                 if ((TmpInt + 1) < Length(DataStr)) then
                   begin
                     TmpChar := Copy(DataStr, TmpInt + 2, 1);

                     Case TmpChar of
                       'A' : Replace('|ZA', UserInf.Name, DataStr, false);
                       'B' : Replace('|ZB', UserInf.Location, DataStr, false);
                       'C' : Replace('|ZC', UserInf.Organisation, DataStr, false);
                       'D' : Replace('|ZD', UserInf.Address1, DataStr, false);
                       'E' : Replace('|ZE', UserInf.Address2, DataStr, false);
                       'F' : Replace('|ZF', UserInf.Address3, DataStr, false);
                       'G' : Replace('|ZG', UserInf.Handle, DataStr, false);
                       'H' : Replace('|ZH', UserInf.LastTime, DataStr, false);
                       'I' : Replace('|ZI', UserInf.LastDate, DataStr, false);
                       'J' : Replace('|ZJ', UserInf.BirthDate, DataStr, false);
                       'K' : Replace('|ZK', usrExtra.MailAddress, DataStr, false);
                       'L' : Replace('|ZL', usrExtra.IcqNumber, DataStr, false);
                       'M' : begin
                               if usrExtra.HomePage <> '' then
                                 Replace('|ZM', usrExtra.HomePage, DataStr, false)
                                   else Replace('|ZM', '', DataStr, false);
                             end; { if }
                       'N' : begin
                               if usrExtra.Avatar = '' then
                                 Replace('|ZN', '(none)', DataStr, false)
                                   else Replace('|ZN', '<IMG SRC="'+ usrExtra.Avatar + '"></IMG>', DataStr,
                                                false);
                             end; { "N" }
                       'O' : begin
                               if usrExtra.IcqNumber <> '' then
                                 begin
                                   Replace('|ZO', '<img src="http://web.icq.com/whitepages/online?icq='+
                                           usrExtra.IcqNumber + '&img=5"></img>', DataStr,
                                           false);
                                 end { if }
                                   else Replace('|ZO', '', DataStr,
                                                false);
                             end; { if }
                         {-- and you can add more ;-) }
                         else Replace('|Z' + TmpChar, '[Unknown macro]', DataStr, false);
                     end; { case }
                   end; { case }

                 Count := Count + 1;
                 TmpInt := Pos('|Z', DataStr);
               end; { while }

             {-- Convert any EleBBS codes to user data ----------------------}
             DataStr := EleCodeStr(DataStr);

             {-- and display it to the user ---------------------------------}
             WriteLn(DataStr);
           end; { DoShow }

       end; { while }
   end; { if }

  {-- Close the file --------------------------------------------------------}
  FileClose(HtmlHandle);
  FileDone(HtmlHandle);
end; { proc. ShowUserEntry }


begin
  {-- we begin by getting the actual logon information, and opening output ---}
  GetUir;
  ShowHeader;

  {-- Make sure this is not run on an anonymous logon ------------------------}
  if NOT web_IsLoggedOn then
    begin
      web_RunErrorScript(4, '');              { Show an "Access denied" error }
      Halt;                                           { Terminate this script }
    end; { if }

  {-- Search this user ------------------------------------------------------}
  if web_GetFormData('uname') = '' then
    begin
      if web_GetFormData('urec') <> '' then
        UserRec := FVal(web_GetFormData('urec'))
         else UserRec := -1;
    end
      else begin
             TmpStr := web_GetFormData('uname');
             UserRec := SearchUser(TmpStr);
           end; { else }

  {-- Assign the file to the proper handle etc ------------------------------}
  FileHandle := FileAssign(GetPath_MsgBasePath + 'users.bbs', 0, 3);
  Error := FileGetError(FileHandle);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileOpen(FileHandle);
  Error := FileGetError(FileHandle);

  if (Error = 0) AND (UserRec >=0) then
    begin
      {-- Get the next record -----------------------------------------------}
      FileSeek(FileHandle, UserRec * GetRecSize(UserInf));
      FileRead(FileHandle, UserInf);
      Error := FileGetError(FileHandle);

      {-- get the extended user info ----------------------------------------}
      TmpDir := GetHomeDirectory(Userinf.Name, true);
      ReadUserInfo(TmpDir);

      {-- And display the users name ----------------------------------------}
      ShowUserEntry;
    end
      else begin
             if NOT web_ShowHtmlFile('uinferr.htm') then
               begin
                 WriteLn('<HTML> <BODY> <B> <H1>');
                 WriteLn('Unable to display file!');
                 WriteLn('</H> </B> </BODY> </HTML>');
               end; { if }
           end; { else }

   {-- Close the file -------------------------------------------------------}
   FileClose(FileHandle);
   FileDone(FileHandle);
end.
