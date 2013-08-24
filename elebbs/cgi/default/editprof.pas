program EditProf;
(*
**
** EleWEB EleXer source.
** Shows user info for this user, and allow user to edit their info
**
** Created: 28-Jul-2002
** Last update: 08-Aug-2002
** Written by: Maarten Bekers
**
*)

{-- Include standard type definitions ---------------------------------------}
{$I stdrec.inc}

{-- User record data structures ---------------------------------------------}
{$I usrrec.inc}


var FileHandle: Integer;
    Error     : Integer;
    UserInf   : UsersRecord;
    UserRec   : Integer;
    TmpStr    : String;
    TmpDir    : String;
    SubAction : Integer;
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
  HtmlHandle := FileAssign(GetPath_HtmlPath + 'editprof.htm', 0, 3);
  Error := FileGetError(HtmlHandle);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileOpen(HtmlHandle);
  Error := FileGetError(HtmlHandle);

  {-- display an error if we cant find this html-file -----------------------}
  if Error > 0 then
    begin
      ShowError('Unable to find editprof.htm');
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
                       'N' : Replace('|ZN', usrExtra.Avatar, DataStr, false);
                       'O' : begin
                               if usrExtra.IcqNumber <> '' then
                                 begin
                                   Replace('|ZO', '<img src="http://web.icq.com/whitepages/online?icq='+
                                           usrExtra.IcqNumber + '&img=5"></img>', DataStr,
                                           false);
                                 end { if }
                                   else Replace('|ZO', '', DataStr, false);
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

function ValidName(Name: String): Boolean;
begin
  {-- default to true --------------------------------------------------------}
  ValidName := true;

  {-- check the string for html ----------------------------------------------}
  if (Pos('>', Name) <> 0) OR (Pos('<', Name) <> 0) then
    ValidName := false;

  {-- Make sure no UBB is in the username ------------------------------------}
  if (Pos(']', Name) <> 0) OR (Pos('[', Name) <> 0) then
    Validname := false;
end; { func. ValidName }


procedure DoModifyUserEntry;
var TmpPw1    : String;
    TmpPw2    : String;
    TmpError  : Integer;
begin
  {-- Initialize the variables -----------------------------------------------}
  TmpError := 0;

  {-- users can change their password ----------------------------------------}
  TmpPw1 := SUpCase(Trim(web_GetFormData('password1')));
  TmpPw2 := SUpCase(Trim(web_GetFormData('password2')));

  if TmpPw1 <> '' then
    begin
      if TmpPw1 <> TmpPw2 then
        TmpError := 6;

      if Length(TmpPw1) < 5 then
        TmpError := 6;

      if SearchCtlFile('pwdtrash.ctl', TmpPw1, false) then
        TmpError := 6;

      {-- now set the password -----------------------------------------------}
      UserInf.Password := TmpPw1;
      UserInf.PasswordCRC := StrCrc(TmpPw1);
   end; { if }

  {-- Update the location ----------------------------------------------------}
  UserInf.Location := web_GetFormData('location');

  {-- and fill in the extra record -------------------------------------------}
  usrExtra.MailAddress := web_GetFormData('email');
  usrExtra.IcqNumber := web_GetFormData('icqnr');
  usrExtra.HomePage := web_GetFormData('homepage');
  usrExtra.Avatar := web_GetFormData('usericon');

  {-- Make sure no HTML or UBB is in the username ----------------------------}
  if NOT ValidName(Userinf.Name) then
    TmpError := 7;

  {-- Make sure no HTML or UBB is in the homepage ----------------------------}
  if NOT ValidName(usrExtra.HomePage) then
    TmpError := 7;

  {-- Make sure no HTML or UBB is in the avatar ------------------------------}
  if NOT ValidName(usrExtra.Avatar) then
    TmpError := 7;

  {-- Make sure no HTML or UBB is in the ICQ number --------------------------}
  if NOT ValidName(usrExtra.IcqNumber) then
    TmpError := 7;

  {-- Make sure no HTML or UBB is in the mail address ------------------------}
  if NOT ValidName(usrExtra.MailAddress) then
    TmpError := 7;

  {-- Now check if the users password has been entered correctly -------------}
  TmpPw1 := SUpCase(Trim(web_GetFormData('password')));
  if (UserInf.PasswordCrc = StrCrc(TmpPw1)) AND (TmpError = 0) then
    begin
      {-- Update the record --------------------------------------------------}
      FileSeek(FileHandle, UserRec * GetRecSize(UserInf));
      FileWrite(FileHandle, UserInf);
      Error := FileGetError(FileHandle);

      {-- and update the extra record ----------------------------------------}
      TmpDir := GetHomeDirectory(Userinf.Name, true);
      WriteUserInfo(TmpDir);

      {-- and show the user we updated successfully --------------------------}
      if NOT web_ShowHtmlFile('profupd.htm') then ;
    end
      else if not web_ShowHtmlFile('proferr.htm') then ;
end; { proc. DoModifyUserEntry }

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

  {-- first get the current Userinfo fields ----------------------------------}
  UserRec := FVal(EleCodeStr('|F{'));

  {-- Assign the file to the proper handle etc ------------------------------}
  FileHandle := FileAssign(GetPath_MsgBasePath + 'users.bbs', 2, 3);
  Error := FileGetError(FileHandle);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileOpen(FileHandle);
  Error := FileGetError(FileHandle);

  {-- get the sub-action ----------------------------------------------------}
  SubAction := FVal(web_GetFormData('sub-action'));

  {-- and go on a hunt ------------------------------------------------------}
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
      if SubAction <> 1 then
        begin
          ShowUserEntry;
        end
          else begin
                 {-- Actually modify the entry ------------------------------}
                 DoModifyUserEntry;
               end; { else }
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
