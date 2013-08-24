program new_two;
(*
**
** EleWEB EleXer source for newuser login for an EleWEB/EleBBS user
**
** Created: 11-jun-2001
** Last update: 11-jun-2001
** Written by: Maarten Bekers
**
*)

{-- Include standard type definitions ---------------------------------------}
{$I stdrec.inc}

{-- User record data structures ---------------------------------------------}
{$I usrrec.inc}



{ TmpError: }
{ 1 - Duplicate username }
{ 2 - Duplicate handle }
{ 3 - user in trashcan.ctl }
{ 4 - missing field }
{ 5 - invalid birthdate }
{ 6 - Invalid password }

var UserInf   : UsersRecord;
    UserExt   : UsrEleRecord;
    TmpError  : Longint;
    TmpBool   : Boolean;
    TmpDay,
    TmpMonth,
    TmpYear   : String;
    TmpPw1    : String;
    TmpPw2    : String;
    usrExtra  : usrExtraInfo;

{-- Include header and getuir routines ---------------------------------------}
{$I header.inc}
{$i getuir.inc}

{-- Include user extension routines ---------------------------------------}
{$i usrfunc.inc}

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

begin
  {-- we begin by getting the actual logon information, and opening output ---}
  GetUir;

  {-- as HEADER.PAS terminates because we arent validated yet, we will -------}
  {-- open output ourselves --------------------------------------------------}
  web_OpenOutput('text/html');
  WriteLn;

  {-- for the adding of new users, we are completely responsible for ---------}
  {-- ourselves. We have to check a lot of data, and put it into the userinfo-}
  {-- field. Then we call the AddUserToBase() and the user is added ----------}
  {-- let's first retrieve the fields we want --------------------------------}

  {-- Set up variables -------------------------------------------------------}
  TmpError := 0;

  {-- first set the userrecord to the EleBBS defaults ------------------------}
  SetNewUserDefaults(UserInf, UserExt);

  {-- now get the data from the POST -----------------------------------------}
  UserInf.Name := web_GetFormData('username');
  UserInf.Handle := web_GetFormData('handle');
  UserInf.Location := web_GetFormData('location');

  {-- if no handle is filled in, handle == username --------------------------}
  if UserInf.Handle = '' then
    UserInf.Handle := UserInf.Name;

  {-- now make sure that data was filled in ----------------------------------}
  if (UserInf.Name = '') OR (UserInf.Location = '') then
    begin
      TmpError := 4;
    end; { if }

  {-- now get the users birthdate --------------------------------------------}
  TmpDay := web_GetFormData('bd_day');
  TmpMonth := web_GetFormData('bd_month');
  TmpYear := web_GetFormData('bd_year');

  if (Length(TmpDay) < 2) then
    TmpDay := '0' + TmpDay;

  if (Length(TmpMonth) < 2) then
    TmpMonth := '0' + TmpMonth;

  if (Length(TmpYear) < 4) then       { we assume 1900 cause its a birthdate }
    TmpMonth := '19' + TmpMonth;

  {-- the check for days could be improved cause its now possible to have a --}
  {-- 30th February, for example.. -------------------------------------------}
  if (FVal(TmpDay) > 31) OR (FVal(TmpDay) < 1) then TmpError := 5;
  if (FVal(TmpMonth) > 12) OR (FVal(TmpMonth) < 1) then TmpError := 5;
  if (FVal(TmpYear) > GetYear) then TmpError := 5;
  UserInf.BirthDate := TmpMonth + '-' + TmpDay + '-' + Copy(TmpYear, 3, 2);

  {-- now check for the entered password -------------------------------------}
  {-- passwords are case insensitive -----------------------------------------}
  TmpPw1 := SUpCase(Trim(web_GetFormData('password1')));
  TmpPw2 := SUpCase(Trim(web_GetFormData('password2')));

  if TmpPw1 <> TmpPw2 then
    TmpError := 6;

  if Length(TmpPw1) < 5 then
    TmpError := 6;

  if SearchCtlFile('pwdtrash.ctl', TmpPw1, false) then
    TmpError := 6;

  {-- now set the password ---------------------------------------------------}
  UserInf.Password := TmpPw1;
  UserInf.PasswordCRC := StrCrc(TmpPw1);

  {-- handle the users' sex --------------------------------------------------}
  if web_GetFormData('sex') = '2' then
    UserInf.Sex := 2 { female }
     else UserInf.Sex := 1; { male }

  {-- and fill in the extra record -------------------------------------------}
  usrExtra.MailAddress := web_GetFormData('email');
  usrExtra.IcqNumber := web_GetFormData('icqnr');
  usrExtra.HomePage := web_GetFormData('homepage');
  usrExtra.Avatar := web_GetFormData('usericon');
  usrExtra.LastMsgDate := NowAsUnixDate;

  {-- and fill the rest with our defaults ------------------------------------}
  UserInf.Address1 := 'Signed on using EleWEB';
  UserInf.ScreenLength := 24;
  UserInf.DataPhone := '';
  UserInf.VoicePhone := '';
  UserInf.DateFormat := 8;                                      { DD-Mmm-YYYY }

  {-- set up some defaults for EleBBS - ignored by EleWEB --------------------}
  SetBit(Userinf.Attribute, 3);                                        { ANSi }
  ClearBit(Userinf.Attribute2, 1);                                { No avatar }
  SetBit(Userinf.Attribute2, 0);                                    { Hotkeys }
  SetBit(UserInf.Attribute2, 2);                      { Fullscreen msg-reader }
  SetBit(UserInf.Attribute, 2);                              { More prompting }
  SetBit(UserInf.Attribute, 1);                                { Clear screen }

  {-- Make sure username is uppercased ---------------------------------------}
  Userinf.Name := FixUserName( NoDoubleSpace (Userinf.Name) );

  {-- Make sure the location is uppercased -----------------------------------}
  UserInf.Location := FixUserName ( NoDoubleSpace (UserInf.Location) );

  {-- Make sure the user is not duplicate ------------------------------------}
  if SearchUser(UserInf.Name) >= 0 then
    TmpError := 1;

  {-- Make sure the handle is not duplicate ----------------------------------}
  if SearchUser(UserInf.Handle) >= 0 then
    TmpError := 2;

  {-- Make sure no HTML or UBB is in the username ----------------------------}
  if NOT ValidName(Userinf.Name) then
    TmpError := 3;

  {-- Make sure no HTML or UBB is in the homepage ----------------------------}
  if NOT ValidName(usrExtra.HomePage) then
    TmpError := 3;

  {-- Make sure no HTML or UBB is in the avatar ------------------------------}
  if NOT ValidName(usrExtra.Avatar) then
    TmpError := 3;

  {-- Make sure no HTML or UBB is in the ICQ number --------------------------}
  if NOT ValidName(usrExtra.IcqNumber) then
    TmpError := 3;

  {-- Make sure no HTML or UBB is in the mail address ------------------------}
  if NOT ValidName(usrExtra.MailAddress) then
    TmpError := 3;

  {-- Make sure this user isnt in the trashcan -------------------------------}
  if SearchCtlFile('trashcan.ctl', UserInf.Name, false) then
    TmpError := 3;

  {-- if an error occurred, show this error ----------------------------------}
  if TmpError > 0 then
    begin
    writeln('error = ', tmperror);
      TmpBool := web_ShowHtmlFile('newerr' + FStr(TmpError) + '.htm');

      if NOT TmpBool then
        begin
          WriteLn('<HTML> <BODY> <B> <H1>');
          WriteLn('Unable to display file! <BR> <BR>');
          WriteLn('</H> </B> </BODY> </HTML>');
        end; { if }
    end; { if }

  {-- now add the user -------------------------------------------------------}
  if TmpError = 0 then
    begin
      {-- actually add the user ----------------------------------------------}
      SetUserRecord(UserInf, UserExt);

      {-- get an user extension record ---------------------------------------}
      usr_ExtSetDefaults(UserExt);

      {-- actually add the user ----------------------------------------------}
      SetUserRecord(UserInf, UserExt);      { and update the extension record }

      {-- and actually add the user ------------------------------------------}
      AddToUserBase;

      {-- Make sure the path exists ------------------------------------------}
      UserExt.Pth := GetHomeDirectory(Userinf.Name, true);

      {-- and set the user extra record --------------------------------------}
      WriteUserInfo(UserExt.Pth);

      {-- now show the user we succesfully added him or her ------------------}
      TmpBool := web_ShowHtmlFile('newwelc.htm');

      if NOT TmpBool then
        begin
          WriteLn('<HTML> <BODY> <B> <H1>');
          WriteLn('Unable to display file!');
          WriteLn('</H> </B> </BODY> </HTML>');
        end; { if }
    end; { if }

end. { new_one }
