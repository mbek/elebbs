(*
**
** Extended user file support routines
**
** Created: 21-Jul-2002
** Last update: 21-Jul-2002
**
** (c) 2002 by Maarten Bekers
**
**
*)


function EncodeName(Name: String): String; { !! copied from usrsup.pas }
var TmpStr : String;
    Counter: Integer;
begin
  for Counter := 01 to Length(Name) do
    begin
      if Pos(SUpCase(Copy(Name, Counter, 1)), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890') > 0 then
        TmpStr := TmpStr + Copy(Name, Counter, 1)
          else begin
                 {-- "escape" other characters ------------------------------}
                 {-- we already escape the _ so that doesnt form a hazard ---}
                 TmpStr := TmpStr + '_' + FStr(Ord(Name[Counter])) + '_';
               end; { else }
    end; { if }

  EncodeName := TmpStr + '.' + fStr(StrCrc(Name));
end; { func. EncodeName }

function GetHomeDirectory(UsrName: String; DoMake: Boolean): String;
var Inf: UsrEleRecord;
begin
  if UsrName = '' then
    begin
      usr_GetExtensions(Inf);
    end
      else begin
             usr_GetExtensions(Inf);
             Inf.Pth := ForceBack(JustPath(Inf.Pth)) + EncodeName(UsrName);
           end; { else }

  {-- assign it --------------------------------------------------------------}
  GetHomeDirectory := Inf.Pth;

  {-- Create the directory if it doesnt exist --------------------------------}
  if DoMake then
    begin
      if NOT FileExist(Inf.Pth + '\') then
        MakeDir(Inf.Pth);
    end; { if }
end; { func. GetHomeDirectory }


procedure ReadUserInfo(HomeDir: String);
var UserHandle: Integer;
    Error     : Integer;
    DataStr   : String;
    TmpStr    : String;
    ValueStr  : String;
begin
  {-- Clear the structure ---------------------------------------------------}
  UsrExtra.MailAddress := '';
  UsrExtra.IcqNumber := '';
  UsrExtra.HomePage := '';
  UsrExtra.Avatar := '';
  UsrExtra.MemberType := '';
  UsrExtra.LastMsgDate := 0;
  UsrExtra.Signature := '';

  {-- Assign the file to the proper handle etc ------------------------------}
  UserHandle := FileAssign(HomeDir + '\userinfo.txt', 0, 3);
  Error := FileGetError(UserHandle);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileOpen(UserHandle);
  Error := FileGetError(UserHandle);

  {-- open the user info ----------------------------------------------------}
  if (Error = 0) then
   begin
     {-- Loop through the whole data file -----------------------------------}
     while (error = 0) do
       begin
         {-- Read the next line ---------------------------------------------}
         FileReadStringLn(UserHandle, DataStr);
         Error := FileGetError(UserHandle);

         {-- Extract the  from ----------------------------------------------}
         TmpStr := Copy(DataStr, 1, Pos('=', DataStr) - 1);
         ValueStr := Trim(Copy(DataStr, Length(TmpStr) + 2, 250));

         {-- and procss the data --------------------------------------------}
         if TmpStr = 'mailaddress' then
           usrExtra.MailAddress := ValueStr;

         if TmpStr = 'icqnumber' then
           usrExtra.Icqnumber := ValueStr;

         if TmpStr = 'homepage' then
           usrExtra.Homepage := ValueStr;

         if TmpStr = 'avatar' then
           usrExtra.Avatar := ValueStr;

         if TmpStr = 'membertype' then
           usrExtra.MemberType := ValueStr;

         if TmpStr = 'lastmsgdate' then
           usrExtra.LastMsgDate := FVal(ValueStr);

         if TmpStr = 'signature' then
           usrExtra.Signature := ValueStr;
       end; { while }
   end; { if }

  {-- Close the file --------------------------------------------------------}
  FileClose(UserHandle);
  FileDone(UserHandle);
end; { proc. ReadUserInfo }



procedure WriteUserInfo(HomeDir: String);
var UserHandle: Integer;
    Error     : Integer;
    DataStr   : String;
    TmpStr    : String;
    ValueStr  : String;

procedure AddLn(TmpStr: String);
begin
  FileWriteStringLn(UserHandle, TmpStr);
end; { proc. AddLn }

begin
  {-- Assign the file to the proper handle etc ------------------------------}
  UserHandle := FileAssign(HomeDir + '\userinfo.txt', 2, 3);
  Error := FileGetError(UserHandle);

  {-- Now open the file -----------------------------------------------------}
  if Error = 0 then
    FileCreate(UserHandle);
  Error := FileGetError(UserHandle);

  {-- open the user info ----------------------------------------------------}
  if (Error = 0) then
   begin
     {-- Write all data -----------------------------------------------------}
     AddLn('mailaddress=' + UsrExtra.MailAddress);
     AddLn('icqnumber=' + UsrExtra.IcqNumber);
     AddLn('homepage=' + UsrExtra.HomePage);
     AddLn('avatar=' + UsrExtra.Avatar);
     AddLn('membertype=' + UsrExtra.MemberType);
     AddLn('lastmsgdate=' + FStr(UsrExtra.LastMsgDate));
     AddLn('signature=' + UsrExtra.Signature);
   end; { if }

  {-- Close the file --------------------------------------------------------}
  FileClose(UserHandle);
  FileDone(UserHandle);
end; { proc. WriteUserInfo }


