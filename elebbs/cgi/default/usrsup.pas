program USRSUP;
(*
**
** EleXer source for adding "user" defined scripts to the userbase
** This is an example, you can extend this or you can use the opportunities
** this script will offer you.
**
** Created: 31-Jun-2001
** Last update: 21-Jul-2002
** Written by: Maarten Bekers
**
*)

const
  HomeDirectory = 'C:\bbs\ele\home\';

type
  UsrEleRecord = record
                   Pth: String[250];
                 end; { record }

{ EleUSER and EleBBS relie on this script to have several seperate tasks }
{ it will communicate with this script by passing it an parameter and }
{ quering the ReturnValue after running the script. }
{ In EleUSER and EleBBS we set aside a 1024 bytes buffer for you to use }
{ EleBBS and EleUSER take care themselves for the actual writing to disk }
{ we just supply the data to write to disk }

{ our extension file consists of basically one string that points at the }
{ users home directory. Specific information is kept in this directory }

{ please note that both EleBBS and EleUSER caches the size information }
{ so *never* change the recordsize while running EleBBS and without deleting }
{ (or migrating) the old users.ele file }
{ if you want to migrate the usersele.bbs file to a larger size, you need to }
{ write a migration script yourself. This should not be too difficult }

var usr_Action: Integer;
    usr_RecNr : Integer;
    usr_Data  : usrEleRecord;

function usr_GetRecordSize: Integer;
begin
  usr_GetRecordSize := 251; { add one for the length identifier }
end; { func. usr_GetRecordSize }

function usr_AllowDelete: Boolean;
begin
  {-- we have (for now) no special reason not to delete this user --}
  usr_AllowDelete := true;
end; { func. usr_AllowDelete }

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


procedure usr_GetDefaults;
var Uname: String;
begin
  {-- we retrieve the username here. Because this script can be called by --}
  {-- both EleWEB and EleBBS we are not sure which usercode to use so we ---}
  {-- test it out ----------------------------------------------------------}
  UName := EleCodeStr(#6 + 'A');
  if UName = #6 + 'A' then
    Uname := EleCodeStr('|FA');

  usr_Data.Pth := HomeDirectory + EncodeName(UName);
  usr_SetExtensions(usr_Data);
end; { proc. usr_GetDefaults }


begin
  {-- EleUSER passes the parameters in the following order: ------------------}
  {-- 1 - record number ------------------------------------------------------}
  {-- 2 - action -------------------------------------------------------------}
  usr_RecNr := FVal(GetParameter(2));
  usr_Action := FVal(GetParameter(2));

  {-- we rely on EleUSER and EleBBS to pass correct, usable information ------}
  {-- hence we do not validate this. A bit tricky. ---------------------------}
  Case usr_Action of
    0 : SetReturnValue(FStr(usr_GetRecordSize));
    1 : if usr_AllowDelete then SetReturnValue('TRUE')
          else SetReturnValue('FALSE');
    2 : usr_GetDefaults;
      else ;
  end; { case }

end. { usrsup }
