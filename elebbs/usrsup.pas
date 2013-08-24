program USRSUP;
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
(*
**
** EleXer source for adding "user" defined scripts to the userbase
** This is an example, you can extend this or you can use the oppurtunities
** this script will offer you.
**
** Created: 31-Jun-2001
** Last update: 31-Jun-2001
** Written by: Maarten Bekers
**
*)

const
  HomeDirectory = '\ele\home\';

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

procedure usr_GetDefaults;
var Uname: String;
begin
  {-- we retrieve the username here. Because this script can be called by --}
  {-- both EleWEB and EleBBS we are not sure which usercode to use so we ---}
  {-- test it out ----------------------------------------------------------}
  Uname := EleCodeStr('|FA');
  if UName = '|FA' then
    UName := EleCodeStr(#6 + 'A');

  usr_Data.Pth := HomeDirectory + HexLong(StrCrc(UName));
  usr_SetExtensions(usr_Data);
end; { proc. usr_GetDefaults }


begin
  {-- EleUSER passes the parameters in the following order: ------------------}
  {-- 1 - record number ------------------------------------------------------}
  {-- 2 - action -------------------------------------------------------------}
  usr_RecNr := FVal(GetParameter(1));
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
