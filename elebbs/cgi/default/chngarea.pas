program ChngArea;
(*
**
** EleWEB EleXer source.
** Program to actually change groups/areas
**
** Created: 24-may-2001
** Last update: 08-Aug-2002
** Written by: Maarten Bekers
**
*)

{-- Include standard type definitions ----------------------------------------}
{$I stdrec.inc}

{-- Include user record file -------------------------------------------------}
{$I usrrec.inc}


var UserInf   : UsersRecord;
    UserExt   : usrEleRecord;
    SubAction : Integer;

{-- Include header and getuir routines ---------------------------------------}
{$I header.inc}
{$i getuir.inc}

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
  GetUserRecord(UserInf, UserExt);

  {-- get the sub action of what we should do --------------------------------}
  { subaction: 0 = message groups to change }
  {            1 = file groups to change }
  {            2 = message areas to change }
  {            3 = file areas to change }
  SubAction := FVal(web_GetFormData('sub-action'));

  {-- now get wether we want file or area listing, or perhaps group listing?--}
  case SubAction of
    0 : UserInf.MsgGroup := FVal(web_GetFormData('value'));
    1 : UserInf.FileGroup := FVal(web_GetFormData('value'));
    2 : UserInf.MsgArea := FVal(web_GetFormData('value'));
    3 : UserInf.FileArea := FVal(web_GetFormData('value'));
      else ;
  end; { case }

  {-- now update the new values ----------------------------------------------}
  SetUserRecord(UserInf, UserExt);

  {-- and write them back to disk --------------------------------------------}
  UpdateUserRecord;

  {-- and now run the script we want the user to be redirected to ------------}
  if NOT web_RunScript(web_GetFormData('new-script'), '') then
    begin
      WriteLn;
      WriteLn;
      WriteLn('<HTML> <BODY> <B> <H1>');
      WriteLn('Unable to run script: ', web_GetFormData('new-script'));
      WriteLn('</H> </B> </BODY> </HTML>');
    end; { if }
end.
