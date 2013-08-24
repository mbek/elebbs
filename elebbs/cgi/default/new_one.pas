program new_one;
(*
**
** EleWEB EleXer source for newuser login for an EleWEB/EleBBS user
**
** Created: 11-jun-2001
** Last update: 11-jun-2001
** Written by: Maarten Bekers
**
*)

{-- Include standard type definitions ----------------------------------------}
{$I stdrec.inc}

{-- Include header and getuir routines ---------------------------------------}
{$I header.inc}
{$i getuir.inc}

begin
  {-- we begin by getting the actual logon information, and opening output ---}
  GetUir;

  {-- open the output --------------------------------------------------------}
  web_OpenOutput('text/html;');

  {-- we can do all sorts of fun stuff here, but we must remember that all ---}
  {-- data of user accounts and stuff is unavailable because no user is ------}
  {-- logged on. For now we restrict ourselves to just displaying a html file-}
  WriteLn;

  {-- and now show the input form --------------------------------------------}
  if NOT web_ShowHtmlFile('newuser.htm') then
    begin
      WriteLn('<HTML> <BODY> <B> <H1>');
      WriteLn('Unable to display file!');
      WriteLn('</H> </B> </BODY> </HTML>');
    end; { if }
end. { new_one }
