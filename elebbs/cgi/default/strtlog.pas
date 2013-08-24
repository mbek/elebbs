program StrtLog;
(*
**
** EleWEB EleXer source.
** Start the login, we check for cookies, and if they exist,
** we use that to try a login. If they dont exist, we setup a
** new login.
**
** Created: 14-Jul-2002
** Last update: 14-Jul-2002
** Written by: Maarten Bekers
**
*)

{-- Include header and getuir routines ---------------------------------------}
{$I header.inc}
{$i getuir.inc}

begin
  {-- we begin by getting the actual logon information, and opening output ---}
  GetUir;
  ShowHeader;

  if (web_GetCookie('web-UIP') <> '') AND (web_getCookie('web-UIR') <> '') then
    begin
      Write('<META HTTP-EQUIV="refresh" CONTENT="0; URL=/cgi-bin/eleweb.exe?action=1&ele_passwordcrc=');
      Write(web_GetCookie('web-UIP'), '&ele_userrec=', web_GetCookie('web-UIR'), '">');
    end
      else begin
             if NOT web_ShowHtmlFile('asklogin.htm') then
               begin
                 WriteLn('Unable to display asklogin.htm');
               end; { if }
           end; { else }
end. { strtlog }
