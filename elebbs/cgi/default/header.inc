(*
**
** EleWEB EleXer source for CGI header of most scripts
** for all user-defined scripts (action = 3) this script
** is run. As parameter the script name is passed.
**
** Created: 24-may-2001
** Last update: 09-Feb-2003
** Written by: Maarten Bekers
**
*)

function GetExpireDate: String;
const
  SecsPerDay = 86400;

begin
  GetExpireDate := UnixToRfcDate(NowAsUnixDate + (120 * SecsPerDay), 'GMT');
end; { func. GetExpireDate }

procedure OpenOutput;                   { Opens the output for the CGI script }
begin
  {-- we use cookies to maintain state information about this session --------}
  {-- for more information please reference RFC2109. For the node number -----}
  {-- we dont set an expiry date - this means that the cookie will be --------}
  {-- removed as soon as the session closes. ---------------------------------}
  if web_IsLoggedOn then
    begin
      web_SendCookie('web-UIR=' + FStr(web_GetUIR) + '; Comment=EleWEB_rec; ' +
              'Path=/; Version=1; expires=' + GetExpireDate);
      web_SendCookie('web-UIP=' + FStr(web_GetUIP) + '; Comment=EleWEB_pwhsh ' +
              'Path=/; Version=1; expires=' + GetExpireDate);
      web_SendCookie('web-UIN=' + FStr(web_GetUIN) + '; Comment=EleWEB_nodenr; ' +
              'Path=/; Version=1;');
    end; { if }

  {-- make sure the webclient knows our current time so we dont expire too ---}
  {-- early ------------------------------------------------------------------}
  web_SendHeader('Date:', web_GetDateTime);

  {-- and ask the client to not cache us ------------------------------------}
  web_SendHeader('Pragma:', 'no-cache');
  web_SendHeader('Cache-Control:', 'no-cache');

  {-- make sure this cookie will not be cached ------------------------------}
  { for Russian text you could use: web_OpenOutput('text/html; charset=cp866'); }
  web_OpenOutput('text/html;');
end; { proc. OpenOutput }



procedure ShowHeader;
begin
  {-- perhaps they dont want an header at all --------------------------------}
  if web_GetFormData('dontshowhdr') = 'true' then
    Halt;

  {-- unless this is a filedownload, we'll open HTML output here -------------}
  OpenOutput;

  {-- always show a header ---------------------------------------------------}
  if not web_ShowHtmlFile(GetPath_HtmlPath + 'header.htm') then ;
end; { proc. ShowHeader }
