program Logout;

procedure RemoveCookie;
begin
  {-- make sure the webclient knows our current time so we dont expire too --}
  {-- early -----------------------------------------------------------------}
  WriteLn('Date: ', web_GetDateTime);

  {-- we clear the cookies here ----------------------------------------------}
  web_SendCookie('web-UIN=; Comment=User_Logged_Out; ' +
          'Path=/; Version=1');
  web_SendCookie('web-UIR=; Comment=User_Logged_Out; ' +
          'Path=/; Version=1');
  web_SendCookie('web-UIP=; Comment=User_Logged_Out; ' +
          'Path=/; Version=1');

  {-- make sure this cookie will not be cached ------------------------------}
  web_SendHeader('Cache-control:', 'no-cache="set cookie"');
  web_OpenOutput('text/html');
  WriteLn;
end; { proc. RemoveCookie }


begin
  {-- Open up the output -----------------------------------------------------}
  RemoveCookie;

  {-- Redirect to another site (blank page is so ugly) -----------------------}
  WriteLn('<HTML>');
  Writeln(' <BODY>');
  WriteLn('   <META HTTP-EQUIV="refresh" CONTENT="0; URL=http://www.elebbs.com/">');
  WriteLn(' </BODY>');
  WriteLn('</HTML>');
end.
