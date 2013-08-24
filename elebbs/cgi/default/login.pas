program login;
(*
**
** EleWEB EleXer source for login of EleWEB user
** We automaticly redirect to the "top" script to avoid the user needing
** to keep rePOST-ing its data.
**
** Created: 24-may-2001
** Last update: 24-may-2001
** Written by: Maarten Bekers
**
*)

{-- Web configuration record -------------------------------------------------}
{$I cfgrec.inc}

var
  webCfgInf: webCfgRecord;
  HostName : String;

{-- Include web configuration record functions -------------------------------}
{$i cfgfnc.inc}

{-- Include header routines --------------------------------------------------}
{$I header.inc}

begin
  {-- Load web config --------------------------------------------------------}
  LoadWebConfig;

  {-- set the cookies to show this user has access ---------------------------}
  OpenOutput;

  {-- get the hostname of this server ----------------------------------------}
  Hostname := GetEnvironment('SERVER_NAME');
  Hostname := 'http://' + Hostname + '/cgi-bin/eleweb.exe?action=3&script=readmsg&sub-action=15&areanum=' +
                FStr(webCfgInf.FrontPageNum);

  {-- now display a redirect to a script so that the POST is done as little --}
  {-- as possible ------------------------------------------------------------}
  WriteLn('<HTML>');
  Writeln(' <BODY>');
  WriteLn('   <META HTTP-EQUIV="refresh" CONTENT="0; URL='+Hostname+'">');
  WriteLn(' </BODY>');
  WriteLn('</HTML>');
end. { login }
