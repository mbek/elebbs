program Errors;
(*
**
** EleWEB EleXer source.
** Show the error defined by EleWEB
**
** Created: 24-may-2001
** Last update: 24-may-2001
** Written by: Maarten Bekers
**
*)

function ErrorStr: String;
var TmpStr   : String;
    ErrorCode: Integer;
begin
  {-- Begin with the error number -------------------------------------------}
  ErrorCode := FVal(GetParameter(1));
  TmpStr := FStr(ErrorCode);

  {-- Now add a user specific string ----------------------------------------}
  Case ErrorCode of
     01 : TmpStr := TmpStr + ' - unknown user';
     02 : TmpStr := TmpStr + ' - Invalid password';
     03 : TmpStr := TmpStr + ' - Unable to read configuration files';
     04 : TmpStr := TmpStr + ' - Access denied';
     05 : TmpStr := TmpStr + ' - unable to find script';
       else TmpStr := TmpStr + ' - unknown error';
  end; { case }

  {-- add any error specific error if there is any --------------------------}
  if GetParameter(2) <> '' then
    TmpStr := TmpStr + ' - '  + GetParameter(2);

  {-- set the error string --------------------------------------------------}
  ErrorStr := TmpStr;
end; { func. ErrorStr }

begin
  {-- We use a template with only the message string dependent on the -------}
  {-- actual error. You can change this to anything you want including ------}
  {-- scripts that automaticly redirect to another page and the likes -------}
  {-- open the HTML output --------------------------------------------------}
  web_OpenOutput('text/html');


  if NOT web_ShowHtmlFile('error' + GetParameter(1) + '.htm') then
    begin
      WriteLn('<HTML>');
      WriteLn('  <BODY>');
      WriteLn('    <B> <H2>');

      {-- show the error ------------------------------------------------------}
      WriteLn(ErrorStr, '<BR>');
      WriteLn('<BR>');
      WriteLn('Try a new <A HREF="/asklogin.htm">login</A> ..<BR><BR>');

      {-- close the output ---------------------------------------------------}
      WriteLn('    </H2> </B>');
      WriteLn('  </BODY>');
      WriteLn('</HTML>');
    end; { if }
end. { errors }
