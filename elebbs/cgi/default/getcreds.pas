program GetCreds;
(*
**
** EleWEB EleXer source.
** Get the UIN, UIP and UIR fields - specifically used by the logout routine
**
** Created: 09-Feb-2003
** Last update: 09-Feb-2003
** Written by: Maarten Bekers
**
*)

(*
** In this example we simply use cookies to get the data,
** if this is not desirable, or not usable we could also
** get this from POST or GET data. By getting this data
** through a script you can basically do whatever you want
** including bypassing a login altogether.
**
*)

var TmpError: Integer;
begin
  {-- get the actual login data ---------------------------------------------}
  web_SetLoginData(FVal(web_GetCookie('web-UIN')),
                   FVal(web_GetCookie('web-UIP')),
                   FVal(web_GetCookie('web-UIR')),
                   true);                               { Allow silent login }

  {-- and try to access validate --------------------------------------------}
  if web_MatchCredentials(web_GetUir,
                          web_GetUip,
                          web_GetUin,
                          TmpError) then ;
end.
