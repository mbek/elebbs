program footer;
(*
**
** EleWEB EleXer source for CGI footer of most scripts
** for all user-defined scripts (action = 3) this script
** is run. As parameter the script name is passed.
**
** Created: 24-may-2001
** Last update: 24-may-2001
** Written by: Maarten Bekers
**
*)

begin
 if SupCase(Trim(GetParameter(1))) <> 'FILEDN' then
    WriteLn('<!--- EleWEB --->');
end. { footer }
