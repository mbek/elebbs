unit ElLog_U;
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
{$I COMPILER.INC}
(*
**
** EleLOG routines for EleBBS
**
** Copyright (c) 1996, 1997 by Maarten Bekers
**
** Created : 13-Nov-1997
** Last update : 13-Nov-1997
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure RaLog (C: Char; S : String);            { make an ELEBBS.LOG entry }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
uses GenDos, Dos, Global, LongStr, FileObj, CfgRec;


procedure RaLog (C: Char; S : String);                 { make an RA.LOG entry }
{ Please be sure never to use the '@' and/or '`' characters, this will make }
{ RaLOG do indirectly controlled filehandling, then it can result in an }
{ endless loop (eg: FILES.RDX does not exist, getfilesrecord reports this in }
{ your RA.LOG, and RaLog needs to lookup FILES.RDX ;-(( }
Const MonthNames:Array[1..12] of String[3] =
                                 ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                  'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
      DayNames    : Array[0..6] of String[3] = ('Sun', 'Mon', 'Tue', 'Wed',
                                                'Thu', 'Fri', 'Sat');

var Log_F     : pFileObj;
    Month     : Word;
    Years     : Word;
    Day       : Word;
    Dow       : Word;
    Hours     : Word;
    Mins      : Word;
    Secs      : Word;
    MSecs     : Word;
    TimeStr   : String;
    CreateStr : String;
begin
  if GlobalCfg^.RaConfig = nil then EXIT;

  Createstr := FullProgName+' v'+VersionID+' system log created.';

  GetDate(Years, Month, Day, Dow);
  GetTime(Hours, Mins, Secs, MSecs);
  TimeStr := LeadingZero(Hours, 2) + ':' + LeadingZero(Mins, 2) + ':' +
             LeadingZero(Secs, 2);

  New(Log_F, Init);
  if Log_F = nil then EXIT;

  Log_F^.Assign(LogFileName);
  Log_F^.FileMode := ReadWriteMode + DenyNone;
  if NOT Log_F^.Open(1) then
    begin
      if NOT Log_F^.Create(1) then
        begin
          Dispose(Log_F, Done);
          EXIT;
        end; { if }

      if GlobalCfg^.RaConfig^.LogStyle = 0 then
        Log_F^.WriteLn('> ' + LeadingZero(Day, 02) + '-' + MonthNames[Month] +
                           ' ' + TimeStr + ' EL' + LeadingZero(LineCfg^.RaNodeNr, 02) + ' ' + CreateStr)
         else Log_F^.WriteLn('> ' + TimeStr + '  ' + CreateStr);
   end; { if }

  if S <> '' then
    if GlobalCfg^.RaConfig^.LogStyle = 0 then
      Log_F^.WriteLn(C + ' ' + LeadingZero(Day, 02) + '-' + MonthNames[Month] +
                     ' ' + TimeStr + ' EL' + LeadingZero(LineCfg^.RaNodeNr, 02) + ' ' + S)
       else Log_F^.WriteLn(C + ' ' + TimeStr + '  ' + S);

  if S = '' then
    begin
      Log_F^.WriteLn('');

      if GlobalCfg^.RaConfig^.LogStyle <> 00 then
        Log_F^.WriteLn('----------  ' + DayNames[Dow] + #32 + LeadingZero(Day, 02) + #32 + MonthNames[Month] + #32 +
                       Copy(Fstr(Years), 3, 2) + ', ' + PidName + RegAddStrShort +' line #' + FStr(LineCfg^.RaNodeNr));
    end; { if }

 Dispose(Log_F, Done);
end; { proc. RaLog }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
