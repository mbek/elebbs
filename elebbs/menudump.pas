program MenuDump;
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
** MENUDUMP, RemoteAccess v2.50 menudump program
**
** Copyright (c) 1996,97,98 by Maarten Bekers
**
** Created : 01-Feb-1997
** Last update : 01-Feb-1997
**
**
*)

Uses Dos,
      Crt,
       CfgRec,
        Use32,
         FileObj;

var Menu_F : pFileObj;
    MnuInf : MnuRecord;

begin
  WriteLn('MenuDump.EXE, (c)1997 - 1999 by Maarten Bekers.');
  WriteLn('Maarten Bekers. All rights reserved.');
  Writeln;

  If ParamCount<1 then
    begin
      WriteLn('Usage: MENUDUMP C:\RA\MENUS\MAIN.MNU');
      Halt;
    end; { Not enough parameters }

  New(Menu_F, Init);
  Menu_F^.Assign(ParamStr(1));
  Menu_F^.Open(1);

  Writeln('General information:');
  Writeln('Full Menu Name   : ', ParamStr(1));
  WriteLn('Number of items  : ', Menu_F^.FileSize div SizeOf(mnuRecord));
  WriteLn('FileSize         : ', Menu_F^.FileSize);
  If ReadKey=#00 then ReadKey;


  While NOT Menu_F^.EOF do
      begin
         Fillchar(MnuInf, SizeOf(mnuInf), #00);

         Menu_F^.BlkRead(MnuInf, SizeOf(mnuRecord));

         Writeln('Menu item [', Menu_F^.FilePos div SizeOf(mnuRecord), '] ...');
         Writeln('Typ               : ', MnuInf.Typ);
         Writeln('Security          : ', MnuInf.Security);
         Writeln('MaxSec            : ', MnuInf.MaxSec);
         Writeln('Not FlagsMask     : <Not Displayed>');
         Writeln('Flags Mask        : <Not Displayed>');
         WriteLn('TimeLeft          : ', MnuInf.TimeLeft);
         WriteLn('TimeUsed          : ', MnuInf.TimeUsed);
         WriteLn('Age               : ', MnuInf.Age);
         WriteLn('TermAttrib        : ', MnuInf.TermAttrib);
         WriteLn('MinSpeed          : ', MnuInf.MinSpeed);
         WriteLn('MaxSpeed          : ', MnuInf.MaxSpeed);
         WriteLn('Credit            : ', MnuInf.Credit);
         WriteLn('OptionCost        : ', MnuInf.OptionCost);
         WriteLn('PerMinCost        : ', MnuInf.PerMinCost);
         WriteLn('Node              : <Not Displayed>');
         WriteLn('Group             : <Not Displayed>');
         WriteLn('StartTime(s)      : <Not Displayed>');
         WriteLn('Stop Time(s) [01] : ', mnuInf.StopTime[01]);
         WriteLn('Display           : ', MnuInf.Display);
         WriteLn('HotKey            : "', MnuInf.HotKey, '"');
         WriteLn('MiscData          : ', MnuInf.MiscData);
         WriteLn('ForeGround        : ', MnuInf.ForeGround);
         WriteLn('BackGround        : ', MnuInf.BackGround);
         Writeln('-------------------------------------------------------------');
         If ReadKey=#00 then Readkey;

      end; { while }

  Dispose(Menu_F, Done);
end.
