unit Curtime;
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
** Current time unit, routines for EleBBS
**
** Copyright (c) 1998-1999 by Maarten Bekers
**
** Created : 05-Jan-1999
** Last update : 06-Feb-1999
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  CurrentTime: LongInt;        { Seconds since midnight of Log-On date }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses GenDos, Dos, Global, ObjDec;

function CurrentTime: LongInt;        { Seconds since midnight of Log-On date }
var Temp    : LongInt;
    Year    : Word;
    Month   : Word;
    Day     : Word;
    DOW     : Word;
    Hour    : Word;
    Min     : Word;
    Sec     : Word;
    MSec    : Word;
begin
  GetDate(Year, Month, Day, Dow);
  GetTime(Hour, Min, Sec, MSec);

  Temp := Longint((Longint(Hour)*3600) + (Longint(Min)*60) + Sec);

  If (LineCfg^.StartTimingDay <> Day) then       { If Curr. date doesn't match logondate }
     begin
       {$IFDEF WITH_FULL}
         contrObj^.SetStartTime;
       {$ENDIF}

       Temp := 00;
 {     Inc(Temp, 86400);                  then increase by one day (24*60*60) }
     end; { if }

  CurrentTime := TEMP;
end; { func. CurrentTime }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
