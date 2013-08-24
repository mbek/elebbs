unit RipMisc;
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
** RIP routines for EleBBS
**
** Copyright (c) 1996, 1997 by Maarten Bekers
**
** Created : 01-Jan-1998
** Last update : 01-Jan-1998
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  WordToMega(Num: Word): String;
Function  WordToMega4(Num: Longint): String;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  WordToMega4(Num: Longint): String;
const
  MegaArray : array[0..35] of Char = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';

var TempStr: String[4];
begin
  TempStr := '';

  While Num>00 do
    begin
      TempStr := MegaArray[Num MOD 36] + TempStr;
      Num := Num DIV 36;
    end; { while }

  While Length(TempStr) < 04 do
    TempStr := '0' + TempStr;

  WordToMega4 := TempStr;
end; { func. WordToMega4 }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function WordToMega(Num: Word): String;
const
  MegaArray : array[0..35] of Char = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';

var TempStr: String[2];
begin
  TempStr:= '';

  if (Num < 00) OR (Num>1295) then
    begin
      WordToMega := #32#32;
      EXIT;
    end;

  While Num>00 do
    begin
      TempStr := MegaArray[Num MOD 36] + TempStr;
      Num := Num DIV 36;
    end; { while }

  While Length(TempStr) < 02 do
    TempStr := '0' + TempStr;

  WordToMega := TempStr;
end; { func. WordToMega }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
