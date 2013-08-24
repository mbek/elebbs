unit Ranges;
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
** Ranges/Min/Max Routines for EleBBS
**
** Copyright (c) 1998-1999 by Maarten Bekers
**
** Created : 24-Jan-1999
** Last update : 24-Jan-1999
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  InRange(nr: LongInt; Start, Stop:Longint): Boolean;
function  Min(X, Y: LongInt): LongInt;
function  Max(X, Y: Longint): LongInt;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function InRange(Nr: LongInt; Start, Stop:LongInt): Boolean;
var Temp: Longint;
begin
  if Start > Stop then
    begin
      Temp := Start;
      Start := Stop;
      Stop := Temp;
    end; { if }

  InRange := true;
  if Nr < Start then InRange := false;
  if Nr > Stop then InRange := false;
end; { func. inRange }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function Min(X, Y: LongInt): LongInt;
begin
  if X < Y then Min := X else Min := Y;
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Max(X, Y: LongInt): LongInt;
begin
  if X > Y then Max := X else Max := Y;
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { ranges }
