unit Colors;
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
** Color routines for EleBBS
**
** Copyright (c) 1996-1998 by Maarten Bekers
**
** Created : 01-Jan-1998
** Last update : 01-Jan-1998
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  MakeAttr(Fore, Back: Longint): Byte;
function GetForeAttr(Color: Longint): Byte;
function GetBackAttr(Color: Longint): Byte;
function  NoColorLength(S: String): Byte;
procedure GetColors(Color : Longint; Var BackGr : Byte; Var ForeGr : Byte);
procedure RemoveRaColors(var Str: String);
function GetLastColor(TempStr: String): Byte;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses LongStr;

function NoColorLength(S: String): Byte;
begin
  RemoveRaColors(s);
  NoColorLength := Length(s);
end; { func. NoColorLength }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure RemoveRaColors(var Str: String);
{ Gekopieerd in GENCFG }
var CenterPos : Byte;
    DelLen    : Byte;
    SavePos   : Byte;
begin
  While Pos('`', Str)>00 do
    begin
       Centerpos:= Pos('`', Str);
       Delete(Str, CenterPos, 01);
       While (CenterPos<=Length(Str)) AND (UpCase(Str[CenterPos]) in
        ['0'..'9',',','$','-','+','A','B','C','D','E','F','G','H','L','S','X','Y'])
         do Delete (Str,CenterPos,1);

       If Copy(Str,CenterPos,1)=':' then
        Delete (Str,CenterPos,1);
    end; { While (radu-codes) }

  While Pos(^K,Str)>0 do
    begin
      if Str[Pos(^K, Str) + 01] = '!' then
        begin
          DelLen := 00;
          Savepos := Pos(^K, Str);

          While (DelLen < 15) AND (Str[SavePos] <> '|') do
           begin
             Delete(Str, SavePos, 1);
             Inc(DelLen);

             if Str[SavePos] = '|' then
               Delete(Str, Savepos, 1);
           end;
        end
         else Delete(Str, Pos(^K, Str), 4);
    end; { while }

  While (Pos('#',Str)>0) AND (Length(Str)>0) do
    begin
       Centerpos:=Pos('#',Str);
       Delete(Str, CenterPos, 1);
       While (Str[centerpos] in ['0'..'9']) AND (CenterPos<Length(Str)) do
          Delete (Str,CenterPos,1);
    end; { While }
end; { proc. removeracolors }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function MakeAttr(Fore, Back: Longint): Byte;
begin
  MakeAttr:= Byte((Byte(Back) SHL 4) OR Byte(Fore));
end; { func. MakeAttr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure GetColors(Color: Longint; Var BackGr, ForeGr: Byte);
begin
  BackGr := Byte(Byte(Color) shr 4);
  ForeGr := Byte(Byte(Color) xor (Backgr shl 4));
end; { proc. GetColors }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function GetForeAttr(Color: Longint): Byte;
var Fore, Back: Byte;
begin
  GetColors(Color, Back, Fore);

  GetForeAttr := Fore;
end; { func. GetForeAttr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function GetBackAttr(Color: Longint): Byte;
var Fore, Back: Byte;
begin
  GetColors(Color, Back, Fore);

  GetBackAttr := Back;
end; { func. GetBackAttr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function GetLastColor(TempStr: String): Byte;

procedure ExtractInteger(var Counter: Longint; var TempByte: Byte);
var TempCol: String;
begin
  TempCol := '';

  While (Counter <= Length(TempStr)) AND (TempStr[Counter] <> ':') do
    begin
      Inc(Counter);

      if TempStr[Counter] in ['0'..'9'] then
        TempCol := TempCol + TempStr[Counter];

    end; { while }

  TempByte := FVal(TempCol);
end; { proc. ExtractInteger }

var Counter  : Longint;
    CurColor : Byte;
    Tempbyte : Byte;
begin
  Counter := 01;
  CurColor := 03;

  While Counter <= Length(TempStr) do
    begin
      Case TempStr[Counter] of
        ^K  : begin
                Inc(Counter);

                if TempStr[Counter] = '[' then
                  begin
                    CurColor := Fval('$' + Copy(TempStr, Counter + 1, 2));
                    Inc(Counter, 2);
                  end; { if }


              end; { ^K }
        '`' : begin
                Inc(Counter);

                Case TempStr[Counter] of
                  'A' : ExtractInteger(Counter, CurColor);
                  'F' : begin
                          ExtractInteger(Counter, TempByte);

                          CurColor := (TempByte AND $8F) OR (CurColor AND $70);
                        end; { foreground }
                  'B' : begin
                          ExtractInteger(Counter, TempByte);

                          CurColor := (TempByte SHL 4) OR (CurColor AND $0F);
                        end; { background }
                end; { case }
              end; { if }
      end; { case }

      Inc(Counter);
    end; { while }

  if CurColor = 0 then
    CurColor := 3;
  GetLastColor := CurColor;
end; { func. GetLastColor }


end.
