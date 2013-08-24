unit CentrStr;
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
** String utilies for EleBBS
**
** Copyright (c) 1996,97 by Maarten Bekers
**
** Created : 17-Sep-1997
** Last update : 01-Nov-1997
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
uses Colors;
{$ENDIF}

Function  Dup (C : Char;T : Byte) : String;       { Duplicate char C T times }
function  Trim (S : String) : String;                  { Same for both sides }
function  TrimLeft (S : String) : String;      { Removes whitespaces on left }
function  TrimRight (S : String) : String;        { Same for trailing spaces }
function  RaduCenter(S: String; Len: Byte): String;
function  NoDoubleSpace(S: String): String;       { Remove all double-spaces }
function  CenterJust (S : String;Len : Byte;full:Boolean) : String;
                                                { Center justify with spaces }
function  CenterJustChar (S : String;Len : Byte;C : Char;full:Boolean):String;
                                                  { Center justify with char }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  Dup (C : Char;T : Byte) : String;       { Duplicate char C T times }
var Tmp   : String;
    Teller: Byte;
begin
 Tmp := '';
 For Teller := 01 to T do
  Tmp := Tmp + C;

 Dup := TMP;
end; { func. Dup }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


function  RaduCenter(S: String; Len: Byte): String;
var DesiredLen: Byte;
    Temp      : String;
begin
  S := Trim(S);

 {$IFDEF WITH_FULL}
   DesiredLen := Byte((Len div 2) - (NoColorLength(S) div 2));
 {$ELSE}
   DesiredLen := Byte((Len div 2) - (Length(S) div 2));
 {$ENDIF}

  Str(DesiredLen, Temp);
  RaduCenter := '`X'+ Temp + ':' + S;
end; { func. RaduCenter }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

Function  CenterJust (S : String;Len : Byte;full:Boolean) : String;
                                                { Center justify with spaces }
var Temp: String;
begin
  Temp := CenterJustChar(S, Len, #32, Full);

  if Full then
    begin
      While (Length(Temp) < Len) AND (Length(Temp) > 0) do Temp := Temp + #32;
      While (Length(Temp) > Len) AND (Length(Temp) > 0) do Delete(Temp, Length(Temp), 01);
    end; { if }

 CenterJust := Temp;
end; { CenterJustChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  CenterJustChar (S : String;Len : Byte;C : Char;full:Boolean):String;
                                                  { Center justify with char }
var DesiredLen: Byte;
begin
{$IFDEF WITH_FULL}
 DesiredLen := Byte((Len div 2) - (NoColorLength(S) div 2));
{$ELSE}
 DesiredLen := Byte((Len div 2) - (Length(S) div 2));
{$ENDIF}

 S := Dup(C, DesiredLen) + S;
 If Full then S := S + Dup(C, DesiredLen);

 CenterJustChar := S;
end; { func. CenterJustChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  TrimLeft (S : String) : String;      { Removes whitespaces on left }
begin
  While (Length(S)>00) AND (S[1]=#32) do Delete(S, 1, 1);
  TrimLeft := S;
end; { func. TrimLeft }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  TrimRight (S : String) : String;        { Same for trailing spaces }
begin
  While (Length(S)>00) AND (S[Length(S)]=#32) do Delete(S, Length(S), 1);
  TrimRight := S;
end; { func. TrimRight }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  Trim (S : String) : String;                  { Same for both sides }
begin
  Trim := TrimLeft(TrimRight(S));
end; { func. Trim }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function NoDoubleSpace(S: String): String;       { Remove all double-spaces }
var Counter: Longint;
    TempStr: String;
begin
  NoDoubleSpace := s;
  TempStr := '';
  Counter := 01;

  while Counter <= Length(s) do
    begin
      TempStr := TempStr + S[Counter];
      if S[Counter] = #32 then
        While (S[Counter + 1] = #32) AND (Counter <= Length(S)) do
         Inc(Counter);

      Inc(Counter);
    end; { while }

  NoDoubleSpace := TempStr;
end; { func. NoDoubleSpace }
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit CentrStr }
