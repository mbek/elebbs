unit LongStr;
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
** Integer / String routines for EleBBS
**
** Copyright (c) 1996,97 by Maarten Bekers
**
** Created : 27-Mar-1998
** Last update : 27-Mar-1998
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses CfgRec;

Function  FStr (N : LongInt) : String;           { Convert integer to string }
{$IFDEF MSDOS}
Function  LeadingZero(L: Longint; Len: Longint): String;  { Adds Zeros in front }
{$ELSE}
function  LeadingZero(L: Longint; Len: Longint): ShortString;
{$ENDIF}
Function  FVal (S : String) : LongInt;           { Convert string to Integer }
Function  StrToReal(Str:string): Real;
Function  Real2Str(R : Real; Width : Byte; Places : ShortInt): String;
Function  FormatMinSec(TotalSecs: LongInt) : String;
Function  FixBaud(L: Longint): Longint;
Function  Word2Time(W: Word): String;
function  FValI(const S: String; D: Word): Longint;
function  CommaStr(L: Longint): String;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF MSDOS}
uses Dos;
{$ENDIF}

{$IFDEF OS2}
uses Dos, SysUtils;
{$ENDIF}

{$IFDEF ELEUNIX}
uses Dos, SysUtils;
{$ENDIF}

{$IFDEF WIN32}
uses SysUtils;
{$ENDIF}

function StrToReal(Str:string): Real;     { Converts a string to a real value }
{$IFNDEF WIN32}
var Code : Integer;
{$ELSE}
var Code : Longint;
{$ENDIF}
    Temp : Real;
begin
  If Length(Str)=00 then
    StrToReal := 00
     else begin
            If Copy(Str, 1, 1)='.' then
              Str := '0' + Str;

            If (Copy(Str, 1, 1)='-') AND
                (Copy(Str, 2, 1)='.') then
                 Insert('0', Str, 2);   { A negative value, makes of a -.1, a -0.1 }

            If Str[Length(Str)] = '.' then
              Delete(Str, Length(Str), 01);     { Remove if lastchar is a dot }

            Val(Str, Temp, Code);

            If Code=00 then StrToReal := Temp
              else StrToReal := 00;
          end; { else }
end; { func. StrToReal }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Real2Str(R : Real; Width : Byte; Places : ShortInt): String;
var S: String;
begin
  Str(R:Width:Places, S);
  Real2Str := S;
end; { func. Real2Str }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Word2Time(W: Word): String;
var Hours: Word;
begin
  Hours := 00;

  While W >= 60 do
   begin
      Inc(Hours);
      Dec(W, 60);
   end; { While }

  Word2Time := LeadingZero(Hours, 2) + ':' + LeadingZero(W, 2);
end; { func. Word2Time }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function FormatMinSec(TotalSecs: LongInt) : String;
var Min, Sec : LongInt;
    S        : String;
begin
  Min := TotalSecs div 60;
  Sec := TotalSecs mod 60;
  Str(Sec:2, S);

  If S[1] = ' ' then S[1] := '0';
  FormatMinSec := FStr(Min) + ':' + S;
end; { func. FormatMinSec }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF MSDOS}
Function LeadingZero(L: Longint; Len: Longint): String;
{$ELSE}
Function LeadingZero(L: Longint; Len: Longint): ShortString;
{$ENDIF}
{$IFDEF MSDOS}
var Tmp: String;
{$ELSE}
var Tmp: ShortString;
{$ENDIF}
begin
  Str(L, Tmp);
  while Length(Tmp) < Len do
    Tmp := '0' + Tmp;

  LeadingZero := Tmp;
end; { func. LeadingZero }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  FVal (S : String) : LongInt;            { Convert string to Integer }
{$IFNDEF WIN32}
var Code  : Integer;
{$ELSE}
var Code  : LongInt;
{$ENDIF}
    Number: LongInt;
begin
  While (Length(S) > 0) AND (S[1] = #32) do Delete(S, 1, 1);
  While (Length(S) > 0) AND (S[1] = #32) do Delete(S, Length(S), 1);

  Val (S,Number,Code);
  If Code<>0 then
    Number:=0;
  FVal:=Number;
end; { func. FVal }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  FStr (N : LongInt) : String;           { Convert integer to string }
var Temp: String;
begin
  Str(n,temp);
  FStr:=Temp;
end; { func. FStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  FixBaud(L: Longint): Longint;
begin
  if L=11520 then
    FixBaud := 115200
     else FixBaud := L;
end; { func. FixBaud }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CommaStr(L: Longint): String;
Const CommaChar : Char = '.';

{$IFDEF MSDOS}
type CountrySpec = record
                      DateFormat : Word;
                      Currency   : Array[0..4] of Char;
                      Thousands  : Array[0..1] of Char;
                      Decimal    : Array[0..1] of Char;
                      Date       : Array[0..1] of Char;
                      Time       : Array[0..1] of Char;
                      CurrencySep: Byte;
                      NumCurr    : Byte;
                      TimeFmt    : Byte;
                      CaseMap    : Pointer;
                      AsciiSep   : Array[0..1] of Char;
                      Reserved   : Array[0..9] of Byte;
                    end; { record }

var Country: CountrySpec;
    Regs   : Registers;
{$ENDIF}

var TempStr: String;
    Counter: Longint;
begin
  {$IFDEF WIN32}
    CommaChar := SysUtils.ThousandSeparator;
  {$ENDIF}

  {$IFDEF OS2}
    CommaChar := SysUtils.ThousandSeparator;
  {$ENDIF}

  {$IFDEF ELEUNIX}
    CommaChar := SysUtils.ThousandSeparator;
  {$ENDIF}

  {$IFDEF MSDOS}
    FillChar(Regs, SizeOf(registers), 00);
    FillChar(Country, SizeOf(Country), 00);

    Regs.AH := $38;
    Regs.AL := $00;
    Regs.DS := Seg(Country);
    Regs.DX := Ofs(Country);
    Intr($21, Regs);

    CommaChar := Country.Thousands[0];
  {$ENDIF}

  TempStr := FStr(L);
  Counter := Length(TempStr);

  While (Counter > 3) do
    begin
      Dec(Counter, 3);

      if TempStr[Counter] <> '-' then
        Insert(CommaChar, TempStr, Counter + 1);
    end; { while }

  CommaStr := TempStr;
end; { func. CommaStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FValI(const S: String; D: Word): Longint;
var Temp     : Byte;
    Value    : longint;
    Code     : Longint;
    Negative : Boolean;
begin
  Negative := false;
  Code := 0;

  While (Code < Length(S)) AND (S[Code + 1] = #32) do           { Skip spaces }
    Inc(Code);

  Case S[Code + 01] of
     '-' : begin
             Negative := true;
             Inc(Code);
           end; { negative }
     '+' : begin
             inc(Code);
           end; { positive }
  end; { case }

  Case S[Code + 01] of
     '$' : begin
              D := 16;
              Inc(Code);
            end; { hex }
     '%' : begin
             D := 2;
             Inc(Code);
           end; { percentage }
  end; { case }

  While (Code < Length(S)) AND (S[Code] = '0') do Inc(Code);

  Value := 0;

  While (Code < Length(s)) do
    begin
      Inc(Code);

      Case S[Code] of
        '0'..'9' : Temp := Ord(S[Code]) - 48;
        'A'..'Z' : Temp := Ord(S[Code]) - 55;
        'a'..'z' : Temp := Ord(S[Code]) - 87;
         else Temp := 16;
      end; { case }

     if Temp >= d then
       begin
         fValI := 00;
         EXIT;
       end; { if }

     Value := (Value * D) + Temp;
   end;

  if Negative then Value := -Value;

  fVali := Value;
end; { func. fValI }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


end. { unit LongStr }
