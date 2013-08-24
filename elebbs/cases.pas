unit Cases;
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
** UpCase/LowCase and Trim() utilies for EleBBS
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


{$IFDEF MSDOS}
uses Dos;
{$ENDIF}

{$IFDEF OS2}
uses SysUtils, Dos;
{$ENDIF}

{$IFDEF WIN32}
uses Sysutils;
{$ENDIF}

{$IFDEF GO32V2}
uses SysUtils;
{$ENDIF}

{$IFDEF ELEUNIX}
uses SysUtils;
{$ENDIF}


function  UpCase (Ch : Char) : Char;             { Change char to upper case }
function  LowCase (Ch : Char) : Char;            { Change char to lower case }
function  SLowCase (S : String) : String;      { Change string to lower case }
{$IFNDEF MSDOS}
function  SLowCaseA(const S : AnsiString) : AnsiString;      { Change string to lower case }
function  SUpCaseA(const S : AnsiString) : AnsiString;                { UpCase for strings }
{$ELSE}
function  SLowCaseA(S : String) : String;      { Change string to lower case }
function  SUpCaseA(S : String) : String;                { UpCase for strings }
{$ENDIF}
function  FirstUpper(S:String): String;    { Shows first character uppercase }
function  SUpCase(S : String) : String;                { UpCase for strings }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  UpCase (Ch : Char) : Char;             { Change char to upper case }
Const HiUpChars : String[9] ='öêÉéèÄíô•';

{$IFDEF MSDOS}
var Regs: Registers;
{$ENDIF}

{$IFDEF WIN32}
var TempStr: String;
{$ENDIF}
begin
  if NOT (CH in ['A'..'Z', 'a'..'z']) then
    begin
      if (Pos(CH, 'ÅÇÉÑÜáëî§') > 0) then
        UpCase := HiUpChars[Pos(CH, 'ÅÇÉÑÜáëî§')]
          else Upcase := CH;

      EXIT;
    end; { if }

  {$IFDEF MSDOS}
    UpCase := System.UpCase(CH);
  {$ENDIF}

  {$IFDEF OS2}
    UpCase := System.UpCase(CH);
  {$ENDIF}

  {$IFDEF WIN32}
    {$IFNDEF VirtualPascal}
      TempStr := UpperCase(CH);
      Result := TempStr[1];
    {$ELSE}
      Result := System.UpCase(CH);
    {$ENDIF}
  {$ENDIF}

  {$IFDEF FPC}
    UpCase := System.UpCase(CH);
  {$ENDIF}
end; { func. UpCase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  LowCase (Ch : Char) : Char;            { Change char to lower case }
Const HiLowChars : String[9] = 'ÅÇÉÑÜáëî§';
begin
  if (UpCase(CH)=CH) AND (CH in ['A'..'Z']) then
       begin
           CH := Chr(Ord(CH) + 32);
       end { if }
         else begin
                if (Pos(CH,'öêÉéèÄíô•') > 00) then
                  CH := HiLowChars[Pos(CH, 'öêÉéèÄíô•')];
              end; { if }

  LowCase := CH;
end; { func. LowCase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  SLowCase (S : String) : String;       { Change string to lower case }
var Teller: Byte;
begin
  {$IFDEF MSDOS}
   For Teller:=01 to Length(S) do
     S[Teller] := LowCase(S[Teller]);
   SLowCase := S;
  {$ELSE}
    SLowCase := AnsiLowerCase(s);
  {$ENDIF}
end; { func. SlowCase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF MSDOS}
function  SLowCaseA(S : String) : String;                { UpCase for strings }
{$ELSE}
function  SLowCaseA(const S : AnsiString) : AnsiString;                { UpCase for strings }
{$ENDIF}
begin
  {$IFDEF MSDOS}
    SLowCaseA := SLowCase(S);
  {$ELSE}
    SLowCaseA := AnsiLowerCase(S);
  {$ENDIF}
end; { func. SUpcase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


function  SUpCase (S : String) : String;                { UpCase for strings }
var Teller: Byte;
begin
  {$IFDEF MSDOS}
    For Teller:=01 to Length(S) do
     S[Teller] := Upcase(S[Teller]);
    SUpcase := S;
  {$ELSE}
    SUpCase := AnsiUpperCase( s );
  {$ENDIF}
end; { func. SUpcase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF MSDOS}
function  SUpCaseA(S : String) : String;                { UpCase for strings }
{$ELSE}
function  SUpCaseA(const S : AnsiString) : AnsiString;                { UpCase for strings }
{$ENDIF}
begin
  {$IFDEF MSDOS}
    SUpCaseA := SUpCase(S);
  {$ELSE}
    SUpCaseA := AnsiUpperCase(S);
  {$ENDIF}
end; { func. SUpcase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FirstUpper(S:String): String;        { Shows first character uppercase }
begin
 S[1] := Upcase(S[1]);
 FirstUpper := S;
end; { FirstUpper }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

end. { unit cases }
