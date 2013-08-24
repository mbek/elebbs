unit FlagStr;
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
** Flag to String (and vv) routines for EleBBS
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
uses Dos, Debug_U, BitWise;
{$ENDIF}

{$IFDEF OS2}
uses Dos, Debug_U, BitWise;
{$ENDIF}

{$IFDEF GO32V2}
uses Debug_U, BitWise;
{$ENDIF}

{$IFDEF ELEUNIX}
uses Debug_U, BitWise;
{$ENDIF}

{$IFDEF WIN32}
uses Debug_U, BitWise;
{$ENDIF}

function  Byte2Flags (B : Byte) : String;       { Convert byte to flag-style }
function  Str2Flags(S:String): Byte;          { Convert string to flag-style }
function  Byte2FlagsOff(B: Byte; Off: Byte): String;


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  Byte2Flags (B : Byte) : String;       { Convert byte to flag-style }
var Teller : Byte;
    TempStr: String;
begin
  For Teller:=00 to 07 do
   If (ReadBit(B, Teller)) then TempStr[Teller+1] := 'X'
     else TempStr[Teller+1] := '-';

  TempStr[0] := #08;
  Byte2Flags := TempStr;
end; { func. Byte2Flags }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  Str2Flags(S:String): Byte;         { Convert string to flag-style }
{$IFNDEF FPC}
{$IFNDEF WIN32}
 {$IFNDEF OS2}
var Result: Byte;
 {$ENDIF}
{$ENDIF}
{$ENDIF}
var Teller: Byte;
begin
  Result := 00;
  For Teller:=01 to Length(S) do
   If UpCase(S[Teller])='X' then SetBit(Result, Teller-1)
    else ClearBit(Result, Teller-1);
  Str2Flags := Result;
end; { func. Str2Flags }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Byte2FlagsOff(B: Byte; Off: Byte): String;
var Counter: Byte;
    TempStr: String;
begin
  For Counter := 00 to 07 do
   If (ReadBit(B, Counter)) then TempStr[Counter+1] := 'X'
     else If ReadBit(Off, Counter) then TempStr[Counter+1] := 'O'
      else TempStr[Counter + 1] := '-';

  TempStr[0] := #08;
  Byte2FlagsOff := TempStr;
end; { func. Byte2FlagsOff }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit FlagStr }
