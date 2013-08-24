unit StUtils;
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

uses Global,
      Colors,
        Debug_U,
         BitWise,
          Cases,
           LongStr,
            CentrStr,
             CfgRec,
              GenDos,
               Dos,
                ObjDec;


procedure FullInsert(Source: String; var S: String; Index: Integer);
Procedure AddrToString(var s:String;zone,net,node,point:SmallWord);
procedure Str2Az(Str: String; MaxLen: Byte; var AZStr);
Procedure StringToAddr(var s:String;var zone,net,node,point:SmallWord);
Procedure Str2Set(S: String; var Chars: CharSet);
Function  MakeLen(Str : String; Len : Byte; Fill : fillUp; Fore, CountEle:Boolean) : String;
Function  RightJust (S : String;Len : Byte) : String; { RJustify with spaces }
Function  HexLong(Number: LongInt): String;
function  DotPadding(S: String; NrOfDots: Word; WasNumeric: Boolean; PadChar: Char): String;
Function  HexStr(Number: SmallWord): ShortString;
function  FixTime(S: String): String;
function  ParseAddr(AStr: String; CurrAddr: AddrType; Var DestAddr: AddrType): Boolean;
function AddrStr(Addr: AddrType): String;
Function  LastChar(S: String): Char;
Function  FirstChar(S: String): Char;
function  PadLeft(Tmp: String; TempCh: Char; Count: Longint): String;
function  PadRight(Tmp: String; TempCh: Char; Count: Longint): String;
Function  SlashStr(S: String): String;
Function  AsciiStr(S: String): String;
Function  LeftJust (S : String;Len : Byte) : String;  { LJustify with spaces }
Function  LeftJustChar (S : String;Len : Byte;C : Char) : String; { LJustify }
function  StripLead(TempStr: String; TempCH: Char): String;
function  StripTrail(TempStr: String; TempCH:Char): String;
function  StripBoth(TempStr: String; TempCH:Char): String;
Function  ToDec (Hex : String) : Byte;         { Conv hex string to dec byte }
Function  ToHex (Dec : Byte) : ShortString;    { Conv decimal byte to 2 digit hex }
Function  RightJustChar (S : String;Len : Byte;C : Char) : String;{ RJustify }
function  PosLastChar(TempCH: Char; TmpStr: String): Word;
function  Under2Norm(S: String): String;
function  Space2Dot(S: String): String;
function  Az2Str(var ResStr: String; MaxLen: Byte): String;
Function  FormattedDate(DT: DateTime; Mask: String): String;
function  ReformatDate(ODate: String; Mask: String): String;
function  NoColorCopy(S: String; Index: Integer; Count: Integer): String;
function  Bool2Str(B: Boolean): String;
function  FixUserName(InputStr: String): String;
{$IFNDEF MSDOS}
procedure AnsiFullInsert(Source: AnsiString; var S: AnsiString; Index: Integer);
Function  AnsiMakeLen(Str : AnsiString; Len : Longint; Fill : fillUp; Fore:Boolean) : AnsiString;
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
uses JDates;

function DotPadding(S: String; NrOfDots: Word; WasNumeric: Boolean; PadChar: Char): String;
begin
  DotPadding := S;
  if NrOfDots=00 then EXIT;

  Case PadChar of
    '.' : if WasNumeric then
            DotPadding := Copy(RightJust(S, NrOfDots), 1, NrOfDots)
               else DotPadding := Copy(LeftJust(S, NrOfDots), 1, NrOfDots);
    ',' : DotPadding := Copy(RightJust(S, NrOfDots), 1, NrOfDots);
    '?' : DotPadding := Copy(LeftJust(S, NrOfDots), 1, NrOfDots);
    '`' : DotPadding := Copy(CenterJust(S, NrOfDots, true), 1, NrOfDots);
  end; { case }
end; { func. DotPadding }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  FixTime(S: String): String;
var SepPos: Byte;
    First : String;
    Second: String;
begin
  FixTime := S;

  SepPos := Pos(':', S);
  if SepPos = 00 then SepPos := 02;

  First := Copy(S, 1, SepPos - 01);
  Second := Copy(S, SepPos + 01, 2);

  First := MakeLen(First, 2, Zero, True, false);
  Second := MakeLen(Second, 2, Zero, True, false);

  FixTime := First + ':' + Second;
end; { func. FixTime }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AddrToString(var s:String;zone,net,node,point:SmallWord);
begin
  S := FStr(Zone) + ':' +
       FStr(Net)  + '/' +
       FStr(Node);

  if Point <> 00 then S := S + '.' + FStr(Point);
end; { proc. AddrToStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure StringToAddr(var S: String; var Zone, Net, Node, Point: SmallWord);
{Code: Hans Siemons}
type NextType = (nNet, nNode, nPoint);

var Pos  : Integer;
    L    : String;
    N    : NextType;

begin
  L := '';
  n := nNode;                           { Next type expected is a node-number }

  if S = '' then                           { Empty? Exit with default address }
    begin
      AddrToString(S, Zone, Net, Node, Point);
      EXIT;
    end; { if }

  For Pos := 01 to Length(s) do
    begin
      if (S[Pos] in ['0','1','2','3','4','5','6','7','8','9']) then
        begin
          L := L + S[Pos];                   { While is a number, keep adding }
        end else

      if S[Pos] = ':' then                                        { :? L=ZONE }
        begin                                                          { Zone }
          if L <> '' then
            begin
              Zone := SmallWord(FVal(L));
              L := '';
            end; { if }

          N := nNet;                     { Next type expected is a net-number }
          Point := 00;                                   { Clear point-number }
        end else
      if S[Pos]='/' then                                             {/? L=NET}
        begin                                                           { Net }
          if L <> '' then
            begin
              Net := SmallWord(FVal(L));
              L := '';
            end;

          N := nNode;                   { Next type expected is a node-number }
          Point:=0;                                            { Clear Point# }
        end else
      if S[Pos]='.' then                                             { Point? }
        begin
          if L <> '' then
            begin
              Node := SmallWord(Fval(L));                  { Nodenumber before it? }
              L := '';
              Point := 00;
            end; { if }

          n := nPoint;                 { Next type expected is a point-number }
        end; { if }
    end; { for }

  if N = nNet then
    begin
      Net := SmallWord(FVal(l));
      Point := 00;
    end else
  if N = nNode then
    begin
      Node := SmallWord(FVal(l));
      Point := 00;
    end
     else Point := SmallWord(FVal(l));

  AddrToString(S, Zone, Net, Node, Point);
end; { StringToAddr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Under2Norm(S: String): String;
var Counter: Byte;
begin
  for Counter := 01 to Length(s) do
   if S[Counter] = '_' then S[counter] := #32;

  Under2Norm := s;
end; { func. Under2Norm }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Space2Dot(S: String): String;
var Counter: Longint;
begin
  for Counter := 01 to Length(s) do
    if S[Counter] = #32 then S[Counter] := '.';

  Space2Dot := s;
end; { func. Space2Dot }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function SlashStr(S: String): String;
var Returns: String;
    Counter: Byte;
    TempCH : String[3];
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'SlashStr (S='+S+')');
  {$ENDIF}
  Returns := '';

  For Counter := 01 to Length(s) do
    begin

      TempCH := S[Counter];
      if (TempCH[1] in ['\', ']', '}']) then
        TempCH := TempCH + TempCH
         else
          If  (TempCH < #32) OR (TempCH > #127) then
            TempCh := '\' + ToHex(Byte(S[Counter]));
          Returns := Returns + TempCH;

    end; { for }

  SlashStr := Returns;
end; { func. SlashStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function AsciiStr(S: String): String;
var Returns: String;
    Counter: Byte;
    TempCH : String[3];
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'AsciiStr: '+S);
  {$ENDIF}
  Returns := '';

  if S <> '' then
    begin
      Counter := 00;

      repeat
        Inc(Counter);

        if S[Counter] = '\' then
          begin

            If (Counter < Length(s)) then
             begin

               if (S[Counter + 01] = '\') then
                begin
                 TempCH := '\';
                 Inc(Counter);
                end
                 else begin
                        TempCH := Char(ToDec(Copy(S, Counter + 01, 2)));
                        Inc(Counter, 2);
                      end;
             end
               else TempCH := S[Counter];

          end
            else TempCH := S[Counter];

        Returns := Returns + TempCH;

      until Counter >= Length(s);
    end; { if s <> '' }

  AsciiStr := Returns;
end; { func. AsciiStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function MakeLen(Str: String; Len: Byte; Fill: fillUp; Fore, CountEle:Boolean): String;

function Length(S: String): Byte;
begin
  if NOT CountELe then Length := System.Length(s)
    else Length := NoColorLength(s);
end; { func. Length }

var C: Char;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'MakeLen (Str='+Str+')');
  {$ENDIF}

  if Fill=Zero then C := '0' else C := #32;

  if Len > Length(str) then
    Case Fore of
      TRUE  : While Len > length(Str) do Str := C + Str;
      FALSE : While Len > Length(Str) do Str := Str + C;
    end; { case }

  if Len > 00 then
    if Length(Str) > len then
      begin
        Case Fore of
          false : While Length(Str) > Len do Delete(Str, Length(Str), 1);
          true  : While Length(Str) > Len do Delete(Str, 1, 1);
        end; { case }
      end; { if }

  MakeLen := Str;
end; { func. MakeLen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF MSDOS}
function AnsiMakeLen(Str: AnsiString; Len: Longint; Fill: fillUp; Fore: Boolean): AnsiString;
var C: Char;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'MakeLen (Str='+Str+')');
  {$ENDIF}

  if Fill=Zero then C := '0' else C := #32;

  if Len > Length(str) then
    Case Fore of
      TRUE  : While Len > length(Str) do Str := C + Str;
      FALSE : While Len > Length(Str) do Str := Str + C;
    end; { case }

  if Len > 00 then
    if Length(Str) > len then
      begin
        Case Fore of
          false : While Length(Str) > Len do Delete(Str, Length(Str), 1);
          true  : While Length(Str) > Len do Delete(Str, 1, 1);
        end; { case }
      end; { if }

  AnsiMakeLen := Str;
end; { func. AnsiMakeLen }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Str2Set(S: String; var Chars: CharSet);
var Teller: Byte;
begin
  FillChar(Chars, SizeOf(Chars), 00);
  Chars := [];

  If Length(S)>0 then
   For Teller:=01 to Length(S) do
    {$IFDEF FPK}
      Chars := Chars + [S[teller]];
    {$ELSE}
      Include(Chars, S[Teller]);
    {$ENDIF}
end; { proc. Str2Set }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FirstChar(S: String): Char;
begin
  FirstChar := S[1];
end; { func. FirstChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function LastChar(S: String): Char;
begin
  LastChar := S[Length(S)];
end; { func. LastChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function LeftJust (S: String; Len: Byte): String;      { LJustify with spaces }
begin
  LeftJust := LeftJustChar(S, Len, ' ');
end; { func. LeftJust }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  LeftJustChar (S: String; Len: Byte; C: Char): String;    { LJustify }
begin
  While Length(s) < Len do
    S := S + C;
  LeftJustChar := S;
end; { func. LeftJustChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function RightJust (S: String; Len: Byte): String;     { RJustify with spaces }
begin
  RightJust := RightJustChar(S, Len, ' ');
end; { func. RightJust }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function RightJustChar (S: String; Len: Byte; C: Char): String;    { RJustify }
begin
  While Length(S)<Len do
    S:= C + S;
  RightJustChar := S;
end; { func. RightJustChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ToDec (Hex : String): Byte;            { Conv hex string to dec byte }
var Temp: Byte;
{$IFNDEF WIN32}
    Err: Integer;
{$ELSE}
    Err: Longint;
{$ENDIF}
begin
  if (NOT (UpCase(Hex[1]) in ['0'..'9', 'A'..'F'])) then Hex[1] := '0';
  if (NOT (UpCase(Hex[2]) in ['0'..'9', 'A'..'F'])) then Hex[2] := '0';

  Val('$'+Hex, Temp, Err);

  If Err>00 then ToDec := 00
   else ToDec := Temp;
end; { func. ToDec }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  ToHex (Dec : Byte) : ShortString;    { Conv decimal byte to 2 digit hex }
const HexChars : Array[0..15] of Char = '0123456789ABCDEF';
begin
  ToHex[0] := #2;
  ToHex[1] := HexChars[Dec SHR  4];
  ToHex[2] := HexChars[Dec AND 15];
end; { func. ToHex }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function HexStr(Number: SmallWord): ShortString;

Type WordRec = Record
                 Lo,
                 Hi : Byte;
               end;

const Digits : Array[0..15] of Char = '0123456789ABCDEF';

var Num: WordRec ABSOLUTE Number;
begin
  HexStr[0] := #4;
  HexStr[1] := Digits[Num.Hi shr 4];
  HexStr[2] := Digits[Num.Hi and $F];
  HexStr[3] := Digits[Num.Lo shr 4];
  HexStr[4] := Digits[Num.Lo and $F];
end; { func. HexStr}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function HexLong(Number: LongInt): String;
type WordRec = Record
                 Lo : SmallWord;
                 Hi : SmallWord;
               End; { record }
begin
  HexLong := HexStr(WordRec(Number).Hi) + HexStr(WordRec(Number).Lo);
end; { func. HexLong }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF MSDOS}
procedure AnsiFullInsert(Source: AnsiString; var S: AnsiString; Index: Integer);
begin
  if Index > Length(s) then
    S := MakeLen(S, Index - 1, Space, false, true);

  Insert(Source, S, Index);
end; { proc. AnsiFullInsert }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FullInsert(Source: String; var S: String; Index: Integer);
begin
  if Index > Length(s) then
    S := MakeLen(S, Index - 1, Space, false, true);

  Insert(Source, S, Index);
end; { proc. FullInsert }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function NoColorCopy(S: String; Index: Integer; Count: Integer): String;
var Counter : Longint;
    Extra   : Longint;
    StartPos: Longint;
    TempStr : String;
begin
  Counter := 01;
  Extra := 0;
  TempStr := '';

  {------------------------ Calculate a start value ------------------------}
  While Counter <= (Index + 1)  do
    begin
      Case S[Counter + Extra] of
        '`' : begin
                Inc(Extra);

                While (UpCase(S[Counter + Extra]) in
                       ['0'..'9',',','$','-','+','A','B','C','D','E','F','G','H','L','S','X','Y', ':']) do
                         Inc(Extra);
              end; { ` }
        ^K  : begin
                Inc(Extra, 4);
              end;
          else Inc(Counter);
      end; { case }

    end; { while }

  {--------------------------- Copy all information ------------------------}
  Counter := 01;
  Extra := 0;
  StartPos := Index + Extra;

  REPEAT
    Case S[Counter + Extra] of
        '`' : begin
                Inc(Extra);

                While (UpCase(S[Counter + Extra]) in
                       ['0'..'9',',','$','-','+','A','B','C','D','E','F','G','H','L','S','X','Y']) do
                         Inc(Extra);
              end; { ` }
        ^K  : begin
                Inc(Extra, 4);
              end;
          else begin
                 Inc(Counter);

                 if S[Counter + Extra] in ['`', ^K] then
                   begin
                     Dec(Counter);
                     Inc(Extra);
                   end; { if }
               end; { else }
    end; { case }
  UNTIL (Counter > Length(s)) OR (Counter > Count);

  TempStr := Copy(S, StartPos, Counter + Extra);
  NoColorCopy := TempStr;
end; { func. NoColorCopy }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FixUserName(InputStr: String): String;
var Counter  : Longint;
    SaveInput: Char;
begin
  for Counter := 01 to Length(InputStr) do
    begin
      SaveInput := InputStr[Counter];

      if (Counter = 01) OR (InputStr[Counter - 1] in [#32, '-', '.', ',']) then
        InputStr[Counter] := UpCase(InputStr[Counter])
          else InputStr[Counter] := LowCase(InputStr[Counter]);

      if InputStr[Length(InputStr)] = 'c' then
        if (sUpCase(Copy(InputStr, Length(InputStr) - 1, 2)) = 'MC') or
            (sUpCase(Copy(InputStr, Length(InputStr) - 2, 3)) = 'MAC') then
              InputStr[Counter] := SaveInput;
    end; { for }

  FixUserName := InputStr;
end; { func. FixUserName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  Bool2Str(B: Boolean): String;
begin
  if B then Bool2Str := 'ON'
    else Bool2Str := 'OFF';
end; { func. Bool2Str }


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function ParseAddr(AStr: String; CurrAddr: AddrType; Var DestAddr: AddrType): Boolean;
{ MkMSG code}
var SPos   : Longint;
    EPos   : Longint;
    TempStr: String;
    Code   : NumreadType;
    BadAddr: Boolean;
    TmpLong: Longint;
begin
  {-- initialize some variables ------------------------------------------}
  BadAddr := False;
  AStr := Trim(SupCase(AStr));
  EPos := Length(AStr);

  {thanks for the fix domain problem to Ryan Murray @ 1:153/942}
  Code := Pos('@', AStr);
  If Code > 0 then
    Delete(Astr, Code, Length(Astr) + 1 - Code);
  SPos := Pos(':',AStr) + 1;

  {-- if we found a dmain, strip it -------------------------------------}
  if SPos > 1 then
    begin
      TempStr := Trim(Copy(AStr,1,Spos - 2));
      Val(TempStr,TmpLong,Code);

      if Code <> 0 Then
        BadAddr := True
         else DestAddr.Zone := SmallWord(TmpLong);

      AStr := Copy(AStr,Spos,Length(AStr));
    end
      else DestAddr.Zone := CurrAddr.Zone;

  {-- get the net -------------------------------------------------------}
  SPos := Pos('/',AStr) + 1;
  if sPos > 1 then
    begin
      TempStr := Trim(Copy(AStr,1,Spos - 2));
      Val(TempStr,TmpLong,Code);
      if Code <> 0 Then
        BadAddr := True
         else DestAddr.Net := SmallWord(TmpLong);

      AStr := Copy(AStr,Spos,Length(AStr));
    end
      else DestAddr.Net := CurrAddr.Net;


  {-- get the point -----------------------------------------------------}
  EPos := Pos('.', AStr) + 1;
  if EPos > 1 then
    begin
      TempStr := Trim(Copy(AStr,EPos,Length(AStr)));
      Val(TempStr,TmpLong,Code);
      If Code <> 0 Then
        DestAddr.Point := 0
         else DestAddr.Point := SmallWord(TmpLong);

      AStr := Copy(AStr,1,EPos -2);
    end
      else DestAddr.Point := 0;

  {-- get the node number -----------------------------------------------}
  TempStr := Trim(AStr);
  if Length(TempStr) > 0 Then
    begin
      Val(TempStr,TmpLong,Code);
      if Code <> 0 Then
        BadAddr := True
         else DestAddr.Node := Smallword(TmpLong);
    end
      else DestAddr.Node := CurrAddr.Node;

  {-- and return -------------------------------------------------------}
  ParseAddr := NOT BadAddr;
end; { func. ParseAddr }


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function AddrStr(Addr: AddrType): String;
var TmpStr: String;
begin
  AddrtoString(TmpStr,
               Addr.Zone,
               Addr.Net,
               Addr.Node,
               Addr.Point);

  AddrStr := TmpStr;
end; { func. AddrStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function StripLead(TempStr: String; TempCH: Char): String;
begin
	while (length(TempStr) > 0) and (TempStr[1] = TempCH) do
		delete(TempStr, 1, 1);

	StripLead := TempStr;
end; { func. StripLead }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function StripTrail(TempStr: String; TempCH:Char): String;
begin
	while (length(TempStr) > 0) and (TempStr[length(TempStr)] = TempCH) do
		delete(TempStr, length(TempStr), 1);

	StripTrail := TempStr;
end; { func. StripTrail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function StripBoth(TempStr: String; TempCH:Char): String;
begin
	StripBoth := StripTrail(StripLead(TempStr, TempCH), TempCH);
end; { func. StripBoth }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function PadLeft(Tmp: String; TempCh: Char; Count: Longint): String;
var TempStr: String;
    Counter: Longint;
begin
  if Length(Tmp) >= Count then
    PadLeft := Copy(Tmp, 1, Count)
      else begin
             for Counter := 1 to (Count - Length(Tmp)) do
               TempStr[Counter] := TempCH;

             TempStr[0] := Chr(Count - Length(Tmp));
             PadLeft := TempStr + Tmp;
           end; { else }
end; { func. PadLeft }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function PadRight(Tmp: String; TempCh: Char; Count: Longint): String;
var TempStr: String;
begin
  TempStr := Tmp;
  if Length(TempStr) > Count then
    TempStr := Copy(TempStr, 1, Count);

  if Length(TempStr) < Count then
    begin
      TempStr := TempStr + Dup(TempCH, Count - Length(TempStr));
    end; { if }

  PadRight := TempStr;
end; { func. PadRight }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function FormattedDate(DT: DateTime; Mask: String): String;
{ MKMSG }
var DStr: String[2];
    MStr: String[2];
    MNStr: String[3];
    YStr: String[4];
    HourStr: String[2];
    MinStr: String[2];
    SecStr: String[2];
    TmpStr: String;
    CurrPos: Word;
    i: Word;
begin
  TmpStr := Mask;
  Mask := SUpCase(Mask);
  DStr := Copy(PadLeft(FStr(Dt.Day),'0',2),1,2);
  MStr := Copy(PadLeft(FStr(Dt.Month),'0',2),1,2);
  YStr := Copy(PadLeft(FStr(Dt.Year),'0',4),1,4);
  HourStr := Copy(PadLeft(FStr(Dt.Hour),' ', 2),1,2);
  MinStr := Copy(PadLeft(FStr(Dt.Min), '0',2),1,2);
  SecStr := Copy(PadLeft(FStr(Dt.Sec), '0',2),1,2);
  MNStr := Copy(MonthString(Dt.Month), 1, 3);
  If (Pos('YYYY', Mask) = 0) Then
    YStr := Copy(YStr,3,2);
  CurrPos := Pos('DD', Mask);
  If CurrPos > 0 Then
    For i := 1 to Length(DStr) Do
      TmpStr[CurrPos + i - 1] := DStr[i];
  CurrPos := Pos('YY', Mask);
  If CurrPos > 0 Then
    For i := 1 to Length(YStr) Do
      TmpStr[CurrPos + i - 1] := YStr[i];
  CurrPos := Pos('MM', Mask);
  If CurrPos > 0 Then
    For i := 1 to Length(MStr) Do
      TmpStr[CurrPos + i - 1] := MStr[i];
  CurrPos := Pos('HH', Mask);
  If CurrPos > 0 Then
    For i := 1 to Length(HourStr) Do
      TmpStr[CurrPos + i - 1] := HourStr[i];
  CurrPos := Pos('SS', Mask);
  If CurrPos > 0 Then
    For i := 1 to Length(SecStr) Do
      TmpStr[CurrPos + i - 1] := SecStr[i];
  CurrPos := Pos('II', Mask);
  If CurrPos > 0 Then
    For i := 1 to Length(MinStr) Do
      TmpStr[CurrPos + i - 1] := MinStr[i];
  CurrPos := Pos('NNN', Mask);
  If CurrPos > 0 Then
    For i := 1 to Length(MNStr) Do
      TmpStr[CurrPos + i - 1] := MNStr[i];
  FormattedDate := TmpStr;
End;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ReformatDate(ODate: String; Mask: String): String;
var DT: DateTime;
    Code: NumReadType;
begin
  val(Copy(ODate,7,2), DT.Year, Code);
  val(Copy(ODate,1,2), DT.Month, Code);
  val(Copy(ODate,4,2), DT.Day, Code);

  if DT.Year < 80 Then
    Inc(DT.Year, 2000)
      else Inc(DT.Year, 1900);

  ReformatDate := FormattedDate(DT, Mask);
end; { func. ReFormatDate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function PosLastChar(TempCH: Char; TmpStr: String): Word;
var Counter: Longint;
begin
  Counter := Length(TmpStr);

  while ((Counter > 0) AND (TmpStr[Counter] <> TempCH)) do
    Dec(Counter);

  PosLastChar := Counter;
end; { func. PosLastChar }


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Str2Az(Str: String; MaxLen: Byte; var AZStr);
begin
  if Length(Str) >= MaxLen then
    begin
      Str[MaxLen] := #0;
      Move(Str[1], AZStr, MaxLen);
    end
      else begin
             Str[Length(Str) + 1] := #0;
             Move(Str[1], AZStr, Length(Str) + 1);
           end;
end; { proc. Str2Az }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Az2Str(var ResStr: String; MaxLen: Byte): String;
var Counter: Word;
    TmpStr: String;
begin
  Move(ResStr, TmpStr[1], MaxLen);
  TmpStr[0] := Chr(MaxLen);
  Counter := Pos(#0, TmpStr);
  if Counter > 0 Then
    TmpStr[0] := Chr(Counter - 1);

  Az2Str := TmpStr;
end; { func. Az2Str }


end. { StUtils }
