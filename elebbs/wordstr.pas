unit WordStr;
(*
**
** Copyright (C) 1999-2003 Scott Little, Maarten Bekers. All rights reserved.
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
** Words String utilies for EleBBS
**
** Copyright (c) 2006 by Scott Little, Maarten Bekers
**
** Created : 2-aug-2006
** Last update : 2-aug-2006
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses CentrStr, Cases, CfgRec;

Const DefExtractWord   : CharSet = [#09, #32, ';'];

function  ExtractWord(const TempStr: String; Nr: Integer;
                      const CharAbortSet: CharSet;
                      const DoQuotes, IncludeEOS: Boolean): String;
function  Wordcount(TempStr: String; CharAbortSet: CharSet; DoQuotes: Boolean): Integer;
function  FirstWord(var TempStr:String; CharAbortSet: CharSet; DoQuotes: Boolean): String; { first word of string & remove it }
function  LastWord(TempStr: String; CharAbortSet: CharSet; DoQuotes: Boolean): String;
function  GetValue(ID: String; var Str: String; Remove: Boolean): String; { Get ID from Str }
function  FirstNameOnly(Str: String): String;
function  LastNameOnly(Str: String): String;
procedure Replace(Const Zoek_Voor, Vervang_Door:String; Var Str : String);
function  InQuotes(S:String): String;
function  InitialString(FromWho: String): String;
function  GetCommaArgument(S: String; Nr:Byte): String;
procedure RemoveWordNr(var TempStr: String; Nr: Word; CharAbortSet: CharSet; DoQuotes: Boolean);
procedure DoWrap(var DestStr, LeftOver: String; MaxLen: Longint);
{$IFDEF MSDOS}
  function  FirstWordA(var TempStr:String; CharAbortSet: CharSet; DoQuotes: Boolean):String; { first word of string & remove it }
  procedure RemoveWordNrA(var TempStr: String; Nr: Word; CharAbortSet: CharSet; DoQuotes: Boolean);
  procedure DoWrapA(var DestStr, LeftOver: String; MaxLen: Longint);
  procedure ReplaceA(Const Zoek_Voor, Vervang_Door:String; Var Str : String);
{$ELSE}
  function  FirstWordA(var TempStr:AnsiString; CharAbortSet: CharSet; DoQuotes: Boolean):AnsiString; { first word of string & remove it }
  procedure RemoveWordNrA(var TempStr: AnsiString; Nr: Word; CharAbortSet: CharSet; DoQuotes: Boolean);
  procedure DoWrapA(var DestStr, LeftOver: AnsiString; MaxLen: Longint);
  procedure ReplaceA(Const Zoek_Voor, Vervang_Door:AnsiString; Var Str : AnsiString);
{$ENDIF}
function FilterString(S: String; Chars: CharSet): String;
procedure RemoveChars(var S: String; Chars: CharSet);

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Const
    MappedWordsMax = {$IFDEF MSDOS}128{$ELSE}256{$ENDIF};

Type { used by MapWords }
    MappedWordsRecord = record
        pos,
        len : {$IFDEF MSDOS}Byte{$ELSE}Integer{$ENDIF};
    end;

    MappedWordsArray = array[1..MappedWordsMax] of MappedWordsRecord;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function MapWords(const TempStr: {$IFDEF MSDOS}String{$ELSE}AnsiString{$ENDIF}; const CharAbortSet: CharSet; const DoQuotes, IncludeEOS: Boolean) : MappedWordsArray;
{
    Populate a MappedWordsArray variable with the starting position and length
    of each word in a string; handle quotes appropriately if DoQuotes is True.

    Quotes are not removed, therefore zero length words are impossible and
    Len=0 shall indicate there are no more words in the string.
}
var
    CurWord : {$IFDEF MSDOS}Byte{$ELSE}Integer{$ENDIF};
    CurChar : {$IFDEF MSDOS}Byte{$ELSE}Integer{$ENDIF};
    inWord  : Boolean;
    inQuote : Boolean;
    MappedWords : MappedWordsArray;

begin
    CurWord := 0;
    CurChar := 1;
    inWord  := False;
    inQuote := False;
    fillchar(MappedWords, SizeOf(MappedWords), 0);

    for CurChar := 1 to Length(TempStr) do
        begin
            { ignore whitespace between words }
            if (not inWord) and (TempStr[CurChar] in CharAbortSet) then
                continue;

            { ignore everything between quotes }
            if inQuote then
                begin
                    if TempStr[CurChar] = '"' then
                        inQuote := False;
                    continue;
                end;

            { start word }
            if not inWord then
                begin
                    if DoQuotes and (TempStr[CurChar] = '"') then
                        inQuote := True;
                    inWord := True;
                    inc(CurWord);
                    MappedWords[CurWord].pos := CurChar;
                    continue;
                end;

            { end word }
            if inWord and (TempStr[CurChar] in CharAbortSet) then
                begin
                    inWord := False;

                    if IncludeEOS then
                        MappedWords[CurWord].len := CurChar-MappedWords[CurWord].pos+1
                    else
                        MappedWords[CurWord].len := CurChar-MappedWords[CurWord].pos;
                end;
        end;

    { handle end-of-string as word terminator }
    if inWord then
        if inQuote then                     { but ignore quoted words without terminating quote }
            MappedWords[CurWord].pos := 0
        else
            MappedWords[CurWord].len := CurChar-MappedWords[CurWord].pos+1;

    MapWords := MappedWords;
end; { func. MapWords }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function CountWords(const MappedWords: MappedWordsArray): Integer;
var
    Counter: Integer;

begin
    for Counter := 0 to MappedWordsMax-1 do
        if MappedWords[Counter+1].len = 0 then
            break;
    CountWords := Counter;
end; { func. CountWords }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function GetWordA(const TempStr: {$IFDEF MSDOS}String{$ELSE}AnsiString{$ENDIF}; Nr: Integer; const MappedWords: MappedWordsArray): {$IFDEF MSDOS}String{$ELSE}AnsiString{$ENDIF};
var
    Counter: Integer;

begin
    if (Nr = 0) or (Nr > MappedWordsMax) or (MappedWords[Nr].len = 0) then
        GetWordA := ''
    else
        GetWordA := copy(TempStr, MappedWords[Nr].pos, MappedWords[Nr].len);
end; { func. GetWordA }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function GetWord(const TempStr: String; Nr: Integer; const MappedWords: MappedWordsArray): String;
var
    TempStr2: {$IFDEF MSDOS}String{$ELSE}AnsiString{$ENDIF};

begin
    TempStr2 := TempStr;
    GetWord := GetWordA(TempStr2, Nr, MappedWords);
end; { func. GetWord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function PopWordA(var TempStr: {$IFDEF MSDOS}String{$ELSE}AnsiString{$ENDIF}; Nr: Integer; const MappedWords: MappedWordsArray): {$IFDEF MSDOS}String{$ELSE}AnsiString{$ENDIF};
var
    Counter: Integer;

begin
    if (Nr = 0) or (Nr > MappedWordsMax) or (MappedWords[Nr].len = 0) then
        PopWordA := ''
    else
        begin
            PopWordA := copy(TempStr, MappedWords[Nr].pos, MappedWords[Nr].len);

            if CountWords(MappedWords) > Nr then
                delete(TempStr, MappedWords[Nr].pos, MappedWords[Nr+1].pos-MappedWords[Nr].pos)
            else
                delete(TempStr, MappedWords[Nr].pos, MappedWords[Nr].len);
        end;
end; { func. PopWordA }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function PopWord(var TempStr: String; Nr: Integer; const MappedWords: MappedWordsArray): String;
var
    TempStr2: {$IFDEF MSDOS}String{$ELSE}AnsiString{$ENDIF};

begin
    TempStr2 := TempStr;
    PopWord := PopWordA(TempStr2, Nr, MappedWords);
    TempStr := TempStr2;
end; { func. PopWord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function ExtractWordA(const TempStr: {$IFDEF MSDOS}String{$ELSE}AnsiString{$ENDIF}; Nr: Integer; const CharAbortSet: CharSet; const DoQuotes, IncludeEOS: Boolean): {$IFDEF MSDOS}String{$ELSE}AnsiString{$ENDIF};
begin
    ExtractWordA := GetWordA(TempStr, Nr, MapWords(TempStr, CharAbortSet, DoQuotes, IncludeEOS));
end; { func. ExtractWordA }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function ExtractWord(const TempStr: String; Nr: Integer; const CharAbortSet: CharSet; const DoQuotes, IncludeEOS: Boolean): String;
begin
    ExtractWord := GetWord(TempStr, Nr, MapWords(TempStr, CharAbortSet, DoQuotes, IncludeEOS));
end; { func. ExtractWord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure RemoveWordNrA(var TempStr: {$IFDEF MSDOS}String{$ELSE}AnsiString{$ENDIF}; Nr: Word; CharAbortSet: CharSet; DoQuotes: Boolean);
begin
    PopWordA(TempStr, Nr, MapWords(TempStr, CharAbortSet, DoQuotes, False));
end; { proc. RemoveWordNrA }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure RemoveWordNr(var TempStr: String; Nr: Word; CharAbortSet: CharSet; DoQuotes: Boolean);
begin
    PopWord(TempStr, Nr, MapWords(TempStr, CharAbortSet, DoQuotes, False));
end; { proc. RemoveWordNr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function Wordcount(TempStr: String; CharAbortSet: CharSet; DoQuotes: Boolean): Integer;
var
    TempStr2: {$IFDEF MSDOS}String{$ELSE}AnsiString{$ENDIF};

begin
    TempStr2 := TempStr;
    Wordcount := CountWords(MapWords(TempStr2, CharAbortSet, DoQuotes, False));
end; { func. WordCount }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function  GetValue(ID: String; var Str: String; Remove: Boolean): String;       { Get ID from Str }
var TempStr : String;
{$IFNDEF FPC}
{$IFNDEF WIN32}
  {$IFNDEF OS2}
    Result  : String;
  {$ENDIF}
{$ENDIF}
{$ENDIF}
begin
 Result := '';
 GetValue := '';
 If Pos(SUpcase(ID), SUpcase(Str))=00 then Exit;

 TempStr := Copy(Str, Pos(SUPcase(ID), SUpcase(Str)) + Pred(Length(ID)), Length(Str));

 If Pos(#32, TempStr)=00 then Result := TempStr
  else Result := Copy(TempStr, 1, Pos(#32, TempStr)-1);

 If Remove then Replace(ID + Copy(Result, 2, 255), '', Str);

 Getvalue := Copy(Result, 2, 255);
end; { func. Getvalue }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  InQuotes(S:String): String;
var Counter: Longint;
    TempStr: String;
begin
  TempStr := '';

  Counter := 01;

  If Length(S)>0 then
   While (Counter<=Length(S)) AND (S[Counter] <> '"') do
    Inc(Counter);

  If S[Counter]='"' then
   begin
     Inc(Counter);

     While (Counter<=Length(S)) AND (S[Counter] <> '"') do
      begin;
        TempStr := TempStr + S[Counter];
        Inc(Counter);
      end; { while }
   end; { If S[Counter] }

   InQuotes := TempStr;
end; { Counter }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  GetCommaArgument(S: String; Nr:Byte): String;
var Counter  : Byte;
    Commas   : Byte;
    TempStr  : String;
begin
  GetCommaArgument := '';

  Commas := 01;
  TempStr := '';

  For Counter := 01 to Length(S) do
    begin;
     If S[Counter]=',' then Inc(Commas);

     If S[Counter] <> ',' then
      If Commas=Nr then TempStr := TempStr + S[Counter];

     If Commas>Nr then break;
    end; { Commas }

 GetCommaArgument := TempStr;
end; { func. GetCommaArgument }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function InitialString(FromWho: String): String;
var TempStr : String;
    MaxLen  : Byte;
    Tmp     : String;
begin
  TempStr := '';

  MaxLen := 01;
  While (Trim(FromWho)<>'') AND (MaxLen<14) do
    begin
       Tmp := Trim(FirstWord(FromWho, defExtractWord, false));

       TempStr := TempStr + UpCase(Tmp[1]);
    end; { while }

  InitialString := TempStr;
end; { func. InitialString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function FirstNameOnly(Str: String): String;
begin
  If Pos(#32, Str)>00 then FirstNameOnly := Copy(Str, 1, Pos(#32, Str)-1)
   else FirstNameOnly := Str;
end; { func. FirstNameOnly }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

Function LastNameOnly(Str: String): String;
begin
  LastNameOnly := '';
  If Pos(#32, Str)>00 then LastNameOnly := Copy(Str, Pos(#32, Str)+1, Length(Str));
end; { func. LastNameOnly }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

{$IFDEF MSDOS}
procedure ReplaceA(Const Zoek_Voor, Vervang_Door:String; Var Str : String);
{$ELSE}
procedure ReplaceA(Const Zoek_Voor, Vervang_Door:AnsiString; Var Str : AnsiString);
{$ENDIF}
var PosCNT : Longint;
begin
  PosCnt := Pos(SupCaseA(Zoek_Voor), SupCaseA(Str));
  if poscnt=0 then exit;

  Delete(Str, PosCnt, Length(Zoek_Voor) );
  Insert(Vervang_Door, Str, PosCnt);
end; { proc. ReplaceA }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure Replace(Const Zoek_Voor, Vervang_Door:String; Var Str : String);
{$IFDEF MSDOS}
var Str_Tmp: String;
{$ELSE}
var Str_Tmp: AnsiString;
{$ENDIF}
begin
  Str_Tmp := Str;

  ReplaceA(Zoek_Voor, Vervang_Door, Str_Tmp);

  Str := Str_Tmp;
end; { proc. Replace }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function  FirstWordA(var TempStr: {$IFDEF MSDOS}String{$ELSE}AnsiString{$ENDIF}; CharAbortSet: CharSet; DoQuotes: Boolean): {$IFDEF MSDOS}String{$ELSE}AnsiString{$ENDIF};
begin
    FirstWordA := PopWordA(TempStr, 1, MapWords(TempStr, CharAbortSet, DoQuotes, False));
end; { func. FirstWordA }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function  FirstWord(var TempStr: String; CharAbortSet: CharSet; DoQuotes: Boolean): String; { first word of string & remove it }
begin
    FirstWord := PopWord(TempStr, 1, MapWords(TempStr, CharAbortSet, DoQuotes, False));
end; { func. FirstWord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function LastWord(TempStr: String; CharAbortSet: CharSet; DoQuotes: Boolean): String; { last word of string & *DON'T* remove it (??) }
var
    MappedWords: MappedWordsArray;
begin
    MappedWords := MapWords(TempStr, CharAbortSet, DoQuotes, False);
    LastWord := GetWord(TempStr, CountWords(MappedWords), MappedWords);
end; { func. LastWord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

{$IFDEF MSDOS}
  procedure DoWrapA(var DestStr, LeftOver: String; MaxLen: Longint);
{$ELSE}
  procedure DoWrapA(var DestStr, LeftOver: AnsiString; MaxLen: Longint);
{$ENDIF}
Const NextWordChars : CharSet = [' ', ',', '.', ')', ']',
                                 '_', '!', '?', '*', #10, #00];

var Counter: Longint;
    TempStr: String;
begin
  TempStr := '';
  LeftOver := '';

  if DestStr[Length(DestStr)] in NextWordChars then
   if Length(DestStr) <= MaxLen then
    begin
      EXIT;
    end; { if }

  {------------------ Scan backwards for a string ---------------------------}
  Counter := Length(DestStr);
  While Counter > 1 do
    begin
      TempStr := DestStr[Counter] + TempStr;

      if DestStr[Counter] in NextWordChars then
       if Counter <= MaxLen then
         begin
           LeftOver := Copy(TempStr, 2, Length(TempStr) - 1);
           DestStr := Copy(DestStr, 1, Length(DestStr) - (Length(LeftOver)));
           BREAK;
         end; { if }

      Dec(Counter);
    end; { while }

end; { proc. DoWrap }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoWrap(var DestStr, LeftOver: String; MaxLen: Longint);
{$IFDEF MSDOS}
var TmpDest: String;
    TmpLeft: String;
{$ELSE}
var TmpDest: AnsiString;
    TmpLeft: AnsiString;
{$ENDIF}
begin
  TmpDest := DestStr;
  TmpLeft := LeftOver;

  DoWrapA(TmpDest, TmpLeft, MaxLen);

  DestStr := TmpDest;
  LeftOver := TmpLeft;
end; { proc. DoWrap }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FilterString(S: String; Chars: CharSet): String;
var TempStr: String;
    Counter: Longint;
begin
  TempStr := '';

  for Counter := 01 to Length(s) do
    if (S[Counter] in Chars) then TempStr := TempStr + S[Counter];

  FilterString := TempStr;
end; { func. FilterString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure RemoveChars(var S: String; Chars: CharSet);
var TempStr: String;
    Counter: Longint;
begin
  TempStr := '';

  for Counter := 01 to Length(s) do
    if (NOT (S[Counter] in Chars)) then TempStr := TempStr + S[Counter];

  S := TempStr;
end; { proc. RemoveChars }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


end. { unit WORDSTR }
