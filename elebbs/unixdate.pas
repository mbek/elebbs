unit UnixDate;
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
** UNIX Date Routines
**
** Part of EleBBS, copyright (c) 1996 by Maarten Bekers
**
** Based on routines of Brian Stark
**
** Created : 16-Dec-1996
** Last update : 16-Dec-1996
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF WIN32}
Uses Dos {$IFDEF WITH_DEBUG}, Debug_U{$ENDIF};
{$ELSE}
Uses SysUtils, GenDos
        {$IFDEF DELPHI}
//          ,Use32
        {$ELSE}
          ,Dos
        {$ENDIF};
{$ENDIF}

Function Norm2Unix(Year, Month, Day, Hour, Min, Sec: Word): LongInt;
Procedure Unix2Norm(Date: LongInt; var Year, Month, Day, Hour, Min, Sec: Word);
function NowAsUnixDate: Longint;
Function IsLeapYear(Source: Word): Boolean;
function IsToday(Date: Longint): Boolean;
function GetTimeZone: Longint;

Const
  DaysPerMonth    : Array[1..12] of Word = (031, 028, 031, 030, 031, 030, 031, 031, 030, 031, 030, 031);
  DaysPerYear     : Array[1..12] of Word = (031, 059, 090, 120, 151, 181, 212, 243, 273, 304, 334, 365);
  DaysPerLeapYear : Array[1..12] of Word = (031, 060, 091, 121, 152, 182, 213, 244, 274, 305, 335, 366);

  SecsPerYear     {: LongInt } = 365 * (24 * 3600);
  SecsPerLeapYear {: LongInt } = 366 * (24 * 3600);
  SecsPerDay      {: LongInt } = 24 * 3600;
  SecsPerHour     {: Integer } = 60 * 60;
  SecsPerMinute   {: ShortInt} = 60;

  SaveTZ          : Longint = 9999;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function NowAsUnixDate: Longint;
var D, M, Y, Dow    : Word;
    H, Min, S, S100 : Word;
begin
  GetDate(Y, M, D, Dow);
  GetTime(H, Min, S, S100);

  NowAsUnixDate := Norm2Unix(Y, M, D, H, Min, S);
end; { func. NowAsUnixDate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function GetTimeZone: Longint;                                 { Get Timezone }
var Environment: String;
    Counter: Byte;
begin
  if SaveTZ = 9999 then
    begin
      SaveTZ := 0;

      Environment := GetEnv('TZ');
      For Counter := 01 to Length(Environment) do
         Environment[Counter] := UpCase(Environment[Counter]);

      If Environment='EST05'    then SaveTZ :=  -05;            { USA EASTERN }
      If Environment='EST05EDT' then SaveTZ :=  -06;
      If Environment='CST06'    then SaveTZ :=  -06;            { USA CENTRAL }
      If Environment='CST06CDT' then SaveTZ :=  -07;
      If Environment='MST07'    then SaveTZ :=  -07;           { USA MOUNTAIN }
      If Environment='MST07MDT' then SaveTZ :=  -08;
      If Environment='PST08'    then SaveTZ :=  -08;
      If Environment='PST08PDT' then SaveTZ :=  -09;
      If Environment='YST09'    then SaveTZ :=  -09;
      If Environment='AST10'    then SaveTZ :=  -10;
      If Environment='BST11'    then SaveTZ :=  -11;
      If Environment='CET-1'    then SaveTZ :=   01;
      If Environment='CET-01'   then SaveTZ :=   01;
      If Environment='EST-10'   then SaveTZ :=   10;
      If Environment='WST-8'    then SaveTZ :=   08; { Perth, West-Austrailia }
      If Environment='WST-08'   Then SaveTZ :=   08;
    end;

  GetTimeZone :=  SaveTZ;
end; { func. GetTimeZone }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function IsLeapYear(Source: Word): Boolean;
begin
  IsLeapYear := (Source mod 4 = 0) AND NOT ((
                (Source mod 100 = 0) AND NOT (Source mod 400 = 0)));
end; { func. IsLeapYear }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Norm2Unix(Year, Month, Day, Hour, Min, Sec: Word): LongInt;
var UnixDate: LongInt;
    Counter : Word;
begin
  UnixDate := 00;                                                { Initialize }

  Inc(UnixDate, Sec);                                       { Add the seconds }
  Inc(UnixDate, (SecsPerMinute * Min));                         { Add minutes }
  Inc(UnixDate, (SecsPerHour * Longint(Hour)));                   { Add hours }

  (*
  ** If UTC = 0, and local time is -06 hours of UTC, then
  ** UTC := UTC - (-06 * SecsPerHour)
  ** Remember that a negative # minus a negative # yields a positive value
  *)

  UnixDate := UnixDate - (GetTimeZone * SecsPerHour);            { UTC offset }
  If Day>1 then                                 { Has ONE day already passed? }
    Inc(UnixDate, (SecsPerDay * (Day-1)));

  If IsLeapYear(Year) then DaysPerMonth[02] := 29
    else DaysPerMonth[02] := 28;                        { Check for leap year }

  Counter := 01;
  if Month > 12 then Month := 12;

  If Month>01 then
    For Counter := 01 to (Month-1) do                         { Has one month }
      Inc(UnixDate, (DaysPerMonth[Counter] * SecsPerDay));   { already passed }

  While Year>1970 do
    begin
     {$IFDEF WIN32}
      try
      {$ENDIF}
        If IsLeapYear((Year-1)) then
          Inc(UnixDate, SecsPerLeapYear)
           else Inc(UnixDate, SecsPerYear);
        Dec(Year, 1);
      {$IFDEF WIN32}
        except
        end;
      {$ENDIF}
    end; { While }

  Norm2Unix := UnixDate;
End; { func. Norm2Unix }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF _NOT_TRUE_}
Procedure Unix2Norm(Date: LongInt; var Year, Month, Day, Hour, Min, Sec: Word);
var LocalDate: LongInt;
    Done     : Boolean;
    X        : ShortInt;
    TotDays  : Integer;
begin
  Year := 1970;
  Month := 01;
  Day := 01;
  Hour := 00;
  Min := 00;
  Sec := 00;

  LocalDate := Date + (GetTimeZone * SecsPerHour);          { Local time/date }

  Done := False;
  While NOT Done do
    begin;
      If LocalDate>=SecsPerYear then
        begin
          Inc(Year, 1);
          Dec(LocalDate, SecsPerYear)
        end
         else Done := True;

      If (IsLeapYear(Year+1)) AND (LocalDate>=SecsPerLeapYear) AND
          (NOT Done) then
            begin
              Inc(Year, 1);
              Dec(LocalDate, SecsPerLeapYear);
            end; { When leap year }
    End; { While }


  Month := 01;
  Day := 01;
  Done := False;

  { seems to be an issue here - TODO FIXME }
  TotDays := round(LocalDate / SecsPerDay);

  If IsLeapYear(Year) then
    begin;
      DaysPerMonth[02] := 29;
      X := 01;

      Repeat;
        If (TotDays < DaysPerLeapYear[X]) then
          begin;
            Month := X;
            Done := True;
            Dec(LocalDate, (TotDays * SecsPerDay));
            Day := Word(DaysPerMonth[Month] - (DaysPerLeapYear[Month]-TotDays) + 01);
          end
           else Done := False;

        Inc(X);
      Until (Done) or (X > 12);
  end
    else begin
          DaysPerMonth[02] := 28;
          X := 01;

          REPEAT
            If (TotDays < DaysPerYear[X]) then
              begin
                Month := X;
                Done := True;
                Dec(LocalDate,(TotDays * SecsPerDay));
                Day := Word(DaysPerMonth[Month]-(DaysPerYear[Month]-TotDays) + 1);
              end
               else Done := False;

            Inc(X);

          UNTIL (DONE) OR (X > 12);
         end; { else }

  Hour := Word(LocalDate DIV SecsPerHour);
  Dec(LocalDate, (Longint(Hour)*SecsPerHour));

  Min := Word(LocalDate DIV SecsPerMinute);
  Dec(LocalDate, (Longint(Min) * SecsPerMinute));

  Sec := Word(LocalDate);
end; { proc. Unix2Norm }
{$ENDIF}

procedure Unix2Norm(Date: LongInt; var Year, Month, Day, Hour, Min, Sec: Word);

(* Scott Little, 2004-07-04
        "Day" was always one day late, except for the first day of the month.
        Couldn't fix.  Rewrote.  Untested.
*)

var
    Done     : Boolean;
    X        : ShortInt;
    TotDays  : Integer;
begin
    Year    := 1970;
    Month   := 01;
    Day     := 01;
    Hour    := 00;
    Min     := 00;
    Sec     := 00;

    Date := Date + (GetTimeZone * SecsPerHour);          { Local time/date }

    while Date >= SecsPerYear do
        begin
        if IsLeapYear(Year) and (Date < SecsPerLeapYear) then
            break;

        if IsLeapYear(Year) and (Date >= SecsPerLeapYear) then
            begin
            Inc(Year, 1);
            Dec(Date, SecsPerLeapYear);
            end
        else
            begin
            Inc(Year, 1);
            Dec(Date, SecsPerYear);
            end;
        end;

    TotDays := Date DIV SecsPerDay;

    for X := 12 downto 1 do
        begin
        if X = 1 then
            break;

        if isLeapYear(Year) and (TotDays >= DaysPerLeapYear[X - 1]) then
            begin
            Dec(Date, DaysPerLeapYear[X - 1] * SecsPerDay);
            Dec(Totdays, DaysPerLeapYear[X - 1]);
            break;
            end
        else if (not isLeapYear(Year)) and (TotDays >= DaysPerYear[X - 1]) 
then
            begin
            Dec(Date, DaysPerYear[X - 1] * SecsPerDay);
            Dec(Totdays, DaysPerYear[X - 1]);
            break;
            end;
        end;

    Month := X;

    Day := TotDays + 1;
    Dec(Date, (Day - 1) * SecsPerDay);

    Hour := Date DIV SecsPerHour;
    Dec(Date, Hour * SecsPerHour);

    Min := Date DIV SecsPerMinute;
    Dec(Date, Min * SecsPerMinute);

    Sec := Date;

end; { proc. Unix2Norm }


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function IsToday(Date: Longint): Boolean;
var Dates: Array[1..2] of record
                            Year,
                            Month,
                            Day,
                            Dummy   : Word;
                          end;
begin
  With Dates[1] do
    Unix2Norm(Date, Year, Month, Day, Dummy, Dummy, Dummy);

  With Dates[2] do
    Unix2Norm(NowAsUnixDate, Year, Month, Day, Dummy, Dummy, Dummy);

  IsToday := FALSE;

  if (Dates[1].Year = Dates[2].Year) then
   if (Dates[1].Month = Dates[2].Month) then
    if (Dates[1].Day = Dates[2].Day) then
      IsToday := TRUE;
end; { func. IsToday }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit Unixdate }
