unit TimeStmp;
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
** TIMESTMP routines
**
** Copyright (c) 1998-1999 by Maarten Bekers
**
** Created : 02-Jan-1999
** Last update : 24-Jan-1999
**
** Note: (c)1996 by Gerhard Hoogterp
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
uses
   GenDos,

{$IFDEF Win32}
   Windows,
   Sysutils,
{$ENDIF}

{$IFDEF VirtualPascal}
   VpUtils,
{$ENDIF}

{$IFDEF ELEUNIX}
 {$IFDEF VER1_0_10}
   Linux,
 {$ELSE}
   Unix,
   OldLinux,
 {$ENDIF}
{$ENDIF}

  Dos, Global;

Const GMTOffset : LongInt = -1;

type RFC822Stamp = String[35];

type Date          = LongInt;
     UnixTimeStamp = LongInt;

const WeekDays   : Array[0..6] of String[9] =
                     ('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday',
                      'Friday', 'Saturday');
      Months     : Array[1..12] of String[9] =
                     ('January', 'February', 'March', 'April', 'May',
                      'June', 'July', 'August', 'September', 'October',
                      'November', 'December');


function TimeStampRFC822(Offset: LongInt): RFC822Stamp;
function RFCDate2Long(S: String): LongInt;
function TouchFileWithStamp(FileName, TimeStamp: String): Longint;
function DayofTheWeek(PD: Date): Byte;
 { Returns the day of the week For any date  Sunday = 0 .. Sat = 6    }
 { pd = a packed date as returned by the Function PackedDate          }
 { eg...  Writeln('today is ',WeekDays[DayofTheWeek(today))];         }

function PackedDate(Year, Month, Day: LongInt): date;
 { Packs a date into a LongInt which represents the number of days since }
 { Dec 31,1899   01-01-1900 = 1                                       }

function UnixTime(Year, Month, Day, Hour, Min, Sec: LongInt): UnixTimeStamp;
 { Packs a date and time into a four Byte unix style Variable which   }
 { represents the number of seconds that have elapsed since midnight  }
 { on Jan 1st 1970.                                                   }

procedure UnPackDate(var Year, Month, Day: Word; pd : date);
 { Unpacks a LongInt returned by the Function PackedDate into its        }
 { respective parts of year, month and day                            }

procedure UnPackUnix(var Year, Month, Day, Hour, Min, Sec: Word; UTS: UnixTimeStamp);
 { Unpacks a UnixTimeStamp Variable into its Component parts.         }

function DateStr(pd: date; Format: Byte): String;
 { Unpacks a LongInt returned by the Function PackedDate into its        }
 { respective parts of year, month and day and then returns a String  }
 { Formatted according to the specifications required.                }
 { if the Format is > 9 then the day of the week is prefixed to the   }
 { returned String.                                                   }
 { Formats supported are:                                             }
 {     0:  dd/mm/yy                                                   }
 {     1:  mm/dd/yy                                                   }
 {     2:  dd/mm/yyyy                                                 }
 {     3:  mm/dd/yyyy                                                 }
 {     4:  [d]d xxx yyyy   (xxx is alpha month of 3 Chars)            }
 {     5:  xxx [d]d, yyyy                                             }
 {     6:  [d]d FullAlphaMth yyyy                                     }
 {     7:  FullAlphaMth [d]d, yyyy                                    }
 {     8:  [d]d-xxx-yy                                                }
 {     9:  xxx [d]d, 'yy                                              }

function ValidDate(Year, Month, Day: LongInt; var ErrorCode: Byte): Boolean;
 { Validates the date and time data to ensure no out of range errors  }
 { can occur and returns an error code to the calling Procedure. A    }
 { errorcode of zero is returned if no invalid parameter is detected. }
 { Errorcodes are as follows:                                         }

 {   Year out of range (< 1901 or > 2078) bit 0 of errorcode is set.  }
 {   Month < 1 or > 12                    bit 1 of errorcode is set.  }
 {   Day < 1 or > 31                      bit 2 of errorcode is set.  }
 {   Day out of range For month           bit 2 of errorcode is set.  }

procedure ParseDateString(var dStr; var Year, Month, Day: LongInt; Format : Byte);
 { Parses a date String in several Formats into its Component parts   }
 { It is the Programmer's responsibility to ensure that the String    }
 { being parsed is a valid date String in the Format expected.        }
 { Formats supported are:                                             }
 {     0:  dd/mm/yy[yy]                                               }
 {     1:  mm/dd/yy[yy]                                               }

function NumbofDaysInMth(Year, Month: LongInt): Byte;
 { returns the number of days in any month                            }
function IncrMonth(pd: date; n: LongInt): date;  { Increments pd by n months. }
function Today: Date;           { returns the number of days since 01-01-1900 }
function OrdDate (Year, Month, Day: LongInt): LongInt;     { Returns ordinal Date yyddd }
function Dateord (S: String): String;              { Returns Date as 'yymmdd' }

{$IFNDEF MSDOS}
function GetMicroSeconds: Real;
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF EleUNIX}
  uses LongStr;
{$ENDIF}


const
  tDays       : Array[Boolean, 0..12] of LongInt =
                  ((0,31,59,90,120,151,181,212,243,273,304,334,365),
                   (0,31,60,91,121,152,182,213,244,274,305,335,366));
  UnixDate    = LongInt(25568);
  SecsPerDay  = 86400;
  SecsPerHour = LongInt(3600);
  SecsPerMin  = LongInt(60);
  MinsPerHour = 60;

{$IFDEF Win32}
{$IFNDEF VirtualPascal}
const
  MicroSecondsInitialized: Boolean = false;
  MicroSecondsFrequency  : Int64 = -1;
{$ENDIF}
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FStr(Nr: LongInt): String;
var Temp   : String[10];
    Count  : Longint;
begin
  Str(Nr:2, Temp);

  For Count := 01 to Length(Temp) do
    if Temp[Count] = #32 then Temp[Count] := '0';

  FStr := Temp;
end; { func. fStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function TimeStampRFC822(Offset : LongInt):RFC822Stamp;
var Year,
    Month,
    Day,
    Dow    : Word;
    Hour,
    Min,
    Sec,
    Tenth  : Word;
    Unix   : UnixTimeStamp;
    Dum    : Date;
begin
  GetDate(Year, Month, Day, Dow);
  GetTime(Hour, Min, Sec, Tenth);

  Unix := UnixTime(Year, Month, Day, Hour, Min, Sec);

  Offset := 3600 * Offset;
  Unix := Unix + Offset;

  UnpackUnix(Year, Month, Day, Hour, Min, Sec, Unix);
  Dum := PackedDate(Year, Month, Day);
  Dow := DayOfTheWeek(Dum);

  TimeStampRFC822 := Copy(WeekDays[Dow],1,3) + ', ' +
                          FStr(Day) + ' ' +
                          Copy(Months[Month],1,3) + ' ' +
                          FStr(Year) + ' ' +
                          FStr(Hour) + ':' +
                          FStr(Min) + ':' + FStr(Sec) + ' GMT';
end; { func. TimeStampRFC 822 }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FileTimeStamp(FileName: String): String;
Var FileStamp : LongInt;
    DirInfo   : SearchRec;
    Time      : DateTime;
    Dow       : LongInt;
    Dummy     : Date;
begin
  FindFirst(FileName, AnyFile, Dirinfo);
  UnpackTime(DirInfo.Time, Time);
  {$IFNDEF MSDOS}
    FindClose(DirInfo);
  {$ENDIF}

  with Time do
    begin
      Dummy := PackedDate(Year, Month, Day);
      Dow := DayOfTheWeek(Dummy);

      FileTimeStamp := WeekDays[Dow] + ', ' +
                       FStr(Day) + '-' +
                       Copy(Months[Month], 1, 3) + '-' +
                       FStr(Year) + ' ' +
                       FStr(Hour) + ':' +
                       FStr(Min) + ':' +
                       FStr(Sec) + ' GMT';
    end; { with }
end; { func. FileTimeStamp }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function SwapSpace(S: String): String;
var Counter: Longint;
begin
  if Pos('_', S) > 00 then
    begin
      for Counter := 01 to Length(s) do
        begin
          if S[Counter] = '_' then S[Counter] := #32;
          if S[Counter] = '@' then S[Counter] := ':';
        end; { for }
    end { if }
     else begin
            for Counter := 01 to Length(s) do
              begin
                if S[Counter] = #32 then S[Counter] := '_';
                if S[Counter] = ':' then S[Counter] := '@';
              end; { for }
          end; { else }

  SwapSpace := S;
end; { func. SwapSpace }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function WhichMonth(Month: String): Byte;
var Temp: Longint;
begin
  Temp := 00;

  While (Temp <= 6) AND (Pos(Month, Months[Temp]) <> 1) do
    Inc(Temp);

  WhichMonth := Temp;
end; { func. WhichMonth }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SkipWhite(var S: String);
begin
  While (S <> '') AND (S[1] in [' ', ':', '-', ',']) do
    Delete(S, 1, 1);
end; { proc. SkipWhite }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function NextNr(var S: String): Longint;
var Temp : Longint;
  {$IFNDEF MSDOS}
    Err  : LongInt;
  {$ELSE}
    Err  : Integer;
  {$ENDIF}
    Out  : LongInt;
begin
  Temp := 01;
  While S[Temp] in ['0'..'9'] do Inc(Temp);

  Val(Copy(S, 1, Temp - 1), Out, Err);
  Delete(S, 1, Temp);

  NextNr := Out;
  SkipWhite(S);
end; { func. NextNr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function NextStr(var S: String): String;
var Temp: Longint;
begin
  Temp := 01;

  while (NOT (S[Temp] in [#32, ':', ',', '-'])) do
    Inc(Temp);

  NextStr := Copy(S, 1, Temp-1);
  Delete(S, 1, Temp);

  SkipWhite(S);
end; { func. NextStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function RFCDate2Long(S: String): LongInt;
var DumStr : String;
    PosCnt : Longint;
    Stamp  : DateTime;
begin
  FillChar(Stamp, SizeOf(Stamp), #00);
  PosCnt := Pos(',', S);

  if PosCnt = 00 then
    begin
      with Stamp do
        begin
          DumStr := NextStr(S);
          Month := WhichMonth(NextStr(S));
          Day := NextNr(S);
          Hour := NextNr(S);
          Min := NextNr(S);
          Sec := NextNr(S);
          Year := NextNr(S);
        end; { with }
    end else begin
               DumStr := NextStr(S);

               With Stamp Do
                 begin
                   Day := NextNr(S);
                   Month := WhichMonth(NextStr(S));
                   Year := NextNr(S);

                   if Year<100 then Inc(Year, 1900);
                   Hour := NextNr(S);
                   Min := NextNr(S);
                   Sec := NextNr(S);
                 end; { with }
             end; { while }

  {$IFNDEF MSDOS}
    PackTime(Stamp, Result);
  {$ENDIF}
end; { func. RFCDATE2LONG }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TouchFileWithStamp(FileName, TimeStamp: String): Longint;
var Temp_F: File;
begin
  Assign(Temp_F, FileName);
  {$i-}
    Reset(Temp_F);
    SetFTime(Temp_F, RfcDate2Long(TimeStamp));
    Close(Temp_F);
  {$i+}
  TouchFileWithStamp := IOResult;
end; { func. TouchFileWithStamp }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function DayofTheWeek(PD: Date): Byte;
begin
  DayofTheWeek := pd MOD 7;
end; { DayofTheWeek }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function IsLeapYear(Source: Word): Boolean;
begin
  IsLeapYear := (Source mod 4 = 0) AND NOT ((
                (Source mod 100 = 0) AND NOT (Source mod 400 = 0)));
end; { func. IsLeapYear }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function PackedDate(Year, Month, Day: Longint): Date;
{ Valid for all years 1901 to 2078 }
var Temp    : Longint;
    IsLeap  : Boolean;
begin
  IsLeap := IsLeapYear(Year);
  if Year >= 1900 then Dec(Year, 1900);

  Temp := Year * Longint(365) + (Year DIV 4) - Ord(IsLeap);
  Inc(Temp, tDays[IsLeap][Month - 01]);
  Inc(Temp, Day);

  PackedDate := Temp;
end; { func. PackedDate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function UnixTime(Year, Month, Day, Hour, Min, Sec: Longint): UnixTimeStamp;
{ Returns the number of seconds since 00:00 01/01/1970 }
begin
  UnixTime := SecsPerDay * (PackedDate(Year, Month, Day) - UnixDate) +
              SecsPerHour * Hour + SecsPerMin * Min + Sec;
end; { func. UnixTime }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure UnpackDate(var Year, Month, Day: Word; PD: date);
{ Valid for all years 1901 to 2078 }
var Julian   : Longint;
    IsLeap   : Boolean;
begin
  Day  := PD;
  Year := (Longint(Day) * 04) DIV 1461;
  Julian := Day - (Year * 365 + (Year DIV 4));

  Inc(Year, 1900);
  IsLeap := IsLeapYear(Year);

  Inc(Julian, Ord(IsLeap));
  Month := 00;

  While Julian > tDays[IsLeap][Month] do
    Inc(Month);

  Day := Julian - tDays[IsLeap][Month - 01];
end; { proc. UnPackDate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure UnpackUnix(var Year, Month, Day, Hour, Min, Sec: Word;
                     Uts: UnixTimeStamp);
var Temp: UnixTimeStamp;
begin
  UnpackDate(Year, Month, day, Date(UTS div SecsPerDay) + UnixDate);

  Temp   := UTS MOD SecsPerDay;
  Hour   := Temp DIV SecsPerHour;
  Min    := (Temp MOD SecsPerHour) DIV MinsPerHour;
  Sec    := Temp MOD SecsPerMin;
end; { proc. UnPackUnix }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function DateStr(PD: Date; Format: Byte): String;
var Year,
    Month,
    Day      : Word;
    YearStr  : String[5];
    MonthStr : String[11];
    DayStr   : String[8];
    TempStr  : String[5];
begin
  UnpackDate(Year, Month, Day, PD);
  Str(Year, YearStr);
  Str(Month, MonthStr);
  Str(Day, DayStr);

  TempStr := '';
  DateStr := '';

  if Format > 9 then
      TempStr := Copy(WeekDays[DayofTheWeek(PD)], 1, 3) + ' ';

  if (Format MOD 10) < 4 then
    begin
      if Month < 10 then MonthStr := '0' + MonthStr;
      if Day < 10 then DayStr := '0' + DayStr;
    end; { if }

  Case (Format MOD 10) of                     { force Format to a valid value }
      00 : DateStr := TempStr + DayStr + '/' + MonthStr + '/' + Copy(YearStr, 3, 2);
      01 : DateStr := TempStr + MonthStr + '/' + DayStr + '/' + Copy(YearStr, 3, 2);
      02 : DateStr := TempStr + DayStr + '/' + MonthStr + '/' + YearStr;
      03 : DateStr := TempStr + MonthStr + '/' + DayStr + '/' + YearStr;
      04 : DateStr := TempStr + DayStr + ' ' + Copy(Months[Month], 1, 3) + ' ' + YearStr;
      05 : DateStr := TempStr + Copy(Months[Month], 1, 3) + ' ' + DayStr + ' ' + YearStr;
      06 : DateStr := TempStr + DayStr + ' ' + Months[Month] + ' ' + YearStr;
      07 : DateStr := TempStr + Months[Month] + ' ' + DayStr + ' ' + YearStr;
      08 : DateStr := TempStr + DayStr + '-' + Copy(Months[Month], 1, 3) + '-' + Copy(YearStr, 3, 2);
      09 : DateStr := TempStr + Copy(Months[Month], 1, 3) + ' ' + DayStr+ ', ''' + Copy(YearStr, 3, 2);
  end;  { Case }
end; { DateStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ValidDate(Year, Month, Day: Longint; var ErrorCode: Byte): Boolean;
begin
  ErrorCode := 00;

  if (Year < 1901) OR (Year > 2078) then ErrorCode := (ErrorCode OR 01);
  if (day < 1) OR (Day > 31) then ErrorCode := (ErrorCode OR 02);
  if (Month < 1) OR (Month > 12) then ErrorCode := (ErrorCode OR 04);

  Case Month of
    4, 6, 9, 11 : if Day > 30 then ErrorCode := (ErrorCode OR 02);
    2           : if Day > (28 + Ord(IsLeapYear(Year))) then
                      ErrorCode := (ErrorCode OR 02);
  end; { case }

  ValidDate := (errorcode = 0);
end; { func. ValidDate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ParseDateString(var dStr; var Year, Month, Day: Longint; Format : Byte);
var Left,
    Middle    : Longint;
    ErrorCode : Longint;
    Str       : String ABSOLUTE dStr;
begin
{$IFNDEF MSDOS}
  Val(Copy(Str, 1, 2), Left, Errorcode);
  Val(Copy(Str, 4, 2), Middle, Errorcode);
  Val(Copy(Str, 7, 4), Year, ErrorCode);
{$ENDIF}

  Case Format of
     00: begin
           Day := Left;
           Month := Middle;
         end;
     01: begin
           Day := Middle;
           Month := Left;
         end;
    end; { Case }
end; { proc. ParseDateString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function NumbofDaysInMth(Year, Month: LongInt): Byte;
{ Valid For the years 1901 - 2078 }
begin
  Case Month of
    1,3,5,7,8,10,12: NumbofDaysInMth := 31;
    4,6,9,11       : NumbofDaysInMth := 30;
    2              : NumbofDaysInMth := 28 + Ord(IsLeapYear(Year));
  end; { case }
end; { func. NumbofDaysInMth }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function IncrMonth(PD: Date; N: Longint): Date;
var Year,
    Month,
    Day     : Word;
begin
  UnpackDate(year, month, day, Pd);

  Dec(Month);
  Inc(Month, n);
  Inc(Year, Month DIV 12);                 { if necessary, increment the year }

  Month := Succ(Month MOD 12);
  if Day > NumbOfDaysInMth(Year, Month) then
        Day := NumbOfDaysInMth(Year, Month);

  IncrMonth := PackedDate(Year, Month, day);
end; { func. IncrMonth }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Today: Date;
var Year,
    Month,
    Day,
    Dow   : Word;
begin
  GetDate(year, month, day, dow);
  Today := PackedDate(year, month, day);
end; { func. Today }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function OrdDate(Year, Month, Day: Longint): Longint;
{ Returns ordinal Date as yyddd }
var IsLeap : Boolean;
    Temp   : Longint;
begin
  IsLeap := IsLeapYear(Year);

  Dec(Year, 1900);
  Temp := Longint(Year) * 1000;
  Inc(Temp, tDays[IsLeap][Month - 01]);   { Compute # days through last month }
  Inc(Temp, Day);                                         { # days this month }

  OrdDate := Temp;
end; { func. ordDate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Dateord (S: String): String;             { Returns Date as 'yymmdd' }
var IsLeap    : Boolean;
    Year,
    Month,
    Day       : Longint;
    Temp      : LongInt;
    ErrorCode : LongInt;
    SW,ST     : String[6];
begin
{$IFNDEF MSDOS}
  Val(Copy(S, 1, 2), Year, ErrorCode);
  Val(Copy(S, 3, 3), Temp, ErrorCode);
{$ENDIF}

  Inc(Year, 1900);
  IsLeap := IsLeapYear(Year);
  Dec(Year, 1900);

  ErrorCode := 00;
  While (tDays[IsLeap][ErrorCode] < Temp) do
    Inc(ErrorCode);

  Month := ErrorCode;
  Day := Temp - tDays[IsLeap][Month - 01];

  Str(Year: 2, Sw);

  Str(Month:2, St);
  if St[01] = #32 then St[01] := '0'; SW := SW+ST;

  Str(Day:2, St);
  if St[01] = #32 then St[01] := '0'; SW := SW+ST;

  DateOrd := SW;
end;  { Dateord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF FPC}
  {$IFDEF Win32}  
  function QueryPerformanceCounter(var lpPerformanceCount: int64): BOOL; stdcall; external 'kernel32';
  function QueryPerformanceFrequency(var lpFrequency: int64): BOOL; stdcall; external 'kernel32';
  {$ENDIF}
{$ENDIF}


{$IFNDEF MSDOS}
function GetMicroSeconds: Real;
{$IFDEF Win32}
 {$IFNDEF VirtualPascal}
 var Frequency: int64;
 {$ENDIF}
{$ENDIF}

{$IFDEF ELEUNIX}
 var TV: TimeVal;
{$ENDIF}

{$IFDEF VirtualPascal}
var Hour,
    Min,
    Sec,
    Tenth  : Word;
{$ENDIF}
begin
  Result := -1;

  {$IFDEF Win32}
   {$IFNDEF VirtualPascal}
    if NOT MicroSecondsInitialized then
      begin
        if QueryPerformanceFrequency(Frequency) then
          begin
            MicroSecondsInitialized := TRUE;
            MicroSecondsFrequency := Frequency;
          end; { if }
      end; { if }

     if QueryPerformanceCounter(Frequency) then
      if MicroSecondsFrequency <> 0 then
       begin
         Result := Frequency / MicroSecondsFrequency;
       end; { if }
   {$ELSE}
    GetMicroSeconds := GetTimeMsec;
   {$ENDIF}
  {$ENDIF}

  {$IFDEF UNIX}
    GetTimeOfDay(Tv);
    Result := StrToReal(FStr(Tv.sec) + '.' + FStr(tv.usec));
  {$ENDIF}

  {$IFDEF OS2}
    GetTime(Hour, Min, Sec, Tenth);
    GetMicroSeconds := ((Hour * 3600) + (Min + 60) + (Sec)) + (Tenth / 10);
  {$ENDIF}
end; { func. GetMicroSeconds }
{$ENDIF}
end. { unit TIMESTMP }
