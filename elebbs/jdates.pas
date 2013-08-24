unit JDates;
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
** Dates (DOS alike for Delphi2 and general DateTime) routines for EleBBS
**
** Copyright (c) 1997 by Maarten Bekers
**
** parts are (c) by Scott bussinger, taken from SWAG
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses
    Debug_U,
     Global,
      UnixDate,
       GenDos,
        LongStr,
         CfgRec,
          Cases,
           Dos,
            WordStr,
             ObjDec
{$IFDEF WIN32},windows{$ENDIF}
{$IFDEF OS2}   ,os2base{$ENDIF}
{$IFDEF WINGUI}
      ,Sysutils,
       Controls
{$ENDIF};


const
  BlankDate = $FFFF;                         { Constant for Not-a-real-Date }

type TDate = Word;
     TDay = (Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday);
     TDaySet = set of TDay;
     DateTime = DOS.DateTime; { needed because of os2base inclusion }

function  Date2Str(L: LongInt): String;                  { RA-date to String }
function  Time2Str(L: LongInt): String;                  { RA-date to String }
Function  DateStr: String;                                  { Date to string }
Function  DateStrY2k: String;                { Date to string (y2k notation) }
Function  TimeStr (WithSec,WithHS : Boolean) : String;      { Time to string }
function  GetHour: Word;
function  GetSecs: Word;
function  GetMonth: Word;
function  GetYear: Word;
function  Getday: Word;
function  GetMins: Word;
function  GetUserAge(const BDate: String; UpdateAgeCache: Boolean): Byte;
function  GetDayOfWeek: String;
function  GetDosDate: Longint;
function  GetVariableDow(Year, Month, Day: Longint): Longint;
function  GetDayOfWeekCur(Dow: Longint): String;
Function  MinsTillTime(Time: String): Word; { Calculates the nr of minutes till 'time' }
Function  MinsFromTime(FromTime: String): Word;  { Calculate the time after < mins }
Function  MinsIsTime(MinsAdd: Word): String; { Calculate the time over > mins }
Function  DaysAgo(Date: String): LongInt;
function  FutureDate(B: Byte): String;
function  HistoryDate(Days: Longint): String;
Function  DaysToGo(Date: String): LongInt;
Function  GregorianToJulian(DT: DateTime): LongInt;
Function  GetDow: Word;                                { Get the Day Of Week }
function  PackTimeStr(Date, Time: String): Longint;
function  Unix2Date(UnixStamp: Longint): Longint;
function  Date2Unix(PackTime: Longint): Longint;
function  DtToUnixDate(DT: DateTime): Longint;
procedure UnixToDt(L: Longint; var Dt: DateTime);
function  MonthNameToNum(S: String): Byte;
function  UnixToRFCDate(UnixStamp: Longint; ForceTZ: String): String;
procedure NntpDateToString(    DateField   : String;
                           var MsgDateStr,
                               MsgTimeStr  : String);

function  CurrentJDate: Tdate;

function  ValidDate(Day,Month,Year: Word): boolean;
  { Check if the day,month,year is a real date storable in a Date variable }


procedure DMYtoDate(Day,Month,Year: Word;var Julian: TDate);
  { Convert from day,month,year to a date }

procedure DateToDMY(Julian: TDate;var Day,Month,Year: Word);
  { Convert from a date to day,month,year }

function BumpDate(Julian: TDate;Days,Months,Years: Integer): TDate;
  { Add (or subtract) the number of days, months, and years to a date }

function DayOfWeek(Julian: TDate): TDay;
  { Return the day of the week for the date }

function DayString(WeekDay: TDay): string;
  { Return a string version of a day of the week }

function MonthString(Month: Word): string;
  { Return a string version of a month }

function DateToStr(Julian: TDate): string;
  { Convert a date to a sortable string }

function StrToDate(StrVar: string): TDate;
  { Convert a sortable string form to a date }


{$IFDEF OS2}
function  GetOffset: SmallInt;
{$ENDIF}
{$IFDEF WIN32}
function  GetOffset: LongInt;
{$ENDIF}
function  OffsetToISO(mins: LongInt): String;
function  ISO8601: String;



(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


function FutureDate(B: Byte): String;
var NowUnix  : Longint;
    Year,
    Month,
    Day,
    Hour,
    Min,
    Sec      : Word;
begin
  NowUnix := NowAsUnixDate;

  Inc(NowUnix, SecsPerDay * B);

  Unix2Norm(NowUnix, Year, Month, Day, Hour, Min, Sec);

  FutureDate := LeadingZero(Month, 02) + '-' +
                LeadingZero(Day, 02) + '-' +
                Copy(FStr(Year), 3, 2);
end; { func. FutureDate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  HistoryDate(Days: Longint): String;
var NowUnix  : Longint;
    Year,
    Month,
    Day,
    Hour,
    Min,
    Sec      : Word;
begin
  NowUnix := NowAsUnixDate;

  Dec(NowUnix, SecsPerDay * Days);

  Unix2Norm(NowUnix, Year, Month, Day, Hour, Min, Sec);

  HistoryDate := LeadingZero(Month, 02) + '-' +
                 LeadingZero(Day, 02) + '-' +
                 Copy(FStr(Year), 3, 2);
end; { func. HistoryDate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  GetDow: Word;                                { Get the Day Of Week }
var Year, Month, Day, Dow: Word;
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logDateTime, 'GetDow'); {$endif}

  GetDate(year, month, day, dow);

  GetDow := Dow;
end; { func. Getdow }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function GregorianToJulian(DT: DateTime): LongInt;
var Century : LongInt;
    XYear   : LongInt;
    Month   : LongInt;
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logDateTime, 'GregorianToJulian');{$endif}
  Month := DT.Month;

  If Month <= 2 then
    begin
      Dec(Dt.Year);
      Inc(Month, 12);
    end; { if Month }

  Dec(Month,3);
  Century := DT.Year Div 100;
  XYear   := DT.Year Mod 100;
  Century := (Century * D1) SHR 2;
  XYear   := (XYear * D0) SHR 2;
  GregorianToJulian :=  ((((Month * 153) + 2) DIV 5) + DT.Day) + D2
                        + XYear + Century;
end; { func. GregorionToJulian }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function MinsIsTime(MinsAdd: Word): String;  { Calculate the time over > mins }
var Temp  : Longint;
    Hour,
    Mins,
    Secs  : Word;
    MSecs : Word;
begin
  GetTime(Hour, Mins, Secs, MSecs);
  Temp := Norm2Unix(GetYear, GetMonth, GetDay, Hour, Mins, Secs);

  Inc(Temp, (MinsAdd * 60));

  Unix2Norm(Temp, MSecs, MSecs, MSecs, Hour, Mins, Secs);

  MinsIsTime := LeadingZero(Hour, 2) + ':' + Leadingzero(Mins, 2);
end; { func. MinsIsTime }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function MinsFromTime(FromTime: String): Word;    { Calculate the time after < mins }
var NowHour,
    NowMins : Word;
    NowTotal: Word;
    OldHour,
    OldMins : Word;
    OldTotal: Word;
begin
  MinsFromTime := 00;

  NowHour := GetHour;
  NowMins := GetMins;
  NowTotal := (NowHour * 60) + NowMins;

  OldHour := FVal(Copy(FromTime, 1, 2));
  OldMins := FVal(Copy(FromTime, 4, 2));
  OldTotal := (OldHour * 60) + OldMins;

  if OldTotal > NowTotal then Inc(NowTotal, (60 * 24));
  MinsFromTime := Abs(NowTotal - OldTotal);
end; { func. MinsFromTime }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function DaysAgo(Date: String): LongInt;
{ date: Month-Day-Year }
var ODate  : DateTime;
    CDate  : DateTime;
    Temp   : Word;
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logDateTime, 'DaysAgo');{$endif}

  GetDate(CDate.Year, CDate.Month, CDate.Day, Temp);
  CDate.Hour := 0;
  CDate.Min  := 0;
  CDate.Sec  := 0;
  ODate.Year := FVal(Copy(Date,7,2));

  If ODate.Year < 80 then
    Inc(ODate.Year, 2000)
  else
    Inc(ODate.Year, 1900);
  ODate.Month := Word(FVal(Copy(Date,1,2)));
  ODate.Day := Word(FVal(Copy(Date, 4, 2)));
  ODate.Hour := 0;
  ODate.Min := 0;
  ODate.Sec := 0;

  DaysAgo := GregorianToJulian(CDate) - GregorianToJulian(ODate);
end; { func. Daysago }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function DaysToGo(Date: String): LongInt;
{ date: Month-Day-Year }

var ODate: DateTime;
    CDate: DateTime;
    Temp   : Word;
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logDateTime, 'DaysToGo');{$endif}

  GetDate(CDate.Year, CDate.Month, CDate.Day, Temp);
  CDate.Hour := 0;
  CDate.Min  := 0;
  CDate.Sec  := 0;
  ODate.Year := FVal(Copy(Date,7,2));

  If ODate.Year < 80 then
    Inc(ODate.Year, 2000)
  else
    Inc(ODate.Year, 1900);
  ODate.Month := FVal(Copy(Date,1,2));
  ODate.Day := FVal(Copy(Date, 4, 2));
  ODate.Hour := 0;
  ODate.Min := 0;
  ODate.Sec := 0;

  DaysTogo := GregorianToJulian(ODate) - GregorianToJulian(CDate);
end; { func. DaysToGo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetDayOfWeek: String;
var CDate: DateTime;
    Temp   : Word;
begin
  GetDate(CDate.Year, CDate.Month, CDate.Day, Temp);

  Case Temp of
    00 : GetDayOfWeek := 'Sunday';
    01 : GetDayOfWeek := 'Monday';
    02 : GetDayOfWeek := 'Tuesday';
    03 : GetDayOfWeek := 'Wednesday';
    04 : GetDayOfWeek := 'Thursday';
    05 : GetDayOfWeek := 'Friday';
    06 : GetDayOfWeek := 'Saturday';
  end; { case }

end; { func. GetDayOfWeek }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetDayOfWeekCur(Dow: Longint): String;
begin
  Case Dow of
    00 : GetDayOfWeekCur := 'Sunday';
    01 : GetDayOfWeekCur := 'Monday';
    02 : GetDayOfWeekCur := 'Tuesday';
    03 : GetDayOfWeekCur := 'Wednesday';
    04 : GetDayOfWeekCur := 'Thursday';
    05 : GetDayOfWeekCur := 'Friday';
    06 : GetDayOfWeekCur := 'Saturday';
  end; { case }
end; { func. GetDayOfWeekCur }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetUserAge(const BDate: String; UpdateAgeCache: Boolean): Byte;
Var Year, Month, Day, Dow: Word;
    TempAge              : Word;     { Nobody gets older than 65535 years :)) }
    TempMonth            : Byte;
    TempDay              : Byte;
    TempBirthYear        : Word;
begin;
  {$ifdef With_Debug}
      DebugObj.DebugLog(logDateTime, 'Getuserage');
      DebugObj.DebugLog(logDateTime, 'BirthdAte user entered: "'+BDate+'"');
  {$endif}
 GetUserAge := 00;
 if BDate = '' then EXIT;

 GetDate(Year, Month, Day, DOW);

 TempBirthYear := Word(FVal(Copy(Bdate, 7, 2)));
 TempMonth     := Byte(FVal(Copy(BDate, 1, 2)));
 TempDay       := Byte(FVal(Copy(BDate, 4, 2)));

 If TempBirthYear > 10 then Inc(TempBirthYear, 1900)
  else Inc(TempBirthYear, 2000);

 TempAge := Byte(Year - TempBirthYear);

 If TempMonth>Month then Dec(TempAge);
 If TempMonth=Month then If Day<TempDay then Dec(TempAge);

 if (UpdateAgeCache) then
   begin
     LineCfg^.UserAgeSave := Byte(TempAge);
   end; { if }

 GetUserAge := Byte(TempAge);

 {$ifdef With_Debug} DebugObj.DebugLog(logDateTime, 'End of GetUserAge');{$endif}
End; { func. GetUserAge }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function GetDosDate: Longint;
var Dt   : DateTime;
    TempL: Longint;
    Tmp  : Word;
begin
  FillChar(Dt, SizeOf(Dt), 0);
  GetDate(Dt.Year, Dt.Month, Dt.Day, Tmp);
  GetTime(Dt.Hour, Dt.Min, Tmp, Tmp);

  Packtime(Dt, TempL);
  GetDosdate := TempL;
end; { func. GetDosDate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function  PackTimeStr(Date, Time: String): Longint;
var Dt     : DateTime;
    TempL  : Longint;
begin
  PackTimeStr := -1;

  FillChar(Dt, SizeOf(Dt), 00);

  Dt.Month := FVal(Copy(Date, 1, 2));
  Dt.Day   := FVal(Copy(Date, 4, 2));
  Dt.Year  := FVal(Copy(Date, 7, 2));

  Dt.Hour  := FVal(Copy(Time, 1, 2));
  Dt.Min   := FVal(Copy(Time, 4, 2));

  if Dt.Year < 80 then Inc(Dt.Year, 2000)
    else Inc(Dt.Year, 1900);

  PackTime(Dt, TempL);

  PackTimeStr := TempL;
end; { func. PackTimeStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

Function MinsTillTime(Time: String): Word;
Const Mins_Per_day = 24 * 60;

var Hours,
    Mins,
    Secs,
    MSec        : Word;
    TimeElapsed : Word;
    TimeDue     : Word;
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logDateTime, 'MinsTillTime');{$endif}

  MinsTillTime := 00;

  GetTime(Hours, Mins, Secs, MSec);
  TimeElapsed := (Hours*60) + Mins;

  TimeDue := (FVal(Copy(Time, 1, 2))* 60) + (FVal(Copy(Time, 4, 2)));

  If TimeDue<TimeElapsed then Inc(TimeDue, Mins_Per_Day);

  MinsTillTime := TimeDue - TimeElapsed;

  If SUpCase(Time)='NONE' then MinsTillTime := 10000;
end; { func. MinsTillTime }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetHour: Word;
var H,M,S,MS: Word;
begin
  GetTime(H,M,S,MS);
  GetHour := H;
end; { func. GetHour }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetMins: Word;
var H,M,S,MS: Word;
begin
  GetTime(H,M,S,MS);
  GetMins := M;
end; { func. GetMins }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetSecs: Word;
var H,M,S,MS: Word;
begin
  GetTime(H,M,S,MS);
  GetSecs := S;
end; { func. GetSecs }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


function GetDay: Word;
var Y,D,M,Dow: Word;
begin
  GetDate(Y,M,D,Dow);
  GetDay := D;
end; { func. GetHour }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  DateStr : String;                                     { Date to string }
var Year, Month, Day, Dow: Word;
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logDateTime, 'DateStr');{$endif}
 GetDate(Year, Month, Day, Dow);

 DateStr := LeadingZero(Month, 02) + '-' +
            LeadingZero(Day, 02) + '-' +
            Copy(FStr(Year), 3, 2);

end; { func. DateStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  DateStrY2k: String;                { Date to string (y2k notation) }
var Year, Month, Day, Dow: Word;
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logDateTime, 'DateStrY2k');{$endif}
 GetDate(Year, Month, Day, Dow);

 DateStrY2k := LeadingZero(Month, 02) + '-' +
               LeadingZero(Day, 02) + '-' +
               FStr(Year);
end; { func. DateStrY2k }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


Function  GetMonth: Word;
var Y,D,M,Dow: Word;
begin
  GetDate(Y,M,D,Dow);
  GetMonth := M;
end; { func. GetMonth }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  GetYear: Word;
var Y,D,M,Dow: Word;
begin
  GetDate(Y,M,D,Dow);
  GetYear := Y;
end; { func. GetYear }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  TimeStr (WithSec,WithHS : Boolean) : String;      { Time to string }
var Hours, Minutes, Secs, MSecs: Word;
    TempStr : String;
begin
 GetTime(Hours, Minutes, Secs, MSecs);

 TempStr := LeadingZero(Hours, 02) + ':' + LeadingZero(Minutes, 02);

 If WithSec then TempStr := TempStr + ':' + LeadingZero(Secs, 02);
 If WithHS then TempStr := TempStr + '.' + LeadingZero(MSecs, 02);

 TimeStr := TempStr;
end; { func. TimeStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  CurrentJdate: Tdate;
var
 y,m,d,w: word;
 jd: TDate;
begin
  GetDate(y,m,d,w);
  DMYtoDate(d,m,y,jd);
  CurrentJDate:= jd;
end;


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ValidDate(Day,Month,Year: Word): boolean;
  { Check if the day,month,year is a real date storable in a Date variable }
begin
  if {(Day<1) or }(Year<1900) or (Year>2078) then
    ValidDate := false
  else
    case Month of
      1,3,5,7,8,10,12: ValidDate := Day <= 31;

      4,6,9,11: ValidDate := Day <= 30;
      2: ValidDate := Day <= 28 + ord((Year mod 4)=0)*ord(Year<>1900)
      else ValidDate := false
    end
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DMYtoDate(Day,Month,Year: Word;var Julian: TDate);
  { Convert from day,month,year to a date }
  { Stored as number of days since January 1, 1900 }
  { Note that no error checking takes place in this routine -- use ValidDate }
begin
if (Year=1900) and (Month<3) then
  if Month = 1 then
    Julian := pred(Day)
  else
    Julian := Day + 30
else
  begin
    if Month > 2 then
      dec(Month,3)
    else
      begin
        inc(Month,9);
        dec(Year)

      end;
    dec(Year,1900);
    Julian := (1461*longint(Year) div 4) + ((153*Month+2) div 5) + Day + 58;
  end
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DateToDMY(Julian: TDate;var Day,Month,Year: Word);
  { Convert from a date to day,month,year }
var
  LongTemp: longint;
      Temp: Word;
begin
  if Julian <= 58 then
    begin
      Year := 1900;
      if Julian <= 30 then
        begin
          Month := 1;
          Day := succ(Julian)
        end
      else
        begin
          Month := 2;
          Day := Julian - 30
        end
    end
  else
    begin
      LongTemp := 4*longint(Julian) - 233;

      Year := LongTemp div 1461;
      Temp := LongTemp mod 1461 div 4 * 5 + 2;
      Month := Temp div 153;
      Day := Temp mod 153 div 5 + 1;
      inc(Year,1900);
      if Month < 10 then
        inc(Month,3)
      else
        begin
          dec(Month,9);
          inc(Year)
        end
    end
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function BumpDate(Julian: TDate;Days,Months,Years: Integer): TDate;
  { Add (or subtract) the number of days, months, and years to a date }
  { Note that months and years are added first before days }
  { Note further that there are no overflow/underflow checks }
var Day: Word;
    Month: Word;
    Year: Word;
begin
  DateToDMY(Julian,Day,Month,Year);

  Month := Month + Months - 1;
  Year := Year + Years + (Month div 12) - ord(Month<0);
  Month := (Month + 12000) mod 12 + 1;
  DMYtoDate(Day,Month,Year,Julian);
  BumpDate := Julian + Days
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function DayOfWeek(Julian: TDate): TDay;
  { Return the day of the week for the date }
begin
  DayOfWeek := TDay(succ(Julian) mod 7)
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function DayString(WeekDay: TDay): string;
  { Return a string version of a day of the week }
const DayStr: array[Sunday..Saturday] of string[9] =
     ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');

begin
  DayString := DayStr[WeekDay]
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function MonthString(Month: Word): string;
  { Return a string version of a month }
  const MonthStr: array[1..12] of string[9] =
     ('January','February','March','April','May','June','July','August',
                                 'September','October','November','December');
begin
  if Month in [1..12] then
    MonthString := MonthStr[Month]
      else MonthString := 'Unknown';
end; { func. MonthString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function DateToStr(Julian: TDate): string;
  { Convert a date to a sortable string - NOT displayable }
const tResult: record
                case integer of
                  0: (Len: byte;  W: word);
                  1: (Str: string[2])

                end = (Str:'  ');
begin
  tResult.W := swap(Julian);
  DateToStr := tResult.Str
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function StrToDate(StrVar: string): TDate;
  { Convert a sortable string form to a date }
var Temp: record
            Len: byte;
              W: word
          end absolute StrVar;
begin
  StrToDate := swap(Temp.W)
end;


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  Date2Str(L: LongInt): String;                  { RA-date to String }
var Dt: DateTime;
begin
  UnpackTime(L, DT);
  Date2Str := Leadingzero(DT.Month, 2) + '-' +
              Leadingzero(Dt.Day, 2)   + '-' +
              Copy(FStr(Dt.Year), 3, 2);

  if L = 00 then Date2Str := '01-01-80';
end; { func. Date2Str }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  Time2Str(L: LongInt): String;                  { RA-time to String }
var Dt: DateTime;
begin
  UnpackTime(L, DT);
  Time2Str := Leadingzero(DT.Hour, 2) + ':' +
              Leadingzero(Dt.min, 2) + ':' +
              Leadingzero(Dt.Sec, 2);

  if L = 00 then Time2Str := '00:00:00';
end; { func. Time2Str }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Unix2Date(UnixStamp: Longint): Longint;
var DT   : DateTime;
    Temp : Longint;
begin
  FillChar(Dt, SizeOf(DateTime), 00);

  Unix2Norm(UnixStamp, Dt.Year, Dt.Month, Dt.Day,
                       Dt.Hour, Dt.Min, Dt.Sec);

  PackTime(Dt, Temp);

  Unix2Date := Temp;
end; { func. Unix2Date }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function MonthNameToNum(S: String): Byte;
begin
  MonthNameToNum := 0;
  S[1] := UpCase(S[1]);
  S[2] := UpCase(S[2]);
  S[3] := UpCase(S[3]);

  if S = 'JAN' then MonthNameToNum := 1
   else if S = 'FEB' then MonthNameToNum := 2
    else if S = 'MAR' then MonthNameToNum := 3
     else if S = 'APR' then MonthNameToNum := 4
      else if S = 'MAY' then MonthNameToNum := 5
       else if S = 'JUN' then MonthNameToNum := 6
        else if S = 'JUL' then MonthNameToNum := 7
         else if S = 'AUG' then MonthNameToNum := 8
          else if S = 'SEP' then MonthNameToNum := 9
           else if S = 'OCT' then MonthNameToNum := 10
            else if S = 'NOV' then MonthNameToNum := 11
             else if S = 'DEC' then MonthNameToNum := 12;
end; { func. MonthNameToNum }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Date2Unix(PackTime: Longint): Longint;
var DT   : DateTime;
begin
  UnpackTime(PackTime, DT);

  Date2Unix := Norm2Unix(Dt.Year, Dt.Month, Dt.Day,
                         Dt.Hour, Dt.Min, Dt.Sec);
end; { func. Date2Unix }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure NntpDateToString(    DateField   : String;
                           var MsgDateStr,
                               MsgTimeStr  : String);
type StampRec = record
        Day,
        Month,
        Year    : Word;
        Hour,
        Min,
        Sec     : Word;
     end; { StampRec }

var Stamp   : StampRec;
    PosCnt  : Byte;
    Dummy   : String;
begin
  FillChar(Stamp, SizeOf(Stamp), 0);
  PosCnt := Pos(',', DateField);

  if PosCnt <> 00 then
    begin
      { This can be either one of these two dates: }
      {  Friday, 19-Nov-82 16:14:55 EST }
      {  Sun, 22 Aug 1999 23:35:07 +0200 }
      { we try to treat them as the same ones by using both #32 and '-' }
      { as a seperator char }

      Stamp.Day   := FVal(ExtractWord(DateField, 2, defExtractWord + [',', '-'], false, false));
      Stamp.Month := MonthNameToNum(ExtractWord(DateField, 3, defExtractWord + [',', '-'], false, false));
      Stamp.Year  := FVal(ExtractWord(DateField, 4, defExtractWord + [',', '-'], false, false));

      Stamp.Hour  := FVal(ExtractWord(DateField, 5, defExtractWord + [':', ',', '-'], false, false));
      Stamp.Min   := FVal(ExtractWord(DateField, 6, defExtractWord + [':', ',', '-'], false, false));
      Stamp.Sec   := FVal(ExtractWord(DateField, 7, defExtractWord + [':', ',', '-'], false, false));
    end
     else begin
            { This can be either one of these two dates: }
            {  31 Aug 1999 10:40:08 GMT }
            {  Fri Nov 19 16:14:55 1982 }
            { we try to determine which one, by looking at the first char }

            Dummy := ExtractWord(DateField, 1, defExtractWord, false, false);
            if Dummy[1] in ['0'..'9'] then
              begin
                {  31 Aug 1999 10:40:08 GMT }
                Stamp.Day   := FVal(ExtractWord(DateField, 1, defExtractWord, false, false));
                Stamp.Month := MonthNameToNum(ExtractWord(DateField, 2, defExtractWord, false, false));
                Stamp.Year  := FVal(ExtractWord(DateField, 3, defExtractWord, false, false));

                Stamp.Hour  := FVal(ExtractWord(DateField, 4, defExtractWord + [':'], false, false));
                Stamp.Min   := FVal(ExtractWord(DateField, 5, defExtractWord + [':'], false, false));
                Stamp.Sec   := FVal(ExtractWord(DateField, 6, defExtractWord + [':'], false, false));
              end
                else begin
                       {  Fri Nov 19 16:14:55 1982 }
                       Stamp.Day   := FVal(ExtractWord(DateField, 3, defExtractWord, false, false));
                       Stamp.Month := MonthNameToNum(ExtractWord(DateField, 2, defExtractWord, false, false));
                       Stamp.Year  := FVal(ExtractWord(DateField, 5, defExtractWord + [':'], false, false));

                       Stamp.Hour  := FVal(ExtractWord(DateField, 4, defExtractWord + [':'], false, false));
                       Stamp.Min   := FVal(ExtractWord(DateField, 5, defExtractWord + [':'], false, false));
                       Stamp.Sec   := FVal(ExtractWord(DateField, 6, defExtractWord + [':'], false, false));
                     end;

          end; { else }

  if Stamp.Year < 100 then                 { Correct the date when necessary }
   begin
     if Stamp.Year < 80 then Inc(Stamp.Year, 2000)
       else Inc(Stamp.Year, 1900);
   end; { if }

  {------------------------ Create the date syntax --------------------------}
  MsgDateStr := LeadingZero(Stamp.Month, 2) + '-' +
                LeadingZero(Stamp.Day, 2)   + '-' +
                Copy(FStr(Stamp.Year), 3, 2);
  MsgTimeStr := LeadingZero(Stamp.Hour, 2)  + ':' +
                LeadingZero(Stamp.Min, 2)   + ':' +
                LeadingZero(Stamp.Sec, 2);
end; { proc. NntpDateToString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function UnixToRFCDate(UnixStamp: Longint; ForceTZ: String): String;
var Dt         : DateTime;
    TimeZoneStr: String;
begin
  {-- Time Zone Str --------------------------------------------------------}
  TimeZoneStr := FStr(GetTimeZone);
  if Length(TimeZoneStr) < 2 then
    Insert('0', TimeZoneStr, 2);

  if TimeZoneStr[1] = '-' then
    begin
      Insert('00', TimeZoneStr, 2);   { create -0002 }
    end
      else begin
             TimeZoneStr := '+00' + TimeZoneStr;
           end; { else }

  {-- Allow an forcable TZ -------------------------------------------------}
  if ForceTZ <> '' then
    TimeZoneStr := ForceTZ;

  {-- Unpack the Unix timestamp to a more readable format ------------------}
  Unix2Norm(UnixStamp, Dt.Year, Dt.Month, Dt.Day,
                       Dt.Hour, Dt.Min, Dt.Sec);

  {-- Now start formatting it ----------------------------------------------}
  UnixToRfcDate
         := GetDayOfWeekCur(
               GetVariableDow(Dt.Year, Dt.Month, Dt.Day)           { Sunday }
                           ) + ', ' +
            FStr(Dt.Day) + '-' +
            Copy(MonthString(Dt.Month), 1, 3) + '-' +                 { Aug }
            FStr(Dt.Year) + ' ' +                                    { 2001 }
            LeadingZero(Dt.Hour, 2) + ':' +
            LeadingZero(Dt.Min, 2) + ':' +
            LeadingZero(Dt.Sec, 2) + ' ' +
            TimeZoneStr;
end; { func. UnixToRfcDate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetVariableDow(Year, Month, Day: Longint): Longint;
var Y1, Y2: Longint;
begin { from SWAG }

 if Month < 3 then
   begin
     Inc(Month, 10);
     Dec(Year)
   end
     else Dec(Month, 2);

  {-- now start doing some calculations -------------------------------------}
  Y1 := Year Div 100;
  Y2 := Year Mod 100;

  GetVariableDow := ((Day + Trunc(2.6 * Month - 0.1) + Y2 + Y2 Div 4 + Y1 Div 4 - 2 *
           Y1 + 49) Mod 7);
end; { func. GetVariableDow }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function DtToUnixDate(DT: DateTime): Longint;
var L: Longint;
begin
  PackTime(DT, L);
  DtToUnixDate := Date2Unix(L);
end; { func. DtToUnixDate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure UnixToDt(L: Longint; var Dt: DateTime);
var Tmp: Longint;
begin
  Tmp := Unix2Date(L);
  UnpackTime(Tmp, DT);
end; { proc. UnixToDt }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WIN32}
function GetOffset: LongInt;
	{ return local UTC offset in minutes (Win32) }

var
	TzInfo: windows.TTimeZoneInformation;

begin
	if (windows.GetTimeZoneInformation(TzInfo) = windows.time_Zone_Id_Daylight)
		then GetOffset := TzInfo.Bias + TzInfo.StandardBias + TzInfo.DaylightBias
		else GetOffset := TzInfo.Bias + TzInfo.StandardBias;
end; { func. GetOffset (Win32) }
{$ENDIF}

{$IFDEF OS2}
function GetOffset: SmallInt;
	{ return local UTC offset in minutes (OS/2) }

var
	Dt: os2base.DateTime;

begin
	os2base.DosGetDateTime(Dt);
	GetOffset := Dt.TimeZone;
end; { func. GetOffset (OS/2) }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function OffsetToISO(mins: LongInt): String;
	{ Take the UTC offset (number of minutes to add to UTC to get the local
	  time) and return the number of hours and minutes to subtract from the
	  local time to get UTC.  ie. -600 becomes +1000 (Australia/Sydney) }

var
	sign: Char;
	hrs : Byte;

begin
	sign := '+';

	if (mins <= 0)
		then mins := mins * -1
		else sign := '-';

	hrs  := mins div 60;
	mins := mins - (hrs * 60);

	OffsetToISO := sign + LeadingZero(hrs, 2) + LeadingZero(mins, 2);
end; { func. OffsetToISO }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ISO8601: String;
	{ return the current date and time in ISO-8601 format }

var
	Y, Mo, D, Dw,
	H, Mi, S, Ms : Word;

begin
	GetDate(Y, Mo, D, Dw);
	GetTime(H, Mi, S, Ms);

	ISO8601 := FStr(Y) + '-' + LeadingZero(Mo, 2) + '-' + LeadingZero(D, 2) +
		'T' + LeadingZero(H, 2) + ':' + LeadingZero(Mi, 2) + ':' + LeadingZero(S, 2) + OffsetToISO(GetOffset);
end; { func. ISO8601 }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


end. { unit JDATES }
