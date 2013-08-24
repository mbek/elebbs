unit RAL;
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
** RAL (RemoteAccess language) Routines for EleBBS
**
** Copyright (c) 1996, 1997 by Maarten Bekers
**
** Created : 29-Dec-1996
** Last update : 24-Sep-1997
**
**
** Note: A RAL consists as follows:
**
**   Entries:  2 bytes (WORD)   [* 1]        Number of prompts following
**   Offsets:  2 bytes (WORD)   [* Entries]  Get entry-number
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Global, CfgRec, FileObj;

type
  LangString = ^String;
  DefLangRec = record
                 TextStr    : LangString;
                 HasDefault : Boolean;
                 DefaultKey : Char;
                 Keys       : String[7];
                 TxtColor   : Byte;
               end; { DefLangRec }

const
  defRalSize    = 715;

type
  ralArray_Ptr        = Array[0..defRalSize] of SmallWord;
  DefaultLangArrayRec = Array[0..defRalSize] of DefLangRec;

type
  TempRecRec = record
                 Dest : Array[0..((16 * 1024) - 01)] of Byte;
                 Start: Longint;
               end; { record }

type tLanguageObj = Object
         FailedInitialize: Boolean;
         RalCodes        : Boolean;
         UsingFile       : Boolean;

         SaveRalName     : String ;
         RalFile         : pFileObj;
         RalArray        : ^ralArray_Ptr;       { PTR to current RAL contents }
         TempRecord      : ^TempRecRec;
         ralSizeFile     : Longint;
         defLangArray    : ^DefaultLangArrayRec;
         OverrideRalFunc : function(nr: Longint): Shortstring;

         {-- Constructors and destructors ----------------------------------}
         constructor Init;
         destructor Done;

         {-- Generic routines ----------------------------------------------}
         function RalGet (Nr : SmallWord) : String; virtual; { Get entry from .RAL file }
         function ralGetFileSize: Longint;
         procedure ResetCache;
         function RalEntries: SmallWord; virtual;    { Number of entries in RAL file }
         function RalGetKey(Nr : SmallWord) : Char; virtual; { Get key for Ral entry }
         function RalGetKeys(Nr : SmallWord) : String; virtual; { Get keys for Ral entry }
         function RalGetStr(Nr : SmallWord) : String; virtual; { Get text from RAL entry }
         function GetRalOffSet(W: Longint): Byte;
         function RalGetTitleInfo: String; virtual; { Get title info entry from .RAL file }
         function RalGetDsp(Nr : SmallWord) : String;    { Get [key] tekst from RAL entry }
         function RalGetDefaultKey(Nr: SmallWord): String;
         function RalHasKey(Nr: SmallWord): Boolean;
         function RalGetColor(Nr: SmallWord): Byte;
         procedure RalGetAll(const Entry: Word; var Color: Byte; var Text, Keys: String; var DefaultKey: Char; HasDefault,
                             HasKeys: Boolean);

         {-- More generic routines -------------------------------------------}
         function Sex2Str(B:Byte; CurLanguage: Boolean): String;
         function LangBool2Str(B: Boolean): String;
         function Bool2YesStr(B: Boolean): String;
         function ReadRalFile(Name: String): Boolean;      { Opens and reads NAME.RAL }
         function RaFormatDate (D: String;
                                Format : Byte;
                                YearType: y2k_YearType;
                                defFormatType: Byte): String;  { Format Date var }
         function DayOfWeek(Full: Boolean): String;
         function Byte2DateFormat(B: Byte; CurLanguage: Boolean): String;
     end; { tLanguageObj }

type pLanguageObj =  ^tLanguageObj;

  {$I rallst.pas}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Sysutils, MemMan, Debug_U, GenDos, Dos,
      Ranges, LongStr, CfgFile, ElLog_U,
       StUtils, ObjDec;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tLanguageObj.Init;
begin
  RalArray := NIL;
  RalFile := NIL;
  TempRecord := NIL;
  FailedInitialize := false;
  RalCodes := TRUE;
  OverrideRalFunc := NIL;
  UsingFile := FALSE;
  SaveRalName := 'english.ral';

  if NOT AllocMem(TempRecord, SizeOf(TempRecRec), 'TempRecRec', 'RAL.Init') then
   begin
     FailedInitialize := true;
   end; { if }

  if NOT AllocMem(RalArray, SizeOf(RalArray_ptr), 'RalArray', 'Ral.Init') then
   begin
     FailedInitialize := true;
   end; { if }

  TempRecord^.Start := -1;
end; { LanguageObj.Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tLanguageObj.Done;
begin
  UsingFile := FALSE;
  if RalFile <> nil then
     Dispose(RalFile, Done);

  ReleaseMem(TempRecord, SizeOf(TempRecRec));
  ReleaseMem(RalArray, SizeOf(RalArray^));

  RalArray := nil;
  TempRecord := nil;
  RalFile := nil;
end; { LanguageObj.Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tLanguageObj.ResetCache;
begin
  TempRecord^.Start := -1;
end; { proc. LanguageObj.ResetCache }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tLanguageObj.GetRalOffSet(W: Longint): Byte;
begin
  {$i-}
    if W > RalFile^.FilEsize then W := RalFile^.FileSize;
  {$i+}
  if RalFIle^.IOResult > 00 then EXIT;

  if (InRange(W, TempRecord^.Start, TempRecord^.Start + (SizeOf(TempRecord^.Dest)-2)))
       AND (W - TempRecord^.Start >= 00)
        AND (TempRecord^.Start >= 00) then
    begin
      GetRalOffset := Byte(TempRecord^.Dest[W - TempRecord^.Start]);
      EXIT;
    end; { if }

  {$i-}
    RalFile^.Seek(w);
    if RalFile^.ioresult > 00 then;

    if RalFile^.FilePos + SizeOf(TempRecord^.Dest) > RalFile^.FileSize then
      RalFile^.Seek(Max(0, RalFile^.FileSize - (SizeOf(TempRecord^.Dest)-01)));

    TempRecord^.Start := RalFile^.FilePos;
    RalFile^.BlkRead(TempRecord^.Dest[0], Min(SizeOf(TempRecord^.Dest) - 01, RalFile^.FileSize - RalFile^.FilePos));
  {$i+}
  if RalFile^.IOResult>00 then ;

  if InRange(W - TempRecord^.Start, 0, SizeOf(TempRecord^.Dest)) then
    GetRalOffset := TempRecord^.Dest[W - TempRecord^.Start]
     else GetRalOffSet := TempRecord^.Start;
end; { func. GetRallOffset }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function tLanguageObj.RalGetColor(Nr: SmallWord): Byte;
var Offset  : SmallWord;
    RalByte : Byte;
begin
  RalGetColor := 00;
  if Nr > ralEntries then EXIT;

  OffSet := RalArray^[Nr];

  If Offset>00 then
    begin
       Dec(OffSet, Pred(Hi(OffSet)));

       RalByte := GetRalOffset(Offset-01);
       If NOT (ralByte in [00,255]) then
         RalGetColor := ralByte;
    end; { If Offset>00 }
end; { func. RalGetColor }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function tLanguageObj.RalGet (Nr : SmallWord) : String;            { Get entry from .RAL file }
var OffSet      : SmallWord;
    Temp        : String;
    TempStr     : String;
    RalByte     : Byte;
    TotalEntries: SmallWord;
    EndPrompt   : Longint;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logRAL, '(RAL) Getting: '+FStr(nr) + ' (begin)');
  {$ENDIF}

 RalGet := '';

 if @OverrideRalFunc <> nil then
   begin
     RalGet := OverrideRalFunc(Nr);
   end; { if }

 if (Nr=00) OR (NR > DefRalSize) OR (NR > ralEntries) then EXIT;

 OffSet := RalArray^[Nr];
 Temp   := '';

 If Offset>00 then
   begin;
      Dec(OffSet, Pred(Hi(OffSet)));

      RalByte := GetRalOffset(Offset - 01);
      If RalCodes then
       If NOT (ralByte in [00,255]) then
        begin;
          Temp := ^K'['+ToHex(ralByte);                 { Get color string }
        end; { if }

      TotalEntries := ralEntries;

      if Nr >= ralEntries then
        EndPrompt := ralGetFileSize
         else EndPrompt := (RalArray^[Nr+1] - Hi(RalArray^[Nr+1]));

      While {NOT (ralArray^.ByteArray[Offset] in [00]) AND}
             (OffSet <> EndPrompt) do
       begin;
         If Nr=TotalEntries then
          If OffSet=ralGetFileSize then Break;

         RalByte := GetRalOffset(Offset);
         Temp := Temp + Chr(Ralbyte);

         If RalByte = 255 then break;

         Inc(OffSet);
       End; { While }
   end; { If Offset>00 }

 { When next is valid response, last-char is #255 }

 RalGet := Temp;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logRAL, '(RAL) Getting: '+FStr(nr) + ' ( end )');
  {$ENDIF}
end; { func. Ralget }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function tLanguageObj.RalGetTitleInfo: String;      { Get title info entry from .RAL file }
var OffSet: SmallWord;
{$IFNDEf WIN32}
{$IFNDEF FPC}
 {$IFNDEF OS2}
    Result: String;
 {$ENDIF}
{$ENDIF}
{$ENDIF}
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logRAL, '(RAL): GetTitleInfo');{$endif}

 OffSet := 2 * ralEntries + 2;
 Result := '';

 While {(ralArray^.ByteArray[Offset]<>00) AND}
        (OffSet <> (RalArray^[00+1] - Hi(OffSet))) do
  begin;
   Result := Result + Chr(GetRalOffset(OffSet));
   Inc(OffSet);
  End; { While }

 RalGetTitleInfo := result;
end; { func. RalgetTitleInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function tLanguageObj.ralGetFileSize: Longint;
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logRAL, '(RAL) GteFileSize');{$endif}
  ralGetFileSize := ralSizeFile;
end; { func. ralFileSize }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function tLanguageObj.RalEntries: SmallWord;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logRAL, '(RAL) GetEntries');
  {$ENDIF}
  RalEntries := ralArray^[0];
{  RalEntries := defRalSize; }
end; { func.  RalEntries }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function tLanguageObj.RalGetKey(Nr : SmallWord) : Char;               { Get key for Ral entry }
var Temp        : String;
    OldralCodes : Boolean;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logRAL, '(RAL) ralGetKey '+FStr(NR) + ' (begin)');
  {$ENDIF}

  OldRalCodes := ralCodes;
  RalCodes := False;

  Temp := Copy(RalGet(Nr), 1, 1);

  RalGetKey := UpCase(Temp[1]);

  RalCodes := OldRalCodes;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logRAL, '(RAL) ralGetKey '+FStr(NR) + ' ( end )');
  {$ENDIF}
end; { func. RalGetKey }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function tLanguageObj.RalGetKeys(Nr : SmallWord) : String;            { Get key for Ral entry }
var Temp: String;
    OldralCodes : Boolean;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logRAL, '(RAL) ralGetKeys '+Fstr(nr) + ' (begin)');
  {$ENDIF}

  OldRalCodes := ralCodes;
  RalCodes := False;

  Temp := RalGet(Nr);
  Temp := Copy(Temp, 1, Pos(' ', Temp));

  While (Length(Temp)>00) AND (Temp[1]=' ') do Delete(Temp, 1, 1);
  While (Length(Temp)>00) AND (Temp[Length(Temp)]=' ') do Delete(Temp, Length(Temp), 1);

  RalGetKeys := Temp;

  RalCodes := OldRalCodes;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logRAL, '(RAL) ralGetKeys '+FStr(NR) + ' ( end )');
  {$ENDIF}
end; { func. RalGetKey }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function tLanguageObj.RalGetDefaultKey(Nr: SmallWord): String;
var LanStr: String;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logRAL, '(RAL) ralGetDefaultKey '+FStr(nr) + ' (begin)');
  {$ENDIF}

  LanStr := RalGet(Nr);
  RalGetDefaultKey := #00;

  RalGetDefaultKey := LanStr[Length(LanStr)];

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logRAL, '(RAL) ralGetDefaultKey '+FStr(nr) + ' ( end )');
  {$ENDIF}
end; { func. ralGetDefaultKey }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function tLanguageObj.RalHasKey(Nr: SmallWord): Boolean;
var LanStr: String;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logRAL, '(RAL) ralHasKey '+FStr(nr) + ' (begin)');
  {$ENDIF}

  LanStr := RalGet(Nr);
  RalHasKey := (LanStr[Length(LanStr)]=#255);

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logRAL, '(RAL) ralHasKey '+FStr(nr) + ' ( end )');
  {$ENDIF}
end; { func. RalHasKey }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function tLanguageObj.RalGetStr(Nr : SmallWord) : String;           { Get text from RAL entry }
var LanStr: String;
    ColStr: String;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logRAL, '(RAL) ralGetStr '+FStr(nr) + ' (begin)');
  {$ENDIF}

  LanStr := RalGet(Nr);
  ColStr := '';

  If LanStr[Length(LanStr)]=#255 then
     begin;
       If LanStr[1]=^K then                                     { Color String }
           ColStr := Copy(LanStr, 1, 4);

       LanStr := Copy(LanStr, Succ(Pos(' ', LanStr)), Length(LanStr));
       SetLength(LanStr, Length(LanStr) - 1);
     end; { Is String with key }

  RalGetStr := ColStr + LanStr;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logRAL, '(RAL) ralGetStr '+FStr(nr) + ' ( end )');
  {$ENDIF}
end; { func. RalGetStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function tLanguageObj.RalGetDsp(Nr : SmallWord) : String;    { Get [key] tekst from RAL entry }
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logRAL, '(RAL) ralGetDsp '+Fstr(nr));
  {$ENDIF}

  With GlobalCfg^.RaConfig^ do
   RalGetDsp := LeftBracket + RalGetKey(Nr) + RightBracket +
                ' ' + RalGetStr(nr);
end; { func. RalGetDsp }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function tLanguageObj.Sex2Str(B:Byte; CurLanguage: Boolean): String;
begin
 If CurLanguage then
  Case B of
       1 : Sex2Str := ralGet(ralMale);
       2 : Sex2Str := ralGet(ralFemale);
      else Sex2Str := ralGet(ralUnknown);
  End; { Case }

 If NOT CurLanguage then
  Case B of
       1 : Sex2Str := 'Male   ';
       2 : Sex2Str := 'Female '
         else Sex2Str:='Unknown';
  End; { Case }
End; { func. Sex2Str }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

Function tLanguageObj.LangBool2Str(B: Boolean): String;
begin
  if UsingFile then
    begin
      If B then LangBool2Str := ralGet(ralOn1)
       else LangBool2Str := ralGet(ralOff);
    end
      else begin
             if B then LangBool2Str := 'ON'
               else LangBool2Str := 'OFF';
           end; { if }
end; { func. Bool2Str }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

Function tLanguageObj.Bool2YesStr(B: Boolean): String;
begin
 If B then Bool2YesStr := ralGetStr(ralYes)
  else Bool2YesStr := ralGetStr(ralNo);
end; { func. BoolYes2Str }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function tLanguageObj.RaFormatDate (D: String;
                                    Format : Byte;
                                    YearType: y2k_YearType;
                                    defFormatType: Byte) : String;  { Format Date var }
const MonthNames : Array[1..13] of String[3] =
                                 ('Jan', 'Feb', 'Mar', 'Apr', 'May',
                                  'Jun', 'Jul', 'Aug', 'Sep', 'Oct',
                                  'Nov', 'Dec', '???');
var TempDate : String;
    Days     : String;
    Months   : String;
    Years    : String;
    YearNr   : Word;
    DaysNr   : Word;
    MonthsNr : Word;
{$IFNDEF WIN32}
    Code     : Integer;
{$ELSE}
    Code     : Longint;
{$ENDIF}
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logString, 'RaFormatDate ('+D+') (BEGIN)');
  {$ENDIF}

  RaFormatDate := '';
  TempDate := '';
  if D='' then EXIT;

  if Format=00 then
    Format := defFormatType;
  if (Format < 01) OR (Format > 08) then Format := 05;

  Months:= Copy(D, 1, 2);
  Days  := Copy(D, 4, 2);

  if Format in [1..4] then
    YearNr := FVal(Copy(D, 7, 2))
     else YearNr := FVal(Copy(D, 7, 4));

  if Format in [5..8] then
    Case YearType of
      y2k_UserDate : if YearNr < y2k_UserFrame then Inc(YearNr, 2000)
                       else Inc(YearNr, 1900);
      y2k_FileDate : if YearNr < y2k_FileFrame then Inc(YearNr, 2000)
                       else Inc(YearNr, 1900);
      y2k_MsgDate  : if YearNr < y2k_MessageFrame then Inc(YearNr, 2000)
                       else Inc(YearNr, 1900);
      y2k_SysDate  : if YearNr < y2k_SysFrame then Inc(YearNr, 2000)
                       else Inc(YearNr, 1900);
    end; { case }

  Val(Days, DaysNr, Code);
  Val(Months, MonthsNr, Code);
  Str(YearNr, Years);

  if Length(Years) < 2 then Years := '0' + Years;
  if (DaysNr=00) OR (MonthsNr=00) then EXIT;
  if (MonthsNr < 01) OR (MonthsNr > 12) then MonthsNr := 13;

  case Format of
 { DD-MM-YY    }  01 : TempDate := Days   + '-' +Months + '-' + Years;
 { MM-DD-YY    }  02 : TempDate := Months + '-' +Days   + '-' + Years;
 { YY-MM-DD    }  03 : TempDate := Years  + '-' +Months + '-' + Days;
 { DD-Mmm-YY   }  04 : if UsingFile then
                         TempDate := Days + '-' + RalGet(ralJan+(MonthsNr-1))+'-'+Years
                          else TempDate := Days + '-' +
                                          MonthNames[MonthsNr] + '-' +
                                          Years;
 { DD-MM-YYYY  }  05 : TempDate := Days   + '-' +Months + '-' + Years;
 { MM-DD-YYYY  }  06 : TempDate := Months + '-' +Days   + '-' + Years;
 { YYYY-MM-DD  }  07 : TempDate := Years  + '-' +Months + '-' + Days;
 { DD-Mmm-YYYY }  08 : if UsingFile then
                         TempDate := Days + '-' + RalGet(ralJan+(MonthsNr-1))+'-'+Years
                          else TempDate := Days + '-' +
                                          MonthNames[MonthsNr] + '-' +
                                          Years;
  end; { case format }

  RaFormatDate := TempDate;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logString, 'RaFormatDate ('+D+') ( end )');
  {$ENDIF}
end; { func. RaFormatDate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function tLanguageObj.DayOfWeek(Full: Boolean): String;
var DT   : DateTime;
    Temp : String;
    Dow  : Word;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'DayOfWeek');
  {$ENDIF}

  Temp := '';

  GetDate(DT.Year, DT.Month, DT.Day, Dow);

  {$IFDEF WITH_FULL}
    Case Dow of
      00 : Temp := ralGet(ralSunday);
      01 : Temp := ralGet(ralMonday);
      02 : Temp := ralGet(ralTuesday);
      03 : Temp := ralGet(ralWednesday);
      04 : Temp := ralGet(ralThursday);
      05 : Temp := ralGet(ralFriday);
      06 : Temp := ralGet(ralSaturday);
    end; { case }
  {$ELSE}
    Case Dow of
      00 : Temp := 'Sunday';
      01 : Temp := 'Monday';
      02 : Temp := 'Tuesday';
      03 : Temp := 'Wednesday';
      04 : Temp := 'Thursday';
      05 : Temp := 'Friday';
      06 : Temp := 'Saturday';
    end; { case }
  {$ENDIF}

   If Full then DayOfWeek := Temp
     else DayOfWeek := Copy(Temp, 1, 3);
end; { func. DayOfWeek }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function tLanguageObj.Byte2DateFormat(B: Byte; CurLanguage: Boolean): String;
begin
  if (NOT (B in [1..8])) then B := 01;

  If CurLanguage then
    begin
      if B in [1..4] then Byte2DateFormat := ralGet(476+B)
        else Byte2DateFormat := ralGet(681+B);
    end
     else begin

            Case B of
              01 : Byte2DateFormat := 'DD-MM-YY   ';
              02 : Byte2DateFormat := 'MM-DD-YY   ';
              03 : Byte2DateFormat := 'YY-MM-DD   ';
              04 : Byte2DateFormat := 'DD-Mmm-YY  ';
              05 : Byte2DateFormat := 'DD-MM-YYYY ';
              06 : Byte2DateFormat := 'MM-DD-YYYY ';
              07 : Byte2DateFormat := 'YYYY-MM-DD ';
              08 : Byte2DateFormat := 'DD-Mmm-YYYY';
            end; { Case }
          end; { NOT CurLanguage }
end; { func. Byte2DateFormat }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tLanguageObj.RalGetAll(const Entry: Word; var Color: Byte; var Text, Keys: String;
                                 var DefaultKey: Char; HasDefault, HasKeys: Boolean);
var TempStr: String;
begin
  if Entry > ralEntries then EXIT;

  Color := RalGetColor(Entry);
  TempStr := RalGet(Entry);

  if HasDefault then
    begin
      DefaultKey := TempStr[Length(TempStr)];
    end; { if }

  if HasKeys then
    begin
      Keys := Copy(TempStr, 1, Pos(' ', TempStr));

      While (Length(Keys)>00) AND (Keys[1]=' ') do Delete(Keys, 1, 1);
      While (Length(Keys)>00) AND (Keys[Length(Keys)]=' ') do Delete(Keys, Length(Keys), 1);

      TempStr := Copy(TempStr, Succ(Pos(' ', TempStr)), Length(TempStr));
      SetLength(TempStr, Length(TempStr) - 1);
    end; { if }

  Text := TempStr;
end; { proc. RalGetAll }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tLanguageObj.ReadRalFile(Name: String): Boolean;      { Opens and reads NAME.RAL }
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMain, 'ReadRalFile: '+Name);
  {$ENDIF}

  if RalFile <> nil then Dispose(RalFile, Done);
  ralSizeFile := 00;
  FillChar(ralArray^, SizeOf(ralArray^), #00);
  ReadRalFile := FALSE;

  if Config_OpenFile(RalFile, Name, 01, ReadMode + DenyNone, False, False)>00 then
   begin;
     RaLog('!', 'Could not open language definition file '+Name);
     EXIT;
   end; { if }

  if Config_ReadFile(RalFile, RalArray^, SizeOf(ralArray^))>00 then
   begin;
     RaLog('!', 'Error reading language definition file '+Name);
     EXIT;
   end; { if }

  ralSizeFile := RalFile^.FileSize;
  UsingFile := TRUE;
  ReadRalFile := true;
  SaveRalName := Name;
  ResetCache;
end; { proc. ReadRalFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit Ral }
