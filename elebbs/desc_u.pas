unit Desc_U;
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
** Description support routines for EleBBS
**
** Copyright (c) 1996-1999 by Maarten Bekers
**
** Created : 28-Feb-1998
** Last update : 30-Dec-1998
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 uses Global, CfgRec, Ra2Fdb, StrUnit;
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Descript2Array(const TempDesc   : TxtRec;
                         const MaxLen     : Byte;
                         var   Description: StringArrayObj;
                         var   Desclines  : Byte;
                               Commenting : Boolean);
procedure GetDescriptionPos(var DescPos: Byte; const ListFormat: String; UseRAL: Boolean; var UseCR: Boolean);
function  Is_Newfile(UploadDate: Longint; const LastDate: String): Boolean;
function  ShowFile(ShowType: FileShowType; const IdxInf: FilesIdxRecord;
                   const TxtInf: TxtRec;
                   SearchID: String; QScan: Boolean;
                   LongFName: String): Boolean;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses SysUtils, Dos, GenDos, StrPath, Debug_U, LongStr, Ranges, Strings, Cases,
     WordStr, Crc_Unit, UnixDate, GenFile, Formlst, Colors, Memman,
     ObjDec;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Descript2Array(const TempDesc   : TxtRec;
                         const MaxLen     : Byte;
                         var   Description: StringArrayObj;
                         var   Desclines  : Byte;
                               Commenting : Boolean);

Const NextWordChars : CharSet = [' ', ',', '.', ')', ']', '_', '!', '?', '*', #10, #00];

var TempStr  : String;
    WordStr  : String;
    Counter  : Word;
    TempArray: Array[0..255] of char;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'Descript2array');
  {$ENDIF}
  Description.Clear;
  DescLines := 00;

  TempStr := '';
  WordStr := '';
  Counter := 00;

  if NOT Commenting then
  begin;
   While TempDesc[Max(0, Max(1, Counter)-1)] <> #00 do
     begin
       If TempDesc[Counter] IN [#32..#255] then
             WordStr := WordStr + TempDesc[Counter];

{ WordStr contains the current word. When word is finished, EleBBS determines }
{ what to do, add it to the end of the description of start a new line }
         If (TempDesc[Counter] IN NextWordChars) OR ((Length(WordStr) + Length(TempStr)) > MaxLen) then
          begin;
            If ((Length(TempStr) + Length(WordStr))>MaxLen) OR (TempDesc[Counter] in [#10, #00]) then
              begin;
                Inc(DescLines);

                if TempDesc[Counter] in [#10, #00] then
                  begin
                    TempStr := TempStr + WordStr;
                    WordStr := '';
                  end; { if }

                Description.Put(DescLines, TempStr);

                TempStr := WordStr;
                WordStr := '';
              end else TempStr := TempStr + WordStr;

            WordStr := '';
          end; { In Next Word }

        If TempDesc[Counter] in [#00] then Break;
        Inc(Counter);
     end; { while }
  end { if not commenting }
    else begin
           Fillchar(TempArray, SizeOf(TempArray), #00);
           For Counter := 00 to 254 do
             TempArray[Counter] := TempDesc[Counter];

           Description.Put(01, StrPas(TempArray));
           DescLines := 01;
         end; { if }

  if DescLines=00 then
    If Counter>00 then Desclines := 01;

  If DescLines>01 then
   While (Description.Get(DescLines) = '') AND (DescLines>1) do Dec(DescLines);
end; { proc. Descript2Array }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ShowFile(ShowType: FileShowType; const IdxInf: FilesIdxRecord; const TxtInf: TxtRec;
                             SearchID: String; QScan: Boolean;
                             LongFName: String): Boolean;
var Counter   : Byte;
    TmpCRC    : Longint;
    SearchAr  : Array[0..255] of Char;
    TxtCopy   : PChar;
    TempStr   : String;

    IsNewStyle: Boolean;
    MustNot   : Boolean;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'ShowFile (begin)');
  {$ENDIF}

  ShowFile := False;
  if LongFName = '' then
    LongFName := IdxInf.Name;

  If (IdxInf.Name[1] = #00) AND
   (Byte(ShowType) in [Byte(fsKeyWord), Byte(fsNewFiles), Byte(fsFileName)])
    then EXIT;                              { Do not show comments in search }

  Case Byte(ShowType) of
    Byte(fsNormal)    : ShowFile := True;
    Byte(fsKeyWord)   : begin
                         SearchId := SUpCase(SearchID);
                         SearchID[Ord(SearchId[0])+1] := #00;

                         {
                         Writeln('Searching for: ', SearchId);
                         Writeln('Searchin In  : ', StrUpper(TempStr));
                         Writeln('Found        : ', StrPos(TempStr, @SearchId[1]) <> nil);
                         Writeln('SearchID     : ', PChar(SearchId[1]));
                         }

                         TxtCopy := StrNew(StrUpper(TxtInf));
                         ShowFile := false;
                         Mustnot := false;
                         IsNewStyle := (Pos('+', SearchID) > 0);

                         if TxtCopy <> nil then
                          for Counter := 01 to WordCount(SearchID, DefExtractWord, false) do
                           begin
                             TempStr := SUpCase(ExtractWord(SearchID, Counter, defExtractWord, false, false));

                             if NOT IsNewStyle then
                               TempStr := '+' + TempStr;

                             FillChar(SearchAr, SizeOf(SearchAr), #00);
                             if TempStr[1] in ['+', '-'] then
                               begin
                                 Move(TempStr[2], SearchAr, Length(TempStr) - 1)
                               end
                                 else begin
                                        Move(TempStr[1], SearchAr, Length(TempStr));
                                      end; { if }


                             If NOT QScan then
                              begin
                                 Case TempStr[1] of
                                   '+' : if (StrPos(TxtCopy,
                                              SearchAr) = nil) then
                                                MustNot := true
                                                 else ShowFile := true;
                                   '-' : if (StrPos(TxtCopy,
                                              SearchAr) <> nil) then
                                               MustNot := true
                                                else ShowFile := true;
                                 end; { case }

                                 if (NOT (TempStr[1] in ['-', '+'])) then
                                  if (StrPos(TxtCopy,
                                             SearchAr) <> nil) then
                                                ShowFile := true;
                              end; { if }

                           end; { for }

                         if MustNot then
                           ShowFile := false;

                         StrDispose(TxtCopy);

                         if TempStr[1] in ['+', '-'] then
                           Delete(TempStr, 1, 1);

						 { Now search the explicit keywords in the header files }
                         TmpCRC := RaCrc(SearchId, true);
                         
                         for Counter := 1 to 5 do
                          if IdxInf.KeyWordCRC[Counter] <> 00 then
                            begin
                              if (TmpCrc = IdxInf.KeywordCrc[Counter]) then
                                begin
                                  ShowFile := TRUE;
                                  EXIT;
                                end; { if }
                            end; { if }

						 { Now if the searchwords entered is present in the LFN, also show }
                         if (Pos(SearchId, SUpCase(LongFName)) > 0) then
                           ShowFile := true;
                  end; { Counter }
    Byte(fsNewFiles)  : ShowFile:= Is_NewFile(IdxInf.UploadDate, SearchID);
    Byte(fsFileName)  : ShowFile := IsWildCard(SearchID, SUpCase(LongFName));
  end; { Case }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'ShowFile ( end )');
  {$ENDIF}
end; { func. ShowFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GetDescriptionPos(var DescPos: Byte; const ListFormat: String; UseRAL: Boolean; var UseCR: Boolean);
var DisplayStr  : String;
    HdrInf      : FilesHdrRecord;
    Counter     : Byte;
    HdrLineAr   : ^DisplayLinesArray;
    MaxHdrLines : Byte;
    FilesInf    : FilesRecord;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'GetDescription (begin)');
  {$ENDIF}
  FillChar(HdrInf, SizeOf(FilesHdrRecord), #0);
  FillChar(FilesInf, SizeOf(FilesRecord), #0);

  if NOT AllocMem(HdrLineAr, SizeOf(DisplayLinesArray), 'HdrlinesAr', 'GetDescriptionPos') then EXIT;
  FillChar(HdrLineAr^, SizeOf(HdrLineAr^), #00);

  UseCR := FALSE;
  DisplayStr := ListFormat;       { To determinate position (on screen) where }

  DisplayListFile(HdrInf, DisplayStr, HdrLineAr^, '@DF', UseRAL, MaxHdrLines,
                  FilesInf, HdrInf.Name, true);       { to display the }

  for Counter := 00 to MaxHdrLines do
    RemoveRaColors(HdrLineAr^[Counter]);

  DescPos := Pos('@DF', HdrLineAr^[MaxHdrLines]);
  Descpos := Max(1, Descpos);
  UseCR := (MaxHdrLines > 0);

  ReleaseMem(HdrLineAr, SizeOf(DisplayLinesArray));

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'GetDescription ( end )');
  {$ENDIF}
end; { proc. GetDescriptionPos }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Is_Newfile(UploadDate: Longint; const LastDate: String): Boolean;
var Dt     : DateTime;
    UpUnix : LongInt;
    NowUnix: LongInt;
    Year,
    Month,
    Day    : Word;
    Hour,
    Min,
    Sec    : Word;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileSys, 'Is_NewFile (begin)');
  {$ENDIF}

  UnpackTime(UploadDate, DT);

  Dt.Min := 00;
  Dt.Hour := 00;
  Dt.Sec := 00;

  UpUnix := Norm2Unix(Dt.Year, Dt.Month, Dt.Day, Dt.Hour, Dt.Min, Dt.Sec);

  Hour := 00;
  Min  := 00;
  Sec  := 00;

  Month := Word(FVal(Copy(LastDate, 1, 2)));
  Day := Word(FVal(Copy(LastDate, 4, 2)));
  Year := Word(FVal(Copy(Lastdate, 7, 2)));

  While Day>31 do Dec(Day);
  While Month>12 do Dec(Month);

  If Year<80 then Inc(Year, 100);
  Inc(Year, 1900);

  NowUnix := Norm2Unix(Year, Month, Day, Hour, Min, Sec);

  Is_NewFile := (NowUnix <= UpUnix);

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileSys, 'Is_NewFile ( end )');
  {$ENDIF}
end; { func. Is_NewFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
