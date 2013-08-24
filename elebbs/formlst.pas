unit Formlst;
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
** Formatting routines for listing files in EleBBS.
**
** Copyright (c) 1997,1998 by Maarten Bekers
**
** Created : 01-Mar-1998
** Last update : 01-Mar-1998
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Global, Ra2Fdb, Colors, CfgRec;

Type DisplayLinesArray = Array[0..25] of String;

procedure DisplayListFile(const HdrInf: FilesHdrRecord; var ListFormat: String;
                          var HdrArray: DisplayLinesArray; const DescLine: String; UseRAL: Boolean; var MaxHdr: Byte;
                          const FilesInf: FilesRecord; const FileName: String;
                          SupportBash: Boolean);

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Debug_U, Ral, BitWise, Stutils, ObjDec,
      Desc_U, ElLog_U,
       Cases, StrPath, LongStr, CentrStr, JDates;

procedure DisplayListFile(const HdrInf: FilesHdrRecord; var ListFormat: String;
                          var HdrArray: DisplayLinesArray; const DescLine: String; UseRAL: Boolean; var MaxHdr: Byte;
                          const FilesInf: FilesRecord; const FileName: String;
                          SupportBash: Boolean);

function MakeFileName(S: String; Delimit: Byte): String;
begin
  if (Length(s) < 12) AND (Delimit=0) then
    MakeFileName := MakeLen(S, 12, space, false, false)
     else MakeFileName := s;
end; { func. MakeFileName }

function GiveListID(S: String; Delimit: Byte): String;
var TempMacro      : String;
    RalStr         : String;
    SaveListID     : String;

    Counter        : Byte;

{$IFNDEF WITH_FULL}
function RaFormatDate (D: String; Format : Byte; YearType: y2k_YearType) : String;  { Format Date var }
var AddID: String;
    Year : Longint;
begin
  AddID := '19';

  Year := FVal(Copy(D, 7, 2));
  Case YearType of
     y2k_Userdate : if Year < y2k_UserFrame then AddID := '20' else AddID := '19';
     y2k_FileDate : if Year < y2k_FileFrame then AddID := '20' else AddID := '19';
     y2k_MsgDate  : if Year < y2k_MessageFrame then AddID := '20' else AddID := '19';
     y2k_SysDate  : if Year < y2k_SysFrame then AddID := '20' else AddID := '19';
  end; { case }

  Insert(AddID, d, 7);

  RaFormatDate := d;
end; { func. RaFormatDate }
{$ENDIF}

begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileSys, 'GiveListID: '+S);
  {$ENDIF}
  for Counter := 01 to Length(s) do
    S[Counter] := UpCase(S[Counter]);

  TempMacro := '';

(*
  If ReadBit(HdrInf.Attrib, 2) then FreeStr := IsFreeStr
    else FreeStr := NotFreeStr;

  If HdrInf.Password <>'' then PassWordStr:= ralPassword_Str
    else PasswordStr:= Dup(#32, OutputObj^.NoColorlength(ralPassword_Str));

  If Is_NewFile(HdrInf.UploadDate, Exitinfo^.Userinfo.LastDate) then NewStr := '*'
    else NewStr := ' ';
*)

  SaveListID := S;

  Case S[1] of
    'N' : begin
            If S='NE' then S := MakeFileName(MakeLen(FileName, Delimit, Space, False, false), Delimit) else
             If S='NN' then S := MakeFileName(MakeLen(NoExtension(FileName), Delimit, Space, False, false), Delimit) else
              If S='NA' then begin
                               if UseRal then RalStr := LangObj^.ralGet(ralNotAvail2)
                                 else RalStr := '(Notavail)';
                               {$IFDEF WITH_FULL}
                                 If (ReadBit(HdrInf.Attrib, 3)) then TempMacro := ralStr
                                    else TempMacro := OutputObj^.AvatarDupl(#32, NoColorlength(ralStr));
                               {$ELSE}
                                 If (ReadBit(HdrInf.Attrib, 3)) then TempMacro := ralStr
                                    else TempMacro := Dup(#32, NoColorlength(ralStr));
                               {$ENDIF}

                               S := TempMacro;
                             end else
               If S='NW' then begin
                                {$IFDEF WITH_FULL}
                                  If Is_NewFile(HdrInf.UploadDate, LineCfg^.Exitinfo^.Userinfo.LastDate) then TempMacro:= '*'
                                    else TempMacro:= ' ';
                                {$ELSE}
                                  TempMacro := #32;
                                {$ENDIF}

                                S := TempMacro;
                              end; { if }
          end; { 'N' }
    'S' : begin
            If S='SB' then
              begin
                S := FStr(HdrInf.Size);

                if Length(FStr(HdrInf.Size)) < 7 then
                  S := MakeLen(FStr(HDRInf.Size), 7, Space, True, false);
              end
               else
                If S='SK' then
                  begin
                    S := FStr(HdrInf.Size div 1024);
                    if Length(s) < 4 then
                      S := MakeLen(S, 4, Space, true, false);
                  end; { if SizeK }
          end; { 'S' }
    'U' : begin
            If S='UL' then S := MakeLen(HdrInf.Uploader, 35, Space, False, false) else
             If S='UD' then S := LangObj^.RaFormatDate(Date2Str(HdrInf.UploadDate), 0, y2k_FileDate,
                                                       LineCfg^.Exitinfo^.Userinfo.DateFormat);
          end; { 'U' }
    'F' : begin
            If S='FD' then S := LangObj^.RaFormatDate(Date2Str(HdrInf.FileDate), 0, y2k_FileDate,
                                   LineCfg^.Exitinfo^.userinfo.DateFormat) else
             If S='FR' then begin
                               if UseRal then RalStr := LangObj^.ralGet(ralFreeDL)
                                 else RalStr := '(Free)';
                               {$IFDEF WITH_FULL}
                                 If (ReadBit(HdrInf.Attrib, 2)) OR (ReadBit(FilesInf.Attrib, 4)) then TempMacro := ralStr
                                    else TempMacro := OutputObj^.AvatarDupl(#32, NoColorlength(ralStr));
                               {$ELSE}
                                 If (ReadBit(HdrInf.Attrib, 2)) OR (ReadBit(FilesInf.Attrib, 4)) then TempMacro := ralStr
                                    else TempMacro := Dup(#32, NoColorlength(ralStr));
                               {$ENDIF}

                               S := TempMacro;
                            end; { free }
          end; { 'F' }
    'L' : begin
            If S='LD' then S := LangObj^.RaFormatDate(Date2Str(HdrInf.LastDL), 0, y2k_SysDate,
                LineCfg^.Exitinfo^.Userinfo.DateFormat) else
          end; { 'L' }
    'T' : begin
            If S='TD' then S := MakeLen(FStr(HdrInf.TimesDl), 4, Zero, true, false);
          end; { 'T' }
    'K' : begin
            If S='K1' then S := MakeLen(HdrInf.KeyWord[1], 15, Space, False, false) else
             If S='K2' then S := MakeLen(HdrInf.KeyWord[2], 15, Space, False, false) else
              If S='K3' then S := MakeLen(HdrInf.KeyWord[3], 15, Space, False, false) else
               If S='K4' then S := MakeLen(HdrInf.KeyWord[4], 15, Space, False, false) else
                If S='K5' then S := MakeLen(HdrInf.KeyWord[5], 15, Space, False, false);
          end; { 'K' }
    'C' : begin
            If S='CS' then S := MakeLen(FStr(HdrInf.Cost), 3, Zero, True, false);
          end; { 'C' }
    'D' : begin
            If S='DF' then S := DescLine else
             If S='DU' then S := DescLine;
          end; { 'D' }
    'P' : begin
            If S='PW' then begin
                             if UseRal then RalStr:= LangObj^.ralGet(ralPassword2)
                               else RalStr := '(Password)';
                             {$IFDEF WITH_FULL}
                               If (HdrInf.Password<>'') OR (Filesinf.PassWord <> '') then TempMacro := ralStr
                                  else TempMacro := OutputObj^.AvatarDupl(#32, NoColorlength(ralStr));
                             {$ELSE}
                               If (HdrInf.Password<>'') OR (Filesinf.PassWord <> '') then TempMacro := ralStr
                                  else TempMacro := Dup(#32, NoColorlength(ralStr));
                             {$ENDIF}

                             S := TempMacro;
                           end; { if }
          end; { 'P' }
  end; { case }

  if (SaveListID <> 'DF') AND (SaveListID <> 'DU') then
    begin
      if SaveListID <> 'TD' then GiveListID := MakeLen(S, Delimit, Space, False, false)
        else GiveListID := MakeLen(S, Delimit, Zero, true, false);
    end { if SaveListID }
      else begin
             GiveListID := S;
           end; { if SaveListID }
end; { func. GiveListID }

var Counter    : Longint;
    Delimiter  : Longint;
    CurrentLine: Longint;
    StartPos   : Longint;
    LineCounter: Longint;
    TempStr    : String;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'DisplayListFile: '+DescLine);
  {$ENDIF}
  Counter := 01;
  Delimiter := 00;                                           { No max. length }
  CurrentLine := 00;
  HdrArray[0] := Listformat;


  StartPos:= 00;
  While Counter <= Length(ListFormat) do
    begin
      if CurrentLine>25 then BREAK;
      if (Pos(^M, Copy(ListFormat, Counter, 255))=00) AND
          (Pos('|', Copy(ListFormat, Counter, 255))=00) then BREAK;

      Case ListFormat[Counter] of
         ^M : if CurrentLine<26 then
                 begin
                   HdrArray[CurrentLine] := Copy(ListFormat, StartPos+1, (Counter - Startpos) - 01);
                   Inc(CurrentLine);
                   Inc(Counter);
                   StartPos := Counter;
                 end;
        '|' : if CurrentLine<26 then
                 begin
                   HdrArray[CurrentLine] := Copy(ListFormat, StartPos+1, (Counter - Startpos) - 01);
                   Inc(CurrentLine);
                   StartPos := Counter;
                 end;
      end; { Case }

      Inc(Counter);
    end; { While }

{  Inc(CurrentLine); }
  HdrArray[CurrentLine] := Copy(ListFormat, Startpos + 01, 255);

  LineCounter := 00;
  While LineCounter <= CurrentLine do
   begin
    Counter := 01;

    While (Counter <= Length(HdrArray[LineCounter])) do
     begin
      case HdrArray[LineCounter, Counter] of
          '#' : If (Counter+2)<=Length(HdrArray[LineCounter]) then
                 if SupportBash then
                  begin
                    Delimiter := FVal(Copy(HdrArray[LineCounter], Counter+1, 2));

                    Delete(HdrArray[LineCounter], Counter, 3);
                    Dec(Counter, 1);
                 end; { Delimiter length }
          '@' : If (Counter+2)<=Length(HdrArray[LineCounter]) then
                begin
                   TempStr := GiveListID(Copy(HdrArray[LineCounter], Counter+1, 2), Delimiter);

                   if TempStr<>Copy(HdrArray[LineCounter], Counter+1, 2) then
                     begin
                       Delete(HdrArray[LineCounter], Counter, 3);
                       Insert(TempStr, HdrArray[LineCounter], Counter);
                     end; { replace }

                    if Length(TempStr) > 0 then
{!}                   Inc(Counter, Length(TempStr) - 01)
                       else Dec(Counter);
{                   Inc(Counter, 1); }
                end; { List Format (Normal) }

          '%' : If (Counter+2)<=Length(HdrArray[LineCounter]) then
                begin
                   TempStr := SlowCase(GiveListID(Copy(HdrArray[LineCounter], Counter+1, 2), Delimiter));
                   If TempStr<>Copy(HdrArray[LineCounter], Counter+1, 2) then
                    begin;
                       Delete(HdrArray[LineCounter], Counter, 3);
                       Insert(TempStr, HdrArray[LineCounter], Counter);
                    end; { replace }

{                   Inc(Counter, 1);}
                    if Length(TempStr) > 0 then
{!}                   Inc(Counter, Length(TempStr) - 01);
                end; { List format (LowerCase) }

          '^' : If (Counter+2)<=Length(HdrArray[LineCounter]) then
                begin
                   TempStr := SUpCase(GiveListID(Copy(HdrArray[LineCounter], Counter+1, 2), Delimiter));
                   If TempStr<>Copy(HdrArray[LineCounter], Counter+1, 2) then
                    begin;
                       Delete(HdrArray[LineCounter], Counter, 3);
                       Insert(TempStr, HdrArray[LineCounter], Counter);
                    end; { replace }

{                   Inc(Counter, 1); }
                    if Length(TempStr) > 0 then
{!}                   Inc(Counter, Length(TempStr) - 01);
                end; { List format (UpperCase) }
      end; { case }

      Inc(Counter);
     end; { while }

    Inc(LineCounter);
   end; { While: LINECOUNTER }

  MaxHdr := CurrentLine;
end; { proc. DisplayListFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
