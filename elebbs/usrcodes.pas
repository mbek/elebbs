unit UsrCodes;
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
** EleBBS usercodes routines for EleBBS
**
** Copyright (c) 1996, 1998 by Maarten Bekers
**
** Created : 11-Oct-1998
** Last update : 11-Oct-1998
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function RauserCodeStr(C:Char; var WasNumeric: Boolean): String;
function UserCodeString(C:Char; var WasNumeric: Boolean; var Buf; DoSet: Boolean): String;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses CfgRec, BitWise, Global, FileRout, RAL, LongStr, JDates,
      Flagstr, WordStr, CentrStr, Cases, ObjDec,
        {$IFNDEF ISCGI}
          Mail,
        {$ENDIF}

        {$IFDEF DELPHI}
          SysUtils;
        {$ELSE}
          Strings;
        {$ENDIF}


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function RauserCodeStr(C:Char; var WasNumeric: Boolean): String;
var Empty: Array[1..1] of Char;
begin
  RaUserCodeStr := UserCodeString(C, WasNumeric, Empty, false);
end; { func. RaUserCodeStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function UserCodeString(C:Char; var WasNumeric: Boolean; var Buf; DoSet: Boolean): String;
var LastReadNumber: Longint;
    ProtInf       : ProtocolRecord;
    Empty         : Longint;

procedure SetResult(ValueStr: String; var Value; BitNr: Longint; Size: Word; NumericValue: Boolean);
type BufT = Array[0..250] of Char;

var TempVal: Longint;
begin
  UserCodeString := ValueStr;

  if DoSet then
  begin
    if NumericValue then
      begin
        BufT(Buf)[Ord(BufT(Buf)[0]) + 01] := #00;
        Move(BufT(Buf)[1], BufT(Buf)[0], 250);

        TempVal := FVal(StrPas(BufT(Buf)));
        Case Size of
{$IFNDEF FPC}
          2 : if (TempVal < 0) then Move(Integer(TempVal), Value, Size)
                 else Move(Word(TempVal), Value, Size);
{$ELSE}
{$WARNING FIX ME!}

{$ENDIF}
          4 : Move(Longint(TempVal), Value, Size);
        end; { case }
      end
       else
         if BitNr = -1 then
           begin
             if DoSet then
                Move(Buf, Value, Size);
           end
            else begin
                   BufT(Buf)[Ord(BufT(Buf)[0]) + 01] := #00;
                   Move(BufT(Buf)[1], BufT(Buf)[0], 250);

                   if Trim(SUpCase(StrPas(BufT(Buf)))) = 'ON' then
                     SetBit(Byte(Value), BitNr);

                   if Trim(SUpCase(StrPas(BufT(Buf)))) = 'OFF' then
                     ClearBit(Byte(Value), BitNr);
                 end; { else }
  end; { if DoSet }
end; { proc. SetResult }

procedure SetLastReadResult(ValueStr: String; var Value);
type BufT = Array[0..250] of Char;

var TempVal: Longint;
begin
  UserCodeString := ValueStr;

  if DoSet then
    begin
     {$IFNDEF ISCGI}
       SetLastRead(LineCfg^.CurMessageRecord^,
                   LineCfg^.CurEleMsgRecord^,
                   Longint(Value));
     {$ENDIF}
    end; { if DoSet }
end; { proc. SetLastReadResult }

begin
  WasNumeric := false;
  C := UpCase(c);
  Empty := -1;

  if LineCfg^.UserAgeSave = 0 then
    GetUserAge(LineCfg^.Exitinfo^.Userinfo.birthdate, true);

  if C in ['*', '+', '9', ':', 'L', 'M', 'N', 'O', 'P', 'Q',
           'R', 'S', 'T', 'U', 'V', '[', '^', '_', '{'] then
             WasNumeric := true;

  if C = 'N' then
    begin
      if ReadBit(LineCfg^.CurMessageRecord^.Attribute, 7) then
        begin
          {$IFNDEF ISCGI}
            LastReadNumber := GetLastReadNum(LineCfg^.CurMessageRecord^);
          {$ENDIF}
        end
         else LastReadNumber := LineCfg^.Exitinfo^.Userinfo.LastRead;
    end; { if }

  UserCodeString := '';
  with LineCfg^ do
  Case C of
  { Spade character} ^F    : SetResult(^F, Empty, 0, 0, false);
  { Def.Prot.Sett }  '!'   : begin
                               {$IFDEF WITH_FULL}
                                 GetProtocolRecord(Exitinfo^.Userinfo.DefaultProtocol, Protinf);
                               {$ENDIF}
                               SetResult(ProtInf.Name, ProtInf.Name, Empty, SizeOf(Exitinfo^.Userinfo.DefaultProtocol), false);
                             end; { DefaultProtocol }
  { Users age }      '@'   : SetResult(FStr(UserAgeSave), Empty, Empty, 0, false);
  { Def.Prot.Sett }  '~'   : SetResult(Exitinfo^.Userinfo.Organisation, Exitinfo^.Userinfo.Organisation, Empty,
                                        SizeOf(Exitinfo^.Userinfo.Organisation), false);
  { Selected only }  '-'   : SetResult(LangObj^.LangBool2Str(NOT ReadBit(Exitinfo^.Userinfo.Attribute3, 0)),
                                        Exitinfo^.Userinfo.Attribute3, 0,
                                        SizeOf(Exitinfo^.Userinfo.Attribute3), false);
  { Excl.EchoMail?}  '"'   : SetResult(LangObj^.LangBool2Str(ReadBit(Exitinfo^.Userinfo.Attribute2, 5)),
                                        Exitinfo^.Userinfo.Attribute2, 5,
                                        SizeOf(Exitinfo^.Userinfo.Attribute), false);
  { Curr.FileGrp }   '#'   : SetResult(GroupName(Ra250Group(Exitinfo^.Userinfo.Filegroup, false), False),
                                       Exitinfo^.Userinfo.FileGroup, Empty, 0, false);
  { Addr.Line 1 }    '$'   : SetResult(Exitinfo^.Userinfo.Address1, Exitinfo^.Userinfo.Address1, Empty,
                                                 SizeOf(Exitinfo^.Userinfo.Address1), false);
  { Addr.Line 2 }    '%'   : SetResult(Exitinfo^.Userinfo.Address2, Exitinfo^.Userinfo.Address2, Empty,
                                                 SizeOf(Exitinfo^.Userinfo.Address2), false);
  { Addr.Line 3 }    '&'   : SetResult(Exitinfo^.Userinfo.Address3, Exitinfo^.Userinfo.Address3, Empty,
                                                 SizeOf(Exitinfo^.Userinfo.Address3), false);
  { Male/Female }    ''''  : SetResult(LangObj^.Sex2Str(Exitinfo^.Userinfo.Sex, True), Exitinfo^.Userinfo.Sex, Empty,
                                                 SizeOf(Exitinfo^.Userinfo.Sex), true);
  { postbilling ? }  '('   : SetResult(LangObj^.LangBool2Str(ReadBit(Exitinfo^.Userinfo.Attribute2, 5)),
                                                 Exitinfo^.Userinfo.Attribute2, 5,
                                                 SizeOf(Exitinfo^.Userinfo.Attribute2), false);
  { Curr. MsgGrp }   ')'   : SetResult(GroupName(Ra250Group(Exitinfo^.Userinfo.Msggroup, true), True),
                                                 Exitinfo^.Userinfo.MsgGroup, Empty, 00, false);
  { Curr.FleGrpNr }  '*'   : SetResult(FStr(Exitinfo^.Userinfo.FileGroup),
                                        Exitinfo^.Userinfo.FileGroup, Empty, SizeOf(Exitinfo^.Userinfo.FileGroup), true);
  { Curr.MsgGrpNr }  '+'   : SetResult(FStr(Exitinfo^.Userinfo.MsgGroup), Exitinfo^.Userinfo.MsgGroup, Empty,
                                        SizeOf(Exitinfo^.Userinfo.MsgGroup), true);
  { FullScrn Editr}  '0'   : SetResult(LangObj^.LangBool2Str(ReadBit(Exitinfo^.Userinfo.Attribute, 6)),
                                        Exitinfo^.Userinfo.Attribute, 6,
                                        SizeOf(Exitinfo^.Userinfo.Attribute), false);
  { Not Disturb? }   '1'   : SetResult(LangObj^.LangBool2Str(ReadBit(Exitinfo^.Userinfo.Attribute, 7)),
                                        Exitinfo^.Userinfo.Attribute, 7,
                                        SizeOf(Exitinfo^.Userinfo.Attribute), false);
  { HotKeys }        '2'   : SetResult(LangObj^.LangBool2Str(ReadBit(Exitinfo^.Userinfo.Attribute2, 0)),
                                        Exitinfo^.Userinfo.Attribute2, 0,
                                        SizeOf(Exitinfo^.Userinfo.Attribute), false);
  { Users Handle }   '3'   : SetResult(Exitinfo^.Userinfo.Handle, Exitinfo^.Userinfo.Handle, Empty,
                                        SizeOf(Exitinfo^.Userinfo.Handle), false);
  { First call }     '4'   : SetResult(LangObj^.RaFormatDate(Exitinfo^.Userinfo.FirstDate,
                                        Exitinfo^.Userinfo.DateFormat, y2k_SysDate, 0),
                                        Exitinfo^.Userinfo.FirstDate, Empty,
                                        SizeOf(Exitinfo^.Userinfo.FirstDate), false);
  { Birthdate }      '5'   : SetResult(LangObj^.RaFormatDate(Exitinfo^.Userinfo.Birthdate,
                                        Exitinfo^.Userinfo.DateFormat, y2k_Userdate, 0),
                                        Exitinfo^.Userinfo.BirthDate, Empty,
                                        SizeOf(Exitinfo^.Userinfo.BirthDate), false);
  { Sub.Expr.Date }  '6'   : SetResult(LangObj^.RaFormatDate(Exitinfo^.Userinfo.SubDate,
                                        Exitinfo^.Userinfo.DateFormat, y2k_SysDate, 0),
                                        Exitinfo^.Userinfo.SubDate, Empty,
                                        SizeOf(Exitinfo^.Userinfo.SubDate), false);
  { Days Till Expr}  '7'   : SetResult(FStr(DaysToGo(Exitinfo^.Userinfo.SubDate)), Empty, Empty, 0, false);
  { Avatar On/Off}   '8'   : SetResult(LangObj^.LangBool2Str(ReadBit(Exitinfo^.Userinfo.Attribute2, 1)),
                                       Exitinfo^.Userinfo.Attribute2, 1,
                                       SizeOf(Exitinfo^.Userinfo.Attribute2), false);
  { Ratio,NrFiles }  '9'   : SetResult(FStr(LimitsInfo^.RatioNum), LimitsInfo^.RatioNum, Empty,
                                       SizeOf(LimitsInfo^.RatioNum), false);
  { Ratio, KBytes }  ':'   : SetResult(FStr(LimitsInfo^.RatioK), LimitsInfo^.RatioK, Empty,
                                       SizeOf(LimitsInfo^.RatioK), false);
  { Full-MsgVwer }   ';'   : SetResult(LangObj^.LangBool2Str(ReadBit(Exitinfo^.Userinfo.Attribute2, 2)),
                                       Exitinfo^.Userinfo.Attribute2, 2,
                                       SizeOf(Exitinfo^.Userinfo.Attribute2), false);
  { DateFormat }     '<'   : begin
                              {$IFDEF WITH_FULL}
                                SetResult(LangObj^.Byte2DateFormat(Exitinfo^.Userinfo.DateFormat, True),
                                          Exitinfo^.Userinfo.DateFormat, Empty,
                                          SizeOf(Exitinfo^.Userinfo.DateFormat), true);
                              {$ELSE}
                                SetResult(FStr(Exitinfo^.Userinfo.DateFormat),
                                          Exitinfo^.Userinfo.DateFormat, Empty,
                                          SizeOf(Exitinfo^.Userinfo.DateFormat), true);
                              {$ENDIF}
                             end; { Dateformat }
  { MsgForwarding }  '='   : SetResult(LangObj^.LangBool2Str(NOT (Exitinfo^.Userinfo.ForwardTo='')),
                                        Exitinfo^.Userinfo.ForwardTo, Empty,
                                        SizeOf(Exitinfo^.Userinfo.ForwardTo), false);
  { Forward To? }    '>'   : begin
                               if Exitinfo^.Userinfo.ForwardTo <> '' then
                                 SetResult(Exitinfo^.Userinfo.ForwardTo, Exitinfo^.Userinfo.ForwardTo, Empty,
                                            SizeOf(Exitinfo^.Userinfo.ForwardTo), false)
                                   else SetResult(LangObj^.ralGet(ralNobody), Exitinfo^.Userinfo.ForwardTo, Empty,
                                                  SizeOf(Exitinfo^.Userinfo.ForwardTo), false);
                             end; { Forward-To Name }
  { FullName }       'A'   : SetResult(Exitinfo^.Userinfo.Name, Exitinfo^.Userinfo.Name, Empty,
                                       SizeOf(Exitinfo^.Userinfo.Name), false);
  { Location }       'B'   : SetResult(Exitinfo^.Userinfo.Location, Exitinfo^.Userinfo.Location, Empty,
                                       SizeOf(Exitinfo^.Userinfo.Location), false);
  { Data Number }    'D'   : SetResult(Exitinfo^.Userinfo.DataPhone, Exitinfo^.Userinfo.DataPhone, Empty,
                                       SizeOf(Exitinfo^.Userinfo.DataPhone), false);
  { Voice Number }   'E'   : SetResult(Exitinfo^.Userinfo.VoicePhone, Exitinfo^.Userinfo.VoicePhone, Empty,
                                       SizeOf(Exitinfo^.Userinfo.VoicePhone), false);
  { Date Lastcall }  'F'   : SetResult(LangObj^.RaFormatDate(Exitinfo^.Userinfo.LastDate,
                                       Exitinfo^.Userinfo.DateFormat, y2k_SysDate, 0),
                                       Exitinfo^.Userinfo.LastDate, Empty,
                                       SizeOf(Exitinfo^.Userinfo.LastDate), false);
  { Time lastcall }  'G'   : SetResult(Exitinfo^.Userinfo.LastTime, Exitinfo^.Userinfo.LastTime, Empty,
                                       SizeOf(Exitinfo^.Userinfo.LastTime), false);
  { 'A' Flags }      'H'   : SetResult(Byte2Flags(Exitinfo^.Userinfo.Flags[1]), Exitinfo^.Userinfo.Flags[1], Empty,
                                         SizeOf(Exitinfo^.Userinfo.Flags[1]), false);
  { 'B' Flags }      'I'   : SetResult(Byte2Flags(Exitinfo^.Userinfo.Flags[2]), Exitinfo^.Userinfo.Flags[2], Empty,
                                         SizeOf(Exitinfo^.Userinfo.Flags[1]), false);
  { 'C' Flags }      'J'   : SetResult(Byte2Flags(Exitinfo^.Userinfo.Flags[3]), Exitinfo^.Userinfo.Flags[3], Empty,
                                         SizeOf(Exitinfo^.Userinfo.Flags[1]), false);
  { 'D' Flags }      'K'   : SetResult(Byte2Flags(Exitinfo^.Userinfo.Flags[4]), Exitinfo^.Userinfo.Flags[4], Empty,
                                         SizeOf(Exitinfo^.Userinfo.Flags[1]), false);
  { Credits Remng }  'L'   : SetResult(FStr(Exitinfo^.Userinfo.Credit-
                                       Exitinfo^.Userinfo.Pending-Exitinfo^.MenuCostPermin), Exitinfo^.Userinfo.Credit,
                                             Empty, 00, false);
  { Ttl Msgs Psted } 'M'   : SetResult(FStr(Exitinfo^.Userinfo.MsgsPosted), Exitinfo^.Userinfo.MsgsPosted, Empty,
                                         SizeOf(Exitinfo^.Userinfo.MsgsPosted), true);
  { LastMsgRead }    'N'   : SetLastReadResult(FStr(LastReadNumber), LastReadNumber);
  { Security Lvl }   'O'   : SetResult(FStr(Exitinfo^.Userinfo.Security), Exitinfo^.Userinfo.Security, Empty,
                                         SizeOf(Exitinfo^.Userinfo.Security), true);
  { Total Calls }    'P'   : SetResult(FStr(Exitinfo^.Userinfo.NoCalls), Exitinfo^.Userinfo.NoCalls, Empty,
                                         SizeOf(Exitinfo^.Userinfo.Nocalls), true);
  { Files Uplded }   'Q'   : SetResult(FStr(Exitinfo^.Userinfo.Uploads), Exitinfo^.Userinfo.Uploads, Empty,
                                         SizeOf(Exitinfo^.Userinfo.Uploads), true);
  { KB's upped }     'R'   : SetResult(FStr(Exitinfo^.Userinfo.UploadsK), Exitinfo^.Userinfo.UploadsK, Empty,
                                         SizeOf(Exitinfo^.Userinfo.UploadsK), true);
  { Files Downed }   'S'   : SetResult(FStr(Exitinfo^.Userinfo.Downloads), Exitinfo^.Userinfo.Downloads, Empty,
                                         SizeOf(Exitinfo^.Userinfo.Downloads), true);
  { KB's downed }    'T'   : SetResult(FStr(Exitinfo^.Userinfo.DownloadsK), Exitinfo^.Userinfo.DownloadsK, Empty,
                                         SizeOf(Exitinfo^.Userinfo.DownloadsK), true);
  { Mins used tday}  'U'   : SetResult(FStr(Exitinfo^.Userinfo.Elapsed), Exitinfo^.Userinfo.Elapsed, Empty,
                                         SizeOf(Exitinfo^.Userinfo.Elapsed), true);
  { Scrn Length }    'V'   : SetResult(FStr(Exitinfo^.Userinfo.ScreenLength), Exitinfo^.Userinfo.ScreenLength, Empty,
                                         SizeOf(Exitinfo^.Userinfo.ScreenLength), true);
  { FirstName }      'W'   : SetResult(FirstNameOnly(Exitinfo^.Userinfo.Name), Exitinfo^.Userinfo.Name, Empty, 00, false);
  { ANSi settings }  'X'   : SetResult(LangObj^.LangBool2Str(Readbit(Exitinfo^.Userinfo.Attribute, 3)),
                                         Exitinfo^.Userinfo.Attribute, 3,
                                         SizeOf(Exitinfo^.UserInfo.Attribute), false);
  { Continue }       'Y'   : SetResult(LangObj^.LangBool2Str(Readbit(Exitinfo^.Userinfo.Attribute, 2)),
                                         Exitinfo^.Userinfo.Attribute, 2,
                                         SizeOf(Exitinfo^.UserInfo.Attribute), false);
  { Scrn Clearing }  'Z'   : SetResult(LangObj^.LangBool2Str(Readbit(Exitinfo^.Userinfo.Attribute, 1)),
                                         Exitinfo^.Userinfo.Attribute, 1,
                                         SizeOf(Exitinfo^.UserInfo.Attribute), false);
  { KB's remaining}  '['   : begin
                               if DoSet then
                                 begin
                                   SetResult(FStr(UsersKBLimit - (Exitinfo^.Userinfo.TodayK)),
                                             Exitinfo^.Userinfo.TodayK, Empty,
                                             Sizeof(Exitinfo^.Userinfo.TodayK), true);
                                 end
                                   else begin
                                          if UsersKbLimit = UnlimitedValue then
                                            SetResult(langObj^.ralGet(ralUnlimited),
                                                      Empty,
                                                      Empty,
                                                      0,
                                                      false)
                                              else
                                                SetResult(FStr(UsersKBLimit - (Exitinfo^.Userinfo.TodayK)),
                                                          Exitinfo^.Userinfo.TodayK, Empty,
                                                          Sizeof(Exitinfo^.Userinfo.TodayK), true);

                                        end; { if }
                             end; { kb's remaining }
  { Comment field }  ']'   : SetResult(Exitinfo^.Userinfo.Comment, Exitinfo^.Userinfo.Comment, Empty,
                                         SizeOf(Exitinfo^.Userinfo.Comment), false);
  { Session tme lmt} '^'   : begin
                               if Limitsinfo^.lTime = UnlimitedValue then
                                 SetResult(langObj^.ralGet(ralUnlimited),
                                           Empty,
                                           Empty,
                                           0,
                                           false)
                                   else SetResult(FStr(LimitsInfo^.lTime), LimitsInfo^.lTime, Empty, 00, false);
                             end; { if }
  { Rmngn session }  '_'   : begin
                               if Limitsinfo^.lTime = UnlimitedValue then
                                 SetResult(langObj^.ralGet(ralUnlimited),
                                           Empty,
                                           Empty,
                                           0,
                                           false)
                                   else  SetResult(FStr(Exitinfo^.TimeLimit), Exitinfo^.TimeLimit, Empty,
                                                   SizeOf(Exitinfo^.TimeLimit), true);
                             end; { if }
  { Language name }  '\'   : SetResult(Language^.Name, Language^.Name, Empty, 00, false);
  { UserRecord nr }  '{'   : SetResult(FStr(Exitinfo^.UserRecord), Exitinfo^.UserRecord, Empty,
                                         SizeOf(Exitinfo^.Userinfo.MsgsPosted), true);
  { Users' ip addr } '}'   : SetResult(LineCfg^.TelnetFromIp, Empty, Empty, 0, false);
  end; { Case }

end; { func. RaUserCodeStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

end. { unit. UsrCode }
