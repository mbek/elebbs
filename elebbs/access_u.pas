unit Access_U;
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
** Access (file,group,message) routines for EleBBS
**
** Copyright (c) 1997 by Maarten Bekers
**
** Created : 12-Oct-1996
** Last update : 12-Oct-1997
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
uses Global, CfgRec;


Function  CheckFlagAccess(UserFlags: FlagType; On_Flags, Off_Flags: FlagType): Boolean;
function  CheckFileAreaAccess(RaInf: FilesRecord;
                              Download, Group, Upload, List: Boolean;
                              GroupNr: Word;
                              var Exitinfo: ExitinfoRecord): Boolean;
function  CheckGroupAccess(Group: GroupRecord; var Exitinfo: ExitinfoRecord): Boolean;
function  CheckLanguageAccess(Language: LanguageRecord;
                              CheckSecurity: Boolean;
                              var Exitinfo: ExitinfoRecord): Boolean;
Function  CheckMsgAreaAccess(RaInf: MessageRecord; Group, Read: Boolean; GroupNr: Word;
                             const UserInf: UsersRecord): Boolean;
function  CheckMenuAccess(Menu: MnuRecord; var Exitinfo: ExitinfoRecord): Boolean;
Function  CheckTimeRange(RangeStart, RangeEnd: TimeType; DefAlways: Boolean):Boolean; { Check if curr. time are in Range }
function  ReadMsgAccess(FromTo: String; SysopAccess, IsPriv: Boolean;
                        const UserInf: UsersRecord): Boolean;
function  SysOpAccess(MessageInf: MessageRecord; const UserInf: UsersRecord): Boolean;
function  CheckMsgDeleteAccess(const MsgInf  : MessageRecord;
                                     MsgTo,
                                     MsgFrom : String;
                                     IsSent  : Boolean;
                                     UserInfo: UsersRecord): Boolean;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses SysUtils, Jdates, Dos, Debug_U, LongStr, BitWise, CentrStr,
     Checkbit, GenDos, Cases;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CheckTimeRange(RangeStart, RangeEnd: TimeType; DefAlways: Boolean):Boolean;
var CurHour,
    CurMinutes   : Word;
    StartHour,
    StartMinutes : Word;
    EndHour,
    EndMinutes   : Word;
    Hours        : Set of Byte;
    Empty        : Word;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logDateTime, 'CheckTimeRange (Start) (RangeStart/RangeEnd: '+RangeStart+' / '+RangeEnd + ')');
  {$ENDIF}

  CheckTimeRange := DefAlways;
  If (RangeStart='00:00') AND (RangeEnd='00:00') then Exit;
  if (RangeStart=RangeEnd) then EXIT;

  CheckTimeRange := True;
  If (RangeStart='00:00') AND (RangeEnd='23:59') then Exit;

  GetTime(CurHour, CurMinutes, Empty, Empty);
  StartHour    := FVal(Copy(RangeStart, 01, 02));
  EndHour      := FVal(Copy(RangeEnd, 01, 02));
  StartMinutes := FVal(Copy(RangeStart, 04, 02));
  EndMinutes   := FVal(Copy(RangeEnd, 04, 02));

  Hours := [];

  if StartHour < EndHour then
   for Empty := StartHour to EndHour do
     Hours := Hours + [Empty];

  if StartHour > EndHour then
   begin
     for Empty := StartHour to 23 do
       Hours := Hours + [Empty];

     for Empty := 00 to EndHour do
       Hours := Hours + [Empty];
   end; { if }

  if CurHour in Hours then
    begin
      If (CurHour=StartHour) then
        If CurMinutes >= StartMinutes then EXIT;
      If (CurHour=EndHour) then
        If CurMinutes <= EndMinutes then EXIT;
      If (CurHour<>StartHour) AND (CurHour<>EndHour)
        then Exit;
    end; { If }
  CheckTimeRange := False;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logDateTime, 'CheckTimeRange ( end )');
  {$ENDIF}
end; { func. CheckTimeRange }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CheckMenuAccess(Menu: MnuRecord; var Exitinfo: ExitinfoRecord): Boolean;
Var Year      : Word;
    Month     : Word;
    Day       : Word;
    Dow       : Word;

    StartTime : CfgRec.Time;
    StopTime  : CfgRec.Time;
    Hours,
    Mins      : Word;
    Temp      : Boolean;
{$IFNDEF FPC}
{$IFNDEF WIN32}
 {$IFNDEF OS2}
    Result    : Boolean;
 {$ENDIF}
{$ENDIF}
{$ENDIF}
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logAccess, 'CheckMenuAccess (begin)');
  {$ENDIF}

  if LineCfg^.UserAgeSave = 0 then
    GetUserAge(LineCfg^.Exitinfo^.Userinfo.Birthdate, true);

{$IFNDEF WINGUI}
  GetDate(Year, Month, Day, Dow);                 { Only will get Day Of Week }
{$ELSE}
  DecodeDate(Now, Year, Month, Dow);
  DOW := SysUtils.DayOfWeek(Now)-1;
{$ENDIF}

  Result := True;

  If Menu.Security>Exitinfo.Userinfo.Security then Result := False;
  If Menu.MaxSec>00 then
   If Exitinfo.Userinfo.Security>Menu.MaxSec then Result := False;

  If Result then
   Result := CheckFlagAccess(Exitinfo.Userinfo.Flags, Menu.Flags, Menu.NotFlagsMask);

  If Menu.TimeLeft > 00 then
    If Menu.TimeLeft>Exitinfo.TimeLimit then Result := False;
  If Menu.TimeUsed > 00 then
  If Menu.TimeUsed>Exitinfo.Userinfo.Elapsed then Result := False;

  If Menu.Age > LineCfg^.UserAgeSave then Result := False;

  If Menu.MinSpeed>00 then
   If Exitinfo.Baud>00 then
    If Menu.MinSpeed>FixBaud(Exitinfo.Baud) then Result := False;
  If Menu.MaxSpeed>00 then
    If Menu.MaxSpeed<FixBaud(Exitinfo.Baud) then Result := False;

  if Menu.Credit > 00 then
   If Menu.Credit>Exitinfo.Userinfo.Credit then Result := False;
  if Menu.OptionCost > 00 then
   If (Menu.OptionCost>Exitinfo.Userinfo.Credit) AND (NOT ReadBit(Exitinfo.Userinfo.Attribute2, 7))
    then Result := False;

  Temp := False;
  If ((ReadBit(Menu.TermAttrib, 0)) AND (LineCfg^.AnsiOn)) then Temp := True;
  If ((ReadBit(Menu.TermAttrib, 1)) AND (LineCfg^.AvatarOn)) then Temp := True;
  If ((ReadBit(Menu.TermAttrib, 2)) AND (LineCfg^.RipOn)) then Temp := True;

  With Menu do
   If (NOT ReadBit(TermAttrib, 0)) AND (NOT ReadBit(TermAttrib, 1)) AND
    (NOT ReadBit(TermAttrib, 2)) then Temp := True;
  If NOT Temp then Result := False;

  If NOT Check32(Menu.Node,  LineCfg^.RaNodeNr) then Result := False;
  If NOT Check32(Menu.Group, Exitinfo.Userinfo.Group) then Result := False;

  Hours := 00;
  Mins  := 00;
  While Menu.StartTime[Dow+1]>=60 do
    begin
      Inc(Hours);
      Dec(Menu.StartTime[Dow+1], 60);
    end; { While }
  Mins := Menu.StartTime[Dow+1];
  StartTime := LeadingZero(Hours, 2)+':'+LeadingZero(Mins, 2);

  Hours := 00;
  Mins  := 00;
  While Menu.StopTime[Dow+1]>=60 do
    begin;
      Inc(Hours);
      Dec(Menu.StopTime[Dow+1], 60);
    end; { while }
  Mins := Menu.StopTime[Dow+1];
  StopTime := LeadingZero(Hours, 2)+':'+LeadingZero(Mins, 2);

  If NOT CheckTimeRange(StartTime, StopTime, false) then Result := False;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logAccess, 'CheckMenuAccess ( end )');
  {$ENDIF}

 CheckMenuAccess := Result;
end; { func. CheckMenuAccess }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  CheckMsgAreaAccess(RaInf: MessageRecord; Group, Read: Boolean; GroupNr: Word;
                             const UserInf: UsersRecord): Boolean;
var BoolResult: Boolean;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logAccess, 'CheckMsgAreaAccess (begin)');
  {$ENDIF}

  BoolResult := True;

  If GroupNr=00 then GroupNr := Userinf.MsgGroup;
  If RaInf.Name='' then BoolResult := False;
  If RaInf.AreaNum=00 then BoolResult := False;
  if BoolResult then
   If Group then
    If (RaInf.Group<>GroupNr) AND
        (RaInf.AltGroup[1]<>GroupNr) AND
         (RaInf.AltGroup[2]<>GroupNr) AND
          (RaInf.AltGroup[3]<>GroupNr) AND
           (NOT ReadBit(RaInf.Attribute2, 0))
            then BoolResult := False;

  If Read then
   begin
     If RaInf.ReadSecurity > Userinf.Security then BoolResult := False
   end
     else If RaInf.WriteSecurity > Userinf.Security then BoolResult := False;

  if BoolResult then
   If Read then
    If NOT CheckFlagAccess(Userinf.Flags, RaInf.ReadFlags, RaInf.ReadNotFlags)
      then BoolResult := False;

  if BoolResult then
   If NOT Read then
    If NOT CheckFlagAccess(Userinf.Flags, RaInf.WriteFlags, RaInf.WriteNotFlags)
      then BoolResult := False;

  if BoolResult then
   If RaInf.Age > LineCfg^.UserAgeSave then BoolResult := False;

  CheckMsgAreaAccess := BoolResult;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logAccess, 'CheckMsgAreaAccess ( end )');
  {$ENDIF}
end; { func. CheckmsgAreaAccess }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function CheckGroupAccess(Group: GroupRecord; var Exitinfo: ExitinfoRecord): Boolean;
var BoolResult: Boolean;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logAccess, 'CheckGroupAccess (begin)');
  {$ENDIF}
  BoolResult:=True;

  If Group.Name = '' then BoolResult := False;
  If Group.AreaNum = 00 then BoolResult := False;

  If NOT CheckFlagAccess(Exitinfo.Userinfo.Flags, Group.Flags, Group.NotFlagsMask)
     then BoolResult := False;

  If Group.Security>Exitinfo.Userinfo.Security then BoolResult:=False;

  CheckGroupAccess := BoolResult;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logAccess, 'CheckGroupAccess ( end )');
  {$ENDIF}
end; { CheckGroupAccess }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function CheckFileAreaAccess(RaInf: FilesRecord;
                             Download, Group, Upload, List: Boolean;
                             GroupNr: Word;
                             var Exitinfo: ExitinfoRecord): Boolean;
var BoolResult: Boolean;
begin
 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logAccess, 'CheckFileAreaAccess (begin)');
 {$ENDIF}

  BoolResult := True;

  if (GroupNr = 0) then
    GroupNr := Exitinfo.Userinfo.FileGroup;

  if (Download) then 
    begin
      if (RaInf.Security > Exitinfo.Userinfo.Security) then 
        BoolResult := false;
        
      if (NOT CheckFlagAccess(Exitinfo.Userinfo.Flags, RaInf.Flags, RaInf.NotFlags)) then
        BoolResult := false;
    end; { DownloadAccess Check }

  if (Upload) then
    begin
      if (RaInf.UploadSecurity > Exitinfo.Userinfo.Security) then 
        BoolResult := false;

      if (NOT CheckFlagAccess(Exitinfo.Userinfo.Flags, RaInf.UploadFlags, RaInf.UploadNotFlags)) then
        BoolResult := false;
   end; { Upload Access Check }

  if (List) then
    begin
      if (RaInf.ListSecurity > Exitinfo.Userinfo.Security) then 
        BoolResult := false;

      if (NOT CheckFlagAccess(Exitinfo.Userinfo.Flags, RaInf.ListFlags, RaInf.ListNotFlags)) then
        BoolResult := false;
    end; { if List }

  {- Make sure the filearea is valid }
  if (Trim(RaInf.Name) = '') then 
    BoolResult := False;

  if (Trim(RaInf.FilePath) = '') then 
    BoolResult := False;
    
  if (RaInf.AreaNum =0) then 
    BoolResult := False;

  {- Check the users' age - if the UserAgeSave (cached age variable) }
  {- is zero, the cache is not filled so we calculate it at runtime }
  if( LineCfg^.UserAgeSave = 0) then
    begin
      if (RaInf.Age > GetUserAge(Exitinfo.Userinfo.Birthdate, false)) then
        BoolResult := false;
    end { if }
      else begin
             if (RaInf.Age > LineCfg^.UserAgeSave) then
               BoolResult := false;
           end; { else }
       
  {- and check the filegroups }
  if (Group) then
    begin
      if (RaInf.Group <> GroupNr) AND
          (RaInf.AltGroup[1] <> GroupNr) AND
           (RaInf.AltGroup[2] <> GroupNr) AND
            (RaInf.AltGroup[3] <> GroupNr) AND
             (NOT ReadBit(RaInf.Attrib2, 0)) then
               BoolResult := false;
     end; { if }

  {- and return the filearea access }
  CheckFileAreaAccess := BoolResult;
 
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logAccess, 'CheckFileAreaAccess ( end )');
  {$ENDIF}
end; { func. CheckFileAreaAccess }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function CheckLanguageAccess(Language: LanguageRecord;
                             CheckSecurity: Boolean;
                             var Exitinfo: ExitinfoRecord): Boolean;
begin
 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logAccess, 'CheckLanguageAccess (begin)');
 {$ENDIF}
  CheckLanguageAccess := FALSE;

  if (Language.Name <> '') AND
      ( (Language.Security < Exitinfo.Userinfo.Security) OR (NOT CheckSecurity)) AND
       ( (CheckFlagAccess(Exitinfo.Userinfo.Flags, Language.Flags, Language.NotFlagsMask)) OR (NOT CheckSecurity)) AND
        (Boolean(Language.Attribute)) then CheckLanguageAccess := TRUE;

 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logAccess, 'CheckLanguageAccess ( end )');
 {$ENDIF}
end; { func. CheckLanguageAccess }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function CheckFlagAccess(UserFlags: FlagType; On_Flags, Off_Flags: FlagType): Boolean;
Var Row        : Byte;
    Column     : Byte;
{$IFNDEF WIN32}
{$IFNDEF FPC}
 {$IFNDEF OS2}
    Result     : Boolean;
 {$ENDIF}
{$ENDIF}
{$ENDIF}
begin
 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logAccess, 'CheckFlagAccess (begin)');
 {$ENDIF}

 Result := True;

 for Column := 01 to 04 do
  for Row:=00 to 07 do
   if (ReadBit(On_Flags[Column], Row)) then
    if (NOT ReadBit(UserFlags[Column], Row)) then
      begin
        Result := false;
        CheckFlagAccess := Result;
        EXIT;
      end; { If }

 for Column := 01 to 04 do
  for Row:=00 to 07 do If (ReadBit(Off_Flags[Column], Row)) then
   if (ReadBit(UserFlags[Column], Row)) then
     begin
       Result := False;
       CheckFlagAccess := Result;
       EXIT;
     end; { If }

 CheckFlagAccess := Result;

 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logAccess, 'CheckFlagAccess ( end )');
 {$ENDIF}
end; { func. CheckFlagAccess }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function ReadMsgAccess(FromTo: String;  SysOpAccess, IsPriv: Boolean;
                       const UserInf: UsersRecord): Boolean;
begin
  ReadMsgAccess := true;
  FromTo := SUpCase(FromTo);
  if FromTo = 'ALL' then IsPriv := FALSE;

  if (FromTo <> SUpCase(userInf.Name)) AND
      (FromTo <> SUpCase(userInf.Handle)) then
       if (NOT SysOpAccess) AND (IsPriv) then
         ReadMsgAccess := false;
end; { func. ReadMsgAccess }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function SysOpAccess(MessageInf: MessageRecord; const UserInf: UsersRecord): Boolean;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailSys, 'SysOp Access: '+MessageInf.Name);
  {$ENDIF}

  SysOpAccess := False;
  If (CheckFlagAccess(Userinf.Flags,
                        MessageInf.SysOpFlags,
                        MessageInf.SysOpNotFlags))
        AND (Userinf.Security>=MessageInf.SysOpSecurity) then
                  SysopAccess := True
                   else SysOpAccess := False;
end; { func. SysOpAccess }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CheckMsgDeleteAccess(const MsgInf  : MessageRecord;
                                    MsgTo,
                                    MsgFrom : String;
                                    IsSent  : Boolean;
                                    UserInfo: UsersRecord): Boolean;
var TmpBool: Boolean;
begin
  {-- Default to not delete --------------------------------------------------}
  TmpBool := FALSE;

  {-- Sysops can always delete -----------------------------------------------}
  if SysopAccess(MsgInf, Userinfo) then
    TmpBool := TRUE;

  {-- now lets see if we can make an mroe exact match ------------------------}
  if NOT TmpBool then
    begin
      {-- make sure this area allows deletes, else we dont override ----------}
      if ReadBit(MsgInf.Attribute, 6) then
        begin
          MsgTo := Trim(SUpCase(MsgTo));
          MsgFrom := Trim(SUpCase(MsgFrom));

          Case MsgInf.Typ of
            Netmail,
            LocalMail : begin
                          if (MsgTo = SUpCase(Userinfo.Name)) OR
                              (MsgFrom = SUpCase(Userinfo.Name)) OR
                               (MsgTo = SUpCase(UserInfo.Handle)) OR
                                (MsgFrom = SUpCase(Userinfo.Handle)) then
                                   TmpBool := TRUE;
                        end; { netmail or localmail }
            Echomail : begin
                          if (MsgFrom = SUpCase(Userinfo.Name)) OR
                               (MsgFrom = SUpCase(UserInfo.Handle)) then
                                if (NOT IsSent) then
                                  TmpBool := TRUE;
                       end; { echomail }
         end; { case }
        end; { if }
    end; { if }

  {-- Set the actual result ---------------------------------------------}
  CheckMsgDeleteAccess := TmpBool;
end; { func. CheckMsgDeleteAccess }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
