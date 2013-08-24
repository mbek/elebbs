unit QaCfg;
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
** QACFG (Q-A) script routines for EleBBS.
**
** Copyright (c) 1996 - 2000 by Maarten Bekers
**
** Created : 10-Jul-2000
** Last update : 17-Sep-2000
**
**
*)

(*
** [*] Cfg_FileOpen <file#> <filename> <filesort>
** [*] Cfg_FileRead <file#>
** [*] Cfg_FileWrite <file#>
** [*] Cfg_FileSeek <file#> <variable#|position>
** [*] Cfg_GetInfo  <file#> <field-num> <variable#>
** [*] Cfg_SetInfo  <file#> <field-num> <variable#>
** [*] Cfg_FileError <file#> <variable#>
**
**)

{ Mapped: }
{ [*] Limits.ra }
{ [*] Language.ra }
{ [*] Lastcall.bbs }
{ [*] fdbxxx.hdr }
{ [*] fdbxxx.idx }
{ [*] usersidx.bbs }
{ [*] users.bbs }
{ [*] sysinfo.bbs }
{ [*] menu.mnu }
{ [*] events.ra }
{ [*] messages.ra }
{ [*] ?groups.ra }
{ [*] files.ra }
{ [*] modem.ra }
{ [*] config.ra }
{ [*] protocol.ra }
{ [*] telnet.ele }
{ [*] menu.mlb }
{ [*] nwserver.ele }
{ [*] nwsgroup.sta }
{ [*] config.ele }
{ [*] messages.ele }
{ [*] files.ele }
{ [*] useron.bbs }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses FileObj, CfgRec;

{$IFNDEF MSDOS} {$H-} {$ENDIF}

type
  qaCfgBufDataType = Array[0..0] of Char;
  qapCfgBufDataType= ^qaCfgBufDataType;

  qaFileSortTypeType = (cfgf_Limits, cfgf_Language, cfgf_Lastcall,
                        cfgf_FdbHdr, cfgf_FdbIdx, cfgf_UsersBbs,
                        cfgf_UsersIdx, cfgf_SysInfo, cfgf_Menu,
                        cfgf_Events, cfgf_Messages, cfgf_Groups,
                        cfgf_Files, cfgf_Modem, cfgf_Config,
                        cfgf_Protocol, cfgf_Telnet, cfgf_Lightbar,
                        cfgf_NewsServer, cfgf_NewsGroup, cfgf_ConfigEle,
                        cfgf_MessagesEle, cfgf_FilesEle, cfgf_UseronBbs);


  qaCfgFileTypeRecord  = record
                           BufSize: Longint;
                         end; { if }
  qaFileSortTypeRecord = record
                           pc_FileSort   : qaFileSortTypeType;
                           human_FileSort: String;
                         end; { if }

const
  qaMaxCfgFiles = 5;
  qaCfgFileSize : Array[qaFileSortTypeType] of qaCfgFileTypeRecord = ((BufSize: SizeOf(LimitsRecord)),
                                                                      (BufSize: SizeOf(LanguageRecord)),
                                                                      (BufSize: SizeOf(LastCallRecord)),
                                                                      (BufSize: SizeOf(FilesHdrRecord)),
                                                                      (BufSize: SizeOf(FilesIdxRecord)),
                                                                      (BufSize: SizeOf(UsersRecord)),
                                                                      (BufSize: SizeOf(UsersIdxRecord)),
                                                                      (BufSize: SizeOf(SysInfoRecord)),
                                                                      (BufSize: SizeOf(MnuRecord)),
                                                                      (BufSize: SizeOf(EventRecord)),
                                                                      (BufSize: SizeOf(MessageRecord)),
                                                                      (BufSize: SizeOf(GroupRecord)),
                                                                      (BufSize: SizeOf(FilesRecord)),
                                                                      (BufSize: SizeOf(ModemRecord)),
                                                                      (BufSize: SizeOf(ConfigRecord)),
                                                                      (BufSize: SizeOf(ProtocolRecord)),
                                                                      (BufSize: SizeOf(TelnetRecord)),
                                                                      (BufSize: SizeOf(LightbarRecord)),
                                                                      (BufSize: SizeOf(NewsServerRecord)),
                                                                      (BufSize: SizeOf(NewsGroupStat)),
                                                                      (BufSize: SizeOf(EleConfigRecord)),
                                                                      (BufSize: SizeOf(EleMessageRecord)),
                                                                      (BufSize: SizeOf(EleFilesRecord)),
                                                                      (BufSize: SizeOf(UseronRecord)));


  qaFileSortType: Array[1..24] of qaFileSortTypeRecord = ((pc_FileSort: cfgf_Limits; human_FileSort: 'LIMITS'),
                                                          (pc_FileSort: cfgf_Language; human_FileSort: 'LANGUAGE'),
                                                          (pc_FileSort: cfgf_Lastcall; human_FileSort: 'LASTCALL'),
                                                          (pc_FileSort: cfgf_FdbHdr; human_FileSort: 'FDBHDR'),
                                                          (pc_FileSort: cfgf_FdbIdx; human_FileSort: 'FDBIDX'),
                                                          (pc_FileSort: cfgf_UsersBbs; human_FileSort: 'USERSBBS'),
                                                          (pc_FileSort: cfgf_UsersIdx; human_FileSort: 'USERSIDX'),
                                                          (pc_FileSort: cfgf_SysInfo; human_FileSort: 'SYSINFO'),
                                                          (pc_FileSort: cfgf_Menu; human_FileSort: 'MENU'),
                                                          (pc_FileSort: cfgf_Events; human_FileSort: 'EVENTS'),
                                                          (pc_FileSort: cfgf_Messages; human_FileSort: 'MESSAGES'),
                                                          (pc_FileSort: cfgf_Groups; human_FileSort: 'GROUP'),
                                                          (pc_FileSort: cfgf_Files; human_FileSort: 'FILES'),
                                                          (pc_FileSort: cfgf_Modem; human_FileSort: 'MODEM'),
                                                          (pc_FileSort: cfgf_Config; human_FileSort: 'CONFIG'),
                                                          (pc_FileSort: cfgf_Protocol; human_FileSort: 'PROTOCOL'),
                                                          (pc_FileSort: cfgf_Telnet; human_FileSort: 'TELNET'),
                                                          (pc_FileSort: cfgf_Lightbar; human_FileSort: 'LIGHTBAR'),
                                                          (pc_FileSort: cfgf_NewsServer; human_FileSort: 'NEWSSERVER'),
                                                          (pc_FileSort: cfgf_NewsGroup; human_FileSort: 'NEWSGROUP'),
                                                          (pc_FileSort: cfgf_ConfigEle; human_FileSort: 'CONFIGELE'),
                                                          (pc_FileSort: cfgf_MessagesEle; human_FileSort: 'MESSAGESELE'),
                                                          (pc_FileSort: cfgf_FilesEle; human_FileSort: 'FILESELE'),
                                                          (pc_FileSort: cfgf_UseronBbs; human_FileSort: 'USERONBBS'));

type CfgFileRecord = record
                       FileName  : String;
                       FileHandle: pFileObj;
                       FileSort  : qaFileSortTypeType;
                       ErrorCode : Longint;
                       BufData   : qapCfgBufDataType;
                     end; { record }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  StrToFileSort(FileSortStr: String): qaFileSortTypeType;


procedure CfgFile_GetSetSmallWord(var Value: String; var WordValue: SmallWord);
procedure CfgFile_GetSetSmallInt(var Value: String; var IntValue: SmallInt);
procedure CfgFile_GetSetTpReal(var Value: String; var RealValue: TPReal);
procedure CfgFile_GetSetResetType(var Value: String; var ResetValue: ResetType);
procedure CfgFile_GetSetString(var Value: String; MaxLen: Longint; var StringValue: String);
procedure CfgFile_GetSetByte(var Value: String; var ByteValue: Byte);
procedure CfgFile_GetSetChar(var Value: String; var CharValue: Char);
procedure CfgFile_GetSetBoolean(var Value: String; var Bool: Boolean);
procedure CfgFile_GetSetMsgType(var Value: String; var MsgTypeValue: MsgType);
procedure CfgFile_GetSetMsgKinds(var Value: String; var MsgKindValue: MsgKindsType);
procedure CfgFile_GetSetAskType(var Value: String; var AskTypeValue: AskType);
procedure CfgFile_GetSetNetAddress(var Value: String; var NetAddressValue: NetAddress);
procedure CfgFile_GetSetProtAttrib(var Value: String; var ByteValue: Byte);
procedure CfgFile_GetSetArcRecord(var Value: String; var ArcRecordValue: ArcRecord);
procedure CfgFile_GetSetCombined(var Value: String; var CombinedValue: CombinedRecord);
procedure CfgFile_GetSetAccessType(var Value: String; var AccessValue: AccessType);
procedure CfgFile_GetSetLongint(var Value: String; var LongValue: Longint);
procedure CfgFile_GetSetUnixTime(var Value: String; var UnixValue: Longint);


procedure CfgFile_MapLimitsRa(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
procedure CfgFile_MapLanguageRa(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
procedure CfgFile_MapLastcallBbs(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
procedure CfgFile_MapFdbHdr(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
procedure CfgFile_MapFdbIdx(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
procedure CfgFile_MapUsersIdx(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
procedure CfgFile_MapUsersBbs(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
procedure CfgFile_MapSysinfoBbs(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
procedure CfgFile_MapMenuFile(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
procedure CfgFile_MapEventRa(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
procedure CfgFile_MapMessageRa(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
procedure CfgFile_MapGroupsRa(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
procedure CfgFile_MapFilesRa(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
procedure CfgFile_MapModemRa(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
procedure CfgFile_MapConfigRa(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
procedure CfgFile_MapProtocolRa(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
procedure CfgFile_MapTelnetEle(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
procedure CfgFile_MapLightbarFile(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
procedure CfgFile_MapNewsServerFile(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
procedure CfgFile_MapNewsGroupStats(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
procedure CfgFile_MapConfigEle(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
procedure CfgFile_MapMessagesEle(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
procedure CfgFile_MapFilesEle(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
procedure CfgFile_MapUseronBBS(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

 uses LongStr, Cases, StUtils, WordStr, CentrStr, FlagStr,
       BitWise, UnixDate;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function StrToFileSort(FileSortStr: String): qaFileSortTypeType;
var Counter: Longint;
begin
  FileSortStr := SupCase(Trim(FileSortStr));

  {-- Convert this string to a easier format --------------------------------}
  for Counter := Low(qaFileSortType) to High(qaFileSortType) do
    with qaFileSortType[Counter] do
     if human_FileSort = FileSortStr then
       begin
         StrToFileSort := pc_FileSort;
         BREAK;
       end; { if }
end; { func. StrToFileSort }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_GetSetSmallWord(var Value: String; var WordValue: SmallWord);
begin
  if Value = '' then
    Value := FStr(WordValue)
     else WordValue := FVal(Value);
end; { proc. CfgFile_GetSetSmallWord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_GetSetSmallInt(var Value: String; var IntValue: SmallInt);
begin
  if Value = '' then
    Value := FStr(IntValue)
     else IntValue := FVal(Value);
end; { proc. CfgFile_GetSetSmallInt }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_GetSetLongint(var Value: String; var LongValue: Longint);
begin
  if Value = '' then
    Value := FStr(LongValue)
     else LongValue := FVal(Value);
end; { proc. CfgFile_GetSetLongint }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_GetSetByte(var Value: String; var ByteValue: Byte);
begin
  if Value = '' then
    Value := FStr(ByteValue)
     else ByteValue := FVal(Value);
end; { proc. CfgFile_GetSetByte }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_GetSetUnixTime(var Value: String; var UnixValue: Longint);
var Year  : Word;
    Month : Word;
    Day   : Word;
    Hour  : Word;
    Min   : Word;
    Sec   : Word;
begin
  if Value = '' then
    begin
      Unix2Norm(UnixValue, year, month, day, hour, min, sec);

      { We return: MM-DD-YYYY HH:MM:SS }
                  {1234578901234567890 }
      Value := LeadingZero(Month, 2) + '-' +
               LeadingZero(Day, 2) + '-' +
               LeadingZero(Year, 4) + #32 +
               LeadingZero(Hour, 2) + ':' +
               LeadingZero(Min, 2) + ':' +
               LeadingZero(Sec, 2);
    end { if }
      else begin
             Month := FVal(Copy(Value, 1, 2));
             Day := FVal(Copy(Value, 4, 2));
             Year := FVal(Copy(Value, 8, 4));

             Hour := FVal(Copy(Value, 13, 2));
             Min := FVal(Copy(Value, 16, 2));
             Sec := FVal(Copy(Value, 19, 2));

             UnixValue := Norm2Unix(Year, Month, Day, Hour, Min, Sec);
           end; { else }
end; { proc. CfgFile_GetSetUnixTime }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_GetSetChar(var Value: String; var CharValue: Char);
begin
  if Value = '' then
    Value := CharValue
     else CharValue := Value[1];
end; { proc. CfgFile_GetSetChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_GetSetString(var Value: String; MaxLen: Longint; var StringValue: String);
begin
  if Value = '' then
    Value := StringValue
     else StringValue := Copy(Value, 1, MaxLen);
end; { proc. CfgFile_GetSetString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_GetSetNetAddress(var Value: String; var NetAddressValue: NetAddress);
begin
  if Value = '' then
    begin
      with NetAddressValue do
        AddrToString(Value, Zone, Net, Node, Point);
    end
     else begin
            With NetAddressValue do
              StringToAddr(Value, Zone, Net, Node, Point);
          end; { else }
end; { proc. CfgFile_GetSetNetAddress }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_GetSetFlagType(var Value: String; FlagIndex: Longint; var FlagValue: FlagType);
begin
  if Value = '' then
    Value := Byte2Flags(FlagValue[FlagIndex])
     else FlagValue[FlagIndex] := Str2Flags(Value);
end; { proc. CfgFile_GetSetFlagType }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_GetSetTpReal(var Value: String; var RealValue: TPReal);
var TmpStr: String;
begin
  if Value = '' then
    begin
      {$IFNDEF FPC}
        Str(RealValue, TmpStr);
      {$ENDIF}
      Value := TmpStr;
    end
      else {$IFNDEF FPC} RealValue := StrToReal(Value) {$ENDIF} ;
end; { proc. CfgFile_GetSetTpReal }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_GetSetArcRecord(var Value: String; var ArcRecordValue: ArcRecord);
var Tmp: Longint;
begin
  if WordCount(Value, defExtractWord, false) <= 1 then
    begin
      Tmp := FVal(ExtractWord(Value, 1, defExtractWord, true, false));

      if Tmp in [1..3] then
        begin
          Case Tmp of
            1 : Value := ArcRecordValue.Extension;
            2 : Value := ArcRecordValue.Unpackcmd;
            3 : Value := ArcRecordValue.PackCmd;
          end; { case }
        end
          else Value := 'You need to specify type of info you want';
    end
      else begin
             Tmp := FVal(ExtractWord(Value, 1, defExtractWord, true, false));

             if Tmp in [1..3] then
               begin
                 Case Tmp of
                   1 : ArcRecordValue.Extension := Value;
                   2 : ArcRecordValue.Unpackcmd := Value;
                   3 : ArcRecordValue.PackCmd := Value;
                 end; { case }
               end
                 else Value := 'You need to specify type of info you want';
           end; { if }
end; { proc. CfgFile_GetSetArcRecord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_GetSetCombined(var Value: String; var CombinedValue: CombinedRecord);
var Tmp: Longint;
begin
  if WordCount(Value, defExtractWord, false) <= 1 then
    begin
      Tmp := FVal(ExtractWord(Value, 1, defExtractWord, true, false));

      if Tmp in [1..200] then
        begin
          Value := fStr(CombinedValue[Tmp]);
        end
          else Value := 'You need to specify type of info you want';
    end
      else begin
             Tmp := FVal(ExtractWord(Value, 1, defExtractWord, true, false));

             if Tmp in [1..200] then
               begin
                 CombinedValue[Tmp] := fVal(Value);
               end
                 else Value := 'You need to specify type of info you want';
           end; { if }
end; { proc. CfgFile_GetSetCombined }

procedure CfgFile_GetSetBit(var Value: String; BitIndex: Longint; var Bit: Byte);
begin
  if Value = '' then
    begin
      if ReadBit(Bit, BitIndex) then
        Value := 'ON'
          else Value := 'OFF';
    end
      else begin
             if Value = 'ON' then
               SetBit(Bit, BitIndex);

             if Value = 'OFF' then
               ClearBit(Bit, BitIndex);
           end; { else }
end; { proc. CfgFile_GetSetFlagType }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_GetSetBoolean(var Value: String; var Bool: Boolean);
begin
  if Value = '' then
    begin
      if Bool then
        Value := 'TRUE'
          else Value := 'FALSE';
    end
      else begin
             if Value = 'TRUE' then Bool := TRUE;

             if Value = 'OFF' then Bool := FALSE;
           end; { else }
end; { proc. CfgFile_GetSetFlagType }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_GetSetAskType(var Value: String; var AskTypeValue: AskType);
begin
  if Value = '' then
    begin
      Case AskTypeValue of
        Yes  : Value := 'Yes';
        No   : Value := 'No';
        Ask  : Value := 'Ask';
        Only : Value := 'Only';
      end; { case }
    end
      else begin
             if Value = 'YES' then AskTypeValue := Yes
               else if Value = 'NO' then AskTypeValue := No
                 else if Value = 'ASK' then AskTypeValue := Ask
                   else if Value = 'ONLY' then AskTypeValue := Only;
           end; { else }

end; { proc. GetSetAskType }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_GetSetAccessType(var Value: String; var AccessValue: AccessType);
begin
  if Value = '' then
    begin
      Case AccessValue of
        nwsPostNever      : Value := 'Never';
        nwsPostAlways     : Value := 'Always';
        nwsPostUseSettings: Value := 'Use settings';
        nwsUseBoth        : Value := 'Both';
      end; { case }
    end
      else begin
             if Value = 'NEVER' then AccessValue := nwsPostNever
               else if Value = 'ALWAYS' then AccessValue := nwsPostAlways
                 else if Value = 'USE SETTINGS' then AccessValue := nwsPostUseSettings
                  else if Value = 'BOTH' then AccessValue := nwsUseBoth
           end; { else }

end; { proc. GetSetAccessType }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_GetSetProtAttrib(var Value: String; var ByteValue: Byte);
begin
  if Value = '' then
    begin
      Case ByteValue of
        1 : Value := 'Not available';
        2 : Value := 'Available';
        3 : Value := 'Error free';
      end; { case }
    end
      else begin
             if Value = 'NOT AVAILABLE' then ByteValue := 1
               else if Value = 'AVAILABLE' then ByteValue := 2
                 else if Value = 'ERROR FREE' then ByteValue := 3;
           end; { else }
end; { proc. CfgFile_GetSetProtAttrib }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_GetSetResetType(var Value: String; var ResetValue: ResetType);
begin
  if Value = '' then
    begin
      Case ResetValue of
        Never  : Value := 'Never';
        Week   : Value := 'Week';
        Month  : Value := 'Month';
        Year   : Value := 'Year';
      end; { case }
    end
     else begin
            if Value = 'NEVER' then ResetValue := Never
             else if Value = 'WEEK' then ResetValue := Week
              else if Value = 'MONTH' then Resetvalue := Month
               else if Value = 'YEAR' then ResetValue := Year;
          end; { else }
end; { proc. CfgFile_GetSetResetType }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_GetSetMsgType(var Value: String; var MsgTypeValue: MsgType);
begin
  if Value = '' then
    begin
      Case MsgTypeValue of
        Localmail : Value := 'Localmail';
        Netmail   : Value := 'Netmail';
        Echomail  : Value := 'Echomail';
        Internet  : Value := 'Internet';
        Newsgroup : Value := 'Newsgroup';
      end; { case }
    end
     else begin
            if Value = 'LOCALMAIL' then MsgTypeValue := LocalMail
              else if Value = 'NETMAIL' then MsgTypeValue := NetMail
               else if Value = 'ECHOMAIL' then MsgTypeValue := EchoMail
                 else if Value = 'INTERNET' then MsgTypeValue := Internet
                  else if Value = 'NEWSGROUP' then MsgTypeValue := Newsgroup;
          end; { else }
end; { proc. CfgFile_GetSetMsgType }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_GetSetMsgKinds(var Value: String; var MsgKindValue: MsgKindsType);
begin
  if Value = '' then
    begin
      Case MsgKindValue of
        eleMsgBoth    : Value := 'Both';
        eleMsgPrivate : Value := 'Private';
        eleMsgPublic  : Value := 'Public';
        eleMsgRonly   : Value := 'Read-only';
        eleMsgNoReply : Value := 'No-reply';
      end; { case }
    end
     else begin
            if Value = 'BOTH' then MsgKindValue := eleMsgBoth
              else if Value = 'PRIVATE' then MsgKindValue := eleMsgPrivate
               else if Value = 'PUBLIC' then MsgKindValue := eleMsgPublic
                 else if Value = 'READ-ONLY' then MsgKindValue := eleMsgRonly
                  else if Value = 'NO-REPLY' then MsgKindValue := eleMsgNoReply;
          end; { else }
end; { proc. CfgFile_GetSetMsgKinds }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_GetSetMenuTime(var Value: String; TimeValue: SmallWord);
var Tmp  : Longint;
    Hours: Longint;
    Mins : Longint;
begin
  if Value = '' then
    begin
      Value := Word2Time(TimeValue);
    end
     else begin
            {-- Convert from strings to value -------------------------------}
            if Pos(':', Value) = 0 then
              begin
                Hours := 0;
                Mins := FVal(Value);
              end
                else begin
                       Tmp := Pos(':', Value);

                       Hours := FVal(Copy(Value, 1, Tmp - 1));
                       Mins := FVal(Copy(Value, Tmp + 1, Length(Value)));
                     end; { else }

            Tmp := 0;

            While (Hours > 0) do
              begin
                Inc(Tmp, 60);
                Dec(Hours);
              end; { while }

            Inc(Tmp, Mins);

            {-- Put the result ----------------------------------------------}
            TimeValue := SmallWord(Tmp);
          end; { else }
end; { proc. CfgFile_GetSetMenuTime }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_GetSetDateFormat(var Value: String; var DateValue: Byte);
begin
  if Value = '' then
    begin
      Case DateValue of
        1 : Value := 'DD-MM-YY';
        2 : Value := 'MM-DD-YY';
        3 : Value := 'YY-MM-DD';
        4 : Value := 'DD-Mmm-YY';
        5 : Value := 'DD-MM-YYYY ';
        6 : Value := 'MM-DD-YYYY ';
        7 : Value := 'YYYY-MM-DD ';
        8 : Value := 'DD-Mmm-YYYY';
      end; { case }
    end
     else begin
            if Value = 'DD-MM-YY' then DateValue := 1
             else if Value = 'MM-DD-YY' then DateValue := 2
               else if Value = 'YY-MM-DD' then DateValue := 3
                 else if Value = 'DD-Mmm-YY' then DateValue := 4
                   else if Value = 'DD-MM-YYYY' then DateValue := 5
                     else if Value = 'MM-DD-YYYY' then DateValue := 6
                       else if Value = 'YYYY-MM-DD' then DateValue := 7
                         else if Value = 'DD-Mmm-YYYY' then DateValue := 8;
          end; { else }
end; { proc. CfgFile_GetSetResetType }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_MapLanguageRa(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
var Language: LanguageRecord;
begin
  Move(Buffer, Language, SizeOf(LanguageRecord));

  With Language do
    Case FieldName of
      01 : CfgFile_GetSetString(Value, 20, Name);
      02 : CfgFile_GetSetByte(Value, Attribute);
      03 : CfgFile_GetSetString(Value, 60, DefName);
      04 : CfgFile_GetSetString(Value, 60, MenuPath);
      05 : CfgFile_GetSetString(Value, 60, TextPath);
      06 : CfgFile_GetSetString(Value, 60, QuesPath);
      07 : CfgFile_GetSetSmallWord(Value, Security);
      08 : CfgFile_GetSetFlagType(Value, 1, Flags);
      09 : CfgFile_GetSetFlagType(Value, 2, Flags);
      10 : CfgFile_GetSetFlagType(Value, 3, Flags);
      11 : CfgFile_GetSetFlagType(Value, 4, Flags);
      12 : CfgFile_GetSetFlagType(Value, 1, NotFlagsMask);
      13 : CfgFile_GetSetFlagType(Value, 2, NotFlagsMask);
      14 : CfgFile_GetSetFlagType(Value, 3, NotFlagsMask);
      15 : CfgFile_GetSetFlagType(Value, 4, NotFlagsMask);
     end; { case }

  Move(Language, Buffer, SizeOf(LanguageRecord));
end; { proc. CfgFile_MapLanguageRa }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_MapLastcallBbs(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
var Lastcall: LastcallRecord;
begin
  Move(Buffer, Lastcall, SizeOf(LastcallRecord));

  With Lastcall do
    Case FieldName of
      01 : CfgFile_GetSetByte(Value, Line);
      02 : CfgFile_GetSetString(Value, 35, Name);
      03 : CfgFile_GetSetString(Value, 35, Handle);
      04 : CfgFile_GetSetString(Value, 25, City);
      05 : CfgFile_GetSetSmallWord(Value, Baud);
      06 : CfgFile_GetSetLongint(Value, Times);
      07 : CfgFile_GetSetString(Value, 5, LogOn);
      08 : CfgFile_GetSetString(Value, 5, LogOff);
      09 : CfgFile_GetSetBit(Value, 0, Attribute);
    end; { case }

  Move(Lastcall, Buffer, SizeOf(LastcallRecord));
end; { proc. CfgFile_MapLastcallBBS }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_MapFdbHdr(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
var FdbHdr: FilesHdrRecord;
begin
  Move(Buffer, FdbHdr, SizeOf(FilesHdrRecord));

  With FdbHdr do
    Case FieldName of
      01 : CfgFile_GetSetString(Value, 12, Name);
      02 : CfgFile_GetSetLongint(Value, Size);
      03 : CfgFile_GetSetLongint(Value, CRC32);
      04 : CfgFile_GetSetString(Value, 35, Uploader);
      05 : CfgFile_GetSetLongint(Value, UploadDate);
      06 : CfgFile_GetSetLongint(Value, FileDate);
      07 : CfgFile_GetSetLongint(Value, LastDL);
      08 : CfgFile_GetSetSmallWord(Value, TimesDL);
      09 : CfgFile_GetSetBit(Value, 0, Attrib); { Bit 0 : Deleted }
      10 : CfgFile_GetSetBit(Value, 1, Attrib); {     1 : Unlisted }
      11 : CfgFile_GetSetBit(Value, 2, Attrib); {     2 : Free (don't adjust ratio) - Does NOT affect "Cost" }
      12 : CfgFile_GetSetBit(Value, 3, Attrib); {     3 : Not available (don't allow downloads) }
      13 : CfgFile_GetSetBit(Value, 4, Attrib); {     4 : Locked (no kill) }
      14 : CfgFile_GetSetBit(Value, 5, Attrib); {     5 : Missing/offline }
      15 : CfgFile_GetSetBit(Value, 6, Attrib); {     6 : No time restrictions - always allow DL }
      16 : CfgFile_GetSetString(Value, 15, PassWord);
      17 : CfgFile_GetSetString(Value, 15, KeyWord[1]);
      18 : CfgFile_GetSetString(Value, 15, KeyWord[2]);
      19 : CfgFile_GetSetString(Value, 15, KeyWord[3]);
      20 : CfgFile_GetSetString(Value, 15, KeyWord[4]);
      21 : CfgFile_GetSetString(Value, 15, KeyWord[5]);
      22 : CfgFile_GetSetSmallWord(Value, Cost);
      23 : CfgFile_GetSetLongint(Value, LongDescPtr);
      24 : CfgFile_GetSetLongint(Value, LfnPtr);
    end; { case }

  Move(FdbHdr, Buffer, SizeOf(FilesHdrRecord));
end; { proc. CfgFile_MapFdbHdr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_MapUsersBbs(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
var UserInf: UsersRecord;
begin
  Move(Buffer, UserInf, SizeOf(UsersRecord));

  With UserInf do
    Case FieldName of
      01 : CfgFile_GetSetString(Value, SizeOf(msgToIdxRecord), Name);
      02 : CfgFile_GetSetString(Value, 25, Location);
      03 : CfgFile_GetSetString(Value, 50, Organisation);
      04 : CfgFile_GetSetString(Value, 50, Address1);
      05 : CfgFile_GetSetString(Value, 50, Address2);
      06 : CfgFile_GetSetString(Value, 50, Address3);
      07 : CfgFile_GetSetString(Value, 35, Handle);
      08 : CfgFile_GetSetString(Value, 80, Comment);
      09 : CfgFile_GetSetLongint(Value, PasswordCRC);
      10 : CfgFile_GetSetString(Value, 15, DataPhone);
      11 : CfgFile_GetSetString(Value, 15, VoicePhone);
      12 : CfgFile_GetSetString(Value, 5, LastTime);
      13 : CfgFile_GetSetString(Value, 8, LastDate);
      14 : CfgFile_GetSetBit(Value, 0, Attribute); { Bit 0 : Deleted }
      15 : CfgFile_GetSetBit(Value, 1, Attribute); {     1 : Clear screen }
      16 : CfgFile_GetSetBit(Value, 2, Attribute); {     2 : More prompt }
      17 : CfgFile_GetSetBit(Value, 3, Attribute); {     3 : ANSI }
      18 : CfgFile_GetSetBit(Value, 4, Attribute); {     4 : No-kill }
      19 : CfgFile_GetSetBit(Value, 5, Attribute); {     5 : Xfer priority }
      20 : CfgFile_GetSetBit(Value, 6, Attribute); {     6 : Full screen msg editor }
      21 : CfgFile_GetSetBit(Value, 7, Attribute); {     7 : Quiet mode }
      22 : CfgFile_GetSetBit(Value, 0, Attribute2);{ Bit 0 : Hot-keys }
      23 : CfgFile_GetSetBit(Value, 0, Attribute2);{     1 : AVT/0 }
      24 : CfgFile_GetSetBit(Value, 0, Attribute2);{     2 : Full screen message viewer }
      25 : CfgFile_GetSetBit(Value, 0, Attribute2);{     3 : Hidden from userlist }
      26 : CfgFile_GetSetBit(Value, 0, Attribute2);{     4 : Page priority }
      27 : CfgFile_GetSetBit(Value, 0, Attribute2);{     5 : No echomail in mailbox scan }
      28 : CfgFile_GetSetBit(Value, 0, Attribute2);{     6 : Guest account }
      29 : CfgFile_GetSetBit(Value, 0, Attribute2);{     7 : Post bill enabled }
      30 : CfgFile_GetSetFlagType(Value, 0, Flags);
      31 : CfgFile_GetSetFlagType(Value, 1, Flags);
      32 : CfgFile_GetSetFlagType(Value, 2, Flags);
      33 : CfgFile_GetSetFlagType(Value, 3, Flags);
      34 : CfgFile_GetSetLongint(Value, Credit);
      35 : CfgFile_GetSetLongint(Value, Pending);
      36 : CfgFile_GetSetSmallWord(Value, MsgsPosted);
      37 : CfgFile_GetSetSmallWord(Value, Security);
      38 : CfgFile_GetSetLongint(Value, LastRead);
      39 : CfgFile_GetSetLongint(Value, NoCalls);
      40 : CfgFile_GetSetLongint(Value, Uploads);
      41 : CfgFile_GetSetLongint(Value, Downloads);
      42 : CfgFile_GetSetLongint(Value, UploadsK);
      43 : CfgFile_GetSetLongint(Value, DownloadsK);
      44 : CfgFile_GetSetLongint(Value, TodayK);
      45 : CfgFile_GetSetSmallInt(Value, Elapsed);
      46 : CfgFile_GetSetSmallWord(Value, ScreenLength);
      47 : CfgFile_GetSetByte(Value, LastPwdChange);
      48 : CfgFile_GetSetSmallWord(Value, Group);
      49 : CfgFile_GetSetCombined(Value, CombinedInfo);
      50 : CfgFile_GetSetString(Value, 8, FirstDate);
      51 : CfgFile_GetSetString(Value, 8, BirthDate);
      52 : CfgFile_GetSetString(Value, 8, SubDate);
      53 : CfgFile_GetSetByte(Value, ScreenWidth);
      54 : CfgFile_GetSetByte(Value, Language);
      55 : CfgFile_GetSetDateFormat(Value, DateFormat);
                     {  No.      Sort of format to use : }
                     {  01       DD-MM-YY        }
                     {  02       MM-DD-YY        }
                     {  03       YY-MM-DD        }
                     {  04       DD-Mmm-YY       }
                     {  05       DD-MM-YYYY      }
                     {  06       MM-DD-YYYY      }
                     {  07       YYYY-MM-DD      }
                     {  08       DD-Mmm-YYYY     }
      56 : CfgFile_GetSetString(Value, 35, ForwardTo);
      57 : CfgFile_GetSetSmallWord(Value, MsgArea);
      58 : CfgFile_GetSetSmallWord(Value, FileArea);
      59 : CfgFile_GetSetChar(Value, DefaultProtocol);
      60 : CfgFile_GetSetSmallWord(Value, FileGroup);
      61 : CfgFile_GetSetByte(Value, LastDOBCheck);
      62 : CfgFile_GetSetByte(Value, Sex);
      63 : CfgFile_GetSetLongint(Value, XIrecord);
      64 : CfgFile_GetSetSmallWord(Value, MsgGroup);
      65 : CfgFile_GetSetBit(Value, 0, Attribute3);
      66 : CfgFile_GetSetString(Value, 15, PassWord);
    end; { case }

  Move(UserInf, Buffer, SizeOf(UsersRecord));
end; { proc. CfgFile_MapusersBBS }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_MapFdbIdx(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
var FdbIdx: FilesIdxRecord;
begin
  Move(Buffer, FdbIdx, SizeOf(FilesIdxRecord));

  With FdbIdx do
    Case FieldName of
      01 : CfgFile_GetSetString(Value, 12, Name);
      02 : CfgFile_GetSetLongint(Value, KeywordCrc[1]);
      03 : CfgFile_GetSetLongint(Value, KeywordCrc[2]);
      04 : CfgFile_GetSetLongint(Value, KeywordCrc[3]);
      05 : CfgFile_GetSetLongint(Value, KeywordCrc[4]);
      06 : CfgFile_GetSetLongint(Value, KeywordCrc[5]);
      07 : CfgFile_GetSetLongint(Value, LongDescPtr);
    end; { case }

  Move(FdbIdx, Buffer, SizeOf(FilesIdxRecord));
end; { proc. CfgFile_MapFdbIdx }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_MapUsersIdx(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
var UsersIdx: UsersIdxRecord;
begin
  Move(Buffer, UsersIdx, SizeOf(UsersIdxRecord));

  With UsersIdx do
    Case FieldName of
      01 : CfgFile_GetSetLongint(Value, NameCRC32);
      02 : CfgFile_GetSetLongint(Value, HandleCRC32);
    end; { case }

  Move(UsersIdx, Buffer, SizeOf(UsersIdxRecord));
end; { proc. CfgFile_MapUsersIDX }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_MapSysinfoBbs(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
var SysInfoBbs: SysInfoRecord;
begin
  Move(Buffer, SysInfoBBS, SizeOf(SysInfoRecord));

  With SysInfoBBS do
    Case FieldName of
      01 : CfgFile_GetSetLongint(Value, TotalCalls);
      02 : CfgFile_GetSetString(Value, SizeOf(MsgToIdxRecord), Lastcaller);
      03 : CfgFile_GetSetString(Value, SizeOf(MsgToIdxRecord), LastHandle);
    end; { case }

  Move(SysInfoBBS, Buffer, SizeOf(SysInfoRecord));
end; { proc. CfgFile_MapSysInfoBBS }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_MapEventRa(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
var EventRa: EventRecord;
begin
  Move(Buffer, EventRa, SizeOf(EventRecord));

  With EventRa do
    Case FieldName of
      01 : CfgFile_GetSetByte(Value, Status);
      02 : CfgFile_GetSetString(Value, 5, StartTime);
      03 : CfgFile_GetSetByte(Value, ErrorLevel);
      04 : CfgFile_GetSetByte(Value, Days);
      05 : CfgFile_GetSetBoolean(Value, Forced);
      07 : CfgFile_GetSetString(Value, 8, LastTimeRun);
    end; { case }

  Move(EventRA, Buffer, SizeOf(EventRecord));
end; { proc. CfgFile_MapEventRa }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_MapMessageRa(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
var MessageRa: MessageRecord;
begin
  Move(Buffer, MessageRa, SizeOf(MessageRecord));

  With MessageRa do
    Case FieldName of
      01 : CfgFile_GetSetSmallWord(Value, AreaNum);
      02 : CfgFile_GetSetString(Value, 40, Name);
      03 : CfgFile_GetSetMsgType(Value, Typ);
      04 : CfgFile_GetSetMsgKinds(Value, MsgKinds);
      05 : CfgFile_GetSetBit(Value, 0, Attribute); { Bit 0 : Enable EchoInfo }
      06 : CfgFile_GetSetBit(Value, 1, Attribute); {     1 : Combined access }
      07 : CfgFile_GetSetBit(Value, 2, Attribute); {     2 : File attaches }
      08 : CfgFile_GetSetBit(Value, 3, Attribute); {     3 : Allow aliases }
      09 : CfgFile_GetSetBit(Value, 4, Attribute); {     4 : Use SoftCRs as characters }
      10 : CfgFile_GetSetBit(Value, 5, Attribute); {     5 : Force handle }
      11 : CfgFile_GetSetBit(Value, 6, Attribute); {     6 : Allow deletes }
      12 : CfgFile_GetSetBit(Value, 7, Attribute); {     7 : Is a JAM area }
      13 : CfgFile_GetSetByte(Value, DaysKill);
      14 : CfgFile_GetSetByte(Value, RecvKill);
      15 : CfgFile_GetSetSmallWord(Value, CountKill);
      16 : CfgFile_GetSetSmallWord(Value, ReadSecurity);
      17 : CfgFile_GetSetFlagType(Value, 1, ReadFlags);
      18 : CfgFile_GetSetFlagType(Value, 2, ReadFlags);
      19 : CfgFile_GetSetFlagType(Value, 3, ReadFlags);
      20 : CfgFile_GetSetFlagType(Value, 4, ReadFlags);
      21 : CfgFile_GetSetFlagType(Value, 1, ReadNotFlags);
      22 : CfgFile_GetSetFlagType(Value, 2, ReadNotFlags);
      23 : CfgFile_GetSetFlagType(Value, 3, ReadNotFlags);
      24 : CfgFile_GetSetFlagType(Value, 4, ReadNotFlags);
      25 : CfgFile_GetSetSmallWord(Value, WriteSecurity);
      26 : CfgFile_GetSetFlagType(Value, 1, WriteFlags);
      27 : CfgFile_GetSetFlagType(Value, 2, WriteFlags);
      28 : CfgFile_GetSetFlagType(Value, 3, WriteFlags);
      29 : CfgFile_GetSetFlagType(Value, 4, WriteFlags);
      30 : CfgFile_GetSetFlagType(Value, 1, WriteNotFlags);
      31 : CfgFile_GetSetFlagType(Value, 2, WriteNotFlags);
      32 : CfgFile_GetSetFlagType(Value, 3, WriteNotFlags);
      33 : CfgFile_GetSetFlagType(Value, 4, WriteNotFlags);
      34 : CfgFile_GetSetSmallWord(Value, SysopSecurity);
      35 : CfgFile_GetSetFlagType(Value, 1, SysopFlags);
      36 : CfgFile_GetSetFlagType(Value, 2, SysopFlags);
      37 : CfgFile_GetSetFlagType(Value, 3, SysopFlags);
      38 : CfgFile_GetSetFlagType(Value, 4, SysopFlags);
      39 : CfgFile_GetSetFlagType(Value, 1, SysopNotFlags);
      40 : CfgFile_GetSetFlagType(Value, 2, SysopNotFlags);
      41 : CfgFile_GetSetFlagType(Value, 3, SysopNotFlags);
      42 : CfgFile_GetSetFlagType(Value, 4, SysopNotFlags);
      43 : CfgFile_GetSetString(Value, 60, OriginLine);
      44 : CfgFile_GetSetByte(Value, AkaAddress);
      45 : CfgFile_GetSetByte(Value, Age);
      46 : CfgFile_GetSetString(Value, 60, Jambase);
      47 : CfgFile_GetSetSmallWord(Value, Group);
      48 : CfgFile_GetSetSmallWord(Value, AltGroup[1]);
      49 : CfgFile_GetSetSmallWord(Value, AltGroup[2]);
      50 : CfgFile_GetSetSmallWord(Value, AltGroup[3]);
      51 : CfgFile_GetSetBit(Value, 0, Attribute2); { Bit 0 : Include in all groups }
      52 : CfgFile_GetSetSmallWord(Value, NetmailArea);
    end; { case }

  Move(MessageRA, Buffer, SizeOf(MessageRecord));
end; { proc. CfgFile_MapMessagesRa }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_MapFilesRa(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
var FilesRa: FilesRecord;
begin
  Move(Buffer, FilesRa, SizeOf(FilesRecord));

  With FilesRa do
    Case FieldName of
      01 : CfgFile_GetSetSmallWord(Value, AreaNum);
      02 : CfgFile_GetSetString(Value, 40, Name);
      03 : CfgFile_GetSetBit(Value, 0, Attrib); { Bit 0 : Include in new files scan }
      04 : CfgFile_GetSetBit(Value, 1, Attrib);     { 1 : Include in upload dupe scan }
      05 : CfgFile_GetSetBit(Value, 2, Attrib);     { 2 : Permit long descriptions }
      06 : CfgFile_GetSetBit(Value, 3, Attrib);     { 3 : Area is on CD-ROM }
      07 : CfgFile_GetSetBit(Value, 4, Attrib);     { 4 : All files are FREE }
      08 : CfgFile_GetSetBit(Value, 5, Attrib);     { 5 : Allow DLs not in FDB }
      09 : CfgFile_GetSetBit(Value, 6, Attrib);     { 6 : Allow users to password uploads }
      10 : CfgFile_GetSetBit(Value, 7, Attrib);     { 7 : Scan uploads }
      11 : CfgFile_GetSetString(Value, 40, FilePath);
      12 : CfgFile_GetSetSmallWord(Value, KillDaysDL);
      13 : CfgFile_GetSetSmallWord(Value, KillDaysFD);
      14 : CfgFile_GetSetString(Value, 15, PassWord);
      15 : CfgFile_GetSetSmallWord(Value, MoveArea);
      16 : CfgFile_GetSetByte(Value, Age);
      17 : CfgFile_GetSetByte(Value, ConvertExt);
      18 : CfgFile_GetSetSmallWord(Value, Group);
      19 : CfgFile_GetSetBit(Value, 0, Attrib2);  { Bit 0 : Include in all groups }
      20 : CfgFile_GetSetSmallWord(Value, DefCost);
      21 : CfgFile_GetSetSmallWord(Value, UploadArea);
      22 : CfgFile_GetSetSmallWord(Value, UploadSecurity);
      23 : CfgFile_GetSetFlagType(Value, 1, UploadFlags);
      24 : CfgFile_GetSetFlagType(Value, 2, UploadFlags);
      25 : CfgFile_GetSetFlagType(Value, 3, UploadFlags);
      26 : CfgFile_GetSetFlagType(Value, 4, UploadFlags);
      27 : CfgFile_GetSetFlagType(Value, 1, UploadNotFlags);
      28 : CfgFile_GetSetFlagType(Value, 2, UploadNotFlags);
      29 : CfgFile_GetSetFlagType(Value, 3, UploadNotFlags);
      30 : CfgFile_GetSetFlagType(Value, 4, UploadNotFlags);
      31 : CfgFile_GetSetSmallWord(Value, Security);
      32 : CfgFile_GetSetFlagType(Value, 1, Flags);
      33 : CfgFile_GetSetFlagType(Value, 2, Flags);
      34 : CfgFile_GetSetFlagType(Value, 3, Flags);
      35 : CfgFile_GetSetFlagType(Value, 4, Flags);
      36 : CfgFile_GetSetFlagType(Value, 1, NotFlags);
      37 : CfgFile_GetSetFlagType(Value, 2, NotFlags);
      38 : CfgFile_GetSetFlagType(Value, 3, NotFlags);
      39 : CfgFile_GetSetFlagType(Value, 4, NotFlags);
      40 : CfgFile_GetSetSmallWord(Value, ListSecurity);
      41 : CfgFile_GetSetFlagType(Value, 1, ListFlags);
      42 : CfgFile_GetSetFlagType(Value, 2, ListFlags);
      43 : CfgFile_GetSetFlagType(Value, 3, ListFlags);
      44 : CfgFile_GetSetFlagType(Value, 4, ListFlags);
      45 : CfgFile_GetSetFlagType(Value, 1, ListNotFlags);
      46 : CfgFile_GetSetFlagType(Value, 2, ListNotFlags);
      47 : CfgFile_GetSetFlagType(Value, 3, ListNotFlags);
      48 : CfgFile_GetSetFlagType(Value, 4, ListNotFlags);
      49 : CfgFile_GetSetSmallWord(Value, AltGroup[1]);
      50 : CfgFile_GetSetSmallWord(Value, AltGroup[2]);
      51 : CfgFile_GetSetSmallWord(Value, AltGroup[3]);
      52 : CfgFile_GetSetByte(Value, Device);
    end; { case }

  Move(FilesRA, Buffer, SizeOf(FilesRecord));
end; { proc. CfgFile_MapFilesRa }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_MapModemRa(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
var ModemRa: ModemRecord;
begin
  Move(Buffer, ModemRa, SizeOf(ModemRecord));

  With ModemRa do
    Case FieldName of
      01 : CfgFile_GetSetByte(Value, ComPort);
      02 : CfgFile_GetSetByte(Value, InitTries);
      03 : CfgFile_GetSetSmallWord(Value, BufferSize);
      04 : CfgFile_GetSetSmallWord(Value, ModemDelay);
      05 : CfgFile_GetSetLongint(Value, MaxSpeed);
      06 : CfgFile_GetSetBoolean(Value, SendBreak);
      07 : CfgFile_GetSetBoolean(Value, LockModem);
      08 : CfgFile_GetSetBoolean(Value, AnswerPhone);
      09 : CfgFile_GetSetBoolean(Value, OffHook);
      10 : CfgFile_GetSetString(Value, 70, InitStr);
      11 : CfgFile_GetSetString(Value, 70, InitStr2);
      12 : CfgFile_GetSetString(Value, 70, BusyStr);
      13 : CfgFile_GetSetString(Value, 40, InitResp);
      14 : CfgFile_GetSetString(Value, 40, BusyResp);
      15 : CfgFile_GetSetString(Value, 40, Connect300);
      16 : CfgFile_GetSetString(Value, 40, Connect1200);
      17 : CfgFile_GetSetString(Value, 40, Connect2400);
      18 : CfgFile_GetSetString(Value, 40, Connect4800);
      19 : CfgFile_GetSetString(Value, 40, Connect7200);
      20 : CfgFile_GetSetString(Value, 40, Connect9600);
      21 : CfgFile_GetSetString(Value, 40, Connect12k);
      22 : CfgFile_GetSetString(Value, 40, Connect14k);
      23 : CfgFile_GetSetString(Value, 40, Connect16k);
      24 : CfgFile_GetSetString(Value, 40, Connect19k);
      25 : CfgFile_GetSetString(Value, 40, Connect38k);
      26 : CfgFile_GetSetString(Value, 40, ConnectFax);
      27 : CfgFile_GetSetString(Value, 20, RingStr);
      28 : CfgFile_GetSetString(Value, 20, AnswerStr);
      29 : CfgFile_GetSetString(Value, 15, ErrorFreeString);
      30 : CfgFile_GetSetString(Value, 40, Connect21k);
      31 : CfgFile_GetSetString(Value, 40, Connect24k);
      32 : CfgFile_GetSetString(Value, 40, Connect26k);
      33 : CfgFile_GetSetString(Value, 40, Connect28k);
      34 : CfgFile_GetSetString(Value, 40, Connect57k);
      35 : CfgFile_GetSetString(Value, 40, Connect64k);
      36 : CfgFile_GetSetString(Value, 40, Connect31k);
      37 : CfgFile_GetSetString(Value, 40, Connect33k);
      38 : CfgFile_GetSetString(Value, 40, Connect115k);
    end; { case }

  Move(ModemRA, Buffer, SizeOf(ModemRecord));
end; { proc. CfgFile_MapModemRa }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_MapConfigRa(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
var ConfigRa: ConfigRecord;
begin
  Move(Buffer, ConfigRa, SizeOf(ConfigRecord));

  With ConfigRa do
    Case FieldName of
      00 : CfgFile_GetSetSmallWord(Value, VersionID);
      01 : CfgFile_GetSetByte(Value, ProductID);
      02 : CfgFile_GetSetSmallWord(Value, MinimumBaud);
      03 : CfgFile_GetSetSmallWord(Value, GraphicsBaud);
      04 : CfgFile_GetSetSmallWord(Value, TransferBaud);
      05 : CfgFile_GetSetString(Value, 5, SlowBaudTimeStart);
      06 : CfgFile_GetSetString(Value, 5, SlowBaudTimeEnd);
      07 : CfgFile_GetSetString(Value, 5, DownloadTimeStart);
      08 : CfgFile_GetSetString(Value, 5, DownloadTimeEnd);
      09 : CfgFile_GetSetString(Value, 5, PageStart[0]);
      10 : CfgFile_GetSetString(Value, 5, PageStart[1]);
      11 : CfgFile_GetSetString(Value, 5, PageStart[2]);
      12 : CfgFile_GetSetString(Value, 5, PageStart[3]);
      13 : CfgFile_GetSetString(Value, 5, PageStart[4]);
      14 : CfgFile_GetSetString(Value, 5, PageStart[5]);
      15 : CfgFile_GetSetString(Value, 5, PageStart[6]);
      16 : CfgFile_GetSetString(Value, 5, PageEnd[0]);
      17 : CfgFile_GetSetString(Value, 5, PageEnd[1]);
      18 : CfgFile_GetSetString(Value, 5, PageEnd[2]);
      19 : CfgFile_GetSetString(Value, 5, PageEnd[3]);
      20 : CfgFile_GetSetString(Value, 5, PageEnd[4]);
      21 : CfgFile_GetSetString(Value, 5, PageEnd[5]);
      22 : CfgFile_GetSetString(Value, 5, PageEnd[6]);
      23 : CfgFile_GetSetString(Value, 22, SeriNum);
      24 : CfgFile_GetSetString(Value, 22, CustNum);
      25 : CfgFile_GetSetSmallWord(Value, PwdExpiry);
      26 : CfgFile_GetSetString(Value, 60, MenuPath);
      27 : CfgFile_GetSetString(Value, 60, TextPath);
      28 : CfgFile_GetSetString(Value, 60, AttachPath);
      29 : CfgFile_GetSetString(Value, 60, NodelistPath);
      30 : CfgFile_GetSetString(Value, 60, MsgBasePath);
      31 : CfgFile_GetSetString(Value, 60, SysPath);
      32 : CfgFile_GetSetString(Value, 60, ExternalEdCmd);
      33 : CfgFile_GetSetNetAddress(Value, Address[0]);
      34 : CfgFile_GetSetNetAddress(Value, Address[1]);
      35 : CfgFile_GetSetNetAddress(Value, Address[2]);
      36 : CfgFile_GetSetNetAddress(Value, Address[3]);
      37 : CfgFile_GetSetNetAddress(Value, Address[4]);
      38 : CfgFile_GetSetNetAddress(Value, Address[5]);
      39 : CfgFile_GetSetNetAddress(Value, Address[6]);
      40 : CfgFile_GetSetNetAddress(Value, Address[7]);
      41 : CfgFile_GetSetNetAddress(Value, Address[8]);
      42 : CfgFile_GetSetNetAddress(Value, Address[9]);
      43 : CfgFile_GetSetString(Value, 30, SystemName);
      44 : CfgFile_GetSetSmallWord(Value, NewSecurity);
      45 : CfgFile_GetSetSmallWord(Value, NewCredit);
      46 : CfgFile_GetSetFlagType(Value, 1, NewFlags);
      47 : CfgFile_GetSetFlagType(Value, 2, NewFlags);
      48 : CfgFile_GetSetFlagType(Value, 3, NewFlags);
      49 : CfgFile_GetSetFlagType(Value, 4, NewFlags);
      50 : CfgFile_GetSetString(Value, 60, OriginLine);
      51 : CfgFile_GetSetString(Value, 15, QuoteString);
      52 : CfgFile_GetSetString(Value, 35, Sysop);
      53 : CfgFile_GetSetString(Value, 60, LogFileName);
      54 : CfgFile_GetSetBoolean(Value, FastLogon);
      55 : CfgFile_GetSetBoolean(Value, AllowSysRem);
      56 : CfgFile_GetSetBoolean(Value, MonoMode);
      57 : CfgFile_GetSetBoolean(Value, StrictPwdChecking);
      58 : CfgFile_GetSetBoolean(Value, DirectWrite);
      59 : CfgFile_GetSetBoolean(Value, SnowCheck);
      60 : CfgFile_GetSetSmallint(Value, CreditFactor);
      61 : CfgFile_GetSetSmallWord(Value, UserTimeOut);
      62 : CfgFile_GetSetSmallWord(Value, LogonTime);
      63 : CfgFile_GetSetSmallWord(Value, PassWordTries);
      64 : CfgFile_GetSetSmallWord(Value, MaxPage);
      65 : CfgFile_GetSetSmallWord(Value, PageLength);
      66 : CfgFile_GetSetBoolean(Value, CheckForMultiLogon);
      67 : CfgFile_GetSetBoolean(Value, ExcludeSysopFromList);
      68 : CfgFile_GetSetBoolean(Value, OneWordNames);
      69 : CfgFile_GetSetAskType(Value, CheckMail);
      70 : CfgFile_GetSetBoolean(Value, AskVoicePhone);
      71 : CfgFile_GetSetBoolean(Value, AskDataPhone);
      72 : CfgFile_GetSetBoolean(Value, DoFullMailCheck);
      73 : CfgFile_GetSetBoolean(Value, AllowFileShells);
      74 : CfgFile_GetSetBoolean(Value, FixUploadDates);
      75 : CfgFile_GetSetBoolean(Value, FreezeChat);
      76 : CfgFile_GetSetAskType(Value, ANSI);
      77 : CfgFile_GetSetAskType(Value, ClearScreen);
      78 : CfgFile_GetSetAskType(Value, MorePrompt);
      79 : CfgFile_GetSetBoolean(Value, UploadMsgs);
      80 : CfgFile_GetSetAskType(Value, KillSent);
      81 : CfgFile_GetSetSmallWord(Value, CrashAskSec);
      82 : CfgFile_GetSetFlagType(Value, 1, CrashAskFlags);
      83 : CfgFile_GetSetFlagType(Value, 2, CrashAskFlags);
      84 : CfgFile_GetSetFlagType(Value, 3, CrashAskFlags);
      85 : CfgFile_GetSetFlagType(Value, 4, CrashAskFlags);
      86 : CfgFile_GetSetSmallWord(Value, CrashSec);
      87 : CfgFile_GetSetFlagType(Value, 1, CrashFlags);
      88 : CfgFile_GetSetFlagType(Value, 2, CrashFlags);
      89 : CfgFile_GetSetFlagType(Value, 3, CrashFlags);
      90 : CfgFile_GetSetFlagType(Value, 4, CrashFlags);
      91 : CfgFile_GetSetSmallWord(Value, FAttachSec);
      92 : CfgFile_GetSetFlagType(Value, 1, FAttachFlags);
      93 : CfgFile_GetSetFlagType(Value, 2, FAttachFlags);
      94 : CfgFile_GetSetFlagType(Value, 3, FAttachFlags);
      95 : CfgFile_GetSetFlagType(Value, 4, FAttachFlags);
      96 : CfgFile_GetSetByte(Value, NormFore);
      97 : CfgFile_GetSetByte(Value, NormBack);
      98 : CfgFile_GetSetByte(Value, StatFore);
      99 : CfgFile_GetSetByte(Value, StatBack);
     100 : CfgFile_GetSetByte(Value, HiBack);
     101 : CfgFile_GetSetByte(Value, HiFore);
     102 : CfgFile_GetSetByte(Value, WindFore);
     103 : CfgFile_GetSetByte(Value, WindBack);
     104 : CfgFile_GetSetByte(Value, ExitLocal);
     105 : CfgFile_GetSetByte(Value, Exit300);
     106 : CfgFile_GetSetByte(Value, Exit1200);
     107 : CfgFile_GetSetByte(Value, Exit2400);
     108 : CfgFile_GetSetByte(Value, Exit4800);
     109 : CfgFile_GetSetByte(Value, Exit9600);
     110 : CfgFile_GetSetByte(Value, Exit19k);
     111 : CfgFile_GetSetByte(Value, Exit38k);
     112 : CfgFile_GetSetBoolean(Value, MultiLine);
     113 : CfgFile_GetSetByte(Value, MinPwdLen);
     114 : CfgFile_GetSetSmallWord(Value, MinUpSpace);
     115 : CfgFile_GetSetAskType(Value, HotKeys);
     116 : CfgFile_GetSetByte(Value, BorderFore);
     117 : CfgFile_GetSetByte(Value, BorderBack);
     118 : CfgFile_GetSetByte(Value, BarFore);
     119 : CfgFile_GetSetByte(Value, BarBack);
     120 : CfgFile_GetSetByte(Value, LogStyle);
     121 : CfgFile_GetSetByte(Value, MultiTasker);
     122 : CfgFile_GetSetByte(Value, PwdBoard);
     123 : CfgFile_GetSetString(Value, 60, FKeys[1]);
     124 : CfgFile_GetSetString(Value, 60, FKeys[2]);
     125 : CfgFile_GetSetString(Value, 60, FKeys[3]);
     126 : CfgFile_GetSetString(Value, 60, FKeys[4]);
     127 : CfgFile_GetSetString(Value, 60, FKeys[5]);
     128 : CfgFile_GetSetString(Value, 60, FKeys[6]);
     129 : CfgFile_GetSetString(Value, 60, FKeys[7]);
     130 : CfgFile_GetSetString(Value, 60, FKeys[8]);
     131 : CfgFile_GetSetString(Value, 60, FKeys[9]);
     132 : CfgFile_GetSetString(Value, 60, FKeys[10]);
     133 : CfgFile_GetSetBoolean(Value, WhyPage);
     134 : CfgFile_GetSetByte(Value, LeaveMsg);
     135 : CfgFile_GetSetBoolean(Value, ShowMissingFiles);
     136 : CfgFile_GetSetBoolean(Value, AllowNetmailReplies);
     137 : CfgFile_GetSetString(Value, 40, LogonPrompt);
     138 : CfgFile_GetSetAskType(Value, CheckNewFiles);
     139 : CfgFile_GetSetString(Value, 60, ReplyHeader);
     140 : CfgFile_GetSetByte(Value, BlankSecs);
     141 : CfgFile_GetSetProtAttrib(Value, ProtocolAttrib[1]);
     142 : CfgFile_GetSetProtAttrib(Value, ProtocolAttrib[2]);
     143 : CfgFile_GetSetProtAttrib(Value, ProtocolAttrib[3]);
     144 : CfgFile_GetSetProtAttrib(Value, ProtocolAttrib[4]);
     145 : CfgFile_GetSetProtAttrib(Value, ProtocolAttrib[5]);
     146 : CfgFile_GetSetProtAttrib(Value, ProtocolAttrib[6]);
     147 : CfgFile_GetSetSmallWord(Value, RenumThreshold);
     148 : CfgFile_GetSetChar(Value, LeftBracket);
     149 : CfgFile_GetSetChar(Value, RightBracket);
     150 : CfgFile_GetSetBoolean(Value, AskForHandle);
     151 : CfgFile_GetSetBoolean(Value, AskForBirthDate);
     152 : CfgFile_GetSetSmallWord(Value, GroupMailSec);
     153 : CfgFile_GetSetBoolean(Value, ConfirmMsgDeletes);
     154 : CfgFile_GetSetString(Value, 60, TempScanDir);
     155 : CfgFile_GetSetAskType(Value, ScanNow);
     156 : CfgFile_GetSetByte(Value, FailedScanAction);
     157 : CfgFile_GetSetSmallWord(Value, FailedScanArea);
     158 : CfgFile_GetSetString(Value, 60, ScanCmd);
     159 : CfgFile_GetSetByte(Value, NewUserGroup);
     160 : CfgFile_GetSetAskType(Value, AVATAR);
     161 : CfgFile_GetSetByte(Value, BadPwdArea);
     162 : CfgFile_GetSetString(Value, 40, Location);
     163 : CfgFile_GetSetByte(Value, DoAfterAction);
     164 : CfgFile_GetSetByte(Value, CRfore);
     165 : CfgFile_GetSetByte(Value, CRback);
     166 : CfgFile_GetSetString(Value, 40, LangHdr);
     167 : CfgFile_GetSetString(Value, 60, ListPath);
     168 : CfgFile_GetSetAskType(Value, FullMsgView);
     169 : CfgFile_GetSetAskType(Value, EMSI_Enable);
     170 : CfgFile_GetSetBoolean(Value, EMSI_NewUser);
     171 : CfgFile_GetSetString(Value, 1, EchoChar);
     172 : CfgFile_GetSetByte(Value, Exit7200);
     173 : CfgFile_GetSetByte(Value, Exit12000);
     174 : CfgFile_GetSetByte(Value, Exit14400);
     175 : CfgFile_GetSetString(Value, 60, ChatCommand);
     176 : CfgFile_GetSetAskType(Value, ExtEd);
     177 : CfgFile_GetSetByte(Value, NewuserLanguage);
     178 : CfgFile_GetSetString(Value, 40, LanguagePrompt);
     179 : CfgFile_GetSetByte(Value, VideoMode);
     180 : CfgFile_GetSetBoolean(Value, AutoDetectANSI);
     181 : CfgFile_GetSetByte(Value, NewUserDateFormat);
     182 : CfgFile_GetSetString(Value, 15, KeyboardPwd);
     183 : CfgFile_GetSetBoolean(Value, CapLocation);
     184 : CfgFile_GetSetByte(Value, NewuserSub);
     185 : CfgFile_GetSetString(Value, 4, PrinterName);
     186 : CfgFile_GetSetByte(Value, HilitePromptFore);
     187 : CfgFile_GetSetByte(Value, HiLitePromptBack);
     188 : CfgFile_GetSetBoolean(Value, AltJSwap);
     189 : CfgFile_GetSetString(Value, 60, SemPath);
     190 : CfgFile_GetSetBoolean(Value, AutoChatCapture);
     191 : CfgFile_GetSetString(Value, 60, FileBasePath);
     192 : CfgFile_GetSetBoolean(Value, NewFileTag);
     193 : CfgFile_GetSetBoolean(Value, IgnoreDupeExt);
     194 : CfgFile_GetSetString(Value, 60, TempCDFilePath);
     195 : CfgFile_GetSetByte(Value, TagFore);
     196 : CfgFile_GetSetByte(Value, TagBack);
     197 : CfgFile_GetSetByte(Value, Exit16k);
     198 : CfgFile_GetSetByte(Value, FilePayback);
     199 : CfgFile_GetSetString(Value, 200, FileLine);
     200 : CfgFile_GetSetString(Value, 200, FileMissingLine);
     201 : CfgFile_GetSetByte(Value, NewUserULCredit);
     202 : CfgFile_GetSetSmallWord(Value, NewUserULCreditK);
     203 : CfgFile_GetSetArcRecord(Value, ArcInfo[1]);
     204 : CfgFile_GetSetArcRecord(Value, ArcInfo[2]);
     205 : CfgFile_GetSetArcRecord(Value, ArcInfo[3]);
     206 : CfgFile_GetSetArcRecord(Value, ArcInfo[4]);
     207 : CfgFile_GetSetArcRecord(Value, ArcInfo[5]);
     208 : CfgFile_GetSetArcRecord(Value, ArcInfo[6]);
     209 : CfgFile_GetSetArcRecord(Value, ArcInfo[7]);
     210 : CfgFile_GetSetArcRecord(Value, ArcInfo[8]);
     211 : CfgFile_GetSetArcRecord(Value, ArcInfo[9]);
     212 : CfgFile_GetSetArcRecord(Value, ArcInfo[10]);
     213 : CfgFile_GetSetString(Value, 60, RAMGRAltFKeys[1]);
     214 : CfgFile_GetSetString(Value, 60, RAMGRAltFKeys[2]);
     215 : CfgFile_GetSetString(Value, 60, RAMGRAltFKeys[3]);
     216 : CfgFile_GetSetString(Value, 60, RAMGRAltFKeys[4]);
     217 : CfgFile_GetSetString(Value, 60, RAMGRAltFKeys[5]);
     218 : CfgFile_GetSetString(Value, 60, ArcViewCmd);
     219 : CfgFile_GetSetByte(Value, ExitFax);
     220 : CfgFile_GetSetBoolean(Value, UseXMS);
     221 : CfgFile_GetSetBoolean(Value, UseEMS);
     222 : CfgFile_GetSetByte(Value, CheckDOB);
     223 : CfgFile_GetSetAskType(Value, EchoCheck);
     224 : CfgFile_GetSetSmallWord(Value, ccSec);
     225 : CfgFile_GetSetSmallWord(Value, ReturnRecSec);
     226 : CfgFile_GetSetBoolean(Value, HonourNetReq);
     227 : CfgFile_GetSetCombined(Value, DefaultCombined);
     228 : CfgFile_GetSetBoolean(Value, AskForSex);
     229 : CfgFile_GetSetBoolean(Value, AskForAddress);
     230 : CfgFile_GetSetAskType(Value, DLdesc);
     231 : CfgFile_GetSetBoolean(Value, NewPhoneScan);
     232 : CfgFile_GetSetByte(Value, Exit21k);
     233 : CfgFile_GetSetByte(Value, Exit24k);
     234 : CfgFile_GetSetByte(Value, Exit26k);
     235 : CfgFile_GetSetByte(Value, Exit28k);
     236 : CfgFile_GetSetByte(Value, Exit57k);
     237 : CfgFile_GetSetByte(Value, Exit64k);
     238 : CfgFile_GetSetBoolean(Value, TagLogoffWarning);
     239 : CfgFile_GetSetBoolean(Value, LimitLocal);
     240 : CfgFile_GetSetBoolean(Value, SavePassWords);
     241 : CfgFile_GetSetByte(Value, BlankLogins);
     242 : CfgFile_GetSetString(Value, 60, ripiconpath);
     243 : CfgFile_GetSetByte(Value, Exit31k);
     244 : CfgFile_GetSetByte(Value, Exit33k);
     245 : CfgFile_GetSetBoolean(Value, IncludeNewCDareas);
     246 : CfgFile_GetSetByte(Value, Exit115k);
    end; { case }

  Move(ConfigRa, Buffer, SizeOf(ConfigRecord));
end; { proc. CfgFile_MapConfigRa }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_MapGroupsRa(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
var GroupsRa: GroupRecord;
begin
  Move(Buffer, GroupsRa, SizeOf(GroupRecord));

  With GroupsRa do
    Case FieldName of
       01 : CfgFile_GetSetSmallWord(Value, AreaNum);
       02 : CfgFile_GetSetString(Value, 40, Name);
       03 : CfgFile_GetSetSmallWord(Value, Security);
       04 : CfgFile_GetSetFlagType(Value, 1, Flags);
       05 : CfgFile_GetSetFlagType(Value, 2, Flags);
       06 : CfgFile_GetSetFlagType(Value, 3, Flags);
       07 : CfgFile_GetSetFlagType(Value, 4, Flags);
       08 : CfgFile_GetSetFlagType(Value, 1, NotFlagsMask);
       09 : CfgFile_GetSetFlagType(Value, 2, NotFlagsMask);
       10 : CfgFile_GetSetFlagType(Value, 3, NotFlagsMask);
       11 : CfgFile_GetSetFlagType(Value, 4, NotFlagsMask);
    end; { case }

  Move(GroupsRa, Buffer, SizeOf(GroupRecord));
end; { proc. CfgFile_MapGroupsRa }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_MapMenuFile(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
var MenuFile: mnuRecord;
begin
  Move(Buffer, MenuFile, SizeOf(mnuRecord));

  With MenuFile do
    Case FieldName of
      01 : CfgFile_GetSetByte(Value, Typ);
      02 : CfgFile_GetSetSmallWord(Value, Security);
      03 : CfgFile_GetSetSmallWord(Value, MaxSec);
      04 : CfgFile_GetSetFlagType(Value, 1, NotFlagsMask);
      05 : CfgFile_GetSetFlagType(Value, 2, NotFlagsMask);
      06 : CfgFile_GetSetFlagType(Value, 3, NotFlagsMask);
      07 : CfgFile_GetSetFlagType(Value, 4, NotFlagsMask);
      08 : CfgFile_GetSetFlagType(Value, 1, Flags);
      09 : CfgFile_GetSetFlagType(Value, 2, Flags);
      10 : CfgFile_GetSetFlagType(Value, 3, Flags);
      11 : CfgFile_GetSetFlagType(Value, 4, Flags);
      12 : CfgFile_GetSetSmallWord(Value, TimeLeft);
      13 : CfgFile_GetSetSmallWord(Value, TimeUsed);
      14 : CfgFile_GetSetByte(Value, Age);
      15 : CfgFile_GetSetByte(Value, TermAttrib);
      16 : CfgFile_GetSetLongint(Value, MinSpeed);
      17 : CfgFile_GetSetLongint(Value, MaxSpeed);
      18 : CfgFile_GetSetLongint(Value, Credit);
      19 : CfgFile_GetSetLongint(Value, OptionCost);
      20 : CfgFile_GetSetLongint(Value, PerMinCost);
      21 : CfgFile_GetSetLongint(Value, PerMinCost);
      22 : ; { NodeArray ?? }
      23 : ; { Group }
      24 : CfgFile_GetSetMenutime(Value, StartTime[1]);
      25 : CfgFile_GetSetMenutime(Value, StartTime[2]);
      26 : CfgFile_GetSetMenutime(Value, StartTime[3]);
      27 : CfgFile_GetSetMenutime(Value, StartTime[4]);
      28 : CfgFile_GetSetMenutime(Value, StartTime[5]);
      29 : CfgFile_GetSetMenutime(Value, StartTime[6]);
      30 : CfgFile_GetSetMenutime(Value, StartTime[7]);
      31 : CfgFile_GetSetMenutime(Value, StopTime[1]);
      32 : CfgFile_GetSetMenutime(Value, StopTime[2]);
      33 : CfgFile_GetSetMenutime(Value, StopTime[3]);
      34 : CfgFile_GetSetMenutime(Value, StopTime[4]);
      35 : CfgFile_GetSetMenutime(Value, StopTime[5]);
      36 : CfgFile_GetSetMenutime(Value, StopTime[6]);
      37 : CfgFile_GetSetMenutime(Value, StopTime[7]);
      38 : CfgFile_GetSetString(Value, 135, Display);
      39 : CfgFile_GetSetString(Value, 8, HotKey);
      40 : CfgFile_GetSetString(Value, 135, MiscData);
      41 : CfgFile_GetSetByte(Value, ForeGround);
      42 : CfgFile_GetSetByte(Value, BackGround);
    end; { case }

  Move(MenuFile, Buffer, SizeOf(mnuRecord));
end; { proc. CfgFile_MapMenuFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_MapLightbarFile(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
var LightBar: LightbarRecord;
begin
  Move(Buffer, Lightbar, SizeOf(LightbarRecord));

  With Lightbar do
    Case FieldName of
      01 : CfgFile_GetSetByte(Value, LightX);
      02 : CfgFile_GetSetByte(Value, LightY);
      03 : CfgFile_GetSetString(Value, 135, LowItem);
      04 : CfgFile_GetSetString(Value, 135, SelectItem);
      05 : CfgFile_GetSetBit(Value, 0, Attrib); { Bit 0: Enabled }
    end; { case }

  Move(Lightbar, Buffer, SizeOf(LightbarRecord));
end; { proc. CfgFile_MapLightBarFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_MapNewsServerFile(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
var NewsServer: NewsServerRecord;
    TmpByte   : Byte;
begin
  Move(Buffer, NewsServer, SizeOf(NewsServerRecord));

  With NewsServer do
    Case FieldName of
      01 : CfgFile_GetSetLongint(Value, MaxSessions);
      02 : CfgFile_GetSetLongint(Value, Serverport);
      03 : begin
             TmpByte := Attribute;
             CfgFile_GetSetBit(Value, 0, TmpByte);
             Attribute := TmpByte;
           end; { }
    end; { case }

  Move(NewsServer, Buffer, SizeOf(NewsServerRecord));
end; { proc. CfgFile_MapNewsServerFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_MapNewsGroupStats(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
var NewsStat: NewsGroupStat;
begin
  Move(Buffer, NewsStat, SizeOf(NewsGroupStat));

  With NewsStat do
    Case FieldName of
      01 : CfgFile_GetSetString(Value, 100, GroupName);
      02 : CfgFile_GetSetLongint(Value, NumRead);
      03 : CfgFile_GetSetLongint(Value, NumWrite);
      04 : CfgFile_GetSetLongint(Value, NumDeny);
      05 : CfgFile_GetSetString(Value, 10, LastAccessed);
    end; { case }

  Move(NewsStat, Buffer, SizeOf(NewsGroupStat));
end; { proc. CfgFile_MapNewsGroupStats }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_MapConfigEle(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
var ConfigEle: EleConfigRecord;
begin
  Move(Buffer, ConfigEle, SizeOf(EleConfigRecord));

  With ConfigEle do
    Case FieldName of
      01 : CfgFile_GetSetSmallWord(Value, VersionID);
      02 : CfgFile_GetSetString(Value, 250, UtilityLogFileName);
      03 : CfgFile_GetSetBoolean(Value, CapitalizeUserName);
      04 : CfgFile_GetSetString(Value, 15, AttachPassword);
    end; { case }

  Move(ConfigEle, Buffer, SizeOf(EleConfigRecord));
end; { proc. CfgFile_MapConfigEle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_MapMessagesEle(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
var MessagesEle: EleMessageRecord;
begin
  Move(Buffer, MessagesEle, SizeOf(EleMessageRecord));

  With MessagesEle do
    Case FieldName of
      01 : CfgFile_GetSetLongint(Value, AreaNum);
      02 : CfgFile_GetSetString(Value, 128, GroupName);
      03 : CfgFile_GetSetBit(Value, 0, Attribute); { Bit 0 : Available for usenet? }
      04 : CfgFile_GetSetAccessType(Value, AccessSettings);
      05 : CfgFile_GetSetLongint(Value, AttachArea);
    end; { case }

  Move(MessagesEle, Buffer, SizeOf(EleMessageRecord));
end; { proc. CfgFile_MapMessagesEle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_MapFilesEle(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
var FilesEle: EleFilesRecord;
begin
  Move(Buffer, FilesEle, SizeOf(EleFilesRecord));

  With FilesEle do
    Case FieldName of
      01 : CfgFile_GetSetLongint(Value, AreaNum);
      02 : CfgFile_GetSetString(Value, 250, ExportURL);
      03 : CfgFile_GetSetString(Value, 250, ftpPath);
      04 : CfgFile_GetSetString(Value, 35, ftpLoginName);
      05 : CfgFile_GetSetString(Value, 35, ftpPassword);
      06 : CfgFile_GetSetBit(Value, 0, Attribute);
    end; { case }

  Move(FilesEle, Buffer, SizeOf(EleFilesRecord));
end; { proc. CfgFile_MapFilesEle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_MapUseronBBS(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
var Useron: UseronRecord;
begin
  Move(Buffer, UserOn, SizeOf(UseronRecord));

  With Useron do
    Case FieldName of
      01 : CfgFile_GetSetString(Value, 35, Name);
      02 : CfgFile_GetSetString(Value, 35, Handle);
      03 : CfgFile_GetSetByte(Value, Line);
      04 : CfgFile_GetSetSmallWord(Value, Baud);
      05 : CfgFile_GetSetString(Value, 25, City);
      06 : CfgFile_GetSetByte(Value, Status);
      07 : CfgFile_GetSetBit(Value, 0, Attribute); { Bit 0 : Hidden }
      08 : CfgFile_GetSetBit(Value, 1, Attribute); {     1 : Wants chat }
      09 : CfgFile_GetSetBit(Value, 2, Attribute); {     2 : Reserved for RANETMGR }
      10 : CfgFile_GetSetBit(Value, 3, Attribute); {     3 : Do not disturb flag }
      11 : CfgFile_GetSetBit(Value, 6, Attribute); {     6 : Ready (0=busy) }
      12 : CfgFile_GetSetString(Value, 10, StatDesc);
      13 : CfgFile_GetSetSmallWord(Value, NoCalls);
      14 : CfgFile_GetSetLongint(Value, NodeNumber);
      15 : CfgFile_GetSetUnixTime(Value, LastUpdate);
    end; { case }

  Move(Useron, Buffer, SizeOf(UseronRecord));
end; { proc. CfgFile_MapUseronBBS }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_MapProtocolRa(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
var Protocol: ProtocolRecord;
begin
  Move(Buffer, Protocol, SizeOf(ProtocolRecord));

  With Protocol do
    Case FieldName of
      01 : CfgFile_GetSetString(Value, 15, Name);
      02 : CfgFile_GetSetChar(Value, ActiveKey);
      03 : CfgFile_GetSetBoolean(Value, OpusTypeCtlFile);
      04 : CfgFile_GetSetBoolean(Value, BatchAvailable);
      05 : CfgFile_GetSetByte(Value, Attribute);
      06 : CfgFile_GetSetString(Value, 80, LogFileName);
      07 : CfgFile_GetSetString(Value, 80, CtlFileName);
      08 : CfgFile_GetSetString(Value, 80, DnCmdString);
      09 : CfgFile_GetSetString(Value, 80, DnCtlString);
      10 : CfgFile_GetSetString(Value, 80, UpCmdString);
      11 : CfgFile_GetSetString(Value, 80, UpCtlString);
      12 : CfgFile_GetSetString(Value, 20, UpLogKeyWord);
      13 : CfgFile_GetSetString(Value, 20, DnLogKeyWord);
      14 : CfgFile_GetSetByte(Value, XferDescWordNum);
      15 : CfgFile_GetSetByte(Value, XferNameWordNum);
    end; { case }

  Move(Protocol, Buffer, SizeOf(ProtocolRecord));
end; { CfgFile_MapProtocolRa }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_MapTagListRa(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
var TagListRa: TagFileRecord;
begin
  Move(Buffer, TagListRa, SizeOf(TagFileRecord));

  With TagListRa do
    Case FieldName of
      01 : CfgFile_GetSetString(Value, 12, Name);
      02 : CfgFile_GetSetString(Value, 15, Password);
      03 : CfgFile_GetSetBit(Value, 0, Attrib); { Bit 0 : Deleted }
      04 : CfgFile_GetSetBit(Value, 1, Attrib); {     1 : Unlisted }
      05 : CfgFile_GetSetBit(Value, 2, Attrib); {     2 : Free (don't adjust ratio) - Does NOT affect "Cost" }
      06 : CfgFile_GetSetBit(Value, 3, Attrib); {     3 : Not available (don't allow downloads) }
      07 : CfgFile_GetSetBit(Value, 4, Attrib); {     4 : Locked (no kill) }
      08 : CfgFile_GetSetBit(Value, 5, Attrib); {     5 : Missing/offline }
      09 : CfgFile_GetSetBit(Value, 6, Attrib); {     6 : No time restrictions - always allow DL }
      10 : CfgFile_GetSetSmallWord(Value, AreaNum);
      11 : CfgFile_GetSetSmallWord(Value, RecordNum);
      12 : CfgFile_GetSetLongint(Value, Size);
      13 : CfgFile_GetSetLongint(Value, Filedate);
      14 : CfgFile_GetSetLongint(Value, Cost);
      15 : CfgFile_GetSetBoolean(Value, CDROM);
      16 : CfgFile_GetSetBoolean(Value, FoundFirst);
      17 : CfgFile_GetSetSmallWord(Value, xFerTime);
    end; { case }

  Move(TagListRa, Buffer, SizeOf(TagFileRecord));
end; { CfgFile_MapTagListRa }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_MapTelnetEle(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
var Telnet : TelnetRecord;
    TmpByte: Byte;
begin
  Move(Buffer, Telnet, SizeOf(TelnetRecord));

  With Telnet do
    Case FieldName of
      01 : CfgFile_GetSetLongint(Value, MaxSessions);
      02 : CfgFile_GetSetLongint(Value, ServerPort);
      03 : CfgFile_GetSetLongint(Value, StartNodeWith);
      04 : begin
             Tmpbyte := Attrib;
             CfgFile_GetSetBit(Value, 0, TmpByte); { Bit 0: Deny telnet download }
             Attrib := TmpByte;
           end;
      05 : begin
             Tmpbyte := Attrib;
             CfgFile_GetSetBit(Value, 1, TmpByte); { Bit 1: Start EleBBS minized for telnet nodes }
             Attrib := TmpByte;
           end;
      06 : CfgFile_GetSetString(Value, 250, ProgramPath);
      07 : CfgFile_GetSetString(Value, 250, NodeDirectories);
    end; { case }

  Move(Telnet, Buffer, SizeOf(TelnetRecord));
end; { CfgFile_MapTelnetEle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CfgFile_MapLimitsRa(var Buffer: qaCfgBufDataType; FieldName: Longint; var Value: String);
var Limit: LimitsRecord;
begin
  Move(Buffer, Limit, SizeOf(LimitsRecord));

  With Limit do
    Case FieldName of
      01 : CfgFile_GetSetSmallWord(Value, Security);
      02 : CfgFile_GetSetSmallWord(Value, Ltime);
      03 : CfgFile_GetSetSmallWord(Value, L300);
      04 : CfgFile_GetSetSmallWord(Value, L1200);
      05 : CfgFile_GetSetSmallWord(Value, L2400);
      06 : CfgFile_GetSetSmallWord(Value, L4800);
      07 : CfgFile_GetSetSmallWord(Value, L7200);
      08 : CfgFile_GetSetSmallWord(Value, L9600);
      09 : CfgFile_GetSetSmallWord(Value, L12000);
      10 : CfgFile_GetSetSmallWord(Value, L14400);
      11 : CfgFile_GetSetSmallWord(Value, L16800);
      12 : CfgFile_GetSetSmallWord(Value, L19200);
      13 : CfgFile_GetSetSmallWord(Value, L38400);
      14 : CfgFile_GetSetSmallWord(Value, Llocal);
      15 : CfgFile_GetSetSmallWord(Value, RatioNum);
      16 : CfgFile_GetSetSmallWord(Value, RatioK);
      17 : CfgFile_GetSetTPReal(Value, PerMinCost);
      18 : CfgFile_GetSetSmallWord(Value, L21600);
      19 : CfgFile_GetSetSmallWord(Value, L24000);
      20 : CfgFile_GetSetSmallWord(Value, L26400);
      21 : CfgFile_GetSetSmallWord(Value, L28800);
      22 : CfgFile_GetSetSmallWord(Value, L57600);
      23 : CfgFile_GetSetSmallWord(Value, L64000);
      24 : CfgFile_GetSetTPReal(Value, FlexiTime);
      25 : CfgFile_GetSetSmallWord(Value, LsessionTime);
      26 : CfgFile_GetSetSmallWord(Value, ResetAmt);
      27 : CfgFile_GetSetResetType(Value, ResetPeriod);
      28 : CfgFile_GetSetSmallWord(Value, ResetOffset);
      29 : CfgFile_GetSetSmallWord(Value, L31200);
      30 : CfgFile_GetSetSmallWord(Value, L33600);
      31 : CfgFile_GetSetSmallWord(Value, L115200);
    end; { case }

  Move(Limit, Buffer, SizeOf(LimitsRecord));
end; { CfgFile_MapLimitsRa }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { QACFG }
