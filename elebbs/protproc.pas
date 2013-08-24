unit ProtProc;
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
** The technical routines for transferring files in EleBBS, this unit is
** not overlaid to make it as fast as possible.
**
**
** Copyright (c) 1997-1999 by Maarten Bekers
**
** Created : 01-Jan-1999
** Last update : 01-Jan-1999
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF WITH_FULL}
  This unit shouldn''t be compiled for an utility program
{$ENDIF}

{$IFDEF ISCGI}
  This unit should not be compiled for an utility program
{$ENDIF}

uses Control,
      BitWise,
     {$IFNDEF WINGUI}
      ScrnU,
      Crt,
     {$ELSE}
       Forms,
       Win_main,
     {$ENDIF}
      Cases,
       ElLog_U,
        JDates,
        ApTimer,
         OoAbsPcl,
          ooZmodem,
           ooYModem,
            ooXmodem,
             Comunit,
              Global,
               CfgRec,
                Tagunit,
                 Multi,
                  GenFile,
                   StrPath,
                    GenDos,
                     ApFossil,
                      debug_u,
                       ObjDec;

Const TotalBytesTransferLeft : Longint = 00;
      TotalTransferBytes     : Longint = 00;

      {$IFDEF MSDOS}
        TransListSize        = 1024 * 10;
      {$ELSE}
        TransListSize        = 1024 * 20;
      {$ENDIF}

var ActualCPS: Longint;
    ProtObj  : AbstractProtocolPtr;
    FLP      : FileListPtr;
    TransScrn: Pointer;

{---------------------------------- Hooks list --------------------------------}
procedure trans_ShowFileList(CurFile: Longint);
procedure trans_BackGrProc(Ap: AbstractProtocolPtr);
procedure trans_WindowStatus(Ap: AbstractProtocolPtr; Start, TransEnd: Boolean);
procedure trans_LogFileProc(Ap: AbstractProtocolPtr; LogFileStatus: LogFileType);
function  trans_MsgTmpAcceptFile(AP: AbstractProtocolPtr): Boolean;
function  trans_EscHook: Boolean;

function  trans_InitializeProtocol(Protocol: Char): Boolean;
function  trans_InitializeHooks(Download, MsgUpping: Boolean): Boolean;
procedure trans_DoneProtocol;
procedure trans_StartTransfer(Download: Boolean);

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses FastScrn, ApMisc, CentrStr, Input_U, Colors,
     StUtils, LongStr, Dos;

function trans_InitializeProtocol(Protocol: Char): Boolean;
begin
  ProtObj := NIL;
  trans_InitializeProtocol := false;


  Case UpCase(Protocol) of
   { ZModem }  'Z' : begin
                       DefProtocolOptions := DefProtocolOptions or apZmodem8K;
                       ProtObj := New(ZModemProtocolPtr, Init (ComPtr));

                       if ProtObj <> NIL then
                        With ZModemProtocolPtr(ProtObj)^ do
                          begin
                            SetFileMgmtOptions(true, false, WriteDifferent);
                            SetRecoverOption(false);
                          end { With }
                            else RaLog('!', 'Not enough memory for ZModem download');
                     end; { ZModem }
      'Y', 'G'     : begin
                       ProtObj := New(YModemProtocolPtr, Init(ComPtr, True, Protocol='G'));

                       if ProtObj <> NIL then
                         YmodemProtocolPtr(ProtObj)^.SetBlockWait(RelaxedBlockWait)
                           else RaLog('!', 'Not enough memory for YModem download');
                     end; { YModem, YModem-G }
     'X', '1', 'Q' : begin
                       ProtObj := New(XModemProtocolPtr, Init(ComPtr, Protocol = '1',
                                                                Protocol = 'Q'));
                       if ProtObj <> nil then
                         With XModemProtocolPtr(ProtObj)^ do
                           begin
                             SetBlockWait(RelaxedBlockWait);
                           end; { With }
                      end; { XModem, XModem/1k, XModem/1K-G }
       else ProtObj := NIL;
  end; { Case }

  if ProtOBJ=NIL then EXIT;

  TotalBytesTransferLeft := 00;
  TotalTransferBytes := 00;

  trans_InitializeProtocol := true;
end; { func. Trans_InitializeProtocol }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure trans_DoneProtocol;
begin
{$IFDEF DELPHI}
  try
{$ENDIF}
    Dispose(ProtObj, Done);
    ProtObj := nil;
{$IFDEF DELPHI}
  except
  end;
{$ENDIF}
end; { proc. trans_DoneProtocol }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure trans_StartTransfer(Download: Boolean);
begin
  {$IFNDEF WINGUI}
    CursorOff;
  {$ENDIF}

  if Download then
    ProtObj^.ProtocolTransmit
     else ProtObj^.ProtocolReceive;

  {$IFNDEF WINGUI}
    CursorOn;
  {$ENDIF}
end; { proc. trans_StartTransfer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function OwnNextFileList(AP : AbstractProtocolPtr;
                         var FName : PathStr) : Boolean;
var Counter: Longint;
    TempStr: String;
    NameStr: String;
    TagInfo: TagFileRecord;
begin
  OwnNextFileList := NextFileList(Ap, FName);
  NameStr := SupCase(Trim(FName));
  NameStr := JustName(NameStr);

  for Counter := 00 to TaggingObj^.CurrentTagged do
    begin
      TaggingObj^.GetTagHeader(Counter, TagInfo);
      TempStr := Trim(GetLfnName(TagInfo.Name, TagInfo.RecordNum, TagInfo.AreaNum));

      if SupCase(TempStr) = SUpCase(NameStr) then
         begin
           trans_ShowFileList(Counter);
           BREAK;
         end; { if }
    end; { for }
end; { func. OwnNextFileList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function trans_InitializeHooks(Download, MsgUpping: Boolean): Boolean;
begin
  trans_InitializeHooks := true;

  {--------- Set the several transfer hooks as slicing and statusupdates ------}
  ProtObj^.SetBackGroundProc({$IFDEF FPC}@{$ENDIF}trans_BackGrProc);
  ProtObj^.SetShowStatusProc({$IFDEF FPC}@{$ENDIF}trans_WindowStatus);
  ProtObj^.SetLogFileProc({$IFDEF FPC}@{$ENDIF}trans_LogFileProc);
  ProtObj^.SetNextFileFunc({$IFDEF FPC}@{$ENDIF}OwnNextFileList);
  ProtObj^.SetAcceptFileFunc({$IFDEF FPC}@{$ENDIF}noAcceptfile);         { (internal hook by APRO) }
  ProtObj^.SetHandshakeWait(Secs2Tics(25), 3);

  ComPtr^.SetAbortFunc({$IFDEF FPC}@{$ENDIF}trans_EscHook);

  if NOT Download then
   if MsgUpping then
     ProtObj^.SetAcceptFileFunc({$IFDEF FPC}@{$ENDIF}trans_MsgTmpAcceptFile);

  if MsgUpping then
   begin
     ProtObj^.SetDestinationDirectory('');
     ProtObj^.SetReceiveFileName('msgtmp.');
   end; { if }
end; { func. trans_InitializeHooks }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure UpdateProgressBar(Col, Row, Attr, Len : Byte; Percent : Real);
const CompleteChar = 'Û';

var CharPercent: Real;
    CharCount  : Longint;
    BarStr     : ShortString;
begin
    Percent := Abs(Percent);

    { Calculate "percent value" of each character space }
    if Len > 00 then
      CharPercent := 100.0 / Len;

    { Calculate how many chars we need to approach (but not exceed) Percent }
    if CharPercent > 00 then
      CharCount := Trunc((Percent * 100) / CharPercent);

    { Make sure we don't go past Len }
    If CharCount > Len then CharCount := Len;
    If CharCount < 0 then CharCount := 0;

    { Write out the complete bar }
    FillChar(BarStr[1], CharCount, CompleteChar);
    BarStr[0] := Char(CharCount);

    If CharCount <> 00 then
      FastWrite(Col, Row, Attr, BarStr);
end; { proc. UpdateProgressBar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure UpdateStatusMsg(Col, Row, Attr : Byte);
const LastStatus : Word = 65535;
      MaxMsgLen  = 40;

var Msg : ShortString;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, '-Status-: "'+StatusStr(AsyncStatus MOD 10000)+'"');
  {$ENDIF}

  if (NOT LineCfg^.ShowStatusScrn) then EXIT;

   if AsyncStatus <> LastStatus then
    begin
      FillChar(Msg[1], MaxMsgLen, ' ');
      Msg[0] := Char(MaxMsgLen);

      FastWrite(Col, Row, Attr, Msg);

      Msg := StatusStr(AsyncStatus mod 10000);
      FastWrite(Col, Row, Attr, Msg);


      InputObj^.UpdateScrn;
    end; { if }
end; { proc. UpdateStatusMsg }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure trans_BackGrProc(Ap: AbstractProtocolPtr);
begin
  DoSlice;
end; { proc. trans_BackGrProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure trans_ShowFileList(CurFile: Longint);
const XLow       = 1;
      YLow       = 17;
      XHigh      = 80;
      YHigh      = 23;

      HeaderAttr : Byte = 00;
      DataAttr   : Byte = 00;

      ScreenMax  : Byte = ( (YHigh - Succ(YLow)) * 2);

var Counter   : Longint;
    RowCounter: Longint;
    MaxPos    : Longint;
    TagInfo   : TagFileRecord;
    TempStr   : String;
    ColorToUse: Longint;
begin
  HeaderAttr := MakeAttr(GlobalCfg^.RaConfig^.BorderFore, GlobalCfg^.RaConfig^.BorderBack);
  DataAttr   := MakeAttr(GlobalCfg^.RaConfig^.HiFore, GlobalCfg^.RaConfig^.BorderBack);

  BoxWindow(XLow, YLow, XHigh, YHigh,
            GlobalCfg^.RaConfig^.BorderFore, GlobalCfg^.RaConfig^.BorderBack,
            'Files');
  TempStr := ' ' + FStr(CurFile + 1) + ' of ' + FStr(TaggingObj^.CurrentTagged) + ' files ';
  FastWrite(XHigh - (Length(TempStr) + 1), YHigh, HeaderAttr, TempStr);

  MaxPos := TaggingObj^.CurrentTagged;
  if (CurFile) >= ScreenMax then
    begin
      Counter := Succ(CurFile - ScreenMax);
    end
      else Counter := 00;

  if (MaxPos - Counter) >= ScreenMax then MaxPos := Pred(Counter + ScreenMax);
  RowCounter := 01;

  While Counter <= MaxPos do
    begin
      {--------------------------- Left column ------------------------------}
      TaggingObj^.GetTagHeader(Counter, TagInfo);
      TempStr := GetLfnName(TagInfo.Name, TagInfo.RecordNum, TagInfo.AreaNum);
      TempStr := MakeLen(TempStr, 35, Space, false, false);

      if Counter > TaggingObj^.CurrentTagged then TempStr := '';

      if Counter = CurFile then ColorToUse := DataAttr
        else ColorToUse := HeaderAttr;
      FastWrite(XLow + 03, YLow + RowCounter, ColorToUse, TempStr);
      Inc(Counter);

      {-------------------------- Right column ------------------------------}
      if Counter < TaggingObj^.CurrentTagged then
        begin
          TaggingObj^.GetTagHeader(Counter, TagInfo);
          TempStr := GetLfnName(TagInfo.Name, TagInfo.RecordNum, TagInfo.AreaNum);
          TempStr := MakeLen(TempStr, 35, Space, false, false);

          if Counter = CurFile then ColorToUse := DataAttr
            else ColorToUse := HeaderAttr;
          FastWrite(XLow + 03 + 38, YLow + RowCounter, ColorToUse, TempStr);
        end; { if }

      Inc(Counter);
      Inc(RowCounter);
    end; { while }

end; { proc. trans_ShowFileList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function EstimateTransferSecs(Size, ActCps: LongInt): LongInt;
begin
  { very rough estimation cause we do not take into acount the efficiency }
  { of this protocol }
  if ActCps > 0 then
    EstimateTransferSecs := Size div ActCps
     else EstimateTransferSecs := 0;
end; { func. EstimateTransferSecs }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure trans_WindowStatus(Ap: AbstractProtocolPtr; Start, TransEnd: Boolean);
const XLow       = 1;
      YLow       = 6;
      XHigh      = 80; { 69 }
      YHigh      = 16;
      NewProgBar = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°';
      HeaderStr  : Array[TransferModeType] of String[19] =
                        ('Downloading', 'Uploading');
      LastBytesTransferred : LongInt = 0;                                 {!!.03}

      HeaderAttr : Byte = 00;
      DataAttr   : Byte = 00;

var R                   : Real;
    CurBlockSize        : Longint;
    CurFileSize         : LongInt;
    CurBytesRemaining   : LongInt;
    CurBytesTransferred : LongInt;
    CurProtocol         : Longint;
    CurElapsedTics      : LongInt;
    TempStr             : String;               { For holding Checksum etc }

function FileNameShow(S: String): String;
begin
  if S='MSGTMP.' then FileNameShow := 'Uploading message text'
    else FileNameShow := SupCase(S);
end; { func. FileNameShow }

begin
  if (NOT LineCfg^.ShowStatusScrn) then EXIT;

  HeaderAttr := MakeAttr(GlobalCfg^.RaConfig^.BorderFore, GlobalCfg^.RaConfig^.BorderBack);
  DataAttr   := MakeAttr(GlobalCfg^.RaConfig^.HiFore, GlobalCfg^.RaConfig^.BorderBack);

  if Start then
   with GlobalCfg^ do
    begin
      SaveScreen(TransScrn);

      BoxWindow(XLow, YLow, XHigh, YHigh, RaConfig^.BorderFore, RaConfig^.BorderBack, HeaderStr[LineCfg^.TransferMode]);

      FastWrite(XLow + 02, YLow + 01, HeaderAttr, 'Protocol:');
      FastWrite(XLow + 02, YLow + 02, HeaderAttr, 'File name:');
      FastWrite(XLow + 02, YLow + 03, HeaderAttr, 'File size:');
      FastWrite(XLow + 02, YLow + 04, HeaderAttr, 'Bytes remaining:');
      FastWrite(XLow + 02, YLow + 05, HeaderAttr, 'CPS:');

      FastWrite(XLow + 38, YLow + 04, HeaderAttr, 'Time remaining:');
      FastWrite(XLow + 38, YLow + 05, HeaderAttr, 'Total errors:');

      FastWrite(XLow + 02, YLow + 07, HeaderAttr, 'Progress:');
      FastWrite(XLow + 02, YLow + 08, HeaderAttr, 'Total:');
      FastWrite(XLow + 02, YLow + 09, HeaderAttr, 'Status:');
      FastWrite(XLow + 00, YLow + 06, HeaderAttr, 'Ã' + Dup(#196, (XHigh - XLow) - 1) + '´');

      {-------------- Determine the protocol type and display it -----------}
      CurProtocol := AP^.GetProtocol;
      TempStr := '';
      case AP^.GetCheckType of
        bcNone      : TempStr := bcsNone;
        bcChecksum1 : TempStr := bcsChecksum1;
        bcChecksum2 : TempStr := bcsChecksum2;
        bcCrc16     : TempStr := bcsCrc16;
        bcCrc32     : TempStr := bcsCrc32;
        bcCrcK      : TempStr := bcsCrcK;
      end; { case }
      FastWrite(XLow + 20, YLow + 01, DataAttr, ProtocolTypeString[CurProtocol] + '/' + TempStr);
    end; { if starting }


  with AP^.APort^ do
    begin
      CurBlockSize        := AP^.GetBlockSize;
      CurFileSize         := AP^.GetFileSize;
      CurBytesRemaining   := AP^.GetBytesRemaining;
      CurBytesTransferred := AP^.GetBytesTransferred;
      CurElapsedTics      := AP^.GetElapsedTics;

      FastWrite(XLow + 20, YLow + 02, DataAttr, MakeLen(FileNameShow(AP^.GetPathName), 54, Space, False, false));
      FastWrite(XLow + 20, YLow + 03, DataAttr, MakeLen(FStr(CurFileSize), 13, Space, False, false));
      FastWrite(XLow + 20, YLow + 04, DataAttr, MakeLen(FStr(CurBytesRemaining) + ' ('+FStr(CurBlockSize)+')',
                18, Space, False, false));

      FastWrite(XLow + 55, YLow + 05, DataAttr, MakeLen(FStr(AP^.GetTotalErrors), 9, Space, False, false));

      { Display an empty progress bar on startup and retransmissions }
      if (CurBytesTransferred = 0) OR
          (CurBytesTransferred < LastBytesTransferred) then
             FastWrite(XLow + 20, YLow + 07, DataAttr, NewProgBar);
      if (TotalTransferBytes = (TotalBytesTransferLeft - (CurFileSize - CurbytesRemaining))) then
        FastWrite(XLow + 20, YLow + 08, DataAttr, NewProgBar);

      LastBytesTransferred := CurBytesTransferred;

      { Update the progress bar (if the file size is known) }
      If CurFileSize <> 00 then
        begin
          R := CurBytesRemaining;
          R := R / CurFileSize;
        end
         else R := 01;
      UpdateProgressBar(XLow + 20, YLow + 07, DataAttr, Length(NewProgBar), 1.0 - R);

      if (TotalTransferBytes) > 00 then
        begin
          R := (TotalBytesTransferLeft - (CurFileSize - CurBytesRemaining));
          R := R / TotalTransferBytes;
          UpdateProgressBar(XLow + 20, YLow + 08, DataAttr, Length(NewProgBar), 1.0 - R);
        end; { if }

      { Update status message }
      UpdateStatusMsg(XLow + 20, YLow + 09, DataAttr);

      { Calculate and display throughput }
      if CurElapsedTics > 0 then
        begin
           R := CurBytesTransferred - AP^.GetInitialFilePos;

          If (CurElapsedTics / 18.2) > 00 then
            ActualCPS := Longint(Round(R / (CurElapsedTics / 18.2)));
        end else ActualCPS := Round(0.0);

      FastWrite(XLow + 20, YLow + 05, DataAttr, MakeLen(FStr(Trunc(ActualCPS)) + ' CPS', 9, Space, False, false));
      FastWrite(XLow + 55, YLow + 04, DataAttr, FormatMinSec(EstimateTransferSecs(CurBytesRemaining, ActualCps))  +
                                      ' mins   ');
      InputObj^.UpdateScrn;
    end; { With }

  if transEnd then
      RestoreScreen(TRansScrn);
end; { proc. WindowStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure trans_LogFileProc(Ap: AbstractProtocolPtr; LogFileStatus: LogFileType);
var Counter : Longint;
    CurNr   : Longint;
    CurName : String;
    LfnName : String;
    TagInfo : TagFileRecord;
begin
  CurNr := 01;
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(LogTransfer, 'trans_LogFileProc = FileName = ' + Ap^.GetFileName);
    DebugObj.DebugLog(logTransfer, 'trans_LogFileProc = currTagg = ' + FStr(TaggingObj^.CurrentTagged));
    DebugObj.DebugLog(logTransfer, 'trans_LogFileProc = transfermode = ' + FStr(Longint(LineCfg^.TransferMode)));
  {$ENDIF}

  Case LineCfg^.TransferMode of
    Transmit : begin
                 CurName := JustName(AP^.GetFileName);
                 TaggingObj^.MakeUpName(CurName);
                 CurNr := 00;

                 {$IFDEF WITH_DEBUG}
                   DebugObj.DebugLog(logTransfer, 'trans_LogFileProc = (.998.)');
                 {$ENDIF}

                 for Counter := 01 to TaggingObj^.CurrentTagged do
                  begin
                    TaggingObj^.GetTagHeader(Pred(Counter), TagInfo);

                    {$IFDEF WITH_DEBUG}
                      DebugObj.DebugLog(logTransfer, 'trans_LogFileProc = (before makeup) '+TagInfo.Name);
                    {$ENDIF}

                    TaggingObj^.MakeUpName(TagInfo.Name);
                    LfnName := GetLfnName(TagInfo.Name, TagInfo.RecordNum, TagInfo.AreaNum);
                    TaggingObj^.MakeUpName(LfnName);

                    {$IFDEF WITH_DEBUG}
                      DebugObj.DebugLog(logTransfer, 'trans_LogFileProc = TagName = '+TagInfo.Name + ' / ' + CurName);
                    {$ENDIF}

                    if (TagInfo.Name = CurName) OR (CurName = LfnName) then
                      begin
                        CurNr := Counter;
                        BREAK;
                      end; { if }
                  end; { for }
               end; { Transmit }
  end; { case }

  if CurNr > 00 then
    begin
      Case LogFileStatus of
         lfReceiveOk    : begin
                            if LineCfg^.TotalUploads in [0..99] then
                             with LineCfg^ do
                             begin
                               Inc(TotalUploads);

                               LineCfg^.UploadInfo^[TotalUploads].FName := AP^.GetFileName;
                               LineCfg^.UploadInfo^[TotalUploads].Size := AP^.GetFileSize;
                               LineCfg^.UploadInfo^[TotalUploads].CPS := SmallWord(ActualCPS);
                             end; { if }
                          end; { if }
         lfReceiveFail  : begin
                            EraseFile(Ap^.GetPathName);
                          end; { failed }
         lfReceiveSkip  : ;
         lfTransmitOk   : begin
                            {$IFDEF WITH_DEBUG}
                              DebugObj.DebugLog(logTransfer, 'trans_LogFileProc - lfTransmitOk - CurNR = ' + FStr(CurNr));
                            {$ENDIF}

                            TaggingObj^.GetTagHeader(Pred(CurNr), TagInfo);

                            TagInfo.FoundFirst := true;
                            TagInfo.xFerTime := SmallWord(ActualCPS);
                            TaggingObj^.SetTagHeader(Pred(CurNr), TagInfo);

                            Dec(TotalBytesTransferLeft, TagInfo.Size);
                          end; { if }
         lfTransmitFail : ;
         lfTransmitSkip : RaLog('!', 'Remote skipped file ['+Ap^.GetfileName+']');
      end; { case }
    end; { if }
end; { proc. trans_LogFileProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function trans_MsgTmpAcceptFile(AP: AbstractProtocolPtr): Boolean;
begin
  Ap^.SetDestinationDirectory('');
  Ap^.SetReceiveFileName('msgtmp.');

  trans_msgTmpAcceptFile := TRUE;
end; { func. trans_MsgTmpAcceptFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function trans_EscHook: Boolean;
var TempCH: Char;
begin
  trans_EscHook := FALSE;
  contrObj^.CheckForEleMon;

  if contrObj^.TimeRemain = 00 then
   if NOT ReadBit(LineCfg^.Exitinfo^.Userinfo.Attribute, 5) then
     begin
       trans_EscHook := true;
       EXIT;
     end; { if }

  if NOT CheckTransfKey then EXIT;
  if (NOT ComObj^.Com_Carrier) then
    begin
      trans_EscHook := TRUE;
      EXIT
    end; { if }

  TempCH := #00;
  {$IFNDEF WINGUI}
    if Crt.KeyPressed then TempCH := Crt.ReadKey;
  {$ELSE}
    if Form1.ColorConsole1.KeyPressed then
      TempCH := Form1.ColorConsole1.ReadKey;
    if ProgTerminated then TempCH := #27;
    if Application.Terminated then TempCH := #27;
  {$ENDIF}

  trans_EscHook := (TempCH = #27);
end; { func. trans_Eschook }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

end. { unit PROTPROC }
