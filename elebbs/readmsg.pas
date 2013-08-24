unit ReadMsg;
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
** Reading message routines for EleBBS
**
** Copyright (c) 1996-1998 by Maarten Bekers
**
** Created : 03-Jan-1998
** Last update : 14-Oct-2001
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
uses MkMsgabs, Global, CfgRec;


type tReadMsgObj = Object
         {-- Constructors and destructors ----------------------------------}
         constructor Init;
         destructor Done;

         {-- Generic routines ----------------------------------------------}
         {$IFDEF WITH_FULL}
         function  ReadMessage(Boardnr, MsgNr: Longint; MsgPause: Boolean; var LineTeller: Byte; var NextMsgNr: Longint;
                               FrmMailBox: Boolean): MailReturnType;
         {$ENDIF}
         procedure HandleMessageRead(var MsgRead      : AbsMsgPtr;
                                         FrmMailBox   : Boolean;
                                     var ConfirmAccept: Boolean;
                                     var MessageInf   : MessageRecord;
                                         BoardNr      : Word;
                                     var MsgNrWritten : Longint);

         {-- Private routines ----------------------------------------------}
         private
           {$IFDEF WITH_FULL}
             function Attr2Str(const MsgRead: AbsMsgPtr): String;
             function BrackStr(CH: Char): String;
             function ShortMsgAttr2Str(var MsgRead: AbsMsgPtr;
                                        var DeleteMsg: Boolean): String;
             function ShortMsgNetAttr2Str(var MsgRead: AbsMsgPtr): String;
             function  SysOpAdjustMenu(var MsgRead   : AbsMsgPtr;
                                       var MessageInf: MessageRecord;
                                       var EleMsgInf : EleMessageRecord;
                                       var TempCH    : Char): Boolean;
             procedure MsgSelectNewArea(var MsgInf: MessageRecord);
             procedure ToggleMsgAttributes(var MsgRead: AbsMsgPtr;
                                            var DeleteMSG: Boolean);
             procedure ToggleNetAttributes(var MsgRead: AbsMsgPtr);
           {$ENDIF}
     end; { tReadMsgObj }

type pReadMsgObj = ^tReadMsgObj;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
uses Crt, Avatar, AreaSel, WriteMsg, FileObj, Colors, ObjDec,
     Support, InOut_U, Mail, MailInt, StUtils, Control, BitWise, FastScrn, RAL,
      Transfer, MkOpen, LineEd, Access_U, Debug_U, ElLog_U,
      {$IFDEF WINGUI} Win_Main, {$ELSE} ScrnU,{$ENDIF}
       FileRout, FileSys, Cases, StrPath, LongStr, MkMsgJam,
        CentrStr, MemMan, GenFile, WordStr, Question, Input_U,
         User_U, Terminal, StrUnit, elx_BBS, JDates;
{$ELSE}
uses RAL, MemMan, Cases, MkOpen, MkMsgJam,
     Mail, GenFile, FileRout, Debug_U, ObjDec,
     LongStr, BitWise, StrUnit, JDates;
{$ENDIF}


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
function PipeCodesUserHook(C: Char; var RetStr: UserHookString): Boolean;
var Back,
    Fore   : Byte;
    TempC  : Byte;
begin
  PipeCodesUserHook := false;
  termObj^.TermHookString := termObj^.TermHookString + C;

  if termObj^.TermHookString[1] = '|' then
    begin
      RetStr := '';
      PipeCodesUserHook := true;
    end { if }
      else termObj^.TermHookString := '';

  if Length(termObj^.TermHookString) >= 3 then
    begin
      if termObj^.TermHookString[1] = '|' then
        begin
          {$IFNDEF WINGUI}
             GetColors(Crt.TextAttr, Back, Fore);
          {$ELSE}
             GetColors(Form1.ColorConsole1.TextAttr, Back, Fore);
          {$ENDIF}

          if (termObj^.TermHookString[2] in ['0'..'9']) AND
              (termObj^.TermHookString[3] in ['0'..'9']) then
                begin
                  TempC := Ord(termObj^.TermHookString[3]);
                  PipeCodesUserHook := true;

                  RetStr := OutputObj^.GetAttrStr(FVal(Copy(termObj^.TermHookString, 2, 2)), true);
                end
                 else begin
                        PipeCodesUserHook := true;
                        RetStr := termObj^.TermHookString;
                      end; { else }
        end; { if }

      termObj^.TermHookString := '';
    end; { if }
end; { func. PipeCodesUserHook }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

constructor tReadMsgObj.Init;
begin
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

destructor tReadMsgObj.Done;
begin
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure tReadMsgObj.HandleMessageRead(var MsgRead      : AbsMsgPtr;
                                            FrmMailBox   : Boolean;
                                        var ConfirmAccept: Boolean;
                                        var MessageInf   : MessageRecord;
                                            BoardNr      : Word;
                                        var MsgNrWritten : Longint);
var NewMsgText: pStringArrayObj;
begin
  If ((SUpcase(MsgRead^.GetTo)=SUpcase(LineCfg^.Exitinfo^.Userinfo.Handle)) OR
      (SUpcase(MsgRead^.GetTo)=SUpcase(LineCfg^.Exitinfo^.Userinfo.Name))) AND
       (NOT MsgRead^.IsRcvd) then
          begin
            MsgRead^.SetRcvd(True);
            MsgRead^.ReWriteHdr;                        { Update the header }

            if (MsgRead^.IsReqRct) AND (LineCfg^.Exitinfo^.Userinfo.Security>=GlobalCfg^.RaConfig^.ReturnRecSec) then
              begin
                ConfirmAccept := True;

                if MessageInf.Typ in [NetMail] then
                  ConfirmAccept := (GlobalCfg^.RaConfig^.HonourNetReq);

                if ConfirmAccept then
                    begin
                      New(NewMsgText, Init(MaxMsgLines));

                      NewMsgText^.Put(1, 'Confirmation receipt');
                      NewMsgText^.Put(2, 'To   : ' + MsgRead^.GetTo);
                      NewMsgText^.Put(3, 'Date : ' + LangObj^.RaFormatDate(MsgRead^.GetDate,0, y2k_MsgDate,
                            LineCfg^.Exitinfo^.Userinfo.DateFormat) +
                                        ' ' + MsgRead^.GetTime);
                      NewMsgText^.Put(4, 'Rcvd : ' + LangObj^.RaFormatDate(DateStr,0, y2k_MsgDate,
                            LineCfg^.Exitinfo^.Userinfo.DateFormat) +
                                        ' ' + TimeStr(true, false));
                      NewMsgText^.Put(5, 'Subj : ' + MsgRead^.GetSubj);

                      PostMessage(Ra250MsgArea(BoardNr),
                                  MsgRead^.GetFrom,
                                  FullProgName,
                                  'Return receipt confirmation',
                                  false,
                                  true,
                                  true,
                                  false,
                                  false,
                                  false,
                                  false,
                                  05,
                                  NewMsgText^,
                                  MsgNrWritten,
                                  '',
                                  '',
                                  true, false,
                                  DateStr,
                                  TimeStr(true, false),
                                  '',
                                  '',
                                  nilAbsMsgPtr);
                       Dispose(NewMsgText, Done);
                    end; { Confirmation accept }
              end; { Request for Return receipt }
          end; { message isn't received yet }

  {---------------------------- Update lastread pointer ---------------------}
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailInt, 'UserRecord#     : '+ FStr(LineCfg^.Exitinfo^.UserRecord));
    DebugObj.DebugLog(logMailInt, 'UserName        : '+ LineCfg^.Exitinfo^.UserInfo.Name);
    DebugObj.DebugLog(logMailInt, 'JamStrCrc       : '+ FStr(JamStrCrc(LineCfg^.Exitinfo^.Userinfo.Name)));
    DebugObj.DebugLog(logMailInt, 'GetMsgNum       : '+ FStr(MsgRead^.GetMsgNum));
    DebugObj.DebugLog(logMailInt, 'GetLastRead     : '+ FStr(MsgRead^.GetLastRead(LineCfg^.Exitinfo^.UserRecord,
     JamStrCrc(LineCfg^.Exitinfo^.Userinfo.Name))));
  {$ENDIF}

  if NOT FrmMailBox then
    begin
      if MsgRead^.GetLastRead(LineCfg^.Exitinfo^.UserRecord, JamStrCrc(LineCfg^.Exitinfo^.Userinfo.Name)) <
          MsgRead^.GetMsgNum then
           MsgRead^.SetLastRead(LineCfg^.Exitinfo^.UserRecord, MsgRead^.GetMsgNum,
              JamStrCrc(LineCfg^.Exitinfo^.Userinfo.Name));

      if (NOT ReadBit(MessageInf.Attribute, 7)) then
       if MsgRead^.GetMsgNum > LineCfg^.Exitinfo^.Userinfo.LastRead then
         LineCfg^.Exitinfo^.Userinfo.LastRead := MsgRead^.GetMsgNum;  { Update lastread pointer }
    end; { FromMailBox }
end; { proc. HandleMessgeRead }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
function tReadMsgObj.Attr2Str(const MsgRead: AbsMsgPtr): String;
var TmpResult: String;
begin
  TmpResult := '';

  if MsgRead^.IsPriv then TmpResult := TmpResult + #32 + LangObj^.ralGet(ralPrivate2);
  if MsgRead^.IsRcvd then TmpResult := TmpResult + #32 + LangObj^.ralGet(ralReceived1);
  if MsgRead^.IsFAttach then TmpResult := TmpResult + #32 + LangObj^.ralGet(ralFileAtt1);
  if MsgRead^.IsKillSent then TmpResult := TmpResult + #32 + LangObj^.ralGet(ralKillSent1);
  if MsgRead^.IsCrash then TmpResult := TmpResult + #32 + LangObj^.ralGet(ralCrash1);
  if MsgRead^.IsReqRct then TmpResult := TmpResult + #32 + LangObj^.ralGet(ralReqRec1);
  if MsgRead^.IsReqAud then TmpResult := TmpResult + #32 + LangObj^.ralGet(ralAuditReq);
  if MsgRead^.IsRetRct then TmpResult := TmpResult + #32 + LangObj^.ralGet(ralRetRec1);

  Attr2Str := Trim(TmpResult);
end; { func. Attr2Str }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
function TReadMsgObj.BrackStr(CH: Char): String;
begin
  BrackStr := GlobalCfg^.RaConfig^.LeftBracket + CH + GlobalCfg^.RaConfig^.RightBracket;
end; { func. BrackStr }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
procedure TReadMsgObj.ToggleMsgAttributes(var MsgRead: AbsMsgPtr;
                                          var DeleteMSG: Boolean);
var TempCH: Char;
begin
  REPEAT
    WriteLn('`A2:');
    WriteLn;
    Writeln(LangObj^.ralGet(ralToglAttr));
    WriteLn(BrackStr('1'), LangObj^.ralGet(ralDeleted2), LangObj^.Bool2YesStr(DeleteMsg));
    WriteLn(BrackStr('2'), LangObj^.ralGet(ralNetmail3), LangObj^.Bool2YesStr(MsgRead^.IsNetMail));
    WriteLn(BrackStr('3'), LangObj^.ralGet(ralPrivate3), LangObj^.Bool2YesStr(MsgRead^.IsPriv));
    WriteLn(BrackStr('4'), LangObj^.ralGet(ralReceived2), LangObj^.Bool2YesStr(MsgRead^.IsRcvd));
    WriteLn(BrackStr('5'), LangObj^.ralGet(ralLocal3), LangObj^.Bool2YesStr(MsgRead^.IsLocal));
    Writeln;
    Write(LangObj^.ralGet(ralSelAttr));

    REPEAT
      TempCH := InputObj^.ReadKey;
      if ProgTerminated then EXIT;

      { Note: NetMail is only added for JAM and HUDSON }
      Case TempCH of
        '1' : DeleteMsg := NOT DeleteMSG;
        '2' : if MsgRead^.IsNetmail then MsgRead^.SetNetmail(False)
                 else MsgRead^.SetNetMail(True);
        '3' : if MsgRead^.IsPriv then MsgRead^.SetPriv(False)
                 else MsgRead^.SetPriv(True);
        '4' : if MsgRead^.IsRcvd then MsgRead^.SetRcvd(False)
                 else MsgRead^.SetRcvd(True);
        '5' : if MsgRead^.IsLocal then MsgRead^.SetLocal(False)
                 else MsgRead^.SetLocal(True);
      end; { case }

    UNTIL (TempCH in [#13, '1'..'5']);

  UNTIL (TempCH = #13);

  WriteLn;
end; { proc. ToggleMsgAttributes }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
procedure TReadMsgObj.ToggleNetAttributes(var MsgRead: AbsMsgPtr);
var TempCH: Char;
begin
  REPEAT
    WriteLn('`A2:');
    WriteLn;
    Writeln(LangObj^.ralGet(ralToglAttr));
    WriteLn(BrackStr('1'), LangObj^.ralGet(ralKillSent2), LangObj^.Bool2YesStr(MsgRead^.IsKillSent));
    WriteLn(BrackStr('2'), LangObj^.ralGet(ralExported2), LangObj^.Bool2YesStr(MsgRead^.IsSent));
    WriteLn(BrackStr('3'), LangObj^.ralGet(ralFileAtt2), LangObj^.Bool2YesStr(MsgRead^.IsFAttach));
    WriteLn(BrackStr('4'), LangObj^.ralGet(ralCrash2), LangObj^.Bool2YesStr(MsgRead^.IsCrash));
    WriteLn(BrackStr('5'), LangObj^.ralGet(ralReqRec2), LangObj^.Bool2YesStr(MsgRead^.IsReqRct));
    WriteLn(BrackStr('6'), LangObj^.ralGet(ralAudReq), LangObj^.Bool2YesStr(MsgRead^.IsReqAud));
    WriteLn(BrackStr('7'), LangObj^.ralGet(ralRetRec2), LangObj^.Bool2YesStr(MsgRead^.IsRetRct));
    Writeln;
    Write(LangObj^.ralGet(ralSelAttr));

    REPEAT
      TempCH := InputObj^.ReadKey;
      if Progterminated then EXIT;

      { Note: NetMail is only added for JAM and HUDSON }
      Case TempCH of
        '1' : if MsgRead^.IsKillSent then MsgRead^.SetKillSent(False)
                 else MsgRead^.SetKillSent(True);
        '2' : if MsgRead^.IsSent then MsgRead^.SetSent(False)
                 else MsgRead^.SetSent(True);
        '3' : if MsgRead^.IsFAttach then MsgRead^.SetFattach(False)
                 else MsgRead^.SetFAttach(True);
        '4' : if MsgRead^.IsCrash then MsgRead^.SetCrash(False)
                 else MsgRead^.SetCrash(True);
        '5' : if MsgRead^.IsReqRct then MsgRead^.SetReqRct(False)
                 else MsgRead^.SetReqRct(True);
        '6' : if MsgRead^.IsReqAud then MsgRead^.SetReqAud(False)
                 else MsgRead^.SetReqAud(True);
        '7' : if MsgRead^.IsRetRct then MsgRead^.SetRetRct(False)
                 else MsgRead^.SetRetRct(True);
      end; { Case }

    UNTIL (TempCH in [#13, '1'..'7']);

  UNTIL (TempCH = #13);

  WriteLn;
end; { proc. ToggleAttributes }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
function TReadMsgObj.ShortMsgAttr2Str(var MsgRead: AbsMsgPtr;
                                      var DeleteMsg: Boolean): String;
var TmpResult: String;
begin
  TmpResult := '';

  if DeleteMsg then TmpResult := TmpResult + #32 + LangObj^.ralGet(ralDeleted1);
  if MsgRead^.IsNetMail then TmpResult := TmpResult + #32 + LangObj^.ralGet(ralNetMail2);
  if MsgRead^.IsPriv then TmpResult := TmpResult + #32 + LangObj^.ralGet(ralPrivate2);
  if MsgRead^.IsRcvd then TmpResult := TmpResult + #32 + LangObj^.ralGet(ralReceived1);
  if MsgRead^.IsLocal then TmpResult := TmpResult + #32 + LangObj^.ralGet(ralLocal2);

  ShortMsgAttr2Str := Trim(TmpResult);
end; { func. ShortMsgAttr2Str }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
function TReadMsgObj.ShortMsgNetAttr2Str(var MsgRead: AbsMsgPtr): String;
var TmpResult: String;
begin
  TmpResult := '';

  if MsgRead^.IsKillSent then TmpResult := TmpResult + #32 + LangObj^.ralGet(ralKillSent1);
  if MsgRead^.IsSent then TmpResult := TmpResult + #32 + LangObj^.ralGet(ralSent2);
  if MsgRead^.IsFAttach then TmpResult := TmpResult + #32 + LangObj^.ralGet(ralFileAtt1);
  if MsgRead^.IsCrash then TmpResult := TmpResult + #32 + LangObj^.ralGet(ralCrash1);
  if MsgRead^.IsReqRct then TmpResult := TmpResult + #32 + LangObj^.ralGet(ralReqRec1);
  if MsgRead^.IsReqAud then TmpResult := TmpResult + #32 + LangObj^.ralGet(ralAuditReq);
  if MsgRead^.IsRetRct then TmpResult := TmpResult + #32 + LangObj^.ralGet(ralRetRec1);

  ShortMsgNetAttr2Str := Trim(TmpResult);
end; { func. ShortMsgNetAttr2Str }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
procedure TReadMsgObj.MsgSelectNewArea(var MsgInf: MessageRecord);
var AreaTemp: String;
    BoardNr : Word;
begin
  {--------------- Show the user an areanumber to forward to ---------------}
  ShowMsgAreaNewMail('/NONEW', false, true, false);

  AreaTemp := '';
  Write('`A15:' , LangObj^.ralGet(ralNetMsgNr));
  GetString(AreaTemp, 5, ['0'..'9'], false, false, false, false);

  if (FVal(AreaTemp) = 00) then EXIT;
  BoardNr := FVal(AreaTemp);

  WriteLn;
  GetMessageRecord(MsgInf, BoardNr, false);
end; { proc. SelectNewArea }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
function tReadMsgObj.SysOpAdjustMenu(var MsgRead   : AbsMsgPtr;
                                     var MessageInf: MessageRecord;
                                     var EleMsgInf : EleMessageRecord;
                                     var TempCH    : Char): Boolean;


procedure EditFidoAddress(var TempAddr: AddrType);
var Address: String;
begin
  {-- .. -------------------------------------------------------------------}
  WriteLn;
  WriteLn;

  {-- Convert to a string --------------------------------------------------}
  With TempAddr do
    AddrToString(Address, Zone, Net, Node, Point);

  {-- Write to the display and stuff ... -----------------------------------}
  Write('`A14:', LangObj^.ralGet(ralAddress));
  GetString(Address, 15, [#32..#254], True, False, False, False);

  {-- empty TempAddr else the default will be kept -------------------------}
  FillChar(TempAddr, SizeOf(TempAddr), #0);

  {-- and convert back to an address ---------------------------------------}
  StringToAddr(Address, TempAddr.Zone,
                        TempAddr.Net,
                        TempAddr.Node,
                        Tempaddr.Point);
end; { proc. EditoFidoAddress }

var OldMessageInf  : MessageRecord;
    DeleteMSG      : Boolean;
    MsgWrite       : AbsMsgPtr;
    TempAddr       : AddrType;
    TempStr        : String;
    AddFromStr     : String;
    AddToStr       : String;
begin
  OldMessageInf := MessageInf;             { Sometimes used for forwarding }
  DeleteMsg := false;

  REPEAT
    WriteLn('`A11:');
    WriteLn;
    Writeln(LangObj^.ralGet(ralSysAdjMnu));
    Writeln('`A3:');

    {-- Clear the fido address, if necessary -------------------------------}
    AddFromStr := '';
    AddToStr := '';

    if MessageInf.Typ in [NetMail] then
      begin
        MsgRead^.GetOrig(TempAddr);
        AddrToString(AddFromStr, TempAddr.Zone, TempAddr.Net, TempAddr.Node, TempAddr.Point);

        AddFromStr := ' ' + LangObj^.ralGet(ralOn2) + ' (' + AddFromStr + ')';

        MsgRead^.GetDest(TempAddr);
        AddrToString(AddToStr, TempAddr.Zone, TempAddr.Net, TempAddr.Node, TempAddr.Point);

        AddToStr := ' ' + LangObj^.ralGet(ralOn2) + ' (' + AddToStr + ')';
      end; { messageinf. typ }


    WriteLn(BrackStr('1'), LangObj^.ralGet(ralAreaNr), MessageInf.AreaNum, ' - ', MessageInf.Name);
    WriteLn(BrackStr('2'), LangObj^.ralGet(ralFrom3), MsgRead^.GetFrom, AddFromStr);
    WriteLn(BrackStr('3'), LangObj^.ralGet(ralTo4), MsgRead^.GetTo, AddToStr);
    WriteLn(BrackStr('4'), LangObj^.ralGet(ralSubject4), MsgRead^.GetSubj);
    WriteLn(BrackStr('5'), LangObj^.ralGet(ralAttr), ShortMsgAttr2Str(MsgRead, DeleteMsg));

    if MessageInf.Typ = EchoMail then
      begin
        WriteLn(BrackStr('6'), LangObj^.ralGet(ralExported1),
                LangObj^.Bool2YesStr(MsgRead^.IsSent))
      end
        else begin
               WriteLn(BrackStr('6'), LangObj^.ralGet(ralExtAttr),
                       ShortMsgNetAttr2Str(MsgRead));
             end; { if }

    WriteLn(BrackStr(LangObj^.ralGetKey(ralSaveExit)), #32, LangObj^.ralGetStr(ralSaveExit));
    WriteLn(BrackStr(LangObj^.ralGetKey(ralQuit2)), #32, LangObj^.ralGetStr(ralQuit2));
    WriteLn;
    Write('`A15:',LangObj^.ralGet(ralSelect));

    REPEAT
      TempCH := UpCase(InputObj^.ReadKey);
      if Progterminated then EXIT;

      Case TempCH of
        '1' : MsgSelectNewArea(MessageInf);
        '2' : begin
                {-- Ask a new "from:" address ------------------------------}
                WriteLn('`A3:');
                WriteLn;
                Write('`A14:',LangObj^.ralGet(ralFrom1));
                TempStr := MsgRead^.GetFrom;
                GetString(TempStr, 35, [#32..#254], True, False, False, False);
                MsgRead^.SetFrom(TempStr);

                {-- If necessary, ask a new address ------------------------}
                if MessageInf.Typ = Netmail then
                  begin
                    MsgRead^.GetOrig(TempAddr); { Get address first }
                    EditFidoAddress(TempAddr);
                    MsgRead^.SetOrig(TempAddr);
                  end; { if }

              end; { From }
        '3' : begin
                WriteLn('`A3:');
                WriteLn;
                Write('`A14:',LangObj^.ralGet(ralTo1));
                TempStr := MsgRead^.GetTo;
                GetString(TempStr, 35, [#32..#254], True, False, False, False);

                MsgRead^.SetTo(TempStr);

                {-- If necessary, ask a new address ------------------------}
                if MessageInf.Typ = netmail then
                  begin
                    MsgRead^.GetDest(TempAddr); { Get address first }
                    EditFidoAddress(TempAddr);
                    MsgRead^.SetDest(TempAddr);
                  end; { if }
              end; { To }
        '4' : begin
                WriteLn('`A3:');
                WriteLn;
                Write('`A14:',LangObj^.ralGet(ralSubject2));

                TempStr := MsgRead^.GetSubj;
                GetString(TempStr, 72, [#32..#254], True, False, False, False);

                MsgRead^.SetSubj(TempStr);
              end; { Subject }
        '5' : ToggleMsgAttributes(MsgRead, DeleteMSG);
        '6' : begin
                if MessageInf.Typ=EchoMail then
                 if MsgRead^.IsSent then MsgRead^.SetSent(False)
                   else MsgRead^.SetSent(True);

                if MessageInf.Typ<>EchoMail then
                 ToggleNetAttributes(MsgRead);
              end; { Toggle .. }
      end; { Case }

    UNTIL (TempCH in ['1'..'6', LangObj^.ralGetKey(ralSaveExit), LangObj^.ralGetKey(ralQuit2)]);
  UNTIL (TempCH in [LangObj^.ralGetKey(ralSaveExit), LangObj^.ralGetKey(ralQuit2)]);


  {------------------------ Update (or discard) the changes ----------------}
  if (TempCH = LangObj^.ralGetKey(ralSaveExit)) then
    begin
      MsgRead^.ReWriteHdr;                         { Save changes to header }
    end
      else begin
             MsgRead^.MsgStartUp;                           { ReRead header }
             DeleteMsg := False;
             MessageInf := OldMessageInf;
           end; { ReRead header }

  {-------------------- If the area is different, forward it ---------------}
  if Messageinf.AreaNum <> OldMessageInf.AreaNum then
    begin
      if NOT OpenOrCreateMsgArea(MsgWrite,
                                 MakeMsgId(MessageInf.AreaNum,
                                           MessageInf.JamBase,
                                           MessageInf.Attribute,
                                           EleMsgInf.Attribute)) then EXIT;

      MsgRead^.MsgStartUp;
      MsgRead^.MsgTxtStartup;

      { Forward the complete header }
      MsgWrite^.SetMailType(mmtNormal);
      MsgWrite^.StartNewMsg;
      MsgRead^.GetDest(TempAddr);             {Set header fields}
      MsgWrite^.SetDest(TempAddr);
      MsgRead^.GetOrig(TempAddr);
      MsgWrite^.SetOrig(TempAddr);
      MsgWrite^.SetFrom(MsgRead^.GetFrom);
      MsgWrite^.SetTo(MsgRead^.GetTo);
      MsgWrite^.SetSubj(MsgRead^.GetSubj);
      MsgWrite^.SetCost(MsgRead^.GetCost);
      MsgWrite^.SetRefer(MsgRead^.GetRefer);
      MsgWrite^.SetSeeAlso(MsgRead^.GetSeeAlso);
      MsgWrite^.SetDate(MsgRead^.GetDate);
      MsgWrite^.SetTime(MsgRead^.GetTime);
      MsgWrite^.SetLocal(MsgRead^.IsLocal);
      MsgWrite^.SetRcvd(MsgRead^.IsRcvd);
      MsgWrite^.SetPriv(MsgRead^.IsPriv);
      MsgWrite^.SetCrash(MsgRead^.IsCrash);
      MsgWrite^.SetKillSent(MsgRead^.IsKillSent);
      MsgWrite^.SetSent(MsgRead^.IsSent);
      MsgWrite^.SetFAttach(MsgRead^.IsFAttach);
      MsgWrite^.SetReqRct(MsgRead^.IsReqRct);
      MsgWrite^.SetRetRct(MsgRead^.IsRetRct);
      MsgWrite^.SetFileReq(MsgRead^.IsFileReq);
      MsgWrite^.SetEcho(False);

      While NOT MsgRead^.EOM do                 { Forward the message text }
        MsgWrite^.DoStringLn(MsgRead^.GetString(250));

      NewWriteMsg(MsgWrite);
      CloseMsgArea(MsgWrite);                            { Close this area }

      DeleteMsg := TRUE;
    end; { if forward this }

  if DeleteMsg then                    { delete the message when necessary }
    DoDeleteMsg(MsgRead);

  SysOpAdjustMenu := DeleteMsg;
end; { func. SysOpAdjustmenu }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
function tReadMsgObj.ReadMessage(Boardnr, MsgNr: Longint;
                                 MsgPause: Boolean;
                                 var LineTeller: Byte;
                                 var NextMsgNr: Longint;
                                 FrmMailBox: Boolean): MailReturnType;
var MessageInf      : MessageRecord;
    EleMsgInf       : EleMessageRecord;
    MsgRead         : AbsMsgPtr;
    MsgText         : String;
    MsgNrWritten    : Longint;
    TotalDisplayed  : Word;
    Counter         : Word;
    UserRecord      : Longint;
    TempL           : Longint;
    UserInfo        : UsersRecord;
    UserExt         : UserExtensionRecord;
    SaveTxtPos      : LongInt;
    FullScreen      : Boolean;
    ConfirmAccept   : Boolean;
    AbortView       : Boolean;
    GoodKeys        : CharSet;
    Refer           : Array[1..3] of LongInt;
    CH              : Char;
    StartScrolling  : Boolean;
    TempDir         : String;
    StartMsgTextPos : Byte;


{* fixed up by Scott Little *}
procedure ClearTheScreen(const WholeScrn, MsgPause: Boolean; var LineTeller: Byte);
var StartLine : Byte;
    Counter   : Byte;
begin
  if WholeScrn then StartLine := 01
    else Startline := StartMsgTextPos;

  if FullScreen then
    begin
      Flush(Output);

      {----------------- Clear the remote screen (ANSI) --------------------}
      if (NOT lineCfg^.AvatarOn) then
        begin
          Write('`A3:');
          if (not WholeScrn) and (LineCfg^.Exitinfo^.Baud > 00) then
            for Counter := (LineCfg^.Exitinfo^.Userinfo.Screenlength - 1) downto StartLine do
             begin
              OutputObj^.SendString(OutputObj^.OneLineUpStr + OutputObj^.MakeXyStr(01, OutputObj^.WhereY) +
                                OutputObj^.MakeClrEolStr + chr(13) + chr(10) + OutputObj^.OneLineUpStr);
             end; { Only send to the remote }
            OutputObj^.SendString(OutputObj^.OneLineUpStr);
        end; { if not avatar }

      if MsgPause then LineTeller := StartMsgTextPos
        else Inc(LineTeller, 6);
    end; { ReadBit }
end; { proc. ClearTheScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowMsgHeader;
var FromStr   : String;
    ToStr     : String;
    SubjStr   : String;
    TempAddr  : Addrtype;
    Question  : QuestObj;

    SaveStop  : Boolean;
    RunScript : Boolean;
    TmpArray  : Array[0..9] of String;
    elxObj    : pElxBbsObj;
begin
  SaveStop := OutputObj^.StopMore;       { Else ansi-files won't be displayed! }
  OutputObj^.SetStopMore(false);

  if MsgPause then
    begin
      ClearTheScreen(True, MsgPause, LineTeller);

      FromStr := '';
      ToStr := '';
      RunScript := FALSE;

      if MessageInf.Typ in [NetMail] then
         begin
           MsgRead^.GetOrig(TempAddr);
           AddrToString(FromStr, TempAddr.Zone, TempAddr.Net, TempAddr.Node, TempAddr.Point);

           FromStr := ' ' + LangObj^.ralGet(ralOn2) + ' (' + FromStr + ')';

           MsgRead^.GetDest(TempAddr);
           AddrToString(ToStr, TempAddr.Zone, TempAddr.Net, TempAddr.Node, TempAddr.Point);

           ToStr := ' ' + LangObj^.ralGet(ralOn2) + ' (' + ToStr + ')';
         end; { messageinf. typ }

      if NOT MsgRead^.IsFAttach then
        SubjStr := MsgRead^.GetSubj
         else SubjStr := langObj^.ralGet(ralAttFiles2);

      if GetScriptType('RDMSGPS') = 'Q-A' then
        begin
          Question.Init;
          Question.Qinfo^.Answers^.Put(01, MessageInf.name);
          Question.Qinfo^.Answers^.Put(02, FStr(MsgRead^.GetMsgNum));
          Question.Qinfo^.Answers^.Put(03, LangObj^.RaFormatDate(MsgRead^.GetDate, 0, y2k_MsgDate,
              LineCfg^.Exitinfo^.Userinfo.DateFormat));
          Question.Qinfo^.Answers^.Put(04, MsgRead^.GetTime);
          Question.Qinfo^.Answers^.Put(05, Attr2Str(MsgRead));
          Question.Qinfo^.Answers^.Put(06, MsgRead^.GetFrom);
          Question.Qinfo^.Answers^.Put(07, FromStr);
          Question.Qinfo^.Answers^.Put(08, MsgRead^.GetTo);
          Question.Qinfo^.Answers^.Put(09, ToStr);
          Question.QInfo^.Answers^.Put(10, SubjStr);

          Question.Process('RDMSGPS /N', false, '');
          RunScript := (Question.GetError = 0);

          Question.Done;
        end  { if }
         else begin
                {-- Write the text file to pass data -------------------------}
                TmpArray[0] := MessageInf.Name;
                TmpArray[1] := FStr(MsgRead^.GetMsgNum);
                TmpArray[2] := LangObj^.RaFormatDate(MsgRead^.GetDate, 0, y2k_MsgDate,
                    LineCfg^.Exitinfo^.Userinfo.DateFormat);
                TmpArray[3] := MsgRead^.GetTime;
                TmpArray[4] := Attr2Str(MsgRead);
                TmpArray[5] := MsgRead^.GetFrom;
                TmpArray[6] := FromStr;
                TmpArray[7] := MsgRead^.GetTo;
                TmpArray[8] := ToStr;
                TmpArray[9] := SubjStr;
                CreateTextFile('rdmsgps.inf', TmpArray);

                {-- Now actually run the script ------------------------------}
                New(elxObj, Init);
                RunScript := elxObj^.RunElexerScript('rdmsgps', '', true);
                Dispose(elxObj, Done);

                {-- and erase that file --------------------------------------}
                EraseFile('rdmsgps.inf');
              end; { else }

      if NOT RunScript then
        begin
          TextColor(Blue);
          Textbackground(Lightgray);
          Write(OutputObj^.MakeClrEolStr);

          Writeln(#32, #254, #32, MessageInf.Name,'`X73:`F4:#', MsgRead^.GetMsgNum);
          Write('`A14:',LangObj^.ralGet(ralDt), '`A10:',LangObj^.RaFormatDate(MsgRead^.GetDate, 0, y2k_MsgDate,
              LineCfg^.Exitinfo^.Userinfo.DateFormat), #32, MsgRead^.GetTime);
          WriteLn('`X33:`A15:', Attr2Str(MsgRead));
          WriteLn('`A14:',LangObj^.ralGet(ralBy), '`A11:',MsgRead^.GetFrom + FromStr);
          WriteLn('`A14:',LangObj^.ralGet(ralTo3), '`A11:',MsgRead^.GetTo + ToStr);

          if NOT MsgRead^.IsFAttach then
            WriteLn('`A14:',LangObj^.ralGet(ralRe), '`A11:', MsgRead^.GetSubj)
             else WriteLn('`A14:', LangObj^.ralGet(ralRe), '`A15:', langObj^.ralGet(ralAttFiles2));
          TextBackground(Blue); Write(OutputObj^.MakeClrEolStr);

          WriteLn;
        end; { if }

      StartMsgTextPos := OutputObj^.WhereY;
    end
      else begin
             FromStr := '';
             ToStr := '';
             RunScript := FALSE;

             if GetScriptType('RDMSG') = 'Q-A' then
               begin
                 Question.Init;
                 Question.Qinfo^.Answers^.Put(01, MessageInf.name);
                 Question.Qinfo^.Answers^.Put(02, FStr(MsgRead^.GetMsgNum));
                 Question.Qinfo^.Answers^.Put(03, LangObj^.RaFormatDate(MsgRead^.GetDate, 0, y2k_MsgDate,
                    LineCfg^.Exitinfo^.Userinfo.DateFormat));
                 Question.Qinfo^.Answers^.Put(04, MsgRead^.GetTime);
                 Question.Qinfo^.Answers^.Put(05, Attr2Str(MsgRead));
                 Question.Qinfo^.Answers^.Put(06, MsgRead^.GetFrom);
                 Question.Qinfo^.Answers^.Put(07, FromStr);
                 Question.Qinfo^.Answers^.Put(08, MsgRead^.GetTo);
                 Question.Qinfo^.Answers^.Put(09, ToStr);
                 Question.Qinfo^.Answers^.Put(10, MsgRead^.GetSubj);
                 Question.Process('RDMSG /N', false, '');

                 RunScript := (Question.GetError = 0);
                 Question.Done;
               end { GetScriptType }
                 else begin
                        {-- Write the text file to pass data -----------------}
                        TmpArray[0] := MessageInf.Name;
                        TmpArray[1] := FStr(MsgRead^.GetMsgNum);
                        TmpArray[2] := LangObj^.RaFormatDate(MsgRead^.GetDate, 0, y2k_MsgDate,
                          LineCfg^.Exitinfo^.Userinfo.DateFormat);
                        TmpArray[3] := MsgRead^.GetTime;
                        TmpArray[4] := Attr2Str(MsgRead);
                        TmpArray[5] := MsgRead^.GetFrom;
                        TmpArray[6] := FromStr;
                        TmpArray[7] := MsgRead^.GetTo;
                        TmpArray[8] := ToStr;
                        TmpArray[9] := SubjStr;
                        CreateTextFile('rdmsg.inf', TmpArray);

                        {-- Now actually run the script ----------------------}
                        New(elxObj, Init);
                        RunScript := elxObj^.RunElexerScript('rdmsg', '', true);
                        Dispose(elxObj, Done);

                        {-- and erase that file ------------------------------}
                        EraseFile('rdmsg.inf');
                      end; { else }

             if NOT RunScript then
               begin
                 WriteLn;
                 Writeln('`A15:',FirstUpper(LangObj^.ralGet(ralMessage)), ' #', MsgRead^.GetMsgNum, ' - ',
                        MessageInf.Name,'  ', Attr2Str(MsgRead));
                 WriteLn('`A14:',LangObj^.ralGet(ralDt), '`A10:',LangObj^.RaFormatDate(MsgRead^.GetDate, 0, y2k_MsgDate,
                   LineCfg^.Exitinfo^.Userinfo.DateFormat), MsgRead^.GetTime);
                 WriteLn('`A14:',LangObj^.ralGet(ralBy), '`A11:',MsgRead^.GetFrom);
                 WriteLn('`A14:',LangObj^.ralGet(ralTo3), '`A11:',MsgRead^.GetTo);
                 WriteLn('`A14:',LangObj^.ralGet(ralRe), '`A11:',MsgRead^.GetSubj);
                 WriteLn;
               end; { if }
           end; { not pause }

  if FullScreen then TotalDisplayed := 00
    else TotalDisplayed := StartMsgTextPos - 1;

  OutputObj^.SetStopMore(SaveStop);
end; { proc. ShowMsgHeader }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ExportMessage;
var FName   : String;
    TempStr : String;
    Export_F: pFileObj;
begin
  FName := '';
  Write('`A14:', LangObj^.ralGet(ralExpName));

  GetString(FName, 78 - OutputObj^.WhereX, [#32..#254], false, false, false, false);
  if Trim(FName) = '' then FName :=  '';

  New(Export_F, Init);
  Export_F^.Assign(FName);
  Export_F^.FileMode := ReadWriteMode + DenyNone;

  if NOT Export_F^.OpenOrCreate(1) then
    begin
      WriteLn('`A3:', LangObj^.ralGet(ralCreaErr));
      InputObj^.PressEnter(false, true);
      EXIT;
    end; { if }

  WriteLn('`A1:', LangObj^.ralGet(ralExporting));

  Export_F^.WriteLn(FirstUpper(LangObj^.ralGet(ralMessage))  + ' #' + FStr(MsgNr) + ' - ' + MessageInf.Name);
  Export_F^.WriteLn(LangObj^.ralGet(ralDate) + LangObj^.RaFormatDate(MsgRead^.GetDate, 0, y2k_MsgDate,
      LineCfg^.Exitinfo^.Userinfo.DateFormat) + #32 + MsgRead^.GetTime);
  Export_F^.WriteLn(LangObj^.ralGet(ralFrom2) + MsgRead^.GetFrom);
  Export_F^.WriteLn(LangObj^.ralGet(ralTo2) + MsgRead^.GetTo);
  Export_F^.WriteLn(LangObj^.ralGet(ralSubject3) + MsgRead^.GetSubj);
  Export_F^.WriteLn(Dup('-', 79));

  While (NOT MsgRead^.EOM) AND (Export_F^.IoResult = 0) do
    begin
      TempStr := MsgRead^.GetString(79);

      if TempStr[1] = #01 then                                { Kludge-string }
        TempStr[1] := '@';

      Export_F^.WriteLn(TempStr);
    end; { Write Message text }

  Export_F^.WriteLn('');
  Dispose(Export_F, Done);
end; { proc. ExportMessage }


procedure ReshowMessage;
begin
{  MsgRead^.MsgTxtStartUp;
  ShowMsgHeader;
}
  ReadMessage := msgAgain;
end; { proc. ReShowMessage }


{----- Procedure for displaying message text and handling counters etc -----}
function DisplayMessageText(MsgText: String; var CH: Char): Boolean;

procedure ShowText(Color: Byte; S: String; NewColor: Byte);
begin
{  if AvatarOn then Write('`Y', LineCfg^.Exitinfo^.Userinfo.ScreenLength,':'); }
  if Color <> 0 then
    Write('`A', Color, ':');
  LineCfg^.RaduCodes := false;

  Write(s);

  LineCfg^.RaduCodes := true;

  if NewColor <> 0 then
    Write('`A', NewColor,':');
end; { proc. ShowText }

procedure ScrollOneLineUp;
begin
  if NOT Startscrolling then EXIT;

  { <^V><^J><numlines><upper><left><lower><right> - scroll area up }
  AvtObj^.AvStr(^V + ^J + #01 + Chr(StartMsgTextPos) + #01 + Chr(LineCfg^.actScrnLen) + #80);

  if lineCfg^.AvatarOn then
   With LineCfg^.Exitinfo^.Userinfo do
    OutputObj^.SendString(^V + ^J + #01 + Chr(StartMsgTextPos) + #01 + Chr(Screenlength) + #80)
     else OutputObj^.SendString(#10);
end; { proc. ScrollOneLineUp }


begin
  DisplayMessageText := true;

  {------------------ Make sure the srceen scrolls correctly ---------------}
  ScrollOneLineUp;

  {---------------------- Check for the abort keys and stuff ---------------}
  if InputObj^.Key_HasPressed(['P', 'p'], CH) then
    begin
      InputObj^.DorCleanKeys;
      InputObj^.ReadKey;
    end; { if }

  if InputObj^.Key_HasPressed(LineCfg^.DispAbortSet, CH) then
    begin
      DisplayMessageText := false;
      EXIT
     end; { if }


  if InputObj^.KeyPressed then
   begin
     CH := UpCase(InputObj^.ReadKey);
     if CH in GoodKeys then
         begin
           DisplayMessageText := false;
           EXIT;
         end; { if }
   end; { if }


  if Progterminated then EXIT;


  while Pos(#01, MsgText) > 01 do
    Replace(#01, '@', MsgText);
  if {$IFDEF WINGUI}Form1.ColorConsole1.{$ENDIF}TextAttr = 0 then
    {$IFDEF WINGUI}Form1.ColorConsole1.{$ENDIF}TextAttr := 3;

  {---------------- Check wether this is a replied message -----------------}
    if Pos('>', Copy(MsgText, 1, 15)) > 00 then
      begin
        ShowText(7, MsgText, 3);
      end

  {------------------- Check in this message for kludges -------------------}
  else if Pos(#1, MsgText) > 00 then
      begin
        With GlobalCfg^.RaConfig^ do
          ShowText(MakeAttr(HiFore, HiBack), Copy(MsgText, 2, 255), 3);
      end
  {--------------- Check in this message for origin/tear -------------------}
  else if (Trim(Copy(TrimLeft(MsgText), 1, 4)) = '---') OR
           (Copy(MsgText, 1, 10) = ' * Origin:') then
      begin
        ShowText(2, Copy(MsgText, 1, 255), 0);
      end { Origin/Tear line }
  {------------------- This is a normal messageline ------------------------}
  else begin
         ShowText(0, MsgText, 0);
       end; { else }

  ShowText(0, #13, 0);
  if (NOT StartScrolling) then
    ShowText(0, #10, 0);

  {-------------------- Handle the messageline counter ---------------------}
  Inc(TotalDisplayed);

  if MsgPause then
    begin
      if NOT FullScreen then
       if TotalDisplayed > (LineCfg^.Exitinfo^.Userinfo.ScreenLength - 2) then
         begin
           OutputObj^.AskMore;
           TotalDisplayed := 00;
           LineTeller := 00;
         end; { if }

      if FullScreen then
       if LineCfg^.LocalLogon then
        if TotalDisplayed > ((LineCfg^.actScrnLen) - (StartMsgTextPos+1)) then
          begin
            ScrollOneLineUp;
            OutputObj^.AskMore;
            StartScrolling := true;
            TotalDisplayed := 00;
            LineTeller := 00;
          end; { if }

      if FullScreen then
       if NOT LineCfg^.LocalLogon then
        if TotalDisplayed > (LineCfg^.Exitinfo^.Userinfo.ScreenLength - (StartMsgTextPos+1)) then
          begin
            ScrollOneLineUp;
            OutputObj^.AskMore;
            StartScrolling := true;
            TotalDisplayed := 00;
            LineTeller := 00;
          end; { if }
    end; { if }

  {----------- If we have our own pausing system, make sure that the -------}
  {----------- general EleBBS pausing system doesn't interfere -------------}
  if MsgPause then OutputObj^.ResetLines(01);
end; { proc. DisplayMessageText }

procedure RestoreHook;
begin
  termObj^.UserHook := {$IFDEF FPC}@{$ENDIF}EmptyUserHook;
  termObj^.TermHookString := '';
end; { proc. RestoreHook }

var Question : QuestObj;
    CmdBar   : String;
    TempCH   : Char;
    SysAccess: Boolean;
    elxObj   : pElxBbsObj;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailInt, 'ReadMessage (begin)');
  {$ENDIF}

  {----------------- Initialize initial values ----------------------------}
  StartMsgTextPos := 07;
  ReadMessage := msgStop;                           { To avoid endless loops }
  NextMsgNr   := -1;
  GetMessageRecord(MessageInf, BoardNr, true);
  GetEleMessageRecord(EleMsgInf, BoardNr, true);

  {----------------------- Try to open the messagebase --------------------}
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailInt, 'ReadMessage - opening messagebase');
  {$ENDIF}

  MsgRead := nil;
  if NOT OpenOrCreateMsgArea(MsgRead, MakeMsgId(BoardNr, MessageInf.JamBase, MessageInf.Attribute,
                                                      EleMsgInf.Attribute)) then
    begin
      {$IFDEF WITH_DEBUG}
        DebugObj.DebugLog(logMailInt, 'ReadMessage - open msgbase FAILED');
      {$ENDIF}

      RaLog('!', 'Unable to read message board ('+MessageInf.Name+')');
      CloseMsgArea(MsgRead);
      EXIT;
    end; { if }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailInt, 'ReadMessage - Messagebase opened');
  {$ENDIF}


  {----------------- Lookup the correct areanr and setup header -----------}
  MsgRead^.SeekFirst(MsgNr);                    { Seek to correct message# }
  if NOT MsgRead^.SeekFound then
    begin
      Ralog('!', 'Mail-system: seek error (internal error): '+FStr(MsgNr)+' / BoardNr='+FStr(BoardNr));
      RaLog('!', 'AreaName='+MessageInf.Name+', BoardNr='+FStr(BoardNr)+', AreaNum='+FStr(MessageInf.AreaNum));

      CloseMsgArea(MsgRead);
      EXIT;
    end; { MessageNumber isn't found }

  MsgRead^.MsgStartUp;           { Initialize the header (date, from who etc) }
  MsgRead^.MsgTxtStartUp;                       { Initialize the message text }

  {-- Check if we have SysOp access -----------------------------------------}
  SysAccess := SysOpAccess(MessageInf, LineCfg^.Exitinfo^.UserInfo);

  {----------- Make sure this message can be read (private etc) ------------}
  if NOT ReadMsgAccess(MsgRead^.GetTo, SysAccess, MsgRead^.IsPriv,
                       LineCfg^.Exitinfo^.userinfo) then
        begin
          ReadMessage := msgNext;
          CloseMsgArea(MsgRead);
          EXIT;
        end; { if }

  {---------------------- Initialize the screen area ----------------------}
  if MsgPause then
    Write('`S:');

  FullScreen := ReadBit(LineCfg^.Exitinfo^.Userinfo.Attribute2, 2);
  if NOT MsgPause then FullScreen := False;

  {------------------- Show the message header ----------------------------}
  ShowMsgHeader;
  if OutputObj^.StopMore then
   if NOT MsgPause then
     begin
       CloseMsgArea(MsgRead);
       EXIT;
     end; { if }

  {------- Set the correct colors and make sure More-prompting is on -------}
  OutputObj^.SetStopMore(false);
  LineCfg^.DispMorePrompt := true;
  Abortview := false;
  StartScrolling := false;
  termObj^.TermhookString := '';
  Flush(Output); {!}
  termObj^.UserHook := {$IFDEF FPC}@{$ENDIF}PipeCodesUserHook;

  GoodKeys := [];
  Write('`A3:');                                { Assume messagetext color }

  {--------------------- Startup the message read loop ---------------------}
  repeat
    CH := #00;

    {--------------- Gather messagetext and information --------------------}
    MsgText := MsgRead^.GetString(79);

    If NOT LineCfg^.ShowKludges then
     While (MsgText[01] = ^A) AND (NOT MsgRead^.EOM) do
       MsgText := MsgRead^.GetString(79);

    if NOT DisplayMessageText(MsgText, CH) then
      begin
        AbortView := true;
      end; { if }

    {-------- If the EleBBS viewing system was aborted, abort the rest -----}
    if NOT AbortView then
      begin
        AbortView := OutputObj^.StopMore;
        if AbortView then
         If NOT MsgPause then BREAK;
      end; { if }

    {-------------- Make sure that the carrier isn't dropped ---------------}
    if Progterminated then
      begin
        RestoreHook;
        EXIT;
      end; { if }

    {----- Clear the screen so that the next message text can be viewed ----}
    if LineTeller=0 then
     if ((lineCfg^.AnsiON) OR (lineCfg^.AvatarOn)) AND (FullScreen) then
       ClearTheScreen(False, MsgPause, LineTeller);


    {--------------- Startup the command-input system ----------------------}
    GoodKeys := [];

    if ((MsgRead^.EOM) OR (Abortview)) AND (MsgPause) then
     begin
       Str2Set(LangObj^.ralGetKey(ralAgain1)+
               LangObj^.ralGetKey(ralNext1)+
               LangObj^.ralGetKey(ralLast1)+
               LangObj^.ralGetKey(ralStop1), GoodKeys);

       if NOT (MessageInf.MsgKinds in [ElemsgROnly, ElemsgNoReply]) then
         Include(GoodKeys, LangObj^.ralGetKey(ralReply1));

       if MsgRead^.isFAttach then
         Include(GoodKeys, LangObj^.ralgetKey(ralFiles2));

       if NOT (MessageInf.MsgKinds in [ElemsgROnly]) then
         Include(GoodKeys, LangObj^.ralGetKey(ralEnter1));

       if AllowEdit(LineCfg^.Exitinfo^,
                    MessageInf, EleMsgInf, MsgRead^.GetFrom) then
         Include(GoodKeys, LangObj^.ralGetKey(ralEdit1));

       if SysOpAccess(MessageInf, LineCfg^.Exitinfo^.userinfo) then
         begin
           Include(GoodKeys, LangObj^.ralGetKey(ralDelete2));
           Include(GoodKeys, '!');
           Include(GoodKeys, '*');
           Include(GoodKeys, 'X');
           Include(GoodKeys, '/');
           Include(GoodKeys, 'U');
           Include(GoodKeys, '=');
         end; { if }

       if NOT SysOpAccess(MessageInf, LineCfg^.Exitinfo^.userinfo) then
        if CheckMsgDeleteAccess(MessageInf,
                                MsgRead^.GetTo,
                                MsgRead^.GetFrom,
                                MsgRead^.IsSent,
                                LineCfg^.Exitinfo^.Userinfo) then
         begin
           Include(Goodkeys, LangObj^.ralGetKey(ralDelete2));
         end; { if }

       with GlobalCfg^.RaConfig^ do
         WriteLn('`A', MakeAttr(BarFore, BarBack), ':');

       {------------------- Fill in the reply settings ---------------------}
        Refer[1] := 00;
        Refer[2] := 00;
        Refer[3] := 00;

        Refer[1] := MsgRead^.GetRefer;
        Refer[2] := MsgRead^.GetSeeAlso;
        Refer[3] := MsgRead^.GetNextSeeAlso;

        if (Refer[1]>00) then
         if (Refer[2]=00) AND (Refer[3]=00) then
          begin
            Include(Goodkeys, '-');
          end; { Refer }

        if (Refer[1]=00) then
         if (Refer[2]>00) AND (Refer[3]=00) then
          begin
            Include(Goodkeys, '+');
          end; { Refer }

        if (Refer[1]=00) then
         if (Refer[2]=00) AND (Refer[3]>00) then
          begin
            Include(Goodkeys, '+');
          end; { Refer }

        if (Refer[1]>00) AND ((Refer[2]>00) OR (Refer[3]>00)) then
          begin
            Include(Goodkeys, '+');
            Include(Goodkeys, '-');
          end; { Refer }

       {----------- Check wether we can use the lightbarred reader ---------}
       if (lineCfg^.AnsiOn) OR (lineCfg^.AvatarOn) then
        begin
          CH := #01;

          CmdBar := '';
          if '+' in GoodKeys then CmdBar := CmdBar + ',REPLNEXT';
          if '-' in GoodKeys then CmdBar := CmdBar + ',REPLPREV';

          if CheckMsgAreaAccess(MessageInf, False, False, 00, LineCfg^.Exitinfo^.userinfo) then
            begin
              if NOT (MessageInf.MsgKinds in [ElemsgROnly, ElemsgNoReply]) then
                  CmdBar := CmdBar + ',REPLY';
              if NOT (MessageInf.MsgKinds in [ElemsgROnly]) then
                 CmdBar := CmdBar + ',ENTER';
            end; { if }

          if LangObj^.ralGetKey(ralDelete2) in GoodKeys then
            CmdBar := CmdBar + ',DELETE';

          if AllowEdit(LineCfg^.Exitinfo^,
                       MessageInf, EleMsgInf, MsgRead^.GetFrom) then
            CmdBar := Cmdbar + ',EDIT';

          if MsgRead^.isFAttach then
            CmdBar := CmdBar + ',FILEATTACH';

          if SysOpAccess(MessageInf, LineCfg^.Exitinfo^.userinfo) then
            CmdBar := CmdBar + ',SYSOPACCESS';

          if CmdBar[1] = ',' then Delete(CmdBar, 1, 1);

          if GetScriptType('MSGBAR') = 'Q-A' then
            begin
              Question.Init;
              Question.Process('MSGBAR '+CmdBar+' /N', true, '');

              if Question.GetError = 00 then
                begin
                  CH := UpCase(FirstChar(Question.GetReturnResult));
                end; { if }

              Question.Done;
            end { if }
              else begin
                     New(elxObj, Init);
                     if elxObj^.RunElexerScript('msgbar', CmdBar, false) then
                       begin
                         CH := UpCase(FirstChar(elxObj^.GetElxReturnString));
                       end
                         else begin
                                CH := #01;
                              end; { else }
                     Dispose(elxObj, Done);
                   end; { if }
        end; { if }

       if (CH=#01) OR ((NOT lineCfg^.AnsiOn) AND (NOT lineCfg^.AvatarOn)) then
         begin
           if ('+' in GoodKeys) AND ('-' in GoodKeys) then
             begin
               WriteLn('(-) <- ',LangObj^.ralGet(ralReadRepl),' -> (+), ');
             end
              else begin
                     if '+' in GoodKeys then
                        WriteLn(LangObj^.ralGet(ralReadReply),' -> (+), ');

                     if '-' in GoodKeys then
                        WriteLn('(-) <- ',LangObj^.ralGet(ralReadReply),', ');
                   end; { else }

           Write(LangObj^.ralGetStr(ralAgain1));
           Write(', ', LangObj^.ralGetStr(ralNext1));
           Write(', ', LangObj^.ralGetStr(ralLast1));

           if CheckMsgAreaAccess(MessageInf, False, False, 00, LineCfg^.Exitinfo^.userinfo) then
            begin
              if NOT (MessageInf.MsgKinds in [ElemsgROnly, ElemsgNoReply]) then
                Write(', ', LangObj^.ralGetStr(ralReply1));
              if NOT (MessageInf.MsgKinds in [ElemsgROnly]) then
                Write(', ', LangObj^.ralGetStr(ralEnter1));
            end; { CheckMsgArea Enter access }

           if LangObj^.ralGetKey(ralDelete2) in GoodKeys then
             Write(', ', LangObj^.ralGetStr(ralDelete2));

           if AllowEdit(LineCfg^.Exitinfo^,
                        MessageInf, ElemsgInf, MsgRead^.GetFrom) then
             Write(', ', LangObj^.ralGetStr(ralEdit1));

           if MsgRead^.isFAttach then
              Write(', ', LangObj^.ralGetStr(ralFiles2));

           If SysOpAccess(MessageInf, LineCfg^.Exitinfo^.userinfo) then
              Write(', (!*X/U=)');

           Write(', ', LangObj^.ralGetStr(ralStop1),': ');
         end; { if CH=#01 }

       repeat
         if Progterminated then
           begin
             Restorehook;
             EXIT;
           end; { if }

         if (CH=#01) OR ((NOT lineCfg^.AnsiOn) AND (NOT lineCfg^.AvatarOn)) then
           begin
             CH := UpCase(InputObj^.ReadKey);
             if CH=#13 then CH := UpCase(LangObj^.ralGetKey(ralNext1));
           end; { if }

         if UpCase(CH) = UpCase(LangObj^.ralGetKey(ralFiles2)) then
          if MsgRead^.isFAttach then
               begin
                 {-- First run a sanity check on dir --------------------------}
                 TempDir := MsgRead^.GetSubj;
                 Delete(TempDir, 1, Length(GlobalCfg^.RaConfig^.AttachPath));
                 Insert(GlobalCfg^.RaConfig^.AttachPath, TempDir, 1);

                 if (SupCase(MsgRead^.GetSubj) = SUpcase(TempDir)) then
                   begin
                     DirectDirectoryListing(MsgRead^.GetSubj, True);

                     if FileCount(MsgRead^.GetSubj) > 00 then
                       begin
                         DoDownload(ForceBack(MsgRead^.GetSubj) + '*.*', False, True, False, False, True);
                         WriteLn('`A12:');

                          if (SUpCase(GlobalCfg^.RaConfig^.SysOp) = SUpCase(LineCfg^.Exitinfo^.UserInfo.Name)) OR
                              (SUpCase(MsgRead^.GetFrom) = SUpCase(LineCfg^.Exitinfo^.Userinfo.Name)) OR
                               (SUpCase(MsgRead^.GetTo) = SUpCase(LineCfg^.Exitinfo^.Userinfo.Name))  OR
                                (SysOpAccess(MessageInf, LineCfg^.Exitinfo^.userinfo)) then
                                  if InputObj^.ralStrYesNoAsk(ralCheckAtt) then
                                    begin
                                      WriteLn('`A12:', langObj^.ralGet(ralKillAtt));
                                      EraseDir(ForceBack(MsgRead^.GetSubj) + '*.*', true);
                                      MsgRead^.SetFAttach(False);
                                      MsgRead^.SetSubj('');
                                    end; { killing attached files }

                       end { filecount > 00 }
                        else begin
                                WriteLn('`A12:');
                                Writeln(langObj^.ralGet(ralNoFiles1));
                                InputObj^.PressEnter(False, True);
                                MsgRead^.SetFAttach(False);
                                MsgRead^.SetSubj('');
                             end; { if }

                     MsgRead^.RewriteHdr;                 { Update the header }
                     ReadMessage := msgAgain;            { Redraw the message }
                   end { if }
                     else begin                     { Sanity check failed }
                            RaLog('!', 'Message file attach path has been modified');
                            ReadMessage := msgAgain;
                          end; { else }
               end; { if }

         if SysOpAccess(MessageInf, LineCfg^.Exitinfo^.userinfo) then
          Case CH of
            '!' : begin
                     Writeln(LangObj^.ralGet(ralKludgeTog));
                     if LineCfg^.ShowKludges then LineCfg^.ShowKludges := false
                      else LineCfg^.ShowKludges := true;

                     ReshowMessage;
                     BREAK;
                  end; { Kludge Toggle }
            '*' : begin
                     WriteLn('*');

                     { MsgStartup is needed for the (hudson) fido addrss }
                     MsgRead^.MsgStartup;

                     { must be a different CH else the "GoodKey" will }
                     { be overwritten with the exit-key of the SysOpAdjust }
                     { function }
                     if SysOpAdjustMenu(MsgRead, MessageInf, EleMsgInf, TempCH) then
                           begin
                             MsgRead^.ReWriteHdr;
                             CloseMsgArea(MsgRead);         { Close this area }
                             ReadMessage := msgNext;
                             Restorehook;
                             EXIT;
                           end; { Message is deleted }

                     ReshowMessage;
                     BREAK;
                  end; { SysOp adjustment menu }
            'X' : begin
                     WriteLn(LangObj^.ralGet(ralExport));
                     MsgRead^.MsgTxtStartUp;
                     ExportMessage;

                     ReshowMessage;
                     BREAK;
                  end; { Export message }
            '/' : begin
                    BREAK;
                  end; { Forward message }
            'U' : begin
                    WriteLn(LangObj^.ralGet(ralUsrSec));

                    UserRecord := SearchUser(MsgRead^.GetFrom);
                    if UserRecord=-1 then
                          begin
                            WriteLn('`A12:"', MsgRead^.GetFrom, '" ', LangObj^.ralGet(ralNoSuchUsr));
                            InputObj^.PressEnter(False, True);
                          end { SearchUser }
                           else begin
                                  GetUserRecord(UserRecord, UserInfo, UserExt, false);

                                  TempL := UserInfo.Security;
                                  RangeEdit(TempL,
                                   '`A14:'+LangObj^.ralGet(ralNewLvl)+ ' '+ UserInfo.Name+ ' ('+ FStr(UserInfo.Security)+'): ',
                                   0, LineCfg^.Exitinfo^.Userinfo.Security, UserInfo.Security);
                                   UserInfo.Security := Word(TempL);

                                  WriteuserRecord(UserRecord, UserInfo, UserExt);
                                  InputObj^.PressEnter(True, True);
                                end; { SearchUser }

                    ReshowMessage;
                    BREAK;
                  end; { UserSec }
            '=' : begin
                     WriteLn(LangObj^.ralGet(ralUnReceive));
                     MsgRead^.SetRcvd(False);                { Unreceive message }

                     MsgRead^.ReWriteHdr;
                     CloseMsgArea(MsgRead);                    { Close this area }

                     ReadMessage := msgNext;
                     RestoreHook;
                     EXIT;
                   end; { Unreceive message }

          end; { Case }

        if NOT (CH in GoodKeys) then CH := #01;

       until CH in GoodKeys;
     end; { If MsgRead }

     if OutputObj^.StopMore then
      if NOT MsgPause then BREAK;
  until (CH in GoodKeys) OR (MsgRead^.EOM);

  {-------------- Let the user know what they have pressed -----------------}
  if (LangObj^.ralGetKey(ralAgain1)=CH) then
     begin
       WriteLn(LangObj^.ralGet(ralAgain2));
       ReadMessage := msgAgain;
     end; { Again }
  if CH = '/' then
    begin
      WriteLn(langObj^.ralGet(ralForward1));
      ReadMessage := msgForward;
    end; { if }
  if LangObj^.ralGetKey(ralNext1)=CH then
     begin
       WriteLn(LangObj^.ralGet(ralNext2));
       ReadMessage := msgNext;
     end; { Next }
  if LangObj^.ralGetKey(ralLast1)=CH then
     begin
       WriteLn(LangObj^.ralGet(ralLast2));
       ReadMessage := msgLast;
     end; { Last }
  if LangObj^.ralGetKey(ralReply1)=CH then
     begin
       WriteLn(LangObj^.ralGet(ralReply2));
       ReadMessage := msgReply;
     end; { Reply }
  if LangObj^.ralGetKey(ralEnter1)=CH then
     begin
       WriteLn(LangObj^.ralGet(ralEnter2));
       ReadMessage := msgEnter;
     end; { Enter }
  if LangObj^.ralGetKey(ralStop1)=CH then
     begin
       WriteLn(LangObj^.ralGetStr(ralStop2));
       ReadMessage := msgStop;
     end; { Enter }
  if LangObj^.ralGetKey(ralDelete2)=CH then
     begin
       WriteLn(LangObj^.ralGet(ralDelete3));
       ReadMessage := msgDelete;
     end; { Enter }
  if LangObj^.ralGetKey(ralEdit1)=CH then
     begin
       WriteLn(LangObj^.ralGet(ralEdit1));
       ReadMessage := msgEdit;
     end; { Edit }

  if CH in ['+', '-'] then
    begin
      ReadMessage := msgRdReply;
      NextMsgNr := 00;

      if CH='+' then
        if (Refer[1]>MsgRead^.GetMsgNum) then
          NextMsgNr := Refer[1];

      if CH='+' then
       if (Refer[2]>MsgRead^.GetMsgNum) then
         NextMsgNr := Refer[2];

      if CH='-' then
       if (Refer[1]<MsgRead^.GetMsgNum) then
         NextMsgNr := Refer[1];

      if CH='-' then
       if (Refer[2]<MsgRead^.GetMsgNum) AND (Refer[2] > 0) then
         NextMsgNr := Refer[2];
    end; { Read Next Reply }

  Write('`A7:');

  If NOT MsgPause then ReadMessage := msgNext;
  if Not MsgPause then
    if OutputObj^.StopMore then ReadMessage := msgStop;
  HandleMessageRead(MsgRead, FrmMailBox,
                    ConfirmAccept, MessageInf,
                    BoardNr, MsgNrWritten);


  if LineCfg^.ContrCodes then
    Write('`F', GlobalCfg^.RaConfig^.NormFore, ':`B', GlobalCfg^.RaConfig^.NormBack, ':');
  CloseMsgArea(MsgRead);                                    { Close this area }
  RestoreHook;
end; { func. ReadMessage }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit READMSG }
