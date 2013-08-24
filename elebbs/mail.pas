unit Mail;
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
** Mail routines for EleBBS
**
** Copyright (c) 1996, 1997 by Maarten Bekers
**
** Created : 06-Oct-1996
** Last update : 31-Sep-1997
**
**
** note: Made some adjustments to JAM and HUDSON for adding "IsNetmail" and
**       "SetNetMail" procedures.
** note2: Changed: Instead of using UNum (JAM), now is used "NameCRC", each
**                 program uses UNum differently :(((
**
** note3: MKMSGHUD.PAS locks the MSGINFO.BBS at byte 406, has to be byte 407.
** note5: 'We' use the procedure "NewWriteMsg" because of locking techniques.
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
uses Global, CfgRec, MkMsgAbs, StrUnit;
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

const TPascal_Zuigt = True;

type
  CheckMailCallBackType = procedure(const MessageInf: MessageRecord;
                                    var OldStr: ShortString);

function  GetAreaAddress(BoardNr: Word): String;
function AllowEdit(var Exitinfo  : ExitinfoRecord;
                   var MessageInf: MessageRecord;
                   var EleMsgInf : EleMessageRecord;
                       FromMsg: String): Boolean;
function  InMarkList(BoardNr, MsgNr: Word): Boolean;
function GetActiveMsgNum(MessageInf: MessageRecord;
                         EleMsgInf : EleMessageRecord): Longint;
function  SearchNetMailBoard: Word;

function  DoQuickJamScan(AreaID: String): Boolean;  { Scan for unread mail }
procedure ScanArea(var   BoardInf    : MessageRecord;
                   var   EleBoardInf : EleMessageRecord;
                   const IsHudson,
                         DoEchomail  : Boolean;
                   var   MsgFound    : MailBoxCheckArrayType;
                   var   CurMsgsFound: Longint;
                   const Username,
                         Userhandle  : MsgToIdxRecord);
procedure ScanMailAreas(var MsgFound       : MailBoxCheckArrayType;
                        var CurMsgsFound   : Longint;
                        var OldCheckMailStr: ShortString;
                            CheckMailCB    : CheckMailCallBackType);

procedure FilePost(const AreaNr   : Longint;
                   const FromWho,
                         ToWho,
                         Subject  : String;
                   const FileName,
                         AddText  : String);
procedure MenuPost(MiscData: String);
procedure AddToMarkList(BoardNr, MsgNr: Word);
function GetHighMsgNum(MessageInf: MessageRecord;
                       EleMsgInf : EleMessageRecord): Longint;
procedure SetLastRead(MessageInf: MessageRecord;
                      EleMsgInf : EleMessageRecord;
                      MsgNr: Longint);
procedure GetMsgAreaStats(const MessageInf: MessageRecord;
                          const EleMsgInf : EleMessageRecord;
                          var Active, FirstMsgNum, HighMsgNum: Longint);
procedure SetReplyNr(BoardNr: Word; OriginalMsg, ReplyNr: LongInt);
procedure SilentDeleteMsg(BoardNr: Longint; MessageNr: Longint; CheckAccess: Boolean);
procedure ReplyToMsg(var   EditorInfo: StringArrayObj;
                     var   ReplyLines: Longint;
                     const BoardNr,
                           MsgNr     : Longint;
                     var   FromWho,
                           ToWho,
                           Subj      : String;
                     var   Address,
                           ReplyTo,
                           ReplyAddr,
                           ReplyKludge,
                           DateStr   : String;
                           IsEditting: Boolean);
function  MakeMSGIDKludge(Node: AddrType) : String;
function  MakeMsgId(BoardNr: Word; JamPath: String; Attr, Attr2: Byte): String;
procedure EditMessage(var MsgNr: Longint;
                      const BoardNr     : Word;
                      const ToWho,
                            FromWho,
                            Subject     : String;
                            msgPrivate  : Boolean;
                      var   MsgLines    : Longint;
                      var   KludgeCount : Longint;
                      var   KludgeInfo  : StringArrayObj;
                      var   EditorInfo  : StringArrayObj);
procedure PostMessage(const BoardNr         : Longint;
                      const ToWho,
                            FromWho,
                            Subject         : String;
                      const ReturnReceipt,
                            MsgPrivate,
                            ReplyReceipt,
                            KillSent,
                            CrashMail,
                            AttachMent,
                            MarkAsSent      : Boolean;
                            MsgLines        : Longint;
                      var   EditorInfo      : StringArrayObj;
                      var   MsgNrWritten    : Longint;
                            DestAddress,
                            FromAddress     : String;
                      const UserinterAction,
                            MarkAsRcvd      : Boolean;
                      const DateStr,
                            TimeStr         : String;
                      const ReplyKludge     : String;
                      const MsgIdKludge     : String;
                      var   CachedMsgPtr    : AbsMsgPtr);
procedure NewWriteMsg(var MsgInfo: AbsMsgPtr);
procedure AddOriginLine(var NewMsg: AbsMsgPtr;
                        var MsgInf: MessageRecord);
function GetHighestMsgNum(const MsgInf: MessageRecord): Longint;
function GetLastReadNum(const MsgInf: MessageRecord): Longint;
function HasNewMail(const MsgInf: MessageRecord): Boolean;
procedure DoDeleteMsg(var MsgInfo: AbsMsgPtr);
{$IFDEF MSDOS}
procedure MailDoLongString(var NewMsg: AbsMsgPtr; TextLine: String);
{$ELSE}
procedure MailDoLongString(var NewMsg: AbsMsgPtr; TextLine: AnsiString);
{$ENDIF}

const
  nilAbsMsgPtr   : AbsMsgptr = nil;


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
Uses Dos,
       Support,
        MultiLn,
          MkOpen,
             MkMsgJam,
              FileRout,
               StUtils,
                JDates,
                 Ral,
                  InOut_U,
                   Debug_U,
                 {$IFDEF WITH_FULL}
                     AreaSel,
                 {$ENDIF}
                      MkFile,
                       IntEdit,
                        Access_U,
                           ElLog_U,
                            LineEd,
                             Dispans,
                              Control,
                                BitWise,
                                  Cases,
                                   LongStr,
                                     WordStr,
                                      MemMan,
                                       GenFile,
                                        StrPath,
                                         CentrStr,
                                          MenuFunc,
                                           MailInt,
                                            WriteMsg,
                                             Transfer,
                                              Terminal,
                                               Input_U,
                                                FileObj,
                                                 ObjDec;
{$ELSE}
  uses Debug_U, Jdates, Longstr, Cases, StUtils, Bitwise,
       FileRout, MkOpen, CentrStr, MkMsgJam, StrPath,
       ElLog_U, WordStr, FileObj, Access_U, Terminal,
       WriteMsg, ObjDec;
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function AllowEdit(var Exitinfo  : ExitinfoRecord;
                   var MessageInf: MessageRecord;
                   var EleMsgInf : EleMessageRecord;
                       FromMsg: String): Boolean;
var DoEdit: Boolean;
begin
  DoEdit := true;

  {-- Lets see if we can edit --------------------------------------------}
  if NOT ReadBit(EleMsgInf.Attribute, 3) then DoEdit := false;

  {-- make sure this is not an echomail etc area -------------------------}
  if MessageInf.Typ in [EchoMail, NetMail, Internet] then DoEdit := false;

  {-- make sure this is not a squish area --------------------------------}
  if ReadBit(EleMsgInf.Attribute, 2) then DoEdit := false;

  {-- user is only allowed to edit their own messages --------------------}
  if DoEdit then
    begin
      FromMsg := sUpCase(Trim(FromMsg));

      if (sUpCase(Trim(Exitinfo.Userinfo.Handle)) <> FromMsg) AND
          (sUpCase(Trim(Exitinfo.UserInfo.Name)) <> FromMsg) then DoEdit := false;

      {-- a sysop can edit all messages ----------------------------------}
      if SysopAccess(MessageInf, Exitinfo.Userinfo) then
        DoEdit := true;
    end; { if }

  {-- and return our value -----------------------------------------------}
  AllowEdit := DoEdit;
end; { func. AllowEdit }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function MakeMsgId(BoardNr: Word; JamPath: String; Attr, Attr2: Byte): String;
var Temp      : String;
begin
  If ReadBit(Attr, 7) then Temp:='J'+JamPath
   else begin
          if ReadBit(Attr2, 2) then
            Temp := 'S' + JamPath
              else Temp:='H'+MakeLen(LeadingZero(BoardNr, 3), 3, Zero, True, false) + GlobalCfg^.RaConfig^.MsgBasePath;
        end; { else }

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMailSys, 'MakeMsgId (for messagebase): "'+Temp+'" / Attr='+FStr(Attr));
  {$ENDIF}

  MakeMsgId := Temp;
end; { func. MakeMsgId }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


Function MakeMSGIDKludge(Node : AddrType) : String;
var Temp     : String;
    Temp2    : String;
    Temp3    : Longint;
    DatTmp,
    TimTmp   : String;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailSys, 'Creating Message-ID string (unique message-id)');
  {$ENDIF}

  Temp := ^A + 'MSGID: ';
  Temp2:= AddrStr(Node);
  Temp := Temp + Temp2 + #32;

  DatTmp := JDates.DateStr;
  TimTmp := JDates.TimeStr(True, True);

  While Pos('-', DatTmp)>0 do Delete(DatTmp, Pos('-', DatTmp), 1);
  While Pos('/', DatTmp)>0 do Delete(DatTmp, Pos('/', DatTmp), 1);
  While Pos(':', TimTmp)>0 do Delete(TimTmp, Pos(':', TimTmp), 1);
  While Pos('.', TimTmp)>0 do Delete(TimTmp, Pos('.', TimTmp), 1);

  Randomize;
  Temp3 := Random(255) + FVal(DatTmp) + FVal(TimTmp);
  Temp := Temp + SLowCase(HexLong(Temp3));

  MakeMsgIdkludge := Temp;
end; { func. MakeMsgIdKludge }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditMessage(var MsgNr: Longint;
                      const BoardNr     : Word;
                      const ToWho,
                            FromWho,
                            Subject     : String;
                            msgPrivate  : Boolean;
                      var   MsgLines    : Longint;
                      var   KludgeCount : Longint;
                      var   KludgeInfo  : StringArrayObj;
                      var   EditorInfo  : StringArrayObj);
var NewMsg     : AbsMsgPtr;
    MessageInf : MessageRecord;
    EleMsgInf  : EleMessageRecord;
    Counter    : Longint;
    TmpError   : Word;
    TmpStr     : String;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailSys, 'EditMessage (begin)');
  {$ENDIF}

  {-- get the message board ---------------------------------------------}
  GetMessageRecord(MessageInf, BoardNr, false);
  GetEleMessageRecord(EleMsgInf, BoardNr, false);
  if (BoardNr=00) OR (MessageInf.AreaNum=00) then EXIT;

  {-- open this message area --------------------------------------------}
  if NOT OpenOrCreateMsgArea(NewMsg, MakeMsgId(MessageInf.AreaNum, MessageInf.JamBase, MessageInf.Attribute,
                                               EleMsgInf.Attribute)) then
    begin
      {$IFDEF WITH_DEBUG}
        DebugObj.DebugLog(logMailSys, 'Failed to open message area '+MessageInf.Jambase);
      {$ENDIF}

      EXIT;
    end; { if }

  {-- get the original message ------------------------------------------}
  NewMsg^.SeekFirst(MsgNr);

  {-- get the message ---------------------------------------------------}
  NewMsg^.MsgStartUp;
  NewMsg^.MsgTxtStartUp;

  {-- update the headers ------------------------------------------------}
  NewMsg^.SetTo(ToWho);
  NewMsg^.SetFrom(FromWho);
  NewMsg^.SetSubj(Subject);
  NewMsg^.SetPriv(msgPrivate);

  {-- clear the message text --------------------------------------------}
  NewMsg^.ClearMsgText;

  {-- put all the kludge lines in there ---------------------------------}
  for Counter := 01 to KludgeCount do
    NewMsg^.DoKludgeLn(KludgeInfo.Get(Counter));

  {-- and put the message text ------------------------------------------}
  for Counter := 01 to MsgLines do
    begin
      MailDoLongString(NewMsg, Editorinfo.Get(Counter));
    end; { for }

  {-- add the "message has been editted ---------------------------------}
  {$IFDEF WITH_FULL}
    TmpStr := LangObj^.RalGet(ralHasEdit);
    Replace('%1', LineCfg^.Exitinfo^.Userinfo.Name, TmpStr);
    Replace('%2', DateStr, TmpStr);
    Replace('%3', TimeStr(true, false), TmpStr);
    NewMsg^.DoStringLn(TmpStr);
  {$ENDIF}

  {-- update the message text -------------------------------------------}
  NewMsg^.UpdateMsgText(TmpError);

  {-- update the message header -----------------------------------------}
  NewMsg^.RewriteHdr;

  {-- and close the message area ----------------------------------------}
  CloseMsgArea(NewMsg);


  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailSys, 'EditMessage ( END )');
  {$ENDIF}
end; { proc. EditMessage }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF MSDOS}
procedure MailDoLongString(var NewMsg: AbsMsgPtr; TextLine: String);
{$ELSE}
procedure MailDoLongString(var NewMsg: AbsMsgPtr; TextLine: AnsiString);
{$ENDIF}
var TextCount: Longint;
begin
  if Copy(TextLine, 1, 1) <> #01 then { is this a kludge line? }
   begin
     if Length(TextLine) > 250 then
       begin
         TextCount := 0;

          while TextCount < Length(TextLine) do
           begin
             NewMsg^.DoString(Copy(TextLine, TextCount, 250));
             Inc(TextCount, 250);
           end; { while }

          NewMsg^.DoStringLn('');
         end
          else NewMsg^.DoStringLn(TextLine);
   end
     else NewMsg^.DoKLudgeLn(TextLine);
end; { proc. MailDoLongString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure PostMessage(const BoardNr         : Longint;
                      const ToWho,
                            FromWho,
                            Subject         : String;
                      const ReturnReceipt,
                            MsgPrivate,
                            ReplyReceipt,
                            KillSent,
                            CrashMail,
                            AttachMent,
                            MarkAsSent      : Boolean;
                            MsgLines        : Longint;
                      var   EditorInfo      : StringArrayObj;
                      var   MsgNrWritten    : Longint;
                            DestAddress,
                            FromAddress     : String;
                      const UserinterAction,
                            MarkAsRcvd      : Boolean;
                      const DateStr,
                            TimeStr         : String;
                      const ReplyKludge     : String;
                      const MsgIdKludge     : String;
                      var   CachedMsgPtr    : AbsMsgPtr);
var NewMsg     : AbsMsgPtr;
    MessageInf : MessageRecord;
    EleMsgInf  : EleMessageRecord;
    TempAddr   : NetAddress;
    DestAddr   : AddrType;
    FromAddr   : AddrType;
    Counter    : Longint;
    TypeOfArea : String;
    TempStr    : String;
    SendMsgTo  : Longint;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailSys, 'PostMessage (begin)');
  {$ENDIF}

  GetMessageRecord(MessageInf, BoardNr, false);
  GetEleMessageRecord(EleMsgInf, BoardNr, false);
  if (BoardNr=00) OR (MessageInf.AreaNum=00) then EXIT;

  {-- if no cached copy is provided, open the area ourself, else use -----}
  {-- the cached copy ----------------------------------------------------}
  if CachedMsgPtr = nil then
    begin
      if NOT OpenOrCreateMsgArea(NewMsg, MakeMsgId(MessageInf.AreaNum, MessageInf.JamBase, MessageInf.Attribute,
                                                   EleMsgInf.Attribute)) then
        begin
          {$IFDEF WITH_DEBUG}
            DebugObj.DebugLog(logMailSys, 'Failed to open message area '+MessageInf.Jambase);
          {$ENDIF}

          EXIT;
        end; { if }
    end else begin
               {$IFDEF WITH_DEBUG}
                 DebugObj.DebugLog(logMailSys, 'CachedMsgPtr is entered, using that');
               {$ENDIF}

               {-- and assign the used info ------------------------------}
               NewMsg := CachedMsgPtr;
             end; { else }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailSys, 'PostMessage - Messagebase opened');
  {$ENDIF}

  {-- Find out what sort of message -----------------------------------------}
  Case MessageInf.typ of
     LocalMail : NewMsg^.SetMailType(mmtNormal);
     NetMail   : begin
                   NewMsg^.SetMailType(mmtNetMail);
                   NewMsg^.SetNetMail(true);

                   LineCfg^.Exitinfo^.NetMailEntered := TRUE;
                 end; { Set NetMail }
     EchoMail  : begin
                   NewMsg^.SetMailType(mmtEchoMail);
                   LineCfg^.Exitinfo^.EchoMailEntered := TRUE;
                 end; { if }
     InterNet  : begin
                   NewMsg^.SetMailType(mmtEchoMail);
                 end; { Internet }
     NewsGroup : begin
                   NewMsg^.SetMailType(mmtEchoMail);
                 end; { Newsgroup }
     ForumGroup: NewMsg^.SetMailType(mmtNormal);
  end; { case }

  {-- log some info ---------------------------------------------------------}
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailSys, 'PostMessage - Set type of msgbase');
  {$ENDIF}

  {-- Setup the address information -----------------------------------------}
  GetAddress(MessageInf.AkaAddress, TempAddr);
  FromAddr.Zone := TempAddr.Zone;
  FromAddr.Net  := TempAddr.Net;
  FromAddr.Node := TempAddr.Node;
  FromAddr.Point:= TempAddr.Point;

  if (MessageInf.Typ in [netMail]) AND (DestAddress='') then
    begin
      DestAddr := FromAddr;
    end; { if }

  if (MessageInf.Typ in [NetMail]) AND (DestAddress <> '') then
       StringToAddr(DestAddress, DestAddr.Zone, DestAddr.Net, DestAddr.Node, DestAddr.Point);
  if (MessageInf.Typ in [NetMail]) AND (FromAddress <> '') then
       StringToAddr(FromAddress, FromAddr.Zone, FromAddr.Net, FromAddr.Node, FromAddr.Point);

  {-- Actually start writing the message ------------------------------------}
  NewMsg^.StartNewMsg;
  NewMsg^.SetOrig(FromAddr);
  NewMsg^.SetDest(DestAddr);
  NewMsg^.SetFrom(FromWho);
  NewMsg^.SetTo  (ToWho);
  NewMsg^.SetSubj(Subject);
  NewMsg^.SetDate(DateStr);
  NewMsg^.SetTime(TimeStr);

  NewMsg^.SetLocal(TRUE);
  NewMsg^.SetEcho (TRUE);
  NewMsg^.SetNetMail(Messageinf.Typ in [NetMail]);

  if MessageInf.Typ in [Internet] then
    NewMsg^.SetKillSent(TRUE)
      else NewMsg^.SetKillSent(KillSent);

  NewMsg^.SetCrash(CrashMail);
  NewMsg^.SetReqRct(ReturnReceipt);
  NewMsg^.SetRetRct(ReplyReceipt);
  NewMsg^.SetFAttach(AttachMent);
  if MarkAsRcvd then
    NewMsg^.SetRcvd(true);

  if MarkAsSent then
    NewMsg^.SetSent(true);

  if MsgPrivate then
      NewMsg^.SetPriv(True);

  {-- Add a PID line using EleBBS info --------------------------------------}
  TempStr := '';
  if RunsRegistered then
    begin
      {$IFDEF WITH_FULL}
        TempStr := #32 {+ FStr(GenInfo^.CreateNr)};
      {$ENDIF}
    end; { if }
  NewMsg^.DoKludgeLn(^A+'PID: '+PIDName + RegAddStrShort + TempStr);

  if MsgIdKludge = '' then
    NewMsg^.DoKludgeLn(MakeMsgIDKludge(FromAddr))
      else NewMsg^.DoKludgeLn('MSGID: ' + MsgIdKludge);

  if ReplyKludge <> '' then
    NewMsg^.DoKludgeLn(^A + 'REPLY: ' + ReplyKludge);

  {-- log some info ---------------------------------------------------------}
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailSys, 'PostMessage - Before adding message info');
  {$ENDIF}

  {-- Now just write the message information --------------------------------}
  for Counter := 01 to MsgLines do
    begin
      MailDoLongString(NewMsg, Editorinfo.Get(Counter));
    end; { if }

  {-- log some info ---------------------------------------------------------}
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailSys, 'PostMessage - Before addoriginline');
  {$ENDIF}

  AddOriginLine(NewMsg, MessageInf);

  {-- log some info ---------------------------------------------------------}
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailSys, 'PostMessage - Comitting the msg');
  {$ENDIF}

  NewWriteMsg(NewMsg);
  MsgNrWritten := NewMsg^.GetMsgNum;

  {-- log some info ---------------------------------------------------------}
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailSys, 'PostMessage - Msg written, lets chat with user');
  {$ENDIF}

  if UserInteraction then
    begin
      {$IFDEF WITH_FULL}
        GetAreaTypeString(TypeOfArea, MessageInf);
        RaLog('>', 'Message #'+FStr(MsgNrWritten)+' posted in '+TypeOfArea+' area #'+FStr(MessageInf.AreaNum)+' : '+
              MessageInf.Name);
      {$ENDIF}
    end; { if }

  {-- log some info ---------------------------------------------------------}
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailSys, 'PostMessage - Close area');
  {$ENDIF}

  if CachedMsgPtr = nil then
    CloseMsgArea(NewMsg);

  {-- log some info ---------------------------------------------------------}
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailSys, 'PostMessage - area closed, lets chat some more ');
  {$ENDIF}

  {-- Check to see wether user is online, if he/she is, post OLM ------------}
  if UserInteraction then
   if Trim(SUpCase(ToWho)) <> Trim(SUpCase(LineCfg^.Exitinfo^.UserInfo.Name)) then
     begin
       {$IFDEF WITH_FULL}
         SendMsgTo := MultiLnObj^.GetUserOn(ToWho, false, false);

         if SendMsgTo <> -1 then
           begin
             With GlobalCfg^.RaConfig^, LangObj^ do
               MultiLnObj^.SendInteractiveMsg(SendMsgTo, FromWho + #32 +
                   LeftBracket + ralGet(ralOnNode) + #32 + FStr(LineCfg^.RaNodeNr) +
                   RightBracket + #32 +
                   ralGet(ralMsgPosted) + #13#10 +
                   '"' + MessageInf.Name  + '"');
           end; { if user is actually online }
       {$ENDIF}
    end; { if }

  {-- log some info ---------------------------------------------------------}
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailSys, 'PostMessage ( END )');
  {$ENDIF}
end; { proc. PostMessage }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AddOriginLine(var NewMsg: AbsMsgPtr;
                        var MsgInf: MessageRecord);
var AddrStr    : String;
    OriginLine : String;
    TempAddr   : NetAddress;
begin
  if (ReadBit(MsgInf.Attribute, 0)) AND (MsgInf.Typ in [EchoMail]) then
    begin
       GetAddress(MsgInf.AkaAddress, TempAddr);
       With TempAddr do
         AddrToString(AddrStr, Zone, Net, Node, Point);

       OriginLine := MsgInf.OriginLine;
       if OriginLine = '' then
         OriginLine := GlobalCfg^.RaConfig^.OriginLine;


       NewMsg^.DoStringLn('---');
       NewMsg^.DoStringLn(' * Origin: '+Copy(OriginLine, 1, 79 - Length(AddrStr) - 12)+' ('+AddrStr+')');
    end; { ReadBit }
end; { proc. AddOriginLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ReplyToMsg(var   EditorInfo: StringArrayObj;
                     var   ReplyLines: Longint;
                     const BoardNr,
                           MsgNr     : Longint;
                     var   FromWho,
                           ToWho,
                           Subj      : String;
                     var   Address,
                           ReplyTo,
                           ReplyAddr,
                           ReplyKludge,
                           DateStr   : String;
                           IsEditting: Boolean);
var MsgUpdate  : AbsMsgPtr;
    TempAddr   : AddrType;
    MessageInf : MessageRecord;
    EleMsgInf  : EleMessageRecord;
    TempStr    : String;

function ReplyHdrLen: Byte;
var Temp: String;
begin
  Temp := GlobalCfg^.RaConfig^.QuoteString;

  Replace('@', SupCase(InitialString(FromWho)), Temp);
  Replace('#', SLowCase(InitialString(FromWho)), Temp);

  ReplyHdrLen := Length(Temp);
end; { func. ReplyHdrLen }

begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMailSys, 'ReplyToMsg');
  {$ENDIF}

  EditorInfo.Clear;
  GetMessageRecord(MessageInf, BoardNr, False);
  GetEleMessageRecord(EleMsgInf, BoardNr, False);

  If OpenOrCreateMsgArea(MsgUpdate, MakeMsgId(MessageInf.AreaNum, MessageInf.JamBase, MessageInf.Attribute,
                                               EleMsgInf.Attribute)) then
   begin
     FromWho    := '';
     ToWho      := '';
     Subj       := '';
     ReplyTo    := '';
     ReplyAddr  := '';
     DateStr    := '';
     ReplyKludge:= '';
     ReplyLines := 00;

     if NOT IsEditting then
       EditorInfo.Put(1, '');

     MsgUpdate^.SeekFirst(MsgNr);
     if MsgUpdate^.SeekFound then
      begin
        MsgUpdate^.MsgStartUp;                    { Initialize messageheader }
        MsgUpdate^.MsgTxtStartUp;                    { Initialize messagetxt }

        FromWho := MsgUpdate^.GetFrom;
        ToWho   := MsgUpdate^.GetTo;
        Subj    := MsgUpdate^.GetSubj;
        DateStr := MsgUpdate^.GetDate + #32 + MsgUpdate^.GetTime;
        MsgUpdate^.GetOrig(TempAddr);


        if MsgUpdate^.IsFAttach then
          Subj := '';

        Address := '';
        AddrToString(Address, TempAddr.Zone, TempAddr.Net, TempAddr.Node, TempAddr.Point);

        While NOT MsgUpdate^.EOM do
         begin
          TempStr[1] := #00;     { Make sure not mistakenly seen as kludge }
          TempStr := '';

          TempStr := MsgUpDate^.GetString(78 - ReplyHdrLen);

          if TempStr[1] = #01 then
           if SUpCase(Copy(TempStr, 2, Length('REPLYTO'))) = 'REPLYTO' then
             begin
               ReplyTo := Copy(TempStr, Length('REPLYTO') + 02, 255);
               ReplyTo := Trim(ReplyTo);
             end; { if }

          if TempStr[1] = #01 then
           if SUpCase(Copy(TempStr, 2, Length('REPLYADDR'))) = 'REPLYADDR' then
             begin
               ReplyAddr := Copy(TempStr, Length('REPLYADDR') + 02, 255);
               ReplyAddr := Trim(ReplyAddr);
             end; { if }

         if TempStr[1] = #01 then
          if SUpCase(Copy(TempStr, 2, Length('MSGID:'))) = 'MSGID:' then
            begin
              ReplyKludge := Copy(TempStr, Length(^A'MSGID:') + 2, 255);
            end; { if }

          if Address = '0:0/0' then
           if TempStr[1] = #01 then
            if SUpCase(Copy(TempStr, 2, Length('MSGID:'))) = 'MSGID:' then
              begin
                Address := Copy(TempStr, Length('MSGID:') + 02, 255);
                Address := Trim(Address);

                if Pos('@', Address) > 00 then
                  Delete(Address, Pos('@', Address), 255);

                if Pos(#32, Address) > 00 then
                  Delete(Address, Pos(#32, Address), 255);
              end; { if }

           if (TempStr[1] <> #01) OR (IsEditting) then
              begin
                Inc(ReplyLines);
                EditorInfo.Put(ReplyLines, TempStr);

                if ReplyLines > MaxReplyLines then BREAK;
              end; { if TempStr }
         end; { while }
      end; { if }

     CloseMsgArea(MsgUpdate);                               { Close this area }
   end; { OpenMsgArea }
end; { proc. ReplyToMsg }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


Procedure NewWriteMsg(var MsgInfo: AbsMsgPtr);
{ As specificied in the locking techniques "RALCK003.DOC", RemoteAccess tries }
{ for 15 seconds to lock the messagebase, if this will not succeed, we give }
{ the user the chance to decide to retry, or to dump the message }
var Temp    : Longint;
    Counter : Longint;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailSys, 'NewWriteMsg (begin)');
  {$ENDIF}

  Counter := 10;
  REPEAT
    BasePath := ForceBack(GlobalCfg^.RaConfig^.MsgBasePath);
    Temp := MsgInfo^.WriteMsg;

    {$IFDEF WITH_DEBUG}
      if Temp <> 00 then
        RaLog('!', 'Locking violation error, asking user (#'+FStr(Temp)+')');
    {$ENDIF}

    If (Temp=5) then                                { Locking violation error }
      begin
       {$IFDEF WITH_FULL}
         If NOT InputObj^.ralStrYesNoAsk(ralMsgInUse) then
            Temp := 00;                                          { Not retrying }
       {$ENDIF}
      end; { Locking violation error }

    Dec(Counter);                      { You can maximally retry for 10 times }
    if Counter <= 0 then Temp := 0;

  UNTIL (Temp=00);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailSys, 'NewWriteMsg ( end )');
  {$ENDIF}
end; { proc. NewWriteMsg }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SetReplyNr(BoardNr: Word; OriginalMsg, ReplyNr: LongInt);
var MsgUpdate : AbsMsgPtr;
    MessageInf: MessageRecord;
    EleMsgInf : EleMessageRecord;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailSys, 'SetReplyNr');
  {$ENDIF}

  GetMessageRecord(MessageInf, BoardNr, false);   { Get message-record for [boardnr] }
  GetEleMessageRecord(EleMsgInf, BoardNr, false);

  if (BoardNr>00) AND (ReplyNr>00) AND (OriginalMsg>00) then
   if OpenOrCreateMsgArea(MsgUpdate, MakeMsgId(MessageInf.AreaNum, MessageInf.JamBase, MessageInf.Attribute,
                                                 EleMsgInf.Attribute)) then
    begin
      {$IFDEF WITH_DEBUG}
        DebugObj.DebugLog(logMailSys, 'Area opened');
      {$ENDIF}

      MsgUpdate^.SeekFirst(ReplyNr);
      if MsgUpdate^.SeekFound then
        begin;
          {$IFDEF WITH_DEBUG}
            DebugObj.DebugLog(logMailSys, 'Message found');
          {$ENDIF}
          MsgUpdate^.MsgStartUp;                        { Initialize messageheader }
          MsgUpdate^.SetRefer(OriginalMsg);            { This is a reply to msg-nr }
          MsgUpdate^.RewriteHdr;                                   { Update header }

          MsgUpdate^.SeekFirst(OriginalMsg);
          if MsgUpdate^.SeekFound then
            begin
              MsgUpdate^.MsgStartUp;                        { Initialize messageheader }

              {-- If the Reply1st field (JAM) is not set, this is the first --}
              {-- reply, and we set it to our mesage -------------------------}
              if MsgUpdate^.GetSeeAlso = 0 then
                MsgUpdate^.SetSeeAlso(ReplyNr) else   { the first reply is at }
                  begin
                    {-- if this it not the first reply, we seek out to the ---}
                    {-- first reply, and update its Reply2nd value -----------}
                    MsgUpdate^.SeekFirst(MsgUpdate^.GetSeeAlso);
                    MsgUpdate^.MsgStartUp;         { Initialize messageheader }

                    if MsgUpdate^.GetNextSeeAlso = 0 then
                      MsgUpdate^.SetNextSeeAlso(ReplyNr)
                       else begin
                              {-- now if theres also has been set the 2nd ----}
                              {-- reply, we have to walk down the tree -------}
                              While (MsgUpdate^.SeekFound) AND
                                     (MsgUpDate^.GetNextSeeAlso <> 0) do
                                begin
                                  MsgUpdate^.SeekFirst(MsgUpdate^.GetNextSeeAlso);
                                  MsgUpdate^.MsgStartUp;
                                end; { while }

                              {-- now set the reply chain --------------------}
                              MsgUpdate^.SetNextSeeAlso(ReplyNr);
                            end; { if}
                  end; { if }

              MsgUpdate^.RewriteHdr;                                   { Update header }
            end; { if }
        end; { if }

      CloseMsgArea(MsgUpdate);                               { Close this area }
      {$IFDEF WITH_DEBUG}
        DebugObj.DebugLog(logMailSys, 'OpenMsgArea end');
      {$ENDIF}
    end; { OpenMsgArea }
end; { proc. SetreplyNr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetHighMsgNum(MessageInf: MessageRecord;
                       EleMsgInf : EleMessageRecord): Longint;
var MsgInfo   : AbsMsgPtr;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailSys, 'GetHighMsgNum: '+FStr(MessageInf.AreaNum));
  {$ENDIF}

  GetHighMsgNum := 00;
  if MessageInf.AreaNum = 00 then EXIT;
  If NOT OpenOrCreateMsgArea(MsgInfo, MakeMsgId(MessageInf.AreaNum, MessageInf.JamBase,
                 MessageInf.Attribute, EleMsgInf.Attribute)) then Exit;

  GetHighMsgNum := MsgInfo^.GetHighMsgNum;

  CloseMsgArea(MsgInfo);
end; { func. GetHighMsgNum }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetAreaAddress(BoardNr: Word): String;
var MessageInf: MessageRecord;
    Address   : NetAddress;
begin
  GetMessageRecord(MessageInf, BoardNr, false);
  GetAddress(MessageInf.AkaAddress, Address);

  With Address do
    GetAreaAddress := FStr(Zone) + ':' +
                      FStr(Net) + '/' +
                      FStr(Node) + '.' +
                      FStr(Point);
end; { func. GetAreaAddress }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FilePost(const AreaNr   : Longint;
                   const FromWho,
                         ToWho,
                         Subject  : String;
                   const FileName,
                         AddText  : String);
var MsgNr     : LongInt;
    MsgLines  : LongInt;
    EditorInfo: pStringArrayObj;
    TempF     : pFileObj;
    TempStr   : String;
begin
  New(EditorInfo, Init(MaxMsgLines));
  MsgLines := 0;

  {-- Open the file to post -------------------------------------------------}
  New(TempF, Init);
  TempF^.Assign(OpenRaFile(FileName));
  TempF^.Filemode := ReadMode + DenyNone;
  if NOT TempF^.Open(1) then
    begin
      Dispose(TempF, Done);
      Dispose(EditorInfo, Done);

      EXIT;
    end; { if }

  {-- Check to see if we need some sort of header to post as well -----------}
  if AddText <> '' then
    begin
      Inc(MsgLines);
      EditorInfo^.Put(MsgLines, AddText);
    end; { if }

  {-- Run the loop ----------------------------------------------------------}
  While NOT TempF^.EOF do
    begin
      Inc(MsgLines);

      TempF^.ReadLn(TempStr);

      termObj^.RaCodeStr(TempStr);
      EditorInfo^.Put(MsgLines, TempStr);
    end; { while }

  Dispose(TempF, Done);

  {-- Now actually post this message ----------------------------------------}
  PostMessage(AreaNr,
              ToWho,
              FromWho,
              Subject,
              False,                                    { Return receipt }
              True,                                            { Private }
              False,                                     { Reply Receipt }
              False,                                         { Kill/Sent }
              False,                                         { CrashMail }
              False,                                        { Attachment }
              false,                                           { Is Sent }
              MsgLines,                        { Number of lines to post }
              EditorInfo^,                         { Actual text to post }
              MsgNr,                    { Message number that is written }
              '',                                  { Destination address }
              '',                                  { Originating address }
              true,                    { We want/allow user interaction? }
              false,                         { Mark message as received? }
              DateStr,                               { Date msg created? }
              TimeStr(true, false),                  { Time msg created? }
              '',
              '',
              nilAbsMsgPtr);

  Dispose(EditorInfo, Done);
end; { proc. FilePost }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure MenuPost(MiscData: String);
var AreaNr   : String;
    ToName   : String;
    Address  : String;
    Subject  : String;
    QuoteText: pStringArrayObj;
    MsgNr    : LongInt;
begin
  {-- Initialize all variables we need --------------------------------------}
  New(QuoteText, Init(MaxMsgLines));
  AreaNr := FirstWord(MiscData, defExtractWord, false);

  if SupCase(AreaNr) = '/M' then
    AreaNr := FStr(LineCfg^.Exitinfo^.Userinfo.MsgArea);

  {-- Make sure the area we''re posting to exists ---------------------------}
  if fVal(AreaNr) = 0 then
    begin
      {$IFDEF WITH_FULL}
        ChangeMessageArea(MiscData);
      {$ENDIF}
      AreaNr := FStr(LineCfg^.Exitinfo^.Userinfo.MsgArea);

      if fVal(AreaNr) = 0 then EXIT;
    end; { if FVal }

  AreaNr := Fstr(Ra250MsgArea(FVal(AreaNr)));
  ToName := Under2Norm(GetValue('/T=', MiscData, True));
  Address := Under2Norm(GetValue('/N=', MiscData, True));
  Subject := Under2Norm(GetValue('/S=', MiscData, true));

  {-- Now actually post the message -----------------------------------------}
  {$IFDEF WITH_FULL}
  WriteMessage(FVal(AreaNr),
               ToName,
               LineCfg^.Exitinfo^.Userinfo.Name,
               Subject,
               MiscData,
               False,   { reply }
               false,   { edit }
               QuoteText^,
               00,
               MsgNr,
               False,
               True,
               '',
               GetAreaAddress(FVal(AreaNr)),
               Address,
               '', '', false, true, '', '', '');
  {$ENDIF}

  {-- Clean up ... ---------------------------------------------------------}
  {$IFDEF WITH_FULL}
    if Pos('/L', sUpCase(MiscData)) > 00 then
      TerminateCall(TRUE);
  {$ENDIF}

  Dispose(QuoteText, Done);
end; { proc. MenuPost }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function InMarkList(BoardNr, MsgNr: Word): Boolean;
var Counter: Byte;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailSys, 'InMarkList');
  {$ENDIF}

  InMarkList := false;

  If LineCfg^.CurMsgsMarked>00 then
   For Counter := 01 to LineCfg^.CurMsgsMarked do
    If (LineCfg^.MarkedMsgArray^[Counter].BoardNr=BoardNr) AND
        (LineCfg^.MarkedMsgArray^[Counter].RecNr=MsgNr) then
         begin
           InMarkList := True;
           Break;
         end; { found }
end; { func. InMarkList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AddToMarkList(BoardNr, MsgNr: Word);
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailSys, 'AddToMarkList');
  {$ENDIF}

  If NOT InMarkList(BoardNr, MsgNr) then
   If LineCfg^.CurMsgsMarked<100 then
    begin
      Inc(LineCfg^.CurMsgsMarked);
      LineCfg^.MarkedMsgArray^[LineCfg^.CurMsgsMarked].BoardNr := BoardNr;
      LineCfg^.MarkedMsgArray^[LineCfg^.CurMsgsMarked].RecNr := MsgNr;
    end; { InMarkList }

end; { proc. AddToMarkList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetActiveMsgNum(MessageInf: MessageRecord;
                         EleMsgInf : EleMessageRecord): Longint;
var MsgInfo   : AbsMsgPtr;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailSys, 'GetActiveMsgNum, BoardNr='+FStr(MessageInf.AreaNum));
  {$ENDIF}

  GetActiveMsgNum := 00;
  if MessageInf.AreaNum = 00 then EXIT;

  If NOT OpenOrCreateMsgArea(MsgInfo, MakeMsgId(MessageInf.AreaNum, MessageInf.JamBase, MessageInf.Attribute,
                                       EleMsgInf.Attribute)) then EXIT;

  GetActiveMsgNum := MsgInfo^.NumberOfMsgs;

  CloseMsgArea(MsgInfo);
end; { func. GetActiveMsgNum }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function SearchNetMailBoard: Word;
var Msg_F       : pFileObj;
    MessageInf  : MessageRecord;
    ItemCounter : Longint;
    TotalItems  : Longint;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailSys, 'Search netmail board');
  {$ENDIF}

  SearchNetMailBoard := 00;

  New(Msg_F, Init);
  Msg_F^.Assign(MessagesFileName);
  Msg_F^.FileMode := ReadMode + DenyNone;
  if Msg_F^.Open(1) then
    begin
      ItemCounter := 00;
      TotalItems := Msg_F^.FileSize div SizeOf(MessageRecord);

      While (ItemCounter < TotalItems) do
        begin;
          FillChar(MessageInf, SizeOf(MessageInf), #00);
          Msg_F^.BlkRead(MessageInf, SizeOf(MessageRecord));
          Inc(ItemCounter);

          If MessageInf.Typ in [NetMail] then
            begin
              SearchNetMailBoard := MessageInf.AreaNum;
              Break;
            end; { While }
        end; { While }
    end; { if }

  Dispose(Msg_F, Done);
end; { func. SearchNetMailBoard }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GetMsgAreaStats(const MessageInf: MessageRecord;
                          const EleMsgInf : EleMessageRecord;
                          var Active, FirstMsgNum, HighMsgNum: Longint);
var MsgInfo   : AbsMsgPtr;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailSys, 'GetMsgAreaStats: '+FStr(MessageInf.AreaNum));
  {$ENDIF}

  Active      := 00;
  FirstMsgNum := 00;
  HighMsgNum  := 00;
  if MessageInf.AreaNum = 00 then EXIT;

  If NOT OpenOrCreateMsgArea(MsgInfo, MakeMsgId(MessageInf.AreaNum, MessageInf.JamBase, MessageInf.Attribute,
                           EleMsgInf.Attribute)) then Exit;

  Active := MsgInfo^.NumberOfMsgs;

  MsgInfo^.SeekFirst(01);
  if MsgInfo^.SeekFound then
    begin
      MsgInfo^.MsgStartUp;

      FirstMsgNum := MsgInfo^.GetMsgNum;

      {-- Most of the time, highmsgnum for Hudson is incorrect, so we just -}
      {-- make sure we dont skip any messages ------------------------------}
      HighMsgNum := GetHighMsgNum(MessageInf, EleMsgInf);
    end; { if }

  CloseMsgArea(MsgInfo);
end; { proc. GetMsgAreaStats }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SetLastRead(MessageInf: MessageRecord;
                      EleMsgInf : EleMessageRecord;
                      MsgNr: Longint);
var MsgInfo   : AbsMsgPtr;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailSys, 'SetLastRead - (BEGIN)');
    DebugObj.DebugLog(logMailSys, 'SetLastRead: '+FStr(MessageInf.AreaNum));
    DebugObj.DebugLog(logMailSys, 'SetLastRead - MsgNr = ' + FStr(MsgNr));
  {$ENDIF}

  if MessageInf.AreaNum = 00 then EXIT;
  If NOT OpenOrCreateMsgArea(MsgInfo, MakeMsgId(MessageInf.AreaNum, MessageInf.JamBase, MessageInf.Attribute,
                                                EleMsgInf.Attribute)) then Exit;

  if MsgInfo^.GetLastRead(LineCfg^.Exitinfo^.UserRecord, JamStrCrc(LineCfg^.Exitinfo^.Userinfo.Name)) < MsgNr then
    MsgInfo^.SetLastRead(LineCfg^.Exitinfo^.UserRecord, MsgNr, JamStrCrc(LineCfg^.Exitinfo^.Userinfo.Name));

  if (NOT ReadBit(MessageInf.Attribute, 7)) then
   if MsgNr > LineCfg^.Exitinfo^.Userinfo.LastRead then
      LineCfg^.Exitinfo^.Userinfo.LastRead := MsgNr;  { Update lastread pointer }

  CloseMsgArea(MsgInfo);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailSys, 'SetLastRead - (END)');
  {$ENDIF}
end; { proc. SetLastRead }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetHighestHudson(BoardNum: Longint): Longint;
const
  HMBBufSize = 24;

type
  HMBHighArrayType = Array[0..(HMBBufSize - 01)] of HMBMsgHdrRecord;

var HmbBuf     : HMBHighArrayType;      { Contains the info for 'x' records }
    Temp_F     : pFileObj;                  { Filehandle for this indexfile }
    DidFound   : Boolean;
    Counter    : Longint;
    NumRead    : NumReadType;
    ReachedBottom: Boolean;
begin
  {-- Sanity check ----------------------------------------------------------}
  if BoardNum > 200 then
    begin
      GetHighestHudson := 0;
      EXIT;
    end; { if }

  {-- Open the index files --------------------------------------------------}
  New(Temp_F, Init);
  Temp_F^.Assign(GlobalCfg^.RaConfig^.MsgBasePath + 'msghdr.bbs');
  Temp_F^.FileMode := ReadMode + DenyNone;

  if NOT Temp_F^.Open(SizeOf(HMBMsgHdrRecord)) then
    begin
      Dispose(Temp_F, Done);
      EXIT;
    end; { if }

  {-- Seek to the last record -----------------------------------------------}
  Temp_F^.Seek(Temp_F^.FileSize - HMBBufSize);

  {-- Now loop backwards till a non-deleted, message is found ---------------}
  DidFound := FALSE;
  GetHighestHudson := 0;
  NumRead := 1;

  While (NumRead > 0) AND (NOT DidFound) do
    begin
      {-- Read the information ----------------------------------------------}
      NumRead := Temp_F^.BlkRead(HMBBuf, HMBBufSize);

      {-- loop through all records we've read -------------------------------}
      if NumRead > 0 then
        Counter := NumRead - 1
          else Counter := -1;

      while (Counter >= 0) AND (NOT DidFound) do
        begin
          with HmbBuf[Counter] do
            if (Board = BoardNum) AND (MsgNum >= 0) then
              begin
                GetHighestHudson := HmbBuf[Counter].MsgNum;
                DidFound := TRUE;
              end; { for }

          Dec(Counter);
        end; { for }

      {-- if not found, go back one record to read (2-to-seek) --------------}
      Temp_F^.Seek(Temp_F^.FilePos - (HmbBufSize * 2));

      {-- make sure we dont keep on reading ---------------------------------}
      if ReachedBottom then
        NumRead := 0;

      if Temp_F^.FilePos <= 0 then
        ReachedBottom := TRUE;
    end; { while }

  {-- Dispose all allocated memory ------------------------------------------}
  Dispose(Temp_F, Done);
end; { func. GetHighestHudson }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetHighestJAM(const MsgInf: MessageRecord): Longint;

type
  JamIDXrecord = record                        { Quick index to these files }
     MsgToCrc : Longint;
     HdrLoc   : Longint;
  end; { JamIdxType record }

type
  JamMsgHdrRecord = record
     Signature  : Array[1..4] of Char;
     Rev        : SmallWord;
     Resvd      : SmallWord;
     SubFieldLen: LongInt;
     TimesRead  : LongInt;
     MsgIdCrc   : LongInt;
     ReplyCrc   : LongInt;
     ReplyTo    : LongInt;
     ReplyFirst : LongInt;
     ReplyNext  : LongInt;
     DateWritten: LongInt;
     DateRcvd   : LongInt;
     DateArrived: LongInt;
     MsgNum     : LongInt;
     Attr1      : LongInt;
     Attr2      : LongInt;
     TextOfs    : LongInt;
     TextLen    : LongInt;
     PwdCrc     : LongInt;
     Cost       : LongInt;
  end; { JamMsgHdrType }

const
  JAMBufSize  = 8;
  Jam_Deleted = $80000000;

type
  JamIdxArrayType = Array[0..(JamIdxBufSize - 01)] of JamIdxRecord;

var JamIdx     : JamIdxArrayType;     {  Contains all CRC's for 512 records }
    JamMsgHdr  : JamMsgHdrRecord;
    TempIDX_F  : pFileObj;                  { Filehandle for this indexfile }
    TempHDR_F  : pFileObj;
    DidFound   : Boolean;
    Counter    : Longint;
    NumRead    : NumReadType;
    ReachedBottom: Boolean;
begin
  {-- Open the index files --------------------------------------------------}
  New(TempIDX_F, Init);
  TempIDX_F^.Assign(MsgInf.JamBase + '.jdx');
  TempIDX_F^.FileMode := ReadMode + DenyNone;

  if NOT TempIDX_F^.Open(SizeOf(JamIdxRecord)) then
    begin
      Dispose(TempIDX_F, Done);
      EXIT;
    end; { if }

  {-- Open the header file --------------------------------------------------}
  New(TempHDR_F, Init);
  TempHDR_F^.Assign(MsgInf.JamBase + '.jhr');
  TempHDR_F^.FileMode := ReadMode + DenyNone;

  if NOT TempHDR_F^.Open(1) then
    begin
      Dispose(TempIDX_F, Done);
      Dispose(TempHDR_F, Done);
      EXIT;
    end; { if }

  {-- Seek to the last record -----------------------------------------------}
  TempIDX_F^.Seek(TempIDX_F^.FileSize - JAMBufSize);
  if TempIDX_F^.FilePos < 0 then
    TempIDX_F^.Seek(0);

  {-- Now loop backwards till a non-deleted, message is found ---------------}
  DidFound := FALSE;
  GetHighestJAM := 0;
  NumRead := 1;

  While (NumRead > 0) AND (NOT DidFound) do
    begin
      {-- Read the information ----------------------------------------------}
      NumRead := TempIDX_F^.BlkRead(JamIDX, JAMBufSize);

      {-- loop through all records we've read -------------------------------}
      if NumRead > 0 then
        Counter := NumRead - 1
          else Counter := -1;

      while (Counter >= 0) AND (NOT DidFound) do
        begin
          with JamIDX[Counter] do
            begin
              TempHDR_F^.Seek(HdrLoc);
              NumRead := TempHDR_F^.BlkRead(JamMsgHdr, SizeOf(JamMsgHdrRecord));

              With JamMsgHdr do
                if (MsgNum >= 0) then
                  if ((Attr1 and JAM_Deleted) = 0) then
                    begin
                      GetHighestJAM := JamMsgHdr.MsgNum;
                      DidFound := TRUE;
                    end; { for }
            end; { with }

          Dec(Counter);
        end; { for }

      {-- if not found, go back one record to read (2-to-seek) --------------}
      TempIDX_F^.Seek(TempIDX_F^.FilePos - (JAMBufSize * 2));
      if TempIDX_F^.FilePos < 0 then
        TempIDX_F^.Seek(0);

      {-- make sure we dont keep on reading ---------------------------------}
      if ReachedBottom then
        NumRead := 0;

      if TempIDX_F^.FilePos <= 0 then
        ReachedBottom := TRUE;
    end; { while }

  {-- Dispose all allocated memory ------------------------------------------}
  Dispose(TempIDX_F, Done);
  Dispose(TempHDR_F, Done);
end; { func. GetHighestJAM }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetLastReadHudson(BoardNum: Word): Longint;
var Temp_F     : pFileObj;                  { Filehandle for this indexfile }
    TmpLastRead: LastReadRecord;
begin
  {-- Sanity check ----------------------------------------------------------}
  if (BoardNum > 200) OR (BoardNum < 1) then
    begin
      GetLastReadHudson := 0;
      EXIT;
    end; { if }

  {-- Open the index files --------------------------------------------------}
  New(Temp_F, Init);
  Temp_F^.Assign(GlobalCfg^.RaConfig^.MsgBasePath + 'lastread.bbs');
  Temp_F^.FileMode := ReadMode + DenyNone;

  if NOT Temp_F^.Open(SizeOf(LastReadRecord)) then
    begin
      Dispose(Temp_F, Done);
      EXIT;
    end; { if }

  {-- Search to the correct position ----------------------------------------}
  Temp_F^.Seek(LineCfg^.Exitinfo^.UserRecord);

  {-- Read the information --------------------------------------------------}
  Temp_F^.BlkRead(TmpLastRead, 1);

  {-- and return the information --------------------------------------------}
  if Temp_F^.IoResult > 0 then
    begin
      FillChar(TmpLastRead, SizeOf(TmpLastRead), #0);
    end; { if }

  GetLastReadHudson := TmpLastRead[BoardNum];

  {-- Dispose all allocated memory ------------------------------------------}
  Dispose(Temp_F, Done);
end; { func. GetLastReadHudson }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetLastReadJAM(const MsgInf: MessageRecord): Longint;
const
  JamLastBufSize = 32;

type
  JamLastType = record                         { Quick index to these files }
     NameCrc : LongInt;
     UserNum : LongInt;
     LastRead: LongInt;
     HighRead: LongInt;
  end; { JamLastType record }

type
  JamLastArrayType = Array[0..(JamLastBufSize - 01)] of JamLastType;

var JamLast      : JamLastArrayType;   { Contains all CRC's for 512 records }
    ThisNameCRC  : Longint;                          { CRC32 for users name }
    ThisHandleCRC: Longint;                        { CRC32 for users handle }
    NumRead      : NumReadType;                { How many records were read }
    AbortRead    : Boolean;                           { We found one? Abort }
    Temp_F       : pFileObj;                { Filehandle for this indexfile }
    Counter      : Longint;
begin
  {-- Initialize some variables ---------------------------------------------}
  GetLastReadJAM := 0;
  AbortRead := FALSE;

  {-- Open the index files --------------------------------------------------}
  New(Temp_F, Init);
  Temp_F^.Assign(MsgInf.JamBase + '.jlr');
  Temp_F^.FileMode := ReadMode + DenyNone;

  if NOT Temp_F^.Open(SizeOf(JamLastType)) then
    begin
      Dispose(Temp_F, Done);
      EXIT;
    end; { if }

  {-- Calculate the CRC32 values for the users' handles and names -----------}
  ThisNameCrc := JamStrCrc(LineCfg^.Exitinfo^.Userinfo.Name);
  ThisHandleCrc := JamStrCrc(LineCfg^.Exitinfo^.Userinfo.Handle);

  {-- Now loop through the lastread file ------------------------------------}
  While (NOT Temp_F^.EOF) AND (NOT AbortRead) do
    begin
      Numread := Temp_F^.BlkRead(JamLast, (JamLastBufSize - 01));

      {-- Loop through all read items ---------------------------------------}
      for Counter := 0 to Numread do
        With JamLast[Counter] do
          if (NameCRC = ThisNameCRC) OR (NameCRC = ThisHandleCRC) then
            begin
              AbortRead := TRUE;
              GetLastReadJAM := JAMLast[Counter].HighRead;

              BREAK;
            end; { if }
    end; { while }

  {-- Dispose all allocated memory ------------------------------------------}
  Dispose(Temp_F, Done);
end; { func. GetLastReadJAM }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetHighestMsgNum(const MsgInf: MessageRecord): Longint;
begin
  if ReadBit(MsgInf.Attribute, 7) then
    GetHighestMsgNum := GetHighestJam(MsgInf)
      else GetHighestMsgNum := GetHighestHudson(MsgInf.AreaNum);
end; { func. GetHighestMsgNum }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetLastReadNum(const MsgInf: MessageRecord): Longint;
begin
  if ReadBit(MsgInf.Attribute, 7) then
    GetLastreadNum := GetLastreadJam(MsgInf)
      else GetLastreadNum := GetLastreadHudson(MsgInf.AreaNum);
end; { func. GetLastReadNum }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function HasNewMail(const MsgInf: MessageRecord): Boolean;
var HighMsgNum: Longint;
    LastRead  : Longint;
begin
  HighMsgNum := GetHighestMsgNum(MsgInf);
  LastRead := GetLastReadNum(MsgInf);

  HasNewMail := (HighMsgNum > LastRead);
end; { func. HasNewMail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function DoQuickJamScan(AreaID: String): Boolean;
const
  JamIdxBufSize = 512;

type
  JamIDXtype = record                          { Quick index to these files }
     MsgToCrc : Longint;
     HdrLoc   : Longint;
  end; { JamIdxType record }

type
  JamIdxArrayType = Array[0..(JamIdxBufSize - 01)] of JamIdxType;

var JamIdx       : ^JamIdxArrayType;   { Contains all CRC's for 512 records }
    NameCRC      : Longint;                          { CRC32 for users name }
    HandleCRC    : Longint;                        { CRC32 for users handle }
    NumRead      : NumReadType;                { How many records were read }
    AbortRead    : Boolean;                           { We found one? Abort }
    Temp_F       : pFileObj;                { Filehandle for this indexfile }
    Counter      : Longint;
begin
  {-- If not an JAM area, assume its worth scanning -------------------------}
  if SUpCase(Copy(AreaID[1], 1, 1)) <> 'J' then
    begin
      DoQuickJAMscan := TRUE;
      EXIT;
    end; { if }

  {-- Initialize some variables ---------------------------------------------}
  DoQuickJamScan := FALSE;
  AbortRead := FALSE;
  New(JamIdx);

  {-- Open the index files --------------------------------------------------}
  New(Temp_F, Init);
  Temp_F^.Assign(Copy(AreaID, 2, Length(AreaID)) + '.jdx');
  Temp_F^.FileMode := ReadMode + DenyNone;

  if NOT Temp_F^.Open(SizeOf(JamIdxType)) then
    begin
      Dispose(Temp_F, Done);
      Dispose(JamIdx);
      EXIT;
    end; { if }

  {-- Calculate the CRC32 values for the users' handles and names -----------}
  NameCrc := JamStrCrc(LineCfg^.Exitinfo^.Userinfo.Name);
  HandleCrc := JamStrCrc(LineCfg^.Exitinfo^.Userinfo.Handle);

  {-- Now loop through the indexfile ----------------------------------------}
  While (NOT Temp_F^.EOF) AND (NOT AbortRead) do
    begin
      Numread := Temp_F^.BlkRead(JamIDX^, (JamIdxBufSize - 01));

      {-- Loop through all read items ---------------------------------------}
      for Counter := 0 to Numread do
        With JamIDX^[Counter] do
          if (MsgToCRC = NameCRC) OR (MsgToCrc = HandleCrc) then
            begin
              AbortRead := TRUE;
              DoQuickJamScan := TRUE;

              BREAK;
            end; { if }
    end; { while }

  {-- Dispose all allocated memory ------------------------------------------}
  Dispose(Temp_F, Done);
  Dispose(JamIDX);
end; { func. DoQuickJamScan }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ScanArea(var   BoardInf    : MessageRecord;
                   var   EleBoardInf : EleMessageRecord;
                   const IsHudson,
                         DoEchomail  : Boolean;
                   var   MsgFound    : MailBoxCheckArrayType;
                   var   CurMsgsFound: Longint;
                   const Username,
                         Userhandle  : MsgToIdxRecord);
var AreaID     : String;                              { AreaID to this area }
    Handle     : MsgToIdxRecord;         { Messagebase object for this area }
    MsgScan    : AbsMsgPtr;
    IncludeThis: Boolean;                               { Include this area }
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailInt, 'ScanArea: '+FStr(BoardInf.AreaNum));
  {$ENDIF}

  {-- Create an appropriate AreaID for this area ----------------------------}
  if IsHudson then
    begin
      AreaID := 'H001' + GlobalCfg^.RaConfig^.MsgBasePath;
    end
      else begin
             AreaID := MakeMsgId(BoardInf.AreaNum, BoardInf.JamBase, BoardInf.Attribute,
                                 EleBoardInf.Attribute);
           end; { else }

  {-- Create an handle else empty to users will show up ---------------------}
  Handle := UserHandle;
  if (Handle = '') then
    Handle := Username;

  {-- Now scan the area, this is a 2 way process: ---------------------------}
  {-- 1: Check if its worth scanning (if JAM area) --------------------------}
  {-- 2: Actually scan the area ---------------------------------------------}
  if (DoQuickJamScan(AreaID)) then
   if (OpenOrCreateMsgArea(MsgScan, AreaID)) then
     begin
       {-- Now actually scan for this users name ----------------------------}
       MsgScan^.YoursFirst(UserName, Handle);
       While (MsgScan^.YoursFound) AND (CurMsgsFound < MailBoxCheckMaxFound) do
         begin
           MsgScan^.MsgStartUp;                    { Initialize the header }

           if (NOT ReadBit(BoardInf.Attribute, 7)) then   { Not a JAM area }
             GetMessageRecord(BoardInf, MsgScan^.GetSubArea, true);

           {-- Make sure we have access to this area ------------------------}
           IncludeThis := TRUE;
           if NOT (CheckMsgAreaAccess(BoardInf, false, true, 0, LineCfg^.Exitinfo^.userinfo)) then
             IncludeThis := FALSE;

           if (BoardInf.Typ = EchoMail) then
            if (NOT DoEchoMail) then IncludeThis := FALSE;

           {-- If included, include it --------------------------------------}
           if IncludeThis then
             begin
               Inc(CurMsgsFound);                        { Increment by one }

               if (IsHudson) then
                 MsgFound[CurMsgsFound].BoardNr := MsgScan^.GetSubArea
                   else MsgFound[CurMsgsFound].BoardNr := BoardInf.AreaNum;

               MsgFound[CurMsgsFound].MsgNr := MsgScan^.GetMsgNum;
               MsgFound[CurMsgsFound].FromWho := MsgScan^.GetFrom;
               MsgFound[CurMsgsFound].ToWho := MsgScan^.GetTo;
               MsgFound[CurMsgsFound].SubJect := MsgScan^.GetSubj;
               MsgFound[CurMsgsFound].Date := MsgScan^.GetDate;
               MsgFound[CurMsgsFound].Time := MsgScan^.GetTime;
             end; { if }

           {-- Skip to the next area ----------------------------------------}
           MsgScan^.YoursNext;
         end; { while }

       {-- Close this area --------------------------------------------------}
       CloseMsgArea(MsgScan);
     end; { if }
end; { proc. ScanArea }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ScanMailAreas(var MsgFound       : MailBoxCheckArrayType;
                        var CurMsgsFound   : Longint;
                        var OldCheckmailStr: ShortString;
                            CheckMailCB    : CheckMailCallBackType);
var DoEchoMail       : Boolean;            { Include echomail in this scan? }
    DidHudson        : Boolean;          { Did we scan an hudson area, yet? }
    TotalItems,                            { Counters for messagearea files }
    ItemCounter      : Longint;
    Area_F           : pFileObj;      { File variable for reading the areas }
    MessageInf       : MessageRecord;  { Msg-area were currently processing }
    EleMsgInf        : EleMessageRecord;
begin
  {-- Initialize the needed variables ---------------------------------------}
  { Clearing the beneath array is important to avoid "false results" }
  FillChar(MsgFound, SizeOf(MailBoxCheckArrayType), 00);
  FillChar(MessageInf, SizeOf(MessageInf), 0);
  CurMsgsFound := 00;                                   { No messages found }
  DidHudson    := false;                          { We didn't do HUDSON yet }

  {-- Only scan echomail areas if the user sais so --------------------------}
  if ReadBit(LineCfg^.Exitinfo^.Userinfo.Attribute2, 5) then
    DoEchoMail := FALSE
     else DoEchoMail := TRUE;


  {-- Warn the user we're going to check this area --------------------------}
  {-- need to call here, else the user wont see a prompt at all -------------}
  {-- if there were no areas to check ---------------------------------------}
  CheckMailCB(MessageInf, OldCheckMailStr);

  {-- Open the areafile -----------------------------------------------------}
  New(Area_F, Init);
  Area_F^.Assign(MessagesFileName);
  Area_F^.FileMode := ReadMode + DenyNone;
  if Area_F^.Open(1) then
    begin
      TotalItems := Area_F^.FileSize div SizeOf(MessageRecord);
      ItemCounter := 00;

      {-- Loop through all areas --------------------------------------------}
      While (ItemCounter < TotalItems) do
        begin
          Area_F^.BlkRead(MessageInf, SizeOf(MessageRecord));
          Inc(ItemCounter);

          {-- Notify the user --------------------------------------------------}
          if @CheckMailCB <> nil then
            CheckMailCB(MessageInf, OldCheckMailStr);

          {-- Make sure we scan HUDSON only once ----------------------------}
          if NOT ReadBit(MessageInf.Attribute, 7) then     { Area is HUDSON }
           if NOT DidHudson then
             begin
               GetEleMessageRecord(EleMsgInf, MessageInf.Areanum, true);

               DidHudson := TRUE;
               ScanArea(MessageInf, EleMsgInf, true, DoEchoMail, MsgFound, CurMsgsFound,
                        LineCfg^.Exitinfo^.Userinfo.Name,
                        LineCfg^.Exitinfo^.Userinfo.Handle);
             end; { if }

          {-- If its an JAM area, scan them all -----------------------------}
          if ReadBit(MessageInf.Attribute, 7) then            { Area is JAM }
            begin
              if (MessageInf.Typ = EchoMail) then
                begin
                  if (DoEchoMail) then
                    ScanArea(MessageInf, EleMsgInf,
                             false, DoEchoMail, MsgFound, CurMsgsFound,
                             LineCfg^.Exitinfo^.Userinfo.Name,
                             LineCfg^.Exitinfo^.Userinfo.Handle)
                end
                  else begin
                         ScanArea(MessageInf, EleMsgInf,
                                  false, DoEchoMail, MsgFound, CurMsgsFound,
                                  LineCfg^.Exitinfo^.Userinfo.Name,
                                  LineCfg^.Exitinfo^.Userinfo.Handle);
                       end; { else }
            end; { if }
        end; { while }

    end; { if Area is opened }

  {-- Close the files -------------------------------------------------------}
  Dispose(Area_F, Done);
end; { proc. ScanMailAreas }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SilentDeleteMsg(BoardNr: Longint; MessageNr: Longint; CheckAccess: Boolean);
var DoDelete  : Boolean;
    MsgBaseObj: AbsMsgPtr;
    MessageInf: MessageRecord;
    EleMsgInf : EleMessageRecord;
begin
  {-- First gather all information we need ------------------------------------}
  DoDelete := FALSE;
  GetMessageRecord(MessageInf, BoardNr, false);
  GetEleMessageRecord(EleMsgInf, BoardNr, false);

  {-- Try to open the messagebase ---------------------------------------------}
  if (NOT OpenOrCreateMsgArea(MsgBaseObj,
                              MakeMsgID(MessageInf.AreaNum,
                                        MessageInf.JamBase,
                                        MessageInf.Attribute,
                                        EleMsgInf.Attribute))) then EXIT;

  {-- Now lookup the correct message ------------------------------------------}
  MsgBaseObj^.SeekFirst(MessageNr);
  MsgBaseObj^.MsgStartup;

  {-- Check the access level --------------------------------------------------}
  if CheckAccess then
    begin
      DoDelete := CheckMsgDeleteAccess(MessageInf,
                                       MsgBaseObj^.GetTo,
                                       MsgBaseObj^.GetFrom,
                                       MsgBaseObj^.IsSent,
                                       LineCfg^.Exitinfo^.Userinfo);
    end
      else begin
             DoDelete := TRUE;
           end; { if }

  {-- If not OK to delete, just log it to the log -----------------------------}
  RaLog('!', 'Unable to delete message #'+ FStr(MessageNr) + ' in area ' + MessageInf.Name);

  {-- Now actually delete the message -----------------------------------------}
  if MsgBaseObj^.SeekFound then
    if DoDelete then
      DoDeleteMsg(MsgBaseObj);

  {-- Close this message area -------------------------------------------------}
  CloseMsgArea(MsgBaseObj);
end; { proc. SilentDeleteMsg }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoDeleteMsg(var MsgInfo: AbsMsgPtr);
var SaveMsg: Longint;
begin
  {-- Make sure our parent (if any) is cleared -----------------------------}
  if MsgInfo^.GetRefer > 0 then
    begin
      {-- Save the message -------------------------------------------------}
      SaveMsg := MsgInfo^.GetMsgNum;

      {-- Lookup the new message -------------------------------------------}
      MsgInfo^.SeekFirst(MsgInfo^.GetRefer);
      if MsgInfo^.SeekFound then
        begin
          {-- Initialize the variables -------------------------------------}
          MsgInfo^.MsgStartUp;

          {-- Now clear out the refers to this message ---------------------}
          if MsgInfo^.GetSeeAlso = SaveMsg then
            MsgInfo^.SetSeeAlso(0);

          {-- Now clear out the refers to this message ---------------------}
          if MsgInfo^.GetNextSeeAlso = SaveMsg then
            MsgInfo^.SetNextSeeAlso(0);

          {-- Now update the message header --------------------------------}
          MsgInfo^.RewriteHdr;
        end; { if }

      {-- and seek to the new message --------------------------------------}
      MsgInfo^.SeekFirst(SaveMsg);
      MsgInfo^.MsgStartUp;
    end; { if }

  {-- Actually delete the message ------------------------------------------}
  MsgInfo^.DeleteMsg;
end; { proc. DoDeleteMsg }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit MAIL }
