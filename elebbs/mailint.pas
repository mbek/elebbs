unit MailInt;
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
** Mail interface routines for EleBBS
**
** Copyright (c) 1996, 1997 by Maarten Bekers
**
** Created : 24-Nov-1997
** Last update : 06-Sep-1999
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF WITH_FULL}
  This unit shouldn''t be compiled for an utility program
{$ENDIF}

{$IFDEF WITH_FULL}
uses Global, CfgRec;


function  ShowScan(var LineTeller: Byte; MsgNr, BoardNr: Word; Date, Time, FromWho, ToWho, Subj: String;
                   MarkMsgs, DoCombined: Boolean): Boolean;
function  ShowQuickScan(First: Boolean; var LineTeller: Byte; MsgNr:Word; FromWho, ToWho, Subject: String;
                         DoCombined: Boolean): Boolean;

procedure MenuDeleteMsg(MiscData: String; var Exitinfo: ExitinfoRecord);
procedure DeleteMsg(const MessageInf : MessageRecord;
                          EleMsgInf  : EleMessageRecord;
                          MsgNr      : LongInt;
                          ShowMessage: Boolean;
                    var   Exitinfo   : ExitinfoRecord);
procedure CheckMailBox(var Exitinfo: ExitinfoRecord);
procedure AskForReadMail(var MsgFound     : MailBoxCheckArrayType;
                             CurMsgsFound : Word;
                         var Exitinfo     : ExitinfoRecord);
procedure GetAreaTypeString(var TypeOfArea: String; const MessageInf: MessageRecord);
procedure ReadMessages(    ReadWay  : MessageShowType;
                           MiscData : String;
                       var Exitinfo : ExitinfoRecord);
function DoReadMail(const ReadWay     : MessageShowType;
                          BoardNr     : Word;
                    const DoForward   : Boolean;
                    const StartNr     : Longint;
                    const Individual,
                          MsgPause,
                          NewScan     : Boolean;
                    const SelectWay   : msgSelectType;
                    const SearchStr   : String;
                    const MarkMessages: Boolean;
                    const DoCombined  : Boolean;
                    var   Exitinfo    : ExitinfoRecord): MailReturnType;
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
uses Mail, StUtils, InOut_U, FileRout, {$IFDEF WITH_FULL}Transfer, {$ENDIF}Support,
      RAL, LineEd, MenuFunc, Control, MkFile, Debug_U, ElLog_U, AreaSel,
       Access_U, DispAns, IntEdit, MkMsgAbs, MkOpen, FileObj,
        FileSys, MkMsgJam, BitWise, Colors, ExecFunc,
         WriteMsg, ReadMsg, Cases, LongStr, CentrStr, WordStr, Question,
          OutBlock, Input_U, StrUnit, elx_BBS, ObjDec
          {$IFDEF WIN32}
            ,SysUtils
          {$ENDIF}, MemMan;


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GetAreaTypeString(var TypeOfArea: String; const MessageInf: MessageRecord);
begin
  TypeOfArea := '';
  case MessageInf.typ of
      LocalMail : TypeOfArea := LangObj^.ralGet(ralLocal1);
      NetMail   : TypeOfArea := LangObj^.ralGet(ralNetMail1);
      EchoMail  : TypeOfArea := LangObj^.ralGet(ralEchoMail);
      InterNet  : TypeOfArea := LangObj^.ralGet(ralInterNet);
      NewsGroup : TypeOfArea := LangObj^.ralGet(ralNewsGrp);
      ForumGroup: TypeOfArea := LangObj^.ralGet(ralForumGrp);
  end; { MessageInf.Typ }

end; { proc. GetAreaTypestring }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure MenuDeleteMsg(MiscData: String; var Exitinfo: ExitinfoRecord);
var MessageInf: MessageRecord;
    EleMsgInf : EleMessageRecord;
    Temp      : Longint;
begin
  if Pos('/M', SUpcase(MiscData))>00 then
    begin
      GetMessageRecord(MessageInf, Exitinfo.Userinfo.MsgArea, True);
      GetEleMessageRecord(EleMsgInf, Exitinfo.Userinfo.MsgArea, True);
    end
     else begin
            GetMessageRecord(MessageInf, FVal(FirstWord(MiscData, defExtractWord, false)), True);
            GetEleMessageRecord(EleMsgInf, FVal(FirstWord(MiscData, defExtractWord, false)), True);
          end; { else }

  Writeln;
  WriteLn('`A11:', LangObj^.ralGet(ralMsgArea),' "', MessageInf.Name,'" ',
          LangObj^.ralGet(ralContains), ' ', GetActiveMsgNum(MessageInf, EleMsgInf), ' ', LangObj^.ralGet(ralMessages2));

  RangeEdit(Temp, '`A11:'+LangObj^.ralGet(ralDelMsg), 0, 65535, 0);
  if (Temp = 00) then EXIT;

  DeleteMsg(MessageInf, EleMsgInf, Temp, True, Exitinfo);
end; { proc. MenuDeleteMsg }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DeleteMsg(const MessageInf : MessageRecord;
                          EleMsgInf  : EleMessageRecord;
                          MsgNr      : LongInt;
                          ShowMessage: Boolean;
                    var   Exitinfo   : ExitinfoRecord);
var MsgInfo   : AbsMsgPtr;
    OkToDelete: Boolean;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailInt, 'DeleteMsg (BoardNr='+FStr(MessageInf.AreaNum)+')');
  {$ENDIF}

  OkToDelete := False;
  If NOT OpenOrCreateMsgArea(MsgInfo, MakeMsgId(MessageInf.AreaNum, MessageInf.JamBase, MessageInf.Attribute,
                                                EleMsgInf.Attribute)) then EXIT;
  MsgInfo^.SeekFirst(MsgNr);
  MsgInfo^.MsgStartup;

  OkToDelete := CheckMsgDeleteAccess(MessageInf,
                                     MsgInfo^.GetTo,
                                     MsgInfo^.GetFrom,
                                     MsgInfo^.IsSent,
                                     Exitinfo.UserInfo);

  If NOT OkToDelete then
    begin
      WriteLn;
      WriteLn('`A12:',LangObj^.ralGet(ralErrDel));
      InputObj^.PressEnter(True, False);
    end; { Not OK to Delete }

  If (ShowMessage) AND (MsgInfo^.SeekFound) then
   if OkToDelete then
    begin
      WriteLn;
      Writeln('`A12:',LangObj^.ralGet(ralDeleting),' ', MsgNr);
      InputObj^.PressEnter(True, False);
    end;

  If (ShowMessage) then
   If OkToDelete then
    If NOT MsgInfo^.SeekFound then
     begin
       WriteLn;
       WriteLn('`A12:',LangObj^.ralGet(ralNoMsgNr), #32, MsgNr, #32, LangObj^.ralGet(ralThisArea));
       InputObj^.PressEnter(True, False);
     end; { If }

  If MsgInfo^.SeekFound then
   if OkToDelete then
     DoDeleteMsg(MsgInfo);

  CloseMsgArea(MsgInfo);
end; { proc. DeleteMsg }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ForwardThisMessage(const MsgNr     : Longint;
                             var   MessageInf: MessageRecord;
                             var   Exitinfo  : ExitinfoRecord);
var AreaTemp  : String;
    BoardNr   : Word;
    QuoteText : pStringArrayObj;
    QuoteLines: Longint;
    NrWritten : Longint;
    ToWhoStr  : String;

    FromWho    : String;
    ToWho      : String;
    Subj       : String;
    Address    : String;
    ReplyTo    : String;
    ReplyAddr  : String;
    DateStr    : String;
    ReplyKludge: String;
begin
  {--------------- Show the user an areanumber to forward to ---------------}
  ShowMsgAreaNewMail('/NONEW', false, true, false);

  AreaTemp := '';
  Write('`A15:' , LangObj^.ralGet(ralFwardArea));
  GetString(AreaTemp, 5, ['0'..'9'], False, False, False, False);
  If FVal(AreaTemp)=00 then EXIT;
  BoardNr := FVal(AreaTemp);

  {------------ Add the forward header to the reply text -------------------}
  New(QuoteText, Init(MaxMsgLines));

  ReplyToMsg(QuoteText^, QuoteLines, Ra250MsgArea(MessageInf.AreaNum), MsgNr,
             FromWho, ToWho, Subj, Address, ReplyTo, ReplyAddr, ReplyKludge,
             DateStr, false);

  {----------------- Actually forward the message ---------------------------}
  GetAreaTypeString(ToWhoStr, MessageInf);
  ToWhoStr := ' ' + ToWhoStr + #32 + langObj^.ralGet(ralArea) + #32 +
              '"' + MessageInf.Name + '"';

  WriteMessage(BoardNr,
               ToWhoStr,
               Exitinfo.Userinfo.Name,
               Subj,
               '',
               true,     { reply }
               false,    { edit }
               QuoteText^,
               QuoteLines,
               NrWritten,
               false,     { addwarnmsg }
               true,      { allow attaches }
               FromWho,   { orig towho }
               '',        { from address }
               '',        { to address }
               '',        { reply-to }
               '',        { reply-addr }
               true, true, DateStr, MessageInf.Name, ReplyKludge);

  Dispose(QuoteText, Done);
end; { proc. ForwardThisMessage }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function ShowQuickScan(First: Boolean; var LineTeller: Byte; MsgNr:Word; FromWho, ToWho, Subject: String;
                       DoCombined: Boolean): Boolean;
Const NrLen   = 05;
      FromLen = 24;
      ToLen   = 23;
      SubjLen = 22;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailInt, 'ShowQuickScan');
  {$ENDIF}

  ShowQuickScan := True;

  If First then
    begin
      Inc(LineTeller, 2);
      WriteLn;
      WriteLn;
      WriteLn('`A14:',LangObj^.ralGet(ralQuickHdr));
      WriteLn(OutputObj^.AvatarDupl('-', NrLen), #32,
              OutputObj^.AvatarDupl('-', FromLen), #32,
              OutputObj^.AvatarDupl('-', ToLen), #32, #32,
              OutputObj^.AvatarDupl('-', SubjLen));
      EXIT;
    end; { Make the message header }

  WriteLn('`A3:',
          MakeLen(FStr(MsgNr), NrLen, Space, False, false), #32,
          MakeLen(FromWho, FromLen, Space, False, false), #32,
          MakeLen(ToWho, ToLen, Space, False, false), #32, #32,
          MakeLen(Subject, SubjLen, Space, False, false));

  ShowQuickScan := NOT OutputObj^.StopMore;
end; { ShowQuickScan }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ShowScan(var LineTeller: Byte; MsgNr, BoardNr: Word; Date, Time, FromWho, ToWho, Subj: String;
                  MarkMsgs, DoCombined: Boolean): Boolean;
var MessageInf: MessageRecord;
    Goodkeys  : CharSet;
    CH        : Char;
    St        : String;
{$IFNDEF WIN32}
{$IFNDEF FPC}
 {$IFNDEF OS2}
    Result    : Boolean;
 {$ENDIF}
{$ENDIF}
{$ENDIF}
begin;
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailInt, 'ShowScan (BoardNr='+Fstr(BoardNr)+')');
  {$ENDIF}

  Result := TRUE;
  GetMessageRecord(MessageInf, BoardNr, true);

  Write('`A', MakeAttr(GlobalCfg^.RaConfig^.NormFore, GlobalCfg^.RaConfig^.NormBack), ':');
  WriteLn;
  Writeln;
  Writeln('`A15:', FirstUpper(LangObj^.ralGet(ralMessage)),' #', MsgNr,' - ', MessageInf.Name);
  Writeln('`A14:',LangObj^.ralGet(ralDate),' ', LangObj^.RaFormatDate(Date, 0, y2k_MsgDate,
        LineCfg^.Exitinfo^.Userinfo.DateFormat), ' ', Time);
  Writeln('`A14:',LangObj^.ralGet(ralFrom2),' ', FromWho);
  Writeln('`A14:',LangObj^.ralGet(ralTo2),' ', ToWho);
  Writeln('`A14:',LangObj^.ralGet(ralSubject3),' ', Subj);

  If MarkMsgs then
   begin
     WriteLn;
     Write('`F',GlobalCfg^.RaConfig^.BarFore,':`B',GlobalCfg^.RaConfig^.BarBack,':');

     GoodKeys := [];

     Str2Set(LangObj^.ralGetKey(ralYes)+
             LangObj^.ralGetKey(ralNo)+
             LangObj^.ralGetKey(ralStop2), GoodKeys);
     Write(LangObj^.ralGetStr(ralAskMark));

     REPEAT
       CH := UpCase(InputObj^.ReadKey);

       IF CH=#13 then
         CH := UpCase(LangObj^.ralGetKey(ralNo));

       If Ch=LangObj^.ralGetKey(ralStop2) then
          begin
            Write(LangObj^.ralGetStr(ralStop2));
            Result := False;
          end; { stop }

       If Ch=LangObj^.ralGetKey(ralYes) then
             begin
               Write(LangObj^.ralGetStr(ralYes));
               AddToMarkList(BoardNr, MsgNr);
             end; { LangObj^.ralGetkey }

       If Ch=LangObj^.ralGetKey(ralNo) then Write(LangObj^.ralGetStr(ralNo));
     UNTIL (CH in GoodKeys) OR (ProgTerminated);
   end; { markmsgs }

  if OutputObj^.StopMore then Result := FALSE;

  ShowScan := RESULT;
  OutputObj^.SetStopMore(False);
end; { func. ShowScan }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure MailScanCallBack(const MessageInf: MessageRecord;
                           var OldCheckMailStr: ShortString);

function ScanMailStr(NewStr, Old: String; const MessageInf: MessageRecord): String;
begin
  ScanMailStr := NewStr;

  Replace(^K'Y', MessageInf.Name, NewStr);
  Replace(^K'1', FStr(Ra250MsgArea(MessageInf.AreaNum)), NewStr);

  if Length(NewStr) < Length(Old) then
    NewStr := NewStr + OutputObj^.AvatarDupl(#32, (Length(Old) - Length(NewStr)));

  While NoColorLength(NewStr) > 75 do
    Delete(NewStr, Length(NewStr), 1);

  ScanMailStr := NewStr;
end; { func. ScanMailStr }

begin
  {-- Write the new area were scanning -------------------------------------}
  if Pos(^K'Y', SUpCase(langObj^.ralGet(ralChkMail))) > 0 then
    begin
      OldCheckMailStr := ScanMailStr(langObj^.ralGet(ralChkMail), OldCheckMailStr, MessageInf);
      Write('`A14:`X1:', OldCheckMailStr);

      Flush(Output);
      OutBlockObj^.DumpBlock;
      InputObj^.UpdateScrn;
    end; { if }

  {-- Or notify of the first update ----------------------------------------}
  if Pos(^K'Y', SUpCase(langObj^.ralGet(ralChkMail))) = 0 then
   if OldCheckMailStr = '' then
    begin
      OldCheckMailStr := ScanMailStr(langObj^.ralGet(ralChkMail), OldCheckMailStr, MessageInf);
      Write('`A14:`X1:', OldCheckMailStr);

      Flush(Output);
      OutBlockObj^.DumpBlock;
      InputObj^.UpdateScrn;
    end; { if }

  {-- Reset idle timer -----------------------------------------------------}
  contrObj^.SetTimeOut;
end; { proc. MailScanCallBack }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CheckMailBox(var Exitinfo: ExitinfoRecord);
                                      { Scan mailbox for new addressed mail }
var AreaCounter      : Longint;        { Used for looping through all areas }
    TempCounter      : Longint; { Used for piling up all msgs for all areas }
    BoardCounter     : Longint;
    MessageCounter   : Longint;         { Total messages found in that area }
    MessagesDisplayed: Longint;             { Total messages found globally }
    CurMsgsFound     : Longint;            { Total amount of messages found }
    MsgFound         : ^MailBoxCheckArrayType;  { Results of scan operation }
    MessageInf       : MessageRecord;  { Msg-area were currently processing }
    OldCheckMailStr  : ShortString;                           { Mail string }
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailInt, 'CheckMailBox (begin)');
  {$ENDIF}

  {-- Allocate memory for this mailboxcheck ---------------------------------}
  OldCheckMailStr := '';
  if NOT AllocMem(MsgFound, SizeOf(MailBoxCheckArrayType), 'MailBoxCheckArrayType', 'CheckMailBox') then
    begin
      RaLog('!', 'Not enough memory to perform mailbox scan operation, please free some memory!!');
      EXIT;
    end; { if }


  {-- Do the lowlevel scan --------------------------------------------------}
  ScanMailAreas(MsgFound^,                      { Array to put the result in }
                CurMsgsFound,               { Total number of messages found }
                OldCheckMailStr,                   { Old checking mailstring }
                {$IFDEF FPC}@{$ENDIF}MailScanCallBack);{ Mailscan callback routine }

  {-- Now display the messages we have found, if any ------------------------}
  WriteLn;
  WriteLn;
  MessagesDisplayed := 00;

  if CurMsgsFound > 00 then
    begin
      {-- Show a message saying they have new mail --------------------------}
      WriteLn('`A10:', LangObj^.ralGet(ralNewMail));
      WriteLn('`A02:', OutputObj^.AvatarDupl(#196, NoColorLength(LangObj^.ralGet(ralNewMail))));

      {-- Group all finds per area ------------------------------------------}
      {-- AreaCounter  A counter that loops through all areas }
      {-- Boardcounter A counter that loops through all founnd messages }
      {--              looking for a message in this board }
      {-- TempCounter  Counter to add up all messages found in this area }
      for AreaCounter := 0 to 65534 do
        begin
          if MessagesDisplayed >= CurMsgsFound then BREAK;

          for BoardCounter := 0 to MailBoxCheckMaxFound do
           if MsgFound^[BoardCounter].BoardNr = AreaCounter then
            if MsgFound^[BoardCounter].BoardNr > 0 then
              begin
                 GetMessageRecord(MessageInf, AreaCounter, true);

                 {-- Look for all messages in this area ---------------------}
                 MessageCounter := 0;
                 for TempCounter := 0 to MailBoxCheckMaxFound do
                   if MsgFound^[TempCounter].BoardNr = AreaCounter then
                     Inc(MessageCounter);

                 {-- Add up the total number --------------------------------}
                 Inc(MessagesDisplayed, MessageCounter);

                 {-- If any messages found in this area, display totals -----}
                 if MessageCounter > 00 then
                   if MessageCounter = 1 then
                     begin
                       WriteLn('`A11:', MakeLen(MessageInf.Name, 30, Space, false, false),
                               ' `A14:: ',MessageCounter, ' ', LangObj^.ralGet(ralMessage))
                     end
                       else begin
                              WriteLn('`A11:', MakeLen(MessageInf.Name, 30, Space, False, false),
                                     ' `A14:: ',MessageCounter, ' ', LangObj^.ralGet(ralMessages2));
                            end; { else }

                 {-- Break the current loop, we've found it all -------------}
                 BREAK;
              end; { if msgfound }
        end; { if }

        {-- Now show this mail to the user ----------------------------------}
        AskForReadMail(MsgFound^, CurMsgsFound, Exitinfo);
    end { Unreceived messages are found }
      else begin
             WriteLn('`A7:', LangObj^.ralGet(ralNoNewMail));
             InputObj^.PressEnter(false, true);
            end; { No new mail received }

  {-- Reset variables and release all allocated memory ----------------------}
  OutputObj^.SetStopMore(False);
  ReleaseMem(MsgFound, SizeOf(MailBoxCheckArrayType));

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailInt, 'CheckMailBox ( end )');
  {$ENDIF}
end; { proc. CheckMailBox }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AskForReadMail(var MsgFound     : MailBoxCheckArrayType;
                             CurMsgsFound : Word;
                         var Exitinfo     : ExitinfoRecord);

procedure ProcessAllMessages(Delete: Boolean; Nr: Longint);
var Counter    : Word;
    MsgUpdate  : AbsMsgPtr;
    MessageInf : MessageRecord;
    EleMsgInf  : EleMessageRecord;
begin
  if Nr=-1 then
   For Counter:=01 to CurMsgsFound do
    begin
       GetMessageRecord(MessageInf, MsgFound[Counter].BoardNr, False);
       GetEleMessageRecord(EleMsgInf, MsgFound[Counter].BoardNr, False);

       if (ReadBit(MessageInf.Attribute, 6)) OR (NOT Delete) then
        If OpenOrCreateMsgArea(MsgUpdate, MakeMsgId(MsgFound[Counter].BoardNr, MessageInf.JamBase, MessageInf.Attribute,
                                                    EleMsgInf.Attribute)) then
         begin;
           MsgUpdate^.SeekFirst(MsgFound[Counter].MsgNr);
           if MsgUpdate^.SeekFound then
             begin
               MsgUpdate^.MsgStartUp;               { Initialize messageheader }
               If Delete then DoDeleteMsg(MsgUpdate)              { Delete msg }
                else MsgUpdate^.SetRcvd(True);       { Set message to received }

               MsgUpdate^.ReWriteHdr;                   { Update messageheader }
               CloseMsgArea(MsgUpdate);                      { Close this area }
             end; { if }
         end; { OpenMsgArea }
      end; { if }

  If Nr<>-1 then
    begin
     GetMessageRecord(MessageInf, MsgFound[Nr].BoardNr, False);
     GetEleMessageRecord(EleMsgInf, MsgFound[Nr].BoardNr, False);

     if (ReadBit(MessageInf.Attribute, 6)) OR (NOT Delete) then
      If OpenOrCreateMsgArea(MsgUpdate, MakeMsgId(MsgFound[Nr].BoardNr, MessageInf.JamBase, MessageInf.Attribute,
                                                   EleMsgInf.Attribute)) then
       begin;
         MsgUpdate^.SeekFirst(MsgFound[Nr].MsgNr);
         If MsgUpdate^.SeekFound then
           begin;
             MsgUpdate^.MsgStartUp;                 { Initialize messageheader }
             If Delete then DoDeleteMsg(MsgUpdate)                { Delete msg }
              else MsgUpdate^.SetRcvd(True);         { Set message to received }

             MsgUpdate^.ReWriteHdr;                     { Update messageheader }
           End; { MsgFound }

         CloseMsgArea(MsgUpdate);                           { Close this area }
       end; { OpenMsgArea }
    end; { if }

end; { proc. ProcessAllMessages }


label EndOfMsgs;

var
    LineTeller: Byte;
    Result: Boolean;
    MsgNrWritten: LongInt;
    MsgNr: Word;
    EditorInfo: pStringArrayObj;
    ToWho, FromWho, Subj, Address: String;
    DateStr: String;
    ReplyLines: Longint;

var QuestionMark : Boolean;                  { Does MAILHELP (ansi) exist? }
    ReplyMsgNr   : Longint;
    CH           : Char;
    GoodKeys     : CharSet;
    TempMsg      : MessageRecord;
    BoardNr      : Word;
    MessageInf   : MessageRecord;
    Counter      : Longint;
    SaveDisp     : Boolean;
    Question     : QuestObj;
    elxObj       : pElxBBsObj;

    ReplyKludge  : String;
    ReplyTo,
    ReplyAddr    : String;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailInt, 'AskForReadMail (part of CheckMailBox)');
  {$ENDIF}

  QuestionMark := FileExist(OpenTextFile('MAILHELP'));

  SaveDisp := LineCfg^.DispMorePrompt;

  REPEAT
    LineCfg^.DispMorePrompt := SaveDisp;

    WriteLn;
    WriteLn('`A14:');

    CH := #01;
    if (LineCfg^.AnsiOn) OR (LineCfg^.AvatarOn) then
      begin
        if GetScriptType('MAILREAD') = 'Q-A' then
          begin
            Question.Init;
            if QuestionMark then
              Question.Process('MAILREAD YES /N', true, '')
               else Question.Process('MAILREAD NO /N', true, '');

            if Question.GetError = 00 then
              begin
                CH := UpCase(FirstChar(Question.GetReturnResult));
              end; { if }
            Question.Done;
          end
            else begin
                   New(elxObj, Init);
                   if NOT elxObj^.RunElexerScript('mailread', '', true) then
                     begin
                       RaLog('!', 'Error occured while executing MAILREAD EleXer script - falling back');
                     end
                       else begin
                              {-- now exit -----------------------------------}
                              CH := FirstChar(elxObj^.GetElxReturnString);
                            end; { else }
                   Dispose(elxObj, Done);
                 end; { else }
      end; { if }

    if CH = #01 then
     begin
       Write(LangObj^.ralGet(ralReadNow),
             GlobalCfg^.RaConfig^.LeftBracket, UpCase(LangObj^.ralGetKey(ralYes)), LangObj^.ralGet(ralSlash),
             LowCase(LangObj^.ralGetKey(ralNo)));

       if QuestionMark then
         Write(LangObj^.ralGet(ralSlash), '?');
       Write(GlobalCfg^.RaConfig^.RightBracket, ': ');

       Str2Set(LangObj^.ralGetKeys(ralYes)+                                    { [Y]es }
               LangObj^.ralGetKeys(ralNo)+                                      { [N]o }
               LangObj^.ralGetKeys(ralRead)+                                  { (R)ead }
               LangObj^.ralGetKeys(ralScan)+                                  { (S)can }
               LangObj^.ralGetKeys(ralQuickScan)+                       { (Q)uick Scan }
               LangObj^.ralGetKeys(ralKilling)+                      { (K)ill messages }
               LangObj^.ralGetKeys(ralMarking), GoodKeys);        { (M)ark as received }
       if QuestionMark then GoodKeys := GoodKeys + ['?'];

       REPEAT
         CH := UpCase(InputObj^.ReadKey);

          If Ch=#13 then
           CH := UpCase(LangObj^.ralGetKey(ralYes));
         If CH=LangObj^.ralGetKey(ralRead) then
           CH := UpCase(LangObj^.ralGetKey(ralYes));
       UNTIL (CH in GoodKeys) OR (ProgTerminated);
     end; { if }
    ReplyMsgNr := -1;                      { Default not following a reply }

    if CH='?' then        { Questionmark is only in GoodKeys if file exist }
      DisplayHotFile('MAILHELP', []);

    If CH=LangObj^.ralGetKey(ralYes) then
       begin
         {$IFDEF WITH_DEBUG}
            DebugObj.DebugLog(logMailInt, 'User wants to read mailbox mail');
         {$ENDIF}

         WriteLn(LangObj^.ralGetStr(ralYes));

         Counter := 01;
         LineTeller := 00;

         REPEAT
           GetMessageRecord(MessageInf, MsgFound[Counter].BoardNr, True);

          {$IFDEF WITH_DEBUG}
            DebugObj.DebugLog(logMailInt, 'Get information for area #'+FStr(MessageInf.AreaNum)+' - '+MessageInf.Name);
          {$ENDIF}

           BoardNr := MsgFound[Counter].BoardNr;
           MsgNr   := MsgFound[Counter].MsgNr;

           If ReplyMsgNr <> -1 then MsgNr := ReplymsgNr;

           Case ReadMsgObj^.ReadMessage(BoardNr,
                                        MsgNr,
                                        True,
                                        LineTeller,
                                        ReplyMsgNr,
                                       true) of
             msgAgain : ; { Nothing }
             msgNext  : begin;
                          Inc(Counter);

                          If Counter>CurMsgsFound then
                            begin
                              WriteLn;
                              Writeln;
                              Writeln(LangObj^.ralGet(ralEndMsgs));
                              InputObj^.PressEnter(False, True);
                              GOTO EndOfMsgs;
                             end; { Last message }
                        end; { MsgNext }
             msgLast  : begin;
                          Dec(Counter);

                          If Counter < 01 then
                            begin;
                              Writeln;
                              Writeln;
                              Writeln(LangObj^.ralGet(ralEndMsgs));
                              InputObj^.PressEnter(false, TRue);
                              GOTO EndOfMsgs;
                            end; { Last message }
                        end; { MsgLast }
            msgForward: begin
                          GetMessageRecord(TempMSG, MsgFound[Counter].BoardNr, true);
                          ForwardThisMessage(MsgFound[Counter].MsgNr, TempMSG, Exitinfo);
                        end; { if }
             msgReply : begin
                          New(EditorInfo, Init(MaxMsgLines));

                          ReplyToMsg(EditorInfo^,
                                     ReplyLines,
                                     Ra250MsgArea(MsgFound[Counter].BoardNr),
                                     MsgFound[Counter].MsgNr,
                                     FromWho,
                                     ToWho,
                                     Subj,
                                     Address,
                                     ReplyTo,
                                     ReplyAddr,
                                     ReplyKludge,
                                     DateStr,
                                     false);

                          BoardNr := MsgFound[Counter].BoardNr;

                          If GlobalCfg^.RaConfig^.AllowNetmailReplies then
                           if (NOT (MessageInf.Typ in [LocalMail, Internet, Newsgroup, Netmail])) then
                            if InputObj^.ralStrYesNoAsk(ralNetReply) then
                             begin
                               if MessageInf.NetmailArea = 00 then
                                 BoardNr := SearchNetmailBoard
                                  else BoardNr := MessageInf.NetMailArea;
                             end; { if }

                          if WriteMessage(Ra250MsgArea(BoardNr),
                                       FromWho,
                                       Exitinfo.Userinfo.Name,
                                       Subj,
                                       '',
                                       True,   { reply }
                                       false,  { edit }
                                       EditorInfo^,
                                       ReplyLines,
                                       MsgNrWritten,
                                       False,
                                       True,
                                       ToWho,
                                       GetAreaAddress(Ra250MsgArea(MsgFound[Counter].BoardNr)),
                                       Address, ReplyTo, ReplyAddr, false, true, DateStr, '', ReplyKludge) then
                                        begin
                                          If BoardNr<>MsgFound[Counter].BoardNr then
                                            SetReplyNr(Ra250MsgArea(MsgFound[Counter].BoardNr),
                                                       MsgFound[Counter].MsgNr,
                                                       MsgNrWritten);
                                        end; { if }

                          Dispose(EditorInfo, Done);
                          Inc(Counter);

                          If Counter>CurMsgsFound then
                            begin
                              WriteLn;
                              Writeln;
                              Writeln(LangObj^.ralGet(ralEndMsgs));
                              InputObj^.PressEnter(False, True);
                              GOTO EndOfMsgs;
                             end; { Last message }
                        end; { msgReply }
             msgEnter : begin
                          New(EditorInfo, Init(MaxMsgLines));

                          WriteMessage(Ra250MsgArea(MsgFound[Counter].BoardNr),
                                       '',
                                       '',
                                       '',
                                       '',
                                       False,   { reply }
                                       false,   { edit }
                                       EditorInfo^,
                                       0,
                                       MsgNrWritten,
                                       False,
                                       True,
                                       '',
                                       GetAreaAddress(Ra250MsgArea(MsgFound[Counter].BoardNr)),
                                       '', '', '', false, true, '', '', '');

                          Dispose(EditorInfo, Done);
                        end; { msgEnter }
             msgStop  : BREAK;
             msgDelete: begin
                            If GlobalCfg^.RaConfig^.ConfirmMsgDeletes then
                             If InputObj^.ralStrYesNoAsk(ralSure) then
                               ProcessAllMessages(True, Counter);

                            If NOT GlobalCfg^.RaConfig^.ConfirmMsgDeletes then
                             ProcessAllMessages(True, Counter);

                            Inc(Counter);
                            If Counter>CurMsgsFound then
                              begin
                                WriteLn;
                                Writeln;
                                Writeln(LangObj^.ralGet(ralEndMsgs));
                                InputObj^.PressEnter(False, True);
                                GOTO EndOfMsgs;
                               end; { Last message }
                          end; { Delete }
            msgRdReply: ; { Not implemented yet }
             msgFiles : ; { Not implemented yet }
           end; { Case }

         UNTIL (Counter>CurMsgsFound);
       end; { ReadMessage }

    If CH=LangObj^.ralGetKey(ralNo) then
        begin;
          WriteLn(LangObj^.ralGetStr(ralNo));
          BREAK;
        end; { No }

    If CH=LangObj^.ralGetKey(ralScan) then
        begin;
          WriteLn(LangObj^.ralGetStr(ralScan));
          Writeln;
          Writeln('`A15:',LangObj^.ralGet(ralScanning));
          Writeln;

          Result:=true;
          Counter := 01;
          While (Result) AND (Counter<=CurMsgsFound) do
           begin;
             Result := ShowScan(LineTeller, MsgFound[Counter].MsgNr,
                                            MsgFound[Counter].BoardNr,
                                            MsgFound[Counter].Date,
                                            MsgFound[Counter].Time,
                                            MsgFound[Counter].FromWho,
                                            MsgFound[Counter].ToWho,
                                            MsgFound[Counter].Subject,
                                            False,
                                            false);
             Inc(Counter);
           end; { While }
           WriteLn;
           WriteLn(LangObj^.ralGet(ralEndMsgs));
           InputObj^.PressEnter(False, True);
        end; { Scan }

    If CH=LangObj^.ralGetKey(ralQuickScan) then
        begin;
          WriteLn(LangObj^.ralGetStr(ralQuickScan));
          Writeln;
          Writeln('`A15:',LangObj^.ralGet(ralScanning));
          Writeln;
          ShowQuickScan(True, LineTeller, 00, '', '', '', false); { Show The header }

          Counter := 01;
          Result:=True;

          While (Result) AND (Counter<=CurMsgsFound) do
           begin;
            Result := ShowQuickScan(False, LineTeller, MsgFound[Counter].MsgNr,
                                                       MsgFound[Counter].FromWho,
                                                       MsgFound[Counter].ToWho,
                                                       MsgFound[Counter].Subject,
                                                       false);
            Inc(Counter);
           End; { While }

           WriteLn;
           WriteLn(LangObj^.ralGet(ralEndMsgs));
           InputObj^.PressEnter(False, True);
        end; { QuickScan }


    If CH=LangObj^.ralGetKey(ralKilling) then begin;
                                       WriteLn(LangObj^.ralGetStr(ralKilling));
                                       Writeln;
                                       Writeln;
                                       If GlobalCfg^.RaConfig^.ConfirmMsgDeletes then
                                        If InputObj^.ralStrYesNoAsk(ralSureKill)
                                         then ProcessAllMessages(True, -1);

                                       If NOT GlobalCfg^.RaConfig^.ConfirmMsgDeletes then
                                           ProcessAllMessages(True, -1);

                                       Break;
                                     end; { Kill }
    If CH=LangObj^.ralGetKey(ralMarking) then begin;
                                       WriteLn(LangObj^.ralGetStr(ralMarking));
                                       ProcessAllMessages(False, -1);
                                     end; { Marking as received }
  UNTIL (CH in [langObj^.ralGetKey(ralMarking), langObj^.ralGetKey(ralKilling), langObj^.ralGetKey(ralNo),
         langObj^.ralGetKey(ralYes)]);

EndOfMsgs:;
    LineCfg^.DispMorePrompt := SaveDisp;
end; { proc. AskForReadMail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function DoReadMail(const ReadWay     : MessageShowType;
                          BoardNr     : Word;
                    const DoForward   : Boolean;
                    const StartNr     : Longint;
                    const Individual,
                          MsgPause,
                          NewScan     : Boolean;
                    const SelectWay   : msgSelectType;
                    const SearchStr   : String;
                    const MarkMessages: Boolean;
                    const DoCombined  : Boolean;
                    var   Exitinfo    : ExitinfoRecord): MailReturnType;
var MsgRead     : AbsMsgPtr;
    SysAccess   : Boolean;
    MessageInf  : MessageRecord;
    EleMsgInf   : EleMessageRecord;
    LineTeller  : Byte;
    EditorInfo  : pStringArrayObj;
    ReplyLines  : Longint;
    TempBoard   : Word;
    LastNr      : LongInt;
    FromWho,
    ToWho,
    Subj,
    Address     : String[35];
    ReplyMsgNr  : Longint;
    MsgNrWritten: LongInt;
    MailReturn  : MailReturnType;
    SavePosition: Longint;

    ReplyKludge : String;
    ReplyTo,
    ReplyAddr   : String;
    ReplyDate   : String;

function DoShowMessage: Boolean;
begin
  DoShowMessage := True;

  If SelectWay in [selNormal] then Exit;

  Case SelectWay of
    selMarked : DoShowMessage := InMarkList(BoardNr, ReplyMsgNr);
      selFrom : DoShowMessage := Pos(SUpcase(SearchStr), SUpcase(MsgRead^.Getfrom))>00;
        selTo : DoShowMessage := Pos(SUpcase(SearchStr), SUpcase(MsgRead^.GetTo))>00;
      selSubj : DoShowMessage := Pos(SUpcase(SearchStr), SUpcase(MsgRead^.GetSubj))>00;
   selKeyword : begin;
                  DoShowMessage := False;

                  While NOT MsgRead^.EOM do
                      If Pos(SUpcase(SearchStr), SUpCase(MsgRead^.GetString(79)))>00 then
                        begin;
                          DoShowMessage := True;
                          Break;
                        end; { if pos }
                end; { pos }
  End; { case }

  MsgRead^.MsgTxtStartUp;                           { Reset to default values }
end; { func. DoShowMessage }

label EndOfMsgs;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailInt, 'DoReadMail');
    DebugObj.DebugLog(logMailInt, 'DoReadMail - StartNr = ' + FStr(StartNr));
    DebugObj.DebugLog(logMailInt, 'DoReadMail - BoardNr = ' + FStr(BoardNr));
  {$ENDIF}

  GetMessageRecord(MessageInf, BoardNr, True);
  GetEleMessageRecord(EleMsgInf, BoardNr, True);
  MailReturn := msgStop;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMailInt, 'DoReadMail - Area info: #'+FStr(MessageInf.Areanum) + ', '+MessageInf.Name);
  {$ENDIF}

  If NOT OpenOrCreateMsgArea(MsgRead, MakeMsgId(MessageInf.AreaNum, MessageInf.JamBase, MessageInf.Attribute,
                                                  EleMsgInf.Attribute)) then
      EXIT;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMailInt, 'DoReadMail - After OpenOrCreate()');
  {$ENDIF}

  If StartNr>00 then
    MsgRead^.SeekFirst(StartNr) else
     MsgRead^.SeekFirst(01);

  {-- Show the scanning message, unless its combined ----------------------}
  if NOT Docombined then
    begin
      WriteLn;
      WriteLn;
      Write('`A15:',LangObj^.ralGet(ralScanning));
      InputObj^.UpdateScrn;
      OutBlockObj^.DumpBlock;
    end; { if }

  LineTeller := 00;

  If ReadWay in [rdQuickScan] then
    ShowQuickScan(True, LineTeller, 00, '', '', '', DoCombined);

  MsgRead^.MsgStartUp;

  New(EditorInfo, Init(MaxMsgLines));

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailInt, 'DoReadMail - While SeekFound: '+Bool2Str(MsgRead^.SeekFound));
  {$ENDIF}

  {-- Make sure that the combined loop keeps running ------------------------}
  if DoCombined then
    if NOT MsgRead^.SeekFound then
      MailReturn := msgEndOfArea;

  {-- Start the loop --------------------------------------------------------}
  While MsgRead^.SeekFound do
      begin
        ReplyMsgNr := -1;
        SavePosition := MsgRead^.GetMsgNum;

        REPEAT
          MsgRead^.MsgStartUp;
          MsgRead^.MsgTxtStartUp;

          If ReplyMsgNr>-1 then
                begin
                  MsgRead^.SeekFirst(ReplyMsgNr);
                  If NOT MsgRead^.SeekFound then
                      MsgRead^.SeekFirst(SavePosition);
                end
                 else ReplyMsgNr := MsgRead^.GetMsgNum; { ReplyMsgNr }

          MailReturn := msgNext;

          If ReadWay in [rdNormal] then
           begin;
            If DoShowMessage then
              MailReturn := ReadMsgObj^.ReadMessage(BoardNr, ReplyMsgNr, MsgPause, LineTeller, ReplyMsgNr, false)
               else MailReturn := msgNext;

            Case MailReturn of
                msgAgain  : ; { Do nothing }
                msgNext   : break;
                msgLast   : begin
                              If DoForward then begin
                            {                    MsgRead^.SeekPrior; }
                                                MsgRead^.SeekPrior;
                                              end
                                else begin
                                      MsgRead^.SeekNext;
{                                      MsgRead^.SeekNext; }
                                    end; { SeekNext }
                               (* Dit wordt dubbel gedaan, omdat anders de
                               bewerking ongedaan wordt gedaan (onder Until) *)
                               Break;
                            end; { last }
                msgForward: begin
                              ForwardThisMessage(MsgRead^.GetMsgNum, MessageInf, Exitinfo);
                            end; { if }
                 msgReply : begin
                              BoardNr := MessageInf.AreaNum;
                              TempBoard := BoardNr;

                              ReplyToMsg(EditorInfo^,
                                         ReplyLines,
                                         Ra250MsgArea(BoardNr),
                                         MsgRead^.GetMsgNum,
                                         FromWho,
                                         ToWho,
                                         Subj,
                                         Address,
                                         ReplyTo,
                                         ReplyAddr,
                                         ReplyKludge,
                                         ReplyDate,
                                         false);

                              If GlobalCfg^.RaConfig^.AllowNetmailReplies then
                               if (NOT (MessageInf.Typ in [LocalMail, Internet, Newsgroup, Netmail])) then
                                If InputObj^.ralStrYesNoAsk(ralNetReply) then
                                 begin
                                   if MessageInf.NetMailArea = 00 then
                                     TempBoard := SearchNetmailBoard
                                      else TempBoard := MessageInf.NetMailArea;
                                 end; { if }

                              TempBoard := TempBoard;
                              if
                              WriteMessage(Ra250MsgArea(TempBoard),
                                           FromWho,
                                           Exitinfo.Userinfo.Name,
                                           Subj,
                                           '',
                                           True,        { reply }
                                           false,       { edit }
                                           EditorInfo^,
                                           ReplyLines,
                                           MsgNrWritten,
                                           False,
                                           True,
                                           ToWho,
                                           GetAreaAddress(Ra250MsgArea(BoardNr)),
                                           Address, ReplyTo, ReplyAddr, false, true, ReplyDate, '',
                                           ReplyKludge) then
                                            begin
                                              If TempBoard=BoardNr then
                                                SetReplyNr(Ra250MsgArea(BoardNr),
                                                           MsgRead^.GetMsgNum,
                                                           MsgNrWritten);
                                            end; { if }
                            end; { msgReply }
                 msgEdit  : begin
                              BoardNr := MessageInf.AreaNum;
                              TempBoard := BoardNr;

                              ReplyToMsg(EditorInfo^,
                                         ReplyLines,
                                         Ra250MsgArea(BoardNr),
                                         MsgRead^.GetMsgNum,
                                         FromWho,
                                         ToWho,
                                         Subj,
                                         Address,
                                         ReplyTo,
                                         ReplyAddr,
                                         ReplyKludge,
                                         ReplyDate,
                                         true);              { IsEdtting }

                              TempBoard := TempBoard;
                              MsgNrWritten := MsgRead^.GetMsgNum;

                              WriteMessage(Ra250MsgArea(TempBoard),
                                           FromWho,
                                           Exitinfo.Userinfo.Name,
                                           Subj,
                                           '',
                                           false,       { reply }
                                           true,        { edit }
                                           EditorInfo^,
                                           ReplyLines,
                                           MsgNrWritten,
                                           False,       { add warnmsg }
                                           false,       { allow attaches }
                                           ToWho,       { towho }
                                           GetAreaAddress(Ra250MsgArea(BoardNr)),
                                           Address,
                                           '',          { replyto }
                                           '',          { replyaddr }
                                           false,       { forwardmsg }
                                           false,       { ask cc }
                                           '',          { replydate }
                                           '',          { orig arestr }
                                           '');         { reply kludge }
                            end; { msgEdit }
                msgEnter  : WriteMessage(Ra250msgArea(BoardNr),
                                         '',
                                         '',
                                         '',
                                         '',
                                         False, { reply }
                                         false, { edit }
                                         EditorInfo^,
                                         00,
                                         MsgNrWritten,
                                         False,
                                         True,
                                         '',
                                         GetAreaAddress(Ra250MsgArea(BoardNr)),
                                         '', '', '', false, true, '', '', '');
                msgStop   : Goto EndOfMsgs;
                msgDelete : begin
                                 If GlobalCfg^.RaConfig^.ConfirmMsgDeletes then
                                  If InputObj^.ralStrYesNoAsk(ralSure) then
                                   DeleteMsg(MessageInf, EleMsgInf, MsgRead^.GetMsgNum, False, Exitinfo);

                                 if NOT GlobalCfg^.RaConfig^.ConfirmMsgDeletes then
                                   DeleteMsg(MessageInf, EleMsgInf, MsgRead^.GetMsgNum, False, Exitinfo);

                                 if NOT DoForward then
                                   begin
                                     MsgRead^.Seekprior;
                                     MsgRead^.MsgStartup;
                                   end; { if }
                               end; { Delete }
                msgRdReply: begin
                            end; { Start Reading a reply }
                msgFiles  : ;
            End; { Case }
           End; { If }

           if ReplyMsgNr >= 00 then
               MsgRead^.SeekFirst(ReplyMsgNr)
                else MsgRead^.SeekFirst(MsgRead^.GetMsgNum);

           If NOT (ReadWay in [rdNormal]) then Break;
        Until (True=False);

        MsgRead^.MsgStartUp;

        LastNr := MsgRead^.GetMsgNum;

        {-- Check if we have SysOp access -------------------------------------}
        SysAccess := SysOpAccess(MessageInf, Exitinfo.UserInfo);

        If ReadWay in [rdQuickScan] then
         If DoShowMessage then
          if ReadMsgAccess(MsgRead^.GetTo, SysAccess, MsgRead^.IsPriv, Exitinfo.userinfo) then
           If NOT ShowQuickScan(false, Lineteller, MsgRead^.GetMsgNum,
                                MsgRead^.GetFrom, MsgRead^.GetTo, MsgRead^.GetSubj,
                                DoCombined)
             then Break;

        If ReadWay in [rdScan] then
         If DoShowMessage then
          if ReadMsgAccess(MsgRead^.GetTo, SysAccess, MsgRead^.IsPriv, Exitinfo.userinfo) then
           If NOT ShowScan(Lineteller, MsgRead^.GetMsgNum,
                           BoardNr, MsgRead^.GetDate, MsgRead^.GetTime,
                           MsgRead^.GetFrom, MsgRead^.GetTo, MsgRead^.GetSubj,
                           MarkMessages,
                           DoCombined) then Break;

        If (NOT (MailReturn in [msgLast, msgReply, msgAgain, msgEdit])) then
         begin
           If DoForward then MsgRead^.SeekNext
             else MsgRead^.SeekPrior;
         end; { mailReturn }
        MsgRead^.MsgStartUp;

        if MailReturn <> msgLast then
         If (LastNr=MsgRead^.GetMsgNum) then Break;

        If Individual then Break;
      end; { While SeekFound }


  if NOT DoCombined then
    begin
      WriteLn('`A7:');
      WriteLn;

      If NOT NewScan then WriteLn(LangObj^.ralGet(ralEndMsgs))
        else WriteLn('`A15:',LangObj^.ralGet(ralEndNewMsg));
      InputObj^.PressEnter(False, True);
    end; { if }

EndOfMsgs: begin
              CloseMsgArea(MsgRead);
              Dispose(EditorInfo, Done);
              DoReadMail := MailReturn;
           end; { Exit }
end; { proc. DoReadMail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GetReadType(var TempCH: Char);
var Question    : QuestObj;
    elxObj      : pElxBbsObj;
    Goodkeys    : CharSet;
begin
  REPEAT
    TempCH := #01;

    {------------------------ Execute the read script -----------------------}
    if (LineCfg^.AnsiOn) OR (LineCfg^.AvatarOn) then
      begin
        if GetScriptType('READTYPE') = 'Q-A' then
          begin
            Question.Init;
            Question.Process('READTYPE /N', true, '');

            if Question.GetError = 00 then
              begin
                TempCH := UpCase(FirstChar(Question.GetReturnResult));
              end; { if }

            Question.Done;
          end
            else begin
                   New(elxObj, Init);
                   if elxObj^.RunElexerScript('readtype', '', false) then
                     begin
                       TempCH := UpCase(FirstChar(elxObj^.GetElxReturnString));
                     end
                       else begin
                              TempCH := #01;
                            end; { else }
                   Dispose(elxObj, Done);
                 end; { else }
      end; { if }

    {------------------- Convert all valid keys to an charset ---------------}
    Str2Set(LangObj^.ralGetKey(ralForward2)+
            LangObj^.ralGetKey(ralReverse1)+
            LangObj^.ralGetKey(ralIndivid1)+
              {Not active: == LangObj^.ralGetKey(ralHelp3)+ }
            LangObj^.ralGetKey(ralMarked1)+
            LangObj^.ralGetKey(ralNewMsgs1)+
            LangObj^.ralGetKey(ralSelected)+
            LangObj^.ralGetKey(ralQuit4), GoodKeys);

    {--------------- Check to see wether the script was successfull ---------}
    if TempCH = #01 then
      begin
        WriteLn('`A3:');
        WriteLn(LangObj^.ralGetStr(ralForward2), ',  ',
                LangObj^.ralGetStr(ralReverse1), ', ',
                LangObj^.ralGetStr(ralIndivid1), ', ',
                LangObj^.ralGetStr(ralHelp3), ',');
        WriteLn(LangObj^.ralGetStr(ralMarked1), ', ',
                LangObj^.ralGetStr(ralNewMsgs1), ',   ',
                LangObj^.ralGetStr(ralSelected), ', ',
                LangObj^.ralGetStr(ralQuit4), '.');
        Write('`A15:',LangObj^.ralGet(ralSelect));

        REPEAT
          TempCH := UpCase(InputObj^.ReadKey);
        UNTIL (TempCH in GoodKeys) OR (TempCH = LangObj^.ralGetKey(ralHelp3)) OR (Progterminated);
      end; { if }

    {------------------ Let''s show the user what hes chosen ----------------}
    if TempCH = LangObj^.ralGetKey(ralForward2) then
      begin
        WriteLn(LangObj^.ralGet(ralForward3));
      end; { Forward }

    if TempCH = LangObj^.ralGetKey(ralReverse1) then
      begin
        WriteLn(LangObj^.ralGet(ralReverse2));
      end; { Reverse }

    if TempCH = LangObj^.ralGetKey(ralIndivid1) then
      begin
        WriteLn(LangObj^.ralGet(ralIndivid2));
      end; { Individual }

    if TempCH = LangObj^.ralGetKey(ralHelp3) then
      begin
        WriteLn(LangObj^.ralGet(ralHelp1));
        DisplayHotFile('READHELP', []);
      end; { Help }

    if TempCH = LangObj^.ralGetKey(ralMarked1) then
      begin
        WriteLn(LangObj^.ralGet(ralMarked2));
      end; { Marked }

    if TempCH = LangObj^.ralGetKey(ralNewMsgs1) then
      begin
        WriteLn(LangObj^.ralGet(ralNewMsgs2));
      end; { New messages }

    if TempCH = LangObj^.ralGetKey(ralSelected) then
      begin
        WriteLn(LangObj^.ralGet(ralSelRead));
      end; { Selected }

    if TempCH = LangObj^.ralGetKey(ralQuit4) then
      begin
        WriteLn(LangObj^.ralGet(ralQuit3));
        BREAK;
      end; { Quit }

  UNTIL (TempCH in GoodKeys);
end; { proc. GetReadType }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GetSelectedInfo(var SearchStr: String;
                          var SelectWay: msgSelecttype;
                          var TempCH   : Char);
var SelCH         : Char;
    SelGoodKeys   : CharSet;
    Question      : QuestObj;
    elxOBj        : pElxBbsObj;
begin
    {-------------------- Handle upon what the user has chosen ----------------}
    if TempCH = LangObj^.ralGetKey(ralSelected) then
      begin
        WriteLn;

        {------------------- Execute the "selected read" script ---------------}
        SelCH := #01;
        if (LineCfg^.AnsiOn) OR (LineCfg^.AvatarOn) then
          begin
            if GetScriptType('RDSELECT') = 'Q-A' then
              begin
                Question.Init;
                Question.Process('RDSELECT /N', true, '');

                if Question.GetError = 00 then
                  begin
                    SelCH := UpCase(FirstChar(Question.GetReturnResult));
                  end; { if }
                Question.Done;
              end
                else begin
                       New(elxObj, Init);
                       if elxObj^.RunElexerScript('rdselect', '', false) then
                         begin
                           SelCH := UpCase(FirstChar(elxObj^.GetElxReturnString));
                         end
                           else begin
                                  SelCH := #01;
                                end; { else }
                       Dispose(elxObj, Done);
                     end; { else }
          end; { if }

        {--------------------- Show the user what to select from --------------}
        if SelCH = #01 then
          begin
            WriteLn('`A11:', LangObj^.ralGet(ralCreteria));
            WriteLn('`A11:', LangObj^.ralGetStr(ralFromName1), ', ', LangObj^.ralGetStr(ralToName3),', ',
                             LangObj^.ralGetStr(ralSubject5),  ', ', LangObj^.ralGetStr(ralKeyWord),', ',
                             LangObj^.ralGetStr(ralQuit5));
            WriteLn;
            Write('`A15:',LangObj^.ralGet(ralSelect));
            Write('`F',GlobalCfg^.RaConfig^.BarFore,':`B',GlobalCfg^.RaConfig^.BarBack,':');
          end; { if }

        SelGoodKeys := [];
        Str2Set(LangObj^.ralGetKey(ralFromName1)+
                LangObj^.ralGetKey(ralToName3)+
                LangObj^.ralGetKey(ralSubject5)+
                LangObj^.ralGetKey(ralKeyWord)+
                LangObj^.ralGetKey(ralQuit5), SelGoodKeys);

        {-------------- if the script failed, get the correct key ------------}
        if SelCH = #01 then
          REPEAT
             SelCH := UpCase(InputObj^.ReadKey);
          UNTIL (SelCH in SelGoodKeys) OR (ProgTerminated);

        {------------------ Now handle upon what the user choose -------------}
        if SelCH = LangObj^.ralGetKey(ralQuit5) then
        begin
          EXIT;
        end; { if }

        if SelCH = LangObj^.ralGetKey(ralFromName1) then
          begin
            WriteLn(LangObj^.ralGet(ralFromName2), '`A15:');
            WriteLn('`A3:');

            Write(LangObj^.ralGet(ralFromSrc));
            SearchStr := '';
            GetString(SearchStr, 35, [#32..#254], true, false, false, false);
            WriteLn;

            if SearchStr = '' then
              begin
                EXIT;
              end; { if }

            SelectWay := selFrom;
          end; { fromName }

        if SelCH = LangObj^.ralGetKey(ralToName3) then
          begin
            WriteLn(LangObj^.ralGet(ralToName4), '`A15:');
            WriteLn('`A3:');

            Write(LangObj^.ralGet(ralToSrc));
            SearchStr := '';
            GetString(SearchStr, 35, [#32..#254], true, false, false, False);
            WriteLn;

            if SearchStr = '' then
              begin
                EXIT;
              end; { if }

            SelectWay := selTo;
          end; { ToName }

        if SelCH = LangObj^.ralGetKey(ralSubject5) then
          begin
            WriteLn(LangObj^.ralGet(ralSubject6), '`A15:');
            WriteLn('`A3:');

            Write(LangObj^.ralGet(ralSubjSrc));
            SearchStr := '';
            GetString(SearchStr, 35, [#32..#254], false, false, false, false);
            WriteLn;

            if SearchStr = '' then
              begin
                EXIT;
              end; { if }

            SelectWay := selSubj;
          end; { Subject }

        if SelCH = LangObj^.ralGetKey(ralKeyWord) then
          begin
            WriteLn(LangObj^.ralGet(ralKeyWord2), '`A15:');
            WriteLn('`A3:');

            SearchStr := '';
            Write(LangObj^.ralGet(ralSrchStr));
            GetString(SearchStr, 35, [#32..#254], false, false, false, false);
            WriteLn;

            if SearchStr = '' then
              begin
                EXIT;
              end; { if }

            SelectWay := selKeyword;
          end; { KeyWord }
      end; { Selected Read }
end; { proc. GetSelectedinfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ReadMessages(    ReadWay  : MessageShowType;
                           MiscData : String;
                       var Exitinfo : ExitinfoRecord);
var SearchStr     : String;
    SelectWay     : msgSelectType;
    TempCH        : Char;
    Combined      : Combinedrecord;
    MessageInf    : MessageRecord;
    EleMsgInf     : EleMessageRecord;
    CurCombined   : Longint;
    MailReturn    : MailReturnType;
    DoCombined    : Boolean;
    DoAbort       : Boolean;
    MarkedMsgs    : Boolean;
    MsgPause      : Boolean;
    SkipThisArea  : Boolean;
    FirstArea     : Boolean;

    Counter       : Longint;
    MsgArea       : Longint;
    StartNr       : Longint;

    HighMsgNum    : Longint;
    ActiveMsgNum  : Longint;
    FirstMsgNum   : Longint;
    LastRead      : Longint;
    Question      : QuestObj;
    OverrideMsgNr : Longint;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMailInt, 'ReadMessages - BEGIN (MiscData='+MiscData+')');
  {$ENDIF}

  {---------------------- Initialize all variables first ----------------------}
  FillChar(Combined, SizeOf(Combined), 0);
  SelectWay    := selNormal;
  SearchStr    := '';
  DoAbort      := FALSE;
  CurCombined  := 01;
  FirstArea    := TRUE;
  OverrideMsgNr:= -1;


  if MiscData = '' then
    MsgArea := Exitinfo.Userinfo.MsgArea;

  if Pos('/M', SUpCase(MiscData)) > 00 then
    MsgArea := Exitinfo.Userinfo.MsgArea
     else MsgArea := FVal(FirstWord(MiscData, defExtractWord, false));

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMailInt, 'ReadMessages - MsgArea = ' + FStr(MsgArea));
  {$ENDIF}

  if MsgArea = 0 then
    begin
      DoCombined := true;

      CurCombined := 00;
      for Counter := 01 to 200 do
        if Exitinfo.Userinfo.CombinedInfo[Counter] <> 0 then
          begin
            Inc(CurCombined);
            Combined[CurCombined] := Exitinfo.Userinfo.CombinedInfo[Counter];
          end; { if }

      RaLog('>', 'Reading message area #0 : COMBINED');
      CurCombined := 01;
    end
       else begin
              DoCombined := false;
              GetMessageRecord(MessageInf, MsgArea, true);
              GetEleMessageRecord(EleMsgInf, MsgArea, true);
              Combined[1] := MessageInf.AreaNum;

              RaLog('>', 'Reading message area #'+FStr(MessageInf.AreaNum)+' : '+Messageinf.Name)
            end; { if }

  {---------------- Fire up the loop for all areas (if combined) --------------}
  REPEAT
    SkipThisArea := FALSE;
    GetMessageRecord(MessageInf, Combined[CurCombined], true);
    GetEleMessageRecord(EleMsgInf, Combined[CurCombined], true);

    if (MessageInf.AreaNum = 0) OR (MessageInf.Name = '') then
      SkipThisArea := TRUE;
    MsgArea := MessageInf.AreaNum;

    {$IFDEF WITH_DEBUG}
       DebugObj.DebugLog(logMailInt, 'ReadMessages - MessageInf.Name = ' + MessageInf.Name);
       DebugObj.DebugLog(logMailInt, 'ReadMessages - SkipThisArea = ' + Bool2Str(SkipThisArea));
    {$ENDIF}

    {--------------- Make sure we have access to this area --------------------}
    if (NOT SkipThisArea) then
      begin
        if (NOT (CheckFlagAccess(Exitinfo.Userinfo.Flags, MessageInf.ReadFlags, MessageInf.ReadNotFlags))) OR
            (MessageInf.ReadSecurity > Exitinfo.Userinfo.Security) then
              begin
                if NOT DoCombined then
                  begin
                    WriteLn;
                    WriteLn(LangObj^.ralGet(ralNoReadAcc));
                    InputObj^.PressEnter(False, True);

                    EXIT;
                  end { Read Messages }
                   else SkipThisArea := true;
              end; { if }
      end; { SkipThisArea }

    {---------------- Check to see if there are messages in there -------------}
    GetMsgAreaStats(MessageInf, EleMsgInf, ActiveMsgNum, FirstMsgNum, HighMsgNum);

    {$IFDEF WITH_DEBUG}
       DebugObj.DebugLog(logMailInt, 'ReadMessages - GetMsgAreaStats = ' + FStr(ActiveMsgNum));
    {$ENDIF}

    if NOT SkipThisArea then
      begin
        if ActiveMsgNum = 0 then
          begin
            if NOT DoCombined then
               begin
                 WriteLn('`A12:');
                 Writeln(LangObj^.ralGet(ralNoMsgs));
                 InputObj^.PressEnter(False, True);

                EXIT;
              end { No messages }
               else SkipThisArea := TRUE;
          end; { if }
      end; { else }

    {------------------- Now, let''s ask this user for the way to read --------}
    if (NOT SkipThisArea) AND (FirstArea) then
      begin
        GetReadType(TempCH);

        {$IFDEF WITH_DEBUG}
          DebugObj.DebugLog(logMailInt, 'ReadMessages - GetReadType = ' + TempCH);
          DebugObj.DebugLog(logMailInt, 'ReadMessages - QuitKey = ' + LangObj^.ralGetKey(ralQuit4));
          DebugObj.DebugLog(logMailInt, 'ReadMessages - SelectedKey = ' + LangObj^.ralGetKey(ralSelected));
        {$ENDIF}

        if TempCH = LangObj^.ralGetKey(ralQuit4) then EXIT;  { Exit if chosen }

        {--------------- Get the info we want from selected messages --------------}
        if TempCH = LangObj^.ralGetKey(ralSelected) then
          begin
            GetSelectedInfo(SearchStr,
                            SelectWay,
                            TempCH);
            if SearchStr = '' then EXIT;
          end; { if }


        {---------- Let''s see if we can get an message# from the user --------}
        if (TempCH in [LangObj^.ralGetKey(ralForward2), LangObj^.ralGetKey(ralReverse1),
                       LangObj^.ralGetKey(ralIndivid1), LangObj^.ralGetKey(ralNewMsgs1),
                       LangObj^.ralGetKey(ralSelected), LangObj^.ralGetKey(ralMarked1)]) then
          begin
            WriteLn('`A11:', LangObj^.ralGet(ralMsgArea),' "', MessageInf.Name,'" ',
                             LangObj^.ralGet(ralContains), ' ', ActiveMsgNum, ' ',
                             LangObj^.ralGet(ralMessages2));
            WriteLn('`A15:', LangObj^.ralGet(ralMsgRange), ' 1 - ', HighMsgNum);
          end; { if }

        {--------------------------- Actually read now ----------------------------}
        if (TempCH in [LangObj^.ralGetKey(ralForward2), LangObj^.ralGetKey(ralReverse1),
                       LangObj^.ralGetKey(ralIndivid1), LangObj^.ralGetKey(ralNewMsgs1),
                       LangObj^.ralGetKey(ralSelected), LangObj^.ralGetKey(ralMarked1)]) then
          begin
            {----------------------- Get a message number from the user -----------}
            if TempCH <> LangObj^.ralGetKey(ralNewMsgs2) then
              begin
                if (NOT (TempCH in [LangObj^.ralGetKey(ralIndivid2)])) then
                  begin
                    RangeEdit(StartNr, '`A15:'+LangObj^.ralGet(ralMsgStart), 1, HighMsgNum, 00)
                  end
                    else begin
                           RangeEdit(StartNr, '`A15:'+LangObj^.ralGet(ralNr2Read), 1, HighMsgNum, 00);
                         end; { else }
              end; { not new messages }

             Write('`A15:');
             OutputObj^.ResetLines(01);

             {----------------------- Does the user wants pausing? ----------------}
             if ReadWay in [rdNormal] then
               begin
                 if InputObj^.ralStrYesNoAsk(ralPause) then MsgPause := TRUE
                   else MsgPause := FALSE;
                end
                  else MsgPause := FALSE;

             {---------------- Does the user want to read "marked" messages? ------}
             MarkedMsgs := FALSE;

             if TempCH <> LangObj^.ralGetKey(ralMarked1) then   { Isn't reading marked msgs }
               if (ReadWay in [rdScan]) then
                 if InputObj^.ralStrYesNoAsk(ralMark) then
                   begin
                     MarkedMsgs := TRUE;

                     if LineCfg^.CurMsgsMarked > 00 then
                       if InputObj^.ralStrYesNoAsk(ralClrMark) then
                         LineCfg^.CurMsgsMarked := 00;
                   end; { Mark Messages }

             if TempCH = LangObj^.ralGetKey(ralMarked1) then Selectway := selMarked;
          end; { if TempCH }
      end; { if not first and not skipping this area }


    {------------------- Now, let''s do the actual reading --------------------}
    if (NOT SkipThisArea) then
      if (TempCH in [LangObj^.ralGetKey(ralForward2), LangObj^.ralGetKey(ralReverse1),
                     LangObj^.ralGetKey(ralIndivid1), LangObj^.ralGetKey(ralNewMsgs1),
                     LangObj^.ralGetKey(ralSelected), LangObj^.ralGetKey(ralMarked1)]) then
        begin
          {$IFDEF WITH_DEBUG}
             DebugObj.DebugLog(logMailInt, 'ReadMessages - User wants new messages!');
          {$ENDIF}

          LastRead := GetLastReadNum(MessageInf);

          {$IFDEF WITH_DEBUG}
             DebugObj.DebugLog(logMailInt, 'ReadMessages - MsgArea = ' + MessageInf.Name);
             DebugObj.DebugLog(logMailInt, 'ReadMessages - LastReadPtr = ' + FStr(LastRead));
          {$ENDIF}

          FirstArea := FALSE;

          {------------- Find out exactly which messagenumber will be doing ----}
          if (StartNr = 0) AND (UpCase(TempCH) = LangObj^.ralGetKey(ralReverse1)) then
            StartNr := HighMsgNum;

          if (StartNr = 00) AND (UpCase(TempCH) = LangObj^.ralGetKey(ralIndivid2)) then
            EXIT;

          if (UpCase(TempCH) = LangObj^.ralGetKey(ralNewMsgs2)) then
            StartNr := LastRead + 1; { was without +1 }

          {$IFDEF WITH_DEBUG}
             DebugObj.DebugLog(logMailInt, 'ReadMessages - LastReadPtr (after handling) = ' + FStr(StartNr));
          {$ENDIF}

          if OverrideMsgNr <> -1 then StartNr := OverrideMsgNr;
          OverrideMsgNr := -1;

          {$IFDEF WITH_DEBUG}
             DebugObj.DebugLog(logMailInt, 'ReadMessages - After override = ' + FStr(StartNr));
             DebugObj.DebugLog(logMailInt, 'ReadMessages - MsgArea = ' + FStr(MsgArea));
          {$ENDIF}

          {---------------------- Actually, do the mail reading ----------------}
          MailReturn :=
              DoReadMail(ReadWay, MsgArea,
                         TempCH in [LangObj^.ralGetKey(ralForward2), LangObj^.ralGetKey(ralNewMsgs1),
                                    LangObj^.ralGetKey(ralSelected), LangObj^.ralGetKey(ralMarked1)],
                         StartNr,
                         TempCH in [LangObj^.ralGetKey(ralIndivid2)],
                         MsgPause,
                         TempCH = LangObj^.ralGetKey(ralNewMsgs2),
                         SelectWay,
                         SearchStr,
                         MarkedMsgs,
                         DoCombined,
                         Exitinfo);

          if ReadWay <> rdNormal then
            if OutputObj^.StopMore then CurCombined := 201;

          Case MailReturn of
             msgStop : CurCombined := 201;
             msgLast : if (CurCombined - 1) < 1 then
                         begin
                           BREAK;
                         end
                           else begin
                                  Dec(CurCombined, 3);
                                end; { else }
          end; { case }

        end; { Forward or reverse }

    REPEAT
      Inc(CurCombined);
    UNTIL (CurCombined > 200) OR (Combined[CurCombined] <> 0);


    {----------------- Make sure we start at the last mesasge --------------}
    if MailReturn = msgLast then
     if CurCombined < 200 then
      begin
        GetMsgAreaStats(MessageInf, EleMsgInf, ActiveMsgNum, FirstMsgNum, HighMsgNum);
        GetMessageRecord(MessageInf, Combined[CurCombined], true);
        GetEleMessageRecord(EleMsgInf, Combined[CurCombined], true);
        OverrideMsgNr := HighMsgNum;
      end; { if }

  UNTIL (DoAbort) OR (CurCombined > 200);

  if DoCombined then
    begin
      if (NOT (TempCH = LangObj^.ralGetKey(ralNewMsgs2))) then
        WriteLn(LangObj^.ralGet(ralEndMsgs))
          else WriteLn('`A15:',LangObj^.ralGet(ralEndNewMsg));

      InputObj^.PressEnter(false, true);
    end; { if }

  {$IFDEF WIT_DEBUG}
    DebugObj.DebugLog(logMailInt, 'EO ReadMessages');
  {$ENDIF}
end; { proc. ReadMessages }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
{$ENDIF}

end.
