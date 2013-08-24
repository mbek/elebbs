unit WriteMsg;
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
** Writing a message routines for EleBBS
**
** Copyright (c) 1996-1998 by Maarten Bekers
**
** Created : 03-Jan-1998
** Last update : 03-Jan-1998
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
uses Global, CfgRec, StrUnit;

{$IFNDEF WITH_FULL}
  This unit shouldn''t be compiled for an utility program
{$ENDIF}


function  WriteMessage(BoardNr: Word;
                       ToWho, FromWho, Subject: String;
                       MiscData: String;
                       ReplyMsg: Boolean;
                       EditMsg : Boolean;
                       var QuoteText: StringArrayObj;
                       QuoteLines: Longint;
                       var MsgNrWritten: LongInt;
                       AddWarnMsg,
                       AllowAttaches: Boolean;
                       OrigToWho: String;
                       FromAddress,
                       ToAddress: String;
                       ReplyTo, ReplyAddr: String;
                       ForwardMsg,
                       AskCarbonCopy: Boolean;
                       OrigDateStr, OrigAreaStr: String;
                       ReplyKludge: String): Boolean;
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
uses FileRout, MailInt, Mail, StUtils, ObjDec, {$IFDEF WITH_FULL}Transfer,{$ENDIF} BitWise, Ral, LineEd,
      DispAns, MenuFunc, Control, Colors, Support, Debug_U, Input_U,
       ElLog_U, Access_U, ExecFunc, IntEdit{$IFDEF WIn32},SysUtils{$ENDIF},
        Cases, LongStr, CentrStr, WordStr, StrPath, MemMan, Limit_U, GenFile,
         User_U, ScrList, FileObj, Fsed, Ranges,
          NodeLst, JDates;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ReadNamesFile(CtlName: String;
                        SearchFor: String;
                        var ToWho, Address, Subject: String);
var Ctl_F  : Text;
    TempStr: String;
    TmpBuf : Array[0..8191] of Char;
begin
  ToWho := '';
  Address := '';
  Subject := '';

  Assign(Ctl_F, OpenRaFile(CtlName));
  SetTextBuf(Ctl_F, TmpBuf);
  {$i-} System.Reset(Ctl_F); {$I+}
  If IOResult>00 then EXIT;

  While NOT Eof(Ctl_F) do
    begin
      {$i-} ReadLn(Ctl_F, TempStr); {$I+}
      If IOResult>00 then BREAK;

      If SUpcase(Trim(GetCommaArgument(TempStr, 01)))=SUpcase(Trim(Copy(SearchFor, 2, 255))) then
         begin
           ToWho := GetCommaArgument(TempStr, 2);
           Address := GetCommaArgument(TempStr, 3);
           Subject := GetCommaArgument(TempStr, 4);
         end; { if }

    end; { while }

  {$i-} Close(Ctl_F); {$I+}
  If IOResult>00 then ;
end; { proc. ReadNamesFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoInternalFSED(var MsgInfo: EditorMsgInfoRec);
var TextLines  : pStringArrayObj;
    QuoteLines : pStringArrayObj;
    NrQuotes   : Longint;
    Temp_F     : pFileObj;
    Counter    : Longint;
    LinesDone  : Longint;
    TempStr    : String;
    FSedObj    : pFsedObj;
begin
  New(TextLines, Init(MaxEditLines));
  New(QuoteLines, Init(MaxQuoteLines));
  NrQuotes := 0;

  {--------------------------- Read in the reply texts ------------------------}
  New(Temp_F, Init);
  Temp_F^.Assign('msgtmp');
  if Temp_F^.Open(1) then
    begin

      While NOT Temp_F^.EOF do
        begin
          Temp_F^.ReadLn(TempStr);
          Inc(NrQuotes);

          QuoteLines^.Put(NrQuotes, TempStr);
        end; { while }

    end; { if }
  Dispose(Temp_F, Done);


  {----------------------------- Do the actual editting -----------------------}
  FsedObj := NIL;
  New(FsedObj, Init);
  if FsedObj <> nil then
    begin
       LinesDone := FsedObj^.FullScreenEditor(TextLines, QuoteLines, (NrQuotes > 0),
                                              MsgInfo);
       Dispose(FsedObj, Done);
    end
      else begin
             RaLog('!', 'Unable to intiate fullscreen editor (memory problem)');
             LinesDone := 0;
           end; { else }

  {------------------------ Write the contents back to a file -----------------}
  if LinesDone > 0 then
    begin
      New(Temp_F, Init);
      Temp_F^.Assign('msgtmp');
      if Temp_F^.Create(1) then
        begin
          Counter := 0;

          for Counter := 1 to LinesDone do
            begin
              TempStr := TextLines^.Get(Counter);

              Temp_F^.WriteLn(TempStr);
              if Temp_F^.IoResult > 0 then EXIT;
            end; { if }
        end; { if }
      Dispose(Temp_F, Done);
    end
      else EraseFile('msgtmp');

  Dispose(TextLines, Done);
  Dispose(QuoteLines, Done);
end; { proc. DoInternalFSED }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function UploadMessage: Boolean;
var Uploads: Longint;
begin
  OutputObj^.ClearScreen;

  {$IFDEF WITH_FULL}
    EraseFile('msgtmp');
    DoUpload('', False, True, '', Uploads);
  {$ENDIF}

  UploadMessage := (Uploads>00);
end; { proc. UploadMessage }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AskFromWho(var FromWho: String; MessageInf: MessageRecord);
var InputLen: Longint;
begin
  If (ReadBit(Messageinf.Attribute, 5)) AND (LineCfg^.Exitinfo^.Userinfo.Handle <> '') then
    FromWho := LineCfg^.Exitinfo^.Userinfo.Handle else
     FromWho := LineCfg^.Exitinfo^.Userinfo.Name;

  if ReadBit(MessageInf.Attribute, 3) then                 { Allow aliases }
   if NOT ReadBit(MessageInf.Attribute, 5) then
    if InputObj^.ralStrYesNoAsk(ralUseAlias) then
        begin
          WriteLn;

          InputLen := 35;     { Hudson and Squid cant take more than 35 }
          if ReadBit(MessageInf.Attribute, 7) then
            InputLen := 65;                             { JAM can do 65 }

          repeat
            FromWho := LineCfg^.Exitinfo^.Userinfo.Handle;

            Write('`A10:',LangObj^.ralGet(ralName2Use));
            GetString(FromWho, InputLen, [#32..#254], GlobalCfg^.ElConfig^.CapitalizeUsername, False, False, False);
            WriteLn;
            If FromWho='' then Exit;

            If (SearchUser(FromWho)>-1) then
             If (SUpcase(FromWho)<>SUpcase(LineCfg^.Exitinfo^.Userinfo.Name)) AND
                 (SUpCase(FromWho)<>SUpCase(LineCfg^.Exitinfo^.Userinfo.Handle)) then
                   begin
                     WriteLn('`A12:', LangObj^.ralGet(ralInvAlias));
                     FromWho := '';
                   end; { Another one's alias }

          until FromWho<>'';

        end; { Pick an alias }
end; { proc. AskFromWho }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AskAddress(var Address: String; const MessageInf: MessageRecord);
var SaveAddr: String;
begin
  REPEAT
    Write('`A14:', LangObj^.ralGet(ralAddress));

    if Address<>'' then
      begin
        WriteLn('`A3:', Address);
        EXIT;
      end; { AskAdress }

    GetString(Address, 15, [#32..#254], True, False, False, False);
    if Address = '' then EXIT;

    {-- Skip a line --------------------------------------------------------}
    WriteLn('`A', MakeAttr(GlobalCfg^.RaConfig^.NormFore, GlobalCfg^.RaConfig^.NormBack), ':');

    {-- Now do a search, if necessary --------------------------------------}
    if Pos('?', Address) > 0 then
      HandleNodeListInput(Address, MessageInf.AkaAddress);

    {-- if it was a query, the Address is empty. else try a match ----------}
    if Address <> '' then
      begin
        {-- Backup it first ------------------------------------------------}
        SaveAddr := Address;

        {-- Match it, will return empty if its not known -------------------}
        MatchNodeList(Address, MessageInf.AkaAddress);

        {-- if its empty, ask to send anyway ------------------------------}
        if Address = '' then
          if InputObj^.ralStrYesNoAsk(ralNetSend) then
            begin
              Address := SaveAddr;
            end; { if }
      end; { if }

    WriteLn;
  UNTIL (Address<>'');
end; { proc. AskAddress }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


procedure AskToWho(var ToWho: String; var AddrStr: String; CheckUserList: Boolean; const MessageInf: MessageRecord;
                   var Subject: String; var MiscData: String);
var DoCap: Boolean;
    InputLen: Longint;
begin
  If (NOT (MessageInf.Typ in [NetMail])) OR (AddrStr='0:0/0') then
       AddrStr := '';

  if SUpCase(ToWho)='SYSOP' then
    if MessageInf.Typ <> Netmail then
      ToWho := GlobalCfg^.RaConfig^.SysOp;

  REPEAT
    Write('`A14:', LangObj^.ralGet(ralTo1));

    If ToWho<>'' then
      begin
        Write('`A03:', ToWho);
        if AddrStr <> '' then
          WriteLn(#32, LangObj^.ralGet(ralOn2), #32, '(', AddrStr, ')')
           else WriteLn;

        EXIT;
      end; { Write to-who-name on the screen, and add address when needed }

    InputLen := 35;     { Hudson and Squid cant take more than 35 }
    if ReadBit(MessageInf.Attribute, 7) then
      InputLen := 65;                             { JAM can do 65 }

    if ToWho='' then
       begin
         DoCap := (MessageInf.Typ <> Internet);
         if DoCap then
           DoCap := GlobalCfg^.Elconfig^.CapitalizeUsername;

         GetString(ToWho, InputLen, [#32..#254], DoCap, False, False, False);
         If ToWho='' then EXIT;

         if MessageInf.Typ in [NetMail] then
           begin
             WriteLn;
             AskAddress(AddrStr, MessageInf);

             if AddrStr = '' then
               begin
                 ToWho := ''; { invalidate whole message }
                 EXIT;
               end; { if }
           end; { netmail message }
       end; { if ToWho is exitted }


    if MessageInf.Typ = Internet then
      if (Pos('.', ToWho)=00) OR (Pos('@', ToWho)=00) then
           begin
             WriteLn;
             WriteLn;
             WriteLn('`A12:', langObj^.ralGet(ralInvInter),'`A3:');
             ToWho := '';
           end; { invalid }

    WriteLn;
    If (ToWho[1] in ['@', '=', '>', '<']) AND
        (GlobalCfg^.RaConfig^.GroupMailSec<=LineCfg^.Exitinfo^.Userinfo.Security) then EXIT;

    if ToWho[1] = '*' then
      ReadNamesFile('names.ctl', ToWho, ToWho, AddrStr, Subject);

    If (SupCase(ToWho)='SYSOP') AND (MessageInf.Typ=EchoMail) then
      begin
        if DisplayHotFile('NOTSYSOP', []) = #01 then
          begin
            WriteLn;
            WriteLn;
            Writeln(LangObj^.ralGet(ralNotSysOp));
            InputObj^.PressEnter(False, True);
          end; { if }

         ToWho := '';
         EXIT;
      end; { SysOp }

    if SUpCase(ToWho)='SYSOP' then
      if MessageInf.Typ <> Netmail then
        ToWho := GlobalCfg^.RaConfig^.SysOp;

    If SUpcase(ToWho)=SUpcase(GlobalCfg^.RaConfig^.SysOp) then EXIT else
     If ((SUpCase(ToWho)='ALL') AND (NOT (MessageInf.MsgKinds = elemsgPrivate))) then ToWho := 'All'
       else If CheckUserlist then
        begin
          WriteLn('`A3:',LangObj^.ralGet(ralVeriUser));

          If SearchUser(ToWho)=-1 then
            begin
              If Pos('/U', SupCase(MiscData))>00 then
                begin
                  WriteLn;
                  If InputObj^.ralStrYesNoAsk(ralInvAskBrw) then
                      begin
                        ShowUserList(MiscData);
                        WriteLn;
                        ToWho := '';
                      end; { Disable current name }
                end; { ShowUserlist }

              If Pos('/U', SUpcase(MiscData))=00 then
                begin
                  WriteLn('`X1:`E:',LangObj^.ralGet(ralInvUser));
                  ToWho := '';
                end; { ToWho }

            end; { User not found }
        end; { Scanning for that user }

  UNTIL (ToWho<>'');
end; { proc. AskToWho }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AskSubject(var Subject: String);
begin
  REPEAT
    Write('`A14:', LangObj^.ralGet(ralSubject1));

    if Subject <> '' then
      begin
        WriteLn('`A3:', Subject);
        EXIT;
      end; { AskSubject }

    If Subject='' then GetString(Subject, 72, [#32..#254], false, False, False, False);
    If Subject='' then Exit;

    WriteLn;
  UNTIL (Subject<>'');
end; { proc. AskSubject }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ChangeThings(const MessageInf: MessageRecord; var ToWho, AddrStr: String; var Subject, MiscData: String);
var CH: Char;
begin

 REPEAT
   WriteLn;
   Writeln('`A03:', LangObj^.ralGet(ralChngWhat));
   if MessageInf.Typ <> Newsgroup then Write(LangObj^.ralGetStr(ralToName1));
   Write(' ', LangObj^.ralGetStr(ralSubAb));

   CH := UpCase(InputObj^.ReadKey);
   if ProgTerminated then EXIT;

   TextColor(GlobalCfg^.RaConfig^.BarFore);
   TextBackground(GlobalCfg^.RaConfig^.BarBack);

   if MessageInf.Typ <> Newsgroup then
    If CH=LangObj^.ralGetKey(ralToName1) then
        begin
          WriteLn(LangObj^.ralGet(ralToName2));
          WriteLn('`A3:');

          ToWho := '';
          AddrStr := '';
          AskToWho(ToWho, AddrStr, MessageInf.Typ in [LocalMail], MessageInf, Subject, MiscData);
          If ToWho='' then BREAK;
        end; { To Name }

   If CH=LangObj^.ralGetKey(ralSubAb) then
       begin
         WriteLn(LangObj^.ralGet(ralSubjName));
         WriteLn('`A3:');

         Subject:= '';
         AskSubject(Subject);
         If SubJect='' then BREAK;
       end; { To Name }

   If CH=Copy(LangObj^.ralGetKeys(ralSubAb), 2, 1) then
       begin
         WriteLn(LangObj^.ralGet(ralAbort));
         WriteLn('`A3:');

         ToWho := '';        { To name is empty, EleBBS aborts the message }
         Subject := '';
         BREAK;
       end; { Abort }

   TextBackground(Black);
   TextColor(Cyan);
 UNTIL CH in [#13];

 WriteLn;
 WriteLn;
end; { proc. Changethings }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  WriteMessage(BoardNr: Word;
                       ToWho, FromWho, Subject: String;
                       MiscData: String;
                       ReplyMsg: Boolean;
                       EditMsg : Boolean;
                       var QuoteText: StringArrayObj;
                       QuoteLines: Longint;
                       var MsgNrWritten: LongInt;
                       AddWarnmsg,
                       AllowAttaches: Boolean;
                       OrigToWho: String;
                       FromAddress,
                       ToAddress: String;
                       ReplyTo, ReplyAddr: String;
                       ForwardMsg,
                       AskCarbonCopy: Boolean;
                       OrigDateStr, OrigAreaStr: String;
                       ReplyKludge : String): Boolean;


procedure ReadTmpFile(const FName      : String;
                      var   EditorInfo : StringArrayObj;
                      var   MsgLines   : Longint;
                      const TreatSoftCR: Boolean);
var Msg_F   : Text;
    TempStr : String;
    CH      : Char;
begin
  {-- Initialize all variables ----------------------------------------------}
  EditorInfo.Clear;
  MsgLines := 01;

  {-- Start reading all data ------------------------------------------------}
  Assign(Msg_F, FName);
  {$i-} System.Reset(Msg_F); {$i+}
  if IoResult > 0 then
    begin
      EXIT;
    end; { if }

  {-- Start the loop --------------------------------------------------------}
  TempStr := '';

  While (NOT Eof(Msg_F)) AND (MsgLines < EditorInfo.MaxStrings) do
    begin
      Read(Msg_F, CH);

      if NOT (CH in [#10, #13]) then
        TempStr := TempStr + CH;

      EditorInfo.Put(MsgLines, TempStr);

      if ((CH=#141) AND (TreatSoftCR)) OR (CH in [#10]) then
        begin
          TempStr := '';
          Inc(MsgLines);
        end; { if }

      if (CH=#32) then
       if Length(TempStr) > 120 then
        begin
          TempStr := '';
          Inc(MsgLines);
        end; { if }
    end; { While }

  {$i-} Close(Msg_F); {$I+}
  if IOResult>00 then ;
end; { proc. ReadTmpFile }


procedure WriteTmpFile(const FName     : String;
                       const EditorInfo: StringArrayObj;
                       var   MsgLines: Longint);
var Msg_F     : pFileObj;
    Counter   : Longint;
begin
  {-- Initialize the variables ----------------------------------------------}
  New(Msg_F, Init);
  Msg_F^.Assign(FName);
  Msg_F^.FileMode := ReadWriteMode + DenyNone;
  if NOT Msg_F^.Create(1) then
    begin
      Dispose(Msg_F, Done);
      EXIT;
    end; { if }

  {-- Write all message text to the file ------------------------------------}
  for Counter := 01 to MsgLines do
   if FirstChar(EditorInfo.Get(Counter)) <> #1 then
    Msg_F^.WriteLn(EditorInfo.Get(Counter));

  {-- Close up --------------------------------------------------------------}
  Dispose(Msg_F, Done);
end; { proc. WriteTmpFile }


procedure MakePostHeader(ReplyMsg, EditMsg: Boolean; const MessageInf: MessageRecord);
var TypeOfArea: String;
begin
  GetAreaTypeString(TypeOfArea, MessageInf);

  OutputObj^.ClearScreen;
  Writeln('`A3:');

  if Not ForwardMsg then
    begin
      If ReplyMsg then Write(LangObj^.ralGet(ralReplying));
      if EditMsg then Write(LangObj^.ralGet(ralEditting));

      if (NOT EditMsg) AND (NOT ReplyMsg) then
        Write(LangObj^.ralGet(ralPosting));

     WriteLn(#32, TypeOfArea, #32, LangObj^.ralGet(ralArea), ' "', MessageInf.Name,'".');
    end
      else begin
             Write(langobj^.ralGet(ralFwarding), ToWHo);
             WriteLn;
             ToWho := '';
           end; { else }

  WriteLn;
end; { proc. MakePostHeader }


var OrigFromWho  : String[65];        { Message originally from (in reply) }
    MessageInf   : MessageRecord;
    EleMsgInf    : EleMessageRecord;
    MsgLines     : Longint;          { Number of lines in the message text }
    Counter      : Longint;
    MsgPrivate   : Boolean;                           { Message is private }
    KillSent     : Boolean;                            { Kill/Sent message }
    CrashMail    : Boolean;                          { Sent message CRASH? }
    UploadingMsg : Boolean;                         { Has message uploaded }
    UseExternal  : Boolean;
    SaveDone     : Boolean;
    CarbonCopy   : Boolean;                   { Send a carbon-copy to <??> }
    GroupMail    : Boolean;                                 { Group mail ? }
    ReturnReceipt: Boolean;                                { ReturnReceipt }
    User_F       : pFileObj;                                    { UserFile }
    TmpEditor    : pStringArrayObj;
    KludgeCount  : Longint;    { Number of kludgelines found when editting }
    ItemCounter  : Longint;
    TotalItems   : Longint;
    UserInf      : UsersRecord;                         { Record for users }
    Numread      : NumReadType;                            { Reading bytes }
    TempStr      : String;
    Originally   : String;              { Message originally addressed to: }
    TempDir      : String;          { Temporarily directory in attach path }
    ExitCode     : Word;              { Code returned from External Editor }
    Uploads      : Longint;                     { Number of files uploaded }
    Success      : Boolean;
    AddParam     : String;                  { String to add for the editor }
    Fsed_MsgInfo : EditorMsgInfoRec;
    Attachment   : Boolean;
begin
   {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMailInt, 'WriteMessage (BoardNr='+FStr(BoardNr)+')');
   {$ENDIF}

  WriteMessage := false;
  Attachment := FALSE;
  KludgeCount := 0;
  If BoardNr=00 then Exit;

  OrigFromWho := ToWho;
  GetMessageRecord(MessageInf, BoardNr, False);
  GetEleMessageRecord(EleMsgInf, BoardNr, false);

  If (MessageInf.Name = '') OR (MessageInf.AreaNum=00) then
    begin
      RaLog('!', 'Invalid messageboard specified');
      EXIT;
    end; { if }

  If ((MessageInf.MsgKinds=ElemsgROnly) AND (LineCfg^.Exitinfo^.Userinfo.Security<>MessageInf.SysOpSecurity))
      OR (NOT CheckMsgAreaAccess(MessageInf, False, False, 00, LineCfg^.Exitinfo^.userinfo)) then
       begin
         WriteLn;
         Writeln('`A3:', LangObj^.ralGet(ralNoWrite));
         InputObj^.PressEnter(False, True);
         EXIT;
       end; { ReadOnly area (for you) }

  MakePostHeader(ReplyMsg, EditMsg, MessageInf);
  AskFromWho(FromWho, MessageInf);
  If FromWho='' then EXIT;

  {------------ Check for REPLYTO and REPLYADDR kludges --------------------}
                                   { We only accept a full set of kludges }
  if (ReplyTo = '') OR (ReplyAddr = '') then
    begin
      ReplyTo := '';
      ReplyAddr := '';
    end; { if }

  if NOT (MessageInf.Typ in [Netmail, EchoMail]) then
    begin
      ReplyTo := '';
      ReplyAddr := '';
    end; { if }

  if ReplyTo <> '' then
    begin
      ToAddress := FirstWord(ReplyTo, defExtractWord, true);
      ToWho := ReplyTo;
    end; { if }


  WriteLn('`A14:',LangObj^.ralGet(ralFrom1), '`A3:',FromWho);

  if Messageinf.Typ <> Newsgroup then
   begin
     AskToWho(ToWho, ToAddress, MessageInf.Typ in [LocalMail], MessageInf, Subject, MiscData);
     If ToWho='' then EXIT;
   end; { if }

  If MessageInf.Typ in [NetMail] then
    begin
      if ToAddress = '' then EXIT;
    end; { netmail message }

  if NOT ForwardMsg then
    begin
     AskSubject(Subject);
     if Subject='' then EXIT;
   end; { if }

  if InputObj^.ralStrYesNoAsk(ralAskChange) then
    begin
      ChangeThings(MessageInf, ToWho, ToAddress, Subject, MiscData);
    end; { if }

  if ((ToWho='') AND (NOT (MessageInf.Typ in [NewsGroup]))) OR (FromWho='') then EXIT;
  if ((Subject = '') AND (NOT ForwardMsg)) then EXIT;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailInt, 'After entering header-data');
  {$ENDIF}

{-------------------- Set private / not private properties -----------------}
  If (SUpcase(ToWho)<>'ALL') AND
      (MessageInf.MsgKinds in [ElemsgBoth]) then
        MsgPrivate := InputObj^.ralStrYesNoAsk(ralPrivate1);
  If (MessageInf.MsgKinds in [ElemsgPrivate]) then MsgPrivate := True;
  If MessageInf.MsgKinds in [ElemsgPublic] then MsgPrivate := False;


{---------------------------- Set killsent properties ----------------------}
  KillSent := False;
  if MessageInf.Typ in [NetMail] then
    case byte(GlobalCfg^.RaConfig^.KillSent) of
         Byte(Yes) : KillSent := True;
         Byte(No)  : KillSent := False;
         Byte(Ask) : KillSent := InputObj^.ralStrYesNoAsk(ralAskDelSnt);
    end; { Case }


{-------------------------- Set CrashMail properties -----------------------}
  CrashMail := False;
  If MessageInf.Typ in [NetMail] then
    If GlobalCfg^.RaConfig^.CrashAskSec<=LineCfg^.Exitinfo^.Userinfo.Security then
        If GlobalCfg^.RaConfig^.CrashSec<=LineCfg^.Exitinfo^.Userinfo.Security then CrashMail := True else
          CrashMail := InputObj^.ralStrYesNoAsk(ralAskCrash);


{------------- Set properties for replying to and upping message -----------}
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailInt, 'Set status of msg, now replying (if)');
  {$ENDIF}

  MsgLines := 00;
  if (NOT ReplyMsg) AND (NOT EditMsg) then QuoteLines := 00;

  UploadingMsg := False;
  if NOT ForwardMsg then
   if LineCfg^.Exitinfo^.Baud>00 then
    if (NOT ReplyMsg) AND (NOT EditMsg) then
     if GlobalCfg^.RaConfig^.UploadMsgs then
      if InputObj^.ralStrYesNoAsk(ralUplPrep) then
         begin
           EraseFile('msgtmp');
           UploadingMsg := True;
           UploadMessage;
           ReadTmpFile('msgtmp', QuoteText, MsgLines, NOT ReadBit(MessageInf.Attribute, 4));
         end; { if }

  {-- Set properties for replying to message --------------------------------}
  if (ReplyMsg) AND (NOT ForwardMsg) then
   begin
     WriteLn(LangObj^.ralGet(ralQuoting));
     New(TmpEditor, Init(MaxMsgLines));

     {-- Create the reply header --------------------------------------------}
     TempStr := GlobalCfg^.RaConfig^.ReplyHeader;

     Replace('@', OrigToWho, TempStr);
     Replace('#', OrigFromWho, TempStr);
     Replace('`', ExtractWord(OrigDateStr, 2, defExtractWord, false, false), TempStr);
     Replace('', ExtractWord(OrigDateStr, 1, defExtractWord, false, false), TempStr);

     TmpEditor^.Put(1, TempStr);
     TmpEditor^.Put(2, '');

     {-- Create the front string ("MB> ") -----------------------------------}
     TempStr := GlobalCfg^.RaConfig^.QuoteString;
     Replace('@', SupCase(InitialString(OrigFromWho)), TempStr);
     Replace('#', SLowCase(InitialString(OrigFromWho)), TempStr);

     {-- Transfer the temp buffer to the real buffer ------------------------}
     for Counter := 01 to QuoteLines do
       TmpEditor^.Put(Counter + 02, QuoteText.Get(Counter));

     for Counter := 01 to TmpEditor^.MaxStrings do
       QuoteText.Put(Counter, TmpEditor^.Get(Counter));
     Inc(QuoteLines, 2);

     {-- Put the front string actually in front ;) --------------------------}
     Dispose(TmpEditor, Done);

     for Counter := 03 to QuoteLines do
      if Trim(QuoteText.Get(Counter)) <> '' then
        QuoteText.Put(Counter, TempStr + QuoteText.Get(Counter));

     {-- Write all this to the temp file ------------------------------------}
     WriteTmpFile('msgtmp', QuoteText, QuoteLines);
   end; { ReplyMsg }

  {-- Set properties for editting a message ---------------------------------}
  if (EditMsg) AND (NOT ForwardMsg) then
   begin
     {-- init the editor ----------------------------------------------------}
     New(TmpEditor, Init(MaxMsgLines));
     KludgeCount := 0;

     for Counter := 1 to MsgLines do
       if FirstChar(QuoteText.Get(Counter)) = #1 then
         begin
           Inc(KludgeCount);
           TmpEditor^.Add(QuoteText.Get(Counter));
         end; { if }

     {-- Write all this to the temp file ------------------------------------}
     WriteTmpFile('msgtmp', QuoteText, QuoteLines);
   end; { ReplyMsg }

  if (ReplyMsg) AND (ForwardMsg) then
   begin
     New(TmpEditor, Init(MaxMsgLines));

     {-- Create a nice forward header ---------------------------------------}
     TmpEditor^.Put(1, ' * Message originally:');
     TmpEditor^.Put(2, '     From: ' + FromWho);
     TmpEditor^.Put(3, '     To  : ' + ToAddress);
     TmpEditor^.Put(4, '     Date: ' + ExtractWord(OrigDateStr, 1, defExtractWord, false, false));
     TmpEditor^.Put(5, '     Area: "'+ OrigAreaStr + '"');
     TmpEditor^.Put(6, ' * Forwarded by ' + FromWho + ' using ' + FullProgName + #32 + VersionID);
     TmpEditor^.Put(7, '');

     for Counter := 01 to QuoteLines do
       TmpEditor^.Put(Counter + 07, QuoteText.Get(Counter));

     {-- Transfer the temp buffer to the real buffer ------------------------}
     for Counter := 01 to TmpEditor^.MaxStrings do
       QuoteText.Put(Counter, TmpEditor^.Get(Counter));
     Inc(QuoteLines, 7);

     {-- Write all this to the temp file ------------------------------------}
     Dispose(TmpEditor, Done);
     WriteTmpFile('msgtmp', QuoteText, QuoteLines);
   end; { ReplyMsg }


{--------------------- System is ready to enter the message ----------------}
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailInt, 'Done UploadingMsg and Quoting text');
    DebugObj.DebugLog(logMailInt, 'External Editor= "' + GlobalCfg^.RaConfig^.ExternalEdCmd + '"');
    DebugObj.DebugLog(logMailInt, 'UseExternal = ' + Bool2Str(ReadBit(LineCfg^.Exitinfo^.Userinfo.Attribute, 6)));
  {$ENDIF}

  UseExternal := false;
  if (GlobalCfg^.RaConfig^.ExternalEdCmd<>'') then
   if (ReadBit(LineCfg^.Exitinfo^.Userinfo.Attribute, 6)) then UseExternal := true;

  if NOT UploadingMsg then
   if UseExternal then
      begin
        {$IFDEF WITH_DEBUG}
          DebugObj.DebugLog(logMailInt, 'Writing MSGINF');
        {$ENDIF}

      {$IFDEF WITH_FULL}
        if MessageInf.Typ in [Newsgroup] then
          ToWho := 'All';

        WriteMsgInf(FromWho, ToWho, Subject, FStr(GetHighMsgNum(MessageInf, EleMsgInf)), MessageInf.Name, MsgPrivate);
      {$ENDIF}

      {$IFDEF WITH_DEBUG}
        DebugObj.DebugLog(logMailInt, 'Erasing MSGTMP');
      {$ENDIF}
      if (NOT ReplyMsg) AND (NOT EditMsg) then
        EraseFile('msgtmp');                          { Erase message text }

      {$IFDEF WITH_DEBUG}
        DebugObj.DebugLog(logMailInt, 'ExecFunc (ext.ed)');
      {$ENDIF}

      if SUpCase(GlobalCfg^.RaConfig^.ExternalEdCmd) <> 'INTERNAL' then
        begin
          AddParam := FStr(LineCfg^.Modem^.ComPort) + #32 +
                      FStr(FixBaud(LineCfg^.Exitinfo^.Baud)) + #32 +
                      FStr(Min(LineCfg^.Exitinfo^.TimeLimit, 32767)) + #32 +
                      FStr(GlobalCfg^.RaConfig^.UserTimeOut);
          RaExec(GlobalCfg^.RaConfig^.ExternalEdCmd + #32 + AddParam, False, False, False, false, ExitCode, Success);
        end
          else begin
                 Fsed_MsgInfo.FromWho   := FromWho;
                 Fsed_MsgInfo.ToWho     := ToWho;
                 Fsed_MsgInfo.Subject   := Subject;
                 Fsed_MsgInfo.AreaName  := MessageInf.Name;
                 Fsed_MsgInfo.MsgNumber := GetHighMsgNum(MessageInf, EleMsgInf);
                 Fsed_MsgInfo.IsPrivate := MsgPrivate;

                 DoInternalFSED(Fsed_MsgInfo);
                 Success := true;
               end; { if }

      OutputObj^.ClearScreen;

      {$IFDEF MSDOS}
        if NOT Success then
          begin
            Writeln(LangObj^.ralGet(ralErrFsEd));
            InputObj^.PressEnter(False, True);
          end; { if }
      {$ENDIF}

      {$IFDEF WITH_DEBUG}
        DebugObj.DebugLog(logMailInt, 'Erasing MSGINF');
      {$ENDIF}
      EraseFile('msginf');

      ReadTmpFile('msgtmp', QuoteText, MsgLines, NOT ReadBit(MessageInf.Attribute, 4));
      While InputObj^.Keypressed do InputObj^.ReadKey;
    end; { External Editor }

  if NOT UploadingMsg then
   if NOT UseExternal then
     begin
       MsgLines := 60;                          { Maximum of 60 lines }

       if ReplyMsg then
        if NOT InputObj^.ralStrYesNoAsk(ralAskQuote) then
          QuoteLines := 00;

       RaAlikeEditor(LangObj^.ralGet(ralBeginMsg),
                        MsgLines,
                        QuoteText,
                        QuoteLines);
     end; { RaAlikeEditor }


{----------------------- We are going to write the message -----------------}
  ReturnReceipt := False;
  Originally := ToWho;

  If LineCfg^.Exitinfo^.Userinfo.ForwardTo <> '' then
    ToWho := LineCfg^.Exitinfo^.Userinfo.ForwardTo;

  if (MsgLines=01) then
    if QuoteText.Get(01) = '' then MsgLines := 0;

  {------------- Insert TO: prefix for Internet gating ---------------------}
  if ReplyAddr <> '' then
    begin
       New(TmpEditor, Init(MaxMsgLines));

       {-- Create a TO: prefix for the internet -----------------------------}
       TmpEditor^.Put(1, 'To: ' + ReplyAddr);
       TmpEditor^.Put(2, '');

       for Counter := 01 to MsgLines do
         TmpEditor^.Put(Counter + 02, QuoteText.Get(Counter));

       {-- Transfer the temp buffer to the real buffer ----------------------}
       for Counter := 01 to TmpEditor^.MaxStrings do
         QuoteText.Put(Counter, TmpEditor^.Get(Counter));
       Inc(MsgLines, 2);

       Dispose(TmpEditor, Done);
    end; { if ReplyTo }


  If MsgLines>00 then
    begin

      {---------------- Set properties for returnreceipt ------------------}
      if (LineCfg^.Exitinfo^.Userinfo.Security>=GlobalCfg^.RaConfig^.ReturnRecSec) AND
          (SUpCase(ToWho)<>'ALL') then
           if (MessageInf.Typ in [LocalMail]) then
            If InputObj^.ralStrYesNoAsk(ralLRRC) then
                 ReturnReceipt := True
                   else ReturnReceipt := false;

      {---------------- Set properties for file-attach --------------------}
      if ReadBit(MessageInf.Attribute, 2) then
       if AllowAttaches then
        if InputObj^.ralStrYesNoAsk(ralAttFiles1) then
          begin
            TempDir := ForceBack(CreateTempDir(GlobalCfg^.RaConfig^.AttachPath, LineCfg^.RaNodeNr));

            if TempDir <> '' then
             begin
               RALog('>', 'Following message has files attached:');
               {$IFDEF WITH_FULL}
                 DoUpload('', True, False, TempDir, Uploads);
               {$ENDIF}

               if Uploads = 00 then
                begin
                  AttachMent := FALSE;
                  RaLog('!', 'Did not receive any attaches, directory removed');

                  {$i-} RmDir(TempDir); {$i+}
                  if IOResult>00 then ;
                end; { if }

               if Uploads <> 0 then
                begin
                  Attachment := TRUE;
                  Subject := TempDir;
                end; { user has really uploaded some files }

               RaLog('>', 'End of file attaches');
             end { if Created Directory }
              else RaLog('!', 'Unable to create attach directory!');
          end; { if }

      {---------------- Set properties for ForwardTo ----------------------}
      SaveDone := False;
      CarbonCopy := False;

      If LineCfg^.Exitinfo^.Userinfo.ForwardTo<>'' then
        begin
          New(TmpEditor, Init(MaxMsgLines));

          {-- Create a forward header ---------------------------------------}
          TmpEditor^.Put(1, '* Automaticly forward for ' + Originally);
          TmpEditor^.Put(2, '');

          for Counter := 01 to MsgLines do
            TmpEditor^.Put(Counter + 02, QuoteText.Get(Counter));

          {-- Transfer the temp buffer to the real buffer -------------------}
          for Counter := 01 to TmpEditor^.MaxStrings do
            QuoteText.Put(Counter, TmpEditor^.Get(Counter));
          Inc(MsgLines, 2);

          Dispose(TmpEditor, Done);
        end; { forward automaticly }

      {------------- Set properties for Message invalid pwd ----------------}
      If AddWarnMsg then
       begin
          New(TmpEditor, Init(MaxMsgLines));

          {-- Create a warning prefix ---------------------------------------}
          TmpEditor^.Put(1, '!!! WARNING - THIS USER ENTERED AN INVALID PASSWORD !!!');
          TmpEditor^.Put(2, '');

          for Counter := 01 to MsgLines do
            TmpEditor^.Put(Counter + 02, QuoteText.Get(Counter));

          {-- Transfer the temp buffer to the real buffer -------------------}
          for Counter := 01 to TmpEditor^.MaxStrings do
            QuoteText.Put(Counter, TmpEditor^.Get(Counter));
          Inc(MsgLines, 2);

          Dispose(TmpEditor, Done);
       end; { AddWarnMsg }

      REPEAT
        {----------- Keep sending carbon copies to everybody --------------}
        If CarbonCopy then
          begin
            New(TmpEditor, Init(MaxMsgLines));

            {-- Create a carboncopy header ----------------------------------}
            TmpEditor^.Put(1, '* Carbon copy, originally to: ' + Originally);
            TmpEditor^.Put(2, '');

            for Counter := 01 to MsgLines do
              TmpEditor^.Put(Counter + 02, QuoteText.Get(Counter));

            {-- Transfer the temp buffer to the real buffer -----------------}
            for Counter := 01 to TmpEditor^.MaxStrings do
              QuoteText.Put(Counter, TmpEditor^.Get(Counter));
            Inc(MsgLines, 2);
            Dispose(TmpEditor, Done);

            repeat
              ToWho := '';
              ToAddress := '';

              AskToWho(ToWho, ToAddress, MessageInf.Typ in [LocalMail], MessageInf, Subject, Miscdata);

              if ToWho <> '' then
               begin
                 WriteLn;
                 Write(LangObj^.ralGet(ralSaving),' (#');

                 {$IFDEF WITH_FULL}
                   PostMessage(BoardNr, ToWho, FromWho, Subject, ReturnReceipt, MsgPrivate, False, KillSent,
                               CrashMail, AttachMent, false, MsgLines, QuoteText, MsgNrWritten, ToAddress, FromAddress, true,
                               false, DateStr, TimeStr(true, false), ReplyKludge, '', nilAbsMsgPtr);

                   Writeln(GetHighMsgNum(MessageInf, EleMsgInf),')');
                 {$ENDIF}
               end; { if }

            until (ToWho='');
          end; { CarbonCopy }

        GroupMail := (ToWho[1] in ['@', '=', '>', '<']);
        If (GlobalCfg^.RaConfig^.GroupMailSec > LineCfg^.Exitinfo^.Userinfo.Security) then
         if GroupMail then
           if (MessageInf.Typ in [LocalMail]) then
               begin
                 RaLog('!', 'Groupmail attempt - aborted');
                 GroupMail := FALSE;
               end; { if }

        {------------- Do not generate groupmail --------------------------}
        If (NOT (GroupMail)) AND ((ToWho<>'') OR (MessageInf.Typ in [Newsgroup])) then
          begin
            WriteLn;
            Write(LangObj^.ralGet(ralSaving),' (#');

            {$IFDEF WITH_FULL}
              if NOT EditMsg then
                begin
                  PostMessage(BoardNr, ToWho, FromWho, Subject, ReturnReceipt, MsgPrivate, False, KillSent,
                              CrashMail, AttachMent, false, MsgLines, QuoteText, MsgNrWritten, ToAddress, FromAddress, true,
                              false, DateStr, TimeStr(true, false), ReplyKludge, '', nilAbsMsgPtr);
                end
                  else begin
                         EditMessage(MsgNrWritten,
                                     BoardNr,
                                     ToWho, FromWho, Subject, msgPrivate,
                                     MsgLines,
                                     KludgeCount,
                                     TmpEditor^,
                                     QuoteText);

                         {-- and dispose of the extra memory --------------}
                         Dispose(TmpEditor, Done);
                       end; { if }

              {-- and show the mesasge number -----------------------------}
              Writeln(GetHighMsgNum(MessageInf, EleMsgInf),')');
            {$ENDIF}
            WriteLn;
          end
          {----------- GroupMail is needed, generate it ---------------------}
           else begin
                  WriteLn;

                  New(User_F, Init);
                  User_F^.Assign(GlobalCfg^.RaConfig^.MsgbasePath + UserBaseName);
                  User_F^.FileMode := ReadMode + DenyNone;
                  if User_F^.Open(1) then
                    begin
                      ItemCounter := 00;
                      TotalItems := User_F^.FileSize div SizeOf(UsersRecord);

                      While (ItemCounter < TotalItems) do
                        begin
                          NumRead := User_F^.BlkRead(UserInf, SizeOf(UsersRecord));
                          Inc(ItemCounter);

                          if SupCase(ToWho)='@ALL' then
                            begin
                              Write('`A15:`X1:`E:',UserInf.Name,' (#');
                              {$IFDEF WITH_FULL}
                                PostMessage(BoardNr, UserInf.Name, FromWho, Subject, ReturnReceipt, MsgPrivate,
                                            False, KillSent, CrashMail, AttachMent, false, MsgLines, QuoteText, MsgNrWritten,
                                            ToAddress, FromAddress, true, false,
                                            DateStr, TimeStr(true, false),
                                            ReplyKludge, '', nilAbsMsgPtr);
                                Write(GetHighMsgNum(MessageInf, EleMsgInf),')');
                              {$ENDIF}
                            end; { @ALL }

                          if ToWho[1]='=' then
                            if FVal(GetValue('=', ToWho, False))=UserInf.Security then
                              begin
                                Write('`A15:`X1:`E:',UserInf.Name,' (#');
                                {$IFDEF WITH_FULL}
                                  PostMessage(BoardNr, UserInf.Name, FromWho, Subject, ReturnReceipt, MsgPrivate,
                                              False, KillSent, CrashMail, AttachMent, false, MsgLines, QuoteText,MsgNrWritten,
                                              ToAddress, FromAddress, true, false,
                                              DateStr, TimeStr(true, false),
                                              ReplyKludge, '', nilAbsMsgPtr);
                                  Write(GetHighMsgNum(MessageInf, EleMsgInf),')');
                                {$ENDIF}
                              end; { GetValue }

                          if ToWho[1]='>' then
                            if FVal(GetValue('>', ToWho, False))>UserInf.Security then
                              begin
                                Write('`A15:`X1:`E:',UserInf.Name,' (#');
                                {$IFDEF WITH_FULL}
                                  PostMessage(BoardNr, UserInf.Name, FromWho, Subject, ReturnReceipt, MsgPrivate,
                                              False, KillSent, CrashMail, AttachMent, false, MsgLines,QuoteText, MsgNrWritten,
                                              ToAddress, FromAddress, true, false,
                                              DateStr, TimeStr(true, false),
                                              ReplyKludge, '', nilAbsMsgPtr);
                                  Write(GetHighMsgNum(MessageInf, ElemsgInf),')');
                                {$ENDIF}
                              end; { GetValue }

                          if ToWho[1]='<' then
                            if FVal(GetValue('<', ToWho, False))<UserInf.Security then
                              begin
                                Write('`A15:`X1:`E:',UserInf.Name,' (#');
                                {$IFDEF WITH_FULL}
                                  PostMessage(BoardNr, UserInf.Name, FromWho, Subject, ReturnReceipt, MsgPrivate,
                                              False, KillSent, CrashMail, AttachMent, false, MsgLines,QuoteText, MsgNrWritten,
                                              ToAddress, FromAddress, true, false,
                                              DateStr, TimeStr(true, false),
                                              ReplyKludge, '', nilAbsMsgPtr);
                                  Write(GetHighMsgNum(MessageInf, EleMsgInf),')');
                                {$ENDIF}
                              end; { GetValue }

                          if ToWho[1]='@' then
                            if UpCase(ToWho[2])<>'A' then
                              if FVal(GetValue('@', ToWho, False))<UserInf.Group then
                                begin
                                  Write('`A15:`X1:`E:',UserInf.Name,' (#');
                                  {$IFDEF WITH_FULL}
                                    PostMessage(BoardNr, UserInf.Name, FromWho, Subject, ReturnReceipt, MsgPrivate,
                                                False, KillSent, CrashMail, AttachMent, false, MsgLines,QuoteText,
                                                MsgNrWritten,
                                                ToAddress, FromAddress, true, false,
                                                DateStr, TimeStr(true, false),
                                                ReplyKludge, '', nilAbsMsgPtr);
                                    Write(GetHighMsgNum(MessageInf, EleMsgInf),')');
                                  {$ENDIF}
                                end; { GetValue }
                        end; { While begin }
                    end; { open }

                  Dispose(User_F, Done);
                  WriteLn;
                end; { GroupMail generated }

        ToWho := '';

        if AskCarbonCopy then
         if NOT CarbonCopy then
          If LineCfg^.Exitinfo^.Userinfo.Security >= GlobalCfg^.RaConfig^.ccSec then
           if (MessageInf.Typ in [LocalMail]) then
            If InputObj^.ralStrYesNoAsk(ralSendCC) then
              begin
                CarbonCopy := TRUE;
                ToWho := 'NonExisting User';
              end; { Carbon Copy }

        SaveDone := (ToWho='');
      Until (SaveDone);

      WriteMessage := true;
    end { MsgLines }
     else begin
            OutputObj^.ClearScreen;
            Writeln('`A12:',LangObj^.ralGet(ralMsgAbort));
            InputObj^.PressEnter(False, True);
          end; { Message Aborted }

  EraseFile('msgtmp');                                   { Erase message text }
  Inc(LineCfg^.Exitinfo^.Userinfo.MsgsPosted);       { Total number of emssages posted }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMailInt, 'WriteMessage ( end )');
  {$ENDIF}
end; { proc. WriteMessage }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
