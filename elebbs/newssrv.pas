unit NEWSSRV;
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
** NEWSSRV, newsgroup server for EleBBS/W32 and EleBBS/2
**
** Copyright (c) 1998-2001 by Maarten Bekers
**
** Created : 01-Apr-1999
** Last update : 11-Sep-2006
**
** Note: Define MARTYDEBUG for Marty's NEWSSRV problems
*)


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
uses TcpSrv;

var
  news_ListenThreadEnded : BOOLEAN;
  NewsServerSocket : TTcpServer;

const
  news_IsRunSlow         : Boolean = false;

procedure news_InitServer;
procedure news_SetupServer;
procedure CreateGroupIndex;
procedure ReadNewsServerEle;                     { Opens and reads NWSERV.ELE }

function news_ExecuteConnection(P: pointer): Longint;
function news_ServerThread(P: pointer): Longint;

procedure news_CreateStatistics;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Crt, Global, CfgRec, Articles,
     CentrStr, ScrnU, LongStr, StUtils, GenCfg,
     JDates, IoRes_Un, SysUtils, ReadCfg,
     WinTitle, GenFile, MemMan, Cases, WriteMsg,
     WordStr, Crc_Unit, BbsKey, FileRout, Mail,
     MkMsgAbs, MkOpen,

     {$IFNDEF FPC}
       VpSysLow,
     {$ENDIF}

     SockFunc, StrPath, SockDef, Access_U,
     TcpCli, Threads, ElLog_U, FileObj,
     BitWise, Debug_U, Aptimer, Cfgfile,
     ChrIso, User_U, ObjDec, SysVars,
     eSrv_U;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


const
  MaxExtrahdrs  = 200;
  NewsSessions  : Longint = 0;

type
  NwsIdxRecord = record
                    GroupCRC    : Longint;
                    AreaNum     : Longint;
                    PostingType : CfgRec.AccessType;
                  end; { record }


type
  MsgPostRecord = record
                     FromWho    : AnsiString;
                     Subject    : AnsiString;
                     MessageID  : AnsiString;
                     ReplyID    : AnsiString;
                     Date       : AnsiString;
                     NewsGroups : AnsiString;
                     NewsReader : AnsiString;
                     NewsServer : AnsiString;
                     ExtraHdrs  : Array[1..MaxExtraHdrs] of AnsiString;
                     HdrCnt     : Longint;
                  end; { MsgPostRecord }


const
  NwsGroupIdxName   : String = 'nwsgroup.idx';
  NwsGroupStatName  : String = 'nwsgroup.sta';
  NwsAllowPath      : String = '';

const
  UseFTN            : Boolean = true;
  SecsTimeOut       = 10 * 60;    { Timeout after 10 minutes inactivity }

  ServerGreeting    : String = 'EleBBS news server running at %s';
  PostingAllowed    : Boolean = true;
  ThisHostName      : String = '@elebbs.bbs';

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

type NewsCommandset = (nwsArticle, nwsBody, nwsGroup, nwsHelp, nwsHead,
                       nwsIhave, nwsLast, nwsList, nwsNewGroups, nwsNewNews,
                       nwsNext, nwsPost, nwsSlave, nwsStat, nwsQuit, nwsXover,
                       nwsError, nwsAuth, nwsMode, nwsCrash);


type
  nwsSrvConnection = record
                       UserName  : ShortString;
                       Password  : ShortString;
                       UserInf   : UsersRecord;
                       UserRecNr : Longint;
                       MsgPtr    : AbsMsgPtr;
                       AreaInf   : MessageRecord;
                     end; { record }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


procedure news_CreateStatistics;
var Stat_F : pFileObj;
    Out_F  : pFileObj;
    StatInf: NewsGroupStat;
    TotInf : NewsGroupStat;
begin
  ScreenClear(LightGray, #32);
  WriteAT(01, 01, Yellow, FullProgName + ' NNTP SERVER v'+VersionID);
  WriteRight(79, 01, Yellow, '(c) 1998-2003 Maarten Bekers');
  PartClear(1, 2, mnuScrnWidth, 2, mnuTopBar, mnuDblWide);

  WriteAT(1, 4, Lightgray, 'Creating newsgroup statistics file (nwsstat.txt)');
  UpdateScreenBuffer(true);
  CursorOn;
  GotoXY(1, 6);

  New(Stat_F, Init);
  Stat_F^.Assign(NwsGroupStatName);
  Stat_F^.FileMode := ReadMode + DenyNone;
  if Stat_F^.Open(1) then
    begin
      FillChar(TotInf, SizeOf(TotInf), 0);

      New(Out_F, Init);
      Out_F^.Assign('nwsstat.txt');
      Out_F^.Filemode := ReadWriteMode + DenyNone;
      Out_F^.Create(1);

      Stat_F^.BlkRead(StatInf, SizeOf(NewsGroupStat));

      Out_F^.WriteLn('EleBBS NEWSSRV Statistics file. (c)2003 by Maarten Bekers');
      Out_F^.WriteLn('');
      Out_F^.WriteLn('');
      Out_F^.WriteLn('Start date: ' + StatInf.LastAccessed + ' ' + StatInf.GroupName);
      Out_F^.WriteLn('End date  : ' + DateStrY2k + ' ' + TimeStr(false, false));
      Out_F^.WriteLn('');
      Out_F^.WriteLn('');

      Out_F^.WriteLn( MakeLen('Group name', 50, Space, false, false) +
                      MakeLen('Read#', 5, Space, false, false) + #32#32 +
                      MakeLen('Write#', 5, Space, false, false) + #32#32 +
                      MakeLen('Deny#', 5, Space, false, false));
      Out_F^.WriteLn(Dup('-', 79));

      While NOT Stat_F^.EOF do
        begin
          Stat_F^.BlkRead(StatInf, SizeOf(NewsGroupStat));

          Out_F^.WriteLn( MakeLen(StatInf.GroupName, 50, Space, false, false) +
                          MakeLen(FStr(StatInf.NumRead), 5, Space, false, false) + #32#32 +
                          MakeLen(FStr(StatInf.NumWrite), 5, Space, false, false) + #32#32 +
                          MakeLen(FStr(StatInf.NumDeny), 5, Space, false, false));

          Inc(TotInf.NumRead, StatInf.NumRead);
          Inc(TotInf.NumWrite, StatInf.NumWrite);
          Inc(TotInf.NumDeny, StatInf.NumDeny);
        end; { while }

      Out_F^.WriteLn(Dup('-', 79));
      Out_F^.WriteLn( MakeLen('Totals', 50, Space, false, false) +
                      MakeLen(FStr(TotInf.NumRead), 5, Space, false, false) + #32#32 +
                      MakeLen(FStr(TotInf.NumWrite), 5, Space, false, false) + #32#32 +
                      MakeLen(FStr(TotInf.NumDeny), 5, Space, false, false));


      Dispose(Out_F, Done);
    end { while }
      else WriteLn('Unable to open: ', NwsGroupStatName);

  Dispose(Stat_F, Done);
end; { proc. news_CreateStatistics }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure news_SetupServer;
var ErrorStr: String;
begin
  NewsServerSocket := TTcpServer.Create;
  NewsServerSocket.ReUseAddr := true;

  if NOT NewsServerSocket.SetupServer(LineCfg^.NewsServer^.ServerPort, ErrorStr, InAddr_Any) then
    FatalError(ErrorStr);

  NewsServerSocket.Blocking := false;
end; { proc. news_SetupServer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CreateGroupIndex;
var Msg_F      : pFileObj;
    MessageInf : EleMessageRecord;

    GrpIdx_F   : pFileObj;
    GroupInf   : NwsIdxRecord;
begin
  New(Msg_F, Init);
  Msg_F^.Assign(MessageEleFileName);
  Msg_F^.FileMode := ReadMode + DenyNone;
  if NOT Msg_F^.Open(1) then
    begin
      FatalError('Unable to open '+MessageEleFileName + '(#' + IntToStr(Msg_F^.IoResult)+')');
    end; { if }

  New(GrpIdx_F, Init);
  GrpIdx_F^.Assign(NwsGroupIdxName);
  GrpIdx_F^.FileMode := ReadWriteMode + DenyWrite;
  if NOT GrpIdx_F^.Create(1) then
    FatalError('Unable to create '+NwsGroupIdxName);

  While NOT Msg_F^.EOF do
    begin
      Msg_F^.BlkRead(MessageInf, SizeOf(EleMessageRecord));
      if Msg_F^.IoResult > 0 then BREAK;

      {---------------------- Initialize the CRC group -----------------------}
      FillChar(GroupInf, SizeOf(NwsIdxRecord), #00);
      GroupInf.GroupCRC := RaCrc(SLowCase(MessageInf.GroupName), true);
      GroupInf.AreaNum := MessageInf.AreaNum;
      GroupInf.PostingType := AccessType(MessageInf.AccessSettings);

      if ReadBit(MessageInf.Attribute, 0) then
        begin
          GrpIdx_F^.BlkWrite(GroupInf, SizeOf(NwsIdxRecord));
          if GrpIdx_F^.IoResult > 0 then BREAK;

          {$IFDEF WITH_DEBUG}
            DebugObj.DebugLog(logTcpip, 'CreateGroupIndex - "' + MessageInf.GroupName + '" - CRC=' + IntToStr(GroupInf.GroupCRC));
          {$ENDIF}
        end; { if }

    end; { while }

  Dispose(Msg_F, Done);
  Dispose(GrpIdx_F, Done);
end; { proc. CreateGroupIndex }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function NewsStatus(Code: Longint): String;
begin
  Result := '';

  Case Code of
    200 : NewsStatus := Format(ServerGreeting, [GlobalCfg^.RaConfig^.SystemName]) + ' ready - posting allowed';
    201 : NewsStatus := Format(ServerGreeting, [GlobalCfg^.RaConfig^.SystemName]) + ' ready - no posting allowed';
    202 : NewsStatus := 'Slave status noted';
    205 : NewsStatus := 'Closing connection - don''t forget to hug your Sysop today!';
    215 : NewsStatus := 'List of newsgroups follow';
    224 : NewsStatus := 'Overview information follows';
    240 : NewsStatus := 'Article posted OK';
    281 : NewsStatus := 'Authentication accepted';
    340 : NewsStatus := 'Send article to be posted. End with <CR-LF>.<CR-LF>';
    381 : NewsStatus := 'More authentication information required';
    411 : NewsStatus := 'No such newsgroup';
    412 : NewsStatus := 'No newsgroup has been selected';
    420 : NewsStatus := 'No current article has been selected';
    422 : NewsStatus := 'No previous or next article in this group';
    423 : NewsStatus := 'No such article number in this group';
    430 : NewsStatus := 'No such article found';
    440 : NewsStatus := 'Posting not allowed';
    441 : NewsStatus := 'Posting failed';
    480 : NewsStatus := 'Authentication required';
    482 : NewsStatus := 'Authentication rejected';
    500 : NewsStatus := 'Command not recognized';
    503 : NewsStatus := 'Program fault - command not peformed';
  end; { case }
end; { func. NewsStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ValidGroup(Groupname: String): Longint;
var GroupCRC: Longint;

    Group_F  : pFileObj;
    GroupInf : NwsIdxRecord;
begin
  ValidGroup := -1;
  GroupCRC := RaCrc(SLowCase(GroupName), true);


  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpip, 'ValidGroup - (BEGIN)');
    DebugObj.DebugLog(logTcpip, 'ValidGroup - NwsGroupIdxName = "' + NwsGroupIdxName + '"');
    DebugObj.DebugLog(logTcpip, 'ValidGroup - Groupname = "' + GroupName + '"');
    DebugObj.DebugLog(logTcpip, 'ValidGroup - GroupCRC  = "' + IntToStr(GroupCRC) + '"');
  {$ENDIF}

{$IFDEF MARTYDEBUG}
{!!!} RaLog('!', '-Marty Log- ValidGroup() begin');
{!!!} RaLog('!', '-Marty Log- NwsGroupIdxName="'+NwsGroupIdxName+'"');
{!!!} RaLog('!', '-Marty Log- GroupName="'+GroupName+'"');
{!!!} RaLog('!', '-Marty Log- GroupCRC='+FStr(GroupCRC));
{$ENDIF}

  New(Group_F, Init);
  Group_F^.Assign(NwsGroupIdxName);
  Group_F^.FileMode := ReadMode + DenyNone;
  if Group_F^.Open(1) then
    begin
      {$IFDEF WITH_DEBUG}
        DebugObj.DebugLog(logTcpip, 'ValidGroup - Newsgroup index file opened');
      {$ENDIF}

{$IFDEF MARTYDEBUG}
{!!!} RaLog('!', '-Marty Log- NwsGroup file opened');
{$ENDIF}

      While NOT Group_F^.EOF do
        begin
          Group_F^.BlkRead(GroupInf, SizeOf(NwsIdxRecord));
          if Group_F^.IoResult > 0 then BREAK;

{$IFDEF MARTYDEBUG}
{!!!} RaLog('!', '-Marty Log- this one CRC='+FStr(GroupInf.GroupCRC));
{$ENDIF}

          if GroupInf.GroupCRC = GroupCRC then
            begin
              ValidGroup := GroupInf.AreaNum;
              BREAK;
            end; { if }
        end; { while }
    end { if }
      else begin
             {$IFDEF WITH_DEBUG}
               DebugObj.DebugLog(logTcpip, 'ValidGroup - I/O error: #'+IntToStr(Group_F^.IoResult));
             {$ENDIF}

{$IFDEF MARTYDEBUG}
{!!!} RaLog('!', '-Marty Log- ValidGroup() !!!!! i/o error');
{$ENDIF}

           end; { else }


  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpip, 'ValidGroup - ( END )');
  {$ENDIF}

{$IFDEF MARTYDEBUG}
{!!!} RaLog('!', '-Marty Log- ValidGroup() end');
{$ENDIF}

  Dispose(Group_F, Done);
end; { func. ValidGroup }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GetNewsGroupStat(GroupName    : String;
                           var ReadNum,
                               WriteNum,
                               DenyNum  : Longint);
var Stats_F  : pFileObj;
    StatInf  : NewsGroupStat;
begin
  ReadNum := 0;
  WriteNum := 0;
  DenyNum := 0;

  New(Stats_F, Init);
  Stats_F^.Assign(NwsGroupStatName);
  Stats_F^.FileMode := ReadMode + DenyNone;
  if NOT Stats_F^.Open(1) then
    begin
      EXIT;
    end; { if }

  While NOT Stats_F^.EOF do
    begin
      Stats_F^.BlkRead(StatInf, SizeOf(StatInf));

      if SupCase(Trim(StatInf.GroupName)) = SUpCase(Trim(GroupName)) then
        begin
          ReadNum := StatInf.NumRead;
          WriteNum := StatInf.NumWrite;
          DenyNum := StatInf.NumDeny;

          BREAK;
        end; { if }

    end; { while }

  Dispose(Stats_F, Done);
end; { proc. GetNewsGroupStat }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


procedure SendCodeStr(var NewsServer: TTcpClient;  Code: Word);
begin
  NewsServer.SendStrLn(Format('%d %s', [Code, NewsStatus(Code)]));
end; { proc. SendCodeStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SendGroupList(var NewsServer: TTcpClient;
                        const CommandStr   : String;
                        const OnlyNew      : Boolean;
                        const Authenticated: Boolean;
                        const ServerSlot   : Longint);
var Message_F     : pFileObj;
    EleMsg_F      : pFileObj;
    MessageInf    : MessageRecord;
    EleMessage    : EleMessageRecord;

    PostMsgChar   : Char;
    LastMsg,
    FirstMsg      : Longint;
    HasAreaAccess : Boolean;

    ListMode      : String;
begin
  if OnlyNew then
    begin
      SendCodeStr(NewsServer, 500);
      EXIT;
    end; { if }

  SendCodeStr(NewsServer, 215);

  ListMode := ExtractWord(CommandStr, 1, defExtractword, false, false);

  if SUpCase(ListMode) = 'OVERVIEW.FMT' then
    begin
      NewsServer.SendStrLn('Subject:');
      NewsServer.SendStrLn('From:');
      NewsServer.SendStrLn('Date:');
      NewsServer.SendStrLn('Message-ID:');
      NewsServer.SendStrLn('References:');
      NewsServer.SendStrLn('Bytes:');
      NewsServer.SendStrLn('Lines:');
      NewsServer.SendStrLn('.');
      EXIT;
    end; { if }

  {-- If only authenticated users can get a group list, make sure this --------}
  {-- user is authenticated, else abort ---------------------------------------}
  if ReadBit(LineCfg^.NewsServer.Attribute, 3) then
    if NOT Authenticated then
      begin
        {-- First send the end-of-list marker. We already sent the 215 --------}
        NewsServer.SendStrLn('.');

        {-- Now send the "access denied" --------------------------------------}
        SendCodeStr(NewsServer, 480);
        RaLog('!', Format('[NEWSSRV] [%s] Listing of groups denied - not authenticated', ['%']));

        EXIT;
      end; { if }


  {-- Initialize the "messages.ra" and "messages.ele" files -------------------}
  New(Message_F, Init);
  New(EleMsg_F, Init);

  Message_F^.Assign(MessagesFileName);
  Message_F^.FileMode := ReadMode + DenyNone;

  EleMSG_F^.Assign(MessageEleFileName);
  EleMSG_F^.FileMode := ReadMode + DenyNone;

  if NOT EleMSG_F^.Open(1) then
    begin
      Dispose(EleMSG_F, Done);
      Dispose(Message_F, Done);
      EXIT;
    end; { if }

  {-- If both filex exist, loop through them ----------------------------------}
  if Message_F^.Open(1) then
    begin

      {-- loop ----------------------------------------------------------------}
      While NOT Message_F^.EOF do
        begin
          {-- Read the actual errors, on error break out of the loop ----------}
          Message_F^.BlkRead(MessageInf, SizeOf(MessageRecord));
          if Message_F^.IoResult > 00 then BREAK;

          EleMSG_F^.BlkRead(EleMessage, SizeOf(EleMessageRecord));
          if EleMSG_F^.IoResult > 00 then BREAK;

          {-- We assume area acess unless the newsserver is setup otherwise ---}
          HasAreaAccess := TRUE;
          if ReadBit(LineCfg^.NewsServer^.Attribute, 3) then
            begin
              {-- Only grant access when the user is authenticated ------------}
              {-- the actual area setting overrides this one though -----------}
              if (EleMessage.AccessSettings in [nwsUseBoth]) then
                begin
                  if (NOT CheckMsgAreaAccess(MessageInf, false, true, 0,
                     nwsSrvConnection(Connections[ServerSlot].SrvData^).Userinf)) then
                    begin
                      HasAreaAccess := FALSE;
                    end; { if }
                end
                  {-- read access isnt denied, so just grant access -----------}
                  else HasAreaAccess := true;
            end; { if }

          {-- Make sure its an NEWSSRV enabled area etc -----------------------}
          if ReadBit(EleMessage.Attribute, 0) then
           if MessageInf.Name <> '' then
            if EleMessage.GroupName <> '' then
             if MessageInf.AreaNum <> 00 then
              if HasAreaAccess then
                begin
                   if (EleMessage.AccessSettings in [nwsPostAlways, nwsPostUseSettings, nwsUseBoth]) then
                     PostMsgChar := 'y'
                      else PostMsgChar := 'n';

                   GetMsgAreaStats(MessageInf, EleMessage, LastMsg, FirstMsg, LastMsg);
                   NewsServer.SendStrLn(Format('%s %d %d %s',
                                        [EleMessage.GroupName,
                                         LastMsg,
                                         FirstMsg,
                                         PostMsgChar]));
                 end; { if }

        end; { while }
    end; { if }

  {-- Close the files etc. ----------------------------------------------------}
  Dispose(Message_F, Done);
  Dispose(EleMsg_F, Done);

  {-- And always send a closing list marker -----------------------------------}
  NewsServer.SendStrLn('.');
end; { proc. SendGroupList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Firstfield(FieldName, MsgText: String; var S: AnsiString): Boolean;
begin
  if SUpCase(Copy(MsgText, 1, Length(FieldName))) = SUpCase(FieldName) then
   begin
     S := Trim(Copy(MsgText, Length(Fieldname) + 1, 255));
     FirstField := true;
   end
    else FirstField := false;
end; { func. FirstField }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetKludgeLine(var MsgPtr: AbsMsgPtr; KludgeID: String): String;
var TempStr: String;
begin
  GetKludgeLine := '';

  MsgPtr^.MsgTxtStartUp;

  While NOT MsgPtr^.EOM do
    begin
      TempStr := MsgPtr^.GetString(80);

      if TempStr[1] = ^A then
       if SupCase(Copy(TempStr, 2, Length(KludgeID))) = KludgeID then
        begin
          GetKludgeLine := Trim(Copy(TempStr, Length(KludgeID) + 2, Length(TempStr)));
          BREAK;
        end; { if }
    end; { while }

  MsgPtr^.MsgTxtStartUp;
end; { func. GetKludgeline }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function MakeHostEmail(FromStr, Name: String; MsgPtr: AbsMsgPtr): String;
var TempStr  : String;
    Addr     : AddrType;
begin
  {-- Is there an original email address, perhaps? --------------------------}
  TempStr := GetKludgeLine(MsgPtr, 'RFC850-From: ');
  if TempStr <> '' then
    begin
      MakeHostEmail := TempStr;
      EXIT;
    end; { if }

 {-- If not, create an FTN style one ----------------------------------------}
 MsgPtr^.GetOrig(Addr);
 MakeHostEmail := Articles.MakeHostEmail(GlobEmailHostName, FromStr, Addr);
end; { func. MakeHostEmail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CreateNewMsgID: String;
var Node    : AddrType;
    TempStr : String;
begin
  FillChar(Node, SizeOf(Node), 00);
  TempStr := MakeMsgIdKludge(Node);

  CreateNewMsgID := ExtractWord(TempStr, 3, defExtractWord, false, false) +
                    ThisHostName;
end; { func. CreateNewMsgID }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SendMsgHeader(var NewsServer: TTcpClient;
                        var MsgPtr: AbsMsgPtr;
                        const MessageID,
                              NewsGroupName: String);
var FromStr,
    SubjStr  : String;
    CharStr  : String;
    TempStr  : String;
    ReplyTo  : String;
begin
  { Relay-Version? }
  { Path }
  CharStr := GetKludgeLine(MsgPtr, 'CHRS: ');
  if CharStr = '' then CharStr := 'IBMPC';
  if Pos(#32, CharStr) > 0 then Delete(CharStr, Pos(#32, CharStr), Length(Charstr));

  ReplyTo := GetKludgeLine(MsgPtr, 'REPLY: ');
  if Pos('@', ReplyTO) = 0 then
   if ReplyTo <> '' then
     begin
       ReplyTo := ExtractWord(ReplyTo, 2, defExtractWord, false, false);
       ReplyTo := ReplyTo + ThisHostName;
     end; { if }

  FromStr := MsgPtr^.GetFrom;
  SubjStr := MsgPtr^.GetSubj;
  if NOT ConvertToLatin(CharStr, FromStr) then ;
  if NOT ConvertToLatin(CharStr, SubjStr) then ;

  NewsServer.SendStrLn(Format('X-News-Server: %s', [Format(ServerGreeting, [GlobalCfg^.RaConfig^.SystemName])]));
  NewsServer.SendStrLn(Format('Date: %s', [MakeNNTPdate(MsgPtr^.GetTime, MsgPtr^.GetDate)]));
  NewsServer.SendStrLn(Format('Message-ID: <%s>', [MessageID]));
  if ReplyTo <> '' then
    NewsServer.SendStrln(Format('References: <%s>', [ReplyTo]));
  NewsServer.SendStrLn(Format('From: %s', [MakeHostEmail(FromStr, GlobalCfg^.RaConfig^.SystemName, MsgPtr)]));
  NewsServer.SendStrLn(Format('Newsgroups: %s', [NewsGroupName]));
  NewsServer.SendStrLn(Format('Subject: %s', [SubjStr]));

  {---------------------------- Send all extra headers ----------------------}
  MsgPtr^.MsgTxtStartUp;

  While NOT MsgPtr^.EOM do
    begin
      TempStr := MsgPtr^.GetString(250);

      if Length(TempStr) > 2 then
       if TempStr[1] = ^A then
         begin
           if Copy(TempStr, 2, 7) = 'RFC850-' then
              NewsServer.SendStrLn(Copy(TempStr, 9, Length(TempStr)));
         end; { if }
    end; { while }
end; { proc. SendMsgHeader }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SendMsgText(var NewsServer: TTcpClient;
                      var MsgPtr: AbsMsgPtr);
var TempStr: String;
    CharStr: String;
begin
  {-- Initialize some variables --------------------------------------------}
  MsgPtr^.MsgTxtStartUp;
  CharStr := 'IBMPC';

  While NOT MsgPtr^.EOM do
    begin
      {-- get the message text ---------------------------------------------}
      TempStr := MsgPtr^.GetString(80);

      {-- if this is not a kludge, display it to the user ------------------}
      if Copy(TempStr, 1, 1) <> ^A then
        begin
          if TempStr = '.' then TempStr := '..';

          if NOT ConvertToLatin(CharStr, TempStr) then ;
          NewsServer.SendStrLn(TempStr);
        end { if }
          else begin
                 if SupCase(Copy(TempStr, 2, Length('CHRS: '))) = 'CHRS: ' then
                   begin
                     CharStr := Trim(Copy(TempStr, Length('CHRS: ') + 2, Length(TempStr)));
                     if Pos(#32, CharStr) > 0 then
                       Delete(CharStr, Pos(#32, CharStr), Length(CharStr));
                   end; { if }

               end; { if }
    end; { while }
end; { proc. SendMsgText }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SetNewsGroupStat(GroupName: String;
                           ReadNum,
                           WriteNum,
                           DenyNum  : Longint);
var Stats_F  : pFileObj;
    StatInf  : NewsGroupStat;
    DidUpdate: Boolean;
begin
  DidUpdate := false;

  New(Stats_F, Init);
  Stats_F^.Assign(NwsGroupStatName);
  Stats_F^.FileMode := ReadWriteMode + DenyNone;
  if NOT Stats_F^.OpenOrCreate(1) then
    begin
      EXIT;
    end; { if }

  {----------------- If it doesn't exist then create a header ---------------}
  if Stats_F^.FileSize = 0  then
    begin
      Fillchar(StatInf, SizeOf(StatInf), #00);
      StatInf.LastAccessed := DateStrY2k;
      StatInf.GroupName := TimeStr(false, false);
      Stats_F^.BlkWrite(StatInf, SizeOf(StatInf));      { Write this header }
    end { if }
      else Stats_F^.BlkRead(StatInf, SizeOf(StatInf));    { Skip the header }

  {---------------------- Search for the already written information --------}
  While NOT Stats_F^.EOF do
    begin
      Stats_F^.BlkRead(StatInf, SizeOf(StatInf));

      if SupCase(Trim(StatInf.GroupName)) = SUpCase(Trim(GroupName)) then
         begin
           StatInf.NumRead   := ReadNum;
           StatInf.NumWrite   := WriteNum;
           StatInf.NumDeny   := DenyNum;
           StatInf.LastAccessed := JDates.DateStrY2k;

           Stats_F^.Seek(Stats_F^.FilePos - SizeOf(StatInf));
           Stats_F^.BlkWrite(StatInf, SizeOf(StatInf));
           DidUpdate := true;

           BREAK;
         end; { if }

    end; { while }

  {---------------------- Search for the already written information --------}
  if NOT DidUpdate then
    begin
      FillChar(StatInf, SizeOf(StatInf), #00);

      StatInf.GroupName := GroupName;
      StatInf.NumRead   := ReadNum;
      StatInf.NumWrite   := WriteNum;
      StatInf.NumDeny   := DenyNum;
      StatInf.LastAccessed := JDates.DateStrY2k;

      Stats_F^.Seek(Stats_F^.FileSize);
      Stats_F^.BlkWrite(StatInf, SizeOf(StatInf));
    end; { if }

  Dispose(Stats_F, Done);
end; { proc. SetNewsGroupStat }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetArticleID(var MsgPtr: AbsMsgPtr): String;
var ArticleID: String;
    TempStr  : String;
begin
  ArticleID := ExtractWord(GetKludgeLine(MsgPtr, 'MSGID:'),
                           2, defExtractWord, false, false);

  if ArticleID = '' then      { Generate an unique one, but always the same! }
    begin
      MsgPtr^.MsgTxtStartUp;
      TempStr := '';

      While NOT MsgPtr^.EOM do
       begin
         TempStr := TempStr + Copy(MsgPtr^.GetNoKludgeStr(80), 3, 1);
       end; { if }

      TempStr := Copy(TempStr, 1, 10);

      ArticleID := MsgPtr^.GetDate + MsgPtr^.GetTime;
      ArticleID := ArticleID + Copy(MsgPtr^.GetSubj, 1, 5) + TempStr;
      ArticleID := FilterString(ArticleID, ['A'..'Z', 'a'..'z', '0'..'9']);
    end; { if }

  if Pos('@', ArticleID) = 0 then
    ArticleID := ArticleID + ThisHostName;

  GetArticleID := ArticleID;
end; { func. GetArticleID }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function SearchArticleNumber(var MsgPtr: AbsMsgPtr; const TempStr: String): Longint;
begin
  SearchArticleNumber := -1;

  MsgPtr^.SeekFirst(0);
  While MsgPtr^.SeekFound do
    begin
      MsgPtr^.MsgStartUp;
      MsgPtr^.MsgTxtStartUp;

      if ('<' + GetArticleID(MsgPtr) + '>') = TempStr then
        begin
          SearchArticleNumber := MsgPtr^.GetMsgNum;
          BREAK;
        end; { if }

      MsgPtr^.SeekNext;
    end; { while }
end; { func. SearchArticleNumber }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CloseMsgArea(var CurrentMsgPtr: AbsMsgPtr;
                      var AreaInf: MessageRecord): Boolean;
begin
  if CurrentMsgPtr <> nil then
    MkOpen.CloseMsgArea(CurrentMsgPtr);

  AreaInf.AreaNum := 0;
  CurrentMsgPtr := nil;
end; { func. CloseMsgArea }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function OpenMsgArea(const ServerSlot: Longint;
                     const CurrentGroup : MessageRecord;
                     const CurrentElegroup: EleMessageRecord): Boolean;
begin
  {-- Check if we cant reuse it ---------------------------------------------}
  if nwsSrvConnection(Connections[ServerSlot].SrvData^).MsgPtr <> nil then
    begin
      {-- See if the currently opened and the one we want, match ------------}
      if (CurrentGroup.AreaNum <> nwsSrvConnection(Connections[ServerSlot].SrvData^).AreaInf.AreaNum) then
       with nwsSrvConnection(Connections[ServerSlot].SrvData^) do
        begin
          {-- They dont match, too bad --------------------------------------}
          CloseMsgArea(MsgPtr,
                       AreaInf);
        end { if }
          else begin
                 OpenMsgArea := TRUE;
                 EXIT;  {-- They *do* match -- }
               end; { else }
    end; { if }

  With nwsSrvConnection(Connections[ServerSlot].SrvData^) do
    if NOT OpenOrCreateMsgArea(MsgPtr,
                               MakeMsgId(CurrentGroup.AreaNum,
                                         CurrentGroup.JamBase,
                                         CurrentGroup.Attribute,
                                         CurrentEleGroup.Attribute)) then
      begin
        OpenMsgArea := FALSE;
        MsgPtr := NIL;
      end { if }
        else begin
               OpenMsgArea := TRUE;
               AreaInf := CurrentGroup;
             end; { else }
end; { func. OpenMsgArea }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SendArticle(var NewsServer: TTcpClient;
                      var CommandStr, TempStr : String;
                      var CurrentGroup: MessageRecord;
                      var CurrArticle: Longint;
                      const Authenticated: Boolean;
                      const ConnectIP: String;
                      const ServerSlot: Longint);
var SearchArticle: Longint;
    ArticleID    : String;
    EleMsgInf    : EleMessageRecord;
    ReadNum      : Longint;
    WriteNum     : Longint;
    DenyNum      : Longint;
    SysAccess    : Boolean;
begin
  GetEleMessageRecord(EleMsgInf, CurrentGroup.AreaNum, true);
  OpenMsgArea(ServerSlot, CurrentGroup, EleMsgInf);

  With nwsSrvConnection(Connections[ServerSlot].SrvData^) do
    if (MsgPtr = nil) OR (CurrentGroup.Name = '') then
      begin
        SendCodeStr(NewsServer, 412);
        CloseMsgArea(MsgPtr, AreaInf);
        EXIT;
      end; { if }

  {-- Make sure te user has access ------------------------------------------}
  if EleMsgInf.AccessSettings in [nwsUseBoth] then
   if (NOT CheckMsgAreaAccess(CurrentGroup, false, true, 0,
         nwsSrvConnection(Connections[ServerSlot].SrvData^).Userinf)) OR (NOT Authenticated) then
     begin
       SendCodeStr(NewsServer, 480);
       RaLog('!', Format('[NEWSSRV] [%s] Reading of %s denied - not authorized', [ConnectIP, CurrentGroup.Name]));

       GetNewsGroupStat(EleMsgInf.GroupName, ReadNum, WriteNum, DenyNum);
       SetNewsGroupStat(EleMsgInf.GroupName, ReadNum, WriteNum, DenyNum + 1);

       EXIT;
     end; { if }

  {-- If this is a ng with only private messages, user has to be auth'd -----}
  if CurrentGroup.MsgKinds = elemsgPrivate then
    if NOT Authenticated then
      begin
        SendCodeStr(NewsServer, 480);
        RaLog('!', Format('[NEWSSRV] [%s] Reading of %s denied - not authorized (trying to read private area)', [ConnectIP, CurrentGroup.Name]));

        GetNewsGroupStat(EleMsgInf.GroupName, ReadNum, WriteNum, DenyNum);
        SetNewsGroupStat(EleMsgInf.GroupName, ReadNum, WriteNum, DenyNum + 1);

        EXIT;
      end; { only private messages }


  if TempStr = '' then
    begin
      SearchArticle := CurrArticle;
    end { if }
      else begin
            if TempStr[1] = '<' then
              SearchArticle := SearchArticleNumber(nwsSrvConnection(Connections[ServerSlot].SrvData^).MsgPtr, TempStr)
               else SearchArticle := FVal(TempStr);
           end; { else }


  if CommandStr = 'ARTICLE' then
    if SearchArticle <= 0 then
      begin
        SendCodeStr(NewsServer, 430);
        EXIT;
      end; { if }

  {------------------- Get the newsgroup information and update it ----------}
  GetNewsGroupStat(EleMsgInf.GroupName, ReadNum, WriteNum, DenyNum);
  SetNewsGroupStat(EleMsgInf.GroupName, ReadNum + 1, WriteNum, DenyNum);

  {-- Check if we have SysOp access -----------------------------------------}
  SysAccess := SysOpAccess(CurrentGroup, nwsSrvConnection(Connections[ServerSlot].SrvData^).UserInf);

  With nwsSrvConnection(Connections[ServerSlot].SrvData^) do
    begin
      MsgPtr^.SeekFirst(SearchArticle);
      if MsgPtr^.SeekFound then
        begin
          MsgPtr^.MsgStartUp;
          MsgPtr^.MsgTxtStartUp;

          {------------------- Extract the MSGID kludge ---------------------}
          ArticleID := GetArticleID(MsgPtr);

          {-- Check if the user may actually read this msg ------------------}
          if NOT ReadMsgAccess(MsgPtr^.GetTo, SysAccess, MsgPtr^.IsPriv, UserInf) then
            begin
              SendCodeStr(NewsServer, 430);             { Article not found }
              RaLog('!', Format('[NEWSSRV] [%s] Reading of %s denied - not authorized (reading private msg not for user)', [ConnectIP, CurrentGroup.Name]));

              GetNewsGroupStat(EleMsgInf.GroupName, ReadNum, WriteNum, DenyNum);
              SetNewsGroupStat(EleMsgInf.GroupName, ReadNum, WriteNum, DenyNum + 1);
              GenericSleep(100);
              EXIT;
            end; { if }


          {----------------- Send the line indicating we have a msg ---------}
          if CommandStr = 'ARTICLE' then
            NewsServer.SendStrLn(Format('220 %d <%s> article retrieved - head and body follows',
                                        [SearchArticle, ArticleID]));

          if CommandStr = 'HEAD' then
            NewsServer.SendStrLn(Format('221 %d <%s> article retrieved - head follows',
                                        [SearchArticle, ArticleID]));

          if CommandStr = 'BODY' then
            NewsServer.SendStrLn(Format('222 %d <%s> article retrieved - body follows',
                                        [SearchArticle, ArticleID]));

          if CommandStr = 'STAT' then
            NewsServer.SendStrLn(Format('223 %d <%s> article retrieved - request text seperately',
                                        [SearchArticle, ArticleID]));


          {------------------ Set the current article pointer ---------------}
          if TempStr[1] <> '<' then CurrArticle := SearchArticle;

          {-- Make a DebugObj^.DebugLog entry -----------------------------------------}
          {$IFDEF WITH_DEBUG}
            DebugObj.DebugLog(logTcpip, 'Start of article: ' + CommandStr);
          {$ENDIF}

          {---------------- Actually send the message header/bodys ----------}
          if (CommandStr = 'ARTICLE') OR (CommandStr = 'HEAD') then
            SendMsgHeader(NewsServer, MsgPtr,
                          ArticleID,
                          EleMsgInf.GroupName);

          if (CommandStr = 'ARTICLE') then
            NewsServer.SendStrLn('');

          if (CommandStr = 'ARTICLE') OR (CommandStr = 'BODY') then
            SendMsgText(NewsServer, MsgPtr);

          if (CommandStr = 'ARTICLE') OR (CommandStr = 'BODY') OR
              (CommandStr  = 'HEAD') then NewsServer.SendStrLn('.');

          {-- Make a DebugObj^.DebugLog entry -----------------------------------------}
          {$IFDEF WITH_DEBUG}
            DebugObj.DebugLog(logtcpip, 'End of article: ' + CommandStr);
          {$ENDIF}
        end
          else SendCodeStr(NewsServer, 430);
  end; { with }


  {-- Give up some time to prevent CPU hogging ------------------------------}
  GenericSleep(0);
  if news_IsRunSlow then GenericSleep(100);
end; { proc. SendArticle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SelectGroup(var CurrentGroup: MessageRecord;
                      var NewsServer: TTcpClient;
                      var TempStr : String;
                      var AbortSession: Boolean;
                      var CurrArticle: Longint;
                      var FirstArticle: Longint;
                      var LastArticle: Longint;
                      var ConnectIP: String;
                      var ServerSlot: Longint);
var LongAr: Array[1..2] of Longint;
    EleMsgInf: EleMessageRecord;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'SelectGroup:- (begin)');
    DebugObj.DebugLog(logString, 'SelectGroup:- (TempStr = '+TempStr+')');
  {$ENDIF}

  LongAr[1] := ValidGroup(TempStr);

{$IFDEF MARTYDEBUG}
{!!!} RaLog('!', '-Marty Log- SelectGroup() begin');
{!!!} RaLog('!', '-Marty Log- TempStr="'+TempStr+'"');
{!!!} RaLog('!', '-Marty Log- LongAr[1] = ' + FStr(LongAr[1]));
{$ENDIF}

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'SelectGroup:- LongAr[1] = '+FStr(LongAr[1]));
  {$ENDIF}

  if LongAr[1] <> -1 then
    begin
      GetMessageRecord(CurrentGroup, LongAr[1], true);
      GetEleMessageRecord(EleMsgInf, LongAr[1], true);
      RaLog('>', '[NEWSSRV] Selected group "' + CurrentGroup.Name + '"');

      {$IFDEF WITH_DEBUG}
        DebugObj.DebugLog(logString, 'SelectGroup:- CurrentGroup.Name = '+CurrentGroup.Name);
        DebugObj.DebugLog(logString, 'SelectGroup:- CurrentGroup.MsgId= '+MakeMsgId(CurrentGroup.AreaNum,
                                                                 CurrentGroup.JamBase,
                                                                 CurrentGroup.Attribute,
                                                                 EleMsgInf.Attribute));
      {$ENDIF}


      if NOT OpenMsgArea(ServerSlot, CurrentGroup, EleMsgInf) then
           begin
             With nwsSrvConnection(Connections[ServerSlot].SrvData^) do
                CloseMsgArea(MsgPtr, AreaInf);

             SendCodeStr(NewsServer, 503);
             AbortSession := true;
             EXIT;
           end; { if }

      With nwsSrvConnection(Connections[ServerSlot].SrvData^) do
        begin
          MsgPtr^.SeekFirst(01);
          if MsgPtr^.SeekFound then
            begin
              MsgPtr^.MsgStartUp;
              MsgPtr^.MsgTxtStartUp;

              LongAr[01] := MsgPtr^.NumberOfMsgs;
              Firstarticle := MsgPtr^.GetMsgNum;
              LastArticle := MsgPtr^.GetHighMsgNum;
              CurrArticle := MsgPtr^.GetMsgNum;
            end
              else begin
                     LongAr[01] := 0;
                     FirstArticle := 0;
                     LastArticle := 0;
                     CurrArticle := 0;
                   end; { if }

          NewsServer.SendStrLn(Format('211 %d %d %d %s',
                                      [LongAr[1],
                                       FirstArticle,
                                       LastArticle,
                                       TempStr]));

        end; { with }
    end
        else begin
               SendCodeStr(NewsServer, 411);
               RaLog('>', Format('[NEWSSRV] [%s] Group %s requested - non existent on this server', [ConnectIP, CurrentGroup.Name]));
             end; { else }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'SelectGroup:- ( end )');
  {$ENDIF}
end; { proc. SelectGroup }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SelectPreviousArticle(var NewsServer: TTcpClient;
                                var CurrentArticle: Longint;
                                var CurrentGroup: MessageRecord;
                                var CurrentEleGroup: EleMessageRecord;
                                var FirstArticle: Longint;
                                var CommandStr: String;
                                var ServerSlot: Longint);
var ArticleID: String;
begin
  OpenMsgArea(ServerSlot, CurrentGroup, CurrentEleGroup);

  if (nwsSrvConnection(Connections[ServerSlot].SrvData^).MsgPtr = nil) OR (CurrentGroup.Name = '') then
    begin
      SendCodeStr(NewsServer, 412);

      With nwsSrvConnection(Connections[ServerSlot].SrvData^) do
        CloseMsgArea(MsgPtr, AreaInf);

      EXIT;
    end; { if }

  if (CurrentArticle = 0) then
    begin
      SendCodeStr(NewsServer, 420);
      EXIT;
    end; { if }

  if CommandStr = 'LAST' then
    if (CurrentArticle = FirstArticle) then
      begin
        SendCodeStr(NewsServer, 422);
        EXIT;
      end; { if }

  With nwsSrvConnection(Connections[ServerSlot].SrvData^) do
    begin
      {--------------------------- Perform the actual seek ----------------------}
      if CommandStr = 'LAST' then
        begin
          MsgPtr^.SeekFirst(CurrentArticle);
          MsgPtr^.SeekPrior;
        end
          else begin
                 MsgPtr^.SeekFirst(CurrentArticle);
                 MsgPtr^.SeekNext;
               end; { if }


      {--------------------------- Send back a response -------------------------}
      if MsgPtr^.SeekFound then
        begin
          MsgPtr^.MsgStartup;
          MsgPtr^.MsgTxtStartup;
          CurrentArticle := MsgPtr^.GetMsgNum;
          ArticleID := GetArticleID(MsgPtr);

          NewsServer.SendStrLn(Format('223 %d <%s> article retrieved - request text seperately',
                                      [CurrentArticle,
                                       ArticleID]));
        end
          else begin
                 SendCodeStr(NewsServer, 422);
                 EXIT;
               end; { if }
    end; { with }
end; { proc. SelectPreviousArticle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SendHelpText(var NewsServer: TTcpClient);
begin
  NewsServer.SendStrLn('100 Help text follows');
  NewsServer.SendStrLn('ARTICLE [MessageID | Number]');
  NewsServer.SendStrLn('BODY    [MessageID | Number]');
  NewsServer.SendStrLn('HEAD    [MessageID | Number]');
  NewsServer.SendStrLn('STAT    [MessageID | Number]');
  NewsServer.SendStrLn('GROUP   newsgroup');
  NewsServer.SendStrLn('HELP');
  NewsServer.SendStrLn('LAST');
  NewsServer.SendStrLn('LIST');
  NewsServer.SendStrLn('NEXT');
  NewsServer.SendStrLn('XOVER');
  NewsServer.SendStrLn('.');
end; { proc. SendHelpText }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function AllowedInArea(const IpStr: String; UserName, AreaName: String): Boolean;
var Allow_F : pFileObj;
    AllowStr: String;
begin
  AllowedInArea := false;
  Username := SUpCase(Trim(Username));

  New(Allow_F, Init);
  Allow_F^.Assign(ForceBack(NwsAllowPath) + 'ALLOW-' + AreaName);
  if NOT Allow_F^.Open(1) then
    begin
      AllowedInArea := TRUE;
      Dispose(Allow_F, Done);
      EXIT;
    end; { if }

  While NOT Allow_F^.EOF do
    begin
      AllowStr := '%';
      While (AllowStr[1] = '%') AND (NOT Allow_F^.EOF) do
        Allow_F^.ReadLn(AllowStr);

      if AllowStr[1] <> '%' then
        begin
          AllowStr := SupCase(Trim(AllowStr));

          if AllowStr = UserName then
            begin
              AllowedInArea := TRUE;
              BREAK;
            end; { if }
        end; { if }
    end; { while }

  Dispose(Allow_F, Done);
end; { func. BannedFromArea }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure PostArticle(var NewsServer: TTcpClient;
                      var CommandStr: String;
                      var UserName  : String;
                      Authenticated : boolean;
                      var ConnectIP : String;
                      var ServerSlot: Longint);

var PostFailed  : Boolean;
    TempStr     : AnsiString;
    ShortMsgStr : ShortString;
    ShortStr    : ShortString;
    MsgRec      : MsgPostRecord;
    TempF       : pFileObj;
    TempName    : String;
    Counter     : Longint;
    UserExtInf  : UserExtensionRecord;

procedure PostToArea(const NewsGroup: String;
                     const TextFile: String);
var TempF       : Text;
    AreaNr      : Longint;
    Currentgroup: MessageRecord;
    CurrentEle  : EleMessageRecord;
    TempStr     : AnsiString;
    AddrStr     : AnsiString;
    TempAddr    : NetAddress;
    FromAddr    : AddrType;
    ReadNum     : Longint;
    WriteNum    : Longint;
    DenyNum     : Longint;
    DateStr,
    Timestr     : String;
begin
  AreaNr := ValidGroup(Newsgroup);
  if AreaNr <> -1 then
    begin
      {-- Note: We do not support msgkinds because all messages posted ------}
      {--       to a newsgroup are considered public since you cannot -------}
      {--       specify a recipient address. Read-only newsgroups -----------}
      {--       need to be controlled by nwsPostNever rather than by r/o ----}
      GetMessageRecord(CurrentGroup, AreaNr, true);
      GetEleMessageRecord(CurrentEle, AreaNr, true);
      if (CurrentGroup.Name = '') OR (CurrentGroup.AreaNum = 0) then EXIT;

      {-- Check access to this newsgroup ------------------------------------}
      if CurrentEle.AccessSettings = nwsPostNever then
        begin
          SendCodeStr(NewsServer, 440);
          EXIT;
        end; { if }

      if CurrentEle.AccessSettings in [nwsPostUseSettings, nwsUseBoth] then
        if NOT Authenticated then
          begin
            SendCodeStr(NewsServer, 480);
            RaLog('!', Format('[NEWSSRV] [%s] Post to %s denied - not authenticated', [ConnectIP, CurrentGroup.Name]));

            GetNewsGroupStat(NewsGroup, ReadNum, WriteNum, DenyNum);
            SetNewsGroupStat(NewsGroup, ReadNum, WriteNum, DenyNum + 1);

            EXIT;
          end; { if }

      if CurrentEle.AccessSettings in [nwsPostUseSettings, nwsUseboth] then
        if Authenticated then
          begin
            if NOT CheckMsgAreaAccess(CurrentGroup, false, false, 0, nwsSrvConnection(Connections[ServerSlot].SrvData^).UserInf) then
              begin
                SendCodeStr(NewsServer, 480);
                RaLog('!', Format('[NEWSSRV] [%s] Post to %s denied - not authenticated', [ConnectIP, CurrentGroup.Name]));

                GetNewsGroupStat(NewsGroup, ReadNum, WriteNum, DenyNum);
                SetNewsGroupStat(NewsGroup, ReadNum, WriteNum, DenyNum + 1);

                EXIT;
              end; { if }
          end; { if }

      if CurrentEle.AccessSettings in [nwsPostUseSettings, nwsUseBoth] then
       if Authenticated then
         if NOT AllowedInArea(ConnectIP, UserName, Newsgroup) then
           begin
             SendCodeStr(NewsServer, 440);
             RaLog('!', Format('[NEWSSRV] [%s] Post to %s denied - user is not authenticated for this area', [ConnectIP, Newsgroup]));

             GetNewsGroupStat(NewsGroup, ReadNum, WriteNum, DenyNum);
             SetNewsGroupStat(NewsGroup, ReadNum, WriteNum, DenyNum + 1);

             EXIT;
           end; { if }

      {-- Make sure the correct "From:" address is used ---------------------}
      if Authenticated then
        begin
          if ReadBit(CurrentEle.Attribute, 1) then       { Override from address }
            begin
              {-- Check if we have to use a forced alias ------------------------}
              if ReadBit(CurrentGroup.Attribute, 5) then
                MsgRec.FromWho := nwsSrvConnection(Connections[ServerSlot].SrvData^).UserInf.Handle;

              {-- Check if we are forced to use the realname --------------------}
              if NOT ReadBit(CurrentGroup.Attribute, 5) then
                begin
                  MsgRec.FromWho := nwsSrvConnection(Connections[ServerSlot].SrvData^).UserInf.Name;
                end; { if }
            end; { if }
        end; { if authenticated }

    end { if }
     else EXIT;

  RaLog('!', Format('[NEWSSRV] [%s] Post to %s', [ConnectIP, CurrentGroup.Name]));
  NntpDateToString(MsgRec.Date, DateStr, TimeStr);
  GetNewsGroupStat(NewsGroup, ReadNum, WriteNum, DenyNum);
  SetNewsGroupStat(NewsGroup, ReadNum, WriteNum + 1, DenyNum);

  Assign(TempF, TextFile);
  {$i-} System.Reset(TempF); {$i+}
  if IoResult > 00 then EXIT;


  {-- Now actually open the message area ------------------------------------}
  OpenMsgArea(ServerSlot, CurrentGroup, CurrentEle);

  With nwsSrvConnection(Connections[ServerSlot].SrvData^) do
    begin
      Case CurrentGroup.typ of
          LocalMail : MsgPtr^.SetMailType(mmtNormal);
          NetMail   : begin
                        MsgPtr^.SetMailType(mmtNetMail);
                        MsgPtr^.SetNetMail(true);
                      end; { Set NetMail }
          EchoMail  : begin
                        MsgPtr^.SetMailType(mmtEchoMail);
                      end; { if }
    (*  InterNet  :  *)
    (*  NewsGroup :  *)
      end; { CurrentGroup.Typ }

      GetAddress(CurrentGroup.AkaAddress, TempAddr);
      FromAddr.Zone := TempAddr.Zone;
      FromAddr.Net  := TempAddr.Net;
      FromAddr.Node := TempAddr.Node;
      FromAddr.Point:= TempAddr.Point;
      AddrStr := StUtils.AddrStr(FromAddr);

      MsgPtr^.StartNewMsg;
      MsgPtr^.SetLocal(True);
      MsgPtr^.SetEcho (True);
      MsgPtr^.SetNetMail(CurrentGroup.Typ in [NetMail]);
      MsgPtr^.SetFrom(MsgRec.FromWho);
      MsgPtr^.SetTo('All');
      MsgPtr^.SetSubj(MsgRec.Subject);
      MsgPtr^.SetTime(TimeStr);
      MsgPtr^.SetDate(DateStr);
      MsgPtr^.DoKludgeLn(^A'ORIG-IP: ' + ConnectIP);
      MsgPtr^.DoKludgeLn(Format(^A'MSGID: %s@NEWSSRV %s', [AddrStr, MsgRec.MessageID]));
      if MsgRec.ReplyID <> '' then
        MsgPtr^.DoKludgeLn(Format(^A'REPLY: %s@NEWSSRV %s', [AddrStr, MsgRec.ReplyID]));
      MsgPtr^.DoKludgeLn(^A'CHRS: IBMPC 2');
      MsgPtr^.DoKludgeLn(^A'TID: ' + MsgRec.NewsReader);

      While NOT Eof(TempF) do
        begin
          {-- read the actual message content -----------------------------}
          {$i-} ReadLn(TempF, TempStr); {$i+}
          if IoResult > 00 then BREAK;

          {-- now write the message ---------------------------------------}
          MailDoLongString(MsgPtr, TempStr);
        end; { while }

      AddOriginLine(MsgPtr, CurrentGroup);
      NewWriteMsg(MsgPtr);

      {$i-} Close(TempF); {$i+}
      if IoResult > 00 then ;
    end; { with }
end; { proc. PostToArea }


begin
  if NOT PostingAllowed then
    begin
      SendCodeStr(NewsServer, 440);
      EXIT;
    end; { if }

  {---------------- Let user know he/she should send their msg --------------}
  SendCodeStr(NewsServer, 340);

  PostFailed := false;
  TempStr := '.';
  FillChar(MsgRec, SizeOf(MsgRec), #00);

  {------------------------ Get the message-header --------------------------}
  While (TempStr <> '') AND (NewsServer.ConnectionAlive) do
    begin
       if NewsServer.RecvStrLn(TempStr, false) then
         begin
            if NOT FirstField('Subject:', TempStr, MsgRec.Subject) then
             if NOT FirstField('Message-ID:', TempStr, MsgRec.MessageID) then
{ commented out on 31-aug-2002: if NOT FirstField('Date:', TempStr, MsgRec.Date) then }
               if NOT FirstField('Newsgroups:', TempStr, MsgRec.Newsgroups) then
                if NOT FirstField('X-News-Reader:', TempStr, MsgRec.NewsReader) then
                 if NOT FirstField('From:', TempStr, MsgRec.FromWho) then
                  if NOT FirstField('References:', TempStr, MsgRec.ReplyID) then
                  begin
                    if Tempstr <> '' then
                     if MsgRec.HdrCnt < MaxExtraHdrs then
                       begin
                         Inc(MsgRec.HdrCnt);
                         MsgRec.ExtraHdrs[MsgRec.HdrCnt] := TempStr;
                       end; { if }
                  end; { if }
         end { if }
          else begin
                 TempStr := '.';
                 GenericSleep(10);
               end; { if }

    end; { while }

  if MsgRec.MessageID = '' then
    MsgRec.MessageID := CreateNewMsgID;

  {------------------- Determine wether this was a valid header -------------}
  if (MsgRec.FromWho = '') OR (MsgRec.Subject = '') OR
       (MsgRec.NewsGroups=  '') then PostFailed := true
         else PostFailed := false;

  if Length(MsgRec.ReplyId) >= 1 then
   if MsgRec.MessageID[1] = '<' then
        begin
          Delete(MsgRec.MessageID, 1, 1);
          Delete(MsgRec.MessageID, Length(MsgRec.MessageID), 1);
        end; { if }

  if Length(MsgRec.ReplyId) >= 1 then
   if MsgRec.ReplyID[1] = '<' then
        begin
          Delete(MsgRec.ReplyID, 1, 1);
          Delete(MsgRec.ReplyID, Length(MsgRec.ReplyID), 1);
        end; { if }

  if MsgRec.MessageID = '' then MsgRec.MessageID := CreateNewMsgID;
  if MsgRec.NewsReader = '' then MsgRec.NewsReader := 'EleBBS news server';
  if MsgRec.Date = '' then MsgRec.Date := MakeNNTPdate(TimeStr(true, false),
                                                       DateStr);
  if MsgRec.NewsServer = '' then MsgRec.NewsServer := Format(ServerGreeting, [GlobalCfg^.RaConfig^.SystemName]);

  ShortStr := MsgRec.FromWho;
  ConvertToIBMPc('LATIN-1', ShortStr);
  MsgRec.FromWho := ShortStr;

  ShortStr := MsgRec.Subject;
  ConvertToIBMPc('LATIN-1', ShortStr);
  MsgRec.Subject := ShortStr;

  {------------------------ Get the message-text ----------------------------}
  if NOT PostFailed then
    begin
      Tempname := MsgRec.MessageID + FStr(GetDosdate) + '.$$$';

      New(TempF, Init);
      TempF^.Assign(TempName);
      TempF^.FileMode := ReadWriteMode + DenyAll;
      if NOT TempF^.Create(1) then EXIT;

      {--------------------- Write the extra headers ------------------------}
      if MsgRec.HdrCnt > 0 then
       for Counter := 01 to MsgRec.HdrCnt do
         TempF^.WriteLn(^A + 'RFC850-' + MsgRec.ExtraHdrs[Counter]);

      {-------------- Actually write all the text to the file ---------------}
      TempStr := '';
      While (TempStr <> '.') AND (NewsServer.ConnectionAlive) do
       begin
         if NewsServer.RecvStrLn(TempStr, false) then
           begin
             if TempStr <> '.' then
               begin
                 ShortMsgStr := TempStr;

                 ConvertToIBMPc('LATIN-1', ShortMsgStr);
                 TempF^.WriteLn(ShortMsgStr)
               end; { if }
           end
            else begin
                   TempStr := '';
                   GenericSleep(50);
                 end; { if }
       end; { while }

      Dispose(TempF, Done);

      if NewsServer.DataAvailable then
        NewsServer.RecvStrLn(TempStr, false);

      While MsgRec.NewsGroups <> '' do
        begin
          if Pos(',', MsgRec.Newsgroups) = 0 then
            MsgRec.NewsGroups := MsgRec.NewsGroups + ',';
          TempStr := Copy(MsgRec.NewsGroups, 1, Pos(',', MsgRec.Newsgroups) - 1);
          Delete(MsgRec.NewsGRoups, 1, Length(TempStr) + 01);

          PostToArea(TempStr, TempName);
        end; { while }

        if NOT EraseFile(TempName) then ;
    end; { if }

  {-- Check to see if we increase the post-count -------------------------------}
  if ReadBit(LineCfg^.NewsServer^.Attribute, 2) then
    begin
      if NOT PostFailed then
        With nwsSrvConnection(Connections[ServerSlot].SrvData^) do
          begin
            if UserRecNr >= 0 then
              begin
                GetUserRecord(UserRecNr, UserInf, UserExtInf, false);
                Inc(UserInf.MsgsPosted);                 { Increase the count }
                WriteUserRecord(UserRecNr, UserInf, UserExtInf);
              end; { if }
          end; { if }
    end; { if }

  if PostFailed then
    SendCodeStr(NewsServer, 441)
     else SendCodeStr(NewsServer, 240);
end; { proc. PostArticle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function XOverStr(S: String): String;
var TempStr: String;
    Counter: Longint;
begin
  TempStr := '';

  for Counter := 01 to Length(S) do
    if ((S[Counter] in [#9, #10, #13])) then TempStr := TempStr + #32
      else TempStr := TempStr + S[Counter];

  XOverStr := TempStr;
end; { func. XOverStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SendXOverview(var NewsServer     : TTcpClient;
                        var TempStr        : String;
                        var CurrentGroup   : MessageRecord;
                        var CurrentEleGroup: EleMessageRecord;
                        var CurrentArticle : Longint;
                        var LastArticle    : Longint;
                        var ServerSlot     : Longint);
const
  TabChr = #09;

var EndArticle   : Longint;
    BeginArticle : Longint;
    FromStr,
    SubjStr      : String;
    CharStr      : String;
    MsgLines     : Longint;
    MsgBytes     : Longint;
    SysAccess    : Boolean;
begin
  OpenMsgArea(ServerSlot, CurrentGroup, CurrentEleGroup);

  if (nwsSrvConnection(Connections[ServerSlot].SrvData^).MsgPtr = nil) OR (CurrentGroup.Name = '') then
    begin
      SendCodeStr(NewsServer, 412);


      With nwsSrvConnection(Connections[ServerSlot].SrvData^) do
        CloseMsgArea(MsgPtr, AreaInf);
      EXIT;
    end; { if }

  if (TempStr = '') then
    begin
      BeginArticle := CurrentArticle;
      EndArticle := CurrentArticle;
    end; { if }

  if Pos('-', TempStr) = 0 then
    begin
      BeginArticle := FVal(TempStr);
      EndArticle := FVal(TempStr);
    end; { if }

  if Pos('-', TempStr) = Length(TempStr) then
    begin
      BeginArticle := CurrentArticle;
      EndArticle := LastArticle
    end; { if }

  if (Pos('-', TempStr) <> 0) then
   if (Pos('-', TempStr) <> Length(TempStr)) then
     begin
       BeginArticle := FVal(Copy(TempStr, 1, Pos('-', TempStr) - 1));
       EndArticle := FVal(Copy(TempStr, Pos('-', TempStr) + 1, Length(TempStr)));
     end; { if }

  {------------------------- Start sending the overview ---------------------}
  SendCodeStr(NewsServer, 224);

  {-- Check if we have SysOp access -----------------------------------------}
  SysAccess := SysOpAccess(CurrentGroup, nwsSrvConnection(Connections[ServerSlot].SrvData^).UserInf);

  With nwsSrvConnection(Connections[ServerSlot].SrvData^) do
    begin
      While (BeginArticle <= EndArticle) AND
             (NewsServer.ConnectionAlive) do
        begin
          MsgPtr^.SeekFirst(BeginArticle);

          if MsgPtr^.SeekFound then
            begin
              {-- give up some time --------------------------------------------}
              NewsServer.DoSleep(0);
              if news_IsRunSlow then
                NewsServer.DoSleep(10);

              {-- Check if the user may actually read this msg -----------------}
              if ReadMsgAccess(MsgPtr^.GetTo, SysAccess, MsgPtr^.IsPriv, UserInf) then
                begin
                  CharStr := GetKludgeLine(MsgPtr, 'CHRS: ');
                  if CharStr = '' then CharStr := 'IBMPC';
                  if Pos(#32, CharStr) > 0 then Delete(CharStr, Pos(#32, CharStr), Length(Charstr));

                  MsgPtr^.MsgStartUp;
                  MsgPtr^.MsgTxtStartUp;

                  SubjStr := MsgPtr^.GetSubj;
                  FromStr := MsgPtr^.GetFrom;
                  MsgLines := 0;
                  MsgBytes := 0;

                  if ReadBit(LineCfg^.NewsServer^.Attribute, 1) then  { If retrieve this info, will slw down }
                    begin
                      While NOT MsgPtr^.EOM do
                        begin
                          Inc(MsgLines);
                          Inc(MsgBytes, Length(MsgPtr^.GetNoKludgeStr(80)));
                        end; { while }
                    end; { if }

                  if NOT ConvertToLatin(CharStr, FromStr) then ;
                  if NOT ConvertToLatin(CharStr, SubjStr) then ;

                  NewsServer.SendStrLn(FStr(MsgPtr^.GetMsgNum) +
                                        TabChr + XoverStr(SubjStr) +
                                        TabChr + XOverStr(FromStr) +
                                        TabChr + XOverStr(MakeNntpDate(MsgPtr^.GetTime, MsgPtr^.GetDate)) +
                                        TabChr + XOverStr(GetArticleID(MsgPtr)) +
                                        TabChr + FStr(0) +
                                        TabChr + FStr(MsgBytes) +
                                        TabChr + FStr(MsgLines) +
                                        TabChr + FStr(0));
                end; { if }
            end; { if has access }

          Inc(BeginArticle);
        end; { while }
    end; { with }

  NewsServer.SendStrLn('.');
end; { proc. SendXoverview }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function MatchUserName(var Username, Password: String; var UserNr: Longint; var UserInf: UsersRecord): Boolean;
var Temp      : Longint;
    UserExtInf: UserExtensionRecord;
begin
  Result := false;

  UserName := SUpcase(UserName);
  if UserName = SUpCase(GlobalCfg^.RaConfig^.SystemName) then
   if NOT ReadBit(LineCfg^.NewsServer^.Attribute, 0) then EXIT;

  UserNr := -1;
  Temp := SearchUser(UserName);
  if Temp <> -1 then
    begin
      GetUserRecord(Temp, UserInf, UserExtInf, false);

      if (RaCrc('', true) = UserInf.PasswordCRC) AND
           (UserInf.Password='') then
             begin
               Result := true;
               UserNr := Temp;
               LineCfg^.Exitinfo^.Userinfo := UserInf;
               EXIT;
             end; { if }

      Result := (RaCrc(Password, true) = UserInf.PasswordCRC);
      if Result then
        begin
          LineCfg^.Exitinfo^.Userinfo := UserInf;
          UserNr := Temp;
          EXIT;
        end; { if }
    end; { if }

end; { func. MatchUserName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SetSlaveMode(var NewsServer: TTcpClient);
begin
  SendCodeStr(NewsServer, 202);
end; { proc. SetSlavemode }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure PerformAuthentication(var NewsServer: TTcpClient;
                                var TempStr: String;
                                var AreaName: String;
                                var Authenticated: Boolean;
                                var UserName, Password: String;
                                var ServerSlot: Longint;
                                const IpStr: String);
var SecondWord: String;
begin
  if Authenticated then EXIT;
  SecondWord := SUpCase(ExtractWord(TempStr, 1, defExtractword, false, false));

  if SecondWord = 'USER' then
     begin
       FirstWord(TempStr, defExtractWord, true);
       SendCodeStr(NewsServer, 381);

       UserName := TempStr;
     end; { if }

  if SecondWord = 'PASS' then
     begin
       FirstWord(TempStr, defExtractword, true);

       Password := TempStr;

       With nwsSrvConnection(Connections[ServerSlot].SrvData^) do
        if MatchUserName(UserName, Password, UserRecNr, Userinf) then
         begin
           Authenticated := true;

           if NoDNSLookup then
             Connections[ServerSlot].Name := UserName;

           RaLog('!', Format('[NEWSSRV] [%s] User %s authenticated', [IpStr, UserName]));
           SendCodeStr(NewsServer, 281);
         end
          else begin
                 RaLog('!', Format('[NEWSSRV] [%s] User failed to logon (%s)', [IpStr, UserName]));
                 SendCodeStr(NewsServer, 482);
               end; { else }
     end; { if }
end; { proc. PerformAuthentication }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function news_ExecuteConnection(P: pointer): Longint;
var BufFlag      : Longint;
    ReturnCode   : Longint;
    CurSlot      : Longint;
    AbortSession : Boolean;
    NewsServer   : TTcpClient;

    ATempStr     : AnsiString;
    TempStr      : String;
    CommandStr   : String;

    CurrentGroup    : ^MessageRecord;
    CurrentEleGroup : ^EleMessageRecord;
    CurrArticle     : Longint;
    FirstArticle    : Longint;
    LastArticle     : Longint;
    Authenticated   : Boolean;
    NewsCommand     : NewsCommandSet;
    StartTime       : EventTimer;
begin
  {-- Update the screen ---------------------------------------------------}
  srvUpdateScreen := TRUE;

  {-- get current slot ----------------------------------------------------}
  CurSlot := Longint(p);

  {-- Log some info -------------------------------------------------------}
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMain, 'ExecuteConn() thread #'+IntToStr(CurSlot)+ ' - (conn01)');
  {$ENDIF}

  {-- Update the total number of connections weve gotten ------------------}
  Inc(TotalSessions);
  Inc(CurrentSessions);
  Inc(NewsSessions);

  {-- initialize some memory ----------------------------------------------}
  AbortSession := false;
  Authenticated := false;
  New(CurrentGroup);
  New(CurrentEleGroup);

  {-- Log some info -------------------------------------------------------}
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMain, 'ExecuteConn() thread #'+IntToStr(CurSlot)+ ' - (conn02)');
  {$ENDIF}

  {-- create server socket ------------------------------------------------}
  NewsServer := TTcpClient.Create;
  NewsServer.SockHandle := Connections[CurSlot].ClientRC;

  {-- start an idle timer -------------------------------------------------}
  NewTimer(StartTime, Secs2Tics(SecsTimeOut));

  {-- Log some info -------------------------------------------------------}
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMain, 'ExecuteConn() thread #'+IntToStr(CurSlot)+ ' - (conn03)');
  {$ENDIF}

  {-- and send a greeting depending upon wether posting is allowed or not -}
  if PostingAllowed then
   SendCodeStr(NewsServer, 200)
    else SendCodeStr(NewsServer, 201);


  {-- now start looping till a "QUIT" or idle timer expired ---------------}
  REPEAT
    {-- Log some info -----------------------------------------------------}
    {$IFDEF WITH_DEBUG}
       DebugObj.DebugLog(logMain, 'ExecuteConn() thread #'+IntToStr(CurSlot)+ ' - (conn04)');
    {$ENDIF}

    {-- Get a command from the user ---------------------------------------}
    if NewsServer.RecvStrLn(ATempStr, false) then
     begin
       GenericSleep(0);
       TempStr := ATempStr;

       if TempStr <> '' then
        begin
          NewTimer(StartTime, Secs2Tics(SecsTimeOut));

          {$IFDEF WITH_DEBUG}
            DebugObj.DebugLog(logMain, 'ExecuteConn() thread #'+IntToStr(CurSlot)+ ' - (conn04.5)');
            DebugObj.DebugLog(logString, 'Command (begin): '+TempStr);
          {$ENDIF}

          CommandStr := SUpCase(FirstWord(TempStr, defExtractword, false));
          NewsCommand := nwsError;

          {---------------- Translate the string to a set code ----------------}
          Case CommandStr[1] of
            'A' : if CommandStr = 'ARTICLE' then NewsCommand := nwsArticle
                    else if CommandStr = 'AUTHINFO' then NewsCommand := nwsAuth;
            'B' : if CommandStr = 'BODY' then NewsCommand := nwsBody;
            'G' : if CommandStr = 'GROUP' then NewsCommand := nwsGroup;
            'H' : if CommandStr = 'HELP' then NewsCommand := nwsHelp
                   else if CommandStr = 'HEAD' then NewsCommand := nwsHead;
            'I' : if CommandStr = 'IHAVE' then NewsCommand := nwsIHave;
            'L' : if CommandStr = 'LAST' then NewsCommand := nwsLast
                  else if CommandStr = 'LIST' then NewsCommand := nwsList;
            'N' : if CommandStr = 'NEWGROUPS' then NewsCommand := nwsNewGroups
                    else if CommandStr = 'NEWNEWS' then NewsCommand := nwsNewNews
                      else if CommandStr = 'NEXT' then NewsCommand := nwsNext;
            'M' : if CommandStr = 'MODE' then NewsCommand := nwsMode;
            'P' : if CommandStr = 'POST' then NewsCommand := nwsPost;
            'S' : if CommandStr = 'SLAVE' then NewsCommand := nwsSlave
                    else if CommandStr = 'STAT' then NewsCommand := nwsStat;
            'Q' : if CommandStr = 'QUIT' then NewsCommand := nwsQuit;
            'X' : if CommandStr = 'XOVER' then NewsCommand := nwsXover
                    else if CommandStr = 'XCRASH' then NewsCommand := nwsCrash;
          end; { case }

          {------------------- Execute the command ----------------------------}
          Case NewsCommand of
            nwsAuth     : PerformAuthentication(NewsServer,
                                                TempStr,
                                                CurrentGroup^.Name,
                                                Authenticated,
                                                nwsSrvConnection(Connections[CurSlot].SrvData^).Username,
                                                nwsSrvConnection(Connections[CurSlot].SrvData^).Password,
                                                CurSlot,
                                                Connections[CurSlot].IpAddr);
            nwsArticle  : SendArticle(NewsServer,
                                      CommandStr,
                                      TempStr,
                                      CurrentGroup^,
                                      CurrArticle,
                                      Authenticated,
                                      Connections[CurSlot].IpAddr,
                                      CurSlot);
            nwsBody     : SendArticle(NewsServer,
                                      CommandStr,
                                      TempStr,
                                      CurrentGroup^,
                                      CurrArticle,
                                      Authenticated,
                                      Connections[CurSlot].IpAddr,
                                      CurSlot);
           {$IFDEF WITH_DEBUG}
             nwsCrash    : RunError(4);
           {$ENDIF}
            nwsError    : begin
                            RaLog('>', Format('[NEWSSRV] [%s] Bad formed command (%s)', [Connections[CurSlot].IpAddr, CommandStr]));
                            SendCodeStr(NewsServer, 500);
                          end; { error }
            nwsGroup    : SelectGroup(CurrentGroup^,
                                      NewsServer,
                                      TempStr,
                                      AbortSession,
                                      CurrArticle,
                                      FirstArticle,
                                      LastArticle,
                                      Connections[CurSlot].IpAddr,
                                      CurSlot);
            nwsHelp     : SendHelpText(NewsServer);
            nwsHead     : SendArticle(NewsServer,
                                      CommandStr,
                                      TempStr,
                                      CurrentGroup^,
                                      CurrArticle,
                                      Authenticated,
                                      Connections[CurSlot].IpAddr,
                                      CurSlot);
            nwsIhave    : SendCodeStr(NewsServer, 500);
            nwsLast     : SelectPreviousArticle(NewsServer,
                                                CurrArticle,
                                                CurrentGroup^,
                                                CurrentEleGroup^,
                                                FirstArticle,
                                                CommandStr,
                                                CurSlot);
            nwsList     : SendGroupList(NewsServer, TempStr, false, Authenticated, CurSlot);
            nwsNewGroups: SendGroupList(NewsServer, TempStr, true, Authenticated, CurSlot);
            nwsNewNews  : SendCodeStr(NewsServer, 500);
            nwsNext     : SelectPreviousArticle(NewsServer,
                                                CurrArticle,
                                                CurrentGroup^,
                                                CurrentEleGroup^,
                                                FirstArticle,
                                                CommandStr,
                                                CurSlot);
            nwsPost     : PostArticle(NewsServer,
                                      CommandStr,
                                      nwsSrvConnection(Connections[CurSlot].SrvData^).Username,
                                      Authenticated,
                                      Connections[CurSlot].IpAddr,
                                      CurSlot);
            nwsSlave    : SetSlaveMode(NewsServer);
            nwsStat     : SendArticle(NewsServer,
                                      CommandStr,
                                      TempStr,
                                      CurrentGroup^,
                                      CurrArticle,
                                      Authenticated,
                                      Connections[CurSlot].IpAddr,
                                      CurSlot);
            nwsQuit     : begin
                            SendCodeStr(NewsServer, 205);
                            AbortSession := true;
                          end; { quit }
            nwsXover    : SendXOverView(NewsServer,
                                        TempStr,
                                        CurrentGroup^,
                                        CurrentEleGroup^,
                                        CurrArticle,
                                        FirstArticle,
                                        CurSlot);
            nwsMode     : SendCodeStr(NewsServer, 200);
          end; { case }

          {$IFDEF WITH_DEBUG}
            DebugObj.DebugLog(logString, 'Command ( end ): '+TempStr);
          {$ENDIF}
        end { if }
     end
      else GenericSleep(10);

    {-- Log some info -----------------------------------------------------}
    {$IFDEF WITH_DEBUG}
       DebugObj.DebugLog(logMain, 'ExecuteConn() thread #'+IntToStr(CurSlot)+ ' - (conn05)');
    {$ENDIF}
  UNTIL (AbortSession) OR (NOT NewsServer.ConnectionAlive)
         OR (TimerExpired(StartTime));

  {-- Log some info -------------------------------------------------------}
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMain, 'ExecuteConn() thread #'+IntToStr(CurSlot)+ ' - (conn06)');
  {$ENDIF}

  {-- and log a friendly message -----------------------------------------}
  if (TimerExpired(StartTime)) then
    begin
      RaLog('>', Format('[NEWSSRV] [%s] Connection timedout', [Connections[CurSlot].IpAddr]));
    end; { if }

  {-- Dispose of unused memory -------------------------------------------}
  if CurrentGroup <> nil then
    Dispose(CurrentGroup);
  if CurrentEleGroup <> nil then
    Dispose(CurrentEleGroup);

  {-- Log some info -------------------------------------------------------}
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMain, 'ExecuteConn() thread #'+IntToStr(CurSlot)+ ' - (conn07)');
  {$ENDIF}

  {-- disconnect the session (in case of idle timeout etc) ---------------}
  NewsServer.Disconnect;
  NewsServer.Destroy;

  {-- Log some info -------------------------------------------------------}
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMain, 'ExecuteConn() thread #'+IntToStr(CurSlot)+ ' - (conn08)');
  {$ENDIF}

  {-- close the area, if it was still open -------------------------------}
  with nwsSrvConnection(Connections[CurSlot].SrvData^) do
    CloseMsgArea(MsgPtr, AreaInf);

  {-- Log some info -------------------------------------------------------}
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMain, 'ExecuteConn() thread #'+IntToStr(CurSlot)+ ' - (conn09)');
  {$ENDIF}

  {-- log some info ------------------------------------------------------}
  RaLog('>', Format('[NEWSSRV] [%s] Connection closed', [Connections[CurSlot].IpAddr]));

  {-- and clear up the connection slow used by this session --------------}
  Connections[CurSlot].IpAddr := '';
  Connections[CurSlot].StartTimeStr := '';
  Connections[CurSlot].ClientRC := -1;
  FreeMem(Connections[CurSlot].SrvData);
  Connections[CurSlot].srvData := nil;
  Connections[CurSlot].Name := '';

  {-- Log some info -------------------------------------------------------}
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMain, 'ExecuteConn() thread #'+IntToStr(CurSlot)+ ' - (conn10)');
  {$ENDIF}

  {-- decrease the total number of sessions ------------------------------}
  Dec(CurrentSessions);
  Dec(NewsSessions);

  {-- and force a screen update ------------------------------------------}
  srvUpdateScreen := TRUE;

  {-- exit this thread seems to leak memory !! ---------------------------}
  EXIT;
end; { func. news_ExecuteConnection }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function news_ServerThread(P: pointer): Longint;
var ClientAddr  : PSockAddr;
    ClientRC    : Longint;
    BufFlag     : Longint;

    ReturnCode  : Longint;
    Counter     : Longint;
    SelectedSlot: Longint;

    Temp        : Longint;
    Client      : String;
    TempStr     : String;
begin
  {-- Initialize basic variables ------------------------------------------}
  New(ClientAddr);
  FillChar(ClientAddr^, SizeOf(ClientAddr^), #00);
  Temp := SockAddr_len;

  {-- we call that here already so we can more easily track memory leaks --}
  Client := SockGetHostNameByAddr(@ClientAddr^.Sin_Addr);

  {-- Now lets keep looping till we get terminated ------------------------}
  REPEAT
    NewsServerSocket.DoSleep(1);

    {-- Log some info -----------------------------------------------------}
    {$IFDEF WITH_DEBUG}
       DebugObj.DebugLog(logMain, 'AcceptConn() thread (begin)');
    {$ENDIF}

    {-- repeat until we get at least one connection -----------------------}
    REPEAT
      ClientRC := SockAccept(NewsServerSocket.ServerHandle,
                             ClientAddr,
                             Temp);
      NewsServerSocket.DoSleep(10);
    UNTIL (ClientRC <> -1) OR (TermProgram);

    {-- Log some info -----------------------------------------------------}
    {$IFDEF WITH_DEBUG}
       DebugObj.DebugLog(logMain, 'AcceptConn() thread (conn01)');
    {$ENDIF}

    {-- and make our program a bit less resource hungry -------------------}
    NewsServerSocket.DoSleep(1);

    {-- Make sure that the calls aren't blocking --------------------------}
    SocksetBlockingIO(ClientRC, false);

    {-- now get a slot for our program to work in -------------------------}
    SelectedSlot := FindEmptySlot;

    {-- Log some info -----------------------------------------------------}
    {$IFDEF WITH_DEBUG}
       DebugObj.DebugLog(logMain, 'AcceptConn() thread (conn02)');
    {$ENDIF}

    {-- make sure we dont exceed the max. number of sessions --------------}
    if NewsSessions >= LineCfg^.NewsServer^.MaxSessions then
      SelectedSlot := 0;

    {-- now act upon what we found ----------------------------------------}
    if NOT TermProgram then
      begin
        {-- Log some info -------------------------------------------------}
        {$IFDEF WITH_DEBUG}
          DebugObj.DebugLog(logMain, 'AcceptConn() thread (conn03)');
        {$ENDIF}

        {-- Now get the client name ---------------------------------------}
        if NOT NoDnsLookup then
          Client := SockGetHostNameByAddr(@ClientAddr^.Sin_Addr)
            else Client := NewsServerSocket.IpToStr(ClientAddr^.sIn_Addr);

        if ReturnCode = -1 then
          Client := 'unknown host';

        {-- Log some info -------------------------------------------------}
        {$IFDEF WITH_DEBUG}
          DebugObj.DebugLog(logMain, 'AcceptConn() thread (conn04)');
        {$ENDIF}

        {-- do we have room for this user anyway? -------------------------}
        if SelectedSlot > 00 then
          begin
            {-- Log some info ---------------------------------------------}
            {$IFDEF WITH_DEBUG}
              DebugObj.DebugLog(logMain, 'AcceptConn() thread (conn05)');
            {$ENDIF}

            {-- clear any pending thread ----------------------------------}
            if Connections[SelectedSlot].ServerThread <> nil then
              Dispose(Connections[SelectedSlot].ServerThread, Done);

            {-- Log some info ---------------------------------------------}
            {$IFDEF WITH_DEBUG}
              DebugObj.DebugLog(logMain, 'AcceptConn() thread (conn06)');
            {$ENDIF}

            {-- and initialize the new section as soon as possible --------}
            Connections[SelectedSlot].SrvType := srvNews;
            if NoDNSLookup then
              Connections[SelectedSlot].Name := ' '
                else Connections[SelectedSlot].Name := Client;
            Connections[SelectedSlot].IpAddr:= NewsServerSocket.IpToStr(ClientAddr^.sIn_Addr);
            Connections[SelectedSlot].StartTimeStr := TimeStr(true, false);
            Connections[SelectedSlot].ClientRC := ClientRC;

            {-- Log some info ---------------------------------------------}
            {$IFDEF WITH_DEBUG}
              DebugObj.DebugLog(logMain, 'AcceptConn() thread (conn07)');
            {$ENDIF}

            {-- now get this data for this specific session ---------------}
            GetMem(Connections[SelectedSlot].SrvData, SizeOf(nwsSrvConnection));
            FillChar(Connections[SelectedSlot].SrvData^, SizeOf(nwsSrvConnection), #0);
            nwsSrvConnection(Connections[SelectedSlot].SrvData^).UserRecNr := -1;

            {-- Log some info ---------------------------------------------}
            {$IFDEF WITH_DEBUG}
              DebugObj.DebugLog(logMain, 'AcceptConn() thread (conn07)');
            {$ENDIF}

            {-- now initialize the new server instance --------------------}
            New(Connections[SelectedSlot].ServerThread, Init);

            {-- Log some info ---------------------------------------------}
            {$IFDEF WITH_DEBUG}
              DebugObj.DebugLog(logMain, 'AcceptConn() thread (conn08)');
            {$ENDIF}

            {-- low some info ---------------------------------------------}
            RaLog('>', Format('[NEWSSRV] [%s] Connection opened', [Connections[SelectedSlot].IpAddr]));

            {-- and actually execute this thread --------------------------}
            Connections[SelectedSlot].ServerThread^.CreateThread(StackSize,
                                                                 @news_ExecuteConnection,
                                                                 Pointer(SelectedSlot),
                                                                 0);

            {-- Log some info ---------------------------------------------}
            {$IFDEF WITH_DEBUG}
              DebugObj.DebugLog(logMain, 'AcceptConn() thread (conn09)');
            {$ENDIF}
          end
            else begin
                   {-- no more sessions left, exit ------------------------}
                   Inc(TotalRefused);
                   SockClose(ClientRc);
                 end; { if busy }
      end; { if not program Terminated }

    {-- Log some info -----------------------------------------------------}
    {$IFDEF WITH_DEBUG}
       DebugObj.DebugLog(logMain, 'AcceptConn() thread (conn10)');
    {$ENDIF}
  until (ReturnCode = -1) OR (TermProgram);

  {-- Log some info -------------------------------------------------------}
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMain, 'AcceptConn() thread (conn11)');
  {$ENDIF}

  {-- and remove unused memory --------------------------------------------}
  Dispose(ClientAddr);
  news_ListenThreadEnded := true;

  ExitThisThread;
end; { func. NewsServer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ReadNewsServerEle;                     { Opens and reads NWSERV.ELE }
var NewsServer_F: pFileObj;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMain, 'ReadNewsServerEle (begin)');
     DebugObj.DebugLog(logMain, 'NWSSERV.ELE Filename = '+TelnetFileName);
  {$ENDIF}

  {-- Read the config file --------------------------------------------------}
  Config_OpenFile(NewsServer_F, NewsserverFileNAme, 01, ReadMode + DenyWrite, False, True);
  Config_ReadFile(NewsServer_F, LineCfg^.NewsServer^, SizeOf(NewsServerRecord));
  Config_DoneFile(NewsServer_F);

  {-- Convert the used variables to "global" ones if necessary --------------}
  GlobEmailHostName := LineCfg^.NewsServer^.DomainName;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMain, 'ReadNewsServer ( end )');
  {$ENDIF}
end; { proc. ReadNewsServerEle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure news_InitServer;
begin
  {-- and allocate some memory -------------------------------------------}
  if NOT AllocMem(LineCfg^.NewsServer, SizeOf(NewsServerRecord), 'ServerRecord', 'ShowHeader') then
    begin
      WriteLn;
      Writeln('Sorry you need at least ', SizeOf(NewsServerRecord) div 1024, 'k more memory to run');
      Halt(255);
    end; { Not enough memory }

  if NOT AllocMem(LineCfg^.Exitinfo, SizeOf(ExitinfoRecord), 'ExitinfoRecord', 'ShowHeader') then
    begin
      WriteLn;
      Writeln('Sorry you need at least ', SizeOf(ExitinfoRecord) div 1024, 'k more memory to run');
      Halt(255);
    end; { Not enough memory }

  {-- Read the newsserver file ---------------------------------------------}
  ReadNewsServerEle;

  {-- make sure our index file is always in the system directory -----------}
  NwsGroupIdxName := GlobalCfg^.RaConfig^.SysPath + NwsGroupIdxName;
  NwsGroupStatName := GlobalCfg^.RaConfig^.SysPath + NwsGroupStatName;

  {-- create an index file for the groups ----------------------------------}
  CreateGroupIndex;
end; { proc. news_InitServer }


end. { NEWSSRV }
