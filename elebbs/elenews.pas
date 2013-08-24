program EleNEWS;
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
{$M 205499, 955000}
{$IFDEF RELEASE}
  {$IFDEF VirtualPascal}
   {$IFDEF WIN32}
    {$ERROR Using both -C and -T will generate a runtime error!! }
   {$ENDIF}
  {$ENDIF}
{$ENDIF}
(*
**
** EleNEWS, Newsgroup (NNTP/RFC977) grabber into message base
**
** Copyright (c) 1996,97,98 by Maarten Bekers
**
** Created : 25-Dec-1998
** Last update : 25-Feb-1999
**
*)
uses {$IFDEF WIN32}
       Windows,
     {$ENDIF}

     {$IFDEF OS2}
       Os2Base,
     {$ENDIF}

     {$IFDEF ELEUNIX}
       {$IFDEF VER1_0}
         Linux,
       {$ELSE}
         Unix,
       {$ENDIF}
     {$ENDIF}
     Dos, Crt, LongStr, SockDef, SockFunc, NntpCli,
     Cases, Global, WordStr, BitWise, Crc_unit, StUtils,
     CentrStr, CfgRec, BbsKey, Ellog_U, StrUnit,
     ReadCfg, Sysutils, Mail, FileRout, MkMsgAbs, MkOpen,
     JDates, Debug_U, GenFile, WinTitle, MemMan,
     PktSup, StrPath, FileObj, MimeDec, MgrFdb,
     Articles, elx_bbs, SysVars;


Type  OptionsType    = (otGetarticles,  otTossArticles, otScanMsgbase,
                        otSendArticles, otListGroups);
Const DefaultOptions : Set of OptionsType = [];

var Options        : Set of OptionsType;
    NntpClient     : tNntpClient;
    Nwsgroup_F     : Text;
    CurrentGroupNr : Longint;
    SaveExitProc   : Pointer;

    AttachInf      : FilesRecord;
    Attach_F       : pFileObj;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Const NntpPort     : Longint = 119;
      MaxMessages  : Longint = -1;
      MaxSent      : Longint = 20;
      Hostname     : String = '';
      UseUsername  : String = '';
      UsePassword  : String = '';

      DeleteSema   : Boolean = false;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CloseConnection;
begin

  if NntpClient <> nil then
   try
     NntpClient.Quit;
     NntpClient.Free;
   except
   end;

  NntpClient := nil;
end; { proc. CloseConnection }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CloseDownExitProc;
begin
  ExitProc := SaveExitProc;

  CloseConnection;
  if DeleteSema then RemoveSema(ElenewsSemaphore);
end; { proc. CloseDownExitProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FatalError(S: String; ErrorCode: Longint);
begin
  TextAttr := Lightgray;
  WriteLn;
  WriteLn(#32, SystemMsgPrefix, 'Fatal error occured!');
  WriteLn(#32, SystemMsgPrefix, S);
  if ErrorCode <> -1 then
    WriteLn(#32, SystemMsgPrefix, '#', ErrorCode, ', ', SockGetErrStr(ErrorCode));
  Halt(255);
end; { proc. FatalError }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowText(const S: ShortString);
begin
 {  WriteLn(s); }
end; { proc. ShowText }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function AbortSession: Boolean;
begin
  Result := false;

  if Keypressed then
   if ReadKey = #27 then
     Result := true;
end; { func. AbortSession }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function SearchArea(const AreaName: String): Longint;
var AreaFound: Longint;
    Area_F   : pFileObj;
    AreaInf  : EleMessageRecord;
begin
  AreaFound := -1;
  SearchArea := AreaFound;

  New(Area_F, Init);
  Area_F^.Assign(MessageEleFileName);
  Area_F^.FileMode := ReadMode + DenyNone;
  if NOT Area_F^.Open(1) then EXIT;

  While NOT Area_F^.EOF do
    begin
      Area_F^.BlkRead(AreaInf, SizeOf(EleMessageRecord));

      if AreaName = SUpCase(Trim(AreaInf.GroupName)) then
        begin
          AreaFound := Area_F^.FilePos dIV SizeOf(EleMessageRecord);
          BREAK;
        end; { if }
    end; { while }

  Dispose(Area_F, Done);

  SearchArea := AreaFound;
end; { func. SearchArea }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure NewsgroupListProc(Const S: Shortstring);
var GroupName: String;
    Posting  : String;
begin
  if s = '' then EXIT;

  GroupName := '';
  Posting := '';

  GroupName := ExtractWord(S, 1, defExtractWord, false, false);
  Posting := ExtractWord(S, 4, defExtractWord, false, false);
  if UpCase(Posting[1]) = 'Y' then Posting := 'Yes'
       else Posting := 'No';

  {$i-}
    WriteLn(NwsGroup_F, MakeLen(GroupName, 50, Space, false, false), '  ', Posting);
  {$i+}
  if IOResult > 00 then ;

  Inc(CurrentGroupNr);

  if (CurrentGroupNr MOD 50) = 0 then
    Write('(', FStr(CurrentGroupNr), ')', Dup(#08, Length(FStr(CurrentGroupNr)) + 2));
end; { proc. NewsGroupList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SetNewsGroupStat(Name: String; DiskLoMsg: Longint);
var Stat_F   : pFileObj;
    StatInf  : NewsGroupStatRecord;
    DidWrite : Boolean;
begin
  DidWrite := false;

  New(Stat_F, Init);
  Stat_F^.FileMode := ReadWriteMode + DenyNone;
  Stat_F^.Assign('nwsgroup.ele');
  if NOT Stat_F^.OpenOrCreate(1) then
    FatalError('Unable to create newsgroup statistics file', -1);

  While NOT Stat_F^.EOF Do
    begin
      Stat_F^.BlkRead(StatInf, SizeOf(NewsGroupStatRecord));
      if Stat_F^.IoResult > 00 then
        FatalError('Unable to read newsgroup statistics file', -1);

      if SUpCase((StatInf.GroupName)) = SUpCase(Trim(Name)) then
        begin
          StatInf.LoMsgCount := DiskLoMsg;

          {$i-}
            Stat_F^.Seek(Stat_F^.FilePos - SizeOf(NewsGroupStatRecord));
            Stat_F^.BlkWrite(StatInf, SizeOf(NewsGroupStatRecord));
          {$i+}
          if Stat_F^.IOResult > 00 then FatalError('Unable to update newsgroup statistics file!', -1);

          DidWrite := true;
        end; { if }

    end; { while }

  if NOT DidWrite then
   begin
     StatInf.LoMsgCount := DiskLoMsg;
     StatInf.Groupname := SUpCase(Trim(Name));

     Stat_F^.BlkWrite(StatInf, SizeOf(NewsGroupStatRecord));
     if Stat_F^.IoResult > 00 then FatalError('Unable to update newsgroup statistics file!', -1);
   end; { if }

  Dispose(Stat_F, Done);
end; { proc. SetNewsGroupStat }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GetNewsGroupStat(Name: String; var DiskLoMsg: Longint; ServLoMsg: Longint);
var Stat_F   : pFileObj;
    StatInf  : NewsGroupStatRecord;
begin
  DiskLoMsg := ServLoMsg - 01;
  if DiskLoMsg < 0 then DiskLoMsg := 0;

  New(Stat_F, Init);
  Stat_F^.FileMode := ReadWriteMode + DenyNone;
  Stat_F^.Assign('nwsgroup.ele');
  if NOT Stat_F^.Open(1) then EXIT;

  While NOT Stat_F^.EOF Do
    begin
      Stat_F^.BlkRead(StatInf, SizeOf(NewsGroupStatRecord));
      if Stat_F^.IoResult > 00 then
        FatalError('Unable to read newsgroup statistics file', -1);

      if SUpCase((StatInf.GroupName)) = SUpCase(Trim(Name)) then
        begin
          DiskLoMsg := StatInf.LoMsgCount;
          BREAK;
        end; { if }

    end; { while }

  Dispose(Stat_F, Done);
end; { proc. GetNewsGroupStat }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure filter_XoverHandler(const S: ShortString);
begin
  if S <> '.' then
    begin
      { subject <tab> author <tab> date <tab> msg-id <tab> references }
      { <tab> byte count }
      FilterArticle.FromWho := ExtractWord(S, 1, [#9], false, false);
      FilterArticle.Subject := ExtractWord(S, 1, [#9], false, false);
      FilterArticle.MsgSize := FVal(ExtractWord(S,  1, [#9], false, false));
      FilterArticle.MsgLines := FVal(ExtractWord(S,  1, [#9], false, false));
    end; { if }
end; { func. filter_xOverHandler }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure filter_HeaderHandler(const S: ShortString);
begin
  if FilterArticle.HdrLines < High(FilterArticle.Headers) then
    begin
      Inc(FilterArticle.HdrLines);
      FilterArticle.Headers[FilterArticle.HdrLines] := S;
    end; { if }
end; { proc. filter_HeaderHandler }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ShouldDownloadArticle(MsgCounter: Longint): Boolean;
var elxObj: pElxBbsObj;
begin
  {-- default to something -------------------------------------------------}
  ShouldDownloadArticle := TRUE;

  {-- add a path if necessary ----------------------------------------------}
  if NOT (FileExist(lineCfg^.Language^.QuesPath + 'nwsfilt.elm')) then
    EXIT;

  {-- default to non-download ----------------------------------------------}
  ShouldDownloadArticle := FALSE;

  {-- and clear info -------------------------------------------------------}
  FillChar(FilterArticle, SizeOf(FilterArticle), #0);

  {-- and get Xover --------------------------------------------------------}
  NntpClient.Xover(FStr(MsgCounter), filter_XOverHandler);

  {-- and get the first 50 header lines ------------------------------------}
  NntpClient.GetHeader(MsgCounter, filter_HeaderHandler);

  {-- now lets run the EleXer script ---------------------------------------}
  New(elxObj, Init);
  if NOT elxObj.RunElexerScript('nwsfilt', '', false) then
    begin
      ShouldDownloadArticle := TRUE;
    end
      else begin
             ShouldDownloadArticle := (SUpCase(elxObj.GetElxReturnString) = 'YES');
           end; { if }

  Dispose(elxObj, Done);
end; { func. ShouldDownloadArticle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CollectNewArticles;
var AreaInf   : MessageRecord;
    EleMsgInf : EleMessageRecord;
    Counter   : Longint;
    FirstMsg  : Longint;
    HighMsg   : Longint;
    DiskLoMsg : Longint;
    MsgText   : ^MessageTextType;
    ErrorStr  : String;
    Area_F    : pFileObj;
begin
  if NOT AllocMem(MsgText, SizeOf(MessageTextType), 'MessageTextType', 'CollectNewArticles') then
    FatalError('Not enough memory to setup buffer', -1);

  New(Area_F, Init);
  Area_F^.Assign(MessagesFileName);
  Area_F^.FileMode := ReadMode + DenyNone;
  if NOT Area_F^.Open(1) then
    FatalError('Error while opening '+MessagesFileName, -1);

  While NOT Area_F^.EOF do
    begin
      Area_F^.BlkRead(AreaInf, SizeOf(MessageRecord));
      if Area_F^.IoResult > 0 then BREAK;

      if AreaInf.Typ = Newsgroup then
        begin
          GetEleMessageRecord(EleMsgInf, AreaInf.AreaNum, true);

          if EleMsgInf.GroupName <> '' then
            begin
              Write(#32, SystemMsgPrefix);

              Write('Getting newsgroup overview (', EleMsgInf.GroupName,')');


              if NOT NntpClient.GetNewsGroup(EleMsgInf.GroupName, FirstMsg, HighMsg, ErrorStr) then
                begin
                  RaLog('!', 'Unable to get information for '+EleMsgInf.GroupName);
                  RaLog('!', 'Cause: '+ErrorStr);
                  Write(', failed! (', ErrorStr, ')');
                  NntpClient.FlushData;
                end { if }
                  else begin
                         GetNewsgroupStat(EleMsgInf.GroupName, DiskLoMsg, FirstMsg);

                         if (HighMsg < DiskLoMsg) then
                           begin
                             Write(' (newsserver has been reset)');
                             WriteLn;
                             DiskLoMsg := HighMsg;
                           end; { if }

                         Write(' (', HighMsg - DiskLoMsg, ' new msgs)');

                         if MaxMessages <> -1 then
                          if (HighMsg - (DiskLoMsg + 01)) > MaxMessages then
                           begin
                             DiskLoMsg := HighMsg - MaxMessages;
                             WriteLn;
                             Write('    start retrieving from ', DiskLoMsg + 01);
                           end; { if }

                         if (HighMsg - DiskLoMsg) > 0 then
                           begin
                             WriteLn;
                             Write(#32, SystemMsgPrefix, 'Retrieving message #');

                             for Counter := (DiskLoMsg+1) to HighMsg do
                               begin
                                 if ShouldDownloadArticle(Counter) then
                                   if NntpClient.GetArticle(Counter, MsgText^) then
                                     begin
                                       Write(Counter, Dup(#08, Length(FStr(Counter))));

                                       AddMsgToBase(ArticlesFileName, EleMsgInf.GroupName, AreaInf.AreaNum, Counter, MsgText^, false);
                                       DiskLoMsg := Counter;
                                     end; { if }
                               end; { for }

                             SetNewsgroupStat(EleMsgInf.GroupName, DiskLoMsg);
                           end; { if }
                       end; { if }

              WriteLn;
            end
              else RaLog('!', 'Groupname defined for "'+AreaInf.Name+'" is empty, skipping area.');

        end; { if }
    end; { while }

  Dispose(Area_F, Done);

  ReleaseMem(MsgText, SizeOf(MessageTextType));
end; { proc. CollectNewArticles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowHeader;
begin
  TextAttr := LightGray;

  WriteLn;
  WriteLn('ELENEWS; EleBBS Usenet retrieval utility, Version '+VersionID);
  WriteLn('         Copyright 1997-2003 Maarten Bekers, All rights reserved.');
  WriteLn;

  New(LineCfg);
  New(GlobalCfg);
  InitBasicGlobalVars;
  InitBasicLineVars(LineCfg^);

  if NOT MemMan.AllocMem(GlobalCfg^.RaConfig, SizeOf(ConfigRecord), 'ConfigRecord', 'ShowHeader') then
     FatalError('Could not allocate enough memory!', -1);

  if NOT MemMan.AllocMem(GlobalCfg^.ElConfig, SizeOf(EleConfigRecord), 'EleConfigRecord', 'ShowHeader') then
     FatalError('Could not allocate enough memory!', -1);

  if NOT MemMan.AllocMem(LineCfg^.Exitinfo, SizeOf(ExitinfoRecord), 'ExitinfoRecord', 'ShowHeader') then
     FatalError('Could not allocate enough memory!', -1);

  if NOT MemMan.AllocMem(LineCfg^.Language, SizeOf(LanguageRecord), 'LanguageRecord', 'ShowHeader') then
     FatalError('Could not allocate enough memory!', -1);
end; { proc. ShowHeader }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ParseCommandLine;
var Invalid : Boolean;
    InvOpt  : String;
    Counter : Byte;
    Param   : String;
    TempCH  : Char;
begin
  Invalid := False;
  InvOpt := '';

  If ParamCount > 00 then
   For Counter := 01 to ParamCount do
    begin
       Param := ParamStr(Counter);
       TempCH := UpCase(Param[2]);

       If (NOT (Param[1] in ['/', '-'])) OR
           ((Length(ParamStr(Counter)) > 2) AND NOT (TempCH in ['H', 'X', 'M', 'D', 'Z', 'I', 'O', 'U', 'E'])) then
            begin
              InvOpt := ParamStr(Counter);
              Invalid := True;
              Break;
            end; { Invalid }

       Case UpCase(TempCH) of
        {$IFDEF WITH_DEBUG}
          'X' : if (SUpCase(Copy(ParamStr(Counter), 2, 255)) = 'XDEBUGLOG') then
                  DebugObj.DebugLogging := TRUE;
        {$ENDIF}
          'C' : Options := Options + [otGetArticles];
          'T' : Options := Options + [otTossArticles];
          'S' : Options := Options + [otScanMsgBase];
          'P' : Options := Options + [otSendArticles];
          'L' : Options := Options + [otListGroups];
          'D' : begin
                  DisclaimName := Copy(ParamStr(Counter), 3, 255);
                end; { if }
          'M' : begin
                  MaxMessages := FVal(Copy(ParamStr(Counter), 3, 255));
                  if MaxMessages = 0 then MaxMessages := -1;
                end; { if }
          'E' : begin
                  GlobEmailHostname := Copy(ParamStr(Counter), 3, 255);
                end; { Email hostname }
          'H' : begin
                  Hostname := Copy(ParamStr(Counter), 3, 255);

                  if Pos(':', HostName)>0 then
                    begin
                      Nntpport := FVal(Copy(HostName, Pos(':', HostName) + 1, 255));
                      HostName := Copy(HostName, 1, Pos(':', HostName) - 1);
                    end; { if }
                end; { hostname }
          'Z' : begin
                  DestFido := Copy(ParamStr(Counter), 3, 255);
                end; { if }
          'U' : begin
                  UseUsername := Copy(ParamStr(Counter), 3, 255);

                  UsePassword := Copy(UseUsername, Pos('@', UseUsername) + 1, 255);
                  UseUsername := Copy(UseUsername, 1, Pos('@', UseUsername) - 1);
                end; { if }
          'I' : begin
                  PktPath := ForceBack(Copy(ParamStr(Counter), 3, 255));
                end; { if }
          'O' : begin
                  OutboundPath := ForceBack(Copy(ParamStr(Counter), 3, 255));
                end; { if }
            else begin
                   Invalid := True;
                   InvOpt := ParamStr(Counter);
                 end; { Invalid option }
       end; { case }

   end; { for Counter }

  if ((DestFido <> '') AND (PktPath = '')) OR
      ((DestFido = '') AND (PktPath <> '')) then Invalid := true;

  if ((DestFido = '') AND (OutboundPath <> '')) then Invalid := true;

  if (otGetArticles in Options) OR (otSendArticles in Options) OR
      (otListGroups in Options) then
       if Hostname = '' then Invalid := true;

  if Invalid then
    begin
      if InvOpt <> '?' then
        begin
          Writeln(#32, SystemMsgPrefix, 'Invalid option : ', InvOpt);
        end; { if }

      WriteLn(#32, SystemMsgPrefix, 'Command-line parameters:');
      WriteLn;
      WriteLn(Dup(#32, 09), '-C               Collect articles from server');
      WriteLn(Dup(#32, 09), '-T               Toss articles into message base');
      WriteLn(Dup(#32, 09), '-S               Scan message base for new articles');
      WriteLn(Dup(#32, 09), '-P               Post articles to newsgroups');
      WriteLn(Dup(#32, 09), '-L               List all newsgroups on server to NWSGROUP.TXT');
      WriteLn(Dup(#32, 09), '-M[max msgs]     Maximum number of messages to retrieve');
      WriteLn(Dup(#32, 09), '-H<name>[:port]  Specify hostname of news server');
      WriteLn(Dup(#32, 09), '-D[filename]     Filename specifies a textfile that will be added to');
      WriteLn(Dup(#32, 09), '                 any posted message');
      WriteLn(Dup(#32, 09), '-Z[fido-address] Specify a Fido-style address');
      WriteLn(Dup(#32, 09), '-I[path]         Path where to create the *.PKT files');
      WriteLn(Dup(#32, 09), '-E<domainname>   Domainname for email address, FTN is used when empty');
      WriteLn(Dup(#32, 09), '-U<name>@<pword> Username and password to use for this connection');
      WriteLn;

      if Hostname = '' then
       if (otGetArticles in Options) OR (otSendArticles in Options) OR
           (otlistGroups in Options) then WriteLn(#32, SystemMsgPrefix, 'You need to specify a news server for this action!');

      WriteLn(#32, SystemMsgPrefix, 'Please refer to the documentation for a more complete command summary');
      WriteLn;

      MemMan.ReleaseMem(GlobalCfg^.RaConfig, SizeOf(ConfigRecord));
      MemMan.ReleaseMem(LineCfg^.Exitinfo, SizeOf(ExitinfoRecord));
      MemMan.ReleaseMem(LineCfg^.Language, SizeOf(LanguageRecord));
      Halt(255);
    end; { Invalid }

  Writeln(#32, SystemMsgPrefix, 'Active options:');

  If otScanMsgBase in Options then WriteLn('   - Scanning message base for new articles')
      else WriteLn('   - Not scanning message base');

  if otSendArticles in Options then WriteLn('   - Posting articles to newsgroups')
      else WriteLn('   - Not posting articles to newsgroups');

  If otGetArticles in Options then WriteLn('   - Collecting articles from ', Hostname)
      else WriteLn('   - Not collecting articles');

  If otTossArticles in Options then WriteLn('   - Tossing articles into message base')
      else WriteLn('   - Not tossing articles');

   if otListGroups in Options then WriteLn('   - Retrieving list of newsgroups from server')
      else WriteLn('   - Not retrieving list of newsgroups');

  Writeln;
end; { proc. ParseCommandLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ScanPacketArea(PacketName: String);
var TempStr    : String;
    DoPost     : Boolean;
    MessageInf : MessageRecord;

    SavePtr    : Longint;
    MsgText    : ^MessageTextType;
    NewText    : ^MessageTextType;

    Pkt_F      : pFileObj;
    PktHdr     : FTS1PktHdr;
    PktMsg     : FTS1PktMsg;

    AreaName,
    AsciiDate,
    ToUser,
    FromUser,
    Subject    : String;

    OrigZone,
    OrigNode,
    OrigNet,
    OrigPoint  : Word;
    OrigAddr   : AddrType;

    AreaFound  : Longint;
    MsgTime,
    MsgDate    : String;
    HostAddress: String;
begin
  if NOT AllocMem(MsgText, SizeOf(MessageTextType), 'MessageTextType', 'CollectNewArticles') then
    FatalError('Not enough memory to setup buffer', -1);
  if NOT AllocMem(NewText, SizeOf(MessageTextType), 'MessageTextType', 'CollectNewArticles-2') then
    FatalError('Not enough memory to setup buffer', -1);

  {----------------------------- Open the packet file -------------------------}
  New(Pkt_F, Init);
  Pkt_F^.Assign(PacketName);
  Pkt_F^.FileMode := ReadWriteMode + DenyNone;

  if NOT Pkt_F^.Open(1) then
    begin
      Dispose(Pkt_F, Done);
      EXIT;
    end; { if }


  {---------------------------- Read the packet header ------------------------}
  Pkt_F^.BlkRead(PktHdr, SizeOf(PktHdr));
  if (PktHdr.DestZone <> OrigZone) OR (PktHdr.DestNet = OrigNet) OR
      (PktHdr.DestNode <> OrigNode) OR (PktHdr.DestPoint = OrigPoint) then
        begin
          {$IFDEF WITH_DEBUG}
            DebugObj.DebugLog(logString,
                     Format('Packet (%s) for %d:%d/%d.%d not for %d:%d/%d.%d',
                     [Packetname, PktHdr.DestZone, PktHdr.DestNet,
                      PktHdr.DestNode, PktHdr.DestPoint, OrigZone,
                      OrigNet, OrigNode, Origpoint]));
          {$ENDIF}

          Dispose(Pkt_F, Done);
          EXIT;
        end; { if }


  {----------------------------- Read in all the messages ---------------------}
  While NOT Pkt_F^.EOF do
    begin
      OrigAddr.Zone := OrigZone;
      OrigAddr.Net  := OrigNet;
      OrigAddr.Node := OrigNode;
      OrigAddr.Point:= OrigPoint;

      Pkt_F^.BlkRead(PktMsg, SizeOf(PktMsg));            { Get message header }
      { AsciiDate, ToUser, FromUser, Subject, MsgText }

      Pkt_F^.BlkRead(MsgText^, SizeOf(MsgText^));    { Get the article header }
      SavePtr := Longint(MsgText);

      AreaName := StrPas(MsgText^);                                { Get text }
      Inc(PChar(MsgText), Length(AreaName) + 1);            { Increase buffer }

      AsciiDate := StrPas(MsgText^);                               { Get text }
      Inc(PChar(MsgText), Length(AsciiDate) + 1);           { Increase buffer }

      ToUser := StrPas(MsgText^);                                  { Get text }
      Inc(PChar(MsgText), Length(ToUser) + 1);              { Increase buffer }

      FromUser := StrPas(MsgText^);                                { Get text }
      Inc(PChar(MsgText), Length(FromUser) + 1);            { Increase buffer }

      Subject := StrPas(MsgText^);                                 { Get text }
      Inc(PChar(MsgText), Length(Subject) + 1);             { Increase buffer }

      FillChar(NewText^, SizeOf(MessageTextType), #00);
      MsgTime := Copy(AsciiDate, 12, Length(AsciiDate) - 12);
      MsgDate := Copy(AsciiDate, 1, 9);

      HostAddress := MakeHostEmail(GlobEmailHostName, FromUser, OrigAddr);
      AddStringLn(NewText^, 'Date: ' + MakeNNTPdate(MsgTime, MsgDate));
      AddStringLn(NewText^, 'From: "' + FromUser+ '" <'+HostAddress+'>');
      AddStringLn(NewText^, 'Subject: ' + Subject);
      AddStringLn(NewText^, 'Newsgroups: ' + AreaName);
      AddStringLn(NewText^, 'X-Newsreader: ' + PidName);
      AddStringLn(NewText^, '');

      Move(MsgText^, NewText^[StrLen(NewText^)], (SizeOf(NewText^) - StrLen(NewText^)) - 25);

      AreaFound := SearchArea(AreaName);
      if AreaFound > 0 then
        begin
          GetMessageRecord(MessageInf, AreaFound, true);

          AddMsgToBase(ArticlesOutFile, AreaName,
                       MessageInf.AreaNum, -1, NewText^, false);
        end; { if }

      MsgText := Pointer(SavePtr);
    end; { while }

  Dispose(Pkt_F, Done);

  ReleaseMem(MsgText, SizeOf(MessageTextType));
  ReleaseMem(NewText, SizeOf(MessageTextType));
end; { proc. ScanPacketArea }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ScanPktAreas;
var Dirinfo: Dos.SearchRec;
begin
  Dos.FindFirst(Outboundpath + '*.pkt', AnyFile, Dirinfo);

  While DosError = 0 do
    begin
      ScanPacketArea(OutboundPath + Dirinfo.Name);

      Dos.FindNext(Dirinfo);
    end; { while }

  Dos.FindClose(Dirinfo);
end; { proc. ScanPktAreas }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ScanMsgBase;
begin
  if OutboundPath = '' then
    ScanMsgAreas(Newsgroup, ArticlesOutFile);

  if OutboundPath <> '' then
    ScanPktAreas;
end; { proc. ScanMsgBase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function PostNewsArticle(var Article: NewsArticleRecord; var MsgText: MessageTextType): Boolean;
var ErrorStr: String;
begin
  Result := NntpClient.PostArticle(Article.GroupName, MsgText, ErrorStr);

  if ErrorStr <> '' then
    begin
      RaLog('!', 'Error occured while posting article');
      RaLog('!', 'Server returned: '+ErrorStr);
    end; { if }
end; { func. PostNewsArticle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SendArticles;
var Article_F    : pFileObj;
    Article      : NewsArticleRecord;
    MsgText      : ^MessageTextType;
    Posted       : Longint;

    ArticleHdrPos: Longint;
    SavePos      : Longint;
    CurArticle   : Longint;
    TmpAttr      : Byte;

    LastName : String;
begin
  if NOT AllocMem(MsgText, SizeOf(MessageTextType), 'MessageTextType', 'CollectNewArticles') then
    FatalError('Not enough memory to setup buffer', -1);

  Posted := 00;
  CurArticle := 0;
  Lastname := '';

  RaLog('>', 'Posting articles to newsgroups');
  WriteLn(#32, SystemMsgPrefix, 'Posting articles to newsgroups');

  New(Article_F, Init);
  Article_F^.Assign(ArticlesOutFile);
  Article_F^.FileMode := ReadWriteMode + DenyAll;
  if NOT Article_F^.Open(1) then
    begin
      Dispose(Article_F, Done);

      WriteLn('   (no articles to process)');
      RaLog('>', 'Posting completed, no articles processed');
      EXIT;
    end; { if }

  While NOT Article_F^.EOF do
     begin
       Inc(CurArticle);
       FillChar(MsgText^, SizeOf(MessageTextType), #00);

       ArticleHdrPos := Article_F^.FilePos;
       Article_F^.BlkRead(Article, SizeOF(NewsArticleRecord));
       if Article_F^.IOResult > 00 then FatalError('Unable to read article database', -1);

       Article_F^.BlkRead(MsgText^, Article.BodyLen);
       if Article_F^.IoResult > 00 then
         FatalError('Error occured while trying to read article text', -1);

       AddDisclaimer(MsgText^);

       if PostNewsArticle(Article, MsgText^) then
         begin
           TmpAttr := Article.Attribute;
           SetBit(TmpAttr, 0);
           Article.Attribute := TmpAttr;
         end { if }
           else begin
                  WriteLn('   Failed to send article #', CurArticle);

                  Inc(Article.TimesSent);
                  Dec(Posted);
                end; { if }

       {---------------------- Update the article header --------------------}
       SavePos := Article_F^.FilePos;
       Article_F^.Seek(ArticleHdrPos);

       Article_F^.BlkWrite(Article, SizeOf(NewsArticleRecord));
       if Article_F^.IoResult > 00 then
           FatalError('Error occured while trying to update article base', -1);
       Article_F^.Seek(SavePos);


       if LastName <> Article.GroupName then
         WriteLn(#32, SystemMsgPrefix, 'Posting in newsgroup ', Article.Groupname);
       Lastname := Article.GroupName;

       Inc(Posted);
     end; { while }

  RaLog('>', 'Posting completed ('+FStr(Posted)+' articles processed)');
  WriteLn('   (', Posted,' messages processed)');

  Dispose(Article_F, Done);
  PurgeSentBase(ArticlesOutFile, MaxSent);

  ReleaseMem(MsgText, SizeOf(MessageTextType));
end; { proc. SendArticles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ListNewsgroups;
begin
  RaLog('>', 'Retrieving newsgroup list from server');
  WriteLn(#32, SystemMsgPrefix, 'Retrieving newsgroup list from server');

  Assign(NwsGroup_F, 'nwsgroup.txt');
  {$i-} System.ReWrite(NwsGroup_F); {$i+}
  if IOResult > 00 then
    FatalError('Unable to create NWSGROUP.TXT', -1);

  {$i-}
    Writeln(NwsGroup_F, MakeLen('Newsgroup name', 50, Space, false, false), '  Posting');
    WriteLn(NwsGroup_F, Dup('-', 50), '  ', '----------');
  {$i+}
  if IoResult > 00 then ;

  CurrentGroupNr := 00;
  Write(#32, #32, #32, 'Retrieving newsgroup #');

  NntpClient.NewsGroupList({$IFDEF FPC}@{$ENDIF}NewsGroupListProc);

  WriteLn;
  WriteLn;

  {$i-}
    Close(NwsGroup_F);
  {$i+}
  if IOResult > 00 then ;
end; { proc. Listnewsgroups }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure StartConnection;
var Temp     : Boolean;
    ErrorStr : String;
begin
  NntpClient := TNntpClient.Create;

  Write(#32, SystemMsgPrefix, 'Connecting to ', Hostname, ',');

  Temp := NntpClient.ConnectToServer(HostName, NntpPort, ErrorStr);
  if NOT Temp then
    FatalError(ErrorStr, -1);

  NntpClient.UserName  := UseUsername;
  NntpClient.Password  := UsePassword;
  NntpClient.ShowLine  := {$IFDEF FPC}@{$ENDIF}ShowText;
  NntpClient.AbortFunc := {$IFDEF FPC}@{$ENDIF}AbortSession;

  NntpClient.WaitForData;                               { Wait for the header }
  NntpClient.FlushData;

  if NOT NNTPClient.Logon then
    begin
      RaLog('!', 'News server ('+HostName+'), refused authentication, aborting.');
      Writeln(' failed.');
      WriteLn;
      Halt(250);
    end { if }
      else Writeln(' success!');

  NntpClient.FlushData;

  if otListGroups in options then ListNewsgroups;
  if otGetArticles in Options then CollectNewArticles;
  if otSendArticles in Options then SendArticles;

  Writeln(#32, SystemMsgPrefix, 'Closing connection to ', Hostname);

  CloseConnection;
end; { proc. StartConnection }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

begin
  SetWindowTitle('EleNEWS');

  NntpClient := nil;
  SaveExitProc := ExitProc;
  ExitProc := @CloseDownExitproc;

  RunningBBS := False;
  Options := DefaultOptions;
  DoAttach := FALSE;


  ShowHeader;
  ReadConfigRa;
  InitSystemNames;
  CheckRegistered;

  ParseCommandLine;

  RaLog(' ', '');
  RaLog('>', 'EleNEWS; Usenet retrieval utility fired up');

  if SemaExist(EleNewsSemaphore) then
    begin
      RaLog('!', 'EleNEWS is already running ('+EleNewsSemaPhore + ' exists), aborting.');
      WriteLn(#32, SystemMsgPrefix, 'EleNEWS is already running ('+EleNewsSemaPhore + ' exists), aborting');
    end { if }
     else begin
            CreateSema(EleNewsSemaphore);
            DeleteSema := true;

            if otScanMsgBase in Options then ScanMsgBase;

            if NOT FileExist(ArticlesOutFile) then
              Options := Options - [otSendArticles];

            if (otGetArticles in Options) OR (otSendArticles in Options) OR
                (otListGroups in Options) then StartConnection;
            if otTossArticles in Options then ProcessNewArticles(ArticlesFileName);
          end; { if }

  Writeln(#32, SystemMsgPrefix, 'Done');

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'EleNEWS is done!');
  {$ENDIF}

end. { EleNEWS }
