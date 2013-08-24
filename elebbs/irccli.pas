unit IRCCLI;
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
{&Delphi+}
(*
**
** IRC chatting component
**
** Copyright (c) 1998, 99 by Maarten Bekers
**
** Created : 13-Nov-1998
** Last update : 13-Nov-1998
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses TcpCli, StrUnit;

const MaxNicks  = 400;
      MaxChans  = 100;
      MaxDCCs   = 1;
      MaxDCCBuf = 1024 * 32;

      dcc_NotActive  = 0;                       { DCC session is not active }
      dcc_Active     = 1;                           { DCC session is active }
      dcc_Finished   = 2;        { DCC session is finished, not handled yet }
      dcc_WaitPerm   = 3;           { Wait for users permission to get file }
      dcc_Pending    = 4;       { User is notified, wait for user to get it }

type tIRCclient = class(tTcpClient)
       protected
        fMotdEnd    : Boolean;
        fNickName   : String;
        fRealName   : String;
        fPassword   : String;
        fUsername   : String;
        fLastError  : Longint;
        fChanPrefix : String;
        fUserPrefix : String;

        function fGetErrStr: String;
       public
        fChannels  : Array[1..MaxChans] of record
                                            fNickList : pStringArrayObj;
                                            fChanName : String;
                                           end; { record }
        fDCCs      : Array[1..MaxDCCs] of record
                            { 0 = not active, 1 = active, 2 = just finished }
                                            Originate    : String;
                                            Active       : Longint;
                                            DCCtype      : String;
                                            FileName     : String;
                                            IpAddress    : String;
                                            PortNr       : Longint;
                                            FileSize     : Longint;
                                            StartTime    : Longint;
                                            DccClient    : tTcpClient;
                                            ErrorStr     : String;
                                            TotalReceived: Longint;
                                          end; { record }
        constructor Create;
        destructor  Destroy; override;

        property UseNickname  : String READ fNickName WRITE fNickName;
        property UseRealname  : String READ fRealname WRITE fRealName;
        property UsePassword  : String READ fPassword WRITE fPassword;
        property UseUserName  : String READ fUserName WRITE fUsername;
        property LastError : Longint READ fLastError DEFAULT 00;
        property ErrorStr  : String READ fGetErrStr;
        property ChanPrefix: String READ fChanPrefix WRITE fChanPrefix;
        property UserPrefix: String READ fUserPrefix WRITe fUserPrefix;
        property MotdEnd   : Boolean READ fMotdEnd WRITE fMotdEnd DEFAULT false;

        function Logon: Boolean;
        function GetOperator(UserName, Password: String): Boolean;
        function Join(Channel, Password: String): Boolean;
        function Part(Channel: String): Boolean;
        function Mode(Options: String): Boolean;
        function Topic(TopicStr: String): Boolean;
        function Names(Channel: String): Boolean;
        function List(Channel: String): Boolean;
        function Invite(_NickName, Channel: String): Boolean;
        function Kick(Channel, _NickName, Comment: String): Boolean;
        function Kill(_NickName, Comment: String): Boolean;
        function Ping(ServerName: String): Boolean;
        function Pong(Daemon: String): Boolean;
        function Error(ErrorMsg: String): Boolean;
        function Away(AwayMsg: String): Boolean;
        function Rehash: Boolean;
        function Restart: Boolean;
        function Summon(_NickName, ServerName: String): Boolean;
        function Users(Servername: String): Boolean;
        function Wallops(Msg: String): Boolean;
        function Userhost(_Nickname: String): Boolean;
        function IsOn(_Nickname: String): Boolean;

        function ServVersion(ServerName: String): Boolean;
        function ServStats(Query, Servername: String): Boolean;
        function ServLinks(RemSrv, Servername: String): Boolean;
        function ServTime(Servername: String): Boolean;
        function ServConnect(TargServer, Port, Remoteserver: String): Boolean;
        function ServTrace(ServerName: String): Boolean;
        function ServAdmin(ServerName: String): Boolean;
        function ServInfo(Servername: String): Boolean;

        function PrivMsg(Receiver, Msg: AnsiString): Boolean;
        function Notice(_NickName, Msg: String): Boolean;

        function UsrWho(_NickName: String; IsOperator: Boolean): Boolean;
        function UsrWhoIs(ServerName, _NickName: String): Boolean;
        function UsrWhoWas(ServerName, _NickName: String; Count: Longint): Boolean;

        procedure Quit(SendMsg: String);
        function CTCPSend(ToName, Query: String): Boolean;

        {----------------------- Utility functions ---------------------------}
        function MakeUpMessage(const IsStr: AnsiString): AnsiString;
        function CtCpEsc(S: String): String;
        function CheckError(TestSet: Array of Word; var LastErr: Longint; NeedResp: Boolean): Boolean;
        function HandleDccs: String;
        procedure ClearDccSession(Counter: Longint);

        procedure DeleteNickFromList(ChanName: String; Value: String);
        function SearchChan(S: String): Longint;
        function SearchNick(ChanIdx: Longint; S: String): Longint;
        procedure RemoveChannel(S: String);
     end; { tIRC client }

function IrcToPcColor(S: String): String;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 uses SockFunc, IrcDef, LongStr, WordStr, SysUtils, Ranges, Cases, CurTime,
       Jdates, StUtils, Global {$IFDEF VirtualPascal}, VpSysLow{$ENDIF},
        UnixDate, FileObj, CfgRec, StrPath, FileRout, RAL,
         ObjDec;
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tIrcClient.Create;
var Counter: longint;
begin
  inherited Create;

  UseNickName := '';
  UseRealName := '';
  UsePassword := '';
  UseUsername := '';
  FillChar(fChannels, SizeOf(fChannels), #00);
  FillChar(fDccs, SizeOf(fDccs), #0);

  for Counter := 01 to MaxChans do
    begin
      fChannels[Counter].fNickList := nil;
    end; { if }

  for Counter := 01 to MaxDccs do
    begin
      fDccs[Counter].DccClient := nil;
    end; { if }
end; { constructor Create }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tIrcClient.Destroy;
var Counter: Longint;
begin
  inherited Destroy;

  for Counter := 01 to MaxDccs do
    if fDccs[Counter].DccClient <> nil then
      begin
        fDccs[Counter].DccClient.Disconnect;
        fDccs[Counter].DccClient.Destroy;
        fDccs[Counter].DccClient := NIL;
      end; { if }

  for Counter := 01 to MaxChans do
    if fChannels[Counter].fChanName <> '' then
      begin
        fChannels[Counter].fChanName := '';
        Dispose(fChannels[Counter].fNickList, Done);
      end; { if }

end; { destructor Destroy }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.SearchChan(S: String): Longint;
var Counter: Longint;
    TempStr: String;
begin
  SearchChan := -1;
  S := SUpCase(s);

  for Counter := 01 to MaxChans do
    begin
      TempStr := fChannels[Counter].fChanName;

      if SUpCase(TempStr) = S then
        begin
          SearchChan := Counter;
          BREAK;
        end; { if }
    end; { if }
end; { func. Searchchan }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tIrcClient.RemoveChannel(S: String);
var Counter: Longint;
    TempStr: String;
begin
  S := SUpCase(s);
  if S = '' then EXIT;

  for Counter := 01 to MaxChans do
    begin
      TempStr := fChannels[Counter].fChanName;

      if TempStr <> '' then
       if SUpCase(TempStr) = S then
         begin
           fChannels[Counter].fChanName := '';
           Dispose(fChannels[Counter].fNickList, Done);
           fChannels[Counter].fNickList := nil;

           BREAK;
         end; { if }
    end; { if }
end; { proc. RemoveChannel }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tIrcClient.DeleteNickFromList(ChanName: String; Value: String);
var Counter: Longint;
    TempStr: String;
    ChanIDX: Longint;
begin
  ChanIDX := SearchChan(ChanName);
  if ChanIDX < 0 then EXIT;

  Value := SUpCase(Value);

  for Counter := 01 to MaxNicks do
    begin
      TempStr := fChannels[ChanIdx].fNickList^.Get(Counter);

      if SUpCase(TempStr) = Value then
        begin
          fChannels[ChanIdx].fNickList^.Delete(Counter);
          BREAK;
        end; { if }
    end; { if }
end; { proc. DeleteNickFromList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.SearchNick(ChanIdx: Longint; S: String): Longint;
var Counter: Longint;
    TempStr: String;
begin
  SearchNick := -1;
  S := SUpCase(s);
  if fChannels[ChanIdx].fNickList = nil then EXIT;

  for Counter := 01 to MaxNicks do
    begin
      TempStr := fChannels[ChanIdx].fNickList^.Get(Counter);


      if SUpCase(TempStr) = S then
        begin
          SearchNick := Counter;
          BREAK;
        end; { if }
    end; { if }
end; { func. SearchNick }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tIrcClient.ClearDccSession(Counter: Longint);
begin
  if fDccs[Counter].DccClient <> nil then
    begin
      fDccs[Counter].DccClient.Disconnect;
      fDccs[Counter].DccClient.Destroy;
      fDccs[Counter].DccClient := NIL;
    end; { if }

   fDccs[Counter].Active := dcc_NotActive;
end; { proc. ClearDccSession }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.HandleDccs: String;
type DccBufType = Array[0..MaxDCCBuf - 1] of Char;

var Counter    : Longint;
    Buf        : ^DccBufType;
    DccFile    : pFileObj;
    DidRead    : Longint;
    ConfirmRead: Longint;
begin
  {-- Allocate the buffer ---------------------------------------------------}
  HandleDccs := '';
  New(Buf);

  for Counter := 01 to MaxDCCs do
    begin
      {-- Check if there are sessions waiting for permissions --------------}
      if fDccs[Counter].Active = dcc_WaitPerm then
        begin
          {-- Notify user of whats coming in ---------------}
          Result := Format(^C'10' + langObj^.ralGet(ralDccSend),
                           [fDccs[Counter].Originate,
                            JustName(fDccs[Counter].FileName),
                            CommaStr(fDccs[Counter].FileSize),
                            FStr(Counter)]);
          fDccs[Counter].Active := dcc_Pending;
        end; { if }


      {-- Check if there are finished DCC sessions --------------------------}
      if fDccs[Counter].Active = dcc_Active then
       if fDccs[Counter].TotalReceived = fDccs[Counter].FileSize then
         begin
           Result := Format(^C'07' + langObj^.ralGet(ralDccDone), [JustName(fDccs[Counter].FileName)]);
           fDccs[Counter].Active := dcc_Finished;
         end; { if }


      {-- Handle pending DCC sessions ---------------------------------------}
      if fDCCs[Counter].Active = dcc_Active then
       if fDCCs[Counter].DccClient.DataAvailable then
          begin
            {-- Send the actual data ----------------------------------------}
            fDccs[Counter].DccClient.RecvBuf(Buf^, MaxDccBuf, DidRead);

            {-- Update the total counter, if TotalReceived = FileSize -------}
            {-- were all done -----------------------------------------------}
            Inc(fDccs[Counter].TotalReceived, DidRead);

            {-- Acknowledge to the sender that we received all --------------}
            ConfirmRead := SockNtoHL(fDccs[Counter].TotalReceived);
            fDccs[Counter].DccClient.SendBuf(ConfirmRead, SizeOf(ConfirmRead), ConfirmRead);

            {-- Commit the data to disk -------------------------------------}
            New(DccFile, Init);
            DccFile^.Assign(fDccs[Counter].FileName);
            DccFile^.FileMode := ReadWriteMode + DenyNone;
            if DccFile^.OpenOrCreate(1) then
              begin
                DccFile^.Seek(DccFile^.FileSize);
                DccFile^.BlkWrite(Buf^, DidRead);
              end; { if }
            Dispose(DccFile, Done);
          end; { if }

      {-- If disconnected, abort --------------------------------------------}
      if fDCCs[Counter].Active = dcc_Active then
       if NOT fDCCs[Counter].DccClient.ConnectionAlive then
        if fDCCs[Counter].FileSize > fDccs[Counter].TotalReceived then
          begin
            Result := Format(^C'07' + langObj^.ralGet(ralDccAbort), [JustName(fDccs[Counter].FileName)]);
            fDccs[Counter].Active := dcc_NotActive;

            ClearDccSession(Counter);
          end; { if }

    end; { for }

  {-- Dispose of the buffer -------------------------------------------------}
  Dispose(Buf);
end; { func. HandleDCCs }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.fGetErrStr: String;
begin
  Case LastError of
      ERR_NOSUCHNICK          : Result := 'No such nick/channel';
      ERR_NOSUCHSERVER        : Result := 'No such server';
      ERR_NOSUCHCHANNEL       : Result := 'No such channel';
      ERR_CANNOTSENDTOCHAN    : Result := 'Cannot send to channel';
      ERR_TOOMANYCHANNELS     : Result := 'You have joined too many channels';
      ERR_WASNOSUCHNICK       : Result := 'There was no such nickname';
      ERR_TOOMANYTARGETS      : Result := 'Duplicate recipients. No message delivered';
      ERR_NOORIGIN            : Result := 'No origin specified';
      ERR_NORECIPIENT         : Result := 'No recipient given';
      ERR_NOTEXTTOSEND        : Result := 'No text to send';
      ERR_NOTOPLEVEL          : Result := 'No toplevel domain specified';
      ERR_WILDTOPLEVEL        : Result := 'Wildcard in toplevel domain';
      ERR_UNKNOWNCOMMAND      : Result := 'Unknown command';
      ERR_NOMOTD              : begin
                                  Result := 'MOTD: File is missing';
                                  MotdEnd := true;
                                end; { if }
      ERR_NOADMININFO         : Result := 'No administrative info available';
      ERR_FILEERROR           : Result := 'File error doing <???> on <???>';
      ERR_NONICKNAMEGIVEN     : Result := 'No nickname given';
      ERR_ERRONEUSNICKNAME    : Result := 'Erroneous nickname';
      ERR_NICKNAMEINUSE       : Result := 'Nickname is already in use';
      ERR_NICKCOLLISION       : Result := 'Nickname collision KILL';
      ERR_USERNOTINCHANNEL    : Result := 'There aren''t on that channel';
      ERR_NOTONCHANNEL        : Result := 'You''re not on that channel';
      ERR_USERONCHANNEL       : Result := 'Is already on channel';
      ERR_NOLOGIN             : Result := 'User not logged on';
      ERR_SUMMONDISABLED      : Result := 'SUMMON has been disabled';
      ERR_USERSDISABLED       : Result := 'USERS has been disabled';
      ERR_NOTREGISTERED       : Result := 'You have not registered';
      ERR_NEEDMOREPARAMS      : Result := 'Not enough parameters';
      ERR_ALREADYREGISTERED   : Result := 'You may not reregister';
      ERR_NOPERMFORHOST       : Result := 'Your host isn''t among the privileged';
      ERR_PASSWDMISMATCH      : Result := 'Password incorrect';
      ERR_YOUREBANNEDCREEP    : Result := 'You are banned from this server';
      ERR_KEYSET              : Result := 'Channel key already set';
      ERR_CHANNELISFULL       : Result := 'Cannot join channel (+l)';
      ERR_UNKNOWNMODE         : Result := 'Is a unknown mode char to me';
      ERR_INVITEONLYCHAN      : Result := 'Cannot join channel (+i)';
      ERR_BANNEDFROMCHAN      : Result := 'Cannot join channel (+b)';
      ERR_BADCHANNELKEY       : Result := 'Cannot join channel (+k)';
      ERR_NOPRIVILEGES        : Result := 'Permission denied - You''re not an IRC operator';
      ERR_CHANOPRIVSNEEDED    : Result := 'You''re not an channel operator';
      ERR_CANTKILLSERVER      : Result := 'You can''t kill a server!';
      ERR_NOOPERHOST          : Result := 'No O-lines for your host!';
      ERR_UMODEUNKNOWNFLAG    : Result := 'Unknown MODE flag';
      ERR_USERSDONTMATCH      : Result := 'Can''t change mode for other users';
        else Result := '#' + FStr(LastError) + ' (unidentified error)';
  end;
end; { func. fGetErrStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.CTCPSend(ToName, Query: String): Boolean;
begin
  CTCPSend := PrivMsg(ToName, ^A + Query + ^A);
end; { proc. CTCPSend }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.Logon: Boolean;
var AbortLogon: Boolean;
    TempStr   : AnsiString;
begin
  AbortLogon := false;
  Result := false;

  if UsePassWord <> '' then
     begin
       SendStrLn('PASS '+UsePassword);

       if CheckError([ERR_NEEDMOREPARAMS, ERR_ALREADYREGISTERED],
                     fLastError, false) then AbortLogon := true;
     end; { if }

  if NOT AbortLogon then
    begin
      SendStrLn('NICK '+UseNickName);

      if CheckError([ERR_NONICKNAMEGIVEN, ERR_ERRONEUSNICKNAME,
                     ERR_NICKNAMEINUSE,   ERR_NICKCOLLISION,
                     ERR_NEEDMOREPARAMS,  ERR_ALREADYREGISTERED], fLastError, false) then
                       AbortLogon := true;
    end; { if }

  if NOT AbortLogon then
    begin
      if UseUserName = '' then UseUsername := 'username';

      SendStrLn('USER '+UseUserName+' hostname servername :'+UseRealName);

      if CheckError([ERR_NEEDMOREPARAMS, ERR_ALREADYREGISTERED,
                     ERR_NONICKNAMEGIVEN, ERR_ERRONEUSNICKNAME,
                     ERR_NICKNAMEINUSE,   ERR_NICKCOLLISION], fLastError, false) then
               AbortLogon := true;
    end; { with }

  if NOT AbortLogon then
    begin
      if NOT DataAvailable then DoSleep(100);
      if NOT DataAvailable then DoSleep(100);
      if NOT DataAvailable then DoSleep(100);

      if CheckError([ERR_NEEDMOREPARAMS, ERR_ALREADYREGISTERED,
                     ERR_NONICKNAMEGIVEN, ERR_ERRONEUSNICKNAME,
                     ERR_NICKNAMEINUSE,   ERR_NICKCOLLISION], fLastError, false) then
               AbortLogon := true;
    end; { if }

  Result := NOT AbortLogon;
  if AbortLogon then
    RecvStrLn(TempStr, false);
end; { proc. Logon }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.GetOperator(UserName, Password:String): Boolean;
begin
  SendStrLn('OPER '+UserName+#32+PassWord);

  Result := NOT CheckError([ERR_NEEDMOREPARAMS,
                            ERR_NOOPERHOST, ERR_PASSWDMISMATCH], fLastError,
                            true);
end; { func. GetOperator }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tIrcClient.Quit(SendMsg: String);
begin
  if SendMsg <> '' then SendMsg := ':' + SendMsg;

  SendStrLn('QUIT '+SendMsg);
end; { proc. Quit }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.Join(Channel, Password: String): Boolean;
var Counter: Longint;
begin
  SendStrLn('JOIN '+Trim(Channel+#32+Password));

  if NOT DataAvailable then DoSleep(50);
  Result := NOT CheckError([ERR_NEEDMOREPARAMS, ERR_BANNEDFROMCHAN,
                            ERR_INVITEONLYCHAN, ERR_BADCHANNELKEY,
                            ERR_CHANNELISFULL,  ERR_BADCHANMASK,
                            ERR_NOSUCHCHANNEL,  ERR_TOOMANYCHANNELS], fLastError,
                            true);
  if Result then
    begin
      for Counter := 01 to MaxChans do
        if fChannels[Counter].fChanName = '' then
          begin
            fChannels[Counter].fChanName := SUpCase(Channel);
            New(fChannels[Counter].fNickList, Init(MaxNicks));

            BREAK;
          end; { if }

    end; { if }
end; { func. Join }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.Part(Channel: String): Boolean;
begin
  SendStrLn('PART '+Channel);

  Result := NOT CheckError([ERR_NEEDMOREPARAMS, ERR_NOTONCHANNEL,
                            ERR_NOSUCHCHANNEL], fLastError,
                            false);

  RemoveChannel(Channel);
end; { func. Part }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.Mode(Options: String): Boolean;
begin
  SendStrLn('MODE '+Options);

  Result := NOT CheckError([ERR_NEEDMOREPARAMS, ERR_CHANOPRIVSNEEDED,
                            ERR_NOSUCHNICK, ERR_NOTONCHANNEL,
                            ERR_KEYSET, ERR_UNKNOWNMODE,
                            ERR_NOSUCHCHANNEL, ERR_USERSDONTMATCH,
                            ERR_UMODEUNKNOWNFLAG], fLastError,
                            false);
end; { func. Mode }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.Topic(TopicStr: String): Boolean;
begin
  if TopicStr <> '' then TopicStr := ':'+TopicStr;
  SendStrLn('TOPIC '+TopicStr);

  Result := NOT CheckError([ERR_NEEDMOREPARAMS, ERR_NOTONCHANNEL,
                            ERR_CHANOPRIVSNEEDED], fLastError,
                            false);
end; { func. Topic }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.Names(Channel: String): Boolean;
begin
  SendStrLn('NAMES '+Channel);

  Result := true;
end; { func. Names }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.List(Channel: String): Boolean;
begin
  SendStrLn('LIST '+Channel);

  Result := NOT CheckError([ERR_NOSUCHSERVER], fLastError, false);
end; { func. List }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.Invite(_NickName, Channel: String): Boolean;
begin
  SendStrLn('INVITE '+_NickName+#32+Channel);

  Result := NOT CheckError([ERR_NEEDMOREPARAMS, ERR_NOSUCHNICK,
                            ERR_NOTONCHANNEL, ERR_USERONCHANNEL,
                            ERR_CHANOPRIVSNEEDED], fLastError, false);
end; { func. Invite }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.Kick(Channel, _NickName, Comment: String): Boolean;
begin
  if Comment <> '' then Comment := ':' + Comment;

  SendStrLn('KICK '+Channel+#32+_NickName+#32+Comment);

  Result := NOT CheckError([ERR_NEEDMOREPARAMS, ERR_NOSUCHCHANNEL,
                            ERR_BADCHANMASK, ERR_NOSUCHCHANNEL,
                            ERR_NOTONCHANNEL, ERR_CHANOPRIVSNEEDED], fLastError,
                            false);
end; { func. Kick }


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.ServVersion(ServerName: String): Boolean;
begin
  SendStrLn('VERSION '+ServerName);

  Result := NOT CheckError([ERR_NOSUCHSERVER], fLastError, false);
end; { func. ServVersion }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.ServStats(Query, ServerName: String): Boolean;
begin
  SendStrLn('STATS '+Query+#32+ServerName);

  Result := NOT CheckError([ERR_NOSUCHSERVER], fLastError, false);
end; { func. ServStats }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.ServLinks(RemSrv, Servername: String): Boolean;
begin
  SendStrLn('LINKS '+RemSrv+#32+ServerName);

  Result := NOT CheckError([ERR_NOSUCHSERVER], fLastError, false);
end; { func. ServLinks }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.ServTime(Servername: String): Boolean;
begin
  SendStrLn('TIME '+ServerName);

  Result := NOT CheckError([ERR_NOSUCHSERVER], fLastError, false);
end; { func. ServTime }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.ServConnect(TargServer, Port, Remoteserver: String): Boolean;
begin
  SendStrLn('CONNECT '+TargServer+#32+Port+#32+RemoteServer);

  Result := NOT CheckError([ERR_NOSUCHSERVER, ERR_NOPRIVILEGES,
                            ERR_NEEDMOREPARAMS], fLastError, false);
end; { func. ServConnect }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.ServTrace(ServerName: String): Boolean;
begin
  SendStrLn('TRACE '+ServerName);

  Result := NOT CheckError([ERR_NOSUCHSERVER], fLastError, false);
end; { func. ServTrace }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.ServAdmin(ServerName: String): Boolean;
begin
  SendStrLn('ADMIN '+ServerName);

  Result := NOT CheckError([ERR_NOSUCHSERVER], fLastError, false);
end; { func. ServAdmin }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.ServInfo(ServerName: String): Boolean;
begin
  SendStrLn('INFO '+ServerName);

  Result := NOT CheckError([ERR_NOSUCHSERVER], fLastError, false);
end; { func. ServInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.PrivMsg(Receiver, Msg: AnsiString): Boolean;
begin
  if Msg <> '' then
    Msg := ':' + Msg;

  SendAnsiStrLn('PRIVMSG '+Receiver+#32+Msg);
  Result := NOT CheckError([ERR_NORECIPIENT, ERR_NOTEXTTOSEND,
                            ERR_CANNOTSENDTOCHAN, ERR_NOTOPLEVEL,
                            ERR_WILDTOPLEVEL, ERR_TOOMANYTARGETS,
                            ERR_NOSUCHNICK], fLastError, false);
end; { func. PrivMsg }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.Notice(_NickName, Msg: String): Boolean;
begin
  if Msg <> '' then
    Msg := ':' + Msg;

  SendStrLn('NOTICE '+_NickName+#32+Msg);

  Result := NOT CheckError([ERR_NORECIPIENT, ERR_NOTEXTTOSEND,
                            ERR_CANNOTSENDTOCHAN, ERR_NOTOPLEVEL,
                            ERR_WILDTOPLEVEL, ERR_TOOMANYTARGETS,
                            ERR_NOSUCHNICK], fLastError, false);
end; { func. PrivMsg }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.UsrWho(_NickName: String; IsOperator: Boolean): Boolean;
begin
  if IsOperator then SendStrLn('WHO '+_NickName+' o')
    else SendStrLn('WHO '+_NickName);

  Result := NOT CheckError([ERR_NOSUCHSERVER], fLastError, false);
end; { func. UsrWho }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.UsrWhoIs(ServerName, _NickName: String): Boolean;
begin
  SendStrLn('WHOIS '+ServerName+#32+_NickName);

  Result := NOT CheckError([ERR_NOSUCHSERVER, ERR_NONICKNAMEGIVEN,
                            ERR_NOSUCHNICK], fLastError, false);
end; { func. UsrWhoIs }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.UsrWhoWas(ServerName, _NickName: String; Count: Longint): Boolean;
begin
  SendStrLn('WHOWAS '+_NickName+#32+FStr(Count)+#32+ServerName);

  Result := NOT CheckError([ERR_NOSUCHSERVER, ERR_WASNOSUCHNICK], fLastError, false);
end; { func. UsrWhoWas }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.Kill(_NickName, Comment: String): Boolean;
begin
  SendStrLn('KILL '+_NickName+#32+Comment);

  Result := NOT CheckError([ERR_NOPRIVILEGES, ERR_NEEDMOREPARAMS,
                            ERR_NOSUCHNICK, ERR_CANTKILLSERVER], fLastError, false);
end; { func. Kill }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.Ping(ServerName: String): Boolean;
begin
  SendStrLn('PING '+ServerName);

  Result := NOT CheckError([ERR_NOORIGIN, ERR_NOSUCHSERVER], fLastError, false);
end; { func. Ping }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.Pong(Daemon: String): Boolean;
begin
  SendStrLn('PONG '+Daemon);

  Result := NOT CheckError([ERR_NOORIGIN, ERR_NOSUCHSERVER], fLastError, false);
end; { func. Pong }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.Error(ErrorMsg: String): Boolean;
begin
  SendStrLn('ERROR '+ErrorMsg);

  Result := NOT CheckError([ERR_NOORIGIN, ERR_NOSUCHSERVER], fLastError, false);
end; { func. Error }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.Away(AwayMsg: String): Boolean;
begin
  SendStrLn('AWAY '+AwayMsg);

  Result := NOT CheckError([ERR_NOORIGIN, ERR_NOSUCHSERVER], fLastError, false);
end; { func. Away }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.Rehash: Boolean;
begin
  SendStrLn('REHASH');

  Result := NOT CheckError([ERR_NOPRIVILEGES], fLastError, false);
end; { func. Rehash }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.Restart: Boolean;
begin
  SendStrLn('RESTART');

  Result := NOT CheckError([ERR_NOPRIVILEGES], fLastError, false);
end; { func. Restart }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.Summon(_NickName, ServerName: String): Boolean;
begin
  SendStrLn('SUMMON '+_NickName+#32+Servername);

  Result := NOT CheckError([ERR_NORECIPIENT, ERR_FILEERROR,
                            ERR_NOLOGIN, ERR_NOSUCHSERVER], fLastError, false);
end; { func. Summon }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.Users(Servername: String): Boolean;
begin
  SendStrLn('USERS '+Servername);

  Result := NOT CheckError([ERR_NOSUCHSERVER, ERR_FILEERROR,
                            ERR_USERSDISABLED], fLastError, false);
end; { func. Users }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.Wallops(Msg: String): Boolean;
begin
  if Msg <> '' then Msg := ':'+Msg;

  SendStrLn('WALLOPS '+Msg);

  Result := NOT CheckError([ERR_NEEDMOREPARAMS], fLastError, false);
end; { func. Wallops }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.Userhost(_Nickname: String): Boolean;
begin
  SendStrLn('USERHOST '+_Nickname);

  Result := NOT CheckError([ERR_NEEDMOREPARAMS], fLastError, false);
end; { func. Userhost }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.IsOn(_Nickname: String): Boolean;
begin
  SendStrLn('ISON '+_Nickname);

  Result := NOT CheckError([ERR_NEEDMOREPARAMS], fLastError, false);
end; { func. IsOn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function IrcToPcColor(S: String): String;
var Temp: Byte;
begin
  Temp := Fval(s);
  Result := '15';

  Case Temp of
     00 : Result := '15';             { 0 to white }
     01 : Result := '00';             { 1 to black }
     02 : Result := '01';             { 2 to blue }
     03 : Result := '02';             { 3 to green }
     04 : Result := '04';             { 4 to red }
     05 : Result := '06';             { 5 to brown }
     06 : Result := '13';             { 6 to Lightmagenta }
     07 : Result := '12';             { 7 to lightred }
     08 : Result := '14';             { 8 to yellow }
     09 : Result := '10';             { 9 to lightgreen }
     10 : Result := '03';             { 10 to lightblue }
     11 : Result := '11';             { 11 to lightcyan }
     12 : Result := '09';             { 12 to lightblue }
     13 : Result := '05';             { 13 to magenta }
     14 : Result := '08';             { 14 to darkgray }
     15 : Result := '15';             { 15 to white }
  end; { case }
end; { func. IrcToPcColor }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.CtCpEsc(S: String): String;
var TempStr: String;
    Counter: Byte;
begin
  TempStr := '';

  for Counter := 01 to Length(s) do
   Case S[Counter] of
     #00 : TempStr := TempStr + '\0';
     #01 : TempStr := TempStr + '\1';
     #10 : TempStr := TempStr + '\n';
     #13 : TempStr := TempStr + '\r';
     '\' : TempStr := TempStr + '\\';
       else TempStr := TempStr + S[Counter];
   end; { case }

  Result := TempStr;
end; { func. CtcpEsc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.MakeUpMessage(const IsStr: AnsiString): AnsiString;

function GetColumnMsg(S: AnsiString): AnsiString;
begin
  if Pos(':', S) > 0 then
    Result := Copy(S, Pos(':', S)+1, Length(s))
     else Result := '';
end; { func. GetColumnMsg }

procedure RemoveColumns(var s: Ansistring);
var TempStr: AnsiString;
    Counter: Longint;
begin
  TempStr := '';

  for Counter := 01 to Length(s) do
   if S[Counter] <> ':' then TempStr := TempStr + s[counter];

  S := TempStr;
end; { proc. RemoveColumns }

var ReplCode  : Longint;
    Counter   : Longint;
    Params    : AnsiString;
    ParamArray: Array[1..250] of AnsiString;
    CommentMsg: AnsiString;
    Command   : AnsiString;
    S         : AnsiString;
    ChanIDX   : Longint;
    ChanCnt   : Longint;
    TempPos   : Longint;
    ThisDCC   : Longint;
begin
  S := IsStr;
  {$IFDEF WITH_DEBUG}
    DebugLog('.', 'MakeUpMessage (begin) S='+S);
  {$ENDIF}

  Result := s;

  if S[1] = ':' then
    Delete(S, 1, 1);

  ReplCode := FVal(ExtractWord(S, 2, defExtractWord, false, false));

  if InRange(ReplCode, ERR_NOSUCHNICK, ERR_USERSDONTMATCH) then
    begin
      fLastError := ReplCode;
      Result := ExtractWord(S, 4, defExtractWord, false, false) + #32 + fGetErrStr;
      EXIT;
    end; { hm }

  if ReplCode > 0 then
   begin
     RemoveWordNrA(s, 1, defExtractWord, false);          { Remove ip }
     RemoveWordNrA(s, 1, defExtractWord, false);          { Remove nick }
     RemoveWordNrA(s, 1, defExtractWord, false);          { Remove error code }

     Params := Copy(S, 1, Length(S) - (Length(GetColumnMsg(s)) + 01));
     for Counter := 01 to WordCount(Params, defExtractWord, false) do
       ParamArray[Counter] := ExtractWord(S, Counter, defExtractWord, false, false);
     CommentMsg := GetColumnMsg(S);

     Case Replcode of
          RPL_NONE           : Result := Format('%s', [CommentMsg]);
{!}       RPL_USERHOST       : Result := Format('%s', [CommentMsg]);
{!}       RPL_ISON           : Result := Format('ison: %s', [CommentMsg]);
          RPL_AWAY           : Result := Format('%s is away: %s', [ParamArray[01], CommentMsg]);
          RPL_UNAWAY         : Result := Format('%s', [CommentMsg]);
          RPL_NOWAWAY        : Result := Format('%s', [CommentMsg]);
          RPL_WHOISUSER      : Result := Format('%s is %s@%s %s %s', [ParamArray[01], ParamArray[02],
                                                  ParamArray[03], ParamArray[03], CommentMsg]);
          RPL_WHOISSERVER    : Result := Format('%s using %s %s', [ParamArray[01], ParamArray[02], CommentMsg]);
{!}       RPL_WHOISOPERATOR  : Result := s;
{!}       RPL_WHOISIDLE      : Result := s;
          RPL_ENDOFWHOIS     : Result := Format('%s %s', [ParamArray[01], CommentMsg]);
{!}       RPL_WHOISCHANNELS  : Result := s;
          RPL_WHOWASUSER     : Result := Format('%s is %s@%s %s %s', [ParamArray[01], ParamArray[02],
                                                  ParamArray[03], ParamArray[03], CommentMsg]);
          RPL_ENDOFWHOWAS    : Result := Format('%s %s', [ParamArray[01], CommentMsg]);
{!}       RPL_LISTSTART      : Result := s;
{!}       RPL_LIST           : Result := s;
          RPL_LISTEND        : Result := Format('%s %s', [ParamArray[01], CommentMsg]);
{!}       RPL_CHANNELMODEIS  : Result := Format('%s %s %s', [ParamArray[01], ParamArray[02], ParamArray[03]]);
          RPL_NOTOPIC        : Result := Format('%s %s', [ParamArray[01], CommentMsg]);
          RPL_TOPIC          : Result := Format('%s Topic is %s', [ParamArray[01], CommentMsg]);
          RPL_INVITING       : Result := Format('%s has been invited to %s', [ParamArray[01], ParamArray[02]]);
          RPL_VERSION        : Result := Format('%s %s %s', [ParamArray[01], ParamArray[02], CommentMsg]);
{!}       RPL_WHOREPLY       : Result := Format('%s %s %s %s@%s :%s',
                                                 [ParamArray[01], ParamArray[05],
                                                  ParamArray[06], ParamArray[02],
                                                  ParamArray[03], CommentMsg]);
          RPL_ENDOFWHO       : Result := Format('%s %s', [ParamArray[01], CommentMsg]);
          RPL_NAMREPLY       : begin
                                  Result := Format('%s %s', [ParamArray[02], CommentMsg]);

                                  for Counter := 01 to WordCount(CommentMsg, defExtractWord, false) do
                                    begin
                                      ParamArray[03] := ExtractWord(CommentMsg, Counter, defExtractWord, false, false);

                                      { A user can have @+elevator ;-) }
                                      if ParamArray[03][1] in ['@', '+'] then Delete(ParamArray[03], 1, 1);
                                       if ParamArray[03][1] in ['@', '+'] then Delete(ParamArray[03], 1, 1);

                                       ChanIDX := SearchChan(ParamArray[02]);
                                       if ChanIDX >= 0 then
                                        if SearchNick(ChanIdx, ParamArray[03]) < 0 then
                                          fChannels[ChanIDX].fNickList^.Add(ParamArray[03]);
                                     end; { for }
                               end; { names }
          RPL_ENDOFNAMES     : Result := Format('%s %s', [ParamArray[01], CommentMsg]);
{!}       RPL_LINKS          : Result := s;
{!}       RPL_ENDOFLINKS     : Result := s;
{!}       RPL_BANLIST        : Result := s;
{!}       RPL_ENDOFBANLIST   : Result := s;
          RPL_INFO           : Result := Format('%s', [CommentMsg]);
          RPL_ENDOFINFO      : Result := Format('%s', [CommentMsg]);
          RPL_MOTDSTART      : Result := Format('%s', [CommentMsg]);
          RPL_MOTD           : Result := Format('%s', [CommentMsg]);
          RPL_ENDOFMOTD      : begin
                                 Result := Format('%s', [CommentMsg]);
                                 MotdEnd := true;
                               end; { motd }
{!}       RPL_YOUREOPER      : Result := s;
{!}       RPL_REHASHING      : Result := s;
          RPL_TIME           : Result := Format('%s', [CommentMsg]);
          RPL_USERSSTART     : Result := Format('%s', [CommentMsg]);
          RPL_USERS          : Result := Format('%-8s %-9s %-8s',
                                                [ExtractWord(CommentMsg, 1, defExtractWord, false, false),
                                                 ExtractWord(CommentMsg, 2, defExtractWord, false, false),
                                                 ExtractWord(CommentMsg, 3, defExtractWord, false, false)]);
          RPL_ENDOFUSERS     : Result := Format('%s', [CommentMsg]);
          RPL_NOUSERS        : Result := Format('%s', [CommentMsg]);
{!}       RPL_TRACELINK      : Result := s;
{!}       RPL_TRACECONNECTING: Result := s;
{!}       RPL_TRACEHANDSHAKE : Result := s;
{!}       RPL_TRACEUNKNOWN   : Result := s;
{!}       RPL_TRACEOPERATOR  : Result := s;
{!}       RPL_TRACEUSER      : Result := s;
{!}       RPL_TRACESERVER    : Result := s;
{!}       RPL_TRACENEWTYPE   : Result := s;
{!}       RPL_TRACELOG       : Result := s;
{!}       RPL_STATSLINKINFO  : Result := s;
{!}       RPL_STATSCOMMANDS  : Result := s;
{!}       RPL_STATSCLINE     : Result := s;
{!}       RPL_STATSNLINE     : Result := s;
{!}       RPL_STATSILINE     : Result := s;
{!}       RPL_STATSKLINE     : Result := s;
{!}       RPL_STATSYLINE     : Result := s;
{!}       RPL_ENDOFSTATS     : Result := s;
{!}       RPL_STATSLLINE     : Result := s;
{!}       RPL_STATSUPTIME    : Result := s;
{!}       RPL_STATSOLINE     : Result := s;
{!}       RPL_STATSHLINE     : Result := s;
{!}       RPL_UMODEIS        : Result := s;
          RPL_LUSERCLIENT    : Result := Format('%s', [CommentMsg]);
          RPL_LUSEROP        : Result := Format('%s %s', [ParamArray[01], CommentMsg]);
          RPL_LUSERUNKNOWN   : Result := Format('%s %s', [ParamArray[01], CommentMsg]);
          RPL_LUSERCHANNELS  : Result := Format('%s %s', [ParamArray[01], CommentMsg]);
          RPL_LUSERME        : Result := Format('%s', [CommentMsg]);
          RPL_ADMINME        : Result := Format('%s', [CommentMsg]);
          RPL_ADMINLOC1      : Result := Format('%s', [CommentMsg]);
          RPL_ADMINLOC2      : Result := Format('%s', [CommentMsg]);
          RPL_ADMINEMAIL     : Result := Format('%s', [CommentMsg]);
        else Result := Copy(S, Pos(':', S) + 01, Length(s));
     end;

     if Result[1] = ':' then
       Delete(Result, 1, 1);
   end
    else begin
           Command := Trim(UpperCase(ExtractWord(S, 2, defExtractWord, false, false)));
           if Length(Command) >= 1 then
             begin
               if Command[1] = ':' then
                Command := Trim(UpperCase(ExtractWord(S, 1, defExtractWord, false, false)));
             end; { if }

           DebugLog('.', Format('IRC, Command=(%s), FullStr = (%s)', [Command, S]));

           CommentMsg := GetColumnMsg(S);

           if Command = 'JOIN' then
            begin
              ParamArray[01] := Copy(S, 1, Pos('!', S) - 1);
              ParamArray[02] := Copy(S, Pos('!', S) + 1, Pos(#32, S) - (Length(ParamArray[01])+02));

              Result := Format(ChanPreFix + '*** %s (%s) has joined %s', [
                               Supcase(CommentMsg),
                               ParamArray[01],
                               ParamArray[02], CommentMsg]);

              ChanIDX := SearchChan(CommentMsg);
              if ChanIDX >= 0 then
                if SearchNick(ChanIdx, ParamArray[01]) < 0 then
                  fChannels[ChanIDX].fNickList^.Add(ParamArray[01]);
            end; { if }

           if Command = 'PART' then
            begin
              ParamArray[01] := Copy(S, 1, Pos('!', S) - 1);
              ParamArray[02] := Copy(S, Pos('!', S) + 1, Pos(#32, S) - (Length(ParamArray[01])+2));
              ParamArray[03] := ExtractWord(S, 3, defExtractWord, false, false);

              Result := Format(ChanPreFix + '*** %s (%s) has left %s (%s)', [
                               Supcase(ParamArray[03]),
                               ParamArray[01],
                               ParamArray[02], ParamArray[03], CommentMsg]);

              DeleteNickFromList(ParamArray[03], ParamArray[01]);
            end; { if }

           if Command = 'QUIT' then
            begin
              ParamArray[01] := Copy(S, 1, Pos('!', S) - 1);

              if CommentMsg <> '' then
                Result := Format('*** %s has quit IRC (%s)', [ParamArray[01], CommentMsg])
                 else Result := Format('*** %s has quit IRC', [ParamArray[01]]);

              for ChanCnt := 01 to MaxChans do
                if fChannels[ChanCnt].fChanName <> '' then
                  DeleteNickFromList(fChannels[ChanCnt].fChanName, ParamArray[01]);
             end; { if }

           if Command = 'KICK' then
            begin
              ParamArray[01] := Copy(S, 1, Pos('!', S) - 1);
              ParamArray[02] := Copy(S, Pos('!', S) + 1, Pos(#32, S) - (Length(ParamArray[01])+2));
              ParamArray[03] := ExtractWord(S, 3, defExtractWord, false, false);

              Result := Format(ChanPreFix + '*** %s (%s) was kicked from %s by %s', [
                               SupCase(ParamArray[03]),
                               ParamArray[01],
                               ParamArray[02], ParamArray[03], CommentMsg]);
              DeleteNickFromList(ParamArray[03], ParamArray[01]);
            end; { if }

           if Command = 'MODE' then
             begin
              ParamArray[01] := Copy(S, 1, Pos('!', S) - 1);
              ParamArray[02] := ExtractWord(S, 3, defExtractWord, false, false);
              ParamArray[04] := ExtractWord(S, 4, defExtractWord, false, false);

              Result := Format(ChanPreFix + ^C'03*** %s sets mode: %s', [
                               SUpCase(ParamArray[02]),
                               ParamArray[01], ParamArray[04]]);
             end; { if }

           if Command = 'PING' then
             begin
               Result := Format(^C'03PING? PONG!', [CommentMsg]);
               SendStrLn('PONG ' +CommentMsg);
             end; { if }

           if Command = 'BAN' then
            begin
              ParamArray[01] := Copy(S, 1, Pos('!', S) - 1);
              ParamArray[02] := Copy(S, Pos('!', S) + 1, Pos(#32, S) - (Length(ParamArray[01])+2));
              ParamArray[03] := ExtractWord(S, 3, defExtractWord, false, false);

              Result := Format(ChanPreFix + '*** %s (%s) was banned from %s by %s', [
                               SUpCase(ParamArray[03]),
                               ParamArray[01],
                               ParamArray[02], ParamArray[03], CommentMsg]);
            end; { if }

           if Command = 'NOTICE' then
            begin
              if CommentMsg[1] <> ^A then
                begin
                  Result := Format(^C'05%s', [CommentMsg]);
                end
                  else begin
                         ParamArray[01] := Copy(S, 1, Pos('!', S) - 1);

                         { Delete the first ^A }
                         Delete(CommentMsg, 1, 1);
                         ParamArray[2] := FirstWordA(CommentMsg, defExtractWord, false);

                         {-- Delete the last ^A }
                         Delete(CommentMsg, Length(CommentMsg), 1);

                         Result := Format(^C'07[%s %s reply]: %s', [ParamArray[1], ParamArray[2], CommentMsg]);
                       end; { if }
            end; { if }

           if Command = 'PRIVMSG' then
            begin
              ParamArray[01] := Copy(S, 1, Pos('!', S) - 1);
              ParamArray[03] := ExtractWord(S, 3, defExtractWord, false, false);

              if CommentMsg[1] <> ^A then
                begin
                  if FirstChar(ExtractWord(S, 3, defExtractWord, false, false)) in ['#', '&'] then
                   begin
                      With GlobalCfg^.RaConfig^ do
                        Result := Format(ChanPreFix + UserPrefix + '%s', [
                                         Supcase(ParamArray[03]), ParamArray[01],
                                          CommentMsg])
                   end
                     else Result := Format('*** Private message from %s: %s', [ParamArray[01], CommentMsg]);
                end; { if NOT CTCP }

              if CommentMsg[1] = ^A then
                begin
                  Result := Format(^C'07[%s] %s', [ParamArray[03], '?']);

                  {-- CTCP protocol -----------------------------------------}
                  if Copy(CommentMsg, 1, Length('ACTION'^A))=^A'ACTION' then
                    begin
                      RemovewordNrA(CommentMsg, 1, defExtractWord, false);
                      Delete(CommentMsg, Length(CommentMsg), 1);

                      Result := Format(ChanPreFix + ^C'06* %s %s', [
                                         SupCase(ParamArray[03]),
                                         ParamArray[01], TrimLeft(CommentMsg)]);
                    end; { action }

                  if CommentMsg=^A'VERSION'^A then
                    begin
                      Result := Format(^C'07[%s] VERSION', [ParamArray[03]]);

                      ParamArray[02] := FullProgName;
                      ParamArray[03] := 'v' + VersionID;
                      ParamArray[04] := PlatformIDStr;

                      Notice(ParamArray[01], ^A'VERSION'+ #32 + ParamArray[02] + #32 +
                                             ParamArray[03] + #32 + ParamArray[04] + ^A);
                    end; { version }

                  if CommentMsg=^A'FINGER'^A then
                   with contrObj^ do
                    begin
                      Result := Format(^C'07[%s] FINGER', [ParamArray[03]]);

                      ParamArray[02] := Format('%s (%s) Idle %d seconds', [UseNickName, UseRealName, Abs((TimeInfo.IdleLimit - CurrentTime) - TimeInfo.IdleTime)]);

                      Notice(ParamArray[01], ^A'FINGER'+ #32 + ParamArray[02] + ^A);
                    end; { finger }

                  if Copy(CommentMsg, 1, Length(^A'PING')) = ^A'PING' then
                    begin
                      Result := Format(^C'07[%s] PING', [ParamArray[03]]);

                      Notice(ParamArray[01], ^A'PING'+ #32 + Copy(CommentMsg, 7, Length(CommentMsg) - 7 )+ ^A);
                    end; { ping }

                  if CommentMsg=^A'TIME'^A then
                    begin
                      Result := Format(^C'07[%s] TIME', [ParamArray[03]]);

                      ParamArray[02] := Format('%s, %s %d %s %d',
                                               [GetDayOfWeek,
                                                MonthString(GetMonth),
                                                GetDay,
                                                TimeStr(true, false),
                                                GetYear]);

                      Notice(ParamArray[01], ^A'TIME'+ #32 + ParamArray[02] + ^A);
                    end; { TIME }

                  if Copy(CommentMsg, 1, Length(^A'DCC'))=^A'DCC' then
                    begin
                      ParamArray[1] := SUpcase(ExtractWord(CommentMsg, 2, defExtractWord, false, false));

                      {-- CHAT request comming in, denying -----------------}
                      if ParamArray[1] = 'CHAT' then
                        begin
                          ParamArray[04] := Copy(S, 1, Pos('!', S) - 1);

                          Result := ^C'07' + langObj^.ralGet(ralDccChat);
                          Notice(ParamArray[4], 'DCC chat not supported by this client');
                        end; { if }

                      {-- Send request coming in ---------------------------}
                      if ParamArray[1] = 'SEND' then
                        begin
                          { Argument (filename), address, port and the size }
                          Result := '';
                          ParamArray[1] := Copy(S, Pos('!', S) + 1, Pos(#32, S) - (Length(ParamArray[01])+02));
                          ParamArray[2] := Copy(S, 1, Pos('!', S) - 1);

                          ParamArray[3] := ExtractWord(CommentMsg, 3, defExtractWord, false, false);
                          ParamArray[4] := ExtractWord(CommentMsg, 4, defExtractWord, false, false);
                          ParamArray[5] := ExtractWord(CommentMsg, 5, defExtractWord, false, false);
                          ParamArray[6] := ExtractWord(CommentMsg, 6, defExtractWord + [^A], false, false);

                          {-- Lookup an empty DCC handle --------------------}
                          ThisDCC := -1;

                          for Counter := 01 to MaxDCCs do
                            if fDCCs[Counter].Active = dcc_NotActive then
                              begin
                                ThisDCC := Counter;
                                BREAK;
                              end; { if }

                          {-- No slots free, ignore DCC send ----------------}
                          if ThisDCC = -1 then
                            begin
                              ParamArray[04] := Copy(S, 1, Pos('!', S) - 1);

                              Result := langObj^.ralGet(ralMaxDccs);
                              Notice(ParamArray[4], 'Maximum number of DCC sessions reached by client');
                            end; { if }

                          {-- If accept, put it in the queue ----------------}
                          if ThisDCC >= 0 then
                            begin
                              {-- Initialize all the data ------------------}
                              fDccs[ThisDCC].IpAddress := Copy(S, Pos('@', S) + 1, Pred(Pos(#32, S) - Pos('@', S)));
                              fDCCs[ThisDCC].Originate := ParamArray[2];
                              fDCCs[ThisDCC].Active := dcc_WaitPerm;
                              fDCCs[ThisDCC].DCCType := 'SEND';
                              fDCCs[ThisDCC].FileName := ForceBack(CreateTempDir(GlobalCfg^.RaConfig^.AttachPath,
                                                LineCfg^.RaNodeNr)) + ParamArray[3];
                              fDCCs[ThisDCC].PortNr := FVal(ParamArray[5]);
                              fDCCs[ThisDCC].FileSize := FVal(ParamArray[6]);
                              fDCCs[ThisDCC].StartTime := NowAsUnixDate;
                              fDCCs[ThisDCC].DccClient := TTcpClient.Create;
                            end; { if }
                        end; { if }
                    end; { DCC }
                end; { if is ^A }
            end; { if }

           if Command = 'NICK' then
            begin
              ParamArray[01] := Copy(S, 1, Pos('!', S) - 1);
              ParamArray[02] := ExtractWord(S, 2, defExtractWord, false, false);

              Result := Format('*** %s is now known as %s', [
                                ParamArray[01], CommentMsg]);

              for ChanIdx := 01 to MaxChans do
                begin
                  if SearchNick(ChanIdx, ParamArray[01]) >= 0 then
                   if fChannels[ChanIdx].fNickList <> nil then
                     begin
                       DeleteNickFromList(fChannels[ChanIdx].fChanName, ParamArray[01]);
                       fChannels[ChanIDX].fNickList^.Add(CommentMsg);
                     end; { if }
                end; { for }
            end; { if }

           if Command = 'TOPIC' then
            begin
              ParamArray[01] := Copy(S, 1, Pos('!', S) - 1);

              Result := Format(ChanPreFix + '*** %s changes topic to ''%s''', [
                               ParamArray[02],
                               ParamArray[01], CommentMsg]);
            end; { if }
         end; { else }

  RemoveColumns(s);

  {$IFDEF WITH_DEBUG}
    DebugLog('.', 'MakeUpMessage ( end )');
  {$ENDIF}
end; { func. MakeUpMessage }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcClient.CheckError(TestSet: Array of Word; var LastErr: Longint; NeedResp: Boolean): Boolean;
var Counter       : Longint;
    FoundResponse : Longint;
    TempStr       : AnsiString;
    Value         : Longint;
begin
  if NeedResp then
    if NOT DataAvailable then
      begin
        Counter := 0;

        While (Counter < 300) AND (NOT DataAvailable) do
          begin
            DoSleep(10);
            Inc(Counter);
          end; { while }
      end; { if }

  RecvStrLn(TempStr, true);
  Result := false;

  FoundResponse := FVal(ExtractWord(TempStr, 2, defExtractWord, false, false));

  for Counter := 01 to High(TestSet) do
   begin
     Value := TestSet[Counter];

      if FoundResponse = Value then
       begin
         LastErr := Value;
         Result := true;
         EXIT;
       end; { CheckReturn }

  end; { for }
end; { func. CheckReturn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


end. { irccli }
