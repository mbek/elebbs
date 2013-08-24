program EleMAIL;
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
** EleMAIL, POP3 (RFC 1725) grabber into message and
** SMTP (RFC 821) mail sending program.
**
** Copyright (c) 1996 - 2000 by Maarten Bekers
**
** Created : 23-Aug-1999
** Last update : 25-Apr-2000
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

     {$IFDEF WITH_DEBUG}
       Debug_U,
     {$ENDIF}

     Dos, Crt, LongStr, SockDef, SockFunc, Pop3Cli,
     Global, FileObj, CfgRec, FileRout, MemMan,
     WinTitle, ReadCFG, BbsKey, ElLog_u, CentrStr,
     Bitwise, MimeDec, StrUnit, Cases, JDates,
     Mail, MgrFdb, GenFile, StrPath, Articles,
     User_U, RAL, MkMsgAbs, MkOpen,
     SmtpCli, StUtils, WordStr, ObjDec, SysVars;


Type  OptionsType    = (otGetMessages, otSendMessages, otTossMessages,
                        otScanMessages);
Const DefaultOptions : Set of OptionsType = [];

var Options        : Set of OptionsType;
    Pop3Client     : tPop3Client;
    SmtpClient     : tSmtpClient;
    SaveExitProc   : Pointer;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

const MsgAreaNr     : Longint = 0;                          { Area to post to }
      ScanAreaNr    : Longint = 0;                        { Area to scan from }
      FwBounceName  : String = 'Sysop';
      Pop3UserName  : String = '';
      Pop3Password  : String = '';
      Pop3HostName  : String = '';
      SmtpHostName  : String = '';
      SmtpUserName  : String = '';    { User and pwd to SMTP-auth with (opt.) }
      SmtpPassword  : String = '';

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Const Pop3Port    : Longint = 110;
      SmtpPort    : Longint = 25;
      MaxSent     : Longint = 20;

      TrashBounces: Boolean = false;
      SmtpReplyTo : Boolean = false;
      DeleteSema  : Boolean = false;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ClosePop3Connection;
begin

  if Pop3Client <> nil then
   try
     Pop3Client.Quit;
     Pop3Client.Free;
   except
   end;

  Pop3Client := nil;
end; { proc. ClosePop3Connection }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CloseSmtpConnection;
begin

  if SmtpClient <> nil then
    try
      SmtpClient.Quit;
      SmtpClient.Free;
    except
    end;

  SmtpClient := nil;
end; { proc. CloseSmtpConnection }

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

procedure CloseDownExitProc;
begin
  ExitProc := SaveExitProc;

  CloseSmtpConnection;
  ClosePop3Connection;
  if DeleteSema then RemoveSema(EleMailSemaphore);
end; { proc. CloseDownExitProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ParseCommandLine;
var Invalid : Boolean;
    InvOpt  : String;
    Counter : Byte;
    Param   : String;
    TempCH  : Char;
begin
  {-- Initialize some default values ----------------------------------------}
  FwBounceName := GlobalCfg^.RaConfig^.Sysop;

  {-- Start the parameter parsing -------------------------------------------}
  Invalid := False;
  InvOpt := '';

  If ParamCount > 00 then
   For Counter := 01 to ParamCount do
    begin
       Param := ParamStr(Counter);
       TempCH := UpCase(Param[2]);

       If (NOT (Param[1] in ['/', '-'])) OR
           ((Length(ParamStr(Counter)) > 2) AND NOT (TempCH in ['A', 'H', 'C', 'B', 'X', 'D', 'I', 'O', 'E', 'U', 'F', 'W', 'Y'])) then 
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
          'R' : Options := Options + [otGetMessages];
          'T' : Options := Options + [otTossMessages];
          'S' : Options := Options + [otScanMessages];
          'P' : Options := Options + [otSendMessages];
          'E' : begin
                  GlobEmailHostname := Copy(ParamStr(Counter), 3, 255);
                end; { Email hostname }
          'F' : begin
                  TrashBounces := true;
                end; { trash bounces }
          'D' : begin
                  DisclaimName := Copy(ParamStr(Counter), 3, 255);
                end; { if }
          'A' : begin
                  MsgAreaNr := FVal(Copy(ParamStr(Counter), 3, 255));
                end; { MsgAreaNr }
          'C' : begin
                  ScanAreaNr := FVal(Copy(ParamStr(Counter), 3, 255));
                end; { ScanAreaNr }
          'H' : begin
                  Pop3Hostname := Copy(ParamStr(Counter), 3, 255);

                  if Pos(':', Pop3HostName)>0 then
                    begin
                      Pop3port := FVal(Copy(Pop3HostName, Pos(':', Pop3HostName) + 1, 255));
                      Pop3HostName := Copy(Pop3HostName, 1, Pos(':', Pop3HostName) - 1);
                    end; { if }
                end; { hostname }
          'B' : begin
                  FwBounceName := Under2Norm(Copy(ParamStr(Counter), 3, 255));
                end; { BounceName }
          'U' : begin
                  Pop3Username := Copy(ParamStr(Counter), 3, 255);

                  Pop3Password := Copy(Pop3Username, Pos('@', Pop3Username) + 1, 255);
                  Pop3Username := Copy(Pop3Username, 1, Pos('@', Pop3Username) - 1);
                end; { POP3 User/Pass }
          'W' : begin
                  SmtpUserName := Copy(ParamStr(Counter), 3, 255);

                  SmtpPassword := Copy(SmtpUserName, Pos('@', SmtpUserName) + 1, 255);
                  SmtpUserName := Copy(SmtpUserName, 1, Pos('@', SmtpUsername) - 1);
                end; { SMTP User/Pass }
          'Y' : begin
                  SmtpReplyTo := true;
                end; { Reply-To: }
                
          'I' : begin
                  SmtpHostname := Copy(ParamStr(Counter), 3, 255);

                  if Pos(':', SmtpHostName)>0 then
                    begin
                      Smtpport := FVal(Copy(SmtpHostName, Pos(':', SmtpHostName) + 1, 255));
                      SmtpHostName := Copy(SmtpHostName, 1, Pos(':', SmtpHostName) - 1);
                    end; { if }
                end; { hostname }
            else begin
                   Invalid := True;
                   InvOpt := ParamStr(Counter);
                 end; { Invalid option }
       end; { case }

   end; { for Counter }

  if (otGetMessages in Options) then
    if Pop3Hostname = '' then Invalid := true;
  if (otSendMessages in Options) then
    if SmtpHostname = '' then Invalid := true;
  if (otGetMessages in Options) then
    if MsgAreaNr = 0 then Invalid := true;
  if (otScanMessages in Options) then
    if ScanAreaNr = 0 then Invalid := true;


  if Invalid then
    begin
      if InvOpt <> '?' then
        begin
          Writeln(#32, SystemMsgPrefix, 'Invalid option : ', InvOpt);
        end; { if }

      WriteLn(#32, SystemMsgPrefix, 'Command-line parameters:');
      WriteLn;
      WriteLn(Dup(#32, 09), '-R               Collect new email from the server');
      WriteLn(Dup(#32, 09), '-T               Toss messages into messagebase');
      WriteLn(Dup(#32, 09), '-S               Scan messagebase for new messages');
      WriteLn(Dup(#32, 09), '-P               Send messages to mail server');
      WriteLn(Dup(#32, 09), '-F               Trash all bounces instead of fwd. Sysop');
      WriteLn(Dup(#32, 09), '-Y               Set Reply-To: field at the sent messages');
      WriteLn(Dup(#32, 09), '-H<name>[:port]  Specify hostname of POP3 server');
      WriteLn(Dup(#32, 09), '-I<name>[:port]  Specify hostname of SMTP server');
      WriteLn(Dup(#32, 09), '-D[filename]     Filename specifies a textfile that will be added to');
      WriteLn(Dup(#32, 09), '                 any posted message');
      WriteLn(Dup(#32, 09), '-E<domainname>   Domainname for email address, FTN is used when empty');
      WriteLn(Dup(#32, 09), '-A<areanum>      Specify an areanumber where to toss new mail in');
      WriteLn(Dup(#32, 09), '-C<areanum>      Specify an areanumber where to scan new mail from');
      WriteLn(Dup(#32, 09), '-B<bouncename>   Specify an username to where all bounced e-mail is');
      WriteLn(Dup(#32, 09), '                 sent to');
      WriteLn(Dup(#32, 09), '-U<name>@<pword> Username and password to use for the POP3 connection');
      WriteLn(Dup(#32, 09), '-W<name>@<pword> Username and password to use for the SMTP connection');
      WriteLn(Dup(#32, 09), '                 If used, the SMTP server may set the From: field');
      WriteLn(Dup(#32, 09), '                 with this Username. -Y recommended it that cases.');
      WriteLn;

      if (Pop3Hostname = '') OR (SmtpHostName = '') then
       if (otSendMessages in Options) OR (otGetMessages in Options) then
           WriteLn(#32, SystemMsgPrefix, 'You need to specify a server for this action!');

      if (otGetMessages in Options) then
       if MsgAreaNr = 0 then
         WriteLn(#32, SystemMsgPrefix, 'You need to specify an areanumber when retrieving messages!');

      if (otScanMessages in Options) then
       if ScanAreaNr = 0 then
         WriteLn(#32, SystemMsgPrefix, 'You need to specify an areanumber when scanning for messages!');

      WriteLn(#32, SystemMsgPrefix, 'Please refer to the documentation for a more complete command summary');
      WriteLn;

      MemMan.ReleaseMem(GlobalCfg^.RaConfig, SizeOf(ConfigRecord));
      MemMan.ReleaseMem(LineCfg^.Exitinfo, SizeOf(ExitinfoRecord));
      Halt(255);
    end; { Invalid }

  Writeln(#32, SystemMsgPrefix, 'Active options:');

  If otScanMessages in Options then WriteLn('   - Scanning messagebase for new mail')
      else WriteLn('   - Not scanning messagebase');

  if otSendMessages in Options then WriteLn('   - Posting articles to mail server')
      else WriteLn('   - Not posting articles to mail server');

  If otGetMessages in Options then WriteLn('   - Collecting messages from ', Pop3Hostname)
      else WriteLn('   - Not collecting messages');

  If otTossMessages in Options then WriteLn('   - Tossing messages into messagebase')
      else WriteLn('   - Not tossing messages');

  Writeln;
end; { proc. ParseCommandLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowHeader;
begin
  TextAttr := LightGray;

  WriteLn;
  WriteLn('ELEMAIL; EleBBS POP3-mail retrieval utility, Version '+VersionID);
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
end; { proc. ShowHeader }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TryAliasses(const Username: String;
                     var   MsgText : MessageTextType): Boolean;
var AliasF    : pFileObj;
    TempStr   : String;
    ToName    : String;
    AliasName : String;
    TmpAreaStr: String;
    CmdName   : String;
    TmpAreaNr : Longint;
begin
  TryAliasses := FALSE;             { Fall back to "bouncing" it to the Sysop }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'TryAliasses(Username = "'+Username+'")');
  {$ENDIF}

  {-- Open up the alias file --------------------------------------------------}
  New(AliasF, Init);
  AliasF^.Assign(OpenRaFile('mailfwd.ctl'));
  if NOT AliasF^.Open(1) then
    begin
      {$IFDEF WITH_DEBUG}
         DebugObj.DebugLog(logString, 'TryAliasses(unable to find mailfwd.ctl)');
      {$ENDIF}

      Dispose(AliasF, Done);
      EXIT;
    end; { if }

  {-- Loop through it ---------------------------------------------------------}
  While NOT AliasF^.EOF do
    begin
      AliasF^.ReadLn(TempStr);                              { Read the string }

      if TempStr[1] <> ';' then                              { Not a comment? }
        begin
          CmdName := ExtractWord(TempStr, 1, defExtractWord, TRUE, false);
          ToName := ExtractWord(TempStr, 2, defExtractWord, TRUE, false);
          AliasName := ExtractWord(TempStr, 3, defExtractWord, TRUE, false);
          TmpAreaStr := ExtractWord(TempStr, 4, defExtractWord, TRUE, false);
          if Fval(TmpAreaStr) <> 0 then
            TmpAreaNr := FVal(TmpAreaStr)
              else TmpAreaNr := MsgAreaNr;

          {$IFDEF WITH_DEBUG}
            DebugObj.DebugLog(logString, 'TryAliasses(Cmdname = "'+Cmdname+'")');
          {$ENDIF}

          if SupCase(CmdName) = 'FWD' then
            begin
              {$IFDEF WITH_DEBUG}
                DebugObj.DebugLog(logString, 'TryAliasses(ToName = "'+ToName+'" / "'+ALiasName+'" / "' + TmpAreaStr + '")');
              {$ENDIF}

              if SUpCase(Trim(ToName)) = SUpCase(Trim(Username)) then
                begin
                  AddMsgToBase(EmailIn_FileName,        { Filename to post in }
                               AliasName,                       { To the user }
                               TmpAreaNr,             { The areanr to post in }
                               999,                      { Articlenr, ignored }
                               MsgText,                        { Message text }
                               true);                      { Is email message }

                  TryAliasses := TRUE;
                end; { if }
            end; { if }
        end; { if }
    end; { while }

  {-- Close it ----------------------------------------------------------------}
  Dispose(AliasF, Done);
end; { func. TryAliasses }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TryAlternateField(var MsgText: MessageTextType): Boolean;
var AliasF    : pFileObj;
    TempStr   : String;
    CmdName   : String;
    FieldName : String;
    Username  : String;
    MailAddr  : String;
    DoAbort   : Boolean;
begin
  {-- some some info ----------------------------------------------------------}
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'TryAlternateField - begin');
  {$ENDIF}


  {-- initialize some variables -----------------------------------------------}
  TryAlternateField := FALSE;       { Fall back to "bouncing" it to the Sysop }
  DoAbort := FALSE;

  {-- now extract the fieldname -----------------------------------------------}
  Fieldname := 'To:';
  UserName := ExtractToName(MsgText, true, FieldName);
  MailAddr := ExtractToName(MsgText, false, FieldName);


  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'TryAlternateField(Username = "'+Username+'")');
  {$ENDIF}

  {-- Open up the alias file --------------------------------------------------}
  New(AliasF, Init);
  AliasF^.Assign(OpenRaFile('mailfwd.ctl'));
  if NOT AliasF^.Open(1) then
    begin
      {$IFDEF WITH_DEBUG}
         DebugObj.DebugLog(logString, 'TryAlternateField(unable to find mailfwd.ctl)');
      {$ENDIF}

      Dispose(AliasF, Done);
      EXIT;
    end; { if }

  {-- Loop through it ---------------------------------------------------------}
  While (NOT AliasF^.EOF) AND (NOT DoAbort) do
    begin
      AliasF^.ReadLn(TempStr);                              { Read the string }

      if TempStr[1] <> ';' then                              { Not a comment? }
        begin
          CmdName := ExtractWord(TempStr, 1, defExtractWord, TRUE, false);
          FieldName := ExtractWord(TempStr, 2, defExtractWord, TRUE, false);

          {$IFDEF WITH_DEBUG}
            DebugObj.DebugLog(logString, 'TryAlternateField(Cmdname = "'+Cmdname+'")');
          {$ENDIF}

          if SupCase(CmdName) = 'ALTFIELD' then
            begin
              {$IFDEF WITH_DEBUG}
                DebugObj.DebugLog(logString, 'TryAlternateField(FieldName = "'+FieldName+'")');
              {$ENDIF}

              {-- match the name with the fields ---------------------------}
              if (SearchUser(UserName) >= 0) then
                begin
                  DoAbort := TRUE;
                 end; { if }

              if (SearchUser(MailAddr) >= 0) then
                begin
                  DoAbort := TRUE;
                end; { if }
            end; { if }
        end; { if }
    end; { while }

  if DoAbort then
    begin
      {$IFDEF WITH_DEBUG}
         DebugObj.DebugLog(logString, 'TryAlternateField - doAbort = TRUE');
      {$ENDIF}

      AddMsgToBase(EmailIn_FileName,
                   Username,
                   MsgAreaNr,
                   01,
                   MsgText,
                   true);
      TryAlternateField := TRUE;
    end; { if }

  {-- Close it ----------------------------------------------------------------}
  Dispose(AliasF, Done);

  {-- some some info ----------------------------------------------------------}
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'TryAlternateField - end');
  {$ENDIF}
end; { func. TryAlternateField }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TryLeaveOnServer(var MsgText : MessageTextType): Boolean;
var AliasF    : pFileObj;
    TempStr   : String;
    ToName    : String;
    Usernames : Array[0..1] of String;
    CmdName   : String;
begin
  TryLeaveOnServer := FALSE;        { Fall back to "bouncing" it to the Sysop }

  {-- Get the two names it can bounce on --------------------------------------}
  UserNames[0] := ExtractToName(MsgText, false, 'To:');
  UserNames[1] := ExtractToName(MsgText, false, 'To:');

  {-- Open up the alias file --------------------------------------------------}
  New(AliasF, Init);
  AliasF^.Assign(OpenRaFile('mailfwd.ctl'));
  if NOT AliasF^.Open(1) then
    begin
      Dispose(AliasF, Done);
      EXIT;
    end; { if }

  {-- Loop through it ---------------------------------------------------------}
  While NOT AliasF^.EOF do
    begin
      AliasF^.ReadLn(TempStr);                              { Read the string }

      if TempStr[1] <> ';' then                              { Not a comment? }
        begin
          CmdName := ExtractWord(TempStr, 1, defExtractWord, TRUE, false);
          ToName := ExtractWord(TempStr, 2, defExtractWord, TRUE, false);
          { AliasName := ExtractWord(TempStr, 3, defExtractWord, TRUE, false); }

          if SupCase(CmdName) = 'LEAVE' then
            begin
              if SUpCase(Trim(ToName)) = SUpCase(Trim(Usernames[0])) then
                begin
                  TryLeaveOnServer := TRUE;
                end; { if }

              if SUpCase(Trim(ToName)) = SUpCase(Trim(Usernames[1])) then
                begin
                  TryLeaveOnServer := TRUE;
                end; { if }
            end; { if }
        end; { if }
    end; { while }

  {-- Close it ----------------------------------------------------------------}
  Dispose(AliasF, Done);
end; { func. TryLeaveOnServer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CollectNewMail;
var Counter   : Longint;
    NrMsgs    : Longint;
    NrOctets  : Longint;
    MsgText   : ^MessageTextType;
    StrLines  : pStringArrayObj;
    Username  : String;
    SaveName  : String;
    DoPost    : Boolean;
    DontDelete: Boolean;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'CollectNewMail - begin');
  {$ENDIF}

  if NOT AllocMem(MsgText, SizeOf(MessageTextType), 'MessageTextType', 'CollectNewMail') then
    FatalError('Not enough memory to setup buffer', -1);

  Pop3Client.GetStat(NrMsgs, NrOctets);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'CollectNewMail - Stat - NrMsgs/Octents: ' + FStr(NrMsgs) + ' / ' + FStr(NrOctets));
  {$ENDIF}

  Write(#32, SystemMsgPrefix);
  Write('Collecting mail for ''', Pop3client.Username, ''' (', NrMsgs, ' messages)');

  if NrMsgs > 0 then
    begin
      DontDelete := FALSE;    { This is a "leave on server" message when true }

      WriteLn;
      Write(#32, SystemMsgPrefix, 'Retrieving message #');

      for Counter := 01 to NrMsgs do
        begin
          {$IFDEF WITH_DEBUG}
            DebugObj.DebugLog(logTcpIp, 'CollectNewMail - Retr - Counter = ' + FStr(Counter) + ' - begin');
          {$ENDIF}

          if Pop3Client.Retr(Counter, MsgText^) then
            begin
              Write(Counter, Dup(#08, Length(FStr(Counter))));

              {-- Check the email address (Maarten.Bekers@localhost) --------}
              {---------------------------------- returns: "Maarten Bekers" -}
              UserName := ExtractToName(MsgText^, true, 'To:');
              SaveName := Username;
              StrLines := NIL;
              DontDelete := TryLeaveOnServer(MsgText^);

              {$IFDEF WITH_DEBUG}
                DebugObj.DebugLog(logTcpIp, 'CollectNewMail - Retr - (01) - Username = ' + UserName);
              {$ENDIF}

              if NOT DontDelete then
                if (UserName = '') OR (SearchUser(UserName) < 0) then
                  begin
                    {-- Check the to: "Maarten Bekers" <blah@localhost> -------}
                    UserName := ExtractToName(MsgText^, false, 'To:');

                    {-- log some info -----------------------------------------}
                    {$IFDEF WITH_DEBUG}
                      DebugObj.DebugLog(logTcpIp, 'CollectNewMail - Retr - (01.5) - Username = ' + UserName);
                      DebugObj.DebugLog(logTcpIp, 'CollectNewMail - Retr - (01.5) - Savename = ' + SaveName);
                    {$ENDIF}

                    {-- check if we can find an alias -------------------------}
                    if (UserName = '') OR (SearchUser(UserName) < 0) then
                      begin
                        if (NOT TryAliasses(Savename, MsgText^)) AND
                            (NOT TryAliasses(Username, MsgText^)) then
                          begin
                            if NOT TryAlterNateField(MsgText^) then
                              begin
                                if TrashBounces then
                                  begin
                                    {-- Bounce text -----------------------------------}
                                    New(StrLines, Init(50));
                                    if Username = '' then
                                      Username := SaveName;

                                    StrLines^.Add('** EleMAIL v' + VersionID + '.');
                                    StrLines^.Add('** Message forwarded at '+TimeStr(false, false) + ' - ' + LangObj^.RaFormatDate(DateStr, 8, y2k_SysDate, 0));
                                    StrLines^.Add('** Message originally to: ' + Username);
                                    StrLines^.Add('** Forward reason: Unknown username');
                                    StrLines^.Add('');

                                    {-- Actually post the message ---------------------}
                                    UserName := FwBounceName;
                                    AddLineToMessageText(MsgText^,        { Orig. txt }
                                                         StrLines^,
                                                         5);

                                    Dispose(StrLines, Done);
                                  end
                                    else begin
                                           {-- we trash bounces ;) ---------}
                                           UserName := '';
                                         end; { else }
                              end
                                else UserName := ''; { alternate field has taken care of it }
                          end
                            else Username := '';  { Aliass have taken care of it }
                      end; { if }
                  end; { if }


              {-- extrat username -------------------------------------------}
              {$IFDEF WITH_DEBUG}
                DebugObj.DebugLog(logTcpIp, 'CollectNewMail - Retr - (02) - Username = ' + UserName);
              {$ENDIF}

              {-- User is known? Mail the message ---------------------------}
              DoPost := (UserName <> '');                  { Message bounced }
              if DontDelete then      { If its leave on server, dont post it }
                DoPost := FALSE;

              if DoPost then
                AddMsgToBase(EmailIn_FileName, Username, MsgAreaNr, Counter, MsgText^, true);

              {-- Delete the message from the server ------------------------}
              if NOT DontDelete then
                begin
                  {$IFDEF WITH_DEBUG}
                    DebugObj.DebugLog(logTcpIp, 'CollectNewMail - Deleting #' + FStr(Counter));
                  {$ENDIF}

                  Pop3Client.DelE(Counter);
                end
                  else RaLog('>', 'Leaving message #' + FStr(Counter) + ' on server (To: "' + SaveName + '")');
            end; { if }

          {$IFDEF WITH_DEBUG}
            DebugObj.DebugLog(logTcpIp, 'CollectNewMail - Retr - Counter = ' + FStr(Counter) + ' - done');
          {$ENDIF}
        end; { for }

      WriteLn;
    end { if }
      else WriteLn;

  ReleaseMem(MsgText, SizeOf(MessageTextType));

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'CollectNewMail - end');
  {$ENDIF}
end; { proc. CollectNewMail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function PostMailArticle(var Article: NewsArticleRecord; var MsgText: MessageTextType; var SendError: Longint): Boolean;
var TempStr   : String;
    FromStr   : String;
    HeloAnswer: Boolean;
begin
  PostMailArticle := FALSE;
  SendError := 0;

	HeloAnswer := SmtpClient.Helo(Copy(GlobEmailHostname, 1, Length(GlobEmailHostname)), (SmtpUserName <> ''));
  if HeloAnswer then
    begin
      {-- Get the from name and strip the text between quotes ---------------}
      SendError := 1;
      if (SmtpUserName <> '') then
      	if not SmtpClient.Auth(SmtpUserName, SmtpPassword)
      		then EXIT;

      FromStr := GetRawEmail(GetFieldLine('From:', MsgText));
      
      if SmtpClient.Mail(FromStr) then
        begin
           SendError := 2;
           TempStr := GetFieldLine('To:', MsgText);       { Get to: address }
           TempStr := Copy(TempStr, Length('To:') + 1, Length(TempStr));
           TempStr := GetRawEmail(TempStr);

           {$IFDEF WITH_DEBUG}
             DebugObj.DebugLog(logTcpIp, 'GetFieldLine(To:) = ' + GetFieldLine('To:', MsgText));
             DebugObj.DebugLog(logTcpIp, 'GetRawEmail = ' + GetRawEmail(GetFieldLine('To:', MsgText)));
           {$ENDIF}

           if SmtpClient.Rcpt(TempStr) then
             begin
               { Now clear out the FromStr if we should not send an reply to }
               if (NOT SmtpReplyTo) then 
	               FromStr := '';
	               
	             { and actually send the message }
               SendError := 3;
               PostMailArticle := SmtpClient.Data(MsgText, FromStr);
             end; { 'rcpt' }

        end; { 'mail from' }
    end; { 'helo' }
end; { func. PostMailArticle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SendMessages;
var Article_F    : pFileObj;
    Article      : NewsArticleRecord;
    MsgText      : ^MessageTextType;
    CurArticle   : Longint;
    SendError    : Longint;
    Posted       : Longint;

    ArticleHdrPos: Longint;
    SavePos      : Longint;
    TmpAttr      : Byte;
begin
  {-- Initialize all sort of variables --------------------------------------}
  if NOT AllocMem(MsgText, SizeOf(MessageTextType), 'MessageTextType', 'CollectNewArticles') then
    FatalError('Not enough memory to setup buffer', -1);
  Posted := 00;
  CurArticle := 0;

  {-- Log some info ---------------------------------------------------------}
  RaLog('>', 'Posting articles to your mailserver');
  WriteLn(#32, SystemMsgPrefix, 'Posting articles to your mailserver');

  {-- Fire up article base --------------------------------------------------}
  New(Article_F, Init);
  Article_F^.Assign(EmailOut_FileName);
  Article_F^.FileMode := ReadWriteMode + DenyAll;
  if NOT Article_F^.Open(1) then
    begin
      Dispose(Article_F, Done);

      WriteLn('   (no articles to process)');
      RaLog('>', 'Posting completed, no articles processed');
      EXIT;
    end; { if }

  {-- Loop through all mails ------------------------------------------------}
  While NOT Article_F^.EOF do
     begin
       FillChar(MsgText^, SizeOf(MessageTextType), #00);
       Inc(CurArticle);

       {-- Remember which article were processing ---------------------------}
       ArticleHdrPos := Article_F^.FilePos;
       Article_F^.BlkRead(Article, SizeOF(NewsArticleRecord));
       if Article_F^.IOResult > 00 then FatalError('Unable to read article database', -1);

       Article_F^.BlkRead(MsgText^, Article.BodyLen);
       if Article_F^.IoResult > 00 then
         FatalError('Error occured while trying to read article text', -1);

       {-- Add a disclaimer to be safe (if needed) --------------------------}
       AddDisclaimer(MsgText^);

       {-- Actually post the message ----------------------------------------}
       if PostMailArticle(Article, MsgText^, SendError) then
         begin
           TmpAttr := Article.Attribute;
           SetBit(TmpAttr, 0);
           Article.Attribute := TmpAttr;
         end { if }
           else begin
                  WriteLn('   Failed to send article #', CurArticle, ' (code: ', SendError,')');
                  RaLog('!', 'Failed to send (SMTP) message #' + FStr(CurArticle) + ' ("' + SmtpClient.fErrorStr + '")');

                  Inc(Article.TimesSent);
                  Dec(Posted);
                end; { if }

       {-- Update the article header ----------------------------------------}
       SavePos := Article_F^.FilePos;
       Article_F^.Seek(ArticleHdrPos);

       Article_F^.BlkWrite(Article, SizeOf(NewsArticleRecord));
       if Article_F^.IoResult > 00 then
           FatalError('Error occured while trying to update article base', -1);
       Article_F^.Seek(SavePos);

       {-- Update the statistics --------------------------------------------}
       Inc(Posted);
     end; { while }

  {-- Log some info ---------------------------------------------------------}
  RaLog('>', 'Posting completed ('+FStr(Posted)+' articles processed)');
  WriteLn('   (', Posted,' messages processed)');

  {-- Dispose all objects ---------------------------------------------------}
  Dispose(Article_F, Done);
  ReleaseMem(MsgText, SizeOf(MessageTextType));

  {-- Remove all messages that are actually sent ----------------------------}
  PurgeSentBase(EmailOut_FileName, MaxSent);
end; { proc. SendMessages }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ConnectPop3(const pop3Username, pop3Password: String);
var Temp      : Boolean;
    ErrorStr  : String;
begin
  {-- Initialize POP3 client ------------------------------------------------}
  Pop3Client := tPop3client.Create;
  Pop3Client.ShowLine  := {$IFDEF FPC}@{$ENDIF}ShowText;
  Pop3Client.AbortFunc := {$IFDEF FPC}@{$ENDIF}AbortSession;
  Pop3Client.Username  := pop3UserName;
  Pop3Client.Password  := pop3Password;

  {-- Now connecto to the server --------------------------------------------}
  Write(#32, SystemMsgPrefix, 'Connecting to ', Pop3Hostname, ',');
  Temp := Pop3Client.ConnectToServer(Pop3HostName, Pop3Port, ErrorStr);
  if NOT Temp then
    FatalError(ErrorStr, -1);

  {-- Flush the headers -----------------------------------------------------}
  Pop3Client.WaitForData;                             { Wait for the header }
  Pop3Client.FlushData;

  {-- Logon to the POP3 server ----------------------------------------------}
  if NOT Pop3Client.Logon then
    begin
      RaLog('!', 'POP3 server ('+Pop3HostName+'), refused authentication, aborting.');
      Writeln(' failed.');
      WriteLn;
      Halt(250);
    end { if }
      else Writeln(' success!');

  {-- Flush pending data, and start sending/collecting mail -----------------}
  Pop3Client.FlushData;

  {-- Now actually collect the new mail -------------------------------------}
  CollectNewMail;

  {-- Were all through with this --------------------------------------------}
  Writeln(#32, SystemMsgPrefix, 'Closing connection to ', Pop3Hostname);
  Pop3Client.Quit;
  ClosePop3Connection;
end; { proc. ConnectPop3 }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ConnectSmtp;
var Temp      : Boolean;
    ErrorStr  : String;
begin
  {-- Initialize SMTP client ------------------------------------------------}
  SmtpClient := tSmtpclient.Create;
  SmtpClient.ShowLine  := {$IFDEF FPC}@{$ENDIF}ShowText;

  {-- Now connecto to the server --------------------------------------------}
  Write(#32, SystemMsgPrefix, 'Connecting to ', SmtpHostname, ',');
  Temp := SmtpClient.ConnectToServer(SmtpHostName, SmtpPort, ErrorStr);
  if NOT Temp then
    FatalError(ErrorStr, -1);

  {-- Flush the headers -----------------------------------------------------}
  SmtpClient.WaitForData;                             { Wait for the header }
  SmtpClient.FlushData;

  {-- Logon to the SMTP server ----------------------------------------------}
  if NOT SmtpClient.ConnectionAlive then
    begin
      RaLog('!', 'SMTP server ('+SmtpHostName+'), disconnected client, aborting.');
      Writeln(' failed.');
      WriteLn;
      Halt(250);
    end { if }
      else Writeln(' success!');

  {-- Flush pending data, and start sending/collecting mail -----------------}
  SmtpClient.FlushData;

  {-- Send the message ------------------------------------------------------}
  SendMessages;

  {-- Were all through with this --------------------------------------------}
  Writeln(#32, SystemMsgPrefix, 'Closing connection to ', SmtpHostname);
  SmtpClient.Quit;
  CloseSmtpConnection;
end; { proc. ConnectSmtp }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

begin
  SetWindowTitle('EleMAIL');

  Pop3Client := nil;
  SmtpClient := nil;
  SaveExitProc := ExitProc;
  ExitProc := @CloseDownExitproc;

  RunningBBS := False;
  Options := DefaultOptions;
  DoAttach := TRUE;

  New(LangObj, Init);
  ShowHeader;
  ReadConfigRa;
  InitSystemNames;
  CheckRegistered;
  ParseCommandLine;

  RaLog(' ', '');
  RaLog('>', 'EleMAIL; POP3/SMTP mail utility fired up');

  if SemaExist(EleMailSemaphore) then
    begin
      RaLog('!', 'EleMAIL is already running ('+EleMailSemaPhore + ' exists), aborting.');
      WriteLn(#32, SystemMsgPrefix, 'EleMAIL is already running ('+EleMailSemaPhore + ' exists), aborting');
    end { if }
     else begin
            CreateSema(EleMailSemaphore);
            DeleteSema := true;

            if (otScanMessages in Options) then
               begin
                 Write(#32, SystemMsgPrefix);
                 Write('Scanning message area for new articles (', ScanAreaNr, ')');

                 ScanMsgArea(ScanAreaNr, EmailOut_FileName, Internet);

                 WriteLn;
               end; { if }

            if NOT FileExist(EmailOut_FileName) then
              Options := Options - [otSendMessages];

            if (otGetMessages in Options) then
              ConnectPop3(Pop3UserName, Pop3Password);
            if (otSendMessages in Options) then
              ConnectSmtp;

            if otTossMessages in Options then ProcessNewArticles(EmailIn_FileName);
          end; { if }


  Writeln(#32, SystemMsgPrefix, 'Done');
end. { EleMAIL }
