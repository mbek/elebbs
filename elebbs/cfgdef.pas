unit CfgDef;
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
** CFGDEF.TPU, Defaults support unit for ElCONFIG
**
** Copyright (c) 1997 by Maarten Bekers
**
** Created : 14-Apr-1997
** Last update : 14-Apr-1997
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
  This unit should not be compiled in EleBBS
{$ENDIF}

uses Global, CfgRec, StrPath;

Procedure LoadConfigDefaults(var RaConfig: ConfigRecord);
procedure LoadEleConfigdefaults(var ElConfig: EleConfigRecord);
Procedure LoadModemDefaults(var RaModem: ModemRecord);
Procedure LoadEventsDefaults(var Events: EventRecordArray);
Procedure LoadOneEventDefault(var Event: EventRecord);
Procedure LoadTelnetDefaults(var Telnet: TelnetRecord);
Procedure LoadNewsServerDefaults(var NewsServer: NewsServerRecord);

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure LoadOneEventDefault(var Event: EventRecord);
begin
  Event.Status     := 2;
  Event.StartTime  := '00:00';
  Event.ErrorLevel := 00;
  Event.Days       := 00;
  Event.Forced     := False;
  Event.LastTimeRun:= '01-01-80';
end; { proc. LoadOneEventDefault }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure LoadEventsDefaults(var Events: EventRecordArray);
var Counter: Byte;
begin
  FillChar(Events, SizeOf(EventRecordArray), #00);

  For Counter := 01 to 20 do
    LoadOneEventDefault(Events[Counter]);
end; { proc. LoadEventsDefaults }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure LoadConfigDefaults(var RaConfig: ConfigRecord);
var Counter: Byte;
begin
  FillChar(RaConfig, SizeOf(ConfigRecord), #00);

  With RaConfig do
   begin;
      VersionID := VersIDword;          { Hexadecimal version# of BBS-program }
      ProductID := EleBBS_ProdID;                     { Product ID for EleBBS }
      MinimumBaud := 300;                               { Minimum logon speed }
      GraphicsBaud := 300;                                    { ANSI baudrate }
      TransferBaud := 300;                                   { Download Speed }
      SlowBaudTimeStart := '00:00';     { Time when 'slow' logons are allowed }
      SlowBaudTimeEnd := '00:00';       { Time when 'slow' logons are allowed }
      DownloadTimeStart := '00:00';  { Time when 'slow' downloads are allowed }
      DownloadTimeEnd := '23:59';    { Time when 'slow' downloads are allowed }

      For Counter := 00 to 06 do
       begin;
         PageStart[Counter] := '00:00';              { When paging is allowed }
         PageEnd[Counter] := '00:00';
       End; { Paging }

      SeriNum := '';                                     { /Pro serial number }
      CustNum := '';                                  { /Pro customers number }

      PwdExpiry := 00;           { Number of calls before pw-change is forced }

      MenuPath := BsChar + 'ele'+BsChar+'menus' + BsChar;
      TextPath := BsChar + 'ele'+BSChar+'txtfiles' + BsChar;
      AttachPath := '';                           { Path for file-attachments }
      NodeListPath := BsChar + 'ele' + BsChar;                { Nodelist path }
      MsgBasePath := BsChar + 'ele'+ BsChar + 'msgbase' + BsChar;{ Messagebase path }
      SysPath := BsChar + 'ele' + BsChar;                       { System path }
      FileBasePath := BsChar + 'ele' + BsChar + 'filebase' + bschar; { fbase path }

      ExternalEdCmd := '';                          { External message editor }

      FillChar(Address, SizeOf(Address), 00);             { NetWork Addresses }

      SystemName := FullProgName + ' BBS';              { Name of this System }

      NewSecurity := 10;                                  { Newusers security }
      NewCredit := 00;                            { Credit newusers are given }
      FillChar(NewFlags, SizeOf(NewFlags), #00);

      OriginLine := 'Default originline';                { Default originline }
      QuoteString := ' > ';                 { String to use when quoting text }

      SysOp := 'SysOp';                      { System Operator of this System }

      LogFileName := 'elebbs.log';                               { System log }

      FastLogon := False;               { Fast Local Logons? (No name typing) }
      AllowSysRem := True;                            { Remote SysOp allowed? }
      MonoMode := False;                                   { MonoChrome mode? }
      StrictPwdChecking := False;                 { Strict password checking? }
      DirectWrite := True;                            { Direct screen writes? }
      SnowCheck := False;                                { CGA snow checking? }

      CreditFactor := 00; { Extra Time in seconds uses receive for every minute }

      UserTimeOut := 400;      { Seconds before user is inactivity time outed }
      LogonTime := 15;                            { Minutes user has to logon }
      PassWordTries := 3;  { Number of password attempts before disconncected }
      MaxPage := 5;          { Maximal number of pages user may yell per call }
      PageLength := 10;                  { Duration (in seconds) of page bell }

      CheckForMultiLogon := True;                    { Check for multi-logons }
      ExcludeSysOpFromList := True;       { Exclude sysop from userlists etc? }
      OneWordNames := False;                     { Accept one word as a name? }

      CheckMail := yes;                            { Check mail from newlogon }
      AskVoicePhone := True;                          { Ask users' voicephone }
      AskDataPhone := True;                            { Ask users' dataphone }
      DoFullMailCheck := True;                     { Perform a full mailcheck }
      AllowFileShells := False;                    { Allow textfile shellings }
      FixUploadDates := True;                  { Upload dates to current date }
      FreezeChat := True;               { Freeze timeleft timer when chatting }

      Ansi := ask;                      { Send ANSI codes to newuser ? }
      ClearScreen := ask;                     { Perform clear-screens? }
      MorePrompt := ask;                                { PagePausing? }

      UploadMsgs := True;         { Is user allowed to upload a prepared msg? }

      KillSent := yes;                      { Kill netmail after sent? }

      CrashAskSec := 90;                      { Min sec# to ask 'Crash Mail?' }
      CrashSec := 100;                           { Min sec# to fore crashmail }

      FAttachSec := 100;                { Security level needed to fileattach }

      NormFore := 07;                            { Default colors (lightgray) }
      NormBack := 00;

      StatFore := 0;                             { StatusLine (black on gray) }
      StatBack := 7;

      HiFore := 14;                    { Window hilight color (yellow on red) }
      HiBack := 4;

      WindFore := 9;                  { "IN" Window color (lightblue on blue) }
      WindBack := 1;

      ExitLocal := 100;                                 { FrontEnd Errorlevel }
      Exit300 := 101;                                   { FrontEnd Errorlevel }
      Exit1200 := 102;                                  { FrontEnd Errorlevel }
      Exit2400 := 103;                                  { FrontEnd Errorlevel }
      Exit4800 := 104;                                  { FrontEnd Errorlevel }
      Exit9600 := 106;                                  { FrontEnd Errorlevel }
      Exit19k := 110;                                   { FrontEnd Errorlevel }
      Exit38k := 115;                                   { FrontEnd Errorlevel }

      MultiLine := False;                             { Run MultiLine system? }
      MinPwdLen := 4;                        { Min chars length for passwords }

      MinUpSpace := 500;   { Minimal space (k's) needed before upload allowed }

      Hotkeys := yes;                            { Hotkeys for newuser }

      BorderFore := 9;         { Border (of window) color (lightblue on blue) }
      BorderBack := 1;

      BarFore := 15;             { Bar (in MsgRead) to display (white on red) }
      BarBack := 4;

      LogStyle := 00;                { Log Style: 00 = Expanded, 01 = Compact }
      MultiTasker := 00;                                        { Auto-Detect }

      PwdBoard := 00;                                         { WatchDog Area }

      WhyPage := True;                                            { Why page? }
      LeaveMsg := 00;                    { Area# to post messages to SysOp in }

      ShowMissingFiles := True;                      { Show the missing files }

      AllowNetMailReplies := False;      { Allow echomail replies via netmail }

      LogonPrompt := 'Please enter your full name: ';

      CheckNewFiles := no;          { Check during logon for newfiles? }

      ReplyHeader := '* In a message originally to @, # said:';

      BlankSecs := 30;             { Secs before screenblanker becomes active }

      For Counter:=1 to 6 do
        ProtocolAttrib[Counter] := 1;                     { All are available }
                                                      { Explanation: 1=Always }
                                                               { 2=Error free }
                                                            { 3=Not available }


(** ??   RenumThreshold := ??;  ?? **)                 { Renumber threshold?? }

      LeftBracket := '(';
      RightBracket := ')';

      AskForHandle := False;                      { Ask new users for handle? }
      AskForBirthDate := False;                { Ask newusers for birthdate ? }

      GroupMailSec := 100; { Minimum security to use the 'groupmail' function }

      ConfirmMsgDeletes := False;            { Confirm before deleting a msg? }

      TempScanDir := '';                            { ReArchive/Viri-Scan dir }
      ScanNow := no;                      { Direct scan? (online scan) }

      FailedScanAction := 00;                             { None options 'on' }
      FailedScanArea := 00;               { Area#, to move 'refused' files to }
      ScanCmd := '';                                          { Virus scanner }

      NewUserGroup := 00;                      { New user's default usergroup }
      Avatar := ask;                       { New users' AVATAR setting }
      BadPwdArea := 00;               { Area# for bad passwords leave comment }

      Location := '';                                     { System's location }

      DoAfterAction := 02;                           { After a system message }

      CRFore := 13; { 'Press (RETURN) to continue:' prompt (magenta on black) }
      CRBack := 00;

      LangHdr := 'Available languages: ';

(** ??     ListPath := ''; ?? **)

      FullMsgView := ask;        { Newuser's fullscreen message viewer }
      EMSI_Enable := yes;                               { Enable IEMSI }
      EMSI_NewUser := True;                          { New User's IEMSI logon }

      EchoChar := '*';                 { Echo character for Password Entering }

      Exit7200 := 105;                                  { FrontEnd Errorlevel }
      Exit12000 := 107;                                 { FrontEnd Errorlevel }
      Exit14400 := 108;                                 { FrontEnd Errorlevel }

      ChatCommand := '';                           { Default external chatter }

      ExtEd := ask;              { Newuser's fulslcreen message editor }
      NewUserLanguage := 00;                     { Newuser's default language }
      LanguagePrompt := 'Select your prefered language: ';

      VideoMode := Byte(Auto);                   { Number of lines on display }

      AutoDetectANSI := False;  { Auto detection of the ANSI(/RIP) emulation? }
      NewUserDateFormat := 05;                     { DateFormat, (DD-MM-YYYY) }

      KeyBoardPwd := '';               { Security, keyboard password to enter }

      CapLocation := False;                   { Capitalize newuser's location }
      NewUserSub := 00;          { Number of days before subscription expires }

      PrinterName := 'LPT1';                             { Device for printer }

      HiLitePromptFore := 00;                          { Color of input field }
      HiLitePromptBack := 00;

      AltJSwap := False;          { Swap out of memory, when ALT-J is pressed }

      SemPath := '';                                         { Semaphore path }
      AutoChatCapture := False;                  { Auto log all typed in chat }

      NewFileTag := False;   { Are users allowed to tag files during newfiles }
                                                                    { Search? }
      TempCDFilePath := '';                           { Temp CD-ROM copy path }

      TagFore := 00;                 { Colors for the numbers for the taglist }
      TagBack := 00;

      Exit16k := 109;                                   { FrontEnd Errorlevel }
      FilePayBack := 00;  { Nr. of credits when user's uploads are downloaded }

      FileLine := '[0E@NE [0D@SB [0A@FD@NW[03@DF';
      FileMissingLine := '[0E@NE [0D@SB [0A@FD@NW[03@DF';

      NewUserUlCredit := 00;       { Number of uploads to credit newuser with }
      NewUserUlCreditK := 5;       { KBytes of uploads to credit newuser with }

      FillChar(ArcInfo, SizeOf(ArcInfo), #00);

      For Counter := 01 to 05 do
          RaMgrAltFKeys[Counter] := '';

      ArcViewCmd := '';                             { External archive viewer }

      ExitFax := 99;              { Errorlevel to exit, when FAX connect made }

      UseXMS := False;                 { Use XMS when swapping out of memory? }
      UseEMS := True;                  { Use EMS when swapping out of memory? }

      CheckDOB := 00;                         { Nr. of Calls before DOB check }

      EchoCheck := yes;      { Perform a mailbox scan when logging on? }

      ccSec := 65535;                              { CarbonCopy Send Security }
      ReturnRecSec := 65535;  { Sec. level required to receive return receipt }
      HonourNetReq := False;               { Net Return Receipts are allowed? }

      For Counter:=1 to 200 do
         DefaultCombined[Counter] := 00;

      AskForSex := True;                                { Ask for user's sex? }
      AskForAddress := False;               { Ask for newuser's mail address? }

      DlDesc := ask;         { Download FileDescription with download? }
      NewPhoneScan := True;       { Scan for dupl. phonenumbers with newusers }

      Exit21k := 111;                                   { FrontEnd Errorlevel }
      Exit24k := 112;                                   { FrontEnd Errorlevel }
      Exit26k := 113;                                   { FrontEnd Errorlevel }
      Exit28k := 114;                                   { FrontEnd Errorlevel }
      Exit57k := 116;                                   { FrontEnd Errorlevel }
      Exit64k := 117;                                   { FrontEnd Errorlevel }

      TagLogoffWarning := False;  { Warn when files are tagged before logging off? }
      LimitLocal := False;                               { Limit local keys ? }
      SavePasswords := False;                  { Save Passwords or only CRC ? }
      BlankLogins := 00;           { Blank Logins before user is disconnected }

      RipIconPath := '';                                      { RIP .ICN path }

      Exit31k := 00;                                    { FrontEnd Errorlevel }
      Exit33k := 00;                                    { FrontEnd Errorlevel }

      IncludeNewCdAreas := False;     { Include CD-ROM areas in newfiles scan }
   end; { With }
end; { proc. LoadConfigDefaults }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure LoadEleConfigdefaults(var ElConfig: EleConfigRecord);
begin
  FillChar(ElConfig, SizeOf(EleConfigRecord), #0);

  With Elconfig do
    begin
      VersionID := EleVersIDWord;
      UtilityLogFileName := BsChar +  'ele' + BsChar + 'elebbs.log';
      CapitalizeUsername := true;
      AttachPassword := '';
      webHtmlPath := BsChar + 'ele' + BsChar + 'web' + BsChar + 'html' + BsChar;
      webElmPath := BsChar + 'ele' + BsChar + 'web' + BsChar + 'script' + BsChar;
    end; { with }
end; { proc. LoadEleConfigDefaults }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure LoadModemDefaults(var RaModem: ModemRecord);
begin
  FillChar(RaModem, SizeOf(ModemRecord), #00);

  With RaModem do
    begin;
      ComPort := 1;
      InitTries := 10;
      BufferSize := 128;
      ModemDelay := 50;
      MaxSpeed := 2400;
      SendBreak := False;
      LockModem := False;
      AnswerPhone := True;
      OffHook := True;
      InitStr := 'ATH0S0=0M0|';
      InitStr2 := '';
      BusyStr := 'ATH1|';
      InitResp := 'OK';
      BusyResp := 'OK';
      Connect300 := 'CONNECT|';
      Connect1200 := 'CONNECT 1200';
      Connect2400 := 'CONNECT 2400';
      Connect4800 := 'CONNECT 4800';
      Connect7200 := 'CONNECT 7200';
      Connect9600 := 'CONNECT 9600';
      Connect12k := 'CONNECT 12000';
      Connect14k := 'CONNECT 14400';
      Connect16k := 'CONNECT 16800';
      Connect19k := 'CONNECT 19200';
      Connect38k := 'CONNECT 38400';
      ConnectFax := 'CONNECT FAX';
      RingStr := 'RING';
      AnswerStr := 'ATA|';
      ErrorFreeString := 'ARQ';
      Connect21k := 'CONNECT 21600';
      Connect24k := 'CONNECT 24000';
      Connect26k := 'CONNECT 26400';
      Connect28k := 'CONNECT 28800';
      Connect57k := 'CONNECT 57600';
      Connect64k := 'CONNECT 64000';
      Connect31k := 'CONNECT 31200';
      Connect33k := 'CONNECT 33600';
    end; { With }
end; { proc. LoadModemDefaults }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure LoadTelnetDefaults(var Telnet: TelnetRecord);
begin
  FillChar(Telnet, SizeOf(TelnetRecord), #00);

  With Telnet do
    begin
      MaxSessions := 3;
      ServerPort := 23;
      StartNodeWith := 200;
      ProgramPath := BsChar + 'ele';
      NodeDirectories := BsChar + 'ele' + BsChar + 'node*n' + BsChar;
    end; { with }
end; { proc. LoadTelnetDefaults }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure LoadNewsServerDefaults(var NewsServer: NewsServerRecord);
begin
  FillChar(NewsServer, SizeOf(NewsServerRecord), #00);

  With NewsServer do
    begin
      NewsServer.MaxSessions := 50;
      NewsServer.ServerPort := 119;
      NewsServer.DomainName := 'elebbs.bbs';
    end; { with }

end; { proc. LoadNewsServerDefaults }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
