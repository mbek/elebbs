unit Global;
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
** Global variable and constants declarations for EleBBS
**
** Copyright (c) 1996 - 2000 by Maarten Bekers
**
** Created : 08-Sep-1996
** Last update : 09-Sep-2000
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

(*
**
** Some general structure information:
**
** Exitinfo^.Userinfo.MsgArea always contains the AREANUM variable.
** Exitinfo^.Userinfo.FileArea always contains the AREANUM variable.
** The dates are stored in MM-DD-YY (12-31-80) value.
**
*)
{$IFDEF WITH_FULL}
{$IFDEF MSDOS}
Uses Overlay, Use32, CfgRec, StrUnit, ScrnU;
{$ENDIF}

{$IFDEF WIN32}
 Uses SysUtils, CfgRec, StrUnit{$IFNDEF DELPHI}, ScrnU{$ENDIF};
{$ENDIF}

{$IFDEF OS2}
uses CfgRec, StrUnit, ScrnU;
{$ENDIF}

{$IFDEF GO32V2}
uses Use32, CfgRec, StrUnit, ScrnU;
{$ENDIF}

{$IFDEF ELEUNIX}
uses Use32, SysUtils, CfgRec, strUnit, Dos, ScrnU;
{$ENDIF}

{$ENDIF}


{$IFNDEF WITH_FULL}
uses {$IFDEF ELEUNIX} SysUtils, {$ENDIF}
     {$IFNDEF MSDOS}
      {$IFNDEF DELPHI}
        ScrnU,
       {$ENDIF}
     {$ENDIF} CfgRec, StrUnit;
{$ENDIF}

Const
  VersionMajor   = 0;
  VersionMinor   = 10;

  ProductCodeHi  = 0;
  ProductCodeLo  = 0;

  WebName        = 'EleWEB';
  

{$IFDEF FPC}
   {$IFDEF GO32V2}
      FullProgName   = 'EleBBS/386';
      PidName        : String = 'EleBBS/386 v0.11.b1';
      EleIrcName     = 'EleIRC/386 v0.11.b1';
      VersionID      : String = '0.11.b1';
      PlatformIDStr  = '386';
   {$ENDIF}

   {$IFDEF ELEUNIX}
      {$IFDEF FreeBSD}
        FullProgName   = 'EleBBS/FreeBSD';
        PidName        : String = 'EleBBS/FreeBSD v0.11.b1';
        EleIrcName     = 'EleIRC/FreeBSD v0.11.b1';
        VersionID      : String = '0.11.b1';
        PlatformIDStr  = 'FreeBSD';
      {$ENDIF}

      {$IFDEF LINUX}
       {$IFNDEF FreeBSD}
        FullProgName   = 'EleBBS/Linux';
        PidName        : String = 'EleBBS/Linux v0.11.b1';
        EleIrcName     = 'EleIRC/Linux v0.11.b1';
        VersionID      : String = '0.11.b1';
        PlatformIDStr  = 'Linux';
       {$ENDIF}
      {$ENDIF}
   {$ENDIF}
{$ENDIF}

{$IFDEF OS2}
      FullProgName   = 'EleBBS/OS2';
      PidName        : String = 'EleBBS/OS2 v0.11.b1';
      EleIrcName     = 'EleIRC/OS2 v0.11.b1';
      VersionID      : String = '0.11.b1';
      PlatformIDStr  = 'OS2';
{$ENDIF}

{$IFDEF MSDOS}
 {$IFNDEF FPK}
      FullProgName   = 'EleBBS/DOS';
      PidName        : String = 'EleBBS/DOS v0.11.b1';
      EleIrcName     = 'EleIRC/DOS v0.11.b1';
      VersionID      : String = '0.11.b1';
      PlatformIDStr  = 'DOS';
 {$ENDIF}
{$ENDIF}

{$IFDEF WIN32}
  {$IFDEF WINGUI}
      FullProgName   = 'EleBBS/GUI';
      PidName        : String = 'EleBBS/GUI v0.11.b1';
      EleIrcName     = 'EleIRC/GUI v0.11.b1';
      VersionID      : String = '0.11.b1';                     { Current version }
      PlatformIDStr  = 'GUI';
  {$ELSE}
      FullProgName   = 'EleBBS/W32';
      EleIrcName     = 'EleIRC/W32 v0.11.b1';
      PidName        : String = 'EleBBS/W32 v0.11.b1';
      VersionID      : String = '0.11.b1';                     { Current version }
      PlatformIDStr  = 'W32';
  {$ENDIF}
{$ENDIF}
      ConfigName     = 'ELCONFIG';
      VersIDWord     = $250;
      EleVersIDword  = $010;
      SysCapabilities= 'ZMO';
      SysEMSINotice  = 'Copyright 1996-2003 Maarten Bekers, All Rights reserved';
      EleBBS_ProdID  = 253;    { Product-ID (old xCommport field in RAConfig) }
    {$IFDEF ELEUNIX}
      SystemMsgPrefix = '*' + #32;
    {$ELSE}
      SystemMsgPrefix = #254 + #32;
    {$ENDIF}


{$IFDEF MSDOS}
  Type ShortString = String;
{$ENDIF}

const FileRetries = 06;                              { Try 6 times till abort }

Type MacroArray = Array[0..(1024 * 2) - 01] of Char;

Type IEMSI_SERV_Record = Record
        SystemID    : String;
        SystemName  : String;
        Location    : String;
        SysOp       : String;
        LocalTime   : String;
        Notice      : String;
        WaitChar    : String;
        Capabilities: String;
      end; { IEMSI_Record }

Type IEMSI_USER_Record = Record
        Name        : String;
        Alias       : String;
        Location    : String;
        DataNr      : String;
        VoiceNr     : String;
        PassWord    : String;
        BirthDate   : String;
        CrtDef      : String;
        Protocols   : String;
        Capabilities: String;
        Requests    : String;
        Software    : String;
        XlatTable   : String;
      end; { IEMSI_Record }

Const
  Black         = 0;
  Blue          = 1;
  Green         = 2;
  Cyan          = 3;
  Red           = 4;
  Magenta       = 5;
  Brown         = 6;
  LightGray     = 7;

{ Foreground color constants }

  DarkGray      = 8;
  LightBlue     = 9;
  LightGreen    = 10;
  LightCyan     = 11;
  LightRed      = 12;
  LightMagenta  = 13;
  Yellow        = 14;
  White         = 15;

{ Add-in for blinking }

  Blink         = 128;

type  y2k_YearType     = (y2k_UserDate, y2k_FileDate, y2k_MsgDate,
                          y2k_SysDate);

const y2k_UserFrame    = 15;
      y2k_FileFrame    = 80;
      y2k_MessageFrame = 80;
      y2k_SysFrame     = 80;

      {$IFDEF MSDOS}
        MaxMsgLines      = 500;
        MaxReplyLines    = 120;
      {$ELSE}
        MaxMsgLines      = 6000;
        MaxReplyLines    = 3000;
      {$ENDIF}

Const UserBaseName     = 'users.bbs';
      UserBaseXiName   = 'usersxi.bbs';
      UserBaseEleName  = 'usersele.bbs';
      UserBaseIdxName  = 'usersidx.bbs';
      UserBaseLastRead = 'lastread.bbs';

type MenuRec=Array[1..100] Of MNURecord;
     LBarRec=Array[1..100] of LightBarRecord;

Const D0 =    1461;      { Constants for gregorian and julian date conversion }
      D1 =  146097;
      D2 = 1721119;

      MaxNestMenus  = 49;
      UnlimitedValue= 32767;

      uonBrowsing   = 00;                              { UserON.BBS constants }
      uonUpDown     = 01;
      uonReading    = 02;
      uonExternal   = 03;
      uonChatting   = 04;
      uonQuestion   = 05;
      uonIRC        = 06;
      uonNewUser    = 07;
      uonUserDefined= 255;

      uonWaitTime   = 3;            { Check each 3 secs. for an internode msg }

      WebNodeBase   = 1000;

Const
  DorStatLn   : Array[8..9,1..2] of String[80] =
          (('',''),
           ('ALT: (C)hat (D)Snoop (H)angup (J)Shell (L)ockout (P)rinter (E)dit, (N)exton',
            '     (S)ecurity (O)verride Paging  PgUp-Inc / PgDn-Dec Time'+
            '  (F1)-(F7)=Extra Stats'));

Const MaxBaudRates = 20;
      BaudRates : Array[1..maxBaudRates] of Word=(300, 1200, 2400, 4800, 7200,
                                                  9600, 12000, 14400, 16800,
                                                  19200, 21600, 24000, 26400,
                                                  28800, 31200, 33600, 38400,
                                                  57600, 64000, 11520);

Const NonUserNameChars : CharSet = ['<', '>', '[', ']'];
      MacroPadSet      : CharSet = ['.',                { use systems default }
                                    ',',                        { Align right }
                                    '?',                         { Align left }
                                    '`'];                      { Align center }

Type ProcessCharTypeRec=(chrDisplay, chrRaSystemCodes, chrRaUserCodes, chrRADU,
                         chrAvatar, chrExecuteCodes, chrDisplayText, chrUserHook,
                         chrExecQA);

const
  MailBoxCheckMaxFound = 201;                      { Max. new mail to find }

const
  ChatSysOpColor = Yellow;
  ChatUserColor  = Cyan;

type
  MailBoxCheckArrayType = array[0..MailBoxCheckMaxFound] of record
                                                  BoardNr : Word;
                                                  MsgNr   : LongInt;
                                                  FromWho : String[24];
                                                  ToWho   : String[23];
                                                  Subject : String[22];
                                                  Date    : String[10];
                                                  Time    : String[5];
                                                end; { record }


Type CloseDownHook = procedure;

Const ProtocolFileName     : String = 'protocol.ra';
      LanguageFileName     : String = 'language.ra';
      LimitsFileName       : String = 'limits.ra';
      EventsFileName       : String = 'events.ra';
      FGroupsFileName      : String = 'fgroups.ra';
      MGroupsFileName      : String = 'mgroups.ra';
      FilesFileName        : String = 'files.ra';
      AddressFileName      : String = 'akas.bbs';
      ModemFileName        : String = 'modem.ra';
      TelnetFileName       : String = 'telnet.ele';
      {$IFNDEF WIN32}
        PageFileName         : String = 'page.ra';
      {$ELSE}
        PageFileName         : String = 'page.wav';
      {$ENDIF}
      PageStatFileName     : String = 'pagestat.ra';
      MessagesFileName     : String = 'messages.ra';
      TagListFileName      : String = 'taglist.ra';
      LastCallFileName     : String = 'lastcall.bbs';
      SysInfoFileName      : String = 'sysinfo.bbs';
      TimeLogFileName      : String = 'timelog.bbs';
      LogFileName          : String = 'elebbs.log';
      ArticlesFileName     : String = 'nwsartic.ele';
      ArticlesOutFile      : String = 'outartic.ele';
      EmailIn_Filename     : String = 'email_in.ele';
      EmailOut_FileName    : String = 'emailout.ele';
      EleNewsSemaphore     : String = 'runnews.lck';
      EleMailSemaphore     : String = 'runmail.lck';
      EleUserSemaphore     : String = 'runuser.lck';
      EleFileSemaphore     : String = 'runfile.lck';
      EleFMgrSemaphore     : String = 'runfmgr.lck';
      MessageEleFileName   : String = 'messages.ele';
      EleFilesFileName     : String = 'files.ele';
      NewsServerFileName   : String = 'nwserver.ele';

Const OverrideSysPath: String = '';


Const PagingHours = 1;           { Constants for RaPageStat and RaSetPageStat }
      PagingOff   = 2;
      PagingOn    = 3;

Const RunningBBS    : Boolean = TRUE;    { Running the EleBBS or sub-program? }
      RunningWEB    : Boolean = FALSE;      { Running EleWEB or EleWEB module }
      MaxNodes      : Byte = 255;   { Maximum number of nodes EleBBS supports }
      MailerCmdLine : String = '';          { Commandline for shell-to-mailer }
      CmdEventSet   : Boolean = false;            { Event set at command-line }
      DefaultMenu   : String = 'top';                  { Default menu to load }
      CheckTransfKey: Boolean = true;        { Check for keys during transfer }
      DefExitCode   : Byte = 00;                           { Default exitcode }
      ProgTerminated: Boolean = false;                   { Terminated program }
      ShowDbgWindow : Boolean = false;                    { Show debug-window }
      PwdAccepted   : Boolean = false;        { Is keyboard password accepted }
      MailerEnd     : Boolean = false;    { Is system ended because of mailer }
      ovrUseXMS     : Boolean = True;              { Use XMS for Overlay file }
      ovrUseEMS     : Boolean = True;              { Use EMS for Overlay file }
      ovrExtraMemory: Boolean = True;      { Add extra memory for the overlay }
      RunsRegistered: Boolean = false;              { Runs in registered mode }
      RegAddStrFull : String[30] = '';
      RegAddStrShort: String[30] = '';
      MaxNestedText = 10;                       { Maximum of nested textfiles }
      InExitProcedure: Boolean = False;
      OpenComDelay  : Longint = 00;        { Wait ms's before opening comport }
      RunExitProc   : Boolean = false;      { Has exit procedure already run? }
      RunHangUpProc : Boolean = false;
      TelnetServ    : Boolean = false;           { Running as a telnet server }
      ClearOnExit   : Boolean = true;              { Clear the screen on exit }
      CloseDown     : CloseDownHook = nil;
      ExtraOvrSize  : Longint = 1024 * 50;        { Extra overlay buffer size }
      DefStatusLine : Byte = 01;                         { Default statusline }
      OldProgramDir : String = '';    { Original directory before changing it }
      NextEventMins : Longint = -1;              { Number of minutes till next event }
      EventRecNr    : Longint = 00;               { Rec-Nr in event-file for active event }
      EventStarted  : Boolean = False;            { Event has already started }

Type TransferModeType = (Transmit, Receive);

Type MailReturnType  = (msgAgain, msgNext, msgLast, msgReply, msgEnter, msgStop,
                        msgDelete, msgFiles, msgRdReply, msgForward, msgEndOfArea,
                        msgEdit);
     MessageShowtype = (rdNormal, rdQuickScan, rdScan);
     FileShowType    = (fsNormal, fsKeyWord, fsNewFiles, fsFileName);
     MsgSelectType   = (selNormal, selFrom, selTo, selSubj, selKeyWord, selMarked);


type FDB_ResultCodeType = (fdb_OK, fdb_NoHDR, fdb_NoTXT, fdb_NoIDX, fdb_NoLock, fdb_NoOutput, fdb_NoInput, fdb_Toolarge,
                           fdb_NoTempPath, fdb_NoMemory, fdb_noParam);

Type DeleteFileOpt = (del_Normal, del_Missing);

Type DeleteFileFuncFunc = Function(Name: DelStr; CurPos: Word): Boolean;
     UserHookString     = String[255];
     UserHookFunc       = Function(C: Char; var RetStr: UserHookString): Boolean;

     UploadsRec = Array[1..100] of record
                                {$IFDEF MSDOS}
                                      FName: String[12];
                                {$ELSE}
                                      FName: String;
                                {$ENDIF}
                                      Size : longint;
                                      CPS  : longint;
                                   end; { upload s }

{     FilesTextRecord = Array[1..2048] of Char; }
     MarkedMsgRec  = Array[1..100] of Record
                                        Boardnr: Word;
                                        RecNr: Word;
                                      end; { marked }

     TagHeaderList = Array[1..100] of Record
                                       {$IFDEF MSDOS}
                                        Name: String[12];
                                       {$ELSE}
                                        Name: String;
                                       {$ENDIF}
                                        AreaNum: Word;
                                        PassWord: String[15];
                                        RecordNum: Word;
                                        Attrib   : Byte;
                                        AreaAttrib: Byte;
                                      end; { TagHeaderList }


type UserExtensionRecord = Array[0..1023] of Char;

{-- Some global variables -------------------------------------------------}
var
    {$IFNDEF MSDOS}
     {$IFNDEF DELPHI}
      OldScreen        : ^ScreenRec;
     {$ENDIF}
    {$ENDIF}
    crtOutput         : Text;

{$IFDEF WITH_DEBUG}
 {$IFDEF WITH_FULL}
  {$IFDEF OVERLAY}
    OldReadBuf        : OvrReadFunc;
  {$ENDIF - OVERLAY}
 {$ENDIF - WITH_FULL}
{$ENDIF - WITH_DEBUG}

type GlobalCfgRec = record
        RaConfig     : ^ConfigRecord;                { CONFIG.RA contents }
        ElConfig     : ^EleConfigRecord;               { CONFIG.ELE contents }
     end; { GlobalCfgRec }


type LineCfgRec = record
       Exitinfo         : ^Exitinforecord;          { The Exitinfo.BBS contents }
       UserExtension    : ^UserExtensionRecord;    { users.ele extension record }
       UserExtensionSize: Longint;               { Size of the extension record }
       SaveGuestRecord  : ^UsersRecord;                           { Usersrecord }
       Language         : ^LanguageRecord;     { Contains users language record }
       UploadInfo       : ^UploadsRec;
       TagPaths         : ^StringArrayObj;                   { String container }
       Modem            : ^ModemRecord;                     { MODEM.RA contents }
       Telnet           : ^TelnetRecord;                  { TELNET.ELE contents }
       LimitsInfo       : ^LimitsRecord;                   { LIMITS.RA contents }
       NewsServer       : ^NewsServerRecord;             { NWSSERV.ELE contents }
       MenuContents     : ^MenuRec;                     { Containing 50 records }
       LBarContents     : ^LBarRec;
       HeaderList       : ^TagHeaderList;   { Array100 cont. last 100 file-hdrs }
       Command_Counter  : Byte;                                 { Current param }
       Command_Array    : Array[0..230] of Char;   { Command currently building }
       LocalOnly        : Boolean;                               { Is LocalOnly }
       MarkedMsgArray   : ^MarkedMsgRec;          { Messages marked for reading }
       UsersKBlimit     : Longint;             { Daily user's download kb limit }
       AnsiOn           : Boolean;                      { Has user ANSI enabled }
       AvatarOn         : Boolean;                    { Has user AVATAR enabled }
       RipOn            : Boolean;                       { Has user RIP enabled }
       ProcessCharType  : ProcessCharTypeRec;   { Type of char curr. processing }
       ProcessingLowLevel: Boolean;         { Not currently in lowlevel routines }
       LockReturnResult : Byte;    { Code to return when "LockResult" is called }
       CurFilesRecord   : ^FilesRecord;          { Current selected filesrecord }
       CurMessageRecord : ^MessageRecord;      { Current selected messagerecord }
       CurEleMsgRecord  : ^EleMessageRecord;
       CurMsgNum        : Word;      { Currently selected (showable) msg number }
       CurFileNum       : Word;     { Currently selected (showable) file number }
       TransferMode     : TransferModeType;         { Curr: Sending / Receiving }
       EMSI_User        : ^IEMSI_User_Record;

       RaNodeNr         : Longint;                          { Current node number }
       OnLineTime       : Word;                     { Minutes connected THIS call }
       CarrierCheck     : Boolean;                             { Checking carrier }
       TimeFrozen       : Boolean;                     { Timeleft has been frozen }
       Snooping         : Boolean;                                     { Snooping }
       PauseChecking    : Boolean;                       { Check for 'p' keys etc }
       StartTimingDay   : Word;
       ProcessingSysOpKeys: Boolean;         { Curerntly processing a SysOpKey }
       SysOpKey         : Boolean;         { Last key read in READKEY was from SysOp }
       DispAbortSet     : CharSet;                         { Keys to abort a view }
       DispMorePrompt   : Boolean;                           { Moreprompt active? }
       ContrCodes       : Boolean;                        { Control codes active? }
       RaduCodes        : Boolean;                           { RADU codes active? }
       RalError         : Boolean;                     { Language def. file error }
       ReLogonBBS       : Boolean;                      { Relogon the user at BBS }
       ReLogMenu        : Boolean;                     { Re-use the old menu file }
       LoggedOn         : Boolean;             { Has user passed log-on procedure }
       PrinterLogging   : Boolean;               { Default is printer logging off }
       TextFileShells   : Boolean;                      { TextFile Shells active? }
       ShowKludges      : Boolean;                      { Don't show kludge-lines }
       CurMsgsMarked    : Byte;             { Currently number of messages marked }
       LocalLogon       : Boolean;
       UserAgeSave      : Byte;                             { The age of the user }
       ConnectStr       : String;                                { Connect String }
       TelnetFromIp     : String;                                { telnet from ip }
       EventDeducted    : Word;        { Number of mins deducted because of event }
       NowChatting      : Boolean;                                { Now Chatting? }
       DispAbortChar    : Char;                        { Key that aborted display }
       Global_Tagging   : Boolean;
       ShowStatusBar    : Boolean;
       ShowStatusScrn   : Boolean;        { Show status screen during up/download }
       GuestUser        : Boolean;              { Is user a guest on this system? }
       CacheDesc        : Boolean;                           { Caching is allowed }
       ForcedExit       : Boolean;                      { Forced exit (raxit.??)? }
       TotalUploads     : Byte;
       BatchAtExit      : String;               { Batchfile to execute at the end }
       InheritedHandle  : Longint;     { Use inherited handle from other process }
       DoFlushScrnBuffer: Boolean;                   { flush the screenbuffer? }
       ComNoClose       : Boolean;          { Don't close the port upon exitting? }
       DosShellMsgTyp   : Longint;          { Show message "Type "EXIT" to return }
       CheckInActivity  : Boolean;                   { Check inactivity timeout? }
       Search_Skip      : Boolean;               { Skip current area to search in }
       IsSearching      : Boolean;                     { The actual screen length }
       DoMonitor        : Boolean;                  { Allow monitoring of screens }
       ActScrnLen       : Byte;      { Actual screen length, depending on statusbar }
     end; { LineCfgRec }

var GlobalCfg  : ^GlobalCfgRec;
    LineCfg    : ^LineCfgRec;
    OldExitProc: Pointer;                { Save current exit procedure }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit Global }
