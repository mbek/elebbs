program EleBBS;
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
{$I apdefine.inc}
{$I COMPILER.INC}

{ Note: In Win32-native CompSys.Initvariables should NOT be called! }
{ Note2: You cannot overlay ApPort because it generates an Runtime Error 204 }
{ Note3: Changed ApFossil so that it supports all baudrates }
{ Note4: EleBBS uses the FIXED version of XMS support (on TPower CD) }
{ Note5: Changed header-delay for faster response times (ooZmodem) :) }
{ Note6: APFOSSIL is changed to support newer baudrates }
{ Note7: Changed APFOSSIL to support PutBlock }
{ Note8: Changed OOCOM.??? to support PutBlock, PutString and make sure }
{        that not written strings are written again! }
{ Note9: Modified PutBlockTimeOut to assure that strings are written }
{ Note10: Changed AsyncPro all "INFINITE" into 10 seconds for all "WaitFOrSingleObjects" }
{ Note11: When due to insufficient file-handles it was impossible to open the }
{         messagebase, the Hudson base wasn't closed after one opening failed }
{         and thus it could leave several files open. Add "CloseMsgBase" to }
{         MkOpen and remove "If MsgRec.opened" from MKMSGHUD.PAS }
{ Note12: OOCOM.PA1 function PutBlockTimeOut added instead of Pr^.Buffered }
{         send to TRUE }
{ Note13: OOZMODEM.PA2 checkt de IOResult voor <> 2 en 0, maar niet voor 110 }
{         wat ook nodig is omdat VP/2 110 terug kan geven }
{ Note14: In AWWIN32.PAS is de security descriptor aangepast }
{ Note15: MKMSGJAM could leak filehandles as well }
{ Note16: Adjust MKMSGJAM (SeekNext and SeekPrior), to check the correct }
{         flag (IsDeleted) instead of assuming that CRC=-1 is deleted }
{ Note17: Allocating memory before the OvrSetBuf / OvrSetRetry functions }
{         are called can result in memory corruption }
{ Note18: MAKE SURE YOU SET INHERITEDHANDLE to true in SysUtils.pas }

{$IFDEF USINGMGR}
  Even bovenstaande boolean uitzetten voor EleBBS - aan voor EleMGR
{$ENDIF}


{$IFDEF ISCGI}
   Please disable this switch - only used for EleWEB
{$ENDIF}

{$IFDEF WINGUI}
uses
  Forms,
  Global,
  Support,
  Main,
  FileSys,
  ComUnit,
  JDates,
  SysUtils,
  Debug_U,
  Dialogs,
  CompSys,
  Avatar,
  StUtils,
  Control,
  StrPath,
  BbsKey,
  InOut_U,
  TagUnit,
  TagObj,
  GenFile,
  ReadCfg,
  Cfgfile,
  FastScrn,
  Ral,
  Startup,
  MemMan,
  ScrnU,
  LongStr,
  FileRout,
  EleMenu,
  IoRes_Un,
  CmdLine,
  ApTimer,
  Mailer,
  WinTitle,
  Input_U,
  Use32,
  StatusB,
  Curtime,
  CfgRec,
  HelpUnit,
  WordStr,
  ExecFunc,
  REmScrn,
  Wfc,
  Windows,
  ExitProg,
  GenDos,
  OpenMdm,
  ElLog_U,
  MultiLn,
  ApMisc,
  ApPort,
  OoCom,
  ApFossil,
  Win_main in 'WIN_MAIN.PAS' {Form1},
  EditMain in 'EDITMAIN.PAS' {Useredit},
  Editpref in 'EDITPREF.PAS' {Preferences},
  EditPsw in 'EDITPSW.PAS' {Password},
  EditUsag in 'EDITUSAG.PAS' {Usage},
  Security in 'SECURITY.PAS' {UserLevel},
  SysSet in 'SysSet.pas' {SystemSetForm},
  MusicFormUnit in 'MusicFormUnit.pas' {MusicForm},
  waitingforcaller_u in 'waitingforcaller_u.pas' {WaitingForCallerForm},
  ErrorFormUnit in 'ErrorFormUnit.pas' {ErrorForm},
  helpscreenunit in 'helpscreenunit.pas' {HelpScreenForm};

{$R *.RES}

{$ELSE}
  Uses
    Question,

{$IFDEF VirtualPascal}
    SysUtils,
{$ENDIF}
{$IFDEF MSDOS}
    Extend,
{$ENDIF}
    Dos,
    IoRes_Un,
    Crt,
    Global,
    Support,
    Scrlist,
    Main,
    EleMenu,
    RemScrn,
    Chat,
    FileSys,
    InOut_U,
    Ral,
    FastScrn,

    OOCom,
    ApPort,
    ApMisc,
    ApFossil,
    OoAbsPcl,
    OoAscii,
    OoKermit,
    OoXModem,
    OOZModem,
    OoYModem,
    ApTimer,
    {$IFNDEF OS2}
      Multi,
    {$ENDIF}
    Debug_U,
    TagObj,
    Input_U,
    JDates,
    TagUnit,
    LoadOvr,

    OpenMdm,
    ComUnit,
    HelpUnit,
    Mail,
    MenuFunc,
    CmdLine,
    MkFile,
    MkMsgAbs,
    StrPath,
    MkMsgHud,
    MkMsgJam,
    WriteMsg,
    MkOpen,
    Mailer,
    WinTitle,

    IBM_ARC,
    IBM_ARJ,
    IBM_DWC,
    IBM_HYP,
    IBM_LHA,
    IBM_MDCD,
    IBM_PKZ,
    IBM_RAR,
    IBM_SQZ,
    IBM_ZOO,

    BSC,
    CompSys,


{$IFDEF MSDOS}
    Exec,
    CheckPat,
{$ENDIF}
    Stutils,
    Terminal,
    Cfgfile,
    GenFile,
    Mnuser,
    ExitProg,
    UnixDate,
    LongStr,
    CfgRec,
    MultiLn,
    WordStr,
    Curtime,
    SysVars,

    Elx_Blck,
    Elx_Int,
    Elx_Glob,
    Elx_BBS,

    {$IFDEF ELEUNIX}
    SigBBS,
    {$ENDIF}

    ElLog_U,
    Wfc,
    ObjDec,
    DispAns,
    MemMan,
    FileRout,
    StrUnit,
    Startup,
    BbsKey,
    Control,
    ScrnU,
    Statusb,
    Avatar
  {$IFDEF OVERLAY}
    ,Overlay
    ,OvrXMS
  {$ENDIF}
  ;
{$ENDIF}

{$I EXITPROC.SUP}

{$IFDEF OVERLAY}
  {$O SUPPORT}   {$O CHAT}      {$O FILESYS}  {$O MAIL}     {$O MAIN}
  {$O ELEMENU}   {$O MENUFUNC}  {$O MkFile}
  {$O MkMsgAbs}  {$O RIPMISC}  {$O AUTODET}
  {$O MkMsgHud}  {$O MkMsgJam}  {$O WFC}      {$O MkOpen}
  {$O IBM_ARC}   {$O IBM_ARJ}   {$O IBM_DWC}  {$O IBM_HYP}  {$O IBM_LHA}
  {$O IBM_MDCD}  {$O IBM_PKZ}   {$O IBM_RAR}  {$O IBM_SQZ}  {$O IBM_ZOO}
  {$O BSC}       {$O COMPSYS}   {$O GLOBAL}   {$O UNIXDATE}
  {$O OOCom}     {$O OoAscii}   {$O OoKermit} {$O OoXModem}
  {$O OOZModem}  {$O OoYModem}  {$O TERMINAL} {$O QUESTION} {$O EMSI}
  {$O INOUT_U}   {$O EDITUSER}  {$O SORT_UN}  {$O TRANSFER}
  {$O AVATAR}    {$O STRINGS}   {$O STUTILS}  {$O MULTILN}  {$O BITWISE}
  {$O JDATES}    {$O ACCESS_U}  {$O IORES_UN} {$O FILEROUT}
  {$O SOUND_U}   {$O CRC_UNIT}  {$O ELFILE_U}
  {$O INTEDIT}   {$O LOGON}     {$O FLAGS}    {$O NODELST}  {$O STATUSB}
  {!!$O FASTSCRN}  {$O AREASEL}   {$O ELLOG_U}  {$O LINEED}   {$O DISPANS}
  {$O SYSKEY}    {$O MAILINT}   {$O BBSKEY}
  {$O COLORS}    {$O WRITEMSG}  {$O READMSG}  {$O RA2FDB}   {$O DESC_U}
  {$O LISTFILE}  {$O RAL}       {$O CASES}    {$O STRPATH}  {$O LONGSTR}
  {$O CFGREC}    {$O COMUNIT}   {$O CENTRSTR} {$O WORDSTR}  {$O MEMMAN}
  {$O LIMIT_U}   {$O EXITPROG}  {$O TAGUNIT}  {$O TAGOBJ}   {$O VIEWFILE}
  {$O GENFILE}   {$O CFGFILE}   {$O READCFG}  {$O USRCODES} {$O SYSCODES}
  {$O OUTBLOCK}  {$O INPUT_U}   {$O AREASW}   {$O RANGES}   {$O WINTITLE}
  {$O MAILER}    {$O USER_U}    {$O ELX_BBS}  {$O ELX_GLOB} {$O ELX_INT}
  {$O ELX_BLCK}  {$O MNUSER}    {$O STARTUP}  {$O OPENMDM}  {$O HELPUNIT}
  {$O REMSCRN}   {$O REMSUP}    {$O CHARUNIT} {$O FSED}
  {$O SCRLIST}   {$O QACFG}     {$O RECDIF}   {$O CURTIME}
  {$O QASYS}     {$O FORMLST}   {$O FLAGSTR}  {$O CHECKBIT}

  { Adding DBGMENU to the overlay-list, will crash the XMS handler }
(****
    Exec,
    CheckPat,

    ApUart,
    Dpmi,
    ApPort,
****)

{$ENDIF}


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure InitPointers;
begin
  GlobalCfg := nil;
  LineCfg := nil;

  AvtObj := nil;
  termObj := nil;
  ContrObj := nil;
  WfcObj := nil;
  ReadMsgObj := nil;
  OutputObj := nil;
  OutblockObj := nil;
  MultiLnObj := nil;
  UserEditObj := nil;
  ChatObj := nil;

  {$IFDEF TCPIP}
    IrcIntObj := nil;
    TelIntObj := nil;
  {$ENDIF}

  {$IFDEF ELEUNUX}
    SigObj := nil;
  {$ENDIF}
  ComObj := nil;
  ComPtr := nil;
  LangObj := nil;
  TaggingObj := nil;

  RemScrnObj := nil;
  {$IFNDEF MSDOS}
    OldScreen := nil;
  {$ENDIF}
end; { proc. Initpointers }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function SUpCase (S : String) : String;                { UpCase for strings }
var Teller: Byte;
begin
 For Teller:=01 to Length(S) do
  S[Teller] := System.Upcase(S[Teller]);
 SUpcase := S;
end; { func. SUpcase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure StartDebugHooks;
var TempStr : String;
    Counter : Longint;
begin
  for Counter := 01 to ParamCount do
    begin
      TempStr := SupCase(ParamStr(Counter));

      {$IFDEF MSDOS}
        if Pos('/XFORCEINT28', TempStr) > 00 then MultiTasker := NoTasker;
        if Pos('/XNOSLICER', TempStr) >00 then MultiTasker := NoSlicing;
      {$ENDIF}
      if Pos('/XDEBUGLOG', TempStr) > 00 then DebugObj.DebugLogging := true;
      if Pos('/XNOCACHE', TempStr) > 00 then LineCfg^.CacheDesc := false;
      if Pos('/XLOGUSER', TempStr) > 00 then begin
                                               {$IFDEF WITH_DEBUG}
                                                 DebugObj.DebugLogging := true;
                                                 DebugObj.LoggingEnabled := [logUserbase];
                                               {$ENDIF}
                                             end; { if }
      if Pos('/XLOGCOM', TempStr) > 00 then begin
                                              {$IFDEF WITH_DEBUG}
                                                DebugObj.DebugLogging := true;
                                                DebugObj.LoggingEnabled := [logAsync];
                                              {$ENDIF}
                                            end; { if }
    end; { for }
end; { proc. StartDebugHooks }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

var Dummy       : PChar;
  {$IFDEF VirtualPascal}
    LineNo      : Longint;
    Temp        : ShortString;
  {$ENDIF}

begin
  {$IFNDEF WINGUI}
    AssignCrt(CrtOutput);
    System.Rewrite(Crtoutput);
  {$ELSE}
    Move(Output, Crtoutput, SizeOf(Crtoutput)); {  Save this for later use }
  {$ENDIF}

  RunningBBS := true;   { Set flag that we are running the BBS not a utility }
  CheckBreak := false;                          { Do not check for CTRL-C''s }
  ProgTerminated := false;                      { The program is still alive }

  InitPointers;                               { Intialize all pointes to nil }
  StartDebugHooks;               { Setup some debugging hooks and parameters }

  {$IFDEF WITH_DEBUG}
    PidName := PidName + '.d';
    VersionID := VersionID + '.d';

    DebugObj.DebugLog(logString, '');
    DebugObj.DebugLog(logString, 'Entering EleBBS v'+FullProgName+ #32 + 'v'+VersionID);
  {$ENDIF}

  {----------------- Initialize platform specific information ---------------}
  ParseCommandLine(false);   { Parse the commandline for needed information }

  {$IFNDEF WINGUI}
    LoadOverlay;                                 { Load the overlayed units }
  {$ENDIF}

  {-- initialize the memory so we can initialize the memory soon ------------}
  New(GlobalCfg);               { Initialize memory for BBS global variables }
  New(LineCfg);                     { intialize session specific global vars }
  InitBasicGlobalVars;
  InitBasicLineVars(LineCfg^);


  {-- Because HangUp can be called from early on (by *nix signal handler) ---}
  {-- we have to make sure that HangUp doesnt use any of them without -------}
  {-- validity checking -----------------------------------------------------}
  LineCfg^.LoggedOn := FALSE;

  {-- now install the Signal handler ----------------------------------------}
  {$IFDEF ELEUNIX}
    New(SigObj, Init);
    SigObj^.InstallSignalHandler;
  {$ENDIF}

   SetupScrnU;                                     { Initialize screen unit }

  {$IFNDEF WINGUI}
    New(ContrObj, Init);
    New(ReadMsgObj, Init);
    New(WfcObj, Init);
    New(MultiLnObj, Init);
    New(OutblockObj, Init);
    New(TermObj, Init);
    New(AvtObj, Init);
    New(LangObj, Init);
    New(LineCfg^.TagPaths, Init(100));
    New(OutputObj, Init);
    New(InputObj, Init);
    New(TaggingObj, Init);
    New(UserEditObj, Init);
    New(ChatObj, Init);


    {$IFDEF TCPIP}
      New(IrcIntObj, Init);
      New(TelIntObj, Init);
    {$ENDIF}

    {$IFDEF OVERLAY}
      CompSys.InitVariables;                  { Initialize compressor toolkit }
    {$ENDIF}
  {$ELSE}
    Application.Initialize;
    Application.Title := 'EleBBS';

    Application.CreateForm(TForm1, Form1);
    Application.CreateForm(TUseredit, Useredit);
    Application.CreateForm(TPreferences, Preferences);
    Application.CreateForm(TPassword, Password);
    Application.CreateForm(TUsage, Usage);
    Application.CreateForm(TUserLevel, UserLevel);
    Application.CreateForm(TSystemSetForm, SystemSetForm);
    Application.CreateForm(TMusicForm, MusicForm);
    Application.CreateForm(TWaitingForCallerForm, WaitingForCallerForm);
    Application.CreateForm(TErrorForm, ErrorForm);
    Application.CreateForm(THelpScreenForm, HelpScreenForm);
    Application.OnException := Form1.NewHandleException;
  {$ENDIF}

  {---------------- Make sure that the carrier checker is enabled -----------}
  OldExitProc := ExitProc;                          { Setup an error-handler }
  ExitProc := @NewExitProc;

  {$IFDEF OVERLAY}
    IoRes_Un.InitErrorHandler;                { Setup another error-handler }
  {$ENDIF}

  contrObj^.InCheckAll := TRUE;                    { Set the timer inactive }
  LineCfg^.DispMorePrompt := false;
  contrObj^.SetTimeOut;                                { Setup idle checker }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'Error handler initted');
  {$ENDIF}

  {-------------- Initialize screen and other ifnromation -------------------}
  Main.Initvariables;
  Main.InitScreen;
  StartDebugHooks;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'Debughooks initialized');
  {$ENDIF}


  StartOpening;                             { Shows opening screen (local) }
  ParseCommandLine(true);        { Parse the command-line information again }
  CheckRegistered;                           { Check the key for validation }



  {---------------------- Check for the remotetakeover check ----------------}
  {$IFNDEF MSDOS}
    if LineCfg^.DoMonitor then
      begin
        New(RemScrnObj, Init);
        RemScrnObj^.CreatePipe(LineCfg^.RaNodeNr);
      end; { if }
  {$ENDIF}

  {-- Make sure Unix wont logon locally -------------------------------------}
  {$IFDEF ELEUNIX}
    LineCfg^.Modem^.Comport := 1;
    LineCfg^.LocalLogon := FALSE;
  {$ENDIF}

  {------------------- Perhaps it's an local logon --------------------------}
  if (LineCfg^.LocalLogon) OR (LineCfg^.Modem^.Comport = 00) then
    begin
      LineCfg^.LocalLogon := True;
      LineCfg^.Exitinfo^.Baud := 00;
      LineCfg^.CarrierCheck := False;
    end; { Baud }


  {------------------ Make sure we use a valid node number ------------------}
  if LineCfg^.RaNodeNr < 01 then LineCfg^.RaNodeNr := 01;
  {if GenInfo^.MaxNodes > 00 then
    if RunsRegistered then MaxNodes := Byte(GenInfo^.MaxNodes);}

  if (LineCfg^.RaNodeNr > MaxNodes) then
    begin
      RaLog('!', 'This version supports a maximum of '+FStr(MaxNodes)+' nodes!');
      LocalScreenLn(SystemMsgPrefix + 'This version supports a maximum of ' + FStr(MaxNodes) + ' nodes.');
      Halt(0);
    end; { Maximum number of nodes }


  {------------------------- Startup EleBBS -----------------------------------}
  {$IFNDEF WINGUI}
    StartupBBS;
  {$ENDIF}

  {$IFDEF WINGUI}
    Form1.Tag := 00;
    Form1.Timer1.Enabled := true;
    if NOT Application.Terminated then
        Application.Run;
  {$ENDIF WinGUI}



  {$IFNDEF WINGUI}
    Start_This_Program;
  {$ENDIF WINGUI}

  Dummy := #13#10 + #13#10 + #13#10;
  Dummy := #13#10 + 'EleBBS v0.10  (c)1998-2003 by Maarten Bekers, All rights reserved.';
  Dummy := #13#10 + 'Reverse engineering or disassembling of this software is illegal!';
  Dummy := #13#10 + 'You shouldn''t be reading this ;)';
  Dummy := #13#10 + #13#10 + #13#10;

  Hangup;
end.
