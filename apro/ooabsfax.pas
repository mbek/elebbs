{$A+,F+,I-,R-,S-,V-}

{$I APDEFINE.INC}

{Include OPro's define file if UseOPro is specified}
{$IFDEF UseOPro}
{$I OPDEFINE.INC}
{$ENDIF}

{******************************************************}
{*                   OOABSFAX.PAS 2.03                *}
{*      Copyright (c) TurboPower Software 1993.       *}
{*                All rights reserved.                *}
{******************************************************}

unit OoAbsFax;
  {-Abstract objects and data for CAS and Class I/II fax}

interface

uses
  Dos,
  Dpmi,
  {$IFDEF UseOPro}
  OpString,
  OpDos,
  OpRoot,
  {$IFDEF Opro12}
  OpConst,
  {$ENDIF}
  {$ENDIF}
  {$IFDEF UseTPro}
  TpMemChk,
  TpString,
  TpDos,
  {$ENDIF}
  ApTimer,
  ApMisc,
  ApPort,
  OoCom,
  OoFaxCvt;

const
  {Constants used to initialize object fields}
  DefConnectAttempts : Word    = 1;       {Default one connect attempt}
  DefMaxRetries : Integer      = 2;       {Max times to retry sending a page}
  DefCmdTimeout : Integer      = 546;     {Ticks to wait for fax cmd response}
  DefDialTimeout : Integer     = 1092;    {Ticks to wait for dial response}
  DefTransTimeout : Integer    = 1092;    {Ticks to wait for outbuf room}
  DefStatusTimeout : Integer   = 18;      {Ticks between status updates}

  {Constants used directly}
  DefInit : String[40]         = 'ATE0Q0V1X4S0=0S2=43'; {Required inits}{!!.02}
  DefStatusBytes : Word        = 10000;   {Force periodic exit}
  MaxBadPercent : Word         = 10;      {Error if this % bad training}
  FlushWait : Word             = 500;     {Msec before/after DTR drop}
  FrameWait : Word             = 40;      {Msec delay before HDLC frame}

  {Undocumented constants}
  AbortDelay : Word            = 1000;    {Msec wait for +++}
  PreCommandDelay : Word       = 100;     {MSec before general modem cmds}
  PreFaxDelay : Word           = 40;      {MSec before inprog fax modem cmds}
  ExtraCommandDelay : Word     = 200;     {MSec extra delay before some cmds}
  PreEOPDelay : Word           = 500;     {MSec delay before sending EOP}
  OkDelay : Word               = 18;      {Tick wait for optional OK}
  InterCharDelay : Word        = 0;       {MSec between modem chars}
  BaudChangeDelay : Word       = 500;     {MSec delay before changing baud} {!!.01}

  {Fax send/receive options}
  afAbortNoConnect      = $0001;   {Abort if no connect}
  afCASWaitTillDone     = $0002;   {Stay in FaxTransmit/Receive till done}
  afExitOnError         = $0004;   {Exit FaxTransmit/Receive on error}
  afCASSubmitUseControl = $0008;   {SubmitSingleFile uses control file}
  afNoSoftwareFlow      = $0010;   {Don't use software flow control in C1/2}

  DefFaxOptions : Word = afCASWaitTillDone;
  BadFaxOptions = 0;

  {Fax progress codes, sending}
  fpInitModem         = 01;  {Initializing modem for fax processing}
  fpDialing           = 02;  {Dialing}
  fpBusyWait          = 03;  {Busy, FaxTransmit is waiting}
  fpSendPage          = 04;  {Sending document page data}
  fpSendPageStatus    = 05;  {Send EOP}
  fpPageError         = 06;  {Error sending page}
  fpPageOK            = 07;  {Page accepted by remote}

  {Fax progress codes, receiving}
  fpWaiting           = 20;  {Waiting for incoming call}
  fpNoConnect         = 21;  {No connect on this call}
  fpAnswer            = 22;  {Answering incoming call}
  fpIncoming          = 23;  {Incoming call validated as fax}
  fpGetPage           = 24;  {Getting page data}
  fpGetPageResult     = 25;  {Getting end-of-page signal}
  fpCheckMorePages    = 26;  {getting end-of-document status}
  fpGetHangup         = 27;  {Get hangup command}
  fpGotHangup         = 28;  {Got Class 2 FHNG code}

  {Fax progress codes, common}
  fpSessionParams     = 40;  {Getting connection params}
  fpGotRemoteID       = 41;  {got called-station ID}

type
  {.Z+}{Private types}
  ClassType = (ctUnknown, ctDetect, ctClass1, ctClass2,                {!!.02}
               ctClass2_0, ctCAS);                                     {!!.02}
  {.Z-}

  {Logging codes}
  TLogFaxCode = (
    lfaxTransmitStart,
    lfaxTransmitOk,
    lfaxTransmitFail,
    lfaxReceiveStart,
    lfaxReceiveOk,
    lfaxReceiveSkip,
    lfaxReceiveFail);

  {General fax states}
  FaxStateType = (
    faxReady,           {State machine ready immediately}
    faxWaiting,         {State machine waiting}
    faxCritical,        {State machine in critical state}
    faxFinished);       {State machine is finished}

type
  {.Z+}{Private types}
  {A list of files/numbers to fax}
  TFaxNumber = String[40];
  PFaxEntry = ^TFaxEntry;
  TFaxEntry = record
    fNumber : TFaxNumber;
    fFName  : PathStr;
    fCover  : PathStr;
    fNext   : PFaxEntry;
  end;
  {.Z-}

  {The abstract fax object}
  AbstractFaxPtr = ^AbstractFax;

  {Various hook types}
  FaxStatusProc = procedure (AP : AbstractFaxPtr;
                             Starting, Ending : Boolean);
  NextFaxFunc = function(AP : AbstractFaxPtr;
                         var Number : String;
                         var FName : PathStr;
                         var Cover : PathStr) : Boolean;
  FaxLogProc = procedure(AP : AbstractFaxPtr;
                         Number : String;
                         FName : PathStr;
                         Log : TLogFaxCode);
  FaxNameFunc = function (AP : AbstractFaxPtr) : PathStr;
  AcceptFaxFunc = function(AP : AbstractFaxPtr;
                           RemoteName : Str20) : Boolean;

  AbstractFax =
    object(Root)
      Sending      : Boolean;       {True if sending faxes}
      SendingCover : Boolean;       {True if sending cover page}
      MaxConnect   : Word;          {max number of connect attempts}
      ConnectCnt   : Word;          {count of connect attempts}
      RetryWait    : Word;          {ticks to wait between connect attempts}
      afFlags      : Word;          {fax send/receive options}
      SaveStatus   : Word;          {Temp var and save between states}
      FaxProgress  : Word;          {For storing progress codes}
      FaxListCount : Word;          {Number of fax entries}
      CurrPage     : Integer;       {counter for pages}
      PageCount    : Integer;       {total pages in document}
      CoverCount   : Integer;       {Number of cover pages, 0 or 1}
      DataCount    : LongInt;       {count of received "real" data bytes}
      PageSize     : LongInt;       {size of page file in bytes}
      FaxListHead  : PFaxEntry;     {Head of fax entry list}
      FaxListTail  : PFaxEntry;     {Tail of fax entry list}
      FaxListNode  : PFaxEntry;     {Current node of fax entry list}
      ClassInUse   : ClassType;     {class of device in use}
      StationID    : Str20;         {Station ID (usually phone #)}
      RemoteID     : Str20;         {StationID of remote}
      DestDir      : PathStr;       {destination directory}
      FaxFileName  : PathStr;       {current document being processed}
      CoverFile    : PathStr;       {cover page file if any}
      PhoneNum     : String[40];    {phone number to dial}
      StatusTimer  : EventTimer;    {Timer for status updates}
      Title        : String;        {Sender title}
      Recipient    : String;        {Recipient's name}
      Sender       : String;        {Sender's name}
      afStatus     : FaxStatusProc; {Proc for status}
      afNextFax    : NextFaxFunc;   {Func for next fax}
      afLogFax     : FaxLogProc;    {Proc for fax logging}
      afFaxName    : FaxNameFunc;   {Func to name incoming fax}
      afAcceptFax  : AcceptFaxFunc; {Func to accept incoming fax}
      SaveMode     : Byte;          {Save FileMode}                    {!!.02}

      {Constructors/destructors}
      constructor Init(ID : Str20);
      destructor Done; virtual;

      {Option management}
      procedure afOptionsOn(OptionFlags : Word);
        {-Activate multiple options}
      procedure afOptionsOff(OptionFlags : Word);
        {-Deactivate multiple options}
      function afOptionsAreOn(OptionFlags : Word) : Boolean;
        {-Return True if all specified options are on}

      {User control}
      procedure SetTitle(NewTitle : String);
        {-Set title of sender (used in header line)}
      procedure SetRecipientName(NewName : String);
        {-Set name of recipient}
      procedure SetSenderName(NewName : String);
        {-Set name of sender}
      procedure SetFaxStatusProc(SP : FaxStatusProc);
        {-Set our status display routine}
      procedure SetDestinationDir(Dest : PathStr);
        {-Set a destination directory for received files}
      procedure SetStationID(NewID : Str20);
        {-Assign our station ID}
      procedure SetConnectAttempts(Attempts : Word; DelayTicks : Word);
        {-Set number of connect attempts per fax, 0 = infinite}
      procedure SetNextFaxFunc(NFF : NextFaxFunc);
        {-Set function to call for next number}
      procedure SetFaxLogProc(FLP : FaxLogProc);
        {-Set procedure to call for each fax transmitted or received}
      procedure SetFaxNameFunc(FNF : FaxNameFunc);
        {-Set function to call to name incoming faxes}
      procedure SetAcceptFaxFunc(AFF : AcceptFaxFunc);
        {-Set function to call to accept incoming faxes}

      {Hook methods}
      procedure FaxStatus(Starting, Ending : Boolean); virtual;
        {-Overridable status proc}

      {Fax entry list stuff}
      procedure AddFaxEntry(Number : String; FName : PathStr; Cover : PathStr);
        {-Add another number to the built-in list}
      procedure ClearFaxEntries;
        {-Remove all fax entries from builtin list}

      {Status info}
      procedure GetPageInfo(var Pages : Word;
                            var Page : Word;
                            var BytesTransferred : LongInt;
                            var PageLength : LongInt); virtual;
      function GetFaxName : PathStr;
        {-Return name of current fax and size if known}
      function GetFaxProgress : Word;
        {-Return fax progress code}
      procedure FaxTransmit; virtual;
        {-abstract method, must be overridden}
      procedure FaxReceive; virtual;
        {-abstract method, must be overridden}

      {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Abstract Load}
      procedure Store(var S : IdStream);
        {-Abstract Store}
      {$ENDIF}

      {.Z+}{Private methods}
      function NextFax(var Number : String;
                       var FName : PathStr;
                       var Cover : PathStr) : Boolean; virtual;
        {-Return next number to dial}
      procedure LogFax(Number : String; FName : PathStr; Log : TLogFaxCode); virtual;
        {-Call user logging procedure}
      function FaxName : PathStr; virtual;
        {-Call user FaxName procedure}
      function AcceptFax(RemoteName : Str20) : Boolean;
        {-Call user AcceptFax function}
      function afConvertHeaderString(S : String) : String;
      procedure afRawInit(ID : Str20);
      {.Z-}
    end;

  function NextFaxList(AP : AbstractFaxPtr;
                       var Number : String;
                       var FName : PathStr;
                       var Cover : PathStr) : Boolean;
    {-Returns next fax name/number in builtin list}

  function FaxNameMD(AP : AbstractFaxPtr) : PathStr;
    {-Returns name for incoming fax}
  function FaxNameCount(AP : AbstractFaxPtr) : PathStr;
    {-Returns name for incoming fax}

{.Z+}{Private data types}
type
  {End-of-page status}
  ReceivePageStatus = (
    rpsBadPage,
    rpsMoreSame,
    rpsNewPage,
    rpsNewDocument,
    rpsEndOfDocument);

  {Send machine states}
  SendStates = (
    tfNone,

    {Setup, both classes}
    tfGetEntry,
    tfInit,

    {Phase A, Class 1}
    tf1Init1,

    {Phase A, Class 2}
    tf2Init1,
    tf2Init2,
    tf2Init3,

    {Phase A, both classes}
    tfDial,
    tfRetryWait,

    {Phase B, Class 1}
    tf1Connect,
    tf1SendTSI,
    tf1TSIResponse,
    tf1DCSResponse,
    tf1TrainStart,
    tf1TrainFinish,
    tf1WaitCFR,
    tf1WaitPageConnect,

    {Phase B, Class 2}
    tf2Connect,
    tf2GetParams,

    {Phase C, both classes}
    tfWaitXon,
    tfWaitFreeHeader,
    tfSendPageHeader,
    tfOpenCover,
    tfSendCover,
    tfPrepPage,
    tfSendPage,
    tfDrainPage,

    {Phase D states for Class 1}
    tf1PageEnd,
    tf1PrepareEOP,
    tf1SendEOP,
    tf1WaitEOP,
    tf1WaitMCF,
    tf1SendDCN,
    tf1Hangup,
    tf1WaitHangup,

    {Phase D, Class 2}
    tf2SendEOP,
    tf2WaitFPTS,
    tf2WaitFET,
    tf2WaitPageOK,
    tf2NextPage,

    {Phase E, both classes}
    tfClose,
    tfCompleteOK,
    tfAbort,
    tfDone);

  {Receive machine states}
  ReceiveStates = (
    rfNone,

    {Setup, both classes}
    rfInit,

    {Setup, class 1}                                                   {!!.02}
    rf1Init1,                                                          {!!.02}

    {Setup, class 2}
    rf2Init1,
    rf2Init2,
    rf2Init3,

    {Phase A, both classes}
    rfWaiting,
    rfAnswer,

    {Phase B, class 1}
    rf1SendCSI,
    rf1SendDIS,
    rf1CollectFrames,
    rf1CollectRetry1,                                                  {!!.03}
    rf1CollectRetry2,                                                  {!!.03}
    rf1StartTrain,
    rf1CollectTrain,
    rf1Retrain,
    rf1FinishTrain,
    rf1SendCFR,
    rf1WaitPageConnect,

    {Phase B, class 2}
    rf2ValidConnect,
    rf2GetSenderID,
    rf2GetConnect,

    {Phase C}
    rfStartPage,
    rfGetPageData,

    {Phase D, class 1}
    rf1FinishPage,
    rf1WaitEOP,
    rf1WritePage,
    rf1SendMCF,
    rf1WaitDCN,
    rf1WaitHangup,

    {Phase D, class 2}
    rf2GetPageResult,
    rf2GetFHNG,

    {Phase E, both classes}
    rfComplete,
    rfAbort,
    rfDone);

  {CAS states send and receive}
  CASStates = (
    csNone,
    csInit,
    csSubmitting,
    csWaiting,
    csAbort,
    csDone);

const
  {Bit reversed fax control fields IDs from HDLC info field}
  NSFFrame = $20;
  EOPFrame = $2E;
  CSIFrame = $40;
  TSIFrame = $42;
  FTTFrame = $44;
  RTNFrame = $4C;
  MPSFrame = $4E;
  DISFrame = $80;
  DCSFrame = $82;
  CFRFrame = $84;
  MCFFrame = $8C;
  EOMFrame = $8E;
  DCNFrame = $FA;                                                      {!!.01}
  RTPFrame = $CC;

  {Size of buffer for fax file data}
  DataBufferSize = 4096;

  {DIS/DCS permanent bit masks, bit reversed}
  DISGroup1   = $00;        {No group 1/2 options}
  DISGroup3_1 = $02;        {RS 465 receiver support}                  {!!.02}
  DISGroup3_2 = $88;        {A4 width, unlimited len, extended byte}   {!!.01}
  DISGroup3_3 = $00;        {No extended options}

  {DIS/DCS option bits for DISGroup3_1}
  DISHighResolution = $40;
  DIS2400BPS        = $00;
  DIS4800BPS        = $08;
  DIS7200BPS        = $0C;
  DIS9600BPS        = $04;
  DIS12000BPS       = $10;
  DIS14400BPS       = $20;

  {!!.03 - added}
  {DIS/DCS option bits for DISGroup3_2}
  DISWideWidth      = $01;

  {Class 1 constants}
  AddrField = #$FF;
  ControlField = #$03;
  ControlFieldLast = #$13;
{.Z-}

  {$IFDEF UseStreams}
  procedure AbstractFaxStream(SPtr : IdStreamPtr);
    {-Register all types needed for fax streams}
  {$ENDIF}

implementation

{AbstractFax}

  procedure AbstractFax.afRawInit(ID : Str20);
  begin
    StationID := ID;
    Title := '';
    CoverFile := '';
    Recipient := '';
    Sender := '';
    DestDir := '';
    CurrPage := 0;
    PageCount := 0;
    CoverCount := 0;
    ClassInUse := ctUnknown;
    MaxConnect := DefConnectAttempts;
    afFlags := DefFaxOptions;
    @afStatus := nil;
    @afNextFax := @NextFaxList;
    @afFaxName := @FaxNameMD;
    @afLogFax := nil;
    @afAcceptFax := nil;
    FaxListHead := nil;
    FaxListTail := nil;
    FaxListNode := nil;
    FaxListCount := 0;
    RemoteID := '';
  end;

  constructor AbstractFax.Init(ID : Str20);
  begin
    if not Root.Init then begin
      AsyncStatus := ecOutOfMemory;
      Fail;
    end;

    afRawInit(ID);
  end;

  destructor AbstractFax.Done;
  begin
    Root.Done;

    {Dispose of faxentry list}
    ClearFaxEntries;
  end;

  procedure AbstractFax.afOptionsOn(OptionFlags : Word);
    {-Activate multiple options}
  begin
    afFlags := afFlags or (OptionFlags and not BadFaxOptions);
  end;

  procedure AbstractFax.afOptionsOff(OptionFlags : Word);
    {-Deactivate multiple options}
  begin
    afFlags := afFlags and not (OptionFlags and not BadFaxOptions);
  end;

  function AbstractFax.afOptionsAreOn(OptionFlags : Word) : Boolean;
    {-Return True if all specified options are on}
  begin
    afOptionsAreOn := (afFlags and OptionFlags = OptionFlags);
  end;

  procedure AbstractFax.SetTitle(NewTitle : String);
  begin
    Title := NewTitle;
  end;

  procedure AbstractFax.SetRecipientName(NewName : String);
    {-Set name of recipient}
  begin
    Recipient := NewName;
  end;

  procedure AbstractFax.SetSenderName(NewName : String);
    {-Set name of sender}
  begin
    Sender := NewName;
  end;

  procedure AbstractFax.FaxStatus(Starting, Ending : Boolean);
  begin
    if @afStatus <> nil then
      afStatus(@Self, Starting, Ending);
  end;

  procedure AbstractFax.SetFaxStatusProc(SP : FaxStatusProc);
  begin
    afStatus := SP;
  end;

  procedure AbstractFax.SetConnectAttempts(Attempts : Word; DelayTicks : Word);
    {-Set number of connect attempts per fax, 0 = infinite}
  begin
    MaxConnect := Attempts;
    RetryWait := DelayTicks;
  end;

  procedure AbstractFax.SetDestinationDir(Dest : PathStr);
    {-Set a destination directory for received files}
  begin
    DestDir := StUpcase(Dest);
  end;

  procedure AbstractFax.SetNextFaxFunc (NFF : NextFaxFunc);
    {-Set function to call for next number}
  begin
    afNextFax := NFF;
  end;

  procedure AbstractFax.SetFaxLogProc(FLP: FaxLogProc);
    {-Set function to call for each fax sent/received}
  begin
    afLogFax := FLP;
  end;

  procedure AbstractFax.SetFaxNameFunc(FNF : FaxNameFunc);
    {-Set function to call to name incoming faxes}
  begin
    afFaxName := FNF;
  end;

  procedure AbstractFax.SetAcceptFaxFunc(AFF : AcceptFaxFunc);
    {-Set function to call to accept incoming faxes}
  begin
    afAcceptFax := AFF;
  end;

  procedure AbstractFax.SetStationID(NewID : Str20);
  begin
    StationID := NewID;
  end;

  function AbstractFax.NextFax(var Number : String;
                               var FName : PathStr;
                               var Cover : PathStr) : Boolean;
    {-Return next number to dial}
  begin
    if @afNextFax <> nil then begin
      NextFax := afNextFax(@Self, Number, FName, Cover);
      FName := StUpcase(FName);
      Cover := StUpcase(Cover);
    end else
      NextFax := False;
  end;

  procedure AbstractFax.LogFax(Number : String;
                               FName: PathStr;
                               Log : TLogFaxCode);
    {-Return next number to dial}
  begin
    if @afLogFax <> nil then
      afLogFax(@Self, Number, FName, Log);
  end;

  function AbstractFax.FaxName : PathStr;
    {-Call FaxName hook}
  begin
    if @afFaxName <> nil then
      FaxName := afFaxName(@Self)
    else
      FaxName := 'NONAME.APF';
  end;

  function AbstractFax.AcceptFax(RemoteName : Str20) : Boolean;
    {-Call AcceptFax hook}
  begin
    if @afAcceptFax <> nil then
      AcceptFax := afAcceptFax(@Self, RemoteName)
    else
      AcceptFax := True;
  end;

  function AbstractFax.afConvertHeaderString(S : String) : String;
    {-compress a fax header string, converting tags to appropriate values}
  var
    I, N : Integer;
    T : String;
  begin
    {walk thru the string, converting tags to appropriate data}
    I := Pos('$', S);
    while I > 0 do begin
      {get length of tag}
      N := I;
      while (N <= Length(S)) and (S[n] <> ' ') do
        Inc(N);
      Dec(N, I);

      {preserve and delete tag from the main string}
      T := Copy(S, I, N);
      Delete(S, I, N);

      {which tag?}
      case Upcase(T[2]) of
        'D':  {insert Date}
          T := TodayString;

        'T':  {insert Time}
          T := NowString;

        'I':  {insert station Id}
          T := StationID;

        'S':  {insert Sender (Title)}
          T := Title;

        'P':  {insert current Page number}
          if CoverCount > 0 then
            if SendingCover then
              T := '1'
            else
              Str(CurrPage+1, T)
          else
            Str(CurrPage, T);

        'N':  {insert Number of pages}
          Str(PageCount+CoverCount, T);

        'F' : {insert from name}
          T := Sender;

        'R' : {insert recipient's name}
          T := Recipient;

        else  {invalid tag, do nothing}
          T := '';
      end;
      Insert(T, S, I);

      {find next tag}
      I := Pos('$', S);
    end;

    afConvertHeaderString := S;
  end;

  procedure AbstractFax.AddFaxEntry(Number : String;
                                    FName : PathStr;
                                    Cover : PathStr);
    {-Add another number to the built-in list}
  var
    Node : PFaxEntry;
  begin
    if GetMemCheck(Node, SizeOf(TFaxEntry)) then begin

      AsyncStatus := ecOk;

      {Create new node}
      with Node^ do begin
        fNumber := Number;
        fFName := FName;
        fCover := Cover;
        fNext := nil;
      end;

      if FaxListHead = nil then begin
        {Set head/tail if this is the first...}
        FaxListHead := Node;
        FaxListTail := Node;
        FaxListNode := Node;
        FaxListCount := 1;
      end else begin
        {Attach to previous tail}
        FaxListTail^.fNext := Node;
        FaxListTail := Node;
        Inc(FaxListCount);
      end;
    end else
      AsyncStatus := ecOutOfMemory;
  end;

  procedure AbstractFax.ClearFaxEntries;
    {-Remove all fax entries from builtin list}
  var
    Node : PFaxEntry;
    Next : PFaxEntry;

  begin
    Node := FaxListHead;
    while Node <> nil do begin
      Next := Node^.fNext;
      FreeMemCheck(Node, SizeOf(TFaxEntry));
      Node := Next;
    end;
    FaxListCount := 0;
    FaxListHead := nil;
    FaxListTail := nil;
    FaxListNode := nil;
  end;

  procedure AbstractFax.GetPageInfo(var Pages : Word;
                                    var Page : Word;
                                    var BytesTransferred : LongInt;
                                    var PageLength : LongInt);
  begin
    Pages := 0;
    Page := 0;
    BytesTransferred := 0;
    PageLength := 0;
  end;

  function AbstractFax.GetFaxName : PathStr;
    {-Return name of current fax, with path if supplied}
  begin
    GetFaxName := FaxFileName;
  end;

  function AbstractFax.GetFaxProgress : Word;
    {-Return fax progress code}
  begin
    GetFaxProgress := FaxProgress;
  end;

  procedure AbstractFax.FaxTransmit;
  begin
    RunError(211);
  end;

  procedure AbstractFax.FaxReceive;
  begin
    RunError(211);
  end;

  {$IFDEF UseStreams}
  constructor AbstractFax.Load(var S : IdStream);
    {-Abstract Load}
  begin
    afRawInit('');

    {Load stream data}
    S.Read(MaxConnect, SizeOf(MaxConnect));
    S.Read(RetryWait, SizeOf(RetryWait));
    S.Read(afFlags, SizeOf(afFlags));
    StationID := S.ReadString;
    Recipient := S.ReadString;
    Sender := S.ReadString;

    @afStatus := S.ReadUserPointer(Nil);
    @afNextFax := S.ReadUserPointer(Nil);
    if @afNextFax = nil then
      @afNextFax := @NextFaxList;
    @afLogFax := S.ReadUserPointer(Nil);
    @afFaxName := S.ReadUserPointer(Nil);
    if @afFaxName = nil then
      @afFaxName := @FaxNameMD;
    @afAcceptFax := S.ReadUserPointer(Nil);
  end;

  procedure AbstractFax.Store(var S : IdStream);
    {-Abstract Store}
  begin
    {Store various data}
    S.Write(MaxConnect, SizeOf(MaxConnect));
    S.Write(RetryWait, SizeOf(RetryWait));
    S.Write(afFlags, SizeOf(afFlags));
    S.WriteString(StationID);
    S.WriteString(Recipient);
    S.WriteString(Sender);

    S.WriteUserPointer(@afStatus, ptNil);
    S.WriteUserPointer(@afNextFax, ptNil);
    S.WriteUserPointer(@afLogFax, ptNil);
    S.WriteUserPointer(@afFaxName, ptNil);
    S.WriteUserPointer(@afAcceptFax, ptNil);
  end;
  {$ENDIF}

{Builtin functions}

  function NextFaxList(AP : AbstractFaxPtr;
                       var Number : String;
                       var FName : PathStr;
                       var Cover : PathStr) : Boolean;
  var
    Node : PFaxEntry;
  begin
    with AP^ do begin
      if FaxListNode <> nil then begin
        NextFaxList := True;
        with FaxListNode^ do begin
          Number := fNumber;
          FName := fFName;
          Cover := fCover;
          FaxListNode := fNext;
        end;
      end else
        NextFaxList := False;
    end;
  end;

  function FaxNameMD(AP : AbstractFaxPtr) : PathStr;
    {-Returns name for incoming fax like MMDD0001.APF}
  var
    I : Word;
    MS, DS : String[2];
    FName1 : String[4];
    FName : String;
    Y,M,D,O : Word;

    procedure MakeFileName(I : Word);
    var
      CountS : String[4];
      J : Word;
    begin
      Str(I:4, CountS);
      for J := 1 to 4 do
        if CountS[J] = ' ' then
          CountS[J] := '0';
      FName := FName1 + CountS + '.' + FaxFileExt;
      with AP^ do
        if DestDir <> '' then
          FName := AddBackSlash(DestDir) + FName;
    end;

  begin
    {Get the date}
    GetDate(Y,M,D,O);
    Str(M:2, MS);
    Str(D:2, DS);
    FName1 := MS + DS;
    for I := 1 to 4 do
      if FName1[I] = ' ' then
        FName1[I] := '0';

    {Find last file with this date}
    I := 0;
    repeat
      Inc(I);
      MakeFileName(I);
    until not ExistFile(FName) or (I = 10000);

    if I < 10000 then begin
      MakeFileName(I);
      FaxNameMD := FName;
    end else begin
      AsyncStatus := ecTooManyFiles;
      FaxNameMD := 'NONAME.APF';
    end;
  end;

  function FaxNameCount(AP : AbstractFaxPtr) : PathStr;
    {-Returns name for incoming fax like FAX0001.APF}
  var
     I : Word;
    FName : String;

    procedure MakeFileName(I : Word);
    var
      CountS : String[4];
      J : Word;
    begin
      Str(I:4, CountS);
      for J := 1 to 4 do
        if CountS[J] = ' ' then
          CountS[J] := '0';
      FName := 'FAX' + CountS + '.' + FaxFileExt;
      with AP^ do
        if DestDir <> '' then
          FName := AddBackSlash(DestDir) + FName;
    end;

  begin
    {Find last file}
    I := 0;
    repeat
      Inc(I);
      MakeFileName(I);
    until not ExistFile(FName) or (I = 10000);

    if I < 10000 then begin
      MakeFileName(I);
      FaxNameCount := FName;
    end else begin
      AsyncStatus := ecTooManyFiles;
      FaxNameCount := 'NONAME.APF';
    end;
  end;

  {$IFDEF UseStreams}
  procedure AbstractFaxStream(SPtr : IdStreamPtr);
    {-Register all types needed for abstract fax converter}
  begin
    SPtr^.RegisterType(otAbstractFax, veAbstractFax,
                       TypeOf(AbstractFax),
                       @AbstractFax.Store, @AbstractFax.Load);
  end;
  {$ENDIF}

end.
