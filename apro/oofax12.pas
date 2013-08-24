{$A+,F+,I-,R-,S-,V-}

{$I APDEFINE.INC}

{Include OPro's define file if UseOPro is specified}
{$IFDEF UseOPro}
{$I OPDEFINE.INC}
{$ENDIF}

{$IFNDEF UseSWFlow}
{$IFNDEF UseHWFlow}
!! STOP COMPILE - this unit requires hardware and/or software flow control
{$ENDIF}
{$ENDIF}

{.$DEFINE BadSendTrain}
{.$DEFINE BadReceiveTrain}

{******************************************************}
{*                   OOFAX12.PAS 2.03                 *}
{*      Copyright (c) TurboPower Software 1993.       *}
{*                All rights reserved.                *}
{******************************************************}

unit OoFax12;
  {-Class 1/2 FAX send/receive objects}

interface

uses
  Dos,
  Dpmi,
  {$IFDEF UseOPro}
  OpInline,
  OpString,
  OpDos,
  OpRoot,
  {$IFDEF Opro12}
  OpConst,
  {$ENDIF}
  {$ENDIF}
  {$IFDEF UseTPro}
  TpInline,
  TpMemChk,
  TpString,
  TpDos,
  {$ENDIF}
  ApMisc,
  ApTimer,
  ApPort,
  OoCom,
  OoFaxCvt,
  OoAbsFax;

const
  {.Z+}
  {Private constants}
  MaxModIndex = 6;
  PhysicalTopMargin : Word = 8;   {Phsyical top margin in raster lines}
  MaxSendCount : Word      = 25;  {Max blocks/lines per FaxTransmitPart}
  BufferMinimum : Word     = 500; {Minimum buffer quantity before yield}
  {.Z-}

type
  C12AbstractFaxPtr = ^C12AbstractFax;
  C12AbstractFax =
    object(AbstractFax)
      ForceStatus  : Boolean;            {True to force special status}
      Critical     : Boolean;            {True if critical period}
      MorePages    : Boolean;            {True if more fax pages to receive}
      SessionRes   : Boolean;            {Remote/session resolution}
      SessionWid   : Boolean;            {Remote/session high width}    {!!.03}
      SessionECM   : Boolean;            {Remote/session ECM}
      CanDoHighRes : Boolean;            {True if capable of high res}  {!!.03}
      CanDoHighWid : Boolean;            {True if capable of high width}{!!.03}
      LastFrame    : Boolean;            {True if last Class 1 frame}
      ToneDial     : Boolean;            {True to use tone dialing}
      LastPageOk   : Boolean;            {True if last page received OK}
      UseLengthWord: Boolean;            {True if file has length words}
      CollectResponse : Boolean;         {True for data, False for OK}
      CoverIsAPF   : Boolean;            {True if cover page is apf file}
      InitSent     : Boolean;            {True if modem/def init sent once}
      SlowBaud     : Boolean;            {True if using init baud rate}{!!.01}
      GotCFR       : Boolean;            {True if got CFR after training}{!!.02}
      CheckChar    : Char;               {ECM character}
      ResC         : Char;               {Resolution character}
      FaxAndData   : Char;               {'0' = fax, '1' = fax and data}
      SessionScan  : Byte;               {Remote/session scantime}
      ReceivedFrame: Byte;               {last received HDLC frame type}
      CRLFIndex    : Byte;               {Index for CRLF checking}
      ETXIndex     : Byte;               {Index for DLE/ETX checking}
      ReplyWait    : Word;               {Ticks to wait for a modem reply}
      TransWait    : Word;               {Ticks to wait for outbuf space}
      StatusWait   : Word;               {Ticks between status updates}
      MaxFaxBPS    : Word;               {Max fax BPS for this modem}
      BPSIndex     : Word;               {Last Class 1BPS index}
      MinBytes     : Word;               {Minimum raster line length}
      HangupCode   : Word;               {Last FHNG code}
      CurrOfs      : Word;               {curr offset in DataBuffer}
      BytesRead    : Word;               {Bytes to be transmitted}
      BadData      : Word;               {Bad bytes during train}
      Retry        : Word;               {General retry counter}
      DialWait     : Integer;            {ticks timeout for dialing}
      AnswerOnRing : Integer;            {ring on which to answer phone}
      RingCounter  : Integer;            {count of rings seen}
      SessionBPS   : LongInt;            {Remote/session BPS}
      NormalBaud   : LongInt;            {Normal baud rate}            {!!.01}
      InitBaud     : LongInt;            {Initialization baud rate}    {!!.01}
      DataBuffer   : PByteBuffer;        {buffer for received data}
      CPort        : AbstractPortPtr;    {com port object to use}

      InFile       : File;               {received data file}
      Response     : String;             {Current modem response}
      ModCode      : String[3];          {modulation code}
      FaxHeader    : FaxHeaderRec;       {fax file header block}
      PageHeader   : PageHeaderRec;      {fax file page header block}
      InFileName   : PathStr;            {name of current file}
      ModemInit    : String[40];         {modem init string}
      DialPrefix   : String[40];         {dialing prefix}
      ReplyTimer   : EventTimer;         {Timer for all replies}
      LocalMods    : array[1..MaxModIndex] of Boolean; {Local Class 1 mods}
      RmtMods      : array[1..MaxModIndex] of Boolean; {Remote Class 1 mods}

      {Constructors/destructors}
      constructor Init(ID : Str20; ComPort : AbstractPortPtr);
      destructor Done; virtual;

      {User Control}
      procedure SetFaxPort(ComPort : AbstractPortPtr);
        {-Select the comport to use}
      procedure SetModemInit(MIS : String);
        {-Define the modem init string}
      function SetClassType(CT : ClassType) : ClassType;
        {-Set type of modem, return detected or set type}
      procedure SetInitBaudRate(InitRate, NormalRate : LongInt);       {!!.01}
        {-Set baud rate to use when initializing modem}                {!!.01}
      function GetModemClassSupport(var Class1, Class2 : Boolean;
                                    Reset : Boolean) : Boolean;
        {-Find out which classes the modem supports}

      function GetModemClassSupportEx(                                 {!!.02}
                 var Class1, Class2, Class2_0 : Boolean;               {!!.02}
                 Reset : Boolean) : Boolean;                           {!!.02}
        {-Find out which classes the modem supports, including 2.0}
      function GetModemInfo(var Class : Char;
                            var Model, Chip, Rev : String;
                            Reset : Boolean) : Boolean;
        {-Get specific data from modem}
      procedure GetModemFeatures(var BPS : LongInt; var Correction : Char);
        {-Return highest possible codes}
      procedure SetModemFeatures(BPS : LongInt; Correction : Char);
        {-Set modem features for this session}
      procedure GetPageInfo(var Pages : Word;
                            var Page : Word;
                            var BytesTransferred : LongInt;
                            var PageLength : LongInt); virtual;
        {-Return page info}
      function GetLastPageStatus : Boolean;
        {-Return True if last page received OK, false otherwise}
      function GetRemoteID : Str20;
        {-Return remote station ID}
      procedure GetSessionParams(var BPS : LongInt;
                                 var Resolution : Boolean;
                                 var Correction : Boolean);
        {-Return remote/session parameters}
      function GetHangupResult : Word;
        {-Return last hangup result, class 2 only}

      {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
      procedure Store(var S : IdStream);
      {$ENDIF}

      {#Z+}
      {Private}
      function afHandleAbort : Boolean;
      procedure caRawInit(ID : Str20; ComPort : AbstractPortPtr);
      function caLocatePage(PgNo : Word) : Integer;
      function caSpeedCode : Char;
      procedure caFlushModem;
      function caAddReceivedCmd(var S : String) : Boolean;
      procedure caPrepResponse;
      procedure caPutModemSlow(S : String);
      procedure caPutModem(S : String);
      procedure caPutFaxCommand(S : String);
      procedure caPutFrameR;
      procedure caPutFrameT;
      function caProcessModemCmd(S : String) : Boolean;
      function caOkResponse : Boolean;
      function caConnectResponse : Boolean;
      function caNoCarrierResponse : Boolean;
      function caErrorResponse : Boolean;
      function caFHNGResponse : Boolean;
      function caExtractFHNGCode : Word;
      procedure caHDLCStart(Last : Boolean);
      procedure caHDLCEnd;
      procedure caPutStandardFrame(Frame : Byte);
      procedure caPutTSIFrame;
      procedure caPutDCSDISFrame(UseDIS : Boolean);
      procedure caPutCSIFrame;
      procedure caPutTCFData;
      procedure caCalcMinBytesPerLine; virtual;
      function caNextBPS : Boolean;
      procedure caGetClass1Modulations(Transmit : Boolean);
      procedure caExtractClass1Params(DIS : Boolean; B2, B3 : Byte);
      procedure caExtractClass2Params(S : String);
      function caProcessFrame(var Retrain : Boolean) : Boolean;
      procedure caSwitchBaud(High : Boolean);                          {!!.01}
      {#Z-}
    end;

type
  C12SendFaxPtr = ^C12SendFax;
  C12SendFax =
    object(C12AbstractFax)
      MCFConnect   : Boolean;            {True if CONNNECT waiting for MCF}
      Retries      : Integer;            {Count of retries on failed page send}
      MaxRetries   : Integer;            {Max times to retry failed send}
      Converter    : AbstractFaxConverterPtr; {Fax converter for header lines}
      State        : SendStates;         {Current state of machine}
      HeaderLine   : String;             {Header line for each page}
      CvrF         : Text;               {File for cover page}
      CvrOpen      : Boolean;            {True if cover file open}     {!!.02}
      RetryPage    : Boolean;            {True to retry page}          {!!.02}

      {Constructor/destructors}
      constructor Init(ID : Str20; ComPort : AbstractPortPtr);
      destructor Done; virtual;

      {User control}
      procedure SetToneDial(Tone : Boolean);
        {-Select tone or pulse dial (send only)}
      procedure SetDialPrefix(P : String);
        {-Set the dial prefix string}
      procedure SetDialTime(DT : Integer);
        {-Set the dialing timeout}
      procedure SetHeaderText(S : String);
        {-Set HeaderLine to S}
      procedure SetMaxRetries(MR : Integer);
        {-Set MaxRetries to MR}
      function FaxTransmitPart : FaxStateType; virtual;
        {-Perform one increment of a fax transmit}
      procedure PrepareFaxTransmitPart;
        {-Prepare to transmit in increments}
      procedure FaxTransmit; virtual;
        {-Call FaxTrasmitPart until fax is sent}

      {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Abstract Load}
      {$ENDIF}

      {#Z+}
      {Private}
      procedure csSendWhiteRasterLines(Count : Word);
      procedure csPutBuffer(var Buffer; Len : Word);
      procedure csPutLineBuffer(var Buffer; Len : Word);
      function csOpenFaxFile(Document : PathStr) : Word;
      function csAdvancePage : Boolean;
      procedure csRawInit;
      function csOpenCoverPage : Boolean;                              {!!.03}
      {#Z-}
    end;

type
  C12ReceiveFaxPtr = ^C12ReceiveFax;
  C12ReceiveFax =
    object(C12AbstractFax)
      OneFax       : Boolean;            {True if only receiving one fax}
      ShowStatus   : Boolean;            {True if status window opened}
      Last         : Char;               {Last received data char}
      PageStatus   : ReceivePageStatus;  {Status of most-recent page}
      State        : ReceiveStates;      {Current state of StateMachine}
      FirstState   : ReceiveStates;      {State in 1st call to FaxReceivePart}

      {Constructor/destructor}
      constructor Init(ID : Str20; ComPort : AbstractPortPtr);
      destructor Done; virtual;

      {User control}
      procedure SetAnswerOnRing(AOR : Integer);
        {-set to answer call on AOR'th ring}
      procedure SetFaxAndData(OnOff : Boolean);
        {-True for fax to answer either fax or data, False for fax only}
      procedure SetConnectState;
        {-Force the receiver to pick up a connection in progress}
      procedure SetOneFax(OnOff : Boolean);
        {-Set "one fax" receive behavior on/off}
      function InitModemForFaxReceive : Boolean;
        {-Prepare faxmodem for receiving faxes}
      function FaxReceivePart : FaxStateType;
        {-State machine for receiving FAX}
      procedure PrepareFaxReceivePart;
        {-Prepare to call FaxReceivePart}
      procedure FaxReceive; virtual;
        {-Wrapper routine to wait for a call and receive a FAX}

      {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
      procedure Store(var S : IdStream);
      {$ENDIF}

      {#Z+}
      {Private}
      procedure crFlushBuffer;
      function crAddReceivedData : Boolean;
      function crUpdateMainHeader : Integer;
      function crUpdatePageHeader(PgNo : Word; var PgInfo : PageHeaderRec) : Integer;
      {#Z-}
    end;

  {$IFDEF UseStreams}
  procedure C12AbstractFaxStream(SPtr : IdStreamPtr);
    {-Register all types needed for C12 abstract fax converter}
  procedure C12SendFaxStream(SPtr : IdStreamPtr);
    {-Register all types needed for C12 send fax converter}
  procedure C12ReceiveFaxStream(SPtr : IdStreamPtr);
    {-Register all types needed for C12 receive fax converter}
  {$ENDIF}

implementation

const
  {For calculating minimum bytes per line}
  ScanTimes : array[0..7, Boolean] of Byte = (
    (0,0),    {0}
    (5,5),    {1}
    (10,5),   {2}
    (10,10),  {3}
    (20,10),  {4}
    (20,20),  {5}
    (40,20),  {6}
    (40,40)); {7}

  {!!.01 new}
  {For calculating scan time response in DCS frame}
  ScanTimeResponse : array[0..7, Boolean] of Byte = (
    ($70, $70),
    ($10, $10),
    ($20, $10),
    ($20, $20),
    ($00, $20),
    ($00, $00),
    ($40, $00),
    ($40, $40));
  ZeroScanTime = $70;

  {For managing Class 1 modulations}
  ModArray : array[1..MaxModIndex] of String[3] = (
    '24', '48', '72', '96', '121', '145');

  {For getting MaxFaxBPS from modulation index}
  Class1BPSArray : array[1..MaxModIndex] of Word = (
    2400, 4800, 7200, 9600, 12000, 14400);

{General purpose}

  function RotateByte(Code : Char) : Byte; assembler;
    {-Flip code MSB for LSB}
  asm
    mov dl,Code
    xor ax,ax
    mov cx,8
@1: shr dl,1
    rcl al,1
    loop @1
  end;

{C12AbstractFax}

  procedure C12AbstractFax.caRawInit(ID : Str20; ComPort : AbstractPortPtr);
  begin
    DataBuffer := nil;
    FillChar(FaxHeader, SizeOf(FaxHeader), 0);
    ClassInUse   := ctDetect;
    CPort        := ComPort;
    ToneDial     := True;
    ModemInit    := '';
    DialPrefix   := '';
    RemoteID     := '';
    DialWait     := DefDialTimeout;
    MaxFaxBPS    := 9600;
    CheckChar    := '0';
    AnswerOnRing := 1;
    DataCount    := 0;
    PageSize     := 0;
    CurrOfs      := 0;
    ReplyWait    := DefCmdTimeout;
    TransWait    := DefTransTimeout;
    StatusWait   := DefStatusTimeout;
    Critical     := False;
    FaxAndData   := '0';
    SessionBPS   := 0;
    SessionScan  := 0;
    SessionRes   := False;
    SessionWid   := False;                                             {!!.03}
    SessionECM   := False;
    InitSent     := False;
    SlowBaud     := False;                                             {!!.01}
    InitBaud     := 0;                                                 {!!.01}
  end;

  constructor C12AbstractFax.Init(ID : Str20; ComPort : AbstractPortPtr);
  begin
    caRawInit(ID, ComPort);

    if not AbstractFax.Init(ID) then
      Fail;

    if not GetMemCheck(DataBuffer, DataBufferSize) then begin
      ComPort^.GotError(ecOutOfMemory);
      Done;
      Fail;
    end;
  end;

  destructor C12AbstractFax.Done;
  begin
    FreeMemCheck(DataBuffer, DataBufferSize);
    AbstractFax.Done;
  end;

  procedure C12AbstractFax.SetFaxPort(ComPort : AbstractPortPtr);
  begin
    CPort := ComPort;
  end;

  procedure C12AbstractFax.SetModemInit(MIS : String);
    {-set modem init string}
  begin
    ModemInit := StUpcase(MIS);
    if (ModemInit <> '') and (ModemInit[1] <> 'A') then
      ModemInit := 'AT'+ModemInit;
  end;

  function C12AbstractFax.SetClassType(CT : ClassType) : ClassType;
    {-Set type of modem, return detected or set type}
  var
    Class1 : Boolean;
    Class2 : Boolean;
  begin
    if CT = ctDetect then begin
      if GetModemClassSupport(Class1, Class2, False) then begin
        if Class2 then
          ClassInUse := ctClass2
        else if Class1 then
          ClassInUse := ctClass1
        else
          ClassInUse := ctUnknown;
      end else
        ClassInUse := ctUnknown;
    end else
      ClassInUse := CT;

    SetClassType := ClassInUse;
  end;

  {!!.01 new}
  procedure C12AbstractFax.SetInitBaudRate(InitRate, NormalRate : LongInt);
    {-Set baud rate to use when initializing modem}
  var
    Parity : ParityType;
    DataBits : DataBitType;
    StopBits : StopBitType;
  begin
    InitBaud := InitRate;
    if NormalRate = 0 then
      CPort^.GetLine(NormalBaud, Parity, DataBits, StopBits, False)
    else
      NormalBaud := NormalRate;

    {Start in low baud}                                                {!!.02}
    caSwitchBaud(False);                                               {!!.02}
  end;

  function C12AbstractFax.afHandleAbort : Boolean;
  begin
    if CPort^.UserAbort then begin
      CPort^.GotError(ecUserAbort);
      afHandleAbort := True;
    end else
      afHandleAbort := False;
  end;

  function C12AbstractFax.caLocatePage(PgNo : Word) : Integer;
  var
    W : Word;
    L : LongInt;
    Result : Word;
    P : PageHeaderRec;
  begin
    caLocatePage := ecDiskRead;                                        {!!.02}

    {validate number}
    if (PgNo = 0) or (PgNo > FaxHeader.PageCount) then
      Exit;

    {start at head of file and walk the list of pages}
    Seek(InFile, FaxHeader.PageOfs);
    Result := IOResult;
    if Result <> 0 then begin
      caLocatePage := Result;
      Exit;
    end;

    if PgNo > 1 then begin
      for W := 1 to (PgNo-1) do begin
        BlockRead(InFile, P, SizeOf(P));
        Result := IOResult;
        if Result <> 0 then begin
          caLocatePage := Result;
          Exit;
        end;
        L := FilePos(InFile);
        Inc(L, P.ImgLength);

        Seek(InFile, L);
        Result := IoResult;
        if Result <> 0 then begin                                      {!!.02}
          caLocatePage := Result;                                      {!!.02}
          Exit;                                                        {!!.02}
        end;                                                           {!!.02}
      end;
    end;

    caLocatePage := Result;
  end;

  function C12AbstractFax.caAddReceivedCmd(var S : String) : Boolean;
    {-if char(s) pending, add to command string; return True if complete
      response has been assembled}
  var
    C : Char;
  begin
    caAddReceivedCmd := False;
    AsyncStatus := ecOK;
    while CPort^.CharReady do begin
      CPort^.GetChar(C);
      if C = #13 then begin
        if S <> '' then begin
          caAddReceivedCmd := True;
          Exit;
        end;
      end else if C >= #32 then
        Merge(S, C);
    end;
  end;

  procedure C12AbstractFax.caPrepResponse;
    {-Prepare to receive/parse a new modem response}
  begin
    Response := '';
    CRLFIndex := 0;
    ETXIndex := 0;
    CollectResponse := True;
    NewTimer(ReplyTimer, ReplyWait);
  end;

  procedure C12AbstractFax.caPutModemSlow(S : String);
    {-Send a modem command and prepare to wait for a response}
  var
    I : Word;
  begin
    for I := 1 to Length(S) do begin
      Delay(InterCharDelay);
      CPort^.PutChar(S[I]);
    end;
  end;

  procedure C12AbstractFax.caPutModem(S : String);
    {-Send a modem command and prepare to wait for a response}
  var
    I : Word;
  begin
    Delay(PreCommandDelay);
    caPutModemSlow(S+^M);
    caPrepResponse;
  end;

  procedure C12AbstractFax.caPutFaxCommand(S : String);
    {-Send FTM/FRM modem command}
  begin
    Delay(PreFaxDelay);
    caPutModemSlow(S+^M);
    caPrepResponse;
  end;

  procedure C12AbstractFax.caPutFrameR;
    {-Send FRH=3 modem command}
  begin
    Delay(PreFaxDelay);
    caPutModemSlow('AT+FRH=3'^M);
    caPrepResponse;
  end;

  procedure C12AbstractFax.caPutFrameT;
    {-Send FTH=3 modem command}
  begin
    Delay(PreFaxDelay);
    caPutModemSlow('AT+FTH=3'^M);
    caPrepResponse;
  end;

  function C12AbstractFax.caProcessModemCmd(S : String) : Boolean;
  var
    C : Char;
    OkString : String;
    Finished : Boolean;
  begin
    with CPort^ do begin
      {Send the command}
      caProcessModemCmd := False;
      caPutModem(S);

      repeat
        {Collect the reponse}
        Finished := False;
        Response := '';
        repeat
          Finished := caAddReceivedCmd(Response);
        until Finished or ptWaitComplete(ReplyTimer);
        if (AsyncStatus <> ecOk) then
          Exit;
      until (Response <> '');

      {Check for errors}
      if not caErrorResponse then begin
        {Collect and discard the OK if above response was data}
        if Pos('OK', Response) = 0 then begin
          OkString := '';
          Finished := False;
          NewTimer(ReplyTimer, OkDelay);
          repeat
            Finished := caAddReceivedCmd(OkString);
          until Finished or ptWaitComplete(ReplyTimer);
          if (AsyncStatus = ecUserAbort) then
            Exit;
        end;
        caProcessModemCmd := True
      end else begin
        caProcessModemCmd := False;
        AsyncStatus := ecError;
      end;
    end;
  end;

  function C12AbstractFax.caOkResponse : Boolean;
    {-Return True if Response contains OK}
  begin
    caOkResponse := Pos('OK', Response) > 0;
  end;

  function C12AbstractFax.caConnectResponse : Boolean;
    {-Return True if Response contains CONNECT}
  begin
    caConnectResponse := Pos('CONNECT', Response) > 0;
  end;

  function C12AbstractFax.caNoCarrierResponse : Boolean;
    {-Return True if Response contains NO CARRIER}
  begin
    caNoCarrierResponse := Pos('CARRIER', Response) > 0;
  end;

  function C12AbstractFax.caErrorResponse : Boolean;
    {-Return True if Response contains ERROR}
  begin
    caErrorResponse := Pos('ERROR', Response) > 0;
  end;

  function C12AbstractFax.caFHNGResponse : Boolean;
    {-Return True if Response contains FHNG}
  begin
    caFHNGResponse := Pos('FHNG', Response) > 0;
  end;

  function C12AbstractFax.caExtractFHNGCode : Word;
    {-Return numeric FHNG response}
  var
    S : String[20];
    W : Word;
    I : Byte;
    Code : Word;
  begin
    I := Pos(':', Response);
    if I <> 0 then begin
      S := Copy(Response, I+1, 3);
      S := Trim(S);
      W := 0;
      Val(S, W, Code);
      caExtractFHNGCode := W;
    end else
      caExtractFHNGCode := 0;
  end;

  procedure C12AbstractFax.caFlushModem;
    {-"Flush" modem for init}
  begin
    with CPort^ do begin
      {"reset" modem}
      FlushInBuffer;
      FlushOutBuffer;
      SetModem(False, False);
      Delay(FlushWait);
      SetModem(True, True);
      Delay(FlushWait);
    end;
  end;

  function C12AbstractFax.GetModemClassSupport(var Class1, Class2 : Boolean;
                                               Reset : Boolean) : Boolean;
  var
    LC, C : Integer;
    Tmp : Str20;
  begin
    {assume failure}
    GetModemClassSupport := False;

    {get class to init}
    Class1 := False;
    Class2 := False;

    {Removed in !!.02}
    {Switch to initialization baud rate}                               {!!.01}
    {caSwitchBaud(False);}                                             {!!.01}

    if Reset then begin
      LC := 0;
      repeat
        caFlushModem;

        Inc(LC);
        if (LC > 2) or not caProcessModemCmd(DefInit) then
          Exit;
      until caOkResponse;
    end;

    if not caProcessModemCmd('AT+FCLASS=?') then
      Exit;
    StripPrefix(Response);
    Class1 := Pos('1', Response) > 0;
    if Pos('2.0', Response) > 0 then                                   {!!.02}
      Class2 := False                                                  {!!.02}
    else                                                               {!!.02}
      Class2 := Pos('2', Response) > 0;

    GetModemClassSupport := True;
  end;

  {!!.02 new}
  function C12AbstractFax.GetModemClassSupportEx(
                          var Class1, Class2, Class2_0 : Boolean;
                          Reset : Boolean) : Boolean;
  var
    LC, C : Integer;
    Tmp : Str20;
  begin
    {Assume failure}
    GetModemClassSupportEx := False;
    Class1 := False;
    Class2 := False;
    Class2_0 := False;

    {Reset the modem}
    if Reset then begin
      LC := 0;
      repeat
        caFlushModem;

        Inc(LC);
        if (LC > 2) or not caProcessModemCmd(DefInit) then
          Exit;
      until caOkResponse;
    end;

    {Ask the modem...}
    if not caProcessModemCmd('AT+FCLASS=?') then
      Exit;
    StripPrefix(Response);

    {...see what it said}
    Class1 := Pos('1', Response) > 0;
    if Pos('2.0', Response) > 0 then begin
      Class2_0 := True;
      Class2 := Pos('2,', Response) > 0;
    end else
      Class2 := Pos('2', Response) > 0;

    GetModemClassSupportEx := True;
  end;

  function C12AbstractFax.GetModemInfo(var Class : Char;
                                       var Model, Chip, Rev : String;
                                       Reset : Boolean) : Boolean;
  var
    C1, C2, C2_0 : Boolean;                                            {!!.02}
  begin
    {assume failure}
    GetModemInfo := False;

    {Get class to init}
    Class := '0';
    Model := '';
    Chip  := '';
    Rev   := '';

    {Removed in !!.02}
    {Switch to initialization baud rate}                               {!!.01}
    {caSwitchBaud(False);}                                             {!!.01}

    if Reset then begin
      {Toggle DTR}
      caFlushModem;

      {Send user init}
      if (ModemInit <> '') and (not caProcessModemCmd(ModemInit)) then
        Exit;

      {Send required inits}
      if not caProcessModemCmd(DefInit) then
        Exit;
    end;
    InitSent := True;

    {Determine highest class}
    if not GetModemClassSupportEx(C1, C2, C2_0, False) then            {!!.02}
      Exit;
    if C2_0 then                                                       {!!.02}
      Class := 'B'                                                     {!!.02}
    else if C2 then                                                    {!!.02}
      Class := '2'
    else if C1 then
      Class := '1'
    else
      Exit;

    {Get modem info}
    if caProcessModemCmd('AT+FMDL?') then begin
      StripPrefix(Response);
      Model := Response;
    end;

    if caProcessModemCmd('AT+FMFR?') then begin
      StripPrefix(Response);
      Chip := Response;
    end;

    if caProcessModemCmd('AT+FREV?') then begin
      StripPrefix(Response);
      Rev := Response;
    end;

    AsyncStatus := ecOk;                                               {!!.02}
    GetModemInfo := True;
  end;

  procedure C12AbstractFax.GetModemFeatures(var BPS : LongInt;
                                            var Correction : Char);
    {-Return highest possible codes}
  var
    C : Char;
    BitChar : Char;
    Finished : Boolean;
  begin
    BPS := 0;
    Correction := '0';

    {Assure we're a class 2 modem first}
    if ClassInUse <> ctClass2 then
      Exit;

    {Removed in !!.02}
    {Switch to initialization baud rate}                               {!!.01}
    {caSwitchBaud(False);}                                             {!!.01}

    {Test bit rates}
    C := '5';
    Finished := False;
    repeat
      if caProcessModemCmd('AT+FDIS=0,'+C+',0,0,0,0,0,0') and caOkResponse then begin
        BitChar := C;
        Finished := True;
      end;
      Dec(Byte(C));
      if (C = '1') and not Finished then begin
        BitChar := C;
        Finished := True;
      end;
    until Finished;
    case BitChar of
      '0' : BPS := 2400;
      '1' : BPS := 4800;
      '2' : BPS := 7200;
      '3' : BPS := 9600;
      '4' : BPS := 12000;
      '5' : BPS := 14400;
    end;

    {Test error correction}
    {Finished := False;}                                               {!!.03}
    if caProcessModemCmd('AT+FDIS=0,0,0,0,0,1,0,0') and caOkResponse then
      Correction := '1'
    else begin                                                         {!!.03}
      Correction := '0';
      AsyncStatus := ecOK;                                             {!!.03}
    end;                                                               {!!.03}
  end;

  procedure C12AbstractFax.SetModemFeatures(BPS : LongInt;
                                       Correction : Char);
    {-Set modem features for this session}
  begin
    MaxFaxBPS := BPS;
    CheckChar := Correction;
  end;

  function C12AbstractFax.caSpeedCode : Char;
    {-returns char code for speed}
  begin
    case MaxFaxBPS of
      2400 : caSpeedCode := '0';
      4800 : caSpeedCode := '1';
      7200 : caSpeedCode := '2';
      12000: caSpeedCode := '4';
      14400: caSpeedCode := '5';
      else   caSpeedCode := '3';
    end;
  end;

  procedure C12AbstractFax.GetPageInfo(var Pages : Word;
                                       var Page : Word;
                                       var BytesTransferred : LongInt;
                                       var PageLength : LongInt);
  var
    OutBuf : Word;
  begin
    if Sending then begin
      {Transmit data}
      Pages := FaxHeader.PageCount;
      if SendingCover then
        Page := 0
      else
        Page := CurrPage;
      OutBuf := CPort^.OutBuffUsed;
      if OutBuf = 1 then
        OutBuf := 0;
      BytesTransferred := DataCount - OutBuf;
      if BytesTransferred < 0 then
        BytesTransferred := 0;
      PageLength := PageSize;
    end else begin
      Pages := 0;
      Page := CurrPage;
      BytesTransferred := DataCount;
      PageLength := 0;
    end;
  end;

  function C12AbstractFax.GetLastPageStatus : Boolean;
    {-Return True if last page received OK, false otherwise}
  begin
    GetLastPageStatus := LastPageOk;
  end;

  function C12AbstractFax.GetRemoteID : Str20;
    {-Return remote station ID}
  begin
    GetRemoteID := RemoteID;
  end;

  procedure C12AbstractFax.GetSessionParams(var BPS : LongInt;
                                            var Resolution : Boolean;
                                            var Correction : Boolean);
    {-Return remote or session parameters}
  begin
    BPS := SessionBPS;
    Resolution := SessionRes;
    Correction := SessionECM;
  end;

  function C12AbstractFax.GetHangupResult : Word;
    {-Return last hangup result, class 2 only}
  begin
    GetHangupResult := HangupCode;
  end;

  procedure C12AbstractFax.caHDLCStart(Last : Boolean);
  begin
    with CPort^ do begin
      Delay(FrameWait);
      PutChar(AddrField);
      if Last then
        PutChar(ControlFieldLast)
      else
        PutChar(ControlField);
    end;
  end;

  procedure C12AbstractFax.caHDLCEnd;
  begin
    with CPort^ do begin
      PutChar(cDLE);
      PutChar(cETX);
    end;
  end;

  procedure C12AbstractFax.caPutStandardFrame(Frame : Byte);
    {-Transmit an standard frame of type Frame}
  begin
    with CPort^ do begin
      {Send HDLC address and control fields}
      caHDLCStart(True);

      {Send fax control field, EOP format}
      PutChar(Char(Frame));

      {Send message terminator}
      caHDLCEnd;
    end;
  end;

  procedure C12AbstractFax.caPutTSIFrame;
    {-Transmit a TSI frame}
  var
    I : Word;
  begin
    with CPort^ do begin
      {Send HDLC address and control fields}
      caHDLCStart(False);

      {Send fax control field, TSI format}
      PutChar(Char(TSIFrame or $01));

      {Send TSI data}
      for I := 19 downto Length(StationID) do
        PutChar(' ');
      for I := Length(StationID) downto 1 do
        PutChar(Char(StationID[I]));

      {Send message terminator}
      caHDLCEnd;
    end;
  end;

  procedure C12AbstractFax.caPutDCSDISFrame(UseDIS : Boolean);
    {-Transmit a DCS or DIS frame}
  var
    B : Byte;
  begin
    with CPort^ do begin
      {Send HDLC address and control fields}
      caHDLCStart(True);

      {Send fax control field, DCS format}
      if UseDIS then
        PutChar(Char(DISFrame))
      else
        PutChar(Char(DCSFrame or $01));

      {Send DCS/DIS data, first byte is static}
      PutChar(Char(DISGroup1));

      {Second byte contains resolution and BPS info}
      B := DISGroup3_1;                                                {!!.02}

      {We can receive at high resolution, or send if doc is high res}  {!!.01}
      if UseDIS or (ResC = '1') then                                   {!!.01}
        B := B or DISHighResolution;

      if UseDIS then begin
        case BPSIndex of
          6 :  B := B or DIS9600BPS or DIS14400BPS;
          5 :  B := B or DIS9600BPS or DIS12000BPS;
          3 :  B := B or DIS7200BPS;
          2 :  B := B or DIS4800BPS;
          1 :  B := B or DIS2400BPS;
          else B := B or DIS7200BPS
        end;
      end else begin
        case BPSIndex of
          6 :  B := B or DIS14400BPS;
          5 :  B := B or DIS12000BPS;
          3 :  B := B or DIS7200BPS;
          2 :  B := B or DIS4800BPS;
          1 :  B := B or DIS2400BPS;
          else B := B or DIS9600BPS;
        end;
      end;
      PutChar(Char(B));

      {Note modulation code for training and message transmission}
      if not UseDIS then
        ModCode := ModArray[BPSIndex];

      {Third byte}                                                     {!!.01}
      if UseDIS then                                                   {!!.01}
        B := DISGroup3_2 or ZeroScanTime                               {!!.01}
      else                                                             {!!.01}
        B := DISGroup3_2 or ScanTimeResponse[SessionScan, SessionRes]; {!!.01}

      if UseDIS or ((PageHeader.ImgFlags and ffHighWidth) <> 0) then   {!!.03}
        B := B or DISWideWidth;                                        {!!.03}

      PutChar(Char(B));                                                {!!.01}

      {Last byte is static}
      PutChar(Char(DISGroup3_3));

      {Send message terminator}
      caHDLCEnd;
    end;
  end;

  procedure C12AbstractFax.caPutCSIFrame;
    {-Transmit a CSI frame}
  var
    I : Word;
  begin
    with CPort^ do begin
      {Send HDLC address and control fields}
      caHDLCStart(False);

      {Send fax control field, CSI format}
      PutChar(Char(CSIFrame));

      {Send CSI data}
      for I := 19 downto Length(StationID) do
        PutChar(' ');
      for I := Length(StationID) downto 1 do
        PutChar(Char(StationID[I]));

      {Send message terminator}
      caHDLCEnd;
    end;
  end;

  procedure C12AbstractFax.caPutTCFData;
    {-Put zeros for 1.5 seconds}
  {$IFDEF BadSendTrain}
  const
    BadCount : Word = 0;
    MaxBad = 1;
  {$ENDIF}
  var
    Required : Word;
    I : Word;
  begin
    with CPort^ do begin
      {Assumes free space is available}
      Required := 17 * (SessionBPS div 100);

      {$IFDEF BadSendTrain}
      Inc(BadCount);
      if BadCount <= MaxBad then
        for I := 1 to Required do
          PutChar(#1)
      else
        for I := 1 to Required do
          PutChar(#0);
      {$ELSE}
      for I := 1 to Required do
        PutChar(#0);
      {$ENDIF}
      caHDLCEnd;
    end;
  end;

  procedure C12AbstractFax.caCalcMinBytesPerLine;
    {-Calculate minimum byte per line}
  begin
    MinBytes := ((SessionBPS * ScanTimes[SessionScan, SessionRes]) div 8000);
  end;

  procedure C12AbstractFax.caExtractClass1Params(DIS : Boolean; B2, B3 : Byte);
    {-Extract bps, res, ecm from 2nd and 3rd byte of DCS/DIS}
  begin
    FillChar(RmtMods, SizeOf(RmtMods), False);
    RmtMods[1] := True;
    RmtMods[2] := True;

    if DIS then begin
      RmtMods[6] := B2 and $20 = $20;
      RmtMods[5] := B2 and $10 = $10;
      case B2 and $0C of
        $04 : RmtMods[4] := True;
        $0C : begin
                RmtMods[3] := True;
                RmtMods[4] := True;
              end;
      end;

      {Set SessionBPS from lowest common between local and remote capabilities}
      {and less than or equal to MaxFaxBPS}
      BPSIndex := MaxModIndex+1;
      repeat
        Dec(BPSIndex);
      until (LocalMods[BPsIndex] and RmtMods[BPSIndex]) and
            (Class1BPSArray[BPSIndex] <= MaxFaxBPS);
      SessionBPS := Class1BPSArray[BPSIndex];

    end else begin
      if B2 and $20 = $20 then
        BPSIndex := 6                                                  {!!.02}
      else if B2 and $10 = $10 then
        BPSIndex := 5                                                  {!!.02}
      else begin
        case B2 and $0C of
          $00 : BPSIndex := 1;
          $08 : BPSIndex := 2;
          $04 : BPSIndex := 4;
          else  BPSIndex := 3;
        end;
      end;
      SessionBPS := Class1BPSArray[BPSIndex];
    end;

    {Set resolution and error correction}
    if DIS then                                                        {!!.01}
      SessionRes := ResC = '1'                                         {!!.01}
    else                                                               {!!.01}
      SessionRes := B2 and $40 = $40;

    {!!.03 - added}
    if DIS then
      SessionWid := PageHeader.ImgFlags and ffHighWidth <> 0
    else
      SessionWid := B3 and $03 = $01;

    SessionECM := False;

    {!!.03 - added}
    if DIS then begin
      CanDoHighRes := B2 and $40 = $40;
      CanDoHighWid := B3 and $03 = $01;
    end;

    {Set scan times}
    case B3 and $70 of
      $70 : SessionScan := 0;   {0,0}
      $10 : SessionScan := 1;   {5,5}
      $60 : SessionScan := 2;   {10,5}
      $20 : SessionScan := 3;   {10,10}
      $30 : SessionScan := 4;   {20,10}
      $00 : SessionScan := 5;   {20,20}
      $50 : SessionScan := 6;   {40,20}
      $40 : SessionScan := 7;   {40,40}
    end;
    caCalcMinBytesPerLine;
  end;

  procedure C12AbstractFax.caExtractClass2Params(S : String);
    {-Extract bps, res, ecm from S, as data from FDCS/FDIS}
  begin
    case S[3] of
      '0': SessionBPS := 2400;
      '1': SessionBPS := 4800;
      '2': SessionBPS := 7200;
      '4': SessionBPS := 12000;
      '5': SessionBPS := 14400;
      else SessionBPS := 9600;
    end;

    SessionRes   := S[1] = '1';
    CanDoHighRes := SessionRes;   {!!.03}
    SessionWid   := S[5] = '1';   {!!.03}
    CanDoHighWid := SessionWid;   {!!.03}

    case Response[11] of
      '1', '2' : SessionECM := True;
      else       SessionECM := False;
    end;

    SessionScan := Ord(Response[15])-$30;

    caCalcMinBytesPerLine;
  end;

  function C12AbstractFax.caNextBPS : Boolean;
    {-Return next lower BPS, False if at 2400}
  begin
    repeat
      Dec(BPSIndex);
    until (BPSIndex = 0) or (LocalMods[BPSIndex] and RmtMods[BPSIndex]);
    caNextBPS := BPSIndex <> 0;
    if BPSIndex <> 0 then                                              {!!.03}
      SessionBPS := Class1BPSArray[BPSIndex];                          {!!.03}
    caCalcMinBytesPerLine;
  end;

  procedure C12AbstractFax.caGetClass1Modulations(Transmit : Boolean);
    {-Get this faxmodem's transmit/receive modulation capabilities}
  var
    Finished : Boolean;
    I : Word;
  begin
    {Set defaults}
    FillChar(LocalMods, SizeOf(LocalMods), False);
    LocalMods[1] := True;
    LocalMods[2] := True;
    BPSIndex := 1;                                                     {!!.03}

    if Transmit then
      caPutModem('AT+FTM=?')
    else
      caPutModem('AT+FRM=?');
    NewTimer(ReplyTimer, ReplyWait);
    repeat
      {Collect the response}
      Finished := False;
      Response := '';
      repeat
        Finished := caAddReceivedCmd(Response);
      until Finished or CPort^.ptWaitComplete(ReplyTimer);

      {If this is a modulation response string, parse it}
      if Pos(',', Response) <> 0 then begin
        for I := 3 to 6 do
          LocalMods[I] := Pos(ModArray[I], Response) <> 0;

        {Note highest mod}
        BPSIndex := MaxModIndex;
        while not LocalMods[BPSIndex] do
          Dec(BPSIndex);
      end;

      {!!.01 rewritten}
      {Wait briefly for OK}
      if AsyncStatus = ecTimeout then begin
        AsyncStatus := ecOk;
        Finished := True;
      end else begin
        NewTimer(ReplyTimer, 36);
        Finished := caOkResponse or caErrorResponse;
      end;
    until Finished or CPort^.ptWaitComplete(ReplyTimer);
  end;

  function C12AbstractFax.caProcessFrame(var Retrain : Boolean) : Boolean;
    {-Process the frame in Response}
  var
    I : Word;
  begin
    Retrain := False;

    {Discard till flag byte}
    while (Response[1] <> #255) and (Length(Response) > 1) do
      Delete(Response, 1, 1);

    {If last frame, return True}
    caProcessFrame := Byte(Response[2]) and $10 = $10;

    ReceivedFrame := Ord(Response[3]);

    {Process frame}
    case (ReceivedFrame and $FE )of
      TSIFrame,
      CSIFrame :
        begin
          {Extract remote station ID}
          for I := 1 to 20 do
            RemoteID[I] := Response[24-I];
          RemoteID[0] := #20;
          FaxProgress := fpGotRemoteID;
          ForceStatus := True;
        end;
      DISFrame :
        begin
          {Extract remote parameters}
          caExtractClass1Params(True, Ord(Response[5]), Ord(Response[6]));

          {!!.03 - Added}
          if (not CanDoHighRes and ((PageHeader.ImgFlags and ffHighRes  ) <> 0)) or
             (not CanDoHighWid and ((PageHeader.ImgFlags and ffHighWidth) <> 0)) then
            AsyncStatus := ecIncompatibleMachine
          else begin
            {!!.03 - Moved to new else clause}
            FaxProgress := fpSessionParams;
            ForceStatus := True;
          end;
        end;
      DCSFrame :
        begin
          {Extract session parameters}
          caExtractClass1Params(False, Ord(Response[5]), Ord(Response[6]));

          {Set modulation code}
          ModCode := ModArray[BPSIndex];

          FaxProgress := fpSessionParams;
          ForceStatus := True;
        end;
      CFRFrame :
        GotCFR := True;                                                {!!.02}
      NSFFrame :
        {Nothing to do for NSF frames} ;
      RTNFrame,
      FTTFrame :
        Retrain := True;
      DCNFrame :                                                       {!!.01}
        {Unexpected disconnect request}                                {!!.01}
        AsyncStatus := ecCancelRequested;                              {!!.01}
    end;
    caPrepResponse;
  end;

  {!!.01 new}
  procedure C12AbstractFax.caSwitchBaud(High : Boolean);
    {-Switch baud rates}
  begin
    if High then begin
      {Switch to the high normal baud rate}
      if (InitBaud <> 0) and SlowBaud then begin
        Delay(BaudChangeDelay);
        CPort^.ChangeBaud(NormalBaud);
        SlowBaud := False;
      end;
    end else begin
      {Switch to low initialization baud rate}
      if (InitBaud <> 0) and not SlowBaud then begin
        Delay(BaudChangeDelay);
        CPort^.ChangeBaud(InitBaud);
        SlowBaud := True;
      end;
    end;
  end;

  {$IFDEF UseStreams}
  constructor C12AbstractFax.Load(var S : IdStream);
    {-Load}
  begin
    caRawInit('', nil);

    {Use ancestor load}
    AbstractFax.Load(S);
    if S.PeekStatus <> 0 then begin
      Done;
      Fail;
    end;

    {Load stream data}
    S.Read(ToneDial, SizeOf(ToneDial));
    S.Read(FaxAndData, SizeOf(FaxAndData));
    S.Read(ReplyWait, SizeOf(ReplyWait));
    S.Read(TransWait, SizeOf(TransWait));
    S.Read(StatusWait, SizeOf(StatusWait));
    S.Read(DialWait, SizeOf(DialWait));
    S.Read(MaxFaxBPS, SizeOf(MaxFaxBPS));
    S.Read(AnswerOnRing, SizeOf(AnswerOnRing));
    ModemInit := S.ReadString;
    DialPrefix := S.ReadString;
    if S.PeekStatus <> 0 then begin
      Done;
      Fail;
    end;

    {Load the AbstractPort pointer}
    CPort := S.ReadPointer;
    if CPort = nil then begin
      CPort := AbstractPortPtr(S.GetPtr);
      if S.PeekStatus <> 0 then begin
        Done;
        Fail;
      end;
    end;

    {Allocate buffer}
    if not GetMemCheck(DataBuffer, DataBufferSize) then begin
      CPort^.GotError(ecOutOfMemory);
      Done;
      Fail;
    end;
  end;

  procedure C12AbstractFax.Store(var S : IdStream);
    {-Store}
  begin
    {Ancestor store}
    AbstractFax.Store(S);

    {Store various data}
    S.Write(ToneDial, SizeOf(ToneDial));
    S.Write(FaxAndData, SizeOf(FaxAndData));
    S.Write(ReplyWait, SizeOf(ReplyWait));
    S.Write(TransWait, SizeOf(TransWait));
    S.Write(StatusWait, SizeOf(StatusWait));
    S.Write(DialWait, SizeOf(DialWait));
    S.Write(MaxFaxBPS, SizeOf(MaxFaxBPS));
    S.Write(AnswerOnRing, SizeOf(AnswerOnRing));
    S.WriteString(ModemInit);
    S.WriteString(DialPrefix);

    {Store the AbstractPort pointer}
    S.WriteUserPointer(CPort, ptNil);
    if not S.PointerRegistered(Cport) then
      S.PutPtr(CPort);
  end;
  {$ENDIF}

{C12SendFax}

  procedure C12SendFax.csRawInit;
  begin
    InFileName  := '';
    HeaderLine  := '';
    Title := '';
    Sender := '';
    Recipient := '';
    MaxRetries  := DefMaxRetries;
    DataCount   := 0;
    Sending := True;
    CvrOpen := False;                                                  {!!.02}
  end;

  constructor C12SendFax.Init(ID : Str20; ComPort : AbstractPortPtr);
  begin
    {Validate buffer sizes}
    with ComPort^, PR^ do begin
      if Buffered and (OutBuffLen < 8192) then begin
        GotError(ecBuffersTooSmall);
        Fail;
      end;
    end;

    if not C12AbstractFax.Init(ID, ComPort) then
      Fail;

    New(Converter, Init);
    if Converter = nil then begin
      ComPort^.GotError(ecOutOfMemory);
      Done;
      Fail;
    end;

    csRawInit;
  end;

  destructor C12SendFax.Done;
  begin
    if Converter <> nil then
      Dispose(Converter, Done);
    C12AbstractFax.Done;
  end;

  procedure C12SendFax.SetToneDial(Tone : Boolean);
  begin
    ToneDial := Tone;
  end;

  procedure C12SendFax.SetDialPrefix(P : String);
  begin
    DialPrefix := P;
  end;

  procedure C12SendFax.SetDialTime(DT : Integer);
  begin
    DialWait := DT;
  end;

  procedure C12SendFax.SetHeaderText(S : String);
    {-set HeaderLine to S}
  begin
    HeaderLine := S;
  end;

  procedure C12SendFax.csSendWhiteRasterLines(Count : Word);
    {-Send white raster lines to create a phsyical top margin}
  const
    WhiteLine = #$00#$80#$B2'Y'#$01#$00;
  var
    I, J : Word;
  begin
    for I := 1 to Count do begin
      CPort^.PutString(WhiteLine);
      for J := Length(WhiteLine) to MinBytes do
        CPort^.PutChar(#0);
    end;
  end;

  procedure C12SendFax.SetMaxRetries(MR : Integer);
    {-set MaxRetries to MR}
  begin
    MaxRetries := MR;
  end;

  procedure C12SendFax.csPutBuffer(var Buffer; Len : Word);
  type
    BA = Array[1..$FFF0] of Char;
  var
    I : Word;
  begin
    with CPort^ do
      for I := 1 to Len do begin

        {Quote and send char}
        if BA(Buffer)[I] = cDLE then
          PutChar(cDLE);

        PutChar(BA(Buffer)[I]);
      end;
  end;

  procedure C12SendFax.csPutLineBuffer(var Buffer; Len : Word);
    {Buffer has <lengthword><data> raster line}
  type
    BA = Array[1..$FFF0] of Char;
  var
    I : Word;
    Index : Word;
    LineLen : Word;
  begin
    with CPort^ do begin
      Index := 1;
      repeat

        {Get and transmit one raster line}
        LineLen := Ord(BA(Buffer)[Index]);
        LineLen := LineLen + (Ord(BA(Buffer)[Index+1]) shl 8);
        Inc(Index, 2);
        for I := Index to (Index+LineLen-1) do begin
          {Quote and send char}
          if BA(Buffer)[I] = cDLE then
            PutChar(cDLE);
          PutChar(BA(Buffer)[I]);
        end;

        {Assure line has minimum number of bytes}
        if LineLen < MinBytes then begin
          for I := 1 to (MinBytes-LineLen) do
            PutChar(#0);
          Inc(DataCount, MinBytes);
        end else
          Inc(DataCount, LineLen);

        Inc(Index, LineLen);
      until (Index >= Len);
    end;
  end;

  function C12SendFax.csOpenFaxFile(Document : PathStr) : Word;
  var
    W : Word;
    S : String[6];
  begin
    InFileName := ForceExtension(Document, FaxFileExt);
    SaveMode := FileMode;                                              {!!.02}
    FileMode := AproFileMode;                                   {!!.02}{!!.03}
    Assign(InFile, InFileName);
    Reset(InFile, 1);
    FileMode := SaveMode;                                              {!!.02}
    W := IOResult;
    if W <> 0 then begin
      csOpenFaxFile := W;
      Exit;
    end;

    BlockRead(InFile, FaxHeader, SizeOf(FaxHeaderRec));
    W := IOResult;
    if W <> 0 then begin
      Close(InFile);
      if IOResult = 0 then ;
      csOpenFaxFile := W;
      Exit;
    end;

    Move(FaxHeader, S[1], 6);
    S[0] := #6;
    if S <> DefSig then begin
      csOpenFaxFile := ecFaxBadFormat;
      Close(InFile);
      if IoResult <> 0 then ;
    end else
      csOpenFaxFile := ecOK;
  end;

  function C12SendFax.csAdvancePage : Boolean;
    {-Advance CurrPage and read new page, return False on any error}
  begin
    {Assume failure}
    csAdvancePage := False;

    {Set page number}
    if SendingCover then begin
      if CoverIsAPF then begin
        Close(InFile);
        AsyncStatus := csOpenFaxFile(FaxFileName);                     {!!.03}
        if AsyncStatus <> ecOK then begin
          CPort^.GotError(AsyncStatus);
          Exit;
        end;
      end else begin
        Close(CvrF);
        if IoResult <> 0 then ;
        CvrOpen := False;                                              {!!.02}
      end;
      SendingCover := False;
      CoverFile := '';
    end else begin                                                     {!!.01}
      Inc(CurrPage);
      DataCount := 0;                                                  {!!.01}
    end;                                                               {!!.01}

    {Seek to page}
    AsyncStatus := caLocatePage(CurrPage);                             {!!.02}
    if AsyncStatus <> 0 then begin                                     {!!.02}
      CPort^.GotError(AsyncStatus);                                    {!!.02}
      Exit;                                                            {!!.02}
    end;                                                               {!!.02}

    {Read in the page header}
    BlockRead(InFile, PageHeader, SizeOf(PageHeaderRec));
    UseLengthWord := FlagIsSet(PageHeader.ImgFlags, ffLengthWords);
    AsyncStatus := IOResult;
    if AsyncStatus <> ecOk then begin
      CPort^.GotError(AsyncStatus);
      Exit;
    end;

    {Build FDIS command params}
    if (PageHeader.ImgFlags and ffHighRes) <> 0 then
      ResC := '1'
    else
      ResC := '0';

    csAdvancePage := True;
  end;

  {!!.03}
  function C12SendFax.csOpenCoverPage : Boolean;
    {-Open the cover file}
  begin
    csOpenCoverPage := False;
    if CoverIsAPF then begin
      {Open as an APF file}
      Close(InFile);
      if IOResult <> 0 then ;
      AsyncStatus := csOpenFaxFile(CoverFile);
      if AsyncStatus <> ecOk then begin
        CPort^.GotError(AsyncStatus);
        Exit;
      end;

      {Goto the first page}
      AsyncStatus := caLocatePage(1);
      if AsyncStatus <> ecOk then begin
        CPort^.GotError(AsyncStatus);
        Exit;
      end;

      {Read in the page header}
      BlockRead(InFile, PageHeader, SizeOf(PageHeaderRec));
      UseLengthWord := FlagIsSet(PageHeader.ImgFlags, ffLengthWords);
      AsyncStatus := IOResult;
      if AsyncStatus <> ecOk then begin
        CPort^.GotError(AsyncStatus);
        Exit;
      end;

    end else begin
      {Open as text file}
      SaveMode := FileMode;
      FileMode := AproFileMode;
      Assign(CvrF, CoverFile);
      Reset(CvrF);
      FileMode := SaveMode;
      AsyncStatus := IoResult;
      if AsyncStatus <> 0 then begin
        CPort^.GotError(AsyncStatus);
        Exit;
      end else begin
        CvrOpen := True;
        {Select font for cover page text}
        if not Converter^.LoadFont(StandardFont, (ResC = '1')) then begin
          CPort^.GotError(ecFaxNoFontFile);
          Exit;
        end;
      end;
    end;

    {Success}
    csOpenCoverPage := True;
  end;

  function C12SendFax.FaxTransmitPart : FaxStateType;
  label
    ExitPoint;
  const
    OldState : SendStates = tfDone;
  var
    Finished : Boolean;
    GotResponse : Boolean;
    Retrain : Boolean;
    C : Char;
    FaxWid : Char; {!!.03}
    Count : Word;
    Result : Word;
    I : Word;
    S : String;

    procedure CheckResponse;
      {-Check for text responses, check for and process HDLC frames}
    begin
      with CPort^ do begin
        {Collect chars till CR/LF or DLE/ETX}
        Finished := False;
        GotResponse := False;
        while CharReady and not Finished do begin
          GetChar(C);
          Response := Response + C;
          if CheckForString(CRLFIndex, C, ^M^J, False) then begin
            Response := Trim(Response);
            if Response <> '' then begin
              {Got a text response}
              Finished := True;
              GotResponse := True;

              {Most error responses are aborts}
              if caErrorResponse then begin
                case State of
                  tf1WaitEOP,                                          {!!.02}
                  tf1WaitMCF,                                          {!!.02}
                  tf1SendEOP : {Let state machine handle}
                  else begin
                    {It's an error}
                    GotError(ecUnknownModemResult);
                    State := tfAbort;
                  end;
                end;
              end else if caFHNGResponse and (State <> tf2WaitFET) then begin
                {Unexpected FHNGs are also aborts}
                GotError(ecFaxSessionError);
                HangupCode := caExtractFHNGCode;
                State := tfAbort;
              end;
            end;
          end else if CheckForString(EtxIndex, C, #16#3, False) then begin
            {An HDLC frame, process it now}
            LastFrame := caProcessFrame(Retrain);

            {Abort if we got a DCN frame - or if machine does not have the}
            {capability to receive the document we're sending}
            if (AsyncStatus = ecCancelRequested) or                    {!!.03}
               (AsyncStatus = ecIncompatibleMachine) then begin        {!!.03}
              GotError(AsyncStatus);                                   {!!.01}
              State := tfAbort;                                        {!!.01}
              GotResponse := True;                                     {!!.01}
            end;                                                       {!!.01}

            {If this is a retrain request change the current state}
            if Retrain then begin
              if caNextBPS then begin
                caPutFrameT;
                State := tf1TSIResponse;
              end else begin
                GotError(ecFaxTrainError);
                State := tfAbort;
              end;
            end;
          end;
        end;
      end;
    end;

    function ReadNextLine : Word;
      {-Read the next wordlength raster line}
    var
      Len : Word;
    begin
      BlockRead(InFile, Len, 2, BytesRead);
      if Len > DataBufferSize then
        Len := DataBufferSize;
      BlockRead(InFile, DataBuffer^, Len, BytesRead);
      ReadNextLine := IoResult;
      if Len < MinBytes then begin
        FillChar(DataBuffer^[Len], MinBytes-BytesRead, 0);
        BytesRead := MinBytes;
      end;
      Inc(DataCount, Len+2);
    end;

    procedure ForceHangup;
    begin
      if CPort^.CheckDCD then begin
        Delay(AbortDelay);
        CPort^.PutString('+++');
        Delay(AbortDelay);
        caPutModem('ATH0');
        Delay(AbortDelay);                                             {!!.02}
        CPort^.FlushInBuffer;                                          {!!.02}

        {If DCD still set then toggle DTR}
        if CPort^.CheckDCD then
          caFlushModem;
      end else
        {Just make sure we're back on hook}
        caPutModem('ATH0');
    end;

  begin
    {Restore AsyncStatus in case something outside state machine changed it}
    AsyncStatus := SaveStatus;

    {Check for user abort request}
    if AsyncStatus <> ecUserAbort then
      if afHandleAbort then begin
        CPort^.FlushOutbuffer;
        CPort^.PutString(cDLE+cETX);
        if (ClassInUse = ctClass2) and
           (State > tf2Connect) and (State < tfClose) then
            {Send abort string but don't worry about response}
            CPort^.PutString('AT+FK'^M);
        {Set status back to ecUserAbort}
        AsyncStatus := ecUserAbort;
        State := tfAbort;
      end;

    {Show status periodically and after major events}
    if TimerExpired(StatusTimer) or ForceStatus then begin
      ForceStatus := False;
      NewTimer(StatusTimer, StatusWait);
      FaxStatus(False, False);
    end;

    {Preprocess pending modem responses}
    case State of
      tfGetEntry,
      tfInit,
      tfDial,
      tfRetryWait,
      tfOpenCover,
      tfSendCover,
      tfWaitXon,
      tfWaitFreeHeader,
      tfSendPageHeader,
      tfPrepPage,
      tfSendPage,
      tfDrainPage,
      tf1PrepareEOP,
      tf2SendEOP,
      tf2NextPage,
      tfClose,
      tfCompleteOK,
      tfAbort : {Don't preprocess these states} ;

      else begin
        {Preprocess all other states, check for text responses and HDLC frames}
        CheckResponse;

        {Check for timeouts}
        if TimerExpired(ReplyTimer) then begin
          case State of
            tf1WaitMCF :
              begin
                CPort^.PutChar(' ');
                State := tf1PrepareEOP;
              end;
            tf1WaitHangup :
              {Timeout waiting for normal hangup, force hangup and close OK}
              begin
                ForceHangup;
                State := tfClose;
              end;
            else begin
              CPort^.GotError(ecTimeout);
              State := tfAbort;
            end;
          end;
        end else begin
          {Skip state machine if we don't have a response yet}
          if not GotResponse then
            goto ExitPoint;
        end;
      end;
    end;

    case State of
      tfGetEntry :
        if NextFax(PhoneNum, FaxFileName, CoverFile) then begin

          {Log transmit started}
          LogFax(PhoneNum, FaxFileName, lfaxTransmitStart);

          {Prepare for transmit}
          ConnectCnt := 0;
          SendingCover := False;
          CoverIsAPF := False;
          PageCount := 0;
          CoverCount := 0;
          CurrPage := 1;
          Retries := 0;
          HangupCode := 0;

          {Open fax file}
          if FaxFileName <> '' then begin
            AsyncStatus := csOpenFaxFile(FaxFileName);
            if AsyncStatus <> ecOK then begin
              CPort^.GotError(AsyncStatus);
              State := tfAbort;
              goto ExitPoint;
            end;

            {Verify it's one of ours}
            Move(FaxHeader, S[1], 6);
            S[0] := #6;
            if S <> DefSig then begin
              CPort^.GotError(ecFaxBadFormat);
              State := tfAbort;
              goto ExitPoint;
            end;

            {Note page count}
            PageCount := (FaxHeader.PageCount);


          end else if CoverFile <> '' then begin
            {Sending just the cover}
            PageCount := 0;
          end else begin
            {Error, no file to send}
            CPort^.GotError(ecNoFilename);
            State := tfAbort;
            goto ExitPoint;
          end;

          State := tfInit;
          FaxProgress := fpInitModem;

          {$IFDEF UseSWFlow}
          {Make sure flow control is off to start}
          if not FlagIsSet(afFlags, afNoSoftwareFlow) then
            CPort^.SWFlowDisable;
          {$ENDIF}
        end else
          State := tfCompleteOK;

      tfInit :
        begin
          {Make sure station ID is all uppercase}
          StationID := StUpcase(StationID);

          {!!.03 moved up}
          {Note type of cover page}
          if CoverFile <> '' then begin
            CoverCount := 1;
            SendingCover := True;
            CoverIsAPF := Pos('.'+FaxFileExt, CoverFile) <> 0;
          end;

          {Read first page header}
          if FaxFileName <> '' then begin
            AsyncStatus := caLocatePage(CurrPage);
            if AsyncStatus <> 0 then begin
              CPort^.GotError(AsyncStatus);
              State := tfAbort;
              goto ExitPoint;
            end;

            BlockRead(InFile, PageHeader, SizeOf(PageHeaderRec));
            UseLengthWord := FlagIsSet(PageHeader.ImgFlags, ffLengthWords);
            AsyncStatus := IOResult;
            if AsyncStatus <> ecOk then begin
              CPort^.GotError(AsyncStatus);
              State := tfAbort;
              goto ExitPoint;
            end;

            {Set resolution character for DIS/DCS commands/frames}
            if (PageHeader.ImgFlags and ffHighRes) <> 0 then
              ResC := '1'
            else
              ResC := '0';
          end else begin                                               {!!.03}
            {Just sending cover, assume low res}
            ResC := '0';
            {If cover is APF, open to get its res}                     {!!.03}
            if CoverIsAPF then begin                                   {!!.03}
              if not csOpenCoverPage then                              {!!.03}
                goto ExitPoint;                                        {!!.03}
              if (PageHeader.ImgFlags and ffHighRes) <> 0 then         {!!.03}
                ResC := '1'                                            {!!.03}
              else                                                     {!!.03}
                ResC := '0';                                           {!!.03}
              Close(InFile);                                           {!!.03}
              if IoResult <> 0 then ;                                  {!!.03}
            end;                                                       {!!.03}
          end;                                                         {!!.03}

          {Say we're initializing}
          FaxProgress := fpInitModem;
          ForceStatus := True;

          {Select next state for Class1 or Class2 modems}
          if ClassInUse = ctClass1 then begin
            State := tf1Init1;
            caPutModem('AT+FCLASS=1');
          end else begin
            caPutModem('AT+FCLASS=2');
            State := tf2Init1;
          end;
        end;

      tf1Init1 :
        if caOkResponse then begin
          caSwitchBaud(True);                                          {!!.02}
          State := tfDial;
        end else begin
          CPort^.GotError(ecFaxInitError);
          State := tfAbort;
        end;

      tf2Init1 :
        if caOkResponse then begin
          caPutModem('AT+FLID="'+StationID+'"');
          State := tf2Init2;
        end else begin
          CPort^.GotError(ecFaxInitError);
          State := tfAbort;
        end;

      tf2Init2 :
        if caOkResponse then begin
          if ((PageHeader.ImgFlags and ffHighWidth) <> 0) then  {!!.03}
            FaxWid := '1'                                       {!!.03}
          else                                                  {!!.03}
            FaxWid := '0';                                      {!!.03}

          caPutModem('AT+FDIS='+ResC+','+
                              caSpeedCode+','+                  {!!.03}
                              FaxWid+                           {!!.03}
                              ',2,0,'+                          {!!.03}
                              CheckChar+
                              ',0,0');
          State := tf2Init3;
        end else begin
          CPort^.GotError(ecFaxInitError);
          State := tfAbort;
        end;

      tf2Init3 :
        if caOkResponse then
          State := tfDial
        else begin
          CPort^.GotError(ecFaxInitError);
          State := tfAbort;
        end;

      tfDial:
        begin
          {Build modem dial string}
          if ToneDial then
            S := 'ATDT'
          else
            S := 'ATDP';
          S := S + DialPrefix + PhoneNum;
          caPutModem(S);
          NewTimer(ReplyTimer, DialWait);
          FaxProgress := fpDialing;
          ForceStatus := True;
          if ClassInUse = ctClass1 then
            State := tf1Connect
          else
            State := tf2Connect;
        end;

      tfRetryWait :
        begin
          FaxProgress := fpBusyWait;
          if TimerExpired(ReplyTimer) then
            State := tfDial;
        end;

      tf1Connect :
        if caConnectResponse then
          caPrepResponse
        else if caOkResponse then begin
          if LastFrame then begin
            {Last frame was processed, prepare to transmit TSI}
            Delay(ExtraCommandDelay);
            caPutFrameT;
            LastFrame := False;
            State := tf1SendTSI;
          end else begin
            {Ask for another frame}
            caPutFrameR;
          end;
        end else if (Pos('BUSY', Response) <> 0) or
                    (Pos('ANSWER', Response) <> 0) then begin
          Inc(ConnectCnt);
          if (ConnectCnt < MaxConnect) or (MaxConnect = 0) then begin
            {No connect, delay and retry}
            FaxProgress := fpBusyWait;
            ForceStatus := True;
            State := tfRetryWait;
            NewTimer(ReplyTimer, RetryWait);
          end else begin
            {Too many failed connect attempts, report to status}
            FaxStatus(False, False);

            {Abort or keep going?}
            if FlagIsSet(afFlags, afAbortNoConnect) then begin
              CPort^.GotError(ecFaxBusy);
              State := tfAbort;
            end else begin
              {Get next fax entry}
              Close(InFile);
              if IOResult = 0 then ;
              CPort^.GotError(ecFaxBusy);
              LogFax(PhoneNum, FaxFileName, lfaxTransmitFail);
              AsyncStatus := ecOk;
              FaxProgress := fpInitModem;
              State := tfGetEntry;
            end;
          end
        end else if Pos('VOICE', Response) > 0 then begin
          {Modem is receiving a voice call}
          CPort^.GotError(ecFaxVoiceCall);
          State := tfAbort;
        end else if Pos('NO DIAL', Response) > 0 then begin            {!!.02}
          {No dialtone when trying to dial}                            {!!.02}
          CPort^.GotError(ecNoDialTone);                               {!!.02}
          State := tfAbort;                                            {!!.02}
        end else if caNoCarrierResponse then begin                     {!!.02}
          {No carrier when trying to call}                             {!!.02}
          CPort^.GotError(ecNoCarrier);                                {!!.02}
          State := tfAbort;                                            {!!.02}
        end else
          {Must have been the command echo, discard it and keep going}
          caPrepResponse;

      tf1SendTSI :
        if caConnectResponse then begin
          caPutTSIFrame;
          caPrepResponse;
          State := tf1TSIResponse;
        end else
          caPrepResponse;

      tf1TSIResponse :
        if caConnectResponse then begin
          caPutDCSDISFrame(False);
          caPrepResponse;
          State := tf1DCSResponse;
        end else
          caPrepResponse;

      tf1DCSResponse :
        if caOkResponse then begin
          {Start training}
          caPutFaxCommand('AT+FTM='+ModCode);
          State := tf1TrainStart;
        end else
          caPrepResponse;

      tf1TrainStart :
        if caConnectResponse then begin
          {Send training data}
          {$IFDEF UseSWFlow}
          if not FlagIsSet(afFlags, afNoSoftwareFlow) then
            with CPort^, PR^ do
              SWFlowEnableOpt(0, 0, sfTransmitFlow);
          {$ENDIF}
          caPutTCFData;
          caPrepResponse;
          State := tf1TrainFinish;
        end else
          caPrepResponse;

      tf1TrainFinish :
        if caOkResponse then begin
          {$IFDEF UseSWFlow}
          {Ask for CFR frame}
          if not FlagIsSet(afFlags, afNoSoftwareFlow) then
            CPort^.SWFlowDisable;
          {$ENDIF}
          Delay(ExtraCommandDelay);
          caPutFrameR;
          State := tf1WaitCFR;
          GotCFR := False;                                             {!!.02}
        end;

      {!!.02 modified}
      tf1WaitCFR :
        if caOkResponse then begin
          if LastFrame then begin
            {Send carrier}
            if GotCFR then begin
              Delay(ExtraCommandDelay);
              caPutFaxCommand('AT+FTM='+ModCode);
              LastFrame := False;
              State := tf1WaitPageConnect;
            end else begin
              {Must be a re-negotiate, restart from DCS again}
              Delay(ExtraCommandDelay);
              caPutFrameT;
              State := tf1TSIResponse;
            end;
          end else begin
            {Ask for another frame}
            caPutFrameR;
            caPrepResponse;
          end;
        end else
          caPrepResponse;

      tf1WaitPageConnect :
        if caConnectResponse then begin
          State := tfWaitXon;
          NewTimer(ReplyTimer, 18);
        end else
          caPrepResponse;

      tf2Connect :
        if Pos('+FCON', Response) <> 0 then begin
          NewTimer(ReplyTimer, ReplyWait);
          Response := '';
          State := tf2GetParams;
        end else if (Pos('BUSY', Response) <> 0) or
                    (Pos('ANSWER', Response) <> 0) then begin
          Inc(ConnectCnt);
          if (ConnectCnt < MaxConnect) or (MaxConnect = 0) then begin
            {No connect, delay and retry}
            FaxProgress := fpBusyWait;
            ForceStatus := True;
            State := tfRetryWait;
            NewTimer(ReplyTimer, RetryWait);
          end else begin
            {Too many failed connect attempts, report to status}
            FaxStatus(False, False);

            {Abort or keep going?}
            if FlagIsSet(afFlags, afAbortNoConnect) then begin
              CPort^.GotError(ecFaxBusy);
              ForceStatus := True;
              State := tfAbort;
            end else begin
              {Get next fax entry}
              Close(InFile);
              if IOResult = 0 then ;
              CPort^.GotError(ecFaxBusy);
              LogFax(PhoneNum, FaxFileName, lfaxTransmitFail);
              AsyncStatus := ecOk;
              State := tfGetEntry;
            end;
          end
        end else if Pos('VOICE', Response) > 0 then begin
          {Modem is receiving a voice call}
          CPort^.GotError(ecFaxVoiceCall);
          State := tfAbort;
        end else if Pos('NO DIAL', Response) > 0 then begin            {!!.02}
          {No dialtone when trying to dial}                            {!!.02}
          CPort^.GotError(ecNoDialTone);                               {!!.02}
          State := tfAbort;                                            {!!.02}
        end else if caNoCarrierResponse then begin                     {!!.02}
          {No carrier when trying to call}                             {!!.02}
          CPort^.GotError(ecNoCarrier);                                {!!.02}
          State := tfAbort;                                            {!!.02}
        end else
          {Unknown but probably acceptable response, just ignore it}
          caPrepResponse;

      tf2GetParams :
        if Pos('+FDIS', Response) <> 0 then begin
          StripPrefix(Response);
          caExtractClass2Params(Response);

          {!!.03 - added}
          if (((PageHeader.ImgFlags and ffHighWidth) <> 0) and not CanDoHighWid) or
             (((PageHeader.ImgFlags and ffHighRes  ) <> 0) and not CanDoHighRes) then begin
            AsyncStatus := ecIncompatibleMachine;
            State := tfAbort;
          end else begin
            FaxProgress := fpSessionParams;
            ForceStatus := True;
            caPrepResponse;
          end;
        end else if Pos('FCSI', Response) <> 0 then begin
          StripPrefix(Response);
          RemoteID := TrimStationID(Response);
          FaxProgress := fpGotRemoteID;
          ForceStatus := True;
          caPrepResponse;
        end else if (Pos('+FDCS', Response) > 0) then begin
          StripPrefix(Response);
          caExtractClass2Params(Response);
          FaxProgress := fpSessionParams;
          ForceStatus := True;
          caPrepResponse;
        end else if caOkResponse then
          caPutModem('AT+FDT')
          {Stay in this state to handle the response}
        else if caConnectResponse then begin
          State := tfWaitXon;
          NewTimer(ReplyTimer, 18);
        end else                                                       {!!.01}
          caPrepResponse;                                              {!!.01}

      tfWaitXon :
        with CPort^ do begin
          if CharReady then
            GetChar(C)
          else
            C := #0;
          if (C = cXOn) or TimerExpired(ReplyTimer) then begin
            State := tfWaitFreeHeader;
            NewTimer(ReplyTimer, TransWait);
          end;
        end;

      tfWaitFreeHeader :
        begin
          {$IFDEF UseSWFlow}
          {Finished with handshaking, turn on flow control}
          if not FlagIsSet(afFlags, afNoSoftwareFlow) then
            CPort^.SWFlowEnableOpt(0, 0, sfTransmitFlow);
          {$ENDIF}

          {Say we're now sending page}
          FaxProgress := fpSendPage;

          {Wait for buffer free space}
          if CPort^.OutBuffFree >= MaxData then
            State := tfSendPageHeader
          else if TimerExpired(ReplyTimer) then begin                  {!!.02}
            CPort^.GotError(ecTimeout);                                {!!.02}
            State := tfAbort;
          end;                                                         {!!.02}
        end;

      tfSendPageHeader :
        begin
          {DataCount := 0;}                                            {!!.01}

          {Select font for title line}
          if not Converter^.LoadFont(SmallFont, (ResC = '1')) then begin
            CPort^.GotError(ecFaxNoFontFile);
            State := tfAbort;
            goto ExitPoint;
          end;

          {Convert and send header line}
          if HeaderLine <> '' then begin
            {!!.01 change start}
            S := afConvertHeaderString(HeaderLine);

            {Force a small top margin}
            csSendWhiteRasterLines(PhysicalTopMargin);

            {Add some zeros to give state machine caller some breathing room}
            Count := CPort^.OutBuffUsed;                               {!!.02}
            for I := Count to BufferMinimum do                         {!!.02}
              CPort^.PutChar(#0);                                      {!!.02}

            {Send all raster rows in header}
            for I := 1 to Converter^.FontRec.Height do begin
              Converter^.acCompressStringRow(S, I, DataBuffer^,
                                             BytesRead, True);

              {Send raster row header}
              csPutLineBuffer(DataBuffer^, BytesRead);
            end;
            {!!.01 change end}

            {Force the first disk read}
            BytesRead := 1;
          end else
            csSendWhiteRasterLines(PhysicalTopMargin);

          {Add some zeros to give state machine caller some breathing room}
          Count := CPort^.OutBuffUsed;
          for I := Count to BufferMinimum do
            CPort^.PutChar(#0);

          if CoverFile = '' then
            State := tfPrepPage
          else
            State := tfOpenCover;
        end;

      {!!.03 rewritten}
      tfOpenCover:
        begin
          DataCount := 0;
          if not csOpenCoverPage then
            goto ExitPoint;

          if CoverIsAPF then
            State := tfPrepPage
          else begin
            State := tfSendCover;
            NewTimer(ReplyTimer, TransWait);
            FaxProgress := fpSendPage;
          end;
        end;

      tfSendCover:
        if CPort^.OutBuffFree >= DataBufferSize then begin
          if not Eof(CvrF) then begin
            ReadLn(CvrF, S);
            AsyncStatus := IoResult;
            if AsyncStatus <> ecOk then begin
              CPort^.GotError(AsyncStatus);
              State := tfAbort;
              goto ExitPoint;
            end;
            S := afConvertHeaderString(S);                             {!!.01}
            for I := 1 to Converter^.FontRec.Height do begin           {!!.01}
              Converter^.acCompressStringRow(S, I, DataBuffer^,        {!!.01}
                                             BytesRead, True);         {!!.01}
              csPutLineBuffer(DataBuffer^, BytesRead);                 {!!.01}
            end;                                                       {!!.01}
          end else begin
            Converter^.acMakeEndOfPage(DataBuffer^, BytesRead);
            csPutBuffer(DataBuffer^, BytesRead);
            for I := BytesRead to MinBytes do
              CPort^.PutChar(#0);
            if ClassInUse = ctClass1 then begin
              caHDLCEnd;
              FaxProgress := fpSendPage;
            end;
            State := tfDrainPage;
          end;
          FaxProgress := fpSendPage;
          ForceStatus := True;
          NewTimer(ReplyTimer, TransWait);
        end else if TimerExpired(ReplyTimer) then begin
          CPort^.GotError(ecBufferIsFull);
          ForceStatus := True;
          State := tfAbort;
          goto ExitPoint;
        end;

      tfPrepPage :
        begin
          {Position at the start of data}
          DataCount := 0;
          PageSize := PageHeader.ImgLength;
          State := tfSendPage;
          ForceStatus := True;
          NewTimer(ReplyTimer, TransWait);
        end;

      tfSendPage :
        if (CPort^.OutBuffFree >= DataBufferSize) then begin
          Count := 0;
          while (Count < MaxSendCount) and
                (CPort^.OutBuffUsed <= BufferMinimum) and
                (CPort^.OutBuffFree >= DataBufferSize) do begin
            Inc(Count);                                                {!!.02}
            if DataCount < PageSize then begin
              {Get next block of data to send}
              if UseLengthWord then
                {Reads line and increments DataCount}
                AsyncStatus := ReadNextLine
              else begin
                BlockRead(InFile, DataBuffer^, DataBufferSize, BytesRead);
                Inc(DataCount, BytesRead);
                AsyncStatus := IoResult;
              end;

              if AsyncStatus <> ecOk then begin
                CPort^.GotError(AsyncStatus);
                State := tfAbort;
                goto ExitPoint;
              end;

              {Send data}
              csPutBuffer(DataBuffer^, BytesRead);

              {Wait for enough space to send next buffer full}
              NewTimer(ReplyTimer, TransWait);

            end else begin
              {End of this page, add Class 1 termination}
              if ClassInUse = ctClass1 then begin
                caHDLCEnd;
                FaxProgress := fpSendPage;
              end;

              {Wait for page to drain}
              State := tfDrainPage;
              NewTimer(ReplyTimer, TransWait);

              {Force exit from while loop}
              Count := MaxSendCount + 1;
            end;
          end;
        end else if TimerExpired(ReplyTimer) then begin
          CPort^.GotError(ecBufferIsFull);
          ForceStatus := True;
          State := tfAbort;
          goto ExitPoint;
        end;

      tfDrainPage :
        if CPort^.OutBuffUsed <= 1 then begin

          {$IFDEF UseSWFlow}
          {Finished with data, turn off flow control}
          if not FlagIsSet(afFlags, afNoSoftwareFlow) then
            CPort^.SWFlowDisable;
          {$ENDIF}

          if ClassInUse = ctClass1 then
            State := tf1PageEnd
          else
            State := tf2SendEOP;

          {Force status one more time so user's display shows 100%}
          ForceStatus := True;
          FaxProgress := fpSendPage;
        end else if TimerExpired(ReplyTimer) then begin
          CPort^.GotError(ecTimeout);
          ForceStatus := True;
          State := tfAbort;
        end;

      tf1PageEnd :
        if caOkResponse then begin
          Retry := 0;
          State := tf1PrepareEOP;
          FaxProgress := fpSendPageStatus;
        end else
          caPrepResponse;

      tf1PrepareEOP :
        begin
          Inc(Retry);
          if Retry > 3 then begin
            CPort^.GotError(ecTimeout);
            ForceStatus := True;
            State := tfAbort;
          end else begin
            Delay(PreEOPDelay);
            caPutFrameT;
            State := tf1SendEOP;
          end;
        end;

      tf1SendEOP :
        if caConnectResponse then begin
          if (FaxFileName <> '') and
             ((CurrPage < FaxHeader.PageCount) or SendingCover) then
            caPutStandardFrame(MPSFrame or $01)
          else
            {Send end of page frame}
            caPutStandardFrame(EOPFrame or $01);

          caPrepResponse;
          State := tf1WaitEOP;
        end else
          caPrepResponse;

      tf1WaitEOP :
        if caOkResponse then begin
          {Ask for comfirmation}
          Delay(ExtraCommandDelay);
          caPutFrameR;
          State := tf1WaitMCF;
          NewTimer(ReplyTimer, 54);
          MCFConnect := False;
        end else
          caPrepResponse;

      tf1WaitMCF :
        if caConnectResponse then begin
          MCFConnect := True;
          caPrepResponse;
        end else if caErrorResponse then begin
          State := tf1PrepareEOP;
        end else if caOkResponse then begin
          if LastFrame then begin
            LastFrame := False;
            case ReceivedFrame of
              RTPFrame :
                if (CurrPage < FaxHeader.PageCount) or SendingCover then begin
                  {Advance to next page}
                  if not csAdvancePage then begin
                    State := tfAbort;
                    goto ExitPoint;
                  end;
                  caPutFrameT;
                  State := tf1TSIResponse;
                end else begin
                  {No more pages}
                  caPutFrameT;
                  State := tf1SendDCN;
                end;
              RTNFrame :
                begin                                                  {!!.02}
                  CPort^.GotError(ecFaxSessionError);                  {!!.02}
                  State := tfAbort;
                end;                                                   {!!.02}
              MCFFrame :
                if (CurrPage < FaxHeader.PageCount) or                 {!!.03}
                   (SendingCover and (FaxFileName <> '')) then begin   {!!.03}
                  {Advance to next page}
                  if not csAdvancePage then begin
                    State := tfAbort;
                    goto ExitPoint;
                  end;
                  caPutFaxCommand('AT+FTM='+ModCode);
                  State := tf1WaitPageConnect;
                end else begin
                  {No more pages}
                  caPutFrameT;
                  State := tf1SendDCN;
                end;
            end;
          end else begin
            {Ask for another frame}
            caPutFrameR;
          end;
        end else
          caPrepResponse;

      tf1SendDCN :
        if caConnectResponse then begin
          caPutStandardFrame(DCNFrame or $01);
          caPrepResponse;
          State := tf1Hangup;
        end else
          caPrepResponse;

      tf1HangUp :
        if caOkResponse then begin
          {Hangup}
          caPutModem('ATH0');

          {Special short (2 seconds) wait for hangup result}
          NewTimer(ReplyTimer, 36);
          State := tf1WaitHangup;
        end else
          caPrepResponse;

      tf1WaitHangup :
        if caOkResponse then
          State := tfClose
        else
          caPrepResponse;

      tf2SendEOP :
        begin
          CPort^.PutChar(cDLE);
          CPort^.PutChar(cETX);
          State := tf2WaitFPTS;
          Response := '';
          NewTimer(ReplyTimer, TransWait);
          FaxProgress := fpSendPageStatus;
          ForceStatus := True;
        end;

      tf2WaitFPTS:
        if caOkResponse then begin
          if (FaxFileName <> '') and                                   {!!.02}
             ((SendingCover) or (CurrPage < FaxHeader.PageCount)) then {!!.02}
            S := 'AT+FET=0'
           else
            S := 'AT+FET=2';
          caPutModem(S);
          State := tf2WaitFET;
          NewTimer(ReplyTimer, ReplyWait);
        end else begin                                                 {!!.02}
          CPort^.GotError(ecFaxSessionError);                          {!!.02}
          State := tfAbort;
        end;                                                           {!!.02}

      tf2WaitFET :
        if caOkResponse then
          {Got OK from FPTS:1}
          State := tf2NextPage
        else if caFHNGResponse then begin
          HangupCode := caExtractFHNGCode;
          caPrepResponse;
        end else if Pos('FPTS', Response) <> 0 then begin
          StripPrefix(Response);
          RetryPage := False;                                          {!!.02}
          case Response[1] of
            '1', '3': {page good}                                      {!!.02}
              begin
                FaxProgress := fpPageOK;
                ForceStatus := True;
                caPrepResponse;
                {Stay in this state for OK}
              end;

            '2' : {page bad, retry requested}                          {!!.02}
              begin                                                    {!!.02}
                RetryPage := True;                                     {!!.02}
                FaxProgress := fpPageError;                            {!!.02}
                State := tf2WaitPageOK;                                {!!.02}
                ForceStatus := True;                                   {!!.02}
                caPrepResponse;                                        {!!.02}
              end;                                                     {!!.02}

            '4', '5' :  {page good/bad, interrupt requested}
              begin
                CPort^.GotError(ecCancelRequested);
                ForceStatus := True;
                State := tfAbort;
              end;
            else begin
              CPort^.GotError(ecFaxPageError);                         {!!.02}
              ForceStatus := True;
              State := tfAbort;
            end;
          end;
        end else begin
          CPort^.GotError(ecFaxPageError);
          ForceStatus := True;
          State := tfAbort;
        end;

      {!!.02 modified}
      tf2WaitPageOK :
        if caOkResponse then begin
          Inc(Retries);
          if Retries > MaxRetries then begin
            {Abort}
            CPort^.PutString('AT+FK');
            CPort^.GotError(ecFaxSessionError);
            ForceStatus := True;
            State := tfAbort;
          end else begin
            {Let modem start retraining...}
            caPutModem('AT+FDT');

            {Reset to beginning of this same page}
            if FaxFileName <> '' then begin
              AsyncStatus := caLocatePage(CurrPage);
              if AsyncStatus <> 0 then begin
                CPort^.GotError(AsyncStatus);
                State := tfAbort;
                goto ExitPoint;
              end;

              BlockRead(InFile, PageHeader, SizeOf(PageHeaderRec));
              UseLengthWord := FlagIsSet(PageHeader.ImgFlags, ffLengthWords);
              AsyncStatus := IOResult;
              if AsyncStatus <> ecOk then begin
                CPort^.GotError(AsyncStatus);
                State := tfAbort;
                goto ExitPoint;
              end;
            end else begin
              {Sending cover only, close to prepare for re-open above}
              Close(CvrF);
              if IoResult <> 0 then ;
              CvrOpen := False;                                        {!!.02}
            end;
          end;
        end else begin
          CPort^.GotError(ecFaxSessionError);
          ForceStatus := True;
          State := tfAbort;
        end;

      tf2NextPage :
        if (FaxFileName <> '') and
           ((CurrPage < FaxHeader.PageCount) or SendingCover) then begin

          {Advance to next page}
          if not csAdvancePage then begin
            State := tfAbort;
            goto ExitPoint;
          end;

          caPutModem('AT+FDT');
          State := tf2GetParams;
        end else
          State := tfClose;

      tfClose :
        begin
          if CvrOpen then begin                                        {!!.02}
            Close(CvrF);
            if IOResult = 0 then ;
            CvrOpen := False;                                          {!!.02}
          end;                                                         {!!.02}
          Close(InFile);
          if IoResult = 0 then ;
          Retries := 0;
          SendingCover := False;

          {Log this successful transfer}
          LogFax(PhoneNum, FaxFileName, lfaxTransmitOk);

          {Look for another fax to send}
          State := tfGetEntry;

          caSwitchBaud(False);                                         {!!.02}
        end;

      tfAbort :
        begin
          {Assure files are closed}                                    {!!.02}
          Close(InFile);                                               {!!.02}
          if IoResult <> 0 then ;                                      {!!.02}
          if CvrOpen then begin                                        {!!.02}
            Close(CvrF);                                               {!!.02}
            if IoResult <> 0 then ;                                    {!!.02}
            CvrOpen := False;                                          {!!.02}
          end;                                                         {!!.02}

          SaveStatus := AsyncStatus;
          LogFax(PhoneNum, FaxFileName, lfaxTransmitFail);

          ForceHangup;
          AsyncStatus := SaveStatus;

          {Exit or remain in state machine}
          if FlagIsSet(afFlags, afExitOnError) or
             FlagIsSet(afFlags, afAbortNoConnect) or
             (AsyncStatus = ecUserAbort) then begin
            FaxStatus(False, True);
            CPort^.PR^.FaxActive := False;
            State := tfDone;
          end else
            State := tfGetEntry;

          caSwitchBaud(False);                                         {!!.02}
        end;

      tfCompleteOK :
        begin
          AsyncStatus := ecOk;
          FaxStatus(False, True);
          State := tfDone;
          CPort^.PR^.FaxActive := False;
        end;
    end;

ExitPoint:
    {Assume a waiting state}
    FaxTransmitPart := faxWaiting;

    {Change ready, critical and finished states}
    case State of
      {Critical states}
      tf1Connect,
      tf1SendTSI,
      tf1TSIResponse,
      tf1DCSResponse,
      tf1TrainStart,
      tf1TrainFinish,
      tf1WaitCFR,
      tf1WaitPageConnect : FaxTransmitPart := faxCritical;

      {Ready states}
      tfGetEntry,
      tfDial,
      tfOpenCover,
      tfSendPageHeader,
      tfPrepPage,
      tfClose,
      tfCompleteOk,
      tfAbort         :  FaxTransmitPart := faxReady;

      tfDone          :  FaxTransmitPart := faxFinished;
    end;

    {Save status in case something outside of state machine changes it}
    SaveStatus := AsyncStatus;
  end;

  procedure C12SendFax.PrepareFaxTransmitPart;
  var
    I : Word;
  begin
    CPort^.PR^.FaxActive := True;
    FillChar(FaxHeader, SizeOf(FaxHeader), 0);
    FaxStatus(True, False);
    AsyncStatus := ecOk;
    SaveStatus := ecOk;
    State := tfGetEntry;
    FaxProgress := fpInitModem;
    FaxListNode := FaxListHead;

    {If Class hasn't been set yet then figure out highest class}
    if ClassInUse = ctDetect then
      if SetClassType(ctDetect) = ctUnknown then begin
        AsyncStatus := ecUnknownModemResult;
        Exit;
      end;

    {Removed in !!.02}
    {Switch to initialization baud rate}                               {!!.01}
    {caSwitchBaud(False);}                                             {!!.01}

    {If not sent already, send modem and def init strings}
    if not InitSent then begin
      {Send user init}
      if (ModemInit <> '') and (not caProcessModemCmd(ModemInit)) then
        Exit;

      {Send required inits}
      if not caProcessModemCmd(DefInit) then
        Exit;
      InitSent := True;
    end;

    {Set class}
    if ClassInUse = ctClass1 then begin
      if not caProcessModemCmd('AT+FCLASS=1') then
        Exit;
    end else begin
      if not caProcessModemCmd('AT+FCLASS=2') then
        Exit;
    end;

    {Switch to normal baud rate}                                       {!!.01}
    caSwitchBaud(True);                                                {!!.01}

    {Get class1 get modulation capabilities}
    if ClassInUse = ctClass1 then
      caGetClass1Modulations(True);
  end;

  procedure C12SendFax.FaxTransmit;
    {-Send fax}
  var
    Result : FaxStateType;
  begin
    PrepareFaxTransmitPart;

    {Stay in state machine until finished}
    repeat
    until FaxTransmitPart = faxFinished;
  end;

  {$IFDEF UseStreams}
  constructor C12SendFax.Load(var S : IdStream);
    {-Load}
  begin
    {Use ancestor load}
    C12AbstractFax.Load(S);
    if S.PeekStatus <> 0 then begin
      Done;
      Fail;
    end;

    csRawInit;

    New(Converter, Init);
    if Converter = nil then begin
      CPort^.GotError(ecOutOfMemory);
      Done;
      Fail;
    end;
  end;
  {$ENDIF}

{C12ReceiveFax}

  constructor C12ReceiveFax.Init(ID : Str20; ComPort : AbstractPortPtr);
  begin
    {Validate buffer sizes}
    with ComPort^, PR^ do begin
      if Buffered and (InBuffLen < 4096) then begin
        AsyncStatus := ecBuffersTooSmall;
        Fail;
      end;
    end;

    if not C12AbstractFax.Init(ID, ComPort) then
      Fail;

    FillChar(FaxHeader, SizeOf(FaxHeader), 0);
    FillChar(PageHeader, SizeOf(PageHeader), 0);
    CurrPage     := 1;
    CurrOfs      := 0;
    Last         := #255;
    Sending      := False;
    State        := rfInit;
    OneFax       := False;
    ShowStatus   := False;
    FirstState   := rfInit;
  end;

  destructor C12ReceiveFax.Done;
  begin
    C12AbstractFax.Done;                                               {!!.01}
  end;

  function C12ReceiveFax.InitModemForFaxReceive : Boolean;
    {-Send nessessary commands to initialize modem for fax receive}
  var
    ClassStr : String[3];
  begin
    InitModemForFaxReceive := False;
    caFlushModem;

    {Removed in !!.02}
    {Switch to initialization baud rate}                               {!!.01}
    {caSwitchBaud(False);}                                             {!!.01}

    {User inits}
    if (ModemInit <> '') and (not caProcessModemCmd(ModemInit)) then
      Exit;

    {Required inits}
    if not caProcessModemCmd(DefInit) then
      Exit;
    InitSent := True;

    {Set requested class}
    if ClassInUse = ctDetect then
      ClassInUse := SetClassType(ClassInUse);
    case ClassInUse of
      ctClass1 : ClassStr := '1';
      ctClass2 : ClassStr := '2';
      else begin
        CPort^.GotError(ecFaxInitError);
        Exit;
      end;
    end;
    if not caProcessModemCmd('AT+FCLASS='+ClassStr) then
      Exit;

    {Switch to normal baud rate}                                       {!!.01}
    caSwitchBaud(True);                                                {!!.01}

    {Set Class 2 specifics}
    if ClassInUse = ctClass2 then begin
      if not caProcessModemCmd('AT+FCR=1') then
        Exit;
      if not caProcessModemCmd('AT+FLID="'+StationID+'"') then
        Exit;
      if not caProcessModemCmd('AT+FAA='+FaxAndData) then
        Exit;

      {!!.03 - Changed parameters}
      if not caProcessModemCmd('AT+FDCC=1,'+caSpeedCode+',1,2,0,0,0,0') then
        Exit;
    end;

    InitModemForFaxReceive := True;
  end;

  procedure C12ReceiveFax.SetConnectState;
    {-Force the receiver to pick up a connection in progress}
  begin
    {Init to midstream values}
    FirstState := rf2GetSenderID;
    InitSent := True;
    PageStatus := rpsNewDocument;
    Response := '';
    Critical := True;
    AsyncStatus := ecOk;
    CurrPage := 0;
    DataCount := 0;
    InFileName := '';
    RemoteID := '';
    PhoneNum := '';
    FaxFileName := '';

    {Show the first status here}
    ShowStatus := True;
    FaxStatus(True, False);

    {Start a timer for collecting the next fax response}
    NewTimer(ReplyTimer, ReplyWait);
  end;

  procedure C12ReceiveFax.SetAnswerOnRing(AOR : Integer);
    {-set Nth ring for modem answer}
  begin
    AnswerOnRing := AOR;
  end;

  procedure C12ReceiveFax.SetFaxAndData(OnOff : Boolean);
    {-True for fax to answer either fax or data, False for fax only}
  begin
    if OnOff then
      FaxAndData := '1'
    else
      FaxAndData := '0';
  end;

  procedure C12ReceiveFax.SetOneFax(OnOff : Boolean);
    {-Set "one fax" receive behavior on/off}
  begin
    OneFax := OnOff;
  end;

  procedure C12ReceiveFax.crFlushBuffer;
    {-write current buffered data to InFile}
  var
    BytesWritten : Word;
  begin
    {Write the block}
    BlockWrite(InFile, DataBuffer^, CurrOfs, BytesWritten);
    AsyncStatus := IOResult;
    if BytesWritten <> CurrOfs then
      AsyncStatus := ecDeviceWrite;
    if AsyncStatus <> ecOk then
      CPort^.GotError(AsyncStatus);
    CurrOfs := 0;
  end;


  function C12ReceiveFax.crAddReceivedData : Boolean;
    {-Get waiting FAX stream data.  Returns True if EOP seen.}
  var
    C : Char;
    Count : Word;

    function AddToStream(C : Char) : Boolean;
    begin
      AddToStream := True;
      if ClassInUse = ctClass2 then
        DataBuffer^[CurrOfs] := RotateByte(C)
      else
        DataBuffer^[CurrOfs] := Byte(C);
      Inc(CurrOfs);
      if CurrOfs >= DataBufferSize then begin
        crFlushBuffer;
        if AsyncStatus <> ecOK then
          AddToStream := False;
      end;
    end;

  begin
    crAddReceivedData := False;
    AsyncStatus := ecOK;

    with CPort^ do begin
      if CharReady then begin

        {Process while received data ready}
        Count := 0;
        while CharReady do begin

          {Periodically exit back to state machine to check abort and status}
          Inc(Count);
          if Count > DefStatusBytes then
            Exit;

          GetChar(C);
          AsyncStatus := ecOk;

          {check for <DLE><ETX> pair indicating end of page}
          if C = cETX then
            if Last = cDLE then begin
              crFlushBuffer;
              crAddReceivedData := True;
              Exit;
            end;

          {Write data, DLE is data link escape}
          if (C <> cDLE) or ((C = cDLE) and (Last = cDLE)) then begin
            if AddToStream(C) then
              Inc(DataCount)
            else begin
              {Error writing to file, let state machine handle error}
              crAddReceivedData := True;
              Exit;
            end;
            if Last = cDLE then
              Last := #255
            else
              Last := C;
          end else
            Last := C;
        end;

        {update our timeout}
        NewTimer(ReplyTimer, ReplyWait);
        Exit;
      end;
    end;

    if TimerExpired(ReplyTimer) then begin
      CPort^.GotError(ecTimeout);
      crAddReceivedData := True;
    end;
  end;

  function C12ReceiveFax.crUpdateMainHeader : Integer;
    {-Update the contents of the main header in the file}
  var
    I : Integer;
    L : LongInt;
    W : Word;
  begin
    {Refresh needed fields of MainHeader rec}
    with FaxHeader do begin
      SenderID := RemoteID;                                            {!!.03}
      FDateTime := GetPackedDateTime;
    end;

    {Save current file position for later}
    L := FilePos(InFile);
    I := IOResult;
    if I <> 0 then begin
      crUpdateMainHeader := I;
      Exit;
    end;

    {seek to head of file}
    Seek(InFile, 0);
    if I <> 0 then begin
      crUpdateMainHeader := I;
      Exit;
    end;

    {Write the header}
    BlockWrite(InFile, FaxHeader, SizeOf(FaxHeader), W);
    I := IOResult;
    if (I = 0) and (W <> SizeOf(FaxHeader)) then
      I := ecDeviceWrite;
    if I <> 0 then begin
      crUpdateMainHeader := I;
      Exit;
    end;

    {Return to original position}
    Seek(InFile, L);
    I := IOResult;
    crUpdateMainHeader := I;
  end;

  function C12ReceiveFax.crUpdatePageHeader(PgNo : Word;
                                       var PgInfo : PageHeaderRec) : Integer;
    {-Update the contents of the PgNo-th page header in the file}
  var
    I : Integer;
    W : Word;
    L : LongInt;
  begin
    {save current file position for later}
    L := FilePos(InFile);
    I := IOResult;
    if I <> 0 then begin
      crUpdatePageHeader := I;
      Exit;
    end;

    {find the page in question}
    I := caLocatePage(PgNo);
    if I <> 0 then begin
      crUpdatePageHeader := I;
      Exit;
    end;

    {update the header}
    BlockWrite(InFile, PgInfo, SizeOf(PageHeaderRec), W);
    I := IOResult;
    if (I = 0) and (W <> SizeOf(PageHeaderRec)) then
      I := ecDeviceWrite;
    if I <> 0 then begin
      crUpdatePageHeader := I;
      Exit;
    end;

    {Return to original position}
    Seek(InFile, L);
    I := IOResult;
    crUpdatePageHeader := I;
  end;

  function C12ReceiveFax.FaxReceivePart : FaxStateType;
    {-Receive fax state machine}
  label
    ExitPoint;
  {$IFDEF BadReceiveTrain}
  const
    BadTrainCount : Word = 0;
    MaxBad = 2;
  {$ENDIF}
  var
    Result : Word;
    C : Char;
    Finished : Boolean;
    GotResponse : Boolean;
    Retrain : Boolean;
    PercentBad : Word;

    procedure CheckResponse;
      {-Check for text responses, check for and process HDLC frames}
    begin
      with CPort^ do begin
        {Collect chars till CR/LF or DLE/ETX}
        Finished := False;
        GotResponse := False;
        while CharReady and not Finished do begin
          GetChar(C);
          Response := Response + C;
          if CheckForString(CRLFIndex, C, ^M^J, False) then begin
            Response := Trim(Response);
            if Response <> '' then begin
              {Got a text response}
              Finished := True;
              GotResponse := True;

              {All error responses are aborts}
              if caErrorResponse then begin
                CPort^.GotError(ecUnknownModemResult);
                State := rfAbort;
              end else if caFHNGResponse and (State <> rf2GetFHNG) then begin
                {Unexpected FHNGs are also aborts}
                CPort^.GotError(ecFaxSessionError);
                HangupCode := caExtractFHNGCode;
                State := rfAbort;
              end;
            end;
          end else if CheckForString(EtxIndex, C, #16#3, False) then begin
            {An HDLC frame, process it now}
            LastFrame := caProcessFrame(Retrain);

            {Abort if we got a DCN frame}                              {!!.01}
            if AsyncStatus = ecCancelRequested then                    {!!.01}
              if State <> rf1WaitDCN then begin                        {!!.01}
                GotError(AsyncStatus);                                 {!!.01}
                State := rfAbort;                                      {!!.01}
                GotResponse := True;                                   {!!.01}
              end else                                                 {!!.01}
                AsyncStatus := ecOk;                                   {!!.01}

            {If this is a retrain request change the current state}
            if Retrain then begin
              Delay(ExtraCommandDelay);
              caPutFrameR;
              State := rf1CollectFrames;
            end;
          end;
        end;
      end;
    end;

    function OpenIncomingFile : Boolean;
    begin
      OpenIncomingFile := False;
      CurrPage := 1;
      FillChar(FaxHeader, SizeOf(FaxHeader), 0);
      Move(DefSig, FaxHeader, SizeOf(DefSig));
      FaxHeader.FDateTime := GetPackedDateTime;
      FaxHeader.SenderID  := PadCh(RemoteID, ' ', 20);
      FaxHeader.PageCount := 0;
      FaxHeader.PageOfs   := SizeOf(FaxHeader);
      FaxFileName := FaxName;
      Assign(InFile, FaxFileName);
      Rewrite(InFile, 1);
      AsyncStatus := IOResult;
      if AsyncStatus = ecOk then begin
        BlockWrite(InFile, FaxHeader, SizeOf(FaxHeader));
        AsyncStatus := IOResult;
        if AsyncStatus <> 0 then
          Exit;
      end else
        Exit;
      OpenIncomingFile := True;
    end;

    function WritePage : Word;
      {-Commit the received page}
    var
      Result : Word;
    begin
      {Write received page}
      Inc(FaxHeader.PageCount);
      Result := crUpdateMainHeader;
      if Result = ecOk then begin
        PageHeader.ImgLength := DataCount;
        Result := crUpdatePageHeader(CurrPage, PageHeader);
      end;
      WritePage := Result;
    end;

  begin
    {Restore previous AsyncStatus}
    AsyncStatus := SaveStatus;

    {Check for user abort request}
    if AsyncStatus <> ecUserAbort then
      if afHandleAbort then begin
        if (ClassInUse = ctClass2) and (State > rfAnswer) then
            {Send abort string but don't worry about response}
            CPort^.PutString('AT+FK'^M);
        AsyncStatus := ecUserAbort;
        State := rfAbort;
      end;

    {Show status periodically and after major events}
    if ShowStatus then
      if TimerExpired(StatusTimer) or ForceStatus then begin
        ForceStatus := False;
        NewTimer(StatusTimer, StatusWait);
        FaxStatus(False, False);
      end;

    {Preprocess pending modem responses}
    case State of
      rf1Init1,                                                        {!!.02}
      rf2Init1,
      rf2Init2,
      rf2Init3,
      rfWaiting,
      rf1SendCSI,
      rf1SendDIS,
      rf1CollectFrames,
      rf1CollectRetry1,                                                {!!.03}
      rf1CollectRetry2,                                                {!!.03}
      rf1StartTrain,
      rf1Retrain,
      rf1FinishTrain,
      rf1SendCFR,
      rf1WaitPageConnect,
      rf1FinishPage,
      rf1WaitEOP,
      rf1SendMCF,
      rf1WaitDCN,
      rf1WaitHangup,
      rf2GetConnect,
      rf2ValidConnect,
      rf2GetSenderID,
      rf2GetPageResult,
      rf2GetFHNG :
        begin
          {Preprocess these states, check for text responses and HDLC frames}
          CheckResponse;

          {Don't let rfWaiting state ever timeout}
          if State = rfWaiting then
            NewTimer(ReplyTimer, ReplyWait);

          {Check for timeouts}
          if TimerExpired(ReplyTimer) then begin
            {Handle timeout in various ways, usually abort}
            case State of
              rf1WaitDCN :
                {Exit normally if the DCN never comes}
                State := rfComplete;
              {!!.03 start new}
              rf1CollectFrames :
                {Retry on collectframes timeouts}
                State := rf1CollectRetry1;
              rf1CollectRetry1 :
                {Ignore timeout after AT<cr>}
                State := rf1CollectRetry2;
              rf1SendCSI :
                {If retry in progress, then retry here as well}
                if Retry <> 0 then
                  State := rf1CollectRetry1
                else begin
                  CPort^.GotError(ecTimeout);
                  State := rfAbort;
                end;
              {!!.03 end new}
              else begin
                CPort^.GotError(ecTimeout);
                State := rfAbort;
              end;
            end;
          end else begin
            {Skip state machine if we don't have a response yet}
            if not GotResponse then
              goto ExitPoint;
          end;
        end;
    end;

    {Main state machine}
    case State of
      rfInit :
        begin
          CurrOfs := 0;
          Last := #255;
          Response := '';
          PageStatus := rpsNewDocument;
          CurrPage := 0;
          RingCounter := 0;
          DataCount := 0;
          InFileName := '';
          RemoteID := '';
          PhoneNum := '';
          FaxFileName := '';

          LastPageOk := False;
          CPort^.SetModem(True, True);
          if ClassInUse = ctClass1 then begin
            caPutModem('AT+FCLASS=1');
            State := rf1Init1;                                         {!!.02}
          end else begin
            caPutModem('AT+FCLASS=2');
            State := rf2Init1;
          end;
          NewTimer(ReplyTimer, ReplyWait);
          CPort^.FlushInBuffer;

          {$IFDEF UseSWFlow}
          {Assure software flow control is off}
          if not FlagIsSet(afFlags, afNoSoftwareFlow) then
            CPort^.SWFlowDisable;
          {$ENDIF}
        end;

      {!!.02}
      rf1Init1 :
        if caOkResponse then begin
          caSwitchBaud(True);                                          {!!.02}
          State := rfWaiting;
          FaxProgress := fpWaiting;                                    {!!.02}
        end else begin
          CPort^.GotError(ecFaxInitError);
          State := rfAbort;
        end;

      rf2Init1 :
        if caOkResponse then begin
          caSwitchBaud(True);                                          {!!.02}

          {!!.03 - Changed parameters}
          caPutModem('AT+FDCC=1,'+caSpeedCode+',1,2,0,'+CheckChar+',0,0');
          State := rf2Init3;                                           {!!.01}
        end else begin
          CPort^.GotError(ecFaxInitError);
          State := rfAbort;
        end;

      {!!.01 removed}
      {rf2Init2 :
        if caOkResponse then begin
          caPutModem('AT+FCR=1;+FLID="'+StationID+'";+FAA='+FaxAndData);
          State := rf2Init3;
        end else begin
          CPort^.GotError(ecFaxInitError);
          State := rfAbort;
        end;}

      rf2Init3 :
        if caOkResponse then begin                                     {!!.02}
          State := rfWaiting;                                          {!!.02}
          FaxProgress := fpWaiting;                                    {!!.02}
        end else begin                                                 {!!.02}
          CPort^.GotError(ecFaxInitError);
          State := rfAbort;
        end;

      rfWaiting :
        if Pos('RING', Response) > 0 then begin
          Inc(RingCounter);
          if RingCounter >= AnswerOnRing then begin
            PageStatus := rpsNewDocument;
            State := rfAnswer;

            {Show the first status here}
            ShowStatus := True;
            FaxStatus(True, False);

            NewTimer(ReplyTimer, ReplyWait);
          end;
        end else if (Pos('NO C', Response) > 0) then begin
          {Report connection failure, stay in this state}
          FaxProgress := fpNoConnect;
          ForceStatus := True;
        end else if caConnectResponse then begin
          {Modem is receiving a data call}
          CPort^.GotError(ecFaxDataCall);
          State := rfAbort;
        end else if Pos('VOICE', Response) > 0 then begin
          {Modem is receiving a voice call}
          CPort^.GotError(ecFaxVoiceCall);
          State := rfAbort;
        end else
          caPrepResponse;

      rfAnswer :
        begin
          NewTimerSecs(ReplyTimer, ReplyWait);
          caPutModem('ATA');
          FaxProgress := fpAnswer;
          ForceStatus := True;
          Retry := 0;                                                  {!!.03}
          if ClassInUse = ctClass1 then
            State := rf1SendCSI
          else
            State := rf2ValidConnect;
        end;

      rf1SendCSI :
        if caConnectResponse then begin
          caPutCSIFrame;
          State := rf1SendDIS;
          caPrepResponse;
          Critical := True;
        end else
          caPrepResponse;

      rf1SendDIS :
        if caConnectResponse then begin
          caPutDCSDISFrame(True);
          caPrepResponse;
        end else if caOkResponse then begin
          Delay(ExtraCommandDelay);
          caPutFrameR;
          NewTimer(ReplyTimer, 54);
          State := rf1CollectFrames;
        end else
          caPrepResponse;

      rf1CollectFrames :
        if caConnectResponse then
          caPrepResponse
        else if caOkResponse then begin
          if LastFrame then begin
            caPutFaxCommand('AT+FRM='+ModCode);
            State := rf1StartTrain;

            {$IFDEF UseSWFlow}
            {Turn on flow control for training}
            if not FlagIsSet(afFlags, afNoSoftwareFlow) then
              with CPort^, PR^ do
                SWFlowEnableOpt(InBuffLen - (InBuffLen shr 2),
                                InBuffLen shr 2,
                                sfReceiveFlow);
            {$ENDIF}
          end else begin
            {Ask for next frame}
            caPutFrameR;
          end;
        end else
          caPrepResponse;

      rf1CollectRetry1 :
        begin
          Inc(Retry);
          if Retry <= 3 then begin
            {Failed to get any frames, first get modem's attention}
            caPutModem('AT'+cCR);
            State := rf1CollectRetry2;
          end else begin
            {Too many errors, bail out}
            CPort^.GotError(ecTimeout);
            State := rfAbort;
          end;
        end;

      rf1CollectRetry2 :
        begin
          caPutFrameT;
          State := rf1SendCSI;
        end;

      rf1StartTrain :
        if caConnectResponse then begin
          State := rf1CollectTrain;
          DataCount := 0;
          BadData := 0;
          NewTimer(ReplyTimer, ReplyWait);
          SaveStatus := AsyncStatus;
        end else
          caPrepResponse;

      rf1CollectTrain :
        if CPort^.CharReady then begin
          SaveStatus := AsyncStatus;
          while CPort^.CharReady and (State = rf1CollectTrain) do begin
            CPort^.GetChar(C);
            Inc(DataCount);
            if C <> #0 then
              Inc(BadData);
            if C = cDLE then begin
              Delay(10);
              CPort^.GetChar(C);
              if C = cETX then begin
                {Calculate amount of valid training data}
                PercentBad := (BadData * LongInt(100)) div DataCount;
                {$IFDEF BadReceiveTrain}
                Inc(BadTrainCount);
                if BadTrainCount <= MaxBad then
                  PercentBad := MaxBadPercent+1;
                {$ENDIF}
                if PercentBad < MaxBadPercent then begin
                  State := rf1FinishTrain;
                  caPrepResponse;
                end else begin
                  {Failed to train, prepare to send FTT}
                  caPutFrameT;
                  State := rf1Retrain;
                end;
              end;
            end;
          end;
          AsyncStatus := SaveStatus;
        end;

      rf1Retrain :
        if caConnectResponse then begin
          caPutStandardFrame(FTTFrame);
          if ModCode <> '24' then begin
            {Step down and try again}
            caPrepResponse;
            {Stay in this state to wait for OK}
          end else begin
            {Fatal error, couldn't train at any baud rate}
            CPort^.GotError(ecFaxTrainError);
            {Don't bother waiting for OK}
            State := rfAbort;
          end;
        end else if caOkResponse then begin
          {Expect DCS again}
          caPutFrameR;
          State := rf1CollectFrames;
        end else
          caPrepResponse;

      rf1FinishTrain :
        if caNoCarrierResponse then begin

          {$IFDEF UseSWFlow}
          {Turn off flow control for HDLC frames}
          if not FlagIsSet(afFlags, afNoSoftwareFlow) then
            CPort^.SWFlowDisable;
          {$ENDIF}

          caPutFrameT;
          State := rf1SendCFR;
        end else
          caPrepResponse;

      rf1SendCFR :
        if caConnectResponse then begin
          caPutStandardFrame(CFRFrame);
          caPrepResponse;

          {$IFDEF UseSWFlow}
          {Finished with Phase B, turn on flow control}
          if not FlagIsSet(afFlags, afNoSoftwareFlow) then
            with CPort^, PR^ do
              SWFlowEnableOpt(InBuffLen - (InBuffLen shr 2),
                              InBuffLen shr 2,
                              sfReceiveFlow);
          {$ENDIF}
        end else if caOkResponse then begin
          caPutFaxCommand('AT+FRM='+ModCode);
          State := rf1WaitPageConnect;
          Critical := False;

          {!!.01 start}
          {Not critical anymore, here's our chance to accept and log}
          if not AcceptFax(RemoteID) then begin
            CPort^.GotError(epNonFatal+ecFileRejected);
            ForceStatus := True;
            LogFax(RemoteID, '', lfaxReceiveSkip);
            State := rfComplete;
          end else begin
            {File accepted, open file and log it}
            if OpenIncomingFile then begin
              {Log receive started}
              LogFax(RemoteID, FaxFileName, lfaxReceiveStart);
              PageStatus := rpsNewPage;
              CurrPage := 0;
            end else begin
              CPort^.GotError(AsyncStatus);
              State := rfAbort;
              goto ExitPoint;
            end;
          end;
          {!!.01 end}

          FaxProgress := fpGotRemoteID;
          ForceStatus := True;
        end else
          caPrepResponse;

      rf1WaitPageConnect :
        if caConnectResponse then begin
          State := rfStartPage;
          FaxProgress := fpSessionParams;
          ForceStatus := True
        end else
          caPrepResponse;

      rf2ValidConnect :
        if Pos('+FCON', Response) > 0 then begin
          NewTimer(ReplyTimer, ReplyWait);
          FaxProgress := fpIncoming;
          State := rf2GetSenderID;
          Critical := True;
        end else
          caPrepResponse;

      rf2GetSenderID :
        if Pos('+FTSI', Response) > 0 then begin
          NewTimerSecs(ReplyTimer, ReplyWait);
          StripPrefix(Response);
          RemoteID := TrimStationID(Response);

          {Let user accept or reject this fax}
          if not AcceptFax(RemoteID) then begin
            CPort^.PutString('AT+FKS'^M);
            CPort^.GotError(epNonFatal+ecFileRejected);
            ForceStatus := True;
            LogFax(RemoteID, '', lfaxReceiveSkip);
            State := rfComplete;
          end else begin
            {File accepted, keep going}
            FaxProgress := fpGotRemoteID;
            ForceStatus := True;

            {Open incoming file and log now}                           {!!.01}
            if OpenIncomingFile then begin                             {!!.01}
              {Log receive started}                                    {!!.01}
              LogFax(RemoteID, FaxFileName, lfaxReceiveStart);         {!!.01}
              PageStatus := rpsNewPage;                                {!!.01}
              CurrPage := 0;                                           {!!.01}
            end else begin                                             {!!.01}
              CPort^.GotError(AsyncStatus);                            {!!.01}
              State := rfAbort;                                        {!!.01}
              goto ExitPoint;                                          {!!.01}
            end;                                                       {!!.01}
          end;
        end else if Pos('+FDCS', Response) > 0 then begin
          NewTimerSecs(ReplyTimer, ReplyWait);
          StripPrefix(Response);
          caExtractClass2Params(Response);
          FaxProgress := fpSessionParams;
          ForceStatus := True;
        end else if caOkResponse then begin
          caPutModem('AT+FDR');
          Critical := False;
          ForceStatus := True;
          State := rf2GetConnect;
          NewTimerSecs(ReplyTimer, ReplyWait);
        end else
          caPrepResponse;

      rf2GetConnect :
        if Pos('+FDCS', Response) > 0 then begin
          {Got current session parameters}
          NewTimer(ReplyTimer, ReplyWait);
          StripPrefix(Response);
          caExtractClass2Params(Response);
          FaxProgress := fpSessionParams;
          ForceStatus := True;
        end else if caConnectResponse then begin

          {$IFDEF UseSWFlow}
          {Finished with Phase B, turn on flow control}
          if not FlagIsSet(afFlags, afNoSoftwareFlow) then
            with CPort^, PR^ do
              SWFlowEnableOpt(InBuffLen - (InBuffLen shr 2),
                              InBuffLen shr 2,
                              sfReceiveFlow);
          {$ENDIF}
          State := rfStartPage;
          Critical := True;
        end else
          caPrepResponse;

      rfStartPage :
        begin
          Critical := False;
          Inc(CurrPage);

          {Start or continue fax file}
          case PageStatus of
            rpsNewPage:
              begin
                FillChar(PageHeader, SizeOf(PageHeader), 0);
                if SessionRes then
                  PageHeader.ImgFlags := PageHeader.ImgFlags or ffHighRes;

                {!!.03 - added}
                if SessionWid then
                  PageHeader.ImgFlags := PageHeader.ImgFlags or ffHighWidth;

                BlockWrite(InFile, PageHeader, SizeOf(PageHeader));
                AsyncStatus := IoResult;
                if AsyncStatus <> 0 then begin
                  CPort^.GotError(AsyncStatus);
                  State := rfAbort;
                  goto ExitPoint;
                end;
              end;
            rpsNewDocument:
              begin
                Close(InFile);
                if IOResult = 0 then ;
                if OpenIncomingFile then begin
                  {Log receive started}
                  LogFax(RemoteID, FaxFileName, lfaxReceiveStart);

                  FillChar(PageHeader, SizeOf(PageHeader), 0);
                  if SessionRes then
                    PageHeader.ImgFlags := PageHeader.ImgFlags or ffHighRes;

                  {!!.03 - Added}
                  if SessionWid then
                    PageHeader.ImgFlags := PageHeader.ImgFlags or ffHighWidth;

                  BlockWrite(InFile, PageHeader, SizeOf(PageHeader));
                  AsyncStatus := IOResult;
                  if AsyncStatus <> 0 then begin
                    CPort^.GotError(AsyncStatus);
                    State := rfAbort;
                    goto ExitPoint;
                  end;
                end else begin
                  CPort^.GotError(AsyncStatus);
                  State := rfAbort;
                  goto ExitPoint;
                end;
              end;
          end;

          {Say we're ready to receive fax data}
          if ClassInUse = ctClass2 then begin
            {CPort^.PutChar(cXON);}                                    {!!.01}
            CPort^.PutChar(cDC2);
          end;

          {Init vars for receiving a new page}
          CurrOfs := 0;
          Last := #255;
          NewTimer(ReplyTimer, ReplyWait);
          DataCount := 0;

          {Set next state and show new status}
          State := rfGetPageData;
          FaxProgress := fpGetPage;
          ForceStatus := True;
        end;

      rfGetPageData :
        if crAddReceivedData then begin
          if AsyncStatus = ecOk then begin
            {Normal end of page}
            Last := #255;
            FaxProgress := fpGetPage;
            ForceStatus := True;
            caPrepResponse;

            {$IFDEF UseSWFlow}
            {Finished with Phase C, turn off flow control}
            if not FlagIsSet(afFlags, afNoSoftwareFlow) then
              CPort^.SWFlowDisable;
            {$ENDIF}

            if ClassInUse = ctClass1 then
              State := rf1FinishPage
            else begin
              State := rf2GetPageResult;
              Critical := True;
            end;
          end else begin
            {Error writing page}
            State := rfAbort;
            ForceStatus := True;
          end;
        end;

      rf1FinishPage :
        if caNoCarrierResponse then begin
          Delay(ExtraCommandDelay);
          caPutFrameR;
          State := rf1WaitEOP;
          Critical := True;
        end else
          caPrepResponse;

      rf1WaitEOP :
        if caConnectResponse then
          caPrepResponse
        else if caOkResponse then begin
          if LastFrame then begin
            LastFrame := False;
            MorePages := ReceivedFrame and $FE = MPSFrame;
            State := rf1WritePage;
            FaxProgress := fpGetPageResult;
            LastPageOk := True;
            ForceStatus := True;
          end else begin
            {Ask for another frame}
            caPutFrameR;
          end;
        end else
          caPrepResponse;

      rf1WritePage :
        begin
          {Write received page}
          Result := WritePage;
          if Result <> ecOk then begin
            State := rfAbort;
            goto ExitPoint;
          end;

          {Prepare to send MCF}
          caPutFrameT;
          State := rf1SendMCF;

          FaxProgress := fpCheckMorePages;
          LastPageOk := True;
          ForceStatus := True;
        end;

      rf1SendMCF :
        if caConnectResponse then begin
          caPutStandardFrame(MCFFrame);
          caPrepResponse;
        end else if caOkResponse then begin
          Critical := False;
          {Get more pages or done}
          if MorePages then begin
            {Prepare to receive another page}
            caPutFaxCommand('AT+FRM='+ModCode);
            PageStatus := rpsNewPage;
            State := rfStartPage;
          end else begin
            {Ask for DCN}
            Delay(ExtraCommandDelay);
            caPutFrameR;
            State := rf1WaitDCN;

            {No more pages, close and log this fax}
            Close(InFile);
            if IOResult = 0 then ;
            LogFax(RemoteID, FaxFileName, lfaxReceiveOK);
            FaxFileName := '';

            FaxProgress := fpGetHangup;
            ForceStatus := True;
          end;
        end else
          caPrepResponse;

      rf1WaitDCN :
        if caConnectResponse then
          caPrepResponse
        else if caOkResponse then begin
          if LastFrame then begin
            {hang up}
            caPutModem('ATH0');
            State := rf1WaitHangup;
          end else
            caPutFrameR;
        end else
          caPrepResponse;

      rf1WaitHangup :
        if caOkResponse then begin
          FaxProgress := fpGotHangup;
          ForceStatus := True;
          State := rfComplete;
        end else
          caPrepResponse;

      rf2GetPageResult :
        if Pos('+FPTS', Response) > 0 then begin
          FaxProgress := fpGetPageResult;
          ForceStatus := True;
          StripPrefix(Response);
          case Response[1] of
            '1','3','5':  {page good}
              begin
                LastPageOk := True;
                AsyncStatus := WritePage;
                if AsyncStatus <> ecOk then begin
                  CPort^.GotError(AsyncStatus);
                  State := rfAbort;
                  goto ExitPoint;
                end;
              end;

            else  {page bad, discard}
              begin
                LastPageOk := False;
                if caLocatePage(CurrPage) <> 0 then
                  State := rfAbort
                else begin
                  Truncate(InFile);
                  AsyncStatus := IOResult;
                  if AsyncStatus <> 0 then begin
                    CPort^.GotError(AsyncStatus);
                    State := rfAbort;
                    goto ExitPoint;
                  end;
                end;
              end;
          end;
        end else if Pos('+FET', Response) > 0 then begin
          FaxProgress := fpCheckMorePages;
          ForceStatus := True;
          StripPrefix(Response);
          case Response[1] of
            '0':  PageStatus := rpsNewPage;
            '1':  PageStatus := rpsNewDocument;
            '2':  PageStatus := rpsEndOfDocument;
            '3':  PageStatus := rpsMoreSame;
            else  PageStatus := rpsBadPage;
          end;
        end else if caOkResponse then begin
          Critical := False;
          caPutModem('AT+FDR');
          if PageStatus = rpsEndOfDocument then begin
            {Close and log receive OK}
            Close(InFile);
            if IOResult = 0 then ;
            LogFax(RemoteID, FaxFileName, lfaxReceiveOK);
            FaxFileName := '';

            State := rf2GetFHNG;
            FaxProgress := fpGetHangup;
            ForceStatus := True;
          end else
            State := rf2GetConnect;
        end else
          caPrepResponse;

      rf2GetFHNG :
        if caFHNGResponse then begin
          HangupCode := caExtractFHNGCode;
          FaxProgress := fpGotHangup;
          ForceStatus := True;
          Response := '';
        end else if Pos('+FET', Response) > 0 then begin
          {allow redundant FET}
          FaxProgress := fpGetHangup;
          ForceStatus := True;
          Response := '';
        end else if caOkResponse then
          State := rfComplete
        else
          caPrepResponse;

      rfAbort :
        begin
          Critical := False;
          SaveStatus := AsyncStatus;

          {Log receive failed}
          if FaxFileName <> '' then begin                              {!!.02}
            if WritePage <> 0 then ;                                   {!!.02}
            Close(InFile);                                             {!!.02}
            if IoResult <> 0 then ;                                    {!!.02}
            LogFax(RemoteID, FaxFileName, lfaxReceiveFail);
          end;                                                         {!!.02}

          {Try to hangup modem}
          (*if CPort^.CheckDCD then begin*)                            {!!.03}
            Delay(AbortDelay);
            CPort^.PutString('+++');
            Delay(AbortDelay);
            caPutModem('ATH');
            Delay(AbortDelay);                                         {!!.02}
            CPort^.FlushInBuffer;                                      {!!.02}
          (*end;*)                                                     {!!.03}

          if CPort^.CheckDCD then
            caFlushModem;

          AsyncStatus := SaveStatus;

          {Finished with status}
          if ShowStatus then begin
            FaxStatus(False, True);
            ShowStatus := False;
          end;

          {Exit on errors or stay?}
          if FlagIsSet(afFlags, afExitOnError) or
             (AsyncStatus = ecUserAbort) then begin
            CPort^.PR^.FaxActive := False;
            State := rfDone;
            FirstState := rfInit;
          end else
            State := rfInit;
        end;

      rfComplete :
        begin
          caSwitchBaud(False);                                         {!!.02}

          {Finished with status}
          FaxStatus(False, True);
          ShowStatus := False;

          {Finished or go look for more faxes?}
          if OneFax then begin
            AsyncStatus := ecOk;
            CPort^.PR^.FaxActive := False;
            State := rfDone;
            FirstState := rfInit;
          end else
            State := rfInit;
        end;
    end;

ExitPoint:
    {Assume a critical state}
    FaxReceivePart := faxCritical;

    case State of
      {waiting states}
      rf2Init1,
      rf2Init2,
      rf2Init3,
      rfWaiting,
      rf1SendCSI,
      rf1SendDIS,
      rf1CollectFrames,
      rf1CollectRetry1,                                                {!!.03}
      rf1CollectRetry2,                                                {!!.03}
      rf1StartTrain,
      rf1CollectTrain,
      rf1Retrain,
      rf1FinishTrain,
      rf1SendCFR,
      rf1WaitPageConnect,
      rf2ValidConnect,
      rf2GetSenderID,
      rf2GetConnect,
      rfGetPageData,
      rf2GetPageResult,
      rf1FinishPage,
      rf1WaitEOP,
      rf1WritePage,
      rf1SendMCF,
      rf1WaitDCN,
      rf1WaitHangup,
      rf2GetFHNG         :  if Critical then
                              FaxReceivePart := faxCritical
                            else
                              FaxReceivePart := faxWaiting;
      {ready states}
      rfInit,
      rfAnswer,
      rfStartPage,
      rfComplete,
      rfAbort           :  if Critical then
                             FaxReceivePart := faxCritical
                           else
                             FaxReceivePart := faxReady;

      rfDone            :  FaxReceivePart := faxFinished;
    end;

    {Save last AsyncStatus}
    SaveStatus := AsyncStatus;
  end;

  procedure C12ReceiveFax.PrepareFaxReceivePart;
  begin
    State := FirstState;
    AsyncStatus := ecOk;
    SaveStatus := ecOk;
    CPort^.PR^.FaxActive := True;

    {Removed in !!.02}
    {Switch to initialization baud rate}                               {!!.01}
    {caSwitchBaud(False);}                                             {!!.01}

    {If Class hasn't been set yet then figure out highest class}
    if ClassInUse = ctDetect then
      if SetClassType(ctDetect) = ctUnknown then
        AsyncStatus := ecUnknownModemResult;

    {If not sent already, send modem and def init strings}
    if not InitSent then begin
      {Send user init}
      if (ModemInit <> '') and (not caProcessModemCmd(ModemInit)) then
        Exit;

      {Send required inits}
      if not caProcessModemCmd(DefInit) then
        Exit;
      InitSent := True;
    end;

    {Set class}
    if (FirstState <> rf2GetSenderID) then                             {!!.01}
      if ClassInUse = ctClass1 then begin
        if not caProcessModemCmd('AT+FCLASS=1') then
          Exit;
        caSwitchBaud(True);                                            {!!.02}
      end else begin
        if not caProcessModemCmd('AT+FCLASS=2') then
          Exit;
        caSwitchBaud(True);                                            {!!.02}
        if not caProcessModemCmd('AT+FCR=1') then                      {!!.01}
          Exit;                                                        {!!.01}
        if not caProcessModemCmd('AT+FLID="'+StationID+'"') then       {!!.01}
          Exit;                                                        {!!.01}
        if not caProcessModemCmd('AT+FAA='+FaxAndData) then            {!!.01}
          Exit;                                                        {!!.01}
      end;

    {Get class1 get modulation capabilities}
    if ClassInUse = ctClass1 then begin
      caGetClass1Modulations(False);

      {Set highest BPSIndex from MaxFaxBPS and local modulation capabilities}
      Inc(BPSIndex);
      repeat
        Dec(BPSIndex)
      until Class1BPSArray[BPSIndex] <= MaxFaxBPS;
    end;

    {Removed in !!.02}
    {Switch to normal baud rate}                                       {!!.01}
    {caSwitchBaud(True);}                                              {!!.01}
  end;

  procedure C12ReceiveFax.FaxReceive;
    {-wrapper routine to receive a fax}
  begin
    PrepareFaxReceivePart;

    repeat
    until FaxReceivePart = faxFinished;
  end;

  {$IFDEF UseStreams}
  constructor C12ReceiveFax.Load(var S : IdStream);
  begin
    {Use ancestor load}
    C12AbstractFax.Load(S);
    if S.PeekStatus <> 0 then begin
      Done;
      Fail;
    end;

    {Load stream data}
    S.Read(OneFax, SizeOf(OneFax));
    S.Read(FirstState, SizeOf(FirstState));
  end;

  procedure C12ReceiveFax.Store(var S : IdStream);
  begin
    {Ancestor store}
    C12AbstractFax.Store(S);

    {Store various data}
    S.Write(OneFax, SizeOf(OneFax));
    S.Write(FirstState, SizeOf(FirstState));
  end;
  {$ENDIF}

  {$IFDEF UseStreams}
  procedure C12AbstractFaxStream(SPtr : IdStreamPtr);
    {-Register all types needed for C12 abstract fax converter}
  begin
    AbstractFaxStream(SPtr);
    SPtr^.RegisterType(otC12AbstractFax, veC12AbstractFax,
                       TypeOf(C12AbstractFax),
                       @C12AbstractFax.Store, @C12AbstractFax.Load);
  end;

  procedure C12SendFaxStream(SPtr : IdStreamPtr);
    {-Register all types needed for C12 send fax converter}
  begin
    C12AbstractFaxStream(SPtr);
    SPtr^.RegisterType(otC12SendFax, veC12SendFax,
                       TypeOf(C12SendFax),
                       @C12SendFax.Store, @C12SendFax.Load);
  end;

  procedure C12ReceiveFaxStream(SPtr : IdStreamPtr);
    {-Register all types needed for C12 receive fax converter}
  begin
    C12AbstractFaxStream(SPtr);
    SPtr^.RegisterType(otC12ReceiveFax, veC12ReceiveFax,
                       TypeOf(C12ReceiveFax),
                       @C12ReceiveFax.Store, @C12ReceiveFax.Load);
  end;

  {$ENDIF}

end.
