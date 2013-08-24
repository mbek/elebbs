{$S-,R-,V-,I-,B-,F+,O+,A-}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{Include OPro's define file if UseOPro is specified}
{$IFDEF UseOPro}
{$I OPDEFINE.INC}
{$ENDIF}

{*********************************************************}
{*                  OOBPLUS.PAS 2.03                     *}
{*     Copyright (c) TurboPower Software 1991.           *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OOBPlus;
  {-Provides CIS B+ remote (not host) protocol}

interface

uses
  Dos,
  {$IFDEF UseOPro}
  OpInline,
  OpString,
  OpRoot,
  {$ENDIF}
  {$IFDEF UseTPro}
  TpMemChk,
  TpInline,
  TpString,
  {$ENDIF}
  ApMisc,
  ApPort,
  ApTimer,
  OOCom,
  OoAbsPcl;

const
  {#Z+}
  {Compile-time constants}
  BPTimeoutMax     = 546;  {Max tics allowed timeout per-char, 30 seconds}
  BPErrorMax       = 10;   {Max sequential errors}
  BPBufferMax      = 2048; {Largest data block available}              {!!.01}
  BPSendAheadMax   = 2;    {max number of packets we can send ahead (normal)}
  BPDefFinishWait : Word = 273;  {Wait time for error ack, 15 seconds}
  {#Z-}

  {For estimating protocol transfer times}
  BPlusTurnDelay : Word = 0;        {Millisecond turnaround delay}
  BPlusOverHead : Word = 20;        {Default overhead for each data subpacket}

  ESCIResponse : String[80] = '#IB1,SSxx,GF,PB,DT';

type
  {#Z+}
  DataBlockTypePtr = ^DataBlockType;                                   {!!.01}
  BPDataBlockType = array[1..BPBufferMax] of Char;                     {!!.01}

  {Window buffer}
  SABuffType = record
    Seq   : Word;                {Sequence number}
    Num   : Word;                {Packet's data size}
    PType : Char;                {Packet type}
    Buf   : ^BPDataBlockType;    {Packet's data}                       {!!.01}
  end;
  SPackets = Array[1..BPSendAheadMax] of SABuffType;

  {For quoting params sets}
  QuoteArray = Array[0..7] of Byte;
  {#Z-}

const
  {#Z+}
  {All chars in ranges $00..$1F and $80..$9F}
  DQFull    : QuoteArray = ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
  {ETX ENQ DLE XON XOFF NAK}
  DQDefault : QuoteArray = ($14, $00, $D4, $00, $00, $00, $00, $00);
  {#Z-}

type
  {#Z+}
  {Main BPlus state table}
  BPlusStateType = (
    {Receive states}
    rbInitial,         {0  Start waiting for first N packet}
    rbGetDLE,          {1  Get header start, <DLE>}
    rbGetB,            {2  Get B of header start}
    rbCollectPacket,   {3  Collect packet, checksum and verify}
    rbProcessPacket,   {4  Check packet type and dispatch}
    rbFinished,        {5  Normal end of protocol}
    rbSendEnq,         {6  Send <ENQ><ENQ> after a timeout}
    rbError,           {7  Error end of protocol}
    rbWaitErrorAck,    {8  Wait for Ack for failure packet}
    rbCleanup,         {9  Cleanup and end protocol}
    rbDone,            {10 Signal end}

    {Transmit states}
    tbInitial,         {11 Startup stuff}
    tbGetBlock,        {12 Read next block to xmit}
    tbWaitFreeSpace,   {13 Wait for free space to send block}
    tbSendData,        {14 Transmit the next block}
    tbCheckAck,        {15 Wait for acknowledgements (handle re-xmits)}
    tbEndOfFile,       {16 Send TC packet}
    tbEofAck,          {17 Wait for TC ack}
    tbError,           {18 Failed}
    tbWaitErrorAck,    {19  Wait for Ack for failure packet}
    tbCleanup,         {20 Cleanup and end protocol}
    tbDone);           {21 Signal end}

  {Packet collection states}
  PacketStateType = (
    psGetDLE,          {0  Waiting for DLE}
    psGetB,            {1  Waiting for B}
    psGetSeq,          {2  Waiting for sequence number}
    psGetType,         {3  Get type byte}
    psGetData,         {4  Collecting data}
    psGetCheck1,       {5  Waiting for first check byte}
    psGetCheck2,       {6  Waiting for second check byte, if any}
    psCheckCheck,      {7  Checking block check}
    psSendAck,         {8  OK, sending ACK (finished)}
    psError,           {9  Error collecting packet}
    psSuccess);        {10 Finished OK}

  {Terminal packet state, when processing packets in terminal mode}
  TermPacketStateType = (
    tpsStart,          {Got DLE, inits}
    tpsWaitB,          {Waiting for B}
    tpsWaitSeq,        {Waiting for sequence}
    tpsWaitType,       {Waiting for packet type, process when found}
    tpsError);         {Error collecting packet}

  {Ack collection state}
  AckCollectionStateType = (
    acGetDLE,          {0  Wait for DLE}
    acGetNum,          {1  Wait for packet number}
    acHaveAck,         {2  Got ack, check sequence}
    acGetPacket,       {3  Got packet, start collecting}
    acCollectPacket,   {4  Collect packet}
    acSkipPacket1,     {5  Discard packet data}
    acSkipPacket2,     {6  Discard 1st checksum byte}
    acSkipPacket3,     {7  Discard quoted part of 1st checksum byte}
    acSkipPacket4,     {8  Discard 2nd checksum byte}
    acSkipPacket5,     {9  Discard quoted part of 2nd checksum byte}
    acTimeout,         {10 Timeout collecting data}
    acError,           {11 Error processing ack/packet}
    acSendNak,         {12 Sending nak}
    acSendEnq,         {13 Sending enq and resyncing}
    acResync1,         {14 Collect 1st DLE of resync}
    acResync2,         {15 Collect seq of first resync}
    acResync3,         {16 Collect 2nd DLE of resync}
    acResync4,         {17 Collect seq of second resync}
    acSendData,        {18 Sending data}
    acFailed);         {19 Failed miserably}

  {Protocol direction options}
  DirectionType = (dUpload, dDownload);

  {Transfer parameters}
  ParamsRecord = record
    WinSend,                              {Send window size}
    WinRecv,                              {Receive window size}
    BlkSize,                              {Block size (* 128)}
    ChkType  : Byte;                      {Check type, chksum or CRC}
    QuoteSet : QuoteArray;                {Chars to quote}
    DROpt,                                {DL Recovery option}
    UROpt,                                {UL Recovery option}
    FIOpt    : Byte;                      {File Info option}
  end;

  BPlusProtocolPtr = ^BPlusProtocol;
  {#Z-}

  {User resume procedure}
  HandleResumeProc = procedure(AP : BPlusProtocolPtr);

  BPlusProtocol = object(AbstractProtocol)
    Quoted      : Boolean;                 {True if last ch recd was quoted}
    QuoteTable  : Array[0..255] of Char;   {Active quoting table}
    Checksum    : Word;                    {Checksum or CRC}
    Direction   : DirectionType;           {upload or download}
    HostParams  : ParamsRecord;            {Host's parameters}
    OurParams   : ParamsRecord;            {Our parameters}
    AbortCount  : Integer;                 {# of abort requests so far}
    ResumeFlag  : Boolean;                 {True if resuming an aborted dl}
    Aborting    : Boolean;                 {True if processing abort}
    BPlusMode   : Boolean;                 {True if in full B+ mode}
    RSize       : Integer;                 {Size of last recd buffer}
    RBuffer     : ^BPDataBlockType;        {Receive buffer}            {!!.01}
    SBuffer     : SPackets;                {Send buffers}
    SeqNum      : Integer;                 {Current sequence number}
    Next2ACK    : Integer;                 {Packet pending ACK}
    Next2Fill   : Integer;                 {Packet to load for send}
    SAMax       : Integer;                 {Highest current sendahead cnt}
    SAWaiting   : Integer;                 {# of packets outstanding ACKs}
    SAErrors    : Integer;                 {Keep track of SendAhead errors}
    RRaw        : LongInt;                 {Raw bytes received}
    RPackets    : LongInt;                 {Packets received}
    SRaw        : LongInt;                 {Raw bytes sent}
    SPackets    : LongInt;                 {Packets sent}
    BPlusState  : BPlusStateType;          {Main state}
    PacketState : PacketStateType;         {Packet collection state}
    AckState    : AckCollectionStateType;  {Ack collection state}
    ReplyTimer  : EventTimer;              {Used to timeout replies}
    QuotePending: Boolean;                 {True if 2nd quote char pending}
    SaveC       : Char;                    {Save last char between states}
    LastType    : Char;                    {Last received packet type}
    NextSeq     : Integer;                 {Next sequence number}
    NAKSent     : Boolean;                 {True if NAK just sent}
    SaveStatus  : Word;                    {Holds status between states}
    Failed      : Boolean;                 {True if write failed}
    PacketNum   : Integer;                 {Current packet num}
    Idx         : Integer;                 {Index for collecting data blocks}
    ErrorStatus : Word;                    {Holds error status after failure}
    LastBlock   : Boolean;                 {True at eof}
    LastBlockSize: Word;                   {Size of last transmit block}
    FinishWait  : Word;                    {Ticks to wait for finish ack}
    SentEnq     : Boolean;                 {True if sent ENQ from CollectAck}
    NewChk      : Word;                    {Calculated checksum}

    HandleResume: HandleResumeProc;        {Resume procedure}

    constructor Init(APPtr : AbstractPortPtr);
    constructor InitCustom(APPtr : AbstractPortPtr; Options : Word);
    destructor Done; virtual;

    {Public methods called by terminal handlers}
    procedure ProcessENQ;
      {-Handle an <ENQ> from host}
    procedure ProcessESCI(X, Y : Byte);
      {-Handle <ESC><'I'> (VT52 terminal capabilities inquiry) from host}
    procedure ProcessDLE(var Start, Upload : Boolean);
      {-Called when <DLE> seen from host, starts protocol}
    procedure SetHandleResumeProc(HRP : HandleResumeProc);
      {-Set custom HandleResume procedure}

    {Publics methods called during file transfer}
    procedure PrepareTransmitPart; virtual;
      {-Prepare to transmit file in parts}
    function ProtocolTransmitPart : ProtocolStateType; virtual;
      {-Perform one increment of a protocol transmit}
    procedure PrepareReceivePart; virtual;
      {-Prepare to receive BPlus parts}
    function ProtocolReceivePart : ProtocolStateType; virtual;
      {-Perform one increment of a protocol receive}

    {Background methods}
    function CollectPacket : Boolean;
    function CollectACK : Boolean;

    {Non-background methods for terminal processing}
    function GetPacket(FirstState : PacketStateType) : Boolean;
    function GetAck : Boolean;
    procedure GetTransferPacket;
    procedure ProcessFileTransferParams;
    procedure GetFileTransferPacket;

    {Misc private methods}
    procedure apAbortProtocol; virtual;
    procedure apCancel; virtual;
    procedure bpSendAck;
    procedure bpGetCharQuoted(var C : Char);
    procedure UpdateQuoteTable(QS : QuoteArray);
    procedure apResetProtocol; virtual;
    procedure apUpdateBlockCheck(CurByte : Byte); virtual;
    procedure SendByte(C : Char);
    procedure SendQuotedByte(C : Char);
    procedure SendNAK;
    function IncSequence(Value : Integer) : Integer;
    procedure SendFailure(Reason : String);
    procedure SendData(BNum : Integer);
    function IncSA(Value : Integer) : Integer;
    procedure SendPacket(PacketType : Char; Size : Integer);
    procedure SendTransportParams;
    procedure ProcessTransportParams; virtual;
    procedure bpInitVars;
    procedure apPrepareWriting; virtual;
    procedure bpHandleResumeFail;

    {$IFDEF UseStreams}
    constructor Load(var S : IdStream);
      {-Load a BPlusProtocol object from a stream}
    procedure Store(var S : IdStream);
      {-Store a BPlusProtocol object to a stream}
    {$ENDIF}
  end;

  procedure NoHandleResume(AP : BPlusProtocolPtr);
    {-Empty HandleResume procedure}

  {$IFDEF UseStreams}
  procedure BPlusProtocolStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing bplus objects}
  {$ENDIF}

implementation

const
  {Default ParamsRecord values}
  DefDR : Byte = 1;   {Can handle Download Resume}
  DefBS : Byte = 16;  {Default to 128 * DefBS (2048) byte packets}
  DefWS = 1;          {Can handle send ahead}
  DefWR = 2;          {Can receive up to 2 packets ahead}
  DefCM = 1;          {Can handle CRC blockchecking}
  DefDQ = 1;          {Can handle special quoting including non-quoted NUL}
  DefUR = 0;          {Can NOT handle Upload Recovery (not supported by CIS)}
  DefFI = 1;          {Can handle File Info packet}
  DefXP = 0;          {FTP/GIF does not use TransportLayer}

  function Long2Str(L : LongInt) : string;
    {-Convert a long/word/integer/byte/shortint to a string}
  var
    S : string;
  begin
    Str(L, S);
    Long2Str := S;
  end;

  constructor BPlusProtocol.Init(APPtr : AbstractPortPtr);
  begin
    if not InitCustom(APPtr, DefProtocolOptions) then
      Fail;
  end;

  constructor BPlusProtocol.InitCustom(APPtr : AbstractPortPtr; Options : Word);
  var
    I : Word;
  begin
    if not AbstractProtocol.InitCustom(APPtr, Options) then
      Fail;

    ProtType := BPlus;
    CheckType := bcChecksum1;
    RRaw      := 0;
    RPackets  := 0;
    SRaw      := 0;
    SPackets  := 0;
    case ActCPS of
      0..30   : DefBS := 1;
      31..120 : DefBS := 4;
      else      DefBS := 16;
    end;

    FinishWait := BPDefFinishWait;
    HandshakeWait := BPTimeoutMax;
    HandleResume := NoHandleResume;
    QuotePending := False;
    TurnDelay := BPlusTurnDelay;
    Overhead := BPlusOverHead;
    SentEnq := False;

    {Allocate buffers}
    Failed := not GetMemCheck(RBuffer, BPBufferMax);
    I := 1;
    while not Failed and (I <= BPSendaheadMax) do begin
      Failed := not GetMemCheck(SBuffer[I].Buf, BPBufferMax);
      Inc(I);
    end;
    if Failed then begin
      Done;
      APPtr^.GotError(epFatal+ecOutOfMemory);                          {!!.01}
      Fail;
    end;
  end;

  destructor BPlusProtocol.Done;
    {-Destroy the protocol object}
  var
    I : Word;
  begin
    AbstractProtocol.Done;
    for I := 1 to BPSendaheadMax do
      FreeMemCheck(SBuffer[I].Buf, BPBufferMax);
    FreeMemCheck(RBuffer, BPBufferMax);
  end;

  procedure BPlusProtocol.bpGetCharQuoted(var C : Char);
    {-Return a character that was transmitted quoted}
  label
    Quote;
  begin
    Quoted := False;

    with APort^ do begin
      if QuotePending then
        goto Quote;
      GetChar(C);
      if C <> cDLE then
        Exit;

  Quote:
      Quoted := True;
      if CharReady then begin
        QuotePending := False;
        GetChar(C);
        if C < #$60 then
          C := Char(Ord(C) and $1F)
        else
          C := Char((Ord(C) and $1F) or $80);
      end else
        QuotePending := True;
    end;
  end;

  procedure BPlusProtocol.UpdateQuoteTable(QS : QuoteArray);
    {-Update our QuoteTable to match the QS quotearray}
  var
    I,J,K : Integer;
    B,C : Byte;
  begin
    K := 0;
    C := $40;

    for I := 0 to 7 do begin
      if I = 4 then begin
        K := 128;
        C := $60;
      end;
      B := QS[I];

      for J := 0 to 7 do begin
        if (B and $80) <> 0 then
          QuoteTable[K] := Char(C);
        B := B shl 1;
        Inc(C);
        Inc(K);
      end;
    end;
  end;

  procedure BPlusProtocol.apResetProtocol;
    {-Init important session-dependant protocol vars}
  begin
    SeqNum := 0;
    SAMax := 1;
    SAErrors := 0;
    BlockLen := 512;
    AbortCount := 0;
    BPlusMode := False;
    CheckType := bcChecksum1;
    FillChar(QuoteTable, SizeOf(QuoteTable), 0);
    FillChar(OurParams, SizeOf(OurParams), 0);
    OurParams.BlkSize := 4;
    OurParams.QuoteSet := DQDefault;
    UpdateQuoteTable(DQDefault);
  end;

  procedure BPlusProtocol.ProcessENQ;
    {-Called when the terminal handler receives an <ENQ>}
  begin
    Aborting := False;
    apResetProtocol;
    APort^.PutString(cDLE+'++'+cDLE+'0');
  end;

  procedure BPlusProtocol.ProcessESCI(X, Y : Byte);
    {-Called by terminal handler when <ESC><'I'> seen at port}
  var
    S : String;
    T : String[5];
    P : Integer;
  begin
    S := ESCIResponse;

    {Make sure tailer is in place for later}
    if Pos(',+',S) = 0 then
      S := S + ',+';

    {If 'SSxx' part of string, insert screen size values}
    P := Pos('SSxx',S);
    if P > 0 then begin
      S[P+2] := Chr(Y+31);
      S[P+3] := Chr(X+31);
    end;

    {Build the string's checksum and append it to the string}
    X := 0;
    for P := 1 to Length(S) do
      Inc(X, Ord(S[P]));
    Str(X, T);
    S := S + T;

    {Send the response}
    APort^.PutString(S+^M);
  end;

  procedure BPlusProtocol.ProcessDLE(var Start, Upload : Boolean);
    {-Called from terminal loop when <DLE> seen from host}
  var
    TermState : TermPacketStateType;
    Finished : Boolean;
    C : Char;
  begin
    Start := False;
    TermState := tpsStart;
    with APort^ do begin

      PR^.ProtocolActive := True;
      Finished := False;

      repeat
        case TermState of
          tpsStart :
            begin
              NewTimer(ReplyTimer, HandshakeWait);
              TermState := tpsWaitB;
            end;
          tpsWaitB :
            if CharReady then begin
              GetChar(C);
              if C = 'B' then begin
                NewTimer(ReplyTimer, HandshakeWait);
                TermState := tpsWaitSeq;
              end else
                TermState := tpsError;
            end else if TimerExpired(ReplyTimer) then
              TermState := tpsError;
          tpsWaitSeq :
            if CharReady then begin
              {Get sequence byte...}
              GetChar(C);
              if CheckType = bcCrc16 then
                CheckSum := $FFFF
              else
                CheckSum := 0;
              apUpdateBlockCheck(Byte(C));
              TermState := tpsWaitType;
              PacketNum := Ord(C)-Ord('0');
            end else if TimerExpired(ReplyTimer) then
              TermState := tpsError;
          tpsWaitType :
            if CharReady then begin
              GetChar(C);
              case C of
                '+' :
                  begin
                    {Collect and process + packet, send our options}
                    bpInitVars;
                    GetTransferPacket;
                    Finished := True;
                  end;
                'T' :
                  begin
                    {Collect and process T packet}
                    GetFileTransferPacket;
                    if AsyncStatus = ecOk then begin
                      Start := True;
                      Upload := Direction = dUpload;
                    end;
                    Finished := True;
                  end;
                else
                  TermState := tpsError;
              end;
            end else if TimerExpired(ReplyTimer) then
              TermState := tpsError;
          tpsError :
            {Timeout getting char or unknown packet type, just return}
            Finished := True;
        end;
      until Finished;

      PR^.ProtocolActive := False;
    end;
  end;

  procedure BPlusProtocol.SetHandleResumeProc(HRP : HandleResumeProc);
    {-Set custom HandleResume procedure}
  begin
    HandleResume := HRP;
  end;

  function BPlusProtocol.GetPacket(FirstState : PacketStateType) : Boolean;
    {-Non-background method to collect entire packet}
  begin
    PacketState := FirstState;
    repeat
      if CollectPacket then begin
        GetPacket := AsyncStatus = ecOk;
        Exit;
      end;
    until False;
  end;

  function BPlusProtocol.GetAck : Boolean;
    {-Non-background method to collect Ack}
  begin
    AckState := acGetDLE;
    repeat
      if CollectAck then begin
        GetAck := AsyncStatus = ecOk;
        Exit;
      end;
    until False;
  end;

  procedure BPlusProtocol.SendTransportParams;
    {-Send our params, collect ack}
  begin
    with SBuffer[Next2Fill] do begin
      Buf^[1] := Char(DefWS);
      Buf^[2] := Char(DefWR);
      Buf^[3] := Char(DefBS);
      Buf^[4] := Char(DefCM);
      Buf^[5] := Char(DefDQ);
      Buf^[6] := Char(DefXP);
      Move(OurParams.QuoteSet, Buf^[7], 8);
      Buf^[15] := Char(DefDR);
      Buf^[16] := Char(DefUR);
      Buf^[17] := Char(DefFI);
    end;

    {Send the transport packet}
    SendPacket('+', 17);

    {Get the ack}
    if not GetAck then ;
  end;

  procedure BPlusProtocol.ProcessTransportParams;
    {-Process received "+" packet, send our params}
  var
    QSP : Boolean;
  begin
    OurParams.QuoteSet := DQDefault;
    FillChar(RBuffer^[RSize+1], SizeOf(RBuffer^)-RSize, 0);
    Move(RBuffer^[1], HostParams.WinSend, 4);
    Move(RBuffer^[7], HostParams.QuoteSet, 11);

    {Send '+' packet under FULL quoting}
    QSP := (RSize >= 14);
    UpdateQuoteTable(DQFull);
    SendTransportParams;

    {Make a minimal set of parameters to work from}
    if HostParams.WinSend < DefWR then
      OurParams.WinSend := HostParams.WinSend
    else
      OurParams.WinSend := DefWR;

    {If > 0, we can use all windows}
    if OurParams.WinSend <> 0 then
      SAMax := BPSendAheadMax;

    if HostParams.WinRecv < DefWS then
      OurParams.WinRecv := HostParams.WinRecv
    else
      OurParams.WinRecv := DefWS;

    if HostParams.BlkSize < DefBS then
      OurParams.BlkSize := HostParams.BlkSize
    else
      OurParams.BlkSize := DefBS;

    if OurParams.BlkSize = 0 then
      OurParams.BlkSize := 4;
    BlockLen := (OurParams.BlkSize * 128);

    if HostParams.ChkType < DefCM then
      OurParams.ChkType := HostParams.ChkType
    else
      OurParams.ChkType := DefCM;

    {If = 1, we need CRC blockchecking}
    if OurParams.ChkType > 0 then
      CheckType := bcCrc16;

    if HostParams.DROpt < DefDR then
      OurParams.DROpt := HostParams.DROpt
    else
      OurParams.DROpt := DefDR;

    if HostParams.UROpt < DefUR then
      OurParams.UROpt := HostParams.UROpt
    else
      OurParams.UROpt := DefUR;

    if HostParams.FIOpt < DefFI then
      OurParams.FIOpt := HostParams.FIOpt
    else
      OurParams.FIOpt := DefFI;

    FillChar(QuoteTable, SizeOf(QuoteTable), 0);  {Clear the Quote Table}
    UpdateQuoteTable(OurParams.QuoteSet);         {Set our quoting}
    if QSP then                                   {If host sent a set,}
      UpdateQuoteTable(HostParams.QuoteSet);      {Add his as well}
    BPlusMode := True;                            {Now using full B+}
    ProtocolTypeString[ProtType] := 'B Plus';
  end;

  procedure BPlusProtocol.GetTransferPacket;
    {-Collect and process + packet}
  begin
    {Start collecting from body field}
    NextSeq := IncSequence(SeqNum);
    Idx := 1;

    apUpdateBlockCheck(Byte('+'));

    {Collect packet without yielding}
    if GetPacket(psGetData) then
      ProcessTransportParams;
  end;

  procedure BPlusProtocol.ProcessFileTransferParams;
    {-Extract Tranfer parameters}
  var
    I : Word;
  begin
    {Start protocol status now}
    apResetStatus;
    apShowFirstStatus;

    {Note direction}
    case RBuffer^[1] of
      'D' : Direction := dDownload;
      'U' : Direction := dUpload;
      else begin
        SendFailure('NUnimplemented Transfer Function');
        APort^.GotError(epNonFatal+ecUnexpectedChar);
      end;
    end;

    {Start timer now...}
    NewTimer(Timer, 1);

    {Verify file type}
    if (RBuffer^[2] <> 'A') and (RBuffer^[2] <> 'B') then begin
      SendFailure('NUnimplemented File Type');
      APort^.GotError(epNonfatal+ecUnexpectedChar);
    end;

    {Retrieve pathname}
    PathName := '';
    I := 2;
    while (RBuffer^[I] <> #0) and (I < RSize-1) do begin
      Inc(I);
      PathName := PathName + Upcase(RBuffer^[I]);
    end;

    case Direction of
      dUpload :
        begin
          LogFile(@Self, lfTransmitStart);

          {Prepare to read file}
          apPrepareReading;
          if AsyncStatus <> ecOk then begin
            SendFailure('AAborted by user');
            if not GetAck then ;
            apShowLastStatus;
            LogFile(@Self, lfReceiveFail);
            AsyncStatus := epNonFatal+ecFileAlreadyExists;
            Exit;
          end;
          FileOfs := 0;
        end;

      dDownLoad :
        begin
          LogFile(@Self, lfReceiveStart);

          if not AcceptFile(@Self) then begin
            APort^.GotError(epNonFatal+ecFileRejected);
            ForceStatus := True;
            SendFailure('AFile rejected');
            if not GetAck then ;
            Exit;
          end;

          {Prepare to write file}
          apPrepareWriting;
          if AsyncStatus mod 10000 = ecFileAlreadyExists then begin
            SendFailure('AAborted by user');
            if not GetAck then ;
            apShowLastStatus;
            LogFile(@Self, lfReceiveFail);
            AsyncStatus := epNonFatal+ecFileAlreadyExists;
            Exit;
          end;
        end;
    end;
  end;

  procedure BPlusProtocol.GetFileTransferPacket;
    {-Collect and process T packet}
  begin
    {Start collecting from body field}
    NextSeq := IncSequence(SeqNum);
    Idx := 1;

    apUpdateBlockCheck(Byte('T'));

    {Collect packet without yielding}
    if GetPacket(psGetData) then
      ProcessFileTransferParams;
  end;

  procedure BPlusProtocol.apUpdateBlockCheck(CurByte : Byte);
    {-Update the CRC/checksum to reflect the new byte}

    function UpdCrc(CurByte : Byte; CurCrc : Word) : Word;
      {-Due to an oddity in the CIS handling of CRC's, we use this special
        version of UpdateCrc rather than the one in APMISC.  This function
        requires the CRC lookup table in APMISC.}
    begin
      UpdCrc := CrcTable[((CurCrc shr 8) xor CurByte) and $FF] xor
                (CurCrc shl 8);
    end;

  begin

    if GetCheckType = bcCrc16 then
      Checksum := UpdCRC(CurByte,Checksum)
    else begin
      {Use classic B's odd checksum method}
      Checksum := Checksum shl 1;
      if Checksum > 255 then
        Checksum := (Checksum and $FF) + 1;
      Checksum := Checksum + CurByte;
      if Checksum > 255 then
        Checksum := (Checksum and $FF) + 1;
    end;
  end;

  procedure BPlusProtocol.SendByte(C : Char);
  begin
    APort^.PutChar(C);
    Inc(SRaw);
  end;

  procedure BPlusProtocol.SendQuotedByte(C : Char);
    {-Quote and transmit I}
  var
    B : Byte absolute C;
  begin
    if QuoteTable[B] <> #0 then begin
      SendByte(cDLE);
      SendByte(QuoteTable[B]);
    end else
      SendByte(Chr(B));
  end;

  procedure BPlusProtocol.apCancel;
  begin
  end;

  procedure BPlusProtocol.bpSendAck;
    {-Send Ack}
  begin
    SendByte(cDLE);
    SendByte(Chr(SeqNum + Ord('0')));
  end;

  procedure BPlusProtocol.SendNAK;
    {-Send Nak}
  begin
    SendByte(cNAK);
  end;

  function BPlusProtocol.IncSequence(Value : Integer) : Integer;
    {-increment a Sequence Number var}
  begin
    IncSequence := (Succ(Value) mod 10);
  end;

  function BPlusProtocol.CollectPacket : Boolean;
    {-Collect a packet}
  var
    C : Char;
  begin
    CollectPacket := False;

    with APort^ do begin
      {Reset char timer each time a new character is received}
      if CharReady then
        NewTimer(ReplyTimer, HandshakeWait);

      {Return error on timeouts}
      if TimerExpired(ReplyTimer) then begin
        {Let caller handle timeout errors}
        GotError(epNonFatal+ecTimeout);
        CollectPacket := True;
        Exit;
      end;

      {Process current packet collection state}
      case PacketState of
        psGetDLE :
          begin
            if CharReady then begin
              GetChar(C);
              case C of
                cDLE : PacketState := psGetB;
                cENQ : PacketState := psSendAck;
              end;
            end;
          end;

        psGetB :
          begin
            if CharReady then begin
              GetChar(C);
              case C of
                'B'  : PacketState := psGetSeq;
                ';'  : PacketState := psGetDLE;
                cEnq : PacketState := psSendAck;
                else   PacketState := psGetDLE;
              end;
            end;
          end;

        psGetSeq :
          begin
            {Reset timer to discount time spent verifying CRCs}
            if ResumeFlag then begin
              NewTimer(Timer, 1);
              RRaw := 2;
            end;

            if CharReady then begin
              GetChar(C);
              case C of
                cEnq : PacketState := psSendAck
                else begin
                  if CheckType = bcCrc16 then
                    CheckSum := $FFFF
                  else
                    CheckSum := 0;
                  apUpdateBlockCheck(Byte(C));
                  PacketNum := Ord(C)-Ord('0');
                  PacketState := psGetType;
                end;
              end;
            end;
          end;

        psGetType :
          if CharReady then begin
            bpGetCharQuoted(C);
            if QuotePending then
              Exit;
            apUpdateBlockCheck(Byte(C));
            LastType := C;
            Idx := 1;
            PacketState := psGetData;
          end;

        psGetData :
          {Stay here while data available...}
          while CharReady do begin
            bpGetCharQuoted(C);
            if QuotePending then
              Exit;
            apUpdateBlockCheck(Byte(C));
            if (C = cETX) and not Quoted then begin
              PacketState := psGetCheck1;
              Exit;
            end else begin
              RBuffer^[Idx] := C;
              Inc(Idx);
            end;
          end;

        psGetCheck1 :
          if CharReady then begin
            bpGetCharQuoted(C);
            if QuotePending then
              Exit;
            if CheckType = bcCrc16 then begin
              apUpdateBlockCheck(Byte(C));
              PacketState := psGetCheck2;
            end else begin
              NewChk := Byte(C);
              PacketState := psCheckCheck;
            end;
          end;

        psGetCheck2 :
          if CharReady then begin
            bpGetCharQuoted(C);
            if QuotePending then
              Exit;
            apUpdateBlockCheck(Byte(C));
            NewChk := 0;
            PacketState := psCheckCheck;
          end;

        psCheckCheck :
          begin
            if NewChk <> Checksum then begin
              {Checksum/CRC error}
              GotError(epNonFatal+ecBlockCheckError);
              ForceStatus := True;
              PacketState := psError;
            end else if LastType = 'F' then
              {Always accept failure packet}
              PacketState := psSuccess
            else if PacketNum = SeqNum then begin
              {Dupe packet}
              GotError(epNonFatal + ecDuplicateBlock);
              ForceStatus := True;
              PacketState := psSendAck;
            end else if PacketNum <> NextSeq then begin
              {Out-of-sequence error...}
              if PacketNum <> IncSequence(NextSeq) then begin
                {...and not a possible SA packet after error}
                GotError(epNonFatal + ecSequenceError);
                ForceStatus := True;
                PacketState := psGetDLE;
              end else
                PacketState := psGetDLE;
            end else
              PacketState := psSuccess;
          end;

        psError :
          begin
            Inc(TotalErrors);
            Inc(BlockErrors);
            if (BlockErrors > BPErrorMax) then begin
              CollectPacket := True;
              Exit;
            end;
            if not NAKSent or not BPlusMode then begin
              NAKSent := True;
              SendNAK;
            end;
            PacketState := psGetDLE;
          end;

        psSendAck :
          begin
            if not Aborting then
              bpSendAck;
            PacketState := psGetDLE;
          end;

        psSuccess :
          begin
            if not Aborting then
              SeqNum := PacketNum;
            ResumeFlag := False;
            RSize := Idx;
            Inc(RPackets);
            CollectPacket := True;
          end;
      end;
    end;
  end;

  procedure BPlusProtocol.SendData(BNum : Integer);
  var
    I : Integer;
  begin
    with SBuffer[BNum] do begin
      if BPlusMode and (GetCheckType = bcCrc16) then
        Checksum := $FFFF
      else
        Checksum := 0;

      SendByte(cDLE);
      SendByte('B');

      SendByte(Chr(Seq+Ord('0')));
      apUpdateBlockCheck(Byte(Seq+Ord('0')));

      SendByte(PType);
      apUpdateBlockCheck(Byte(PType));

      for I := 1 to Num do begin
        SendQuotedByte(Buf^[I]);
        apUpdateBlockCheck(Byte(Buf^[I]));
      end;

      SendByte(cETX);
      apUpdateBlockCheck(Byte(cETX));

      if BPlusMode and (GetCheckType = bcCrc16) then
        SendQuotedByte(Char(Hi(Checksum)));
      SendQuotedByte(Char(Lo(Checksum)));
    end;
  end;

  function BPlusProtocol.IncSA(Value : Integer) : Integer;
  begin
    if Value = SAMax then
      IncSA := 1                                                       {!!.01}
    else
      IncSA := Value + 1;
  end;

  function BPlusProtocol.CollectAck : Boolean;
    {-Collect acknowledgement to last packet}
  var
    SAIdx : Integer;
    C : Char;
    W : Word;
    I : Word;
  begin
    with APort^ do begin

      CollectAck := False;

      {Restart the character timer with each character}
      if CharReady and not Aborting then
        NewTimer(ReplyTimer, HandshakeWait);

      if TimerExpired(ReplyTimer) and (AckState <> acSendENQ) then
        AckState := acTimeout;

      case AckState of
        acGetDLE :
          if CharReady then begin
            GetChar(C);
            case C of
              cDLE : AckState := acGetNum;           {Potential ACK}
              cNAK : AckState := acSendENQ;          {Packet error}
              cETX : AckState := acSendNAK;          {Sequence problem}
            end;
          end;

        acGetNum :
          if CharReady then begin
            GetChar(C);
            SaveC := C;
            case C of
              '0'..'9' : AckState := acHaveAck;     {Have ACK, check it}
              'B'      : if Aborting then
                           AckState := acSkipPacket1
                         else
                           AckState := acGetPacket;
              cNak     : AckState := acSendEnq;
              ';'      : begin
                           GotError(epNonFatal+ecWaitAck);
                           ForceStatus := True;
                           AckState := acGetDLE;
                         end;
              else       AckState := acGetDLE;
            end;
          end;

        acGetPacket :
          begin
            {Prepare to collect packet}
            BPlusState := rbCollectPacket;
            BlockErrors := 0;
            PacketState := psGetSeq;
            AckState := acCollectPacket;
          end;

        acCollectPacket :
          if APort^.CharReady then begin
            if CollectPacket then begin
              {Got a complete packet -- finished here}
              if AsyncStatus = ecOk then begin
                BlockErrors := 0;
                CollectAck := True;
              end else
                {Error getting packet, retry}
                AckState := acGetDLE;
            end;
          end else if TimerExpired(ReplyTimer) then
            AckState := acGetDLE;

        acSkipPacket1 :
          if CharReady then begin
            GetChar(C);
            if C = cETX then begin
              bpGetCharQuoted(C);
              if QuotePending then
                AckState := acSkipPacket2
              else if CheckType = bcCrc16 then
                AckState := acSkipPacket3
              else
                AckState := acGetDLE;
            end;
          end;

        acSkipPacket2 : {Collect 2nd byte of 1st check byte}
          if CharReady then begin
            bpGetCharQuoted(C);
            AckState := acSkipPacket3;
          end;

        acSkipPacket3 : {Collect 2nd check byte}
          if CharReady then begin
            bpGetCharQuoted(C);
            if QuotePending then
              AckState := acSkipPacket4
            else
              AckState := acGetDLE;
          end;

        acSkipPacket4 : {Collect 2nd byte of 2st check byte}
          if CharReady then begin
            bpGetCharQuoted(C);
            AckState := acGetDLE;
          end;

        acHaveACK :
           begin
            PacketNum := Byte(SaveC) - Byte('0');
            if SBuffer[Next2ACK].Seq = PacketNum then begin
              {Expected ACK}
              Next2ACK := IncSA(Next2ACK);
              Dec(SAWaiting);
              if SAErrors > 0 then
                Dec(SAErrors);
              CollectACK := True;
              AckState := acGetDLE;
              Exit;
            end else if (SBuffer[IncSA(Next2ACK)].Seq = PacketNum) and
                        (SAWaiting = 2) then begin
              {Missed ACK}
              GotError(epNonFatal+ecSequenceError);
              Dec(SAWaiting, 2);

              {Inc twice to skip the miss}
              Next2ACK := IncSA(Next2ACK);
              Next2ACK := IncSA(Next2ACK);
              if SAErrors > 0 then
                Dec(SAErrors);
              CollectACK := True;
              AckState := acGetDLE;
              Exit;
            end else if SBuffer[Next2ACK].Seq = IncSequence(PacketNum) then begin
              if SentENQ then
                {Remote missed first packet}
                AckState := acSendData
              else
                {Duplicate ACK}
                AckState := acGetDLE;
            end else begin
              if Aborting then
                AckState := acGetDLE
              else
                AckState := acTimeOut;
            end;
            SentENQ := False;
          end;

        acTimeout :
          begin
            GotError(epNonFatal+ecTimeout);
            ForceStatus := True;
            AckState := acSendENQ;
            NewTimer(ReplyTimer, HandshakeWait);
          end;

        acSendNAK :
          begin
            Inc(BlockErrors);
            Inc(TotalErrors);
            if (BlockErrors > BPErrorMax) or Aborting then begin
              CollectAck := True;
              Exit;
            end;
            SendNAK;
            AckState := acGetDLE;
          end;

        acResync1 :
          if CharReady then begin
            GetChar(C);
            if C = cDLE then
              AckState := acResync2
          end;

        acResync2 :
          if CharReady then begin
            GetChar(C);
            case C of
              'B' : if Aborting then
                      AckState := acSkipPacket1
                    else
                      AckState := acGetPacket;
              '0'..'9' : AckState := acResync3;
            end;
          end;

        acResync3 :
          if CharReady then begin
            GetChar(C);
            if C = cDLE then
              AckState := acResync4
          end;

        acResync4 :
          if CharReady then begin
            GetChar(C);
            case C of
              'B' : if Aborting then
                      AckState := acSkipPacket1
                    else
                      AckState := acGetPacket;
              '0'..'9' : AckState := acHaveAck;
            end;
          end;

        acSendENQ :
          begin
            Inc(BlockErrors);
            Inc(TotalErrors);
            if (BlockErrors > BPErrorMax) or Aborting then begin
              APort^.GotError(epNonFatal+ecTooManyErrors);
              CollectACK := True;
              Exit;
            end;

            SendByte(cENQ);
            SendByte(cENQ);
            AckState := acResync1;
            SentEnq := True;
          end;

        acSendData :
          begin
            Inc(SAErrors, 3);
            if SAErrors >= 12 then
              {If too many SA errors, cease SendAhead}
              SAMax := 1;

            {Flush all pending packets to send}
            SAIdx := Next2ACK;
            for I := 1 to SAWaiting do begin
              SendData(SAIdx);
              SAIdx := IncSA(SAIdx);
            end;
            SentENQ := False;
            AckState := acGetDLE;
          end;

        acFailed :
          begin
            CollectAck := True;
            AckState := acGetDLE;
          end;
      end;
    end;
  end;

  procedure BPlusProtocol.SendPacket(PacketType : Char; Size : Integer);
    {-Send a packet of data}
  begin
    SeqNum := IncSequence(SeqNum);
    with SBuffer[Next2Fill] do begin
      Seq   := SeqNum;
      Num   := Size;
      PType := PacketType;
    end;
    SendData(Next2Fill);
    Next2Fill := IncSA(Next2Fill);
    Inc(SAWaiting);
    Inc(SPackets);
  end;

  procedure BPlusProtocol.SendFailure(Reason : String);
    {-Send a failure packet}
  begin
    Next2ACK := 1;
    Next2Fill := 1;
    SAWaiting := 0;
    Aborting := True;

    with SBuffer[1] do
      Move(Reason[1], Buf^[1], Length(Reason));

    SendPacket('F', Length(Reason));
  end;

  procedure BPlusProtocol.apAbortProtocol;
    {-Aborts the protocol}
  var
    SaveStat : Word;
  begin
    SaveStat := AsyncStatus;
    APort^.GotError(epFatal+ecCancelRequested);
    if SaveStat <> ecUserAbort then
      AsyncStatus := SaveStat;
  end;

  procedure BPlusProtocol.bpInitVars;
    {-Init vars that need resetting each time a DLE is seen}
  begin
    Next2ACK  := 1;
    Next2Fill := 1;
    SAWaiting := 0;
    SAMax     := 1;
    AbortCount:= 0;
    RPackets  := 0;
    RRaw      := 0;
    SPackets  := 0;
    SRaw      := 0;
    TotalErrors := 0;
    ResumeFlag := False;
  end;

  procedure BPlusProtocol.apPrepareWriting;
    {-Opens a file to receive, handles resume/overwrite request}
  label
    ExitPoint;
  var
    Dir : DirStr;
    Name : NameStr;
    Ext : ExtStr;
    Result : Word;
    OvrW : Boolean;
    ET : EventTimer;
    I : Integer;
    S,T : PathStr;
    F : LongInt;
  begin
    {!! moved to constructor}
    {Allocate a file buffer}
    {if not GetMemCheck(FileBuffer, FileBufferSize) then begin
      APort^.GotError(ecOutOfMemory);
      ForceStatus := True;
      Exit;
    end;}

    {Does the file exist already?}
    ResumeFlag := False;
    FileOpen := False;
    OvrW := False;

    SaveMode := FileMode;                                              {!!.02}
    FileMode := AproFileMode;                                   {!!.02}{!!.03}
    Assign(WorkFile, PathName);
    Reset(WorkFile, 1);
    FileMode := SaveMode;                                              {!!.02}
    Result := IOResult;

    {Exit on errors other than FileNotFound}
    if (Result <> 0) and (Result <> 2) and (Result <> 110) then begin
      APort^.GotError(epFatal+Result);
      goto ExitPoint;
    end;

    {If file exists process potential resume}
    if Result = 0 then begin
      HandleResume(@Self);
      case WriteFailOpt of
        WriteFail :
          begin
            APort^.GotError(epNonFatal+ecFileAlreadyExists);
            goto ExitPoint;
          end;
        WriteResume :
          ResumeFlag := True;
        WriteRename :
          APort^.GotError(epNonFatal+ecFileRenamed);
        WriteAnyway :
          OvrW := True;
      end;
    end;

    if ResumeFlag then begin
      {Calculate CRC on existing file's contents}
      APort^.GotError(epNonFatal+ecTryResume);
      apUserStatus(False, False);
      NewTimer(ET, Secs2Tics(SecsPerDay));
      F := FileSize(WorkFile);

      with SBuffer[Next2Fill] do begin
        Seek(WorkFile, 0);
        Checksum := $FFFF;
        repeat
          BlockRead(WorkFile,Buf^[1], 512, Result);
          for I := 1 to (Result) do
            apUpdateBlockCheck(Byte(Buf^[I]));
          if ElapsedTimeInSecs(ET) >= 10 then begin
            {Send WACK so host knows we're busy}
            NewTimer(ET, 1);
            SendByte(cDLE);
            SendByte(';');
            APort^.GotError(epNonFatal+ecTryResume);
            apUserStatus(False, False);
          end;
        until (Result = 0) or (IOResult <> 0);

        {Send the host a "Tr" packet with our info}
        FillChar(Buf^, SizeOf(Buf^), 0);
        Buf^[1] := 'r';

        {Send filesize and CRC}
        S := Long2Str(F) + ' ' + Long2Str(Checksum) + ' ';
        Move(S[1], Buf^[2], Length(S));
        SendPacket('T', Length(S)+1);

        {Collect ACK here}
        if not GetAck then ;

        {Assume resuming....}
        APort^.GotError(epNonFatal+ecHostResume);
        apUserStatus(False, False);
        FileOfs := F;
        BytesTransferred := F;
        InitFilePos := F;
        StartOfs := F;
        LastOfs := F;
        EndOfs := StartOfs + FileBufferSize;

        Seek(WorkFile, F);
        AsyncStatus := IOResult;
        if AsyncStatus <> 0 then begin
          Inc(AsyncStatus,epNonFatal);
          goto ExitPoint;
        end;
        FileOpen := True;
        Exit;
      end;
    end else begin
      Close(WorkFile);
      if IOResult = 0 then ;

      {Change the file name if needed}
      if (Result = 0) and not ResumeFlag and not OvrW then begin
        FSplit(Pathname, Dir, Name, Ext);
        Name[1] := '$';
        Pathname := Dir + Name + Ext;
        APort^.GotError(epNonFatal+ecFileRenamed);
      end;

      {Give status a chance to show that the file was renamed}
      apUserStatus(False, False);
      AsyncStatus := ecOk;

      {Ok to rewrite file now}
      Assign(WorkFile, Pathname);
      Rewrite(WorkFile, 1);
      Result := IOResult;
      if Result <> 0 then begin
        APort^.GotError(epFatal+Result);
        goto ExitPoint;
      end;

      {Acknowledge the T packet}
      bpSendAck;

      {Initialized the buffer management vars}
      BytesTransferred := 0;
      BytesRemaining := 0;
      FileOfs := 0;
      StartOfs := 0;
      LastOfs := 0;
      EndOfs := StartOfs + FileBufferSize;
      FileOpen := True;
      Exit;
    end;

ExitPoint:
    Close(WorkFile);
    if IOResult <> 0 then ;
  end;

  procedure BPlusProtocol.bpHandleResumeFail;
    {-Resume failed, rewrite the file}
  var
    Dir : DirStr;
    Name : NameStr;
    Ext : ExtStr;
    Result : Word;
  begin
    Close(WorkFile);

    {If we default to Rename, rename the file}
    if WriteFailOpt = WriteRename then begin
      FSplit(Pathname, Dir, Name, Ext);
      Name[1] := '$';
      Pathname := Dir + Name + Ext;
      Assign(WorkFile, PathName);
      APort^.GotError(epNonFatal+ecFileRenamed);
    end;

    {Otherwise just overwrite}
    Rewrite(WorkFile, 1);
    Result := IoResult;
    if Result <> 0 then begin
      FileOpen := False;
      APort^.GotError(epNonFatal+Result);
      apUserStatus(False, False);
      SendFailure('CCannot create file');
      Exit;
    end;

    {Set status vars}
    BytesTransferred := 0;
    APort^.GotError(epNonFatal+ecResumeBad);
    apUserStatus(False, False);
    ResumeFlag := False;
    RPackets := 0;
    SPackets := 0;
    bpSendAck;
    NewTimer(Timer, 1);
    FileOfs := 0;
    InitFilePos := 0;
  end;

  procedure BPlusProtocol.PrepareTransmitPart;
    {-Prepare to transmit file in parts}
  begin
    AbstractProtocol.PrepareTransmitPart;

    {Set initial state}
    BPlusState := tbInitial;
    AsyncStatus := ecOk;
    SaveStatus := ecOk;
    ErrorStatus := ecOk;
  end;

  function BPlusProtocol.ProtocolTransmitPart : ProtocolStateType;
    {-Perform one increment of a protocol transmit}
  label
    ExitPoint;
  begin
    with APort^ do begin

      {Restore previous AsyncStatus}
      AsyncStatus := SaveStatus;

      {Check for user abort}
      case SaveStatus of
        ecCancelRequested,
        ecFileRejected : ;
        else begin
          if apHandleAbort then begin
            if BPlusState = tbWaitErrorAck then
              BPlusState := tbCleanup
            else begin
              {Send failure packet}
              SendFailure('AAborted by user');
              AsyncStatus := ecCancelRequested;
              BPlusState := tbError;
            end;
            ForceStatus := True;
          end;
        end;
      end;

      {Show status at requested intervals and after significant events}
      if ForceStatus or TimerExpired(StatusTimer) then begin
        if ErrorStatus <> ecOk then
          AsyncStatus := ErrorStatus;
        ElapsedTics := ElapsedTime(Timer);
        apUserStatus(False, False);
        NewTimer(StatusTimer, StatusInterval);
        ForceStatus := False;
      end;

      {Main state processor}
      case BPlusState of
        tbInitial :
          begin
            NewTimer(ReplyTimer, HandshakeWait);
            BPlusState := tbGetBlock;
            LastBlock := False;
          end;

        tbGetBlock :
          if LastBlock then
            BPlusState := tbEndOfFile
          else with SBuffer[Next2Fill] do begin
            if PR^.Buffered then                                       {!!.01}
              if (PR^.OutBuffLen > 4096+10) and                        {!!.01}
                 (FlagIsSet(apFlags, apBP2KTransmit)) then             {!!.01}
                Num := BlockLen                                        {!!.01}
              else                                                     {!!.01}
                Num := 1024                                            {!!.01}
            else                                                       {!!.01}
              Num := BlockLen;                                         {!!.01}
            LastBlock :=                                               {!!.01}
              apReadProtocolBlock(DataBlockTypePtr(Buf)^, Num);        {!!.01}
            if AsyncStatus = ecOk then begin
              Inc(FileOfs, Num);
              BPlusState := tbWaitFreeSpace;
              NewTimer(ReplyTimer, TransTimeout);
            end else begin
              SaveStatus := AsyncStatus;
              SendFailure('EFile read failure');
              BPlusState := tbError;
            end;
          end;

        tbWaitFreeSpace :
          with SBuffer[Next2Fill] do begin
            if OutBuffFree > (Num*2)+10 then
              BPlusState := tbSendData
            else if TimerExpired(ReplyTimer) then begin
              SendFailure('ETimeout waiting for output buffer space');
              BPlusState := tbError;
            end;
          end;

        tbSendData :
          with SBuffer[Next2Fill] do begin
            SendPacket('N', Num);
            LastBlockSize := Num;
            ForceStatus := True;

            if SAWaiting = 0 then begin
              NewTimer(ReplyTimer, HandshakeWait);
              AckState := acGetDLE;
            end;
            BPlusState := tbCheckAck;
          end;

        tbCheckAck :
          if SAWaiting < SAMax then
            BPlusState := tbGetBlock
          else if CollectAck then begin
            if AsyncStatus = ecOk then begin
              BPlusState := tbGetBlock;
              Inc(BytesTransferred, LastBlockSize);
              Dec(BytesRemaining, LastBlockSize);
            end else begin
              ForceStatus := True;
              SendFailure('EToo many errors');
              BPlusState := tbError;
            end;
          end;

        tbEndOfFile :
          begin
            {Send TransferComplete packet}
            with SBuffer[Next2Fill] do begin
              Buf^[1] := 'C';
              SendPacket('T', 1);
              NewTimer(ReplyTimer, HandshakeWait);
              BPlusState := tbEofAck;
            end;
            if SAWaiting = 0 then begin
              NewTimer(ReplyTimer, HandshakeWait);
              AckState := acGetDLE;
            end;
          end;

        tbEofAck :
          if CollectAck then
            if AsyncStatus = ecOK then
              BPlusState := tbCleanup
            else begin
              SendFailure('EToo many errors');
              ForceStatus := True;
              BPlusState := tbError;
            end;

        tbError :
          begin
            {Save failure status}
            ErrorStatus := AsyncStatus;

            {Start waiting for acknowledgment (failure packet already sent)}
            BPlusState := tbWaitErrorAck;
            AckState := acGetDLE;
            NewTimer(ReplyTimer, FinishWait);
          end;

        tbWaitErrorAck :
          if CollectAck then begin
            AsyncStatus := ErrorStatus;
            ForceStatus := True;
            BPlusState := tbCleanup;
          end;

        tbCleanup :
          begin
            SaveStatus := AsyncStatus;
            apFinishReading;
            AsyncStatus := SaveStatus;

            {Log file}
            if AsyncStatus = ecOk then
              LogFile(@Self, lfTransmitOK)
            else
              LogFile(@Self, lfTransmitFail);

            apShowLastStatus;
            APort^.FlushInBuffer;
            APort^.PR^.ProtocolActive := False;
            BPlusState := tbDone;
          end;
      end;

ExitPoint:
      {Set function result}
      case BPlusState of
        tbWaitFreeSpace,
        tbCheckAck,
        tbEofAck        : ProtocolTransmitPart := psWaiting;

        tbInitial,
        tbGetBlock,
        tbSendData,
        tbEndOfFile,
        tbError,
        tbCleanup       : ProtocolTransmitPart := psReady;

        tbDone          : ProtocolTransmitPart := psFinished;
      end;
    end;

    {Store AsyncStatus}
    SaveStatus := AsyncStatus;
  end;

  procedure BPlusProtocol.PrepareReceivePart;
    {-Prepare to receive BPlus parts}
  begin
    {Do parent inits}
    AbstractProtocol.PrepareReceivePart;

    {Init the status timer}
    NewTimer(StatusTimer, StatusInterval);

    {Init the state machine}
    BPlusState := rbInitial;
    AsyncStatus := ecOk;
    SaveStatus := ecOk;
    ErrorStatus := ecOk;
  end;

  function BPlusProtocol.ProtocolReceivePart : ProtocolStateType;
    {-Perform one increment of a protocol receive}
  label
    ExitPoint;
  var
    BlockSize : Word;
    Handshake : Char;
    C : Char;
    TempStatus : Word;
    I : Word;
    S : String;
    SaveSize : LongInt;
  begin
    with APort^ do begin

      {Restore previous AsyncStatus}
      AsyncStatus := SaveStatus;

      {Check for user abort}
      case SaveStatus of
        ecCancelRequested,
        ecFileRejected : ;
        else begin
          if apHandleAbort then begin
            if BPlusState = rbWaitErrorAck then
              BPlusState := rbCleanup
            else begin
              {Send failure packet}
              SendFailure('AAborted by user');
              AsyncStatus := ecCancelRequested;
              BPlusState := rbError;
            end;
            ForceStatus := True;
          end;
        end;
      end;

      {Show status at requested intervals and after significant events}
      if ForceStatus or TimerExpired(StatusTimer) then begin
        if ErrorStatus <> ecOk then
          AsyncStatus := ErrorStatus;
        apUserStatus(False, False);
        NewTimer(StatusTimer, StatusInterval);
        ForceStatus := False;
      end;

      {Main state processor}
      case BPlusState of
        rbInitial :
          begin
            {Start waiting for first packet}
            NewTimer(ReplyTimer, HandshakeWait);
            ErrorStatus := ecOK;

            BPlusState := rbGetDLE;
          end;

        rbGetDLE :
          if CharReady then begin
            GetChar(C);
            if C = cDLE then
              BPlusState := rbGetB
            else
              BPlusState := rbGetDLE;
          end else if TimerExpired(ReplyTimer) then begin
            BPlusState := rbSendEnq;
          end;

        rbGetB :
          if CharReady then begin
            GetChar(C);
            if C = 'B' then begin
              BPlusState := rbCollectPacket;
              NAKsent := False;
              NextSeq := IncSequence(SeqNum);
              BlockErrors := 0;
              PacketState := psGetSeq;
            end else begin
              BPlusState := rbSendEnq;
            end;
          end else if TimerExpired(ReplyTimer) then begin
            BPlusState := rbSendEnq;
          end;

        rbCollectPacket :
          if CollectPacket then begin
            {Got a complete packet -- process it}
            if AsyncStatus = ecOk then begin
              BlockErrors := 0;
              BPlusState := rbProcessPacket;
              ForceStatus := True;
            end else begin
              {Timeout or too many block errors, let rbSendEnq handle}
              BPlusState := rbSendEnq;
              NewTimer(ReplyTimer, HandshakeWait);
            end;
          end;

        rbProcessPacket :
          with APort^ do begin
            ForceStatus := True;

            case LastType of
              'N':  {Next data packet, write it to file}
                begin
                  {Call the write method to write this block}
                  Failed := apWriteProtocolBlock(
                            DataBlockTypePtr(RBuffer)^, RSize-1);      {!!.01}

                  {Process result}
                  if Failed then begin
                    TempStatus := AsyncStatus;
                    SendFailure('EWrite failure');
                    AsyncStatus := TempStatus;
                    ForceStatus := True;
                    BPlusState := rbError;
                  end else begin
                    Inc(FileOfs, RSize-1);
                    Dec(BytesRemaining, RSize-1);
                    Inc(BytesTransferred, RSize-1);
                    ElapsedTics := ElapsedTime(Timer);
                    bpSendAck;

                    {Prepare to get next packet}
                    BPlusState := rbGetDLE;
                    NewTimer(ReplyTimer, HandshakeWait);
                  end;
                end;

              'T':     {Transfer control packet, process per second byte}
                begin
                  case RBuffer^[1] of
                    'C':   {Transfer Complete packet}
                      begin
                        apFinishWriting;
                        bpSendAck;
                        BPlusState := rbCleanup;
                      end;

                    'I':   {Transfer Info packet, we only use FileSize field here}
                      begin
                        bpSendAck;
                        I := 4;
                        S := '';
                        while (I <= RSize-1) and
                              (RBuffer^[I] >= '0') and
                              (RBuffer^[I] <= '9') do begin
                          S := S + RBuffer^[I];
                          Inc(I);
                        end;
                        Val(S, BytesRemaining, I);
                        if I <> 0 then
                          BytesRemaining := 0;
                        SrcFileLen := BytesRemaining;

                        {Reset packet counts to reflect data}
                        RPackets := 0;
                        SPackets := 0;

                        {Get next packet}
                        BPlusState := rbGetDLE;
                      end;

                    'f':   {Host Failed Resume, rewrite the file}
                      begin
                        bpHandleResumeFail;
                        BPlusState := rbGetDLE;
                      end;

                    else begin
                        {Unknown T packet type}
                        APort^.GotError(epNonFatal + ecUnexpectedChar);
                        SendFailure('NInvalid T Packet');
                        BPlusState := rbError;
                      end;
                  end;
                end;

              'F':    {Failure packet, exit immediately}
                begin
                  APort^.GotError(epNonFatal+ecHostCan);
                  bpSendAck;
                  BPlusState := rbCleanup;
                end;
             else begin
                  {Unsupported packet type, exit immediately}
                  APort^.GotError(epNonFatal+ecUnexpectedChar);
                  SendFailure('NUnknown packet type');
                  BPlusState := rbError;
                end;
            end;
          end;

        rbSendEnq :
          begin
            APort^.GotError(epNonFatal+ecTimeout);
            Inc(BlockErrors);
            Inc(TotalErrors);
            if BlockErrors > BPErrorMax then begin
              SendFailure('ATimeout');
              BPlusState := rbError;
            end else
              BPlusState := rbGetDLE;
          end;

        rbError :
          begin
            {Save failure status}
            ErrorStatus := AsyncStatus;

            {Start waiting for acknowledgment (failure packet already sent)}
            BPlusState := rbWaitErrorAck;
            AckState := acGetDLE;
            NewTimer(ReplyTimer, FinishWait);
          end;

        rbWaitErrorAck :
          if CollectAck then begin
            AsyncStatus := ErrorStatus;
            ForceStatus := True;
            BPlusState := rbCleanup;
          end;
        rbCleanup :
          begin
            {Close file}
            SaveStatus := AsyncStatus;
            SaveSize := SrcFileLen;
            apFinishWriting;
            AsyncStatus := SaveStatus;
            SrcFileLen := SaveSize;

            {Log receive status}
            if AsyncStatus <> ecOk then
              LogFile(@Self, lfReceiveFail)
            else
              LogFile(@Self, lfReceiveOK);

            apShowLastStatus;
            APort^.FlushInBuffer;
            APort^.PR^.ProtocolActive := False;
            BPlusState := rbDone;
          end;
      end;

ExitPoint:
      {Set function result}
      case BPlusState of
        rbFinished,
        rbCleanup,
        rbProcessPacket,
        rbSendEnq,
        rbError :                 ProtocolReceivePart := psReady;

        rbInitial,
        rbGetDLE,
        rbGetB,
        rbWaitErrorAck,
        rbCollectPacket :         ProtocolReceivePart := psWaiting;

        rbDone :                  ProtocolReceivePart := psFinished;
      end;
      {Store AsyncStatus}
      SaveStatus := AsyncStatus;
    end;
  end;

  {$IFDEF UseStreams}
  constructor BPlusProtocol.Load(var S : IdStream);
    {-Load an BPlusProtocol object from a stream}
  var
    I : Word;
  begin
    {Init pointers}
    RBuffer := nil;
    FillChar(SBuffer, SizeOf(SBuffer), 0);

    {Load parents}
    if not AbstractProtocol.Load(S) then begin
      Done;
      Fail;
    end;

    {Allocate buffers}
    Failed := not GetMemCheck(RBuffer, BPBufferMax);
    I := 1;
    while not Failed and (I <= BPSendaheadMax) do begin
      Failed := not GetMemCheck(SBuffer[I].Buf, BPBufferMax);
      Inc(I);
    end;
    if Failed then begin
      Done;
      APort^.GotError(epFatal+ecOutOfMemory);
      Fail;
    end;

    {Init fields not loaded from stream}
    RRaw      := 0;
    RPackets  := 0;
    SRaw      := 0;
    SPackets  := 0;
    case ActCPS of
      0..30   : DefBS := 1;
      31..120 : DefBS := 4;
      else      DefBS := 16;
    end;
    QuotePending := False;
    SentEnq := False;

    {Load BPlus specific data}
    S.ReadRange(FinishWait, SentEnq);

    {Load the HandleResume pointer}
    @HandleResume := S.ReadUserPointer(@NoHandleResume);
  end;

  procedure BPlusProtocol.Store(var S : IdStream);
    {-Store a BPlusProtocol object to a stream}
  begin
    {Store parents}
    AbstractProtocol.Store(S);

    {Store BPlus specific data}
    S.WriteRange(FinishWait, SentEnq);

    {Store the handle resume pointer}
    S.WriteUserPointer(@HandleResume, ptNoHandleResume);
  end;

  procedure BPlusProtocolStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing bplus objects}
  begin
    AbstractProtocolStream(SPtr);
    SPtr^.RegisterType(otBPlusProtocol, veBPlusProtocol,
                       TypeOf(BPlusProtocol),
                       @BPlusProtocol.Store, @BPlusProtocol.Load);
  end;
  {$ENDIF}

  procedure NoHandleResume(AP : BPlusProtocolPtr);
    {-Empty HandleResume procedure}
  begin
  end;

end.
