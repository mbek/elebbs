{$S-,R-,V-,I-,B-,F+,O+,A-}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{Include OPro's define file if UseOPro is specified}
{$IFDEF UseOPro}
{$I OPDEFINE.INC}
{$ENDIF}

{*********************************************************}
{*                   OOXMODEM.PAS 2.03                   *}
{*     Copyright (c) TurboPower Software 1991.           *}
{* Portions copyright (c) Information Technology 1989,   *}
{*    and used under license to TurboPower Software      *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OoXmodem;
  {-Provides Xmodem/Crc/1K recieve and transmit functions (using OOP)}

interface

uses
  {$IFNDEF DELPHI}
    Dos,
  {$ELSE}
    GenDos,
  {$ENDIF}
  {$IFDEF UseOpro}
  OpString,
  OpRoot,
  {$ENDIF}
  {$IFDEF UseTpro}
  TpMemChk,
  TpString,
  {$ENDIF}
  ApMisc,
  ApPort,
  ApTimer,
  OoCom,
  OoAbsPcl;

const
  {Compile-time constants}
  RelaxedHandShakeWait = 364;    {Relaxed wait time during handshaking (20 sec)}
  DefBlockWait = 91;             {Normal between-block wait time (5 sec)}
  RelaxedBlockWait = 182;        {Relaxed between-block wait time (10 sec)}
  CancelTimeout = 3;             {Drain timeout for cancel sequence}
  MaxSkipChars = 1000;           {Maximum chars skipped while seeking Soh}
  DrainWait = 1092;              {OutBuf drain time before error (60 sec)}
  MaxCrcTry = 3;                 {Max tries for Crc before trying checksum}
  CrcReq = 'C';                  {Crc mode request}

  {Run-time constants}
  DefMaxBlockErrors : Byte = 5;  {Default maximum acceptable errors per block}
  DefFinishWait     : Word = DefBlockWait*2; {Wait GMode EOT response}

  {For estimating protocol transfer times}
  XmodemOverhead : Word = 5;     {Overhead bytes for each block}
  XmodemTurnDelay : Word = 1000; {MSec turnaround delay for each block}

type
  {Xmodem protocol transmit states}
  XmodemStateType = (
    {Transmit states}
    txInitial,              {0  Open file, log it, etc.}
    txHandshake,            {1  Waiting for handshake}
    txGetBlock,             {2  Get the next block to transmit}
    txWaitFreeSpace,        {3  Wait until outbuffer has enough freespace}
    txSendBlock,            {4  Send next protocol block}
    txDraining,             {5  Waiting for protocol block to drain}
    txReplyPending,         {6  Waiting for reply to last block}
    txEndDrain,             {7  Wait for output buffer to drain before EOT}
    txFirstEndOfTransmit,   {8  Send first EOT}
    txRestEndOfTransmit,    {9  Send subseqent EOTs}
    txEotReply,             {10  Waiting for EOT reply}
    txFinished,             {11 Close file, log it, etc.}
    txDone,                 {12 Signal end of protocol}

    {Receive states}
    rxInitial,              {13 Initialize vars, get buffers, etc.}
    rxWaitForHSReply,       {14 Waiting for 1st reply to handshake}
    rxWaitForBlockStart,    {15 Wait for block start}
    rxCollectBlock,         {16 Collect data}
    rxProcessBlock,         {17 Process block}
    rxFinishedSkip,         {18 Close file, log as skip}
    rxFinished,             {19 Close file, log as good/bad}
    rxDone);                {20 Signal end of protocol}

  {An xmodem/Crc/1K protocol object}
  XmodemProtocolPtr = ^XmodemProtocol;
  XmodemProtocol = object(AbstractProtocol)
    StartChar      : Char;              {Block start character}
    OneKMode       : Boolean;           {True for XModem1K}
    BlockWait      : Byte;              {Wait seconds between blocks}
    MaxBlockErrors : Byte;              {Max number of allowed block errors}
    FirstBlockNum  : Byte;              {First block number to use}
    GMode          : Boolean;           {True if streaming}
    TimerPending   : Boolean;           {True if waiting to start rcv timer}
    FinishWait     : Word;              {Extra wait GMode EOT response}

    {Temp vars that state machine requires to be static}
    HandshakeChar  : Char;              {Last handshake char we sent}
    NaksReceived   : Byte;              {Count naks received}
    EotCounter     : Byte;              {Counts received EOTs}
    CanCounter     : Byte;              {Counts received cCans}
    ReplyTimer     : EventTimer;        {Track timeouts waiting for replies}
    NoMoreData     : Boolean;           {Flag for tracking end-of-file}
    DataBlock      : ^DataBlockType;    {Data block}
    CharsLeft      : Word;              {Characters not yet xmitted}
    OutBufPos      : Word;              {Output buffer pos for reduced blocks}
    BlkIndex       : Word;              {Index into received chars in DataBlock}
    OverheadLen    : Byte;              {Number of overhead bytes per block}
    LastBlockSize  : Word;              {Number of bytes in last read block}

    {State information}
    XmodemState    : XmodemStateType;   {Current state of Xmodem}

    constructor Init(APPtr : AbstractPortPtr; Use1K, UseGMode : Boolean);
      {-Allocates and initializes a protocol control block}
    constructor InitCustom(APPtr : AbstractPortPtr;
                           Use1K, UseGMode : Boolean;
                           Options : Word);
      {-Allocates and initializes a protocol control block with options}
    destructor Done; virtual;                                          {!!.01}
      {-Deallocates heap space}
    procedure Set1KMode(Enable : Boolean);
      {-Enable/disable Xmodem1K (aka Ymodem)}
    procedure SetGMode(Enable : Boolean);
      {-Enable/disable streaming}
    procedure SetBlockWait(NewBlockWait : Byte);
      {-Set inter-block wait time}
    procedure SetFinishWait(NewFinishWait : Word);
      {-Set additional finish wait (time to wait for EOT response)}
    procedure PrepareTransmitPart; virtual;
      {-Prepare for calling ProtocolTransmitPart}
    function ProtocolTransmitPart : ProtocolStateType ; virtual;
      {-Performs one "increment" of a protocol transmit and returns}
    procedure PrepareReceivePart; virtual;
      {-Prepare for calling ProtocolReceivePart}
    function ProtocolReceivePart : ProtocolStateType ; virtual;
      {-Performs one "increment" of a protocol receive and returns}

    {$IFDEF UseStreams}
    constructor Load00(var S : IdStream);
      {-Load an old style XmodemProtocol object from a stream}
    constructor Load(var S : IdStream);
      {-Load an XmodemProtocol object from a stream}
    procedure Store(var S : IdStream);
      {-Store an XmodemProtocol object to a stream}
    {$ENDIF}

    {#Z+}
    {++++ Internal methods ++++}
    procedure apUpdateBlockCheck(CurByte : Byte); virtual;
    procedure apSendBlockCheck; virtual;
    function apVerifyBlockCheck : Boolean; virtual;
    procedure apCancel; virtual;
    function apGetFirstBlockNum : Byte; virtual;
    function apGetHandshakeChar : Char; virtual;
    function apProcessHandshake : Boolean; virtual;
    function apPrepHandshake : Boolean;
    function apProcessBlockReply : Boolean; virtual;
    procedure apTransmitBlock(var Block : DataBlockType;
                              BLen : Word; BType : Char); virtual;
    procedure apTransmitEot(First : Boolean); virtual;
    function apProcessEotReply : Boolean;
    procedure apSendHandshakeChar(Handshake : Char); virtual;
    function apCheckForBlockStart(var C : Char) : Boolean;
    procedure apReceiveBlock(var Block : DataBlockType;
                             var BlockSize : Word;
                             var HandShake : Char); virtual;
    {#Z-}
  end;

  {$IFDEF UseStreams}
  procedure XmodemProtocolStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing xmodem objects}
  {$ENDIF}

implementation

const
  GReq = 'G';                    {G mode request}

type
  Bytes = array[1..1024] of Byte;      {Maximum buffer supported}

  constructor XmodemProtocol.Init(APPtr : AbstractPortPtr;
                                  Use1K, UseGMode : Boolean);
    {-Allocates and initializes a protocol control block}
  begin
    if not XmodemProtocol.InitCustom(APPtr, Use1K, UseGMode,
                                     DefProtocolOptions) then
      Fail;
  end;

  constructor XmodemProtocol.InitCustom(APPtr : AbstractPortPtr;
                                        Use1K, UseGMode : Boolean;
                                        Options : Word);
    {-Allocates and initializes a protocol control block with options}
  begin
    {Init ancestor}
    AbstractProtocol.InitCustom(APPtr, Options);

    {Set block mode}
    OneKMode := Use1K;

    {Miscellaneous inits}
    BlockWait := DefBlockWait;
    MaxBlockErrors := DefMaxBlockErrors;
    Overhead := XmodemOverhead;
    TurnDelay := XmodemTurnDelay;
    FinishWait := DefFinishWait;

    {Set streaming mode}
    GMode := UseGMode;
    if GMode then begin
      EotCheckCount := 0;
      OneKMode := True;
      TurnDelay := 0;
    end;

    {Assume crc mode}
    CheckType := bcCrc16;

    {Set the initial block size (and protocol type)}
    ProtType := XmodemCRC;
    Set1KMode(OneKMode);

    {!!.01 moved from state machine}
    {Get a protocol DataBlock}
    if not GetMemCheck(DataBlock, SizeOf(DataBlockType)+5) then begin
      APort^.GotError(epFatal+ecOutOfMemory);
      Fail;
    end;
  end;

  {!!.01 new}
  destructor XmodemProtocol.Done;
    {-Deallocates heap space}
  begin
    AbstractProtocol.Done;
    FreeMemCheck(DataBlock, SizeOf(DataBlockType)+5);
  end;

  procedure XmodemProtocol.Set1KMode(Enable : Boolean);
    {-Enable/disable Xmodem1K (aka Ymodem)}
  const
    GType : array[Boolean] of Byte = (YmodemG, Xmodem1KG);
    KType : array[Boolean] of Byte = (Ymodem, Xmodem1K);
    XType : array[Boolean] of Byte = (Ymodem, Xmodem);
  var
    WasXmodem : Boolean;
  begin
    {Note the original protocol type}
    case ProtType of
      Xmodem,Xmodem1K,Xmodem1KG,XmodemCRC : WasXmodem := True;
      else WasXmodem := False;
    end;

    {Turn 1K mode on or off}
    OneKMode := Enable;
    if OneKMode then begin
      BlockLen := 1024;
      StartChar := cStx;
      if GMode then
        ProtType := GType[WasXmodem]
      else
        ProtType := KType[WasXmodem];
    end else begin
      BlockLen := 128;
      StartChar := cSoh;
      ProtType := XType[WasXmodem];
    end;
  end;

  procedure XmodemProtocol.SetGMode(Enable : Boolean);
    {-Enable/disable streaming}
  begin
    GMode := Enable;
    if GMode then begin
      Set1KMode(True);
      TurnDelay := 0;
      EotCheckCount := 0;
    end else begin
      EotCheckCount := 1;
      MaxBlockErrors := DefMaxBlockErrors;
      TurnDelay := XmodemTurnDelay;
    end;
  end;

  procedure XmodemProtocol.SetBlockWait(NewBlockWait : Byte);
    {-Set inter-block wait time}
  begin
    BlockWait := NewBlockWait;
  end;

  procedure XmodemProtocol.SetFinishWait(NewFinishWait : Word);
    {-Set additional finish wait (time to wait for EOT response)}
  begin
    FinishWait := NewFinishWait;
  end;

  function XmodemProtocol.apPrepHandshake : Boolean;
    {-Set up to wait for a handshake char}
  begin
    NewTimer(ReplyTimer, HandshakeWait);
    Inc(HandshakeAttempt);
    if HandshakeAttempt > HandshakeRetry then begin
      apPrepHandshake := False;
      APort^.GotError(ecTimeout);
    end else
      apPrepHandshake := True;
  end;

  procedure XmodemProtocol.apUpdateBlockCheck(CurByte: Byte);
    {-Updates the block check character (whatever it is)}
  begin
    if CheckType = bcCrc16 then
      BlockCheck := UpdateCrc(CurByte, BlockCheck)
    else
      BlockCheck := UpdateCheckSum(CurByte, Lo(BlockCheck));
  end;

  procedure XmodemProtocol.apSendBlockCheck;
    {-Makes final adjustment and sends the BlockCheck character}
  begin
    {Send the check character}
    if CheckType = bcCrc16 then begin
      apUpdateBlockCheck(0);
      apUpdateBlockCheck(0);
      APort^.PutChar(Char(Hi(BlockCheck)));
      APort^.PutChar(Char(Lo(BlockCheck)));
    end else
      APort^.PutChar(Char(Lo(BlockCheck)));
  end;

  function XmodemProtocol.apVerifyBlockCheck : Boolean;
    {-Receives and checks the block check value}
  var
    CheckLo : Byte;
    CheckHi : Byte;
  begin
    {Assume a block check error}
    apVerifyBlockCheck := False;

    if CheckType = bcCrc16 then begin
      {Get the hi byte of the Crc}
      apUpdateBlockCheck(0);
      apUpdateBlockCheck(0);
      APort^.GetChar(Char(CheckHi));
      if AsyncStatus = ecUserAbort then begin
        apAbortProtocol;
        Exit;
      end;
    end;

    {Get the (rest of the) block check value}
    APort^.GetChar(Char(CheckLo));
    if AsyncStatus = ecUserAbort then begin
      apAbortProtocol;
      Exit;
    end;

    {Verify the block check value}
    if CheckType = bcCrc16 then
      if (CheckHi <> Hi(BlockCheck)) or (CheckLo <> Lo(BlockCheck)) then
        Exit
      else
    else
      if CheckLo <> Lo(BlockCheck) then
        Exit;

    {If we get here, the block check value is ok}
    apVerifyBlockCheck := True;
  end;

  procedure XmodemProtocol.apCancel;
    {-Sends cancel request to remote}
  const
    CanStr = cCan+cCan+cCan+cBS+cBS+cBS;
  var
    ET : EventTimer;
    Tics : Word;
    C : Char;
    SaveStatus : Word;
  begin
    with APort^ do begin
      SaveStatus := AsyncStatus;

      {Flush anything that might be left in the output buffer}
      FlushOutBuffer;

      {Cancel with three CANCEL chars}
      PutString(CanStr);

      {Wait until all three CANs have been transmitted}
      NewTimer(ET, CancelTimeout);
      while (AsyncStatus = ecOk) and
            (OutBuffUsed <> 0) and
            (not ptWaitComplete(ET)) do ;

      {Discard characters for about one block}
      Tics := (BlockLen div ActCPS) * 18;
      if Tics = 0 then
        Tics := 9;
      NewTimer(ET, Tics);
      repeat
        if APort^.CharReady then
          APort^.GetChar(C);
      until TimerExpired(ET);

      {Set AsyncStatus appropriately}
      if SaveStatus = ecOk then
        AsyncStatus := ecCancelRequested
      else
        AsyncStatus := SaveStatus;
    end;
  end;

  function XmodemProtocol.apGetFirstBlockNum : Byte;
    {-Returns first block number}
  begin
    apGetFirstBlockNum := 1;
  end;

  function XmodemProtocol.apGetHandshakeChar : Char;
    {-Returns proper handshake character}
  begin
    if GMode then
      apGetHandshakeChar := GReq
    else if CheckType = bcCrc16 then
      apGetHandshakeChar := CrcReq
    else
      apGetHandshakeChar := cNak;
  end;

  function XmodemProtocol.apProcessHandshake : Boolean;
    {-Process initial handshake, return true if OK}
  var
    C : Char;
  begin
    {If we get here we know a character is waiting}
    APort^.GetChar(C);
    if AsyncStatus <> ecOk then begin
      {Line error or timeout}
      Inc(BlockErrors);
      Inc(TotalErrors);
    end else
      case C of
        cCan : {Remote requested a cancel}
          APort^.GotError(epFatal+ecInitCancel);
        cNak : {Set checksum mode}
          CheckType := bcChecksum1;
        CrcReq : {Set CRC mode (and possibly reset the protocol type)}
          begin
            CheckType := bcCrc16;
            if ProtType = Xmodem then
              ProtType := XmodemCRC;
          end;
        GReq : {Set G mode (streaming mode)}
          begin
            GMode := True;
            CheckType := bcCrc16;
          end;
        else {Unexpected character}
          Inc(BlockErrors);
          Inc(TotalErrors);
          AsyncStatus := ecUnexpectedChar;
      end;
    apProcessHandshake := AsyncStatus = ecOk;
  end;

  function XmodemProtocol.apProcessBlockReply : Boolean;
    {-Process reply to last block; return True if TransmitBlock should exit}
  var
    C : Char;
  begin
    {Handle GMode (all blocks are assumed to succeed)}
    if GMode then begin
      AsyncStatus := ecOk;
      Inc(BytesTransferred, BlockLen);
      Dec(BytesRemaining, BlockLen);
      if BytesRemaining < 0 then
        BytesRemaining := 0;
      ElapsedTics := ElapsedTime(Timer);
      Inc(BlockNum);
      Inc(FileOfs, BlockLen);

      {Check for cancel from remote}
      if APort^.CharReady then begin
        APort^.GetChar(C);
        if C = cCan then
          APort^.GotError(epFatal+ecCancelRequested);
      end;
      apProcessBlockReply := AsyncStatus = ecOk;
      Exit;
    end;

    {We know a character is ready}
    APort^.GetChar(C);
    if C <> cCan then
      CanCounter := 0;

    {Process the reply}
    if AsyncStatus = ecOk then
      case C of
        cAck : {Adjust buffer pos (in case we switched block sizes)}
          begin
            Dec(CharsLeft, BlockLen);
            Inc(OutBufPos, BlockLen);
            Inc(BytesTransferred, BlockLen);
            Dec(BytesRemaining, BlockLen);
            if BytesRemaining < 0 then
              BytesRemaining := 0;
            ElapsedTics := ElapsedTime(Timer);
            Inc(BlockNum);
            Inc(FileOfs, BlockLen);
          end;
        cCan : {Cancel requested by remote}
          begin
            Inc(CanCounter);
            if CanCounter >= 2 then
              APort^.GotError(epFatal+ecCancelRequested);
          end;
        else {Unexpected character or Nak}
          Inc(BlockErrors);
          Inc(TotalErrors);
          APort^.GotError(epNonFatal+ecUnexpectedChar);
      end
    else begin
      {Reply timeout}
      Inc(BlockErrors);
      Inc(TotalErrors);
    end;
    apProcessBlockReply := AsyncStatus = ecOk;
  end;

  procedure XmodemProtocol.apTransmitBlock(var Block : DataBlockType;
                                           BLen : Word; BType : Char);
    {-Transmits one data block}
  var
    I : Integer;
    INum : Byte;
    BytesWritten : Word;
  begin
    if BlockErrors > MaxBlockErrors then
      {Too many errors}
      if OneKMode and (BlockLen = 1024) then begin
        {1KMode - reduce the block size and try again}
        BlockLen := 128;
        StartChar := cSoh;
        BlockErrors := 0;
      end else begin
        {Std Xmodem - have to cancel}
        apCancel;
        APort^.GotError(epFatal+ecTooManyErrors);
        Exit;
      end;

    {Send the StartBlock char, the block sequence and its compliment}
    APort^.PutChar(StartChar);
    INum := Lo(BlockNum);
    APort^.PutChar(Char(INum));
    APort^.PutChar(Char(not INum));

    {Init the BlockCheck value}
    BlockCheck := 0;

    {Send the data on its way}
    APort^.PutBlockDirect(Bytes(Block)[OutBufPos], BlockLen, BytesWritten);

    {Calculate the check character}
    if CheckType = bcCrc16 then
      for  I := 1 to BlockLen do
        BlockCheck := UpdateCrc(Bytes(Block)[OutBufPos + (I-1)], BlockCheck)
    else
      for  I := 1 to BlockLen do
        BlockCheck := UpdateCheckSum(Bytes(Block)[OutBufPos + (I-1)], BlockCheck);

    {Send the check character}
    if CheckType = bcCrc16 then begin
      BlockCheck := UpdateCrc(0, BlockCheck);
      BlockCheck := UpdateCrc(0, BlockCheck);
      APort^.PutChar(Char(Hi(BlockCheck)));
      APort^.PutChar(Char(Lo(BlockCheck)));
    end else
      APort^.PutChar(Char(BlockCheck));
  end;

  procedure XmodemProtocol.apTransmitEot(First : Boolean);
    {-Transmit an Xmodem EOT (end of transfer)}
  begin
    AsyncStatus := ecOk;
    if First then begin
      BlockErrors := 0;
      NaksReceived := 0;
      Dec(BlockNum);
    end;

    {Send the Eot char}
    APort^.PutChar(cEot);
  end;

  function XmodemProtocol.apProcessEotReply : Boolean;
    {-Get a response to an EOT}
  var
    C : Char;
  begin
    {Get the response}
    APort^.GetChar(C);
    if AsyncStatus = ecOk then
      case C of
        cAck : {Receiver acknowledged Eot, this transfer is over}
          begin
            apProcessEotReply := True;
            AsyncStatus := ecEndFile;
          end;
        cCan : {Receiver asked to cancel, this transfer is over}
          begin
            apProcessEotReply := True;
            AsyncStatus := ecCancelRequested;
          end;
        cNak : {Some Xmodems always NAK the first 1 or 2 EOTs}
               {So, don't count them as errors till we get 3 }
          begin
            apProcessEotReply := False;
            Inc(NaksReceived);
            If NaksReceived >= 3 then begin
              Inc(BlockErrors);
              Inc(TotalErrors);
            end;
          end;
        else {Unexpected character received}
          apProcessEotReply := False;
          Inc(BlockErrors);
          Inc(TotalErrors);
      end
    else begin
      {Line error}
      apProcessEotReply := False;
      Inc(BlockErrors);
      Inc(TotalErrors)
    end;
  end;

  procedure XmodemProtocol.apSendHandshakeChar(Handshake : Char);
    {-Send the current handshake char}
  begin
    {If in Gmode, filter out all standard Acks}
    if not Gmode or (Handshake <> cAck) then
      APort^.PutChar(Handshake);
  end;

  function XmodemProtocol.apCheckForBlockStart(var C : Char) : Boolean;
    {-Scan input buffer for start char, return True if found}
  begin
    AsyncStatus := ecOk;
    apCheckForBlockStart := False;

    {Ready to scan...}
    BlockErrors := 0;
    while APort^.CharReady do begin

      {Check the next character}
      APort^.GetChar(C);
      if AsyncStatus = ecOk then begin
        case C of
          cSoh,
          cStx,
          cEot,
          cCan : begin
                   apCheckForBlockStart := True;
                   Exit;
                 end;
          else begin
            APort^.GotError(epNonFatal+ecUnexpectedChar);
            EotCounter := 0;
            CanCounter := 0;
          end;
        end;
      end;
    end;
  end;

  procedure XmodemProtocol.apReceiveBlock(var Block : DataBlockType;
                                          var BlockSize : Word;
                                          var HandShake : Char);
    {-Process the data already in Block}
  var
    R1, R2 : Byte;
    GotLen : Word;
    I : Word;
    CrcHi : Byte;
    CrcLo : Byte;
  begin
    AsyncStatus := ecOk;

    {Assume an error}
    Handshake := cNak;

    {Get and compare block sequence numbers}
    R1 := Byte(Block[1]);
    R2 := Byte(Block[2]);
    if (not R1) <> R2 then begin
      Inc(BlockErrors);
      Inc(TotalErrors);
      APort^.FlushInBuffer;
      apCancel;
      APort^.GotError(epFatal+ecSequenceError);
      Exit;
    end;

    {Init checkchar accumulators}
    BlockCheck := 0;
    for I := 3 to BlockLen+2 do
      if CheckType = bcCrc16 then
        BlockCheck := UpdateCrc(Byte(Block[I]), BlockCheck)
      else
        BlockCheck := UpdateCheckSum(Byte(Block[I]), BlockCheck);

    {Check the block-check character}
    if CheckType = bcCrc16 then begin
      BlockCheck := UpdateCrc($00, BlockCheck);
      BlockCheck := UpdateCrc($00, BlockCheck);
      CrcHi := Byte(Block[BlockLen+3]);
      CrcLo := Byte(Block[BlockLen+4]);
    end else
      CrcLo := Byte(Block[BlockLen+3]);
    if CheckType = bcCrc16 then
      if (CrcHi <> Hi(BlockCheck)) or (CrcLo <> Lo(BlockCheck)) then begin
        {Block check error}
        Inc(BlockErrors);
        Inc(TotalErrors);
        APort^.FlushInBuffer;
        APort^.GotError(epNonFatal+ecBlockCheckError);
        Exit;
      end else
    else
      if CrcLo <> Lo(BlockCheck) then begin
        {Block check error}
        Inc(BlockErrors);
        Inc(TotalErrors);
        APort^.FlushInBuffer;
        APort^.GotError(epNonFatal+ecBlockCheckError);
        Exit;
      end;

    {Check the block sequence for missing or duplicate blocks}
    if R1 = Lo(BlockNum-1) then begin
      {This is a duplicate block}
      HandShake := cAck;
      BlockErrors := 0;
      APort^.GotError(epNonFatal+ecDuplicateBlock);
      Exit;
    end;
    if R1 <> Lo(BlockNum) then begin
      {Its a sequence error}
      apCancel;
      APort^.GotError(epFatal+ecSequenceError);
      Exit;
    end;

    {Block is ok, do ACK on next call to ReceiveBlock}
    Handshake := cAck;
    AsyncStatus := ecOk;
    Move(Block[3], Block[1], BlockLen);

    {Update status fields for the next call to the user status routine}
    Inc(BlockNum);
    Inc(BytesTransferred, BlockLen);
    Dec(BytesRemaining, BlockLen);
    if BytesRemaining < 0 then
      BytesRemaining := 0;
    ElapsedTics := ElapsedTime(Timer);
    BlockErrors := 0;
    BlockSize := BlockLen;
  end;

  procedure XmodemProtocol.PrepareTransmitPart;
    {-Prepare for calling ProtocolTransmitPart}
  begin
    AbstractProtocol.PrepareTransmitPart;
    apResetStatus;
    CheckType := bcChecksum1;

    apShowFirstStatus;
    if not NextFile(@Self, Pathname) then begin
      {AsyncStatus already set}
      apShowLastStatus;
      APort^.PR^.ProtocolActive := False;
      Exit;
    end;

    ForceStatus := True;
    XmodemState := txInitial;
    {DataBlock := nil;}                                                {!!.01}
  end;

  function XmodemProtocol.ProtocolTransmitPart : ProtocolStateType;
    {-Performs on increment of an Xmodem transmit and returns}
  label
    ExitPoint;
  begin
    {Check for user abort}
    if apHandleAbort then
      XmodemState := txFinished
    else if TimerExpired(StatusTimer) or ForceStatus then begin
      {Show status periodically}
      NewTimer(StatusTimer, StatusInterval);
      ForceStatus := False;
      apUserStatus(False, False);
    end;

    {Process current state}
    case XmodemState of
      txInitial :
        begin
          {Reset status vars}
          apResetStatus;
          NoMoreData := False;

          {!!.01 moved to constructor}
          {Get a protocol DataBlock}
          {if not GetMemCheck(DataBlock, SizeOf(DataBlockType)) then begin
            APort^.GotError(epFatal+ecOutOfMemory);
            apShowLastStatus;
            ProtocolTransmitPart := psFinished;
            APort^.PR^.ProtocolActive := False;
            Exit;
          end;}

          Pathname := StUpcase(Pathname);

          {Show file name to user logging routine}
          LogFile(@Self, lfTransmitStart);

          {Go prepare for reading protocol blocks}
          apPrepareReading;
          if AsyncStatus <> ecOk then begin
            XmodemState := txFinished;
            goto ExitPoint;
          end;

          {Set the first block number}
          BlockNum := apGetFirstBlockNum;
          MaxBlockErrors := DefMaxBlockErrors;

          {Clear possible garbage (but allow case for already receieved handshake)}
          if APort^.InBuffUsed > 1 then
            APort^.FlushInBuffer;

          {Check for handshake character}
          XmodemState := txHandshake;
          HandshakeAttempt := 0;
          if not apPrepHandshake then
            XmodemState := txFinished;
        end;

      txHandshake :
        begin
          if APort^.CharReady then
            if apProcessHandshake then begin
              {Start protocol timer now}
              NewTimer(Timer, 1);
              XmodemState := txGetBlock;
              FileOfs := 0;
              if GMode then
                MaxBlockErrors := 0;
            end else begin
              if AsyncStatus = ecInitCancel then
                XmodemState := txFinished
              else if not apPrepHandshake then
                XmodemState := txFinished
            end
          else
            if TimerExpired(ReplyTimer) then
              if not apPrepHandshake then
                XmodemState := txFinished
        end;

      txGetBlock :
        begin
          LastBlockSize := BlockLen;
          CharsLeft := BlockLen;
          OutBufPos := 1;
          BlockErrors := 0;
          NoMoreData := apReadProtocolBlock(DataBlock^, LastBlockSize);
          if AsyncStatus = ecOk then begin
            XmodemState := txWaitFreespace;
            NewTimer(ReplyTimer, TransTimeout);
          end else
            XmodemState := txFinished;
        end;

      txWaitFreeSpace :
        begin
          if APort^.OutBuffFree > BlockLen+5 then
            XmodemState := txSendBlock
          else if TimerExpired(ReplyTimer) then begin
            {Must be buffer full error}
            APort^.GotError(epFatal+ecBufferIsFull);
            XmodemState := txFinished;
          end;
        end;

      txSendBlock :
        begin
          {Don't send empty blocks (will only happen with empty files)}
          if LastBlockSize <= 0 then
            XmodemState := txFirstEndOfTransmit
          else begin
            {If no errors, then send this block to the remote}
            if AsyncStatus = ecOk then begin
              apTransmitBlock(DataBlock^, BlockLen, ' ');

              {If TransmitBlock failed, go clean up}
              if AsyncStatus <> ecOk then begin
                APort^.FlushOutBuffer;
                XmodemState := txFinished;
                goto ExitPoint;
              end;

              {Prepare to handle reply}
              if GMode then
                {Go process psuedo reply}
                XmodemState := txReplyPending
              else begin
                {Wait for output buffer to drain}
                XmodemState := txDraining;
                NewTimer(ReplyTimer, DrainWait);
              end;

              {Force a status update}
              ForceStatus := True;
            end else begin
              {Most likely a disk read error, have to give up}
              apCancel;
              XmodemState := txFinished;
            end;
          end;
        end;

      txDraining :
        begin
          if (APort^.OutBuffUsed <= 1) or TimerExpired(ReplyTimer) then begin
            NewTimer(ReplyTimer, BlockWait);
            XmodemState := txReplyPending;
          end;
        end;

      txReplyPending :
        begin
          if APort^.CharReady or GMode then begin
            if apProcessBlockReply then
              {Got reply, go send next block}
              if NoMoreData then begin
                XmodemState := txEndDrain;
                NewTimer(ReplyTimer,
                  Secs2Tics((APort^.PR^.OutBuffLen div ActCPS)) * 2);
              end else
                XmodemState := txGetBlock
            else
              if AsyncStatus = ecCancelRequested then begin
                {Got two cancels, we're finished}
                APort^.FlushOutBuffer;
                XmodemState := txFinished;
              end else
                if CanCounter > 0 then
                  {Just got a cancel, look for another}
                  XmodemState := txReplyPending
                else begin
                  {Got junk for a response, go send block again}
                  XmodemState := txWaitFreespace;
                  AsyncStatus := ecOk;
                end;
          end else
            if TimerExpired(ReplyTimer) then begin
              XmodemState := txSendBlock;
              Inc(BlockErrors);
              Inc(TotalErrors);
            end;
        end;

      txEndDrain:
        begin
          if (APort^.OutBuffUsed <= 1) or TimerExpired(ReplyTimer) then
            XmodemState := txFirstEndOfTransmit;
        end;

      txFirstEndOfTransmit :
        begin
          apTransmitEot(True);
          if GMode then
            NewTimer(ReplyTimer, BlockWait+FinishWait)
          else
            NewTimer(ReplyTimer, BlockWait);
          XmodemState := txEotReply;
        end;

      txRestEndOfTransmit :
        begin
          apTransmitEot(False);
          NewTimer(ReplyTimer, BlockWait);
          if BlockErrors <= MaxBlockErrors then
            XmodemState := txEotReply
          else begin
            APort^.GotError(epFatal+ecTooManyErrors);
            XmodemState := txFinished;
          end;
        end;

      txEotReply :
        begin
          if APort^.CharReady then
            if apProcessEotReply then
              XmodemState := txFinished
            else
              XmodemState := txRestEndOfTransmit
          else
            if TimerExpired(ReplyTimer) then
              XmodemState := txRestEndOfTransmit;
        end;

      txFinished :
        begin
          if AsyncStatus = ecCancelRequested then
            APort^.FlushInBuffer;

          {Close the file (or whatever was giving us blocks)}
          apFinishReading;

          {Show status, user logging, and clean up}
          if AsyncStatus = ecEndFile then begin
            AsyncStatus := ecOk;
            LogFile(@Self, lfTransmitOk)
          end else
            LogFile(@Self, lfTransmitFail);
          apShowLastStatus;

          {FreeMemCheck(DataBlock, SizeOf(DataBlockType));}            {!!.01}
          XmodemState := txDone;
          APort^.PR^.ProtocolActive := False;
        end;
    end;

ExitPoint:
    {Set function result}
    case XmodemState of
      txInitial,
      txGetBlock,
      txSendBlock,
      txFirstEndOfTransmit,
      txRestEndOfTransmit,
      txFinished             : ProtocolTransmitPart := psReady;

      txEndDrain,
      txHandshake,
      txWaitFreeSpace,
      txDraining,
      txReplyPending,
      txEotReply             : ProtocolTransmitPart := psWaiting;

      txDone                 : begin
			      				ProtocolTransmitPart := psFinished;
      							LogFile(@Self, lfTransmitOk);
      			       		   end; { txDone }
    end;
  end;

  procedure XmodemProtocol.PrepareReceivePart;
    {-Starts Xmodem receive protocol}
  begin
    {Do parent inits}
    AbstractProtocol.PrepareReceivePart;

    {Prepare to enter state machine}
    XmodemState := rxInitial;
    {DataBlock := nil;}                                                {!!.01}
    apResetStatus;
  end;

  function XmodemProtocol.ProtocolReceivePart : ProtocolStateType;
    {-Perform one increment on an Xmodem receive}
  label
    ExitPoint;
  var
    C : Char;
    BlockSize : Word;
    Error : Boolean;
    RcvStatus : Word;
    {Cnt : Word;}                                                      {!!.03}

    procedure Cleanup(DisposeBuffers : Boolean);
      {-Handle error reporting and other cleanup}
    begin
      {if DisposeBuffers then
        FreeMemCheck(DataBlock, SizeOf(DataBlockType)+5);}             {!!.01}
      apShowLastStatus;
      XmodemState := rxDone;
      APort^.PR^.ProtocolActive := False;
    end;

    function CheckErrors : Boolean;
      {-Increment block errors, return True if too many}
    begin
      Inc(BlockErrors);
      Inc(TotalErrors);
      if BlockErrors > MaxBlockErrors then begin
        CheckErrors := True;
        APort^.GotError(ecTimeout);
      end else
        CheckErrors := False;
    end;

  begin
    {Check for user abort}
    if apHandleAbort then
      XmodemState := rxFinished;

    {Process current state}
    case XmodemState of
      rxInitial :
        begin
          {Show (possible) first status}
          AsyncStatus := ecHandshakeInProgress;
          apShowFirstStatus;

          {!!.01 moved to constructor}
          {Get a protocol DataBlock}
          {if not GetMemCheck(DataBlock, SizeOf(DataBlockType)+5) then begin
            APort^.GotError(epFatal+ecOutOfMemory);
            Cleanup(False);
            goto ExitPoint;
          end;}

          {Pathname should already have name of file to receive}
          if Pathname = '' then begin
            APort^.GotError(epFatal+ecNoFilename);
            Cleanup(True);
            goto ExitPoint;
          end else begin
            {Merge in destdir if not already done}
            if (JustPathname(Pathname) = '') and (DestDir <> '') then
              Pathname := AddBackslash(DestDir)+Pathname;
            PathName := StUpcase(PathName);
          end;

          {Send file name to user's LogFile procedure}
          LogFile(@Self, lfReceiveStart);

          {Accept this file}
          if not AcceptFile(@Self) then begin
            apCancel;
            APort^.GotError(epNonFatal+ecFileRejected);
            XmodemState := rxFinishedSkip;
            goto ExitPoint;
          end;

          {Prepare file for writing protocol blocks}
          apPrepareWriting;
          if AsyncStatus <> ecOk then begin
            apCancel;
            XmodemState := rxFinished;
            goto ExitPoint;
          end;

          {Start sending handshakes}
          FileOfs := 0;
          XmodemState := rxWaitForHSReply;
          HandshakeChar := apGetHandshakeChar;
          apSendHandshakeChar(HandshakeChar);
          BlockNum := 1;
          EotCounter := 0;
          CanCounter := 0;
          MaxBlockErrors := DefMaxBlockErrors;

          NewTimer(ReplyTimer, HandshakeWait);
          TimerPending := True;

          {Set overhead length based on check type}
          if CheckType = bcCrc16 then
            OverheadLen := 4
          else
            OverheadLen := 3;
        end;

      rxWaitForHSReply :
        begin
          if APort^.CharReady then
            XmodemState := rxWaitForBlockStart
          else if TimerExpired(ReplyTimer) then
            if CheckErrors then
              XmodemState := rxFinished
            else begin
              if (HandshakeChar = CrcReq) and (BlockErrors > MaxCrcTry) then begin
                {Step down to Xmodem checksum}
                HandshakeChar := cNak;
                BlockErrors := 0;
                Dec(OverheadLen);
                CheckType := bcChecksum1;
              end;
              APort^.PutChar(HandshakeChar);
              NewTimer(ReplyTimer, HandshakeWait);
            end;
        end;

      rxWaitForBlockStart :
        begin
          if APort^.CharReady then begin
            if TimerPending then begin
              NewTimer(Timer, 0);
              TimerPending := False;
            end;
            if apCheckForBlockStart(C) then begin
              case C of
                cSoh,
                cStx :
                  begin
                    if C = cSoh then
                      BlockLen := 128
                    else
                      BlockLen := 1024;
                    XmodemState := rxCollectBlock;
                    NewTimer(ReplyTimer, BlockWait);
                    BlkIndex := 0;
                    if GMode then
                      MaxBlockErrors := 0;
                   end;
                cCan :
                  begin
                    EotCounter := 0;
                    Inc(CanCounter);
                    if CanCounter > 2 then begin
                      apCancel;
                      XmodemState := rxFinished;
                    end;
                  end;
                cEot :
                  begin
                    CanCounter := 0;
                    Inc(EotCounter);
                    if EotCounter > EotCheckCount then begin
                      APort^.PutChar(cAck);
                      XmodemState := rxFinished;
                      AsyncStatus := ecEndFile;
                    end else begin
                      APort^.PutChar(cNak);
                      XmodemState := rxWaitForBlockStart;
                      NewTimer(ReplyTimer, BlockWait);
                    end;
                  end;
              end;
            end else begin
              {Line error or junk data, discard all and send nak}
              if not CheckErrors then begin                            {!!.02}
                APort^.FlushInBuffer;
                APort^.PutChar(cNak);
              end else                                                 {!!.02}
                XmodemState := rxFinished;                             {!!.02}
            end;
          end else begin
            {No chars yet, check timeout}
            if TimerExpired(ReplyTimer) then begin
              {Finished if too many timeouts or timeout during eot or can}
              if EotCounter <> 0 then begin
                {Timeout out waiting for second cEot, end normally}
                APort^.PutChar(cAck);
                XmodemState := rxFinished;
                AsyncStatus := ecEndFile;
              end else if CheckErrors or (CanCounter <> 0) then
                XmodemState := rxFinished
              else begin
                {Simple timeout, resend handshake}
                XmodemState := rxWaitForHSReply;
                apSendHandshakeChar(HandshakeChar);
                NewTimer(ReplyTimer, BlockWait);
              end;
            end;
          end;
        end;

      rxCollectBlock :
        begin
          {Cnt := 1;}                                                  {!!.03}
          while APort^.CharReady and
                {(Cnt < 10) and}                                       {!!.03}
                (BlkIndex < BlockLen + OverheadLen) do begin
            APort^.GetChar(C);
            Inc(BlkIndex);
            {Inc(Cnt);}                                                {!!.03}
            DataBlock^[BlkIndex] := C;
            NewTimer(ReplyTimer, BlockWait);                           {!!.02}
          end;

          if BlkIndex >= BlockLen + OverheadLen then
            {Got a complete block, go process it}
            XmodemState := rxProcessBlock
          else if TimerExpired(ReplyTimer) then begin
            {Timeout out waiting for complete block, resend handshake}
            APort^.GotError(epNonFatal+ecTimeout);
            APort^.PutChar(cNak);
            XmodemState := rxWaitForBlockStart;
            NewTimer(ReplyTimer, BlockWait);
          end;
        end;

      rxProcessBlock :
        begin
          {Go process what's in DataBlock}
          apReceiveBlock(DataBlock^, BlockSize, HandshakeChar);
          RcvStatus := AsyncStatus;
          apSendHandshakeChar(HandshakeChar);

          if RcvStatus = ecOk then begin
            {Got block ok, go write it out}
            Error := apWriteProtocolBlock(DataBlock^, BlockSize);
            if AsyncStatus = ecOk then
              Inc(FileOfs, BlockSize)
            else begin
              {Failed to write the block, clean up and exit}
              apCancel;
              XmodemState := rxFinished;
              goto ExitPoint;
            end;

            {Normal received block -- keep going}
            XmodemState := rxWaitForBlockStart;
            NewTimer(ReplyTimer, BlockWait);
            ForceStatus := True;
          end else begin
            AsyncStatus := RcvStatus;
            if (AsyncStatus = ecSequenceError) or GMode then begin
              {Fatal error - just exit}
              if GMode then
                apCancel;
              APort^.FlushOutBuffer;
              XmodemState := rxFinished;
              goto ExitPoint;
            end else begin
              {Failed to get block, go try again (already sent Nak)}
              XmodemState := rxWaitForHSReply;
              ForceStatus := True;
              NewTimer(ReplyTimer, HandshakeWait);
              goto ExitPoint;
            end;
          end;
        end;

      rxFinishedSkip :
        begin
          apFinishWriting;
          LogFile(@Self, lfReceiveSkip);
          Cleanup(True);
          goto ExitPoint;
        end;

      rxFinished :
        begin
          apFinishWriting;
          if AsyncStatus = ecEndFile then begin
            AsyncStatus := ecOk;
            LogFile(@Self, lfReceiveOk);
          end else
            LogFile(@Self, lfReceiveFail);
          Cleanup(True);
          goto ExitPoint;
        end;
    end;

    {Show status periodically}
    if TimerExpired(StatusTimer) or ForceStatus then begin
      ForceStatus := False;
      NewTimer(StatusTimer, StatusInterval);
      apUserStatus(False, False);
    end;

ExitPoint:
    {Set function result}
    case XmodemState of
      rxInitial,
      rxFinishedSkip,
      rxProcessBlock,
      rxFinished           : ProtocolReceivePart := psReady;

      rxWaitForHSReply,
      rxWaitForBlockStart,
      rxCollectBlock       : ProtocolReceivePart := psWaiting;

      rxDone               : ProtocolReceivePart := psFinished;
    end;
  end;

  {$IFDEF UseStreams}
  constructor XmodemProtocol.Load00(var S : IdStream);
    {-Load an XmodemProtocol object from a stream}
  begin
    {Load the parent}
    if not AbstractProtocol.Load(S) then begin
      Done;
      Fail;
    end;

    {Load the Xmodem specific fields}
    S.ReadRange(StartChar, MaxBlockErrors);
    S.Read(MaxBlockErrors, SizeOf(MaxBlockErrors));
    if S.PeekStatus <> 0 then begin
      Done;
      Fail;
    end;
  end;

  constructor XmodemProtocol.Load(var S : IdStream);
    {-Load an XmodemProtocol object from a stream}
  begin
    {Use Load00 to load most things}
    if not XmodemProtocol.Load00(S) then begin
      Done;
      Fail;
    end;

    {Load the new fields}
    S.Read(GMode, SizeOf(GMode));
    if S.PeekStatus <> 0 then begin
      Done;
      Fail;
    end;
  end;

  procedure XmodemProtocol.Store(var S : IdStream);
    {-Store an XmodemProtocol object to a stream}
  begin
    {Store the parent}
    AbstractProtocol.Store(S);

    {Store the Xmodem specific fields}
    S.WriteRange(StartChar, MaxBlockErrors);
    S.Write(MaxBlockErrors, SizeOf(MaxBlockErrors));

    {Store the GMode field}
    S.Write(GMode, SizeOf(GMode));
  end;

  procedure XmodemProtocolStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing xmodem objects}
  begin
    AbstractProtocolStream(SPtr);
    SPtr^.RegisterType(otXmodemProtocol, veXmodemProtocol,
                       TypeOf(XmodemProtocol),
                       @XmodemProtocol.Store, @XmodemProtocol.Load);
    SPtr^.RegisterOldVersion(otXmodemProtocol, 00,
                       TypeOf(XmodemProtocol),
                       @XmodemProtocol.Load00);
  end;
  {$ENDIF}

end.
