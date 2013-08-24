{$S-,R-,I-,O+,F+}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{Include OPro's define file if UseOPro is specified}
{$IFDEF UseOPro}
{$I OPDEFINE.INC}
{$ENDIF}

{*********************************************************}
{*                 OOASCII.PAS 2.03                      *}
{*     Copyright (c) TurboPower Software 1991.           *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OOAscii;
  {-Provides ASCII recieve and transmit functions (using OOP)}

interface

uses
  Dos,
  {$IFDEF UseOPro}
  OpInline,
  OpString,
  OpRoot,
  OpDate,
  {$ENDIF}
  {$IFDEF UseTPro}
  TpMemChk,
  TpInline,
  TpString,
  TpDate,
  {$ENDIF}
  ApPort,
  ApTimer,
  ApMisc,
  OoCom,
  OoAbsPcl;

const
  {Run-time constants}
  DefInterCharDelay : Word = 0;   {Default is zero ms delay between chars}
  DefInterLineDelay : Word = 0;   {Default is zero ms delay between lines}
  DefEOLChar : Char = cCr;        {Default EOL char is carriage return}
  DefRcvTimeout : Word = 364;    {Default tics to assume end of receive}
  DefBlockLen : Word = 60;        {Default block length (assume avg of 60)}

  {#Z+}
  DefMaxAccumDelay : Word = 30;   {Max accum milliseconds to delay in one call}
  {#Z-}

  {---- Option code for AsciiProtocol ----}
  apSuppressCtrlZ  = $0800;

  DefAsciiOptions  : Word = 0;

type
  {AsciiProtocol states}
  AsciiStateType = (
    taInitial,        {Prepare to transmit file}
    taGetBlock,       {Get next block to transmit}
    taWaitFreeSpace,  {Wait for free space in output buffer}
    taSendBlock,      {Transmit current block, handling char/line delays}
    taFinishDrain,    {Wait for last data to go out}
    taFinished,       {Normal or error completion, cleanup}
    taDone,           {Done with transmit}

    raInitial,        {Prepare to receive file}
    raCollectBlock,   {Collect block}
    raProcessBlock,   {Check for ^Z, write block to disk}
    raFinished,       {Normal or error completion, cleanup}
    raDone);          {Done with receive}

  AsciiProtocolPtr = ^AsciiProtocol;
  AsciiProtocol = object(AbstractProtocol)
    apInterCharDelay : Word;
    apInterLineDelay : Word;
    RcvTimeOut       : Word;
    apEOLChar        : Char;
    CtrlZEncountered : Boolean;
    TimerPending     : Boolean;
    MaxAccumDelay    : Word;
    SendIndex        : Word;
    LastBlockSize    : Word;
    AsciiState       : AsciiStateType;
    DataBlock        : ^DataBlockType;
    NoMoreData       : Boolean;
    ReplyTimer       : EventTimer;
    BlkIndex         : Word;

    constructor Init(APPtr : AbstractPortPtr);
      {-Allocates and initializes a protocol control block}
    constructor InitCustom(APPtr : AbstractPortPtr;
                           InterCharDelay, InterLineDelay : Word;
                           Options : Word);
      {-Allocates and initializes a protocol control block}
    destructor Done; virtual;
      {-Disposes of the protocol record}
    procedure SetDelays(InterCharDelay, InterLineDelay : Word);
      {-Set the delay (in ms) between each character and each line}
    procedure SetEOLChar(C : Char);
      {-Set the character used to mark the end of line}
    function GetLineNumber : LongInt;
      {-Return the current line number}

    procedure PrepareTransmitPart; virtual;
      {-Prepare for calling ProtocolTransmitPart}
    function ProtocolTransmitPart : ProtocolStateType ; virtual;
      {-Performs one increment of a protocol transmit and returns}
    procedure PrepareReceivePart; virtual;
      {-Prepare for calling ProtocolReceivePart}
    function ProtocolReceivePart : ProtocolStateType ; virtual;
      {-Performs one increment of a protocol receive and returns}

    {$IFDEF UseStreams}
    constructor Load(var S : IdStream);
      {-Load an AsciiProtocol object from a stream}
    procedure Store(var S : IdStream);
      {-Store an AsciiProtocol object to a stream}
    {$ENDIF}

    {#Z+}
    procedure apCancel; virtual;
      {-Sends cancel request to remote}
    function apSendBlockPart(var Block : DataBlockType) : Boolean;
      {-Transmits one data block}
    procedure apReceiveBlock(var Block : DataBlockType;
                             var BlockSize : Word;
                             var HandShake : Char); virtual;
      {-Receive on record into Buffer}

    {#Z-}
  end;

{$IFDEF UseStreams}
procedure AsciiProtocolStream(SPtr : IdStreamPtr);
  {-Register all types needed for streams containing AsciiProtocol objects}
{$ENDIF}

implementation

  constructor AsciiProtocol.Init(APPtr : AbstractPortPtr);
    {-Allocates and initializes a protocol control block}
  begin
    {Init ancestor}
    if not AbstractProtocol.InitCustom(APPtr, DefAsciiOptions) then
      Fail;
    ProtType := Ascii;
    apInterCharDelay := DefInterCharDelay;
    apInterLineDelay := DefInterLineDelay;
    apEOLChar        := DefEOLChar;
    CtrlZEncountered := False;
    RcvTimeout       := DefRcvTimeout;
    BlockLen         := DefBlockLen;
    MaxAccumDelay    := DefMaxAccumDelay;

    {!!.01 moved from state machine}
    {Get a protocol DataBlock}
    if not GetMemCheck(DataBlock, SizeOf(DataBlockType)) then begin
      APort^.GotError(epFatal+ecOutOfMemory);
      Fail;
    end;
  end;

  constructor AsciiProtocol.InitCustom(APPtr : AbstractPortPtr;
                                       InterCharDelay, InterLineDelay : Word;
                                       Options : Word);
    {-Allocates and initializes a protocol control block}
  begin
    if not AsciiProtocol.Init(APPtr) then
      Fail;
    apOptionsOn(Options);
    SetDelays(InterCharDelay, InterLineDelay);

    {!!.01 moved from state machine}
    {Get a protocol DataBlock}
    if not GetMemCheck(DataBlock, SizeOf(DataBlockType)) then begin
      APort^.GotError(epFatal+ecOutOfMemory);
      Fail;
    end;
  end;

  destructor AsciiProtocol.Done;
    {-Disposes of the protocol record}
  begin
    AbstractProtocol.Done;
    FreeMemCheck(DataBlock, SizeOf(DataBlockType));                    {!!.01}
  end;

  procedure AsciiProtocol.apCancel;
    {-Sends cancel request to remote}
  begin
  end;

  procedure AsciiProtocol.SetDelays(InterCharDelay, InterLineDelay : Word);
    {-Set the delay (in ms) between each character and each line}
  begin
    apInterCharDelay := InterCharDelay;
    apInterLineDelay := InterLineDelay;
  end;

  procedure AsciiProtocol.SetEOLChar(C : Char);
    {-Set the character used to mark the end of line}
  begin
    apEOLChar := C;
  end;

  function AsciiProtocol.GetLineNumber : LongInt;
    {-Return the current line number}
  begin
    GetLineNumber := BlockNum;
  end;

  function AsciiProtocol.apSendBlockPart(var Block : DataBlockType) : Boolean;
    {-Transmits one data block, handling delays}
  var
    C : Char;
    AccumDelay : Byte;
  begin
    with APort^ do begin
      {Assume not finished}
      apSendBlockPart := False;

      AccumDelay := 0;
      repeat
        C := Block[SendIndex];

        {Check for ^Z exit}
        if (C = ^Z) and FlagIsSet(apFlags, apSuppressCtrlZ) then begin
          apSendBlockPart := True;
          Exit;
        end;

        {Send the character}
        PutChar(C);
        Inc(SendIndex);
        Inc(BytesTransferred);

        {Handle char delay}
        if apInterCharDelay > 0 then begin
          Delay(apInterCharDelay);
          Inc(AccumDelay);
          if AccumDelay > MaxAccumDelay then
            Exit;
        end;

        {Handle line delay}
        if (C = apEOLChar) and (apInterLineDelay > 0) then begin
          Delay(apInterLineDelay);
          Inc(AccumDelay);
          if AccumDelay > MaxAccumDelay then
            Exit;
        end;
      until (SendIndex > LastBlockSize);
      apSendBlockPart := True;
    end;
  end;

  procedure AsciiProtocol.apReceiveBlock(var Block : DataBlockType;
                                         var BlockSize : Word;
                                         var HandShake : Char);
    {-Receive block into Buffer}
  var
    I : Word;

  begin
    {Check for ^Z}
    for I := 1 to BlockSize do begin
      if Block[I] = ^Z then begin
        BlockSize := I;
        CtrlZEncountered := True;
      end;
    end;

    {Update data areas and show status}
    Inc(BytesTransferred, BlockSize);
    Dec(BytesRemaining, BlockSize);
    if BytesRemaining < 0 then
      BytesRemaining := 0;
    ElapsedTics := ElapsedTime(Timer);
  end;

  procedure AsciiProtocol.PrepareTransmitPart;
    {-Starts Ascii protocol transmit}
  begin
    AbstractProtocol.PrepareTransmitPart;

    {Do startup and init stuff}
    apResetStatus;
    apShowFirstStatus;
    CtrlZEncountered := False;
    BlockNum := 0;
    FindingFirst := True;
    FileListIndex := 0;


    {Transmit one file}
    if not NextFile(@Self, Pathname) then begin
      {AsyncStatus already set}
      apShowLastStatus;
      APort^.PR^.ProtocolActive := False;
      Exit;
    end;

    ForceStatus := True;
    AsciiState := taInitial;
    {DataBlock := nil;}                                                {!!.01}
  end;

  function AsciiProtocol.ProtocolTransmitPart : ProtocolStateType;
    {-Performs one increment of a protocol transmit and returns}
  label
    ExitPoint;
  begin
    {Check for user abort}
    if apHandleAbort then begin
      AsciiState := taFinished;
      ForceStatus := False;
    end;

    {Show status periodically}
    if TimerExpired(StatusTimer) or ForceStatus then begin
      ForceStatus := False;
      NewTimer(StatusTimer, StatusInterval);
      apUserStatus(False, False);
    end;

    {Process current state}
    case AsciiState of
      taInitial :
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

          {Pathname must already be set before we get here}
          Pathname := StUpcase(Pathname);

          {Show file name to user logging routine}
          LogFile(@Self, lfTransmitStart);

          {Go prepare for reading protocol blocks}
          apPrepareReading;
          if AsyncStatus <> ecOk then begin
            AsciiState := taFinished;
            goto ExitPoint;
          end;

          {Start sending blocks}
          FileOfs := 0;
          AsciiState := taGetBlock;
          NewTimer(Timer, 1);
        end;

      taGetBlock :
        begin
          LastBlockSize := BlockLen;
          NoMoreData := apReadProtocolBlock(DataBlock^, LastBlockSize);
          AsciiState := taWaitFreespace;
          NewTimer(ReplyTimer, TransTimeout);
          SendIndex := 1;
        end;

      taWaitFreeSpace :
        begin
          if APort^.OutBuffFree > BlockLen then
            AsciiState := taSendBlock
          else if TimerExpired(ReplyTimer) then begin
            {Must be buffer full error}
            APort^.GotError(epFatal+ecBufferIsFull);
            AsciiState := taFinished;
          end;
        end;

      taSendBlock :
        begin
          {Don't send empty blocks (will only happen with empty files)}
          if LastBlockSize <= 0 then
            AsciiState := taFinished
          else begin
            {If no errors, then send this block to the remote}
            if AsyncStatus = ecOk then begin
              if apSendBlockPart(DataBlock^) then begin

                {If SendBlockPart failed, go clean up}
                if AsyncStatus <> ecOk then begin
                  APort^.FlushOutBuffer;
                  AsciiState := taFinished;
                  goto ExitPoint;
                end;

                {Adjust block number and file position}
                Inc(BlockNum);
                Inc(FileOfs, BlockLen);

                {Go get next block to send}
                if NoMoreData then begin
                  AsciiState := taFinishDrain;
                  NewTimer(ReplyTimer, TransTimeout);
                end else
                  AsciiState := taGetBlock;

                {Force a status update}
                ForceStatus := True;
                ElapsedTics := ElapsedTime(Timer);
              end;
            end else begin
              {Most likely a disk read error, have to give up}
              AsciiState := taFinished;
            end;
          end;
        end;

      taFinishDrain :
        begin
          if (APort^.OutBuffUsed <= 1) then begin
            AsciiState := taFinished;
            AsyncStatus := ecEndFile;
          end;
          if TimerExpired(ReplyTimer) then begin
            APort^.GotError(epNonFatal+ecTimeout);
            AsciiState := taFinished;
          end;
        end;

      taFinished :
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
          AsciiState := taDone;
          APort^.PR^.ProtocolActive := False;
        end;
    end;

ExitPoint:
    {Set function result}
    case AsciiState of
      taInitial,
      taGetBlock,
      taSendBlock,
      taFinished             : ProtocolTransmitPart := psReady;

      taFinishDrain,
      taWaitFreeSpace        : ProtocolTransmitPart := psWaiting;

      taDone                 : ProtocolTransmitPart := psFinished;
    end;
  end;

  procedure AsciiProtocol.PrepareReceivePart;
    {-Prepare for calling ProtocolReceivePart}
  begin
    {Do parent inits}
    AbstractProtocol.PrepareReceivePart;

    {Prepare to enter state machine}
    TimerPending := False;
    AsciiState := raInitial;
    {DataBlock := nil;}                                                {!!.01}
    apResetStatus;
    AsyncStatus := ecOk;
    apShowFirstStatus;
  end;

  function AsciiProtocol.ProtocolReceivePart : ProtocolStateType;
    {-Performs one increment of a protocol receive and returns}
  label
    ExitPoint;
  var
    C : Char;
    BlockSize : Word;
    Error : Boolean;
    Handshake : Char;
    SaveStatus : Word;
    Aborted : Boolean;                                                 {!!.02}
  begin
    {Check for user abort}
    if AsciiState <> raFinished then                                   {!!.02}
      if apHandleAbort then begin
        AsciiState := raProcessBlock;                                  {!!.02}
        AsyncStatus := ecEndFile;
        ForceStatus := False;
        Aborted := True;                                               {!!.02}
      end else                                                         {!!.02}
        Aborted := False;                                              {!!.02}

    {Show status periodically}
    if AsciiState <> raFinished then                                   {!!.02}
      if TimerExpired(StatusTimer) or ForceStatus then begin
        ForceStatus := False;
        NewTimer(StatusTimer, StatusInterval);
        SaveStatus := AsyncStatus;
        if AsyncStatus = ecOk then
          AsyncStatus := ecAsciiReceiveInProgress;
        apUserStatus(False, False);
        AsyncStatus := SaveStatus;
      end;

    {Process current state}
    case AsciiState of
      raInitial :
        begin
          {!!.01 moved to constructor}
          {Get a protocol DataBlock}
          {if not GetMemCheck(DataBlock, SizeOf(DataBlockType)) then begin
            APort^.GotError(epFatal+ecOutOfMemory);
            AsciiState := raFinished;
            goto ExitPoint;
          end;}

          {Pathname should already have name of file to receive}
          if Pathname = '' then begin
            APort^.GotError(epFatal+ecNoFilename);
            AsciiState := raFinished;
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
            APort^.GotError(epNonFatal+ecFileRejected);
            AsciiState := raFinished;
            goto ExitPoint;
          end;

          {Prepare file for writing protocol blocks}
          apPrepareWriting;
          if AsyncStatus <> ecOk then begin
            AsciiState := raFinished;
            goto ExitPoint;
          end;

          {Prepare to collect first block}
          AsciiState := raCollectBlock;
          FileOfs := 0;
          BlockNum := 1;
          NewTimer(ReplyTimer, RcvTimeout);
          TimerPending := True;
          BlkIndex := 0;
          ForceStatus := True;
        end;

      raCollectBlock :
        begin
          while APort^.CharReady and (BlkIndex < BlockLen) do begin

            {Start the protocol timer on the first received character}
            if TimerPending then begin
              TimerPending := False;
              NewTimer(Timer, 1);
            end;

            APort^.GetChar(C);
            Inc(BlkIndex);
            DataBlock^[BlkIndex] := C;
          end;

          if BlkIndex >= BlockLen then
            {Got a complete block, go process it}
            AsciiState := raProcessBlock
          else if TimerExpired(ReplyTimer) then begin
            {Timeout out waiting for complete block, assume EOF}
            CtrlZEncountered := True;
            AsciiState := raProcessBlock;
          end;
        end;

      raProcessBlock :
        begin
          {Go process what's in DataBlock}
          BlockSize := BlkIndex;
          apReceiveBlock(DataBlock^, BlockSize, Handshake);
          if (AsyncStatus = ecOk) or                                   {!!.02}
             (AsyncStatus = ecEndFile) then begin                      {!!.02}
            Error := apWriteProtocolBlock(DataBlock^, BlockSize);
            if AsyncStatus = ecOk then begin
              {Normal received block}
              Inc(FileOfs, BlockSize);
              NewTimer(ReplyTimer, RcvTimeout);
              ForceStatus := True;
              BlkIndex := 0;
              if CtrlZEncountered or Aborted then                      {!!.02}
                AsciiState := raFinished
              else
                AsciiState := raCollectBlock;
            end else
              {Error during write, clean up and exit}
              AsciiState := raFinished;
          end else
            {End of file}
            AsciiState := raFinished;
        end;

      raFinished :
        begin
          apFinishWriting;
          LogFile(@Self, lfReceiveOk);
          apShowLastStatus;
          APort^.PR^.ProtocolActive := False;
          {FreeMemCheck(DataBlock, SizeOf(DataBlock^));}               {!!.01}
          AsciiState := raDone;
          AsyncStatus := ecOk;
        end;
    end;

ExitPoint:
    {Set function result}
    case AsciiState of
      raInitial,
      raProcessBlock,
      raFinished           : ProtocolReceivePart := psReady;

      raCollectBlock       : ProtocolReceivePart := psWaiting;

      raDone               : ProtocolReceivePart := psFinished;
    end;
  end;

  {$IFDEF UseStreams}
  procedure AsciiProtocolStream(SPtr : IdStreamPtr);
  begin
    AbstractProtocolStream(SPtr);
    SPtr^.RegisterType(otAsciiProtocol, veAsciiProtocol,
                       TypeOf(AsciiProtocol),
                       @AsciiProtocol.Store, @AsciiProtocol.Load);
  end;

  constructor AsciiProtocol.Load(var S : IdStream);
    {-Load an AsciiProtocol object from a stream}
  begin
    if not AbstractProtocol.Load(S) then
      Fail;
    S.ReadRange(apInterCharDelay, TimerPending);
    S.Read(TimerPending, SizeOf(Boolean));
  end;

  procedure AsciiProtocol.Store(var S : IdStream);
    {-Store an AsciiProtocol object to a stream}
  begin
    AbstractProtocol.Store(S);
    S.WriteRange(apInterCharDelay, TimerPending);
    S.Write(TimerPending, SizeOf(Boolean));
  end;
  {$ENDIF}

end.
