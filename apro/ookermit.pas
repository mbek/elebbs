{$S-,R-,V-,I-,B-,F+,O+,A-}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{Include OPro's define file if UseOPro is specified}
{$IFDEF UseOPro}
{$I OPDEFINE.INC}
{$ENDIF}

{*********************************************************}
{*                  OOKERMIT.PAS 2.03                    *}
{*     Copyright (c) TurboPower Software 1991.           *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OOKermit;
  {-Provides Kermit receive and transmit functions (using OOP)}

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
  OOAbsPcl;

const
  {Run-time constants}
  DefMinRepeatCnt : Byte = 4;      {Minimum characters to use repeat prefix}
  FastAbort : Boolean = False;     {Use Error packet for aborting}
  DefHibitPrefix : Char = '&';     {Default char for hibit prefixing}

  {#Z+}
  CancelWait : Word = 182;         {Wait 10 seconds for cancel transmit}

  {Compile-time constants}
  DiscardChar = 'D';               {For signaling an abort}
  MaxWindowSlots = 27;             {Avoids MS-Kermit bug}
  {#Z-}

  {For estimating protocol transfer times}
  KermitOverhead : Word = 20;      {Bytes of overhead for each block}
  KermitTurnDelay : Word = 1000;   {Msecs of turn around delay}
  SWCKermitTurnDelay : Word = 5;   {Msecs of turn around delay on SWC xfers}

  {#Z+}
  {Packet types}
  KBreak           = 'B';        {Break transmission (EOT)}
  KData            = 'D';        {Data packet}
  KError           = 'E';        {Error packet}
  KFile            = 'F';        {File header packet}
  KNak             = 'N';        {Negative acknowledge packet}
  KSendInit        = 'S';        {Initial packet (exchange options)}
  KDisplay         = 'X';        {Display text on screen packet}
  KAck             = 'Y';        {Acknowledge packet}
  KEndOfFile       = 'Z';        {End of file packet}
  {#Z-}

type
  {#Z+}
  {Kermit state machine states}
  KermitStateType = (
    {Transmit states}
    tkInit,           {Send SendInit packet}
    tkInitReply,      {Wait for header reply to SendInit}
    tkCollectInit,    {Collect data packet for SendInit reply}
    tkOpenFile,       {Open next file to transmit}
    tkSendFile,       {Send File packet}
    tkFileReply,      {Wait for header reply to File}
    tkCollectFile,    {Collect data packet for File reply}
    tkCheckTable,     {Check table for space, escape next block if room}
    tkSendData,       {Send next Data packet}
    tkBlockReply,     {Wait for header reply to Data}
    tkCollectBlock,   {Collect data packet for Data reply}
    tkSendEof,        {Send Eof packet}
    tkEofReply,       {Wait for header reply to Eof}
    tkCollectEof,     {Collect data packet for Eof reply}
    tkSendBreak,      {Send Break packet}
    tkBreakReply,     {Wait for header reply to Break}
    tkCollectBreak,   {Collect data packet for Break reply}
    tkComplete,       {Send Complete packet}
    tkWaitCancel,     {Wait for cancel to go out}
    tkError,          {Done, log and clean up}
    tkDone,           {Signals end of protocol}

    {Receive states}
    rkInit,           {Set initial timer}
    rkGetInit,        {Wait for SendInit header}
    rkCollectInit,    {Collect SendInit data field}
    rkGetFile,        {Wait for File header}
    rkCollectFile,    {Collect File data field}
    rkGetData,        {Wait for Data header}
    rkCollectData,    {Collect Data data field}
    rkComplete,       {Normal completion}
    rkWaitCancel,     {Wait for cancel to go out}
    rkError,          {Error completion}
    rkDone);          {Signals end of protocolcompletion}


  {Header state machine states}
  HeaderStateType = (
    hsNone,           {0  No header collection in process}
    hsGotMark,        {1  Got mark}
    hsGotLen,         {2  Got length byte}
    hsGotSeq,         {3  Got sequence number}
    hsGotType,        {4  Got packet type}
    hsGotLong1,       {5  Got first byte of long length}
    hsGotLong2,       {6  Got second byte of long length}
    hsDone);          {7  Got everything}

  DataStateType = (
    dsData,           {0  Collecting data bytes}
    dsCheck1,         {1  Collecting check bytes}
    dsCheck2,         {2  Collecting check bytes}
    dsCheck3);        {3  Collecting check bytes}

  {Kermit option record}
  KermitOptionRec = record
    MaxPacketLen : Byte;
    MaxTimeout : Byte;
    PadCount : Byte;
    PadChar : Char;
    Terminator : Char;
    CtlPrefix : Char;
    HibitPrefix : Char;
    Check : Char;
    RepeatPrefix : Char;
    CapabilitiesMask : Byte;
    WindowSize : Byte;
    MaxLongPacketLen : Word;
  end;

type
  {Holds info about Kermit data in Window slots}
  SlotInfo = record
    InUse   : Boolean;
    Len     : Integer;
    Seq     : Integer;
    Acked   : Boolean;
    Retries : Byte;
  end;

  {Sliding window table, info}
  InfoTableType = array[1..MaxWindowSlots] of SlotInfo;

  {Sliding window table, data}
  DataTablePtr = ^DataTableType;
  DataTableType = array[0..(MaxWindowSlots*1024)-1] of Char;
  {#Z-}

const
  {Default kermit options (from the Kermit Protocol Manual}
  DefKermitOptions : KermitOptionRec =
    (MaxPacketLen : 80;                    {80 characters}
     MaxTimeout :  5;                      {5 seconds}
     PadCount : 0;                         {No pad chars}
     PadChar : #0;                         {Null pad char}
     Terminator : cCR;                     {Carriage return}
     CtlPrefix : '#';                      {'#' char}
     HibitPrefix : 'Y';                    {Space means no hibit prefixing}
     Check : '1';                          {1 byte chksum}
     RepeatPrefix : '~';                   {Default repeat prefix}
     CapabilitiesMask : 0;                 {No default extended caps}
     WindowSize : 0;                       {No default windows}
     MaxLongPacketLen : 0);                {No default long packets}

  {#Z+}
  {Default kermit options (from the Kermit Protocol Manual}
  MissingKermitOptions : KermitOptionRec =
    (MaxPacketLen : 80;                    {80 characters}
     MaxTimeout :  5;                      {5 seconds}
     PadCount : 0;                         {No pad chars}
     PadChar : #0;                         {Null pad char}
     Terminator : cCR;                     {Carriage return}
     CtlPrefix : '#';                      {'#' char}
     HibitPrefix : ' ';                    {No hibit prefixing}
     Check : '1';                          {1 byte chksum}
     RepeatPrefix : ' ';                   {Default repeat prefix}
     CapabilitiesMask : 0;                 {No default extended caps}
     WindowSize : 0;                       {No default windows}
     MaxLongPacketLen : 0);                {No default long packets}
   {#Z-}

type
  {Pointer to a protocol record}
  KermitProtocolPtr = ^KermitProtocol;

  {A Kermit protocol object}
  KermitProtocol = object(AbstractProtocol)
    {General...}
    DataLen          : Word;            {Length of sent packet data field}
    RecDataLen       : Word;            {Length of recd packet data field}
    ActualDataLen    : Word;            {Length decoded data bytes}
    DataBlock        : ^DataBlockType;  {Standard data block (data field)}

    KermitState      : KermitStateType; {Current state of machine}
    HeaderState      : HeaderStateType; {Current header state}
    DataState        : DataStateType;   {Current data state}
    ReplyTimer       : EventTimer;      {Used to timeout replies}
    PacketType       : Char;            {Type of last packet}
    KermitOptions    : KermitOptionRec; {Options for this transfer}
    RmtKermitOptions : KermitOptionRec; {Options remote says to use}
    UsingHibit       : Boolean;         {True if prefixing hibit chars}
    UsingRepeat      : Boolean;         {True if using repeat cnt feature}
    MinRepeatCnt     : Byte;            {Min threshold to use repeat feature}
    RecBlockNum      : Word;            {Blocknum of last received packet}
    ExpectedAck      : Word;            {Blocknum of next expected Ack}
    BlockCheck2      : Word;            {For holding Crc check value}
    CheckKnown       : Boolean;         {True if we've agreed on check type}
    LPInUse          : Boolean;         {True if we're using long packets}
    ReceiveInProgress  : Boolean;       {True if receiving a file}

    {Transmitting...}
    WorkBlock        : ^DataBlockType;  {Holds transmit temp pool}
    WorkLen          : Word;            {Count of bytes in temp pool}
    LastWorkIndex    : Word;            {For managing data pool}
    WorkEndPending   : Boolean;         {True if no more WorkBlocks}

    TableSize        : Byte;            {Size of window table, 1-31}
    InfoTable        : InfoTableType;   {Window table info}
    DataTable        : DataTablePtr;    {Window table data}
    TableHead        : Byte;            {Newest used slot}
    TableTail        : Byte;            {Oldest used slot, rcv only}
    Next2Send        : Integer;         {Slot in table to send}
    BlockIndex       : Word;            {Collects data field}

    {Temp variables used in state machine}
    GetLong          : Boolean;         {True if long header coming}
    SaveStatus       : Word;            {Save status between states}
    SaveCheck        : LongInt;         {Save incoming check between states}
    SaveCheck2       : Word;            {Save incoming check between states}
    LongCheck        : Byte;            {Long header checksum}         {!!.03}
    TimerStarted     : Boolean;         {True once timer has been started}
    Skipped          : Boolean;         {True if file was not accepted}
    TempCheck        : Char;            {Used to collect header chars}
    C1               : Char;            {Used to collect header chars}
    C2               : Char;            {Used to collect header chars}
    C3               : Char;            {Used to collect header chars}

    {General...}
    constructor Init(APPtr : AbstractPortPtr);
      {-Allocates and initializes a protocol control block}
    constructor InitCustom(APPtr : AbstractPortPtr;
                           KOptions : KermitOptionRec;
                           Options : Word);
      {-Allocates and initializes a protocol control block with options}
    destructor Done; virtual;
      {-Disposes of the protocol record}
    procedure SetKermitOptions(KOptions : KermitOptionRec);
      {-Update the KermitProtocol object to use KOptions}
    procedure SetMaxPacketLen(MaxLen : Byte);
      {-Set the maximum packet length}
    procedure SetMaxLongPacketLen(MaxLen : Word);
      {-Set the maximum long packet length}
    procedure SetMaxWindows(MaxNum : Byte);
      {-Set the max number of SWC windows to allow}
    procedure SetSWCTurnDelay(TrnDelay : Word);
      {-Set the turnaround delay factor for SWC transfers}
    procedure SetMaxTimeoutSecs(MaxTimeout : Byte);
      {-Set the maximum time to wait for a packet}
    procedure SetPacketPadding(C : Char; Count : Byte);
      {-Set the pad character and count}
    procedure SetTerminator(C : Char);
      {-Set the packet terminator}
    procedure SetCtlPrefix(C : Char);
      {-Set the control character quote prefix}
    procedure SetHibitPrefix(C : Char);
      {-Set the hibit quote prefix}
    procedure SetRepeatPrefix(C : Char);
      {-Set the repeat quote prefix}
    procedure SetKermitCheck(CType : Byte);
      {-Set the block check type (bcCheckSum1 (default), bcCheckSum2, bcCrcK)}
    function GetSwcSize : Byte;
      {-Return size of sliding window (0 if not in use)}
    procedure GetLPStatus(var InUse : Boolean; var PacketSize : Word);
      {-Return status of long packet feature}
    function WindowsUsed : Word;
      {-Return number of window slots used}

    procedure PrepareTransmitPart; virtual;
      {-Prepare to start transmitting}
    function ProtocolTransmitPart : ProtocolStateType; virtual;
      {-Process current transmit state}

    procedure PrepareReceivePart; virtual;
      {-Prepare to start receiving}
    function ProtocolReceivePart : ProtocolStateType; virtual;
      {-Process current receive state}

    {$IFDEF UseStreams}
    constructor Load(var S : IdStream);
      {-Load a KermitProtocol object from a stream}
    procedure Store(var S : IdStream);
      {-Store a KermitProtocol object to a stream}
    {$ENDIF}

    {#Z+}
    {++++ Internal methods ++++}
    procedure kpRawInit;
    function kpAllocateWindowTable : Boolean;
    procedure kpDeallocateWindowTable;
    procedure apUpdateBlockCheck(CurByte: Byte); virtual;
    procedure apSendBlockCheck; virtual;
    procedure apCancel; virtual;
    procedure apAbortProtocol; virtual;
    procedure apResetStatus;
    procedure kpSendTerminator;
    procedure kpPutToChar(C : Char);
    procedure kpGetDataChar(var C : Char;
                            var TableIndex : Word;
                            var RepeatCnt : Word);
    procedure kpPutHeader(HType : Char; Len : Word);
    procedure kpCheckForHeader;
    function  kpNextSeq(I : Integer) : Integer;
    function  kpPrevSeq(I : Integer) : Integer;
    procedure kpSendAck(Seq : Byte);
    procedure kpSendNak;
    procedure kpProcessOptions;
    procedure kpSendOptions;
    procedure kpSendError(Msg : String);
    function kpCheckRetries : Boolean;
    procedure apFinishWriting; virtual;
    procedure kpReceiveBlock;
    function kpTableFull : Boolean;
    function kpPacketsOutstanding : Boolean;
    procedure kpGotAck(SeqNum : Word);
    function kpSeqInTable(SeqNum : Integer) : Integer;
    procedure kpExtractFileInfo;
    procedure kpSendPacket(PT : Char);
    procedure kpSendFilePacket;
    procedure kpSendDataPacket(Slot : Word);
    procedure kpSendInitialize;
    procedure kpDisplayPacket; virtual;
    procedure apTransmitBlock(var Block : DataBlockType;
                              BLen : Word; BType : Char); virtual;
    procedure kpLoadTransmitData;
    procedure kpOpenFile;
    procedure kpWritePacket(Index : Byte);
    function kpSeqGreater(Seq1, Seq2 : Byte) : Boolean;
    procedure kpProcessDataPacket;
    function kpSeqDiff(Seq1, Seq2 : Byte) : Byte;
    function kpIncTableIndex(Index, Increment : Byte) : Byte;
    function kpDataCount(Index : Byte) : Word;
    procedure kpAddToTable(Seq : Byte);
    procedure kpFlushTableToDisk;
    function kpLoSeq : Byte;
    function kpHiSeq : Byte;
    {#Z-}
  end;

  {$IFDEF UseStreams}
  procedure KermitProtocolStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing kermit objects}
  {$ENDIF}

implementation

const
  {'S' - SendInit packet option index}
  MaxL    = 1;     {Max packet length sender can receive (Def = none)}
  Time    = 2;     {Max seconds to wait before timing out (Def = none)}
  NPad    = 3;     {Number of padding chars before packets (Def = none)}
  PadC    = 4;     {Padding character (Def = Nul)}
  EOL     = 5;     {Packet terminator character (Def = CR)}
  QCtl    = 6;     {Prefix char for control-char encoding (Def = #)}
  QBin    = 7;     {Prefix char for hi-bit encoding (Def = ' ' none)}
  Chkt    = 8;     {1=chksum, 2=2 byte chksum, 3=CRC (Def = 1)}
  Rept    = 9;     {Prefix char for repeat-count encoding (Def = ' ' none)}
  Capa    = 10;    {Advanced capabilities bit masks}
  Windo   = 11;    {Size of the sliding window (in packets)}
  MaxLx1  = 12;    {long packet size div 95}
  MaxLx2  = 13;    {Long packet size mod 95}
  SendInitLen = 13; {Size of SendInit data block}
  MaxKermitOption = 13;

  {Advanced capability bit masks}
  LastMask       = $01;  {Set if more bit masks follow}
  LongPackets    = $02;  {Set if using long packets}
  SlidingWindows = $04;  {Set if using sliding windows}
  FileAttribute  = $08;  {Set if using Attribut packets, not supported}

  {Text strings for various error/abort conditions}
  eRecInitTO = 'Timeout waiting for RecInit packet';
  eFileTO = 'Timeout waiting for File packet';
  eDataTO = 'Timeout waiting for Data packet';
  eSync = 'Failed to syncronize protocol';
  eAsync = 'Blockcheck or other error';
  eCancel = 'Canceled';
  eFileExists = 'Not allowed to overwrite existing file';
  eFileError = 'Error opening or writing file';

  {Check to CheckType conversion array}
  CheckVal : array[1..3] of Byte = (bcChecksum1, bcChecksum2, bcCrcK);

  {Used in ProtocolReceivePart/ProtocolTransmitPart}
  FirstDataState : array[Boolean] of DataStateType = (dsData, dsCheck1);
  FreeMargin = 20;

  {$I OOKERMIT.PA1}   {Kermit part 1}
  {$I OOKERMIT.PA2}   {Kermit part 2}

end.

