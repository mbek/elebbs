{$S-,R-,V-,I-,B-,F+,O+,A-}

{Conditional defines that may affect this unit}
{$I Compiler.inc}
{$I APDEFINE.INC}

{Include OPro's define file if UseOPro is specified}
{$IFDEF UseOPro}
{$I OPDEFINE.INC}
{$ENDIF}

{*********************************************************}
{*                  OOZMODEM.PAS 2.03                    *}
{*     Copyright (c) TurboPower Software 1991.           *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OOZmodem;
  {-Provides Zmodem recieve and transmit functions (using OOP)}

interface

uses
  {$IFNDEF DELPHI}
  Dos,
  {$ELSE}
  Windows,
  Sysutils,
  {$ENDIF}
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
  CfgRec,
  GenDos,
  OOCom,
  OoAbsPcl,
  Multi;

const
  {Compile-time constants}
  MaxAttentionLen = 32;              {Maximum length of attention string}
  ZMaxBlockSize = 8192;              {Maximum data bytes on one zmodem frame}
  ZHandshakeWait = 1092;             {Tics to wait for first hdr (60 secs)}
  PeekTimeout = 5;                   {Wait 5 tics for various peeks}

  {Run-time constants}
  DrainingStatusInterval : Word = 18;{Default status interval for draining eof}
  DefFinishWait : Word = 364;        {Wait time for ZFins, 30 secs}
  DefFinishRetry : Word = 3;         {Retry ZFin 3 times}
  MaxBadBlocks : Byte = 20;          {Quit if this many bad blocks}

  {For estimating protocol transfer times}
  ZmodemTurnDelay : Word = 0;        {Millisecond turnaround delay}
  ZmodemOverHead : Word = 20;        {Default overhead for each data subpacket}

  {For checking max block sizes}
  ZMaxBlk : Array[Boolean] of Word = (1024, 8192);
  ZMaxWrk : Array[Boolean] of Word = (2048, 16384);

  {Zmodem constants}
  ZPad       = '*';                  {Pad}
  ZDle       = ^X;                   {Data link escape}
  ZDleE      = 'X';                  {An escaped data link escape character}
  ZBin       = 'A';                  {Binary header using Crc16}
  ZHex       = 'B';                  {Hex header using Crc16}
  ZBin32     = 'C';                  {Binary header using Crc32}

  {Zmodem frame types}
  ZrQinit    = #0;                   {Request init (to receiver)}
  ZrInit     = #1;                   {Init (to sender)}
  ZsInit     = #2;                   {Init (to receiver) (optional)}
  ZAck       = #3;                   {Acknowledge last frame}
  ZFile      = #4;                   {File info frame (to receiver)}
  ZSkip      = #5;                   {Skip to next file (to receiver)}
  ZNak       = #6;                   {Error receiving last data subpacket}
  ZAbort     = #7;                   {Abort protocol}
  ZFin       = #8;                   {Finished protocol}
  ZRpos      = #9;                   {Resume from this file position}
  ZData      = #10;                  {Data subpacket(s) follows}
  ZEof       = #11;                  {End of current file}
  ZFerr      = #12;                  {Error reading or writing file}
  ZCrc       = #13;                  {Request for file CRC (to receiver)}
  ZChallenge = #14;                  {Challenge the sender}
  ZCompl     = #15;                  {Complete}
  ZCan       = #16;                  {Cancel requested (to either)}
  ZFreeCnt   = #17;                  {Request diskfree}
  ZCommand   = #18;                  {Execute this command (to receiver)}

  {File management options (how and when to accept a file)}
  WriteNewerLonger = 1;          {Transfer if new, newer or longer}
  WriteCrc         = 2;          {Not supported, same as WriteNewer}
  WriteAppend      = 3;          {Transfer if new, append if exists}
  WriteClobber     = 4;          {Transfer regardless}
  WriteNewer       = 5;          {Transfer if new or newer}
  WriteDifferent   = 6;          {Transfer if new or diff dates/lens}
  WriteProtect     = 7;          {Transfer only if new}

type
  {Holds an escaped character}
  Str2 = String[2];

  {Main Zmodem state table}
  ZmodemStateType = (
    {Transmit states}
    tzInitial,       {0  Allocates buffers, sends zrqinit}
    tzHandshake,     {1  Wait for hdr (zrinit), rsend zrqinit on timout}
    tzGetFile,       {2  Call NextFile, build ZFile packet}
    tzSendFile,      {3  Send ZFile packet}
    tzCheckFile,     {4  Wait for hdr (zrpos), set next state to tzData}
    tzStartData,     {5  Send ZData and next data subpacket}
    tzEscapeData,    {6  Check for header, escape next block}
    tzSendData,      {7  Wait for free space in buffer, send escaped block}
    tzSendEof,       {8  Send eof}
    tzWaitAck,       {9  Wait for Ack on ZCRCW packets}
    tzDrainEof,      {10 Wait for output buffer to drain}
    tzCheckEof,      {11 Wait for hdr (zrinit)}
    tzSendFinish,    {12 Send zfin}
    tzCheckFinish,   {13 Wait for hdr (zfin)}
    tzError,         {14 Cleanup after errors}
    tzCleanup,       {15 Release buffers and other cleanup}
    tzDone,          {16 Signal end of protocol}

    {Receive states}
    rzRqstFile,      {17 Send zrinit}
    rzDelay,         {18 Delay for Telix}
    rzWaitFile,      {19 Waits for hdr (zrqinit, zrfile, zsinit, etc)}
    rzCollectFile,   {20 Collect file info into work block}
    rzSendInit,      {21 Extract send init info}
    rzSendBlockPrep, {22 Collect post-hexhdr chars}                    {!!.03}
    rzSendBlock,     {23 Collect sendinit block}
    rzSync,          {24 Send ZrPos with current file position}
    rzStartFile,     {25 Extract file info, prepare writing, etc., put zrpos}
    rzStartData,     {26 Wait for hdr (zrdata)}
    rzCollectData,   {27 Collect data subpacket}
    rzGotData,       {28 Got dsp, put it}
    rzWaitEof,       {29 Wait for hdr (zreof)}
    rzEndOfFile,     {30 Close file, log it, etc}
    rzSendFinish,    {31 Send ZFin, goto rzWaitOO}
    rzCollectFinish, {32 Check for OO, goto rzFinish}
    rzError,         {33 Handle errors while file was open}
    rzWaitCancel,    {34 Wait for the cancel to leave the outbuffer}
    rzCleanup,       {35 Clean up buffers, etc.}
    rzDone);         {36 Signal end of protocol}

  {General header collection states}
  HeaderStateType = (
    hsNone,          {Not currently checking for a header}
    hsGotZPad,       {Got initial or second asterisk}
    hsGotZDle,       {Got ZDle}
    hsGotZBin,       {Got start of binary header}
    hsGotZBin32,     {Got start of binary 32 header}
    hsGotZHex,       {Got start of hex header}
    hsGotHeader);    {Got complete header}

  {Hex header collection states}
  HexHeaderStates = (
    hhFrame,         {Processing frame type char}
    hhPos1,          {Processing 1st position info byte}
    hhPos2,          {Processing 2nd position info byte}
    hhPos3,          {Processing 3rd position info byte}
    hhPos4,          {Processing 4th position info byte}
    hhCrc1,          {Processing 1st CRC byte}
    hhCrc2);         {Processing 2nd CRC byte}

  {Binary header collection states}
  BinaryHeaderStates = (
    bhFrame,         {Processing frame type char}
    bhPos1,          {Processing 1st position info byte}
    bhPos2,          {Processing 2nd position info byte}
    bhPos3,          {Processing 3rd position info byte}
    bhPos4,          {Processing 1th position info byte}
    bhCrc1,          {Processing 1st CRC byte}
    bhCrc2,          {Processing 2nd CRC byte}
    bhCrc3,          {Processing 3rd CRC byte}
    bhCrc4);         {Processing 4th CRC byte}

  {Only two states possible when receiving blocks}
  ReceiveBlockStates = (
    rbData,          {Receiving data bytes}
    rbCrc);          {Receiving block check bytes}

  {Describes working buffer for expanding a standard buffer with escapes}
  WorkBlockType = array[1..2*ZMaxBlockSize] of Char;

  {Describes data area of headers}
  PosFlagsType = array[0..3] of Byte;

  {Pointer to a protocol record}
  ZmodemProtocolPtr = ^ZmodemProtocol;

  {A Zmodem protocol object}
  ZmodemProtocol = object(AbstractProtocol)
    {General...}
    UseCrc32         : Boolean;         {True when using 32bit CRCs}
    CanCrc32         : Boolean;         {True when Crc32 capable}
    LastFileOfs      : LongInt;         {File position reported by remote}
    AttentionStr     : array[1..MaxAttentionLen] of Byte;   {Attn string value}
    ConvertOpts      : Byte;            {File conversion opts rqst by sender}
    FileMgmtOpts     : Byte;            {File mgmt opts rqst by sender}
    TransportOpts    : Byte;            {File transport opts rqst by sender}
    FileMgmtOverride : Boolean;         {True to override senders file mg opts}
    ReceiverRecover  : Boolean;         {True to force file recovery}
    FinishWait       : Word;            {Wait time for ZFin response}
    FinishRetry      : Byte;            {Times to resend ZFin}
    LastFrame        : Char;            {Holds last frame type for status}
    EscapeAll        : Boolean;         {True when escaping all ctl chrs}{!!.02}

    {For controlling autoadjustment of blocksize}
    Use8KBlocks      : Boolean;         {True when using 8K blocks}
    TookHit          : Boolean;         {True if we got ZrPos packet}
    GoodAfterBad     : Word;            {Holds count of good blocks}

    ZmodemState      : ZmodemStateType; {Current Zmodem state}
    HeaderState      : HeaderStateType; {Current Header state}
    ReplyTimer       : EventTimer;      {Used to timeout replies}
    WorkSize         : Word;            {Index into working buffer}
    LastBlock        : Boolean;         {True if no more blocks}
    Terminator       : Char;            {Current block type}
    HexByte          : Byte;            {Used to assemble hex byte}
    HexPending       : Boolean;         {True for next char in hex pair}
    EscapePending    : Boolean;         {True for next char in esc pair}
    ControlCharSkip  : Boolean;         {True when skipping ctl chars} {!!.01}
    HeaderType       : Char;            {Current header type}
    HexHdrState      : HexHeaderStates; {Current hex header state}
    BinHdrState      : BinaryHeaderStates; {Current binary header state}
    RcvBlockState    : ReceiveBlockStates; {Current receive block state}
    FilesSent        : Boolean;         {True if at least one file sent}
    CanCount         : Byte;            {Track contiguous <cancels>}
    SaveStatus       : Word;            {Maintain status across parts}
    CrcCnt           : Byte;            {Number of CRC chars expected}
    LastStatus       : Word;            {Status to set in zpReceiveBlock}
    OCnt             : Byte;            {Count of O's recvd (for 'OO')}
    TimerPending     : Boolean;         {True if ready to start timer}
    DataInTransit    : Word;            {Bytes transmitted in window}
    TimerStarted     : Boolean;         {True if timer started}
    WasHex           : Boolean;         {True if last header was hex}  {!!.03}
    DiscardCnt       : Word;            {Count chars before sendblock} {!!.03}

    {Working buffers}
    DataBlock        : ^DataBlockType;   {Standard data block}
    DataBlockLen     : Word;             {Count of valid data in DataBlock}
    WorkBlock        : ^WorkBlockType;   {Holds fully escaped data block}

    {Receiving...}
    RcvHeader        : PosFlagsType;    {Received header}
    RcvFrame         : Char;            {Type of last received frame}

    {Transmitting...}
    TransHeader      : PosFlagsType;    {Header to transmit}
    RcvBuffLen       : Word;            {Size of receiver's buffer}
    LastChar         : Char;            {Last character sent}

    {General...}
    constructor Init(APPtr : AbstractPortPtr);
      {-Allocates and initializes a protocol control block}
    constructor InitCustom(APPtr : AbstractPortPtr; Options : Word);
      {-Allocates and initializes a protocol control block with options}
    destructor Done; virtual;
      {-Disposes of the protocol record}
    procedure SetFileMgmtOptions(Override, SkipNoFile : Boolean; FOpt : Byte);
      {-Set file mgmt options to use (overrides sender's request}
    procedure SetRecoverOption(OnOff : Boolean);
      {-Turn file recovery on (will be ignored if dest file doesn't exist)}
    procedure SetBigSubpacketOption(UseBig : Boolean);
      {-Turn on/off 8K subpackets}
    procedure SetFinishWait(NewWait : Word; NewRetry : Byte);
      {-Set new finish wait and retry values}

    procedure PrepareTransmitPart; virtual;
      {-Prepare to transmit file in parts}
    function ProtocolTransmitPart : ProtocolStateType; virtual;
      {-Perform one "increment" of a protocol transmit}
    procedure PrepareReceivePart; virtual;
      {-Prepare to receive Zmodem parts}
    function ProtocolReceivePart : ProtocolStateType; virtual;
      {-Perform one "increment" of a protocol receive}

    {$IFDEF UseStreams}
    constructor Load00(var S : IdStream);
      {-Load an ZmodemProtocol object from a stream}
    constructor Load(var S : IdStream);
      {-Load an ZmodemProtocol object from a stream}
    procedure Store(var S : IdStream);
      {-Store an ZmodemProtocol object to a stream}
    {$ENDIF}

    {#Z+}
    {++++ Internal methods ++++}
    procedure zpRawInit;
    procedure apUpdateBlockCheck(CurByte: Byte); virtual;
    procedure apSendBlockCheck; virtual;
    function apVerifyBlockCheck : Boolean; virtual;
    procedure apCancel; virtual;
    procedure apAbortProtocol; virtual;
    procedure zpGetCharStripped(var C : Char);
    procedure zpPutAttentionString;
    procedure zpPutCharHex(C : Char);
    procedure zpPutHexHeader(FrameType : Char);
    procedure zpGetCharEscaped(var C : Char);
    procedure zpGetCharHex(var C : Char);
    function zpCollectHexHeader : Boolean;
    function zpCollectBinaryHeader(Crc32 : Boolean) : Boolean;
    procedure zpCheckForHeader;
    procedure apPrepareWriting; virtual;
    procedure apFinishWriting; virtual;
    procedure zpWriteDataBlock;
    function zpReceiveBlock(var Block : DataBlockType;
                            var BlockSize : Word;
                            var Handshake : Char) : Boolean;
    procedure zpExtractFileInfo;
    procedure zpInsertFileInfo; virtual;
    procedure zpExtractReceiverInfo;
    procedure zpPutCharEscaped(C : Char);
    function zpEscapeChar(C : Char) : Str2;
    procedure zpPutBinaryHeader(FrameType : Char);
    procedure zpEscapeBlock(var Block : DataBlockType;
                            BLen : Word);
    procedure zpTransmitBlock;
    {#Z-}
  end;

  {$IFDEF UseStreams}
  procedure ZmodemProtocolStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing zmodem objects}
  {$ENDIF}

implementation

const
  {For various hex digit manipulations}
  HexDigits : array[0..15] of Char = '0123456789abcdef';

  {For manipulating file management options}
  FileMgmtMask = 7;                {Isolate file mgmnt values}
  FileSkipMask = $80;              {Skip file if dest doesn't exist}

  {Only supported conversion option}
  FileRecover = $03;               {Resume interrupted file transfer}

  {Data subpacket terminators}
  ZCrcE      = 'h';                {End  - last data subpacket of file}
  ZCrcG      = 'i';                {Go   - no response necessary}
  ZCrcQ      = 'j';                {Ack  - requests ZACK or ZRPOS}
  ZCrcW      = 'k';                {Wait - sender waits for answer}

  {Translate these escaped sequences}
  ZRub0      = 'l';                {Translate to $7F}
  ZRub1      = 'm';                {Translate to $FF}

  {Byte offsets for pos/flag bytes}
  ZF0 = 3;                         {Flag byte 3}
  ZF1 = 2;                         {Flag byte 2}
  ZF2 = 1;                         {Flag byte 1}
  ZF3 = 0;                         {Flag byte 0}
  ZP0 = 0;                         {Position byte 0}
  ZP1 = 1;                         {Position byte 1}
  ZP2 = 2;                         {Position byte 2}
  ZP3 = 3;                         {Position byte 3}

  {Bit masks for ZrInit}
  CanFdx  = $0001;           {Can handle full-duplex}
  CanOvIO = $0002;           {Can do disk and serial I/O overlaps}
  CanBrk  = $0004;           {Can send a break}
  CanCry  = $0008;           {Can encrypt/decrypt, not supported}
  CanLzw  = $0010;           {Can LZ compress, not supported}
  CanFc32 = $0020;           {Can use 32 bit CRC}
  EscAll  = $0040;           {Escapes all control chars}               {!!.02}
  Esc8    = $0080;           {Escapes the 8th bit, not supported}

  {Bit masks for ZsInit}
  TEscAll  = $0040;           {Sender needs escaped ctl chars}         {!!.02}
  TEsc8    = $0080;           {Sender needs escaped 8th bit, not supported} {!!.02}

  {Character constants}
  Hibit   = $80;
  cDleHi  = Char(Ord(cDle) + HiBit);
  cXonHi  = Char(Ord(cXon) + HiBit);
  cXoffHi = Char(Ord(cXoff) + HiBit);

{$I OOZMODEM.PA1}         {Zmodem primitives}
{$I OOZMODEM.PA2}         {Zmodem receive/transmit}

  {$IFDEF UseStreams}
  procedure ZmodemProtocolStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing zmodem objects}
  begin
    AbstractProtocolStream(SPtr);
    SPtr^.RegisterType(otZmodemProtocol, veZmodemProtocol,
                       TypeOf(ZmodemProtocol),
                       @ZmodemProtocol.Store, @ZmodemProtocol.Load);
  end;
  {$ENDIF}

end.
