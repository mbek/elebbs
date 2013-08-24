{$S-,R-,V-,I-,B-,F+,O+,A-}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{Include OPro's define file if UseOPro is specified}
{$IFDEF UseOPro}
{$I OPDEFINE.INC}
{$ENDIF}

{*********************************************************}
{*                   OOABSPCL.PAS 2.03                   *}
{*     Copyright (c) TurboPower Software 1991.           *}
{* Portions copyright (c) Information Technology 1989,   *}
{*    and used under license to TurboPower Software      *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OoAbsPcl;
  {-AbstractProtocol definition}

interface

uses
  {$IFNDEF DELPHI}
  Dos,
  {$ELSE}
  Windows,
  Sysutils,
  JDates,
  {$ENDIF}
  {$IFDEF UseOPro}
  OpInline,
  OpString,
  OpRoot,
  {$IFDEF Opro12}
  OpConst,
  {$ENDIF}
  OpDate,
  {$ENDIF}
  {$IFDEF UseTPro}
  TpMemChk,
  TpInline,
  TpString,
  TpDate,
  {$ENDIF}
  ApMisc,
  ApPort,
  ApTimer,
  OOCom,
  CfgRec,
  GenDos;

const DrainSecs = 5;

const
  {Compile-time options}
  FileBufferSize = 8192;    {Size of working buffer for receive/xmit files}
  DefHandshakeWait = 182;   {Wait time for resp during handshake (10 sec)}
  DefHandshakeRetry = 10;   {Number of times to retry handshake}
  DefTransTimeout = 1092;   {Tics to wait for receiver flow control release}
  DefStatusInterval = 91;   {5 seconds between status updates}

  {Run-time constants}
  BlockFillChar : Char = ^Z;     {Fill character for partial protocol blocks}
  GmtHourOffset : Integer = 0;   {Default no GMT adjustments}
  TelixDelay    : Byte = 9;      {Delay handshake for 9 tics (for Telix)}

  {---- Option codes for AbstractProtocol ----}
  apIncludeDirectory   = $0001;   {True to include directory in file names}
  apHonorDirectory     = $0002;   {True to honor directory in file names}
  apRTSLowForWrite     = $0004;   {True to lower RTS during disk writes}
  apKermitNoStripName  = $0008;   {True to *not* strip names in Kermit}
  apKermitDisplay      = $1000;   {True to honor KDisplay packets}
  apKermitLongPackets  = $2000;   {True to support long packets}
  apKermitSWC          = $4000;   {True to support SWC}
  apZmodem8K           = $8000;   {True to support 8K blocks}
  apBP2KTransmit       = $0100;   {True if 2K B+ transmit desired}

  {---- Default options for AbstractProtocol ----}
  DefProtocolOptions    : Word = 0;
  BadProtocolOptions    : Word = apKermitLongPackets+apKermitSWC+apZmodem8K;

  {Block check codes}
  bcNone      = 0;        {No block checking}
  bcChecksum1 = 1;        {Basic checksum}
  bcChecksum2 = 2;        {Two byte checksum}
  bcCrc16     = 3;        {16 bit Crc}
  bcCrc32     = 4;        {32 bit Crc}
  bcCrcK      = 5;        {Kermit style Crc}

const
  {Convenient blockcheck string constants}
  bcsNone      = 'No check ';
  bcsChecksum1 = 'Checksum ';
  bcsChecksum2 = 'Checksum2';
  bcsCrc16     = 'Crc16    ';
  bcsCrc32     = 'Crc32    ';
  bcsCrck      = 'CrcKermit';

const
  {Constants for supported protocol types}
  Xmodem      = 0;
  XmodemCRC   = 1;
  Xmodem1K    = 2;
  Xmodem1KG   = 3;
  Ymodem      = 4;
  YmodemG     = 5;
  Zmodem      = 6;
  Kermit      = 7;
  Ascii       = 8;
  BPlus       = 9;
  UserProt1   = 10;
  UserProt2   = 11;
  UserProt3   = 12;

const
  {Convenient protocol string constants}
  ProtocolTypeString : array[XModem..UserProt3] of String[10] = (
    'Xmodem', 'XmodemCRC', 'Xmodem1K', 'Xmodem1KG',
    'Ymodem', 'YmodemG', 'Zmodem', 'Kermit', 'Ascii',
    'BPlus', 'UserProt1', 'UserProt2', 'UserProt3');

type
  {Pointer to a protocol object}
  AbstractProtocolPtr = ^AbstractProtocol;

  {General protocol states}
  ProtocolStateType = (
    psReady,           {Ok to call again immediately}
    psWaiting,         {Protocol is waiting, ok to do something else}
    psFinished);       {Protocol is finished}

  {Action to take if not allowed to write a file}
  WriteFailOptions =
    (WriteFail, WriteRename, WriteAnyway, WriteResume);

  {For storing received and transmitted blocks}
  DataBlockType = array[1..1024] of Char;

  {For buffering received and transmitted files}
  FileBufferArray = array[0..FileBufferSize-1] of Byte;

  {For holding lists of files to transmit}
  FileListType = array[0..65535-1] of Char;
  FileListPtr = ^FileListType;

  {For specifying log file calls}
  LogFileType = (lfReceiveStart,
                 lfReceiveOk,
                 lfReceiveFail,
                 lfReceiveSkip,
                 lfTransmitStart,
                 lfTransmitOk,
                 lfTransmitFail,
                 lfTransmitSkip);

  {User procedure types}
  ShowStatusProc = procedure(AP : AbstractProtocolPtr;
                             Starting, Ending : Boolean);
  NextFileFunc = function(AP : AbstractProtocolPtr;
                          var FName : PathStr) : Boolean;
  LogFileProc = procedure(AP : AbstractProtocolPtr;
                          LogFileStatus : LogFileType);
  AcceptFileFunc = function(AP : AbstractProtocolPtr) : Boolean;
  UserBackProc = procedure(AP : AbstractProtocolPtr);

  {An abstract protocol object}
  AbstractProtocol = object(Root)
    {General...}
    APort            : AbstractPortPtr;   {Pointer to port object}
    ProtType         : Byte;              {Protocol type}
    SrcFileLen       : LongInt;           {Size of file (in bytes)}
    SrcFileDate      : LongInt;           {Timestamp of source file}
    UserStatus       : ShowStatusProc;    {Hook for user display}
    BatchProtocol    : Boolean;           {True if protocol supports batch}
    BlockCheck       : LongInt;           {Block check value}
    CheckType        : Byte;              {Code for block check type}
    HandshakeWait    : Word;              {Wait seconds during handshaking}
    HandshakeRetry   : Byte;              {Attempts to retry handshaking}
    HandshakeAttempt : Word;              {Current handshake attempt}
    BlockLen         : Word;              {Either 128 or 1024}
    BlockNum         : Word;              {Current block number}
    apFlags          : Word;              {AbstractProtocol options}
    TransTimeout     : Word;              {Tics to wait for trans freespace}
    GotOneFile       : Boolean;           {True if we've received one file}
    InitFilePos      : LongInt;           {Initial file pos during resumes}

    {For getting the next file to transmit}
    PathName         : PathStr;           {Complete path name of current file}
    NextFile         : NextFileFunc;      {NextFile function}
    CurRec           : SearchRec;         {NextFileMask search record}
    SearchMask       : PathStr;           {NextFileMask search mask}
    FindingFirst     : Boolean;           {NextFileMask flag}
    FileList         : FileListPtr;       {NextFileList list pointer}
    FileListIndex    : Word;              {NextFileList index}

    {When receiving files}
    DestDir          : DirStr;            {Destination directory}

    {Miscellaneous hooks}
    LogFile          : LogFileProc;       {User proc to call when file received}
    AcceptFile       : AcceptFileFunc;    {User proc to accept rcvd files}
    UserBack         : UserBackProc;           {User background procedure}

    {New fields that don't need to be stored in streams}
    FileListMax      : Word;              {Size of file list}

    {Status...}
    BytesRemaining   : LongInt;           {Bytes not yet transferred}
    BytesTransferred : LongInt;           {Bytes already transferred}
    BlockErrors      : Word;              {Number of tries for block}
    TotalErrors      : Word;              {Number of total tries}
    Timer            : EventTimer;        {Used to time a transfer}
    ElapsedTics      : LongInt;           {Elapseds tics as of last block}
    InProgress       : Byte;              {Non-zero if protocol in progress}
    EotCheckCount    : Byte;              {Number of Eot retries required}
    ActCPS           : Word;              {Port or modem CPS}
    Overhead         : Word;              {Overhead bytes per block}
    TurnDelay        : Word;              {MSec turnaround delay}
    StatusTimer      : EventTimer;        {How often to show status}
    ForceStatus      : Boolean;           {Force status update}
    StatusInterval   : Word;              {Tics between status updates}

    {File buffer managment...}
    WorkFile         : File;              {Temp file for Get/PutProtocolBlock}
    FileBuffer       : ^FileBufferArray;  {For reading/writing files}
    StartOfs         : LongInt;           {Holds starting offset of file}
    EndOfs           : LongInt;           {Holds ending offset of file}
    LastOfs          : LongInt;           {FileOfs of last Get/Put}
    FileOfs          : LongInt;           {Current file offset}
    EndOfDataOfs     : LongInt;           {Ofs of buffer of end-of-file}
    EndPending       : Boolean;           {True when end-of-file is in buffer}
    WriteFailOpt     : WriteFailOptions;  {Rules for overwriting files}
    FileOpen         : Boolean;           {True if file open in protocol}
    SaveMode         : Byte;              {Save FileMode}              {!!.02}

    {Stream stuff}
    DeallocPort      : Boolean;           {True if we need to dispose the port}

    {Misc.}
    UserData         : LongInt;           {Reserved for user data storage}

    {General methods...}
    constructor InitCustom(AP : AbstractPortPtr; Options : Word);
      {-Allocates and initializes a protocol control block}
    destructor Done; virtual;
      {-Destroys a protocol}
    procedure SetShowStatusProc(SProc : ShowStatusProc);
      {-Sets a user status function}
    procedure SetNextFileFunc(NFFunc : NextFileFunc);
      {-Sets function for batch protocols to call to get file to transmit}
    procedure SetFileMask(NewMask : PathStr);
      {-Sets dir/file mask for the built-in NextFileMask function}
    procedure SetFileList(FLP : FileListPtr);
      {-Sets the file list to use for the built-in NextFileList function}
    procedure MakeFileList(var FLP : FileListPtr; Size : Word);
      {-Allocates a new file list of Size bytes}
    procedure DisposeFileList(var FLP : FileListPtr; Size : Word);     {!!.01}
      {-Disposes of file list FLP}
    procedure AddFileToList(FLP : FileListPtr; PName : PathStr);
      {-Adds pathname PName to file list FLP}
    procedure SetDestinationDirectory(Dir : DirStr);
      {-Set the destination directory for received files}
    procedure SetReceiveFilename(Fname : PathStr);
      {-Give a name to the file to be received}
    procedure SetLogFileProc(LFP : LogFileProc);
      {-Sets a procedure to log file transfers}
    procedure SetAcceptFileFunc(AFP : AcceptFileFunc);
      {-Sets a procedure to be called when a file is received}
    procedure SetBackgroundProc(BP : UserBackProc);
      {-Sets a background procedure to be called while a file is transferred}
    procedure SetHandshakeWait(NewHandshake, NewRetry : Word);
      {-Set the wait tics and retry count for the initial handshake}
    procedure SetOverwriteOption(Opt : WriteFailOptions);
      {-Set option for what to do when the destination file already exists}
    procedure SetActualBPS(BPS : LongInt);
      {-Sets actual BPS rate (only needed if modem differs from port)}
    procedure SetEfficiencyParms(BlockOverhead, TurnAroundDelay : Word);
      {-Sets efficiency parameters for EstimateTransferSecs}
    procedure SetProtocolPort(AP : AbstractPortPtr);
      {-Set AP as the port object for this protocol}
    procedure apOptionsOn(OptionFlags : Word);
      {-Activate multiple options}
    procedure apOptionsOff(OptionFlags : Word);
      {-Deactivate multiple options}
    function apOptionsAreOn(OptionFlags : Word) : Boolean;
      {-Return True if all specified options are on}
    procedure PrepareTransmitPart; virtual;
      {-Parent-level inits for derived protocols}
    function ProtocolTransmitPart : ProtocolStateType ; virtual;
      {-Abstract - must be overridden}
    procedure ProtocolTransmit; virtual;
      {-Loops on ProtocolTransmitPart until finished}
    procedure PrepareReceivePart; virtual;
      {-Parent-level inits for derived protocols}
    function ProtocolReceivePart : ProtocolStateType ; virtual;
      {-Abstract - must be overridden}
    procedure ProtocolReceive; virtual;
      {-Loops on ProtocolReceivePart until finished}

    {Status methods...}
    function GetFilename : PathStr;
      {-Returns the name of the current file}
    function GetPathname : PathStr;
      {-Returns the complete pathname of the current file (if known)}
    function GetFileSize : LongInt;
      {-Returns current file size (0 if no file active)}
    function GetBytesRemaining : LongInt;
      {-Return bytes not yet transferred}
    function GetBytesTransferred : LongInt;
      {-Returns bytes already transferred}
    function GetElapsedTics : LongInt;
      {-Returns tics since first block was sent (or received)}
    function GetBlockErrors : Word;
      {-Returns the number of errors received this block}
    function GetTotalErrors : Word;
      {-Returns the number of errors recieved this transfer}
    function GetProtocol : Byte;
      {-Returns the current protocol type}
    function GetBlockSize : Word;
      {-Returns the current block size}
    function GetBlockNum : Word;
      {-Returns the current block number}
    function GetCurrentBlockNum : Word;
      {-Returns the number of the block being transferred}
    function SupportsBatch : Boolean;
      {-Returns True if this protocol supports batch file transfers}
    function GetCheckType : Byte;
      {-Returns the bcXxx code for the block check type}
    function GetInitialFilePos : LongInt;
      {-Returns the file position at the start of resumed file transfer}
    function EstimateTransferSecs(Size : LongInt) : LongInt; virtual;
      {-Returns estimated seconds to transfer Size bytes}

    {#Z+}
    {$IFDEF UseStreams}
    constructor Load00(var S : IdStream);
      {-Abstract Load for a old-style protocol object}
    constructor Load(var S : IdStream);
      {-Abstract Load for a protocol object}
    procedure Store(var S : IdStream);
      {-Abstract Store for a protocol object}
    {$ENDIF}

    {++++ Internal methods ++++}
    procedure apRawInit;
    procedure apUpdateBlockCheck(CurByte : Byte); virtual;
    procedure apSendBlockCheck; virtual;
    function apVerifyBlockCheck : Boolean; virtual;
    procedure apCancel; virtual;
    function apGetFirstBlockNum : Byte; virtual;
    function apGetHandshakeChar : Char; virtual;
    procedure apResetStatus;
    procedure apShowFirstStatus;
    procedure apShowLastStatus;
    function apProcessHandshake : Boolean; virtual;
    function apNextFile(var FName : PathStr) : Boolean; virtual;
    procedure apPrepareReading; virtual;
    function apReadProtocolBlock(var Block : DataBlockType;
                                 var BlockSize : Word) : Boolean; virtual;
    procedure apFinishReading; virtual;
    function apProcessBlockReply : Boolean; virtual;
    procedure apTransmitBlock(var Block : DataBlockType;
                              BLen : Word; BType : Char); virtual;
    procedure apTransmitEot(First : Boolean); virtual;
    procedure apSendHandshakeChar(Handshake : Char); virtual;
    procedure apPrepareWriting; virtual;
    function apWriteProtocolBlock(var Block : DataBlockType;
                                  BlockSize : Word) : Boolean; virtual;
    procedure apFinishWriting; virtual;
    procedure apReceiveBlock(var Block : DataBlockType;
                             var BlockSize : Word;
                             var HandShake : Char); virtual;
    function apTrimZeros(S : string) : string;
    function apOctalStr(L : LongInt) : String;
    function apOctalStr2Long(S : String) : LongInt;
    function apPackToYMTimeStamp(RawTime : LongInt) : LongInt;
    function apYMTimeStampToPack(YMTime : LongInt) : LongInt;
    function apCurrentTimeStamp : LongInt;
    procedure apAbortProtocol; virtual;
    function apHandleAbort : Boolean;
    function apDrainOutput(CanCnt : Byte) : Boolean;
    function apWaitForFreeSpace(W, T : Word) :  Boolean;
    function apCrc32OfFile(FName : PathStr; Len : LongInt) : LongInt;
    procedure apUserStatus(Starting, Ending : Boolean); virtual;
    {#Z-}
  end;

  {Empty procedures}
  procedure NoStatus (AP : AbstractProtocolPtr;
                      Starting, Ending : Boolean);
    {-Empty show status procedure}
  function NoNextFile(AP : AbstractProtocolPtr) : Boolean;
    {-Empty next file function -- always returns False}
  procedure NoLogFile(AP : AbstractProtocolPtr; LogFileStatus : LogFileType);
    {-Empty LogFile procedure}
  function NoAcceptFile(AP : AbstractProtocolPtr) : Boolean;
    {-Empty AcceptFile function}
  procedure NoUserBack(AP : AbstractProtocolPtr);
    {-Empty UserBack procedure}

  {Built-in NextFile functions}
  function NextFileMask(AP : AbstractProtocolPtr;
                        var FName : PathStr) : Boolean;
    {-Built-in function that works with file mask fields}
  function NextFileList(AP : AbstractProtocolPtr;
                        var FName : PathStr) : Boolean;
    {-Built-in function that works with a list of files}

  {Built-in AcceptFile function}
  function AcceptOneFile(AP : AbstractProtocolPtr) : Boolean;
    {-Built-in function that accepts one file only}

  {$IFDEF UseStreams}
  procedure AbstractProtocolStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing protocol objects}
  {$ENDIF}

  {#Z+}
  function LoCaseMac(Ch : Char) : Char;
  {$IFDEF MSDOS}
    {-Lowercase character macro, no international character support}
    inline(
      $58/                     {POP  AX}
      $3C/$41/                 {CMP    AL,'A'}
      $72/$06/                 {JB     No}
      $3C/$5A/                 {CMP    AL,'Z'}
      $77/$02/                 {JA     No}
      $0C/$20);                {OR     AL,$20}
                               {No:}
  {$ENDIF}
  {#Z-}

implementation

{!!.01}
{$IFDEF MSDOS}
procedure IntOff; inline($9C/$FA);      {PUSHF/CLI}
procedure IntOn; inline($9D);           {POPF}
{$ELSE}
procedure IntOff; begin; end;
procedure IntOn; begin; end;
{$ENDIF}

var
  {Set to 1/1/1970 00:00 GMT}
  StartDate : DateTimeRec;

  {$IFNDEF MSDOS}
  function LoCaseMac(Ch : Char) : Char;
  begin
    if CH in ['A'..'Z'] then LoCaseMac := Chr(Ord(CH) OR $20)
      else LoCaseMac := CH;
  end;
  {$ENDIF}

  procedure AbstractProtocol.apRawInit;
    {-Perform low-level initializations}
  begin
    UserStatus := {$IFDEF FPC}@{$ENDIF}NoStatus;
    HandshakeWait := DefHandshakeWait;
    HandshakeRetry := DefHandshakeRetry;
    BlockLen := 128;
    PathName := '';
    SrcFileLen := 0;
    SrcFileDate := 0;
    ElapsedTics := 0;
    BytesRemaining := 0;
    BytesTransferred := 0;
    InProgress := 0;
    EotCheckCount := 1;
    BatchProtocol := False;
    WriteFailOpt := WriteFail;
    FileOpen := False;
    NextFile := {$IFDEF FPC}@{$ENDIF}NextFileMask;
    SearchMask := '';
    apFlags := DefPortOptions;
    LogFile := {$IFDEF FPC}@{$ENDIF}NoLogFile;
    AcceptFile := {$IFDEF FPC}@{$ENDIF}NoAcceptFile;
    UserBack := {$IFDEF FPC}@{$ENDIF}NoUserBack;
    DestDir := '';
    CheckType := bcNone;
    DeallocPort := False;
    TransTimeout := DefTransTimeout;
    Overhead := 0;
    TurnDelay := 0;
    InitFilePos := 0;
    StatusInterval := DefStatusInterval;
  end;

  constructor AbstractProtocol.InitCustom(AP : AbstractPortPtr;
                                          Options : Word);
    {-Allocates and initializes a protocol control block with options}
  begin
    Root.Init;
    AsyncStatus := ecOk;

    {Save the port object}
    APort := AP;

    {Initialize the protocol fields}
    apRawInit;
    apFlags := Options;
    ActCPS := APort^.PR^.CurBaud div 10;

    {!!.01}
    {Allocate a file buffer, used by Read/WriteProtocolBlock}
    if not GetMemCheck(FileBuffer, FileBufferSize) then begin
      APort^.GotError(epFatal+ecOutOfMemory);
      Fail;
    end;
  end;

  destructor AbstractProtocol.Done;
    {-Destroys a protocol}
  begin
    if DeallocPort then
      Dispose(APort, Done);
    FreeMemCheck(FileBuffer, FileBufferSize);                          {!!.01}
  end;

  procedure AbstractProtocol.SetShowStatusProc(SProc : ShowStatusProc);
    {-Sets a user status function}
  begin
    UserStatus := SProc;
  end;

  procedure AbstractProtocol.SetNextFileFunc(NFFunc : NextFileFunc);
    {-Sets function for batch protocols to call to get file to transmit}
  begin
    NextFile := NFFunc;
  end;

  procedure AbstractProtocol.SetFileMask(NewMask : PathStr);
    {-Sets dir/file mask for built-in NextFileMask function}
  begin
    SearchMask := NewMask;
  end;

  procedure AbstractProtocol.SetFileList(FLP : FileListPtr);
    {-Sets the file list to use for the built-in NextFileList function}
  begin
    FileList := FLP;
  end;

  procedure AbstractProtocol.MakeFileList(var FLP : FileListPtr; Size : Word);
    {-Allocates a new file list of Size bytes}
  begin
    AsyncStatus := ecOk;
    if GetMemCheck(FLP, Size) then begin
      FillChar(FLP^, Size, 0);
      FileListMax := Size;
    end else
      APort^.GotError(epFatal+ecOutOfMemory);
  end;

  procedure AbstractProtocol.DisposeFileList(var FLP : FileListPtr;    {!!.01}
                                             Size : Word);             {!!.01}
    {-Disposes of file list FLP}
  begin
    FreeMemCheck(FLP, Size);
  end;

  procedure AbstractProtocol.AddFileToList(FLP : FileListPtr; PName : PathStr);
    {-Adds pathname PName to file list FLP}
  const
    Separator = ';';
    EndOfListMark = #0;
  var
    I : Word;
  begin
    AsyncStatus := ecOk;

    {Search for the current end of the list}
    i := 0;
    while i < FileListMax - 1 do
     begin
       if FLP^[I] = EndOfListMark then begin
        {Found the end of the list -- try to add the new file}
        if (LongInt(I)+Length(PName)+1) >= FileListMax then begin
          {Not enough room to add file}
          APort^.GotError(epNonFatal+ecOutOfMemory);
          Exit;
        end else begin
          {There's room -- add the file}
          if I <> 0 then begin
            FLP^[I] := Separator;
            Inc(I);
          end;
          Move(PName[1], FLP^[I], Length(PName));
          FLP^[I+Length(PName)] := EndOfListMark;
          Exit;
        end;
      end;

      inc(i);
     end; { while }
    {Never found endoflist marker}
    APort^.GotError(epFatal+ecBadFileList);
  end;

  procedure AbstractProtocol.SetDestinationDirectory(Dir : DirStr);
    {-Set the destination directory for received files}
  begin
    DestDir := Dir;
  end;

  procedure AbstractProtocol.SetReceiveFilename(Fname : PathStr);
    {-Give a name to the file to be received}
  begin
    if (DestDir <> '') and (JustPathName(Fname) = '') then
      Pathname := AddBackSlash(DestDir)+Fname
    else
      Pathname := Fname;
  end;

  procedure AbstractProtocol.SetLogFileProc(LFP : LogFileProc);
    {-Sets a procedure to be called when a file is received}
  begin
    LogFile := LFP;
  end;

  procedure AbstractProtocol.SetAcceptFileFunc(AFP : AcceptFileFunc);
    {-Sets a procedure to be called when a file is received}
  begin
    AcceptFile := AFP;
  end;

  procedure AbstractProtocol.SetBackgroundProc(BP : UserBackProc);
    {-Sets a background procedure to be called while a file is transferred}
  begin
    UserBack := BP;
  end;

  procedure AbstractProtocol.SetHandshakeWait(NewHandshake,
                                              NewRetry : Word);
    {-Set the wait tics for the initial handshake}
  begin
    if NewHandshake <> 0 then
      HandshakeWait := NewHandshake;
    if NewRetry <> 0 then
      HandshakeRetry := NewRetry;
  end;

  procedure AbstractProtocol.SetOverwriteOption(Opt : WriteFailOptions);
    {-Set option for what to do when the destination file already exists}
  begin
    WriteFailOpt := Opt;
  end;

  procedure AbstractProtocol.SetActualBPS(BPS : LongInt);
    {-Sets actual CPS rate (only needed if modem differs from port)}
  begin
    ActCPS := BPS div 10;
  end;

  procedure AbstractProtocol.SetEfficiencyParms(BlockOverhead,
                                                TurnAroundDelay : Word);
    {-Sets efficiency parameters for EstimateTransferSecs}
  begin
    Overhead := BlockOverhead;
    TurnDelay := TurnAroundDelay;
  end;

  procedure AbstractProtocol.SetProtocolPort(AP : AbstractPortPtr);
    {-Set AP as the port object for this protocol}
  begin
    APort := AP;
  end;

  procedure AbstractProtocol.apOptionsOn(OptionFlags : Word);
    {-Activate multiple options}
  begin
    apFlags := apFlags or (OptionFlags and not BadProtocolOptions);
  end;

  procedure AbstractProtocol.apOptionsOff(OptionFlags : Word);
    {-Deactivate multiple options}
  begin
    apFlags := apFlags and not (OptionFlags and not BadProtocolOptions);
  end;

  function AbstractProtocol.apOptionsAreOn(OptionFlags : Word) : Boolean;
    {-Return True if all specified options are on}
  begin
    apOptionsAreOn := (apFlags and OptionFlags = OptionFlags);
  end;

  procedure AbstractProtocol.PrepareTransmitPart;
    {-Prepare to transmit in parts}
  begin
    FindingFirst := True;
    FileListIndex := 0;
    APort^.PR^.ProtocolActive := True;
    AsyncStatus := ecOk;
  end;

  function AbstractProtocol.ProtocolTransmitPart : ProtocolStateType;
    {-Abstract - must be overridden}
  begin
    ProtocolTransmitPart := psFinished;
  end;

  procedure AbstractProtocol.ProtocolTransmit;
    {-Used the derived part methods to transmit all files}
  var
    State : ProtocolStateType;
  begin

    PrepareTransmitPart;
    if AsyncStatus <> ecOk then
      Exit;
    repeat
      State := ProtocolTransmitPart;

      APort^.DrainOutBuffer(DrainSecs);

      if State = psWaiting then
        UserBack(@Self);
    until State = psFinished;
  end;

  procedure AbstractProtocol.PrepareReceivePart;
    {-Parent-level inits for derived protocols}
  begin
    APort^.PR^.ProtocolActive := True;
    GotOneFile := False;
    AsyncStatus := ecOk;
  end;

  function AbstractProtocol.ProtocolReceivePart : ProtocolStateType;
    {-Receive a batch of files}
  begin
    ProtocolReceivePart := psFinished;
  end;

  procedure AbstractProtocol.ProtocolReceive;
    {-Use the derived part methods to receive all files}
  var
    State : ProtocolStateType;
  begin
    PrepareReceivePart;
    if AsyncStatus <> ecOk then
      Exit;
    repeat
      State := ProtocolReceivePart;

      APort^.DrainOutBuffer(DrainSecs);

      if State = psWaiting then
        UserBack(@Self);
    until State = psFinished;
  end;

  function AbstractProtocol.GetFilename : PathStr;
    {-Returns the name of the current file}
  begin
    GetFileName := JustFilename(Pathname);
  end;

  function AbstractProtocol.GetPathname : PathStr;
    {-Returns the complete pathname of the current file (if known)}
  begin
    GetPathname := Pathname;
  end;

  function AbstractProtocol.GetFileSize : LongInt;
    {-Returns current file size (0 if no file active)}
  begin
    GetFileSize := SrcFileLen;
  end;

  function AbstractProtocol.GetBytesRemaining : LongInt;
    {-Return bytes not yet transferred}
  var
    BR : Longint;
  begin
    BR := SrcFileLen - GetBytesTransferred;
    if BR < 0 then
      BR := 0;
    GetBytesRemaining := BR;
  end;

  function AbstractProtocol.GetBytesTransferred : LongInt;
    {-Returns bytes already transferred}
  var
    TotalOverhead : Word;
    OutBuffUsed : Word;
    BT : LongInt;
  begin
    OutBuffUsed := APort^.OutBuffUsed;
    if OutBuffUsed >= BlockLen then begin
      {Upload in progress, subtract outbuff from bytestransferred}
      if BlockLen <> 0 then                                            {!!.01}
        TotalOverhead := Overhead * (OutBuffUsed div BlockLen)
      else                                                             {!!.01}
        TotalOverhead := Overhead;                                     {!!.01}
      BT := BytesTransferred - (OutBuffUsed - TotalOverhead);
      if BT > 0 then
        GetBytesTransferred := BT
      else
        GetBytesTransferred := 0;
    end else
      GetBytesTransferred := BytesTransferred;
  end;

  function AbstractProtocol.GetElapsedTics : LongInt;
    {-Returns tics since first block was sent (or received)}
  begin
    GetElapsedTics := ElapsedTics;
  end;

  function AbstractProtocol.GetBlockErrors : Word;
    {-Returns the number of errors received this block}
  begin
    GetBlockErrors := BlockErrors;
  end;

  function AbstractProtocol.GetTotalErrors : Word;
    {-Returns the number of errors recieved this transfer}
  begin
    GetTotalErrors := TotalErrors;
  end;

  function AbstractProtocol.GetProtocol : Byte;
    {-Returns the current protocol type}
  begin
    GetProtocol := ProtType;
  end;

  function AbstractProtocol.GetBlockSize : Word;
    {-Returns the current block size}
  begin
    GetBlockSize := BlockLen;
  end;

  function AbstractProtocol.GetBlockNum : Word;
    {-Returns the current block number}
  begin
    GetBlockNum := GetBytesTransferred div BlockLen;
  end;

  function AbstractProtocol.GetCurrentBlockNum : Word;
    {-Returns the number of the block being transferred}
  var
    BT : Longint;
    Block : Word;
  begin
    BT := GetBytesTransferred;
    Block := BT div BlockLen;
    if BT mod BlockLen <> 0 then
      Inc(Block);
    GetCurrentBlockNum := Block;
  end;

  function AbstractProtocol.SupportsBatch : Boolean;
    {-Returns True if this protocol supports batch file transfers}
  begin
    SupportsBatch := BatchProtocol;
  end;

  function AbstractProtocol.GetCheckType : Byte;
    {-Returns the bcXxx code for the block check type}
  begin
    GetCheckType := CheckType;
  end;

  function AbstractProtocol.GetInitialFilePos : LongInt;
    {-Returns the file position at the start of resumed file transfer}
  begin
    GetInitialFilePos := InitFilePos;
  end;

  function AbstractProtocol.EstimateTransferSecs(Size : LongInt) : LongInt;
    {-Return estimated seconds to transfer Size bytes}
  var
    Efficiency : LongInt;
    EffectiveCPS : LongInt;
  begin
    if Size = 0 then
      EstimateTransferSecs := 0
    else begin
      {Calcuate efficiency of this protocol}
      Efficiency := (BlockLen * LongInt(100)) div
                    (BlockLen + OverHead +
                    ((LongInt(TurnDelay) * ActCPS) div 1000));         {!!.02}
      EffectiveCPS := (ActCPS * Efficiency) div 100;

      {Calculate remaining seconds}
      if EffectiveCPS > 0 then
        EstimateTransferSecs := Size div EffectiveCPS
      else
        EstimateTransferSecs := 0;
    end;
  end;

  {$IFDEF UseStreams}
  constructor AbstractProtocol.Load00(var S : IdStream);
    {-Abstract Load for a protocol object}
  begin
    {Init those fields not stored in the stream}
    apRawInit;

    {Load the AbstractPort pointer}
    APort := S.ReadPointer;
    if APort = nil then begin
      APort := AbstractPortPtr(S.GetPtr);
      if S.PeekStatus <> 0 then begin
        Done;
        Fail;
      end else
        DeallocPort := True;
    end;

    {Port's now loaded, set ActCPS}
    ActCPS := APort^.PR^.CurBaud div 10;

    {Load the protocol type}
    S.Read(ProtType, SizeOf(ProtType));

    {Load the UserStatus pointer}
    @UserStatus := S.ReadUserPointer(@NoStatus);

    {Load various other fields}
    S.ReadRange(BatchProtocol, NextFile);

    {Load the NextFile pointer}
    @NextFile := S.ReadUserPointer(@NextFileMask);

    {Load various other fields}
    S.ReadRange(CurRec, LogFile);

    {Load the LogFile and AcceptFile pointers}
    @LogFile := S.ReadUserPointer(@NoLogFile);
    @AcceptFile := S.ReadUserPointer(@NoAcceptFile);

    {Read in overhead calcs}
    S.Read(Overhead, SizeOf(Overhead));
    S.Read(TurnDelay, SizeOf(TurnDelay));

    {Exit if anything failed}
    if S.PeekStatus <> 0 then begin
      Done;
      Fail;
    end;
  end;

  constructor AbstractProtocol.Load(var S : IdStream);
    {-Abstract Load for a protocol object}
  begin
    {Load everthing from Load00 constructor}
    if not AbstractProtocol.Load00(S) then
      Fail;

    {Load the background hook}
    @UserBack := S.ReadUserPointer(@NoUserBack);

    {Exit if anything failed}
    if S.PeekStatus <> 0 then begin
      Done;
      Fail;
    end;
  end;

  procedure AbstractProtocol.Store(var S : IdStream);
    {-Abstract Store for a protocol object}
  begin
    {Store the AbstractPort pointer}
    S.WriteUserPointer(APort, ptNil);
    if not S.PointerRegistered(Aport) then
      S.PutPtr(APort);

    {Store the protocol type}
    S.Write(ProtType, SizeOf(ProtType));

    {Store the user status pointer}
    S.WriteUserPointer(@UserStatus, ptNoUserStatus);

    {Store additional fields}
    S.WriteRange(BatchProtocol, NextFile);

    {Store the next file pointer}
    S.WriteUserPointer(@NextFile, ptNextFileMask);

    {Store additional fields}
    S.WriteRange(CurRec, LogFile);

    {Store the log file and accept file pointers}
    S.WriteUserPointer(@LogFile, ptNoLogFile);
    S.WriteUserPointer(@AcceptFile, ptNoAcceptFile);

    {Store in overhead calcs}
    S.Write(Overhead, SizeOf(Overhead));
    S.Write(TurnDelay, SizeOf(TurnDelay));

    {Store UserBack}
    S.WriteUserPointer(@UserBack, ptNoUserBack);
  end;
  {$ENDIF}

  procedure AbstractProtocol.apUpdateBlockCheck(CurByte : Byte);
    {-Updates the block check character (whatever it is)}
  begin
    RunError(211);
  end;

  procedure AbstractProtocol.apSendBlockCheck;
    {-Makes final adjustment and sends the BlockCheck character}
  begin
    RunError(211);
  end;

  function AbstractProtocol.apVerifyBlockCheck : Boolean;
    {-Receives and checks the block check value}
  begin
    RunError(211);
  end;

  procedure AbstractProtocol.apCancel;
    {-Sends cancel request to remote}
  begin
    RunError(211);
  end;

  function AbstractProtocol.apGetFirstBlockNum : Byte;
    {-Returns first block number}
  begin
    RunError(211);
  end;

  function AbstractProtocol.apGetHandshakeChar : Char;
    {-Returns proper handshake character}
  begin
    RunError(211);
  end;

  procedure AbstractProtocol.apResetStatus;
    {-Conditionally reset all status vars}
  begin
    if InProgress = 0 then begin
      {New protocol, reset status vars}
      SrcFileLen := 0;
      BytesRemaining := 0;
    end;
    BytesTransferred := 0;
    ElapsedTics := 0;
    BlockErrors := 0;
    BlockNum := 0;
    TotalErrors := 0;
  end;

  procedure AbstractProtocol.apShowFirstStatus;
    {-Show (possible) first status}
  begin
    apUserStatus((InProgress = 0), False);
    Inc(InProgress);
  end;

  procedure AbstractProtocol.apShowLastStatus;
    {-Reset field and show last status}
  begin
    if InProgress <> 0 then begin
      Dec(InProgress);
      apUserStatus(False, (InProgress = 0));
    end;
  end;

  procedure AbstractProtocol.apPrepareReading;
    {-Prepare to send protocol blocks (usually opens a file)}
  var
    Result : Word;
  begin
    AsyncStatus := ecOk;

    {If file is already open then leave without doing anything}
    if FileOpen then
      Exit;

    {Report notfound error for empty filename}
    if PathName = '' then begin
      APort^.GotError(epFatal+ecFileNotFound);
      Exit;
    end;

    {!!.01 moved to constructor}
    {Allocate a file buffer}
    {if not GetMemCheck(FileBuffer, FileBufferSize) then begin
      APort^.GotError(epFatal+ecOutOfMemory);
      Exit;
    end;}

    {Open up the previously specified file}
    SaveMode := FileMode;                                              {!!.02}
    FileMode := AproFileMode;                                   {!!.02}{!!.03}
    Assign(WorkFile, PathName);
    {$i-}
    Reset(WorkFile, 1);
    FileMode := SaveMode;                                              {!!.02}
    Result := IOResult;
    if Result <> 0 then begin
      APort^.GotError(epFatal+Result);
      {FreeMemCheck(FileBuffer, FileBufferSize);}                      {!!.01}
      Exit;
    end;

    {Show file name and size}
    SrcFileLen := FileSize(WorkFile);
    BytesRemaining := SrcFileLen;
    apUserStatus(False, False);

    {Note file date/time stamp (for those protocols that care)}
    GetFTime(WorkFile, SrcFileDate);

    {Initialize the buffering variables}
    StartOfs := 0;
    EndOfs := 0;
    LastOfs := 0;
    EndPending := False;
    FileOpen := True;
  end;

  procedure AbstractProtocol.apFinishReading;
    {-Clean up after reading protocol blocks (usually closes a file)}
  begin
    if FileOpen then begin
      {Error or end-of-protocol, clean up}
      Close(WorkFile);
      if IOResult <> 0 then ;
      {FreeMemCheck(FileBuffer, FileBufferSize);}                      {!!.01}
      FileOpen := False;
    end;
  end;

  function AbstractProtocol.apReadProtocolBlock(var Block : DataBlockType;
                                                var BlockSize : Word) : Boolean;
    {-Return with a block to transmit (True to quit)}
  var
    BytesRead : NumReadType;
    BytesToMove : Word;
    BytesToRead : NumreadType;
    ResultTmp : Word;
  begin
    AsyncStatus := ecOk;

    {Check for a request to start further along in the file (recovering)}
    {if (LastOfs = 0) and (FileOfs > 0) then}
    if FileOfs > EndOfs then
      {First call to read is asking to skip blocks -- force a reread}
      EndOfs := FileOfs;

    {Check for a request to retransmit an old block}
    if FileOfs < LastOfs then
      {Retransmit - reset end-of-buffer to force a reread}
      EndOfs := FileOfs;

    if (FileOfs + BlockSize) > EndOfs then begin
      {Buffer needs to be updated, First shift end section to beginning}
      BytesToMove := EndOfs - FileOfs;
      if BytesToMove > 0 then
        Move(FileBuffer^[FileOfs - StartOfs], FileBuffer^, BytesToMove);

      {Fill end section from file}
      BytesToRead := FileBufferSize - BytesToMove;
      Seek(WorkFile, EndOfs);
      BlockRead(WorkFile, FileBuffer^[BytesToMove], BytesToRead, BytesRead);
      ResultTmp := IOResult;
      if (ResultTmp <> 0) then begin
        {Exit on error}
        APort^.GotError(epFatal+ResultTmp);
        apReadProtocolBlock := True;
        BlockSize := 0;
        Exit;
      end else begin
        {Set buffering variables}
        StartOfs := FileOfs;
        EndOfs := FileOfs + FileBufferSize;
      end;

      {Prepare for the end of the file}
      if BytesRead < BytesToRead then begin
        EndOfDataOfs := BytesToMove + BytesRead;
        FillChar(FileBuffer^[EndofDataOfs], FileBufferSize - EndOfDataOfs,
                 BlockFillChar);
        Inc(EndOfDataOfs, StartOfs);
        EndPending := True;
      end else
        EndPending := False;
    end;

    {Return the requested block}
    Move(FileBuffer^[(FileOfs - StartOfs)], Block, BlockSize);
    apReadProtocolBlock := False;
    LastOfs := FileOfs;

    {If it's the last block then say so}
    if EndPending and ((FileOfs + BlockSize) >= EndOfDataOfs) then begin
      apReadProtocolBlock := True;
      BlockSize := EndOfDataOfs - FileOfs;
    end;
  end;

  function AbstractProtocol.apProcessHandshake : Boolean;
    {-Process initial handshake, return true to exit}
  begin
    RunError(211);
  end;

  function AbstractProtocol.apNextFile(var FName : PathStr) : Boolean;
    {-Virtual method for calling NextFile procedure}
  begin
    apNextFile := NextFile(@Self, FName);
  end;

  function AbstractProtocol.apProcessBlockReply : Boolean;
    {-Process reply to last block; return True if TransmitBlock should exit}
  begin
    RunError(211);
  end;

  procedure AbstractProtocol.apTransmitBlock(var Block : DataBlockType;
                                             BLen : Word; BType : Char);
    {-Transmits one data block}
  begin
    RunError(211);
  end;

  procedure AbstractProtocol.apTransmitEot(First: Boolean);
    {-Signal end-of-transmit to remote}
  begin
    RunError(211);
  end;

  procedure AbstractProtocol.apSendHandshakeChar(Handshake : Char);
    {-Send the current handshake char}
  begin
    RunError(211);
  end;

  procedure AbstractProtocol.apPrepareWriting;
    {-Prepare to save protocol blocks (usually opens a file)}
  var
    Dir : DirStr;
    Name : NameStr;
    Ext : ExtStr;
    ResultTmp : Word;
  label
    ExitPoint;
  begin
    {!!.01 moved to constructor}
    {Allocate a file buffer}
    {if not GetMemCheck(FileBuffer, FileBufferSize) then begin
      APort^.GotError(epFatal+ecOutOfMemory);
      Exit;
    end;}

    {Does the file exist already?}
    SaveMode := FileMode;                                              {!!.02}
    FileMode := AproFileMode;                                   {!!.02}{!!.03}
    Assign(WorkFile, PathName);
    {$i-}
    Reset(WorkFile, 1);
    FileMode := SaveMode;                                              {!!.02}
    ResultTmp := IOResult;

    {Exit on errors other than FileNotFound}
    if (ResultTmp <> 0) and (ResultTmp <> 2) and (ResultTmp <> 110) then begin
      APort^.GotError(epFatal+ResultTmp);
      goto ExitPoint;
    end;

    {Exit if file exists and option is WriteFail}
    if (ResultTmp = 0) and (WriteFailOpt = WriteFail) then begin
      APort^.GotError(epNonFatal+ecFileAlreadyExists);
      goto ExitPoint;
    end;

    Close(WorkFile);
    if IOResult = 0 then ;

    {Change the file name if it already exists the option is WriteRename}
    if (ResultTmp = 0) and (WriteFailOpt = WriteRename) then begin
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
    ResultTmp := IOResult;
    if ResultTMp <> 0 then begin
      APort^.GotError(epFatal+ResultTmp);
      goto ExitPoint;
    end;

    {Initialized the buffer management vars}
    StartOfs := 0;
    LastOfs := 0;
    EndOfs := StartOfs + FileBufferSize;
    FileOpen := True;
    Exit;

ExitPoint:
    Close(WorkFile);
    if IOResult <> 0 then ;
    {FreeMemCheck(FileBuffer, FileBufferSize);}                        {!!.01}
  end;

  procedure AbstractProtocol.apFinishWriting;
    {-Cleans up after saving all protocol blocks}
  var
    BytesToWrite : Word;
    BytesWritten : NumReadtype;
    ResultTmp : Word;
  begin
    if FileOpen then begin
      {Error or end-of-protocol, commit buffer and cleanup}
      BytesToWrite := FileOfs - StartOfs;
      BlockWrite(WorkFile, FileBuffer^, BytesToWrite, BytesWritten);
      ResultTmp := IOResult;
      if (ResultTmp <> 0) then
        APort^.GotError(epFatal+ResultTmp);
      if (BytesToWrite <> BytesWritten) then
        APort^.GotError(epFatal+ecDiskFull);

      {Get file size and time for those protocols that don't know}
      SrcFileLen := FileSize(WorkFile);
      GetFTime(WorkFile, SrcFileDate);

      Close(WorkFile);
      if IOResult <> 0 then ;
      {FreeMemCheck(FileBuffer, FileBufferSize);}                      {!!.01}
      FileOpen := False;
    end;
  end;

  function AbstractProtocol.apWriteProtocolBlock(var Block : DataBlockType;
                                               BlockSize : Word) : Boolean;
    {-Write a protocol block (return True to quit)}
  var
    ResultTmp : Word;
    BytesToWrite : Word;
    BytesWritten : NumReadType;

    procedure BlockWriteRTS;
      {-Set RTS before BlockWrite (assumes all BlockWrite params filled in)}
    var
      OffState    : Boolean;
      DTR, RTS    : Boolean;
      CurrentlyOn : Boolean;
    begin
      with APort^, PR^ do begin
        if FlagIsSet(apFlags, apRTSLowForWrite) then begin
          IntOff;                                                      {!!.01}
          GetModem(DTR, RTS);
          if FlagIsSet(HWFRecHonor, hfUseRTS) and
             FlagIsSet(HWFRecMask, hfRTSActiveLow) then begin
            OffState := True;
            CurrentlyOn := not RTS;
          end else begin
            OffState := False;
            CurrentlyOn := RTS;
          end;
          if CurrentlyOn then                                          {!!.01}
            APort^.SetRTS(OffState);                                   {!!.01}
          IntOn;                                                       {!!.01}
        end else                                                       {!!.02}
          CurrentlyOn := False;                                        {!!.02}

        BlockWrite(WorkFile, FileBuffer^, BytesToWrite, BytesWritten);
        if FlagIsSet(apFlags, apRTSLowForWrite) and CurrentlyOn then
          APort^.SetRTS(not OffState);
        AsyncStatus := ecOK;                                           {!!.02}
      end;
    end;

  begin
    AsyncStatus := ecOk;
    apWriteProtocolBlock := True;

    if not FileOpen then begin
      APort^.GotError(epFatal+ecNotOpen);
      Exit;
    end;

    if FileOfs < LastOfs then
      {This is a retransmitted block}
      if FileOfs > StartOfs then begin
        {FileBuffer has some good data, commit that data now}
        Seek(WorkFile, StartOfs);
        BytesToWrite := FileOfs - StartOfs;
        BlockWriteRTS;
        ResultTmp := IOResult;
        if (ResultTmp <> 0) then begin
          APort^.GotError(epFatal+ResultTmp);
          Exit;
        end;
        if (BytesToWrite <> BytesWritten) then begin
          APort^.GotError(epFatal+ecDiskFull);
          Exit;
        end;
      end else begin
        {Block is before data in buffer, discard data in buffer}
        StartOfs := FileOfs;
        EndOfs := StartOfs + FileBufferSize;
        {Position file just past last good data}
        Seek(WorkFile, FileOfs);
        ResultTmp := IOResult;
        if ResultTmp <> 0 then begin
          APort^.GotError(epFatal+ResultTmp);
          Exit;
        end;
      end;

    {Will this block fit in the buffer?}
    if (FileOfs + BlockSize) > EndOfs then begin
      {Block won't fit, commit current buffer to disk}
      BytesToWrite := FileOfs - StartOfs;
      BlockWriteRTS;
      ResultTmp := IOResult;
      if (ResultTmp <> 0) then begin
        APort^.GotError(epFatal+ResultTmp);
        Exit;
      end;
      if (BytesToWrite <> BytesWritten) then begin
        APort^.GotError(epFatal+ecDiskFull);
        Exit;
      end;

      {Reset the buffer management vars}
      StartOfs := FileOfs;
      EndOfs := StartOfs + FileBufferSize;
      LastOfs := FileOfs;
    end;

    {Add this block to the buffer}
    Move(Block, FileBuffer^[FileOfs - StartOfs], BlockSize);
    Inc(LastOfs, BlockSize);
    apWriteProtocolBlock := False;
  end;

  procedure AbstractProtocol.apReceiveBlock(var Block : DataBlockType;
                                            var BlockSize : Word;
                                            var HandShake : Char);
    {-Receive one block}
  begin
    RunError(211);
  end;

  function AbstractProtocol.apTrimZeros(S : string) : string;
    {-Return a string with leading and trailing white space removed}
  var
    I : Word;
    SLen : Byte absolute S;
  begin
    while (SLen > 0) and (S[SLen] <= ' ') do
      Dec(SLen);

    I := 1;
    while (I <= SLen) and ((S[I] <= ' ') or (S[I] = '0')) do
      Inc(I);
    Dec(I);
    if I > 0 then
      Delete(S, 1, I);

    apTrimZeros := S;
  end;

  function AbstractProtocol.apOctalStr(L : LongInt) : String;
    {-Convert L to octal base string}
  const
    Digits : array[0..7] of Char = '01234567';
  var
    I : Word;
  begin
    apOctalStr[0] := #12;
    for I := 0 to 11 do begin
      apOctalStr[12-I] := Digits[L and 7];
      L := L shr 3;
    end;
  end;

  function AbstractProtocol.apOctalStr2Long(S : String) : LongInt;
    {-Convert S from an octal string to a longint}
  const
    HiMag = 10;
    Magnitude : array[1..HiMag] of LongInt =
      (1, 8, 64, 512, 4096, 32768, 262144, 2097152, 16777216, 134217728);
    ValidDigits : set of '0'..'7' = ['0', '1', '2', '3', '4', '5', '6', '7'];
  var
    I, J : Integer;
    Len : Byte absolute S;
    Part, ResultTmp : LongInt;
    Code : NumreadType;
  begin
    {Assume failure}
    apOctalStr2Long := 0;

    {Remove leading blanks and zeros}
    S := apTrimZeros(S);

    {Return 0 for invalid strings}
    if Len > HiMag then
      Exit;

    {Convert it}
    ResultTmp := 0;
    J := 1;
    for I := Len downto 1 do begin
      if not (S[I] in ValidDigits) then
        Exit;
      {$IFNDEF VER0_99_11}
        Val(S[I], Part, Code);
      {$ENDIF}
      ResultTmp := ResultTmp + Part * Magnitude[J];
      Inc(J);
    end;
    apOctalStr2Long := ResultTmp
  end;

  function AbstractProtocol.apPackToYMTimeStamp(RawTime : LongInt) : LongInt;
    {-Return date/time stamp as octal seconds since 1/1/1970 00:00 GMT}
  var
    DT : DateTime;
    DTR : DateTimeRec;
    DiffDays : Word;
    DiffSecs : LongInt;
  begin
    {Convert to julian date}
    UnpackTime(RawTime, DT);
    with DT do begin
      DTR.D := DMYtoDate(Day, Month, Year);
      DTR.T := HMStoTime(Hour, Min, Sec);
    end;

    {Subtract GMT hour offset}
    IncDateTime(DTR, DTR, 0, -(3600 * GmtHourOffset));

    {Diff between date/time stamp and 1/1/1970 (in seconds)}
    DateTimeDiff(DTR, StartDate, DiffDays, DiffSecs);
    apPackToYMTimeStamp := DiffSecs + (DiffDays * SecondsInDay);
  end;

  function AbstractProtocol.apYMTimeStampToPack(YMTime : LongInt) : LongInt;
    {-Return a file time stamp in packed format from a Ymodem time stamp}
  var
    DT : DateTime;
    DTR  : DateTimeRec;
    Ptime : LongInt;
    H,M,S : Byte;
{$IFNDEF MSDOS}
   TempDay,
   TempMonth,
   TempYear  : Integer;
{$ENDIF}
  begin
    {Add the time stamp to StartDate}
    IncDateTime(StartDate, DTR, 0, YMTime);

    {Add the GMT hour offset}
    IncDateTime(DTR, DTR, 0, 3600 * GmtHourOffset);

    {Convert to DT format}
    with DT do begin
      {$IFDEF MSDOS}
        DateToDMY(DTR.D, Integer(Day), Integer(Month), Integer(Year));
      {$ELSE}
        TempDay := Day;
        TempMonth := Month;
        TempYear := Year;

        DateToDMY(DTR.D, TempDay, TempMonth, TempYear);

        Year := TempYear;
        Month := TempMonth;
        Day := TempDay;
      {$ENDIF}
      TimeToHMS(DTR.T, H, M, S);
      Hour := H;
      Min := M;
      Sec := S;
    end;

    {Convert to packed format}
    PackTime(DT, Ptime);
    apYMTimeStampToPack := Ptime;
  end;

  function AbstractProtocol.apCurrentTimeStamp : LongInt;
    {-Return a Ymodem format file time stamp of the current date/time}
  var
    Ptime : LongInt;
    DT : DateTime;
    Sec100, DOW : Word;
  begin
    with DT do begin
      GetTime(Hour, Min, Sec, Sec100);
      GetDate(Year, Month, Day, DOW);
    end;
    PackTime(DT, Ptime);
    apCurrentTimeStamp := apPackToYMTimeStamp(Ptime);
  end;

  procedure AbstractProtocol.apAbortProtocol;
    {-Aborts the protocol}
  var
    SaveStatus : Word;
  begin
    SaveStatus := AsyncStatus;
    apCancel;
    APort^.GotError(epFatal+ecCancelRequested);
    apShowLastStatus;
    if SaveStatus <> ecUserAbort then
      AsyncStatus := SaveStatus;
  end;

  function AbstractProtocol.apHandleAbort : Boolean;
    {-Call user abort function, abort and return True if user aborts}
  begin
    if APort^.UserAbort then begin
      AsyncStatus := ecUserAbort;
      apAbortProtocol;
      apHandleAbort := True;
    end else
      apHandleAbort := False;
  end;

  function AbstractProtocol.apDrainOutput(CanCnt : Byte) : Boolean;
    {-Drain output, updating status and checking for aborts (False to abort)}
  var
    C : Char;
    Check1, Check2 : Word;
    I : Byte;
  begin
    {Assume abort}
    apDrainOutput := False;

    Check1 := APort^.OutBuffUsed;
    while APort^.OutBuffUsed > 0 do begin
      Check2 := APort^.OutBuffUsed;
      if (Check1 - Check2) > BlockLen then begin
        {Update elapsed tics and show status}
        ElapsedTics := ElapsedTime(Timer);
        apUserStatus(False, False);

        {Check for local abort}
        if apHandleAbort then begin
          {Manually lower BytesTransferred for last status}
          BytesTransferred := GetBytesTransferred;
          Exit;
        end;

        {Check for remote abort (discard all but cCan chars)}
        I := 0;
        while APort^.InBuffUsed > 0 do begin
          APort^.GetChar(C);
          if C = cCan then begin
            Inc(I);
            if I = CanCnt then begin
              APort^.FlushOutBuffer;
              APort^.GotError(epFatal+ecCancelRequested);
              Exit;
            end
          end else
            I := 0;
        end;

        {Prepare for next check}
        Check1 := Check2;
      end;
    end;

    {No abort if we get here}
    apDrainOutput := True;
  end;

  function AbstractProtocol.apWaitForFreeSpace(W, T : Word) :  Boolean;
    {Wait until buffer has W free bytes}
  var
    ET : EventTimer;
  begin
    apWaitForFreeSpace := True;
    NewTimer(ET, T);
    while (APort^.OutBuffFree < W) and
           not APort^.ptWaitComplete(ET) do ;

    {Check for user abort during ptWaitComplete}
    if AsyncStatus <> ecOk then
      apWaitForFreeSpace := False;
  end;

  function AbstractProtocol.apCrc32OfFile(FName : PathStr; Len : Longint) : LongInt;
    {-Returns Crc32 of FName}
  const
    BufSize = 8192;
  type
    BufArray = array[1..BufSize] of Byte;
  var
    BytesRead, I : NumReadType;
    ResultTmp : Word;
    F : File;
    Buffer : ^BufArray;
    FileLoc : LongInt;
  label
    ExitPoint;
  begin
    AsyncStatus := ecOk;

    {If Len is zero then check the entire file}
    if Len = 0 then
      Len := MaxLongint;

    {Get a buffer}
    if not GetMemCheck(Buffer, BufSize) then begin
      APort^.GotError(epNonFatal+ecOutOfMemory);
      Exit;
    end;

    {Open the file}
    SaveMode := FileMode;                                              {!!.02}
    FileMode := AproFileMode;                                   {!!.02}{!!.03}
    Assign(F, FName);
    {$i-}
    Reset(F, 1);
    FileMode := SaveMode;                                              {!!.02}
    ResultTmp := IOResult;
    if ResultTmp <> 0 then begin
      APort^.GotError(epNonFatal+ResultTmp);
      goto ExitPoint;
    end;

    {Initialize Crc}
    BlockCheck := $FFFFFFFF;

    {Start at beginning, loop thru file calculating Crc32}
    FileLoc := 0;
    repeat
      BlockRead(F , Buffer^, BufSize, BytesRead);
      ResultTmp := IOResult;
      if ResultTmp = 0 then begin
        if Len <> MaxLongint then begin
          Inc(FileLoc, BytesRead);
          if FileLoc > Len then
            BytesRead := BytesRead - (FileLoc - Len);
        end;
        for I := 1 to BytesRead do
          apUpdateBlockCheck(Buffer^[I]);
      end;
    until (BytesRead = 0) or (ResultTmp <> 0) or (FileLoc >= Len);

    Close(F);
    if IOResult = 0 then ;

ExitPoint:
    apCrc32OfFile := BlockCheck;
    FreeMemCheck(Buffer, BufSize);
  end;

  procedure AbstractProtocol.apUserStatus(Starting, Ending : Boolean);
    {-Calls user status routine while preserving current AsyncStatus}
  var
    SaveStatus : Word;
  begin
    SaveStatus := AsyncStatus;
    if AsyncStatus = ecNoHeader then
      AsyncStatus := ecOk;
    UserStatus(@Self, Starting, Ending);
    AsyncStatus := SaveStatus;
  end;

  {$F+}
  procedure NoStatus(AP : AbstractProtocolPtr;
                     Starting, Ending : Boolean);
    {-Empty show status procedure}
  begin
  end;

  function NoNextFile(AP : AbstractProtocolPtr) : Boolean;
    {-Empty next file function -- always returns False}
  begin
    NoNextFile := False;
  end;

  procedure NoLogFile(AP : AbstractProtocolPtr; LogFileStatus : LogFileType);
    {-Empty LogFile procedure}
  begin
  end;

  function NoAcceptFile(AP : AbstractProtocolPtr) : Boolean;
    {-Empty AcceptFile function}
  begin
    NoAcceptFile := True;
  end;

  procedure NoUserBack(AP : AbstractProtocolPtr);
    {-Empty UserBack procedure}
  begin
  end;

  function AcceptOneFile(AP : AbstractProtocolPtr) : Boolean;
    {-Built-in function that accepts one file only}
  begin
    with AP^ do begin
      AcceptOneFile := not GotOneFile;
      GotOneFile := True;
    end;
  end;

  function NextFileMask(AP : AbstractProtocolPtr;
                        var FName : PathStr) : Boolean;
    {-Built-in function that works with file mask fields}
  const
    AnyFileButDir = AnyFile and not (Directory or VolumeID);
  begin
    AsyncStatus := 0;
    with AP^ do begin
      {Check for uninitialized search mask}
      if SearchMask = '' then begin
        APort^.GotError(epFatal+ecNoSearchMask);
        NextFileMask := False;
        Exit;
      end;

      {Search for a matching file}
      if FindingFirst then begin
        FindFirst(SearchMask, AnyFileButDir, CurRec);

        if DosError = 18 then begin
          APort^.GotError(epFatal+ecNoMatchingFiles);
          FName := '';
          NextFileMask := False;
          Exit;
        end else
          FindingFirst := False;
      end else
        FindNext(CurRec);

      {Check for errors}
      if DosError <> 0 then begin
        {Failed to find file, return error status}
        case DosError of
          3  : APort^.GotError(epFatal+ecDirNotFound);
        end;
        FName := '';
        NextFileMask := False;
      end else begin
        {Found a file, return fully qualified file name}
        FName := AddBackSlash(JustPathName(SearchMask)) + CurRec.Name;
        NextFileMask := True;
      end;
    end;
  end;

  function NextFileList(AP : AbstractProtocolPtr;
                        var FName : PathStr) : Boolean;
    {-Built-in function that works with a list of files}
  const
    Separator = ';';
    EndOfListMark = #0;
    MaxLen = SizeOf(PathStr);
  var
    MaxNext : Word;
    I : Word;
    Len : Word;
  begin
    AsyncStatus := 0;

    with AP^ do begin
      {Return immediately if no more files}
      if FileList^[FileListIndex] = EndOfListMark then begin
        NextFileList := False;
        FName := '';
        Exit;
      end;

      {Increment past the last separator}
      if FileListIndex <> 0 then
        Inc(FileListIndex);

      {Define how far to look for the next marker}
      if LongInt(FileListIndex) + MaxLen > 65535 then
        MaxNext := 65535
      else
        MaxNext := FileListIndex + MaxLen;

      {Look for the next marker}
      for I := FileListIndex to MaxNext do begin
        if (FileList^[I] = Separator) or
           (FileList^[I] = EndOfListMark) then begin
          {Extract the pathname}
          Len := I - FileListIndex;
          Move(FileList^[FileListIndex], FName[1], Len);
          FName[0] := Char(Len);
          NextFileList := True;
          Inc(FileListIndex, Len);
          Exit;
        end;
      end;

      {Bad format list (no separator) -- show error}
      APort^.GotError(epFatal+ecBadFileList);
      NextFileList := False;
      FName := '';
    end;
  end;
  {$F-}

  {$IFDEF UseStreams}
  procedure AbstractProtocolStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing protocol objects}
  begin
    SPtr^.RegisterType(otAbstractProtocol, veAbstractProtocol,
                       TypeOf(AbstractProtocol),
                       @AbstractProtocol.Store, @AbstractProtocol.Load);
    SPtr^.RegisterOldVersion(otAbstractProtocol, 00,
                       TypeOf(AbstractProtocol),
                       @AbstractProtocol.Load00);
  end;
  {$ENDIF}

begin
  {Set StartDate to 1/1/1970 00:00 GMT}
  with StartDate do begin
    T := 0;
    D := DMYToDate(1, 1, 1970);
  end;
end.
