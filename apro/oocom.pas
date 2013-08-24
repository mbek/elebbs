{$S-,R-,V-,I-,B-,F+,O+,A-}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{Include OPro's define file if UseOPro is specified}
{$IFDEF UseOPro}
{$I OPDEFINE.INC}
{$ENDIF}

{*********************************************************}
{*                  OOCOM.PAS 2.03                       *}
{*     Copyright (c) TurboPower Software 1991.           *}
{* Portions copyright (c) Information Technology 1989,   *}
{*    and used under license to TurboPower Software      *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OOCom;

interface

uses
  {$IFDEF UseOPro}
  OpRoot,
  OpInline,
  {$ENDIF}
  {$IFDEF UseTPro}
  TpInline,
  TpMemChk,
  {$ENDIF}

  Multi,
  ApMisc,
  ApPort,
  ApTimer

  {$IFDEF UseUart}
  ,ApUart
  {$ENDIF}
  {$IFDEF UseInt14}
  ,ApInt14
  {$ENDIF}
  {$IFDEF UseFossil}
  ,ApFossil
  {$ENDIF}
  {$IFDEF UseDigi14}
  ,ApDigi14
  {$ENDIF};

type
  AbstractPortPtr = ^AbstractPort;

  {Procedure for handling chars during WaitForChar/String}
  WaitCharProc = procedure(APPtr : AbstractPortPtr; C : Char);

  AbstractPort = object(Root)
    PR          : PortRecPtr;        {Pointer to port record}
    ComPortName : ComNameType;       {Port name}
    WaitChar    : WaitCharProc;      {Handles chars during Waits}
    UserData    : LongInt;           {Reserved for user data storage}

    constructor Init;
      {-Low level initializations of AbstractPort}
    destructor Done; virtual;
      {-Closes the com port}

    {---- Low level hooks ----}
    procedure SetLine(Baud : LongInt; Parity : ParityType;
                      DataBits : DataBitType;
                      StopBits : StopBitType); virtual;
      {-Calls device-level SetLine}
    procedure GetLine(var Baud : LongInt; var Parity : ParityType;
                      var DataBits : DataBitType;
                      var StopBits : StopBitType;
                      FromHardware : Boolean); virtual;
      {-Calls device-level GetLine}
    procedure SetModem(DTR, RTS : Boolean); virtual;
      {-Calls device-level SetModem}
    procedure GetModem(var DTR, RTS : Boolean); virtual;
      {-Calls device-level GetModem}
    procedure GetChar(var C : Char); virtual;
      {-Calls device-level GetChar}
    procedure PeekChar(var C : Char; PeekAhead : Word); virtual;
      {-Calls device-level PeekChar}
    procedure PutChar(C : Char); virtual;
      {-Calls device-level PutChar}
    procedure StartTransmitter; virtual;
      {-Calls device-level StartTransmitter}
    function CharReady : Boolean; virtual;
      {-Calls device-level CharReady}
    function TransReady : Boolean; virtual;
      {-Calls device-level TransReady}
    procedure SendBreak; virtual;
      {-Calls device-level SendBreak}
    procedure ActivatePort(Restore : Boolean); virtual;
      {-Calls device-level ActivatePort}
    procedure DeactivatePort(Restore : Boolean); virtual;
      {-Calls device-level DeactivatePort}
    procedure SavePort(var PSR); virtual;
      {-Saves line, modem and interrupt state of the port (returns PSR)}
    procedure RestorePort(var PSR); virtual;
      {-Restores line, modem and interrupt state from PSR}
    procedure GotError(StatusCode : Word); virtual;
      {-Calls device-level GotError}

    function UpdateLineStatus : Byte; virtual;
      {-Returns line status register value}
    function UpdateModemStatus : Byte; virtual;
      {-Returns modem status register value}
    {$IFDEF UseHWFlow}
    procedure HWFlowSet(Enable : Boolean;
                        BufferFull, BufferResume : Word;
                        Options : Word); virtual;
      {-Enables/disables hardware flow control}
    function HWFlowGet : FlowState; virtual;
      {-Returns hardware flow control state}
    {$ENDIF}
    {$IFDEF UseSWFlow}
    procedure SWFlowSet(Enable : Boolean;
                        BufferFull, BufferResume : Word;
                        Options : Word); virtual;
      {-Enables/disables software flow control}
    function SWFlowGet : FlowState; virtual;
      {-Returns software flow control state}
    procedure SWFlowCtl(OnChar, OffChar : Char; Resume : Boolean); virtual;
      {-Sets software flow control characters and/or resumes transmits}
    {$ENDIF}
    procedure BufferStatus(var InFree, OutFree, InUsed, OutUsed : Word); virtual;
      {-Returns various buffer values}
    procedure BufferFlush(FlushIn, FlushOut: Boolean); virtual;
      {-Flushes input/output buffers}

    {---- Open/change routines ----}
    procedure ChangeBaud(NewBaud : LongInt);
      {-Changes baud rate of P to NewBaud}
    procedure ChangeParity(NewParity : ParityType);
      {-Changes parity of P to NewParity}
    procedure ChangeDataBits(NewDataBits : DataBitType);
      {-Changes data bits of P to NewDataBits}
    procedure ChangeStopBits(NewStopBits : StopBitType);
      {-Changes stop bit of P to NewStopBits}
    procedure ChangeBufferSizes(NewInSize, NewOutSize : Word);
      {-Changes input/output buffer sizes (0 means no change)}
    procedure ForceBufferLimits(NewInLimit, NewOutLimit: Word);
      {-Forces new buffer limits (_no_ error checking)}

    {---- Get routines -----}
    function BlockReady(ExpectedLen : Word; DelimSet : CharSet) : Boolean;
      {-Returns True if a string is ready of ExpectedLen or with DelimSet}
    function GetDelimLoc(DelimSet : CharSet) : Word;
      {-Returns the location in the input buffer of DelimSet}
    procedure GetCharTimeout(var C : Char; Timeout : Word);
      {-Waits for C or Timeout}
    procedure PeekCharTimeout(var C : Char; PeekAhead : Word; Timeout : Word);
      {-Waits for C at PeekAhead or Timeout}
    procedure GetString(var S : String; ExpectedLen : Byte;
                        DelimSet : CharSet); virtual;
      {-Returns string S with Len or ending with DelimSet}
    procedure GetStringTimeout(var S : String; ExpectedLen : Byte;
                               DelimSet : CharSet; Timeout : Word);
      {-Waits for string S or timeoout}
    procedure GetBlock(var Block; ExpectedLen : Word;
                       var ReceivedLen : Word; DelimSet : CharSet); virtual;
      {-Returns block of ExpectedLen or delimited by DelimSet}
    procedure GetBlockDirect(var Block; ExpectedLen : Word;
                             var ReceivedLen : Word; DelimSet : CharSet); virtual;
      {-Returns block of ExpectedLen}
    procedure GetBlockTimeout(var Block; ExpectedLen : Word;
                              var ReceivedLen : Word; DelimSet : CharSet;
                              Timeout : Word);
      {-Waits for block or Timeout}

    {---- Put routines ---}
    procedure PutCharTimeout(C : Char; Timeout : Word);
      {-Puts char in output buffer or Timeout}
    procedure PutString(S : String); virtual;
      {-Puts string S in output buffer}
    procedure PutStringTimeout(S : String; Timeout : Word); virtual;
      {-Puts string S in output buffer}
    procedure PutBlock(var Block; BlockLen : Word;
                       var BytesWritten : Word); virtual;
      {-Puts block of BlockLen}
    procedure PutBlockDirect(var Block; BlockLen : Word;
                             var BytesWritten : Word); virtual;
      {-Puts block of BlockLen directly into the output buffer}
    procedure PutBlockTimeout(var Block; BlockLen : Word;
                              var BytesWritten : Word; Timeout : Word);
      {-Puts a block or Timeout}

    {----- Buffer management -----}
    procedure FlushInBuffer; virtual;
      {-Erases input buffer}
    procedure FlushOutBuffer; virtual;
      {-Erases output buffer}
    function InBuffUsed : Word; virtual;
      {-Returns number of chars in input buffer}
    function OutBuffUsed : Word; virtual;
      {-Returns number of chars in output buffer}
    function InBuffFree : Word; virtual;
      {-Returns free space in input buffer}
    function OutBuffFree : Word; virtual;
      {-Returns free space in output buffer}
    procedure DrainOutBuffer(Timeout : Word); virtual;
      {-Delays until output buffer drained or Timeout}

    {$IFDEF StatusBuffering}
    procedure EnableStatusBuffer;
      {-Enables input status buffer}
    procedure DisableStatusBuffer;
      {-Disables input status buffer}
    function StatusBuffering : Boolean;
      {-Return state of status buffering}
    {$ENDIF}

    {$IFDEF UseSWFlow}
    {-----Software flow control-----}
    procedure SWFlowEnable(BufferFull, BufferResume : Word); virtual;
      {-Enables automatic xon/xoff flow control}
    procedure SWFlowEnableOpt(BufferFull, BufferResume : Word;
                              Opt : Word); virtual;
      {-Enables automatic xon/xoff flow control with options}
    procedure SWFlowDisable; virtual;
      {-Disables automatic xon/xoff flow control}
    function SWFlowState : FlowState; virtual;
      {-Returns current state of software flow control}
    procedure SWFlowResume; virtual;
      {-Forces transmits to resume even if currently blocked by Xoff}
    procedure SWFlowSetChars(OnChar, OffChar : Char); virtual;
      {-Sets custom xon/xoff char}
    {$ENDIF}

    {$IFDEF UseHWFlow}
    {---- Hardware flow control ----}
    procedure HWFlowEnable(BufferFull, BufferResume : Word;
                           Options : Word); virtual;
      {-Enable hardware flow control (DTR and/or RTS)}
    procedure HWFlowDisable; virtual;
      {-Disable automatic hardware handshaking}
    function HWFlowState : FlowState; virtual;
      {-Returns state of AutoHandshaking}
    {$ENDIF}
    procedure SetDTR(State : Boolean);
      {-Raise/lower DTR}
    procedure SetRTS(State : Boolean);
      {-Raise/lower RTS}

    {---- Modem status ----}
    function GetModemControl : Byte; virtual;
      {-Return the modem control byte from the port record}
    function GetModemStatusPrim(ClearMask : Byte) : Byte; virtual;
      {-Primitive to return modem status byte and clear selected bits}
    function GetModemStatus : Byte;
      {-Return the modem status byte from the port record}
    function CheckCTS : Boolean;
      {-Returns True if CTS is high}
    function CheckDSR : Boolean;
      {-Returns True if DSR is high}
    function CheckRI : Boolean;
      {-Returns True if RI is high}
    function CheckDCD : Boolean;
      {-Returns True if DCD is high}
    function CheckDeltaCTS : Boolean;
      {-Returns True if DeltaCTS is high}
    function CheckDeltaDSR : Boolean;
      {-Returns True if DeltaDSR is high}
    function CheckDeltaRI : Boolean;
      {-Returns True if DeltaRI is high}
    function CheckDeltaDCD : Boolean;
      {-Returns True if DeltaDCD is high}

    {--- Line Status ---}
    function GetLineControl : Byte; virtual;
      {-Return the line control byte from the port record}
    function GetLineStatus : Byte; virtual;
      {-Return the line status byte from the port record}
    function CheckDataReady : Boolean;
      {-Returns True if DR (Data Ready) is high}
    function CheckLineError : Boolean;
      {-Returns True if any error bits are set in line status register}
    function GetLineError : Word;
      {-Returns line error type (here and in AsyncStatus)}
    function CheckLineBreak : Boolean;
      {-Returns True if Break was received (BI bit is high)}
    function CheckTHRE : Boolean;
      {-Returns True if transmitter holding register is empty}
    function CheckTE : Boolean;
      {-Returns True if transmitter is empty}
    function CheckFifoError : Boolean;
      {-Returns if Fifo error bit is set}

    {---- Miscellaneous ----}
    function GetComName : ComNameType;
      {-Returns the ComName of this port}
    function GetBaseAddr : Word;
      {-Returns the BaseAddr of this port (zero if NA)}
    procedure SetErrorProc(EP : AsyncErrorProc);
      {-Sets an error handler for ComPort P}
    procedure ptOptionsOn(OptionFlags : Word);
      {-Activate multiple options}
    procedure ptOptionsOff(OptionFlags : Word);
      {-Deactivate multiple options}
    function ptOptionsAreOn(OptionFlags : Word) : Boolean;
      {-Return True if all specified options are on}
    procedure WaitForChar(DelimSet : CharSet; var C : Char; Timeout : Word);
      {-Waits for char in DelimSet or Timeout}
    procedure WaitForString(S : String; Timeout : Word);
      {-Waits for string S or Timeout}
    procedure WaitForMultiString(SL : String; SepChar : Char;
                                 var FoundS : String;
                                 var FoundI : Byte;
                                 Timeout : Word);
      {-Waits for any substring in SL or Timeout}
    procedure SetWaitCharProc(WCP : WaitCharProc);
      {-Sets procedure to get chars during WaitForChar/String}
    procedure SetAbortFunc(AFunc : AbortFunc);
      {-Sets AFunc as the user abort function}
    function UserAbort : Boolean; virtual;
      {-Calls the user abort function}
    function ProtocolInProgress : Boolean;
      {-Returns True if this port is currently processing a protocol}
    function FaxInProgress : Boolean;
      {-Returns True if this port is currently processing a protocol}

    {#Z+}
    {$IFDEF UseStreams}
    constructor Load(var S : IdStream);
      {-Abstract Load for a port object}
    procedure Store(var S : IdStream);
      {-Abstract Store for a port object}
    {$ENDIF}

    {---- Internal methods -------}
    procedure ptAllcaseCharSet(var CS : CharSet);
      {-Make CS a non case sensitive set}
    {function ptUserAbort : Boolean; virtual;}                         {!!.02}
      {-User function -- calls the user specified abort function}
    function ptWaitComplete(ET : EventTimer) : Boolean;
      {-Returns True if ET expired or UserAbort returns True}
    {#Z-}
  end;

  {$IFDEF UseUart}
  UartPortPtr = ^UartPort;
  UartPort = object(AbstractPort)
    constructor InitFast(ComName : ComNameType; NewBaud : LongInt);
      {-Opens ComName with default line options}
    constructor InitKeep(ComName : ComNameType; InSize, OutSize : Word);
      {-Opens ComName (without changing line options)}
    constructor InitCustom(ComName : ComNameType; Baud : LongInt;
                           Parity : ParityType; DataBits : DataBitType;
                           StopBits : StopBitType;
                           InSize, OutSize : Word;
                           Options : Word);
      {-Opens the ComName com port}

    {#Z+}
    destructor Done; virtual;
      {-Closes the com port}

    {---- Low level hooks ----}
    procedure SetLine(Baud : LongInt; Parity : ParityType;
                      DataBits : DataBitType;
                      StopBits : StopBitType); virtual;
      {-Calls device-level SetLine}
    procedure GetLine(var Baud : LongInt; var Parity : ParityType;
                      var DataBits : DataBitType;
                      var StopBits : StopBitType;
                      FromHardware : Boolean); virtual;
      {-Calls device-level GetLine}
    procedure SetModem(DTR, RTS : Boolean); virtual;
      {-Calls device-level SetMode}
    procedure GetModem(var DTR, RTS : Boolean); virtual;
      {-Calls device-level GetMode}
    procedure GetChar(var C : Char); virtual;
      {-Calls device-level GetChar}
    procedure PeekChar(var C : Char; PeekAhead : Word); virtual;
      {-Calls device-level PeekChar}
    procedure PutChar(C : Char); virtual;
      {-Calls device-level PutChar}
    procedure StartTransmitter; virtual;
      {-(Re)starts the transmit stream}
    function CharReady : Boolean; virtual;
      {-Returns True if at least one character has been received}
    function TransReady : Boolean; virtual;
      {-Returns True if it's ok to transmit one character}
    procedure SendBreak; virtual;
      {-Calls device-level SendBreak}
    procedure ActivatePort(Restore : Boolean); virtual;
      {-Calls device-level ActivatePort}
    procedure DeactivatePort(Restore : Boolean); virtual;
      {-Calls device-level DeactivatePort}
    procedure SavePort(var PSR); virtual;
      {-Calls device-level SavePort}
    procedure RestorePort(var PSR); virtual;
      {-Calls device-level RestorePort}
    procedure GotError(StatusCode : Word); virtual;
      {-Calls device-level GotError}

    function UpdateLineStatus : Byte; virtual;
      {-Returns line status register value}
    function UpdateModemStatus : Byte; virtual;
      {-Returns modem status register value}
    {$IFDEF UseHWFlow}
    procedure HWFlowSet(Enable : Boolean;
                        BufferFull, BufferResume : Word;
                        Options : Word); virtual;
      {-Enables/disables hardware flow control}
    function HWFlowGet : FlowState; virtual;
      {-Returns hardware flow control state}
    {$ENDIF}
    {$IFDEF UseSWFlow}
    procedure SWFlowSet(Enable : Boolean;
                        BufferFull, BufferResume : Word;
                        Options : Word); virtual;
      {-Enables/disables software flow control}
    function SWFlowGet : FlowState; virtual;
      {-Returns software flow control state}
    procedure SWFlowCtl(OnChar, OffChar : Char; Resume : Boolean); virtual;
      {-Sets software flow control characters and/or resumes transmits}
    {$ENDIF}
    procedure BufferStatus(var InFree, OutFree, InUsed, OutUsed : Word); virtual;
      {-Returns various buffer values}
    procedure BufferFlush(FlushIn, FlushOut: Boolean); virtual;
      {-Flushes input/output buffers}

    {$IFDEF UseStreams}
    constructor Load(var S : IdStream);
      {-Load a UartPort from a stream}
    procedure Store(var S : IdStream);
      {-Store a UartPort in a stream}
    {$ENDIF}

    {#Z-}
  end;
  {$ENDIF}

  {$IFDEF UseInt14}
  Int14PortPtr = ^Int14Port;
  Int14Port = object(AbstractPort)
    constructor InitFast(ComName : ComNameType; NewBaud : LongInt);
      {-Opens ComName with default line options}
    constructor InitCustom(ComName : ComNameType; Baud : LongInt;
                           Parity : ParityType; DataBits : DataBitType;
                           StopBits : StopBitType;
                           InSize, OutSize : Word;
                           Options : Word);
      {-Opens the ComName com port}
    constructor InitKeep(ComName : ComNameType; InSize, OutSize : Word);
      {-Opens ComName (without changing line options)}

    {#Z+}
    destructor Done; virtual;
      {-Closes the com port}

    {---- Low level hooks ----}
    procedure SetLine(Baud : LongInt; Parity : ParityType;
                      DataBits : DataBitType;
                      StopBits : StopBitType); virtual;
      {-Calls device-level SetLine}
    procedure GetLine(var Baud : LongInt; var Parity : ParityType;
                      var DataBits : DataBitType;
                      var StopBits : StopBitType;
                      FromHardware : Boolean); virtual;
      {-Calls device-level GetLine}
    procedure SetModem(DTR, RTS : Boolean); virtual;
      {-Calls device-level SetModem}
    procedure GetModem(var DTR, RTS : Boolean); virtual;
      {-Calls device-level GetModem}
    procedure GetChar(var C : Char); virtual;
      {-Calls device-level GetChar}
    procedure PeekChar(var C : Char; PeekAhead : Word); virtual;
      {-Calls device-level PeekChar}
    procedure PutChar(C : Char); virtual;
      {-Calls device-level PutChar}
    procedure StartTransmitter; virtual;
      {-(Re)starts the transmit stream}
    function CharReady : Boolean; virtual;
      {-Returns True if at least one character has been received}
    function TransReady : Boolean; virtual;
      {-Returns True if it's ok to transmit one character}
    procedure SendBreak; virtual;
      {-Calls device-level SendBreak}
    procedure ActivatePort(Restore : Boolean); virtual;
      {-Calls device-level ActivatePort}
    procedure DeactivatePort(Restore : Boolean); virtual;
      {-Calls device-level DeactivatePort}
    procedure SavePort(var PSR); virtual;
      {-Calls device-level SavePort}
    procedure RestorePort(var PSR); virtual;
      {-Calls device-level RestorePort}
    procedure GotError(StatusCode : Word); virtual;
      {-Calls device-level GotError}

    function UpdateLineStatus : Byte; virtual;
      {-Returns line status register value}
    function UpdateModemStatus : Byte; virtual;
      {-Returns modem status register value}
    {$IFDEF UseHWFlow}
    procedure HWFlowSet(Enable : Boolean;
                        BufferFull, BufferResume : Word;
                        Options : Word); virtual;
      {-Enables/disables hardware flow control}
    function HWFlowGet : FlowState; virtual;
      {-Returns hardware flow control state}
    {$ENDIF}
    {$IFDEF UseSWFlow}
    procedure SWFlowSet(Enable : Boolean;
                        BufferFull, BufferResume : Word;
                        Options : Word); virtual;
      {-Enables/disables software flow control}
    function SWFlowGet : FlowState; virtual;
      {-Returns software flow control state}
    procedure SWFlowCtl(OnChar, OffChar : Char; Resume : Boolean); virtual;
      {-Sets software flow control characters and/or resumes transmits}
    {$ENDIF}
    procedure BufferStatus(var InFree, OutFree, InUsed, OutUsed : Word); virtual;
      {-Returns various buffer values}
    procedure BufferFlush(FlushIn, FlushOut: Boolean); virtual;
      {-Flushes input/output buffers}

    {$IFDEF UseStreams}
    constructor Load(var S : IdStream);
      {-Load an Int14Port from a stream}
    procedure Store(var S : IdStream);
      {-Store an Int14Port in a stream}
    {$ENDIF}
    {#Z-}
  end;
  {$ENDIF}

  {$IFDEF UseFossil}
  FossilPortPtr = ^FossilPort;
  FossilPort = object(AbstractPort)
    constructor InitFast(ComName : ComNameType; NewBaud : LongInt);
      {-Opens ComName with default line options}
    constructor InitKeep(ComName : ComNameType; InSize, OutSize : Word);
      {-Opens ComName (without changing line options)}
    constructor InitCustom(ComName : ComNameType; Baud : LongInt;
                           Parity : ParityType; DataBits : DataBitType;
                           StopBits : StopBitType;
                           InSize, OutSize : Word;
                           Options : Word);
      {-Opens the ComName com port}

    {#Z+}
    destructor Done; virtual;
      {-Closes the com port}

    {---- Low level hooks ----}
    procedure SetLine(Baud : LongInt; Parity : ParityType;
                      DataBits : DataBitType;
                      StopBits : StopBitType); virtual;
      {-Calls device-level SetLine}
    procedure GetLine(var Baud : LongInt; var Parity : ParityType;
                      var DataBits : DataBitType;
                      var StopBits : StopBitType;
                      FromHardware : Boolean); virtual;
      {-Calls device-level GetLine}
    procedure SetModem(DTR, RTS : Boolean); virtual;
      {-Calls device-level SetMode}
    procedure GetModem(var DTR, RTS : Boolean); virtual;
      {-Calls device-level GetMode}
    procedure GetChar(var C : Char); virtual;
      {-Calls device-level GetChar}
    procedure PeekChar(var C : Char; PeekAhead : Word); virtual;
      {-Calls device-level PeekChar}
    procedure PutChar(C : Char); virtual;
      {-Calls device-level PutChar}
    procedure PutBlock(var Block; BlockLen : Word;
                       var BytesWritten : Word); virtual;
    procedure StartTransmitter; virtual;
      {-(Re)starts the transmit stream}
    function CharReady : Boolean; virtual;
      {-Returns True if at least one character has been received}
    function TransReady : Boolean; virtual;
      {-Returns True if it's ok to transmit one character}
    procedure SendBreak; virtual;
      {-Calls device-level SendBreak}
    procedure ActivatePort(Restore : Boolean); virtual;
      {-Calls device-level ActivatePort}
    procedure DeactivatePort(Restore : Boolean); virtual;
      {-Calls device-level DeactivatePort}
    procedure SavePort(var PSR); virtual;
      {-Calls device-level SavePort}
    procedure RestorePort(var PSR); virtual;
      {-Calls device-level RestorePort}
    procedure GotError(StatusCode : Word); virtual;
      {-Calls device-level GotError}
    function UpdateLineStatus : Byte; virtual;
      {-Returns line status register value}
    function UpdateModemStatus : Byte; virtual;
      {-Returns modem status register value}
    {$IFDEF UseHWFlow}
    procedure HWFlowSet(Enable : Boolean;
                        BufferFull, BufferResume : Word;
                        Options : Word); virtual;
      {-Enables/disables hardware flow control}
    function HWFlowGet : FlowState; virtual;
      {-Returns hardware flow control state}
    {$ENDIF}
    {$IFDEF UseSWFlow}
    procedure SWFlowSet(Enable : Boolean;
                        BufferFull, BufferResume : Word;
                        Options : Word); virtual;
      {-Enables/disables software flow control}
    function SWFlowGet : FlowState; virtual;
      {-Returns software flow control state}
    procedure SWFlowCtl(OnChar, OffChar : Char; Resume : Boolean); virtual;
      {-Sets software flow control characters and/or resumes transmits}
    {$ENDIF}
    procedure BufferStatus(var InFree, OutFree, InUsed, OutUsed : Word); virtual;
      {-Returns various buffer values}
    procedure BufferFlush(FlushIn, FlushOut: Boolean); virtual;
      {-Flushes input/output buffers}
    {$IFDEF UseStreams}
    constructor Load(var S : IdStream);
      {-Load a FossilPort from a stream}
    procedure Store(var S : IdStream);
      {-Store a FossilPort in a stream}
    {$ENDIF}

    {#Z-}
  end;
  {$ENDIF}

  {$IFDEF UseDigi14}
  Digi14PortPtr = ^Digi14Port;
  Digi14Port = object(AbstractPort)
    constructor InitFast(ComName : ComNameType; NewBaud : LongInt);
      {-Opens ComName with default line options}
    constructor InitKeep(ComName : ComNameType; InSize, OutSize : Word);
      {-Opens ComName (without changing line options)}
    constructor InitCustom(ComName : ComNameType; Baud : LongInt;
                           Parity : ParityType; DataBits : DataBitType;
                           StopBits : StopBitType;
                           InSize, OutSize : Word;
                           Options : Word);
      {-Opens the ComName com port}

    {#Z+}
    destructor Done; virtual;
      {-Closes the com port}

    {---- Low level hooks ----}
    procedure SetLine(Baud : LongInt; Parity : ParityType;
                      DataBits : DataBitType;
                      StopBits : StopBitType); virtual;
      {-Calls device-level SetLine}
    procedure GetLine(var Baud : LongInt; var Parity : ParityType;
                      var DataBits : DataBitType;
                      var StopBits : StopBitType;
                      FromHardware : Boolean); virtual;
      {-Calls device-level GetLine}
    procedure SetModem(DTR, RTS : Boolean); virtual;
      {-Calls device-level SetMode}
    procedure GetModem(var DTR, RTS : Boolean); virtual;
      {-Calls device-level GetMode}
    procedure GetChar(var C : Char); virtual;
      {-Calls device-level GetChar}
    procedure PeekChar(var C : Char; PeekAhead : Word); virtual;
      {-Calls device-level PeekChar}
    procedure PutChar(C : Char); virtual;
      {-Calls device-level PutChar}
    procedure StartTransmitter; virtual;
      {-(Re)starts the transmit stream}
    function CharReady : Boolean; virtual;
      {-Returns True if at least one character has been received}
    function TransReady : Boolean; virtual;
      {-Returns True if it's ok to transmit one character}
    procedure SendBreak; virtual;
      {-Calls device-level SendBreak}
    procedure ActivatePort(Restore : Boolean); virtual;
      {-Calls device-level ActivatePort}
    procedure DeactivatePort(Restore : Boolean); virtual;
      {-Calls device-level DeactivatePort}
    procedure SavePort(var PSR); virtual;
      {-Calls device-level SavePort}
    procedure RestorePort(var PSR); virtual;
      {-Calls device-level RestorePort}
    procedure GotError(StatusCode : Word); virtual;
      {-Calls device-level GotError}
    function UpdateLineStatus : Byte; virtual;
      {-Returns line status register value}
    function UpdateModemStatus : Byte; virtual;
      {-Returns modem status register value}
    {$IFDEF UseHWFlow}
    procedure HWFlowSet(Enable : Boolean;
                        BufferFull, BufferResume : Word;
                        Options : Word); virtual;
      {-Enables/disables hardware flow control}
    function HWFlowGet : FlowState; virtual;
      {-Returns hardware flow control state}
    {$ENDIF}
    {$IFDEF UseSWFlow}
    procedure SWFlowSet(Enable : Boolean;
                        BufferFull, BufferResume : Word;
                        Options : Word); virtual;
      {-Enables/disables software flow control}
    function SWFlowGet : FlowState; virtual;
      {-Returns software flow control state}
    procedure SWFlowCtl(OnChar, OffChar : Char; Resume : Boolean); virtual;
      {-Sets software flow control characters and/or resumes transmits}
    {$ENDIF}
    procedure BufferStatus(var InFree, OutFree, InUsed, OutUsed : Word); virtual;
      {-Returns various buffer values}
    procedure BufferFlush(FlushIn, FlushOut: Boolean); virtual;
      {-Flushes input/output buffers}
    {$IFDEF UseStreams}
    constructor Load(var S : IdStream);
      {-Load a Digi14Port from a stream}
    procedure Store(var S : IdStream);
      {-Store a Digi14Port in a stream}
    {$ENDIF}
    {#Z-}
  end;
  {$ENDIF}

  procedure NoWaitChar(APPtr : AbstractPortPtr; C : Char);
    {-Empty wait char procedure}

  {$IFDEF UseStreams}
  {$IFDEF UseUart}
  procedure UartPortStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing UartPort objects}
  {$ENDIF}
  {$IFDEF UseInt14}
  procedure Int14PortStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing Int14Port objects}
  {$ENDIF}
  {$IFDEF UseFossil}
  procedure FossilPortStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing FossilPort objects}
  {$ENDIF}
  {$IFDEF UseDigi14}
  procedure Digi14PortStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing Digi14Port objects}
  {$ENDIF}
  {$ENDIF}

  {=======================================================================}

implementation

type
  {For typecasting seg:ofs}
  OS = record
    Ofs : Word;
    Seg : Word;
  end;

const
  EmptySet : CharSet = [];

{$IFDEF MSDOS}
procedure IntOff; inline($9C/$FA);      {PUSHF/CLI}
procedure IntOn; inline($9D);           {POPF}
{$ELSE}
procedure IntOff; begin;end;
procedure IntOn; begin;end;
{$ENDIF}

{$I OOCOM.PA1}   {AbstractPort core, gets, puts, flow, modem status}
{$I OOCOM.PA2}   {AbstractPort line status, misc, UartPort, ApInt14Port}

end.
