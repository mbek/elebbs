{$S-,R-,V-,I-,B-,F-,A-}
{$M 16384, 0, 655360}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{*********************************************************}
{*                   COMTESTO.PAS 2.03                   *}
{*     Copyright (c) TurboPower Software 1991.           *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$IFNDEF UseUart}
{$IFNDEF UseInt14}
{$IFNDEF UseFossil}
{$IFNDEF UseDigi14}
  !! STOP COMPILE - no device type specified
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}

program ComTestO;
  {-Test program for Com ports (using oop port)}
uses
  {$IFDEF UseOpro}
  OpRoot,
  OpCrt,
  {$ENDIF}
  {$IFDEF UseTPro}
  TpMemChk,
  TpCrt,
  {$ENDIF}
  {$IFDEF Standalone}
  Crt,
  FastW1,
  {$ENDIF}
  Dos,
  ApMisc,
  ApPort,
  ApTimer,
  {$IFDEF UseUart}
  ApUart,
  {$ENDIF}
  {$IFDEF UseInt14}
  ApInt14,
  {$ENDIF}
  {$IFDEF UseFossil}
  ApFossil,
  {$ENDIF}
  {$IFDEF UseDigi14}
  ApDigi14,
  {$ENDIF}
  OOCom;

type
  ColorType =
  (WindowAttr,                    {Color for normal unselected items}
   HeaderAttr,                    {Color for window header}
   OffAttr,                       {Color for low register bits}
   ValueAttr,                     {Color for status values and high reg bits}
   HelpFAttr,                     {Color for help frame and header}
   HelpAttr,                      {Color for help text}
   TitleAttr);                    {Color for title}
  ColorArray = array[ColorType] of Byte;

  CapArray = array[1..65520] of Char;

  HdwFlowOption = (hOff, hDTR, hRTS, hBoth);
  SfwFlowOption = (sOff, sRcv, sTrans, sBoth);

const
  ColorColors : ColorArray = ($1E, $30, $13, $1B, $7B, $70, $3F);
  MonoColors : ColorArray = ($07, $70, $07, $0F, $07, $07, $70);

  {Misc}
  UseMono : Boolean = False;
  SafeMode : Boolean = True;
  TitleLine = 'Async Professional ComTestO 2.03';
  IrqNum : Byte = 0;                                                   {!!.02}

  {$IFDEF Tracing}
  TraceState : Boolean = False;
  StdTraceFile = 'COMTESTO.TRC';
  StdTraceSize = 500;
  {$ENDIF}

  {$IFDEF EventLogging}
  LoggingState : Boolean = False;
  StdLogFile = 'COMTESTO.LOG';
  StdLogSize = 2000;
  {$ENDIF}

  {For capturing received data}
  CaptureState : Boolean = False;
  StdCaptureFile = 'COMTESTO.CAP';
  CapBuffSize = 8192;

var
  AP : AbstractPortPtr;                {Pointer to a port object}
  ComName : ComNameType;               {Name of current port}
  Baud : LongInt;                      {Current baud rate}
  Parity : ParityType;                 {Current parity setting}
  DataBits : DataBitType;              {Current data bit setting}
  StopBits : StopBitType;              {Current stop bit setting}
  BufferSize : Word;                   {Size of in/out buffers}
  CurrentDTR, CurrentRTS : Boolean;    {Current state of DTR, RTS}
  CurrentError : Word;                 {Current line status}
  HexMode : Boolean;                   {Display received data in hex?]

  {Globals for holding old status states}
  LC, LS, MC, MS : Byte;
  OldLC, OldLS, OldMC, OldMS : Byte;
  OldBaud : LongInt;
  OldInUsed, OldOutUsed : Word;
  OldSWFState : FlowState;
  OldHdwFlow : HdwFlowOption;
  OldError : Word;
  UseDTR, UseRTS : Boolean;
  OldBreak, GotBreak : Boolean;
  HdwFlow : HdwFlowOption;
  SfwFlow : SfwFlowOption;
  OldSfwFlow : SfwFlowOption;

  {Misc globals}
  Colors : ColorArray;
  SaveAttr : Byte;
  SaveMode : Byte;
  RequestedDevice : Byte;
  PortOptions : Word;

  {Receive capturing stuff}
  CapFile : File;
  CapBuff : ^CapArray;
  CapIndex : Word;

const
  {Forces update of all status variables}
  FirstStatus : Boolean = True;

type
  String2 = String[2];

procedure WriteCaptureBlock;
  {-Writes the current capture block to disk}
var
  BytesWritten : Word;
begin
  Dec(CapIndex);
  BlockWrite(CapFile, CapBuff^, CapIndex, BytesWritten);
  if (IOResult <> 0) or (BytesWritten <> CapIndex) then begin
    Close(CapFile);
    if IOResult <> 0 then ;
    CaptureState := False;
  end;
  CapIndex := 1;
end;

procedure CaptureChar(C : Char);
  {-Adds C to the capture buffer. Writes buffer when full}
begin
  {Add the character}
  CapBuff^[CapIndex] := C;
  Inc(CapIndex);

  {Flush to disk}
  if CapIndex > CapBuffSize then
    WriteCaptureBlock;
end;

procedure StopCapture;
  {-Closes the capture file and turns off capturing}
begin
  if CaptureState then begin
    CaptureChar(^Z);
    WriteCaptureBlock;
    Close(CapFile);
    CaptureState := False;
  end;
end;

procedure CleanUp;
  {-Close the global port and anything else that needs to be done}
begin
  if AP <> nil then begin
    Dispose(AP, Done);
    AP := nil;
  end;

  if CaptureState then
    StopCapture;

  {$IFDEF Tracing}
  if TraceState then
    DumpTrace(StdTraceFile);
  {$ENDIF}

  {$IFDEF EventLogging}
  if LoggingState then
    DumpEvents(StdLogFile);
  {$ENDIF}
end;

procedure Abort(Msg : String; Code : Word);
  {-Show msg, clean up, and abort}
begin
  CleanUp;
  WriteLn(Msg,' ', Code);
  Halt(1);
end;

procedure InitCapture(Fname : PathStr);
  {-Starts receive capturing}
var
  Result : Word;
begin
  {If capture is already on, close the current file and start again}
  if CaptureState then
    Close(CapFile);

  {Open up a capture file}
  Assign(CapFile, Fname);
  ReWrite(CapFile, 1);
  Result := IOResult;
  if Result <> 0 then
    Abort('Failed to open capture file', Result);

  {Allocate a buffer}
  if not GetMemCheck(CapBuff, CapBuffSize) then
    Abort('Failed to allocate capture buffer', 0);

  {Prepare to start capturing}
  CapIndex := 1;
  CaptureState := True;
end;

function CharStr(C : Char; Len : Byte) : string;
  {-Return a string of length len filled with C}
var
  S : string;
begin
  if Len = 0 then
    CharStr[0] := #0
  else begin
    S[0] := Chr(Len);
    FillChar(S[1], Len, C);
    CharStr := S;
  end;
end;

function Long2Str(L : LongInt) : string;
  {-Convert a long/word/integer/byte/shortint to a string}
var
  S : string;
begin
  Str(L, S);
  Long2Str := S;
end;

function PadCh(S : string; C : Char; Len : Byte) : string;
  {-Return a string right-padded to length len with C}
var
  o : string;
  SLen : Byte absolute S;
begin
  if Length(S) >= Len then
    PadCh := S
  else begin
    o[0] := Chr(Len);
    Move(S[1], o[1], SLen);
    if SLen < 255 then
      FillChar(o[Succ(SLen)], Len-SLen, C);
    PadCh := o;
  end;
end;

function Pad(S : string; Len : Byte) : string;
  {-Return a string right-padded to length len with blanks}
begin
  Pad := PadCh(S, ' ', Len);
end;

procedure DrawWindows;
  {-Draw all windows}
var
  I : Integer;
begin
  {Init backgrounds}
  FastWrite(CharStr(' ', ScreenWidth), 1, 1, Colors[HeaderAttr]);
  FastWrite(CharStr(' ', ScreenWidth), 2, 1, Colors[HeaderAttr]);
  for I := 3 to ScreenHeight do
    FastWrite(CharStr(' ', ScreenWidth), I, 1, Colors[WindowAttr]);

  {Draw text}
  FastWrite('Line           Modem', 1, 5, Colors[HeaderAttr]);
  FastWrite(' Ctrl/Status     Ctrl/Status     ', 2, 1, Colors[HeaderAttr]);
  FastWrite('Port Setup       Buffer Status      Misc.', 2, 35, Colors[HeaderAttr]);

  {Port Setup text}
  FastWrite('ComPort', 3, 34, Colors[WindowAttr]);
  FastWrite('Baud', 4, 34, Colors[WindowAttr]);
  FastWrite('Parity', 5, 34, Colors[WindowAttr]);
  FastWrite('Data', 6, 34, Colors[WindowAttr]);
  FastWrite('Stop', 7, 34, Colors[WindowAttr]);

  {Buffer status text}
  FastWrite('MaxSize', 3, 52, Colors[WindowAttr]);
  FastWrite('InFree', 4, 52, Colors[WindowAttr]);
  FastWrite('InUsed', 5, 52, Colors[WindowAttr]);
  FastWrite('OutFree', 6, 52, Colors[WindowAttr]);
  FastWrite('OutUsed', 7, 52, Colors[WindowAttr]);

  {Misc. text}
  FastWrite('Flow', 3, 70, Colors[WindowAttr]);
  FastWrite('Hdw', 4, 70, Colors[WindowAttr]);
  FastWrite(CharStr(' ', 80), 11, 1, Colors[HeaderAttr]);
  FastWrite(' Receive Window ', 11, 30, Colors[HeaderAttr]);
  FastWrite(CharStr(' ', 80), 19, 1, Colors[HeaderAttr]);
  FastWrite(' Transmit/Message Window ', 19, 27, Colors[HeaderAttr]);
  FastWrite(
    Pad('F1-Help  F2-Xmit Test  F3-DTR  F4-RTS  F5-Clear  F6-Hex Mode', 80),
    25, 1, Colors[HeaderAttr]);
  FastWrite(Pad(TitleLine, 40), 1, 40, Colors[TitleAttr]);
end;

procedure UpdateStatus;
  {-Update the status window values}
const
  OnOff : array[Boolean] of String[3] = ('Off', 'On ');
  OffSent : array[Boolean] of String[5] = ('     ', 'XSent');
  OffReceived : array[Boolean] of String[5] = ('     ', 'XRcvd');
  ParityStr : array[ParityType] of String[5] = (
    'None ', 'Odd  ', 'Even ', 'Mark ', 'Space');
  DataBitStr : array[DataBitType] of Char = ('5', '6', '7', '8');
  StopBitStr : array[StopBitType] of Char = ('1', '2');
  OverRunStr : array[Boolean] of String[7] = ('       ', 'OverRun');
  ParityErrorStr : array[Boolean] of String[6] = ('      ', 'Parity');
  FrameErrorStr : array[Boolean] of String[5] = ('     ', 'Frame');
  HdwState : array[HdwFlowOption] of String[4] =
    ('Off ', 'DTR ', 'RTS ', 'Both');
  SfwState : array[SfwFlowOption] of String[4] =
    ('Off ', 'Recv', 'Trans', 'Both');

  LineControlStr : array[0..7] of String[4] =
    ('WSL0', 'WSL1', 'STB', 'PEN', 'EPS', 'STK', 'BRK', 'DLAB');
  LineStatusStr : array[0..7] of String[4] =
    ('DR', 'OE', 'PE', 'FE', 'BI', 'THRE', 'TEMT', 'FERR');
  ModemControlStr : array[0..4] of String[4] =
    ('DTR', 'RTS', 'OW1', 'OW2', 'LOOP');
  ModemStatusStr : array[0..7] of String[4] =
    ('DCTS', 'DDSR', 'DRI', 'DDCD', 'CTS', 'DSR', 'RI', 'DCD');
  BreakStatusStr : array[Boolean] of String[5] = ('     ', 'Break');
  FifoStatusStr : array[Boolean] of String[4] = ('    ', 'FIFO');

  BitMask : array[0..7] of Byte = ($01, $02, $04, $08, $10, $20, $40, $80);

var
  B : Boolean;
  StateAttr : array[Boolean] of Byte;
  I : Word;
  NewBit : Byte;
begin
  {Fill in the StateAttr attributes}
  StateAttr[True] := Colors[ValueAttr];
  StateAttr[False] := Colors[OffAttr];

  with AP^ do begin
    {Get new values for LC, LS, MC and MS}
    LC := GetLineControl;
    LS := GetLineStatus;
    MC := GetModemControl;
    MS := GetModemStatus;

    {Update LC column}
    for I := 0 to 7 do begin
      NewBit := LC and BitMask[I];
      if (NewBit <> (OldLC and BitMask[I])) or FirstStatus then
        FastWrite(LineControlStr[I], I+3, 2, StateAttr[NewBit <> 0]);
    end;

    {Update LS column}
    for I := 0 to 7 do begin
      NewBit := LS and BitMask[I];
      if (NewBit <> (OldLS and BitMask[I])) or FirstStatus then
        FastWrite(LineStatusStr[I], I+3, 8, StateAttr[NewBit <> 0]);
    end;

    {Update MC column}
    for I := 0 to 4 do begin
      NewBit := MC and BitMask[I];
      if (NewBit <> (OldMC and BitMask[I])) or FirstStatus then
        FastWrite(ModemControlStr[I], I+3, 18, StateAttr[NewBit <> 0]);
    end;

    {Update MS column}
    for I := 0 to 7 do begin
      NewBit := MS and BitMask[I];
      if (NewBit <> (OldMS and BitMask[I])) or FirstStatus then
        FastWrite(ModemStatusStr[I], I+3, 24, StateAttr[NewBit <> 0]);
    end;

    {Update Port Status}
    if FirstStatus then begin
      FastWrite(ComNameString(ComName), 3, 42, Colors[ValueAttr]);
      FastWrite(ParityStr[Parity], 5, 42, Colors[ValueAttr]);
      FastWrite(DataBitStr[DataBits], 6, 42, Colors[ValueAttr]);
      FastWrite(StopBitStr[StopBits], 7, 42, Colors[ValueAttr]);
    end;
    if (Baud <> OldBaud) or FirstStatus then begin
      FastWrite(Pad(Long2Str(Baud),6), 4, 42, Colors[ValueAttr]);
      OldBaud := Baud;
    end;

    {Update Buffer status}
    if FirstStatus then
      FastWrite(Pad(Long2Str(AP^.PR^.InBuffLen),5), 3, 60, Colors[ValueAttr]);
    if (OldInUsed <> InBuffUsed) or FirstStatus then begin
      FastWrite(Pad(Long2Str(InBuffFree),5), 4, 60, Colors[ValueAttr]);
      FastWrite(Pad(Long2Str(InBuffUsed),5), 5, 60, Colors[ValueAttr]);
      OldInUsed := InBuffUsed;
    end;
    if (OldOutUsed <> OutBuffUsed) or FirstStatus then begin
      FastWrite(Pad(Long2Str(OutBuffFree),6), 6, 60, Colors[ValueAttr]);
      FastWrite(Pad(Long2Str(OutBuffUsed),5), 7, 60, Colors[ValueAttr]);
      OldOutUsed := OutBuffUsed;
    end;

    {$IFDEF UseSWFlow}
    {Update flow control indicators}
    if (OldSfwFlow <> SfwFlow) or FirstStatus then begin
      OldSfwFlow := SfwFlow;
      FastWrite(SfwState[OldSfwFlow], 3, 76, Colors[ValueAttr]);
    end;
    {Update Xon/Xoff receive/send indicators}
    if (OldSWFState <> SWFlowState) or FirstStatus then begin
      OldSWFState := SWFlowState;
      B := (OldSWFState = fsRecWait) or (OldSWFState = fsAllWait);
      FastWrite(OffSent[B], 5, 70, Colors[ValueAttr]);
      B := (OldSWFState = fsTransWait) or (OldSWFState = fsAllWait);
      FastWrite(OffReceived[B], 6, 70, Colors[ValueAttr]);
    end;
    {$ENDIF}

    {$IFDEF UseHWFlow}
    if (OldHdwFlow <> HdwFlow) or FirstStatus then begin
      OldHdwFlow := HdwFlow;
      FastWrite(HdwState[OldHdwFlow], 4, 76, Colors[ValueAttr]);
    end;
    {$ENDIF}

    {Update error text}
    if (CurrentError <> OldError) or FirstStatus then begin
      B := CurrentError = ecOverrunError;
      FastWrite(OverRunStr[B], 7, 70, Colors[ValueAttr]);
      B := CurrentError = ecParityError;
      FastWrite(ParityErrorStr[B], 8, 70, Colors[ValueAttr]);
      B := CurrentError = ecFramingError;
      FastWrite(FrameErrorStr[B], 9, 70, Colors[ValueAttr]);
      OldError := CurrentError;
    end;

    {Update break indicator}
    if (OldBreak <> GotBreak) or FirstStatus then begin
      FastWrite(BreakStatusStr[GotBreak], 10, 70, Colors[ValueAttr]);
      OldBreak := GotBreak;
    end;

    {$IFDEF UseUart}
    {Update FIFO indicator}
    if FirstStatus and (RequestedDevice = UartDevice) then
      FastWrite(FifoStatusStr[FifoStatus(GetBaseAddr)],
                25, 75, Colors[HeaderAttr]);
    {$ENDIF}
  end;

  {Note current values for next pass}
  FirstStatus := False;
  OldLC := LC;
  OldLS := LS;
  OldMC := MC;
  OldMS := MS;
end;

procedure DisplayHelpWindow;
  {-Display a screen of commands}
const
  HelpText : array[1..18] of String[66] = (
  ' <F2> - Timed transmit test (500 characters)',
  ' <F3> - Toggle DTR On/Off (Data Terminal Ready)',
  ' <F4> - Toggle RTS On/Off (Request To Send)',
  ' <F5> - Acknowledge line errors (clears them from screen)',
  ' <F6> - Toggle display mode between Normal and Hex',
  ' <F9> - Perform "current test"',
  ' ',
  ' <AltC> - Next COMn port (cycles through all known ports)',
  ' <AltB> - Next baud rate (cycles from 1200 to 115K)',
  ' <AltD> - Next data bit setting (cycles from 5 to 8)',
  ' <AltS> - Next stop bit setting (1 or 2)',
  ' <AltP> - Next parity setting (None, Even, Odd, Mark, Space)',
  ' <AltF> - Toggle automatic Xon/Xoff flow control',
  ' <AltH> - Toggle automatic hardware handshaking',
  ' <AltA> - Toggle receive data capturing',
  ' <AltT> - Toggle Tracing of PutChar/GetChar',
  ' <AltE> - Toggle EventLogging of low-level interrupts',
  ' <AltX> or <Escape> to exit');

var
  I, HAttr : Byte;
  Covers : Pointer;
  C : Char;

begin
  {Save the underlying window}
  if not SaveWindow(6, 4, 73, 23, True, Covers) then begin
    Write(^G);
    Exit;
  end;

  {Frame the window}
  FrameWindow(6, 4, 73, 23, Colors[HelpFAttr], Colors[HelpFAttr],
              ' ComTestO commands ');

  HAttr := Colors[HelpAttr];
  for I := 5 to 22 do
    FastWrite(Pad(HelpText[I-4], 66), I, 7, HAttr);

  {Wait for a keystroke}
  while KeyPressed do C := ReadKey;
  repeat until KeyPressed;
  while KeyPressed do C := ReadKey;

  {Restore the window}
  RestoreWindow(6, 4, 73, 23, True, Covers);
end;

procedure TransmitTest;
const
  CharsToTrans = 100;
  Digits : array[1..10] of char = '1234567890';
var
  ET : EventTimer;
  I : Integer;
  T : real;
  Block : array[1..CharsToTrans] of Char;
  BlockLen : Word;
  BytesWritten : Word;
  CPS : Real;
begin
  WriteLn(^M^J'Transmit Test in progress...');
  NewTimer(ET, 0);
  for I := 1 to (CharsToTrans div 10) do
    Move(Digits, Block[(I-1)*10+1], SizeOf(Digits));
  BlockLen := SizeOf(Block);
  AP^.PutBlockDirect(Block, BlockLen, BytesWritten);

  {Dawdle until all chars sent}
  repeat
    UpdateStatus;
  until AP^.OutBuffUsed = 0;

  {Show elapsed time and chars/second}
  T := ElapsedTime(ET);
  Writeln(^M^M);
  Writeln('Transmit Test complete');
  Writeln('Elapsed time: ', (T/18.2):6:2, ' seconds');
  if T > 0 then begin
    CPS := CharsToTrans / (T/18.2);
    Writeln('CPS :         ', CPS:6:2);
  end;
end;

procedure CurrentTest;
  {-For coding adhoc tests}
begin
end;

function OpenRequestedPort(ComName : ComNameType; Baud : LongInt;
                           Parity : ParityType;
                           DataBits : DataBitType;
                           StopBits : StopBitType) : Boolean;
  {-Open a port from the requested device (Sets global AP)}
begin
  OpenRequestedPort := False;
  case RequestedDevice of
    {$IFDEF UseUart}
    UartDevice :
      New(UartPortPtr(AP), InitCustom(ComName,
                                      Baud, Parity, DataBits, StopBits,
                                      BufferSize, BufferSize,
                                      DefPortOptions));
    {$ENDIF}
    {$IFDEF UseInt14}
    Int14Device :
      New(Int14PortPtr(AP), InitCustom(ComName,
                                       Baud, Parity, DataBits, StopBits,
                                       BufferSize, BufferSize,
                                       DefPortOptions));
    {$ENDIF}
    {$IFDEF UseFossil}
    FossilDevice :
      New(FossilPortPtr(AP), InitCustom(ComName,
                                        Baud, Parity, DataBits, StopBits,
                                        BufferSize, BufferSize,
                                        DefPortOptions or
                                        DefFossilOptions or
                                        ptTrueOutBuffFree));
    {$ENDIF}
    {$IFDEF UseDigi14}
    Digi14Device :
      New(Digi14PortPtr(AP), InitCustom(ComName,
                                        Baud, Parity, DataBits, StopBits,
                                        BufferSize, BufferSize,
                                        DefPortOptions));
    {$ENDIF}
  end;
  if AP <> nil then
    OpenRequestedPort := True;
end;

function CheckCommands : Char;
  {-Key was pressed, return key or perform special action}
const
  F1  = #$3B;           {Display help window}
  F2  = #$3C;
  F3  = #$3D;
  F4  = #$3E;
  F5  = #$3F;
  F6  = #$40;
  F7  = #$41;
  F8  = #$42;
  F9  = #$43;
  F10 = #$44;

  AltC = #$2E;           {Inc Com port}
  AltB = #$30;           {Inc baud rate}
  AltD = #$20;           {Inc data bits}
  AltS = #$1F;           {Inc stop bits}
  AltP = #$19;           {Inc Parity}
  AltM = #$32;           {Inc Buffer Size}
  AltF = #$21;           {Toggle Flow control}
  AltH = #$23;           {Toggle hardware handshaking}
  AltX = #$2D;           {Quit}
  AltT = #$14;           {Tracing}
  AltE = #$12;           {EventLogging}
  AltA = #$1E;           {Capturing}

const
  HexModeStr : array[Boolean] of String[10] = ('          ', '[Hex mode]');
  TraceStr : array[Boolean] of String[9] = ('         ', '[Tracing]');
  LogStr : array[Boolean] of String[9] = ('         ', '[Logging]');
  CaptureStr : array[Boolean] of String[9] = ('         ', '[Capture]');

var
  Start : ComNameType;
  Options : Word;
  C : Char;
  Ok : Boolean;

begin
  C := ReadKey;
  CheckCommands := C;

  with AP^ do begin
    {Check for exit request}
    if C = #0 then begin
      C := ReadKey;
      case C of
        F1  : {Display help window}
          DisplayHelpWindow;
        F2  : {Timed transmit test}
          TransmitTest;
        F3  : {Toggle DTR (data terminal ready)}
          begin
            CurrentDTR := not CurrentDTR;
            SetDTR(CurrentDTR);
          end;
        F4  : {Toggle RTS (request to send)}
          begin
            CurrentRTS := not CurrentRTS;
            SetRTS(CurrentRTS);
          end;

        F5  : {Acknowledge line errors and breaks}
          begin
            CurrentError := 0;
            GotBreak := False;
          end;

        F6  : {Toggle hex display mode}
          begin
            HexMode := not HexMode;
            FastWrite(HexModeStr[HexMode], 11, 50, Colors[HeaderAttr]);
          end;

        F9  : {Perform adhoc testing}
          CurrentTest;

        AltC : {Close this port and open the next one}
          begin
            {Close the current port}
            if AP <> nil then begin
              Dispose(AP, Done);
              AP := nil;
            end;

            {Loop through all defined ports checking for UARTs}
            Start := ComName;
            repeat
              {Get next port name}
              if ComName = Com8 then
                ComName := Com1
              else
                Inc(ComName);

              {$IFDEF UseUart}
              Ok := True;
              if SafeMode then
                if not UartTest3(DefBaseAddr[ComName]) then
                  Ok := False;
              {$ENDIF}

              {Try to open it}
              if Ok then begin
                if OpenRequestedPort(ComName, Baud, Parity,
                                     DataBits, StopBits) then begin
                  AP^.SetModem(True, True);
                  CurrentDTR := True;
                  CurrentRTS := True;
                  FirstStatus := True;
                  {$IFDEF UseUart}
                  if ClassifyUart(GetBaseAddr, False) = U16550A then
                    SetFifoBuffering(GetBaseAddr, True, 8);
                  {$ENDIF}
                  Exit;
                end;
              end;

              {Anymore ports to try}
              if ComName = Start then
                {If we get here, we couldn't even open the old port}
                Abort('Failed to find any ports ', 0);
            until False;
          end;

        AltB : {Select the next baud rate}
          begin
            repeat
              case (Baud div 10)  of
                120   : Baud := 2400;
                240   : Baud := 4800;
                480   : Baud := 9600;
                960   : Baud := 19200;
                1920  : Baud := 38400;
                3840  : Baud := 57600;
                5760  : Baud := 115200;
                11520 : Baud := 1200;
              end;
              SetLine(Baud, Parity, DataBits, StopBits);
            until AsyncStatus = ecOk;
            FirstStatus := True;
          end;

        AltP : {Select the next parity value}
          begin
            if Parity = SpaceParity then
              Parity := NoParity
            else
              inc(Parity);
            SetLine(Baud, Parity, DataBits, StopBits);
            FirstStatus := True;
          end;

        AltD : {Select the next data bit value}
          begin
            if DataBits = 8 then
              DataBits := 5
            else
              Inc(DataBits);
            SetLine(Baud, Parity, DataBits, StopBits);
            FirstStatus := True;
          end;

        AltS : {Switch stxop bit values}
          begin
            if StopBits = 1 then
              StopBits := 2
            else
              StopBits := 1;
            SetLine(Baud, Parity, DataBits, StopBits);
            FirstStatus := True;
          end;

        {$IFDEF UseSWFlow}
        AltF : {Toggle software flow control}
          begin
            SWFlowDisable;

            {Increment option to next setting}
            if SfwFlow = sBoth then
              SfwFlow := sOff
            else
              Inc(SfwFlow);

            {Build option word based on current setting}
            Options := 0;
            case SfwFlow of
              sOff :  {Turn both off (already are)};
              sRcv :  {Turn receive on}
                Options := sfReceiveFlow;
              sTrans: {Turn transmit on}
                Options := sfTransmitFlow;
              sBoth : {Turn both on}
                Options := sfReceiveFlow or sfTransmitFlow;
            end;

            {Turn flow control on}
            if SfwFlow <> sOff then
              SWFlowEnableOpt(Round(BufferSize * 0.75),
                              Round(BufferSize * 0.25),
                              Lo(Options));
            FirstStatus := True;
          end;
        {$ENDIF}

        {$IFDEF UseHWFlow}
        AltH : {Next hardware flow control mode}
          begin
            {Force handshaking to off}
            HWFlowDisable;

            {Increment option to next setting}
            if HdwFlow = hBoth then
              HdwFlow := hOff
            else
              Inc(HdwFlow);

            {Build option word based on current setting}
            Options := 0;
            case HdwFlow of
              hOff : {Turn both off (already are)};
              hDTR : {Turn DTR on}
                Options := hfUseDTR or hfRequireDSR;
              hRTS : {Turn RTS on}
                Options := hfUseRTS or hfRequireCTS;
              hBoth : {Turn both on}
                Options := hfUseDTR or hfRequireDSR or hfUseRTS or hfRequireCTS;
            end;

            {Turn handshaking on and require the corresponding signal}
            if HdwFlow <> hOff then
              HWFlowEnable(Round(BufferSize * 0.75),
                           Round(BufferSize * 0.25),
                           Options);
            FirstStatus := True;
          end;
        {$ENDIF}

        {$IFDEF Tracing}
        AltT : {Toggle Tracing on/off (dump the trace when toggling off)}
          begin
            TraceState := not TraceState;
            if not TraceState then
              DumpTrace(StdTraceFile)
            else
              InitTracing(StdTraceSize);
            FastWrite(TraceStr[TraceState], 11, 60, Colors[HeaderAttr]);
          end;
        {$ENDIF}

        {$IFDEF EventLogging}
        AltE : {Toggle EventLogging on/off (dump the log when toggling off)}
          begin
            LoggingState := not LoggingState;
            if not LoggingState then
              DumpEvents(StdLogFile)
            else
              InitEventLogging(StdLogSize);
            FastWrite(LogStr[LoggingState], 11, 70, Colors[HeaderAttr]);
          end;
        {$ENDIF}

        AltA : {Toggle disk capture (rewrite file when toggled on)}
          begin
            if not CaptureState then
              InitCapture(StdCaptureFile)
            else
              StopCapture;
            FastWrite(CaptureStr[CaptureState], 11, 20, Colors[HeaderAttr]);
          end;

        AltX : {Exit program}
          CheckCommands := #27;
      end;
    end;
  end;
end;

procedure Terminal;
  {-Terminal plus special functions}
var
  C : Char;
  S : String[1];
  SaveMin, SaveMax : Word;
  SaveX, SaveY : Byte;
  TermRow : Integer;
  TermCol : Integer;

  function HexB(C : Char) : String2;
    {-return hex representation of C}
  const
    hc : array[0..15] of Char = '0123456789ABCDEF';
  begin
    HexB := hc[Byte(C) shr 4]+hc[Byte(C) and $F];
  end;

begin
  {Setup}
  TermRow := 1;
  TermCol := 1;
  Window(1, 20, 80, 24);
  GotoXY(1, 1);

  {Loop for I/O}
  repeat
    {Check for char to send}
    if KeyPressed then begin
      {Check (and perform) special commands}
      C := CheckCommands;
      if C = #27 then
        Exit
      else if C <> #0 then begin
        {Send to remote and display in transmit window}
        AP^.PutChar(C);
        Write(C);
        if C = ^M then
          Write(^J);
      end;
    end;

    {Check for char to receive}
    if AP^.CharReady then begin
      AP^.GetChar(C);
      if AsyncStatus <> ecOk then
        CurrentError := AsyncStatus mod 10000;

      {Check for receive capturing}
      if CaptureState then
        CaptureChar(C);

      {Save current window info}
      SaveMin := WindMin;
      SaveMax := WindMax;
      SaveX := WhereX;
      SaveY := WhereY;
      Window(1, 12, ScreenWidth, 18);

      {Go display received char (as char or hex}
      GotoXY(TermCol, TermRow);
      if HexMode then
        Write(HexB(C)+' ')
      else begin
        if C = ^M then
          TermCol := 0
        else if (C = ^J) then begin
          Dec(TermCol);
          if (TermRow < 7)  then
            Inc(TermRow)
          else
            WriteLn;
        end else
          Write(C);
      end;

      {Increment row, column}
      if HexMode then
        Inc(TermCol, 3)
      else
        Inc(TermCol);
      if (TermCol > ScreenWidth) or
         (HexMode and (TermCol > ScreenWidth-2)) then begin
        TermCol := 1;
        if TermRow < 7 then
          Inc(TermRow)
        else if HexMode then
          WriteLn;
      end;

      {Restore old window}
      WindMin := SaveMin;
      WindMax := SaveMax;
      GotoXY(SaveX, SaveY);
    end;

    {Check for break}
    if AP^.CheckLineBreak then begin
      {Show status that we got a break}
      GotBreak := True;
      {Clear line errors if we got a break}
      CurrentError := 0;
    end;

    UpdateStatus;
  until false;
end;

procedure WriteHelp;
begin
  WriteLn('Usage: COMDIAG [options]');
  WriteLn('  /B Baudrate  300 - 115200 [Default = 1200]');
  WriteLn('  /C Comport   1 - 8 [Default = 1]');
  WriteLn('  /Q Irq       2 - 15 [Default = auto]');                   {!!.02}
  Writeln('  /M           Force monochrome mode');
  WriteLn('  /I           Use Int14 device');
  WriteLn('  /F           Use Fossil device');
  WriteLn('  /D           Use Digi14 device');
  WriteLn('  /X           Skip BIOS check before opening ports');
  Halt(1);
end;

procedure ParseCommandLine;
  {-Gets command line options and sets various parameters.}
var
  Code : Word;
  Param : String;
  Cnt : Word;
  ComNum : Word;

begin
  {Set defaults}
  ComName := Com1;
  Baud := 1200;
  Cnt := 1;

  {Scan command line}
  if ParamCount = 0 then
    Exit;
  Param := ParamStr(Cnt);
  Inc(Cnt);

  while Length(Param) <> 0 do begin
    case Param[1] of
      '/', '-' :
        if Length(Param) <> 2 then
          Abort('Invalid parameter: '+Param, 1)
        else
          case Upcase(Param[2]) of

            'B' : {Set baud rate}
              begin
                Param := ParamStr(Cnt);
                Inc(Cnt);
                Val(Param, Baud, Code);
                if Code <> 0 then
                  Abort('Invalid baud rate: '+Param, 1);
              end;

            'C' : {Set Com port}
              begin
                Param := ParamStr(Cnt);
                Inc(Cnt);
                Val(Param, ComNum, Code);
                if Code <> 0 then
                  Abort('Invalid com port: '+Param, 1);
                if (ComNum < 1) or (ComNum > 36) then
                  Abort('Com port number out of range: '+Param, 1);
                ComName := ComNameType(ComNum-1);
              end;

            {!!.02 new}
            'Q' : {Set custom IRQ}
              begin
                Param := ParamStr(Cnt);
                Inc(Cnt);
                Val(Param, IrqNum, Code);
                if Code <> 0 then
                  Abort('Invalid IRQ: '+Param, 1);
                if (IrqNum < 2) or (IrqNum > 15) then
                  Abort('IRQ number out of range: '+Param, 1);
              end;

            '?' : {Request for help}
              WriteHelp;

            'M' : {Request mono mode}
              UseMono := True;

            'I' : {Request Int14 device layer}
              RequestedDevice := Int14Device;

            'F' : {Request Fossil device}
              RequestedDevice := FossilDevice;

            'D' : {Request Digi14 device}
              RequestedDevice := Digi14Device;

            'X' : {Skip BIOS check before opening ports}
              SafeMode := False;

          else
            Abort('Invalid parameter: '+Param, 1);
          end;
    end;

    {Get next parameter}
    if Cnt > ParamCount then                                           {!!.03}
      Exit;
    Param := ParamStr(Cnt);
    Inc(Cnt);
  end;
end;

begin
  {Pick a default port type}
  {$IFDEF UseInt14}
  RequestedDevice := Int14Device;
  {$ENDIF}
  {$IFDEF UseFossil}
  RequestedDevice := FossilDevice;
  {$ENDIF}
  {$IFDEF UseDigi14}
  RequestedDevice := Digi14Device;
  {$ENDIF}
  {$IFDEF UseUart}
  RequestedDevice := UartDevice;
  {$ENDIF}

  {Get options}
  ParseCommandLine;

  {Force mono colors if last mode was a BW mode}
  if (LastMode = 2) or (LastMode = 7) then
    UseMono := True;

  {Always force TextMode 80x25}
  SaveAttr := TextAttr;
  SaveMode := LastMode;
  if UseMono then begin
    TextMode(BW80);
    Colors := MonoColors;
  end else begin
    TextMode(CO80);
    Colors := ColorColors;
  end;
  TextAttr := Colors[WindowAttr];

  {Set initial port values}
  DataBits := 8;
  StopBits := 1;
  Parity := NoParity;
  BufferSize := 500;

  {$IFDEF UseUart}                                                     {!!.02}
  {Set custom IRQ}                                                     {!!.02}
  if IrqNum <> 0 then                                                  {!!.02}
    uSetUart(ComName, 0, IrqNum, 0);                                   {!!.02}
  {$ENDIF}                                                             {!!.02}

  {Open requested port...pointer stored in global AP}
  if not OpenRequestedPort(ComName, Baud, Parity,
                           DataBits, StopBits) then
    Abort('Failed to open port ', AsyncStatus);

  {Turn on our DTR, RTS}
  AP^.SetModem(True, True);
  CurrentDTR := True;
  CurrentRTS := True;

  {Other inits}
  HexMode := False;
  UseDTR := False;
  UseRTS := False;
  GotBreak := False;
  HdwFlow := hOff;
  SfwFlow := sOff;

  {$IFDEF UseUart}
  {Turn on FIFO support if available}
  if ClassifyUart(AP^.GetBaseAddr, False) = U16550A then
    SetFifoBuffering(AP^.GetBaseAddr, True, 8);
  {$ENDIF}

  {Draw windows}
  DrawWindows;
  UpdateStatus;

  {Go start terminal}
  Terminal;

  {Clean up (close port, possibly close Tracing and EventLogging)}
  CleanUp;

  {Restore the video}
  TextMode(SaveMode);
  TextAttr := SaveAttr;
  Window(1,1,80,25);
  ClrScr;
end.
