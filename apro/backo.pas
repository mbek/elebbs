{$S-,R-,I-,F+,V-}
{$M 16384, 0, 655360}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{Activate this define only if you have OPRO 1.13 or greater}
{.$DEFINE SuppressMouse}

{Activate *one* of the following defines}
{$DEFINE DemoXYModem}
{.$DEFINE DemoZmodem}
{.$DEFINE DemoKermit}
{.$DEFINE DemoBPlus}

{*********************************************************}
{*                     BACKO.PAS 2.03                    *}
{*     Copyright (c) TurboPower Software 1991.           *}
{*                 All rights reserved.                  *}
{*********************************************************}

program BackO;
  {-Demonstration of background protocols}
uses
  {--- RTL  ---}
  Dos,
  {--- OPRO ---}
  OpMouse,
  OpRoot,
  OpCrt,
  OpInt,
  OpTsr,
  OpString,
  {--- APRO ---}
  ApTimer,
  ApMisc,
  ApPort,
  ApUart,
  OoCom,
  OoAbsPcl,
  {$IFDEF DemoXYModem}
  OoXmodem,
  OoYmodem;
  {$ENDIF}
  {$IFDEF DemoZmodem}
  OoZmodem;
  {$ENDIF}
  {$IFDEF DemoKermit}
  OoKermit;
  {$ENDIF}
  {$IFDEF DemoBPlus}
  OoBPlus;
  {$ENDIF}

type
  TransferType = (tmTransmit, tmReceive);

  {Protocol information record}
  ProtInfoPtr = ^ProtInfoRec;
  ProtInfoRec = record
    TransferMode  : TransferType;  {Transmit or receive}
    FName         : PathStr;       {Holds filename (if needed)}
    {$IFDEF DemoXYModem}
    Prot          : Byte;          {Type of protocol requested}
    Use1KBlocks   : Boolean;       {True if 1K mode requested}
    UseGMode      : Boolean;       {True if Gmode requested}
    {$ENDIF}
    {$IFDEF DemoZmodem}
    ClobberFile   : Boolean;       {True to overwrite existing files}
    ResumeFile    : Boolean;       {True to resume previous transfer}
    NewerLonger   : Boolean;       {True to overwrite only if newer or longer}
    {$ENDIF}
  end;

const
  {TSR control constants}
  ClockHandle  : Byte = 15;
  ModuleName   : String[6]  = 'BACKO';
  PgmId        : String[10] = 'BackO 2.03';
  HotKey1      : Word = $0519;       {Ctrl + RightShift, 'P' (main pop)}
  HotKey2      : Word = $051F;       {Ctrl + RightShift, 'S' (show status)}
  HotKey3      : Word = $0514;       {Ctrl + RightShift, 'T' (terminal)}
  ProtInterval : Word = 5;           {Do a protocol part every 5 tics}

  {Various flags and vars for controlling protocol/TSR behavior}
  StartXmitInts  : Boolean = True;    {True to kickstart forground app on abort}
  InTerminal     : Boolean = False;   {True if TermPop popped up}
  ForceTermExit  : Boolean = False;   {True to force TermPop to exit}
  DiscardPInfo   : Boolean = True;    {True if we've allocated PInfo}
  ProtocolInProg : Boolean = False;   {True if we've started a transfer}
  PFirst         : Boolean = False;   {True if first call to ProtProc}
  PendingAbort   : Boolean = False;   {True if cancel requested}
  ProtocolReady  : Boolean = False;   {True to recall protocol immediately}
  BaseAddr       : Word = 0;          {Where to send kickstart char}
  PInfo          : ProtInfoPtr = nil; {Pointer to protocol info}
  RestorePic     : Boolean = True;    {Force COM PIC bit on after DonePort}

  {PIC bits for each com port}
  PicBit : array[Com1..Com4] of Byte = ($10, $08, $10, $08);

  {Color attributes}
  TermWinAttr     = $07;
  TermFrameAttr   = $0F;
  TermHdrAttr     = $70;
  PromptWinAttr   = $30;
  PromptFrameAttr = $3F;
  PromptHdrAttr   = $3F;
  WinAttr         = $17;
  FrameAttr       = $1F;
  HdrAttr         = $71;

  {Mono attributes}
  TermWinAttrM     = $07;
  TermFrameAttrM   = $0F;
  TermHdrAttrM     = $0F;
  PromptWinAttrM   = $17;
  PromptFrameAttrM = $1F;
  PromptHdrAttrM   = $1F;
  WinAttrM         = $17;
  FrameAttrM       = $1F;
  HdrAttrM         = $1F;

var
  ComName        : ComNameType;             {COM port to use}
  Baud           : LongInt;                 {Baud rate to use}
  ComPort        : UartPortPtr;             {ComPort record}
  {$IFDEF DemoXYModem}
  PP             : XmodemProtocolPtr;       {Protocol record}
  {$ENDIF}
  {$IFDEF DemoZmodem}
  PP             : ZmodemProtocolPtr;       {Protocol record}
  {$ENDIF}
  {$IFDEF DemoKermit}
  PP             : KermitProtocolPtr;       {Protocol record}
  {$ENDIF}
  {$IFDEF DemoBPlus}
  PP             : BPlusProtocolPtr;        {Protocol record}
  {$ENDIF}
  ExitSave       : Pointer;                 {For saving prev exit proc}
  ProtProcHandle : Byte;                    {Protocol popproc}
  ProtStack      : array[1..4096] of Byte;  {Procotol popproc stack}
  TermStack      : array[1..4096] of Byte;  {Terminal popproc stack}
  StatStack      : array[1..4096] of Byte;  {Status popup stack}
  PopStack       : array[1..4096] of Byte;  {Main pop stack}
  Covers         : Pointer;                 {General covers buffer}
  MainCovers     : Pointer;                 {Covers buffer for MainPop}
  TermCovers     : Pointer;                 {Covers buffer for TermPop}
  SaveXY, SaveSL : Word;                    {For saving cursor state}

procedure Abort(Msg : String; Code : Word);
  {-Show msg, clean up, and abort}
begin
  WriteLn(Msg,' ', Code);
  Halt(1);
end;

procedure Beep;
begin
  Sound(700);
  Delay(100);
  NoSound;
end;

procedure PopMsg(Msg : String);
  {-Popup a message window with 'press any key to continue' msg}
var
  C : Char;
  P : Pointer;
begin
  if not SaveWindow(10, 10, 70, 12, True, P) then
    {assured to work};
  ClearWindow(10, 10, 70, 12, ' ', WinAttr);
  FrameWindow(10, 10, 70, 12, FrameAttr, HdrAttr, '<press any key to continue>');
  FastWrite(Msg, 11, 11, WinAttr);
  C := ReadKey;
  RestoreWindow(10, 10, 70, 12, True, P);
end;

function GetFile : Boolean;
  {-Prompt for file name}
var
  P : Pointer;
  XY, SL : Word;
begin
  if not SaveWindow(10, 10, 70, 12, True, P) then ;
  GetCursorState(XY, SL);
  ClearWindow(10, 10, 70, 12, ' ', PromptWinAttr);
  FrameWindow(10, 10, 70, 12, PromptFrameAttr, PromptHdrAttr,
              ' Enter the file name ');
  GotoXYAbs(11, 11);
  NormalCursor;
  OpCrt.BufLen := 64;
  TextAttr := PromptWinAttr;
  PInfo^.FName := '';
  ReadLn(PInfo^.FName);
  PInfo^.FName := Trim(PInfo^.FName);
  GetFile := PInfo^.FName <> '';
  RestoreWindow(10, 10, 70, 12, True, P);
end;

{$IFDEF DemoXYModem}
function GetProtocolType : Boolean;
  {-Prompt for protocol type}
var
  P : Pointer;
  XY, SL : Word;
  C : Char;
begin
  if not SaveWindow(25, 10, 55, 16, True, P) then ;
  GetCursorState(XY, SL);
  ClearWindow(25, 10, 55, 16, ' ', PromptWinAttr);
  FrameWindow(25, 10, 55, 16, PromptFrameAttr, PromptHdrAttr, ' Select Protocol ');
  FastWrite(' X - Xmodem ',    11, 27, PromptWinAttr);
  FastWrite(' K - Xmodem1K ',  12, 27, PromptWinAttr);
  FastWrite(' L - Xmodem1KG ', 13, 27, PromptWinAttr);
  FastWrite(' Y - Ymodem ',    14, 27, PromptWinAttr);
  FastWrite(' G - YmodemG ',   15, 27, PromptWinAttr);
  FastWrite(' Press <Escape> to exit ', 16, 28, PromptHdrAttr);
  repeat
    C := Upcase(ReadKey);
  until C in ['X', 'K', 'L', 'Y', 'G', #27];
  if C = #27 then
    GetProtocolType := False
  else begin
    {Set protocol}
    case C of
      'X', 'K', 'L' : PInfo^.Prot := Xmodem;
      'Y', 'G'      : PInfo^.Prot := Ymodem;
    end;
    {Set options}
    with PInfo^ do begin
      UseGMode := False;
      Use1KBlocks := False;
      case C of
        'K'      : Use1KBlocks := True;
        'L', 'G' : begin
                     Use1KBlocks := True;
                     UseGMode := True;
                   end;
      end;
    end;
  end;

  RestoreWindow(25, 10, 55, 16, True, P);
  RestoreCursorState(XY, SL);
end;
{$ENDIF}

procedure XErrorProc(P : Pointer; var StatusCode : Word);
begin
  if (StatusCode div 10000) = epFatal then begin
    PopMsg('Protocol Error: ' + StatusStr(StatusCode));
    PendingAbort := True;
  end;
end;

function ResidentAbort : Boolean;
  {-Notifies protocol of pending abort}
begin
  ResidentAbort := PendingAbort;
end;

procedure ClockInt(BP : Word); interrupt;
  {-INT $1C handler -- periodically invokes the ProtProc popproc}
const
  Counter : Byte = 0;
var
  Regs : IntRegisters absolute BP;
begin
  {Always increment protocol interval counter}
  Inc(Counter);

  if ProtocolInProg and ProtocolReady then begin
    {Protocol is ready for immediate servicing}
    ProtocolReady := False;
    SetPopTicker(ProtProcHandle, 4);
    Counter := 0;
  end else begin
    {See if protocol is ready for interval servicing}
    if Counter > ProtInterval then begin
      Counter := 0;
      if ProtocolInProg then
        SetPopTicker(ProtProcHandle, 4);
    end;
  end;

  {Chain to previous INT $1C handler}
  ChainInt(Regs, IsrArray[ClockHandle].OrigAddr);
end;

procedure OpenPort;
  {-Open the com port and set all hooks}
begin
  New(ComPort, InitCustom(ComName, Baud, NoParity, 8, 1, 2148, 2148, DefPortOptions));
  ComPort^.ptOptionsOff(ptDropModemOnClose);
  if AsyncStatus <> ecOk then begin
    WriteLn('Failed to open port, ', AsyncStatus);
    ProtocolInProg := False;
    Exit;
  end;

  {Set a port abort function}
  ComPort^.SetAbortFunc(ResidentAbort);

  {Set user hooks}
  ComPort^.SetErrorProc(XErrorProc);

  {$IFDEF Tracing}
  {Start tracing}
  InitTracing(5000);
  {$ENDIF}

  {$IFDEF EventLogging}
  {Start event logging}
  InitEventLogging(2500);
  {$ENDIF}
end;

procedure ProtProc(var Regs : Registers);
  {-Do a small portion of the protocol}
const
  OurVec : Pointer = nil;
var
  I : Word;
  CurVec : Pointer;
  State : ProtocolStateType;
  ET : EventTimer;
  Start : Boolean;
  Upload : Boolean;
  C : Char;

  procedure StopProtocol(Error : Boolean);
  var
    B : Byte;
  begin
    {Turn off the protocol}
    ProtocolInProg := False;

    {Handle Tracing and EventLogging}
    {$IFDEF Tracing}
    DumpTrace('BACKO.TRC');
    {$ENDIF}
    {$IFDEF EventLogging}
    DumpEvents('BACKO.LOG');
    {$ENDIF}

    {Clean up the port}
    if PP <> nil then
      Dispose(PP, Done);
    BaseAddr := ComPort^.GetBaseAddr;

    {If error, then try to minimize the potential damage of DonePort....}
    if Error then
      ComPort^.ptOptionsOff(ptRestoreOnClose+ptDropModemOnClose);

    Dispose(ComPort, Done);

    {Optionally send bogus character to get underlying xmits going again}
    if StartXmitInts then
      Port[BaseAddr] := 0;

    {Optionally restore PIC bit}
    if RestorePIC then begin
      B := Port[$21];
      B := B xor PicBit[ComName];
      Port[$21] := B;
    end;
  end;

begin
  if InTerminal then
    Exit;

  if PFirst then begin
    {Open the port}
    OpenPort;
    if AsyncStatus <> ecOk then
      Exit;

    {Set flags}
    PendingAbort := False;

    {Init a protocol record}
    {$IFDEF DemoXYModem}
    case PInfo^.Prot of
      Xmodem : PP := New(XmodemProtocolPtr, Init(ComPort, PInfo^.Use1KBlocks, PInfo^.UseGMode));
      Ymodem : PP := New(YmodemProtocolPtr, Init(ComPort, True, PInfo^.UseGMode));
    end;
    {$ENDIF}
    {$IFDEF DemoZmodem}
    New(PP, Init(ComPort));
    {$ENDIF}
    {$IFDEF DemoKermit}
    New(PP, Init(ComPort));
    {$ENDIF}
    {$IFDEF DemoBPlus}
    New(PP, Init(ComPort));
    {$ENDIF}
    if PP = nil then begin
      StopProtocol(False);
      Exit;
    end;

    {More inits}
    case PInfo^.TransferMode of
      tmTransmit :
        PP^.SetFileMask(PInfo^.FName);
      tmReceive :
        ;
    end;

    {$IFDEF DemoBPlus}
    {Stay here while processing ENQ and DLEs....}
    NewTimerSecs(ET, 10);
    Start := False;

    {Assume ENQ already arrived...}
    with PP^, ComPort^ do begin
      ProcessENQ;

      {Process expected DLEs}
      repeat
        if CharReady then begin
          GetChar(C);
          if C = cDLE then
            ProcessDLE(Start, Upload);
        end;
      until Start or TimerExpired(ET);

      {Exit if we couldn't start the protocol}
      if not Start then begin
        StopProtocol(False);
        Exit;
      end;

      if Upload then
        PInfo^.TransferMode := tmTransmit
      else
        PInfo^.TransferMode := tmReceive;
    end;
    {$ENDIF}

    {Prepare the protocol}
    case PInfo^.TransferMode of
      tmTransmit :
        begin
          PP^.PrepareTransmitPart;
          if AsyncStatus <> ecOk then begin
            StopProtocol(False);
            Exit;
          end;
        end;
      tmReceive :
        begin
          {$IFDEF DemoXYModem}
          PP^.SetReceiveFilename(PInfo^.FName);
          {$ENDIF}
          PP^.SetOverwriteOption(WriteRename);
          PP^.PrepareReceivePart;
          if AsyncStatus <> ecOk then begin
            StopProtocol(False);
            Exit;
          end;
        end;
    end;

    {Say we're started}
    PFirst := False;

    {Note our vector for quick compares (later)}
    GetIntVec(ComPort^.PR^.IrqNumber+8, OurVec);
  end;

  {Protocol is in progress - assure we still own the vector}
  GetIntVec(ComPort^.PR^.IrqNumber+8, CurVec);

  if CurVec <> OurVec then begin
    {Someone stole the com port, we have no choice but to give up}
    StopProtocol(True);
    Exit;
  end;

  {Do another small portion of the protocol transfer}
  case PInfo^.TransferMode of
    tmTransmit :
      begin
        State := PP^.ProtocolTransmitPart;
        {Process results}
        case State of
          psFinished :
            StopProtocol(False);
          psReady :
            ProtocolReady := True;
          else
            ProtocolReady := False;
        end;
      end;

    tmReceive :
      begin
        State := PP^.ProtocolReceivePart;
        {Process results}
        case State of
          psFinished :
            StopProtocol(False);
          psReady :
            ProtocolReady := True;
          else
            ProtocolReady := False;
        end;
      end;
  end;
end;

procedure MainPop(var Regs : Registers);
  {-Invoke or abort the protocol}
var
  SaveXY, SaveSL : Word;   {for saving cursor position and shape}
  C : Char;
  WC : WindowCoordinates;
  MouseOn : Boolean;
begin
  if InTerminal then
    StoreWindowCoordinates(WC)
  else
    ReinitCrt;

  {Exit if not in text mode}
  if not InTextMode then begin
    Beep;
    Exit;
  end;

  {Hide mouse if mouse is installed}
  if MouseInstalled then
    HideMousePrim(MouseOn);

  {Note cursor position and shape, then hide it}
  GetCursorState(SaveXY, SaveSL);
  HiddenCursor;

  {Make a window, display choices, get choice}
  if not SaveWindow(10, 10, 70, 16, True, MainCovers) then ;
  ClearWindow(10, 11, 70, 16, ' ', WinAttr);
  FrameWindow(10, 11, 70, 16, FrameAttr, HdrAttr, ' '+PgmId+' ');
  FastWrite('  T)ransmit a file', 12, 11, WinAttr);
  FastWrite('  R)eceive a file', 13, 11, WinAttr);
  FastWrite('  A)bort a protocol in progress', 14, 11, WinAttr);
  FastWrite('  K)ickstart underlying program''s transmitter', 15, 11, WinAttr);
  FastWrite(' Press <Escape> to exit ', 16, 28, HdrAttr);
  repeat
    C := Upcase(ReadKey);
  until C in ['T', 'R', 'A', 'K', #27];
  case C of
    'T' : if not ProtocolInProg then begin
            {Make a PInfo record}
            if not GetMemCheck(PInfo, SizeOf(PInfo^)) then ;
            {$IFDEF DemoXYModem}
            if GetProtocolType then
            {$ENDIF}
              if GetFile then begin
                PFirst := True;
                ProtocolInProg := True;
                PInfo^.TransferMode := tmTransmit;
              end
          end else
            PopMsg('Already in progress');

    'R' : if not ProtocolInProg then begin
            {Make a PInfo record}
            if not GetMemCheck(PInfo, SizeOf(PInfo^)) then ;
            {$IFDEF DemoXYModem}
            if GetProtocolType then begin
              if PInfo^.Prot = Xmodem then begin
                if GetFile then begin
            {$ENDIF}
                  PFirst := True;
                  ProtocolInProg := True;
                  PInfo^.TransferMode := tmReceive;
            {$IFDEF DemoXYModem}
                end;
              end else begin
                PFirst := True;
                ProtocolInProg := True;
                PInfo^.TransferMode := tmReceive;
              end;
            end;
            {$ENDIF}
          end else
            PopMsg('Already in progress');
     'A' : if ProtocolInProg then
             PendingAbort := True
           else
             PopMsg('Not in progress');
     'K' : if not ProtocolInProg and (BaseAddr <> 0) then
             Port[BaseAddr] := 0;
  end;

  {Restore window and cursor}
  RestoreWindow(10, 10, 70, 16, True, MainCovers);
  if InTerminal then begin
    RestoreWindowCoordinates(WC);
    ForceTermExit := True;
  end;
  RestoreCursorState(SaveXY, SaveSL);

  {Show mouse if mouse is installed}
  if MouseInstalled and MouseOn then
    ShowMousePrim(True);
end;

procedure TermPop(var Regs : Registers);
  {-Execute a simple terminal}
var
  SaveXY, SaveSL : Word;   {for saving cursor position and shape}
  KW : Word;
  C : Char absolute KW;
  Finished : Boolean;
  WC : WindowCoordinates;
  SaveAttr : Byte;
  MouseOn : Boolean;
begin
  {Don't allow terminal to popup if protocol is in progress}
  if ProtocolInProg then
    Exit;

  {Reinitialize CRT}
  ReinitCrt;

  {Exit if not in text mode}
  if not InTextMode then begin
    Beep;
    Exit;
  end;

  {Hide mouse if mouse is installed}
  if MouseInstalled then
    HideMousePrim(MouseOn);

  InTerminal := True;
  ForceTermExit := False;

  {Note cursor position and shape, then set it}
  GetCursorState(SaveXY, SaveSL);
  NormalCursor;

  {Open the port}
  OpenPort;

  if not SaveWindow(1, 7, ScreenWidth, 18, True, TermCovers) then ;
  ClearWindow(1, 7, ScreenWidth, 18, ' ', TermWinAttr);
  FrameWindow(1, 7, ScreenWidth, 18, TermFrameAttr, TermHdrAttr, ' Terminal ');
  FastWrite(' Press <AltX> to exit ', 18, 31, TermHdrAttr);
  StoreWindowCoordinates(WC);
  Window(2, 8, ScreenWidth-1, 17);
  SaveAttr := TextAttr;
  TextAttr := TermWinAttr;

  Finished := False;
  repeat
    if ComPort^.CharReady then begin
      ComPort^.GetChar(C);
      Write(Char(Byte(C) and $7F));
    end;

    if KeyPressed then begin
      KW := ReadKeyWord;
      if C = #0 then begin
        case Hi(KW) of
          $2D : Finished := True;  {AltX}
        end;
      end else
        ComPort^.PutChar(C);
    end else
      {Let MainPop get control if its hotkey was just pressed}
      inline($CD/$28);
  until Finished or ForceTermExit;

  {Finished with the port (for now)}
  ComPort^.ptOptionsOff(ptRestoreOnClose or ptDropModemOnClose);
  Dispose(ComPort, Done);

  {Restore the screen and cursor}
  TextAttr := SaveAttr;
  RestoreWindow(1, 7, ScreenWidth, 18, True, TermCovers);
  RestoreWindowCoordinates(WC);
  RestoreCursorState(SaveXY, SaveSL);
  InTerminal := False;

  {Show mouse if mouse is installed}
  if MouseInstalled and MouseOn then
    ShowMousePrim(True);
end;

function FormatMinSec(TotalSecs : LongInt) : String;
  {-Format TotalSecs as minutes:seconds}
var
  Min, Sec : LongInt;
  S : String;
begin
  Min := TotalSecs div 60;
  Sec := TotalSecs mod 60;
  Str(Sec:2, S);
  if S[1] = ' ' then
    S[1] := '0';
  FormatMinSec := Long2Str(Min) + ':' + S;
end;

procedure StatusPop(var Regs : Registers);
  {-Shows current protocol status}
const
  Hdr : array[TransferType] of String[17] =
        (' Upload Status ', ' Download Status ');
var
  RepeatTimer : EventTimer;
  ActualCPS, R : Real;
  SaveXY, SaveSL : Word;
  C : Char;
  MouseOn : Boolean;
  State : Byte;
begin
  {Exit immediately if protocol not in progress}
  if not ProtocolInProg then
    Exit;

  {Exit immediately if the protocol hasn't been initialized yet}
  if PFirst then
    Exit;

  {Reinitialize CRT}
  ReinitCrt;

  {Exit if not in text mode}
  if not InTextMode then begin
    Beep;
    Exit;
  end;

  {Hide mouse cursor if mouse is installed}
  if MouseInstalled then
    HideMousePrim(MouseOn);

  {Note cursor position and shape, then hide it}
  GetCursorState(SaveXY, SaveSL);
  HiddenCursor;

  {Show the status}
  with PP^ do begin
    HiddenCursor;
    if SaveWindow(20, 10, 60, 18, True, Covers) then
      {assured to work} ;
    ClearWindow(20, 10, 60, 18, ' ', WinAttr);
    FrameWindow(20, 10, 60, 18, FrameAttr, HdrAttr, Hdr[PInfo^.TransferMode]);
    FastWrite(' Press <anykey> to exit ', 18, 28, HdrAttr);
    NewTimer(RepeatTimer, 1);
    repeat
      if TimerExpired(RepeatTimer) then begin

        {$IFDEF DemoXYModem}
        State := Ord(XmodemState);
        {$ENDIF}
        {$IFDEF DemoZmodem}
        State := Ord(ZmodemState);
        {$ENDIF}
        {$IFDEF DemoKermit}
        State := Ord(KermitState);
        {$ENDIF}
        {$IFDEF DemoBPlus}
        State := Ord(BPlusState);
        {$ENDIF}

        NewTimer(RepeatTimer, 36);
        FastWrite('  Protocol:        ' +
                  ProtocolTypeString[PP^.GetProtocol], 11, 21, WinAttr);
        FastWrite('  Filename:        ' +
                  PP^.GetFilename, 12, 21, WinAttr);
        FastWrite('  Protocol State:  ' +
                  LeftPad(Long2Str(State),2), 13, 21, WinAttr);
        FastWrite('  Elapsed Time:    ' +
                  FormatMinSec(Tics2Secs(PP^.GetElapsedTics)), 14, 21, WinAttr);
        FastWrite('  Transferred:     ' +
                  LeftPad(Long2Str(PP^.GetBytesTransferred),8), 15, 21, WinAttr);
        FastWrite('  Remaining:       ' +
                  LeftPad(Long2Str(PP^.GetBytesRemaining),8), 16, 21, WinAttr);

        {Calculate and display throughput}
        if PP^.GetElapsedTics > 0 then begin
          R := PP^.GetBytesTransferred - PP^.GetInitialFilePos;
          ActualCPS := R / (PP^.GetElapsedTics / 18.2);
        end else
          ActualCPS := 0.0;
        FastWrite('  Throughput:      ' +
                  Pad(Long2Str(Trunc(ActualCPS))+' CPS',9), 17, 21, WinAttr);
        NewTimer(RepeatTimer, 5);
      end;

      {Let MainPop get control if its hot key is pressed}
      inline($CD/$28);

    until KeyPressed or not ProtocolInProg;

    {Eat the ending keystroke}
    while KeyPressed do
      C := ReadKey;

    {Restore screen and cursor}
    RestoreWindow(20, 10, 60, 18, True, Covers);
    RestoreCursorState(SaveXY, SaveSL);
  end;

  {Show mouse cursor if mouse is installed}
  if MouseInstalled and MouseOn then
    ShowMousePrim(True);
end;

procedure ExternalIFC(BP : Word); interrupt;
  {-Entry point for external requests}
type
  OS = record
    O : Word;
    S : Word;
  end;
var
  Regs : IntRegisters absolute BP;
  SavePSP : Word;
begin
  {Unload TSR if requested}
  case Regs.AH of
    1 : begin   {Unload}
          SavePSP := GetPSP;
          SetPSP(PrefixSeg);
          Regs.AL := Ord(DisableTSR);
          SetPSP(SavePSP);
        end;
    2 : begin   {Start protocol}
          DiscardPInfo := False;
          OS(PInfo).O := Regs.DI;
          OS(PInfo).S := Regs.ES;
          PFirst := True;
          ProtocolInProg := True;
        end;
    3 : begin   {In Progress check}
          Regs.AH := Ord(ProtocolInProg);
        end;
  end;
end;

procedure UnloadTsr;
  {-Try to unload the resident copy}
var
  P : IfcPtr;
  Regs : IntRegisters;
begin
  P := ModulePtrByName(ModuleName);
  if P = nil then
    WriteLn('BackO isn''t loaded.')
  else if (P^.CmdEntryPtr <> nil) then begin
    RestoreAllVectors;
    Regs.AH := 1;
    EmulateInt(Regs, P^.CmdEntryPtr);
    if Regs.AL = 1 then
      WriteLn('BackO Unloaded')
    else
      WriteLn('Can''t unload BackO');
  end;
  Halt;
end;

procedure WriteHelp;
  {-Show command line help}
begin
  WriteLn('Usage: BackO [options]');
  WriteLn('  /B Baudrate  300 - 115200 [Default = 2400]');
  WriteLn('  /C Comport   1 - 8 [Default = 1]');
  WriteLn('  /U           Unload TSR');
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
  Baud := 2400;
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
                if (ComNum < 1) or (ComNum > 4) then
                  Abort('Com port number out of range: '+Param, 1);
                ComName := ComNameType(ComNum-1);
              end;

            '?' : {Request for help}
              WriteHelp;

            'U' : {Unload TSR}
              UnloadTsr;

          else
            Abort('Invalid parameter: '+Param, 1);
          end;
    end;

    {Get next parameter}
    if Cnt >= ParamCount then
      Exit;
    Param := ParamStr(Cnt);
    Inc(Cnt);
  end;
end;

begin
  {Process command line arguments}
  ParseCommandLine;

  {Don't install twice}
  if ModuleInstalled(ModuleName) then begin
    WriteLn('BackO already installed');
    Halt;
  end;

  {Install the module}
  InstallModule(ModuleName, @ExternalIfc);

  {$IFDEF SuppressMouse}
  {Turn off OPTSR's automatic mouse handling}
  if MouseInstalled then
    SuppressMouseHandling;
  {$ENDIF}

  {Setup entry points and go resident}
  if DefinePop(HotKey1, MainPop, @PopStack[SizeOf(PopStack)], True) and
     DefinePop(HotKey2, StatusPop, @StatStack[SizeOf(StatStack)], True) and
     DefinePop(HotKey3, TermPop, @TermStack[SizeOf(TermStack)], True) and
     DefinePopProc(ProtProcHandle, ProtProc, @ProtStack[SizeOf(ProtStack)]) then begin

    {Install clock ISR}
    if not InitVector($1C, ClockHandle, @ClockInt) then
      {Will never fail} ;

    {Show loading process...}
    HighVideo;
    WriteLn(^M^J,PgmId, ' - A demonstration of background protocol');
    WriteLn('Copyright (c) 1992 by TurboPower Software');
    LowVideo;
    WriteLn;
    WriteLn('BackO loaded, press Ctrl-RightShift-P to start protocol.');
    WriteLn('              press Ctrl-RightShift-T to use popup terminal');
    WriteLn('              press Ctrl-RightShift-S to view protocol status');

    {Enable popups}
    PopupsOn;

    {Terminate and stay resident}
    StayRes(ParagraphsToKeep+2048, 0);
  end;

  WriteLn('Failed to load BackO');
end.
