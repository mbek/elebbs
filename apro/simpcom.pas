{$R-,S-,I-,V-,F+}
{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{$M 16384, 0, 60000}
{*********************************************************}
{*                   SIMPCOM.PAS 2.03                    *}
{*     Copyright (c) TurboPower Software 1991.           *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$IFNDEF UseUart}
  !! STOP COMPILE - this program requires UseUart defined
{$ENDIF}
{$IFNDEF UseHWFlow}
  !! STOP COMPILE - this program requires UseHWFlow defined
{$ENDIF}

program SimpCom;
  {-A simple oop communications program}
uses
  {$IFDEF LeakCheck}
  LeakChek,
  {$ENDIF}
  {$IFDEF UseOpro}
  OpCrt,
  {$ENDIF}
  {$IFDEF UseTpro}
  TpCrt,
  {$ENDIF}
  {$IFDEF Standalone}
  Crt,
  {$ENDIF}
  Dos,
  FastW1,
  ApPort,
  ApUart,
  ApMisc,
  ApTimer,
  OOCom,
  OOModem,
  OOAbsPcl,
  OOXModem,
  OOYModem,
  OOZModem,
  OOKermit,
  OOBPlus,
  OOAscii,
  ApAnsi,
  ComUtil;

const
  F1               = $3B;     {scan code of F1 key}
  F2               = $3C;     {scan code of F2 key}
  F3               = $3D;     {scan code of F3 key}
  F4               = $3E;     {scan code of F4 key}
  F5               = $3F;     {scan code of F5 key}
  F6               = $40;     {scan code of F6 key}
  F7               = $41;     {scan code of F7 key}
  F8               = $42;     {scan code of F8 key}
  F9               = $43;     {scan code of F9 key}
  F10              = $44;     {scan code of F10 key}
  AltD             = $20;     {extended code of AltD key}
  AltX             = $2D;     {extended code of AltX key}
  PgUp             = $49;     {extended code of PgUp}
  PgDn             = $51;     {extended code of PgDn}

  MaxWindowStack    = 5;
  DefInBufferSize   = 4 * 1024;
  DefOutBufferSize  = 2 * 1024 + 30;

  DefCaptureBufferSize = 1024;

  {coordinates for terminal window}
  TX1 = 1;
  TY1 = 2;
  TX2 = 80;
  TY2 = 22;

  {coordinates for help window}
  HX1 = 16;
  HY1 = 4;
  HX2 = 67;
  HY2 = 19;

  {coordinates for parameters window}
  PX1  = 40;
  PY1  = 6;
  PX2  = 78;
  PY2  = 19;

  {coordinates for Set Options window}
  OX1  = 5;
  OY1  = 5;
  OX2  = 47;
  OY2  = 18;

  {coordinates for select protocol window}
  SPX1 = 25;
  SPY1 = 8;
  SPX2 = 55;
  SPY2 = 19;

  NumHelpLines = (HY2 - HY1) - 2;
  MaxHelpWidth = (HX2 - HX1) - 2;

  TitleStr         =          {the title of this program}
'SIMPCOM 2.03 - A simple communications program using Async Professional';
  HelpStr          =          {the simple help line on line 25}
'F1-Help F2-Params F3-Dial F4-Capture F5-Options F6-Hang up PgUp-Upl PgDn-Down';
  {various com parameter defaults}
  DefPort = Com1;
  DefBaud = 2400;
  DefParity = NoParity;
  DefBits = 8;
  DefStop = 1;

  HardwareFlowOn = hfUseRTS + hfRequireCTS;

  DefHardwareOptions = 0;
  DefSoftwareFlow = False;

  SimpComSignature = $6373;   {signature ID for SimpCom config files}

type
  HelpLinesType = Array[1..NumHelpLines] of String[MaxHelpWidth];
  WindowPtr = ^WindowRec;     {a pointer to our window record}
  WindowRec =                 {a simple window type}
    record
      wX1, wY1, wX2, wY2, wAttr : Byte;
      wFramed : Boolean;
      Buf : Pointer;
      LastXY : Word;
    end;
  WindowStack = Array[1..MaxWindowStack] of WindowPtr; {a simple window
                                                        stack type}
  TransferModeType = (Transmit, Receive);
  TonePulseType = (Default, Tone, Pulse);
  ComParamRecord =
    record
      Signature : Word;
      Port : ComNameType;
      Baud : LongInt;
      Parity : ParityType;
      Bits : DataBitType;
      Stop : StopBitType;
      ToneOrPulse : TonePulseType;
      SWFlow     : Boolean;
      HWHOptions : Word;
      PortOptions : Word;
    end;
  SimpComModem =
    object(HayesModem)
      DialingMode : TonePulseType;
      constructor Init(Port : AbstractPortPtr);
      procedure SetDialingMode(ToneOrPulse : TonePulseType);
      function GetDialingMode : TonePulseType;
    end;
var
  Uart : TerminalUart;        {the Port object used by our program}
  Modem : SimpComModem;       {the modem object used by our program}
  Capture : CaptureObj;       {object that manages file captures}
  TermWin : WindowPtr;        {the terminal window}
  WStack : WindowStack;       {the window stack}
  WStackPos : Byte;           {current position within the window stack}
  TransferMode : TransferModeType;
  MsgY, StatusY : Byte;
  BP : BPlusProtocolPtr;
const
  ParityChar : array[ParityType] of Char = ('N', 'O', 'E', 'M', 'S');
  TonePulse : Array[TonePulseType] of String[5] = ('Def  ', 'Tone ', 'Pulse');
  OnOff : array[Boolean] of String[3] = ('Off', 'On ');

  ProtoType : Array[Boolean] of String[8] = ('Download', 'Upload');
  HelpLines : HelpLinesType =
  ('',                                                 {1}
   '',                                                 {2}
   '  F1    Displays this detailed help screen',       {3}
   '  F2    Set communication parameters',             {4}
   '  F3    Prompt for number to dial and dial modem', {5}
   '  F4    Toggle state of capture file',             {6}
   '  F5    Set various SIMPCOM options',              {7}
   '  F6    Hang up the modem',                        {8}
   '  PgUp  Upload a file using a protocol',           {9}
   '  PgDn  Download a file using a protocol',        {10}
   '  Alt-D to shell to DOS',                         {11}
   '  Alt-X to Exit',                                 {12}
   '');                                               {13}

  ParamFName       : PathStr = 'SIMPCOM.DAT';

  OurTextAttr      : Byte = $07;
  StatusAttr       : Byte = $0F; {attribute to display status line}
  PromptAttr       : Byte = $17;
  MsgAttr          : Byte = $70;
  HelpAttr         : Byte = $70;
  TAttr            : Byte = $07;
  PAttr            : Byte = $70;
  SPAttr           : Byte = $70;
  WAttr            : Byte = $07;          {Window attribute}
  FAttr            : Byte = $70;          {Frame attribute}
  DAttr            : Byte = $0F;          {Data attribute}
  OAttr            : Byte = $70;

  AsciiInterChar     : Word = 0;
  AsciiInterLine     : Word = 0;
  IncludeDirs        : Boolean = False;
  Cr2CrLf            : Boolean = False;
  ShowOutGoingChars  : Boolean = False;
  AnsiMode           : Boolean = True;
  BPlusStarted       : Boolean = False;

  constructor SimpComModem.Init(Port : AbstractPortPtr);
  begin
    if not HayesModem.Init(Port) then
      Fail;
    DialingMode := Default;
  end;

  procedure SimpComModem.SetDialingMode(ToneOrPulse : TonePulseType);
  begin
    case ToneOrPulse of
      Tone :
        begin
          SetDialTone;
          DialingMode := Tone;
        end;
      Pulse :
        begin
          SetDialPulse;
          DialingMode := Pulse;
        end;
      Default :
        DialingMode := Default;
    end;
  end;

  function SimpComModem.GetDialingMode : TonePulseType;
  begin
    GetDialingMode := DialingMode;
  end;

  procedure ErrorBeep;
    {-Beep on error}
  begin
    Sound(440);
    Delay(800);
    NoSound;
  end;

  procedure FullWindow;
    {-Make Window() = whole screen}
  begin
    Window(1, 1, 80, ScreenHeight);
  end;

  procedure Abort(Msg : String);
    {-Close port and halt}
  begin
    FullWindow;
    ClrScr;
    WriteLn(Msg);
    ErrorBeep;
    Halt(1);
  end;

  procedure ActivateWindow(W : WindowPtr);
    {-Call CRT's Window() procedure for W}
  begin
    with W^ do begin
      if wFramed then
        Window(Succ(wX1), Succ(wY1), Pred(wX2), Pred(wY2))
      else
        Window(wX1, wY1, wX2, wY2);
      TextAttr := wAttr;
    end;
  end;

  function MakeWindow(X1, Y1, X2, Y2 : Byte;
                      TxtAttr, FrameAttr : Byte; Frame : Boolean;
                      Header : String) : WindowPtr;
    {-Create and display a simple window}
  var
    W : WindowPtr;
  begin
    MakeWindow := Nil;
    if WStackPos = MaxWindowStack then
      Exit;
    New(W);
    with W^ do begin
      WhereXYAbs(LastXY);
      wX1  := X1;
      wY1  := Y1;
      wX2  := X2;
      wY2  := Y2;
      wAttr:= TxtAttr;
      wFramed := Frame;
      if not SaveWindow(X1, Y1, X2, Y2, True, Buf) then
        Exit;
    end;
    if Frame then
      FrameWindow(X1, Y1, X2, Y2, FrameAttr, FrameAttr, Header);
    ActivateWindow(W);
    TextAttr := TxtAttr;
    ClrScr;
    MakeWindow := W;
    Inc(WStackPos);
    WStack[WStackPos] := W;
  end;

  procedure DisposeWindow(W : WindowPtr);
    {-Erase and dispose of a simple window}
  begin
    with W^ do begin
      RestoreWindow(wX1, wY1, wX2, wY2, True, Buf);
      Dec(WStackPos);
      if WStackPos > 0 then
        ActivateWindow(WStack[WStackPos])
      else
        Window(1, 1, ScreenWidth, ScreenHeight);
      GotoXYAbs(LastXY);
    end;
    Dispose(W);
  end;

  procedure Pause;
    {-Wait for a keystroke}
  begin
    if ReadKey = #0 then
      if ReadKey = #0 then ;
  end;

  procedure ShowHelp;
    {-Display a detailed help message}
  var
    W : WindowPtr;
    I : Byte;
  begin
    W := MakeWindow(HX1, HY1, HX2, HY2,
                    HelpAttr, HelpAttr, True,
                    'SimpCom Help');
    if W = Nil then Exit;
    for I := 1 to NumHelpLines do
      FastWrite(HelpLines[I], HY1+I, HX1 + 1, HelpAttr);
    Pause;
    DisposeWindow(W);
  end;

  procedure Message(S : String; WaitForKey, Beep : Boolean; var Key : Char);
    {-Display a message}
  begin
    FastWrite(Center(S, 80), MsgY, 1, MsgAttr);
    if Beep then begin
      Sound(220);
      Delay(500);
      NoSound;
    end;
    if WaitForKey then begin
      Key := ReadKey;
      if Key = #0 then
        Key := ReadKey;
    end;
  end;

  procedure ClearMessage;
    {-Clears the message line}
  var
    K : Char;
  begin
    Message(' ', False, False, K);
  end;

  procedure TempMessage(S : String; Pause : Word);
    {-Display a message, delay Pause ms, then clear message line}
  var
    K : Char;
  begin
    Message(S, False, False, K);
    Delay(Pause);
    ClearMessage;
  end;

  function Prompt(PromptStr : String; MaxLen : Word; var S : String) : Boolean;
    {-Prompt user and return string}
  var
    XY : Word;
    A  : Byte;
    Esc : Boolean;
  begin
    WhereXYAbs(XY);
    ClearMessage;
    A := TextAttr;
    TextAttr := PromptAttr;
    FullWindow;
    GotoXY(1, MsgY);
    Write(PromptStr);
    TextAttr := MsgAttr;
    ReadLine(S, MaxLen, Esc);
    Prompt := (not Esc) and (Length(S) > 0);
    ActivateWindow(WStack[WStackPos]);
    ClearMessage;
    TextAttr := A;
    GotoXYAbs(XY);
  end;

  function YesNo(S : String; DefaultChar : Char) : Boolean;
    {-display a yes/no prompt}
  var
    XY : Word;
    A  : Byte;
    C  : Char;
  begin
    DefaultChar := UpCase(DefaultChar);
    if not (DefaultChar in ['N', 'Y']) then
      Exit;
    WhereXYAbs(XY);
    ClearMessage;
    A := TextAttr;
    TextAttr := PromptAttr;
    FullWindow;
    GotoXY(1, MsgY);
    Write(S);
    TextAttr := MsgAttr;
    Write('[' + DefaultChar + ']');
    GotoXY(WhereX - 2, WhereY);
    C := UpCase(ReadKey);
    case C of
      ^[ : C := 'N';
      ^M : C := DefaultChar;
      #0 : if ReadKey = #0 then ;
    end;
    YesNo := C = 'Y';
    ActivateWindow(WStack[WStackPos]);
    ClearMessage;
    TextAttr := A;
    GotoXYAbs(XY);
  end;

  function KeyboardAbort : Boolean;
    {-Used by protocol upload/downloads to allow user to abort with
      ^C or ESC}
  begin
    KeyboardAbort := False;
    if KeyPressed then
      case ReadKey of
        #0     : if ReadKey = #0 then ;
        ^[, ^C : KeyboardAbort := True;
      end;
  end;

  procedure MyErrorProc(P : Pointer; var StatusCode : Word);
  var
    C : Char;
  begin
    with AbstractPortPtr(P)^ do begin
      {Do nothing if a protocol is in progress}
      if ProtocolInProgress then
        Exit;
      StatusCode := StatusCode mod 10000;
      if StatusCode <> 0 then begin
        {Build an error message}
        Message('Async Error: '+ StatusStr(StatusCode), True, True, C);
        ClearMessage;
        StatusCode := 0;
      end;
    end;
  end;

  procedure GetComParameters(var Port : ComNameType;
                             var Baud : LongInt;
                             var Parity : ParityType;
                             var Bits : DataBitType;
                             var Stop : StopBitType;
                             var TonePulse : TonePulseType;
                             var HWHandshakeOptions : Word;
                             var SWHandshake : Boolean;
                             var PortOptions : Word);
    {-Return the active COMM port, Baud rate, Parity type, data bits, and
      stop bits}
  begin
    with Uart do begin
      GetLine(Baud, Parity, Bits, Stop, False);
      Port := GetComName;
      GetOptions(HWHandshakeOptions, PortOptions, SWHandshake);
    end;
    TonePulse := Modem.GetDialingMode;
  end;

  procedure ConfigRecToPort(var ConfigRec : ComParamRecord);
  begin
    Uart.Done;
    with ConfigRec do begin
      if not Uart.InitCustom(Port, Baud, Parity, Bits, Stop,
                           DefInBufferSize, DefOutBufferSize,
                           HWHOptions, SWFlow,
                           PortOptions) then
        Abort('Error reinitializing TerminalUart');
      Uart.SetErrorProc(MyErrorProc);
      Uart.SetAbortFunc(KeyboardAbort);
      Modem.SetDialingMode(ToneOrPulse);

      {$IFDEF UseUart}
      if ClassifyUart(Uart.GetBaseAddr, False) = U16550A then
        SetFifoBuffering(Uart.GetBaseAddr, True, 4);
      {$ENDIF}
    end;
  end;

  procedure PortToConfigRec(var ConfigRec : ComParamRecord);
  begin
    with ConfigRec do begin
      GetComParameters(Port, Baud, Parity, Bits, Stop, ToneOrPulse,
                       HWHOptions, SWFlow, PortOptions);
      Signature := SimpComSignature;
    end;
  end;

  procedure SaveConfigFile;
  const
    PromptS = 'Enter name of file to save config: ';
    FName : String = '';
  var
    F : File;
    K : Char;
    ConfigRec : ComParamRecord;
  begin
    if Length(FName) = 0 then
      FName := ParamFName;
    if not Prompt(PromptS, 80 - Length(PromptS), FName) then
      Exit;
    Assign(F, FName);
    Rewrite(F, 1);
    if IoResult <> 0 then begin
      Message('Unable to open ' + FName, True, True, K);
      ClearMessage;
      Exit;
    end;
    PortToConfigRec(ConfigRec);
    BlockWrite(F, ConfigRec, SizeOf(ConfigRec));
    if IoResult <> 0 then
      Message('Unable to write to ' + FName, True, True, K);
    Close(F);
    if IoResult = 0 then
      TempMessage('Configuration stored in ' + FName, 1500)
    else
      Message('Unable to close to ' + FName, True, True, K);
    ClearMessage;
  end;

  function ReadConfigFile(FName : PathStr;
                          var ConfigRec : ComParamRecord) : Boolean;
  var
    F : File;
    BytesRead : Word;
  begin
    ReadConfigFile := False;
    Assign(F, FName);
    Reset(F, 1);
    if IoResult <> 0 then
      Exit;
    BlockRead(F, ConfigRec, SizeOf(ConfigRec), BytesRead);
    if (IoResult <> 0) or (BytesRead < SizeOf(ConfigRec)) or
        (ConfigRec.Signature <> SimpComSignature) then
      Exit
    else
      ReadConfigFile := True;
    Close(F);
    if IoResult = 0 then ;
  end;

  procedure LoadConfigFile;
  const
    PromptS = 'Enter name of config file to load: ';
    FName : String = '';
  var
    K : Char;
    ConfigRec : ComParamRecord;
  begin
    if Length(FName) = 0 then
      FName := ParamFName;
    if not Prompt(PromptS, 80 - Length(PromptS), FName) then
      Exit;
    if ReadConfigFile(FName, ConfigRec) then
      TempMessage('Configuration read from ' + FName, 1500)
    else begin
      Message('Unable to read ' + FName, True, True, K);
      Exit;
    end;
    ClearMessage;
    ConfigRecToPort(ConfigRec);
  end;

  procedure DisplayStatus;
    {-Display a status line}
  var
    Port : ComNameType;
    Baud : LongInt;
    DataBits : DataBitType;
    StopBits : StopBitType;
    Parity : ParityType;
    DTR, RTS, SWF : Boolean;
    S : String;
    HWO, PO : Word;
    TP : TonePulseType;
  begin
    {Note current line parameters}
    GetComParameters(Port, Baud, Parity, DataBits, StopBits, TP, HWO, SWF, PO);
    {Get the current modem control settings}
    Uart.GetModem(DTR, RTS);
    S := ComNameString(Port) + ' opened at ' + Long2Str(Baud) +  ' ';
    S := S + ParityChar[Parity] + ' ' + Byte2Str(DataBits) + ' ';
    S := S + Byte2Str(StopBits) + ' DTR is ' + OnOff[DTR];
    S := S + ' RTS is ' + OnOff[RTS];
    if AnsiMode then
      S := S + '  ANSI is ON';
    if Capture.CaptureEnabled then
      S := S + '  CAPTURE is ON';
    {$IFDEF UseUart}
    if FifoStatus(Uart.GetBaseAddr) then
      S := S + '  FIFO';
    {$ENDIF}
    S := Pad(S, 80);
    FastWrite(S, StatusY, 1, StatusAttr);
  end;

  procedure ReadComParameters(FName : PathStr;
                              var Port : ComNameType;
                              var Baud : LongInt;
                              var Parity : ParityType;
                              var Bits : DataBitType;
                              var Stop : StopBitType;
                              var TP : TonePulseType;
                              var HWO : Word;
                              var SWF : Boolean;
                              var PO : Word);
    {-Read the communications parameters from config file}
  var
    Config : ComParamRecord;
    K : Char;
  begin
    if FileExists(FName) then begin
      Message('Reading config file: '+FName, False, False, K);
      if ReadConfigFile(FName, Config) then begin
        Port     := Config.Port;
        Baud     := Config.Baud;
        Parity   := Config.Parity;
        Bits     := Config.Bits;
        Stop     := Config.Stop;
        TP       := Config.ToneOrPulse;
        HWO      := Config.HWHOptions;
        SWF      := Config.SWFlow;
        PO       := Config.PortOptions;
        Delay(1000);
        ClearMessage;
      end
      else
        Abort('Error reading '+FName);
    end
    else begin
      TempMessage('No config file found, using defaults', 2000);
      Port := DefPort;
      Baud := DefBaud;
      Parity := DefParity;
      Bits := DefBits;
      Stop := DefStop;
      TP   := Default;
      HWO  := DefHardWareOptions;
      SWF  := DefSoftwareFlow;
      PO   := DefPortOptions;
    end;
  end;

  procedure Call;
    {-Call/Dialer}
  const
    PhoneNo : String = '';

  begin
    if Prompt('Enter number to dial: ', MaxDialStr, PhoneNo) then
      Modem.DialModem(PhoneNo);
  end;

  procedure SetParameters; Forward;
    {-Forward definition so SetOptions and SetParameters can call each other}

  procedure SetOptions;
    {-Set various SIMPCOM options}
    procedure DrawOptionsScr;
    begin
      WriteLn;
      WriteLn('  Toggle assorted options:');
      WriteLn('    A - ANSI mode              [', OnOff[AnsiMode],']');
      WriteLn('    O - Show Outgoing chars    [', OnOff[ShowOutgoingChars], ']');
      WriteLn('    C - Convert CR to CR/LF    [', OnOff[Cr2CrLf], ']');
      WriteLn('    I - Include directories    [', OnOff[IncludeDirs], ']');
      WriteLn('    H - Hardware flow control  [', OnOff[Uart.HWHState <> 0], ']');
      WriteLn('    S - Software flow control  [', OnOff[Uart.SWFlow], ']');
      WriteLn('    P - Default, Tone or Pulse [', TonePulse[Modem.GetDialingMode], ']');
      WriteLn;
      WriteLn('    T - Clear terminal window');
    end;
  var
    OptWin : WindowPtr;
    C : Char;
  begin
    OptWin := MakeWindow(OX1, OY1, OX2, OY2,
                         OAttr, OAttr, True, 'Options');
    if OptWin = Nil then
      Exit;

    repeat
      ClrScr;
      DrawOptionsScr;
      C := ReadKey;
      if C = #0 then begin
        C := ReadKey;
        case Ord(C) of
          F1 : ShowHelp;
          F2 : SetParameters;
        end;
      end
      else
        case UpCase(C) of
          'A' : AnsiMode := not AnsiMode;
          'O' : ShowOutGoingChars := not ShowOutGoingChars;
          'C' : Cr2CrLf := not Cr2CrLf;
          'I' : IncludeDirs := not IncludeDirs;
          'P' :
            with Modem do
              case GetDialingMode of
                Default : SetDialingMode(Tone);
                Tone    : SetDialingMode(Pulse);
                Pulse   : SetDialingMode(Default);
              end;

          'T' :
            begin
              DisposeWindow(OptWin);
              ActivateWindow(TermWin);
              ClrScr;
              OptWin := MakeWindow(OX1, OY1, OX2, OY2,
                                   OAttr, OAttr, True, 'Set Options');
              if OptWin = Nil then
                Exit;
            end;
          {$IFDEF UseSWFlow}
          'S' : with Uart do
                  SetSoftwareFlowControl(not SWFlow);
          {$ENDIF}
          'H' : with Uart do
                  if HWHState = 0 then
                    SetHardwareFlowControl(HardwareFlowOn)
                  else
                    SetHardwareFlowControl(0);
        end;
        DisplayStatus;
    until C = ^[;
    DisposeWindow(OptWin);
  end;

  procedure SetParameters;
    {-Enter set com parameter mode}

    procedure ErrorMsg(S : String);
      {-Display an error message}
    var
      K : Char;
    begin
      Message(S, True, True, K);
      ClearMessage;
    end;

    procedure SetBaudRate;
      {-Select the baud rate}
    var
      C : Char;
      Keys : Set of Char;
      Done : Boolean;
    begin
      ClrScr;
      WriteLn;
      WriteLn('  Possible baud rates:');
      WriteLn('      1 -     300');
      WriteLn('      2 -    1200');
      WriteLn('      3 -    2400');
      WriteLn('      4 -    4800');
      WriteLn('      5 -    9600');
      WriteLn('      6 -   19200');
      WriteLn('      7 -   38400');
      WriteLn('      8 -   57600');
      WriteLn('      9 -  115200');
      Write('  Enter the number of baud rate: ');
      Keys := ['1'..'9',^[];
      repeat
        C := ReadKey;
        if C = #0 then
          C := ReadKey;
        Done := C in Keys;
      until Done;

      with Uart do
        case C of
          '1' : ChangeBaud(300);
          '2' : ChangeBaud(1200);
          '3' : ChangeBaud(2400);
          '4' : ChangeBaud(4800);
          '5' : ChangeBaud(9600);
          '6' : ChangeBaud(19200);
          '7' : ChangeBaud(38400);
          '8' : ChangeBaud(57600);
          '9' : ChangeBaud(115200);
        end
    end;

    procedure SetParityType;
      {-Select the parity type}
    var
      C : Char;
      Keys : Set of Char;
      Done : Boolean;
    begin
      ClrScr;
      WriteLn;
      WriteLn('  Possible parity types:');
      WriteLn('     N - No parity');
      WriteLn('     O - Odd parity');
      WriteLn('     E - Even parity');
      WriteLn('     M - Mark parity');
      WriteLn('     S - Space parity');
      Write('  Enter parity type: ');
      Keys := ['N','O', 'E', 'M', 'S',^[];
      repeat
        C := UpCase(ReadKey);
        if C = #0 then
          C := ReadKey;
        Done := C in Keys;
      until Done;

      with Uart do
        case C of
          'N' : ChangeParity(NoParity);
          'O' : ChangeParity(OddParity);
          'E' : ChangeParity(EvenParity);
          'M' : ChangeParity(MarkParity);
          'S' : ChangeParity(SpaceParity);
        end;
    end;

    procedure SetDataBits;
      {-Select data bits}
    var
      C : Char;
      Keys : Set of Char;
      Done : Boolean;
    begin
      ClrScr;
      WriteLn;
      WriteLn('  Possible data bits:');
      WriteLn('      5 data bits');
      WriteLn('      6 data bits');
      WriteLn('      7 data bits');
      WriteLn('      8 data bits');
      Write('  Enter number of data bits: ');
      Keys := ['5'..'8', ^[];
      repeat
        C := ReadKey;
        if C = #0 then
          C := ReadKey;
        Done := C in Keys;
      until Done;
      case C of
        '5'..'8' :
          Uart.ChangeDataBits(DataBitType(Ord(C) - Ord('0')));
      end;
    end;

    procedure SetStopBits;
      {-Select stop bits}
    var
      C : Char;
      Keys : Set of Char;
      Done : Boolean;
    begin
      ClrScr;
      WriteLn;
      WriteLn('  Possible stop bits:');
      WriteLn('      1 stop bit');
      WriteLn('      2 stop bits');
      Write('  Enter number of stop bits: ');
      Keys := ['1', '2', ^[];
      repeat
        C := ReadKey;
        if C = #0 then
          C := ReadKey;
        Done := C in Keys;
      until Done;
      case C of
        '1', '2' :
          Uart.ChangeStopBits(StopBitType(Ord(C) - Ord('0')))
      end;
    end;

    procedure SetComPort;
      {-Select the COMM port}
    var
      PS2 : Boolean;
      Value : Integer;
      C, SavePort : ComNameType;
      B            : LongInt;
      P            : ParityType;
      D            : DataBitType;
      SB           : StopBitType;
      InS, OutS    : Word;
      HWO, PO      : Word;
      SWO          : Boolean;
      Ch           : Char;
      Keys         : Set of Char;
      Done         : Boolean;
    begin
      PS2 := IsPS2;
      ClrScr;
      WriteLn;
      WriteLn('  Possible COM ports:');
      WriteLn('    1 - COM1');
      WriteLn('    2 - COM2');
      WriteLn('    3 - COM3');
      WriteLn('    4 - COM4');
      if PS2 then begin
        WriteLn('    5 - COM5');
        WriteLn('    6 - COM6');
        WriteLn('    7 - COM7');
        WriteLn('    8 - COM8');
        Keys := ['1'..'8', ^[];
      end
      else
        Keys := ['1'..'4', ^[];
      Write('  Enter number of desired port: ');
      repeat
        Ch := ReadKey;
        if Ch = #0 then
          Ch := ReadKey;
        Done := Ch in Keys;
      until Done;
      if Ch = ^[ then
        Exit;
      {enumerated types start with value of 0}
      Value := Ord(Ch) - Ord('1');
      C := ComNameType(Value);
      {only do the work if the user picked a com port other than the
       currently active one}
      SavePort := Uart.GetComName;
      if C <> SavePort then begin
        {save the current line parameters}
        Uart.GetLine(B, P, D, SB, False);
        Uart.GetBufferSizes(InS, OutS);
        Uart.GetOptions(HWO, PO, SWO);
        {destruct port object}
        Uart.Done;
        {construct new port object}
        if not Uart.InitCustom(C, B, P, D, SB,
                               InS, OutS,
                               HWO, SWO, PO) then begin
          ErrorMsg('Unable to open ' + ComNameString(C));
          Uart.InitCustom(SavePort, B, P, D, SB,
                          InS, OutS, HWO, SWO, PO);
        end;
        Uart.SetAbortFunc(KeyboardAbort);
        Uart.SetErrorProc(MyErrorProc);
        {fixup the port pointer in the Modem object}
        Modem.APort := @Uart;
      end;
      {$IFDEF UseUart}
      if ClassifyUart(Uart.GetBaseAddr, False) = U16550A then
        SetFifoBuffering(Uart.GetBaseAddr, True, 4);
      {$ENDIF}
    end;

    procedure DrawParamScr;
      {-Draw parameter prompt}
    var
      C            : ComNameType;
      B            : LongInt;
      P            : ParityType;
      D            : DataBitType;
      S            : StopBitType;
      HWO, PO      : Word;
      SWF          : Boolean;
      TP           : TonePulseType;
    begin
      GetComParameters(C, B, P, D, S, TP, HWO, SWF, PO);
      WriteLn;
      WriteLn('  Select the parameter to set:');
      WriteLn('      B - to set baud        ', B);
      WriteLn('      P - to set parity      ', ParityChar[P]);
      WriteLn('      D - to set data bits   ', D );
      WriteLn('      S - to set stop bits   ', S);
      WriteLn('      C - COM port           ', ComNameString(C));
      WriteLn;
      WriteLn('  Or press:');
      WriteLn('      X - Save parameters');
      WriteLn('      L - Load parameters');
    end;

  var
    ParamWin : WindowPtr;
    C : Char;
  begin
    ParamWin := MakeWindow(PX1, PY1, PX2, PY2,
                           PAttr, PAttr, True, 'Port Parameters');
    if ParamWin = Nil then
      Exit;

    repeat
      ClrScr;
      DrawParamScr;
      C := ReadKey;
      if C = #0 then begin
        C := ReadKey;
        case Ord(C) of
          F1 : ShowHelp;
          F5 : SetOptions;
        end;
      end
      else
        case UpCase(C) of
          'B' : SetBaudRate;
          'P' : SetParityType;
          'D' : SetDataBits;
          'S' : SetStopBits;
          'C' : SetComPort;
          'X' : SaveConfigFile;
          'L' : LoadConfigFile;
          else if C <> ^[ then
            ErrorBeep;
        end;
        DisplayStatus;
    until C = ^[;
    DisposeWindow(ParamWin);
  end;

  procedure ToggleFileCapture;
    {-Toggle the state of file capture}
  const
    FName : String = '';
  var
    K : Char;
    AppendToFile : Boolean;
  begin
    with Capture do begin
      if CaptureEnabled then begin
        if not YesNo('Close the capture file? ', 'Y') then
          Exit;
        captureClose;
        if captureIoRes = 0 then
          TempMessage(captureFileName + ' closed', 1000)
        else begin
          Message('Error ' + Long2Str(captureIoRes) + ' closing ' +
                  captureFileName,
                  True, True,K);
          ClearMessage;
        end;
      end
      else begin

        if Prompt('Enter capture file name:', 50, FName) then begin
          AppendToFile := False;
          if FileExists(FName) then
            if YesNo(FName + ' exists, append? ', 'Y') then
              AppendToFile := True
            else
              if not YesNo('Overwrite it? ', 'N') then
                Exit;
          captureOpen(FName, AppendToFile);
          if captureIoRes <> 0 then begin
            Message('Error ' + Long2Str(captureIoRes) + ' opening ' + FName,
                    True, True,K);
            ClearMessage;
            Exit;
          end;
          TempMessage('File capture now enabled to ' + CaptureFileName, 1500);
        end;
      end;
    end;
    DisplayStatus;
  end;

  procedure UpdateProgressBar(Row, Col, Len : Byte; Percent : Real);
    {-Fills in a progress bar with Percent complete}
  const
    CompleteChar = '�';
  var
    CharPercent : Real;
    CharCount : Byte;
    BarStr : String;
  begin
    {Calculate "percent value" of each character space}
    CharPercent := 100.0 / Len;

    {Calculate how many chars we need to approach (but not exceed) Percent}
    CharCount := Trunc((Percent * 100) / CharPercent);

    {Make sure we don't go past Len}
    if CharCount > Len then
      CharCount := Len;

    {Write out the complete bar}
    FillChar(BarStr[1], CharCount, CompleteChar);
    BarStr[0] := Char(CharCount);
    if CharCount <> 0 then
      FastWrite(BarStr, Row, Col, DAttr);
  end;

  procedure UpdateStatusMsg(Row, Col, Len : Byte);
    {-Translate the current AsyncStatus into a status message}
  const
    LastStatus : Word = 65535;
    MaxMsgLen = 40;
  var
    Msg : String;
  begin
    if AsyncStatus <> LastStatus then begin
      FillChar(Msg[1], MaxMsgLen, ' ');
      Msg[0] := Char(MaxMsgLen);
      FastWrite(Msg, Row, Col, DAttr);
      Msg := StatusStr(AsyncStatus);
      FastWrite(Msg, Row, Col, DAttr);
    end;
  end;

  procedure XferShowStatus(AP : AbstractProtocolPtr;
                           Starting, Ending : Boolean);
    {-Show the status of a protocol upload/download}
  const
    XLow = 10;
    YLow = 4;
    XHigh = 69;
    YHigh = 22;
    DividerBar = '쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑';
    NewProgBar = '같같같같같같같같같같같같같같같같같같같같';
    HeaderStr : array[TransferModeType] of String[19] =
      (' Protocol Upload ', ' Protocol Download ');
    ModeStr : array[TransferModeType] of String[9] =
      ('sent:', 'received:');
    OnOffStr : array[Boolean] of String[3] = ('Off', ' On');
  var
    Blocks : Integer;
    Efficiency, MaxCPS, ActualCPS, R : Real;
    CurBlockSize : Word;
    CurFileSize : LongInt;
    CurBytesRemaining : LongInt;
    CurBytesTransferred : LongInt;
    CurProtocol : Byte;
    CurElapsedTics : LongInt;
    CurBlock : Word;
    S : String;
    I : Word;
    B : Boolean;
    Size : Byte;
  const
    W : WindowPtr = Nil;
  begin
    if Starting then begin
      {Build and frame the window}
      W := MakeWindow(XLow, YLow, XHigh, YHigh,
                      WAttr, FAttr, True, HeaderStr[TransferMode]);
      if W = nil then
        Abort('Insufficient memory ');

      {Write out the fixed text strings}
      FastWrite('Protocol:', YLow+1, XLow+2, WAttr);
      FastWrite('Block check:', YLow+2, XLow+2, WAttr);
      FastWrite('File name:', YLow+3, XLow+2, WAttr);
      FastWrite('File size:', YLow+4, XLow+2, WAttr);
      FastWrite('Block size:', YLow+5, XLow+2, WAttr);
      FastWrite('Total blocks:', YLow+6, XLow+2, WAttr);

      FastWrite('Est. time:', YLow+8, XLow+2, WAttr);
      FastWrite('Elapsed time:', YLow+9, XLow+2, WAttr);
      FastWrite('Remaining time:', YLow+10, XLow+2, WAttr);

      FastWrite('Bytes '+ModeStr[TransferMode], YLow+1, XLow+33, WAttr);
      FastWrite('Bytes remaining:', YLow+2, XLow+33, WAttr);
      FastWrite('Blocks '+ModeStr[TransferMode], YLow+3, XLow+33, WAttr);
      FastWrite('Blocks remaining:', YLow+4, XLow+33, WAttr);
      FastWrite('Block errors:', YLow+5, XLow+33, WAttr);
      FastWrite('Total errors:', YLow+6, XLow+33, WAttr);

      FastWrite('Throughput:', YLow+8, XLow+33, WAttr);
      FastWrite('Efficiency:', YLow+9, XLow+33, WAttr);

      {If Kermit then show sliding window status}
      if AP^.GetProtocol = Kermit then
        FastWrite('Windows Max/Used:', YLow+10, XLow+33, WAttr);

      FastWrite('Progress:', YLow+12, XLow+2, WAttr);
      FastWrite('Status:', YLow+13, XLow+2, WAttr);

      FastWrite(DividerBar, YLow+14, XLow, FAttr);
      FastWrite('Baud:', YLow+15, XLow+2, WAttr);
      FastWrite('DataBits:', YLow+16, XLow+2, WAttr);
      FastWrite('Sfw Flow:', YLow+17, XLow+2, WAttr);

      FastWrite('StopBits:', YLow+15, XLow+33, WAttr);
      FastWrite('Parity:', YLow+16, XLow+33, WAttr);
      FastWrite('Hdw Flow:', YLow+17, XLow+33, WAttr);

      {Only update the port status on startup}
      with AP^.APort^.PR^ do begin
        FastWrite(LeftPad(Long2Str(CurBaud), 8), YLow+15, XLow+18, DAttr);
        FastWrite(LeftPad(Long2Str(CurDataBits), 8), YLow+16, XLow+18, DAttr);
        {$IFDEF UseSWFlow}
        B := AP^.APort^.SWFlowState <> fsOff;
        {$ELSE}
        B := False;
        {$ENDIF}
        FastWrite(OnOffStr[B], YLow+17, XLow+23, DAttr);
        FastWrite(LeftPad(Long2Str(CurStopBits), 8), YLow+15, XLow+50, DAttr);
        FastWrite(LeftPad(ParityString[CurParity], 8), YLow+16, XLow+50, DAttr);
        B := AP^.APort^.HWFlowState <> fsOff;
        FastWrite(OnOffStr[B], YLow+17, XLow+55, DAttr);
      end;
    end;

    {Update the data areas}
    with AP^ do begin
      {Store common status info in local variables}
      CurBlockSize := GetBlockSize;
      CurFileSize := GetFileSize;
      CurBytesRemaining := GetBytesRemaining;
      CurBytesTransferred := GetBytesTransferred;
      CurProtocol := GetProtocol;
      CurElapsedTics := GetElapsedTics;
      CurBlock := GetBlockNum;

      {Protocol and file name}
      FastWrite(ProtocolTypeString[CurProtocol], YLow+1, XLow+18, DAttr);

      case GetCheckType of
        bcChecksum1 : S := bcsChecksum1;
        bcChecksum2 : S := bcsChecksum2;
        bcCrc16     : S := bcsCrc16;
        bcCrc32     : S := bcsCrc32;
        bcCrcK      : S := bcsCrcK;
        else S := '';
      end;

      FastWrite(S, YLow+2, XLow+18, DAttr);
      FastWrite(Pad(StUpcase(GetFileName), 12), YLow+3, XLow+18, DAttr);

      {File size, block size, block check and total blocks}
      FastWrite(LeftPad(Long2StrBlank(CurFileSize),8), YLow+4, XLow+18, DAttr);
      FastWrite(LeftPad(Long2Str(CurBlockSize),8), YLow+5, XLow+18, DAttr);
      if CurFileSize = 0 then
        I := 0
      else
        I := Succ(CurFileSize div CurBlockSize);
      FastWrite(LeftPad(Long2StrBlank(I),8), YLow+6, XLow+18, DAttr);

      {Estimated time, elapsed time and time remaining}
      FastWrite(FormatMinSec(EstimateTransferSecs(CurFileSize)),
                YLow+8, XLow+18, DAttr);
      FastWrite(FormatMinSec(Tics2Secs(CurElapsedTics)), YLow+9, XLow+18, DAttr);
      FastWrite(FormatMinSec(EstimateTransferSecs(CurBytesRemaining)),
                YLow+10, XLow+18, DAttr);

      {Bytes transferred and bytes remaining}
      FastWrite(LeftPad(Long2Str(CurBytesTransferred),8), YLow+1, XLow+50, DAttr);
      FastWrite(LeftPad(Long2StrBlank(CurBytesRemaining),8), YLow+2, XLow+50, DAttr);

      {Blocks transferred and blocks remaining}
      FastWrite(LeftPad(Long2Str(CurBlock),8), YLow+3, XLow+50, DAttr);
      Blocks := (CurBytesRemaining+Pred(CurBlockSize)) div CurBlockSize;
      FastWrite(LeftPad(Long2StrBlank(Blocks),8), YLow+4, XLow+50, DAttr);

      {Error counts}
      FastWrite(LeftPad(Long2Str(GetBlockErrors),8), YLow+5, XLow+50, DAttr);
      FastWrite(LeftPad(Long2Str(GetTotalErrors),8), YLow+6, XLow+50, DAttr);

      {Display an empty progress bar on startup}
      if CurBytesTransferred = 0 then
        FastWrite(NewProgBar, YLow+12, XLow+18, DAttr);

      {Update the progress bar (if the file size is known}
      if CurFileSize <> 0 then begin
        R := CurBytesRemaining;
        R := R / CurFileSize;
      end else
        R := 1;
      UpdateProgressBar(YLow+12, XLow+18, Length(NewProgBar), 1.0 - R);

      {Update status message}
      UpdateStatusMsg(YLow+13, XLow+18, 35);

      {Calculate and display throughput}
      if CurElapsedTics > 0 then begin
        R := CurBytesTransferred;
        ActualCPS := R / (CurElapsedTics / 18.2);
      end else
        ActualCPS := 0.0;
      FastWrite(LeftPad(Long2Str(Trunc(ActualCPS))+' CPS',9),
                YLow+8, XLow+49, DAttr);

      {Calculate and display efficiency}
      if Modem.GetConnectSpeed <> 0 then
        MaxCPS := Modem.GetConnectSpeed div 10
      else
        MaxCPS := APort^.PR^.CurBaud div 10;
      if MaxCPS > 0 then
        Efficiency := (ActualCPS / MaxCPS) * 100.0
      else
        Efficiency := 0.0;
      FastWrite(Real2Str(Efficiency, 7, 0)+'%', YLow+9, XLow+50, DAttr);

      {If protocol is Kermit then show sliding window status}
      if CurProtocol = Kermit then
        with KermitProtocolPtr(AP)^ do begin
          Size := GetSwcSize;
          if Size = 0 then
            S := '0/0'
          else
            S := Long2Str(Size) + '/' + Long2Str(WindowsUsed);
          FastWrite(LeftPad(S, 5), YLow+10, XLow+53, DAttr);
        end;
    end;

    {Remove the window on the last status call}
    if Ending then
      DisposeWindow(W);
  end;

  function SelectProtocol(Up : Boolean) : AbstractProtocolPtr;
    {-Select a upload/download protocol}
  var
    P : AbstractProtocolPtr;
    W : WindowPtr;
    C : Char;
    Keys : Set of Char;
    Done : Boolean;
  begin
    SelectProtocol := Nil;
    W := MakeWindow(SPX1, SPY1, SPX2, SPY2, SPAttr, SPAttr, True,
                    'Select ' + ProtoType[Up] + ' Protocol');
    if W = Nil then
      Exit;
    WriteLn;
    WriteLn('  Available Protocols:');
    WriteLn('    1 - XMODEM');
    WriteLn('    2 - XMODEM  1K');
    WriteLn('    3 - XMODEM  1KG');
    WriteLn('    4 - YMODEM  (batch)');
    WriteLn('    5 - YMODEMG (batch)');
    WriteLn('    6 - ZMODEM  (batch)');
    WriteLn('    7 - KERMIT  (batch)');
    WriteLn('    8 - ASCII');
    Write('  Enter choice: ');
    Keys := ['1'..'8',^[];
    repeat
      C := ReadKey;
      if C = #0 then
        C := ReadKey;
      Done := C in Keys;
      if not Done then
        ErrorBeep;
    until Done;
    DisposeWindow(W);
    case C of
      '1', '2', '3' :
        begin
          P := New(XModemProtocolPtr, Init(@Uart, C = '2', C = '3'));
          if P <> Nil then
            with XModemProtocolPtr(P)^ do begin
              SetBlockWait(RelaxedBlockWait);
              SetHandshakeWait(DefHandShakeWait, 0);
            end;
        end;
      '4', '5' :
        begin
          P := New(YModemProtocolPtr, Init(@Uart, True, C = '5'));
          if P <> Nil then
            with YModemProtocolPtr(P)^ do begin
              SetBlockWait(RelaxedBlockWait);
              SetHandshakeWait(DefHandShakeWait, 0);
            end;
        end;
      '6' :
        begin
          P := New(ZModemProtocolPtr, Init(@Uart));
          if P <> Nil then
            with ZModemProtocolPtr(P)^ do begin
              SetFileMgmtOptions(True, False, WriteClobber);
            end;

        end;
      '7' :
        begin
          P := New(KermitProtocolPtr, Init(@Uart));
          (* ucomment for long block and/or sliding widows}
          KermitProtocolPtr(P)^.SetMaxLongPacketLen(500);
          KermitProtocolPtr(P)^.SetMaxWindows(25);
          *)
        end;
      '8' :
        begin
          P := New(AsciiProtocolPtr, Init(@Uart));
          if P <> Nil then
            with AsciiProtocolPtr(P)^ do begin
              SetDelays(AsciiInterChar, AsciiInterLine);
            end;
        end;
      else P := Nil;
    end;
    if P <> Nil then
      P^.SetShowStatusProc(XferShowStatus);
    SelectProtocol := P;
  end;

  procedure Upload;
    {-Upload a file using a protocol}
  const
    FName : String = '';
  var
    Proto : AbstractProtocolPtr;
    K : Char;
    Switch : SwitchToBinary;

    procedure Error(S : String);
      {-Dispose of protocol object and display error message}
    begin
      Dispose(Proto, Done);
      Message(S, True, True, K);
      ClearMessage;
    end;

  begin
    Proto := SelectProtocol(True);
    if Proto = Nil then
      Exit;
    if Prompt('Enter file to upload: ', 50, FName) then begin
      if HasWildcards(FName) then begin
        if not Proto^.SupportsBatch then begin
          {if wildcards in filename, then must be a batch supporting protocol}
          Error('The protocol you selected does not support batch transfers');
          Exit;
        end;
      end
      else
        {not batch, so check to insure file exists}
        if not FileExists(FName) then begin
          Error(FName + ' does not exist');
          Exit;
        end;
      TransferMode := Transmit;
      if TypeOf(Proto^) <> TypeOf(KermitProtocol) then
        Switch.Init(@Uart);
      with Proto^ do begin
        SetFileMask(FName);
        if SupportsBatch then
          if IncludeDirs then
            apOptionsOn(apIncludeDirectory)
          else
            apOptionsOff(apIncludeDirectory);
        if Modem.GetConnectSpeed <> 0 then
          SetActualBPS(Modem.GetConnectSpeed);
        ProtocolTransmit;
        if AsyncStatus = ecOK then
          TempMessage('Upload complete', 2000)
        else
          Message('Upload failed: ' + StatusStr(AsyncStatus),
                  True, True, K);
        if TypeOf(Proto^) <> TypeOf(KermitProtocol) then
          Switch.Done;
        Dispose(Proto, Done);
      end;
    end;
    ClearMessage;
  end;

  procedure Download;
    {-Download a file using a protocol}
  const
    FName : String = '';
  var
    Proto : AbstractProtocolPtr;
    K : Char;
    Switch : SwitchToBinary;

  begin
    Proto := SelectProtocol(False);
    if Proto = Nil then
      Exit;
    if not Proto^.SupportsBatch then begin
      {if this isn't a batch protocol, we need a filename}
      if Prompt('Enter name for file: ', 50, FName) then begin
        if FileExists(FName) then
          if not YesNo(FName + ' already exists, overwrite? ', 'N') then begin
            Dispose(Proto, Done);
            Exit;
          end;
      end;
    end
    else
      FName := '';
    TransferMode := Receive;
    if TypeOf(Proto^) <> TypeOf(KermitProtocol) then
      Switch.Init(@Uart);
    with Proto^ do begin
      {always overwrite}
      SetReceiveFilename(FName);
      SetOverwriteOption(WriteAnyway);
      ProtocolReceive;
      if AsyncStatus = ecOK then
        TempMessage('Download complete', 2000)
      else begin
        Message('Download failed: ' + StatusStr(AsyncStatus),
                True, True, K);
      end;
      if TypeOf(Proto^) <> TypeOf(KermitProtocol) then
        Switch.Done;
      Dispose(Proto, Done);
    end;
    ClearMessage;
  end;

  procedure HangUp;
    {-Hang up the modem}
  begin
    if YesNo('Hangup modem? ', 'Y') then
      Modem.HangupModem(0, True);
  end;

  procedure ExecShell;
    {-Execute a plain DOS shell}
  var
    PSR : PortSaveRec;
    Buf : Pointer;
    LastXY : Word;
  begin
    {Save window and clear the screen}
    WhereXYAbs(LastXY);
    if not SaveWindow(1, 1, ScreenWidth, ScreenHeight, True, Buf) then
      Exit;
    Window(1, 1, ScreenWidth, ScreenHeight);
    ClrScr;

    {Turn off interrupts and save the ComPort}
    Uart.SavePort(PSR);
    Uart.DeactivatePort(True);

    {Shell to DOS}
    Exec(GetEnv('COMSPEC'), '');

    {Restore the ComPort and turn on interrupts}
    Uart.RestorePort(PSR);
    Uart.ActivatePort(True);

    {Restore screen contents, window coords and cursor position}
    RestoreWindow(1, 1, ScreenWidth, ScreenHeight, True, Buf);
    if WStackPos > 0 then
      with wStack[WStackPos]^ do
        Window(wX1, wY1, wX2, wY2)
    else
      Window(1, 1, ScreenWidth, ScreenHeight);
    GotoXYAbs(LastXY);
  end;

  procedure StartBPlus;
  begin
    if BPlusStarted then
      Exit;
    New(BP, Init(@Uart));
    if BP <> nil then
      BPlusStarted := True;
    BP^.SetShowStatusProc(XferShowStatus);
  end;

  procedure TerminalMode;
    {-get and process user keystrokes}
  var
    C  : Char;
    Finished, Literal : Boolean;
    SendCode : Boolean;
    Start, IsUpload : Boolean;
    Index : Byte;
  begin
    {Simple terminal}
    TermWin := MakeWindow(TX1, TY1, TX2, ScreenHeight-3,
                          TAttr, TAttr, False, 'Terminal');
    if TermWin = NIL then
      Exit;
    Finished := False;
    DisplayStatus;
    Index := 0;

    {loop processing both incoming and outgoing characters}
    repeat
      if not AnsiMode then
        TextAttr := OurTextAttr;
      {Process chars received}
      if Uart.CharReady then begin
        Uart.GetChar(C);
        {Process character}
        if AsyncStatus = ecOk then begin
          {Check for B+ requests}
          case C of
            cENQ : begin
                     if not BPlusStarted then
                       StartBPlus;
                     BP^.ProcessENQ;
                   end;
            cDLE : begin
                     if not BPlusStarted then
                       StartBPlus;
                     BP^.ProcessDLE(Start, IsUpload);
                     if Start then begin
                       if IsUpload then
                         BP^.ProtocolTransmit
                       else
                         BP^.ProtocolReceive;
                       Dispose(BP, Done);
                       BPlusStarted := False;
                     end;
                   end;
            else begin
              {Check for CompuServe Interogation Sequence}
              if CheckForString(Index, C, cEsc+'I', False) then begin
                if not BPlusStarted then
                  StartBPlus;
                BP^.ProcessESCI(80, 25);
              end else begin
                {write the character to the terminal window}
                if AnsiMode then
                  WriteCharAnsiPort(Uart.PR, C)
                else
                  Write(C);
                {if Capture is on, then write character to capture file}
                if Capture.CaptureEnabled then
                  Capture.capturePut(C);
                {should we convert CRs to CR/LFs?}
                if (C = ^M) and Cr2CrLf then begin
                  Write(^J);
                  {also send line feed to capture file if necessary}
                  if Capture.CaptureEnabled then
                    Capture.capturePut(^J);
                end;
              end;
            end;
          end;
        end;
      end;

      {Process chars to send}
      if KeyPressed then begin
        SendCode := False;
        C := ReadKey;
        if C = #0 then begin
          {extended key sequence}
          C := ReadKey;
          Literal := False;
          {check to see if it is a hotkey}
          case Ord(C) of
            F1 : ShowHelp;
            F2 : SetParameters;
            F3 : Call;
            F4 : ToggleFileCapture;
            F5 : SetOptions;
            F6 : HangUp;
            PgUp : Upload;
            PgDn : Download;
{$IFDEF Tracing}
            F9 :
              begin
                InitTracing(10000);
                Sound(110);
                Delay(100);
                NoSound;
              end;
            F10:
              begin
                DumpTraceHex('SIMPCOM.TRC');
                Sound(1100);
                Delay(100);
                NoSound;
              end;
{$ENDIF}
            AltD : ExecShell;
            AltX : Finished := True;
            else begin
              {Send all extended keys in psuedo "doorway mode"}
              Literal := False;
              SendCode := True;
            end;
          end;
        end
        else
          Literal := True;   {this is a literal character to send}
        {does the user want out?}
        if Finished then
          Finished := YesNo('Do you really want to quit? ', 'N');
        if Literal and Uart.TransReady then begin
          {send the character}
          Uart.PutChar(C);
          if ShowOutGoingChars then begin
            Write(C);
            if (C = ^M) and Cr2CrLf then
              Write(^J);
          end;
        end;
        if SendCode and Uart.TransReady then begin
          {Send scan code and key code (psuedo "doorway mode")}
          Uart.PutChar(#0);
          Uart.PutChar(C);
        end;
      end;
    until Finished;
    DisposeWindow(TermWin);                                            {!!.01}
  end;

  procedure Initialize;
    {-Read parameters and init port object}
  var
    Port : ComNameType;
    Baud : LongInt;
    Parity : ParityType;
    Bits : DataBitType;
    Stop : StopBitType;
    HWO, PO : Word;
    SWF : Boolean;
    TP : TonePulseType;
    Temp : String;                                                     {!!.01}
    PortParm : Byte;                                                   {!!.01}
  begin
    {!!.01 rewritten}
    PortParm := 0;
    if ParamCount > 0 then begin
      Temp := ParamStr(1);
      if (Length(Temp) = 1) and (Temp[1] >= '1') and (Temp[1] <= '8') then
        PortParm := Ord(Temp[1]) - $31
      else begin
        ParamFName := Temp;
        if ParamCount > 1 then begin
          Temp := ParamStr(2);
          case Temp[1] of
            '1'..'8' : PortParm := Ord(Temp[1]) - $31;
          end;
        end;
      end;
    end;

    ReadComParameters(ParamFName, Port, Baud, Parity, Bits, Stop,
                      TP, HWO, SWF, PO);

    if PortParm <> 0 then                                              {!!.01}
      Port := ComNameType(PortParm);                                   {!!.01}

    {allocate heap space for capture buffer}
    if not Capture.Init(DefCaptureBufferSize) then
      Abort('Insufficient memory for Capture buffer');
    {Open a port}
    if not Uart.InitCustom(Port, Baud, Parity, Bits,
                         Stop, DefInBufferSize, DefOutBufferSize,
                         HWO, SWF, PO) then
      Abort('Failed to open port');
    Uart.SetAbortFunc(KeyboardAbort);
    Uart.SetErrorProc(MyErrorProc);
    {initialize a Modem object}
    if not Modem.Init(@Uart) then
      Abort('Failed to instantiate modem object');
    {$IFDEF UseUart}
    if ClassifyUart(Uart.GetBaseAddr, False) = U16550A then
      SetFifoBuffering(Uart.GetBaseAddr, True, 4);
    {$ENDIF}
  end;

begin
  TextAttr := OurTextAttr;
  WStackPos := 0;               {initialize the window stack pointer to zero}
  ClrScr;
  StatusY := ScreenHeight - 1;
  MsgY := StatusY - 1;
  FastWrite(Center(TitleStr, 80), 1, 1, HelpAttr);   {write title}
  FastWrite(Center(HelpStr, 80), ScreenHeight, 1, HelpAttr);   {write help line}
  ClearMessage;                 {create the message line}
  GotoXY(1,2);                  {put cursor in terminal window}
  Initialize;                   {instantiate objects}
  TerminalMode;                 {enter terminal loop}
  Modem.Done;                   {call Modem's destructor}
  Uart.Done;                    {call Comm port object's destructor}
  Capture.Done;                 {call Capture object's destructor}
  FullWindow;
  TextAttr := 7;
  ClrScr;

  {!!.01 Clean up possible active BPlus object}
  if BPlusStarted then
    Dispose(BP, Done);
end.
