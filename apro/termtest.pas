{$R-,S-,F+,I-}

{*********************************************************}
{*                  TERMTEST.PAS 2.03                    *}
{*     Copyright (c) TurboPower Software 1990.           *}
{*                 All rights reserved.                  *}
{*********************************************************}
program TermTest;
  {-A program to test the TerminalWindow object}

{
  this program currently supports the following keyboard commands:

    Key      Command
    -------------------------------------------------------------------------
    F1       Help
    F2       Toggle "Output Wrapping." When enabled, this options causes the
             output to a TerminalWindow to word wrap and scroll within the
             window.
    F3       Prompt for string to send to modem for dialing.
    F4       Clears the TerminalWindow. NOTE: the entire virtual screen is
             cleared.
    F5       Zoom/UnZoom.
    F6       When in UseFile mode, starts/stops feeding of NewTerminalWindow
             from file (see command line options below).
    F7       Select next baud rate. This option allows the user to step
             through the available baud rates. The current baud rate appears
             in the status line at the bottom of the screen.
    F8       Toggles between 8 data bits/no parity/1 stop bit and
             7 data bits/even parity/1 stop bit.
    F9       When in UseFile mode, prompts for new filename.
    F10      When in UseFile mode, resets input file back to start.
    Alt-F1   Toggle "Floating View." When enabled, this option causes the
             current View to follow the cursor through the TerminalWindow's
             virtual screen (scroll back buffer). In most cases, you want
             this option enabled.
    Alt-F2   Log current scroll back buffer to a file
    Alt-F3   Close windows and exit
    Alt-F4   Obscure Terminal window (open a window that overlaps it)
    Alt-F5   Maximize TerminalWindow. Like Zoom, except keeps frame,
             scrollbars, and hot spots.
    Alt-C    Toggle capture state

  See TERMWIN.PCD for default keyboard processing of TerminalWindow.

  Command Line Options:

    /Ffilename        use the specified filename to feed the terminal
                      window. Note that there is no space between the /F and
                      the filename. When /F is specified, characters will be
                      taken from the file instead of the com port (so the com
                      port is effectively disabled). Examples:
                        TERMTEST /FSIMPCOM.PAS - feed from file SIMPCOM.PAS

    /Cportnum         use specified port number (1..8). Examples:
                        TERMTEST /C2       - use COM2
                        TERMTEST /C3       - use COM3

    /Sfilename        save to stream using specified filename

    /Lfilename        load from stream using specified filename

  This program is compatible with all versions of Object Professional. If
  using Object Professional 1.03 or greater, the UseDrag directive will be
  honored.

  If you are using this program to communicate with a program that strips line
  feeds, then you will want to enable the twInCRtoCRLF option.

  To really see this program perform, try sending data to it that contains
  ANSI escape sequences (we use a brief animation sequence to test it). Since
  the TerminalWindow utilizes a virtual screen, you see only data being
  written within the active view. When twFloatingView is enabled, the view
  will automatically follow the cursor as it fills the virtual screen. The
  virtual screen allows for a scrollback buffer. The maximum capacity of the
  virtual screen is about 400 lines.
}

{$I OPDEFINE.inc}
{$I APDEFINE.inc}                                                      {!!.01}

uses
  {$IFDEF LeakCheck}                                                   {!!.01}
  LeakChek,                                                            {!!.01}
  {$ENDIF}                                                             {!!.01}
  ApMisc,                  {error codes and misc.}
  ApPort,                  {low level port support}
  OOCom,                   {async objects}
  OOModem,                 {modem object}
  OOEmu,                   {emulator objects}
  OpInline,                {assorted inline macros}
  OpString,                {string routines}
  OpRoot,
  {$IFDEF OPRO12}
  OpConst,                 {const split out in OPRO 1.2}
  {$ENDIF}
  {$IFDEF UseMouse}
  OpMouse,                 {mouse routines}
  {$ENDIF}
  OpCrt,                   {video routines}
  OpFrame,                 {frame routines}
  OpCmd,                   {command processor}
  {$IFDEF UseDrag}
  OpDrag,                  {drag processor}
  {$ENDIF}
  OpWindow,                {window routines}
  OpSEdit,                 {simple line editor}
  TermWin;                 {terminal window}

type
  StatusTerminalWindowPtr = ^StatusTerminalWindow;
  StatusTerminalWindow =
    object(CaptureTerminalWindow)
      stwNumIn  : LongInt;
      stwNumOut : LongInt;
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             WindowOptions : LongInt;
                             Port : AbstractPortPtr;
                             Emu : TerminalEmulatorPtr;
                             ScrollBackRows : Word;
                             ScrollBackCols : Word;
                             TerminalOptions : LongInt;
                             CaptureFileName : String;
                             CaptureBufferSize : Word);
      function GetIncomingChar(var Key : Char) : Boolean; Virtual;
      procedure SendOutgoingChar(C : Char); Virtual;
      procedure UpdateContents; Virtual;
      procedure UserStatus;
      procedure GetCharCounts(var Incoming, Outgoing : LongInt);
      procedure Dial(Number : NumberStr);
      {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
      procedure Store(var S : IdStream);
      {$ENDIF}
    end;
  {type derived from TerminalWindow to provide feeding TW from disk file
   instead of COM port. Also overrides SendOutgoingChar so characters aren't
   sent out the COM port (to prevent filling buffer when testing).
   Useful for debugging and demonstration.}
  NewTerminalWindowPtr = ^NewTerminalWindow;
  NewTerminalWindow =
    object(StatusTerminalWindow)
      function GetIncomingChar(var Key : Char) : Boolean; Virtual;
      procedure SendOutgoingChar(C : Char); Virtual;
      procedure Dial(Number : NumberStr);
      {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
      procedure Store(var S : IdStream);
      {$ENDIF}
    end;


var
  TW : StatusTerminalWindowPtr;
  InFile : File;
  SStreamFileName : String;
  LStreamFileName : String;
  CTP : CmdTablePtr;                                                   {!!.01}

const
  HWHandshakeOptions = hfUseDTR + hfUseRTS + hfRequireCTS;
  SecondaryCmdSize = 100;                                              {!!.01}

  {$IFDEF UseStreams}
  otStatusTerminalWindow = otCaptureTerminalWindow + 1;
  veStatusTerminalWindow = 0;
  otNewTerminalWindow    = otStatusTerminalWindow + 1;
  veNewTerminalWindow    = 0;
  {$ENDIF}

  {$IFNDEF UseDrag}          {if drag isn't define, then define hotcodes for}
  MoveHotCode   = hsRegion0; {move, resize and zoom}
  ResizeHotCode = hsRegion1;
  ZoomHotCode   = hsRegion2;
  {$ENDIF}


  HeaderStr = ' TerminalWindow ';
  InBufSize = 8 * 1024;
  OutBufSize = 4 * 1024;
  Title = 'TermTest 1.0 - A Terminal Window Test Program';

  Idle : Boolean = False;
  UseAnsi : Boolean = True;
  SaveAsyncError : Word = 0;
  ComName : ComNameType = Com1;
  DefBaud : LongInt = 9600;
  UseFile : Boolean = False;
  InFileName : String = 'TERMTEST.PAS';
  LogFileName : String = 'TERMTEST.LOG';
  ScrollBackRows : Word = 200;

  CaptureFileName    : String = 'TERMTEST.CAP';
  CaptureBufferSize  : Word = 2048;

  StoreToStream   : Boolean = False;
  LoadFromStream  : Boolean = False;

  {our colors}
  TTColors : ColorSet = (
    TextColor       : $0F; TextMono       : $0F;
    CtrlColor       : $07; CtrlMono       : $07;
    FrameColor      : $13; FrameMono      : $07;
    HeaderColor     : $3F; HeaderMono     : $70;
    ShadowColor     : $08; ShadowMono     : $70;
    HighlightColor  : $4F; HighlightMono  : $70;
    PromptColor     : $30; PromptMono     : $07;
    SelPromptColor  : $30; SelPromptMono  : $07;
    ProPromptColor  : $30; ProPromptMono  : $07;
    FieldColor      : $31; FieldMono      : $0F;
    SelFieldColor   : $31; SelFieldMono   : $0F;
    ProFieldColor   : $17; ProFieldMono   : $07;
    ScrollBarColor  : $13; ScrollBarMono  : $07;
    SliderColor     : $13; SliderMono     : $0F;
    HotSpotColor    : $30; HotSpotMono    : $70;
    BlockColor      : $3E; BlockMono      : $0F;
    MarkerColor     : $5F; MarkerMono     : $70;
    DelimColor      : $31; DelimMono      : $0F;
    SelDelimColor   : $31; SelDelimMono   : $0F;
    ProDelimColor   : $31; ProDelimMono   : $0F;
    SelItemColor    : $3E; SelItemMono    : $70;
    ProItemColor    : $17; ProItemMono    : $07;
    HighItemColor   : $1F; HighItemMono   : $0F;
    AltItemColor    : $1F; AltItemMono    : $0F;
    AltSelItemColor : $3F; AltSelItemMono : $70;
    FlexAHelpColor  : $1F; FlexAHelpMono  : $0F;
    FlexBHelpColor  : $1F; FlexBHelpMono  : $0F;
    FlexCHelpColor  : $1B; FlexCHelpMono  : $70;
    UnselXrefColor  : $1E; UnselXrefMono  : $09;
    SelXrefColor    : $5F; SelXrefMono    : $70;
    MouseColor      : $4F; MouseMono      : $70
  );

  constructor StatusTerminalWindow.InitCustom(X1, Y1, X2, Y2 : Byte;
                                              var Colors : ColorSet;
                                              WindowOptions : LongInt;
                                              Port : AbstractPortPtr;
                                              Emu : TerminalEmulatorPtr;
                                              ScrollBackRows : Word;
                                              ScrollBackCols : Word;
                                              TerminalOptions : LongInt;
                                              CaptureFileName : String;
                                              CaptureBufferSize : Word);
  begin
    if not CaptureTerminalWindow.InitCustom(X1, Y1, X2, Y2,
                                            Colors, WindowOptions,
                                            Port, Emu,
                                            ScrollBackRows, ScrollBackCols,
                                            TerminalOptions,
                                            CaptureFileName,
                                            CaptureBufferSize) then
      Fail;
    stwNumIn  := 0;
    stwNumOut := 0;
  end;

  function StatusTerminalWindow.GetIncomingChar(var Key : Char) : Boolean;
  begin
    if CaptureTerminalWindow.GetIncomingChar(Key) then begin
      Inc(stwNumIn);
      GetIncomingChar := True;
    end
    else
      GetIncomingChar := False;
  end;

  procedure StatusTerminalWindow.SendOutgoingChar(C : Char);
  begin
    CaptureTerminalWindow.SendOutgoingChar(C);
    Inc(stwNumOut);
  end;

  procedure StatusTerminalWindow.UpdateContents;
  begin
    CaptureTerminalWindow.UpdateContents;
    UserStatus;
  end;

  procedure StatusTerminalWindow.GetCharCounts(var Incoming,
                                                   Outgoing : LongInt);
  begin
    Incoming := stwNumIn;
    Outgoing := stwNumOut;
  end;

  procedure StatusTerminalWindow.UserStatus;
    {-Show some status information on the bottom line.}
  const
    Paritys : Array[ParityType] of Char = ('N', 'O', 'E', 'M', 'S');
    OnOff : array[Boolean] of String[3] = ('Off', 'On ');
    OffOn : Array[Boolean] of String[3] = ('off', 'on');
  var
    S : String;
    Baud : LongInt;
    Parity : ParityType;
    DataBits : DataBitType;
    StopBits : StopBitType;
    Incoming, Outgoing : LongInt;
    {$IFDEF UseMouse}
    MouseState : Boolean;
    {$ENDIF}
  begin
    {$IFDEF UseMouse}
    HideMousePrim(MouseState);
    {$ENDIF}
    with GetTerminalWinPort^ do begin
      GetLine(Baud, Parity, DataBits, StopBits, False);
      S := ComNameString(GetComName) + ' ';
      S := S + Long2Str(Baud) +  ' baud ' +
           Paritys[Parity] + Long2Str(DataBits) + Long2Str(StopBits);
      with twVScreen^ do
      S := S + '   viewX=' + Long2Str(svsViewX) +
           ' viewY=' + Long2Str(svsViewY);
      GetCharCounts(Incoming, Outgoing);
      S := S + '   In=' + Long2Str(Incoming) + ' Out=' + Long2Str(Outgoing);
    end;

    with TTColors do
      FastWrite(Pad(S, 80), ScreenHeight, 1,
                ColorMono(HeaderColor, HeaderMono));
    {$IFDEF UseMouse}
    ShowMousePrim(MouseState);
    {$ENDIF}
  end;

  procedure StatusTerminalWindow.Dial(Number : NumberStr);
  var
    M : AbstractModemPtr;
  begin
    M := New(HayesModemPtr, Init(GetTerminalWinPort));
    if M = Nil then
      Exit;
    M^.DialModem(Number);
    Dispose(M, Done);
  end;

  {$IFDEF UseStreams}
  constructor StatusTerminalWindow.Load(var S : IdStream);
  begin
    if not CaptureTerminalWindow.Load(S) then
      Fail;
  end;

  procedure StatusTerminalWindow.Store(var S : IdStream);
  begin
    CaptureTerminalWindow.Store(S);
  end;

  procedure StatusTerminalWindowStream(SPtr : IdStreamPtr);
  begin
    CaptureTerminalWindowStream(SPtr);
    SPtr^.RegisterType(otStatusTerminalWindow, veStatusTerminalWindow,
                       TypeOf(StatusTerminalWIndow),
                       @StatusTerminalWindow.Store,
                       @StatusTerminalWindow.Load);
  end;
  {$ENDIF}

  function NewTerminalWindow.GetIncomingChar(var Key : Char) : Boolean;
    {-This method allows a NewTerminalWindow to be fed from a disk file
      instead of the COM port. Note: This method does not allow characters to
      come in via the COM port (although it could be easily modified to do
      so).}
  begin
    if Idle or EOF(InFile) then
      GetIncomingChar := False
    else begin
      GetIncomingChar := True;
      BlockRead(InFile, Key, 1);
      if IoResult = 0 then begin
        GetIncomingChar := (Key <> ^Z);
        Inc(stwNumIn);
      end
      else
        GetIncomingChar := False;
    end;
  end;

  procedure NewTerminalWindow.SendOutgoingChar(C : Char);
    {-Do nothing routine, prevents output buffer from overflowing when testing
      with no device attached.}
  begin
    Inc(stwNumOut);
  end;

  procedure NewTerminalWindow.Dial(Number : NumberStr);
  begin
  end;

  {$IFDEF UseStreams}
  constructor NewTerminalWindow.Load(var S : IdStream);
  begin
    if not StatusTerminalWindow.Load(S) then
      Fail;
  end;
  procedure NewTerminalWindow.Store(var S : IdStream);
  begin
    StatusTerminalWindow.Store(S);
  end;

  procedure NewTerminalWindowStream(SPtr : IdStreamPtr);
  begin
    StatusTerminalWindowStream(SPtr);
    SPtr^.RegisterType(otNewTerminalWindow, veNewTerminalWindow,
                       TypeOf(NewTerminalWIndow),
                       @NewTerminalWindow.Store,
                       @NewTerminalWindow.Load);
  end;
  {$ENDIF}

  function GetKey : Word;
  var
    C : Char;
  begin
    while not KeyPressed do
      with TW^ do
        if GetIncomingChar(C) then
          WriteChar(C);
    GetKey := ReadKeyWord;
  end;

  procedure PortErrorProc(P : Pointer; var StatusCode : Word);
    {-This routine is called by errors encountered in the AbstractPort object}
  var
    S : String;
    C : Char;
  begin
    with AbstractPortPtr(P)^ do begin
      {Do nothing if a protocol is in progress}
      if ProtocolInProgress then
        Exit;
      StatusCode := StatusCode mod 10000;
      if StatusCode <> 0 then begin
        {Build an error message}
        with TTColors do
          FastWrite(Pad('Async Error: '+ StatusStr(StatusCode mod 10000), 80),
                    ScreenHeight, 1,
                    ColorMono(HeaderColor, HeaderMono));
        Sound(110);
        Delay(100);
        NoSound;
        StatusCode := 0;
        if GetKey =   0 then ;
        TW^.UserStatus;
      end;
    end;
  end;

  procedure TTErrorProc(UnitCode : Byte; var ErrorCode : Word;
                        ErrorMsg : String);
    {-Called by TerminalWindow on errors.}
  begin
    with TTColors do
      FastWrite(Pad('UnitCode=' + Long2Str(UnitCode) + '  Line Error: ' +
                StatusStr(ErrorCode mod 10000), 80), ScreenHeight, 1,
                ColorMono(HeaderColor, HeaderMono));

    Sound(110);
    Delay(100);
    NoSound;
    if GetKey = 0 then ;
    TW^.UserStatus;
  end;

{The following code is only needed if using the mouse and NOT using 1.03's
 OPDRAG unit}

{$IFDEF UseMouse}
{$IFNDEF UseDrag}
  function Delta(I : Integer) : Integer;
    {-Force I to the range -1..1}
  begin
    if I < -1 then
      Delta := -1
    else if I > 1 then
      Delta := 1
    else
      Delta := 0;
  end;

  procedure EvaluateMouseEvent;
    {-React on a mouse event.}
  var
    Clicked, MouseOn : Boolean;
    FP : FramePosType;
    HC : Byte;
    BP : LongInt;
    XAbs : Byte;
    YAbs : Byte;
    MicH : Integer;
    MicV : Integer;
  begin
    with TW^ do begin
      {get absolute mouse coordinates}
      XAbs := MouseKeyWordX+MouseXLo;
      YAbs := MouseKeyWordY+MouseYLo;

      {evaluate mouse position}
      EvaluatePos(XAbs, YAbs);
      BP := PosResults(FP, HC);
      case HC of
        MoveHotCode :   {move}
          if not IsZoomed then begin
            HideMousePrim(MouseOn);
            Dec(XAbs, wFrame.frXL);
            Dec(YAbs, wFrame.frYL);
            GetMickeyCount(MicH, MicV);
            repeat
              GetMickeyCount(MicH, MicV);
              MoveWindow(Delta(MicH), Delta(MicV));
              if ClassifyError(GetLastError) = etFatal then begin
                WriteLn('Out of memory');
                Halt;
              end;
              if MousePressed then
                Clicked := (MouseKeyWord = MouseLft)
              else
                Clicked := False;
            until Clicked;
            Inc(XAbs, wFrame.frXL);
            Inc(YAbs, wFrame.frYL);
            MouseGoToXY(XAbs, YAbs);
            ShowMousePrim(MouseOn);
          end;

        ZoomHotCode : {Zoom}
          if IsZoomed then
            UnZoom
          else
            Zoom;

        ResizeHotCode : {Resize}
          if wOptionsAreOn(wResizeable) and not IsZoomed then begin
            HideMousePrim(MouseOn);
            GetMickeyCount(MicH, MicV);
            repeat
              GetMickeyCount(MicH, MicV);
              ResizeWindow(Delta(MicH), Delta(MicV));
              if ClassifyError(GetLastError) = etFatal then begin
                WriteLn('Out of memory');
                Halt;
              end;
              if MousePressed then
                Clicked := (MouseKeyWord = MouseLft)
              else
                Clicked := False;
            until Clicked;
            MouseGoToXY(wFrame.frXH, wFrame.frYH);
            ShowMousePrim(MouseOn);
          end;
      end;
    end;
  end;
{$ENDIF}
{$ENDIF}

  procedure ShowHelp;
    {-Display command line options and halt}
  begin
    WriteLn('TERMTEST [options]');
    WriteLn('  /Ffilename     use specified filename to feed TerminalWindow');
    WriteLn('  /Cportnum[P]   use specified port number (1..8) [follow with P for PS/2]');
    {$IFDEF UseStreams}
    WriteLn('  /Sfilename     store to stream using specified filename');
    WriteLn('  /Lfilename     load from stream using specified filename');
    {$ENDIF}
    WriteLn('  /?             show this help message');
    Halt;
  end;

  procedure ParseCommandLine;
    {-Parse the command line for options}
  var
    I : Byte;
    S : String;
  begin
    for I := 1 to ParamCount do begin
      S := ParamStr(I);
      if S[1] in ['-', '/'] then
        case UpCase(S[2]) of
          'F' :
            begin
              UseFile := True;
              if Length(S) > 2 then
                InFileName := Copy(S, 3, Length(S));
            end;
          'C' : ComName := ComNameType(Ord(S[3]) - Ord('1'));
          {$IFDEF UseStreams}
          'L' :
            begin
              LoadFromStream  := True;
              LStreamFileName := Copy(S, 3, Length(S));
            end;
          'S' :
            begin
              StoreToStream  := True;
              SStreamFileName := Copy(S, 3, Length(S));
            end;
          {$ENDIF}
          '?' : ShowHelp;
        end;
    end;

  end;

  procedure SetBaudRate;
    {-Step through all available baud rates}
  var
    B : LongInt;
    P : ParityType;
    D : DataBitType;
    S : StopBitType;
  begin
    with TW^, GetTerminalWinPort^ do begin
      GetLine(B, P, D, S, False);
      if B = 1200 then
        B := 2400
      else if B = 2400 then
        B := 4800
      else if B = 4800 then
        B := 9600
      else if B = 9600 then
        B := 19200
      else if B = 19200 then
        B := 38400
      else if B = 38400 then
        B := 57600
      else if B = 57600 then
        B := 115200
      else
        B := 1200;
      SetLine(B, P, D, S);
    end;
  end;

  procedure SetCommOptions;
    {-Toggle between 8N1 and 7E1}
  var
    B : LongInt;
    P : ParityType;
    D : DataBitType;
    S : StopBitType;
  begin
    with TW^, GetTerminalWinPort^ do begin
      GetLine(B, P, D, S, False);
      if D = 8 then begin
        D := 7;
        P := EvenParity;
      end
      else begin
        D := 8;
        P := NoParity;
      end;
      SetLine(B, P, D, S);
    end;
  end;

  procedure Dialer;
    {-Prompt for a number to dial and dial it}
  const
    Prompt = 'Enter number to dial: ';
    LastNum : String = '';
  var
    SLE : SimpleLineEditorPtr;
    S : String;
    {$IFDEF UseMouse}
    MouseState : Boolean;
    {$ENDIF}
  begin
    {$IFDEF UseMouse}
    HideMousePrim(MouseState);
    {$ENDIF}
    New(SLE, Init(TTColors));
    if SLE = Nil then
      Exit;
    S := LastNum;
    with SLE^ do begin
      ReadString(Prompt, ScreenHeight, 1, SizeOf(S)-1,
                 ScreenWidth-Length(Prompt), S);
      if GetLastCommand = ccSelect then begin
        LastNum := S;
        TW^.Dial(S);
      end;
    end;
    Dispose(SLE, Done);
    {$IFDEF UseMouse}
    ShowMousePrim(MouseState);
    {$ENDIF}
  end;

  procedure GetFileName;
    {-When operating in UseFile mode, this routine prompts for a new filename
    to use to feed the NewTerminalWindow}
  const
    Prompt = 'Enter filename: ';
  var
    SLE : SimpleLineEditorPtr;
    S : String;
    {$IFDEF UseMouse}
    MouseState : Boolean;
    {$ENDIF}
  begin
    {$IFDEF UseMouse}
    HideMousePrim(MouseState);
    {$ENDIF}
    New(SLE, Init(TTColors));
    if SLE = Nil then
      Exit;
    S := InFileName;
    with SLE^ do begin
      ReadString(Prompt, ScreenHeight, 1, SizeOf(S)-1,
                 ScreenWidth-Length(Prompt), S);
      if GetLastCommand = ccSelect then begin
        if IoResult <> 0 then ;                                        {!!.02}
        Assign(InFile, S);
        Reset(InFile, 1);
        if IoResult = 0 then
          InFileName := S
        else begin
          Assign(InFile, InFileName);
          Reset(InFile, 1);
          if IoResult <> 0 then
            UseFile := False;
        end;
      end;
    end;
    Dispose(SLE, Done);
    {$IFDEF UseMouse}
    ShowMousePrim(MouseState);
    {$ENDIF}
  end;

  procedure LogToFile;
  const
    Prompt = 'Enter file to write to: ';
    ErrorMsg = 'Unable to write to ';
  var
    SLE : SimpleLineEditorPtr;
    S : String;
    {$IFDEF UseMouse}
    MouseState : Boolean;
    {$ENDIF}
  begin
    {$IFDEF UseMouse}
    HideMousePrim(MouseState);
    {$ENDIF}
    New(SLE, Init(TTColors));
    if SLE = Nil then
      Exit;
    S := LogFileName;
    with SLE^ do begin
      ReadString(Prompt, ScreenHeight, 1, SizeOf(S)-1,
                 ScreenWidth-Length(Prompt), S);
      if GetLastCommand = ccSelect then begin
        LogFileName := S;
        if Length(S) > 0 then
          TW^.CopyVirtToFile(1, ScrollBackRows, LogFileName,
                             True, True, True);
      end;
    end;
    Dispose(SLE, Done);
    {$IFDEF UseMouse}
    ShowMousePrim(MouseState);
    {$ENDIF}
  end;

  procedure DoCapture;
  const
    Prompt = 'Enter file to capture to: ';
  var
    SLE : SimpleLineEditorPtr;
    S : String;
    {$IFDEF UseMouse}
    MouseState : Boolean;
    {$ENDIF}
  begin
    {$IFDEF UseMouse}
    HideMousePrim(MouseState);
    {$ENDIF}
    with TW^ do
      if CaptureIsOn then begin
        with TTColors do
          FastWrite(Pad('Capture disabled', 80), ScreenHeight, 1,
                ColorMono(HeaderColor, HeaderMono));
        Capture(False);
      end;
    New(SLE, Init(TTColors));
    if SLE = Nil then
      Exit;
    S := CaptureFileName;
    with SLE^ do begin
      ReadString(Prompt, ScreenHeight, 1, SizeOf(S)-1,
                 ScreenWidth-Length(Prompt), S);
      if GetLastCommand = ccSelect then begin
        CaptureFileName := S;
        if Length(S) > 0 then
          with TW^ do begin
            SetCaptureFileName(S);
            Capture(True);
          end;
      end;
    end;
    Dispose(SLE, Done);
    {$IFDEF UseMouse}
    ShowMousePrim(MouseState);
    {$ENDIF}
  end;

  procedure ToggleOption(Option : Word);
    {-Toggle the specified option.}
  begin
    with TW^ do
      if twOptionsAreOn(Option) then
        twOptionsOff(Option)
      else
        twOptionsOn(Option);
  end;

  procedure Maximize;
    {-Like zoom, except keeps frame. If not already maximized, then calling
    this procedure maximizes the window, otherwise the window is returned to
    its pre-maximized state.}
  const
    Maximized : Boolean = False;
    X1 : Byte = 0;
    Y1 : Byte = 0;
    X2 : Byte = 0;
    Y2 : Byte = 0;
  begin
    with TW^ do begin
      if IsZoomed then
        Exit;
      if Maximized then
        AdjustWindow(X1, Y1, X2, Y2)
      else begin
        Coordinates(X1, Y1, X2, Y2);
        AdjustWindow(2, 3, ScreenWidth - 1, ScreenHeight - 2);
      end;
    end;
    Maximized := not Maximized;
  end;

  procedure Obscure;
  var
    SW : StackWindow;

  begin
    SW.InitCustom(50, 10, 75, 20, TTColors, DefWindowOptions or wBordered);
    with SW do begin
      Draw;
      if GetKey = 0 then ;
      Erase;
    end;
    SW.Done;
  end;


  procedure InitCommandProcessor;
  begin
    with TerminalCommands do begin
      if not GetMemCheck(CTP, SecondaryCmdSize) then ;                 {!!.01}
      FillChar(CTP^, SecondaryCmdSize, 0);
      SetSecondaryKeyPtr(CTP, SecondaryCmdSize);
      {customize the TerminalCommands command processor to our tastes}
      cpOptionsOn(cpCtrlsAreChars);

      AddSecondaryCommand(ccUser1, 1, $6800, 0); {Alt-F1}
      AddSecondaryCommand(ccUser2, 1, $3C00, 0); {F2}
      AddSecondaryCommand(ccUser3, 1, $3D00, 0); {F3}
      AddSecondaryCommand(ccUser4, 1, $3E00, 0); {F4}
      AddSecondaryCommand(ccUser5, 1, $3F00, 0); {F5}
      AddSecondaryCommand(ccUser6, 1, $4000, 0); {F6}
      AddSecondaryCommand(ccUser7, 1, $4100, 0); {F7}
      AddSecondaryCommand(ccUser8, 1, $4200, 0); {F8}
      AddSecondaryCommand(ccUser9, 1, $4300, 0); {F9}
      AddSecondaryCommand(ccUser10, 1, $4400, 0);{F10}
      AddSecondaryCommand(ccUser11, 1, $6C00, 0);{Alt-F5}
      AddSecondaryCommand(ccUser12, 1, $6900, 0);{Alt-F2}
      AddSecondaryCommand(ccUser13, 1, $6B00, 0);{Alt-F4}
      AddSecondaryCommand(ccUser14, 1, $2E00, 0);{Alt-C}
    end;
  end;

  procedure ClearEntireWindow;
  var
    SaveAOC : Boolean;
  begin
    with TW^ do begin
      with TTColors do
        twSetTextAttr(ColorMono(TextColor, TextMono));
      SaveAOC := twOptionsAreOn(twAdvanceOnClear);
      twOptionsOff(twAdvanceOnClear);
      twClrScr;
      if SaveAOC then
        twOptionsOn(twAdvanceOnClear);
      SetView(1, 1);
    end;
  end;

{variables local to the main code block}
var
  Port : UartPort;
  Modem : HayesModem;
  Emu : TerminalEmulatorPtr;
  Cmd : Word;

  {$IFDEF UseStreams}
  procedure LoadObjects;
  var
    OurStream : BufIdStream;
    ErrorCode : Word;
  begin
    if not OurStream.Init(LStreamFileName, SOpen, 4096) then begin
      WriteLn('Failed to init stream, InitStatus = ', InitStatus);
      Halt(1);
    end;
    OurStream.RegisterHier(NewTerminalWindowStream);

    OurStream.RegisterHier(UartPortStream);
    OurStream.RegisterPointer(ptErrorProc, @PortErrorProc);
    ErrorCode := OurStream.GetStatus;
    if ErrorCode <> 0 then begin
      WriteLn('Error registering protocol object: ', ErrorCode);
      Halt(1);
    end;

    TW := StatusTerminalWindowPtr(OurStream.GetPtr);
    ErrorCode := OurStream.GetStatus;
    if (TW = Nil) or (ErrorCode <> 0) then begin
      WriteLn('Error loading from stream ', ErrorCode);
      Halt(1);
    end;
    OurStream.Done;

    with TW^ do begin
      stwNumIn := 0;
      stwNumOut := 0;
    end;
  end;

  procedure StoreObjects;
  var
    OurStream : BufIdStream;
    ErrorCode : Word;
  begin
    if not OurStream.Init(SStreamFileName, SCreate, 4096) then begin
      WriteLn('Failed to init stream, InitStatus = ', InitStatus);
      Halt(1);
    end;
    OurStream.RegisterHier(NewTerminalWindowStream);

    OurStream.RegisterHier(UartPortStream);
    OurStream.RegisterPointer(ptErrorProc, @PortErrorProc);
    ErrorCode := OurStream.GetStatus;
    if ErrorCode <> 0 then begin
      WriteLn('Error registering protocol object: ', ErrorCode);
      Halt(1);
    end;

    OurStream.PutPtr(TW);
    ErrorCode := OurStream.GetStatus;
    if ErrorCode <> 0 then begin
      WriteLn('Error storing stream ', ErrorCode);
      Halt(1);
    end;
    OurStream.Done;
  end;
  {$ENDIF}

  procedure InitObjects;
  begin
    {init the Uart port}
    if not Port.InitCustom(ComName, DefBaud, NoParity, 8, 1,
                           InBufSize, OutBufSize, DefPortOptions) then begin
      WriteLn('Failed to init port: ', StatusStr(AsyncStatus));
      Halt;
    end;

    {init the modem object}
    if not Modem.Init(@Port) then begin
      WriteLn('Failed to init modem: ', StatusStr(AsyncStatus));
      Halt;
    end;

    {init the emulator object}
    if UseAnsi then
      Emu := New(AnsiEmulatorPtr, Init(32))
    else
      Emu := New(TerminalEmulatorPtr, Init(32));
    if Emu = Nil then begin
      WriteLn('Failed to init emulator');
      Halt;
    end;

    {init the CaptureTerminalWindow (or NewTerminalWindow)}
    if UseFile then
      TW := New(NewTerminalWindowPtr, InitCustom(5, 5, 60, 20,
                                                 TTColors,
                                                 DefWindowOptions or wBordered,
                                                 @Port, Emu,
                                                 ScrollBackRows,
                                                 DefScrollBackCols,
                                                 DefTerminalWinOptions,
                                                 CaptureFileName,
                                                 CaptureBufferSize))
    else
      TW := New(StatusTerminalWindowPtr, InitCustom(5, 5, 60, 20,
                                                 TTColors,
                                                 DefWindowOptions or wBordered,
                                                 @Port, Emu,
                                                 ScrollBackRows,
                                                 DefScrollBackCols,
                                                 DefTerminalWinOptions,
                                                 CaptureFileName,
                                                 CaptureBufferSize));
    if TW = Nil then begin
      WriteLn('Failed to init TerminalWindow, InitStatus = ', InitStatus);
      Halt;
    end;

    (*Uncomment to turn off auto LF                                    {!!.02}
    TW^.twOptionsOn(twInhibitAutoLF);                                  {!!.02}
    *)                                                                 {!!.02}
  end;

begin
  FileMode := $40;                                                     {!!.02}
  ParseCommandLine;

  {if UseFile, then open input file for feeding NewTerminalWindow}
  if UseFile then begin
    Assign(InFile, InFileName);
    Reset(InFile, 1);
    if IoResult <> 0 then
      UseFile := False;
  end;

  {$IFDEF UseStreams}
  if LoadFromStream then
    LoadObjects
  else
    InitObjects;
  {$ELSE}
  InitObjects;
  {$ENDIF}
  TW^.SetPageSize(ScreenHeight);

  {draw screen backdrop}
  TextAttr := 7;
  TextChar := '²';
  ClrScr;

  InitCommandProcessor;

  {draw the title line}
  with TTColors do
    FastWrite(Center(Title, ScreenWidth), 1, 1,
              ColorMono(HeaderColor, HeaderMono));

  {$IFDEF UseMouse}
  {if mouse is installed, turn on support for it}
  if MouseInstalled then
    TerminalCommands.cpOptionsOn(cpEnableMouse);
  {$ENDIF}

  with TW^ do begin
    with GetTerminalWinPort^ do begin
      SetErrorProc(PortErrorProc);    {set up port error handler}
      {enable hardware handshaking on port}
      HWFlowEnable((InBufSize div 10) * 9, (InBufSize div 10),
                   HWHandshakeOptions);
    end;
    SetPosLimits(1, 2, ScreenWidth, ScreenHeight - 1); {set max window size}
    SetErrorProc(TTErrorProc);                         {set up error handler}

    {add headers, scroll bars and hotspots to frame}
    with wFrame, TTColors do begin
      AddHeader(HeaderStr, heTC);
      AddHotHeader(heTC, MoveHotCode, Length(HeaderStr));
      if wOptionsAreOn(wResizeable) then begin
        {add zoom button}
        AddCustomHeader('¸',  frTR,   -4, +0, FrameColor, FrameMono);
        AddCustomHeader(^R,   frTR,   -3, +0, HotSpotColor, HotSpotMono);
        AddCustomHeader('Õ',  frTR,   -2, +0, FrameColor, FrameMono);
        AddHotRegion(frTR, ZoomHotCode, -3, +0, 1, 1);                 {Zoom}

        {add resize button}
        AddCustomHeader('+',  frBR,   +0, +0, FrameColor, FrameMono);
        AddHotRegion(frBR, ResizeHotCode, +0, +0, 1, 1);               {Resize}
      end;

      AddScrollBar(frRR, 0, 100, TTColors);
      AddScrollBar(frBB, 0, 100, TTColors);
    end;

    {display the window}
    Draw;

    repeat
      TW^.UserStatus;                              {update status line}
      Process;                                     {process TW}
      Cmd := GetLastCommand;                       {process the last command}
      case Cmd of
        ccUser1 :
          begin
            ToggleOption(twFloatingVView);         {toggle floating view}
          end;
        ccUser2 : ToggleOption(twWrapInWin);       {toggle output wrapping}
        ccUser3 : Dialer;                          {prompt for number to dial}
        ccUser4 : ClearEntireWindow;               {clear screen}
        ccUser5 :                                  {zoom/unzoom}
          if IsZoomed then
            UnZoom
          else
            Zoom;
        ccUser6 : Idle := not Idle;                {stop/start file feeding}
        ccUser7 : SetBaudRate;                     {step through baud rates}
        ccUser8 : SetCommOptions;                  {toggle between 8N1/7E1}
        ccUser9 :
          if UseFile then                          {get new filename}
            GetFileName;
        ccUser10:
          if UseFile then
            Seek(InFile, 0);                       {reset input file}
        ccUser11 : Maximize;                       {maximize the window}
        ccUser12 : LogToFile;
        ccUser13 : Obscure;
        ccUser14 : DoCapture;
        {$IFDEF UseMouse}
        {$IFDEF UseDrag}
        ccMouseDown : if HandleMousePress(TW^) = 0 then ; {handle drag}
        {$ELSE}
        ccMouseSel :                               {handle mouse event}
          EvaluateMouseEvent;
        {$ENDIF}
        {$ENDIF}
      end;
    until (Cmd in [ccQuit, ccError]);
    Erase;                                         {erase the window}
  end;

  ClrScr;
  if Cmd = ccError then
    WriteLn('Fatal Error = ', StatusStr(SaveAsyncError));
  {$IFDEF UseStreams}
  if StoreToStream then
    StoreObjects;
  {$ENDIF}
  {dispose of our objects}
  Dispose(TW, Done);
  if not LoadFromStream then begin
    Dispose(Emu, Done);
    Port.Done;
  end;
  FreeMemCheck(CTP, SecondaryCmdSize);                                 {!!.01}
  wStack.Done;                                                         {!!.01}
end.
