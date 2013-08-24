{$S-,R-,F+,V-,O+}

{*********************************************************}
{*                   TERMWIN.PAS 2.03                    *}
{*     Copyright (c) TurboPower Software 1990.           *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit TermWin;
  {-Implements a TerminalWindow object}

interface

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{$IFNDEF UseOpro}
  !! STOP COMPILE - This unit requires that UseOpro be defined in APDEFINE.INC
{$ENDIF}

{$I OPDEFINE.INC}
uses
  ApMisc,
  ApPort,
  ApUart,
  OOCom,
  OOEmu,
  OpRoot,
  {$IFDEF Opro12}
  OpConst,
  {$ENDIF}
  OpInline,
  {$IFDEF UseMouse}
  OpMouse,
  {$ENDIF}
  OpDos,
  OpString,
  OpCrt,
  OpFrame,
  OpCmd,
  {$IFDEF UseDrag}
  OpDrag,
  {$ENDIF}
  {$IFDEF Opro13}                                                      {!!.02}
  OpAbsWin,                                                            {!!.02}
  {$ENDIF}                                                             {!!.02}
  OpWindow;

const
  {options for TerminalWindows}
  twFloatingHView = $00000001;  {horiz view follows cursor through vscreen}
  twFloatingVView = $00000002;  {vertical view follows cursor through vscreen}
  twAdvanceOnClear= $00000004;  {advance view automatically on ClrScr}
  twShowOutgoing  = $00000008;  {echo outgoing characters}
  twWrapInWin     = $00000010;  {wrap text within window}
  twOutCRtoCRLF   = $00000020;  {convert outgoing ^M to ^M^J}
  twInCRtoCRLF    = $00000040;  {convert incoming ^M to ^M^J}
  twOutLFtoCRLF   = $00000080;  {convert outgoing ^J to ^M^J}
  twInLFtoCRLF    = $00000100;  {convert incoming ^J to ^M^J}
  twMapMono       = $00000200;  {map ANSI/VT100 attributes to mono}
  tw7Bits         = $00000400;  {strip high bit of incoming chars}
  twFullScreenZoom= $00000800;  {eliminate border when zooming}
  twInhibitAutoLF = $00002000;  {don't advance after writing col 80}   {!!.02}

  {options for CaptureCommandWindows}
  ctwOverwrite    = $00001000;  {capture should overwrite existing file}

  {internal flags for TerminalWindow family}
  ctwCaptureOn    = $08000000;  {internal flags}
  twWasBordered   = $10000000;
  twCursorIsOn    = $20000000;
  twMouseCursor   = $40000000;
  twZoomed        = $80000000;

  {Aliased bit, just for svsFlags}
  tsvsInhibitAutoLF = $80;      {aliased bit, just for svsFlags}       {!!.02}

  DefTerminalWinOptions : LongInt =
    twFloatingVView + twAdvanceOnClear + twFullScreenZoom;
  BadTerminalWinOptions : LongInt =
    twWasBordered+twCursorIsOn+twMouseCursor+twZoomed+ctwCaptureOn;

  DefScrollBackRows     : Word = 200;
  DefScrollBackCols     : Word = 80;

  ucTermWin = 40;
  ccIncomingChar = ccUser54;    {a special user command used internally}

  {CaptureTerminalWindow constants}
  MinCaptureBufferSize = 128;   {min size of buffer for CaptureTerminalWindows}
  DefCaptureBufferSize : Word = MinCaptureBufferSize;
  DefCaptureFileName = 'CAPTURE.LOG';

type
  TerminalWindowPtr = ^TerminalWindow;
  {#Z+}
  SmartVirtScreenPtr = ^SmartVirtScreen;
  SmartVirtScreen =               {an intelligent virtual screen object}
    object(VirtScreen)
      svsTextAttr     : Byte;
      svsTextChar     : Char;
      svsScrolled     : Boolean;
      svsFlags        : Byte;
      svsX            : Integer;
      svsY            : Integer;
      svsViewX        : Integer;  {view relative to current page}
      svsViewY        : Integer;
      svsViewWidth    : Integer;
      svsPageNum      : Integer;
      svsViewHeight   : Integer;
      svsPageHeight   : Integer;
      constructor Init(Attr : Byte; Rows, Cols : Word;
                       ViewWidth, ViewHeight : Integer;
                       Flags : Byte);
      procedure svsGotoXY(X : Integer; var Y : Integer);
      procedure svsClrScr;
      procedure svsClrPart(X1, Y1, X2, Y2 : Integer);
      procedure svsClrEOL;
      procedure svsPutChar(C : Char);
      procedure svsHome;
      procedure svsWhereXY(var X, Y : Integer);
      procedure svsAdvanceChar;
      procedure svsAdvanceLine;
      procedure svsBackSpace;
      procedure svsBell;
      procedure svsCopyTo;
      procedure svsSetView(X, Y : Integer);
      procedure svsSetPageHeight(PageHeight : Integer);
      procedure svsScroll;
      function svsValidView(X, Y : Integer) : Boolean;
      procedure svsAdjustVView;
      procedure svsAdjustHView;
      procedure svsNoPutChar(C : Char);
      function svsGetLine(LineNo : Word; AsciiFormat : Boolean) : String;
      function svsRowInView(Row : Integer) : Boolean;
      {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load for a SmartVirtScreen object}
      procedure Store(var S : IdStream);
        {-Store for a SmartVirtScreen  object}
      {$ENDIF}
      {---internal methods---}
      procedure svsZeroLocalData;
      procedure svsSetViewSize(Rows, Cols : Word);
      procedure svsGotoXYPrim(X, Y : Integer);
      procedure svsClrPartPrim(X1, Y1, X2, Y2 : Integer);
    end;
  {#Z-}

  TerminalWindow =       {an OPRO CommandWindow with async terminal support}
    object(CommandWindow)
      twOptions        : LongInt;            {TerminalWindow option flags}
      twCursorType     : CursorType;         {our cursor type}
      twPort           : AbstractPortPtr;    {Ptr to the port object}
      twEmulator       : TerminalEmulatorPtr;{Ptr to the emulator object}
      twVScreen        : SmartVirtScreenPtr; {our virtual screen}
      twWFrame         : FramePtr;           {to save active frame on zoom}
      twAFrame         : FramePtr;           {to save alt frame on unzoom}
      twSaveX          : Integer;
      twSaveY          : Integer;
      twSaveViewX      : Integer;
      twSaveViewY      : Integer;
      constructor Init(X1, Y1, X2, Y2 : Byte; Port : AbstractPortPtr);
        {-Create a TerminalWindow}
      constructor InitCustom(X1, Y1, X2, Y2 : Byte;
                             var Colors : ColorSet;
                             WindowOptions : LongInt;
                             Port : AbstractPortPtr;
                             Emu : TerminalEmulatorPtr;
                             ScrollBackRows : Word;
                             ScrollBackCols : Word;
                             TerminalOptions : LongInt);
        {-Create a TerminalWindow window with custom options}
      destructor Done; virtual;
        {-Dispose of a TerminalWindow}
      procedure ProcessSelf; virtual;
        {-Begin processing a TerminalWindow}
      function GetIncomingChar(var Key : Char) : Boolean; virtual;
        {-Returns true if incoming character has arrived at the serial port}
      procedure SendOutgoingChar(C : Char); virtual;
        {-Send a character out the serial port}
      procedure WriteChar(C : Char); virtual;
        {-Write a character to the TerminalWindow with emulation}
      procedure WriteString(S : String);
        {-Write a string to the TerminalWindow with emulation}
      procedure CopyVirtToFile(StartLine, EndLine : Integer;
                               FName : String;
                               StripTrailing : Boolean;
                               AsciiText : Boolean;
                               AppendIfExists : Boolean);
        {-Copy the specified lines of the VirtScreen to a file.}
      procedure SetView(X, Y : Integer);
        {-Set the viewport into virtual screen}
      procedure SetPageSize(PageHeight : Integer);
        {-Set the size of a page into the virtual screen}
      procedure twOptionsOn(Options : LongInt);
        {-Activate multiple options}
      procedure twOptionsOff(Options : LongInt);
        {-Deactivate multiple options}
      function twOptionsAreOn(Options : LongInt) : Boolean;
        {-Return True if all specified options are on}

      procedure SetTerminalWinPort(NewPort : AbstractPortPtr);
        {-Change the internal Port object}
      procedure SetTerminalWinEmulator(NewEmu : TerminalEmulatorPtr);
        {-Change the internal Emulator object}
      function GetTerminalWinPort : AbstractPortPtr;
        {-Return a pointer to port object}
      function GetTerminalWinEmulator : TerminalEmulatorPtr;
        {-Return a pointer to emulator object}

      {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a TerminalWindow object from a stream}
      procedure Store(var S : IdStream);
        {-Store a TerminalWindow object to a stream}
      {$ENDIF}

      {#Z+}
      {...internal methods...}
      procedure twCursorOn(On : Boolean);
        {-Turn TerminalWindow's cursor on or off}
      function twIsCursorOn : Boolean;
        {-Return True if TerminalWindow's cursor is on}
      procedure twPutChar(C : Char);
        {-Put a character to the TerminalWindow without emulation}
      procedure twPutString(S : String);
        {-Put a string to the TerminalWindow without emulation}
      {$IFDEF UseAdjustableWindows}
      procedure Zoom; virtual;
        {-Expand TerminalWindow to full screen}
      procedure UnZoom; virtual;
        {-Shrink a TerminalWindow to its normal size}
      procedure AdjustWindow(X1, Y1, X2, Y2 : Word); virtual;
        {-Set new coordinates and adjust all related structures}
      {$ENDIF}
      procedure GetNextCommand; virtual;
        {-Return the next command from port or keyboard}
      procedure twGotoXY(X, Y : Integer);
        {-Goto coordinates X, Y. Coordinates are absolute.}
      procedure twSwapTextAttr;                                        {!!.02}
        {-If Inverse is set, swap textattr nibbles}                    {!!.02}
      procedure twClrScr;
        {-Clear the TerminalWindow}
      procedure twClrPart(X1, Y1, X2, Y2 : Integer);
        {-Clear part of the TerminalWindow. Coordinates are absolute.}
      procedure twClrEOL;
        {-Clear from cursor position to the end of current row}
      function twVScreenCursorInWin : Boolean;
        {-Return True if vscreen's cursor is in our window}
      procedure twSetTextAttr(Attr : Byte);
        {-Set Text Attribute used for output}
      procedure twRelGotoXY(X, Y : Integer);
        {-Convert vscreen absolute X Y coordinates to window relative}
      procedure UpdateContents; virtual;
        {-Update the window from the vscreen}
    {$IFDEF UseScrollBars}
      procedure twSetupForScrollBars;
        {-Set boundaries for all scroll bars}
    {$ENDIF}
      function twScrollBarMaxX : LongInt;
        {-Return maximum column position for ChangeScrollBar}
      function twScrollBarMaxY : LongInt;
        {-Return maximum row position for ChangeScrollBar}
    {$IFDEF UseMouse}
      function twProcessMouseCommand(Cmd : Word) : Boolean;
        {-Process ccMouseSel command. Returns True to return control to user.}
    {$ENDIF}

      procedure twScrollVertical(Delta : Integer);
        {-Scroll window vertically}
      procedure twScrollHorizontal(Delta : Integer);
        {-Scroll window horizontally}
      procedure twFastWrite(C : Char);
      function twInWindow(X, Y : Integer) : Boolean;
        {-Return true if absolute X, Y in window}
      procedure twSaveFrameState;
        {-Save the active and alternate frame type}
      procedure twRestoreFrameState;
        {-Restore the active and alternate frame type}
      procedure twAdvanceCursor;
        {-Advance cursor by character}
      procedure twBackspace;
        {-Backspace one character}
      procedure twAdvanceLine;
        {-Advance one line}
      procedure twCarriageReturn;
        {-move cursor to start of line}
      procedure twSaveCursorPos;
      procedure twRestoreCursorPos;
      procedure twScaleViewForZoom;
      procedure twScaleViewForUnZoom;
      procedure twUpdateContents;
      procedure twGotoXYAdjust(X, Y : Integer);
      function twValidView(X, Y : Integer) : Boolean;
      {$IFDEF UseMouse}
      function twMouseInWin : Boolean;
      {$ENDIF}
      procedure twZeroOut;
      {#Z-}
    end;

  {type relating to CaptureTerminalWindows}
  CaptureBuffer = Array[1..$FFF1] of Char;
  CaptureBufferPtr = ^CaptureBuffer;
  CaptureTerminalWindowPtr = ^CaptureTerminalWindow;
  CaptureTerminalWindow =    {a TerminalWindow with support to capture
                              characters to a file}
    object(TerminalWindow)
      ctwSize            : Word;
      ctwIndex           : Word;
      ctwCaptureFileName : String;
      ctwFile            : File;
      ctwBufferPtr   : CaptureBufferPtr;
      constructor Init(X1, Y1, X2, Y2 : Byte;
                       Port : AbstractPortPtr);
        {-Create a CaptureTerminalWindow}
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
        {-Create a CaptureTerminalWindow window with custom options}
      destructor Done; virtual;
        {-Dispose of a CaptureTerminalWindow}
      function GetIncomingChar(var Key : Char) : Boolean; virtual;
        {-Returns true if incoming character has arrived at the serial port}
      procedure SetCaptureFileName(FName : String);
        {-Set the name of the Capture file}
      function GetCaptureFileName : String;
        {-Get the name of the Capture file}
      procedure Capture(On : Boolean);
        {-Turn Capture on or off}
      function CaptureIsOn : Boolean;
        {-Return True if capture is on}

      {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a CaptureTerminalWindow object from a stream}
      procedure Store(var S : IdStream);
        {-Store a CaptureTerminalWindow object to a stream}
      {$ENDIF}

      {#Z+}
      {...internal methods...}
      procedure ctwFlushCapture;
        {-Commit data in the capture buffer to disk}
      procedure ctwPutCapture(C : Char); virtual;
        {-Put a character to the capture buffer}
      procedure ctwOpenCapture; virtual;
        {-open the capture file (primarily for internal use by Capture method)}
      procedure ctwCloseCapture; virtual;
        {-close the capture file (primarily for internal use by Capture method)}
      {#Z-}
    end;

var
  {default command processor for TerminalWindows}
  {$IFDEF UseDrag}
  TerminalCommands : DragProcessor;
  {$ELSE}
  TerminalCommands : CommandProcessor;
  {$ENDIF}

{$IFDEF UseStreams}

{#Z+}
procedure SmartVirtScreenStream(SPtr : IdStreamPtr);
  {-Register all types needed for streams containing SmartVirtScreens}
{#Z-}

procedure TerminalWindowStream(SPtr : IdStreamPtr);
  {-Register all types needed for streams containing TerminalWindows}

procedure CaptureTerminalWindowStream(SPtr : IdStreamPtr);
  {-Register all types needed for streams containing CaptureTerminalWindows}

{$ENDIF}

{$I TERMWIN.PCD}     {Terminal window configuration data}

  {===========================================================}

implementation

  procedure SmartVirtScreen.svsZeroLocalData;
  begin
    svsHome;
    svsViewX := 1;
    svsViewY := 1;
    svsViewWidth := 1;
    svsViewHeight := 1;
    svsPageNum := 0;
    svsPageHeight := ScreenHeight;
  end;

  constructor SmartVirtScreen.Init(Attr : Byte;
                                   Rows, Cols : Word;
                                   ViewWidth, ViewHeight : Integer;
                                   Flags : Byte);
  begin
    {initialize variables}
    svsZeroLocalData;
    {allocate the virtual screen}
    if not VirtScreen.Alloc(Rows, Cols) then
      Fail;
    svsTextAttr := Attr;
    svsTextChar := ' ';
    svsViewWidth := ViewWidth;
    svsViewHeight := ViewHeight;
    svsScrolled := False;
    svsFlags := Flags;
    svsClrScr;
  end;

  procedure SmartVirtScreen.svsGotoXY(X : Integer; var Y : Integer);
  begin
    if ByteFlagIsSet(svsFlags, twAdvanceOnClear) then
      Y := ((svsY div svsPageHeight) * svsPageHeight) + Y;
    {goto X, Y on virtual screen (does nothing if x or y is invalid)}
    if (X <= vsWidth) and (Y <= vsHeight) then begin
      svsX := X;
      svsY := Y;
    end;
  end;

  procedure SmartVirtScreen.svsGotoXYPrim(X, Y : Integer);
  begin
    {goto X, Y on virtual screen (does nothing if x or y is invalid)}
    if (X <= vsWidth) and (Y <= vsHeight) then begin
      svsX := X;
      svsY := Y;
    end;
  end;

  procedure SmartVirtScreen.svsClrEOL;
  begin
    {clear from cursor to the end of the line on vscreen}
    svsClrPart(svsX, svsY, vsWidth, svsY);
  end;

  procedure SmartVirtScreen.svsClrScr;
  var
    NewRow, Bottom, I, Page : Integer;
    H : Word;
  begin
    if ByteFlagIsSet(svsFlags, twAdvanceOnClear) and
       (svsY > 1) and (svsPageHeight > 0) then begin

      H := vsHeight;
      Page := (svsY + (svsPageHeight - 1)) div svsPageHeight;
      NewRow := Succ(Page * svsPageHeight);

      if NewRow > H then begin
        for I := 1 to svsPageHeight do
          svsScroll;
        NewRow := (H - svsPageHeight);
      end;

      {!!.02 modify start}
      Bottom := NewRow + svsPageHeight;
      if Bottom > H then begin
        for I := Bottom downto H do
          svsScroll;
        Bottom := H;
        NewRow := (H - svsPageHeight);
      end;
      {!!.02 modify end}

      svsClrPartPrim(1, NewRow, vsWidth, Bottom);
      if ByteFlagIsSet(svsFlags, twFloatingVView) then begin
        svsSetView(svsViewX, NewRow);
        svsScrolled := True;
      end;
      svsGotoXYPrim(1, NewRow);
    end
    else begin
      {clear entire vscreen and home the cursor}
      Clear(svsTextAttr, svsTextChar);
      svsHome;
    end;
  end;

  procedure SmartVirtScreen.svsSetViewSize(Rows, Cols : Word);
  begin
    svsViewWidth  := Cols;
    svsViewHeight := Rows;
  end;

  procedure SmartVirtScreen.svsClrPart(X1, Y1, X2, Y2 : Integer);
  var
    Width, Height, I : Word;
    S : String;
  begin
    {!!.02 removed
    if ByteFlagIsSet(svsFlags, twAdvanceOnClear) then begin
      Height := (((svsY + (svsPageHeight - 1)) div svsPageHeight) * svsPageHeight);
      Y1 := Height + Y1;
      Y2 := Height + Y2;
    end;}
    svsClrPartPrim(X1, Y1, X2, Y2);
  end;

  procedure SmartVirtScreen.svsClrPartPrim(X1, Y1, X2, Y2 : Integer);
  var
    Width, Height, I : Word;
    S : String;
  begin
    {calc width of area to clear}
    Width  := (X2 - X1) + 1;
    {calc height of area to clear}
    Height := (Y2 - Y1) + 1;
    {if invalid parameters, then exit}
    if (Height > vsHeight) or (Height = 0) or
       (Width > vsWidth) or (Width = 0) then
      Exit;
    {construct a string of appropriate width}
    S := CharStr(svsTextChar, Width);
    {clear the rectangle by writing the string height times}
    for I := 0 to Height-1 do
      WriteTo(S, Y1+I, X1, svsTextAttr);
    {position cursor at top left corner of rectangle}
    svsGotoXYPrim(X1, Y1);
  end;

  procedure SmartVirtScreen.svsBackSpace;
  var
    X, Y : Integer;
  begin
    {are we already in leftmost column?}
    if svsX = 1 then begin
      {yes, so are we on a line other than the top line?}
      if svsY > 1 then begin
        {yes, so decrement row and position in rightmost column}
        svsX := vsWidth;
        Dec(svsY);
      end
      else
        Exit; {already at 1, 1}
    end
    else
      Dec(svsX); {simply decrement column}
    X := svsX;
    Y := svsY;
    svsPutChar(svsTextChar); {wipe out the previous character}
    svsX := X;
    svsY := Y;
  end;

  procedure SmartVirtScreen.svsNoPutChar(C : Char);
  begin
    case C of
      ^M : svsX := 1;           {carriage return}
      ^J : svsAdvanceLine;      {line feed}
      ^H : svsBackSpace;        {backspace}
      else                      {regular character}
        svsAdvanceChar;
    end;
  end;

  procedure SmartVirtScreen.svsBell;
  begin
    Sound(440);
    Delay(70);
    NoSound;
    Delay(10);
  end;

  procedure SmartVirtScreen.svsPutChar(C : Char);
    {-Put the character to the virtual screen and advance internal cursor}
  type
    WordPtr = ^Word;
  var
    VideoOfst : Word;
  begin
    case C of
      ^M : svsX := 1;           {carriage return}
      ^J : svsAdvanceLine;      {line feed}
      ^H : svsBackSpace;        {backspace}
      ^G : svsBell;             {bell}
      else begin                {regular character}
        VideoOfst := ((Word(svsY)-1) * vsWidth + (svsX-1)) shl 1;
        WordPtr(Ptr(vsVideoSeg, VideoOfst))^ := (Word(svsTextAttr) shl 8) +
                                                 Ord(C);
        svsAdvanceChar;
      end;
    end;
  end;

  procedure SmartVirtScreen.svsHome;
  begin
    svsX := 1;
    svsY := 1;
  end;

  procedure SmartVirtScreen.svsCopyTo;
  begin
    CopyToWindow(svsViewY, svsViewX);
  end;


  function SmartVirtScreen.svsValidView(X, Y : Integer) : Boolean;
  begin
    svsValidView := (X <= vsWidth) and (X > 0) and (Y <= vsHeight) and (Y > 0);
  end;

  procedure SmartVirtScreen.svsSetView(X, Y : Integer);
  begin
    if svsValidView(X, Y) then begin
      svsViewX := X;
      svsViewY := Y;
    end;
  end;

  procedure SmartVirtScreen.svsSetPageHeight(PageHeight : Integer);
  begin
    if (PageHeight <= vsHeight) then
      svsPageHeight := PageHeight;
  end;

  procedure SmartVirtScreen.svsWhereXY(var X, Y : Integer);
  begin
    X := svsX;
    Y := svsY;
  end;

  procedure SmartVirtScreen.svsAdvanceChar;
  begin
    {are we at the right edge?}
    if svsX = vsWidth then begin
      {yes, so wrap back to first column}
      svsX := 1;
      if not ByteFlagIsSet(svsFlags, tsvsInhibitAutoLF) then           {!!.02}
        svsAdvanceLine;
    end
    else
      Inc(svsX);  {simply advance to next column}
    if ByteFlagIsSet(svsFlags, twFloatingHView) then
      svsAdjustHView;
  end;

  procedure SmartVirtScreen.svsAdvanceLine;
  begin
    {are there more rows?}
    if svsY < vsHeight then begin
      Inc(svsY);  {yes, so increment row}
      if ByteFlagIsSet(svsFlags, twFloatingVView) then
        svsAdjustVView;
    end
    else
      svsScroll;  {no, so scroll screen}
  end;

  procedure SmartVirtScreen.svsScroll;
  begin
    ScrollVert(1, svsTextAttr, TextChar);
    svsScrolled := True;     {set flag so TerminalWindow knows we scrolled}
  end;

  function SmartVirtScreen.svsRowInView(Row : Integer) : Boolean;
  var
    Y : Integer;
  begin
    Y := svsViewY + svsViewHeight - 1;
    if Y > vsHeight
      then Y := vsHeight - 1;
    svsRowInView := (Row >= svsViewY) and (Row <= Y);
  end;

  procedure SmartVirtScreen.svsAdjustVView;
  var
    Y : Integer;
  begin
    if not svsRowInView(svsY) then begin
      Y := (svsY - svsViewHeight) + 1;
      if Y < 1 then
        Y := 1;
      svsSetView(svsViewX, Y);
      svsScrolled := True;
    end
  end;

  procedure SmartVirtScreen.svsAdjustHView;
  var
    X : Integer;
  begin
    X := (svsX - svsViewWidth) + 1;
    if X < 1 then
      X := 1;
    if (X <> svsViewX) then begin
      svsSetView(X, svsViewY);
      svsScrolled := True;
    end;
  end;

  function SmartVirtScreen.svsGetLine(LineNo : Word;
                                      AsciiFormat : Boolean) : String;
  type
    CharPos =
      record
        C : Char;
        A : Byte;
      end;
    CharPosPtr = ^CharPos;
  var
    CP : CharPosPtr;
    S  : String;
    W  : Word;
    I  : Word;
  begin
    S  := '';
    W  := vsWidth;
    CP := CharPosPtr(Ptr(vsVideoSeg, ((LineNo-1) * W) shl 1));
    if (AsciiFormat and (W > SizeOf(String)-1)) or
       ((not AsciiFormat) and (W > (SizeOf(String)-1) div 2)) then begin
      {no can do, vscreen width too long}
      svsGetLine := '';
      Exit;
    end;
    for I := 1 to W do begin
      if AsciiFormat then
        S := S + CP^.C
      else begin
        Move(CP^, S[Length(S) + 1], SizeOf(CP^));
        Inc(Byte(S[0]), SizeOf(CP^));
      end;
      Inc(Word(CP), SizeOf(CP^));
    end;
    svsGetLine := S;
  end;

  {$IFDEF UseStreams}
  constructor SmartVirtScreen.Load(var S : IdStream);
  begin
    if not VirtScreen.Load(S) then
      Fail;
    svsZeroLocalData;
    S.ReadRange(svsTextAttr, svsPageHeight);
    S.Read(svsPageHeight, 2);
    if S.PeekStatus <> 0 then begin
      Done;
      Fail;
    end;
    svsClrScr;
  end;

  procedure SmartVirtScreen.Store(var S : IdStream);
  begin
    VirtScreen.Store(S);
    S.WriteRange(svsTextAttr, svsPageHeight);
    S.Write(svsPageHeight, 2);
  end;

  procedure SmartVirtScreenStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing SmartVirtScreens}
  begin
    VirtScreenStream(SPtr);
    with SPtr^ do
      RegisterType(otSmartVirtScreen, veSmartVirtScreen,
                   TypeOf(SmartVirtScreen),
                   @SmartVirtScreen.Store, @SmartVirtScreen.Load);
  end;
  {$ENDIF}

  constructor TerminalWindow.Init(X1, Y1, X2, Y2 : Byte;
                                  Port : AbstractPortPtr);
  begin
    {call InitCustom constructor to do the job}
    if not TerminalWindow.InitCustom(X1, Y1, X2, Y2,
                                       DefaultColorSet,
                                       DefWindowOptions,
                                       Port, Nil,
                                       DefScrollBackRows,
                                       DefScrollBackCols,
                                       DefTerminalWinOptions
                                       ) then
      Fail;
  end;
  constructor TerminalWindow.InitCustom(X1, Y1, X2, Y2 : Byte;
                                        var Colors : ColorSet;
                                        WindowOptions : LongInt;
                                        Port : AbstractPortPtr;
                                        Emu : TerminalEmulatorPtr;
                                        ScrollBackRows : Word;
                                        ScrollBackCols : Word;
                                        TerminalOptions : LongInt);

  begin
    twOptions := TerminalOptions;
    twPort    := Port;
    twEmulator:= Emu;
    twSaveX   := 0;
    twSaveY   := 0;
    twWFrame  := Nil;
    twAFrame  := Nil;
    twCursorType := cuNormal;
    twVScreen := Nil;

    SetLongFlag(WindowOptions, wUserContents);
    if not CommandWindow.InitCustom(X1, Y1, X2, Y2,
                                    Colors, WindowOptions,
                                    TerminalCommands, ucTermWin) then
      Fail;

    with Colors do
      New(twVScreen, Init(ColorMono(TextColor, TextMono),
                          ScrollBackRows, ScrollBackCols,
                          Succ(X2 - X1), Succ(Y2 - Y1),
                          Byte(TerminalOptions)));
    if twVScreen = Nil then begin
      if InitStatus = ecWinCoordsBad then
        InitStatus := ecScrollBackTooBig;
      Done;
      Fail;
    end;

    if TerminalOptions and twInhibitAutoLF = twInhibitAutoLF then      {!!.02}
      twOptionsOn(tsvsInhibitAutoLF);                                  {!!.02}
  end;

  destructor TerminalWindow.Done;
  begin
    if twWFrame <> Nil then
      Dispose(twWFrame, Done);
    if twAFrame <> Nil then
      Dispose(twAFrame, Done);
    if twVScreen <> Nil then
      Dispose(twVScreen, Done);
    CommandWindow.Done;
  end;

  function TerminalWindow.GetIncomingChar(var Key : Char) : Boolean;
  var
    C : Char;
  begin
    with twPort^ do
      if CharReady then begin
        GetChar(C);
        if LongFlagIsSet(twOptions, tw7Bits) then
          Key := Char(Ord(C) and $7F)
        else
          Key := C;
        GetIncomingChar := True;
      end
      else
        GetIncomingChar := False;
  end;

  procedure TerminalWindow.SendOutgoingChar(C : Char);
  begin
    if LongFlagIsSet(twOptions, twShowOutgoing) then                   {!!.02}
      WriteChar(C);                                                    {!!.02}
    twPort^.PutChar(C);
  end;

  procedure TerminalWindow.GetNextCommand;
  var
    C : Char;
  begin
    while not cwCmdPtr^.cpKeyPressed do
      if GetIncomingChar(C) then begin
        cwCmd := ccIncomingChar;
        cwKey := Word(C);
        Exit;
      end;
    cwCmdPtr^.cpOptionsOn(cpCtrlsAreChars);
    CommandWindow.GetNextCommand;
  end;

  procedure TerminalWindow.twScaleViewForZoom;
  begin
    {$IFDEF UseAdjustableWindows}
    with twVScreen^ do begin
      twSaveViewX := svsViewX;
      twSaveViewY := svsViewY;
      SetView(1, twSaveViewY + ((wMaxYH - wYH) + 1));
      if ByteFlagIsSet(svsFlags, twFloatingVView) then
        svsAdjustVView;
      if ByteFlagIsSet(svsFlags, twFloatingHView) then
        svsAdjustHView;
    end;
    {$ENDIF}
  end;

  procedure TerminalWindow.twScaleViewForUnZoom;
  begin
    {$IFDEF UseAdjustableWindows}
    SetView(twSaveViewX, twSaveViewY);
    with twVScreen^ do begin
      if ByteFlagIsSet(svsFlags, twFloatingVView) then
        svsAdjustVView;
      if ByteFlagIsSet(svsFlags, twFloatingHView) then
        svsAdjustHView;
    end;
    {$ENDIF}
  end;

  {$IFDEF UseAdjustableWindows}
  procedure TerminalWindow.Zoom;
  var
    Active : Boolean;
  begin
    if not LongFlagIsSet(twOptions, twFullScreenZoom) then begin
      CommandWindow.Zoom;
      twScaleViewForZoom;
      Exit;
    end;
    if not IsZoomed then begin
      SetLongFlag(twOptions, twZoomed);
      Active := IsActive;
      if Active then begin
        ActivateWrite;
        {$IFDEF UseMouse}
        if cwCmdPtr^.MouseEnabled and
           (not wOptionsAreOn(wAllMouseEvents)) then begin
          ClearFlag(cwCmdPtr^.cpOptions, cpEnableMouse);
          SetLongFlag(twOptions, twMouseCursor)
        end
        else
          ClearLongFlag(twOptions, twMouseCursor);
        {$ENDIF}
        Erase;
      end;
      if LongFlagIsSet(wFlags, wBordered) then begin
        twSaveFrameState;
        SetLongFlag(twOptions, twWasBordered);
        wOptionsOff(wBordered);
      end
      else
        ClearLongFlag(twOptions, twWasBordered);
      twVScreen^.svsSetViewSize(Width, Height);
      CommandWindow.Zoom;
      twScaleViewForZoom;
      if Active then begin
        with twVScreen^ do
          twRelGotoXY(svsX, svsY);
        Draw;
        DeactivateWrite;

        twCursorOn(twVScreenCursorInWin)
      end;
    end;
  end;

  procedure TerminalWindow.UnZoom;
  var
    Active : Boolean;
  begin
    if IsZoomed then begin
      if not LongFlagIsSet(twOptions, twZoomed) then begin
        CommandWindow.Unzoom;
        twScaleViewForUnZoom;
        Exit;
      end;
      ClearLongFlag(twOptions, twZoomed);
      Active := IsActive;
      if Active then begin
        ActivateWrite;
        {$IFDEF UseMouse}
        if LongFlagIsSet(twOptions, twMouseCursor) then begin
          ClearLongFlag(twOptions, twMouseCursor);
          SetFlag(cwCmdPtr^.cpOptions, cpEnableMouse);
        end;
        {$ENDIF}
        Erase;
      end;
      CommandWindow.UnZoom;
      if LongFlagIsSet(twOptions, twWasBordered) then begin
        wOptionsOn(wBordered);
        twRestoreFrameState;
      end;
      twScaleViewForUnZoom;
      if Active then begin
        with twVScreen^ do
          twRelGotoXY(svsX, svsY);
        Draw;
        DeactivateWrite;

        twCursorOn(twVScreenCursorInWin)
      end;
    end;
  end;

  procedure TerminalWindow.AdjustWindow(X1, Y1, X2, Y2 : Word);
  var
    XB1, XB2, YB1, YB2 : Byte;
  begin
    CommandWindow.AdjustWindow(X1, Y1, X2, Y2);
    Coordinates(XB1, YB1, XB2, YB2);
    twVScreen^.svsSetViewSize(Succ(YB2 - YB1), Succ(XB2 - XB1));
  end;
  {$ENDIF}

  procedure TerminalWindow.twSetTextAttr(Attr : Byte);
  var
    A : Byte;
  begin
    if LongFlagIsSet(twOptions, twMapMono) then
      A := MapColor(Attr)
    else
      A := Attr;
    twVScreen^.svsTextAttr := ColorMono(Attr, A);
    SetTextAttr(Attr, A);
  end;

  procedure TerminalWindow.twUpdateContents;
  begin
    twVScreen^.svsCopyTo;
    with twVScreen^ do begin
      if twInWindow(svsX, svsY) then
        twRelGotoXY(svsX, svsY);
      {$IFDEF UseScrollBars}
      DrawAllSliders(svsViewX, svsViewY);
      {$ENDIF}
    end;
  end;

  procedure TerminalWindow.UpdateContents;
  begin
    twUpdateContents;
    CommandWindow.UpdateContents;
  end;

  procedure TerminalWindow.twGotoXY(X, Y : Integer);
  begin
    if twInWindow(X, Y) and IsActive then
      twRelGotoXY(X, Y);
    twVScreen^.svsGotoXYPrim(X, Y);
  end;

  procedure TerminalWindow.twGotoXYAdjust(X, Y : Integer);
  begin
    with twVScreen^ do begin                                           {!!.02}
      svsGotoXY(X, Y);                                                 {!!.02}
      if twInWindow(svsX, svsY) and IsActive then                      {!!.02}
        twRelGotoXY(X, Y);                                             {!!.02}
    end;                                                               {!!.02}
  end;

  procedure TerminalWindow.twRelGotoXY(X, Y : Integer);
  begin
    with twVScreen^ do begin
      X := X - (svsViewX - 1);
      Y := Y - (svsViewY - 1);
      wGotoXY(X, Y);
    end;
  end;

  function TerminalWindow.twInWindow(X, Y : Integer) : Boolean;
  var
    XX, YY : Integer;
  begin
    with twVScreen^ do begin
      XX := (X - (svsViewX - 1)) + (Integer(wXL) - 1);
      YY := (Y - (svsViewY - 1)) + (Integer(wYL) - 1);
    end;
    if (XX > 0) and (YY > 0) then
      twInWindow := AbstractWindow.InWindow(Word(XX), Word(YY))
    else
      twInWindow := False;
  end;

  {!!.02}
  procedure TerminalWindow.twSwapTextAttr;
    {-If Inverse it set, swap text attr nibbles}
  var
    Attr : Byte;
  begin
    if FlagIsSet(twEmulator^.teOptions, teIsAnsi) then
      if AnsiEmulatorPtr(twEmulator)^.Inverse then begin
        Attr := ColorMono(wTextColor, wTextMono);
        Attr := ((Attr shl 4) or (Attr shr 4));
        if twEmulator^.teOptionsAreOn(teMaskBlinkOnInverse) then       {!!.03}
          Attr := Attr and $7F;                                        {!!.03}
        twSetTextAttr(Attr);
      end;
  end;

  procedure TerminalWindow.twClrScr;
  begin
    twSwapTextAttr;                                                    {!!.02}
    twVScreen^.svsClrScr;
    if IsActive then
      twVScreen^.svsCopyTo;
    twSwapTextAttr;                                                    {!!.02}
  end;

  procedure TerminalWindow.twClrPart(X1, Y1, X2, Y2 : Integer);
  begin
    twSwapTextAttr;                                                    {!!.02}
    twVScreen^.svsClrPart(X1, Y1, X2, Y2);
    if IsActive then
      twVScreen^.svsCopyTo;
    twSwapTextAttr;                                                    {!!.02}
  end;

  procedure TerminalWindow.twClrEOL;
  var
    X, Y : Integer;
  begin
    twSwapTextAttr;                                                    {!!.02}
    with twVScreen^ do begin
      svsWhereXY(X, Y);
      if twInWindow(X, Y) and IsActive then begin
        twRelGotoXY(X, Y);
        ClrEOL;
      end;
      svsClrEOL;
    end;
    twSwapTextAttr;                                                    {!!.02}
  end;

  procedure TerminalWindow.twAdvanceCursor;
    {-Advance cursor by character}
  var
    X, Y : Integer;
  begin
    wWhereXY(X, Y);
    if X < Width then begin
      Inc(X);
      wGotoXY(X, Y);
    end
    else begin
      wGotoXY(1, Y);
      twAdvanceLine;
    end;
  end;

  procedure TerminalWindow.twBackspace;
    {-Backspace one character}
  var
    X, Y : Integer;
  begin
    wWhereXY(X, Y);
    if X > 1 then
      Dec(X)
    else
      if Y > 1 then begin
        Dec(Y);
        X := Width;
      end;
    wGotoXY(X, Y);
    twFastWrite(twVScreen^.svsTextChar);
    wGotoXY(X, Y);
  end;

  procedure TerminalWindow.twAdvanceLine;
    {-Advance one line}
  var
    X, Y : Integer;
  begin
    wWhereXY(X, Y);
    if Y = Height then begin
      if LongFlagIsSet(twOptions, twWrapInWin) then
        ScrollVert(1);
      Exit;
    end
    else
      Inc(Y);
    wGotoXY(X, Y);
  end;

  procedure TerminalWindow.twCarriageReturn;
    {-Advance one line}
  var
    X, Y : Integer;
  begin
    wWhereXY(X, Y);
    wGotoXY(1, Y);
  end;

  procedure TerminalWindow.twFastWrite(C : Char);
  begin
    case C of
      ^J : twAdvanceLine;
      ^M : twCarriageReturn;
      ^H : twBackspace;
      ^G : twVScreen^.svsBell;
      else begin
        FastFill(1, C, WhereYAbs, WhereXAbs, twVScreen^.svsTextAttr);
        twAdvanceCursor;
      end;
    end;
  end;

  procedure TerminalWindow.twPutString(S : String);
  var
    I : Byte;
    X, Y : Integer;
    C : Char;
    WasCurrent : Boolean;
    WasActive  : Boolean;
    {$IFDEF UseMouse}
    MouseHere, MouseState : Boolean;
    {$ENDIF}
  begin
    if Length(S) = 0 then
      Exit;
    WasActive  := IsActive;
    WasCurrent := IsCurrent or (not WasActive);
    if not WasCurrent then
      ActivateWrite;
    {$IFDEF UseMouse}
    if cwCmdPtr^.MouseEnabled and WasActive then begin
      MouseHere := twMouseInWin;
      if MouseHere then
        HideMousePrim(MouseState);
    end
    else
      MouseHere := False;
    {$ENDIF}

    for I := 1 to Length(S) do begin
      C := S[I];
      with twVScreen^ do
        if LongFlagIsSet(twOptions, twWrapInWin) then begin
          svsPutChar(C);
          if WasActive then
            twFastWrite(C);
        end
        else begin
          svsWhereXY(X, Y);
          svsPutChar(C);
          if svsScrolled then begin
            if WasActive then
              twUpdateContents;
            svsScrolled := False;
          end;
          if twInWindow(X, Y) and WasActive then begin
            twRelGotoXY(X, Y);
            if not twIsCursorOn then
              twCursorOn(True);
            twFastWrite(C);
          end
          else
            if WasActive then
              twCursorOn(False);
        end;
    end;
    if not WasCurrent then
      DeactivateWrite;
    {$IFDEF UseMouse}
    if MouseHere then
      ShowMousePrim(MouseState);
    {$ENDIF}
  end;

  procedure TerminalWindow.twPutChar(C : Char);
  var
    X, Y : Integer;
    WasCurrent : Boolean;
    WasActive  : Boolean;
    {$IFDEF UseMouse}
    MouseHere, MouseState : Boolean;
    {$ENDIF}
  begin
    WasActive  := IsActive;
    WasCurrent := IsCurrent or (not WasActive);
    if not WasCurrent then
      ActivateWrite;
    {$IFDEF UseMouse}
    if cwCmdPtr^.MouseEnabled and WasActive then begin
      MouseHere := twMouseInWin;
      if MouseHere then
        HideMousePrim(MouseState);
    end
    else
      MouseHere := False;
    {$ENDIF}
    with twVScreen^ do
      if LongFlagIsSet(twOptions, twWrapInWin) then begin
        svsPutChar(C);
        if WasActive then
          twFastWrite(C);
      end
      else begin
        svsWhereXY(X, Y);
        svsPutChar(C);
        if svsScrolled then begin
          if WasActive then
            twUpdateContents;
          svsScrolled := False;
        end;
        if twInWindow(X, Y) and WasActive then begin
          twRelGotoXY(X, Y);
          if not twIsCursorOn then
            twCursorOn(True);
          twFastWrite(C);
        end
        else
          if WasActive then
            twCursorOn(False);
      end;
    if not WasCurrent then
      DeactivateWrite;
    {$IFDEF UseMouse}
    if MouseHere then
      ShowMousePrim(MouseState);
    {$ENDIF}
  end;

  procedure TerminalWindow.twSaveCursorPos;
  begin
    twVScreen^.svsWhereXY(twSaveX, twSaveY);
  end;

  procedure TerminalWindow.twRestoreCursorPos;
  begin
    twGotoXY(twSaveX, twSaveY);
  end;

  procedure TerminalWindow.twScrollVertical(Delta : Integer);
  var
    MaxY : Integer;
  begin
    with twVScreen^ do begin
      Delta := svsViewY + Delta;
      if (Delta < 1) then
        Delta := 1
      else begin
        MaxY := twScrollBarMaxY;
        if Delta > MaxY then
          Delta := MaxY;
      end;
      if Delta = svsViewY then
        Exit;
      SetView(svsViewX, Delta);
      twUpdateContents;
    end;
  end;

  procedure TerminalWindow.twScrollHorizontal(Delta : Integer);
  var
    MaxX : Integer;
  begin
    with twVScreen^ do begin
      Delta := svsViewX + Delta;
      if (Delta < 1) then
        Delta := 1
      else begin
        MaxX := twScrollBarMaxX;
        if Delta > MaxX then
          Delta := MaxX;
      end;
      if Delta = svsViewX then
        Exit;
      SetView(Delta, svsViewY);
      twUpdateContents;
    end;
  end;

  procedure TerminalWindow.twCursorOn(On : Boolean);
  begin
    if not IsCurrent then
      Exit;
    if On then begin
      SetLongFlag(twOptions, twCursorIsOn);
      SetCursor(twCursorType);
    end
    else begin
      ClearLongFlag(twOptions, twCursorIsOn);
      SetCursor(cuHidden);
    end;
  end;

  function TerminalWindow.twIsCursorOn : Boolean;
  begin
    twIsCursorOn := LongFlagIsSet(twOptions, twCursorIsOn);
  end;

  function TerminalWindow.twVScreenCursorInWin : Boolean;
  begin
    with twVScreen^ do
      twVScreenCursorInWin := twInWindow(svsX, svsY);
  end;

  function TerminalWindow.twValidView(X, Y : Integer) : Boolean;
  begin
    twValidView := (X <= twScrollBarMaxX) and (Y <= twScrollBarMaxY) and
                   (X > 0) and (Y > 0);
  end;

  procedure TerminalWindow.SetView(X, Y : Integer);
    {-Set the viewport into virtual screen}
  begin
    with twVScreen^ do
      if twValidView(X, Y) then
        svsSetView(X, Y);
  end;

  procedure TerminalWindow.twOptionsOn(Options : LongInt);
  begin
    SetLongFlag(twOptions, Options);
    twVScreen^.svsFlags := twVScreen^.svsFlags or Byte(Options);
    if Options and twInhibitAutoLF = twInhibitAutoLF then              {!!.02}
      twVScreen^.svsFlags := twVScreen^.svsFlags or tsvsInhibitAutoLF; {!!.02}
  end;

  procedure TerminalWindow.twOptionsOff(Options : LongInt);
  begin
    ClearLongFlag(twOptions, Options);
    twVScreen^.svsFlags := twVScreen^.svsFlags and (not Byte(Options));
    if Options and twInhibitAutoLF = twInhibitAutoLF then              {!!.02}
      twVScreen^.svsFlags := twVScreen^.svsFlags and not tsvsInhibitAutoLF; {!!.02}
  end;

  function TerminalWindow.twOptionsAreOn(Options : LongInt) : Boolean;
  begin
    twOptionsAreOn := ((twOptions and Options) = Options);
  end;

  procedure TerminalWindow.SetPageSize(PageHeight : Integer);
  begin
    twVScreen^.svsSetPageHeight(PageHeight);
  end;

  procedure TerminalWindow.twSaveFrameState;
    {-Save the active and alternate frame type}
  begin
    New(twWFrame, fCopy(wFrame));
    New(twAFrame, fCopy(aFrame));
  end;

  procedure TerminalWindow.twRestoreFrameState;
    {-Restore the active and alternate frame type}
  begin
    wFrame.Done;
    aFrame.Done;
    wFrame.fCopy(twWFrame^);
    aFrame.fCopy(twAFrame^);
    Dispose(twWFrame, Done);
    Dispose(twAFrame, Done);
    twWFrame := Nil;
    twAFrame := Nil;
  end;

  function TerminalWindow.GetTerminalWinPort : AbstractPortPtr;
    {-Return a pointer to port object}
  begin
    GetTerminalWinPort := twPort;
  end;

  function TerminalWindow.GetTerminalWinEmulator : TerminalEmulatorPtr;
    {-Return a pointer to emulator object}
  begin
    GetTerminalWinEmulator := twEmulator;
  end;

  procedure TerminalWindow.SetTerminalWinPort(NewPort : AbstractPortPtr);
    {-Change the internal Port object}
  begin
    twPort := NewPort;
  end;

  procedure TerminalWindow.SetTerminalWinEmulator(NewEmu : TerminalEmulatorPtr);
    {-Change the internal emulator object}
  begin
    twEmulator := NewEmu;
  end;

  procedure TerminalWindow.WriteChar(C : Char);
  const
    LastCmdClrScr : Boolean = False;
  var
    CmdRec : CommandRecord;
  begin
    if twEmulator <> Nil then
      twEmulator^.ProcessChar(C, CmdRec)
    else
      CmdRec.Cmd := eChar;
    with CmdRec, twVScreen^ do
      case Cmd of
        eChar             : twPutChar(C);
        eNone             : ;
        eGotoXY           : twGotoXYAdjust(X, Y);
        eUp               : if svsY > Y then
                              twGotoXY(svsX, svsY - Y);
        eDown             : twGotoXY(svsX, svsY + Y);
        eRight            : twGotoXY(svsX + X, svsY);
        eLeft             : if svsX > X then
                              twGotoXY(svsX - X, svsY);
        eClearBelow       : twClrPart(svsX, svsY, 80, 25);
        eClearAbove       : twClrPart(1, 1, svsX, svsY);
        eClearScreen      : if not LastCmdClrScr then
                              twClrScr;
        eClearEndOfLine   : twClrEOL;
        eClearStartOfLine : twClrPart(1, svsY, svsX, svsY);
        eClearLine        : twClrPart(1, svsY, 80, svsY);
        eSetAttribute     : twSetTextAttr(X);
        eSaveCursorPos    : twSaveCursorPos;
        eRestoreCursorPos : twRestoreCursorPos;
      end;
    LastCmdClrScr := CmdRec.Cmd = eClearScreen;
  end;

  procedure TerminalWindow.WriteString(S : String);
  var
    WasCurrent : Boolean;
    I : Byte;
  begin
    WasCurrent := IsCurrent or (not IsActive);
    if not WasCurrent then
      ActivateWrite;
    for I := 1 to Length(S) do
      WriteChar(S[I]);
    if not WasCurrent then
      DeactivateWrite;
  end;

  {$IFDEF UseScrollBars}
  procedure TerminalWindow.twSetupForScrollBars;
    {-Set boundaries for all scroll bars}
  begin
    with twVScreen^ do
      ChangeAllScrollBars(1, twScrollBarMaxX, 1, twScrollBarMaxY);
  end;
  {$ENDIF}

  function TerminalWindow.twScrollBarMaxX : LongInt;
  begin
    twScrollBarMaxX := (twVScreen^.vsWidth - Width) + 1;
  end;

  function TerminalWindow.twScrollBarMaxY : LongInt;
  begin
    twScrollBarMaxY := (twVScreen^.vsHeight - Height) + 1;
  end;

  {$IFDEF UseMouse}
  function TerminalWindow.twMouseInWin : Boolean;
  begin
    with ActiveFramePtr^ do
      twMouseInWin := (MouseLastX >= frXL) and (MouseLastX <= frXH) and
                      (MouseLastY >= frYL) and (MouseLastY <= frYH);
  end;

  function TerminalWindow.twProcessMouseCommand(Cmd : Word) : Boolean;
    {-Process ccMouseSel command. Returns True to return control to user.}
  var
    L : LongInt;
    FramePos : FramePosType;
    HotCode : Byte;
    Dragging : Boolean;
  begin
    twProcessMouseCommand := False;

    {determine position of mouse}
    {$IFDEF UseDrag}
    L := cwMouseResults(Cmd, FramePos, HotCode);
    {Should mouse event be ignored?}
    if cwIgnoreMouseEvent(Dragging, Cmd, FramePos, HotCode) then
      Exit;
    {$ELSE}
    EvaluateMousePos;
    L := PosResults(FramePos, HotCode);
    {$ENDIF}

    case HotCode of
      hsNone :               {not a hot spot}
        case FramePos of
          frInsideActive :   {inside window}
            {does nothing in this unit} ;
          else
            twProcessMouseCommand := LongFlagIsSet(wFlags, wAllMouseEvents);
        end;

      {$IFDEF UseScrollBars}
      hsDecV :               {the decrement fixture of a vertical scroll bar}
          twScrollVertical(-1);
      hsDecH :               {the decrement fixture of a horizontal scroll bar}
        twScrollHorizontal(-1);
      hsIncV :               {the increment fixture of a vertical scroll bar}
          twScrollVertical(1);
      hsIncH :               {the increment fixture of a horizontal scroll bar}
        twScrollHorizontal(1);
      hsBar :                {the slider portion of a scroll bar}
        case FramePos of
          frLL, frRR :       {vertical scroll bar}
            begin
              L := TweakSlider(FramePos, MouseKeyWordY+MouseYLo, L, 1);
              twScrollVertical(L-twVScreen^.svsViewY);
            end;
          else begin         {horizontal scroll bar}
            L := TweakSlider(FramePos, MouseKeyWordX+MouseXLo, L, 1);
            twScrollHorizontal(L-twVScreen^.svsViewX);
          end;
        end;
      {$ENDIF}

      hsSpot,                {a single character hot spot}
      hsRegion0..255 :       {a user-defined region relative to a frame}
        {$IFDEF UseDrag}
        twProcessMouseCommand := (Cmd <> ccMouseAuto);
        {$ELSE}
        twProcessMouseCommand := True;
        {$ENDIF}
    end;
  end;
  {$ENDIF}

  procedure TerminalWindow.ProcessSelf;
  var
    ProcessDone : Boolean;
    V : Integer;
  begin
    {$IFDEF UseScrollBars}
    {make sure we're set up for scroll bars}
    twSetupForScrollBars;
    {$ENDIF}

    Draw;
    if RawError <> 0 then
      Exit;

    if twVScreenCursorInWin then
      twCursorOn(True);

    ProcessDone := False;
    AsyncStatus := 0;
    repeat
      GetNextCommand;
      case cwCmd of
        ccChar :
          begin
            {!!.02 moved to SendOutgoingChar
            if LongFlagIsSet(twOptions, twShowOutgoing) then
              WriteChar(Char(cwKey));}
            SendOutgoingChar(Char(cwKey));
            if (Char(cwKey) = ^M) then
              if LongFlagIsSet(twOptions, twOutCRtoCRLF) then
                SendOutgoingChar(^J);
            if (Char(cwKey) = ^J) then
              if LongFlagIsSet(twOptions, twOutLFtoCRLF) then
                SendOutgoingChar(^M)
          end;
        ccIncomingChar :
          begin
            WriteChar(Char(cwKey));
            if (Char(cwKey) = ^M) and
               LongFlagIsSet(twOptions, twInCRtoCRLF) then
              WriteChar(^J);
            if (Char(cwKey) = ^J) and
               LongFlagIsSet(twOptions, twInLFtoCRLF) then
              WriteChar(^M);
          end;
        ccLeft :
          twScrollHorizontal(-1);
        ccRight:
          twScrollHorizontal(1);
        ccUp:
          twScrollVertical(-1);
        ccDown:
          twScrollVertical(1);
        ccTopOfFile :
          with twVScreen^ do
            if svsViewY <> 1 then begin
              SetView(svsViewX, 1);
              twUpdateContents;
            end;
        ccEndOfFile :
          with twVScreen^ do begin
            V := twScrollBarMaxY;
            if V <> svsViewY then begin
              SetView(svsViewX, V);
              twUpdateContents;
            end;
          end;
        ccHome :
          with twVScreen^ do
            if svsViewX <> 1 then begin
              SetView(1, svsViewY);
              twUpdateContents;
            end;
        ccEnd :
          with twVScreen^ do begin
            V := twScrollBarMaxX;
            if svsViewX <> V then begin
              SetView(V, svsViewY);
              twUpdateContents;
            end;
          end;
        ccPageUp:
          with twVScreen^ do begin
            V := svsViewY - svsViewHeight;
            if V < 0 then
              V := 1;
            if svsViewY <> V then begin
              SetView(svsViewX, V);
              twUpdateContents;
            end;
          end;
        ccPageDn:
          with twVScreen^ do begin
            V := svsViewY + svsViewHeight;
            if V > twScrollBarMaxY then
              V := twScrollBarMaxY;
            if svsViewY <> V then begin
              SetView(svsViewX, V);
              twUpdateContents;
            end;
          end;
        ccHelp :
          RequestHelp(wHelpIndex);
        ccError,
        ccQuit,
        ccUser0..ccUser65335 :
          ProcessDone := True;
      {$IFDEF UseMouse}
        {$IFDEF UseDrag}
        ccMouseAuto,
        ccMouseDown :                                                  {!!.03}
        {$ELSE}                                                        {!!.03}
        ccMouseSel :
        {$ENDIF}                                                       {!!.03}
          ProcessDone := twProcessMouseCommand(cwCmd);
      {$ENDIF}
        else if (cwCmd <= 255) and (GetExitCommandPtr <> Nil) then
          {Possibly a special exit command defined by a derived object}
          ProcessDone := (cwCmd in GetExitCommandPtr^);
      end;
      if AsyncStatus <> 0 then begin
        cwCmd := ccError;
        ProcessDone := True;
      end;
    until ProcessDone;

    {save the window state}
    rwSaveWindowState;
  end;

  procedure TerminalWindow.CopyVirtToFile(StartLine, EndLine : Integer;
                                          FName : String;
                                          StripTrailing : Boolean;
                                          AsciiText : Boolean;
                                          AppendIfExists : Boolean);
    {-Copy the specified lines of the VirtScreen to a file.}
  var
    F : File;
    I : Word;
    ErrorCode : Word;
    S : String;
  begin
    I := twVScreen^.vsHeight;
    if (StartLine = 0) or (StartLine > I) or (EndLine = 0) or
       (EndLine > I) or (EndLine < StartLine) then begin
      GotError(epNonFatal + ecBadCoordinates, '');
      Exit;
    end;
    Assign(F, FName);
    {$I-}
    Reset(F, 1);
    ErrorCode := IoResult;
    if (ErrorCode = 0) and AppendIfExists then begin
      Seek(F, FileSize(F));
      ErrorCode := IoResult;
      if ErrorCode <> 0 then begin
        Close(F);
        if IoResult <> 0 then ;
        GotError(epNonFatal + ErrorCode, '');
        Exit;
      end;
    end
    else begin
      if ErrorCode = 0 then begin
        Close(F);
        ErrorCode := IoResult;
      end;
      Rewrite(F, 1);
      ErrorCode := IoResult;
    end;
    if ErrorCode <> 0 then begin
      Close(F);
      if IoResult <> 0 then ;
      GotError(epNonFatal + ErrorCode, '');
      Exit;
    end;
    for I := StartLine to EndLine do begin
      S := twVScreen^.svsGetLine(I, AsciiText);
      if StripTrailing and AsciiText then
        S := TrimTrail(S);
      if AsciiText then
        S := S + ^M^J;
      BlockWrite(F, S[1], Length(S));
      ErrorCode := IoResult;
      if ErrorCode <> 0 then begin
        Close(F);
        if IoResult <> 0 then ;
        GotError(epNonFatal + ErrorCode, '');
        Exit;
      end;
    end;
    Close(F);
    ErrorCode := IoResult;
    GotError(epNonFatal + ErrorCode, '');
  end;

  procedure TerminalWindow.twZeroOut;
  begin
    FillChar(twOptions, (Ofs(twSaveViewY) - Ofs(twOptions)) + SizeOf(twSaveViewY), 0);
  end;

  {$IFDEF UseStreams}
  constructor TerminalWindow.Load(var S : IdStream);
  begin
    twZeroOut;
    if not CommandWindow.Load(S) then
      Fail;
    if cwCmdPtr = Nil then
      SetCommandProcessor(TerminalCommands);
    S.Read(twOptions, SizeOf(LongInt));
    S.Read(twCursorType, SizeOf(CursorType));

    twPort := AbstractPortPtr(S.ReadPointer);
    if twPort = nil then begin
      twPort := AbstractPortPtr(S.GetPtr);
      if S.PeekStatus <> 0 then begin
        Done;
        Fail;
      end;
    end;

    twEmulator := TerminalEmulatorPtr(S.ReadUserPointer(Nil));

    twVScreen := SmartVirtScreenPtr(S.GetPtr);
    if S.PeekStatus <> 0 then begin
      Done;
      Fail;
    end;
  end;

  procedure TerminalWindow.Store(var S : IdStream);
  begin
    CommandWindow.Store(S);
    S.Write(twOptions, SizeOf(LongInt));
    S.Write(twCursorType, SizeOf(CursorType));
    S.WriteUserPointer(twPort, ptNil);
    if not S.PointerRegistered(twPort) then
      S.PutPtr(twPort);
    S.WriteUserPointer(twEmulator, ptNil);
    S.PutPtr(twVScreen);
  end;

  procedure TerminalWindowStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing TerminalWindows}
  begin
    CommandWindowStream(SPtr);
    SmartVirtScreenStream(SPtr);
    with SPtr^ do
      RegisterType(otTerminalWindow, veTerminalWindow,
                   TypeOf(TerminalWindow),
                   @TerminalWindow.Store, @TerminalWindow.Load);
  end;
  {$ENDIF}

  constructor CaptureTerminalWindow.Init(X1, Y1, X2, Y2 : Byte;
                                         Port : AbstractPortPtr);
    {-Create a TerminalWindow}
  begin
    ctwBufferPtr := Nil;
    ctwSize  := 0;
    ctwIndex := 0;
    ctwCaptureFileName := DefCaptureFileName;
    if not TerminalWindow.Init(X1, Y1, X2, Y2, Port) then
      Fail;
      if not GetMemCheck(ctwBufferPtr, DefCaptureBufferSize) then begin
        InitStatus := ecOutOfMemory;
        Done;
        Fail;
      end;
    ctwSize := DefCaptureBufferSize;
  end;

  constructor CaptureTerminalWindow.InitCustom(X1, Y1, X2, Y2 : Byte;
                                               var Colors : ColorSet;
                                               WindowOptions : LongInt;
                                               Port : AbstractPortPtr;
                                               Emu : TerminalEmulatorPtr;
                                               ScrollBackRows : Word;
                                               ScrollBackCols : Word;
                                               TerminalOptions : LongInt;
                                               CaptureFileName : String;
                                               CaptureBufferSize : Word);
    {-Create a TerminalWindow window with custom options}
  begin
    ctwBufferPtr := Nil;
    ctwSize  := 0;
    ctwIndex := 0;
    ctwCaptureFileName := CaptureFileName;
    if not TerminalWindow.InitCustom(X1, Y1, X2, Y2, Colors,
                                     WindowOptions,
                                     Port, Emu,
                                     ScrollBackRows,
                                     ScrollBackCols,
                                     TerminalOptions
                                     ) then
      Fail;
    if CaptureBufferSize < MinCaptureBufferSize then
      CaptureBufferSize := MinCaptureBufferSize;
      if not GetMemCheck(ctwBufferPtr, CaptureBufferSize) then begin
        InitStatus := ecOutOfMemory;
        Done;
        Fail;
      end;
    ctwSize := CaptureBufferSize;
  end;

  destructor CaptureTerminalWindow.Done;
    {-Dispose of a TerminalWindow}
  begin
    if CaptureIsOn then
      Capture(False);
    FreeMemCheck(ctwBufferPtr, ctwSize);
    TerminalWindow.Done;
  end;

  function CaptureTerminalWindow.GetIncomingChar(var Key : Char) : Boolean;
    {-Returns true if incoming character has arrived at the serial port}
  begin
    if TerminalWindow.GetIncomingChar(Key) then begin
      GetIncomingChar := True;
      if CaptureIsOn then
        ctwPutCapture(Key);
    end
    else
      GetIncomingChar := False;
  end;

  procedure CaptureTerminalWindow.SetCaptureFileName(FName : String);
  begin
    ctwCaptureFileName := FName;
    if CaptureIsOn then begin
      Capture(False);
      Capture(True);
    end;
  end;

  function CaptureTerminalWindow.GetCaptureFileName : String;
  begin
    GetCaptureFileName := ctwCaptureFileName;
  end;

  function CaptureTerminalWindow.CaptureIsOn : Boolean;
  begin
    CaptureIsOn := LongFlagIsSet(twOptions, ctwCaptureOn);
  end;

  procedure CaptureTerminalWindow.ctwFlushCapture;
  var
    IoRes : Word;
  begin
    if CaptureIsOn then begin
      BlockWrite(ctwFile, ctwBufferPtr^, ctwIndex);
      IoRes := IoResult;
      if IoRes <> 0 then
        GotError(epNonFatal + IoRes, '');
      ctwIndex := 0;
    end;
  end;

  procedure CaptureTerminalWindow.Capture(On : Boolean);
  var
    IsOn : Boolean;
  begin
    IsOn := CaptureIsOn;
    if On then begin
      if IsOn then
        ctwFlushCapture
      else
        ctwOpenCapture;
    end
    else
      if IsOn then
        ctwCloseCapture;
  end;

  procedure CaptureTerminalWindow.ctwPutCapture(C : Char);
  begin
    if ctwIndex >= ctwSize then
      ctwFlushCapture;
    Inc(ctwIndex);
    ctwBufferPtr^[ctwIndex] := C;
  end;

  procedure CaptureTerminalWindow.ctwOpenCapture;
  var
    IoRes : Word;
  begin
    Assign(ctwFile, ctwCaptureFileName);
    if ExistFile(ctwCaptureFileName) and
       (not LongFlagIsSet(twOptions, ctwOverwrite)) then begin
      Reset(ctwFile, 1);
      IoRes := IoResult;
      if IoRes <> 0 then begin
        GotError(epNonFatal + IoRes, '');
        Exit;
      end;
      if FileSize(ctwFile) > 0 then
        Seek(ctwFile, FileSize(ctwFile)-1)
      else
        Seek(ctwFile, FileSize(ctwFile));
    end
    else
      Rewrite(ctwFile, 1);
    IoRes := IoResult;
    if IoRes = 0 then
      SetLongFlag(twOptions, ctwCaptureOn)
    else
      GotError(epNonFatal + IoRes, '');
  end;

  procedure CaptureTerminalWindow.ctwCloseCapture;
  var
    IoRes : Word;
  begin
    if CaptureIsOn then begin
      ctwFlushCapture;
      Close(ctwFile);
      IoRes := IoResult;
      if IoRes <> 0 then
        GotError(epNonFatal + IoRes, '');
      ClearLongFlag(twOptions, ctwCaptureOn);
    end;
  end;

  {$IFDEF UseStreams}
  constructor CaptureTerminalWindow.Load(var S : IdStream);
  begin
    ctwBufferPtr := Nil;
    if not TerminalWindow.Load(S) then
      Fail;
    S.ReadRange(ctwSize, ctwFile);
    if not GetMemCheck(ctwBufferPtr, ctwSize) then begin
      InitStatus := ecOutOfMemory;
      Done;
      Fail;
    end;
    if LongFlagIsSet(twOptions, ctwCaptureOn) then begin
      ClearLongFlag(twOptions, ctwCaptureOn);
      Capture(True);
    end;
  end;

  procedure CaptureTerminalWindow.Store(var S : IdStream);
  begin
    TerminalWindow.Store(S);
    S.WriteRange(ctwSize, ctwFile);
  end;

  procedure CaptureTerminalWindowStream(SPtr : IdStreamPtr);
    {-Register all types needed for streams containing CaptureTerminalWindows}
  begin
    TerminalWindowStream(SPtr);
    with SPtr^ do
      RegisterType(otCaptureTerminalWindow, veCaptureTerminalWindow,
                   TypeOf(CaptureTerminalWindow),
                   @CaptureTerminalWindow.Store, @CaptureTerminalWindow.Load);
  end;
  {$ENDIF}

begin
  {Initialize command processor}
  TerminalCommands.Init(@TerminalKeySet, TerminalKeyMax);
end.
