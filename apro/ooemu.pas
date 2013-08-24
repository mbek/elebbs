{$R-,S-,O+,F+}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{$IFDEF UseOpro}
{$I OPDEFINE.INC}
{$ENDIF}

{*********************************************************}
{*                   OOEMU.PAS 2.03                      *}
{*     Copyright (c) TurboPower Software 1991.           *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OOEmu;
interface
uses
  {$IFDEF UseOpro}
  OpRoot,
  OpCrt,
  {$ELSE}
    {$IFDEF UseTpro}
    TpCrt,
    {$ELSE}
    Crt,
    {$ENDIF}
  {$ENDIF}
  ApMisc;

const
  {#Z+}
  AnsiMaxParams    = 5;       {maximum parameters for our ansi interpreter}
  {#Z-}

  {emulator commands}
  eNone            = 0;       {no command, ignore this char}
  eChar            = 1;       {no command, process the char}
  eGotoXY          = 2;       {absolute goto cursor position call}
  eUp              = 3;       {cursor up}
  eDown            = 4;       {cursor down}
  eRight           = 5;       {cursor right}
  eLeft            = 6;       {cursor left}
  eClearBelow      = 7;       {clear screen below cursor}
  eClearAbove      = 8;       {clear screen above cursor}
  eClearScreen     = 9;       {clear entire screen}
  eClearEndofLine  = 10;      {clear from cursor to end of line}
  eClearStartOfLine= 11;      {clear from cursor to the start of line}
  eClearLine       = 12;      {clear entire line that cursor is on}
  eSetMode         = 13;      {set video mode}
  eSetBackground   = 14;      {set background attribute}
  eSetForeground   = 15;      {set foreground attribute}
  eSetAttribute    = 16;      {set video attribute (foreground and background)}
  eSaveCursorPos   = 17;      {save cursor position}
  eRestoreCursorPos= 18;      {restore cursor position}
  eDeviceStatusReport = 19;   {report device status or cursor position}

  eError           = 255;     {indicates a parser error}

  {Special parser characters for ANSI escape sequences}
  Escape = #27;
  LeftBracket = #91;
  Semicolon = #59;
  FormFeed = #12;

  {AsciiEmulator option flags}
  teMapVT100           = $0001;
  teMaskBlinkOnInverse = $0002;
  teIsAnsi             = $0004;                                        {!!.02}

type
  {#Z+}
  {array used for internal queue}
  QueueType = Array[1..255] of Char;
  QueuePtr  = ^QueueType;
  {#Z-}

  {record type used to describe a command and its parameters}
  CommandRecord =
    record
      Ch   : Char;     {the character}
      Cmd  : Byte;     {the command}
      X, Y, Z : Byte;  {the parameters (only X and Y used currently)}
    end;

  {abstract definition of an emulator object}
  TerminalEmulatorPtr = ^TerminalEmulator;
  TerminalEmulator =
    object(Root)
      teOptions        : Word;
      teQueueSize      : Byte;      {size of our queue}
      teTextAttr       : Byte;      {set to Crt's TextAttr on Init}
      teQueueIndex     : Byte;      {current index into queue}
      teQueue          : QueuePtr;  {ptr to our queue}
      constructor Init(QueueSize : Byte);
        {-Initialize a TerminalEmulator}
      destructor Done; Virtual;
        {-Dispose of a TerminalEmulator}
      procedure ProcessChar(C : Char; var Command : CommandRecord); Virtual;
        {-Process a character and return a CommandRecord}
      procedure teOptionsOn(Options : Word);
        {-Turn options on}
      procedure teOptionsOff(Options : Word);
        {-Turn options off}
      function teOptionsAreOn(Options : Word) : Boolean;
        {-Return True if all specified options are on}

      {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a TerminalEmulator object from a stream}
      procedure Store(var S : IdStream);
        {-Store a TerminalEmulator object to a stream}
      {$ENDIF}

      {#Z+}
      {... internal methods ...}
      procedure tePutQueue(C : Char);
        {-Put a char into our queue}
      function teGetQueue(var C : Char) : Boolean;
        {-Return True if char in queue and return it in C}
      procedure teSetTextAttr(Attr : Byte);
        {-Set teTextAttr to Attr}
      procedure teResetTextAttr;
        {-Set teTextAttr to Crt's TextAttr}
      procedure teZeroOut;
      {#Z-}
    end;

  {#Z+}
  {type used by AnsiEmulator's internal parser}
  AnsiParserType = (GotNone, GotEscape, GotBracket, GotSemiColon,
                    GotParam, GotCommand);
  {#Z-}

  {emulator object for PC ANSI codes}
  AnsiEmulatorPtr = ^AnsiEmulator;
  AnsiEmulator =
    object(TerminalEmulator)
      Params      : Array[1..AnsiMaxParams] of String[5]; {parameter strings}
      ParamInt    : Array[1..AnsiMaxParams] of Integer;   {params as ints}
      ParamIndex  : Byte;           {last param's index}
      Inverse     : Boolean;        {used internally}
      Intense     : Boolean;        {used internally}
      Blink       : Boolean;        {used internally}
      Invis       : Boolean;        {used internally}
      ParserState : AnsiParserType; {used internally by parser}
      constructor Init(QueueSize : Byte);
        {-Initialize an AnsiEmulator}
      destructor Done; Virtual;
        {-Dispose of an AnsiEmulator}
      procedure ProcessChar(C : Char; var Command : CommandRecord); Virtual;
        {-Process a character and return a CommandRecord}
      {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Load a AnsiEmulator object from a stream}
      procedure Store(var S : IdStream);
        {-Store a AnsiEmulator object to a stream}
      {$ENDIF}

      {#Z+}
      {...internal methods...}
      procedure InitParser;
      procedure BuildParam(C : Char);
      procedure ConvertParams(C : Char);
      procedure MakeCommand(C : Char; var Command : CommandRecord); Virtual;
        {-used by ProcessChar to create the command record}
      procedure ansiZeroOut;
      {#Z-}
    end;

{$IFDEF UseStreams}
procedure TerminalEmulatorStream(SPtr : IdStreamPtr);
  {-Register all types needed for streams containing TerminalEmulators}

procedure AnsiEmulatorStream(SPtr : IdStreamPtr);
  {-Register all types needed for streams containing AnsiEmulators}
{$ENDIF}

implementation

  constructor TerminalEmulator.Init(QueueSize : Byte);
  begin
    if not Root.Init then
      Fail;
    {insure sufficient memory for queue or fail}
    if (MaxAvail < QueueSize) or (QueueSize = 0) then
      Fail;
    GetMem(teQueue, QueueSize);      {allocate queue}
    teQueueSize := QueueSize;
    teQueueIndex := 0;
    teResetTextAttr;
    teOptions := 0;
  end;

  destructor TerminalEmulator.Done;
  begin
    FreeMem(teQueue, teQueueSize);   {dispose of queue}
    Root.Done;
  end;

  procedure TerminalEmulator.teSetTextAttr(Attr : Byte);
  begin
    teTextAttr := Attr;
  end;

  procedure TerminalEmulator.teResetTextAttr;
  begin
    teSetTextAttr(TextAttr);
  end;

  procedure TerminalEmulator.ProcessChar(C : Char;
                                         var Command : CommandRecord);
  begin
    with Command do begin
      Ch := C;
      Cmd := eChar;
    end;
  end;

  procedure TerminalEmulator.tePutQueue(C : Char);
  begin
    if teQueueIndex < teQueueSize then begin
      Inc(teQueueIndex);
      teQueue^[teQueueIndex] := C;
    end;
  end;

  function TerminalEmulator.teGetQueue(var C : Char) : Boolean;
  begin
    {are there chars in the queue?}
    if teQueueIndex > 0 then begin
      {yes, return it}
      C := teQueue^[1];
      Dec(teQueueIndex);
      {if necessary, move remaining chars down in queue}
      if teQueueIndex > 0 then
        Move(teQueue^[2], teQueue^[1], teQueueIndex);
      teGetQueue := True;
    end
    else
      teGetQueue := False;
  end;

  {$IFDEF UseStreams}
  constructor TerminalEmulator.Load(var S : IdStream);
  begin
    teZeroOut;
    S.Read(teOptions, SizeOf(Word));
    S.Read(teQueueSize, SizeOf(Byte));
    teResetTextAttr;
    if (MaxAvail < teQueueSize) or (teQueueSize = 0) then
      Fail;
    GetMem(teQueue, teQueueSize);
    if teQueue = Nil then
      Fail;
  end;

  procedure TerminalEmulator.Store(var S : IdStream);
  begin
    S.Write(teOptions, SizeOf(Word));
    S.Write(teQueueSize, SizeOf(Byte));
  end;

  procedure TerminalEmulatorStream(SPtr : IdStreamPtr);
  begin
    with SPtr^ do
      RegisterType(otTerminalEmulator, veTerminalEmulator,
                   TypeOf(TerminalEmulator),
                   @TerminalEmulator.Store, @TerminalEmulator.Load);
  end;
  {$ENDIF}

  procedure TerminalEmulator.teZeroOut;
  begin
    FillChar(teOptions, (Ofs(teQueue) - Ofs(teOptions)) + SizeOf(QueuePtr), 0);
  end;

  procedure TerminalEmulator.teOptionsOn(Options : Word);
    {-Turn options on}
  begin
    teOptions := teOptions or Options;
  end;

  procedure TerminalEmulator.teOptionsOff(Options : Word);
    {-Turn options off}
  begin
    teOptions := teOptions and (not Options);
  end;

  function TerminalEmulator.teOptionsAreOn(Options : Word) : Boolean;
    {-Return True if all specified options are on}
  begin
    teOptionsAreOn := (teOptions and Options = Options);
  end;

  constructor AnsiEmulator.Init(QueueSize : Byte);
  begin
    if not TerminalEmulator.Init(QueueSize) then
      Fail;
    Intense := False;
    Inverse := False;
    Blink := False;
    Invis := False;
    InitParser;    {prepare parser}
    teOptions := teOptions or teIsAnsi;
  end;

  destructor AnsiEmulator.Done;
  begin
    TerminalEmulator.Done;
  end;

  procedure AnsiEmulator.ProcessChar(C : Char; var Command : CommandRecord);

    procedure ErrorCondition;
      {-Set error state and reset parser}
    begin
      Command.Cmd := eError;
      InitParser;
    end;

  begin
    tePutQueue(C);         {put char in queue in case of subsequent error}
    with Command do begin
      Ch := C;             {store char}
      Cmd := eNone;        {assume we do nothing with it}
    end;
    {evaluate parser state}
    case ParserState of
      GotNone :            {look for escape or form feed}
        if C = Escape then
          ParserState := GotEscape
        else if C = FormFeed then
          Command.Cmd := eClearScreen
        else
          Command.Cmd := eChar;
      GotEscape :          {last char was escape, so look for [}
        if C = LeftBracket then
          ParserState := GotBracket
        else
          ErrorCondition;
      GotParam,
      GotBracket,
      GotSemicolon :       {need a parameter char, semicolon or command}
        if (C >= #48) and (C <= #57) then begin
          BuildParam(C);
          ParserState := GotParam;
        end
        else if C = Semicolon then
          if ParserState = GotSemicolon then
            {ErrorCondition}                                           {!!.02}
          else begin
            ParserState := GotSemicolon;
            Inc(ParamIndex);
            if ParamIndex > AnsiMaxParams then
              ErrorCondition;
          end
        else begin
          MakeCommand(C, Command);
          if Command.Cmd = eError then                                 {!!.02}
            ErrorCondition;                                            {!!.02}
          InitParser;
        end;
    end;
  end;

  procedure AnsiEmulator.InitParser;
    {-Reset parser state}
  begin
    ParamIndex := 1;
    FillChar(Params, SizeOf(Params), 0);
    ParserState := GotNone;
    teQueueIndex := 0;
  end;

  procedure AnsiEmulator.BuildParam(C : Char);
    {-Build a param string}
  begin
    Params[ParamIndex] := Params[ParamIndex] + C;
  end;

  procedure AnsiEmulator.ConvertParams(C : Char);
    {-Convert param strings to integers}
  var
    I, Code : Integer;
  begin
    for I := 1 to AnsiMaxParams do begin
      Val(Params[I], ParamInt[I], Code);
      if Code <> 0 then
        ParamInt[I] := 1;
    end;
    if (Length(Params[1]) = 0) and (C in ['J', 'K']) then
      if ((teOptions and teMapVT100) <> 0) then
        ParamInt[1] := 0
      else
        ParamInt[1] := 2;
    if (ParamInt[1] = 0) and (C in ['A','B','C','D']) then             {!!.02}
      ParamInt[1] := 1;                                                {!!.02}
    if (C = 'm') and (Length(Params[1]) = 0) then                      {!!.02}
      ParamInt[1] := 0;                                                {!!.02}
  end;

  procedure AnsiEmulator.MakeCommand(C : Char; var Command : CommandRecord);
    {-Make a command record}
  var
    I, TextFg, TextBk : Byte;
  begin
    ConvertParams(C);
    with Command do begin
      Ch := C;
      case C of
        'f', 'H' :
          begin
            Cmd := eGotoXY;
            X   := ParamInt[2];
            Y   := ParamInt[1];
          end;
        'A' :
          begin
            Cmd := eUp;
            Y   := ParamInt[1];
          end;
        'B' :
          begin
            Cmd := eDown;
            Y   := ParamInt[1];
          end;
        'C' :
          begin
            Cmd := eRight;
            X   := ParamInt[1];
          end;
        'D' :
          begin
            cmd := eLeft;
            X   := ParamInt[1];
          end;
        'J' :
          begin
            case ParamInt[1] of
              0 : Cmd := eClearBelow;
              1 : Cmd := eClearAbove;
              2 : Cmd := eClearScreen;
              else Cmd := eChar;
            end;
          end;
        'K' :
          begin
            case ParamInt[1] of
              0 : Cmd := eClearEndOfLine;
              1 : Cmd := eClearStartOfLine;
              2 : Cmd := eClearLine;
              else Cmd := eChar;
            end;
          end;
        'h' :
          begin
            Cmd := eSetMode;
            X   := ParamInt[1];
          end;
        'm' :
          begin
            Cmd := eSetAttribute;
            X   := teTextAttr;
            for I := 1 to ParamIndex do begin
              if Inverse then begin
                {Note blink, intense attributes, then strip}
                Blink := X and $80 = $80;
                Intense := X and $08 = $08;
                X := X and $77;

                {Restore inverted TextAttr before continuing}
                X := Byte((Word(X) shl 4) or (Word(X) shr 4));
              end;

              {Separate out the forground and background bits}
              TextFg := X and $0F;
              TextBk := X and $F0;

              {Process the color command}
              case ParamInt[I] of
                0  : begin
                       X := $07;            {White on black}
                       Inverse := False;
                       Intense := False;
                       Blink := False;
                       Invis := False;
                     end;
                1  : Intense  := True;      {Set intense bit later}
                4  : Intense  := True;      {Subst intense for underline}
                5  : Blink := True;         {set blinking on}
                7  : Inverse  := True;      {Invert TextAttr later}
                8  : Invis := True;         {Invisible}
                27 : Inverse  := False;     {Stop inverting TextAttr}
                30 : X := TextBk or $00;    {Black foreground}
                31 : X := TextBk or $04;    {Red foreground}
                32 : X := TextBk or $02;    {Green foreground}
                33 : X := TextBk or $06;    {Yellow forground}
                34 : X := TextBk or $01;    {Blue foreground}
                35 : X := TextBk or $05;    {Magenta foreground}
                36 : X := TextBk or $03;    {Cyan foreground}
                37 : X := TextBk or $07;    {White foreground}
                40 : X := TextFg;
                41 : X := TextFg or $40;    {Red background}
                42 : X := TextFg or $20;    {Green background}
                43 : X := TextFg or $60;    {Yellow background}
                44 : X := TextFg or $10;    {Blue background}
                45 : X := TextFg or $50;    {Magenta background}
                46 : X := TextFg or $30;    {Cyan background}
                47 : X := TextFg or $70;    {White background}
              end;
            end;

            {Handle all fixups here}
            if Inverse then
              X := Byte((Word(X) shl 4) or (Word(X) shr 4));
            if Inverse and teOptionsAreOn(teMaskBlinkOnInverse) then
              X := X and $7F;
            if Invis then
              X := $00;
            if Intense then
              X := X or $08;
            if Blink then
              X := X or $80;
            teTextAttr := X;
          end;
        's' : Cmd := eSaveCursorPos;
        'u' : Cmd := eRestoreCursorPos;
        'n' : cmd := eDeviceStatusReport;
        else
          Cmd := eError;
      end;
    end;
  end;

  procedure AnsiEmulator.ansiZeroOut;
  begin
    FillChar(Params, (Ofs(ParserState) - Ofs(Params)) +
                     SizeOf(AnsiParserType), 0);
  end;

  {$IFDEF UseStreams}
  constructor AnsiEmulator.Load(var S : IdStream);
  begin
    ansiZeroOut;
    if not TerminalEmulator.Load(S) then
      Fail;
    InitParser;
  end;

  procedure AnsiEmulator.Store(var S : IdStream);
  begin
    TerminalEmulator.Store(S);
  end;

  procedure AnsiEmulatorStream(SPtr : IdStreamPtr);
  begin
    TerminalEmulatorStream(SPtr);
    with SPtr^ do
      RegisterType(otAnsiEmulator, veAnsiEmulator,
                   TypeOf(AnsiEmulator),
                   @AnsiEmulator.Store, @AnsiEmulator.Load);
  end;
  {$ENDIF}

end.
