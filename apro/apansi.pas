{$S-,R-,V-,I-,B-,F+,O+,A-}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{*********************************************************}
{*                   APANSI.PAS 2.03                     *}
{*     Copyright (c) TurboPower Software 1991.           *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit ApAnsi;
  {-Does ANSI screen writing and produces ANSI sequences}

interface

uses
  {$IFDEF UseOpro}
  OpCrt,
  {$ENDIF}
  {$IFDEF UseTpro}
  TpCrt,
  {$ENDIF}
  {$IFDEF Standalone}
  Crt,
  {$ENDIF}
  ApMisc,
  ApPort;

type
  ClearScreenProc = procedure;
  SoundBellProc  = procedure;

const
  {Prevent remote from changing our video mode}
  InhibitModeChange : Boolean = True;
  UseVT100Mode : Boolean = False;
  MaskBlinkOnInverse : Boolean = True;
  CurrentAnsiPort : PortRecPtr = nil;
  InhibitAutoLF : Boolean = True;

procedure WriteCharAnsi(C : Char);
  {-Writes C (and handles ANSI escape sequences)}

procedure WriteCharAnsiPort(P : PortRecPtr; C : Char);
  {-Writes C with ANSI support using port P}

procedure WriteStringAnsi(S : String);
  {-Writes S (and handles ANSI escape sequences)}

procedure WriteStringAnsiPort(P : PortRecPtr; S : String);
  {-Writes S with ANSI support using port P}

procedure SetClearScreenProc(CSP : ClearScreenProc);
  {-Sets a ClearScreen procedure to be called on FormFeed characters}

procedure SetSoundBellProc(SBP : SoundBellProc);
  {-Sets a SoundBell procedure to be called on Bell characters}

procedure SetCurrentAnsiPort(P : PortRecPtr);
  {-Sets the current ansi port for any required ANSI output}

implementation

type
  {Token types}
  ParserType = (GotNone, GotEscape, GotBracket, GotSemiColon,
                GotParm, GotCommand);

const
  {Special parser characters}
  Escape = #27;
  LeftBracket = #91;
  Semicolon = #59;
  LineFeed = #10;
  FormFeed = #12;
  BellChar = #07;
  EqualSign = #61;
  QuestionMark = #63;

  {For sizing parser}
  MaxQueueChars = 10;
  MaxParms = 5;

  {For sizing the screen}
  AnsiWidth : Word = 80;
  AnsiHeight : Word = 25;

  {For saving TextAttr states}
  Inverse : Boolean = False;
  Intense : Boolean = False;
  BlinkOn : Boolean = False;

  {For saving and restoring the cursor state}
  SaveX : Byte = 1;
  SaveY : Byte = 1;

var
  {For saving invalid escape sequences}
  SaveCharQueue : array[1..MaxQueueChars] of Char;
  QTail : Byte;

  {For collecting and converting parameters}
  Parms : array[1..MaxParms] of String[5];
  ParmInt : array[1..MaxParms] of Integer;
  ParmDefault : array[1..MaxParms] of Boolean;
  ParmIndex : Byte;

  {Current token}
  ParserState : ParserType;

  {User hooks}
  ClearScreen    : ClearScreenProc;
  SoundBell      : SoundBellProc;

  procedure WriteStringAnsi(S : String);
    {-Writes S (and handles ANSI escape sequences)}
  var
    I : Byte;
  begin
    for I := 1 to Length(S) do
      WriteCharAnsi(S[I]);
  end;

  procedure WriteStringAnsiPort(P : PortRecPtr; S : String);
    {-Writes S with ANSI support using port P}
  begin
    CurrentAnsiPort := P;
    WriteStringAnsi(S);
  end;

  procedure WriteCharAnsiPort(P : PortRecPtr; C : Char);
    {-Writes C with ANSI support using port P}
  begin
    CurrentAnsiPort := P;
    WriteCharAnsi(C);
  end;

  procedure InitParser;
    {-Initialize parser for next ansi sequence}
  var
    I : Byte;
  begin
    QTail := 0;
    ParmIndex := 1;
    for I := 1 to MaxParms do begin
      Parms[I] := '';
      ParmDefault[I] := False;
    end;
    ParserState := GotNone;
  end;

  procedure PushChar(C : Char);
    {-Push C into the saved char queue}
  begin
    if QTail < MaxQueueChars then begin
      Inc(QTail);
      SaveCharQueue[QTail] := C;
    end;
  end;

  function HeadChar(var C : Char) : Boolean;
    {-Returns the first character on the saved stack and moves the rest down}
  begin
    if QTail > 0 then begin
      C := SaveCharQueue[1];
      HeadChar := True;
      Dec(QTail);
      Move(SaveCharQueue[2], SaveCharQueue[1], QTail);
    end else
      HeadChar := False;
  end;

  procedure BuildParm(C : Char);
    {-Gets the next character of the current parameter}
  begin
    Parms[ParmIndex] := Parms[ParmIndex] + C;
  end;

  procedure ConvertParms(C : Char);
    {-Convert the parms into integers}
  var
    I, Code : Integer;
  begin
    for I := 1 to MaxParms do begin
      Val(Parms[I], ParmInt[I], Code);
      if Code <> 0 then begin
        ParmInt[I] := 1;
        ParmDefault[I] := True;
      end;
    end;
    if ParmDefault[1] and (C in ['J', 'K']) then
      if UseVT100Mode then
        ParmInt[1] := 0
      else
        ParmInt[1] := 2;
    if (ParmInt[1] = 0) and (C in ['A','B','C','D']) then
      ParmInt[1] := 1;
    if (C = 'm') and ParmDefault[1] then                               {!!.02}
      ParmInt[1] := 0;                                                 {!!.02}
  end;

  {!!.02}
  procedure ClearEndOfLine;
    {-Clear to the end of the line}
  begin
    {Un-invert TextAttr before clearing}
    if Inverse then
      TextAttr := (TextAttr shl 4) or (TextAttr shr 4);
    ClrEol;

    {Restore inverted TextAttr}
    if Inverse then
      TextAttr := (TextAttr shl 4) or (TextAttr shr 4);
  end;

  procedure ClearPart(X1, Y1, X2, Y2 : Integer);
    {-Clear from X1, Y1 to X2, Y2}
  var
    Row, Col : Integer;
    SaveX, SaveY : Word;

    procedure ClearRow(X1, X2 : Integer);
    var
      I : Integer;
    begin
      GotoXY(X1, WhereY);
      if X2 = AnsiWidth then
        ClrEol
      else
        for I := X1 to X2 do
          Write(' ');
    end;

  begin
    {Save cursor position}
    SaveX := WhereX;
    SaveY := WhereY;
    GotoXY(X1, Y1);

    {Un-invert TextAttr before clearing}                               {!!.02}
    if Inverse then                                                    {!!.02}
      TextAttr := (TextAttr shl 4) or (TextAttr shr 4);                {!!.02}

    if Y1 = Y2 then
      ClearRow(X1, X2)
    else begin
      ClearRow(X1, AnsiWidth);
      if Y1+1 <= Y2-1 then
        for Row := Y1+1 to Y2-1 do begin
          GotoXY(1, Row);
          ClearRow(1, AnsiWidth);
        end;
      GotoXY(1, Y2);
      ClearRow(1, X2);
    end;
    GotoXY(SaveX, SaveY);

    {Restore inverted TextAttr}                                        {!!.02}
    if Inverse then                                                    {!!.02}
      TextAttr := (TextAttr shl 4) or (TextAttr shr 4);                {!!.02}
  end;

  procedure GotoXYCheck(X, Y : Integer);
    {-GotoXY that checks against negative numbers}
  begin
    if X < 1 then
      X := 1;
    if Y < 1 then
      Y := 1;
    GotoXY(X, Y);
  end;

  procedure ReportCursorPosition;
    {-Output CPR sequence with cursor position (no error checking)}
  const
    AnsiStart = #27'[';
  var
    S1, S2 : String[8];
    I : Byte;
  begin
    {Exit immediately if an ANSI output port hasn't been set}
    if (CurrentAnsiPort = nil) or (@AnsiOutput = nil) then
      Exit;

    {Make an output string like so: <esc>[<wherex>;<wherey>R}
    Str(WhereX, S1);
    Str(WhereY, S2);
    S1 := AnsiStart + S1 + ';' + S2 + 'R';

    {Send out the string (no error checking)}
    AsyncStatus := ecOk;
    I := 1;
    while (AsyncStatus = ecOk) and (I <= Length(S1)) do begin
      AnsiOutput(CurrentAnsiPort, S1[I]);
      Inc(I);
    end;
  end;

  procedure ProcessCommand(C : Char);
    {-Process the current command}
  var
    I, TextFg, TextBk : Byte;

  begin
    {Convert parameter strings to integers (and assign defaults)}
    ConvertParms(C);

    {Act on the accumulated parameters}
    case C of
      'f',  {HVP - horizontal and vertical position}
      'H' : {CUP - cursor position}
        GotoXYCheck(ParmInt[2], ParmInt[1]);

      'A' : {CUU - cursor up}
        GotoXYCheck(WhereX, WhereY - ParmInt[1]);

      'B' : {CUD - cursor down}
        GotoXYCheck(WhereX, WhereY + ParmInt[1]);

      'C' : {CUF - cursor forward}
        GotoXYCheck(WhereX + ParmInt[1], WhereY);

      'D' : {CUB - cursor back}
        GotoXYCheck(WhereX - ParmInt[1], WhereY);

      'J' : {ED - erase display}
        case ParmInt[1] of
          0 : ClearPart(WhereX, WhereY, AnsiWidth, AnsiHeight);
          1 : ClearPart(1, 1, WhereX, WhereY);
          2 : ClearScreen;
        end;

      'K' : {EL - erase in line}
        begin
          case ParmInt[1] of
            0 : ClearEndOfLine;                                        {!!.02}
            1 : ClearPart(1, WhereY, WhereX, WhereY);
            2 : ClearPart(1, WhereY, AnsiWidth, WhereY);
          end;
        end;

      'l',
      'h' : {SM - set mode (supports text modes only)}
        if not InhibitModeChange then begin
          case ParmInt[1] of
            0 : TextMode(BW40);
            1 : TextMode(CO40);
            2 : TextMode(BW80);
            3 : TextMode(CO80);
          end;
          case ParmInt[1] of
            0,1 : AnsiWidth := 40;
            2,3 : AnsiWidth := 80;
          end;
        end;

      'm' : {SGR - set graphics rendition (set background color)}
        begin
          for I := 1 to ParmIndex do begin
            if Inverse then
              {Restore inverted TextAttr before continuing}
              TextAttr := (TextAttr shl 4) or (TextAttr shr 4);

            {Separate out the forground and background bits}
            TextFg := TextAttr and $0F;
            TextBk := TextAttr and $F0;

            {Process the color command}
            case ParmInt[I] of
              0  : begin
                     TextAttr := $07;                {White on black}
                     Inverse := False;
                     Intense := False;
                     BlinkOn := False;
                   end;
              1  : Intense  := True;               {Set intense bit later}
              4  : Intense  := True;               {Subst intense for underline}
              5,6: BlinkOn  := True;               {Set blinking on}
              7  : Inverse  := True;               {Invert TextAttr later}
              8  : TextAttr := $00;                {Invisible}
              27 : Inverse  := False;              {Stop inverting TextAttr}
              30 : TextAttr := TextBk or $00;      {Black foreground}
              31 : TextAttr := TextBk or $04;      {Red foreground}
              32 : TextAttr := TextBk or $02;      {Green foreground}
              33 : TextAttr := TextBk or $06;      {Yellow forground}
              34 : TextAttr := TextBk or $01;      {Blue foreground}
              35 : TextAttr := TextBk or $05;      {Magenta foreground}
              36 : TextAttr := TextBk or $03;      {Cyan foreground}
              37 : TextAttr := TextBk or $07;      {White foreground}
              40 : TextAttr := TextFg;             {Black background}
              41 : TextAttr := TextFg or $40;      {Red background}
              42 : TextAttr := TextFg or $20;      {Green background}
              43 : TextAttr := TextFg or $60;      {Yellow background}
              44 : TextAttr := TextFg or $10;      {Blue background}
              45 : TextAttr := TextFg or $50;      {Magenta background}
              46 : TextAttr := TextFg or $30;      {Cyan background}
              47 : TextAttr := TextFg or $70;      {White background}
            end;

            {Fix up TextAttr for inverse and intense}
            if Inverse then begin
              TextAttr := (TextAttr shl 4) or (TextAttr shr 4);
              if MaskBlinkOnInverse then
                TextAttr := TextAttr and $7F;
            end;
            if Intense then
              TextAttr := TextAttr or $08;
            if BlinkOn then
              TextAttr := TextAttr or $80;
          end;
        end;

      'n' : {DSR - device status report}
        if ParmInt[1] = 6 then
          ReportCursorPosition;

      's' : {SCP - save cursor position}
        begin
          SaveX := WhereX;
          SaveY := WhereY;
        end;

      'u' : {RCP - restore cursor position}
        GotoXY(SaveX, SaveY);

      else
        {Invalid esc sequence - display all the characters accumulated so far}
        while HeadChar(C) do
          case C of
            FormFeed : ClearScreen;
            BellChar : SoundBell;
            else Write(C);
          end;
    end;
  end;

  procedure WriteCharAnsi(C : Char);
    {-Writes C (and handles ANSI sequences)}
  Var
    SaveAttr : Word;
  label
    ErrorExit;
  begin
    PushChar(C);

    case ParserState of
      GotNone : {Not in an ANSI sequence}
        begin
          if C = Escape then
            ParserState := GotEscape
          else
            case C of
              LineFeed : {Clear background before scrolling window}
                begin
                  SaveAttr := TextAttr;
                  TextAttr := TextAttr and $0F;
                  Write(C);
                  TextAttr := SaveAttr;
                end;
              FormFeed : {Special case - clear screen on formfeed}
                ClearScreen;
              BellChar : {Special case - ring bell on bell character}
                SoundBell;
              else       {Normal character, just write it}
                if (WhereX <> Lo(WindMax)+1) then
                  Write(C)
                else case C of
                  cCR :
                    Write(cCR);
                  else
                    Write(C);
                    if InhibitAutoLF then
                      GotoXYCheck(Lo(WindMax)+1, WhereY-1);
                end;
            end;
          {Fast reinit of parser}
          QTail := 0;
        end;

      GotEscape : {Last character was escape -- need [}
        if C = LeftBracket then
          ParserState := GotBracket
        else
          goto ErrorExit;

      GotParm,
      GotBracket,
      GotSemicolon : {Need parameter char, semicolon, equalsign or command}
        if (C >= #48) and (C <= #57) then begin
          {It's a number, go add it to the current parameter}
          BuildParm(C);
          ParserState := GotParm;
        end else if (C = EqualSign) or (C = QuestionMark) then
          {just ignore it}
        else if C = Semicolon then
          {It's a semicolon, prepare for next parameter}
          if ParserState = GotSemicolon then
            {goto ErrorExit}                                           {!!.02}
          else begin
            ParserState := GotSemicolon;
            Inc(ParmIndex);
            if ParmIndex > MaxParms then
              goto ErrorExit;
          end
        else begin
          {Must be a command, go process it}
          ProcessCommand(C);
          InitParser;
        end;
    end;
    Exit;

  ErrorExit:
    {Invalid escape sequence -- display all the characters accumulated so far}
    while HeadChar(C) do
      Write(C);
    InitParser;
  end;

  procedure DefClearScreen;
  begin
    ClrScr;
  end;

  procedure DefSoundBell;
  begin
    Sound(220);
    Delay(200);
    NoSound;
  end;

  procedure SetClearScreenProc(CSP : ClearScreenProc);
    {-Sets a ClearScreen procedure to be called on FormFeed characters}
  begin
    ClearScreen := CSP;
  end;

  procedure SetSoundBellProc(SBP : SoundBellProc);
    {-Sets a SoundBell procedure to be called on Bell characters}
  begin
    SoundBell := SBP;
  end;

  procedure SetCurrentAnsiPort(P : PortRecPtr);
    {-Sets the current ansi port for any required ANSI output}
  begin
    CurrentAnsiPort := P;
  end;

begin
  InitParser;
  SoundBell := DefSoundBell;
  ClearScreen := DefClearScreen;
end.
