{$S-,R-,V-,I-,B-,F-,A-}
{$Ifdef msdos} {$M 16384, 0, 655360}{$endif}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{.$IFNDEF UseUart}                                                      {!!.02}
  {STOP COMPILE - This program requires UseUart                         {!!.02}
{.$ENDIF}                                                               {!!.02}

{Include OPro's define file if UseOPro is specified}
{$IFDEF UseOPro}
{$I OPDEFINE.INC}
{$ENDIF}

{$IFDEF UseStreams}
{$DEFINE CreateStream}      {Define this to test protocol streams}
{$ENDIF}

{.$DEFINE Test8KZmodem}       {Define this to test 8K zmodem}           {!!.01}

{*********************************************************}
{*                     FXO.PAS 2.03                      *}
{*     Copyright (c) TurboPower Software 1991.           *}
{*                 All rights reserved.                  *}
{*********************************************************}

program FXO;
  {-File transfer demonstration (using OOP version of AP)}

uses
  {$IFDEF LeakCheck}                                                   {!!.01}
  LeakChek,                                                            {!!.01}
  {$ENDIF}                                                             {!!.01}
  Dos,
  Crt,
{$ifdef msdos}  FastW1, {$endif}
  {$IFDEF UseOpro}
  OpRoot,
  {$ENDIF}
  {$IFDEF UseTpro}
  TpMemChk,
  {$ENDIF}
  ApMisc,
  ApTimer,
  ApPort,
{$ifdef win32}
  Win32Com,
  telnet,
{$endif}
{$ifdef msdos}
  Fos_Com,
{$endif}
{$ifdef os2}
  oS2Com,
  telnet,
{$endif}
  ApFossil,
  OoCom,
  OoAbsPcl,
  OoXmodem,
  OoYmodem,
  OoZmodem,
  OoKermit,
  OoAscii,
  OoBPlus;

const
  Version = 'FXO  Copyright(c) TurboPower Software. Version 2.03';

  {Number of entries to keep in the trace queue}
  TraceEntries = 10000;

  {Determines whether port is opened normal or with "keep"}
  BaudSpecified : Boolean = False;

  {Determines if background process is demonstrated}
  BackgroundDemo : Boolean = False;

  {Misc constants}
  WAttr : Byte = $1B;          {Window attribute}
  FAttr : Byte = $1E;          {Frame attribute}
  DAttr : Byte = $1F;          {Data attribute}
  StatusDelay = 2000;          {Delay 2 seconds for status messages}

type
  BufPtr = ^BufferArray;
  BufferArray = array[0..MaxInt] of Char;
  TransferModeType = (Transmit, Receive);

var
  SrcFile : File;
  SrcFilename : String;
  ComX : ComNameType;
  Baud : LongInt;
  {$IFDEF USEFOSSIL}
  ComPort : FossilPortPtr;
  {$ELSE}
  ComPort : UartPortPtr;
  {$ENDIF}
  SkipPortDone : Boolean;
  Protocol : Byte;
  TransferMode : TransferModeType;
  ResumeFile : Boolean;
  ClobberFile : Boolean;
  NewerLonger : Boolean;
  FLP : FileListPtr;
  SaveExit : Pointer;
  BW : Pointer;
  ForceAbort : Boolean;
  SkipNoFile : Boolean;

  {$F+}
  procedure FxoExitProc;
  begin
    ExitProc := SaveExit;

    {$IFDEF Tracing}
    {Save the trace to FXO.TRC}
    DumpTrace('FXO.TRC');
    {$ENDIF}

    {$IFDEF EventLogging}
    {Save the trace to FXO.LOG}
    DumpEvents('FXO.LOG');
    {$ENDIF}
  end;
  {$F-}

  procedure Abort(Msg : String; Code : Word);
  begin
    if ComPort <> Nil then
      Dispose(ComPort, Done);
    Write(Msg);
    if Code <> 0 then
      WriteLn(Code)
    else
      WriteLn;
    Halt(Code);
  end;

  function StUpcase(S : String) : String;
  var
    I : Byte;
  begin
    for I := 1 to Length(S) do
      S[I] := Upcase(S[I]);
    StUpcase := S;
  end;

  function CharStr(Ch : Char; Len : Byte) : string;
    {-Return a string of length len filled with ch}
  var
    S : string;
  begin
    if Len = 0 then
      CharStr[0] := #0
    else begin
      S[0] := Chr(Len);
      FillChar(S[1], Len, Ch);
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

  function Long2StrBlank(L : LongInt) : string;
    {-Convert a long/word/integer/byte/shortint to a string}
  begin
    if L = 0 then
      Long2StrBlank := ''
    else
      Long2StrBlank := Long2Str(L);
  end;

  function Real2Str(R : Real; Width : Byte; Places : ShortInt) : string;
    {-Convert a real to a string}
  var
    S : string;
  begin
    Str(R:Width:Places, S);
    Real2Str := S;
  end;

  function Pad(S : string; Len : Byte) : string;
    {-Return a string right-padded to length len with ch}
  var
    o : string;
    SLen : Byte absolute S;
  begin
    if Length(S) >= Len then
      Pad := S
    else begin
      o[0] := Chr(Len);
      Move(S[1], o[1], SLen);
      if SLen < 255 then
        FillChar(o[Succ(SLen)], Len-SLen, ' ');
      Pad := o;
    end;
  end;

  function LeftPad(S : string; Len : Byte) : string;
    {-Return a string left-padded to length len}
  var
    o : string;
    SLen : Byte absolute S;
  begin
    if Length(S) >= Len then
      LeftPad := S
    else if SLen < 255 then begin
      o[0] := Chr(Len);
      Move(S[1], o[Succ(Word(Len))-SLen], SLen);
      FillChar(o[1], Len-SLen, ' ');
      LeftPad := o;
    end;
  end;

  procedure WriteHelp;
    {-Write help and halt}
  begin
    WriteLn('Usage: FXO [options] SrcFilename');
    WriteLn('  /B BaudRate  Baudrate ');
    WriteLn('  /C #         Comport name  [default = 1]');
    WriteLn('  /T           Transmit mode [default]');
    WriteLn('  /R           Receive mode');
    WriteLn('  /S           ASCII transfer');
    WriteLn('  /X           Xmodem/XmodemCRC [default]');
    WriteLn('  /K           Xmodem1K');
    WriteLn('  /L           Xmodem1KG');
    WriteLn('  /Y           Ymodem');
    WriteLn('  /G           YmodemG');
    WriteLn('  /Z           Zmodem');
    WriteLn('  /A           Zmodem option - resume interrupted transfer');
    WriteLn('  /O           Zmodem option - always overwrite files');
    WriteLn('  /N           Zmodem option - only overwrite if newer');
    WriteLn('  /I           Zmodem option - skip if file doesn''t exist');
    WriteLn('  /F           Kermit');
    WriteLn('  /P           BPlus');
    WriteLn('  /D           Demonstration of background process');
    Halt;
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
    ComX := Com1;
    Baud := 1200;
    TransferMode := Transmit;
    Protocol := Xmodem;
    SrcFilename := '';
    ResumeFile := False;
    ClobberFile := False;
    NewerLonger := False;
    SkipNoFile := False;

    {Scan command line}
    if ParamCount = 0 then
      WriteHelp;
    Param := ParamStr(1);
    Cnt := 2;

    while True do begin
      case Param[1] of
        '/', '-' :
          if Length(Param) <> 2 then
            Abort('Invalid parameter: '+Param, 0)
          else
            case Upcase(Param[2]) of

              'B' : {Set baud rate}
                begin
                  BaudSpecified := True;
                  Param := ParamStr(Cnt);
                  Inc(Cnt);
                  Val(Param, Baud, Code);
                  if Code <> 0 then
                    Abort('Invalid baud rate: '+Param, 0);
                end;

              'C' : {Set Com port}
                begin
                  Param := ParamStr(Cnt);
                  Inc(Cnt);
                  Val(Param, ComNum, Code);
                  if Code <> 0 then
                    Abort('Invalid com port: '+Param, 0);
                  ComX := ComNameType(ComNum-1);
                end;

              'T' : TransferMode := Transmit;
              'R' : TransferMode := Receive;

              'S' : Protocol := Ascii;
              'X' : Protocol := Xmodem;
              'K' : Protocol := Xmodem1K;
              'L' : Protocol := Xmodem1KG;
              'Y' : Protocol := Ymodem;
              'G' : Protocol := YmodemG;
              'Z' : Protocol := Zmodem;
              'F' : Protocol := Kermit;
              'P' : Protocol := BPlus;

              'A' : ResumeFile := True;
              'O' : ClobberFile := True;
              'N' : NewerLonger := True;
              'I' : SkipNoFile := True;

              'D' : BackgroundDemo := True;

              '?' : {Request for help}
                WriteHelp;

            else
              Abort('Invalid parameter: '+Param, 0);
            end;
      else
        SrcFilename := Param;
      end;

      {Get next parameter}
      if Cnt > ParamCount then begin
        if (SrcFilename = '') and
           not ((TransferMode = Receive) and
                ((Protocol = Ymodem) or
                (Protocol = YmodemG) or
                (Protocol = Kermit) or
                (Protocol = Zmodem) or
                (Protocol = BPlus))) then
          WriteHelp;
        Exit;
      end;
      Param := ParamStr(Cnt);
      Inc(Cnt);
    end;
  end;

  function BuildWindow(XLow, YLow, XHigh, YHigh : Byte; Header : String) : Pointer;
    {-Saves the underlying screen, frames and clears a window}
  type
    FrameCharType = (ULeft, LLeft, URight, LRight, Horiz, Vert);
    FrameArray = array[FrameCharType] of Char;
  const
    FrameChars : FrameArray = '略맘固';
  var
    CoversP : BufPtr;
    WordsPerRow : Word;
    BufBytes : Word;
    SrcPos : Word;
    DestPos : Word;
    Row : Word;
    HeaderLen : Byte absolute Header;
    Width, HeaderPos : Byte;
    Span : string[132];
    SpanLen : Byte absolute Span;

  begin
    BuildWindow := nil;

    {Compute number of words to move per row}
    WordsPerRow := Succ(XHigh-XLow);

    {Compute bytes needed for screen buffer}
    BufBytes := (WordsPerRow*Succ(YHigh-YLow)) shl 1;

    {Make sure enough memory is available}
    if not GetMemCheck(CoversP, BufBytes) then
      Exit;

    {Save current contents to the screen buffer}
    DestPos := 0;
{    SrcPos := (Pred(YLow)*ScreenWidth+Pred(XLow)) shl 1;
    for Row := YLow to YHigh do begin
      MoveFromScreen(Mem[VideoSegment:SrcPos], CoversP^[DestPos], WordsPerRow);
      Inc(SrcPos, ScreenWidth shl 1);
      Inc(DestPos, WordsPerRow shl 1);
    end;
}

    {Calculate width of window and position of header}
    SpanLen := Succ(XHigh - XLow);
    Width := SpanLen-2;

    {construct the upper border and draw it}
    FillChar(Span[2], Width, FrameChars[Horiz]);
    Span[1] := FrameChars[ULeft];
    Span[SpanLen] := FrameChars[URight];
{    FastWrite(Span, YLow, XLow, FAttr); }

    {Draw the vertical bars}
    for Row := Succ(YLow) to Pred(YHigh) do begin
{      FastWrite(FrameChars[Vert], Row, XLow, FAttr);
      FastWrite(FrameChars[Vert], Row, XHigh, FAttr);
}
    end;

    {Draw the bottom border}
    Span[1] := FrameChars[LLeft];
    Span[SpanLen] := FrameChars[LRight];
{{    FastWrite(Span, YHigh, XLow, FAttr); }

    {Draw the header}
    if HeaderLen > 0 then begin
      if HeaderLen > Width then
        HeaderLen := Width;
      HeaderPos := (SpanLen-HeaderLen) shr 1;
{      FastWrite(Header, YLow, XLow + HeaderPos, FAttr); }
    end;

    {Fill in the window}
{    for Row := Ylow+1 to YHigh-1 do
      FastWrite(CharStr(' ', Pred(XHigh-XLow)), Row, XLow+1, FAttr);
}

    BuildWindow := CoversP;
  end;

  procedure RemoveWindow(P : Pointer; XLow, YLow, XHigh, YHigh : Byte);
    {-Restore screen contents and deallocate buffer space if requested}
  var
    CoversP : BufPtr absolute P;
    WordsPerRow : Word;
    SrcPos : Word;
    DestPos : Word;
    Row : Word;
  begin
    {Compute number of words to move per row}
    WordsPerRow := Succ(XHigh-XLow);

    {Restore current contents to the screen buffer}
    DestPos := 0;
{    SrcPos := (Pred(YLow)*ScreenWidth+Pred(XLow)) shl 1;
    for Row := YLow to YHigh do begin
      MoveToScreen(CoversP^[DestPos], Mem[VideoSegment:SrcPos], WordsPerRow);
      Inc(SrcPos, ScreenWidth shl 1);
      Inc(DestPos, WordsPerRow shl 1);
    end;
}

    {Deallocate buffer space}
    FreeMemCheck(CoversP, (WordsPerRow*Succ(YHigh-YLow)) shl 1);
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
    FormatMinSec := LeftPad(Long2Str(Min) + ':' + S, 6);               {!!.01}
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
{    if CharCount <> 0 then
      FastWrite(BarStr, Row, Col, DAttr); }
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
{      FastWrite(Msg, Row, Col, DAttr); }
      Msg := StatusStr(AsyncStatus);
{      FastWrite(Msg, Row, Col, DAttr); }
    end;
  end;

  {$F+}
  procedure BackgroundDemoWindow(PP : AbstractProtocolPtr);
    {-Demonstration of background hook}
  var
    Cnt : Word;
    C : Char;
  begin
    Cnt := 0;
    while KeyPressed and (Cnt < 10) do begin
      C := ReadKey;
      if C = #27 then
        {User requested protocol abort...}
        ForceAbort := True
      else
        {Just display this character}
        Write(C);
    end;
  end;
  {$F-}

  {$F+}
  procedure WindowStatus(AP : AbstractProtocolPtr;
                         Starting, Ending : Boolean);
    {-Windowed show status procedure}
  const
    XLow = 10;
    YLow = 3;
    XHigh = 69;
    YHigh = 21;
    P : Pointer = nil;
    DividerBar = '쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑';
    NewProgBar = '같같같같같같같같같같같같같같같같같같같같';
    HeaderStr : array[TransferModeType] of String[19] =
      (' Protocol Upload ', ' Protocol Download ');
    ModeStr : array[TransferModeType] of String[9] =
      ('sent:', 'received:');
    OnOffStr : array[Boolean] of String[3] = ('Off', 'On ');
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
  begin
(*
    if Starting then begin
      {Build and frame the window}
      P := BuildWindow(XLow, YLow, XHigh, YHigh, HeaderStr[TransferMode]);
      if P = nil then
        Abort('Insufficient memory ', 1);

      {Write out the fixed text strings}
{      FastWrite('Protocol:', YLow+1, XLow+2, WAttr);
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
{      if AP^.GetProtocol = Kermit then
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
        {$IFDEF UseHWFlow}
        B := AP^.APort^.HWFlowState <> fsOff;
        {$ELSE}
        B := False;
        {$ENDIF}
        FastWrite(OnOffStr[B], YLow+17, XLow+56, DAttr);
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
        bcNone      : S := bcsNone;
        bcChecksum1 : S := bcsChecksum1;
        bcChecksum2 : S := bcsChecksum2;
        bcCrc16     : S := bcsCrc16;
        bcCrc32     : S := bcsCrc32;
        bcCrcK      : S := bcsCrcK;
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
        R := CurBytesTransferred - GetInitialFilePos;
        ActualCPS := R / (CurElapsedTics / 18.2);
      end else
        ActualCPS := 0.0;
      FastWrite(LeftPad(Long2Str(Trunc(ActualCPS))+' CPS',9),
                YLow+8, XLow+49, DAttr);

      {Calculate and display efficiency}
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
      RemoveWindow(P, XLow, YLow, XHigh, YHigh);
*)
  end;

  function KbdAbort : Boolean;
    {-Default abort function}
  const
    Escape = #$1B;
  var
    Ch : Char;
  begin
    if BackgroundDemo and (BW <> nil) then
      {Let background process control abort}
      KbdAbort := ForceAbort
    else begin
      KbdAbort := False;
      if KeyPressed then begin
        Ch := ReadKey;
        if Ch = #0 then
          Ch := ReadKey;
        if Ch = Escape then
          KbdAbort := True;
      end;
    end;
  end;

  procedure LogFileActivity(AP : AbstractProtocolPtr; LogFileStatus : LogFileType);
    {-Maintains a history of all file transmits and receives}
  var
    FLog : Text;
    F : File;
    FName : PathStr;
    Prot : Byte;
  begin
    with AP^ do begin
      Assign(FLog, 'FXO.HIS');
      Append(FLog);
      if IOResult = 2 then
        ReWrite(FLog);
      if IOResult <> 0 then
        Exit;
      FName := GetPathName;
      Prot := GetProtocol;
      case LogFileStatus of
        lfReceiveStart :
          {do nothing} ;
        lfReceiveOk :
          WriteLn(FLog, ProtocolTypeString[Prot], ' receive ', FName);
        lfReceiveFail :
          begin
            WriteLn(FLog, ProtocolTypeString[Prot], ' receive aborted ', FName);
            if (GetProtocol <> Zmodem) and
               (GetProtocol <> BPlus) then begin
              Assign(F, FName);
              Erase(F);
              if IOResult <> 0 then ;
            end;
          end;
        lfReceiveSkip :
          WriteLn(FLog, ProtocolTypeString[Prot], ' receive skipped ', FName);
        lfTransmitStart :
          {do nothing} ;
        lfTransmitOk :
          WriteLn(FLog, ProtocolTypeString[Prot], ' transmit ', FName);
        lfTransmitFail :
          WriteLn(FLog, ProtocolTypeString[Prot], ' transmit aborted ', FName);
        lfTransmitSkip :
          WriteLn(FLog, ProtocolTypeString[Prot], ' transmit skipped ', FName);
      end;
      Close(FLog);
      if IOResult <> 0 then ;
    end;
  end;

  function FxoAcceptFile(AP : AbstractProtocolPtr) : Boolean;
    {-Test of file renaming with AcceptFile function}
  var
    FName : String[12];
  begin
    with AP^ do begin
      FName := GetFilename;
      FName[1] := '$';
      SetReceiveFileName(FName);
      FxoAcceptFile := True;
    end;
  end;

  procedure FxoErrorProc(P : Pointer; var StatusCode : Word);
  var
    AP : AbstractPortPtr absolute P;
    C : Char;
    S : String;
    W : Pointer;
  const
    BlankStr : String[78] =
  '                                                                              ';
  begin
    with AP^ do begin
      {Do nothing if a protocol is in progress}
      if ProtocolInProgress then
        Exit;
      if StatusCode mod 10000 <> 0 then begin
        {Build an error message}
        W := BuildWindow(1, 23, 80, 25, ' Press any key to continue ');
        if W = nil then begin
          {Not enough memory to show error -- just beep and exit}
          {Beep;}
          Exit;
        end;
        {FastWrite(BlankStr, 24, 2, DAttr);
        Str(AsyncStatus, S);
        FastWrite(
          'Error during processing ('+S+'): '+StatusStr(AsyncStatus),
           24, 2, DAttr);
        C := ReadKey;
        FastWrite(BlankStr, 24, 2, DAttr);
        RemoveWindow(W, 1, 23, 80, 25);}

      end;
    end;
  end;
  {$F-}

  procedure SetupDemo(PP : AbstractProtocolPtr);
    {-Setup for background demo window}
  begin
    {Setup for background hook demo}
    BW := BuildWindow(10, 22, 69, 25, ' Background Window ');
    if BW <> nil then begin
      PP^.SetBackgroundProc(BackgroundDemoWindow);
      Window(11, 23, 68, 24);
      GotoXY(11, 23);
      TextAttr := DAttr;
      ForceAbort := False;
    end;
  end;

  {$IFDEF CreateStream}
  procedure CreateStream(PP : AbstractProtocolPtr);
    {-Store PP to a stream}
  var
    S : BufIdStream;
    Status : Word;
  begin
    {Create a new stream}
    if not S.Init('PROTOCOL.STM', SCreate, 1024) then
      Abort('Failed to make stream: ', InitStatus);

    {Register all protocol hierarchies}
    S.RegisterHier(KermitProtocolStream);
    S.RegisterHier(ZmodemProtocolStream);
    S.RegisterHier(YmodemProtocolStream);
    S.RegisterHier(XmodemProtocolStream);
    S.RegisterHier(AsciiProtocolStream);
    S.RegisterHier(BPlusProtocolStream);
    Status := S.GetStatus;
    if Status <> 0 then
      Abort('Error registering protocol object: ', Status);

    {Register our user procedures}
    S.RegisterPointer(1000, @WindowStatus);
    S.RegisterPointer(1001, @LogFileActivity);

    (* if this code is used the port object is stored *)
    {Register port hierarchy}
    S.RegisterHier(UartPortStream);
    S.RegisterPointer(ptErrorProc, @FxoErrorProc);
    S.RegisterPointer(ptAbortProc, @KbdAbort);
    Status := S.GetStatus;
    if Status <> 0 then
      Abort('Error registering protocol object: ', Status);

    (* if this code is used only a pointer code to the port object is stored
    {Register our port}
    S.RegisterPointer(ptPortPtr, ComPort);
    *)

    {Store the protocol}
    S.PutPtr(PP);
    Status := S.GetStatus;
    if Status <> 0 then
      Abort('Error storing protocol: ', Status);

    {Clean up}
    S.Done;
  end;

  procedure LoadStream(var PP : AbstractProtocolPtr);
    {-Load PP from a stream}
  var
    S : BufIdStream;
    Status : Word;
  begin
    {Re-open existing new stream}
    if not S.Init('PROTOCOL.STM', SOpen, 1024) then
      Abort('Failed to open stream: ', InitStatus);

    {Register all protocol hierarchies}
    S.RegisterHier(KermitProtocolStream);
    S.RegisterHier(ZmodemProtocolStream);
    S.RegisterHier(YmodemProtocolStream);
    S.RegisterHier(XmodemProtocolStream);
    S.RegisterHier(AsciiProtocolStream);
    S.RegisterHier(BPlusProtocolStream);
    Status := S.GetStatus;
    if Status <> 0 then
      Abort('Error registering protocol object: ', Status);

    {Register our user procedures}
    S.RegisterPointer(1000, @WindowStatus);
    S.RegisterPointer(1001, @LogFileActivity);

    (* if this is code is used the port object is stored *)
    {Register port hierarchy}
    S.RegisterHier(UartPortStream);
    S.RegisterPointer(ptErrorProc, @FxoErrorProc);
    S.RegisterPointer(ptAbortProc, @KbdAbort);
    Status := S.GetStatus;
    if Status <> 0 then
      Abort('Error registering protocol object: ', Status);

    (* if this code is used only a pointer code to the port is stored
    {Register our port}
    S.RegisterPointer(ptPortPtr, ComPort);
    *)

    {Load the protocol}
    RootPtr(PP) := S.GetPtr;
    Status := S.GetStatus;
    if Status <> 0 then
      Abort('Error loading protocol: ', Status);

    {Clean up}
    S.Done;
  end;
  {$ENDIF}

  procedure TransferFiles;
    {-Send or receive files}
  var
    PP : AbstractProtocolPtr;
    Start : Boolean;
    Upload : Boolean;
    ET : EventTimer;
    C : Char;

    procedure SetHooks;
    begin
      with PP^ do begin
        SetShowStatusProc(WindowStatus);
        SetOverwriteOption(WriteRename);
        SetLogFileProc(LogFileActivity);
        SetFileMask(SrcFileName);

        if BackgroundDemo then
          SetupDemo(PP);

        (* Examples of AcceptFile procs
        SetAcceptFileFunc(FxoAcceptFile);
        SetAcceptFileFunc(AcceptOneFile);
        *)

        (* Examples of various options
        SetHandshakeWait(Secs2Tics(10), 5);
        apOptionsOn(apIncludeDirectory);
        *)

        (* Example of using file list
        SetNextFileFunc(NextFileList);
        MakeFileList(FLP, 500);
        AddFileToList(FLP, 'APDEFINE.INC');
        AddFileToList(FLP, 'APCOM.PAS');
        AddFileToList(FLP, 'E:\ASYNC\APKERMIT.PAS');
        AddFileToList(FLP, 'E:\ASYNC\APLZH.PAS');
        AddFileToList(FLP, 'E:\ASYNC\APMISC.PAS');
        AddFileToList(FLP, 'E:\ASYNC\APUART.PAS');
        SetFileList(FLP);
        *)
      end;
    end;

  begin
    {Instantiate proper protocol type and add specific customizations}
    case Protocol of
      Ascii :
        begin
          PP := New(AsciiProtocolPtr, Init(ComPort));
          if PP <> nil then begin
            if TransferMode = Transmit then begin
              AsciiProtocolPtr(PP)^.SetDelays(0, 100);
              PP^.SetFileMask(SrcFileName);
            end else
              PP^.SetReceiveFileName(SrcFileName);
          end;
        end;

      Xmodem, Xmodem1K, Xmodem1KG :
        begin
          PP := New(XmodemProtocolPtr, Init(ComPort,
                                            Protocol = Xmodem1K,
                                            Protocol = Xmodem1KG));
          if PP <> nil then
            with XmodemProtocolPtr(PP)^ do begin
              SetReceiveFileName(SrcFileName);
              SetBlockWait(RelaxedBlockWait);
            end;
        end;
      Ymodem, YmodemG :   {Ymodem derivatives}
        begin
          PP := New(YmodemProtocolPtr, Init(ComPort, True, Protocol = YmodemG));
          YmodemProtocolPtr(PP)^.SetBlockWait(RelaxedBlockWait);
        end;
      ZModem :            {Zmodem protocol}
        begin
          {$IFDEF Test8KZmodem}                                        {!!.01}
          PP := New(ZmodemProtocolPtr,                                 {!!.01}
                   InitCustom(ComPort, DefProtocolOptions or apZmodem8K)); {!!.01}
          {$ELSE}                                                      {!!.01}
          PP := New(ZmodemProtocolPtr, Init(ComPort));
          {$ENDIF}                                                     {!!.01}
          if PP <> nil then
            with ZmodemProtocolPtr(PP)^ do begin
              if ClobberFile then
                SetFileMgmtOptions(True, SkipNoFile, WriteClobber);
              if NewerLonger then
                SetFileMgmtOptions(True, SkipNoFile, WriteNewerLonger);
              if ResumeFile then
                SetRecoverOption(True);
              {$IFDEF Test8KZmodem}                                    {!!.01}
              SetBigSubPacketOption(True);                             {!!.01}
              {$ENDIF}                                                 {!!.01}
            end;
          end;

      Kermit :            {Kermit protocol}
        begin
          PP := New(KermitProtocolPtr, Init(ComPort));
          (* Uncomment the following to test window and long block support
          KermitProtocolPtr(PP)^.SetMaxWindows(15);
          KermitProtocolPtr(PP)^.SetMaxLongPacketLen(500);
          *)
        end;

      BPlus :            {BPlus protocol}
        begin
          PP := New(BPlusProtocolPtr, Init(ComPort));
          if PP <> nil then
            with BPlusProtocolPtr(PP)^, ComPort^ do begin
              {Need to set hooks now for terminal mode processing}
              SetHooks;

              NewTimerSecs(ET, 10);
              Start := False;

              {Assume ENQ already arrived...}
              ProcessENQ;

              {Process expected DLEs}
              repeat
                if CharReady then begin
                  GetChar(C);
                  if C = cDLE then
                    ProcessDLE(Start, Upload);
                end;
              until Start or TimerExpired(ET) or KbdAbort;             {!!.01}

              {Exit if we couldn't start the protocol}
              if not Start then begin                                  {!!.01}
                Dispose(PP, Done);                                     {!!.01}
                Abort('Failed to pick up B+ protocol in progress', 0); {!!.01}
              end;                                                     {!!.01}

              if Upload then
                TransferMode := Transmit
              else
                TransferMode := Receive;
            end;
        end;
    end;

    {Check results of initialization}
    if PP = nil then
      Abort('Failed to instantiate protocol ', AsyncStatus);

    with PP^ do begin
      {Add general customizations}
      if TypeOf(PP^) <> TypeOf(BPlusProtocol) then
        SetHooks;
    end;

    {$IFDEF CreateStream}
    CreateStream(PP);
    Dispose(PP, Done);
    LoadStream(PP);
    PP^.SetFileMask(SrcFileName);
    if PP^.DeallocPort then
      SkipPortDone := True;
    {$ENDIF}

    with PP^ do begin
      {Process the protocol request}
      case TransferMode of
        Transmit : ProtocolTransmit;
        Receive :  ProtocolReceive;
      end;

      {Cleanup}
      if AsyncStatus = ecOk then
        Write(^M^J'Transfer complete'^M^J)
      else
        Write(^M^J'Transfer failed ', AsyncStatus,^M^J);
      Dispose(PP, Done);
    end;
  end;

const telnetserv:boolean=false;

begin
    {$IFDEF WIN32}
      if NOT TelnetServ then ComObj := New(PWin32Obj, init)
        else ComObj := New(PTelnetObj, init);
    {$ENDIF}

    {$IFDEF OS2}
      if NOT TelnetServ then ComObj := New(POs2Obj, init)
        else ComObj := New(PTelnetObj, init);
    {$ENDIF}

    {$IFDEF MSDOS}
      ComObj := New(PFossilObj, init);
    {$ENDIF}

  {Set ComPort to nil so abort can see if it needs to be disposed}
  ComPort := nil;

  {Use standard out for error messages}
  Assign(Output, '');
  Rewrite(Output);

  {Display version}
  WriteLn(Version);

  {Get command line parameters}
  ParseCommandLine;

  {Use direct writes again for background test}
  AssignCrt(Output);
  Rewrite(Output);

  {Make a port object}
  if BaudSpecified then
    New(ComPort, InitCustom(ComX,
                            Baud, NoParity, 8, 1,
                            {$IFDEF Test8KZmodem}                      {!!.01}
                            16384, 16384+30,                           {!!.01}
                            {$ELSE}                                    {!!.01}
                            4096, 4096,
                            {$ENDIF}                                   {!!.01}
                            DefPortOptions))
  else begin
    {$IFDEF Test8KZmodem}                                              {!!.01}
    New(ComPort, InitKeep(ComX, 16384, 16384+30));                     {!!.01}
    {$ELSE}
    New(ComPort, InitKeep(ComX, 4096, 4096));                          {!!.01}
    {$ENDIF}
    ComPort^.ptOptionsOff(ptRestoreOnClose or ptDropModemOnClose);
    ComPort^.SetDTR(True);
    ComPort^.SetRTS(True);
  end;

  if ComPort = nil then
    Abort('Failed to open port, Status = ', AsyncStatus);

  {$IFDEF Tracing}
  {Start a trace}
  InitTracing(TraceEntries);
  {$ENDIF}

  {$IFDEF EventLogging}
  {Start a log}
  InitEventLogging(5000);
  {$ENDIF}

  {$IFDEF UseHWFlow}
  {Turn on hardware flow control (CTS only)}
  ComPort^.HWFlowEnable(3800, 200, hfUseRTS or hfRequireCTS);
  {$ENDIF}

  {Set the port-level user abort function}
  ComPort^.SetAbortFunc(KbdAbort);

  {Set the port-level user error handler}
  ComPort^.SetErrorProc(FxoErrorProc);

  {Flag to avoid disposing ComPort twice when testing streams}
  SkipPortDone := False;

  {Set a user exit proc}
  SaveExit := ExitProc;
  ExitProc := @FxoExitProc;

  {Transfer file(s)}
  TransferFiles;

  {Remove background demo window}
  if BackgroundDemo and (BW <> nil) then
    RemoveWindow(BW, 10, 22, 69, 25);

  {$IFDEF Tracing}
  {Save the trace to FXO.TRC}
  DumpTrace('FXO.TRC');
  {$ENDIF}

  {$IFDEF EventLogging}
  {Save the trace to FXO.LOG}
  DumpEvents('FXO.LOG');
  {$ENDIF}

  {Clean up}
  if not SkipPortDone then
    Dispose(ComPort, Done);
end.
