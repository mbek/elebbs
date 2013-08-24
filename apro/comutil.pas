{$S-,R-,I-,V-}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{*********************************************************}
{*                   COMUTIL.PAS 2.03                    *}
{*     Copyright (c) TurboPower Software 1990.           *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$IFNDEF UseHWFlow}
  !! STOP COMPILE - this program requires UseHWFlow defined
{$ENDIF}

unit ComUtil;
  {-Support unit for Async Professional demo programs}

interface

uses
  {$IFDEF UseOpro}
  OpCrt,
  {$ELSE}
    {$IFDEF UseTpro}
    TpCrt,
    {$ELSE}
    Crt,
    {$ENDIF}
  {$ENDIF}
  Dos,
  ApPort,
  OOCom,
  OOModem,
  OOXModem;

type
  {types relating to the capture buffer}
  CaptureBufferType = Array[1..$FFF1] of Char;
  CaptureBufferPtr  = ^CaptureBufferType;
  CaptureObj =
    object
      captureF         : File;
      captureIndex     : Word;
      captureBufSize   : Word;
      captureBufPtr    : CaptureBufferPtr;
      captureIoRes     : Word;
      captureFileName  : PathStr;
      captureEnabled   : Boolean;
      constructor Init(BufferSize : Word);
      destructor Done;
      procedure capturePut(C : Char);
      procedure captureFlush;
      procedure captureClose;
      procedure captureOpen(FName : PathStr; AppendMode : Boolean);
    end;
  TerminalUart =
    object (UartPort)
      HWHState : Word;
      SWFlow : Boolean;
      constructor Init(ComName : ComNameType; NewBaud : LongInt;
                       HWHandshakeOptions : Word; SWFlowControl : Boolean);
        {-Opens ComName with defualt line options}
      constructor InitKeep(ComName : ComNameType; InSize, OutSize : Word;
                           HWHandshakeOptions : Word;
                           SWFlowControl : Boolean);
        {-Opens ComName (without changing line options)}
      constructor InitCustom(ComName : ComNameType; Baud : LongInt;
                             Parity : ParityType; DataBits : DataBitType;
                             StopBits : StopBitType;
                             InSize, OutSize : Word;
                             HWHandshakeOptions : Word;
                             SWFlowControl : Boolean;
                             PortOptions : Word);
        {-Opens the ComName com port}
      destructor Done; virtual;
      procedure GetBufferSizes(var InS, OutS : Word);
      procedure GetOptions(var HWHOptions, PortOptions : Word;
                           var SWFlowControl : Boolean);
      procedure SetHardwareFlowControl(HWHandshakeOptions : Word);
      {$IFDEF UseSWFlow}
      procedure SetSoftwareFlowControl(On : Boolean);
      {$ENDIF}
    end;

  {this type switches a port to 8 data bits, no parity, 1 stop bit for
   file transfers}
  SwitchToBinary =
    object
      Port     : AbstractPortPtr;
      Parity   : ParityType;
      DataBits : DataBitType;
      StopBits : StopBitType;
      Baud     : LongInt;
      constructor Init(AP : AbstractPortPtr);
        {-Save current settings and switch to 81N}
      destructor Done;
        {-Restore original settings}
    end;

const
  BufferFullPercent   : Real = 0.8;
  BufferResumePercent : Real = 0.2;

{general purpose routines}

function Pad(S : String; Width : Byte) : String;
  {-Pad a string to Width bytes (will truncate to Width if longer)}

function Center(S : String; Width : Byte) : String;
  {-Center S in a string of width bytes}

function Byte2Str(B : Byte) : String;
  {-convert a byte to a string}

function Long2Str(L : LongInt) : String;
  {-convert a LongInt to a string}

function Real2Str(R : Real; Width : Byte; Places : ShortInt) : string;
  {-Convert a real to a string}

function Str2Int(S : String; var I : Integer) : Boolean;
  {-convert a string to an integer}

function FileExists(FName : String) : Boolean;
  {-Returns True if FName exists}

function LeftPad(S : string; Len : Byte) : string;
  {-Return a string left-padded to length len}

function StUpcase(S : String) : String;

function Long2StrBlank(L : LongInt) : string;
  {-Convert a long/word/integer/byte/shortint to a string}

function HasWildcards(FName : String) : Boolean;
  {-Returns True if FName contains any wildcards (* or ?).}

function FormatMinSec(TotalSecs : LongInt) : String;
  {-Format TotalSecs as minutes:seconds}

function FormatMinTenths(TotalSecs : LongInt) : String;
  {-Format TotalSecs as minutes.tenths}

procedure ReadLine(var S : string; BufLen : Byte; var Escaped : Boolean);
  {-Read a line of at most Buflen characters into S}

implementation

  constructor CaptureObj.Init(BufferSize : Word);
  begin
    if MaxAvail < BufferSize then
      Fail;
    GetMem(captureBufPtr, BufferSize);
    captureBufSize := BufferSize;
    captureIndex   := 0;
    captureEnabled      := False;
    captureFileName := '';
  end;

  destructor CaptureObj.Done;
  begin
    if captureEnabled then
      captureClose;
    FreeMem(captureBufPtr, captureBufSize);
  end;

  procedure CaptureObj.capturePut(C : Char);
  begin
    Inc(captureIndex);
    captureBufPtr^[captureIndex] := C;
    if captureIndex = captureBufSize then
      captureFlush;
  end;

  procedure CaptureObj.captureFlush;
  begin
    if captureIndex > 0 then begin
      BlockWrite(captureF, captureBufPtr^, captureIndex);
      captureIoRes := IoResult;
      captureIndex := 0;
    end;
  end;

  procedure CaptureObj.captureClose;
  begin
    captureFlush;
    Close(captureF);
    captureIoRes := IoResult;
    captureEnabled := False;
  end;

  procedure CaptureObj.captureOpen(FName : PathStr; AppendMode : Boolean);
  begin
    captureFileName := FName;
    Assign(captureF, FName);
    if AppendMode then begin
      Reset(captureF, 1);
      captureIoRes := IoResult;
      if captureIoRes = 0 then begin
        if FileSize(captureF) > 0 then
          Seek(captureF, FileSize(captureF)-1)
        else
          Seek(captureF, FileSize(captureF));
      end;
    end
    else
      Rewrite(captureF, 1);
    captureIoRes := IoResult;
    captureEnabled := captureIoRes = 0;
  end;

  constructor TerminalUart.Init(ComName : ComNameType; NewBaud : LongInt;
                                HWHandshakeOptions : Word;
                                SWFlowControl : Boolean);
    {-Opens ComName with defualt line options}
  begin
    if not UartPort.InitFast(ComName, NewBaud) then
      Fail;
    SetHardwareFlowControl(HWHandshakeOptions);
    {$IFDEF UseSWFlow}
    SetSoftwareFlowControl(SWFlowControl);
    {$ENDIF}
  end;

  constructor TerminalUart.InitKeep(ComName : ComNameType;
                                    InSize, OutSize : Word;
                                    HWHandshakeOptions : Word;
                                    SWFlowControl : Boolean);
    {-Opens ComName (without changing line options)}
  begin
    if not UartPort.InitKeep(ComName, InSize, OutSize) then
      Fail;
    SetHardwareFlowControl(HWHandshakeOptions);
    {$IFDEF UseSWFlow}
    SetSoftwareFlowControl(SWFlowControl);
    {$ENDIF}
  end;

  constructor TerminalUart.InitCustom(ComName : ComNameType; Baud : LongInt;
                                      Parity : ParityType; DataBits : DataBitType;
                                      StopBits : StopBitType;
                                      InSize, OutSize : Word;
                                      HWHandshakeOptions : Word;
                                      SWFlowControl : Boolean;
                                      PortOptions : Word);
    {-Opens the ComName com port}
  begin
    if not UartPort.InitCustom(ComName, Baud, Parity, DataBits, StopBits,
                               InSize, OutSize, PortOptions) then
      Fail;
    SetHardwareFlowControl(HWHandshakeOptions);
    {$IFDEF UseSWFlow}
    SetSoftwareFlowControl(SWFlowControl);
    {$ENDIF}
  end;

  destructor TerminalUart.Done;
  begin
    UartPort.Done;
  end;

  procedure TerminalUart.GetBufferSizes(var InS, OutS : Word);
  begin
    with PR^ do begin
      InS  := InBuffLen;
      OutS := OutBuffLen;
    end;
  end;

  procedure TerminalUart.GetOptions(var HWHOptions, PortOptions : Word;
                                    var SWFlowControl : Boolean);
  begin
    with PR^ do
      PortOptions := Flags;
    HWHOptions := HWHState;
    SWFlowControl := SWFlow;
  end;

  procedure TerminalUart.SetHardwareFlowControl(HWHandshakeOptions : Word);
  var
    rFull, rResume : Real;
    InSize, OutSize : Word;
  begin
    HWHState := HWHandshakeOptions;
    if HWHandshakeOptions <> 0 then begin
      GetBufferSizes(InSize, OutSize);
      rFull := InSize * BufferFullPercent;
      rResume := InSize * BufferResumePercent;
      HWFlowEnable(Trunc(rFull), Trunc(rResume), HWHandshakeOptions);
    end
    else
      HWFlowDisable;
  end;

  {$IFDEF UseSWFlow}
  procedure TerminalUart.SetSoftwareFlowControl(On : Boolean);
  var
    rFull, rResume : Real;
    InSize, OutSize : Word;
  begin
    SWFlow := On;
    if On then begin
      GetBufferSizes(InSize, OutSize);
      rFull := InSize * BufferFullPercent;
      rResume := InSize * BufferResumePercent;
      SWFlowEnable(Trunc(rFull), Trunc(rResume));
    end
    else
      SWFlowDisable;
  end;
  {$ENDIF}

  function Pad(S : String; Width : Byte) : String;
  var
    L, I : Byte;
    Temp : String;
  begin
    L := Length(S);
    Temp := S;
    for I := Succ(L) to Width do
      Temp[I] := ' ';
    Temp[0] := Char(Width);
    Pad := Temp;
  end;

  function Center(S : String; Width : Byte) : String;
  var
    L, P : Byte;
    Temp : String;
  begin
    L := Length(S);
    if L >= Width then
      S[0] := Char(Width)
    else begin
      P := (Width div 2) - (L div 2);
      Temp := Pad(' ', P);
      S := Pad(Temp + S, Width);
    end;
    Center := S;
  end;

  function Byte2Str(B : Byte) : String;
  var
    S : String;
  begin
    Str(B, S);
    Byte2Str := S;
  end;

  function Long2Str(L : LongInt) : String;
  var
    S : String;
  begin
    Str(L, S);
    Long2Str := S;
  end;

  function Str2Int(S : String; var I : Integer) : Boolean;
  var
    Code : Integer;

  begin
    Val(S, I, Code);
    Str2Int := Code = 0;
  end;

  function FileExists(FName : String) : Boolean;
  var
    F : File;
    A : Word;
  begin
    Assign(F, FName);
    GetFAttr(F, A);
    FileExists := (DosError = 0);
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

  function StUpcase(S : String) : String;
  var
    I : Byte;
  begin
    for I := 1 to Length(S) do
      S[I] := Upcase(S[I]);
    StUpcase := S;
  end;

  function Long2StrBlank(L : LongInt) : string;
    {-Convert a long/word/integer/byte/shortint to a string}
  begin
    if L = 0 then
      Long2StrBlank := ''
    else
      Long2StrBlank := Long2Str(L);
  end;

  function HasWildcards(FName : String) : Boolean;
    {-Returns True if FName contains any wildcards (* or ?).}
  var
    P1, P2 : Byte;
  begin
    P1 := Pos('*', FName);
    P2 := Pos('?', FName);
    HasWildcards := (P1 + P2 > 0)
  end;

  function Real2Str(R : Real; Width : Byte; Places : ShortInt) : string;
    {-Convert a real to a string}
  var
    S : string;
  begin
    Str(R:Width:Places, S);
    Real2Str := S;
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

  function FormatMinTenths(TotalSecs : LongInt) : String;
    {-Format TotalSecs as minutes.tenths}
  var
    Min : Real;
    S : String;
  begin
    Min := TotalSecs / 60;
    Str(Min:6:1, S);
    FormatMinTenths := S;
  end;

  constructor SwitchToBinary.Init(AP : AbstractPortPtr);
  begin
    AP^.GetLine(Baud, Parity, DataBits, StopBits, False);
    Port := AP;
    AP^.SetLine(0, NoParity, 8, 1);
  end;

  destructor SwitchToBinary.Done;
  begin
    Port^.SetLine(0, Parity, DataBits, StopBits);
  end;

  procedure ReadLine(var S : string; BufLen : Byte; var Escaped : Boolean);
    {-Read a line of at most Buflen characters into S}
  var
    C : Char;
    SLen : Byte absolute S;
    First : Boolean;

    procedure BackSpace(N : Byte);
    var
      I : Byte;
    begin
      for I := 1 to N do
        if SLen > 0 then begin
          Dec(SLen);
          Write(^H' '^H)
        end;
    end;

  begin
    Write(S);
    Escaped := False;
    First := (SLen <> 0);
    repeat
      C := ReadKey;
      case C of
        ^H, #127 :
          Backspace(1);
        #0 :
          Escaped := (ReadKey = #0);
        ^C, ^M, #27 :
          Escaped := (C <> ^M);
        #32..#255 :
          begin
            if First then
              BackSpace(SLen);
            if SLen < BufLen then begin
              Inc(SLen);
              S[SLen] := C;
              Write(C);
            end;
          end;
      end;
      First := False;
    until (C = ^M) or Escaped;
    WriteLn;
  end;

end.
