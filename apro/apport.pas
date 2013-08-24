{$S-,R-,V-,I-,B-,F+,O+,A-}

{Conditional defines that may affect this unit}
{$I COMPILER.INC}
{$I APDEFINE.INC}

{*********************************************************}
{*                    APPORT.PAS 2.03                    *}
{*     Copyright (c) TurboPower Software 1991.           *}
{* Portions copyright (c) Information Technology 1989,   *}
{*    and used under license to TurboPower Software      *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit ApPort;
  {-Defines an abstract port data block and some common procedures}

interface

uses
  Dpmi,
  {$IFDEF UseOPro}
  OpRoot,
  OpDos,                                                               {!!.02}
  {$ENDIF}
  {$IFDEF UseTPro}
  TpMemChk,
  TpDos,                                                               {!!.02}
  {$ENDIF}
  ApMisc,
  {$IFNDEF DELPHI}
  Dos;
  {$ELSE}
  Windows,
  Sysutils;
  {$ENDIF}

{$I APPORT.PA0}

  {====================================================================}

implementation

var
  PortExitSave : Pointer;

{$IFDEF Tracing}
const
  HighestTrace = 32760;
type
  TraceRecord = record
    EventType : Char;
    C : Char;
  end;
  TraceQueueType = array[0..HighestTrace] of TraceRecord;
const
  TraceQueue : ^TraceQueueType = nil;
var
  TraceIndex : Word;
  TraceMax : Word;
  TraceWrapped : Boolean;
{$ENDIF}

function IsPS2 : Boolean;
  {-Returns True if the current machine is a PS/2}
{$IFDEF MSDOS}
type
  OS = record
    O : Word;
    S : Word;
  end;
  MachineID = record
    Length   : Word;
    Model    : Byte;
    SubModel : Byte;
  end;
var
  ID : ^MachineID;
  Regs : Registers;
  Model : ^Byte;
begin
  IsPS2 := False;
  Model := Ptr(BiosSele, $FFFE);

  {Exit if machine is pre-AT}
  if Model^ > $FC then
    Exit;

  {Get the location of the System Config Table}
  with Regs do begin
    AX := $C000;

    {Initialize ES:BX to nil}
    ES := 0;
    BX := 0;

    Intr($15, Regs);

    {Exit if carry set or ES:BX didn't change}
    if ((ES = 0) and (BX = 0)) or Odd(Flags) then
      Exit;

    OS(ID).S := ES;
    OS(ID).O := BX;
  end;

  {ID points to system config table}
  with ID^ do begin
    {Check for BIOS's that set the high order submodel bit}
    if (SubModel and $80) = $80 then
      Exit;
    {Check for PS/2s}
    if (Model < $FC) or
       ((Model = $FC) and (SubModel > $03)) then
      IsPS2 := True;
  end;

end;
{$ELSE}
begin
  IsPs2 := false;
end;
{$ENDIF}

function ComNameString(ComName : ComNameType) : String;
  {-Returns a displayable comport name string}
var
  S : String[3];
begin
  Str(Ord(ComName)+1, S);
  ComNameString := 'COM'+S;
end;

function CheckForString(var Index : Byte; C : Char;
                        S : String; IgnoreCase : Boolean) : Boolean;
  {-Checks for string S on consecutive calls, returns True when found}
begin
  CheckForString := False;
  Inc(Index);

  {Upcase both data if ignoring case}
  if IgnoreCase then begin
    C := Upcase(C);
    S[Index] := Upcase(S[Index]);
  end;

  {Compare...}
  if C = S[Index] then
    {Got match, was it complete?}
    if Index = Length(S) then begin
      Index := 0;
      CheckForString := True;
    end else
  else
    {No match, reset Index}
    if C = Upcase(S[1]) then
      Index := 1
    else
      Index := 0;
end;

procedure RotateIrqPriority(Irq : Byte);
  {-Rotate priorities to give Irq the highest priority at the PIC}
{$IFDEF MSDOS}
var
  OCW2 : Byte;
{$ENDIF}

{$IFDEF MSDOS}
  procedure RotatePrim(PortAddr : Byte; I : Byte);
  begin
    if I = 0 then
      I := 7
    else
      Dec(I);
    OCW2 := $C0 + I;
    Port[PortAddr] := OCW2;
  end;
{$ENDIF}

begin
{$IFDEF MSDOS}
  if (Irq >= 8) and (Irq <= $F) then
    {Alter slave}
    RotatePrim($A0, Irq-8)
  else if (Irq >= 0) and (Irq <=7) then
    {Alter master}
    RotatePrim($20, Irq)
{$ENDIF}
end;

procedure SetPS2DetectMode(Mode : PS2Mode);
begin
  PS2DetectMode := Mode;
end;

procedure NoErrorProc(P : Pointer; var StatusCode : Word);
  {-Dummy error procedure}
begin
end;

{$IFNDEF UseOOP}
procedure eInitPort(var P : PortRecPtr; ComName : ComNameType;
                    Baud : LongInt;
                    Parity : ParityType; DataBits : DataBitType;
                    StopBits : StopBitType;
                    InSize, OutSize : Word;
                    Options : Word);
  {-Error stub}
begin
  AsyncStatus := ecNoDevice;
end;

procedure eInitPortKeep(var P : PortRecPtr; ComName : ComNameType;
                        InSize, OutSize : Word);
  {-Error stub}
begin
  AsyncStatus := ecNoDevice;
end;

procedure eDonePort(var P : PortRecPtr);
  {-Error stub}
begin
  AsyncStatus := ecNoDevice;
end;


procedure eSetLine(P : PortRecPtr; Baud : LongInt; Parity : ParityType;
                  DataBits : DataBitType; StopBits : StopBitType);
  {-Error stub}
begin
  AsyncStatus := ecNoDevice;
end;

procedure eGetLine(P : PortRecPtr; var Baud : LongInt;
                  var Parity : ParityType;
                  var DataBits : DataBitType;
                  var StopBits : StopBitType;
                  FromHardware : Boolean);
  {-Error stub}
begin
  AsyncStatus := ecNoDevice;
end;

procedure eSetModem(P : PortRecPtr; SetDTR, SetRTS : Boolean);
  {-Error stub}
begin
  AsyncStatus := ecNoDevice;
end;

procedure eGetModem(P : PortRecPtr; var DTR, RTS : Boolean);
  {-Error stub}
begin
  AsyncStatus := ecNoDevice;
end;

procedure eGetChar(P : PortRecPtr; var C : Char);
  {-Error stub}
begin
  AsyncStatus := ecNoDevice;
end;

procedure ePeekChar(P : PortRecPtr; var C : Char; PeekAhead : Word);
  {-Error stub}
begin
  AsyncStatus := ecNoDevice;
end;

procedure ePutChar(P : PortRecPtr; C : Char);
  {-Error stub}
begin
  AsyncStatus := ecNoDevice;
end;

procedure eStartTransmitter(P : PortRecPtr);
  {-Error stub}
begin
  AsyncStatus := ecNoDevice;
end;

function eCharReady(P : PortRecPtr) : Boolean;
  {-Error stub}
begin
  AsyncStatus := ecNoDevice;
  eCharReady := False;
end;

function eTransReady(P : PortRecPtr) : Boolean;
  {-Error stub}
begin
  AsyncStatus := ecNoDevice;
  eTransReady := False;
end;

procedure eSendBreak(P : PortRecPtr);
  {-Error stub}
begin
  AsyncStatus := ecNoDevice;
end;

procedure eActivatePort(P : PortRecPtr; Restore : Boolean);
  {-Error stub}
begin
  AsyncStatus := ecNoDevice;
end;

procedure eDeactivatePort(P : PortRecPtr; Restore : Boolean);
  {-Error stub}
begin
  AsyncStatus := ecNoDevice;
end;

procedure eSavePort(P : PortRecPtr; var PSR);
  {-Error stub}
begin
  AsyncStatus := ecNoDevice;
end;

procedure eRestorePort(P : PortRecPtr; var PSR);
  {-Error stub}
begin
  AsyncStatus := ecNoDevice;
end;

procedure eGotError(P : PortRecPtr; StatusCode : Word);
  {-Error stub}
begin
end;

function eUpdateLineStatus(P : PortRecPtr) : Byte;
  {-Error stub}
begin
  AsyncStatus := ecNoDevice;
  eUpdateLineStatus := 0;
end;

function eUpdateModemStatus(P : PortRecPtr) : Byte;
  {-Error stub}
begin
  AsyncStatus := ecNoDevice;
  eUpdateModemStatus := 0;
end;

procedure eHWFlowSet(P : PortRecPtr; Enable : Boolean;
                     BufferFull, BufferResume : Word;
                     Options : Word);
begin
  AsyncStatus := ecNoDevice;
end;

function eHWFlowGet(P : PortRecPtr) : FlowState;
  {-Error stub}
begin
  AsyncStatus := ecNoDevice;
  eHWFlowGet := fsOff;
end;

procedure eSWFlowSet(P : PortRecPtr; Enable : Boolean;
                     BufferFull, BufferResume : Word;
                     Options : Word);
begin
  AsyncStatus := ecNoDevice;
end;

function eSWFlowGet(P : PortRecPtr) : FlowState;
  {-Error stub}
begin
  AsyncStatus := ecNoDevice;
  eSWFlowGet := fsOff;
end;

procedure eSWFlowCtl(P : PortRecPtr; OnChar, OffChar : Char;
                     Resume : Boolean);
  {-Error stub}
begin
  AsyncStatus := ecNoDevice;
end;

procedure eBufferStatus(P : PortRecPtr;
                        var InFree, OutFree, InUsed, OutUsed : Word);
  {-Error stub}
begin
  AsyncStatus := ecNoDevice;
end;

procedure eBufferFlush(P : PortRecPtr; FlushIn, FlushOut: Boolean);
  {-Error stub}
begin
  AsyncStatus := ecNoDevice;
end;
{$ENDIF}

procedure eSetUart(ComName : ComNameType; NewBase : Word;
                   NewIrq, NewVector : Byte);
  {-Error stub}
begin
  AsyncStatus := ecNoDevice;
end;

procedure ActivateErrorStubs;
  {-Activate the non-OOP error stubs}
begin
  {$IFNDEF UseOOP}
  InitPort := {$IFDEF FPC}@{$ENDIF}eInitPort;
  InitPortKeep := {$IFDEF FPC}@{$ENDIF}eInitPortKeep;
  DonePort := {$IFDEF FPC}@{$ENDIF}eDonePort;
  SetLine := {$IFDEF FPC}@{$ENDIF}eSetLine;
  GetLine := {$IFDEF FPC}@{$ENDIF}eGetLine;
  SetModem := {$IFDEF FPC}@{$ENDIF}eSetModem;
  GetModem := {$IFDEF FPC}@{$ENDIF}eGetModem;
  GetChar := {$IFDEF FPC}@{$ENDIF}eGetChar;
  PeekChar := {$IFDEF FPC}@{$ENDIF}ePeekChar;
  PutChar := {$IFDEF FPC}@{$ENDIF}ePutChar;
  StartTransmitter := {$IFDEF FPC}@{$ENDIF}eStartTransmitter;
  CharReady := {$IFDEF FPC}@{$ENDIF}eCharReady;
  TransReady := {$IFDEF FPC}@{$ENDIF}eTransReady;
  SendBreak := {$IFDEF FPC}@{$ENDIF}eSendBreak;
  ActivatePort := {$IFDEF FPC}@{$ENDIF}eActivatePort;
  DeactivatePort := {$IFDEF FPC}@{$ENDIF}eDeactivatePort;
  SavePort := {$IFDEF FPC}@{$ENDIF}eSavePort;
  RestorePort := {$IFDEF FPC}@{$ENDIF}eRestorePort;

  UpdateLineStatus := {$IFDEF FPC}@{$ENDIF}eUpdateLineStatus;
  UpdateModemStatus := {$IFDEF FPC}@{$ENDIF}eUpdateModemStatus;
  {$IFDEF UseHWFlow}
  HWFlowSet := {$IFDEF FPC}@{$ENDIF}eHWFlowSet;
  HWFlowGet := {$IFDEF FPC}@{$ENDIF}eHWFlowGet;
  {$ENDIF}
  {$IFDEF UseSWFlow}
  SWFlowSet := {$IFDEF FPC}@{$ENDIF}eSWFlowSet;
  SWFlowGet := {$IFDEF FPC}@{$ENDIF}eSWFlowGet;
  SWFlowCtl := {$IFDEF FPC}@{$ENDIF}eSWFlowCtl;
  {$ENDIF}
  BufferStatus := {$IFDEF FPC}@{$ENDIF}eBufferStatus;
  BufferFlush := {$IFDEF FPC}@{$ENDIF}eBufferFlush;

  {$ENDIF}
  SetUart := {$IFDEF FPC}@{$ENDIF}eSetUart;
end;

{$IFDEF Tracing}
procedure ClearTracing;
  {-Clears the trace buffer}
begin
  TraceIndex := 0;
  TraceWrapped := False;
end;

procedure AbortTracing;
  {-Stops tracing and destroys the tracebuffer}
begin
  if TraceQueue <> nil then begin
    TracingOn := False;
    FreeMemCheck(TraceQueue, TraceMax*2);
  end;
end;

procedure InitTracing(NumEntries : Word);
  {-Prepare a circular tracing queue}
begin
  AsyncStatus := ecOk;

  if TraceQueue <> nil then
    {Just clear buffer if already on}
    ClearTracing
  else begin
    {Limit check size of trace buffer}
    if NumEntries > HighestTrace then begin
      AsyncStatus := ecInvalidArgument;
      Exit;
    end;

    {Allocate trace buffer and start tracing}
    TraceMax := NumEntries;
    TraceIndex := 0;
    TraceWrapped := False;
    if not GetMemCheck(TraceQueue, NumEntries*2) then begin
      AsyncStatus := ecOutOfMemory;
      Exit;
    end;
  end;
  TracingOn := True;
end;

procedure AddTraceEntry(CurEntry : Char; CurCh : Char);
  {-Add a trace event to the global TraceQueue}
begin
  TraceQueue^[TraceIndex].EventType := CurEntry;
  TraceQueue^[TraceIndex].C := CurCh;
  Inc(TraceIndex);
  if TraceIndex = TraceMax then begin
    TraceIndex := 0;
    TraceWrapped := True;
  end;
end;

procedure DumpTracePrim(FName : PathStr; AppendTrace, DumpInHex : Boolean);
  {-Write the TraceQueue to FName}
const
  Digits : array[0..$F] of Char = '0123456789ABCDEF';
  LowChar : array[Boolean] of Byte = (32, 33);
var
  Start, Len : Word;
  TraceFile : Text;
  TraceFileBuffer : array[1..2048] of Char;
  LastEventType : Char;
  First : Boolean;
  Col : Byte;
  I : Byte;
label
  ExitPoint;

  function HexB(B : Byte) : string;
    {-Return hex string for byte}
  begin
    HexB[0] := #2;
    HexB[1] := Digits[B shr 4];
    HexB[2] := Digits[B and $F];
  end;

  procedure CheckCol(N : Byte);
    {-Wrap if N bytes would exceed column limit}
  begin
    Inc(Col, N);
    if Col > MaxTraceCol then begin
      WriteLn(TraceFile);
      Col := N;
    end;
  end;

begin
  {Make sure we have something to do}
  if TraceQueue = nil then
    Exit;

  {Set the Start and Len markers}
  Len := TraceIndex;
  if TraceWrapped then
    Start := TraceIndex
  else if TraceIndex <> 0 then
    Start := 0
  else
    {No events, just exit}
    goto ExitPoint;


  {Append to the specified trace file}                                 {!!.02}
  if AppendTrace and ExistFile(FName) then begin                       {!!.02}
    Assign(TraceFile, FName);                                          {!!.02}
    Append(TraceFile);                                                 {!!.02}
    if IoResult <> 0 then begin                                        {!!.02}
      AsyncStatus := ecTraceFileError;                                 {!!.02}
      goto ExitPoint;                                                  {!!.02}
    end;                                                               {!!.02}
    WriteLn(TraceFile);                                                {!!.03}
  end else begin                                                       {!!.02}
    {Open the file (overwritting any existing trace file)}
    Assign(TraceFile, FName);
    SetTextBuf(TraceFile, TraceFileBuffer, SizeOf(TraceFileBuffer));
    ReWrite(TraceFile);
    if IoResult <> 0 then begin
      AsyncStatus := ecTraceFileError;
      goto ExitPoint;
    end;
  end;                                                                 {!!.02}

  {Write the trace queue}
  LastEventType := #0;
  First := True;
  Col := 0;
  repeat
    {Some formattting}
    with TraceQueue^[Start] do begin
      if EventType <> LastEventType then begin
        if not First then begin
          WriteLn(TraceFile,^M^J);
          Col := 0;
        end;
        First := False;
        case EventType of
          'T' : WriteLn(TraceFile, 'Transmit: ');
          'R' : WriteLn(TraceFile, 'Receive: ');
          else  WriteLn(TraceFile, 'Special-'+EventType+': ');
        end;
        LastEventType := EventType;
      end;

      {Write the current char}
      if (Ord(C) < LowChar[DumpInHex]) or (Ord(C) > 126) then begin
        if DumpInHex then begin
          CheckCol(4);
          Write(TraceFile, '[',HexB(Ord(C)),']')
        end else begin
          if Ord(C) > 99 then
            I := 5
          else if Ord(C) > 9 then
            I := 4
          else
            I := 3;
          CheckCol(I);
          Write(TraceFile, '[',Ord(C),']')
        end;
      end else begin
        CheckCol(1);
        Write(TraceFile, C);
      end;

      {Get the next char}
      Inc(Start);
      if Start = TraceMax then
        Start := 0;
    end;
    {First := False;}                                                  {!!.03}
  until Start = Len;

  {End with a CR/LF}                                                   {!!.03}
  WriteLn(TraceFile);                                                  {!!.03}

  {Clean up}
ExitPoint:
  Close(TraceFile);
  if IOResult <> 0 then ;
  AbortTracing;
end;

procedure DumpTrace(FName : PathStr);
  {-Write the TraceQueue to FName}
begin
  DumpTracePrim(FName, False, False);
end;

procedure DumpTraceHex(FName : PathStr);
  {-Write the TraceQueue to FName}
begin
  DumpTracePrim(FName, False, True);
end;

{!!.02}
procedure AppendTrace(FName : PathStr);
  {-Append to the specified trace file}
begin
  DumpTracePrim(FName, True, False);
end;

{!!.02}
procedure AppendTraceHex(FName : PathStr);
  {-Append to the specified trace file, dump in hex}
begin
  DumpTracePrim(FName, True, True);
end;

procedure StartTracing;
  {-Restarts tracing after a StopTracing}
begin
  if TraceQueue <> nil then
    TracingOn := True;
end;

procedure StopTracing;
  {-Stops tracing temporarily}
begin
  TracingOn := False;
end;
{$ENDIF}

function NoAbortFunc : Boolean;
  {-Empty abort function}
begin
  NoAbortFunc := False;
end;

procedure PortExitProc;
  {-Exit procedure to close any open com ports}
var
  I : Byte;
begin
  ExitProc := PortExitSave;
  for I := 1 to MaxActivePort do
    if ActiveComPort[I] <> nil then
      ActiveComPort[I]^.DoneProc(ActiveComPort[I]);
end;

procedure GetProcPointers(var AInitPort          : InitPortProc;
                          var AInitPortKeep      : InitPortKeepProc;
                          var ADonePort          : DonePortProc;
                          var ASetLine           : SetLineProc;
                          var AGetLine           : GetLineProc;
                          var ASetModem          : SetModemProc;
                          var AGetModem          : GetModemProc;
                          var AGetChar           : GetCharProc;
                          var APeekChar          : PeekCharProc;
                          var APutChar           : PutCharProc;
                          var AStartTransmitter  : StartTransmitterProc;
                          var ACharReady         : CharReadyFunc;
                          var ATransReady        : TransReadyFunc;
                          var ASendBreak         : SendBreakProc;
                          var AActivatePort      : ActivatePortProc;
                          var ADeactivatePort    : ActivatePortProc;
                          var ASavePort          : SavePortProc;
                          var ARestorePort       : SavePortProc;
                          var AGotError          : GotErrorProc;
                          var AUpdateLineStatus  : UpdateLineStatusFunc;
                          var AUpdateModemStatus : UpdateModemStatusFunc;
                          var AHWFlowSet         : FlowSetProc;
                          var AHWFlowGet         : FlowGetFunc;
                          var ASWFlowSet         : FlowSetProc;
                          var ASWFlowGet         : FlowGetFunc;
                          var ASWFlowCtl         : FlowCtlProc;
                          var ABufferStatus      : BufferStatusProc;
                          var ABufferFlush       : BufferFlushProc;
                          var ASetUart           : SetUartProc;
                          var AAnsiOutput        : PutCharProc);
begin
  {$IFNDEF UseOOP}
  AInitPort := InitPort;
  AInitPortKeep := InitPortKeep;
  ADonePort := DonePort;
  ASetLine := SetLine;
  AGetLine := GetLine;
  ASetModem := SetModem;
  AGetModem := GetModem;
  AGetChar := GetChar;
  APeekChar := PeekChar;
  APutChar := PutChar;
  AStartTransmitter := StartTransmitter;
  ACharReady := CharReady;
  ATransReady := TransReady;
  ASendBreak := SendBreak;
  AActivatePort := ActivatePort;
  ADeactivatePort := DeactivatePort;
  ASavePort := SavePort;
  ARestorePort := RestorePort;
  AGotError := GotError;
  AUpdateLineStatus := UpdateLineStatus;
  AUpdateModemStatus := UpdateModemStatus;
  {$IFDEF UseHWFlow}
  AHWFlowSet := HWFlowSet;
  AHWFlowGet := HWFlowGet;
  {$ELSE}
  AHWFlowSet := eHWFlowSet;
  AHWFlowGet := eHWFlowGet;
  {$ENDIF}
  {$IFDEF UseSWFlow}
  ASWFlowSet := SWFlowSet;
  ASWFlowGet := SWFlowGet;
  ASWFlowCtl := SWFlowCtl;
  {$ELSE}
  ASWFlowSet := eSWFlowSet;
  ASWFlowGet := eSWFlowGet;
  ASWFlowCtl := eSWFlowCtl;
  {$ENDIF}
  ABufferStatus := BufferStatus;
  ABufferFlush := BufferFlush;
  ASetUart := SetUart;
  AAnsiOutput := AnsiOutput;
  {$ENDIF}
end;

var
  I : Integer;
begin
  {Install error stubs}
  ActivateErrorStubs;

  {Init all ports as available}
  for I := 1 to MaxActivePort do
    ActiveComPort[I] := nil;

  {Setup exit procedure to close open com ports}
  PortExitSave := ExitProc;
  ExitProc := @PortExitProc;

  {Init ANSI hook}
  {$IFNDEF UseOOP}
    AnsiOutput := {$IFDEF FPC}@{$ENDIF}ePutChar;
  {$ELSE}
    @AnsiOutput := nil;
  {$ENDIF}
end.
