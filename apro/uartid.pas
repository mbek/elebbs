{$S-,R-,V-,I-,B-,F-,A-}
{$M 16384, 0, 655360}

{Conditional defines that may affect this program}
{$I APDEFINE.INC}

{*********************************************************}
{*                    UARTID.PAS 2.03                    *}
{*     Copyright (c) TurboPower Software 1991.           *}
{*                 All rights reserved.                  *}
{*********************************************************}

program UartID;
  {-Identifies UART part numbers, optionally toggles FIFO buffering}

  (* Note: Since this program is specific to the APUART device layer
          it bypasses the global procedure pointers and calls the
          APUART routines directly. The major reason for doing this
          is to make this program compilable even if UseOOP is
          defined in APDEFINE.INC (which removes the global
          procedure pointers).
  *)

uses
  ApPort,
  ApUart;

const
  {Title line}
  Copyright : String[80] = 'UartID 2.03  Copyright 1991 TurboPower Software';

  {For displaying general on/off statuses}
  OnOff : array[Boolean] of String[3] = ('Off', 'On ');

  {For only reporting on uarts if they are in the BIOS data area}
  SafeMode : Boolean = False;

var
  CurCom, Com, StartCom, EndCom : ComNameType;
  BaseAddr : Word;
  PortNumber : Byte;
  ChangeFifoState : Boolean;
  NewFifoState : Boolean;
  NewFifoLevel : Byte;
  P : PortRecPtr;
  Baud : LongInt;
  Parity : ParityType;
  DataBits : DataBitType;
  StopBits : StopBitType;

label
  SkipIt;

procedure WriteHelp;
  {-Displays usage information and halts}
begin
  WriteLn('Usage : UARTID [options]');
  WriteLn('      1-8           Portnumber for toggling FIFO');
  WriteLn('      On/Off        New FIFO status');
  WriteLn('      L1/L4/L8/L14  Trigger level for FIFO to use');
  WriteLn('      S             Use Safe mode');
  WriteLn('      ?             Show this help');
  Halt;
end;

procedure ParseCommandLine;
  {-Gets command line options and sets various parameters.}
var
  Code : Word;
  Param : String;
  Cnt : Word;
  I : Byte;

begin
  {Set defaults}
  PortNumber := 0;
  ChangeFifoState := False;
  NewFifoLevel := 14;

  {Scan command line}
  if ParamCount = 0 then
    Exit;
  Param := ParamStr(1);
  Cnt := 2;

  while Length(Param) <> 0 do begin
    for I :=  1 to Length(Param) do
      Param[I] := Upcase(Param[I]);

    case Param[1] of
      '1'..'8' :
        begin
          Val(Param, PortNumber, Code);
          if PortNumber > 8 then
            WriteHelp;
        end;
      'L' : begin
              Delete(Param, 1, 1);
              Val(Param, NewFifoLevel, Code);
              if Code <> 0 then
                WriteHelp;
              if not (NewFifoLevel in [1, 4, 8, 14]) then
                WriteHelp;
            end;
      'O' : begin
              ChangeFifoState := True;
              if Param = 'ON' then
                NewFifoState := True
              else if Param = 'OFF' then
                NewFifoState := False
              else
                WriteHelp;
            end;
      'S' : SafeMode := True;
      '?' : WriteHelp;
      else
        WriteHelp;
    end;

    {Get next parameter}
    Param := ParamStr(Cnt);
    Inc(Cnt);
  end;
end;

begin
  {Get command line parms}
  WriteLn(Copyright,^M^J);
  ParseCommandLine;

  {Set for loop limits}
  StartCom := Com1;
  EndCom := Com8;

  {If a port number was specified, try to toggle that port's FIFO}
  if (PortNumber <> 0) then begin
    {Convert the port number to a ComName}
    CurCom := ComNameType(PortNumber-1);
    StartCom := CurCom;
    EndCom := CurCom;

    if ChangeFifoState then begin
      BaseAddr := DefBaseAddr[CurCom];
      if ClassifyUart(BaseAddr, True) = U16550A then
        SetFifoBuffering(BaseAddr, NewFifoState, NewFifoLevel)
      else begin
        WriteLn(ComNameString(CurCom), ' is not a 16550A');
        Halt;
      end;
    end;
  end;

  {Report on all requested ports}
  for Com := StartCom to EndCom do begin

    {Get the base address}
    BaseAddr := DefBaseaddr[Com];

    {If SafeMode then only report on UARTs the BIOS knows about}
    if SafeMode and not UartTest3(BaseAddr) then
      goto SkipIt;

    {Is it a UART?}
    if not UartTest1(BaseAddr) or not UartTest2(BaseAddr) then
      if StartCom = EndCom then begin
        WriteLn(ComNameString(CurCom), ' not found');
        Halt;
      end else
        goto SkipIt;

    {Write out start of status msg}
    Write(ComNameString(Com));

    {Write out uart type}
    case ClassifyUart(BaseAddr, False) of
      U8250A  : Write(' is an 8250A or 16450');
      U8250B  : Write(' is an 8250 or 8250-B');
      U16550  : Write(' is a 16550');
      U16550A : Write(' is a 16550A');
    end;

    {Report if uart is registered in BIOS data area}
    if UartTest3(BaseAddr) then
      Write(' [known to BIOS]')
    else
      Write(' [not known to BIOS]');

    {If Uart has FIFO buffers, report state of buffers}
    if ClassifyUart(BaseAddr, True) = U16550A then begin
      Write(' [FIFO buffering is ', OnOff[FifoStatus(BaseAddr)],']');
      if ChangeFifoState and FifoStatus(BaseAddr) then
        Write(' [Trigger level = ', NewFifoLevel, ']');
    end;

    {Finish line}
    WriteLn;

    {Get UART configuration}
    uInitPortKeep(P, Com, 10, 10);
    P^.Flags := P^.Flags and not ptRestoreOnClose;
    uGetLine(P, Baud, Parity, DataBits, StopBits, True);
    uDonePort(P);

    {Write out UART configuration}
    WriteLn('    Line parameters: ', Baud, ',',
                                     ParityString[Parity], ',',
                                     DataBits, ',',
                                     StopBits);
SkipIt:
  end;
end.
