{$S-,R-,V-,I-,B-,F+,O+,A-}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{*********************************************************}
{*                    OOTFDD.PAS 2.03                    *}
{*     Copyright (c) TurboPower Software 1991.           *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OoTFDD;
  {-Text file device driver for port objects}

interface

uses
  Dos,
  ApMisc,
  ApPort,
  OoCom;

const
  SimulateLF : Boolean = True;

procedure AssignPortDev(var F : Text; P : AbstractPortPtr);
  {-Assign F to the opened port P}

function GetPortDevPtr(var F : Text) : AbstractPortPtr;
  {-Returns a pointer to the portrec associated with F}

implementation

type
  PortData = record
    AP : AbstractPortPtr;
    Junk : array[1..12] of Byte;
  end;

function PortDevIn(var F : TextRec) : Integer;
  {-Move chars from input to buffer}
var
  Cnt : Word;
  C : Char;
  Finished : Boolean;
  BufMax : Word;
begin
  with F, PortData(UserData).AP^ do begin

    {Loop until CR, Eof, async error, ^C or ^Break}
    Cnt := 0;
    BufMax := BufSize - 2;
    repeat
      {Get a character}
      repeat
        {Allow typical Ctl-C, Ctl-Break handling}
        Inline(
          $B8/$00/$01/           {       MOV     AX,$0100}
          $CD/$16/               {       INT     $16}
          $74/$0C/               {       JZ      NOBREAK}
          $3D/$03/$2E/           {       CMP     AX,$2E03}
          $74/$05/               {       JZ      BREAK}
          $3D/$00/$00/           {       CMP     AX,0}
          $75/$02/               {       JNZ     NOBREAK}
                                 {       BREAK:}
          $CD/$23);              {       INT     $23}
                                 {       NOBREAK:}
      until CharReady or PR^.UserAbort;

      Finished := True;
      if CharReady then begin
        GetChar(C);
        if AsyncStatus = ecOk then begin
          BufPtr^[Cnt] := C;
          if Cnt < BufMax then
            Inc(Cnt);
          case C of
            cCR :       {Carriage return}
              begin
                if SimulateLF then begin
                  if Cnt = BufMax then
                    Inc(Cnt);
                  BufPtr^[Cnt] := cLF;
                  Inc(Cnt);
                end;
              end;
            cLF, cSub : {Line feed or ^Z}
              ;
            else        {Normal character}
              Finished := False;
          end;
        end else begin
          {Error during GetChar, leave immediately}
          Cnt := 0;
          InOutRes := AsyncStatus;
        end;
      end else begin
        {User requested abort, leave immediately}
        Cnt := 0;
        InOutRes := ecUserAbort;
      end;
    until Finished;

    BufPos := 0;
    BufEnd := Cnt;
    PortDevIn := AsyncStatus;
  end;
end;

function PortDevOut(var F : TextRec) : Integer;
  {-Move chars from buffer to output}
var
  Cnt : Word;
  Tics : LongInt;
begin
  with F, PortData(UserData).AP^ do begin
    {Calculate drain time}
    Tics := (BufSize * LongInt(100)) div (PR^.CurBaud div 10);
    Tics := (Tics * 182) div 1000;

    {Send all chars in buffer}
    Cnt := 0;
    while Cnt < BufPos do begin
      PutChar(BufPtr^[Cnt]);
      if AsyncStatus = ecOk then
        Inc(Cnt)
      else begin
        PortDevOut := 0;
        BufPos := 0;
        DrainOutBuffer(Tics);
        Exit;
      end;
    end;
    BufPos := 0;
    DrainOutBuffer(Tics);
  end;
  PortDevOut := 0;
end;

function PortDevNA(var F : TextRec) : Integer;
  {-Dummy function for unused functions}
begin
  PortDevNA := 0;
end;

function PortDevOpen(var F : TextRec) : Integer;
  {-Open the TFDD for input or output}
begin
  with TextRec(F) do begin
    if Mode = fmInput then begin
      InOutFunc := @PortDevIn;
      FlushFunc := @PortDevNA;
    end else begin
      Mode := fmOutput;
      InOutFunc := @PortDevOut;
      FlushFunc := @PortDevOut;
    end;
  end;
  PortDevOpen := 0;
end;

procedure AssignPortDev(var F : Text; P : AbstractPortPtr);
  {-Assign F to the opened port P}
var
  Len : Byte;
  S : String[5];
begin
  with TextRec(F) do begin
    Handle := $FFFF;
    Mode := fmClosed;
    BufSize := SizeOf(Buffer);
    BufPtr := @Buffer;
    OpenFunc := @PortDevOpen;
    CloseFunc := @PortDevNA;
    PortData(UserData).AP := P;
    Len := Length(ComNameString(P^.PR^.PortName));
    S := ComNameString(P^.PR^.PortName);
    Move(S, Name, Len);
  end;
end;

function GetPortDevPtr(var F : Text) : AbstractPortPtr;
  {-Returns a pointer to the portrec associated with F}
begin
  with TextRec(F), PortData(UserData) do
    GetPortDevPtr := AP;
end;

end.

