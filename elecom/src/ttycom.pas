unit ttyCom;
{$I COMPILER.INC}
(*
**
** Serial and TCP/IP communication routines for DOS, OS/2 and Win9x/NT.
** Tested with: TurboPascal   v7.0,    (DOS)
**              VirtualPascal v2.0,    (OS/2, Win32)
**              FreePascal    v0.99.10 (DOS, Win32)
**              Delphi        v2.0.    (Win32)
**
** Version : 0.01
** Created : 21-May-1998
** Last update : 10-Jun-1999
**
** Note: (c) 1998-1999 by Maarten Bekers
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Combase;

type TTtyObj = Object(TCommObj)
        ttyInput,
        ttyOutput  : Longint;


        constructor Init;
        destructor Done;

        function  Com_Open(Comport: Byte; BaudRate: Longint; DataBits: Byte;
                           Parity: Char; StopBits: Byte): Boolean; virtual;
        function  Com_OpenKeep(Comport: Byte): Boolean; virtual;
        function  Com_GetChar: Char; virtual;
        function  Com_CharAvail: Boolean; virtual;
        function  Com_Carrier: Boolean; virtual;
        function  Com_SendChar(C: Char): Boolean; virtual;
        function  Com_ReadyToSend(BlockLen: Longint): Boolean; virtual;
        function  Com_GetBPSrate: Longint; virtual;
        function  Com_GetHandle: longint; virtual;

        procedure Com_OpenQuick(Handle: Longint); virtual;
        procedure Com_Close; virtual;
        procedure Com_SendBlock(var Block; BlockLen: Longint; var Written: Longint); virtual;
        procedure Com_ReadBlock(var Block; BlockLen: Longint; var Reads: Longint); virtual;
        procedure Com_GetBufferStatus(var InFree, OutFree, InUsed, OutUsed: Longint); virtual;
        procedure Com_SetDtr(State: Boolean); virtual;
        procedure Com_GetModemStatus(var LineStatus, ModemStatus: Byte); virtual;
        procedure Com_SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte); virtual;
        procedure Com_PurgeInBuffer; virtual;
        procedure Com_PurgeOutBuffer; virtual;
     end; { object TTtyObj }

Type PTtyObj = ^TTtyObj;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Crt, Strings
       {$IFDEF ELEUNIX}
         {$IFDEF VER1_0}
            ,Linux
         {$ELSE}
            ,OldLinux
            ,Unix
         {$ENDIF}
         ,LongStr
       {$ENDIF}

       {$IFDEF WIN32}
         ,Windows
         ,Dos
       {$ENDIF}

       {$IFDEF GO32V2}
         ,Go32
       {$ENDIF} ;


Const GotCarrier : Boolean = true;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor TTtyObj.Init;
begin
  inherited Init;
  BlockAll := true;

  {$IFDEF EleUNIX}
    ttyInput := 0;
    ttyOutput := 1;
  {$ENDIF}

  {$IFDEF Win32}
    Assign(Output, '');
    Rewrite(Output);

    Assign(Input, '');
    Reset(Input);
  {$ENDIF}
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor TTtyObj.Done;
begin
  inherited Done;
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTtyObj.Com_Open(Comport: Byte; BaudRate: Longint; DataBits: Byte;
                             Parity: Char; StopBits: Byte): Boolean;
begin
  Com_Open := true;
end; { func. TTtyObj.Com_OpenCom }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTtyObj.Com_OpenKeep(Comport: Byte): Boolean;
begin
  Com_OpenKeep := true;
end; { func. Com_OpenKeep }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTtyObj.Com_OpenQuick(Handle: Longint);
begin
end; { proc. Com_OpenQuick }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTtyObj.Com_SetLine(BpsRate: longint; Parity: Char; DataBits, Stopbits: Byte);
begin
end; { proc. SetLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTtyObj.Com_GetBPSrate: Longint;
begin
  Com_GetBpsRate := 115200;
end; { func. TTtyObj.Com_GetBpsRate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTtyObj.Com_Close;
begin
end; { proc. TTtyObj.Com_Close }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTtyObj.Com_SendChar(C: Char): Boolean;
var Written: Longint;
begin
  Com_SendChar := true;
  if BlockAll then EXIT;

  Com_SendBlock(C, SizeOf(C), Written);
end; { proc. TTtyObj.Com_SendChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTtyObj.Com_GetChar: Char;
var Reads: Longint;
    TmpC : Char;
begin
  if BlockAll then EXIT;

  Com_ReadBlock(TmpC, SizeOf(TmpC), Reads);
  Result := TmpC;
end; { proc. TTtyObj.Com_ReadChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTtyObj.Com_ReadBlock(var Block; BlockLen: Longint; var Reads: Longint);
var BufType : Array[0..(1024 * 63)] of char ABSOLUTE Block;
    Counter : Longint;
    TmpReads: Longint;
    C       : Char;
begin
  Reads := 0;
  if BlockAll then EXIT;

  Counter := 0;
  if BlockLen = 0 then EXIT;
  REPEAT
    {$IFDEF ELEUNIX}
      TmpReads := fdRead(ttyInput, C, SizeOf(c));
    {$ENDIF}

    {$IFDEF Win32}
      ReadFile(TextRec(Input).Handle,
               C,
               SizeOf(c),
               TmpReads,
               nil);
    {$ENDIF}

    if TmpReads = SizeOf(c) then
      begin
        BufType[Counter] := C;
        Inc(Counter);
      end; { if }
  UNTIL (Counter >= BlockLen) OR (Counter > (1024*40));

  Reads := Counter;
end; { proc. TTtyObj.Com_ReadBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTtyObj.Com_SendBlock(var Block; BlockLen: Longint; var Written: Longint);
var Counter: Longint;
    BufType: Array[0..(1024 * 63)] of char ABSOLUTE Block;
begin
  if BlockAll then EXIT;
  if BlockLen = 0 then EXIT;

  Counter := 0;
  Written := 0;

  repeat
    {$IFDEF ELEUNIX}
      Counter := fdWrite(ttyOutput, BufType[Written], BlockLen - Written);
    {$ENDIF}

    {$IFDEF Win32}
      WriteFile(TextRec(Output).Handle,
                BufType[Written],
                BlockLen - Written,
                Counter,
                nil);
    {$ENDIF}

    Inc(Written, Counter);
  until (Written >= BlockLen);

  Written := BlockLen;
end; { proc. TTtyObj.Com_SendBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTtyObj.Com_CharAvail: Boolean;
{$IFDEF Win32}
var TmpInt: Longint;
{$ENDIF}

{$IFDEF ELEUNIX}
Const ttyIn = 0;

var fds: fdset;
{$ENDIF}
begin
  if BlockAll then
    begin
      Com_CharAvail := false;
      EXIT;
    end; { if }

  {$IFDEF Win32}
    if GetNumberOfConsoleInputEvents(TextRec(Input).Handle, TmpInt) then
      Com_CharAvail := (TmpInt <> 0)
        else Com_CharAvail := false;
  {$ENDIF}

  {$IFDEF ELEUNIX}
    {-- Try reading a character from remote -------------------------------------}
    FD_Zero(fds);
    FD_Set(TtyIn, fds);
    Com_CharAvail := (Select(ttyIn + 1, @fds, nil, nil, 0) <> 0);
  {$ENDIF}
end; { func. TTtyObj.Com_CharAvail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  TTtyObj.Com_ReadyToSend(BlockLen: Longint): Boolean;
begin
  if BlockAll then EXIT;

  Com_ReadyToSend := true;
end; { func. TTtyObj.Com_ReadyToSend }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTtyObj.Com_Carrier: Boolean;
begin
  Com_Carrier := GotCarrier;
  if BlockAll then EXIT;
end; { func. TTtyObj.Com_Carrier }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTtyObj.Com_SetDtr(State: Boolean);
begin
  GotCarrier := State;
end; { proc. TTtyObj.Com_SetDtr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTtyObj.Com_GetModemStatus(var LineStatus, ModemStatus: Byte);
begin
end; { proc. TTtyObj.Com_GetModemStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTtyObj.Com_GetBufferStatus(var InFree, OutFree, InUsed, OutUsed: Longint);
begin
  if com_CharAvail then
    begin
      InFree := 16383;
      InUsed := 1;
    end
      else begin
             InFree := 16384;
             Inused := 0;
           end; { else }

  OutFree := 16384;
  Outused := 0;
end; { proc. TTtyObj.Com_GetBufferStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTtyObj.Com_PurgeInBuffer;
begin
end; { proc. TTtyObj.Com_PurgeInBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TTtyObj.Com_PurgeOutBuffer;
begin
end; { proc. Com_PurgeOutBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TTtyObj.Com_GetHandle: longint;
begin
  Com_GetHandle := -1;
end; { func. Com_GetHandle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit ttyCOM }
