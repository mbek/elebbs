unit StrEdit;
(*
**
** Copyright (C) 1999-2003 Maarten Bekers. All rights reserved.
**
** This file is part of EleBBS, EleXer and EleWEB.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
** This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
** WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
**
*)
{$I COMPILER.INC}
{$IFDEF WIN32}
  {$H-}
{$ENDIF}

{$IFDEF FPC}
 {$H-}
{$ENDIF}
(*
**
** STREDIT.TPU, string editor for ELCONFIG
**
** Copyright (c) 1998 by Maarten Bekers
**
** Created : 04-Nov-1998
** Last update : 04-Nov-1998
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF MSDOS}
uses Use32, CfgRec, Global;
{$ENDIF}

{$IFDEF WIN32}
uses CfgRec, Global
        {$IFDEF VirtualPascal}
          , VpSysLow
        {$ENDIF};
{$ENDIF}

{$IFDEF OS2}
uses CfgRec, Global, Os2Base, VpSysLow, VpUtils;
{$ENDIF}

{$IFDEF GO32V2}
uses Use32, CfgRec, Global;
{$ENDIF}

{$IFDEF ELEUNIX}
uses Use32, CfgRec, Global;
{$ENDIF}

Type CharSet = Set of Char;

procedure GetInput;
procedure KeyDelay(W:Longint; Remove: Boolean);
function  GetKey: Byte;
function  ReadKey: Char;
function  KeyPressed: Boolean;
function  ShiftState: Boolean;

procedure EditWord (var S : SmallWord; X,Y,Attr:Byte; EscAbort: Boolean);
procedure EditInteger(var L: SmallInt; X,Y,Attr, Len:Byte; EscAbort: Boolean);
procedure EditLongint(var L: Longint; X,Y,Attr, Len:Byte; EscAbort: Boolean);
procedure EditReal (var S : Real; X,Y,Attr:Byte; EscAbort: Boolean);
procedure EditByte (var S : Byte; X,Y,Attr:Byte; EscAbort: Boolean);
procedure Edit_Boolean (var B: Boolean; X,Y,Attr:Byte);
procedure EditTimeWord(var Time: SmallWord; X, Y: Byte; EscAbort: Boolean);
procedure EditBitArray32(Title: String; var BitArray: ByteArray32);
procedure EditAddress(X, Y: Byte; var NetAddr: NetAddress; Escabort: Boolean);
procedure EditAskType(X, Y: Byte; var AskEdit: Byte; doYes, doNo, doAsk, doOnly: Boolean);
procedure EditTime(var TimeStr: Time; X, Y: Byte; EscAbort: Boolean);
procedure EditModemSpeed(var L: Longint; X, Y: Byte);
procedure EditBaudRate(var W: SmallWord; X, Y: Byte);
procedure GetString(X,Y,Attr:Byte; Var S:String; Chars, Abort:CharSet; Len, ShowLen:Byte; AllUpper,ColorMap,EscAbort,IsPsw:
                     Boolean; DefColor: Byte);
function  EditKey (EditStr : Char; X,Y, Attr:Byte; EscAbort: Boolean) : Char;
function  Edit (S : String; X,Y, Attr, Len:Byte; AllUpper, ColorMap,EscAbort: Boolean):String;
function  Edit_Bit (Bits, Nr :Byte; X,Y,Attr:Byte):Byte;
function  PubReadKey: Char;
function  PubKeypressed: Boolean;

Const
    ExtraWaiting     : Boolean = False;
    ExtraKey         : Char    = #00;
    ShowSample       : Boolean = False;

const IdleProc        : procedure = nil;

var HelpProc        : Procedure(ID:Word);         { Will trap when F1 pressed }
    KeyProc         : Procedure(Var Key:Char);   { Will trap keys AFTER a #00 }
    EnterProc       : Procedure(Var Key:Char);    { Will trap the [ENTER] key }
    KeyHookProc     : Function(var CH: Char): Boolean; { Will allow for extra keys }
    KeyFuncPressed  : Function: Boolean;

    rdLastKey       : Char;                                { Last key pressed }
    Helping         : Boolean;                   { Currently in help-function }
    rdAlEntered     : Boolean;
    curMenuID       : Word;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses ScrnU, GenCfg, Multi, LongStr, FidoAddr, Ranges,
      MenuSys, BitWise, CentrStr, Cases, StUtils, CheckBit,
       ObjDec
      {$IFDEF ELEUNIX}
        ,Crt
      {$ELSE}
        {$IFNDEF DELCRT}
          ,Crt
        {$ELSE}
          ,Crt32
        {$ENDIF}
      {$ENDIF}
      {$IFNDEF WITH_FULL}
         ,MacroLst
      {$ENDIF}
        {$IFDEF WITH_DEBUG}
          ,Debug_U
        {$ENDIF};

procedure DefIdleProc;
begin
end; { proc. DefIdleproc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GetInput;
begin
  UpdateScreenBuffer(true);
  if Readkey=#00 then READKEY;
end; { proc. GetInput }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function DefaultKeyHookProc(var cH: Char): Boolean;
begin
  DefaultKeyhookProc := false;
end; { func. DefaultKeyHookProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function DefaultKeyPressed: Boolean;
begin
  DefaultKeyPressed := false;
end; { func. DefaultKeyPressed }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function KeyPressed: Boolean;
begin
  Keypressed := false;

  if {$IFDEF ELEUNIX}Crt{$ELSE}{$IFDEF DELCRT}Crt32{$ELSE}Crt{$ENDIF}{$ENDIF}.KeyPressed then KeyPressed := true
   else if KeyFuncPressed{$IFDEF FPC}(){$ENDIF} then KeyPressed := true;
end; { func. Keypressed }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ReadKey: Char;
var TempCH: Char;
    AltKey: Char;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logInOut, 'Readkey (start)');
  {$ENDIF}

  if NOT ExtraWaiting then
    While (NOT Keypressed) AND (NOT Extrawaiting) do
      begin
        DoSlice;
        if @IdleProc <> nil then IdleProc;
      end; { while }

  if NOT KeyHookProc(TempCH) then
    begin
      if NOT ExtraWaiting then
        begin
          {$IFDEF ELEUNIX}
            TempCH := Crt.ReadKey;
          {$ELSE}
            TempCH := {$IFDEF DELCRT}Crt32{$ELSE}Crt{$ENDIF}.Readkey
          {$ENDIF}
        end
         else begin
                ExtraWaiting := False;
                TempCH := Extrakey;
              end; { ExtraWating }
    end; { keyhook }


  if (rdLastKey=#00) AND (TempCH=#59) then
     HelpProc(CurMenuID);                                 { Help pressed }

  if rdLastKey=#00 then
    KeyProc(TempCH);

  if TempCH = #26 then  { ctrl-z }
     begin
       TempCH := #00;    { extended key }
       ExtraWaiting := TRUE; { the actual key }

       {$IFDEF ELEUNIX}
         AltKey := Crt.Readkey;
       {$ELSE}
         AltKey := {$IFDEF DELCRT}Crt32{$ELSE}Crt{$ENDIF}.ReadKey; { get the key }
       {$ENDIF}

       {-- and convert it ---}
       {-- i dont use a table cause its too much work for a few keys -----}
       case UpCase(AltKey) of
         'Q' : ExtraKey := #16;
         'W' : ExtraKey := #17;
         'E' : ExtraKey := #18;
         'R' : ExtraKey := #19;
         'T' : ExtraKey := #20;
         'Y' : ExtraKey := #21;
         'U' : ExtraKey := #22;
         'I' : ExtraKey := #23;
         'O' : ExtraKey := #24;
         'P' : ExtraKey := #25;
         'A' : ExtraKey := #30;
         'S' : ExtraKey := #31;
         'D' : ExtraKey := #32;
         'F' : ExtraKey := #33;
         'G' : ExtraKey := #34;
         'H' : ExtraKey := #35;
         'J' : ExtraKey := #36;
         'K' : ExtraKey := #37;
         'L' : ExtraKey := #38;
         'Z' : ExtraKey := #44;
         'X' : ExtraKey := #45;
         'C' : ExtraKey := #46;
         'V' : ExtraKey := #47;
         'B' : ExtraKey := #48;
         'N' : ExtraKey := #49;
         'M' : ExtraKey := #50;
       end; { case }
     end; { if }

  if (TempCH=#13) AND
   (NOT rdAlEntered) then EnterProc(TempCH);

  if (TempCH = #12) then
    UpdateScreenBuffer(true);

  rdLastKey := TempCH;
  ReadKey := TempCH;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logInOut, 'Readkey (end)');
  {$ENDIF}
end; { func. ReadKey }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function PubReadKey: Char;
begin
  PubReadKey := Readkey;
end; { func. PubReadKey }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function PubKeypressed: Boolean;
begin
  PubKeypressed := Keypressed;
end; { func. PubKeyPressed }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetKey: Byte;
begin
  GetKey := Ord(ReadKey);
end; { func. GetKey }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure KeyDelay(W: Longint; Remove: Boolean);
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logInOut, 'Keydelay');
  {$ENDIF}
  If W>10 then Dec(W, 2);     { Make up for checking keypressed all the time! }

  repeat;
    Delay(01);
    Dec(W,01);

    DoSlice;
    {$IFNDEF MSDOS}
      Dec(W, 10); { sleep 1 also decreasesd one second }
    {$ENDIF}
  until (W < 01) OR (KeyPressed);

  if Remove then
    While Keypressed do ReadKey;
end; { proc. KeyDelay }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF MSDOS}
function ShiftState: Boolean;
var C: Byte absolute $40:$17;
begin
  ShiftState := FALSE;

  if C AND 2 = 2 then ShiftState := TRUE;
  if C AND 1 = 1 then ShiftState := TRUE;
end; { func. MsDos }
{$ELSE}
function ShiftState: Boolean;

{$IFDEF OS2}
var Left  : Boolean;
    Right : Boolean;
{$ENDIF}

{$IFDEF GO32V2}
var C     : Byte;
{$ENDIF}

begin
 ShiftState := false;

 {$IFDEF VIRTUALPASCAL}
  {$IFDEF OS2}
    Left := GetKeyboardState(kbdstf_LeftShift);
    Right := GetKeyboardState(kbdstf_RightShift);

    if (Left) OR (Right) then ShiftState := true
      else ShiftState := false;
  {$ENDIF}

  {$IFDEF WIN32}
    ShiftState := false;

    if SysTvGetShiftState AND 1 = 1 then ShiftState := TRUE;
    if SysTvGetShiftState AND 2 = 2 then ShiftState := TRUE;
  {$ENDIF}
 {$ENDIF}

  {$IFDEF GO32V2}
    ShiftState := FALSE;
    C := Mem[$40:$17];

    if C AND 2 = 2 then ShiftState := TRUE;
    if C AND 1 = 1 then ShiftState := TRUE;
  {$ENDIF}
end; { func. ShiftState }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditWord (var S : SmallWord; X,Y,Attr:Byte; EscAbort: Boolean);
var Temp  : String;
begin
  Str(S, Temp);

  repeat
    GetString(X, Y, Attr, Temp, ['0'..'9'], [#13, #27], 5, 5, false, false,
              EscAbort, false, 00);

    if rdLastKey=#27 then
      Str(S, Temp);

    if FVal(Temp) > 65535 then Write(^g);
    S := SmallWord(FVal(Temp));

  until (S < 65536);
end; { proc. EditWord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditInteger(var L : SmallInt; X,Y,Attr, Len:Byte; EscAbort: Boolean);
var Temp  : String;
begin
  Str(l, Temp);

  GetString(X, Y, Attr, Temp, ['0'..'9'], [#13, #27], len, len, false,
            false, EscAbort, false,0);

  if rdLastKey=#27 then Str(L, Temp);
  L := FVal(Temp);
end; { proc. EditInteger }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditLongint(var L: Longint; X,Y,Attr, Len:Byte; EscAbort: Boolean);
var Temp  : String;
begin
  Str(l, Temp);

  GetString(X, Y, Attr, Temp, ['0'..'9'], [#13, #27], len, len, false,
            false, EscAbort, false,0);

  if rdLastKey=#27 then Str(L, Temp);
  L := FVal(Temp);
end; { proc. EditLongint }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


procedure EditReal (var S : Real; X,Y,Attr:Byte; EscAbort: Boolean);
var Temp: String;
begin
  Temp := Real2Str(S, 0, 3);

  GetString(X, Y, Attr, Temp, ['0'..'9', '.'], [#13, #27], 8, 8, false,
            false, EscAbort, false, 0);

  if rdLastKey=#27 then Temp := Real2Str(S, 0, 3);
  S := StrToReal(Temp);
end; { proc. EditReal }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditByte (var S : Byte; X,Y,Attr:Byte; EscAbort: Boolean);
var TempStr: String;
    Temp   : Longint;
begin
  Str(S, TempStr);
  GetString(X, Y, Attr, TempStr, ['0'..'9'], [#13, #27], 3, 3, false,
            false, EscAbort, false,0);

  Temp := FVal(TempStr);

  if Temp < 0 then Temp := 00;
  if Temp > 255 then Temp := 255;

  S := Byte(FVal(TempStr));
end; { proc. EditByte }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function EditKey (EditStr : Char; X,Y, Attr:Byte; EscAbort: Boolean) : Char;
var TempStr: String;
begin
  TempStr := EditStr;
  GetString(X, Y, Attr, TEmpStr, [#32..#255], [#13, #27], 1, 1, false, false,
            EscAbort, false, 00);

  EditStr := TempStr[01];
  EditKey := EditStr;
end; { func. EditKey }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Edit (S :String; X,Y, Attr, Len:Byte; AllUpper, ColorMap, EscAbort: Boolean):String;
begin
  GetString(X, Y, Attr, S, [#32..#255], [#13, #27], Len, Len, AllUpper, ColorMap, EscAbort, false, 0);
  Edit := S;
end; { func. Edit }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Edit_Bit (Bits, Nr :Byte; X,Y,Attr:Byte): Byte;
begin
  if ReadBit(Bits, Nr) then
    ClearBit(Bits, Nr)
     else SetBit(Bits, Nr);

  WriteAT(X, Y, Attr, Bit2Str(Bits, Nr));

  Edit_Bit := Bits;
end; { func. Edit_Bit }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Edit_Boolean (var B: Boolean; X,Y,Attr:Byte);
begin
  B := NOT B;

  if B then WriteAT(X, Y, Attr, 'Yes')
    else WriteAT(X, Y, Attr, 'No ');
end; { proc. Edit_Boolean }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditBaudRate(var W: SmallWord; X,Y: Byte);
var Index  : Byte;
    Counter: Byte;
    Temp   : String;
begin
  for Counter := 01 to MaxBaudRates do
    if BaudRates[Counter] = W then
      begin
        Index := Counter;
        BREAK;
      end; { if }

  Inc(Index);
  if Index > maxBaudRates then
    Index := 01;
  W := BaudRates[Index];
  Str(W, Temp);
  if W = 11520 then Temp := '115200';

  WriteAT(X, Y, mnuNormColor, MakeLen(Temp, 6, space, false, false));
end; { proc. EditBaudRate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditModemSpeed(var L: Longint; X,Y: Byte);
var Index  : Byte;
    Counter: Byte;
    Temp   : String;
begin
  Index := MaxBaudRates + 1;

  for Counter := 01 to MaxBaudRates do
    if BaudRates[Counter] = L then
      begin
        Index := Counter;
        BREAK;
      end; { if }

  Inc(Index);
  if Index > (maxBaudRates + 1) then
    Index := 01;
  if L = 11520 then L := 115200;

  if Index <= (maxBaudRates) then
    L := BaudRates[Index]
      else begin
             EditLongint(L, X, Y, mnuEditColor, 6, true);
           end; { else }

  Str(L, Temp);
  if L = 11520 then Temp := '115200';

  WriteAT(X, Y, mnuNormColor, MakeLen(Temp, 6, space, false, false));
end; { proc. EditModemSpeed }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditTimeWord(var Time: SmallWord; X, Y: Byte; EscAbort: Boolean);
var PartOne  : String[03];
    PartTwo  : String[03];
    TimeStr  : String;
    Hours    : Word;
    Mins     : Word;
begin
  TimeStr := Word2Time(Time);

  PartOne := Copy(TimeStr, 1, 2);
  PartTwo := Copy(TimeStr, 4, 2);

  repeat
    GetString(X, Y, mnuEditColor, PartOne, ['0'..'9'], [#13, #27], 2, 2, False, False, EscAbort, false, 0);
    If FVal(PartOne)>23 then Write(^G);
  until (FVal(PartOne) < 25);

  WriteAT(X, Y, mnuNormColor, MakeLen(PartOne, 2, space, false, false));

  repeat
    GetString(X+3, Y, mnuEditColor, PartTwo, ['0'..'9'], [#13, #27], 2, 2, False, False, EscAbort, false, 0);
    If FVal(PartTwo)>59 then Write(^G);
  until (FVal(PartTwo) < 61);

  TimeStr := MakeLen(PartOne, 2, space, false, false) + ':' +
             MakeLen(PartTwo, 2, space, false, false);

  Time := 00;
  Hours:= FVal(PartOne);
  Mins := FVal(PartTwo);

  While Hours>00 do
    begin
      Inc(Time, 60);
      Dec(Hours);
    end; { While }
  Inc(Time, Mins);

  WriteAT(X, Y, mnuNormColor, Word2Time(Time));
end; { EditTime }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditBitArray32(Title: String; var BitArray: ByteArray32);

procedure RedrawScreen;
var XCounter    : Integer;
    YCounter    : Integer;
    AddChar     : Char;
    ShowNumber  : Integer;
    SaveDirect  : Boolean;
begin
  SaveDirect := DirectScrnUpdate;
  DirectscrnUpdate := false;

  for XCounter := 01 to 15 do
   for YCounter := 00 to 17 do
     begin
       ShowNumber := ((18 * (XCounter-1)) + YCounter);
       If ShowNumber > 255 then break;

       AddChar := #32;
       If Check32(BitArray, ShowNumber) then AddChar := #254;

       WriteAT(03 + ((XCounter-1) * 5), 4 + YCounter, mnuNormColor, Addchar + FStr(ShowNumber));
     end; { for }

  DirectScrnUpdate := SaveDirect;
end; { proc. RedrawScreen }

var XCounter    : Integer;
    YCounter    : Integer;
    AddChar     : Char;
    CH          : Char;
    ShowNumber  : Word;
    SaveBitArray: ByteArray32;

    OldRow,
    OldCol      : Integer;

    RowCounter  : Integer;
    ColCounter  : Integer;
    Row         : Word;
    Number      : Word;
    SaveScrn    : Pointer;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logSecurity, 'Editbitarray32 (begin)');
  {$ENDIF}
  SaveScreen(SaveScrn);

  SaveBitArray := BitArray;

  ShadFillBoxTitle(02, 03, 79, 22, mnuBoxColor, mnuStyle, True, Title);
  WriteCenter(22, mnuTitleColor, ' (SPACE) tag, (ALT-U/T) un/tag all, (ENTER) or (ESCAPE) to continue ');

  RowCounter := 00;
  ColCounter := 00;

  OldCol := 00;
  OldRow := 00;

  RedrawScreen;

  repeat
    ShowNumber := Word((18 * OldCol) + OldRow);
    WriteAT(03 + (OldCol) * 5, 04 + OldRow, mnuNormColor, AddChar + FStr(ShowNumber));

    If ColCounter < 00 then begin
                              ColCounter := 13;

                              If RowCounter=00 then
                                  ColCounter := 14;

                              Dec(RowCounter);
                              If RowCounter < 4 then ColCounter := 14;
                            end; { if }
    If RowCounter < 00 then begin
                              RowCounter := 17;
                              Dec(ColCounter);

                              if ColCounter < 00 then
                                begin
                                  ColCounter := 14;
                                  RowCounter := 03;
                                end; { colcounter }

                            end; { if }
    If (ColCounter >= 14) then
       If (RowCounter >= 4) then begin
                                   ColCounter := 00;
                                   Inc(RowCounter);

                                   if Ord(rdLastkey)=80 then
                                    RowCounter := 00;

                                   If RowCounter = 18 then
                                     begin;
                                        RowCounter := 00;
                                        ColCounter := 00;
                                      end; { last column }
                                 end; { if }
    If (ColCounter >= 15) then begin
                                 ColCounter := 00;
                                 Inc(RowCounter);
                               end; { if }
    If RowCounter >= 18 then begin
                               Inc(ColCounter);
                               RowCounter := 00;
                             end; { if }

    ShowNumber := ((18 * ColCounter) + RowCounter);
    AddChar := #32;
    If Check32(BitArray, ShowNumber) then AddChar := #254;
    WriteAT(03 + (ColCounter) * 5, 04 + RowCounter, mnuTopHiColor, AddChar + FStr(ShowNumber));

    CH := ReadKey;

    OldRow := RowCounter;
    OldCol := ColCounter;

    Case CH of
       #32 : begin

               Number := ShowNumber;
               Row := 01;
               While Number>07 do
                begin
                  Inc(Row);
                  Dec(Number, 8);
                end; { While }

                If Check32(BitArray, ShowNumber) then
                    ClearBit(BitArray[Row], Number)
                      else SetBit(BitArray[Row], Number);

                If Check32(BitArray, ShowNumber) then AddChar := #254
                  else AddChar := #32;

               Inc(RowCounter);

               if RowCounter = 4 then
                if ColCounter = 14 then
                  begin
                    RowCounter := 0;
                    ColCounter := 0;
                  end; { if }

             end; { toggle (space) }
    End; { case }

    If Ch=#00 then
      begin
        CH := ReadKey;

        Case Ord(CH) of
          72 : Dec(RowCounter);
          80 : Inc(RowCounter);

          75 : Dec(ColCounter);
          77 : Inc(ColCounter);

          71 : begin
                          ColCounter := 00;
                          RowCounter := 00;
                        end;
          79  : begin
                          RowCounter := 03;
                          ColCounter := 14;
                        end;

          20 : begin
                          For XCounter := 1 to 32 do
                           For YCounter := 00 to 07 do
                            SetBit(BitArray[XCounter], YCounter);

                          RedrawScreen;
                        end; { Alt-T }

          22 : begin
                          For XCounter := 1 to 32 do
                           For YCounter := 00 to 07 do
                            ClearBit(BitArray[XCounter], YCounter);

                          RedrawScreen;
                        end; { Alt-U }

        end; { case }

      end; { extended key pressed }

  until rdLastKey in [#27, #13];

  if rdLastkey = #27 then BitArray := SaveBitArray;

  RestoreScreen(SaveScrn);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logSecurity, 'Editbitarray32 ( end )');
  {$ENDIF}
end; { proc. EditBitArray32 }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditAddress(X,Y: Byte; var NetAddr: NetAddress; EscAbort: Boolean);
var TempStr: ShortString;
begin
  With NetAddr do
    AddrToString(TempStr, Zone, Net, Node, Point);

  GetString(X, Y, mnuEditColor, TempStr, [#32..#255], [#13, #27], 20, 20, False, False, EscAbort, false, 0);

  With NetAddr do
    FidoAddr.StringToAddr(TempStr, Zone, Net, Node, Point);
end; { proc. EditAddress }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditAskType(X, Y: Byte; var AskEdit: Byte; doYes, doNo, doAsk, doOnly: Boolean);
var B: Byte;
begin
  B := Byte(AskEdit);

  Inc(B);

  If (B=Byte(Yes)) AND (NOT doYes) then Inc(B);
  If (B=Byte(No)) AND (NOT doNo) then Inc(B);
  If (B=Byte(Ask)) AND (NOT doAsk) then Inc(B);
  If (B=Byte(Only)) AND (NOT doOnly) then Inc(B);

  if B>3 then B := 00;

  Byte(AskEdit) := B;

  WriteAT(X, Y, mnuNormColor, Ask2Str(AskEdit));

  case Byte(AskEdit) of
     Byte(Yes)  : WriteAT(X, Y, mnuNormColor, 'Yes ');
     Byte(No)   : WriteAT(X, Y, mnuNormColor, 'No  ');
     Byte(Ask)  : WriteAT(X, Y, mnuNormColor, 'Ask ');
     Byte(Only) : WriteAT(X, Y, mnuNormColor, 'Only');
  end; { case }
end; { proc. EditAskType }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditTime(var TimeStr: CfgRec.Time; X, Y: Byte; EscAbort: Boolean);
var PartOne  : String[03];
    PartTwo  : String[03];
    TempBool : Boolean;
begin
  PartOne := Copy(TimeStr, 1, 2);
  PartTwo := Copy(TimeStr, 4, 2);

  repeat;
    GetString(X, Y, mnuEditColor, PartOne, ['0'..'9'], [#13, #27], 2, 2, False, False, EscAbort, false, 0);

    If FVal(PartOne)>23 then Write(^G);

    WriteAT(X, Y, mnuNormColor, MakeLen(PartOne, 2, space, false, false) + ':');
  until (FVal(PartOne) < 24);

  WriteAT(X, Y, mnuNormColor, MakeLen(PartOne, 2, space, false, false));

  repeat;
    TempBool := True;
    GetString(X+3, Y, mnuEditColor, PartTwo, ['0'..'9'], [#13, #27], 2, 2, False, False, EscAbort, false, 0);
    if FVal(PartTwo)>59 then TempBool := False;
    if FVal(PartOne)=24 then
     if FVal(PartTwo)>00 then TempBool := False;

    if NOT TempBool then Write(^g);
    WriteAT(X+3, Y, mnuNormColor, PartTwo);
  until (TempBool);
  TimeStr := MakeLen(PartOne, 2, space, false, false) + ':' +
             MakeLen(PartTwo, 2, space, false, false);

  WriteAT(X, Y, mnuNormColor, TimeStr);
end; { EditTime }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure GetString(X,Y,Attr:Byte; Var S:String; Chars, Abort:CharSet; Len, ShowLen:Byte; AllUpper,ColorMap,EscAbort,IsPsw:
          Boolean; DefColor: Byte);
Const JumpyEdit : Boolean = True;

Var TempOld    : String;
    TempNew    : String;
    CH         : Char;
    CH2        : Char; { To keep the routine from aborting when CTRL-[ is pressed }
    Counter    : Byte;
    TempStr    : String;
    IsFirstChar: Boolean;
    InsertMode : Boolean;
    OkToAbort  : Boolean;
    LastChar   : Char;


    TempFore,
    TempBack   : Byte;

    CursorPos    : Byte;            { X + CursorPos = Current cursor location }
    FirstCharPos : Byte;                          { First character displayed }

Procedure AddChar(CH: Char; CheckValid: Boolean);
begin
  if Len=01 then
   if (CH in Chars) OR (NOT CheckValid) then
      begin
        TempNew := Ch;
        exit;
      end; { if }

  If (CH in Chars) OR (NOT CheckValid) then
    begin
      if IsFirstChar then
       if JumpyEdit then
         begin
           FirstCharPos := 01;
           CursorPos := 00;
           TempNew := '';
         end; { if }

      If (InsertMode) AND (Len>Length(TempNew)) then
       begin;
        Insert(CH, TempNew, FirstCharPos + CursorPos);
        If CursorPos<Pred(ShowLen) then Inc(CursorPos)
          else Inc(FirstCharPos);
       End; { if InsertMode etc }

(**
     writeat(1,1,15, 'FirstCharPos/CursorPos/Len(TempNew)=  '+FStr(FirstCharPos)+ ' / '+FStr(CUrsorPos)+' / '+
     FStr(Length(TempNew)));
**)

     If NOT InsertMode then
      If ((FirstCharPos+CursorPos)-1)<Length(TempNew) then
         begin;
           TempNew[FirstCharPos+CursorPos] := Ch;
           If CursorPos<Pred(ShowLen) then Inc(CursorPos)
             else Inc(FirstCharPos);
         end else
                begin
                   If Length(TempNew)<Len then TempNew := TempNew + CH;
                   If CursorPos<Pred(ShowLen) then Inc(CursorPos);
{ was:                      else Inc(FirstCharPos);}
                end; { else }

    End; { If CH In Chars }

 IsFirstChar := False;
end; { proc. AddChar }

Function GiveCharColor(C: Char): Byte;
begin
  If C in [#32..#255] then GiveCharColor := Attr { mnuEditColor }
   else GiveCharColor := mnuHiLightColor;
end; { func. GiveCharColor }

Function GiveNeatChar(C: Char): Char;
begin
  If C in [#32..#255] then GiveNeatChar := C
   else GiveNeatChar := Chr( Byte(Ord(UpCase(C)) + 64));

  if IsPsw then GiveNeatChar := #07;
end; { func. GiveNeatChar }

procedure ScrollToEnd;
begin
 If Length(TempNew)>=ShowLen then
    begin;
      FirstcharPos := (Length(TempNew) - ShowLen) + 01;
      CursorPos := ShowLen + 01;
    end
     else begin;
            FirstCharPos := 01;
            CursorPos := Length(TempNew);
            if FirstCharPos+CursorPos > ShowLen then
              CursorPos := ShowLen + 01;
          end; { Fits on screen }
end; { proc. ScrollToEnd }

function FirstChar(S: String): Char;
begin
  FirstChar := S[1];
end; { func. FirstChar }

function  ToHex (Dec : Byte) : String;    { Conv decimal byte to 2 digit hex }
const HexChars : Array[0..15] of Char = '0123456789ABCDEF';
begin
  ToHex[0] := #2;
  ToHex[1] := HexChars[Dec SHR  4];
  ToHex[2] := HexChars[Dec AND 15];
end; { func. ToHex }

var Menu       : PullRecord;
    SaveDirect : Boolean;
    MacroInpStr: String;
    InputCntr  : Byte;
Begin;
{$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logEditor, 'Editor.....S='+S);
{$ENDIF}
 TempOld := S;
 TempNew := S;
 InsertMode := True;
 CursorOn;

 ScrollToEnd;


 If ColorMap then WriteRight(79, mnuMsgYPos, mnuMsgColor, 'F1 - Colors  F2 - Sample  F3 - Macro''s');
 IsFirstchar := True;

 CH := #01;
 LastChar := #01;

 Repeat;
  if Len=01 then InsertMode := True;

  If InsertMode then CursorHalf else
    CursorOn;

  If CursorPos>Length(TempNew) then CursorPos := Length(TempNew);
  If CursorPos>ShowLen then CursorPos := ShowLen;

  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;

  if ShowLen>=80 then
    WriteAT(X, Y, Attr, Dup(mnuBlockChar, ShowLen + 01))
     else WriteAT(X, Y, Attr, Dup(mnuBlockChar, ShowLen));

  if Len>ShowLen then TempStr := Copy(TempNew, FirstCharPos, ShowLen)
   else TempStr := TempNew;

  if Length(TempNew)>00 then
   For Counter:=01 to Length(TempStr) do
     begin
       WriteAT(X+(Counter-1), Y, GiveCharColor(TempStr[Counter]), GiveNeatChar(TempStr[Counter]));
     end;

  if (Length(TempNew)=00) AND (Len=01) then
   begin;
     TempNew[01] := #32;
     TempNew[00] := #01;
   end; { if }

  if Len=01 then
    WriteAT(X,Y, GiveCharColor(TempNew[01]), TempNew[01]);

  GotoXY(Min(X + CursorPos, mnuScrnWidth), Y);
  DirectScrnUpdate := SaveDirect;
  UpdateScreenBuffer(true);

  While NOT Keypressed do
     DoSlice;

  LastChar := CH;
  CH := ReadKey;
  If AllUpper then Ch := UpCase(CH);

  AddChar(CH, True);

  Case CH of
    { BckSpc }  #08 : If Length(TempNew)>0 then
                       If (FirstCharPos + CursorPos) > 01 then
                        begin;
                          Delete(TempNew, FirstCharPos + (Max(Cursorpos - 01, 0)), 1);

                          If FirstCharPos > 01 then Dec(FirstCharPos)
                           else Dec(Cursorpos);
                        end; { Delete }
    { CTRL-O }  #15,
    { CTRL-P }  #16 : begin
                        CursorBlock;

                        CH2:= ReadKey;                  { Read Extended key }
                        AddChar(CH2, False);

                        CursorOn;
                      end; { CTRL-P }
    { CTRL-Y }  #25 : Begin;
                        TempNew := '';
                        FirstCharPos := 01;
                        CursorPos := 01;
                      End; { Ctrl-Y }
    { #00 }     #00 : begin;
                       CH := ReadKey;
                       Case CH of
                        { F-1 }    #59 : If ColorMap then
                                            begin
                                              Edit_Color(00, TempFore, TempBack, Menu, False);

                                              if rdLastkey=#13 then
                                               begin
                                                 AddChar(^K, false);
                                                 AddChar('[', false);
                                                 AddChar(FirstChar(ToHex((TempBack*16+TempFore))), true);
                                                 AddChar(FirstChar(Copy(ToHex((TempBack*16+TempFore)), 2, 1)), true);
                                               end; { if }
                                            end; { if }
                        { F2 }     #60 : If ColorMap then Color_Example(TempNew, DefColor);
                {$IFNDEF WITH_FULL}
                        { F3 }     #61 : if ColorMap then
                                           begin
                                             Macro_List(MacroInpStr);
                                             if rdLastKey = #13 then
                                               for InputCntr := 01 to Length(MacroInpStr) do
                                                 AddChar(MacroInpStr[InputCntr], false);
                                           end; { if }
                {$ENDIF}
                        { Insert } #82 : If InsertMode then InsertMode := False
                                            else InsertMode := True;
                        { Left }   #75 : begin;
                                          If (FirstCharPos>01) then
                                           If CursorPos=00 then Dec(FirstCharPos);
                                          If CursorPos>00 then Dec(CursorPos);
                                         end; { Left }
                        { Right }  #77 : begin;
                                          If (CursorPos >= Pred(ShowLen)) then
                                           If FirstCharPos < ((Length(TempNew) - ShowLen) + 01) then Inc(FirstCharPos);

                                          If (CursorPos<>ShowLen) then Inc(CursorPos);
                                         end; { Right }
                        { Home }   #71 : begin;
                                           FirstCharPos := 01;
                                           CursorPos := 00;
                                         end; { Home }
                        { End }    #79 : ScrollToEnd;
                        { Delete } #83 : If (CursorPos+FirstCharPos)-1>00 then Delete(TempNew, CursorPos+FirstCharPos, 1)
                                          else Delete(TempNew, 1, 1);
                       end; { Case }

                       LastChar := #00;
                      End; { #00 }
   end; { Case CH }

   OkToAbort := true;
   if CH in Chars then
    if Ch in Abort then
     if LastChar <> #00 then
       OkToAbort := false;

 Until (CH in Abort) AND (OkToAbort);

 If (Ch=#27) AND (EscAbort) then S := TempOld
  else S := TempNew;
 CleanString(S);

 If AllUpper then S := SUpcase(S);

 WriteAT(X, Y, mnuNormColor, Dup(#32, ShowLen)); { was:  ShowLen + 01 }

 CursorOff;
end; { proc. GetString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


begin
  {$IFNDEF FPC}
    KeyProc := Empty;
    EnterProc := Empty;
    HelpProc := EmptyID;
    KeyHookProc := DefaultKeyhookProc;
    KeyFuncPressed := DefaultKeyPressed;
    IdleProc := DefIdleProc;
  {$ELSE}
    KeyProc := @Empty;
    EnterProc := @Empty;
    KeyHookProc := @DefaultKeyhookProc;
    HelpProc := @EmptyID;
    KeyFuncPressed := @DefaultKeyPressed;
    IdleProc := @DefIdleProc;
  {$ENDIF}
  rdAlEntered := false;
end. { unit StrEdit }
