unit Editor;
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
(*
**
** Editor Routines for EleBBS
**
** Copyright (c) 1996, 1997 by Maarten Bekers
**
** Note: Written by Konst (Tornado Author)
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses {$IFDEF DELCRT}
       Crt32,
     {$ELSE}
       Crt,
     {$ENDIF}
     Dos, GenFile, ScrnU, StrEdit, Multi, GenCfg, CentrStr
        {$IFDEF OS2}
          ,Os2Base
        {$ENDIF};

{$I KEYS.INC}

Const TabSize        = 05;        { Number of characters to insert with TAB }

      NumOfStrings   = 500;                     { Maximum number of strings }

      WindowColor    : Byte = Cyan;
      NormColor      : Byte = Lightgray;

type  Editor_TxtRec  = Array[1..NumOfStrings] of String[80];


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

var Txt             : ^Editor_TxtRec;                        { The text ;-) }
    LastString      : Longint;                        { Last of the strings }
    MaxStrLen       : Longint;               { Maximum length of one string }
    Editor_Modified : Boolean;                      { Is the text modified? }


function Editor_InitEditor(X1, Y1, X2, Y2: Longint;
                    ShowWindow    : Boolean): Boolean;

procedure Editor_Edit (var bcx, bcy: Longint);
procedure Editor_DoneEditor (var ex, ey: Longint);
procedure Editor_Redraw;
function Editor_ShowStatus: Boolean;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Const WordDelims : Set Of Char = [' ', ',', '.', '!', '?'];


var CurPosX         : Longint;      { Current cursor position on the screen }
    CurPosY         : Longint;

    GlobalY         : Longint;    {?????????????????????????????????????????}

    StringsPerWindow: Longint;   { Number of strings that fit on the screen }


    WindowX1,                                          { Window coordinates }
    WindowY1,
    WindowX2,
    WindowY2        : Longint;

    InsFlag         : Boolean;                                     { Insert }

    InsChrRF        : Boolean;     { Set to false to prevent from redrawing }
    DelChrRF        : Boolean;     { Set to false to prevent from redrawing }
    CurrInsChar     : Boolean;                      { Current in InsertChar }

    LastPressed     : Char;                              { Last key pressed }
    LastSearch      : String;                      { Last text searched for }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoEnter; forward;
procedure DelChar (x, y: Longint); forward;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SetCurShape;
begin
  if InsFlag then CursorOn
    else CursorHalf;
end; { proc. SetCurShape }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SetCursorPos;
begin
  GotoXY(CurPosX + WindowX1 - 01,
         CurPosY - GlobalY + WindowY1);
end; { proc. SetCursorPos }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure WriteCoords;
begin
  SetCursorPos;
  SetCurShape;
end; { proc. Writecoords }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Editor_Redraw;
var Range     : Longint;
    Counter   : Longint;
    SaveDirect: Boolean;
begin
  if (NOT DelChrRF) OR (NOT InsChrRF) then EXIT;
  if KeyPressed then EXIT;

  CursorOff;

  if GlobalY + StringsPerWindow <= LastString then
    Range := StringsPerWindow
     else Range := LastString - GlobalY;

  SaveDirect := DirectScrnUpdate;
  DirectscrnUpdate := false;

  for Counter := 00 to Range do
    begin
      WriteAT(WindowX1, WindowY1 + Counter,
              NormColor, Txt^[GlobalY + Counter]);

      PartClear(WindowX1 + Length(Txt^[GlobalY + Counter]),
                WindowY1 + Counter,
                WindowX2 {+ (MaxStrLen - Length(Txt^[GlobalY + Counter]))},
                WindowY1 + Counter,
                NormColor,
                #32);
    end; { for }

   if Range < StringsPerWindow then
     PartClear(WindowX1, WindowY1 + Range + 01,
               WindowX2, WindowY2, NormColor, #32);

  WriteCoOrds;
  SetCurShape;

  DirectScrnUpdate := SaveDirect;
  UpdateScreenBuffer(DirectScrnUpdate);
end; { proc. Editor_Redraw }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Editor_InitEditor(X1, Y1, X2, Y2: Longint;
                         ShowWindow    : Boolean): Boolean;
begin
  Editor_InitEditor := true;

  Txt := nil;
  New(Txt);

  if Txt = nil then
   begin
     Editor_InitEditor := false;
     EXIT;
   end; { if TxtRec }

  {------------------------ Initialize all the variables --------------------}
  Editor_Modified  := false;

  WindowX1         := X1;
  WindowY1         := Y1;
  WindowX2         := X2;
  WindowY2         := Y2;

  StringsPerWindow := WindowY2 - WindowY1;
  MaxStrLen        := (WindowX2 - WindowX1);

  LastSearch       := '';

  LastString       := 01;                              { Last string loaded }
  InsFlag          := true;                                       { Insert? }

  CurPosX          := 01;                                 { Cursor position }
  CurPosY          := 01;

  GlobalY          := 01; {?????????????????????????????????????????}

  InsChrRF         := true;                 { Must be set to true to redraw }
  DelChrRF         := true;

  CurrInsChar      := false;             { Currently in "InsChar" procedure }

  FillChar(Txt^, SizeOf(Editor_TxtRec), #00);
  if ShowWindow then
    ShadFillBox(WindowX1 - 01, WindowY1 - 01,
                WindowX2 + 01, WindowY2 + 01,
                WindowColor, mnuStyle, true);

  Editor_Redraw;                                              { Show the file }
end; { func. InitEditor }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Editor_DoneEditor(var EX, EY: Longint);
begin
  Dispose(Txt);

  EX := CurPosX;                        { Return cursor position to caller }
  EY := CurPosY;

  CursorOff;
end; { proc. DoneEditor }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure InsChar(Sym: Char);
Label LEND;

var CurLen    : Longint;                     { Length of the current string }
    OldX,
    OldY      : Longint;
    LinePos   : Longint;
    TempStr   : String;

    BackX,
    BackY     : Longint;

    CharCount : Longint;
    Counter   : Longint;
begin
  Editor_Modified := true;                               { Text is modified }

  OldX   := CurPosX;
  OldY   := CurPosY;
  CurLen := Length(Txt^[OldY]);

  { Check to see if we're somewhere in the string (not at the end) }
  if OldX < CurLen + 01 then
    begin
      if NOT InsFlag then       { In overwrite modus, just replace the char }
        begin
          Txt^[OldY][OldX] := Sym;                       { Replace the char }
          Inc(OldX);

          Goto LEND;
        end; { if }

      { Check to make sure the string fits, else put it on the next line }
      Counter := OldY;
      While (Counter < NumOfStrings) AND
             (Length(Txt^[Counter]) = MaxStrLen) do Inc(Counter);
      if Counter > LastString then Inc(LastString);

      { Add the string, replace the 1st character if it's the last line }
      While Counter > OldY do
        begin
          if (Counter = NumOfStrings) AND
              (Length(Txt^[Counter]) = MaxStrLen) then
               Delete(Txt^[Counter], MaxStrLen, 1);

          Insert(Txt^[Counter - 01][MaxStrLen], Txt^[Counter], 01);
          Delete(Txt^[Counter - 01], MaxStrLen, 1);
          Dec(Counter);
        end; { while }

      { If we're on the last row and the string is full, replace the char }
      { else insert it at the current position }
      if (OldY = NumOfStrings) AND (Length(Txt^[OldY]) = MaxStrLen) then
        Txt^[OldY][OldX] := Sym
         else Insert(Sym, Txt^[OldY], OldX);

      { Check if we're at the end of the string, if so, wrap to next line }
      if OldX < MaxStrLen then
        Inc(OldX)
         else begin
                OldX := 01;
                if OldY < LastString then Inc(OldY);
              end; { else }

      Goto LEND;
    end; { if }

  { Check to see if we're at the end, but the string is not yet fully used }
  if (OldX = CurLen + 01) AND (CurLen < MaxStrLen) then
    begin
      Txt^[OldY] := Txt^[OldY] + Sym;
      Inc(OldX);

      Goto LEND;
    end; { if }

  { Check to see if we're at the end of the string }
  if (OldX = CurLen + 01) AND (CurLen = MaxStrLen) then
    begin
      if NOT InsFlag then
        begin
          if (LastString < NumOfStrings) AND (OldY = LastString) then
            Inc(LastString);

          if OldY < LastString then Inc(OldY);
          OldX := 01;

          Delete(Txt^[OldY], OldX, 01);     { Insert character at first pos }
          Insert(Sym, Txt^[OldY], 01);

          Inc(OldX);
          Goto LEND;
        end; { if NOT InsertFlag }

      { Seek a line where we can add this character ;-) }
      Counter := OldY;
      While (Counter < NumOfStrings) AND
             (Length(Txt^[Counter]) = MaxStrLen) do
              Inc(Counter);
      if Counter > LastString then Inc(LastString);

      if OldY < LastString then Inc(OldY);

      While Counter > OldY do
        begin
          if (Counter = NumOfStrings) AND
              (Length(Txt^[Counter]) = MaxStrLen) then
               Delete(Txt^[Counter], MaxStrLen, 01);

          Insert(Txt^[Counter - 01][MaxStrLen], Txt^[Counter], 01);
          Delete(Txt^[Counter - 01], MaxStrLen, 01);
          Dec(Counter);
        end; { while }

      OldX := 01;
      if OldY = NumOfStrings then Txt^[OldY][OldX] := Sym
          else Insert(Sym, Txt^[OldY], OldX);

      if OldX < MaxStrLen then Inc(OldX);
      Goto LEND;
    end; { if }

LEND:
  {-------------------- Start moving the words ------------------------------}
  if (CurLen + 01 = MaxStrLen) AND (NOT CurrInsChar) then
   begin
     TempStr := '';
     LinePos := MaxStrLen;

     BackX := CurPosX;
     BackY := CurPosY;
     CharCount := 00;

     { Gather the word that need to be wrapped }
     REPEAT
       TempStr := TempStr + Txt^[CurPosY][LinePos];
       Dec(LinePos);
     UNTIL (Txt^[CurPosY][LinePos] in WordDelims) OR (LinePos = 01);

     if Length(TempStr)+1 < Length(Txt^[CurPosY]) then
      if LastString < NumOfStrings then
       begin
        { If the cursor was behind the wrapped word, move down the cursor }
        if CurPosX >= LinePos then
         begin
           OldX := CurPosX - LinePos + 01;
           OldY := CurPosY + 01;
         end; { if }

        CurPosX := LinePos;
        DelChrRF := false;

        { Remove the characters }
        for Counter := LinePos to MaxStrLen do
          DelChar(CurPosX, CurPosY);

        DoEnter;     { Adds an enter to the string (including needed things) }
        DelChrRF := true;
        CharCount := Length(TempStr);
        CurrInsChar := true;
        InsChrRF := false;

        for Counter := CharCount downto 01 do
          InsChar(TempStr[Counter]);

        InsChrRF := true;
        CurrInsChar := false;
       end; { if the string could be wrapped }
   end; { if }
  {-------------------- The end of moving the words--------------------------}

   CurPosX := OldX;
   CurPosY := OldY;

   if CurPosY - StringsPerWindow > GlobalY then Inc(GlobalY);
   Editor_Redraw;
end; { proc. InsChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GoUp;
begin
  if CurPosY = 01 then EXIT;

  { Check to see if we're at the top of the window }
  if CurPosY = GlobalY then
    begin
      Dec(GlobalY);
      Dec(CurPosY);

      If CurPosX > Length(Txt^[CurPosY]) + 01 then
        CurPosX := Length(Txt^[CurPosY]) + 01;

      Editor_Redraw;
      EXIT;
    end; { if }

  Dec(CurPosY);
  if CurPosX > Length(Txt^[CurPosY]) + 01 then
    CurPosX := Length(Txt^[CurPosY]) + 01;

  SetCursorPos;
  WriteCoords;
end; { proc. GoUp }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GoDn;
begin
  if CurPosY = NumOfStrings then EXIT;
  if CurPosY + 01 > LastString then EXIT;

  if CurPosY = GlobalY + StringsPerWindow then
    begin
      Inc(GlobalY);
      Inc(CurPosY);

      if CurPosX > Length(Txt^[CurPosY]) + 01 then
        CurPosX := Length(Txt^[CurPosY]) + 01;

      Editor_Redraw;
      EXIT;
    end; { if we're at the end of the screen }

  Inc(CurPosY);
  if CurPosX > Length(Txt^[CurPosY]) + 01 then
    CurPosX := Length(Txt^[CurPosY]) + 01;

  WriteCoords;
end; { proc. GoDn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GoWordLeft;
var LinePos : Byte;
begin
  LinePos := CurPosX;

  if LinePos > 02 then
    begin
      Dec(LinePos);

      While (Txt^[CurPosY] [LinePos] in WordDelims) AND (Linepos > 01) do
        Dec(LinePos);

      While (NOT (Txt^[CurposY][LinePos] in WordDelims)) AND (LinePos > 01) do
        Dec(LinePos);

      if LinePos > 01 then Inc(LinePos);
    end
      else if (LinePos=01) AND (CurPosY > 1) then
             begin
               Dec(CurPosY);
               CurPosX := Length(Txt^[CurPosY]) + 01;

               if CurPosY < GlobalY then Dec(GlobalY);
               Editor_Redraw;
               EXIT;
             end; { if }

  CurPosX := LinePos;
  Editor_Redraw;
end; { proc. GoWordLeft }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GoWordRight;
var LinePos: Byte;
begin
  LinePos := CurPosX;

  if LinePos < Length(Txt^[CurPosY]) + 01 then
    begin
      Inc(LinePos);

      While (NOT (Txt^[CUrPosY][LinePos] in WordDelims)) AND
             (LinePos <= Length(Txt^[CurPosY])) do
              Inc(LinePos);

      While (Txt^[CurPosY][LinePos] in WordDelims) AND
             (LinePos <= Length(Txt^[CurPosY])) do
              Inc(LinePos);

    end
      else if (LinePos = Length(Txt^[CurPosY]) + 01) AND (CurPosY < NumOfStrings) then
             begin
                if CurPosY < LastString then
                  begin
                    Inc(CurPosY);
                    CurPosX := 01;

                    if CurPosY > GlobalY + StringsPerWindow then
                      Inc(GlobalY);
                    Editor_ReDraw;
                  end; { if }
                EXIT;
             end; { if }

  CurPosX := LinePos;
  Editor_ReDraw;
end; { proc. GoWordRight }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GoLeft;
begin
  If CurPosX = 01 then EXIT;

  Dec(CurPosX);
  WriteCoords;
end; { proc. GoLeft }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GoRight;
begin
  if CurPosX = Length(Txt^[CurPosY]) + 01 then EXIT;

  Inc(CurPosX);
  WriteCoords;
end; { proc. GoRight }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GoEOL;
begin
  CurPosX := Length(Txt^[CurPosY]) + 01;
  WriteCoords;
end; { proc. GoEol }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GoBOL;
begin
  CurPosX := 01;
  WriteCoords;
end; { proc. GoBOL }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GoPgUp;
begin
  if CurPosY - StringsPerWindow < 01 then
    begin
      if GlobalY - StringsPerWindow < 01 then GlobalY := 01
        else Dec(GlobalY, StringsPerWindow);

      CurPosY := 01;
      if CurPosX > Length(Txt^[CurPosY]) + 01 then
          CurPosX := Length(Txt^[CurposY]) + 01;

      Editor_Redraw;
      EXIT;
    end
      else begin
             if GlobalY - StringsPerWindow < 01 then GlobalY := 01
               else Dec(GlobalY, StringsPerWindow);

             Dec(CurPosY, StringsPerWindow);
             if CurPosX > Length(Txt^[CurPosY]) + 01 then
                CurPosX := Length(Txt^[CurPosY]) + 01;

             Editor_Redraw;
             EXIT;
           end; { else }
end; { proc. GoPgUp }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GoPgDn;
begin
  if CurPosY + 02 * StringsPerWindow > LastString then
    begin
      CurPosY := LastString;

      if CurPosY - StringsPerWindow > 00 then
        GlobalY := CurPosY - StringsPerWindow
         else GlobalY := 01;

      if CurPosX > Length(Txt^[CurPosY]) + 01 then
        CurPosX := Length(Txt^[CurPosY]) + 01;

      Editor_Redraw;
      EXIT;
    end; { if }

  Inc(CurPosY, StringsPerWindow);
  Inc(GlobalY, StringsPerWindow);

  if CurPosX > Length(Txt^[CurPosY]) + 01 then
    CurPosX := Length(Txt^[CurPosY]) + 01;

  Editor_Redraw;
  EXIT;
end; { proc. GoPgDn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ToggleIns;
begin
  InsFlag:= NOT InsFlag;
  SetCurShape;
end; { proc. ToggleIns }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DelChar (X, Y: Longint);

  function WordPosition(const N : Longint; const S : String; const WordDelims : CharSet) : Longint;
    {-Given a set of word delimiters, return start position of N'th word in S}
  var
    Count : Longint;
    I     : Longint;

  begin
    Count := 0;
    I := 1;
    WordPosition := 0;

    while (I <= Length(S)) and (Count <> N) do begin
      {skip over delimiters}
      while (I <= Length(S)) and (S[I] in WordDelims) do
        Inc(I);

      {if we're not beyond end of S, we're at the start of a word}
      if I <= Length(S) then
        Inc(Count);

      {if not finished, find the end of the current word}
      if Count <> N then
        while (I <= Length(S)) and not (S[I] in WordDelims) do
          Inc(I)
      else
        WordPosition := I;
    end;
  end;

function ExtractWord(const N : Longint; const S : String; const WordDelims : CharSet) : String;
{-Given a set of word delimiters, return the N'th word in S}
var I     : Word;
    Len   : Byte;
{$IFNDEF VirtualPascal}
 {$IFNDEF FPC}
  {$IFNDEF DELPHI}
    Result: String;
  {$ENDIF}
 {$ENDIF}
{$ENDIF}
begin
  Len := 0;
  I := WordPosition(N, S, WordDelims);
    if I <> 0 then
      {find the end of the current word}
      while (I <= Length(S)) and not(S[I] in WordDelims) do begin
        {add the I'th character to result}
        Inc(Len);
        {$IFDEF Win32}
        {$IFOPT H+}
        SetLength(Result, Len);
        {$ENDIF}
        {$ENDIF}
        Result[Len] := S[I];
        Inc(I);
      end;
    {$IFDEF Win32}
    {$IFOPT H+}
    SetLength(Result, Len);
    {$ELSE}
    Result[0] := Char(Len);
    {$ENDIF}
    {$ELSE}
    Result[0] := Char(Len);
    {$ENDIF}

    ExtractWord := Result;
  end;

var Counter    : Longint;
    LeftLength : Longint;
    TempBuf    : String;
    AddStr,
    AddStr1    : String;

begin
  { Check if we are deleting a char somewhere in the char (not at the end) }
  if X < Length(Txt^[Y]) + 01 then
    begin
      Counter := Y;
      Delete(Txt^[Y], X, 01);
      Editor_Redraw;

      Editor_Modified := true;
      EXIT;
    end; { if }

  { We're at the end of the string, and not at the last row }
  if (X = Length(Txt^[Y]) + 01) AND (Y < NumOfStrings) then
    begin
      { When we're at a empty line, move all strings one row up }
      if Txt^[Y] = '' then
        begin
          for Counter := y to LastString do
           if Counter < NumOfStrings then
             begin
               Txt^[Counter] := Txt^[Counter + 01];
               Txt^[Counter + 01] := ''
             end
               else Txt^[Counter] := '';

          if CurPosY <> LastString then Dec(LastString);
          Editor_Redraw;
          Editor_Modified := true;
          EXIT;
        end; { if }

      LeftLength := MaxStrLen - Length(Txt^[Y]);
      TempBuf := Txt^[Y + 01];

      AddStr := Copy (txt^ [y+1], 1, LeftLength);
      AddStr1 := ExtractWord(1, AddStr, WordDelims);

      if ((Length(AddStr1) < Length(ExtractWord(1, Txt^[Y + 01], WordDelims)))
          OR (Length(AddStr1 + Txt^[Y]) >= MaxStrLen)) then EXIT;

      if Length (TempBuf) > LeftLength then
        begin
          Counter := Length(AddStr);
          While NOT (AddStr[Counter] in WordDelims) AND
                      (Counter > 0) do
                       Dec(Counter);

          if Counter <> 00 then AddStr := Copy(AddStr, 1, Counter);
        end; { if }

      Txt^[Y] := TrimRight(Txt^[Y] + AddStr);
      Delete(Txt^[Y + 01], 1, Length(AddStr) + 01);

      if Txt^ [Y + 1] = '' then
        begin
          for Counter := Y + 01 to LastString do
           if Counter < NumOfStrings then
            begin
              Txt^[Counter]      := Txt^[Counter + 01];
              Txt^[Counter + 01] := ''
           end else Txt^[Counter]:='';

          if CurPosY <> LastString then Dec(LastString);
        end; { if }

      Editor_Redraw;
    end;

  Editor_Modified := true;
end; { proc. DelChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure BackSpace(X, Y: Longint);
var Counter   : Longint;
    LeftLength: Longint;
begin
  if X > 01 then
    begin
      Dec(X);
      Delete(Txt^[Y], X, 01);
      CurPosX := X;

      Editor_Redraw;
      Editor_Modified := true;
      EXIT;
    end; { if }

  if X = 01 then
   begin
     if Y > 01 then
       begin
         LeftLength := MaxStrLen - Length(Txt^[Y - 01]);
         X := Length(Txt^[Y - 01]) + 01;

         Txt^[Y - 01] := Txt^[Y - 01] + Copy(Txt^[Y], 1, LeftLength);
         Delete(Txt^[Y], 1, LeftLength);

         if Length(Txt^[Y]) + 01 = 1 then
           begin
             for Counter := Y to LastString do
               begin
                 if Counter < NumOfStrings then
                   begin
                     Txt^[Counter] := Txt^[Counter + 01];
                     Txt^[Counter + 01] := '';
                   end
                     else Txt^[Counter] := '';
               end; { for }

             Dec(LastString);
           end; { if }

         Dec(y);
       end; { if X > 01 }

     CurPosX := X;
     CurPosY := Y;

     if GlobalY > CurPosY then Dec(GlobalY);
     Editor_Redraw;
   end; { if }

  Editor_Modified := true;
end; { proc. BackSpace }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoEnter;
var rMin,
    rMax    : Longint;
    Counter : Longint;
begin
  rMax := LastString;
  rMin := CurPosY + 01;

  if (rMax >= NumOfStrings) OR (rMin >= NumOfStrings) then EXIT;
  for Counter := rMax downto rMin do
     Txt^[Counter + 01] := Txt^[Counter];

  Txt^[CurPosY + 01] := Copy(Txt^[CurPosY], CurPosX,
                             Length(Txt^[CurPosY]) - CurPosX + 01);

  Delete(Txt^[CurPosY], CurPosX,
         Length(Txt^[CurPosY]) - CurPosX + 01);

  if LastString < NumOfStrings then Inc(LastString);
  if CurPosY < NumOfStrings then Inc(CurPosY);

  CurPosX := 01;
  if GlobalY + StringsPerWindow < CurPosY then Inc(GlobalY);
  Editor_Redraw;
  Editor_Modified := true;
end; { proc. DoEnter }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GoBeginOF;
begin
  GlobalY := 01;
  CurPosY := 01;

  if Length(Txt^[CurposY]) < CurPosX then
    CurPosX := Length(Txt^[CurPosY]) + 01;

  Editor_Redraw;
end; { proc. GoBeginOF }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GoEndOF;
begin
  CurPosY := LastString;

  if LastString - StringsPerWindow < 01 then GlobalY := 01
    else GlobalY := LastString - StringsPerWindow;

  if Length(Txt^[CurPosY]) < CurPosX then
    CurPosX := Length(Txt^[CurPosY]) + 01;

  Editor_Redraw;
end; { proc. GoEndOf }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GoBeginOS;
begin
  CurPosY := GlobalY;
  if Length(Txt^[CurPosY]) < CurPosX then
    CurPosX := Length(Txt^[CurPosY]) + 01;

  WriteCoords;
end; { proc. GoBeginOS }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GoEndOS;
begin
  if GlobalY + StringsPerWindow > LastString then CurPosY := LastString
    else CurPosY := GlobalY + StringsPerWindow;

  if Length(Txt^[CurPosY]) < CurPosX then CurPosX := Length(Txt^[CurposY]) + 01;

  Editor_Redraw;
end; { proc. GoEndOS }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DelLine;
var Counter: LongInt;
begin
  for Counter := CurPosY to LastString do
    begin
      if Counter < NumOfStrings then
        begin
          Txt^[COunter] := Txt^[Counter + 01];
          Txt^[Counter + 01] := '';
        end
          else Txt^[Counter] := '';
    end; { for }

  if CurPosY <> LastString then Dec(LastString);
  CurPosX := 01;
  Editor_Redraw;
  Editor_Modified := true;
end; { proc. DelLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure InsTab;
var Counter : Longint;
    SaveX   : Longint;
begin
  Counter := CurPosX;
  SaveX := CurPosX;

  While (Counter < SaveX + TabSize) AND (Counter < MaxStrLen) do
    begin
      InsChrRF := false;
      InsChar(kSpaceBar);
      Inc(Counter);
    end; { while }

  InsChrRF := true;
end; { proc. InsTab }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Editor_ShowStatus: Boolean;
begin
  WriteCoords;
  Editor_ShowStatus := true;
end; { func. ShowStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Editor_Edit(var bcx, bcy: Longint);
var Key   : Char;
begin
  if Bcy <= LastString then
    CurPosY := BCY
      else CurPosY := LastString;

  if CurPosY < 01 then CurPosY := 01;

  if Length(Txt^[CurPosY]) + 01 >= Bcx then
    CurPosX := Bcx
      else CurPosX := Length(Txt^[CurPosY]) + 01;

  Key := #00;
  WriteCoords;

  repeat
    if KeyPressed then
      begin
        Key := Readkey;

        if Key in [#32..#255] then
          begin
            InsChar(Key);
            Continue;
          end;

        if Key in [kENTER, kBSP, kCtrlY, kTAB, kCtrlP] then
          begin
            Case Key Of
              kENTER : DoEnter;
              kBSP   : BackSpace(CurPosX, CurPosY);
              kCtrlY : DelLine;
              kCtrlP : begin
                         Key := ReadKey;
                         InsChar(Key);
                         Editor_Redraw;
                         Continue;
                       end; { Ctrl-P }
              kTAB   : InsTab;
            end; { case }

            Editor_ReDraw;
            Continue;
          end; { if }

        if Key = #00 then
          begin
            Key := ReadKey;

            Case Key Of
               kUpArrow        : GoUp;
               kDownArrow      : GoDn;
               kLeftArrow      : GoLeft;
               kCtrlLeftArrow  : GoWordLeft;
               kCtrlRightArrow : GoWordRight;
               kRightArrow     : GoRight;
               kEnd            : GoEOL;
               kHome           : GoBOL;
               kPgUp           : GoPgUp;
               kPgDown         : GoPgDn;
               kDel            : DelChar(CurPosX, CurPosY);
               kIns            : ToggleIns;
               kCtrlPgUp       : GoBeginOF;
               kCtrlPgDn       : GoEndOF;
               kCtrlHome       : GoBeginOS;
               kCtrlEnd        : GoEndOS;
            end; { case }
          end; { if key #00 }
      end { if keypressed }
        else DoSlice;
  until Key = kESC;

  bcx := curposx;
  bcy := curposy;
end; { proc. Editor_Edit }

end.
