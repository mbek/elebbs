unit FastScrn;
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
** Fast screen access routines for EleBBS
**
** Copyright (c) 1997 by Maarten Bekers
**
** Created : 12-Nov-1997
** Last update : 09-May-1999
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF WITH_FULL}
  This unit shouldn''t be compiled for an utility program
{$ENDIF}

{$IFDEF WINGUI}
  Const mnuScrnLen  = 25;
        mnuScrnWidth= 80;

type    ScreenRec     = Array[1..50, 1..80] of record
                                                 CH   : Char;
                                                 Attr : Byte;
                                               end; { ScreenRec }

        SaveArrayType = record
                          Scrn    : ScreenRec;
                          WindMax : Word;
                          WindMin : Word;
                          OldX,
                          OldY    : Byte;
                        end; { Record }
{$ENDIF}


procedure SaveScreen(var P: Pointer);
procedure RestoreScreen(var P: Pointer);                   { Restores screen }
procedure DisposeScreen(var P: Pointer);
Procedure FastWrite(X,Y,Attr: Byte; S:String);
Procedure BoxWindow(X1, Y1, X2, Y2: Byte; Fore, Back: Byte; Title: String);
Procedure ColorArea(X1, Y1, X2, Y2: Word; Attr: Byte);
procedure FillArea(X1, Y1, X2, Y2: Word; Ch: Char; Attr: Byte);
procedure LocalScreen(S: String);
procedure LocalScreenLn(S: String);
procedure DoBeep;
procedure GetScreen(X, Y: Word; var TempC: Char; var TempA: Byte);
procedure PutScreen(X, Y: Word; TempC: Char; TempA: Byte);

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses {$IFDEF WINGUI}
       InOut_U,
       Win_Main,
     {$ENDIF}

     {$IFDEF VirtualPascal}
       {$IFNDEF ISCGI}
         Crt,
       {$ENDIF}
       Dos,
       VpSysLow,
       ScrnU,
     {$ENDIF}

     {$IFDEF FPC}
       Crt,
       Dos,
       ScrnU,
     {$ENDIF}

     {$IFDEF MSDOS}
       Crt,
       Dos,
       ScrnU,
     {$ENDIF}

     {$IFDEF WIN32}
       Windows,
     {$ENDIF}

      LongStr, Debug_U, CentrStr, Colors, Global, Memman, Avatar,
      RemScrn, ObjDec;

{$IFDEF WINGUI}
  Const DirectScrnUpdate : Boolean = true;
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SaveScreen(var P: Pointer);
{$IFDEF WINGUI}
var TempAttr   : Byte;
    TempChar   : Char;
    XCounter,
    YCounter   : Longint;
{$ENDIF}
begin
  if ProgTerminated then EXIT;

  if NOT AllocMem(P, SizeOf(SaveArrayType), 'SaveArrayType', 'SaveScreen') then EXIT;

  {$IFNDEF WINGUI}
    Move(ScrnU.ScrPtr^, SaveArrayType(P^).Scrn, (mnuScrnLen * mnuScrnWidth) * 2);
    SaveArrayType(P^).OldX := Crt.WhereX;
    SaveArrayType(P^).OldY := Crt.WhereY;
  {$ELSE}
    for XCounter := 01 to ScrnWidth do
     for YCounter := 01 to ScrnHeight do
       begin
         Form1.ColorConsole1.GetScreen(XCounter, YCounter, TempChar, TempAttr);

         SaveArrayType(P^).Scrn[YCounter, XCounter].CH  := TempChar;
         SaveArrayType(P^).Scrn[YCounter, XCounter].Attr:= TempAttr;
       end; { Counter }

    SaveArrayType(P^).OldX := Form1.ColorConsole1.WhereX;
    SaveArrayType(P^).OldY := Form1.ColorConsole1.WhereX;
  {$ENDIF}
end; { proc. SaveScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure RestoreScreen(var P: Pointer);
var XCounter,
    YCounter      : Word;
begin
  if P = nil then EXIT;

  {$IFNDEF WINGUI}
    Move(SaveArrayType(P^).Scrn, ScrnU.ScrPtr^, (mnuScrnLen * mnuScrnWidth) * 2);
    With SaveArrayType(P^) do
      Crt.GotoXY(OldX, OldY);
  {$ELSE}
    for XCounter := 01 to ScrnWidth do
     for YCounter := 01 to ScrnHeight do
       begin
         if ProgTerminated then EXIT;

         With SaveArrayType(P^).Scrn[YCounter, XCounter] do
           Form1.ColorConsole1.PutScreen(XCounter, YCounter, CH, Attr);
       end; { Counter }

    With SaveArrayType(P^) do
      Form1.Colorconsole1.CursorTo(OldX, OldY);
  {$ENDIF}

  DisposeScreen(p);

  {$IFNDEF WINGUI}
    UpdateScreenBuffer(true);
  {$ENDIF}
end; { proc. RestoreScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DisposeScreen(var P: Pointer);
begin
   ReleaseMem(P, SizeOf(SaveArrayType));
end; { proc. DisposeScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FastWrite(X,Y,Attr: Byte; S:String);
var Counter: Byte;

    xCounter,
    yCounter   : Word;
    TempChar   : Char;
    TempAttr   : Longint;
begin
 {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, 'FastWriting (begin): "'+S+'" ('+FStr(X)+','+FStr(Y)+')');
 {$ENDIF}

  if RemScrnObj <> nil then
    RemScrnObj^.ScrnChanged := true;

 if Length(s) > mnuScrnWidth then
  begin
    {$IFNDEF WIN32}
      S[0] := Chr(mnuScrnWidth);
    {$ELSE}
      SetLength(s, mnuScrnWidth);
    {$ENDIF}
  end; { if }

 {$IFDEF WINGUI}
   if ProgTerminated then EXIT;

   if Length(S) > 01 then
    For Counter := 01 to Length(S) do
      Form1.ColorConsole1.PutScreen(X + (Counter-1), Y, S[Counter], Attr);

   if Length(S) = 01 then
    Form1.ColorConsole1.PutScreen(X, Y, S[01], Attr);
 {$ELSE}
   ScrnU.WriteAT(X, Y, Attr, S);
 {$ENDIF}

 {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, 'FastWriting ( end )');
 {$ENDIF}
end; { proc. FastWrite }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  Str2Word(C:char; Attr: Byte): Word;
begin
  Str2Word := Ord(C) + (Attr SHL 8);
end; { func. Str2Word }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ColorArea(X1, Y1, X2, Y2: Word; Attr: Byte);
var XCounter,
    YCounter  : Byte;
    Temp      : Word;
    TempCH    : Char;
    TempA     : Byte;
    SaveDirect: Boolean;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, 'ColorArea (begin)');
  {$ENDIF}

  if RemScrnobj <> nil then
    RemScrnObj^.ScrnChanged := true;

  If X1 > X2 then X1 := X2;
  If Y1 > Y2 then Y1 := Y2;

  {$IFDEF WINGUI}
    For XCounter := X1 to X2 do
     For YCounter := Y1 to Y2 do
      begin
         if ProgTerminated then EXIT;
         Form1.ColorConsole1.GetScreen(XCounter, YCounter, TempCH, TempA);
         Temp := Str2Word(TempCH, TempA);

         FastWrite(XCounter, YCounter, Attr, Chr(Lo(Temp)));
      end; { for Counter }
  {$ELSE}
    ChangeAttr(X1, Y1, X2, Y2, Attr);
  {$ENDIF}

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, 'ColorArea ( end )');
  {$ENDIF}
end; { proc. ColorArea }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FillArea(X1, Y1, X2, Y2: Word; Ch: Char; Attr: Byte);
var XCounter,
    YCounter  : Byte;
    Temp      : Word;
    SaveDirect: Boolean;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, 'FillArea');
  {$ENDIF}
  If X1 > X2 then X1 := X2;
  If Y1 > Y2 then Y1 := Y2;

  if RemScrnObj <> nil then
    RemScrnObj^.ScrnChanged := true;

  {$IFNDEF WINGUI}
    SaveDirect := DirectScrnUpdate;
    DirectScrnUpdate := false;
  {$ENDIF}

  For YCounter := Y1 to Y2 do
   begin
      FastWrite(X1, YCounter, Attr, Dup(Ch, Succ(X2 - X1)));
   end; { for Counter }

  {$IFNDEF WINGUI}
    DirectScrnUpdate := false;
  {$ENDIF}
end; { proc. FillArea }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure BoxWindow(X1, Y1, X2, Y2: Byte; Fore, Back: Byte; Title: String);
Const Style   : Byte = 04;
const StyleStr: Array[1..05] Of String[10] = ('ÚÄ¿³´ÙÀÃ³Ä',
                                              'ÉÍ»º¹¼ÈÌºÍ',
                                              'ÖÄ·º¶½ÓÇºÄ',
                                              'ÕÍ¸³µ¾ÔÆ³Í',
                                              'ÚÄ·³µ¼ÔÆºÍ');



var Teller,
    LengthTel : Byte;
    TitleLen  : Byte;

    SaveDirect: Boolean;
begin
 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logFastScrn, 'BoxWindow (begin)');
 {$ENDIF}

  {$IFNDEF WINGUI}
    if GetCodePage <> 437 then Style := 01 else Style := 04;
  {$ELSE}
    Style := 01;
  {$ENDIF}

  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;

  TitleLen := Length(Title);
  If TitleLen>00 then Inc(TitleLen, 02);

  FastWrite(X1, Y1, MakeAttr(Fore, Back), StyleStr[Style, 1]);
  For Teller := (X1+1) to ((X2-1)-TitleLen) do
     FastWrite(Teller, Y1, MakeAttr(Fore, Back), StyleStr[Style, 2]);

  If TitleLen>00 then
   begin
     FastWrite(X2 - (2+Length(Title)), Y1, MakeAttr(Fore, Back), ' ');
     For Teller := 01 to Length(Title) do
       FastWrite((X2 - (2+Length(Title))+Teller), Y1, MakeAttr(Fore, Back), Title[Teller]);
     FastWrite(X2-1, Y1, MakeAttr(Fore, Back), ' ');
    end; { for }

   FastWrite(X2, Y1, MakeAttr(Fore, Back), StyleStr[Style, 3]);

   For LengthTel := (Y1+1) to (Y2-1) do
    begin
     FastWrite(X1, LengthTel, MakeAttr(Fore, Back),StyleStr[Style, 4]);

     For Teller := (X1+1) to (X2-1) do
      FastWrite(Teller, LengthTel, MakeAttr(Fore, Back), ' ');

     FastWrite(X2, LengthTel, MakeAttr(Fore, Back), StyleStr[Style, 4]);
    end; { for length }

   FastWrite(X1, Y2, MakeAttr(Fore, Back), StyleStr[Style, 7]);
   For Teller := (X1+1) to (X2-1) do
      FastWrite(Teller, Y2, MakeAttr(Fore, Back), StyleStr[Style, 10]);
   FastWrite(X2, Y2, MakeAttr(Fore, Back), StyleStr[Style, 6]);

   If (X2<78) AND (Y2<24) then
                 begin
                   ColorArea(X2+1, Y1+1, X2+2, Y2, 08);
                   ColorArea(X1+2, Y2+1, X2+2, Y2+1, 08);
                 end; { Make Shadow }

  DirectScrnUpdate := Savedirect;
  {$IFNDEF WINGUI}
    UpdateScreenBuffer(DirectScrnUpdate);
  {$ENDIF}

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, 'BoxWindow ( end )');
  {$ENDIF}
end; { BoxWindow }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure LocalScreen(S: String);
var Counter: longint;
begin
  if RunningBBS then
    begin
      for Counter := 01 to Length(s) do
       begin
         AvtObj^.DoWrite(s[Counter]);
         AvtObj^.AddXChar(S[Counter]);
       end; { for }
    end
      else Write(s);
end; { proc. LocalScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure LocalScreenLn(S: String);
begin
  LocalScreen(s);
  LocalScreen(#13 + #10);
end; { proc. LocalScreenLn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoBeep;
begin
  {$IFDEF OS2}
    PlaySound(800, 150);
  {$ENDIF}

  {$IFDEF MSDOS}
    Sound(800);
    Delay(150);
    NoSound;
  {$ENDIF}

  {$IFDEF ELEUNIX}
    Sound(800);
    Delay(150);
    NoSound;
  {$ENDIF}

  {$IFDEF WIN32}
    MessageBeep($FFFFFFFF);
  {$ENDIF}

  if RemScrnObj <> nil then
    RemScrnObj^.rem_DoBeep;
end; { proc. DoBeep }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GetScreen(X, Y: Word; var TempC: Char; var TempA: Byte);
begin
  {$IFDEF WINGUI}
    Form1.ColorConsole1.GetScreen(X, Y, TempC, TempA);
  {$ELSE}
    GetScrnInfo(ScrPtr^, X, Y, TempC, TempA);
  {$ENDIF}
end; { proc. GetScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure PutScreen(X, Y: Word; TempC: Char; TempA: Byte);
begin
  {$IFDEF WINGUI}
    Form1.ColorConsole1.PutScreen(X, Y, TempC, TempA);
  {$ELSE}
    WriteScrn(X, Y, TempA, TempC);
  {$ENDIF}
end; { proc. PutScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit FASTSCRN }
