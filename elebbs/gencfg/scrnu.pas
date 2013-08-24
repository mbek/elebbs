unit ScrnU;
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
(*
**
** SCRNU.TPU, Screenwriting unit for ELCONFIG/EleMGR etc.
**
** Copyright (c) 1998 by Maarten Bekers
**
** Created : 16-Oct-1996
** Last update : 29-Sep-1998
**
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF WINGUI}
 {$IFNDEF MSDOS}
  {$IFNDEF OS2}
   {$IFNDEF WIN32}
    {$IFNDEF GO32V2}
      {$DEFINE SLOWSCRN}
    {$ENDIF}
   {$ENDIF}
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF WINGUI}
 {  you shouldn't use this unit! }
{$ENDIF}

{$DEFINE DOSSCRN}
{$IFDEF WIN32}
  {$UNDEF DOSSCRN}
{$ENDIF}


{ Extend has to be included! This unit allocates memory on the heap }
uses Extend
      {$IFNDEF WITH_FULL}
       {$IFNDEF ISCGI} ,Crt {$ENDIF}
      {$ENDIF}


      {$IFDEF WITH_FULL}  ,Crt {$ENDIF}

      {$IFNDEF VirtualPascal} ,Use32{$ENDIF} ;


Const
  Black         = 0;
  Blue          = 1;
  Green         = 2;
  Cyan          = 3;
  Red           = 4;
  Magenta       = 5;
  Brown         = 6;
  LightGray     = 7;

{ Foreground color constants }

  DarkGray      = 8;
  LightBlue     = 9;
  LightGreen    = 10;
  LightCyan     = 11;
  LightRed      = 12;
  LightMagenta  = 13;
  Yellow        = 14;
  White         = 15;

{ Add-in for blinking }

  Blink         = 128;


const lnx_SwitchToAnsi = #27 + '(U';
      lnx_SwitchToLatin= #27 + '(B';

Const mnuNormColor   : Byte = Cyan;            { Default color for menu-items }
      mnuPullColor   : Byte = LightGray;
      mnuBoxColor    : Byte = LightCyan;
      mnuTitleColor  : Byte = 30;
      mnuShadowColor : Byte = 08;
      mnuDisabled    : Byte = Blue;
      mnuTopHiColor  : Byte = 49;             { MenuItem color when hilighted }
      mnuPullHiColor : Byte = 113;
      mnuMsgColor    : Byte = LightGray;
      mnuMsgXPos     : Byte = 01;
      mnuMsgYPos     : Word = 25;
      mnuScrnLen     : Word = 25;
      mnuScrnWidth   : Word = 80;
      mnuTopBar      : Byte = Blue;
      mnuErrorColor  : Byte = 112;
      mnuStyle       : Byte = 04; { 4}
      mnuEditColor   : Byte = 33;                             { Blue on green }
      mnuClearColor  : Byte = Cyan;
      mnuHiLightColor: Byte = 47;
      mnuGlobColor   : Byte = LightGray + Blink;         { Blinking lightgray }
      mnuGlobSelect  : Byte = 113;                            { Blue on White }
      mnuLangTopColor: Byte = 112;
      mnuBackGrChar  : Char = #177;
      mnuBlockChar   : Char = #176;
      mnuBackGrCol   : Byte = Lightgray;
      mnuDblWide     : Char = #205;
      mnuSngWide     : Char = #196;
      mnuUpArrow     : Char = #24;
      mnuDnArrow     : Char = #25;

Const DirectScrnUpdate : Boolean = true;

                                              {1234567890}
const StyleStr: Array[1..06] Of String[10] = ('ÚÄ¿³´ÙÀÃ³Ä',
                                              'ÉÍ»º¹¼ÈÌºÍ',
                                              'ÖÄ·º¶½ÓÇºÄ',
                                              'ÕÍ¸³µ¾ÔÆ³Í',
                                              'ÚÄ·³µ¼ÔÆºÍ',
                                              '+-+|++++|-');





{$IFDEF DOSSCRN}
  Const MaxLines   = 50;
        MaxColumns = 80;
{$ELSE}
  Const MaxLines   = 1024;
        MaxColumns = 1024;
{$ENDIF}

{$IFDEF DOSSCRN}
Type
   ScreenLineType= Array[0..MaxColumns] of record
                                     CH  : Char;
                                     Attr: Byte;
                                   end; { if }
   ScreenRec     = Array[1..MaxLines, 1..MaxColumns] of record
                                            CH   : Char;
                                            Attr : Byte;
                                          end; { ScreenRec }
{$ELSE}
Const MaxScrnBufSize  = (32*1024) - 01;
Type  ScreenRec       = Array[0..MaxScrnBufSize] of Char;
      ScreenLineType  = Array[0..1024] of record
                                            CH: Char;
                                            Attr: Byte;
                                          end; { if }
{$ENDIF}

   SaveArrayType= record
                    Scrn    : ScreenRec;
                    WindMax : Word;
                    WindMin : Word;
                    OldX,
                    OldY    : Byte;
                  end; { Record }

   SavePartType = record
                    Scrn    : ScreenRec;
                    WindMax : Word;
                    WindMin : Word;
                    X1, Y1  : Byte;
                    X2, Y2  : Byte;
                  end; { Record }




var ScrPtr          : ^ScreenRec;
    RealScrnBuf     : ^ScreenRec;
    SaveOutput      : Text;
    cursStartLine   : Byte;
    UpdateScreenHook: procedure(var Screen: ScreenRec);
    cursEndLine     : Byte;

    MainScreenSaver : Pointer;
{$IFDEF Win32}
 {$IFNDEF FPC}
    OutputHandle    : Longint;
 {$ELSE}
    OutputHandle    : Thandle;
 {$ENDIF}
{$ELSE}
    OutputHandle    : Longint;
{$ENDIF}

{$IFNDEF OS2}
    ScrnBufSize     : Longint;
{$ELSE}
    ScrnBufSize     : SmallWord;
{$ENDIF}

procedure SetupScrnU;
{$IFDEF MSDOS}
procedure DetermineScrPtr;
{$ENDIF}
procedure UpdateScreenBuffer(DoUpdate: Boolean);
procedure WriteScrnChar(X, Y: Byte; C: Char);
procedure WriteScrnAttr(X, Y: Byte; A: Byte);
procedure WriteScrn(X, Y: Byte; A: Byte; C: Char);
procedure GetScrnInfo(var Screen: ScreenRec; X, Y: Byte; var C: Char; var A: Byte);
procedure GetScrnLine(var Screen: ScreenRec; X, Y, Len: Byte; var ScreenLine: ScreenLineType);
procedure WriteScrnLine(X, Y, Len: Byte; var ScreenLine: ScreenLineType);
procedure SaveScreen(var P: Pointer);
procedure RestoreScreen(var P: Pointer);
procedure SavePart(var P: Pointer; X1, Y1, X2, Y2: Byte);
procedure RestorePart(var P: Pointer; DoUpdate: Boolean);
procedure CursorOn;
procedure CursorOff;
procedure CursorHalf;
procedure CursorBlock;
{$IFDEF MSDOS}
procedure DesqViewTest(Scr: Pointer);
{$ENDIF}

function  ColorOn: Boolean;
function  GetCodePage: Word;

procedure WriteDescrip(C:Byte; S:String);
procedure WriteRight(X, Y, Attr: Byte; S: String);
procedure WriteAT(X, Y: Longint; Attr:Integer; S:String);
procedure WriteCenter(Y, Attr: Byte; S:String);
procedure WriteCenterPos(X, Y, Len, Attr:Byte; S:String);
procedure ScreenClear(Attr:Byte; CH:Char);
procedure PartClear(X1, Y1, X2, Y2, Attr:Byte; CH:Char);
procedure Box(X1, Y1, X2, Y2, Attr, Style:Byte; Fill:boolean);
procedure BoxTitle(X1, Y1, X2, Y2, Attr, Style:Byte; Fill:Boolean; Msg: String);
procedure ShadFillBox(X1, Y1, X2, Y2, Attr, Style:Byte; Fill:Boolean);
procedure ShadFillBoxTitle(X1, Y1, X2, Y2, Attr, Style:Byte; Fill:Boolean; Msg: String);
procedure ChangeAttr(X1, Y1, X2, Y2, NewAttr: Byte);

procedure ScrollScrnUp(X1, Y1, X2, Y2, Count: Byte; NewAttr: Byte; DoUpdate: Boolean);
procedure ScrollScrnDn(X1, Y1, X2, Y2, Count: Byte; NewAttr: Byte; DoUpdate: Boolean);

function CalcCharPos(X, Y: Byte): Longint;
function CalcAttrPos(X, Y: Byte): Longint;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF OS2}
  uses {$IFDEF WITH_DEBUG}
         Debug_U,
       {$ENDIF}
        VpSysLow, Os2base, Os2Def, LongStr;
{$ENDIF}

{$IFDEF MSDOS}
  uses {$IFDEF WITH_DEBUG}
         Debug_U,
       {$ENDIF}
        Dos, LongStr;
{$ENDIF}

{$IFDEF WIN32}
  uses Windows, LongStr

         {$IFDEF FPC}
           ,Sysutils
           ,Dos
         {$ENDIF}

         {$IFDEF WITH_DEBUG}
           ,Debug_U
         {$ENDIF}

          {$IFDEF WINGUI}
            , Dialogs
          {$ENDIF}

          {$IFDEF VirtualPascal}
            , VpSysLow
          {$ENDIF};
          
{$IFDEF VirtualPascal}
{ Virtual Pascal defines it as an TCharInfo, FPC as a CHAR_INFO, we prefer FPC's way }
type
  CHAR_INFO = TCharInfo;
{$ENDIF}
          
{$ENDIF}

{$IFDEF GO32V2}
  uses Go32,
         {$IFDEF WITH_DEBUG}
           Debug_U,
         {$ENDIF}
          Dos, LongStr;
{$ENDIF}

{$IFDEF ELEUNIX}
  uses Global,
         {$IFDEF WITH_DEBUG}
           Debug_U,
         {$ENDIF}
        Dos, LongStr;
{$ENDIF}



(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

var SaveAttr : Byte;
    SaveProc : Pointer;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure NewExitProc; FAR;
begin
  ExitProc := SaveProc;

  {$IFDEF GO32V2}
    {$IFNDEF SLOWSCRN}
      if ScrPtr <> NIL then
        Dispose(ScrPtr);
    {$ENDIF}
  {$ENDIF}

  {$IFDEF ELEUNIX}
    Write(lnx_SwitchToLatin);
  {$ENDIF}

  {$IFNDEF WINGUI}
   {$IFDEF WITH_FULL}
    TextAttr := SaveAttr;
   {$ENDIF}
  {$ENDIF}
end; { proc. NewExitProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DefUpdateScreenHook(var Screen: ScreenRec);
begin
  if (High(Screen) = 0) then; { fix some warning }
end; { proc. DefUpdateScreenHook }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WIN32}

{$IFDEF FPC}
function WriteConsoleOutput(hConsoleOutput:HANDLE; lpBuffer:pointer; dwBufferSize:COORD; dwBufferCoord:COORD;
   var lpWriteRegion:SMALL_RECT):WINBOOL; stdcall; external 'kernel32' name 'WriteConsoleOutputA';
{$ENDIF}

procedure GetBufCoords(var X, Y: SHORT);
var
  Buffer: TConsoleScreenBufferInfo;
begin
  X := 0;       { default to top of buffer }
  Y := 0;

  if GetConsoleScreenBufferInfo(OutputHandle, Buffer) then
    begin
      X := Buffer.srwindow.Left;
      Y := Buffer.srWindow.Top;
    end; { if }
end; { proc. GetBufCoords }

{$IFNDEF WINGUI}
procedure ShowWin32Screen(var Screen: ScreenRec);
type TmpRec = Array[0..(1024*32) - 1] of CHAR_INFO;

var LineCounter: Longint;
    ColCounter : Longint;

    LineBuf    : ^TmpRec;
    BufSize    : TCoord;
    BufCoord   : TCoord;
    Region     : TSmallRect;
    BufCounter : Longint;
begin
  BufCounter := 0;
  
  { Allocate memory for the buffer - this should be optimized by only allocating }
  { memory once - but for now this will do TODO }
  GetMem(LineBuf, SizeOf(TmpRec) );
  if LineBuf = nil then
    Halt(255);

  { and start filling up the array }
  for LineCounter := 01 to mnuScrnLen do
    begin
       for ColCounter := 01 to mnuScrnWidth do
         With LineBuf^[BufCounter] do
           begin
             {$IFDEF FPC}
               UniCodeChar := WCHAR(Screen[CalcCharPos(ColCounter, LineCounter)]);
             {$ELSE}
               UniCodeChar := WCHAR(Screen[CalcCharPos(ColCounter, LineCounter)]);
             {$ENDIF}
             Attributes := Ord(Screen[CalcAttrPos(ColCounter, LineCounter)]);
            
             Inc(BufCounter);
           end; { for }
      end; { for }

  BufSize.X := mnuScrnWidth;
  BufSize.Y := mnuScrnLen;
  BufCoord.X := 0;
  BufCoord.Y := 0;

  { FIXME!! }
  { !! GetBufCoords(BufCoord.X, BufCoord.Y); }

  Region.Top := 0;
  Region.Left := 0;
  Region.Right := mnuScrnWidth;
  Region.Bottom := mnuScrnLen;

  WriteConsoleOutput(OutputHandle, LineBuf, BufSize, BufCoord, Region);
  
  FreeMem(LineBuf);
end; { proc. ShowWin32Screen }
{$ENDIF}
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WINGUI}
procedure ShowGuiScreen(var Screen: ScreenRec);
var LineCounter: Longint;
    ColCounter : Longint;

    LineBuf    : String;
    CurAttr    : Longint;
    SaveAttr   : Longint;
begin
  LineBuf := '';
  CurAttr := Ord(Screen[CalcAttrPos(1, 1)]);
  SaveAttr := Form1.ColorConsole1.TextAttr;

  for LineCounter := 01 to mnuScrnLen do
    begin
       for ColCounter := 01 to mnuScrnWidth do
         begin
           if (Ord(Screen[CalcAttrPos(ColCounter, LineCounter)])) <> CurAttr then
             begin
               Form1.ColorConsole1.TextAttr := CurAttr;
               Form1.ColorConsole1.CursorTo(ColCounter - Length(LineBuf), LineCounter);
               Form1.ColorConsole1.WriteCodedBuf(@LineBuf[1], Length(LineBuf));

               LineBuf := '';
             end; { if }

           LineBuf := LineBuf + Screen[CalcCharPos(ColCounter, LineCounter)];
           CurAttr := Ord(Screen[CalcAttrPos(ColCounter, LineCounter)]);
         end; { for }

       if LineCounter = mnuScrnLen then
         begin
           Form1.ColorConsole1.UseLineFeed := FALSE;
         end; { if }

       Form1.ColorConsole1.TextAttr := CurAttr;
       Form1.ColorConsole1.CursorTo(ColCounter - Length(LineBuf), LineCounter);
       Form1.ColorConsole1.WriteCodedBuf(@LineBuf[1], Length(LineBuf));

       if LineCounter = mnuScrnLen then
         begin
           Form1.ColorConsole1.UseLineFeed := TRUE;
         end; { if }

       LineBuf := '';
    end; { for }

  Form1.ColorConsole1.TextAttr := SaveAttr;
end; { proc. ShowGuiScreen }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure UpdateScreenBuffer(DoUpdate: Boolean);
{$IFDEF SLOWSCRN}
 {$IFNDEF ISCGI}
type ChangeRecType = Array[1..(MaxColumns * MaxLines)] of record
                                                    X, Y, CH, Attr: Byte;
                                                  end; { changes }
var PrevCH       : Char;
    PrevAT       : Byte;
    TempStr      : String[80];
    xCounter,
    yCounter     : Longint;
    UpdateLn     : Boolean;

    ChangeRec    : ^ChangeRecType;
    Changes,
    SaveAttr,
    SaveX,
    SaveY        : Longint;

    SaveWindMax,
    SaveWindMin  : Longint;
 {$ENDIF}
{$ENDIF}
begin
 if NOT DoUpdate then EXIT;

 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logString, 'UpdateScreenBuffer (begin)');
 {$ENDIF}

 if DoUpdate then                               { EleMON has to be updated }
   begin
     UpdateScreenHook(ScrPtr^);
   end; { if }

  {$IFDEF NOLINLOCAL}
    if RunningBBS then EXIT;
  {$ENDIF}

 {$IFDEF OS2}
  if DoUpdate then
   VioShowBuf(0,        { Buffer offset }
              ScrnBufSize,  { Length in bytes to show }
              0);       { Vio handle - should be 0 for text mode apps }
 {$ENDIF}

 {$IFDEF WIN32}
   {$IFDEF VirtualPascal}
     if DoUpdate then
       SysTvShowBuf(00, (mnuScrnWidth * mnuScrnLen) * 2);
   {$ENDIF}

   {$IFDEF FPC}
     if DoUpdate then
       ShowWin32Screen(ScrPtr^);
   {$ENDIF}

   {$IFDEF WINGUI}
     ShowGuiScreen(ScrPtr^);
   {$ENDIF}
 {$ENDIF}

 {$IFDEF GO32V2}
   if DoUpdate then
     DosMemPut(SegB800, 0000, ScrPtr^, (mnuScrnWidth * mnuScrnlen) * 2);
 {$ENDIF}

 {$IFDEF SLOWSCRN}
  {$IFNDEF ISCGI}
   if NOT DoUpdate then EXIT;

   TempStr := '';
   Changes := 0;
   SaveWindMin := WindMin;
   SaveWindMax := WindMax;
   SaveAttr := TextAttr;
   SaveX := WhereX;
   SaveY := WhereY;

   if (mnuScrnWidth > 0) AND (mnuScrnlen > 0) then
     Window(1, 1, mnuScrnWidth, mnuScrnLen);
   New(ChangeRec);

   {---------------------- First check for all the changes -----------------}
   for yCounter := 01 to mnuScrnLen do
    for xCounter := 01 to mnuScrnWidth do
     if (ScrPtr^[yCounter, xCounter].Attr <> RealScrnBuf^[yCounter, xCounter].Attr) OR
         (ScrPtr^[yCounter, xCounter].CH <> RealScrnBuf^[yCounter, xCounter].CH) then
          begin
            Inc(Changes);
            ChangeRec^[Changes].X := xCounter;
            ChangeRec^[Changes].Y := yCounter;
            ChangeRec^[Changes].CH := Ord(ScrPtr^[yCounter, xCounter].CH);
            ChangeRec^[Changes].Attr := ScrPtr^[yCounter, xCounter].Attr;
          end; { if }

   {----------------------------- Now update everything --------------------}
   for xCounter := 01 to Changes do
     begin
       GotoXY(ChangeRec^[xCounter].X, ChangeRec^[xCounter].Y);
       TextAttr := ChangeRec^[xCounter].Attr;

       TempStr := Chr(ChangeRec^[xCounter].CH);
       {$IFDEF ELEUNIX}
         if Ord(TempStr[1]) < 32  then TempStr := '.';
       {$ENDIF}

       if (ChangeRec^[xCounter].Y <> mnuScrnLen) OR
           (ChangeRec^[xCounter].X <> mnuScrnWidth) then
             Write(SaveOutput, TempStr);
     end; { for }

   WindMin := SaveWindMin;
   TextAttr := SaveAttr;
   WindMax := SaveWindMax;
   GotoXY(SaveX, SaveY);

   RealScrnBuf^ := ScrPtr^;
   Dispose(ChangeRec);
  {$ENDIF}
 {$ENDIF}

 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logString, 'UpdateScreenBuffer ( end )');
 {$ENDIF}

end; { proc. UpdateScreenBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CalcCharPos(X, Y: Byte): Longint;
begin
  CalcCharPos := ( ((Pred(Y) * mnuScrnWidth) + Pred(X)) * 02);
end; { func. CalcCharPos }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CalcAttrPos(X, Y: Byte): Longint;
begin
  CalcAttrPos := ( ((Pred(Y) * mnuScrnWidth) + Pred(X)) * 02) + 01;
end; { func. CalcAttrPos }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure WriteScrnChar(X, Y: Byte; C: Char);
begin
  if (NOT (X in [1..mnuScrnWidth])) OR (NOT (Y in [1..mnuScrnLen])) then EXIT;

  {$IFDEF DOSSCRN}
    ScrPtr^[Y, X].CH := C;
  {$ELSE}
    ScrPtr^[CalcCharPos(X, Y)] := C;
  {$ENDIF}
end; { proc. WriteScrnChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure WriteScrnAttr(X, Y: Byte; A: Byte);
begin
  if (NOT (X in [1..mnuScrnWidth])) OR (NOT (Y in [1..mnuScrnLen])) then EXIT;

  {$IFDEF DOSSCRN}
    ScrPtr^[Y, X].Attr := A;
  {$ELSE}
    ScrPtr^[CalcAttrPos(X, Y)] := Chr(a);
  {$ENDIF}
end; { proc. WriteScrnAttr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure WriteScrn(X, Y: Byte; A: Byte; C: Char);
begin
  if (NOT (X in [1..mnuScrnWidth])) OR (NOT (Y in [1..mnuScrnLen])) then EXIT;
  {$IFDEF DOSSCRN}
    ScrPtr^[Y, X].Attr := A;
    ScrPtr^[Y, X].CH := C;
  {$ELSE}
    ScrPtr^[CalcAttrPos(X, Y)] := Chr(a);
    ScrPtr^[CalcCharPos(X, Y)] := C;
  {$ENDIF}
end; { proc. WriteScrn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GetScrnInfo(var Screen: ScreenRec; X, Y: Byte; var C: Char; var A: Byte);
begin
  {$IFDEF DOSSCRN}
    C := Screen[Y, X].CH;
    A := Screen[Y, X].Attr;
  {$ELSE}
    C := Screen[CalcCharPos(X, Y)];
    A := Ord(Screen[CalcAttrPos(X, Y)]);
  {$ENDIF}
end; { proc. GetScrnInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GetScrnLine(var Screen: ScreenRec;
                      X, Y, Len: Byte; var ScreenLine: ScreenLineType);
begin
  {$IFDEF DOSSCRN}
    System.Move(Screen[Y, X], ScreenLine, Len * 02);
  {$ELSE}
    Move(Screen[CalcCharPos(X, Y)], ScreenLine, Len * 02);
  {$ENDIF}
end; { proc. GetScrnLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure WriteScrnLine(X, Y, Len: Byte; var ScreenLine: ScreenLineType);
begin
  {$IFDEF DOSSCRN}
    System.Move(ScreenLine, ScrPtr^[Y, X], Len * 2);
  {$ELSE}
    Move(ScreenLine, ScrPtr^[CalcCharPos(X, Y)], Len * 02);
  {$ENDIF}
end; { proc. WriteScrnLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SaveScreen(var P: Pointer);
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logRestoreSave, 'Saving screen');
  {$ENDIF}

  P := nil;
  GetMem(P, SizeOf(SaveArrayType));
  if p=nil then
   begin
     {$IFDEF WITH_FULL}
       ClrScr;
     {$ENDIF}
     Writeln('Unable to locate enough memory for screen image.');
     Halt(255);
   end; { for }

  {$IFNDEF OS2}
     SaveArrayType(P^).Scrn := ScrPtr^;
  {$ELSE}
     Move(ScrPtr^, SaveArrayType(P^).Scrn, ScrnBufSize);
  {$ENDIF}

  {$IFDEF WITH_FULL}
    SaveArrayType(P^).WindMin := WindMin;
    SaveArrayType(P^).WindMax := WindMax;
  {$ENDIF}
end; { proc. SaveScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure RestoreScreen(var P: Pointer);
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logRestoreSave, 'RestoreScreen (begin)');
  {$ENDIF}

  if p=nil then EXIT;

  {$IFNDEF OS2}
    ScrPtr^ := SaveArrayType(P^).Scrn;
  {$ELSE}
    Move(SaveArrayType(P^).Scrn, ScrPtr^, ScrnBufSize);
  {$ENDIF}

   {$IFDEF WITH_FULL}
     WindMin := SaveArrayType(P^).WindMin;
     WindMax := SaveArrayType(P^).WindMax;
   {$ENDIF}

   FreeMem(P, SizeOf(SaveArrayType));
   if RealScrnBuf <> nil then
     FillChar(RealScrnBuf^, SizeOf(ScreenRec), #0);
   UpdateScreenBuffer(true);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logRestoreSave, 'Restorescreen ( end )');
  {$ENDIF}

   P:= nil;
end; { proc. RestoreScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure RestoreScreenNoUpdate(var P: Pointer);
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logRestoreSave, 'RestoreScreen (begin)');
  {$ENDIF}

  if p=nil then EXIT;

  {$IFNDEF OS2}
    ScrPtr^ := SaveArrayType(P^).Scrn;
  {$ELSE}
    Move(SaveArrayType(P^).Scrn, ScrPtr^, ScrnBufSize);
  {$ENDIF}

   {$IFDEF WITH_FULL}
     WindMin := SaveArrayType(P^).WindMin;
     WindMax := SaveArrayType(P^).WindMax;
   {$ENDIF}

   FreeMem(P, SizeOf(SaveArrayType));
   if RealScrnBuf <> nil then
     FillChar(RealScrnBuf^, SizeOf(ScreenRec), #0);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logRestoreSave, 'Restorescreen ( end )');
  {$ENDIF}

   P:= nil;
end; { proc. RestoreScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SavePart(var P: Pointer; X1, Y1, X2, Y2: Byte);
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logRestoreSave, 'Saving part of screen');
  {$ENDIF}

  P := nil;

  GetMem(P, SizeOf(SavePartType));
  if p=nil then
   begin
     {$IFDEF WITH_FULL}
       ClrScr;
     {$ENDIF}

     Writeln('Unable to locate enough memory for screen image.');
     Halt(255);
   end; { for }

  {$IFNDEF OS2}
    SavePartType(P^).Scrn := ScrPtr^;
  {$ELSE}
    Move(ScrPtr^, SavePartType(P^).Scrn, ScrnBufSize);
  {$ENDIF}

  {$IFDEF WITH_FULL}
    SavePartType(P^).WindMin := WindMin;
    SavePartType(P^).WindMax := WindMax;
  {$ENDIF}
  SavePartType(P^).X1 := X1;
  SavePartType(P^).Y1 := Y1;
  SavePartType(P^).X2 := X2;
  SavePartType(P^).Y2 := Y2;
end; { proc. SavePart }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure RestorePart(var P: Pointer; DoUpdate: Boolean);
var xCounter,
    yCounter   : Word;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logRestoreSave, 'RestorePart (begin)');
  {$ENDIF}

  if p=nil then EXIT;

  for xCounter := SavePartType(P^).X1 to SavePartType(P^).X2 do
   for yCounter := SavePartType(P^).Y1 to SavePartType(P^).Y2 do
     begin
       {$IFDEF DOSSCRN}
         WriteScrnChar(xCounter, yCounter, SavePartType(P^).Scrn[yCounter, xCounter].CH);
         WriteScrnAttr(xCounter, yCounter, SavePartType(P^).Scrn[yCounter, xCounter].Attr);
       {$ELSE}
         WriteScrnChar(xCounter, yCounter, SavePartType(P^).Scrn[CalcCharPos(xCounter, yCounter)]);
         WriteScrnAttr(xCounter, yCounter, Ord(SavePartType(P^).Scrn[CalcAttrPos(xCounter, yCounter)]));
       {$ENDIF}
     end; { for }

   {$IFDEF WITH_FULL}
     WindMin := SavePartType(P^).WindMin;
     WindMax := SavePartType(P^).WindMax;
   {$ENDIF}

   FreeMem(P, SizeOf(SavePartType));
   if RealScrnBuf <> nil then
     FillChar(RealScrnBuf^, SizeOf(ScreenRec), #0);

   UpdateScreenBuffer(DoUpdate);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logRestoreSave, 'Restorepart ( end )');
  {$ENDIF}

   P:= nil;
end; { proc. RestorePart }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ColorOn: Boolean;

  {$IFNDEF WIN32}
  {$IFNDEF FPC}
   type Videotype = (UnKnown, Mono, CGA, MCGAMono, MCGACol,
                     EGAMono, EGACol, VGAMono, VGACol);
  {$ENDIF}
  {$ENDIF}

   {$IFDEF MSDOS}
   var CurMode: Byte;
       Regs   : Registers;

   function TestVideo: VideoType;
   var Equip : Byte;
       Temp  : VideoType;
   begin
     Regs.AL := $00;
     Regs.AH := $1A;                                       { Grab VGA Info }
     Intr($10, Regs);

     if Regs.AL = $1A then
      Case Regs.BL of
       $00: Temp := unknown;
       $01: Temp := Mono;
       $04: Temp := EGACol;
       $05: Temp := EGAMono;
       $07: Temp := VGAMono;
       $08: Temp := VGACol;
       $0A,
       $0C: Temp := MCGACol;
       $0B: Temp := MCGAMono;
        else Temp := CGA;
    end { Case }
     else begin                                 { More checking is needed }
            Regs.AH := $12;
            Regs.BX := $10;                               { Grab EGA data }
            Intr($10, Regs);

            if Regs.BX=$10 then                             { Ega or Mono }
              begin
               Intr($11, Regs);
               If ((Regs.AL AND $30) SHR 4) = 03 then
                 Temp := Mono
                  else Temp := CGA;
              end
               else begin
                      Regs.AH := $12;
                      Regs.BX := $10;
                      Intr($10, Regs);
                      if Regs.BH = 00 then Temp := EgaCOL else
                        Temp := EgaMono;
                    end; { Else Begin }
          end; { Else Begin }

     TestVideo := Temp;
   end; { func. TestVideo }
 {$ENDIF}

 {$IFDEF OS2}
  function TestVideo: VideoType;
  var V : VioConfigInfo;
      rc: ApiRet;
  begin
    V.cb := sizeof(V);
    VioGetConfig( vio_Config_Current, V, 0 );
    if rc <> 0 then
      TestVideo := VGACol
    else
      case V.Adapter of
        display_Monochrome : TestVideo := Mono;
        display_CGA        : TestVideo := CGA;
        display_EGA        :
          case V.Display of
            monitor_Monochrome : TestVideo := EGAMono;
          else
            TestVideo := EGACol;
         end;
        display_VGA        :
          case V.Display of
            monitor_Monochrome : TestVideo := VGAMono
          else
            TestVideo := VGACol;
          end;
        else
          TestVideo := VGACol;
      end;
  end;
 {$ENDIF}

begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, 'Calling coloron');
  {$ENDIF}

  {$IFNDEF WIN32}
  {$IFNDEF FPC}
    if (TestVideo in [Mono, MCGAMono, EGAMono, VGAMono])
      {$IFNDEF OS2} OR (LastMode=02) {$ENDIF} then
        ColorOn := false
         else ColorOn := true;
  {$ENDIF}
  {$ENDIF}

  {$IFDEF WIN32}
    ColorOn := true;
  {$ENDIF}

  {$IFDEF GO32V2}
    ColorOn := true;
  {$ENDIF}

  {$IFDEF ELEUNIX}
    ColorOn := true;
  {$ENDIF}
end; { func. ColorOn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetCodePage: Word;
{$IFDEF MSDOS}
var Regs: Registers;
begin
  Regs.AX := $6601;
  Intr($21, Regs);

  If Regs.AL = $01 then GetCodePage := Regs.BX
   else GetCodePage := 00; { ? }
end; { func. GetCodePage }
{$ENDIF}

{$IFDEF WIN32}
begin
  {$IFDEF VirtualPascal}
    GetCodePage := SysGetCodePage;
  {$ENDIF}
end; { func. GetCodePage }
{$ENDIF}

{$IFDEF OS2}
begin
  GetCodePage := SysGetCodePage;
end; { func. GetCodePage }
{$ENDIF}

{$IFDEF GO32V2}
begin
  GetCodePage := 850;
end; { func. GetCodePage }
{$ENDIF}

{$IFDEF ELEUNIX}
begin
  GetCodePage := 850;
end; { func. GetCodePage }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SetCursorShape(Start, Stop:Byte);
{$IFDEF MSDOS}
var Regs : registers;
begin
  Regs.AX := $0100;
  Regs.CH := Start;
  Regs.CL := Stop;
  Intr($10,Regs);
end; { proc. SetCursorShape }
{$ENDIF}

{$IFDEF OS2}
begin
  SysTvSetCurType(Start, Stop, NOT ((Start=16) AND (Stop=00)));
end; { proc. SetCursorShape }
{$ENDIF}

{$IFDEF WIN32}
begin
  {$IFDEF VirtualPascal}
    SysTvSetCurType(Start, Stop, NOT ((Start=16) AND (Stop=00)));
  {$ENDIF}
end; { proc. SetCursorShape }
{$ENDIF}

{$IFDEF GO32V2}
var Regs : registers;
begin
  Regs.AX := $0100;
  Regs.CH := Start;
  Regs.CL := Stop;
  Intr($10, Regs);
end; { proc. SetCursorShape }
{$ENDIF}

{$IFDEF ELEUNIX}
begin
 {$IFNDEF ISCGI}
  Case Start of
    16 : Crt.CursorOff;
    00 : if Stop = 16 then Crt.CursorBig
           else Crt.CursorOn;
    02 : { ?? } ;
  end; { case }
 {$ENDIF}
end; { proc. SetCursorShape }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF MSDOS}
procedure GetCursorShape(var startline, endline : byte); assembler;
ASM
  MOV AH, 03h
  MOV BH, 00h
  INT 10h
  LES DI, StartLine
  MOV BYTE PTR ES:[DI], CH
  LES DI, EndLine
  MOV Byte PTR ES:[DI], CL
end; { proc. GetCursorShape }
{$ENDIF}

{$IFDEF OS2}
procedure GetCursorShape(var startline, endline : byte);
var Visible    : Boolean;
    TempStart,
    TempEnd    : Longint;
begin
  SysTvGetCurType(TempStart, TempEnd, Visible);

  StartLine := TempStart;
  EndLine := TempEnd;
end; { proc. GetCursorShape }
{$ENDIF}

{$IFDEF WIN32}
procedure GetCursorShape(var startline, endline : byte);
var Visible    : Boolean;
    TempStart,
    TempEnd    : Longint;
begin
  TempStart := 0;
  TempEnd := 16;

  {$IFDEF VirtualPascal}
    SysTvGetCurType(TempStart, TempEnd, Visible);
  {$ENDIF}

  StartLine := TempStart;
  EndLine := TempEnd;
end; { proc. GetCursorShape }
{$ENDIF}

{$IFDEF GO32V2}
procedure GetCursorShape(var startline, endline : byte);
var Regs: Registers;
begin
  Regs.AH := $03;
  Regs.BH := $00;
  Intr($10, Regs);

  StartLine := Regs.CH;
  EndLine := Regs.CL;
end; { proc. GetCursorShape }
{$ENDIF}

{$IFDEF ELEUNIX}
procedure GetCursorShape(var startline, endline : byte);
begin
  StartLine := 00;
  EndLine := 16;
end; { proc. GetCursorShape }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CursorOff;
begin
  SetCursorShape(16, 00);
end; { proc. CursorOff }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CursorOn;
begin
  SetCursorShape(cursStartLine, cursEndLine);
end; { proc. cursorOn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CursorHalf;
begin
  SetCursorShape(cursStartLine div 2, cursEndLine);
end; { proc. CursorHalf }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CursorBlock;
begin
 SetCursorShape(0, cursEndLine);
end; { proc. Cursorblock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure WriteDescrip(C:Byte; S:String);
var SaveDirect: Boolean;
    Temp      : String;
begin
  SaveDirect := DirectScrnUpdate;
  DirectscrnUpdate := false;

  if Pos(#13, S)=00 then
    begin
      Partclear(mnuMsgXPos, mnuMsgYPos, mnuScrnWidth, mnuMsgYPos, C, #32);
      WriteAT(mnuMsgXPos, mnuMsgYPos, C, S);
    end
      else begin
             Temp := Copy(S, 1, Pos(#13, S) - 01);
             {$IFDEF VER0_99_11}System.{$ENDIF}Delete(S, 1, Length(Temp) + 01);
             PartClear(mnuMsgXPos, mnuMsgYpos - 1, mnuScrnWidth, mnuMsgYPos - 1, C, #32);
             WriteAT(mnuMsgXPos, mnuMsgYpos - 1, C, Temp);

             Partclear(mnuMsgXPos, mnuMsgYPos, mnuScrnWidth, mnuMsgYPos, C, #32);
             WriteAT(mnuMsgXPos, mnuMsgYPos, C, S);
           end; { if }

  DirectScrnUpdate := SaveDirect;
end; { proc. WriteDescrip }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure WriteRight(X, Y, Attr: Byte; S: String);
var Counter   : Longint;
    SaveDirect: Boolean;
begin
  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;

  for Counter := Length(S) downto 01 do
    WriteAT((X-Length(S)) + Counter, Y, Attr, S[Counter]);

  DirectScrnUpdate := SaveDirect;
end; { proc. WriteRight }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure WriteAT(X, Y: Longint; Attr:Integer; S:String);
var Counter: Longint;
begin
  if X < 1 then X := 01;
  if X > mnuScrnWidth then X := mnuScrnWidth;
  if Y < 1 then Y := 01;
  if Y > mnuScrnLen then Y := mnuScrnLen;

  for Counter := X to (X+Length(S))-1 do
   if Counter < (mnuScrnWidth + 01) then
    begin
      WriteScrnChar(Counter, Y, S[(Counter - X) + 01]);
      WriteScrnAttr(Counter, Y, Attr);
    end; { ScrnFigure }

  UpdateScreenBuffer(DirectScrnUpdate);
end; { proc. WriteAT }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure WriteCenter(Y, Attr:Byte; S:String);
var XPos : Byte;
begin
  XPos := (mnuScrnWidth div 2) - (Length(S) div 2);

  WriteAT(XPos, Y, Attr, S);
end; { proc. WriteCenter }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure WriteCenterPos(X, Y, Len, Attr:Byte; S:String);
var XPos : Byte;
begin
  XPos := 00;
  if (Len div 2) < (Length(s) div 2) then
    XPos := 01 else
     XPos := Byte((Len div 2) - (Length(S) div 2));

  WriteAT(X+XPos, Y, Attr, S);
end; { proc. WriteCenterPos }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ScreenClear(Attr:Byte; CH:Char);
var X, Y: Longint;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, 'ScreenClear (begin)');
  {$ENDIF}

  for Y := 01 to mnuScrnLen do
   begin
     for X := 01 to mnuScrnWidth do
       begin
         WriteScrnChar(X, Y, CH);
         WriteScrnAttr(X, Y, Attr);
       end; { For }
   end; { for }

  UpdateScreenBuffer(true);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, 'ScreenClear (end)');
  {$ENDIF}
end; { proc. ScreenClear }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure PartClear(X1, Y1, X2, Y2, Attr:Byte; CH:Char);
var X, Y      : Byte;
    SaveDirect: Boolean;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, 'PartClear (begin)');
  {$ENDIF}

  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;

  For Y:=Y1 to Y2 do
   For X:=X1 to X2 do begin
                         WriteScrnChar(X, Y, CH);
                         WriteScrnAttr(X, Y, Attr);
                      end; { PartClear }

  DirectScrnUpdate := SaveDirect;
  UpdateScreenBuffer(DirectScrnUpdate);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, 'PartClear ( end )');
  {$ENDIF}
end; { proc. PartClear }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Box(X1, Y1, X2, Y2, Attr, Style:Byte; Fill:boolean);
var Counter: Byte;
    TempStr: String;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, 'Boxing (begin)');
  {$ENDIF}

  TempStr := '';
  For Counter := 01 to X2-X1-1 do
    TempStr := TempStr + StyleStr[Style, 2];

  WriteAT(X1, Y1, Attr, StyleStr[Style,1] + TempStr + StyleStr[Style, 3]);

  for Counter:=(Y1+1) to (Y2-1) do
    begin
      if NOT Fill then
        begin
          WriteAT(X1, Counter, Attr, StyleStr[Style, 4]);
          WriteAT(X2, Counter, Attr, StyleStr[Style, 4]);
        end else begin
                   WriteAT(X1, Counter, Attr, StyleStr[Style, 4]);
                   PartClear(X1 + 01, Counter, X2, Counter, Attr, #32);
                   WriteAT(X2, Counter, Attr, StyleStr[Style, 4]);
                 end; { fill }
    end; { For }

  WriteAT(X1, Y2, Attr, StyleStr[Style, 7] + TempStr + StyleStr[Style, 6]);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, 'Boxing ( end )');
  {$ENDIF}
end; { proc. Box }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure BoxTitle(X1, Y1, X2, Y2, Attr, Style:Byte; Fill:Boolean; Msg: String);
var SaveDirect: Boolean;
begin
  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;

  Box(X1, Y1, X2, Y2, Attr, Style, Fill);
  WriteAT((X2 - Length(Msg)), Y1, mnuTitleColor, Msg);

  DirectScrnUpdate := SaveDirect;
end; { proc. BoxTitle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShadFillBox(X1, Y1, X2, Y2, Attr, Style:Byte; Fill:Boolean);
var Counter   : Byte;
    SaveDirect: Boolean;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, 'Shadfillbox  (begin)');
  {$ENDIF}

  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;
  Box(X1, Y1, X2, Y2, Attr, Style, Fill);

  for Counter := (Y1+1) to (Y2+1) do
    ChangeAttr(X2 + 1, Counter, X2 + 2, Counter, mnuShadowColor);

  DirectScrnUpdate := SaveDirect;
  ChangeAttr(X1+2, Y2+1, X2, Y2+1, mnuShadowColor);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, 'Shadfillbox  ( end )');
  {$ENDIF}
end; { proc. ShadFillBox }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShadFillBoxTitle(X1, Y1, X2, Y2, Attr, Style:Byte; Fill:Boolean; Msg: String);
var SaveDirect: Boolean;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, 'ShadFillBoxTitle: '+Msg+ ' (begin)');
  {$ENDIF}

  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;

  ShadFillBox(X1, Y1, X2, Y2, Attr, Style, Fill);
  WriteAT((X2 - Length(Msg)), Y1, mnuTitleColor, Msg);
  DirectScrnUpdate := SaveDirect;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, 'ShadFillBoxTitle: '+Msg+ ' ( end )');
  {$ENDIF}
end; { proc. ShadFillBoxTitle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ChangeAttr(X1, Y1, X2, Y2, NewAttr: Byte);
var X, Y: Byte;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, 'ChangeAttr (begin)');
  {$ENDIF}

  For Y:=Y1 to Y2 do
   For X:=X1 to X2 do
    If (Y<=mnuScrnLen) AND (X<=MnuScrnWidth) then WriteScrnAttr(X, Y, NewAttr);

  UpdateScreenBuffer(DirectScrnUpdate);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, 'ChangeAttr ( end )');
  {$ENDIF}
end; { proc. ChangeAttr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF MSDOS}
procedure DesqViewTest(Scr: Pointer);
var Regs: Registers;
begin
  Regs.AX := $2B01;
  Regs.CX := $4445;
  Regs.DX := $5351;
  Intr($21, Regs);

  if Regs.Al <> $FF then                                   { DESQView present }
    begin
      Regs.AH := $FE;
      Intr($10, regs);
      Scr := ptr(Regs.ES,Regs.DI);
    end;
end; { proc. DesqViewTest }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ScrollScrnUp(X1, Y1, X2, Y2, Count: Byte; NewAttr: Byte; DoUpdate: Boolean);
var YCounter  : Longint;
    Counter   : Longint;
    TempLine  : ScreenLineType;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, 'ScrollScrnUp');
  {$ENDIF}

  if (NOT (X1 in [1..mnuScrnWidth])) OR (NOT (X2 in [1..mnuScrnWidth])) then EXIT;
  if (NOT (Y1 in [1..50])) OR (NOT (Y2 in [1..50])) then EXIT;

  for Counter := 01 to Count do
   for yCounter := (Y1 + 01) to Y2 do
      begin
        GetScrnLine(ScrPtr^, X1, yCounter, (X2 - Pred(X1)), TempLine);
        WriteScrnLine(X1, yCounter - 01, (X2 - Pred(X1)),  TempLine);
      end; { for }

  yCounter := 00;
  while yCounter <= X2 do
    begin
      TempLine[yCounter].CH := #32;
      TempLine[yCounter].Attr := NewAttr;

      Inc(yCounter);
    end; { for }
  WriteScrnLine(X1, Y2, (X2 - Pred(X1)), TempLine);

  UpdateScreenBuffer(DoUpdate);
end; { proc. ScrollScrnUp }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ScrollScrnDn(X1, Y1, X2, Y2, Count: Byte; NewAttr: Byte; DoUpdate: Boolean);
var YCounter  : Longint;
    Counter   : Longint;
    TempLine  : ScreenLineType;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, 'ScrollScrnDn');
  {$ENDIF}

  if (NOT (X1 in [1..mnuScrnWidth])) OR (NOT (X2 in [1..mnuScrnWidth])) then EXIT;
  if (NOT (Y1 in [1..50])) OR (NOT (Y2 in [1..50])) then EXIT;

  for Counter := 01 to Count do
   for yCounter := (Y2 - 01) downto Y1 do
      begin
        if yCounter > 01 then
          begin
            GetScrnLine(ScrPtr^, X1, yCounter, (X2 - Pred(X1)), TempLine);
            WriteScrnLine(X1, yCounter + 01, (X2 - Pred(X1)),  TempLine);
          end; { if }
      end; { for }

  yCounter := 00;
  while yCounter <= X2 do
    begin
      TempLine[yCounter].CH := #32;
      TempLine[yCounter].Attr := NewAttr;

      Inc(yCounter);
    end; { for }
  WriteScrnLine(X1, Y2, (X2 - Pred(X1)), TempLine);

  UpdateScreenBuffer(DoUpdate);
end; { proc. ScrollScrnDn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF MSDOS}
procedure DetermineScrPtr;
var Regs        : Registers;
    AdapterType : Byte;
begin
  Regs.AH := $1a;
  Regs.AL := $00;
  Intr($10, Regs);
  if Regs.AL = $1a then
   begin
     AdapterType := Regs.BL;
     if AdapterType = 12 then
       AdapterType := 10;
     if AdapterType > 11 then
       AdapterType := 2;
   end
    else begin
           Regs.AH := $12;
           Regs.BX := $10;
           Intr($10, Regs);
           if Regs.BX <> $10 Then
             begin
               Regs.AH := $12;
               Regs.BL := $10;
               Intr($10, Regs);
               if (Regs.BH = 0) then
                AdapterType := 4
                  else AdapterType := 5
             end
              else begin
                     Intr($11, Regs);
                     If (((Regs.Al and $30) shr 4) = 3) then
                       AdapterType := 1
                        else AdapterType := 2;
                   end;
         end;

  case AdapterType of
    1,5,11: ScrPtr := Ptr(SegB000, 0);
       else ScrPtr := Ptr(SegB800, 0);
  end;
end; { proc. DetermineScrPtr }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SetupScrnU;
var ScrnSize: Longint;
begin
  ScrPtr := NIL;
  RealScrnBuf := NIL;

  SaveProc := ExitProc;
  ExitProc := @NewExitPRoc;

  {$IFNDEF WINGUI}
    {$IFDEF WITH_FULL}
      SaveAttr := TextAttr;
    {$ENDIF}
  {$ENDIF}

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logRestoreSave, 'Starting initialisation of EleBBS');
  {$ENDIF}

  {$IFDEF SLOWSCRN}
    Move(Output, SaveOutput, SizeOf(SaveOutput));

    {$IFNDEF ELEUNIX}
      Writeln(#254, #32, 'Using slow screen writes!');
      ReadLn;
    {$ENDIF}
  {$ENDIF}


  GetCursorShape(cursStartLine, cursEndLine);

  MainScreenSaver := nil;
  UpdateScreenHook := {$IFDEF FPC}@{$ENDIF}defUpdateScreenHook;

  {$IFDEF WITH_FULL}
    mnuScrnLen := Succ(Hi(Crt.WindMax));
    mnuScrnWidth := Succ(Lo(Crt.WindMax));

  {$ENDIF}

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'Width WindMax = '+FStr(mnuScrnWidth));
    DebugObj.DebugLog(logString, 'Height WindMax= '+FStr(ScrnBufSize));
    {$IFDEF ELEUNIX}
      DebugObj.DebugLog(logString, 'LINES value = '+GetEnv('LINES'));
    {$ENDIF}
  {$ENDIF}

  {$IFDEF FPC}
   {$IFNDEF ISCGI}
    if (Hi(WindMax) = 254) AND (Lo(WindMax) = 255) then
      begin
        mnuScrnLen := 0;
        mnuScrnWidth := 0;
     end; { if }

    if mnuScrnLen = 1 then mnuScrnLen := 0;
    if mnuScrnWidth = 1 then mnuScrnWidth := 0;

    if mnuScrnLen = 0 then mnuScrnLen := FVal(GetEnv('LINES'));
    if mnuScrnWidth = 0 then mnuScrnWidth := FVal(GetEnv('COLUMNS'));

    if mnuScrnLen = 0 then mnuScrnLen := 25;
    if mnuScrnWidth = 0 then mnuScrnWidth := 80;

    if mnuScrnLen > MaxLines then mnuScrnLen := MaxLines;
    if mnuScrnWidth > MaxColumns then mnuScrnWidth := MaxColumns;

    if fVal(GetEnv('FORCE_LINES')) <> 0 then mnuScrnLen := fVal(GetEnv('FORCE_LINES'));
    if fVal(GetEnv('FORCE_COLS')) <> 0 then mnuScrnWidth := fVal(GetEnv('FORCE_COLS'));

    {$IFDEF ELEUNIX}
      {-- now correct the screen lengths ------------------------------------}
      Dec(mnuScrnLen);
      Dec(mnuScrnWidth);

      Crt.ScreenHeight := mnuScrnLen;
      Crt.ScreenWidth := mnuScrnWidth;
      GetMem(ConsoleBuf, Crt.ScreenHeight * Crt.ScreenWidth * 2);
      FillChar(ConsoleBuf^, Crt.ScreenHeight * Crt.ScreenWidth * 2, 0);
    {$ENDIF}

    Window(1, 1, mnuScrnWidth, mnuScrnLen);
   {$ENDIF}
  {$ENDIF}


  mnuMsgYPos := mnuScrnLen;
  ScrnBufSize := mnuScrnLen * mnuScrnWidth;

  {$IFDEF WITH_DEBUG}
    {$IFDEF ELEUNIX}
     DebugObj.DebugLog(logString, 'ForceLiens = ' +GetEnv('FORCE_LINES'));
    {$ENDIF}
    DebugObj.DebugLog(logString, 'mnuScrlen = '+FStr(mnuScrnLen));
    DebugObj.DebugLog(logString, 'mnuScrnWidth = '+FStr(mnuScrnWidth));
    DebugObj.DebugLog(logString, 'mnuScrnbufsize = '+FStr(ScrnBufSize));
  {$ENDIF}

  {$IFNDEF DOSSCRN}
    {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logFastScrn, 'Abort due to too large screenbuffer');
    {$ENDIF}

    if ((MaxScrnBufSize div 2) <= ScrnBufSize) OR (mnuScrnLen > 100) then
      begin
        {$IFDEF WITH_DEBUG}
          DebugObj.DebugLog(logFastScrn, 'Abort due to too large screenbuffer');
        {$ENDIF}

        mnuScrnLen := 100;
        ScrnBufSize := (maxScrnBufSize DIV 2) - 1;
{
        WriteLn;
        WriteLn('Sorry. Your screen size settings need an screen buffer of 32k or you');
        Writeln('have set your screen length to above the 100 lines These sort of screen-');
        WriteLn('sizes are not supported for this program.');
        WriteLn;
        ReadLn;
        Halt(255);
}
      end; { if }
  {$ENDIF}

  {$IFDEF MSDOS}
    {$IFNDEF SLOWSCRN}
      DetermineScrPtr;
      DesqViewTest(ScrPtr);
    {$ENDIF}
  {$ENDIF}


  {$IFDEF OS2}
    VioGetBuf(Pointer(ScrPtr), ScrnBufSize, 0);   { Get logical screen buffer address }
    SelToFlat(Pointer(ScrPtr));               { Convert it from selector:offset form to flat }
  {$ENDIF}

  {$IFDEF WIN32}
    {$IFDEF VirtualPascal}
      ScrPtr := SysTvGetSrcbuf;
    {$ENDIF}

    {$IFDEF FPC}
      New(ScrPtr);
      FillChar(ScrPtr^, SizeOf(ScrPtr^), #00);
    {$ENDIF}

    {$IFDEF WINGUI}
      New(ScrPtr);
      FillChar(ScrPtr^, SizeOf(ScrPtr^), #00);
    {$ENDIF}
  {$ENDIF}

  {$IFDEF GO32V2}
    {$IFNDEF SLOWSCRN}
      New(ScrPtr);
      FillChar(ScrPtr^, SizeOf(ScrPtr^), #00);
    {$ENDIF}
  {$ENDIF}

  {$IFDEF SLOWSCRN}
    GetMem(ScrPtr, SizeOf(ScrPtr^));
    GetMem(RealScrnBuf, SizeOf(RealScrnBuf^));
    FillChar(ScrPtr^, SizeOf(ScrPtr^), #00);
    FillChar(RealScrnBuf^, SizeOf(RealScrnBuf^), #00);
  {$ENDIF}

  {$IFNDEF ELEUNIX}
    if mnuStyle = 4 then
     if GetCodePage <> 437 then mnuStyle := 1;
  {$ENDIF}

  {$IFDEF ELEUNIX}
    Write(lnx_SwitchToAnsi);
  {$ENDIF}

  {$IFDEF WITH_FULL}
    CheckBreak := False;
  {$ENDIF}

  {$IFDEF WIN32}
    OutputHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  {$ENDIF}
end; { proc. SetupScrnU }

end.
