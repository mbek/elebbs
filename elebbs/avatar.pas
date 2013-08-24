unit Avatar;
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
** ANSI/ AVATAR routines for EleBBS
**
** Copyright (c) 1996 by Maarten Bekers
**
** Created : 08-Sep-1996
** Last update : 13-Oct-2001
**
** note: Based on routines of Mark May - (c) MK Software
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF WITH_FULL}
  This unit shouldn''t be compiled for an utility program
{$ENDIF}

{$IFDEF ISCGI}
  This unit should not be compiled into EleWEB
{$ENDIF}

uses
  ScrnU,
  Debug_U,
  Crt,
  FastScrn,
  Global;


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

const
  MaxAnsiParms = 200;

const
  ControlCh: Set of Char = ['A','B','C','D','f','s','u','H','J','K','m',';',
                            'P', '@', 'M', 'L', '?'];

  MusicCh  : Set of Char = ['A','B','C','D','E','F','G','#','+','-','O','>',
    '<','N','L','P','T','S','0','1','2','3','4','5','6','7','8','9','.',
    ' '];


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

type tAvatarObj = Object
         AnsiSoundOn   : Boolean;     { default: false }
         AnsiDoBeep    : Boolean;     { default: true }
         DoingBinary   : Boolean;     { default: false }

         StatusAdjustXY: Array[1..2] of Byte;      { X/Y positions to adjust }
                                             { to avoid scrolling the screen }
         AvState       : Longint;                  { 0=normal, 1=esc, 2=esc[ }                { Ansi }
                                                 { 5=^Y, 6=^Y#, 7=^V, 8=^V^A }
                                                           { 9=^V^H 10=^V^H# }
                                                     { 11=Collect Parameters }
                                                        { 14="extended ansi" }

         AnsiParm      : Array [1..MaxAnsiParms] of Byte;
         InsertMode    : Boolean;
         CommandType   : Longint;
         AvAttr        : Longint;                            { Current color }
         AnsiParmNo    : Longint;
         SaveX         : Longint;  { Used by the ANSi functions save/restore }
         SaveY         : Longint;
         RemainingParms: Longint;
         RepCount      : Longint;
         MusicStr      : String[128];

         {-- Constructors and destructors ----------------------------------}
         constructor Init;
         destructor Done;

         {-- Generic routines ----------------------------------------------}
         function InAvatar: Boolean;
         function AnsiColor (Fore:Byte;Back:Byte):String;  { Return Ansi String to set color }
         Function AnsiAttr(AA: Byte): String;              { Return ansi string for attribute }
         Function AnsiAttrDiff(OldA: Byte; NewA: Byte): String; { Return minimal ansi string to update attribute }
         Procedure AvatarChar (ch:Char);   { Interpret Ansi/Avatar codes and display to screen }
         Function CvtColor(colr:Byte):String;  { Convert attr color codes to ansi numbers }
         procedure AvatarNul(const Macro: MacroArray);
         procedure DoWrite(CH: Char);
         procedure AVInit;
         procedure AVReset;
         procedure AvStr(St: String);
         procedure AvStrLn(St: String);
         procedure AddXChar(CH: Char);

         procedure ScrollScrnRegionUp(xl,yl,xh,yh, count: Byte; UpdateGUI: Boolean);
         procedure ScrollScrnRegionDown(xl,yl,xh,yh, count: Byte; UpdateGUI: Boolean);

         {-- Private routines ----------------------------------------------}
         private
           function GetWhereX: Byte;
           function GetWhereY: Byte;

           procedure GotoXY(X, Y: Byte);
           procedure WriteAT(X, Y, A: Byte; S: String);
           procedure ClrEol;
           procedure ClrScr;

           procedure DelCharInLine(Sx: Byte; Sy: Byte);
           procedure InsCharInLine(Sx: Byte; Sy: Byte; Ch: Char);
           procedure InitializeScrnRegion(xl,yl,xh,yh: Byte; Ch: Char);
           procedure ColorParm(Parm:Byte);
           procedure ProcCtl(ch: Char);
           procedure Accum(ch: Char);
           procedure MusicChar(ch: Char);
     end; { tAvatarObj }

type
  pAvatarObj = ^tAvatarObj;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tAvatarObj.Init;
begin
  AvInit;

  AnsiSoundOn := FALSE;
  AnsiDoBeep := TRUE;
  DoingBinary := FALSE;
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tAvatarObj.Done;
begin
end; { destructor done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tAvatarObj.GetWhereX: Byte;
begin
  if StatusAdjustXY[1] > mnuScrnWidth then
     StatusAdjustXY[1] := mnuScrnWidth;

  GetWhereX := StatusAdjustXY[1];
end; { func. GetWhereX }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tAvatarObj.GetWhereY: Byte;
begin
  if StatusAdjustXY[2] > (LineCfg^.ActScrnLen) then
    StatusAdjustXY[2] := LineCfg^.ActScrnLen;

  GetWhereY := StatusAdjustXY[2];
end; { func. GetWhereY }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tAvatarObj.InAvatar: Boolean;
begin
  InAvatar := (AvState > 0);
end; { func. InAvatar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tAvatarObj.GotoXY(X,Y:Byte);
begin
  if (X < 1) then X := 01;
  if (Y < 1) then Y := 01;

  StatusAdjustXY[1] := X;
  StatusAdjustXY[2] := Y;

  if StatusAdjustXY[1] > mnuScrnWidth then
    StatusAdjustXY[1] := mnuScrnWidth;

  if StatusAdjustXY[2] > (LineCfg^.ActScrnLen) then
    StatusAdjustXY[2] := LineCfg^.actScrnLen;

  LineCfg^.DoFlushScrnBuffer := true;
end; { proc. GotoXY }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tAvatarObj.WriteAT(X, Y, A: Byte; S: String);
begin
  {-- Update the internal buffer (for EleMON) ----------------}
    if NOT (S[1] in [#08, #10, #13, #07]) then
      ScrnU.WriteAT(X, Y, A, S);

  {-- Usually thats enough, unless the OS doesnt use a screen-}
  {-- buffer that matches the Videocard buffer, if so, -------}
  {-- manually update the local screen -----------------------}

  {$IFDEF ELEUNIX}
    {$IFNDEF NOLINLOCAL}
      Crt.GotoXY(X, Y);
      Crt.TextAttr := A;
      Write(CrtOutput, S);
    {$ENDIF}
  {$ENDIF}
end; { proc. WriteAT }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tAvatarObj.DoWrite(CH: Char);
var SaveDirect: Boolean;
begin
  if (NOT LineCfg^.AnsiOn) AND (NOT LineCfg^.AvatarOn) then
    begin
      AvAttr := 7;

      {$IFNDEF WINGUI}
        TextAttr := 7;
      {$ENDIF}
    end; { if }

  if LineCfg^.Snooping then
   begin
       if CH = #07 then
        begin
          if AnsiDoBeep then DoBeep;
        end; { if beep }

       {$IFNDEF ELEUNIX}
         if NOT (CH in [#08, #10, #13, #07]) then
       {$ELSE}
         {$IFDEF NOLINLOCAL}
           if NOT (CH in [#08, #10, #13, #07]) then
         {$ENDIF}
       {$ENDIF}
            begin
              LineCfg^.DoFlushScrnBuffer := true;
              SaveDirect := DirectScrnUpdate;
              DirectScrnUpdate := false;

              {$IFNDEF WINGUI}
                WriteAT(GetWhereX, GetWhereY, TextAttr, CH);
              {$ENDIF}
              DirectScrnUpdate := SaveDirect;
            end; {  if }
   end; { if }
end; { proc. DoWrite }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tAvatarObj.AddXChar(CH: Char);
begin
  if NOT (CH in [#13,#10,#08,#07]) then Inc(StatusAdjustXY[1]);
  if CH in [#08] then
   if StatusAdjustXY[1] > 01 then Dec(StatusAdjustXY[01]);

                              {was:+01}
  if (StatusAdjustXY[1] > mnuScrnWidth) then
     begin
        Inc(StatusAdjustXY[2]);
        StatusAdjustXY[1] := 01;

        if StatusAdjustXY[2] > (LineCfg^.actScrnLen) then
          begin
            ScrollScrnRegionUp(01, 01, mnuScrnWidth, LineCfg^.actScrnLen, 1, false);
          end; { if }

        {$IFDEF ELEUNIX}
         {$IFNDEF NOLINLOCAL}
          if StatusAdjustXY[2] > Succ(Hi(WindMax)) then
            begin
              StatusAdjustXY[2] := GetWhereY;
            end; { if }
         {$ENDIF}
        {$ENDIF}
     end
     else begin
             Case CH of
{ CR }         #13 : StatusAdjustXY[1] := 01;
{ LF }         #10 : begin
                       Inc(StatusAdjustXY[2]);

                       if StatusAdjustXY[2] > (LineCfg^.actScrnLen) then
                         begin
                           ScrollScrnRegionUp(01, 01, mnuScrnWidth, LineCfg^.actScrnLen, 1, false);

                           {$IFDEF ELEUNIX}
                             StatusAdjustXY[2] := GetWhereY;
                           {$ENDIF}
                         end; { if }
                     end; { if }
             end; { case }
          end; { if }

  If StatusAdjustXY[1] > mnuScrnWidth then StatusAdjustXY[1] := mnuScrnWidth;
  If StatusAdjustXY[2] > (LineCfg^.actScrnLen) then
    StatusAdjustXY[2] := LineCfg^.actScrnLen;
end; { proc. AddXChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tAvatarObj.ClrEol;
var SaveDirect: Boolean;
begin
  if NOT LineCfg^.Snooping then EXIT;

  LineCfg^.DoFlushScrnBuffer := true;

  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;

  {$IFNDEF WINGUI}
    PartClear(GetWhereX, GetWhereY, mnuScrnWidth, GetWhereY, TextAttr, #32);
  {$ENDIF}
  DirectScrnUpdate := SaveDirect;

  {$IFDEF ELEUNIX}
    {$IFNDEF NOLINLOCAL}
      Crt.GotoXY(GetWhereX, GetWhereY);
      Crt.TextAttr := TextAttr;
      Crt.ClrEol;
    {$ENDIF}
  {$ENDIF}
end; { proc. ClrEol }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tAvatarObj.ClrScr;
var SaveDirect: Boolean;
begin
  if NOT LineCfg^.Snooping then EXIT;
  GotoXY(1,1);

  LineCfg^.DoFlushScrnBuffer := true;

  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;

  {$IFNDEF WINGUI}
    PartClear(1, 1, mnuScrnWidth, LineCfg^.actScrnLen, TextAttr, #32);
  {$ENDIF}

  DirectScrnUpdate := SaveDirect;

      {$IFDEF ELEUNIX}
        {$IFDEF NOLINLOCAL}
          Crt.TextAttr := TextAttr;
          Crt.ClrScr;
        {$ENDIF}
      {$ENDIF}
end; { proc. ClrScr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tAvatarObj.ScrollScrnRegionUp(xl,yl,xh,yh, count: Byte; UpdateGUI: Boolean);
begin
  if NOT LineCfg^.Snooping then EXIT;

  { Validity checking of the parameters is done by the function called! }
  LineCfg^.DoFlushScrnBuffer := true;

  if UpdateGUI then
    begin
      {$IFNDEF WINGUI}
        if (Count > (YH - YL)) OR (Count=0) then
         PartClear(XL, YL, XH, YH, TextAttr, #32)
          else ScrollScrnUp(XL, YL, XH, YH, Count, TextAttr, false);
      {$ENDIF}
    end
      else begin
             {$IFNDEF WINGUI}
              if (Count > (YH - YL)) OR (Count=0) then
               ScrnU.PartClear(XL, YL, XH, YH, TextAttr, #32)
                else ScrnU.ScrollScrnUp(XL, YL, XH, YH, Count, TextAttr, false);
             {$ENDIF}
           end; { if }
end; { proc. ScrollScrnRegionUp }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tAvatarObj.DelCharInLine(Sx: Byte; Sy: Byte);
var Counter: Longint;
    TempC  : Char;
    TempA  : Byte;
begin
  if NOT LineCfg^.Snooping then EXIT;

    LineCfg^.DoFlushScrnBuffer := true;
    For Counter := mnuScrnWidth downto SX do
      begin
        GetScrnInfo(ScrPtr^, Counter, SY, TempC, TempA);
        WriteScrnAttr(Counter + 01, SY, TempA);
        WriteScrnChar(Counter + 01, SY, TempC);
      end; { for }
end; { proc. }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tAvatarObj.InsCharInLine(Sx: Byte; Sy: Byte; Ch: Char);
var TempC: Char;
    TempA: Byte;
    Counter: Byte;
begin
  if NOT LineCfg^.Snooping then EXIT;

  LineCfg^.DoFlushScrnBuffer := true;
  for Counter := mnuScrnWidth downto SX do
    begin
      GetScrnInfo(Scrptr^, Counter, SY, TempC, TempA);
      WriteScrnAttr(Counter - 01, SY, TempA);
      WriteScrnChar(Counter - 01, SY, TempC);
    end; { for }

  {-- and now write the character ------------------------------------------}
  WriteScrnChar(Sx, Sy, Ch);
end; { proc. }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tAvatarObj.ScrollScrnRegionDown(xl,yl,xh,yh, count: Byte; UpdateGUI: Boolean);
begin
  if NOT LineCfg^.Snooping then EXIT;

  LineCfg^.DoFlushScrnBuffer := true;

  if UpdateGUI then
    begin
      {$IFNDEF WINGUI}
        if (Count > (YH - YL)) OR (Count=0) then
          PartClear(XL, YL, XH, YH, TextAttr, #32)
           else ScrollScrnDn(XL, YL, XH, YH, Count, TextAttr, false);
      {$ENDIF}
    end
      else begin
             {$IFNDEF WINGUI}
              if (Count > (YH - YL)) OR (Count=0) then
                ScrnU.PartClear(XL, YL, XH, YH, TextAttr, #32)
                 else ScrnU.ScrollScrnDn(XL, YL, XH, YH, Count, TextAttr, false);
             {$ENDIF}
           end; { if }
end; { proc. }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tAvatarObj.InitializeScrnRegion(xl,yl,xh,yh: Byte; Ch: Char);
begin
  if NOT LineCfg^.Snooping then EXIT;

  LineCfg^.DoFlushScrnBuffer := true;
  {$IFNDEF WINGUI}
    PartClear(Xl, Yl, Xh, Yh, TextAttr, CH);
  {$ENDIF}
end; { proc. InitializeScrnRegion }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure tAvatarObj.AvStr(St: String);
var Counter: Byte;
begin
 for Counter:=01 to Length(St) do
    AvatarChar(St[Counter]);
end; { func. AvStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure tAvatarObj.AvStrLn(St: String);
begin
 AvStr(St);
 AvatarChar(#13);
 AvatarChar(#10);
end; { AvStrLn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tAvatarObj.AvatarNul(const Macro: MacroArray);
var Counter: Word;
begin
  Counter := 00;

  While (Macro[Counter] <> #00) do
    begin
       AvatarChar(Macro[Counter]);
       Inc(Counter);
    end; { while }
end; { proc. AvatarNul }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function tAvatarObj.CvtColor(colr:Byte):String;
begin
  Colr := Colr MOD 8;

  Case Colr of
    0 : CvtColor := '0';
    1 : CvtColor := '4';
    2 : CvtColor := '2';
    3 : CvtColor := '6';
    4 : CvtColor := '1';
    5 : CvtColor := '5';
    6 : CvtColor := '3';
    7 : CvtColor := '7';
  end; { Case Colr }

end; { func. CvtColor }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tAvatarObj.AnsiAttrDiff(OldA: Byte; NewA: Byte): String;
var DoReset: Boolean;
    DoBlink: Boolean;
    DoHigh : Boolean;
    DoFore : Boolean;
    DoBack : Boolean;
    TmpStr : String;
begin
 If OldA=NewA then AnsiAttrDiff := ''
  else begin;
        DoReset := ((OldA AND $88) AND (NOT (NewA AND $88))) <> 0;
        DoBlink := ((NewA AND $80) <> 0) AND (DoReset OR (OldA AND $80 = 0));
        DoHigh  := ((NewA AND $08) <> 0) AND (DoReset OR (OldA AND $08 = 0));
        DoFore  := (((NewA AND $07) <> (OldA AND $07)) OR (DoReset AND ((NewA AND $07) <> 7)));
        DoBack  := (((NewA AND $70) <> (OldA AND $70)) OR (DoReset AND ((NewA AND $70) <> 0)));

        TmpStr := #27 + '[';

        If DoReset then TmpStr := TmpStr + '0;';
        If DoBlink then TmpStr := TmpStr + '5;';
        If DoHigh  then TmpStr := TmpStr + '1;';
        If DoFore  then TmpStr := TmpStr + '3' + CvtColor(NewA and $07) + ';';
        If DoBack  then TmpStr := TmpStr + '4' + CvtColor((NewA shr 4) and $07) + ';';

        TmpStr[Length(TmpStr)] := 'm';
        AnsiAttrDiff := TmpStr;
       end; { else begin }
end; { func. AnsiAttrDiff }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function tAvatarObj.AnsiColor(Fore:Byte; Back:Byte): String;
var TempStr: String;
begin
  TempStr := #27;

  TempStr := TempStr +'['+ '0;';

  If Fore>7 then
    begin;
      TempStr := TempStr + '1;';
      Fore := Fore - 8;
    end; { If Fore > 7 }

  If Back>7 then
    begin;
      TempStr := TempStr + '5;';
      Back := Back - 8;
    end; { if back > 7 }

  TempStr := TempStr + '3';
  TempStr := TempStr + CvtColor(Fore) + ';' + '4' + CvtColor(Back) + 'm';

  AnsiColor := TempStr;
end; { func. AnsiColor }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function tAvatarObj.AnsiAttr(AA: Byte): String;
begin
  AnsiAttr := AnsiColor(AA AND $0f, AA SHR 4);
end; { func. AnsiAttr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure tAvatarObj.AVReset;
begin
  AvState := 0;
  AvAttr := 3;

  ClrScr;

  {$IFNDEF WINGUI}
    TextAttr := AvAttr;
  {$ENDIF}
  InsertMode := False;
end; { proc. AvReset }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tAvatarObj.AVInit;
begin
  SaveX := 0;
  SaveY := 0;
  AvState := 0;
  AvAttr := 3;
  {$IFNDEF WINGUI}
    TextAttr := AvAttr;
  {$ENDIF}
  InsertMode := False;
  AnsiParmNo := 0;

  StatusAdjustXY[1] := 01;
  StatusAdjustXY[2] := 01;
end; { proc. AvInit }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tAvatarObj.ColorParm(Parm:Byte);
var Temp: Word;
begin

  Case Parm of
    00: AvAttr := LightGray;
    01: AvAttr := AvAttr OR $08;                               { Hi intensity }
    04: AvAttr := (AvAttr AND $F8) OR Blue;
    05: AvAttr := AvAttr OR $80;                                      { Blink }
    07: begin;
          Temp := AvAttr AND $77;
          AvAttr := (AvAttr AND $88) OR ((Temp SHR 4) AND $07);
          AvAttr := AvAttr OR ((Temp SHL 4) AND $70);
        end; { 07 }
    08: AvAttr := AvAttr AND $88;                            { Black on black }
    30: AvAttr := (AvAttr AND $F8) OR Black;
    31: AvAttr := (AvAttr AND $F8) OR Red;
    32: AvAttr := (AvAttr AND $F8) OR Green;
    33: AvAttr := (AvAttr AND $F8) OR Brown;
    34: AvAttr := (AvAttr AND $F8) OR Blue;
    35: AvAttr := (AvAttr AND $F8) OR Magenta;
    36: AvAttr := (AvAttr AND $F8) OR Cyan;
    37: AvAttr := (AvAttr AND $F8) OR LightGray;
    40: AvAttr := (AvAttr AND $8F) OR (Black SHL 4);
    41: AvAttr := (AvAttr AND $8F) OR (Red SHL 4);
    42: AvAttr := (AvAttr AND $8F) OR (Green SHL 4);
    43: AvAttr := (AvAttr AND $8F) OR (Brown SHL 4);
    44: AvAttr := (AvAttr AND $8F) OR (Blue SHL 4);
    45: AvAttr := (AvAttr AND $8F) OR (Magenta SHL 4);
    46: AvAttr := (AvAttr AND $8F) OR (Cyan SHL 4);
    47: AvAttr := (AvAttr AND $8F) OR (LightGray SHL 4);
  end; { case }
end; { proc. ColorParm }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure tAvatarObj.ProcCtl(ch:Char);
  Var
    i:  Longint;  { was: word, but this can't go beneath 0, making wrap-around easy }

  Begin
  Case ch of
    '?': begin
           AvState := 14;
           RemainingParms := 02;
         end; { if }
    ';': Begin
         Ansiparmno := Ansiparmno + 1;
         if Ansiparmno > 10 Then
           Ansiparmno := 10;
         End;
    'A': Begin  {cursor up}
         If Ansiparm[1] = 0 Then
           Ansiparm[1] := 1;
         i := GetWhereY;
         Dec(i,AnsiParm[1]);
         If i < 0 Then
           i := 0;
         GoToXy(GetWhereX, i);
         AvState := 0;
         End;
    'B': Begin {cursor down}
           If Ansiparm[1] = 0 Then
             AnsiParm[1] := 1;
           GoToXy(GetWhereX, GetWhereY + AnsiParm[1]);
           AvState := 0;
         End;
    'C': Begin {cursor right}
         If Ansiparm[1] = 0 Then
           Ansiparm[1] := 1;
         GoToXy(GetWhereX + AnsiParm[1], GetWhereY);
         AvState := 0;
         End;
    'D': Begin {cursor left}
         If AnsiParm[1] = 0 Then
            AnsiParm[1] := 1;
         i := GetWhereX;
         if AnsiParm[1] > i then I := 01
          else Dec(i, AnsiParm[1]);
         If i <= 0 Then
           i := 1; { was: 0 }
         GoToXy(Byte(i), GetWhereY);
         AvState := 0;
         End;
    'H','f': {set cursor position}
         Begin
         if Ansiparm[1] = 0 Then
           Ansiparm[1] := 1;
         If Ansiparm[2] = 0 Then
            Ansiparm[2] := 1;
         GoToXy(Ansiparm[2],Ansiparm[1]);
         AvState := 0;
         End;
    'J': Begin
         AvState := 0;
         Case AnsiParm[1] of
           0: Begin {erase to end of screen}
              ClrEol;
              InitializeScrnRegion(1, GetWhereY + 1, mnuScrnWidth, LineCfg^.actScrnLen, ' ');
              End;
           1: Begin {erase from start of screen}
              InitializeScrnRegion(1, 1, mnuScrnWidth, GetWhereY - 1, ' ');
              InitializeScrnRegion(1, GetWhereY, GetWhereX - 1, GetWhereY, ' ');
              End;
           2: Begin  {clear screen}
               {$IFNDEF WINGUI}
                 TextAttr := AvAttr;
               {$ENDIF}
               ClrScr;
              End;
           End;
         End;
    'K': Begin
         AvState := 0;
         Case AnsiParm[1] of
           0: Begin {clear to end of line}
              ClrEol;
              End;
           1: Begin {clear from start of line}
              InitializeScrnRegion(1, GetWhereY, GetWhereX - 1, GetWhereY, ' ');
              End;
           2: Begin {erase whole line}
              InitializeScrnRegion(1, GetWhereY ,mnuScrnWidth, GetWhereY, ' ');
              End;
           End;
         End;
    's': Begin {save cursor position}
         SaveX := GetWhereX;
         SaveY := GetWhereY;
         AvState := 0;
         End;
    'u': Begin {restore cursor position}
         GoToXy(SaveX, SaveY);
         AvState := 0;
         End;
    'm': Begin {set color attribute}
         AvState := 0;
         If AnsiParmNo > 0 Then
           For i := 1 to AnsiParmNo Do
             ColorParm(AnsiParm[i]);
          {$IFNDEF WINGUI}
            TextAttr := AvAttr;
          {$ENDIF}
         End;
    'P': Begin {delete characters}
         AvState := 0;
         If AnsiParm[1] = 0 Then
           AnsiParm[1] := 1;
         For i := 1 to AnsiParm[1] Do
           DelCharInLine(GetWhereX, GetWhereY);
         End;
    '@': Begin {insert characters}
         AvState := 0;
         If AnsiParm[1] = 0 Then
           AnsiParm[1] := 1;
         For i := 1 to AnsiParm[1] Do
           InsCharInLine(GetWhereX, GetWhereY, ' ');
         End;
    'M': Begin {delete lines or "ansi" music}
         If ((AnsiParmNo = 1) and (AnsiParm[1] = 0)) Then
           Begin
           AvState := 12;
           MusicStr := '';
           End
         Else
           Begin
           AvState := 0;
           If AnsiParm[1] = 0 Then
             AnsiParm[1] := 1;
           ScrollScrnRegionUp(1, GetWhereY + 1, mnuScrnWidth, LineCfg^.actScrnLen, AnsiParm[1], true);
           End;
         End;
    'L': Begin {insert lines}
         AvState := 0;
         If AnsiParm[1] = 0 Then
           AnsiParm[1] := 1;
         ScrollScrnRegionDown(1, GetWhereY, mnuScrnWidth, LineCfg^.actScrnLen, AnsiParm[1], true);
         End;
    Else
      AvState := 0;
    End;
  End;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure tAvatarObj.Accum(ch: Char);
begin
  AnsiParm[AnsiParmNo] := (AnsiParm[AnsiParmNo] * 10)  + (Ord(ch) - 48);
end; { proc. Accum }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tAvatarObj.MusicChar(ch: Char);
begin
  Case CH of
    #27   : AvState := 01;
    #$0e  : AvState := 00;
    #13   : begin
              if Length(MusicStr) > 00 then
                begin
                  {$IFDEF WITH_MUSIC}
                    If AnsiSoundOn then
                      Play(MusicStr);
                  {$ENDIF}
                  MusicStr := '';
                end; { if }
            end; { if }
    #10   : ;
     else begin
            if CH in MusicCH then
              begin
                if Length(MusicStr) > 120 then
                  begin
                    {$IFDEF WITH_MUSIC}
                    if AnsiSoundOn then
                      Play(MusicStr);
                    {$ENDIF}
                    MusicStr := '';
                  end; { if }

                Inc(MusicStr[0]);
                MusicStr[Length(MusicStr)] := CH;
              end { else }
              else begin
                     AvState := 00;
                   end; { else }
          end; { else }
  end; { case }

  If ((AvState < 12) AND (Length(MusicStr) > 0)) then
    begin
      {$IFDEF WITH_MUSIC}
      if AnsiSoundOn then Play(MusicStr);
      {$ENDIF}
      MusicStr := '';
    end; { if }
end; { proc. MusicChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure tAvatarObj.AvatarChar(ch:Char);
  Var
    i: Word;

  Begin
  if AnsiParmNo > MaxAnsiParms then AvState := 00;
  if DoingBinary then AvState := 0;

  Case AvState of
    0: Begin
       Case ch of
         #009: Begin                         {tab}
               If GetWhereX > 71 Then
                begin;
                 DoWrite(#13);
                 DoWrite(#10);
                 Inc(StatusAdjustXY[2]);
                 StatusAdjustXY[1] := 01;
                end
               Else
                 GoToXY(((GetWhereX Div 8) + 1) * 8,GetWhereY);
               End;
         #027: AvState := 1;
         #012: AvReset;                      {^L - Avatar}
         #025: AvState := 5;                 {^Y - Avatar}
         #022: AvState := 7;                 {^V - Avatar}
         Else
           If InsertMode Then
             InsCharInLine(GetWhereX, GetWhereY, ch);

            if CH <> #07 then DoWrite(CH);
            if CH <> #07 then AddXChar(CH);
         End;
       End;
    1: Begin
       Case ch of
         #27: Begin
              AvState := 1;
              If InsertMode Then
                InsCharInLine(GetWhereX, GetWhereY, #27);
              DoWrite(#27);
              AddXChar(#27);
              End;
         '[': Begin
              AvState := 2;
              AnsiParmNo := 1;
              For i := 1 To 10 Do
                Ansiparm[i] := 0;
              End;
         #12: Begin
              AvReset;
              AvState := 0;
              End;
         #25: Begin
              If InsertMode Then
                InsCharInLine(GetWhereX, GetWhereY, #27);
              DoWrite(#27);
              AddXChar(#27);
              AvState := 5;
              End;
         #22: Begin
              If InsertMode Then
                InsCharInLine(GetWhereX, GetWhereY, #27);
              DoWrite(#27);
              AddXChar(#27);
              AvState := 6;
              End
         Else
           Begin
           If InsertMode Then
             InsCharInLine(GetWhereX, GetWhereY, #27);
           DoWrite(#27);
           AddXChar(#27);
           If InsertMode Then
             InsCharInLine(GetWhereX, GetWhereY, ch);
           DoWrite(CH);
           AddXChar(CH);
           AvState := 0;
           End;
         End;
       End;
    2: Begin
       Case ch of
         #27: Begin
              AvState := 1;
              If InsertMode Then
                InsCharInLine(GetWhereX, GetWhereY, #27);
              DoWrite(#27);
              AddXChar(#27);
              If InsertMode Then
                InsCharInLine(GetWhereX, GetWhereY, '[');
              DoWrite('[');
              AddXChar('[');
              End;
         '0' .. '9': Accum(ch);
         Else
           If ch in ControlCh Then
             ProcCtl(ch)
           Else
             AvState :=0;
         End;
       End;
    5: Begin
       AnsiParm[1] := Ord(ch);
       AvState := 6;
       End;
    6: Begin
       AvState := 0;
       i := 1;
       While i <= Ord(ch) Do
         Begin
         If InsertMode Then
           InsCharInLine(GetWhereX, GetWhereY, Chr(AnsiParm[1]));
         DoWrite(Chr(AnsiParm[1]));
         AddXChar(Chr(AnsiParm[1]));
         Inc(i);
         End;
       End;
    7: Begin
       Case ch of
         #001: begin
                 AvState := 8;                 {^V^A}
               end; { if }
         #002: Begin
               AvAttr := AvAttr or Blink;    {^B}
               InsertMode := False;
               AvState := 0;
               End;
         #003: Begin
               If GetWhereY > 1 Then            {^C}
                 GoToXy(GetWhereX, GetWhereY - 1);
               InsertMode := False;
               AvState := 0;
               End;
         #004: Begin
               GoToXy(GetWhereX, GetWhereY + 1);   {^D}
               InsertMode := False;
               AvState := 0;
               End;
         #005: Begin
               GoToXy(GetWhereX - 1, GetWhereY);   {^E}
               InsertMode := False;
               AvState := 0;
               End;
         #006: Begin
               If GetWhereX > 1 Then            {^F}
                 GoToXy(GetWhereX + 1, GetWhereY);
               InsertMode := False;
               AvState := 0;
               End;
         #007: Begin
               ClrEol;                       {^G}
               InsertMode := False;
               AvState := 0;
               End;
         #008: begin
                AvState := 9;                 {^H}
               end;
         #009: Begin
               InsertMode := True;           {^I}
               AvState := 0;
               End;
         #010: Begin                         {^J}
               AvState := 11;
               RemainingParms := 5;
               CommandType := 1;
               InsertMode := False;
               AnsiParmNo := 1;
               End;
         #011: Begin                         {^K}
               AvState := 11;
               RemainingParms := 5;
               CommandType := 2;
               InsertMode := False;
               AnsiParmNo := 1;
               End;
         #012: Begin                         {^L}
               AvState := 11;
               RemainingParms := 3;
               CommandType := 3;
               InsertMode := False;
               AnsiParmNo := 1;
               End;
         #013: Begin                         {^M}
               AvState := 11;
               RemainingParms := 4;
               CommandType := 4;
               InsertMode := False;
               AnsiParmNo := 1;
               End;
         #014: Begin
               DelCharInLine(GetWhereX, GetWhereY);{^N}
               InsertMode := False;
               AvState := 0;
               End;
         #025: Begin                         {^Y}
               AvState := 11;
               RemainingParms := 1;
               CommandType := 5;
               AnsiParmNo := 1;
               End;
         End;
       End;
    8: Begin                                 {^V^A}
       AvAttr := Ord(ch);
       {$IFNDEF WINGUI}
         TextAttr := AvAttr;
       {$ENDIF}
       AvState := 0;
       InsertMode := False;
       End;
    9: Begin                                 {^V^H}
       AvState := 10;
       AnsiParm[1] := Ord(ch);
       End;
    10:Begin                                 {^V^H#}
       AvState := 0;
       GoToXy(Ord(ch), AnsiParm[1]);
       InsertMode := False;
       End;
    11:Begin
       AnsiParm[AnsiParmNo] := Ord(ch);
       Inc(AnsiParmNo);
       If AnsiParmNo > MaxAnsiParms Then
         AnsiParmNo := MaxAnsiParms;
       Dec(RemainingParms);
       If RemainingParms < 1 Then
         Begin
         Case CommandType of
           1: Begin                         {^V^J}
              ScrollScrnRegionUp(AnsiParm[3], AnsiParm[2], AnsiParm[5],
                AnsiParm[4], AnsiParm[1], true);
              AvState := 0;
              End;
           2: Begin                         {^V^K}
              ScrollScrnRegionDown(AnsiParm[3], AnsiParm[2], AnsiParm[5],
                AnsiParm[4], AnsiParm[1], true);
              AvState := 0;
              End;
           3: Begin                         {^V^L}
              {$IFNDEF WINGUI}
                TextAttr := AnsiParm[1];
              {$ENDIF}
              InitializeScrnRegion(GetWhereX, GetWhereY, Byte(GetWhereX + AnsiParm[3]),
                Byte(GetWhereY + AnsiParm[2]), ' ');
              AvState := 0;
              End;
           4: Begin                         {^V^M}
              {$IFNDEF WINGUI}
                TextAttr := AnsiParm[1];
              {$ENDIF}
              InitializeScrnRegion(GetWhereX, GetWhereY, Byte(GetWhereX + AnsiParm[4]),
                Byte(GetWhereY + AnsiParm[3]), Chr(AnsiParm[2]));
              AvState := 0;
              End;
           5: Begin                         {Have num chars swith to 6}
              RemainingParms := Ord(Ch) + 1; { was:!!!!! 2}
              CommandType := 6;
              End;
           6: Begin                         {^V^Y}
              RepCount := AnsiParm[AnsiParmNo - 1];
              While RepCount > 0 Do
                Begin
                AnsiParmNo := 2;
                While (AnsiParmNo < (AnsiParm[1]+ 2)) AND (AnsiParmNo < MaxAnsiParms) Do
                  Begin
                  DoWrite(Chr(AnsiParm[AnsiParmNo]));
                  AddXChar(Chr(AnsiParm[AnsiParmNo]));
                  Inc(AnsiParmNo);
                  End;
                Dec(RepCount);
                End;
              AvState := 0;
              End;
           End;
         End;
       End;
    12:Begin {"ansi" music}
         case CH of
           'F': AvState := 13;
           'B': AvState := 13;
          else begin
                 AvState := 13;
                 MusicChar(UpCase(CH));
               end; { if }
         end; { case }
       End; { 12 }
    13:begin {"Ansi" music after F/B}
         MusicChar(UpCase(ch));
       end;
    14:begin
         Dec(RemainingParms);
         If RemainingParms <= 00 then AvState := 00;
       end; { if }
    End;
  End;
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { avatar }
