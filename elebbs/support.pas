unit Support;
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
** Support Routines for EleBBS
**
** Copyright (c) 1996, 1997 by Maarten Bekers
**
** Created : 08-Sep-1996
** Last update : 01-May-1998
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses CfgRec;

{$IFNDEF WITH_FULL}
  This unit shouldn''t be used in an utility program
{$ENDIF}

{$IFDEF ISCGI}
  This unit should not be compiled for EleWEB
{$ENDIF}

{$IFDEF WITH_FULL}
procedure Delay(MSec: Word);
procedure TextBackground(Color: Byte); { Local/remote BackGroundColor change }
procedure TextColor(Color: Byte);            { Local/Remote TextColor change }
procedure Halt(ExitCode: Word);
{$ENDIF}


{$IFDEF WINGUI}
{$IFDEF WITH_FULL}
(*
function  WhereX: Byte;
function  WhereY: Byte;
procedure GotoXY(X, Y: Byte);
*)
{$ENDIF}
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses {$IFDEF VirtualPascal}
       VpSysLow,
     {$ENDIF}

     {$IFDEF WIN32}
       Windows,
     {$ENDIF}


     {$IFDEF WITH_FULL}
       {$IFDEF WINGUI}
         Forms,
       {$ENDIF}
     {$ENDIF}

     {$IFDEF OS2}
       Os2PmApi,
       Os2Base,
       Os2Def,
     {$ENDIF}
     Crt, Multi, Global, ExitProg, Colors,
     ObjDec;     

{$IFDEF WINGUI}
{$IFDEF WITH_FULL}
procedure Delay(MSec: Word);
var FirstTickCount:longint;
begin
  {$IFNDEF Win32}
    DebugLog(logSupport, 'Delaying (Win32) (Start)');
  {$ENDIF}

  FirstTickCount:=GetTickCount;
  DumpBlock(TermCodesArray^);
  {$IFNDEF WINGUI}
    InObj^.UpdateScrn;
  {$ENDIF}

  REPEAT
    Application.ProcessMessages;     { Allowing access to other controls, etc }
  UNTIL ((GetTickCount - FirstTickCount) >= LongInt(MSec));

  {$IFNDEF Win32}
    DebugLog(logSupport, 'Delaying (Win32) ( end )');
  {$ENDIF}
end; { proc. Delay }
{$ENDIF}
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
{$IFNDEF WINGUI}
procedure Delay(MSec: Word);
begin
  OutBlockObj^.DumpBlock;

  While MSec > 100 do
     begin
       {$IFDEF MSDOS}
         Crt.Delay(100);                           { Wait for 100th of a second }
       {$ENDIF}

       {$IFDEF Win32}
         Sleep(100);
       {$ENDIF}

       {$IFDEF OS2}
         DosSleep(100);
       {$ENDIF}

       {$IFDEF ELEUNIX}
         Delay(100);
       {$ENDIF}

       Dec(MSec, 100);                       { Decrease them from the counter }

       {$IFDEF MSDOS}
         DoSlice;
       {$ENDIF}
     end; { while }

  if MSec > 00 then Crt.Delay(MSec);
end; { proc. Delay }
{$ENDIF}
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WINGUI}
{$IFDEF WITH_FULL}
(*
Function WhereX: Byte;
begin
  WhereX := Form1.ColorConsole1.WhereX;
end; { func. Wherex }

Function WhereY: Byte;
begin
  WhereY := Form1.ColorConsole1.WhereY;
end; { func. WhereY }

procedure GotoXY(X, Y: Byte);
begin
  Form1.ColorConsole1.CursorTo(X, Y);
end; { proc. GotoXY }
*)
{$ENDIF}
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


{$IFDEF WITH_FULL}
procedure Halt(ExitCode: Word);
begin
  DoExitProcedures;

  System.Halt(defExitCode);
end; { proc. Halt }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
procedure TextBackground(Color: Byte); { Local/remote BackGroundColor change }
var Back, Fore: Byte;
begin
  {$IFNDEF WINGUI}
   GetColors(TextAttr, Back, Fore);
  {$ELSE}
   GetColors(Form1.ColorConsole1.TextAttr, Back, Fore);
  {$ENDIF}

  {$IFNDEF USINGMGR}
    Write('`B', Color,':');
  {$ENDIF}
end; { proc. TextBackGround }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
{$IFDEF WITH_FULL}
procedure TextColor(Color: Byte);            { Local/Remote TextColor change }
var Back, Fore: Byte;
begin
  {$IFNDEF WINGUI}
   GetColors(TextAttr, Back, Fore);
  {$ELSE}
   GetColors(Form1.ColorConsole1.TextAttr, Back, Fore);
  {$ENDIF}

  {$IFNDEF USINGMGR}
    Write('`F', Color,':');
  {$ENDIF}
end; { proc. Textcolor }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit Support }
