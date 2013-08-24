unit AutoDet;
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
** AUTO ansi detection routines for EleBBS
**
** Copyright (c) 1996-1997 by Maarten Bekers
**
** Created : 01-Jan-1998
** Last update : 01-Jan-1998
**
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

procedure AutoDetectAnsi;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Global, InOut_U, Support, ComUnit, CurTime, Input_U, ApFossil, FastScrn,
     {$IFDEF WINGUI}
       Win_Main,
     {$ENDIF}
     {$IFNDEF NOLINLOCAL}
       Crt,
     {$ENDIF} Debug_U, Multi, ObjDec;

procedure AutoDetectAnsi;

procedure Remote(S: String);
var Written: Longint;
begin
 {$IFDEF WITH_FULL}
   ComObj^.Com_SendWait(S[01], Length(s), Written, {$IFDEF FPC}@{$ENDIF}DoSlice);
 {$ENDIF}
end; { proc. Remote }

var Finished : Boolean;
    CH       : Char;
    Counter  : Byte;
    EndTime  : Longint;
    Temp     : String;
    CharTime : Longint;
    WaitCount: Byte;
begin
  if LineCfg^.Exitinfo^.Baud= 00 then EXIT;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logSupport, 'AutoDetectAnsi (begin)');
  {$ENDIF}

 {$IFDEF WITH_FULL}
   if ComObj = NIL then EXIT;
 {$ENDIF}

  {$IFDEF ELEUNIX}
    LineCfg^.AnsiOn := true;
    EXIT;
  {$ENDIF}

 LocalScreen(SystemMsgPrefix + 'Testing remote capabilities');

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logSupport, 'AutoDetectAnsi (-03)');
  {$ENDIF}

 EndTime := CurrentTime + 10;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logSupport, 'AutoDetectAnsi (-04)');
  {$ENDIF}

 {$IFDEF WITH_FULL}
   ComObj^.Com_PurgeInBuffer;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logSupport, 'AutoDetectAnsi (-05)');
  {$ENDIF}

   While (ComObj^.Com_CharAvail) AND (CurrentTime <= EndTime) do
      CH := ComObj^.Com_GetChar;
 {$ENDIF}

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logSupport, 'AutoDetectAnsi (-06)');
  {$ENDIF}

 Finished := False;
 Counter := 00;
 EndTime := CurrentTime + 05;                     { 40 Seconds max wait }
 Temp := '';

 {$IFDEF WITH_FULL}
 While (NOT Finished) AND (Counter < 450) AND
         (CurrentTime <= EndTime) AND
          (ComObj^.Com_Carrier) do
      begin
        Remote(#12);
        Remote(#27 + '[!');                                          { RIP }
        Remote(#27 + '[6n');                                        { Ansi }
        Remote(#12);                                        { Clear screen }

        WaitCount := 01;
        repeat
          Support.Delay(50);
          Inc(WaitCount);
        until (WaitCount>20) OR
                 (ComObj^.Com_CharAvail);

        CharTime := CurrentTime + 6;
        While (ComObj^.Com_CharAvail) AND (CurrentTime <= CharTime) do
          begin
            CH := ComObj^.Com_GetChar;

            Temp := Temp + CH;

            If Copy(Temp, 1, 3) = 'RIP' then
              begin
                Finished := True;
                LineCfg^.RipOn := True;
                LineCfg^.AnsiOn := True;    { Also supports ANSI by default }
                LocalScreenLn(#32 + '(RIP)');

                While InputObj^.Keypressed do InputObj^.ReadKey;
                Delay(50);
                While InputObj^.Keypressed do InputObj^.ReadKey;
              end; { if }

            if Pos(#27 + '[', Temp) > 0 then
              begin
                Finished := True;
                LineCfg^.AnsiOn := True;

                LocalScreenLn(#32 + '(ANSI)');
                Support.Delay(150);
                While InputObj^.Keypressed do InputObj^.ReadKey;
                Delay(50);
                While InputObj^.Keypressed do InputObj^.ReadKey;
              end; { if }
          end; { If CharReady }
      end; { while }
 {$ENDIF}

  While InputObj^.Keypressed do InputObj^.ReadKey;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logSupport, 'Temp = '+Temp);
    DebugObj.DebugLog(logSupport, 'AutoDetectAnsi ( end )');
  {$ENDIF}
end; { proc. AutoDetectAnsi }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
