unit REMSUP;
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
** RemoteScrn support routines for EleBBS
**
** Copyright (c) 1996-2000 by Maarten Bekers
**
** Created : 16-Jan-2000
** Last update : 16-Jan-2000
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF ISCGI}
  This unit should not be compiled into EleWEB
{$ENDIF}

uses {$IFDEF WITH_FULL}
       InOut_U,
     {$ENDIF}
       StrEdit,

      RemScrn,
       ScrnU,
        Global,
         Crt,
          ObjDec;

{$IFNDEF MSDOS}
{$IFNDEF DELPHI}
function RemKeyHookProc(var CH: Char): Boolean;
function RemKeypressed: Boolean;
procedure RemScreenHook(var Screen: ScreenRec);
{$ENDIF}
{$ENDIF}
procedure RemUtilitySetup;
procedure InitRemHooks;


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 uses Cases, LongStr, Debug_U;

function RemKeyHookProc(var CH: Char): Boolean;
begin
  RemKeyHookProc := false;
  if RemScrnObj = nil then EXIT;

  if RemScrnObj^.rem_KeyPressed then
    begin
      CH := RemScrnObj^.rem_ReadKey;
      RemKeyHookProc := true;
    end; { if }
end; { func. RemKeyHookProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF MSDOS}
{$IFNDEF DELPHI}
procedure RemScreenHook(var Screen: ScreenRec);
Const Maxlines = 10;

type LinesRecord = Array[1..MaxLines] of record
                                           ThisRow: Longint;
                                           Scrn   : ScreenLineType;
                                         end;

var RowCounter,
    ColCounter  : Longint;
    ChangesFound: Longint;
    OldC, NewC  : Char;
    OldA, NewA  : Byte;
    ThisLine    : ^LinesRecord;
begin
  if RemScrnObj <> nil then
    begin
      if NOT RemScrnObj^.Connected then EXIT;

      New(ThisLine);
      ChangesFound := 0;

      for RowCounter := 01 to mnuScrnLen do
        begin
          for ColCounter := 01 to mnuScrnWidth do
           begin
             GetScrnInfo(OldScreen^, ColCounter, RowCounter, OldC, OldA);
             GetScrnInfo(Screen, ColCounter, RowCounter, NewC, NewA);

             if (OldC <> NewC) OR (OldA <> NewA) then
               begin
                 Inc(ChangesFound);
                 GetScrnLine(Screen, 01, RowCounter, mnuScrnWidth, ThisLine^[ChangesFound].Scrn);
                 Thisline^[ChangesFound].ThisRow := RowCounter;
                 BREAK;
               end; { if }
           end; { for }

          if ChangesFound > (MaxLines - 1) then
            begin
              RemScrnObj^.rem_SendScreen(Screen, (mnuScrnWidth * mnuScrnLen) * 2);
              ChangesFound := 0;
              BREAK;
            end; { if }

        end; { for }

      if ChangesFound > 0 then
        begin
          for RowCounter := 01 to ChangesFound do
            RemScrnObj^.rem_SendLine(ThisLine^[RowCounter].Scrn, ThisLine^[RowCounter].ThisRow, mnuScrnWidth * 2);

          RemScrnObj^.rem_UpdateDone;
        end; { if }


      OldScreen^ := Screen;

      {$IFDEF WITH_FULL}
        RemScrnObj^.rem_GotoXY(OutputObj^.WhereX, OutputObj^.WhereY, TextAttr);
      {$ENDIF}

      RemScrnObj^.rem_GetInputBuffer;

      Dispose(ThisLine);
    end; { if }
end; { proc. RemScreenHook }
{$ENDIF}
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function RemKeypressed: Boolean;
begin
  RemkeyPressed := false;
  if RemScrnObj = nil then EXIT;

  RemScrnObj^.rem_GetInputBuffer;
  RemKeyPressed := RemScrnObj^.rem_KeyPressed;

  {$IFNDEF WITH_FULL}
    {-- Make sure that ELCONFIG and EleMGR etc exit immediatly after --------}
    {-- the connection with EleMON would be dropped -------------------------}
    if NOT RemScrnObj^.Connected then
      begin
        Halt;
      end; { if }
  {$ENDIF}
end; { func. RemKeypressed }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure InitRemHooks;
begin
 {$IFNDEF MSDOS}
  {$IFNDEF DELPHI}
   {$IFDEF FPC}
     KeyHookProc := @RemKeyHookProc;
     KeyFuncPressed := @RemKeyPressed;
     UpdateScreenHook := @RemScreenHook;
   {$ELSE}
     StrEdit.KeyHookProc := RemKeyHookProc;
     StrEdit.KeyFuncPressed := RemKeyPressed;
     ScrnU.UpdateScreenHook := RemScreenHook;
   {$ENDIF}
  {$ENDIF}
 {$ENDIF}
end; { proc. InitRemHooks }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure RemUtilitySetup;
var Counter: Longint;
    EleMonHandle: Longint;
begin
  {-- Test if we are executed with EleMON enabled ---------------------------}
  {$IFNDEF MSDOS}
  {$IFNDEF DELPHI}
  for Counter := 01 to ParamCount do
    if (Pos('-H', SUpCase(ParamStr(Counter))) > 0) then
      begin
        EleMonHandle := FVal(Copy(ParamStr(Counter), 3, Length(ParamStr(Counter)) - 2));

        {$IFDEF WITH_DEBUG}
          DebugObj.DebugLog(logFastScrn, 'EleMon Specified - ' + ParamStr(Counter));
          DebugObj.DebugLog(logFastScrn, 'EleMon Handle = ' + FStr(EleMonHandle));
        {$ENDIF}

        if EleMonHandle >= 0 then
          begin
            {$IFDEF TCPIP}
              New(RemScrnObj, Init);
              New(OldScreen);

              RemScrnObj^.IsValidated := TRUE;
              RemScrnObj^.PipeHandle.ServerHandle := EleMonHandle;
              RemScrnObj^.PipeHandle.SockHandle := EleMonHandle;
              RemScrnObj^.PipeHandle.HaveConnection := TRUE;
              RemScrnObj^.PipeHandle.DoDisconnect := FALSE;
              RemScrnObj^.Connected := TRUE;

              InitRemHooks;
              RemScrnObj^.Connected := TRUE;
            {$ENDIF}
          end; { if }
      end; { if }
  {$ENDIF}
  {$ENDIF}
end; { proc. RemUtilitySetup }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
