unit OutBlock;
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
** EleBBS output (block) routines for EleBBS
**
** Copyright (c) 1996, 1998 by Maarten Bekers
**
** Created : 11-Oct-1998
** Last update : 13-Oct-2001
**
**
*)

{$IFNDEF WITH_FULL}
  This unit shouldn''t be compiled for an utility program
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Global;

type tOutblockObj = Object
         TotalAdded    : Longint; { default: 0 }
         TermCodesArray: ^MacroArray;

         {-- Constructors and destructors ----------------------------------}
         constructor Init;
         destructor Done;

         {-- Generic routines ----------------------------------------------}
         procedure AddToBlock(const CharsToAdd: String);
         procedure DumpBlock;
         procedure ClearBlock;
     end; { tOutBlockObj }

type pOutBlockObj = ^tOutBlockObj;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Debug_U, Avatar, ComUnit, ApFossil, Multi, Input_U
      ,ScrnU, MemMan, ObjDec;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tOutBlockObj.ClearBlock;
begin
  TotalAdded := 00;
  TermCodesArray^[0] := #00;
end; { proc. ClearBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tOutBlockObj.AddToBlock(const CharsToAdd: String);
begin
  AvtObj^.AvStr(CharsToAdd);

  if NOT LineCfg^.LocalOnly then
    begin
      Move(CharsToAdd[1], TermCodesArray^[TotalAdded], Length(CharsToAdd));
      Inc(TotalAdded, Length(CharsToAdd));
    end; { if }

  if ComObj <> nil then
    begin
      if TotalAdded > 1024 then
        begin
          repeat
            DumpBlock;

            if TotalAdded > 0 then
              DoSlice;
          until (TotalAdded < (TotalAdded + Length(CharsToAdd)));
        end; { if }
    end { if }
      else TotalAdded := 0;

end; { proc. AddToBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure tOutBlockObj.DumpBlock;
var BytesWritten: Longint;
    TempStr     : MacroArray;
begin
  if TotalAdded=00 then EXIT;
  TermCodesArray^[TotalAdded + 01] := #00;

  {$IFDEF WITH_FULL}
    if ComObj <> nil then
      ComObj^.Com_SendWait(TermCodesArray^[0], TotalAdded, BytesWritten, {$IFDEF FPC}@{$ENDIF}DoSlice)
       else BytesWritten := TotalAdded;
  {$ENDIF}

  if BytesWritten > 0 then
    begin
      if BytesWritten < TotalAdded then
        begin
          TempStr := TermCodesArray^;

          Move(TempStr[BytesWritten], TermCodesArray^[0], (TotalAdded - BytesWritten));
          TotalAdded := (TotalAdded - BytesWritten);
        end { if }
          else TotalAdded := 0;
    end; { if }

end; { proc. DumpBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

constructor tOutBlockObj.Init;
begin
  TotalAdded := 0;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMain, 'allocating memory for termcodesarray');
  {$ENDIF}
  AllocMem(TermCodesArray, SizeOf(TermCodesArray^), 'TermCodesArray', 'InitVariables');

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMain, 'filling termcodesarray');
  {$ENDIF}
  FillChar(TermCodesArray^, SizeOf(MacroArray), #00);
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

destructor tOutblockObj.Done;
begin
end; { destructor done }


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

end. { unit OutBlock }
