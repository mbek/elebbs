unit StrUnit;
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
{$I-,R-}
{$IFNDEF MSDOS}
 {$H+}          // Enable long strings
{$ENDIF}
(*
**
** String array handling routines for EleBBS
**
** Copyright (c) 1998-1999 by Maarten Bekers
**
** Created : 04-Feb-1999
** Last update : 04-Feb-2002
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses CfgRec;

{$IFDEF MSDOS}
type StrIndexType = Array[0..1] of ^String;
{$ELSE}
type StrIndexType = Array[0..1024 * 20] of record
                                              Content: pChar;
                                              Len    : Longint;
                                           end; { record }
{$ENDIF}

type StringArrayObj = Object
          MaxStrings  : Longint;
          InitFailed  : LongBool;
          {$IFDEF MSDOS}
          StringIndex : ^StrIndexType;
          {$ELSE}
          StringIndex : StrIndexType;
          {$ENDIF}

          constructor Init(AllocStrings: Longint);
          destructor Done;

          procedure Clear;

          function Get(Index: Longint): String;
          procedure Put(Index: Longint; const Value: String);
          procedure Delete(Index: Longint);
          procedure Add(Value: String);
     end; { StringArrayObj }

type pStringArrayObj = ^StringArrayObj;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 {$IFNDEF MSDOS}
 uses SysUtils;
 {$ENDIF}

constructor StringArrayObj.Init(AllocStrings: Longint);
var Counter: Longint;
begin
  MaxStrings := AllocStrings;
  InitFailed := false;

  {$IFDEF MSDOS}
  StringIndex := nil;

  {$i-} GetMem(StringIndex, SizeOf(StrIndexType) *  MaxStrings); {$i+}
  if (IoResult <> 0) OR (StringIndex = nil) then
    begin
      InitFailed := true;

      FAIL;
    end; { if }
  {$ENDIF}

  if NOT InitFailed then
   for Counter := 00 to MaxStrings do
     {$IFDEF MSDOS}
       StringIndex^[Counter] := nil;
     {$ELSE}
       StringIndex[Counter].Content := nil;
     {$ENDIF}
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor StringArrayObj.Done;
var Counter: Longint;
begin
  if NOT InitFailed then
    begin
       for Counter := 01 to MaxStrings do
        {$IFDEF MSDOS}
          if StringIndex^[Counter] <> nil then
            FreeMem(StringIndex^[Counter], Length(StringIndex^[Counter]^) + 01);
        {$ELSE}
          if StringIndex[Counter].Content <> nil then
            begin
              FreeMem(StringIndex[Counter].Content, StringIndex[Counter].Len);
            end;
        {$ENDIF}

      {$IFDEF MSDOS}
        FreeMem(StringIndex, SizeOf(StrIndexType) * MaxStrings);
      {$ENDIF}
    end; { if }
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure StringArrayObj.Clear;
var Counter: Longint;
begin
  if NOT InitFailed then
    begin
      for Counter := 01 to MaxStrings do
       {$IFDEF MSDOS}
        if StringIndex^[Counter] <> nil then
          begin
            FreeMem(StringIndex^[Counter], Length(StringIndex^[Counter]^) + 01);
            StringIndex^[Counter] := nil;
          end; { if }
       {$ELSE}
        if StringIndex[Counter].Content <> nil then
          begin
            FreeMem(StringIndex[Counter].Content, StringIndex[Counter].Len);
            StringIndex[Counter].Content := nil;
          end; { if }
       {$ENDIF}

      {$IFDEF MSDOS}
        FreeMem(StringIndex, SizeOf(StrIndexType) * MaxStrings);
      {$ENDIF}
    end; { if }

  {$IFDEF MSDOS}
  StringIndex := nil;

  {$i-} GetMem(StringIndex, SizeOf(StrIndexType) * MaxStrings); {$i+}
  if (IoResult > 0) OR (StringIndex = nil) then
    begin
      InitFailed := true;
    end; { if }
  {$ENDIF}

  if NOT InitFailed then
   for Counter := 01 to MaxStrings do
    {$IFDEF MSDOS}
      StringIndex^[Counter] := nil;
    {$ELSE}
      StringIndex[Counter].Content := nil;
    {$ENDIF}
end; { proc. Clear }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function StringArrayObj.Get(Index: Longint): String;
begin
  if InitFailed then
    begin
      Get := 'Not enough memory available';
      EXIT;
    end; { if }

  if Index > MaxStrings then
    begin
      Get := '';
      EXIT;
    end; { if }

  if Index <= 0 then
    begin
      Get := '';
      EXIT;
    end; { if }

  {$IFDEF MSDOS}
  if StringIndex^[Index] <> nil then
    Get := StringIndex^[Index]^
      else Get := '';
  {$ELSE}
  if StringIndex[Index].Content <> nil then
    Get := StrPas(StringIndex[Index].Content)
      else Get := '';
  {$ENDIF}
end; { func. Get }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure StringArrayObj.Put(Index: Longint; const Value: String);
var oo: longint;
begin
  if InitFailed then EXIT;
  if Index > MaxStrings then EXIT;

  {$IFDEF MSDOS}
  if StringIndex^[Index] <> nil then
    begin
      FreeMem(StringIndex^[Index], Length(StringIndex^[Index]^) + 01);
      StringIndex^[Index] := nil;
    end; { if }
  {$ELSE}
  if StringIndex[Index].COntent <> nil then
    begin
      FreeMem(StringIndex[Index].Content, StringIndex[Index].Len);
      StringIndex[Index].Content := nil;
    end; { if }
  {$ENDIF}

  {$IFDEF MSDOS}
    {$i-} GetMem(StringIndex^[Index], Length(Value) + 01); {$i+}
    if (IoResult <> 0) OR (StringIndex^[Index] = NIL) then
      begin
        EXIT;
      end; { if }
  {$ELSE}
    // ugly hack because VP seems to get confused.
    StringIndex[Index].Len := (((Length(Value) div 1024) + 2) * 1024) + 1;
    GetMem(StringIndex[Index].Content, StringIndex[Index].Len);
  {$ENDIF}

  {$IFDEF MSDOS}
    StringIndex^[Index]^ := Value;
  {$ELSE}
    StrPCopy(StringIndex[Index].Content, Value);
  {$ENDIF}
end; { proc. Put }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure StringArrayObj.Delete(Index: Longint);
begin
  Put(Index, Get(MaxStrings));
  Put(MaxStrings, '');
end; { proc. Delete }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure StringArrayObj.Add(Value: String);
var Counter: Longint;
begin
  if InitFailed then EXIT;

  for Counter := 01 to MaxStrings do
    if Get(Counter) = '' then
      begin
        BREAK;
      end; { if }

  if Counter < MaxStrings then
    Put(Counter, Value);
end; { proc. Add }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { strunit }
