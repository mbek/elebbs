unit CharUnit;
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
{$I-}
(*
**
** Char array handling routines for EleBBS
**
** Copyright (c) 1998-1999 by Maarten Bekers
**
** Created : 05-Jan-1999
** Last update : 05-Jan-1999
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses CfgRec;

Type CharArrayType = Array[0..65520] of Char;

type CharArrayObj = Object
          TxtArr    : ^CharArrayType;
          TxtMaxLen : Longint;
          BytesUsed : Longint;

          constructor Init(TxtSize: Longint);
          destructor Done;

          function AddStringLn(Str: String): Boolean;
          function AddString(Str: String): Boolean;

          procedure Clear;
     end; { CharArrayObj }

type pCharArrayObj = ^CharArrayObj;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WIN32}
uses Sysutils;
{$ENDIF}

{$IFDEF OS2}
uses Strings;
{$ENDIF}

{$IFDEF MSDOS}
uses Strings;
{$ENDIF}

{$IFDEF GO32V2}
uses Strings, MemMan;
{$ENDIF}

constructor CharArrayObj.Init(TxtSize: Longint);
begin
  TxtMaxLen := TxtSize;

  System.Getmem(TxtArr, TxtMaxLen);
  System.FillChar(TxtArr^, TxtMaxLen, #00);
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor CharArrayObj.Done;
begin
  System.FreeMem(TxtArr, TxtMaxLen);
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CharArrayObj.AddString(Str: String): Boolean;
begin
  AddString := false;

  if ((StrLen(TxtArr^) + Length(Str)) + 01) > TxtMaxLen then EXIT;

  Move(Str[1], TxtArr^[StrLen(TxtArr^)], Length(Str));
  AddString := true;

  Inc(BytesUsed, Length(Str));
end; { func. AddString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CharArrayObj.AddStringLn(Str: String): Boolean;
begin
  {$IFNDEF ELEUNIX}
    Str := Str + #13#10;
  {$ELSE}
    Str := Str + #10;
  {$ENDIF}

  AddStringLn := AddString(Str);
end; { func. AddString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CharArrayObj.Clear;
begin
  System.FreeMem(TxtArr, TxtMaxLen);
  System.Getmem(TxtArr, TxtMaxLen);

  FillChar(TxtArr^, TxtMaxLen, #00);
  BytesUsed := 0;
end; { proc. Clear }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
end.{ charunit }
