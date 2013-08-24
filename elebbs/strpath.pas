unit StrPath;
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
** Path String routines for EleBBS
**
** Copyright (c) 1996,97 by Maarten Bekers
**
** Created : 27-Mar-1998
** Last update : 27-Mar-1998
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF MSDOS}
uses SysUtils, Dos;
{$ENDIF}

{$IFDEF OS2}
uses Dos;
{$ENDIF}

{$IFDEF WIN32}
uses Sysutils;
{$ENDIF}

{$IFDEF GO32V2}
uses Dos;
{$ENDIF}

{$IFDEF ELEUNIX}
uses Dos;
{$ENDIF}

{$IFNDEF ELEUNIX}
Const BsChar        = '\';
{$ELSE}
Const BsChar        = '/';
{$ENDIF}

function  IsFtpUrl(const Path: String): Boolean;
Function  GetFileName(Start: Byte; Orig: String; NextWord: Byte): String;
Function  ForceBack (Path : String) : String;    { Force backslash if needed }
Function  NoExtension(S: String): String;
Function  JustExtension(S: String): String;
Function  JustPath(S: String): String;                   { Returns just path }
Function  JustName(Path: String): String;
function  NoBackSlash(Path: String): String;
function  RdxName(S: String): String;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Cases,
      WordStr;

function NoBackSlash(Path: String): String;
begin
  While (Length(Path) > 1) AND (Path[Length(Path)] = BsChar) do
    SetLength(Path, Length(Path) - 1);

  NoBackSlash := Path;
end; { func. NoBackSlash }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  ForceBack (Path : String) : String;    { Force backslash if needed }
begin
  if Length(path) > 0 then
    begin
      if NOT (Path[length(path)] in ['\', '/', ':']) and (Path<>'') then
        Path := Path + BsChar;
    end; { if }

  ForceBack:=Path;
end; { func. ForceBack }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetFileName(Start: Byte; Orig: String; NextWord: Byte): String;
var Temp    : String;
    WordNum : Byte;
begin;
  Temp := Copy(Orig, Start, Length(Orig));
  WordNUm := 00;

  While (Pos(#32, Temp) > 00) AND (WordNum <> Nextword) do
    begin
      Delete(Temp, 01, Pos(#32, Temp));
      While (Length(Temp) > 0) AND (Temp[1]=#32) do Delete(Temp, 1, 1);

      Inc(WordNum);
    end; { while }

  GetFileName := ExtractWord(Temp, 1, defExtractWord, true, false);
end; { func. GetfileName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  JustExtension(S: String): String;
var Dir, Name, Ext: String;
begin
  {$IFNDEF WIN32}
    FSplit(S, Dir, Name, Ext);
    JustExtension := Copy(Ext, 2, Length(Ext));
  {$ELSE}
    Name := ExtractFileName(S);
    If Pos('.', Name)>00 then
     Result := UpperCase(Copy(Name, Pos('.', Name)+1, 255)) else Result := '';
 {$ENDIF}
end; { func. JustExtension }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  NoExtension(S: String): String;
var Dir, Name, Ext: String;
begin
  {$IFNDEF WIN32}
    FSplit(S, Dir, Name, Ext);
    NoExtension := Dir + Name;
  {$ELSE}
     Name := ForceBack(ExtractFilePath(S)) + ExtractFileName(S);

     if Length(ExtractFileExt(Name)) > 0 then
       Delete(Name, (Length(Name) - Length(ExtractFileExt(Name))) + 1, 255);

     NoExtension := Name;
  {$ENDIF}
end; { func. NoExtension }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function JustName(Path: String): String;
var Dir,
    Name,
    Ext     : String;
    Counter : Byte;
begin
  {$IFDEF ELEUNIX}
    For Counter := 01 to length(path) do
     If Path[Counter]='\' then Path[Counter]:='/';
  {$ELSE}
    For Counter := 01 to length(path) do
     If Path[Counter]='/' then Path[Counter]:='\';
  {$ENDIF}

  {$IFNDEF WIN32}
    FSplit(Path, Dir, Name, Ext);
    JustName := Name+Ext;
  {$ELSE}
    JustName := ExtractFileName(Path);
  {$ENDIF}
End; { func. JustName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function  JustPath(S: String): String;                   { Returns just path }
var Dir, Name, Ext: String;
begin
  {$IFNDEF WIN32}
    FSplit(S, Dir, Name, Ext);
    JustPath := Dir;
  {$ELSE}
    JustPath := ExtractFilepath(S);
  {$ENDIF}
end; { func. JustPath }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function RdxName(S: String): String;
begin
  RdxName := NoExtension(s) + '.rdx';
end; { func. RdxName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function IsFtpUrl(const Path: String): Boolean;
begin
  if SupCase(Copy(Path, 1, 6)) = 'FTP://' then
    begin
      IsFtpUrl := true
    end
     else IsFtpUrl := false;
end; { func. IsFtpUrl }


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit StrPath }
