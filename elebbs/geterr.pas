program GetErr;
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
** GETERR - GETERR program for EleBBS/W32 and EleBBS/GUI
**          gets the exitcode from an program and reads it.
**
** Copyright (c) 1996,97,98 by Maarten Bekers
**
** Created : 26-Dec-1998
** Last update : 26-Dec-1998
**
**
*)

uses Dos, Memory;

function SUpCase(S: String): String;
var Counter: Byte;
begin
  for counter := 01 to length(s) do
   s[counter]:=upcase(s[counter]);
  supcase:=s;
end; { func. SuPCase }

var F: File;
    S: String;
    W: Word;
    OldHeapEnd: Pointer;
    CmdLine: String;
    Path: String;
    Counter: Byte;
begin
  CmdLine := '';
  Path := ParamStr(2);
  for Counter := 03 to ParamCount do CmdLine := CmdLine + ParamStr(Counter) + #32;

  OldHeapEnd := HeapEnd;                                    { Save old HeapEnd }
  HeapEnd := HeapPtr;                           { Set the HeapEnd to Heap-Used }
  SetMemTop(HeapEnd);

  if Pos('.BAT', SUpCase(ParamStr(2))) > 00 then
   begin
     CmdLine := '/C ' + Path + #32 + CmdLine;
     Path := GetEnv('COMSPEC');
   end; { if }

  Exec(Path, Cmdline);                                          { Do Dos Shell }

  HeapEnd := OldHeapEnd;                                     { Restore heapend }
  SetMemTop(HeapEnd);
  W := DosExitCode;

  Assign(F, '$!$EXIT' + ParamStr(1) + '.E$$');
  FileMode := 2;
  {$i-}
    Rewrite(f, 1);
    BlockWrite(f, W, SizeOf(Word));
    Close(f);
  {$i+}
  if IOResult > 00 then ;
end.
