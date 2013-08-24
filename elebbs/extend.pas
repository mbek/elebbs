unit Extend;
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
** Extend the number of available filehandles
**
** Copyright (c) 1996 by Maarten Bekers
**
** Created : 22-Mar-1997
** Last update : 22-Mar-1997
**
** note: Not (c) by me, but is from the compuserve forum
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


{$IFDEF MSDOS}
Uses DOS;

Const MaxHandles = 255;                       { Maximum number of filehandles }

var Regs: Registers;
begin
  Regs.AH := $30;                                         { Check DOS version }
  MsDos(Regs);
  If Regs.AL < 03 then
    begin
      Writeln('File extension unit requires DOS 3.00 or higher..');
      Halt(255);
    end; { Dos-Version lower than 3.00 }

{$IFNDEF DPMI}

  If HeapOrg <> HeapPtr then                         { Check if heap is empty }
    begin
      WriteLn('Heap must be empty before file-extension unit initializes..');
      WriteLn;
      Halt(255);
    end; { Heap is not empty }

  HeapEnd := Ptr(Seg(HeapEnd^) - (MaxHandles DIV 8 + 01), Ofs(HeapEnd^));

                          { Determine how much memory is allocated to program }
  Regs.AH := $4A;   { Regs.BX will return how many paragraphs used by program }
  Regs.ES := PrefixSeg;
  Regs.BX := $FFFF;
  MsDos(Regs);


  Regs.AH := $4A;         { Set the program size to the allow for new handles }
  Regs.ES := PrefixSeg;
  Regs.BX := Regs.BX - (MaxHandles DIV 8 + 01);
  MsDos(Regs);


  If (Regs.FLAGS AND 01)=01 then { Error when a Block Size is not appropriate }
    begin
      Writeln('Runtime Error ',Regs.AX);
      Writeln('In the fileextension unit...');
      Halt(255);
    end; { Block size not correct }
{$ENDIF}

  Regs.AH := $67;                     { Allocate Space for Additional Handles }
  Regs.BX := MaxHandles;
  MsDos(Regs);
{$ENDIF}
end.
