unit WinTitle;
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
** Set the window title routines for EleBBS
**
** Copyright (c) 1998-1999 by Maarten Bekers
**
** Created : 24-Jan-1999
** Last update : 24-Jan-1999
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SetWindowTitle(S: String);
procedure RestoreTitle;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF MSDOS}
uses Multi, Dos;
{$ENDIF}

{$IFDEF GO32V2}
uses Multi, Go32, Dos;
{$ENDIF}

{$IFDEF WIN32}
uses Multi, Windows;
{$ENDIF}

{$IFDEF OS2}
uses Multi, Os2Def, Os2Base, OS2PmApi, SysUtils;
{$ENDIF}

Const
  SaveTitleStr : String = '';   { we can only save the title once per program }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure RestoreTitle;
begin
  if SaveTitleStr <> '' then
    SetWindowTitle(SaveTitleStr);
  SAveTitleStr := '';
end; { proc. RestoreTitle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SetWindowTitle(S: String);
{$IFDEF MSDOS}
var Regs: Registers;
    Tmp : Array[0..79] of Char;
{$ENDIF}

{$IFDEF GO32V2}
var Regs     : Registers;
    Tmp      : Array[0..79] of Char;
    Selector : Word;
    Segment  : Word;

procedure DosAlloc(var Selector: Word; var SegMent: Word; Size: Longint);
var Res: Longint;
begin
  {$IFDEF GO32V2}
    Res := Global_DOS_Alloc(Size);
    Selector := Word(Res);

    Segment := Word(RES SHR 16);
  {$ENDIF}
end; { proc. DosAlloc }

procedure DosFree(Selector: Word);
begin
  {$IFDEF GO32V2}
    Global_DOS_Free(Selector);
  {$ENDIF}
end; { proc. DosFree }

{$ENDIF}

{$IFDEF WIN32}
var TempLen: Word;
{$ENDIF}

{$IFDEF OS2}
var ReturnCode : Longint;
    hSw        : hSwitch;
    SwData     : SwCntrl;

function _GetPid: Longint;
var MyPIB : pPIB;
    MyTIB : pTIB;
begin
  New(MyTIB);
  New(MyPIB);

  DosGetInfoBlocks(MyTIB, MyPIB);
  Result := MyPib^.PIB_ulPID;
end; { func. _Getpid }
{$ENDIF}
begin
  {$IFDEF WIN32}
     TempLen := GetConsoleTitle(@SaveTitleStr[1], SizeOf(SaveTitleStr) - 1);
     SetLength(SaveTitleStr, TempLen);

     S := S + #00;
     SetConsoleTitle(@S[1]);
  {$ENDIF}

  {$IFDEF OS2}
     hSw := WinQuerySwitchHandle(00, _GetPid);
     ReturnCode := WinQuerySwitchEntry(hSw, @SwData);

     if ReturnCode = 00 then
       begin
         SaveTitleStr := StrPas(SwData.szSwTitle);

         StrPcopy(SwData.szSwTitle, S);
         ReturnCode := WinChangeSwitchEntry(hSw, @SwData);
       end; { if }
  {$ENDIF}

  {$IFDEF MSDOS}
    FillChar(Regs, SizeOf(Registers), 00);

    FillChar(Tmp, SizeOf(Tmp), #00);
    if Length(s) > 78 then S[0] := Chr(78);
    S := S + #00;
    Move(S[1], Tmp[0], Length(s));

    if MultiTasker = WindowsTask then
      begin
        Regs.AX := $168E;
        Regs.DX := $0000;
        Regs.ES := Seg(Tmp);
        Regs.DI := Ofs(Tmp);

        Intr($2F, regs);
      end; { if }

    if MultiTasker = OS2 then
      begin
        Regs.AH := $64;
        Regs.BX := $0000;
        Regs.CX := $636C;
        Regs.DX := $0001;
        Regs.ES := Seg(Tmp);
        Regs.DI := Ofs(Tmp);

        Intr($21, regs);
      end; { if }
  {$ENDIF}

  {$IFDEF GO32V2}
    FillChar(Regs, SizeOf(Registers), 00);

    FillChar(Tmp, SizeOf(Tmp), #00);
    if Length(s) > 78 then S[0] := Chr(78);
    S := S + #00;
    Move(S[1], Tmp[0], Length(s));

    if MultiTasker = WindowsTask then
      begin
        DosAlloc(Selector, Segment, SizeOf(Tmp));
        if Int31Error <> 0 then EXIT;
        DosmemPut(Segment, 0, Tmp, SizeOf(Tmp));

        Regs.AX := $168E;
        Regs.DX := $0000;
        Regs.ES := Segment;
        Regs.DI := 0;

        Intr($2F, regs);

        DosMemGet(Segment, 0, Tmp, SizeOf(Tmp));
        DosFree(Selector);
      end; { if }

    if MultiTasker = OS2 then
      begin
        DosAlloc(Selector, Segment, SizeOf(Tmp));
        if Int31Error <> 0 then EXIT;
        DosmemPut(Segment, 0, Tmp, SizeOf(Tmp));

        Regs.AH := $64;
        Regs.BX := $0000;
        Regs.CX := $636C;
        Regs.DX := $0001;
        Regs.ES := Segment;
        Regs.DI := 0;

        Intr($21, regs);

        DosMemGet(Segment, 0, Tmp, SizeOf(Tmp));
        DosFree(Selector);
      end; { if }
  {$ENDIF}
end; { proc. SetWindowTitle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit WINTITLE }
