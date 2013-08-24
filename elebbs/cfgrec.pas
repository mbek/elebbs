unit CfgRec;
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
{$IFNDEF MSDOS}
  {$H-}
{$ENDIF}
(*
**
** Config declaration EleBBS
**
** Copyright (c) 1996, 1997, 1998 by Maarten Bekers
**
** Created : 28-Mar-1998
** Last update : 28-Mar-1998
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF ELEUNIX}
  uses SysUtils;
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
   {$I struct.250}
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Type TimeType       = String[5];
     DateType       = String[8];
     DelStr         = String[250];

type CharSet = Set of Char;
     FillUp  = (Zero, Space);


{$IFDEF WIN32}                        { Use this to easily create an constant }
        Type NumReadType = Longint;
{$ENDIF}

{$IFDEF OS2}
        Type NumReadType = Longint;
{$ENDIF}

{$IFDEF GO32V2}
        Type NumreadType = longint;
{$ENDIF}

{$IFDEF MSDOS}
        Type NumReadType = Word;
{$ENDIF}

{$IFDEF ELEUNIX}
        Type NumreadType = Longint;
{$ENDIF}

Const
{$IFNDEF ELEUNIX}
  ReadMode      = 0;                         { Constants for System.FileMode }
  WriteMode     = 1;
  ReadWriteMode = 2;

  DenyAll       = $10;
  DenyWrite     = $20;
  DenyRead      = $30;
  DenyNone      = $40;
  Inheritance   = $80;
{$ELSE}
  {$WARNING *** SHARING MODES ARE NOT DEFINED YET !!! ***}

  ReadMode      = SysUtils.fmOpenRead;
  WriteMode     = SysUtils.fmOpenWrite;
  ReadWriteMode = SysUtils.fmOpenReadWrite;

  DenyAll       = 0;
  DenyWrite     = 0;
  DenyRead      = 0;
  DenyNone      = 0;
{$ENDIF}

type  MacroStr = String[255];


{-- to support MKMSG -----------------------------------------------------}
Type AddrType = Record                 {Used for Fido style addresses}
  Zone: SmallWord;
  Net: SmallWord;
  Node: SmallWord;
  Point: SmallWord;
  End;


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { CFGREC }
