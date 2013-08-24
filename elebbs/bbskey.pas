unit BBSKey;
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
** Key generator routines for EleBBS
**
** Copyright (c) 1996, 1997 by Maarten Bekers
**
** Created : 28-Dec-1997
** Last update : 28-Dec-1997
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CheckRegistered;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Global, StUtils, Debug_U, JDates, ElLog_U, StrPath,
      LongStr, Dos, Multi, GenDos, RAL, Genfile, ObjDec,
      SysUtils, FileObj, CfgRec
      {$IFDEF WITH_FULL}
        ,Input_u
      {$ENDIF}

      {$IFDEF WINGUI}
        ,Dialogs
      {$ENDIF} ;

procedure CheckRegistered;
begin
  RunsRegistered := TRUE;
end; { proc. CheckRegistered }

end.
