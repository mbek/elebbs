unit RecDif;
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
** RecordDifferent Routines for EleBBS
**
** Copyright (c) 1996, 1997 by Maarten Bekers
**
** Created : 08-Sep-1996
** Last update : 15-Nov-1997
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function RecordsDifferent(Var R1, R2; Size: Word) : Boolean;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Debug_U, Global{$IFDEF WITH_FULL}, StUtils{$ENDIF}, LongStr,
     ObjDec;

Function RecordsDifferent(Var R1, R2; Size: Word) : Boolean;
{ Update QSetup's RecordsDifferent also!!! }
Type ByteArray = Array[0..65534] of Byte;

var ByteArray_1 : ByteArray ABSOLUTE R1;
    ByteArray_2 : ByteArray ABSOLUTE R2;
    Counter     : Word;
begin
  RecordsDifferent := FALSE;
  {$IFDEF WITH_DEBUG}
    {$IFDEF WITH_FULL}
      DebugObj.DebugLog(logSupport, 'Comparing 2 records, Size='+FStr(Size));
    {$ENDIF}
  {$ENDIF}

  For Counter := 0 to (Size - 01) do
   If ByteArray_1[Counter] <> ByteArray_2[Counter] then
    begin
      {$IFDEF WITH_DEBUG}
       {$IFDEF WITH_FULL}
         DebugObj.DebugLog(logSupport, 'Difference between 2 recs at byte: '+FStr(Counter));
       {$ENDIF}
      {$ENDIF}

      RecordsDifferent := True;
      break;
    end; { for }

end; { RecordsDifferent }

end.

