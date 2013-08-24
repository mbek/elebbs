unit Flags;
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
** FLAGS routines for EleBBS
**
** Copyright (c) 1997 by Maarten Bekers
**
** Created : 01-Nov-1996
** Last update : 01-Apr-1998
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
uses CfgRec, Bitwise, LongStr, ElLog_U;

procedure RaSetFlags (Var Fl :FlagType;Which : String);         { Set flags }
procedure RaToggleFlags (Var Fl : FlagType;Witch : String);   { Toggle flags }
procedure MenuChangeFlags(MiscData: String; var Flags: FlagType); { Menu-Change flags }
procedure RaResetFlags (Var Fl : FlagType;Which : String);     { Reset flags }
function  RaCheckFlags (Fl : FlagType;Which : String) : Boolean; { Chk flags }
function  RaCmpFlags (FlUser,Fl : FlagType) : Boolean;       { Compare flags }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  RaCheckFlags (Fl : FlagType;Which : String) : Boolean; { Chk flags }
var Counter: Byte;
{$IFNDEF FPC}
{$IFNDEF WIN32}
 {$IFNDEF OS2}
    Result: Boolean;
 {$ENDIF}
{$ENDIF}
{$ENDIF}
begin
  Result := True;
  Counter := 01;

  while (Counter < Length(Which)) do
    begin
      Inc(Counter);

      case UpCase(Which[Counter - 1]) of
        'A' : If NOT ReadBit(Fl[1], FVal(Which[Counter])-1) then Result:=False;
        'B' : If NOT ReadBit(Fl[2], FVal(Which[Counter])-1) then Result:=False;
        'C' : If NOT ReadBit(Fl[3], FVal(Which[Counter])-1) then Result:=False;
        'D' : If NOT ReadBit(Fl[4], FVal(Which[Counter])-1) then Result:=False;
      end; { Case }

      Inc(Counter);
   end; { For Teller }

  RaCheckFlags := Result;
end; { func. RaCheckFlags }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  RaCmpFlags (FlUser,Fl : FlagType) : Boolean;       { Compare flags }
var Column, Row: Byte;
{$IFNDEF FPC}
{$IFNDEF WIN32}
 {$IFNDEF OS2}
    Result: Boolean;
 {$ENDIF}
{$ENDIF}
{$ENDIF}
begin
  Result := True;

  For Column:=01 to 04 do
   For Row:=00 to 07 do
    If (ReadBit(Fl[Column], Row)) AND
     (NOT ReadBit(FlUser[Column], Row)) then Result:=False;

  RaCmpFlags := Result;
end; { func. RaCmpFlags }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure RaResetFlags (Var Fl : FlagType;Which : String);     { Reset flags }
var Counter: Longint;
begin
  Counter := 01;
  While Counter <= Length(Which) do
    begin
      Case UpCase(Which[Counter]) of
         'A' : ClearBit(Fl[1], FVal(Which[Counter + 1]) - 1);
         'B' : ClearBit(Fl[2], FVal(Which[Counter + 1]) - 1);
         'C' : ClearBit(Fl[3], FVal(Which[Counter + 1]) - 1);
         'D' : ClearBit(Fl[4], FVal(Which[Counter + 1]) - 1);
      end; { Case }

      if (UpCase(Which[Counter]) in ['A'..'D']) then
        RaLog('>', 'Flag '+Copy(Which, Counter, 2)+' set OFF');

      Inc(Counter, 2);
    end; { for Teller }

end; { proc. RaResetFlags }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure RaSetFlags (Var Fl : FlagType;Which : String);         { Set flags }
var Counter: Longint;
begin
  Counter := 01;
  While Counter <= Length(Which) do
    begin
      Case UpCase(Which[Counter]) of
         'A' : SetBit(Fl[1], FVal(Which[Counter + 1]) - 1);
         'B' : SetBit(Fl[2], FVal(Which[Counter + 1]) - 1);
         'C' : SetBit(Fl[3], FVal(Which[Counter + 1]) - 1);
         'D' : SetBit(Fl[4], FVal(Which[Counter + 1]) - 1);
      end; { Case }

      if (UpCase(Which[Counter]) in ['A'..'D']) then
        RaLog('>', 'Flag '+Copy(Which, Counter, 2)+' set ON');

       Inc(Counter, 2);
    end; { for Te }
end; { proc. RaSetFlags }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure MenuChangeFlags(MiscData: String; var Flags: FlagType);
var Temp  : String;
    Teller: Byte;
begin;
  Teller:=00;

  repeat;
    Inc(Teller);
    Temp := Copy(MiscData, 1, 3);
    Delete(MiscData, 1, 4); { Space }

    Case Temp[3] of
       '+' : RASetFlags(Flags, Temp[1]+Temp[2]);
       '-' : RAReSetFlags(Flags, Temp[1]+Temp[2]);
       '*' : RAToggleFlags(Flags, Temp[1]+Temp[2]);
    end; { Case }
  until (Length(MiscData)=0) Or (Teller>200);
end; { MenuChangeFlags }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure RaToggleFlags (Var Fl : FlagType;Witch : String);   { Toggle flags }
var Teller: Byte;
begin
  For Teller:=01 to Length(Witch) do
    begin
      case Upcase(Witch[Teller]) of
        'A' : If ReadBit(Fl[1], FVal(Witch[Teller+1])-1) then ClearBit(Fl[1], FVal(Witch[Teller+1])-1)
               else SetBit(Fl[1], FVal(Witch[Teller+1])-1);
        'B' : If ReadBit(Fl[2], FVal(Witch[Teller+1])-1) then ClearBit(Fl[2], FVal(Witch[Teller+1])-1)
               else SetBit(Fl[2], FVal(Witch[Teller+1])-1);
        'C' : If ReadBit(Fl[3], FVal(Witch[Teller+1])-1) then ClearBit(Fl[3], FVal(Witch[Teller+1])-1)
               else SetBit(Fl[3], FVal(Witch[Teller+1])-1);
        'D' : If ReadBit(Fl[4], FVal(Witch[Teller+1])-1) then ClearBit(Fl[4], FVal(Witch[Teller+1])-1)
               else SetBit(Fl[4], FVal(Witch[Teller+1])-1);
      end; { Case }
    end; { for Teller }
end; { proc. RaToggleFlags }

(*+-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
