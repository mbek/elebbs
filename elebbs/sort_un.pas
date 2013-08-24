unit Sort_Un;
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
** QSort Routines for EleBBS
**
** Copyright (c) 1996 by Maarten Bekers
**
** Created : 30-Mar-1997
** Last update : 30-Mar-1997
**
** note: Routines are taken from swag!, written by Erez Amir
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Type
  _Compare  = Function(V1, V2: Pointer): Boolean;  { QuickSort Calls This }

Procedure QuickSort(Var Struct;      { array of any Type }
                    Num,             { Number of elements }
                    Size:Integer;    { Size of each element ( byte ) }
                    Comp:_Compare);

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
{$F+,R-,S-}
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure QuickSort(Var Struct;      { array of any Type }
                    Num,             { Number of elements }
                    Size:Integer;    { Size of each element ( byte ) }
                    Comp:_Compare);
Const MaxQsortCount = 240;

var Temp      : Pointer;
    StructBase: Array[0..0] of Byte ABSOLUTE Struct;
    QSortCount: Longint;

Function VLoc(N: Integer):Pointer;   { Note that no range check is performed! }
begin
  {$R-}     { addr }
    VLoc := @(StructBase[N * Size]);
  {$R+}
end; { func. VLoc }

procedure Swap(N1, N2: Integer);                          { Swap two elements }
begin
  Move(VLoc(n1)^, Temp^,     Size);
  Move(VLoc(n2)^, VLoc(n1)^, Size);
  Move(Temp^,     VLoc(n2)^, Size);
end; { proc. Swap }


procedure Qsort(l,r:Integer);                            { Quick sort routine }
var I, J : Integer;
    Pivot: Pointer;
begin
  Inc(QSortCount);
  i := l;
  j := r;


  GetMem(Pivot,Size);                               { Hopefully, the midpoint }
  Move(Vloc( (L+R) DIV 2)^, Pivot^, Size);

  repeat
    While (Comp(Pivot, Vloc(i))) AND (I <= NUM) do Inc(i);
    While (Comp(Vloc(J), pivot)) AND (J <= NUM) do Dec(j);

    if I<=J then
        begin
          Swap(I, J);
          Inc(I);
          Dec(J);
        end; { if }
  Until I > J;

  if QSortCount < MaxQSortCount then
    begin
      if J > L then Qsort(L,J);                            { Recursive Call }
      if I < R then Qsort(I,R);
    end; { if }

  FreeMem(Pivot,Size);
  Dec(QSortCount);
end; { proc. QSort }

begin
  GetMem(Temp, Size);                                 { Temp is used for swap }
  QSortCount := 0;

  if Num>01 then
    QSort(0, Num - 01);

  FreeMem(Temp, Size);
end; { proc. QuickSort }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit Sort_Un }
