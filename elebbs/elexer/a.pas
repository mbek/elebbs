program test;
{$R-}
uses crt, Sysutils;

type
  pNulIntArray = Array[0..0] of Pointer;
  dynArrayType = record
                    ArrayType  : Integer;   { 0 = child, 1 = parent }
                    ChildCount : Integer;      { Number of children }
                    ArID       : Integer;

                    ChildAr    : ^pNulIntArray;
                    Contents   : ^pNulIntArray;
                 end; { record }


var b: pointer;
    i: pointer;

    id: integer;

    x,y,z: integer;

function getid: integer;
begin
  getid := id;
  id := id + 1;
end;

procedure InitChildAr(var B: Pointer; Dims: Longint);
type
  ar = dynArrayType;

var CurChild : Pointer;
begin
  CurChild := b;

  {-- first initialize the base child if necessary ---------------------------}
  if CurChild = nil then
    begin
      {-- alllocate memory for the child -------------------------------------}
      GetMem(CurChild, SizeOf(dynArrayType));

      {-- intiailize the pointer ---------------------------------------------}
      Ar(CurChild^).ArrayType := 0;
      Ar(CurChild^).ChildCount := 0;
      Ar(CurChild^).ArID := GetID;
      Ar(CurChild^).ChildAr := nil;
    end; { if }

  {-- and return the value ---------------------------------------------------}
  B := CurChild;
end; { proc. InitChildAr }


function GetDimensionPtr(Dims: Array of Longint): Pointer;
type
  ar = dynArrayType;

var Counter   : Integer;
    CurChild  : dynArrayType;
begin
  {-- make sure the base pointer we have gotten isnt nil ---------------------}
  if B = nil then
    begin
      WriteLn('OHMY!');
      Halt;
    end; { if }

  {-- now assign the children and walk it ------------------------------------}
  CurChild := Ar(B^);

  {-- iterate through the elements -------------------------------------------}
  Counter := 0;
  while Counter < High(Dims) do
    begin
      {-- make sure the appropriate amount of children exist -----------------}
      if CurChild.ChildCount < Dims[Counter] then
        begin
          {-- retrieve data --------------------------------------------------}
          InitChildAr(CurChild, Dims[Counter]);
        end; { if }

      {-- and go for the next pointer ----------------------------------------}
      CurChild := Ar(CurChild.ChildAr^[Dims[Counter]]^);

      {-- and save the parent ------------------------------------------------}
      SaveParent := Pointer(CurChild);
    end; { for }

  GetDimensionPtr := CurChild;
end; { func. GetDimensionPtr }

type
  ar = dynArrayType;

begin
  clrscr;
  id := 0;

  InitChild(B, 1);
  writeln('initialized');

writeln('B = ', format('%p', [b]));
writeln('***********************************');

  i := GetDimensionPtr([1,1]);

writeln('(01) = ', Format('%p', [i]));

  i := GetDimensionPtr([1,1]);

writeln('(02) = ', Format('%p', [i]));

(*
  for x := 1 to 100 do
    for y := 1 to 100 do
      for z := 1 to 100 do
        begin
          i := GetDimensionPtr([x, y, z]);
          Longint(Ar(i^).StackInf) := x*y*z;

          i := GetDimensionPtr([x, y, z]);
          if Longint(Ar(i^).StackInf) <> x*y*z then
            begin
                writeln(^g,'error!!!!: ', x, ' / ', y, ' / ', z);
                writeln('errror!!!!!: ', format('%p', [i]));
                writeln('blah: ', x*y*z, ' -> ', longint(ar(i^).stackinf));
            end;
        end; { for }
*)
end.
