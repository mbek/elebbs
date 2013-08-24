{
 From : Hans Siemons                2:285/705.10            Tue 23 Feb 93 01:04
 To   : Mark Schipper
 Subj : geldig fido adres
}

Unit FidoAddr;
{$I compiler.inc}

(*
** Converts fidonet adresses from/to string
**
** By Hans Siemons
**
** Released to the public domain
*)

interface

{$IFNDEF VirtualPascal}
uses Use32;
{$ENDIF}

Function  FVal (S : String) : LongInt; {Convert string to Integer}
Function  FStr (N : LongInt) : String; {Convert integer to string}

Procedure AddrToString(var s:String;zone,net,node,point:Word);
Procedure StringToAddr(var s:String;var zone,net,node,point:SmallWord);

implementation

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  FVal (S : String) : LongInt; {Convert string to Integer}

Var
{$IFNDEF Win32}
  Code        : Integer;
{$ELSE}
  Code        : Longint;
{$ENDIF}
  Number      : LongInt;

Begin { FVal }
  Val (S,Number,Code);
  If Code<>0 then
    Number:=0;
  FVal:=Number;
End;  { FVal }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  FStr (N : LongInt) : String; {Convert integer to string}

Var
  Temp        : String;

Begin { FStr }
  Str(n,temp);
  FStr:=Temp;
End;  { FStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure AddrToString(var s:String;zone,net,node,point:Word);
{Convert address to string}
Begin
  s:=FStr(Zone)+':'+FStr(net)+'/'+Fstr(Node);
  if point<>0 then s:=s+'.'+FStr(Point);
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure StringToAddr(var s:String;var zone,net,node,point:SmallWord);
{Convert string to address}

type nexttype=(nnet,nnode,npoint);

var pos:Integer;
    l:String;
    n:NextType;

Begin
  l:='';n:=NNode; {Next expected is NODE#}
  if s='' then {Empty string? Exit with default address}
  begin
    AddrToString(s,Zone,Net,Node,Point);
    exit;
  end;

  for pos:=1 to length(s) do
  begin
    if s[pos] in ['0','1','2','3','4','5','6','7','8','9'] then
      begin
        l:=l+s[pos]; {While # add to L}
      end else
      if s[pos]=':' then {:? L=ZONE}
        begin {Zone}
          if l<>'' then begin
                          Zone:=FVal(l);
                          l:='';
                        end;
          N:=NNet; {Expect Net# next}
          Point:=0; {Clear point#}
        end else
      if s[pos]='/' then {/? L=NET}
        begin {Net}
          if l<>'' then begin
                          Net:=FVal(l);
                          l:='';
                        end;
          n:=NNode; {Next expected is node}
          Point:=0; {Clear Point#}
        end else
      if s[pos]='.' then {Point?}
        begin
          if l<>'' then begin
                          Node:=FVal(l); {Node# before it?}
                          l:='';
                          Point:=0;
                        end;
          n:=NPoint; {Next expected is POINT#}
        end;
  end; {For}
  if n=nnet then
    begin
      Net:=FVal(l);
      Point:=0;
    end else
  if n=nnode then
    Begin
      Node:=FVal(l);
      Point:=0;
    end else
    Point:=FVal(l);
  AddrToString(s,zone,net,node,point);
end;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

End.
