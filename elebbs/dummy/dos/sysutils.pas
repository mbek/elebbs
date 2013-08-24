unit Sysutils;
{$I COMPILER.INC}
(*
**
** Dummy SYSUTIILS unit for DOS only!
**
** Copyright (c) 1999 by Maarten Bekers
**
** Created : 23-Jan-1999
** Last update : 23-Jan-1999
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

type THandle = Longint;

procedure SetLength(var S: String; Len: Byte);

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SetLength(var S: String; Len: Byte);
begin
  S[0] := Chr(Len);
end; { proc. SetLength }

end. { unit SYSUTILS }
