unit CRT;
{$I COMPILER.INC}
(*
**
** Dummy CRT unit for Delphi only!
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

Const
  Black         = 0;
  Blue          = 1;
  Green         = 2;
  Cyan          = 3;
  Red           = 4;
  Magenta       = 5;
  Brown         = 6;
  LightGray     = 7;

{ Foreground color constants }

  DarkGray      = 8;
  LightBlue     = 9;
  LightGreen    = 10;
  LightCyan     = 11;
  LightRed      = 12;
  LightMagenta  = 13;
  Yellow        = 14;
  White         = 15;

{ Add-in for blinking }

  Blink         = 128;

var WindMin,
    WindMax   : Word;
    CheckBreak: Boolean;

{$IFNDEF DELCRT}
procedure Clrscr;
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ClrScr;
begin
end; { proc. ClrScr }

initialization
  WindMin:=((01 - 1) SHL 8) + (01 - 1);
  WindMax:=((25 - 1) SHL 8) + (80 - 1);

end. { unit CRT }
