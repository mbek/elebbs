{!!! S-,R-,V-,I-,B-,F+}

{$IFNDEF Ver40}
  {$R-,O-,A-}
{$ENDIF}

{*********************************************************}
{*                     DPMI.PAS 1.00                     *}
{*        Copyright (c) TurboPower Software 1992.        *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit Dpmi;       {primitive routines for DPMI management}

interface

{-The following consts are used throughout Object Professional.  Your code
  is free to reference them, but they must *not* be changed.}
const
  DpmiInUse : Boolean = False;        {True if running in protected mode}
  ColorSele : Word = $B800;           {selector/segment for color video}
  MonoSele  : Word = $B000;           {selector/segment for mono video}
  BiosDataSele : Word = $0040;        {selector/segment for bios data area}
  BiosSele : Word = $F000;            {selector/segment for bios memory}


implementation

end.

