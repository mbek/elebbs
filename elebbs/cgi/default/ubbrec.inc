(*
** UBB code parser
** Smiley parser
**
** Header file for EleXer
**
** Written: 14-Jul-2002
**
**)

var
  Smiley_Pic     : Array[1..9] of String;                { Record of smileys }
  Smiley_Code    : Array[1..9] of String;             { Codes to convet from }
  TableCount     : Integer;                   { Total number of Smileys found }

  ubb_AllowColors: Boolean;                        { Allow [color=xx] in text }
  ubb_AllowImg   : Boolean;                                 { allow [img] tag }
  ubb_AllowHtml  : Boolean;                                   { Allow < and > }

