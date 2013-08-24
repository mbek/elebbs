(*
**
** Standard type definitions almost all scripts use
**
** Created: 21-Jul-2002
** Last update: 08-Aug-2002
**
** (c)2002 by Maarten Bekers
**
**
*)

type
  SmallWord      = Integer[2];
  SmallInt       = Integer[2];
  Longint        = Integer[4];
  Byte           = Integer[1];

  FlagType       = array[1..4] of Byte;
  MsgType        = Byte;
  MsgKindsType   = Byte;

  Time           = String[5];
  Date           = String[8];

  COMBINEDrecord = array[1..200] of SmallWord;
  MSGTOIDXrecord = String[35];


