unit Use32;

interface

{$IFDEF WIN32}
  {$DEFINE USE16}
{$ENDIF}

{$IFDEF USE16}
  {$UNDEF USE32}
{$ENDIF}

{$IFDEF USE32}
type
  Integer    = System.Longint;
  Word       = System.Longint;
const
  MaxInt     = MaxLongint;
{$ELSE}
Type
{$IFNDEF WIN32}
  SmallInt   = System.Integer;
{$ENDIF}
  SmallWord  = System.Word;
  Word       = SmallWord;
  Integer    = SmallInt;
{$ENDIF}
type
  PByte      = ^Byte;
  PWord      = ^Word;
  PLongint   = ^Longint;
  PSmallInt  = ^SmallInt;
{$IFNDEF WIN32}
  PSmallWord = ^SmallWord;
{$ENDIF}  

implementation

end.
