(**
  unit HashMap.pas
  HashMap data structure with strings as keys and pointers as values.

  This class mimics java.util.HashMap.

  Original copyright (c)2001 by Timon Christl (homepage: http://www.christltimon.de/ )

  Copyright (c) 2002 Timon Christl and Barend Garvelink

  Modified Feb 2002 by Barend Garvelink (degarf@myrealbox.com)
  + Documented each function
  + Added ContainsKey() function
  + Added IsEmpty() function
  + Added Size() function
  + Added KeySet() function
  * Put() now throws an exception if a duplicate key is entered
  - Removed the ForEach procedure, which would let you alter the -KEYS- in the map.


 *** IMPORTANT ***
  Unlike Java, Delphi has no garbage collection. This class therefore has a
  high potential for memory leaks. It is your own responsibility to properly
  dispose whatever values you store in this class. For this reason I did not
  include a clear() procedure. You'll have to iterate the map to dispose of
  the values anyway, so there is no point.

 *** DISCLAIMER ***
  This class comes with no warranty of any kind. You are free to use it in your
  own programs in its original state, or modified to your tastes. Neiter Timon nor
  Barend can be held responsible in any way for problems, crashes or unforeseen
  results in using this class. Please include the original authors' names if you
  redistribute a modified version.

**)

{$R-}
unit HashMap;

interface

uses SysUtils, Classes;

type
  PHashBucket  = ^THashBucket;
  THashBucket  = record
    Next       : PHashBucket; //collision list
    Key        : ansistring;
    Value      : longint;
  end;

  PHashBuckets = ^THashBuckets;
  THashBuckets = array[0..0] of PHashBucket;

  PObject      = ^TObject;

  DuplicateKeyException = class( Exception );

  THashMap = class
    constructor Create(Capacity : Cardinal);
    destructor  Destroy; override;
    procedure   Put(Key : ansistring; Value : longint);
    function    Get(Key : ansistring): longint;
    procedure   Remove(Key : ansistring);
    function    ContainsKey(Key : ansistring): boolean;
    function    IsEmpty: boolean;
    function    Size: Cardinal;
  private
    FCapacity   : Cardinal;
    FBuckets    : PHashBuckets;
    Count       : Cardinal;
    function    HashCode(s:ansistring):Cardinal;
  end;

implementation

{**
 * Initializes a new THashMap.
 * Capacity:   Specifiy the amount of buckets in the HashMap. For best
 *             performance, use a prime number slightly larger than the
 *             expected amount of mappings.
 *}
constructor THashMap.Create(Capacity:Cardinal);
var
  i:Cardinal;
begin
  inherited Create;
  Count := 0;

  FCapacity:=Capacity;
  getmem(FBuckets,sizeof(THashBucket)*FCapacity);
  for i:=0 to FCapacity-1 do
  begin
    FBuckets^[i]:=nil;
  end;
end;

{**
 * Deconstructs the THashMap instance and frees the allocated memory.
 *
 * Note the risk of a memory leak: this function does not free the objects
 * referenced by the Value part of the mappings. You should manually dispose
 * of the contents of the map before destroying it.
 *}
destructor THashMap.Destroy;
var
  i:Cardinal;
  P,T:PhashBucket;
begin
  for i:=0 to FCapacity-1 do
  begin
    P:=FBuckets^[i];
    while P<>nil do
    begin
      T:=P;
      P:=P^.Next;
      dispose(T);
    end;
  end;
  freemem(FBuckets);

  {$IFNDEF VirtualPascal}
    inherited;
  {$ENDIF}
end;

{**
 * Returns true if a given key exists in the map
 * Key:   The key to search for.
 *}
function THashMap.ContainsKey(Key:ansistring):boolean;
var
  i:Cardinal;
  P:PHashBucket;
begin
  result := false;

  i := HashCode(Key);
  P:=FBuckets^[i];
  while P<>nil do
  begin
  if (AnsiCompareStr(P^.Key,Key)=0) then
    result := true;
     P:=P^.Next;
  if result then break;
  end;

end;

{**
 * Returns true if this map contains no key-value mappings.
 *}
function THashMap.IsEmpty:boolean;
begin
  Result := (count = 0);
end;

{**
 * Returns the number of Key-Value mappings in this map
 *}
function THashMap.Size:Cardinal;
begin
  result := count;
end;

{**
 * Puts an item in the HashMap
 * Key:      String. Identifier for the object you're storing.
 * Value:    Longint. A longint to your "payload" data.
 *}
procedure THashMap.Put(Key:ansistring;Value:longint);
var
  P:PHashBucket;
  Code:Cardinal;
begin
  Code:=HashCode(Key);

  {check for duplicate keys (added, BarendG)}
  P:=FBuckets^[Code];
  while P<>nil do
  begin
    if (AnsiCompareStr(P^.Key,Key)=0) then
      raise DuplicateKeyException.CreateFmt('Key "%s" already exists.',[Key])
    else
      P:=P^.Next;
  end;

  new(P);
  P^.Next:=FBuckets^[Code];
  P^.Key:=Key;
  P^.Value:=Value;

  FBuckets^[Code]:=P;
  inc(count);
end;

{**
 * Returns the value longint for the key specified. Returns
 * nil if no item exists by that key.
 * Key:     String. The key to fetch.
 *}
function THashMap.Get(Key:ansistring):longint;
var
  P:PHashBucket;
  Code:Cardinal;
begin
  Code:=HashCode(Key);

  P:=FBuckets^[Code];
  while (P<>nil) and (AnsiCompareStr(P^.Key,Key)<>0) do
    P:=P^.Next;

  if P<>nil then
    result:=P^.Value
  else
    result:=-1; { was: nil }
end;

{**
 * Removes an item from the map identified by the key specified.
 * Does nothing if the key doesn't exist.
 * Key:       String. The key to remove.
 *}
procedure THashMap.Remove(Key:ansistring);
var
  P,PP:PHashBucket;
  Code:Cardinal;
begin
  Code:=HashCode(Key);

  PP:=nil;
  P:=FBuckets^[Code];
  while (P<>nil) and (AnsiCompareStr(P^.Key,Key)<>0) do
  begin
    PP:=P;
    P:=P^.Next;
  end;

  if P<>nil then
  begin
    if PP=nil then
      FBuckets^[Code]:=FBuckets^[Code]^.Next
    else
      PP^.Next:=P^.Next;
    dispose(P);
    dec(count);
  end;
end;

{**
 * Generate a hashcode for the key specified.
 *}
function THashMap.HashCode(s:ansistring):Cardinal;
var
  i:Cardinal;
  Code:cardinal;
begin
  Code:=0;

  for i:=1 to length(s) do
    Code:=(Code*256+Cardinal(ord(s[i]))) mod FCapacity;

  result:=Code;
end;

end.
