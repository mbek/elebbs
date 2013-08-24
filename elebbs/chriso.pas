unit CHRISO;
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
** CHRISO, Charset ISO (FSC-0054)
**
** Copyright (c) 1998-1999 by Maarten Bekers
**
** Created : 16-Aug-1999
** Last update : 16-Aug-1999
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF VirtualPascal}
  uses Use32;
{$ENDIF}

type csDatHeader = record
                      MachineID: Byte;
                      Filler   : Array[0..2] of Byte;
                      CharSet  : Array[0..7] of Char;
                   end; { csDatHeader }

type csLookUpTable = record
                        Identification : Longint;
                        ModuleVer      : SmallWord;
                        Level          : SmallWord;
                        Reserved       : Array[0..7] of Byte;
                        FromCharSet    : Array[0..7] of Char;
                        ToCharSet      : Array[0..7] of Char;
                        LookupTable    : Array[0..127] of record
                                                            Byte1,
                                                            Byte2: Byte;
                                                          end; { record }
                     end; { record }


function ReadMap(FName: String; var Table: csLookupTable;
                 FromCS, ToCS: String): Boolean;
function ConvertToLatin(CharSet: String; var S: String): Boolean;
function ConvertToIBMPC(CharSet: String; var S: String): Boolean;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ConvertString(var Table: csLookupTable; var S: String);
var Counter: Longint;
    DestStr: String;
begin
  DestStr := '';

  for Counter := 01 to Length(s) do
    begin
      if Ord(S[Counter]) > 127 then
        begin
          if Table.LookupTable[Ord(S[Counter]) - 128].Byte1 in [0..1] then
            begin
              { Second byte = 0: no output }
              if Table.LookupTable[Ord(S[Counter]) - 128].Byte2 = 0 then ;
              if Table.LookUpTable[Ord(S[Counter]) - 128].Byte2 > 0 then
                DestStr := DestStr + Chr(Table.LookUpTable[Ord(S[Counter]) - 128].byte2);
            end; { if }

          if Table.LookUpTable[Ord(S[Counter]) -  128].Byte1 > 31 then
            begin
              if Table.LookUpTable[Ord(S[Counter]) - 128].Byte2 > 31 then
                DestStr := DestStr + Chr(Table.LookUpTable[Ord(S[Counter]) - 128].Byte1) +
                                     Chr(Table.LookUpTable[Ord(S[Counter]) - 128].Byte2);
            end; { if }
        end
          else DestStr := DestStr + S[Counter];
    end; { for }

  S := DestStr;
end; { proc. ConvertChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FromAsciiZ(var Temp: Array of Char): String;
var Counter: Longint;
    TempStr: String;
begin
  Counter := 0;
  TempStr := '';

  While Counter < 8 do
    begin
      if Temp[Counter] <> #0 then
        TempStr := TempStr + Temp[Counter]
          else Break;

      Inc(Counter);
    end; { while }

  FromAsciiZ := TempStr;
end; { func. FromAsciiZ }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ReadMap(FName: String; var Table: csLookupTable;
                 FromCS, ToCS: String): Boolean;

var Char_F  : File;
    DatHdr  : csDatHeader;
begin
  ReadMap := false;
  FillChar(Table, SizeOf(Table), #00);

  Assign(Char_F, FName);
  {$i-}
    Reset(Char_F, 1);
    BlockRead(Char_F, DatHdr, SizeOf(DatHdr));
  {$i+}
  if IoResult > 0 then EXIT;

  While NOT Eof(Char_F) do
    begin
      FillChar(Table, SizeOf(Table), #00);

      {$i-} BlockRead(Char_F, Table, SizeOf(Table)); {$i+}
      if IoResult > 0 then BREAK;

      if FromAsciiz(Table.FromCharSet) = FromCS then
       if FromAsciiz(Table.ToCharSet) = ToCS then
         begin
           ReadMap := true;
           BREAK;
         end; { if }

    end; { while }

  {$i-} Close(Char_F); {$i+}
  if IoResult > 0 then ;
end; { proc. ReadMap }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ConvertToLatin(CharSet: String; var S: String): Boolean;
var Table: csLookUpTable;
begin
  ConvertToLatin := false;

  if ReadMap('writmaps.dat', Table, CharSet, 'LATIN-1') then
    begin
      ConvertString(Table, S);
      ConvertToLatin := true;
    end; { if }
end; { proc. ConvertToLatin }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ConvertToIBMPC(CharSet: String; var S: String): Boolean;
var Table: csLookUpTable;
begin
  ConvertToIbmPC := false;

  if ReadMap('readmaps.dat', table, CharSet, 'IBMPC') then
    begin
      ConvertString(Table, S);
      ConvertToIbmPc := true;
    end; { if }

end; { func. ConvertToIBMPC }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { CHRISO }
