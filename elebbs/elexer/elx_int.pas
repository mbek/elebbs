UNIT ELX_INT;
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
{$I Compiler.Inc}
{$R-,S-,W-}
(*
**
** Interpreter module for Elexer
**
** Copyright (c) 2000-2003 by Maarten Bekers
**
** Created : 26-Oct-2000
** Last update : 23-Mar-2003
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses FileObj,
      Elx_Glob

      ,jdates
      {$IFDEF WITH_FULL}
       ,ElLog_U
      {$ENDIF};

{ ---------------------------------------------------------------- }
{ Block: a Procedure, Function or record declaration               }
{ ---------------------------------------------------------------- }
{ BlockTable: BlockTable is a array of all Blocks in the program   }
{             Its used as following:                               }
{             BlockTable[1]   - First block ("program")            }
{             BlockTable[2-xx]- the rest of the blocks.            }
{                               all actual code is in the > 1    }
{                               blocks                             }
{ ---------------------------------------------------------------- }
{ Level: Level is the level where a procedure/variable is located. }
{        All procs directly under "program" are all level 1,       }
{        procedures beneath another procedures is another level,   }
{        for example:                                              }
{         procedure hallo1;                     <- Level 1         }
{          procedure hallo2;                    <- Level 2         }
{           procedure hallo3;                   <- Level 3         }
{           type record = record                <- Level 4         }
{           (closing declarations)                                 }
{         procedure hallo4;                     <- Level 1         }
{        Thats it.                                                 }
{ ---------------------------------------------------------------- }
{ Display: A sort of a pointer to the BlockTable position of the   }
{          level. So Display[Level] -> Blocktable record of Level  }
{          Display from high to low gives the order of all         }
{          procedures in the way they are parsed.                  }
{ ---------------------------------------------------------------- }
{ Stack: The stack is only used by the interpreter.                }
{        The 0 element is not used, byte 1 till 4 are initialized  }
{        as follows:                                               }
{                                                                  }
{        Stack[1].i := 0;                                          }
{        Stack[2].i := 0;                                          }
{        Stack[3].i := -1;                                         }
{        Stack[4].i := BlockTable[1].Last; ( Last identifier of )  }
{                                          ( first block )         }
{ ---------------------------------------------------------------- }
{ ---------------------------------------------------------------- }

function interpret(var elx_Globals: elx_GlobalsType; Params: String; var CallObjPtr: Pointer): Boolean;
function Stack2Buf(var elx_Globals: elx_GlobalsType; var Buf; RecordType, RecordVar: Longint;
                   IsRecord, IsLoadAddress: Boolean): Boolean;
function Buf2Stack(var elx_Globals: elx_GlobalsType; var Buf; RecordType, RecordVar: Longint;
                   IsRecord, IsLoadAddress: Boolean): Boolean;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
uses LongStr, Dos, GenDos, TimeStmp
        {$IFDEF MSDOS}
          ,Strings
        {$ELSE}
          ,SysUtils
        {$ENDIF} ;


{.$DEFINE ELX_PROFILE}


var  OpCodeCount    : Longint;

{$IFDEF ELX_PROFILE}
var elx_ProfInfo: Array[0..255] of record
                                     MSecs: Real;
                                     Count: Longint;
                                   end; { record }
    elx_ProfTimeTmp: real;
    elx_ProfCntr   : longint;
    elx_ProfTotal  : Real;
    ConcatCount    : Array[0..5] of record
                                      MSecs: real;
                                      Count: longint;
                                    end; { record }
    UserFuncCount  : Array[0..1000] of record
                                         MSecs: Real;
                                         Count: Longint;
                                       end; { record }
    UserFuncSpecifics: Array[0..5] of record
                                       Count: Longint;
                                       MSecs: Real;
                                     end; { if }
    FuncCount      : Array[0..1000] of record
                                         MSecs: Real;
                                         Count: Longint;
                                       end; { record }
    concattmp      : Real;
    StrDuplTmp     : Real;
    StrDuplResult  : Real;
    RunUserTmp     : Real;
    FuncCountTmp   : Real;

function elx_ProfGetMsec: Real;
var Hour,
    Min,
    Sec,
{$IFDEF VirtualPascal}
    MSec   : Integer;
{$ELSE}
    MSec: Word;
{$ENDIF}
    Temp   : Integer;
begin
(*
  GetTime(Hour, Min, Sec, MSec);

  Temp := (Hour * 3600) + (Min * 60) + Sec;
  elx_ProfGetMsec:= (Temp * 100) + MSec;
*)
  elx_ProfGetMSec := GetMicroSeconds;  
end; { func. elx_ProfGetMSec }
{$ENDIF}

function Buf2Stack(var elx_Globals: elx_GlobalsType; var Buf; RecordType, RecordVar: Longint;
                   IsRecord, IsLoadAddress: Boolean): Boolean;

  procedure LoadRecordData(var RecordType, BufPtr, ThisSize: Longint; Idx: Longint); FORWARD;


  procedure LoadStringData(var BufPtr, ThisSize: Longint; var Ident: IdentRecord; Idx: Longint);
  var CharBuf: Array[0..MaxRecByteSize] of Char ABSOLUTE Buf;
      TmpStr : String;
      StringPtr: Longint;
  begin
    {-- Point the buffer at the beginning of this data ------------------}
    Dec(BufPtr, ThisSize);

    {-- Get the data ----------------------------------------------------}
    Move(CharBuf[BufPtr], TmpStr, ThisSize);

    {-- Set the string size ---------------------------------------------}
    TmpStr[0] := CharBuf[BufPtr];

    {-- Set the new string ----------------------------------------------}
    if TmpStr = '' then
      begin
        {-- StrPtr ------------------------------------------------------}
        StringPtr := RecordVar + Ident.Adr + Idx;

        {-- Free up the string ------------------------------------------}
        if elx_Globals.Stack[StringPtr].S <> 0 then
          StrFreeString(elx_Globals, elx_Globals.Stack[StringPtr].S);

        {-- Set the string to zero --------------------------------------}
        elx_Globals.Stack[StringPtr].S := elx_Globals.NulString;
      end
        else
          with elx_Globals do
             begin
               {-- StrPtr -----------------------------------------------}
               StringPtr := RecordVar + Ident.Adr + Idx;

               {-- Free up the string -----------------------------------}
               if Stack[StringPtr].S = 0 then
                 StrAllocNew(elx_Globals, Stack[StringPtr].S, Length(TmpStr));


               StringTable^[Stack[StringPtr].S]^.Len := Length(TmpStr);
               {$IFDEF MSDOS}
                 TmpStr := StringTable^[Stack[StringPtr].S]^.Content;
               {$ELSE}
                 sysUtils.StrPCopy(PChar(StringTable^[Stack[StringPtr].S]^.Content), TmpStr);
               {$ENDIF}

{!!!
writeln('loading: "',tmpstr,' / ' , StringTable^[Stack[StringPtr].S]^.Len);
!!!}

             end; { else }

    {-- Set data type ---------------------------------------------------}
    elx_Globals.Stack[StringPtr].CN := typ_Strngs;
  end; { proc. LoadStringData }


  procedure LoadIntegerData(var BufPtr, ThisSize: Longint; var Ident: IdentRecord; Idx: Longint);
  var TmpBuf : Array[0..16] of Integer;
      CharBuf: Array[0..0] of Char ABSOLUTE Buf;
      TmpByte: Byte ABSOLUTE TmpBuf;
      {$IFDEF MSDOS}
        TmpInt : Integer ABSOLUTE TmpBuf;
      {$ELSE}
        TmpInt : SmallInt ABSOLUTE TmpBuf;
      {$ENDIF}
      TmpLong: LongInt ABSOLUTE TmpBuf;
  begin
    {-- Point the buffer at the beginning of this data ------------------}
    Dec(BufPtr, ThisSize);

    {-- Get the data ----------------------------------------------------}
    Move(CharBuf[BufPtr], TmpBuf, ThisSize);


    {-- Set the string size ---------------------------------------------}
    Case ThisSize of
      1 : elx_Globals.Stack[RecordVar + Ident.Adr + Idx].i := TmpByte;
      2 : elx_Globals.Stack[RecordVar + Ident.Adr + Idx].i := TmpInt;
      3 : { unk };
      4 : elx_Globals.Stack[RecordVar + Ident.Adr + Idx].i := TmpLong;
    end; { case }

    {-- Set data type ---------------------------------------------------}
    elx_Globals.Stack[RecordVar + Ident.Adr + Idx].CN := typ_Ints;
  end; { proc. LoadIntegerData }


  procedure LoadCharData(var BufPtr, ThisSize: Longint; var Ident: IdentRecord; Idx: Longint);
  var CharBuf: Array[0..0] of Char ABSOLUTE Buf;
  begin
    {-- Point the buffer at the beginning of this data ------------------}
    Dec(BufPtr, ThisSize);

    {-- Get the data ----------------------------------------------------}
    elx_Globals.Stack[RecordVar + Ident.Adr + Idx].C := CharBuf[BufPtr];

    {-- Set data type ---------------------------------------------------}
    elx_Globals.Stack[RecordVar + Ident.Adr + Idx].CN := typ_Chars;
  end; { proc. LoadCharData }


  procedure LoadBoolData(var BufPtr, ThisSize: Longint; var Ident: IdentRecord; Idx: Longint);
  var BoolBuf: Array[0..0] of Boolean ABSOLUTE Buf;
  begin
    {-- Point the buffer at the beginning of this data ------------------}
    Dec(BufPtr, ThisSize);

    {-- Get the data ----------------------------------------------------}
    elx_Globals.Stack[RecordVar + Ident.Adr + Idx].B := BoolBuf[BufPtr];

    {-- Set data type ---------------------------------------------------}
    elx_Globals.Stack[RecordVar + Ident.Adr + Idx].CN := typ_Bools;
  end; { proc. LoadBoolData }

  procedure LoadRealData(var BufPtr, ThisSize: Longint; var Ident: IdentRecord; Idx: Longint);
  var TmpBuf : Array[0..16] of Real;
      CharBuf: Array[0..0] of Char ABSOLUTE Buf;
      TmpReal: Real ABSOLUTE TmpBuf;
  begin
    {-- Point the buffer at the beginning of this data ------------------}
    Dec(BufPtr, ThisSize);

    {-- Get the data ----------------------------------------------------}
    Move(CharBuf[BufPtr], TmpBuf, ThisSize);

    {-- Set the string size ---------------------------------------------}
    {-- Get the data ----------------------------------------------------}
    elx_Globals.Stack[RecordVar + Ident.Adr + Idx].R := TmpReal;

    {-- Set data type ---------------------------------------------------}
    elx_Globals.Stack[RecordVar + Ident.Adr + Idx].CN := typ_Reals;
  end; { proc. LoadRealData }

  procedure LoadArrayData(var BufPtr, ThisSize: Longint; var Ident: IdentRecord; Idx: Longint);
  var Counter  : Integer;
      ElMaxSize: Longint;
      IdxPtr   : Longint;
      TempType : Longint;
  begin
    {-- Now loop through the whole array elements -----------------------}
    With elx_Globals, ArrayTable^[Ident.Ref].ArrayInf do
      for Counter := High downto Low do
        begin
          {-- Now set the element to the right size ---------------------}
          ElMaxSize := ElementMaxSize;
          TempType := ElementRef;
          IdxPtr := (((Counter - Low) * ElementSize) + Idx){ + Ident.Adr};

          Case ElementType of
             typ_Strngs : LoadStringData(BufPtr, ElMaxSize, Ident, IdxPtr);
             typ_ints   : LoadIntegerData(BufPtr, ElMaxSize, Ident, IdxPtr);
             typ_Arrays : LoadArrayData(BufPtr, ElMaxSize, Ident, IdxPtr);
             typ_Chars  : LoadCharData(BufPtr, ElMaxSize, Ident, IdxPtr);
             typ_Records: LoadRecordData(TempType, BufPtr, ElMaxSize, IdxPtr + Ident.Adr);
             typ_Bools  : LoadBoolData(BufPtr, ElMaxSize, Ident, IdxPtr);
             typ_Reals  : LoadRealData(BufPtr, ElMaxSize, Ident, IdxPtr);
          end; { case }

        end; { for }

    {-- Set data type ---------------------------------------------------}
    elx_Globals.Stack[RecordVar + Ident.Adr + Idx].CN := typ_Arrays;
  end; { proc. LoadArrayData }


  procedure LoadRecordData(var RecordType, BufPtr, ThisSize: Longint; Idx: Longint);
  var Counter : Longint;
      TempType: Longint;
  begin
    Counter := elx_Globals.BlockTable^[RecordType].Last;

    while Counter <> 0 do
     with elx_Globals do
      begin
        {-- Lookup the size of this variable ----------------------------}
        ThisSize := GetFieldSize(elx_Globals, IdentTable^[Counter]);
        TempType := IdentTable^[Counter].Ref;

        {-- Now give away all this precious data to the corr. variable --}
        Case IdentTable^[Counter].Typ of
          typ_Strngs : LoadStringData(BufPtr, ThisSize, IdentTable^[Counter], Idx);
          typ_ints   : LoadIntegerData(BufPtr, ThisSize, IdentTable^[Counter], Idx);
          typ_Arrays : LoadArrayData(BufPtr, ThisSize, IdentTable^[Counter], Idx);
          typ_Chars  : LoadCharData(BufPtr, ThisSize, IdentTable^[Counter], Idx);
          typ_Bools  : LoadBoolData(BufPtr, ThisSize, IdentTable^[Counter], Idx);
          typ_Reals  : LoadRealData(BufPtr, ThisSize, IdentTable^[Counter], Idx);
          typ_Records: LoadRecordData(TempType, BufPtr, ThisSize, Idx + IdentTable^[Counter].Adr);
        end; { case }

        {-- Get the next variable ---------------------------------------}
        Counter := IdentTable^[Counter].Link;
      end; { while }
  end; { proc. LoadRecordData }

var BufPtr  : Longint;
    ThisSize: Longint;
begin { buf2stack }
  {-- Initialize some variables -----------------------------------------}
  if IsRecord then
   with elx_Globals do
    begin
      if IsLoadAddress then
        begin
          RecordVar := Display^[IdentTable^[RecordType].Lev] +
                        IdentTable^[RecordType].Adr;
        end
          else RecordVar := IdentTable^[RecordType].Lev +
                              IdentTable^[RecordType].Adr;

      RecordType := IdentTable^[RecordType].Ref;
      BufPtr := BlockTable^[RecordType].ByteSize;
    end
      else BufPtr := GetFieldSize(elx_Globals, elx_Globals.IdentTable^[RecordType]);

  {-- Make sure we wont start flooding memory ---------------------------}
  if BufPtr > MaxRecByteSize then
    begin
      Buf2Stack := FALSE;
      EXIT;
    end; { if }

  {-- Convert the buffer to variables on the stack ----------------------}
  if IsRecord then LoadRecordData(RecordType, BufPtr, ThisSize, 0)
    else LoadArrayData(BufPtr, ThisSize, elx_Globals.IdentTable^[RecordType], 0);

  {-- We succeeded ------------------------------------------------------}
  Buf2Stack := TRUE;
end; { func. Buf2Stack }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ReadBuffer(var elx_Globals: elx_GlobalsType;
                        FileNr, RecordType, RecordVar: Longint;
                        IsLoadAddress: Boolean): Boolean;
var Buf: Array[0..MaxRecByteSize] of Char;
begin
  {-- Actually read the data --------------------------------------------}
  with elx_Globals do
    FileTable[FileNr].FilePtr^.BlkRead(Buf, BlockTable^[IdentTable^[RecordType].Ref].ByteSize);

  {-- Now (reverse) loop through the records ----------------------------}
  ReadBuffer := Buf2Stack(elx_Globals, Buf, RecordType, RecordVar, true,
                          IsLoadAddress);
end; { func. ReadBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Stack2Buf(var elx_Globals: elx_GlobalsType; var Buf; RecordType, RecordVar: Longint;
                   IsRecord, IsLoadAddress: Boolean): Boolean;

  procedure LoadRecordData(var Buf; var RecordType, BufPtr, ThisSize: Longint; Idx: Longint); FORWARD;

  procedure LoadStringData(var Buf; var BufPtr, ThisSize: Longint; var Ident: IdentRecord; Idx: Longint);
  var CharBuf  : Array[0..MaxRecByteSize] of Char ABSOLUTE Buf;
      TmpStr   : String;
      StringPtr: Longint;
  begin
    {-- Point the buffer at the beginning of this data ------------------}
    Dec(BufPtr, ThisSize);

    {-- Fill the gaps ---------------------------------------------------}
    FillChar(CharBuf[BufPtr], ThisSize, #0);
    FillChar(TmpStr, SizeOf(TmpStr), #0);

    {-- StrPtr ----------------------------------------------------------}
    StringPtr := RecordVar + Ident.Adr + Idx;

    {-- Get the string --------------------------------------------------}
    {$IFNDEF MSDOS}
    TmpStr := Copy(StrPas(PChar(elx_Globals.StringTable^[elx_Globals.Stack[StringPtr].S]^.Content)),
                   1,
                   elx_Globals.StringTable^[elx_Globals.Stack[StringPtr].S]^.Len);
    {$ELSE}
    TmpStr := elx_Globals.StringTable^[elx_Globals.Stack[StringPtr].S]^.Content;
    {$ENDIF}

    {-- Move it into the buffer -----------------------------------------}
    Move(TmpStr, CharBuf[BufPtr], ThisSize);
  end; { proc. LoadStringData }


  procedure LoadIntegerData(var Buf; var BufPtr, ThisSize: Longint; var Ident: IdentRecord; Idx: Longint);
  var CharBuf: Array[0..0] of Char ABSOLUTE Buf;
      TmpBuf : Array[0..16] of Byte;
      TmpByte: Byte ABSOLUTE TmpBuf;
      TmpInt : Integer ABSOLUTE TmpBuf;
      TmpLong: LongInt ABSOLUTE TmpBuf;
  begin
    {-- Point the buffer at the beginning of this data ------------------}
    Dec(BufPtr, ThisSize);

    {-- Set the string size ---------------------------------------------}
    Case ThisSize of
      1 : TmpByte := elx_Globals.Stack[RecordVar + Ident.Adr + Idx].i;
      2 : TmpInt := elx_Globals.Stack[RecordVar + Ident.Adr + Idx].i;
      3 : { unk };
      4 : TmpLong := elx_Globals.Stack[RecordVar + Ident.Adr + Idx].i;
    end; { case }

    {-- Get the data ----------------------------------------------------}
    Move(TmpBuf[0], CharBuf[BufPtr], ThisSize);
  end; { proc. LoadIntegerData }

  procedure LoadCharData(var Buf; var BufPtr, ThisSize: Longint; var Ident: IdentRecord; Idx: Longint);
  var CharBuf: Array[0..0] of Char ABSOLUTE Buf;
  begin
    {-- Point the buffer at the beginning of this data ------------------}
    Dec(BufPtr, ThisSize);

    {-- Get the data ----------------------------------------------------}
    CharBuf[BufPtr] := elx_Globals.Stack[RecordVar + Ident.Adr + Idx].C;
  end; { proc. LoadCharData }

  procedure LoadBoolData(var Buf; var BufPtr, ThisSize: Longint; var Ident: IdentRecord; Idx: Longint);
  var BoolBuf: Array[0..0] of Boolean ABSOLUTE Buf;
  begin
    {-- Point the buffer at the beginning of this data ------------------}
    Dec(BufPtr, ThisSize);

    {-- Get the data ----------------------------------------------------}
    BoolBuf[BufPtr] := elx_Globals.Stack[RecordVar + Ident.Adr + Idx].B;
  end; { proc. LoadBoolData }

  procedure LoadRealData(var Buf; var BufPtr, ThisSize: Longint; var Ident: IdentRecord; Idx: Longint);
  var CharBuf: Array[0..0] of Char ABSOLUTE Buf;
      TmpBuf : Array[0..16] of Real;
      TmpReal: Real ABSOLUTE TmpBuf;
  begin
    {-- Point the buffer at the beginning of this data ------------------}
    Dec(BufPtr, ThisSize);

    {-- Get the data ----------------------------------------------------}
    TmpReal := elx_Globals.Stack[RecordVar + Ident.Adr + Idx].R;

    {-- Get the data ----------------------------------------------------}
    Move(TmpReal, CharBuf[BufPtr], ThisSize);
  end; { proc. LoadRealData }


  procedure LoadArrayData(var Buf; var BufPtr, ThisSize: Longint; var Ident: IdentRecord; Idx: Longint);
  var Counter  : Integer;
      ElMaxSize: Longint;
      IdxPtr   : Longint;
      TempType : Longint;
  begin
    {-- Now loop through the whole array elements -----------------------}
    With elx_Globals.ArrayTable^[Ident.Ref].ArrayInf do
      for Counter := High downto Low do
        begin
          {-- Now set the element to the right size ---------------------}
          ElMaxSize := ElementMaxSize;
          TempType := ElementRef;
          IdxPtr := (((Counter - Low) * ElementSize) + Idx){ + Ident.Adr};

          Case ElementType of
             typ_Strngs : LoadStringData(Buf, BufPtr, ElMaxSize, Ident, IdxPtr);
             typ_ints   : LoadIntegerData(Buf, BufPtr, ElMaxSize, Ident, IdxPtr);
             typ_Arrays : LoadArrayData(Buf, BufPtr, ElMaxSize, Ident, IdxPtr);
             typ_Chars  : LoadCharData(Buf, BufPtr, ElMaxSize, Ident, IdxPtr);
             typ_Records: LoadRecordData(Buf, TempType, BufPtr, ElMaxSize, IdxPtr + Ident.Adr);
             typ_Bools  : LoadBoolData(Buf, BufPtr, ElMaxSize, Ident, IdxPtr);
             typ_Reals  : LoadRealData(Buf, BufPtr, ElMaxSize, Ident, IdxPtr);
          end; { case }
        end; { for }
  end; { proc. LoadArrayData }

  procedure LoadRecordData(var Buf; var RecordType, BufPtr, ThisSize: Longint; Idx: Longint);
  var Counter : Longint;
      TempType: Longint;
  begin
    Counter := elx_Globals.BlockTable^[RecordType].Last;

    while Counter <> 0 do
     with elx_Globals do
      begin
        {-- Lookup the size of this variable ----------------------------}
        ThisSize := GetFieldSize(elx_Globals, IdentTable^[Counter]);
        TempType := IdentTable^[Counter].Ref;

        {-- Now give away all this precious data to the corr. variable --}
        Case elx_Globals.IdentTable^[Counter].Typ of
          typ_Strngs : LoadStringData(Buf, BufPtr, ThisSize, IdentTable^[Counter], Idx);
          typ_ints   : LoadIntegerData(Buf, BufPtr, ThisSize, IdentTable^[Counter], Idx);
          typ_Arrays : LoadArrayData(Buf, BufPtr, ThisSize, IdentTable^[Counter], Idx);
          typ_Chars  : LoadCharData(Buf, BufPtr, ThisSize, IdentTable^[Counter], Idx);
          typ_Bools  : LoadBoolData(Buf, BufPtr, ThisSize, IdentTable^[Counter], Idx);
          typ_Reals  : LoadRealData(Buf, BufPtr, ThisSize, IdentTable^[Counter], Idx);
          typ_Records: LoadRecordData(Buf, TempType, BufPtr, ThisSize, Idx + IdentTable^[Counter].Adr);
        end; { case }

        {-- Get the next variable ---------------------------------------}
        Counter := IdentTable^[Counter].Link;
      end; { while }
  end; { proc. LoadRecordData }

var BufPtr  : Longint;
    ThisSize: Longint;
begin { stack2buf }
  {-- Initialize some variables -----------------------------------------}
  Stack2Buf := TRUE;

  if IsRecord then
   with elx_Globals do
    begin
      if IsLoadAddress then
        begin
          RecordVar := Display^[IdentTable^[RecordType].Lev] +
                        IdentTable^[RecordType].Adr;
        end
          else RecordVar := IdentTable^[RecordType].Lev +
                              IdentTable^[RecordType].Adr;
      RecordType := IdentTable^[RecordType].Ref;
(*
{!!}
      RecordVar := Display^[IdentTable^[RecordType].Lev] +
                     IdentTable^[RecordType].Adr;
      RecordType := IdentTable^[RecordType].Ref;
{!!}
*)

      BufPtr := BlockTable^[RecordType].ByteSize
    end
      else begin
             BufPtr := GetFieldSize(elx_Globals, elx_Globals.IdentTable^[RecordType]);
           end; { else }

  {-- Make sure we wont start flooding memory ---------------------------}
  if BufPtr > MaxRecByteSize then
    begin
      EXIT;
    end; { if }

  {-- Now (reverse) loop through the records ----------------------------}
  if IsRecord then LoadRecordData(Buf, RecordType, BufPtr, ThisSize, 0)
    else LoadArrayData(Buf, BufPtr, ThisSize, elx_Globals.IdentTable^[RecordType],
                       0);
end; { func. Stack2Buf }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure WriteBuffer(var elx_Globals: elx_GlobalsType;
                      FileNr, RecordType, RecordVar: Longint;
                      IsLoadAddress: Boolean);
var Buf: Array[0..MaxRecByteSize] of Char;
begin
  {-- Convert the data --------------------------------------------------}
  Stack2Buf(elx_Globals, Buf, RecordType, RecordVar, true, IsLoadAddress);

  {-- Actually read the data --------------------------------------------}
  with elx_Globals do
    FileTable[FileNr].FilePtr^.BlkWrite(Buf, BlockTable^[IdentTable^[RecordType].Ref].ByteSize);
end; { proc. WriteBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure StrLinkUp(var elx_Globals: elx_GlobalsType; J: Integer);
var I: Integer;
    DoLink: Integer;
begin
  with elx_Globals do
    begin
      int_BaseZero := int_BaseIndex;
      I := IdentTable^[Stack[int_BaseZero + 4].I].Lev;
    end; { with }

  {-- First lookup where (which level) this variable should be in ---------}
{ !!! was:    while J < BaseZero do }
  with elx_Globals do
    while J <= int_BaseZero do
      begin
        int_BaseZero := display^[I];
        Dec(I);
      end; { while }

  {-- we check to make sure this is not a dynamic string array, we dont -}
  {-- want those individual strng items to be linked as it will cause ---}
  {-- a loop. Instead, we link the whole array, and free it at once -----}
  if elx_Globals.Stack[J].DynArraySet < 1 then
    begin
      {-- Now setup the linkedlist --------------------------------------------}
      with elx_Globals do
        begin
          Stack[J].P := Stack[int_BaseZero+5].I; { Point this one to prev. latest one }
          Stack[int_BaseZero + 5].I := J;    { Now set the base one to this one }
          Stack[J].CN := typ_strngs;
        end; { with }
    end { if }
      else begin
             {-- Now setup the linkedlist --------------------------------------------}
             with elx_Globals do
              if Stack[int_BaseZero + 6].I <> J then
               begin
                 DoLink := Stack[int_BaseZero + 6].I;
                 while DoLink <> 0 do
                   begin
                     if Stack[DoLink].DynArraySet = Stack[J].DynArraySet then
                       begin
                         BREAK;
                       end; { if }

                     DoLink := Stack[DoLink].P;
                   end; { while }

                 {-- make absolutely sure we dont interlink as it will -----}
                 {-- hang the interpreter. not a pretty sight --------------}
                 if DoLink = 0 then
                   begin
                     Stack[J].P := Stack[int_BaseZero+6].I; { Point this one to prev. latest one }
                     Stack[int_BaseZero + 6].I := J;{ Now set the base one to this one }
                     Stack[J].CN := typ_dynarray;
                   end; { if }
               end; { with }
           end; { if }
end; { proc. StrLinkup }


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function StrDupl(var elx_Globals: elx_GlobalsType; var Lf, Rt: Longint): Boolean;
var TH1,
    TH2,
    TH3,
    TH4    : Longint;
  {$IFDEF MSDOS}
    DuplTmp: String;
  {$ELSE}
    DuplTmp: AnsiString;
  {$ENDIF}
begin
{$IFDEF ELX_PROFILE}
strdupltmp := elx_profgetmsec;
{$ENDIF}
  {-- Default to true ----------------------------------------------------}
  StrDupl := TRUE;

  {-- First get the left side (destination) ------------------------------}
  with elx_Globals do
    begin
      TH1 := Stack[LF].s;

      if TH1 <> 0 then TH2 := 1       { if string is not 'nil' }
        else TH2 := 0;
    end; { with }

  {-- Now get the right (original) side ----------------------------------}
  with elx_Globals do
    begin
      TH3 := Stack[RT].S;
      TH4 := StringTable^[TH3]^.Len;
    end; { with }

  {-- Make sure all fits and is well -------------------------------------}
  if (TH1 = 0) OR (TH2 < TH4) then
    begin
      if TH1 = 0 then
        begin
          StrLinkUp(elx_Globals, LF)       { make sure its freed after exitting this func };
        end
          else
            if TH2 <> 0 then StrFreeString(elx_Globals, TH1);

      {-- Create a new copy ------------------------------------------------}
      if NOT StrAllocNew(elx_Globals, TH1, TH4) then
        StrDupl := FALSE;

{$IFDEF ELX_PROFILE}
strduplresult := strduplresult + (elx_profgetmsec - strdupltmp);
{$ENDIF}

      {-- And set it to the new one ----------------------------------------}
      with elx_Globals do
        Stack[LF].S := TH1;
    end; { if }


  {-- Now actually copy the data -----------------------------------------}
  with elx_Globals do
    begin
      {!!!: was een copy tot TH4 }
      {$IFDEF MSDOS}
        DuplTmp := elx_Globals.StringTable^[TH3]^.Content;
        elx_Globals.StringTable^[TH1]^.Content := DuplTmp;
        elx_Globals.StringTable^[TH1]^.Len := Length(DuplTmp);
      {$ELSE}
        elx_Globals.StringTable^[TH1]^.Len := elx_Globals.StringTable^[TH3]^.Len;

        if StringTable^[TH1]^.AllocSz <=
            StringTable^[TH1]^.Len then
              StrGrowString(elx_Globals, TH1,
                            StringTable^[TH1]^.Len);

        Move(elx_Globals.StringTable^[TH3]^.Content^,
             elx_GLobals.StringTable^[Th1]^.Content^,
             StringTable^[TH1]^.Len);

        with elx_Globals do
          PChar(StringTable^[TH1]^.Content)[ StringTable^[TH1]^.Len ] := #0;
                                                               { was + 1 }
{!!!
writeln('StrDupl() = ', StrPas(PChar(elx_Globals.StringTable^[TH3]^.Content)), ' / ', StringTable^[TH1]^.Len, ' / ', th3);
!!!}
      {$ENDIF}
    end; { with }

end; { func. StrDupl }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Do_RunUserFunc(var elx_Globals: elx_GlobalsType; const Func, CallList: Longint;
                         var CallObjPtr: Pointer);
var TmpStack  : Array[1..20] of StackType;
    FuncReturn: StackType;
  {$IFDEF MSDOS}
    TmpStr    : String;
  {$ELSE}
    TmpStr    : AnsiString;
  {$ENDIF}
    Counter   : Longint;
    NrParams  : Longint;
    ParLst    : String;

    Tmp1      : Integer;
    Tmp2      : Integer;
    TmpBool   : Boolean;
{$IFDEF ELX_PROFILE}
    RunFuncTmp: real;
{$ENDIF}

begin
    {-- This function will build a 2nd stack (max. 20 params) --------------}
    {-- that will contain all the parameters. The changing of variables ----}
    {-- etc is also left over to this routine. The actual procedure who ----}
    {-- gets called is all unknowing about this ----------------------------}

     {$IFDEF ELX_PROFILE}
       inc(UserFuncSpecifics[1].Count);
       RunFuncTmp := elx_profgetmsec;
     {$ENDIF}

    {-- Get a local copy of the parameter list. ----------------------------}
    TmpStr := Copy(elx_Globals.UserParamList^[Func]^, 2, 255);
    NrParams := Length(TmpStr);

    for Counter := 01 to NrParams do
      begin
        {-- First determine wether its a var param or not ------------------}
        if TmpStr[Counter] in ['A'..'Z'] then
         with elx_Globals do
          begin
            {-- Var parameter ----------------------------------------------}
            TmpStack[Counter] := Stack[Stack[IdentCount - (NrParams - Counter)].I];
          end
            else with elx_Globals do
                 begin
                   {-- Local copy ------------------------------------------}
                   TmpStack[Counter] := Stack[IdentCount - (NrParams - Counter)];
                 end; { if }
      end; { for }

    {-- Set the current function result ------------------------------------}
    with elx_Globals do
      FuncReturn := Stack[IdentCount - (NrParams - 1)];

      {$IFDEF ELX_PROFILE}
         UserFuncSpecifics[1].msecs := UserFuncSpecifics[1].msecs + (elx_profgetmsec - RunFuncTmp);
      {$ENDIF}
     {$IFDEF ELX_PROFILE}
       inc(UserFuncSpecifics[2].Count);
       RunFuncTmp := elx_profgetmsec;
     {$ENDIF}

    {-- Lets actually call the procedures ----------------------------------}
    with elx_Globals do
      if Assigned(elx_UserFuncsHook) then
        elx_UserFuncsHook(Func, elx_Globals, TmpStack, FuncReturn,
                          CallObjPtr, UserCallList^[CallList]^);

      {$IFDEF ELX_PROFILE}
         UserFuncSpecifics[2].msecs :=  UserFuncSpecifics[2].msecs + (elx_profgetmsec - RunFuncTmp);
         inc(UserFuncSpecifics[3].Count);
         RunFuncTmp := elx_profgetmsec;
     {$ENDIF}

    {-- Now see if there any strings we have to dispose of -----------------}
    {!!!! experimental! }
    with elx_Globals do
     if Pos('o', UserCallList^[CallList]^) > 0 then
      begin
        {-- Get the parameter list and strip the exxx addresses ------------}
        ParLst := UserCallList^[CallList]^;
        Counter := 01;
        while Counter < Length(ParLst) do
          begin
            if (UpCase(ParLst[Counter]) = 'E') OR
                (UpCase(ParLst[Counter]) = 'A') then
              begin
                Delete(ParLst, Counter + 1, 5);
              end; { if }

            Inc(Counter);
          end; { while }

        {-- Now actually match the stack names -----------------------------}
        for Counter := 01 to Length(ParLst) do
         begin
            {-- lets see if there any temporarily strings ----------------------}
            if ParLst[Counter] = 'o' then
             with elx_Globals do
              begin
                StrFreeString(elx_Globals, Stack[IdentCount - (NrParams - Counter)].S);
              end; { if }
           end; { for }
      end; { with }

      {$IFDEF ELX_PROFILE}
         UserFuncSpecifics[3].msecs  := UserFuncSpecifics[3].msecs + (elx_profgetmsec - RunFuncTmp);
         inc(UserFuncSpecifics[4].Count);
         RunFuncTmp := elx_profgetmsec;
     {$ENDIF}


    {-- Now set the changed variables back, if needed ----------------------}
    for Counter := 01 to NrParams do
      begin
        {-- First determine wether its a var param or not ------------------}
        if TmpStr[Counter] in ['A'..'Z'] then
         with elx_Globals do
          begin
            {-- Var parameter ----------------------------------------------}
            if (NOT (TmpStr[Counter] in ['E', 'A'])) then
              begin
                TmpBool := false;

                if TmpStr[Counter] = 'S' then
                  begin
                    if Stack[Stack[IdentCount - (NrParams - Counter)].I].S = 0 then
                      begin
                        {-- we link up this string with previous content -}
                        Tmp1 := Stack[IdentCount - (NrParams - Counter)].I;
                        StrLinkUp(elx_Globals, Tmp1);
                        TmpBool := true;

                        {-- we have to save the pointer, because the tmp--}
                        {-- stack overwrites us with the new values, -----}
                        Tmp2 := Stack[tmp1].p;
                      end;
                  end; { if }

                {-- update the real stack with the tmpstack values -------}
                Stack[Stack[IdentCount - (NrParams - Counter)].I] := TmpStack[Counter];

                {-- because the linked list ptr is overwritten, correct ----}
                if TmpBool then
                  begin
                    Stack[Tmp1].P := Tmp2; { Point this one to prev. latest one }
                    Stack[Stack[Tmp1].P].P := 0; { // String fix for large reals }
                  end; { if }
              end; { if }
          end;
      end; { for }

    {-- Dispose the variables that were used -------------------------------}
    with elx_Globals do
      Dec(IdentCount, NrParams);

      {$IFDEF ELX_PROFILE}
         UserFuncSpecifics[4].msecs :=  UserFuncSpecifics[4].msecs  + (elx_profgetmsec - RunFuncTmp);
         inc(UserFuncSpecifics[5].Count);
         RunFuncTmp := elx_profgetmsec;
      {$ENDIF}

    {-- Now set the returning value, if needed -----------------------------}
    with elx_Globals do
      if upCase(UserParamList^[Func]^[1]) = 'F' then
        begin
          Inc(IdentCount);

          {-- if we return a string (uppercase F), lets link it to the rest-}
          if UserParamList^[Func]^[1] = 'F' then
            begin
              if Stack[IdentCount].i = 0 then
                begin

(* we should not link it up here, the op_StringTemp already deallocates
   the value we would want to use, so this is really useless.
                  {-- we link up this string with previous content -}
                  Tmp1 := IdentCount;
                  StrLinkUp(elx_Globals, Tmp1);

                  {-- and setup the pointer -------------------------}
                  FuncReturn.P := Stack[Tmp1].P;
*)
                end; { if }
            end; { if }
          {-- and return the strng -----------------------------------------}
          Stack[IdentCount] := FuncReturn;
        end; { if }

      {$IFDEF ELX_PROFILE}
         UserFuncSpecifics[5].msecs := UserFuncSpecifics[5].msecs + (elx_profgetmsec - RunFuncTmp);
      {$ENDIF}
end; { Do_funcRunUserFunction }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


procedure DoStackDump(var elx_Globals: elx_GlobalsType);
var Counter: Integer;
    CurLev : Integer;
begin
  if elx_Globals.LogErrors then
    with elx_Globals do
      begin
        {-- Set the current level were at --------------------------------------}
        CurLev := IdentTable^[int_H2].Lev;

        {-- Print the header ---------------------------------------------------}
        ErrorFile^.WriteLn('');
        ErrorFile^.WriteLn('');
        ErrorFile^.WriteLn('Calling (proc.name) : ' + IdentTable^[int_h2].Name);
        ErrorFile^.WriteLn('Level               : ' + IntStr(int_H3, 4));
        ErrorFile^.WriteLn('Start of code       : ' + IntStr(int_ProgPtr, 4));
        ErrorFile^.WriteLn('');
        ErrorFile^.WriteLn('');

        {-- Show the display ---------------------------------------------------}
        ErrorFile^.WriteLn('Contents of display');
        ErrorFile^.WriteLn('');
        for Counter := (CurLev + 1) downto 01 do
          ErrorFile^.WriteLn(IntStr(Counter, 4) + #32 + IntStr(Display^[Counter], 6));

        {-- Display the stack --------------------------------------------------}
        ErrorFile^.WriteLn('');
        ErrorFile^.WriteLn('');
        ErrorFile^.WriteLn('Top of stack: ' + IntStr(IdentCount, 4));
        ErrorFile^.WriteLn('Frame Base  : ' + IntStr(int_BaseIndex, 4));
        ErrorFile^.WriteLn('');
        ErrorFile^.WriteLn('');
        ErrorFile^.WriteLn('            Stack contents');
        ErrorFile^.WriteLn('');

        for Counter := IdentCount downto 01 do
          ErrorFile^.WriteLn(IntStr(Counter, 14) + #32 + IntStr(Stack[Counter].i, 8));

        ErrorFile^.WriteLn('                            < = = = >')
     end; { if }
end; { proc. Dump }


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

  function RunFunction(var elx_Globals: elx_GlobalsType; int_Param2: Longint; var CallObjPtr: Pointer): Boolean;
  var TmpInt : Integer;
      sBuff  : String[80];
      {$IFNDEF DELPHI}
        TmpCode: Word;
      {$ELSE}
        TmpCode: Integer;
      {$ENDIF}
      TmpStack : Array[0..5] of StackType;

  {$IFNDEF MSDOS}
      ExpandStr: AnsiString;

   function ExpandVars(TmpStr: String; Level: integer): String;

   function FindVarName(TmpStr: String): String;
   var Counter: Longint;
       TempInt: Longint;

       LookFor: AlfaType;
   begin
     Counter := Level;

     {-- Fixup the variable name --------------------------------------------}
     Delete(TmpStr, 1, 1);

     {-- prepare the ident table for the search -----------------------------}
     with elx_Globals do                 { Make sure we always find this ID }
       begin
         FillChar(IdentTable^[0].Name, SizeOf(IdentTable^[0].Name), #32);
         Move(TmpStr[1], IdentTable^[0].Name, Length(TmpStr));

         LookFor := IdentTable^[0].Name;
       end; { with }

     with elx_Globals do
       REPEAT
         TempInt := BlockTable^[Counter + 1].Last;

         While IdentTable^[TempInt].Name <> LookFor do
           TempInt := IdentTable^[TempInt].Link;

         Dec(Counter);
       UNTIL (Counter < 0) OR (TempInt <> 0) OR (AbortCompile);

       if TempInt <> 0 then
        begin
          with elx_globals, IdentTable^[Tempint] do
           with Stack[elx_Globals.Display^[Lev] + Adr]  do
             begin
               case cn of
                 typ_strngs : FindVarName := StrPas(PChar(StringTable^[s]^.Content))
                  else FindVarName := FStr(i);
              end; { case }
           end; { with }
        end
          else FindVarName := '{unknown variable}';
   end; { func. FindVarname }

   var Counter  : Longint;
       SmallStr : String;

       NewStr   : String;
   begin
     {-- Initialize some variables ------------------------------------------}
     TmpInt := 01;
     Counter := 01;
     NewStr := '';

     while (Counter <= Length(TmpStr)) AND (NOT elx_Globals.AbortCompile) do
       begin
         {-- now check wether this was a variable ---------------------------}
         if TmpStr[Counter] <> '$' then
           SmallStr := SmallStr + TmpStr[Counter]
              else begin
                     {-- submit the first part of this string ---------------}
                     NewStr := NewStr + SmallStr;

                     {-- and put the invalid character at first pos ---}
                     SmallStr := TmpStr[Counter];

                     {-- first character was a $ ----------------------------}
                     Inc(Counter);

                     {-- now loop through the variable name -----------------}
                     while (Counter <= Length(TmpStr)) AND (NOT elx_Globals.AbortCompile) do
                        begin
                           if TmpStr[Counter] in ['A'..'Z', 'a'..'z', '0'..'9', '_'] then
                             begin
                               SmallStr := SmallStr + TmpStr[Counter];

                               Inc(Counter);
                             end
                               else BREAK;
                        end; { while }

                     {-- and submit it as a variable ------------------}
                     if SmallStr = '$' then
                       begin
                         SmallStr := '$';
                       end
                         else begin
                                NewStr := NewStr + FindVarName(SmallStr);

                                if Counter <= Length(TmpStr) then
                                  SmallStr := TmpStr[Counter]
                                   else SmallStr := '';
                              end; { else }
                   end; { else }

         {-- and keep looping through the string ---------------------}
         Inc(Counter);
       end; { while }

    {-- and submit the constant ---------------------------------------------}
    NewStr := NewStr + SmallStr;

    ExpandVars := NewStr;
  end; { func. ExpandVars }
  {$ENDIF}

  begin
    RunFunction := TRUE;

     {$IFDEF ELX_PROFILE}
       inc(FuncCount[int_Param2].Count);
       RunUserTmp := elx_profgetmsec;
     {$ENDIF}

    with elx_Globals do
     begin
      if (int_Param2 < 10) then
      Case int_Param2 of
        00 : Stack[IdentCount].I := ABS(Stack[IdentCount].I);
        01 : Stack[IdentCount].R := ABS(Stack[IdentCount].R);
        02 : Stack[IdentCount].I := SQR(Stack[IdentCount].I);
        03 : Stack[IdentCount].R := SQR(Stack[IdentCount].R);
        04 : Stack[IdentCount].B := ODD(Stack[IdentCount].I);
        05 : Stack[IdentCount].C := CHR(Stack[IdentCount].I);
        06 : Stack[IdentCount].I := ORD(Stack[IdentCount].C);
        07 : Stack[IdentCount].C := Succ(Stack[IdentCount].C);
        08 : Stack[IdentCount].C := Pred(Stack[IdentCount].C);
        09 : Stack[IdentCount].I := Round(Stack[IdentCount].R);
      end { case }

    else if (int_Param2 < 20) then
      Case int_Param2 of
        10 : Stack[IdentCount].I := Trunc(Stack[IdentCount].R);
        11 : Stack[IdentCount].R := SIN(Stack[IdentCount].R);
        12 : Stack[IdentCount].R := COS(Stack[IdentCount].R);
        13 : Stack[IdentCount].R := EXP(Stack[IdentCount].R);
        14 : if (Stack[IdentCount].R <= 0 ) then
               begin
                 int_ProgState := FnChk;
                 RunFunction := FALSE;
                 EXIT;
               end { if }
                 else Stack[IdentCount].R := LN(Stack[IdentCount].R);
        15 : if Stack[IdentCount].R < 0 then
               begin
                 int_ProgState := FnChk;
                 RunFunction := FALSE;
                 EXIT;
               end
                 else Stack[IdentCount].R := Sqrt(Stack[IdentCount].R);
        16 : Stack[IdentCount].R := ArcTan(Stack[IdentCount].R);
        17 : begin { eof(input) }
               Inc(IdentCount);

               if IdentCount > StackSize then
                 begin
                   int_ProgState := StkChk;
                   RunFunction := FALSE;
                   EXIT;
                 end { if }
                   else Stack[IdentCount].B := FALSE;
                     { EOF(Input); }
              end; { EOF(Input) }
        18 : begin { EOLN(Input) }
               Inc(IdentCount);

               if IdentCount > StackSize then
                 begin
                   int_ProgState := StkChk;
                   RunFunction := FALSE;
                   EXIT;
                 end { if }
                   else Stack[IdentCount].B := FALSE;
                     { EoLN(Input); }
             end; { EOLN(Input) }
        19 : begin
               Inc(IdentCount);

               if IdentCount > StackSize then
                 begin
                   int_ProgState := StkChk;
                   RunFunction := FALSE;
                   EXIT;
                 end { if }
                   else Stack[Identcount].I := {$IFDEF MSDOS}MaxAvail{$ELSE}0{$ENDIF};
             end; { MaxAvail }
      end { case }

    else if (int_Param2 < 33) then
      Case int_Param2 of
        20 : Stack[IdentCount].I := elx_Globals.StringTable^[Stack[IdentCount].I]^.Len;
        21 : begin           { Length of a temp string }
                         { Actual string to get len of }
               int_H1 := Stack[IdentCount].I;

                                  { Now get the length }
               Stack[IdentCount].I := StringTable^[int_H1]^.Len;

                           { And dispose of the string }
               StrFreeString(elx_Globals, int_H1);
            end;
        22 : Stack[IdentCount].I := 1;  { Length(Char) }
        23 : begin                     { Copy(S, 1, 1) }
                            { H1 is originating string }
               int_H1 := Stack[IdentCount - 2].I;
                              { H4 contains the length }
               int_H4 := StringTable^[int_H1]^.Len;

                            { H2 is the start position }
               int_H2 := Stack[IdentCount - 1].I;

               {-- Check invalid values ----------------}
               if (int_H2 < 1) OR (int_H2 > int_H4) then
                 begin
                   int_H4 := 0;
                   int_H2 := 2;
                 end; { if }

               {-- H3 will contain the chars to copy ---}
               int_H3 := Stack[IdentCount].I;

               {-- Correct H3 if necessary -------------}
               if (int_H3 > (int_H4 - (int_H2) + 1)) then
                 int_H3 := int_H4 - (int_H2) + 1;

               if int_H3 < 0 then int_H3 := 0;

               {-- Now get a new, temp, string ---------}
               if NOT StrAllocNew(elx_Globals, int_H5, int_H3) then
                 begin
                   RunFunction := FALSE;
                   EXIT;
                 end; { if }

               {-- Now actually copy the content -------}
               {$IFDEF MSDOS}
                 elx_Globals.StringTable^[int_H5]^.Content :=
                   Copy(elx_Globals.StringTable^[int_H1]^.Content, int_H2, int_H3);
                 elx_Globals.StringTable^[int_H5]^.Len := Length(elx_Globals.StringTable^[int_H5]^.Content);
               {$ELSE}
                 StrPCopy(PChar(elx_Globals.StringTable^[int_H5]^.Content),
                   Copy(StrPas(PChar(elx_Globals.StringTable^[int_H1]^.Content)), int_H2, int_H3));
                 elx_Globals.StringTable^[int_H5]^.Len := StrLen(PChar(elx_Globals.StringTable^[int_H5]^.Content));
               {$ENDIF}

               {-- Now set the value -------------------}
               Stack[IdentCount - 2].I := int_H5;

               {-- And dispose of the stack ------------}
               Dec(Identcount, 2);
             end; { copy(s, 1, 1) }

        24 : begin           { Copy('mb' + 'mb', 1, 1) }
                            { H1 is originating string }
               int_H1 := Stack[IdentCount - 2].I;
                              { H4 contains the length }
               int_H4 := StringTable^[int_H1]^.Len;
                            { H2 is the start position }
               int_H2 := Stack[IdentCount - 1].I;

               {-- Make sure we copy something ---------}
               if (int_H2 < 1) OR (int_H2 > int_H4) then
                 elx_Globals.StringTable^[int_H1]^.Len := 0
                   else begin
                          {-- Get the amount of chars --}
                          {-- to actually copy ---------}
                          int_H3 := Stack[IdentCount].I;

                          {-- Correct H3 if necessary --}
                          if (int_H3 > (int_H4 - (int_H2) + 1)) then
                            int_H3 := int_H4 - (int_H2) + 1;

                          if int_H3 < 0 then int_H3 := 0;

                          {-- Now copy the strng -------}
                          {$IFDEF MSDOS}
                            elx_Globals.StringTable^[int_H1]^.Content :=
                              Copy(StringTable^[int_H1]^.Content, int_H2, int_H3);
                            elx_Globals.StringTable^[int_H1]^.Len := Length(elx_Globals.StringTable^[int_H1]^.Content);
                          {$ELSE}
                            StrPCopy(PChar(elx_Globals.StringTable^[int_H1]^.Content),
                              Copy(StrPas(PChar(elx_Globals.StringTable^[int_H1]^.Content)), int_H2, int_H3));
                            elx_Globals.StringTable^[int_H1]^.Len := StrLen(PChar(elx_Globals.StringTable^[int_H1]^.Content));
                          {$ENDIF}
                        end; { else }

               {-- And dispose of the stack ------------}
               Dec(Identcount, 2);
             end; { Copy('mb' + 'mb', 1, 1) }

        25 : begin
               {-- Get a new temp string ---------------}
               if NOT StrAllocNew(elx_Globals, int_H1, 1) then
                 begin
                   RunFunction := FALSE;
                   EXIT;
                 end; { if }

               {-- Validate the assignment ----------------}
               if (Stack[IdentCount - 1].I = 1) and
                   (Stack[IdentCount].I > 0) then
                     begin
                       {$IFDEF MSDOS}
                         elx_Globals.StringTable^[int_H1]^.Content :=
                           Copy(Chr(Stack[IdentCount - 2].I), 1, 1);
                       {$ELSE}
                         PChar(elx_Globals.StringTable^[int_H1]^.Content)[0] :=
                           Chr(Stack[IdentCount - 2].I);
                         PChar(elx_Globals.StringTable^[int_H1]^.Content)[1] := #0;
                       {$ENDIF}
                       elx_Globals.StringTable^[int_H1]^.Len := 1;
                     end
                       else begin
                              elx_Globals.StringTable^[int_H1]^.Len := 0;
                              {$IFDEF MSDOS}
                                elx_Globals.StringTable^[int_H1]^.Content := '';
                              {$ELSE}
                                PChar(elx_Globals.StringTable^[int_H1]^.Content)[0] := #0;
                              {$ENDIF}
                            end; { else }

               {-- Set the result ----------------------}
               Stack[IdentCount - 2].I := int_H1;

               {-- And dispose of the stack ------------}
               Dec(Identcount, 2);
             end; { Copy(c, 1, 1) }

        26,                                { Pos(S, S) }
        27,                        { Pos('M' + 'B', S) }
        30,                        { Pos(S, 'M' + 'B') }
        31 : begin         { Pos('M' + 'B', 'M' + 'B') }
                        { H1 contains the first string }
               int_H1 := Stack[IdentCount - 1].I;
                       { H2 contains the second string }
               int_H2 := Stack[IdentCount].I;

               {-- Decrease the stack ------------------}
               Dec(IdentCount);

               {-- H6 contains the length of the 1st ---}
               {-- string ------------------------------}
               int_H6 := StringTable^[int_H1]^.Len + 4;

               {-- H3 contains the length of the 2nd ---}
               {-- string. This is the string we are ---}
               {-- actually looking *in* ---------------}
               {-- We calculate it a bit, so that if ---}
               {-- the string were looking for is ------}
               {-- larger, it will direct quit ---------}
               int_H3 := (StringTable^[int_H2]^.Len + 5) - int_H6;

               if (int_H3 <= 0) OR (int_H6 = 4) then
                 begin
                   Stack[IdentCount].I := 0
                 end
                   else begin
                          {$IFDEF MSDOS}
                            int_TmpStr := StringTable^[int_H2]^.Content;
                            int_H4 := Pos(StringTable^[int_H1]^.Content, int_TmpStr);
                          {$ELSE}

                            int_H4 := Longint(StrPos(PChar(StringTable^[int_H2]^.Content),
                                                     PChar(StringTable^[int_H1]^.Content))
                                      - Longint(PChar(StringTable^[int_H2]^.Content))) + 1;

                            if int_H4 < 0 then
                              int_H4 := 0;
                          {$ENDIF}

                          Stack[IdentCount].I := int_H4;
                        end; { else }

               {-- Dispose of temp. used strings -------}
               if ODD(int_Param2) then
                 StrFreeString(elx_Globals, int_H1);

               if (int_Param2 > 29) then
                 StrFreeString(elx_Globals, int_H2);
             end; { multiple pos operations }

        28,                                { Pos(C, S) }
        32 : begin               { Pos(C, 'Aa' + 'Bb') }
                        { H1 contains the first string }
               int_H1 := Stack[IdentCount - 1].I;
                       { H2 contains the second string }
               int_H2 := Stack[IdentCount].I;


               {-- Put the value in --------------------}
               {$IFDEF MSDOS}
                 int_TmpStr := StringTable^[int_H2]^.Content;
                 int_H4 := Pos(Chr(int_H1), int_TmpStr);
               {$ELSE}
                 int_TmpStr := StrPas(PChar(StringTable^[int_H2]^.Content));
                 int_H4 := Pos(Chr(int_H1), int_TmpStr);
               {$ENDIF}
               Stack[IdentCount - 1].I := int_H4;

               {-- Dispose of temp. used strings -------}
               if int_Param2 = 32 then
                 StrFreeString(elx_Globals, int_H2);

               {-- Decrease the stack ------------------}
               Dec(IdentCount);
             end; { multiple pos operations on a char }
      end { case }

    else if (int_Param2 < 40) then
      Case int_Param2 of
        33,                                { Str(i, s) }
        34 : begin                         { Str(r, s) }
               {-- Convert it to a temp buffer ---------}
               if (int_Param2 = 34) then
                 Str(Stack[IdentCount].R:18, sBuff)
                   else Str(Stack[IdentCount].I:1, sBuff);

               {-- Get the length ----------------------}
               int_H2 := Length(sBuff);

               {-- Create a new string -----------------}
               if NOT StrAllocNew(elx_Globals, int_H1, int_H2) then
                 begin
                   RunFunction := FALSE;
                   EXIT;
                 end; { if }

               {-- Set the string ----------------------}
               {$IFDEF MSDOS}
                 elx_Globals.StringTable^[int_H1]^.Content := sBuff;
               {$ELSE}
                 StrPCopy(PChar(elx_Globals.StringTable^[int_H1]^.Content), sBuff);
               {$ENDIF}
               elx_Globals.StringTable^[int_H1]^.Len := Length(sBuff);

               {-- Now actually return the value -------}
               Stack[IdentCount].I := int_H1;
             end; { Str (both integer and real) }

        35,                                { Val(S, I) }
        36,                        { Val('1' + '2', I) }
        37,                                { Val(S, R) }
        38 : begin                 { Val('1' + '2', R) }
                        { The actual string to convert }
               int_H1 := Stack[IdentCount].I;
                                { Length of that strng }
               int_H2 := StringTable^[int_H1]^.Len;

               {-- Convert it to this tmpstring --------}
               {$IFDEF MSDOS}
                 sBuff := Copy(StringTable^[int_H1]^.Content, 1, StringTable^[int_H1]^.len);
               {$ELSE}
                 sBuff := Copy(StrPas(PChar(StringTable^[int_H1]^.Content)), 1, StringTable^[int_H1]^.len);
               {$ENDIF}

               {-- Now convert it to a integer/real ----}
               if int_Param2 < 37 then
                 begin
                   Val(sBuff, Stack[IdentCount].I, TmpCode);
                 end
                   else begin
                          Val(sBuff, Stack[IdentCount].R, TmpCode);
                        end; { else }

               {-- If its a temp, dispose it -----------}
               if (NOT ODD(int_Param2)) then
                 StrFreeString(elx_Globals, int_H1);
             end; { val(i), val(r) }

        39 : begin                         { KeyPressed }
               {-- Increase identcount for result ------}
               Inc(IdentCount);

               {-- Check stacksize ---------------------}
               if IdentCount > StackSize then
                 begin
                   int_ProgState := StkChk;
                   RunFunction := FALSE;
                   EXIT;
                 end; { if }

               {-- Set the result ----------------------}
               if Assigned(elx_SystemFuncsHook) then
                 elx_SystemFuncsHook(int_Param2, elx_Globals, Stack,
                                     CallObjPtr, Stack[IdentCount])
                   {$IFDEF WITH_FULL}
                     else {Stack[IdentCount].B := KeyPressed};
                   {$ENDIF};
             end; { KeyPressed }
      end { case }

    else if (int_Param2 < 50) then
      Case int_Param2 of
        40 : begin                            { Random }
                                { Get random(xx) value }
               int_H1 := Stack[Identcount].I;

               {-- Sanity check the value --------------}
               if int_H1 < 1 then
                 begin
                   int_ProgState := FnChk;
                   RunFunction := FALSE;
                   EXIT;
                 end; { if }

               {-- Return the value --------------------}
               Stack[IdentCount].I := Random(int_H1);
             end; { Random }

        41 : begin
               {-- Increase identcount for result ------}
               Inc(IdentCount);

               {-- Check stacksize ---------------------}
               if IdentCount > StackSize then
                 begin
                   int_ProgState := StkChk;
                   RunFunction := FALSE;
                   EXIT;
                 end; { if }

               {-- Return the value --------------------}
               Stack[IdentCount].R := Random;
             end; { r := random }

        42 : Stack[IdentCount].C := UpCase(Stack[IdentCount].C);
        43 : Randomize;
        44 : begin                            { ClrScr }
               if Assigned(elx_SystemFuncsHook) then
                 elx_SystemFuncsHook(int_Param2, elx_Globals, Stack,
                                     CallObjPtr, Stack[IdentCount]);
             end; { ClrScr }
        45 : begin                            { GotoXY }
                                     { Get X parameter }
               int_H1 := Stack[IdentCount - 1].I;
                                     { Get Y parameter }
               int_H2 := Stack[IdentCount].I;

               {-- Go to the actual position -----------}
               if Assigned(elx_SystemFuncsHook) then
                 begin
                   TmpStack[0].I := int_H1;
                   TmpStack[1].I := int_H2;
                   elx_SystemFuncsHook(int_Param2, elx_Globals, TmpStack,
                                       CallObjPtr, Stack[IdentCount])
                 end;

               {-- Dispose of the temp variables -------}
               Dec(Identcount, 2);
             end; { GotoXY }

        46 : begin                         { Textcolor }
               if Assigned(elx_SystemFuncsHook) then
                 begin
                   TmpStack[0].I := Stack[IdentCount].I;
                   elx_SystemFuncsHook(int_Param2, elx_Globals, TmpStack,
                                       CallObjPtr, Stack[IdentCount])
                 end;

               Dec(IdentCount);
             end; { TextColor }

        47 : begin                            { Readkey }
               {-- Increase identcount for result ------}
               Inc(IdentCount);

               {-- Check stacksize ---------------------}
               if IdentCount > StackSize then
                 begin
                   int_ProgState := StkChk;
                   RunFunction := FALSE;
                   EXIT;
                 end; { if }

               {-- Retrieve the value ------------------}
               if Assigned(elx_SystemFuncsHook) then
                 begin
                   elx_SystemFuncsHook(int_Param2, elx_Globals, TmpStack,
                                       CallObjptr, Stack[IdentCount]);

                   {$IFDEF MSDOS}
                     int_TmpStr := Copy(StringTable^[Stack[IdentCount].S]^.Content, 1,
                                    StringTable^[Stack[IdentCount].S]^.len);
                   {$ELSE}
                     int_TmpStr := Copy(StrPas(PChar(StringTable^[Stack[IdentCount].S]^.Content)), 1,
                                    StringTable^[Stack[IdentCount].S]^.len);


{!!! writeln('oeps (06)'); }
                   {$ENDIF}
                   StrFreeString(elx_Globals, Stack[IdentCount].S);
                 end;

               {-- Set the value -----------------------}
               if NOT StrAllocNew(elx_Globals, int_H3, Length(int_TmpStr)) then
                 begin
                   RunFunction := FALSE;
                   EXIT;
                 end; { if }

               {-- Return the value --------------------}
               {$IFDEF MSDOS}
                 StringTable^[int_H3]^.Content := int_TmpStr;
               {$ELSE}
                 StrPCopy(PChar(StringTable^[int_H3]^.Content), int_TmpStr);
               {$ENDIF}
               StringTable^[int_H3]^.Len := Length(int_TmpStr);
               Stack[IdentCount].I := int_H3;
             end; { ReadKey }

        48 : begin                             { WhereX }
               {-- Create stackspace for the result ----}
               Inc(IdentCount);

               {-- Set the value -----------------------}
               if Assigned(elx_SystemFuncsHook) then
                 begin
                   elx_SystemFuncsHook(int_Param2, elx_Globals, TmpStack,
                                       CallObjPtr, Stack[IdentCount])
                 end;
             end; { WhereX }

        49 : begin                             { WhereY }
               {-- Create stackspace for the result ----}
               Inc(IdentCount);

               {-- Set the value -----------------------}
               if Assigned(elx_SystemFuncsHook) then
                 begin
                   elx_SystemFuncsHook(int_Param2, elx_Globals, TmpStack,
                                       CallObjPtr, Stack[IdentCount])
                 end; { if }
             end; { WhereY }
      end { case }

    else if (int_Param2 < 80) then
      Case int_Param2 of
        50 : begin                             { Delay }
               {-- Run the procedure -------------------}
               if Assigned(elx_SystemFuncsHook) then
                 begin
                   TmpStack[0].I := Stack[IdentCount].I;
                   elx_SystemFuncsHook(int_Param2, elx_Globals, TmpStack,
                                       CallObjPtr, Stack[IdentCount])
                 end;

               {-- Decrease this ID --------------------}
               Dec(IdentCount);
             end; { Delay }

        51 : begin                     { Textbackground }
               {-- Run the procedure -------------------}
               if Assigned(elx_SystemFuncsHook) then
                 begin
                   TmpStack[0].I := Stack[IdentCount].I;
                   elx_SystemFuncsHook(int_Param2, elx_Globals, TmpStack,
                                       CallObjPtr, Stack[IdentCount])
                 end;

               {-- Decrease this ID --------------------}
               Dec(IdentCount);
             end; { TextBackGround }

        52 : begin                              { Sound }
               {$IFDEF MSDOS}
                 {-- Run the procedure -------------------}
                 {Sound(Stack[IdentCount].i);}
               {$ENDIF}

               {-- Decrease this ID --------------------}
               Dec(IdentCount);
             end; { Sound }

        53 : begin                        { NoSound :) }
               {$IFDEF MSDOS}
                 {NoSound;}
               {$ENDIF}
             end; { NoSound }

        54,                                                     { fileopen }
        55,
        56 : begin
               {-- First get the parameters, int_H1 = filename, H2 = mode -----}
               int_H1 := Stack[IdentCount - 2].I;    { Get the filename (ptr) }
               int_H2 := Stack[IdentCount - 1].I;        { Get the filenmode }
               int_H3 := Stack[IdentCount].I;        { Get the sharing mode }


               {-- Get the filename first ---------------------------------}
               Case int_Param2 of
                 {$IFDEF MSDOS}
                   54 : int_TmpStr := Copy(elx_Globals.StringTable^[int_H1]^.Content, 1, StringTable^[int_H1]^.Len);
                   55 : int_TmpStr := Copy(elx_Globals.StringTable^[int_H1]^.Content, 1, StringTable^[int_H1]^.Len);
                 {$ELSE}
                   54 : int_TmpStr := StrPas(PChar(elx_Globals.StringTable^[int_H1]^.Content));
                   55 : int_TmpStr := StrPas(PChar(elx_Globals.StringTable^[int_H1]^.Content));
                 {$ENDIF}
                 56 : int_TmpStr := Copy(Chr(int_H1), 1, 1);
               end; { case }

               {-- Imagine we cant find a spot ----------------------------}
               Stack[IdentCount - 2].I := -1;

               {-- Get a free spot ----------------------------------------}
               for TmpInt := 01 to maxFileTable do
                 if NOT FileTable[TmpInt].Used then
                   begin
                     Stack[IdentCount - 2].I := TmpInt;
                     FileTable[TmpInt].Used := TRUE;
                     BREAK;
                   end; { if }

               {-- if we found a spot, use it -----------------------------}
               if Stack[IdentCount - 2].I >= 0 then
                 begin
                   if FileTable[TmpInt].FilePtr = nil then
                     New(FileTable[TmpInt].FilePtr, Init);

                   {-- Assign the file ------------------------------------}
                   FileTable[TmpInt].FilePtr^.Assign(int_TmpStr);

                   {-- Set the sharing mode -------------------------------}
                   int_H4 := DenyNone;
                   Case int_H3 of
                     0 : int_H4 := DenyAll;
                     1 : int_H4 := DenyWrite;
                     2 : int_H4 := DenyRead;
                     3 : int_H4 := DenyNone;
                   end; { case }

                   {-- Set the filemode -----------------------------------}
                   Case int_H2 of
                     0 : FileTable[TmpInt].FilePtr^.FileMode := ReadMode + int_H4;
                     1 : FileTable[TmpInt].FilePtr^.FileMode := WriteMode + int_H4;
                     2 : FileTable[TmpInt].FilePtr^.FileMode := ReadWriteMode + int_H4;
                       else FileTable[TmpInt].FilePtr^.FileMode := ReadWriteMode + int_H4;
                   end; { case }
                 end { if }
                   else Stack[IdentCount - 2].I := -1;

               {-- Dispose of the string ----------------------------------}
               if int_Param2 = 55 then
                 StrFreeString(elx_Globals, int_H1);

               {-- Decrease the stack -------------------------------------}
               Dec(IdentCount, 2);
             end; { if }

        57,                                                 { FileGetError }
        61,                                                  { FileGetSize }
        62,                                                   { FileGetPos }
        64 : begin                                             { FileExist }
               int_H1 := Stack[Identcount].I;                { Get the handle }

               {-- Sanity check the value ---------------------------------}
               if int_H1 < 1 then                             { Invalid handle }
                 begin
                   int_H2 := -1
                 end { if }
                   else begin
                          Case int_Param2 of
                            57 : int_H2 := FileTable[int_H1].FilePtr^.IoResult;
                            61 : int_H2 := FileTable[int_H1].FilePtr^.FileSize;
                            62 : int_H2 := FileTable[int_H1].FilePtr^.FilePos;
                            64 : int_H2 := Integer(FileTable[int_H1].FilePtr^.Exists);
                          end; { case }
                        end; { case }

               {-- Return the value ---------------------------------------}
               Stack[IdentCount].I := int_H2;
             end; { FileGetError }

        58,                                                     { FileOpen }
        59,                                                   { FileCreate }
        60,                                                   { FileDelete }
        63 : begin                                             { FileClose }
               int_H1 := Stack[Identcount].I;                { Get the handle }

               {-- Sanity check the value ---------------------------------}
               if int_H1 < 1 then                             { Invalid handle }
                 begin
                   int_H2 := -1
                 end { if }
                   else begin
                          Case int_Param2 of
                            58 : FileTable[int_H1].FilePtr^.Open(1);
                            59 : FileTable[int_H1].FilePtr^.Create(1);
                            60 : FileTable[int_H1].FilePtr^.Erase;
                            63 : FileTable[int_H1].FilePtr^.Close;
                          end; { case }
                        end; { else }

               {-- Return the value ---------------------------------------}
               Dec(IdentCount);
             end; { FileOpen, FileCreate, etc }

        65 : begin                                              { FileSeek }
               {-- First get the parameters -------------------------------}
               int_H1 := Stack[IdentCount - 1].I;       { Get the filehandle }
               int_H2 := Stack[IdentCount].I;             { Get the position }

               {-- Sanity check the value ---------------------------------}
               if int_H1 >= 1 then                            { Invalid handle }
                 begin
                   FileTable[int_H1].FilePtr^.Seek(int_H2);
                 end; { if }

               {-- Decrease the stack -------------------------------------}
               Dec(IdentCount, 2);
             end; { fileseek }

        66,                                           { FileRename (Char) }
        67 : begin                                  { FileRename (String) }
               int_H1 := Stack[Identcount - 1].I;           { Get the handle }
               int_H2 := Stack[IdentCount].I;

               {-- Get the new filename -----------------------------------}
               Case int_Param2 of
                 66 : int_TmpStr := Copy(Chr(int_H2), 1, 1);
                 {$IFDEF MSDOS}
                   67 : int_TmpStr := Copy(elx_Globals.StringTable^[int_H2]^.Content, 1, StringTable^[int_H2]^.Len);
                 {$ELSE}
                   67 : int_TmpStr := Copy(StrPas(PChar(elx_Globals.StringTable^[int_H2]^.Content)), 1,
                                         elx_Globals.StringTable^[int_H2]^.Len);
                 {$ENDIF}
               end; { case }

               {-- Sanity check the value ---------------------------------}
               if int_H1 >= 1 then                            { Invalid handle }
                 begin
                   FileTable[int_H1].FilePtr^.Rename(int_TmpStr);
                 end; { if }

               {-- Decrease the stack -------------------------------------}
               Dec(IdentCount, 2);
             end; { if }

        68,                                                 { FileReadStr }
        74 : begin                                       { FileReadString }
               int_H1 := Stack[Identcount - 2].I;           { Get the handle }
               int_H3 := Stack[IdentCount].I; { this is filled by op_LoadAddr or op_LoadValue }
  { H3 contains the addr/level position where the variable is to be found }
               int_H2 := Stack[int_H3].i;

               {-- Get the string to write --------------------------------}
               int_TmpStr := '';

               {-- Sanity check the value ---------------------------------}
               if int_H1 >= 1 then                            { Invalid handle }
                 Case int_Param2 of
                   68 : {$IFDEF MSDOS}
                           FileTable[int_H1].FilePtr^.ReadLn(int_TmpStr);
                        {$ELSE}
                           FileTable[int_H1].FilePtr^.AnsiReadLn(int_TmpStr);
                        {$ENDIF}
                   74 : {$IFDEF MSDOS}
                           FileTable[int_H1].FilePtr^.Read(int_TmpStr);
                        {$ELSE}
                           FileTable[int_H1].FilePtr^.AnsiRead(int_TmpStr);
                        {$ENDIF}
                 end; { case }

               {-- if it was a read, update the string --------------------}
               if int_Param2 in [68, 74] then
                 begin
                   {-- Dispose the current string -------------------------}
                   if int_H2 <> 0 then
                     StrFreeString(elx_Globals, int_H2);

                   {-- Create a new string --------------------------------}
                   if NOT StrAllocNew(elx_Globals, int_H2, Length(int_TmpStr)) then
                     begin
                       RunFunction := FALSE;
                       EXIT;
                     end; { if }

                   {-- Set the string -------------------------------------}
                   {$IFDEF MSDOS}
                     StringTable^[int_H2]^.Content := int_TmpStr;
                     StringTable^[int_H2]^.Len := Length(int_TmpStr);
                   {$ELSE}
                     SysUtils.StrPCopy(PChar(StringTable^[int_H2]^.Content), int_TmpStr);
                   {$ENDIF}

                   {-- link up the string ---------------------------------}
                   if Stack[int_H3].I = 0 then
                    StrLinkup(elx_Globals, int_h3);

                   {-- Now actually return the value ----------------------}
                   Stack[int_H3].I := int_H2;
                 end; { if }

               {-- Decrease the stack -------------------------------------}
               Dec(IdentCount, 3);
             end; { if }

        69,                                                { FileWriteStr }
        73 : begin                                       { FileWriteStrLn }
               int_H1 := Stack[Identcount - 1].I;           { Get the handle }
               int_H2 := Stack[IdentCount].I; { Get the string to read/write }

               {-- Get the string to write --------------------------------}
               {$IFDEF MSDOS}
                 int_TmpStr := Copy(elx_Globals.StringTable^[int_H2]^.Content, 1, elx_Globals.StringTable^[int_H2]^.Len);
               {$ELSE}
                 int_TmpStr := Copy(StrPas(PChar(elx_Globals.StringTable^[int_H2]^.Content)), 1, Stringtable^[int_h2]^.Len);
               {$ENDIF}

               {-- Sanity check the value ---------------------------------}
               if int_H1 >= 1 then                            { Invalid handle }
                 Case int_Param2 of
                  {$IFDEF MSDOS}
                     69 : FileTable[int_H1].FilePtr^.Write(int_TmpStr);
                     73 : FileTable[int_H1].Fileptr^.WriteLn(int_TmpStr);
                  {$ELSE}
                     69 : FileTable[int_H1].FilePtr^.AnsiWrite(int_TmpStr);
                     73 : FileTable[int_H1].Fileptr^.AnsiWriteLn(int_TmpStr);
                  {$ENDIF}
                 end; { case }

               {-- Decrease the stack -------------------------------------}
               Dec(IdentCount, 2);
             end; { if }
        70,                                                    { FileRead }
        71 : begin                                            { FileWrite }
               int_H1 := Stack[Identcount - 4].I;           { Get the handle }
               int_H2 := Stack[IdentCount - 2].I;      { Get the record type }
               int_H3 := Stack[IdentCount - 1].I;           { Get record var }
               int_H4 := Stack[IdentCount].I;{ 0 = Loadaddress, 1 = loadvalue }


               {-- Sanity check the value ---------------------------------}
               if int_H1 >= 1 then                            { Invalid handle }
                 begin
                   {-- Read or write the buffer ---------------------------}
                   Case int_Param2 of
                     70 : if NOT ReadBuffer(elx_Globals, int_H1, int_H2, int_H3, (int_H4 = 0)) then
                            begin
                              int_ProgState := REDCHK;
                              RunFunction := FALSE;
                              EXIT;
                            end; { if }
                     71 : WriteBuffer(elx_Globals, int_H1, int_H2, int_H3, (int_H4 = 0));
                   end; { case }
                 end; { if }

               {-- Decrease the stack -------------------------------------}
               Dec(IdentCount, 5);
             end; { FileRead, FileWrite }

        72 : begin                                              { FileDone }
               int_H1 := Stack[Identcount].I;                { Get the handle }

               {-- Sanity check the value ---------------------------------}
               if int_H1 < 1 then                             { Invalid handle }
                 begin
                   int_H2 := -1
                 end { if }
                   else begin
                          {!!Dispose(FileTable[int_H1].FilePtr, Done); }
                          {!!FileTable[int_H1].FilePtr := NIL;}
                          FileTable[int_H1].FilePtr^.SetError(0);
                          FileTable[int_H1].Used := FALSE;
                        end; { else }

               {-- Return the value ---------------------------------------}
               Dec(IdentCount);
             end; { FileDone }

        75 : begin                                           { Concatenate }
               int_H3 := Stack[Identcount - 1].I;    { Get the dest string }
               int_H1 := Stack[int_h3].i;
               int_H2 := Stack[IdentCount].I;      { get the append string }
               int_Param1 := ProgCode[int_PrevPc].X;  { is it str or char? }

               {-- Create a new string ------------------------------------}
               if int_H1 = 0 then
                 if NOT StrAllocNew(elx_Globals, int_H1, 1024) then
                   begin
                     RunFunction := FALSE;
                     EXIT;
                   end; { if }

               {-- and append "h2" to "h1" --------------------------------}
               if int_Param1 AND 3 = 0 then                         { Char }
                 begin
                   StringTable^[int_H1]^.Len := StringTable^[int_H1]^.Len + 1;

                   {$IFDEF MSDOS}
                     StringTable^[int_H1]^.Content := StringTable^[int_H1]^.Content + Stack[IdentCount].C;
                   {$ELSE}
                     if StringTable^[int_H1]^.AllocSz <=
                       (StringTable^[int_H1]^.Len + 1) then
                          StrGrowString(elx_Globals, int_H1,
                                        StringTable^[int_H1]^.Len + 2);

                     PChar(StringTable^[int_H1]^.Content)[StringTable^[int_H1]^.Len - 1] := Stack[IdentCount].C;
                     PChar(StringTable^[int_H1]^.Content)[StringTable^[int_H1]^.Len] := #0;
                   {$ENDIF}
                 end
                   else begin                                     { String }
                          int_H4 := StringTable^[int_H1]^.Len;{ Save the len }
                          Inc(StringTable^[int_H1]^.Len, StringTable^[int_H2]^.Len);

                          {$IFDEF MSDOS}
                             StringTable^[int_H1]^.Content :=
                                StringTable^[int_H1]^.Content +
                                StringTable^[int_H2]^.Content;
                          {$ELSE}
                             if StringTable^[int_H1]^.AllocSz <=
                               (StringTable^[int_H1]^.Len + 1) then
                                  StrGrowString(elx_Globals, int_H1,
                                                StringTable^[int_H1]^.Len + 2);

                             Move(StringTable^[int_H2]^.Content^,
                                     PChar(StringTable^[int_H1]^.Content)[int_H4],
                                     StringTable^[int_H2]^.Len);
                          {$ENDIF}

                          if (int_Param1 AND 4) = 4 then
                           begin
                            StrFreeString(elx_Globals, int_H2);
                           end; { if }
                        end; { string }

               {-- link up the string -------------------------------------}
               if Stack[int_H3].I = 0 then
                 StrLinkup(elx_Globals, int_h3);

               {-- Now actually return the value --------------------------}
               Stack[int_H3].I := int_H1;

               {-- 'free' the variables -----------------------------------}
               Dec(IdentCount, 3);
             end; { if }

        76 : begin                         { expand_vars }
               {-- get the amount of parameters -----------------------------}
               int_Param1 := ProgCode[int_PrevPc].X;  { amount of parameters }

               {-- now walk through this string, replacing all $'s ----------}
               {$IFNDEF MSDOS}
               ExpandStr := ExpandVars(StrPas(PChar(StringTable^[Stack[IdentCount - 1].S]^.Content)),
                                        Stack[IdentCount].I);

               {-- Get the length -------------------------------------------}
               int_H2 := Length(ExpandStr);
               {$ENDIF}

               {-- Create a new string --------------------------------------}
               if NOT StrAllocNew(elx_Globals, int_H1, int_H2) then
                 begin
                   RunFunction := FALSE;
                   EXIT;
                 end; { if }

               {-- Set the string -------------------------------------------}
               {$IFDEF MSDOS}
                 elx_Globals.StringTable^[int_H1]^.Content := '?';
               {$ELSE}
                 StrPCopy(PChar(elx_Globals.StringTable^[int_H1]^.Content), ExpandStr);
                 elx_Globals.StringTable^[int_H1]^.Len := Length(ExpandStr);
               {$ENDIF}

               {-- and free the allocated variables -------------------------}
               Dec(IdentCount, 1);

               {-- Now actually return the value ----------------------------}
               Stack[IdentCount].I := int_H1;
             end; { expand_var }


      end { case }
        else begin
                int_ProgState := SysChk;
                RunFunction := FALSE;
                EXIT;
             end; { else }
     end; { with }

      {$IFDEF ELX_PROFILE}
         FuncCount[int_Param2].msecs := FuncCount[int_Param2].msecs + (elx_profgetmsec - RunUserTmp);
      {$ENDIF}
   end; { proc. RunFunction }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure int_op_LoadAddress(var elx_Globals: elx_GlobalsType);
begin
                                                          { int_Param1: Level }
                                                        { int_Param2: Address }
  with elx_Globals do
    begin
      int_Param1 := ProgCode[int_PrevPC].X;

      Inc(IdentCount);

      if IdentCount > StackSize then
        begin
          int_ProgState := StkChk;
          int_Continue := false;
        end { if }
          else begin
                 Stack[IdentCount].I := Display^[int_Param1] + int_Param2;
               end; { else }
    end; { with }
end; { op_LoadAddress }


procedure int_op_LoadValue(var elx_Globals: elx_GlobalsType);
begin
                                                          { int_Param1: Level }
                                                        { int_Param2: Address }
  with elx_Globals do
    begin
      Inc(IdentCount);
      Stack[IdentCount] := Stack[Display^[ProgCode[int_PrevPC].X] + ProgCode[int_PrevPC].Y];
    end; { with }
end; { op_LoadValue }


procedure int_op_LoadIndirect(var elx_Globals: elx_GlobalsType);
begin
                                                          { int_Param1: Level }
                                                        { int_Param2: Address }

                                                { Get the *current* level }
  with elx_Globals do
    begin
      int_Param1 := ProgCode[int_PrevPC].X;

      Inc(IdentCount);{ Lets try to push it on the stack }
      if IdentCount > StackSize then
        begin
          int_ProgState := StkChk;

          int_Continue := FALSE;
        end; { if }

                       {-- int_Param2 is the identifier number of this level --}
                       {-- but because its a reference to a variable ------}
                       {-- defined in a later display, we have to look in -}
                       {-- in another "Display[xx]" -----------------------}
      int_H1 := Display^[int_Param1] + int_Param2;

                       {-- Now we know where the actual value of this -----}
                       {-- variable is. Lets look this up -----------------}
                       {-- H1 now contains a pointer to where the real ----}
                       {-- value is. Look this up -------------------------}
      int_H1 := Stack[int_H1].I;

      {-- Now actually retrieve the value ----------------}
      Stack[IdentCount] := Stack[int_H1];
  end; { with }
end; { op_LoadIndirect }

procedure int_op_UpdateDisplay(var elx_Globals: elx_GlobalsType);
begin                                                { Updates the display }
                 { if a procedure is called which is at a lower level than }
                      { the executing procedure, the Display[xx] could get }
                        { overwritten. This routine makes sure the current }
                                                            { level is set }
                                                       { int_Param1: Level }
                                                     { int_Param2: Address }

                                                    { Set the code counter }
  with elx_Globals do
    begin
      int_Param1 := Progcode[int_PrevPC].X;

                       {-- Now set the variables --------------------------}
      int_H1 := int_Param2;             { Level were currently at }
      int_H2 := int_Param1;             { level of this procedure }
      int_H3 := int_BaseIndex;{ Current stack pointer of this proc }

                       {-- Now loop to remove all "dead" displays ---------}
      REPEAT
        Display^[int_H1] := int_H3;       { Set current display to }
                                            { first variable of this block }

        Dec(int_H1);                     { This level is done }

                                                          { Static link }
                                                    { this is the first }
        int_H3 := Stack[int_H3 + 2].I;         { variable of this level }
      UNTIL (int_H1 = int_H2);
    end; { with }
end; { op_UpdateDisplay }


procedure int_op_Concatenate(var elx_Globals: elx_GlobalsType);
begin
                                                          { int_Param1: [n/a] }
                                                 { int_Param2: type of concat }


{$IFDEF ELX_PROFILE}
inc(concatcount[(elx_Globals.int_Param2 and 3)].count);
concattmp := elx_profgetmsec;
{$ENDIF}

  with elx_Globals do
    begin
      Case (int_Param2 AND 3) of
        0 : begin
              {-- Allocate a string for 2 chars ----------}
              if NOT StrAllocNew(elx_Globals, int_H1, 2) then
                begin
                  int_ProgState := StrChk;
                  int_Continue := FALSE;
                end; { if }

                {-- Set the values -------------------------}
                {$IFDEF MSDOS}
                  int_TmpStr := Chr(Stack[IdentCount-1].I) +
                           Chr(Stack[IdentCount].I);
                  StringTable^[int_H1]^.Content := Copy(int_TmpStr, 1, 2);
                {$ELSE}
                  PChar(StringTable^[int_H1]^.Content)[0] := Chr(Stack[IdentCount - 1].i);
                  PChar(StringTable^[int_H1]^.Content)[1] := Chr(Stack[IdentCount].i);
                  PChar(StringTable^[int_H1]^.Content)[2] := #0;
                {$ENDIF}
                 StringTable^[int_H1]^.Len := 2;

                               {-- Dispose of the temp vars ---------------}
              Dec(IdentCount);

                               {-- Set the string index -------------------}
              Stack[IdentCount].I := int_H1;
            end; { Char + Char }

        1 : begin              {-- Get the original string ----------------}
              int_H1 := Stack[IdentCount - 1].I;
              int_H2 := StringTable^[int_H1]^.Len;

              {-- Allocate a string for s + 1char --------}
              if NOT StrAllocNew(elx_Globals, int_H3, int_H2 + 1) then
                begin
                  int_ProgState := StrChk;
                  int_Continue := FALSE;
                end; { if }

              {-- Now set the value back to H3 -----------}
              {$IFDEF MSDOS}
                StringTable^[int_H3]^.Content :=
                   Copy(StringTable^[int_H1]^.Content, 1, StringTable^[int_H1]^.Len) +
                   Chr(Stack[IdentCount].I);
                StringTable^[int_H3]^.Len := Length(StringTable^[int_H3]^.Content);
              {$ELSE}
                Move(StringTable^[int_H1]^.Content^,
                     StringTable^[int_H3]^.Content^,
                     StringTable^[int_H1]^.Len);

                PChar(StringTable^[int_H3]^.Content)[StringTable^[int_H1]^.Len] :=
                     Chr(Stack[Identcount].i);
                PChar(Stringtable^[int_H3]^.Content)[Stringtable^[int_H1]^.Len + 1] := #0;
                StringTable^[int_H3]^.Len := StringTable^[int_H1]^.Len + 1;
              {$ENDIF}


              {-- Dispose of this string if necessary ----}
              if (int_Param2 AND 4) = 4 then
                 StrFreeString(elx_Globals, int_H1);

              {-- Decrease the counter -------------------}
              Dec(IdentCount);

              {-- Set the actual string index ------------}
              Stack[IdentCount].I := int_H3;
            end; { String + Char }

        2 : begin
              {-- Get the original string ----------------}
              int_H1 := Stack[IdentCount].I;
              int_H2 := StringTable^[int_H1]^.Len;

              {-- Allocate a string for s + 1char --------}
              if NOT StrAllocNew(elx_Globals, int_H4, int_H2 + 1) then
                begin
                  int_ProgState := StrChk;
                  int_Continue := FALSE;
                end; { if }

              {-- Get the original value, and add the ch -}
              {$IFDEF MSDOS}
                int_TmpStr := Copy(StringTable^[int_H1]^.Content, 1, StringTable^[int_H1]^.Len);
              {$ELSE}
                int_TmpStr := Copy(StrPas(PChar(StringTable^[int_H1]^.Content)), 1, StringTable^[int_H1]^.Len);
              {$ENDIF}

              int_TmpStr := Chr(Stack[IdentCount - 1].I) + int_TmpStr;

              {-- Now set the value back to H3 -----------}
              {$IFDEF MSDOS}
                  StringTable^[int_H4]^.Content := int_TmpStr;
              {$ELSE}
                StrPCopy(PChar(StringTable^[int_H4]^.Content), int_TmpStr);
              {$ENDIF}
              StringTable^[int_H4]^.Len := Length(int_TmpStr);

              {-- Dispose of this string if necessary ----}
              if (int_Param2 AND 8) = 8 then
                StrFreeString(elx_Globals, int_H1);

              {-- Decrease the counter -------------------}
              Dec(IdentCount);

              {-- Set the actual string index ------------}
              Stack[IdentCount].I := int_H4;
            end; { Char + String }

        3 : begin
              {-- Get the original strings ---------------}
              int_H5 := Stack[IdentCount - 1].I;
              int_H3 := StringTable^[int_H5]^.Len;

              int_H6 := Stack[IdentCount].I;
              int_H4 := StringTable^[int_H6]^.Len;

              {-- Allocate a string for s + string -------}
              if NOT StrAllocNew(elx_Globals, int_H2, int_H3 + int_H4) then
                begin
                  int_ProgState := StrChk;
                  int_Continue := FALSE;
                end; { if }

              {-- Get the original values and add them ---}
              {$IFDEF MSDOS}
                int_TmpStr := Copy(StringTable^[int_H5]^.Content, 1, StringTable^[int_H5]^.Len);
                int_TmpStr := int_TmpStr + Copy(StringTable^[int_H6]^.Content, 1, StringTable^[int_H6]^.Len);

                {-- Now set the actual value -------------}
                StringTable^[int_H2]^.Content := int_TmpStr;
                StringTable^[int_H2]^.Len := Length(int_TmpStr);
              {$ELSE}
{$IFDEF ELX_PROFILE}
inc(concatcount[(4)].count);
concattmp := elx_profgetmsec;
{$ENDIF}

                {-- make sure this string is not the nulstring ---------------}
                if int_H5 <> elx_Globals.NulString then
                  begin
                    Move(StringTable^[int_H5]^.Content^,
                         StringTable^[int_H2]^.Content^,
                         int_H3);
                  end { if }
                    else int_H3 := 0;


{$IFDEF ELX_PROFILE}
concatcount[(4)].msecs := concatcount[(4)].msecs + (elx_profgetmsec - concattmp);
{$ENDIF}


                Move(StringTable^[int_H6]^.Content^,
                     PChar(StringTable^[int_H2]^.Content)[int_H3],
                     int_H4);

                {-- Update the 0 terminator --------------}
                PChar(StringTable^[int_H2]^.Content)[int_H3 + int_H4] := #0;
                StringTable^[int_H2]^.Len := int_H3 + int_H4;
              {$ENDIF}

              {-- Dispose of this string if necessary ----}
              if (int_Param2 AND 4) = 4 then
                  StrFreeString(elx_Globals, int_H5);

              {-- Dispose of this string if necessary ----}
              if (int_Param2 AND 8) = 8 then
                  StrFreeString(elx_Globals, int_H6);

              {-- Decrease the counter -------------------}
              Dec(IdentCount);

              {-- Set the actual string index ------------}
              Stack[IdentCount].I := int_H2;
            end; { '3' }
      end; { case }

{$IFDEF ELX_PROFILE}
concatcount[(int_Param2 and 3)].msecs := concatcount[(int_Param2 and 3)].msecs + (elx_profgetmsec - concattmp);
{$ENDIF}

    end; { with }
end; { proc. op_Concatenate }


procedure int_op_RunFunction(var elx_Globals: elx_GlobalsType);
begin
  RunFunction(elx_Globals,                            { int_Param1: [n/a] }
              elx_Globals.int_Param2,
              elx_Globals.int_CallObjptr);
                                      { int_Param2: actual routine to run }
end; { proc. int_op_RunFunction }

procedure int_op_Offset(var elx_Globals: elx_GlobalsType);
begin
                                                                  { value }
                                                    { Example: t = record }
                                               { offset 0 ->  t1: integer }
                                               { offset 1 ->  t2: integer }
                                { offset 2 ->  t3: array[1..2] of integer }
                                               { offset 4 ->  t3: integer }
                                                          { int_Param1: [n/a] }
                                                          { int_Param2: value }
  with elx_Globals do
    Stack[IdentCount].I := Stack[IdentCount].I + int_Param2;   { offset }
end; { proc. op_Offset }

procedure int_op_JumpTo(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do                                     { int_Param1: [n/a] }
    int_ProgPtr := int_Param2;                            { int_Param2: value }
end; { op_JumpTo }


procedure int_op_ConditionalJmp(var elx_Globals: elx_GlobalsType);
begin                           { Conditional jump, if NOT then jump to that }
                                 { Address. The result is pushed on the stack }
                                                          { int_Param1: [n/a] }
                                                          { int_Param2: value }
  with elx_Globals do
    begin
      if NOT Stack[IdentCount].B then
        int_ProgPtr := int_Param2;

      Dec(IdentCount);
    end; { with }
end; { proc. op_ConditionalJmp }

procedure int_op_Switch(var elx_Globals: elx_GlobalsType);
begin
                                                          { int_Param1: [n/a] }
                            { int_Param2: jump-position after the case labels }

                            { Does this case statement have an else block }
  with elx_Globals do
    begin
      int_Param1 := ProgCode[int_PrevPC].X;    { The value to compare with }

      int_H1 := Stack[IdentCount].I;
      int_H2 := int_Param2;          { Set the position to jump to }
      int_H3 := 0;
      Dec(Identcount);

                     {-- Now loop through all labels ----------------------}
      REPEAT
                       {-- Error out if the case label isnt found ---------}
        if ProgCode[int_H2].Func <> 13 then
          begin
            int_ProgState := CASCHK;
            int_Continue := FALSE;
          end; { if }

                       {-- Lets see if this one matches -------------------}
        if ProgCode[int_H2].Y = int_H1 then
          begin
            int_H3 := 1;          { Mark the exit of this loop }

                                                  { Jump to this code block }
            int_ProgPtr := ProgCode[int_H2 + 1].Y;
          end { if }
            else begin
                   Inc(int_H2, 2);
                 end; { else }

                         {-- If we have an else statement -------------------}
        if (int_Param1 = 1) AND (int_H3 = 0) then
          begin
                             {-- Check wether this is the last statement ----}
            if ProgCode[int_H2 + 2].Func <> 13 then
              begin
                ProgCode[int_H2].Y := int_H1;
              end; { if }
          end; { if }

      UNTIL (int_H3 <> 0);
    end; { with }
end; { op_Switch }


procedure int_op_For1Up(var elx_Globals: elx_GlobalsType);
begin
                                                          { int_Param1: [n/a] }
                                                          { int_Param2: value }
                          { This code ets executed twice. Once the first time }
                         { the FOR loop is run, and once when the FOR loop is }
                                                      { run for the last time }

  with elx_Globals do
    begin
                                       { Assign the "current" value to H1 }
      int_H1 := Stack[IdentCount - 1].I;

                      { Make sure the current value isn't > the max value }
      if int_H1 <= Stack[IdentCount].i then
        Stack[Stack[IdentCount - 2].I].I := int_H1
          else begin
                                {-- Jump to AFTER the loop ----------------}
                 Dec(IdentCount, 3);
                 int_ProgPtr := int_Param2
               end; { else }
    end; { with }
end; { op_For1Up }


procedure int_op_For2Up(var elx_Globals: elx_GlobalsType);
begin
                                                          { int_Param1: [n/a] }
                                                          { int_Param2: value }
                                    { This code gets executed a few times }
                               { its placed after the actual code to loop }
                            { and it returns back to the loop if the loop }
                                   { isnt done yet. If it is through, the }
                                   { stack gets emptied, and we fall thru }
  with elx_Globals do
    begin
                          { H2 is the POINTER to the actual loop variable }
      int_H2 := Stack[IdentCount - 2].I;
                                     { Assign the actual value (+1) to H1 }
      int_H1 := Stack[int_H2].I + 1;

                     {-- Check if the loop should be continued or not -----}
      if int_H1 <= Stack[IdentCount].I then
        begin
                            { We just "upped" H1, lets assign this to the }
                          { actual value of the loop so we dont get stuck }
          Stack[int_H2].I := int_H1;

                              { Now jump back to the code we have to loop }
          int_ProgPtr := int_Param2;
        end
          else Dec(IdentCount, 3);
    end; { with }
end; { op_For2Up }


procedure int_op_For1Down(var elx_Globals: elx_GlobalsType);
begin
                                                          { int_Param1: [n/a] }
                                                          { int_Param2: value }
                     { This code gets executed twice. Once the first time }
                     { the FOR loop is run, and once when the FOR loop is }
                     { run for the last time }

  with elx_Globals do
    begin
                                       { Assign the "current" value to H1 }
      int_H1 := Stack[IdentCount - 1].I;

                      { Make sure the current value isn't < the max value }
      if int_H1 >= Stack[IdentCount].i then
        Stack[Stack[IdentCount - 2].I].I := int_H1
          else begin
                                {-- Jump to AFTER the loop ----------------}
                 Dec(IdentCount, 3);
                 int_ProgPtr := int_Param2
               end; { else }
    end; { with }
end; { op_For1Down }


procedure int_op_For2Down(var elx_Globals: elx_GlobalsType);
begin
                                                      { int_Param1: [n/a] }
                                                      { int_Param2: value }
                                    { This code gets executed a few times }
                               { its placed after the actual code to loop }
                            { and it returns back to the loop if the loop }
                                   { isnt done yet. If it is through, the }
                                   { stack gets emptied, and we fall thru }

  with elx_Globals do
    begin
                          { H2 is the POINTER to the actual loop variable }
      int_H2 := Stack[IdentCount - 2].I;
                                     { Assign the actual value (-1) to H1 }
      int_H1 := Stack[int_H2].I - 1;

                     {-- Check if the loop should be continued or not -----}
      if int_H1 >= Stack[IdentCount].I then
        begin
                            { We just "upped" H1, lets assign this to the }
                          { actual value of the loop so we dont get stuck }
          Stack[int_H2].I := int_H1;

                              { Now jump back to the code we have to loop }
          int_ProgPtr := int_Param2;
        end
          else Dec(IdentCount, 3);
    end; { with }
end; { op_For2Down }

procedure int_op_MarkStack(var elx_Globals: elx_GlobalsType);
begin
                     { it also sets up the needed environment for calling }
                                              { a function or a procedure }
                                                      { int_Param1: [n/a] }
                                  { int_Param2: Identifier-# being called }

                              { Retrieve the total size of all parameters }
            { This is 7 (variables used for calling a function/procedure) }
                    { and all extra variables both declared in the proc's }
                      { parameter list, and those used as local variables }
                              { * the size of each variable. This size is }
                          { determined by the "BLOCK.PAS" in the variable }
                                                              { DataAlocd }
  with elx_Globals do
    begin
      int_H1 := BlockTable^[IdentTable^[int_Param2].Ref].vSize;

                      { Now make sure that the current variables, and the }
                       { additional overhead of the called variables wont }
                                                     { overflow the stack }
      if (IdentCount + int_H1) > StackSize then
        begin
          int_ProgState := STKCHK;
          int_Continue := false;
        end; { if }


                    { Now already allocate the first 7 parameters we need }
                               { to actually call this procedure/function }
      Inc(IdentCount, 7);

                   { Lets setup the stack so that we have the information }
                                                                { we want }
      int_BaseZero := IdentCount;

      {-- Now save this base zero value, in case we nest function calls---}
      {-- eg if we use: --------------------------------------------------}
      {---- MyFunc(MyFunc2(s), s); ---------------------------------------}
      {-- the 2nd markstack will overwrite our basezero, making linking --}
      {-- of the strings, impossible, and thus causing string exhaustion--}
      {-- by saving this value, we fix this. We could also pass it as a---}
      {-- stack parameter, but this is easier ----------------------------}
      if BaseZeroIdx < MaxLevel then
        begin
          inc(BaseZeroIdx);
          BaseZeroArray^[BaseZeroIdx] := int_BaseZero;
        end
          else begin
                 int_ProgState := basezchk;
                 int_Continue := false;
               end; { else }

{WriteLn('[',ick:2,'] int_BaseZero - set - ', int_BaseZero, ' [free count=',
StrGetFreeCount(elx_Globals),'] Str should be at: ',int_baseZero-1);}
{writeln('op_MarkStack (basezer= ', int_BaseZero - 6);}

                                                  { (B + 6). StrArray ptr }
                         { Exactly the same principle as regular strings, }
                                { only stringarrays are linked per array. }
      Stack[int_BaseZero - 0].I := 0;

                                                { (B + 5). String pointer }
                   { All strings are linked, and B+5 indicates the latest }
                         { defined string. By using a linked-list, we can }
                          { walk our way back up, and dispose all strings }
                                                       { while were at it }
                                     { a value of '0' means "end-of-list" }
      Stack[int_BaseZero - 1].I := 0;

                          { (B + 4). Identifier it's actually called from }
      Stack[IdentCount - 2].i := int_Param2;

                                                  { (B + 3). Dynamic link }
                                   { Total size of parameters (see above) }
      Stack[IdentCount - 3].I := (int_H1 - 1);
    end; { with }
end; { op_MarkStack }


procedure int_op_Call(var elx_Globals: elx_GlobalsType);
begin
                                                      { int_Param1: [n/a] }
                                    { int_Param2: size of procedure block }

                { int_Param2 is the size of the declarations in the block }
                             { H1 will point to where this procedure will }
                                                    { be defined at first }
  with elx_Globals do
    begin
      int_H1 := IdentCount - int_Param2;       { H1 points to base }

                                     { Make H2 point to the "table index" }
                                          { we look up the actual data of }
                                        { this variable in the IdentTable }
      int_H2 := Stack[int_H1 + 04].I;

                            { Lets see which level *this* procedure is in }
      int_H3 := IdentTable^[int_H2].Lev;

                        { Setup the Display. Display[level] is set to the }
                           { last variable we currently have available in }
                                                             { this block }
      Display^[int_H3 + 1] := int_H1;

                                     { Now set H4. We take "Dynamic link" }
                                          { basically this is the size of }
                                                 { all parameters defined }
                                     { plus the counter where this starts }
      int_H4 := Stack[int_H1 + 3].I + int_H1;

                       { First we set the address to return to after this }
                                            { procedure/function is ended }
      Stack[int_H1 + 1].I := int_ProgPtr;         { Return address }

                     { Now set this to the first variable in this display }
      Stack[int_H1 + 2].I := Display^[int_H3];       { Static link }

                                { Now save the current index to the stack }
      Stack[int_H1 + 3].I := int_BaseIndex;         { Dynamic link }
                                                        { Clear the stack }
      FillChar(Stack[IdentCount + 1],
               (int_H4 - IdentCount) * SizeOf(Stack[1]),
               0);

      {-- BaseIndex now points to the 6 parameters as ------}
      {-- configured by "op_Call" and "op_MarkStack" -------}
      int_BaseIndex := int_H1;

      {!!} {-- Now clear the default string return --------------}
      {!!} FillChar(Stack[int_BaseIndex], SizeOf(Stack[1]), 0);


(*
writeln('[', ick:2,'] op_Call = ', int_H1 + 6, ' (Stack[',int_h1+5,'] = ', stack[int_h1+5].i,']');
*)

      {-- Now set IdentCount after the declaration of all --}
      {-- parameters ---------------------------------------}
      IdentCount := int_H4;

      {-- Set the program counter to jump to the actual ----}
      {-- procedure ----------------------------------------}
      int_ProgPtr := IdentTable^[int_H2].Adr;

      {-- If enabled, dump stack before proceeding ---------}
      if StackDump then
       if LogErrors then DoStackDump(elx_Globals);
    end; { with }

{writeln('op_Call (passed val.)= ', elx_Globals.int_BaseIndex);}
end; { op_Call }


procedure int_op_Index1(var elx_Globals: elx_GlobalsType);
begin
                   { we set the Stack[IdentCount] to the actual reference }
                                          { where the value we want is at }
                                                      { int_Param1: [n/a] }
                              { int_Param2: pointer to array table index  }
  with elx_Globals do
    begin
      int_H1 := int_Param2;              { Index to the array table }
      int_H2 := ArrayTable^[int_H1].ArrayInf.Low; { Low index of this array }
      int_H3 := Stack[IdentCount].I;     { The element we want }

      {-- Now lets range-check -----------------------------}
      if int_H3 < int_H2 then
        begin
          int_ProgState := InxChk;
          int_Continue := false;
        end; { if }

      if int_H3 > ArrayTable^[int_H1].ArrayInf.High then
        begin
          int_ProgState := InxChk;
          int_Continue := false;
        end; { if }

      {-- Now lets execute ---------------------------------}
      Dec(Identcount);
      Stack[Identcount].I := Stack[IdentCount].I + (int_H3 - int_H2);
    end; { with }
end; { op_Index1 }


procedure int_op_Index(var elx_Globals: elx_GlobalsType);
begin
                   { we set the Stack[IdentCount] to the actual reference }
                                          { where the value we want is at }
                                                          { int_Param1: [n/a] }
                                  { int_Param2: pointer to array table index  }
  with elx_Globals do
    begin
      int_H1 := int_Param2;             { Index to the array table }
      int_H2 := ArrayTable^[int_H1].ArrayInf.Low;  { Low index of this array }
      int_H3 := Stack[IdentCount].I;     { The element we want }

      {-- Now lets range-check -----------------------------}
      if int_H3 < int_H2 then
        begin
          int_ProgState := InxChk;
          int_Continue := false;;
        end; { if }

      if int_H3 > ArrayTable^[int_H1].ArrayInf.High then
        begin
          int_ProgState := InxChk;
          int_Continue := false;
        end; { if }

      {-- Now lets execute ---------------------------------}
      Dec(Identcount);
      Stack[IdentCount].I := Stack[IdentCount].I +
                               (int_H3 - int_H2) * ArrayTable^[int_H1].ArrayInf.ElementSize;
    end; { with }
end; { op_Index }


procedure int_op_LoadBlock(var elx_Globals: elx_GlobalsType);
begin
                                                          { int_Param1: [n/a] }
                                                           { int_Param2: size }
                                            { The actual place to copy to }
  with elx_Globals do
    begin
      int_H1 := Stack[IdentCount].I;
      Dec(IdentCount);

      { The place where we eventually will end up }
      int_H2 := int_Param2 + IdentCount;

      {-- Make sure theres enough stack --------------------}
      if int_H2 > StackSize then
        begin
          int_ProgState := StkChk;
          int_Continue := FALSE;
        end; { if }

      {-- Now start the actual copying ---------------------}
      While (IdentCount < int_H2) do
        begin
          Inc(IdentCount);

          {-- If its a string, copy it directly ------------}
          if Stack[int_H1].CN = typ_Strngs then
            begin
              Stack[IdentCount].S := 00;
              if NOT StrDupl(elx_Globals, IdentCount, int_H1) then
                int_Continue := FALSE;
            end { if }
              else Stack[IdentCount] := Stack[int_H1];

          {-- Increase H1 ----------------------------------}
          Inc(int_H1);
        end; { while }
    end; { with }
end; { op_LoadBlock }


procedure int_op_CopyBlock(var elx_Globals: elx_GlobalsType);
begin
                               { piece of data. Usefull for nested arrays }
                                                          { int_Param1: [n/a] }
                                           { int_Param2: size of data to copy }

  with elx_Globals do
    begin
                                      { H1 is the start variable to start }
                                                           { copying with }
      int_H1 := Stack[IdentCount - 1].I;

                                        { H2 is te start of the new place }
                                        { where this should be copied too }
      int_H2 := Stack[IdentCount].I;

                                { H3 is the total number of "copys" we do }
      int_H3 := int_H1 + int_Param2;              { start + size = end }

      while int_H1 < int_H3 do
        begin
          {-- If they are strings, use internal routines ---}
          if Stack[int_H2].CN = typ_Strngs then
            begin
              Stack[int_H1].S := 0;

              if NOT StrDupl(elx_Globals, int_H1, int_H2) then
                begin
                  int_Continue := FALSE;
                end; { if }
            end { not copying strings }
           {-- else use normal assignments --------------}
              else Stack[int_H1] := Stack[int_H2];

           {-- Increase origin and destination position -----}
           Inc(int_H1);
           Inc(int_H2);
        end; { while }

      Dec(IdentCount, 2);
    end; { with }
end; { op_CopyBlock }

procedure int_op_Literal(var elx_Globals: elx_GlobalsType);
begin
   with elx_Globals do                                 { int_Param1: [n/a] }
     begin                                             { int_Param2: value }
       Inc(IdentCount);

       {-- Make sure there's room in the stack -------------}
       if IdentCount > StackSize then
         begin
           int_ProgState := StkChk;
           int_Continue := FALSE;
         end; { if }


       {-- Assign the stack with this value ----------------}
       Stack[IdentCount].i := int_Param2;
     end; { with }
end; { op_Literal }

procedure int_op_LoadReal(var elx_Globals: elx_GlobalsType);
begin
   with elx_Globals do                                 { int_Param1: [n/a] }
     begin                                             { int_Param2: value }
       Inc(IdentCount);

       {-- Make sure the stack is large enough -------------}
       if IdentCount > StackSize then
         begin
           int_ProgState := StkChk;
           int_Continue := FALSE;
         end; { if }

       {-- Actually load the real onto the stack -----------}
       Stack[IdentCount].R := RealsTable[int_Param2];
     end; { with }
end; { op_LoadReal }

procedure int_op_LoadFloat(var elx_Globals: elx_GlobalsType);
begin                                                  { int_Param1: [n/a] }
                        { int_Param2: Offset to convert from current ident }
  with elx_Globals do
    begin
      int_H1 := IdentCount - int_Param2;

      Stack[int_H1].R := Stack[int_H1].I;
    end; { with }
end; { op_LoadFloat }


procedure int_op_Read(var elx_Globals: elx_GlobalsType);
var sBuff: String[80];
begin
                                                       { int_Param1: [n/a] }
                                    { int_Param2: type of variable to read }
  with elx_Globals do
  begin
    Case tType(int_Param2) of
      typ_Ints  : if Assigned(elx_UserReadHook) then
                      elx_UserReadHook(Stack[Stack[IdentCount].I].I, typ_ints, false)
                        else Read(Input, Stack[Stack[IdentCount].I].I);
      typ_Reals : if Assigned(elx_UserReadHook) then
                      elx_UserReadHook(Stack[Stack[IdentCount].I].R, typ_reals, false)
                        else Read(Input, Stack[Stack[IdentCount].I].R);
      typ_Chars : if Assigned(elx_UserReadHook) then
                      elx_UserReadHook(Stack[Stack[IdentCount].I].C, typ_chars, false)
                        else Read(Input, Stack[Stack[IdentCount].I].C);
      typ_Strngs : begin
                    {-- Read into a temporarily buffer -----------------------}
                    if Assigned(elx_UserReadHook) then
                      elx_UserReadHook(sBuff, typ_Strngs, false)
                        else Read(Input, sBuff);

                    {-- Make sure it has any length --------------------------}
                    int_H1 := Length(sBuff);
                    if int_H1 = 0 then int_H3 := NulString
                      else begin
                             {-- Get memory for H3 of H1 ---------------------}
                             {-- length --------------------------------------}
                             if NOT StrAllocNew(elx_Globals, int_h3, int_h1) then
                               begin
                                 int_ProgState := StrChk;
                                 int_Continue := FALSE;
                               end; { if }

                             {-- Now put the string in -----------------------}
                             {-- H3 = String index ---------------------------}
                             {-- 1 = Position to insert ----------------------}
                             {-- sBuff = Actual insert -----------------------}
                             {-- H1 = Number to ins --------------------------}
                             {$IFDEF MSDOS}
                               StringTable^[int_H3]^.Content := sBuff;
                             {$ELSE}
                               StrPCopy(PChar(StringTable^[int_H3]^.Content), sBuff);
                             {$ENDIF}
                             StringTable^[int_H3]^.Len := Length(sBuff);
                           end; { else }

                    {-- Now link up this string ------------------------------}
                    {-- to this procedure ------------------------------------}

                                       { H4 is the variable number to read in }
                    int_H4 := Stack[IdentCount].I;
                                             { H5 is the actual value of this }
                    int_H5 := Stack[int_H4].I;

                                        {-- If current string is empty, ------}
                                        {-- add it to the line of strings ----}
                    if int_H5 = 0 then
                      begin
                        StrLinkUp(elx_Globals, int_H4)
                      end
                        else begin
                                                   { Free up the old instance }
                               if int_H5 <> 0 then                { dont free }
                                                                    { the nil }
                                 StrFreeString(elx_Globals, int_H5);
                             end; { else }

                    {-- Now add it -------------------------------------------}
                    Stack[int_H4].I := int_H3;
                  end; { typ_Strngs }
    end; { case }

    Dec(IdentCount);
  end; { with }
end; { proc. int_op_Read }



procedure int_op_Write1(var elx_Globals: elx_GlobalsType);
begin
                                                       { int_Param1: [n/a] }
                                                   { int_Param2: Fieldtype }
                                                { Use default fieldlengths }
  with elx_Globals do
  begin
    {-- First convert the value to a string --------------}
    int_TmpStr := '';

    Case int_Param2 of
      01 : Str(Stack[IdentCount].I, int_TmpStr);
      02 : Str(Stack[IdentCount].R, int_TmpStr);
      03 : if Stack[IdentCount].B then
             int_TmpStr := 'TRUE'
               else int_TmpStr := 'FALSE';
      04 : int_TmpStr := Copy(Chr(Stack[IdentCount].I), 1, 1);
    end; { case }

    {-- Actually write the values ------------------------}
    if Assigned(elx_UserFuncsHook) then
      elx_UserWriteHook(int_TmpStr, false)
        else Write(int_TmpStr);

    {-- Dispose of the temp variable ---------------------}
    Dec(IdentCount);
  end; { with }
end; { op_Write1 }


procedure int_op_Write2(var elx_Globals: elx_GlobalsType);
begin
                                                      { int_Param1: [n/a] }
                                                  { int_Param2: Fieldtype }
                                         { The stack is filled like this: }
                                                 { Stack[0] := fieldvalue }
                                                   { Stack[1] := fieldlen }
                                  { stackpointer (IdentCount) is set at 1 }

                                                { Use default fieldlengths }
  with elx_Globals do
  begin
    {-- First convert the value to a string --------------}
    int_TmpStr := '';

    Case int_Param2 of
      01 : Str(Stack[IdentCount - 1].I: Stack[IdentCount].I, int_TmpStr);
      02 : Str(Stack[IdentCount - 1].R: Stack[IdentCount].I, int_TmpStr);
      03 : if Stack[IdentCount - 1].B then
             int_TmpStr := 'TRUE'
               else int_TmpStr := 'FALSE';
      04 : int_TmpStr := Copy(Chr(Stack[IdentCount - 1].I), 1, 1);
    end; { case }

    {-- Actually write the values ------------------------}
    if Assigned(elx_UserFuncsHook) then
      elx_UserWriteHook(int_TmpStr, false)
        else Write(int_TmpStr);

    {-- Dispose of the temp variables --------------------}
    Dec(IdentCount, 2);
  end; { with }
end; { proc. op_Write2 }


procedure int_op_CharAndString(var elx_Globals: elx_GlobalsType);
begin                                                          { C := S[1] }
                                                        { int_Param1: [n/a] }
                                               { int_Param2: type of concat }
  with elx_Globals do
    begin
      int_H1 := Stack[IdentCount].I;             { Get character to put in }

      {-- Make sure S[1] is used (not whole string) ------}
      if StringTable^[int_H1]^.Len <> 1 then
        begin
          int_ProgState := STRCHK;
          int_Continue := FALSE;
        end; { if }

      {-- Now set the actual char-value ------------------}
      {$IFDEF MSDOS}
        int_TmpStr := Copy(elx_Globals.StringTable^[int_H1]^.Content,
                     1, elx_Globals.StringTable^[int_H1]^.Len);

        if Length(int_TmpStr) < 1 then
          int_TmpStr := #0;

        Stack[Stack[IdentCount - 1].I].I := Ord(int_TmpStr[1]);
      {$ELSE}
        Stack[Stack[IdentCount - 1].I].I :=
           Ord(PChar(StringTable^[int_H1]^.Content)[0]);
      {$ENDIF}

      {-- Dispose of the temporary string if necessary ---}
      if (int_Param2 AND 8) = 8 then
        StrFreeString(elx_Globals, int_H1);

      {-- Remove all temporarily values ------------------}
      Dec(IdentCount, 2);
    end; { with }
end; { op_CharAndString }


procedure int_op_StringRel(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do                                  { int_Param1: [n/a] }
    begin                                              { int_Param2: [n/a] }
                                      { Get the index of the first string }
      int_H2 := Stack[IdentCount - 1].I;

                               { Get the index of the second string(/char) }
      int_H3 := Stack[IdentCount].I;

      {-- Do a good comparison --------------------------------------------}
      Case (int_Param2 AND 3) of
        1 : begin
                               {-- Get the length of the string -----------}
              int_H4 := StringTable^[int_H2]^.Len;

                               {-- Now do some tricky comparisons ---------}
                                 { If the stringlength is 0, the char wins }
              if int_H4 = 0 then int_H5 := 64
                else begin
                       {$IFDEF MSDOS}
                         int_TmpStr := Copy(elx_Globals.StringTable^[int_H2]^.Content,
                                        1, elx_Globals.StringTable^[int_H2]^.Len);
                       {$ELSE}
                         int_TmpStr := Copy(StrPas(PChar(elx_Globals.StringTable^[int_H2]^.Content)),
                                        1, elx_Globals.StringTable^[int_H2]^.Len);

{!!! writeln('oeps (11)'); }
                       {$ENDIF}
                       if Length(int_TmpStr) < 1 then
                         int_TmpStr := #0;

                                        {-- Now compare the content -------}
                       if int_H3 > Ord(int_TmpStr[1]) then int_H5 := 64
                         else if int_H3 < Ord(int_TmpStr[1]) then int_H5 := 32
                                        {-- else compare the length -------}
                          else if int_H4 = 1 then int_H5 := 16
                            else int_H5 := 32;
                     end; { else }
            end; { string and char }

        2 : begin
                               {-- Get the length of the string -----------}
              int_H4 := StringTable^[int_H3]^.Len;


                               {-- Now do some tricky comparisons ---------}
                                 { If the stringlength is 0, the char wins }
              if int_H4 = 0 then int_H5 := 32
                else begin
                       {$IFDEF MSDOS}
                         int_TmpStr := Copy(elx_Globals.StringTable^[int_H3]^.Content,
                                        1, elx_Globals.StringTable^[int_H3]^.Len);
                       {$ELSE}
                         int_TmpStr := Copy(StrPas(PChar(elx_Globals.StringTable^[int_H3]^.Content)),
                                        1, elx_Globals.Stringtable^[int_H3]^.Len);

{!!! writeln('oeps (11)'); }
                       {$ENDIF}
                       if Length(int_TmpStr) < 1 then
                         int_TmpStr := #0;

                       {-- Now compare the content -------}
                       if int_H2 > Ord(int_TmpStr[1]) then int_H5 := 32
                         else if int_H2 < Ord(int_TmpStr[1]) then int_H5 := 64
                          {-- else compare the length -------}
                          else if int_H4 = 1 then int_H5 := 16
                            else int_H5 := 64;
                     end; { else }
            end; { char and string }

        3 : begin
                                     { Get the length of the first string }
              int_H4 := StringTable^[int_H2]^.Len;

                                    { Get the length of the second string }
              int_H5 := StringTable^[int_H3]^.Len;
              int_H1 := 0;

                                             { Now make sure both strings }
                                       { are as large as the shortest one }
              if int_H5 < int_H4 then int_H4 := int_H5
                else int_H5 := int_H4;

                               {-- Loop through all characters for diffs --}
              while (int_H1 < int_H4) do
                begin
                  {$IFDEF MSDOS}
                    if StringTable^[int_H2]^.Content[int_H1 + 1] <>
                        StringTable^[int_H3]^.Content[int_H1 + 1] then
                  {$ELSE}
                    if PChar(StringTable^[int_H2]^.Content)[int_H1] <>
                        PChar(StringTable^[int_H3]^.Content)[int_H1] then
                  {$ENDIF}
                        begin
                                           { Abort immediatly }
                          int_H4 := int_H1;
                        end
                          else Inc(int_H1);
                end; { while }

              {-- First check if the loop was all thru ---}
              if int_H4 = int_H5 then
                begin
                                 {-- It was -------------------------------}
                if StringTable^[int_H2]^.Len = StringTable^[int_H3]^.Len then
                  begin
                    int_H5 := 16
                  end
                    else if StringTable^[int_H2]^.Len < StringTable^[int_H3]^.Len then
                           begin
                             int_H5 := 64
                           end
                             else int_H5 := 32
                  end
                    else begin
                           {$IFDEF MSDOS}
                           if StringTable^[int_H2]^.Content[int_H1 + 1] <
                               StringTable^[int_H3]^.Content[int_H1 + 1] then
                           {$ELSE}
                           if PChar(StringTable^[int_H2]^.Content)[int_H1] <
                               PChar(StringTable^[int_H3]^.Content)[int_H1] then
                           {$ENDIF}
                                 int_H5 := 64
                                   else int_H5 := 32;
                         end; { else }
            end; { string ~ string }
      end; { case }

      {-- Check the parameters --------------------------------------------}
      if (int_Param2 AND 5) = 5 then
        StrFreeString(elx_Globals, int_h2);

      if (int_Param2 AND 10) = 10 then
        StrFreeString(elx_Globals, int_h3);

      {-- Dispose any pending vars -----------------------}
      Dec(IdentCount);

      {-- Now return the boolean value -------------------}
      Stack[IdentCount].B := (int_Param2 AND int_H5) > 0;
    end; { with }
end; { proc. op_StringRel }


procedure int_op_RunUserFunc(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do                   { int_Param1: Func. number to run }
    begin                { int_Param2: ptr to exact vartypes used in call }

                                               { Load the function number }
      int_Param1 := ProgCode[int_PrevPC].X;

     {$IFDEF ELX_PROFILE}
       inc(UserFuncCount[int_Param1].Count);
       RunUserTmp := elx_profgetmsec;
     {$ENDIF}

      Do_RunUserFunc(elx_Globals, int_Param1, int_Param2,
                     int_CallObjPtr);     { Execute }

      {-- Make sure the password parameters are skipped ----}
      {-- this is done at Do_RunUserFunc -------------------}

      {$IFDEF ELX_PROFILE}
         UserFuncCount[int_Param1].msecs := UserFuncCount[int_Param1].msecs + (elx_profgetmsec - RunUserTmp);
      {$ENDIF}
    end; { with }
end; { op_RunUserFunc }

procedure int_op_StringLink(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do
    begin
      if Stack[Stack[IdentCount].I].I = 0 then
        begin
           StrLinkup(elx_Globals, Stack[IdentCount].I);

           {-- Try to allocate more memory for the new string ---}
           if NOT StrAllocNew(elx_Globals, Stack[Stack[IdentCount].i].S, 0) then
            begin
              int_ProgState := StkChk;
              int_Continue := FALSE;
            end; { if }
        end; { if }
    end; { with }
end; { op_StringLink }


procedure int_op_DynArIndex(var elx_Globals: elx_GlobalsType);
var Counter: Longint;
begin
{$IFNDEF MSDOS}
  with elx_Globals do
    begin
                                    { Load the index to the dynarratable }
      int_Param1 := ProgCode[int_PrevPC].Y;
                                          { load the index to this array }
      int_Param2 := Stack[IdentCount].I;

      {-- Now lets execute ----------------------------------------------}
      Dec(Identcount);

(*
WriteLn('DynArrayTable[', int_Param1, '].LastElement= ',
  DynArrayTable^[int_Param1].LastElementLd);
*)

      {-- first update the previous element set -------------------------}
      if DynArrayTable^[int_Param1].LastElementLd >= 0 then
        begin
          {-- first make sure it would fit ------------------------------}
          with DynArrayTable^[int_Param1] do
           if LastElementLd >= DynArrayTable^[int_Param1].alloc_High then
            begin
              {$IFNDEF MSDOS}
              DynArrayPutElement(elx_Globals,
                                 int_Param1,              { array number }
                                 LastElementLd,      { element to update }
                                 Stack[identcount].i); { stack start pos }
              {$ENDIF}
            end; { if }

          {-- else use an itnernal routine to update it -----------------}
          with DynArrayTable^[int_Param1] do
            begin
              Move(Stack[Stack[IdentCount].I],            { actual stack }
                   ArrayPtrRec^[LastElementLd *
                                 ElementSize],           { array pointer }
                   (ElementSize * SizeOf(Stack[1])));
            end; { if }

          {-- and update the highest item, if necessary -----------------}
          if DynArrayTable^[int_Param1].HighestItem < DynArrayTable^[int_Param1].LastElementLd then
            DynArrayTable^[int_Param1].HighestItem := DynArrayTable^[int_Param1].LastElementLd;
          if DynArrayTable^[int_Param1].LowestItem > DynArrayTable^[int_Param1].LastElementLd then
            DynArrayTable^[int_Param1].LowestItem := DynArrayTable^[int_Param1].LastElementLd;
        end; { if }

      {-- Moved this to below, else 2 consecutive updates would not work-}
      {-- as the highestitem would not be updated, so not selecting an --}
      {-- unique index number for the assoc array -----------------------}
      {-- if user passed a string as an index, fix it -------------------}
      if ProgCode[int_PrevPC].X = 1 then
        begin
          with DynArrayTable^[int_Param1].ArrayNames do
            Counter := Get(StrPas(PChar(StringTable^[int_Param2]^.Content)));

          if Counter = -1 then
            begin
              {-- undefined element, so add it --------------------------}
              with DynArrayTable^[int_Param1] do
                begin
                  ArrayNames.Put(StrPas(PChar(StringTable^[int_Param2]^.Content)),
                                 HighestItem + 2);

                  int_Param2 := HighestItem + 2;
                end; { with }
            end
              else begin
                     int_Param2 := Counter;
                   end; { else }
        end; { if }

(*
WriteLn('DynArrayTable[', int_Param1, '] --> Element = ',
  int_Param2);
*)

      {-- first make sure it would fit ------------------------------}
      with DynArrayTable^[int_Param1] do
        if Stack[IdentCount].i >= DynArrayTable^[int_Param1].alloc_High then
          begin
            {$IFNDEF MSDOS}
            DynArrayPutElement(elx_Globals,
                               int_Param1,              { array number }
                               LastElementLd,      { element to update }
                               Stack[IdentCount].I); { stack start pos }
                               
            {$ENDIF}
          end; { if }

(*
WriteLn('DynArrayTable[', int_Param1, '] --> Array extended if nec');
WriteLn('Array Max Size: ', DynArrayTable^[int_Param1].alloc_high);
*)

{writeln('Array#', int_Param1, ' [', int_Param2, '] ...');}
{if DynArrayTable^[int_Param1].ElementType = typ_DynArray then}
{  writeln('a nested dynamic array, parent = ');}


      {-- get the unique value ------------------------------------------}
      with DynArrayTable^[int_Param1] do
        begin
          Move(ArrayPtrRec^[int_Param2 * ElementSize],   { array pointer }
               Stack[Stack[IdentCount].I],                { actual stack }
               (ElementSize * SizeOf(Stack[1])));

(*
writeln('OK: Retrieved from(',int_param1,'): ', int_Param2, ' * ', ElementSize, ' (=',int_param2*elementsize);
*)
        end; { if }

      {-- mark this stack item as belonging to an dynamic array ---------}
      with DynArrayTable^[int_Param1] do
        begin
          for Counter := 0 to (ElementSize - 1) do
            Stack[Stack[IdentCount].I + Counter].DynArraySet := int_Param1;
        end; { with }


      {-- and update the pointer of last element we have updated --------}
      DynArrayTable^[int_Param1].LastElementLd := int_Param2;
    end; { with }
{$ENDIF}
end; { op_DynArIndex }



procedure int_op_DynArCommand(var elx_Globals: elx_GlobalsType);
var TmpParam: Integer;
begin
  with elx_Globals do
    begin
                                    { Load the index to the dynarraytable }
      int_Param1 := ProgCode[int_PrevPC].X;
                                          { load the type of info we want }
      int_Param2 := ProgCode[int_PrevPC].Y;

      {-- create room for a temp variable --------------------------------}
      Inc(IdentCount);

      {-- select which info we want --------------------------------------}
{$IFNDEF MSDOS}
      with elx_Globals do
        case int_Param2 of
          0 : Stack[IdentCount].i := DynArrayTable^[int_Param1].HighestItem;
          1 : Stack[IdentCount].i := DynArrayTable^[int_Param2].LowestItem;
        end; { case }
{$ENDIF}
    end; { with }
end; { proc. int_op_DynArCommand }


procedure int_op_DynArChild(var elx_Globals: elx_GlobalsType);
begin
  writeln('og my !!!');
end; { proc. int_op_DynArChild }

procedure int_op_Halt(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do                                  { int_Param1: [n/a] }
    begin                                              { int_Param2: [n/a] }
      int_ProgState := FIN;
      int_Continue := FALSE;
    end; { with }
end; { proc. op_halt }


procedure int_op_ExitProc(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do                                  { int_Param1: [n/a] }
    begin                                              { int_Param2: [n/a] }
              { has been made a seperate procedure so that RunUserFunc can }
                                           { also call this same procedure }
                                                 { Get pointer to the last }
      int_H1 := Stack[int_BaseIndex + 5].I;  { registered str item }

(*
writeln('[', ick:2,'] op_ExitProc ', int_BaseIndex + 6, ' (Stack[',int_h1,'])');
writeln('op_exitproc:');
writeln('int_baseIndex = ', int_BaseIndex);
WriteLn('[',ick:2,'] int_BaseZero - get - ', int_BaseZero, ' [free count=',
StrGetFreeCount(elx_Globals), ' (..: ', int_baseindex + 6);
*)

      {-- Now delete all string references -------------------------------}
      while int_H1 <> 0 do
        begin
          StrFreeString(elx_Globals, Stack[int_H1].I);{ Dispose of the string }
          int_H1 := Stack[int_H1].P; { Get next in line to dispose }
        end; { while }

      {-- and now delete all string arrays -------------------------------}
      int_H1 := Stack[int_BaseIndex + 6].I;    { registered dynarray item }

      {-- Now delete all stringarray references --------------------------}
      while int_H1 <> 0 do
        begin
          {-- add the last string to the set to be updated --------------}
{$IFNDEF MSDOS}
          if DynArrayTable^[Stack[int_H1].DynArraySet].LastElementLd >= 0 then
            with DynArrayTable^[Stack[int_H1].DynArraySet] do
              begin
                 {!!!!!!!!!!!!!FIXME!!!!!!!!!!!!!!}
              end; { with }
{$ENDIF}

          {-- and actually free the strings and close the array ----------}
          {$IFNDEF MSDOS}
           DynArrayFreeIt(elx_Globals, Stack[int_H1].DynArraySet, false);
          {$ENDIF}
          int_H1 := Stack[int_H1].P; { Get next in line to dispose }
        end; { while }

      {-- Set stack marker to before this procedure -------}
      IdentCount := int_BaseIndex - 1;

      {-- Set the program pointer to return address -------}
      int_ProgPtr := Stack[int_BaseIndex + 1].I;

      {-- Set the base index for THIS procedure -----------}
      int_BaseIndex := Stack[int_BaseIndex + 3].I;

      {-- decrease the basezeroidx value, see -------------}
      {-- op_markstack for an explanation of what this does}
      Dec(BaseZeroIdx);
      int_BaseZero := BaseZeroArray^[BaseZeroIdx];
    end; { with }
end; { proc. op_ExitProc }


procedure int_op_ExitFunc(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do                                  { int_Param1: [n/a] }
    begin                                              { int_Param2: [n/a] }

                                                 { Get pointer to the last }
                                                  { registered string item }

      int_H1 := Stack[int_BaseIndex + 5].I;

(*
writeln('[', ick:2,'] op_ExitFunc ', int_BaseIndex + 6, ' (Stack[',int_h1,'])');
writeln('(01) - Stack[', int_BaseZero -1,'].i = ', Stack[int_BaseZero -1].i);
writeln('(03) - Stack[', int_BaseIndex +5,'].i = ', Stack[int_baseIndex + 5].i);
WriteLn('[',ick:2,'] int_BaseZero - get - ', int_BaseZero, ' [free count=',
StrGetFreeCount(elx_Globals), ' (..: ', int_baseindex + 6);
*)
{writeln('Adding ExitFunc      = ', int_BaseIndex);}

      {-- Now delete all string references -------------------------------}
      while int_H1 <> 0 do
        begin
  (*
  writeln('     Freeing: ', stack[int_h1].i, ' (where...: ', int_BaseIndex,')');
  *)

          if int_H1 <> int_BaseIndex then
            StrFreeString(elx_Globals, Stack[int_H1].I); { Dispose of the string }
          int_H1 := Stack[int_H1].P; { Get next in line to dispose }
        end; { while }

      {-- and now delete all string arrays -------------------------------}
      int_H1 := Stack[int_BaseIndex + 6].I;    { registered dytnrray item }

      {-- Now delete all stringarray references --------------------------}
      while int_H1 <> 0 do
        begin
          {-- add the last string to the set to be updated --------------}
{$IFNDEF MSDOS}
          if DynArrayTable^[Stack[int_H1].DynArraySet].LastElementLd >= 0 then
            with DynArrayTable^[Stack[int_H1].DynArraySet] do
              begin
                 {!!!!!!!!!!!!!FIXME!!!!!!!!!!!!!!}
              end; { with }
{$ENDIF}

          {-- and actually free the strings and close the array ----------}
          {$IFNDEF MSDOS}
           DynArrayFreeIt(elx_Globals, Stack[int_H1].DynArraySet, false);
          {$ENDIF}
          int_H1 := Stack[int_H1].P; { Get next in line to dispose }
        end; { while }

      {-- Set stack marker to before this function --------}
      IdentCount := int_BaseIndex;

      {-- Set the program pointer to return address -------}
      int_ProgPtr := Stack[int_BaseIndex + 1].I;

      {-- Set the base index for THIS procedure -----------}
      int_BaseIndex := Stack[int_BaseIndex + 3].I;

      {-- decrease the basezeroidx value, see -------------}
      {-- op_markstack for an explanation of what this does}
      Dec(BaseZeroIdx);
      int_BaseZero := BaseZeroArray^[BaseZeroIdx];
    end; { with }
end; { proc. op_ExitFunc }


{-- op_CopyArray -------------------------------------------------------}
procedure int_op_CopyArray(var elx_Globals: elx_GlobalsType);
begin                                                     { int_Param1: [n/a] }
  with elx_Globals do                                     { int_Param2: [n/a] }
    Stack[IdentCount] := Stack[Stack[IdentCount].I];
end; { op_CopyArray }


procedure int_op_MakeBoolNot(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do                                     { int_Param1: [n/a] }
    Stack[IdentCount].B := NOT Stack[IdentCount].B;       { int_Param2: [n/a] }
end; { op_MakeBoolNot }


procedure int_op_MakeIntNegative(var elx_Globals: elx_GlobalsType);
begin                                           { Inverse an integer value }
                                                       { int_Param1: [n/a] }
  with elx_Globals do                                  { int_Param2: [n/a] }
    Stack[IdentCount].I := -Stack[IdentCount].I;
end; { op_MakeIntNegative }


procedure int_op_DecimalReal(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do                                  { int_Param1: [n/a] }
    begin                                              { int_Param2: [n/a] }
      {-- First convert it to a string ---------------------}
      Str(Stack[IdentCount - 2].R :
          Stack[IdentCount - 1].I : { width }
          Stack[IdentCount].I,      { decimals }
          int_TmpStr);

      {-- Now write the actual string ----------------------}
      if Assigned(elx_UserFuncsHook) then
        elx_UserWriteHook(int_TmpStr, false)
          else Write(int_TmpStr);

      {-- Dispose of the used stack ------------------------}
      Dec(IdentCount, 3);
    end; { with }
end; { op_DecimalReal }


procedure int_op_Store(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do                  { the variable as referenced before }
    begin                            { the variable that is assigned to is }
                                              { loaded by "op_LoadAddress" }
                                                       { int_Param1: [n/a] }
                                                       { int_Param2: [n/a] }
      Stack[Stack[IdentCount - 1].i] := Stack[IdentCount];
      Dec(IdentCount, 2);
    end; { with }
end; { proc. op_Store }


procedure int_op_RealEql(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do                                   { int_Param1: [n/a] }
    begin                                               { int_Param2: [n/a] }
      Dec(IdentCount);
      Stack[IdentCount].B := (Stack[IdentCount].R = Stack[IdentCount + 1].R);
    end; { with }
end; { op_RealEql }

procedure int_op_RealNeq(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do                                   { int_Param1: [n/a] }
    begin                                               { int_Param2: [n/a] }
      Dec(IdentCount);
      Stack[IdentCount].B := (Stack[IdentCount].R <> Stack[IdentCount + 1].R);
    end; { with }
end; { op_RealNeq }

procedure int_op_RealLss(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do                                   { int_Param1: [n/a] }
    begin                                               { int_Param2: [n/a] }
      Dec(IdentCount);
      Stack[IdentCount].B := (Stack[IdentCount].R < Stack[IdentCount + 1].R);
    end; { with }
end; { op_RealLss }

procedure int_op_RealLeq(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do                                   { int_Param1: [n/a] }
    begin                                               { int_Param2: [n/a] }
      Dec(IdentCount);
      Stack[IdentCount].B := (Stack[IdentCount].R <= Stack[IdentCount + 1].R);
    end; { with }
end; { op_RealLeq }


procedure int_op_RealGtr(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do                                   { int_Param1: [n/a] }
    begin                                               { int_Param2: [n/a] }
      Dec(IdentCount);
      Stack[IdentCount].B := (Stack[IdentCount].R > Stack[IdentCount + 1].R);
    end; { with }
end; { op_RealGtr }


procedure int_op_RealGeq(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do                                   { int_Param1: [n/a] }
    begin                                               { int_Param2: [n/a] }
      Dec(IdentCount);
      Stack[IdentCount].B := (Stack[IdentCount].R >= Stack[IdentCount + 1].R);
    end; { with }
end; { op_RealGeq }

procedure int_op_IntEql(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do                                   { int_Param1: [n/a] }
    begin                                               { int_Param2: [n/a] }
      Dec(IdentCount);
      Stack[IdentCount].B := (Stack[IdentCount].I = Stack[IdentCount + 1].I);
    end; { with }
end; { op_IntEql }

procedure int_op_IntNeq(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do                                   { int_Param1: [n/a] }
    begin                                               { int_Param2: [n/a] }
      Dec(IdentCount);
      Stack[IdentCount].B := (Stack[IdentCount].I <> Stack[IdentCount + 1].I);
    end; { with }
end; { proc. op_IntNeq }


procedure int_op_IntLss(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do                                   { int_Param1: [n/a] }
    begin                                               { int_Param2: [n/a] }
      Dec(IdentCount);
      Stack[IdentCount].B := (Stack[IdentCount].I < Stack[IdentCount + 1].I);
    end; { with }
end; { op_IntLss }


procedure int_op_IntLeq(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do                                   { int_Param1: [n/a] }
    begin                                               { int_Param2: [n/a] }
      Dec(IdentCount);
      Stack[IdentCount].B := (Stack[IdentCount].I <= Stack[IdentCount + 1].I);
    end; { with }
end; { op_IntLeq }

procedure int_op_IntGtr(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do                                   { int_Param1: [n/a] }
    begin                                               { int_Param2: [n/a] }
      Dec(IdentCount);
      Stack[IdentCount].B := (Stack[IdentCount].I > Stack[IdentCount + 1].I);
    end; { with }
end; { op_IntGtr }

procedure int_op_IntGeq(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do                                   { int_Param1: [n/a] }
    begin                                               { int_Param2: [n/a] }
      Dec(IdentCount);
      Stack[IdentCount].B := (Stack[IdentCount].I >= Stack[IdentCount + 1].I);
    end; { with }
end; { op_IntGeq }

procedure int_op_BoolOr(var elx_Globals: elx_GlobalsType);
begin                                                     { int_Param1: [n/a] }
  with elx_Globals do                                     { int_Param2: [n/a] }
    begin
      Dec(IdentCount);
      Stack[IdentCount].B := (Stack[IdentCount].B OR Stack[IdentCount + 1].B);
    end; { with }
end; { op_IntGeq }

procedure int_op_MinInt(var elx_Globals: elx_GlobalsType);
begin                                                     { int_Param1: [n/a] }
  with elx_Globals do                                     { int_Param2: [n/a] }
    begin
      Dec(IdentCount);
      Dec(Stack[IdentCount].i, Stack[IdentCount + 1].i);
    end; { with }
end; { op_MinInt }

procedure int_op_AddInt(var elx_Globals: elx_GlobalsType);
begin                                                     { int_Param1: [n/a] }
  with elx_Globals do                                     { int_Param2: [n/a] }
    begin
      Dec(IdentCount);
      Inc(Stack[IdentCount].i, Stack[IdentCount + 1].i);
    end; { with }
end; { op_AddInt }


procedure int_op_MinReal(var elx_Globals: elx_GlobalsType);
begin                                                     { int_Param1: [n/a] }
  with elx_Globals do                                     { int_Param2: [n/a] }
    begin
      Dec(IdentCount);
      Stack[IdentCount].R := Stack[IdentCount].R - Stack[IdentCount + 1].R;
    end; { with }
end; { op_MinReal }

procedure int_op_AddReal(var elx_Globals: elx_GlobalsType);
begin                                                     { int_Param1: [n/a] }
  with elx_Globals do                                     { int_Param2: [n/a] }
    begin
      Dec(IdentCount);
      Stack[IdentCount].R := Stack[IdentCount].R + Stack[IdentCount + 1].R;
    end; { with }
end; { op_AddReal }

procedure int_op_MakeBoolAnd(var elx_Globals: elx_GlobalsType);
begin                                                     { int_Param1: [n/a] }
  with elx_Globals do                                     { int_Param2: [n/a] }
    begin
      Dec(IdentCount);
      Stack[IdentCount].B := Stack[IdentCount].B AND Stack[IdentCount + 1].B;
    end; { with }
end; { op_MakeBoolAnd }


procedure int_op_MultiplyInt(var elx_Globals: elx_GlobalsType);
begin                                                     { int_Param1: [n/a] }
  with elx_Globals do                                     { int_Param2: [n/a] }
    begin
      Dec(IdentCount);
      Stack[IdentCount].i := Stack[IdentCount].I * Stack[IdentCount + 1].I;
    end; { with }
end; { op_MultiplyInt }

procedure int_op_DivideInt(var elx_Globals: elx_GlobalsType);
begin                                                     { int_Param1: [n/a] }
  with elx_Globals do                                     { int_Param2: [n/a] }
    begin
      Dec(IdentCount);

      {-- Catch a divide by zero error ---------------------}
      if Stack[IdentCount + 1].I = 0 then
        begin
          int_ProgState := DivChk;
          int_Continue := FALSE;
        end; { if }

      {-- Perform the divide -------------------------------}
      Stack[IdentCount].I := Stack[IdentCount].I DIV Stack[IdentCount + 1].I;
    end; { with }
end; { op_DivideInt }


procedure int_op_ModInt(var elx_Globals: elx_GlobalsType);
begin                                                     { int_Param1: [n/a] }
  with elx_Globals do                                     { int_Param2: [n/a] }
    begin
      Dec(IdentCount);

      {-- Catch a divide by zero error ---------------------}
      if Stack[IdentCount + 1].I = 0 then
        begin
          int_ProgState := DivChk;
          int_Continue := FALSE;
        end; { if }

      {-- Perform the divide -------------------------------}
      Stack[IdentCount].I := Stack[IdentCount].I MOD Stack[IdentCount + 1].I;
    end; { with }
end; { op_ModInt }

procedure int_op_MultiplyReal(var elx_Globals: elx_GlobalsType);
begin                                                     { int_Param1: [n/a] }
  with elx_Globals do                                     { int_Param2: [n/a] }
    begin
      Dec(IdentCount);

      Stack[IdentCount].R := Stack[IdentCount].R * Stack[IdentCount + 1].R;
    end; { with }
end; { proc. op_MultiplyReal }

procedure int_op_DivideReal(var elx_Globals: elx_GlobalsType);
begin                                                     { int_Param1: [n/a] }
  with elx_Globals do                                     { int_Param2: [n/a] }
    begin
      Dec(IdentCount);

      Stack[IdentCount].R := Stack[IdentCount].R / Stack[IdentCount + 1].R;
    end; { with }
end; { op_DivideReal }

procedure int_op_ReadLn(var elx_Globals: elx_GlobalsType);
begin                                                     { int_Param1: [n/a] }
  with elx_Globals do                                     { int_Param2: [n/a] }
    begin
      if EOF(Input) then
        begin
          int_ProgState := REDCHK;
          int_Continue := FALSE;
        end; { if }

      {-- Actually read the values -------------------------}
      if Assigned(elx_UserReadHook) then
        elx_UserReadHook(int_TmpStr, typ_noTyp, true)
          else ReadLn;
    end; { with }
end; { op_ReadLn }


procedure int_op_LineFeed(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do
    begin
      if Assigned(elx_UserFuncsHook) then
        elx_UserWriteHook('', true)
          else WriteLn;
    end; { with }
end; { op_LineFeed }

procedure int_op_MakeRealNegative(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do
    begin
      Stack[IdentCount].R := -Stack[IdentCount].R;
    end; { with }
end; { proc. op_MakeRealNegative }


procedure int_op_StrIndex(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do
    begin
                             { H1 contains the actual string index to use }
      int_H1 := Stack[IdentCount - 1].I;

                               { H2 contains the element-# of this string }
      int_H2 := Stack[IdentCount].I;

      {-- Range check this one -----------------------------}
(* excluded... relax this a bit
      if (int_H2 <= 0) OR (int_H2 > StringTable^[int_H1]^.Len) then
        begin
          int_ProgState := INXCHK;
          int_Continue := FALSE;
        end; { if }
*)

      {-- Dispose of unused vars ---------------------------}
      Dec(IdentCount);

      {-- Return the actual value --------------------------}
      if (int_H2 > 0) AND (int_H2 <= StringTable^[int_H1]^.Len) then
      {$IFDEF MSDOS}
        Stack[IdentCount].I := Ord(elx_Globals.StringTable^[int_H1]^.Content[int_H2]);
      {$ELSE}
        Stack[IdentCount].I := Ord(PChar(elx_Globals.StringTable^[int_H1]^.Content)[int_H2 - 1]);
      {$ENDIF}
    end; { with }
end; { op_StrIndex }


procedure int_op_StringTemp(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do                                 { int_Param1: [n/a] }
    begin                                             { int_Param2: [n/a] }
                               { H2 is the destination of this action ("s") }
      int_H2 := Stack[IdentCount - 1].I;

                                  { H1 is the actual value (string-index) }
      int_H1 := Stack[int_H2].I;

      {-- If its not used yet, add it to the list ----------}
      if int_H1 = 0 then
        begin
          StrLinkup(elx_Globals, int_H2);
        end
          else begin
                 {-- else dispose of the old value ---------}
                 {-- scheck for nilstrings, we cant free them}
                 if int_H1 <> 0 then
                   StrFreeString(elx_Globals, int_H1);
               end; { else }

    {-- Set the destination result to this value ---------}
    Stack[int_H2].I := Stack[IdentCount].I;

    {-- Dispose of unused vars ---------------------------}
    Dec(IdentCount, 2);
  end; { with }
end; { op_StringTemp }


procedure int_op_ArToString(var elx_Globals: elx_GlobalsType);
var TmpBuf   : Array[0..1025] of Char;
begin
  with elx_Globals do                                 { int_Param1: [n/a] }
    begin                                     { int_Param2: size of array }
                                   { H1 now points to the original string }
      int_H1 := Stack[IdentCount].I;

      {-- Try to allocate more memory for the new string ---}
      if NOT StrAllocNew(elx_Globals, int_H3, int_Param2) then
        begin
          int_ProgState := StkChk;
          int_Continue := FALSE;
        end; { if }

      {-- Now copy the content of the array to the string --}
      FillChar(TmpBuf, SizeOf(TmpBuf), #0);
      if int_Param2 > 1024 then
        int_Param2 := 1024;

      int_H4 := 0;
      while (int_H4 <= (int_Param2 - 1)) AND (Stack[int_H1 + int_H4].C <> #0) do
        begin
          TmpBuf[int_H4] := Stack[int_H1 + int_H4].C;
          Inc(int_H4);
        end; { while }
      TmpBuf[int_H4 + 1] := #0;

      {-- Now set the actual contents of the new string ----}
      StringTable^[int_H3]^.Len := int_H4;
      {$IFDEF MSDOS}
        StringTable^[int_H3]^.Content := StrPas(TmpBuf);
      {$ELSE}
        StrCopy(PChar(StringTable^[int_H3]^.Content), TmpBuf);
      {$ENDIF}
      Stack[IdentCount].I := int_H3;
    end; { with }
end; { op_ArToString }


procedure int_op_StringandChar(var elx_Globals: elx_GlobalsType);
begin                                { Assign a char to a string (S := c;) }
  with elx_Globals do                                  { int_Param1: [n/a] }
    begin                          { int_Param2: string index to assign to }
                                           { H2 is the destination string }
      int_H2 := Stack[Stack[IdentCount - 1].I].I;

      {-- Link it up with the rest -------------------------}
      if int_H2 = 0 then
        begin
          int_TmpStr := '';
          StrLinkUp(elx_Globals, Stack[IdentCount - 1].I)
        end
          else begin
                 {$IFDEF MSDOS}
                   int_TmpStr := Copy(StringTable^[int_H2]^.Content,
                                            1, StringTable^[int_H2]^.Len);
                 {$ELSE}
                   int_TmpStr := Copy(StrPas(PChar(StringTable^[int_H2]^.Content)),
                                                 1, StringTable^[int_H2]^.Len);

                 {$ENDIF}
                 StrFreeString(elx_Globals, int_h2);
               end; { else }

      {-- if stringindex= 0 we create a new string ---------}
      if int_Param2 < 0 then
        int_TmpStr := Copy(Chr(Stack[IdentCount].I), 1, 1)
          else begin
                 if Length(int_TmpStr) < int_Param2 then
                   begin
                      int_ProgState := StrChk;
                      int_Continue := false;
                   end; { if }

                 int_TmpStr[int_Param2] := Chr(Stack[IdentCount].I);
               end; { else }

      {-- Get the new string -------------------------------}
      if NOT StrAllocNew(elx_Globals, int_H3, Length(int_TmpStr)) then
        begin
          int_ProgState := STKCHK;
          int_Continue := false;
        end; { if }

      {-- Update the current stack to the new string -------}
      Stack[Stack[IdentCount - 1].I].I := int_H3;

      {-- Now set the first character to this string -------}
      {$IFDEF MSDOS}
        StringTable^[int_H3]^.Content := int_TmpStr;
        StringTable^[int_H3]^.Len := Length(int_TmpStr);
      {$ELSE}
        StrPCopy(PChar(StringTable^[int_H3]^.Content), int_TmpStr);
        StringTable^[int_H3]^.Len := Length(int_TmpStr);
      {$ENDIF}

      {-- Dispose of unused vars ---------------------------}
      Dec(IdentCount, 2);
    end; { with }
end; { proc. op_StringAndChar }


procedure int_op_StrAndStr(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do                                 { int_Param1: [n/a] }
    begin                                             { int_Param2: [n/a] }
                                                           { Just copy it }
      if NOT StrDupl(elx_Globals, Stack[IdentCount - 1].i, IdentCount) then
        begin
          int_Continue := false;
        end; { if }

      {-- Dispose of unused vars ---------------------------}
      Dec(IdentCount, 2);
    end; { with }
end; { op_StrAndStr }


procedure int_op_WriteStrTmp(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do                                 { int_Param1: [n/a] }
    begin                                             { int_Param2: [n/a] }
                                     { Get the index to the actual string }
      int_H3 := Stack[IdentCount].I;
                                           { Get the actual string length }

      {-- Now actually write the string --------------------}
      if StringTable^[int_h3] <> nil then
        begin
          {$IFDEF MSDOS}
            int_TmpStr := StringTable^[int_H3]^.Content;
          {$ELSE}
            int_TmpStr := StrPas(PChar(StringTable^[int_H3]^.Content));
          {$ENDIF}
        end; { if }

      if Assigned(elx_UserFuncsHook) then
        elx_UserWriteHook(int_TmpStr, false)
          else Write(int_TmpStr);

      {-- Dispose the string if necessary ------------------}
      if (int_OpCode = op_WriteStrTmp) then
        StrFreeString(elx_Globals, int_h3);

      {-- Dispose of unused vars ---------------------------}
      Dec(IdentCount);
    end; { with }
end; { op_WriteStr, op_WriteStrTmp }

procedure int_op_StrValueParam(var elx_Globals: elx_GlobalsType);
begin                  { Pass a string as a value parameter to a func/proc }
  with elx_Globals do                                  { int_Param1: [n/a] }
    begin                                              { int_Param2: [n/a] }
                                     { H1 contains the actual string-index }
      int_H1 := Stack[IdentCount].I;
                                                    { Get the string length }
      int_H4 := StringTable^[int_H1]^.Len;

      {-- Now create a new copy of this ---------------------}
      if NOT StrAllocNew(elx_Globals, int_H2, int_H4) then
        begin
          int_ProgState := StkChk;
          int_Continue := false;
        end; { if }

      {-- Copy the content ----------------------------------}
      {$IFDEF MSDOS}
        StringTable^[int_H2]^.Content := Copy(StringTable^[int_H1]^.Content,
                                        1, StringTable^[int_H1]^.Len);
      {$ELSE}
        StrPCopy(PChar(StringTable^[int_H2]^.Content), Copy(StrPas(PChar(StringTable^[int_H1]^.Content)),
                                          1, StringTable^[int_H1]^.Len));
      {$ENDIF}
      StringTable^[int_H2]^.Len := StringTable^[int_H1]^.Len;

      {-- Set the rest of the parameters --------------------}
      Stack[IdentCount].I := int_H2;

      Stack[IdentCount].p := Stack[int_BaseZero - 1].I;
      { Stack[Stack[IdentCount].P].P := 0; rolled back, causes string resource }
      { leakage. like .. major leakage. String fix for large reals }
      Stack[int_BaseZero - 1].I := IdentCount;


      {-- Update the stack ----------------------------------------------}
      Stack[IdentCount].I := int_H2;
{writeln('Adding linkup (2)    = ', int_BaseZero - 6, ' ---> ', IdentCount);}
(*writeln('op_StrValParam - Stack[', int_BaseZero -1,'].i = ', Stack[int_BaseZero -1].i);*)

    end; { with }
end; { proc. op_StrValueParam }


procedure int_op_StrValParamTmp(var elx_Globals: elx_GlobalsType);
begin      { Call a func/proc with string as param. eg: func('Aa' + 'bb') }
  with elx_Globals do                                 { int_Param1: [n/a] }
    begin                                             { int_Param2: [n/a] }
                                                { Link it up with another }

      Stack[IdentCount].P := Stack[int_BaseZero - 1].I;
      { Stack[Stack[IdentCount].P].P := 0;  // String fix for large reals }
      { rolled back as it causes string resource leakage. like .. major }
      { leakage }


                                             { Add this as another string }
      Stack[int_BaseZero - 1].I := IdentCount;

{writeln('Adding linkup (1)    = ', int_BaseZero - 6, ' ---> ', IdentCount);}
(*writeln('op_StrValParam - Stack[', int_BaseZero -1,'].i = ', Stack[int_BaseZero -1].i);*)
    end; { with }
end; { proc. op_StrValParamTmp }


procedure int_op_CharArAndStr(var elx_Globals: elx_GlobalsType); { Assigns a string to a char array  }
var TmpCount: Longint;
    TempC: Char;
begin                        { Assigns a temporaril string to a char array }
  with elx_Globals do                                  { int_Param1: [n/a] }
    begin                                 { int_Param2: Size of char array }

                                    { Get the string we want to set it to }
      int_H1 := Stack[IdentCount].I;
                                          { Get the length of that string }
      int_H2 := StringTable^[int_H1]^.Len;

                             { H4 now contains the index to the char aray }
      int_H4 := Stack[IdentCount - 1].I;

      {-- Now actually copy the string ---------------------}
      if int_H2 >= int_Param2 then
        begin
          {-- The string is longer than the array ----------}
          for TmpCount := 0 to (int_Param2 - 1) do
            begin
              if TmpCount > int_H2 then TempC := #0
              {$IFDEF MSDOS}
                else TempC := StringTable^[int_H1]^.Content[TmpCount + 1];
              {$ELSE}
                else TempC := PChar(StringTable^[int_H1]^.Content)[TmpCount];
              {$ENDIF}

              Stack[int_H4 + TmpCount].C := TempC;
            end; { for }
        end
          else begin
                 {-- otherwise -----------------------------}
                 for TmpCount := 0 to (int_H2 - 1) do
                   begin
                     if TmpCount > int_H2 then TempC := #0
                      {$IFDEF MSDOS}
                         else TempC := StringTable^[int_H1]^.Content[TmpCount + 1];
                      {$ELSE}
                         else TempC := PChar(StringTable^[int_H1]^.Content)[TmpCount];
                      {$ENDIF}

                     Stack[int_H4 + TmpCount].C := TempC;
                   end; { for }

                 for TmpCount := (int_H4 + int_H2) to (int_H4 + (int_Param2 - 1)) do
                   Stack[TmpCount].C := #0;
               end; { else }

      {-- If its a temp. string, dispose it ----------------}
      if (int_OpCode = op_CharArAndStrTmp) then
        StrFreeString(elx_Globals, int_h1);

      {-- Dispose of unused vars ---------------------------}
      Dec(IdentCount, 2);
    end; { with }
end; { op_CharArStr, op_CharArStrTmp }


procedure int_op_WriteStr2Tmp(var elx_Globals: elx_GlobalsType);
begin                     { Writes a temp. string to the screen with a len }
  with elx_Globals do                                  { int_Param1: [n/a] }
    begin                                              { int_Param2: [n/a] }
                                 { H4 contains the len we expect from this }
      int_H4 := Stack[IdentCount].I;
                                                 { H3 is the actual string }
      int_H3 := Stack[IdentCount - 1].I;
                                 { Now get the length of the actual string }
      int_H2 := StringTable^[int_H3]^.Len;

      {-- Write padding spaces if necessary ----------------}
      if int_H2 >= int_H4 then
        begin
          int_H2 := int_H4
        end
          else begin
                 REPEAT
                  if Assigned(elx_UserFuncsHook) then
                    elx_UserWriteHook(' ', false)
                     else Write(#32);
                   Dec(int_H4);
                 UNTIL (int_H4 = int_H2);
               end; { else }

      {-- Now write the string -----------------------------}
      {$IFDEF MSDOS}
        int_TmpStr := Copy(StringTable^[int_H3]^.Content, 1, StringTable^[int_H3]^.Len);
      {$ELSE}
        int_TmpStr := Copy(StrPas(PChar(StringTable^[int_H3]^.Content)), 1, StringTable^[int_H3]^.Len);
      {$ENDIF}

      if Assigned(elx_UserFuncsHook) then
        elx_UserWriteHook(int_TmpStr, false)
          else Write(int_TmpStr);

      {-- Dispose of the old line --------------------------}
      if (int_OpCode = op_WriteStr2Tmp) then
        StrFreeString(elx_Globals, int_H3);

      {-- Dispose of unused vars ---------------------------}
      Dec(IdentCount, 2);
    end; { with }
end; { op_WriteStr2, op_WriteStr2Tmp }


procedure int_op_InternalError(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do
    begin
      int_ProgState := SysChk;
      int_Continue := FALSE;
    end; { with }
end; { op_InternalError }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function interpret(var elx_Globals: elx_GlobalsType; Params: String; var CallObjptr: Pointer): Boolean;
var int_CallTable: Array[0..255] of CallProcType;

procedure int_InitCallTable;
begin
  int_CallTable[op_LoadAddress] := {$IFDEF FPC}@{$ENDIF}int_op_LoadAddress;
  int_CallTable[op_LoadValue] := {$IFDEF FPC}@{$ENDIF}int_op_LoadValue;
  int_CallTable[op_LoadIndirect] := {$IFDEF FPC}@{$ENDIF}int_op_LoadIndirect;
  int_CallTable[op_UpdateDisplay] := {$IFDEF FPC}@{$ENDIF}int_op_UpdateDisplay;
  int_CallTable[op_IntError_04] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_05] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_06] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_Concatenate] := {$IFDEF FPC}@{$ENDIF}int_op_Concatenate;
  int_CallTable[op_RunFunction] := {$IFDEF FPC}@{$ENDIF}int_op_RunFunction;
  int_CallTable[op_OffSet] := {$IFDEF FPC}@{$ENDIF}int_op_OffSet;
  int_CallTable[op_JumpTo] := {$IFDEF FPC}@{$ENDIF}int_op_JumpTo;
  int_CallTable[op_ConditionalJmp] := {$IFDEF FPC}@{$ENDIF}int_op_ConditionalJmp;
  int_CallTable[op_Switch] := {$IFDEF FPC}@{$ENDIF}int_op_Switch;
  int_CallTable[op_IntError_13] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_For1Up] := {$IFDEF FPC}@{$ENDIF}int_op_For1Up;
  int_CallTable[op_For2Up] := {$IFDEF FPC}@{$ENDIF}int_op_For2Up;
  int_CallTable[op_For1Down] := {$IFDEF FPC}@{$ENDIF}int_op_For1Down;
  int_CallTable[op_For2Down] := {$IFDEF FPC}@{$ENDIF}int_op_For2Down;
  int_CallTable[op_MarkStack] := {$IFDEF FPC}@{$ENDIF}int_op_MarkStack;
  int_CallTable[op_Call] := {$IFDEF FPC}@{$ENDIF}int_op_Call;
  int_CallTable[op_Index1] := {$IFDEF FPC}@{$ENDIF}int_op_Index1;
  int_CallTable[op_Index] := {$IFDEF FPC}@{$ENDIF}int_op_Index;
  int_CallTable[op_LoadBlock] := {$IFDEF FPC}@{$ENDIF}int_op_LoadBlock;
  int_CallTable[op_CopyBlock] := {$IFDEF FPC}@{$ENDIF}int_op_CopyBlock;
  int_CallTable[op_Literal] := {$IFDEF FPC}@{$ENDIF}int_op_Literal;
  int_CallTable[op_LoadReal] := {$IFDEF FPC}@{$ENDIF}int_op_LoadReal;
  int_CallTable[op_LoadFloat] := {$IFDEF FPC}@{$ENDIF}int_op_LoadFloat;
  int_CallTable[op_Read] := {$IFDEF FPC}@{$ENDIF}int_op_Read;
  int_CallTable[op_IntError_28] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_Write1] := {$IFDEF FPC}@{$ENDIF}int_op_Write1;
  int_CallTable[op_Write2] := {$IFDEF FPC}@{$ENDIF}int_op_Write2;
  int_CallTable[op_CharAndString] := {$IFDEF FPC}@{$ENDIF}int_op_CharAndString;
  int_CallTable[op_StringRel] := {$IFDEF FPC}@{$ENDIF}int_op_StringRel;
  int_CallTable[op_RunUserFunc] := {$IFDEF FPC}@{$ENDIF}int_op_RunUserFunc;
  int_CallTable[op_StringLink] := {$IFDEF FPC}@{$ENDIF}int_op_StringLink;
  int_CallTable[op_DynArIndex] := {$IFDEF FPC}@{$ENDIF}int_op_DynArIndex;
  int_CallTable[op_DynArCommand] := {$IFDEF FPC}@{$ENDIF}int_op_DynArCommand;
  int_CallTable[op_DynArChild] := {$IFDEF FPC}@{$ENDIF}int_op_DynArChild;
  int_CallTable[op_IntError_38] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_39] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_40] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_41] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_42] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_43] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_44] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_45] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_46] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_47] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_48] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_49] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_50] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_51] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_52] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_53] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_54] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_55] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_56] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_57] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_58] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_59] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_60] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_61] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_62] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_63] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_64] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_65] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_66] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_67] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_68] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_69] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_70] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_71] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_72] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_73] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_74] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_75] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_76] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_77] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_78] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_79] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_80] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_81] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_82] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_83] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_84] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_85] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_86] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_87] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_88] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_89] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_90] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_91] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_92] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_93] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_94] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_95] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_96] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_97] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_98] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_99] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_100] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_101] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_102] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_103] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_104] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_105] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_106] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_107] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_108] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_109] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_110] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_111] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_112] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_113] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_114] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_115] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_116] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_117] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_118] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_119] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_120] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_121] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_122] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_123] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_124] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_125] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_126] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_127] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_128] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_129] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_130] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_Halt] := {$IFDEF FPC}@{$ENDIF}int_op_Halt;
  int_CallTable[op_ExitProc] := {$IFDEF FPC}@{$ENDIF}int_op_ExitProc;
  int_CallTable[op_ExitFunc] := {$IFDEF FPC}@{$ENDIF}int_op_ExitFunc;
  int_CallTable[op_CopyArray] := {$IFDEF FPC}@{$ENDIF}int_op_CopyArray;
  int_CallTable[op_MakeBoolNot] := {$IFDEF FPC}@{$ENDIF}int_op_MakeBoolNot;
  int_CallTable[op_MakeIntNegative] := {$IFDEF FPC}@{$ENDIF}int_op_MakeIntNegative;
  int_CallTable[op_DecimalReal] := {$IFDEF FPC}@{$ENDIF}int_op_DecimalReal;
  int_CallTable[op_Store] := {$IFDEF FPC}@{$ENDIF}int_op_Store;
  int_CallTable[op_RealEql] := {$IFDEF FPC}@{$ENDIF}int_op_RealEql;
  int_CallTable[op_RealNeq] := {$IFDEF FPC}@{$ENDIF}int_op_RealNeq;
  int_CallTable[op_RealLss] := {$IFDEF FPC}@{$ENDIF}int_op_RealLss;
  int_CallTable[op_RealLeq] := {$IFDEF FPC}@{$ENDIF}int_op_RealLeq;
  int_CallTable[op_RealGtr] := {$IFDEF FPC}@{$ENDIF}int_op_RealGtr;
  int_CallTable[op_RealGeq] := {$IFDEF FPC}@{$ENDIF}int_op_RealGeq;
  int_CallTable[op_IntEql] := {$IFDEF FPC}@{$ENDIF}int_op_IntEql;
  int_CallTable[op_IntNeq] := {$IFDEF FPC}@{$ENDIF}int_op_IntNeq;
  int_CallTable[op_IntLss] := {$IFDEF FPC}@{$ENDIF}int_op_IntLss;
  int_CallTable[op_IntLeq] := {$IFDEF FPC}@{$ENDIF}int_op_IntLeq;
  int_CallTable[op_IntGtr] := {$IFDEF FPC}@{$ENDIF}int_op_IntGtr;
  int_CallTable[op_IntGeq] := {$IFDEF FPC}@{$ENDIF}int_op_IntGeq;
  int_CallTable[op_BoolOr] := {$IFDEF FPC}@{$ENDIF}int_op_BoolOr;
  int_CallTable[op_MinInt] := {$IFDEF FPC}@{$ENDIF}int_op_MinInt;
  int_CallTable[op_AddInt] := {$IFDEF FPC}@{$ENDIF}int_op_AddInt;
  int_CallTable[op_MinReal] := {$IFDEF FPC}@{$ENDIF}int_op_MinReal;
  int_CallTable[op_AddReal] := {$IFDEF FPC}@{$ENDIF}int_op_AddReal;
  int_CallTable[op_MakeBoolAnd] := {$IFDEF FPC}@{$ENDIF}int_op_MakeBoolAnd;
  int_CallTable[op_MultiplyInt] := {$IFDEF FPC}@{$ENDIF}int_op_MultiplyInt;
  int_CallTable[op_DivideInt] := {$IFDEF FPC}@{$ENDIF}int_op_DivideInt;
  int_CallTable[op_ModInt] := {$IFDEF FPC}@{$ENDIF}int_op_ModInt;
  int_CallTable[op_MultiplyReal] := {$IFDEF FPC}@{$ENDIF}int_op_MultiplyReal;
  int_CallTable[op_DivideReal] := {$IFDEF FPC}@{$ENDIF}int_op_DivideReal;
  int_CallTable[op_ReadLn] := {$IFDEF FPC}@{$ENDIF}int_op_ReadLn;
  int_CallTable[op_LineFeed] := {$IFDEF FPC}@{$ENDIF}int_op_LineFeed;
  int_CallTable[op_MakeRealNegative] := {$IFDEF FPC}@{$ENDIF}int_op_MakeRealNegative;
  int_CallTable[op_StrIndex] := {$IFDEF FPC}@{$ENDIF}int_op_StrIndex;
  int_CallTable[op_StringTemp] := {$IFDEF FPC}@{$ENDIF}int_op_StringTemp;
  int_CallTable[op_ArToString] := {$IFDEF FPC}@{$ENDIF}int_op_ArToString;
  int_CallTable[op_StringAndChar] := {$IFDEF FPC}@{$ENDIF}int_op_StringAndChar;
  int_CallTable[op_StrAndStr] := {$IFDEF FPC}@{$ENDIF}int_op_StrAndStr;
  int_CallTable[op_WriteStr] := {$IFDEF FPC}@{$ENDIF}int_op_WriteStrTmp;
  int_CallTable[op_WriteStrTmp] := {$IFDEF FPC}@{$ENDIF}int_op_WriteStrTmp;
  int_CallTable[op_StrValueParam] := {$IFDEF FPC}@{$ENDIF}int_op_StrValueParam;
  int_CallTable[op_StrValParamTmp] := {$IFDEF FPC}@{$ENDIF}int_op_StrValParamTmp;
  int_CallTable[op_CharArAndStr] := {$IFDEF FPC}@{$ENDIF}int_op_CharArAndStr;
  int_CallTable[op_CharArAndStrTmp] := {$IFDEF FPC}@{$ENDIF}int_op_CharArAndStr;
  int_CallTable[op_WriteStr2] := {$IFDEF FPC}@{$ENDIF}int_op_WriteStr2Tmp;
  int_CallTable[op_WriteStr2Tmp] := {$IFDEF FPC}@{$ENDIF}int_op_WriteStr2Tmp;
  int_CallTable[op_IntError_178] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_179] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_180] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_181] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_182] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_183] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_184] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_185] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_186] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_187] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_188] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_189] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_190] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_191] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_192] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_193] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_194] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_195] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_196] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_197] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_198] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_199] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_200] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_201] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_202] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_203] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_204] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_205] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_206] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_207] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_208] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_209] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_210] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_211] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_212] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_213] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_214] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_215] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_216] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_217] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_218] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_219] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_220] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_221] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_222] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_223] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_224] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_225] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_226] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_227] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_228] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_229] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_230] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_231] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_232] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_233] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_234] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_235] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_236] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_237] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_238] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_239] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_240] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_241] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_242] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_243] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_244] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_245] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_246] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_247] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_248] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_249] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_250] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_251] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_252] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_253] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_254] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
  int_CallTable[op_IntError_255] := {$IFDEF FPC}@{$ENDIF}int_op_InternalError;
end; { proc. int_InitCallTable }



procedure showtime(s:string;l: integer);
var hour, min, sec, msec: word;
begin
gettime(hour,min,sec,msec);

writeln(s, l, ' - ', min:2, ':', sec:2, ':', msec:2);
end; { poc. showtime }

var BlockCnt: Longint;

{$ifdef elx_profile}
ick: array[0..100] of real;
iii: integer;
{$endif}
begin { interpret }
{ writeln('bliep(06.5)');}
{$ifdef elx_profile}
{!!!} ick[0] := GetMicroSeconds;	
{$endif}

  {-- Initialize values ---------------------------------------------------}
  elx_Globals.elx_ParamStr := Params;
  Interpret := FALSE;

{$ifdef elx_profile}
{!!!} ick[1] := GetMicroSeconds;	
{$endif}

  with elx_Globals do
    begin
      int_Continue := true;
      int_InitCallTable;
      int_CallObjptr := CallObjptr;
      int_ErrorStr := '';
      FillChar(FileTable, SizeOf(FileTable), 0);
      int_BaseIndex := 0;
      Stack[1].I := 0;
      Stack[2].I := 0;
      Stack[3].I := -1;
      Stack[4].I := BlockTable^[StartBlock].last; { blocktable[2] }
      Display^[StartBlock] := 0;

      IdentCount := BlockTable^[StartBlock + 1].vSize - 1; { blocktable[3] }
      int_ProgPtr := IdentTable^[Stack[4].I].Adr;   { Run the last specified block }
      int_ProgState := RUN;
    end; { with }
{$ifdef elx_profile}
{!!!} ick[2] := GetMicroSeconds;	
{$endif}
{!! writeln('bliep(06.6)'); }

  {-- Now do a stack check -----------------------------------------------}
  if elx_Globals.IdentCount > StackSize then
    with elx_Globals do
      begin
        int_ProgState := STKCHK;
        int_Continue := FALSE;
      end; { if }

{!! writeln('bliep(06.7)'); }

  {-- Clear the stack ----------------------------------------------------}
  with elx_Globals do
    FillChar(Stack[5], (StackSize - 4) * SizeOf(Stack[1]), 0);

{$ifdef elx_profile}
{!!!} ick[3] := GetMicroSeconds;	
{$endif}
{!! writeln('bliep(06.8)'); }

  {-- Initialize EleXer profile information ------------------------------}
  {$IFDEF ELX_PROFILE}
    StrDuplResult := 0;
    FillChar(elx_ProfInfo, SizeOf(elx_ProfInfo), 0);
    FillChar(ConcatCount, SizeOf(ConcatCount), 0);
    FillChar(UserFuncCount, SizeOf(UserFuncCount), 0);
    FillChar(UserFuncSpecifics, SizeOf(UserFuncSpecifics), 0);
    FillChar(FuncCount, SizeOf(FuncCount), 0);
    while elx_globals.modulename[length(elx_globals.modulename)] = ' ' do
      delete(elx_Globals.modulename, length(elx_globals.modulename), 1);
  {$ENDIF}
   OpCodeCount := 0;

{$ifdef elx_profile}
  {!!!} ick[4] := GetMicroSeconds;	
{$endif}

(*{!!!} ShowTime('', 1);*)
{ writeln('bliep(06.9)');}


  {-- Loop through the program -------------------------------------------}
  with elx_Globals do
   while (elx_Globals.int_Continue) do
     begin
       {$IFDEF ELX_PROFILE}
         elx_ProfTimeTmp := elx_ProfGetMSec;
       {$ENDIF}
       OpCodeCount := OpCodeCount + 1;

       {-- Set some default values ----------------------------------------}
       with elx_Globals do
         begin
           {-- get opcode parameters --------------------------------------}
           int_Param2 := ProgCode[int_ProgPtr].Y;     { Default parameter }
           int_OpCode := ProgCode[int_ProgPtr].Func;

           int_PrevPC := int_ProgPtr;
         end; { with }

       {-- And make sure the program keeps flowing ------------------------}
       Inc(int_ProgPtr);

{!!!!!!!!!!!!!!!!}
{WriteLn(opCodeToStr(int_OpCode):20,' -- begin -- ', StrGetFreeCount(elx_Globals),
' stack[118].p = ', elx_Globals.stack[118].p);}
(*
if OpCodeCount MOD 1000 = 0 then
  WriteLn('(', opCodeCount, ') -- [', StrGetFreeCount(elx_Globals), ']...');
*)
{!!!!!!!!!!!!!!!!}

{writeln(sizeof(elx_Globals.stack[1]));}

       {-- and now actually call the program ------------------------------}
       int_CallTable[int_OpCode](elx_Globals);

{!!!!!!!!!!!!!!!!}
{WriteLn(opCodeToStr(int_OpCode):20,' -- exit  -- ', StrGetFreeCount(elx_Globals), ' [', int_progptr,'] -- ', stack[23].i);}
{!!!!!!!!!!!!!!!!}

{!! writeln('bliep(06.-) (02) = ', int_progPtr, ' / ' , opCodeToStr(int_OpCode)); }

       {$IFDEF ELX_PROFILE}
         Inc(elx_ProfInfo[int_OpCode].Count);
         elx_ProfInfo[int_OpCode].MSecs := elx_ProfInfo[int_OpCode].MSecs + (elx_ProfGetMSec - elx_ProfTimeTmp);
       {$ENDIF}
     end; { while }

{$ifdef elx_profile}
{!!!} ick[5] := GetMicroSeconds;	

for iii := 5 downto 0 do
  {!!!} writeln('interpreter: ', iii:2, ']. Seconds: ', real(ick[iii] - ick[0]):3:4, '<br />' );

{!! writeln('bliep(06.10)'); }
{$endif}

  {-- Cleanup routines ----------------------------------------------------}
(* {!!!} showtime('',1); *)

{!!!!}
{WriteLn('Free count  -- exit  -- ', StrGetFreeCount(elx_Globals));}
{!!!!}

    {-- Show profiling information ----------------------------------------}
    {$IFDEF ELX_PROFILE}
    while elx_globals.modulename[length(elx_globals.modulename)] = ' ' do
      delete(elx_Globals.modulename, length(elx_globals.modulename), 1);

    if elx_Globals.modulename = 'eleforum' then
    begin
    elx_ProfTotal := 0;
{    WriteLn('Content-type: text/html;');
    WriteLn;
    WriteLn; }

    for elx_ProfCntr := 0 to 255 do
      begin
        if elx_ProfInfo[elx_ProfCntr].Count > 0 then
          with elx_ProfInfo[elx_ProfCntr] do
            begin
              WriteLn(elx_ProfCntr:3, ': (', Copy(OpCodeToStr(elx_ProfCntr), 1, 29) : 30, ') - ',
                      Count:7, ' (', MSecs:3:8, ' msecs)');
              elx_proftotal := elx_proftotal + msecs;
            end; { with }

      end; { for }

     WriteLn;
     WriteLn;
     WriteLn('elx_ProfTotal=  ', elx_ProfTotal:3:8);
     WriteLn('StrDuplResult = ', StrDuplResult:3:8);
     WriteLn('Total opcodes processed: ', OpCodeCount);
     WriteLn('String entries still available: ', StrGetFreeCount(elx_Globals));

     for elx_ProfCntr  := 0 to 5 do
       writeln('Concat[', elx_ProfCntr,'] = ', ConcatCount[elx_ProfCntr].count, ' (',
               concatcount[elx_ProfCntr].msecs:3:8, ' msecs)');

     writeln('---------------------------');
     for elx_ProfCntr  := 0 to 1000 do
      if UserFuncCount[elx_ProfCntr].count <> 0 then
       writeln('UserFuncCount[', elx_ProfCntr,'] = ', UserFuncCount[elx_ProfCntr].count, ' (',
               userFuncCount[elx_ProfCntr].msecs:3:8, ' msecs)');


     writeln('---------------------------');
     for elx_ProfCntr := 0 to 1000 do
      if FuncCount[elx_ProfCntr].count <> 0 then
       writeln('FuncCount[', elx_ProfCntr,'] = ', FuncCount[elx_ProfCntr].count, ' (',
               FuncCount[elx_ProfCntr].msecs:3:8, ' msecs)');


     writeln('---------------------------');
     for elx_ProfCntr := 0 to 5 do
      if UserFuncSpecifics[elx_ProfCntr].count <> 0 then
       writeln('UserFuncSpecifics[', elx_ProfCntr,'] = ', UserFuncSpecifics[elx_ProfCntr].count, ' (',
               userFuncSpecifics[elx_ProfCntr].msecs:3:8, ' msecs)');
    end; { for }
    {$ENDIF}

    {-- Dispose the stack -------------------------------------------------}

    {-- If program didnt terminate cleanly, show an error -----------------}
    if elx_Globals.int_ProgState <> FIN then
      begin
        Interpret := FALSE;
        if NOT elx_Globals.LogErrors then
         with elx_Globals do
          begin
            int_ErrorStr := 'Error in interpretor code at [' + IntStr((int_ProgPtr - 1), 0) + '] because of: ';

            {-- Show the error ------------------------------------------------}
            Case int_ProgState of
              caschk: int_ErrorStr := int_ErrorStr + 'Undefined case item';
              divchk: int_ErrorStr := int_ErrorStr + 'Division by zero';
              inxchk: int_ErrorStr := int_ErrorStr + 'Invalid index specified';
              stkchk: int_ErrorStr := int_ErrorStr + 'Storage overflow (stack size too small)';
              redchk: int_ErrorStr := int_ErrorStr + 'Reading past end of file';
              strchk: int_ErrorStr := int_ErrorStr + 'String length error';
              fnchk : int_ErrorStr := int_ErrorStr + 'Function argument out of range';
              syschk: int_ErrorStr := int_ErrorStr + 'Bug in compiler (internal error)';
                else int_ErrorStr := int_ErrorStr + 'Undefined runtime error!';
            end; { case }

            {$IFDEF WITH_FULL}
              RaLog('>', '');
              RaLog('>', '');
              RaLog('>', int_ErrorStr);
            {$ENDIF}
          end; { if }

        if elx_Globals.LogErrors then
         with elx_Globals do
          begin
            ErrorFile^.WriteLn('');
            ErrorFile^.Write(' Halt at ' + IntStr((int_ProgPtr - 1), 5) + ' because of ');

            {-- Show the error ------------------------------------------------}
            Case int_ProgState of
              caschk  : ErrorFile^.Writeln('Undefined case item');
              divchk  : ErrorFile^.Writeln('Division by zero');
              inxchk  : ErrorFile^.Writeln('Invalid index specified');
              stkchk  : ErrorFile^.Writeln('Storage overflow (stack size too small)');
              redchk  : ErrorFile^.Writeln('Reading past end of file');
              strchk  : ErrorFile^.Writeln('String length error');
              fnchk   : ErrorFile^.Writeln('Function argument out of range');
              syschk  : ErrorFile^.Writeln('Bug in compiler (internal error)');
              basezchk: ErrorFile^.WriteLn('BaseZero out of range');
            end; { case }

            {-- Now, dump part of the stack -------------------------------------}
            int_H1 := int_BaseIndex;
            BlockCnt := 10;                        { Max. dump the last 10 blocks }

            REPEAT
              ErrorFile^.WriteLn('');

              Dec(BlockCnt);                        { Make sure we exit some time }
              if BlockCnt = 0 then int_H1 := 0;

              {-- Get a index to the tables -------------------------------------}
              int_H2 := Stack[int_H1 + 4].I;

              {-- Write a header ------------------------------------------------}
              if int_H1 <> 0 then
                ErrorFile^.WriteLn(#32 + IdentTable^[int_H2].Name + ' called at ' + IntStr(Stack[int_H1+1].I, 5));

              {-- Loop through all vars defined here ----------------------------}
              int_H2 := BlockTable^[IdentTable^[int_H2].Ref].Last;

              while int_H2 <> 0 do
                with IdentTable^[int_h2] do
                  begin
                    if obj = obj_variable then
                      if typ in StandardTypes then
                        begin
                          ErrorFile^.Write('    ' + Name + ' = ');

                          if Normal then
                            int_H3 := int_H1 + Adr
                              else int_H3 := Stack[int_H1 + Adr].I;

                          {-- Show the actual value -----------------------------}
                          Case Typ of
                            typ_ints  : ErrorFile^.WriteLn(IntStr(Stack[int_h3].I, 0));
                            typ_reals : ErrorFile^.WriteLn('<real>');
                            typ_bools : if Stack[int_H3].B then
                                          ErrorFile^.WriteLn('true')
                                            else ErrorFile^.WriteLn('false');
                            typ_chars : ErrorFile^.WriteLn(Chr(Stack[int_h3].i MOD 64));
                            typ_strngs: begin
                                          int_H3 := Stack[int_h3].i;

                                          ErrorFile^.Write('"');
                                          {$IFDEF MSDOS}
                                            int_TmpStr := Copy(StringTable^[int_H3]^.Content, 1, StringTable^[int_H3]^.Len);
                                          {$ELSE}
                                            int_TmpStr := Copy(StrPas(PChar(StringTable^[int_H3]^.Content)), 1,
                                                               StringTable^[int_H3]^.Len);
                                          {$ENDIF}
                                          ErrorFile^.Write(int_TmpStr);
                                          ErrorFile^.Write('"');
                                        end; { typ_Strngs }
                          end; { case }
                        end; { if in standard types and stuff }

                    {-- Move one up in the row ----------------------------------}
                    int_H2 := Link;
                  end; { while }


              int_H1 := Stack[int_h1+3].I;
            UNTIL (int_H1 < 0);
          end; { if logerrors }
      end { if the program crashed }
        else Interpret := TRUE;
end; { func. Interpret }

end. { unit ELX_INT }
