{$R-,S-,V-,I-,B-,F-}
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

{Disable the following define if you don't have Turbo Professional}
{.$DEFINE UseTpro}

{*********************************************************}
{*                    TPENV.PAS 1.02                     *}
{*                by TurboPower Software                 *}
{*********************************************************}

{
  Version 1.01 11/7/88
    Find master environment in DOS 3.3 and 4.0
  Version 1.02 11/14/88
    Correctly find master environment when run
      within AUTOEXEC.BAT
}

unit TpEnv;
  {-Manipulate the environment}

interface

  {$IFDEF UseTpro}
uses
  TpString,
  TpDos;
  {$ENDIF}

type
  EnvArray = array[0..32767] of Char;
  EnvArrayPtr = ^EnvArray;
  EnvRec =
    record
      EnvSeg : Word;              {Segment of the environment}
      EnvLen : Word;              {Usable length of the environment}
      EnvPtr : Pointer;           {Nil except when allocated on heap}
    end;

const
  ShellUserProc : Pointer = nil;  {Put address of ExecDos user proc here if desired}

procedure MasterEnv(var Env : EnvRec);
  {-Return master environment record}

procedure CurrentEnv(var Env : EnvRec);
  {-Return current environment record}

procedure NewEnv(var Env : EnvRec; Size : Word);
  {-Allocate a new environment on the heap}

procedure DisposeEnv(var Env : EnvRec);
  {-Deallocate an environment previously allocated on heap}

procedure SetCurrentEnv(Env : EnvRec);
  {-Specify a different environment for the current program}

procedure CopyEnv(Src, Dest : EnvRec);
  {-Copy contents of Src environment to Dest environment}

function EnvFree(Env : EnvRec) : Word;
  {-Return bytes free in environment}

function GetEnvStr(Env : EnvRec; Search : string) : string;
  {-Return a string from the environment}

function SetEnvStr(Env : EnvRec; Search, Value : string) : Boolean;
  {-Set environment string, returning true if successful}

procedure DumpEnv(Env : EnvRec);
  {-Dump the environment to StdOut}

function ProgramStr : string;
  {-Return the complete path to the current program, '' if DOS < 3.0}

function SetProgramStr(Env : EnvRec; Path : string) : Boolean;
  {-Add a program name to the end of an environment if sufficient space}

  {$IFDEF UseTpro}
function ShellWithPrompt(Prompt : string) : Integer;
  {-Shell to DOS with a new prompt}
  {$ENDIF}

  {===============================================================}

implementation

type
  SO =
    record
      O : Word;
      S : Word;
    end;

  procedure ClearEnvRec(var Env : EnvRec);
    {-Initialize an environment record}
  begin
    FillChar(Env, SizeOf(Env), 0);
  end;

  procedure MasterEnv(var Env : EnvRec);
    {-Return master environment record}
  var
    Owner : Word;
    Mcb : Word;
    Eseg : Word;
    Done : Boolean;
  begin
    with Env do begin
      ClearEnvRec(Env);

      {Interrupt $2E points into COMMAND.COM}
      Owner := MemW[0:(2+4*$2E)];

      {Mcb points to memory control block for COMMAND}
      Mcb := Owner-1;
      if (Mem[Mcb:0] <> Byte('M')) or (MemW[Mcb:1] <> Owner) then
        Exit;

      {Read segment of environment from PSP of COMMAND}
      Eseg := MemW[Owner:$2C];

      {Earlier versions of DOS don't store environment segment there}
      if Eseg = 0 then begin
        {Master environment is next block past COMMAND}
        Mcb := Owner+MemW[Mcb:3];
        if (Mem[Mcb:0] <> Byte('M')) or (MemW[Mcb:1] <> Owner) then
          {Not the right memory control block}
          Exit;
        Eseg := Mcb+1;
      end else
        Mcb := Eseg-1;

      {Return segment and length of environment}
      EnvSeg := Eseg;
      EnvLen := MemW[Mcb:3] shl 4;
    end;
  end;

  procedure CurrentEnv(var Env : EnvRec);
    {-Return current environment record}
  var
    ESeg : Word;
    Mcb : Word;
  begin
    with Env do begin
      ClearEnvRec(Env);
      ESeg := MemW[PrefixSeg:$2C];
      Mcb := ESeg-1;
      if (Mem[Mcb:0] <> Byte('M')) or (MemW[Mcb:1] <> PrefixSeg) then
        Exit;
      EnvSeg := ESeg;
      EnvLen := MemW[Mcb:3] shl 4;
    end;
  end;

  procedure NewEnv(var Env : EnvRec; Size : Word);
    {-Allocate a new environment (on the heap)}
  var
    Mcb : Word;
  begin
    with Env do
      if MaxAvail < Size+31 then
        {Insufficient space}
        ClearEnvRec(Env)
      else begin
        {31 extra bytes for paragraph alignment, fake MCB}
        GetMem(EnvPtr, Size+31);
        EnvSeg := SO(EnvPtr).S+1;
        if SO(EnvPtr).O <> 0 then
          Inc(EnvSeg);
        EnvLen := Size;
        {Fill it with nulls}
        FillChar(EnvPtr^, Size+31, 0);
        {Make a fake MCB below it}
        Mcb := EnvSeg-1;
        Mem[Mcb:0] := Byte('M');
        MemW[Mcb:1] := PrefixSeg;
        MemW[Mcb:3] := (Size+15) shr 4;
      end;
  end;

  procedure DisposeEnv(var Env : EnvRec);
    {-Deallocate an environment previously allocated on heap}
  begin
    with Env do
      if EnvPtr <> nil then begin
        FreeMem(EnvPtr, EnvLen+31);
        ClearEnvRec(Env);
      end;
  end;

  procedure SetCurrentEnv(Env : EnvRec);
    {-Specify a different environment for the current program}
  begin
    with Env do
      if EnvSeg <> 0 then
        MemW[PrefixSeg:$2C] := EnvSeg;
  end;

  procedure CopyEnv(Src, Dest : EnvRec);
    {-Copy contents of Src environment to Dest environment}
  var
    Size : Word;
    SPtr : EnvArrayPtr;
    DPtr : EnvArrayPtr;
  begin
    if (Src.EnvSeg = 0) or (Dest.EnvSeg = 0) then
      Exit;

    if Src.EnvLen <= Dest.EnvLen then
      {Space for the whole thing}
      Size := Src.EnvLen
    else
      {Take what fits}
      Size := Dest.EnvLen-1;

    SPtr := Ptr(Src.EnvSeg, 0);
    DPtr := Ptr(Dest.EnvSeg, 0);
    Move(SPtr^, DPtr^, Size);
    FillChar(DPtr^[Size], Dest.EnvLen-Size, 0);
  end;

  procedure SkipAsciiZ(EPtr : EnvArrayPtr; var EOfs : Word);
    {-Skip to end of current AsciiZ string}
  begin
    while EPtr^[EOfs] <> #0 do
      Inc(EOfs);
  end;

  function EnvNext(EPtr : EnvArrayPtr) : Word;
    {-Return the next available location in environment at EPtr^}
  var
    EOfs : Word;
  begin
    EOfs := 0;
    if EPtr <> nil then begin
      while EPtr^[EOfs] <> #0 do begin
        SkipAsciiZ(EPtr, EOfs);
        Inc(EOfs);
      end;
    end;
    EnvNext := EOfs;
  end;

  function EnvFree(Env : EnvRec) : Word;
    {-Return bytes free in environment}
  begin
    with Env do
      if EnvSeg <> 0 then
        EnvFree := EnvLen-EnvNext(Ptr(EnvSeg, 0))-1
      else
        EnvFree := 0;
  end;

  {$IFNDEF UseTpro}
  function StUpcase(S : string) : string;
    {-Uppercase a string}
  var
    SLen : byte absolute S;
    I : Integer;
  begin
    for I := 1 to SLen do
      S[I] := UpCase(S[I]);
    StUpcase := S;
  end;
  {$ENDIF}

  function SearchEnv(EPtr : EnvArrayPtr;
                     var Search : string) : Word;
    {-Return the position of Search in environment, or $FFFF if not found.
      Prior to calling SearchEnv, assure that
        EPtr is not nil,
        Search is not empty
    }
  var
    SLen : Byte absolute Search;
    EOfs : Word;
    MOfs : Word;
    SOfs : Word;
    Match : Boolean;
  begin
    {Force upper case search}
    Search := StUpcase(Search);

    {Assure search string ends in =}
    if Search[SLen] <> '=' then begin
      Inc(SLen);
      Search[SLen] := '=';
    end;

    EOfs := 0;
    while EPtr^[EOfs] <> #0 do begin
      {At the start of a new environment element}
      SOfs := 1;
      MOfs := EOfs;
      repeat
        Match := (EPtr^[EOfs] = Search[SOfs]);
        if Match then begin
          Inc(EOfs);
          Inc(SOfs);
        end;
      until not Match or (SOfs > SLen);

      if Match then begin
        {Found a match, return index of start of match}
        SearchEnv := MOfs;
        Exit;
      end;

      {Skip to end of this environment string}
      SkipAsciiZ(EPtr, EOfs);

      {Skip to start of next environment string}
      Inc(EOfs);
    end;

    {No match}
    SearchEnv := $FFFF;
  end;

  procedure GetAsciiZ(EPtr : EnvArrayPtr; var EOfs : Word; var EStr : string);
    {-Collect AsciiZ string starting at EPtr^[EOfs]}
  var
    ELen : Byte absolute EStr;
  begin
    ELen := 0;
    while (EPtr^[EOfs] <> #0) and (ELen < 255) do begin
      Inc(ELen);
      EStr[ELen] := EPtr^[EOfs];
      Inc(EOfs);
    end;
  end;

  function GetEnvStr(Env : EnvRec; Search : string) : string;
    {-Return a string from the environment}
  var
    SLen : Byte absolute Search;
    EPtr : EnvArrayPtr;
    EOfs : Word;
    EStr : string;
    ELen : Byte absolute EStr;
  begin
    with Env do begin
      ELen := 0;
      if (EnvSeg <> 0) and (SLen <> 0) then begin
        {Find the search string}
        EPtr := Ptr(EnvSeg, 0);
        EOfs := SearchEnv(EPtr, Search);
        if EOfs <> $FFFF then begin
          {Skip over the search string}
          Inc(EOfs, SLen);
          {Build the result string}
          GetAsciiZ(EPtr, EOfs, EStr);
        end;
      end;
      GetEnvStr := EStr;
    end;
  end;

  function SetEnvStr(Env : EnvRec; Search, Value : string) : Boolean;
    {-Set environment string, returning true if successful}
  var
    SLen : Byte absolute Search;
    VLen : Byte absolute Value;
    EPtr : EnvArrayPtr;
    ENext : Word;
    EOfs : Word;
    MOfs : Word;
    OldLen : Word;
    NewLen : Word;
    NulLen : Word;
  begin
    with Env do begin
      SetEnvStr := False;
      if (EnvSeg = 0) or (SLen = 0) then
        Exit;
      EPtr := Ptr(EnvSeg, 0);

      {Find the search string}
      EOfs := SearchEnv(EPtr, Search);

      {Get the index of the next available environment location}
      ENext := EnvNext(EPtr);

      {Get total length of new environment string}
      NewLen := SLen+VLen;

      if EOfs <> $FFFF then begin
        {Search string exists}
        MOfs := EOfs+SLen;
        {Scan to end of string}
        SkipAsciiZ(EPtr, MOfs);
        OldLen := MOfs-EOfs;
        {No extra nulls to add}
        NulLen := 0;
      end else begin
        OldLen := 0;
        {One extra null to add}
        NulLen := 1;
      end;

      if VLen <> 0 then
        {Not a pure deletion}
        if ENext+NewLen+NulLen >= EnvLen+OldLen then
          {New string won't fit}
          Exit;

      if OldLen <> 0 then begin
        {Overwrite previous environment string}
        Move(EPtr^[MOfs+1], EPtr^[EOfs], ENext-MOfs-1);
        {More space free now}
        Dec(ENext, OldLen+1);
      end;

      {Append new string}
      if VLen <> 0 then begin
        Move(Search[1], EPtr^[ENext], SLen);
        Inc(ENext, SLen);
        Move(Value[1], EPtr^[ENext], VLen);
        Inc(ENext, VLen);
      end;

      {Clear out the rest of the environment}
      FillChar(EPtr^[ENext], EnvLen-ENext, 0);

      SetEnvStr := True;
    end;
  end;

  procedure DumpEnv(Env : EnvRec);
    {-Dump the environment to StdOut}
  var
    EOfs : Word;
    EPtr : EnvArrayPtr;
  begin
    with Env do begin
      if EnvSeg = 0 then
        Exit;
      EPtr := Ptr(EnvSeg, 0);
      EOfs := 0;
      WriteLn;
      while EPtr^[EOfs] <> #0 do begin
        while EPtr^[EOfs] <> #0 do begin
          Write(EPtr^[EOfs]);
          Inc(EOfs);
        end;
        WriteLn;
        Inc(EOfs);
      end;
      WriteLn('Bytes free: ', EnvFree(Env));
    end;
  end;

  function DosVersion : Word;
    {-Return the DOS version, major part in AX}
  inline(
         $B4/$30/                 {mov ah,$30}
         $CD/$21/                 {int $21}
         $86/$C4);                {xchg ah,al}

  function ProgramStr : string;
    {-Return the name of the current program, '' if DOS < 3.0}
  var
    EOfs : Word;
    Env : EnvRec;
    EPtr : EnvArrayPtr;
    PStr : string;
  begin
    ProgramStr := '';
    if DosVersion < $0300 then
      Exit;
    CurrentEnv(Env);
    if Env.EnvSeg = 0 then
      Exit;
    {Find the end of the current environment}
    EPtr := Ptr(Env.EnvSeg, 0);
    EOfs := EnvNext(EPtr);
    {Skip to start of path name}
    Inc(EOfs, 3);
    {Collect the path name}
    GetAsciiZ(EPtr, EOfs, PStr);
    ProgramStr := PStr;
  end;

  function SetProgramStr(Env : EnvRec; Path : string) : Boolean;
    {-Add a program name to the end of an environment if sufficient space}
  var
    PLen : Byte absolute Path;
    EOfs : Word;
    Numb : Word;
    EPtr : EnvArrayPtr;
  begin
    SetProgramStr := False;
    with Env do begin
      if EnvSeg = 0 then
        Exit;
      {Find the end of the current environment}
      EPtr := Ptr(EnvSeg, 0);
      EOfs := EnvNext(EPtr);
      {Assure space for path}
      if EnvLen < PLen+EOfs+4 then
        Exit;
      {Put in the count field}
      Inc(EOfs);
      Numb := 1;
      Move(Numb, EPtr^[EOfs], 2);
      {Skip to start of path name}
      Inc(EOfs, 2);
      {Move the path into place}
      Path := StUpcase(Path);
      Move(Path[1], EPtr^[EOfs], PLen);
      {Null terminate}
      Inc(EOfs, PLen);
      EPtr^[EOfs] := #0;
      SetProgramStr := True;
    end;
  end;

  {$IFDEF UseTpro}
  function ShellWithPrompt(Prompt : string) : Integer;
    {-Shell to DOS with a new prompt}
  const
    PromptStr : string[7] = 'PROMPT=';
  var
    PLen : Byte absolute Prompt;
    NSize : Word;
    Status : Integer;
    CE : EnvRec;
    NE : EnvRec;
    OldP : string;
    OldPLen : Byte absolute OldP;
  begin
    {Point to current environment}
    CurrentEnv(CE);
    if CE.EnvSeg = 0 then begin
      {Error getting environment}
      ShellWithPrompt := -5;
      Exit;
    end;

    {Compute size of new environment}
    OldP := GetEnvStr(CE, PromptStr);
    NSize := CE.EnvLen;
    if OldPLen < PLen then
      Inc(NSize, PLen-OldPLen);

    {Allocate and initialize a new environment}
    NewEnv(NE, NSize);
    if NE.EnvSeg = 0 then begin
      {Insufficient memory for new environment}
      ShellWithPrompt := -6;
      Exit;
    end;
    CopyEnv(CE, NE);

    {Get the program name from the current environment}
    OldP := ProgramStr;

    {Set the new prompt string}
    if not SetEnvStr(NE, PromptStr, Prompt) then begin
      {Program error, should have enough space}
      ShellWithPrompt := -7;
      Exit;
    end;

    {Transfer program name to new environment if possible}
    if not SetProgramStr(NE, OldP) then
      ;

    {Point to new environment}
    SetCurrentEnv(NE);

    {Shell to DOS with new prompt in place}
    Status := ExecDos('', True, ShellUserProc);

    {Restore previous environment}
    SetCurrentEnv(CE);

    {Release the heap space}
    if Status >= 0 then
      DisposeEnv(NE);

    {Return exec status}
    ShellWithPrompt := Status;
  end;
  {$ENDIF}

end.
