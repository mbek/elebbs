{$S-,R-,I-}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{*********************************************************}
{*                 LEAKCHEK.PAS 2.03                     *}
{*     Copyright (c) TurboPower Software 1991.           *}
{*********************************************************}

unit LeakChek;
  {-Check for memory leaks between startup and shutdown}

interface

implementation

{$IFDEF DPMI}
uses
  Dpmi;
{$ENDIF}

var
  StartMem   : LongInt;
  StopMem    : LongInt;
  StartMax   : LongInt;
  StopMax    : LongInt;
  StartFH    : Word;
  StopFH     : Word;
  SaveExit   : Pointer;

  function FileHandlesOpen(CountDevices : Boolean) : Byte;
    {-Return the number of open files owned by a program}
  type
    HandleTable = array[0..254] of Byte;
  var
    HandlesPtr : ^HandleTable;
    I, N, Max : Byte;
    {$IFDEF Dpmi}
    Sele : Word;
    P : Pointer;
    {$ENDIF}
  begin
    {$IFDEF Dpmi}
    LongInt(P) := MemL[PrefixSeg:$34];
    if not GetSelectorForRealMem(P, $100, Sele) = 0 then
      Exit;
    HandlesPtr := Ptr(Sele, 0);
    {$ELSE}
    {pointer to file handles table at PrefixSeg:$34}
    HandlesPtr := Pointer(MemL[PrefixSeg:$34]);
    {$ENDIF}

    {size of file handles table at PrefixSeg:$32}
    Max := Mem[PrefixSeg:$0032]-1;

    N := 0;
    for I := 0 to Max do
      if HandlesPtr^[I] <> $FF then
        case I of
          0..4 : Inc(N, Ord(CountDevices));
          else   Inc(N);
        end;

    FileHandlesOpen := N;
    {$IFDEF Dpmi}
    if FreeLDTDescriptor(Sele) = 0 then ;
    {$ENDIF}
  end;

  procedure LCExitProc; far;
  begin
    ExitProc := SaveExit;
    StopMax := MaxAvail;
    StopMem := MemAvail;
    StopFH  := FileHandlesOpen(True);
    WriteLn(^J^M^J, '-----------------------------------');
    WriteLn('Starting mem/max: ', StartMem, '/', StartMax);
    WriteLn('Ending mem/max:   ', StopMem, '/', StopMax);
    {$IFDEF DPMI}
    if (StartMem <> StopMem) then begin
      WriteLn('Memory loss of ' , StartMem - StopMem, ' bytes');
      WriteLn('due to memory leaks or RTM consumption');
    end;
    {$ELSE}
    if (StartMem <> StopMem) then begin
      WriteLn(^G, 'memory leak!');
      WriteLn('Lost ', StartMem - StopMem, ' bytes');
    end;
    {$ENDIF}
    WriteLn;
    WriteLn('Starting file handles: ', StartFH);
    WriteLn('Ending file handles: ', StopFH);
    if StartFH <> StopFH then
      WriteLn('File handle leak of ', StopFH - StartFH);
    WriteLn('-----------------------------------');
  end;

begin
  SaveExit := ExitProc;
  ExitProc := @LCExitProc;
  StartMem := MemAvail;
  StartMax := MaxAvail;
  StartFH := FileHandlesOpen(True);
end.



