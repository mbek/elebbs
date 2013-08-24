{!!! S-,R-,V-,I-,B-,F+,O+,A-}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{Include OPro's define file if UseOPro is specified}
{$IFDEF UseOPro}
{$I OPDEFINE.INC}
{$ENDIF}

{Activates Standalone mode heap debugging reports}
{.$DEFINE HeapDebug}

{*********************************************************}
{*                   APMISC.PAS 2.03                     *}
{*        Copyright (c) TurboPower Software 1991.        *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit ApMisc;
  {-Error codes, checksuming, date and other miscellaneous routines}

interface

uses
  {$IFDEF UseOPro}
  OpDate,
  {$ENDIF}
  {$IFDEF UseTPro}
  TpDate,
  {$ENDIF}
  {$IFNDEF DELPHI}
  Dos;
  {$ELSE}
  Windows,
  Sysutils;
  {$ENDIF}

{$I APMISC.PA0}

  {==========================================================================}

implementation

{$I APMISC.PA1}         {StatusStr procedure with default messages}

{$IFDEF HeapDebug}
var
  HDebug : Text;
  SaveExit : Pointer;
{$ENDIF}

{$IFNDEF MSDOS}
function UpdateCrc32(CurByte : Byte; CurCrc : LongInt) : LongInt;
  {-Returns an updated crc32}
begin
  UpdateCrc32 := Longint(Crc32Table[Byte(CurCrc xor LongInt(CurByte))] xor
                 ((CurCrc shr 8) and $00FFFFFF));
end;
{$ENDIF}

{$IFNDEF MSDOS}
procedure SetFlag(var Flags : Word; FlagMask : Word);
begin                                                                  {!!.03}
  Flags := Flags or FlagMask;                                          {!!.03}
end;                                                                   {!!.03}

procedure ClearFlag(var Flags : Word; FlagMask : Word);
begin                                                                  {!!.03}
  Flags := Flags and not FlagMask;                                     {!!.03}
end;                                                                   {!!.03}

function FlagIsSet(Flags, FlagMask : Word) : Boolean;
begin                                                                  {!!.03}
  FlagIsSet := (Flags and FlagMask) <> 0;                              {!!.03}
end;                                                                   {!!.03}

function MinWord(A, B : Word) : Word;
begin
  if A < B then MinWord := A
    else MinWord := B;
end; { func. MinWord }

function MakeLongInt(H, L : Word) : LongInt;
var HL: Array[1..2] of Word;
    L2: Longint ABSOLUTE HL;
begin
  HL[1] := H;
  HL[2] := L;

  MakeLongint := L2;
end;

function AddWordToPtr(P : Pointer; W : Word) : Pointer;
begin
  Inc(Longint(P^), W);
  AddWordToptr := p;
end;

procedure DecPtr(var P; Size : Word);
begin
  Dec(Longint(P), Size);
end;

procedure IncPtr(var P; Size : Word);
begin
  Inc(Longint(P), Size);
end;


{$ENDIF}

{$IFNDEF UseOPro}
  constructor Root.Init;
    {-Root constructor for all objects}
  begin
    {nothing to do}
  end;

  destructor Root.Done;
    {-Root destructor for all objects}
  begin
    {nothing to do}
  end;
{$ENDIF}

{$IFDEF Standalone}
const
  DosDelimSet : set of Char = ['\', ':', '/', #0];
  Digits : array[0..$F] of Char = '0123456789ABCDEF';
{$ENDIF}

  function UpdateChecksum(CurByte : Byte; CheckSum : Word) : Word;
    {-Returns an updated checksum}
  begin
    UpdateCheckSum := CheckSum + CurByte;
  end;

  function UpdateCrc(CurByte : Byte; CurCrc : Word) : Word;
    {-Returns an updated CRC16}
  begin
    UpdateCrc := CrcTable[((CurCrc shr 8) and 255)] xor
                 (CurCrc shl 8) xor CurByte;
  end;

  function UpdateCrcKermit(CurByte : Byte; CurCrc : Word) : Word;
    {-Returns an updated Crc16 (kermit style)}
  var
    I : Integer;
    Temp : Integer;
  begin
    for I := 0 to 7 do begin
      Temp := CurCrc xor CurByte;
      CurCrc := CurCrc shr 1;
      if Odd(Temp) then
        CurCrc := CurCrc xor $8408;
      CurByte := CurByte shr 1;
    end;
    UpdateCrcKermit := CurCrc;
  end;

  {$IFDEF Standalone}
  function IsLeapYear(Year : Integer) : Boolean;
    {-Return True if Year is a leap year}
  begin
    IsLeapYear := (Year mod 4 = 0) and (Year mod 4000 <> 0) and
      ((Year mod 100 <> 0) or (Year mod 400 = 0));
  end;

  function DaysInMonth(Month, Year : Integer) : Integer;
    {-Return the number of days in the specified month of a given year}
  begin
    if Word(Year) < 100 then begin
      Inc(Year, 1900);
      if Year < Threshold2000 then
        Inc(Year, 100);
    end;

    case Month of
      1, 3, 5, 7, 8, 10, 12 :
        DaysInMonth := 31;
      4, 6, 9, 11 :
        DaysInMonth := 30;
      2 :
        DaysInMonth := 28+Ord(IsLeapYear(Year));
    else
      DaysInMonth := 0;
    end;
  end;

  function ValidDate(Day, Month, Year : Integer) : Boolean;
    {-Verify that day, month, year is a valid date}
  begin
    if Word(Year) < 100 then begin
      Inc(Year, 1900);
      if Year < Threshold2000 then
        Inc(Year, 100);
    end;

    if (Day < 1) or (Year < MinYear) or (Year > MaxYear) then
      ValidDate := False
    else case Month of
      1..12 :
        ValidDate := Day <= DaysInMonth(Month, Year);
    else
      ValidDate := False;
    end
  end;

  function DMYtoDate(Day, Month, Year : Integer) : Date;
    {-Convert from day, month, year to a julian date}
  begin
    if Word(Year) < 100 then begin
      Inc(Year, 1900);
      if Year < Threshold2000 then
        Inc(Year, 100);
    end;

    if not ValidDate(Day, Month, Year) then
      DMYtoDate := BadDate
    else if (Year = MinYear) and (Month < 3) then
      if Month = 1 then
        DMYtoDate := Pred(Day)
      else
        DMYtoDate := Day+30
    else begin
      if Month > 2 then
        Dec(Month, 3)
      else begin
        Inc(Month, 9);
        Dec(Year);
      end;
      Dec(Year, MinYear);
      DMYtoDate :=
          ((LongInt(Year)*1461) div 4)+
          (((153*Month)+2) div 5)+Day+First2Months;
    end;
  end;

  procedure DateToDMY(Julian : Date; var Day, Month, Year : Integer);
    {-Convert from a julian date to month, day, year}
  var
    I : LongInt;
  begin
    if Julian = BadDate then begin
      Day := 0;
      Month := 0;
      Year := 0;
    end
    else if Julian <= First2Months then begin
      Year := MinYear;
      if Julian <= 30 then begin
        Month := 1;
        Day := Succ(Julian);
      end
      else begin
        Month := 2;
        Day := Julian-30;
      end;
    end
    else begin
      I := (4*LongInt(Julian-First2Months))-1;
      Year := I div 1461;
      I := (5*((I mod 1461) div 4)) + 2;
      Month := I div 153;
      Day := ((I mod 153)+5) div 5;
      if Month < 10 then
        Inc(Month, 3)
      else begin
        Dec(Month, 9);
        Inc(Year);
      end;
      Inc(Year, MinYear);
    end;
  end;

  procedure DateTimeDiff(DT1, DT2 : DateTimeRec; var Days : Word; var Secs : LongInt);
    {-Return the difference in days and seconds between two points in time}
  var
    DTTemp : DateTimeRec;
  begin
    {swap if DT1 later than DT2}
    if (DT1.D > DT2.D) or ((DT1.D = DT2.D) and (DT1.T > DT2.T)) then begin
      DTTemp := DT1;
      DT1 := DT2;
      DT2 := DTTemp;
    end;

    {the difference in days is easy}
    Days := DT2.D-DT1.D;

    {difference in seconds}
    if DT2.T < DT1.T then begin
      {subtract one day, add 24 hours}
      Dec(Days);
      Inc(DT2.T, SecondsInDay);
    end;
    Secs := DT2.T-DT1.T;
  end;

  procedure TimeToHMS(T : Time; var Hours, Minutes, Seconds : Byte);
    {-Convert a Time variable to Hours, Minutes, Seconds}
  begin
    if T = BadTime then begin
      Hours := 0;
      Minutes := 0;
      Seconds := 0;
    end
    else begin
      Hours := T div SecondsInHour;
      Dec(T, LongInt(Hours)*SecondsInHour);
      Minutes := T div SecondsInMinute;
      Dec(T, LongInt(Minutes)*SecondsInMinute);
      Seconds := T;
    end;
  end;

  function HMStoTime(Hours, Minutes, Seconds : Byte) : Time;
    {-Convert Hours, Minutes, Seconds to a Time variable}
  var
    T : Time;
  begin
    Hours := Hours mod HoursInDay;
    T := (LongInt(Hours)*SecondsInHour)+(LongInt(Minutes)*SecondsInMinute)+Seconds;
    HMStoTime := T mod SecondsInDay;
  end;

  procedure IncDateTime(var DT1, DT2 : DateTimeRec; Days : Integer; Secs : LongInt);
    {-Increment (or decrement) DT1 by the specified number of days and seconds
      and put the result in DT2}
  begin
    DT2 := DT1;

    {date first}
    {$IFDEF MSDOS}
      Inc(Integer(DT2.D), Days);
    {$ELSE}
      Inc(Dt2.D, Days);
    {$ENDIF}


    if Secs < 0 then begin
      {change the sign}
      Secs := -Secs;

      {adjust the date}
      Dec(DT2.D, Secs div SecondsInDay);
      Secs := Secs mod SecondsInDay;

      if Secs > DT2.T then begin
        {subtract a day from DT2.D and add a day's worth of seconds to DT2.T}
        Dec(DT2.D);
        Inc(DT2.T, SecondsInDay);
      end;

      {now subtract the seconds}
      Dec(DT2.T, Secs);
    end
    else begin
      {increment the seconds}
      Inc(DT2.T, Secs);

      {adjust date if necessary}
      Inc(DT2.D, DT2.T div SecondsInDay);

      {force time to 0..SecondsInDay-1 range}
      DT2.T := DT2.T mod SecondsInDay;
    end;
  end;

  function WordPosition(N : Byte; S : string; WordDelims : CharSet) : Byte;
    {-Given a set of word delimiters, return start position of N'th word in S}
  var
    {I,} Count : Byte;
    I : Word;
    SLen : Byte absolute S;
  begin
    Count := 0;
    I := 1;
    WordPosition := 0;

    while (I <= SLen) and (Count <> N) do begin
      {skip over delimiters}
      while (I <= SLen) and (S[I] in WordDelims) do
        Inc(I);

      {if we're not beyond end of S, we're at the start of a word}
      if I <= SLen then
        Inc(Count);

      {if not finished, find the end of the current word}
      if Count <> N then
        while (I <= SLen) and not(S[I] in WordDelims) do
          Inc(I)
      else
        WordPosition := I;
    end;
  end;

  function ExtractWord(N : Byte; S : string; WordDelims : CharSet) : string;
    {-Given a set of word delimiters, return the N'th word in S}
  var
    I : Word;
    Len : Byte;
    SLen : Byte absolute S;
  begin
    Len := 0;
    I := WordPosition(N, S, WordDelims);
    if I <> 0 then
      {find the end of the current word}
      while (I <= SLen) and not(S[I] in WordDelims) do begin
        {add the I'th character to result}
        Inc(Len);
        ExtractWord[Len] := S[I];
        Inc(I);
      end;
    ExtractWord[0] := Char(Len);
  end;

  function StUpcase(S : string) : string;
    {-Convert lower case letters in string to uppercase}
  var
    I : Word;
  begin
    StUpcase[0] := S[0];
    for I := 1 to Length(S) do
      StUpcase[I] := UpCase(S[I]);
  end;

  function PadCh(S : string; Ch : Char; Len : Byte) : string;
    {-Return a string right-padded to length Len with Ch}
  var
    O : string;
    SLen : Byte absolute S;
  begin
    if Length(S) >= Len then
      PadCh := S
    else begin
      O[0] := Chr(Len);
      Move(S[1], O[1], SLen);
      if SLen < 255 then
        FillChar(O[Succ(SLen)], Len-SLen, Ch);
      PadCh := O;
    end;
  end;

  function HexW(W : Word) : string;
    {-Return hex string for word}
  begin
    HexW[0] := #4;
    HexW[1] := Digits[hi(W) shr 4];
    HexW[2] := Digits[hi(W) and $F];
    HexW[3] := Digits[lo(W) shr 4];
    HexW[4] := Digits[lo(W) and $F];
  end;

  function HexL(L : LongInt) : string;
    {-Return hex string for LongInt}
  begin
    with LH(L) do
      HexL := HexW(H)+HexW(L);
  end;

  function AddBackSlash(DirName : string) : string;
    {-Add a default backslash to a directory name}
  begin
    {$IFNDEF ELEUNIX}
      if DirName[Length(DirName)] in DosDelimSet then
        AddBackSlash := DirName
      else
        AddBackSlash := DirName+'\';
    {$ELSE}
      if DirName[Length(DirName)] in DosDelimSet then
        AddBackSlash := DirName
      else
        AddBackSlash := DirName+'/';
    {$ENDIF}
  end;

  function JustPathname(PathName : string) : string;
    {-Return just the drive:directory portion of a pathname}
  var
    I : Word;
  begin
    I := Succ(Word(Length(PathName)));
    repeat
      Dec(I);
    until (PathName[I] in DosDelimSet) or (I = 0);

    if I = 0 then
      {Had no drive or directory name}
      JustPathname[0] := #0
    else if I = 1 then
      {Either the root directory of default drive or invalid pathname}
      JustPathname := PathName[1]
    else if (PathName[I] = '\') then begin
      if PathName[Pred(I)] = ':' then
        {Root directory of a drive, leave trailing backslash}
        JustPathname := Copy(PathName, 1, I)
      else
        {Subdirectory, remove the trailing backslash}
        JustPathname := Copy(PathName, 1, Pred(I));
    end else
      {Either the default directory of a drive or invalid pathname}
      JustPathname := Copy(PathName, 1, I);
  end;

  function HasExtension(Name : string; var DotPos : Word) : Boolean;
    {-Return whether and position of extension separator dot in a pathname}
  var
    I : Word;
  begin
    DotPos := 0;
    for I := Length(Name) downto 1 do
      if (Name[I] = '.') and (DotPos = 0) then
        DotPos := I;
    HasExtension := (DotPos > 0) and (Pos('\', Copy(Name, Succ(DotPos), 64)) = 0);
  end;

  function DefaultExtension(Name : string; Ext : String) : string;
    {-Return a pathname with the specified extension attached}
  var
    DotPos : Word;
  begin
    if HasExtension(Name, DotPos) then
      DefaultExtension := Name
    else if Name = '' then
      DefaultExtension := ''
    else
      DefaultExtension := Name+'.'+Ext;
  end;

  function Search(var Buffer; BufLength : Word;
                  var Match; MatLength : Word) : Word;
   {-Search through Buffer for Match. BufLength is length of range to search.
     MatLength is length of string to match. Returns number of bytes searched
     to find Match, $FFFF if not found.}
  {$IFDEF MSDOS}
   begin
    inline(
      $1E/                   {PUSH DS                 ;Save DS}
      $FC/                   {CLD                     ;Go forward}
      $C4/$7E/<Buffer/       {LES  DI,[BP+<Buffer]    ;ES:DI => Buffer}
      $89/$FB/               {MOV  BX,DI              ;BX = Ofs(Buffer)}
      $8B/$4E/<BufLength/    {MOV  CX,[BP+<BufLength] ;CX = Length of range to scan}
      $8B/$56/<MatLength/    {MOV  DX,[BP+<MatLength] ;DX = Length of match string}
      $85/$D2/               {TEST DX,DX              ;Length(Match) = 0?}
      $74/$24/               {JZ   Error              ;If so, we're done}
      $C5/$76/<Match/        {LDS  SI,[BP+<Match]     ;DS:SI => Match buffer}
      $AC/                   {LODSB                   ;AL = Match[1]; DS:SI => Match[2]}
      $4A/                   {DEC  DX                 ;DX = MatLength-1}
      $29/$D1/               {SUB  CX,DX              ;CX = BufLength-(MatLength-1)}
      $76/$1B/               {JBE  Error              ;Error if BufLength is less}
                             {;Search for first character in Match}
                             {Next:}
      $F2/$AE/               {REPNE SCASB             ;Search forward for Match[1]}
      $75/$17/               {JNE  Error              ;Done if not found}
      $85/$D2/               {TEST DX,DX              ;If Length = 1 (DX = 0) ...}
      $74/$0C/               {JZ   Found              ; the "string" was found}
                             {;Search for remainder of Match}
      $51/                   {PUSH CX                 ;Save CX}
      $57/                   {PUSH DI                 ;Save DI}
      $56/                   {PUSH SI                 ;Save SI}
      $89/$D1/               {MOV  CX,DX              ;CX = Length(Match) - 1}
      $F3/$A6/               {REPE CMPSB              ;Does rest of string match?}
      $5E/                   {POP  SI                 ;Restore SI}
      $5F/                   {POP  DI                 ;Restore DI}
      $59/                   {POP  CX                 ;Restore CX}
      $75/$EC/               {JNE  Next               ;Try again if no match}
                             {;Calculate number of bytes searched and return}
                             {Found:}
      $4F/                   {DEC  DI                 ;DX = Offset where found}
      $89/$F8/               {MOV  AX,DI              ;AX = Offset where found}
      $29/$D8/               {SUB  AX,BX              ;Subtract starting offset}
      $EB/$03/               {JMP  SHORT SDone        ;Done}
                             {;Match was not found}
                             {Error:}
      $31/$C0/               {XOR  AX,AX              ;Return $FFFF}
      $48/                   {DEC  AX}
                             {SDone:}
      $1F/                   {POP  DS                 ;Restore DS}
      $89/$46/<Search);      {MOV [BP+<Search],AX     ;Set func result}
    end;
   {$ELSE}
   var Counter      : Word;
       CurInSearch  : Word;
       MaxArray     : Array[0..65534] of Char ABSOLUTE Buffer;
       SearchArray  : Array[0..65534] of Char ABSOLUTE MATCH;
  begin
    Search := $FFFF;                                            { Not found }

    CurInSearch := 00;

    For Counter := 00 to BufLength do
     begin;
      If MaxArray[Counter] <> SearchArray[CurInSearch] then
        CurInSearch := 00;

      If MaxArray[Counter] = SearchArray[CurInSearch] then
        begin
          Inc(CurInSearch);

          If CurInSearch = MatLength then
            begin
              Search := Counter - Pred(CurInSearch);
              Exit;
            end; { all characters found }

        end; { if found }

     end; { for }
  end; { func. Search }
   {$ENDIF}

  {$IFDEF HeapDebug}
  const
    ExtraSize = 10;
    MarkValue = $AA;
  type
    CharSet = set of Char;
    ByteArray = array[1..65521] of Byte;
    OS = record
      O : Word;
      S : Word;
    end;

  function HexPtr(P : Pointer) : string;
    {-Return hex string for pointer}
  begin
    HexPtr := HexW(OS(P).S)+':'+HexW(OS(P).O);                         {!!.01}
  end;

  procedure DebugExit;
    {-Exit procedure to close the debug file}
  begin
    ExitProc := SaveExit;;
    WriteLn(HDebug);
    {$IFDEF MSDOS}
	    WriteLn(HDebug, 'Ending MemAvail: ', MemAvail);
	    WriteLn(HDebug, 'Ending MaxAvail: ', MaxAvail);
	{$ENDIF}
    Close(HDebug);
  end;

  procedure LogError(P : Pointer; Size : LongInt; Bound : Word);
    {-Log memory overwrite errors}
  var
    F : Text;
  begin
    Assign(F, 'HEAPERR.RPT');
    if ExistFile('HEAPERR.RPT') then
      Append(F)
    else
      ReWrite(F);
    if IOResult = 0 then begin
      if Size = -1 then
         WriteLn(F, '  also at ', Bound)
      else
        WriteLn(F, 'Out of bounds occurred on ', HexPtr(P), ' (', Size, ') variable at ', Bound);
      if IOResult = 0 then ;
      Close(F);
      if IOResult = 0 then ;
    end;
  end;
  {$ENDIF}

  function GetMemCheck(var P; Bytes : Word) : Boolean;
    {-Allocate heap space, returning true if successful}
  var
    Pt : Pointer absolute P;
    {$IFDEF HeapDebug}
    X : Pointer;
    {$ENDIF}
  begin
    {$IFDEF HeapDebug}
    Inc(Bytes, ExtraSize);
    GetMem(Pt, Bytes);
    if (Pt <> nil) then begin
      FillChar(Pt^, Bytes, MarkValue);
      GetMemCheck := True;
    end else
      GetMemCheck := False;

    {Log the allocation}
    X := Pointer(Pointer(LongInt(@Bytes)-4)^);
    Dec(OS(X).S, PrefixSeg+$10);
    WriteLn(HDebug, 'from ', HexPtr(X),
                    '  allocate: ', HexPtr(Pointer(P)), ' ', Bytes-ExtraSize);
    {$ELSE}
    {$IFNDEF DELPHI}
      GetMem(Pt, Bytes);
      GetMemCheck := (Pt <> nil);
    {$ELSE}
      GetMem(Pointer(p), Bytes);
      GetMemCheck := (Pointer(p) <> nil);
    {$ENDIF}

    {$ENDIF}
  end;

  procedure FreeMemCheck(var P; Bytes : Word);
    {-Deallocate heap space}
  var
    Pt : Pointer absolute P;
    {$IFDEF HeapDebug}
    X : Pointer;
    Found : Boolean;
    I : Word;
    {$ENDIF}
  begin
    {$IFDEF HeapDebug}
    X := Pointer(Pointer(LongInt(@Bytes)-4)^);
    Dec(OS(X).S, PrefixSeg+$10);
    WriteLn(HDebug, 'from ', HexPtr(X),
                    '  free:     ', HexPtr(Pointer(P)), ' ', Bytes);
    if (Pt <> nil) then begin
      I := Bytes + 1;
      Found := False;
      while (I <= Bytes + ExtraSize) do begin
        if (ByteArray(Pt^)[I] <> MarkValue) then begin
          if not Found then begin
            Found := True;
            LogError(Pointer(P), Bytes, I);
          end else
            LogError(Pointer(P), -1, I);
        end;
        Inc(I);
      end;

      Inc(Bytes, ExtraSize);
      FreeMem(Pt, Bytes);
      Pt := nil;
    end;
    {$ELSE}
    if Pt <> nil then begin
      {$IFNDEF DELPHI}
        FreeMem(Pt, Bytes);
        Pt := nil;
      {$ELSE}
        FreeMem(Pointer(p), Bytes);
        Pointer(p) := nil;
      {$ENDIF}
    end;
    {$ENDIF}
  end;

  function JustFilename(PathName : string) : string;
    {-Return just the filename of a pathname}
  const
    DosDelimSet : set of Char = ['\', ':', #0];
  var
    I : Word;
  begin
    I := Succ(Word(Length(PathName)));
    repeat
      Dec(I);
    until (PathName[I] in DosDelimSet) or (I = 0);
    JustFilename := Copy(PathName, Succ(I), 64);
  end;

  function ExistFile(FName : string) : Boolean;
    {-Return true if file is found}
  {$IFDEF MSDOS}
  var
    FLen : Byte absolute FName;
    Regs : Registers;
  begin
    {check for empty string}
    if Length(FName) = 0 then
      ExistFile := False
    else with Regs do begin
      Inc(FLen);
      FName[FLen] := #0;
      AX := $4300;           {get file attribute}
      DS := Seg(FName);
      DX := Ofs(FName[1]);
      MsDos(Regs);
      ExistFile := (not Odd(Flags)) and (CX and (VolumeID+Directory) = 0);
    end;
  end;
  {$ELSE}
  var F: File;
      W: Word;
  begin
    Assign(F, FName);
    {$IFNDEF DELPHI}
      GetFAttr(F, W);
      ExistFile := (DosError=0);
    {$ELSE}
      ExistFile := FileGetAttr(FName) <> -1;
    {$ENDIF}
  end;
  {$ENDIF}

  function StringToHeap(S : string) : StringPtr;
    {-Allocate space for s and return pointer}
  var
    L : Word;
    P : Pointer;
  begin
    L := Succ(Word(Length(S)));
    {$IFDEF MSDOS}
    if MaxAvail < L then
      StringToHeap := nil
    else {$ENDIF}begin
      GetMem(P, L);
      Move(S, P^, L);
      StringToHeap := P;
    end;
  end;

  procedure DisposeString(P : StringPtr);
    {-Deallocate space for string at p}
  begin
    if P <> nil then
      FreeMem(P, Succ(Word(Byte(P^[0]))));
  end;

  function HeapFunc(Size : Word) : Integer; far;
    {-Return nil pointer if insufficient memory}
  begin
    {$IFDEF Heap6}
    if Size = 0 then
      HeapFunc := 2
    else
    {$ENDIF}
    HeapFunc := 1;
  end;
  {$ENDIF}

  function CheckRange(Value, Low, High : Word) : Boolean;
    {-Range check Value}
  begin
    if (Value < Low) or (Value > High) then begin
      CheckRange := False;
    end else
      CheckRange := True;
  end;

  function GetAsyncStatus : Word;
  begin
    GetAsyncStatus := AsyncStatus;
  end;

  procedure SetAsyncStatus(Status : Word);
  begin
    AsyncStatus := Status;
  end;

  function GetArchiveStatus : Word;
  begin
    GetArchiveStatus := ArchiveStatus;
  end;

  procedure SetArchiveStatus(Status : Word);
  begin
    ArchiveStatus := Status;
  end;

  {!!.03 new}
  function GetAproFileMode : Byte;
  begin
    GetAproFileMode := AproFileMode;
  end;

  {!!.03 new}
  procedure SetAproFileMode(NewMode : Byte);
  begin
    AproFileMode := NewMode;
  end;

begin
  {$IFDEF Standalone}
  {$IFNDEF DELPHI}
   {$IFDEF VER70}
   {$IFNDEF FPC}
   {$IFNDEF VirtualPascal}
    HeapError := @HeapFunc;
   {$ENDIF}
   {$ENDIF}
   {$ENDIF}
  {$ENDIF}
  {$ENDIF}

  {$IFDEF HeapDebug}
  {Delete old heap error report}
  Assign(HDebug, 'HEAPERR.RPT');
  Erase(HDebug);
  if IOResult <> 0 then ;

  {$IFDEF MSDOS}
  {Open new heap allocation report file}
  Assign(HDebug, 'HEAP.RPT');
  Rewrite(HDebug);
  WriteLn(HDebug, 'Initial MemAvail: ', MemAvail);
  WriteLn(HDebug, 'Initial MaxAvail: ', MaxAvail);
  WriteLn(HDebug);
  if IOResult <> 0 then ;
  {$ENDIF}

  SaveExit := ExitProc;
  ExitProc := @DebugExit;
  {$ENDIF}

  {$IFNDEF DELPHI}
    {$IFNDEF UsePModeDLL}
    Crc32TableOfs := Ofs(Crc32Table);
    {$ENDIF}
  {$ENDIF}
end.

  {This acknowledgment is for the table/code used in the UpdateCrc routine}
  (*
   * updcrc derived from article Copyright (C) 1986 Stephen Satchell.
   *  NOTE: First argument must be in range 0 to 255.
   *        Second argument is referenced twice.
   *
   * Programmers may incorporate any or all code into their programs,
   * giving proper credit within the source. Publication of the
   * source routines is permitted so long as proper credit is given
   * to Stephen Satchell, Satchell Evaluations and Chuck Forsberg,
   * Omen Technology.
   *)



