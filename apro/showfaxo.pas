{$S-,R-,V-,I-,B-,F+,A-}

{$I APDEFINE.INC}

{.$DEFINE TestStreams}

{.$DEFINE ShowLocation}
{.$DEFINE ShowBadCodes}

{******************************************************}
{*                 SHOWFAXO.PAS  2.03                 *}
{*      Copyright (c) TurboPower Software 1993.       *}
{*                All rights reserved.                *}
{******************************************************}

program ShowFaxO;

uses
  {$IFDEF LeakCheck}                                                   {!!.01}
  LeakChek,                                                            {!!.01}
  {$ENDIF}                                                             {!!.01}
  Dos,
  {$IFDEF UseOPro}
  OpRoot,
  OpCrt,
  OpString,
  {$ENDIF}
  {$IFDEF UseTPro}
  TpMemChk,
  TpCrt,
  TpString,
  {$ENDIF}
  {$IFDEF Standalone}
  Crt,
  {$ENDIF}
  ApMisc,
  OoFaxCvt;

const
  CopyrightLine = 'SHOWFAXO 2.03 -- displays APRO fax files  (c)1993 TurboPower Software';

    Up = #$48;
  Down = #$50;
  Left = #$4B;
 Right = #$4D;
  Home = #$47;
EndKey = #$4F;
  PgUp = #$49;
  PgDn = #$51;
   Esc = #$1B;

{video stuff}
const
  MaxLines = 4000;
  CurPage  : Word = 1;

  {$IFDEF VER60}
  SegA000: Word = $A000;
  {$ENDIF}

type
  PLine = ^TLine;
  TLine = Array[0..$FFF0] of Byte;

  PNode = ^TNode;
  TNode =
    record
      Data  : PLine;
      Len   : Word;
    end;

  PList = ^TList;
  TList =
    record
      Lines   : Array[1..MaxLines] of PNode;
      LineCnt : Word;
    end;

const
  SelMode  : Byte = $12;
  SelWid   = 640;
  WidthBytes = SelWid shr 3;
  SelHit   = 480;
  DoubleIt : Boolean = False;
  RemoveIt : Boolean = False;

var
  RasterList : TList;
  Unpacker : UnpackFaxPtr;
  FaxFile : String;
  TmpBuffer : PLine;
  FaxHeader : FaxHeaderRec;
  PageHeader : PageHeaderRec;
  C : Char;
  DT : DateTime;

label                                                                  {!!.01}
  Cleanup;                                                             {!!.01}

{!!.01 begin}
type
  RasterByteArray = array[0..639] of Byte;
const
  ScaleIt  : Boolean = False;
  ScaleFact: Word = 3;  {3x reduces 1728 pixel fax to 576 screen pixels}
  ScaleThre: Word = 1;  {Min pixels in input region to show pixel in output}
var
  RasterCounts : RasterByteArray;
{!!.01 end}

  procedure Abort(Msg : String);
  begin
    WriteLn(Msg);
    Halt(1);
  end;

  procedure PackBuffer(var Buffer; var Len : Word);
  type
    PW = ^Word;
  var
    TL : TLine absolute Buffer;
    W,X : Word;
    P : PW;
  begin
    X := Len shr 1;
    Len := X;
    asm
      mov    cx,X
      jcxz   @@3
      push   ds
      les    di,Buffer
      lds    si,Buffer
@@1:
      push   cx
      mov    ax,word ptr ds:[si]
      xchg   al,ah
      xor    bl,bl
      mov    cx,0008h
@@2:
      shl    ax,1
      shl    ax,1
      rcl    bl,1
      loop   @@2
      mov    es:[di],bl
      inc    di
      inc    si
      inc    si
      pop    cx
      loop   @@1
      pop    ds
@@3:
    end;
  end;

  function InitNode(var T : TNode; var Buffer; L : Word) : Boolean;
  var
    I : Word;
  begin
    InitNode := False;

    T.Data := nil;
    if (L = 0) then begin
      InitNode := True;
      T.Len := 0;
      Exit;
    end;

    if not GetMemCheck(T.Data, L) then
      Exit;

    InitNode := True;
    Move(Buffer, T.Data^, L);
    T.Len := L;
    asm
      les    di,T
      les    di,TNode(es:[di]).Data
      mov    cx,L
@@1:  not    byte ptr es:[di]
      inc    di
      loop   @@1
    end;
  end;

  procedure DoneNode(var P : PNode);
  begin
    With P^ do
      FreeMemCheck(Data, Len);
    Dispose(P);
  end;

  procedure ClearRasterList;
  var
    W : Word;
  begin
    with RasterList do begin
      W := LineCnt;
      while W > 0 do begin
        DoneNode(Lines[w]);
        Dec(W);
      end;
    end;
    FillChar(RasterList, SizeOf(RasterList), 0);
  end;

  {!!.01 begin}
  procedure ClearRasterCounts;
  begin
    FillChar(RasterCounts, SizeOf(RasterCounts), 0);
  end;

  procedure UpdateRasterCounts(Buffer : PByteBuffer;
                               Len : Word;
                               var RasterCounts : RasterByteArray);
  var
    BPos : Word;
    BLen : Word;
  begin
    BLen := (Len shl 3)-1;
    if BLen div ScaleFact > 639 then
      BLen := 639*ScaleFact;
    for BPos := 0 to BLen do
      if Buffer^[BPos shr 3] and ($80 shr (BPos and 7)) <> 0 then
        inc(RasterCounts[BPos div ScaleFact]);
  end;

  procedure PackRasterCounts(Buffer : PByteBuffer;
                             var RasterCounts : RasterByteArray;
                             var Len : Word);
  var
    BPos : Word;
    BytePos : Word;
    Threshold : Word;
    B : Byte;
  begin
    FillChar(Buffer^, Len, 0);
    Threshold := (Ord(DoubleIt)+1)*ScaleThre;
    for BPos := 0 to 639 do
      if RasterCounts[BPos] >= Threshold then begin
        BytePos := BPos shr 3;
        Buffer^[BytePos] := Buffer^[BytePos] or ($80 shr (BPos and 7));
      end;
    Len := 640 shr 3;
  end;
  {!!.01 end}

  function UnpLine(UP : UnpackFaxPtr; Buffer : PByteBuffer;
                   Len : Word; PH : PageHeaderRec) : Boolean;
  const
    B : Boolean = True;
  var
    P : PNode;
  begin
    UnpLine := False;
    AsyncStatus := ecOK;

    if ScaleIt then                                                    {!!.01}
      UpdateRasterCounts(Buffer, Len, RasterCounts);                   {!!.01}

    if DoubleIt then begin
      B := not(B);
      if B then
        exit;
    end;

    with RasterList do begin
      New(P);
      if P <> nil then begin
        FillChar(P^, SizeOf(TNode), 0);
        if ScaleIt then begin                                          {!!.01}
          PackRasterCounts(Buffer, RasterCounts, Len);                 {!!.01}
          ClearRasterCounts;                                           {!!.01}
        end else                                                       {!!.01}
          PackBuffer(Buffer^, Len);
        if InitNode(P^, Buffer^, Len) then begin
          Inc(LineCnt);
          if LineCnt > MaxLines then
            AsyncStatus := ecOutOfMemory
          else begin
            Lines[LineCnt] := P;
            Write(^H^H^H^H^H, LineCnt:5);
          end
        end else
          AsyncStatus := epFatal+ecOutOfMemory;
      end else
        AsyncStatus := epFatal+ecOutOfMemory;
    end;
  end;

  procedure Mode3;
  var
    R: Registers;
  begin
    R.AX := $0003;
    Intr($10, R);
  end;

  procedure ClrMode12;
  begin
    Fillchar(Ptr(SegA000, 0)^, $FFFF, $FF);
  end;

  procedure Mode12;
  var
    R : Registers;
  begin
    R.AX := SelMode;
    Intr($10, R);
    R.AX := $0F00;
    Intr($10, R);
    if R.AL <> SelMode then begin
      Mode3;
      WriteLn('Monitor doesn''t support graphics mode 12');
      Halt;
    end;
  end;

  procedure BltLine(var Buffer; Len, X, Y : Word); assembler;
  asm
    { BX will contains WidthBytes throughout }
    mov   bx,WidthBytes
    xor   dx,dx
    cld

    { point ES:DI to the proper line on the screen }
    mov   ax,SegA000
    mov   es,ax
    mov   ax,Y
    mul   bx
    mov   di,ax

    { point DS:SI to the buffer }
    push  ds
    lds   si,Buffer

    { now, figure out the actual number of bytes to be transfered from Buffer }
    mov   ax,X
    mov   cx,Len

    cmp   ax,cx               { is the x offset greater than the length? }
    ja    @3                  { if so, fill the line with zeros }
    add   si,ax
    sub   cx,ax               { CX gets WidthBytes or (Len - X), }
    cmp   cx,bx               {   whichever is smaller }
    jb    @1
    mov   cx,bx

@1:
    mov   dx,cx               { save this length count for later }
    shr   cx,1                { divide length by two for move-by-word }
    jnc   @2
    movsb                     { move the odd byte }

@2:
    jcxz  @3
    rep   movsw               { move the rest of the data }

    { by the time the code gets to @3, dx should contain the number of }
    { bytes that have been written to the screen }

@3:
    sub   bx,dx               { get the number of "padding" bytes }
    mov   cx,bx
    jcxz  @9                  { if no padding, then bail }

    mov   ax,$FFFF
    shr   cx,1                { divide cx by two for move-by-word }
    jnc   @4
    stosb                     { store the odd byte }

@4:
    jcxz  @9
    rep   stosw

@9:
    pop   ds
  end;

  procedure WriteLoc(S : String);
  var
    R : Registers;
    X : Byte;
  begin
    FillChar(R, SizeOf(R), 0);
    R.ah := 2;
    R.dx := $0000;
    Intr($10, R);
    for X := 1 to Length(S) do begin
      R.ah := $0E;
      R.al := Byte(S[x]);
      R.bx := $00FF;
      Intr($10, R);
    end;
  end;

  {!!.03 - Added}
  procedure ReverseBits(var Dest, Src; L : Word); assembler;
  asm
    push  ds
    lds   si,Src          {DS:SI->Src}
    les   di,Dest         {ES:DI->Dest}
    mov   cx,L            {Get length of row in CX}
    add   di,cx           {point ES:DI to end of destination}
    dec   di
    dec   di
    shr   cx,1            {count words, not bytes}

@1: cld                   {go forward}
    lodsw                 {get next input word}
    xchg  ah,al           {put bits in proper order}

@2:
    {put reverse of AL in AH}
    shr   ax,1
    rcl   bx,1
    shr   ax,1
    rcl   bx,1
    shr   ax,1
    rcl   bx,1
    shr   ax,1
    rcl   bx,1
    shr   ax,1
    rcl   bx,1
    shr   ax,1
    rcl   bx,1
    shr   ax,1
    rcl   bx,1
    shr   ax,1
    rcl   bx,1
    shr   ax,1
    rcl   bx,1
    shr   ax,1
    rcl   bx,1
    shr   ax,1
    rcl   bx,1
    shr   ax,1
    rcl   bx,1
    shr   ax,1
    rcl   bx,1
    shr   ax,1
    rcl   bx,1
    shr   ax,1
    rcl   bx,1
    shr   ax,1
    rcl   bx,1

    mov   ax,bx
    xchg  ah,al
    std
    stosw
    loop  @1

    pop   ds
  end;

  procedure ShowTheFax;
  var
    Y : Word;
    X : Word;
    C : Char;
    W : Word;
    Finished : Boolean;

    function Min(A, B : Word) : Word;
    begin
      if A < B then Min := A else Min := B;
    end;

    procedure UpdScrn;
    var
      N : Word;
      Z : Word;
      P : PNode;
      B : Byte;
      S : String;
    begin
      if Y > RasterList.LineCnt then
        Exit;
      B := $FF;
      N := 0;
      Z := Y;
      P := RasterList.Lines[z];
      if P = nil then
        Exit;

      for N := 1 to SelHit do begin
        if P <> nil then
          BltLine(P^.Data^, P^.Len, X, N)
        else
          BltLine(B, 1, X, N);
        if P <> nil then begin
          Inc(Z);
          if Z > RasterList.LineCnt then
            P := nil
          else
            P := RasterList.Lines[z]
        end;
      end;

      {$IFDEF ShowLocation}
      S := Pad(Long2Str(X), 4)+Pad(Long2Str(Y), 4)+' '+Long2Str(RasterList.LineCnt);
      WriteLoc(S);
      {$ENDIF}
    end;

    {!!.03 - added}
    procedure RotateImage;
    var
      I  : Word;
      H  : Word;
      B1 : array[1..216] of Byte;
      B2 : array[1..216] of Byte;

    begin
      H := RasterList.LineCnt div 2;
      for I := 1 to H do begin
        ReverseBits(B1, RasterList.Lines[I]^.Data^, RasterList.Lines[I]^.Len);
        ReverseBits(B2, RasterList.Lines[RasterList.LineCnt - I + 1]^.Data^,
          RasterList.Lines[RasterList.LineCnt - I + 1]^.Len);
        Move(B2, RasterList.Lines[I]^.Data^, RasterList.Lines[I]^.Len);
        Move(B1, RasterList.Lines[RasterList.LineCnt - I + 1]^.Data^,
          RasterList.Lines[RasterList.LineCnt - I + 1]^.Len);
      end;

      if ((RasterList.LineCnt mod 2) <> 0) then begin
        Inc(H);
        ReverseBits(B1, RasterList.Lines[H]^.Data^, RasterList.Lines[H]^.Len);
        Move(B1, RasterList.Lines[H]^.Data^, RasterList.Lines[H]^.Len);
      end;
    end;

  begin
    ClrScr;
    Mode12;
    ClrMode12;
    Y := 1;
    X := 0;

    while True do begin
      UpdScrn;

      if RemoveIt then begin
        Delay(2000);
        Mode3;
        Exit;
      end;

      Finished := False;
      repeat
        C := ReadKey;
        if C = #0 then begin
          C := ReadKey;
          Finished := True;
        end else begin
          C := Upcase(C);
          Finished := C in ['N', 'B', 'F', 'L', 'R', Esc]; {!!.03}
        end;
      until Finished;

      case C of
        Esc:
          begin
            Mode3;
            Exit;
          end;

        Up:
          if Y > 1 then
            Dec(Y);

        Down:
          if Y < RasterList.LineCnt then
            Inc(Y);

        Left:
          if X > 0 then
            Dec(X, 8);

        Right:
          if X < 255 then
            Inc(X, 8);

        Home:
          begin
            X := 0;
            Y := 1;
          end;

        EndKey:
          begin
            Y := RasterList.LineCnt;
            W := 455;
            while W > 0 do begin
              if Y > 1 then
                Dec(Y);
              if Y = 1 then
                W := 0
              else
                Dec(W);
            end;
            X := 0;
          end;

        PgUp:
          begin
            W := 72;
            while W > 0 do begin
              if Y > 1 then
                Dec(Y);
              Dec(W);
            end;
          end;

        PgDn:
          begin
            W := 72;
            while W > 0 do begin
              if Y < RasterList.LineCnt then
                Inc(Y);
              Dec(W);
            end;
          end;

        'N':
          if CurPage < Unpacker^.FaxHeader.PageCount then begin
            Inc(CurPage);
            ClearRasterList;
            Unpacker^.UnpackPage(FaxFile, CurPage);
            if AsyncStatus <> 0 then begin
              Mode3;
              WriteLn('Error ', AsyncStatus);
              Exit;
            end;
            X := 0;  Y := 1;
          end else begin
            Sound(440);
            Delay(100);
            NoSound;
          end;

        'L' :
          begin
            CurPage := Unpacker^.FaxHeader.PageCount;
            ClearRasterList;
            Unpacker^.UnpackPage(FaxFile, CurPage);
            if AsyncStatus <> 0 then begin
              Mode3;
              WriteLn('Error ', AsyncStatus);
              Exit;
            end;
            X := 0;  Y := 1;
          end;

        'B':
          if CurPage > 1 then begin
            Dec(CurPage);
            ClearRasterList;
            Unpacker^.UnpackPage(FaxFile, CurPage);
            if AsyncStatus <> 0 then begin
              Mode3;
              WriteLn('Error ', AsyncStatus);
              Exit;
            end;
            X := 0;  Y := 1;
          end else begin
            Sound(440);
            Delay(100);
            NoSound;
          end;

        'F' :
          begin
            CurPage := 1;
            ClearRasterList;
            Unpacker^.UnpackPage(FaxFile, CurPage);
            if AsyncStatus <> 0 then begin
              Mode3;
              WriteLn('Error ', AsyncStatus);
              Exit;
            end;
            X := 0;  Y := 1;
          end;

        'R' :             {!!.03}
          begin           {!!.03}
            RotateImage;  {!!.03}
            X := 0;       {!!.03}
            Y := 1;       {!!.03}
          end;            {!!.03}
      end;
    end;
  end;

  procedure ShowHelp;
  begin
    WriteLn('Usage: SHOWFAXO [options] FaxFile');
    WriteLn('  /D        Reduce image height (50% reduction)');        {!!.01}
    WriteLn('  /P Page   Starting page number');
    WriteLn('  /R        Remove display after 2 seconds');
    WriteLn('  /S        Scale image width to fit on screen');         {!!.01}
    WriteLn('  /?        Display help screen');
    Halt(1);
  end;

var
  Code : Word;

  procedure ParseCmdLine;
  var
    S : String;
    I : Integer;
  begin
    FaxFile := '';
    if ParamCount = 0 then
      ShowHelp;
    I := 1;
    repeat
      S := ParamStr(I);
      if S[1] in ['/','-'] then begin
        case Upcase(S[2]) of
          'D' : DoubleIt := True;
          'P' : begin
                  Inc(I);
                  S := ParamStr(I);
                  Val(S, CurPage, Code);
                  if Code <> 0 then
                    CurPage := 1;
                end;
          'R' : RemoveIt := True;
          'S' : ScaleIt := True;                                       {!!.01}
          else  ShowHelp;
        end;
      end else
        FaxFile := DefaultExtension(StUpcase(S), 'APF');
      Inc(I);
    until I > ParamCount;
    if FaxFile = '' then
      ShowHelp;
  end;

function ZP(W : Word) : String;
  {-Zero-pad a value to a two byte string}
var
  S : string;
begin
  Str(W, S);
  if Length(S) = 1 then begin
    S[2] := S[1];
    S[1] := '0';
    S[0] := #2;
  end;
  ZP := S;
end;

function GetFileSize(FName : PathStr) : LongInt;
var
  F : File;
  SaveMode : Byte;                                                     {!!.02}
begin
  SaveMode := FileMode;                                                {!!.02}
  FileMode := $40;                                                     {!!.02}
  Assign(F, FName);
  Reset(F, 1);
  FileMode := SaveMode;                                                {!!.02}
  GetFileSize := FileSize(F);
  Close(F);
  if IoResult <> 0 then
    GetFileSize := 0;
end;

{$IFDEF TestStreams}
procedure CreateStream;
  {-Store Unpacker to stream}
var
  S : BufIdStream;
  Status : Word;
begin
  WriteLn('Creating stream file');

  {Create a new stream}
  if not S.Init('UNPACK.STM', SCreate, 1024) then
    Abort('Failed to make stream ');

  {Register all hierarchies}
  S.RegisterHier(UnpackFaxStream);
  Status := S.GetStatus;
  if Status <> 0 then
    Abort('Error registering object ');

  {Register our user procedures}
  S.RegisterPointer(1000, @UnpLine);

  {Store the converter}
  S.PutPtr(Unpacker);
  Status := S.GetStatus;
  if Status <> 0 then
    Abort('Error storing');

  {Clean up}
  S.Done;
end;

procedure LoadStream;
  {-Load Unpacker from a stream}
var
  S : BufIdStream;
  Status : Word;
begin
  WriteLn('Loading stream file');

  {Re-open existing new stream}
  if not S.Init('UNPACK.STM', SOpen, 1024) then
    Abort('Failed to open stream');

  {Register all hierarchies}
  S.RegisterHier(UnpackFaxStream);
  Status := S.GetStatus;
  if Status <> 0 then
    Abort('Error registering object');

  {Register our user procedures}
  S.RegisterPointer(1000, @UnpLine);

  {Load the converter}
  RootPtr(Unpacker) := S.GetPtr;
  Status := S.GetStatus;
  if Status <> 0 then
    Abort('Error loading converter');

  {Clean up}
  S.Done;
end;
{$ENDIF}

begin
  WriteLn(CopyrightLine);

  ParseCmdLine;

  FillChar(RasterList, SizeOf(RasterList), 0);

  New(Unpacker, Init);
  if Unpacker = nil then begin
    WriteLn('Error initializing unpacker');
    Halt(1);
  end;
  Unpacker^.SetOutputLineFunc(UnpLine);
  ClearRasterCounts;                                                   {!!.01}

  {$IFDEF TestStreams}
  CreateStream;
  Dispose(Unpacker, Done);
  LoadStream;
  {$ENDIF}

  WriteLn;
  WriteLn('Unpacking fax file ', FaxFile);
  Unpacker^.GetFaxHeader(FaxFile, FaxHeader);
  if AsyncStatus = ecOk then begin
    with FaxHeader do begin
      UnpackTime(FDateTime, DT);
      with DT do
        WriteLn('  File date/time: ', ZP(Month),'/',ZP(Day),'/',ZP(Year), '  ',
                                      ZP(Hour),':',ZP(Min));
      WriteLn('  File size:      ', GetFileSize(FaxFile));
      WriteLn('  Sender ID:      ', SenderID);
      WriteLn('  Pages:          ', PageCount);
    end;
  end else begin
    case AsyncStatus mod 10000 of
      2,3  : WriteLn(^M^J'File not found');
      9970 : WriteLn(^M^J'Not a valid APF file');
      else   WriteLn(^M^J'Error searching for header: ', AsyncStatus);
    end;

    goto Cleanup;
  end;

  if not RemoveIt then begin
    WriteLn;
    WriteLn('Press <Q> to abort, any other to display fax image');
    C := Upcase(ReadKey);
    if C = 'Q' then
      goto Cleanup;
  end else
    Delay(2000);

  Unpacker^.GetPageHeader(FaxFile, CurPage, PageHeader);
  if AsyncStatus = ecOk then begin
    if not DoubleIt then
      DoubleIt := PageHeader.ImgFlags and ffHighRes = ffHighRes;
  end else begin
    case AsyncStatus mod 10000 of
      9970 : WriteLn(^M^J'Not a valid APF file');
      else   WriteLn(^M^J'Error upacking fax ', AsyncStatus,', lines = ', RasterList.LineCnt);
    end;
    goto Cleanup;
  end;

  Unpacker^.UnpackPage(FaxFile, CurPage);
  if AsyncStatus <> 0 then begin
    WriteLn(^M^J'Error upacking fax ', AsyncStatus,', lines = ', RasterList.LineCnt);
    goto Cleanup;
  end;

  {$IFDEF ShowBadCodes}
  WriteLn;
  WriteLn;
  WriteLn('Bad codes found: ', Unpacker^.BadCodes);
  ReadLn;
  {$ENDIF}

  ShowTheFax;

Cleanup:                                                               {!!.01}
  Dispose(Unpacker, Done);
  ClearRasterList;                                                     {!!.01}
end.
