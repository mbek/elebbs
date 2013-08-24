{$A+,F+,I-,R-,S-,V-}

{$I APDEFINE.INC}

{Include OPro's define file if UseOPro is specified}
{$IFDEF UseOPro}
{$I OPDEFINE.INC}
{$ENDIF}

{******************************************************}
{*                   OOFAXCAS.PAS 2.03                *}
{*      Copyright (c) TurboPower Software 1993.       *}
{*                All rights reserved.                *}
{******************************************************}

unit OoFaxCas;
  {-CAS fax send/receive objects}

interface

uses
  {$IFDEF DPMI}
  WinAPI,
  {$ENDIF}
  Dos,
  Dpmi,
  {$IFDEF UseOPro}
  OpInline,
  OpString,
  OpDos,
  OpRoot,
  {$IFDEF Opro12}
  OpConst,
  {$ENDIF}
  {$ENDIF}
  {$IFDEF UseTPro}
  TpInline,
  TpMemChk,
  TpString,
  TpDos,
  {$ENDIF}
  ApMisc,
  ApTimer,
  ApPort,
  OoCom,
  OoAbsFax,
  OoFaxCvt;

const
  {Maximum size of cover data, data beyond this is discarded}
  MaxCoverData = 2048;

  {Lines/page value to use when calculating page count}                {!!.01}
  LinesPerPage : Word = 73;                                            {!!.01}

  {Undocumented constants}
  TransmitWait : Word = 182;     {Ticks to wait to assume all xmits complete}
  ReceiveWait : Word = 546;      {Ticks to wait between receive file checks}

type
  {Types of CAS queues}
  QueueType = (qTask, qReceive, qLog);

  {.Z+}
  CoverData = array[0..MaxCoverData-1] of Char;

  {$IFDEF DPMI}
  DosMemRec = record
    Sele, Segm : Word;
  end;
  {$ENDIF}
  {.Z-}

type
  {CAS FTR record, except cover data}
  FileTransferRecord = record
    FileType      : Byte;
    TextSize      : Byte;
    Status        : Byte;
    BytesSent     : LongInt;
    SizeTotal     : LongInt;
    PagesSent     : Word;
    PagesTotal    : Word;
    Path          : Array[0..79] of Char;
    Increments    : Byte;
    PageLen       : Byte;
    Reserved      : Array[0..30] of Byte;
  end;

  {CAS CFR record}
  ControlFileRecordPtr = ^ControlFileRecord;
  ControlFileRecord = record
    EventType     : Byte;
    XferType      : Byte;
    Status        : Integer;
    SendTime      : Word;
    SendDate      : Word;
    FilesToXfer   : Word;
    OfsToFTR      : Word;
    PhoneNum      : Array[0..46] of Char;
    UserTag       : Array[0..63] of Char;
    Reserved      : Byte;
    ConnectSec    : Byte;
    ConnectMin    : Byte;
    ConnectHr     : Byte;
    TotalPages    : LongInt;
    PagesSent     : LongInt;
    FilesSent     : Word;
    CoverPage     : Byte;
    ErrorCount    : Word;
    DeleteFiles   : Byte;
    EventHandle   : Word;
    Reserved2     : Array[0..52] of Byte;
    Internal      : Array[0..19] of Byte;
    CoverRead     : Byte;
    SuppHead      : Byte;
    RemoteCSID    : Array[0..20] of Char;
    DestName      : Array[0..31] of Char;
    SenderName    : Array[0..31] of Char;
    Logo          : Array[0..79] of Char;
  end;

  {CAS record type for submit single file function}
  SubmitFileType = record
    EventType     : Byte;
    XferType      : Byte;
    SendTime      : Word;
    SendDate      : Word;
    DestName      : array[0..31] of Char;
    FaxName       : array[0..79] of Char;
    PNumber       : array[0..46] of Char;
    AppTag        : array[0..63] of Char;
    Zero1         : Byte;
    CoverYesNo    : Byte;
    Zero2         : array[0..22] of Char;
    CoverBuffer   : CoverData;
  end;

  {CAS EDB record}
  ExternalDataBlock= record
    Major         : Byte;
    Minor         : Byte;
    Path          : Array[0..67] of Char;
    PB            : Array[0..12] of Char;
    Logo          : Array[0..12] of Char;
    Sender        : Array[0..31] of Char;
    CSID          : Array[0..20] of Char;
    Reserved      : Array[0..106] of Byte;
  end;

  {CAS status record, used only by GetEventStatus}
  StatusRecord = record
    CFRec         : ControlFileRecord;
    FTRec         : FileTransferRecord;
    Fill          : Byte;
  end;

  {Array for vendor specific hardware status}
  StatusArray = array[0..127] of Char;

type
  {The standard CAS send/receive object}
  CASFaxPtr = ^CASFax;
  CASFax =
    object(AbstractFax)
      FirstReceive   : Boolean;
      InProgress     : Boolean;
      CurPages       : Word;
      CurPage        : Word;
      Resolution     : Word;
      StatusWait     : Word;
      CurBytes       : LongInt;
      CASAbort       : AbortFunc;
      Regs           : Registers;
      State          : CASStates;
      SubmitFile     : SubmitFileType;
      LogoFile       : PathStr;
      ReplyTimer     : EventTimer;

      {Constructor/destructors}
      constructor Init(ID : Str20);
      destructor Done; virtual;

      {CAS primitives}
      procedure SubmitTask(FName : PathStr; var Handle : Word);
        {-Submit a CAS task}
      procedure AbortCurrentEvent(var Handle : Word);
        {-Aborts the current CAS event, may take up to 30 seconds}
      procedure FindFirstCAS(Q : QueueType; Direction : Boolean;
                             Status : Integer; var Handle : Word);
        {-Find the first event in the specified queue}
      procedure FindNextCAS(Q : QueueType; var Handle : Word);
        {-Find the next event in the specified queue}
      procedure OpenFile(Q : QueueType; FileNumber : Word;
                         Handle : Word; var FHandle : Word);
        {-Open a queue file}
      procedure CloseFile(Handle : Word);
        {-Close a previously-opened file handle}
      procedure DeleteFile(Handle : Word; FileNumber : Word; Q : QueueType);
        {-Delete the file associated with Hdl from queue Q}
      procedure DeleteAllFiles(Q : QueueType);
        {-Delete all files in queue Q}
      procedure GetEventDate(Handle : Word; Q : QueueType;
                                   var Year, Month, Day : Word);
        {-Get the date info for event Handle in queue Q}
      procedure SetTaskDate(Handle : Word; Year, Month, Day : Word);
        {-Set the date info for event Handle in queue Q}
      procedure GetEventTime(Handle : Word; Q : QueueType;
                                   var Hour, Min, Sec : Word);
        {-Get the time info for event Handle in queue Q}
      procedure SetTaskTime(Handle : Word; Hour, Min, Sec : Word);
        {-Set the time info for event Handle in queue Q}
      procedure GetExternalDataBlock(var EDB : ExternalDataBlock);
        {-Return data in external data block, function 0E}
      procedure GetSetAutoReceive(var Rings : Word; GetSet : Word);
        {-Enable/disable autoreceive, function 0F}
      procedure GetEventStatus(var SRec : StatusRecord);
        {-Return status of current event, function 10}
      procedure GetQueueStatus(Q : QueueType;
                               var Changes, ControlFiles, ReceivedFiles : Word);
        {-Get status of a queue, function 11}
      procedure GetHardwareStatus(var SArray : StatusArray);
        {-Return vendor-specific hardware status info, function 12}
      procedure RunDiagnostics(Mode : Word);
        {-Run hardware diagnostics, function 13}
      procedure MoveReceivedFile(Handle : Word;
                                 FileNumber : Word;
                                 NewName : PathStr);
        {-Move a received file to NewName (path and filename), function 14}

      procedure SubmitSingleFile(TransType : Word;
                                 TextSize : Word;
                                 Time : Word;
                                 Date : Word;
                                 Dest : String;
                                 FName : PathStr;
                                 Number : String;
                                 Cover : PathStr;
                                 var Handle : Word);
        {-Submit a single file to the CAS manager}

      {User control}
      procedure SetLogoFile(LF : PathStr);
        {-Set PCX logo file}
      procedure GetPageInfo(var Pages : Word;
                            var Page : Word;
                            var BytesTransferred : LongInt;
                            var PageLength : LongInt); virtual;
      procedure SetCASAbortFunc(CAF : AbortFunc);
        {-Set a CAS abort func}
      procedure GetAllStatus(var Event : Byte;
                             var AStatus : Word;
                             var Pages : Word;
                             var PageTotal : Word;
                             var Bytes : LongInt;
                             var FName : PathStr;
                             var Remote : String);
        {Make status call and return these fields}
      procedure SetCASResolution(High : Boolean);
        {-Pass true to set high resolution}
      procedure PrepareFaxTransmitPart;
        {-Prepare to call FaxTransmitPart}
      function FaxTransmitPart : FaxStateType;
        {-Do one part of CAS send process}
      procedure FaxTransmit; virtual;
        {-Call FaxTrasmitPart until fax is sent}
      procedure PrepareFaxReceivePart;
        {-Prepare to call FaxReceivePart}
      function FaxReceivePart : FaxStateType;
        {-Do one part of CAS receive process}
      procedure FaxReceive; virtual;
        {-Call FaxReceivePart until fax is sent}

      {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
      procedure Store(var S : IdStream);
      {$ENDIF}

      {.Z+}
      {Private}
      function csHandleAbort : Boolean; virtual;
      {.Z-}
    end;

  function CASInstalled : Boolean;
    {-Return True if CAS manager TSR is installed}

var
  {Undocumented error variables}
  LastCasFunc   : Word;
  LastCasReturn : Word;

  {$IFDEF UseStreams}
  procedure CasFaxStream(SPtr : IdStreamPtr);
    {-Register all types needed for cas fax converter}
  {$ENDIF}

implementation

type
  TCharArray = array[0..255] of Char;

const
  {CAS function codes}
  cfIsInstalled               = $00;
  cfSubmitTask                = $01;
  cfAbortEvent                = $02;
  cfFindFirst                 = $05;
  cfFindNext                  = $06;
  cfOpenFile                  = $07;
  cfDeleteFile                = $08;
  cfDeleteAll                 = $09;
  cfGetEventDate              = $0A;
  cfSetTaskDate               = $0B;
  cfGetEventTime              = $0C;
  cfSetTaskTime               = $0D;
  cfGetEDB                    = $0E;
  cfGetSetAutoReceive         = $0F;
  cfGetEventStatus            = $10;
  cfGetQueueStatus            = $11;
  cfGetHardwareStatus         = $12;
  cfRunDiagnostics            = $13;
  cfMoveReceivedFile          = $14;
  cfSubmitFile                = $15;

  dosCloseFile                = $3E;

  function StrLen(var A) : Byte;
    {-Return length of pchar S}
  var
    N : Byte;
    S : TCharArray absolute A;
  begin
    N := 0;
    while (N < 255) and (S[N+1] <> #0) do
      Inc(N);
    StrLen := N;
  end;

  procedure Prep(var R : Registers; CASFunc : Byte);
    {-Prepare to call CAS manager}
  begin
    FillChar(R, SizeOf(R), 0);
    R.AL := CASFunc;
  end;

  procedure CASCall(var R : Registers);
    {-Call CAS manager}
  {$IFDEF DPMI}
  var
    DRegs : DPMIRegisters;
  {$ENDIF}
  begin
    R.AH := $CB;
    LastCasFunc := R.AL;
    {$IFDEF DPMI}
    FillChar(DRegs, SizeOf(DRegs), 0);
    with DRegs do begin
      AX := R.AX;
      BX := R.BX;
      CX := R.CX;
      DX := R.DX;
      DS := R.DS;
      if SimulateRealModeInt($2F, DRegs) = 0 then ;
      R.AX := AX;
      R.BX := BX;
      R.CX := CX;
      R.DX := DX;
    end;
    {$ELSE}
    Intr($2F, R);
    {$ENDIF}
    LastCasReturn := R.AX;
  end;

  function CASInstalled : Boolean;
    {-Return True if CAS manager TSR is installed}
  var
    R : Registers;
  begin
    Prep(R, cfIsInstalled);
    CASCall(R);
    CASInstalled := (R.AL = $FF);
  end;

  constructor CASFax.Init(ID : Str20);
    {-Init CASFax object}
  begin
    if not CASInstalled then begin
      AsyncStatus := epFatal+ecFaxNoCASManager;
      Fail;
    end;

    if not AbstractFax.Init(ID) then
      Fail;

    StatusWait := DefStatusTimeout;
    ClassInUse := ctCAS;
    Resolution := 1;
    LogoFile := '';
    @CASAbort := nil;
  end;

  destructor CASFax.Done;
    {-Destroy CASFax object}
  begin
    AbstractFax.Done;
  end;

  procedure CASFax.SubmitTask(FName : PathStr; var Handle : Word);
    {-Submit a CAS task, function 01}
  var
    FNameZ : array[0..80] of Char;
    {$IFDEF DPMI}
    M : DosMemRec;
    {$ENDIF}
  begin
    Prep(Regs, cfSubmitTask);
    Move(FName[1], FNameZ, Length(FName));
    FNameZ[Length(FName)] := #0;
    with Regs do begin
      {$IFDEF DPMI}
      LongInt(M) := GlobalDosAlloc(SizeOf(FName));
      if LongInt(M) = 0 then begin
        AsyncStatus := ecOutOfMemory;
        Handle := 0;
        Exit;
      end;
      Move(FNameZ, Ptr(M.Sele, 0)^, SizeOf(FName));
      DS := M.Segm;
      DX := 0;
      {$ELSE}
      DS := Seg(FNameZ);
      DX := Ofs(FNameZ);
      {$ENDIF}
      CASCall(Regs);
      {$IFDEF DPMI}
      M.Sele := GlobalDosFree(M.Sele);
      {$ENDIF}
      if Integer(AX) > 0 then begin
        AsyncStatus := ecOk;
        Handle := AX;
      end else begin
        Handle := 0;
        AsyncStatus := Word(-AX);
      end;
    end;
  end;

  procedure CASFax.AbortCurrentEvent(var Handle : Word);
    {-Aborts the current CAS event, may take up to 30 seconds, function 02}
  begin
    with Regs do begin
      Prep(Regs, cfAbortEvent);
      CASCall(Regs);
      if Integer(AX) > 0 then begin
        AsyncStatus := ecOk;
        Handle := AX;
      end else begin
        AsyncStatus := Word(-AX);
        Handle := 0;
      end;
    end;
  end;

  procedure CASFax.FindFirstCAS(Q : QueueType; Direction : Boolean;
                                Status : Integer; var Handle : Word);
    {-Find the first event in the specified queue, function 5}
  begin
    with Regs do begin
      Prep(Regs, cfFindFirst);
      CX := Word(Status);
      DH := Byte(Direction);
      DL := Byte(Q);
      CASCall(Regs);
      if AX = 0 then begin
        AsyncStatus := ecOk;
        Handle := BX
      end else begin
        AsyncStatus := Word(-AX);
        Handle := 0;
      end;
    end;
  end;

  procedure CASFax.FindNextCAS(Q : QueueType; var Handle : Word);
    {-Find the next event in the specified queue, function 6}
  begin
    with Regs do begin
      Prep(Regs, cfFindNext);
      DL := Byte(Q);
      CASCall(Regs);
      if AX = 0 then begin
        AsyncStatus := ecOk;
        Handle := BX;
      end else begin
        AsyncStatus := Word(-AX);
        Handle := 0;
      end;
    end;
  end;

  procedure CASFax.OpenFile(Q : QueueType; FileNumber : Word;
                            Handle : Word; var FHandle : Word);
    {-Open a queue file, function 07}
  begin
    with Regs do begin
      Prep(Regs, cfOpenFile);
      BX := Handle;
      CX := FileNumber;
      DL := Byte(Q);
      CASCall(Regs);
      if AX = 0 then begin
        AsyncStatus := ecOk;
        FHandle := BX;
      end else begin
        AsyncStatus := Word(-AX);
        FHandle := 0;
      end;
    end;
  end;

  procedure CASFax.CloseFile(Handle : Word);
    {-Close a previously-opened file handle}
  begin
    FillChar(Regs, SizeOf(Regs), 0);
    with Regs do begin
      AH := dosCloseFile;
      BX := Handle;
      MsDos(Regs);
    end;
  end;

  procedure CASFax.DeleteFile(Handle : Word; FileNumber : Word; Q : QueueType);
    {-Delete the file associated with Handle from queue Q, function 08}
  begin
    with Regs do begin
      Prep(Regs, cfDeleteFile);
      BX := Handle;
      CX := FileNumber;
      DL := Byte(Q);
      CASCall(Regs);
      if AX = 0 then
        AsyncStatus := ecOk
      else
        AsyncStatus := Word(-AX);
    end;
  end;

  procedure CASFax.DeleteAllFiles(Q : QueueType);
    {-Delete all files in queue Q, function 09}
  begin
    with Regs do begin
      Prep(Regs, cfDeleteAll);
      DL := Byte(Q);
      CASCall(Regs);
      if AX = 0 then
        AsyncStatus := ecOk
      else
        AsyncStatus := Word(-AX);
    end;
  end;

  procedure CASFax.GetEventDate(Handle : Word; Q : QueueType;
                               var Year, Month, Day : Word);
    {-Get the date info for event Handle in queue Q, function 0A}
  begin
    with Regs do begin
      Prep(Regs, cfGetEventDate);
      BX := Handle;
      DL := Byte(Q);
      CASCall(Regs);
      if AX = 0 then begin
        AsyncStatus := ecOk;
        Year  := CX;
        Month := DH;
        Day   := DL;
      end else
        AsyncStatus := Word(-AX);
    end;
  end;

  procedure CASFax.SetTaskDate(Handle : Word; Year, Month, Day : Word);
    {-Set the date info for event Handle in queue Q, function 0B}
  begin
    with Regs do begin
      Prep(Regs, cfSetTaskDate);
      BX := Handle;
      CX := Year;
      DH := Byte(Month);
      DL := Byte(Day);
      CASCall(Regs);
      if AX = 0 then
        AsyncStatus := ecOk
      else
        AsyncStatus := Word(-AX);
    end;
  end;

  procedure CASFax.GetEventTime(Handle : Word; Q : QueueType;
                               var Hour, Min, Sec : Word);
    {-Get the time info for event Handle in queue Q, function 0C}
  begin
    with Regs do begin
      Prep(Regs, cfGetEventTime);
      BX := Handle;
      DL := Byte(Q);
      CASCall(Regs);
      if AX = 0 then begin
        AsyncStatus := ecOk;
        Hour := CH;
        Min  := CL;
        Sec  := DH;
      end else
        AsyncStatus := Word(-AX);
    end;
  end;

  procedure CASFax.SetTaskTime(Handle : Word; Hour, Min, Sec : Word);
    {-Set the time info for event Handle in queue Q, function 0D}
  begin
    with Regs do begin
      Prep(Regs, cfSetTaskTime);
      BX := Handle;
      CH := Hour;
      CL := Min;
      DH := Sec;                                                       {!!.01}
      CASCall(Regs);
      if AX = 0 then
        AsyncStatus := ecOk
      else
        AsyncStatus := Word(-AX);
    end;
  end;

  procedure CASFax.GetExternalDataBlock(var EDB : ExternalDataBlock);
    {-Return data in external data block, function 0E}
    {$IFDEF DPMI}
  var
    M : DosMemRec;
    {$ENDIF}
  begin
    with Regs do begin
      FillChar(EDB, SizeOf(EDB), 0);
      Prep(Regs, cfGetEDB);
      {$IFDEF DPMI}
      LongInt(M) := GlobalDosAlloc(SizeOf(EDB));
      if LongInt(M) = 0 then begin
        AsyncStatus := ecOutOfMemory;
        Exit;
      end;
      DS := M.Segm;
      DX := 0;
      {$ELSE}
      DS := Seg(EDB);
      DX := Ofs(EDB);
      {$ENDIF}
      CASCall(Regs);
      if AX = 0 then begin
        AsyncStatus := ecOk;
        {$IFDEF DPMI}
        Move(Ptr(M.Sele, 0)^, EDB, SizeOf(EDB));
        {$ENDIF}
      end else
        AsyncStatus := Word(-AX);
      {$IFDEF DPMI}
      M.Sele := GlobalDosFree(M.Sele);
      {$ENDIF}
    end;
  end;

  procedure CASFax.GetSetAutoReceive(var Rings : Word; GetSet : Word);
    {-Enable/disable autoreceive, function 0F}
  begin
    with Regs do begin
      Prep(Regs, cfGetSetAutoReceive);
      DL := GetSet;
      DH := Rings;
      CASCall(Regs);
      if Integer(AX) >= 0 then begin
        AsyncStatus := ecOk;
        Rings := AX;
      end else begin
        Rings := 0;
        AsyncStatus := Word(-AX);
      end;
    end;
  end;

  procedure CASFax.GetEventStatus(var SRec : StatusRecord);
    {-Return status of current event, function 10}
  {$IFDEF DPMI}
  var
    M : DosMemRec;
  {$ENDIF}
  begin
    with Regs do begin
      Prep(Regs, cfGetEventStatus);
      {$IFDEF DPMI}
      LongInt(M) := GlobalDosAlloc(SizeOf(SRec));
      if LongInt(M) = 0 then begin
        AsyncStatus := ecOutOfMemory;
        Exit;
      end;
      FillChar(Ptr(M.Sele,0)^, SizeOf(SRec), 0);
      DS := M.Segm;
      DX := 0;
      {$ELSE}
      DS := Seg(SRec);
      DX := Ofs(SRec);
      {$ENDIF}
      CASCall(Regs);
      if AX = 0 then begin
        AsyncStatus := ecOk;
        {$IFDEF DPMI}
        Move(Ptr(M.Sele, 0)^, SRec, SizeOf(SRec));
        {$ENDIF}
      end else
        AsyncStatus := Word(-AX);
      {$IFDEF DPMI}
      M.Sele := GlobalDosFree(M.Sele);
      {$ENDIF}
    end;
  end;

  procedure CASFax.GetQueueStatus(Q : QueueType;
                                  var Changes, ControlFiles, ReceivedFiles : Word);
    {-Get status of a queue, function 11}
  begin
    with Regs do begin
      Prep(Regs, cfGetQueueStatus);
      DL := Ord(Q);
      CASCall(Regs);
      Changes := AX;
      ControlFiles := BX;
      ReceivedFiles := CX;
      AsyncStatus := ecOk;
    end;
  end;

  procedure CASFax.GetHardwareStatus(var SArray : StatusArray);
    {-Return vendor-specific hardware status info, function 12}
  begin
    with Regs do begin
      Prep(Regs, cfGetHardwareStatus);
      DS := Seg(SArray);
      DX := Ofs(SArray);
      CASCall(Regs);
      if AX = 0 then
        AsyncStatus := 0
      else
        AsyncStatus := Word(-AX);
    end;
  end;

  procedure CASFax.RunDiagnostics(Mode : Word);
    {-Run hardware diagnostics, function 13}
  begin
    with Regs do begin
      Prep(Regs, cfRunDiagnostics);
      CASCall(Regs);
      DX := Mode;
      if AX <> 0 then
        AsyncStatus := Word(-AX)
      else
        AsyncStatus := 0;
    end;
  end;

  procedure CASFax.MoveReceivedFile(Handle : Word;
                                    FileNumber : Word;
                                    NewName : PathStr);
    {-Move a received file to NewName (path and filename), function 14}
  var
    FNameZ : array[0..80] of Char;
    {$IFDEF DPMI}
    M : DosMemRec;
    {$ENDIF}
  begin
    with Regs do begin
      Move(NewName[1], FNameZ, Length(NewName));
      FNameZ[Length(NewName)] := #0;
      Prep(Regs, cfMoveReceivedFile);
      BX := Handle;
      CX := FileNumber;
      {$IFDEF DPMI}
      LongInt(M) := GlobalDosAlloc(SizeOf(FNameZ));
      if LongInt(M) = 0 then begin
        AsyncStatus := ecOutOfMemory;
        Handle := 0;
        Exit;
      end;
      Move(FNameZ, Ptr(M.Sele, 0)^, SizeOf(FNameZ));
      DS := M.Segm;
      DX := 0;
      {$ELSE}
      DS := Seg(FNameZ);
      DX := Ofs(FNameZ);
      {$ENDIF}
      CASCall(Regs);
      {$IFDEF DPMI}
      M.Sele := GlobalDosFree(M.Sele);
      {$ENDIF}
      if AX = 0 then
        AsyncStatus := ecOk
      else
        AsyncStatus := Word(-AX)
    end;
  end;

  procedure CASFax.SubmitSingleFile(TransType : Word;
                                    TextSize : Word;
                                    Time : Word;
                                    Date : Word;
                                    Dest : String;
                                    FName : PathStr;
                                    Number : String;
                                    Cover : PathStr;
                                    var Handle : Word);
    {-Submit a single file, function 15}
  const
    CRLF : array[0..1] of Char = (cCR, cLF);
  var
    CFRSize : Word;
    CoverLen  : Word;
    Len : Word;
    BytesWritten : Word;
    DotLoc : Byte;
    CF : ControlFileRecordPtr;
    P  : ControlFileRecordPtr;
    FT : FileTransferRecord;
    TempF : File;
    S : String;
    Dir : PathStr;
    {$IFDEF DPMI}
    M : DosMemRec;
    {$ENDIF}

  function ReadCover(var Buf) : Boolean;
  var
    F : Text;
    Index : Word;
    Finished : Boolean;
    Buffer : CoverData absolute Buf;
  begin
    ReadCover := False;

    {Read data from cover file into cover buffer}
    Assign(F, Cover);
    Reset(F);
    AsyncStatus := IoResult;
    if AsyncStatus <> 0 then
      Exit;
    FillChar(Buffer, MaxCoverData, 0);
    Index := 0;
    Finished := False;
    repeat
      ReadLn(F, S);
      Finished := Eof(F);
      S := afConvertHeaderString(S);
      if (Length(S) + Index) < MaxCoverData then begin
        Move(S[1], Buffer[Index], Length(S));
        Inc(Index, Length(S));
        Move(CRLF, Buffer[Index], 2);
        Inc(Index, 2);
      end else
        Finished := True;
    until Finished;
    Close(F);
    if IoResult <> 0 then ;

    CoverLen := Index;
    ReadCover := True;
  end;

  {!!.01 new}
  function CalcPages : Word;
    {-Return the proper page count}
  type
    DcxHeader = record
      DcxCheck : LongInt;
      Offsets  : array[1..1024] of LongInt;
    end;
  var
    LineCnt : Word;
    PageCnt : Word;
    Max     : Word;
    BW      : Word;
    I       : Word;
    F1      : File;
    F2      : Text;
    DH      : DcxHeader;
  begin
    {Make sure function result is assigned for error exits}
    CalcPages := 0;

    case FT.FileType of
      {Text files, scan for pages}
      0 : begin
            Assign(F2, FName);
            Reset(F2);
            AsyncStatus := IoResult;
            LineCnt := 0;
            PageCnt := 0;
            while (AsyncStatus = ecOk) and not Eof(F2) do begin
              ReadLn(F2, S);
              AsyncStatus := IoResult;
              Inc(LineCnt);
              if (Pos(cFF, S) <> 0) or (LineCnt > LinesPerPage) then begin
                Inc(PageCnt);
                LineCnt := 0;
              end;
            end;
            Close(F2);
            if IoResult <> 0 then ;
            if LineCnt <> 0 then
              Inc(PageCnt);
            CalcPages := PageCnt;
          end;

      {PCX files, always one page}
      1 : CalcPages := 1;

      {DCX files, get page count}
      2 : begin
            {Read DCX header}
            SaveMode := FileMode;                                      {!!.02}
            FileMode := AproFileMode;                           {!!.02}{!!.03}
            Assign(F1, FName);
            Reset(F1, 1);
            FileMode := SaveMode;                                      {!!.02}
            BlockRead(F1, DH, SizeOf(DH), BW);
            Close(F1);
            AsyncStatus := IoResult;
            if AsyncStatus <> ecOk then
              Exit;
            if BW < 1028 then
              Max := (BW-4) div 4
            else
              Max := 1024;

            {Scan thru header, count pages}
            I := 1;
            while (I <= Max) and (DH.Offsets[I] <> 0) do
              Inc(I);

            CalcPages := I-1;
          end;

      {Unknown file type, say one page}
      else
        CalcPages := 1;
    end;
  end;

  begin
    if not FlagIsSet(afFlags, afCASSubmitUseControl) then begin
      {Use function 15}
      FillChar(SubmitFile, SizeOf(SubmitFile), 0);
      with SubmitFile do begin
        EventType := TransType;
        XferType := TextSize;
        SendTime := Time;
        SendDate := Date;
        Move(Dest[1], DestName, Length(Dest));
        Move(FName[1], FaxName, Length(FName));
        Move(Number[1], PNumber, Length(Number));
        CoverYesNo := 0;
        if (Cover <> '') then begin
          if not ReadCover(CoverBuffer) then
            Exit;
          CoverYesNo := 1;
        end;
      end;

      {Submit buffer to CAS}
      with Regs do begin
        Prep(Regs, cfSubmitFile);
        {$IFDEF DPMI}
        LongInt(M) := GlobalDosAlloc(SizeOf(SubmitFile));
        if LongInt(M) = 0 then begin
          AsyncStatus := ecOutOfMemory;
          Exit;
        end;
        Move(SubmitFile, Ptr(M.Sele, 0)^, SizeOf(SubmitFile));
        DS := M.Segm;
        DX := 0;
        {$ELSE}
        DS := Seg(SubmitFile);
        DX := Ofs(SubmitFile);
        {$ENDIF}
        CASCall(Regs);
        {$IFDEF DPMI}
        M.Sele := GlobalDosFree(M.Sele);
        {$ENDIF}
        if AX > 0 then begin
          AsyncStatus := ecOk;
          Handle := AX;
        end else begin
          AsyncStatus := Word(-AX);
          Handle := 0;
        end;
      end;
    end else begin
      {Create a ControlFile and submit it ourselves}

      {Get a ControlFile block with room for a file transfer record}
      Len := SizeOf(ControlFileRecord) +
             SizeOf(FileTransferRecord) +
             MaxCoverData;
      if not GetMemCheck(CF, Len) then begin
        Handle := 0;
        AsyncStatus := ecOutOfMemory;
        Exit;
      end;

      FillChar(CF^, Len, 0);
      with CF^ do begin
        EventType := 0;
        XferType := TransType;
        SendTime := Time;
        SendDate := Date;
        if FName = '' then
          FilesToXfer := 0
        else
          FilesToXfer := 1;
        Move(Number[1], PhoneNum, Length(Number));
        Move(Dest[1], DestName, Length(Dest));
        Move(Sender[1], SenderName, Length(Sender));
        Move(LogoFile[1], Logo, Length(LogoFile));
      end;

      FillChar(FT, SizeOf(FT), 0);
      FT.TextSize := TextSize;
      with FT do begin
        DotLoc := Pos('.', FName);
        if DotLoc = 0 then
          FileType := 0
        else begin
          S := Copy(FName, DotLoc+1, 3);
          S := StUpcase(S);
          if S = 'PCX' then
            FileType := 1
          else if S = 'DCX' then
            FileType := 2
          else
            FileType := 0;
        end;

        {Get size of file to transmit}
        SaveMode := FileMode;                                          {!!.02}
        FileMode := AproFileMode;                               {!!.02}{!!.03}
        Assign(TempF, FName);
        Reset(TempF, 1);
        FileMode := SaveMode;                                          {!!.02}
        SizeTotal := FileSize(TempF);
        Close(TempF);
        if IoResult <> 0 then ;

        PagesTotal := CalcPages;                                       {!!.01}
        if AsyncStatus <> ecOk then                                    {!!.01}
          Exit;                                                        {!!.01}
        Move(FName[1], Path, Length(FName));
      end;

      {Handle cover}
      P := CF;
      Inc(P);
      with CF^ do begin
        if Cover <> '' then begin
          if not ReadCover(P^) then begin
            FreeMemCheck(CF, Len);
            Exit;
          end;
          CoverPage := 1;
          OfsToFTR := SizeOf(CF^) + CoverLen;
        end else begin
          CoverLen := 0;
          CoverPage := 0;
          OfsToFTR := SizeOf(CF^);
        end;
      end;

      {Move FT to proper position}
      Move(FT, PByteBuffer(CF)^[CF^.OfsToFTR], SizeOf(FT));

      {Create and write control file}
      GetDir(0, Dir);
      S := AddBackSlash(Dir) + 'TEMP.QUE';
      Assign(TempF, S);
      Rewrite(TempF, 1);
      BlockWrite(TempF, CF^, SizeOf(CF^)+SizeOf(FT)+CoverLen, BytesWritten);
      Close(TempF);
      AsyncStatus := IoResult;
      if BytesWritten <> SizeOf(CF^)+SizeOf(FT)+CoverLen then
        AsyncStatus := ecDiskFull;

      FreeMemCheck(CF, Len);

      {Submit the control file to CAS}
      SubmitTask(S, Handle);
    end;
  end;

  procedure CASFax.SetCASAbortFunc(CAF : AbortFunc);
    {-Set a CAS abort func}
  begin
    CASAbort := CAF;
  end;

  procedure CASFax.GetAllStatus(var Event : Byte;
                                var AStatus : Word;
                                var Pages : Word;
                                var PageTotal : Word;
                                var Bytes : LongInt;
                                var FName : PathStr;
                                var Remote : String);
    {Make status call and return these fields}
  var
    SRec : StatusRecord;
    Len : Byte;
  begin
    GetEventStatus(SRec);
    if AsyncStatus = ecOk then begin
      with SRec do begin
        with CFRec do begin
          Event := EventType;
          AStatus := Word(Status);
          Pages := PagesSent;

          Len := StrLen(RemoteCSID);
          Move(RemoteCsId, Remote[1], Len);
          Remote[0] := Char(Len);

          {WriteLn('  ', Pages, '  ', Status);}                        {!!.01}
        end;

        with FTRec do begin
          Bytes := BytesSent;
          PageTotal := PagesTotal;

          Len := StrLen(Path);
          Move(Path, FName[1], Len);
          FName[0] := Char(Len);
        end;
      end;
    end;
  end;

  function CASFax.csHandleAbort : Boolean;
  begin
    if @CASAbort <> nil then begin
      if CASAbort then begin
        AsyncStatus := ecUserAbort;
        csHandleAbort := True;
      end else
        csHandleAbort := False;
    end else
      csHandleAbort := False;
  end;

  procedure CASFax.SetLogoFile(LF : PathStr);
    {-Set PCX logo file}
  begin
    LogoFile := LF;
  end;

  procedure CASFax.GetPageInfo(var Pages : Word;
                               var Page : Word;
                               var BytesTransferred : LongInt;
                               var PageLength : LongInt);
  var
    Handle : Word;
    Event : Byte;
    AStatus : Word;
    FName : PathStr;
  begin
    if not FlagIsSet(afFlags, afCASWaitTillDone) then begin
      GetAllStatus(Event, AStatus, Page, Pages, BytesTransferred,
                   FName, RemoteID);
    end else begin
      Pages := CurPages;
      Page := CurPage;
      BytesTransferred := CurBytes;
      PageLength := 0;
    end;
  end;

  procedure CASFax.SetCASResolution(High : Boolean);
    {-Pass true to set high resolution}
  begin
    if High then
      Resolution := 0
    else
      Resolution := 1;
  end;

  function CASFax.FaxTransmitPart : FaxStateType;
    {-Do one part of CAS send process}
  var
    Handle : Word;
    Event : Byte;
    AStatus : Word;
    Pages : Word;
    PageTotal : Word;
    Bytes : LongInt;
    FName : PathStr;
  begin
    {Check for user abort request}
    if AsyncStatus <> ecUserAbort then
      if csHandleAbort then
        State := csAbort;

    {Show status periodically}
    if (State <> csInit) and TimerExpired(StatusTimer) then begin
      {Get status info}
      GetAllStatus(Event, AStatus, Pages, PageTotal, Bytes,
                   FName, RemoteID);
      CurPages := PageTotal;
      CurPage := Pages;
      CurBytes := Bytes;

      {Convert status to closest AsyncStatus value}
      if AsyncStatus = ecOk then begin
        case AStatus of
          0,1 : FaxProgress := fpWaiting;
          2   : FaxProgress := fpDialing;
          3   : FaxProgress := fpSendPage;
          5   : AsyncStatus := ecUserAbort;
        end;

        {Reset progress timer}
        NewTimer(ReplyTimer, TransmitWait);
      end else if AsyncStatus = $202 then
        {No current event, say we're waiting}
        FaxProgress := fpWaiting
      else
        {Error, we're done}
        State := csAbort;

      FaxStatus(False, False);
      NewTimer(StatusTimer, StatusWait)
    end;

    {Main state machine}
    case State of
      csInit :
        begin
          FaxProgress := fpWaiting;
          FaxStatus(True, False);
          NewTimer(StatusTimer, StatusWait);
          State := csSubmitting;
          InProgress := False;
        end;

      csSubmitting :
        if NextFax(PhoneNum, FaxFileName, CoverFile) then begin
          LogFax(PhoneNum, FaxFileName, lfaxTransmitStart);
          SubmitSingleFile(Resolution,              {Resolution}
                           0,                       {Text size, 80}
                           0,                       {Send time}
                           0,                       {Send date}
                           Recipient,               {Dest}
                           FaxFileName,             {File to fax}
                           PhoneNum,                {Number to dial}
                           CoverFile,               {Cover file}
                           Handle);                 {Returned handle}

          {Log results of submission}
          if AsyncStatus = ecOk then
            LogFax(PhoneNum, FaxFileName, lfaxTransmitOk)
          else
            LogFax(PhoneNum, FaxFileName, lfaxTransmitFail);
        end else begin
          if FlagIsSet(afFlags, afCASWaitTillDone) then
            State := csWaiting
          else begin
            AsyncStatus := ecOk;
            FaxStatus(False, True);
            State := csDone
          end;

          {Set progress timer}
          NewTimer(ReplyTimer, TransmitWait);
        end;

      csWaiting :
        if TimerExpired(ReplyTimer) then begin
          {Too much time elapsed without a current event, must be finished}
          AsyncStatus := ecOk;
          FaxStatus(False, True);
          State := csDone;
        end;

      csAbort :
        begin
          FaxStatus(False, True);
          State := csDone;
        end;
    end;

    if State = csDone then
      FaxTransmitPart := faxFinished
    else
      FaxTransmitPart := faxWaiting;
  end;

  procedure CASFax.PrepareFaxTransmitPart;
    {-Prepare to call FaxTransmitPart}
  begin
    AsyncStatus := ecOk;
    FaxProgress := fpWaiting;
    State := csInit;
    NewTimer(StatusTimer, StatusWait);
  end;

  procedure CASFax.FaxTransmit;
    {-Call CAS function to submit all fax entries}
  var
    Status : FaxStateType;
  begin
    PrepareFaxTransmitPart;

    repeat
      Status := FaxTransmitPart;
    until Status = faxFinished;
  end;

  function CASFax.FaxReceivePart : FaxStateType;
    {-Do one part of CAS receive process}
  var
    Handle : Word;
    Event : Byte;
    AStatus : Word;
    Pages : Word;
    PageTotal : Word;
    Bytes : LongInt;
    FName : PathStr;
    Number : String;
  begin
    {Check for user abort request}
    if AsyncStatus <> ecUserAbort then
      if csHandleAbort then
        State := csAbort;

    {Show status periodically}
    if (State <> csInit) and TimerExpired(StatusTimer) then begin
      {Get status info}
      GetAllStatus(Event, AStatus, Pages, PageTotal, Bytes,
                   FName, RemoteID);
      CurPages := PageTotal;
      CurPage := Pages;
      CurBytes := Bytes;

      {InProgress is set True after first non-zero status}
      if (AsyncStatus = ecOk) and not InProgress then begin
        InProgress := AStatus <> 0;
        if InProgress then
          FaxStatus(True, False);
      end else if (AsyncStatus <> ecOk) and InProgress then begin
        {Current receive is finished, prepare to move it}
        NewTimer(ReplyTimer, 0);
        InProgress := False;
        AsyncStatus := ecOk;
        FaxStatus(False, True);
      end;

      {Convert status to closest AsyncStatus value}
      if AsyncStatus = ecOk then begin
        case AStatus of
          0 : if InProgress then begin
                {Current receive is finished, prepare to move it}
                NewTimer(ReplyTimer, 0);
                InProgress := False;
                AsyncStatus := ecOk;
                FaxStatus(False, True);
              end else
                FaxProgress := fpWaiting;
          1 : FaxProgress := fpWaiting;
          4 : FaxProgress := fpGetPage;
          5 : AsyncStatus := ecUserAbort;
        end;
      end else if AsyncStatus = $202 then begin
        {No current event, say we're waiting}
        FaxProgress := fpWaiting;
        AsyncStatus := ecOk;
      end else
        {Error, we're done}
        State := csAbort;

      if InProgress then begin
        FaxStatus(False, False);
        NewTimer(StatusTimer, StatusWait)
      end;
    end;

    {Main state machine}
    case State of
      csInit :
        begin
          FaxProgress := fpWaiting;
          NewTimer(StatusTimer, StatusWait);
          State := csWaiting;
          InProgress := False;
          FirstReceive := True;
        end;

      csWaiting :
        {Check for file in receive log, move it to dest dir}
        begin
          {Don't check while receiving a file}
          if InProgress then begin
            FaxReceivePart := faxWaiting;
            Exit;
          end;

          {Don't check too often}
          if FirstReceive then
            if not TimerExpired(ReplyTimer) then begin
              FaxReceivePart := faxWaiting;
              Exit;
            end;

          if FirstReceive then begin
            FindFirstCAS(qReceive, False, 0, Handle);
            FirstReceive := False;
          end else begin
            FindNextCAS(qReceive, Handle);
          end;

          if AsyncStatus = ecOk then begin
            FaxFileName := FaxName;
            LogFax('', FaxFileName, lfaxReceiveStart);
            MoveReceivedFile(Handle, 1, FaxFileName);
            if AsyncStatus = ecOk then begin
              LogFax('', FaxFileName, lfaxReceiveOk);
              DeleteFile(Handle, 0, qReceive);
            end else
              LogFax('', FaxFileName, lfaxReceiveFail);
          end else begin
            FirstReceive := True;
            if not FlagIsSet(afFlags, afCASWaitTillDone) then begin
              FaxStatus(False, True);
              State := csDone;
            end else begin
              NewTimer(ReplyTimer, ReceiveWait);
            end;
          end;
        end;

      csAbort :
        begin
          if InProgress then
            FaxStatus(False, True);
          State := csDone;
        end;
    end;

    if State = csDone then
      FaxReceivePart := faxFinished
    else
      FaxReceivePart := faxWaiting;
  end;

  procedure CASFax.PrepareFaxReceivePart;
    {-Prepare to call FaxReceivePart}
  begin
    FaxProgress := fpWaiting;
    State := csInit;
    NewTimer(StatusTimer, StatusWait);
    NewTimer(ReplyTimer, 0);
  end;

  procedure CASFax.FaxReceive;
    {-Call FaxReceivePart until fax is sent}
  var
    Status : FaxStateType;
  begin
    PrepareFaxReceivePart;

    repeat
      Status := FaxReceivePart;
    until Status = faxFinished;
  end;

  {$IFDEF UseStreams}
  constructor CASFax.Load(var S : IdStream);
  begin
    {Use ancestor load}
    AbstractFax.Load(S);
    if S.PeekStatus <> 0 then begin
      Done;
      Fail;
    end;

    {Load data}
    S.Read(Resolution, SizeOf(Resolution));
    S.Read(StatusWait, SizeOf(StatusWait));
    LogoFile := S.ReadString;

    @CasAbort := S.ReadUserPointer(Nil);
  end;

  procedure CASFax.Store(var S : IdStream);
  begin
    {Ancestor store}
    AbstractFax.Store(S);

    {Store data}
    S.Write(Resolution, SizeOf(Resolution));
    S.Write(StatusWait, SizeOf(StatusWait));
    S.WriteString(LogoFile);

    S.WriteUserPointer(@CasAbort, PtNil);
  end;

  procedure CasFaxStream(SPtr : IdStreamPtr);
    {-Register all types needed for cas fax converter}
  begin
    AbstractFaxStream(SPtr);
    SPtr^.RegisterType(otCasFax, veCasFax,
                       TypeOf(CasFax),
                       @CasFax.Store, @CasFax.Load);
  end;
  {$ENDIF}

end.
