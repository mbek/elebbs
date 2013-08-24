{$A+,F+,I-,R-,S-,V-,B+}
{$M 8192, 0, 655360}

{$I APDEFINE.INC}       {Conditional defines that may affect this program}
{$I OPDEFINE.INC}       {Conditional defines that may affect this program}

(* NOTE:
RFAXO calls OPTSR.SuppressMouseHandling to avoid the hiding and restoring the
mouse cursor while popping up and popping down (which, at the frequency RFAXO
works, can result in annoying mouse cursor blinking). SuppressMouseHandling
was added in OPRO 1.13 so it's in a {$IFDEF OPRO12} block to avoid compilation
errors with earlier versions of OPRO. If you have OPRO 1.13 or 1.14 you can
remove that conditional check to take advantage of SupressMouseHandling
*)

{.$DEFINE CheckStack}    {Enable for FillChar-style stack checking}

{$DEFINE SmallStatus}   {Enable for small status during background receive}

{$IFNDEF UseOPro}
  !! STOP COMPILE: This program requires Object Professional or
                   TSR's Made Easy !!
{$ENDIF}

{$IFDEF DPMI}
  !! STOP COMPILE: This program is real-mode only !!
{$ENDIF}

{$IFDEF Windows}
  !! STOP COMPILE: This program is real-mode only !!
{$ENDIF}

{******************************************************}
{*                   RFAXO.PAS 2.03                   *}
{*      Copyright (c) TurboPower Software 1993.       *}
{*                All rights reserved.                *}
{******************************************************}

program RFaxO;
  {-Demo TSR for receiving faxes using OOP}

uses
  Dos,
  OpString,
  OpCrt,
  OpRoot,
  OpInt,
  OpTsr,
  ApMisc,
  ApPort,
  ApUart,
  OoCom,
  OoFaxCvt,
  OoAbsFax,
  OoFax12,
  OoFaxCAS;

const
  CopyrightLine = 'RFAXO 2.03 -- receive fax TSR demo   (c)1993 TurboPower Software';
  ClockHandle   = 16;
  RFaxNotLoaded = 'RFAXO isn''t loaded';
  RFaxInterval  = 9;
  OutBufSize    = 128;

const
  ModuleName    : String[5] = 'RFAXO';
  InBufSize     : Word = 4096;
  Disabled      : Boolean = False;
  CommandBaud   : LongInt = 0;                                         {!!.01}

const
  WAttr      = $1B;   {Window attribute}
  FAttr      = $1E;   {Frame attribute}
  DAttr      = $1F;   {Data attribute}
  SmallAttr  = $03;   {Small status line attribute}

const
  CName           : ComNameType = Com1;
  BRate           : LongInt = 19200;
  FaxBRate        : Word = 9600;
  MInit           : String[40] = 'ATM0V1X4';
  StatID          : String[40] = 'RFAXO DEMO';
  FaxPath         : PathStr = '';
  CPort           : AbstractPortPtr = nil;
  Receiver        : C12ReceiveFaxPtr = nil;
  NeedMasterReset : Boolean = False;
  InBackground    : Boolean = True;
  FaxClass        : ClassType = ctClass2;
  IrqNum          : Byte = 0;

type
  BufPtr = ^BufferArray;
  BufferArray = array[0..MaxInt] of Char;

var
  PopStack     : Array[1..6000] of Byte;
  RFaxProcHdl  : Byte;
  FaxState     : FaxStateType;

  procedure Abort(Msg : String);
  begin
    WriteLn(Msg);
    Halt(1);
  end;

  function InitAbort : Boolean;
  begin
    InitAbort := False;
    if KeyPressed then
      InitAbort := ReadKey = #27;
  end;

  procedure ClockInt(BP : Word); interrupt;
    {-INT $1C handler -- periodically invokes the RcvProc popproc}
  const
    Counter : Byte = 0;
  var
    Regs : IntRegisters absolute BP;
  begin
    {if we're active...}
    if not Disabled then begin
      {Always increment protocol interval counter}
      Inc(Counter);

      if (CPort <> nil) and (Receiver <> nil) then
        with Receiver^, CPort^ do
          if (Counter > RFaxInterval) or
             (CharReady) or
             (NeedMasterReset) then begin
            SetPopTicker(RFaxProcHdl, 8);
            Counter := 0;
          end;
    end;

    {Chain to previous INT $1C handler}
    ChainInt(Regs, IsrArray[ClockHandle].OrigAddr);
  end;

  function BuildWindow(XLow, YLow, XHigh, YHigh : Byte; Header : String) : Pointer;
    {-Saves the underlying screen, frames and clears a window}
  type
    FrameCharType = (ULeft, LLeft, URight, LRight, Horiz, Vert);
    FrameArray = array[FrameCharType] of Char;
  const
    FrameChars : FrameArray = 'ÕÔ¸¾Í³';
  var
    CoversP : BufPtr;
    WordsPerRow : Word;
    BufBytes : Word;
    SrcPos : Word;
    DestPos : Word;
    Row : Word;
    HeaderLen : Byte absolute Header;
    Width, HeaderPos : Byte;
    Span : string[132];
    SpanLen : Byte absolute Span;

  begin
    BuildWindow := nil;

    {Compute number of words to move per row}
    WordsPerRow := Succ(XHigh-XLow);

    {Compute bytes needed for screen buffer}
    BufBytes := (WordsPerRow*Succ(YHigh-YLow)) shl 1;

    {Make sure enough memory is available}
    if not GetMemCheck(CoversP, BufBytes) then
      Exit;

    {Save current contents to the screen buffer}
    DestPos := 0;
    SrcPos := (Pred(YLow)*ScreenWidth+Pred(XLow)) shl 1;
    for Row := YLow to YHigh do begin
      MoveScreen(Mem[VideoSegment:SrcPos], CoversP^[DestPos], WordsPerRow);
      Inc(SrcPos, ScreenWidth shl 1);
      Inc(DestPos, WordsPerRow shl 1);
    end;

    {Calculate width of window and position of header}
    SpanLen := Succ(XHigh - XLow);
    Width := SpanLen-2;

    {construct the upper border and draw it}
    FillChar(Span[2], Width, FrameChars[Horiz]);
    Span[1] := FrameChars[ULeft];
    Span[SpanLen] := FrameChars[URight];
    FastWrite(Span, YLow, XLow, FAttr);

    {Draw the vertical bars}
    for Row := Succ(YLow) to Pred(YHigh) do begin
      FastWrite(FrameChars[Vert], Row, XLow, FAttr);
      FastWrite(FrameChars[Vert], Row, XHigh, FAttr);
    end;

    {Draw the bottom border}
    Span[1] := FrameChars[LLeft];
    Span[SpanLen] := FrameChars[LRight];
    FastWrite(Span, YHigh, XLow, FAttr);

    {Draw the header}
    if HeaderLen > 0 then begin
      if HeaderLen > Width then
        HeaderLen := Width;
      HeaderPos := (SpanLen-HeaderLen) shr 1;
      FastWrite(Header, YLow, XLow + HeaderPos, FAttr);
    end;

    {Fill in the window}
    for Row := Ylow+1 to YHigh-1 do
      FastWrite(CharStr(' ', Pred(XHigh-XLow)), Row, XLow+1, FAttr);

    BuildWindow := CoversP;
  end;

  procedure RemoveWindow(P : Pointer; XLow, YLow, XHigh, YHigh : Byte);
    {-Restore screen contents and deallocate buffer space if requested}
  var
    CoversP : BufPtr absolute P;
    WordsPerRow : Word;
    SrcPos : Word;
    DestPos : Word;
    Row : Word;
  begin
    {Compute number of words to move per row}
    WordsPerRow := Succ(XHigh-XLow);

    {Restore current contents to the screen buffer}
    DestPos := 0;
    SrcPos := (Pred(YLow)*ScreenWidth+Pred(XLow)) shl 1;
    for Row := YLow to YHigh do begin
      MoveScreen(CoversP^[DestPos], Mem[VideoSegment:SrcPos], WordsPerRow);
      Inc(SrcPos, ScreenWidth shl 1);
      Inc(DestPos, WordsPerRow shl 1);
    end;

    {Deallocate buffer space}
    FreeMemCheck(CoversP, (WordsPerRow*Succ(YHigh-YLow)) shl 1);
  end;

  procedure RFaxStatus(FP : AbstractFaxPtr; Starting, Ending : Boolean);
  const
    P : Pointer = nil;
  var
    S : String[40];
    BPS : LongInt;
    Res : Boolean;
    ECM : Boolean;
    Progress : Word;
  begin
    {do nothing if running totally in background}
    if InBackground then
      Exit;

    if Starting then begin
      if P = nil then begin
        P := BuildWindow((ScreenWidth shr 1)-22, 8, (ScreenWidth shr 1)+22, 14, ' Receiving FAX ');
        FastWrite(' State:', 10, (ScreenWidth shr 1)-20, WAttr);
        FastWrite('Status:', 11, (ScreenWidth shr 1)-20, WAttr);
        FastWrite(' Speed:', 12, (ScreenWidth shr 1)-20, WAttr);
        FastWrite(Pad(Long2Str(FaxBRate), 33), 12, (ScreenWidth shr 1)-12, DAttr);
      end;
    end;

    with C12ReceiveFaxPtr(FP)^ do
      if P <> nil then begin
        FastWrite(Pad(Long2Str(Ord(State)), 5), 10, (ScreenWidth shr 1)-12, DAttr);
        Progress := GetFaxProgress;
        case Progress of
          fpAnswer:
            S := 'Answering incoming call';
          fpIncoming:
            S := 'Incoming call is fax';
          fpGotRemoteID :
            S := 'Call from '+RemoteID;
          fpSessionParams :
            begin
              GetSessionParams(BPS, Res, ECM);
              S := Long2Str(BPS);
              if Res then
                S := S + ', high'
              else
                S := S + ', std';
              if ECM then
                S := S + ', ECM'
              else
                S := S + ', no ECM';
              FastWrite(Pad(Long2Str(BPS)+'   ', 33), 12, (ScreenWidth shr 1)-12, DAttr);
            end;
          fpGetPage:
            S := 'Receiving page data: '+Long2Str(DataCount);
          fpGetPageResult :
            S := 'Getting page result';
          fpCheckMorePages :
            S := 'Checking for more pages';
          fpGetHangup:
            S := 'Waiting for hangup command';
          fpGotHangup:
            S := 'Disconnect request acknowledged';
          else
            S := '';
        end;
        FastWrite(Pad(S, 33), 11, (ScreenWidth shr 1)-12, DAttr);
      end;

    if Ending then
      if P <> nil then begin
        RemoveWindow(P, (ScreenWidth shr 1)-22, 8, (ScreenWidth shr 1)+22, 14);
        P := nil;
      end;
  end;

  procedure RFaxLog(CP : AbstractFaxPtr;
                    Number : String;
                    FName : PathStr;
                    Log : TLogFaxCode);
  var
    FLog : Text;
  begin
    with C12ReceiveFaxPtr(CP)^ do begin
      Assign(FLog, 'RFAXO.HIS');
      Append(FLog);
      if IOResult = 2 then
        ReWrite(FLog);
      if IOResult <> 0 then
        Exit;
      case Log of
        lfaxReceiveStart :
          WriteLn(FLog, TodayString, ' ', NowString,
                        ' -- start fax receive: ', FName,
                        '  from ', GetRemoteID);
        lfaxReceiveOk :
          begin
            WriteLn(FLog, TodayString, ' ', NowString,
                          ' -- end fax receive: ', FName);
            WriteLn(FLog);
          end;
        lfaxReceiveSkip :
          begin
            WriteLn(FLog, TodayString, ' ', NowString,
                          ' -- fax rejected from ', GetRemoteID);
            WriteLn(FLog);
          end;
        lfaxReceiveFail :
          begin
            WriteLn(FLog, TodayString, ' ', NowString,
                          ' -- fail fax receive(', AsyncStatus, '): ', FName);
            WriteLn(FLog);
          end;
      end;
      Close(FLog);
      if IOResult <> 0 then ;
    end;
  end;

  function OpenPort : Boolean;
    {-Open the com port}
  begin
    OpenPort := False;
    if IrqNum <> 0 then begin
      SetUart(CName, 0, IrqNum, IrqNum+8);
      if (CName = Com3) or (CName = Com4) then
        PS2DetectMode := PS2Ignore;
    end;
    CPort := New(UartPortPtr, InitCustom(CName, BRate, NoParity, 8, 1,
                                         InBufSize, OutBufSize, DefPortOptions));
    OpenPort := (CPort <> nil);
  end;

  function InitFaxModem(Startup : Boolean) : Boolean;
  var
    Class : Char;
    Model, Chip, Rev : String;
    OK : Boolean;                                                      {!!.01}
  begin
    InitFaxModem := False;
    NeedMasterReset := False;

    if not Startup then begin
      if CPort <> nil then begin
        CPort^.SetModem(False, False);
        Dispose(CPort, Done);
        CPort := nil;
      end;
      if not OpenPort then
        WriteLn('Unable to initialize port')
      else
        CPort^.SetModem(True, True);
    end;

    with Receiver^ do begin
      afOptionsOn(afExitOnError);                                      {!!.03}
      SetFaxPort(CPort);
      SetDestinationDir(FaxPath);
      SetFaxStatusProc(RFaxStatus);
      SetFaxLogProc(RFaxLog);
      SetModemInit(MInit);
      if CommandBaud <> 0 then                                         {!!.01}
        SetInitBaudRate(CommandBaud, 0);                               {!!.01}

      if Startup then begin
        if GetModemInfo(Class, Model, Chip, Rev, True) then begin
          if Class = 'B' then                                          {!!.02}
            WriteLn('Highest class: 2.0')                              {!!.02}
          else                                                         {!!.02}
            WriteLn('Highest class: ', Class);
          if Class = '1' then
            {Force to class 1}
            FaxClass := ctClass1;
        end else
          Abort('Unable to identify modem');

        {Set desired class}
        FaxClass := SetClassType(FaxClass);
        case FaxClass of
          ctClass1 : WriteLn('Fax class is set to Class 1');
          ctClass2 : WriteLn('Fax class is set to Class 2');
          else begin
            WriteLn('Failed to set fax class');
            Exit;
          end;
        end;
      end;
      OK := InitModemForFaxReceive;                                    {!!.01}
      if OK then                                                       {!!.01}
        PrepareFaxReceivePart;                                         {!!.01}
      InitFaxModem := OK;                                              {!!.01}
    end;
  end;

  procedure PrepareFaxModem;
  begin
    Receiver := New(C12ReceiveFaxPtr, Init(StatID, CPort));
    if Receiver = nil then
      Abort('Unable to init fax manager');

    {Set abort function for initialization only}
    CPort^.SetAbortFunc(InitAbort);

    {Prepare for receive}
    WriteLn('Identifying and initializing modem...');
    if not InitFaxModem(True) then
      Abort('Unable to initialize modem');

    {Remove abort function}
    CPort^.SetAbortFunc(NoAbortFunc);
  end;

  procedure RFaxProc(var Regs : Registers);
    {-Do a small portion of the protocol}
  const
    First : Boolean = True;
  var
    Pages : Word;
    Page : Word;
    Bytes : LongInt;
    Total : LongInt;
  begin
    with Receiver^ do begin
      if NeedMasterReset then
        if InitFaxModem(False) then ;

      if (not InBackground) and (State >= rfAnswer) then begin
        {Stay in popup if not in background and already answered phone}
        SetOneFax(True);
        repeat
          FaxState := FaxReceivePart;
        until (AsyncStatus <> ecOK) or (FaxState = faxFinished);  {!!.03}
        State := rfInit;
      end else begin
        {Call state machine once and show little status}
        repeat
          FaxState := FaxReceivePart;
          {$IFDEF SmallStatus}
          if InTextMode and InBackground then begin
            case State of
              rfAnswer :
                FastWrite('Answering...        ', 1, 60, SmallAttr);
              rfGetPageData :
                if First then begin
                  FastWrite('Rec: '+ Pad(GetFaxName, 15), 1, 60, SmallAttr);
                  First := False;
                end else begin
                  GetPageInfo(Pages, Page, Bytes, Total);
                  FastWrite('Page ' + Pad(Long2Str(Page), 2) +
                            ': ' + Pad(Long2Str(Bytes), 5) +
                            '       ', 1, 60, SmallAttr);
                end;
              rfComplete,
              rfAbort :
                begin
                  FastWrite('Receive complete    ', 1, 60, SmallAttr);
                  First := True;
                end;
            end;
          end;
          {$ENDIF}
        until FaxState <> faxCritical;
      end;
    end;
  end;

  procedure ExternalIFC(BP : Word); interrupt;
    {-Entry point for external requests}
  var
    Regs : IntRegisters absolute BP;
    SavePSP : Word;
  begin
    {Unload TSR if requested}
    case Regs.AH of
      1 : begin   {Unload}
            {$IFDEF Tracing}
            DumpTrace('RFAXO.TRC');
            {$ENDIF}

            {$IFDEF EventLogging}
            DumpEvents('RFAXO.LOG');
            {$ENDIF}

            {$IFDEF CheckStack}
            {Check stack usage}
            I := 1;
            while (I < SizeOf(PopStack)) and (PopStack[I] = $FF) do
              Inc(I);
            WriteLn;
            WriteLn('Used ', SizeOf(PopStack)-I, ' bytes of stack');
            WriteLn;
            {$ENDIF}

            Dispose(CPort, Done);
            SavePSP := GetPSP;
            SetPSP(PrefixSeg);
            Regs.AL := Ord(DisableTSR);
            SetPSP(SavePSP);
          end;
      2 : begin
            Disabled := False;
            CPort^.SetModem(True, True);
            NeedMasterReset := True;
          end;
      3 : if Receiver^.State > rfWaiting then
            Regs.AL := 1
          else begin
            Regs.AL := 0;
            Disabled := True;
            CPort^.SetModem(False, False);
          end;
    end;
  end;

  procedure UnloadRFax;
    {-Try to unload the resident copy}
  var
    P : IfcPtr;
    Regs : IntRegisters;
  begin
    P := ModulePtrByName(ModuleName);
    if P = nil then
      WriteLn(RFaxNotLoaded)
    else if (P^.CmdEntryPtr <> nil) then begin
      RestoreAllVectors;
      Regs.AH := 1;
      EmulateInt(Regs, P^.CmdEntryPtr);
      if Regs.AL = 1 then
        WriteLn('RFAXO Unloaded')
      else
        WriteLn('Can''t unload RFAXO');
    end;
    Halt;
  end;

  procedure ResetRFax;
    {-Try to reset the resident copy}
  var
    P : IfcPtr;
    Regs : IntRegisters;
  begin
    P := ModulePtrByName(ModuleName);
    if P = nil then
      WriteLn(RFaxNotLoaded)
    else if (P^.CmdEntryPtr <> nil) then begin
      RestoreAllVectors;
      Regs.AH := 2;
      EmulateInt(Regs, P^.CmdEntryPtr);
      WriteLn('RFAXO reset');
    end;
    Halt;
  end;

  procedure DisableRFax;
    {-Try to disable the resident copy}
  var
    P : IfcPtr;
    Regs : IntRegisters;
  begin
    P := ModulePtrByName(ModuleName);
    if P = nil then
      WriteLn(RFaxNotLoaded)
    else if (P^.CmdEntryPtr <> nil) then begin
      RestoreAllVectors;
      Regs.AH := 3;
      EmulateInt(Regs, P^.CmdEntryPtr);
      if Regs.AL = 0 then
        WriteLn('RFAXO disabled, run RFAXO /R to re-enable')
      else
        WriteLn('Receive in progress, can''t disable right now.');
    end;
    Halt;
  end;

  procedure ShowHelp;
  begin
    WriteLn('Usage: RFAXO ComPort [options]');
    WriteLn('  /U             Unload resident copy of RFAXO');
    WriteLn('  /R             Tell the resident copy of RFAXO to reset the modem');
    WriteLn('  /D             Disable (but leave loaded) resident copy of RFAXO');
    WriteLn('  /T             Load as pop-up (default=full background)');
    WriteLn('  /B Baud        Set port baud rate (4800, 7200, 9600, 19200; default = 19200)');
    WriteLn('  /L LowBaud     Set low baud rate for modem init (same as above; default=none)'); {!!.01}
    WriteLn('  /F FaxBPS      Set fax BPS rate (2400, 4800, 7200, 9600, 12000, 14400;');
    WriteLn('                                   default=9600)');
    WriteLn('  /M InitString  Set modem initialization string');
    WriteLn('  /I StationID   Set station ID');
    WriteLn('  /P Path        Set received fax files path');
    WriteLn('  /S FaxClass    Set fax class (1, 2, A(uto); default=2)');
    WriteLn('  /?             Display this help screen');
    Halt(0);
  end;

  procedure ParseCmdLine;
  var
    S : String;
    I : Integer;
    C : Integer;
    GotC : Boolean;

    function NextS : String;
    begin
      Inc(I);
      NextS := ParamStr(I);
    end;

    function GetDelimited : String;
    var
      S, SPart: String[20];
    begin
      Inc(I);
      S := ParamStr(I);
      if S[1] = '"' then begin
        Delete(S, 1, 1);
        while (S[Length(S)] <> '"') and (I < ParamCount) do begin
          Inc(I);
          SPart := ParamStr(I);
          S := S + ' ' + SPart;
        end;
        Dec(Byte(S[0]));
      end;
      GetDelimited := S;
    end;

  begin
    WriteLn(CopyrightLine);

    GotC := False;
    for I := 1 to ParamCount do begin
      S := ParamStr(i);

      if S[1] in ['/','-'] then begin
        case Upcase(S[2]) of
          '?' :
            ShowHelp;
          'B' :
            begin
              Val(NextS, BRate, C);
              if C <> 0 then
                BRate := 19200;
            end;
          'L' :                                                        {!!.01}
            begin                                                      {!!.01}
              Val(NextS, CommandBaud, C);                              {!!.01}
              if C <> 0 then                                           {!!.01}
                CommandBaud := 0;                                      {!!.01}
            end;                                                       {!!.01}
          'F' :
            begin
              Val(NextS, FaxBRate, C);
              if C <> 0 then
                FaxBRate := 9600;
            end;
          'M' :
            MInit := NextS;
          'U' :
            UnloadRFax;
          'R' :
            ResetRFax;
          'D' :
            DisableRFax;
          'I' :
            StatID := GetDelimited;
          'P' :
            FaxPath := NextS;
          'T' :
            InBackground := False;
          'S' :
            begin
              S := NextS;
              case S[1] of
                '1' : FaxClass := ctClass1;
                '2' : FaxClass := ctClass2;
                'A' : FaxClass := ctDetect;
                else ShowHelp;
              end;
            end;
          'Q' :
            Val(NextS, IrqNum, C);
          else
            begin
              WriteLn(^G'Unknown parameter "'+S+'"');
              ShowHelp;
            end;
        end;
      end else begin
        GotC := True;
        case S[1] of
          '1': CName := Com1;
          '2': CName := Com2;
          '3': CName := Com3;
          '4': CName := Com4;
          else ShowHelp;
        end;
      end;
    end;

    if not GotC then
      ShowHelp;
  end;

begin
  {Warn about CAS}
  if CASInstalled then
    Abort('CASMODEM.EXE detected, RFAXO is aborting');

  {Process command line arguments}
  ParseCmdLine;

  {Don't install twice}
  if ModuleInstalled(ModuleName) then begin
    WriteLn('RFAXO already installed');
    Halt;
  end;

  {$IFDEF Tracing}
  InitTracing(10000);
  {$ENDIF}

  {$IFDEF EventLogging}
  InitEventLogging(10000);
  DumpEvents('x.log');
  {$ENDIF}

  {Open port, fax objects; initalize modem for receive}
  if not OpenPort then
    Halt;
  PrepareFaxModem;

  {Install the module}
  InstallModule(ModuleName, @ExternalIfc);

  {$IFDEF Opro12}
  {Don't hide mouse during popup}
  SuppressMouseHandling;
  {$ENDIF}

  {$IFDEF CheckStack}
  FillChar(PopStack, SizeOf(PopStack), $FF);
  {$ENDIF}

  {Setup entry points and go resident}
  if DefinePopProc(RFaxProcHdl, RFaxProc, @PopStack[SizeOf(PopStack)]) then begin
    {Install clock ISR}
    if not InitVector($1C, ClockHandle, @ClockInt) then begin
      WriteLn('Failed to set clock int');
      Halt;
    end;

    Write('RFAXO loaded');
    if InBackground then
      WriteLn(', full background operation.')
    else
      WriteLn(', will pop up when fax received.');
    WriteLn('Enter "RFAXO /U" to unload, "RFAXO /R" to reset modem');

    {Enable popups}
    PopupsOn;

    {Terminate and stay resident}
    if InBackground then
      StayRes(ParagraphsToKeep, 0)
    else
      StayRes(ParagraphsToKeep+200, 0);
  end;

  WriteLn('Failed to load RFAXO');
  RestoreVector(ClockHandle);
end.
