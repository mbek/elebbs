{$S-,R-,V-,I-,B-,F+,A-}
{$M 16384, 0, 655360}

{$I APDEFINE.INC}    {Conditional defines that may affect this program}

{.$DEFINE TestStreams}  {Enable stream testing}

{$DEFINE AutoRetry}  {Enable to allow the fax object to automatically
                      retry busy fax numbers. Disable to give the
                      application control over retries.}

{$DEFINE FaxHWFlow}  {Enable to turn on one-way hardware flow control}

{******************************************************}
{*                 SIMPSNDO.PAS  2.03                 *}
{*      Copyright (c) TurboPower Software 1993.       *}
{*                All rights reserved.                *}
{******************************************************}

program SimpSndO;
  {-Simple send-fax demo using OOP}

uses
  {$IFDEF LeakCheck}                                                   {!!.01}
  LeakChek,                                                            {!!.01}
  {$ENDIF}                                                             {!!.01}
  Dos,
  {$IFDEF UseOPro}
  OpCrt,
  OpRoot,
  OpString,                                                            {!!.03}
  {$ENDIF}
  {$IFDEF UseTPro}
  TpCrt,
  TpString,                                                            {!!.03}
  {$ENDIF}
  {$IFDEF Standalone}
  Crt,
  {$ENDIF}
  ApMisc,
  ApTimer,
  ApPort,
  ApUart,
  OoCom,
  OoFaxCvt,
  OoAbsFax,
  OoFax12,
  OoFaxCas;

const
  CopyrightLine = 'SIMPSNDO 2.03 -- send fax demo   (c)1993 TurboPower Software';
  Header = '$D  $T   From: $S  $I                            Page $P of $N';

const
  MInit       : String[40] = '';
  FaxFile     : PathStr = '';                    {Fax file to send}
  CoverPage   : PathStr = '';                    {cover page file}
  PNumber     : String  = '';                    {number to dial}
  StatID      : Str20   = 'SIMPSNDO DEMO';       {this station's ID}
  DefTitle    : String  = 'TurboPower Software'; {this station's title}
  CName       : ComNameType = Com1;              {port to use}
  BRate       : LongInt = 19200;                 {bit rate}
  FaxBRate    : Word = 9600;                     {FAX bit rate}
  SetFeatures : Boolean = False;                 {True to get/set features}
  SetFaxBRate : Boolean = False;                 {True to set fax bit rate}
  FaxClass    : ClassType = ctClass2;            {Class1,2 or CAS}
  IrqNum      : Byte = 0;                        {Undocumented IRQ switch}
  SendInitBaud: LongInt = 0;                     {Baud rate for init cmds} {!!.01}

var
  Sender : AbstractFaxPtr;
  CPort  : UartPortPtr;
  Class : Char;
  Model, Chip, Rev : String;
  BitRate : LongInt;
  Check : Char;

  procedure AbortMsg(S : String);
    {-Display message and halt}
  begin
    {$IFDEF Tracing}
    DumpTraceHex('SIMPSNDO.TRC');
    {$ENDIF}

    {!!.03 - Added}
    {$IFDEF EventLogging}
    DumpEvents('SIMPSNDO.LOG');
    {$ENDIF}

    if Sender <> nil then                                              {!!.01}
      Dispose(Sender, Done);                                           {!!.01}
    if CPort <> nil then                                               {!!.01}
      Dispose(CPort, Done);                                            {!!.01}

    WriteLn(S);
    Halt(1);
  end;

  function Long2Str(L : LongInt) : String;
  var
    S : String[20];
  begin
    Str(L, S);
    Long2Str := S;
  end;

  function ExistFile(FN : PathStr) : Boolean;
    {-True if file FN exists}
  var
    F : File;
  begin
    Assign(F, FN);
    Reset(F);
    ExistFile := (IOResult = 0);
    Close(F);
    if IOResult = 0 then ;
  end;

  procedure ShowHelp;
    {-Display cmd line params options and halt}
  begin
    WriteLn('Usage: SIMPSNDO [options] FaxFile');
    WriteLn('  /I StationID   Set Station ID');
    WriteLn('  /N Number      Set fax phone number to call');
    WriteLn('  /C ComPort     Set com port (1-4, default=1)');
    WriteLn('  /B Baud        Set port baud rate (4800, 7200, 9600, 19200; default=19200)');
    WriteLn('  /L LowBaud     Set low baud rate for modem init (same as above; default=none)'); {!!.01}
    WriteLn('  /F FaxBPS      Set fax BPS rate (2400, 4800, 7200, 9600, 12000, 14400;');
    WriteLn('                                   default=9600)');
    WriteLn('  /M InitString  Set modem initialization string');
    WriteLn('  /V CoverFile   Use cover page file');
    WriteLn('  /H             Use highest possible bit rate and ECM if available');
    WriteLn('  /S FaxClass    Set fax class (1, 2, A(uto), C(AS); default=2)');
    WriteLn('  /?             Display help screen');
    Halt(0);
  end;

  procedure MyFaxErrorProc(P : Pointer; var StatusCode : Word);
  var
    AP : AbstractPortPtr absolute P;
  begin
    case StatusCode mod 10000 of
      ecParityError,
      ecFramingError : {ignore} ;
      else WriteLn(^M^J'Error: ', StatusStr(StatusCode mod 10000));
    end;
  end;

  function MyFaxAbort : Boolean;
    {-Abort fax send process if user presses <ESC>}
  var
    C : Char;
  begin
    MyFaxAbort := False;
    if KeyPressed then begin
      C := ReadKey;
      if C = #27 then
        MyFaxAbort := True
      else begin
        if C = #0 then
          C := ReadKey;
      end;
    end;
  end;

  procedure MyFaxStatus(FP : AbstractFaxPtr; Starting, Ending : Boolean);
    {-Status display routine}
  const
    First : Boolean = True;
    LastProgress : Word = 65535;
    PrevBPS : LongInt = 0;                                             {!!.03}
  var
    L : LongInt;
    Pages : Word;
    Page : Word;
    Bytes : LongInt;
    Total : LongInt;
    BPS : LongInt;
    Res : Boolean;
    ECM : Boolean;
    Progress : Word;
  begin
    {Check for errors first}
    if AsyncStatus <> ecOk then
      {Do nothing, we'll display an error message after FaxTransmit aborts}
      Exit;

    {No errors, show current progress. Show most progress values only once}
    Progress := FP^.GetFaxProgress;
    if Progress <> LastProgress then begin
      First := True;
      if (LastProgress = fpBusyWait) or (LastProgress = fpSendPage) then
        WriteLn;
      LastProgress := Progress;
    end else
      First := False;

    {$IFNDEF AutoRetry}
    {Ignore open/close requests if in manual retry and fax is busy}
    if (Starting or Ending) and (Progress = fpBusyWait) then
      Exit;
    {$ENDIF}

    if Starting then begin
      WriteLn('--------------------------');
      WriteLn('Fax transmission beginning');
      LastProgress := 65535;
    end else if Ending then begin
      WriteLn;
      WriteLn('Fax transmission ended');
      WriteLn('----------------------');
    end else with C12SendFaxPtr(FP)^ do begin
      {Exit if we've already shown this status}
      if not First then
        case Progress of
          fpSendPage, fpBusyWait, fpSessionParams : ;                  {!!.03}
          else Exit;
        end;

      case Progress of
        fpWaiting :
          WriteLn('Waiting to process CAS send event');
        fpInitModem :
          WriteLn('Initializing fax modem');
        fpDialing :
          begin
            if GetFaxName = '' then
              WriteLn('Sending cover page only')
            else
              WriteLn('Fax file to send: ', GetFaxName);
            WriteLn('Dialing/waiting for connection');
          end;
        {$IFDEF AutoRetry}
        fpBusyWait:
          Write(^M'Busy, waiting for redial: ', RemainingTimeInSecs(ReplyTimer):2);
        {$ENDIF}
        fpGotRemoteID :
          WriteLn('Connected to ', GetRemoteID);
        fpSessionParams :
          begin
            GetSessionParams(BPS, Res, ECM);
            if (PrevBPS <> BPS) then begin                             {!!.03}
              Write('Session capabilities: ', BPS, ' bps');
              if Res then
                Write(',  high resolution')
              else
                Write(',  standard resolution');
              if ECM then
                WriteLn(',  ECM')
              else
                WriteLn(',  no ECM');
            end;                                                       {!!.03}
            PrevBPS := BPS;                                            {!!.03}
          end;
        fpSendPage :
          begin
            GetPageInfo(Pages, Page, Bytes, Total);
            case FaxClass of
              ctClass1, ctClass2 :
                begin
                  if Page = 0 then
                    Write(^M'Sending cover: ', Bytes)
                  else begin
                    if Total <> 0 then
                      L := Trunc(((Bytes * 1.0) / Total) * 100.0)
                    else
                      L := 0;
                    Write(^M'Sending page ', Page, ': ', Bytes, ' (',L,'%)');
                    ClrEol;
                  end;
                end;
              ctCAS :
                Write(^M'Pages sent, total bytes sent: ', Page, ', ', Bytes);
            end;
          end;
        fpSendPageStatus:
          WriteLn('Sending end-of-page');
        fpPageOK:
          WriteLn('Page accepted');
      end;
    end;
  end;

  procedure MyFaxLog(CP : AbstractFaxPtr;
                     Number : String;
                     FName : PathStr;
                     Log : TLogFaxCode);
  var
    FLog : Text;
  begin
    with CP^ do begin
      Assign(FLog, 'SIMPSNDO.HIS');
      Append(FLog);
      if IOResult = 2 then
        ReWrite(FLog);
      if IOResult <> 0 then
        Exit;
      case Log of
        lfaxTransmitStart :
          WriteLn(FLog, TodayString, ' ', NowString,
                        ' -- start fax transmit: ', FName,
                        '  to ', Number);
        lfaxTransmitOk :
          begin
            WriteLn(FLog, TodayString, ' ', NowString,
                          ' -- end fax transmit:', FName);
            WriteLn(FLog);
          end;
        lfaxTransmitFail :
          begin
            WriteLn(FLog, TodayString, ' ', NowString,
                          ' -- fail fax transmit(', AsyncStatus, '): ', FName);
            WriteLn(FLog);
          end;
      end;
      Close(FLog);
      if IOResult <> 0 then ;
    end;
  end;

  procedure OurSendFax;
    {-send the fax using special repeat-on-busy logic}

    procedure Wait30;
    var
      C  : Char;
      ET : EventTimer;
    begin
      NewTimer(ET, Secs2Tics(30));
      while not TimerExpired(ET) do begin
        if KeyPressed then begin
          C := Upcase(ReadKey);
          if C = #27 then begin
            AsyncStatus := ecUserAbort;
            Exit;
          end else if C = 'C' then
            Exit;
        end;
      end;
    end;

  begin
    with Sender^ do begin

      {Add single fax file info from command line data}
      AddFaxEntry(PNumber, FaxFile, CoverPage);

      {$IFDEF AutoRetry}
      FaxTransmit;
      {$ELSE}
      repeat
        FaxTransmit;
        if AsyncStatus = ecFaxBusy then begin
          WriteLn('Busy, waiting 30 seconds for redial (<ESC> aborts, <"C"> cycles): ');
          Wait30;
          WriteLn;
        end;
      until AsyncStatus <> ecFaxBusy;
      {$ENDIF}
    end;
  end;

  procedure ParseCmdLine;
    {-Get command line params and validate}
  var
    N : Integer;
    I : Integer;
    L : LongInt;
    S : String;

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
    if ParamCount = 0 then
      ShowHelp;

    I := 1;
    while I <= ParamCount do begin
      S := ParamStr(i);
      if S[1] = '?' then
        ShowHelp;

      if S[1] in ['-','/'] then begin
        case upcase(S[2]) of
          '?':
            ShowHelp;
          'I':  {station ID}
            StatID := GetDelimited;
          'N':  {number to dial}
            PNumber := NextS;
          'C':  {comm port to use}
            begin
              S := NextS;
              case S[1] of
                '1': CName := Com1;
                '2': CName := Com2;
                '3': CName := Com3;
                '4': CName := Com4;
                else ShowHelp;
               end;
            end;
          'B':  {baud}
            begin
              Val(NextS, L, N);
              if N = 0 then
                BRate := L;
            end;
          'L':  {lowbaud}                                              {!!.01}
            begin                                                      {!!.01}
              Val(NextS, L, N);                                        {!!.01}
              if N = 0 then                                            {!!.01}
                SendInitBaud := L;                                     {!!.01}
            end;                                                       {!!.01}
          'F':  {max fax bit rate}
            begin
              Val(NextS, L, N);
              if N = 0 then begin
                FaxBRate := Word(L);
                SetFaxBRate := True;
              end;
            end;
          'M':  {modem init string}
            MInit := NextS;
          'T':  {sender title}
            DefTitle := NextS;
          'V':  {cover page filename}
            CoverPage := NextS;
          'H' : {use highest features}
            SetFeatures := True;
          'S' : {set fax class}
            begin
              S := NextS;
              case Upcase(S[1]) of
                '1' : FaxClass := ctClass1;
                '2' : FaxClass := ctClass2;
                'A' : FaxClass := ctDetect;
                'C' : FaxClass := ctCAS;
                else ShowHelp;
              end;
            end;
          'Q' :
            Val(NextS, IrqNum, N);
          else begin
            WriteLn(^G'Unknown parameter "'+S+'"');
            ShowHelp;
          end;
        end;
      end else
        FaxFile := S;

      Inc(I);
    end;

    if (FaxFile = '') and (CoverPage = '') then
      ShowHelp
    else begin
      if FaxClass <> ctCAS then
        FaxFile := ForceExtension(FaxFile, 'APF');
      if not ExistFile(FaxFile) then
        AbortMsg('Specified fax file not found');
    end;
  end;

  {$IFDEF TestStreams}
  procedure CreateStream;
    {-Store Sender to stream}
  var
    S : BufIdStream;
    Status : Word;
  begin
    WriteLn('Creating stream file');
    {Create a new stream}
    if not S.Init('SEND.STM', SCreate, 1024) then
      AbortMsg('Failed to make stream ');

    {Register all hierarchies}
    S.RegisterHier(C12SendFaxStream);
    S.RegisterHier(CasFaxStream);
    Status := S.GetStatus;
    if Status <> 0 then
      AbortMsg('Error registering object ');

    {Register our user procedures}
    S.RegisterPointer(1000, @MyFaxStatus);
    S.RegisterPointer(1001, @MyFaxLog);

    {Register port hierarchy}
    S.RegisterHier(UartPortStream);
    S.RegisterPointer(ptErrorProc, @MyFaxErrorProc);
    S.RegisterPointer(ptAbortProc, @MyFaxAbort);
    Status := S.GetStatus;
    if Status <> 0 then
      AbortMsg('Error registering object');

    {Store the object}
    S.PutPtr(Sender);
    Status := S.GetStatus;
    if Status <> 0 then
      AbortMsg('Error storing');

    {Clean up}
    S.Done;
  end;

  procedure LoadStream;
    {-Load Sender from a stream}
  var
    S : BufIdStream;
    Status : Word;
  begin
    WriteLn('Loading stream file');

    {Re-open existing new stream}
    if not S.Init('SEND.STM', SOpen, 1024) then
      AbortMsg('Failed to open stream');

    {Register all hierarchies}
    S.RegisterHier(C12SendFaxStream);
    Status := S.GetStatus;
    if Status <> 0 then
      AbortMsg('Error registering object');

    {Register our user procedures}
    S.RegisterPointer(1000, @MyFaxStatus);
    S.RegisterPointer(1001, @MyFaxLog);

    {Register port hierarchy}
    S.RegisterHier(UartPortStream);
    S.RegisterPointer(ptErrorProc, @MyFaxErrorProc);
    S.RegisterPointer(ptAbortProc, @MyFaxAbort);
    Status := S.GetStatus;
    if Status <> 0 then
      AbortMsg('Error registering object');

    {Load the object}
    RootPtr(Sender) := S.GetPtr;
    Status := S.GetStatus;
    if Status <> 0 then
      AbortMsg('Error loading converter');

    {Clean up}
    S.Done;
  end;
  {$ENDIF}

  procedure SendFaxCAS;
    {-Submit file to CAS manager}
  begin
    {Init fax object}
    Sender := New(CASFaxPtr, Init(StatID));
    if Sender = nil then
      AbortMsg('Could not initialize CAS fax object, status = ' +
                Long2Str(AsyncStatus));

    {Set an abort hook just for CAS}
    with CASFaxptr(Sender)^ do begin
      SetCASAbortFunc(MyFaxAbort);
      SetFaxStatusProc(MyFaxStatus);
      SetFaxLogProc(MyFaxLog);
    end;

    {Uncomment this line to have SubmitSingleFile use a control file}
    Sender^.afOptionsOn(afCASSubmitUseControl);

    OurSendFax;

    Dispose(Sender, Done);
  end;

  procedure SendFaxClass12;
    {-Send faxes with APRO's class 1/2 engine}
  begin
    {$IFDEF Tracing}
    InitTracing(32000);
    if AsyncStatus <> ecOk then
      AbortMsg('Failed to start trace');
    {$ENDIF}

    {!!.03 - Added}
    {$IFDEF EventLogging}
    InitEventLogging(10000);
    if AsyncStatus <> ecOk then
      AbortMsg('Failed to start event log');
    {$ENDIF}

    {Init port}
    if IrqNum <> 0 then begin
      SetUart(CName, 0, IrqNum, IrqNum+8);
      if (CName = Com3) or (CName = Com4) then
        PS2DetectMode := PS2Ignore;
    end;
    CPort := New(UartPortPtr, InitCustom(CName, BRate, NoParity, 8, 1,
                                         2048, 8192, DefPortOptions));
    if CPort = nil then
      AbortMsg('Unable to open port, status = '+Long2Str(AsyncStatus));

    {$IFDEF FaxHWFlow}
    {Modem may want to flow control us (but we can't flow control it)}
    CPort^.HWFlowEnable(2000, 50, hfRequireCTS or hfUseRTS);
    if AsyncStatus <> ecOk then
      AbortMsg('Failed to set hardware flow control, status = '+Long2Str(AsyncStatus));
    {$ENDIF}

    {Init fax object}
    Sender := New(C12SendFaxPtr, Init(StatID, CPort));
    if Sender = nil then
      AbortMsg('Could not initialize fax send engine, status = '+Long2Str(AsyncStatus));

    {Set up fax options}
    with C12SendFaxPtr(Sender)^ do begin
      CPort^.SetAbortFunc(MyFaxAbort);
      CPort^.SetErrorProc(MyFaxErrorProc);
      SetFaxStatusProc(MyFaxStatus);
      SetHeaderText(Header);
      SetTitle(DefTitle);
      SetModemInit(MInit);
      SetFaxLogProc(MyFaxLog);
      {$IFDEF AutoRetry}
      SetConnectAttempts(3, 1092);
      {$ELSE}
      SetConnectAttempts(1, 1092);
      afOptionsOn(afAbortNoConnect);
      {$ENDIF}
      if SendInitBaud <> 0 then                                        {!!.01}
        SetInitBaudRate(SendInitBaud, 0);                              {!!.01}

      {Force an exit on errors, since we're only sending one fax}
      afOptionsOn(afExitOnError);
    end;

    {$IFDEF TestStreams}
    CreateStream;
    Dispose(Sender, Done);
    LoadStream;
    {$ENDIF}

    with C12SendFaxPtr(Sender)^ do begin
      {Make sure modem supports class 1/2}
      WriteLn('Identifying modem...');
      if GetModemInfo(Class, Model, Chip, Rev, True) then begin
        if Class = 'B' then                                            {!!.02}
          WriteLn('Highest class: 2.0')                                {!!.02}
        else                                                           {!!.02}
          WriteLn('Highest class: ', Class);
        WriteLn('Chip: ', Chip);
        WriteLn('Model: ', Model);
        WriteLn('Revision: ', Rev);
        WriteLn;
        if Class = '1' then
          FaxClass := ctClass1;
      end else
        AbortMsg('Unable to identify modem');

      {Set desired class}
      FaxClass := SetClassType(FaxClass);
      case FaxClass of
        ctClass1 : WriteLn('Fax class is set to Class 1');
        ctClass2 : WriteLn('Fax class is set to Class 2');
        ctCAS    : WriteLn('Fax class is CAS');
        else       AbortMsg('Failed to set fax class');
      end;

      {Test modem features and set best possible compression and error checking}
      if FaxClass = ctClass2 then begin
        WriteLn('Testing modem features...');
        GetModemFeatures(BitRate, Check);
        WriteLn('Max bits/second: ', BitRate, ',   Error checking: ', Check);
        if SetFeatures then
          SetModemFeatures(FaxBRate, Check);
      end;

      {Check for request to set fax bitrate}
      if SetFaxBRate then
        SetModemFeatures(FaxBRate, '0');

      {Send the fax}
      OurSendFax;
      case AsyncStatus of
        ecOk               : WriteLn('Fax transmit session complete');
        ecTimeout          : WriteLn('Timeout waiting for data');
        ecUserAbort        : WriteLn('Fax aborted by user');
        ecFaxVoiceCall     : WriteLn('Voice answered phone');
        ecFaxDataCall      : WriteLn('Data modem answered phone');
        ecFaxBusy          : WriteLn('Line was busy');
        ecFaxNoFontFile    : WriteLn('Font file not found');
        ecFaxInitError     : WriteLn('Error initializing modem');
        ecFaxTrainError    : WriteLn('Error during modem training');
        ecFaxSessionError : begin
                              WriteLn('Error during session');
                              WriteLn('Hangup result was ', GetHangupResult);
                            end;
        else                WriteLn('Unknown error: ', AsyncStatus);
      end;
    end;

    {Cleanup}
    Dispose(Sender, Done);
    Dispose(CPort, Done);

    {$IFDEF Tracing}
    DumpTracehex('SIMPSNDO.TRC');
    {$ENDIF}
  end;

begin
  WriteLn(CopyrightLine);
  Sender := nil;                                                       {!!.01}
  CPort := nil;                                                        {!!.01}

  {Set stdout for output}
  Assign(Output, '');
  Rewrite(Output);

  {Get parameters}
  ParseCmdLine;

  if FaxClass = ctCAS then
    SendFaxCAS
  else
    SendFaxClass12;
end.
