{$V-,B-,I-}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}
{$X+}
{*********************************************************}
{*                   OOMODEM2.PAS 2.03                   *}
{*     Copyright (c) TurboPower Software 1995.           *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OoModem2;
  {-Modem access}

interface

{$DEFINE DispatchDebug}

uses
  {$IFDEF UseTPro}
  TpMemChk,
  TpDos,
  TpString,
  {$ENDIF}
  {$IFDEF UseOPro}
  OpRoot,
  OpDos,
  OpString,
  {$ENDIF}
  ApMisc,
  ApPort,
  ApTimer,
  OoCom,
  OoModDB;

const
  {default values}
  DefDialTimeout       = 60;     {Default seconds for dial timeout}
  DefAnswerTimeout     = 60;     {Default seconds for answer timeout}
  DefDelayFactor       = 2;      {Default Tics for inter-cmd delay}
  DefCmdTimeout        = 182;    {Default Tics for command timeout (10 secs)}
  DefDTRDropHold       = 8;      {Default Tics for DTR low during hangup}
  DefModemCharDelay    = 0;      {Default Tics between each outgoing cmd char}
  DefTildeDelay        = 9;      {Default Tics to delay for ~'s in cmd Strings}
  DefRingWaitTimeout   = 182;    {Default Tics before auto answer resets}
  DefFeatureWait       = 9;      {Default number of Tics to wait for features}
  DefBaudWait          = 36;     {Default number of Tics to wait for a BPS rate}
  TicsPerSec           = 18;     {Default number of Tics in a second}

  {response constants}
  NumResponses         = 8;
  NumWaitStrs          = NumResponses + 2 * MaxTags;
  RspOK                = 1;
  RspConnect           = 2;
  RspBusy              = 3;
  RspVoice             = 4;
  RspNoCarrier         = 5;
  RspNoDialTone        = 6;
  RspError             = 7;
  RspRing              = 8;
  RspErrorCorr1        = 9;
  RspErrorCorrLast     = RspErrorCorr1 + MaxTags - 1;
  RspDataComp1         = RspErrorCorrLast + 1;
  RspDataCompLast      = RspDataComp1 + MaxTags - 1;

  CmdSepChar           = '|'; {Character to separate multiple commands}
  TentBaudLen          = 13;  {Maximum length of a baud rate string}

type
  {set to handle response codes}
  ModemResponseSet = set of Byte;

  {pointer to string}
  PStr = ^String;

  {string type for baud rate}
  TTentBaudStr = String[TentBaudLen];

  {record for holding response data}
  WaitStringRec = record
    WaitStr : PStr;
    Enabled : Boolean;
    Idx     : Byte;
  end;

  {array for holding response strings}
  WaitStringArray = array [1..(NumWaitStrs)] of WaitStringRec;

  {object for holding modem data and handling modem commands}
  ModemPtr = ^Modem;
  ModemStatusProc = procedure(P : ModemPtr; MsgType, SecsRemaining : Word);

  Modem =
    object
      InitCmd         : PStr;
      DialCmd         : PStr;
      DialTerm        : PStr;
      DialCancel      : PStr;
      HangupCmd       : PStr;
      ConfigCmd       : PStr;
      AnswerCmd       : PStr;
      WaitStrings     : WaitStringArray;
      DialTimeout     : Word;
      AnswerTimeout   : Word;
      DelayFactor     : Word;
      CmdTimeout      : Word;
      DTRDropHold     : Word;
      ModemCharDelay  : Word;
      TildeDelay      : Word;
      RingWaitTimeout : Word;
      FeatureWait     : Word;
      BaudWait        : Word;
      TicsLeft        : Word;
      ConnectSpeed    : LongInt;
      ResponseTimer   : EventTimer;
      P               : AbstractPortPtr;
      DisplayStatus   : ModemStatusProc;
      LockDTE         : Boolean;
      ErrorCorrection : Boolean;
      DataCompression : Boolean;
      ModemBusy       : Boolean;
      LastECNum       : Byte;
      LastDCNum       : Byte;
      LastField       : record end;

      constructor Init(Port : AbstractPortPtr; var Data : ModemData);
        {-Initialize a modem}
      destructor Done; virtual;
        {-Destroy a modem}

      procedure SetDialTimeout(Secs : Word);
        {-Set the number of seconds before a dial attempt times out}
      procedure SetAnswerTimeout(Secs : Word);
        {-Set the number of seconds before an answer attempt times out}
      procedure SetDelayFactor(Tics : Word);
        {-Set the number of Tics to wait between commands sent to the modem}
      procedure SetCmdTimeout(Tics : Word);
        {-Set the number of Tics to wait for a modem response}
      procedure SetDTRDropHold(Tics : Word);
        {-Set the number of Tics to hold DTR low during hangup}
      procedure SetCharDelay(Tics : Word);
        {-Set the number of Tics to wait between each command character sent}
      procedure SetTildeDelay(Tics : Word);
        {-Set the number of Tics to wait when a '~' is encountered in a command}
      procedure SetRingWaitTimeout(Tics : Word);
        {-Set the number of Tics to wait before mAutoAnswerModem resets}
      procedure SetStatusProc(MSP : ModemStatusProc);
        {-Set the DisplayStatus procedure to procedure SP}

      procedure ShowStatus(MsgType, SecsRemaining : Word); virtual;
        {-Virtual method to be overriden by user-defined procedure which
         deals with the status information sent to it}

      procedure PutCommand(Cmd : String; Wait : Boolean);
        {-Send a command to the modem}
      procedure Initialize(Wait : Boolean);
        {-Send the initialization string to the modem}
      procedure Configure;
        {-Send the configuration Strings to the modem}
      function NumConfigStrings : Word;
        {-Get the number of command strings in the configuration string}
      function GetConfigString(StrNum : Word) : CmdStrType;
        {-Get a command string from the configuration string}
      procedure HangUp(Wait : Boolean);
        {-Send the hangup String to the modem}
      function ProcessCommandResponse : Boolean;
        {-Process command responses from modem}
      procedure ProcessConnectResponse;
        {-Process connection responses from modem}
      procedure Dial(Number : String; Wait : Boolean);
        {-Dial the modem, waiting for response or timeout}
      procedure SendCancel;
        {-Sends the cancel string to the modem}
      function GetConnectSpeed : LongInt;
        {-Get the actual speed of the connection}
      function GetDataCompression : Boolean;
        {-Returns the value of data compression}
      function GetErrorCorrection : Boolean;
        {-Returns the value of error correction}
      function ModemWorking : Boolean;
        {-Sees if the modem is processing a command}
      procedure Answer(Wait : Boolean);
        {-Answer the modem}
      procedure AutoAnswer(Rings : Word);
        {-Answer the modem after Rings rings}

      {+++internal+++}
      function mGetDialTimeout : Word;
      function mCheckWaitStrs : Byte;
      function mTimedCheckWaitStrs(CheckTics : Word) : Byte;
      procedure mDoneModemDynamic;
      procedure mDialAnswer(Number : String; IsDialing : Boolean;
                            Wait : Boolean);
      procedure mEnableWaitStrs(StrsToEnable : ModemResponseSet);
      procedure mDisableWaitStrs(StrsToDisable : ModemResponseSet);
      function mHandleCommandResponse(Wait : Boolean) : Boolean;
      procedure mHandleConnectionAttemptResponse(Dialing : Boolean;
                                                 Wait   : Boolean);
      procedure mEnableFeatureTags;
      procedure mDisableFeatureTags;
      procedure mDisableErrorTags;
      procedure mDisableCompressTags;
      procedure mPutXlatStr(Str : String);
      procedure mWaitForRing(NumRings : Word);
    end;

implementation

const
  RspWaitSet  = [RspOK, RspError];
  DialWaitSet = [RspConnect, RspBusy, RspVoice, RspNoCarrier, RspNoDialTone,
                 RspError];
{$IFDEF DispatchDebug}
  DebugFileName = 'debug.rpt';

var
  DebugFile : Text;

  function Long2Str(L : LongInt) : string;
    {-Convert a long/word/integer/byte/shortint to a string}
  var
    S : string;
  begin
    Str(L, S);
    Long2Str := S;
  end;
{$ENDIF}

  {non-public modem routines}
{$IFNDEF UseOPro}
  {$IFNDEF UseTPro}
  function WordCount(S : string; WordDelims : CharSet) : Byte;
    {-Given a set of word delimiters, return number of words in S}
  var
    {I,} Count : Byte;
    I : Word;
    SLen : Byte absolute S;
  begin
    Count := 0;
    I := 1;

    while I <= SLen do begin
      {skip over delimiters}
      while (I <= SLen) and (S[I] in WordDelims) do
        Inc(I);

      {if we're not beyond end of S, we're at the start of a word}
      if I <= SLen then
        Inc(Count);

      {find the end of the current word}
      while (I <= SLen) and not(S[I] in WordDelims) do
        Inc(I);
    end;

    WordCount := Count;
  end;
  {$ENDIF}
{$ENDIF}

  function StrNewCheck(var NewSt : PStr; SrcStr : String) : Boolean;
    {-Allocate a new String on the heap, checking for available memory}

  begin
    if GetMemCheck(NewSt, Length(SrcStr) + 1) then begin
      StrNewCheck := True;
      NewSt^ := SrcStr;
    end else
      StrNewCheck := False;
  end;

  procedure StrDisposeCheck(var St : PStr);
  begin
    FreeMemCheck(St, Length(St^) + 1);
  end;

  procedure Modem.mDoneModemDynamic;
    {-Dispose of all dynamic modem data}
  var
    I : Word;

  begin
    StrDisposeCheck(InitCmd);
    StrDisposeCheck(DialCmd);
    StrDisposeCheck(DialTerm);
    StrDisposeCheck(DialCancel);
    StrDisposeCheck(HangupCmd);
    StrDisposeCheck(ConfigCmd);
    StrDisposeCheck(AnswerCmd);

    for I := 1 to NumWaitStrs do
      if WaitStrings[I].WaitStr <> nil then
        StrDisposeCheck(WaitStrings[I].WaitStr);
  end;

{$IFDEF DispatchDebug}

  procedure DebugOut(DebugStr : String);
    {-Writes the String to the debug file}

  begin
    Assign(DebugFile, DebugFileName);
    if not ExistFile(DebugFileName) then
      Rewrite(DebugFile)
    else
      Append(DebugFile);
    Writeln(DebugFile, DebugStr);
    if IOResult = 0 then ;
    Close(DebugFile);
    if IOResult = 0 then ;
  end;

{$ENDIF}
  procedure Modem.mEnableWaitStrs(StrsToEnable : ModemResponseSet);
    {-Enable one or more modem responses and set their indexes to 0}
  var
    I : Word;

  begin
    for I := 1 to NumResponses do
      if I in StrsToEnable then
        if (WaitStrings[I].WaitStr <> nil) then begin
          WaitStrings[I].Enabled := True;
          WaitStrings[I].Idx := 0;
        end;
  end;

  procedure Modem.mDisableWaitStrs(StrsToDisable : ModemResponseSet);
    {-Disable one or more modem responses}
  var
    I : Word;

  begin
    for I := 1 to NumResponses do
      if I in StrsToDisable then
        WaitStrings[I].Enabled := False;
  end;

  procedure Modem.mEnableFeatureTags;
  var
    I : Word;

  begin
    for I := RspErrorCorr1 to LastDCNum do begin
      WaitStrings[I].Enabled    := True;
      WaitStrings[I].Idx        := 0;
    end;
  end;

  procedure Modem.mDisableErrorTags;
  var
    I : Word;

  begin
    for I := RspErrorCorr1 to LastECNum do begin
      WaitStrings[I].Enabled := False;
      WaitStrings[I].Idx     := 0;
    end;
  end;

  procedure Modem.mDisableCompressTags;
  var
    I : Word;

  begin
    for I := RspDataComp1 to LastDCNum do
      WaitStrings[I].Enabled := False;
  end;

  procedure Modem.mDisableFeatureTags;

  begin
    mDisableErrorTags;
    mDisableCompressTags;
  end;

  procedure Modem.mPutXlatStr(Str : String);
    {-Send a String to the modem, XLATing control chars}
  var
    I     : Word;
    Len   : Word;
    Delay : Boolean;

  begin
    AsyncStatus := ecOK;
    Len := Length(Str);
    if (Len = 0) then
      Exit;

    I := 1;
    Delay := True;
    while (I <= Len) do begin
      {convert special characters}
      case Str[I] of
        '^':
          if (I <> Len) and (UpCase(Str[I+1]) in ['@'..'_']) then begin
            Inc(I);
            P^.PutChar(Char(Ord(UpCase(Str[I])) - Ord('A') + 1));
          end else
            P^.PutChar(Str[I]);
        '~':
          begin
            DelayTics(TildeDelay);
            Delay := False;
          end;
        else
          P^.PutChar(Str[I]);
      end;

      if (AsyncStatus <> ecOK) then
        Exit;

      Inc(I);

      if Delay and (I <> Len) then
        if (ModemCharDelay <> 0) then
          DelayTics(ModemCharDelay);
    end;
    ModemBusy := True;
    {$IFDEF DispatchDebug}
    DebugOut('Command sent: ' + Str);
    {$ENDIF}
  end;

  function Modem.mTimedCheckWaitStrs(CheckTics : Word) : Byte;
  var
    I          : Byte;
    CheckTimer : EventTimer;

  begin
    AsyncStatus := ecOK;
    NewTimer(CheckTimer, CheckTics);
    repeat
      I := mCheckWaitStrs;
      mTimedCheckWaitStrs := I;
      if I <> 0 then
        TicsLeft := RemainingTime(CheckTimer);
    until TimerExpired(CheckTimer) or (AsyncStatus = ecUserAbort) or
      (I <> 0);

    if (TimerExpired(CheckTimer)) and (I = 0) then
      AsyncStatus := ecTimeOut;
  end;

  function Modem.mCheckWaitStrs : Byte;
    {-Checks to see if we got a String we're looking for by taking chars from
     the buffer.}
  var
    I  : Word;
    Ch : Char;

  begin
    mCheckWaitStrs := 0;
    while P^.CharReady do begin
      P^.GetChar(Ch);
      {$IFDEF DispatchDebug}
      DebugOut('Got a character : ' + Ch);
      {$ENDIF DispatchDebug}
      if AsyncStatus = ecOK then
        for I := 1 to NumWaitStrs do
          if WaitStrings[I].WaitStr <> nil then
            with WaitStrings[I] do
              if Enabled then
                if CheckForString(Idx, Ch, WaitStr^, True) then begin
                  mCheckWaitStrs := I;

                  {$IFDEF DispatchDebug}
                  DebugOut('Current string is ' + WaitStrings[I].WaitStr^);
                  {$ENDIF}

                  if (I < RspErrorCorr1) then
                    ModemBusy := False;
                  Exit;
                end;

      P^.WaitChar(P, Ch);
    end;
    if P^.UserAbort then begin
      {$IFDEF DispatchDebug}
      DebugOut('User pressed abort');
      {$ENDIF DispatchDebug}
      AsyncStatus := ecUserAbort;
      ShowStatus(ecUserAbort, TicsLeft);
      Exit;
    end;
  end;

  function Modem.mHandleCommandResponse(Wait : Boolean) : Boolean;
    {-Wait for an OK or ERROR response from the modem}
  var
    RspNum : Byte;

  begin
    mHandleCommandResponse := False;
    {set AsyncStatus according to what String we get back}
    if Wait then
      RspNum := mTimedCheckWaitStrs(CmdTimeOut)
    else
      RspNum := mCheckWaitStrs;
    if AsyncStatus = ecOK then begin
      case RspNum of
        RspOK :
          begin
            AsyncStatus := ecOK;
            mHandleCommandResponse := True;
          end;
        RspError :
          begin
            AsyncStatus := ecError;
            mHandleCommandResponse := True;
          end;
      end;

      if (RspNum <> 0) then
        mDisableWaitStrs(RspWaitSet);
    end;
  end;

  procedure Modem.mHandleConnectionAttemptResponse(Dialing : Boolean;
                                                   Wait   : Boolean);
    {-The modem is attempting to connect, be it via an answer or
      a dial.  This routine processes the responses that can
      be returned during a connect attempt}
  var
    TimeLeft : Word;
    ChkNum   : Byte;

    procedure CheckForTags(TagNum : Byte);
      {-checks to see if the character is in an error correction or
       data compression tag}

    begin
      if TagNum in [RspErrorCorr1..LastECNum] then begin
        mDisableErrorTags;
        ErrorCorrection := True;
        ShowStatus(ecGotErrorCorrection, TimeLeft);
      end;

      if TagNum in [RspDataComp1..LastDCNum] then begin
         mDisableCompressTags;
         DataCompression := True;
         ShowStatus(ecGotDataCompression, TimeLeft);
       end;
    end;

    procedure AbortConnectAttempt(SendCancel : Boolean);
      {-Disable responses and feature tags and then optionally send the cancel
        command to the modem}

    begin
        {$IFDEF DispatchDebug}
        DebugOut('Aborting connect attempt');
        {$ENDIF}

        {-send the cancel command to the modem if necessary}
        if SendCancel then begin
          DelayTics(DelayFactor * 2);
          mPutXlatStr(DialCancel^);
          DelayTics(DelayFactor);
        end;

        ModemBusy := False;
        mDisableWaitStrs(DialWaitSet);
        mDisableFeatureTags;
        ConnectSpeed := 0;
    end;

    procedure EstablishConnection(var TimeLeft : Word);
      {-After we've connected, we start waiting for the baud rate and any
        remaining feature tags}
    var
      I            : Word;
      Started      : Boolean;
      GotBaudRate  : Boolean;
      Code         : Word;
      CurrBaud     : LongInt;
      CurrDataBits : DataBitType;
      CurrStopBits : StopBitType;
      CurrParity   : ParityType;
      Ch           : Char;
      BaudTimer    : EventTimer;
      FeatureTimer : EventTimer;
      ChkNum       : Byte;
      TentBaudRate : TTentBaudStr;

    begin
      {$IFDEF DispatchDebug}
      DebugOut('Establishing a connection');
      {$ENDIF}
      Started := False;
      GotBaudRate := False;
      mDisableWaitStrs(DialWaitSet);

      {start waiting for baud rate}
      TentBaudRate := '';
      NewTimer(BaudTimer, BaudWait);

      repeat
        if P^.CharReady then begin
          P^.GetChar(Ch);
          {$IFDEF DispatchDebug}
          DebugOut('Got a character : ' + Ch);
          {$ENDIF DispatchDebug}

          if not Started then begin
            if (Ch in ['0'..'9']) then begin
              TentBaudRate := Ch;
              Started := True;
            end;
          end else
            if not (Ch in ['0'..'9']) then
              GotBaudRate := True
            else
              TentBaudRate := TentBaudRate + Ch;
        end;
        if AsyncStatus = ecTimeOut then
          Exit;
        if P^.UserAbort then begin
          {$IFDEF DispatchDebug}
          DebugOut('User pressed abort');
          {$ENDIF DispatchDebug}
          AsyncStatus := ecUserAbort;
          ShowStatus(ecUserAbort, RemainingTimeInSecs(BaudTimer));
          Exit;
        end;
      until GotBaudRate or (TimerExpired(BaudTimer));

      {convert baud rate and inform user that we got it}
      if TentBaudRate <> '' then begin
        Val(TentBaudRate, ConnectSpeed, Code);
        ShowStatus(ecGotBaud, TimeLeft);
      end;

      {change the line speed if LockDTE is false}
      if not LockDTE then begin
        P^.GetLine(CurrBaud, CurrParity, CurrDataBits, CurrStopBits, True);
        if not GotBaudRate then
          P^.SetLine(300, CurrParity, CurrDataBits, CurrStopBits)
        else
          P^.SetLine(ConnectSpeed, CurrParity, CurrDataBits, CurrStopBits);
      end;

      {check to see if any error correction or data comp tags came just now}
      for I := RspErrorCorr1 to LastDCNum do
        WaitStrings[I].Idx := 0;
      NewTimer(FeatureTimer, FeatureWait);

      repeat
        ChkNum := mTimedCheckWaitStrs(FeatureWait);
        if AsyncStatus = ecOK then
          CheckForTags(ChkNum);
      until (ErrorCorrection and DataCompression) or (TimerExpired(FeatureTimer));
    end;

    procedure ProcessResults;

    begin
      if AsyncStatus = ecOK then
        case ChkNum of
          RspConnect:
            begin
              ShowStatus(ecConnect, TimeLeft);
              EstablishConnection(TimeLeft);
              AsyncStatus := ecConnect;
            end;

          RspBusy:
            begin
              AbortConnectAttempt(True);
              AsyncStatus := ecBusy;
              ShowStatus(ecBusy, TimeLeft);
            end; {RspBusy}

          RspVoice:
            begin
              AbortConnectAttempt(False);
              AsyncStatus := ecVoice;
              ShowStatus(ecVoice, TimeLeft);
            end; {RspVoice}


          RspNoCarrier:
            begin
              AbortConnectAttempt(False);
              AsyncStatus := ecNoCarrier;
              ShowStatus(ecNoCarrier, TimeLeft);
            end; {RspNoCarrier}

          RspNoDialTone:
            begin
              AbortConnectAttempt(False);
              AsyncStatus := ecNoDialTone;
              ShowStatus(ecNoDialTone, TimeLeft);
            end; {RspNoDialTone}

          RspError:
            begin
              AbortConnectAttempt(False);
              AsyncStatus := ecError;
              ShowStatus(ecError, TimeLeft);
            end; {RspError}

          else
            CheckForTags(ChkNum);
        end; {case ResponseCode}
    end;

  begin
    AsyncStatus := ecOK;
    {initialize the time remaining and the done variable before entering loop}
    if Wait then begin
      if Dialing then
        TimeLeft := DialTimeOut
      else
        TimeLeft := AnswerTimeOut;

      {$IFDEF DispatchDebug}
      DebugOut('Starting the repeat loop with connection attempt.');
      {$ENDIF DispatchDebug}
      repeat
        ChkNum := mTimedCheckWaitStrs(TicsPerSec);
        ProcessResults;
        if AsyncStatus = ecTimeOut then begin
          Dec(TimeLeft);
          ShowStatus(ecTimeUpd, TimeLeft);
          AsyncStatus := ecOK;
        end;
      until (AsyncStatus <> ecOK) or (TimeLeft = 0);

      if TimeLeft = 0 then
        AsyncStatus := ecTimeOut;
    end else begin
      ChkNum := mCheckWaitStrs;
      ProcessResults;
    end;

    if AsyncStatus = ecUserAbort then begin
      AbortConnectAttempt(True);
      AsyncStatus := ecUserAbort;
    end;
  end;

  procedure Modem.mWaitForRing(NumRings : Word);
    {-Waits until NumRings rings come in, then tries to answer}
  var
    RingWait  : Word;
    CountDown : EventTimer;
    RspNum    : Byte;
    Finished  : Boolean;
    {$IFDEF DispatchDebug}
    DidDebug  : Boolean;
    {$ENDIF}

  begin
    RingWait := NumRings;
    Finished := False;
    {$IFDEF DispatchDebug}
    DebugOut('Now entering AutoAnswer mode...');
    {$ENDIF DispatchDebug}

    repeat
      RspNum := mTimedCheckWaitStrs(RingWait);
      if AsyncStatus = ecOK then
        case RspNum of
          RspRing :
            begin
              {$IFDEF DispatchDebug}
              DebugOut('Got a RING');
              DidDebug := False;
              {$ENDIF}
              Dec(RingWait);
              NewTimer(CountDown, RingWaitTimeOut);

              if (RingWait = 0) then begin
                {$IFDEF DispatchDebug}
                DebugOut('Got the LAST ring, starting answer');
                {$ENDIF}
                mDisableWaitStrs([RspRing]);
                Answer(True);
                if AsyncStatus = ecConnect
                  then Finished := True;
              end;
            end;
          RspError :
            begin
              AsyncStatus := ecError;
              Finished := True;
            end;
        end
      else
        if AsyncStatus = ecUserAbort then
          Finished := True;
      if TimerExpired(CountDown) then begin
        {$IFDEF DispatchDebug}
        if not DidDebug then begin
          DebugOut('Resetting dial count...');
          DidDebug := True;
        end;
        {$ENDIF}
        RingWait := NumRings;
      end;
    until Finished;

  end;
{******************************************************************************}

  constructor Modem.Init(Port : AbstractPortPtr; var Data : ModemData);
    {-Initialize the modem}
  var
    I : Word;

  begin
    FillChar(InitCmd, Ofs(LastField) - Ofs(InitCmd), 0);

    {initialize pointers in structure}
    if not StrNewCheck(InitCmd, Data.Data.InitCmd) or
       not StrNewCheck(DialCmd, Data.Data.DialCmd) or
       not StrNewCheck(DialTerm, Data.Data.DialTerm) or
       not StrNewCheck(DialCancel, Data.Data.DialCancel) or
       not StrNewCheck(HangupCmd, Data.Data.HangupCmd) or
       not StrNewCheck(ConfigCmd, Data.Data.ConfigCmd) or
       not StrNewCheck(AnswerCmd, Data.Data.AnswerCmd) or
       not StrNewCheck(WaitStrings[RspOk].WaitStr, Data.Data.OkMsg) or
       not StrNewCheck(WaitStrings[RspConnect].WaitStr, Data.Data.ConnectMsg) or
       not StrNewCheck(WaitStrings[RspBusy].WaitStr, Data.Data.BusyMsg) or
       not StrNewCheck(WaitStrings[RspVoice].WaitStr, Data.Data.VoiceMsg) or
       not StrNewCheck(WaitStrings[RspNoCarrier].WaitStr, Data.Data.NoCarrierMsg) or
       not StrNewCheck(WaitStrings[RspNoDialTone].WaitStr, Data.Data.NoDialToneMsg) or
       not StrNewCheck(WaitStrings[RspError].WaitStr, Data.Data.ErrorMsg) or
       not StrNewCheck(WaitStrings[RspRing].WaitStr, Data.Data.RingMsg) then begin
      mDoneModemDynamic;
      AsyncStatus := ecOutOfMemory;
      Fail;
    end;

    {allocate memory for data compression and error correction Strings}
    LastECNum := RspErrorCorr1 + Data.NumErrors - 1;
    for I := RspErrorCorr1 to LastECNum do
      if not StrNewCheck(WaitStrings[I].WaitStr, Data.Errors[(I - RspErrorCorr1 + 1)]) then begin
        mDoneModemDynamic;
        AsyncStatus := ecOutOfMemory;
        Fail;
      end;

    LastDCNum := RspDataComp1 + Data.NumComps - 1;
    for I := RspDataComp1 to LastDCNum do
      if not StrNewCheck(WaitStrings[I].WaitStr, Data.Compression[I - RspDataComp1 + 1]) then begin
        mDoneModemDynamic;
        AsyncStatus := ecOutOfMemory;
        Fail;
      end;

    {assign fields to default values}
    DialTimeout     := DefDialTimeout;
    AnswerTimeout   := DefAnswerTimeout;
    DelayFactor     := DefDelayFactor;
    CmdTimeout      := DefCmdTimeout;
    DTRDropHold     := DefDTRDropHold;
    ModemCharDelay  := DefModemCharDelay;
    TildeDelay      := DefTildeDelay;
    RingWaitTimeout := DefRingWaitTimeout;
    FeatureWait     := DefFeatureWait;
    BaudWait        := DefBaudWait;
    LockDTE         := Data.LockDTE;
    ErrorCorrection := False;
    DataCompression := False;
    ConnectSpeed    := 300;

    AsyncStatus := ecOK;
    P := Port;
    @DisplayStatus := nil;

{$IFDEF DispatchDebug}
    Assign(DebugFile, DebugFileName);
    Erase(DebugFile);
    if IOResult = 0 then ;
    Close(DebugFile);
    if IOResult = 0 then ;
{$ENDIF}
  end;

  destructor Modem.Done;
    {-Destroy a modem}
  begin
    mDoneModemDynamic;
  end;

  procedure Modem.SetDialTimeout(Secs : Word);
    {-Set the number of seconds before a dial attempt times out}
  begin
    DialTimeOut := Secs;
  end;

  function Modem.mGetDialTimeout : Word;
    {-Get the number of seconds before the modem aborts a dial attempt}
  begin
    mGetDialTimeOut := DialTimeOut;
  end;

  procedure Modem.SetAnswerTimeout(Secs : Word);
    {-Set the number of seconds before an answer attempt times out}
  begin
    AnswerTimeOut := Secs;
  end;

  procedure Modem.SetDelayFactor(Tics : Word);
    {-Set the number of Tics to wait between commands sent to the modem}
  begin
    DelayFactor := Tics;
  end;

  procedure Modem.SetCmdTimeout(Tics : Word);
    {-Set the number of Tics to wait for a modem response}
  begin
    CmdTimeOut := Tics;
  end;

  procedure Modem.SetDTRDropHold(Tics : Word);
    {-Set the number of Tics to hold DTR low during hangup}
  begin
    DTRDropHold := Tics;
  end;

  procedure Modem.SetCharDelay(Tics : Word);
    {-Set the number of Tics to wait between each command character sent}
  begin
    ModemCharDelay := Tics;
  end;

  procedure Modem.SetTildeDelay(Tics : Word);
    {-Set the number of Tics to wait when a '~' is encountered in a command}
  begin
    TildeDelay := Tics;
  end;

  procedure Modem.SetRingWaitTimeout(Tics : Word);
    {-Set the number of Tics to wait before mAutoAnswerModem resets}
  begin
    RingWaitTimeOut := Tics;
  end;

  procedure Modem.SetStatusProc(MSP : ModemStatusProc);

  begin
    DisplayStatus := MSP;
  end;

  procedure Modem.ShowStatus(MsgType, SecsRemaining : Word);

  begin
    if (@DisplayStatus <> nil) then begin
      {$IFDEF DispatchDebug}
      DebugOut('show status');
      {$ENDIF}
      DisplayStatus(@Self, MsgType, SecsRemaining);
    end;
  end;

  procedure Modem.PutCommand(Cmd : String; Wait : Boolean);
    {-Send a command to the modem}
  begin
    if ModemBusy then begin
      AsyncStatus := ecModemBusy;
      Exit;
    end;

    {enable response codes}
    mEnableWaitStrs(RspWaitSet);

    {send the command to the modem}
    mPutXlatStr(Cmd);
    if (AsyncStatus <> ecOK) then begin
      mDisableWaitStrs(RspWaitSet);
      Exit;
    end;

    ModemBusy := True;
    NewTimer(ResponseTimer, CmdTimeOut);

    mHandleCommandResponse(Wait);
  end;

  procedure Modem.Initialize(Wait : Boolean);
    {-Send the initialization String to the modem}
  begin
    PutCommand(InitCmd^, Wait)
  end;

  function Modem.NumConfigStrings : Word;
    {-Returns number of command strings with configuration string}
  begin
    NumConfigStrings := WordCount(ConfigCmd^, [CmdSepChar]);
  end;

  function Modem.GetConfigString(StrNum : Word) : CmdStrType;
    {-Returns command string from configuration string}
  begin
    if (StrNum > NumConfigStrings) then
      GetConfigString := ''
    else
      GetConfigString := ExtractWord(StrNum, ConfigCmd^, [CmdSepChar]);
  end;

  procedure Modem.Configure;
    {-Send the configuration strings to the modem}
  var
    I        : Integer;
    HadError : Boolean;

  begin
    HadError := False;
    for I := 1 to NumConfigStrings do begin
      PutCommand(GetConfigString(I), False);

      if (AsyncStatus <> ecOK) and (AsyncStatus <> ecError) then
        Exit;
      if AsyncStatus = ecError then
        HadError := True;

      DelayTics(DelayFactor);
    end;

    if HadError then
      AsyncStatus := ecError
    else
      AsyncStatus := ecOK;
  end;

  procedure Modem.Hangup(Wait : Boolean);
    {-Send hangup command to modem}
  begin
    ConnectSpeed := 0;

    {check to see if we want to drop DTR or not}
    if (HangupCmd^ = '') or (StUpCase(HangupCmd^) = 'DTR') then begin
      {$IFDEF DispatchDebug}
      DebugOut('Entering DropDTR state');
      {$ENDIF}
      P^.SetDTR(False);
      if (AsyncStatus <> ecOK) then
        Exit;

      DelayTics(DTRDropHold);

      {$IFDEF DispatchDebug}
      DebugOut('Restablishing DTR');
      {$ENDIF}
      P^.SetDtr(True);
    end else
      PutCommand(HangupCmd^, Wait);
  end;

  procedure Modem.mDialAnswer(Number : String; IsDialing : Boolean;
                              Wait : Boolean);
  {-Dials or answers, depending upon IsDialing}
  begin
    if ModemBusy then begin
      AsyncStatus := ecModemBusy;
      Exit;
    end;

    if IsDialing then
      if Number = '' then begin
        AsyncStatus := epNonFatal + ecInvalidArgument;
        Exit;
      end;

    {initialize dialing variables}
    ErrorCorrection := False;
    DataCompression := False;
    ConnectSpeed    := 0;

    {enable dial response codes}
    mEnableWaitStrs(DialWaitSet);
    mEnableFeatureTags;

    {send the dial command}
    if IsDialing then begin
      {$IFDEF DispatchDebug}
      DebugOut('Sending dial command');
      {$ENDIF}
      mPutXlatStr(DialCmd^);
      if (AsyncStatus = ecOK) then begin
        mPutXlatStr(Number);
        if (AsyncStatus = ecOK) then
          mPutXlatStr(DialTerm^);
      end;
      NewTimer(ResponseTimer, Secs2Tics(DialTimeOut));
    end else begin
      mPutXlatStr(AnswerCmd^);
      NewTimer(ResponseTimer, Secs2Tics(AnswerTimeOut));
    end;

    if (AsyncStatus <> ecOK) then begin
      mDisableWaitStrs(DialWaitSet);
      mDisableFeatureTags;
      Exit;
    end;

    ModemBusy := True;
    mHandleConnectionAttemptResponse(IsDialing, Wait);
  end;

  procedure Modem.Dial(Number : String; Wait : Boolean);
    {-Dial the modem}
  begin
    mDialAnswer(Number, True, Wait);
  end;

  procedure Modem.SendCancel;
    {-Sends the cancel command to the modem}
  begin
    mPutXlatStr(DialCancel^);
    DelayTics(DelayFactor);
    ModemBusy := False;
  end;

  function Modem.GetConnectSpeed : LongInt;
    {-Get the actual speed of the connection}
  begin
    GetConnectSpeed := ConnectSpeed;
  end;

  function Modem.GetErrorCorrection : Boolean;

  begin
    GetErrorCorrection := ErrorCorrection;
  end;

  function Modem.GetDataCompression : Boolean;

  begin
    GetDataCompression := DataCompression;
  end;

  function Modem.ModemWorking : Boolean;

  begin
    ModemWorking := ModemBusy;
  end;

  procedure Modem.Answer(Wait : Boolean);
    {-Answer the modem}
  begin
    mDialAnswer('', False, Wait);
  end;

  procedure Modem.AutoAnswer(Rings : Word);
    {-Answer the modem after Rings rings}
  begin
    if ModemBusy then begin
      AsyncStatus := ecModemBusy;
      Exit;
    end;

    if (Rings = 0) then begin
      AsyncStatus := epNonFatal + ecInvalidArgument;
      Exit;
    end;

    mEnableWaitStrs([RspRing]);
    mEnableFeatureTags;

    AsyncStatus := ecOK;
    mWaitForRing(Rings);
  end;

  function Modem.ProcessCommandResponse : Boolean;
  begin
    if not ModemBusy then begin
      AsyncStatus := ecModemNotBusy;
      ProcessCommandResponse := False;
    end else
      if TimerExpired(ResponseTimer) then begin
        ProcessCommandResponse := False;
        AsyncStatus := ecTimeOut;
        ModemBusy := False;
        mDisableWaitStrs(RspWaitSet);
      end else
        ProcessCommandResponse := mHandleCommandResponse(False);
  end;

  procedure Modem.ProcessConnectResponse;
  begin
    if not ModemBusy then
      AsyncStatus := ecModemNotBusy
    else
      if TimerExpired(ResponseTimer) then begin
        ModemBusy := False;
        AsyncStatus := ecTimeOut;
        mDisableWaitStrs(DialWaitSet);
        mDisableFeatureTags;
      end else
        mHandleConnectionAttemptResponse(True, False);
  end;

end.
