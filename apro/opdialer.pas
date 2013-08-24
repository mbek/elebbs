{$V-,B-,I-}

{Conditional defines that may affect this unit}
{$I OPDEFINE.INC}
{$I APDEFINE.INC}
{$X+}
{*********************************************************}
{*                  OPDIALER.PAS 2.03                    *}
{*     Copyright (c) TurboPower Software 1995.           *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$IFNDEF UseOOP}
  !! OPDIALER requires UseOOP to be defined
{$ENDIF}

{$IFNDEF UseOPRO}
  !! OPDIALER requires UseOPRO to be defined
{$ENDIF}

unit OpDialer;

interface

uses
  {$IFDEF UseMouse}
  OpMouse,
  {$ENDIF}
  OoModem2,
  ApMisc,
  ApTimer,
  OpInline,
  OpString,
  OpCrt,
  OpRoot,
  OpCmd,
  OpWindow,
  OpPick,
  OpField,
  OpCtrl,
  OpDialog;

const
  DefBusyWait    = 60;       {Default time in seconds between retries when busy}
  DefDialTries   = 10;       {Default number of dial attempts}
  DefExtendTime  = 5;        {Default number of seconds to extend dial attempt}
  DefMsgDelay    = 3;        {Default delay between message and closing box}
  ccCycle        = ccUser1;  {Command constant for Cycle button}
  ccExtend       = ccUser2;  {Command constant for Extend button}
  ccAbort        = ccUser3;  {Command constant for Abort button}
  ccFinished     = ccUser4;  {bogus command for dialing finished}
  BogusKeyCode   = $E100;    {key code used to fake out command processor}
  InfoStringLen  = 40;       {Length of strings displayed in dialog box}
  NumInfoStrings = 9;        {Number of strings displayed in dialog box}

  dExitVoice      = $01;      {Bit flag to exit on voice result}
  dExitError      = $02;      {Bit flag to exit on error result}
  dExitNoCarrier  = $04;      {Bit flag to exit on no carrier result}
  dExitNoDialTone = $08;      {Bit flag to exit on no dialtone result}
  dExitBusy       = $10;      {Bit flag to exit on busy result}

  {Default exit options}
  DefExitOptions : Byte = dExitVoice + dExitError + dExitNoCarrier +
                          dExitNoDialTone;

type
  {type of string displayed in dialog box}
  InfoString = string[InfoStringLen];

  {array for holding display strings}
  InfoStringArray = array[1..NumInfoStrings] of InfoString;

  {the great and almighty dialer dialog box}
  DialerDialogPtr = ^DialerDialog;
  DialerDialog =
    object(DialogBox)
      {dialing data}
      Modem             : ModemPtr;
      DialTry           : Integer;
      MaxTries          : Integer;
      PhoneNumber       : String;
      Connected         : Boolean;

      {retry data}
      Retrying          : Boolean;
      RetryWait         : Integer;
      RetrySecs         : Integer;

      {interface data}
      DialOptions       : Word;
      OnStatus          : Integer;
      SecsRemaining     : Word;
      CntDownTimer      : EventTimer;
      StatusMsgs        : array[1..NumInfoStrings] of InfoString;

      {old hooks}
      OldGetKeyProc     : GetKeyProc;
      OldKeyPressedProc : KeyPressedProc;
      OldStatusProc     : ModemStatusProc;

      {other stuff}
      ExitStatus        : Word;
      StopProcessing    : Boolean;

      {constructors/destructors}
      constructor Init( X, Y          : Word;
                        Colors        : ColorSet;
                        dColors       : DialogColorSet;
                        AModem        : ModemPtr;
                        APhoneNumber  : String;
                        AMaxDialTries : Integer;
                        ARetrySecs    : Integer;
                        AOptions      : Integer);
        {-Instantiate a dialer dialog box}

      procedure ProcessSelf; virtual;
      procedure UpdateContents; virtual;

      {+++private+++}
      procedure BackgroundProcessing;
        {-Do background processing tasks}
      procedure AddStatusString(St : String);
        {-Add a string to the display}
      procedure DrawStatusDisplay;
        {-Draw strings in status area}
      procedure BeginAttempt;
        {-Begin a dial attempt}
      procedure BeginRetry(WhyStr : String);
        {-Begin a dial retry wait}
      procedure EndRetry;
        {-End a dial retry wait}
    end;

implementation

  var
    SelfDialogPtr : DialerDialogPtr;

  function ModemKeyPressed : Boolean; far;
  begin
    if SelfDialogPtr^.OldKeyPressedProc then
      ModemKeyPressed := True
    else begin
      SelfDialogPtr^.BackgroundProcessing;
      ModemKeyPressed := SelfDialogPtr^.StopProcessing;
    end;
  end;

  function ModemReadKey : Word; far;
  begin
    {$IFNDEF UseDrag}
    while not ModemKeyPressed do ;
    {$ENDIF}
    if SelfDialogPtr^.StopProcessing then
      ModemReadKey := BogusKeyCode
    else
      ModemReadKey := SelfDialogPtr^.OldGetKeyProc;
  end;

  procedure DialerStatusProc(P : ModemPtr; MsgType, SecsRemaining : Word); far;
  begin
    with SelfDialogPtr^ do
      case MsgType of
        ecConnect   :
          begin
            ExitStatus := MsgType;
            Connected := True;
            AddStatusString('Modem connected!');
          end;

        ecGotBaud   :
          begin
            AddStatusString('Connected at ' + Long2Str(SelfDialogptr^.Modem^.ConnectSpeed) + ' baud');
            StopProcessing := True;
          end;

        ecBusy      :
          begin
            ExitStatus := MsgType;
            if ((DialOptions and dExitBusy) <> 0) then
              StopProcessing := True
            else
              BeginRetry('Result: Remote is busy');
          end;

        ecVoice     :
          begin
            ExitStatus := MsgType;
            if ((DialOptions and dExitVoice) <> 0) then
              StopProcessing := True
            else
              BeginRetry('Result: Remote answered with voice');
          end;

        ecNoCarrier :
          begin
            ExitStatus := MsgType;
            if ((DialOptions and dExitNoCarrier) <> 0) then
              StopProcessing := True
            else
              BeginRetry('Result: No carrier, connection failed');
          end;

        ecNoDialTone:
          begin
            ExitStatus := MsgType;
            if ((DialOptions and dExitNoDialTone) <> 0) then
              StopProcessing := True
            else
              BeginRetry('Result: No dialtone, check connections');
          end;

        ecError     :
          begin
            ExitStatus := MsgType;
            if ((DialOptions and dExitError) <> 0) then
              StopProcessing := True
            else
              BeginRetry('Result: Modem returned an error');
          end;
      end;
  end;

  constructor DialerDialog.Init(X, Y          : Word;
                                Colors        : ColorSet;
                                dColors       : DialogColorSet;
                                AModem        : ModemPtr;
                                APhoneNumber  : String;
                                AMaxDialTries : Integer;
                                ARetrySecs    : Integer;
                                AOptions      : Integer);
  var
    I : Integer;

  begin
    if not DialogBox.InitCustom(X, Y, X + InfoStringLen + 1, Y + NumInfoStrings + 2,
                                Colors, wBordered+wClear+wUserContents, dColors) then
      Fail;

    AddPushButton('&Cycle', 11, 4, 8, 0, ccCycle, True);
    AddPushButton('E&xtend', 11, 18, 8, 0, ccExtend, False);
    AddPushButton('&Abort', 11, 32, 8, 0, ccAbort, False);

    SelfDialogPtr := @Self;

    {check for error}
    if (RawError <> 0) then begin
      InitStatus := RawError;
      Done;
    end;

    PhoneNumber   := APhoneNumber;
    Modem         := AModem;
    DialTry       := 1;
    MaxTries      := AMaxDialTries;
    Retrying      := False;
    RetryWait     := 0;
    RetrySecs     := ARetrySecs;
    OnStatus      := 0;
    SecsRemaining := 0;
    DialOptions   := AOptions;

    {clear informational strings}
    for I := 1 to NumInfoStrings do
      StatusMsgs[I] := '';
  end;

  procedure DialerDialog.ProcessSelf;
  var
    Finished : Boolean;

  begin
    {save old user command procedures}
    OldGetKeyProc     := CwCmdPtr^.cpGetKeyProc;
    OldKeyPressedProc := CwCmdPtr^.cpKeyPressedProc;

    Modem^.SetStatusProc(DialerStatusProc);

    cwCmdPtr^.AddCommand(ccFinished, 1, BogusKeyCode, 0);

    {set new command procedures}
    CwCmdPtr^.SetKeyPressedProc(ModemKeyPressed);
    CwCmdPtr^.SetGetKeyProc(ModemReadKey);

    Draw;

    {start the dial attempt}
    BeginAttempt;
    DialTry := 1;

    Finished   := False;
    ExitStatus := 0;

    repeat
      DialogBox.ProcessSelf;

      case cwCmd of
        0         : ;
        ccAbort:
          begin
            Finished   := True;
            ExitStatus := ecUserAbort;
          end;

        ccExtend:
          if not Retrying and not Connected then
            Inc(SecsRemaining, DefExtendTime);

        ccCycle:
          begin
            if Retrying then
              EndRetry
            else begin
              Inc(DialTry);
              if (DialTry > MaxTries) then begin
                ExitStatus := ecUserAbort;
                Finished := True;
              end else begin
                Modem^.SendCancel;
                AddStatusString('Cycling dial attempt . . .');
                ApTimer.DelayTics(4);
                BeginAttempt;
              end;
            end;
          end;

        ccFinished: Finished := True;
      end;
    until Finished;

    AsyncStatus := ExitStatus;

    cwCmdPtr^.AddCommand(ccNone, 1, BogusKeyCode, 0);

    Modem^.SetStatusProc(OldStatusProc);

    CwCmdPtr^.SetKeyPressedProc(OldKeyPressedProc);
    CwCmdPtr^.SetGetKeyProc(OldGetKeyProc);
  end;

  procedure DialerDialog.UpdateContents;
  begin
    DialogBox.UpdateContents;

    DrawStatusDisplay;
  end;

  procedure DialerDialog.AddStatusString(St : String);
    {-Add a string to the display}
  var
    I         : Integer;
    OldStatus : Word;

  begin
    OldStatus := OnStatus;
    Inc(OnStatus);
    if (OnStatus > NumInfoStrings) then begin
      for I := 2 to NumInfoStrings do
        StatusMsgs[I-1] := StatusMsgs[I];
      Dec(OnStatus);
    end;

    StatusMsgs[OnStatus] := St;

    DrawStatusDisplay;
  end;

  procedure DialerDialog.BackgroundProcessing;
    {-Do background processing tasks}
  begin
    if not Retrying and not Connected then begin
      Modem^.ProcessConnectResponse;
      if TimerExpired(CntDownTimer) then begin
        Dec(SecsRemaining);

        if (SecsRemaining > 0) then begin
          NewTimer(CntDownTimer, TicsPerSec);
          StatusMsgs[OnStatus] := 'Dialing . . . ' + Long2Str(SecsRemaining);
          DrawStatusDisplay;
        end else begin
          Modem^.SendCancel;
          BeginRetry('Result: Dial timed out');
        end;
      end;
    end else if Retrying then begin
      if TimerExpired(CntDownTimer) then begin
        Dec(RetryWait);

        if (RetryWait <> 0) then begin
          NewTimer(CntDownTimer, TicsPerSec);
          StatusMsgs[OnStatus] := 'Waiting . . . ' + Long2Str(RetryWait);
          DrawStatusDisplay;
        end else
          EndRetry;
      end;
    end;
  end;

  procedure DialerDialog.DrawStatusDisplay;
    {-Draw strings in status area}
  var
    I : Word;

  begin
    for I := 1 to NumInfoStrings do
      wFastWrite(
        Pad(StatusMsgs[I], InfoStringLen), I, 2,
        ColorMono(dgColors.ClusterColor, dgColors.ClusterMono));
  end;

  procedure DialerDialog.BeginAttempt;
    {-Begin a dial attempt}
  begin
    AddStatusString('Dialing . . . ' + Long2Str(Modem^.mGetDialTimeout));
    Modem^.Dial(PhoneNumber, False);
    NewTimer(CntDownTimer, TicsPerSec);
    SecsRemaining  := Modem^.mGetDialTimeout;
    Connected      := False;
    StopProcessing := False;
  end;

  procedure DialerDialog.BeginRetry(WhyStr : String);
    {-Begin a dial retry wait}
  begin
    Inc(DialTry);
    if (DialTry <= MaxTries) then begin
      AddStatusString(WhyStr);
      Retrying := True;
      AddStatusString('Waiting . . . ' + Long2Str(RetrySecs));
      RetryWait := RetrySecs;
      NewTimer(CntDownTimer, TicsPerSec);
    end else
      StopProcessing := True;
  end;

  procedure DialerDialog.EndRetry;
    {-End a dial retry wait}
  begin
    Dec(OnStatus);
    AddStatusString('Wait complete, redialing . . .');
    Retrying := False;
    BeginAttempt;
  end;

end.
