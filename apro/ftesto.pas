{$R-,S-,F+}
{$M 8192, 0, 655350}

{$I APDEFINE.INC}       {Defines we care about it}

{$IFDEF UseUart}
{$IFDEF UseFossil}
  Error - This program only supports one device layer at a time.
          Disable UseUart or UseFossil in APDEFINE.INC
          (See COMTESTO for an example of multiple device layers.)
{$ENDIF}
{$ENDIF}

{******************************************************}
{*                   FTESTO.PAS 2.03                  *}
{*      Copyright (c) TurboPower Software 1993.       *}
{*                All rights reserved.                *}
{******************************************************}

program FossilTest;
  {-A simple terminal}
uses
  Crt,
  ApMisc,
  ApPort,
  ApAnsi,
  {$IFDEF UseFossil}
  ApFossil,
  {$ENDIF}
  {$IFDEF UseUart}
  ApUart,
  {$ENDIF}
  OoCom,
  OoAbsPcl,
  OoXmodem,
  OoYmodem,
  OoZmodem,
  OoKermit,
  OoAscii;

const
  {Change these constants as required}
  DesiredPort = Com2;
  Speed       = 38400;

var
  {$IFDEF UseFossil}
  ComPort : FossilPort;
  {$ENDIF}
  {$IFDEF UseUart}
  ComPort : UartPort;
  {$ENDIF}
  C : Char;
  Finished : Boolean;

function KbdAbort : Boolean;
  {-Return True if user wants to abort}
var
  C : Char;
begin
  KbdAbort:=False;
  if KeyPressed then begin
    C:=ReadKey;
    if C=#0 then
      C:=ReadKey
    else if C = #$1B then
      KbdAbort:=True;
  end;
end;

procedure ProtocolStatus(AP : AbstractProtocolPtr;
                         Starting, Ending : Boolean);
  {-Display simple protocol status}
const
  LastFileName : String = '';
var
  FileName : String;
begin
  if Starting then
    Writeln(ProtocolTypeString[AP^.GetProtocol],' starting');

  {Get and display filename}
  FileName := AP^.GetPathName;
  if FileName = '' then
    FileName := AP^.GetFileName;
  if LastFileName <> FileName then begin
    WriteLn;
    LastFileName := FileName;
  end;

  {If we have a filename show status}
  if FileName <> '' then begin
    GotoXY(1, 25);
    ClrEol;
    Write(FileName, ': ', AP^.GetBytesTransferred,
          '/', AP^.GetBytesRemaining);
  end;

  if Ending then begin
    Writeln;
    Writeln(ProtocolTypeString[AP^.GetProtocol],' ending');
  end;
end;

function GetProtocol : AbstractProtocolPtr;
  {-Ask which protocol}
var
  C : Char;
begin
  repeat
    WriteLn;
    WriteLn;
    WriteLn('A - ASCII');
    WriteLn('X - Xmodem  K - Xmodem1K  L - Xmodem1KG');
    WriteLn('Y - Ymodem  G - YmodemG');
    WriteLn('Z - Zmodem');
    WriteLn('F - Kermit');
    WriteLn('Q - cancel protocol');
    Write('Protocol: ');
    C := Upcase(ReadKey);
    GetProtocol := nil;
    case C of
      'X' : GetProtocol := New(XmodemProtocolPtr, Init(@ComPort, False, False));
      'K' : GetProtocol := New(XmodemProtocolPtr, Init(@ComPort, True, False));
      'L' : GetProtocol := New(XmodemProtocolPtr, Init(@ComPort, True, True));
      'Y' : GetProtocol := New(YmodemProtocolPtr, Init(@ComPort, True, False));
      'G' : GetProtocol := New(YmodemProtocolPtr, Init(@ComPort, True, True));
      'Z' : GetProtocol := New(ZmodemProtocolPtr, Init(@ComPort));
      'F' : GetProtocol := New(KermitProtocolPtr, Init(@ComPort));
      'A' : GetProtocol := New(AsciiProtocolPtr, Init(@ComPort));
      'Q' : GetProtocol := nil;
    end;
  until C in ['X', 'K', 'L', 'Y', 'G', 'Z', 'F', 'A', 'Q'];
end;

procedure UploadFile;
  {-Transmit a file}
var
  FileName : String;
  Protocol  : AbstractProtocolPtr;
begin

  Protocol := GetProtocol;
  if Protocol = nil then begin
    Writeln('Failed to initialize protocol object: ',AsyncStatus);
    Exit;
  end;
  ComPort.SetAbortFunc(KbdAbort);

  WriteLn;
  Write('Upload file name: ');
  Readln(FileName);
  if FileName = '' then begin
    Dispose(Protocol, Done);
    Exit;
  end;

  with Protocol^ do begin
    SetShowStatusProc(ProtocolStatus);
    SetFileMask(FileName);
    ProtocolTransmit;
  end;

  if AsyncStatus = ecOk then
    Writeln('Upload completed successfully!')
  else
    Writeln('Upload failed: ',AsyncStatus);
  Dispose(Protocol,Done);
end;

procedure DownloadFile;
  {-Receive a file}
var
  FilePath : String;
  Protocol : AbstractProtocolPtr;
begin
  Protocol := GetProtocol;
  if Protocol = nil then begin
    Writeln('Failed to initialize protocol object: ',AsyncStatus);
    Exit;
  end;

  if Protocol^.SupportsBatch then
    Write('Download path: ')
  else
    Write('File name and path:');
  Readln(FilePath);
  ComPort.SetAbortFunc(KbdAbort);

  with Protocol^ do begin
    SetShowStatusProc(ProtocolStatus);
    if SupportsBatch then
      SetDestinationDirectory(FilePath)
    else
      SetReceiveFileName(FilePath);
    ProtocolReceive;
  end;

  if AsyncStatus = ecOk then
    Writeln('Download completed successfully!')
  else
    Writeln('Download failed: ',AsyncStatus);
  Dispose(Protocol,Done);
end;

begin
  {$IFDEF USEFOSSIL}
    WriteLn('Using FOSSIL device layer');
  {$ENDIF}

  {$IFDEF USEUART}
    WriteLn('Using UART device layer');
  {$ENDIF}

  {Open a port}
  ComPort.InitFast(DesiredPort, Speed);
  if AsyncStatus <> ecOk then begin
    WriteLn('Failed to open port: ', AsyncStatus);
    Halt;
  end;

  Finished := False;
  Writeln('<PGUP> to upload, <PGDN> to download, ALT-X to exit');

  {$IFDEF TRACING}
  InitTracing(4000);
  {$ENDIF}

  repeat
    {Process chars to send}
    if KeyPressed then begin
      C := ReadKey;
      if C = #0 then
      begin
        C := ReadKey;
        Case C of
          #$2D: Finished := True;
          #$49: UploadFile;
          #$51: DownloadFile;
        end;
      end else begin
        while not ComPort.TransReady do ;
        ComPort.PutChar(C);
      end;
    end;

    {Process chars received}
    if ComPort.CharReady then begin
      ComPort.GetChar(C);
      if AsyncStatus <> ecOk then begin
        WriteLn(^M^J'Line error ', AsyncStatus);
        ComPort.FlushInBuffer;
      end else
        WriteCharAnsi(C);
    end;
  until Finished;

  {$IFDEF TRACING}
  DumpTrace('FTEST.TRC');
  {$ENDIF}

  ComPort.Done;
end.
