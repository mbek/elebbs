{$S-,R-,V-,I-,B-,F+,A-}

{$I APDEFINE.INC}

{.$DEFINE TestStreams}

{******************************************************}
{*                  PCX2FAXO.PAS  2.03                *}
{*      Copyright (c) TurboPower Software 1993.       *}
{*                All rights reserved.                *}
{******************************************************}

program Fax2PcxO;
  {-Converts a FAX to a PCX/DCX}

uses
  {$IFDEF LeakCheck}                                                   {!!.01}
  LeakChek,                                                            {!!.01}
  {$ENDIF}                                                             {!!.01}
  Crt,
  Dos,
  {$IFDEF UseOPRO}
  OpString,
  OpRoot,
  {$ENDIF}
  {$IFDEF UseTPRO}
  TpString,
  {$ENDIF}
  ApMisc,
  OoFaxCvt;

const
  CopyrightLine = 'FAX2PCXO 2.03 -- APF to PCX/DCX   (c)1993 TurboPower Software'; {!!.01}
  HalveIt : Boolean = False;  {True to use 1/2 horizontal width}
  IsDCX : Boolean = False;    {True for DCX file}

var
  UPF  : AbstractPcxUnpackPtr;
  PcxFile : PathStr;
  FaxFile : PathStr;
  Lines : Word;
  DotPos : Byte;

  procedure Abort(Msg : String);
  begin
    WriteLn(Msg);
    Halt(1);
  end;

  procedure ShowHelp;
  begin
    WriteLn('Usage: FAX2PCXO [options] FaxFile');
    WriteLn('  /H    Reduce image width (50% reduction)');
    WriteLn('  /D    Output to a DCX file');
    WriteLn('  /?    Display help screen');
    Halt(1);
  end;

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
          'H' : HalveIt := True;
          'D' : IsDcx := True;
          else  ShowHelp;
        end;
      end else
        FaxFile := DefaultExtension(StUpcase(S), 'APF');
      Inc(I);
    until I > ParamCount;
    if FaxFile = '' then
      ShowHelp;
  end;

  function ConvertFaxLine(UP : UnpackFaxPtr; Buffer : PByteBuffer;
                          Len : Word; PH : PageHeaderRec) : Boolean;
    {-No conversion to do, just show status or abort}
  var
    C : Char;
  begin
    if KeyPressed then begin
      C := ReadKey;
      if C = #0 then
        C := ReadKey
      else
        ConvertFaxLine := C = #27;
    end else begin
      ConvertFaxLine := False;
      Inc(Lines);
      if Lines mod 10 = 0 then
        Write(^H^H^H^H^H, Lines:5);
    end;
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
    S.RegisterHier(UnpackToPcxStream);
    S.RegisterHier(UnpackToDcxStream);
    Status := S.GetStatus;
    if Status <> 0 then
      Abort('Error registering object ');

    {Register our user procedures}
    S.RegisterPointer(1000, @ConvertFaxLine);

    {Store the unpacker}
    S.PutPtr(UPF);
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
    S.RegisterHier(UnpackToPcxStream);
    S.RegisterHier(UnpackToDcxStream);
    Status := S.GetStatus;
    if Status <> 0 then
      Abort('Error registering object');

    {Register our user procedures}
    S.RegisterPointer(1000, @ConvertFaxLine);

    {Load the converter}
    RootPtr(UPF) := S.GetPtr;
    Status := S.GetStatus;
    if Status <> 0 then
      Abort('Error loading unpacker');

    {Clean up}
    S.Done;
  end;
  {$ENDIF}

begin
  WriteLn(CopyrightLine);

  ParseCmdLine;

  {Create the output file}
  DotPos := Pos('.', FaxFile);
  if IsDcx then
    PcxFile := Copy(FaxFile, 1, DotPos-1) + '.DCX'
  else
    PcxFile := Copy(FaxFile, 1, DotPos-1) + '.PCX';

  {Init the fax decompressor object}
  if IsDcx then
    UPF := New(UnpackToDcxPtr, Init)
  else
    UPF := New(UnpackToPcxPtr, Init);
  if UPF = nil then begin
    WriteLn('Error initializing unpacker');
    Halt;
  end;

  if HalveIt then
    UPF^.ufOptionsOn(ufHalfWidth);

  {$IFDEF TestStreams}
  CreateStream;
  Dispose(UPF, Done);
  LoadStream;
  {$ENDIF}

  {Convert the fax}
  Write(^M^J'converting raster line:    ');
  UPF^.SetOutputLineFunc(ConvertFaxLine);
  Lines := 0;
  if IsDcx then
    UnpackToDcxPtr(UPF)^.UnpackFileToDcx(FaxFile, PcxFile)
  else
    UnpackToPcxPtr(UPF)^.UnpackFileToPcx(FaxFile, PcxFile);
  if AsyncStatus <> 0 then
    WriteLn(^M^J'Decompress error ', AsyncStatus);

  Dispose(UPF, Done);
end.

