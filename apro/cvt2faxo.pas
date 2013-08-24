{$S-,R-,V-,I-,B-,F+,A-}

{$I APDEFINE.INC}

{.$DEFINE TestStreams}

{******************************************************}
{*                 CVT2FAXO.PAS  2.03                 *}
{*      Copyright (c) TurboPower Software 1993.       *}
{*                All rights reserved.                *}
{******************************************************}

program Cvt2FaxO;
  {-Convert text, PCX, DCX, TIFF, or BMP file to fax format}

uses
  {$IFDEF LeakCheck}                                                   {!!.01}
  LeakChek,                                                            {!!.01}
  {$ENDIF}                                                             {!!.01}
  Dos,
  {$IFDEF UseOPro}
  OpCrt,
  OpString,
  OpRoot,
  {$IFDEF Opro12}
  OpConst,
  {$ENDIF}
  {$ENDIF}
  {$IFDEF UseTPro}
  TpCrt,
  TpString,
  {$ENDIF}
  {$IFDEF Standalone}
  Crt,
  {$ENDIF}
  ApMisc,
  OoFaxCvt;

type
  CvtFileType = (cvtNone, cvtText, cvtPcx, cvtDcx, cvtTiff, cvtBmp);   {!!.03}

const
  CvtFile     : String = '';
  StaID       : String = 'FAX DEMO';
  CvtType     : CvtFileType = cvtText;
  HiRes       : Boolean = False;
  HiResFont   : Boolean = False;
  UseHPFont   : Boolean = False;
  HPFontName  : PathStr = '';
  PgBrk       : Integer = 0;
  SingleWidth : Boolean = False;
  LeftImage   : Boolean = False;
  ExtraWide   : Boolean = False;  {!!.03}

var
  FCP : AbstractFaxConverterPtr;

  procedure Abort(S : String);
  begin
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

  procedure ShowHelp;
  begin
    WriteLn('Usage: CVT2FAXO [options] FileName');
    WriteLn('  /T           Convert from ASCII text to fax (default)');
    WriteLn('  /P           Convert from PCX (mono) to fax');
    WriteLn('  /D           Convert from DCX (mono) to fax');
    WriteLn('  /F           Convert from TIFF (mono) to fax');
    Writeln('  /M           Convert from BMP (mono) to fax');          {!!.03}
    WriteLn('  /L Lines     Specify page break (text only, default=0 (no breaks),');
    WriteLn('  /H           High resolution output (200x200; default=100x200)');
    WriteLn('  /B           Use high resolution font when converting text');
    WriteLn('  /S FontName  Use HP soft font file');
    WriteLn('  /N           No width doubling for graphics files');
    WriteLn('  /W           Generate 2048 pixel wide fax');            {!!.03}
    WriteLn('  /J           Left justify graphics images instead of centering them');
    WriteLn('  /?           This screen');
    Halt(0);
  end;

  procedure ParseCmdLine;
  var
    S : String;
    I, N, C : Integer;
  begin
    if ParamCount = 0 then
      ShowHelp;
    I := 1;
    while I <= ParamCount do begin
      S := ParamStr(I);
      if S[1] in ['/','-'] then begin
        case Upcase(S[2]) of
          '?':
            ShowHelp;
          'M':                                                         {!!.03}
            CvtType := cvtBmp;                                         {!!.03}
          'T':
            CvtType := cvtText;
          'P':
            CvtType := cvtPCX;
          'D':
            CvtType := cvtDCX;
          'F':
            CvtType := cvtTIFF;
          'H':
            HiRes := True;
          'B' :
            HiResFont := True;
          'I':
            begin
              Inc(I);
              StaID := ParamStr(I);
            end;
          'L':
            begin
              Inc(I);
              S := ParamStr(I);
              Val(S, N, C);
              if (C = 0) and (N >= 0) then
                PgBrk := N
              else
                ShowHelp;
            end;
          'S' :
            begin
              Inc(I);
              HPFontName := ParamStr(I);
              UseHPFont := True;
            end;
          'N' :
            SingleWidth := True;
          'W' :                      {!!.03}
            ExtraWide := True;       {!!.03}
          'J' :
            LeftImage := True;
          else
            begin
              WriteLn('Unknown parameter "'+S+'"');
              ShowHelp;
            end;
        end;
      end else
        CvtFile := StUpcase(S);

      Inc(I);
    end;

    if CvtFile = '' then begin
      WriteLn('No filename provided');
      ShowHelp;
    end;
  end;

  function OurStatusFunc(FCP : AbstractFaxConverterPtr;
                         Starting, Ending : Boolean) : Boolean;
  const
    First : Boolean = True;
  var
    Line : LongInt;
    Page : Integer;
  begin
    with FCP^ do begin
      if First then begin
        WriteLn('Converting ', StUpcase(GetFileName));
        First := False;
      end;
      GetStatusInfo(Line, Page);
      if (AsyncStatus mod 10000 )= ecHPFontCvt then
        Write(^H^H^H^H^H'('+PadCh(Long2Str(CID), ' ', 3)+')')
      else
        Write(^M'Processing line ', Line:4, ' of page ', Page:4);

      {Check for abort request}
      if KeyPressed then
        OurStatusFunc := (ReadKey = #27)
      else
        OurStatusFunc := False;
    end;
  end;

  {$IFDEF TestStreams}
  procedure CreateStream;
    {-Store FCP to stream}
  var
    S : BufIdStream;
    Status : Word;
  begin
    WriteLn('Creating stream file');
    {Create a new stream}
    if not S.Init('CVT.STM', SCreate, 1024) then
      Abort('Failed to make stream ');

    {Register all hierarchies}
    S.RegisterHier(TextFaxConverterStream);
    S.RegisterHier(PcxFaxConverterStream);
    S.RegisterHier(DcxFaxConverterStream);
    S.RegisterHier(TiffFaxConverterStream);
    Status := S.GetStatus;
    if Status <> 0 then
      Abort('Error registering object ');

    {Register our user procedures}
    S.RegisterPointer(1000, @OurStatusFunc);

    {Store the converter}
    S.PutPtr(FCP);
    Status := S.GetStatus;
    if Status <> 0 then
      Abort('Error storing');

    {Clean up}
    S.Done;
  end;

  procedure LoadStream;
    {-Load FCP from a stream}
  var
    S : BufIdStream;
    Status : Word;
  begin
    WriteLn('Loading stream file');

    {Re-open existing new stream}
    if not S.Init('CVT.STM', SOpen, 1024) then
      Abort('Failed to open stream');

    {Register all hierarchies}
    S.RegisterHier(TextFaxConverterStream);
    S.RegisterHier(PcxFaxConverterStream);
    S.RegisterHier(DcxFaxConverterStream);
    S.RegisterHier(TiffFaxConverterStream);
    Status := S.GetStatus;
    if Status <> 0 then
      Abort('Error registering object');

    {Register our user procedures}
    S.RegisterPointer(1000, @OurStatusFunc);

    {Load the converter}
    RootPtr(FCP) := S.GetPtr;
    Status := S.GetStatus;
    if Status <> 0 then
      Abort('Error loading converter');

    {Clean up}
    S.Done;
  end;
  {$ENDIF}

begin
  Assign(Output, '');
  Rewrite(Output);

  WriteLn('CVT2FAXO 2.03 - Convert text, PCX, DCX, TIFF, or BMP files to APF format'); {!!.03}
  WriteLn('Copyright (c)1993  TurboPower Software');
  ParseCmdLine;
  case CvtType of
    cvtPCX  : FCP := New(PcxFaxConverterPtr, Init);
    cvtDCX  : FCP := New(DcxFaxConverterPtr, Init);
    cvtTIFF : FCP := New(TiffFaxConverterPtr, Init);
    cvtBMP  : FCP := New(BmpFaxConverterPtr, Init);                    {!!.03}
    else      FCP := New(TextFaxConverterPtr, Init);
  end;
  if FCP = nil then
    Abort('Error '+Long2Str(AsyncStatus)+' initializing converter object');

  {Set hooks}
  FCP^.SetStatusFunc(OurStatusFunc);
  FCP^.SetPageSize(PgBrk);
  FCP^.SetResolutionMode(HiRes);
  if ExtraWide then                    {!!.03}
    FCP^.SetResolutionWidth(rw2048);   {!!.03}
  FCP^.SetStationID(StaID);

  {$IFDEF TestStreams}
  CreateStream;
  Dispose(FCP, Done);
  LoadStream;
  {$ENDIF}

  {Pick a font}
  if UseHPFont then begin
    Write('Loading HP font ', HPFontName, ':      ');
    if not FCP^.LoadHPFont(HPFontName, HiResFont) then
      Abort(^M^J'Failed to load HP soft font, font uses an supported feature');
    WriteLn;
  end else if HiResFont then
    if not FCP^.LoadFont(StandardFont, True) then ;

  (* test margin setting
  FCP^.SetMargins(100, 10);
  *)

  {Convert it}
  case CvtType of
    cvtPCX, cvtDCX:
      begin
        if SingleWidth then
          FCP^.fcOptionsOff(fcDoubleWidth);
        if LeftImage then
          FCP^.fcOptionsOff(fcCenterImage);
        FCP^.ConvertFax(CvtFile);
      end;
    cvtTiff:
      begin                                                            {!!.02}
        if SingleWidth then                                            {!!.02}
          FCP^.fcOptionsOff(fcDoubleWidth);                            {!!.02}
        if LeftImage then                                              {!!.02}
          FCP^.fcOptionsOff(fcCenterImage);                            {!!.02}
        FCP^.ConvertFax(CvtFile);
      end;                                                             {!!.02}
    cvtBmp :                                                           {!!.03}
      begin                                                            {!!.03}
        if SingleWidth then                                            {!!.03}
          FCP^.fcOptionsOff(fcDoubleWidth);                            {!!.03}
        if LeftImage then                                              {!!.03}
          FCP^.fcOptionsOff(fcCenterImage);                            {!!.03}
        FCP^.ConvertFax(CvtFile);                                      {!!.03}
      end;                                                             {!!.03}
    else
      {Assumed to be text}
      FCP^.ConvertFax(CvtFile);
  end;

  {Cleanup}
  case AsyncStatus of
    0 :     WriteLn(^M^J'Conversion complete');
    9976 :  WriteLn(^M^J'Input file is bad or uses an unsupported feature');
    else    WriteLn(^M^J'Error ', AsyncStatus, ' converting file');
  end;
  WriteLn;
  Dispose(FCP, Done);
end.
