{$S-,R-,V-,I-,B-,F+,A-}

{******************************************************}
{*                  PRNFAXO.PAS 2.03                  *}
{*      Copyright (c) TurboPower Software 1993.       *}
{*                All rights reserved.                *}
{******************************************************}

{$I APDEFINE.INC}

program PrnFaxO;
  {-Print APro FAX file to Epson or PCL-compatible printers}

uses
  {$IFDEF LeakCheck}                                                   {!!.01}
  LeakChek,                                                            {!!.01}
  {$ENDIF}                                                             {!!.01}
  Crt,
  Dos,
  {$IFDEF UseOpro}
  OpString,
  {$ENDIF}
  {$IFDEF UseTpro}
  TpString,
  {$ENDIF}
  ApMisc,
  OoFaxCvt,
  OoFaxPrn;

const
  CopyrightLine = 'PRNFAXO 2.03 -- prints APRO fax files  (c) 1993 TurboPower Software';

  {Default printer type to use}
  PrintDest        : PathStr     = 'PRN';  {Default print destination}
  UseEpson         : Boolean     = False;  {True to use Epson printer}
  UseEpson24       : Boolean     = False;  {True to use Epson 24 pin printer}
  UseHP300         : Boolean     = False;  {True to use 300 dpi res}
  UsePcl5Mode      : Boolean     = False;  {Use HP PCL5 print commands}
  UseScaling       : Boolean     = True;   {Are we scaling the output}
  SuppressBlanks   : Boolean     = False;  {Suppress printing large blank areas}

var
  FP               : FaxPrinterPtr;         {The fax printer object}
  Printer          : AbstractPrinterPtr;    {Our printer type}
  FaxFN            : PathStr;               {Fax file to print}
  CurrentPage      : Word;                  {Page that is printing}
  TotalPages       : Word;                  {Document page count}
  I                : Word;

  procedure ShowHelp;
  begin
    WriteLn('Usage: PRNFAXO [options] FaxFile');
    WriteLn('  /F FileName   Write output to specified location (default = PRN)');
    WriteLn('  /E            Use Epson 9-pin output (default = HP)');
    WriteLn('  /2            Use Epson 24-pin output');
    WriteLn('  /3            Use HP 300x300 resolution (default = 150x150)');
    WriteLn('  /5            Use HP PCL5 print commands (default = PCL4)');
    WriteLn('  /L            Enable blank line suppression (HP only)');
    WriteLn('  /S            Disable page scaling');
    WriteLn('  /?            Display help screen');
    Halt(1);
  end;

  procedure ParseCmdLine;
  var
    S : String;
    I : Integer;
  begin
    FaxFN := '';
    if ParamCount = 0 then
      ShowHelp;

    I := 1;
    while I <= ParamCount do begin
      S := ParamStr(I);
      if S[1] = '?' then
        ShowHelp;
      if S[1] in ['/','-'] then begin
        case Upcase(S[2]) of
          'F':
            begin
              Inc(I);
              PrintDest := ParamStr(I);
            end;
          'E': UseEpson := True;
          '2' : UseEpson24 := True;
          '3' : UseHP300 := True;
          '5' : UsePcl5Mode := True;
          'L' : SuppressBlanks := True;
          'S' : UseScaling := False;
        end;
      end else
        FaxFN := DefaultExtension(S, 'APF');
      Inc(I);
    end;

    if (FaxFN = '') or (PrintDest = '') then
      ShowHelp;
  end;

  function MyLineNotifyFunc(Lines, LineSize : Word) : Boolean; far;
    {-Called for each raster line sent to the printer/file}
  var
    i  : Byte;
    Ch : Char;
  begin
    MyLineNotifyFunc := False;  {Don't abort}
    if Lines mod 10 = 0 then begin
      for i := 1 to 25 do
        Write(^H);
      Write(CurrentPage:3, ',   Raster line: ', Lines:5);
    end;
    if KeyPressed then begin
      while KeyPressed do Ch := ReadKey;
      if Ch = #27 then
        MyLineNotifyFunc := True;  {Abort}
    end;
  end;

  function MyPageNotifyFunc(Page, Pages : Word) : Boolean; far;
    {-Called before each page is sent to the printer/file}
  begin
    MyPageNotifyFunc := False;  {Don't abort}
    CurrentPage := Page;
    TotalPages := Pages;
  end;

  procedure AbortMsg(S : string);
    {-Writes an error message and then halts}
  begin
    WriteLn('Error: ', S);
    Halt(1);
  end;

begin
  WriteLn(CopyrightLine);

  ParseCmdLine;

  if UseEpson then
    New(EpsonPrinterPtr(Printer), Init)
  else if UseEpson24 then
    New(Epson24PinPrinterPtr(Printer), Init)
  else if UseHP300 then
    New(HP300PrinterPtr(Printer), Init)
  else
    New(HP150PrinterPtr(Printer), Init);
  if Printer = nil then
    AbortMsg('Failed to initialize print object');

  FP := nil;
  New(FP, Init(Printer));
  if FP <> nil then begin

    {Set print hooks}
    FP^.SetPageNotifyFunc(MyPageNotifyFunc);
    FP^.SetLineNotifyFunc(MyLineNotifyFunc);

    {Set options}
    if not(UseEpson or UseEpson24) then
      FP^.SetPcl5Mode(UsePcl5Mode);
    FP^.SetScaling(UseScaling);
    FP^.SetHeader(True);
    FP^.SetBlankLineSuppression(SuppressBlanks);

    {Some status stuff...}
    WriteLn;
    WriteLn('Press <Escape> to abort');
    Write('Printing - Page:                          ');

    FP^.PrintFax(PrintDest, FaxFN);

    Dispose(FP, Done);
  end;

  {!!.01 moved down}
  if UseEpson then
    Dispose(EpsonPrinterPtr(Printer), Done)
  else if UseEpson24 then
    Dispose(Epson24PinPrinterPtr(Printer), Done)
  else if UseHP300 then
    Dispose(HP300PrinterPtr(Printer), Done)
  else
    Dispose(HP150PrinterPtr(Printer), Done);

  Writeln;
  case AsyncStatus of
    ecOk        : WriteLn(FaxFN, ' printed successfully');
    ecUserAbort : WriteLn('Print canceled');
  else
    WriteLn('Error: ', StatusStr(AsyncStatus), '.');
  end;

end.
