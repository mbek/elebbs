{$S-,R-,V-,I-,B-,F-}
{$M 4096,65536,655360}

{Conditional defines that may affect this program}
{$I APDEFINE.INC}

{*********************************************************}
{*                    LZHXO.PAS 2.03                     *}
{*     Copyright (c) TurboPower Software 1991, 1993.     *}
{*                 All rights reserved.                  *}
{*********************************************************}


program LzhXO;
  {-Lzh file extraction utility}

uses
  {$IFDEF LeakCheck}                                                   {!!.01}
  LeakChek,                                                            {!!.01}
  {$ENDIF}                                                             {!!.01}
  Dos,
  {$IFDEF UseOpro}
  OpString,
  {$ENDIF}
  {$IFDEF UseTPro}
  TpString,
  {$ENDIF}
  ApMisc,
  OoArchiv,
  OoLzh;

const
  Version = 'LzhXO. Copyright (c) 1991 TurboPower Software. Version 2.03';

  CreateDirs : Boolean = False;       {True to create directories stored in LZH}
  OverwriteFiles : Boolean = False;   {True to overwrite existing files}
  OverwriteIfNewer : Boolean = False; {True to overwrite if files are newer}

var
  LzhMask : PathStr;              {mask for directory search}
  LzhName : PathStr;              {name of LZH file}
  U       : UnLzh;                {object used for dearchiving}
  FML     : FileMaskList;         {list of files to extract}
  SRec    : SearchRec;            {used for finding LZH files}
  OutPath : PathStr;              {path for output files}
  OutName : PathStr;              {name of file to extract}
  SaveOF  : Boolean;              {saved value of OverwriteFiles}
  Result  : Word;

  function GetKey : Word;
    {-Get a key}
  var
    Regs : Registers;
  begin
    Regs.AH := 0;
    Intr($16, Regs);
    GetKey := Regs.AX;
  end;

  function WaitForKey : Word;
    {-Wait for a keypress and throw away the result}
  var
    Key : Word;
    Ch  : Char absolute Key;
  begin
    {halt if ^Break or ^C pressed}
    Key := GetKey;
    if (Key = $0000) or (Ch = ^C) then
      Halt;
    WaitForKey := Key;
  end;

  function GetConfirmation(Def : Char) : Boolean;
    {-Confirm a replace/copy operation}
  var
    Key : Word;
    Ch  : Char absolute Key;
  begin
    GetConfirmation := True;
    Write(' (Y/N/A/Q) [', Def, ']'^H^H);
    repeat
      Key := WaitForKey;
      if Ch = ^M then
        Ch := Def;
      Ch := Upcase(Ch);
    until Pos(Ch, 'YNAQ') <> 0;
    Write(Ch);
    case Ch of
      'A' : OverwriteFiles := True;
      'Q' : Halt;
      'N' : GetConfirmation := False;
    end;
  end;

{$F+}

  function OkToWrite(LNP : LzhNodePtr; var FName : PathStr; UP : UnLzhPtr) : Boolean;
    {-Return True if it is OK to (over)write FName}
  var
    SR : SearchRec;
  begin
    OkToWrite := True;
    if not OverwriteFiles then begin
      FindFirst(FName, $6, SR);
      if DosError <> 0 then
        Exit
      else if OverwriteIfNewer then
        with LNP^.lnLH do
          OkToWrite := (MakeLongInt(Date, Time) > SR.Time)
      else begin
        Write('Warning: ', FName, ' exists. Overwrite it?');
        OkToWrite := GetConfirmation('N');
        WriteLn;
      end;
    end;
  end;

{$F-}

  procedure ShowHelp;
    {-Displays help message with LZHXO options}
  begin
    WriteLn('Usage: LZHXO [Options] Filename[.LZH] [files...]'^M^J);
    WriteLn('Parameters:');
    WriteLn('  /D          create directories stored in LZH file');
    WriteLn('  /N          extract only newer files');
    WriteLn('  /O          overwrite existing files');
    WriteLn('  /P path     path for output files');
    WriteLn('  Filename    name of LZH file; LZH extension assumed');
    WriteLn('  files...    one or more file masks; may contain wildcards');
    WriteLn('  /?          display this help screen');
    Halt(0);
  end;

  procedure Initialize;
    {-Initialize and check for command line parameters}
  var
    I   : Word;
    Opt : string[127];

    procedure InvalidOption;
      {-displays error message and aborts}
    begin
      WriteLn(Opt, ' is an invalid option');
      ShowHelp;
    end;

  begin
    {initialize global variables}
    LzhMask := '';
    OutPath := '';
    FML.Init;

    I := 1;
    while I <= ParamCount do begin
      Opt := ParamStr(I);
      if (Opt[1] = '/') or (Opt[1] = '-') then
        if Length(Opt) <> 2 then
          InvalidOption
        else
          case UpCase(Opt[2]) of
            'D' : CreateDirs := True;
            'N' : OverwriteIfNewer := True;
            'O' : OverwriteFiles := True;
            'P' : begin
                    Inc(I);
                    OutPath := AddBackslash(ParamStr(I));
                    OutPath := StUpcase(OutPath);
                  end;
            '?' : ShowHelp;
            else InvalidOption;
          end
      else if LzhMask = '' then begin
        LzhMask := DefaultExtension(Opt, 'LZH');
        LzhMask := StUpCase(LzhMask);
      end
      else if not FML.Append(Opt) then begin
        WriteLn('Insufficient memory');
        Halt(1);
      end;

      Inc(I);
    end;

    {make sure we have a LZH file mask}
    if LzhMask = '' then
      ShowHelp;
  end;

begin
  {display signon message}
  WriteLn(Version);
  WriteLn;

  {get command line parameters}
  Initialize;

  {get the first matching file}
  FindFirst(LzhMask, $6, SRec);
  if DosError <> 0 then
    WriteLn('No matching file(s) found');

  {save value of OverwriteFiles}
  SaveOF := OverwriteFiles;

  while (DosError = 0) do begin
    {reset OverwriteFiles}
    OverwriteFiles := SaveOF;

    {get name of next LZH file}
    LzhName := AddBackSlash(JustPathName(LzhMask))+SRec.Name;

    {try to open the LZH file}
    if not U.Init(LzhName) then begin
      if ArchiveStatus = ecNotAnLzhFile  then
        WriteLn(LzhName, ' is not a valid LZH file')
      else
        WriteLn('Unable to open ', LzhName);
      Halt(1);
    end;

    {set options}
    if CreateDirs then
      U.arOptionsOn(arCreateDirs);
    U.arOptionsOn(arReadArcComments);
    U.SetShowNameProc(DefShowNameProc);
    U.SetOkToWriteFunc(OkToWrite);
    U.SetShowMethodProc(DefShowMethodProc);
    U.SetExtractSuccessFunc(DefExtractSuccessFunc);
    U.SetShowProgressFunc(DefShowProgressFunc);
    U.SetOutputPath(OutPath);

    {extract all matching files}
    U.ExtractFileMaskList(FML);

    {report errors}
    Result := U.GetLastError mod 10000;
    case Result of
      ecOK :
        {ok};
      ecPathNotFound :
        WriteLn('Path not found');
      ecInvalidDrive :
        WriteLn('Invalid drive');
      ecDiskFull :
        WriteLn('Disk full');
      ecDriveNotReady :
        WriteLn('Drive not ready');
      ecOutOfMemory :
        WriteLn('Insufficient memory');
      ecBadFileFormat :
        WriteLn('Bad LZH file format');
      ecNoMatchingFiles :
        WriteLn('No matching file(s) found', ' in ', LzhName);
      ecUserAbort :
        WriteLn('^C');
      else
        WriteLn('I/O error ', Result);
    end;

    {halt if we failed}
    case Result of
      ecOK, ecNoMatchingFiles :
        {ok} ;
      ecUserAbort :
        Halt(0);
      else
        Halt(1);
    end;

    {close the LZH file}
    U.Done;

    {get next file, if any}
    FindNext(SRec);
    if DosError = 0 then
      WriteLn;
  end;
end.
