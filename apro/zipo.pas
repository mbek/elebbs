{$S-,R-,V-,I-,B-,F-,A+}
{$M 4096,65536,655360}

{Conditional defines that may affect this program}
{$I APDEFINE.INC}

{*********************************************************}
{*                    ZIPO.PAS 2.03                      *}
{*         Copyright (c) TurboPower Software.           *}
{*                 All rights reserved.                  *}
{*********************************************************}

program ZipO;
  {-Zip file compression utility (OOP)}

uses
  {$IFDEF LeakCheck}                                                   {!!.01}
  LeakChek,                                                            {!!.01}
  {$ENDIF}                                                             {!!.01}
  Dos,
  {$IFDEF UseOpro}
  OpCrt,
  OpString,
  {$ENDIF}
  {$IFDEF UseTPro}
  TpCrt,
  TpString,
  {$ENDIF}
  {$IFDEF Standalone}
  Crt,
  {$ENDIF}
  ApMisc,
  OoArchiv,
  OoZip;

const
  Version = 'ZipO. Copyright (c) TurboPower Software. Version 2.03';

  StripPath       : Boolean = False;     {strip pathname}
  NewArchive      : Boolean = False;     {true if archive was created}
  HaltSoon        : Boolean = False;     {true to halt as soon as possible}
  AllOk           : Boolean = True;      {true to skip confirmations}
  GotOne          : Boolean = False;     {helps us delete empty files}
  AddZipComment   : Boolean = False;     {true if zip comment wanted}
  AddFileComments : Boolean = False;     {true if file comments wanted}
  UseDriveLetter  : Boolean = False;     {true to keep driver letter}

type
  ActionType = (zipAdd, zipDelete, zipFreshen, zipUpdateComments, zipNoAction);

var
  Z            : Zip;               {Zip object}
  ZipName      : PathStr;           {name of ZIP file}
  FML          : FileMaskList;      {list of files to extract}
  Action       : ActionType;        {action to perform this pass}
  F            : File;              {only needed for deleting bad archives}
  Results      : Word;              {result code of compression}
  ZipComment   : String[127];       {new ZIP comment}
  ForcedMode   : CompressionMode;   {used to set the compression mode}
  DeflateLevel : Byte;              {used to set Deflate level}  {!!.01}

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
      HaltSoon := True;
    WaitForKey := Key;
  end;

  function GetConfirmation(NewFile : PathStr; Def : Char) : Boolean;
    {-Confirm a replace/copy operation}
  var
    Key : Word;
    Ch  : Char absolute Key;
  begin
    GetConfirmation := True;
    Write('Compress ', JustFileName(NewFile));
    Write(' (Y/N/A/Q) [', Def, ']'^H^H);
    repeat
      Key := WaitForKey;
      if Ch = ^M then
        Ch := Def;
      Ch := Upcase(Ch);
    until Pos(Ch, 'YNAQ') <> 0;
    Write(Ch);
    case Ch of
      'A' : AllOk := True;
      'Q' : HaltSoon := True;
      'N' : GetConfirmation := False;
    end;
  end;

  {$F+}
  function MyShowProgressFunc(UP : UnZipPtr;
                              BytesWritten, TotalBytes : LongInt) : Boolean;
    {-Checks for user abort, then calls default function to show progress}
  var
    C : Char;
  begin
    {Handle pending halts}
    if HaltSoon then begin
      MyShowProgressFunc := False;
      Exit;
    end;

    {Check for abort key}
    while KeyPressed do begin
      C := ReadKey;
      if C = #0 then
        C := ReadKey
      else if C = #27 then begin
        MyShowProgressFunc := False;
        Exit;
      end;
    end;

    {Call default function to show progress}
    MyShowProgressFunc := DefShowProgressFunc(UP, BytesWritten, TotalBytes);
  end;
  {$F-}

  {$F+}
  function MyOkToCompressFunc(NewName, OldName : PathStr;
                              var CDH : CentralDirHead;
                              ZP : ZipPtr) : Boolean;
    {-Call old function to get OK, then request confirmation from user}
  var
    OK : Boolean;
  begin
    {Get OK from default compress function}
    OK := DefOkToCompressFunc(NewName, OldName, CDH, ZP);

    {If no additional confirmation needed, just set result and exit}
    if AllOk then begin
      MyOkToCompressFunc := OK;
      Exit;
    end;

    {If still OK, go ask user}
    if OK then begin
      OK := GetConfirmation(NewName, 'Y');
      if OK then
        GotOne := True;
      MyOkToCompressFunc := OK;
      WriteLn;
    end else
      MyOktoCompressFunc := False;
  end;
  {$F-}

  procedure ShowHelp;
    {-Displays help message with ZIP options}
  begin
    WriteLn('Usage: ZIPO [Options] Filename[.ZIP] [files...]'^M^J);
    WriteLn('Parameters:');
    WriteLn('  /A          add files to directory');
    WriteLn('  /D          delete files from directory');
    WriteLn('  /F          freshen files in this archive');
    WriteLn('  /K          prompt for a file comment for every existing file in ZIP file');
    WriteLn('  /S          strip the path name when adding');
    WriteLn('  /C          request confirmations');
    WriteLn('  /Z          add ZIP comment (you will be prompted for the comment)');
    WriteLn('  /P          prompt for file comments while compressing new files');
    WriteLn('  /L          keep drive letter with file name');
    WriteLn('  /M#         override compression method (# can be 1 thru 4:');{!!.01}
    WriteLn('              1=store, 2=shrink, 3=implode, or 4=deflate)');{!!.01}
    WriteLn('  /Y#         specify deflate level (# can be 1 thru 9:');{!!.01}
    WriteLn('              1=fastest, 9=smallest, 5=default)');        {!!.01}
    WriteLn('  Filename    name of ZIP file; ZIP extension assumed');
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
    Action := zipNoAction;
    ZipName := '';
    ZipComment := '';
    FML.Init;
    ForcedMode := cmBestMethod;
    DeflateLevel := 5;  {default}                                      {!!.01}

    I := 1;
    while I <= ParamCount do begin
      Opt := ParamStr(I);
      if (Opt[1] = '/') or (Opt[1] = '-') then begin
        if (Length(Opt) <> 2) and (Length(Opt) <> 3) then
          InvalidOption;
        if Length(Opt) = 3 then begin
          case UpCase(Opt[2]) of                                       {!!.01}
            'M' : begin                                                {!!.01}
              if not (Opt[3] In ['1'..'4']) then                       {!!.01}
                InvalidOption                                          {!!.01}
            end;                                                       {!!.01}
            'Y' : begin                                                {!!.01}
              if not (Opt[3] In ['1'..'9']) then                       {!!.01}
                InvalidOption                                          {!!.01}
            end;                                                       {!!.01}
            else                                                       {!!.01}
              InvalidOption;                                           {!!.01}
          end;                                                         {!!.01}
        end;
        case UpCase(Opt[2]) of
          'A' : Action := zipAdd;
          'D' : Action := zipDelete;
          'F' : Action := zipFreshen;
          'K' : Action := zipUpdateComments;
          'S' : StripPath := True;
          'C' : AllOk := False;
          'Z' : AddZipComment := True;
          'P' : AddFileComments := True;
          'L' : UseDriveLetter := True;
          'M' : ForcedMode := CompressionMode(Ord(Opt[3])-Ord('0'));
          'Y' : DeflateLevel := Byte(Ord(Opt[3]) - Ord('0'));          {!!.01}
          '?' : ShowHelp;
        else
          InvalidOption;
        end
      end else if ZipName = '' then begin
        ZipName := DefaultExtension(Opt, 'ZIP');
        ZipName := StUpCase(ZipName);
      end else if not FML.Append(Opt) then begin
        WriteLn('Insufficient memory');
        Halt(1);
      end;

      Inc(I);
    end;

    {make sure we have a ZIP file mask}
    if ZipName = '' then
      ShowHelp;
  end;

begin
  {Display signon message}
  WriteLn(Version);
  WriteLn;

  {Allow output redirection}
  Assign(Output, '');
  Rewrite(Output);

  {Get command line parameters}
  Initialize;

  CheckBreak := False;

  {Open/create ZIP file}
  Z.Init(ZipName);
  if ArchiveStatus <> ecOk then
    if (Action = zipFreshen) or (Action = zipDelete) then begin
      WriteLn('Can''t find archive (', ZipName, ')');
      Halt(1);
    end else begin
      Z.Create(ZipName);
      NewArchive := True;
    end;
  if ArchiveStatus <> ecOk then begin
    WriteLn('Failed to open ', ZipName, ', error was ', ArchiveStatus);
    Halt(1);
  end;

  {Set the compression mode}
  Z.SetCompressionMode(ForcedMode);

  {Statement to adjust the speed/compression for the deflate method}   {!!.01}
  Z.SetDeflateLevel(DeflateLevel);                                     {!!.01}

  {Set options}
  Z.SetShowMethodProc(DefShowMethodProc);
  Z.SetShowProgressFunc(MyShowProgressFunc);
  Z.SetShowCommentsProc(DefShowCommentsProc);

  {Set compressing-only options}
  Z.SetOkToCompressFunc(MyOkToCompressFunc);
  Z.SetCompressSuccessFunc(DefCompressSuccessFunc);

  {Use default FileComment function if file comments requested}
  if AddFileComments or (Action = zipUpdateComments) then
    Z.SetFileCommentFunc(DefFileCommentFunc);

  {Don't store drive letter for PKZIP 2.0 compatibility}
  if not UseDriveLetter then
    Z.arOptionsOn(arNoDriveLetter);

  Z.arOptionsOn(arRemoveDots);

  if StripPath then
    Z.arOptionsOn(arStripPath);
  GotOne := AllOk;

  {Perform requested action}
  case Action of
    zipAdd :
      Z.CompressFileMaskList(FML);
    zipDelete :
      Z.DeleteFileMaskList(FML);
    zipFreshen :
      Z.FreshenArchive;
    zipUpdateComments :
      Z.UpdateCommentsFileMaskList(FML);
    zipNoAction :
      if not AddZipComment then
        if NewArchive then begin
          Assign(F, ZipName);
          Erase(F);
          if IOResult <> 0 then ;
          ShowHelp;
        end;
  end;

  {report errors}
  Results := Z.GetLastError mod 10000;
  case Results of
    ecOk : ;
    ecEmptyFileMaskList :
      WriteLn(^M^J'No files to compress');
    ecDiskFull :
      WriteLn(^M^J'Failed due to insufficient disk space');
    ecFileNotFound :
      WriteLn(^M^J'Failed because a file in the list wasn''t found');
    ecPathNotFound :
      Writeln(^M^J'Failed because a file in the list had a bad path name');
    ecAccessDenied :
      WriteLn(^M^J'Failed because a file in the list couldn''t be read (access denied)');
    ecInvalidDrive :
      WriteLn(^M^J'Failed because a file in the list specified an invalid drive');
    ecUserAbort :
      WriteLn(^M^J'Canceled on request, old archive was restored');
    else
      WriteLn(^M^J'Failed due to error ', Results);
  end;

  {Add new ZIP comment if requested}
  if (Results = ecOK) and AddZipComment then begin
    WriteLn('Enter new ZIP comment below:');
    ZipComment := Char(0);
    ReadLn(ZipComment);
    if ZipComment[1] <> Char(0) then begin
      if ZipComment = ' ' then
        ZipComment := '';
      Z.SetZipComment(ZipComment[1], Length(ZipComment));
      Results := Z.GetLastError mod 10000;
      if Results = ecOk then
        WriteLn(^M^J'ZIP comment updated')
      else
        WriteLn(^M^J'Failed to add ZIP comment, result was ', Results);
    end;
  end;

  {Delete archive if errors occurred and this was a new file}
  if NewArchive and ((Results <> ecOk) or not GotOne) then begin
    Assign(F, ZipName);
    Erase(F);
    if IOResult <> 0 then ;
  end;

  {close the ZIP file}
  Z.Done;
  FML.Done;
end.

