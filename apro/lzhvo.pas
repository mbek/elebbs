{$S-,R-,V-,I-,B-,F-}
{$M 4096,65536,655360}

{Conditional defines that may affect this program}
{$I APDEFINE.INC}

{*********************************************************}
{*                    LZHVO.PAS 2.03                     *}
{*     Copyright (c) TurboPower Software 1991, 1993.     *}
{*                 All rights reserved.                  *}
{*********************************************************}


program LzhVO;
  {-Lzh file viewer}

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
  Version = 'LzhVO. Copyright (c) 1991 TurboPower Software. Version 2.03';
  ShowDetail : Boolean = False;

type
  String2 = string[2];
  String4 = string[4];
  String5 = string[5];
  String8 = string[8];
  String21 = string[21];

var
  LzhMask   : PathStr;
  LzhName   : PathStr;
  U         : UnLzh;
  LFL       : LzhFileList;
  FML       : FileMaskList;
  SRec      : SearchRec;
  Result    : Word;

  function GetRatio(OldSize, NewSize : LongInt) : Byte;
    {-Given OldSize and NewSize, return a compression ratio}
  begin
    if OldSize = 0 then
      GetRatio := 0
    else
      GetRatio := 100-((NewSize*100) div OldSize);
  end;

  function ZeroDigit(B : Byte) : String2;
    {-Convert a value in the range 0..99 to a string, left padded with 0}
  var
    S : string[2];
  begin
    Str(B:2, S);
    if S[1] = ' ' then
      S[1] := '0';
    ZeroDigit := S;
  end;

  function MakeDateSt(var DT : DateTime) : String8;
    {-Return a string representing a file's date stamp}
  begin
    with DT do
      MakeDateSt :=
        ZeroDigit(Month)+'-'+ZeroDigit(Day)+'-'+ZeroDigit(Year-1900);
  end;

  function MakeTimeSt(var DT : DateTime) : String5;
    {-Return a string representing a file's time stamp}
  begin
    with DT do
      MakeTimeSt := ZeroDigit(Hour)+':'+ZeroDigit(Min);
  end;

  function FullDateTime(var DT : DateTime) : String21;
    {-Return a string representing a file's date and time stamp}
  const
    MonthSt : array[1..12] of string[3] = (
      'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct',
      'Nov', 'Dec');
  var
    DateSt : string[20];
    TimeSt : string[8];
  begin
    with DT do begin
      Str(Year, DateSt);
      DateSt := MonthSt[Month]+' '+ZeroDigit(Day)+','+DateSt;
      TimeSt := MakeTimeSt(DT)+':'+ZeroDigit(Sec);
      FullDateTime := DateSt+'  '+TimeSt;
    end;
  end;

  function MakeAttrSt(A : Word) : String4;
    {-Return a string representing the external attributes of a file}
  begin
    MakeAttrSt[0] := #4;
    if A and Dos.Archive <> 0 then
      MakeAttrSt[1] := 'a'
    else
      MakeAttrSt[1] := '-';
    if A and SysFile <> 0 then
      MakeAttrSt[2] := 's'
    else
      MakeAttrSt[2] := '-';
    if A and Hidden <> 0 then
      MakeAttrSt[3] := 'h'
    else
      MakeAttrSt[3] := '-';
    if A and ReadOnly <> 0 then
      MakeAttrSt[4] := 'r'
    else
      MakeAttrSt[4] := 'w';
  end;

  procedure ShowFile(var LN : LzhNode; First : Boolean);
    {-Show a one-line report on a file in the archive}
  const
    Header1 =
      ' Length  Method  Size  Ratio   Date    Time   CRC   Attr  Name';
    Header2 =
      ' ------  ------  ----- ----- --------  -----  ----  ----  ------------';
  var
    DT : DateTime;
  begin
    if First then
      WriteLn(^M^J, Header1, ^M^J, Header2);

    with LN, lnLH do begin
      UnpackTime(MakeLongInt(Date, Time), DT);

      WriteLn(
        OrigSize:7, '  ',
        HeadID,
        NewSize:8, '  ',
        GetRatio(OrigSize, NewSize):2, '%  ',
        MakeDateSt(DT), '  ',
        MakeTimeSt(DT), '  ',
        HexW(Crc), '  ',
        MakeAttrSt(Attr), '  ', FName);
    end;
  end;

  procedure ShowFileDetailed(var LN : LzhNode);
    {-Show a detailed report on a file in the archive}
  var
    DT : DateTime;
  begin
    with LN, lnLH do begin
      UnpackTime(MakeLongInt(Date, Time), DT);
      WriteLn(^M^J,
                  'Filename: ':21, FName, ^M^J,
                'Attributes: ':21, MakeAttrSt(Attr), ^M^J,
             'Date and time: ':21, FullDateTime(DT), ^M^J,
        'Compression method: ':21, HeadID, ^M^J,
           'Compressed size: ':21, NewSize, ^M^J,
         'Uncompressed size: ':21, OrigSize, ^M^J,
          '16-bit CRC value: ':21, HexW(Crc) );
    end;
  end;

  procedure ViewFiles;
    {-View the files listed in the central directory}
  const
    Header3 =
      ' ------         ------  ---                               ---';
  var
    LNP : LzhNodePtr;
    TotalLen  : LongInt;
    TotalSize : LongInt;
  begin
    {initialize counters}
    TotalLen  := 0;
    TotalSize := 0;

    LNP := LFL.lfHead;
    while LNP <> nil do
      with LNP^ do begin
        Inc(TotalLen, lnLH.OrigSize);
        Inc(TotalSize, lnLH.NewSize);

        if ShowDetail then
          ShowFileDetailed(LNP^)
        else
          ShowFile(LNP^, LNP = LFL.lfHead);

        LNP := lnNext;
      end;

    if not ShowDetail then
      WriteLn(
        Header3, ^M^J, TotalLen:7, TotalSize:15, '  ',
        GetRatio(TotalLen, TotalSize):2, '%', LFL.lfCount:34);
  end;

  procedure ShowHelp;
    {-Displays help message with LZHVO options}
  begin
    WriteLn('Usage: LZHVO [Options] Filename[.LZH] [files...]'^M^J);
    WriteLn('Parameters:');
    WriteLn('  /T          technical (detailed) listing');
    WriteLn('  Filename    name of LZH file to view; LZH extension assumed');
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
    LzhMask := '';
    FML.Init;

    for I := 1 to ParamCount do begin
      Opt := ParamStr(I);
      if (Opt[1] = '/') or (Opt[1] = '-') then
        if Length(Opt) <> 2 then
          InvalidOption
        else
          case UpCase(Opt[2]) of
            'T' : ShowDetail := True;
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
    end;

    {make sure we have a LZH file}
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

  while (DosError = 0) do begin
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
    if ShowDetail then
      U.arOptionsOn(arReadFileComments);
    U.arOptionsOn(arReadArcComments);
    U.SetShowNameProc(DefShowNameProc);

    {build a list of files to be viewed}
    LFL.Init;
    U.BuildLzhFileList(LFL, FML);

    {report errors}
    Result := U.GetLastError mod 10000;
    case Result of
      ecOK :
        ViewFiles;
      ecOutOfMemory :
        WriteLn('Insufficient memory');
      ecBadFileFormat :
        WriteLn('Bad LZH file format');
      ecNoMatchingFiles :
        WriteLn('No matching file(s) found', ' in ', LzhName);
      ecNotAnLzhFile :
        WriteLn(LzhName, ' is not a valid LZH file');
      else
        WriteLn('I/O error ', Result);
    end;

    {halt if we failed}
    case Result of
      ecOK, ecNoMatchingFiles :
        {ok} ;
      else
        Halt(1);
    end;

    {dispose of the LZH file list}
    LFL.Done;

    {close the LZH file}
    U.Done;

    {get next file, if any}
    FindNext(SRec);
    if DosError = 0 then
      WriteLn;
  end;
end.
