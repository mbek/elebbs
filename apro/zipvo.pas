{$S-,R-,V-,I-,B-,F-}
{$M 4096,65536,655360}

{Conditional defines that may affect this program}
{$I APDEFINE.INC}

{*********************************************************}
{*                    ZIPVO.PAS 2.03                     *}
{*          Copyright (c) TurboPower Software.           *}
{*                 All rights reserved.                  *}
{*********************************************************}

program ZipVO;
  {-Zip file viewer}

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
  OoZip;

const
  Version = 'ZipVO. Copyright (c) 1991 TurboPower Software. Version 2.03.';

  ShowDetail : Boolean = False;
type
  String2 = string[2];
  String3 = string[3];
  String5 = string[5];
  String7 = string[7];
  String8 = string[8];
  String21 = string[21];
var
  ZipMask   : PathStr;
  ZipName   : PathStr;
  U         : UnZip;
  ZFL       : ZipFileList;
  FML       : FileMaskList;
  SRec      : SearchRec;
  Result    : Word;

  function GetRatio(OldSize, NewSize : LongInt) : Byte;
    {-Given OldSize and NewSize, return a compression ratio}
  begin
    if NewSize <=0 then                                                {!!.02}
      GetRatio := 0                                                    {!!.02}
    else                                                               {!!.02}
      GetRatio := 100-((NewSize*100) div OldSize);
  end;

  function MakeMethodSt(Method : Byte) : String7;
    {-Return a string representing a compression method}
  begin
    case Method of
      cmcStored :
        MakeMethodSt := 'Stored ';
      cmcShrunk :
        MakeMethodSt := 'Shrunk ';
      cmcReduced1..cmcReduced4 :
        MakeMethodSt := 'Reduced';
      cmcImploded :
        MakeMethodSt := 'Implode';
      cmcDeflated :                                                    {!!.01}
        MakeMethodSt := 'Deflate';                                     {!!.01}
      else
        MakeMethodSt := '???????'
    end;
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

  function MakeAttrSt(A : Word) : String3;
    {-Return a string representing the external attributes of a file}
  begin
    MakeAttrSt[0] := #3;
    if A and SysFile <> 0 then
      MakeAttrSt[1] := 's'
    else
      MakeAttrSt[1] := '-';
    if A and Hidden <> 0 then
      MakeAttrSt[2] := 'h'
    else
      MakeAttrSt[2] := '-';
    if A and ReadOnly <> 0 then
      MakeAttrSt[3] := 'r'
    else
      MakeAttrSt[3] := 'w';
  end;

  procedure ShowFile(var CDH : CentralDirHead;
                     FName   : PathStr;
                     First   : Boolean);
    {-Show a one-line report on a file in the archive}
  const
    Header1 =
      ' Length  Method   Size  Ratio   Date    Time   CRC     Attr  Name';
    Header2 =
      ' ------  -------  ----- ----- --------  -----  ------  ----  ------------';
  var
    DT : DateTime;
  begin
    if First then
      WriteLn(^M^J, Header1, ^M^J, Header2);

    with CDH do begin
      UnpackTime(MakeLongInt(LastModDate, LastModTime), DT);

      WriteLn(
        OrigSize:7, '  ',
        MakeMethodSt(Method),
        NewSize:7, '  ',
        GetRatio(OrigSize, NewSize):2, '%  ',
        MakeDateSt(DT), '  ',
        MakeTimeSt(DT), '  ',
        HexL(Crc), ' ',
        MakeAttrSt(ExternalAttrs), '  ', FName);
    end;
  end;

  function GetVersion(Ver : Byte) : String3;
    {-Return a string representing a version number}
  begin
    GetVersion[0] := #3;
    GetVersion[1] := Char(Ver div 10+Ord('0'));
    GetVersion[2] := '.';
    GetVersion[3] := Char(Ver mod 10+Ord('0'));
  end;

  function Long2Str(L : LongInt) : string;
    {-Convert a long/word/integer/byte/shortint to a string}
  var
    S : string;
  begin
    Str(L, S);
    Long2Str := S;
  end;

  procedure ShowFileDetailed(var CDH : CentralDirHead;
                             FName : PathStr;
                             CP  : CommentPtr);
    {-Show a detailed report on a file in the archive}
  const
    BinaryText : array[Boolean] of string[6] = ('binary', 'text');
  var
    DT : DateTime;
    I : Word;
    MSt : string[20];
  begin
    with CDH do begin
      MSt := MakeMethodSt(Method);
      case Method of
        cmcReduced1..cmcReduced4 :
          MSt := MSt+' (factor '+Long2Str(Method-cmcReduced1+1)+')';
      end;
      UnpackTime(MakeLongInt(LastModDate, LastModTime), DT);
      WriteLn(^M^J,
                  'Filename: ':21, FName, ^M^J,
                 'File type: ':21, BinaryText[Odd(InternalAttrs)], ^M^J,
                'Attributes: ':21, MakeAttrSt(ExternalAttrs) );
      if CP <> nil then begin
        Write('Comment: ':21);
        for I := 1 to CDH.CommentLength do
          Write(CP^[I]);
        WriteLn;
      end;
      WriteLn(
             'Date and time: ':21, FullDateTime(DT), ^M^J,
        'Compression method: ':21, MSt, ^M^J,
           'Compressed size: ':21, NewSize, ^M^J,
         'Uncompressed size: ':21, OrigSize, ^M^J,
          '32-bit CRC value: ':21, HexL(Crc), ^M^J,
               'Name length: ':21, NameLength, ^M^J,
              'Extra length: ':21, ExtraLength, ^M^J,
            'Comment length: ':21, CommentLength, ^M^J,
         'Disk number start: ':21, DiskNumberStart, ^M^J,
            'Internal attrs: ':21, HexW(InternalAttrs), ^M^J,
            'External attrs: ':21, HexL(ExternalAttrs), ^M^J,
          'Local header ofs: ':21, HexL(LocalHeaderOfs), ^M^J,
                'Created by:   PKZIP ':29, GetVersion(VersionMade), ^M^J,
         'Needed to extract: PKUNZIP ':29, GetVersion(VersionNeeded) );
    end;
  end;

  procedure ViewFiles;
    {-View the files listed in the central directory}
  const
    Header3 =
      ' ------          ------  ---                                 ---';
  var
    ZNP : ZipNodePtr;
    TotalLen  : LongInt;
    TotalSize : LongInt;
  begin
    {initialize counters}
    TotalLen  := 0;
    TotalSize := 0;

    ZNP := ZFL.zfHead;
    while ZNP <> nil do
      with ZNP^ do begin
        Inc(TotalLen, znCDH.OrigSize);
        Inc(TotalSize, znCDH.NewSize);

        if ShowDetail then
          ShowFileDetailed(znCDH, znFName, znCP)
        else
          ShowFile(znCDH, znFName, ZNP = ZFL.zfHead);

        ZNP := znNext;
      end;

    if not ShowDetail then
      WriteLn(
        Header3, ^M^J, TotalLen:7, TotalSize:16, '  ',
        GetRatio(TotalLen, TotalSize):2, '%', ZFL.zfCount:36);
  end;

  procedure ShowHelp;
    {-Displays help message with ZIPV options}
  begin
    WriteLn('Usage: ZIPVO [Options] Filename[.ZIP] [files...]'^M^J);
    WriteLn('Parameters:');
    WriteLn('  /T          technical (detailed) listing');
    WriteLn('  Filename    name of ZIP file to view; ZIP extension assumed');
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
    ZipMask := '';
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
      else if ZipMask = '' then begin
        ZipMask := DefaultExtension(Opt, 'ZIP');
        ZipMask := StUpCase(ZipMask);
      end
      else if not FML.Append(Opt) then begin
        WriteLn('Insufficient memory');
        Halt(1);
      end;
    end;

    {make sure we have a ZIP file}
    if ZipMask = '' then
      ShowHelp;
  end;

begin
  {display signon message}
  WriteLn(Version);
  WriteLn;

  {get command line parameters}
  Initialize;

  {get the first matching file}
  FindFirst(ZipMask, $6, SRec);
  if DosError <> 0 then
    WriteLn('No matching file(s) found');

  while (DosError = 0) do begin
    {get name of next ZIP file}
    ZipName := AddBackSlash(JustPathName(ZipMask))+SRec.Name;

    {try to open the zip file}
    if not U.Init(ZipName) then begin
      if ArchiveStatus = ecNotAZipFile  then
        WriteLn(ZipName, ' is not a valid ZIP file')
      else
        WriteLn('Unable to open ', ZipName);
      Halt(1);
    end;

    {set options}
    if ShowDetail then
      U.arOptionsOn(arReadFileComments);
    U.arOptionsOn(arReadArcComments);
    U.SetShowCommentsProc(DefShowCommentsProc);

    {build a list of ZIPped files to be viewed}
    ZFL.Init;
    U.BuildZipFileList(ZFL, FML);

    {report errors}
    Result := U.GetLastError mod 10000;
    case Result of
      ecOK :
        ViewFiles;
      ecOutOfMemory :
        WriteLn('Insufficient memory');
      ecBadFileFormat :
        WriteLn('Bad ZIP file format');
      ecNoMatchingFiles :
        WriteLn('No matching file(s) found', ' in ', ZipName);
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

    {dispose of the zip file list}
    ZFL.Done;

    {close the zip file}
    U.Done;

    {get next file, if any}
    FindNext(SRec);
    if DosError = 0 then
      WriteLn;
  end;
end.
