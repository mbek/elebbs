unit ViewFile;
(*
**
** Copyright (C) 1999-2003 Maarten Bekers. All rights reserved.
**
** This file is part of EleBBS, EleXer and EleWEB.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
** This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
** WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
**
*)
{$I COMPILER.INC}
(*
**
** ViewFile (from tagging) routines for EleBBS
**
** Copyright (c) 1998 by Maarten Bekers
**
** Created : 01-May-1998
** Last update : 01-May-1998
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF WITH_FULL}
  This unit shouldn''t be compiled for an utility program
{$ENDIF}

uses Global, Bsc, CompSys, CfgRec;


procedure ViewTagFile(var ListRec : TagHeaderList;
                      var Exitinfo: ExitinfoRecord);
procedure ShowDirectFile(MiscData: String);
function  ArchiveListing(    MiscData: String;
                             DoUser  : Boolean;
                             TempStr : String;
                         var Exitinfo: ExitinfoRecord): Boolean;
function  ShowAnsiFile(    MiscData: String;
                           DoUser  : Boolean;
                           TempStr : String;
                       var Exitinfo: ExitinfoRecord): Boolean;
function  SearchFileInList(Name: String; var ListRec: TagHeaderList): Byte;
function  CheckGifFile(FName: String; var Width, Length, Colours: Word): Boolean;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Debug_U, ElLog_U, RAL, LineEd, LongStr, WordStr, FastScrn,
      FileSys, Objdec, CentrStr, Strpath, GenFile, Ra2Fdb, TagUnit,
       Input_U, GenDos, FileObj, MGrFdb,
        {$IFDEF MSDOS}
          Dos
        {$ENDIF}

        {$IFDEF OS2}
          Dos
        {$ENDIF}

        {$IFDEF GO32V2}
          Dos
        {$ENDIF}

        {$IFDEF ELEUNIX}
          Dos
        {$ENDIF}

        {$IFDEF WIN32}
          SysUtils
          {$IFDEF VirtualPascal}
            ,Dos
          {$ENDIF}
          {$IFDEF FPC}
            ,Dos
          {$ENDIF}
        {$ENDIF}, Cases, FileRout, ExecFunc, Colors, DispAns;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ViewTagFile(var ListRec : TagHeaderList;
                      var Exitinfo: ExitinfoRecord);
var TagNrStr  : String;
    PwdStr    : String;
    SaveName  : String;
    TagNumber : Byte;
    DidView   : Boolean;
    FileName  : String;
    SaveScrn  : Pointer;
    SavePause : Boolean;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileSys, 'ViewTagFile (begin)');
  {$ENDIF}

  Write('`X1:`A15:`E:', LangObj^.ralGet(ralViewName));

  SavePause := LineCfg^.PauseChecking;
  LineCfg^.PauseChecking := true;
  TagNrStr := '';

  GetString(TagNrStr, 80, [#32..#254], false, false, false, false);
  Write('`X1:`A15:`E:');

  SaveName := Trim(TagNrStr);
  TagNumber := Byte(FVal(FirstWord(TagNrStr, defExtractWord, true)));

  if TagNumber=00 then
   if SaveName <> '' then
    TagNumber := SearchFileInList(SaveName, ListRec);

  if TagNumber = 0 then
   if Savename <> '' then
    begin
      WriteLn('`A12:', LangObj^.ralGet(ralInvViewNm));
      InputObj^.PressEnter(False, True);
    end; { if }

  if TagNumber in [1..100] then
   if ListRec[TagNumber].PassWord <> '' then
     begin
       Write('`X1:`A12:`E:', LangObj^.ralGet(ralPassword));
       PwdStr := '';

       GetString(PwdStr, 15, [#32..#254], False, True, False, False);
       Write('`A3:`X1:`E:');

       if SUpCase(Trim(PwdStr)) = SUpCase(Trim(ListRec[TagNumber].Password)) then
         ListRec[TagNumber].Password := ''
           else begin
                  WriteLn('`A12:', LangObj^.ralGet(ralIncorrect));
                  InputObj^.PressEnter(false, true);

                  Ralog('>', 'Viewing password-protected file '+ListRec[TagNumber].Name);
                  RaLog('!', 'Used password "'+PwdStr+'"');
                end; { if }
     end; { if password }

  if TagNumber in [1..100] then
   if ListRec[TagNumber].Password = '' then
    begin
      SaveScreen(SaveScrn);

      FileName := GetLfnName(ListRec[TagNumber].Name,
                             ListRec[TagNumber].RecordNum, ListRec[TagNumber].AreaNum);

       DidView := Archivelisting(FStr(ListRec[TagNumber].AreaNum), False, FileName, Exitinfo);

       If (NOT DidView) AND (NOT CompSys_LowMemory) then
         DidView := ShowAnsiFile(FStr(ListRec[TagNumber].AreaNum) + ' /BINARY', false, Filename,
                                  Exitinfo);

      if DidView then
       begin
         RestoreScreen(SaveScrn);
         OutputObj^.DoRefresh;
       end { if }
         else DisposeScreen(SaveScrn);
    end; { Not a valid input }

  LineCfg^.PauseChecking := SavePause;
  Write('`X1:`E:');

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileSys, 'ViewTagFile ( end )');
  {$ENDIF}
end; { proc. ViewTagFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  CheckGifFile(FName: String; var Width, Length, Colours: Word): Boolean;
type GIFHeader = record
        Sig    : Array[0..2] of Char;                       { Signature, "GIF" }
        Ver    : Array[0..2] of Char;                    { Version, 87a or 89a }
        Width,                                                  { Screen width }
        Height : Word;                                         { Screen height }
        Attrib : Byte;
     end; { GIFHeader, not complete header but will do fine ;-) }

var Gif_F : pFileObj;
    GifHDR: GifHeader;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'CheckGifFile ('+FName+') - (begin)');
  {$ENDIF}
  CheckGifFile := false;

  New(Gif_F, Init);

  Gif_F^.Assign(Fname);
  Gif_F^.FileMode := ReadMode + DenyNone;
  Gif_F^.Open(1);
  Gif_F^.BlkRead(GifHDR, SizeOf(GifHeader));

  Dispose(Gif_F, Done);

  If GifHdr.Sig='GIF' then
    begin
      CheckGifFile := true;

      Width := GifHdr.Width;
      Length := GifHdr.Height;
      Colours := 2 SHL (GifHDR.Attrib AND 7);
    end; { valid GIF file }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'CheckGifFile ('+FName+') - ( end )');
  {$ENDIF}
end; { func. CheckGIFfile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  ArchiveListing(    MiscData: String;
                             DoUser  : Boolean;
                             TempStr : String;
                         var Exitinfo: ExitinfoRecord): Boolean;

procedure ShowEntry(var IBMInfo: IBM);
Const  Months     : Array[0..12] of String[3] =
                    ('???',
                     'Jan','Feb','Mar','Apr','May','Jun',
                     'Jul','Aug','Sep','Oct','Nov','Dec');
var TimeStr    : String;
    DateStr    : String;
    Counter    : Byte;

    FileDispStr: String;
    OrigSizeStr: String;
    CompSizeStr: String;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'ShowEntry (begin)');
  {$ENDIF}

  TimeStr := Trim(Copy(IBMInfo.FileDate, 1, 5));
  DateStr := Trim(Copy(IBMInfo.FileDate, 10, 12));

  for Counter := 00 to 12 do
    Replace(Months[Counter], LeadingZero(Counter, 02), DateStr);

  DateStr := Copy(DateStr, 4, 2) + '-' +
             Copy(DateStr, 1, 2) + '-' +
             Copy(DateStr, 7, 4);

  if IBMInfo.OriginalSize = 00 then IbmInfo.Originalsize := 01;
  if IBMInfo.CompressedSize = 00 then IbmInfo.CompressedSize := 01;

  {-- Make sure the filename isnt too long ------------------------------------}
  FileDispStr := JustName(IbmInfo.FileName);
  if Length(FileDispStr) > 12 then
    FileDispStr := Copy(FileDispStr, 1, 9) + '...';

  {-- Make sure the originalsize and compressedsizes arent too long -----------}
  OrigSizeStr := FStr(IbmInfo.OriginalSize);
  if Length(OrigSizeStr) > 7 then
    OrigSizeStr := FStr(IbmInfo.OriginalSize DIV 10240) + 'mb';

  CompSizeStr := FStr(IbmInfo.CompressedSize);
  if Length(CompSizeStr) > 7 then
    CompSizeStr := FStr(IbmInfo.CompressedSize DIV 10240) + 'mb';

  {-- Display the file contents -----------------------------------------------}
  with IBMInfo do
    WriteLn('`A3:',   FileDispStr, '`A7:',
            '`X15:',  OrigSizeStr,
            '`X24:',  CompSizeStr,
            '`X33:',  Round(((OriginalSize - CompressedSize) / OriginalSize) * 100.00):2,'%',
            '`X39:',  DateStr,
            '`X50:',  TimeStr);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'ShowEntry ( end )');
  {$ENDIF}
end; { proc. ShowEntry }

var DirInfo  : SearchRec;
    ExecStr  : String;
    FilesInf : FilesRecord;
    CompInfo : CompressorType;
    IBMInfo  : IBM;
    Width    : Word;
    Height   : Word;
    Colours  : Word;
    OrigBytes: Longint;
    CompBytes: Longint;
    ExitCode : Word;
    Success  : Boolean;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileSys, 'ArchiveListing (begin)');
  {$ENDIF}

  ArchiveListing := False;

  If DoUser then
    begin
      WriteLn;
      Writeln;
      Write('`A10:', LangObj^.ralGet(ralArc2View));
      TempStr := '';
      GetString(TempStr, 80, [#32..#254], False, False, False, False);
      WriteLn;

    end; { if DoUser }

  TempStr := Trim(TempStr);
  if TempStr = '' then EXIT;

  If Pos('/F', SUpcase(MiscData))>00 then
      GetFilesRecord(FilesInf, Exitinfo.Userinfo.FileArea, True)
       else GetFilesRecord(FilesInf, FVal(FirstWord(MiscData, defExtractWord, false)), True);

  if Pos('.', TempStr)>00 then
      TempStr := FilesInf.FilePath+TempStr+'*'
       else TempStr := FilesInf.FilePath+TempStr+'.*';

  FindFirst(TempStr, Anyfile - VolumeID, DirInfo);

  if DosError > 00 then
    begin
      if DoUser then
        begin
          WriteLn('`A12:', LangObj^.ralGet(ralInvViewNm));
          InputObj^.PressEnter(False, True);
        end; { if DoUser }

      EXIT;
    end; { DosError }

  if GlobalCfg^.RaConfig^.ArcViewCmd<>'' then
    begin
      ExecStr := GlobalCfg^.RaConfig^.ArcViewCmd;

      Replace('@', FilesInf.FilePath + Dirinfo.Name, ExecStr);
      RaExec(ExecStr, False, False, False, False, ExitCode, Success);

      ArchiveListing := true;
      EXIT;
    end; { if }

  if NOT DetectCompressor(FilesInf.FilePath + DirInfo.Name, CompInfo) then
    begin
      if CheckGifFile(FilesInf.FilePath + DirInfo.Name, Width, Height, Colours) then
        begin
          OutputObj^.ClearScreen;
          WriteLn(LangObj^.ralGet(ralIsAGif), ' ', Width,'x', Height, ', ',
                  Colours, ' ', LangObj^.ralGet(ralColours));

          ArchiveListing := true;
          InputObj^.PressEnter(false, true);
        end
          else begin
                 if DoUser then
                   begin
                     WriteLn;
                     WriteLn('`A12:', LangObj^.ralGet(ralInvArc));
                     InputObj^.PressEnter(False, True);
                   end; { DoUser }
               end; { if }

      EXIT;
    end; { Is not a compressor }

  OrigBytes := 00;
  CompBytes := 00;

  OutputObj^.ClearScreen;
  WriteLn;
  WriteLn('`A11:', LangObj^.ralGet(ralArcList),' ', DirInfo.Name);
  WriteLn('`A10:', LangObj^.ralGet(ralArcLstHdr));
  WriteLn('`A02:', OutputObj^.AvatarDupl(#196, NoColorLength(LangObj^.ralGet(ralArcLstHdr))));

  CompInfo^.CheckProtection;                            { Grab the info about }
                                                      { Security, version etc }
  CompInfo^.FindFirstEntry;                      { Find the first file inside }
  While (NOT CompInfo^.LastEntry) AND                   { The compressed file }
         (NOT OutputObj^.StopMore) do
           begin
             CompInfo^.ReturnEntry(IBMInfo);
             ShowEntry(IBMInfo);

             Inc(OrigBytes, IBMInfo.OriginalSize);
             Inc(CompBytes, IBMInfo.CompressedSize);

             CompInfo^.FindNextEntry;                  { Find the next entry }
           end; { While }

  If OrigBytes = 00 then OrigBytes := 01;        { To avoid div.by.zero error }

  Writeln('`A2:', '`X15:컴컴컴컴 컴컴컴컴 컴컴�');
  WriteLn('`A3:', '`X15:', OrigBytes,
                  '`X24:', CompBytes, '`A14:',
                  '`X33:', Round(((OrigBytes-CompBytes) / OrigBytes) * 100.00):2,'%');


  InputObj^.PressEnter(true, true);
  ArchiveListing := true;
  OutputObj^.SetStopMore(False);

  FindClose(Dirinfo);

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileSys, 'ArchiveListing ( end )');
  {$ENDIF}
end; { func. ArchiveListing }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  ShowAnsiFile(    MiscData: String;
                           DoUser  : Boolean;
                           TempStr : String;
                       var Exitinfo: ExitinfoRecord): Boolean;
var FilesInf  : FilesRecord;
    DirInfo   : SearchRec;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileSys, 'ShowAnsiFile: '+MiscData+ ' - (begin)');
     DebugObj.DebugLog(logFileSys, 'ShowAnsiFile: TempStr='+TempStr);
  {$ENDIF}

  ShowAnsiFile := false;
  If Pos('/F', SUpcase(MiscData))>00 then
      GetFilesRecord(FilesInf, Exitinfo.Userinfo.FileArea, True)
       else begin
              if FVal(ExtractWord(MiscData, 1, defExtractWord, false, false)) > 00 then
               GetFilesRecord(FilesInf, FVal(FirstWord(MiscData, defExtractWord, false)), True);
            end; { if }

  if DoUser then
    begin;
      WriteLn;
      Writeln;
      Write('`A10:', LangObj^.ralGet(ralViewName));

      TempStr := '';
      GetString(TempStr, 80, [#32..#254], False, False, False, False);
      WriteLn;
    end; { if DoUser }

  If Pos('.', TempStr)>00 then
    TempStr := FilesInf.FilePath+TempStr+'*'
     else TempStr := FilesInf.FilePath+TempStr+'.*';

  {$IFDEF WITH_DEBUG}

     DebugObj.DebugLog(logFileSys, 'ShowAnsiFile: TempStr = '+TempStr);
  {$ENDIF}

  FindFirst(TempStr, AnyFile - VolumeID, DirInfo);
  FindClose(DirInfo);

  if DosError>00 then
    begin
      WriteLn('`A14:', LangObj^.ralGet(ralInvViewNm));
      InputObj^.PressEnter(False, True);
      EXIT;
    end; { DosError }

  Write('`A3:');

  OutputObj^.ResetLines(01);                          { Reset all the counters }
  OutputObj^.SetStopMore(False);

  DisplayHotFile('"' + FilesInf.FilePath + DirInfo.Name + '"' + #32 + MiscData, []);

  LineCfg^.ContrCodes := True;

  InputObj^.PressEnter(True, True);
  OutputObj^.SetStopMore(False);

  ShowAnsiFile := True;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileSys, 'ShowAnsiFile: '+MiscData+ ' - ( end )');
  {$ENDIF}
end; { proc. ShowAnsiFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowDirectFile(MiscData: String);
var FilesInf: FilesRecord;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'ShowDirectFile: '+MiscData+' - (begin)');
  {$ENDIF}

  FillChar(FilesInf, SizeOf(FilesRecord), #00);
  If GetValue('/A=', MiscData, False)<>'' then
    begin
      GetFilesRecord(FilesInf, FVal(GetValue('/A=', MiscData, True)), True);
      MiscData := FilesInf.FilePath + MiscData;
    end; { Area nr specified }

  DisplayHotFile(MiscData, []);
  OutputObj^.SetStopMore(false);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'ShowDirectFile: '+MiscData+' - ( end )');
  {$ENDIF}
end; { proc. ShowDirectFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function SearchFileInList(Name: String; var ListRec: TagHeaderList): Byte;
var Counter    : Byte;
    WildSearch : Boolean;
    UseLFN     : Boolean;
    ComparStr  : String;
begin
  {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logFileSys, 'SearchFileInList: "'+ Name + '"');
  {$ENDIF}
  SearchFileInList := 00;

  if Name[1] = '"' then Delete(Name, 1, 1);
  if Name[Length(name)] = '"' then Delete(Name, Length(Name), 1);

  Name := Trim(SUpCase(Name));
  WildSearch := FALSE;
  UseLFN := IsLfn(Name);
  If Pos('?', Name) > 00 then WildSearch := TRUE;
  If Pos('*', Name) > 00 then WildSearch := TRUE;

  if WildSearch then
   For Counter := 01 to 99 do
    begin
       if UseLFN then
         ComparStr := GetLfnName(ListRec[Counter].Name, ListRec[Counter].RecordNum,
                                 ListRec[Counter].AreaNum)
           else ComparStr := ListRec[Counter].Name;

       If IsWildCard(Name, SUpCase(Trim(ComparStr))) then
        begin
          SearchFileInList := Counter;
          break;
        end; { Found }
    end; { if }

  if NOT WildSearch then
   For Counter := 01 to 99 do
     begin
       if UseLfn then
         ComparStr := GetLfnName(ListRec[Counter].Name, ListRec[Counter].RecordNum,
                                 ListRec[Counter].AreaNum)
           else ComparStr := ListRec[Counter].Name;

      If Name = SUpCase(Trim(ComparStr)) then
        begin
          SearchFileInList := Counter;
          break;
        end; { Found }
     end; { if }
end; { func. SearchFileInList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit ViewFile }
