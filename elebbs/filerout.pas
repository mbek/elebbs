unit FileRout;
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
** FileROUT.TPU, File routines for EleBBS
**
**
** Copyright (c) 1997 by Maarten Bekers
**
** Created : 28-Jul-1997
** Last update : 01-Oct-1997
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Global, CfgRec, Cfgfile, RAL;

procedure ReadSysInfoBBS(var SysInfo: SysinfoRecord);
procedure DoRaBusy(DoErase: Boolean);
procedure ReadLanguageRA(Nr: Longint);          { Opens and reads LANGUAGE.RA }
procedure WriteMsgInf(FromWho, ToWho, Subject, MsgNr, BoardName: String; MsgPrivate: Boolean);

function  GroupName(Board: SmallWord; Message: Boolean): String;
function  GetGroupRecord(BoardNr: SmallWord; CheckRDX, Message: Boolean; var GroupInf: Grouprecord): Boolean;
function  GetMessageRecord(var MsgInf: MessageRecord; BoardNr: SmallWord; CheckRDX: Boolean): Boolean;
function  GetEleMessageRecord(var EleMSGInf: EleMessageRecord; BoardNr: SmallWord; CheckRDX: Boolean): Boolean;
function  Ra250Area(Board:SmallWord):Longint;
function  Ra250MsgArea(Board:SmallWord):Longint;
function  GetFilesRecord(var RaInf: FilesRecord; BoardNr: SmallWord; CheckRDX: Boolean): Boolean;
function  GetEleFilesRecord(var EleFileInf: EleFilesRecord; Boardnr: SmallWord; CheckRDX: Boolean): Boolean;
function  Ra250Group(Board:SmallWord; Message: Boolean):Longint;
function  FileCopy(Orig, Dest: String; KeepDates, DoText: Boolean): Boolean;
function  SearchCTLFile(const CtlName: String; const UserName: String; const WildCard: Boolean): Boolean;{ Searchs a CTL-file }
function  OpenRAfile(S:String): String;                       { Open RA file }
function  UpToDateFile(FName1, FName2: String): Boolean;
function  CreateTempDir(S: String; NodeNr: Longint): String;
function  GetLogFileName: String;
function  IsElexerScript(ScrName: String): Boolean;

procedure GetAddress(Nr: Byte; var Address: NetAddress);
procedure SetAddress(Nr: Byte; var Address: NetAddress);
procedure UpdateStatistics;
procedure BuildFastIndex;
procedure BuildUserExt;
procedure BuildLastRead;
procedure InitSystemNames;
function  SemaExist(S: String): Boolean;
procedure SelectAllCombined(var Combined: CombinedRecord);
procedure RemoveSema(S: String);
procedure CreateSema(S: String);
procedure GetExitinfoInfo(var Exitinfo: ExitinfoRecord); { Get EXITINFO info }

{$IFDEF WITH_FULL}
function  SearchNextMsgArea(GroupNum: Word): Word;
function  SearchNextFileArea(GroupNum: Word): Word;

procedure GetProtocolRecord(CH: Char; var ProtInf: ProtocolRecord);
procedure CreateDorinfoDef(OldStyle, UseHandle: Boolean; BpsRate: Longint); { Create DORINFO1.DEF }
procedure CreateDoorSys(BPsRate: LongInt);              { Create an DOOR.SYS }
procedure CreateDoor32Sys(BPsRate: LongInt);          { Create an DOOR32.SYS }
procedure CreateTemplateFile(S:String);            { Create RA template file }
procedure CreateTextFile(FName: String; const Contents: Array of String);
procedure WriteExitinfo(var Exitinfo: ExitinfoRecord); { Writes an new Exitinfo^.BBS }
procedure SearchNextEvent(var Event: EventRecord);
procedure ReadExitinfo(var Exitinfo: ExitinfoRecord);{ Reads an Exitinfo^.BBS }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses GenDos, SysUtils, Debug_U, LongStr, StrPath, Ranges, FileObj, GenFile,
     Crc_Unit, Bitwise, usrExt, objDec
     {$IFDEF WITH_FULL}
       ,InOut_U, Input_U, CharUnit, Strings, Sound_U,
        User_U, Memman, UsrCodes, Terminal, Limit_U,
        Transfer, ElLog_U, ApFossil
     {$ENDIF}, Dos, CentrStr, UnixDate, Cases, WordStr, JDates;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  FileCopy(Orig, Dest: String; KeepDates, DoText: Boolean): Boolean;
var Orig_F,
    Dest_F  : File;
{$IFDEF MSDOS}
    Buffer  : Array[0..1023] of Byte;
{$ELSE}
    Buffer  : Array[0..((1024 * 32) - 01)] of Byte;
{$ENDIF}
    FTime   : Longint;
    NumRead,
    NumWrite: NumReadType;
    OldFMode: Longint;
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logFileRout, 'FileCopy: Orig/Dest: '+Orig+' / '+Dest);{$endif}
  FileCopy := False;

  Assign(Orig_F, Orig);
  OldFMode := FileMode;
  FileMode := ReadMode + DenyNone;
  {$i-} Reset(Orig_F, 1); {$I+}
  FileMode := OldFMode;
  If IOResult>00 then
      begin
        if DoText then
          begin;
           {$IFDEF WITH_FULL}
            WriteLn('`A15:');
            WriteLn('`A12:', LangObj^.ralGet(ralErrOpen));
            InputObj^.PressEnter(False, True);
           {$ELSE}
           {$ENDIF}
          end; { if }
          EXIT;
      end; { Could not open source file }

  Assign(Dest_F, Dest);
  OldFMode := FileMode;
  FileMode := WriteMode + DenyNone;
  {$i-} Rewrite(Dest_F, 1); {$I+}
  FileMode := OldFMode;
  If IOResult>00 then begin;
                        if DoText then
                          begin;
                            {$IFDEF WITH_FULL}
                             WriteLn('`A15:');
                             WriteLn('`A12:', LangObj^.ralGet(ralErrCreate));
                             InputObj^.PressEnter(False, True);
                            {$ENDIF}
                          end; { if }

                          {$i-}
                           System.Close(Orig_F);
                          {$i+}
                          if IOResult>00 then ;
                          EXIT;
                      end; { Unable to create destination file }

  NumRead := 00;
  NumWrite := 00;

  While (NOT Eof(Orig_F)) AND (NumRead=NumWrite) do
    begin
      {$i-}
        BlockRead(Orig_F, Buffer, SizeOf(Buffer), NumRead);
        BlockWrite(Dest_F, Buffer, NumRead, NumWrite);
      {$i+}
      if IOResult > 00 then
        begin
          FileCopy := false;
          BREAK;
        end; { if }

      If (NumWrite <> NumRead) AND (NOT Eof(Orig_F)) then
        begin
           if DoText then
            begin;
             {$IFDEF WITH_FULL}
              WriteLn('`A15:');
              WriteLn('`A12:', LangObj^.ralGet(ralErrCopy));
              InputObj^.PressEnter(False, True);
             {$ENDIF}
            end; { if }
        end; { Error occured during copy }
    end; { while }

  if KeepDates then
    begin
      {$i-}
        GetFTime(Orig_F, FTime);
        SetFTime(Dest_F, Ftime);
      {$i+}
      if IOResult > 00 then ;
    end; { Set filestamps }

  {$i-}
    Close(Orig_F);
    Close(Dest_F);
  {$I+}
  If IOResult>00 then;

  FileCopy := True;
end; { func. FileCopy }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Ra250Area(Board:SmallWord):Longint;
var Rdx_F   : pFileObj;
    Index   : SmallWord;
    FilesInf: FilesRecord;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileRout, 'Ra250Area: '+FStr(Board));
  {$ENDIF}
  Index := 00;
  Ra250Area := Board;
  if Board<1 then Board:=01;

  if Config_OpenFile(Rdx_F, RdxName(FilesFileName), SizeOf(SmallWord), ReadMode + DenyNone, False, False)>00 then
    begin
      Config_DoneFile(Rdx_F);

      Config_OpenFile(Rdx_F, FilesFileName, SizeOf(FilesRecord), ReadMode + DenyNone, False, True);
      While NOT RDX_F^.EOF do
        begin
          if Config_ReadFile(Rdx_F, FilesInf, 1)>00 then BREAK;
          if FilesInf.AreaNum=Board then
            begin
               Index := RDX_F^.FilePos;
               BREAK;
            end; { if }
        end; { while }

      Config_DoneFile(Rdx_F);
    end
     else begin
            Config_SeekFile(Rdx_F, (Board - 01));
            Config_ReadFile(Rdx_F, Index, 1);
            Config_DoneFile(Rdx_F);
          end; { if }

  Ra250Area := Index;
end; { Ra250Area }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

Function Ra250MsgArea(Board:SmallWord):Longint;
var Rdx_F     : pFileObj;
    Index     : SmallWord;
    MessageInf: MessageRecord;
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logFileRout, 'ra250msgasrea: '+FStr(Board));{$endif}
  Ra250MsgArea := Board;
  if Board<1 then Board:=01;

  if Config_OpenFile(Rdx_F, RdxName(MessagesFileName), SizeOf(SmallWord), ReadMode + DenyNone, False, False)>00 then
    begin
      Config_DoneFile(Rdx_F);

      Config_OpenFile(Rdx_F, MessagesFileName, SizeOf(MessageRecord), ReadMode + DenyNone, False, True);
      While NOT RDX_F^.EOF do
        begin
          if Config_ReadFile(Rdx_F, MessageInf, 1)>00 then BREAK;
          if MessageInf.AreaNum=Board then
            begin
               Index := RDX_F^.FilePos;
               BREAK;
            end; { if }
        end; { while }

      Config_DoneFile(Rdx_F);
    end
     else begin
            Config_SeekFile(Rdx_F, (Board - 01));
            Config_ReadFile(Rdx_F, Index, 1);
            Config_DoneFile(Rdx_F);
          end; { if }

  Ra250MsgArea := Index;
end; { func. Ra250MsgArea }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

Function Ra250Group(Board:SmallWord; Message: Boolean):Longint;
Var Rdx_F    : pFileObj;
    Index    : SmallWord;
    FName    : String;
begin;
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileRout, 'Ra250Group: '+FStr(Board)+ ' (start)');
  {$ENDIF}
  Ra250Group := Board;
  if Board<01 then Board:=01;

  if Message then FName := RdxName(MGroupsFileName)
     else FName := RdxName(FGroupsFileName);

  if Config_OpenFile(Rdx_F, FName, SizeOf(SmallWord), ReadMode + DenyNone,
                     False, False) = 00 then
   begin
     Config_SeekFile(Rdx_F, (Board - 01));
     Config_ReadFile(Rdx_F, Index, 1);
   end; { if }
  Config_DoneFile(Rdx_F);

  Ra250Group := Index;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileRout, 'Ra250Group: '+FStr(Board)+ ' ( end )');
  {$ENDIF}
end; { func. Ra250group }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

Function GetFilesRecord(var RaInf: FilesRecord; BoardNr: SmallWord; CheckRDX: Boolean): Boolean;
var Area_F  : pFileObj;
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logFileRout, 'GetFilesRecord: '+Fstr(BoardNr));{$endif}
 FillChar(RaInf, SizeOf(FilesRecord), #00);
 GetFilesRecord := False;

 if Config_OpenFile(Area_F, FilesFileName, SizeOf(FilesRecord), ReadMode + DenyNone, False, False)>00 then
   begin
     Config_DoneFile(Area_F);
     EXIT;
   end; { if }

 If CheckRDX then
   Config_SeekFile(Area_F, (Ra250Area(BoardNr)-1)) else
    Config_SeekFile(Area_F, (BoardNr-1));

 Config_ReadFile(Area_F, RaInf, 1);
 Config_DoneFile(Area_F);
 RaInf.FilePath := ForceBack(RaInf.FilePath);

 GetFilesRecord := True;
end; { func. GetFilesRecord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function GetEleFilesRecord(var EleFileInf: EleFilesRecord; Boardnr: SmallWord; CheckRDX: Boolean): Boolean;
var Area_F: pFileObj;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileRout, 'GetEleFilesRecord: '+Fstr(BoardNr));
  {$ENDIF}

  FillChar(EleFileInf, SizeOf(EleFilesRecord), #00);
  GetEleFilesRecord := False;

  if Config_OpenFile(Area_F, EleFilesFileName, SizeOf(EleFilesRecord), ReadMode + DenyNone, False, False)>00 then
    begin
      Config_DoneFile(Area_F);
      EXIT;
    end; { if }

  If CheckRDX then
    Config_SeekFile(Area_F, (Ra250Area(BoardNr)-1)) else
     Config_SeekFile(Area_F, (BoardNr-1));

  Config_ReadFile(Area_F, EleFileInf, 1);
  Config_DoneFile(Area_F);

  GetEleFilesRecord := true;
end; { func. GetEleFilesRecord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)


{$IFDEF WITH_FULL}
Procedure CreateDorinfoDef(OldStyle, UseHandle: Boolean; BpsRate: Longint); { Create DORINFO1.DEF }
var Dorinfo_F : pFileObj;
    BaudStr   : String;
    Emulation : Byte;
    UserName  : String;
    CharObj   : CharArrayObj;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileRout, 'CreateDorInfoDef (begin)');
  {$ENDIF}

  If (NOT LineCfg^.Exitinfo^.ErrorFreeConnect) OR (OldStyle) then
    BaudStr := 'BAUD' else BaudStr:='BAUD-R';
  Emulation := 00;
  If LineCfg^.AnsiOn then Emulation := 01;
  If LineCfg^.AvatarOn then Emulation := 02;
  if LineCfg^.RipOn then Emulation := 03; { ?? }

  if UseHandle then UserName := LineCfg^.Exitinfo^.Userinfo.Handle
    else UserName := LineCfg^.Exitinfo^.Userinfo.Name;

  CharObj.Init(1024 * 10);

{ Nr }
{ 01 }  CharObj.AddStringLn(SUpcase(GlobalCfg^.RaConfig^.SystemName));
{ 02 }  CharObj.AddStringLn(SUpcase(FirstNameOnly(GlobalCfg^.RaConfig^.Sysop)));
{ 03 }  CharObj.AddStringLn(SUpcase(LastNameOnly(GlobalCfg^.RaConfig^.Sysop)));
{ 04 }  CharObj.AddStringLn('COM'+FStr(LineCfg^.Modem^.Comport));
{ 05 }  CharObj.AddStringLn(FStr(BpsRate)+' '+BaudStr+',N,8,1');
{ 06 }  CharObj.AddStringLn(FStr(00));
{ 07 }  CharObj.AddStringLn(SUpcase(FirstNameOnly(UserName)));
{ 08 }  CharObj.AddStringLn(SUpcase(LastNameOnly(UserName)));
{ 09 }  CharObj.AddStringLn(SUpcase(LineCfg^.Exitinfo^.Userinfo.Location));
{ 10 }  CharObj.AddStringLn(FStr(Emulation));
{ 11 }  CharObj.AddStringLn(FStr(LineCfg^.Exitinfo^.Userinfo.Security));
{ 12 }  CharObj.AddStringLn(FStr(Min(LineCfg^.Exitinfo^.TimeLimit, 32767)));
{ 13 }  CharObj.AddStringLn('');


  New(DorInfo_F, Init);

  DorInfo_F^.Assign('dorinfo1.def');
  DorInfo_F^.FileMode := ReadWriteMode + DenyNone;
  DorInfo_F^.Create(1);
  DorInfo_F^.BlkWrite(CharObj.TxtArr^, StrLen(CharObj.TxtArr^));

  Dispose(DorInfo_F, Done);

  CharObj.Done;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileRout, 'CreateDorInfoDef ( end )');
  {$ENDIF}
end; { proc. CreateDorinfoDef }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
Procedure CreateDoorSys(BPsRate: LongInt);              { Create an DOOR.SYS }

Function Bool2Char(B:Boolean):Char;
begin
   If B then Bool2Char := 'Y' else Bool2Char := 'N';
end; { func. Bool2Char }

var Door_F      : pFileObj;
    GraphicsMode: String;
    SubDate     : String;
    CharObj     : CharArrayObj;
begin
 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logFileRout, 'CreateDoorSys (start)');
 {$ENDIF}

  if LineCfg^.AnsiOn then GraphicsMode := 'GR' else GraphicsMode := 'NG';
  if LineCfg^.Exitinfo^.Baud = 00 then LineCfg^.Modem^.Comport := 00;
  SubDate := LineCfg^.Exitinfo^.Userinfo.SubDate;
  if SubDate = '' then
    begin
      SubDate := DateStr;
      SubDate := '31-12' + Copy(SubDate, 7, 2)
    end; { if }

  CharObj.Init(1024 * 10);

{ 01 } CharObj.AddStringLn('COM'+FStr(LineCfg^.Modem^.Comport)+':');
{ 02 } CharObj.AddStringLn(FStr(BpsRate));
{ 03 } CharObj.AddStringLn(Fstr(8));
{ 04 } CharObj.AddStringLn(FStr(LineCfg^.RaNodeNr));
{ 05 } CharObj.AddStringLn(FStr(BpsRate));
{ 06 } CharObj.AddStringLn(Bool2Char(LineCfg^.Snooping));
{ 07 } CharObj.AddStringLn(Bool2Char(LineCfg^.PrinterLogging));
{ 08 } CharObj.AddStringLn(Bool2Char(RaYell));
{ 09 } CharObj.AddStringLn(Bool2Char(RaYell));
{ 10 } CharObj.AddStringLn(LineCfg^.Exitinfo^.Userinfo.Name);
{ 11 } CharObj.AddStringLn(LineCfg^.Exitinfo^.Userinfo.Location);
{ 12 } CharObj.AddStringLn(LineCfg^.Exitinfo^.Userinfo.VoicePhone);
{ 13 } CharObj.AddStringLn(LineCfg^.Exitinfo^.Userinfo.DataPhone);
{ 14 } CharObj.AddStringLn('');                                 { User's PassWord }
{ 15 } CharObj.AddStringLn(FStr(LineCfg^.Exitinfo^.Userinfo.Security));
{ 16 } CharObj.AddStringLn(FStr(LineCfg^.Exitinfo^.Userinfo.Nocalls));
{ 17 } CharObj.AddStringLn(LineCfg^.Exitinfo^.Userinfo.LastDate);
{ 18 } CharObj.AddStringLn(FStr(Min(LineCfg^.Exitinfo^.TimeLimit, 32767) * 60));
{ 19 } CharObj.AddStringLn(FStr(Min(LineCfg^.Exitinfo^.TimeLimit, 32767)));
{ 20 } CharObj.AddStringLn(GraphicsMode);
{ 21 } CharObj.AddStringLn(FStr(LineCfg^.Exitinfo^.Userinfo.ScreenLength));
{ 22 } CharObj.AddStringLn('N');
{ 23 } CharObj.AddStringLn('');                                    { Always blank }
{ 24 } CharObj.AddStringLn('');                                    { Always blank }
{ 25 } CharObj.AddStringLn(SubDate);
{ 26 } CharObj.AddStringLn(FStr(SearchUser(LineCfg^.Exitinfo^.Userinfo.Name)));
{ 27 } CharObj.AddStringLn(LineCfg^.Exitinfo^.Userinfo.DefaultProtocol);
{ 28 } CharObj.AddStringLn(FStr(LineCfg^.Exitinfo^.Userinfo.Uploads));
{ 29 } CharObj.AddStringLn(FStr(LineCfg^.Exitinfo^.Userinfo.Downloads));
{ 30 } CharObj.AddStringLn(FStr(LineCfg^.Exitinfo^.Userinfo.TodayK));
{ 31 } CharObj.AddStringLn(FStr(Min(LineCfg^.UsersKbLimit, 65534)));
{ 32 } CharObj.AddStringLn(LineCfg^.Exitinfo^.Userinfo.Birthdate);
{ 33 } CharObj.AddStringLn(GlobalCfg^.RaConfig^.MsgBasePath);
{ 34 } CharObj.AddStringLn(GlobalCfg^.RaConfig^.MsgBasePath);
{ 35 } CharObj.AddStringLn(GlobalCfg^.RaConfig^.Sysop);
{ 36 } CharObj.AddStringLn(LineCfg^.Exitinfo^.Userinfo.Handle);
{ 37 } CharObj.AddStringLn(LineCfg^.Exitinfo^.EventInfo.StartTime);
{ 38 } CharObj.AddStringLn(Bool2Char(LineCfg^.Exitinfo^.ErrorFreeConnect));
{ 39 } CharObj.AddStringLn(Bool2Char(False));
{ 40 } CharObj.AddStringLn(Bool2Char(True));
{ 41 } CharObj.AddStringLn(FStr(GlobalCfg^.RaConfig^.NormFore));
{ 42 } CharObj.AddStringLn(FStr(00));
{ 43 } CharObj.AddStringLn(LineCfg^.Exitinfo^.Userinfo.LastDate);
{ 44 } CharObj.AddStringLn(LineCfg^.Exitinfo^.LoginTime);
{ 45 } CharObj.AddStringLn(LineCfg^.Exitinfo^.Userinfo.LastTime);
{ 46 } CharObj.AddStringLn(FStr(32768));
{ 47 } CharObj.AddStringLn(FStr(0));
{ 48 } CharObj.AddStringLn(FStr(LineCfg^.Exitinfo^.Userinfo.UploadsK));
{ 49 } CharObj.AddStringLn(FStr(LineCfg^.Exitinfo^.Userinfo.DownloadsK));
{ 50 } CharObj.AddStringLn(LineCfg^.Exitinfo^.Userinfo.Comment);
{ 51 } CharObj.AddStringLn(FStr(00));
{ 52 } CharObj.AddStringLn(FStr(LineCfg^.Exitinfo^.Userinfo.MsgsPosted));
{ 53 } CharObj.AddStringLn('');

  New(Door_F, Init);

  Door_F^.Assign('door.sys');
  Door_F^.FileMode := ReadWriteMode + DenyNone;
  Door_F^.Create(1);
  Door_F^.BlkWrite(CharObj.TxtArr^, StrLen(CharObj.TxtArr^));

  Dispose(Door_F, Done);

  CharObj.Done;

 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logFileRout, 'CreateDoorSys ( end )');
 {$ENDIF}
end; { proc. CreateDooorSys }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
procedure CreateTextFile(FName: String; const Contents: Array of String);
var Text_F      : pFileObj;
    CharObj     : CharArrayObj;
    Counter     : Longint;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileRout, 'CreateTextFile (start)');
  {$ENDIF}

  {-- Initialize the string array first -------------------------------------}
  CharObj.Init(1024 * 10);

  {-- now add all the strings -----------------------------------------------}
  for Counter := 0 to High(Contents) do
    CharObj.AddStringLn(Contents[Counter]);

  {-- Now dump the file to disk -----------------------------------------------}
  New(Text_F, Init);

  Text_F^.Assign(SlowCase(FName));
  Text_F^.FileMode := ReadWriteMode + DenyNone;
  Text_F^.Create(1);
  Text_F^.BlkWrite(CharObj.TxtArr^, StrLen(CharObj.TxtArr^));
  Dispose(Text_F, Done);

  CharObj.Done;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileRout, 'CreateTextFile ( end )');
  {$ENDIF}
end; { proc. CreateTextFile }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
procedure CreateDoor32Sys(BPsRate: LongInt);          { Create an DOOR32.SYS }
var Door32_F    : pFileObj;
    CharObj     : CharArrayObj;

    ComType     : String;
    EmulType    : String;
begin
 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logFileRout, 'CreateDoor32Sys (start)');
 {$ENDIF}

  {-- Setup some variables ----------------------------------------------------}
  ComType := '1';                                              { Assume serial }
  if LineCfg^.Exitinfo^.Baud = 0 then ComType := '0';
  if TelnetServ then ComType := '2';

  EmulType := '0';
  if LineCfg^.AnsiOn then EmulType := '1';
  if LineCfg^.AvatarOn then EmulType := '2';

  {-- Initialize the string array first ---------------------------------------}
  CharObj.Init(1024 * 10);

{ 01 } CharObj.AddStringLn(ComType);            { 0=local, 1=serial, 2=telnet }
{ 02 } CharObj.AddStringLn(FStr(Apro_Use_Old_Handle));          { Comm-handle }
{ 03 } CharObj.AddStringLn(FStr(BpsRate));                         { BPS-rate }
{ 04 } CharObj.AddStringLn(PidName);                         { EleBBS version }
{ 05 } CharObj.AddStringLn(FStr(LineCfg^.Exitinfo^.UserRecord + 1));{ Userrec# (1-based) }
{ 06 } CharObj.AddStringLn(LineCfg^.Exitinfo^.Userinfo.Name);        { users real name }
{ 07 } CharObj.AddStringLn(LineCfg^.Exitinfo^.Userinfo.Handle);         { users handle }
{ 08 } CharObj.AddStringLn(FStr(LineCfg^.Exitinfo^.Userinfo.Security)); { usrs sec.lvl }
{ 09 } CharObj.AddStringLn(FStr(LineCfg^.Exitinfo^.TimeLimit)); { Users timeleft (mns) }
{ 10 } CharObj.AddStringLn(EmulType);  { Usr emultype (0=none, 1=ansi, 2=avt) }
{ 11 } CharObj.AddStringLn(FStr(LineCfg^.RaNodeNr));               { Nodenumber }
{ 12 } CharObj.AddStringLn('');

  {-- Now dump the file to disk -----------------------------------------------}
  New(Door32_F, Init);

  Door32_F^.Assign('door32.sys');
  Door32_F^.FileMode := ReadWriteMode + DenyNone;
  Door32_F^.Create(1);
  Door32_F^.BlkWrite(CharObj.TxtArr^, StrLen(CharObj.TxtArr^));
  Dispose(Door32_F, Done);

  CharObj.Done;

 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logFileRout, 'CreateDoor32Sys ( end )');
 {$ENDIF}
end; { proc. Create32DoorSys }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
Procedure CreateTemplateFile(S:String);            { Create RA template file }
var Template_F: Text;
    MakeFile_F: Text;
    MacroStr  : ^MacroArray;
    Str       : String;
    Teller    : Byte;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileRout, 'CreateTemplateFile: '+S);
  {$ENDIF}
  If S='' then Exit;

  if NOT MemMan.AllocMem(MacroStr, SizeOf(MacroArray), 'MacroArray', 'CreateTemplateFile') then EXIT;

  Assign(Template_F, S+'.RAT');
  {$i-} System.Reset(Template_F); {$I+}
  If IOResult>00 then Exit;

 {$Ifdef Win32}
   If SysUtils.FileExists(S)
    then SysUtils.DeleteFile(S);
 {$endif}
  Assign(MakeFile_F, S);
  {$i-} System.Rewrite(MakeFile_F); {$I+}
  If IOResult>00 then Exit;

  While NOT Eof(Template_F) do
    begin
      ReadLn(Template_F, Str);

      termObj^.RaCodeStr(Str);
      WriteLn(MakeFile_F, Str);
    end; { While }

  MemMan.ReleaseMem(MacroStr, Sizeof(MacroArray));
  Close(Template_F);
  Close(MakeFile_F);
end; { proc. CreateTemplateFile }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
procedure WriteExitinfo(var Exitinfo: ExitinfoRecord); { Writes an new LineCfg^.Exitinfo^.BBS }
var Exit_F   : pFileObj;
    ExtraPath: String;
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logFileRout, 'WriteExitinfo');{$endif}
  Exitinfo.TimeOfCreation := TimeStr(True, True);

  ExtraPath := '';
  if NOT GlobalCfg^.RaConfig^.MultiLine then ExtraPath:= GlobalCfg^.RaConfig^.Syspath;

  Config_OpenFile(Exit_F, ExtraPath + 'exitinfo.bbs', SizeOf(ExitinfoRecord), WriteMode + DenyNone, True, True);
  Config_SeekFile(Exit_F, 00);
  Config_WriteFile(Exit_F, Exitinfo, 1);
  Config_DoneFile(Exit_F);
end; { proc. WriteExitinfo }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

{$IFDEF WITH_FULL}
Procedure ReadExitinfo(var Exitinfo: ExitinfoRecord); { Reads an LineCfg^.Exitinfo^.BBS }
var Exit_F   : pFileObj;
    ExtraPath: String;
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logFileRout, 'ReadExitinfo');{$endif}

  ExtraPath := '';
  if NOT GlobalCfg^.RaConfig^.MultiLine then ExtraPath:= GlobalCfg^.RaConfig^.Syspath;

  Config_OpenFile(Exit_F, ExtraPath + 'exitinfo.bbs', SizeOf(ExitinfoRecord), ReadMode + DenyNone, True, True);
  Config_SeekFile(Exit_F, 00);
  Config_ReadFile(Exit_F, Exitinfo, 1);
  Config_DoneFile(Exit_F);
end; { proc. ReadExitinfo }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

Function GroupName(Board: SmallWord; Message: Boolean): String;
var Group_F : pFileObj;
    Group   : GroupRecord;
    FName   : String;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileRout, 'GroupName (Start)');
  {$ENDIF}
  GroupName := '';
  FillChar(Group, SizeOf(GroupRecord), #00);
  if Board = 0 then EXIT;

  if Message then FName := MGroupsFileName
      else FName := FGroupsFileName;
  if Config_OpenFile(Group_F, FName, SizeOf(GroupRecord), ReadMode + DenyNone, False, False)>00
      then begin
             Config_DoneFile(Group_F);
             EXIT;
           end; { if }

  Config_SeekFile(Group_F, (Board-01));
  Config_ReadFile(Group_F, Group, 1);
  Config_DoneFile(Group_F);

  GroupName := Group.Name;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileRout, 'GroupName ( end )');
  {$ENDIF}
end; { fumc. GroupName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

Function  GetGroupRecord(BoardNr: SmallWord; CheckRDX, Message: Boolean; var GroupInf: Grouprecord): Boolean;
var Group_F : pFileObj;
    Fname   : String;
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logFileRout, 'GetGroupRecord');{$endif}
  Fillchar(GroupInf, SizeOf(GroupRecord), #00);
  GetGroupRecord := False;

  if Message then FName := MGroupsFileName else
     FName := FGroupsFileName;

  if Config_OpenFile(Group_F, FName, SizeOf(GroupRecord), ReadMode + DenyNone, False, False)>00
      then begin
             Config_DoneFile(Group_F);
             EXIT;
           end; { if }

  If CheckRDX then
      Config_SeekFile(Group_F, ((Ra250Group(BoardNr, Message)-1)))
       else Config_SeekFile(Group_F, (BoardNr-1));

  Config_ReadFile(Group_F, GroupInf, 1);
  Config_DoneFile(Group_F);

  GetGroupRecord := True;
end; { func. GetGroupRecord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

Function GetMessageRecord(var MsgInf: MessageRecord; BoardNr: SmallWord; CheckRDX: Boolean): Boolean;
var Msg_F   : pFileObj;
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logFileRout, 'getmessagerecord');{$endif}
  Fillchar(MsgInf, SizeOf(MessageRecord), #00);
  GetMessageRecord := False;

  if Config_OpenFile(Msg_F, MessagesFileName, SizeOf(MessageRecord), ReadMode + DenyNone, False, False)>00 then
    begin
      Config_DoneFile(Msg_F);
      EXIT;
    end; { if }

  If CheckRDX then
    Config_SeekFile(Msg_F, (Ra250MsgArea(BoardNr)-1))
     else Config_SeekFile(Msg_F, (BoardNr-1));

  Config_ReadFile(Msg_F, MsgInf, 1);
  Config_DoneFile(Msg_F);

  GetMessageRecord := True;
end; { func. GetMessageRecord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function  GetEleMessageRecord(var EleMSGInf: EleMessageRecord; BoardNr: SmallWord; CheckRDX: Boolean): Boolean;
var Msg_F   : pFileObj;
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logFileRout, 'getmessagerecord');{$endif}
  Fillchar(EleMsgInf, SizeOf(EleMessageRecord), #00);
  GetEleMessageRecord := False;

  if Config_OpenFile(Msg_F, MessageEleFileName, SizeOf(EleMessageRecord), ReadMode + DenyNone, False, False)>00 then
    begin
      Config_DoneFile(Msg_F);
      EXIT;
    end; { if }

  If CheckRDX then
    Config_SeekFile(Msg_F, (Ra250MsgArea(BoardNr)-1))
     else Config_SeekFile(Msg_F, (BoardNr-1));

  Config_ReadFile(Msg_F, EleMsgInf, 1);
  Config_DoneFile(Msg_F);

  GetEleMessageRecord := True;
end; { func. GetEleMessageRecord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure WriteMsgInf(FromWho, ToWho, Subject, MsgNr, BoardName: String; MsgPrivate: Boolean);
var MsgInf_F: Text;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileRout, 'WriteMsgInf (begin)');
  {$ENDIF}

  {$IFDEF WIN32}
    if SysUtils.FileExists('msginf') then
      SysUtils.DeleteFile('msginf');
  {$ENDIF}

  Assign(MsgInf_F, 'msginf');
  {$i-} System.ReWrite(MsgInf_F); {$I+}
  if IOResult>00 then
    begin
      {$IFDEF WITH_FULL}
        RaLog('!', 'Unable to write file "msginf"');
      {$ENDIF}

      EXIT;
    end; { if }

  {$i-}
    WriteLn(MsgInf_F, FromWho);
    WriteLn(MsgInf_F, ToWho);
    WriteLn(MsgInf_F, Subject);
    WriteLn(MsgInf_F, MsgNr);
    WriteLn(MsgInf_F, BoardName);
    If MsgPrivate then WriteLn(MsgInf_F, 'YES')
      else WriteLn(MsgInf_F, 'NO');
    Close(MsgInf_F);
  {$I+}
  If IOResult>00 then;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileRout, 'WriteMsgInf ( end )');
  {$ENDIF}
end; { proc. WriteMsgInf }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)


{$IFDEF WITH_FULL}
Procedure SearchNextEvent(var Event: EventRecord);
Const MinsADay = 24 * 60;

var Next_F   : pFileObj;
    EventInf : Array[1..20] of EventRecord;
    EventTemp: EventRecord;
    Counter  : Byte;
    Found    : Boolean;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileRout, 'SearchNextEvent (begin)');
  {$ENDIF}

  if CmdEventSet then
    begin
       Event.StartTime := MinsIsTime(NextEventMins);
       EXIT;
    end; { if }

  FillChar(Event, SizeOf(EventRecord), #00);
  FillChar(EventInf, SizeOf(EventInf), #00);

  Event.StartTime := 'none';
  NextEventMins := 10000;

  New(Next_F, Init);
  Next_F^.Assign(EventsFileName);
  Next_F^.FileMode := ReadMode + DenyNone;
  Next_F^.Open(SizeOf(EventInf));
  Next_F^.BlkRead(EventInf, 01);
  Dispose(Next_F, Done);

  EventTemp.StartTime := 'none';
  Found := False;

  For Counter := 01 to 20 do
   If EventInf[Counter].Status=01 then                              { Enabled }
    If ReadBit(EventInf[Counter].Days, GetDow) then
     If EventInf[Counter].LastTimeRun<>DateStr then
       begin
         Found := True;

         If MinsTillTime(EventInf[Counter].StartTime) <=
           MinsTillTime(EventTemp.StartTime) then
                begin
                  EventTemp := EventInf[Counter];
                  EventRecNr := Counter;
                end; { Found which is nearer by }
       end; { Status Enabled }

  If Found then
        begin
           Event := EventTemp;
           NextEventMins := MinsTillTime(Event.StartTime);
        end; { if found }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileRout, 'SearchNextEvent ( end )');
  {$ENDIF}
end; { proc. SearchNextEvent }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ReadSysInfoBBS(var SysInfo: SysinfoRecord);
var Templ_F: pFileObj;
begin
  if Config_OpenFile(Templ_F, SysInfoFileName, SizeOf(SysInfoRecord), ReadMode + DenyNone, False, False)=00 then
    begin
      Config_ReadFile(Templ_F, SysInfo, 1);
    end; { if configfile is zero }

  Config_DoneFile(Templ_F);
end; { proc. ReadSysInfoBBS }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

Procedure GetExitinfoInfo(var Exitinfo: ExitinfoRecord); { Get EXITINFO info }
var Templ_F   : pFileObj;
    OldFMode  : Longint;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileRout, 'GetExitinfoInfo (begin)');
    DebugObj.DebugLog(logFileRout, 'LastDate: '+Exitinfo.Userinfo.LastDate);
    DebugObj.DebugLog(logFileRout, 'DateStr : '+DateStr);
  {$ENDIF}

  Inc(Exitinfo.Userinfo.NoCalls);
  if Exitinfo.Userinfo.LastDate <> DateStr then
    begin
      Exitinfo.Userinfo.Elapsed := 00;
      Exitinfo.Userinfo.TodayK := 00;

      {$IFDEF WITH_FULL}
        ChangeTime(00, false);
      {$ENDIF}
    end; { If another date of logon }

  ReadSysInfoBBS(Exitinfo.SysInfo);
  if Exitinfo.Userinfo.LastPwdChange < 255 then
   Inc(Exitinfo.Userinfo.LastPwdChange);
  if Exitinfo.Userinfo.LastDobCheck < 255 then
  Inc(Byte(Exitinfo.Userinfo.LastDobCheck));

  Exitinfo.LoginTime       := TimeStr(false, false);
  Exitinfo.LoginDate       := DateStr;
  Exitinfo.LoginSec        := Exitinfo.Userinfo.Security;
  { ReadThru }
  Exitinfo.DownloadLimit   := LineCfg^.UsersKbLimit;
  Exitinfo.LogonPassWordCRC:= Exitinfo.Userinfo.PassWordCRC;
  Exitinfo.StatusLine      := 01;
  Exitinfo.DoesAVT         := ReadBit(Exitinfo.Userinfo.Attribute2, 1);
  Exitinfo.RipMode         := LineCfg^.RipOn;

  {$IFDEF WITH_FULL}
    SearchNextEvent(Exitinfo.EventInfo);
  {$ENDIF}

  if Config_OpenFile(Templ_F, TimeLogFileName, SizeOf(TimeLogRecord), ReadMode + DenyNone, False, False)=00 then
    begin
      Config_ReadFile(Templ_F, Exitinfo.TimeLogInfo, 1);
      Config_DoneFile(Templ_F);
    end { if configfile is zero }
     else begin
            Config_DoneFile(Templ_F);
            FillChar(Exitinfo.TimeLogInfo, SizeOf(TimeLogRecord), 00);
            Exitinfo.Timeloginfo.StartDate := DateStr;

            if Config_OpenFile(Templ_F, TimeLogFileName, SizeOf(TimeLogRecord), ReadWriteMode + DenyNone, True, True)=00 then
              begin
                Config_WriteFile(Templ_F, Exitinfo.TimeLogInfo, 1);
                Config_DoneFile(Templ_F);
              end { if }
                else Config_DoneFile(Templ_F);
          end; { if }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileRout, 'GetExitinfoInfo ( end )');
  {$ENDIF}
end; { proc. GetExitinfoInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
procedure GetProtocolRecord(CH: Char; var ProtInf: ProtocolRecord);
var Protocol_F  : pFileObj;
    NumRead     : NumReadType;
    NrItems     : Longint;
    ItemsCounter: Longint;
    InternKeys  : CfgRec.CharSet;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileRout, 'Getting character: '+CH);
  {$ENDIF}
  Fillchar(ProtInf, SizeOf(ProtInf), #00);

  GetInternKeys(InternKeys);
  If UpCase(CH) in InternKeys then
   begin;
     ProtInf.ActiveKey := CH;

     Case UpCase(CH) of
         'X' : if 'X' in Internkeys then ProtInf.Name := 'Xmodem';
         '1' : if '1' in Internkeys then ProtInf.Name := 'Xmodem/1K';
         'Q' : if 'Q' in Internkeys then ProtInf.Name := 'Xmodem/1K-G';
         'Y' : if 'Y' in Internkeys then ProtInf.Name := 'Ymodem';
         'G' : if 'G' in Internkeys then ProtInf.Name := 'Ymodem-G';
         'Z' : if 'Z' in Internkeys then ProtInf.Name := 'Zmodem';
       End; { case }

     if ProtInf.Name <> '' then EXIT;
   end; { if }

  New(Protocol_F, Init);
  Protocol_F^.Assign(ProtocolFileName);
  Protocol_F^.FileMode := ReadMode + DenyNone;
  if Protocol_F^.Open(1) then
    begin
      NrItems := Protocol_F^.FileSize DIV SizeOf(ProtocolRecord);
      ItemsCounter := 00;

      While (NrItems > ItemsCounter) AND (NrItems > 00) do
        begin
          Numread := Protocol_F^.BlkRead(ProtInf, SizeOf(ProtocolRecord));
          if NumRead <> SizeOf(ProtocolRecord) then BREAK;

          if UpCase(ProtInf.ActiveKey)=(CH) then BREAK
            else FillChar(ProtInf, SizeOf(ProtInf), #00);

          Inc(ItemsCounter);
        end; { While }
    end; { if }

  Dispose(Protocol_F, Done);
end; { proc. GetProtocolRecord }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GetAddress(Nr: Byte; var Address: NetAddress);
var Address_F: pFileObj;
begin
  FillChar(Address, SizeOf(Address), #00);

  if Nr in [0..9] then
    Address := GlobalCfg^.RaConfig^.Address[Nr];

  if Nr > 9 then
    begin
      if Config_OpenFile(Address_F, AddressFileName, SizeOf(NetAddress), ReadMode + DenyNone, false, false)=00 then
        begin
          Config_SeekFile(Address_F, Nr - 10);
          Config_ReadFile(Address_F, Address, 1);
          Config_DoneFile(Address_F);
        end { if }
          else Config_DoneFile(Address_F);
    end { if configfile is zero }

end; { proc. GetAddress }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SetAddress(Nr: Byte; var Address: NetAddress);
var Address_F: pFileObj;
begin
  if Nr in [0..9] then
    GlobalCfg^.RaConfig^.Address[Nr] := Address;

  if Nr > 9 then
    begin
      if Config_OpenFile(Address_F, AddressFileName, SizeOf(NetAddress), ReadWriteMode + DenyNone, true, false)=00 then
        begin
          Config_SeekFile(Address_F, Nr - 10);
          Config_WriteFile(Address_F, Address, 1);
          Config_DoneFile(Address_F);
        end { if }
          else Config_DoneFile(Address_F);
    end { if configfile is zero }

end; { proc. SetAddress }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ReadLanguageRA(Nr: Longint);          { Opens and reads LANGUAGE.RA }
var Language_F: pFileObj;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMain, 'ReadLanguageRa: '+FStr(Nr));
  {$ENDIF}

  Config_OpenFile(Language_F, LanguageFileName, 01, ReadMode + DenyNone, False, True);
  Config_SeekFile(Language_F, SizeOf(LanguageRecord) * Nr);
  if Config_ReadFile(Language_F, LineCfg^.Language^, SizeOf(LanguageRecord))>00 then
    begin
      Config_SeekFile(Language_F, (Max(0, GlobalCfg^.RaConfig^.NewUserLanguage-1)) * SizeOf(LanguageRecord));
      Config_ReadFile(Language_F, LineCfg^.Language^, SizeOf(LanguageRecord));
    end; { if }

  LineCfg^.Language^.MenuPath := ForceBack(LineCfg^.Language^.MenuPath);
  LineCfg^.Language^.TextPath := ForceBack(LineCfg^.Language^.TextPath);
  LineCfg^.Language^.QuesPath := ForceBack(LineCfg^.Language^.QuesPath);

  Config_DoneFile(Language_F);
end; { proc. ReadLanguageRA }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
function SearchNextMsgArea(GroupNum: Word): Word;
var Area_F    : pFileObj;
    MessageInf: MessageRecord;
begin
  SearchNextMsgArea := LineCfg^.Exitinfo^.Userinfo.MsgArea;

  if Config_OpenFile(Area_F, MessagesFileName, SizeOf(MessageRecord), ReadMode + DenyNone, False, False)>00
      then begin
             Config_DoneFile(Area_F);
             EXIT;
           end; { if }

  While Config_ReadFile(Area_F, MessageInf, 1) = 0 do
    begin
      if (MessageInf.Group = GroupNum) OR
          (MessageInf.AltGroup[1] = GroupNum) OR
           (MessageInf.AltGroup[2] = GroupNum) OR
            (MessageInf.AltGroup[3] = GroupNum) OR
             (ReadBit(MessageInf.Attribute2, 0)) then
               begin
                 SearchNextMsgArea := MessageInf.AreaNum;
                 BREAK;
               end; { if }
    end; { while }

  Config_DoneFile(Area_F);
end; { func. SearchNextMsgArea }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
function SearchNextFileArea(GroupNum: Word): Word;
var Area_F   : pFileObj;
    FilesInf : FilesRecord;
begin
  SearchNextFileArea := LineCfg^.Exitinfo^.Userinfo.FileArea;

  if Config_OpenFile(Area_F, FilesFileName, SizeOf(FilesRecord), ReadMode + DenyNone, False, False)>00
      then begin
             Config_DoneFile(Area_F);
             EXIT;
           end; { if }

  While Config_ReadFile(Area_F, FilesInf, 1) = 0 do
    begin
      if (FilesInf.Group = GroupNum) OR
          (FilesInf.AltGroup[1] = GroupNum) OR
           (FilesInf.AltGroup[2] = GroupNum) OR
            (FilesInf.AltGroup[3] = GroupNum) OR
             (ReadBit(FilesInf.Attrib2, 0)) then
              begin
                SearchNextFileArea := FilesInf.AreaNum;
                BREAK;
              end; { if }
    end; { while }

  Config_DoneFile(Area_F);
end; { func. SearchNextFileArea }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SelectAllCombined(var Combined: CombinedRecord);
var Counter   : Longint;
    Area_F    : pFileObj;
    MessageInf: MessageRecord;
begin
  Counter := 00;

  New(Area_F, Init);
  Area_F^.Assign(MessagesFileName);
  Area_F^.FileMode := ReadMode + DenyNone;

  if Area_F^.Open(1) then
    begin
      While (NOT Area_F^.EOF) AND (Counter < 200) do
        begin
           Area_F^.BlkRead(MessageInf, SizeOf(MessageRecord));

           if MessageInf.AreaNum <> 0 then
            if MessageInf.Name <> '' then
              begin
                Inc(Counter);
                Combined[Counter] := MessageInf.AreaNum;
              end; { if }
        end; { while }

    end; { if }

  Dispose(Area_F, Done);
end; { proc. SelectAllCombined }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoRaBusy(DoErase: Boolean);
var Temp_F  : pFileObj;
    MsgPath : String;
begin
  if GlobalCfg^.RaConfig = nil then EXIT;

  if GlobalCfg^.RaConfig^.SemPath = '' then MsgPath := GlobalCfg^.RaConfig^.SysPath
    else MsgPath := GlobalCfg^.RaConfig^.SemPath;

  New(Temp_F, Init);
  Temp_F^.Assign(MsgPath + 'rabusy.' + FStr(LineCfg^.RaNodeNr));
  Temp_F^.FileMode := ReadWriteMode + DenyNone;

  if DoErase then
    begin
      Temp_F^.Erase
    end
      else Temp_F^.Create(1);

  Dispose(Temp_F, Done);
end; { proc. DoRaBusy }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function OpenRAfile(S:String): String;                       { Open RA file }
var CurDir: String;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileRout, 'OpeningRaFile: ('+S+') (begin)');
  {$ENDIF}

  {-- Get the current directory --------------------------------------------}
  GetDir(0, CurDir);
  CurDir := ForceBack(CurDir);

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileRout, 'OpeningRaFile: (CurDir="'+CurDir+'")');
  {$ENDIF}

  OpenRaFile := SlowCase(S);  { Make default to first check so it is always compatible }

  if FileExist(CurDir + Slowcase(S)) then
    begin
      OpenRaFile := CurDir + Slowcase(S);
    end
     else
        begin
          if FileExist(GlobalCfg^.RaConfig^.SysPath + slowcase(S)) then
              OpenRaFile := GlobalCfg^.RaConfig^.SysPath + Slowcase(S)
               else begin
                      if FileExist(ForceBack(GetSysEnv) + Slowcase(S)) then
                        OpenRaFile := ForceBack(GetSysEnv) + Slowcase(S);
                    end; { else }
        end; { if }

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFileRout, 'OpeningRaFile: ('+S+') ( end )');
  {$ENDIF}
end; { func. OpenRaFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure BuildFastIndex;
var Users_F    : pFileObj;
    UsersIDX_F : pFileObj;
    Temp_F     : pFileObj;
    UsersInf   : UsersRecord;
    UsersIdx   : UsersIdxRecord;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logUserBase, 'BuildFastIndex - (BEGIN)');
  {$ENDIF}

  New(Users_F, Init);
  Users_F^.Assign(GlobalCfg^.RaConfig^.MsgBasePath + UserBaseName);
  Users_F^.FileMode := ReadMode + DenyWrite;
  if NOT Users_F^.Open(1) then
    begin
      {$IFDEF WITH_DEBUG}
        DebugObj.DebugLog(logUserBase, 'BuildFastIndex - Cannot open that file: '+FStr(Users_F^.IoResult));
      {$ENDIF}

      Dispose(Users_F, Done);
      EXIT;
    end; { if }

  New(UsersIDX_F, Init);
  UsersIDX_F^.Assign(GlobalCfg^.RaConfig^.MsgBasePath + 'USERSIDX.NEW');
  UsersIDX_F^.FileMode := WriteMode + DenyWrite;
  if NOT UsersIDX_F^.Create(1) then
    begin
      {$IFDEF WITH_DEBUG}
        DebugObj.DebugLog(logUserBase, 'BuildFastIndex - Cannot create IDX file: '+FStr(UsersIdx_F^.IoResult));
      {$ENDIF}

      Dispose(Users_F, Done);
      Dispose(UsersIDX_F, Done);
      EXIT;
    end; { if }

  While (NOT Users_F^.EOF) do
    begin
       Users_F^.BlkRead(UsersInf, SizeOf(UsersRecord));

       FillChar(UsersIdx, SizeOf(UsersIdx), #00);

       UsersIdx.NameCRC32 := RaCrc(UsersInf.Name, true);
       UsersIdx.HandleCRC32 := RaCrc(UsersInf.Handle, true);

       UsersIDX_F^.BlkWrite(UsersIDX, SizeOf(UsersIdxRecord));
     end; { While }

  Dispose(Users_F, Done);
  UsersIDX_F^.Close;

  {$i-}
    New(Temp_F, Init);

    Temp_F^.Assign(GlobalCfg^.RaConfig^.MsgBasePath + UserBaseIdxName);
    Temp_F^.FileMode := ReadWriteMode + DenyNone;
    Temp_F^.Erase;

    Dispose(Temp_F, Done);
  {$i+}
  if IOresult>00 then ;

  RenameObjWithDrive(UsersIdx_F, GlobalCfg^.RaConfig^.MsgbasePath + UserBaseIdxName);
  Dispose(UsersIDX_F, Done);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logUserBase, 'BuildFastIndex - ( END )');
  {$ENDIF}
end; { proc. BuildFastIndex }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure BuildUserExt;
var Users_F    : pFileObj;
    UsersExt_F : pFileObj;
    Temp_F     : pFileObj;
    UserExt    : UserExtensionRecord;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logUserBase, 'BuildUserExt - (BEGIN)');
  {$ENDIF}

  {-- Make sure we have the size of the userbase extension file -----------}
  LineCfg^.UserExtensionSize := usrext_GetSize;

  {-- Initialize the base userbase file -----------------------------------}
  New(Users_F, Init);
  Users_F^.Assign(GlobalCfg^.RaConfig^.MsgBasePath + UserBaseName);
  Users_F^.FileMode := ReadMode + DenyWrite;
  if NOT Users_F^.Open(1) then
    begin
      {$IFDEF WITH_DEBUG}
        DebugObj.DebugLog(logUserBase, 'BuildUserExtension - Cannot open that file: '+FStr(Users_F^.IoResult));
      {$ENDIF}

      Dispose(Users_F, Done);
      EXIT;
    end; { if }

  {-- Now open the new userbase file ----------------------------------------}
  New(UsersExt_F, Init);
  UsersExt_F^.Assign(GlobalCfg^.RaConfig^.MsgBasePath + 'usersele.new');
  UsersExt_F^.FileMode := WriteMode + DenyWrite;
  if NOT UsersExt_F^.Create(1) then
    begin
      {$IFDEF WITH_DEBUG}
        DebugObj.DebugLog(logUserBase, 'BuildUserExtension - Cannot create IDX file: '+FStr(UsersExt_F^.IoResult));
      {$ENDIF}

      Dispose(Users_F, Done);
      Dispose(UsersExt_F, Done);
      EXIT;
    end; { if }

  {-- now loop through the file --------------------------------------------}
  While (NOT Users_F^.EOF) do
    begin
       {-- get the user basefile -------------------------------------------}
       Users_F^.BlkRead(LineCfg^.Exitinfo^.UserInfo, SizeOf(UsersRecord));

       {-- Set the user record ---------------------------------------------}
       LineCfg^.Exitinfo^.UserRecord := (Users_F^.FilePos - SizeOf(UsersRecord)) DIV SizeOf(UsersRecord);

       {-- now build the new data ------------------------------------------}
       FillChar(UserExt, SizeOf(UserExt), #00);
       usrExt_SetDefaults(UserExt);

       {-- and write it to disk --------------------------------------------}
       UsersExt_F^.BlkWrite(UserExt, LineCfg^.UserExtensionSize);
     end; { While }

  Dispose(Users_F, Done);
  UsersExt_F^.Close;

  {$i-}
    New(Temp_F, Init);

    Temp_F^.Assign(GlobalCfg^.RaConfig^.MsgBasePath + UserBaseEleName);
    Temp_F^.FileMode := ReadWriteMode + DenyNone;
    Temp_F^.Erase;

    Dispose(Temp_F, Done);
  {$i+}
  if IOresult>00 then ;

  RenameObjWithDrive(UsersExt_F, GlobalCfg^.RaConfig^.MsgbasePath + UserBaseEleName);
  Dispose(UsersExt_F, Done);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logUserBase, 'BuildUserExt - ( END )');
  {$ENDIF}
end; { proc. BuildUserExt }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure BuildLastread;
var Users_F    : File;
    Lastread_F : File;
    Temp_F     : File;
    Orig_F     : File;
    UsersInf   : UsersRecord;
    LastreadInf: LastReadRecord;
    OldFMode   : Longint;
begin
  Assign(Users_F, GlobalCfg^.RaConfig^.MsgBasePath + UserBaseName);
  OldFMode := FileMode;
  FileMode := ReadMode + DenyWrite;
  {$i-}
    Reset(Users_F, 1);
  {$I+}
  FileMode := OldFMode;
  If IOResult>00 then EXIT;

  Assign(Orig_F, GlobalCfg^.RaConfig^.MsgBasePath + UserBaseLastRead);
  OldFMode := FileMode;
  FileMode := ReadMode + DenyNone;
  {$i-}
    Reset(Orig_F, 1);
  {$I+}
  FileMode := OldFMode;
  If IOResult>00 then
    begin
      Close(Users_F);
      EXIT;
    end; { if }


  Assign(Lastread_F, GlobalCfg^.RaConfig^.MsgBasePath + 'LASTREAD.NEW');
  OldFMode := FileMode;
  FileMode := WriteMode + DenyWrite;
  {$i-}
    Rewrite(Lastread_F, 1);
  {$I+}
  FileMode := OldFMode;
  If IOResult>00 then
    begin
      Close(Users_F);
      Close(Orig_F);
      EXIT;
    end;


  While (IOResult=00) AND (NOT Eof(Users_F)) do
    begin
       {$i-}
         BlockRead(Users_F, UsersInf, SizeOf(UsersRecord));
         FillChar(LastreadInf, SizeOf(LastReadInf), 00);
       {$i+}
       if IoResult > 0 then BREAK;

       {$i-}
         BlockRead(Orig_F, LastReadInf, SizeOf(LastReadRecord));
       {$i+}
       if IoResult > 0 then
         FillChar(lastreadInf, SizeOf(LastReadInf), #00);

       {$i+}
         BlockWrite(Lastread_F, LastReadInf, SizeOf(LastreadRecord));
       {$i+}
    end; { While }

  {$i-}
    Close(LastRead_F);
    Close(Users_F);
    Close(Orig_F);

    {$i-}
      Assign(Temp_F, GlobalCfg^.RaConfig^.MsgbasePath + UserBaseLastRead);
      Erase(Temp_F);
    {$i+}
    if IOresult>00 then ;

    RenameWithDrive(Lastread_F, GlobalCfg^.RaConfig^.MsgbasePath + UserBaseLastRead);
  {$i-}
  If IOResult>00 then;
end; { proc. BuildLastRead }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function SemaExist(S: String): Boolean;
var TempStr: String;
begin
  TempStr := GlobalCfg^.RaConfig^.SemPath;

  if (TempStr = '') OR (NOT FileExist(TempStr)) then
    Tempstr := GlobalCfg^.RaConfig^.Syspath;

  SemaExist := FileExist(TempStr + s);
end; { func. SemaExist }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure RemoveSema(S: String);
var TempStr: String;
begin
  TempStr := GlobalCfg^.RaConfig^.SemPath;

  if (TempStr = '') OR (NOT FileExist(TempStr)) then
    Tempstr := GlobalCfg^.RaConfig^.Syspath;

  EraseFile(TempStr + S);
end; { func. RemoveSema }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CreateSema(S: String);
var TempStr: String;
    F      : pFileObj;
begin
  TempStr := GlobalCfg^.RaConfig^.SemPath;

  if (TempStr = '') OR (NOT FileExist(TempStr)) then
    Tempstr := GlobalCfg^.RaConfig^.Syspath;

  New(F, Init);
  F^.Assign(TempStr + S);
  F^.FileMode := ReadWritemode + DenyNone;
  F^.Create(1);
  Dispose(F, Done);
end; { func. CreateSema }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetLogFileName: String;
var TempName: String;
begin
  TempName := GlobalCfg^.RaConfig^.LogFileName;
  Replace('*N', FStr(LineCfg^.RaNodeNr), TempName);

  if JustPath(TempName) = '' then
    TempName := ForceBack(GetCurDir) + TempName;

  if NOT RunningBBS then
    TempName := GlobalCfg^.ELCONFIG^.UtilityLogFileName;

  GetLogFileName := TempName;
end; { func. GetLogFileName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure InitSystemNames;
begin
  GlobalCfg^.RaConfig^.SysPath := ForceBack(GlobalCfg^.RaConfig^.SysPath);
  GlobalCfg^.RaConfig^.FileBasePath := ForceBack(GlobalCfg^.RaConfig^.FileBasePath);
  GlobalCfg^.RaConfig^.MsgBasePath := ForceBack(GlobalCfg^.RaConfig^.MsgBasePath);
  GlobalCfg^.RaConfig^.TempCdFilepath := ForceBack(GlobalCfg^.RaConfig^.TempCdFilepath);
  GlobalCfg^.RaConfig^.SemPath := ForceBack(GlobalCfg^.RaConfig^.SemPath);
  GlobalCfg^.RaConfig^.MenuPath := ForceBack(GlobalCfg^.RaConfig^.MenuPath);
  GlobalCfg^.RaConfig^.NodeListPath := ForceBack(GlobalCfg^.RaConfig^.NodeListPath);
  GlobalCfg^.RaConfig^.AttachPath := ForceBack(GlobalCfg^.RaConfig^.AttachPath);
  GlobalCfg^.RaConfig^.TextPath := ForceBack(GlobalCfg^.RaConfig^.TextPath);
  GlobalCfg^.RaConfig^.RipIconpath := ForceBack(GlobalCfg^.RaConfig^.RipIconPath);

  LogFileName        := GetLogFileName;
  ProtocolFileName   := OpenRaFile('protocol.ra');
  LanguageFileName   := OpenRaFile('language.ra');
  LimitsFileName     := OpenRaFile('limits.ra');
  EventsFileName     := OpenRaFile('events.ra');
  FGroupsFileName    := OpenRaFile('fgroups.ra');
  MGroupsFileName    := OpenRaFile('mgroups.ra');
  FilesFileName      := OpenRaFile('files.ra');
  ModemFileName      := OpenRaFile('modem.ra');
  TelnetFileName     := OpenRaFile('telnet.ele');
  AddressFileName    := OpenRaFile('akas.bbs');
  {$IFNDEF WIN32}
    PageFileName       := OpenRaFile('page.ra');
  {$ELSE}
    PageFileName       := OpenRaFile('page.wav');
  {$ENDIF}
  PageStatFileName   := OpenRaFile('pagestat.ra');
  MessagesFileName   := OpenRaFile('messages.ra');
  TagListFileName    := 'taglist.ra';
  LastCallFileName   := ForceBack(GlobalCfg^.RaConfig^.SysPath) + 'lastcall.bbs';
  SysInfoFileName    := ForceBack(GlobalCfg^.RaConfig^.Syspath) + 'sysinfo.bbs';
  TimeLogFileName    := OpenRaFile('timelog.bbs'); {!!}
  MessageEleFileName := OpenRaFile('messages.ele');
  NewsServerFileName := OpenRaFile('nwserver.ele');
  EleFilesFileName   := OpenRaFile('files.ele');
end; { proc. InitSystemNames }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function SearchCTLFile(const CtlName: String; const UserName: String; const WildCard: Boolean): Boolean; { Searchs a CTL-file }
var Ctl_F  : pFileObj;
    TempStr: ShortString;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileRout, 'SearchCtlFile: '+CtlName+ ' - (begin)');
  {$ENDIF}
  SearchCtlFile := False;

  New(Ctl_F, Init);
  Ctl_F^.Assign(OpenRaFile(CtlName));
  Ctl_F^.FileMode := ReadMode + DenyNone;
  if NOT Ctl_F^.Open(1) then
    begin
      Dispose(Ctl_F, Done);
      EXIT;
    end; { if }

  While (NOT Ctl_F^.EOF) do
    begin
      Ctl_F^.ReadLn(TempStr);
      if Ctl_F^.IoResult > 00 then BREAK;

      if WildCard then
       if IsWildCard(TempStr, UserName) then
         begin
           SearchCtlFile := True;
           BREAK;
         end; { Is wildcard }

      if Pos(SUPcase(Trim(UserName)), SUPcase(Trim(TempStr)))>00 then
        begin
          SearchCtlFile := True;
          BREAK;
        end; { while }
    end; { while }

  Dispose(Ctl_F, Done);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileRout, 'SearchCtlFile: '+CtlName+ ' - ( end )');
  {$ENDIF}
end; { proc. SearchCtlFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CreateTempDir(S: String; NodeNr: Longint): String;
var DatTmp : String;
    TimTmp : String;
    DirName: String;
begin
  DatTmp := JDates.DateStr;
  TimTmp := JDates.TimeStr(True, True);

  While Pos('-', DatTmp) > 00 do
    Delete(DatTmp, Pos('-', DatTmp), 1);
  While Pos('/', DatTmp) > 00 do
    Delete(DatTmp, Pos('/', DatTmp), 1);
  While Pos(':', TimTmp) > 00 do
    Delete(TimTmp, Pos(':', TimTmp), 1);
  While Pos('.', TimTmp) > 00 do
    Delete(TimTmp, Pos('.', TimTmp), 1);

  DirName := Copy(TimTmp+DatTmp, 1, 8) + '.' + FStr(NodeNr);

  {$i-}
    MkDir(ForceBack(S) + DirName);
  {$i+}
  if IOResult > 00 then CreateTempDir := ''
    else CreateTempdir := ForceBack(S) + DirName;
end; { func. CreateTempDir }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  UpToDateFile(FName1, FName2: String): Boolean;
var Temp_F   : pFileObj;
    Time1,
    Time2    : Longint;

function Compare(Time1, Time2: Longint): Boolean;
var Dt1, Dt2: DateTime;
    L1, L2  : Longint;
begin
  FillChar(Dt1, SizeOf(Dt1), 0);
  FillChar(Dt2, SizeOf(Dt2), 0);
  UnpackTime(Time1, Dt1);
  UnpackTime(Time2, Dt2);

  L1 := Norm2Unix(Dt1.Year, Dt1.Month, Dt1.Day, Dt1.Hour, Dt1.Min, Dt1.Sec);
  L2 := Norm2Unix(Dt2.Year, Dt2.Month, Dt2.Day, Dt2.Hour, Dt2.Min, Dt2.Sec);

  if L2 > L1 then Compare := true
    else Compare := (InRange(L1, L2 - 2, L2 + 2));
end; { func. Compare }

begin
  UpToDateFile := FALSE;

  {--------------------- Get the filetime for the first file ------------------}
  New(Temp_F, Init);
  Temp_F^.Assign(FName1);
  Temp_F^.FileMode := ReadMode + DenyNone;
  if Temp_F^.Open(1) then
    begin
      Time1 := Temp_F^.GetFileTime;
    end; { if }
  Dispose(Temp_F, Done);


  {-------------------- Get the filetime for the second file -----------------}
  New(Temp_F, Init);
  Temp_F^.Assign(FName2);
  Temp_F^.FileMode := ReadMode + DenyNone;
  if Temp_F^.Open(1) then
    begin
      Time2 := Temp_F^.GetFileTime;
    end; { if }
  Dispose(Temp_F, Done);


  UpToDateFile := (Compare(Time1, Time2));
end; { func. UpToDateFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure UpdateStatistics;
var SysInf_F: pFileObj;
    Last_F  : pFileObj;
    LastInf : LastCallRecord;
begin
  if LineCfg^.LoggedOn then
   If GetFileDate(LastCallFileName) <> JDates.DateStr then
       EraseFile(LastCallFileName);

  if LineCfg^.LoggedOn then
   if Config_OpenFile(Last_F, LastCallFileName, 01, WriteMode + DenyNone, True, False)=00 then
      begin
        Config_SeekFile(Last_F, Last_F^.FileSize);

        FillChar(LastInf, SizeOf(LastcallRecord), #00);

        if LineCfg^.RaNodeNr >= WebNodeBase then
          begin
            LastInf.Line := 255;
          end
            else begin
                   LastInf.Line := Byte(LineCfg^.RaNodeNr);
                 end; { if }
        LastInf.Name      := LineCfg^.Exitinfo^.Userinfo.Name;
        LastInf.Handle    := LineCfg^.Exitinfo^.Userinfo.Handle;
        LastInf.City      := LineCfg^.Exitinfo^.Userinfo.Location;
        LastInf.Baud      := LineCfg^.Exitinfo^.Baud;
        LastInf.Times     := LineCfg^.Exitinfo^.Userinfo.NoCalls;
        LastInf.LogOn     := LineCfg^.Exitinfo^.LoginTime;
        LastInf.LogOff    := TimeStr(False, False);

        if LineCfg^.RaNodeNr >= WebNodeBase then
          begin
            LastInf.Logon := 'Ele';
            LastInf.LogOff:= 'WEB';
          end; { if }

        if ReadBit(LineCfg^.Exitinfo^.Userinfo.Attribute2, 3) then
          SetBit(LastInf.Attribute, 0)
           else ClearBit(LastInf.Attribute, 0);                  { Hidden Bit }

        Config_WriteFile(Last_F, LastInf, SizeOf(LastCallRecord));
        Config_DoneFile(Last_F);
      end; { if opening=00 }

  Inc(LineCfg^.Exitinfo^.SysInfo.TotalCalls);
  if LineCfg^.LoggedOn then
    begin
      LineCfg^.Exitinfo^.SysInfo.LastCaller := LineCfg^.Exitinfo^.Userinfo.Name;
      LineCfg^.Exitinfo^.SysInfo.LastHandle := LineCfg^.Exitinfo^.Userinfo.Handle;
    end; { if }
  FillChar(LineCfg^.Exitinfo^.SysInfo.ExtraSpace, SizeOf(LineCfg^.Exitinfo^.SysInfo.ExtraSpace), 00);

  if LineCfg^.LoggedOn then
   if LineCfg^.Exitinfo^.SysInfo.TotalCalls>=00 then
    if Config_OpenFile(SysInf_F, SysInfoFileName, 01, WriteMode + DenyNone, True, False)=00 then
      begin
        Config_SeekFile(SysInf_F, 00);
        Config_WriteFile(SysInf_F, LineCfg^.Exitinfo^.SysInfo, SizeOf(LineCfg^.Exitinfo^.SysInfo));
        Config_DoneFile(SysInf_F);
      end; { if }

  Inc(LineCfg^.Exitinfo^.Timeloginfo.Busyperhour[Gethour], LineCfg^.OnlineTime);

  if LineCfg^.LoggedOn then
   if Config_OpenFile(SysInf_F, TimelogFileName, 01, WriteMode + DenyNone, True, False)=00 then
     begin
       Config_SeekFile(SysInf_F, 00);
       Config_WriteFile(SysInf_F, LineCfg^.Exitinfo^.TimeLogInfo, SizeOf(LineCfg^.Exitinfo^.TimeLogInfo));
       Config_DoneFile(SysInf_F);
     end; { if }

end; { proc. UpdateStatistics }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function IsElexerScript(ScrName: String): Boolean;
var Ext: String;
begin
  IsElexerScript := FALSE;

  {-- extract the extension ------------------------------------------------}
  Ext := SUpCase(JustExtension(ScrName));

  {-- if its a questionnaire, its safe -------------------------------------}
  if Ext = 'Q-A' then
    begin
      IsElexerScript := FALSE;
      EXIT;
    end; { if }

  {-- if its ends with ELM we can be sure ----------------------------------}
  if Ext = 'ELM' then
    begin
      IsElexerScript := TRUE;
      EXIT;
    end; { if }

  {-- make sure it does have an extension ----------------------------------}
  {-- if it has an extension we default to EleXer --------------------------}
  if (Ext <> '') then
    begin
      IsElexerScript := TRUE;
      EXIT;
    end; { if }

  {-- make sure it does have an extension ----------------------------------}
  {-- if it has an extension and a path, we default to EleXer --------------}
  if (Ext = '') then
    begin
      if FileExist(ScrName + '.ELM') then
        begin
         IsElexerScript := TRUE;
         EXIT;
        end; { if }

      if FileExist(LineCfg^.Language^.QuesPath + ScrName + '.ELM') then
        begin
         IsElexerScript := TRUE;
         EXIT;
        end; { if }
    end; { if }
end; { func. IsElexerScript }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { FileRout }
