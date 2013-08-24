unit Transfer;
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
{$IFDEF WIN32} {$H-} {$ENDIF}
{&delphi-}
(*
**
** The filetransfer of EleBBS
**
** Copyright (c) 1997 by Maarten Bekers
**
** Created : 10-Aug-1997
** Last update : 01-Oct-1997
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF WITH_FULL}
  This unit shouldn''t be compiled for an utility program
{$ENDIF}

{$IFDEF ISCGI}
  This unit should not be compiled for EleWEB
{$ENDIF}

uses CfgRec, Global, GenDos, User_U, CurTime, Multiln
      {$IFDEF TCPIP}
        ,FtpUnit
      {$ENDIF}

      {$IFDEF ELEUNIX}
        ,TtyCom
      {$ENDIF}

      {$IFNDEF VirtualPascal}
        ,Use32
      {$ENDIF}

      {$IFDEF WINGUI}
        ,Windows
        ,Classes
      {$ENDIF};


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GetInternKeys(var InternKeys: CfgRec.CharSet);
procedure CopyFiles_FromCDROm;
Function AcceptUploadedFile(FName: String): Boolean;
Function DownloadRestrictions(ShowUser: Boolean): Boolean;
procedure GiveBackUploadTime(StartTime: Longint);
procedure CheckForVirusScan(const FilesInf: FilesRecord);
Function LocalDownload(var NrDownloads, DownloadsK: LongInt): Boolean;
Function LocalUpload(var Uploads, UploadsK: Longint; FilesInf: FilesRecord; FAttaching, UpMsg: Boolean;
                     UlPath: String): Boolean;
Function DoNormalDownload(var NrDownloads, DownloadsK: LongInt): Boolean;
Procedure DoDownload(MiscData: String; Global, Specific, AnyFile, AllowEdit, AllFree: Boolean);
Procedure DoUpload(MiscData: String; FAttaching, UpMsg: Boolean; UlPath: String; var Uploads: Longint);
function Zmodem_Time(Baud, Size: Longint): Longint;
Function DoNormalUpload(var NrDownloads, DownloadsK, NrUploads, UploadsK: Longint; FilesInf: FilesRecord;
                        UpMsg: Boolean; UploadPath: String; FAttaching: Boolean): Boolean;
Procedure GetFileDescriptions;
procedure DeleteCDROMfiles;
procedure DoTransferExternal(Description, Download: Boolean; ProtInf: ProtocolRecord;
                                    var NrDownloads, DownloadsK: LongInt; UlPath: String;AreaNum:Use32.Word;
                                    Fattaching: Boolean);
Procedure DoTransferInternal(Description, Download: Boolean; ProtInf: ProtocolRecord;
                         var NrDownloads, DownloadsK: LongInt; UlPath: String; AreaNum: Word;
                         MessageUpping, FAttaching: Boolean);

Procedure AskForFilePasswords;
Procedure UpdateFDB(TagHdr: TagFileRecord);


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

 uses BitWise,
      Memman,
       Debug_U,
       TagObj,
       TagUnit,
       Support,
       OutBlock,
       {$ifdef newprot}
         prot2,
       {$else}
         ProtProc,
       {$endif}
       Ra2Fdb,
       MgrFDb,
       StatusB,
       Terminal,
       ApFossil,
       SysUtils,

        {$IFDEF WINGUI}
          Win_Main,
          Dialogs,
          Forms,
        {$ENDIF}

        {$IFDEF OS2}
          Os2Base,
        {$ENDIF}

        {$IFDEF FPC}
          Dos,
          Crt,
        {$ENDIF}

        {$IFDEF VirtualPascal}
          Dos,
          Crt,
          VpSysLow,
        {$ENDIF}

        {$IFDEF MSDOS}
          Dos,
          Crt,
          Multi,
        {$ENDIF}

          ComUnit,
          Limit_U,
          JDates,
          RAL,
          InOut_U,
          Input_U,
          Colors,
          MenuFunc,
          Desc_U,
          LineEd,

          ooZmodem,
          ooYModem,
          ooXModem,
          ApTimer,
          ApMisc,
          OoAbsPcl,
          MnuSer,
          FastScrn,
          Control,
          ExecFunc,
          ElLog_U,
          Dispans,
          FileRout,
          Filesys,
          Access_U,
          StUtils,
          FileObj,
          ObjDec,

         Cases, StrPath, LongStr,
          CentrStr, WordStr, GenFile;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Zmodem_Time(Baud, Size: Longint): Longint;
begin
  if Baud>00 then
    ZModem_Time := Round(((Size * 10.00) / FixBaud(Baud)) / 60)
      else ZModem_Time := 00;
end; { func. ZModem_Time }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GiveBackUploadTime(StartTime: Longint);
var SecsSpent: Longint;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'GiveBackUploadTime (begin)');
  {$ENDIF}

  if GlobalCfg^.RaConfig^.CreditFactor <= 0 then EXIT;
  SecsSpent := (CurrentTime - StartTime);
  if SecsSpent < 60 then EXIT;

  WriteLn;
  Write('`A12:', Langobj^.ralGet(ralGranted), #32,
                   Round(SecsSpent DIV 60), #32,
                   LangObj^.ralGet(ralMinutes));

  if SecsSpent > 119 then
    RaLog('>', 'Granted an extra '+FStr(SecsSpent DIV 60)+' minutes for uploading')
     else RaLog('>', 'Granted an extra '+FStr(SecsSpent DIV 60)+' minute for uploading');

  ChangeTime(+ (SecsSpent DIV 60), false);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'GiveBackUploadTime ( end )');
  {$ENDIF}
end; { proc. GivebackUploadTime }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GetInternKeys(var InternKeys: CfgRec.CharSet);
begin
  InternKeys := [];

  If GlobalCfg^.RaConfig^.ProtocolAttrib[1] in [01, 02] then         { Always, Error Free }
   begin
     If (GlobalCfg^.RaConfig^.ProtocolAttrib[1] = 02) then
      If LineCfg^.Exitinfo^.ErrorFreeConnect then Include(InternKeys, 'X');
     If (GlobalCfg^.RaConfig^.ProtocolAttrib[1] = 01) then Include(InternKeys, 'X');
   end; { if }

  If GlobalCfg^.RaConfig^.ProtocolAttrib[2] in [01, 02] then         { Always, Error Free }
   begin
     If (GlobalCfg^.RaConfig^.ProtocolAttrib[2] = 02) then
      If LineCfg^.Exitinfo^.ErrorFreeConnect then Include(InternKeys, '1');
     If (GlobalCfg^.RaConfig^.ProtocolAttrib[2] = 01) then Include(InternKeys, '1');
   end; { if }

  If GlobalCfg^.RaConfig^.ProtocolAttrib[3] in [01, 02] then         { Always, Error Free }
   begin
     If (GlobalCfg^.RaConfig^.ProtocolAttrib[3] = 02) then
      If LineCfg^.Exitinfo^.ErrorFreeConnect then Include(InternKeys, 'Q');
     If (GlobalCfg^.RaConfig^.ProtocolAttrib[3] = 01) then Include(InternKeys, 'Q');
   end; { if }

  If GlobalCfg^.RaConfig^.ProtocolAttrib[4] in [01, 02] then         { Always, Error Free }
   begin
     If (GlobalCfg^.RaConfig^.ProtocolAttrib[4] = 02) then
      If LineCfg^.Exitinfo^.ErrorFreeConnect then Include(InternKeys, 'Y');
     If (GlobalCfg^.RaConfig^.ProtocolAttrib[4] = 01) then Include(InternKeys, 'Y');
   end; { if }

  If GlobalCfg^.RaConfig^.ProtocolAttrib[5] in [01, 02] then         { Always, Error Free }
   begin
     If (GlobalCfg^.RaConfig^.ProtocolAttrib[5] = 02) then
      If LineCfg^.Exitinfo^.ErrorFreeConnect then Include(InternKeys, 'G');
     If (GlobalCfg^.RaConfig^.ProtocolAttrib[5] = 01) then Include(InternKeys, 'G');
   end; { if }

  If GlobalCfg^.RaConfig^.ProtocolAttrib[6] in [01, 02] then         { Always, Error Free }
   begin
     If (GlobalCfg^.RaConfig^.ProtocolAttrib[6] = 02) then
      If LineCfg^.Exitinfo^.ErrorFreeConnect then Include(InternKeys, 'Z');
     If (GlobalCfg^.RaConfig^.ProtocolAttrib[6] = 01) then Include(InternKeys, 'Z');
   end; { if }

end; { proc. GetInternKeys }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

{$IFDEF WITH_FULL}
function DownloadRestrictions(ShowUser: Boolean): Boolean;
var TotalK      : Longint;
    TotalFiles  : Longint;
    TotalMinutes: Longint;
    Counter     : Byte;
    TagInfo     : TagFileRecord;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'DownloadRestrictions (begin)');
  {$ENDIF}

  DownloadRestrictions := false;
  TotalK      := 00;
  TotalFiles  := 00;
  TotalMinutes:= 00;

  for Counter := 01 to TaggingObj^.CurrentTagged do
   begin
     TaggingObj^.GetTagHeader(Pred(Counter), TagInfo);

     if NOT ReadBit(TagInfo.Attrib, 2) then             { NOT Free }
      if NOT ReadBit(TagInfo.Attrib, 6) then
        begin
          Inc(TotalK, TagInfo.Size DIV 1024);
          Inc(TotalFiles, 1);
        end; { is not a free file }

     if NOT ReadBit(TagInfo.Attrib, 6) then               { NoTime }
       Inc(TotalMinutes, ZModem_Time(LineCfg^.Exitinfo^.Baud, TagInfo.Size));
   end; { for }

  if LineCfg^.UsersKbLimit <> UnlimitedValue then
    if TotalK > 00 then
     If (TotalK + LineCfg^.Exitinfo^.Userinfo.TodayK) > LineCfg^.UsersKbLimit then
       begin
         DownloadRestrictions := False;

         if ShowUser then
           begin
             WriteLn('`A15:', LangObj^.ralGet(ralExceed), #32, LineCfg^.UsersKbLimit, #32, LangObj^.ralGet(ralDailyLm2));
             InputObj^.PressEnter(True, False);
           end; { if }

         EXIT;
       end; { Today K-limit }

  if (TotalFiles > 00) then
   if LineCfg^.LimitsInfo^.RatioNum>00 then
    if (LineCfg^.Exitinfo^.Userinfo.Downloads + (TotalFiles)) { Include those already tagged }
       > ((LineCfg^.Exitinfo^.UserInfo.Uploads * LineCfg^.LimitsInfo^.RatioNum) + 01) then
           begin
             DownloadRestrictions := False;

             if ShowUser then
               begin
                 if DisplayHotFile('ratio', []) = #01 then
                   begin
                     WriteLn('`A15:', LangObj^.ralGet(ralExceed), #32, LineCfg^.LimitsInfo^.Rationum,':1', #32,
                             LangObj^.ralGet(ralRatioLm));
                     InputObj^.PressEnter(True, False);
                   end; { if }
                end; { if }

             EXIT;
           end; { Ratio (num) }

  if (TotalFiles > 00) then
   if LineCfg^.LimitsInfo^.RatioK>00 then
    if (LineCfg^.Exitinfo^.Userinfo.DownloadsK + (TotalK)) >
       ((LineCfg^.Exitinfo^.Userinfo.UploadsK * LineCfg^.LimitsInfo^.RatioK) + 01) then
         begin
           DownloadRestrictions := False;

           if ShowUser then
             begin
               if DisplayHotFile('ratiok', []) = #01 then
                 begin
                   WriteLn('`A15:', LangObj^.ralGet(ralExceed), #32,
                           LineCfg^.LimitsInfo^.RatioK,':1', #32, LangObj^.ralGet(ralRatioLm));
                   InputObj^.PressEnter(True, False);
                 end; { if }
             end; { if }

           EXIT;
         end; { if }

  if TotalFiles > 00 then
   if TotalMinutes > LineCfg^.Exitinfo^.TimeLimit then
     begin
       DownloadRestrictions := False;

       if ShowUser then
         begin
           if DisplayHotFile('XFERTIME', []) = #01 then
             begin
               WriteLn('`A15:', LangObj^.ralGet(ralExceed), #32, LangObj^.ralGet(ralTimeLm));
               InputObj^.PressEnter(true, false);
             end; { if }
         end; { if }

       EXIT;
     end; { if }

  DownloadRestrictions := True;
end; { func. DownloadRestrictions }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

{$IFDEF WITH_FULL}
procedure AskForFilePasswords;
var HeaderShown    : Boolean;
    Counter        : Byte;
    TagInfo        : TagFileRecord;
    TempStr        : String;
    TempName       : String;
{$IFDEF MSDOS}
    MustDelete     : Array[1..100] of String[12];
{$ELSE}
    MustDelete     : Array[1..100] of String;
{$ENDIF}
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logTransfer, 'AskForFilePassword (begin)');
  {$ENDIF}

  For Counter := 01 to 100 do
    MustDelete[Counter] := '';
  HeaderShown := false;

  for Counter := 01 to TaggingObj^.CurrentTagged do
    begin
      TaggingObj^.GetTagHeader(Pred(Counter), TagInfo);

      if TagInfo.Password <> '' then
        begin
          if NOT HeaderShown then
            begin
              WriteLn;
              WriteLn('`A12:', LangObj^.ralGet(ralVFPassw));
              WriteLn;
              HeaderShown := true;
            end;

          TempName := GetLfnName(TagInfo.Name, TagInfo.RecordNum, TagInfo.AreaNum);;
          Write('`A3: ', TempName);
          Write('`X16:', LangObj^.RalGet(RalPassword));

          TempStr := '';
          GetString(TempStr, 15, [#32..#254], False, True, False, False);

          TaggingObj^.MakeupName(TempStr);
          TaggingObj^.MakeUpName(TagInfo.Password);

          Write('`X16:`A3:`E:');

          if TempStr = TagInfo.Password then
            begin
              WriteLn(LangObj^.ralGet(ralAccepted));

              Taginfo.Password := '';                  { Password validated }
              TaggingObj^.SetTagHeader(Pred(Counter), Taginfo);
            end { Correct }
          else
            begin
              WriteLn(LangObj^.ralGet(ralIncorrect));
              RaLog('!', 'Incorrect password "'+TempStr+'" on file '+TempName);

              { This cannot be done in once, because this would make EleBBS }
              { skip one row, because the order of the files is changed!!   }
              MustDelete[Counter] := TagInfo.Name;
            end; { Incorect }

        end; { if file has password }
    end; { for }

  For Counter := 01 to 100 do
   if MustDelete[Counter] <> '' then
     begin
      TaggingObj^.DeleteFileFromTagged(MustDelete[Counter]);
    end; { if }

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logTransfer, 'AskForFilePassword ( end )');
  {$ENDIF}
end; { proc. AskForPassWords }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

{$IFDEF WITH_FULL}
procedure UpdateFDB(TagHdr: TagFileRecord);
{ ** ALSO UPDATE BBS_SENDFILE.PAS ** }
var FileMgr : MgrFileObj;
    PayBack : Longint;
    DT      : DateTime;
    DOW     : Word;
    OldFMode: Longint;

    User_F  : pFileObj;
    UserInf : UsersRecord;
    TempUser: Integer;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logTransfer, 'UpdateFDB (begin)');
  {$ENDIF}

  GetDate(DT.Year, Dt.Month, DT.Day, Dow);
  GetTime(DT.Hour, DT.Min, DT.Sec, Dow);

  Inc(LineCfg^.Exitinfo^.Userinfo.Downloads);
  Inc(LineCfg^.Exitinfo^.Userinfo.DownloadsK, TagHdr.Size DIV 1024);

  if LineCfg^.UsersKbLimit <> UnlimitedValue then
    Inc(LineCfg^.Exitinfo^.Userinfo.TodayK, TagHdr.Size div 1024);
  Dec(LineCfg^.Exitinfo^.Userinfo.Credit, TagHdr.Cost);

  If TagHdr.AreaNum=00 then Exit;

  {------------------------ Update the filebase -------------------------------}
  FileMgr.Init(TagHdr.AreaNum, true);
  FileMgr.Read(TagHdr.RecordNum - 01);

  PackTime(DT, FileMgr.CurHdr^.LastDl);
  Inc(FileMgr.CurHdr^.TimesDL);

  FileMgr.WriteHdr(TagHdr.RecordNum - 01, FileMgr.CurHdr^);


  {---------------------- Give a user payback credit --------------------------}
  if GlobalCfg^.RaConfig^.FilePayBack>00 then
    begin
      PayBack := GlobalCfg^.RaConfig^.FilepayBack;
      if PayBack=255 then PayBack := FileMgr.GetCost;

      TempUser := SearchUser(FileMgr.GetUploader);

      if TempUser >= 00 then
        begin
          RaLog('>', 'Credited uploader ('+FileMgr.GetUploader+') with '+FStr(PayBack)+' credit(s)');

          New(User_F, Init);
          User_F^.Assign(GlobalCfg^.RaConfig^.MsgBasePath + UserBaseName);
          User_F^.FileMode := ReadWriteMode + DenyAll;
          if User_F^.Open(1) then
            begin
              User_F^.Seek(TempUser * SizeOf(UsersRecord));
              User_F^.BlkRead(UserInf, SizeOf(UsersRecord));

              Inc(UserInf.Credit, PayBack);

              User_F^.Seek(TempUser * SizeOf(UsersRecord));
              User_F^.BlkWrite(UserInf, SizeOf(UsersRecord));
            end; { if }

          Dispose(User_F, Done);
        end; { if UserFound }

    end; { FilePayBack }

  FileMgr.Done;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logTransfer, 'UpdateFDB ( end )');
  {$ENDIF}
end; { proc. UpdateFDB }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

{$IFDEF WITH_FULL}
function LocalDownload(var NrDownloads, DownloadsK: LongInt): Boolean;
var TempStr   : String;
    TempName  : String;
    Counter   : Byte;
    FileDowned: Boolean;
    TagInfo   : TagFileRecord;
    FilesInf  : FilesRecord;
    OrigPath  : String;

procedure DeleteDowned;
var Counter: Byte;

function FilesDeleted: Boolean;
var TempCnt: Byte;
begin
  FilesDeleted := false;

  for TempCnt := 01 to TaggingObj^.CurrentTagged do
    begin
      TaggingObj^.GetTagHeader(Pred(TempCnt), TagInfo);
      if Taginfo.xFerTime > 00 then
        begin
          FilesDeleted := true;
          break;
        end;
    end; { for }
end; { func. FilesDeleted }

begin

  While FilesDeleted do
    begin
      for Counter := 01 to TaggingObj^.CurrentTagged do
       begin
         TaggingObj^.GetTagheader(Pred(Counter), TagInfo);

         if TagInfo.xFerTime > 00 then
           begin
             TaggingObj^.DeleteTagNumber(Pred(Counter));
             BREAK;
           end; { if }
       end; { for }
    end; { while}

end; { proc. DelteDowned }

begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'LocalDownload (begin)');
  {$ENDIF}
  {$IFDEF ELEUNIX}
    EXIT;
  {$ENDIF}

  NrDownloads := 00;
  DownloadsK := 00;
  LocalDownload := False;

  {$IFNDEF ELEUNIX}
    If (LineCfg^.Exitinfo^.Baud=00) then
       begin
          WriteLn;
          WriteLn('`A15:', LangObj^.ralGet(ralLocDown));
          WriteLn;
          Write('`A10:', LangObj^.ralGet(ralAskMvdir));

          TempStr := '';
          GetString(TempStr, 70, [#32..#254], False, False, False, False);
          If Trim(TempStr)='' then EXIT;
          Writeln;
       end; { If NOT MovedDir }
  {$ENDIF}

  Counter := 01;
  CopyFiles_FromCDROm;

  for Counter := 01 to TaggingObj^.CurrentTagged do
   begin
     TaggingObj^.GetTagHeader(Pred(Counter), TagInfo);
     TagInfo.xFerTime := 00;
     TaggingObj^.SetTagHeader(Pred(Counter), TagInfo);

     if TagInfo.Name <> '' then
       begin
          TempName := GetLfnName(TagInfo.Name, TagInfo.RecordNum, TagInfo.AreaNum);;
          Write('`A10:', TempName);
          TempStr := ForceBack(TempStr);
          FileDowned := false;

          OrigPath := ForceBack(LineCfg^.TagPaths^.Get(Counter));
          if OrigPath = '' then
            begin
              GetFilesRecord(FilesInf, Taginfo.AreaNum, True);

              OrigPath := FilesInf.FilePath;
            end; { if }

          With TagInfo do
           if (NOT FileCopy(OrigPath + TempName, TempStr + TempName, true, false)) then
             WriteLn('`X1:`A12:', LangObj^.ralGet(ralErrMove), ' `A15:', TempName)
              else FileDowned := TRUE;

          if FileDowned then
            begin
              Inc(NrDownloads);
              Inc(DownloadsK);
              UpdateFDB(TagInfo);

              RaLog('>', 'Download [Local]: '+TempName);

              TagInfo.xFerTime := 01;
              TaggingObj^.SetTagHeader(Pred(Counter), TagInfo);
              WriteLn;
            end; { succeeded }
       end; { if }
   end; { for Counter }

  DeleteCdromFiles;
  DeleteDowned;
  LocalDownload := True;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'LocalDownload (begin)');
  {$ENDIF}
end; { proc. LocalDownload }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

{$IFDEF WITH_FULL}
procedure GetFileDescriptions;
var Desc_F     : Text;
    Counter    : Byte;
    TagInfo    : TagFileRecord;

    AreaObj    : FdbFileObj;
    FilesInf   : FilesRecord;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'GetFileDescriptions (begin)');
  {$ENDIF}

  Writeln;
  Writeln('`A10:', LangObj^.ralGet(ralFetchDes));

  {$IFDEF WIN32}
   If SysUtils.FileExists(GlobalCfg^.RaConfig^.FileBasePath+'FILE_ID.'+FStr(LineCfg^.RaNodeNr)) then
       Sysutils.DeleteFile(GlobalCfg^.RaConfig^.FileBasePath+'FILE_ID.'+FStr(LineCfg^.RaNodeNr));
  {$ENDIF}

  Assign(Desc_F, GlobalCfg^.RaConfig^.FileBasePath + 'FILE_ID.' + FStr(LineCfg^.RaNodeNr));
  {$i-} System.ReWrite(Desc_F); {$I+}
  If IOResult>00 then EXIT;

  for Counter := 01 to TaggingObj^.CurrentTagged do
   begin
     TaggingObj^.GetTagHeader(Pred(Counter), TagInfo);

     if GetFilesRecord(FilesInf, TagInfo.AreaNum, True) then
       begin
         AreaObj.Init(FilesInf.AreaNum, false);
         if AreaObj.GetError=00 then
           begin
             AreaObj.Read(TagInfo.RecordNum - 01);   { Read the description }
             AreaObj.ReadDescription(AreaObj.TxtF, AreaObj.GetLongDescPtr);

             WriteLn(Desc_F, MakeLen(AreaObj.GetFileName, 15, Space, False, false),
                             AreaObj.GetDescLine(78 - 15));

             While NOT AreaObj.EndOfDesc do
              begin
                WriteLn(Desc_F, Dup(#32, 15),
                                AreaObj.GetDescLine(78 - 15));
              end; { while }
           end; { if no error }

         AreaObj.Done;
       end; { if  }

   end; { for }

  {$i-} Close(Desc_F); {$I+}
  if IOResult>00 then;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'GetFileDescriptions ( end )');
  {$ENDIF}
end; { proc. GetFileDescriptions }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

{$IFDEF WITH_FULL}
procedure CopyFiles_FromCDROm;
var CDRomMsg   : Boolean;
    Counter    : Word;
    FilesInf   : FilesRecord;
    EleFilesInf: EleFilesRecord;
    TagInfo    : TagFileRecord;
    TempStr    : String;
    TempName   : String;
    {$IFDEF TCPIP}
      ftpObj     : pFtpInterfaceObj;
    {$ENDIF}
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logTransfer, 'CopyFiles_FromCDROM (begin)');
  {$ENDIF}

 CDRomMsg := true;                    { Display the cd-rom copying message }

 for Counter := 01 to TaggingObj^.CurrentTagged do
   begin
     TaggingObj^.GetTagHeader(Pred(Counter), TagInfo);

     if TagInfo.CDRom then
      if GlobalCfg^.RaConfig^.TempCDfilepath<>'' then
        begin
          GetFilesRecord(FilesInf, TagInfo.AreaNum, True);
          GetEleFilesRecord(EleFilesInf, TagInfo.AreaNum, True);

          if LineCfg^.TagPaths^.Get(Counter) <> '' then
            FilesInf.FilePath := LineCfg^.TagPaths^.Get(Counter);
          if CDROMmsg then
            begin
              WriteLn('`A12:', LangObj^.ralGet(ralTFCDRom));
              InputObj^.UpdateScrn;
              OutBlockObj^.DumpBlock;
            end; { if }

          CDROMmsg := false;
          TempName := GetLfnName(TagInfo.Name, TagInfo.RecordNum, TagInfo.AreaNum);;

          if IsFtpUrl(EleFilesInf.ftpPath) then
            begin
                TempStr := EleFilesInf.ftpPath;
                if TempStr[Length(TempStr)] = '\' then
                  TempStr[Length(TempStr)] := '/';


                {$IFDEF TCPIP}
                  New(ftpObj, Init);
                  if ftpObj^.StartUpFtp(TempStr + TempName,
                                        EleFilesInf.ftpLoginName,
                                        EleFilesInf.ftpPassword) then
                    begin
                      if FtpObj^.GetFtpFile(TempStr + TempName, GlobalCfg^.RaConfig^.TempCdfilePath) then
                         LineCfg^.TagPaths^.Put(Counter, ForceBack(GlobalCfg^.RaConfig^.TempCdFilePath));
                      FtpObj^.DoneFtp;
                    end; { if StartUpFtp }
                  Dispose(ftpObj, Done);
                {$ELSE}
                  RaLog('!', 'FTP not implemented in the DOS version');
                {$ENDIF}
            end else
                     begin
                       if NOT FileCopy(FilesInf.FilePath + TempName,
                                       GlobalCfg^.RaConfig^.TempCdFilepath + TempName, True, True) then
                          begin
                            RaLog('!', 'Unable to copy file to temporary cd-rom filepath!');
                            RaLog('!', 'Source: '+FilesInf.FilePath + TempName+', Dest: ' +
                                      GlobalCfg^.RaConfig^.TempCdFilepath + TempName);
                          end
                            else LineCfg^.TagPaths^.Put(Counter, ForceBack(GlobalCfg^.RaConfig^.TempCdFilePath));
                     end; { FTP site }
        end; { CDRom }
   end; { For Counter }

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logTransfer, 'CopyFiles_FromCDROM ( end )');
  {$ENDIF}
end; { proc. CopyFiles_FromCDROM }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

{$IFDEF WITH_FULL}
procedure DoTransferExternal(Description, Download: Boolean; ProtInf: ProtocolRecord;
                                    var NrDownloads, DownloadsK: LongInt; UlPath: String;AreaNum:Use32.Word;
                                    Fattaching: Boolean);

procedure CreateCtlFile(AddDescription: Boolean);
var CTL_F   : Text;
    TempStr : String;
    Counter : Byte;
    FilesInf: FilesRecord;
    TagInfo : TagFileRecord;
    TempName: String;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'CreateCtlFile (begin)');
  {$ENDIF}

  if ProtInf.CtlFileName='' then EXIT;

  {$IFDEF WIN3}
   If SysUtils.FileExists(ProtInf.CtlFileName) then
     SysUtils.DeleteFile(ProtInf.CtlFileName);
  {$ENDIF}

  Assign(Ctl_F, ProtInf.CtlFileName);
  {$i-} System.Rewrite(Ctl_F); {$I+}
  if IOResult>00 then EXIT;

  if ProtInf.OpusTypeCtlFile then
    begin
      {$i-}
        Writeln(Ctl_F, 'Port ', LineCfg^.Modem^.Comport);
        Writeln(Ctl_F, 'Baud ', FixBaud(LineCfg^.Exitinfo^.Baud));
        Writeln(Ctl_F, 'Log ', ProtInf.LogFileName);
        Writeln(Ctl_F, 'Time ', LineCfg^.Exitinfo^.TimeLimit);
      {$i+}
      If IOResult>00 then ;
    end; { If OpusType LogFile }

  if Download then
    for Counter := 01 to TaggingObj^.CurrentTagged do
      begin
        TaggingObj^.GetTagHeader(Pred(Counter), TagInfo);

        if GetFilesRecord(FilesInf, TagInfo.AreaNum, True) then
          begin
            if LineCfg^.TagPaths^.Get(Counter) <> '' then
              FilesInf.FilePath := LineCfg^.TagPaths^.Get(Counter);
            TempName := GetLfnName(TagInfo.Name, TagInfo.RecordNum, TagInfo.AreaNum);;

            if FileExist(FilesInf.FilePath + TempName) then
              begin
                TempStr := ProtInf.DnCtlString;
                Replace('@', FilesInf.FilePath + TempName, TempStr);

                WriteLn(Ctl_F, TempStr);
              end; { FileExist }
          end; { if }
      end; { for }

  if NOT Download then
   begin
     TempStr := ProtInf.UpCtlString;
     Replace('@', UlPath, TempStr);

     WriteLn(Ctl_F, TempStr);
   end; { Uploading }

  if AddDescription then
    begin
      TempStr := ProtInf.DnCtlString;
      Replace('@', GlobalCfg^.RaConfig^.FileBasePath + 'FILE_ID.' + FStr(LineCfg^.RaNodeNr), TempStr);

      WriteLn(Ctl_F, TempStr);
    end; { If adding description }

  {$i-} Close(Ctl_F); {$I+}
  If IOResult>00 then;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'CreateCtlFile ( end )');
  {$ENDIF}
end; { proc. CreateCtlFile }


procedure EraseLogFile;
var Temp: Text;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'EraseLogFile (begin)');
  {$ENDIF}

  Assign(Temp, ProtInf.LogFileName);
  {$i-} Erase(Temp); {$I+}
  If IOResult>00 then ;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'EraseLogFile ( end )');
  {$ENDIF}
end; { proc. EraseLogFile }


procedure EraseCTLFile;
var Temp: Text;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'EraseCtlFile (begin)');
  {$ENDIF}

  Assign(Temp, ProtInf.CTLFileName);
  {$i-} Erase(Temp); {$I+}
  If IOResult>00 then ;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'EraseCtlFile ( end )');
  {$ENDIF}
end; { proc. EraseCTLfile }


procedure ProcessTransferLog(Name: String);
{IORAM 20070422}
function TakeQuotesOff(Str: String):String;
begin
	TakeQuotesOff := Str;
	
	if Copy(Str, 1, 1) = '"' then 
	  begin
			TakeQuotesOff := Copy(Str, 2, Length(Str)-1)
		end 
			else if Copy(Str, Length(Str), 1) = '"' then
						begin
				  		TakeQuotesOff := Copy(Str, 1, Length(Str)-1);
		  			end; { else }
end; { func. TakeQuotesOff }

{/IORAM}
var TempLog   : Text;
    TagInfo   : TagFileRecord;
    TempStr   : String;
    FileName  : String;
    FilePath  : String;
    Counter   : Byte;
    FileSize  : Longint;
    FilesInf  : FilesRecord;
    TagCounter: Byte;
    TempName  : String;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'ProcessTransferLog (begin)');
  {$ENDIF}

  if Trim(Name) = '' then EXIT;

  Assign(TempLog, Name);
  {$i-} System.Reset(TempLog); {$I+}
  If IOResult>00 then
   begin
     LocalScreenLn(SystemMsgPrefix + 'Unable to open protocol logfile, userinfo is not being updated.');
     RaLog('!', 'Unable to open logfile, protocol: "'+ProtInf.Name+'"');
     EXIT;
   end; { if IOResult }

  While NOT Eof(TempLog) do
   begin
     {$i-} ReadLn(TempLog, TempStr); {$i+}
     if IOResult>00 then ;

     if Pos(ProtInf.UpLogKeyWord, TempStr)>00 then
       begin
         if AreaNum = 00 then
           begin
             AreaNum := LineCfg^.Exitinfo^.Userinfo.FileArea;
             GetFilesRecord(FilesInf, AreaNum, False);

             if FilesInf.UploadArea>00 then AreaNum := FilesInf.UploadArea;
           end; { Areanum = 00 }

         FileName := SUpcase(GetFileName(Pos(ProtInf.UpLogKeyWord, TempStr), TempStr, ProtInf.XFerNameWordNum));
         FileSize := GetFileSize(ForceBack(UlPath) + JustName(FileName), 1);

         if FileName <> '' then
          begin
            Inc(LineCfg^.TotalUploads);
            LineCfg^.UploadInfo^[LineCfg^.TotalUploads].FName := JustName(FileName);
            LineCfg^.UploadInfo^[LineCfg^.TotalUploads].Size := GetFileSize(FileName, 01);

            {$IFDEF WITH_DEBUG}
              DebugObj.DebugLog(logTransfer, 'ProcessTransferLog (Filename = '+FileName+')');
              DebugObj.DebugLog(logTransfer, 'ProcessTransferLog (new Filename = '+FilesInf.FilePath + FileName+')');
              DebugObj.DebugLog(logTransfer, 'ProcessTransferLog (UlPath = '+UlPath+')');
            {$ENDIF}

            { Move the file to it's new location }
            RenameFile(FileName, ForceBack(UlPath) + JustName(FileName));
          end; { if }
       end; { if upload }

     DeleteCDROMfiles;

     if Pos(ProtInf.DnLogKeyWord, TempStr)>00 then
       begin
         FileName := JustName(GetFileName(Pos(ProtInf.DnLogKeyWord, TempStr), TempStr, ProtInf.XFerNameWordNum));
         {IORAM 20070422}
         FileName := TakeQuotesOff(FileName);
         {/IORAM}
         TaggingObj^.MakeUpName(FileName);

         TagCounter := 01;
         if FileName <> '' then
          While TagCounter < Succ(TaggingObj^.CurrentTagged) do
            begin
              TaggingObj^.GetTagHeader(Pred(TagCounter), TagInfo);
              TempName := GetLfnName(TagInfo.Name, TagInfo.RecordNum, TagInfo.AreaNum);
              FilePath := JustPath(GetFileName(Pos(ProtInf.DnLogKeyWord, TempStr), TempStr, ProtInf.XFerNameWordNum));
              {IORAM 20070422}
              FilePath := TakeQuotesOff(FilePath);
              {/IORAM}
              TaggingObj^.MakeUpName(TempName);

              if (TagInfo.Name <> '') AND (FileName <> '') then
               if (Taginfo.Name = FileName) OR (TempName = FileName) then
                 begin
                   Inc(Nrdownloads, 01);
                   Inc(DownloadsK, TagInfo.Size div 1024);

                   RaLog('>', 'Download ['+ProtInf.Name+']: '+SUpcase(JustPath(FilePath) + JustName(TempName)));

                   UpdateFDB(TagInfo);
                   TaggingObj^.DeleteFileFromTagged(TagInfo.Name);
                   TaggingObj^.DeleteFileFromTagged(TempName);

                   Dec(TagCounter); { Correct the counter for messing up the filebase }
                end; { if match }

              Inc(TagCounter);
            end; { for }

       end; { Download }
   end; { Not EOF_Log }

  {$i-} Close(TempLog); {$I+}
  If IOResult>00 then;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'ProcessTransferLog (begin)');
  {$ENDIF}
end; { proc. ProcessTransferLog }


var ExitTemp : Use32.Word;
    ExecStr  : String;
    Success  : Boolean;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logTransfer, 'DoTransferExternal (begin)');
  {$ENDIF}

  CreateCtlFile(Description);
  EraseLogFile;

  If (ProtInf.Attribute=0) then EXIT;
  If (ProtInf.Attribute=2) AND (NOT LineCfg^.Exitinfo^.ErrorFreeConnect) then EXIT;

  LineCfg^.LocalOnly := True;
  {$IFNDEF WINGUI}
    ClrScr;
  {$ELSE}
    Form1.ColorConsole1.ClrScr;
  {$ENDIF}

  LocalScreenLn(SystemMsgPrefix + 'Executing external protocol: "'+ProtInf.Name+'"');
  LocalScreenLn('');

  LineCfg^.CheckInactivity := false;       { Disable keyboard inactivity timeout checking }

  If Download then ExecStr := ProtInf.DnCmdString
   else ExecStr := ProtInf.UpCmdString;

  If Download then Replace('#', '', ExecStr)
      else Replace('#', UlPath, ExecStr);

  RAExec(ExecStr, False, False, False, False, ExitTemp, Success);

  If NOT Download then contrObj^.SetStartTime;

  LineCfg^.CheckInactivity := True;
  contrObj^.SetTimeOut;

  InputObj^.DorCleanKeys;

  LocalScreenLn('');

  {$IFNDEF WINGUI}
    ClrScr;
    TextAttr := 3;
  {$ELSE}
    Form1.ColorConsole1.ClrScr;
    Form1.ColorConsole1.TextAttr := 3;
  {$ENDIF}

  LocalScreenLn(SystemMsgPrefix + 'Processing external log..');
  LineCfg^.LocalOnly := FALSE;

  ProcessTransferLog(ProtInf.LogFileName);
  EraseLogFile;
  EraseCTLFile;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logTransfer, 'DoTransferExternal ( end )');
  {$ENDIF}
end; { proc. DoTransferExternal }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

{$IFDEF WITH_FULL}
procedure CheckForVirusScan(const FilesInf: FilesRecord);

procedure FailedScanAction(var Name: String; const Areanum: Word; const FilesInf: FilesRecord);
var TestVar   : Integer;
    HdrInfs   : Array[1..2] of FilesHdrRecord;
    AreaObj   : FdbFileObj;
    TxtArray  : ^TxtRec;
    MoveToArea: Word;
begin
  Name := Trim(SUpCase(Name));

  {  FailedScanArea      : SmallWord; }
  {    FailedScanAction    : Byte; Bit 0:Mark deleted, 1:Mark unlisted, 2:Mark notavail }

  TestVar := SearchNameInArea(AreaNum, Name, true);
  if TestVar < 0 then EXIT;

  AreaObj.Init(AreaNum, false);
  AreaObj.Read(TestVar - 01);
  HdrInfs[1] := AreaObj.CurHdr^;
  AreaObj.Done;

  if SUpCase(Trim(HdrInfs[1].Name)) <> Name then EXIT;

  if ReadBit(GlobalCfg^.RaConfig^.FailedScanAction, 0) then { Delete the file }
   begin
     EraseFile(FilesInf.FilePath + Name);
   end; { if }

  if ReadBit(GlobalCfg^.RaConfig^.FailedScanAction, 1) then { Mark unlisted }
   begin
     SetBit(HdrInfs[1].Attrib, 1);
     SetBit(HdrInfs[1].Attrib, 5);
   end; { if }

  if ReadBit(GlobalCfg^.RaConfig^.FailedScanAction, 2) then { Mark not available }
   begin
     SetBit(HdrInfs[1].Attrib, 3);
   end; { if }

  MoveToArea := FilesInf.AreaNum;
  if GlobalCfg^.RaConfig^.FailedScanArea > 0 then
    MoveToArea := GlobalCfg^.RaConfig^.FailedScanArea;

  InsertInHdr(MoveToArea, 0, 1, HdrInfs);

  if MoveToArea <> FilesInf.AreaNum then
    begin
       if MemMan.AllocMem(TxtArray, SizeOf(TxtRec), 'TxtRec', 'FailedScanAction') then
         begin
           AreaObj.Init(MoveToArea, True);
           AreaObj.Read(0);
           AreaObj.GetDescBlock(TxtArray^);
           AreaObj.Done;

           ReleaseMem(TxtArray, SizeOf(TxtRec));
         end; { if }

    end; { if }
end; { proc. FailedScanAction }

var DoScan  : Boolean;
    DosExit : Word;
    Success : Boolean;
    ScanCmd : String;
    Counter : Byte;
    DoExit  : Boolean;
begin
  DoExit := true;
  for Counter := 01 to LineCfg^.TotalUploads do
    if LineCfg^.UploadInfo^[Counter].FName <> '' then
      begin
        DoExit := false;
        BREAK;
      end;
  if DoExit then EXIT;

  {--------------------------------- Virus Scan -------------------------------}
  if ReadBit(FilesInf.Attrib, 7) then
   begin
     Case Byte(GlobalCfg^.RaConfig^.ScanNow) of
        Byte(Yes) : DoScan := true;
        Byte(Ask) : begin
                       DisplayHotFile('upldscan', []);
                       DoScan := InputObj^.ralStrYesNoAsk(ralScanUplN);
                    end
         else DoScan := false;
      end; { case }
   end { if }
     else DoScan := false;

  if DoScan then
   begin
     WriteLn;
     WriteLn('`A14:', langObj^.ralGet(ralScanUpl));

     for Counter := 01 to LineCfg^.TotalUploads do
      if LineCfg^.UploadInfo^[Counter].FName <> '' then
       begin
         Write(MakeLen(LineCfg^.UploadInfo^[Counter].FName, 12, Space, False, false), ' .. ');

         ScanCmd := GlobalCfg^.RaConfig^.ScanCmd;
         Replace('@', FilesInf.FilePath + LineCfg^.UploadInfo^[counter].FName, ScanCmd);

         RaExec(ScanCmd, False, False, False, False, DosExit, Success);

         {----------------------- Failed scan -------------------------}
         if DosExit <> 00 then
           begin
             WriteLn(langObj^.ralGet(ralFailViru));

             RaLog('!', FilesInf.FilePath + LineCfg^.UploadInfo^[counter].FName + ' failed virus check!');

             FailedScanAction(LineCfg^.UploadInfo^[counter].FName, FilesInf.AreaNum, FilesInf);
           end { failed }
             else WriteLn(langObj^.ralGet(ralPassed));
       end; { if }
   end; { if }
end; { proc. CheckForVirusscan }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure ProcessUploads(AreaNum: Word; var Uploads, UploadsK: Longint;
                         ProtName: String; FAttaching, UpMessage: Boolean);
var Counter  : Longint;
    FilesInf : FilesRecord;
    FirstTime: Boolean;
    Success  : Boolean;
begin
{$IFDEF WITH_FULL}
  Firsttime := false;

  for Counter := 01 to LineCfg^.TotalUploads do
   if LineCfg^.UploadInfo^[Counter].FName <> '' then
     begin
       GetFilesRecord(FilesInf, AreaNum, True);

       if AreaNum=00 then
         begin
           AreaNum := LineCfg^.Exitinfo^.Userinfo.FileArea;
           GetFilesRecord(FilesInf, AreaNum, False);

           if FilesInf.UploadArea > 00 then
             AreaNum := FilesInf.UploadArea;
         end; { Areanum = 00 }

       Success := true;
       if (NOT Fattaching) AND (NOT UpMessage) then
         Success := AddToFdb(LineCfg^.UploadInfo^[Counter].FName, LineCfg^.UploadInfo^[Counter].Size, AreaNum, FirstTime);

{!!       if NOT Success then }
{!!        EraseFile(FilesInf.FilePath + UploadInfo^[Counter].FName); }

       if Success then
         begin
           Inc(Uploads, 01);
           Inc(UploadsK, LineCfg^.UploadInfo^[Counter].Size div 1024);

           RaLog('>', 'Upload ['+ProtName+'] '+FilesInf.FilePath + Justname(LineCfg^.UploadInfo^[Counter].FName) +
                  ', ' + FStr(LineCfg^.UploadInfo^[Counter].CPS) + ' CPS');

           RaLog('>', '('+FStr(LineCfg^.UploadInfo^[Counter].Size)+ ' bytes, '+
                        FStr(LineCfg^.UploadInfo^[Counter].Size div 1024)+'k)');
         end; { if }
     end; { if }

  {------------------- Run the virus scanner -----------------------------}
  if NOT FAttaching then
    CheckForVirusScan(FilesInf);
{$ENDIF}
end; { proc. ProcessUploads }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

{$IFDEF WITH_FULL}
procedure DoTransferInternal(Description, Download: Boolean; ProtInf: ProtocolRecord;
                                    var NrDownloads, DownloadsK: LongInt; UlPath: String;AreaNum:Use32.Word;
                                    MessageUpping, FAttaching: Boolean);
var Counter : Longint;
    TempStr : String;
    Filesinf: FilesRecord;
    TagInfo : TagFileRecord;
    LfnName : String;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logTransfer, 'DoTransferInternal (BEGIN)');
  {$ENDIF}

  if NOT trans_InitializeProtocol(ProtInf.ActiveKey) then EXIT;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logTransfer, 'DoTransferInternal (After InitialzieProtocol)');
  {$ENDIF}

  if NOT trans_InitializeHooks(Download, MessageUpping) then
        begin
          trans_DoneProtocol;
          RaLog('!', 'Unable to initialize protocol hooks, transfer aborted');
          EXIT;
        end; { if }


  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logTransfer, 'DoTransferInternal (After InitializeHooks)');
  {$ENDIF}

  {---------------------------- Create the filelist ---------------------------}
  {------------------------------ (download only) -----------------------------}
  if Download then
    begin
      {$IFNDEF NEWPROT}
      ProtObj^.MakeFileList(FLP, TransListSize);
      ProtObj^.SetFileList(FLP);
      {$ENDIF}

      for Counter := 01 to TaggingObj^.CurrentTagged do
        begin
          TaggingObj^.GetTagHeader(Pred(Counter), TagInfo);

          if GetFilesRecord(FilesInf, TagInfo.AreaNum, True) then
            begin
              if LineCfg^.TagPaths^.Get(Counter) <> '' then
                   FilesInf.FilePath := LineCfg^.TagPaths^.Get(Counter);

              TempStr := GetLfnName(TagInfo.Name, TagInfo.RecordNum, TagInfo.AreaNum);

              if FileExist(FilesInf.FilePath + TempStr) then
                begin
                  {$IFNDEF NEWPROT}
                    ProtObj^.AddFileToList(FLP, FilesInf.FilePath + TempStr);
                  {$ENDIF}

                  {$IFDEF WITH_DEBUG}
                    DebugObj.DebugLog(logAsync, 'Adding: '+FilesInf.FilePath + TempStr);
                  {$ENDIF}

                  Inc(TotalBytesTransferLeft, TagInfo.Size);
                  Inc(TotalTransferBytes, TagInfo.Size);
                end; { if }
            end; { if }

          TagInfo.FoundFirst := false;
          TaggingObj^.SetTagHeader(Pred(Counter), TagInfo);
        end; { for counter }

      if Description then
        begin
          {$IFNDEF NEWPROT}
            ProtObj^.AddFileToList(FLP, GlobalCfg^.RaConfig^.FileBasePath + 'FILE_ID.' + FStr(LineCfg^.RaNodeNr));
          {$ENDIF}
        end; { if }
    end; { if }

  {---------------------------- Create the filelist ---------------------------}
  {------------------------------- (upload only) ------------------------------}
  if NOT Download then
    begin
      {$IFNDEF NEWPROT}
      if SUpCase(ForceBack(UlPath)) <> SUpCase(GlobalCfg^.RaConfig^.SysPath) then
        ProtObj^.SetDestinationDirectory(UlPath)
         else begin
                RaLog('!', 'Upload directory is the same as the system directory.');
                RaLog('!', 'This can be a major security hazard, upload aborted!');
                UlPath := 'nul';
                trans_DoneProtocol;

                EXIT;
              end; { if }
      {$ENDIF}
    end; { user is uploading }

  {------------------------ Initialize the statistics -------------------------}
  NrDownloads := 00;
  DownloadsK := 00;


  {---------------- Disable the inactivity checker and make sure --------------}
  {--------------------- the outbound buffer is empty -------------------------}
  LineCfg^.CheckInactivity := false;
  OutBlockObj^.DumpBlock;
  InputObj^.UpdateScrn;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logTransfer, 'DoTransferInternal (now really begin)');
  {$ENDIF}

  {----------------------- Actually start the transfer ------------------------}
  {$IFDEF ELEUNIX}
    {$IFNDEF NOLINLOCAL}
      ComObj^.BlockAll := false;
    {$ENDIF}
  {$ENDIF}
  trans_StartTransfer(Download);
  {$IFDEF ELEUNIX}
    {$IFNDEF NOLINLOCAL}
      ComObj^.BlockAll := true;
    {$ENDIF}
  {$ENDIF}

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logTransfer, 'DoTransferInternal (finished the real part)');
  {$ENDIF}

  {-------------- We're back to the living, reset inactivity timer ------------}
  contrObj^.SetTimeOut;
  LineCfg^.CheckInactivity := true;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'Download routine list (begin)');
    DebugObj.DebugLog(logTransfer, 'Download - Download = '+Bool2Str(Download));
    DebugObj.DebugLog(logTransfer, 'Download - CurrentTagged = '+FStr(TaggingObj^.CurrentTagged));
  {$ENDIF}

  {---------------- Check for all the files that are downloaded ---------------}
  {------------------------------ (download only) -----------------------------}
  DeleteCDROMfiles;                              { Delete all the CD-Temp path }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logtransfer, 'Download - Deleting: ' + FStr(Succ(TaggingObj^.CurrentTagged)));
    DebugObj.DebugLog(logTransfer, 'Download - DoDnload: ' + Bool2Str(Download));
  {$ENDIF}


  Counter := 01;
  if Download then
    While Counter < Succ(TaggingObj^.CurrentTagged) do
      begin
        TaggingObj^.GetTagHeader(Pred(Counter), TagInfo);
        LfnName := GetLfnName(TagInfo.Name, TagInfo.RecordNum, TagInfo.AreaNum);
        
        {$IFDEF WITH_DEBUG}
          DebugObj.DebugLog(logTransfer, 'Download - Deleting from list: ' + TagInfo.Name + ' / ' + LfnName);
          DebugObj.DebugLog(logTransfer, 'Download - Deleting from list: ' + Bool2Str(TagInfo.FoundFirst));
        {$ENDIF}

		if (TagInfo.xFerTime = 0) then 
		  TagInfo.FoundFirst := false;

        if TagInfo.FoundFirst then
          begin
            Inc(NrDownloads, 01);
            Inc(DownloadsK, Taginfo.Size div 1024);

            if GetFilesRecord(FilesInf, TagInfo.AreaNum, True) then
              begin
                if LineCfg^.TagPaths^.Get(Counter) <> '' then
                   FilesInf.FilePath := LineCfg^.TagPaths^.Get(Counter);

                RaLog('>', 'Download ['+ProtInf.Name+']: ' + FilesInf.FilePath + LfnName + ', ' + FStr(TagInfo.XFerTime) +
                     ' CPS');
              end; { if }

            UpdateFDB(TagInfo);
            TaggingObj^.DeleteFileFromTagged(LfnName);

            Dec(Counter); { Correct the counter for messing up the filebase }
          end; { if }

        Inc(Counter);
      end; { while }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'Download routine list ( end )');
  {$ENDIF}

  {--------- Clear the fossil input buffer and dispose the downloadlist -------}
  InputObj^.DorCleanKeys;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'Disposing FileListsize');
  {$ENDIF}

  {$IFNDEF NEWPROT}
  if Download then
    ProtObj^.DisposeFileList(FLP, TransListSize);
  {$ENDIF}

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'trans_DoneProtocol (1)');
  {$ENDIF}

  trans_DoneProtocol;
  StatusDisplay(99, false);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'trans_DoneProtocol (2)');
  {$ENDIF}


  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logTransfer, 'DoTransferInternal ( END )');
  {$ENDIF}
end; { proc. DoTransferInternal }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure DeleteCDROMfiles;
var Counter  : Longint;
    TagInfo  : TagFileRecord;
    TempName : String;
begin
  for Counter := 01 to TaggingObj^.CurrentTagged do
   begin
     TaggingObj^.GetTagHeader(Pred(Counter), TagInfo);

     if TagInfo.CDROM then
      if LineCfg^.TagPaths^.Get(Counter) <> '' then
        begin
          TempName := GetLfnName(TagInfo.Name, TagInfo.RecordNum, TagInfo.AreaNum);;
          EraseFile(ForceBack(LineCfg^.TagPaths^.Get(Counter)) + TempName);
        end; { if }

   end; { for }

end; { proc. DeleteCDromFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

{$IFDEF WITH_FULL}
function DoNormalDownload(var NrDownloads, DownloadsK: LongInt): Boolean;
var AddDescription: Boolean;
    TempPos       : Byte;
    ProtInf       : ProtocolRecord;
    CH            : Char;
    LogOff        : Boolean;
    TotalK,
    TotalFiles,
    TotalMinutes  : Longint;
    Counter       : Byte;
    EndTime       : Longint;
    InternKeys    : CfgRec.CharSet;
    TagInfo       : TagFileRecord;
    FreeK,
    FreeFiles     : Longint;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'DoNormalDownload');
  {$ENDIF}

  Write('`A10:');
  DoNormalDownload := False;
  AddDescription := false;

  Case Byte(GlobalCfg^.RaConfig^.DlDesc) of
 Byte(Yes) : begin;
               GetFileDescriptions;
               AddDescription := True;
             end; { Yes }
 Byte(Ask) : If InputObj^.ralStrYesNoAsk(ralDlFd) then
                    begin;
                      GetFileDescriptions;
                      AddDescription := True;
                    end; { AddDescription }
 Byte(No)  : AddDescription := False;
  end; { GlobalCfg^.RaConfig^.DlDesc }

  Repeat;
    GetProtocolRecord(LineCfg^.Exitinfo^.Userinfo.DefaultProtocol, ProtInf);

    While (ProtInf.Name='') do
     begin
       SelectProtocol;
       GetProtocolRecord(UpCase(LineCfg^.Exitinfo^.Userinfo.DefaultProtocol), ProtInf);
     end; { While }

    WriteLn('`A10:', LangObj^.ralGet(ralDefProt), ProtInf.Name);
    Write('`A11:', LangObj^.ralGetStr(ralStrtAbr));

    repeat;
      CH := UpCase(InputObj^.ReadKey);

      TempPos := Pos(CH, SUpcase(LangObj^.ralGetKeys(ralStrtAbr)));
      If CH=#13 then TempPos := 01;
    Until TempPos in [01, 02, 03, 04];

    Case TempPos of
        01 : LogOff := False;
        02 : LogOff := True;
        03 : SelectProtocol;
        04 : EXIT;
    end; { case }

  Until TempPos in [01, 02];

  TotalK      := 00;
  TotalFiles  := 00;
  TotalMinutes:= 00;
  FreeK       := 00;
  FreeFiles   := 00;

  for Counter := 01 to TaggingObj^.CurrentTagged do
    begin
      TaggingObj^.GetTagHeader(Pred(Counter), TagInfo);

      If NOT ReadBit(TagInfo.Attrib, 2) then             { NOT Free }
        begin
          Inc(TotalK, TagInfo.Size DIV 1024);
          Inc(TotalFiles, 1);
        end { Not ReadBit }
          else begin
                 Inc(FreeK, TagInfo.Size DIV 1024);
                 Inc(FreeFiles, 1);
               end; { if }

      If NOT ReadBit(TagInfo.Attrib, 6) then               { NoTime }
        Inc(TotalMinutes, ZModem_Time(LineCfg^.Exitinfo^.Baud, TagInfo.Size));
    end; { for counter }

  WriteLn;
  WriteLn;
  CopyFiles_FromCDROm;

  Write('`A14:`S:');
  Writeln('`A14:', LangObj^.ralGet(ralXferFiles), TotalFiles + FreeFiles);
  Writeln('`A03:', LangObj^.ralGet(ralXferSize), TotalK + FreeK,'k');
  Writeln('`A03:', LangObj^.ralGet(ralXferTime), ZModem_Time(LineCfg^.Exitinfo^.Baud, TotalK * 1024), #32,
                   LangObj^.ralGet(ralMinsAt), #32, FixBaud(LineCfg^.Exitinfo^.Baud), #32, LangObj^.ralGet(ralBaud));
  Writeln('`A03:', LangObj^.ralGet(ralProtocol1), ProtInf.Name);
  Writeln('`A15:', LangObj^.ralGet(ralStartRecv));

  LineCfg^.TransferMode := Transmit;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'Starting DoTransferInternal');
  {$ENDIF}

  GetInternKeys(InternKeys);
  if (NOT (UpCase(ProtInf.ActiveKey) in InternKeys)) then
    DoTransferExternal(AddDescription, True, ProtInf, NrDownloads, DownloadsK,
                       '', 00, False)
     else DoTransferInternal(AddDescription, True, ProtInf, NrDownloads, DownloadsK, '', 00, False, False);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'Returning from DoTransferInternal');
    DebugObj.DebugLog(logTransfer, 'Deleting files');
  {$ENDIF}

  DoNormalDownload := True;
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'Returning from DoNormalDownload');
  {$ENDIF}

  if LogOff then
    begin
      Delay(2000);
      WriteLn;
      WriteLn('`A15:', LangObj^.ralGet(ralAutoLogof));
      WriteLn;
      Write('`A10:');

      Counter := 10;
      repeat
        Write('`X1:', Counter:2, #07);                { Beep and display time }
        Dec(Counter);

        EndTime := CurrentTime + 01;
        repeat
          if InputObj^.KeyPressed then
           begin
             CH := UpCase(InputObj^.ReadKey);

             case CH of
               '!' : TerminateCall(False);
               'S' : BREAK;
             end; { case }
           end; { if }
        until (CH in ['S', '!']) OR (CurrentTime = EndTime);
      until (counter=0) or (CH='S');

      if Ch <> 'S' then TerminateCall(False);
    end; { if }
end; { func. DoNormalDownload }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

{$IFDEF WITH_FULL}
procedure DoDownload(MiscData: String; Global, Specific, AnyFile, AllowEdit, AllFree: Boolean);
var NrDownloads: LongInt;
    DownloadsK : LongInt;

    Uploads,
    UploadsK   : Longint;

    AreaToUse  : Word;
    Temp       : String;
begin
   {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logTransfer, 'DoDownload (begin)');
   {$ENDIF}
   MultiLnObj^.WriteUseron('', uonUpDown);

   If NOT CheckTimeRange(GlobalCfg^.RaConfig^.DownloadTimeStart, GlobalCfg^.RaConfig^.DownloadTimeEnd, true) then
      begin
        WriteLn('`A12:');
        With GlobalCfg^.RaConfig^ do
          Writeln(LangObj^.ralGet(ralDlTime), #32, DownloadTimeStart, ' - ', DownloadTimeEnd);
        InputObj^.PressEnter(False, False);
        MultiLnObj^.WriteUseron('', uonBrowsing);
        EXIT;
      end; { Not In TimeWindow }

   {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logTransfer, 'DoDownload - timerange checked');
   {$ENDIF}

   If LineCfg^.Exitinfo^.Baud>00 then
    If FixBaud(LineCfg^.Exitinfo^.Baud) < GlobalCfg^.RaConfig^.TransferBaud then
       begin
         WriteLn('`A12:');
         Writeln(LangObj^.ralGet(ralXferSlow));
         InputObj^.PressEnter(False, False);
         MultiLnObj^.WriteUseron('', uonBrowsing);
         EXIT;
       end; { Too Slow }

   {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logTransfer, 'DoDownload - let''s clear some paths');
   {$ENDIF}

    LineCfg^.TagPaths^.Clear;
    NrDownloads := 00;
    DownloadsK := 00;
    Uploads := 00;
    UploadsK := 00;

   {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logTransfer, 'DoDownload - paths cleared, individual pathlist?');
   {$ENDIF}

    If Specific then
      begin
        MakeSpecificTagList(MiscData, AllFree);
        ShowTaggedFiles(false);
      end; { if }

    AreaToUse := LineCfg^.Exitinfo^.Userinfo.FileArea;
    Temp := FirstWord(MiscData, defExtractWord, true);
    if FVal(Temp) = 00 then MiscData := Temp + #32 + MiscData
      else AreaToUse := FVal(Temp);

    If AllowEdit then
      EditTagList(False, AnyFile, Global, Pos('/FG', SUpCase(MiscData)) > 00, AreaToUse);

    WriteLn;
    AskForFilePasswords;
    WriteLn;

   {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logTransfer, 'DoDownload - now see if any files are selected');
   {$ENDIF}

    If TaggingObj^.CurrentTagged=00 then
      begin
         WriteLn;
         WriteLn('`A14:', LangObj^.ralGet(ralnoFiles));
         InputObj^.PressEnter(True, False);
         MultiLnObj^.WriteUseron('', uonBrowsing);
         Exit;                              { After adding nothing? Abort }
      end; { CurrentTagged }

    If NOT DownloadRestrictions(TRUE) then EXIT;     { No download access }

    {$IFNDEF ELEUNIX}
    If LineCfg^.Exitinfo^.Baud=00 then
      If NOT LocalDownload(NrDownloads, DownloadsK) then EXIT;
    If LineCfg^.Exitinfo^.Baud>00 then
    {$ENDIF}
      If NOT DoNormalDownload(NrDownloads, DownloadsK) then EXIT;

 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logTransfer, 'Returning from DoNormalDownload, delaying 1 sec');
 {$ENDIF}

  Support.Delay(1000);                  { Wait 1 second to let remote recover }
  OutBlockObj^.DumpBlock;
  OutBlockObj^.ClearBlock;

 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logTransfer, 'After delay, sending CR/CLREOL');
 {$ENDIF}


  Write(#10,'`E:');
  If NrDownloads>01 then
      WriteLn('`A12:', NrDownloads, ' ', LangObj^.ralGet(ralFilesSent));

  If NrDownloads=01 then
      WriteLn('`A12:', NrDownloads, ' ', LangObj^.ralGet(ralFileSent));

  If NrDownloads=00 then
      WriteLn('`A12:', LangObj^.ralGet(ralNoSent));

 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logTransfer, 'Ok, sent statistics, wait for the user to press enter');
 {$ENDIF}

  InputObj^.PressEnter(False, False);

 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logTransfer, 'User has pressed enter, are we happy?');
 {$ENDIF}

  If Specific then
    begin
{      TagInfo^ := SaveTag;
      CurrentTagged := SaveNrFiles;
}
    end; { MakeSpecific }

   {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logTransfer, 'DoDownload ( end )');
   {$ENDIF}

   MultiLnObj^.WriteUseron('', uonBrowsing);
end; { proc. DoDownLoad }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function SearchIdxFast(FName: String): Boolean;
var IDX_F     : pFileObj;
    RaFile    : pFileObj;
    FilesInf  : FilesRecord;
    Counter   : Longint;
    NumRead   : NumReadType;
    IdxArray  : Array[0..119] of FilesIdxRecord;
    AbortSrch : Boolean;
    TempName  : String;
    TempStr   : String;
begin
{$IFDEF WITH_FULL}
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'SearchIDXFast (begin)');
  {$ENDIF}

  SearchIdxFast := false;
  AbortSrch := false;

  TempName := SUpCase(Trim(FName));
  if GlobalCfg^.RaConfig^.IgnoreDupeExt then
    TempName := NoExtension(TempName);

  {------------------------- Open the area file  ---------------------------}
  New(RaFile, Init);
  RaFile^.Assign(FilesFileName);
  RaFile^.FileMode := ReadMode + DenyNone;
  if NOT RaFile^.Open(1) then
    begin
      Dispose(RaFile, Done);
      EXIT;
    end; { if }


  While NOT RaFile^.EOF do
    begin
      if AbortSrch then BREAK;

      {---------------------- Read all areas defined -----------------------}
      RaFile^.BlkRead(FilesInf, SizeOf(FilesRecord));
      FilesInf.Filepath := ForceBack(FilesInf.FilePath);


      {------------------ If include the area in the dupe search -----------}
      if ReadBit(FilesInf.Attrib, 1) then
        begin
          New(Idx_F, Init);
          Idx_F^.Assign(GlobalCfg^.RaConfig^.FileBasePath+'idx'+BsChar+'fdb'+FStr(FilesInf.AreaNum)+'.idx');
          Idx_F^.FileMode := ReadMode + DenyNone;

          if Idx_F^.Open(SizeOf(FilesIdxRecord)) then
            begin
              Numread := 120;

              While NumRead = 120 do
                begin
                  NumRead := Idx_F^.BlkRead(IdxArray, SizeOf(IdxArray) div SizeOf(FilesIdxRecord));

                  if NumRead > 0 then
                   for Counter := 00 to (Numread - 01) do
                     begin
                       if GlobalCfg^.RaConfig^.IgnoreDupeExt then
                         IdxArray[Counter].Name := NoExtension(IdxArray[Counter].Name);

                       if (TempName = IdxArray[Counter].Name) then
                         begin

                           if NOT IsLfn(TempName) then
                             begin
                               AbortSrch := TRUE;
                             end
                               else begin
                                      TempStr := GetLfnName(IdxArray[Counter].Name, (Idx_F^.FilePos - (SizeOf(IdxArray)
                                                             div SizeOf(FilesIdxRecord))) + Counter,
                                                             Filesinf.AreaNum);
                                      if GlobalCfg^.RaConfig^.IgnoreDupeExt then
                                        TempStr := NoExtension(TempStr);

                                      if TempName = TempStr then
                                        AbortSrch := true;
                                    end; { if }

                         end; { if }

                       if AbortSrch then BREAK;
                     end; { for }
                end; { while }
            end; { IndexFile exist }

          Dispose(Idx_F, Done);
        end; { include in dupe upload scan }
    end; { While }

  Dispose(RaFile, Done);
  SearchIdxFast := AbortSrch;


  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'SearchIDXFast Result = ' + Bool2Str(AbortSrch));
    DebugObj.DebugLog(logTransfer, 'SearchIDXFast ( end )');
  {$ENDIF}
{$ENDIF}
end; { func. SearchIdxFast }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
function AcceptUploadedFile(FName: String): Boolean;
var Duplicate: Boolean;
    DoAccept : Boolean;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'AcceptUploadedFile (begin)');
    DebugObj.DebugLog(logTransfer, 'AcceptUploadedFile - Searching badfiles.ctl');
  {$ENDIF}

  DoAccept := true;


  if SearchCtlFile('badfiles.ctl', FName, True) then
    begin
      DoAccept := false;

      WriteLn('`A12:');
      WriteLn(LangObj^.ralGet(ralRejected2));
      WriteLn;
      InputObj^.PressEnter(true, true);
    end; { Is badfile }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'AcceptUploadedFile - Searching IDX files (FName='+Fname+', Accept='+Bool2Str(DoAccept));
  {$ENDIF}

  if DoAccept then
   if SearchIdxFast(Fname) then
     begin
        DoAccept := false;

        WriteLn('`A12:');
        WriteLn(LangObj^.ralGet(ralRejected1));
        WriteLn;
        InputObj^.PressEnter(true, true);
      end; { Dupe }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'AcceptUploadedFile - Done all checks (FName='+Fname+', Accept='+Bool2Str(DoAccept));
  {$ENDIF}

  AcceptUploadedFile := DoAccept;


  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'AcceptUploadedFile ( end )');
  {$ENDIF}
end; { func. AcceptUploadedFile }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
function DoNormalUpload(var NrDownloads, DownloadsK, NrUploads, UploadsK: Longint; FilesInf: FilesRecord;
                        UpMsg: Boolean; UploadPath: String; FAttaching: Boolean): Boolean;
var TempCH     : Char;
    TempPos    : Byte;
    ProtInf    : ProtocolRecord;
    InternKeys : CfgRec.CharSet;
    UpStartTime: Longint;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'DoNormalUpLoad (begin)');
  {$ENDIF}


  {--------------------- Initialize all standard values -----------------------}
  DoNormalUpload := true;


  {------------------- Make sure we use a valid protocol ----------------------}
  WriteLn('`A7:');

  repeat
    GetProtocolRecord(LineCfg^.Exitinfo^.Userinfo.DefaultProtocol, ProtInf);

    While (ProtInf.Name='') do
      begin
        SelectProtocol;
        GetProtocolRecord(LineCfg^.Exitinfo^.Userinfo.DefaultProtocol, ProtInf);
      end; { While }

    WriteLn('`A10:', LangObj^.ralGet(ralDefProt), ProtInf.Name);
    Write('`A11:', LangObj^.ralGetStr(ralSNA));

    repeat
      TempCH := UpCase(InputObj^.ReadKey);

      TempPos := Pos(TempCH, SUpcase(LangObj^.ralGetKeys(ralStrtAbr)));
      if TempCH=#13 then TempPos := 01;
    until (TempPos in [01, 02, 03, 04]);

    Case TempPos of
        01 : ;
        03 : SelectProtocol;
        04 : EXIT;
    end; { case }
  Until TempPos in [01];


  {----- Make sure that message uploads only with internal protocols ----------}
  GetInternKeys(InternKeys);

  if UpMsg then
    if NOT (UpCase(ProtInf.ActiveKey) in InternKeys) then
      begin
        OutputObj^.ClearScreen;
        WriteLn('`A14:', LangObj^.ralGet(ralOnlyIntr));
        InputObj^.PressEnter(False, True);
        DoNormalUpload := False;
        EXIT;
      end; { uppingmessage }

  {-------------------- Check for enough free diskspace -----------------------}
  OutputObj^.ClearScreen;
  Writeln('`A14:', LangObj^.ralGet(ralDrivSpace), CommaStr(GetDiskFree(FilesInf.FilePath[1])), #32, LangObj^.ralGet(ralBytes));

  if (GetDiskFree(FilesInf.FilePath[1]) div 1024) < GlobalCfg^.RaConfig^.MinUpSpace then
    begin
      WriteLn('`A12:');
      WriteLn(LangObj^.ralGet(ralNoSpace));
      InputObj^.PressEnter(False, False);
      EXIT;
    end; { Not Enough Free Space }

  WriteLn('`A11:', LangObj^.ralGet(ralProtocol1), ProtInf.Name);
  WriteLn('`A15:', LangObj^.ralGet(ralStartUl));


  {-------------------- Actually perform the upload ---------------------------}
  if UploadPath <> '' then
    FilesInf.FilePath := UploadPath;

  UpStartTime := CurrentTime;

  if NOT (UpCase(ProtInf.ActiveKey) in Internkeys) then
    DoTransferExternal(False, False, ProtInf, NrDownloads, DownloadsK,
                       Filesinf.FilePath, Filesinf.UploadArea, FAttaching)
     else DoTransferInternal(False, False, ProtInf, NrDownloads, DownloadsK,
                       FilesInf.FilePath, FilesInf.UploadArea, UpMsg, FAttaching);

  ProcessUploads(FilesInf.AreaNum, NrUploads, UploadsK, ProtInf.Name, FAttaching, Upmsg);

  if NrUploads > 0 then
   GiveBackUploadTime(UpStartTime);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'DoNormalUpLoad ( end )');
  {$ENDIF}
end; { func. DoNormalUpload }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
function LocalUpload(var Uploads, UploadsK: Longint; FilesInf: FilesRecord; FAttaching, UpMsg: Boolean;
                     UlPath: String): Boolean;
var UploadName : String;
    AreaNum    : Word;
    FileName   : String;
    FileSize   : LongInt;
    AbortUpload: Boolean;
    Dirinfo    : SearchRec;
    SourcePath : String;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'LocalUpload (begin)');
  {$ENDIF}

  {$IFDEF ELEUNIX}
    EXIT;
  {$ENDIF}

  AreaNum := FilesInf.AreaNum;
  LocalUpload := False;

  if NOT FAttaching then
   begin
     OutputObj^.ClearScreen;
     Writeln('`A14:', LangObj^.ralGet(ralDrivSpace), CommaStr(GetDiskFree(FilesInf.FilePath[1])), #32,
                      LangObj^.ralGet(ralBytes));
    end; { if }

  If (GetDiskFree(FilesInf.FilePath[1]) div 1024) < GlobalCfg^.RaConfig^.MinUpSpace then
    begin
      WriteLn('`A12:');
      WriteLn(LangObj^.ralGet(ralNoSpace));
      InputObj^.PressEnter(False, False);
      EXIT;
    end; { Not Enough Free Space }

  if NOT Fattaching then
   begin
     WriteLn('`A15:', LangObj^.ralGet(ralLocUpl));
     WriteLn;
   end; { if }

  WriteLn('`A10:', langObj^.ralGet(ralEntrName));
  WriteLn;

  repeat
    Write('`A03:', LangObj^.ralGet(ralFile1));

    UploadName := '';
    GetString(UploadName, 80, [#32..#254], false, False, false, False);
    UploadName := SUpCase(Trim(UploadName));
    SourcePath := JustPath(UploadName);

    If AreaNum=00 then
        begin
          AreaNum := LineCfg^.Exitinfo^.Userinfo.FileArea;
          GetFilesRecord(FilesInf, AreaNum, False);

          If FilesInf.UploadArea>00 then AreaNum := FilesInf.UploadArea;
        end; { Areanum = 00 }

    if UlPath <> '' then FilesInf.FilePath := ForceBack(UlPath);

    FindFirst(UploadName, AnyFile - VolumeID, Dirinfo);

    if DosError <> 0 then
     if UploadName <> '' then
      begin
        WriteLn('`X1:`E:`A12:', LangObj^.ralGet(ralNotFound2), #32, UploadName);
      end; { if }

    While DosError = 00 do
      begin
        AbortUpload := false;
        if Dirinfo.Attr AND Directory <> 0 then Dirinfo.Name := '';

        if Dirinfo.Name <> '' then
         if FileExist(FilesInf.FilePath + JustName(Dirinfo.Name)) then
           begin
             if NOT FAttaching then
               begin
                 WriteLn('`A11:`X1:`E:', Dirinfo.Name+': '+LangObj^.ralGet(ralExists2));
                 AbortUpload := True;
               end
                else begin
                       WriteLn;
                       AbortUpload := NOT InputObj^.ralStrYesNoAsk(ralAlreadAtt);
                     end; { is file-attaching }
           end; { already exist }

        Write('`X1:`A15:`E:');

        if Dirinfo.Name <> '' then
         if SearchIdxFast(Dirinfo.Name) then
           begin
             WriteLn('`A3:`X1:`E:', Dirinfo.Name, ': ', LangObj^.ralGet(ralExists1));
             Dirinfo.Name := '';
           end; { if }

        if Dirinfo.Name <> '' then
          if NOT AbortUpload then
            if NOT FileCopy(SourcePath + Dirinfo.Name, FilesInf.FilePath + JustName(Dirinfo.Name), True, True) then
              begin
                WriteLn('`A12:', LangObj^.ralGet(ralNotFound2), #32, Dirinfo.Name);
              end { File doesn't exist }
                else begin
                       FileName := JustName(Dirinfo.Name);
                       FileSize := GetFileSize(FilesInf.FilePath + FileName, 1);

                       WriteLn('`X1:`A11:', LangObj^.ralGet(ralSent1), Dirinfo.Name);

                       Inc(LineCfg^.TotalUploads);
                       LineCfg^.UploadInfo^[LineCfg^.TotalUploads].FName := JustName(FileName);
                       LineCfg^.UploadInfo^[LineCfg^.TotalUploads].Size := GetFileSize(SourcePath + FileName, 01);
                     end; { File DOES exist }

        FindNext(Dirinfo);
      end; { while }

      WriteLn('`X1:`A15:`E:');
      OutputObj^.ResetLines(01);

      FindClose(Dirinfo);
  until (UploadName = '');

  LocalUpload := True;

  ProcessUploads(FilesInf.UploadArea, Uploads, UploadsK, 'Local', FAttaching, UpMsg);


  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'LocalUpload ( end )');
  {$ENDIF}
end; { func. DoLocalUpload }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
Procedure DoUpload(MiscData: String; FAttaching, UpMsg: Boolean; UlPath: String; var Uploads: Longint);
var NrDownloads,
    DownloadsK   : Longint;
    UploadsK     : Longint;
    FilesInf     : FilesRecord;
    LocalUpload  : Boolean;
    AreaSpec     : String;
    SaveFrozen   : Boolean;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTransfer, 'DoUpload (begin)');
  {$ENDIF}

  MultiLnObj^.WriteUseron('', uonUpDown);

  if LineCfg^.Exitinfo^.Baud>00 then
    If FixBaud(LineCfg^.Exitinfo^.Baud) < GlobalCfg^.RaConfig^.TransferBaud then
       begin
         WriteLn('`A12:');
         Writeln(LangObj^.ralGet(ralXferSlow));
         InputObj^.PressEnter(False, False);
         EXIT;
       end; { Too Slow }

  AreaSpec := SUpCase(FirstWord(MiscData, defExtractWord, false));
  if FVal(AreaSpec) <> 00 then GetFilesRecord(FilesInf, FVal(AreaSpec), True)
    else GetFilesRecord(FilesInf, LineCfg^.Exitinfo^.Userinfo.FileArea, True);

  If FilesInf.UploadArea>00 then
      GetFilesRecord(FilesInf, FilesInf.UploadArea, True);
  if FilesInf.UploadArea = 00 then FilesInf.UploadArea := FilesInf.AreaNum;


  if NOT FAttaching then
   if NOT UpMsg then
     If NOT CheckFileAreaAccess(FilesInf, False, False, True, False, 00, LineCfg^.Exitinfo^) then
       begin
         WriteLn('`A12:');;
         Writeln;
         Writeln(LangObj^.ralGet(ralNoUpAcc));
         InputObj^.PressEnter(False, FAlse);
         EXIT;
       end; { CheckFileAreaAccess }

  NrDownloads := 00;
  DownloadsK  := 00;
  Uploads     := 00;
  UploadsK    := 00;
  LineCfg^.TotalUploads:= 00;
  SaveFrozen  := LineCfg^.TimeFrozen;
  LineCfg^.TimeFrozen  := true;

  if NOT FAttaching then
    OutputObj^.ClearScreen;

  LineCfg^.TransferMode := Receive;

  if FAttaching then
   FilesInf.FilePath := ForceBack(UlPath);

  if NOT FileExist(FilesInf.FilePath) then
       begin
          RaLog('!', 'Upload path ('+SUpCase(FilesInf.FilePath)+') does not exist!');
          EXIT;
       end; { if }

  LocalUpload := false;
  if FAttaching then
   if SUpcase(Trim(GlobalCfg^.RaConfig^.SysOp)) = SUpcase(Trim(LineCfg^.Exitinfo^.Userinfo.Name)) then
    if LineCfg^.Exitinfo^.Baud > 00 then
     if InputObj^.ralStrYesNoAsk(ralLocAttach) then
       begin
         LocalUpload := True;
       end; { if }

  if NOT LocalUpload then
  {$IFNDEF ELEUNIX}
   if LineCfg^.Exitinfo^.Baud>00 then
  {$ENDIF}
    if NOT DoNormalUpload(NrDownloads, DownloadsK, Uploads, UploadsK, FilesInf, UpMsg, UlPath, FAttaching) then EXIT;

   {$IFNDEF ELEUNIX}
   if (LineCfg^.Exitinfo^.Baud=00) OR (LocalUpload) then
    if NOT Transfer.LocalUpload(Uploads, UploadsK, FilesInf, FAttaching, UpMsg, UlPath) then EXIT;
   {$ENDIF}

  LineCfg^.TotalUploads := 00;
  contrObj^.TimeRemain;
  LineCfg^.TimeFrozen := SaveFrozen;

  if NOT Fattaching then
   if NOT UpMsg then
     begin
       if ComObj <> nil then
         begin
           ComObj^.Com_PurgeOutBuffer;
           ComObj^.Com_PurgeInBuffer;
           termObj^.Resetterminal;
         end; { if }


       Write('`X1:`E:');
       If Uploads>01 then
           WriteLn('`A12:', Uploads, ' ', LangObj^.ralGet(ralFilesRec));

       If Uploads=01 then
          WriteLn('`A12:', Uploads, ' ', LangObj^.ralGet(ralFileRec));

       If Uploads=00 then
          WriteLn('`A12:', LangObj^.ralGet(ralNoRec));
       InputObj^.PressEnter(False, False);
     end; { if }


  MultiLnObj^.WriteUseron('', uonBrowsing);
end; { proc. DoUpload }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { Transfer }

