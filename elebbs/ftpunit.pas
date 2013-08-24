unit FTPUNIT;
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
{&Delphi+}
(*
**
** FTP client component
**
** Copyright (c) 1998, 99 by Maarten Bekers
**
** Created : 02-May-1999
** Last update : 02-May-1999
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses FtpCli, FileObj;

type tFtpInterfaceObj = object
         FTP          : tFtpClient;

         {-- Constructors and destructors ----------------------------------}
         constructor Init;
         destructor Done;

         {-- Generic routines ----------------------------------------------}
         function  StartUpFTP(      Url: String;
                              UserName,
                              Password: String): Boolean;
         function  GetFtpFile(Url      : String;
                              DestPath : String): Boolean;
         function  DoneFTP: Boolean;
         procedure FtpGetList(URL: String; var DateStr, TimeStr, SizeStr: String);
         procedure GetFtpDirList(URL: String;
                                 ListProc: ShowLineProc);
         procedure FtpListToStr(var IsDir: Boolean; DirStr: String; var TimeStr, DateStr, NameStr, SizeStr: String);
         function  FtpRenameFile(Url: String; Orig, Dest: String): Boolean;
         function  FtpDeleteFile(Url: String): Boolean;
         function  ExtractFtpHost(Hostname: String): String;
         function  ExtractFtpName(Hostname: String): String;
         function  ExtractFtpPath(Hostname: String): String;
     end; { tFtpInterfaceObj }

type pFtpInterfaceObj = ^tFtpInterfaceObj;

var GlobListStr: String;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Global, Cases, ElLog_U, SysUtils,
      {$IFDEF WITH_DEBUG}
        Debug_u,
      {$ENDIF}
      StrPath, LongStr, WordStr, JDates,
      ObjDec;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GetSizeProc(DirStr: ShortString);
begin
  GlobListStr := DirStr;
end; { proc. GetSizeProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


procedure FtpDebugLog(C: Char; S: ShortString);
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, '(FTP) ('+C+'): '+s);
  {$ENDIF}
end; { proc. FtpDebugLog }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure RecvDataProc(path: String; S: String; Mode: ftpTransferStatus;
                       var Dest_F: pFileObj;
                       var DestPath: String);
begin
  if Mode = transStart then
    begin
      New(Dest_F, Init);
      Dest_F^.Assign(DestPath + Path);
      Dest_F^.Create(1);
    end; { if }

  if Mode = transDone then
    begin
      RaLog('!', Format('Transfer of %s completed (%d bytes)', [Path, Dest_F^.FileSize]));

      Dispose(Dest_F, done);
      Dest_F := nil;
    end; { if }

  if Mode = transNormal then
    begin
      Dest_F^.BlkWrite(S[1], Length(s));
    end; { if }
end; { proc. RecvDataProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tFtpInterfaceObj.Init;
begin
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tFtpInterfaceObj.Done;
begin
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFtpInterfaceObj.ExtractFtpHost(Hostname: String): String;
var Counter : Longint;
    TempHost: String;
begin
  TempHost := '';

  if SupCase(Copy(HostName, 1, 6)) = 'FTP://' then
    Delete(Hostname, 1, 6);

  for Counter := 01 to Length(HostName) do
    if Hostname[Counter] = '/' then
     begin
       TempHost := Copy(Hostname, 1, Counter - 01);
       BREAK;
     end; { if }

  ExtractFtpHost := TempHost;
end; { func. ExtractFtpHost }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFtpInterfaceObj.ExtractFtpName(Hostname: String): String;
var Counter : Longint;
    TempName: String;
begin
  TempName := '';

  if SupCase(Copy(HostName, 1, 6)) = 'FTP://' then
    Delete(Hostname, 1, 6);

  for Counter := length(HostName) downto 01 do
    if Hostname[Counter] = '/' then
     begin
       TempName := Copy(Hostname, Counter + 01, Length(HostName) - Pred(Counter));
       BREAK;
     end; { if }

  ExtractFtpName := TempName;
end; { func. ExtractFtpName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFtpInterfaceObj.ExtractFtpPath(Hostname: String): String;
var Counter : Longint;
    TempPath: String;
begin
  TempPath := '';
  if SupCase(Copy(HostName, 1, 6)) = 'FTP://' then
    Delete(Hostname, 1, 6);

  TempPath := ExtractFtpHost(HostName);
  Delete(Hostname, 1, Length(TempPath));

  for Counter := Length(Hostname) downto 01 do
    if Hostname[Counter] = '/' then
     begin
       TempPath := Copy(Hostname, 1, Counter);
       BREAK;
     end; { if }

  ExtractFtpPath := TempPath;
end; { func. ExtractFtpPath }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFtpInterfaceObj.StartupFTP(      Url: String;
                                     UserName,
                                     Password: String): Boolean;
var ErrorStr: String;
    PortNum : Longint;
    Counter : Longint;
    TempStr : String;

    Hostname: String;
begin
  Result := false;

  FTP := TFtpClient.Create;

  if Username = '' then Username := 'anonymous';
  if Password = '' then Password := 'john.doe@elebbs.bbs';;

  Ftp.UserName := Username;
  Ftp.Password := Password;
  Ftp.RecvData := {$IFDEF FPC}@{$ENDIF}RecvDataProc;
  Ftp.DebugLog := {$IFDEF FPC}@{$ENDIF}FtpDebugLog;


  {--------------------- Extract the hostname, filename and path --------------}
  Counter := Length(url);
  TempStr := '';
  Portnum := 21;

  While (Url[Counter] in ['0'..'9', ':']) AND (Counter > 01) do
    begin
      if Url[Counter] in ['0'..'9'] then
        TempStr := TempStr + Url[Counter];

      if Url[Counter] = ':' then
        begin
          Portnum := FVal(TempStr);
          BREAK;
        end; { if }

      Dec(Counter);
    end; { while }

  Hostname := ExtractFtpHost(Url);

  {---------------------- Try to connect to the FTP server --------------------}
  if NOT Ftp.ConnectToServer(Hostname, PortNum, ErrorStr) then
    begin
      RaLog('!', 'Unable to connect to '+Url);
      EXIT;
    end; { if }

  {-- Clear any pending input -------------------------------------------------}
  Ftp.ShowAllText(false);

  {---------------------- Try to give in FTP credentials ----------------------}
  if NOT Ftp.Logon then
    begin
      RaLog('!', 'Unable to logon to '+Url);
      EXIT;
    end; { if }

  Result := true;
end; { func. StartupFtp }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFtpInterfaceObj.GetFtpFile(Url      : String;
                                     DestPath : String): Boolean;
var Hostname,
    PathName,
    FileName   : String;
    Dest_F     : pFileObj;
begin
  Hostname := ExtractFtpHost(Url);
  Pathname := ExtractFtpPath(Url);
  Filename := ExtractFtpName(Url);

  RaLog('!', Format('Transferring %s from %s', [Filename, Hostname]));

  {---------------------- Actually start the transfer -------------------------}
  Ftp.Cwd(Pathname);                            { Select the wanted directory }
  if Ftp.Retr(Filename, Dest_F, DestPath) then
    begin
      Result := true;
    end; { if }
end; { func. GetFtpfile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFtpInterfaceObj.GetFtpDirList(Url: String;
                                         ListProc: ShowLineProc);
var PathName: String;
begin
  Pathname := ExtractFtpPath(Url);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logTcpIp, 'GetFtpDirList(), Url = '+Url+', PathName='+PathName);
  {$ENDIF}

{$IFNDEF FPC}
  Ftp.ListData := @Listproc;
{$ENDIF}
  Ftp.Cwd(Pathname);                            { Select the wanted directory }
  Ftp.List('.');
end; { proc. GetFtpDirList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFtpInterfaceObj.FtpGetList(URL: String; var DateStr, TimeStr, SizeStr: String);
var PathName : String;
    FileName : String;
    IsDir    : Boolean;
    NameStr  : String;
begin
  Pathname := ExtractFtpPath(Url);
  FileName := ExtractFtpName(Url);

{$IFNDEF FPC}
  Ftp.ListData := GetSizeProc;
{$ENDIF}
  Ftp.Cwd(Pathname);                            { Select the wanted directory }
  Ftp.List(Filename);

  FtpListToStr(IsDir, GlobListStr, TimeStr, DateStr, NameStr, SizeStr);
end; { func. FtpGetSize }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFtpInterfaceObj.DoneFTP: Boolean;
begin
  if Ftp <> nil then Ftp.Free;
  Ftp := nil;

  DoneFTP := true;
end; { func. DoneFTP }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFtpInterfaceObj.FtpListToStr(var IsDir: Boolean; DirStr: String; var TimeStr, DateStr, NameStr, SizeStr: String);
var YearStr,
    MonthStr: String;
begin
  { Try to determinate type of filelisting }

  if DirStr[1] in ['0'..'9'] then                               { DOS-mode }
    begin
      { 10-14-97  10:23AM                   35 WATCHDOG.MSG }
      { 05-05-98  04:42PM       <DIR>          SBCD }
      DateStr := Extractword(DirStr, 1, defExtractWord, false, false);
      TimeStr := ExtractWord(DirStr, 2, defExtractWord, false, false);
      SizeStr := ExtractWord(DirStr, 3, defExtractWord, false, false);
      NameStr := Copy(DirStr, 40, Length(DirStr) - 39);

      IsDir := DirStr[33] = '<';
    end { if }
     else begin                                                { UNIX-mode }
     {NT}   { dr-xr-xr-x   1 owner    group               0 May  5  1998 TP }
     {LNX}  { -rw-r--r--   1 ftp      ftp          1491 Aug  1 12:48 QA_MAIN.ANS }
            TimeStr := '12:00AM';
            IsDir   := DirStr[1] = 'd';

            SizeStr := ExtractWord(DirStr, 5, defExtractWord, false, false);
            MonthStr:= SUpCase(Trim(Extractword(DirStr, 6, defExtractWord, false, false)));

            NameStr := DirStr;
            RemoveWordNr(NameStr, 1, defExtractWord, false);
            RemoveWordNr(NameStr, 1, defExtractWord, false);
            RemoveWordNr(NameStr, 1, defExtractWord, false);
            RemoveWordNr(NameStr, 1, defExtractWord, false);
            RemoveWordNr(NameStr, 1, defExtractWord, false);
            RemoveWordNr(NameStr, 1, defExtractWord, false);
            RemoveWordNr(NameStr, 1, defExtractWord, false);
            RemoveWordNr(NameStr, 1, defExtractWord, false);
            NameStr := Trim(NameStr);

            YearStr := ExtractWord(DirStr, 8, defExtractWord, false, false);

            if Pos(':', YearStr) > 00 then
              begin
                TimeStr := YearStr;
                YearStr := Copy(FStr(GetYear), 3, 2);
              end
                else YearStr := Copy(YearStr, 3, 2);

            MonthStr := LeadingZero(MonthNameToNum(MonthStr), 2);
            DateStr := MonthStr + '-' +
                       LeadingZero(FVal(ExtractWord(DirStr, 7, defExtractWord, false, false)), 2) +
                       '-' + YearStr;
          end; { unix }

end; { proc. FtpListTostr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  tFtpInterfaceObj.FtpDeleteFile(Url: String): Boolean;
var PathName : String;
    FileName : String;
begin
  Pathname := ExtractFtpPath(Url);
  FileName := ExtractFtpName(Url);
  Ftp.Cwd(Pathname);                            { Select the wanted directory }

  FtpDeleteFile := Ftp.Dele(FileName);
end; { func. FtpDeleteFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFtpInterfaceObj.FtpRenameFile(Url: String; Orig, Dest: String): Boolean;
var PathName : String;
begin
  Pathname := ExtractFtpPath(Url);
  Ftp.Cwd(Pathname);                            { Select the wanted directory }

  FtpRenameFile := Ftp.Rename(Orig, Dest);
end; { func. RenameFtpFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit FTPUNIT }
