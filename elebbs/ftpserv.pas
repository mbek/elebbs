UNIT FTPSERV;
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
** FTPSERV, FTP server (RFC-959) for EleBBS.
**
** Copyright (c) 2000 by Maarten Bekers
**
** Created : 05-May-2000
** Last update : 03-Aug-2007
**
*)

(*
214-The following  commands are recognized(* ==>'s unimplemented).
   ABOR
   ALLO
   APPE
   CDUP
   CWD
   DELE
   HELP
   LIST
   MDTM
   MKD
   MODE
   NLST
   NOOP
   PASS
   PASV
   PORT
   PWD
   QUIT
   REST
   RETR
   RMD
   RNFR
   RNTO
   SITE
   SMNT
   STAT
   STOR
   STRU
   SYST
   TYPE
   USER
214  HELP command successful.
*)


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses
	TcpSrv;

const
	ftp_ListenThreadEnded   : Boolean = False;

var
	FtpServerSocket: TTcpServer;

procedure ftp_SetupServer;
procedure ftp_initserver;

function  ftp_ServerThread(P: pointer): Longint;
function  ftp_ExecuteConnection(P: Pointer): Longint;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Crt, Global, CfgRec, CentrStr, ScrnU, LongStr, StUtils,
      GenCfg, JDates, IoRes_Un, SysUtils, ReadCfg,
       WinTitle, GenFile, MemMan, Cases, WordStr, Crc_Unit, BbsKey,
        FileRout, Mail, MkMsgAbs, MkOpen, SockFunc, StrPath, SockDef,
          Access_U, TcpCli, Threads, Windows, Ellog_U,
          FileObj, BitWise, Debug_U, Aptimer, Cfgfile,
           ChrIso, User_U, SysVars, ObjDec, Ra2Fdb, esrv_u, Dos,
            FileSys, MgrFdb, limit_u, UnixDate, MultiLn, FtpServDiz;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

type
	FtpCommandset = (ftpUser, ftpPass, ftpRetr, ftpError, ftpHelp,
                     ftpQuit, ftpPort, ftpList, ftpAllo,  ftpMkd,
                     ftpMode, ftpNlst, ftpNoop, ftpPwd,   ftpSmnt,
                     ftpCdUp, ftpCwd,  ftpStru, ftpType,  ftpSyst,
                     ftpOpts, ftpRnFr, ftpRnTo, ftpSite,  ftpDele,
                     ftpStat, ftpRmd,  ftpAppe, ftpRest,  ftpAbor,
                     ftpPasv, ftpStor);

	pFtpConnectionRec   = ^ftpConnectionRec;
	ftpConnectionRec    = record
			IpAddr      : String[36];
			DataThread  : TTcpServer;
			FtpSession  : TTcpClient;
			Validated   : Boolean;
			UserName    : ShortString;
			UserRec     : Longint;
			UserExt     : UserExtensionRecord;
			LimitInfo   : LimitsRecord;
			PasswordCRC : Longint;
			RemAddr     : Array[1..6] of Byte;
			IsPassive   : Boolean;
			Exitinfo    : ExitinfoRecord;
			UseDataConn : Boolean;
			CurGroup    : Longint;
			CurArea     : Longint;
			ResumeOfs   : Longint;
			Anonymous   : Boolean;
			VirtIdxBuff : AnsiString;   { buffer for the VirtualIndex file contents }
			VirtIdxArea : LongInt;      { the area our VirtualIndex buffer pertains to }
			LogOnTime   : String[5];    { start time of /this/ session }
		end; { ftpConnectionRec = record }

const
	FtpServerPort       = 21;
	FtpMaxSessions      = 10;
	SecsTimeOut         = 10 * 60;   { Timeout after 10 minutes inactivity }
	ServerName          : String = 'EleBBS - FTP server running at %s';

	{-- command line options --}
	ftp_AllowAnonymous      : Boolean = False;                          { -XA                }
	ftp_PasvSrvIP           : Array[1..4] of Byte = (0, 0, 0, 0);       { -PASVSRVIP:addr    }
	ftp_PasvPorts           : Array[1..2] of SmallWord = (1025, 65535); { -PASVPORTS:x-y     }
	ftp_PasvOffset          : SmallWord = 0;                            { -PASVOFFSET:x      }
	ftp_DynIp				: String = '';
	ftp_TransferLog         : String = '';                              { -FTPXLOG:filename  }
	ftp_VirtualIndex        : String = '';                              { -FTPINDeX:filename }
	ftp_ImportDiz           : Boolean = False;                          { -FTPDIZ            }
	ftp_FirstNode           : LongInt = 256;                            { -FTPNODE:x         }
	ftp_MaxSessions         : SmallWord = FtpMaxSessions;               { -FTPLIMIT:x        }
	ftp_ServerPort          : SmallWord = FtpServerPort;                { -FTPPORT:x         }
	ftp_UpdateUseron        : Boolean = False;                          { -FTPUSERON         }


var
	DizImportThread : pDizImportThread;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function dynquotes(str: String): String;
{
	enclose string in double-quotes if there are any spaces
}
begin
	if (pos(' ', str) > 0)
		then dynquotes := '"' + str + '"'
		else dynquotes := str;
end; { func. dynquotes }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure WriteTransferLog(IPAddr           : String;
                           CurSlot          : LongInt;
                           Direction        : Char;
                           UserName         : String;
                           FileArea         : Word;
                           FileName         : String;
                           FileSize,
                           BytesTransferred,
                           ResumeOffset,
                           SecondsElapsed   : LongInt);
{
	cmdln option: -FTPXLOG

	Write transfer statistics to summary log
}
var
	LogFile: pFileObj;
	LogStr : String;

begin
	{$IFDEF WITH_DEBUG}
		DebugObj.DebugLog(logString, 'WriteTransferLog (begin)');
	{$ENDIF}

	if (ftp_TransferLog <> '') then
		begin
			LogStr := Format('%s %s %d %sx %s %d %s %d %d %d %d',[
				ISO8601,                { current date/time in a sensible format }
				IPAddr, CurSlot,
				SUpCase(Direction), 	{ 'R'x: received from client; 'T'x: sent to client }
				dynquotes(UserName),
				FileArea, dynquotes(FileName), FileSize,
				BytesTransferred, ResumeOffset, SecondsElapsed]);

			New(LogFile, Init);

			if (LogFile <> nil) then
				begin
					LogFile^.Assign(ftp_TransferLog);
					LogFile^.FileMode := ReadWriteMode + DenyNone;

					if LogFile^.Open(1) or LogFile^.Create(1) then
						begin
							LogFile^.WriteLn(LogStr);
							LogFile^.Close;
						end;

					Dispose(LogFile, Done);
    			end; { IF LogFile <> nil }
		end; { IF ftp_TransferLog <> '' }

	{$IFDEF WITH_DEBUG}
		DebugObj.DebugLog(logString, 'WriteTransferLog (end)');
	{$ENDIF}
end; { proc. WriteTransferLog }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FtpStatus(Code: Longint): String;
begin
  Result := '';

  Case Code of
   {200 : ;  we cannot send 200 as it can mean many things }
    220 : Result := Format(ServerName, [GlobalCfg^.RaConfig^.SystemName]) + ' ready.';
    221 : Result := 'Goodbye.';
    226 : Result := 'Transfer complete';
    425 : Result := 'Couldn''t open data connection.';
    450 : Result := 'Requested file action not taken';
    500 : Result := 'Unknown command';
    502 : Result := 'Command not implemented';
    550 : Result := 'Requested action not taken: file unavailable.';
    552 : Result := 'Requested action not taken: storage allocation exceeded.';
    553 : Result := 'Requested action not taken: illegal filename.';
    else begin
          {$IFDEF WITH_DEBUG}
            DebugObj.DebugLog(logString, 'FtpStatus: missing string for code ' + FStr(Code));
          {$ENDIF}
         end; { else }
  end; { case }
end; { func. FtpStatus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SendCodeStr(const CurSlot: Longint; Code: Word);
begin
  with ftpConnectionRec(Connections[Curslot].SrvData^) do
    FtpSession.SendStrLn(Format('%d %s', [Code, FtpStatus(Code)]));
end; { proc. SendCodeStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SendCodeMsg(const CurSlot: Longint; Code: Word; MsgStr: String);
begin
  with ftpConnectionRec(Connections[Curslot].SrvData^) do
    FtpSession.SendStrLn(Format('%d %s', [Code, MsgStr]));
end; { proc. SendCodeMsg }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CloseDataConnection(const CurSlot: Longint; DidSuccess: Boolean): Boolean;
{
	XXXBUGBUG: DidSuccess: not used
}
begin
    {-- initialize basic variables -------------------------------------------}
    CloseDataConnection := True;

    with ftpConnectionRec(Connections[CurSlot].SrvData^) do
        begin
            if (not UseDataConn) then
                DataThread.Disconnect;

            IsPassive := False;
            FillChar(RemAddr, SizeOf(RemAddr), #0);
            {DataThread.SockHandle := -1;}
            DataThread.Destroy;
            DataThread := TTcpServer.Create;
			DataThread.ReuseAddr := True;
        end; { with }

    SendCodeStr(CurSlot, 226)
end; { func. CloseDataConnection }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function OpenDataConnection(const CurSlot: Longint; var ResStr: String): Boolean;
var
	IpStr	   : String;
	PortAr	   : Array[1..2] of Byte;
	PortNum    : Smallword ABSOLUTE PortAr;
	ClientAddr : PSockAddr; 					{ Address socket structure }
	ClientRC   : Longint;					{ Return code for socket calls }
	TempLen    : Longint;				 { Variable used for Accept() call }
	Counter    : Longint;										 { timeout }

begin
	{-- initialize basic variables -------------------------------------------}
	OpenDataConnection := FALSE;
	ResStr := '';

	with ftpConnectionRec(Connections[CurSlot].SrvData^) do
		begin
			if UseDataConn then
				{-- if we use the current connection, just do that -----------------------}
				begin
					OpenDataConnection := True;
					DataThread.SockHandle := FtpSession.SockHandle;
				end

			else if (NOT IsPassive) and (RemAddr[1] <> 0) then
				begin
					{-- Make sure our data is in the specified form ----------------------}
					Move(RemAddr, IpAddr, 4);

					{-- move the last 2 bytes (ports) to the word-var --------------------}
					PortAr[1] := RemAddr[6];
					PortAr[2] := RemAddr[5];

					{-- setup the ipstring -----------------------------------------------}
					IpStr := FStr(RemAddr[1]) + '.' +
							 FStr(RemAddr[2]) + '.' +
							 FStr(RemAddr[3]) + '.' +
							 FStr(RemAddr[4]);

					{-- lets connect to the server ---------------------------------------}
					if DataThread.ConnectToServer(IpStr, PortNum, ResStr) then
						begin
							OpenDataConnection := True;
							SendCodeMsg(CurSlot, 150, Format('Connecting to port %d (%s)', [PortNum, IpStr]))
						end
					else
						begin
							OpenDataConnection := False;
							SendCodeStr(CurSlot, 425);
						end;
				end

			else if (IsPassive) and (DataThread.ServerHandle <> -1) then
				begin
					New(ClientAddr);
					FillChar(ClientAddr^, SizeOf(ClientAddr^), #00);
					Counter := 0;

					repeat
						Counter := Counter + 1;
                                                TempLen := SockAddr_Len;


						ClientRC := SockAccept(DataThread.ServerHandle,
									  ClientAddr,
									  TempLen);

						DataThread.DoSleep(1);
					until (ClientRC <> -1) or (Counter >= 4000);

					Dispose(ClientAddr);

					if (ClientRC = -1) then
						begin
							OpenDataConnection := False;
							SendCodeStr(CurSlot, 425);
						end
					else
						begin
							SocksetBlockingIO(ClientRC, false);
							DataThread.SockHandle := ClientRC;
							OpenDataConnection := True;
							SendCodeMsg(CurSlot, 150, 'Data connection opened...');
						end;
				end

			else
				begin
					OpenDataConnection := False;
					SendCodeMsg(CurSlot, 425, 'Send PORT or PASV first.');
				end;
		end;
end; { func. OpenDataConnection }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdHelp(const CurSlot: Longint; var ParamString: String);
var HelpCmd: String;

procedure SendHelpMsg(Msg: String);
begin
  with ftpConnectionRec(Connections[Curslot].SrvData^) do
    FtpSession.SendStrLn(Format('%d Syntax: %s', [214, Msg]));
  HelpCmd := '';
end; { proc. SendHelpMsg }

begin
  {-- Check if we need to send a list of all supported commands -------------}
  if ParamString = '' then
    with ftpConnectionRec(Connections[Curslot].SrvData^) do
      begin
        FtpSession.SendStrLn('214-The following commands are recognized (* ==> ''s unimplemented).');

        FtpSession.SendStrLn('   ABOR');
        FtpSession.SendStrLn('   ALLO');
        FtpSession.SendStrLn('   APPE');
        FtpSession.SendStrLn('   CDUP');
        FtpSession.SendStrLn('   CWD');
        FtpSession.SendStrLn('   DELE');
        FtpSession.SendStrLn('   HELP');
        FtpSession.SendStrLn('   LIST');
        FtpSession.SendStrLn('   MKD');
        FtpSession.SendStrLn('   MODE');
        FtpSession.SendStrLn('   NLST');
        FtpSession.SendStrLn('   NOOP');
        FtpSession.SendStrLn('   PASS (*)');
        FtpSession.SendStrLn('   PASV (*)');
        FtpSession.SendStrLn('   PORT');
        FtpSession.SendStrLn('   PWD');
        FtpSession.SendStrLn('   QUIT (*)');
        FtpSession.SendStrLn('   REST');
        FtpSession.SendStrLn('   RETR');
        FtpSession.SendStrLn('   RMD');
        FtpSession.SendStrLn('   RNFR');
        FtpSession.SendStrLn('   RNTO');
        FtpSession.SendStrLn('   SITE');
        FtpSession.SendStrLn('   STAT');
        FtpSession.SendStrLn('   STOR (*)');
        FtpSession.SendStrLn('   STRU');
        FtpSession.SendStrLn('   SYST');
        FtpSession.SendStrLn('   TYPE');
        FtpSession.SendStrLn('   USER (*)');

        FtpSession.SendStrLn('214  HELP command successful.');
      end
        else begin      { Send a command depending upon twhat the user wants }
               HelpCmd := SUpCase(Trim(ExtractWord(ParamString, 1, defExtractWord, false, false)));

               if HelpCmd = 'ABOR' then SendHelpMsg('ABOR (abort operation)');
               if HelpCmd = 'ALLO' then SendHelpMsg('ALLO (allocate storage vacuously)');
               if HelpCmd = 'APPE' then SendHelpMsg('APPE <file-name>');
               if HelpCmd = 'CDUP' then SendHelpMsg('CDUP change to parent directory');
               if HelpCmd = 'CWD'  then SendHelpMsg('CWD <directory-name>');
               if HelpCmd = 'DELE' then SendHelpMsg('DELE <file-name>');
               if HelpCmd = 'HELP' then SendHelpMsg('HELP <string>');
               if HelpCmd = 'LIST' then SendHelpMsg('LIST <path-name>');
               if HelpCmd = 'MKD'  then SendHelpMsg('MKD <path-name>');
               if HelpCmd = 'MODE' then SendHelpMsg('MODE (specify transfer mode)');
               if HelpCmd = 'NLST' then SendHelpMsg('NLST <path-name>');
               if HelpCmd = 'NOOP' then SendHelpMsg('NOOP');
               if HelpCmd = 'PASS' then SendHelpMsg('PASS <password>');
               if HelpCmd = 'PASV' then SendHelpMsg('PASV (set server in passive mode)');
               if HelpCmd = 'PORT' then SendHelpMsg('PORT <b0,b1,b2,b3,b4,b5>');
               if HelpCmd = 'PWD'  then SendHelpMsg('PWD (return current directory)');
               if HelpCmd = 'QUIT' then SendHelpMsg('QUIT (terminate service)');
               if HelpCmd = 'REST' then SendHelpMsg('REST (marker)');
               if HelpCmd = 'RETR' then SendHelpMsg('RETR <file-name>');
               if HelpCmd = 'RMD'  then SendHelpMsg('RMD <path-name>');
               if HelpCmd = 'RNFR' then SendHelpMsg('RNFR <file-name>');
               if HelpCmd = 'RNTO' then SendHelpMsg('RNTO <file-name>');
               if HelpCmd = 'SITE' then SendHelpMsg('SITE (site-specific commands)');
               if HelpCmd = 'SMNT' then SendHelpMsg('SMNT <pathname>');
               if HelpCmd = 'STAT' then SendHelpMsg('STAT (get server status)');
               if HelpCmd = 'STOR' then SendHelpMsg('STOR <file-name>');
               if HelpCmd = 'STRU' then SendHelpMsg('STRU (specify file structure)');
               if HelpCmd = 'SYST' then SendHelpMsg('SYST (get operating system type)');
               if HelpCmd = 'TYPE' then SendHelpMsg('TYPE < I >');
               if HelpCmd = 'USER' then SendHelpMsg('USER <username>');

               {-- Return an error if we dont know it -----------------------}
               if HelpCmd <> '' then
                with ftpConnectionRec(Connections[Curslot].SrvData^) do
                 FtpSession.SendStrLn(Format('%d Unknown command %s', [501, HelpCmd]));
            end; { else }
end; { proc. cmdHelp }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdPort(const CurSlot: Longint; var ParamString: String);
begin
  {-- We get the port to connect to as follows: ----------------------------}
  {-- PORT aa,bb,cc,dd,ee,ff -----------------------------------------------}
  {-- aa,bb,cc,dd are the 4 bytes of the IP address, ee and ff are the -----}
  {-- port numbers ---------------------------------------------------------}

  {-- extract the ip-address -----------------------------------------------}
  with ftpConnectionRec(Connections[Curslot].SrvData^) do
    begin
      RemAddr[1] := Byte(FVal(ExtractWord(ParamString, 1, defExtractWord + [','], false, false))); { Byte() to make FPC STFU }
      RemoveWordNr(ParamString, 1, defExtractword + [','], true);
      RemAddr[2] := Byte(FVal(ExtractWord(ParamString, 1, defExtractWord + [','], false, false)));
      RemoveWordNr(ParamString, 1, defExtractword + [','], true);
      RemAddr[3] := Byte(FVal(ExtractWord(ParamString, 1, defExtractWord + [','], false, false)));
      RemovewordNr(ParamString, 1, defExtractword + [','], true);
      RemAddr[4] := Byte(FVal(ExtractWord(ParamString, 1, defExtractWord + [','], false, false)));
      RemovewordNr(ParamString, 1, defExtractword + [','], true);
   end; { with }

  {-- extract the port number ----------------------------------------------}
  with ftpConnectionRec(Connections[Curslot].SrvData^) do
    begin
      RemAddr[5] := Byte(FVal(ExtractWord(ParamString, 1, defExtractWord + [','], false, false)));
      RemovewordNr(ParamString, 1, defExtractword + [','], true);
      RemAddr[6] := Byte(FVal(ExtractWord(ParamString, 1, defExtractWord + [','], false, false)));
      RemovewordNr(ParamString, 1, defExtractword + [','], true);
    end; { with }

  {-- now validate the send parameters and respond -------------------------}
  with ftpConnectionRec(Connections[Curslot].SrvData^) do
    begin
      IsPassive := FALSE;
      UseDataConn := FALSE;

      if (RemAddr[1] = 0) then
        begin
          SendCodeMsg(CurSlot, 501, 'Parameter syntax error');
        end
          else SendCodeMsg(CurSlot, 200, 'PORT command successful');
    end; { with }
end; { proc. cmdPort }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ConvDirName(DirName: String): String;
type
  CharSet = set of char;
const IllegalChars : CharSet = [#32, '/', '\'];

var Counter: Longint;
    TmpStr : String;
begin
  TmpStr := '';

  {-- now loop to remove at least the spaces ------------------------------}
  for Counter := 01 to Length(DirName) do
    begin
      if DirName[Counter] in IllegalChars then
        TmpStr := TmpStr + '_'
         else TmpStr := TmpStr + DirName[Counter];
    end; { for }

  ConvDirName := TmpStr;
end; { func. ConvDirName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function MakeFileEntry(IsFile   : Boolean;
                       SizeStr  : String;
                       DateStr  : String;
                       FileName : String;
                       IsNLST   : Boolean): String;
var AccStr : String;                                        { Access string }
    UnkStr : String;                      { dont really know what it is for }
    UName,                                                       { username }
    GName  : String;                                            { Groupname }
begin
  {-- now create the access list -------------------------------------------}
  if IsFile then
    AccStr := '-r--r--r--'
     else AccStr := 'dr-xr-xr-x';

  {-- unknown string -------------------------------------------------------}
  UnkStr := '1';

  {-- username -------------------------------------------------------------}
  UName := 'user';

  {-- groupname-------------------------------------------------------------}
  GName := 'group';

  {-- and add the "filename" -----------------------------------------------}
  MakeFileEntry := AccStr + #32 +                              { ---------- }
                   MakeLen(UnkStr, 3, Space, true, false) + #32 +     {   1 }
                   MakeLen(UName, 8, Space, false, false) + #32 +    { user }
                   MakeLen(GName, 8, Space, false, false) + #32 +   { group }
                   MakeLen(SizeStr, 10, Space, true,  false)+ #32 +{ (size) }
                   DateStr + #32 +                                   { date }
                   ConvDirName(FileName);

  {-- if we want a simple listing, override this ---------------------------}
  if IsNLST then
    MakeFileEntry := FileName;
end; { func. MakeFileEntry }


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function MakeDirString(DirName: String; IsNLST: Boolean): String;
var DateStr: String;
begin
  {-- Create a datestr ----------------------------------------------------}
  DateStr := FormatDateTime('mmm dd  yyyy', Now);

  {-- and build it all now ------------------------------------------------}
  Result := MakeFileEntry(false,                              { is a file }
                          '1024',                                  { size }
                          DateStr, { directories always have current date }
                          ConvDirName(DirName),
                          IsNLST);

  {-- if we want a simple listing, override this ---------------------------}
  if IsNLST then
    Result := DirName;
end; { func. MakeDirString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SendGroupList(const CurSlot: Longint; WCName: String; IsNLST: Boolean);
var Group_F : pFileObj;
    GroupInf: GroupRecord;
begin
  {-- Initialize some variables --------------------------------------------}
  if WcName = '' then
    WcName := '*';

  {-- we have a working dataconnection, use it as we want ------------------}
  With ftpConnectionRec(Connections[CurSlot].SrvData^).DataThread do
    begin
      {-- We send a list of the groups, spaces replaced with ---------------}
      {-- underscores ------------------------------------------------------}

      {-- open the group file ----------------------------------------------}
      New(Group_F, Init);
      Group_F^.Assign(FGroupsFileName);
      Group_F^.FileMode := ReadMode + DenyNone;
      Group_F^.Open(1);

      {-- now loop through it ------------------------------------------}
      while NOT Group_F^.EOF do
        begin
          {-- read the grouprecord -------------------------------------}
          Group_F^.BlkRead(GroupInf, SizeOf(GroupRecord));

          {-- convert the groupname to a directory-listing -------------}
          if CheckGroupAccess(GroupInf, ftpConnectionRec(Connections[CurSlot].SrvData^).Exitinfo) then
           if IsWildCard(WcName, GroupInf.Name) then
             SendStrLn(MakeDirString(GroupInf.Name, IsNLST));
        end; { while }

      {-- and close the file -------------------------------------------}
      Dispose(Group_F, Done);
    end; { with }
end; { proc. SendGroupList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SendAreaList(const CurSlot: Longint; GroupNum: Longint; WCName: String;
                       IsNlst: Boolean);
var Area_F  : pFileObj;
    FilesInf: FilesRecord;
begin
  {-- Initialize some variables --------------------------------------------}
  if WcName = '' then
    WcName := '*';

  {-- we have a working dataconnection, use it as we want ------------------}
  With ftpConnectionRec(Connections[CurSlot].SrvData^).DataThread do
    begin
      {-- We send a list of the groups, spaces replaced with ---------------}
      {-- underscores ------------------------------------------------------}

      {-- open the group file ----------------------------------------------}
      New(Area_F, Init);
      Area_F^.Assign(FilesFileName);
      Area_F^.FileMode := ReadMode + DenyNone;
      Area_F^.Open(1);

      {-- now loop through it ------------------------------------------}
      while NOT Area_F^.EOF do
        begin
          {-- read the filesrecord -------------------------------------}
          Area_F^.BlkRead(FilesInf, SizeOf(FilesRecord));

          {-- if we have access to this group --------------------------}
          if CheckFileAreaAccess(FilesInf,
                                 FALSE,                      { download }
                                 TRUE,                     { groupcheck }
                                 FALSE,                  { upload check }
                                 TRUE,                    { list access }
                                 SmallWord(GroupNum),
                                 ftpConnectionRec(
                                   Connections[CurSlot].SrvData^).Exitinfo) then
            begin
              {-- convert the groupname to a directory-listing -------------}
              if IsWildCard(WcName, FilesInf.Name) then
                SendStrLn(MakeDirString(FilesInf.Name, IsNLST));
            end; { if }

        end; { while }

      {-- and close the file -----------------------------------------------}
      Dispose(Area_F, Done);
    end; { with }
end; { proc. SendAreaList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Dos2FtpDate(Date: Longint): String;
begin
  {-- Create a datestr ----------------------------------------------------}
  Result := FormatDateTime('mmm dd  yyyy', FileDateToDateTime(Date));
end; { func. Dos2FtpDate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function FileAvailForDownload(var AreaObj: FdbFileObj): Boolean;
begin
  FileAvailForDownload :=
    (NOT AreaObj.IsMissing) AND
    (NOT AreaObj.IsComment) AND
    (NOT AreaObj.IsUnlisted) AND
    (AreaObj.GetPassword = '') AND
    (NOT AreaObj.IsDeleted) AND
    (NOT AreaObj.IsNotAvail);
end; { func. FileAvailForDownload }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GenVirtualIndex(CurSlot: LongInt;  AreaNum: SmallWord);
{
	Generate a list of files and descriptions for AreaNum, store it in the
	connection record for future reference.

	We have to cache it so that it doesn't change between a LIST and a RETR
	because some clients try to match the downloaded file's size with the
	listed file's size.
}
var
	SrvData : pFtpConnectionRec;
	AreaObj : FdbFileObj;
	Counter : Longint;
	TmpBuff1,
	TmpBuff2: AnsiString;

begin
	{$IFDEF WITH_DEBUG}
		DebugObj.DebugLog(logString, 'GenVirtualIndex (begin)');
	{$ENDIF}

	{-- preparation: initialise variables and open the file area --}
	SrvData := Connections[CurSlot].SrvData;
	SrvData^.VirtIdxBuff := '';
	SrvData^.VirtIdxArea := -1;
	AreaObj.Init(AreaNum, False);	{ create = false }

	if (AreaObj.GetError = 0) then
		begin
			SrvData^.VirtIdxArea := AreaNum;

			{-- generate the actual file list --}
			for Counter := 0 to AreaObj.TotalRecords-1 do
				begin
					AreaObj.Read(Counter);

					if (AreaObj.GetError = 0) and (FileAvailForDownload(AreaObj)) then
						begin
							AreaObj.ReadDescription(AreaObj.TxtF, AreaObj.GetLongDescPtr);

							SrvData^.VirtIdxBuff := SrvData^.VirtIdxBuff + Format('%s B %s %s %s%s', [
								LeftJust(ConvDirName(AreaObj.GetFileName), 20),
								RightJust(CommaStr(AreaObj.GetSize), 11),
								FormatDateTime('yymmdd', FileDateToDateTime(AreaObj.GetFileDate)),
								AreaObj.GetDescLine(250),
								chr(10)]);
						end; { FileAvailForDownload(AreaObj) }
				end; { for Counter := 1 to AreaObj.TotalRecords }

			{-- generate a header --}
			{ XXXTODO: SystemName; "Directory $Group/$Area"; (optional) external header file }
			TmpBuff1 :=
				'Filename          Type      Length   Date Description' + chr(10) +
				'=================================================' + chr(10);

			{-- generate the fake description for our fake file; has to be done here to get the correct size --}
			Counter := 0;

			repeat
				TmpBuff2 := Format('%s B %s %s %s %s', [
					LeftJust(ConvDirName(ftp_VirtualIndex), 20),
					RightJust(CommaStr(length(SrvData^.VirtIdxBuff) + length(TmpBuff1) + Counter), 11),
					FormatDateTime('yymmdd', Now),
					'** this file **',
					chr(10)]);

				if Counter = length(TmpBuff2)
					then break
					else Counter := length(TmpBuff2);
			until False;

			{-- now we can add the header and fake file to the top of the main buffer --}
			SrvData^.VirtIdxBuff :=
				TmpBuff1 + TmpBuff2 + SrvData^.VirtIdxBuff;

			{-- we're done - clean up the mess --}
			AreaObj.Done;
		end; { AreaObj.GetError = 0 }

	{$IFDEF WITH_DEBUG}
		DebugObj.DebugLog(logString, 'GenVirtualIndex (end)');
	{$ENDIF}
end; { proc. GenVirtualIndex }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SendVirtualIndex(const CurSlot: LongInt;  AreaNum: SmallWord;  ResumeOfs: LongInt);
{
	Send the VirtualIndex buffer to the client.
}
var
	SrvData   : pFtpConnectionRec;
	LocalCache: Boolean;     { True: we called GenVirtualIndex; False: using previously cached copy }
	StartTime,
	EndTime,
	ToSend,                  { the size of the buffer to send }
	DidSend,                 { how much of the buffer was sent }
	TotalSent,               { total bytes transmitted }
	CurrentPos: LongInt;

begin
	{$IFDEF WITH_DEBUG}
		DebugObj.DebugLog(logString, 'SendVirtualIndex (begin)');
	{$ENDIF}

	{-- ftpConnectionRec(Connections[CurSlot].SrvData^) and "With" make even my 22" widescreen cry --}
	SrvData    := Connections[CurSlot].SrvData;

	{-- Always use the existing buffer (generated from the last LIST command) if valid, so that semi-intelligent clients don't --}
	{-- get their panties in a twist if the filesize changes.  Otherwise, generate a fresh list --}
	if (SrvData^.VirtIdxBuff = '') or (SrvData^.VirtIdxArea <> AreaNum) then
		begin
			LocalCache := True;
			GenVirtualIndex(CurSlot, AreaNum);
		end
	else
		LocalCache := False;

	CurrentPos := ResumeOfs;
	TotalSent  := 0;
	StartTime  := NowAsUnixDate;

	{-- send the buffer --}
	while (SrvData^.DataThread.ConnectionAlive) and (CurrentPos < length(SrvData^.VirtIdxBuff)) do
		begin
			ToSend := length(SrvData^.VirtIdxBuff) - CurrentPos;
			if ToSend > 4096 then
				ToSend := 4096;

			SrvData^.DataThread.SendBuf(SrvData^.VirtIdxBuff[CurrentPos+1], ToSend, DidSend);

			if (DidSend > 0) then
				begin
					inc(CurrentPos, DidSend);
					inc(TotalSent, DidSend);
				end
			else
				SrvData^.DataThread.DoSleep(100);
		end; { while }

	EndTime := NowAsUnixDate;

	{-- log the transfer; user record isn't updated as this is the FTP equivalent of a List Files op (always free) --}
	WriteTransferLog(
	Connections[CurSlot].IpAddr, CurSlot, 'T',
		SrvData^.UserName,
		AreaNum, ftp_VirtualIndex,
		length(SrvData^.VirtIdxBuff), TotalSent, ResumeOfs, (EndTime - StartTime));

	{-- Clear the cache if we made it ourselves so subsequent requests will be more up-to-date --}
	if LocalCache then
		begin
			SrvData^.VirtIdxBuff := '';
			SrvData^.VirtIdxArea := -1;
		end;

	{$IFDEF WITH_DEBUG}
		DebugObj.DebugLog(logString, 'SendVirtualIndex (end)');
	{$ENDIF}
end; { proc. SendVirtualIndex }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SendFileList(const CurSlot: Longint; CurArea: Longint; WCName: String;
                       IsNlst: Boolean);
var AreaObj     : FdbFileObj;
    TotalItems  : Longint;
    ItemCounter : Longint;
    TmpStr      : String;
begin
  {-- Initialize some variables --------------------------------------------}
  if WcName = '' then
    WcName := '*';

  {-- we have a working dataconnection, use it as we want ------------------}
  With ftpConnectionRec(Connections[CurSlot].SrvData^).DataThread do
    begin
      {-- virtual index support: send a fake index file entry --------------}
      if (ftp_VirtualIndex <> '') then
        begin
          GenVirtualIndex(CurSlot, SmallWord(CurArea));

          SendStrLn(MakeFileEntry(True,
                                  FStr(length(ftpConnectionRec(Connections[CurSlot].SrvData^).VirtIdxBuff)),
                                  FormatDateTime('mmm dd  yyyy', Now),
                                  ConvDirName(ftp_VirtualIndex),
                                  IsNLST));
        end;

      {-- open the group file ----------------------------------------------}
      AreaObj.Init(CurArea, false);                        { create = false }
      if AreaObj.GetError <> 0 then
        begin
          AreaObj.Done;
          EXIT;
        end; { if }

      {-- get the total number of items ------------------------------------}
      TotalItems := AreaObj.TotalRecords;
      ItemCounter := 0;

      {-- loop through all files -------------------------------------------}
      while (ItemCounter < TotalItems) AND (TotalItems > 0) do
        begin
          AreaObj.Read(ItemCounter);             { Read header sequentially }
          Inc(ItemCounter);

          {-- if the file is missing, we dont show it ----------------------}
          if FileAvailForDownload(AreaObj) then
             begin
               {-- create the filestring -----------------------------------}
               TmpStr := MakeFileEntry(true,                       { isFile }
                                       FStr(AreaObj.GetSize),      { FileSz }
                                       Dos2FtpDate(AreaObj.GetFileDate),
                                       AreaObj.GetFileName,
                                       IsNLST);

               {-- and sed the file ----------------------------------------}
               if IsWildCard(WcName, AreaObj.GetFileName) then
                 SendStrLn(TmpStr);
             end; { if }
        end; { while }

      AreaObj.Done;
    end; { with }
end; { proc. SendFileList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function StripIllegal(DirName: String): String;
var Counter: Longint;
begin
  {-- first change all /'s to \'s ------------------------------------------}
  for Counter := 01 to Length(DirName) do
    if DirName[Counter] = '/' then Dirname[Counter] := '\';

  {-- First remove all ../'s and ./'s --------------------------------------}
  {-- (we do not support it at all -----------------------------------------}
  while Pos('..\', DirName) > 0 do
    Replace('..\', '', DirName);

  while Pos('.\', DirName) > 0 do
    Replace('.\', '', DirName);

  {-- and set the contents -------------------------------------------------}
  StripIllegal := DirName;
end; { func. StripIllegal }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure BreakUpDirStr(DirName: String;
                        var GroupName, AreaName, FileName, WildCardStr: String;
                        var StartAtRoot: Boolean;
                        var ListArea, ListGroup: Longint);
var Counter  : Longint;
    WordOne  : Array[1..3] of String;
begin
  {-- Initialize the variables ---------------------------------------------}
  StartAtRoot := FALSE;
  WordOne[1] := '';
  WordOne[2] := '';
  WordOne[3] := '';
  Groupname := '';
  AreaName := '';
  FileName := '';
  WildCardStr := '';

  {-- first change all \'s to /'s ------------------------------------------}
  for Counter := 01 to Length(DirName) do
    if DirName[Counter] = '\' then Dirname[Counter] := '/';

  {-- First remove all ../'s and ./'s --------------------------------------}
  {-- (we do not support it at all -----------------------------------------}
  while Pos('../', DirName) > 0 do
    Replace('../', '', DirName);

  while Pos('./', DirName) > 0 do
    Replace('./', '', DirName);

  {-- do we start at the root? ---------------------------------------------}
  if DirName[1] = '/' then
    StartAtRoot := TRUE;

  {-- now lets see how many path seperators there are ----------------------}
  WordOne[1] := ExtractWord(DirName, 1, ['/'], false, false);
  RemoveWordNr(DirName, 1, ['/'], false);

  if Length(DirName) > 0 then
    begin
      WordOne[2] := ExtractWord(DirName, 1, ['/'], false, false);
      RemoveWordNr(DirName, 1, ['/'], false);
    end; { if }

  if Length(DirName) > 0 then
    begin
      WordOne[3] := ExtractWord(DirName, 1, ['/'], false, false);
      RemoveWordNr(DirName, 1, ['/'], false);
    end; { if }

  {-- move the wildcard to its own place -----------------------------------}
  if (Pos('*', WordOne[3]) > 0) OR
      (Pos('?', WordOne[3]) > 0) then
    begin
      WildCardStr := WordOne[3];
      WordOne[3] := '';
    end; { if }

  if (Pos('*', WordOne[2]) > 0) OR
      (Pos('?', WordOne[2]) > 0) then
    begin
      WildCardStr := WordOne[2];
      WordOne[2] := '';
    end; { if }

  if (Pos('*', WordOne[1]) > 0) OR
      (Pos('?', WordOne[1]) > 0) then
    begin
      WildCardStr := WordOne[1];
      WordOne[1] := '';
    end; { if }

  {-- reset everything if we start from the root ---------------------------}
  if StartAtRoot then
    begin
      ListArea := -1;
      ListGroup := -1;
    end; { if }

  {-- now put the result in the correct boxes ------------------------------}
  if WordOne[3] <> '' then
    begin
      Groupname := WordOne[1];
      AreaName := WordOne[2];
      FileName := WordOne[3];
    end { if }
      else begin
             if WordOne[2] <> '' then
               begin
                 if ListGroup > 0 then
                   begin
                     AreaName := WordOne[1];
                     FileName := WordOne[2];
                   end
                     else begin
                            GroupName := WordOne[1];
                            AreaName := WordOne[2];
                          end; { else }
               end
                 else begin
                        if (ListGroup > 0) AND (ListArea > 0) then
                          FileName := WordOne[1];

                        if (ListGroup > 0) AND (ListArea < 0) then
                          AreaName := WordOne[1];

                        if (ListGroup < 0) then
                          GroupName := WordOne[1];
                      end; { else }
           end; { else }
end; { proc. BreakUpDirStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


function SearchBjorkedName(AreaNum: Longint; FileName: String; Attrib: Word): String;
var
  AreaObj  : fdbFileObj;

  FileNum,
  Counter  : Longint;
begin
  for Counter := 1 to length(FileName) do
    begin
      if FileName[Counter] = '_' then
        FileName[Counter] := '?';
    end;

  { this should/could be improved to actually check wether the user is
    allowed to download the file, if its passwordless etc }
  FileNum := SearchNameInArea(AreaNum, Filename, true) - 1;

  if (FileNum < 0) then
    begin
    writeln('File(' + FileName + ') not found in ', areanum);
      SearchBjorkedName := '';
      EXIT;
    end; {if}

  {-- attempt to read the FDB record ---------------------------------------}
  AreaObj.Init(AreaNum, false);
  if (AreaObj.GetError <> 0) then
    begin
      AreaObj.Done;
      SearchBjorkedName := '';
      EXIT;
    end; { if }

  AreaObj.Read(FileNum);
  if (AreaObj.GetError <> 0) then
    begin
      AreaObj.Done;
      SearchBjorkedName := '';
      EXIT;
    end; { if }

  {-- check attributes, make sure this file is downloadable. ---------------}
  if (NOT FileAvailForDownload(AreaObj)) then
    begin
      AreaObj.Done;
      SearchBjorkedName := '';
      EXIT;
    end; { if }

  {-- w00t! mission accomplished -------------------------------------------}
  SearchBjorkedName := AreaObj.GetFileName;
  AreaObj.Done;
end; { func. SearchForBjorkedFileName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function SearchGroupName(CurSlot:Longint; GroupName: String): Longint;
var Group_F : pFileObj;
    GroupInf: GroupRecord;
begin
  {-- Initialize some variables -------------------------------------------}
  SearchGroupName := -1;
  GroupName := Trim(SUpCase(GroupName));

  {-- open the group file -------------------------------------------------}
  New(Group_F, Init);
  Group_F^.Assign(FGroupsFileName);
  Group_F^.FileMode := ReadMode + DenyNone;
  Group_F^.Open(1);

  {-- now loop through it -------------------------------------------------}
  while NOT Group_F^.EOF do
    begin
      {-- read the grouprecord --------------------------------------------}
      Group_F^.BlkRead(GroupInf, SizeOf(GroupRecord));

      {-- convert the groupname to a directory-listing --------------------}
      if CheckGroupAccess(GroupInf, ftpConnectionRec(Connections[CurSlot].SrvData^).Exitinfo) then
        begin
          if GroupName = SUpcase(ConvDirName(GroupInf.Name)) then
            begin
              SearchGroupName := GroupInf.AreaNum;
            end; { if }
        end; { if }
    end; { while }

  {-- and close the file --------------------------------------------------}
  Dispose(Group_F, Done);
end; { func. SearchGroupName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function SearchAreaName(CurSlot:Longint; AreaName: String): Longint;
var Area_F  : pFileObj;
    FilesInf: FilesRecord;
begin
  {-- Initialize some variables -------------------------------------------}
  SearchAreaName := -1;
  AreaName := Trim(SUpCase(AreaName));

  {-- we have a working dataconnection, use it as we want ------------------}
  With ftpConnectionRec(Connections[CurSlot].SrvData^).DataThread do
    begin
      {-- We send a list of the areas, spaces replaced with underscores ----}

      {-- open the group file ----------------------------------------------}
      New(Area_F, Init);
      Area_F^.Assign(FilesFileName);
      Area_F^.FileMode := ReadMode + DenyNone;
      Area_F^.Open(1);

      {-- now loop through it ------------------------------------------}
      while NOT Area_F^.EOF do
        begin
          {-- read the filesrecord -------------------------------------}
          Area_F^.BlkRead(FilesInf, SizeOf(FilesRecord));

          {-- if we have access to this group --------------------------}
          if CheckFileAreaAccess(FilesInf,
                                 FALSE,                      { download }
                                 FALSE,                    { groupcheck }
                                 FALSE,                  { upload check }
                                 TRUE,                    { list access }
                                 0,
                                 ftpConnectionRec(
                                  Connections[CurSlot].SrvData^).Exitinfo){ exitinfo } then
            begin
              if AreaName = SUpcase(ConvDirName(FilesInf.Name)) then
                begin
                  SearchAreaName := FilesInf.AreaNum;
                  BREAK;
                end; { if }
            end; { if }

        end; { while }

      {-- and close the file -----------------------------------------------}
      Dispose(Area_F, Done);
    end; { with }
end; { func. SearchAreaName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GetPartsFromDir(var   ParamString: String;
                          var   WildCardStr: String;
                          const CurSlot    : Longint;
                          var   InvalidDir : Boolean;
                          var   ListGroup,
                                ListArea   : Longint);
var DirStr      : String;

    GroupName,
    AreaName,
    FileName    : String;

    StartRoot   : Boolean;

begin
  {-- Initialize some variables --------------------------------------------}
  GroupName := '';
  AreaName := '';
  FileName := '';
  WildCardStr := '';
  InvalidDir := FALSE;
  StartRoot := FALSE;

  {-- Dirstring ----------------------------------------------------}
(*
	XXXBUGBUG: why? this breaks filenames with spaces
  DirStr := ExtractWord(ParamString, 1, defExtractWord, true, false);
*)
  DirStr := ParamString;

  {-- BreakUpDirStr ------------------------------------------------}
  if DirStr <> '' then
    begin
      {-- break it up in area and stuff ----------------------------}
      BreakUpDirStr(DirStr,
                    GroupName,
                    AreaName,
                    FileName,
                    WildCardStr,
                    StartRoot,
                    ListArea,
                    ListGroup);

      {-- now make a judgement what to list -------------------------}
      if StartRoot then
        begin
          ListGroup := -1;
          ListArea := -1;
        end; { if }

      if GroupName <> '' then
        begin
          ListGroup := SearchGroupName(CurSlot, GroupName);
          if ListGroup < 0 then
            InvalidDir := TRUE;
        end; { if }

      if AreaName <> '' then
        begin
          ListArea := SearchAreaName(CurSlot, AreaName);
          if ListArea < 0 then
            InvalidDir := TRUE;
        end; { if }

      if WildCardStr = '' then
        WildCardStr := FileName;
    end; { if }
end; { proc. GetPartsFromdir }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure LoadUser(CurSlot: Longint);
{
	Resync cached user record from file
}
begin
	{$IFDEF WITH_DEBUG}
		DebugObj.DebugLog(logUserBase, 'LoadUser (begin)');
	{$ENDIF}

	ftpConnectionRec(Connections[CurSlot].SrvData^).UserRec := SearchUser(ftpConnectionRec(Connections[CurSlot].SrvData^).ExitInfo.UserInfo.Name);

	if (ftpConnectionRec(Connections[CurSlot].SrvData^).UserRec >= 0) then
		begin
			GetUserRecord(ftpConnectionRec(Connections[CurSlot].SrvData^).UserRec,
						  ftpConnectionRec(Connections[CurSlot].SrvData^).ExitInfo.UserInfo,
						  ftpConnectionRec(Connections[CurSlot].SrvData^).UserExt,
						  False);

			ftpConnectionRec(Connections[CurSlot].SrvData^).UserName := ftpConnectionRec(Connections[CurSlot].SrvData^).ExitInfo.UserInfo.Name;

			{-- grab security limit record --}
			GetLevelLimitRecord(ftpConnectionRec(Connections[CurSlot].SrvData^).ExitInfo.UserInfo.Security, ftpConnectionRec(Connections[CurSlot].SrvData^).LimitInfo);

			{-- check if deleted --}
			if ReadBit(ftpConnectionRec(Connections[CurSlot].SrvData^).ExitInfo.UserInfo.Attribute, 0) then
				ftpConnectionRec(Connections[CurSlot].SrvData^).Validated := False;
		end
	else
		{ fail quietly for now - user will get spanked next time he tries to do something }
		ftpConnectionRec(Connections[CurSlot].SrvData^).Validated := False;

	{$IFDEF WITH_DEBUG}
		DebugObj.DebugLog(logUserBase, 'LoadUser (end)');
	{$ENDIF}
end; { proc. LoadUser }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SaveUser(CurSlot: Longint);
{
	Save cached user record to file
}
begin
	{$IFDEF WITH_DEBUG}
		DebugObj.DebugLog(logUserBase, 'SaveUser (begin)');
	{$ENDIF}

	ftpConnectionRec(Connections[CurSlot].SrvData^).UserRec := SearchUser(ftpConnectionRec(Connections[CurSlot].SrvData^).ExitInfo.UserInfo.Name);

	if (ftpConnectionRec(Connections[CurSlot].SrvData^).UserRec >= 0) then
		begin
			WriteUserRecord(ftpConnectionRec(Connections[CurSlot].SrvData^).UserRec,
							ftpConnectionRec(Connections[CurSlot].SrvData^).ExitInfo.UserInfo,
							ftpConnectionRec(Connections[CurSlot].SrvData^).UserExt);

			ftpConnectionRec(Connections[CurSlot].SrvData^).UserName := ftpConnectionRec(Connections[CurSlot].SrvData^).ExitInfo.UserInfo.Name;
		end
	else
		{ fail quietly for now - user will get spanked next time he tries to do something }
		ftpConnectionRec(Connections[CurSlot].SrvData^).Validated := False;

	{$IFDEF WITH_DEBUG}
		DebugObj.DebugLog(logUserBase, 'SaveUser (end)');
	{$ENDIF}
end; { proc. SaveUser }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure UpdateUserOn(CurSlot: LongInt);
{
	Create/update the useron.bbs record for the specified session
}
var
	UserOn : USERONrecord;
	UserOnF: pFileObj;
begin
	if not ftp_UpdateUseron then
		exit;

	FillChar(UserOn, SizeOf(USERONrecord), #0);
	if Config_OpenFile(UserOnF, GlobalCfg^.RaConfig^.SysPath+'useron.bbs', 1, WriteMode+DenyNone, True, False) > 0 then
		begin
			Config_DoneFile(UserOnF);
			exit;
		end;

	Config_SeekFile(UserOnF, LongInt(SizeOf(USERONrecord)) * (ftp_FirstNode + CurSlot -1));
	Config_ReadFile(UserOnF, UserOn, SizeOf(USERONrecord));
	Config_SeekFile(UserOnF, LongInt(SizeOf(USERONrecord)) * (ftp_FirstNode + CurSlot -1));

	with ftpConnectionRec(Connections[CurSlot].SrvData^) do
		begin
			UserOn.NodeNumber := ftp_FirstNode + CurSlot -1;
			if UserOn.NodeNumber < 255 then
				UserOn.Line := UserOn.NodeNumber
			else
				UserOn.Line := 255;

			UserOn.Name       := ExitInfo.UserInfo.Name;
			UserOn.Handle     := ExitInfo.UserInfo.Handle;
			UserOn.City       := ExitInfo.UserInfo.Location;
			UserOn.StatDesc   := 'FTP';
			UserOn.Status     := 255;
			UserOn.LastUpdate := NowAsUnixDate;

			if ExitInfo.UserInfo.NoCalls < 65535 then
				UserOn.NoCalls := ExitInfo.UserInfo.NoCalls
			else
				UserOn.NoCalls := 65535;

			if ReadBit(ExitInfo.UserInfo.Attribute2, 3) then { hidden }
				SetBit(UserOn.Attribute, 0);
		end;

	Config_WriteFile(UserOnF, UserOn, SizeOf(USERONrecord));
	Config_DoneFile(UserOnF);
end; { proc. UpdateUserOn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure RemoveUserOn(CurSlot: LongInt);
{
	Remove (if present) the useron.bbs record for the specified session
}
var
	UserOn : USERONrecord;
	UserOnF: pFileObj;

begin
	if not ftp_UpdateUseron then
		exit;

	FillChar(UserOn, SizeOf(USERONrecord), #0);

	if Config_OpenFile(UserOnF, GlobalCfg^.RaConfig^.SysPath+'useron.bbs', 1, WriteMode+DenyNone, False, False) = 0 then
		begin
			Config_SeekFile(UserOnF, LongInt(SizeOf(USERONrecord)) * (ftp_FirstNode + CurSlot - 1));
			Config_WriteFile(UserOnF, UserON, SizeOf(USERONrecord));
		end;

	Config_DoneFile(UserOnF);
end; { proc. RemoveUserOn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AppendLastCall(CurSlot: LongInt);
{
	Create the lastcall.bbs record for the specified session
}
var
	LastCall : LASTCALLrecord;
	LastCallF: pFileObj;

begin
	if not ftp_UpdateUseron then
		exit;

	if (ftpConnectionRec(Connections[CurSlot].SrvData^).UserName = '') or
	   (ftpConnectionRec(Connections[CurSlot].SrvData^).UserRec = 0) then
		exit;

	FillChar(LastCall, SizeOf(LASTCALLrecord), #0);

	with ftpConnectionRec(Connections[CurSlot].SrvData^) do
		begin
			LastCall.Name    := ExitInfo.UserInfo.Name;
			LastCall.Handle  := ExitInfo.UserInfo.Handle;
			LastCall.City    := ExitInfo.UserInfo.Location;
			LastCall.Times   := ExitInfo.UserInfo.NoCalls;
			LastCall.LogOn   := LogOnTime;
			LastCall.LogOff  := TimeStr(False, False);

			if (ftp_FirstNode + CurSlot -1) < 255 then
				LastCall.Line := (ftp_FirstNode + CurSlot -1)
			else
				LastCall.Line := 255;

			if ReadBit(ExitInfo.UserInfo.Attribute2, 3) then { hidden }
				SetBit(LastCall.Attribute, 0);
		end;

	if Config_OpenFile(LastCallF, LastCallFileName, 1, WriteMode+DenyNone, True, False) = 0 then
		begin
			Config_SeekFile(LastCallF, LastCallF.FileSize);
			Config_WriteFile(LastCallF, LastCall, SizeOf(LASTCALLrecord));
		end;

	Config_DoneFile(LastCallF);
end; { proc. AppendLastCall }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SendPleaseAuthFirst(const CurSlot: Longint);
begin
  SendCodeMsg(CurSlot, 530, 'Please login with USER and PASS first');
end; { proc. SendPleaseAuthFirst }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdSendList(const CurSlot: Longint; var ParamString: String; NLST: Boolean);
var ResStr      : String;
    InvalidDir  : Boolean;
    WildCardStr : String;
    ListArea    : Longint;
    ListGroup   : Longint;
begin
  {-- Initialize some variables --------------------------------------------}
  WildCardStr := '';

  {-- we default to the current group/area listing -------------------------}
  ListArea := ftpConnectionRec(Connections[CurSlot].SrvData^).CurArea;
  ListGroup := ftpConnectionRec(Connections[CurSlot].SrvData^).CurGroup;

  {-- open data ------------------------------------------------------------}
  if OpenDataConnection(CurSlot, ResStr) then
    begin
      {-- we have a working dataconnection, use it as we want --------------}
      With ftpConnectionRec(Connections[CurSlot].SrvData^).DataThread do
        begin
          {-- if we are at the root, show a group list ---------------------}
          with ftpConnectionRec(Connections[Curslot].SrvData^) do
            begin
              {-- Break it up ----------------------------------------------}
              GetPartsFromDir(ParamString,
                              WildCardStr,
                              CurSlot,
                              InvalidDir,
                              ListGroup,
                              ListArea);

              {-- send the grouplist ---------------------------------------}
              if (ListArea < 0) AND (ListGroup < 0) then
                begin
                  SendGroupList(CurSlot, WildCardStr, NLST);
                end
                  else begin
                         {-- send the arealist -----------------------------}
                         if (ListArea < 0) then
                           SendAreaList(CurSlot, ListGroup, WildCardStr, NLST);

                         {-- send the filelist -----------------------------}
                         if (ListArea > 0) AND (ListGroup > 0) then
                           begin
                             SendFileList(CurSlot,
                                          ListArea,
                                          WildCardStr,
                                          NLST);
                           end; { if }
                       end; { if }

            end; { with }
        end; { with }

      {-- and eventually close it again ------------------------------------}
      CloseDataConnection(CurSlot, true);
    end { if }
      else begin
             {-- send an error ---------------------------------------------}
             SendCodeMsg(CurSlot, 425, 'Unable to open data connection');
           end; { if }
end; { proc. cmdSendList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdAllo(const CurSlot: Longint; var ParamString: String);
begin
  SendCodeMsg(CurSlot, 200, 'Go ahead');
end; { proc. cmdAllo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdMkd(const CurSlot: Longint; var ParamString: String);
begin
  SendCodeStr(CurSlot, 502);
end; { proc. cmdMkd }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdRmd(const CurSlot: Longint; var ParamString: String);
begin
  SendCodeStr(CurSlot, 502);
end; { proc. cmdRmd }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdRest(const CurSlot: Longint; var ParamString: String);
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'cmd-Rest: '+ ParamString);
  {$ENDIF}
  ftpConnectionRec(Connections[CurSlot].SrvData^).ResumeOfs := FVal(ParamString);
  SendCodeMsg(CurSlot, 350, 'Restarting at ' + FStr(ftpConnectionRec(Connections[CurSlot].SrvData^).ResumeOfs) + '. Send STORE or RETRIEVE to initiate transfer.');
end; { proc. cmdRest }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdRnFr(const CurSlot: Longint; var ParamString: String);
begin
  SendCodeStr(CurSlot, 502);
end; { proc. cmdRnFr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdRnTo(const CurSlot: Longint; var ParamString: String);
begin
  SendCodeStr(CurSlot, 502);
end; { proc. cmdRnTo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdStor(const CurSlot: Longint; var ParamString: String);
type
	CharSet  = Set of Char;
	TFileBuf = Array[0..65535] of Char;
	PFileBuf = ^TFileBuf;

const
	AllowChars	: CharSet = ['0'..'9', 'A'..'Z', 'a'..'z', ' ', '_', '-', '+', '.', ',', '[', ']', '(', ')'];

var
	StartTime,
	EndTime,
	ResumeOfs,
	HdrRec,
	AreaNum,
	GroupNum	: Longint;
	FileName,
	ResStr		: String;
	FilesInf	: FilesRecord;
	GroupInf	: GroupRecord;
	FileMgr 	: MgrFileObj;
	Temp_F		: pFileObj;
	InvalidDir	: Boolean;
	DidRead,
	DidSave 	: Longint;
	TmpBuf		: PFileBuf;

begin
	ResumeOfs := ftpConnectionRec(Connections[CurSlot].SrvData^).ResumeOfs;
	ftpConnectionRec(Connections[CurSlot].SrvData^).ResumeOfs := 0;

	{-- separate ParamString into /group/area/filename -----------------------}
	AreaNum := ftpConnectionRec(Connections[CurSlot].SrvData^).CurArea;
	GroupNum := ftpConnectionRec(Connections[CurSlot].SrvData^).CurGroup;
	ParamString := StripIllegal(Trim(ParamString));

	GetPartsFromDir(ParamString,
	                FileName,
	                CurSlot,
	                InvalidDir,
	                GroupNum,
	                AreaNum);

	if (InvalidDir) OR (AreaNum <= 0) then
		begin
			SendCodeStr(CurSlot, 550); { Requested action not taken }
			EXIT;
		end; { if }

	{-- because that would be silly --}
	if (FileName = ftp_VirtualIndex) then
		FileName := '';

	{-- check FileName is valid ----------------------------------------------}
	if (FilterString(FileName, AllowChars) <> Filename) then
		begin
			FileName := '';
		end; { if }

	{lame.. fix me}
	if (FileName <> '') and ((pos('..', FileName) > 0) or (FileName[1] = '.')) then
		FileName := '';

	{-- and if we end up with an empty filename, it was invalid --------------}
	if FileName = '' then
		begin
			SendCodeStr(CurSlot, 553);
			EXIT;
		end; { if }

	{-- Grab the record for the current file area ----------------------------}
	if (not GetFilesRecord(FilesInf, SmallWord(AreaNum), True)) or (not (GetGroupRecord(SmallWord(GroupNum), True, False, GroupInf))) then
		begin
			RaLog('!', Format('[FTPSERV:%d] [%s] Unable to retrieve file(#%d)/group(#%D) record for upload', [CurSlot, Connections[CurSlot].IpAddr, AreaNum, GroupNum]));

			SendCodeStr(CurSlot, 550); { Requested action not taken }
			EXIT;
		end; { if }

	{-- Check if user has upload access --------------------------------------}
	if (not CheckGroupAccess(GroupInf, ftpConnectionRec(Connections[CurSlot].SrvData^).Exitinfo)) or
		(not CheckFileAreaAccess(FilesInf,
		                         False,     { download		 }
		                         False,     { groupcheck	 } { checks current group from user record? }
		                         True,      { upload check }
		                         False,     { list access  }
		                         0,         { group num, checking is disabled }
		                         ftpConnectionRec(Connections[CurSlot].SrvData^).Exitinfo)) then
		begin
			SendCodeStr(CurSlot, 550); { Requested action not taken }
			exit;
		end;

	{-- Redirect to alternate upload area ------------------------------------}
	if (FilesInf.UploadArea > 0) and (not GetFilesRecord(FilesInf, FilesInf.UploadArea, True)) then
		begin
			RaLog('!', Format('[FTPSERV:%d] [%s] Unable to retrieve upload filearea! AreaNumber %d has ' +
				 ' upload area #%d defined which does not exist!', [CurSlot, Connections[CurSlot].IpAddr, FilesInf.AreaNum, FilesInf.UploadArea]));

			SendCodeStr(CurSlot, 550); { Requested action not taken }
			EXIT;
		end
	else
		AreaNum := FilesInf.AreaNum; { just in case }

	{-- Check if there's enough space to accept an upload --------------------}
	if (GetDiskFree(FilesInf.FilePath[1]) div 1024) < GlobalCfg^.RaConfig^.MinUpSpace then
		begin
			SendCodeStr(CurSlot, 552);
			exit;
		end;

	{-- do some checking on destination file ---------------------------------}
	New(Temp_F, Init);
	Temp_F^.Assign(ForceBack(FilesInf.FilePath) + FileName);
	Temp_F^.FileMode := WriteMode + DenyNone;

	HdrRec := SearchNameInArea(FilesInf.AreaNum, FileName, False);

	{-- never overwrite what isn't ours --------------------------------------}
	if (Temp_F^.Exists) and (HdrRec <= 0) then
		begin
			RaLog('>', Format('[FTPSERV:%d] [%s] Upload rejected - file exists on disk but not in area #%d: %s', [CurSlot, Connections[CurSlot].IpAddr, FilesInf.AreaNum, FileName]));
			SendCodeStr(CurSlot, 550);

			{ Clean up }
			Dispose(Temp_F, Done);
			EXIT;
		end;

	{-- only resume when file exists -----------------------------------------}
	{-- since resumeofs is 0 by default (rather than -1), a non-resuming -----}
	{-- client will be allowed to overwrite the file completely --------------}
	if (HdrRec <= 0) and (not Temp_F^.Exists) and (ResumeOfs > 0) then
		begin
			RaLog('>', Format('[FTPSERV:%d] [%s] Upload rejected - cannot resume non-existant file', [CurSlot, Connections[CurSlot].IpAddr, FilesInf.AreaNum]));
			SendCodeStr(CurSlot, 550);

			{ Clean up }
			Dispose(Temp_F, Done);
			EXIT;
		end;

	{-- check the HDR record --}
	if (HdrRec > 0) then
		begin
			FileMgr.Init(FilesInf.AreaNum, False);
			FileMgr.Read(HdrRec - 1);

			{-- don't allow resume unless by the same user --}
			if SUpCase(FileMgr.CurHdr^.Uploader) <> SUpCase(ftpConnectionRec(Connections[CurSlot].SrvData^).ExitInfo.UserInfo.Name) then
				begin
					Dispose(Temp_F, Done);
					FileMgr.Done;

					RaLog('>', Format('[FTPSERV:%d] [%s] Upload rejected - only original uploader may resume', [CurSlot, Connections[CurSlot].IpAddr, FilesInf.AreaNum]));
					SendCodeStr(CurSlot, 550);
					exit;
				end;

			{-- make sure the resumeofs is sensible --}
			if (ResumeOfs > FileMgr.CurHdr^.Size) then
				begin
					Dispose(Temp_F, Done);
					FileMgr.Done;

					RaLog('>', Format('[FTPSERV:%d] [%s] Upload rejected - resume offset greater than file size', [CurSlot, Connections[CurSlot].IpAddr, FilesInf.AreaNum]));
					SendCodeStr(CurSlot, 550);
					exit;
				end;

			FileMgr.Done;
		end;

	{-- Now try opening the file ---------------------------------------------}
	if Temp_F^.Exists then
		begin
			Temp_F^.Open(1);

			if (ResumeOfs > 0) then
				Temp_F^.Seek(ResumeOfs);
		end
	else
		Temp_F^.Create(1);

	if (Temp_F^.ErrorCode = 0) then
		begin
			{-- open data connections --------------------------------------------}
			if OpenDataConnection(CurSlot, ResStr) then
				begin
					{-- now prepare for the transfer --}
					RaLog('>', Format('[FTPSERV:%d] [%s] Receiving file in area #%d: %s', [CurSlot, Connections[CurSlot].IpAddr, FilesInf.AreaNum, FileName]));
					New(TmpBuf);
					StartTime := NowAsUnixDate;

					{-- we have a working dataconnection, use it as we want ----------}
					DidSave := 0;

					{ XXXTODO: check disk space here? }
					while ftpConnectionRec(Connections[Curslot].SrvData^).DataThread.ConnectionAlive do
						begin
							ftpConnectionRec(Connections[Curslot].SrvData^).DataThread.RecvBuf(TmpBuf^, sizeof(TFileBuf), DidRead);
							if DidRead > 0 then
								begin
									Temp_F^.BlkWrite(TmpBuf^, DidRead);
									DidSave := DidSave + DidRead;

									{ XXXBUGBUG: this loop would otherwise cause high CPU usage, but this is ugly. }
									{ XXXBUGBUG  replace sleep() with recvbuf alternative that fills the buffer if possible }
									ftpConnectionRec(Connections[Curslot].SrvData^).DataThread.DoSleep(1);
								end
							else
								ftpConnectionRec(Connections[Curslot].SrvData^).DataThread.DoSleep(100);
						end; { while }

					{-- and eventually close it again ----------------------------------}
					EndTime := NowAsUnixDate;
					CloseDataConnection(CurSlot, true);
					Dispose(TmpBuf);

					RaLog('>', Format('[FTPSERV:%d] [%s] %d bytes received', [CurSlot, Connections[CurSlot].IpAddr, DidSave]));

					WriteTransferLog(Connections[CurSlot].IpAddr, CurSlot, 'R',
					                 ftpConnectionRec(Connections[CurSlot].SrvData^).UserName,
					                 FilesInf.AreaNum, FileName,
					                 Temp_F^.FileSize, DidSave, ResumeOfs, (EndTime - StartTime));

					{-- update user record ---------------------------------------------}
					LoadUser(CurSlot);
					inc(ftpConnectionRec(Connections[CurSlot].SrvData^).ExitInfo.UserInfo.Uploads);
					inc(ftpConnectionRec(Connections[CurSlot].SrvData^).ExitInfo.UserInfo.UploadsK, DidSave div 1024);
					SaveUser(CurSlot);

					{-- now update the FDB ---------------------------------------------}
					FileMgr.Init(FilesInf.AreaNum, True);

					HdrRec := SearchNameInArea(FilesInf.AreaNum, FileName, False);

					if (HdrRec > 0) then { update existing record }
						begin
							FileMgr.Read(HdrRec - 1);
							FileMgr.CurHdr^.CRC32 := GenFileCRC32(ForceBack(FilesInf.FilePath) + FileName);
							FileMgr.CurHdr^.Size := Temp_F^.FileSize;
							FileMgr.CurHdr^.FileDate := GetPackedFileTime(ForceBack(FilesInf.FilePath) + FileName);
							FileMgr.CurHdr^.UploadDate := GetDosDate;
							FileMgr.CurHdr^.LastDl := GetDosDate;
							FileMgr.WriteHdr(HdrRec - 1, FileMgr.CurHdr^);
						end
					else { make a new one }
						begin
							FillChar(FileMgr.CurHdr^, SizeOf(FileMgr.CurHdr^), #0);
							FileMgr.CurHDr^.CRC32 := -1;
							FileMgr.CurHdr^.LfnPtr := FileMgr.AddLfnPtr(FileName, ForceBack(FilesInf.FilePath) + FileName, FileMgr.CurHdr^.Name);
							FileMgr.CurHdr^.LongDescPtr := -1;
							FileMgr.CurHdr^.Size := Temp_F^.FileSize;
							FileMgr.CurHdr^.Uploader := ftpConnectionRec(Connections[CurSlot].SrvData^).ExitInfo.UserInfo.Name;
							FileMgr.CurHdr^.FileDate := GetPackedFileTime(FilesInf.FilePath + FileName);
							FileMgr.CurHdr^.UploadDate := GetDosDate;
							FileMgr.CurHdr^.LastDl := GetDosDate;
							FileMgr.AddHeader(FileMgr.CurHdr^);
						end; { HdrRec > 0 }

					FileMgr.Done;
					Temp_F.Close;

					{-- file descriptions are imported in another thread; let it know it's got work to do }
					if ftp_ImportDiz then
						DizImportThread^.SubmitJob(FilesInf.AreaNum, FileName);

				end; { if OpenDataConnection }
		end { Temp_F^.ErrorCode = 0 }
	else
		begin
			RaLog('!', Format('[FTPSERV:%d] [%s] Upload aborted - unable to create/open file', [CurSlot, Connections[CurSlot].IpAddr]));
			SendCodeStr(CurSlot, 450); { Requested fileaction not taken }
		end;

	{-- cleanup --------------------------------------------------------------}
	Dispose(Temp_F, Done);
end; { proc. cmdStor }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdRetr(const CurSlot: Longint; var ParamString: String);
type
	TFileBuf = Array[0..4095] of Char;
	PFileBuf = ^TFileBuf;

var
	ResStr		: String;
	WildCardStr : String;
	ListGroup	: Longint;
	ListArea	: Longint;

	FilesInf	: FilesRecord;
	FilesHdr	: FilesHdrRecord;

	Temp_F		: pFileObj;
	InvalidDir	: Boolean;
	DidRead 	: Longint;
	Counter 	: Longint;
	DidSent 	: Longint;
	TotalBytes	: Longint;
	TmpBuf		: PFileBuf;
	zResumeOfs	: Longint;         { the z is avoid a conflict while in a "with ftpConnectionRec" clause }
	HdrRec		: Longint;
	FileMgr 	: MgrFileObj;

	StartTime,
	EndTime 	: LongInt;

begin
  {-- do this here so we can quit at any time ------------------------------}
  zResumeOfs := ftpConnectionRec(Connections[CurSlot].SrvData^).ResumeOfs;
  ftpConnectionRec(Connections[CurSlot].SrvData^).ResumeOfs := 0;

  {-- we default to the current group/area listing -------------------------}
  ListArea := ftpConnectionRec(Connections[CurSlot].SrvData^).CurArea;
  ListGroup := ftpConnectionRec(Connections[CurSlot].SrvData^).CurGroup;

  {-- make sure we can retrieve the file etc, then actually send it --------}
  with ftpConnectionRec(Connections[Curslot].SrvData^) do
    begin
      {-- in case someone uses spaces instead of the virtual underscores ---}
      for Counter := 1 to Length(ParamString) do
        if ParamString[Counter] = ' ' then
           ParamString[Counter] := '_';

      {-- Break it up ------------------------------------------------------}
      GetPartsFromDir(ParamString,
                      WildCardStr,
                      CurSlot,
                      InvalidDir,
                      ListGroup,
                      ListArea);

      if (InvalidDir) OR (ListArea <= 0) then
        begin
          SendCodeStr(CurSlot, 550); { Requested action not taken }
          EXIT;
        end; { if }

      {-- Get the file path ------------------------------------------------}
      GetFilesRecord(FilesInf, SmallWord(ListArea), true);

      {-- Virtual index support; check the LIST permissions, not DOWNLOAD --}
      if (ftp_VirtualIndex <> '') and (ConvDirName(ftp_VirtualIndex) = WildCardStr) then
        begin
          if CheckFileAreaAccess(FilesInf,
                                 False,          { download }
                                 False,          { groupcheck }
                                 False,          { upload check }
                                 True,           { list access }
                                 SmallWord(ListGroup),
                                 ExitInfo) then
            begin
              if OpenDataConnection(CurSlot, ResStr) then
                begin
                  SendVirtualIndex(CurSlot, FilesInf.AreaNum, zResumeOfs);
                  CloseDataConnection(CurSlot, true);
                end;
            end
          else
            SendCodeMsg(CurSlot, 450, 'Area not available - security violation');

          EXIT;
        end;

      {-- if we have access to this group ----------------------------------}
      if NOT CheckFileAreaAccess(FilesInf,
                                 TRUE,                       { download }
                                 FALSE,                    { groupcheck }
                                 FALSE,                  { upload check }
                                 FALSE,                   { list access }
                                 SmallWord(ListGroup),
                                 ExitInfo) then
        begin
          SendCodeMsg(CurSlot, 450, 'Area not available - security violation');
          EXIT;
        end; { if }

      {-- find the true filename so we can open it later -------------------}
      WildcardStr := SearchBjorkedName(FilesInf.AreaNum, WildcardStr, Dos.Anyfile - Dos.VolumeID);

      {-- get the file record ----------------------------------------------}
      HdrRec := SearchNameInArea(FilesInf.AreaNum, WildCardStr, False);

      if (HdrRec < 0) then
        begin
          SendCodeStr(CurSlot, 550);
          exit;
        end;

      FileMgr.Init(FilesInf.AreaNum, False);
      FileMgr.Read(HdrRec - 1);
      FilesHdr := FileMgr.CurHdr^;
      FileMgr.Done;

      {-- check file record attributes -------------------------------------}
      if ReadBit(FilesHdr.Attrib, 0) or   { deleted }
         ReadBit(FilesHdr.Attrib, 3) or   { not available }
         ReadBit(FilesHdr.Attrib, 5) or   { missing/offline }
         (FilesHdr.Password <> '') then   { password protected }
        begin
          SendCodeMsg(CurSlot, 450, 'File not available - security violation');
          exit;
        end; { if }

      {-- check user limits ------------------------------------------------}
      if (not ReadBit(FilesHdr.Attrib, 2)) { not free file } and (not ReadBit(FilesInf.Attrib, 4)) then { not free area }
        begin

          { daily limit -- we'll use the local limit }
          if (ftpConnectionRec(Connections[CurSlot].SrvData^).LimitInfo.LLocal <> UnlimitedValue) and
             ((ftpConnectionRec(Connections[CurSlot].SrvData^).ExitInfo.UserInfo.TodayK + (FilesHdr.Size div 1024)) >
               (ftpConnectionRec(Connections[CurSlot].SrvData^).LimitInfo.LLocal)) then
            begin
              SendCodeMsg(CurSlot, 450, 'Download limit (daily) exceeded');
              EXIT;
            end; { if }

          { count ratio }
          if (ftpConnectionRec(Connections[CurSlot].SrvData^).LimitInfo.RatioNum > 0) and
             ((ftpConnectionRec(Connections[CurSlot].SrvData^).ExitInfo.UserInfo.Downloads + 1) >
               (ftpConnectionRec(Connections[CurSlot].SrvData^).ExitInfo.UserInfo.Uploads * ftpConnectionRec(Connections[CurSlot].SrvData^).LimitInfo.RatioNum + 1)) then
            begin
              SendCodeMsg(CurSlot, 450, 'Download ratio (count) exceeded');
              EXIT;
            end; { if }

          { bytes ratio }
          if (ftpConnectionRec(Connections[CurSlot].SrvData^).LimitInfo.RatioK > 0) and
             ((ftpConnectionRec(Connections[CurSlot].SrvData^).ExitInfo.UserInfo.DownloadsK + (FilesHdr.Size div 1024)) >
               (ftpConnectionRec(Connections[CurSlot].SrvData^).ExitInfo.UserInfo.UploadsK * ftpConnectionRec(Connections[CurSlot].SrvData^).LimitInfo.RatioK + 1)) then
            begin
              SendCodeMsg(CurSlot, 450, 'Download ratio (bytes) exceeded');
              EXIT;
            end; { if }
        end; { check user limits }


      {-- make sure we have download access --------------------------------}
      if WildcardStr <> '' then
        begin
          {-- Write the debugging info -------------------------------------}
          {$IFDEF WITH_DEBUG}
            DebugObj.DebugLog(logString, 'cmd-Retr: '+ WildcardStr);
          {$ENDIF}

          {-- Now try opening the file -------------------------------------}
          New(Temp_F, Init);
          Temp_F^.Assign(FilesInf.FilePath + StripIllegal(WildCardStr));
          Temp_F^.FileMode := ReadMode + DenyNone;

          if Temp_F^.Open(1) and (Temp_F^.FileSize >= zResumeOfs) then
            begin
              Temp_F^.Seek(zResumeOfs);

              {-- open data connections ------------------------------------}
              if OpenDataConnection(CurSlot, ResStr) then
                begin
                  {-- now prepare for the transfer --}
                  RaLog('>', Format('[FTPSERV:%d] [%s] Sending file from area #%d: %s', [CurSlot, Connections[CurSlot].IpAddr, FilesInf.AreaNum, WildCardStr]));
                  New(TmpBuf);
                  TotalBytes := 0;
                  StartTime := NowAsUnixDate;

                  {-- we have a working dataconnection, use it as we want --}
                  while (NOT Temp_F^.EOF) AND (DataThread.ConnectionAlive) do
                    begin
                      {-- read the required data ---------------------------}
                      DidRead := Temp_F^.BlkRead(TmpBuf^, SizeOf(TFileBuf));

                      {-- create a small loop to ensure we send everything -}
                      Counter := 0;

                      while (Counter < DidRead) AND (DataThread.ConnectionAlive) do
                        begin
                          DataThread.SendBuf(TmpBuf^[Counter], (DidRead - Counter), DidSent);

                          if DidSent > 0 then
                            begin
                              Inc(Counter, DidSent);
                              Inc(TotalBytes, DidSent);
                            end
                          else
                            DataThread.DoSleep(100);
                        end; { while }

                    end; { while }

                  {-- and eventually close it again ------------------------}
                  CloseDataConnection(CurSlot, true);

                  EndTime := NowAsUnixDate;
                  Dispose(TmpBuf);
                  RaLog('>', Format('[FTPSERV:%d] [%s] %d bytes sent', [CurSlot, Connections[CurSlot].IpAddr, TotalBytes]));

                  WriteTransferLog(
                      Connections[CurSlot].IpAddr, CurSlot, 'T',
                      ftpConnectionRec(Connections[CurSlot].SrvData^).UserName,
                      FilesInf.AreaNum, WildCardStr,
                      Temp_F^.FileSize, TotalBytes, zResumeOfs, (EndTime - StartTime));

                  {-- update the user's record -----------------------------}
                  if (not ReadBit(FilesHdr.Attrib, 2)) { not free file } and (not ReadBit(FilesInf.Attrib, 4)) then { not free area }
                    begin
                      LoadUser(CurSlot);
                      inc(ftpConnectionRec(Connections[CurSlot].SrvData^).ExitInfo.UserInfo.Downloads);
                      inc(ftpConnectionRec(Connections[CurSlot].SrvData^).ExitInfo.UserInfo.DownloadsK, TotalBytes div 1024);
                      inc(ftpConnectionRec(Connections[CurSlot].SrvData^).ExitInfo.UserInfo.TodayK, TotalBytes div 1024);
                      SaveUser(CurSlot);
                    end;
                end; { if }
            end { file open }
              else SendCodeStr(CurSlot, 450);

          {-- and close the file -------------------------------------------}
          Dispose(Temp_F, Done);
        end { if }
         else begin
                SendCodeStr(CurSlot, 450);
              end; { file is not available (security) }
    end; { with }
end; { proc. cmdRetr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetPwdStr(CurSlot: Longint): String;
var CurDir    : String;
    GroupInf  : GroupRecord;
    FilesInf  : FilesRecord;
begin
  {-- default to root location ---------------------------------------------}
  CurDir := '/';

  if ftpConnectionRec(Connections[CurSlot].SrvData^).CurGroup > 0 then
    begin
      {-- now get the current directory for the group ----------------------}
      GetGroupRecord(ftpConnectionRec(Connections[CurSlot].SrvData^).CurGroup, true, false, GroupInf);
      CurDir := CurDir + ConvDirName(GroupInf.Name) + '/';

      {-- and if we are in an area, add that too ---------------------------}
      if ftpConnectionRec(Connections[CurSlot].SrvData^).CurArea > 0 then
        begin
          GetFilesRecord(FilesInf, ftpConnectionRec(
              Connections[CurSlot].SrvData^).CurArea, true);

          CurDir := CurDir + ConvDirName(FilesInf.Name) + '/';
        end; { if }
    end; { if }

  {-- and return the string ------------------------------------------------}
  GetPwdStr := CurDir;
end; { func. GetPwdStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdPwd(const CurSlot: Longint; var ParamString: String);
begin
  {-- and send the result --------------------------------------------------}
  SendCodeMsg(CurSlot, 257, Format('"%s" is your current location', [GetPwdStr(CurSlot)]));
end; { proc. cmdPwd }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdMode(const CurSlot: Longint; var ParamString: String);
begin
  if SupCase(Trim(ParamString)) = 'S' then
    begin
      SendCodeMsg(CurSlot, 200, 'mode S ok');
    end
      else SendCodeMsg(CurSlot, 504, 'Please use S(tream) mode');
end; { proc. CmdMode }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdType(const CurSlot: Longint; var ParamString: String);
begin
  if (SupCase(Trim(ParamString)) = 'I') OR (SupCase(Trim(ParamString)) = 'A') then
    begin
      SendCodeMsg(CurSlot, 200, 'mode I (binary) ok');
    end
      else SendCodeMsg(CurSlot, 504, 'Please use I (binary) mode');
end; { proc. CmdMode }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdStru(const CurSlot: Longint; var ParamString: String);
begin
  if SupCase(Trim(ParamString)) = 'F' then
    begin
      SendCodeMsg(CurSlot, 200, 'F OK');
    end
      else SendCodeMsg(CurSlot, 504, 'Please use F (file) mode');
end; { proc. CmdStru }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdSmnt(const CurSlot: Longint; var ParamString: String);
begin
  SendCodeStr(CurSlot, 502);
end; { proc. cmdSmnt }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdappe(const CurSlot: Longint; var ParamString: String);
begin
  SendCodeStr(CurSlot, 502);
end; { proc. cmdAppe }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdOpts(const CurSlot: Longint; var ParamString: String);
begin
  SendCodeStr(CurSlot, 502);
end; { proc. cmdOpts }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdNoop(const CurSlot: Longint; var ParamString: String);
begin
  SendCodeMsg(CurSlot, 200, '...');
end; { proc. cmdNoop }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdUser(const CurSlot: Longint; var ParamString: String);
var UserExt  : userExtensionRecord;
begin
  ftpConnectionRec(Connections[CurSlot].SrvData^).UserName := '';
  ftpConnectionRec(Connections[CurSlot].SrvData^).PasswordCRC := -1;
  ftpConnectionRec(Connections[CurSlot].SrvData^).Validated := false;
  ftpConnectionRec(Connections[CurSlot].SrvData^).Anonymous := false;

  {-- Allow for login name translation before scanning userbase --}
  ParamString := SUpCase(Trim(ParamString));
  if (ftp_AllowAnonymous) and (ParamString = 'FTP') then
    ParamString := 'ANONYMOUS';

  ftpConnectionRec(Connections[CurSlot].SrvData^).UserRec := SearchUser(ParamString);

  if ftpConnectionRec(Connections[CurSlot].SrvData^).UserRec >= 0 then
    begin
      {-- retrieve the users password --------------------------------------}
      GetUserRecord(ftpConnectionRec(Connections[CurSlot].SrvData^).UserRec,
                    ftpConnectionRec(Connections[CurSlot].SrvData^).Exitinfo.UserInfo,
                    UserExt,
                    false);

      ftpConnectionRec(Connections[CurSlot].SrvData^).UserName := ftpConnectionRec(Connections[CurSlot].SrvData^).ExitInfo.UserInfo.Name;

      {-- Substitute username when hostnames are disabled --}
      if NoDNSLookup then
        Connections[CurSlot].Name := ftpConnectionRec(Connections[CurSlot].SrvData^).UserName;

      {-- Check for anonymous access --}
      if (ftp_AllowAnonymous) AND (SUpCase(ftpConnectionRec(Connections[CurSlot].SrvData^).UserName) = 'ANONYMOUS') then
        ftpConnectionRec(Connections[CurSlot].SrvData^).Anonymous := true;

      {-- Reject Guest users --}
      if (NOT ftp_AllowAnonymous) AND (ReadBit(ftpConnectionRec(Connections[CurSlot].SrvData^).Exitinfo.UserInfo.Attribute2, 6)) then
        begin
          RaLog('>', Format('[FTPSERV:%d] [%s] Access denied: Guest users not allowed unless Anonymous logins enabled.', [CurSlot, Connections[CurSlot].IpAddr]));
          ftpConnectionRec(Connections[CurSlot].SrvData^).UserRec := -1;
        end;

      {-- Reject sysop logins --}
      if (NOT GlobalCfg^.RaConfig^.AllowSysRem)
          AND ((SUpcase(ftpConnectionRec(Connections[CurSlot].SrvData^).ExitInfo.UserInfo.Name) = SUpcase(GlobalCfg^.RaConfig^.SysOp))
            OR (SUpcase(ftpConnectionRec(Connections[CurSlot].SrvData^).ExitInfo.UserInfo.Handle) = SUpcase(GlobalCfg^.RaConfig^.Sysop)))
          AND (Connections[CurSlot].IpAddr <> '127.0.0.1') then
        begin
          RaLog('>', Format('[FTPSERV:%d] [%s] Access denied: remote sysop logins have been disabled.', [CurSlot, Connections[CurSlot].IpAddr]));
          ftpConnectionRec(Connections[CurSlot].SrvData^).UserRec := -1;
        end; { AllowSysRem }

    end; { else }

  {-- we always ask for a password to not give away our userlist ;) --------}
  if ftpConnectionRec(Connections[CurSlot].SrvData^).Anonymous then
    SendCodeMsg(CurSlot, 331, 'User name, okay. Please send email address.')
  else
    SendCodeMsg(CurSlot, 331, 'User name, okay. Please send password.');
end; { proc. cmdUser }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdPass(const CurSlot: Longint; var ParamString: String);
var
  CurDate,
  CurTime: String;
begin
  {-- if a username was entered, try validating the password ---------------}
  if ftpConnectionRec(Connections[CurSlot].SrvData^).UserRec >= 0 then
    begin
      {-- Default to not validated -----------------------------------------}
      ftpConnectionRec(Connections[CurSlot].SrvData^).Validated := false;

      {-- calculcate the given CRC -----------------------------------------}
      ftpConnectionRec(Connections[CurSlot].SrvData^).PasswordCRC := RaCrc(ParamString, true);

      {-- first check for anonymous account --}
      if (ftpConnectionRec(Connections[CurSlot].SrvData^).Anonymous) then
        begin
          RaLog('>', Format('[FTPSERV:%d] [%s] Anonymous (%s) on-line', [CurSlot, Connections[CurSlot].IpAddr, ParamString]));
          ftpConnectionRec(Connections[CurSlot].SrvData^).Validated := true
        end; { anonymous login }

      {-- otherwise validate with the given password -----------------------}
      if (NOT ftpConnectionRec(Connections[CurSlot].SrvData^).Validated) then
        begin
          if (ftpConnectionRec(Connections[CurSlot].SrvData^).PasswordCRC = ftpConnectionRec(Connections[CurSlot].SrvData^).Exitinfo.UserInfo.PasswordCRC) then
            begin
              RaLog('>', Format('[FTPSERV:%d] [%s] %s on-line', [CurSlot, Connections[CurSlot].IpAddr, ftpConnectionRec(Connections[CurSlot].SrvData^).UserName]));
              ftpConnectionRec(Connections[CurSlot].SrvData^).Validated := true
            end; {  normal login }
        end; { if }

      { if the connection is not validated, abort -------------------------}
      if (NOT ftpConnectionRec(Connections[CurSlot].SrvData^).Validated) then
        begin
          ftpConnectionRec(Connections[CurSlot].SrvData^).UserName := '';
          ftpConnectionRec(Connections[CurSlot].SrvData^).UserRec := 0;
          ftpConnectionRec(Connections[CurSlot].SrvData^).Validated := false;

          if NoDNSLookup then
            Connections[CurSlot].Name := '';
        end; { failed login }

      {-- and the user has logged on ---------------------------------------}
      if ftpConnectionRec(Connections[CurSlot].SrvData^).Validated then
        begin
          SendCodeMsg(CurSlot, 230, 'User logged in, proceed.');

          {-- update daily user fields -------------------------------------}
          LoadUser(CurSlot); { this also loads the LIMITS record }

          CurDate := DateStr;
          CurTime := TimeStr(False, False);

          if PackTimeStr(CurDate, '00:00') > PackTimeStr(ftpConnectionRec(Connections[CurSlot].SrvData^).Exitinfo.UserInfo.LastDate, '00:00') then
             ftpConnectionRec(Connections[CurSlot].SrvData^).Exitinfo.UserInfo.TodayK := 0;

          ftpConnectionRec(Connections[CurSlot].SrvData^).Exitinfo.UserInfo.LastDate := CurDate;
          ftpConnectionRec(Connections[CurSlot].SrvData^).Exitinfo.UserInfo.LastTime := CurTime;
          ftpConnectionRec(Connections[CurSlot].SrvData^).LogOnTime := CurTime;

          SaveUser(CurSlot);
          UpdateUserOn(CurSlot);
        end;
    end; { if }

  {-- if the username was not sent, or was invalid deny immedaitly ---------}
  if NOT ftpConnectionRec(Connections[CurSlot].SrvData^).Validated then
    begin
      SendCodeMsg(CurSlot, 530, 'User cannot log in');
    end; { if }
end; { proc. cmdPass }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdSyst(const CurSlot: Longint; var ParamString: String);
begin
  SendCodeMsg(CurSlot, 215, 'UNIX');
end; { proc. cmdSyst }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdDele(const CurSlot: Longint; var ParamString: String);
begin
  SendcodeStr(CurSlot, 502);
end; { proc. cmdDele }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdAbor(const CurSlot: Longint; var ParamString: String);
begin
  SendcodeStr(CurSlot, 502);
end; { proc. cmdabor }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdSite(const CurSlot: Longint; var ParamString: String);
begin
  SendCodeMsg(CurSlot, 500, 'No SITE commands implemented here');
end; { proc. cmdSite }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdPasv(const CurSlot: Longint; var ParamString: String);
var
	SrvData 	: pFtpConnectionRec;
	Counter 	: Byte;
	ErrorStr	: String;
	DataPort	: Word;
	DataAddr	: tSockAddr;
	TempStr		: String;
	IpFile  	: pFileObj;
	IpStr   	: String;
	ipDotCounter: Integer;

begin
	SrvData := Connections[CurSlot].SrvData;

	{-- attempt to bind to random ports until one succeeds --}
	for Counter := 1 to 3 do
		begin
			{ XXXFIXME: this will be a problem for slow CPUs and high, narrow port ranges; sleep a bit just in case }
			repeat
				DataPort := SmallWord(Random(ftp_PasvPorts[2]));
				SrvData^.DataThread.DoSleep(1);
			until (DataPort >= ftp_PasvPorts[1]);

			if SrvData^.DataThread.SetupServer(DataPort, ErrorStr, InAddr_Any)
				then break
				else DataPort := 0;
		end; { for }

	{-- inform the client of the selected port if successful, else send an error code --}
	if (DataPort = 0) then
		begin
			SrvData^.IsPassive := False;
			SendCodeStr(CurSlot, 425);
		end
	else
		begin
			SrvData^.IsPassive := True;
			SrvData^.DataThread.Blocking := False;
			SockGetSockAddr(SrvData^.FtpSession.SockHandle, DataAddr);

			if (ftp_DynIp <> '') then
				begin
					New(IpFile, Init);
	
					if (IpFile <> nil) then
						begin
							IpFile^.Assign(ftp_DynIp);
							IpFile^.FileMode := ReadMode + DenyNone;
		
							if IpFile^.Open(1) then
								begin
									IpFile^.ReadLn(IpStr);
									IpFile^.Close;					
								end;
							
							IpStr := Trim(IpStr);
	
							{$IFDEF WITH_DEBUG}
								DebugObj.DebugLog(logString, 'IpFile: |'+IpStr+'|');
							{$ENDIF}		
		
							Dispose(IpFile, Done);
		    			end; { IF IpFile <> nil }
					
	 				for ipDotCounter := 1 to 4 do
						ftp_PasvSrvIP[ipDotCounter] := Byte(FVal(ExtractWord(IpStr, ipDotCounter, ['.'], False, False)));

				end; {if ftp_DynIp <> ''}
				
			{ override address if PasvSrvIP is non-zero and we're not on the loopback interface }
			if (ftp_PasvSrvIP[1] = 0) or (DataAddr.Sin_Addr.ClassA = 127) then
				TempStr := Format('%d,%d,%d,%d', [DataAddr.Sin_Addr.ClassA, DataAddr.Sin_Addr.ClassB, DataAddr.Sin_Addr.ClassC, DataAddr.Sin_Addr.ClassD])
			else
				TempStr := Format('%d,%d,%d,%d', [ftp_PasvSrvIP[1], ftp_PasvSrvIP[2], ftp_PasvSrvIP[3], ftp_PasvSrvIP[4]]);

			DataPort := DataPort + ftp_PasvOffset;
			TempStr := Format('Entering Passive Mode (%s,%d,%d)', [TempStr, Hi(DataPort), Lo(DataPort)]);

			SendCodeMsg(CurSlot, 227, TempStr);
		end; { NOT (DataPort = 0) }
end; { proc. cmdPasv }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdStat(const CurSlot: Longint; var ParamString: String);
begin
  if Trim(ParamString) = '' then
    begin
      SendCodeMsg(CurSlot, 211, Format(ServerName, [GlobalCfg^.RaConfig^.SystemName]) + ' ready.');
    end
      else begin
             {-- send the directory listing over the wire ... --------------}
             ftpConnectionRec(Connections[CurSlot].SrvData^).UseDataConn := TRUE;

             {-- send the filelisting --------------------------------------}
             cmdSendList(CurSlot, ParamString, FALSE);

             {-- and revert to normal operation ----------------------------}
             ftpConnectionRec(Connections[CurSlot].SrvData^).UseDataConn := FALSE;
           end; { else }
end; { proc. cmdStat }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdCdUp(const CurSlot: Longint; var ParamString: String);
begin
  {-- go one directory level up --------------------------------------------}
  if ftpConnectionRec(Connections[CurSlot].SrvData^).CurArea > 0 then
    begin
      ftpConnectionRec(Connections[CurSlot].SrvData^).CurArea := -1;
    end { if }
      else ftpConnectionRec(Connections[CurSlot].SrvData^).CurGroup := -1;

  {-- return the current directory -----------------------------------------}
  cmdPwd(CurSlot, ParamString);
end; { proc. cmdCdUp }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdCwd(const CurSlot: Longint; var ParamString: String);
var WildCardStr : String;

    ListArea,
    ListGroup   : Longint;
    InvalidDir  : Boolean;
begin
  {-- Initialize some variables --------------------------------------------}
  ListArea := ftpConnectionRec(Connections[CurSlot].SrvData^).CurArea;
  ListGroup := ftpConnectionRec(Connections[CurSlot].SrvData^).CurGroup;

  {-- basic support for relative path traversal ".."
      XXXFIXME: this would be better off in BreakUpDirStr or GetPartsFromDir
                but I couldn't be stuffed ;) }
  if (ParamString = '..') then
    begin
      {if ListArea = -1 then
        ftpConnectionRec(Connections[CurSlot].SrvData^).CurGroup := -1
      else
        ftpConnectionRec(Connections[CurSlot].SrvData^).CurArea := -1;

      SendCodeMsg(CurSlot, 250, Format('Changed directory to "%s"', [GetPwdStr(CurSlot)]));}
      cmdCdUp(CurSlot, ParamString);
      EXIT;
    end;

  {-- Break the string up in sevearl parts ---------------------------------}
  GetPartsFromDir(ParamString,
                  WildCardStr,
                  CurSlot,
                  InvalidDir,
                  ListGroup,
                  ListArea);

  {-- now set the current directory ----------------------------------------}
  if NOT InvalidDir then
    begin
      ftpConnectionRec(Connections[CurSlot].SrvData^).CurArea := ListArea;
      ftpConnectionRec(Connections[CurSlot].SrvData^).CurGroup := ListGroup;

      {-- and show the directory we changed to -----------------------------}
      SendCodeMsg(CurSlot, 250, Format('Changed directory to "%s"', [GetPwdStr(CurSlot)]));
    end
      else SendCodeStr(CurSlot, 550);
end; { proc. cmdCwd }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure cmdQuit(const CurSlot: Longint; var ParamString: String;
                  var AbortSession: Boolean);
begin
  {-- goodbye --------------------------------------------------------------}
  SendCodeStr(CurSlot, 221);

  {-- andw e allow this to close -------------------------------------------}
  AbortSession := TRUE;
end; { proc. cmdQuit }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ftp_ExecuteConnection(P: pointer): Longint;
var CurSlot      : Longint;          { Current selected slot of information }
    AbortSession : Boolean;          { Indicates wether to stop his session }

    ATempStr     : AnsiString;                   { String used for commands }
    TempStr      : String;    { Convert it to a "short" string for handling }
    CommandStr   : String;                                 { Actual command }
    ParamString  : String;                       { String of all parameters }
    FtpCommand   : FtpCommandSet;                      { Command in an enum }

    StartTime    : EventTimer;  { Make sure this session wont idle too long }
begin
  {-- Initialize some variables ---------------------------------------------}
  CurSlot := Longint(p);
  Inc(CurrentSessions);
  Inc(TotalSessions);

  {-- Make sure this session wont abort immediatly --------------------------}
  AbortSession := false;

  {-- Create seperate FTP-client for this user ------------------------------}
  with ftpConnectionRec(Connections[Curslot].SrvData^) do
    begin
      FtpSession := TTcpServer.Create;
      FtpSession.SockHandle := Connections[CurSlot].ClientRC;
    end; { with }

  {-- Start a timing thread to avoid idling too long ------------------------}
  NewTimer(StartTime, Secs2Tics(SecsTimeOut));

  {-- Send an "HELLO" to the user -------------------------------------------}
  with ftpConnectionRec(Connections[Curslot].SrvData^) do
    SendCodeStr(CurSlot, 220);

  {-- Start the loop that receives commands an processes them ---------------}
  repeat
    if ftpConnectionRec(Connections[CurSlot].SrvData^).FtpSession.RecvStrLn(ATempStr, false) then
     with ftpConnectionRec(Connections[Curslot].SrvData^) do
       begin
         TempStr := ATempStr;

         {-- If an string is entered, process it ------------------------------}
         if TempStr <> '' then
          begin
            {-- Reset the timer -----------------------------------------------}
            NewTimer(StartTime, Secs2Tics(SecsTimeOut));

            {-- Write the debugging info --------------------------------------}
            {$IFDEF WITH_DEBUG}
              DebugObj.DebugLog(logString, 'Command: '+TempStr);
            {$ENDIF}

            {-- Extract the exact command and default to "error" --------------}
            CommandStr := SUpCase(FirstWord(TempStr, defExtractword, false));
            ftpCommand := ftpError;

            {-- Extract the parameters ----------------------------------------}
            ParamString := Trim(TempStr);

            {-- Translate the string to a set code ----------------------------}
            Case CommandStr[1] of
              'A' : if CommandStr = 'ALLO' then FtpCommand := ftpAllo
                      else if CommandStr = 'APPE' then FtpCommand := ftpAppe
                       else if CommandStr = 'ABORT' then FtpCommand := ftpAbor;
              'H' : if CommandStr = 'HELP' then FtpCommand := ftpHelp;
              'O' : if CommandStr = 'OPTS' then FtpCommand := ftpOpts;
              'Q' : if CommandStr = 'QUIT' then FtpCommand := ftpQuit;
              'D' : if CommandStr = 'DELE' then FtpCommand := ftpDele;
              'P' : if CommandStr = 'PORT' then FtpCommand := ftpPort
                      else if CommandStr = 'PWD' then FtpCommand := ftppwd
                       else if CommandStr = 'PASV' then FtpCommand := ftpPasv
                        else if CommandStr = 'PASS' then FtpCommand := ftpPass;
              'L' : if CommandStr = 'LIST' then FtpCommand := ftpList;
              'C' : if CommandStr = 'CDUP' then FtpCommand := ftpCdUp
                     else if CommandStr = 'CWD' then FtpCommand := ftpCwd;
              'S' : if CommandStr = 'SMNT' then FtpCommand := ftpSmnt
                     else if CommandStr = 'STRU' then FtpCommand := ftpStru
                      else if CommandStr = 'SYST' then FtpCommand := ftpSyst
                       else if CommandStr = 'SITE' then FtpCommand := ftpSite
                        else if CommandStr = 'STAT' then ftpCommand := ftpStat
                         else if CommandStr = 'STOR' then ftpCommand := ftpStor;
              'T' : if CommandStr = 'TYPE' then FtpCommand := ftpType;
              'R' : if CommandStr = 'RETR' then FtpCommand := ftpRetr
                     else if CommandStr = 'RNFR' then FtpCommand := ftpRnFr
                       else if CommandStr = 'RNTO' then FtpCommand := ftpRnTo
                        else if CommandStr = 'RMD' then FtpCommand := ftpRmd
                         else if CommandStr = 'REST' then FtpCommand := ftpRest;
              'U' : if CommandStr = 'USER' then FtpCommand := ftpUser;
              'N' : if CommandStr = 'NLST' then FtpCommand := ftpNlst
                      else if CommandStr = 'NOOP' then FtpCommand := ftpNoop;
              'M' : if CommandStr = 'MKD' then FtpCommand := ftpMkd
                      else If CommandStr = 'MODE' then FtpCommand := ftpMode;
            end; { case }

            {-- if the user is already logged in, resync their record first so --}
            {-- that any security changes are noticed and if their record is ----}
            {-- gone next bit will boot them off --------------------------------}
            if (ftpConnectionRec(Connections[CurSlot].SrvData^).Validated) then
              LoadUser(CurSlot);

            {------------------- Execute the command ----------------------------}
            if (ftpConnectionRec(Connections[CurSlot].SrvData^).Validated) OR
                (FtpCommand = ftpUser) OR (FtpCommand = ftpPass) then
                 begin
                   Case FtpCommand of
                     ftpAllo     : cmdAllo(CurSlot, ParamString);
                     ftpAbor     : cmdAbor(CurSlot, ParamString);
                     ftpHelp     : cmdHelp(CurSlot, ParamString);
                     ftpQuit     : cmdQuit(CurSlot, ParamString, AbortSession);
                     ftpList     : cmdSendList(CurSlot, ParamString, FALSE);
                     ftpMkd      : cmdMkd(CurSlot, ParamString);
                     ftpRmd      : cmdRmd(CurSlot, ParamString);
                     ftpAppe     : cmdAppe(CurSlot, ParamString);
                     ftpRetr     : cmdRetr(CurSlot, ParamString);
                     ftpStor     : cmdStor(CurSlot, ParamString);
                     ftpPort     : cmdPort(CurSlot, ParamString);
                     ftpSite     : cmdSite(CurSlot, ParamString);
                     ftpNlst     : cmdSendList(CurSlot, ParamString, TRUE);
                     ftpRest     : cmdRest(CurSlot, ParamString);
                     ftpSmnt     : cmdSmnt(CurSlot, ParamString);
                     ftpPass     : cmdPass(CurSlot, ParamString);
                     ftpPasv     : cmdPasv(CurSlot, ParamString);
                     ftpSyst     : cmdSyst(CurSlot, ParamString);
                     ftpOpts     : cmdOpts(CurSlot, ParamString);
                     ftpStru     : CmdStru(CurSlot, ParamString);
                     ftpRnFr     : CmdRnFr(CurSlot, ParamString);
                     ftpRnTo     : CmdRnFr(CurSlot, ParamString);
                     ftpNoop     : cmdNoop(CurSlot, ParamString);
                     ftpType     : cmdType(CurSlot, ParamString);
                     ftpStat     : cmdStat(CurSlot, ParamString);
                     ftpDele     : cmdDele(CurSlot, ParamString);
                     ftpUser     : cmdUser(CurSlot, ParamString);
                     ftpMode     : cmdMode(CurSlot, ParamString);
                     ftpCdup     : cmdCdUp(CurSlot, ParamString);
                     ftpCwd      : cmdCwd(CurSlot, ParamString);
                     ftpPwd      : cmdPwd(CurSlot, ParamString);
                     ftpError    : begin
                                     RaLog('>', Format('[FTPSERV:%d] [%s] Bad formed command (%s)', [CurSlot, Connections[CurSlot].IpAddr, TempStr]));
                                     SendCodeStr(CurSlot, 500);
                                   end; { error }
                   end { case }
                 end { if authenticated }
                   else begin
                          {-- user is not authenticated - let him auth first-}
                          SendPleaseAuthFirst(CurSlot);
                        end; { if }
          end { if }
       end { with }
        else ftpConnectionRec(Connections[CurSlot].SrvData^).FtpSession.DoSleep(1);

  until (AbortSession) OR (NOT ftpConnectionRec(Connections[CurSlot].SrvData^).FtpSession.ConnectionAlive)
         OR (TimerExpired(StartTime));

  {-- If this session idle timed out, report this ---------------------------}
  if (TimerExpired(StartTime)) then
    begin
      RaLog('>', Format('[FTPSERV:%d] [%s] Connection timedout', [CurSlot, Connections[CurSlot].IpAddr]));
    end; { if }

  {-- Close down the connection ---------------------------------------------}
  with ftpConnectionRec(Connections[Curslot].SrvData^) do
    begin
      RemoveUserOn(CurSlot);
      AppendLastCall(CurSlot);
      FtpSession.Disconnect;
      FtpSession.Free;
    end; { with }

  {-- Let the log know what we are doing ------------------------------------}
  RaLog('>', Format('[FTPSERV:%d] [%s] Connection closed', [CurSlot, Connections[CurSlot].IpAddr]));

  {-- Clear the variables ---------------------------------------------------}
  Connections[CurSlot].IpAddr := '';
  Connections[CurSlot].StartTimeStr := '';
  Connections[CurSlot].ClientRC := -1;
  ftpConnectionRec(Connections[CurSlot].SrvData^).UserName := '';
  ftpConnectionRec(Connections[CurSlot].SrvData^).PasswordCRC := -1;
  FreeMem(Connections[CurSlot].SrvData);
  Connections[CurSlot].srvData := nil;
  Connections[CurSlot].Name := '';

  Dec(CurrentSessions);

  ftp_ExecuteConnection := 0;
  ExitThisThread;
end; { func. ftp_ExecuteConnection }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$S-}
function ftp_ServerThread(P: pointer): Longint;
var ClientAddr  : PSockAddr;                     { Address socket structure }
    ClientRC    : Longint;                   { Return code for socket calls }
    TempLen     : Longint;                { Variable used for Accept() call }

    Counter     : Longint;                                   { Misc counter }
    SelectedSlot: Longint;             { Selected slot to use for this call }

    ClientName  : String;                    { Hostname used by this client }

begin
  {-- Initialize all variables ----------------------------------------------}
  FillChar(Connections, SizeOf(Connections), #0);

  {-- A variable passed to the Accept() call --------------------------------}
  TempLen := SockAddr_len;

  {-- Setup the client address variable -------------------------------------}
  New(ClientAddr);
  FillChar(ClientAddr^, SizeOf(ClientAddr^), #00);

  {-- Start the DIZ importer ------------------------------------------------}
  if ftp_ImportDiz then
    begin
      New(DizImportThread, Init);
      DizImportThread^.CreateThread(StackSize, @xDizImportThread, DizImportThread, 0);
    end;

  {-- This loop will keep going till the program is ended -------------------}
  repeat

    {-- Now just wait till someone tries to connect -------------------------}
    repeat
      ClientRC := SockAccept(FtpServerSocket.ServerHandle,
                             ClientAddr,
                             TempLen);

      FtpServerSocket.DoSleep(1);
    until (ClientRC <> -1) OR (TermProgram);

    {-- Select a slot for this session --------------------------------------}
    SelectedSlot := 00;
    for Counter := 01 to ftp_MaxSessions do
      if Connections[Counter].Name = '' then
        begin
          SelectedSlot := Counter;
          BREAK;
        end; { if }


    {-- We''ve got an connection, lets initialize it ------------------------}
    if NOT TermProgram then
      begin
        SocksetBlockingIO(ClientRC, false);  { Make that it is non-blocking }

        {-- Lookup host name if allowed, else use the IP address ------------}
        if NOT NoDnsLookup then
          ClientName := SockGetHostNameByAddr(@ClientAddr^.Sin_Addr)
            else ClientName := FtpServerSocket.IpToStr(ClientAddr^.sIn_Addr);

        {-- Make sure we have a slot available for this user ----------------}
        if SelectedSlot > 00 then
          begin
            {-- Initialize all needed data for this session -----------------}
            if NoDNSLookup then
              Connections[SelectedSlot].Name := ' '
                else Connections[SelectedSlot].Name := ClientName;
            Connections[SelectedSlot].SrvType := srvFtp;
            Connections[SelectedSlot].IpAddr:= FtpServerSocket.IpToStr(ClientAddr^.sIn_Addr);
            Connections[SelectedSlot].StartTimeStr := TimeStr(true, false);
            Connections[SelectedSlot].ClientRC := ClientRC;

            {-- allocate memory ---------------------------------------------}
            GetMem(Connections[SelectedSlot].srvData, SizeOf(ftpConnectionRec));
            FillChar(Connections[SelectedSlot].srvData^, SizeOf(ftpConnectionRec), #0);

            ftpConnectionRec(Connections[SelectedSlot].SrvData^).UserName := '';
            ftpConnectionRec(Connections[SelectedSlot].SrvData^).PasswordCRC := -1;
            ftpConnectionRec(Connections[SelectedSlot].SrvData^).Validated := FALSE;
            ftpConnectionRec(Connections[SelectedSlot].SrvData^).IsPassive := FALSE;
            ftpConnectionRec(Connections[SelectedSlot].SrvData^).UseDataConn := FALSE;
            ftpConnectionRec(Connections[SelectedSlot].SrvData^).CurArea := -1;
            ftpConnectionRec(Connections[SelectedSlot].SrvData^).CurGroup := -1;
            ftpConnectionRec(Connections[SelectedSlot].SrvData^).ResumeOfs := 0;
            ftpConnectionRec(Connections[SelectedSlot].SrvData^).VirtIdxBuff := '';
            ftpConnectionRec(Connections[SelectedSlot].SrvData^).VirtIdxArea := -1;
            New(Connections[SelectedSlot].ServerThread, Init);
            ftpConnectionRec(Connections[SelectedSlot].SrvData^).DataThread := TTcpServer.Create;
            ftpConnectionRec(Connections[SelectedSlot].SrvData^).DataThread.ReuseAddr := True; { is this needed? }

            {-- Fire up the thread to handle this session -------------------}
            With Connections[SelectedSlot].ServerThread^ do
              CreateThread(StackSize,
                           @Ftp_ExecuteConnection,
                           Pointer(SelectedSlot),
                           0);

            {-- Now notify the system operator ------------------------------}
            RaLog('>', Format('[FTPSERV:%d] [%s] Connection opened', [SelectedSlot, Connections[SelectedSlot].IpAddr]));
          end
            else begin
                   {-- We cannot accept this session ------------------------}
                   Inc(TotalRefused);
                   SockClose(ClientRc);
                 end; { if busy }
      end; { if not program Terminated }

  until (ClientRC = -1) OR (TermProgram);

  {-- Stop the DIZ importer -------------------------------------------------}
  if ftp_ImportDiz then
    begin
      DizImportThread.Stop(True);
      Dispose(DizImportThread, Done);
    end;

  {-- Dispose all allocated memory ------------------------------------------}
  Dispose(ClientAddr);

  ftp_ListenThreadEnded := true;
  ftp_ServerThread := 0;
  ExitThisThread;
end; { func. ftp_ServerThread }
{$S+}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ftp_initserver;
var
	Counter,
	Counter2 : Integer;
	TmpStr	 : String;
begin
	for Counter := 1 to ParamCount do
		begin
			TmpStr := ParamStr(Counter);

			Case UpCase(TmpStr[2]) of
				'X':
					begin
						{-- Option: -XA for anonymous access -------------------}
						if (UpCase(TmpStr[3]) ) = 'A' then
							ftp_AllowAnonymous := TRUE;
					end; { case 'X' }

				'P':
					begin
						{-- Option: -PASVSRVIP for overriding reported IP address --}
						if SUpCase(Copy(TmpStr, 2, 10)) = 'PASVSRVIP:' then
							begin
								RemoveWordNr(TmpStr, 1, [':'], False);

								for Counter2 := 1 to 4 do
									ftp_PasvSrvIP[Counter2] := Byte(FVal(ExtractWord(TmpStr, Counter2, ['.'], False, False)));
							end { if }

						{-- Option: -PASVDYNIP for overriding reported IP address by an dynamic IP --}
						else if SUpCase(Copy(TmpStr, 2, 10)) = 'PASVDYNIP:' then
							ftp_DynIp := ExtractWord(TmpStr, 2, [':'], False, False)

						{-- Option: -PASVPORTS for restricting passive port range --}
						else if SUpCase(Copy(TmpStr, 2, 10)) = 'PASVPORTS:' then
							begin
								RemoveWordNr(TmpStr, 1, [':'], False);

								for Counter2 := 1 to 2 do
									ftp_PasvPorts[Counter2] := SmallWord(FVal(ExtractWord(TmpStr, Counter2, ['-'], False, False)));
							end { if }

						{-- Option: -PASVOFFSET for padding reported passive port number --}
						else if SUpCase(Copy(TmpStr, 2, 11)) = 'PASVOFFSET:' then
								ftp_PasvOffset := SmallWord(FVal(ExtractWord(TmpStr, 2, [':'], False, False)));

					end; { case 'P' }

				'F':
					begin
						{-- Option: -FTPXLOG for transfer summary log --}
						if SUpCase(Copy(TmpStr, 2, 8)) = 'FTPXLOG:' then
							ftp_TransferLog := ExtractWord(TmpStr, 2, [':'], False, False);

						{-- Option: -FTPINDEX for virtual index files --}
						if SUpCase(Copy(TmpStr, 2, 9)) = 'FTPINDEX:' then
							ftp_VirtualIndex := ExtractWord(TmpStr, 2, [':'], False, False);

						{-- Option: -FTPDIZ for importing uploaded file descriptions --}
						if SUpCase(Copy(TmpStr, 2, 6)) = 'FTPDIZ' then
							ftp_ImportDiz := True;

						{-- Option: -FTPNODE for assigning node numbers to FTP sessions --}
						if SUpCase(Copy(TmpStr, 2, 8)) = 'FTPNODE:' then
							begin
								ftp_FirstNode := Byte(FVal(ExtractWord(TmpStr, 2, [':'], False, False)));

								if ftp_FirstNode = 0 then
									ftp_FirstNode := 255;
							end;

						{-- Option: -FTPLIMIT for session limit --}
						if SUpCase(Copy(TmpStr, 2, 9)) = 'FTPLIMIT:' then
							begin
								ftp_MaxSessions := LongInt(FVal(ExtractWord(TmpStr, 2, [':'], False, False)));

								if ftp_MaxSessions = 0 then
									ftp_MaxSessions := FtpMaxSessions;
							end;

						{-- Option: -FTPPORT for incoming connections --}
						if SUpCase(Copy(TmpStr, 2, 8)) = 'FTPPORT:' then
							begin
								ftp_ServerPort := LongInt(FVal(ExtractWord(TmpStr, 2, [':'], False, False)));

								if ftp_ServerPort = 0 then
									ftp_ServerPort := FtpServerPort;
							end;

						{-- Option: -FTPUSERON for updating USERON.BBS and LASTCALL.BBS --}
						if SUpCase(Copy(TmpStr, 2, 9)) = 'FTPUSERON' then
							ftp_UpdateUseron := True;
					end { case 'F' }
			end; { case }
	end; { for }

	{ ensure the PasvOffset doesn't result in an invalid upper port limit }
	if (ftp_PasvPorts[2] + ftp_PasvOffset > 65535) then
		ftp_PasvPorts[2] := ftp_PasvPorts[2] - ftp_PasvOffset;

end; { proc. ftp_initserver }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ftp_SetupServer;
var ErrorStr: String;
begin
  FtpServerSocket := TTcpServer.Create;
  FtpServerSocket.ReUseAddr := true;

  if NOT FtpServerSocket.SetupServer(ftp_ServerPort, ErrorStr, InAddr_Any) then
    FatalError(ErrorStr);

  FtpServerSocket.Blocking := false;
end; { proc. ftp_SetupServer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

begin
	Randomize;
end. { unit FTPSERV }
