unit FtpServDiz;
(*
**	Author : Scott Little (slittle@slittle.com)
**	Package: EleBBS (EleServ/FtpServ)
**	Purpose: Serialised file description importing subthread, called from EleServ/FtpServ
**	Version: 20070820
*)
{$I COMPILER.INC}

(* ****************************************************************************************************************************** *)
INTERFACE
(* ****************************************************************************************************************************** *)

uses
	SysUtils,
	CfgRec,
	esrv_u,
	Debug_U,
	Ellog_U,
	Threads;


type
	pDizImportJob   = ^tDizImportJob;
	tDizImportJob   = record
			JobID   : LongInt;
			AreaNum : SmallWord;
			FileName: String;
			NextJob : pDizImportJob;
		end;

	pDizImportThread    = ^tDizImportThread;
	tDizImportThread    = Object(TThreadsObj)
		protected
			IsRunning,
			StopThread  : Boolean;
			JobCount    : LongInt;
			JobQueue    : pDizImportJob;
			QueueMutex  : TExclusiveObj;
		public
			constructor   Init;
			destructor    Done;
			function      Start(P: Pointer): LongInt;
			procedure     Stop(Wait: Boolean);
			procedure     SubmitJob(AreaNum: SmallWord;  FileName: String);
		private
			function      RequestJob: pDizImportJob;
		end;


function xDizImportThread(DizImportThread: pDizImportThread): LongInt;
function Spawn(Command: String): Boolean;

(* ****************************************************************************************************************************** *)
IMPLEMENTATION
(* ****************************************************************************************************************************** *)

uses
	Dos,
	{$IFDEF WIN32}
		Windows;
	{$ENDIF}
	{$IFDEF OS2}
		Os2Def, Os2Base;
	{$ENDIF}

(* ****************************************************************************************************************************** *)

constructor tDizImportThread.Init;
begin
	IsRunning := False;
	StopThread := False;
	JobQueue := nil;
	JobCount := -1;
	QueueMutex.Init;
	QueueMutex.CreateExclusive;
end;

(* ****************************************************************************************************************************** *)

destructor tDizImportThread.Done;
var
	J: pDizImportJob;

begin
	StopThread := True;

	{-- empty the JobQueue --}
	J := RequestJob;
	while (J <> nil) do
		begin
			Dispose(J);
			J := RequestJob;
		end;

	QueueMutex.Done; { automatically calls DisposeExclusive }
	IsRunning := False;
end;

(* ****************************************************************************************************************************** *)

procedure tDizImportThread.SubmitJob(AreaNum: SmallWord;  FileName: String);
{
	Add a job to the queue, with thread safety
}
var
	Q: pDizImportJob;

begin
	{$IFDEF WITH_DEBUG}
		DebugObj.DebugLog(logString, 'DizImportThread.SubmitJob (begin)');
	{$ENDIF}

	QueueMutex.EnterExclusive;

	Q := JobQueue;

	if (Q = nil) then
		begin
			New(Q);
			JobQueue := Q;
		end
	else
		begin
			while (Q^.NextJob <> nil) do
				Q := Q^.NextJob;

			New(Q^.NextJob);
			Q := Q^.NextJob;
		end;

	inc(JobCount);
	Q^.JobID   := JobCount;
	Q^.AreaNum := AreaNum;
	Q^.FileName:= FileName;
	Q^.NextJob := nil;

	QueueMutex.LeaveExclusive;

	{$IFDEF WITH_DEBUG}
		DebugObj.DebugLog(logString, 'DizImportThread.SubmitJob (end)');
	{$ENDIF}
end; { proc. SubmitJob }

(* ****************************************************************************************************************************** *)

function tDizImportThread.RequestJob: pDizImportJob;
{
	Pop the next job off the queue.
	Memory is freed by the caller, so we just have to return the current pointer, then advance the queue to the next job.
}
begin
	{$IFDEF WITH_DEBUG}
		DebugObj.DebugLog(logString, 'DizImportThread.RequestJob (begin)');
	{$ENDIF}

	QueueMutex.EnterExclusive;

	RequestJob := JobQueue;

	if (JobQueue <> nil) then
		JobQueue := JobQueue^.NextJob;

	QueueMutex.LeaveExclusive;

	{$IFDEF WITH_DEBUG}
		DebugObj.DebugLog(logString, 'DizImportThread.RequestJob (end)');
	{$ENDIF}
end; { func. RequestJob }

(* ****************************************************************************************************************************** *)

{$S-}
function tDizImportThread.Start(P: Pointer): LongInt;
{
	Main thread function; loop until told to exit, check queue and dispatch jobs to importer (ELEFILE.EXE, for now)
}
var
	WorkJob: pDizImportJob;

begin
	{$IFDEF WITH_DEBUG}
		DebugObj.DebugLog(logString, 'DizImportThread.Start (begin)');
	{$ENDIF}

	IsRunning := True;

	while (not StopThread) do
		begin
			GenericSleep(250);

			WorkJob := RequestJob;

			while (not StopThread) and (WorkJob <> nil) do
				begin
					RaLog('>', Format('[FTPSERV:ImportDiz] [Job #%d] Importing file description for "%s" in area #%d', [WorkJob^.JobID, WorkJob^.FileName, WorkJob^.AreaNum]));
					if not Spawn(Format('%s %d "%s"', [GetEnv('COMSPEC') + ' /C ELEFILE.EXE DESCRIBE ', WorkJob^.AreaNum, WorkJob^.FileName])) then
						RaLog('>', Format('[FTPSERV:ImportDiz] [Job #%d] Failed to run external program', [WorkJob^.JobID, WorkJob^.FileName, WorkJob^.AreaNum]));
					Dispose(WorkJob);
					WorkJob := RequestJob;
				end;
		end;

	IsRunning := False;
	Start := 0;

	{$IFDEF WITH_DEBUG}
		DebugObj.DebugLog(logString, 'DizImportThread.Start (end)');
	{$ENDIF}

	ExitThisThread;
end; { func. Start }
{$S+}

(* ****************************************************************************************************************************** *)

procedure tDizImportThread.Stop(Wait: Boolean);
begin
	StopThread := True;

	while (Wait) and (IsRunning) do
		GenericSleep(10);
end;

(* ****************************************************************************************************************************** *)

function xDizImportThread(DizImportThread: pDizImportThread): LongInt;
{
	Wrapper for thread object Start method, because apparently object methods can't be thread functions...
}
begin
	xDizImportThread := DizImportThread.Start(nil);
end;

(* ****************************************************************************************************************************** *)

{$IFDEF WIN32}
function Spawn(Command: String): Boolean;
{
	Run the specified script through the default command interpreter, spawning a new window

	XXXNOTE: this is here because ExecFunc.pas is for EleBBS proper, not utilities
	XXXWARN: this will block the program from exiting while the external program runs; might be a problem in the future when
	XXXWARN  scheduled events are implemented (eg. automated server restart) and an external program has hung.
	XXXWARN  Possibly put a timeout on WaitForSingleObject in a loop, testing for WAIT_OBJECT_0=0 or StopThread?
}
var
	PCommand: PChar;
	StartupInfo: tStartupInfo;
	ProcessInfo: tProcessInformation;

begin
	{$IFDEF WITH_DEBUG}
		DebugObj.DebugLog(logString, 'ImportDiz (begin)');
	{$ENDIF}

	FillChar(StartupInfo, SizeOf(tStartupInfo), #0);
	StartupInfo.CB := SizeOf(tStartupInfo);
	StartupInfo.dwFlags := startf_UseShowWindow;
	StartupInfo.wShowWindow := {SW_MINIMIZE}SW_SHOWMINNOACTIVE;

	PCommand := StrAlloc(Succ(Length((Command))));

	if CreateProcess(nil,
	                 StrPCopy(PCommand, Command),
	                 nil,
	                 nil,
	                 False,
	                 CREATE_NEW_CONSOLE,
	                 nil,
	                 nil,
	                 StartupInfo,
	                 ProcessInfo) then
		begin
			WaitForSingleObject(ProcessInfo.hProcess, Infinite);
			Spawn := True;
		end
	else

		Spawn := False;

	CloseHandle(ProcessInfo.hThread);
	CloseHandle(ProcessInfo.hProcess);
	StrDispose(PCommand);

	{$IFDEF WITH_DEBUG}
		DebugObj.DebugLog(logString, 'ImportDiz (end)');
	{$ENDIF}
end; { func. Spawn }
{$ENDIF}

(* ****************************************************************************************************************************** *)

{$IFDEF OS2}
function Spawn(Command: String): Boolean;
{
	Run the specified script through the default command interpreter, spawning a new window

	XXXNOTE: this is here because ExecFunc.pas is for EleBBS proper, not utilities
	XXXWARN: not tested (OS/2 sucks, is obsolete and makes VMWare defecate pottery; I'm also lazy)
}
type
	rcStruct = record
			SessionID  : uShort;
			ReturnCode : uShort;
		end;

var
	IdSession : ULong;
	Pid1      : Pid;
	StData    : StartData;
	hqQueue   : HQUEUE;
	rdRequest : REQUESTDATA;
	ulSzData  : ULong;
	bPriority : Byte;
	pvData	  : ^RcStruct;

	CmdBuf	  : Array[0..259] of Char;
	ParamBuf  : Array[0..259] of Char;
	QBUf	  : Array[0..259] of Char;
	TitleBuf  : Array[0..259] of Char;

begin
	Spawn := True;												{ Success }

	FillChar(StData, SizeOf(StartData), 00);
	FillChar(CmdLine, SizeOf(CmdLine), #00);
	FillChar(ParamBuf, SizeOf(ParamBuf), #00);
	FillChar(QBuf, SizeOf(QBuf), #00);
	FillChar(TitleBuf, SizeOf(TitleBuf), #00);

	StData.Length := SizeOf(StartData);
	StData.Related := ssf_Related_Child;
	StData.FgBg := ssf_FgBg_Back;
	StData.TraceOpt := ssf_TraceOpt_None;
	StData.PgmTitle := StrPCopy(TitleBuf, 'EleServ/FtpServ (ImportDiz)');
	StData.PgmName := nil; {StrPCopy(CmdBuf, FileName);}
	StData.PgmInputs := StrPCopy(ParamBuf, Command);
	StData.TermQ := StrPCopy(QBuf, 'EleServ');
	StData.Environment := nil;
	StData.InheritOpt := ssf_InhertOpt_Parent;
	StData.SessionType := ssf_Type_Default;
{?}  StData.IconFile := nil;
{?}  StData.PgmHandle := 0;
{?}  StData.PgmControl := ssf_Control_Visible;
	StData.InitXPos := ssf_control_setpos;
	StData.InitYPos := ssf_control_setpos;
	StData.InitXSize := ssf_control_setpos;
	StData.InitYSize := ssf_control_setpos;
{?}  StData.ObjectBuffer := nil;
{?}  StData.ObjectBuffLen := 0;

	if DosCreateQueue(hqQueue, QUE_FIFO AND QUE_CONVERT_ADDRESS, StrPCopy(QBuf, 'EleServ')) = 0 then
	 begin
		 if DosStartSession(StData, IdSession, Pid1) = 0 then
			begin
				if DosReadQueue(hqQueue, rdRequest, ulSzData, pvData, 0, 0, bPriority, 0) = 0 then
					begin
						DosFreeMem(pvData);
					end
						else Spawn := False;
			end
				else Spawn := False;

		 DosCloseQueue(hqQueue);
	 end
		else Result := False;
end; { func. Spawn }
{$ENDIF}

(* ****************************************************************************************************************************** *)

end.
