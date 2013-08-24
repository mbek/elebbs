program ELESERV;
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
** EleSERV, Generic Server for NNTP, IDENT, Telnet, etc.
**
** Copyright (c) 1999 - 2002 by Maarten Bekers
**
** Created : 07-Dec-2001
** Last update : 31-Aug-2002
**
*)

uses Global,                     { needed for things like version# etc }
      Debug_U,                   { Debug-logging routines }
       Cases,                    { String routines }
        LongStr,                 { String/Integer routines }
         Crt,                    { Standard console i/o }
          TcpSrv,                { TCP/IP server routines }
           Threads,              { multi-threaded support }
            ApTimer,             { Timing support }
             MemMan,             { memory allocation routines }
              ScrnU,             { Configuration screen routines }
               TcpCli,           { TCP/IP client routines }
                SockDef,         { TCP/IP definitions }
                 SockFunc,       { Standard TCP/IP routines }
                  GenCfg,        { Standard screen i/o routines }
                   ElLog_U,      { Standard logging }
                    WinTitle,    { Change the console window }
                     StUtils,    { String routines }
                      SysUtils,  { SysUtils }
                       JDates,   { Time and Date routines }
                        ReadCfg, { Configuration files }
                         CfgRec, { Configuration records }
                          WordStr,
                           CentrStr,
                            MenuSys,
                           SysVars,      { Variables }
                            eSrv_U,      { EleServer primitives }
                             IdentSrv,   { IdentServer routines }
                              NewsSrv,   { NNTP (usenet/news) routines }
                               FileRout, { file init routines }
                                TelSrv,  { EleBBS telnet server }
                                 FtpServ  { FTP server }


                    {$IFDEF WIN32}
                      ,Windows   { Support routines (Sleep, etc) }
                    {$ENDIF}

                    {$IFDEF OS2}
                      ,Os2Base   { Support routines (Sleep, etc) }
                    {$ENDIF} ;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

const
  maxServers   = 4;                      { IDENTSRV, NEWSSRV, TELSRV, FTPSRV }
  DoRunServers : Boolean = TRUE;                             { run services }

const
  ServersActive : Set of SrvTypeType = [];

var
  OldProc     : Pointer;

  ServerTID   : Array[0..maxServers] of TThreadsObj;
  InputTID    : TThreadsObj;
  UpdateTimer : Eventtimer;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure BuildScreen;
var SaveUpdate: Boolean;
begin
  New(LineCfg);
  New(GlobalCfg);
  InitBasicGlobalVars;
  InitBasicLineVars(LineCfg^);

  {-- Allocate the memory --------------------------------------------------}
  if NOT MemMan.AllocMem(GlobalCfg^.RaConfig, SizeOf(ConfigRecord), 'ConfigRecord', 'ShowHeader') then
    begin
      WriteLn;
      Writeln('Sorry you need at least ', SizeOf(ConfigRecord) div 1024, 'k more memory to run');
      Halt(255);
    end; { Not enough memory }

  if NOT MemMan.AllocMem(GlobalCfg^.ElConfig, SizeOf(EleConfigRecord), 'EleConfigRecord', 'ShowHeader') then
    begin
      WriteLn;
      Writeln('Sorry you need at least ', SizeOf(EleConfigRecord) div 1024, 'k more memory to run');
      Halt(255);
    end; { Not enough memory }


  {-- Initialize some variables --------------------------------------------}
  SaveUpdate := DirectScrnUpdate;
  DirectScrnUpdate := FALSE;
  CursorOff;
  TextAttr := LightGray;

  {-- Now setup a typical EleBBS server screen -----------------------------}
  ScreenClear(LightGray, mnuBackGrChar);       { Initialize the screen area }
  PartClear(1, 1, mnuScrnWidth, 1, LightGray, ' ');
  PartClear(1, 2, mnuScrnWidth, 2, mnuTopBar, #205);
  PartClear(1, mnuScrnLen - 1, mnuScrnWidth, mnuScrnLen - 1, mnuTopBar, mnuSngWide);
  PartClear(1, mnuScrnLen, mnuScrnWidth, mnuScrnLen, LightGray, ' ');

  {-- Write the program name and version header ----------------------------}
  WriteAT(01, 01, Yellow, FullProgName + ' SERVER v'+VersionID);

  {-- And write copyright notice -------------------------------------------}
  WriteRight(79, 01, Yellow, '(c) 1998-2003 Maarten Bekers');

  {-- And some helpfull messages -------------------------------------------}
  WriteAT(01, mnuScrnLen, mnuNormColor, '(ESC) Shutdown server, (ALT-M) Menu');

  {-- Now update the screen ------------------------------------------------}
  UpdateScreenBuffer(true);
  DirectScrnUpdate := SaveUpdate;
end; { proc. ShowHeader }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure BuildWindow;
var TempStr   : String;
    SrvString : String;
    SaveUpdate: Boolean;
begin
  {-- Make sure we dont update too often -----------------------------------}
  SaveUpdate := DirectScrnUpdate;
  DirectScrnUpdate := FALSE;

  {-- Create a box with server statistics ----------------------------------}
  ShadFillBoxTitle(03, 04, 76, 09, mnuBoxColor, mnuStyle, true, ' Server ');

  {-- Get the local hostname -----------------------------------------------}
  TempStr := SockGetHostName;

  {-- Get a list of servers active -----------------------------------------}
  SrvString := '';
  if srvTelnet in ServersActive then SrvString := SrvString + '(Telnet), ';
  if srvNews in ServersActive then SrvString := SrvString + '(News), ';
  if srvIdent in ServersActive then SrvString := SrvString + '(Ident), ';
  if srvFtp in ServersActive then SrvString := SrvString + '(FTP), ';
  Delete(SrvString, Length(SrvString) - 1, 2);

  {-- And display this all -------------------------------------------------}
  WriteAT(05, 05, mnuNormColor, 'Localhost           : ' + TempStr);
  WriteAT(05, 06, mnuNormColor, 'Servers active      : ' + SrvString);
  WriteAT(05, 07, mnuNormColor, 'Current connections : ');

  {-- Create the session box -----------------------------------------------}
  ShadFillBoxTitle(03, 11, 76, 22, mnuBoxColor, mnuStyle, true, ' Sessions ');

  {-- Now update the screen ------------------------------------------------}
  UpdateScreenBuffer(true);
  DirectScrnUpdate := SaveUpdate;
end; { proc. BuildWindow }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ServiceToStr(Srv: SrvTypeType): string;
begin
  ServiceToStr := '';

  case Srv of
    srvNews   : ServiceToStr := 'News';
    srvIdent  : ServiceToStr := 'Ident';
    srvTelnet : ServiceToStr := 'Telnet';
    srvFtp    : ServiceToStr := 'FTP';
  end; { case }
end; { func. ServiceToStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoShowMenu;
var Menu  : PullRecord;
    Choice: Word;
    CH    : Char;
begin
  {-- Initialize menu first ------------------------------------------------}
  Memman.AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', '()');
  InitPullMenu(Menu);

  {-- add the menu item(s) -------------------------------------------------}
  AddPullItem(Menu.PullInf^[01], ' Reload news ', 101, #00, 'Reload newsgroups', 1);

  Menu.Items   := 1;
  Menu.Width   := 14;
  Menu.Length  := 1;
  Menu.X       := 53;
  Menu.Y       := 07;
  Menu.HiLight := 01;
  Menu.AddSpace:= False;
  Menu.Title   := ' Options ';
  Menu.PosArray[1].XInc := 00;
  Menu.PosArray[1].YDec := 00;
  MenuSys.ShowMenu(Menu, False);

  {-- ask for a selection --------------------------------------------------}
  Choice := DoPullMenu(Menu, CH, False, False);

  if Choice <> 0 then
    begin
      case Choice of
        101 : begin
                if srvNews in ServersActive then
                  begin
                    CreateGroupIndex;
                    ReadNewsServerEle;
                  end; { if }
              end; { reload news }
      end; { Case }
    end; { if }


  {-- and clean up ---------------------------------------------------------}
  MenuSys.RemoveMenu(Menu);
  ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. DoShowMenu }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure UpdateWindow;
var Counter    : Longint;
    DisplayedNo: Longint;
    SaveUpdate : Boolean;
begin
  {-- Make sure we dont update for a dead server ---------------------------}
  if HaltScrn then EXIT;

  {-- Log some info to the debugfile ---------------------------------------}
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMain, 'UpdateWindow (begin)');
  {$ENDIF}

  {-- Only update on the timer ---------------------------------------------}
  if (NOT TimerExpired(UpdateTimer)) then
   if NOT srvUpdateScreen then EXIT;

  {-- Make sure we dont update too often -----------------------------------}
  srvUpdateScreen := FALSE;
  SaveUpdate := DirectScrnUpdate;
  DirectScrnUpdate := FALSE;

  {-- Clear the box --------------------------------------------------------}
  ShadFillBoxTitle(03, 11, 76, 22, mnuBoxColor, mnuStyle, true, ' Sessions ');

  {-- reset displaycount timer ---------------------------------------------}
  DisplayedNo := 01;

  {-- Now show all sessions, stop at 10 ------------------------------------}
  for Counter := 01 to Intern_MaxSessions do
    begin
      if DisplayedNo > 10 then BREAK;

      if Counter in [1..10] then
        With Connections[Counter] do
         begin
           {-- Make sure we only show connected entries --------------------}
           if Name <> '' then
             begin
               Inc(DisplayedNo);

               {-- Now actually display the data ---------------------------}
               WriteAT(05, 11 + Counter, mnuNormColor, MakeLen(ServiceToStr(SrvType), 10, Space, false, false));
               WriteAT(16, 11 + Counter, mnuNormColor, MakeLen(Name, 34, space, false, false));
               WriteAT(51, 11 + Counter, mnuNormColor, MakeLen(IpAddr, 15, space, false, false));
               WriteAT(67, 11 + Counter, mnuNormColor, MakeLen(StartTimeStr, 8, space, false, false));
             end; { if }
         end; { with }
    end; { for }

  {-- Show general statistics ----------------------------------------------}
  WriteAT(05, 07, mnuNormColor,
          Format('Current connections : %d (%d total, %d refused)   ', [CurrentSessions, TotalSessions, TotalRefused]));

  {-- Reset the timer, update each 3 seconds -------------------------------}
  NewTimer(UpdateTimer, Secs2Tics(3));

  {-- Now update the screen ------------------------------------------------}
  UpdateScreenBuffer(true);
  DirectScrnUpdate := SaveUpdate;

  {-- Log some info to the debugfile ---------------------------------------}
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMain, 'UpdateWindow ( end )');
  {$ENDIF}
end; { proc. UpdateWindow }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowHelpScreen;
begin
  ClrScr;

  WriteLn(FullProgName, ' SERVER - Command line parameters');
  WriteLn;
  WriteLn;
  WriteLn('-IDENT       - Enables IDENT server');
  WriteLn('-FTP         - Enables FTP server');
  WriteLn('-NEWS        - Enables NNTP (news) server');
  WriteLn('-TELNET      - Enables telnet server');
  WriteLn('-STAT        - Create newsserver statistics file');
  WriteLn('-XM          - Enable EleMON on Telnet connections');
  WriteLn('-XA          - Enable Anonymous access on FTP connections');
  WriteLn;
  WriteLn('-PASVSRVIP:<addr> - Override server IP address for passive FTP');
  WriteLn('-PASVDYNIP:<file> - Similar to PASVSRVIP - One line file with IP address');
  WriteLn('-PASVPORTS:<x-y>  - Specify port range for passive FTP');
  WriteLn('-PASVOFFSET:<x>   - Add x to reported data stream port for passive FTP');
  WriteLn('-FTPXLOG:<file>   - Log FTP transfers to summary log file');
  WriteLn('-FTPINDEX:<file>  - Enable virtual index files (eg. 00_index.txt)');
  WriteLn('-FTPDIZ           - Enable import of file descriptions from uploads');
  WriteLn('-FTPPORT:<x>        Accept incoming FTP connections on port <x>');
  WriteLn('-FTPLIMIT:<x>       Allow a maximum of <x> FTP connections');
  WriteLn('-FTPUSERON          Write login info to USERON.BBS and LASTCALL.BBS');
  WriteLn('-FTPNODE:<x>        Assign node numbers to FTP sessions, starting at <X>');
  WriteLn;
  WriteLn;
  CursorOn;
end; { proc. ShowHelpScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ParseCommandLine;
var Counter: Longint;
    TempCH : Char;
    Param  : String;
begin
  {-- Process all (if any) parameters we have ------------------------------}
  if ParamCount > 00 then
    begin
      for Counter := 01 to ParamCount do
        begin
          Param := ParamStr(Counter);
          TempCH := UpCase(Param[2]);

          {-- We found an parameter, process it ----------------------------}
          if ((Param[1] in ['/', '-'])) then
             Case UpCase(TempCH) of
                '?' : begin
                        ShowHelpScreen;
                        CursorOn;
                        Halt;
                      end; { ? }
                'N' : begin
                        {-- 'N' --------------------------------------------}
                        if SUpCase(Copy(Param, 2, 255)) = 'NODNS' then
                          begin
                            NoDNSLookup := true;
                          end { if }
                           else if SUpCase(Copy(Param, 2, 255)) = 'NEWS' then
                                  begin
                                    ServersActive := ServersActive + [srvNews];
                                  end; { if }
                       end; { 'N' }
                'L' : begin
                        if SUpCase(Copy(Param, 2, 255)) = 'LOW' then
                          begin
                            news_IsRunSlow := TRUE;
                          end; { if }
                      end; { 'L' }
                'I' : begin
                        if SUpCase(Copy(Param, 2, 255)) = 'IDENT' then
                          begin
                            ServersActive := ServersActive + [srvIdent];
                          end; { if }
                      end; { 'L' }
                'F' : begin
                        if SUpCase(Copy(Param, 2, 255)) = 'FTP' then
                          begin
                            ServersActive := ServersActive + [srvFtp];
                          end; { if }
                      end; { 'F' }
                'T' : begin
                        if SUpCase(Copy(Param, 2, 255)) = 'TELNET' then
                          begin
                            ServersActive := ServersActive + [srvTelnet];
                          end; { if }
                      end; { 'L' }
                'S' : begin
                        if SUpCase(Copy(Param, 2, 255)) = 'STAT' then
                          begin
                            news_CreateStatistics;
                            CursorOn;
                            Halt;
                          end; { if }
                      end; { 'S' }
                'X' : begin
                        if SUpCase(Copy(Param, 3, 255)) = 'M' then
                          begin
                            AllowEleMon := TRUE;
                          end { if }
                         else if SUpCase(Copy(Param, 3, 255)) = 'DEBUGLOG' then
                                begin
                                  DebugObj.DebugLogging := true;
                                end; { if }
                      end; { 'X' }
             end; { case }
        end; { for counter }
    end; { if }
end; { proc. ParseCommandLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure NewExitProc;
var Counter   : Longint;
begin
  {-- Restore the old exit procedure first ---------------------------------}
  ExitProc := OldProc;

  {-- And make sure we close all socket connections ------------------------}
  for Counter := 01 to Intern_MaxSessions do
    if Connections[Counter].Name <> '' then
      begin
        SockShutDown(Connections[Counter].ClientRC, 2);
        SockClose(Connections[Counter].ClientRC);

        GenericSleep(10);
      end; { for }

  {-- Now free the server --------------------------------------------------}
  if IdentServer <> nil then IdentServer.Free;
  if NewsServerSocket <> nil then NewsServerSocket.Free;
  if TelnetServer <> nil then TelnetServer.Free;
  if FtpServerSocket <> nil then FtpServerSocket.Free;
end; { proc. NewExitProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function InputFunc(P: pointer): Longint;
var CH: Char;
begin
  {-- Routine that gets polled and which make sure we dont want to exit ----}
  REPEAT
    if Crt.KeyPressed then
      begin
        HaltScrn := true;

        {-- A key is pressed -----------------------------------------------}
        CH := ReadKey;

        if CH = #27 then
          begin

            if DoSaveChanges('Shutdown EleBBS server? (y/N)? °', false, false) then
              begin
                {-- Close the program --------------------------------------}
                TermProgram := true;

                IdentServer.Free;
                IdentServer := nil;

                {-- Give the threads time to shutdown ----------------------}
                Delay(1000);
              end; { if }
          end { if }
            else begin
                   if CH = #00 then
                     begin
                       {-- get next key ------------------------------------}
                       CH := ReadKey;

                       {-- act upon that alt-<key> combi -------------------}
                       case CH of
                         { alt-m } #50 : DoShowMenu;
                       end; { case }
                     end; { if }
                 end; { else }


        HaltScrn := false;
      end; { KeyPressed }

    {-- Now wait for half a second -----------------------------------------}
    GenericSleep(500);

    {-- update the info ----------------------------------------------------}
    UpdateWindow;
  UNTIL (TRUE = FALSE) OR (TermProgram);

  {-- Exit this thread -----------------------------------------------------}
  InputFunc := 0;
  ExitThisThread;
end; { func. InputFunc}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function AllServersDown: Boolean;
var TmpBool: Boolean;
begin
  TmpBool := TRUE;

  if srvIdent in ServersActive then
    if NOT ident_ListenThreadEnded then TmpBool := FALSE;

  if srvNews in ServersActive then
    if NOT news_ListenThreadEnded then TmpBool := FALSE;

  if srvTelnet in ServersActive then
    if NOT telnet_ListenThreadEnded then TmpBool := FALSE;

  if srvFtp in ServersActive then
    if NOT ftp_ListenThreadEnded then TmpBool := FALSE;

  AllServersDown := TmpBool;
end; { func. AllServersDown }
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

var Counter: Longint;
begin
  {-- Setup an fallback routine so all sockets are always closed -----------}
  Oldproc := ExitProc;
  ExitProc := @NewExitProc;

  {-- Make sure we are not running a full fledged BBS (ral etc) ------------}
  RunningBBS := False;

  {-- Set the windows title ------------------------------------------------}
  SetWindowTitle(FullProgName + ' SERVER');

  {-- Initialize screen unit -----------------------------------------------}
  SetupScrnU;

  {-- Parse any commandlines -----------------------------------------------}
  ParseCommandLine;

  {-- Setup the screen -----------------------------------------------------}
  BuildScreen;

  {-- Read config files ----------------------------------------------------}
  ReadConfigRa;
  LogFileName := GlobalCfg^.ElConfig^.UtilityLogFileName;

  {-- get filenames --------------------------------------------------------}
  InitSystemNames;

  {-- if some param only wanted something else, now exit -------------------}
  if NOT DoRunServers then
    begin
      EXIT;
    end; { if }

  {-- Initialize server sessions -------------------------------------------}
  for Counter := 01 to Intern_MaxSessions do
    begin
      Connections[Counter].ServerThread := nil;
      Connections[Counter].Name := '';
    end; { for }

  {-- Setup the server info ------------------------------------------------}
  if srvIdent in ServersActive then ident_InitServer;
  if srvNews in ServersActive then news_InitServer;
  if srvTelnet in ServersActive then telnet_InitServer;
  if srvFtp in ServersActive then ftp_InitServer;

  {-- Initialize the server ------------------------------------------------}
  if srvIdent in ServersActive then ident_SetupServer;
  if srvNews in ServersActive then news_SetupServer;
  if srvTelnet in ServersActive then telnet_SetupServer;
  if srvFtp in ServersActive then ftp_SetupServer;

  {-- Initialize the connections screen and stuff --------------------------}
  BuildWindow;
  srvUpdateScreen := TRUE;
  UpdateWindow;

  {-- Fire up the input handler thread -------------------------------------}
  InputTid.Init;
  InputTID.CreateThread(StackSize,
                        @InputFunc,
                        nil,
                        0);


  {-- if no servers specified, complain ------------------------------------}
  if ServersActive = [] then
    begin
      ShowHelpScreen;
      EXIT;
    end; { if }

  {-- Fire up the server thread --------------------------------------------}
  if srvIdent in ServersActive then
    begin
      ServerTID[0].Init;
      ServerTID[0].CreateThread(StackSize,
                                @Ident_ServerThread,
                                nil,
                                0);
    end; { if }

  {-- Fire up the server thread --------------------------------------------}
  if srvNews in ServersActive then
    begin
      ServerTID[1].Init;
      ServerTID[1].CreateThread(StackSize,
                                @News_ServerThread,
                                nil,
                                0);

    end; { if }

  {-- Fire up the server thread --------------------------------------------}
  if srvTelnet in ServersActive then
    begin
      ServerTID[2].Init;
      ServerTID[2].CreateThread(StackSize,
                                @Telnet_ServerThread,
                                nil,
                                0);

    end; { if }

  {-- Fire up the server thread --------------------------------------------}
  if srvFtp in ServersActive then
    begin
      ServerTID[3].Init;
      ServerTID[3].CreateThread(StackSize,
                                @Ftp_ServerThread,
                                nil,
                                0);

    end; { if }

  {-- Initialize the window update timer -----------------------------------}
  NewTimer(UpdateTimer, Secs2Tics(3));

  {-- And lets log to the server we have initialized -----------------------}
  RaLog('>', '');
  RaLog('>', Format('EleBBS EleServ %s firing up', [VersionID]));


  {-- Now loop till we are finished ----------------------------------------}
  REPEAT
    GenericSleep(10);
  UNTIL (AllServersDown);

  {-- And were done --------------------------------------------------------}
  ClrScr;
  CursorOn;
end. { program EleServ }
