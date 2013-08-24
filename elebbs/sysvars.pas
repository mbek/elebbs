UNIT SysVars;
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
** System variables init routines for EleBBS
**
** Copyright (c) 2001 by Maarten Bekers
**
** Created : 03-Nov-2001
** Last update : 03-Nov-2001
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 uses Global;

procedure InitBasicGlobalVars;
procedure InitBasicLineVars(var LineCfg: LineCfgRec);

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure InitBasicGlobalVars;
begin
  GlobalCfg^.Raconfig := nil;
  GlobalCfg^.ElConfig := nil;
end; { proc. InitBasicGlobalVars }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure InitBasicLineVars(var LineCfg: LineCfgRec);
begin
  FillChar(LineCfg, SizeOf(LineCfg), #0);
  LineCfg.Exitinfo := nil;
  LineCfg.UserExtension := nil;
  LineCfg.UserExtensionSize := 0;
  LineCfg.SaveGuestRecord := nil;
  LineCfg.Language := nil;
  LineCfg.UploadInfo := nil;
  LineCfg.TagPaths := nil;
  LineCfg.Modem := nil;
  LineCfg.Telnet := nil;
  LineCfg.LimitsInfo := nil;
  LineCfg.MenuContents := nil;
  LineCfg.LBarContents := nil;
  LineCfg.HeaderList := nil;
  LineCfg.MarkedMsgArray := nil;
  LineCfg.CurFilesRecord := nil;
  LineCfg.CurMessageRecord := nil;
  LineCfg.EMSI_user := nil;

  LineCfg.RaNodeNr         := 1;                      { Current node number }
  LineCfg.OnLineTime       := 00;                { Minutes connected THIS call }
  LineCfg.CarrierCheck     := True;                      { Checking carrier }
  LineCfg.TimeFrozen       := False;             { Timeleft has been frozen }
  LineCfg.Snooping         := True;                              { Snooping }
  LineCfg.PauseChecking    := False;               { Check for 'p' keys etc }
  LineCfg.StartTimingDay   := 00;
  LineCfg.ProcessingSysOpKeys:= False; { Curerntly processing a SysOpKey }
  LineCfg.SysOpKey         := False; { Last key read in READKEY was from SysOp }
  LineCfg.DispAbortSet     := ['S', 's'];            { Keys to abort a view }
  LineCfg.DispMorePrompt   := True;                    { Moreprompt active? }
  LineCfg.ContrCodes       := True;                 { Control codes active? }
  LineCfg.RaduCodes        := True;                    { RADU codes active? }
  LineCfg.RalError         := false;             { Language def. file error }
  LineCfg.ReLogonBBS       := false;              { Relogon the user at BBS }
  LineCfg.ReLogMenu        := false;             { Re-use the old menu file }
  LineCfg.LoggedOn         := False;     { Has user passed log-on procedure }
  LineCfg.PrinterLogging   := False;       { Default is printer logging off }
  LineCfg.TextFileShells   := True;               { TextFile Shells active? }
  LineCfg.ShowKludges      := False;              { Don't show kludge-lines }
  LineCfg.CurMsgsMarked    := 00;        { Currently number of messages marked }
  LineCfg.LocalLogon       := false;
  LineCfg.UserAgeSave      := 00;                        { The age of the user }
  LineCfg.ConnectStr       := '';                           { Connect String }
  LineCfg.TelnetFromIp     := '';                               { ip address }
  LineCfg.EventDeducted    := 00;   { Number of mins deducted because of event }
  LineCfg.NowChatting      := False;                        { Now Chatting? }
  LineCfg.DispAbortChar    := #00;                  { Key that aborted display }
  LineCfg.Global_Tagging   := False;
  LineCfg.ShowStatusBar    := False;
  LineCfg.ShowStatusScrn   := true; { Show status screen during up/download }
  LineCfg.GuestUser        := false;      { Is user a guest on this system? }
  LineCfg.CacheDesc        := true;                    { Caching is allowed }
  LineCfg.ForcedExit       := false;              { Forced exit (raxit.??)? }
  LineCfg.TotalUploads     := 00;
  LineCfg.BatchAtExit      := '';          { Batchfile to execute at the end }
  LineCfg.InheritedHandle  := -1;{ Use inherited handle from other process }
  LineCfg.DoFlushScrnBuffer:= false;           { flush the screenbuffer? }
  LineCfg.ComNoClose       := false;  { Don't close the port upon exitting? }
  LineCfg.DosShellMsgTyp   := 00;     { Show message "Type "EXIT" to return }
  LineCfg.CheckInActivity  := false;           { Check inactivity timeout? }
  LineCfg.Search_Skip      := false;       { Skip current area to search in }
  LineCfg.IsSearching      := false;             { The actual screen length }
  LineCfg.DoMonitor        := false;          { Allow monitoring of screens }
  LineCfg.ActScrnLen       := 25; { Actual screen length, depending on statusbar }
end; { proc. InitBasicLineVars }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { sysvars }
