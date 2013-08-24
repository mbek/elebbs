unit MnuSys;
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
** MNUSYS.TPU, System unit for ELCONFIG
**
** Copyright (c) 1996-1998 by Maarten Bekers
**
** Created : 07-Feb-1999
** Last update : 07-Feb-1999
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
  This unit should not be compiled in EleBBS
{$ENDIF}

procedure DoPaths;
procedure DoSiteInfo;
procedure DoSecurity;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses MenuSys, StrEdit, CfgRec, MemMan, ScrnU, FidoAddr,
      LongStr, Global, GenCfg, FileRout, CentrStr, StrPath;

procedure DoPaths;
var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    SaveDirect: Boolean;
begin
  AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'DoPaths');
  InitPullMenu(Menu);

  AddPullItem(Menu.PullInf^[01], 'System     ', 501, #00, 'Where your system files are stored', 1);
  AddPullItem(Menu.PullInf^[02], 'Menus      ', 502, #00, 'Where your default menu (.MNU) files are stored', 1);
  AddPullItem(Menu.PullInf^[03], 'Textfiles  ', 503, #00, 'Where your default text (.A??) files are stored', 1);
  AddPullItem(Menu.PullInf^[04], 'Msg base   ', 504, #00, 'Where your message and users files are stored', 1);
  AddPullItem(Menu.PullInf^[05], 'File base  ', 505, #00, 'Where your file database files are stored', 1);
  AddPullItem(Menu.PullInf^[06], 'File attach', 506, #00, FullProgName+' will use this directory to store user '+
                                                         'file attaches', 1);
  AddPullItem(Menu.PullInf^[07], 'Nodelist   ', 507, #00, 'Path to the raw nodelist and index files', 1);
  AddPullItem(Menu.PullInf^[08], 'CD-ROM temp', 508, #00, 'Temporary holding directory for CD-ROM downloads', 1);
  AddPullItem(Menu.PullInf^[09], 'Semaphore  ', 509, #00, 'Alternative semaphore file directory (default is system '+
                                                         'directory)', 1);
  AddPullItem(Menu.PullInf^[10], 'Rearchive  ', 510, #00, 'Temporary directory for archive conversion', 1);
  AddPullItem(Menu.PullInf^[11], 'System log ', 511, #00, 'Path and filename of system log', 1);
  AddPullItem(Menu.PullInf^[12], 'Utility log', 512, #00, 'Path and filename of the server and utility logs', 1);
  AddPullItem(Menu.PullInf^[13], 'RIP Icons  ', 513, #00, 'Directory which contains your RIP icons', 1);
  AddPullItem(Menu.PullInf^[14], 'Telnet base', 514, #00, 'Path where TELSRV will find ELEBBS - only needed for TELNET', 1);
  AddPullItem(Menu.PullInf^[15], 'Node paths ', 515, #00, 'Directories to execute the telnet nodes from, *N is supported', 1);
  AddPullItem(Menu.PullInf^[16], 'EleWEB Html', 516, #00, 'Path where EleWEB will find its HTML files', 1);
  AddPullItem(Menu.PullInf^[17], 'EleWEB ELM ', 517, #00, 'Where your EleWEB EleXer binaries (.ELM) files are stored', 1);

  Menu.Items   := 17;
  Menu.Width   := 75;
  Menu.Length  := 17;
  Menu.X       := 03;
  Menu.Y       := 02;
  Menu.HiLight := 01;
  Menu.AddSpace:= True;
  Menu.Title   := ' Paths ';
  Menu.PosArray[1].XInc := 00;
  Menu.PosArray[1].YDec := 00;

  ShowMenu(Menu, False);

  repeat
    SaveDirect := DirectScrnUpdate;
    DirectScrnUpdate := false;

    WriteAT(17, 04, mnuNormColor, GlobalCfg^.RaConfig^.SysPath);
    WriteAT(17, 05, mnuNormColor, GlobalCfg^.RaConfig^.MenuPath);
    WriteAT(17, 06, mnuNormColor, GlobalCfg^.RaConfig^.TextPath);
    WriteAT(17, 07, mnuNormColor, GlobalCfg^.RaConfig^.MsgBasePath);
    WriteAT(17, 08, mnuNormColor, GlobalCfg^.RaConfig^.FileBasePath);
    WriteAT(17, 09, mnuNormColor, GlobalCfg^.RaConfig^.AttachPath);
    WriteAT(17, 10, mnuNormColor, GlobalCfg^.RaConfig^.NodeListPath);
    WriteAT(17, 11, mnuNormColor, GlobalCfg^.RaConfig^.TempCDFilePath);
    WriteAT(17, 12, mnuNormColor, GlobalCfg^.RaConfig^.SemPath);
    WriteAT(17, 13, mnuNormColor, GlobalCfg^.RaConfig^.TempScanDir);
    WriteAT(17, 14, mnuNormColor, GlobalCfg^.RaConfig^.LogFileName);
    WriteAT(17, 15, mnuNormColor, GlobalCfg^.ElConfig^.UtilityLogFileName);
    WriteAT(17, 16, mnuNormColor, GlobalCfg^.RaConfig^.RipIconPath);
    WriteAT(17, 17, mnuNormColor, LineCfg^.Telnet^.ProgramPath);
    WriteAT(17, 18, mnuNormColor, LineCfg^.Telnet^.NodeDirectories);
    WriteAT(17, 19, mnuNormColor, GlobalCfg^.ElConfig^.webHtmlPath);
    WriteAT(17, 20, mnuNormColor, GlobalCfg^.ElConfig^.webElmPath);

    DirectScrnUpdate := SaveDirect;

    Choice := DoPullMenu(Menu, CH, False, False);

    if Choice<>0 then
      Case Choice of
          { SysPath }      501 : EditDirectory(17, 04, 59, GlobalCfg^.RaConfig^.SysPath);
          { MenuPath }     502 : EditDirectory(17, 05, 59, GlobalCfg^.RaConfig^.MenuPath);
          { TextPath }     503 : EditDirectory(17, 06, 59, GlobalCfg^.RaConfig^.TextPath);
          { MsgBasePath }  504 : EditDirectory(17, 07, 59, GlobalCfg^.RaConfig^.MsgBasePath);
          { FileBase }     505 : begin
                                    EditDirectory(17, 08, 59, GlobalCfg^.RaConfig^.FileBasePath);
                                    if GlobalCfg^.RaConfig^.FileBasePath<>'' then
                                      begin
                                        MakeDirectory(GlobalCfg^.RaConfig^.FileBasePath+'hdr');
                                        MakeDirectory(GlobalCfg^.RaConfig^.FileBasePath+'idx');
                                        MakeDirectory(GlobalCfg^.RaConfig^.FileBasePath+'txt');
                                      end; { Create Sub-Directorys }
                                 end; { FileBase }
          { FileAttach }   506 : EditDirectory(17, 09, 59, GlobalCfg^.RaConfig^.AttachPath);
          { NodeList }     507 : EditDirectory(17, 10, 59, GlobalCfg^.RaConfig^.NodeListPath);
          { Temp. CD }     508 : EditDirectory(17, 11, 59, GlobalCfg^.RaConfig^.TempCDFilePath);
          { Semaphore }    509 : EditDirectory(17, 12, 59, GlobalCfg^.RaConfig^.SemPath);
          { ReArchive }    510 : EditDirectory(17, 13, 59, GlobalCfg^.RaConfig^.TempScanDir);
          { LogFileName }  511 : begin
                                   GlobalCfg^.RaConfig^.LogFileName := Edit(GlobalCfg^.RaConfig^.LogFileName, 17, 14,
                                         mnuEditColor, 60, false,
                                   False, True);

                                   if Justpath(GlobalCfg^.RaConfig^.LogFileName) <> '' then
                                        MakeDirectory(JustPath(GlobalCfg^.RaConfig^.LogFileName));
                                 end; { LOgFileName }
          { Telnet path }  512 : begin
                                   GlobalCfg^.ElConfig^.UtilityLogFileName := Edit(GlobalCfg^.ElConfig^.UtilityLogFileName,
                                           17, 15, mnuEditColor, 60, false, false, true);

                                   if JustPath(GlobalCfg^.ElConfig^.UtilityLogFileName) <> '' then
                                        MakeDirectory(JustPath(GlobalCfg^.ElConfig^.UtilityLogFileName));
                                 end; { Utility log }
          { RIP Icons }    513 : EditDirectory(17, 16, 59, GlobalCfg^.RaConfig^.RipIconPath);
          { ProgramPath }  514 : EditDirectory(17, 17, 59, LineCfg^.Telnet^.ProgramPath);
          { NodeDirs }     515 : LineCfg^.Telnet^.NodeDirectories := Edit(LineCfg^.Telnet^.NodeDirectories, 17, 18,
                                   mnuEditColor, 60, false, False, True);
          { WEB Html    }  516 : EditDirectory(17, 19, 59, GlobalCfg^.ElConfig^.webHtmlPath);
          { WEB ELMs    }  517 : EditDirectory(17, 20, 59, GlobalCfg^.ElConfig^.webElmPath);
        end; { Case }

  until Choice=0;

  RemoveMenu(Menu);
  ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. DoPaths }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoSiteInfo;
var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    SaveDirect: Boolean;
begin
  AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'DoSiteInfo');
  InitPullMenu(Menu);
  AddPullItem(Menu.PullInf^[01], 'Name    ', 601, #00, 'Name of the system', 1);
  AddPullItem(Menu.PullInf^[02], 'SysOp   ', 602, #00, 'Name of the system operator', 1);
  AddPullItem(Menu.PullInf^[03], 'Location', 603, #00, 'Where the system is located', 1);

  Menu.Items   := 3;
  Menu.Width   := 52;
  Menu.Length  := 3;
  Menu.X       := 02;
  Menu.Y       := 11;
  Menu.HiLight := 01;
  Menu.AddSpace:= True;
  Menu.Title   := ' Site information ';
  Menu.PosArray[1].XInc := 00;
  Menu.PosArray[1].YDec := 00;

  ShowMenu(Menu, False);

  repeat
    SaveDirect := DirectScrnUpdate;
    DirectScrnUpdate := false;

    WriteAT(13, 13, mnuNormColor, GlobalCfg^.RaConfig^.SystemName);
    WriteAT(13, 14, mnuNormColor, GlobalCfg^.RaConfig^.SysOp);
    WriteAT(13, 15, mnuNormColor, GlobalCfg^.RaConfig^.Location);

    DirectScrnUpdate := SaveDirect;
    Choice := DoPullMenu(Menu, CH, False, False);

    if Choice<>0 then
      Case Choice of
          601 : GlobalCfg^.RaConfig^.SystemName := Edit(GlobalCfg^.RaConfig^.SystemName, 13, 13, mnuEditColor, 30, false,
                      false, True);
          602 : GlobalCfg^.RaConfig^.SysOp := Edit(GlobalCfg^.RaConfig^.Sysop, 13, 14, mnuEditColor, 35, false, false,
                      True);
          603 : GlobalCfg^.RaConfig^.Location := Edit(GlobalCfg^.RaConfig^.Location, 13, 15, mnuEditColor, 40, false,
                      false, True);
      end; { Case }

  until Choice=0;

  RemoveMenu(Menu);
  ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. DoSiteInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoSecurity;
var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    SaveDirect: Boolean;
    SaveScrn  : Pointer;
begin
  AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'DoSecurity');
  InitPullMenu(Menu);

  AddPullItem(Menu.PullInf^[01], 'Keyboard  ', 601, #00, 'Set a keyboard password for EleBBS and ELCONFIG etc.', 1);
  AddPullItem(Menu.PullInf^[02], 'Attaching ', 602, #00, 'Set a password required to attach to a screen (EleMON)', 1);

  Menu.Items   := 2;
  Menu.Width   := 32;
  Menu.Length  := 2;
  Menu.X       := 02;
  Menu.Y       := 11;
  Menu.HiLight := 01;
  Menu.AddSpace:= True;
  Menu.Title   := ' Security ';
  Menu.PosArray[1].XInc := 00;
  Menu.PosArray[1].YDec := 00;

  ShowMenu(Menu, False);

  repeat
    SaveDirect := DirectScrnUpdate;
    DirectScrnUpdate := false;

    WriteAT(15, 13, mnuNormColor, Dup('*', 15));
    WriteAT(15, 14, mnuNormColor, Dup('*', 15));

    DirectScrnUpdate := SaveDirect;
    Choice := DoPullMenu(Menu, CH, False, False);

    if Choice<>0 then
      Case Choice of
          601 : begin
                  SaveScreen(SaveScrn);

                  ShadFillBoxTitle(35, 04, 53, 08, mnuBoxColor, mnuStyle, True, ' Keyboard pwd ');

                  WriteAT(mnuMsgXPos, mnuMsgYPos, mnuMsgColor,
                      'Password must be entered prior to entering '+ConfigName+' or using any ALT keys');
                  GlobalCfg^.RaConfig^.KeyBoardPwd := Edit(GlobalCfg^.RaConfig^.KeyBoardPwd, 37, 06, mnuEditColor, 15, True,
                         False, True);
                  RestoreScreen(SaveScrn);
                end; { if }
          602 : begin
                  SaveScreen(SaveScrn);
                  ShadFillBoxTitle(35, 04, 53, 08, mnuBoxColor, mnuStyle, True, ' Attach pwd ');

                  WriteAT(mnuMsgXPos, mnuMsgYPos, mnuMsgColor,
                      'Password must be entered prior to attaching to a screen using EleMON');
                  GlobalCfg^.ElConfig^.AttachPassword := Edit(GlobalCfg^.ElConfig^.AttachPassword, 37, 06, mnuEditColor, 15,
                        false, False, True);
                  RestoreScreen(SaveScrn);
                end; { if }
      end; { Case }

  until Choice=0;

  RemoveMenu(Menu);
  ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. DoSecurity }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


end.
