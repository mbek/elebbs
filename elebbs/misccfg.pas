unit MiscCFG;
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
** MISCCFG.TPU, Main support unit for ElCONFIG
**
** Copyright (c) 1996 by Maarten Bekers
**
** Created : 16-Oct-1996
** Last update : 16-Apr-1997
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
  This unit should not be compiled in EleBBS
{$ENDIF}

{$IFDEF ISCGI}
  This unit should not be compiled in EleWEB
{$ENDIF}

uses {$IFDEF DELCRT}
       Crt32,
     {$ELSE}
       Crt,
     {$ENDIF}

     {$IFDEF WIn32}
       Windows,
       {$IFDEF VirtualPascal}
         VpSysLow,
       {$ENDIF}
     {$ENDIF}

     RecDif, Dos, Strings, AkaLst, Sort_Un,
     Global, FidoAddr, Area_Lst, CfgDef, FileRout,
     {$IFDEF MSDOS}
       Memory,
       Exec,
     {$ENDIF}
     {$IFDEF OS2}
       Os2Base,
     {$ENDIF}
     MenuCfg, GenCfg, CfgScrn, Debug_U, AreaDef, CfgRec, MemMan,
     StrPath, ScrnU, StUtils, GenFile, WordStr, ReadCfg, Editor,
     StrEdit, MenuSys, ListSys, BitWise, CentrStr, Cases, Colors,
     mnuModem, mnuSys, GenDos, Ranges, LimLst, DispMnu, FlagStr,
     FileObj, LongStr, RemScrn, ObjDec;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure StartMenu;                     { Start menu (displays it etcetera) }
procedure DoEditLanguage;
procedure DoEditMenus(Params: String);
procedure MiscInit;
procedure StartInitting;
procedure ProcessKey(var CH:Char);


Const ConfigRaName : String[128] = '';
      ConfigEleName: String[128] = '';

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Bool2Str(B: Boolean): String;
begin
  if B then Bool2Str := 'Yes'
    else Bool2Str := 'No';
end; { func. Bool2Str }


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure NewExitProc;

Function HexWord(W:Word) : String;
Const Hexchars : Array[0..$F] of Char = '0123456789ABCDEF';
begin
 HexWord := HexChars[Hi(W) SHR 4]  +
            HexChars[Hi(W) AND $F] +
            HexChars[Lo(W) SHR 4]  +
            HexChars[Lo(W) AND $F];
End; { Func. HexWord }

begin
 ExitProc := OldExitProc;
 CursorOn;
 {$IFDEF DELCRT}Crt32{$ELSE}Crt{$ENDIF}.TextAttr := 15;
 ScreenClear(White, #32);

 ReleaseMem(GlobalCfg^.RaConfig, SizeOf(ConfigRecord));
 ReleaseMem(GlobalCfg^.ElConfig, SizeOf(EleConfigRecord));
 ReleaseMem(LineCfg^.Modem, SizeOf(ModemRecord));
 ReleaseMem(Events, SizeOf(EventRecordArray));
 ReleaseMem(LineCfg^.Telnet, SizeOf(TelnetRecord));
 ReleaseMem(LineCfg^.NewsServer, SizeOf(NewsServerRecord));

 ReleaseMem(OldConfig, SizeOf(ConfigRecord));
 ReleaseMem(OldEleConfig, SizeOf(EleConfigRecord));
 ReleaseMem(OldModem, SizeOf(ModemRecord));
 ReleaseMem(OldEvents, SizeOf(EventRecordArray));
 ReleaseMem(OldTelnet, SizeOf(TelnetRecord));
 ReleaseMem(OldNewsServer, SizeOf(NewsServerRecord));

 ReleaseMem(PullMenu, SizeOf(PullMenu^));
 if MnuFileMenu <> nil then
  ReleaseMem(mnuFileMenu^.PullInf, SizeOf(mnuFileMenu^.PullInf^));
 if MnuSystemMenu <> nil then
   ReleaseMem(mnuSystemMenu^.PullInf, SizeOf(mnuSystemMenu^.PullInf^));
 if MnuOptionsMenu <> nil then
   ReleaseMem(mnuOptionsMenu^.PullInf, SizeOf(mnuOptionsMenu^.PullInf^));
 if MnuModemMenu <> nil then
   ReleaseMem(mnuModemMenu^.PullInf, SizeOf(mnuModemMenu^.PullInf^));
 if MnuManagerMenu <> nil then
   ReleaseMem(mnuManagerMenu^.PullInf, SizeOf(mnuManagerMenu^.PullInf^));

 ReleaseMem(mnuFileMenu, SizeOf(mnuFileMenu^));
 ReleaseMem(mnuSystemMenu, SizeOf(mnuSystemMenu^));
 ReleaseMem(mnuOptionsMenu, SizeOf(mnuOptionsMenu^));
 ReleaseMem(mnuModemMenu, SizeOf(mnuModemMenu^));
 ReleaseMem(mnuManagerMenu, SizeOf(mnuManagerMenu^));

 {$IFNDEF DELPHI}
 If ErrorAddr<>Nil then
    WriteLn('Runtime error '+FStr(ExitCode)+' at '+HexWord(Seg(ErrorAddr^))+':'+
             HexWord(Ofs(ErrorAddr^)));
 {$ENDIF}

 {$IFNDEF WIN32}
   WriteLn;
 {$ENDIF}
end; { NewExitProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoPickFunctionKey(var CH: Char);
var Menu      : PullRecord;
    Choice    : Word;
    SaveDirect: Boolean;
    TempCH    : Char;
begin
  AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'DoPickFunctioNkey');
  InitPullMenu(Menu);
  AddPullItem(Menu.PullInf^[01], 'F1 ', 1101, #00, 'Emulate F1 keypress', 1);
  AddPullItem(Menu.PullInf^[02], 'F2 ', 1102, #00, 'Emulate F2 keypress', 1);
  AddPullItem(Menu.PullInf^[03], 'F3 ', 1103, #00, 'Emulate F3 keypress', 1);
  AddPullItem(Menu.PullInf^[04], 'F4 ', 1104, #00, 'Emulate F4 keypress', 1);
  AddPullItem(Menu.PullInf^[05], 'F5 ', 1105, #00, 'Emulate F5 keypress', 1);
  AddPullItem(Menu.PullInf^[06], 'F6 ', 1106, #00, 'Emulate F6 keypress', 1);
  AddPullItem(Menu.PullInf^[07], 'F7 ', 1107, #00, 'Emulate F7 keypress', 1);
  AddPullItem(Menu.PullInf^[08], 'F8 ', 1108, #00, 'Emulate F8 keypress', 1);
  AddPullItem(Menu.PullInf^[09], 'F9 ', 1109, #00, 'Emulate F9 keypress', 1);
  AddPullItem(Menu.PullInf^[10], 'F10', 1110, #00, 'Emulate F10 keypress', 1);

  Menu.Items   := 10;
  Menu.Width   := 7;
  Menu.Length  := 10;
  Menu.X       := 2;
  Menu.Y       := 4;
  Menu.HiLight := 01;
  Menu.AddSpace:= true;
  Menu.Title   := ' Key  ';
  Menu.PosArray[1].XInc := 00;
  Menu.PosArray[1].YDec := 00;

  ShowMenu(Menu, False);
  Choice := DoPullMenu(Menu, TempCH, False, False);
  RemoveMenu(Menu);
  ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));

  if rdLastKey = #13 then
    begin
      rdLastKey := #00;
      CH := Chr(58 + (Choice - 1100));
    end; { if }

end; { proc. DoPickFunctionKey }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ProcessKey(var CH:Char);
begin
 { ALT-Z } if CH=#44 then DoDosShell('Type "EXIT" to return to '+ConfigName+'.');
 { ALT-J } if CH=#36 then DoDosShell('Type "EXIT" to return to '+ConfigName+'.');
 { ALT-K } if CH=#18 then DoPickFunctionKey(CH);
 { F1 }    if CH=#59 then If EdittingMsgArea then DoEditAreaNr(74, 1, RdxName(MessagesFileName), MessageInf^.AreaNum);
 { F1 }    if CH=#59 then If EdittingFileArea then DoEditAreaNr(61, 1, RdxName(FilesFileName), FilesInf.AreaNum);
 { F1 }    if CH=#59 then If (EdittingGroupRecord) AND (GroupMessage) then DoEditAreaNr(61, 6, RdxName(MGroupsFileName),
                          GroupInf.AreaNum);
 { F1 }    if CH=#59 then If (EdittingGroupRecord) AND (NOT GroupMessage) then
                              DoEditAreaNr(61, 6, RdxName(FGroupsFileName), GroupInf.AreaNum);
end; { proc. Processkey }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowInfo;
var SaveDirect: Boolean;
begin
  (*SaveScreen(3);*)

  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;
  ShadFillBoxTitle(24, 09, 60, 18, mnuBoxColor, mnuStyle, True, ' About '+FullProgName+' ');
  WriteCenterPos(24, 11, 36, LightGray, FullProgname+' '+VersionID);
  WriteCenterPos(24, 13, 36, LightGray, 'Copyright 1996-2003');
  WriteCenterPos(24, 14, 36, LightGray, 'Maarten Bekers');
  DirectScrnUpdate := SaveDirect;
  WriteCenterPos(24, 16, 36, LightGray, 'All Rights Reserved');

  GetInput;
end; { proc. ShowInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoNewsServer;
var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    SaveDirect: Boolean;
    TmpWord   : SmallWord;
begin
  AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'DoRestrictions');
  InitPullMenu(Menu);

  AddPullItem(Menu.PullInf^[01], 'Max. Sessions ', 3601,#00, 'Maximum number of concurrent sessions allowed', 1);
  AddPullItem(Menu.PullInf^[02], 'Server port   ', 3602,#00, 'TCP/IP port to listen on', 1);
  AddPullItem(Menu.PullInf^[03], 'Allow SysOp   ', 3603,#00, 'Is the SysOp allowed to logon to the newsserver?', 1);
  AddPullItem(Menu.PullInf^[04], 'Domain name   ', 3604,#00, 'Use domain name for email addresses (empty= use FTN style)', 1);
  AddPullItem(Menu.PullInf^[05], 'Send msgstats ', 3605,#00, 'Send the message statistics (lines/bytes). Slows down system',1);
  AddPullItem(Menu.PullInf^[06], 'Update postcnt', 3606,#00, 'Update the user message post count when writing messages?', 1);
  AddPullItem(Menu.PullInf^[07], 'Auth.group lst', 3607,#00, 'Only send group listings when authenticated', 1);

  Menu.Items   := 7;
  Menu.Width   := 58;
  Menu.Length  := 7;
  Menu.X       := 2;
  Menu.Y       := 4;
  Menu.HiLight := 01;
  Menu.AddSpace:= True;
  Menu.Title   := ' Newsserver ';
  Menu.PosArray[1].XInc := 00;
  Menu.PosArray[1].YDec := 00;

  ShowMenu(Menu, False);

  repeat
    SaveDirect := DirectScrnUpdate;
    DirectscrnUpdate := false;

    WriteAT(19, 06, mnuNormColor, FStr(LineCfg^.Newsserver^.MaxSessions));
    WriteAT(19, 07, mnuNormColor, FStr(LineCfg^.NewsServer^.ServerPort));
    WriteAT(19, 08, mnuNormColor, Bit2Str(LineCfg^.NewsServer^.Attribute, 0));
    WriteAT(19, 09, mnuNormColor, Copy(LineCfg^.NewsServer^.DomainName, 1, 40));
    WriteAT(19, 10, mnuNormColor, Bit2Str(LineCfg^.NewsServer^.Attribute, 1));
    WriteAT(19, 11, mnuNormColor, Bit2Str(LineCfg^.NewsServer^.Attribute, 2));
    WriteAT(19, 12, mnuNormColor, Bit2Str(LineCfg^.NewsServer^.Attribute, 3));
    DirectScrnUpdate := SaveDirect;

    Choice := DoPullMenu(Menu, CH, False, False);

    with lineCfg^ do
    Case Choice of
      3601 : begin
               TmpWord := NewsServer^.MaxSessions;
               EditWord(Tmpword, 19, 06, mnuEditColor, True);
               NewsServer^.MaxSessions := TmpWord;
             end;
      3602 : begin
               TmpWord := NewsServer^.ServerPort;
               EditWord(TmpWord, 19, 07, mnuEditColor, True);
               NewsServer^.Serverport := TmpWord;
             end;
      3603 : NewsServer^.Attribute := Edit_Bit(NewsServer^.Attribute, 00, 19, 08, mnuNormColor);
      3604 : GetString(19, 09, mnuEditColor, NewsServer^.DomainName, [#32..#255], [#27, #13], 120, 40, False, False, True,
                       false, 0);
      3605 : NewsServer^.Attribute := Edit_Bit(NewsServer^.Attribute, 01, 19, 10, mnuNormColor);
      3606 : NewsServer^.Attribute := Edit_Bit(NewsServer^.Attribute, 02, 19, 11, mnuNormColor);
      3607 : NewsServer^.Attribute := Edit_Bit(NewsServer^.Attribute, 03, 19, 12, mnuNormColor);
    end; { Case }

  until Choice = 0;

  RemoveMenu(Menu);
  ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. DoNewsServer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure DoMessage;

Var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    SaveDirect: Boolean;
Begin
 AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'DoMessage');
 InitPullMenu(Menu);
 AddPullItem(Menu.PullInf^[01], 'Logon.Newmail  ', 901, #00, 'Perform a new mail scan at logon?', 1);
 AddPullItem(Menu.PullInf^[02], 'Full mailcheck ', 902, #00, 'Check for mail from the start of messages, '+
                                                            'or from the user''s lastread msg', 1);
 AddPullItem(Menu.PullInf^[03], 'Quote string   ', 903, #00, 'Text to place before quoted message text', 1);
 AddPullItem(Menu.PullInf^[04], 'External editor', 904, #00, 'DOS command-line to invoke an external message editor', 1);
 AddPullItem(Menu.PullInf^[05], 'Default origin ', 905, #00, 'The default origin line which appears in '+
                                                            'outbound echomail messages', 1);
 AddPullItem(Menu.PullInf^[06], 'Reply header   ', 906, #00, 'Text placed at the top of a reply', 1);
 AddPullItem(Menu.PullInf^[07], 'Msg uploads    ', 907, #00, 'Allow users to upload messages?', 1);
 AddPullItem(Menu.PullInf^[08], 'Echo netreplies', 908, #00, 'Allow users to reply to echomail messages via '+
                                                            'private netmail?', 1);
 AddPullItem(Menu.PullInf^[09], 'Net.KillSent   ', 909, #00, 'Kill netmail after sent?', 1);
 AddPullItem(Menu.PullInf^[10], 'Confirm delete ', 910, #00, 'Ask for message delete confirmation?', 1);
 AddPullItem(Menu.PullInf^[11], 'Net.Crash opt  ', 911, #00, 'Security level for optional crashmail', 1);
 AddPullItem(Menu.PullInf^[12], 'Net.Crash force', 912, #00, 'Security level to force crashmail', 1);
 AddPullItem(Menu.PullInf^[13], 'Net.Attach     ', 913, #00, 'Security level required to attach files to netmail messages', 1);
 AddPullItem(Menu.PullInf^[14], 'Group mail     ', 914, #00, 'Security level required to generate group messages', 1);
 AddPullItem(Menu.PullInf^[15], 'CC mail        ', 915, #00, 'Security level required to send carbon copy messages', 1);
 AddPullItem(Menu.PullInf^[16], 'Return receipts', 916, #00, 'Security level required to request return receipts', 1);
 AddPullItem(Menu.PullInf^[17], 'Net.receipts   ', 917, #00, 'Honour netmail return receipt requests?', 1);

 Menu.Items   := 17;
 Menu.Width   := 24;
 Menu.Length  := 17;
 Menu.X       := 2;
 Menu.Y       := 3;
 Menu.HiLight := 01;
 Menu.AddSpace:= True;
 Menu.Title   := ' Message options ';
 Menu.PosArray[1].XInc := 00;
 Menu.PosArray[1].YDec := 00;

 ShowMenu(Menu, False);

 with GlobalCfg^ do
 Repeat;
  SaveDirect := DirectScrnUpdate;
  DirectscrnUpdate := false;

  WriteAT(20, 05, mnuNormColor, Ask2Str(Byte(RaConfig^.CheckMail)));
  WriteAT(20, 06, mnuNormColor, Bool2Str(RaConfig^.DoFullMailCheck));
  { Quote String }
  { External Ed CMD }
  { OriginLine }
  { ReplyHeader }
  WriteAT(20, 11, mnuNormColor, Bool2Str(RaConfig^.UploadMsgs));
  WriteAT(20, 12, mnuNormColor, Bool2Str(RaConfig^.AllowNetMailReplies));
  WriteAT(20, 13, mnuNormColor, Ask2Str(Byte(RaConfig^.KillSent)));
  WriteAT(20, 14, mnuNormColor, Bool2Str(RaConfig^.ConfirmMsgDeletes));
  WriteAT(20, 15, mnuNormColor, FStr(RaConfig^.CrashAskSec));
  WriteAT(20, 16, mnuNormColor, FStr(RaConfig^.CrashSec));
  WriteAT(20, 17, mnuNormColor, FStr(RaConfig^.FAttachSec));
  WriteAT(20, 18, mnuNormColor, FStr(RaConfig^.GroupMailSec));
  WriteAT(20, 19, mnuNormColor, FStr(RaConfig^.ccSec));
  WriteAT(20, 20, mnuNormColor, FStr(RaConfig^.ReturnRecSec));
  WriteAT(20, 21, mnuNormColor, Bool2Str(RaConfig^.HonourNetReq));
  DirectScrnUpdate := SaveDirect;

  Choice := DoPullMenu(Menu, CH, False, False);

  Case Choice of
      901 : EditAskType(20, 05, Byte(RaConfig^.CheckMail), True, True, True, False);
      902 : Edit_Boolean(RaConfig^.DoFullMailCheck, 20, 06, mnuNormColor);
      903 : MakeEditBox(5, 19, 5, RaConfig^.QuoteString, ' Quote string ', false, Menu);
      904 : MakeEditBox(60, 63, 60, RaConfig^.ExternalEdCmd, ' External editor command-line ', false, Menu);
      905 : MakeEditBox(60, 63, 60, RaConfig^.OriginLine, ' Default origin line ', false, Menu);
      906 : MakeEditBox(60, 63, 60, RaConfig^.ReplyHeader, ' Reply header text ', false, Menu);
      907 : Edit_Boolean(RaConfig^.UploadMsgs, 20, 11, mnuNormColor);
      908 : Edit_Boolean(RaConfig^.AllowNetMailReplies, 20, 12, mnuNormColor);
      909 : EditAskType(20, 13, Byte(RaConfig^.KillSent), True, True, True, false);
      910 : Edit_Boolean(RaConfig^.ConfirmMsgDeletes, 20, 14, mnuNormColor);
      911 : EditWord(RaConfig^.CrashAskSec, 20, 15, mnuEditColor, True);
      912 : EditWord(RaConfig^.CrashSec, 20, 16, mnuEditColor, True);
      913 : EditWord(RaConfig^.FAttachSec, 20, 17, mnuEditColor, True);
      914 : EditWord(RaConfig^.GroupMailSec, 20, 18, mnuEditColor, True);
      915 : EditWord(RaConfig^.ccSec, 20, 19, mnuEditColor, True);
      916 : EditWord(RaConfig^.ReturnRecSec, 20, 20, mnuEditColor, True);
      917 : Edit_Boolean(RaConfig^.HonourNetReq, 20, 21, mnuEditColor);
  End; { Case }

 Until Choice=0;

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. DoMessage }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure DoFiles;


Procedure DoUploadScan;

Procedure EditFailedScan;
var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    SaveDirect: Boolean;
begin
 AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'EditFailedScan');
 InitPullMenu(Menu);
 AddPullItem(Menu.PullInf^[01], 'Delete  ', 01, #00, '"Yes" means that files which fail will be deleted', 1);
 AddPullItem(Menu.PullInf^[02], 'Unlisted', 02, #00, '"Yes" means that files which fail will be marked as unlisted', 1);
 AddPullItem(Menu.PullInf^[03], 'Notavail', 03, #00, '"Yes" means that files which fail will be marked as not available', 1);
 AddPullItem(Menu.PullInf^[04], 'Move to ', 04, #00, 'Area to move failed files to', 1);

 Menu.Items   := 4;
 Menu.Width   := 17;
 Menu.Length  := 4;
 Menu.X       := 59;
 Menu.Y       := 15;
 Menu.HiLight := 01;
 Menu.AddSpace:= True;
 Menu.Title   := ' Fail action ';
 Menu.PosArray[1].XInc := 00;
 Menu.PosArray[1].YDec := 00;

 ShowMenu(Menu, False);

 with GlobalCfg^ do
 Repeat;
  SaveDirect := DirectScrnUpdate;
  DirectscrnUpdate := false;

  WriteAT(70, 17, mnuNormColor, Bit2Str(RaConfig^.FailedScanAction, 0));
  WriteAT(70, 18, mnuNormColor, Bit2Str(RaConfig^.FailedScanAction, 1));
  WriteAT(70, 19, mnuNormColor, Bit2Str(RaConfig^.FailedScanAction, 2));
  WriteAT(70, 20, mnuNormColor, FStr(RaConfig^.FailedScanArea));
  DirectScrnUpdate := SaveDirect;

  Choice := DoPullMenu(Menu, CH, False, False);

  Case Choice of
    01 : begin
          RaConfig^.FailedScanAction := Edit_Bit(RaConfig^.FailedScanAction, 00, 70, 18, mnuNormColor);

          If ReadBit(RaConfig^.FailedScanAction, 00) then
                begin
                  ClearBit(RaConfig^.FailedScanAction, 01);
                  ClearBit(RaConfig^.FailedScanAction, 02);
                  RaConfig^.FailedScanArea := 00;
                end; { Delete is 'on' }
         end; { Delete }
    02 : begin
           RaConfig^.FailedScanAction := Edit_Bit(RaConfig^.FailedScanAction, 01, 70, 18, mnuNormColor);
           If ReadBit(RaConfig^.FailedScanAction, 01) then ClearBit(RaConfig^.FailedScanAction, 00);
         end; { Unlisted }
    03 : begin
           RaConfig^.FailedScanAction := Edit_Bit(RaConfig^.FailedScanAction, 02, 70, 19, mnuNormColor);
           If ReadBit(RaConfig^.FailedScanAction, 02) then ClearBit(RaConfig^.FailedScanAction, 00);
         end; { NotAvail }
    04 : begin
           EditWord(RaConfig^.FailedScanArea, 70, 20, mnuEditColor, True);
           If RaConfig^.FailedScanArea<>00 then ClearBit(RaConfig^.FailedScanAction, 00);
         end; { Move Area }
  End; { Case }

 Until Choice=0;

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. EditFailedScan }

var Menu : PullRecord;
    Choice: Word;
    CH: Char;
    TempStr: String;
    SaveDirect: Boolean;
begin
 AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'DoUploadScan');
 InitPullMenu(Menu);
 AddPullItem(Menu.PullInf^[01], 'Scan online ', 01, #00, 'Yes=scan uploads immediately, No=scan after user disconnects', 1);
 AddPullItem(Menu.PullInf^[02], 'Failed scan ', 02, #00, 'Action to take on files which fail virus scan', 1);
 AddPullItem(Menu.PullInf^[03], 'Scan utility', 03, #00, 'Full DRIVE AND PATH to activate external virus-scan utility', 1);

 Menu.Items   := 3;
 Menu.Width   := 76;
 Menu.Length  := 3;
 Menu.X       := 2;
 Menu.Y       := 8;
 Menu.HiLight := 01;
 Menu.AddSpace:= True;
 Menu.Title   := ' Upload scanning ';
 Menu.PosArray[1].XInc := 00;
 Menu.PosArray[1].YDec := 00;

 ShowMenu(Menu, False);

 with GlobalCfg^ do
 Repeat;
  SaveDirect := DirectScrnUpdate;
  DirectscrnUpdate := false;

  TempStr := '';
  If ReadBit(RaConfig^.FailedScanAction, 00) then TempStr := 'Delete';
  If ReadBit(RaConfig^.FailedScanAction, 01) then TempStr := 'Unlisted';
  If ReadBit(RaConfig^.FailedScanAction, 02) then TempStr := TempStr + '/unavailable';
  If RaConfig^.FailedScanArea>00 then TempStr := TempStr + '/move to '+FStr(RaConfig^.FailedScanArea);
  If TempStr='' then TempStr := 'None';
  If TempStr[1]='/' then begin
                           TempStr := Copy(TempStr, 2, 255);
                            TempStr[1] := UpCase(TempStr[1]);
                         end; { Neat string }

  WriteAT(17, 10, mnuNormColor, Dup(#32, 60));
  WriteAT(17, 11, mnuNormColor, Dup(#32, 60));
  WriteAT(17, 12, mnuNormColor, Dup(#32, 60));

  WriteAT(17, 10, mnuNormColor, Ask2Str(Byte(RaConfig^.ScanNow)));
  WriteAT(17, 11, mnuNormColor, TempStr);
  WriteAT(17, 12, mnuNormColor, RaConfig^.ScanCmd);
  DirectScrnUpdate := SaveDirect;

  Choice := DoPullMenu(Menu, CH, False, False);

  Case Choice of
    01 : EditAskType(17, 10, Byte(RaConfig^.ScanNow), True, True, True, False);
    02 : begin
            EnDisAbleMenu(Menu, False);
            EditFailedScan;
            EnDisAbleMenu(Menu, True);
         end; { edit }
    03 : RaConfig^.ScanCmd := Edit(RaConfig^.ScanCmd, 17, 12, mnuEditColor, 60, False, False, True);
  End; { Case }

 Until Choice=0;

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. DoUploadScan }


Procedure DoManagerKeys;
var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    SaveDirect: Boolean;
begin
 AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'DoManagerKeys');
 InitPullMenu(Menu);
 AddPullItem(Menu.PullInf^[01], '1 ', 1, #00, 'Enter a DOS command-line, "@" will be replaced with the file name', 1);
 AddPullItem(Menu.PullInf^[02], '2 ', 2, #00, 'Enter a DOS command-line, "@" will be replaced with the file name', 1);
 AddPullItem(Menu.PullInf^[03], '3 ', 3, #00, 'Enter a DOS command-line, "@" will be replaced with the file name', 1);
 AddPullItem(Menu.PullInf^[04], '4 ', 4, #00, 'Enter a DOS command-line, "@" will be replaced with the file name', 1);
 AddPullItem(Menu.PullInf^[05], '5 ', 5, #00, 'Enter a DOS command-line, "@" will be replaced with the file name', 1);

 Menu.Items   := 5;
 Menu.Width   := 67;
 Menu.Length  := 5;
 Menu.X       := 7;
 Menu.Y       := 7;
 Menu.HiLight := 01;
 Menu.AddSpace:= True;
 Menu.Title   := ' ELEMGR ALT-function key manager ';
 Menu.PosArray[1].XInc := 00;
 Menu.PosArray[1].YDec := 00;

 ShowMenu(Menu, False);

 Repeat;
  SaveDirect := DirectScrnUpdate;
  DirectscrnUpdate := false;

  WriteAT(12, 09, mnuNormColor, GlobalCfg^.RaConfig^.RAMGRAltFKeys[1]);
  WriteAT(12, 10, mnuNormColor, GlobalCfg^.RaConfig^.RAMGRAltFKeys[2]);
  WriteAT(12, 11, mnuNormColor, GlobalCfg^.RaConfig^.RAMGRAltFKeys[3]);
  WriteAT(12, 12, mnuNormColor, GlobalCfg^.RaConfig^.RAMGRAltFKeys[4]);
  WriteAT(12, 13, mnuNormColor, GlobalCfg^.RaConfig^.RAMGRAltFKeys[5]);
  DirectScrnUpdate := SaveDirect;

  Choice := DoPullMenu(Menu, CH, False, False);

  with GlobalCfg^ do
  Case Choice of
      01 : RaConfig^.RaMgrAltFKeys[1] := Edit(RaConfig^.RaMgrAltFkeys[1], 12, 09, mnuEditColor, 60, False, False, True);
      02 : RaConfig^.RaMgrAltFKeys[2] := Edit(RaConfig^.RaMgrAltFkeys[2], 12, 10, mnuEditColor, 60, False, False, True);
      03 : RaConfig^.RaMgrAltFKeys[3] := Edit(RaConfig^.RaMgrAltFkeys[3], 12, 11, mnuEditColor, 60, False, False, True);
      04 : RaConfig^.RaMgrAltFKeys[4] := Edit(RaConfig^.RaMgrAltFkeys[4], 12, 12, mnuEditColor, 60, False, False, True);
      05 : RaConfig^.RaMgrAltFKeys[5] := Edit(RaConfig^.RaMgrAltFkeys[5], 12, 13, mnuEditColor, 60, False, False, True);
  End; { Case }

 Until Choice=0;

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. DoManagerKeys }


Procedure DoArchivers;

Procedure EditArcInfo(nr: Word; Menu: Pullrecord);
begin
  With GlobalCfg^.RaConfig^.ArcInfo[Nr] do
   begin
     Extension := Edit(Extension, 22, 09+Nr, mnuEditColor, 3, True, False, True);
     MakeEditBox(60, 63, 60, UnpackCmd, Extension + ' Unpack command-line. FULL DRIVE AND PATH!', false, Menu);
     MakeEditBox(60, 63, 60, PackCmd, Extension + ' Pack command-line. FULL DRIVE AND PATH!', false, Menu);
   end; { with }
end; { proc. EditArcInfo }

var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    Counter   : Byte;
    SaveDirect: Boolean;
begin
 AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'DoArchivers');
 InitPullMenu(Menu);
 AddPullItem(Menu.PullInf^[01], 'Arc 1', 01, #00, 'Archive format definition', 1);
 AddPullItem(Menu.PullInf^[02], 'Arc 2', 02, #00, 'Archive format definition', 1);
 AddPullItem(Menu.PullInf^[03], 'Arc 3', 03, #00, 'Archive format definition', 1);
 AddPullItem(Menu.PullInf^[04], 'Arc 4', 04, #00, 'Archive format definition', 1);
 AddPullItem(Menu.PullInf^[05], 'Arc 5', 05, #00, 'Archive format definition', 1);
 AddPullItem(Menu.PullInf^[06], 'Arc 6', 06, #00, 'Archive format definition', 1);
 AddPullItem(Menu.PullInf^[07], 'Arc 7', 07, #00, 'Archive format definition', 1);
 AddPullItem(Menu.PullInf^[08], 'Arc 8', 08, #00, 'Archive format definition', 1);
 AddPullItem(Menu.PullInf^[09], 'Arc 9', 09, #00, 'Archive format definition', 1);
 AddPullItem(Menu.PullInf^[10], 'Arc10', 10, #00, 'Archive format definition', 1);

 Menu.Items   := 10;
 Menu.Width   := 54;
 Menu.Length  := 10;
 Menu.X       := 14;
 Menu.Y       := 8;
 Menu.HiLight := 01;
 Menu.AddSpace:= True;
 Menu.Title   := ' Archivers ';
 Menu.PosArray[1].XInc := 00;
 Menu.PosArray[1].YDec := 00;

 ShowMenu(Menu, False);

 Repeat;
  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;

  For Counter:=01 to 10 do
   begin
     WriteAT(22, 09+Counter, mnuNormColor, GlobalCfg^.RaConfig^.ArcInfo[Counter].Extension);
     WriteAT(26, 09+Counter, mnuNormColor, MakeLen(GlobalCfg^.RaConfig^.ArcInfo[Counter].UnpackCmd, 20, Space, False, false));
     WriteAT(47, 09+Counter, mnuNormColor, MakeLen(GlobalCfg^.RaConfig^.ArcInfo[Counter].packCmd, 20, Space, False, false));
   end; { For Counter }
  DirectscrnUpdate := SaveDirect;

  Choice := DoPullMenu(Menu, CH, False, False);

  If Choice>00 then EditArcInfo(Choice, Menu);

 Until Choice=0;

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. DoArchivers }

Var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    SaveDirect: Boolean;
begin
 AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'DoArchivers');
 InitPullMenu(Menu);
 AddPullItem(Menu.PullInf^[01], 'Upload Credit ', 1001, #00, 'Extra time (in seconds) users receive for every 1 '+
                                                            'upload minute', 1);
 AddPullItem(Menu.PullInf^[02], 'Payback Credit', 1002, #00, 'Number of credits to give a user whenever one of his/her '+
                                                            'uploads is downloaded', 1);
 AddPullItem(Menu.PullInf^[03], 'Touch dates   ', 1003, #00, 'Set to Yes to set uploaded files to the current date', 1);
 AddPullItem(Menu.PullInf^[04], 'Show missing  ', 1004, #00, 'Set to Yes to display entries for missing files', 1);
 AddPullItem(Menu.PullInf^[05], 'Upload space  ', 1005, #00, 'Minimum free space (in Kb) required to permit uploads', 1);
 AddPullItem(Menu.PullInf^[06], 'Logon.newfiles', 1006, #00, 'Show new files since last call at logon?', 1);
 AddPullItem(Menu.PullInf^[07], 'Newfiles.tag  ', 1007, #00, 'Enable file tagging for Logon.Newfiles?', 1);
 AddPullItem(Menu.PullInf^[08], 'Download start', 1008, #00, 'Hours to permit downloads (START)', 1);
 AddPullItem(Menu.PullInf^[09], 'Download end  ', 1009, #00, 'Hours to permit downloads (END)', 1);
 AddPullItem(Menu.PullInf^[10], 'Download speed', 1010, #00, 'Minimum speed required for downloading', 1);
 AddPullItem(Menu.PullInf^[11], 'No dupe ext   ', 1011, #00, 'Ignore file extensions when checking for upload duplicates', 1);
 AddPullItem(Menu.PullInf^[12], 'DL description', 1012, #00, 'Allow users to download descriptions with file downloads', 1);
 AddPullItem(Menu.PullInf^[13], 'List format   ', 1013, #00, 'File list display format', 1);
 AddPullItem(Menu.PullInf^[14], 'Missing format', 1014, #00, 'File list display format (MISSING files)', 1);
 AddPullItem(Menu.PullInf^[15], 'Upload scan   ', 1015, #00, 'Sub-menu to configure upload scanning options', 1);
 AddPullItem(Menu.PullInf^[16], 'ELEMGR FnKeys ', 1016, #00, 'Configure the ELEMGR Alt-Function keys', 1);
 AddPullItem(Menu.PullInf^[17], 'Ext arcview   ', 1017, #00, 'Command-line for an external archive viewing utility', 1);
 AddPullItem(Menu.PullInf^[18], 'Archivers     ', 1018, #00, 'Command-lines for automatic archive conversion', 1);
 AddPullItem(Menu.PullInf^[19], 'Logoff warning', 1019, #00, 'Warn user about tagged files at logoff?', 1);
 AddPullItem(Menu.PullInf^[20], 'New CD areas  ', 1020, #00, 'Include CD-ROM areas in new files scan?', 1);

 Menu.Items   := 20;
 Menu.Width   := 23;
 Menu.Length  := 20;
 Menu.X       := 2;
 Menu.Y       := 1;
 Menu.HiLight := 01;
 Menu.AddSpace:= True;
 Menu.Title   := ' File options ';
 Menu.PosArray[1].XInc := 00;
 Menu.PosArray[1].YDec := 00;

 ShowMenu(Menu, False);

 with GlobalCfg^ do
 Repeat;
  SaveDirect := DirectScrnUpdate;
  DirectscrnUpdate := false;

  WriteAT(19, 03, mnuNormColor, FStr(RaConfig^.CreditFactor));
  WriteAT(19, 04, mnuNormColor, FStr(RaConfig^.FilePayBack));
  WriteAT(19, 05, mnuNormColor, Bool2Str(RaConfig^.FixUploadDates));
  WriteAT(19, 06, mnuNormColor, Bool2Str(RaConfig^.ShowMissingFiles));
  WriteAT(19, 07, mnuNormColor, FStr(RaConfig^.MinUpSpace));
  WriteAT(19, 08, mnuNormColor, Ask2Str(Byte(RaConfig^.CheckNewFiles)));
  WriteAT(19, 09, mnuNormColor, Bool2Str(RaConfig^.NewFileTag));
  WriteAT(19, 10, mnuNormColor, RaConfig^.DownloadTimeStart);
  WriteAT(19, 11, mnuNormColor, RaConfig^.DownloadTimeEnd);
  WriteAT(19, 12, mnuNormColor, FStr(FixBaud(RaConfig^.TransferBaud)));
  WriteAT(19, 13, mnuNormColor, Bool2Str(RaConfig^.IgnoreDupeExt));
  WriteAT(19, 14, mnuNormColor, Ask2Str(Byte(RaConfig^.DlDesc)));
  { List Format }
  { Missing Format }
  { Upload Scan }
  { EleMgr F<x> Keys }
  { External Archiveviewr }
  { Archivers }
  WriteAT(19, 21, mnuNormColor, Bool2Str(RaConfig^.TagLogOffWarning));
  WriteAT(19, 22, mnuNormColor, Bool2Str(RaConfig^.IncludeNewCDareas));
  DirectScrnUpdate := SaveDirect;

  Choice := DoPullMenu(Menu, CH, False, False);

  If (Choice=1015) OR (Choice=1016) OR (Choice=1018) then
   EnDisAbleMenu(Menu, False);

  Case Choice of
      1001 : EditInteger(RaConfig^.CreditFactor, 19, 03, mnuEditColor, 4, True);
      1002 : EditByte(RaConfig^.FilePayBack, 19, 04, mnuEditColor, True);
      1003 : Edit_Boolean(RaConfig^.FixUploadDates, 19, 05, mnuNormColor);
      1004 : Edit_Boolean(RaConfig^.ShowMissingFiles, 19, 06, mnuNormColor);
      1005 : EditWord(RaConfig^.MinUpSpace, 19, 07, mnuEditColor, True);
      1006 : EditAskType(19, 08, Byte(RaConfig^.CheckNewFiles), True, True, True, False);
      1007 : Edit_Boolean(RaConfig^.NewFileTag, 19, 09, mnuNormColor);
      1008 : EditTime(RaConfig^.DownloadTimeStart, 19, 10, True);
      1009 : EditTime(RaConfig^.DownloadTimeEnd, 19, 11, True);
      1010 : EditBaudRate(RaConfig^.TransferBaud, 19, 12);
      1011 : Edit_Boolean(RaConfig^.IgnoreDupeExt, 19, 13, mnuNormColor);
      1012 : EditAskType(19, 14, Byte(RaConfig^.DlDesc), True, True, True, False);
      1013 : MakeEditBox(200, 64, 60, RaConfig^.FileLine, ' File list format ', True, Menu);
      1014 : MakeEditBox(200, 64, 60, RaConfig^.FileMissingLine, ' Missing file list format ', True, Menu);
      1015 : DoUploadScan;
      1016 : DoManagerKeys;
      1017 : MakeEditBox(60, 64, 60, RaConfig^.ArcViewCmd, ' External archive viewer ', False, Menu);
      1018 : DoArchivers;
      1019 : Edit_Boolean(RaConfig^.TagLogOffWarning, 19, 21, mnuNormColor);
      1020 : Edit_Boolean(RaConfig^.IncludeNewCDareas, 19, 22, mnuNormColor);
  End; { Case }

  If (Choice=1015) OR (Choice=1016) OR (Choice=1018) then
   EnDisAbleMenu(Menu, True);
 Until Choice=0;

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. DoFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoRestrictions;
var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    SaveDirect: Boolean;
begin
 AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'DoRestrictions');
 InitPullMenu(Menu);
 AddPullItem(Menu.PullInf^[01], 'Logon speed', 1101, #00, 'Minimum speed required to log-on', 1);
 AddPullItem(Menu.PullInf^[02], 'ANSI speed ', 1102, #00, 'Minimum speed required for ANSI graphics', 1);
 AddPullItem(Menu.PullInf^[03], 'No300 start', 1103, #00, 'Hours to deny 300 baud access (START)', 1);
 AddPullItem(Menu.PullInf^[04], 'No300 end  ', 1104, #00, 'Hours to deny 300 baud access (END)', 1);

 Menu.Items   := 4;
 Menu.Width   := 22;
 Menu.Length  := 4;
 Menu.X       := 2;
 Menu.Y       := 4;
 Menu.HiLight := 01;
 Menu.AddSpace:= True;
 Menu.Title   := ' Restrictions ';
 Menu.PosArray[1].XInc := 00;
 Menu.PosArray[1].YDec := 00;

 ShowMenu(Menu, False);

 Repeat;
  SaveDirect := DirectScrnUpdate;
  DirectscrnUpdate := false;

  WriteAT(17, 06, mnuNormColor, FStr(FixBaud(GlobalCfg^.RaConfig^.MinimumBaud)));
  WriteAT(17, 07, mnuNormColor, FStr(FixBaud(GlobalCfg^.RaConfig^.GraphicsBaud)));
  WriteAT(17, 08, mnuNormColor, GlobalCfg^.RaConfig^.SlowBaudTimeStart);
  WriteAT(17, 09, mnuNormColor, GlobalCfg^.RaConfig^.SlowBaudTimeEnd);
  DirectScrnUpdate := SaveDirect;

  Choice := DoPullMenu(Menu, CH, False, False);

  Case Choice of
    1101 : EditBaudRate(GlobalCfg^.RaConfig^.MinimumBaud, 17, 06);
    1102 : EditBaudRate(GlobalCfg^.RaConfig^.GraphicsBaud, 17, 07);
    1103 : EditTime(GlobalCfg^.RaConfig^.SlowBaudTimeStart, 17, 08, True);
    1104 : EditTime(GlobalCfg^.RaConfig^.SlowBaudTimeEnd, 17, 09, True);
  End; { Case }

 Until Choice=0;

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. DoRestrictions }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure DoErrorLevels;
Var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    SaveDirect: Boolean;
begin

 AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'DoErrorLevels');
 InitPullMenu(Menu);
 AddPullItem(Menu.PullInf^[01], 'Local ', 1201, #00, 'Frontend exit errorlevel values', 1);
 AddPullItem(Menu.PullInf^[02], '300   ', 1202, #00, 'Frontend exit errorlevel values', 1);
 AddPullItem(Menu.PullInf^[03], '1200  ', 1203, #00, 'Frontend exit errorlevel values', 1);
 AddPullItem(Menu.PullInf^[04], '2400  ', 1204, #00, 'Frontend exit errorlevel values', 1);
 AddPullItem(Menu.PullInf^[05], '4800  ', 1205, #00, 'Frontend exit errorlevel values', 1);
 AddPullItem(Menu.PullInf^[06], '7200  ', 1206, #00, 'Frontend exit errorlevel values', 1);
 AddPullItem(Menu.PullInf^[07], '9600  ', 1207, #00, 'Frontend exit errorlevel values', 1);
 AddPullItem(Menu.PullInf^[08], '12000 ', 1208, #00, 'Frontend exit errorlevel values', 1);
 AddPullItem(Menu.PullInf^[09], '14400 ', 1209, #00, 'Frontend exit errorlevel values', 1);
 AddPullItem(Menu.PullInf^[10], '16800 ', 1210, #00, 'Frontend exit errorlevel values', 1);
 AddPullItem(Menu.PullInf^[11], '19200 ', 1211, #00, 'Frontend exit errorlevel values', 1);
 AddPullItem(Menu.PullInf^[12], '21600 ', 1212, #00, 'Frontend exit errorlevel values', 2);
 AddPullItem(Menu.PullInf^[13], '24000 ', 1213, #00, 'Frontend exit errorlevel values', 2);
 AddPullItem(Menu.PullInf^[14], '26400 ', 1214, #00, 'Frontend exit errorlevel values', 2);
 AddPullItem(Menu.PullInf^[15], '28800 ', 1215, #00, 'Frontend exit errorlevel values', 2);
 AddPullItem(Menu.PullInf^[16], '31200 ', 1216, #00, 'Frontend exit errorlevel values', 2);
 AddPullItem(Menu.PullInf^[17], '33600 ', 1217, #00, 'Frontend exit errorlevel values', 2);
 AddPullItem(Menu.PullInf^[18], '38400 ', 1218, #00, 'Frontend exit errorlevel values', 2);
 AddPullItem(Menu.PullInf^[19], '57600 ', 1219, #00, 'Frontend exit errorlevel values', 2);
 AddPullItem(Menu.PullInf^[20], '64000 ', 1220, #00, 'Frontend exit errorlevel values', 2);
 AddPullItem(Menu.PullInf^[21], '115200', 1221, #00, 'Frontend exit errorlevel values', 2);
 AddPullItem(Menu.PullInf^[22], 'Fax   ', 1222, #00, 'Frontend exit errorlevel values', 2);

 Menu.Items   := 22;
 Menu.Width   := 31;
 Menu.Length  := 11;
 Menu.X       := 2;
 Menu.Y       := 2;
 Menu.HiLight := 01;
 Menu.AddSpace:= True;
 Menu.Title   := ' Errorlevels ';
 Menu.PosArray[1].XInc := 00;
 Menu.PosArray[1].YDec := 00;
 Menu.PosArray[2].XInc := 17;
 Menu.PosArray[2].YDec := 11;

 ShowMenu(Menu, False);

 with GlobalCfg^ do
 Repeat;
  SaveDirect := DirectScrnUpdate;
  DirectscrnUpdate := false;

  WriteAT(11, 04, mnuNormColor, FStr(RaConfig^.ExitLocal));
  WriteAT(11, 05, mnuNormColor, FStr(RaConfig^.Exit300));
  WriteAT(11, 06, mnuNormColor, FStr(RaConfig^.Exit1200));
  WriteAT(11, 07, mnuNormColor, FStr(RaConfig^.Exit2400));
  WriteAT(11, 08, mnuNormColor, FStr(RaConfig^.Exit4800));
  WriteAT(11, 09, mnuNormColor, FStr(RaConfig^.Exit7200));
  WriteAT(11, 10, mnuNormColor, FStr(RaConfig^.Exit9600));
  WriteAT(11, 11, mnuNormColor, FStr(RaConfig^.Exit12000));
  WriteAT(11, 12, mnuNormColor, FStr(RaConfig^.Exit14400));
  WriteAT(11, 13, mnuNormColor, FStr(RaConfig^.Exit16k));
  WriteAT(11, 14, mnuNormColor, FStr(RaConfig^.Exit19k));
  WriteAT(28, 04, mnuNormColor, FStr(RaConfig^.Exit21k));
  WriteAT(28, 05, mnuNormColor, FStr(RaConfig^.Exit24k));
  WriteAT(28, 06, mnuNormColor, FStr(RaConfig^.Exit26k));
  WriteAT(28, 07, mnuNormColor, FStr(RaConfig^.Exit28k));
  WriteAT(28, 08, mnuNormColor, FStr(RaConfig^.Exit31k));
  WriteAT(28, 09, mnuNormColor, FStr(RaConfig^.Exit33k));
  WriteAT(28, 10, mnuNormColor, FStr(RaConfig^.Exit38k));
  WriteAT(28, 11, mnuNormColor, FStr(RaConfig^.Exit57k));
  WriteAT(28, 12, mnuNormColor, FStr(RaConfig^.Exit64k));
  WriteAT(28, 13, mnuNormColor, FStr(RaConfig^.Exit115k));
  WriteAT(28, 14, mnuNormColor, FStr(RaConfig^.ExitFax));
  DirectScrnUpdate := SaveDirect;

  Choice := DoPullMenu(Menu, CH, False, False);

  Case Choice of
      1201 : EditByte(RaConfig^.ExitLocal, 11, 04, mnuEditColor, True);
      1202 : EditByte(RaConfig^.Exit300, 11, 05, mnuEditColor, True);
      1203 : EditByte(RaConfig^.Exit1200, 11, 06, mnuEditColor, True);
      1204 : EditByte(RaConfig^.Exit2400, 11, 07, mnuEditColor, True);
      1205 : EditByte(RaConfig^.Exit4800, 11, 08, mnuEditColor, True);
      1206 : EditByte(RaConfig^.Exit7200, 11, 09, mnuEditColor, True);
      1207 : EditByte(RaConfig^.Exit9600, 11, 10, mnuEditColor, True);
      1208 : EditByte(RaConfig^.Exit12000, 11, 11, mnuEditColor, True);
      1209 : EditByte(RaConfig^.Exit14400, 11, 12, mnuEditColor, True);
      1210 : EditByte(RaConfig^.Exit16k, 11, 13, mnuEditColor, True);
      1211 : EditByte(RaConfig^.Exit19k, 11, 14, mnuEditColor, True);
      1212 : EditByte(RaConfig^.Exit21k, 28, 04, mnuEditColor, True);
      1213 : EditByte(RaConfig^.Exit24k, 28, 05, mnuEditColor, True);
      1214 : EditByte(RaConfig^.Exit26k, 28, 06, mnuEditColor, True);
      1215 : EditByte(RaConfig^.Exit28k, 28, 07, mnuEditColor, True);
      1216 : EditByte(RaConfig^.Exit31k, 28, 08, mnuEditColor, True);
      1217 : EditByte(RaConfig^.Exit33k, 28, 09, mnuEditColor, True);
      1218 : EditByte(RaConfig^.Exit38k, 28, 10, mnuEditColor, True);
      1219 : EditByte(RaConfig^.Exit57k, 28, 11, mnuEditColor, True);
      1220 : EditByte(RaConfig^.Exit64k, 28, 12, mnuEditColor, True);
      1221 : EditByte(RaConfig^.Exit115k, 28, 13, mnuEditColor, True);
      1222 : EditByte(RaConfig^.ExitFax, 28, 14, mnuEditColor, True);
  End; { Case }

 Until Choice=0;

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. DoErrorLevels }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure DoPrompts;
Var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    TempStr   : String;
    SaveDirect: Boolean;
begin
 AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'DoPrompts');
 InitPullMenu(Menu);
 AddPullItem(Menu.PullInf^[01], 'Logon prompt   ', 1801, #00, 'Prompt to ask for the user''s name', 1);
 AddPullItem(Menu.PullInf^[02], 'Left bracket   ', 1802, #00, 'Left bracket character', 1);
 AddPullItem(Menu.PullInf^[03], 'Right bracket  ', 1803, #00, 'Right bracket character', 1);
 AddPullItem(Menu.PullInf^[04], 'Language prompt', 1804, #00, 'Prompt to ask for the user''s preferred language', 1);
 AddPullItem(Menu.PullInf^[05], 'Language header', 1805, #00, 'Header that appears above the list of available languages', 1);

 Menu.Items   := 5;
 Menu.Width   := 61;
 Menu.Length  := 5;
 Menu.X       := 2;
 Menu.Y       := 14;
 Menu.HiLight := 01;
 Menu.AddSpace:= True;
 Menu.Title   := ' System prompts ';
 Menu.PosArray[1].XInc := 00;
 Menu.PosArray[1].YDec := 00;

 ShowMenu(Menu, False);

 with GlobalCfg^ do
 Repeat;
  SaveDirect := DirectScrnUpdate;
  DirectscrnUpdate := false;
  WriteAT(20, 16, mnuNormColor, RaConfig^.LogonPrompt);
  WriteAT(20, 17, mnuNormColor, RaConfig^.LeftBracket);
  WriteAT(20, 18, mnuNormColor, RaConfig^.RightBracket);
  WriteAT(20, 19, mnuNormColor, RaConfig^.LanguagePrompt);
  WriteAT(20, 20, mnuNormColor, RaConfig^.LangHdr);
  DirectScrnUpdate := SaveDirect;

  Choice := DoPullMenu(Menu, CH, False, False);

  Case Choice of
    1801 : RaConfig^.LogonPrompt := Edit(RaConfig^.LogonPrompt, 20, 16, mnuEditColor, 40, False, True, True);
    1802 : begin
              TempStr := Edit(RaConfig^.LeftBracket, 20, 17, mnuEditColor, 1, False, False, True);
              RaConfig^.LeftBracket := TempStr[1];
           end; { LeftBracket }
    1803 : begin
              Tempstr := Edit(RaConfig^.RightBracket, 20, 18, mnuEditColor, 1, False, False, True);
              RaConfig^.RightBracket := TempStr[1];
           end; { RightBracket }
    1804 : RaConfig^.LanguagePrompt:= Edit(RaConfig^.LanguagePrompt, 20, 19, mnuEditColor, 40, False, False, True);
    1805 : RaConfig^.LangHdr:= Edit(RaConfig^.LangHdr, 20, 20, mnuEditColor, 40, False, False, True);
  End; { Case }

 Until Choice=0;

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. DoPrompts }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoPaging;
var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    SaveDirect: Boolean;
    TmpByte   : Byte;
begin
 AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'DoPaging');
 InitPullMenu(Menu);

 With Menu do
   begin
     AddPullItem(PullInf^[01], 'Duration  ', 1501, #00, 'Duration of page bell (in seconds)', 1);
     AddPullItem(PullInf^[02], 'Max number', 1502, #00, 'Maximum number of times a user may page per call', 1);
     AddPullItem(PullInf^[03], 'Ask why   ', 1503, #00, 'Ask the user for a paging reason?', 1);
     AddPullItem(PullInf^[04], 'Sysop msgs', 1504, #00, 'Area to post a msg in if the page was not answered '+
                                                        '(0 to disable)', 1);
     AddPullItem(PullInf^[05], 'External  ', 1505, #00, 'DOS command line to invoke an external chat utility', 1);
     AddPullItem(PullInf^[06], 'Suspend   ', 1506, #00, 'Suspend user''s time during chat?', 1);
     AddPullItem(PullInf^[07], 'Auto log  ', 1507, #00, 'Open chat capture log automatically?', 1);
     AddPullItem(PullInf^[08], '',           1508, #00, '', 1);
     AddPullItem(PullInf^[09], 'Sunday    ', 1509, #00, 'Paging start/end times', 1);
     AddPullItem(PullInf^[10], 'Monday    ', 1510, #00, 'Paging start/end times', 1);
     AddPullItem(PullInf^[11], 'Tuesday   ', 1511, #00, 'Paging start/end times', 1);
     AddPullItem(PullInf^[12], 'Wednesday ', 1512, #00, 'Paging start/end times', 1);
     AddPullItem(PullInf^[13], 'Thursday  ', 1513, #00, 'Paging start/end times', 1);
     AddPullItem(PullInf^[14], 'Friday    ', 1514, #00, 'Paging start/end times', 1);
     AddPullItem(PullInf^[15], 'Saturday  ', 1515, #00, 'Paging start/end times', 1);
   end; { with }

 Menu.PullInf^[08].mnuSelect := False;

 Menu.Items   := 15;
 Menu.Width   := 25;
 Menu.Length  := 15;
 Menu.X       := 2;
 Menu.Y       := 4;
 Menu.HiLight := 01;
 Menu.AddSpace:= True;
 Menu.Title   := ' Paging ';
 Menu.PosArray[1].XInc := 00;
 Menu.PosArray[1].YDec := 00;

 ShowMenu(Menu, False);

 with GlobalCfg^ do
 Repeat;
  SaveDirect := DirectScrnUpdate;
  DirectscrnUpdate := false;
  WriteAT(15, 06, mnuNormColor, FStr(RaConfig^.PageLength));
  WriteAT(15, 07, mnuNormColor, FStr(RaConfig^.MaxPage));
  WriteAT(15, 08, mnuNormColor, Bool2Str(RaConfig^.WhyPage));
  WriteAT(15, 09, mnuNormColor, FStr(RaConfig^.LeaveMsg));
  { External Chat utility }
  WriteAT(15, 11, mnuNormColor, Bool2Str(RaConfig^.FreezeChat));
  WriteAT(15, 12, mnuNormColor, Bool2Str(RaConfig^.AutoChatCapture));
  WriteAT(15, 14, mnuNormColor, RaConfig^.PageStart[0] + ' ' + RaConfig^.PageEnd[0]);
  WriteAT(15, 15, mnuNormColor, RaConfig^.PageStart[1] + ' ' + RaConfig^.PageEnd[1]);
  WriteAT(15, 16, mnuNormColor, RaConfig^.PageStart[2] + ' ' + RaConfig^.PageEnd[2]);
  WriteAT(15, 17, mnuNormColor, RaConfig^.PageStart[3] + ' ' + RaConfig^.PageEnd[3]);
  WriteAT(15, 18, mnuNormColor, RaConfig^.PageStart[4] + ' ' + RaConfig^.PageEnd[4]);
  WriteAT(15, 19, mnuNormColor, RaConfig^.PageStart[5] + ' ' + RaConfig^.PageEnd[5]);
  WriteAT(15, 20, mnuNormColor, RaConfig^.PageStart[6] + ' ' + RaConfig^.PageEnd[6]);
  DirectScrnUpdate := SaveDirect;

  Choice := DoPullMenu(Menu, CH, False, False);

  Case Choice of
       1501 : begin
                Tmpbyte := RaConfig^.PageLength;
                EditByte(TmpByte, 15, 06, mnuEditColor, True);
                RaConfig^.PageLength := TmpByte;
              end;
       1502 : begin
                TmpByte := RaConfig^.MaxPage;
                EditByte(TmpByte, 15, 07, mnuEditColor, True);
                RaConfig^.MaxPage := TmpByte;
              end;
       1503 : Edit_Boolean(RaConfig^.WhyPage, 15, 08, mnuNormColor);
       1504 : EditByte(RaConfig^.LeaveMsg, 15, 09, mnuEditColor, True);
       1505 : MakeEditBox(60, 64, 60, RaConfig^.ChatCommand, ' External chat utility ', False, Menu);
       1506 : Edit_Boolean(RaConfig^.FreezeChat, 15, 11, mnuNormColor);
       1507 : Edit_Boolean(RaConfig^.AutoChatCapture, 15, 11, mnuNormColor);
       1508 : ; { Empty slot }
 1509..1516 : begin
                EditTime(RaConfig^.PageStart[Choice-1509], 15, Choice-1495, True);
                EditTime(RaConfig^.PageEnd[Choice-1509], 21, Choice-1495, True);
              end; { PageStart }
  End; { Case }

 Until Choice=0;

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. DoPaging }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function FormatDate2Str(B: Byte): String;
begin
  FormatDate2Str := '';

  Case B of
    0 : FormatDate2Str := 'Ask        ';
    1 : FormatDate2Str := 'DD-MM-YY   ';
    2 : FormatDate2Str := 'MM-DD-YY   ';
    3 : FormatDate2Str := 'YY-MM-DD   ';
    4 : FormatDate2Str := 'DD-Mmm-YY  ';
    5 : FormatDate2Str := 'DD-MM-YYYY ';
    6 : FormatDate2Str := 'MM-DD-YYYY ';
    7 : FormatDate2Str := 'YYYY-MM-DD ';
    8 : FormatDate2Str := 'DD-Mmm-YYYY';
  End; { Case }
end; { func. FormatDate2Str }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure DoNewUser;
Var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    TempStr   : String;
    B         : Byte;
    SaveDirect: Boolean;
begin
 AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'DoNewUser');
 InitPullMenu(Menu);
 AddPullItem(Menu.PullInf^[01], 'Security ', 1601, #00, 'New user default security level (1 to 65535, or 0 '+
                                                        'for a private system)', 1);
 AddPullItem(Menu.PullInf^[02], 'A flag   ', 1602, #00, 'New user default A flag settings', 1);
 AddPullItem(Menu.PullInf^[03], 'B flag   ', 1603, #00, 'New user default B flag settings', 1);
 AddPullItem(Menu.PullInf^[04], 'C flag   ', 1604, #00, 'New user default C flag settings', 1);
 AddPullItem(Menu.PullInf^[05], 'D flag   ', 1605, #00, 'New user default D flag settings', 1);
 AddPullItem(Menu.PullInf^[06], 'Credit   ', 1606, #00, 'Credit awarded to new users', 1);
 AddPullItem(Menu.PullInf^[07], 'Group    ', 1607, #00, 'Default group number for new users', 1);
 AddPullItem(Menu.PullInf^[08], 'ANSI     ', 1608, #00, 'Enable ANSI for new users?', 1);
 AddPullItem(Menu.PullInf^[09], 'AVATAR   ', 1609, #00, 'Enable AVATAR (AVT/0+) for new users?', 1);
 AddPullItem(Menu.PullInf^[10], 'Clrscr   ', 1610, #00, 'Enable clear screen codes for new users?', 1);
 AddPullItem(Menu.PullInf^[11], 'More     ', 1611, #00, 'Enable more prompting for new users?', 1);
 AddPullItem(Menu.PullInf^[12], 'Sub days ', 1612, #00, 'Default subscription length for new users (days) (1 to 255 or 0 for no expiry)', 1);
 AddPullItem(Menu.PullInf^[13], 'UL credit', 1613, #00, 'Number of uploads to credit new users with', 1);
 AddPullItem(Menu.PullInf^[14], 'EchoCheck', 1614, #00, 'Include EchoMail in mail-box scan?', 1);
 AddPullItem(Menu.PullInf^[15], 'Sex      ', 1615, #00, 'Ask new users for their sex?', 1);

 AddPullItem(Menu.PullInf^[16], 'Voice phone   ', 1616, #00, 'Ask new users for voice telephone number?', 2);
 AddPullItem(Menu.PullInf^[17], 'Data phone    ', 1617, #00, 'Ask new users for data telephone number?', 2);
 AddPullItem(Menu.PullInf^[18], 'One word names', 1618, #00, 'Permit new users to use one word names?', 2);
 AddPullItem(Menu.PullInf^[19], 'Handle        ', 1619, #00, 'Ask new users for a handle/alias?', 2);
 AddPullItem(Menu.PullInf^[20], 'Birthdate     ', 1620, #00, 'Ask new users for a date of birth?', 2);
 AddPullItem(Menu.PullInf^[21], 'Hotkeys       ', 1621, #00, 'Enable hotkeys for new users?', 2);
 AddPullItem(Menu.PullInf^[22], 'FS msg view   ', 1622, #00, 'Enable the full screen message viewer for new users?', 2);
 AddPullItem(Menu.PullInf^[23], 'FS msg edit   ', 1623, #00, 'Enable the full screen message editor for new users?', 2);
 AddPullItem(Menu.PullInf^[24], 'IEMSI         ', 1624, #00, 'Permit new users to autologon with Interactive EMSI?', 2);
 AddPullItem(Menu.PullInf^[25], 'Language      ', 1625, #00, 'Default language number for new users (0 means don''t ask '+
                                                             'for preference)', 2);
 AddPullItem(Menu.PullInf^[26], 'Date format   ', 1626, #00, 'Default date format for new users, or Ask '+
                                                             'to allow the user to select one', 2);
 AddPullItem(Menu.PullInf^[27], 'Cap location  ', 1627, #00, 'Auto-capitalise new users location?', 2);
 AddPullItem(Menu.PullInf^[28], 'UL creditK    ', 1628, #00, 'Number of kilobytes of uploads to credit new users with', 2);
 AddPullItem(Menu.PullInf^[29], 'Mail address  ', 1629, #00, 'Ask new users for a 3-line mailing address', 2);
 AddPullItem(Menu.PullInf^[30], 'Telephone scan', 1630, #00, 'Check new user telephone number for duplicates?', 2);

 Menu.Items   := 30;
 Menu.Width   := 52;
 Menu.Length  := 15;
 Menu.X       := 15;
 Menu.Y       := 4;
 Menu.HiLight := 01;
 Menu.AddSpace:= True;
 Menu.Title   := ' New user defaults ';
 Menu.PosArray[1].XInc := 00;
 Menu.PosArray[1].YDec := 00;
 Menu.PosArray[2].XInc := 23;
 Menu.PosArray[2].YDec := 15;

 ShowMenu(Menu, False);

 with GlobalCfg^ do
 Repeat;
  SaveDirect := DirectScrnUpdate;
  DirectscrnUpdate := false;
  WriteAT(27, 06, mnuNormColor, FStr(RaConfig^.NewSecurity));
  WriteAT(27, 07, mnuNormColor, Byte2FlagsOff(RaConfig^.NewFlags[1], 00));
  WriteAT(27, 08, mnuNormColor, Byte2FlagsOff(RaConfig^.NewFlags[2], 00));
  WriteAT(27, 09, mnuNormColor, Byte2FlagsOff(RaConfig^.NewFlags[3], 00));
  WriteAT(27, 10, mnuNormColor, Byte2FlagsOff(RaConfig^.NewFlags[4], 00));
  WriteAT(27, 11, mnuNormColor, FStr(RaConfig^.NewCredit));
  WriteAT(27, 12, mnuNormColor, FStr(RaConfig^.NewUserGroup));
  WriteAT(27, 13, mnuNormColor, Ask2Str(Byte(RaConfig^.Ansi)));
  WriteAT(27, 14, mnuNormColor, Ask2Str(Byte(RaConfig^.Avatar)));
  WriteAT(27, 15, mnuNormColor, Ask2Str(Byte(RaConfig^.ClearScreen)));
  WriteAT(27, 16, mnuNormColor, Ask2Str(Byte(RaConfig^.MorePrompt)));
  WriteAT(27, 17, mnuNormColor, FStr(RaConfig^.NewUserSub));
  WriteAT(27, 18, mnuNormColor, FStr(RaConfig^.NewUserUlCredit));
  WriteAT(27, 19, mnuNormColor, Ask2Str(Byte(RaConfig^.EchoCheck)));
  WriteAT(27, 20, mnuNormColor, Bool2Str(RaConfig^.AskForSex));

  WriteAT(55, 06, mnuNormColor, Bool2Str(RaConfig^.AskVoicePhone));
  WriteAT(55, 07, mnuNormColor, Bool2Str(RaConfig^.AskDataPhone));
  WriteAT(55, 08, mnuNormColor, Bool2Str(RaConfig^.OneWordNames));
  WriteAT(55, 09, mnuNormColor, Bool2Str(RaConfig^.AskForHandle));
  WriteAT(55, 10, mnuNormColor, Bool2Str(RaConfig^.AskForBirthdate));
  WriteAT(55, 11, mnuNormColor, Ask2Str(Byte(RaConfig^.HotKeys)));
  WriteAT(55, 12, mnuNormColor, Ask2Str(Byte(RaConfig^.FullMsgView)));
  WriteAT(55, 13, mnuNormColor, Ask2Str(Byte(RaConfig^.ExtEd)));
  WriteAT(55, 14, mnuNormColor, Bool2Str(RaConfig^.EMSI_NewUser));
  WriteAT(55, 15, mnuNormColor, FStr(RaConfig^.NewUserLanguage));
  WriteAT(55, 16, mnuNormColor, FormatDate2Str(RaConfig^.NewUserDateFormat));
  WriteAT(55, 17, mnuNormColor, Bool2Str(RaConfig^.CapLocation));
  WriteAT(55, 18, mnuNormColor, FStr(RaConfig^.NewUserUlCreditK));
  WriteAT(55, 19, mnuNormColor, Bool2Str(RaConfig^.AskForAddress));
  WriteAT(55, 20, mnuNormColor, Bool2Str(RaConfig^.NewPhoneScan));
  DirectScrnUpdate := SaveDirect;

  Choice := DoPullMenu(Menu, CH, False, False);

  Case Choice of
      1601 : EditWord(RaConfig^.NewSecurity, 27, 06, mnuEditColor, True);
      1602 : EditFlags(RaConfig^.NewFlags[01], B, False, 60, 03, Menu);
      1603 : EditFlags(RaConfig^.NewFlags[02], B, False, 60, 03, Menu);
      1604 : EditFlags(RaConfig^.NewFlags[03], B, False, 60, 03, Menu);
      1605 : EditFlags(RaConfig^.NewFlags[04], B, False, 60, 03, Menu);
      1606 : EditWord(RaConfig^.NewCredit, 27, 11, mnuEditColor, True);
      1607 : EditByte(RaConfig^.NewUserGroup, 27, 12, mnuEditColor, True);
      1608 : EditAskType(27, 13, Byte(RaConfig^.Ansi), True, True, True, False);
      1609 : EditAskType(27, 14, Byte(RaConfig^.Avatar), True, True, True, False);
      1610 : EditAskType(27, 15, Byte(RaConfig^.ClearScreen), True, True, True, False);
      1611 : EditAskType(27, 16, Byte(RaConfig^.MorePrompt), True, True, True, False);
      1612 : EditByte(RaConfig^.NewUserSub, 27, 17, mnuEditColor, True);
      1613 : EditByte(RaConfig^.NewUserUlCredit, 27, 18, mnuEditColor, True);
      1614 : EditAskType(27, 19, Byte(RaConfig^.EchoCheck), True, True, True, false);
      1615 : Edit_Boolean(RaConfig^.AskForSex, 27, 20, mnuNormColor);
      1616 : Edit_Boolean(RaConfig^.AskVoicePhone, 55, 06, mnuNormColor);
      1617 : Edit_Boolean(RaConfig^.AskDataPhone, 55, 07, mnuNormColor);
      1618 : Edit_Boolean(RaConfig^.OneWordNames, 55, 08, mnuNormColor);
      1619 : Edit_Boolean(RaConfig^.AskForHandle, 55, 09, mnuNormColor);
      1620 : Edit_Boolean(RaConfig^.AskForBirthDate, 55, 10, mnuNormColor);
      1621 : EditAskType(55, 11, Byte(RaConfig^.HotKeys), True, True, True, false);
      1622 : EditAskType(55, 12, Byte(RaConfig^.FullMsgView), True, True, True, false);
      1623 : EditAskType(55, 13, Byte(RaConfig^.ExtEd), True, True, True, false);
      1624 : Edit_Boolean(RaConfig^.Emsi_NewUser, 55, 14, mnuNormColor);
      1625 : EditByte(RaConfig^.NewUserLanguage, 55, 15, mnuEditColor, True);
      1626 : If RaConfig^.NewUserDateFormat<8 then Inc(RaConfig^.NewUserDateFormat)
              else RaConfig^.NewUserDateFormat := 00;
      1627 : Edit_Boolean(RaConfig^.CapLocation, 55, 17, mnuNormColor);
      1628 : EditWord(RaConfig^.NewUserUlCreditK, 55, 18, mnuEditColor, True);
      1629 : Edit_Boolean(RaConfig^.AskForAddress, 55, 19, mnuNormColor);
      1630 : Edit_Boolean(RaConfig^.NewPhoneScan, 55, 20, mnuNormColor);
  End; { Case }

 Until Choice=0;

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. DoNewUser }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure DoSystem;
Var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    TempStr   : String;
    SaveDirect: Boolean;
    TmpByte   : Byte;
begin
 AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'DoSystem');
 InitPullMenu(Menu);
 AddPullItem(Menu.PullInf^[01], 'Fast logons  ', 1801, #00, 'Permit local fast sysop logons?', 1);
 AddPullItem(Menu.PullInf^[02], 'Check multi  ', 1802, #00, 'Check that a user does not log on to more than 1 '+
                                                            'node simultaneously?', 1);
 AddPullItem(Menu.PullInf^[03], 'Remote sysop ', 1803, #00, 'Allow the system operator to log in remotely?', 1);
 AddPullItem(Menu.PullInf^[04], 'Exclude sysop', 1804, #00, 'Exclude the system operator from userlists?', 1);
 AddPullItem(Menu.PullInf^[05], 'Text shells  ', 1805, #00, 'Allow shelling from text files?', 1);
 AddPullItem(Menu.PullInf^[06], 'Log style    ', 1806, #00, 'Type of logging', 1);
 AddPullItem(Menu.PullInf^[07], 'Multi node   ', 1807, #00, 'Running a multi-node system?', 1);
 AddPullItem(Menu.PullInf^[08], 'Environment  ', 1808, #00, 'AT BIOS, DoubleDOS, DESQview, TopView, MultiLink, '+
                                                            'PC-MOS, MS-Windows, OS/2', 1);
 AddPullItem(Menu.PullInf^[09], 'Screen blank ', 1809, #00, 'Number of seconds to blank the screen after inactivity '+
                                                            '(0 to disable)', 1);
 AddPullItem(Menu.PullInf^[10], 'After msgs   ', 1810, #00, 'Seconds to pause after system messages (0 = wait for '+
                                                            'Enter key)', 1);
 AddPullItem(Menu.PullInf^[11], 'ALT-J swap   ', 1811, #00, 'Swap to disk/EMS for jump to DOS (ALT-J)', 1);
 AddPullItem(Menu.PullInf^[12], 'Use XMS      ', 1812, #00, 'Use XMS for swap storage', 1);
 AddPullItem(Menu.PullInf^[13], 'Use EMS      ', 1813, #00, 'Use EMS for swap storage', 1);
 AddPullItem(Menu.PullInf^[14], 'Save password', 1814, #00, 'Yes=Users passwords will be stored as text. '+
                                                             'No=Passwords will be encrypted', 1);
 AddPullItem(Menu.PullInf^[15], 'IEMSI        ', 1815, #00, 'Enable Interactive EMSI?', 1);

 AddPullItem(Menu.PullInf^[16], 'Pwd echo     ', 1816, #00, 'Character to echo when password is entered', 2);
 AddPullItem(Menu.PullInf^[17], 'Auto ANSI/RIP', 1817, #00, 'Attempt to auto-detect remote user''s ANSI/RIP capability?', 2);
 AddPullItem(Menu.PullInf^[18], 'Pwd tries    ', 1818, #00, 'Number of password attempts before user is disconnected', 2);
 AddPullItem(Menu.PullInf^[19], 'Pwd change   ', 1819, #00, 'Number of calls before a password change is forced', 2);
 AddPullItem(Menu.PullInf^[20], 'Pwd strict   ', 1820, #00, 'Strict password checking?', 2);
 AddPullItem(Menu.PullInf^[21], 'Pwd length   ', 1821, #00, 'Minimum password length', 2);
 AddPullItem(Menu.PullInf^[22], 'Logon time   ', 1822, #00, 'Maximum time allowed to complete the new user procedure '+
                                                            '(minutes)', 2);
 AddPullItem(Menu.PullInf^[23], 'User timeout ', 1823, #00, 'Maximum inactivity period before user is disconnected '+
                                                            '(seconds)', 2);
 AddPullItem(Menu.PullInf^[24], 'Watchdog area', 1824, #00, 'Msg area to post password violation warnings to users in '+
                                                            '(0 to disable)', 2);
 AddPullItem(Menu.PullInf^[25], 'Sysop area   ', 1825, #00, 'Msg area for users to post to the sysop after a password '+
                                                            'failure', 2);
 AddPullItem(Menu.PullInf^[26], 'Check DOB    ', 1826, #00, 'Check users date of birth for security every number of calls', 2);
 AddPullItem(Menu.PullInf^[27], 'Limit local  ', 1827, #00, 'Yes=Disable all sysop keys during local logon', 2);
 AddPullItem(Menu.PullInf^[28], 'Blank logins ', 1828, #00, 'Number of times a user may hit (Enter) at the login prompt', 2);
 AddPullItem(Menu.PullInf^[29], 'Cap.Logon    ', 1829, #00, 'Capitalize the username and handle', 2);

 Menu.Items   := 29;
 Menu.Width   := 50;
 Menu.Length  := 15;
 Menu.X       := 15;
 Menu.Y       := 4;
 Menu.HiLight := 01;
 Menu.AddSpace:= True;
 Menu.Title   := ' System ';
 Menu.PosArray[1].XInc := 00;
 Menu.PosArray[1].YDec := 00;
 Menu.PosArray[2].XInc := 28;
 Menu.PosArray[2].YDec := 15;

 ShowMenu(Menu, False);

 Repeat;
  SaveDirect := DirectScrnUpdate;
  DirectscrnUpdate := false;
  WriteAT(31, 06, mnuNormColor, Bool2Str(GlobalCfg^.RaConfig^.FastLogon));
  WriteAT(31, 07, mnuNormColor, Bool2Str(GlobalCfg^.RaConfig^.CheckForMultiLogon));
  WriteAT(31, 08, mnuNormColor, Bool2Str(GlobalCfg^.RaConfig^.AllowSysRem));
  WriteAT(31, 09, mnuNormColor, Bool2Str(GlobalCfg^.RaConfig^.ExcludeSysOpFromList));
  WriteAT(31, 10, mnuNormColor, Bool2Str(GlobalCfg^.RaConfig^.AllowFileShells));
  Case GlobalCfg^.RaConfig^.LogStyle of
    00 : WriteAT(31, 11, mnuNormColor, 'Expanded');
    01 : WriteAT(31, 11, mnuNormColor, 'Compact ');
  End; { Case LogStyle }

  WriteAT(31, 12, mnuNormColor, Bool2Str(GlobalCfg^.RaConfig^.MultiLine));

  Case GlobalCfg^.RaConfig^.MultiTasker of
    00 : WriteAT(31, 13, mnuNormColor, 'Auto-detect');
    01 : WriteAT(31, 13, mnuNormColor, 'AT-BIOS    ');
    02 : WriteAT(31, 13, mnuNormColor, 'DoubleDOS  ');
    03 : WriteAT(31, 13, mnuNormColor, 'DESQview   ');
    04 : WriteAT(31, 13, mnuNormColor, 'TopView    ');
    05 : WriteAT(31, 13, mnuNormColor, 'MultiLink  ');
    06 : WriteAT(31, 13, mnuNormColor, 'PC-MOS/386 ');
    07 : WriteAT(31, 13, mnuNormColor, 'MS-Windows ');
    08 : WriteAT(31, 13, mnuNormColor, 'OS/2       ');
  End; { Case MultiTasker }

  WriteAT(31, 14, mnuNormColor, FStr(GlobalCfg^.RaConfig^.BlankSecs));
  WriteAT(31, 15, mnuNormColor, FStr(GlobalCfg^.RaConfig^.DoAfterAction));
  WriteAT(31, 16, mnuNormColor, Bool2Str(GlobalCfg^.RaConfig^.AltJSwap));
  WriteAT(31, 17, mnuNormColor, Bool2Str(GlobalCfg^.RaConfig^.UseXMS));
  WriteAT(31, 18, mnuNormColor, Bool2Str(GlobalCfg^.RaConfig^.UseEMS));
  WriteAT(31, 19, mnuNormColor, Bool2Str(GlobalCfg^.RaConfig^.SavePasswords));
  WriteAT(31, 20, mnuNormColor, Ask2Str(Byte(GlobalCfg^.RaConfig^.Emsi_Enable)));

  WriteAT(59, 06, mnuNormColor, GlobalCfg^.RaConfig^.EchoChar);
  WriteAT(59, 07, mnuNormColor, Bool2Str(GlobalCfg^.RaConfig^.AutoDetectANSI));
  WriteAT(59, 08, mnuNormColor, FStr(GlobalCfg^.RaConfig^.PasswordTries));
  WriteAT(59, 09, mnuNormColor, FStr(GlobalCfg^.RaConfig^.PwdExpiry));
  WriteAT(59, 10, mnuNormColor, Bool2Str(GlobalCfg^.RaConfig^.StrictPwdChecking));
  WriteAT(59, 11, mnuNormColor, FStr(GlobalCfg^.RaConfig^.MinPwdLen));
  WriteAT(59, 12, mnuNormColor, FStr(GlobalCfg^.RaConfig^.LogonTime));
  WriteAT(59, 13, mnuNormColor, FStr(GlobalCfg^.RaConfig^.UserTimeOut));
  WriteAT(59, 14, mnuNormColor, FStr(GlobalCfg^.RaConfig^.PwdBoard));
  WriteAT(59, 15, mnuNormColor, FStr(GlobalCfg^.RaConfig^.BadPwdArea));
  WriteAT(59, 16, mnuNormColor, FStr(GlobalCfg^.RaConfig^.CheckDOB));
  WriteAT(59, 17, mnuNormColor, Bool2Str(GlobalCfg^.RaConfig^.LimitLocal));
  WriteAT(59, 18, mnuNormColor, FStr(GlobalCfg^.RaConfig^.BlankLogins));
  WriteAT(59, 19, mnuNormColor, Bool2Str(GlobalCfg^.Elconfig^.CapitalizeUsername));
  DirectScrnUpdate := SaveDirect;

  Choice := DoPullMenu(Menu, CH, False, False);

  with GlobalCfg^ do
  Case Choice of
     1801 : Edit_Boolean(RaConfig^.FastLogon, 31, 06, mnuNormColor);
     1802 : Edit_Boolean(RaConfig^.CheckForMultiLogon, 31, 07, mnuNormColor);
     1803 : Edit_Boolean(RaConfig^.AllowSysRem, 31, 08, mnuNormColor);
     1804 : Edit_Boolean(RaConfig^.ExcludeSysopFromList, 31, 09, mnuNormColor);
     1805 : Edit_Boolean(RaConfig^.AllowFileShells, 31, 10, mnuNormColor);
     1806 : If RaConfig^.LogStyle=01 then RaConfig^.LogStyle:=00 else
              RaConfig^.LogStyle := 01;
     1807 : Edit_Boolean(RaConfig^.MultiLine, 31, 12, mnuNormColor);
     1808 : If RaConfig^.MultiTasker<08 then Inc(RaConfig^.MultiTasker)
             else RaConfig^.MultiTasker:= 00;
     1809 : EditByte(RaConfig^.Blanksecs, 31, 14, mnuEditColor, True);
     1810 : EditByte(RaConfig^.DoAfterAction, 31, 15, mnuEditColor, True);
     1811 : Edit_Boolean(RaConfig^.AltJSwap, 31, 16, mnuNormColor);
     1812 : Edit_Boolean(RaConfig^.UseXMS, 31, 17, mnuNormColor);
     1813 : Edit_Boolean(RaConfig^.UseEMS, 31, 18, mnuNormColor);
     1814 : Edit_Boolean(RaConfig^.SavePasswords, 31, 19, mnuNormColor);
     1815 : EditAskType(31, 20, Byte(RaConfig^.Emsi_Enable), True, True, False, True);
     1816 : RaConfig^.EchoChar := Edit(RaConfig^.EchoChar, 59, 06, mnuEditColor, 1, False, False, True);
     1817 : Edit_Boolean(RaConfig^.AutoDetectAnsi, 59, 07, mnuNormColor);
     1818 : begin
              TmpByte := RaConfig^.PassWordTries;
              EditByte(TmpByte, 59, 08, mnuEditColor, True);
              RaConfig^.PassWordTries := TmpByte;
            end;
     1819 : begin
              TmpByte := RaConfig^.PwdExpiry;
              EditByte(TmpByte, 59, 09, mnuEditColor, True);
              RaConfig^.PwdExpiry := TmpBYte;
            end;
     1820 : Edit_Boolean(RaConfig^.StrictPwdChecking, 59, 10, mnuNormColor);
     1821 : EditByte(RaConfig^.MinPwdLen, 59, 11, mnuEditColor, True);
     1822 : begin
              TmpByte := RaConfig^.LogonTime;
              EditByte(TmpByte, 59, 12, mnuEditColor, True);
              RaConfig^.LogonTime := TmpByte;
            end;
     1823 : EditWord(RaConfig^.UserTimeOut, 59, 13, mnuEditColor, True);
     1824 : EditByte(RaConfig^.PwdBoard, 59, 14, mnuEditColor, True);
     1825 : EditByte(RaConfig^.BadPwdArea, 59, 15, mnuEditColor, True);
     1826 : EditByte(RaConfig^.CheckDOB, 59, 16, mnuEditColor, True);
     1827 : Edit_Boolean(RaConfig^.LimitLocal, 59, 17, mnuNormColor);
     1828 : EditByte(RaConfig^.BlankLogins, 59, 18, mnuEditColor, True);
     1829 : Edit_Boolean(ElConfig^.CapitalizeUserName, 59, 19, mnuNormColor);
  End; { Case }

 Until Choice=0;

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. DoSystem }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure DoProtocols;

Function ProtocolSelect(var Hilight: Byte): Byte;
Var Menu   : PullRecord;
    Choice : Word;
    CH     : Char;
begin
 AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'ProtocolSelect');
 InitPullMenu(Menu);
 AddPullItem(Menu.PullInf^[01], 'Internal', 2701, #00, 'Edit internal protocol availability', 1);
 AddPullItem(Menu.PullInf^[02], 'External', 2702, #00, 'Edit external protocol settings', 1);

 Menu.Items   := 02;
 Menu.Width   := 13;
 Menu.Length  := 02;
 Menu.X       := 60;
 Menu.Y       := 16;
 Menu.HiLight := HiLight;
 Menu.AddSpace:= True;
 Menu.Title   := ' Protocols ';
 Menu.PosArray[1].XInc := 01;
 Menu.PosArray[1].YDec := 00;

 ShowMenu(Menu, False);

 ProtocolSelect := 00;

 Repeat;
  Choice := DoPullMenu(Menu, CH, False, False);

  Case Choice of
    2701 : ProtocolSelect := 01;
    2702 : ProtocolSelect := 02;
  End; { Case }

 if Choice=00 then ProtocolSelect := 00;

 If Choice>00 then HiLight := Choice - 2700;
 If (Choice=2701) or (Choice=2702) then break; { was:exit}
 Until Choice=00;

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
End; { func. ProtocolSelect }

Procedure EditInternal;
Var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    Teller    : Byte;
    SaveDirect: Boolean;
begin
 AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'EditInternal');
 InitPullMenu(Menu);
 AddPullItem(Menu.PullInf^[01], 'Xmodem     ', 2701, #00, 'Always available, not available, error-free connects only', 1);
 AddPullItem(Menu.PullInf^[02], 'Xmodem/1K  ', 2702, #00, 'Always available, not available, error-free connects only', 1);
 AddPullItem(Menu.PullInf^[03], 'Xmodem/1K-g', 2703, #00, 'Always available, not available, error-free connects only', 1);
 AddPullItem(Menu.PullInf^[04], 'Ymodem     ', 2704, #00, 'Always available, not available, error-free connects only', 1);
 AddPullItem(Menu.PullInf^[05], 'Ymodem-g   ', 2705, #00, 'Always available, not available, error-free connects only', 1);
 AddPullItem(Menu.PullInf^[06], 'Zmodem     ', 2706, #00, 'Always available, not available, error-free connects only', 1);

 Menu.Items   := 06;
 Menu.Width   := 28;
 Menu.Length  := 06;
 Menu.X       := 20;
 Menu.Y       := 10;
 Menu.HiLight := 01;
 Menu.AddSpace:= True;
 Menu.Title   := ' Internal protocols ';
 Menu.PosArray[1].XInc := 00;
 Menu.PosArray[1].YDec := 00;

 ShowMenu(Menu, False);

 Repeat;
  SaveDirect := DirectScrnUpdate;
  DirectscrnUpdate := false;
  For Teller := 01 to 06 do
    WriteAT(34, 11 + Teller, mnuNormColor, ProtAvail2Str(GlobalCfg^.RaConfig^.ProtocolAttrib[Teller], False));
  DirectScrnUpdate := SaveDirect;

  Choice := DoPullMenu(Menu, CH, False, False);

  If Choice>00 then
   with GlobalCfg^ do
   begin
      Inc(RaConfig^.ProtocolAttrib[Choice - 2700]);

      If RaConfig^.ProtocolAttrib[Choice-2700]>2 then
            RaConfig^.ProtocolAttrib[Choice-2700] := 00;
   end; { Choice }

 Until Choice=00;

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. EditInternal }


procedure EditExternal;
begin
  if NOT FileExist(ProtocolFileName) then
     if NOT CreateNewProtocolRA then
      begin
        CantCreate(JustName(ProtocolFileName), False);
        EXIT;
      end; { if }

  if NOT OpenFile(ProtocolFileName, ReadWriteMode + DenyWrite) then
      begin
        CantCreate(JustName(ProtocolFileName), True);
        EXIT;
      end; { if }

  New(Protocol_F, Init);
  Protocol_F^.Assign(ProtocolFileName);
  Protocol_F^.FileMode := ReadWriteMode + DenyWrite;
  Protocol_F^.Open(1);

  DoList(True, 8, 42, 03, 00, 00, 00, 00, 00, 00, 00,
         7, 04, 54, 20, ' Select a protocol ',
         'Edit external protocol settings',
         {$IFDEF FPC}@{$ENDIF}protocol_GetInfo,
         {$IFDEF FPC}@{$ENDIF}protocol_Activate,
         {$IFDEF FPC}@{$ENDIF}protocol_Seek,
         {$IFDEF FPC}@{$ENDIF}General_AbortCheck,
         {$IFDEF FPC}@{$ENDIF}Copy_Keypress,
         {$IFDEF FPC}@{$ENDIF}protocol_GetItems,
         {$IFDEF FPC}@{$ENDIF}CreateNewProtocolRA,
         {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
         False,
         00, 00,
         true, false);

  Dispose(Protocol_F, Done);
end; { proc. EditExternal }


var HiLight: Byte;
    SaveScrn: Pointer;
begin
  HiLight:=01;

  Repeat;
   Case ProtocolSelect(HiLight) of
    { Exit }     00 : Exit;
    { Internal } 01 : EditInternal;
    { External } 02 : begin
                        SaveScreen(SaveScrn);
                         EditExternal;
                        RestoreScreen(SaveScrn);
                      end; { EditExternal }
   End; { Case }
  Until (True=False);

end; { proc. DoProtocols }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function DoExit: Boolean;
var Changes  : Boolean;
    Config_F : pFileObj;
begin
 if IOResult>00 then ;

 Changes := False;
 DoExit := True;

 GlobalCfg^.RaConfig^.ProductID := EleBBS_ProdID;
 GlobalCfg^.RaConfig^.VersionID := VersIdWord;
 GlobalCfg^.ElConfig^.VersionID := EleVersIdWord;

 If RecordsDifferent(GlobalCfg^.RaConfig^, OldConfig^, SizeOf(ConfigRecord)) then
      Changes := True;
 If RecordsDifferent(GlobalCfg^.ElConfig^, OldEleConfig^, SizeOf(EleConfigRecord)) then
      Changes := True;
 If RecordsDifferent(LineCfg^.Modem^, OldModem^, SizeOf(ModemRecord)) then
      Changes := True;
 If RecordsDifferent(LineCfg^.Telnet^, OldTelnet^, SizeOf(TelnetRecord)) then
      Changes := True;
 If RecordsDifferent(LineCfg^.NewsServer^, OldNewsServer^, SizeOf(NewsServerRecord)) then
      Changes := True;

 If Changes then
  If DoSaveChanges('Save changes (Y/n) ? °', true, true) then
     begin
        New(Config_F, Init);
        Config_F^.Assign(ConfigRaName);
        Config_F^.FileMode := ReadWriteMode + DenyAll;
        Config_F^.Open(1);
        Config_F^.BlkWrite(GlobalCfg^.RaConfig^, SizeOf(ConfigRecord));
        if Config_F^.IoResult > 0 then
          CantCreate(ConfigRaName, TRUE);
        Dispose(Config_F, Done);

        New(Config_F, Init);
        Config_F^.Assign(ConfigEleName);
        Config_F^.FileMode := ReadWriteMode + DenyAll;
        Config_F^.Open(1);
        Config_F^.BlkWrite(GlobalCfg^.ElConfig^, SizeOf(EleConfigRecord));
        if Config_F^.IoResult > 0 then
          CantCreate(ConfigEleName, TRUE);
        Dispose(Config_F, Done);

        New(Config_F, Init);
        Config_F^.Assign(ModemFileName);
        Config_F^.FileMode := ReadWriteMode + DenyAll;
        Config_F^.Open(1);
        Config_F^.BlkWrite(LineCfg^.Modem^, SizeOf(ModemRecord));
        if Config_F^.IoResult > 0 then
          CantCreate(ModemFileName, TRUE);
        Dispose(Config_F, Done);

        New(Config_F, Init);
        Config_F^.Assign(TelnetFileName);
        Config_F^.FileMode := ReadWriteMode + DenyAll;
        Config_F^.Open(1);
        Config_F^.BlkWrite(LineCfg^.Telnet^, SizeOf(TelnetRecord));
        if Config_F^.IoResult > 0 then
          CantCreate(TelnetFileName, TRUE);
        Dispose(Config_F, Done);

        New(Config_F, Init);
        Config_F^.Assign(NewsServerFileName);
        Config_F^.FileMode := ReadWriteMode + DenyAll;
        Config_F^.Open(1);
        Config_F^.BlkWrite(LineCfg^.NewsServer^, SizeOf(NewsServerRecord));
        if Config_F^.IoResult > 0 then
          CantCreate(NewsServerFileName, TRUE);
        Dispose(Config_F, Done);
     end; { if }

 if Changes then
  if rdLastKey=#27 then
    begin
      DoExit := False;
      EXIT;
    end; { if }

 ScreenClear(White, #32);
 { Was: Halt(0); }
end; { proc. DoExit }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoEditLanguage;
var SaveDirect: Boolean;
begin
  If NOT FileExist(LanguageFileName) then
   if NOT CreateNewLanguageRa then
      begin
        CantCreate(JustName(LanguagefileName), False);
        EXIT;
      end; { if }

  if NOT OpenFile(LanguageFileName, ReadWriteMode + DenyWrite) then
      begin
        CantCreate(JustName(LanguageFileName), True);
        EXIT;
      end; { if }

  ScreenClear(White, #32);

  New(Language_F, Init);
  Language_F^.Assign(LanguageFileName);
  Language_F^.FileMode := ReadMode + DenyNone;
  Language_F^.Open(1);

  SaveDirect := DirectscrnUpdate;
  DirectScrnUpdate := false;

  PartClear(01, 02, 80, 02, mnuTopBar, mnuDblWide);
  PartClear(01, mnuScrnLen - 01, 80, mnuScrnLen - 01, mnuTopBar, mnuSngWide);

  Box(02, 04, 79, 14, mnuDisabled, 04, True);                 { Language Info }
  Box(02, 16, 79, mnuScrnLen - 03, mnuBoxColor, 04, True);            { Language selection }
  DirectScrnUpdate := SaveDirect;

  DoList(True, 03, 20, 00, 00, 00, 00, 00, 00, 00, 00,   { Length (info) coordinates }
         02, 16, 79, mnuScrnLen - 03,
         '',
         'Select a language to edit or a blank slot to start a new one, then press RETURN',
         {$IFDEF FPC}@{$ENDIF}Language_GetInfo,
         {$IFDEF FPC}@{$ENDIF}language_Activate,
         {$IFDEF FPC}@{$ENDIF}language_Seek,
         {$IFDEF FPC}@{$ENDIF}General_AbortCheck,
         {$IFDEF FPC}@{$ENDIF}Copy_Keypress,
         {$IFDEF FPC}@{$ENDIF}Language_GetItems,
         {$IFDEF FPC}@{$ENDIF}CreateNewLanguageRA,
         {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
         False,
         00, 00,
         False, false);

  Dispose(Language_F, Done);
  ScreenClear(White, #32);
end; { proc. DoEditLanguage }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure DoEditAltKeys;
var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    SaveDirect: Boolean;
begin
 AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'DoEditAltKeys');
 InitPullMenu(Menu);
 AddPullItem(Menu.PullInf^[01], '1 ',  1, #00, 'Enter a DOS command-line, #TextFile, ?ErrorLevel or $Questionnaire', 1);
 AddPullItem(Menu.PullInf^[02], '2 ',  2, #00, 'Enter a DOS command-line, #TextFile, ?ErrorLevel or $Questionnaire', 1);
 AddPullItem(Menu.PullInf^[03], '3 ',  3, #00, 'Enter a DOS command-line, #TextFile, ?ErrorLevel or $Questionnaire', 1);
 AddPullItem(Menu.PullInf^[04], '4 ',  4, #00, 'Enter a DOS command-line, #TextFile, ?ErrorLevel or $Questionnaire', 1);
 AddPullItem(Menu.PullInf^[05], '5 ',  5, #00, 'Enter a DOS command-line, #TextFile, ?ErrorLevel or $Questionnaire', 1);
 AddPullItem(Menu.PullInf^[06], '6 ',  6, #00, 'Enter a DOS command-line, #TextFile, ?ErrorLevel or $Questionnaire', 1);
 AddPullItem(Menu.PullInf^[07], '7 ',  7, #00, 'Enter a DOS command-line, #TextFile, ?ErrorLevel or $Questionnaire', 1);
 AddPullItem(Menu.PullInf^[08], '8 ',  8, #00, 'Enter a DOS command-line, #TextFile, ?ErrorLevel or $Questionnaire', 1);
 AddPullItem(Menu.PullInf^[09], '9 ',  9, #00, 'Enter a DOS command-line, #TextFile, ?ErrorLevel or $Questionnaire', 1);
 AddPullItem(Menu.PullInf^[10], '10', 10, #00, 'Enter a DOS command-line, #TextFile, ?ErrorLevel or $Questionnaire', 1);

 Menu.Items   := 10;
 Menu.Width   := 67;
 Menu.Length  := 10;
 Menu.X       := 7;
 Menu.Y       := 7;
 Menu.HiLight := 01;
 Menu.AddSpace:= True;
 Menu.Title   := ' ALT function key manager ';
 Menu.PosArray[1].XInc := 00;
 Menu.PosArray[1].YDec := 00;

 ShowMenu(Menu, False);

 with GlobalCfg^ do
 Repeat;
  SaveDirect := DirectScrnUpdate;
  DirectscrnUpdate := false;
  WriteAT(12, 09, mnuNormColor, RaConfig^.FKeys[1]);
  WriteAT(12, 10, mnuNormColor, RaConfig^.FKeys[2]);
  WriteAT(12, 11, mnuNormColor, RaConfig^.FKeys[3]);
  WriteAT(12, 12, mnuNormColor, RaConfig^.FKeys[4]);
  WriteAT(12, 13, mnuNormColor, RaConfig^.FKeys[5]);
  WriteAT(12, 14, mnuNormColor, RaConfig^.FKeys[6]);
  WriteAT(12, 15, mnuNormColor, RaConfig^.FKeys[7]);
  WriteAT(12, 16, mnuNormColor, RaConfig^.FKeys[8]);
  WriteAT(12, 17, mnuNormColor, RaConfig^.FKeys[9]);
  WriteAT(12, 18, mnuNormColor, RaConfig^.FKeys[10]);
  DirectScrnUpdate := SaveDirect;

  Choice := DoPullMenu(Menu, CH, False, False);

  If Choice in [1..10] then
    RaConfig^.FKeys[Choice] := Edit(RaConfig^.FKeys[Choice], 12, 08 + Choice, mnuEditColor, 60, False, False, True);

 Until Choice=0;

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. DoEditAltKeys }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure DoEditCtlFiles;
var Menu  : PullRecord;
    Choice: Word;
    CH    : Char;
begin
 AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'DoEditCtlFiles');
 InitPullMenu(Menu);
 AddPullItem(Menu.PullInf^[01], 'VIP',       1, #00, 'Notify the sysop when any of these users call in', 1);
 AddPullItem(Menu.PullInf^[02], 'TRASHCAN',  2, #00, 'Disallow certain user names', 1);
 AddPullItem(Menu.PullInf^[03], 'NODECOST',  3, #00, 'Nodelist and netmail costing information', 1);
 AddPullItem(Menu.PullInf^[04], 'BADFILES',  4, #00, 'Specify a list of unwanted files', 1);
 AddPullItem(Menu.PullInf^[05], 'HANDLES',   5, #00, 'Disallow certain handle names', 1);
 AddPullItem(Menu.PullInf^[06], 'PHONENUM',  6, #00, 'Disallow certain telephone numbers or telephone number segments', 1);
 AddPullItem(Menu.PullInf^[07], 'PWDTRASH',  7, #00, 'Disallow undesirable passwords', 1);
 AddPullItem(Menu.PullInf^[08], 'NAMES',     8, #00, 'Specify a list of shortname macros for frequently-addressed users', 1);
 AddPullItem(Menu.PullInf^[09], 'MAILFWD',   9, #00, 'Create a alias file for EleMAIL', 1);
 AddPullItem(Menu.PullInf^[10], 'Other',    10, #00, 'Edit an ASCII text file (you supply the file name)', 1);

 Menu.Items   := 10;
 Menu.Width   := 12;
 Menu.Length  := 10;
 Menu.X       := 2;
 Menu.Y       := 4;
 Menu.HiLight := 01;
 Menu.AddSpace:= True;
 Menu.Title   := ' Ctl files';
 Menu.PosArray[1].XInc := 00;
 Menu.PosArray[1].YDec := 00;

 ShowMenu(Menu, False);

 with GlobalCfg^ do
 Repeat;
  Choice := DoPullMenu(Menu, CH, False, False);

  Case Choice of
    01 : Text_Editor(RaConfig^.SysPath + 'vip.ctl', Menu, true);
    02 : Text_Editor(RaConfig^.SysPath + 'trashcan.ctl', Menu, true);
    03 : Text_Editor(RaConfig^.SysPath + 'nodecost.ctl', Menu, true);
    04 : Text_Editor(RaConfig^.SysPath + 'badfiles.ctl', Menu, true);
    05 : Text_Editor(RaConfig^.SysPath + 'handles.ctl', Menu, true);
    06 : Text_Editor(RaConfig^.SysPath + 'phonenum.ctl', Menu, true);
    07 : Text_Editor(RaConfig^.SysPath + 'pwdtrash.ctl', Menu, true);
    08 : Text_Editor(RaConfig^.SysPath + 'names.ctl', Menu, true);
    09 : Text_Editor(RaConfig^.SysPath + 'mailfwd.ctl', Menu, true);
    10 : Text_Editor('', Menu, true);
  end; { case }

 Until Choice=0;

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. DoEditAltKeys }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Byte2Week(B: Byte): String;
var Counter: Byte;
    TempStr: String;
begin
  TempStr := '';
  TempStr[00] := #07;

  For Counter := 00 to 07 do
   If ReadBit(B, Counter) then
    TempStr[Counter+1] := 'X' else
     TempStr[Counter+1] := '-';

  Byte2Week := TempStr;
end; { func. Byte2Week }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  Byte2Status(B: Byte): String;
begin
  Byte2Status := '';

  Case B of
    00 : Byte2Status := 'Deleted ';
    01 : Byte2Status := 'Enabled ';
    02 : Byte2Status := 'Disabled';
  end; { case }
end; { func. Byte2Status }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoEditEvents;

Function MakeLen(B: Byte): String;
begin
  If B<10 then MakeLen := '  ' + FStr(B)
   else MakeLen := ' ' + FStr(B);
end; { func. MakeLen }


Procedure EditEvents(var Event: EventRecord; Nr: Byte; DisMenu: PullRecord);
Var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    TempStr   : String;
    SaveDirect: Boolean;
begin
 AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'EditEvents');
 InitPullMenu(Menu);
 AddPullItem(Menu.PullInf^[01], 'Start     ', 01, #00, 'Start time of the event', 1);
 AddPullItem(Menu.PullInf^[02], 'Status    ', 02, #00, 'Enabled, disabled or deleted', 1);
 AddPullItem(Menu.PullInf^[03], 'Errorlevel', 03, #00, 'Errorlevel to exit at when event is executed', 1);
 AddPullItem(Menu.PullInf^[04], 'Forced    ', 04, #00, 'Yes means that filetransfers will be interrupted '+
                                                       'to ensure the event runs', 1);
 AddPullItem(Menu.PullInf^[05], 'Days      ', 05, #00, 'Days that this event will run on', 1);

 Menu.Items   := 5;
 Menu.Width   := 23;
 Menu.Length  := 5;
 Menu.X       := 57;
 Menu.Y       := 16;
 Menu.HiLight := 01;
 Menu.AddSpace:= True;
 Menu.Title   := ' Event #'+FStr(Nr) + ' ';
 Menu.PosArray[1].XInc := 00;
 Menu.PosArray[1].YDec := 00;

 EnDisAbleMenu(DisMenu, False);
 ShowMenu(Menu, False);


 Repeat;
  If Event.Status >2 then Event.Status := 00;
  SaveDirect := DirectScrnUpdate;
  DirectscrnUpdate := false;

  WriteAT(71, 18, mnuNormColor, Event.StartTime);
  WriteAT(71, 19, mnuNormColor, Byte2Status(Event.Status));
  WriteAT(71, 20, mnuNormColor, FStr(Event.ErrorLevel));
  WriteAT(71, 21, mnuNormColor, Bool2Str(Event.Forced));
  WriteAT(71, 22, mnuNormColor, Byte2Week(Event.Days));
  DirectScrnUpdate := SaveDirect;

  Choice := DoPullMenu(Menu, CH, False, False);

  Case Choice of
      01 : EditTime(Event.StartTime, 71, 18, True);
      02 : Inc(Event.Status);
      03 : EditByte(Event.ErrorLevel, 71, 20, mnuEditColor, True);
      04 : Edit_Boolean(Event.Forced, 71, 21, mnuEditColor);
      05 : EditWeeks(Event.Days);
  End; { Case }

 Until Choice=0;
 EnDisAbleMenu(DisMenu, True);

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. EditEvents }



var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    Event_F   : pFileObj;
    Counter   : Word;
    SaveDirect: Boolean;
begin
 AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'DoEditEvents');
 InitPullMenu(Menu);

 OldEvents^ := Events^;

 For Choice := 01 to 20 do
  AddPullItem(Menu.PullInf^[Choice], MakeLen(Choice),  Choice, #00, '', 01);

 Menu.Items   := 20;
 Menu.Width   := 79;
 Menu.Length  := 22;
 Menu.X       := 1;
 Menu.Y       := 1;
 Menu.HiLight := 01;
 Menu.AddSpace:= False;
 Menu.Title   := ' Events ';
 Menu.PosArray[1].XInc := 01;
 Menu.PosArray[1].YDec := -02;

 ShowMenu(Menu, False);

 Repeat;
  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;
  WriteAT(02, 02, mnuMsgColor, ' Event #    Time   Status   Errorlevel   Forced   Days:SMTWTFS');
  WriteAT(02, 03, mnuMsgColor, Dup(mnuSngWide, 78));

  For Choice := 01 to 20 do
   begin
    If Events^[Choice].Status = 00 then
      LoadOneEventDefault(Events^[Choice]);

     With Events^[Choice] do
      begin
        WriteAT(14, 03 + Choice, mnuNormColor, StartTime);
        WriteAT(21, 03 + Choice, mnuNormColor, Byte2Status(Status));
        WriteAT(35, 03 + Choice, mnuNormColor, FStr(ErrorLevel));
        WriteAT(45, 03 + Choice, mnuNormColor, Bool2Str(Forced));
        WriteAT(57, 03 + Choice, mnuNormColor, Byte2Week(Days));
      end; { with }
   end; { for Choice }
  DirectScrnUpdate := SaveDirect;
  Choice := DoPullMenu(Menu, CH, False, False);

  If Choice in [1..20] then
    EditEvents(Events^[Choice], Choice, Menu);
 Until Choice=0;


 If RecordsDifferent(Events^, OldEvents^, SizeOf(EventRecordArray)) then
  If DoSaveChanges('Save changes (Y/n) ? °', true, false) then
     begin
       New(Event_F, Init);
       Event_F^.Assign(EventsFileName);
       Event_F^.Create(1);

       for Counter := 1 to 20 do
         Event_F^.BlkWrite(Events^[Counter], SizeOf(EventRecord));

       Dispose(Event_F, Done);
     end; { if }

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. DoEditEvents }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure PickMenuLanguage;
begin
  If NOT FileExist(LanguageFileName) then
   if NOT CreateNewLanguageRa then
      begin
        CantCreate(JustName(LanguageFileName), False);
        EXIT;
      end; { if }

  if NOT OpenFile(LanguageFileName, ReadMode + DenyNone) then
      begin
        CantCreate(JustName(LanguageFileName), True);
        EXIT;
      end; { if }

  ScreenClear(White, #32);

  New(Language_F, Init);
  Language_F^.Assign(LanguageFileName);
  Language_F^.FileMode := ReadWriteMode + DenyWrite;
  Language_F^.Open(1);

  DoList(True, 31, 19, 00, 00, 00, 00, 00, 00, 00, 00,   { Length (info) coordinates }
         30, 05, 50, mnuScrnLen - 05,
         '',
         '',
         {$IFDEF FPC}@{$ENDIF}Language_NoEditGetInfo,
         {$IFDEF FPC}@{$ENDIF}Language_MenuActivate,
         {$IFDEF FPC}@{$ENDIF}language_Seek,
         {$IFDEF FPC}@{$ENDIF}Copy_AbortCheck,
         {$IFDEF FPC}@{$ENDIF}Copy_Keypress,
         {$IFDEF FPC}@{$ENDIF}Language_GetItems,
         {$IFDEF FPC}@{$ENDIF}CreateNewLanguageRA,
         {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
         False,
         00, 00,
         False, false);

  Dispose(Language_F, Done);
end; { proc. PickMenuLanguage }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function _MenuSortComp(V1, V2: Pointer): Boolean;
begin
  _MenuSortComp := String(V1^) > String(V2^);
end; { func. _MenuSortComp }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SortMenuList(var MenuInfo: MenuListRec; Items: Word);
begin
  QuickSort(MenuInfo,
            Items,
            SizeOf(MenuInfo[1]),
            {$IFDEF FPC}@{$ENDIF}_MenuSortComp);
end; { proc. SortMenuList }

(*-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure Menu_GetInfo(var Info1, Info2, Info3, Info4, Info5, Info6, Info7, Info8, Info9: List_StringType;
                           ItemNr: LongInt; HiLight: LongInt);
begin
  Info1 := MenuList^[ItemNr];
  Info2 := '';
  Info3 := '';
  Info4 := '';
  Info5 := '';
  Info6 := '';
  Info7 := '';
  Info8 := '';
  Info9 := '';
end; { proc. Menu_GetInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Menu_Seek(Str: String): LongInt;
var Counter: Word;
begin
  Menu_Seek := -1;

  For Counter := 01 to 2000 do
    If MenuList^[Counter] <> '' then
      If SUpcase(Str) = SupCase(Copy(MenuList^[Counter], 1, Length(Str))) then
          begin
            Menu_Seek := Counter;
            Break;
          end; { Found one }

end; { func. Menu_Seek }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Menu_GetItems: LongInt;
var Counter: Word;
begin
  For Counter := 01 to 2000 do
    If MenuList^[Counter] = '' then break;

  If MenuList^[01]='' then Counter := 01;

  Menu_GetItems := Counter - 01;
end; { func. Menu_GetItems }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure LoadMenuList(var MnuCounter: Word);
var DirInfo: SearchRec;
begin
  ReleaseMem(MenuList, SizeOf(MenuListRec));
  if NOT AllocMem(MenuList, SizeOf(MenuListRec), 'MenuListRec', 'LoadMenuList') then EXIT;

  FillChar(MenuList^, SizeOf(MenuListRec), #00);

  MnuCounter := 00;

  {$IFNDEF DELPHI} { FIXME !!! }
  FindFirst(ForceBack(MenuPath) + '*.*', AnyFile - VolumeID, DirInfo);

  While  Dos.DosError=00 do
    begin
      If DirInfo.Name <> '' then
       if SupCase(JustExtension(Dirinfo.Name)) = 'MNU' then
        begin
          Inc(MnuCounter);
          MenuList^[MnuCounter] := SlowCase(NoExtension(DirInfo.Name));
        end; { if }

     FindNext(DirInfo);
    end; { While }

  FindClose(Dirinfo);
  {$ENDIF}

  SortMenuList(MenuList^, mnuCounter);
end; { proc. LoadMenuList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Menu_Activate(HiLight: LongInt; HiBarPos: LongInt): Longint;
var MnuCounter: Word;
begin
  Menu_Activate := -1;
  EditMenu(ForceBack(MenuPath), MenuList^[HiLight]);

  ReleaseMem(MenuList, SizeOf(MenuListRec));
  LoadMenuList(mnuCounter);
end; { proc. Menu_Activate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Menu_CreateNewMenu: Boolean;
var Empty     : String;
    MnuCounter: Word;
    DirInfo   : SearchRec;
    SaveScrn  : Pointer;
begin
  Menu_CreateNewMenu := True;
  Empty := '';

  SaveScreen(SaveScrn);
  EditMenu(ForceBack(MenuPath), Empty);
  RestoreScreen(SaveScrn);

  FillChar(MenuList^, SizeOf(MenuListRec), #00);

  MnuCounter := 00;

  {$IFNDEF DELPHI} { FIXME !!!!! }
  FindFirst(ForceBack(MenuPath) + '*.*', AnyFile - VolumeID, DirInfo);
  While Dos.DosError=00 do
    begin
      if SUpCase(JustExtension(DirInfo.Name)) = 'MNU' then
        begin
          Inc(MnuCounter);
          MenuList^[MnuCounter] := NoExtension(DirInfo.Name);
        end; { if }

      FindNext(Dirinfo);
    end; { While }
  {$ENDIF}

  SortMenuList(MenuList^, mnuCounter);
end; { proc. Menu_CreateNewMenu }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Menu_KeyPress(CH: Char; HiLight: LongInt;
                       var HiBarPos, TopOfScrn: Longint;
                       var StartRange, EndRange: LongInt): LongInt;
var mnuCounter: Word;
    MenuName  : String;
    SaveScrn  : Pointer;
begin
  Menu_Keypress := -1;

  Case CH of
    #83 : if DoSaveChanges('Confirm delete (y/N)? °', false, false) then
            begin
              EraseFile(ForceBack(Menupath) + MenuList^[HiLight] + '.mnu');
              EraseFile(ForceBack(Menupath) + MenuList^[HiLight] + '.mlb');
            end; { if }
    #82 : begin
            SaveScreen(SaveScrn);

             ShadFillBoxTitle(06, 08, 70, 11, mnuBoxColor, mnuStyle, True, ' Save menu ');
             WriteAT(08, 09, mnuNormColor, 'Name of menu to save:');
             WriteAT(08, 10, mnuMsgColor, 'Press (Esc) to abort.');

             MenuName := '';
             GetString(30, 09, mnuEditColor, MenuName, [#32..#254], [#13, #27], 8, 8, True, False, True, false,0);

             if MenuName <> '' then
               EditMenu(ForceBack(MenuPath), MenuName);

            RestoreScreen(SaveScrn);
          end; { Insert }
  end; { case }

  LoadMenuList(MnuCounter);
end; { func. Menu_Keypress }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ListMenus: String;
var DirInfo   : SearchRec;
    MnuCounter: Word;
begin
  MenuList := nil;
  LoadMenuList(MnuCounter);
  ListMenus := '';

  DoList(True, 31, 19, 00, 00, 00, 00, 00, 00, 00, 00,   { Length (info) coordinates }
         30, 05, 50, mnuScrnLen - 5,
         '',
         'Pick a menu to edit, [DEL] to Delete  [INS] Insert menu',
         {$IFDEF FPC}@{$ENDIF}Menu_GetInfo,
         {$IFDEF FPC}@{$ENDIF}Menu_Activate,
         {$IFDEF FPC}@{$ENDIF}Menu_Seek,
         {$IFDEF FPC}@{$ENDIF}General_AbortCheck,
         {$IFDEF FPC}@{$ENDIF}Menu_Keypress,
         {$IFDEF FPC}@{$ENDIF}Menu_GetItems,
         {$IFDEF FPC}@{$ENDIF}Menu_CreateNewMenu,
         {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
         False,
         00, 00,
         False, false);

  ReleaseMem(MenuList, SizeOf(MenuListRec));
  MenuList := nil;
end; { func. ListMenus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoEditMenus(Params: String);
var MenuPicked  : String;
    SaveScrn    : Pointer;
    Temp        : String;
begin
  ScreenClear(White, #32);
  MenuPath := GlobalCfg^.RaConfig^.MenuPath;

  if NOT FileExist(ForceBack(Menupath)) then MenuPath := '';

  Replace('-M', '', Params);                            { Remove params }
  Replace('/M', '', Params);

  if WordCount(Params, defExtractWord, false) = 2 then
    begin
      AllocMem(LineCfg^.Language, SizeOf(LanguageRecord), 'LanguageRecord', 'DoEditMenus');
        ReadLanguageRa(Max(FVal(FirstWord(Params, defExtractWord, false)) - 01, 0));
        MenuPath := ForceBack(LineCfg^.Language^.MenuPath);
      ReleaseMem(LineCfg^.Language, SizeOf(LanguageRecord));
      if MenuPath='' then EXIT;

      Temp := FirstWord(Params, defExtractWord, false);

      EditMenu(ForceBack(MenuPath), Temp);
      EXIT;
    end; { if }

  repeat;
      PickMenuLanguage;

      If rdLastkey=#13 then
        begin
          MenuPath := CurLangPicked.Menupath;
          if NOT FileExist(Menupath) then Menupath := GlobalCfg^.RaConfig^.MenuPath;

          if MenuPath='' then
            begin
              SaveScreen(SaveScrn);

              ShadFillBox(10, 10, 70, 14, mnuErrorColor, mnuStyle, True);
              WriteAT(15, 12, mnuErrorColor, 'No menu path for this language - using default path');
              UpdateScreenBuffer(true);
              Delay(2000);

              MenuPath := GlobalCfg^.RaConfig^.MenuPath;

              RestoreScreen(SaveScrn);
            end; { if }

          MenuPicked := ListMenus;
          rdLastkey := #13;
        end; { enter pressed }

  Until (rdLastkey=#27);

  ScreenClear(White, #32);
end; { proc. DoEditMenus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ReadConfigFiles;
var Config_F    : pFileObj;
    Modem_F     : pFileObj;
    Events_F    : pFileObj;
    Telnet_F    : pFileObj;
    NewsServer_F: pFileObj;
    NumRead     : Word;
begin
 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, 'Fillcharing OldConfig^');
 {$ENDIF}
  FillChar(OldConfig^, SizeOf(OldConfig^), #00);
  FillChar(OldEleConfig^, SizeOf(OldEleConfig^), #00);
 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, 'Fillcharing OldEvents^');
 {$ENDIF}
  FillChar(OldEvents^, SizeOf(OldEvents^), #00);
 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, 'Fillcharing OldModem^');
 {$ENDIF}
  FillChar(OldModem^, SizeOf(OldModem^), #00);
 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, 'Fillcharing OldTelnet^');
 {$ENDIF}
  FillChar(OldTelnet^, SizeOf(OldTelnet^), #00);
 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, 'Fillcharing OldNewsServer^');
 {$ENDIF}
  FillChar(OldNewsServer^, SizeOf(OldNewsServer^), #00);


  GlobalCfg^.RaConfig^.SysPath := ForceBack(GetSysEnv);
  ConfigRaName := OpenRaFile('config.ra');
  ConfigEleName := JustPath(ConfigRaName) + 'config.ele';

  New(Config_F, Init);
  Config_F^.Assign(ConfigRaName);
  Config_F^.FileMode := ReadWriteMode + DenyNone;

  if NOT Config_F^.Open(1) then
    begin
      Config_F^.Create(1);
      Config_F^.BlkWrite(GlobalCfg^.RaConfig^, SizeOf(ConfigRecord));

      if Config_F^.IoResult > 0 then
        CantCreate(ConfigRaName, FALSE);

      Config_F^.Seek(0);
    end; { if }

  Config_F^.BlkRead(GlobalCfg^.RaConfig^, SizeOf(ConfigRecord));

  Dispose(Config_F, Done);

  New(Config_F, Init);
  Config_F^.Assign(ConfigEleName);
  Config_F^.FileMode := ReadWriteMode + DenyNone;
  if NOT Config_F^.Open(1) then
    begin
      Config_F^.Create(1);
      Config_F^.BlkWrite(GlobalCfg^.ElConfig^, SizeOf(EleConfigRecord));

      if Config_F^.IoResult > 0 then
        CantCreate(ConfigEleName, FALSE);

      Config_F^.Seek(0);
    end; { if }

  Config_F^.BlkRead(GlobalCfg^.ElConfig^, SizeOf(EleConfigRecord));

  {-- if this is a lower version, correct it -------------------------------}
  if GlobalCfg^.ElConfig^.VersionID < EleVersIDword then
    begin
      GlobalCfg^.ElConfig^.webHtmlPath := BsChar + 'ele' + BsChar + 'web' + BsChar + 'html' + BsChar;
      GlobalCfg^.ElConfig^.webElmPath := BsChar + 'ele' + BsChar + 'web' + BsChar + 'script' + BsChar;
    end; { if }

  Dispose(Config_F, Done);

  InitSystemNames;

  New(Modem_F, Init);
  Modem_F^.Assign(ModemFileName);
  Modem_F^.FileMode := ReadWriteMode + DenyNone;
  if NOT Modem_F^.Open(1) then
    begin
      Config_F^.Create(1);
      Config_F^.BlkWrite(LineCfg^.Modem^, SizeOf(ModemRecord));

      if Config_F^.IoResult > 0 then
        CantCreate(ModemFileName, FALSE);

      Config_F^.Seek(0);
    end; { if }

  Modem_F^.BlkRead(LineCfg^.Modem^, SizeOf(ModemRecord));
  Dispose(Modem_F, Done);


  New(Telnet_F, Init);
  Telnet_F^.Assign(TelnetFileName);
  Telnet_F^.FileMode := ReadWriteMode + DenyNone;
  if NOT Telnet_F^.Open(1) then
    begin
      Telnet_F^.Create(1);
      Telnet_F^.BlkWrite(LineCfg^.Telnet^, SizeOf(TelnetRecord));

      if Telnet_F^.IoResult > 0 then
        CantCreate(TelnetFileName, FALSE);

      Telnet_F^.Seek(0);
    end; { if }

  Telnet_F^.BlkRead(LineCfg^.Telnet^, SizeOf(TelnetRecord));
  Dispose(Telnet_F, Done);


  New(NewsServer_F, Init);
  NewsServer_F^.Assign(NewsServerFileName);
  NewsServer_F^.FileMode := ReadWriteMode + DenyNone;
  if NOT NewsServer_F^.Open(1) then
   begin
     NewsServer_F^.Create(1);
     NewsServer_F^.BlkWrite(LineCfg^.NewsServer^, SizeOf(NewsServerRecord));

      if NewsServer_F^.IoResult > 0 then
        CantCreate(NewsServerFileName, FALSE);

     NewsServer_F^.Seek(0);
   end; { if }

  NewsServer_F^.BlkRead(LineCfg^.NewsServer^, SizeOf(NewsServerRecord));
  Dispose(NewsServer_F, Done);


  New(Events_F, Init);
  Events_F^.Assign(EventsFileName);
  Events_F^.FileMode := ReadWriteMode + DenyNone;
  if NOT Events_F^.Open(1) then
    begin
      Events_F^.Create(1);
      Events_F^.BlkWrite(Events^, SizeOf(EventRecordArray));

      if Events_F^.IoResult > 0 then
        CantCreate(EventsFileName, FALSE);

      Events_F^.Seek(0);
    end; { if }

  Events_F^.BlkRead(Events^, SizeOf(EventRecordArray));
  Dispose(Events_F, Done);

 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, 'Saving old data...');
 {$ENDIF}
  OldModem^  := LineCfg^.Modem^;
  OldConfig^ := GlobalCfg^.RaConfig^;
  OldEleConfig^ := GlobalCfg^.ElConfig^;
  OldEvents^ := Events^;
  OldTelnet^ := LineCfg^.Telnet^;
  OldNewsServer^ := LineCfg^.NewsServer^;


 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, 'Exitting ReadConfigFiles');
 {$ENDIF}
end; { proc. ReadConfigFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure InitPointers;
begin
  if GlobalCfg <> nil then
    begin
     GlobalCfg^.RaConfig := nil;
      GlobalCfg^.Elconfig := nil;
    end; { if }
  Events := nil;

  if LineCfg <> nil then
    begin
      lineCfg^.Modem := nil;
      LineCfg^.Telnet := nil;
      LineCfg^.Newsserver := nil;
    end; { if }

  OldModem := nil;
  OldEvents := nil;
  OldConfig := nil;
  OldEleConfig := nil;
  OldTelnet := nil;
  OldNewsServer := nil;

  PullMenu := nil;
  mnuFileMenu := nil;
  mnuSystemMenu := nil;
  mnuOptionsMenu := nil;
  mnuModemMenu := nil;
  mnuManagerMenu := nil;
end; { proc. Initpointers }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure StartInitting;
begin
 Initpointers;

 {$IFDEF WITH_DEBUG}
 DebugObj.DebugLog(logFastScrn, '(MiscCfg): Beging of StartInit');
 DebugObj.DebugLog(logFastScrn, '(MiscCfg): Getting memory for: RaConfig');
{$ENDIF}

   AllocMem(GlobalCfg^.RaConfig, SizeOf(ConfigRecord), 'ConfigRecord', 'StartInitting');
   AllocMem(GlobalCfg^.ElConfig, SizeOf(EleConfigRecord), 'EleConfigRecord', 'StartInitting');

 {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, '(MiscCfg): Getting memory for: OldConfig');
 {$ENDIF}
  AllocMem(OldConfig, SizeOf(ConfigRecord), 'ConfigRecord', 'StartInitting');
  AllocMem(OldEleConfig, SizeOf(EleConfigRecord), 'EleConfigRecord', 'StartInitting');
 {$IFDEF WITH_DEBUG}
 DebugObj.DebugLog(logFastScrn, '(MiscCfg): Getting memory for: Modem');
 {$ENDIF}
  AllocMem(LineCfg^.Modem, SizeOf(ModemRecord), 'ModemRecord', 'StartInitting');
 {$IFDEF WITH_DEBUG}
 DebugObj.DebugLog(logFastScrn, '(MiscCfg): Getting memory for: OldModem');
 {$ENDIF}
  AllocMem(OldModem, SizeOf(ModemRecord), 'ModemRecord', 'StartInitting');
 {$IFDEF WITH_DEBUG}
 DebugObj.DebugLog(logFastScrn, '(MiscCfg): Getting memory for: Events');
 {$ENDIF}
  AllocMem(Events, SizeOf(EventRecordArray), 'EventRecordArray', 'StartInitting');
 {$IFDEF WITH_DEBUG}
 DebugObj.DebugLog(logFastScrn, '(MiscCfg): Getting memory for: OldEvents');
 {$ENDIF}
  AllocMem(OldEvents, SizeOf(EventRecordArray), 'EventRecordArray', 'StartInitting');
 {$IFDEF WITH_DEBUG}
 DebugObj.DebugLog(logFastScrn, '(MiscCfg): Getting memory for: Telnet');
 {$ENDIF}
  AllocMem(LineCfg^.Telnet, SizeOf(TelnetRecord), 'TelnetRecord', 'StartInitting');
 {$IFDEF WITH_DEBUG}
 DebugObj.DebugLog(logFastScrn, '(MiscCfg): Getting memory for: OldTelnet');
 {$ENDIF}
  AllocMem(OldTelnet, SizeOf(TelnetRecord), 'TelnetRecord', 'StartInitting');
 {$IFDEF WITH_DEBUG}
 DebugObj.DebugLog(logFastScrn, '(MiscCfg): Getting memory for: NewsServer');
 {$ENDIF}
  AllocMem(LineCfg^.NewsServer, SizeOf(NewsServerRecord), 'NewsServerRecord', 'StartInitting');
 {$IFDEF WITH_DEBUG}
 DebugObj.DebugLog(logFastScrn, '(MiscCfg): Getting memory for: Old newsServer');
 {$ENDIF}
  AllocMem(OldNewsServer, SizeOf(NewsServerRecord), 'OldNewsServerRecord', 'StartInitting');


 {$IFDEF WITH_DEBUG}
 DebugObj.DebugLog(logFastScrn, '(MiscCfg): Loading defaults for: RaConfig^');
 {$ENDIF}
  LoadConfigDefaults(GlobalCfg^.RaConfig^);            { Load config.ra defaults }
  LoadELeConfigDefaults(GlobalCfg^.ElConfig^);         { Load config.ele defaults }


 {$IFDEF WITH_DEBUG}
 DebugObj.DebugLog(logFastScrn, '(MiscCfg): Loading defaults for: Modem^');
 {$ENDIF}
  LoadModemDefaults(LineCfg^.Modem^);                         { Load Modem.RA defaults }
 {$IFDEF WITH_DEBUG}
 DebugObj.DebugLog(logFastScrn, '(MiscCfg): Loading defaults for: Events^');
 {$ENDIF}
  LoadEventsDefaults(Events^);                      { Load Events.RA defaults }
 {$IFDEF WITH_DEBUG}
 DebugObj.DebugLog(logFastScrn, '(MiscCfg): Loading defaults for: Telnet^');
 {$ENDIF}
  LoadTelnetDefaults(LineCfg^.Telnet^);                 { Load Telnet.ELE defaults }
  LoadNewsServerDefaults(LineCfg^.NewsServer^);


 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, '(MiscCfg): Reading configfiles.');
 {$ENDIF}
  ReadConfigFiles;
end; { proc. StartInitting }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure StartMenu;
var Choice   : Word;
    Counter  : Byte;
    PartScrn : Pointer;
    SavePull : Longint;
begin
 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, 'Trying to get enough memory for menus.');
 {$ENDIF}
  if NOT AllocMem(PullMenu, SizeOf(PullMenu^), 'Pullmenu^', 'StartMenu') then
    Error('Not enough memory to run this program...(PullMenu)');
  if NOT AllocMem(mnuFileMenu, SizeOf(mnuFileMenu^), 'Pullmenu^', 'StartMenu') then
    Error('Not enough memory to run this program...(mnuFileMenu)');
  if NOT AllocMem(mnuSystemMenu, SizeOf(mnuSystemMenu^), 'Pullmenu^', 'StartMenu') then
    Error('Not enough memory to run this program...(mnuSystemMenu)');
  if NOT AllocMem(mnuOptionsMenu, SizeOf(mnuOptionsMenu^), 'Pullmenu^', 'StartMenu') then
    Error('Not enough memory to run this program...(mnuOptionsMenu)');
  if NOT AllocMem(mnuModemMenu, SizeOf(mnuModemMenu^), 'Pullmenu^', 'StartMenu') then
    Error('Not enough memory to run this program...(mnuModemMenu)');
  if NOT AllocMem(mnuManagerMenu, SizeOf(mnuManagerMenu^), 'Pullmenu^', 'StartMenu') then
    Error('Not enough memory to run this program...(mnuManagerMenu)');
 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, 'Initialized memory heaps.');
 {$ENDIF}

 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, 'Fillcharing pullmenu');
 {$ENDIF}
  FillChar(PullMenu^, SizeOf(PullMenu^), #00);
 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, 'Fillcharing mnuFileMenu');
 {$ENDIF}
  FillChar(mnuFileMenu^, SizeOf(mnuFileMenu^), #00);
 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, 'Fillcharing mnuSystemMenu');
 {$ENDIF}
  FillChar(mnuSystemMenu^, SizeOf(mnuSystemMenu^), #00);
 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, 'Fillcharing mnuOptionsMenu');
 {$ENDIF}
  FillChar(mnuOptionsMenu^, SizeOf(mnuOptionsMenu^), #00);
 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, 'Fillcharing mnuModemMenu');
 {$ENDIF}
  FillChar(mnuModemMenu^, SizeOf(mnuModemMenu^), #00);
 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, 'Fillcharing mnuManagerMenu');
 {$ENDIF}
  FillChar(mnuManagerMenu^, SizeOf(mnuManagerMenu^), #00);

 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, 'Variables initialized');
 {$ENDIF}

  InitSubMenus;                               { Initialize the pulldown menus }

 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, 'Init submenus done.');
 {$ENDIF}

  AddMenuItem(PullMenu^.MenuInf[1], ' File '  ,  'F', 01, '', 09, mnuFileMenu^);
  AddMenuItem(PullMenu^.MenuInf[2], ' System ',  'D', 02, '', 21, mnuSystemMenu^);
  AddMenuItem(PullMenu^.MenuInf[3], ' Options ', 'G', 03, '', 35, mnuOptionsMenu^);
  AddMenuItem(PullMenu^.MenuInf[4], ' Modem '  , 'D', 04, '', 50, mnuModemMenu^);
  AddMenuItem(PullMenu^.MenuInf[5], ' Manager ', 'D', 05, '', 63, mnuManagerMenu^);

  PullMenu^.Items       := 05;
  PullMenu^.HiLight     := 01;
  PullMenu^.Width       := 10;
  PullMenu^.X           := 05;
  PullMenu^.Y           := 01;

  SaveScreen(MainScreenSaver);
  Choice := 01; { important! make sure that savepartscrn<> is executed }
                { so that we atleast have a screen thats saved!! }

  REPEAT
    {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logFastScrn, 'Saving screen.....');
    {$ENDIF}

   SavePull := mnuPullColor;
   mnuPullColor := mnuNormColor;
   if Choice <> 00 then
     SavePart(PartScrn, 1, 2, mnuScrnWidth, mnuScrnlen);

 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logFastScrn, 'Topbar');
 {$ENDIF}
   Choice := TopBar(PullMenu^);
 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logFastScrn, 'Choice was: '+FStr(choice));
 {$ENDIF}

   mnuPullColor := LightGray;
   mnuPullColor := SavePull;

   EdittingMsgArea := False;
   EdittingFileArea := False;
   EdittingGroupRecord := False;

   Case Choice of
      { Switches }         100 : ShowSwitches;
      { Info }             200 : ShowInfo;
      { Exit }             300 : Choice := 00;
      { DOS Shell }        400 : DoDosShell('Type "EXIT" to return to '+ConfigName+'.');
      { Paths }            500 : DoPaths;
      { SiteInfo }         600 : DoSiteInfo;
      { Addresses }        700 : AkaLister(true);
      { Security }         800 : DoSecurity;
      { Messages }         900 : DoMessage;
      { Files }           1000 : DoFiles;
      { Restrictions }    1100 : DoRestrictions;
      { ErrorLevels }     1200 : DoErrorLevels;
      { Display }         1300 : DoDisplay;
      { Colours }         1400 : DoColours;
      { Paging }          1500 : DoPaging;
      { NewUser }         1600 : DoNewUser;
      { System }          1700 : DoSystem;
      { Prompts }         1800 : DoPrompts;
      { Modem Options }   2000 : DoModemOptions;
      { Modem Commands }  2100 : DoModemCommands;
      { Modem responses } 2200 : DoModemResponses;
      { Message Areas }   2300 : DoMessageAreas;
      { Message groups }  2400 : begin
                                    GroupFileName := MGroupsFileName;
                                    GroupTitle := ' Message groups ';
                                    GroupMessage := True;
                                    DoGroupList;
                                 end; { Message Groups }
      { File areas }      2500 : DoFileAreas;
      { File Groups }     2600 : begin
                                    GroupFileName := FGroupsFileName;
                                    GroupTitle := ' File groups ';
                                    GroupMessage := False;
                                    DoGroupList;
                                 end; { Message Groups }
      { Protocols }       2700 : DoProtocols;
      { LanguageEditor }  2800 : DoEditLanguage;
      { ALT-F[n] Keys }   2900 : DoEditAltKeys;
      { EditEvents }      3000 : DoEditEvents;
      { Menus }           3100 : DoEditMenus('');
      { Editten }         3200 : DoEditCtlFiles;
      { Combined }        3300 : DoCombined;
      { Limits }          3400 : DoLimits;
      { Modem -> Telnet } 3500 : DoModemTelnet;
      { Newsserver }      3600 : DoNewsserver;
   end; { Case Choice }

   if Choice=00 then
    if NOT DoExit then Choice := 01;

 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logFastScrn, 'Restoring screen.....');
 {$ENDIF}

   if PartScrn <> nil then
    if Choice <> 0 then
     begin
       RestorePart(PartScrn, false);
       PartScrn := nil;
     end; { if }

 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logFastScrn, 'Screen restored.....');
 {$ENDIF}

  Until Choice=00;

  {-- restore the original screen ------------------------------------------}
{   RestoreScreen(MainScreenSaver); }
end; { StartMenu }

procedure MiscInit;
begin
  Initpointers;
 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, 'Setting exitproc to new.');
 {$ENDIF}
  OldExitProc := ExitProc;
  ExitProc := @NewExitProc;
end;

{$IFNDEF OVERLAY}
begin
  MiscInit;
{$ENDIF}
End. { Unit. MiscCFG }
