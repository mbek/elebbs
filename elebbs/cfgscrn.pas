unit CfgScrn;
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
** CFGSCRN.TPU, Screen procedures for ElCONFIG
**
** Copyright (c) 1997 by Maarten Bekers
**
** Created : 21-Sep-1997
** Last update : 21-Sep-1997
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
  This unit should not be compiled into EleWEB
{$ENDIF}

uses CfgDef, Global, GenCfg, CfgRec, MemMan, Menusys;

Type MenuListRec = Array[1..2000] of String[8];

var Events       : ^EventRecordArray;
    Menulist     : ^MenuListRec;
    MenuPath     : String;
    OldConfig    : ^ConfigRecord;
    OldEleConfig : ^EleConfigRecord;
    OldModem     : ^ModemRecord;
    OldEvents    : ^EventRecordArray;
    OldTelnet    : ^TelnetRecord;
    OldNewsServer: ^NewsServerRecord;


procedure Error(S: String);
procedure CreateScreen;
procedure Config_MessageBox(S: String);
procedure InitSubMenus;
procedure ShowSwitches;
procedure MakeEditBox(EditLen, Len, ShowEditLen: Byte; var S: String; Title: String; ColorMap: Boolean; Menu: PullRecord);
procedure Text_Editor(Name: String; Menu: PullRecord; DoColor: Boolean);

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses {$IFDEF DELCRT}
       Crt32,
     {$ELSE}
       Crt,
     {$ENDIF}

     Dos, Strings,
     {$IFDEF MSDOS}
       Memory,
       Exec,
     {$ENDIF}
     SysUtils, FidoAddr, Debug_U, ScrnU, StrEdit, Editor,
     ObjDec;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure Error(S: String);
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logFastScrn, 'Fatal Error: '+S); {$endif}
  ClrScr;
  TextColor(15); TextBackground(00);
  Writeln('þ Fatal Error occured!',^G);
  WriteLn('þ ',S);
  Halt(0);
end; { proc. Error }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CreateScreen;
var SaveDirect: Boolean;
begin
  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;

 {$IFDEF WITH_DEBUG}
 DebugObj.DebugLog(logFastScrn, '(MiscCfg): Setting cursor off.');
 {$ENDIF}
 CursorOff;

 TextAttr := LightGray;

 {$IFDEF WITH_DEBUG}
 DebugObj.DebugLog(logFastScrn, '(MiscCfg): Screen clear and building.');
 {$ENDIF}

 ScreenClear(mnuBackGrCol, mnuBackGrChar);               { Initialize the screen area }
 PartClear(1, 1, mnuScrnWidth, 1, LightGray, ' ');
 PartClear(1, 2, mnuScrnWidth, 2, mnuTopBar, mnuDblWide);
 PartClear(1, mnuScrnLen-1, mnuScrnWidth, mnuScrnLen-1, mnuTopBar, mnuSngWide);
 PartClear(1, mnuScrnLen, mnuScrnWidth, mnuScrnLen, LightGray, ' ');

 WriteCenter((mnuScrnLen div 2), 112, ' '+FullProgName+'; CONFIG '+VersionID+' ');
 WriteCenter((mnuScrnLen div 2) + 2, 112, ' Copyright (C) 1996-2003 Maarten Bekers ');
 DirectScrnUpdate := SaveDirect;
 WriteCenter((mnuScrnLen div 2) + 4, 112 ,' All rights reserved. ');

 {$IFDEF WITH_DEBUG}
 DebugObj.DebugLog(logFastScrn, '(MiscCfg): Created screen.');
 {$ENDIF}
end; { proc. CreateScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Config_MessageBox(S: String);
var XLen      : Byte;
    XStart    : Byte;
    SaveScrn  : Pointer;
    SaveDirect: Boolean;
begin
  SaveScreen(SaveScrn);
  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;

  if Length(S) > 75 then S[0] := #76;
  XLen := 02 + Length(S) + 02;
  XStart := (mnuScrnWidth div 2) - (XLen div 2);

  ShadFillBoxTitle(XStart, 10, (XStart + Xlen) - 01, 14, mnuBoxColor, mnuStyle, True, ' ERROR ');
  DirectScrnUpdate := SaveDirect;
  WriteAT(XStart + 02, 12, Lightgray, S);
  GetInput;

  RestoreScreen(SaveScrn);
end; { proc. Config_MessageBox }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure InitSubMenus;
begin;
 {$IFDEF WITH_DEBUG}
 DebugObj.DebugLog(logFastScrn, 'Initiaed initsubmenus');
 {$ENDIF}

 if NOT MemMan.AllocMem(mnuFileMenu^.PullInf, SizeOf(mnuFileMenu^.PullInf^),
      'Menu.PullInf^', 'InitSubMenus') then
      Error('Unable to initialize file menu (memory error)');
 Initpullmenu(mnuFileMenu^);


 AddPullItem(mnuFileMenu^.PullInf^[01], ' Switches  ',  100, 'S', '', 1);
 AddPullItem(mnuFileMenu^.PullInf^[02], ' Info      ',  200, 'I', '', 1);
 AddPullItem(mnuFileMenu^.PullInf^[03], ' Exit      ',  300, 'E', '', 1);
 AddPullItem(mnuFileMenu^.PullInf^[04], ' DOS Shell ',  400, 'D', '', 1);

 mnuFileMenu^.Items  := 04;
 mnuFileMenu^.Width  := 12;
 mnuFileMenu^.Length := 04;
 mnuFileMenu^.X      := 08;
 mnuFileMenu^.Y      := 02;
 mnuFileMenu^.HiLight:= 01;

 {$IFDEF WITH_DEBUG}
 DebugObj.DebugLog(logFastScrn, 'Initiating mnuSystemMenu');
 {$ENDIF}

 if NOT MemMan.AllocMem(mnuSystemMenu^.PullInf, SizeOf(mnuSystemMenu^.PullInf^), 'Menu.PullInf^', 'InitSubMenus') then
      Error('Unable to initialize system menu (memory error)');
 InitpullMenu(mnuSystemMenu^);


 AddPullItem(mnuSystemMenu^.PullInf^[01], ' Paths     ',  500, 'P', '', 1);
 AddPullItem(mnuSystemMenu^.PullInf^[02], ' Site info ',  600, 'S', '', 1);
 AddPullItem(mnuSystemMenu^.PullInf^[03], ' Addresses ',  700, 'A', '', 1);
 AddPullItem(mnuSystemMenu^.PullInf^[04], ' Security  ',  800, 'S', '', 1);

 mnuSystemMenu^.Items  := 04;
 mnuSystemMenu^.Width  := 12;
 mnuSystemMenu^.Length := 04;
 mnuSystemMenu^.X      := 20;
 mnuSystemMenu^.Y      := 02;
 mnuSystemMenu^.HiLight:= 01;


 {$IFDEF WITH_DEBUG}
 DebugObj.DebugLog(logFastScrn, 'Initiating mnuOptiosnMenu');
 {$ENDIF}

 if NOT MemMan.AllocMem(mnuOptionsMenu^.PullInf, SizeOf(mnuOptionsMenu^.PullInf^), 'Menu.PullInf^', 'InitSubMenus') then
      Error('Unable to initialize options menu (memory error)');
 InitpullMenu(mnuOptionsMenu^);

 AddPullItem(mnuOptionsMenu^.PullInf^[01], ' Messages     ',  900, 'M', '', 1);
 AddPullItem(mnuOptionsMenu^.PullInf^[02], ' Files        ', 1000, 'F', '', 1);
 AddPullItem(mnuOptionsMenu^.PullInf^[03], ' Restrictions ', 1100, 'R', '', 1);
 AddPullItem(mnuOptionsMenu^.PullInf^[04], ' Errorlevels  ', 1200, 'E', '', 1);
 AddPullItem(mnuOptionsMenu^.PullInf^[05], ' Display      ', 1300, 'D', '', 1);
 AddPullItem(mnuOptionsMenu^.PullInf^[06], ' Colours      ', 1400, 'C', '', 1);
 AddPullItem(mnuOptionsMenu^.PullInf^[07], ' Paging       ', 1500, 'P', '', 1);
 AddPullItem(mnuOptionsMenu^.PullInf^[08], ' New users    ', 1600, 'N', '', 1);
 AddPullItem(mnuOptionsMenu^.PullInf^[09], ' System       ', 1700, 'S', '', 1);
 AddPullItem(mnuOptionsMenu^.PullInf^[10], ' Prompts      ', 1800, 'P', '', 1);
 AddPullItem(mnuOptionsMenu^.PullInf^[11], ' Newsserver   ', 3600, 'W', '', 1);

 mnuOptionsMenu^.Items  := 11;
 mnuOptionsMenu^.Width  := 15;
 mnuOptionsMenu^.Length := 11;
 mnuOptionsMenu^.X      := 35;
 mnuOptionsMenu^.Y      := 02;
 mnuOptionsMenu^.HiLight:= 01;


 {$IFDEF WITH_DEBUG}
 DebugObj.DebugLog(logFastScrn, 'Initiating mnuModemMenu');
 {$ENDIF}

 if NOT MemMan.AllocMem(mnuModemMenu^.PullInf, SizeOf(mnuModemMenu^.PullInf^),
                        'Menu.PullInf^', 'InitSubMenus') then
      Error('Unable to initialize modem menu (memory error)');
 InitPullMenu(mnuModemMenu^);

 AddPullItem(mnuModemMenu^.PullInf^[01], ' Options   ', 2000, 'O', '', 1);
 AddPullItem(mnuModemMenu^.PullInf^[02], ' Commands  ', 2100, 'C', '', 1);
 AddPullItem(mnuModemMenu^.PullInf^[03], ' Responses ', 2200, 'R', '', 1);
 AddPullItem(mnuModemMenu^.PullInf^[04], ' Telnet    ', 3500, 'T', '', 1);

 mnuModemMenu^.Items  := 4;
 mnuModemMenu^.Width  := 12;
 mnuModemMenu^.Length := 4;
 mnuModemMenu^.X      := 50;
 mnuModemMenu^.Y      := 02;
 mnuModemMenu^.HiLight:= 01;



 {$IFDEF WITH_DEBUG}
 DebugObj.DebugLog(logFastScrn, 'Initiating mnuManager');
 {$ENDIF}

 if NOT MemMan.AllocMem(mnuManagerMenu^.PullInf, SizeOf(mnuManagerMenu^.PullInf^),
                        'Menu.PullInf^', 'InitSubMenus') then
      Error('Unable to initialize manager menu (memory error)');

 AddPullItem(mnuManagerMenu^.PullInf^[01], ' Msg areas  ', 2300, 'M', '', 1);
 AddPullItem(mnuManagerMenu^.PullInf^[02], ' ' + StyleStr[mnuStyle, 7] + StyleStr[mnuStyle, 10] + 'Groups   ', 2400, 'G','',1);
 AddPullItem(mnuManagerMenu^.PullInf^[03], ' File areas ', 2500, 'F', '', 1);
 AddPullItem(mnuManagerMenu^.PullInf^[04], ' ' + StyleStr[mnuStyle, 7] + StyleStr[mnuStyle, 10] + 'Groups   ', 2600, 'G','',1);
 AddPullItem(mnuManagerMenu^.PullInf^[05], ' Protocols  ', 2700, 'P', '', 1);
 AddPullItem(mnuManagerMenu^.PullInf^[06], ' Languages  ', 2800, 'L', '', 1);
 AddPullItem(mnuManagerMenu^.PullInf^[07], ' AltFn keys ', 2900, 'A', '', 1);
 AddPullItem(mnuManagerMenu^.PullInf^[08], ' Events     ', 3000, 'E', '', 1);
 AddPullItem(mnuManagerMenu^.PullInf^[09], ' Menus      ', 3100, 'M', '', 1);
 AddPullItem(mnuManagerMenu^.PullInf^[10], ' Ctl files  ', 3200, 'C', '', 1);
 AddPullItem(mnuManagerMenu^.PullInf^[11], ' Combined   ', 3300, 'C', '', 1);
 AddPullItem(mnuManagerMenu^.PullInf^[12], ' Limits     ', 3400, 'L', '', 1);

 mnuManagerMenu^.Items  := 12;
 mnuManagerMenu^.Width  := 13;
 mnuManagerMenu^.Length := 12;
 mnuManagerMenu^.X      := 63;
 mnuManagerMenu^.Y      := 02;
 mnuManagerMenu^.HiLight:= 01;


 {$IFDEF WITH_DEBUG}
 DebugObj.DebugLog(logFastScrn, 'Leaving mnuManager');
 {$ENDIF}
end; { proc. InitSubMenus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure ShowSwitches;
var SaveDirect: Boolean;
begin
  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;
  ShadFillBoxTitle(17, 09, 62, 19, mnuBoxColor, mnuStyle, True, ' Command-line switches ');

  WriteAT(19, 11, Lightgray, 'ELCONFIG Command-line switches:');
  WriteAT(20, 13, Lightgray, '-B Force black & white (monochrome) mode');
  WriteAT(20, 14, Lightgray, '-N No dupe msg/file area number checking');
  WriteAT(20, 16, Lightgray, '-L Use the language manager directly');
  DirectScrnUpdate := SaveDirect;
  WriteAT(20, 17, Lightgray, '-M Use the menu editor directly');
  GetInput;
end; { proc. Switches }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure MakeEditBox(EditLen, Len, ShowEditLen: Byte; var S: String; Title: String; ColorMap: Boolean; Menu: PullRecord);
var SaveScrn  : Pointer;
    SaveDirect: Boolean;
begin
  SaveScreen(SaveScrn);

  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;
  EnDisableMenu(Menu, False);
  ShadFillBoxTitle(02, 17, 02 + Len, 21, mnuBoxColor, mnuStyle, True, Title);
  DirectScrnUpdate := SaveDirect;
  GetString(04, 19, mnuEditColor, S, [#32..#254], [#13, #27], EditLen, ShowEditLen, False, ColorMap, True, false,0);
  (* S := Edit(S, 04, 19, mnuEditColor, EditLen, False); *)

  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;
  EnDisableMenu(Menu, True);
  RestoreScreen(SaveScrn);
  DirectScrnUpdate := SaveDirect;
end; { proc. MakeEditBox }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Text_Editor(Name: String; Menu: PullRecord; DoColor: Boolean);

procedure MsgBox(S: String);
var SaveScrn: Pointer;
begin
  SaveScreen(SaveScrn);

  CursorOff;
  ShadFillBox(25, 07, 60, 11, mnuBoxColor, mnuStyle, True);
  WriteCenter(09, mnuMsgColor, S);

  While Keypressed do ReadKey;
  If ReadKey=#00 then ReadKey;

  RestoreScreen(SaveScrn);
end; { proc. MsgBox }

procedure LoadFile(FName: String);
var F: Text;
    S: String;
begin
  Assign(F, fname);
  {$i-} System.Reset(f); {$i+}
  if IOResult>00 then EXIT;

  LastString := 00;

  While NOT Eof(F) do
    begin
      Inc(LastString);
      ReadLn(F, S);

      if Length(S) > MaxStrLen then S[0] := Chr(MaxStrLen);
      Txt^[LastString] := S;

      if LastString >= NumOfStrings then
        begin
          MsgBox('File too large to load!');
          Editor_Redraw;
          EXIT;
        end;
    end; { while }

  {$i-} Close(f); {$i+}
  if IOResult> 00 then ;

  Editor_Redraw;
end; { func. LoadFile }

procedure SaveFile(FName: String);
var F       : Text;
    Counter : Longint;
begin
  if NOT Editor_Modified then EXIT;
  if NOT DoSaveChanges('Save Changes (Y/n)? °', true, false) then EXIT;

  Assign(F, fname);
  {$i-} System.ReWrite(f); {$i+}
  if IOResult>00 then EXIT;

  For Counter := 01 to LastString do
    begin
      {$i-} WriteLn(F, Txt^[Counter]); {$i+}
      if IOResult>00 then BREAK;
    end; { for }

  {$i-} Close(f); {$i+}
  if IOResult> 00 then ;
end; { func. LoadFile }

var EditX   : Longint;
    EditY   : Longint;
    SaveScrn: Pointer;
begin
  If Name='' then
    MakeEditBox(60, 63, 60, Name, ' Edit text file ', False, Menu);
  If Name='' then EXIT;

  EditX := 01;
  EditY := 01;

  if DoColor then
    begin
      WindowColor := mnuBoxColor;
      NormColor := mnuMsgColor;
    end; { if }

  SaveScreen(SaveScrn);

  if NOT Editor_InitEditor(02, 02, 79, mnuScrnlen - 01, true) then
    begin
      MsgBox('Not enough memory!');
      RestoreScreen(SaveScrn);
      EXIT;
    end; { if }

  LoadFile(Name);

  Editor_Edit(EditX, EditY);

  SaveFile(Name);

  Editor_DoneEditor(EditX, EditY);

  RestoreScreen(SaveScrn);
end; { proc. Text_Editor }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit CFGSCRN }
