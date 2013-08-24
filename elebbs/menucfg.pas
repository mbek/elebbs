unit MenuCfg;
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
** MENUCFG.TPU, MenuEditor for ElConfig
**
** Copyright (c) 1996, 97 by Maarten Bekers
**
** Created : 12-Sep-1997
** Last update : 12-Sep-1997
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
  This unit should not be compiled in EleBBS
{$ENDIF}

uses CfgRec;

procedure EditMenu(MenuPath: String; var MenuName: String);
procedure EditMenuRecord(var MenuInf: MnuRecord; var LBar: LightBarRecord);

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Global, CfgDef, ReadCfg, Area_lst, AreaDef, StrPath, ListSys,
     Strings, SysUtils, ScrnU, RecDif, GenCfg, Cases, MenuSys,
     MemMan, Stutils, Colors, StrEdit, WordStr, GenFile, FileRout,
     CentrStr, LongStr, BitWise, FlagStr, FileObj;

Const TotalMenuFunctions = 101;

Type MnuListTypeRec = (mnuDisplay,  mnuMnuFuncs, mnuTxtFiles, mnuGeneral,
                       mnuMsgBase,  mnuFileArea, mnuExternal, mnuChange,
                       mnuInternet, mnuNone);

Type MnuDisplayRec = Record
                        FullName  : PChar;
                        ShortName : PChar;
                        MenuType  : MnuListTypeRec;
                     end; { for }

Const MenuDisplayRec : Array[0..TotalMenuFunctions] of MnuDisplayRec = (
{ 00 }   (FullName: 'Display only';                                 ShortName: '';           MenuType: mnuDisplay),
{ 01 }   (FullName: 'Goto another menu';                            ShortName: 'Goto menu';  MenuType: mnuMnuFuncs),
{ 02 }   (FullName: 'Gosub another menu';                           ShortName: 'Gosub menu'; MenuType: mnuMnuFuncs),
{ 03 }   (FullName: 'Return from gosub';                            ShortName: 'Return';     MenuType: mnuMnuFuncs),
{ 04 }   (FullName: 'Clear gosubs & goto menu';                     ShortName: 'Clear/goto'; MenuType: mnuMnuFuncs),
{ 05 }   (FullName: 'Display .ANS/.ASC file';                       ShortName: 'Display';    MenuType: mnuTxtFiles),
{ 06 }   (FullName: 'Selection menu';                               ShortName: 'Select menu';MenuType: mnuMnuFuncs),
{ 07 }   (FullName: 'Execute sub-program';                          ShortName: 'Execute';    MenuType: mnuExternal),
{ 08 }   (FullName: 'Version information';                          ShortName: 'Version';    MenuType: mnuGeneral),
{ 09 }   (FullName: 'Terminate call (logoff)';                      ShortName: 'Logoff';     MenuType: mnuGeneral),
{ 10 }   (FullName: 'Display system usage graph';                   ShortName: 'Usag graph'; MenuType: mnuGeneral),
{ 11 }   (FullName: 'Page Systems Operator for a chat';             ShortName: 'Page Sysop'; MenuType: mnuGeneral),
{ 12 }   (FullName: 'Execute a script (EleXer or Q-A)';             ShortName: 'Script';     MenuType: mnuGeneral),
{ 13 }   (FullName: 'View/Scan user-list';                          ShortName: 'User-list';  MenuType: mnuGeneral),
{ 14 }   (FullName: 'Current call time statistics';                 ShortName: 'Time stat';  MenuType: mnuGeneral),
{ 15 }   (FullName: 'Exit with errorlevel';                         ShortName: 'Exit';       MenuType: mnuExternal),
{ 16 }   (FullName: 'Change user''s location';                      ShortName: 'Ch Locatn';  MenuType: mnuChange),
{ 17 }   (FullName: 'Change user''s password';                      ShortName: 'Ch Passwrd'; MenuType: mnuChange),
{ 18 }   (FullName: 'Change user''s screen length';                 ShortName: 'Ch Length';  MenuType: mnuChange),
{ 19 }   (FullName: 'Adjust screen-clearing';                       ShortName: 'Pg clring';  MenuType: mnuChange),
{ 20 }   (FullName: 'Adjust page-pausing';                          ShortName: 'Pg pause';   MenuType: mnuChange),
{ 21 }   (FullName: 'Adjust ANSI capabilities';                     ShortName: 'Ch Graphix'; MenuType: mnuChange),
{ 22 }   (FullName: 'Check personal mail-box';                      ShortName: 'Mail check'; MenuType: mnuMsgBase),
{ 23 }   (FullName: 'Read messages';                                ShortName: 'Read msgs';  MenuType: mnuMsgBase),
{ 24 }   (FullName: 'Scan messages';                                ShortName: 'Scan msgs';  MenuType: mnuMsgBase),
{ 25 }   (FullName: 'Quick-Scan messages';                          ShortName: 'QScan msgs'; MenuType: mnuMsgBase),
{ 26 }   (FullName: 'Delete a specified message';                   ShortName: 'Delete msg'; MenuType: mnuMsgBase),
{ 27 }   (FullName: 'Post a new message';                           ShortName: 'Post msg';   MenuType: mnuMsgBase),
{ 28 }   (FullName: 'Choose ''combined'' msg areas';                ShortName: 'Pick areas'; MenuType: mnuMsgBase),
{ 29 }   (FullName: 'Move file to another area';                    ShortName: 'Move file';  MenuType: mnuFileArea),
{ 30 }   (FullName: 'Direct disk-directory listing';                ShortName: 'Directory';  MenuType: mnuFileArea),
{ 31 }   (FullName: 'List files in area';                           ShortName: 'Files list'; MenuType: mnuFileArea),
{ 32 }   (FullName: 'Download (send to user) file(s)';              ShortName: 'Download';   MenuType: mnuFileArea),
{ 33 }   (FullName: 'Upload (receive from user) file(s)';           ShortName: 'Upload';     MenuType: mnuFileArea),
{ 34 }   (FullName: 'List files in specified archive';              ShortName: 'List arc';   MenuType: mnuFileArea),
{ 35 }   (FullName: 'Search file-areas for a keyword';              ShortName: 'Search key'; MenuType: mnuFileArea),
{ 36 }   (FullName: 'Search file-areas for a filename';             ShortName: 'Search nam'; MenuType: mnuFileArea),
{ 37 }   (FullName: 'Search for newfiles since last call';          ShortName: 'New files';  MenuType: mnuFileArea),
{ 38 }   (FullName: 'Display a text file in a file area';           ShortName: 'View file';  MenuType: mnuFileArea),
{ 39 }   (FullName: 'Display a direct text file (any dir)';         ShortName: 'File-dir';   MenuType: mnuTxtFiles),
{ 40 }   (FullName: 'Display .ANS/.ASC file with menu HotKeys';     ShortName: 'Display-H';  MenuType: mnuTxtFiles),
{ 41 }   (FullName: 'Adjust use of full-screen/line-editor';        ShortName: 'Msg editor'; MenuType: mnuChange),
{ 42 }   (FullName: 'Adjust use of HotKey menus';                   ShortName: 'Ch HotKeys'; MenuType: mnuChange),
{ 43 }   (FullName: '{+} List msg areas that contain new mess';     ShortName: '+Newmail';   MenuType: mnuMsgBase),
{ 44 }   (FullName: 'Clear all selected ''combined'' message ar';   ShortName: 'Clr areas';  MenuType: mnuMsgBase),
{ 45 }   (FullName: 'Display .ANS/.ASC with CR pause at end';       ShortName: 'Disp CR';    MenuType: mnuTxtFiles),
{ 46 }   (FullName: 'Display direct text file (any dir) with';      ShortName: 'Disp CR/D';  MenuType: mnuTxtFiles),
{ 47 }   (FullName: 'Make an entry in the system log';              ShortName: 'Log entry';  MenuType: mnuGeneral),
{ 48 }   (FullName: 'Download a specific file';                     ShortName: 'Dnld spec';  MenuType: mnuFileArea),
{ 49 }   (FullName: 'Select the current template msg area';         ShortName: 'Msgareas';   MenuType: mnuMsgBase),
{ 50 }   (FullName: 'Select the current template file area';        ShortName: 'Filearea';   MenuType: mnuFileArea),
{ 51 }   (FullName: 'Todays callers list';                          ShortName: 'T Callers';  MenuType: mnuGeneral),
{ 52 }   (FullName: '"Who''s online" list';                         ShortName: 'Who online'; MenuType: mnuGeneral),
{ 53 }   (FullName: 'Allow user to toggle "do not disturb" flag';   ShortName: 'Adj distrb'; MenuType: mnuChange),
{ 54 }   (FullName: 'Send a message to a user on another line';     ShortName: 'Send msg';   MenuType: mnuGeneral),
{ 55 }   (FullName: 'Download *any* file (no restrictions)';        ShortName: 'Dnld any';   MenuType: mnuFileArea),
{ 56 }   (FullName: '{+} Browse nodelist';                          ShortName: 'Nodelist';   MenuType: mnuMsgBase),
{ 57 }   (FullName: 'Change home/voice phone number';               ShortName: 'New home#';  MenuType: mnuChange),
{ 57 }   (FullName: 'Change work/data phone number';                ShortName: 'New work#';  MenuType: mnuChange),
{ 59 }   (FullName: '{+} Global download';                          ShortName: '+GlobalDL';  MenuType: mnuFileArea),
{ 60 }   (FullName: 'Change handle';                                ShortName: 'New handle'; MenuType: mnuChange),
{ 61 }   (FullName: 'Toggle AVATAR codes';                          ShortName: 'Toggle AVT'; MenuType: mnuChange),
{ 62 }   (FullName: 'Toggle fullscreen message reader';             ShortName: 'Reader';     MenuType: mnuChange),
{ 63 }   (FullName: 'Select a new language';                        ShortName: 'Language';   MenuType: mnuChange),
{ 64 }   (FullName: 'Select a new date format';                     ShortName: 'Date fmt';   MenuType: mnuChange),
{ 65 }   (FullName: 'Change user''s flag';                          ShortName: 'User flags'; MenuType: mnuChange),
{ 66 }   (FullName: 'Toggle text file shells';                      ShortName: 'Txt shells'; MenuType: mnuGeneral),
{ 67 }   (FullName: 'Toggle auto message-forwarding';               ShortName: 'Auto fwd';   MenuType: mnuChange),
{ 68 }   (FullName: 'Create a new real time conference';            ShortName: 'Create RTC'; MenuType: mnuGeneral),
{ 69 }   (FullName: 'Join an existing real time conference';        ShortName: 'Join RTC';   MenuType: mnuGeneral),
{ 70 }   (FullName: 'Delete an existing real time conference';      ShortName: 'Del RTC';    MenuType: mnuGeneral),
{ 71 }   (FullName: 'View/edit list of tagged files';               ShortName: 'Review tag'; MenuType: mnuFileArea),
{ 72 }   (FullName: 'Select new default protocol';                  ShortName: 'Protocol';   MenuType: mnuChange),
{ 73 }   (FullName: 'Toggle echomail in mailbox scan';              ShortName: 'New echo';   MenuType: mnuChange),
{ 74 }   (FullName: 'Change user''s mailing address';               ShortName: 'New addr';   MenuType: mnuChange),
{ 75 }   (FullName: 'Mailbox scan: selected only/all areas';        ShortName: 'Scan sel';   MenuType: mnuChange),
{ 76 }   (FullName: 'Display a RIP icon (will send if necessary)';  ShortName: 'RIP Icon';   MenuType: mnuTxtFiles),
{ 77 }   (FullName: '-Unused-';                                     ShortName: '??';         MenuType: mnuNone),
{ 78 }   (FullName: '-Unused-';                                     ShortName: '??';         MenuType: mnuNone),
{ 79 }   (FullName: '-Unused-';                                     ShortName: '??';         MenuType: mnuNone),
{ 80 }   (FullName: '-Unused-';                                     ShortName: '??';         MenuType: mnuNone),
{ 81 }   (FullName: '-Unused-';                                     ShortName: '??';         MenuType: mnuNone),
{ 82 }   (FullName: '-Unused-';                                     ShortName: '??';         MenuType: mnuNone),
{ 83 }   (FullName: '-Unused-';                                     ShortName: '??';         MenuType: mnuNone),
{ 84 }   (FullName: '-Unused-';                                     ShortName: '??';         MenuType: mnuNone),
{ 85 }   (FullName: '-Unused-';                                     ShortName: '??';         MenuType: mnuNone),
{ 86 }   (FullName: '-Unused-';                                     ShortName: '??';         MenuType: mnuNone),
{ 87 }   (FullName: '-Unused-';                                     ShortName: '??';         MenuType: mnuNone),
{ 88 }   (FullName: '-Unused-';                                     ShortName: '??';         MenuType: mnuNone),
{ 89 }   (FullName: '-Unused-';                                     ShortName: '??';         MenuType: mnuNone),
{ 90 }   (FullName: '-Unused-';                                     ShortName: '??';         MenuType: mnuNone),
{ 91 }   (FullName: '-Unused-';                                     ShortName: '??';         MenuType: mnuNone),
{ 92 }   (FullName: '-Unused-';                                     ShortName: '??';         MenuType: mnuNone),
{ 93 }   (FullName: '-Unused-';                                     ShortName: '??';         MenuType: mnuNone),
{ 94 }   (FullName: '-Unused-';                                     ShortName: '??';         MenuType: mnuNone),
{ 95 }   (FullName: '-Unused-';                                     ShortName: '??';         MenuType: mnuNone),
{ 96 }   (FullName: '-Unused-';                                     ShortName: '??';         MenuType: mnuNone),
{ 97 }   (FullName: '-Unused-';                                     ShortName: '??';         MenuType: mnuNone),
{ 98 }   (FullName: '-Unused-';                                     ShortName: '??';         MenuType: mnuNone),
{ 99 }   (FullName: '-Unused-';                                     ShortName: '??';         MenuType: mnuNone),
{ 100 }  (FullName: 'Join IRC server';                              ShortName: 'Join IRC';   MenuType: mnuInternet),
{ 101 }  (FullName: 'Telnet out';                                   ShortName: 'Telnet';     MenuType: mnuInternet)
                                                        );

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


Type PickMenuItemsRec = Array[1..103] of String;


var PickMenuItems : ^PickMenuItemsRec;
    TotalMenuItems: Byte;
    TotalMenuName : String;


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Menu_CreateNewMenu: Boolean;
var NewMenu_F   : pFileObj;
    MenuInf     : MnuRecord;
begin
  Menu_CreateNewMenu := False;

  New(NewMenu_F, Init);
  NewMenu_F^.Assign(TotalMenuName);
  if NOT NewMenu_F^.Create(1) then
    begin
       CantCreate(TotalMenuName, False);

       Dispose(NewMenu_F, Done);
       EXIT;
    end; { if }

  FillChar(MenuInf, SizeOf(mnuRecord), #00);

  LoadMenuDefaults(MenuInf);
  MenuInf.Typ := 15;
  MenuInf.ForeGround := 15;

  NewMenu_F^.BlkWrite(MenuInf, SizeOf(mnuRecord));
  LoadMenuDefaults(MenuInf);
  NewMenu_F^.BlkWrite(MenuInf, SizeOf(mnuRecord));

  Dispose(NewMenu_F, Done);

  Menu_CreateNewMenu := true;
end; { proc. CreateNewMenu }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Menu_CreateNewLBar;
var NewMenu_F   : pFileObj;
    LBarInfo    : LightBarRecord;
begin
  New(NewMenu_F, Init);
  NewMenu_F^.Assign(NoExtension(TotalMenuName) + '.mlb');
  NewMenu_F^.FileMode := ReadWriteMode + DenyNone;

  if NOT NewMenu_F^.Create(1) then
    begin
       CantCreate(NoExtension(TotalMenuName) + '.mlb', False);

       Dispose(NewMenu_F, Done);
       EXIT;
    end; { if }

  FillChar(LBarInfo, SizeOf(LightBarRecord), #00);

  LoadLightBarDefaults(LBarInfo);

  NewMenu_F^.Blkwrite(LBarInfo, SizeOf(LightBarRecord));
  LoadLightBarDefaults(LBarInfo);
  NewMenu_F^.Blkwrite(LBarInfo, SizeOf(LightBarRecord));

  Dispose(NewMenu_F, Done);
end; { proc. Menu_CreateNewLBar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function MenuRec_GetItems: LongInt;
var MenuInf: mnuRecord;
begin
  if (Menu_F^.FileSize div SizeOf(mnuRecord)) = 01 then
    begin
      FillChar(MenuInf, SizeOf(mnuRecord), #00);
      LoadMenuDefaults(MenuInf);

      Menu_F^.Seek(Menu_F^.FileSize);
      Menu_F^.BlkWrite(MenuInf, SizeOf(mnuRecord));
    end; { if }

  MenuRec_GetItems := (Menu_F^.FileSize DIV SizeOf(MnuRecord)) {- 01};
end; { func. MenuRec_GetItems }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure MenuRec_GetInfo(var Info1, Info2, Info3, Info4, Info5, Info6, Info7, Info8, Info9: List_Stringtype;
                          ItemNr: LongInt; HiLight: LongInt);
begin
  if ItemNr <> MenuRec_GetItems then
    begin
      Menu_F^.Seek(ItemNr * SizeOf(mnuRecord));
      Menu_F^.BlkRead(MenuInf, SizeOf(mnuRecord));

      if Menu_F^.IoResult > 0 then ;
    end; { if }

  Info1 := MenuInf.Display;

  If Info1[Length(Info1)]= ';' then
    Dec(Info1[0]);

  If Length(Info1)=26 then
    Info1[26] := #32;


  Info2[0] := #06;
  Info2[1] := #179;
  Info2[2] := '''';
  Info2[3] := UpCase(MenuInf.HotKey[1]);
  Info2[4] := '''';
  Info2[5] := #179;
  Info2[6] := #32;

  If MenuInf.HotKey[01] = #01 then                         { Auto Execute }
     begin
       Info2[2] := '*';
       Info2[3] := '*';
       Info2[4] := '*';
     end; { auto execute }

  If MenuInf.Typ=00 then
    begin
       Info2[2] := #32;
       Info2[3] := #32;
       Info2[4] := #32;
    end; { display only }

  if TotalMenuFunctions >= MenuInf.Typ then
    Info3 := StrPas(MenuDisplayRec[MenuInf.Typ].ShortName)
     else Info3 := '';

  Info4 := #179 + #32 + MenuInf.MiscData;

  if ItemNr=MenuRec_GetItems then
    begin
      Info1 := '';
      Info2 := '';
      Info3 := '';
      Info4 := '';
      Info5 := '';
      Info6 := '';
      Info7 := '';
      Info8 := '';
      Info9 := '';
    end; { if }
end; { proc. MenuRec_GetInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function MenuRec_Activate(HiLight: LongInt; HibarPos: LongInt): Longint;

procedure FillUpLightBar;
var LBarInfo    : LightBarRecord;
    ItemsToWrite: Longint;
    Counter     : Longint;
begin
  LoadLightBarDefaults(LBarInfo);

  if (LBar_F^.FileSize div SizeOf(LightBarRecord)) < HiLight then
    begin
      LBar_F^.Seek(LBar_F^.Filesize);

      ItemsToWrite := (HiLight - (LBar_F^.FileSize div SizeOf(LightBarrecord)));

      {$i-}
        for Counter := 01 to ItemsToWrite do
          LBar_F^.BlkWrite(LBarInfo, SizeOf(LightBarRecord));
      {$i+}
      if IOResult > 00 then ;
    end; { if }

end; { proc. FillUpLightBar }

var SaveMenu : MnuRecord;
    SaveScrn : Pointer;
    SaveLBar : LightBarRecord;
begin
  MenuRec_Activate := -1;

  SaveScreen(SaveScrn);

  Menu_F^.Seek((HiLight) * SizeOf(MnuRecord));
  LBar_F^.Seek((HiLight) * SizeOf(LightBarRecord));

  if HiLight=MenuRec_GetItems then
    begin
      LoadMenuDefaults(MenuInf);
      LoadLightBarDefaults(LBarInfo);
    end
     else begin
            Menu_F^.BlkRead(MenuInf, SizeOf(mnuRecord));
            if Menu_F^.IOResult > 00 then ;

            LBar_F^.BlkRead(LBarInfo, SizeOf(LightBarRecord));
            if LBar_F^.IoResult > 0 then
              LoadLightBarDefaults(LBarInfo);
          end; { if }

  SaveMenu := MenuInf;
  SaveLBar := LBarInfo;
  EditMenuRecord(MenuInf, LbarInfo);

  if (RecordsDifferent(SaveMenu, MenuInf, SizeOf(mnuRecord))) OR
      (RecordsDifferent(SaveLBar, LbarInfo, SizeOf(LightBarRecord))) then
      If DoSaveChanges('Save changes (Y/n) ? °', True, false) then
           begin
              Menu_F^.Seek( HiLight * SizeOf(mnuRecord));
              Menu_F^.BlkWrite(MenuInf, SizeOf(mnuRecord));
              if Menu_F^.IOresult > 00 then ;

              FillUpLightBar;

              LBar_F^.Seek( HiLight * SizeOf(LightBarRecord));
              LBar_F^.BlkWrite(LBarInfo, SizeOf(LightBarRecord));
              if LBar_F^.IoResult > 0 then ;
            end { SaveChanges }
              else begin
                     MenuInf := SaveMenu;
                     LBarInfo := SaveLBar;
                   end;

  RestoreScreen(SaveScrn);
end; { proc. MenuRec_Activate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function MenuRec_Seek(Str: String): LongInt;
var MenuInf: MnuRecord;
begin
  Menu_F^.Seek(SizeOf(mnuRecord));

  MenuRec_Seek := -1;

  While NOT Menu_F^.EOF do
   begin
     Menu_F^.BlkRead(MenuInf, SizeOf(mnuRecord));

     if SUpcase(Str) = SupCase(Copy(MenuInf.Display, 1, Length(Str))) then
       begin
         MenuRec_Seek := (Menu_F^.FilePos DIV SizeOf(mnuRecord)) - 01;
         BREAK;
       end; { Found one }

   end; { While NOT eof }

end; { func. MenuRec_Seek }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditMenuPrompt(var MenuInf: mnuRecord);
var Menu        : PullRecord;
    Choice      : Word;
    CH          : Char;
    Counter     : Byte;
    SaveDirect  : Boolean;
    NewStr      : String;
    BarStartItem: Byte;
    TmpByte     : Byte;
begin
 AllocMem(Menu.PullInf, SizeOf(Menu.PullInf^), 'PullRecord', 'EditMenuPrompt');
 InitPullMenu(Menu);
 AddPullItem(Menu.PullInf^[01], 'Menu prompt   ', 01, #00, '', 01);
 AddPullItem(Menu.PullInf^[02], 'Prompt colour ', 02, #00, '', 01);
 AddPullItem(Menu.PullInf^[03], 'Hilight colour', 03, #00, '', 01);
 AddPullItem(Menu.PullInf^[04], 'Supress       ', 04, #00, '', 01);
 AddPullItem(Menu.PullInf^[05], 'Global menu   ', 05, #00, '', 01);
 AddPullItem(Menu.PullInf^[06], 'Default lbar  ', 06, #00, '', 01);
 AddPullItem(Menu.PullInf^[07], 'Run All Items ', 07, #00, '', 01);

 Menu.Items   := 7;
 Menu.Width   := 79;
 Menu.Length  := 9;
 Menu.X       := 01;
 Menu.Y       := 04;
 Menu.HiLight := 01;
 Menu.AddSpace:= True;
 Menu.Title   := ' Edit Menu Settings ';
 Menu.PosArray[1].XInc := 04;
 Menu.PosArray[1].YDec := -2;

 ShowMenu(Menu, False);


 NewStr := '';
 if Pos('/NO', MenuInf.MiscData) > 00 then NewStr := NewStr + '/NO';
 if Pos('/NG', MenuInf.MiscData) > 00 then NewStr := NewStr + '/NG';
 if Pos('/RA', MenuInf.MiscData) > 00 then NewStr := NewStr + '/RA';
 BarStartItem := Succ(FVal(GetValue('/BS=', MenuInf.MiscData, false)));
 MenuInf.MiscData := NewStr;

 repeat
  SaveDirect := DirectScrnUpdate;
  DirectscrnUpdate := false;
  WriteAT(03, 06, mnuEditColor, MakeLen(MenuInf.Display, 75, space, false, false));
  WriteAT(25, 09, MakeAttr(MenuInf.ForeGround, MenuInf.BackGround), 'Prompt example');
  WriteAT(25, 10, MakeAttr(MenuInf.Typ, MenuInf.Security), 'Hilight example');
  WriteAT(25, 11, mnuMsgColor, Bool2Str( (Pos('/NO', MenuInf.MiscData) > 00)) + ' ');
  WriteAT(25, 12, mnuMsgColor, Bool2Str( (Pos('/NG', MenuInf.MiscData) = 00)) + ' ');
  WriteAT(25, 13, mnuMsgColor, FStr(BarStartItem));
  WriteAT(25, 14, mnuMsgColor, Bool2Str( (Pos('/RA', MenuInf.MiscData) > 00)) + ' ');
  DirectScrnUpdate := SaveDirect;

  Choice := DoPullMenu(Menu, CH, False, False);

  Case Choice of
    01 : MenuInf.Display := Edit(MenuInf.Display, 03, 06, mnuEditColor, 75, False, True, True);
    02 : Edit_Color(MakeAttr(MenuInf.ForeGround, MenuInf.BackGround), MenuInf.ForeGround, MenuInf.BackGround, Menu, True);
    03 : begin
           TmpByte := MenuInf.Security;
           Edit_Color(MakeAttr(MenuInf.Typ, MenuInf.Security), MenuInf.Typ, TmpByte, Menu, True);
           MenuInf.Security := TmpByte;
         end; { .. }
    04 : If Pos('/NO', MenuInf.MiscData)>00 then
           Replace('/NO', '', MenuInf.MiscData)
            else MenuInf.MiscData := MenuInf.MiscData + '/NO';
    05 : If Pos('/NG', MenuInf.MiscData)>00 then
           Replace('/NG', '', MenuInf.MiscData)
            else MenuInf.MiscData := MenuInf.MiscData + '/NG';
    06 : begin
           repeat
             EditByte(BarStartItem, 25, 13, mnuEditColor, true);
           until BarStartItem > 0;
         end; { if }
    07 : If Pos('/RA', MenuInf.MiscData)>00 then
           Replace('/RA', '', MenuInf.MiscData)
            else MenuInf.MiscData := MenuInf.MiscData + '/RA';
  end; { case }

 Until Choice=0;

 MenuInf.MiscData := MenuInf.MiscData + ' /BS='+FStr(Pred(BarStartItem));

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(Menu.PullInf^));
end; { proc. EditMenuPrompt }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure MessageBox(S: String);
var XLen      : Byte;
    XStart    : Byte;
    SaveScrn  : Pointer;
begin
  SaveScreen(SaveScrn);

  if Length(S) > 75 then S[0] := #76;
  XLen := 02 + Length(S) + 02;
  XStart := (80 div 2) - (XLen div 2);

  ShadFillBoxTitle(XStart, 10, (XStart + Xlen) - 01, 14, mnuBoxColor, mnuStyle, True, ' MESSAGE ');
  WriteAT(XStart + 02, 12, mnuMsgColor, S);
  GetInput;

  RestoreScreen(SaveScrn);
end; { proc. MessageBox }


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function MenuRec_KeyPress(CH: Char; HiLight: LongInt;
                          var HiBarPos, TopOfScrn: Longint;
                          var StartRange, EndRange: LongInt): LongInt;


var TempMenu_F: pFileObj;
    MenuInf   : MnuRecord;
    OldMenuInf: MnuRecord;
    SaveScrn  : Pointer;
    SavePath  : String;
    OldName   : String;
    GlobInf   : MnuRecord;
    GlobLBar  : LightBarRecord;
    NumRead   : Numreadtype;
begin
  MenuRec_KeyPress := -1;

  Case CH of
{ ALT-S }    #31 : begin
                     SaveScreen(SaveScrn);

                     ShadFillBoxTitle(06, 08, 70, 11, mnuBoxColor, mnuStyle, True, ' Save menu ');
                     WriteAT(08, 09, mnuNormColor, 'Name of menu to save:');
                     WriteAT(08, 10, mnuMsgColor, 'Press (Esc) to abort.');

                     OldName := TotalMenuName;
                     SavePath := JustPath(TotalMenuName);
                     TotalMenuName := JustName(NoExtension(TotalMenuName));
                     GetString(30, 09, mnuEditColor, TotalMenuName, [#32..#254], [#13, #27], 8, 8, True, False, True,false,0);
                     TotalMenuName := ForceBack(SavePath) + TotalMenuName + '.mnu';

                     if TotalMenuName <> OldName then
                      if FileExist(TotalMenuName) then
                        begin
                          MessageBox('Target file already exists!');
                          TotalMenuName := OldName;
                        end; { if }

                     if OldName <> TotalMenuName then
                       if NOT FileCopy(OldName, TotalMenuName, False, True) then
                           begin
                              MessageBox('Unable to create new file');
                              TotalMenuName := OldName;
                           end; { if }

                     if OldName <> TotalMenuName then
                       if NOT FileCopy(NoExtension(OldName)+'.mlb', NoExtension(TotalMenuName)+'.mlb', False, True) then
                           begin
                              MessageBox('Unable to create new file');
                              TotalMenuName := OldName;
                           end; { if }

                     RestoreScreen(SaveScrn);

                     WriteAT(01, 01, mnuBoxColor, #213 + Dup(mnuDblWide, 78) + #184);
                     WriteAT((80 - Length(#32+TotalMenuName+#32)), 1, mnuTitleColor, #32 + TotalMenuName + #32);

                     {$i-}
                       Dispose(Menu_F, Done);
                       New(Menu_F, Init);
                       Menu_F^.Assign(TotalMenuName);
                       Menu_F^.FileMode := ReadWriteMode + DenyNone;
                       MenU_F^.Open(1);

                       Dispose(LBar_F, Done);
                       New(LBar_F, Init);
                       Lbar_F^.Assign(NoExtension(TotalMenuName) + '.mlb');
                       Lbar_F^.FileMode := ReadWriteMode + DenyNone;
                       Lbar_F^.Open(1);
                     {$i+}
                     if IOResult>00 then ;
                   end; { Save menu under other name }
{ ALT-P }    #25 : begin
                     Menu_F^.Seek(0);
                     Menu_F^.BlkRead(MenuInf, SizeOf(mnuRecord));
                     if Menu_F^.IoResult > 00 then ;

                     OldMenuInf := MenuInf;

                     EditMenuPrompt(MenuInf);

                     If RecordsDifferent(MenuInf, OldMenuInf, SizeOf(MnuRecord)) then
                      If DoSaveChanges('Save changes (Y/n) ? °', True, false) then
                        begin
                          Menu_F^.Seek(0);
                          Menu_F^.BlkWrite(MenuInf, SizeOf(mnuRecord));
                          if Menu_F^.IoResult > 0 then;
                        end; { if }

                   end; { ALT-P(rompt) }
{ ALT-M }    #50 : begin;
                      PartClear(mnuMsgXPos, mnuMsgYpos - 1, 80, mnuMsgYPos - 1, 7, #32);

                      DoList(True, 02, 26, 06, 11, 34, 00, 00, 00, 00, 00,   { Length (info) coordinates }
                             01, 01, 80, mnuScrnLen - 03,
                             #32 + TotalMenuName + #32,
                             'Select move/copy insertion point',
                             {$IFDEF FPC}@{$ENDIF}MenuRec_GetInfo,
                             {$IFDEF FPC}@{$ENDIF}Copy_Activate,
                             {$IFDEF FPC}@{$ENDIF}MenuRec_Seek,
                             {$IFDEF FPC}@{$ENDIF}Copy_AbortCheck,
                             {$IFDEF FPC}@{$ENDIF}Copy_Keypress,
                             {$IFDEF FPC}@{$ENDIF}MenuRec_GetItems,
                             {$IFDEF FPC}@{$ENDIF}Menu_CreateNewMenu,
                             {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
                             False,
                             HiBarPos,
                             TopOfScrn,
                             false, true);

                        If StartRange=EndRange then
                          begin
                            StartRange := HiLight+01;
                            EndRange := HiLight+01;
                          end; { StartRange=EndRange }


                          if CopyPoint <> -1 then Inc(CopyPoint);
                          CopyRecord(StartRange, EndRange, True,
                                     SizeOf(MnuRecord), Menu_F,
                                     GlobInf, 1);

                          LoadLightBarDefaults(GlobLBar);
                          CopyRecord(StartRange, EndRange, True,
                                     SizeOf(LightBarRecord), LBar_F,
                                     GlobLBar, 1);

                          StartRange := -1;
                          EndRange := -1;
                      end; { move }

{ ALT-C }    #46 : begin;
                      PartClear(mnuMsgXPos, mnuMsgYpos - 1, 80, mnuMsgYPos - 1, 7, #32);

                      DoList(True, 02, 26, 06, 11, 34, 00, 00, 00, 00, 00,   { Length (info) coordinates }
                             01, 01, 80, mnuScrnLen - 03,
                             #32 + TotalMenuName + #32,
                             'Select move/copy insertion point',
                             {$IFDEF FPC}@{$ENDIF}MenuRec_GetInfo,
                             {$IFDEF FPC}@{$ENDIF}Copy_Activate,
                             {$IFDEF FPC}@{$ENDIF}MenuRec_Seek,
                             {$IFDEF FPC}@{$ENDIF}Copy_AbortCheck,
                             {$IFDEF FPC}@{$ENDIF}Copy_Keypress,
                             {$IFDEF FPC}@{$ENDIF}MenuRec_GetItems,
                             {$IFDEF FPC}@{$ENDIF}Menu_CreateNewMenu,
                             {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
                             False,
                             HiBarPos,
                             TopOfScrn,
                             false, true);

                        If StartRange=EndRange then
                          begin
                            StartRange := HiLight+01;
                            EndRange := HiLight+01;
                          end; { StartRange=EndRange }

                          if CopyPoint <> -1 then Inc(CopyPoint);
                          CopyRecord(StartRange, EndRange, false,
                                     SizeOf(MnuRecord), Menu_F,
                                     GlobInf, 1);

                          LoadLightBarDefaults(GlobLBar);
                          CopyRecord(StartRange, EndRange, false,
                                     SizeOf(LightBarRecord), LBar_F,
                                     GlobLBar, 1);

                          StartRange := -1;
                          EndRange := -1;
                      end; { copy }

{ Insert }   #82 : begin
                     New(TempMenu_F, Init);
                     TempMenu_F^.Assign('linkmnu.mnu');
                     TempMenu_F^.FileMode := ReadWriteMode + DenyAll;

                     if NOT TempMenu_F^.Create(1) then EXIT;

                     Menu_F^.Seek(0);

                     While NOT Menu_F^.EOF do
                       begin
                          Menu_F^.BlkRead(MenuInf, SizeOf(mnuRecord));
                          TempMenu_F^.BlkWrite(MenuInf, SizeOf(mnuRecord));

                          If (((Menu_F^.FilePos div SizeOf(mnuRecord))) = HiLight) then
                            begin
                              LoadMenuDefaults(MenuInf);

                              TempMenu_F^.BlkWrite(MenuInf, SizeOf(mnuRecord));
                              if TempMenu_F^.IoResult > 0 then ;
                            end; { if }
                       end; { while }

                     Menu_F^.Close;
                     Menu_F^.Erase;

                     TempMenu_F^.Close;
                     RenameObjWithDrive(TempMenu_F, TotalMenuName);

                     Dispose(TempMenu_F, Done);
                     Menu_F^.Open(1);

                     {--------------------------------------------}
                     New(TempMenu_F, Init);
                     TempMenu_F^.Assign('linkmnu.mlb');
                     TempMenu_F^.FileMode := ReadWriteMode + DenyAll;

                     if NOT TempMenu_F^.Create(1) then EXIT;

                     LBar_F^.Seek(0);

                     While NOT LBar_F^.EOF do
                       begin
                          LBar_F^.BlkRead(LBarInfo, SizeOf(LightBarRecord));
                          TempMenu_F^.BlkWrite(LBarInfo, SizeOf(LightBarRecord));

                          If (((LBar_F^.FilePos div SizeOf(LightBarRecord))) = HiLight) then
                            begin
                              LoadLightBarDefaults(LBarInfo);

                              TempMenu_F^.BlkWrite(LBarInfo, SizeOf(LightBarRecord));
                              if TempMenu_F^.IoResult > 00 then ;
                            end; { if }
                       end; { while }

                     LBar_F^.Close;
                     LBar_F^.Erase;

                     TempMenu_F^.Close;
                     RenameObjWithDrive(TempMenu_F, NoExtension(TotalMenuName) + '.mlb');

                     Dispose(TempMenu_F, Done);
                     LBar_F^.Open(1);
                   end; { insert }

{ Delete }   #83 : if DoSaveChanges('Confirm delete (y/N) °', false, false) then
                    begin
                     New(TempMenu_F, Init);
                     TempMenu_F^.Assign('linkmnu.mnu');
                     TempMenu_F^.FileMode := ReadWriteMode + DenyAll;

                     if NOT TempMenu_F^.Create(1) then EXIT;

                     Menu_F^.Seek(0);

                     While NOT Menu_F^.EOF do
                       begin
                         Menu_F^.BlkRead(MenuInf, SizeOf(mnuRecord));
                         if Menu_F^.IoResult > 0 then ;

                         if (((Menu_F^.FilePos) div SizeOf(mnuRecord))-1) <> HiLight then
                           TempMenu_F^.BlkWrite(MenuInf, SizeOf(mnuRecord));
                         if TempMenu_F^.IoResult > 0 then;
                       end; { while }

                     Menu_F^.Close;
                     Menu_F^.Erase;

                     TempMenu_F^.Close;
                     RenameObjWithDrive(TempMenu_F, TotalMenuName);

                     Dispose(TempMenu_F, Done);
                     Menu_F^.Open(1);

                     {-----------------------------------------------------}
                     New(TempMenu_F, Init);
                     TempMenu_F^.Assign('linkmnu.mlb');
                     TempMenu_F^.FileMode := ReadWriteMode + DenyAll;

                     if NOT TempMenu_F^.Create(1) then EXIT;

                     LBar_F^.Seek(0);

                     While NOT LBar_F^.EOF do
                       begin
                          LBar_F^.BlkRead(LBarInfo, SizeOf(LightBarRecord));

                          if (((LBar_F^.FilePos div SizeOf(LightBarRecord)) - 1) <> HiLight) then
                            TempMenu_F^.BlkWrite(LBarInfo, SizeOf(LightBarRecord));
                       end; { while }

                     LBar_F^.Close;
                     LBar_F^.Erase;

                     TempMenu_F^.Close;
                     RenameObjWithDrive(TempMenu_F, NoExtension(TotalMenuName) + '.mlb');

                     Dispose(TempMenu_F, Done);
                     LBar_F^.Open(1);
                   end; { delete }
  End; { case }
end; { func. MenuRec_KeyPress }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function PickMenu_Seek(Str: String): LongInt;
begin
  PickMenu_Seek := -1;
end; { func. PickMenu_Seek }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function PickMenu_GetItems: LongInt;
begin
  PickMenu_GetItems := TotalMenuItems;
end; { func. PickMenu_GetItems }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function PickMenu_CreateNewFile: Boolean;
begin
  PickMenu_CreateNewFile := True;
end; { proc. Pickmenu_CreateNewFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure PickMenu_GetInfo(var Info1, Info2, Info3, Info4, Info5, Info6, Info7, Info8, Info9: List_StringType;
                          ItemNr: LongInt; HiLight: LongInt);
begin
  Info1 := PickMenuItems^[ItemNr];
end; { proc. PickMenu_GetInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function ListMenus(MnuListType: MnuListTypeRec; CurType: Byte): Byte;
var Counter   : Byte;
    MenuTitle : String;

    TempFile: Text;
    StartItem: Integer;
begin
  MenuTitle := '';
  {$IFDEF MSDOS}
  if MemAvail < SizeOf(PickMenuItemsRec) then
     EXIT;
  {$ENDIF}

  AllocMem(PickMenuItems, SizeOf(PickMenuItems^), 'PickMenuItems', 'ListMenus');
  FillChar(PickMenuItems^, SizeOf(PickMenuItems^), #00);
  TotalMenuItems := 00;
  StartItem := 01;

  Case MnuListType of
    mnuDisplay   : ;
    mnuMnuFuncs  : MenuTitle := ' Menu functions ';
    mnuTxtFiles  : MenuTitle := ' Display text-files ';
    mnuGeneral   : MenuTitle := ' General system options ';
    mnuMsgBase   : MenuTitle := ' Message-base commands ';
    mnuFileArea  : MenuTitle := ' File-area commands ';
    mnuExternal  : MenuTitle := ' External - door, exits ';
    mnuChange    : MenuTitle := ' Change user options ';
    mnuInternet  : MenuTitle := ' Internet connectivity ';
  End; { case }

  For Counter := 01 to TotalMenuFunctions do
   begin

    If MenuDisplayRec[Counter].MenuType=mnuListType then
      begin
         If Counter=CurType then
             Startitem := TotalMenuItems + 01;

         Inc(TotalMenuItems);
         PickMenuItems^[TotalMenuItems] := StUtils.MakeLen(FStr(Counter), 3, Space, True, false) + #32 +
                                          StrPas(MenuDisplayRec[Counter].FullName);

         PickMenuItems^[TotalMenuItems] := StrPas(MenuDisplayRec[Counter].FullName);
         PickMenuItems^[TotalMenuItems] := StUtils.MakeLen(FStr(Counter), 3, Space, true, false) + #32 +
           PickMenuItems^[TotalMenuItems];
      end; { if }


   end; { for }

 DoList(True, 38,
        39, 0, 0, 0, 0, 0, 0, 0, 0,
        37, 13, 78, 20, MenuTitle,
        '',
        {$IFDEF FPC}@{$ENDIF}PickMenu_GetInfo,
        {$IFDEF FPC}@{$ENDIF}Copy_Activate,
        {$IFDEF FPC}@{$ENDIF}PickMenu_Seek,
        {$IFDEF FPC}@{$ENDIF}Copy_AbortCheck,
        {$IFDEF FPC}@{$ENDIF}Copy_KeyPress,
        {$IFDEF FPC}@{$ENDIF}PickMenu_GetItems,
        {$IFDEF FPC}@{$ENDIF}PickMenu_CreateNewFile,
        {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
        False, StartItem, 00, True, True);

 If CopyPoint < 00 then ListMenus := 00
   else begin         { Well, not all coding is too "school-book" alike :-)) }
           ListMenus := FVal(Trim(Copy(PickMenuItems^[CopyPoint], 1, 3)));
        end;

 ReleaseMem(PickMenuItems, SizeOf(PickMenuItems^));
end; { func. ListMenus }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditMenuAction(var Typ: Byte);
Var Menu    : PullRecord;
    Choice  : Word;
    CH      : Char;
    Counter : Byte;
    TempMenu: Integer;
    TempScrn: Pointer;
begin;
 AllocMem(Menu.PullInf, SizeOf(Menu.PullInf^), 'Menu.PullInf', 'EditMenuAction');
 InitPullMenu(Menu);
 AddPullItem(Menu.PullInf^[01], ' Display only (no command)   ', 01, #00, '', 1);
 AddPullItem(Menu.PullInf^[02], ' Menu functions              ', 02, #00, '', 1);
 AddPullItem(Menu.PullInf^[03], ' Display text-files          ', 03, #00, '', 1);
 AddPullItem(Menu.PullInf^[04], ' General system options      ', 04, #00, '', 1);
 AddPullItem(Menu.PullInf^[05], ' Message-base commands       ', 05, #00, '', 1);
 AddPullItem(Menu.PullInf^[06], ' File-area commands          ', 06, #00, '', 1);
 AddPullItem(Menu.PullInf^[07], ' External - door, exits      ', 07, #00, '', 1);
 AddPullItem(Menu.PullInf^[08], ' Change user options         ', 08, #00, '', 1);
 AddPullItem(Menu.PullInf^[09], ' Internet connectivity       ', 09, #00, '', 1);

 Menu.Items   := 9;
 Menu.Width   := 30;
 Menu.Length  := 9;
 Menu.X       := 34;
 Menu.Y       := 10;
 Menu.HiLight := 01;
 Menu.AddSpace:= False;
 Menu.Title   := ' Menu Action Groups ';
 Menu.PosArray[1].XInc := 00;
 Menu.PosArray[1].YDec := 00;

 ShowMenu(Menu, False);

 Case MenuDisplayRec[Typ].MenuType of
    MnuDisplay  : Menu.HiLight := 01;
    MnuMnuFuncs : Menu.HiLight := 02;
    MnuTxtFiles : Menu.HiLight := 03;
    MnuGeneral  : Menu.HiLight := 04;
    MnuMsgBase  : Menu.HiLight := 05;
    MnuFileArea : Menu.HiLight := 06;
    MnuExternal : Menu.HiLight := 07;
    MnuChange   : Menu.HiLight := 08;
    mnuInternet : Menu.HiLight := 09;
 end; { case }

 If Menu.HiLight <> 01 then
  begin;
     ExtraWaiting := True;
     ExtraKey     := #13;
  end; { if }

 Repeat;
  Choice := DoPullMenu(Menu, CH, False, False);

  TempMenu := -1;                                            { Escape pressed }

  SaveScreen(TempScrn);

  Case Choice of
    01 : TempMenu := 00;
    02 : TempMenu := ListMenus(MnuMnuFuncs, Typ);
    03 : TempMenu := ListMenus(MnuTxtFiles, Typ);
    04 : TempMenu := ListMenus(MnuGeneral, Typ);
    05 : TempMenu := ListMenus(MnuMsgBase, Typ);
    06 : TempMenu := ListMenus(MnuFileArea, Typ);
    07 : TempMenu := ListMenus(MnuExternal, Typ);
    08 : TempMenu := ListMenus(MnuChange, Typ);
    09 : TempMenu := ListMenus(MnuInternet, Typ);
  End; { Case Choice }
  RestoreScreen(TempScrn);

 Until (Choice=0) OR ( (TempMenu>00) OR (Choice=01) );

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(Menu.PullInf^));

 If Choice>00 then Typ := TempMenu;
end; { proc. EditMenuAction }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  Term2Str(B: Byte): String;
var TempStr: String;
begin
  TempStr := '';

  If ReadBit(B, 00) then TempStr := TempStr + ' ANSI';
  If ReadBit(B, 01) then TempStr := TempStr + ' AVT';
  If ReadBit(B, 02) then TempStr := TempStr + ' RIP';

  If TempStr='' then Tempstr := 'Any';

  Term2Str := MakeLen(TrimLeft(TempStr), 12, space, false, false);
end; { func. Term2Str }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


procedure EditMenuRecord(var MenuInf: MnuRecord; var LBar: LightBarRecord);

Procedure EditTermAttrib(var B: Byte);
Var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    Counter   : Byte;
    SaveDirect: Boolean;
begin;
 AllocMem(Menu.PullInf, SizeOf(Menu.PullInf^), 'Menu.PullInf', 'EditTermAttrib');
 InitPullMenu(Menu);
 AddPullItem(Menu.PullInf^[01], 'ANSI', 01, #00, 'Press ENTER to toggle setting', 1);
 AddPullItem(Menu.PullInf^[02], 'AVT ', 02, #00, 'Press ENTER to toggle setting', 1);
 AddPullItem(Menu.PullInf^[03], 'RIP ', 03, #00, 'Press ENTER to toggle setting', 1);

 Menu.Items   := 3;
 Menu.Width   := 9;
 Menu.Length  := 3;
 Menu.X       := 66;
 Menu.Y       := 09;
 Menu.HiLight := 01;
 Menu.AddSpace:= True;
 Menu.Title   := ' Select ';
 Menu.PosArray[1].XInc := 00;
 Menu.PosArray[1].YDec := 00;

 ShowMenu(Menu, False);

 Repeat;
  SaveDirect := DirectScrnUpdate;
  DirectscrnUpdate := false;
  WriteAT(73, 11, mnuNormColor, '-');
  WriteAT(73, 12, mnuNormColor, '-');
  WriteAT(73, 13, mnuNormColor, '-');

  If ReadBit(B, 00) then WriteAT(73, 11, mnuNormColor, 'X');
  If ReadBit(B, 01) then WriteAT(73, 12, mnuNormColor, 'X');
  If ReadBit(B, 02) then WriteAT(73, 13, mnuNormColor, 'X');
  DirectScrnUpdate := SaveDirect;

  Choice := DoPullMenu(Menu, CH, False, False);

  If Choice in [1..3] then
    If ReadBit(B, Choice-1) then ClearBit(B, Choice-1) else
      SetBit(B, Choice-1);

 Until Choice=0;

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(Menu.PullInf^));
end; { proc. EditTermAttrib }


procedure EditTimes(var Start, Stop: Array of SmallWord);
var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    Counter   : Byte;
    SaveDirect: Boolean;
begin;
 AllocMem(Menu.PullInf, SizeOf(Menu.PullInf^), 'Menu.PullInf^', 'EditTimes');
 InitPullMenu(Menu);
 AddPullItem(Menu.PullInf^[01], 'Sunday   ', 01, #00, 'Time period during which option is available', 1);
 AddPullItem(Menu.PullInf^[02], 'Monday   ', 02, #00, 'Time period during which option is available', 1);
 AddPullItem(Menu.PullInf^[03], 'Tuesday  ', 03, #00, 'Time period during which option is available', 1);
 AddPullItem(Menu.PullInf^[04], 'Wednesday', 04, #00, 'Time period during which option is available', 1);
 AddPullItem(Menu.PullInf^[05], 'Thursday ', 05, #00, 'Time period during which option is available', 1);
 AddPullItem(Menu.PullInf^[06], 'Friday   ', 06, #00, 'Time period during which option is available', 1);
 AddPullItem(Menu.PullInf^[07], 'Saturday ', 07, #00, 'Time period during which option is available', 1);

 Menu.Items   := 7;
 Menu.Width   := 24;
 Menu.Length  := 7;
 Menu.X       := 38;
 Menu.Y       := 03;
 Menu.HiLight := 01;
 Menu.AddSpace:= True;
 Menu.Title   := ' Select day/time ';
 Menu.PosArray[1].XInc := 00;
 Menu.PosArray[1].YDec := 00;

 ShowMenu(Menu, False);

 Repeat;
  SaveDirect := DirectScrnUpdate;
  DirectscrnUpdate := false;

  For Counter := 00 to 06 do
    WriteAT(50, 05 + Counter, mnuNormColor, Word2Time(Start[Counter]) + #32 + Word2Time(Stop[Counter]));

  DirectScrnUpdate := SaveDirect;

  Choice := DoPullMenu(Menu, CH, False, False);

  If Choice in [01..07] then
    begin
      EditTimeWord(Start[Choice-1], 50, 04 + Choice, True);
      EditTimeWord(Stop[Choice-1], 56, 04 + Choice, True);
    end; { if }

 Until Choice=0;

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(Menu.PullInf^));
end; { proc. EditTimes }

procedure EditLongWord (var S : Longint; X,Y,Attr:Byte; EscAbort: Boolean);
var Temp: SmallWord;
begin
  Temp := s;
  EditWord(Temp, X,y,Attr,EscAbort);
  S := Temp;
end; { proc. EditLongWord }

procedure EditLightBarOptions(var LBar: LightBarRecord);
Var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    Counter   : Byte;
    SaveDirect: Boolean;
begin;
 AllocMem(Menu.PullInf, SizeOf(Menu.PullInf^), 'Menu.PullInf^', 'EditLightbarOptions');
 InitPullMenu(Menu);

 AddPullItem(Menu.PullInf^[01], 'Position ', 01, #00, 'X and Y coordinates for this lightbar', 1);
 AddPullItem(Menu.PullInf^[02], 'LowItem  ', 02, #00, 'To display when the item is not active', 1);
 AddPullItem(Menu.PullInf^[03], 'HighItem ', 03, #00, 'Display when the item is hilighted', 1);
 AddPullItem(Menu.PullInf^[04], 'Enabled  ', 04, #00, 'Enable/Disable this option', 1);

 Menu.Items   := 4;
 Menu.Length  := 4;
 Menu.Width   := 70;
 Menu.X       := 04;
 Menu.Y       := 03;
 Menu.HiLight := 01;
 Menu.AddSpace:= True;
 Menu.Title   := ' Lightbar info ';
 Menu.PosArray[1].XInc := 00;
 Menu.PosArray[1].YDec := 00;

 ShowMenu(Menu, False);

 repeat;
   SaveDirect := DirectScrnUpdate;
   DirectscrnUpdate := false;

   WriteAT(16, 05, mnuNormColor, FStr(LBar.LightX));
   WriteAT(19, 05, mnuNormColor, '/');
   WriteAT(21, 05, mnuNormColor, FStr(LBar.LightY));
   WriteAT(16, 06, mnuNormColor, Copy(LBar.LowItem, 1, 57));
   WriteAT(16, 07, mnuNormColor, Copy(LBar.SelectItem, 1, 57));
   WriteAT(16, 08, mnuNormColor, Bit2Str(LBar.Attrib, 0));

   DirectScrnUpdate := SaveDirect;

   Choice := DoPullMenu(Menu, CH, False, False);

   Case Choice of
     01 : begin
            repeat
              EditByte(LBar.LightX, 16, 05, mnuEditColor, True);

              if (NOT (LBar.LightX in [1..80])) then Write(^G);
            until (LBar.LightX in [1..80]);

            WriteAT(16, 05, mnuNormColor, FStr(LBar.LightX));
            WriteAT(19, 05, mnuNormColor, '/');

            if rdLastKey <> #27 then
             begin
               repeat
                 EditByte(LBar.LightY, 21, 05, mnuEditColor, True);

                 if (NOT (LBar.LightY in [1..50])) then Write(^G);
               until (LBar.LightY in [1..50]);
             end; { if }
          end; { Coordinates }
     02 : begin
            GetString(16, 06, mnuEditColor, LBar.LowItem, [#32..#254], [#13, #27], 135, 57, false, true, True,false, 0);

            if LBar.LowItem <> '' then
             if LBar.SelectItem = '' then LBar.SelectItem := LBar.LowItem;
          end; { lowitem }
     03 : GetString(16, 07, mnuEditColor, LBar.SelectItem, [#32..#254], [#13, #27], 135, 57, false, true, True,false, 0);
     04 : LBar.Attrib := Edit_Bit(LBar.Attrib, 0, 16, 08, mnuNormColor);
   end; { case }

  until Choice=0;

  RemoveMenu(Menu);
  ReleaseMem(Menu.PullInf, SizeOf(Menu.PullInf^));
end; { proc. EditLightBarOptions }

var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    SaveDirect: Boolean;
    TempStr   : String;
begin
 AllocMem(Menu.PullInf, SizeOf(Menu.PullInf^), 'Menu.PullInf^', 'EditMenuRecord');
 InitPullMenu(Menu);

 AddPullItem(Menu.PullInf^[01], 'Action  ',   01, 'A', 'What will happen when item is selected', 01);
 AddPullItem(Menu.PullInf^[02], 'Display ',   02, 'D', 'Text which is displayed for this option before selection', 01);
 AddPullItem(Menu.PullInf^[03], 'OptData ',   03, 'O', 'Optional data', 01);
 AddPullItem(Menu.PullInf^[04], 'HotKey  ',   04, 'H', 'Key to activate this option', 01);
 AddPullItem(Menu.PullInf^[05], 'AutoExec',   05, 'U', '"Yes" means that this option will be automatically executed', 01);
 AddPullItem(Menu.PullInf^[06], 'Colour  ',   06, 'C', 'Colour used to display this option', 01);
 AddPullItem(Menu.PullInf^[07], '',           07, #00, '', 01);
 AddPullItem(Menu.PullInf^[08], 'MinSec  ',   08, 'M', 'Minimum security level required to activate this option', 01);
 AddPullItem(Menu.PullInf^[09], 'MaxSec  ',   09, 'X', 'Maximum security level allowed to activate this option (0=any)', 01);
 AddPullItem(Menu.PullInf^[10], 'A flags ',   10, '1', 'A flags setting required to activate this option', 01);
 AddPullItem(Menu.PullInf^[11], 'B flags ',   11, '2', 'B flags setting required to activate this option', 01);
 AddPullItem(Menu.PullInf^[12], 'C flags ',   12, '3', 'C flags setting required to activate this option', 01);
 AddPullItem(Menu.PullInf^[13], 'D flags ',   13, '4', 'D flags setting required to activate this option', 01);
 AddPullItem(Menu.PullInf^[14], 'Lightbar',   14, 'L', 'Set lightbar preferences for this menu-item', 01);

 AddPullItem(Menu.PullInf^[15], 'TimeUsed',   15, 'T', 'Minimum amount of time limit that must be '+
                                                       'used before option is selectable', 02);
 AddPullItem(Menu.PullInf^[16], 'TimeLeft',   16, 'I', 'Minimum amount of time left required to select this option', 02);
 AddPullItem(Menu.PullInf^[17], 'Age     ',   17, 'G', 'Minimum age required for this option', 02);
 AddPullItem(Menu.PullInf^[18], 'MinSpeed',   18, #00, 'Minimum connect speed required to make this option selectable', 02);
 AddPullItem(Menu.PullInf^[19], 'MaxSpeed',   19, #00, 'Maximum connect speed allowed to make this '+
                                                       'option selectable (0=any)', 02);
 AddPullItem(Menu.PullInf^[20], 'Credit  ',   20, #00, 'Credit required to select this option, '+
                                                       '0=don''t care (not deducted)', 02);

 AddPullItem(Menu.PullInf^[21], 'FlatCost',   21, #00, 'Number of credits deducted from account when option is selected', 03);
 AddPullItem(Menu.PullInf^[22], 'TimeCost',   22, #00, 'Number of credits deducted from account per '+
                                                       'minute until menu is redisplayed', 03);
 AddPullItem(Menu.PullInf^[23], 'Terminal',   23, #00, 'User must have ONE OR MORE of these emulations'+
                                                       ' enabled to select option', 03);
 AddPullItem(Menu.PullInf^[24], 'Nodes   ',   24, #00, 'Nodes on which this option is available', 03);
 AddPullItem(Menu.PullInf^[25], 'Groups  ',   25, #00, 'User groups to which this option is available', 03);
 AddPullItem(Menu.PullInf^[26], 'DayTimes',   26, #00, 'Days and times this option is available', 03);

 Menu.PullInf^[07].mnuSelect := False;                                { Space }

 Menu.Items   := 26;
 Menu.Width   := 79;
 Menu.Length  := 22;
 Menu.X       := 1;
 Menu.Y       := 1;
 Menu.HiLight := 01;
 Menu.AddSpace:= False;
 Menu.Title   := ' Edit Menu Item ';
 Menu.PosArray[1].XInc := 01;
 Menu.PosArray[1].YDec := -07;
 Menu.PosArray[2].XInc := 29;
 Menu.PosArray[2].YDec := 0; {-1}
 Menu.PosArray[3].XInc := 57;
 Menu.PosArray[3].YDec := 6;

 ShowMenu(Menu, False);

 SaveDirect := DirectScrnUpdate;
 DirectscrnUpdate := false;
 WriteAT(03, 03, mnuNormColor, '         1         2         3         4         5         6         7     ');
 WriteAT(03, 04, mnuNormColor, '123456789012345678901234567890123456789012345678901234567890123456789012345');

 Repeat;

  With MenuInf do
    begin;
       WriteAT(03, 05, mnuEditColor, MakeLen(Display, 75, space, false, false));
       WriteAT(03, 07, mnuEditColor, MakeLen(MiscData, 75, space, false, false));

       { temporary fix }
       if Typ = 102 then Typ := 12;

       WriteAT(12, 09, mnuNormColor, MakeLen(StrPas(MenuDisplayRec[Typ].FullName) + #32 +
                                     '(Type '+FStr(Typ)+')', 68, space, false, false));

       TempStr  := StrPas(MenuDisplayRec[Typ].FullName);
       TempStr  := TempStr + #32 + '(Type ' + FStr(Typ) + ')';
       TempStr  := MakeLen(TempStr, 68, Space, False, False);
       WriteAT(12, 09, mnuNormColor, TempStr);

       If HotKey[1] <> #01 then
         WriteAT(12, 12, mnuNormColor, Hotkey);
       WriteAT(12, 13, mnuNormColor, MakeLen(Bool2Str(HotKey[1] = #01), 3, Space, false, false));

       WriteAT(12, 14, MakeAttr(Foreground, background), 'Colour example');

       WriteAT(12, 16, mnuNormColor, FStr(Security));
       WriteAT(12, 17, mnuNormColor, FStr(MaxSec));
       WriteAT(12, 18, mnuNormColor, Byte2FlagsOff(Flags[1], NotFlagsMask[1]));
       WriteAT(12, 19, mnuNormColor, Byte2FlagsOff(Flags[2], NotFlagsMask[2]));
       WriteAT(12, 20, mnuNormColor, Byte2FlagsOff(Flags[3], NotFlagsMask[3]));
       WriteAT(12, 21, mnuNormColor, Byte2FlagsOff(Flags[4], NotFlagsMask[4]));

       WriteAT(40, 16, mnuNormColor, FStr(TimeUsed));
       WriteAT(40, 17, mnuNormColor, FStr(TimeLeft));
       WriteAT(40, 18, mnuNormColor, FStr(Age));
       WriteAT(40, 19, mnuNormColor, FStr(MinSpeed));
       WriteAT(40, 20, mnuNormColor, FStr(MaxSpeed));
       WriteAT(40, 21, mnuNormColor, FStr(Credit));

       WriteAT(68, 16, mnuNormColor, FStr(OptionCost));
       WriteAT(68, 17, mnuNormColor, FStr(PerMinCost));
       WriteAT(68, 18, mnuNormColor, Term2Str(TermAttrib));
    end; { with }

  DirectScrnUpdate := SaveDirect;
  Choice := DoPullMenu(Menu, CH, False, False);

  With MenuInf do
    Case Choice of
      01 : EditMenuAction(Typ);
      02 : GetString(03, 05, mnuEditColor, Display, [#32..#254], [#13, #27], 134, 75, False, True, True, false, 0);
      03 : GetString(03, 07, mnuEditColor, MiscData, [#32..#254], [#13, #27], 250, 75, False, False, True, false, 0);
      04 : GetString(12, 12, mnuEditColor, HotKey, [#32..#254], [#13, #27], 01, 01, False, False, True, false, 0);
      05 : If HotKey[1] = #01 then
              begin
                HotKey := ' ';
              end
               else begin
                      HotKey[1] := #01;
                      HotKey[0] := #01;
                    end;
      06 : Edit_Color(MakeAttr(Foreground, BackGround), ForeGround, BackGround, Menu, True);

      08 : EditWord(Security, 12, 16, mnuEditColor, True);
      09 : EditWord(MaxSec, 12, 17, mnuEditColor, True);
      10 : EditFlags(Flags[1], NotFlagsMask[1], True, 60, 03, Menu);
      11 : EditFlags(Flags[2], NotFlagsMask[2], True, 60, 03, Menu);
      12 : EditFlags(Flags[3], NotFlagsMask[3], True, 60, 03, Menu);
      13 : EditFlags(Flags[4], NotFlagsMask[4], True, 60, 03, Menu);
      14 : EditLightbarOptions(LBar);

      15 : EditWord(TimeUsed, 40, 16, mnuEditColor, True);
      16 : EditWord(TimeLeft, 40, 17, mnuEditColor, True);
      17 : EditByte(Age,      40, 18, mnuEditColor, True);
      18 : EditLongWord(MinSpeed, 40, 19, mnuEditColor, True);
      19 : EditLongWord(MaxSpeed, 40, 20, mnuEditColor, True);
      20 : EditLongWord(Credit,   40, 21, mnuEditColor, True);

      21 : EditLongWord(OptionCost, 68, 16, mnuEditColor, True);
      22 : EditLongWord(PerMinCost, 68, 17, mnuEditColor, True);
      23 : EditTermAttrib(TermAttrib);
      24 : EditBitArray32(' Select nodes ', Node);
      25 : EditBitArray32(' Select groups ', Group);
      26 : EditTimes(StartTime, StopTime);
    end; { case }

 Until Choice=0;

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(Menu.PullInf^));
end; { proc. EditMenuRecord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditMenu(MenuPath: String; var MenuName: String);
begin
  if MenuName='' then MenuName := 'noname';

  TotalMenuName := ForceBack(MenuPath) + Slowcase(NoExtension(MenuName) + '.mnu');

  If GetFileSize(ForceBack(MenuPath) + NoExtension(MenuName) +  '.mnu', SizeOf(mnuRecord))<2 then
    Menu_CreateNewMenu;

  If GetFileSize(ForceBack(MenuPath) + NoExtension(MenuName) +  '.mlb', SizeOf(LightbarRecord))<2 then
    Menu_CreateNewLbar;

  If GetFileSize(ForceBack(MenuPath) + NoExtension(MenuName) +  '.mnu', SizeOf(mnuRecord))<2 then
    EXIT;

  New(Menu_F, Init);
  New(LBar_F, Init);

  Menu_F^.Assign(ForceBack(MenuPath) + Slowcase(NoExtension(MenuName) + '.mnu'));
  Menu_F^.FileMode := ReadWriteMode + DenyWrite;

  LBar_F^.Assign(ForceBack(MenuPath) + Slowcase(NoExtension(MenuName) + '.mlb'));
  LBar_F^.FileMode := ReadWriteMode + DenyWrite;

  if NOT Menu_F^.Open(1) then
    begin
       MessageBox('Unable to open '+NoExtension(MenuName)+', already in use?');

       Dispose(Menu_F, Done);
       Dispose(LBar_F, Done);

       EXIT;
    end; { if }

  if NOT LBar_F^.Open(1) then
    begin
       MessageBox('Unable to open '+NoExtension(MenuName)+', already in use?');

       Dispose(Menu_F, Done);
       Dispose(LBar_F, Done);

       EXIT;
    end; { if }

  DoList(True, 02, 26, 06, 11, 34, 00, 00, 00, 00, 00,   { Length (info) coordinates }
         01, 01, 80, mnuScrnLen - 03,
         #32 + ForceBack(MenuPath) + NoExtension(MenuName) + '.mnu' + #32,
         'ALT-M Move   ALT-C Copy' + #13 +
         'Enter-Edit   Ins-Insert   Del-Delete   Esc-Exit   ALT-P Prompt   ALT-S Save',
         {$IFDEF FPC}@{$ENDIF}MenuRec_GetInfo,
         {$IFDEF FPC}@{$ENDIF}MenuRec_Activate,
         {$IFDEF FPC}@{$ENDIF}MenuRec_Seek,
         {$IFDEF FPC}@{$ENDIF}General_AbortCheck,
         {$IFDEF FPC}@{$ENDIF}MenuRec_Keypress,
         {$IFDEF FPC}@{$ENDIF}MenuRec_GetItems,
         {$IFDEF FPC}@{$ENDIF}Menu_CreateNewMenu,
         {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
         False,
         00, 00,
         False, False);

  Dispose(Menu_F, Done);
  Dispose(LBar_F, Done);
end; { proc. EditMenu }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
