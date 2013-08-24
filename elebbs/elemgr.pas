program EleMGR;
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
{$DEFINE WITH_SWAPPER}
{$IFNDEF MSDOS}
  {$UNDEF WITH_SWAPPER}
{$ENDIF}
{$IFNDEF WIN32}
 {$IFNDEF VirtualPascal}
   {$M 60000, 0, 650535}
 {$ELSE}
   {$M 60000, 650535}
 {$ENDIF}
{$ELSE}
  {$IFNDEF VirtualPascal}
  {$APPTYPE CONSOLE}
 {$ENDIF}
{$ENDIF}
(*
**
** EleMGR, RemoteAccess v2.50 compatible user and filebase manager (for EleBBS)
**
** Copyright (c) 1996, 97 by Maarten Bekers
**
** Created : 01-Apr-1997
** Last update : 04-Jun-1998
**
**
*)
{$IFNDEF USINGMGR}
  Even bovenstaande boolean defineeren voor EditUser
{$ENDIF}

uses Crt,
     Scrnu,
      IoRes_Un,
       RecDif,
{$Ifndef WinGUI}
       Dos,
      {$IFNDEF VirtualPascal}
       {$IFNDEF FPC}
         TpEnv,
          Memory,
       {$ENDIF}
      {$ENDIF}
{$else}
      {$IFNDEF VirtualPascal}
      {$ELSE}
        Crt,
      {$ENDIF}
         SysUtils,
{$Endif}
         Global,
            GenCFG,
             Area_Lst,
              FileMgr,
               EditUser,
               {$IFDEF MSDOS}
                 Exec,
               {$ENDIF}
                  CfgRec,
                   MemMan,
                    Readcfg,
                     BBSkey,
                       StrEdit,
                        ListSys,
                         BitWise,
                          MenuSys,
                           Cases,
                            LongStr,
                             Colors,
                              GenFile,
                               WinTitle,
                                FileObj,
                                 Crc_unit,
                                  FileRout,
                                   ObjDec,
                                    Sysvars,
                                     RemSup;


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Const NowPagedown : boolean = false;

Const CacheStart: Longint = -1;
      CacheEnd  : Longint = -1;
      SearchStr : String = '';

Type UserArray = Array[1..20] of UsersRecord;

var UserInf        : ^UserArray;
    User_F         : pFileObj;
    UserEditObj    : pUserEditObj;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowHeader;
begin
  New(LineCfg);
  New(GlobalCfg);
  InitBasicGlobalVars;
  InitBasicLineVars(LineCfg^);

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

  if NOT MemMan.AllocMem(LineCfg^.Exitinfo, SizeOf(ExitInfoRecord), 'ExitinfoRecord', 'ShowHeader') then
    begin
      WriteLn;
      Writeln('Sorry you need at least ', SizeOf(ExitInfoRecord) div 1024, 'k more memory to run');
      Halt(255);
    end; { Not enough memory }

  ReadConfigRa;
  DirectScrnUpdate := false;
  CursorOff;

  TextAttr := LightGray;

  ScreenClear(mnuBackGrCol, mnuBackGrChar);         { Initialize the screen area }
  PartClear(1, 1, mnuScrnWidth, 1, LightGray, ' ');
  PartClear(1, 2, mnuScrnWidth, 2, mnuTopBar, mnuDblWide);
  PartClear(1, mnuScrnLen-1, mnuScrnWidth, mnuScrnLen-1, mnuTopBar, mnuSngWide);
  PartClear(1, mnuScrnLen, mnuScrnWidth, mnuScrnLen, LightGray, ' ');

  WriteAT(01, 01, Yellow, FullProgName + ' MANAGER v'+VersionID);
  DirectScrnUpdate := true;
  WriteRight(mnuScrnWidth - 01, 01, Yellow, 'Copyright 1997-2003 Maarten Bekers');
end; { proc. ShowHeader }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowInfo;
var SaveScrn: Pointer;
begin
  SaveScreen(SaveScrn);

  DirectScrnUpdate := false;
  ShadFillBoxTitle(22, 09, 62, 18, Makeattr(LightBlue, Blue), mnuStyle, True, ' About '+FullProgName+' Manager ');
  WriteCenterPos(24, 11, 36, MakeAttr(Lightgray, Blue), FullProgname+' Manager '+VersionID);
  WriteCenterPos(24, 13, 36, MakeAttr(Lightgray, Blue), 'Copyright 1997-2003');
  WriteCenterPos(24, 14, 36, MakeAttr(Lightgray, Blue), 'Maarten Bekers.');
  DirectScrnUpdate := true;
  WriteCenterPos(24, 16, 36, MakeAttr(Lightgray, Blue), 'All Rights Reserved');

  GetInput;

  RestoreScreen(SaveScrn);
end; { proc. ShowInfo }

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

procedure ShowHelpScreen;
var CH: Char;
begin
  Crt.ClrScr;
  WriteLn(Output, SystemMsgPrefix, 'EleBBS MANAGER - Command line parameters');
  WriteLn(Output);
  WriteLn(Output, '-U                     - Go directly to the usermanager');
  WriteLn(Output, '-F                     - Start directly in the filebase manager');
  WriteLn(Output, '-N                     - Don''t verify the filepaths exist');
  WriteLn(Output, '-SIMPLE                - Use a very basic characet set for the screen');
  WriteLn(Output);

  CH := UpCase(ReadKey);
  if CH = 'Q' then EXIT;
  if CH = #00 then ReadKey;
end; { proc. ShowHelpScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


procedure UserList_GetInfo(var Info1, Info2, Info3, Info4, Info5, Info6, Info7, Info8, Info9: List_StringType;
                          ItemNr: LongInt; HiLight: LongInt);
var Numread: NumReadType;
begin
  if (ItemNr > CacheEnd) OR (ItemNr < CacheStart) OR
      (NOT ((ItemNr - CacheStart) in [1..20])) then
       begin
         User_F^.Seek(Pred(ItemNr) * Sizeof(UsersRecord));
         NumRead := User_F^.BlkRead(UserInf^, SizeOf(UserInf^));

        CacheStart := ItemNr - 1;
        CacheEnd := (ItemNr - 01) + (NumRead div SizeOf(usersRecord));
      end; { if }

  Info1 := UserInf^[ItemNr - CacheStart].Name;
  Info2 := UserInf^[ItemNr - CacheStart].Location;
  Info3 := FStr(Itemnr - 01);
  Info4 := FStr(UserInf^[ItemNr - CacheStart].Security);

  if ReadBit(UserInf^[ItemNr - CacheStart].Attribute, 0) then Info5 := #254
       else Info5 := #32;
end; { proc. UserList_GetInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function UserList_CreateNewFile: Boolean;
begin
  UserList_Createnewfile := false;
end; { proc. UserList_CreateNewFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowUserHeader;
begin
  WriteAT(01, 01, mnuNormColor, ' Name                                Location                     #    Sec   Del');
end; { proc. ShowUserHeader }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure UpdateUserRecord(var Userinfo: UsersRecord; HiLight: Longint);
var UserTemp_F: pFileObj;
    UsersIdx  : UsersIdxRecord;
begin
  {-------------------------- Update USERS.BBS ------------------------------}
  New(UserTemp_F, Init);

  UserTemp_F^.Assign(GlobalCfg^.RaConfig^.MsgBasePath + UserBaseName);
  UserTemp_F^.FileMode := ReadWriteMode + DenyNone;
  UserTemp_F^.Open(1);
  UserTemp_F^.Seek(HiLight * SizeOf(UsersRecord));
  UserTemp_F^.BlkWrite(UserInfo, SizeOf(UsersRecord));

  Dispose(UserTemp_F, Done);

  {----------------------------- Update USERSIDX.BBS ------------------------}
  FillChar(UsersIdx, SizeOf(UsersIdx), 0);
  UsersIdx.NameCRC32 := RaCrc(SUpCase(UserInfo.Name), true);
  UsersIdx.HandleCRC32 := RaCrc(SUpCase(UserInfo.Handle), true);

  New(UserTemp_F, Init);

  UserTemp_F^.Assign(GlobalCfg^.RaConfig^.MsgBasePath + UserBaseIdxName);
  UserTemp_F^.FileMode := ReadWriteMode + DenyNone;
  UserTemp_F^.Open(1);
  UserTemp_F^.Seek(HiLight * SizeOf(UsersIdxRecord));
  UserTemp_F^.BlkWrite(UsersIdx, SizeOf(UsersIdxRecord));

  Dispose(UserTemp_F, Done);
end; { proc. UpdateUserRecord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function UserList_Activate(HiLight: LongInt; HibarPos: LongInt): Longint;
var OldUserInf: UsersRecord;
begin
  UserList_Activate := -1;
  CacheEnd := -1;

  MemMan.AllocMem(LineCfg^.Exitinfo, SizeOf(ExitinfoRecord), 'ExitinfoRecord', 'Userlist_Activate');
  FillChar(LineCfg^.Exitinfo^.UserInfo, SizeOf(UsersRecord), #00);

  User_F^.Seek((HiLight - 01) * SizeOf(UsersRecord));
  User_F^.BlkRead(LineCfg^.Exitinfo^.Userinfo, SizeOf(UsersRecord));

  UserEditObj^.OnlineEditor := FALSE;
  FillChar(OldUserInf, SizeOf(UsersRecord), #00);
  OldUserInf := LineCfg^.Exitinfo^.UserInfo;

  UserEditObj^.UserEdit(NOT NowPageDown);

  If RecordsDifferent(OldUserinf, LineCfg^.Exitinfo^.Userinfo, SizeOf(UsersRecord)) then
    begin
      UpdateUserRecord(LineCfg^.Exitinfo^.Userinfo, (HiLight - 1));
    end; { if recordsdifferent }

  ExtraWaiting := True;
  ExtraKey     := #13;
  NowPagedown := TRUE;

  Case UserEditObj^.AbortKeypress of
{ Pg-Down }  #81 : UserList_Activate := HiLight + 01;
{ Pg-Up }    #73 : UserList_Activate := HiLight - 01
    else begin
           ExtraWaiting := True;
           ExtraKey := #01;
           NowPageDown := false;
           ShowUserHeader;
         end; { else }
  end; { case }

  ReleaseMem(LineCfg^.Exitinfo, SizeOf(ExitinfoRecord));

  If NOT ExtraWaiting then
    begin
      Partclear(01, 01, mnuScrnWidth, mnuScrnLen, White, #32);
      ShowUserHeader;
      PartClear(1, 02, mnuScrnWidth, 02, mnuDisabled, mnuDblWide);
      PartClear(1, 24, mnuScrnWidth, 24, mnuDisabled, mnuSngWide);
    end; { if }
end; { proc. UserList_Activate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  UserList_Seek(Str: String): LongInt;
var UserInf: UsersRecord;
begin
  User_F^.Seek(0);

  UserList_Seek := -1;

  While NOT User_F^.EOF do
   begin
     User_F^.BlkRead(UserInf, SizeOf(UsersRecord));

     If SUpcase(Str) = SupCase(Copy(UserInf.Name, 1, Length(Str)))
      then begin
              UserList_Seek := User_F^.FilePos div SizeOf(Usersrecord);
              BREAK;
           end; { Found one }

   end; { While NOT eof }

end; { func. UserList_Seek }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  UserList_GetItems: LongInt;
begin
  UserList_GetItems := User_F^.FileSize div SizeOf(UsersRecord);
end; { func. UserList_GetItems }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  UserList_KeyPress(CH: Char; HiLight: LongInt;
                            var HiBarPos, TopOfScrn: Longint;
                            var StartRange, EndRange: LongInt): LongInt;
var Found     : Boolean;
    SaveScrn  : Pointer;
    UserTemp_F: pFileObj;
begin
  UserList_Keypress := -1;

  MemMan.AllocMem(LineCfg^.Exitinfo, SizeOf(ExitinfoRecord), 'ExitinfoRecord', 'Userlist_Keypress');
  Case CH of
    { Insert }   #82 : begin
                         User_F^.Seek(User_F^.FileSize);
                         FillChar(LineCfg^.Exitinfo^.Userinfo, SizeOf(UsersRecord), #00);

                         UserEditObj^.OnlineEditor := false;
                         SaveScreen(SaveScrn);
                         UserEditObj^.UserEdit(True);
                         RestoreScreen(SaveScrn);

                         if LineCfg^.Exitinfo^.Userinfo.Name <> '' then
                          begin
                            UpdateUserRecord(LineCfg^.Exitinfo^.Userinfo, User_F^.FileSize div SizeOf(usersRecord));
                          end; { if }
                       end; { if }
    { Delete }   #83 : begin
                         New(UserTemp_F, Init);

                         UserTemp_F^.Assign(GlobalCfg^.RaConfig^.MsgBasePath + UserBaseName);
                         UserTemp_F^.FileMode := ReadWriteMode + DenyNone;
                         UserTemp_F^.Open(1);

                         User_F^.Seek((HiLight - 01) * SizeOf(UsersRecord));
                         User_F^.BlkRead(LineCfg^.Exitinfo^.Userinfo, SizeOf(UsersRecord));

                         if ReadBit(LineCfg^.Exitinfo^.Userinfo.Attribute, 0) then
                           ClearBit(LineCfg^.Exitinfo^.Userinfo.Attribute, 00)
                             else SetBit(LineCfg^.Exitinfo^.Userinfo.Attribute, 00);

                         UserTemp_F^.Seek((HiLight - 01) * SizeOf(UsersRecord));
                         UserTemp_F^.BlkWrite(LineCfg^.Exitinfo^.UserInfo, SizeOf(UsersRecord));

                         Dispose(UserTemp_F, Done);

                         CacheEnd := -1;
                       end; { delete }
    { ALT-F }    #33 : begin
                        GotoXY(1,1); ClrEol;
                        WriteAT(01, 01, 15, 'Find: ');
                        CursorOn;

                        Found := False;
                        SearchStr := Edit(SearchStr, 7, 1, 15, 30, False, False, True);

                        CursorOff;

                        User_F^.Seek((HiLight) * SizeOf(UsersRecord));
                        While NOT User_F^.EOF do
                          begin
                            User_F^.BlkRead(LineCfg^.Exitinfo^.Userinfo, SizeOf(UsersRecord));

                              If (Pos(SupCase(SearchStr), SupCase(LineCfg^.Exitinfo^.Userinfo.Name)) > 00) OR
                                  (Pos(SupCase(SearchStr), SupCase(LineCfg^.Exitinfo^.Userinfo.Handle)) > 00) OR
                                   (Pos(SupCase(SearchStr), SUpCase(LineCfg^.Exitinfo^.Userinfo.Location)) >00) then
                                     begin
                                       Found := True;
                                       break;
                                     end; { found }
                            {$I+}
                          end; { while }

                         If Found then
                          UserList_Keypress := (User_F^.FilePos DIV SizeOf(UsersRecord))
                           else begin
                                  PartClear(01, 01, mnuScrnWidth, 01, 15, #32);
                                  WriteAT(01, 01, 15 + 128, 'Not found');
                                  While Keypressed do readkey;
                                  while not keypressed do ;
                                end; { not found }
                         SHowUserHeader;
                       end; { ALT-F }
  end; { case }
  ReleaseMem(LineCfg^.Exitinfo, SizeOf(ExitinfoRecord));
end; { func. UserList_Keypress }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure UserEditor;
var SaveScrn     : Pointer;
    OldWindFore,
    OldWindBack  : Byte;
    OldBordFore,
    OldBordBack  : Byte;
    OldHiFore    : Byte;
begin
  SearchStr := '';

  New(User_F, Init);
  User_F^.Assign(GlobalCfg^.RaConfig^.MsgBasePath + UserBaseName);
  User_F^.FileMode := ReadWriteMode + DenyNone;

  if NOT User_F^.OpenOrCreate(1) then
    begin
      SaveScreen(SaveScrn);
      DirectScrnUpdate := false;
      ShadFillBoxTitle(15, 10, 69, 16, MakeAttr(LightBlue, Blue), mnuStyle, True, ' Error ');
      WriteCenterPos(26, 12, 36, MakeAttr(Lightgray, Blue), UserBaseName +' does not exist.');
      DirectScrnUpdate := true;
      WriteCenterPos(16, 14, 36, MakeAttr(Lightgray, Blue), 'Unable to (re)create '+UserBaseName+' file, please correct');

      GetInput;
      RestoreScreen(SaveScrn);
      EXIT;
    end; { if }

  if User_F^.FileSize < SizeOf(UsersRecord) then
    begin
      MemMan.AllocMem(LineCfg^.Exitinfo, SizeOf(ExitinfoRecord), 'ExitinfoRecord', 'UserEditor');
      Fillchar(LineCfg^.Exitinfo^.Userinfo, SizeOf(UsersRecord), #00);
      UserEditObj^.OnLineEditor := False;

      OldWindFore := GlobalCfg^.RaConfig^.WindFore;
      OldWindBack := GlobalCfg^.RaConfig^.WindBack;
      OldBordFore := GlobalCfg^.RaConfig^.BorderFore;
      OldBordBack := GlobalCfg^.RaConfig^.BorderBack;
      OldHiFore   := GlobalCfg^.RaConfig^.HiFore;

      GlobalCfg^.RaConfig^.WindFore := LightGray;
      GlobalCfg^.RaConfig^.WindBack := Black;
      GlobalCfg^.RaConfig^.BorderFore := LightCyan;
      GlobalCfg^.RaConfig^.BorderBack := Black;
      GlobalCfg^.RaConfig^.HiFore := Yellow;

      SaveScreen(SaveScrn);
      UserEditObj^.UserEdit(True);
      RestoreScreen(SaveScrn);

      GlobalCfg^.RaConfig^.HiFore := OldHiFore;
      GlobalCfg^.RaConfig^.WindFore := OldWindFore;
      GlobalCfg^.RaConfig^.WindBack := OldWindBack;
      GlobalCfg^.RaConfig^.BorderFore := OldBordFore;
      GlobalCfg^.RaConfig^.BorderBack := OldBordBack;

      if LineCfg^.Exitinfo^.Userinfo.Name <> '' then
        begin
          UpdateUserRecord(LineCfg^.Exitinfo^.Userinfo, 0);
        end; { if }

      ReleaseMem(LineCfg^.Exitinfo, SizeOf(ExitinfoRecord));
    end; { if (create new record }

  if User_F^.FileSize < SizeOf(UsersRecord) then
    begin
      Dispose(User_F, Done);
      EXIT;
    end; { if }

  SaveScreen(SaveScrn);

  Partclear(01, 01, mnuScrnWidth, mnuScrnLen, White, #32);
  ShowUserHeader;
  PartClear(1, 02, mnuScrnWidth, 02, mnuDisabled, mnuDblWide);
  PartClear(1, 24, mnuScrnWidth, mnuScrnLen - 01, mnuDisabled, mnuSngWide);

  OldWindFore := GlobalCfg^.RaConfig^.WindFore;
  OldWindBack := GlobalCfg^.RaConfig^.WindBack;
  OldBordFore := GlobalCfg^.RaConfig^.BorderFore;
  OldBordBack := GlobalCfg^.RaConfig^.BorderBack;

  GlobalCfg^.RaConfig^.WindFore := LightGray;
  GlobalCfg^.RaConfig^.WindBack := Black;
  GlobalCfg^.RaConfig^.BorderFore := LightCyan;
  GlobalCfg^.RaConfig^.BorderBack := Black;

  CacheStart := -1;
  CacheEnd := -1;
  NowPageDown := false;

  Memman.AllocMem(Userinf, SizeOf(UserInf^), 'Userinfo^', 'UserEditor');
  DoList(False, 01,
         36, 29, 05, 07, 05, 00, 00, 00, 00,
         01, 02, 80, mnuScrnLen - 01, '',
         '          (ENTER) Edit   (INS)ert   (DEL)ete   (ALT-F)ind   (ESC) Exit',
         {$IFDEF FPC}@{$ENDIF}UserList_GetInfo,
         {$IFDEF FPC}@{$ENDIF}UserList_Activate,
         {$IFDEF FPC}@{$ENDIF}UserList_Seek,
         {$IFDEF FPC}@{$ENDIF}General_AbortCheck,
         {$IFDEF FPC}@{$ENDIF}UserList_Keypress,
         {$IFDEF FPC}@{$ENDIF}UserList_GetItems,
         {$IFDEF FPC}@{$ENDIF}UserList_CreateNewFile,
         {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
         False,
         00, 00,
         True, false);
  ReleaseMem(Userinf, SizeOf(UserInf^));

  GlobalCfg^.RaConfig^.WindFore := OldWindFore;
  GlobalCfg^.RaConfig^.WindBack := OldWindBack;
  GlobalCfg^.RaConfig^.BorderFore := OldBordFore;
  GlobalCfg^.RaConfig^.BorderBack := OldBordBack;

  Dispose(User_F, Done);

  RestoreScreen(SaveScrn);
end; { proc. UserEditor }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ProcessKey(var CH:Char);
begin
 { ALT-Z } If Ch=#44 then DoDosShell('Type "EXIT" to return to '+FullProgName+ ' Manager.');
 { ALT-J } if CH=#36 then DoDosShell('Type "EXIT" to return to '+FullProgName+ ' Manager.');
 { ALT-K } if CH=#18 then DoPickFunctionKey(CH);
end; { proc. Processkey }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Const DirectUser : Boolean = false;
      DirectFile : Boolean = false;
      DirectHelp : Boolean = false;

var Menu  : PullRecord;
    Choice: Word;
    CH    : Char;
begin
  SetWindowTitle(FullProgName + ' MANAGER');

  {$IFNDEF WINGUI}
    AssignCrt(CrtOutput);
    System.Rewrite(Crtoutput);
  {$ELSE}
     Move(Output, Crtoutput, SizeOf(Crtoutput)); {  Save this for later use }
  {$ENDIF}


  RunningBBS := False;
  SetupScrnU;
  {$IFDEF OVERLAY}
    InitErrorHandler;
    GenCfgInit;
  {$ENDIF}

  {-- Test if we are executed with EleMON enabled --------------------------}
  remUtilitySetup;

  {-- initialize some objects well use later on ----------------------------}
  New(LangObj, Init);
  New(UserEditObj, Init);

  ShowHeader;
  InitSystemNames;
  CheckRegistered;
  KeyProc := {$IFDEF FPC}@{$ENDIF}ProcessKey;

  if NOT AskForPassword(GlobalCfg^.Raconfig^.KeyBoardPwd) then
   begin
    TextAttr := 07;
    CursorOn;
    ReleaseMem(GlobalCfg^.RaConfig, SizeOf(ConfigRecord));
    Partclear(01, 01, mnuScrnWidth, mnuScrnLen, White, #32);
    Exit;
   end; { wrong password }

  if (Pos('?', ParamString) > 0) then DirectHelp := true;
  if (Pos('-F', SupCase(ParamString)) > 0) OR (Pos('/F', SupCase(ParamString)) > 0) then
    DirectFile := TRUE;
  if (Pos('-U', SupCase(ParamString)) > 0) OR (Pos('/U', SupCase(ParamString)) > 0) then
    DirectUser := TRUE;

  if (NOT DirectUser) AND (NOT DirectFile) AND (NOT DirectHelp) then
    begin;
      Memman.AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', '()');
      InitPullMenu(Menu);
      AddPullItem(Menu.PullInf^[01], ' Files      ', 101, #00, 'File database manager and editor', 1);
      AddPullItem(Menu.PullInf^[02], ' Users      ', 102, #00, 'User database manager and editor', 1);
      AddPullItem(Menu.PullInf^[03], ' Info       ', 103, #00, 'Version information', 1);
      AddPullItem(Menu.PullInf^[04], ' DOS Shell  ', 104, #00, 'Shell to DOS', 1);
      AddPullItem(Menu.PullInf^[05], ' Exit       ', 105, #00, 'Exit to DOS', 1);

      Menu.Items   := 5;
      Menu.Width   := 13;
      Menu.Length  := 5;
      Menu.X       := 33;
      Menu.Y       := 09;
      Menu.HiLight := 01;
      Menu.AddSpace:= False;
      Menu.Title   := ' Manager ';
      Menu.PosArray[1].XInc := 00;
      Menu.PosArray[1].YDec := 00;

      ShowMenu(Menu, False);

      Repeat;
        Choice := DoPullMenu(Menu, CH, False, False);

        If Choice<>0 then
          Case Choice of
              101 : FileMgr_Start;
              102 : UserEditor;
              103 : ShowInfo;
              104 : DoDosShell('Type "EXIT" to return to '+FullProgName+ ' Manager.');
              105 : Choice := 00;
          End; { Case }

       Until Choice=0;

     RemoveMenu(Menu);
     ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
    end; { if not DirectUser }

  if DirectHelp then ShowHelpScreen
    else begin
           if DirectUser then UserEditor;
           if (DirectFile) AND (NOT DirectUser) then FileMgr_Start;
         end; { if }

  ReleaseMem(GlobalCfg^.RaConfig, SizeOf(ConfigRecord));

  CursorOn;
  Partclear(01, 01, mnuScrnWidth, mnuScrnLen, White, #32);
  GotoXY(1, 1);
end.
