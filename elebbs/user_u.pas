unit User_U;
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
** USER_U.TPU, Userbase routines for EleBBS
**
**
** Copyright (c) 1999 by Maarten Bekers
**
** Created : 01-Feb-1999
** Last update : 01-Feb-1999
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Global, CfgRec, Cfgfile;

procedure GetUserRecord(Nr: SmallWord;
                        var User: UsersRecord;
                        var UserExt: UserExtensionRecord;
                        UpdateInfo: Boolean);
procedure UpdateUserRecord;                               { Writes userrecord }
procedure AddToUserBase;              { Add Exitinfo^.USERINFO to Messagebase }
procedure WriteUserRecord(Nr: SmallWord; var User: UsersRecord; var UserExt: UserExtensionRecord);
function  SearchUser(Name: String):Integer;        { Search username/handle }
procedure SetNewUserDefaults(var UserInfo: UsersRecord;  { Default userinfo }
                             var UserExt : UserExtensionRecord);

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses ReadCfg, BitWise, Debug_U, GenFile, FileRout, LongStr,
     Cases, CentrStr, ElLog_U, Crc_Unit, JDates, FileObj,
     usrExt, ObjDec;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function SearchUser(Name: String): Integer;{ Search username/handle }
var User_F      : pFileObj;
    User        : UsersIdxRecord;
    Counter     : Longint;
    FoundUser   : Boolean;
    TotalUsers  : Longint;
    NumRead     : NumReadType;
    NrItems     : Longint;
    ItemsCounter: Longint;

    UserCRC     : Longint;
    UserInfo    : UsersRecord;
    UserExt     : UserExtensionRecord;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logUserbase, 'SearchUser: ' + Name + ' (begin)');
     DebugObj.DebugLog(logUserBase, 'SearchUser: MsgBasePath : ' + GlobalCfg^.RaConfig^.MsgbasePath);
     DebugObj.DebugLog(logUserBase, 'SearchUser: UserBaseName: ' + UserBaseName);
  {$ENDIF}

  NrItems := GetFileSize(GlobalCfg^.RaConfig^.MsgBasePath + UserBaseName, SizeOf(UsersRecord));

  if GetFileSize(GlobalCfg^.RaConfig^.MsgBasePath + UserBaseIdxName, SizeOf(UsersIdxRecord)) < NrItems then
      begin
        {$IFDEF WITH_DEBUG}
           DebugObj.DebugLog(logUserBase, 'SearchUser - Rebuilding user-fast-index: NrItems = '+FStr(NrItems));
           DebugObj.DebugLog(logUserBase, 'SearchUser - Rebuilding user-fast-index: FSize   = '+FStr(
                GetFileSize(GlobalCfg^.RaConfig^.MsgBasePath + UserBaseIdxName, SizeOf(UsersIdxRecord))));
        {$ENDIF}

        BuildFastIndex;
        RaLog('!', 'Rebuilding user fast-index');
      end; { if }

  if GetFileSize(GlobalCfg^.RaConfig^.MsgBasePath + UserBaseLastRead, SizeOf(LastreadRecord)) < NrItems then
      begin
        BuildLastRead;
        RaLog('!', 'Rebuilding user lastread index');
      end; { if }

  SearchUser := -1;
  Name := NoDoubleSpace(SUpcase(Trim(Name)));
  Counter := 00;


  New(User_F, Init);
  User_F^.Assign(GlobalCfg^.RaConfig^.MsgbasePath + UserBaseIdxName);
  User_F^.FileMode := ReadMode + DenyNone;
  if NOT User_F^.OpenOrCreate(1) then
    begin
      {$IFDEF WITH_DEBUG}
         DebugObj.DebugLog(logUserbase, 'SearchUser: Unable to open UserBaseIDX name');
      {$ENDIF}

      Dispose(User_F, Done);
      EXIT;
    end; { if }

  TotalUsers := User_F^.FileSize div SizeOf(UsersIdxRecord);
  FoundUser := False;
  UserCRC := RaCrc(Name, true);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logUserBase, 'SearchUser - TotalUsers= ' + FStr(TotalUsers));
    DebugObj.DebugLog(logUserbase, 'SearchUser - UserCRC   = ' + FStr(UserCRC));
    DebugObj.DebugLog(logUserBase, 'SearchUser - Name      = ' + name);
  {$ENDIF}

  While (NOT FoundUser) AND (Counter < TotalUsers) AND (TotalUsers>00) do
   begin
     NumRead := User_F^.BlkRead(User, SizeOf(UsersIdxRecord));

     {$IFDEF WITH_DEBUG}
       DebugObj.DebugLog(logUserbase, 'SearchUser - record #' + FStr(Counter) + ' /  UserCRC = '+FStr(User.NameCrc32));
       DebugObj.DebugLog(logUserBase, 'SearchUser - numread#' + FStr(Numread));
     {$ENDIF}

     if NumRead <> SizeOf(UsersIdxRecord) then BREAK;
     FoundUser := ((UserCRC = User.NameCRC32) OR (UserCRC = User.HandleCRC32));

     if FoundUser then
       begin
         {$IFDEF WITH_DEBUG}
           DebugObj.DebugLog(logUserbase, 'SearchUser: Found the user');
         {$ENDIF}

         GetUserRecord(Counter, UserInfo, UserExt, false);
         if ReadBit(Userinfo.Attribute, 0) then FoundUser := false;

          {$IFDEF WITH_DEBUG}
            DebugObj.DebugLog(logUserbase, 'SearchUser - Attribute: ' + FStr(UserInfo.Attribute));
            if FoundUser then
              DebugObj.DebugLog(logUserbase, 'SearchUser - DID found the user')
               else DebugObj.DebugLog(logUserbase, 'SearchUser - did NOT! found the user!');
          {$ENDIF}
       end; { if }

     if NOT FoundUser then Inc(Counter);
   end; { If }

  Dispose(User_F, Done);
  if FoundUser then SearchUser := Counter
    else SearchUser := -1;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logUserbase, 'SearchUser: Found the user - ByteBool: '+FStr(Byte(FoundUser)));
    DebugObj.DebugLog(logUserBase, 'SearchUser: Counter                  : '+FStr(Counter));
    DebugObj.DebugLog(logUserBase, 'SearchUser: ' + Name + ' ( end )');
  {$ENDIF}
end; { func. SearchUser }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GetUserRecord(Nr: SmallWord;
                        var User: UsersRecord;
                        var UserExt: UserExtensionRecord;
                        UpdateInfo: Boolean);
var User_F  : pFileObj;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logUserBase, 'GetUserRecord (begin)');
  {$ENDIF}

  FillChar(User, SizeOf(UsersRecord), #0);

  Config_OpenFile(User_F, GlobalCfg^.RaConfig^.MsgbasePath + UserbaseName,
                            SizeOf(UsersRecord), ReadMode + DenyNone,False,True);
  Config_SeekFile(User_F, Nr);
  Config_ReadFile(User_F, User, 1);
  Config_DoneFile(User_F);

  if UpdateInfo then
    begin
      LineCfg^.AnsiOn := ReadBit(User.Attribute, 3);
      LineCfg^.AvatarOn := ReadBit(User.Attribute2, 1);
      LineCfg^.GuestUser := ReadBit(User.Attribute2, 6);

      if LineCfg^.GuestUser then
        LineCfg^.SaveGuestRecord^ := User;

      if LineCfg^.RipOn then LineCfg^.AnsiOn := True;
      LineCfg^.Exitinfo^.Userinfo.ScreenWidth := 80;
      LineCfg^.UserAgeSave := GetUserAge(LineCfg^.Exitinfo^.Userinfo.BirthDate, true);
    end; { if UpdateInfo }

  {-- retrieve useridx.ele data -------------------------------------------}
  if LineCfg^.UserExtensionSize > 0 then
    begin
      Config_OpenFile(User_F, GlobalCfg^.RaConfig^.MsgbasePath + UserBaseEleName,
                          LineCfg^.UserExtensionSize, ReadMode + DenyNone, True,
                      True);
      Config_SeekFile(User_F, Nr);
      Config_ReadFile(User_F, UserExt, 1);
      Config_DoneFile(User_F);
    end; { if }

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logUserBase, 'GetUserRecord ( end )');
  {$ENDIF}
end; { proc. GetUserRecord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure WriteUserRecord(Nr: SmallWord; var User: UsersRecord; var UserExt: UserExtensionRecord);
var User_F  : pFileObj;
    UserIDX : UsersIDXrecord;
begin;
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileRout, 'WriteUserRecord (begin)');
  {$ENDIF}

  {-- update users.bbs ----------------------------------------------------}
  Config_OpenFile(User_F, GlobalCfg^.RaConfig^.MsgbasePath + UserbaseName,
                            SizeOf(UsersRecord), WriteMode + DenyNone, True, True);
  Config_SeekFile(User_F, Nr);
  Config_WriteFile(User_F, User, 1);
  Config_DoneFile(User_F);

  {-- update useridx.bbs --------------------------------------------------}
  FillChar(UserIDX, SizeOf(UserIDX), #00);
  UserIDX.NameCRC32 := RaCrc(Trim(User.Name), true);
  UserIDX.HandleCRC32 := RaCrc(Trim(User.Handle), true);

  Config_OpenFile(User_F, GlobalCfg^.RaConfig^.MsgbasePath + UserBaseIdxName, SizeOf(UsersIdxRecord), WriteMode+DenyNone,True,
                  True);
  Config_SeekFile(User_F, Nr);
  Config_WriteFile(User_F, UserIDX, 1);
  Config_DoneFile(User_F);

  {-- update useridx.ele --------------------------------------------------}
  if LineCfg^.UserExtensionSize > 0 then
    begin
      Config_OpenFile(User_F, GlobalCfg^.RaConfig^.MsgbasePath + UserBaseEleName,
                      LineCfg^.UserExtensionSize, WriteMode+DenyNone,True,
                      True);
      Config_SeekFile(User_F, Nr);
      Config_WriteFile(User_F, UserExt, 1);
      Config_DoneFile(User_F);
    end; { if }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileRout, 'WriteUserRecord ( end )');
  {$ENDIF}
end; { proc. WriteUserRecord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AddToUserBase;              { Add Exitinfo^.USERINFO to Messagebase }
var User_F     : pFileObj;
    UserIDX    : UsersIDXrecord;
    LastReadInf: LastReadRecord;
    UserRecNr  : Longint;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileRout, 'AddToUserBase (begin)');
  {$ENDIF}

  {-- Write the actual USER record --------------------------------------------}
  Config_OpenFile(User_F, GlobalCfg^.RaConfig^.MsgbasePath + UserBaseName, SizeOf(UsersRecord), WriteMode+DenyNone,True,True);
  Config_SeekFile(User_F, User_F^.FileSize);

  UserRecNr := User_F^.FileSize DIV SizeOf(UsersRecord);

  Config_WriteFile(User_F, LineCfg^.Exitinfo^.Userinfo, 1);
  Config_DoneFile(User_F);

  {-- Write the USERIDX record ------------------------------------------------}
  FillChar(UserIDX, SizeOf(UserIDX), #00);
  UserIDX.NameCRC32 := RaCrc(Trim(LineCfg^.Exitinfo^.Userinfo.Name), true);
  UserIDX.HandleCRC32 := RaCrc(Trim(LineCfg^.Exitinfo^.Userinfo.Handle), true);

  Config_OpenFile(User_F, GlobalCfg^.RaConfig^.MsgbasePath +UserBaseIdxName, SizeOf(UsersIdxRecord), WriteMode+DenyNone, True,
                  True);
  Config_SeekFile(User_F, User_F^.FileSize);
  Config_WriteFile(User_F, UserIDX, 1);
  Config_DoneFile(User_F);

  {-- Write the Lastread record -----------------------------------------------}
  FillChar(LastreadInf, SizeOf(LastReadInf), #0);

  Config_OpenFile(User_F, GlobalCfg^.RaConfig^.MsgbasePath + UserBaseLastRead, SizeOf(LastReadRecord), WriteMode+DenyNone,True,
                  True);
  Config_SeekFile(User_F, User_F^.FileSize);
  Config_WriteFile(User_F, LastReadInf, 1);
  Config_DoneFile(User_F);

  {-- write the userele.bbs file ------------------------------------------}
  if LineCfg^.UserExtensionSize > 0 then
    begin
      Config_OpenFile(User_F, GlobalCfg^.RaConfig^.MsgbasePath + UserBaseEleName,
                      LineCfg^.UserExtensionSize, WriteMode+DenyNone, True,
                      True);
      Config_SeekFile(User_F, User_F^.FileSize);
      Config_WriteFile(User_F, LineCfg^.UserExtension^, 1);
      Config_DoneFile(User_F);
    end; { if }

  {-- update the userrecordnumber --------------------------------------------}
{  Exitinfo^.UserRecord := UserRecNr; }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileRout, 'AddToUserBase ( end )');
  {$ENDIF}
end; { proc. AddToUserBase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure UpdateUserRecord;                              { Writes userrecord }
var UserRecord: Integer;
    User_F    : pFileObj;
    UserIDX   : UsersIDXrecord;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileRout, 'UpdateUserRecord');
  {$ENDIF}

  UserRecord := LineCfg^.Exitinfo^.UserRecord;
  if NOT LineCfg^.LoggedOn then EXIT;
  if UserRecord=-1 then exit;

  WriteUserRecord(UserRecord, LineCfg^.Exitinfo^.UserInfo, LineCfg^.UserExtension^);
end; { proc. UpdateUserRecord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SetNewUserDefaults(var UserInfo: UsersRecord;  { Default userinfo }
                             var UserExt : UserExtensionRecord);
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logLogon, 'SetNewUserDefaults');{$endif}

 FillChar(UserInfo, SizeOf(Userinfo), #00);

 With UserInfo do
  begin
    FileArea := 01;
    MsgArea := 01;
    FileGroup := 01;
    MsgGroup := 01;
    LastPwdChange := 00;

    LastTime := TimeStr(False, False);
    LastDate := DateStr;

    ClearBit(Attribute, 0);                                     { Deleted off }
    If Byte(GlobalCfg^.RaConfig^.ClearScreen)=Byte(No) then ClearBit(Attribute, 1)
     else SetBit(Attribute, 01);                               { Clear Screen }
    If Byte(GlobalCfg^.RaConfig^.MorePrompt)=Byte(No) then ClearBit(Attribute, 2)
     else SetBit(Attribute, 02);                                { More prompt }
    If Byte(GlobalCfg^.RaConfig^.Ansi)=Byte(Yes) then SetBit(Attribute, 3)
     else ClearBit(Attribute, 3);                                      { ANSI }
    ClearBit(Attribute, 4);                                         { No Kill }
    ClearBit(Attribute, 5);                                   { XFer priority }
    If Byte(GlobalCfg^.RaConfig^.ExtEd)=Byte(Yes) then SetBit(Attribute, 6)
     else ClearBit(Attribute, 6);                     { Fullscreen msg editor }
    ClearBit(Attribute, 7);                                      { Quiet mode }

    If Byte(GlobalCfg^.RaConfig^.Hotkeys)=Byte(Yes) then SetBit(Attribute2, 0)
     else ClearBit(Attribute2, 0);                                  { HotKeys }
    If Byte(GlobalCfg^.RaConfig^.Avatar)=Byte(Yes) then SetBit(Attribute2, 1)
     else ClearBit(Attribute2, 1);                                   { Avatar }
    If Byte(GlobalCfg^.RaConfig^.FullMsgView)=Byte(Yes) then SetBit(Attribute2, 2)
     else ClearBit(Attribute2, 2);                                   { Avatar }
    ClearBit(Attribute2, 3);                                         { Hidden }
    ClearBit(Attribute2, 4);                                  { Page priority }
    If Byte(GlobalCfg^.RaConfig^.EchoCheck)=Byte(Yes) then ClearBit(Attribute2, 5)
     else SetBit(Attribute2, 5);                              { EchoMailCheck }
    ClearBit(Attribute2, 6);                                  { Guest account }
    ClearBit(Attribute2, 7);                              { Post bill enabled }

    Flags := GlobalCfg^.RaConfig^.NewFlags;
    Credit := GlobalCfg^.RaConfig^.NewCredit;
    Security := GlobalCfg^.RaConfig^.NewSecurity;

    Uploads := GlobalCfg^.RaConfig^.NewUserUlCredit;
    UploadsK := GlobalCfg^.RaConfig^.NewUserUlCreditK;
    Group := GlobalCfg^.RaConfig^.NewUserGroup;
    CombinedInfo := GlobalCfg^.RaConfig^.DefaultCombined;
    FirstDate := DateStr;

    SubDate := FutureDate(GlobalCfg^.RaConfig^.NewUserSub);
    if GlobalCfg^.RaConfig^.NewUserSub = 00 then
      SubDate := '';

    ScreenWidth := 80;
    Language := GlobalCfg^.RaConfig^.NewUserLanguage;
    DateFormat := GlobalCfg^.RaConfig^.NewUserDateFormat;

    ClearBit(Attribute3, 0);              { MailBoxCheck: Selected areas only }
  end; { With }

  {-- now get the userele.bbs defualts ------------------------------------}
  usrext_SetDefaults(UserExt);
end; { proc. SetNewUserDefaults }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)


end. { unit USER_U }

