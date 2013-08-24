unit Logon;
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
** LOGON routines for EleBBS
**
** Copyright (c) 1996, 1997 by Maarten Bekers
**
** Created : 01-Nov-1997
** Last update : 01-Nov-1997
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF WITH_FULL}
  This unit shouldn''t be compiled for an utility program
{$ENDIF}

Uses Crt, Dos, Global, FileRout, StUtils, Support, Ral, FileSys,
     {$IFNDEF Os2}Multi,{$ENDIF}
      EMSI, Mail, MenuFunc, JDates, Question, InOut_U, Crc_Unit,
       Debug_U, Access_U, UnixDate, StatusB, FastScrn, ElLog_U,
        LineEd, MultiLn, Dispans, Control, MailInt, AutoDet, BitWise, Colors,
         Mnuser, Cases, LongStr, CfgRec, CentrStr, MemMan, Limit_U,
          ExitProg, GenFile, OutBlock, Input_U, User_u, CurTIme,
           WinTitle, StrUnit, elx_BBS, usrExt, objDec;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GetUsersPassword(TempPassword: String; var QuitOnEmpty: Boolean);
procedure InitUserSettings(ResetTime: Boolean);
procedure GetUserName(var TempPSW: String);
Procedure ScanPhoneNumbers;          { Scan for duplicate or invalid phone#s }
Function PerformNewUserLogon: Byte;
procedure PerformLogon;
procedure DoEventWarning(var Exitinfo: ExitinfoRecord);

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
uses WriteMsg, Main, EleMenu, Ranges, FileObj, ScrnU;

procedure InitUserSettings(ResetTime: Boolean);
begin
  ReadLanguageRA(Max(0, LineCfg^.Exitinfo^.Userinfo.Language-1));
  if NOT LangObj^.ReadRalFile(LineCfg^.Language^.DefName) then
    LoadDefaultLanguage;

 {$IFDEF WITH_FULL}
   GetLevelLimitsInfo(LineCfg^.Exitinfo^.Userinfo.Security, False, ResetTime);
 {$ENDIF}
end; { proc. InitUserSettings }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoEventWarning(var Exitinfo: ExitinfoRecord);
var OldTimeLimit: Word;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logLogon, 'DoEventWarning (begin)');
  {$ENDIF}

  if (SUpcase(Exitinfo.EventInfo.StartTime) = 'NONE') then EXIT;
  if (SUpcase(Exitinfo.EventInfo.StartTime) = '') then EXIT;
  if (NextEventMins < 0) then EXIT;
  if (Exitinfo.TimeLimit <= NextEventMins) then EXIT;

  OldTimeLimit := Word(Exitinfo.TimeLimit);

  Exitinfo.Userinfo.Elapsed := SmallInt(LineCfg^.LimitsInfo^.LTime - NextEventMins);
  Exitinfo.TimeLimit := SmallWord(LineCfg^.LimitsInfo^.LTime - Exitinfo.Userinfo.Elapsed);
  if defStatusLine <> 10 then
    StatusDisplay(DefStatusLine + 10, false);

  LineCfg^.EventDeducted := Word(OldTimeLimit - Exitinfo.TimeLimit);
  contrObj^.SetStartTime;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logLogon, 'DoEventWarning ( end )');
  {$ENDIF}
end; { proc. DoEventWarning }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowEventWarning(var Exitinfo: ExitinfoRecord);
begin
  if LineCfg^.EventDeducted <= 0 then EXIT;

  OutputObj^.ClearScreen;
  RaLog('!', 'Event at '+Exitinfo.EventInfo.StartTime+', time adjusted downwards');

  if DisplayhotFile('TIMEWARN', []) = #01 then
    begin
      WriteLn('`A12:');
      WriteLn(LangObj^.ralGet(ralEventIn), #32, NextEventMins, #32, LangObj^.ralGet(ralMinutes));
      WriteLn(LangObj^.ralGet(ralTimeAdj), #32, NextEventMins, #32, LangObj^.ralGet(ralMinutes));
      InputObj^.PressEnter(True, True);
    end; { if }
end; { proc. ShowEventWarning }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GetUserName(var TempPSW: String);
var Counter  : Byte;
    SaveScrn : Pointer;
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logLogon, 'GetUsername');{$endif}
  Counter := 01;
  LineCfg^.CheckInactivity := TRUE;
  contrObj^.SetTimeOut;

  repeat;

{---------------------------------}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{---------------------------------}

    LineCfg^.Exitinfo^.Userinfo.Name := '';
    Write('`A', MakeAttr(GlobalCfg^.RaConfig^.NormFore, GlobalCfg^.RaConfig^.NormBack), ':');

    Write(GlobalCfg^.RaConfig^.LogonPrompt);

    {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logLogon, 'L in charset(1) = '+Bool2Str('L' in [#32..#254] - NonUserNameChars));
      DebugObj.DebugLog(logLogon, 'L in charset(2) = '+Bool2Str('L' in [#32..#254]));
    {$ENDIF}

    GetString(LineCfg^.Exitinfo^.Userinfo.Name, 35, [#32..#254] - NonUserNameChars,
              GlobalCfg^.Elconfig^.CapitalizeUsername, False, False,
              (Byte(GlobalCfg^.RaConfig^.Emsi_Enable)=Byte(yes)));
    LineCfg^.Exitinfo^.Userinfo.Name := NoDoubleSpace(Trim(LineCfg^.Exitinfo^.Userinfo.Name));
    if (ProgTerminated) then EXIT;

    {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logLogon, 'Got username = '+LineCfg^.Exitinfo^.Userinfo.Name);
    {$ENDIF}

    if Pos('**EMSI_', SUpCase(LineCfg^.Exitinfo^.UserInfo.Name))>00 then
        begin
          {$IFDEF WITH_DEBUG}
            DebugObj.DebugLog(logLogon, 'IsIemsi');
          {$ENDIF}

          SaveScreen(SaveScrn);

          BoxWindow(25, 11, 55, 15, GlobalCfg^.RaConfig^.BorderFore, GlobalCfg^.RaConfig^.BorderBack, 'Message');
          FastWrite(27, 13, MakeAttr(GlobalCfg^.RaConfig^.WindFore, GlobalCfg^.RaConfig^.WindBack),
                   'Negotiating IEMSI session.');

          {$IFDEF WITH_FULL}
           IEMSI_GetUser;
           if Pos('**EMSI_', SUpcase(LineCfg^.Exitinfo^.Userinfo.Name))>00 then
             LineCfg^.Exitinfo^.Userinfo.Name := '';
          {$ENDIF}

          InputObj^.DorCleanKeys;

          RestoreScreen(SaveScrn);
          Counter := 01;
        end; { if }
    LineCfg^.PauseChecking := True;

    TempPSW := '';
    if Pos(';', LineCfg^.Exitinfo^.Userinfo.Name) > 00 then
       begin
         TempPSW := Copy(LineCfg^.Exitinfo^.Userinfo.Name, Pos(';', LineCfg^.Exitinfo^.Userinfo.Name)+01, 255);
         Delete(LineCfg^.Exitinfo^.Userinfo.Name, Pos(';', LineCfg^.Exitinfo^.Userinfo.Name), 255);
       end; { if }

    WriteLn;

    Inc(Counter);

    If GlobalCfg^.RaConfig^.BlankLogins>0 then
     If Counter>GlobalCfg^.RaConfig^.BlankLogins then
      If LineCfg^.Exitinfo^.Userinfo.Name='' then
       begin;
         RaLog('!', 'Blank login threshold exceeded, exiting.');
         HangUp;
         Exit;
       End; { If Counter }

  Until (LineCfg^.Exitinfo^.Userinfo.Name<>'');
end; { proc. GetUserName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GetUsersPassword(TempPassword: String; var QuitOnEmpty: Boolean);
var Counter: Byte;
    WrongPassWord: Boolean;
    QuoteText: pStringArrayObj;
    MsgNr: LongInt;
begin
  LineCfg^.ShowStatusBar := true;

  if defStatusLine <> 10 then Statusdisplay(1, false);

  {$ifdef With_Debug}
    DebugObj.DebugLog(logLogon, 'TempPassword='+temppassword);
    DebugObj.DebugLog(logLogon, 'User''s password is: "'+LineCfg^.Exitinfo^.Userinfo.Password+'"');
    DebugObj.DebugLog(logLogon, 'User''s PasswordCRC is: "'+FStr(LineCfg^.Exitinfo^.Userinfo.PasswordCRC)+'"');
    DebugObj.DebugLog(logLogon, 'RaCrc('''''') =        : "'+FStr(RaCrc('', true))+'"');
  {$endif}

  if (RaCrc('', true) = LineCfg^.Exitinfo^.Userinfo.PasswordCRC)
      AND (LineCfg^.Exitinfo^.Userinfo.Password='') then EXIT;
  if LineCfg^.Exitinfo^.Emsi_Session then
    TempPassword := LineCfg^.Emsi_User^.Password;

  if TempPassword = '' then
    WriteLn;

  Counter := 00;
  repeat
    Inc(Counter);

    If Counter>GlobalCfg^.RaConfig^.PassWordTries then
      begin;
        Writeln(LangObj^.ralGet(ralNoAccess));
        WriteLn;

        RaLog('!', 'Exceeded maximum password attempts, user disconnected');

        If GlobalCfg^.RaConfig^.PwdBoard<>00 then
            begin;
              RaLog('!', 'WatchDog: User notified of password attempt');
              LocalScreenLn(SystemMsgPrefix + 'Security watchdog activated, notifying user.');

              If NOT FileExist(OpenRaFile('watchdog.msg')) then
                  RaLog('!', 'Unable to open watchdog.msg');

              {$IFDEF WITH_FULL}
                FilePost(Ra250MsgArea(GlobalCfg^.RaConfig^.PwdBoard), GlobalCfg^.RaConfig^.Sysop,
                         LineCfg^.Exitinfo^.Userinfo.Name, 'Incorrect password logon attempt',
                         'watchdog.msg', '!!! WARNING - THIS USER ENTERED AN INVALID PASSWORD !!!');
              {$ENDIF}
            End; { PwdBoard (WATCHDOG!) }

        if DisplayHotFile('BADPWD', []) <> #01 then
          InputObj^.PressEnter(True, True);

        New(QuoteText, Init(MaxMsgLines));

        If GlobalCfg^.RaConfig^.BadPwdArea>00 then
            If InputObj^.ralStrYesNoAsk(ralAskMsg) then
               WriteMessage(Ra250MsgArea(GlobalCfg^.RaConfig^.BadPwdArea),
                             GlobalCfg^.RaConfig^.Sysop,
                             LineCfg^.Exitinfo^.Userinfo.Name,
                             '',
                             '',
                             False,     { reply }
                             false,     { edit }
                             QuoteText^,
                             00,
                             MsgNr,
                             True,
                             False,
                             '',
                             GetAreaAddress(Ra250MsgArea(GlobalCfg^.RaConfig^.BadPwdArea)),
                             '',
                             '', '', false, false, '', '', '');
        Dispose(QuoteText, Done);

        HangUp;
        Exit;
      End; { If Counter }

    if TempPassword = '' then
      begin
        Write('`A', MakeAttr(GlobalCfg^.RaConfig^.NormFore, GlobalCfg^.RaConfig^.NormBack), ':');
        Write(LangObj^.ralGet(ralPassword));
        GetString(TempPassWord, 15, [#32..#254], True, True, False, False);

        if Trim(TempPassword)='' then
         if QuitOnEmpty then EXIT;
      end; { if }

    if (ProgTerminated) then EXIT;
    WriteLn('`A7:');

    WrongPassWord := (RaCrc(TempPassWord, true)<>LineCfg^.Exitinfo^.Userinfo.PasswordCRC);

    if NOT WrongPassword then
      if GlobalCfg^.RaConfig^.SavePasswords then
       LineCfg^.Exitinfo^.Userinfo.Password := SUpCase(Trim(TempPassword));

    If WrongPassword then
     begin
       WriteLn;

       If NOT ((Counter+1)>GlobalCfg^.RaConfig^.PassWordTries) then
        Writeln(LangObj^.ralGet(ralIncPsw));

       RaLog('!', 'Incorrect password : "'+TempPassWord+'"');
       Writeln;
     End; { TempPassWord }
    TempPassWord := '';

  Until (NOT WrongPassWord);
  QuitOnEmpty := false;
end; { proc. GetUsersPassword }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function PerformNewUserLogon: Byte;

function IEMSI_NewUser: Boolean;
begin
  IEMSI_NewUser := FALSE;

  if LineCfg^.Exitinfo^.EMSI_Session then
   If GlobalCfg^.RaConfig^.Emsi_NewUser then IEMSI_NewUser := TRUE;
end; { func. IEMSI_NewUser }

procedure IEMSI_SetUser(Srch: String; CrtDef: String; var Attrib: Byte; C: Byte);
begin
  If (IEMSI_NewUser) then
   if (Pos(Srch, CrtDef) > 00) then
    SetBit(Attrib, C)
     else ClearBit(Attrib, C);
end; { proc. IEMSI_Newuser }

var TempL  : Longint;
    Temp   : Word;
    Year,
    Month,
    Day    : Word;
    DateFMT: Byte;
    TempStr: String;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logLogon, 'PerformNewUserlogon');
  {$ENDIF}

  PerformNewUserLogon := 01; { Not correct }

  DisplayHotFile('NOTFOUND', []);
  WriteLn;
  Writeln(LangObj^.ralGet(ralNotFound1));
  Writeln;
  Writeln(LangObj^.ralGet(ralNmEntered), LineCfg^.Exitinfo^.Userinfo.Name, '.');
  WriteLn;

  If InputObj^.ralStrYesNoAsk(ralAskNmOk) then
   begin;
     if (ProgTerminated) then EXIT;
     WriteLn;
     If GlobalCfg^.RaConfig^.NewSecurity=00 then
        begin;
           If DisplayHotFile('Private', [])=#01 then
             WriteLn(LangObj^.ralGet(ralPrivSys));
           hangup;
        end; { private system }

     If SearchCtlFile('trashcan.ctl', LineCfg^.Exitinfo^.Userinfo.Name, False) then
      begin
        if DisplayHotFile('trashcan', []) = #01 then
          begin;
            WriteLn('`A12:');
            Writeln(LangObj^.ralGet(ralInvName));
          end; { if }

        RaLog('!', 'Name entered is unacceptable, Trashcan message given');

        HangUp;
      end; { SearchCtlFile }

   End { If Entered Name OK }
    else Exit;

  LineCfg^.DispMorePrompt := true;
  LineCfg^.ShowStatusBar := true;
  if defStatusLine <> 10 then Statusdisplay(1, false);
  DisplayHotFile('NEWUSER1', []);

  WriteLn;
  WriteLn;

  (* Enable ANSI ? *)
  If (Byte(GlobalCfg^.RaConfig^.ANSI)=Byte(Ask)) AND (NOT IEMSI_NewUser) then
   begin;
     if (FixBaud(LineCfg^.Exitinfo^.Baud) >= GlobalCfg^.RaConfig^.GraphicsBaud) OR (LineCfg^.Exitinfo^.Baud=00) then
       begin;
        If InputObj^.ralStrYesNoAsk(ralAskAnsi) then
         SetBit(LineCfg^.Exitinfo^.Userinfo.Attribute, 3)
          else ClearBit(LineCfg^.Exitinfo^.Userinfo.Attribute, 3);
       end
        else ClearBit(LineCfg^.Exitinfo^.Userinfo.Attribute, 3);
     WriteLn;
   End; { ANSI }
  IEMSI_SetUser('ANSI', LineCfg^.EMSI_User^.CrtDef, LineCfg^.Exitinfo^.Userinfo.Attribute, 3);

  LineCfg^.AnsiOn := ReadBit(LineCfg^.Exitinfo^.UserInfo.Attribute, 3);

  (* Enable AVATAR ? *)
  If (Byte(GlobalCfg^.RaConfig^.Avatar)=Byte(Ask)) AND (NOT IEMSI_NewUser) then
   begin;
     if (FixBaud(LineCfg^.Exitinfo^.Baud) >= GlobalCfg^.RaConfig^.GraphicsBaud) OR (LineCfg^.Exitinfo^.Baud=00) then
       begin;
         If InputObj^.ralStrYesNoAsk(ralAskAvt) then
          SetBit(LineCfg^.Exitinfo^.Userinfo.Attribute2, 1)
           else ClearBit(LineCfg^.Exitinfo^.Userinfo.Attribute2, 1);
       end
        else ClearBit(LineCfg^.Exitinfo^.Userinfo.Attribute2, 1);
     WriteLn;
   end; { Avatar }
  IEMSI_SetUser('AVT0', LineCfg^.EMSI_User^.CrtDef, LineCfg^.Exitinfo^.Userinfo.Attribute2, 1);
  if (IEMSI_NewUser) AND (ReadBit(LineCfg^.Exitinfo^.Userinfo.Attribute2, 1)) then
    SetBit(LineCfg^.Exitinfo^.Userinfo.Attribute, 3);

  LineCfg^.AvatarOn := ReadBit(LineCfg^.Exitinfo^.UserInfo.Attribute2, 1);

  (*
  { Enable HotKeys ? }
  If (Byte(GlobalCfg^.RaConfig^.HotKeys)=Byte(Ask)) AND (NOT IEMSI_NewUser) then
   begin;
     If InputObj^.ralStrYesNoAsk(ralAskHotkey) then
      SetBit(LineCfg^.Exitinfo^.Userinfo.Attribute2, 0)
        else ClearBit(LineCfg^.Exitinfo^.Userinfo.Attribute2, 0);
     WriteLn;
   end; { HotKeys }
  IEMSI_SetUser('HOT', EMSI_User^.Requests, LineCfg^.Exitinfo^.Userinfo.Attribute2, 0);
  *)

  (* Enable Fullscreen message reader *)
  If (Byte(GlobalCfg^.RaConfig^.FullMsgView)=Byte(Ask)) then
   begin
     If InputObj^.ralStrYesNoAsk(ralAskFSView) then
       SetBit(LineCfg^.Exitinfo^.Userinfo.Attribute2, 2)
        else ClearBit(LineCfg^.Exitinfo^.Userinfo.Attribute2, 2);
     WriteLn;
   end; { FullMsgView }

  (* Screen Length *)
  TempL := LineCfg^.Exitinfo^.Userinfo.ScreenLength;
  RangeEdit(TempL, LangObj^.RalGet(ralAskLines), 10, 66, 24);
  LineCfg^.Exitinfo^.Userinfo.ScreenLength := Byte(Templ);
  WriteLn;

  (* Enable PagePausing *)
  If (Byte(GlobalCfg^.RaConfig^.MorePrompt)=Byte(Ask)) AND (NOT IEMSI_NewUser) then
   begin;
     Write('`A14:');
     If InputObj^.ralStrYesNoAsk(ralAskPause) then
       SetBit(LineCfg^.Exitinfo^.Userinfo.Attribute, 2)
        else ClearBit(LineCfg^.Exitinfo^.Userinfo.Attribute, 2);
     WriteLn;
   end; { MorePrompt }
  IEMSI_SetUser('MORE', LineCfg^.EMSI_User^.Requests, LineCfg^.Exitinfo^.Userinfo.Attribute, 2);

  (* Enable Clearscreen codes *)
  If (Byte(GlobalCfg^.RaConfig^.ClearScreen)=Byte(Ask)) AND (NOT IEMSI_NewUser) then
   begin
     Write('`A13:');
     If InputObj^.ralStrYesNoAsk(ralAskClr) then
      SetBit(LineCfg^.Exitinfo^.Userinfo.Attribute, 1)
        else ClearBit(LineCfg^.Exitinfo^.Userinfo.Attribute, 1);
     WriteLn;
   end; { ClearScreen }
  IEMSI_SetUser('CLR', LineCfg^.EMSI_User^.Requests, LineCfg^.Exitinfo^.Userinfo.Attribute, 1);

  (* Users Location *)
  if (NOT IEMSI_NewUser) then
   begin
     MustEdit(LineCfg^.Exitinfo^.Userinfo.Location,LangObj^.RalGet(ralAskLoc), 25,
              GlobalCfg^.RaConfig^.CapLocation, True, 14, 0, [#32..#254], 01);
     WriteLn;
   end else LineCfg^.Exitinfo^.Userinfo.Location := LineCfg^.EMSI_User^.Location;

  if defStatusLine <> 10 then Statusdisplay(1, false);
  (* Users Mail Address *)
  If GlobalCfg^.RaConfig^.AskForAddress then
   AskForMailAddress(True);

  (* Users sex *)
  If GlobalCfg^.RaConfig^.AskForSex then
   begin
     Writeln;
     AskForUsersSex;
     WriteLn;
   end; { AskForSex (...) }

  (* Enable EchoMail in MailBox-Check ? *)
  If (Byte(GlobalCfg^.RaConfig^.EchoCheck)=Byte(Ask)) then
   begin
     If InputObj^.ralStrYesNoAsk(ralShowEcho) then
      ClearBit(LineCfg^.Exitinfo^.Userinfo.Attribute2, 5)
       else ClearBit(LineCfg^.Exitinfo^.Userinfo.Attribute2, 5);
     WriteLn;
   end; { EchoCheck }

  (* Users VoicePhone *)
  If (GlobalCfg^.RaConfig^.AskvoicePhone) AND (NOT IEMSI_NewUser) then
   GetPhoneNumber(LineCfg^.Exitinfo^.Userinfo.VoicePhone, ralAskvoice);
  if IEMSI_NewUser then
      begin
        LineCfg^.Exitinfo^.Userinfo.VoicePhone := Trim(LineCfg^.EMSI_User^.VoiceNr);
        if (LineCfg^.Exitinfo^.Userinfo.VoicePhone = '') OR (NOT ValidPhone(LineCfg^.Exitinfo^.Userinfo.VoicePhone)) then
           GetPhoneNumber(LineCfg^.Exitinfo^.Userinfo.VoicePhone, ralAskvoice);
      end; { if }

  (* Users DataPhone *)
  If (GlobalCfg^.RaConfig^.AskDataPhone) AND (NOT IEMSI_NewUser) then
   GetPhoneNumber(LineCfg^.Exitinfo^.Userinfo.DataPhone, ralAskData);
  if IEMSI_NewUser then
      begin
        LineCfg^.Exitinfo^.Userinfo.DataPhone := Trim(LineCfg^.EMSI_User^.DataNr);
        if (LineCfg^.Exitinfo^.Userinfo.DataPhone = '') OR (NOT ValidPhone(LineCfg^.Exitinfo^.Userinfo.DataPhone)) then
           GetPhoneNumber(LineCfg^.Exitinfo^.Userinfo.DataPhone, ralAskData);
      end; { if }


  (* Users Handle *)
  If (GlobalCfg^.RaConfig^.AskForHandle) AND (NOT IEMSI_NewUser) then
   ChangeHandle('');
  if IEMSI_NewUser then
    begin
      if NOT ValidHandle(LineCfg^.Emsi_User^.Alias) then ChangeHandle('')
        else LineCfg^.Exitinfo^.Userinfo.Handle := LineCfg^.EMSI_User^.Alias;
    end; { if }
  if (LineCfg^.Exitinfo^.Userinfo.Handle='') then
    LineCfg^.Exitinfo^.Userinfo.Handle := LineCfg^.Exitinfo^.Userinfo.Name;

  if (ProgTerminated) then EXIT;

  (* Ask users preferred date format *)
  if GlobalCfg^.RaConfig^.NewUserDateFormat=00 then
   begin
     AskDateFormat;
     WriteLn;
   end; { AskDateFormat }

  (* Get users Birthdate *)
  If (GlobalCfg^.RaConfig^.AskForBirthDate) AND (NOT IEMSI_NewUser) then
    begin
      TempStr := LineCfg^.Exitinfo^.Userinfo.BirthDate;
      GetDateStr(TempStr, False, '`A3:' + LangObj^.ralGet(ralAskBirth) + #32, true,false);
      LineCfg^.Exitinfo^.Userinfo.BirthDate := TempStr;
    end; { if }

  if (IEMSI_NewUser) then
   begin
     Unix2Norm(FVal('$'+LineCfg^.EMSI_User^.BirthDate), Year, Month, Day, Temp, Temp, Temp);
     LineCfg^.Exitinfo^.Userinfo.BirthDate := LeadingZero(Month, 2) + '-' + LeadingZero(Day, 2)+'-'+
                Copy(LeadingZero(Year, 4), 2, 2);

     DateFmt := 01;
     Case LineCfg^.Exitinfo^.Userinfo.DateFormat of
       { DD-MM-YY }    01 : DateFMT := 01;
       { MM-DD-YY }    02 : DateFMT := 02;
       { YY-MM-DD }    03 : DateFMT := 03;
       { DD-Mmm-YY }   04 : DateFMT := 01;
       { DD-MM-YYYY }  05 : DateFMT := 01;
       { MM-DD-YYYY }  06 : DateFMT := 02;
       { YYYY-MM-DD }  07 : DateFMT := 03;
       { DD-Mmm-YYYY } 08 : DateFMT := 01;
     end; { case }

     if NOT ValidBirthDate(LineCfg^.Exitinfo^.Userinfo.BirthDate, DateFMT, false, Month, Day, Year) then
       begin
         TempStr := LineCfg^.Exitinfo^.Userinfo.BirthDate;
         GetDateStr(TempStr, false, '`A3:' + LangObj^.ralGet(ralAskBirth) + #32, true,false);
         LineCfg^.Exitinfo^.Userinfo.BirthDate := TempStr;
       end; { if }

   end; { if }

  LineCfg^.Exitinfo^.Userinfo.PasswordCRC := -1;
  (* Get New PassWord *)
  if (NOT IEMSI_NewUser) then GetNewPassword(True, False)
    else begin
           If GlobalCfg^.RaConfig^.SavePassWords then LineCfg^.Exitinfo^.Userinfo.PassWord := LineCfg^.EMSI_User^.Password
            else LineCfg^.Exitinfo^.Userinfo.Password := '';
           LineCfg^.Exitinfo^.Userinfo.PassWordCRC := RaCrc(LineCfg^.EMSI_User^.Password, true);
         end; { if }

  PerformNewUserLogon := 02; { Correct }
  GetUserAge(LineCfg^.Exitinfo^.Userinfo.BirthDate, true); { Update saved users' age }
end; { proc. PerformNewUserLogon }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure PerformLogon;

Label LabelGetUserName;

var SaveElapsed: Longint;
    SaveLTime  : Longint;
    UserRecord : Longint;
    IsFastLogon: Boolean;
    PswBool    : Boolean;
    Quest      : ^QuestObj;
    elxObj     : pElxBbsObj;
    CH         : Char;
    TempPSW    : String;
begin
 {$IFDEF WITH_FULL}
  {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logLogon, 'Performlogon');
  {$ENDIF}

  {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logLogon, 'Performlogon (0.5)');
  {$ENDIF}


  StatusDisplay(defStatusLine, false);

  {$IFDEF NOLINLOCAL}
    LineCfg^.AnsiOn := TRUE;       { EleBBS cannot survive without ANSI codes }
        { in the Unix variant }
  {$ENDIF}

  {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logLogon, 'Performlogon (1)');
  {$ENDIF}
  LineCfg^.UserExtensionSize := usrext_GetSize;

  SetNewUserDefaults(LineCfg^.Exitinfo^.Userinfo,
                     LineCfg^.UserExtension^);    { Set all newuser defaults }
  If LineCfg^.LocalLogon then LineCfg^.AnsiOn := true;
  contrObj^.SetIdleTimeLimit(GlobalCfg^.RaConfig^.UserTimeOut);
  LineCfg^.Exitinfo^.TimeLimit := GlobalCfg^.RaConfig^.LogonTime;
  LineCfg^.LimitsInfo^.LTime := GlobalCfg^.RaConfig^.LogonTime;
  LineCfg^.Exitinfo^.Userinfo.Elapsed := 00;
  LineCfg^.DispMorePrompt := False;   { Else a (Y/n/=)? prompt will be given at LOGO.* }

  {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logLogon, 'Performlogon (2)');
  {$ENDIF}


  If (GlobalCfg^.RaConfig^.AutoDetectANSI) AND (NOT LineCfg^.LocalLogon) then
    AutoDetectAnsi;

  {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logLogon, 'Performlogon (3)');
  {$ENDIF}

  StatusDisplay(10, false);                { Temporarly disable statusline }
  OutputObj^.ClearScreen;

  ReadSysInfoBBS(LineCfg^.Exitinfo^.Sysinfo);

  Write('`A7:');

  contrObj^.SetTimeOut;                      { Reset the inactivity timer }
  DisplayHotFile('DUMMY', []);

  {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logLogon, 'Performlogon (3)');
  {$ENDIF}

  if (LineCfg^.Exitinfo^.Baud>00) then
    DisplayHotFile('LOGO', []);             { Display LOGO.ANS/ASC/AVT }

  If (LineCfg^.Exitinfo^.Baud=00) then
   If NOT GlobalCfg^.RaConfig^.FastLogon then
    DisplayHotFile('LOGO', []);             { Display LOGO.ANS/ASC/AVT }

  if (ProgTerminated) then EXIT;

  LineCfg^.DispMorePrompt := False;                    { Can be reset by LOGO.ANS }
  RaLog(' ', '');                                                { Empty line }
  Write('`A', MakeAttr(GlobalCfg^.RaConfig^.NormFore, GlobalCfg^.RaConfig^.NormBack), ':');

  If LineCfg^.Exitinfo^.Baud>00 then
   If LineCfg^.Exitinfo^.Baud=300 then
    If CheckTimeRange(GlobalCfg^.RaConfig^.SlowBaudTimeStart, GlobalCfg^.RaConfig^.SlowBaudTimeStart, false) then
       begin
         RaLog('!', 'Connect speed too slow for this timewindow, user disconnected ('+
           FStr(FixBaud(LineCfg^.Exitinfo^.Baud))+' BPS)');

         if DisplayHotFile('NO300', []) = #01 then
           WriteLn(LangObj^.ralGet(ralNo300));
         HangUp;
       end; { TooSlow }

  if (ProgTerminated) then EXIT;

  If LineCfg^.Exitinfo^.Baud>00 then
   If FixBaud(LineCfg^.Exitinfo^.Baud)<GlobalCfg^.RaConfig^.MinimumBaud then
       begin
         RaLog('!', 'Connect speed too slow, user disconnected ('+FStr(FixBaud(LineCfg^.Exitinfo^.Baud))+' BPS)');

         if DisplayHotFile('TOOSLOW', []) = #01 then
           WriteLn(LangObj^.ralGet(ralTooSlow));
         HangUp;
       end; { TooSlow }

  WriteLn;
  LineCfg^.PauseChecking := false; { Disable PauseChecking else the buffer eats EMSI }
  StartIEMSI_Session;                 { Indicate to remote that we have IEMSI }
  Write('`X1:');                { Position the cursor at the beginning because }
                                  { this StartIEMSI_Session directly writes to }
                                  { the remote thus messing up the interaction }

  WriteLn(PidName + RegAddStrFull);
  IsFastLogon := false;
  if (LineCfg^.Exitinfo^.Baud=00) AND (GlobalCfg^.RaConfig^.FastLogon) then
    IsFastLogon := true;

  Repeat;
LabelGetUsername:

    if (ProgTerminated) then EXIT;
    SetNewUserDefaults(LineCfg^.Exitinfo^.Userinfo,
                       LineCfg^.UserExtension^);         { Set all newuser defaults }

    If LineCfg^.Exitinfo^.Baud>00 then GetUserName(TempPSW)
      else begin
             if NOT IsFastLogon then GetUserName(TempPSW)
               else LineCfg^.Exitinfo^.Userinfo.Name := GlobalCfg^.RaConfig^.SysOp;
           end; { FastLogon }

    if (ProgTerminated) then EXIT;
    LineCfg^.PauseChecking := true;

    Writeln;
    Write(LangObj^.RalGet(ralScanUsers));
    OutBlockObj^.DumpBlock;

    UserRecord := SearchUser(LineCfg^.Exitinfo^.Userinfo.Name);

    If UserRecord>-1 then
      begin;
         GetUserRecord(UserRecord, LineCfg^.Exitinfo^.Userinfo,
                       LineCfg^.UserExtension^, true);
         LineCfg^.Exitinfo^.UserRecord := UserRecord;

         If LineCfg^.Exitinfo^.Baud>00 then
          If NOT GlobalCfg^.RaConfig^.AllowSysRem then
           If (SUpcase(LineCfg^.Exitinfo^.UserInfo.Name)=SUpcase(GlobalCfg^.RaConfig^.SysOp)) OR
              (SUpcase(LineCfg^.Exitinfo^.Userinfo.Handle)=SUpcase(GlobalCfg^.RaConfig^.Sysop)) then
                begin
                  WriteLn;
                  WriteLn(LangObj^.ralGet(ralNoSysOp));
                  WriteLn;
                  HangUp;
                end; { RemoteAccess has been disbaled }
      end
      else begin;
            LineCfg^.Exitinfo^.Userinfo.Security := GlobalCfg^.RaConfig^.NewSecurity;
            LineCfg^.Exitinfo^.Userinfo.Language := 01;

            If NOT GlobalCfg^.RaConfig^.OneWordNames then
             If Pos(#32, Trim(LineCfg^.Exitinfo^.Userinfo.Name))=00 then
              begin
                WriteLn;
                WriteLn;
                Write(LangObj^.ralGet(ralBothNames));
                LineCfg^.Exitinfo^.Userinfo.Name := '';
                UserRecord := -1;
              end; { One name }

           End; { getUserRecord }

    if LineCfg^.Exitinfo^.Userinfo.Name <> '' then
      begin
        if LineCfg^.Exitinfo^.Baud = 65529 then                   { telnet }
          begin
            RaLog('>', LineCfg^.Exitinfo^.Userinfo.Name+' on-line using telnet (' + LineCfg^.TelnetFromIp + ')');
          end
            else RaLog('>', LineCfg^.Exitinfo^.Userinfo.Name+' on-line at '+FStr(FixBaud(LineCfg^.Exitinfo^.Baud))+' BPS');
      end; { if }

    {$IFDEF WINGUI}
      Application.Title := 'EleBBS - '+LineCfg^.Exitinfo^.Userinfo.Name + ' (node '+FStr(RaNodeNr) + ')';
      Form1.Caption := 'EleBBS - '+LineCfg^.Exitinfo^.Userinfo.Name + ' (node '+FStr(RaNodeNr) + ')';;
    {$ENDIF}

    InitUserSettings(true);
    SaveElapsed := LineCfg^.Exitinfo^.Userinfo.Elapsed;
    SaveLTime := LineCfg^.LimitsInfo^.lTime;
    LineCfg^.Exitinfo^.TimeLimit := GlobalCfg^.RaConfig^.LogonTime;
    LineCfg^.LimitsInfo^.lTime := GlobalCfg^.RaConfig^.LogonTime;
    LineCfg^.Exitinfo^.Userinfo.Elapsed := 0;
    ChangeTime(0, true);

    if defStatusLine <> 10 then StatusDisplay(DefStatusLine + 10, false);
    WriteLn;

    If LineCfg^.Exitinfo^.Userinfo.Name<>'' then
     If UserRecord=-1 then
     begin;
       RaLog('!', 'Name not in user file');

       if GlobalCfg^.RaConfig^.NewUserLanguage <> 00 then
        Selectlanguage(True)
         else begin
                LoadDefaultLanguage;
                LineCfg^.Exitinfo^.Userinfo.Language := 01;
              end; { if }

       Repeat;
         Case PerformNewUserLogon of
{ Wrong name }      01 : begin
                            writeln;
                            Goto LabelGetUserName;
                         end; { Wrong Name }
{ Correct }         02 : ;
         End; { case }

         Writeln('`A15:');
         WriteLn;
       Until (InputObj^.ralStrYesNoAsk(ralAskInfOK)) OR ((ProgTerminated));
       WriteLn;

       if (ProgTerminated) then EXIT;

       (* Scan for duplicate and/or invalid telephone number *)
       ScanPhoneNumbers;
       DisplayHotFile('NEWUSER2', []);

       {-- run an Q-A or EleXer script -------------------------------------}
       if GetScriptType('NEWUSER') = 'Q-A' then
         begin
           New(Quest, Init);
             Quest^.Process('NEWUSER', true, '');
           Dispose(Quest, Done);
         end
           else begin
                  New(elxObj, Init);
                  elxObj^.RunElexerScript('newuser', '', false);
                  Dispose(elxObj, Done);
                end; { else }

       usrext_SetDefaults(LineCfg^.UserExtension^);
       AddToUserBase;
       UserRecord := SearchUser(LineCfg^.Exitinfo^.Userinfo.Name);

       if FileExist(OpenRaFile('newuser.msg')) then
         begin
           {$IFDEF WITH_FULL}
             FilePost(Ra250MsgArea(GlobalCfg^.RaConfig^.PwdBoard),
                      GlobalCfg^.RaConfig^.Sysop,
                      LineCfg^.Exitinfo^.Userinfo.Name,
                      LangObj^.ralGet(ralWlcmSubj),
                      'newuser.msg',
                      '');
           {$ENDIF}
         end; { if }

       RaLog('>', 'Completed new-user procedure');
     end { user not found }
      else begin
             PswBool := IsFastLogon;
             GetUsersPassword(TempPSW, PswBool);

             if PswBool then
              if IsFastLogon then
               begin;
                 LineCfg^.Exitinfo^.Userinfo.Name := '';
                 IsFastLogon := false;
               end; { if }
            end; { Else ask users password }
    WriteLn;
    IsFastLogon := false;

  Until (UserRecord>-1) AND (LineCfg^.Exitinfo^.Userinfo.Name<>'');

  If LineCfg^.Exitinfo^.Baud>00 then
    If FixBaud(LineCfg^.Exitinfo^.Baud) < GlobalCfg^.RaConfig^.GraphicsBaud then
          begin
            LineCfg^.AnsiOn := False;
            LineCfg^.AvatarOn := False;
          end; { Slower than GraphicsBaud }


  LineCfg^.ShowStatusBar := TRUE;
  if defStatusLine <> 10 then StatusDisplay(DefStatusLine + 10, false);

  If LineCfg^.Exitinfo^.Userinfo.Security=00 then
   begin;
     If DisplayHotFile('LOCKOUT', [])=#01 then
        begin
         WriteLn;
         WriteLn(LangObj^.ralGet(ralSuspended));
        end;
     Ralog('!', 'User disconnected after lockout message');
     HangUp;
   end; { Locked Out }

  If GlobalCfg^.RaConfig^.CheckForMultiLogon then
   If MultiLnObj^.DoubleUseron(LineCfg^.Exitinfo^.Userinfo.Name) then
    if (NOT LineCfg^.GuestUser) then
     begin
       RaLog('!', 'Attempted to log on to multiple lines');

       if DisplayHotFile('1ATATIME', []) = #01 then
         begin
           WriteLn;
           WriteLn(langObj^.ralGet(ral1AtAtime));
           WriteLn;
         end; { if }
       HangUp;
     end; { One at a time }

  LineCfg^.Exitinfo^.UserRecord := UserRecord;
  LineCfg^.Exitinfo^.Userinfo.Elapsed := SaveElapsed;
  LineCfg^.LimitsInfo^.lTime := SaveLtime;
  LineCfg^.Exitinfo^.TimeLimit := Smallword((LineCfg^.Limitsinfo^.lTime - LineCfg^.Exitinfo^.Userinfo.Elapsed));
  GetLevelLimitsInfo(LineCfg^.Exitinfo^.Userinfo.Security, False, true);
  GetExitinfoInfo(LineCfg^.Exitinfo^);

  if LineCfg^.GuestUser then
    begin
      LineCfg^.Exitinfo^.Userinfo.Elapsed := 00;
      LineCfg^.Exitinfo^.Userinfo.Downloads := 00;
      LineCfg^.Exitinfo^.Userinfo.DownloadsK := 00;
      LineCfg^.Exitinfo^.Userinfo.Uploads := 00;
      LineCfg^.Exitinfo^.Userinfo.UploadsK := 00;
      LineCfg^.Exitinfo^.Userinfo.TodayK := 00;
      ChangeTime(0, true);
    end; { if GuestUser }

  DoEventWarning(LineCfg^.Exitinfo^); { Important! Now the next event is just read! }

  if LineCfg^.Exitinfo^.Userinfo.Language = 0 then
    Selectlanguage(TRUE);

  if defStatusLine <> 10 then StatusDisplay(DefStatusLine + 10, false);
  SetWindowTitle('EleBBS - Node #'+FStr(LineCfg^.RaNodeNr) + ' - ' +LineCfg^.Exitinfo^.Userinfo.Name);

  if (ProgTerminated) then EXIT;
  ShowEventWarning(LineCfg^.Exitinfo^);
  GetUserAge(LineCfg^.Exitinfo^.Userinfo.Birthdate, true);


  If SearchCtlFile('vip.ctl', LineCfg^.Exitinfo^.Userinfo.Name, False) then
        begin;
          DisplayHotFile('VIP', []);
         {$Ifndef WinGUI}
         {$IFNDEF VirtualPascal}
          Sound(500); Support.Delay(15);
          Sound(100); Support.Delay(25);
          Sound(500); Support.Delay(15);
          NoSound;
         {$ELSE - VP}
           PlaySound(500, 15);
           PlaySound(100, 25);
           PlaySound(500, 25);
         {$ENDIF}
         {$ELse - WINGUI}
         {$Endif}
        End; { SearchCtlFile }

  MultiLnObj^.WriteUserOn('', uonBrowsing);
  if (ProgTerminated) then EXIT;
 {$ENDIF}
end; { proc. PerformLogon }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF ELEBBS}
procedure ScanPhoneNumbers;          { Scan for duplicate or invalid phone#s }
var User_F  : pFileObj;
    UserInfo: UsersRecord;
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logLogon, 'Scanphonenumber');{$endif}
  If NOT GlobalCfg^.RaConfig^.NewPhoneScan then EXIT;

  WriteLn;
  WriteLn('`A11:',LangObj^.ralGet(ralScanPhone));

  If SearchCtlFile('phonenum.ctl', LineCfg^.Exitinfo^.Userinfo.VoicePhone, False) then
        begin
          WriteLn('`A12:', LangObj^.ralget(ralInvVoice));  { Invalid }
          RaLog('!', 'Entered an unacceptable Home/Voice phone number');
        end; { invalid phonennumber }

  New(User_F, Init);
  User_F^.Assign(GlobalCfg^.RaConfig^.MsgBasePath + UserBaseName);
  if NOT User_F^.Open(1) then
    begin
      Dispose(User_F, Done);
      EXIT;
    end; { if }

  While NOT User_F^.EOF  do
    begin
      User_F^.BlkRead(UserInfo, SizeOf(UsersRecord));
      if User_F^.IoResult > 00 then BREAK;

      if Userinfo.VoicePhone=LineCfg^.Exitinfo^.Userinfo.VoicePhone then
       if Userinfo.VoicePhone <> '' then
        begin
          WriteLn('`A12:', LangObj^.ralget(ralInvData));  { Duplicate }
          RaLog('!', 'Entered the same Home/Voice phone number as '+UserInfo.Name);


        end; { if }
    end; { While }

  Dispose(User_F, Done);
end; { proc. ScanPhoneNumber }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)
{$ENDIF}

end.
