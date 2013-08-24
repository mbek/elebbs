unit Main;
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
** MAIN routines for EleBBS
**
** Copyright (c) 1996 by Maarten Bekers
**
** Created : 08-Sep-1996
** Last update : 01-Oct-1997
**
** ** Completely revised filesystem at 28-Jul-1997
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF WITH_FULL}
  This unit shouldn''t be compiled for an utility program
{$ENDIF}

{$IFDEF ISCGI}
  This unit should not be compiled into EleWEB
{$ENDIF}

type test=string[1];

{$IFDEF WITH_FULL}
procedure LoadDefaultLanguage;
procedure Start_This_Program;                    { Both EleBBS/DOS and /WIn32 }
procedure InitScreen;                                      { Init screen code }
procedure InitVariables;
procedure StartOpening;                { Open .RA files and show start screen }
procedure WelcomeInformation;    { Shows WELCOME screens, Dob/Mail check, etc }
procedure SelectLanguage(ShowMenu: Boolean);
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF WINGUI}
Uses Crt, Dos, Global, FileRout, StUtils, EleMenu, Logon, WinTitle, Ranges,
     FileObj, ScrnU, Execfunc, ScrList,
     {$IFNDEF WINGUI}Strings,{$ENDIF}
      Support, Ral, FileSys,
 StrEdit,
        EMSI, Mail, MenuFunc, JDates, Question, InOut_U,
         Crc_Unit, Debug_U,  Access_U, CurTime,
          UnixDate, ElLog_U, LineEd, DispAns, Control, Multiln,
           MailInt, BitWise, Mnuser, StatusB, ListFile, RemScrn,
            Cases, StrPath, LongStr, CfgRec, CentrStr, MemMan, ExitProg,
             TagUnit, GenFile, ReadCfg, cfgfile, Input_U, GenDos, FastScrn,
              RemSup, elx_BBS, usrExt, ObjDec
              {$IFNDEF OS2}
                ,Multi
              {$ENDIF};
{$ELSE}
Uses Global, Support, MenuFunc, FileSys, Mail, Win_Main, SysUtils, JDates, MkFile, Emsi, FileRout, Question, StUtils,
      Ral, InOut_U, Crc_Unit, Debug_U,  Access_U, UnixDate, Forms, GenDos,
       ElLog_U, LineEd, DispAns, Control, MailInt, Input_U, Ranges, CurTime,
        BitWise, Mnuser, StatusB, {$IFNDEF VirtualPascal}Use32,{$ENDIF} ListFile, Cases, StrPath, LongStr,
         CfgRec, CentrStr, MemMan, Exitprog, tagUnit, GenFile, WinTitle,
          FileObj, FastScrn, RemScrn, ScrnU, MultiLn, RemSup,
          ReadCfg, CfgFile, EleMenu, Logon {$IFNDEF WINGUI},Strings{$ENDIF};
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 {$I OUTPUT.SUP}
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
{$IFNDEF WIN32}
procedure GetMultiTasker;
begin
 {$IFNDEF OS2}
  If GlobalCfg^.RaConfig^.MultiTasker<>00 then
   Case GlobalCfg^.RaConfig^.MultiTasker of
        01 : MultiTasker := NoTasker;
        02 : MultiTasker := DoubleDos;
        03 : MultiTasker := DesqView;
        04 : MultiTasker := NoTasker;
        07 : MultiTasker := WindowsTask;
        08 : MultiTasker := OS2;
   end; { Case }
 {$ELSE}
 {$ENDIF}
end; { proc. GetMultiTasker }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Start_This_Program;
var Counter: Byte;
    MsgPath: String;

    Year,
    Month,
    day,
    Dow: Word;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMain, 'Start_This_Program');
  {$ENDIF}

  {$IFDEF WITH_FULL}
    if GlobalCfg^.RaConfig^.SemPath <> '' then MsgPath := GlobalCfg^.RaConfig^.SemPath
       else MsgPath := GlobalCfg^.RaConfig^.SysPath;
    EraseFile(MsgPath + 'NODE'+FStr(LineCfg^.RaNodeNr)+'.RA');


   {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMain, 'After EraseFile');
   {$ENDIF}

   if NOT LineCfg^.ReLogOnBBS then LineCfg^.Exitinfo^.UserRecord := -1;
   contrObj^.SetStartTime;
   LineCfg^.TimeFrozen:= False;

   {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMain, 'After SetStartTime');
   {$ENDIF}

   DoRaBusy(false);

{$IFDEF WITH_DEBUG}
  {!!} DebugObj.DebugLog(logMain, 'After DoEleBusy');
{$ENDIF}


  if NOT LineCfg^.ReLogonBBS then
   begin
    PerformLogon;                     { Ask user's name and password etcetera }

    if NOT ProgTerminated then
      WelcomeInformation;  { Shows WELCOME screens, DOB and mail check etc }
   end
     else begin
            RaLog('>', 'Logging '+LineCfg^.Exitinfo^.Userinfo.Name+' back on-line');
            InitUserSettings(true);

            lineCfg^.AnsiOn := ReadBit(LineCfg^.Exitinfo^.UserInfo.Attribute, 3);
            lineCfg^.AvatarOn := ReadBit(LineCfg^.Exitinfo^.UserInfo.Attribute2, 1);
            LineCfg^.GuestUser := Readbit(LineCfg^.Exitinfo^.Userinfo.Attribute2, 6);
            if lineCfg^.RipOn then lineCfg^.AnsiOn := True;

            LineCfg^.ShowStatusBar := TRUE;
            StatusDisplay(DefStatusLine + 10, false);
          end; { if }

  if LineCfg^.LimitsInfo^.lTime = UnlimitedValue then
    begin
      RaLog('>', 'Security level '+FStr(LineCfg^.Exitinfo^.Userinfo.Security)+ ', '+
            'no time limit');
    end
     else begin
            RaLog('>', 'Security level '+FStr(LineCfg^.Exitinfo^.Userinfo.Security)+ ', '+
                   FStr(LineCfg^.Exitinfo^.TimeLimit)+' mins today')
          end; { if }


  LineCfg^.DispMoreprompt := True;
  LineCfg^.PauseChecking := true;


  {$IFDEF WINGUI}
    Form1.EditUserButton.Enabled := true;
    if NOT Application.Terminated then LoggedOn := True;
  {$ELSE}
    LineCfg^.LoggedOn := true;
  {$ENDIF}
  contrObj^.TimeInfo.InterNodeChecking := GlobalCfg^.RaConfig^.MultiLine;


  if NOT ProgTerminated then
    Enter_Menu;
{$ENDIF}
end; { proc. Start_This_Program }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure DoBirthdateCheck;                    { Check for correct birthdate }
var Temp  : String;
    Quest : ^QuestObj;
    elxObj: pElxBbsObj;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMain, 'DoBirthDateCheck');
  {$ENDIF}

  if (SupCase(Trim(GlobalCfg^.RaConfig^.SysOp)) = SUpCase(Trim(LineCfg^.Exitinfo^.UserInfo.Name)))
     AND (LineCfg^.Exitinfo^.Baud=00) then EXIT;

  if LineCfg^.GuestUser then EXIT;

   if GlobalCfg^.RaConfig^.CheckDob>00 then
    If LineCfg^.Exitinfo^.Userinfo.LastDOBcheck>= GlobalCfg^.RaConfig^.CheckDOB then
          begin
            if (LineCfg^.Exitinfo^.Userinfo.BirthDate = '') OR
                (Trim(LineCfg^.Exitinfo^.Userinfo.BirthDate) = '-  -') then
                  begin
                    Temp := LineCfg^.Exitinfo^.Userinfo.Birthdate;
                    GetDateStr(Temp, false, '`A3:' + LangObj^.ralGet(ralAskBirth) + #32, true,false);
                    LineCfg^.Exitinfo^.Userinfo.Birthdate := Temp;
                  end; { if }

            GetDateStr(Temp, True, '`A3:' + LangObj^.ralGet(ralAskBirth) + #32, true,false);
            If SUpCase(Temp) <> SUpcase(LineCfg^.Exitinfo^.Userinfo.Birthdate) then
              begin;
                RaLog('!', 'Failed date of birth security check');

                {$IFDEF WITH_FULL}
                  if GetScriptType('DOBCFAIL') = 'Q-A' then
                    begin
                      New(Quest, Init);
                        Quest^.Process('DOBCFAIL /N', true, '');
                      Dispose(Quest, Done);
                    end
                      else begin
                             New(elxObj, Init);
                             elxObj^.RunElexerScript('dobcfail', '', false);
                             Dispose(elxObj, Done);
                           end; { if }
                {$ENDIF}

                WriteLn('`A12:');
                WriteLn(LangObj^.ralGet(ralNoAccess));
                WriteLn;
                HangUp;
              end; { GetDateStr }

            LineCfg^.Exitinfo^.Userinfo.LastDobCheck := 00;
          end; { birthdate Expired }
end; { proc. DoBirthDateCheck }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoPasswordCheck;                       { Check for password-change }
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMain, 'DoPasswordCheck');
  {$ENDIF}

  if (SupCase(Trim(GlobalCfg^.RaConfig^.SysOp)) = SUpCase(Trim(LineCfg^.Exitinfo^.UserInfo.Name)))
      AND (LineCfg^.Exitinfo^.UserInfo.PasswordCRC <> -1) then EXIT;

   if ((GlobalCfg^.RaConfig^.PwdExpiry>00) AND (LineCfg^.Exitinfo^.Userinfo.LastPwdChange >= GlobalCfg^.RaConfig^.PwdExpiry)
       OR ((LineCfg^.Exitinfo^.Userinfo.PasswordCRC = -1))) AND (NOT LineCfg^.Guestuser) then
          begin
            RaLog('!', 'System forced a password change');

            If DisplayHotFile('EXPIRED', [])=#01 then
              begin
                OutputObj^.ClearScreen;
                Writeln('`A13:');
                Writeln('`A13:', LangObj^.ralGet(ralPswExp));
                WriteLn;
                WriteLn('`A12:', LangObj^.ralGet(ralPswChng1));
                Writeln('`A12:', GlobalCfg^.RaConfig^.PwdExpiry, #32, langObj^.ralGet(ralTimesCall),
                        #32, GlobalCfg^.RaConfig^.SystemName);
                Writeln('`A14:');
              end; { Expired }

            GetNewPassword(False, True);
            LineCfg^.Exitinfo^.Userinfo.LastPwdChange := 00;
          end; { PassWord Expired }
end; { proc. DoPasswordCheck }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure InitScreen;
begin
 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logMain, 'InitScreen (begin)');
 {$ENDIF}

 {$IFNDEF WIN32}
   GetMultiTasker;
 {$ENDIF}

 {$IFNDEF ISCGI}
   AssignOutput;
 {$ENDIF}

 OutputObj^.ClearScreen;
 lineCfg^.ProcessCharType := chrDisplay;

 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logMain, 'InitScreen ( end )');
 {$ENDIF}
end; { proc. InitScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure InitVariables;

procedure Error(S: String);
begin
 {$ifndef WinGUI}
  TextAttr := 15;
 {$else}
  Form1.ColorCOnsole1.TextAttr := 15;
 {$endif}
  WriteLn(#254, #32, 'Memory error occured.');
  WriteLn(#254, #32, S);
  Halt(defExitCode);
end; { proc. Error }

begin
 {$IFDEF WITH_FULL}
  {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'Initvariables');{$endif}
  {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'Initing LangObj');{$endif}


 if LangObj <> nil then
   if LangObj^.FailedInitialize then
     begin
       RaLog('!', 'Unable to initialize RAL buffer, program halted');
       {$IFDEF WITH_FULL}
         Halt(defExitCode);
       {$ENDIF}
     end; { if }

  {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'Initting output obj');{$endif}

 {$IFNDEF WINGUI}
  (*
   If InOut <> nil then
     While (OutputObj^.LocalKeypressed) AND (OutputObj^.Ok) do OutputObj^.LocalReadKey;
  *)
 {$ENDIF}

 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'Allocating memory for savearray');{$endif}
 AllocMem(LineCfg^.UploadInfo, SizeOf(UploadsRec), 'UploadsRec', 'InitVariables');
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'Allocating memory for exitinfo');{$endif}
 AllocMem(LineCfg^.Exitinfo, SizeOf(LineCfg^.Exitinfo^), 'Exitinfo', 'InitVariables');
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'Allocating memory for userExtension');{$endif}
 AllocMem(LineCfg^.UserExtension, SizeOf(LineCfg^.UserExtension^), 'UserExtnsion', 'InitVariables');
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'Allocating memory for SaveGuestInfo');{$endif}
 AllocMem(LineCfg^.SaveGuestRecord, SizeOf(LineCfg^.SaveGuestRecord^), 'SaveGuestRecord', 'InitVariables');
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'Allocating memory for raconfig');{$endif}
 AllocMem(GlobalCfg^.RaConfig, SizeOf(GlobalCfg^.RaConfig^), 'RaConfig', 'InitVariables');
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'Allocating memory for ELCONFIG');{$endif}
 AllocMem(GlobalCfg^.ElConfig, SizeOf(GlobalCfg^.ElConfig^), 'ElConfig', 'InitVariables');
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'Allocating memory for language');{$endif}
 AllocMem(LineCfg^.Language, SizeOf(LineCfg^.Language^), 'Language', 'InitVariables');
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'allocating memory for Modem');{$endif}
 AllocMem(LineCfg^.Modem, SizeOf(LineCfg^.Modem^), 'Modem', 'InitVariables');
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'allocating memory for termstr');{$endif}
 AllocMem(LineCfg^.LimitsInfo, SizeOf(LimitsRecord), 'LimitsInfo', 'InitVariables');
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'allocating memory for markedmsgarray');{$endif}
 AllocMem(lineCfg^.MarkedMsgArray, SizeOf(lineCfg^.MarkedMsgArray^), 'MarkedMsgArray', 'InitVariables');
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'allocating memory for curfilesrecord');{$endif}
 AllocMem(lineCfg^.CurFilesRecord, SizeOf(lineCfg^.CurFilesRecord^), 'CurFilesRecord', 'InitVariables');
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'allocating memory for curmessagerecord');{$endif}
 AllocMem(lineCfg^.CurMessageRecord, SizeOf(lineCfg^.CurMessageRecord^), 'CurMessageRecord', 'InitVariables');
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'allocating memory for CurEleMsgRecord');{$endif}
 AllocMem(lineCfg^.CurEleMsgRecord, SizeOf(lineCfg^.CurEleMsgRecord^), 'CurEleMsgRecord', 'InitVariables');
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'allocating memory for emsi record');{$endif}
 AllocMem(lineCfg^.Emsi_User, SizeOf(lineCfg^.Emsi_User^), 'Emsi_User', 'InitVariables');
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'Allocating memory for Telnet'); {$endif}
 AllocMem(LineCfg^.Telnet, SizeOf(LineCfg^.Telnet^), 'Telnet', 'InitVariables');    { Initialize RAL information array }
 {$IFNDEF MSDOS}
  {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'allocating memory for oldscreen');{$endif}
  AllocMem(OldScreen, SizeOf(OldScreen^), 'OldScreen', 'InitVariables');
 {$ENDIF}

 if LangObj=nil then Error('LangObj is not initialized!');
 if OutputObj=nil then Error('InOutObj is not initialized!');
 if InputObj=nil then Error('InputObj is not initialized!');
 if TaggingObj=nil then Error('TaggingObj is not initialized!');

 if LineCfg^.UploadInfo=nil then Error('Uploadsinfo is not initialized!');
 if LineCfg^.Exitinfo=nil then Error('Exitinfo is not initialized!');
 if LineCfg^.UserExtension=nil then Error('Userextension is not initialized');
 if LineCfg^.SaveGuestRecord=nil then Error('SaveGuestRecord is not initialized!');
 if GlobalCfg^.RaConfig=nil then Error('RaConfig is not initialized!');
 if GlobalCfg^.ElConfig=nil then Error('ELCONFIG is not initialized!');
 if LineCfg^.Language=nil then Error('Language is not initialized!');
 if LineCfg^.Modem=nil then Error('Modem is not initialized!');
 if LineCfg^.LimitsInfo=nil then Error('LimitsInfo is not initialized!');
 if lineCfg^.MarkedMsgArray=nil then Error('MarkedMsgArray is not initialized!');
 if lineCfg^.CurFilesRecord=nil then Error('CurFilesRecord is not initialized!');
 if lineCfg^.CurMessageRecord=nil then Error('CurMessageRecord is not initialized!');
 if lineCfg^.CurEleMsgRecord=nil then Error('CurEleMsgRecord is not initialized!');
 if lineCfg^.Emsi_User=nil then Error('EMSI_USER is not initialized!');
 {$IFNDEF MSDOS}
   if OldScreen=nil then Error('OldScreen is not initialized!');
 {$ENDIF}

 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'filling exitinfo');{$endif}
 FillChar(LineCfg^.Exitinfo^, SizeOf(ExitinfoRecord), #00);
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'filling userextension');{$endif}
 FillChar(LineCfg^.UserExtension^, SizeOf(userExtensionRecord), #00);
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'filling saveguestinfo');{$endif}
 FillChar(LineCfg^.SaveGuestRecord^, SizeOf(LineCfg^.SaveGuestRecord^), #00);
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'filling raconfig');{$endif}
 FillChar(GlobalCfg^.RaConfig^, SizeOf(ConfigRecord), #00);
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'filling ELCONFIG');{$endif}
 FillChar(GlobalCfg^.ElConfig^, SizeOf(EleConfigRecord), #00);
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'filling language');{$endif}
 FillChar(LineCfg^.Language^, SizeOf(LanguageRecord), #00);
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'filling limitsinfo');{$endif}
 FillChar(LineCfg^.LimitsInfo^, SizeOf(LimitsRecord), #00);
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'filling command_array');{$endif}
 FillChar(LineCfg^.Command_Array, SizeOf(LineCfg^.Command_Array), #00);
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'filling modem');{$endif}
 FillChar(LineCfg^.Modem^, SizeOf(ModemRecord), #00);
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'filling markedmsgarray');{$endif}
 FillChar(lineCfg^.MarkedMsgArray^, SizeOf(lineCfg^.MarkedMsgArray), #00);
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'filling curfilesrecord');{$endif}
 FillChar(lineCfg^.CurFilesRecord^, SizeOf(FilesRecord), #00);
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'filling curmsgrecord');{$endif}
 FillChar(lineCfg^.CurMessageRecord^, SizeOf(MessageRecord), #00);
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'filling curelemsgrecord');{$endif}
 FillChar(lineCfg^.CurEleMsgRecord^, SizeOf(EleMessageRecord), #00);
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'filling uploadsinfo');{$endif}
 FillChar(LineCfg^.UploadInfo^, SizeOf(UploadsRec), #00);
 {$IFNDEF MSDOS}
   {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'filling oldscreen');{$endif}
   FillChar(OldScreen^, SizeOf(OldScreen^), #00);
 {$ENDIF}

 lineCfg^.AnsiOn := False;
 lineCfg^.AvatarOn := False;
 lineCfg^.LocalOnly := False;
 lineCfg^.ProcessingLowLevel := False;
 lineCfg^.UsersKbLimit := 00;
 LineCfg^.CheckInActivity := False;
 LineCfg^.Exitinfo^.Userinfo.Screenlength := 24;
 LineCfg^.Exitinfo^.SysInfo.TotalCalls := -1;
 LineCfg^.Exitinfo^.UserRecord := -1;
 LineCfg^.Exitinfo^.TimeLimit := 00;
 LineCfg^.LimitsInfo^.LTime := 00;
 contrObj^.Timeinfo.SecondsTimer := CurrentTime;

 InitRemHooks;

 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logMain, 'exiting initvariables');
 {$ENDIF}

 TaggingObj^.ClearTagList;
 {$ENDIF}
end; { proc. Initvariables }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF WIN32}
{$IFNDEF OS2}
{$IFNDEF ELEUNIX}
Function ShowMultiTasker : String;
var Append: String;
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'ShowMultiTasker...');{$endif}
  GetMultiTasker;

  If GlobalCfg^.RaConfig^.MultiTasker=00 then append := 'detected'
   else append := 'assumed';

  if GlobalCfg^.RaConfig^.MultiTasker>00 then
    Case GlobalCfg^.RaConfig^.MultiTasker of
      01 : MultiTasker := NoTasker;
      02 : MultiTasker := DoubleDos;
      03 : MultiTasker := DesqView;
      04 : MultiTasker := NoTasker;    { Well, sort of .. ;-), really TopView }
      05 : MultiTasker := DesqView;  { Well, sort of .. :-), really MultiLink }
      06 : MultiTasker := NoTasker;      { Well, sort of .. :), really PC/Mos }
      07 : MultiTasker := WindowsTask;
      08 : MultiTasker := OS2;
    end; { case }

  Case MultiTasker of
    NoTasker   : Append := 'AT-BIOS ' + Append;
    DoubleDos  : Append := 'DoubleDOS ' + Append;
    DesqView   : Append := 'DESQview ' + Append;
    WindowsTask: Append := 'Windows ' + Append;
    OS2        : Append := 'OS/2 ' + Append;
    NoSlicing  : Append := 'No timeslicing support (forced)';
  End; { Case MultiTasker }

  {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'MultiTasker='+append);{$endif}
  ShowMultiTasker := Append;
end; { func. ShowMultiTasker }
{$ENDIF}
{$ENDIF}
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure StartOpening;                { Open .RA files and show start screen }
Const MonthNames: Array[1..12] of String[3]=('Jan', 'Feb', 'Mar',
                                             'Apr', 'May', 'Jun',
                                             'Jul', 'Aug', 'Sep',
                                             'Oct', 'Nov', 'Dec');
var Year, Month, Day, Dow: Word;
    Hours, Mins, Secs, MSecs: Word;
begin
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'StartOpening');{$endif}

 GetDate(year, month, day, dow);
 GetTime(hours, mins, secs, msecs);

 {$IFNDEF WINGUI}
   Crt.ClrScr;
   Crt.TextColor(07);
   Crt.TextBackground(00);
 {$ELSE}
   Form1.ColorConsole1.ClrScr;
   Form1.ColorConsole1.TextColor(07);
   Form1.ColorConsole1.TextBackground(00);
 {$ENDIF}

  LocalScreenLn(SystemMsgPrefix + FullProgName +
                ' firing up at ' + LeadingZero(Hours, 2) + ':' + LeadingZero(Mins, 02) +
                ' on ' + LeadingZero(Day, 2) + '-' + MonthNames[Month] + '-' + FStr(Year) +
                ' (' + FStr(MaxNodes) + ' node version)');
  LocalScreenLn(SystemMsgPrefix + 'Copyright 1996-2003 Maarten Bekers, All rights reserved.');

 ReadConfigRA;                                               { Read CONFIG.RA }
 InitSystemNames;
 ReadModemRA(LineCfg^.Modem^);                             { Read MODEM config }
 If NOT FileExist(LimitsFileName) then
   Config_ShowError('Unable to open', JustName(LimitsFileName), 02);


 if GlobalCfg^.RaConfig^.SysPath <> '' then
   if NOT FileExist(ForceBack(GlobalCfg^.RaConfig^.SysPath)) then Config_ShowError('Path does not exist (syspath)',
          GlobalCfg^.RaConfig^.Syspath, 03);
 if GlobalCfg^.RaConfig^.MenuPath <> '' then
   if NOT FileExist(ForceBack(GlobalCfg^.RaConfig^.MenuPath)) then Config_ShowError('Path does not exist (menupath)',
          GlobalCfg^.RaConfig^.Menupath, 03);
 if GlobalCfg^.RaConfig^.TextPath <> '' then
   if NOT FileExist(ForceBack(GlobalCfg^.RaConfig^.TextPath)) then Config_ShowError('Path does not exist (textpath)',
          GlobalCfg^.RaConfig^.Textpath, 03);
 if GlobalCfg^.RaConfig^.MsgBasePath <> '' then
   if NOT FileExist(ForceBack(GlobalCfg^.RaConfig^.MsgBasePath)) then Config_ShowError('Path does not exist (msgbasepath)',
          GlobalCfg^.RaConfig^.MsgBasepath, 03);
 if GlobalCfg^.RaConfig^.FileBasePath <> '' then
   if NOT FileExist(ForceBack(GlobalCfg^.RaConfig^.FileBasePath)) then Config_ShowError('Path does not exist (filebasepath)',
          GlobalCfg^.RaConfig^.FileBasepath,03);

 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'Commport='+FStr(LineCfg^.Modem^.Comport));{$endif}

 LoadDefaultLanguage;                                 { Read default language }

 {$IFDEF MSDOS}
    LocalScreenLn(SystemMsgPrefix + ShowMultiTasker);
 {$ENDIF}

 LineCfg^.TextFileShells := GlobalCfg^.RaConfig^.AllowFileShells;

{$ifndef WinGUI}
 Case GlobalCfg^.RaConfig^.VideoMode of
    Byte(Short) : if LastMode <> co80 then TextMode(co80);
    Byte(Long)  : if hi(LastMode) <> font8x8 then TextMode(Font8x8);
 end; { case }
{$endif}
end; { proc. StartOpening }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure LoadDefaultLanguage;
begin
  ReadLanguageRA(Max(0, GlobalCfg^.RaConfig^.NewUserLanguage - 01));

  if (NOT LangObj^.ReadRalFile(LineCfg^.Language^.DefName)) OR (NOT CheckLanguageAccess(LineCfg^.Language^, False,
     LineCfg^.Exitinfo^)) then
     begin
       RaLog('!', 'Could not open language definition file '+LineCfg^.Language^.DefName);
       LineCfg^.RalError := True;
       defExitCode := 255;

       { Note: }
       { Normally this error is shown at the exit of the program }
       { in file (exitproc.sup) but EleBBS/Unix doesnt use this }

       {$IFDEF ELEUNIX}
         Config_ShowError('Could not open language definition file', LineCfg^.Language^.DefName, 2);
       {$ENDIF}

       {$IFDEF WINGUI}
         Config_ShowError('Could not open language definition file', LineCfg^.Language^.DefName, 2);
       {$ENDIF}

       System.Halt(1);
     end; { if }
end; { proc. LoadDefaultLanguage }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SelectLanguage(ShowMenu: Boolean);
var Select    : Longint;
    DoCrLf    : Boolean;
    Language_F: pFileObj;
begin
 {$ifdef With_Debug} DebugObj.DebugLog(logMain, 'SelectLanguage');{$endif}

 if ShowMenu then
   begin
     if DisplayHotFile('LANGUAGE', [])=#01 then
      begin;
         DoCrLf := False;
         WriteLn;
         OutputObj^.DisplayAttr(15);
         Writeln(GlobalCfg^.RaConfig^.LangHdr);
         WriteLn;

         Config_OpenFile(Language_F, LanguageFileName, 01, ReadMode + DenyWrite, False, True);

         While NOT Language_F^.EOF do
           begin
             Config_ReadFile(Language_F, LineCfg^.Language^, SizeOf(LanguageRecord));

             if DoCrLf then Write('`X43:');

             if CheckLanguageAccess(LineCfg^.Language^, True, LineCfg^.Exitinfo^) then
              with lineCfg^ do
                begin;
                    OutputObj^.DisplayAttr(Yellow);    Write((Language_F^.FilePos div SizeOf(LanguageRecord)):3);
                    OutputObj^.DisplayAttr(Lightgray); Write(' ..... ');
                    OutputObj^.DisplayAttr(Cyan);      Write(Language^.Name);
                    If DoCrLf then WriteLn;

                    DoCrLF := NOT DoCrLf;
                end; { if }

          end; { While }

          If DoCrLf then WriteLn;                         { Switched already, and eof }
          Writeln;

          Config_DoneFile(Language_F);
      end; { When LANGUAGE.ANS doesn't exist }
   end; { if ShowMenu }

  OutputObj^.DisplayAttr(15);
  Writeln;

  RangeEdit(Select, '`A15:'+GlobalCfg^.RaConfig^.LanguagePrompt, 1, 100, GlobalCfg^.RaConfig^.NewUserLanguage);
  if ProgTerminated then EXIT;

  ReadLanguageRA(Byte(Select - 01));
  if NOT LangObj^.ReadRalFile(LineCfg^.Language^.DefName) then
      LoadDefaultLanguage;
  Writeln;
  Writeln;

  LineCfg^.Exitinfo^.Userinfo.Language := Byte(Select);
end; { proc. SelectLanguage }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure WelcomeInformation;    { Shows WELCOME screens, Dob/Mail check, etc }
var MiscData: String;
    Quest   : ^QuestObj;
    elxObj  : pElxBbsObj;
begin
 {$IFDEF WITH_FULL}
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMain, 'Welcome-information');
  {$ENDIF}
  OutputObj^.ResetLines(01);
  LineCfg^.DispMorePrompt := True;

  DoBirthdateCheck;
  if ProgTerminated then EXIT;
  DoPasswordCheck;
  if ProgTerminated then EXIT;

  OutputObj^.ClearScreen;
  LineCfg^.LoggedOn := true;
  {$IFDEF WINGUI}
    Form1.EditUserButton.Enabled := true;
  {$ENDIF}
  DisplayHotFile('WELCOME', []);                            { WELCOME.ANS }
  if Progterminated then EXIT;

  if (DaysToGo(LineCfg^.Exitinfo^.Userinfo.SubDate)=0) OR (DaysAgo(LineCfg^.Exitinfo^.Userinfo.SubDate)>0)
      AND (LineCfg^.Exitinfo^.Userinfo.SubDate <> '  -  -  ') AND (LineCfg^.Exitinfo^.Userinfo.SubDate <> '') then
         begin
         {-- run an Q-A or EleXer script -----------------------------------}
         if GetScriptType('SUBDATE') = 'Q-A' then
           begin
             New(Quest, Init);
               Quest^.Process('SUBDATE /N', true, '');
             Dispose(Quest, Done);
           end
             else begin
                   New(elxObj, Init);
                    elxObj^.RunElexerScript('subdate', '', false);
                   Dispose(elxObj, Done);
                  end; { else }

           LineCfg^.Exitinfo^.Userinfo.SubDate := '';
        end; { if }

  if ProgTerminated then EXIT;
  if (DaysToGo(LineCfg^.Exitinfo^.Userinfo.Subdate)>00) then
     if (LineCfg^.Exitinfo^.Userinfo.SubDate <> '  -  -  ') AND (LineCfg^.Exitinfo^.Userinfo.SubDate <> '') then
       begin
         {-- run an Q-A or EleXer script -----------------------------------}
         if GetScriptType('SUBDAY' + FStr(DaysToGo(LineCfg^.Exitinfo^.Userinfo.SubDate))) = 'Q-A' then
           begin
             New(Quest, Init);
               Quest^.Process('SUBDAY' + FStr(DaysToGo(LineCfg^.Exitinfo^.Userinfo.SubDate)) + ' /N', true, '');
             Dispose(Quest, Done);
           end
             else begin
                   New(elxObj, Init);
                   elxObj^.RunElexerScript('subday' + FStr(DaysToGo(LineCfg^.Exitinfo^.Userinfo.SubDate)), '', false);
                   Dispose(elxObj, Done);
                  end; { else }

           {$IFDEF WITH_DEBUG}
             DebugObj.DebugLog(logQuest, 'Executing question SUBDAY');
             DebugObj.DebugLog(logQuest, 'DaysToGo = '+FStr(DaysToGo(LineCfg^.Exitinfo^.Userinfo.SubDate)));
           {$ENDIF}
       end; { if }
  if ProgTerminated then EXIT;

  DisplayHotFile('WELCOME1', []);
  if ProgTerminated then EXIT;
  DisplayHotFile('SEC'+FStr(LineCfg^.Exitinfo^.Userinfo.Security), []); { SEC10.ANS }

  LineCfg^.DispMorePrompt := True;
  WriteLn;
  if (NOT LineCfg^.Exitinfo^.Emsi_Session) OR (Byte(GlobalCfg^.RaConfig^.CheckMail) <> Byte(Ask)) then
    begin
      Case Byte(GlobalCfg^.RaConfig^.CheckMail) of
         Byte(No)  : ;
         Byte(Yes) : CheckMailBox(LineCfg^.Exitinfo^);
         Byte(Ask) : If InputObj^.ralStrYesNoAsk(ralCheckMail) then
                      CheckMailBox(LineCfg^.Exitinfo^);
      end; { Case }
    end
     else begin
            if Pos('MAIL', SUpCase(LineCfg^.Exitinfo^.Emsi_Requests))>0 then
               CheckMailBox(LineCfg^.Exitinfo^);
          end; { if }

  if ProgTerminated then EXIT;

  if GlobalCfg^.RaConfig^.NewFileTag then MiscData := '/T' else MiscData := '';
  WriteLn;

  if (NOT LineCfg^.Exitinfo^.Emsi_Session) then
   if LineCfg^.Exitinfo^.Userinfo.Nocalls > 01 then
    begin
      Case Byte(GlobalCfg^.RaConfig^.CheckNewFiles) of
         Byte(No)  : ;
         Byte(Yes) : ListFiles(MiscData, fsNewFiles, false);
         Byte(Ask) : if InputObj^.ralStrYesNoAsk(ralCheckFile) then
                       ListFiles(MiscData, fsNewFiles, false);
      end; { case }
    end else If (Pos('FILE', SUpCase(LineCfg^.Exitinfo^.Emsi_Requests))>0) then
              If Byte(GlobalCfg^.RaConfig^.CheckNewFiles) <> Byte(No) then
               ListFiles(MiscData, fsNewFiles, false);

  if ProgTerminated then EXIT;

  if (NOT (LineCfg^.Exitinfo^.Userinfo.Sex in [1,2])) then
   if (NOT LineCfg^.GuestUser) then
    if GlobalCfg^.RaConfig^.AskForSex then AskForUsersSex;

  if GetPackedFileTime(OpenTextfile('ONCEONLY')) > PackTimeStr(LineCfg^.Exitinfo^.Userinfo.LastDate,
          LineCfg^.Exitinfo^.Userinfo.LastTime) then
    begin
      DisplayHotFile('ONCEONLY', []);
      if ProgTerminated then EXIT;
    end; { if file ahs changed since last logon }

  DisplayHotFile('TIME'+LeadingZero(GetHour, 2), []);
  if ProgTerminated then EXIT;

  if (Pos('NEWS', SUpCase(LineCfg^.Exitinfo^.EMSI_Requests))>00) OR (NOT LineCfg^.Exitinfo^.Emsi_Session) then
    DisplayHotFile('NEWS', []);                                { NEWS.ANS }
  if (LineCfg^.Exitinfo^.Emsi_Session) AND (Pos('NEWS', SUpCase(LineCfg^.Exitinfo^.Emsi_Requests))=00) then
    DisplayHotFile('ALTNEWS', []);
  {$IFDEF WITH_DEBUG}
    DisplayHotFile('ELEBBS', []);
  {$ENDIF}

  DisplayHotFile('TIME'+Copy(TimeStr(False, False), 1, 2), []);  { TIME20.ANS }
  if ProgTerminated then EXIT;

  if Copy(JDates.DateStr, 1, 5) = Copy(LineCfg^.Exitinfo^.Userinfo.Birthdate, 1, 5) then
    DisplayHotFile('BIRTHDAY', []);

  DisplayHotFile(LeadingZero(GetMonth, 2) + '-' + LeadingZero(GetDay, 2), []);
  if ProgTerminated then EXIT;
 {$ENDIF}
end; { proc. WelcomeInformation }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
{$ENDIF}


end. { unit Main }
