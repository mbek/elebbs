unit eleMenu;
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
** Menu declarations for EleBBS
**
** Copyright (c) 1996, 1997 by Maarten Bekers
**
** Created : 08-Sep-1996
** Last update : 16-Sep-1997
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

uses global, cfgrec;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

(*
**
** Note for structure of menu:
**
** .Typ             Contains "HiLight" color (SHL 4 .Security)
** .ForeGround      Contains normal color (SHL 4 .Background)
** .MiscData        /NG = No Global Menu
**                  /NO = No CR/LF codes
**
*)


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

{$IFNDEF WITH_FULL}
  This unit shouldn''t be compiled for an utility program
{$ENDIF}

{$IFDEF WITH_FULL}
procedure  Enter_Menu;
function   GetArrowKeys: Char;     { Assumes that <ESC> is already entered }
function   Read_Menu_Structure(MenuName: String; var Menu_Inhoud: MenuRec;
                               var MenuLines: Byte; SkipPrompt: Boolean): Boolean;
Function   ExecuteMenuFunc(Typ: Byte; MiscData: String; HotKeys:String;
                           var GotoMenu: Boolean;
                           var HotKeySet: CharSet):Char;
Function   AskMenuAccessPassword(PassWord, MenuName: String): Boolean;
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

{$IFDEF WITH_FULL}
Uses
     ApTimer,
{$IFNDEF WINGUI}
     Dos,
     {$IFDEF VirtualPascal}
       VpSysLow,
     {$ENDIF}

{$ENDIF}
        Multi,
        Support,
          MenuFunc,
           Mail,
            FileSys,
             Question,
             {$IFDEF WITH_FULL}
               Transfer,
               AreaSel,
                Terminal,
                {$IFDEF TCPIP}
                  IrcUnit,
                  TelUnit,
                {$ENDIF}
             {$ENDIF}
               Ral,
                InOut_U,
                 Input_U,
                  StrPath,
                  Debug_U,
                   Access_U,
                     MkFile,
                      Flags,
                       Sound_u,
                        NodeLst,
                         StatusB,
                           ElLog_U,
                            LineEd,
                             MultiLn,
                              DispAns,
                               Control,
                                MailInt,
                                 BitWise,
                                  ExecFunc,
                                   Mnuser,
                                    Colors,
                                      ListFile,
                                       Cases,
                                        LongStr,
                                         CentrStr,
                                          WordStr,
                                           MemMan,
                                            Limit_U,
                                             ExitProg,
                                              TagUnit,
                                               ViewFile,
                                                FileRout,
                                                 Stutils,
                                                  Main,
                                                   AreaSw,
                                                    FsEd,
                                                     FileObj,
                                                      ScrList,
                                                       Elx_BBS,
                                                        ObjDec;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function MenuUserHook(C: Char; var RetStr: UserHookString): Boolean;
begin
  RetStr := '';
  MenuUserHook := false;

  Case C of
    '~'  : begin
             if Longint(Longint(LineCfg^.Limitsinfo^.LTime) - Longint(LineCfg^.Exitinfo^.Userinfo.Elapsed))=UnlimitedValue then
               RetStr := langObj^.ralGet(ralUnlimited)
                 else RetStr := FStr(LineCfg^.Exitinfo^.TimeLimit);
             MenuUserHook := true;
           end;
  end; { case }
end; { func. MenuUserHook }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

Procedure DisplayMenuPrompt(const Menu, Prompt:MnuRecord; PromptItem: Boolean);
var Counter       : Byte;
    CurHilighting : Boolean;
    TempStr       : String;
    DisplayStr    : String;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMenuFunc, 'DisplayMenuPrompt');
  {$ENDIF}
 TermObj^.UserHook := {$IFDEF FPC}@{$ENDIF}MenuUserHook;

 With Menu do
 begin;
   CurHiLighting := False;
   DisplayStr := Display;

   TempStr := '`A'+FStr(MakeAttr(ForeGround, BackGround))+':';

   for Counter := 01 to Length(DisplayStr) do
    begin
      If NOT (DisplayStr[Counter] in ['^', ';']) then
        TempStr := TempStr + DisplayStr[Counter]
         else begin
                if (DisplayStr[Counter]=';') AND (Length(DisplayStr)<>Counter) then
                   TempStr := TempStr + ';';

                If DisplayStr[Counter]='^' then
                  If CurHiLighting then
                    begin
                      TempStr := TempStr + '`A' + FStr(MakeAttr(ForeGround, BackGround)) + ':';
                      CurHilighting := false
                    end
                     else begin
                            TempStr := TempStr + '`A' + FStr(MakeAttr(Prompt.Typ, Prompt.Security)) + ':';
                            CurHiLighting := True;
                          end; { if not curhilighting }
              end;
    end; { for }

   Write(TempStr);
   If DisplayStr[Length(DisplayStr)]<>';' then
    If NOT PromptItem then WriteLn;
 end; { With Menu }

 Flush(Output);
 TermObj^.UserHook := {$IFDEF FPC}@{$ENDIF}EmptyUserHook;
end; { displayMenuPrompt }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

Function  AskMenuAccessPassword(PassWord, MenuName: String): Boolean;
var TempStr: String;
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logMenu, 'Askpassword: '+Password+' / '+MenuName);{$endif}
 AskMenuAccessPassword := False;

 Writeln;
 Writeln;
 Write('`A12:', LangObj^.RalGet(ralPassword));

 TempStr := '';
 GetString(TempStr, 15, [#32..#254], True, True, False, False);
 WriteLn;

 TempStr := SUpcase(Trim(TempStr));
 Password := SUpcase(Trim(Password));

 AskMenuAccessPassword := (TempStr=Password);

 If TempStr<>Password then begin;
                             WriteLn;
                             WriteLn;
                             WriteLn('`A10:', LangObj^.ralGet(ralNoAccess));
                             WriteLn;
                             RaLog('!', 'Used password "'+TempStr+'"');
                             RaLog('!', 'Attempted access to passworded menu "'+MenuName+'"');
                           end; { Invalid password }
end; { func. AskMenuAccessPassword }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function ExecuteMenuFunc(Typ: Byte; MiscData: String; HotKeys:String;
                         var GotoMenu: Boolean;
                         var HotKeySet: CharSet):Char;
var Teller    : Byte;
    MenuName  : String;
    TempStr   : String;
    UserOnStr : String;
    HasAccess : Boolean;
    SwitchStr : String;
    ExitCode  : Word;
    TempChar  : Char;
    TempL     : Longint;
    Uploads   : Longint;
    Success   : Boolean;

    FilesInf  : FilesRecord;
    MessageInf: MessageRecord;
    Quest     : ^QuestObj;
    elxObj    : pElxBbsObj;
    SupressCls: Boolean;
    {$IFDEF MSDOS}
      SaveAvail : Longint;
    {$ENDIF}
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMenu, '  MenuTyp='+FStr(typ)+' / '+MiscData);
  {$ENDIF}

  {$IFDEF MSDOS}
    SaveAvail := MemAvail;
  {$ENDIF}

  {-- Initialize some variables ---------------------------------------------}
  GotoMenu := FALSE;
  if LineCfg^.Exitinfo^.MenuStackPointer < 01 then
    LineCfg^.Exitinfo^.MenuStackPointer := 01;
  SupressCls := false;
  ExecuteMenuFunc := #00;
  if Typ <> 40 then HotKeySet := [];


  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMenu,'  Executing Menu type ['+FStr(Typ)+']: MiscData='+MiscData+ ' / MenuStackPointer='+
            FStr(LineCfg^.ExitInfo^.MenuStackPointer));
  {$ENDIF}

  {-- Make sure the linecounter is reset ------------------------------------}
  OutputObj^.SetStopMore(false);

  {-- if we should not clear the screen, dont do it -------------------------}
  if (Pos('/NS', SUpCase(MiscData)) <> 0) then
    begin
      SupressCLS := true;
      Replace('/NS', '', MiscData);
    end; { if }

  {-- see if we should put some extra keys in the buffer --------------------}
  if GetValue('/K=', MiscData, FALSE) <> '' then
    InputObj^.PutInBuffer(Getvalue('/K=', MiscData, TRUE), FALSE);


  {-- Now lets see if we can execute the menus we want ----------------------}
 Case Typ of
{ Display only }             00 : ;
{ Goto another menu }        01 : begin;
                                    MenuName := FirstWord(MiscData, defExtractWord, true);
                                    UserOnStr := Under2Norm(GetValue('*U', MiscData, True));
                                    SwitchStr := '';
                                    While (Pos('/', MiscData)>00) do
                                       SwitchStr := SwitchStr + '/' + GetValue('/', MiscData, True) + #32;
                                    TempStr := FirstWord(MiscData, defExtractWord, false);
                                    GotoMenu := True;

                                    HasAccess := False;

                                    If TempStr<>'' then
                                      If AskMenuAccessPassword(TempStr, MenuName) then
                                           HasAccess := True;

                                    If TempStr='' then HasAccess := True;
                                    if NOT HasAccess then GotoMenu := false;

                                    If HasAccess then
                                         begin
                                           AreaSwitch(Switchstr);
                                           LineCfg^.Exitinfo^.MenuStack[LineCfg^.Exitinfo^.MenuStackPointer] := MenuName;
                                           If UserOnStr<>'' then
                                              MultiLnObj^.UserOn^.StatDesc := UserOnStr;
                                           MultiLnObj^.WriteUserOn(MultiLnObj^.Useron^.StatDesc, uonBrowsing);
                                         end; { User has access to this menu }

                                    If NOT SupressCls then
                                        OutputObj^.ClearScreen;
                                    SupressCls := False;
                                  end; { goto }
{ Gosub another menu }       02 : begin;
                                    MenuName := FirstWord(MiscData, defExtractWord, true);
                                    UserOnStr := Under2Norm(GetValue('*U', MiscData, True));
                                    SwitchStr := '';
                                    While (Pos('/', MiscData)>00) do
                                       SwitchStr := SwitchStr + '/' + GetValue('/', MiscData, True) + #32;
                                    AreaSwitch(SwitchStr);
                                    TempStr := FirstWord(MiscData, defExtractWord, false);
                                    GotoMenu := True;

                                    HasAccess := False;

                                    If TempStr<>'' then
                                      If AskMenuAccessPassword(TempStr, MenuName) then HasAccess := True;
                                    If TempStr='' then HasAccess := True;
                                    if NOT HasAccess then GotoMenu := false;

                                    If HasAccess then
                                         begin;
                                           AreaSwitch(SwitchStr);
                                           If UserOnStr<>'' then
                                              MultiLnObj^.UserOn^.StatDesc := UserOnStr;
                                           MultiLnObj^.WriteUserOn(MultiLnObj^.Useron^.StatDesc, uonBrowsing);

                                           Inc(LineCfg^.Exitinfo^.MenuStackPointer);
                                           If LineCfg^.Exitinfo^.MenuStackPointer>MaxNestMenus then
                                               LineCfg^.Exitinfo^.MenuStackPointer := MaxNestMenus;

                                           LineCfg^.Exitinfo^.MenuStack[LineCfg^.Exitinfo^.MenuStackPointer] := MenuName;
                                         end; { User has access to this menu }

                                    If NOT SupressCls then
                                        OutputObj^.ClearScreen;
                                    SupressCls := False;
                                  end; { Gosub }
{ Return from gosub }        03 : begin;
                                     UserOnStr := Under2Norm(GetValue('*U', MiscData, True));
                                     If UserOnStr<>'' then
                                      MultiLnObj^.UserOn^.StatDesc := UserOnStr;
                                     MultiLnObj^.WriteUserOn(MultiLnObj^.Useron^.StatDesc, uonBrowsing);
                                     GotoMenu := True;

                                     SwitchStr := '';
                                     While (Pos('/', MiscData)>00) do
                                        SwitchStr := SwitchStr + '/' + GetValue('/', MiscData, True) + #32;
                                     AreaSwitch(SwitchStr);

                                     If LineCfg^.Exitinfo^.MenuStackPointer>01 then
                                       Dec(LineCfg^.Exitinfo^.MenuStackPointer);

                                    If NOT SupressCls then
                                        OutputObj^.ClearScreen;
                                    SupressCls := False;
                                  end; { Return from gosub }
{ Goto after clearing stack } 04 : begin;
                                    MenuName := FirstWord(MiscData, defExtractWord, true);
                                    UserOnStr := Under2Norm(GetValue('*U', MiscData, True));
                                    GotoMenu := True;
                                    SwitchStr := '';
                                    While (Pos('/', MiscData)>00) do
                                       SwitchStr := SwitchStr + '/' + GetValue('/', MiscData, True) + #32;
                                    AreaSwitch(SwitchStr);

                                    HasAccess := False;

                                    TempStr := FirstWord(MiscData, defExtractWord, false);
                                    If TempStr<>'' then
                                      If AskMenuAccessPassword(TempStr, MenuName) then HasAccess := True;
                                    If TempStr='' then HasAccess := True;
                                    if NOT HasAccess then GotoMenu := false;

                                    If HasAccess then
                                         begin;
                                           For Teller := 01 to MaxNestMenus do
                                                LineCfg^.Exitinfo^.MenuStack[Teller] := '';

                                           LineCfg^.Exitinfo^.MenuStackPointer := 01;
                                           MiscData := MiscData + ' ';

                                           AreaSwitch(SwitchStr);
                                           LineCfg^.Exitinfo^.MenuStack[LineCfg^.Exitinfo^.MenuStackPointer] := MenuName;

                                           If UserOnStr<>'' then
                                              MultiLnObj^.UserOn^.StatDesc := UserOnStr;
                                           MultiLnObj^.WriteUserOn(MultiLnObj^.Useron^.StatDesc, uonBrowsing);
                                         end; { User has access to this menu }

                                    If NOT SupressCls then
                                        OutputObj^.ClearScreen;
                                    SupressCls := False;
                                  end; { Clear gosubs&return }

{ Display .ASC/.ANS file }      05 : DisplayHotFile(MiscData, []);
{ Selection menu }              06 : BullettMenu(MiscData);
{ Execute sub-program }         07 : RaExec(MiscData, False, Pos('*V', SUpCase(MiscData)) = 0, False, False, ExitCode,Success);
{ Version information }         08 : ShowVersionInformation;
{ Terminate call }              09 : TerminateCall(True);
{ Display system usage graph }  10 : ShowUsageGraph(LineCfg^.Exitinfo^);
{ Page System operator }        11 : PageSystemOperator(MiscData);
{ Execute questionnaire }       12 : begin
                                       {-- Get the filename ----------------}
                                       TempStr := FirstWord(MiscData, defExtractWord, true);

                                       {-- if its an EleXer script run that-}
                                       {-- else run an Q-A one -------------}
                                       if IsElexerScript(TempStr) then
                                         begin
                                           New(elxObj, Init);
                                           elxObj^.RunElexerScript(TempStr, MiscData, true);
                                           Dispose(elxObj, Done);
                                         end
                                           else begin
                                                  Quest := nil;
                                                  New(Quest, Init);

                                                  if QUEST <> nil then
                                                    begin
                                                      Quest^.Process(TempStr + #32 + Miscdata, true, '');
                                                      Dispose(Quest, Done);
                                                    end { if }
                                                      else RaLog('!', 'Not enough memory to initiate questionnaire');
                                                end; { else }
                                     end; { script }
{ View users list }             13 : ShowUserList(MiscData);
{ Call time statistics }        14 : ShowTimeStatistics;
{$IFDEF WITH_FULL}
{ Exit with errorlevel }        15 : ExitWithErrorLevel(MiscData);
{$ENDIF}
{ Change users location }       16 : begin
                                       WriteLn; WriteLn; WriteLn; WriteLn;
                                       MustEdit(LineCfg^.Exitinfo^.Userinfo.Location, LangObj^.RalGet(ralAskLoc), 25,
                                                GlobalCfg^.RaConfig^.CapLocation, True, 14, 0, [#32..#254], 01);
                                       WriteLn;
                                     end; { Change users location }
{ Change password }             17 : GetNewPassword(False, False);
{ Change screenlength }         18 : begin
                                        WriteLn; WriteLn;
                                        TempL := LineCfg^.Exitinfo^.Userinfo.ScreenLength;
                                        RangeEdit(TempL, LangObj^.RalGet(ralAskLines), 10, 66, 24);
                                        LineCfg^.Exitinfo^.Userinfo.Screenlength := Byte(TempL);
                                        WriteLn;
                                        WriteLn(LangObj^.RalGet(ralScrChange),' ', LineCfg^.Exitinfo^.Userinfo.ScreenLength);
                                        Writeln;
                                        InputObj^.PressEnter(False, True);
                                     end; { ScreenLength }
{ Adjust screen-clearing }      19 : ToggleProc(ralClrAct, ralClrInact, LineCfg^.Exitinfo^.Userinfo.Attribute, 1, false);
{ Adjust page pausing }         20 : ToggleProc(ralPagePause, ralPagePause, LineCfg^.Exitinfo^.Userinfo.Attribute, 2, True);
{ Adjust ANSI capabilities }    21 : begin
                                        If (FixBaud(LineCfg^.Exitinfo^.Baud) < GlobalCfg^.RaConfig^.GraphicsBaud) AND
                                          (LineCfg^.Exitinfo^.Baud > 00) then
                                          begin
                                            WriteLn('`A12:');
                                            WriteLn;
                                            WriteLn(LangObj^.ralGet(ralAnsSlow));
                                            InputObj^.PressEnter(False, False);
                                          end
                                           else begin
                                                   ToggleProc(ralANSAct, ralANSInact, LineCfg^.Exitinfo^.Userinfo.Attribute,
                                                         3, false);
                                                   if NOT ReadBit(LineCfg^.Exitinfo^.Userinfo.Attribute, 3) then
                                                    if NOT InputObj^.ralStrYesNoAsk(ralAnsReq) then
                                                        ClearBit(LineCfg^.Exitinfo^.Userinfo.Attribute, 6);
                                                end; { if }
                                        LineCfg^.AnsiOn := ReadBit(LineCfg^.Exitinfo^.UserInfo.Attribute, 3);
                                        Write('`A7:');
                                        StatusDisplay(99, false);
                                     end; { ANSI }
{ Check mailbox }               22 : CheckMailBox(LineCfg^.Exitinfo^);
{ Read messages }               23 : ReadMessages(rdNormal, Miscdata, LineCfg^.Exitinfo^);
{ Scan messages }               24 : ReadMessages(rdScan, Miscdata, LineCfg^.Exitinfo^);
{ Quickscan messages }          25 : ReadMessages(rdQuickScan, Miscdata, LineCfg^.Exitinfo^);
{ Delete a specified message }  26 : MenuDeleteMsg(MiscData, LineCfg^.Exitinfo^);
{ Post new message }            27 : MenuPost(MiscData);
{ Choose combined message }     28 : SelectCombined(MiscData);
{ -- Move file to other directory} 29 : ;  (*** NOT implemented yet ***)
{ Direct dsk dir. listing }     30 : DirectDirectoryListing(MiscData, False);
{ List files in area }          31 : ListFiles(MiscData, fsNormal, true);
{$IFDEF WITH_FULL}
{ Download (send to user) }     32 : DoDownload(MiscData, False, False, False, True, false);
{ Upload (receive from user) }  33 : DoUpload(MiscData, False, False, '', Uploads);
{ List files in archive }       34 : ArchiveListing(MiscData, True, '', LineCfg^.Exitinfo^);
{ Search keyword in all areas } 35 : ListFiles(MiscData, fsKeyWord, true);
{ Search filename in areas }    36 : ListFiles(MiscData, fsFileName, true);
{ Search for newfiles areas }   37 : ListFiles(MiscData, fsNewFiles, true);
{ Display text in area }        38 : ShowAnsiFile(MiscData, True, '', LineCfg^.Exitinfo^);
{ Display direct text-file }    39 : ShowDirectFile(MiscData);
{$ENDIF}
{ Display ANS/ASC with hotkeys} 40 : begin
                                       LineCfg^.DispMorePrompt := False;
                                       LineCfg^.DispAbortSet := [];
                                       TempChar := DisplayHotFile(MiscData, HotKeySet);
                                       if TempChar <> #01 then
                                        ExecuteMenuFunc := TempChar;
                                       LineCfg^.DispAbortSet := ['S', 's'];
                                       OutputObj^.ResetLines(01);
                                       LineCfg^.DispMorePrompt := True;
                                     end; { DisplayHotFile }
{ Adjust full screen editor }   41 : ToggleProc(ralFSEdAct, ralFSEdInact, LineCfg^.Exitinfo^.Userinfo.Attribute, 6, false);
{ Adjust hotkey settings }      42 : ToggleProc(ralHotAct, ralHotInact, LineCfg^.Exitinfo^.Userinfo.Attribute2, 0, false);
{$IFDEF WITH_FULL}
{ List msg-areas with new msgs} 43 : ShowMsgAreaNewMail(MiscData, True, False, false);
{ Clear combined message-areas} 44 : ClearCombined(MiscData);
{ Display .ANS/ASC with CR }    45 : DisplayWithCr(MiscData);
{ Display direct with CR at end}46 : DisplayDirect(MiscData);
{ Make entry in system log }    47 : begin
                                       if Pos('@', MiscData) > 00 then
                                         begin
                                           GetFilesRecord(FilesInf, LineCfg^.Exitinfo^.Userinfo.FileArea, True);
                                           Replace('@',  FilesInf.Name, MiscData);
                                         end; { if }

                                       if Pos('`', MiscData) > 00 then
                                         begin
                                           GetMessageRecord(MessageInf, LineCfg^.Exitinfo^.Userinfo.MsgArea, True);
                                           Replace('`',  MessageInf.Name, MiscData);
                                         end; { if }

                                        TermObj^.RaCodeStr(MiscData);
                                        RaLog('>', MiscData);
                                     end; { MakeEntry }
{$IFDEF WITH_FULL}
{ Download specific file }      48 : DoDownload(MiscData, false, true, false, false, (Pos('/FREE', SUpCase(MiscData))>00));
{$ENDIF}
{ Select message area }         49 : ChangeMessageArea(MiscData);
{ Select curr. templ. file area}50 : ChangeFileArea(MiscData, LineCfg^.Exitinfo^.Userinfo.FileArea);
{ Todays callers list }         51 : ShowTodaysCallers(MiscData);
{ Who's online list }           52 : MultiLnObj^.ShowUsersOnline(MiscData, True);
{ Toggle do not disturb }       53 : ToggleProc(ralQuiet, ralQuiet, LineCfg^.Exitinfo^.Userinfo.Attribute, 7, true);
{ Send message to another line} 54 : MultiLnObj^.BBSSendMessage(00, MiscData);
{$IFDEF WITH_FULL}
{ Download *ANY* file }         55 : DoDownload(MiscData, False, False, True, True, false);
{ Browse nodelist }             56 : BrowseNodeList;
{ Change home-number }          57 : GetPhoneNumber(LineCfg^.Exitinfo^.Userinfo.VoicePhone, ralAskvoice);
{ Change data-number }          58 : GetPhoneNumber(LineCfg^.Exitinfo^.Userinfo.DataPhone, ralAskData);
{ Global download }             59 : DoDownload(MiscData, True, False, False, True, false);
{ Change handle }               60 : ChangeHandle(MiscData);
{ Toggle avatar }               61 : begin
                                      ToggleProc(ralAvtAct, ralAvtInact, LineCfg^.Exitinfo^.Userinfo.Attribute2, 1, false);
                                      LineCfg^.AvatarOn := ReadBit(LineCfg^.Exitinfo^.UserInfo.Attribute2, 1);
                                      StatusDisplay(99, false);
                                      Write('`A7:');
                                     end; { if ToggleAvatar }
{ Toggle fullscreen msg reader} 62 : ToggleProc(ralFSvAct, ralFsvInact, LineCfg^.Exitinfo^.Userinfo.Attribute2, 2, false);
{ Select a new language }       63 : SelectLanguage(True);
{ Select new date-format }      64 : AskDateFormat;
{ Change user's flag }          65 : begin
                                       MenuChangeFlags(MiscData, LineCfg^.Exitinfo^.Userinfo.Flags);
                                       WriteExitinfo(LineCfg^.Exitinfo^);
                                     end; { MenuChangeFlags }
{ Toggle textfile shells }      66 : If Pos('ON', SUpcase(MiscData))>00 then LineCfg^.TextfileShells := True
                                          else LineCfg^.TextFileShells := False;
{ Toggle auto-msg forwarding }  67 : ToggleForwarding(MiscData);
{ -- Create new RTC }              68 : ;  (*** Not finished yet ***)
{ -- Join RTC }                    69 : ;  (*** Not finished yet ***)
{ -- Delete RTC }                  70 : ;  (*** Not finished yet ***)
{ View/edit list of tagged }    71 : EditTagList(False, False, True, False, LineCfg^.Exitinfo^.Userinfo.FileArea);
{ New default protocol }        72 : SelectProtocol;
{ Toggle echomail in mailbox }  73 : ToggleProc(ralIgnEchoM, ralDisEchoM, LineCfg^.Exitinfo^.Userinfo.Attribute2, 5, false);
{ Change users mailing addr }   74 : AskForMailAddress(False);
{ Mailbox scan }                75 : ToggleProc(ralScanSel, ralScanAll, LineCfg^.Exitinfo^.Userinfo.Attribute3, 0, false);
{ Display a RIP icon }          76 : DisplayRipIcon(MiscData);
{------------------- Here is were RemoteAccess ends ------------------------}
 {$IFDEF TCPIP}
    { Join IRC conference }        100 : IrcIntObj^.Join_IRC(miscdata);
    { Telnet to another system }   101 : TelIntObj^.Telnet_System(miscdata);
 {$ENDIF}
{$ENDIF}
{$ENDIF}
 End; { CASE }

 {$IFDEF MSDOS}
  {$IFDEF WITH_DEBUG}
    if SaveAvail > MemAvail then
      begin
        RaLog('!', 'Memory leak in '+FullProgName+ ', after executing menu type '+FStr(Typ));
        RaLog('!', 'Lost a total of '+FStr(SaveAvail - MemAvail)+ ' bytes of memory!');
      end; { if }
 {$ENDIF}
 {$ENDIF}
End; { ExecuteMenuFunc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function Read_Menu_Structure(MenuName: String; var Menu_Inhoud: MenuRec;
                             var MenuLines: Byte; SkipPrompt: Boolean): Boolean;
var Menu_F       : pFileObj;
    NumRead      : NumReadType;
    FullName     : String;
    SaveMenuLines: Byte;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMenu,'  Reading menustructure: '+MenuName+ ' (begin)');
  {$ENDIF}

  {-- initialize some variables ---------------------------------------------}
  MenuName := SlowCase(menuName);
  Read_Menu_Structure := false;

  if (FileExist(LineCfg^.Language^.MenuPath + MenuName + '.mnu')) then
    FullName := LineCfg^.Language^.MenuPath + MenuName + '.mnu'
     else FullName := GlobalCfg^.RaConfig^.MenuPath + MenuName + '.mnu';

  New(Menu_F, Init);
  Menu_F^.Assign(FullName);
  Menu_F^.FileMode := ReadMode + DenyNone;
  if NOT Menu_F^.Open(1) then
    begin
      {$IFDEF WITH_DEBUG}
        DebugObj.DebugLog(logMenu, 'Unable to find menu: '+LineCfg^.Language^.MenuPath + MenuName + '.mnu');
      {$ENDIF}

      Dispose(Menu_F, Done);
      EXIT;
    end; { if }

  if SkipPrompt then
    Menu_F^.Seek(SizeOf(mnuRecord));
  NumRead := Menu_F^.Blkread(Menu_Inhoud[MenuLines + 1], SizeOf(Menu_Inhoud) - (MenuLines * SizeOf(mnuRecord)));

  Dispose(Menu_F, Done);

  Inc(MenuLines, NumRead div SizeOf(mnuRecord));


  Read_Menu_Structure := true;
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMenu,'  Reading menustructure: '+MenuName+ ' ( end )');
  {$ENDIF}
end; { func. Read_Menu_Structure }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure ReadLightBars(MenuName: String; var Lbar_Inhoud: LBarRec;
                        SkipPrompt: Boolean; var StartItems: Byte);
var FullName      : String;
    NumRead       : NumreadType;
    LBar_F        : pFileObj;
    SaveStartItems: Byte;
begin
  MenuName := SlowCase(MenuName);

  if (FileExist(LineCfg^.Language^.MenuPath + MenuName + '.mnu')) then
    FullName := LineCfg^.Language^.MenuPath + MenuName + '.mnu'
     else FullName := GlobalCfg^.RaConfig^.MenuPath + MenuName + '.mnu';

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMenu,'  ReadLightBars: '+NoExtension(FullName)+ '.mlb ( end )');
  {$ENDIF}


  New(LBar_F, Init);
  Lbar_F^.Assign(NoExtension(FullName) + '.mlb');
  LBar_F^.FileMode := ReadMode + DenyNone;

  if LBar_F^.Open(1) then
    begin
      if SkipPrompt then
        LBar_F^.Seek(SizeOf(LightBarRecord));

      {$i-}
        NumRead := LBar_F^.BlkRead(LBar_Inhoud[StartItems + 1], SizeOf(LBar_Inhoud) - (StartItems * SizeOf(LightbarRecord)));
      {$I+}
      if IOResult>00 then ;

      Inc(StartItems, NumRead div SizeOf(LightbarRecord));
    end; { if }

  Dispose(LBar_F, Done);
end; { proc. ReadLightBars }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function KeyDelay(Sec: Byte; Default: Char): Char;
var CH: Char;
    ET: EventTimer;
begin
  NewTimer(ET, Sec);

  CH := Default;

  While (NOT InputObj^.InputChr(CH)) AND (NOT ProgTerminated)
          AND (NOT TimerExpired(ET)) AND (contrObj^.CheckAll) do
           begin
             if LineCfg^.DoFlushScrnBuffer then
               InputObj^.UpdateScrn;

             DoSlice;
           end; { if }

  KeyDelay := CH;
  contrObj^.SetTimeOut;
end; { func. KeyDelay  }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function GetArrowKeys: Char;       { Assumes that <ESC> is already entered }
var CH: Char;
begin
  CH := KeyDelay(Longint((Longint(Round(0.25 * TicsFreq))) div SecsFreq), #27);

  if CH = '[' then
    begin
      CH := UpCase(KeyDelay(Secs2Tics(1), #27));

      if (NOT (CH in ['A'..'D', 'H', 'K'])) then CH := #00;
    end; { if }

  GetArrowKeys := CH;
end; { func. GetArrowKeys }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure DisplayLightBar(const LBar: LightBarRecord; Active: Boolean);
begin
  With LBar do
   begin
     if ReadBit(Attrib, 0) then
      begin
        Write('`X', LightX, ':');
        Write(OutputObj^.DoForceY(LightY));

        if Active then Write(LBar.SelectItem)
          else Write(LBar.LowItem);
      end; { if }
  end; { with }
end; { proc. DisplayLightBar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure BuildLightBarMenu(ActivePos, MenuEntries: Byte);
var Counter: Byte;
begin
  for Counter := 01 to MenuEntries do
   if CheckMenuAccess(LineCfg^.MenuContents^[Counter], LineCfg^.Exitinfo^) then
    DisplayLightBar(LineCfg^.LBarContents^[Counter], (ActivePos = Counter));
end; { proc. BuildLightBarMenu }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure CorrectArrowPos(var ArrowPos: Byte; MenuEntries: Byte; GoingUp: Boolean);
var Counter   : Byte;
begin
  if ArrowPos > MenuEntries then ArrowPos := 02;
   if LineCfg^.LBarContents = nil then EXIT;

  if ArrowPos > 1 then
  if (NOT ReadBit(LineCfg^.LBarContents^[ArrowPos].Attrib, 0)) OR
      (NOT CheckMenuAccess(LineCfg^.MenuContents^[ArrowPos], LineCfg^.Exitinfo^)) then
       with LineCfg^ do
   begin
     if NOT GoingUp then
      for Counter := ArrowPos to MenuEntries do
       if (ReadBit(LBarContents^[Counter].Attrib, 0)) then
        if CheckMenuAccess(MenuContents^[Counter], LineCfg^.Exitinfo^) then
         begin
           ArrowPos := Counter;
           BREAK;
         end; { if }

      if GoingUp then
       for Counter := ArrowPos downto 02 do
        if (ReadBit(LBarContents^[Counter].Attrib, 0)) then
         if CheckMenuAccess(MenuContents^[Counter], LineCfg^.Exitinfo^) then
          begin
            ArrowPos := Counter;
            BREAK;
          end; { if }

     if (NOT ReadBit(LBarContents^[Counter].Attrib, 0)) OR
         (NOT CheckMenuAccess(MenuContents^[Counter], LineCfg^.Exitinfo^)) then
       begin
         if NOT GoingUp then
           For Counter := 01 to MenuEntries do
             if (ReadBit(LBarContents^[Counter].Attrib, 0)) then
              if CheckMenuAccess(MenuContents^[Counter], LineCfg^.Exitinfo^) then
                begin
                  ArrowPos := Counter;
                  BREAK;
                end; { if }

         if GoingUp then
           For Counter := MenuEntries downto 02 do
             if (ReadBit(LBarContents^[Counter].Attrib, 0)) then
              if CheckMenuAccess(MenuContents^[Counter], LineCfg^.Exitinfo^) then
                begin
                  ArrowPos := Counter;
                  BREAK;
                end; { if }

         if (NOT ReadBit(LBarContents^[ArrowPos].Attrib, 0)) then ArrowPos := 02;
         if NOT CheckMenuAccess(MenuContents^[Counter], LineCfg^.Exitinfo^) then
           ArrowPos := 02;
       end; { if }
   end; { if }

  if ArrowPos < 02 then
    begin
      For Counter := MenuEntries downto 02 do
       if ReadBit(LineCfg^.LBarContents^[Counter].Attrib, 0) then
        if CheckMenuAccess(LineCfg^.MenuContents^[Counter], LineCfg^.Exitinfo^) then
          begin
            ArrowPos := Counter;
            BREAK;
          end; { if }
     end; { if }
end; { proc. CorrectArrowPos }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure GetMenuChoice(var CH: Char; LBar: Boolean; MenuEntries: Byte;
                        var MenuContents: Menurec;
                        var HotKeySet: CharSet);

procedure SearchNextRight(var ArrowPos: byte);
var Counter  : Byte;
    NewArrow : Byte;
begin
  NewArrow := ArrowPos;

  for Counter := ArrowPos to MenuEntries do
    if LineCfg^.LbarContents^[Counter].LightY = LineCfg^.LBarContents^[ArrowPos].LightY then
     if LineCfg^.LBarContents^[Counter].LightX > LineCfg^.LBarContents^[ArrowPos].LightX then
      if ReadBit(LineCfg^.LBarContents^[Counter].Attrib, 0) then
       if CheckMenuAccess(MenuContents[Counter], LineCfg^.Exitinfo^) then
         begin
            NewArrow := Counter;
            BREAK;
         end; { if }

  if ArrowPos = NewArrow then
   for Counter := 01 to ArrowPos do
     if LineCfg^.LbarContents^[Counter].LightY = LineCfg^.LBarContents^[ArrowPos].LightY then
      if LineCfg^.LBarContents^[Counter].LightX > LineCfg^.LBarContents^[ArrowPos].LightX then
       if ReadBit(LineCfg^.LBarContents^[Counter].Attrib, 0) then
        if CheckMenuAccess(MenuContents[Counter], LineCfg^.Exitinfo^) then
          begin
             NewArrow := Counter;
             BREAK;
          end; { if }

  ArrowPos := NewArrow;
end; { proc. SearchNextRight }

procedure SearchNextLeft(var ArrowPos: byte);
var Counter  : Byte;
    NewArrow : Byte;
begin
  NewArrow := ArrowPos;

  for Counter := ArrowPos to MenuEntries do
    if LineCfg^.LbarContents^[Counter].LightY = LineCfg^.LBarContents^[ArrowPos].LightY then
     if LineCfg^.LBarContents^[Counter].LightX < LineCfg^.LBarContents^[ArrowPos].LightX then
      if ReadBit(LineCfg^.LBarContents^[Counter].Attrib, 0) then
       if CheckMenuAccess(MenuContents[Counter], LineCfg^.Exitinfo^) then
         begin
           NewArrow := Counter;
           BREAK;
         end; { if }

  if ArrowPos = NewArrow then
   for Counter := ArrowPos downto 01 do
     if LineCfg^.LbarContents^[Counter].LightY = LineCfg^.LBarContents^[ArrowPos].LightY then
      if LineCfg^.LBarContents^[Counter].LightX < LineCfg^.LBarContents^[ArrowPos].LightX then
       if ReadBit(LineCfg^.LBarContents^[Counter].Attrib, 0) then
        if CheckMenuAccess(MenuContents[Counter], LineCfg^.Exitinfo^) then
          begin
             NewArrow := Counter;
             BREAK;
          end; { if }

  ArrowPos := NewArrow;
end; { proc. SearchNextLeft }

var ArrowKey: Boolean;
    ArrowPos: Byte;
    SaveX   : Byte;
    SaveY   : Byte;
begin
{!}  ArrowPos := 02;
  ArrowPos := Succ(Succ(FVal(Getvalue('/BS=', MenuContents[01].MiscData, false))));
  if ArrowPos = 0 then ArrowPos := 02;

  CorrectArrowPos(ArrowPos, MenuEntries, false);

  SaveX := OutputObj^.WhereX;
  SaveY := OutputObj^.WhereY;

  if LBar then
   if CH = #00 then
    BuildLightBarMenu(ArrowPos, MenuEntries);

  if Ch = #00 then
    repeat
      if LBar then
        begin
          Write(OutputObj^.DoForceY(SaveY));
          Write('`X', SaveX,':');
        end; { if }

      ArrowKey := false;
      CH:=Upcase(InputObj^.ReadKey);

      if LBar then
        Case CH of
          #27 : begin
                  CH := GetArrowKeys;

                  if CH in ['A'..'D'] then
                   begin
                     DisplayLightBar(LineCfg^.LBarContents^[ArrowPos], false);
                     ArrowKey := true;
                   end; { if }

                   Case CH of
                   { Up }    'A' : Dec(ArrowPos);
                   { Down }  'B' : Inc(ArrowPos);
                   { Right } 'C' : SearchNextRight(ArrowPos);
                   { Left }  'D' : SearchNextLeft(ArrowPos);
                   end; { case }

                   CorrectArrowPos(ArrowPos, MenuEntries, (CH = 'A'));

                   if CH in ['A'..'D'] then
                      DisplayLightBar(LineCfg^.LBarContents^[ArrowPos], true);
                end;
          #13 : begin
                  CH := MenuContents[ArrowPos].HotKey[1];
                end;
        end; { case }

    until ((CH in HotKeySet + [#255]) OR (ProgTerminated)) AND (NOT ArrowKey);
end; { proc. GetMenuChoice }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function ExistLightBars(MenuName, MiscData: String): Boolean;
var FullName: String;
    Temp    : Boolean;
begin
  MenuName := SlowCase(MenuName);
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMenu, 'MenuName = ' + LineCfg^.Language^.MenuPath + MenuName + '.mlb');
  {$ENDIF}

  if (FileExist(LineCfg^.Language^.MenuPath + MenuName + '.mlb')) then
    FullName := LineCfg^.Language^.MenuPath + MenuName + '.mlb'
     else FullName := GlobalCfg^.RaConfig^.MenuPath + MenuName + '.mlb';

  Temp := FileExist(FullName);

  if (NOT (Pos('/NG', SupCase(MiscData)) > 00)) then
    if NOT Temp then Temp := FileExist(JustPath(FullName) + 'globalra.mnu');

  ExistLightBars := Temp;
end; { func. ExistLightBars }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure Enter_Menu;
var MenuEntries    : Byte;
    Counter        : Byte;
    CH             : Char;
    HotKeyStr      : String;
    DispHotKeys    : String;
    TempMnu        : MnuRecord;
    MenuError      : Boolean;
    GotoMenu       : Boolean;
    RunAll         : Boolean;
    SupportLBar    : Boolean;
    SaveHotKeySet  : CharSet;
    StartGlobalmenu: Byte;
    HotKeySet      : CharSet;
    SaveRunAll     : Char;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMenu, 'Enter_Menu');
  {$ENDIF}

  if (NOT LineCfg^.ReLogMenu) OR (NOT LineCfg^.ReLogonBBS) then
   with LineCfg^ do
    begin
      Exitinfo^.MenuStackPointer := 01;              { Menu currently viewing }
      Exitinfo^.MenuStack[01] := DefaultMenu;                  { Default menu }
    end;

  InputObj^.DorCleankeys;                            { Clear the keyboard buffer }
  CH := #00;                                                  { Set to 'none' }

  with LineCfg^ do
  REPEAT
  {--------------------- Here we start the indefinite loop --------------------}

    {-------------- Allocate memory for menu/lbar records ---------------------}
    LBarContents := nil;
    MenuContents := nil;
    if NOT AllocMem(MenuContents, SizeOf(MenuRec), 'MenuRec', 'Enter_Menu') then
      begin
        OutputObj^.ClearScreen;
        TextColor(3);
        WriteLn('There is not enough memory available for ', FullProgName);
        WriteLn('to run the menu system for EleBBS');

        HangUp;
        EXIT;
      end; { if }

    if MenuContents <> nil then
     FillChar(MenuContents^, SizeOf(MenuRec), 00);        { Clear menu border }

    {-------------------- Clear the hotkeyset and error flags -----------------}
    HotKeySet := [];
    HotKeyStr := '';
    MenuError := false;
    GotoMenu  := false;
    CH        := #00;


    {---------------------- Setup and check all settings ----------------------}
    OutputObj^.SetStopMore(False);                           { Enable output again }

    if Exitinfo^.MenuStackPointer < 01 then    { Can't be less than 01 (doors) }
      Exitinfo^.MenuStackPointer := 01;

    {--------- Check wether there are lightbars and if they are usable --------}
    SupportLBar := ExistLightBars(Exitinfo^.MenuStack[Exitinfo^.MenuStackPointer], '');
    if ((NOT AnsiON) AND (NOT AvatarON)) then
      SupportLBar := false;

    {------------------ Actually read the menu and lightbars ------------------}
    MenuEntries     := 00;
    StartGlobalmenu := 00;
    if NOT Read_Menu_Structure(Exitinfo^.MenuStack[Exitinfo^.MenuStackPointer],
                               MenuContents^,
                               MenuEntries,
                               false) then
      {---------------------- If menu could not be loaded ---------------------}
      begin
        {$IFDEF WITH_DEBUG}
          DebugObj.DebugLog(logMenu,'   Unable to read the menu!');
        {$ENDIF}

        With Exitinfo^ do
          begin
            MenuStack[MenuStackPointer] := SlowCase(MenuStack[MenuStackPointer]);

            if MenuStack[MenuStackPointer] <> DefaultMenu then
              RaLog('!', MenuStack[MenuStackPointer] + ' menu missing');

            WriteLn('`A12:', LangObj^.ralGet(ralErrMnu), MenuStack[MenuStackPointer]);
            InputObj^.PressEnter(true, true);

            if MenuStack[MenuStackPointer] = DefaultMenu then
              begin
                RaLog('!', DefaultMenu + ' menu missing!');

                WriteLn('`A12:', LangObj^.RalGet(ralErrTop));
                HangUp;
                EXIT;
              end; { TOP menu is missing }
          end; { with }

        Exitinfo^.MenuStackPointer := 01;                   { Set to TOP menu }
        Exitinfo^.MenuStack[Exitinfo^.MenuStackPointer] := DefaultMenu;
        MenuError := TRUE;
      end; { if not read }

    {---------------------- If no error, read GLOBALRA ------------------------}
    if NOT MenuError then
     if (NOT (Pos('/NG', SupCase(MenuContents^[01].MiscData)) > 00)) then
       begin
         StartGlobalmenu := MenuEntries;
         Read_Menu_Structure('globalra',
                             MenuContents^,
                             MenuEntries,
                             true);
       end; { if }

    {-------------------- Generate the hotkey string --------------------------}
    for Counter := 01 to MenuEntries do            { Build the hotkeys string }
      if (MenuContents^[Counter].HotKey[01] <> #01) then       { Not autoexec }
       if CheckMenuAccess(MenuContents^[Counter], LineCfg^.Exitinfo^) then
        begin
          HotKeySet := HotKeySet + [UpCase(MenuContents^[Counter].HotKey[1]),
                                   LowCase(MenuContents^[Counter].HotKey[1])];
          HotkeyStr := HotKeyStr + UpCase(MenuContents^[Counter].HotKey[1]) +
                                   LowCase(MenuContents^[Counter].HotKey[1]);
        end; { if }
    HotKeySet := HotKeySet + [#13,#255];

    {-- Check if the Sysop wants to be able to executee duplicates ---------}
    if POS('/RA', SUpCase(MenuContents^[01].MiscData)) = 00 then
      RunAll := FALSE
        else RunAll := TRUE;

    {---------------------- Start executing the menus -------------------------}
    {$IFDEF WITH_DEBUG}
       DebugObj.DebugLog(logMenu,'  Executing menus');
    {$ENDIF}

    for Counter := 02 to MenuEntries do     { AutoExec functions, skip prompt }
     if CheckMenuAccess(MenuContents^[Counter], LineCfg^.Exitinfo^) then
      begin
        DisplayMenuPrompt(MenuContents^[Counter], MenuContents^[01], False);

        OutputObj^.ResetLines(01);          { We cannot pause during menu display }

        if MenuContents^[Counter].HotKey[01] = #01 then            { Autoexec }
          begin
            {$IFDEF WITH_DEBUG}
              DebugObj.DebugLog(logMenu, '  Executing entry: '+FStr(Counter));
            {$ENDIF}

            {-------------- Make sure ansi's can be aborted with hotkeys ------}
            DispHotKeys := HotKeyStr;
            SaveHotKeySet := HotKeySet;

            if SupportLBar then
              begin
                DispHotKeys := '';
{                HotKeySet := []; }
              end; { If }

            {-------------------- Execute this item ---------------------------}
            CH := Upcase(ExecuteMenuFunc(MenuContents^[Counter].Typ,
                                         MenuContents^[Counter].MiscData,
                                         DispHotKeys,
                                         GotoMenu,
                                         HotKeySet));

            {----------------------- Restore hotkeys --------------------------}
            HotKeySet := SaveHotKeySet;

            if GotoMenu then BREAK;            { If AutoExec=GotoMenu then to }
                                                { another menu and thus abort }


            {---------------- Check if the user has pressed any key -----------}
            if CH=#27 then
             if SupportLBar then
              GetArrowKeys;

            if InputObj^.KeyPressed then
             begin
               CH := InputObj^.ReadKey;

               {------------- Check to see if keypress was cursorkey ----------}
               if SupportLbar then
                if CH=#27 then                      { Discard the lightbar key }
                 GetArrowKeys;

               {------- If keypress was in hotkeyset, abort display -----------}
               if CH <> #13 then
                if CH in HotKeySet then
                  begin
                    BREAK
                  end
                    else CH := #00;
             end; { if keypressed }
          end; { if is an autoexec function }
      end; { for }

    {---------------- Allocate memory if there are lightbars ------------------}
    if SupportLBar then
     begin
       LBarContents := nil;
       if NOT AllocMem(LBarContents, SizeOf(LBarRec), 'LbarRec', 'Enter_Menu') then
         begin
           LBarContents := nil;
           SupportLBar := false;
         end
           else begin
                  FillChar(LBarContents^, SizeOf(LBarRec), 00);
                end; { if }
     end; { if }

     if SupportLBar then
      begin
        Counter := 00;
        ReadLightBars(Exitinfo^.MenuStack[Exitinfo^.MenuStackPointer],
                      LBarContents^,
                      false,
                      Counter);

       if NOT MenuError then
         if (NOT (Pos('/NG', SupCase(MenuContents^[01].MiscData)) > 00)) then
          ReadLightBars('globalra',
                        LBarContents^,
                        true,
                        StartGlobalMenu);
     end; { if }

    {------------------ Check if there are lightbars --------------------------}
    if SupportLBar then
      begin
        SupportLBar := false;
        for Counter := 01 to MenuEntries do
         if ReadBit(LBarContents^[Counter].Attrib, 0) then
             begin
               SupportLbar := true;
               BREAK;
             end; { for }
       end; { if }


    {--------------------- AutoExec function has been finished ----------------}
    LineCfg^.Global_Tagging := false;
    LineCfg^.DispMorePrompt := false;  { Disable more prompt - because of keypress }

    {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logMenu, '  GotoMenu = '+Bool2Str(GotoMenu));
    {$ENDIF}

    if NOT MenuError then
      if NOT GotoMenu then                                 { Show menu prompt }
        begin
          {---------- If key hasn't been pressed, show "Select: prompt --------}
          if CH=#00 then
            begin
              if MenuContents^[01].Display <> '' then
                begin
                  WriteLn;
                  DisplayMenuPrompt(MenuContents^[01], MenuContents^[01], True);
                end; { if }
             end; { if }

          {$IFDEF WITH_DEBUG}
             DebugObj.DebugLog(logMenu, '  Just displayed main menu prompt');
          {$ENDIF}

          {------------- Get the menu choice from user or from lightbars ------}
          GetMenuChoice(CH, SupportLBar, MenuEntries, MenuContents^, HotKeySet);

          {$IFDEF WITH_DEBUG}
              DebugObj.DebugLog(logMenu, '  Read from sysop: '+Ch);
          {$ENDIF}

          {-------------- Check wether displaying the key is allowed ----------}
          if POS('/NO', SUpCase(MenuContents^[01].MiscData)) = 00 then
            begin
              if CH in [#32..#254] then
                Write(UpCase(CH));
              WriteLn;
            end; { if not surpress }
        end; { if }

   {-------------- We got the choice from the user, now do that ---------------}
   HotKeySet      := [];
   LineCfg^.DispMorePrompt := True;
   OutputObj^.ResetLines(01);

   {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logMenu, '  CH <> #00 '+Bool2Str(CH <> #00));
   {$ENDIF}

   {----------- Release memory of lightbar prompts, not needed anymore --------}
   ReleaseMem(LBarContents, SizeOf(LBarRec));

   {------------------ Check to see if hotkey is pressed ----------------------}
   SaveRunAll := CH;

   if CH <> #00 then
    for Counter := 02 to MenuEntries do
     if UpCase(MenuContents^[Counter].HotKey[1]) = UpCase(CH) then
      if CheckMenuAccess(MenuContents^[Counter], LineCfg^.Exitinfo^) then
        begin
          TempMNU := MenuContents^[Counter];

          {-------------- Release memory of the menu ---------------------------}
          if (NOT RunAll) then
            begin
              ReleaseMem(MenuContents, SizeOf(MenuRec));
            end; { if }

          CH := ExecuteMenuFunc(TempMNU.Typ,
                                TempMNU.MiscData,
                                HotKeyStr,
                                GotoMenu,
                                HotKeySet);

          if (NOT RunAll) then BREAK
            else CH := SaveRunAll;
        end; { if User pressed key }

   {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logMenu, '-- End of loop (MENU)');
   {$ENDIF}

   {------------------------ Release memory of the menu -----------------------}
   if MenuContents <> nil then
     ReleaseMem(MenuContents, SizeOf(MenuRec));

 UNTIL (FALSE) OR (Progterminated);
end; { Enter_Menu }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)
{$ENDIF}

end.
