unit SysKey;
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
** SysOp key routines for EleBBS
**
** Copyright (c) 1996, 1997 by Maarten Bekers
**
** Created : 16-Nov-1997
** Last update : 16-Nov-1997
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF WITH_FULL}
  This unit shouldn''t be compiled for an utility program
{$ENDIF}

{$IFDEF ISCGI}
  This unit should not be compiled for EleWEB
{$ENDIF}

{$IFDEF WITH_FULL}
function LocalCommand(CommandChar:Char; UseSysOp: Longint; FromMgr: Boolean): Boolean;
procedure ShowGarbage;
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


{$IFDEF WITH_FULL}
uses Crt, Global, InOut_U, MenuFunc, DispAns, Support, LineEd, ExecFunc,
      StUtils, StatusB, Chat, Colors, Cases, LongStr, WordStr, ExitProg,
       Question, Input_U, Limit_U, ElLog_U, RemScrn, ObjDec, CentrStr,
        {$IFDEF WIN32}
          Windows,
        {$ENDIF}
        
        {$IFDEF VirtualPascal}
        	VpSysLow,
        {$ENDIF}

        {$IFNDEF WINGUI}
          EditUser,
          ScrnU,
        {$ELSE}
          Win_Main,
        {$ENDIF} RAL, FastScrn
        {$IFDEF WITH_DEBUG}
          , Debug_U
        {$ENDIF};

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowGarbage;                          { Show some 'fake' line noise }
var Teller : Byte;
    TempChr: Char;
begin
 Randomize;

 for Teller := 01 to 05 do
  begin
    TempCHR := Chr(Random(256));

    if NOT (TempChr in [#01]) then
      OutputObj^.DisplayStr(Chr(Random(256)), LineCfg^.PauseChecking, []);
  end; { for }

end; { proc. Showgarbage }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_DEBUG}
procedure ToggleDebugLogging;
var SaveScrn: Pointer;
begin
  SaveScreen(SaveScrn);

  { Toggle debuglogging }
  DebugObj.DebugLogging := NOT DebugObj.DebugLogging;

  { And show the sysop what weve done }
  BoxWindow(23, 10, 57, 14, GlobalCfg^.RaConfig^.BorderFore, GlobalCfg^.RaConfig^.BorderBack, '');
  FastWrite(24, 12, (GlobalCfg^.RaConfig^.WindBack SHL 4) OR GlobalCfg^.RaConfig^.WindFore,
                CenterJust('Debuglogging turned ' + Bool2Str(DebugObj.DebugLogging), 30, False));

  { Give the user 2 seconds to look at the nicely status msg }
  {$IFNDEF VirtualPascal}
    Sleep(2000);
  {$ELSE}
    SysCtrlSleep(2000);
  {$ENDIF}

  RestoreScreen(SaveScrn);
end; { proc. ToggleDebugLogging }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AltFKeys(S: String);
var ExitCd      : Word;
    Success     : Boolean;
    AddEnter    : Boolean;
    Quest       : QuestObj;
    ForceInherit: Boolean;
    MonHandle   : Longint;
begin
  {$IFDEF WITH_FULL}
    if Pos('@REFRESH@', SUPcase(S))>00 then
      AddEnter := True
       else AddEnter := false;

   if NOT (LineCfg^.Exitinfo^.MenuStackPointer in [1..50]) then
     LineCfg^.Exitinfo^.MenuStackPointer := 01;

   Replace('@MENU@', LineCfg^.Exitinfo^.MenuStack[LineCfg^.Exitinfo^.MenuStackPointer], S);
                                                        { For current menu }
   Replace('@LANGNR@', FStr(LineCfg^.Exitinfo^.Userinfo.Language), S); { Curr. Lang }
   Replace('@REFRESH@', '', S);

  ForceInherit := Pos('@ELEMON@', SUpCase(S)) > 0;
  MonHandle := -1;

  {$IFNDEF MSDOS}
  if RemScrnObj <> nil then
    begin
      if RemScrnObj ^.Connected then
        begin
          {$IFDEF OS2}
             MonHandle := RemScrnObj^.PipeHandle.SockHandle;
          {$ENDIF}

          {$IFDEF WIN32}
             MonHandle := GetDuplHandle(RemScrnObj^.PipeHandle.SockHandle);
          {$ENDIF}

          Replace('@ELEMON@', '-H' + FStr(MonHandle), S);
        end; { if }
    end; { if }
  {$ENDIF}

   {-- Delete it when its left over ----------------------------------------}
   Replace('@ELEMON@', '', S);

   Case S[1] of
      '?' : ExitWithErrorLevel(Copy(S, 2, 255));
      '#' : DisplayHotFile(Copy(S, 2, 255), []);
      '$' : begin
              Quest.Init;
              Quest.Process(Copy(S, 2, 255), true, '');
              Quest.Done;
             end; { questionnaire }
       else RaExec(Copy(S, 1, 255), False, False, False, ForceInherit, ExitCd, Success);
   end; { case }

   {$IFDEF WIN32}
     {-- Now close the duplicated handle ----------------------------}
     if MonHandle <> -1 then
       Windows.CloseHandle(MonHandle);
   {$ENDIF}

   InputObj^.PutInBuffer(#255, false);
  {$ENDIF}
end; { proc. AltFKeys }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function KeyAccess(AskRemove, FromMgr: Boolean): Boolean;
var TempPsw : String;
    CH      : Char;
    SaveX,
    SaveY    : Byte;
    SaveScrn: Pointer;
begin
{$IFDEF WITH_FULL}
  KeyAccess := true;

  if (GlobalCfg^.RaConfig^.LimitLocal) AND (NOT FromMgr) then KeyAccess := False
   else if (GlobalCfg^.RaConfig^.KeyBoardPwd<> '') AND (NOT PwdAccepted) then
     begin
       SaveX := OutputObj^.WhereX;
       SaveY := OutputObj^.WhereY;

       FastScrn.SaveScreen(SaveScrn);
       lineCfg^.LocalOnly := True;
       TempPsw := '';

       if EditBox(TempPSW, 23, 10, 57, 14, 'Password', 15, true, true) <> #13 then TempPSW := '';

       if SUpcase(TempPsw) <> SUpCase(GlobalCfg^.RaConfig^.KeyBoardPwd) then
        begin
          KeyAccess := False;
          DoBeep;
        end else
          If AskRemove then
            begin
              BoxWindow(23, 10, 57, 14, GlobalCfg^.RaConfig^.BorderFore, GlobalCfg^.RaConfig^.BorderBack, 'Unlock keyboard');
              TextColor(GlobalCfg^.RaConfig^.BorderFore);
              TextBackGround(GlobalCfg^.RaConfig^.BorderBack);

              {$IFNDEF WINGUI} Crt.GotoXY(26, 12); {$ENDIF}
              Fastwrite(26, 12, MakeAttr(GlobalCfg^.RaConfig^.BorderFore, GlobalCfg^.RaConfig^.BorderBack),
                       'Remove password lock (y,N)? °');

              repeat
                {$IFNDEF WINGUI}
                  Crt.GotoXY(54, 12);
                {$ENDIF}

                {$IFNDEF WINGUI}
                  CH := UpCase(Crt.ReadKey);
                {$ELSE}
                  CH := UpCase(Form1.ColorConsole1.ReadKey);
                {$ENDIF}
               until CH in [#13, 'Y', 'N'];

               PwdAccepted := CH = 'Y';
            end; { correct password }

       lineCfg^.LocalOnly := false;
       FastScrn.RestoreScreen(SaveScrn);
       {$IFNDEF WINGUI}
         Crt.GotoXY(SaveX, SaveY);
       {$ENDIF}
     end; { if }
{$ENDIF}
end; { func. KeyAccess }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function LocalCommand(CommandChar:Char; UseSysOp: Longint; FromMgr: Boolean): Boolean;
var ExitCode : Word;
    Success  : Boolean;
    SaveLocal: Boolean;
    CH       : Char;

    TempStr  : String;
    MsgStr   : String;
    Counter  : Longint;
    SaveScrn : Pointer;
begin
  LocalCommand := true;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logInOut, 'LocalCommand('+FStr(Ord(CommandChar))+') - (start)');
  {$ENDIF}

  If lineCfg^.ProcessingLowLevel then EXIT;
  lineCfg^.ProcessingLowLevel := TRUE;

      Case Ord(CommandChar) of
       { Cursor keys }
{ Left }        75   : InputObj^.PutInBuffer(#27 + '[D', true);
{ Right }       77   : InputObj^.PutInBuffer(#27 + '[C', true);
{ Up }          72   : InputObj^.PutInBuffer(#27 + '[A', true);
{ Down }        80   : InputObj^.PutInBuffer(#27 + '[B', true);
{ Insert }      82   : InputObj^.PutInBuffer(#22 + #09, true);
{ Delete }      83   : InputObj^.PutInBuffer(#127, true);
{ Home }        71   : InputObj^.PutInBuffer(#27 + '[H', true);
{ End }         79   : InputObj^.PutInBuffer(#27 + '[K', true);
       { End of cursor keys }

  {$IFNDEF WINGUI}
        59   : Statusdisplay(11, true);
        60   : Statusdisplay(12, true);
        61   : Statusdisplay(13, true);
        62   : Statusdisplay(14, true);
        63   : Statusdisplay(15, true);
        64   : Statusdisplay(16, true);
        65   : Statusdisplay(17, true);
        66   : Statusdisplay(18, true);
        67   : Statusdisplay(19, true);
        68   : Statusdisplay(20, true);
{ PgUp} 81   : if KeyAccess(True, FromMgr) then
                if LineCfg^.Exitinfo^.TimeLimit > 00 then
                  begin
                    {$IFDEF WITH_FULL}
                      ChangeTime(-01, true);

                      StatusDisplay(11, false);
                      StatusDisplay(99, false);
                    {$ENDIF}
                  end; { if }
{ PgDn} 73   : if KeyAccess(True, FromMgr) then
                if LineCfg^.Exitinfo^.TimeLimit < 32000 then
                  begin
                    {$IFDEF WITH_FULL}
                      ChangeTime(+1, true);

                      Statusdisplay(11, false);
                      StatusDisplay(99, false);
                    {$ENDIF}
                  end; { Time Up }
        25 : If KeyAccess(True, FromMgr) then
              LineCfg^.PrinterLogging := NOT LineCfg^.PrinterLogging;
        35 : If KeyAccess(False, FromMgr) then HangUp;
{$IFDEF WITH_FULL}
        24 : If KeyAccess(True, FromMgr) then ChatObj^.PickPaging;
        34 : If KeyAccess(True, FromMgr) then ShowGarbage;
        31 : If KeyAccess(True, FromMgr) then PickSecurity;
        18 : If KeyAccess(True, FromMgr) then begin
                                                UserEditObj^.OnLineEditor := True;
                                                UserEditObj^.UserEdit(True);
                                              end; { ALT-E }
        36 : If KeyAccess(True, FromMgr) then
                    begin
                       LineCfg^.DispMorePrompt := False;

                       if UseSysOp in [0, 1] then
                        begin
                          if UseSysop = 00 then
                            begin
                              WriteLn; WriteLn;
                              WriteLn(LangObj^.ralGet(ralMoment));
                              WriteLn;
                            end
                             else begin
                                    MsgStr := #13#13 + LangObj^.ralGet(ralMoment);
                                    TempStr := '';

                                    for Counter := 01 to Length(MsgStr) do
                                     begin
                                       TempStr := MsgStr[Counter];
                                       ChatObj^.HandleChatInput(MsgStr[Counter], true, TempStr, TempStr);
                                     end; { for }
                                  end; { if }

                          SaveScreen(SaveScrn);
                          lineCfg^.LocalOnly := True;

                          {$IFNDEF WINGUI}
                            Crt.Window(01, 01, 80, mnuScrnLen);
                            Crt.TextAttr := 07;
                            Crt.ClrScr; { Clear also statusline }
                            Partclear(01, 01, 80, mnuScrnLen, White, #32);
                            Crt.GotoXY(01, 04);
                          {$ELSE}
                            (*
                             MkWCrt.Window(01, 01, 80, scrnHeight);
                             MkWCrt.TextAttr := 07;
                             MkWCrt.ClrScr;
                            *)
                          {$ENDIF}
                          Savelocal := lineCfg^.LocalOnly;
                          lineCfg^.LocalOnly := True;
                          FastWrite(01, 01, 07, 'Type "EXIT" to return to ' + FullProgName);
                          lineCfg^.LocalOnly := SaveLocal;
                        end; { if UseSysOp}

                        If GlobalCfg^.RaConfig^.AltJSwap then RaExec('*C *M', True, False, False, False, ExitCode, Success)
                           else RaExec('*C', True, False, False, False, ExitCode, Success);

                       if UseSysOp in [0, 1] then
                         begin
                           RestoreScreen(SaveScrn);

                           lineCfg^.LocalOnly := False;
                           if UseSysop = 00 then
                             begin
                               Writeln(LangObj^.ralGet(ralThanks));
                             end
                              else begin
                                     MsgStr := #13#13+LangObj^.ralGet(ralThanks);
                                     TempStr := '';

                                     for Counter := 01 to Length(MsgStr) do
                                       begin
                                         TempStr := MsgStr[Counter];
                                         ChatObj^.HandleChatInput(MsgStr[Counter], true, TempStr, TempStr);
                                       end; { for }
                                    end; { if }


                           LineCfg^.DispMorePrompt := True;
                         end; { if UseSysOp }
                     end; { DosShell }
{$IFDEF WITH_DEBUG}
        37 : If KeyAccess(True, FromMgr) then ToggleDebugLogging;
{$ENDIF}
        38 : If KeyAccess(False, FromMgr) then LockOutUsers;
        46 : If KeyAccess(True, FromMgr) then
                      begin
                          lineCfg^.ProcessingLowLevel := False;
                          If NOT LineCfg^.NowChatting then
                            ChatObj^.BeginChatting;
                      end; { Chat }
        32 : If KeyAccess(True, FromMgr) then
                       if LineCfg^.Snooping then LineCfg^.Snooping := False
                         else LineCfg^.Snooping := True;
        49 : If KeyAccess(True, FromMgr) then
                      begin
                        if LineCfg^.Exitinfo^.SysOpNext then LineCfg^.Exitinfo^.SysOpNext := False
                          else LineCfg^.Exitinfo^.SysOpNext := True;
                        Statusdisplay(11, false);
                      end; { SwapBoolean }
{$ENDIF}
        104..113: If KeyAccess(True, FromMgr) then AltFKeys(GlobalCfg^.RaConfig^.FKeys[Ord(CommandChar) - 103]);
{$ENDIF}
         else LocalCommand := false;
      End; { Case }

  lineCfg^.ProcessingLowLevel := False;
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logInOut, 'LocalCommand('+FStr(Ord(CommandChar))+') - ( end )');
  {$ENDIF}
end; { func. LocalCommand }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
{$ENDIF}

end. { unit SYSKEY }
