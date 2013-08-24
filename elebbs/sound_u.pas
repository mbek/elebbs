unit Sound_U;
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
** Sound routines for EleBBS
**
** Copyright (c) 1997 by Maarten Bekers
**
** Created : 12-Oct-1996
** Last update : 12-Oct-1997
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
  This unit should not be compiled into EleWEB
{$ENDIF}

{$IFDEF WITH_FULL}
function  RaYell : Boolean;             { Returns true when sound is allowed }
function  ScrollLockOn: Boolean;
procedure Sound (Hz, Len: Word);   { Same as CRT.Sound when PRA.RaYell=True }
procedure PageSystemOperator(MiscData: String);
procedure NoSound;
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
uses {$IFDEF WINGUI}
        Sysutils,
        JDates,
        Win_Main,
        MusicFormUnit,
     {$ENDIF}

     {$IFDEF MSDOS}
        Crt,
        Dos,
     {$ENDIF}

     {$IFDEF FPC}
        Crt,
        Dos,
     {$ENDIF}

     {$IFDEF VirtualPascal}
        VpUtils,
        Crt,
        Dos,
     {$ENDIF}

        {$IFDEF OS2}
          Os2Base,
        {$ENDIF}

        {$IFDEF Win32}
          Windows,
        {$ENDIF}


       Debug_U, Colors, Access_U, StatusB, ElLog_U, FastScrn, Input_U,
       DispAns, WordStr, Ral, ObjDec, LineEd, CentrStr, WriteMsg,
       Global, Chat, Mail, MemMan, GenDos, OutBLock, StrUnit;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure NoSound;                                     { Same as CRT.NoSound }
begin
 {$IFDEF OS2}
   PlaySound(0, 0);
 {$ENDIF}

 {$IFDEF MSDOS}
   Crt.NoSound;
 {$ENDIF}

 {$IFDEF GO32V2}
   Crt.NoSound;
 {$ENDIF}
end; { proc. NoSound }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  RaYell : Boolean;             { Returns true when sound is allowed }
var Year  : Word;
    Month : Word;
    Day   : Word;
    Dow   : Word;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logChat, 'RaYell');
  {$ENDIF}

  GetDate(Year, Month, Day, Dow);

  Case ChatObj^.RaPageStat of
{ Paging hours } 01 : RaYell := CheckTimeRange(GlobalCfg^.RaConfig^.PageStart[Dow],
                                               GlobalCfg^.RaConfig^.PageEnd[Dow], false);
{ Paging Off }   02 : RaYell := False;
{ Paging On }    03 : RaYell := True;
  end; { Case }
end; { func. RaYell }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure Sound (Hz, Len: Word);         { Same as CRT.Sound when PRA.RaYell=True }
begin
 {$IFNDEF WIN32}
   {$IFNDEF OS2}
     if RaYell then
       Crt.Sound(Hz);
     Delay(len);
     NoSound;
   {$ELSE}
     if RaYell then
       PlaySound(Hz, Len);
   {$ENDIF}
 {$ELSE}
   FastScrn.DoBeep;
 {$ENDIF}
end; { proc. Sound }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure PageSystemOperator(MiscData: String);
var PageFile    : String;
    Key         : Char;
    QuoteText   : pStringArrayObj;
    MsgNr       : LongInt;
    OldKey      : Char;
    Year        : Word;
    Month       : Word;
    Day         : Word;
    Dow         : Word;
    SaveScrn    : Pointer;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logChat, 'PageSystemOperator');
  {$ENDIF}

  If lineCfg^.Exitinfo^.NumberPages >= GlobalCfg^.RaConfig^.MaxPage then
      begin
        if DisplayHotFile('MAXPAGE', []) = #01 then
          begin;
            WriteLn;
            WriteLn('`A12:', LangObj^.ralGet(ralMaxPage1), #32,
                    GlobalCfg^.RaConfig^.MaxPage, #32, LangObj^.ralGet(ralMaxPage2));
            InputObj^.PressEnter(False, false);
          end; { if }
        EXIT;
      end; { Numerpages }

  PageFile := GetValue('/P=', MiscData, True);
  If PageFile='' then PageFile := PageFileName;

  if GlobalCfg^.RaConfig^.WhyPage then
    begin
      OutputObj^.ClearScreen;

      WriteLn('`A12:',RaduCenter(LangObj^.ralGet(ralAskWhy), 79));
      WriteLn;
      Writeln('`A14:`X3:',GlobalCfg^.RaConfig^.LeftBracket, OutputObj^.AvatarDupl(#45, 75), GlobalCfg^.RaConfig^.RightBracket);
      Write('`X4:');
      lineCfg^.Exitinfo^.PageReason := '';
      GetString(lineCfg^.Exitinfo^.PageReason, 75, [#32..#254], False, False, False, False);

      If lineCfg^.Exitinfo^.PageReason='' then Exit;                          { No page reason? Exit }
    end; { if ask why }

  lineCfg^.Exitinfo^.WantChat := true;
  StatusDisplay(99, false);

  If NOT RaYell then
    begin
      RaLog('>', 'Sysop paged (outside paging hours)');

      if DisplayHotFile('NOTAVAIL', []) = #01 then
        begin
          GetDate(Year, Month, Day, Dow);

          WriteLn('`A12:');
          WriteLn(LangObj^.ralGet(ralPageHours));
          WriteLn(GlobalCfg^.RaConfig^.PageStart[Dow], #32, '-', #32, GlobalCfg^.RaConfig^.PageEnd[Dow]);
          Writeln;
          InputObj^.PressEnter(False, False);
        end; { if }

      New(QuoteText, Init(MaxMsgLines));

      if QuoteText <> nil then
       If GlobalCfg^.RaConfig^.LeaveMsg>00 then
         If InputObj^.RalStrYesNoAsk(ralLeaveMsg) then
          WriteMessage(GlobalCfg^.RaConfig^.LeaveMsg,
                     'SysOp',
                     lineCfg^.Exitinfo^.Userinfo.Name,
                     '',
                     '',
                     False,     { reply }
                     false,     { edit }
                     QuoteText^,
                     00,
                     MsgNr,
                     False,
                     True,
                     '',
                     GetAreaAddress(GlobalCfg^.RaConfig^.LeaveMsg),
                     '', '', '', false, true, '', '', '');
      Dispose(QuoteText, Done);

      EXIT;
    end; { not available }

  if GlobalCfg^.RaConfig^.WhyPage then
    RaLog('>', 'Page reason : '+lineCfg^.Exitinfo^.PageReason);
  WriteLn;
  WriteLn;
  Writeln('`A10:', MiscData);
  OutBlockObj^.DumpBlock;

  Key := #00;                                           { SysOp not available }

    SaveScreen(SaveScrn);
    BoxWindow(24, 11, 56, 16, GlobalCfg^.RaConfig^.BorderFore, GlobalCfg^.RaConfig^.BorderBack, 'SysOp page');
    FastWrite(28, 13, MakeAttr(GlobalCfg^.RaConfig^.WindFore, GlobalCfg^.RaConfig^.WindBack), '(C) To break in for a chat');
    FastWrite(28, 14, MakeAttr(GlobalCfg^.RaConfig^.WindFore, GlobalCfg^.RaConfig^.WindBack), '(A) To terminate the page');

    Inc(lineCfg^.Exitinfo^.Numberpages);
    StatusDisplay(6, false);

    If RaYell then
      Key := ChatObj^.DorPage('CcAa', '', PageFile)        { Do actually page the SysOp }
       else Key := #00;

    RestoreScreen(SaveScrn);

  StatusDisplay(99, false);                 { Will restore the old statusline }

  Key := UpCase(key);
  OldKey := Key;

  If Key='A' then begin;
                    RaLog('>', 'Sysop paged (aborted at local console)');
                    Key:=#00;    { When aborted, act like SysOp not available }

                     if DisplayHotFile('PAGEABRT', []) = #01 then
                       begin
                         WriteLn;
                         WriteLn('`A12:',LangObj^.ralGet(ralNotAvail));
                         WriteLn;
                         InputObj^.PressEnter(False, True);
                       end; { if }
                  end; { Aborted }

  If Key=#00 then begin
                   if OldKey <> 'A' then
                    begin;
                      RaLog('>', 'Sysop paged (no response)');

                      if DisplayHotFile('PAGED', []) = #01 then
                       begin;
                         WriteLn;
                         WriteLn('`A12:',LangObj^.ralGet(ralNotAvail));
                         WriteLn;
                         InputObj^.PressEnter(False, True);
                       end; { if }
                    end; { if }

                    New(QuoteText, Init(MaxMsgLines));

                    If QuoteText <> nil then
                    If GlobalCfg^.RaConfig^.LeaveMsg>00 then
                     If InputObj^.RalStrYesNoAsk(ralLeaveMsg) then
                      WriteMessage(GlobalCfg^.RaConfig^.LeaveMsg,
                     'SysOp',
                     lineCfg^.Exitinfo^.Userinfo.Name,
                     '',
                     '',
                     False,     { reply }
                     false,     { edit }
                     QuoteText^,
                     00,
                     MsgNr,
                     False,
                     True,
                     '',
                     GetAreaAddress(GlobalCfg^.RaConfig^.LeaveMsg),
                     '', '', '', false, true, '', '', '');

                     Dispose(QuoteText, Done);
                  end; { SysOp not available }

  If Key='C' then begin;
                    RaLog('>', 'Sysop paged (chatted with user)');
                    lineCfg^.Exitinfo^.WantChat := False;
                    Statusdisplay(99, false);

                    ChatObj^.BeginChatting;
                  end; { If Key='C' }
end; { proc. PageSystemOperator }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function ScrollLockOn: Boolean;
{$IFDEF VirtualPascal}
Const
  kbd_ScrollLock = {$IFDEF OS2} kbdstf_ScrollLock_On {$ELSE} VK_SCROLL {$ENDIF} ;
{$ENDIF}
begin
  ScrollLockOn := false;

 {$IFDEF MSDOS}
   if ((Mem[$0:$0417] AND 16)=16) then ScrollLockOn := true;
 {$ENDIF}

 {$IFDEF OS2}
   if VpUtils.GetKeyboardState(kbd_ScrollLock) then ScrollLockOn := true;
 {$ENDIF}

 {$IFDEF WIN32}
   Result := (GetAsyncKeyState(VK_SCROLL) <> 0);
 {$ENDIF}
end; { func. ScrollLockOn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)
{$ENDIF}

end.
