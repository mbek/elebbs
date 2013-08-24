unit Input_U;
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
** Input routines for EleBBS
**
** Copyright (c) 1996, 1997 by Maarten Bekers
**
** Created : 08-Sep-1996
** Last update : 02-Jan-1999
**
** note: The design of this unit is based on MKUSRABS.PAS, (c) by Mark May
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


{$IFNDEF WITH_FULL}
  This unit shouldn''t be compiled for an utility program
{$ENDIF}

uses CfgRec;

{$IFDEF WINGUI}
Const ScrnHeight  = 25;
      ScrnWidth   = 80;
{$ENDIF}

type tInputObj = Object
        Constructor Init;                                        { Initialize }
        Destructor Done; virtual;                                      { Done }

        {---------------------- Input routines ------------------------------}
        function  Key_HasPressed(Chars: CharSet; var LastChar: Char): Boolean; virtual;
        procedure DorCleanKeys;
        procedure PressEnter(Real, AddCr: Boolean);
        function  InputChr(var CH: Char): boolean; virtual;
        function  KeyPressed: Boolean; virtual;              { Is keypressed? }
        function  ReadKey: Char; virtual;                 { CRT-alike readkey }
        procedure WaitEnter; virtual;          { Wait for enter to be pressed }
        function  LocalKeyPressed: Boolean; Virtual;
        function  LocalReadKey: Char; Virtual;

        procedure PutInBuffer(S: String; FromSysOp: Boolean);
        procedure DeleteBufferChar(OffSet: Byte);
        procedure AddCharToBuffer(C: Char; FromSysOp, Emulated: Boolean);

        function YesNoAsk(Default : Boolean): Boolean;
        function RalStrYesNoAsk(Nr : Word): Boolean;
        procedure UpdateScrn;
     end; { InputObj }

type
  pInputObj = ^tInputObj;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Avatar, Syskey, Terminal, FileSys, TagUnit, ViewFile, InOut_U,
     Comunit, Debug_U, Control, LongStr, ExitProg, Cases,
     RAL, BitWise, Colors, ElLog_U, JDates, RemScrn, ScrnU,
     OutBlock, Support, Question, Multi, CurTime, ApFossil, Global,
     elx_BBS, ObjDec

        {$IFDEF WIN32}
          ,SysUtils
        {$ENDIF}

        {$IFDEF WINGUI}
          ,Forms, Windows, Win_Main, ApTimer
        {$ENDIF}

        {$IFNDEF WINGUI}
         , ApTimer, Crt, Dos
        {$ENDIF}

        {$IFDEF VirtualPascal}
          ,Strings, VpSysLow
        {$ENDIF}

        {$IFDEF MSDOS}
          , Strings
        {$ENDIF}, Memman;


procedure tInputObj.PutInBuffer(S: String; FromSysOp: Boolean);
var Counter   : Byte;
begin
{$IFDEF WITH_FULL}
  if S='' then EXIT;
  if (contrObj^.TimeInfo.KeyStart + Length(s)) > 240 then EXIT;

  For Counter := Length(s) downto 01 do
    begin
      If s[counter] = ';' then S[Counter] := #13;
      if S[Counter] = '_' then S[Counter] := #32;
    end; { if }

  for Counter := 01 to Length(s) do
    begin
      AddCharToBuffer(S[Counter], FromSysOp, True);
    end; { if }
{$ENDIF}
end; { proc. PutInBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tInputObj.UpdateScrn;
begin
  UpdateScreenBuffer(true);
  LineCfg^.DoFlushScrnBuffer := false;

  {$IFDEF WINGUI}
    Form1.ColorConsole1.CursorTo(OutputObj^.WhereX, OutputObj^.WhereY);
  {$ELSE}
    {$IFNDEF NOLINLOCAL}
      Crt.GotoXY(OutputObj^.WhereX, OutputObj^.WhereY);
    {$ENDIF}
  {$ENDIF}
end; { proc. UpdateScrn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tInputObj.LocalKeyPressed: Boolean;
var Extra: Boolean;
    Temp : Char;
begin
{$IFDEF WITH_FULL}
  {$IFNDEF WINGUI}
(* Expirmental: 06-Jun-99
    if DoFlushScrnBuffer then
     begin
       UpdateScrn;
     end; { if }
*)
  {$ENDIF}

  if LineCfg^.PauseChecking then
    if Key_HasPressed([], Temp) then ;

  Extra := False;
  if contrObj^.TimeInfo.KeyStart > 00 then
   if (contrObj^.TimeInfo.KeyBuffer[contrObj^.TimeInfo.KeyStart].IsSysop) then
     Extra := true;

  LocalKeyPressed := Extra;
  {$IFNDEF NOLINLOCAL}
    {$IFNDEF WINGUI}
      if Crt.KeyPressed then LocalKeyPressed := true;
    {$ELSE}
      if Form1.ColorConsole1.KeyPressed then LocalKeypressed := true;
    {$ENDIF}
  {$ENDIF}

  if RemScrnObj <> nil then
    if RemScrnObj^.rem_KeyPressed then
      LocalKeyPressed := true;
{$ENDIF}
end; { func. LocalKeyPressed }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tInputObj.DeleteBufferChar(OffSet: Byte);
begin
{$IFDEF WITH_FULL}

  Dec(contrObj^.TimeInfo.KeyStart);

  contrObj^.TimeInfo.KeyBuffer[OffSet].TempC   := #00;
  contrObj^.TimeInfo.KeyBuffer[OffSet].IsSysOp := false;
  contrObj^.TimeInfo.KeyBuffer[OffSet].Emulated:= false;
{$ENDIF}
end; { proc. InOutObj.DeleteBufferChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tInputObj.AddCharToBuffer(C: Char; FromSysOp, Emulated: Boolean);
begin
{$IFDEF WITH_FULL}
  Move(contrObj^.TimeInfo.KeyBuffer[01], contrObj^.TimeInfo.KeyBuffer[02],
         contrObj^.TimeInfo.KeyStart * SizeOf(contrObj^.TimeInfo.Keybuffer[01]));
  Inc(contrObj^.TimeInfo.KeyStart);

  if contrObj^.TimeInfo.KeyStart > 249 then
    contrObj^.TimeInfo.KeyStart := 249;

  contrObj^.TimeInfo.KeyBuffer[01].TempC := C;
  contrObj^.TimeInfo.KeyBuffer[01].IsSysop := FromSysOp;
  contrObj^.TimeInfo.KeyBuffer[01].Emulated := Emulated;

  contrObj^.TimeInfo.KeyBuffer[contrObj^.TimeInfo.KeyStart + 01].TempC := #00;
{$ENDIF}
end; { proc. AddCharToBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tInputObj.LocalReadKey: Char;
var KeyFound: Boolean;
    TempCH  : Char;
    MaxWait : Word;
begin
{$IFDEF WITH_FULL}
  if contrObj^.TimeInfo.KeyStart > 00 then
   If contrObj^.TimeInfo.KeyBuffer[contrObj^.TimeInfo.KeyStart].IsSysOp then
     begin
       LocalReadKey := contrObj^.TimeInfo.KeyBuffer[contrObj^.TimeInfo.KeyStart].TempC;

       DeleteBufferChar(contrObj^.TimeInfo.KeyStart);
       KeyFound := TRUE;
       LineCfg^.SysOpKey := TRUE;

       EXIT;
     end; { if }

  KeyFound := FALSE;
  MaxWait := 00;
  While (NOT Keyfound) AND (contrObj^.CheckAll) do
    begin
      DoSlice;

      Inc(MaxWait);
      if MaxWait > 300 then EXIT;       { Loop for max. 300 times }

      if RemScrnObj <> nil then
        begin
          if RemScrnObj^.rem_keyPressed then
            begin
              TempCH := RemScrnObj^.rem_ReadKey;

              LocalReadKey := TempCH;
              Keyfound := TRUE;
            end; { if Keypressed }
        end; { if }

{$IFNDEF NOLINLOCAL}
{$IFNDEF WINGUI}
      If Crt.KeyPressed then
{$ELSE}
      If Form1.ColorConsole1.Keypressed then
{$ENDIF}
       begin
         {$IFNDEF WINGUI}
           TempCH := Crt.ReadKey;
         {$ELSE}
           TempCH := Form1.ColorConsole1.ReadKey;
         {$ENDIF}

         LocalReadKey := TempCH;
         Keyfound := TRUE;
       end; { if Keypressed }
{$ENDIF - WITH_FULL}
{$ENDIF - NoLinLocal }
    end; { while }
end; { func. LocalReadKey }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tInputObj.Key_HasPressed(Chars: Charset; var LastChar: Char): Boolean;
var Counter   : Byte;
    TempChar  : Char;
    NewChar   : Char;

    MaxRead   : Longint;
    InFree    : Longint;
    OutFree   : Longint;
    InUsed    : Longint;
    OutUsed   : Longint;
    DidRead   : Longint;
    InputBuf  : Array[0..255] of Char;
begin
{$IFDEF WITH_FULL}
  Key_HasPressed := false;

  if ComObj <> NIL then
   if LineCfg^.Exitinfo^.Baud <> 00 then
    While (ComObj^.Com_Carrier) AND (ComObj^.Com_CharAvail) AND
            (contrObj^.Timeinfo.KeyStart < SizeOf(contrObj^.TimeInfo.KeyBuffer)) do
      with contrObj^ do
       begin
         MaxRead := ((SizeOf(TimeInfo.KeyBuffer) div (SizeOf(TimeInfo.KeyBuffer[1]))) - TimeInfo.KeyStart) - 5;

         if MaxRead > 0 then
           begin
             ComObj^.Com_GetBufferStatus(InFree, OutFree,
                                         InUsed, Outused);

             if InUsed < MaxRead then MaxRead := InUsed;

             if MaxRead > 0 then
               begin
                 ComObj^.Com_ReadBlock(InputBuf[0], MaxRead, DidRead);
                 for InUsed := 0 to (DidRead-1) do
                   AddCharToBuffer(InputBuf[Inused], false, false);
               end
                 else BREAK;

             {$IFDEF WITH_DEBUG}
               DebugObj.DebugLog(logInOut, 'Read size(1): '+FStr(DidRead));
             {$ENDIF}
           end
             else BREAK;
      end; { if }

{$IFNDEF NOLINLOCAL}
   {$IFNDEF WINGUI}
     While (Crt.Keypressed) AND (contrObj^.TimeInfo.KeyStart < SizeOf(contrObj^.TimeInfo.KeyBuffer)) do
   {$ELSE}
     While (Form1.ColorConsole1.Keypressed) AND (contrObj^.TimeInfo.KeyStart < SizeOf(contrObj^.TimeInfo.KeyBuffer)) do
   {$ENDIF}
       begin
        {$IFNDEF WINGUI}
          TempChar := Crt.Readkey;
        {$ELSE}
          TempChar := Form1.ColorConsole1.Readkey;
        {$ENDIF}

        if TempChar = #00 then
          begin
            {$IFNDEF WINGUI}
              NewChar := Crt.Readkey;
            {$ELSE}
              NewChar := Form1.ColorConsole1.ReadKey;
            {$ENDIF}

            {$IFNDEF ELEUNIX}
              LocalCommand(NewChar, LineCfg^.DosShellMsgTyp, false);
            {$ENDIF}
          end
          else begin
                 AddCharToBuffer(TempChar, true, false);
               end; { if }
       end; { while }
{$ENDIF - NOLINLOCAL}

   if contrObj^.TimeInfo.KeyStart > 00 then
    For Counter := 0 to contrObj^.TimeInfo.KeyStart do
     if Counter>00 then
      if contrObj^.TimeInfo.KeyBuffer[Counter].TempC in Chars then
       if NOT contrObj^.TimeInfo.KeyBuffer[Counter].Emulated then
        begin
          LastChar := contrObj^.TimeInfo.KeyBuffer[Counter].TempC;
          Key_HasPressed := TRUE;
          Break;
        end; { if }
{$ENDIF}
end; { func. Key_HasPressed }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tInputObj.DorCleanKeys;                { Clean all keyboard buffers }
begin
{$IFDEF WITH_FULL}
  contrObj^.TimeInfo.KeyStart := 00; { Else it reads it owns keys ;-( }
  FillChar(contrObj^.TimeInfo.KeyBuffer, SizeOf(contrObj^.TimeInfo.KeyBuffer), #0);

  While Keypressed do ReadKey;
{$ENDIF}
end; { proc. DorCleanKeys }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tInputObj.InputChr(Var Ch:Char): Boolean;
var OldCH     : Char;
    MaxRead   : Longint;
    InFree    : Longint;
    OutFree   : Longint;
    InUsed    : Longint;
    OutUsed   : Longint;
    DidRead   : Longint;
    InputBuf  : Array[0..255] of Char;
begin
{$IFDEF WITH_FULL}
  InputChr := False;

  if contrObj^.TimeInfo.KeyStart > 00 then
   If (NOT contrObj^.TimeInfo.KeyBuffer[contrObj^.TimeInfo.KeyStart].IsSysOp) then
    With contrObj^.TimeInfo do
     begin
       InputChr := True;
       CH := KeyBuffer[contrObj^.TimeInfo.KeyStart].TempC;

       DeleteBufferChar(contrObj^.TimeInfo.KeyStart);

       LineCfg^.SysopKey := FALSE;
       EXIT;
     end; { if }

  If (LocalKeyPressed) then
   begin
    contrObj^.SetTimeOut;                               { Reset inactivity timeout }
    LineCfg^.SysOpKey := True;

    CH := LocalReadKey;

    If CH = #00 then                                { If extended key pressed }
      begin
        OldCH := LocalReadKey;
        {$IFNDEF ELEUNIX}  { On Unix *all* logins are local, so this would create a security void }
          LocalCommand(OldCH, LineCfg^.DosShellMsgTyp, false);
        {$ENDIF}
      end
        else InputChr := True
   end { If LocalKeyPressed }
    else begin
           if (LineCfg^.Exitinfo^.baud <> 00) then
            if ComObj <> NIL then
             if ComObj^.Com_CharAvail then
                begin
                  MaxRead := ((SizeOf(contrObj^.TimeInfo.KeyBuffer) div
                              (SizeOf(contrObj^.TimeInfo.KeyBuffer[1]))) - contrObj^.TimeInfo.KeyStart) - 5;

                  {$IFDEF WITH_DEBUG}
                    DebugObj.DebugLog(logInOut, 'InputChr() - MaxRead = '+FStr(MaxRead));
                  {$ENDIF}

                  if MaxRead > 0 then
                    begin
                  {$IFDEF WITH_DEBUG}
                    DebugObj.DebugLog(logInOut, 'InputChr()1 - MaxRead = '+FStr(MaxRead));
                  {$ENDIF}
                      ComObj^.Com_GetBufferStatus(InFree, OutFree,
                                                  InUsed, Outused);

                  {$IFDEF WITH_DEBUG}
                    DebugObj.DebugLog(logInOut, 'InputChr()1.5 - after getbuf');
                  {$ENDIF}
                      if InUsed < MaxRead then MaxRead := InUsed;

                      if MaxRead > 0 then
                       begin
                  {$IFDEF WITH_DEBUG}
                    DebugObj.DebugLog(logInOut, 'InputChr()1.7 - before readbuf: ' + fstr(maxread));
                  {$ENDIF}
                         ComObj^.Com_ReadBlock(InputBuf[0], MaxRead, DidRead);

                  {$IFDEF WITH_DEBUG}
                    DebugObj.DebugLog(logInOut, 'InputChr()2 - MaxRead = '+FStr(MaxRead));
                    DebugObj.DebugLog(logInOut, 'InputChr()2 - DidRead = '+FStr(DidRead));
                  {$ENDIF}
                         for InUsed := 0 to (DidRead-1) do
                           AddCharToBuffer(InputBuf[Inused], false, false);

                 {$IFDEF WITH_DEBUG}
                    DebugObj.DebugLog(logInOut, 'InputChr()3 - MaxRead = '+FStr(MaxRead));
                  {$ENDIF}
                       end;

                      {$IFDEF WITH_DEBUG}
                        DebugObj.DebugLog(logInOut, 'Read size(2): '+FStr(DidRead));
                      {$ENDIF}
                    end; { if }
                end; { if }
         end; { else begin }

  OutBlockObj^.DumpBlock;
{$ENDIF}
end; { func. InputChr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tInputObj.WaitEnter;
var TempChar: Char;
begin
{$IFDEF WITH_FULL}
  OutBlockObj^.DumpBlock;
  InputObj^.UpdateScrn;

  TempChar := #01;
  If InputChr(TempChar) then ;

  if TempChar <> #13  then
   repeat
     TempChar := ReadKey;
     DoSlice;
   until (TempChar = #13) OR (NOT contrObj^.CheckAll);

  OutputObj^.ResetLines(01);
  contrObj^.SetTimeOut;
{$ENDIF}
end; { proc. WaitEnter }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Constructor tInputObj.Init;
begin
{$IFDEF WITH_FULL}
  Fillchar(contrObj^.TimeInfo.KeyBuffer, SizeOf(contrObj^.TimeInfo.KeyBuffer), #00);
  contrObj^.TimeInfo.Keystart := 00;
{$ENDIF}
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Destructor tInputObj.Done;
begin
end; { Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tInputObj.KeyPressed: Boolean;                      { Is keypressed? }
var Available: Boolean;
begin
{$IFDEF WITH_FULL}
  Available := FALSE;
  if NOT contrObj^.CheckAll then HANGUP;

  if contrObj^.TimeInfo.KeyStart > 00 then
   if (NOT contrObj^.TimeInfo.KeyBuffer[contrObj^.TimeInfo.KeyStart].IsSysop) then
     Available := true;

  if NOT Available then
   if LocalKeypressed then
       Available := True;

  if NOT Available then
   if LineCfg^.Exitinfo^.Baud <> 00 then
    if ComObj <> NIL then
     if ComObj^.Com_CharAvail then Available := true;

  Keypressed := Available;
{$ENDIF}
end; { func. KeyPressed }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  tInputObj.ReadKey: Char;                        { CRT-alike readkey }
var TempChar: Char;
    DoAbort : Boolean;
begin
 {$IFDEF WINGUI}
   {$IFDEF WITH_FULL}
     if NOT CheckAll then Application.Terminate;
   {$ENDIF}
 {$ENDIF}

  if NOT Keypressed then
   if LineCfg^.DoFlushScrnBuffer then
      UpdateScrn;

  DoAbort := InputChr(TempChar);

  if NOT DoAbort then
    begin
      repeat
       {$IFDEF WITH_FULL}
         if InputChr(TempChar) then DoAbort := true
           else if NOT contrObj^.CheckAll then DoAbort := true
            else if ProgTerminated then DoAbort := true
             else DoSlice;
       {$ENDIF}
      until (DoAbort);
    end; { if }

  OutputObj^.ResetLines(01);                            { Reset lines (!!!!!!!) }
  if ProgTerminated then
    Hangup;

  {$IFDEF WITH_FULL}
    contrObj^.SetTimeOut;
  {$ENDIF}

  ReadKey := TempChar;
end; { func. ReadKey }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tInputObj.PressEnter(Real, AddCr: Boolean);
begin
{$IFDEF WITH_FULL}
  OutBlockObj^.DumpBlock;

  WriteLn;
  If (GlobalCfg^.RaConfig^.DoAfterAction=0) OR (Real) then
      OutputObj^.DisplayStr('`F' + FStr(GlobalCfg^.RaConfig^.CRFore) +
                 ':`B' + FStr(GlobalCfg^.RaConfig^.CRBack) + ':' +
                 LangObj^.ralGet(ralRtnCont) + ^A, LineCfg^.PauseChecking, [])
         else Support.Delay(GlobalCfg^.RaConfig^.DoafterAction*1000);

  If AddCr then OutputObj^.DisplayNewLine([]);

  OutputObj^.ResetLines(01);
  OutputObj^.SetStopMore(False);
{$ENDIF}
end; { proc. PressEnter }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tInputObj.YesNoAsk(Default : Boolean): Boolean;
var TempCH    : Char;
    YesKey,
    NoKey     : Char;
    AddStr    : String;
    elxObj    : pElxBbsObj;
{$IFDEF WITH_FULL}
    Question  : QuestObj;
{$ENDIF}
    TmpResult : Boolean;
begin
  TempCH := #01;

  {---------------- Check if the YESNO.Q-A questionnaire exists ---------------}
  {$IFDEF WITH_FULL}
   if (lineCfg^.AnsiOn) OR (lineCfg^.AvatarOn) then
     begin
      {-- If no extension was specified, assume ELM file if that exists ------}
      if GetScriptType('YESNO') = 'Q-A' then
        begin
          Question.Init;
          if Default then
            Question.Process('YESNO YES /N', true, '')
             else Question.Process('YESNO NO /N', true, '');

          if (SUpCase(Question.GetReturnResult) = 'YES') then
            begin
              YesNoAsk := True;
              Question.Done;
              EXIT;
            end; { if }

          if (SUpCase(Question.GetReturnResult) = 'NO') then
            begin
              YesNoAsk := false;
              Question.Done;
              EXIT;
           end; { if }

          Question.Done;
        end
          else begin
                 {-- Run the code ------------------------------------------}
                 if Default then AddStr := 'YES'
                   else AddStr := 'NO';

                 New(elxObj, Init);
                 if NOT elxObj^.RunElexerScript('yesno', AddStr, false) then
                   begin
                     RaLog('!', 'Error occured while executing YESNO EleXer script - falling back');
                   end
                     else begin
                            {-- now exit -----------------------------------}
                            YesNoAsk := (SUpCase(elxObj^.GetElxReturnString) = 'YES');
                            EXIT;
                          end; { else }
                 Dispose(elxObj, Done);
               end; { if }
     end; { if }
  {$ENDIF}

  {------- Questionnaire did not exist or returned garbage, fall back ---------}
  YesKey := UpCase(LangObj^.ralGetKey(ralYes));
  NoKey := UpCase(LangObj^.ralGetKey(ralNo));

  With GlobalCfg^.RaConfig^, Langobj^ do
   Case Default of
      True  : Write(#32, LeftBracket, YesKey, ralGet(ralSlash), LowCase(NoKey), RightBracket, '? ');
      False : Write(#32, LeftBracket, LowCase(YesKey), ralGet(ralSlash), NoKey, RightBracket, '? ');
   end; { Case }

  repeat
     TempCH := UpCase(ReadKey);

     if TempCH = #13 then TmpResult := Default
      else if TempCH = YesKey then TmpResult := true
       else if TempCH = NoKey then TmpResult := false;
  until (TempCH in [YesKey, NoKey, #13]) OR (ProgTerminated);

  case TmpResult of
    True  : WriteLn(LangObj^.ralGetStr(ralYes));
    False : WriteLn(LangObj^.ralGetStr(ralNo));
  end; { Case }

  YesNoAsk := TmpResult;
end; { func. YesNoAsk }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tInputObj.RalStrYesNoAsk(Nr: Word): Boolean;
begin
  Write('`A14:',Copy(LangObj^.ralGet(nr), 1, Length(LangObj^.RalGet(nr))-1));

  RalStrYesNoask := YesNoAsk(Copy(LangObj^.RalGet(Nr), Length(LangObj^.RalGet(nr)), 1) = 'Y');
end; { func. RalStrYesNoAsk }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { input_u }
