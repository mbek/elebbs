unit InOut_U;
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
** Output Routines for EleBBS
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

{$IFDEF ISCGI}
  This unit should not be compiled into EleWEB
{$ENDIF}

uses CfgRec;

const
  MinToCheck_Char  = 100;     { each 'xx' characters we check for keypress }

type
  Last50LinesRec  = array[1..52] of String[150];

type tOutputObj = Object
        CheckAllCounter: Longint;
        DisplayCounter : Longint;
        DoBufferKeys   : Boolean;
        Last50Lines    : ^Last50LinesRec;
        Last50Counter  : Longint;

        {-- Constructors and destructors ------------------------------------}
        constructor Init;                                       { Initialize }
        destructor Done; virtual;                                     { Done }

        {----------------- ANSI/AVATAR routines -----------------------------}
        procedure SendCharacter(OutChr: Char);           { Send char to port }
        procedure SendString(S: String);
        procedure SetStopMore(St: Boolean);                      { Set more? }
        procedure ResetLines(Ct: Word);              { Reset more line count }
        procedure CheckMore;             { Inc line count and check for more }
{!}     procedure AskMore;                 { Shows moreprompt and ask for it }
        procedure DisplayStr(const OutStr:String; CheckKey: Boolean; const HotKeySet: CharSet);
        procedure DisplayStrLn(const OutStr: String; const HotKeySet: CharSet);     { Disp. with CR/LF }
        procedure DisplayNewLine(const HotKeySet: CharSet);  { Display CR/LF }
        procedure ClearScreen;                                { Clear screen }
        procedure DisplayXY(XL: Byte; YL: Byte);           { GoToXY position }
        procedure DisplayAttr(Attr:Byte);              { set color attribute }
        procedure DoRefresh;                                { Refresh screen }
        function  MakeClrEolStr: String;              { Clr till end of line }
        function  ClearScreenStr: String;                     { Clear screen }
        function  GetAttrStr(Attr:Byte; SetAttr: Boolean): String;
        Function  WhereX: Byte;                                 { Get WhereX }
        Function  WhereY: Byte;                                 { Get WhereY }
        function  MakeXYStr(XL: Byte; YL: Byte): String;        { X/Y cursor }
        function  OneLineUpStr: String;
        function  DoForceY(YL: Byte): String;          { Force to Y position }
        function  AvatarDupl(S: String; Times: Byte): String;
        function  StopMore: Boolean;                                 { More? }
     end; { tOutputObj }

type
  pOutputObj = ^tOutputObj;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Avatar, Syskey, Terminal, FileSys, TagUnit, ViewFile,
     Comunit, Debug_U, Control, LongStr, ExitProg, Global,
     RAL, BitWise, Colors, ElLog_U, JDates, Input_U, Crt,
     OutBlock, Support, Multi, CurTime, ApFossil, ScrnU,
     ObjDec, StUtils

        {$IFDEF WIN32}
          ,SysUtils
        {$ENDIF}

        {$IFDEF WINGUI}
          ,Forms, Windows, Win_Main, ApTimer
        {$ENDIF}

        {$IFNDEF WINGUI}
          , ApTimer, Dos
        {$ENDIF}

        {$IFDEF VirtualPascal}
          ,Strings, VpSysLow
        {$ENDIF}

        {$IFDEF MSDOS}
          ,Strings
        {$ENDIF}, Memman;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Dupl(C: Char; Times: Byte): String;
var Res    : String;
    Counter: Byte;
begin
  Res := '';
  For counter := 01 to Times do
    Res := Res + C;
  Dupl := Res;
end; { func. Dupl }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tOutputObj.MakeClrEolStr: String;
begin
  MakeClrEolStr := '';

  If LineCfg^.AvatarOn Then
    MakeClrEolStr := ^V + ^G
     else If LineCfg^.AnsiOn then MakeClrEolStr := #27 + '[K';

  if (NOT LineCfg^.AvatarOn) AND (NOT LineCfg^.AnsiOn) then
    begin
      MakeClrEolStr := Dupl(#32, Byte((80-WhereY))) +  Dupl(#08, Byte((80-WhereY) * 2));
    end; { if }
end; { func. MakeClrEolStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tOutputObj.ClearScreenStr: String;          { Clear screen }
var TempStr: String;
begin
  TempStr := GetAttrStr(Cyan, False);

  if (NOT LineCfg^.AnsiOn) then TempStr := TempStr + #12;
  if LineCfg^.AnsiOn then TempStr := TempStr + #27'[40m' + #27 + '[1H' +#27+'[2J';

  ClearScreenStr := TempStr;

  {$IFDEF WITH_FULL}
    if ComObj <> nil then
      ComObj^.Com_PurgeOutBuffer;
  {$ENDIF}
end; { proc. ClearScreenStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tOutputObj.ClearScreen;
begin
  ResetLines(01);
  DisplayStr(ClearScreenStr, LineCfg^.PauseChecking, []);

  {$IFNDEF WINGUI}
    AvtObj^.StatusAdjustXY[1] := 01;
    AvtObj^.StatusAdjustXY[2] := 01;
  {$ENDIF}
end; { proc. ClearScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tOutputObj.MakeXYStr(XL: Byte; YL: Byte): String;
begin
  MakeXyStr := '';

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'MakeXYStr-01(XL='+FStr(XL) + ', YL='+FStr(YL));
  {$ENDIF}

  if XL=01 then
   begin
     MakeXYStr := #13;
     EXIT;
   end; { if }
  if XL=WhereX then EXIT;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'MakeXyStr-02(XL='+FStr(XL) + ', YL='+FStr(YL));
    DebugObj.DebugLog(logString, 'MakeXyStr-03(WhereX = ' + FStr(WhereX));
  {$ENDIF}

  If (NOT LineCfg^.AnsiOn) then
    begin
      if XL < WhereX then MakeXYStr := Dupl(#08, (Byte(WhereX) - XL))
        else MakeXyStr := Dupl(#32, (XL - WhereX));
    end; { if }

  if (LineCfg^.AnsiOn) then
    begin
      if XL < WhereX then MakeXYStr := #27 + '[' + FStr(WhereX - XL) + 'D'
        else MakeXyStr := #27 + '[' + FStr(XL - WhereX) + 'C';
    end; { if }
end; { func. InOutObj.MakeXYStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tOutputObj.DisplayXY(XL: Byte; YL: Byte);
begin
  DisplayStr(MakeXyStr(XL, YL), LineCfg^.PauseChecking, []);
end; { proc. DisplayXY }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tOutputObj.GetAttrStr(Attr:Byte; SetAttr: Boolean): String;
begin
  GetAttrStr := '';

  If LineCfg^.AvatarOn then
    GetAttrStr := ^V + ^A + Chr(Attr)
     else begin
            If LineCfg^.AnsiOn then
              GetAttrStr := AvtObj^.AnsiColor(Attr AND 15, Attr SHR 4)
               else GetAttrStr := '';
          end; { else }

  if SetAttr then
    {$IFDEF WINGUI}
      Form1.ColorConsole1.TextAttr := Attr;
    {$ELSE}
      TextAttr := Attr;
    {$ENDIF}
end; { func. GetAttrStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tOutputObj.WhereX: Byte;                               { Get WhereX }
begin
  if AvtObj^.StatusAdjustXY[1] > mnuScrnWidth then
    AvtObj^.StatusAdjustXY[1] := mnuScrnWidth;

  WhereX := AvtObj^.StatusAdjustXY[1];
end; { func. WhereX }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tOutputObj.WhereY: Byte;                               { Get WhereY }
{ Also update AVATAR.PAS ! (getWhereY) }
begin
  if AvtObj^.StatusAdjustXY[2] > LineCfg^.actScrnLen then
     AvtObj^.StatusAdjustXY[2] := LineCfg^.actScrnLen;

  WhereY := AvtObj^.StatusAdjustXY[2];
end; { func. WhereY }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tOutputObj.OneLineUpStr: String;
var TempStr: String;
begin
  TempStr := '';
  OneLineUpStr := '';

  If (LineCfg^.AnsiOn) AND (Not LineCfg^.AvatarOn) then
    TempStr := #27 + '[1A';

  if (LineCfg^.AvatarOn) then
    TempStr := ^V^C;

  OneLineUpStr := TempStr;
end; { func. OneLineUpStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tOutputObj.DoForceY(YL: Byte): String;          { Force to Y position }
begin
  DoForceY := '';

  if (LineCfg^.AvatarOn) then
    begin
      DoForceY := #22#8 + Chr(YL) + Chr(WhereX);
    end
     else begin
            If LineCfg^.AnsiOn then
              DoForceY := #27'[' + FStr(YL) + ';' + FStr(WhereX) + 'H';
          end; { else }
end; { func. DoForceY }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tOutputObj.DisplayAttr(Attr:Byte);
begin
  DisplayStr(GetAttrStr(Attr, False), LineCfg^.PauseChecking, []);
end; { proc. DisplayAttr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tOutputObj.DoRefresh;                         { Refresh the screen }
var Counter   : Longint;
    SaveDisp  : Boolean;
    SaveCapt  : Boolean;
begin
  ClearScreen;
  SaveDisp := LineCfg^.DispMorePrompt;
  LineCfg^.DispMorePrompt := false;

  for Counter := 01 to Last50Counter do
   if NOT StopMore then
    begin
       Write(Last50Lines^[Counter]);
       if Counter <> Last50Counter then WriteLn;
    end; { if }

  LineCfg^.DispMoreprompt := SaveDisp;
end; { proc. DoRefresh }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tOutputObj.DisplayStr(const OutStr:String; CheckKey: Boolean; const HotKeySet: CharSet);
var TempChar  : Char;
    SaveState : Boolean;
    LastKey   : Char;
begin
{$IFDEF WITH_FULL}
  SaveState := AvtObj^.InAvatar;

  Inc(contrObj^.TimeInfo.CharCounter, Length(OutStr));
  Inc(DisplayCounter);

  termObj^.ParseColorString(OutStr);                           { Send to local }

  if contrObj^.TimeInfo.CharCounter >= MinToCheck_Char then
  begin
    {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logString, 'CheckAll - HangUp! (1)');
      DebugObj.DebugLog(logString, 'CheckAll - '+Bool2Str(contrObj^.CheckAll));
    {$ENDIF}

     if NOT contrObj^.CheckAll then Hangup;

    {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logString, 'CheckAll - HangUp! (2)');
      DebugObj.DebugLog(logString, 'CheckAll - ProgTerminated: '+Bool2Str(ProgTerminated));
    {$ENDIF}

     contrObj^.TimeInfo.CharCounter := 00;

     if CheckKey then
      begin
        if HotKeySet = [] then
         if InputObj^.Key_HasPressed(['P', 'p'], LastKey) then
          begin
            InputObj^.DorCleanKeys;
            InputObj^.ReadKey;
          end; { if }

       if InputObj^.Key_HasPressed(LineCfg^.DispAbortSet + HotKeySet, LastKey) then
         begin
           InputObj^.DorCleanKeys;
           SetStopMore(True);
           LineCfg^.DispAbortChar := LastKey;
         end; { if }
      end; { Inputted key }

      {$IFDEF WITH_DEBUG}
        DebugObj.DebugLog(logString, 'CheckAll - HangUp! (3)');
      {$ENDIF}
     end; { if }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'CheckAll ( end )');
  {$ENDIF}
{$ENDIF}
end; { proc. DisplayStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tOutputObj.DisplayStrLn(const OutStr:String; const HotkeySet: CharSet);
begin
  DisplayStr(OutStr + #13#10, LineCfg^.PauseChecking, HotkeySet);
end; { proc. DisplayStrLn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tOutputObj.DisplayNewLine(const HotKeySet: CharSet);
begin
  DisplayStr(#13#10, LineCfg^.PauseChecking, HotKeySet);
end; { proc. DisplayNewLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tOutputObj.SendString(S: String);
var BytesWritten: Longint;
begin
{$IFDEF WITH_FULL}
   if (LineCfg^.Exitinfo^.Baud=00) OR
       (ComObj = nil) OR
        (LineCfg^.LocalOnly) then EXIT;

   OutBlockObj^.DumpBlock;
   ComObj^.Com_SendWait(S[01], Length(s), BytesWritten, {$IFDEF FPC}@{$ENDIF}DoSlice);
{$ENDIF}
end; { proc. SendString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tOutputObj.SendCharacter(OutChr: Char);
var CharWait: LongInt;
begin
{$IFDEF WITH_FULL}
   if (LineCfg^.Exitinfo^.Baud=00) OR
       (ComObj = nil) OR
        (LineCfg^.LocalOnly) then EXIT;

  CharWait := CurrentTime + contrObj^.TimeInfo.IdleTime;

  OutBlockObj^.DumpBlock;
  ComObj^.Com_SendChar(OutChr);
{$ENDIF}
end; { proc. SendCharacter }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tOutputObj.AvatarDupl(S: String; Times: Byte): String;
var TempStr: String;
    Counter: Byte;
begin
  TempStr := '';

  if NOT LineCfg^.AvatarOn then
   begin
      for Counter := 01 to Times do
        TempStr := TempStr + S;
   end; { if }

  if LineCfg^.AvatarOn then
    begin
      TempStr := ^V^Y + Chr(Length(s)) + S + Chr(Times);
    end; { if }

  AvatarDupl := TempStr;
end; { func. AvatarDupl  }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tOutputObj.Askmore;
var OrigAttr      : Byte;
    TempChar      : Char;
    KeysStr       : String;
    Valid         : Boolean;
    Counter       : Word;
    SaveContr     : Boolean;
    SaveRadu      : Boolean;
    Save_Tagging  : Boolean;
    SaveMorePrompt: Boolean;
    SaveCapt      : Boolean;
    SaveBinary    : Boolean;

    InFree,
    OutFree,
    InUsed,
    OutUsed       : Longint;
    FlushTimer    : EventTimer;
    UsePrompt     : Word;
begin
{$IFDEF WITH_FULL}
  if NOT contrObj^.CheckAll then Hangup;
  contrObj^.SetTimeout;

{$IFDEF WITH_FULL}
  if AvtObj^.InAvatar then EXIT;

  {$IFNDEF WINGUI}          { Resetting buffersize to 01 will fix it also !! }
    TextRec(Output).BufPos := 00;
  {$ENDIF}

  {$IFDEF WINGUI}
    TTextRec(Output).BufPos := 00;
  {$ENDIF}

  If (LineCfg^.Exitinfo^.baud<>00) then
   begin
     if ComObj <> NIL then
      if LineCfg^.Exitinfo^.Baud <> 00 then
        ComObj^.Com_GetBufferStatus(InFree, OutFree, InUsed, OutUsed);

     NewTimer(FlushTimer, Secs2Tics(5));

     if ComObj <> nil then
     if LineCfg^.Exitinfo^.Baud <> 00 then
      While ((OutUsed > 25) AND (contrObj^.CheckAll)) AND (NOT TimerExpired(FlushTimer)) do
       begin
         DoSlice;
         ComObj^.Com_GetBufferStatus(InFree, OutFree, InUsed, OutUsed);
      end; { While }
   end; { If }

  ResetLines(01);
  if LineCfg^.IsSearching then UsePrompt := ralMoreTVEK
    else UsePrompt := ralMoreTVE;

  if LineCfg^.DispMorePrompt then
   if (ReadBit(LineCfg^.Exitinfo^.Userinfo.Attribute, 2)) then
    begin;
      SaveContr := LineCfg^.ContrCodes;
      SaveRadu := LineCfg^.RaduCodes;
      SaveBinary := AvtObj^.DoingBinary;
      LineCfg^.ContrCodes := True;
      LineCfg^.RaduCodes := true;
      AvtObj^.DoingBinary := false;

      {$IFNDEF WINGUI}
        OrigAttr := TextAttr;
      {$ELSE}
        OrigAttr := Form1.ColorConsole1.TextAttr;
      {$ENDIF}

      DisplayStr('`F' + FStr(GlobalCfg^.RaConfig^.CRFore) +
                 ':`B' + FStr(GlobalCfg^.RaConfig^.CRBack) + ':', LineCfg^.PauseChecking, []);
      if NOT LineCfg^.Global_Tagging then DisplayStr('`X01:' + LangObj^.ralGetStr(ralMore), LineCfg^.PauseChecking, [])
           else DisplayStr('`X01:' + LangObj^.ralGetStr(UsePrompt), LineCfg^.PauseChecking, []);
      Write('`A', OrigAttr, ':');

      LineCfg^.ContrCodes := SaveContr;
      LineCfg^.RaduCodes := SaveRadu;
      AvtObj^.DoingBinary := SaveBinary;

      if NOT LineCfg^.Global_Tagging then KeysStr := LangObj^.ralGetKeys(ralMore)
        else KeysStr := LangObj^.ralGetKeys(UsePrompt);

      Valid := False;

      While ((NOT Valid) AND (contrObj^.CheckAll)) do
        begin
          if InputObj^.InputChr(TempChar) then
            begin
              if NOT contrObj^.CheckAll then Hangup;

              TempChar := UpCase(TempChar);
              If TempChar=#13 then TempChar := KeysStr[1];
              Save_Tagging := LineCfg^.Global_Tagging;
              LineCfg^.Global_Tagging := False;
              SaveMorePrompt := LineCfg^.DispMorePrompt;

              if TempChar='S' then
                if Pos(TempChar, KeysStr) = 0 then
                 TempChar := KeysStr[2];                       { S is stop }

              Case Pos(TempChar, KeysStr) of
 { Yes }           01 : begin;
                          Valid := True;
                        end;
 { No }            02 : begin;
                          Valid := True;
                          contrObj^.TimeInfo.StopMore := True;
                        end;
 { = (continue) }  03 : begin
                          Valid := True;
                          LineCfg^.DispMorePrompt := False;
                          SaveMorePrompt := False;
                        end;
 { 'T'agfiles }    04 : if Save_Tagging then TagFiles(LineCfg^.HeaderList^);
 { 'V'iewfile }    05 : if Save_Tagging then ViewTagFile(LineCfg^.Headerlist^, LineCfg^.Exitinfo^);
 { 'E'dit tag }    06 : if Save_Tagging then
                         begin
                           EditTagList(True, False, False, False, LineCfg^.Exitinfo^.Userinfo.FileArea);
                           DoRefresh;
                         end; { edit }
 { S'k'ip area }   07 : if LineCfg^.Issearching then
                         begin
                          if Save_Tagging then LineCfg^.Search_Skip := true;
                          Valid := true;
                        end; { Skip }
              end; { Case }

              LineCfg^.Global_Tagging := Save_Tagging;
              LineCfg^.DispMorePrompt := SaveMorePrompt;

              if NOT Valid then
               if Pos(TempChar, KeysStr)>00 then
                begin
                  SaveContr := LineCfg^.ContrCodes;
                  SaveRadu := LineCfg^.RaduCodes;
                  LineCfg^.ContrCodes := True;
                  LineCfg^.RaduCodes := true;

                  DisplayStr('`F' + FStr(GlobalCfg^.RaConfig^.CRFore) +
                             ':`B' + FStr(GlobalCfg^.RaConfig^.CRBack) + ':', LineCfg^.PauseChecking, []);
                  if NOT LineCfg^.Global_Tagging then DisplayStr('`X1:' + LangObj^.ralGetStr(ralMore),
                                                                 LineCfg^.PauseChecking, [])
                     else DisplayStr('`X01:' + LangObj^.ralGetStr(UsePrompt), LineCfg^.PauseChecking, []);

                  LineCfg^.ContrCodes := SaveContr;
                  LineCfg^.RaduCodes := SaveRadu;
                end; { NOT valid }
            End { If Input char }
             else begin
                    contrObj^.CheckAll;
                    if LineCfg^.DoFlushScrnBuffer then
                      InputObj^.UpdateScrn;
                    DoSlice;
                  end; { if }
        End; { WHILE NOT VALID }

     if NOT LineCfg^.Global_Tagging then
       Counter := NoColorLength(langObj^.ralGetStr(ralMore))
        else Counter := NoColorLength(langObj^.ralGetStr(UsePrompt));

      LineCfg^.ContrCodes := True;
      LineCfg^.RaduCodes := true;
      AvtObj^.DoingBinary := false;

      Write('`A', OrigAttr, ':');

      LineCfg^.ContrCodes := SaveContr;
      LineCfg^.RaduCodes := SaveRadu;
      AvtObj^.DoingBinary := SaveBinary;

      KeysStr := Dupl(#08, Counter) + Dupl(#32, Counter) + Dupl(#08, Counter);
      DisplayStr(KeysStr, false, []);
    end; { if dispmore }

  ResetLines(01);
  InputObj^.DorCleanKeys;
{$ENDIF}
{$ENDIF}
end; { proc. AskMore }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tOutputObj.ResetLines(Ct: Word);             { Reset more line count }
begin
{$IFDEF WITH_FULL}
  contrObj^.TimeInfo.CurrLines := CT;
{$ENDIF}
end; { proc. ResetLines }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tOutputObj.SetStopMore(St: Boolean);
begin
{$IFDEF WITH_FULL}
  contrObj^.TimeInfo.StopMore := St;
{$ENDIF}
end; { proc. SetStopMore }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tOutputObj.StopMore: Boolean;                    { More? answer of NO }
begin
{$IFDEF WITH_FULL}
  StopMore := contrObj^.TimeInfo.StopMore;

  if ProgTerminated then
    StopMore := true;
{$ENDIF}
end; { func. StopMore }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tOutputObj.CheckMore;
begin
{$IFDEF WITH_FULL}
  if LineCfg^.DispMorePrompt then
    Inc(contrObj^.TimeInfo.CurrLines);         { increase the line counter }

  If (contrObj^.TimeInfo.CurrLines >= LineCfg^.Exitinfo^.Userinfo.ScreenLength) AND
      (LineCfg^.Exitinfo^.Userinfo.ScreenLength > 00) then
    begin
      If LineCfg^.DispMorePrompt then AskMore
       else ResetLines(01);
    end; { If }
{$ENDIF}
end; { proc. CheckMore }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Constructor tOutputObj.Init;
begin
  {$IFDEF WITH_FULL}
    if NOT AllocMem(Last50Lines, SizeOf(Last50LinesRec), 'Last50LinesRec', 'InOutObj.Init') then
      RaLog('!', 'Unable to gather enough memory!');
    FillChar(Last50Lines^, SizeOf(Last50LinesRec), #00);
    Last50Counter := 01;

    contrObj^.TimeInfo.CurrLines := 0;
    contrObj^.TimeInfo.StopMore := False;
    contrObj^.TimeInfo.TwoMinWarn := False;
    contrObj^.TimeInfo.OneMinWarn := False;
    contrObj^.TimeInfo.TimeOverWarn := False;
    contrObj^.TimeInfo.IdleTime := 180;
    LineCfg^.StartTimingDay := GetDay;
    contrObj^.TimeInfo.StartTime := CurrentTime;
    contrObj^.TimeInfo.InterNodeTime := CurrentTime;
    contrObj^.TimeInfo.SecondsTimer := CurrentTime + 60;
    contrObj^.TimeInfo.CharCounter := 00;
    contrObj^.TimeInfo.InterNodeChecking := false;   { To avoid breaking in during log-on }

    CheckAllCounter:= 0;
    DisplayCounter := 0;
    DoBufferKeys := false;
  {$ENDIF}
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tOutputObj.Done;
begin
  ReleaseMem(Last50Lines, SizeOf(Last50LinesRec));
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { InOut_unit }
