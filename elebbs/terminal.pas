unit Terminal;
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
** TERMINAL routines for EleBBS
**
** Copyright (c) 1996 by Maarten Bekers
**
** Created : 08-Sep-1996
** Last update : 13-Oct-2001
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
  uses CfgRec, Global;
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

type
  {$IFDEF MSDOS}
    termLargestStr = String;
  {$ELSE}
    termLargestStr = AnsiString;
  {$ENDIF}

type tTerminalObj = Object
         CurrentNestedText: Longint;         { default: 00 }
         NrOfDots         : Longint;
         UserHook         : UserHookFunc;    { default: EmptyUserHook; }
         TermHookString   : String;          { default: <empty> }
         DotPadChar       : Char;

         {-- Constructors and destructors ----------------------------------}
         constructor Init;
         destructor Done;

         {-- Generic routines ----------------------------------------------}
         procedure RacodeStr(var S: MacroStr);
         procedure ParseRaCodeStr(var S: termLargestStr);
         procedure ResetTerminal;

         {$IFDEF WITH_FULL}
          procedure ParseColorString(const InputStr: String);
         {$ENDIF}

         {-- Private routines ----------------------------------------------}
         private
           function MacroQaStr(Input, Code: String): String;

           procedure SetProcessor(Processor: processchartyperec; CharToSet: Char);
           {$IFDEF WITH_FULL}
            function Raduprocessor(C: Char): Boolean;
            procedure TextFileProcessor(C: Char);
            procedure ScriptFileProcessor(C: Char);
            procedure ExecuteProcessor(C: Char);
            procedure RaSystemCodes(TempCH: Char);
            procedure RaUserCodes(TempCH: Char);
           {$ENDIF}
     end; { tTerminalObj }

type
  pTerminalObj = ^tTerminalObj;


function  EmptyUserHook(C: Char; var RetStr: Global.UserHookString): Boolean;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Debug_U,

     {$IFNDEF ISCGI}
       Crt,
     {$ENDIF}

     Dos,
     UsrCodes,
     SysCodes,
     MemMan,
     WordStr,
     StUtils,
     Strings,
     CentrStr,
     Elx_BBS,
     ObjDec

    {$IFDEF WITH_FULL}
     , Avatar, Support, Input_U, InOut_U, RAL, Control,
       Cases, ElLog_U, DispAns, Question, ExecFunc, OutBlock,
       LongStr, Colors
     {$ENDIF} ;


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  EmptyUserHook(C: Char; var RetStr: UserHookString): Boolean;
begin
  EmptyUserHook := false;
  RetStr := '';
end; { func. EmptyUserHook }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tTerminalObj.Init;
begin
  CurrentNestedText := 0;
  NrOfDots := 0;
  UserHook := {$IFDEF FPC}@{$ENDIF}EmptyUserHook;
  DotPadChar := '.';
  TermHookString := '';
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tTerminalObj.Done;
begin
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tTerminalObj.MacroQaStr(Input, Code: String): String;
begin
  MacroQaStr := Input;
end; { func. MacroQaStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTerminalObj.ParseRaCodeStr(var S: termLargestStr);
var Counter      : Longint;
    TempStr      : String;
    TempB        : Boolean;
    DotCounter   : Longint;
    SavePos      : Longint;
    UserChar     : Char;
    SystemChar   : Char;
    DummyChar    : String;
begin
  Counter := 01;

  {$IFNDEF ISCGI}
    UserChar := ^F;
    SystemChar := ^K;
    DummyChar := '';
  {$ELSE}
    UserChar := 'F';
    SystemChar := 'K';
    DummyChar := '|';
  {$ENDIF}

  While Counter <= Length(S) do
    begin
      {$IFDEF ISCGI}
      if S[Counter] = '|' then
      {$ENDIF}
        begin
          {$IFDEF ISCGI}
            Inc(Counter);
          {$ENDIF}

          if S[Counter] = SystemChar then
            begin
              Inc(Counter);
              DotCounter := Counter;
              SavePos := DotCounter;

              if S[Counter] in MacroPadSet then
                While (Counter < Length(s)) AND (Counter<60) AND
                       (S[Counter] in MacroPadSet) do
                        Inc(Counter);
              DotCounter := Counter - DotCounter;

                  if S[Counter] in [']', '['] then
                    begin
                      if LineCfg^.Command_Counter = 0 then
                        begin
                          LineCfg^.Command_Counter := 4;
                          LineCfg^.Command_Array[1] := S[Counter];
                          LineCfg^.Command_Array[2] := S[Counter + 1];
                          LineCfg^.Command_Array[3] := S[Counter + 2];
                          LineCfg^.Command_Array[4] := S[Counter + 3];
                        end; { if }

                        Case S[Counter] of
                          ']' : begin
                                    TempStr := RaSystemCodesStr(S[Counter], TempB, DotCounter);
                                    {$IFDEF MSDOS} Replace {$ELSE} ReplaceA {$ENDIF}
                                      (DummyChar + SystemChar + ']' + Copy(S, Counter + 1, 3),
                                            MacroQaStr(DotPadding(TempStr, DotCounter, TempB,
                                            S[SavePos]), SystemChar + ']'), S);
                                    Inc(Counter, 3);
                                end; { if }
                          '[' : begin
                                  TempStr := RaSystemCodesStr(S[Counter], TempB, DotCounter);
                                  {$IFDEF MSDOS} Replace {$ELSE} ReplaceA {$ENDIF}
                                     (DummyChar + SystemChar + '[' + Copy(S, Counter + 1, 2),
                                          MacroQaStr(DotPadding(TempStr, DotCounter, TempB,
                                          S[SavePos]), SystemChar + '['), S);
                                  Inc(Counter, 2);
                                end;
                        end; { case }
                    end { if }
                      else begin
                             TempStr := RaSystemCodesStr(S[Counter], TempB, DotCounter);

                             ReplaceA(DummyChar + SystemChar + Dup(S[SavePos], DotCounter) + S[Counter],
                                     MacroQaStr(DotPadding(TempStr, DotCounter, TempB, S[SavePos]),
                                     SystemChar + UpCase(LineCfg^.Command_Array[1])), S);
                           end; { if }
              end; { if systemchar }

          if Counter <= Length(s) then
           if S[Counter] = UserChar then
            begin
              Inc(Counter);
              DotCounter := Counter;
              SavePos := DotCounter;

              if Counter <= Length(S) then
                if S[Counter] in MacroPadSet then
                  While (Counter < Length(s)) AND (Counter<60) AND
                         (S[Counter] in MacroPadSet) do
                          Inc(Counter);
              DotCounter := Counter - DotCounter;

              TempStr := RaUserCodeStr(S[Counter], TempB);

              {$IFDEF MSDOS} Replace {$ELSE} ReplaceA {$ENDIF}
              (DummyChar + UserChar + Dup(S[SavePos], NrOfDots) + S[Counter],
                  MacroQaStr(DotPadding(TempStr, DotCounter, TempB, S[SavePos]),
                  UserChar + UpCase(LineCfg^.Command_Array[1])), S);
            end; { if userchar }
        end; { if pipe symbol }

      Inc(Counter);
    end; { while }
end; { proc. ParseRaCodeStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tTerminalObj.RacodeStr(var S: MacroStr);
var TmpStr: termLargestStr;
begin
  TmpStr := S;
  ParseRaCodeStr(TmpStr);
  S := TmpStr;
end; { proc. RaCodeStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
procedure tTerminalObj.RaSystemCodes(TempCH: Char);
var TempStr   : String;
    Counter   : Byte;
    WasNumeric: Boolean;
    SystemChar: Char;
    Userchar  : Char;
    DummyChar : String;
begin
  {$IFNDEF ISCGI}
    UserChar := ^F;
    SystemChar := ^K;
    DummyChar := '';
  {$ELSE}
    UserChar := 'F';
    SystemChar := 'K';
    DummyChar := '|';
  {$ENDIF}

  if NOT (TempCH in MacroPadSet) then
    begin
       Inc(LineCfg^.Command_Counter);
       LineCfg^.Command_Array[LineCfg^.Command_Counter] := TempCH;
       LineCfg^.ProcessCharType := chrDisplay;

       TempStr := RaSystemCodesStr(UpCase(LineCfg^.Command_Array[1]), WasNumeric, NrOfDots);
       TempStr := MacroQaStr(DotPadding(TempStr, NrOfDots, WasNumeric, DotPadChar),  SystemChar +
                               UpCase(LineCfg^.Command_Array[1]));

{ !!!!!!!!!!!      DumpBlock(MacroStr); }
       ParseColorString(TempStr);
       If (NOT (LineCfg^.Command_Array[1] in ['[', ']'])) then
         LineCfg^.Command_Counter := 00;
    end; { if valid }

  if LineCfg^.Command_Counter>20 then
    begin
      LineCfg^.Command_Counter := 00;
      LineCfg^.ProcessCharType := chrdisplay;
    end; { No more than 20 }

  if TempCH in MacroPadSet then
    begin
      Inc(NrOfDots);
      DotPadChar := TempCH;
    end; { if }
end; { proc. RaSystemCodes }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

{$IFDEF WITH_FULL}
procedure tTerminalObj.RaUserCodes(TempCH: Char);
var TempStr   : String;
    Counter   : Word;
    SaveAdded : Word;
    WasNumeric: Boolean;
begin
  if NOT (TempCH in MacroPadSet) then
    begin
      Inc(LineCfg^.Command_Counter);
      LineCfg^.Command_Array[LineCfg^.Command_Counter] := TempCH;
      LineCfg^.ProcessCharType := chrDisplay;

      TempStr := RaUserCodeStr(UpCase(LineCfg^.Command_Array[1]), WasNumeric);
      TempStr := MacroQaStr(DotPadding(TempStr, NrOfDots, WasNumeric, DotPadChar),
                              ^F + UpCase(LineCfg^.Command_Array[1])); ;

      ParseColorString(TempStr);
    end; { No dots padding }

  if TempCH in MacroPadSet then
    begin
      Inc(NrOfDots);
      DotPadChar := TempCH;
    end; { if }
end; { proc. RaUserCodes }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

{$IFDEF WITH_FULL}
procedure tTerminalObj.TextFileProcessor(C: Char);
var TempStr   : String;
    ExitCode  : Word;
    SaveBuffer: array[0..127] of Char;
    SavePos   : Word;
begin
  if C <> '|' then
   begin;
     Inc(LineCfg^.Command_Counter);
     LineCfg^.Command_Array[LineCfg^.Command_Counter] := C;
   end; { if }

  if (LineCfg^.Command_Counter > 200) OR (C='|') OR (C=#13) then
    begin
      LineCfg^.ProcessCharType := chrDisplay;
      LineCfg^.Command_Counter := 00;
      Inc(CurrentNestedText);

      Move(TextRec(Output).Buffer, SaveBuffer, Sizeof(SaveBuffer));
      SavePos := TextRec(Output).BufPos;
      TextRec(Output).BufPos := 00;
      Fillchar(TextRec(Output).Buffer, SizeOf(SaveBuffer), #00);

      if CurrentNestedText > MaxNestedText then
        Ralog('!', 'Maximum of nested textfiles exceeded, please fix this error in your textfiles!')
         else DisplayHotFile(Copy(StrPas(LineCfg^.Command_Array), 2, 255), ['S', 's']);
      OutputObj^.ResetLines(01);
      Flush(Output);

      Move(SaveBuffer, TextRec(Output).Buffer, Sizeof(SaveBuffer));
      TextRec(Output).BufPos := SavePos;

      Dec(CurrentNestedText);
    end; { If End of File }

end; { proc. TextFileProcessor }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

{$IFDEF WITH_FULL}
procedure tTerminalObj.ScriptFileProcessor(C: Char);
var Question  : QuestObj;
    elxObj    : pElxBbsObj;
    SaveBuffer: Array[0..127] of Char;
    SavePos   : Word;
    TempStr   : String;
    TempName  : String;
begin
  if C <> '|' then
   begin
     Inc(LineCfg^.Command_Counter);
     LineCfg^.Command_Array[LineCfg^.Command_Counter] := C;
   end; { if }

  if (LineCfg^.Command_Counter > 200) OR (C='|') OR (C=#13)  then
    begin
      LineCfg^.ProcessCharType := chrDisplay;
      LineCfg^.Command_Counter := 00;

      Move(TextRec(Output).Buffer, SaveBuffer, Sizeof(SaveBuffer));
      SavePos := TextRec(Output).BufPos;
      TextRec(Output).BufPos := 00;
      Fillchar(TextRec(Output).Buffer, SizeOf(SaveBuffer), #00);
      Flush(Output);

      {-- Extract the filename -----------------------------------------------}
      TempStr := Copy(StrPas(LineCfg^.Command_Array), 2, 255);
      TempName := FirstWord(TempStr, defExtractWord, true);

      {-- If no extension was specified, assume ELM file if that exists ------}
      if GetScriptType(TempName) = 'Q-A' then
        begin
          Question.Init;
          Question.Process(Copy(Strpas(LineCfg^.Command_Array), 2, 255) + ' /O', true, '');
          Question.Done;
        end
          else begin
                 New(elxObj, Init);
                 elxObj^.RunElexerScript(TempName, TempStr, true);
                 Dispose(elxObj, Done);
               end; { if }

      Move(SaveBuffer, TextRec(Output).Buffer, Sizeof(SaveBuffer));
      TextRec(Output).BufPos := SavePos;
    end; { If End of File }

end; { proc. ScriptFileProcessor }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

{$IFDEF WITH_FULL}
procedure tTerminalObj.ExecuteProcessor(C: Char);
var TempStr : String;
    ExitCode: Word;
    Success : Boolean;
begin
  if C <> '|' then
   begin;
     Inc(LineCfg^.Command_Counter);
     LineCfg^.Command_Array[LineCfg^.Command_Counter] := C;
   end; { if }

  if (LineCfg^.Command_Counter > 200) OR (C='|') OR (C=#13)  then
    begin
      RaExec(Copy(Strpas(LineCfg^.Command_Array), 2, 255), False, False, False, False, ExitCode, Success);
      LineCfg^.ProcessCharType := chrDisplay;
    end; { If End of File }
end; { proc. ExecuteProcessor }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure tTerminalObj.SetProcessor(Processor: processchartyperec; CharToSet: Char);
begin
  LineCfg^.ProcessCharType := Processor;

  FillChar(LineCfg^.Command_Array, SizeOf(LineCfg^.Command_Array), #00);
  LineCfg^.Command_Counter := 00;
  NrOfDots        := 00;
  LineCfg^.Command_Array[LineCfg^.Command_Counter] := CharToSet;
end; { proc. SetProcessor }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure tTerminalObj.ResetTerminal;
begin
  SetProcessor(chrDisplay, #0);
end; { proc. ResetTerminal }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

{$IFDEF WITH_FULL}
function tTerminalObj.Raduprocessor(C: Char): Boolean;

Const RaduParams : CharSet = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'L',
                              'X', 'Y', 'S', 'U', '`'];

function GetRaduParam(S: String): String;
var CommaPos: Byte;
begin
 CommaPos := Pos(':', S);
 GetRaduParam := Copy(S, 3, (Length(S)-3));
end; { func. GetRaduParam }

function GetParam(S:String; NR: Byte):String;
var CommaPos: Byte;
begin;
 CommaPos := Pos(':', S);

 If Nr=01 then GetParam := Copy(S, 3, Pos(',', S)-3)
  else GetParam := Copy(S, Pos(',', S)+1, CommaPos - (Pos(',', S)+1));
end; { func. GetParam }

procedure PositionX(S:String);
begin
 Case S[1] of
  '+' : OutBlockObj^.AddToBlock(OutputObj^.MakeXYStr(Abs(OutputObj^.WhereX+FVal(Copy(S, 2, 255))),
             OutputObj^.WhereY));
  '-' : OutBlockObj^.AddToBlock(OutputObj^.MakeXYStr(Abs(OutputObj^.WhereX-FVal(Copy(S, 2, 255))),
             OutputObj^.WhereY));
  else OutBlockObj^.AddToBlock(OutputObj^.MakeXYStr(FVal(S), OutputObj^.WhereY));
 end
end; { proc. PositionX }

procedure PositionY(S:String);
begin
 Case S[1] of
  '+' : OutBlockObj^.AddToBlock(OutputObj^.DoForceY(Abs(OutputObj^.WhereY+FVal(Copy(S, 2, 255)))));
  '-' : OutBlockObj^.AddToBlock(OutputObj^.DoForceY(Abs(OutputObj^.WhereY-FVal(Copy(S, 2, 255)))));
  else OutBlockObj^.AddToBlock(OutputObj^.DoForceY(FVal(S)));
 end
end; { proc. PositionY }

var CurFore,
    CurBack  : Byte;
begin
 RaduProcessor := true;
 GetColors(TextAttr, CurBack, CurFore);

 if C in [#08] then
   begin
     FillChar(LineCfg^.Command_Array, SizeOf(LineCfg^.Command_Array), #00);
     LineCfg^.Command_Counter := 00;
     NrOfDots        := 00;
     LineCfg^.Command_Array[LineCfg^.Command_Counter] := C;
     EXIT;
   end; { if }

 Inc(LineCfg^.Command_Counter);

 LineCfg^.Command_Array[LineCfg^.Command_Counter] := C;

 if LineCfg^.Command_Counter > 15 then
  begin
    LineCfg^.Command_Counter := 00;
    LineCfg^.ProcessCharType := chrDisplay;
  end; { if }

 if (NOT (UpCase(LineCfg^.Command_Array[01]) in RaduParams)) then
   begin
     OutBlockObj^.AddToBlock('`');
     RaduProcessor := false;
     LineCfg^.Command_Counter := 00;
     LineCfg^.ProcessCharType := chrDisplay;
   end; { if }

 if (LineCfg^.Command_Array[LineCfg^.Command_Counter] in [':','`']) then          { RADU code is completed }
  begin
     {-- Check the parameters more strict if possible ----------------------}
     if UpCase(LineCfg^.Command_Array[1]) in ['C', 'E', 'S', 'U'] then
       begin
         if LineCfg^.Command_Counter <> 2 then
          with LineCfg^ do
           begin
             OutBlockObj^.AddtoBlock(Copy(Command_Array, 1, Command_Counter));
             RaduProcessor := FALSE;
             FillChar(Command_Array, SizeOf(Command_Array), #0);
           end; { if }
       end; { if }

     LineCfg^.Command_Counter := 00;
     LineCfg^.ProcessCharType := chrDisplay;

     Case UpCase(LineCfg^.Command_Array[1]) of
       { Attr }         'A' : OutBlockObj^.AddToBlock(OutputObj^.GetAttrStr(FVal(GetRaduParam(
                                  StrPas(LineCfg^.Command_Array))), True));
       { Background }   'B' : OutBlockObj^.AddToBlock(OutputObj^.GetAttrStr(MakeAttr(CurFore,
                                                   FVal(GetRaduParam(StrPas(LineCfg^.Command_Array)))), True));
       { Blink }        'C' : OutBlockObj^.AddToBlock(OutputObj^.GetAttrStr(Byte(MakeAttr(CurFore,
                                  CurBack) + 128), true));
       { Duplicate }    'D' : OutBlockObj^.AddToBlock(OutputObj^.AvatarDupl(Chr(FVal(GetParam(
                                  StrPas(LineCfg^.Command_Array), 01))),
                                  Byte(FVal(GetParam(StrPas(LineCfg^.Command_Array), 02)))));
       { ClrEol }       'E' : OutBlockObj^.AddToBlock(OutputObj^.MakeClrEolStr);
       { Foreground }   'F' : OutBlockObj^.AddToBlock(OutputObj^.GetAttrStr(MakeAttr(
                                  FVal(GetRaduParam(StrPas(LineCfg^.Command_Array))),
                                                       CurBack), True));
       { DisplayXY }    'G' : begin
                                OutBlockObj^.AddToBlock(OutputObj^.DoForceY(FVal(
                                  GetParam(StrPas(LineCfg^.Command_Array), 02))));
                                OutBlockObj^.AddToBlock(OutputObj^.MakeXYStr(FVal(
                                  GetParam(StrPas(LineCfg^.Command_Array), 01)),
                                  FVal(GetParam(StrPas(LineCfg^.Command_Array), 02))));
                              end;
       { GotoX? }       'X' : PositionX(GetRaduParam(StrPas(LineCfg^.Command_Array)));
       { Goto?Y :) }    'Y' : PositionY(GetRaduParam(StrPas(LineCfg^.Command_Array)));
       { ClrScrn }      'S' : OutBlockObj^.AddToBlock(OutputObj^.ClearScreenStr);
       { LineUp }       'U' : OutBlockObj^.AddToBlock(OutputObj^.OneLineUpStr);
       { Just the ` }   '`' : begin
                                OutBlockObj^.AddToBlock('`');
                                RaduProcessor := false;
                              end; { if }
     End; { Case }
  End; { Command_Array }
end; { proc. RaduProcessor }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

{$IFDEF WITH_FULL}
procedure tTerminalObj.ParseColorString(const InputStr: String);
var Counter   : Longint;
    RetStr    : String;

    SaveExec  : ProcessCharTypeRec;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFastScrn, 'ParseColorString-01 (begin)');
     DebugObj.DebugLog(logFastScrn, 'ParseColorString-02 (InputStr="' + InputStr + '")');
  {$ENDIF}

  Counter := 01;
  While Counter <= Length(InputStr) do
    begin
        if LineCfg^.Command_Counter>19 then
          if (NOT (LineCfg^.ProcessCharType = chrExecuteCodes)) then
            LineCfg^.ProcessCharType := chrDisplay;

        if LineCfg^.ProcessCharType = chrDisplay then
         if UserHook(InputStr[Counter], RetStr) then
           begin
             SaveExec := LineCfg^.ProcessCharType;

             OutBlockObj^.AddToBlock(RetStr);
             OutBlockObj^.DumpBlock;

             LineCfg^.ProcessCharType := chrUserHook;
           end; { else }

        Case LineCfg^.ProcessCharType of
          chrUserHook     : begin
                              LineCfg^.ProcessCharType := SaveExec;
                            end; { if }
          chrExecuteCodes : begin
                              OutBlockObj^.DumpBlock;
                              ExecuteProcessor(InputStr[Counter]);
                            end; { Execute door }
          chrDisplayText  : begin
                               OutBlockObj^.DumpBlock;
                               TextFileProcessor(InputStr[Counter]);
                            end; { nested textfiles }
          chrExecQA       : begin
                               OutBlockObj^.DumpBlock;
                               ScriptFileProcessor(InputStr[Counter]);
                            end; { execute q-a file }
          chrRaSystemCodes: RaSystemCodes(InputStr[Counter]);
          chrRaUserCodes  : RaUserCodes(InputStr[Counter]);
          chrRADU         : if NOT RaduProcessor(InputStr[Counter]) then
                             Dec(Counter);

           else
            if (NOT AvtObj^.InAvatar) then
             Case InputStr[Counter] of
{ Enter }     ^A  : if LineCfg^.ContrCodes then
                    begin
                      OutBlockObj^.DumpBlock;

                      InputObj^.WaitEnter;
                    end; { Enter }
{ dis. Abtr}  ^B  : if LineCfg^.ContrCodes then LineCfg^.DispAbortSet := [];
{ en. Abort}  ^C  : if LineCfg^.ContrCodes then LineCfg^.DispAbortSet := ['S', 's'];
{ en. Cont }  ^D  : if LineCfg^.ContrCodes then LineCfg^.DispMorePrompt := True;
{ dis.Cont }  ^E  : if LineCfg^.ContrCodes then LineCfg^.DispMorePrompt := False;
{ ^F }        ^F  : if LineCfg^.ContrCodes then SetProcessor(chrRaUserCodes, InputStr[Counter]);
{ #10 }       ^J  : begin
                      {$IFDEF ELEUNIX}
                        OutBlockObj^.AddToBlock(#13);
                      {$ENDIF}
                      OutBlockObj^.AddToBlock(#10);
                      OutputObj^.CheckMore;

                      if NOT OutputObj^.StopMore then
                        begin
                          contrObj^.TimeInfo.CharCounter := MinToCheck_Char;
{                          DumpBlock(TermCodesArray^); }
                        end { if }
                         else BREAK;
                    end; { if }
{ ^K }        ^K  : if LineCfg^.ContrCodes then SetProcessor(chrRaSystemCodes, InputStr[Counter]);
{ Wait }      ^W  : if LineCfg^.ContrCodes then
                      begin

                        InputObj^.UpdateScrn;
                        OutBlockObj^.DumpBlock;
                        Support.Delay(1000);
                      end; { 1 second delay }
{ Execute }   ^X  : if LineCfg^.ContrCodes then
                    if LineCfg^.TextFileShells then
                      begin
                        OutBlockObj^.DumpBlock;
                        SetProcessor(chrExecuteCodes, InputStr[Counter]);
                      end; { if }
{ EOF }       ^Z  : ;
              '`' : if LineCfg^.ContrCodes then
                     begin
                       If LineCfg^.RaduCodes then
                         SetProcessor(chrRADU, InputStr[Counter]) else
                           OutBlockObj^.AddToBlock(InputStr[Counter]);
                     end { if }
                       else OutBlockObj^.AddToBlock(InputStr[Counter]);

              else begin
                    OutBlockObj^.AddToBlock(InputStr[Counter]);
                   end; { else }
            end { case }
             else OutBlockObj^.AddToBlock(InputStr[Counter]);
        end; { Case processCharType }

      Inc(Counter);
    end; { while }

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFastScrn, 'ParseColorString ( end )');
  {$ENDIF}
end; { proc. ParseColorString }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

end. { unit. Terminal }
