unit QASYS;
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
** Q-A script routines for EleBBS.
**
** Copyright (c) 1996 - 1999 by Maarten Bekers
**
** Created : 08-Sep-1996
** Last update : 20-Aug-1999
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Strunit, CfgRec, FileObj;


Const
{$IFDEF MSDOS}
      QAMaxAnswers   = 50;
{$ELSE}
      QAMaxAnswers   = 100;
{$ENDIF}
      QAMaxOutput    = 100;
      QAMaxFiles     = 10;
      MaxIf          = 120;
      MaxGoSub       = 20;
      MaxWhile       = 5;
      MaxFileStack   = 5;
      {$IFDEF ISCGI}
        TermStringChar = '''';
      {$ELSE}
       TermStringChar = '"';
      {$ENDIF}

      CrLf           = #13#10;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

type
  MacroCodeProc  = procedure(var OutStr: MacroStr);
  OutputCodeProc = procedure(OutStr: MacroStr);
  GetInfoHookProc= procedure(RecordNum: Longint; var Answers: StringArrayObj; StartRecord: Longint;
                             GoingDown: Boolean; var GlobalFile: pFileObj);
  LogInfoProc    = procedure(const Sev: Char; const OutStr: MacroStr);
  AbortHookFunc  = function: Boolean;
  DoSliceProc    = procedure(var SliceCnt: Longint);

     QuestInfoType = Record
       OutputProc   : OutputCodeProc;
       GetInfoHook  : GetInfoHookProc;
       LogHook      : LogInfoProc;
       AbortHook    : AbortHookFunc;
       SliceHook    : DoSliceProc;
       Answers      : ^StringArrayObj;
       PostLines    : ^StringArrayObj;
       Name         : String[255];
       OutPutFile   : String;
       GetInfoFile  : pFileObj;
       WhileCmd     : Array[1..MaxWhile] of String;
       Error        : Word;
       QF           : Array[1..QAMaxFiles] of Text; { was: TFile }
       CurrentFile  : Byte;
       Finished     : Boolean;
       OutLines     : Word;
       SkippingIf   : Boolean;
       SkippingWhile: Boolean;
       IfStackPtr   : Word;
       IgnoredIf    : Word;
       IfStack      : Array[1..MaxIf] of Boolean;
       GoSubPtr     : Word;
       WhilePtr     : Word;
       GoSubStack   : Array[1..MaxGoSub] of LongInt;
       WhileStack   : Array[1..MaxWhile] of LongInt;
       FileStack    : Array[1..MaxFileStack] of pFileObj;
       FileResult   : Array[1..MaxFileStack] of String[20];
       Capitalize   : Boolean;
       ReturnResult : String;
       Parameters   : String;
       QuesPath     : String;
       ScriptName   : String;
       SavedX       : Array[1..20] of Byte;
       SavedY       : Array[1..20] of Byte;
       SavedAttr    : Array[1..20] of Byte;
     end; { QuestInfoType (record) }

  SplitLongType   = Array[1..10] of Longint;
  SplitStringType = Array[1..10] of String;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

  CommandHookProc= function(var   SplitUpStr  : SplitStringType;
                            var   SplitUpLong : SplitLongType;
                            const CommandStr  : MacroStr;
                            const CommandCount: Longint;
                                  QaObj       : Pointer): Boolean;

Type QaSysObj = Object
      QInfo: ^QuestInfoType;

      constructor Init;                                        { Initialize }
      destructor Done;                                               { Done }

      function GetReturnResult: String;
      function FindLabel(LabelToSearch: String): Boolean;
      function GetError: Word;                             { Get error code }

      procedure Process(MiscData: String;
                        StartLabel: String;
                        OutputName: String;
                        CommandHook: CommandHookproc);   { Load Q-A file and execute }
      procedure AddStr(TempStr: String);                { Add output string }
      procedure HandleCommand(const CommandStr: String;
                              CommandHook   : CommandHookProc;
                              var SliceCount: Longint); { Process one line }
      procedure QuesCommit;                                  { Commits data }

      function  TestIfCommand(CommandStr: String): Boolean;
      function MakeDisplayStr(var TempStr: String; Remove: Boolean): String;
     end; { QuestObj }



(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

 uses Dos,
      {$IFDEF WINGUI}
        GenDos,
      {$ENDIF}
       SysUtils,
        WordStr,
         Cases,
          CentrStr,
           GenFile,
            LongStr,
             StrPath,
              StUtils,
               ObjDec;
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EmptyMacroStr(var MacroStr: MacroStr); {$IFDEF MSDOS}   {$ENDIF}
begin
end; { proc. EmptyMacroStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EmptyGetInfoHook(RecordNum: Longint; var Answers: StringArrayObj; StartRecord: Longint; GoingDown: Boolean;
                           var GlobalFile: pFileObj);
begin
end; { proc. EmptyMacroStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DefaultOutputStr(OutStr: MacroStr); {$IFDEF MSDOS}  {$ENDIF}
begin
  Write(OutStr);
end; { proc. DefaultOutputStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EmptyLogHook(const Sev: Char; const OutStr: MacroStr); {$IFDEF MSDOS}  {$ENDIF}
begin
end; { proc. EmptyLogHook }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function EmptyAbortHook: Boolean;
begin
  EmptyAbortHook := false;
end; { func. EmptyAbortHook }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EmptySliceHook(var SliceCnt: Longint);
begin
end; { proc. DoSliceHook }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{ This is needed because we may not support ";" for breaking up lines!! }
function ExtractWord(Str: String; N: Integer): String;
begin
  ExtractWord := WordStr.ExtractWord(Str, N, defExtractWord - [';'], false, false);
end; { func. ExtractWord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function WordCount(Str: String): Integer;
begin
  WordCount := WordStr.WordCount(Str, defExtractWord - [';'], false);
end; { func. WordCount }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor QaSysObj.Init;                                     { Initialize }
var Counter: Longint;
begin
  Randomize;
  QInfo := nil;
  GetMem(QInfo, SizeOf(QuestInfoType));

  if QInfo = nil then FAIL
    else begin
           FillChar(QInfo^, SizeOf(QuestInfoType), 00);
           New(Qinfo^.Answers, Init(qaMaxAnswers));
           New(Qinfo^.PostLines, Init(qaMaxOutput));

           QInfo^.Capitalize := false;
           FillChar(QInfo^.WhileCmd, SizeOf(QInfo^.WhileCmd), #0);

           QInfo^.OutputProc := {$IFDEF FPC}@{$ENDIF}DefaultOutputStr;
           QInfo^.GetInfoHook := {$IFDEF FPC}@{$ENDIF}EmptyGetInfoHook;
           QInfo^.LogHook := {$IFDEF FPC}@{$ENDIF}EmptyLogHook;
           QInfo^.AbortHook :=  {$IFDEF FPC}@{$ENDIF}EmptyAborthook;
           Qinfo^.SliceHook := {$IFDEF FPC}@{$ENDIF}EmptySliceHook;
           Qinfo^.ScriptName := '';
           QInfo^.QuesPath := '';

           for Counter := 01 to MaxFileStack do
             New(Qinfo^.FileStack[Counter], Init);
         end; { if }
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function QaSysObj.GetReturnResult: String;
begin
  if QInfo <> nil then GetReturnResult := QInfo^.ReturnResult
    else GetReturnResult := '';
end; { func. GetReturnResult }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function QaSysObj.GetError: Word;
begin
  if QInfo <> nil then GetError := QInfo^.Error
    else GetError := 203;
end; { func. Geterror }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function QaSysObj.FindLabel(LabelToSearch: String): Boolean;
var SavePos   : Longint;
    TempStr   : String;
    FoundLabel: Boolean;
begin
  LabelToSearch := SUpCase(Trim(LabelToSearch));
  if Pos('#', LabelToSearch) > 0 then
    begin
      TempStr := Copy(LabelToSearch, Pos('#', LabelToSearch) + 1, 255);
      Delete(LabelToSearch, Pos('#', LabelToSearch), 255);

      LabelToSearch := LabelToSearch +
                       SUpCase(QInfo^.Answers^.Get(FVal(TempStr)));
    end; { if }

  With QInfo^ do
    begin
      SavePos := TextFilePos(QF[CurrentFile]);
      TextSeek(QF[CurrentFile], 0);

      FoundLabel := false;

      While (NOT FoundLabel) do
        begin
          {$i-}
            ReadLn(QF[CurrentFile], TempStr);
          {$i+}
          if IOResult > 00 then BREAK;
          TempStr := SupCase(Trim(TempStr));

          if TempStr[01] = ':' then                      { This is a label }
             begin
               TempStr := Copy(TempStr, 2, Length(TempStr) - 1);

               if Pos(#32, TempStr) > 00 then         { Copy the first word }
                 TempStr := SupCase(Copy(TempStr, 1, Pos(#32, TempStr) - 1));

               if TempStr = LabelToSearch then
                 FoundLabel := true;
             end; { if }

          if EOF(QF[CurrentFile]) then BREAK;
        end; { While }

      if NOT FoundLabel then
        begin
          TextSeek(QF[CurrentFile], SavePos);

          LogHook('>', 'Could not find label in GOTO/GOSUB statement: '+LabelToSearch);
        end; { Label not found }
    end; { with }

  FindLabel := Foundlabel;
end; { func. FindLabel }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure QaSysObj.AddStr(TempStr: String);
begin
  With QInfo^ do
    begin
      if OutLines < QaMaxOutput then
        begin
          Inc(OutLines);
          PostLines^.Put(Outlines, TempStr);
        end; { if }
    end; { with }
end; { proc. AddStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function QaSysObj.TestIfCommand(CommandStr: String): Boolean;
var WorkStr      : String;
    Variable1    : String;
    Variable2    : String;
    ComparStr    : String;
    AtVarNum     : Longint;
    Counter      : Longint;
    TempBool     : Boolean;
    NumericComp  : Boolean;
begin
  WorkStr := Copy(CommandStr, 4, 255); { Skip "IF" }
  WorkStr := Trim(WorkStr);

  Variable1   := '';
  Variable2   := '';
  ComparStr   := '';
  AtVarNum    := 00;
  NumericComp := false;
  TempBool    := false;

  Counter := 01;
  While (WorkStr[Counter] in ['0'..'9', '~']) AND
         (Counter <= Length(WorkStr)) do
          begin
            Variable1 := Variable1 + WorkStr[Counter];
            Inc(Counter);
          end; { While }

  Delete(WorkStr, 01, Counter - 1);      { Delete }
  WorkStr := Trim(WorkStr);

  ComparStr := FirstWord(WorkStr, defExtractWord, false); { Get comparstr and remove it}

  Variable1 := Trim(Variable1);
  Variable2 := Trim(WorkStr);
  ComparStr := Trim(ComparStr);

  if Variable1[Length(Variable1)] = '~' then
    begin
      SetLength(Variable1, Length(Variable1) - 1);
      NumericComp := true;
    end; { if }

  if FVal(Variable1) in [1..qaMaxAnswers] then
    Replace(Variable1, QInfo^.Answers^.Get(FVal(Variable1)), Variable1);

  if Variable2[1] in ['#', '%'] then
    begin
      Variable2 := Copy(Variable2, 2, 255);

      if WordCount(Variable2) > 1 then
        begin
          if SUpCase(ExtractWord(Variable2, 2)) = 'AT' then
           begin
             AtVarNum := FVal(ExtractWord(Variable2, 3));
          end; { if }

          Variable2 := ExtractWord(Variable2, 1);
        end; { if }


      if FVal(Variable2) in [1..qaMaxAnswers] then
        Replace(Variable2, QInfo^.Answers^.Get(FVal(Variable2)), Variable2);
    end; { if Variable = '#' }

  if Variable2[1] = TermStringChar then
    Variable2 := MakeDisplayStr(Variable2, false);

  termObj^.RaCodeStr(Variable1);
  termObj^.RaCodeStr(Variable2);

  Variable1 := SUpcase(Variable1);
  Variable2 := SUpcase(Variable2);

  if ComparStr = '==' then ComparStr := '=';
  ComparStr := SUpCase(ComparStr);

  if NumericComp then
    begin
      if ComparStr = '='   then TempBool := (FVal(Variable1) = FVal(Variable2));
      if ComparStr = '>'   then TempBool := (FVal(Variable1) > FVal(Variable2));
      if ComparStr = '<'   then TempBool := (FVal(Variable1) < FVal(Variable2));
      if ComparStr = '<>'  then TempBool := (FVal(Variable1) <> FVal(Variable2));
      if ComparStr = '>='  then TempBool := (FVal(Variable1) >= FVal(Variable2));
      if ComparStr = '=>'  then TempBool := (FVal(Variable1) >= FVal(Variable2));
      if ComparStr = '<='  then TempBool := (FVal(Variable1) <= FVal(Variable2));
      if ComparStr = '=<'  then TempBool := (FVal(Variable1) <= FVal(Variable2));
      if ComparStr = 'IN'  then begin
                                  TempBool := (Pos(Variable1, Variable2) > 00);

                                  if TempBool then
                                    if AtVarNum in [1..QaMaxAnswers] then
                                      QInfo^.Answers^.Put(AtVarNum, FStr(Pos(Variable1, Variable2)));
                                end; { if }
      if ComparStr = 'NIN' then TempBool := (Pos(Variable1, Variable2) = 00);
    end { if }
     else begin
            if ComparStr = '='   then TempBool := (Variable1 = Variable2);
            if ComparStr = '>'   then TempBool := (Variable1 > Variable2);
            if ComparStr = '<'   then TempBool := (Variable1 < Variable2);
            if ComparStr = '<>'  then TempBool := (Variable1 <> Variable2);
            if ComparStr = '>='  then TempBool := (Variable1 >= Variable2);
            if ComparStr = '=>'  then TempBool := (Variable1 >= Variable2);
            if ComparStr = '<='  then TempBool := (Variable1 <= Variable2);
            if ComparStr = '=<'  then TempBool := (Variable1 <= Variable2);
            if ComparStr = 'IN'  then begin
                                        TempBool := (Pos(Variable1, Variable2) > 00);

                                        if TempBool then
                                          if AtVarNum in [1..QaMaxAnswers] then
                                            QInfo^.Answers^.Put(AtVarNum, FStr(Pos(Variable1, Variable2)));
                                      end; { if }
            if ComparStr = 'NIN' then TempBool := (Pos(Variable1, Variable2) = 00);
          end; { else }

  TestIfCommand := TempBool;
end; { func. TestIfCommand }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure QaSysObj.Process(MiscData    : String;   { Load Q-A file and exec }
                           StartLabel  : String;
                           OutputName  : String;
                           CommandHook : CommandHookProc);
var NoLog      : Boolean;
    OnlyError  : Boolean;
    SliceCount : Longint;           { Counter used to slice once in a while }
    MaxSlice   : Longint;
    ScriptStr  : String;
begin
  if QInfo = nil then EXIT;

  {----------------------- Check the log settings and stuff -----------------}
  NoLog := (Pos('/N', MiscData) > 00);
  if NOT NoLog then
    OnlyError := (Pos('/O', MiscData) > 00)
      else OnlyError := false;
  if OnlyError then NoLog := true;
  Replace('/N', '', MiscData);

  {--------------------- Setup all the information data ---------------------}
  With QInfo^ do
    begin
      SliceCount := 00;

      SkippingIf := false;
      IgnoredIf := 00;
      IfStackPtr := 00;
      GoSubPtr := 00;
      WhilePtr := 00;

      OutLines := 00;
      Error := 00;
      Finished := false;
      Capitalize := false;
      CurrentFile := 01;
      OutputFile := OutputName;

      Name := NoExtension(WordStr.ExtractWord(MiscData, 1, defExtractWord, true, false));
      Name := Slowcase(Trim(Name));
    end; { with }

  {--------------------- Try to open the questionnaire ----------------------}
  With QInfo^ do
    begin
      Assign(QF[CurrentFile], QuesPath + Name + '.q-a');
      {$i-}
        System.Reset(QF[CurrentFile]);
      {$i+}
      if IoResult > 0 then
        begin
          if (NOT NoLog) OR (OnlyError) then
            LogHook('!', 'Unable to find questionnaire file: ' + Name + '.q-a');

          Error := 103;                                   { File not opened }
          EXIT;
        end; { if }
    end; { with }

  if NOT NoLog then
    QInfo^.LogHook('>', 'Questionnaire '+ QInfo^.Name +' initiated:');


  {------------ Questionnaire opened, now remove this parameter -------------}
  FirstWord(MiscData, defExtractWord, true);       { Remove the name of this }

  QInfo^.Parameters := MiscData;    { Pass the rest of the params to the Q-A }

  if StartLabel <> '' then
    FindLabel(StartLabel);

  {------------------------ Start the program loop --------------------------}
  With QInfo^ do
    While (NOT EOF(QF[CurrentFile])) AND
           (NOT Finished) AND
            (NOT AbortHook{$IFDEF FPC}(){$ENDIF}) do
              begin
                {------------ Read the scriptline and execute it ------------}
                {$i-} ReadLn(Qf[CurrentFile], ScriptStr); {$i+}
                if IOResult > 00 then BREAK;

                HandleCommand(Trim(ScriptStr), CommandHook, SliceCount);

                {------ Make sure there will be sliced once in a while ------}
                SliceHook(SliceCount);

                {----- If at the end of a script, see if we can fallback ----}
                if (EOF(Qf[CurrentFile])) then
                 if CurrentFile > 01 then
                   begin
                     {$I-} Close(QF[CurrentFile]); {$i+}
                     if IoResult > 0 then ;

                     Dec(CurrentFile);
                   end; { if }
              end; { while }

  {---------------------- Shutdown the questionnaire system -----------------}
  {$i-} Close(QInfo^.Qf[QInfo^.CurrentFile]); {$I+}
  if IOResult > 00 then ;

  QuesCommit;            { Make sure all data is written to the output file }

  If NOT NoLog then
    QInfo^.LogHook('>', 'Questionnaire completed');
end; { proc. Process }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor QaSysObj.Done;                                              { Done }
var Counter: Longint;
begin
  for Counter := 01 to MaxFileStack do
    if QInfo^.FileStack[Counter] <> NIL then
      Dispose(QInfo^.FileStack[Counter], Done);

  if QInfo^.PostLines <> nil then
    Dispose(Qinfo^.PostLines, Done);
  if QInfo^.Answers <> nil then
    Dispose(QInfo^.Answers, Done);
  FreeMem(QInfo, SizeOf(QuestInfoType));
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function QaSysObj.MakeDisplayStr(var TempStr: String; Remove: Boolean): String;
var OldStr  : String;
    NewStr  : String;
    TempPos : Byte;
    Counter : Longint;
begin
  TempStr := Trim(TempStr);
  OldStr := TempStr;
  NewStr := '';

  { Check if first character is a ", if so treat it as we should }
  if TempStr[01] = TermStringChar then
    begin
      Delete(TempStr, 1, 1);                        { Remove this character }

      TempPos := 01;
      While ((TempStr[TempPos] <> TermStringChar) OR
             (TempStr[TempPos - 1] = '\')) AND (TempPos <= Length(TempStr)) do
        begin
          Inc(TempPos);
        end; { while }

(*
      TempPos := Pos(TermStringChar, TempStr);     { Calculate the position }
*)
      if TempPos = 00 then TempPos := 255;

      Counter := 01;
      NewStr := '';
      While (TempPos > Counter) do
        begin
          if TempStr[Counter] <> '\' then
            NewStr := NewStr + TempStr[Counter];

          Inc(Counter);
        end; { while }

      Delete(TempStr, 1, TempPos);        { Remove this whole "<str>" range }
    end; { if }

  TempStr := Trim(TempStr);

  if (FVal(ExtractWord(TempStr, 1)) in [1..qaMaxAnswers]) then
    begin
      NewStr := NewStr + QInfo^.Answers^.Get(FVal(ExtractWord(TempStr, 1)));

      Delete(TempStr, 01, Length(ExtractWord(TempStr, 1)));
    end; { if }

  MakeDisplayStr := NewStr;
  if NOT Remove then TempStr := OldStr;
end; { func. MakeDisplayStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure QaSysObj.HandleCommand(const CommandStr: String;
                                 CommandHook   : CommandHookProc;
                                 var SliceCount: Longint); { Process one line }

var Command      : String;
    CommandCount : Longint;
    WorkStr      : String;

    SplitUpStr   : SplitStringType;
    SplitUpLong  : SplitLongType;
    Counter      : Longint;

    TempBool,
    TempBool2    : Boolean;
    Variable1,
    Variable2    : String;
    Temp         : Boolean;

    DoFill       : FillUp;
begin
  {------------------- Check for comment and skip it ------------------------}
  if (CommandStr[1] in [';', ':']) then EXIT;
  Command := SupCase(ExtractWord(CommandStr, 1));

  {----------------- Handle the IF, ENDIF and ELSE commands -----------------}
  With QInfo^ do
    if SkippingIf then
      begin
        if Command = 'IF' then Inc(IgnoredIf);

        if Command = 'ENDIF' then
          begin
            if IgnoredIf > 00 then
              begin
                Dec(IgnoredIf);
              end
               else begin
                      if IfStackPtr > 1 then
                        begin
                          Dec(IfStackPtr);
                          SkippingIf := IfStack[IfStackPtr];
                        end
                         else begin
                                if IfStackPtr > 00 then Dec(QInfo^.IfStackPtr);
                                QInfo^.SkippingIf := false;
                              end; { else }
                    end; { else begin }
          end; { if ENDIF }

        if Command = 'ELSE' then
          begin
            if IgnoredIf = 00 then
              begin
                if IfStackPtr > 00 then
                  begin
                    IfStack[IfStackPtr] := NOT IfStack[IfStackPtr];
                    SkippingIf := IfStack[IfStackPtr];
                  end; { if }
              end; { if }
          end; { if ELSE }

        Command := ';';
      end; { Skipping If }

  {---------------------- Handler for the BREAK command------------------------}
  With QInfo^ do
    if SkippingWhile then
      begin
        if Command = 'ENDWHILE' then
          begin
            if WhilePtr > 0 then
              Dec(WhilePtr);
            SkippingWhile := false;
          end; { if }

        Command := ';';
      end; { if }

  {--------------------- Handle the actual command --------------------------}
  CommandCount := WordCount(CommandStr);

  for Counter := 01 to High(SplitUpStr) do
    begin
      SplitUpStr[Counter] := ExtractWord(CommandStr, Counter + 01);

      if SplitUpStr[Counter, 1] in ['#', '%'] then
        SplitUpLong[Counter] := FVal(Copy(SplitUpStr[Counter], 2, 255))
         else SplitUpLong[Counter] := FVal(SplitUpStr[Counter]);
    end; { for }


  if (NOT (Command[1] in [':', ';'])) then
   With QInfo^ do
    begin
      Temp := false;

      if @CommandHook <> nil then
        Temp := CommandHook(SplitUpStr, SplitUpLong, CommandStr, CommandCount, @Self);

      if NOT Temp then
       Case Command[01] of
        {------------------------------- 'A' --------------------------------}
        'A' : begin
                Case Command[2] of
                  'S' : if Command = 'ASCII' then
                         begin
                           { Syntax: Ascii <var num> <value> }
                           if CommandCount >= 3 then
                             begin
                               if (SplitUpLong[1] in [1..qaMaxAnswers]) then
                                 Answers^.Put(SplitUpLong[1], Copy(Chr(SplitUpLong[2]), 1, 1));
                             end; { if }
                         end { 'ASCII' }

                  else if Command = 'ASSIGN' then
                         begin
                           { Syntax: Assign <var num> <"literal" | #<var num>> }
                           if CommandCount >= 3 then
                             begin
                               WorkStr := Copy(CommandStr,
                                               Length('ASSIGN') + 01 +
                                               Length(SplitUpStr[1]) + 1,
                                               255);

                               if SplitUpStr[1, 1] = '%' then
                                 begin
                                   Delete(SplitUpStr[1], 1, 1); { Delete "@" sign }

                                   if FVal(SplitUpStr[1]) in [1..qaMaxAnswers] then
                                     begin
                                       SplitUpStr[1] := QInfo^.Answers^.Get(FVal(SplitUpStr[1]));
                                       SplitUpLong[1] := FVal(SplitUpStr[1]);
                                     end; { if }
                                 end; { if }

                               if SplitUpLong[1] in [1..qaMaxAnswers] then
                                 begin
                                   if SplitUpStr[2, 1] in ['#', '%'] then
                                     begin
                                       if SplitUpStr[2, 1] = '#' then
                                         begin
                                           if SplitUpLong[2] in [1..qaMaxAnswers] then
                                             Answers^.Put(SplitUpLong[1], Answers^.Get(SplitUpLong[2]));
                                         end; { if }

                                       if SplitUpStr[2, 1] = '%' then
                                         begin
                                           if SplitUpLong[2] in [1..qaMaxAnswers] then
                                             Answers^.Put(SplitUpLong[1], Answers^.Get(FVal(Answers^.Get(SplitUpLong[2]))));
                                         end; { if }

                                      end { if }
                                        else begin
                                               WorkStr := Trim(WorkStr);
                                               termObj^.RaCodeStr(WorkStr);

                                               Answers^.Put(SplitUplong[1], WorkStr);
                                             end; { else }
                                 end; { if }
                             end; { if }
                         end; { 'ASSIGN' }
                end; { case }
              end; { 'A' }

        {------------------------------- 'B' --------------------------------}
        'B' : begin
                Case Command[2] of
                  {------------------------- 'BE' ---------------------------}
                  'E' : if Command = 'BEEP' then OutputProc(^G);

                  {------------------------- 'BR' ---------------------------}
                  'R' : if Command = 'BREAK' then
                          begin
                            SkippingWhile := true;
                          end; { if }
                end; { case }
              end; { 'B' }

        {------------------------------- 'C' --------------------------------}
        'C' : begin
                Case Command[2] of
                  {------------------------- 'CA' ---------------------------}
                  'A' : if Command = 'CALC' then
                          begin
                            if SplitUpLong[1] in [1..qaMaxAnswers] then
                             if SplitUpLong[2] in [1..qaMaxAnswers] then
                              if SplitUpLong[4] in [1..qaMaxAnswers] then
                               begin
                                 if SplitUpStr[3, 1] = '/' then
                                  if FVal(Answers^.Get(SplitUpLong[4])) = 0 then
                                   Answers^.Put(SplitUpLong[4], FStr(1));

                                 Case SplitUpStr[3,1] of
                                   '*' : QInfo^.Answers^.Put(SplitUpLong[1],
                                            FStr(FVal(Answers^.Get(SplitUpLong[2])) * FVal(Answers^.Get(SplitUpLong[4]))));
                                   '/' : QInfo^.Answers^.Put(SplitUpLong[1],
                                            FStr(FVal(Answers^.Get(SplitUpLong[2])) div FVal(Answers^.Get(SplitUpLong[4]))));
                                   '+' : QInfo^.Answers^.Put(SplitUpLong[1],
                                            FStr(FVal(Answers^.Get(SplitUpLong[2])) + FVal(Answers^.Get(SplitUpLong[4]))));
                                   '-' : QInfo^.Answers^.Put(SplitUpLong[1],
                                            FStr(FVal(Answers^.Get(SplitUpLong[2])) - FVal(Answers^.Get(SplitUpLong[4]))));
                                   '%' : QInfo^.Answers^.Put(SplitUpLong[1],
                                            FStr(FVal(Answers^.Get(SplitUpLong[2])) MOD FVal(Answers^.Get(SplitUpLong[4]))));
                                 end; { case }
                              end; { if }
                          end { 'CALC' }

                     else if Command = 'CAPITALISE' then
                            begin
                              { Syntax: Capitalise <ON | OFF> }
                              if SUpCase(SplitUpStr[1]) = 'ON' then
                                Capitalize := true
                                 else Capitalize := false;
                            end; { 'CAPITALISE' }

                  {------------------------- 'CO' ---------------------------}
                  'O' : if Command = 'COMMIT' then
                          begin
                            QuesCommit
                          end { 'COMMIT' }

                    else if Command = 'CONCAT' then
                             begin
                              { Syntax: ConCat <result var num> <var num> <var num> }
                              if CommandCount >= 4 then
                                begin
                                  if SplitUpLong[1] in [1..qaMaxAnswers] then
                                   if SplitUpLong[2] in [1..qaMaxAnswers] then
                                    if SplitUpLong[3] in [1..qaMaxAnswers] then
                                      Answers^.Put(SplitUpLong[1],
                                          Answers^.Get(SplitUpLong[2]) +
                                          Answers^.Get(SplitUpLong[3]));
                                 end; { if }
                            end; { 'CONCAT' }

                end; { case }
              end; { 'C' }

        {------------------------------- 'D' --------------------------------}
        'D' : begin
                Case Command[2] of
                  {------------------------- 'DE' ---------------------------}
                  'E' : if Command = 'DEFINEOUTPUT' then
                          begin
                            if CommandCount > 1 then
                              begin
                                if SplitUpStr[1, 1] = '#' then
                                  SplitUpStr[1] := Answers^.Get(SplitUpLong[1]);

                                OutputFile := SplitUpStr[1];
                              end; { if }
                          end { 'DEFINEOUTPUT' }

                   else if Command = 'DELIMIT' then
                          begin
                            if CommandCount > 2 then
                              begin
                                if SplitUpLong[1] in [1..qaMaxAnswers] then
                                 if SplitUpLong[2] in [1..qaMaxAnswers] then
                                   begin
                                     if (SplitUpStr[3] = 'ZERO') then
                                       DoFill := Zero
                                        else DoFill := Space;
                                     if (SplitUpStr[4] = 'FRONT') then
                                       TempBool := true
                                        else TempBool := false;
                                     if (SplitUpStr[5] = 'YES') then
                                       TempBool2 := false
                                        else TempBool2 := true;

                                     if CommandCount > 3 then
                                       begin
                                         Answers^.Put(SplitUpLong[1], MakeLen(Answers^.Get(SplitUpLong[1]),
                                                                              SplitUpLong[2],
                                                                              DoFill,
                                                                              TempBool,
                                                                              TempBool2));
                                       end
                                         else Answers^.Put(SplitUpLong[1],
                                                           Copy(Answers^.Get(SplitUpLong[1]), 1, SplitUpLong[2]));

                                   end; { if }
                              end; { CommandCount }
                          end { 'DELIMIT' }

                   else if Command = 'DEC' then
                         begin
                           if CommandCount > 1 then
                             begin
                                if SplitUpStr[1, 1] in ['%'] then
                                  begin
                                    SplitUpStr[1] := Answers^.Get(FVal(Copy(SplitUpStr[1], 2, 255)));
                                    SplitUpLong[1] := FVal(SplitUpStr[1]);
                                  end; { if }

                               if (SplitUpLong[1] in [1..qaMaxAnswers]) then
                                 Answers^.Put(SplitUpLong[1], FStr(FVal(Answers^.Get(SplitUpLong[1])) - 01));
                             end; { if }
                         end; { 'DEC' }

                  {------------------------- 'DI' ---------------------------}
                  'I' : if Command = 'DISPLAY' then
                          begin
                            WorkStr := Trim(Copy(CommandStr, Length('DISPLAY') + 01, 255));
                            WorkStr := MakeDisplayStr(WorkStr, false);

                            While Pos('|', WorkStr) > 0 do
                               Replace('|', #13#10, WorkStr);

                            OutputProc(WorkStr);
                          end  { 'DISPLAY' }
               end; { case }
              end; { 'D' }

        {------------------------------- 'E' --------------------------------}
        'E' : begin
                Case Command[2] of
                  {------------------------- 'EN' ---------------------------}
                  'N' : if Command = 'ENDIF' then
                           begin
                             if IfStackPtr > 0 then
                               begin
                                 Dec(IfStackPtr);

                                 if IfStackPtr > 00 then
                                   SkippingIf := IfStack[IfStackPtr]
                                     else SkippingIf := false;
                               end { if }
                                 else SkippingIf := false;
                           end { 'ENDIF' }

                   else if Command = 'ENDWHILE' then
                     begin
                       if WhilePtr > 0 then
                         begin
                           if TestIfCommand(WhileCmd[WhilePtr]) then
                             TextSeek(QF[CurrentFile], WhileStack[WhilePtr])
                              else Dec(QInfo^.WhilePtr);
                         end; { if }
                     end; { EndWhile }

                  {------------------------- 'EL' ---------------------------}
                  'L' : if Command = 'ELSE' then
                          begin
                            if (IgnoredIf = 0) AND (IfStackPtr > 0) then
                              begin
                                IfStack[IfStackPtr] := NOT IfStack[IfStackPtr];
                                SkippingIf := IfStack[IfStackPtr];
                              end; { if }
                          end; { 'ELSE' }

                  {------------------------- 'EX' ---------------------------}
                  'X' : if Command = 'EXTRACTWORD' then
                          begin
                           if CommandCount >= 4 then
                             begin
                               if SplitUpLong[1] in [1..qaMaxAnswers] then
                                if SplitUpLong[2] in [1..qaMaxAnswers] then
                                  Answers^.Put(SplitUpLong[1], WordStr.ExtractWord(Answers^.Get(SplitUpLong[2]),
                                                                           SplitUpLong[3],
                                                                           defExtractWord,
                                                                           SupCase(SplitUpStr[4]) = 'YES',
                                                                           false));
                             end; { if }
                          end; { ExtractWord }

                end; { case }
              end; { 'E' }

        {------------------------------- 'F' --------------------------------}
        'F' : begin
                Case Command[2] of
                  {------------------------- 'FI' ---------------------------}
                  'I' : if Command = 'FILEEXIST' then
                          begin
                            if CommandCount >= 2 then
                              begin
                                if SplitUpStr[2, 1] = '#' then
                                  SplitUpStr[2] := Answers^.Get(SplitUpLong[2]);

                                termObj^.RaCodeStr(SplitUpStr[2]);
                                if FileExist(SplitUpStr[2]) then WorkStr := 'YES'
                                  else WorkStr := 'NO';


                                if SplitUpLong[1] in [1..qaMaxAnswers] then
                                  Answers^.Put(SplitUpLong[1], WorkStr);
                              end; { if }

                          end { if }

                    else if Command = 'FILEDELETE' then
                           begin
                             if CommandCount >= 2 then
                               begin
                                 if SplitUpStr[1, 1] = '#' then
                                   SplitUpStr[1] := Answers^.Get(SplitUpLong[1]);

                                 if Trim(SplitUpStr[1]) <> '' then
                                   EraseFile(SplitUpStr[1]);
                               end; { if }
                           end { 'FILEDELETE' }

                    else if Command = 'FILEOPEN' then
                           begin
                             if CommandCount >= 3 then
                               begin
                                 if SplitUpLong[1] in [1..MaxFileStack] then
                                   begin
                                     SplitUpStr[2] := WordStr.ExtractWord(CommandStr, 3, defExtractWord, true, false);
                                     termObj^.RacodeStr(SplitUpStr[2]);

                                     if SplitUpStr[2, 1] = '#' then
                                       SplitUpStr[2] := Answers^.Get(SplitUpLong[2]);

                                     FileStack[SplitUpLong[1]]^.Assign(SplitUpStr[2]);
                                     FileStack[SplitUpLong[1]]^.FileMode := ReadWritemode + DenyNone;

                                     if FileStack[SplitUpLong[1]]^.OpenOrCreate(1) then
                                       FileResult[SplitUpLong[1]] := 'YES'
                                        else FileResult[SplitUpLong[1]] := 'NO';
                                   end; { if }
                               end; { if }

                           end { 'FILEOPEN' }

                    else if Command = 'FILEREAD' then
                           begin
                             if CommandCount >= 3 then
                               begin
                                 if SplitUpLong[1] in [1..MaxFileStack] then
                                  if SplitUpLong[2] in [1..qaMaxAnswers] then
                                    begin
                                      FileStack[SplitUpLong[1]]^.ReadLn(SplitUpStr[1]);
                                      Answers^.Put(SplitUpLong[2], SplitUpStr[1]);

                                      if FileStack[SplitUpLong[1]]^.IoResult = 0 then
                                        FileResult[SplitUpLong[1]] := 'YES'
                                         else FileResult[SplitUpLong[1]] := 'NO'
                                   end; { if }
                               end; { if }
                           end { 'FILEREAD' }

                    else if Command = 'FILEWRITE' then
                           begin
                             if CommandCount >= 3 then
                               begin
                                 if SplitUpLong[1] in [1..MaxFileStack] then
                                    begin
                                      SplitUpStr[2] := WordStr.ExtractWord(CommandStr, 3, defExtractWord, true, false);
                                      if SplitUpStr[2, 1] = '#' then
                                        if SplitUpLong[2] in [1..qaMaxAnswers] then
                                          SplitUpStr[2] := Answers^.Get(SplitUpLong[2]);

                                      FileStack[SplitUpLong[1]]^.WriteLn(SplitUpStr[2]);

                                      if FileStack[SplitUpLong[1]]^.IoResult = 0 then
                                        FileResult[SplitUpLong[1]] := 'YES'
                                         else FileResult[SplitUpLong[1]] := 'NO'
                                   end; { if }
                               end; { if }
                           end { 'FILEREAD' }

                    else if Command = 'FILECLOSE' then
                           begin
                             if CommandCount >= 2 then
                               begin
                                 if SplitUpLong[1] in [1..MaxFileStack] then
                                   FileStack[SplitUpLong[1]]^.Close;
                               end; { if }

                           end { 'FILECLOSE' }

                    else if Command = 'FILERESULT' then
                           begin
                             if CommandCount >= 3 then
                               begin
                                 if SplitUpLong[1] in [1..MaxFileStack] then
                                  if SplitUpLong[2] in [1..qaMaxAnswers] then
                                    begin
                                      Answers^.Put(SplitUpLong[2], FileResult[SplitUpLong[1]]);
                                   end; { if }
                               end; { if }

                           end { 'FILERESULT' }

                end; { case }
              end; { 'F' }

        {------------------------------- 'G' --------------------------------}
        'G' : begin
                Case Command[2] of
                   'E' : if Command = 'GETRECORDINFO' then
                          begin
                            if CommandCount >= 3 then
                              begin
                                if SplitUpStr[1, 1] = '#' then
                                  SplitUpStr[1] := Answers^.Get(SplitUpLong[1]);

                                GetInfoHook(FVal(SplitUpStr[1]), Answers^, SplitUpLong[2], SUpCase(SplitUpStr[3]) = 'DOWN',
                                            GetInfoFile);
                              end; { if }
                          end { if }

                   else if Command = 'GETBBSOS' then
                         begin
                           if CommandCount >= 2 then
                             begin
                               if SplitUpLong[1] in [1..qaMaxAnswers] then
                                 begin
                                   Answers^.Put(SplitUpLong[1], 'Unknown');

                                   {$IFDEF WIN32}
                                     {$IFDEF WINGUI}
                                       Answers^.Put(SplitUpLong[1], 'GUI');
                                     {$ELSE}
                                       Answers^.Put(SplitUpLong[1], 'W32');
                                     {$ENDIF}
                                   {$ENDIF}

                                   {$IFDEF MSDOS}
                                     Answers^.Put(SplitUpLong[1], 'DOS');
                                   {$ENDIF}

                                   {$IFDEF OS2}
                                     Answers^.Put(SplitUpLong[1], 'OS2');
                                   {$ENDIF}
                                 end; { if }
                             end; { if }
                         end { 'GETBBSOS' }

                   else if Command = 'GETVOLUMELABEL' then
                         begin
                           if CommandCount > 2 then
                             begin
                               if SplitUpLong[1] in [1..qaMaxAnswers] then
                                 begin
                                   Answers^.Put(SplitUpLong[1], GetVolumeLabel(SplitUpStr[2]));
                                 end; { if }
                             end; { if }
                         end { 'GETVOLUMELABEL' }

                   else if Command = 'GETENV' then
                         begin
                           if CommandCount > 2 then
                             begin
                               if SplitUpLong[1] in [1..qaMaxAnswers] then
                                 begin
                                   Answers^.Put(SplitUpLong[1], GetEnv(SplitUpStr[2]));
                                 end; { if }
                             end; { if }
                         end { 'GETENV' }

                   else if Command = 'GETPARAMETER' then
                         begin
                           if CommandCount >= 3 then
                             begin
                               if SplitUpLong[1] in [1..qaMaxAnswers] then
                                 begin
                                   Answers^.Put(SplitUpLong[1], ExtractWord(Parameters, SplitUpLong[2]));
                                 end; { if }
                             end; { if }
                         end; { 'GETPARAMETER' }

                  {------------------------- 'GO' ---------------------------}
                  'O' : if Command = 'GOTO' then
                          begin
                            if CommandCount > 1 then
                              begin
                                if IfStackPtr > 00 then
                                  IfStackPtr := 00;

                                if SplitUpStr[1, 1] = '#' then
                                  SplitUpStr[1] := Answers^.Get(SplitUpLong[1]);

                                Findlabel(SplitUpStr[1]);
                              end; { if }
                          end { 'GOTO' }

                   else if Command = 'GOSUB' then
                          begin
                            if GoSubPtr < MaxGosub then
                              begin

                                if CommandCount > 1 then
                                  begin
                                    Inc(GosubPtr);

                                    if SplitUpStr[1, 1] = '#' then
                                      SplitUpStr[1] := Answers^.Get(SplitUpLong[1]);

                                    GosubStack[GosubPtr] := TextFilePos(QF[CurrentFile]);
                                    FindLabel(SplitUpStr[1]);
                                  end; { if }

                              end; { if }
                          end; { 'GOSUB' }
                end; { case }
              end; { 'G' }

        {------------------------------- 'I' --------------------------------}
        'I' : begin
                Case Command[2] of
                  {------------------------- 'IF' ---------------------------}
                  'F' : if Command = 'IF' then
                          begin
                            TempBool := TestIfCommand(CommandStr);
                            TempBool := Not TempBool;

                            if IfStackPtr < MaxIf then
                              begin
                                Inc(IfStackPtr);
                                SkippingIf := TempBool;

                                IfStack[IfStackPtr] := TempBool;
                              end; { IF IfStackPtr }
                          end; { if }

                  {------------------------- 'IN' ---------------------------}
                  'N' : if Command = 'INCLUDEQA' then
                          begin
                            if (CurrentFile + 1) < QaMaxFiles then
                             begin
                               if SplitUpStr[1] <> '' then
                                 begin
                                   Inc(CurrentFile);

                                   if SplitUpStr[1, 1] = '#' then
                                     SplitUpStr[1] := Answers^.Get(SplitUpLong[1]);

                                   if NOT FileExist(QuesPath + Slowcase(NoExtension(SplitUpStr[1])) + '.q-a') then
                                     WorkStr := ''
                                       else WorkStr := ForceBack(QuesPath);

                                   Assign(QF[CurrentFile], WorkStr + Slowcase(NoExtension(SplitUpStr[1])) + '.q-a');
                                   {$i-} System.Reset(QF[CurrentFile]); {$I+}
                                   if IOResult > 00 then Dec(CurrentFile);
                                 end; { if }
                             end; { if }
                          end { 'INCLUDEQA' }

                   else if Command = 'INC' then
                          begin
                            if CommandCount > 1 then
                              begin
                                if SplitUpStr[1, 1] in ['%'] then
                                  begin
                                    SplitUpStr[1] := Answers^.Get(FVal(Copy(SplitUpStr[1], 2, 255)));
                                    SplitUpLong[1] := FVal(SplitUpStr[1]);
                                  end; { if }

                                if (SplitUpLong[1] in [1..qaMaxAnswers]) then
                                  Answers^.Put(SplitUpLong[1], FStr(FVal(Answers^.Get(SplitUpLong[1])) + 01));
                              end; { if }
                          end; { 'INC' }
                end; { 'case' }
              end; { 'I' }

        {------------------------------- 'L' --------------------------------}
        'L' : begin
                Case Command[2] of
                  {------------------------- 'LI' ---------------------------}
                  'I' : if Command = 'LISTANSWER' then
                          begin
                            if CommandCount > 1 then
                              begin
                                if SplitUpLong[1] in [1..qaMaxAnswers] then
                                  OutputProc(Answers^.Get(SplitUpLong[1]) + CrLf);
                              end; { if }
                          end; { 'LISTANSWER' }

                  {------------------------- 'LE' ---------------------------}
                  'E' : if Command = 'LENGTH' then
                          begin
                            if CommandCount > 2 then
                              begin
                                if SplitUpLong[1] in [1..qaMaxAnswers] then
                                 if SplitUpLong[2] in [1..qaMaxAnswers] then
                                  Answers^.Put(SplitUpLong[1], FStr(Length(Answers^.Get(SplitUpLong[2]))));
                              end; { if }
                          end; { 'LENGTH' }
                  {------------------------- 'LO' ---------------------------}
                  'O' : if Command = 'LOWERCASE' then
                          begin
                            if CommandCount > 1 then
                              begin
                                if SplitUpLong[1] in [1..qaMaxAnswers] then
                                  Answers^.Put(SplitUpLong[1], SlowCase(Answers^.Get(SplitUpLong[1])));
                              end; { if }
                          end; { 'LOWERCASE' }
                end; { case }

              end; { 'L' }
        {------------------------------- 'O' --------------------------------}
        'O' : begin
                Case Command[2] of
                  {------------------------- 'OU' ---------------------------}
                  'U' : if Command = 'OUTPUTANSWER' then
                          begin
                             WorkStr := Commandstr;

                             FirstWord(WorkStr, defExtractWord, false);
                             Variable1 := MakeDisplayStr(WorkStr, true);

                             While Pos('|', Variable2) > 00 do Replace('|', #13 + #10, Variable2);

                             termObj^.RaCodeStr(Variable1);

                             AddStr(Variable1 + #13 + #10);
                          end; { if }

                  {------------------------- 'OR' ---------------------------}
                  'R' : if Command = 'ORD' then
                          begin
                            if CommandCount > 1 then
                              begin
                                if SplitUpLong[1] in [1..qaMaxAnswers] then
                                 if SplitUpLong[2] in [1..qaMaxAnswers] then
                                   Answers^.Put(SplitUpLong[1], FStr(Ord(FirstChar(Answers^.Get(SplitUpLong[2])))));
                              end; { if }
                          end; { 'ORD' }
                end; { case }
           end; { 'O' }

        {------------------------------- 'Q' --------------------------------}
        'Q' : begin
                Case Command[2] of
                  {------------------------- 'QU' ---------------------------}
                  'U' : if Command = 'QUIT' then Finished := true;
                end; { case }
              end; { 'Q' }

        {------------------------------- 'R' --------------------------------}
        'R' : begin
                Case Command[2] of
                  {------------------------- 'RE' ---------------------------}
                  'E' : if Command = 'RETURN' then
                          begin
                            if (GoSubPtr > 00) then
                              begin
                                TextSeek(QF[CurrentFile], GosubStack[GosubPtr]);
                                Dec(QInfo^.GoSubPtr);
                              end; { if }
                          end; { 'RETURN' }

                  {------------------------- 'RA' ---------------------------}
                  'A' : if Command = 'RANDOM' then
                          begin
                            if CommandCount >= 3 then
                              begin
                                if SplitUpLong[2] in [1..qaMaxAnswers] then
                                  Answers^.Put(SplitUpLong[2], FStr(Random(SplitUpLong[1])));
                              end; { if }
                          end { 'RANDOM' }

                   else if Command = 'RAWDISPLAY' then
                          begin
                            WorkStr := Trim(Copy(CommandStr, Length('RAWDISPLAY') + 01, 255));
                            WorkStr := MakeDisplayStr(WorkStr, false);

                            OutputProc(WorkStr);
                          end  { 'RAWDISPLAY' }
                end; { case }
              end; { 'R' }

        {------------------------------- 'S' --------------------------------}
        'S' : begin
                Case Command[2] of
                  'E' : if Command = 'SETRESULT' then
                           begin
                             if CommandCount > 1 then
                               ReturnResult := Trim(Copy(CommandStr, Length('SETRESULT') + 01, 255));
                           end; { 'SETRESULT' }

                  {------------------------- 'SU' ---------------------------}
                  'U' : if Command = 'SUBSTRING' then
                         begin
                           if CommandCount >= 5 then
                             begin
                               if SplitUpLong[1] in [1..qaMaxAnswers] then
                                if SplitUpLong[2] in [1..qaMaxAnswers] then
                                  Answers^.Put(SplitUpLong[1], Copy(Answers^.Get(SplitUpLong[2]),
                                                                  SplitUpLong[3],
                                                                  SplitUpLong[4]));
                             end; { if }
                         end { 'SUBSTRING' }

                    else if Command = 'SUBSTRINGVAR' then
                          begin
                           if CommandCount >= 5 then
                             begin
                               if SplitUpLong[1] in [1..qaMaxAnswers] then
                                if SplitUpLong[2] in [1..qaMaxAnswers] then
                                 if SplitUpLong[3] in [1..qaMaxAnswers] then
                                  if SplitUpLong[4] in [1..qaMaxAnswers] then
                                    Answers^.Put(SplitUpLong[1], Copy(Answers^.Get(SplitUpLong[2]),
                                                                    FVal(Answers^.Get(SplitUpLong[3])),
                                                                    FVal(Answers^.Get(SplitUpLong[4]))));
                             end; { if }
                          end; { 'SUBSTRINGVAR' }
                end; { case }
              end; { 'S' }

        {------------------------------- 'U' --------------------------------}
        'U' : begin
                Case Command[2] of
                  {------------------------- 'UP' ---------------------------}
                  'P' : if Command = 'UPPERCASE' then
                          begin
                            if CommandCount > 1 then
                              begin
                                if SplitUpLong[1] in [1..qaMaxAnswers] then
                                  Answers^.Put(SplitUpLong[1], SUpCase(Answers^.Get(SplitUpLong[1])));
                              end; { if }
                          end; { 'UPPERCASE' }
                end; { case }
              end; { 'U' }

        {------------------------------- 'W' --------------------------------}
        'W' : begin
                Case Command[2] of

                  {------------------------- 'WH' ---------------------------}
                  'H' : if Command = 'WHILE' then
                          begin  { Syntax: WHILE [xx] [<|>|<=|>=] [xx] do" }
                            if WhilePtr < MaxWhile then
                             if CommandCount > 4 then
                               begin
                                  WhileCmd[WhilePtr + 1] := 'IF ' + SplitUpStr[1] + #32 + SplitUpStr[2] + #32 + SplitUpStr[3];

                                  if TestIfCommand(WhileCmd[WhilePtr + 1]) then
                                    begin
                                      Inc(WhilePtr);
                                      WhileStack[WhilePtr] := TextFilePos(QF[CurrentFile]);
                                    end { if }
                                     else SkippingWhile := true;
                                end; { if }
                          end; { Command = 'While' }
                end; { case }
              end; { 'W' }

        {------------------------------- 'Y' --------------------------------}
        'Y' : begin
                Case Command[2] of
                {------------------------- 'YI' -----------------------------}
                'I' : if Command = 'YIELD' then SliceHook(SliceCount);
                end; { case }
              end; { 'Y' }
      end; { case }
    end; { with }
end; { proc. HandleCommand }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure QaSysObj.QuesCommit;
var OutFile: Text;
    Counter: Longint;
begin
  if QInfo = nil then EXIT;
  if Trim(Qinfo^.OutputFile) = '' then EXIT;
  if QInfo^.OutLines = 00 then EXIT;

  Assign(OutFile, QInfo^.OutputFile);
  {$i-} Append(Outfile); {$I+}
  if IoResult > 00 then
    begin
      {$i-} System.Rewrite(OutFile); {$I+}
      if IoResult > 00 then EXIT;
    end; { if }

  With Qinfo^ do
   for Counter := 01 to OutLines do
     begin
       {$i-} Write(OutFile, QInfo^.PostLines^.Get(Counter)); {$i+}
       if IoResult >00 then BREAK;
     end; { for }

  {$i-} Close(OutFile); {$I+}
  if IOResult>00 then ;

  QInfo^.OutLines := 00;
end; { proc. Commit }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit. Question }
