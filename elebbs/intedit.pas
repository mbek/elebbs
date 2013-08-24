unit IntEdit;
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
** Internal (line) editor routines for EleBBS
**
** Copyright (c) 1997 by Maarten Bekers
**
** Created : 12-Oct-1996
** Last update : 25-Nov-1999
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Global, StrUnit;

{$IFNDEF WITH_FULL}
  This unit shouldn''t be compiled for an utility program
{$ENDIF}

procedure RaAlikeEditor(const HeaderMsg : String;
                        var   TotLines  : Longint;
                        var   EditorInfo: StringArrayObj;
                              CurInText : Longint);
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Ral, InOut_U, Support, Debug_U, StUtils, LineEd, DispAns, LongStr,
      CentrStr, WordStr, Input_U, Ranges, Control, ObjDec;
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure RaAlikeEditor(const HeaderMsg : String;
                        var   TotLines  : Longint;
                        var   EditorInfo: StringArrayObj;
                              CurInText : Longint);

  procedure EditLineNr(var TempLine,
                           TempLine2  : String;
                           CurLine    : Longint);
  var WrapString: String;
  begin
    Write('`A2:', CurLine, '`X4:: ');                         { Write line# }
    Write('`A3:');
    Write(TempLine, '`X-', Length(TempLine), ':');

    LineEdit(TempLine,                                     { String to edit }
             70,
             [#32..#254],                              { Characters allowed }
             false,                                            { Capitalize }
             false,                                             { Password? }
             #32,                                                { Fillchar }
             true,                                               { Wordwrap }
             false,                                        { AddShowspace ? }
             false);                                       { IsIEMSI login? }

    if Length(TempLine) = 70 then                         { Should we wrap? }
     if Pos(#32, TempLine) <> 00 then                        { Can we wrap? }
      if (NOT (TempLine[70] in defExtractWord)) then
        begin
          WrapString := ExtractWord(TempLine,
                                    WordCount(TempLine,
                                              defExtractWord,
                                              false),
                                    defExtractWord,
                                    false,
                                    false);
          Delete(TempLine,
                 Length(TempLine) - Pred(Length(WrapString)),
                 Length(WrapString));
          Insert(WrapString, TempLine2, 1);

          Write('`X', 6 + Length(TempLine), ':');
          Write(OutputObj^.MakeClrEolStr);
        end; { WordWrapped }

    WriteLn;
  end; { proc. EditLineNr }

  procedure RangeEdit(var TempStr : String;
                          ShowMsg : String;
                          StartNr,
                          StopNr  : Longint);
  var MsgLen: Longint;
  begin
    REPEAT
      TempStr := '';
      Write('`A2:', ShowMsg, #32,
            GlobalCfg^.RaConfig^.LeftBracket, StartNr, '-', StopNr,
            GlobalCfg^.RaConfig^.RightBracket, ': ');

      MsgLen := Length(FStr(StopNr));
      if Length(FStr(StartNr)) > MsgLen then
        MsgLen := Length(FStr(StartNr));

      LineEdit(TempStr,                                    { String to edit }
               MsgLen,
               ['0'..'9'],                             { Characters allowed }
               false,                                          { Capitalize }
               false,                                           { Password? }
               #32,                                              { Fillchar }
               false,                                            { Wordwrap }
               false,                                      { AddShowspace ? }
               false);                                     { IsIEMSI login? }
      WriteLn;
      if TempStr = '' then EXIT;

      if NOT InRange(FVal(TempStr), StartNr, StopNr) then
        begin
          Writeln;
          Writeln(LangObj^.ralGet(ralNrInRange), ' ', StartNr, '-', StopNr);
        end; { if NOT InRange }

    UNTIL (InRange(FVal(TempStr), StartNr, StopNr)) OR (NOT ContrObj^.CheckAll);
  end; { proc. RangeEdit }

  procedure DeleteLines(var CurLine: Longint);
  var StartLine  : String;
      StopLine   : String;
      Counter    : Longint;
  begin
    StartLine := '';
    StopLine := '';

    RangeEdit(StartLine, LangObj^.ralGet(ralDelStart), 1, CurLine);
    if StartLine = '' then EXIT;

    RangeEdit(StopLine, LangObj^.ralGet(ralDelEnd), 1, CurLine);
    if StopLine = '' then EXIT;

    if fVal(StopLine) < fVal(StartLine) then EXIT;

    for Counter := fVal(StopLine) to CurLine do
      EditorInfo.Put(fVal(StartLine) + (Counter - fVal(StopLine)), EditorInfo.Get(Counter + 1));

    CurLine := CurLine - (fVal(StopLine) - fVal(StartLine));

    for Counter := CurLine to EditorInfo.MaxStrings do
      EditorInfo.Put(Counter, '');
  end; { proc. DeleteLines }


  procedure ChangeLine(CurLines: Longint);
  var LineToChange: String;
      TempStr1,
      TempStr2    : String;
  begin
    RangeEdit(LineToChange, LangObj^.ralGet(ralEditLine1), 1, CurLines);
    if LineToChange = '' then EXIT;

    TempStr1 := EditorInfo.Get(FVal(LineToChange));
    TempStr2 := EditorInfo.Get(FVal(LineToChange) + 1);

    EditLineNr(TempStr1, TempStr2, FVal(LineToChange));

    EditorInfo.Put(FVal(LineToChange), TempStr1);
    EditorInfo.Put(FVal(LineToChange) + 1, TempStr2);
  end; { proc. ChangeLine }


  procedure ReplaceLine(CurLine: Longint);
  var LineNrStr  : String;                       { Linenumber to replace in }
      ReplaceStr : String;                                { Text to replace }
      WithStr    : String;                      { Text to replace this with }
      TempStr    : String;
  begin
    RangeEdit(LineNrStr, LangObj^.ralGet(ralAdjLine), 1, CurLine);
    if LineNrStr = '' then EXIT;

    WriteLn;
    WriteLn('`A14:', LangObj^.ralGet(ralCurrLine));
    WriteLn('`A2:',CurLine,'`X4::`X6:`A3:', EditorInfo.Get(fVal(LineNrStr)));
    WriteLn;

    REPEAT
      ReplaceStr := '';
      WithStr := '';

      {---------------------- What text should we replace -------------------}
      Write('`A3:', LangObj^.ralGet(ralReplWhat));
      LineEdit(ReplaceStr,                                 { String to edit }
               40,
               [#32..#254],                            { Characters allowed }
               false,                                          { Capitalize }
               false,                                           { Password? }
               #32,                                              { Fillchar }
               false,                                            { Wordwrap }
               false,                                      { AddShowspace ? }
               false);                                     { IsIEMSI login? }

      WriteLn('`A3:');
      if ReplaceStr = '' then BREAK;

      {---------------------- What should we replace it with? ---------------}
      Write('`A3:', LangObj^.ralGet(ralReplWith));
      LineEdit(WithStr,                                    { String to edit }
               40,
               [#32..#254],                            { Characters allowed }
               false,                                          { Capitalize }
               false,                                           { Password? }
               #32,                                              { Fillchar }
               false,                                            { Wordwrap }
               false,                                      { AddShowspace ? }
               false);                                     { IsIEMSI login? }

      WriteLn('`A3:');
      if WithStr = '' then BREAK;

      {------------------------- Actually replace it ------------------------}
      TempStr := EditorInfo.Get(FVal(LineNrStr));
      Replace(ReplaceStr, WithStr, TempStr);
      EditorInfo.Put(FVal(LineNrStr), TempStr);

      {----------------------- Show the user how it now looks ---------------}
      WriteLn;
      WriteLn;
      WriteLn('`A14:', LangObj^.ralGet(ralNowReads));
      WriteLn('`A2:',CurLine,'`X4::`X6:`A3:', EditorInfo.Get(FVal(LineNrStr)));
      WriteLn;

    UNTIL (TRUE = FALSE) OR (NOT contrObj^.CheckAll);
  end; { proc. ReplaceLine }


  procedure InsertText(var CurLine: Longint);
  var SelLine    : String;
      TempLine   : Longint;
      StartLine  : Longint;
      TempStr1,
      TempStr2   : String;
      TempEditor : pStringArrayObj;
      Counter    : Longint;
  begin
    RangeEdit(SelLine, LangObj^.ralGet(ralInsLine), 1, CurLine);
    if SelLine = '' then EXIT;
    if fVal(SelLine) < 1 then EXIT;

    New(TempEditor, Init(40));

    TempLine  := FVal(SelLine);
    StartLine := FVal(SelLine);

    REPEAT
      TempStr1 := TempEditor^.Get(StartLine);
      TempStr2 := TempEditor^.Get(StartLine + 1);

      EditLineNr(TempStr1, TempStr2, StartLine);

      TempEditor^.Put(StartLine, TempStr1);
      TempEditor^.Put(StartLine, TempStr2);

      if TempEditor^.Get(StartLine) <> '' then
        Inc(StartLine)
          else BREAK;

      if StartLine > TempEditor^.MaxStrings then
        begin
          WriteLn;
          WriteLn('`A3:', LangObj^.ralGet(ralMaxLen));
          BREAK;
        end; { if }

    UNTIL (TRUE = FALSE) OR (NOT contrObj^.CheckAll);

    for Counter := 01 to TempLine - 1 do
      TempEditor^.Put(Counter, EditorInfo.Get(Counter));

    if StartLine < TotLines then
      for Counter := TempLine to TotLines do
        TempEditor^.Put(Counter + (StartLine - TempLine),
                        EditorInfo.Get(Counter));

    for Counter := 01 to TempEditor^.MaxStrings do
      EditorInfo.Put(Counter, TempEditor^.Get(Counter));

    Inc(CurLine, (StartLine - TempLine) + 1);
    Dispose(TempEditor, Done);
  end; { proc. InsertText }


  procedure ShowAllLines(CurLine: Longint);
  var Counter: Longint;
  begin
    WriteLn;
    WriteLn('`X5:`A14:',
            GlobalCfg^.RaConfig^.LeftBracket, OutputObj^.AvatarDupl('-', 70),
            GlobalCfg^.RaConfig^.RightBracket);

    for Counter := 01 to CurLine do
      begin
        if Pos('>', Copy(EditorInfo.Get(Counter), 1, 15)) = 00 then
          begin
            WriteLn('`A2:', Counter, '`X4::`X6:`A3:',
                    EditorInfo.Get(Counter))
          end
            else begin
                   WriteLn('`A2:', Counter, '`X4::`X6:`A7:',
                           EditorInfo.Get(Counter));
                 end; { else }
      end; { for }
  end; { proc. ShowAllLines }

var CurLine   : Longint;
    AbortEdit : Boolean;
    EnterMenu : Boolean;
    TempCH    : Char;
    TempStr1  : String;
    TempStr2  : String;
    Counter   : Longint;
begin
  {----------------------------- Show the header ----------------------------}
  OutputObj^.ClearScreen;
  Writeln('`A3:', CenterJust(HeaderMsg, 79, false));
  Writeln('`A3:', CenterJust(LangObj^.ralGet(ralMaxOf) + #32 +
                  FStr(TotLines) + #32 + LangObj^.ralGet(ralLines)+', 70 ' +
                  LangObj^.ralGet(ralChrPerLn), 79, false));

  WriteLn;
  WriteLn('`X5:`A14:',
          GlobalCfg^.RaConfig^.LeftBracket,
          OutputObj^.AvatarDupl('-', 70),
          GlobalCfg^.RaConfig^.RightBracket);

  {--------------------------- Actually start editting ----------------------}
  CurLine := 01;
  AbortEdit := FALSE;
  EnterMenu := FALSE;

  for Counter := CurInText + 01 to TotLines do
    EditorInfo.Put(Counter, '');

  if CurInText > 00 then
    begin
      CurLine := CurInText + 1;
      ShowAllLines(CurLine - 1);
    end; { CurIntext }

  REPEAT
    if CurLine > TotLines then
      begin
        WriteLn;
        WriteLn('`A3:', LangObj^.ralGet(ralMaxLen));
        EnterMenu := TRUE;
      end; { Maximum message length exceeded }

    if NOT EnterMenu then
      begin
        TempStr1 := EditorInfo.Get(CurLine);
        TempStr2 := EditorInfo.Get(CurLine + 1);

        EditLineNr(TempStr1, TempStr2, CurLine);

        EditorInfo.Put(CurLine, TempStr1);
        EditorInfo.Put(CurLine + 1, TempStr2);

        if EditorInfo.Get(CurLine) <> '' then Inc(CurLine)
          else EnterMenu := TRUE;
      end; { NOT EnterMenu }

    if EnterMenu then
      begin
        WriteLn;
        WriteLn('`A14:', LangObj^.ralGet(ralFuncAvail),    '  ',
                         GlobalCfg^.RaConfig^.Leftbracket,CurLine-1,  ' ',
                           LangObj^.ralGet(ralLinesTxt),
                         GlobalCfg^.RaConfig^.RightBracket);
        WriteLn('`A11:', LangObj^.ralGetDsp(ralDelete1),   ',  ',
                         LangObj^.ralGetDsp(ralEditLine2), ', ',
                         LangObj^.ralGetDsp(ralReplace),   ',    ',
                         LangObj^.ralGetDsp(ralInsert),    ',    ',
                         LangObj^.ralGetDsp(ralHelp2),     ',');

        Writeln('`A11:', LangObj^.ralGetDsp(ralContinue),  ',     ',
                         LangObj^.ralGetDsp(ralList),      ', ',
                         LangObj^.ralGetDsp(ralSave),      ',    ',
                         LangObj^.ralGetDsp(ralQuit1));
        WriteLn;
        Write(LangObj^.ralGet(ralSelect));

        InputObj^.DorCleanKeys;
        TempCH := UpCase(InputObj^.ReadKey);

        {------------------- Make sure there is message text ----------------}
        if TempCH in [upCase(LangObj^.ralGetKey(ralDelete1)),
                      upCase(LangObj^.ralGetKey(ralEditLine2)),
                      upCase(LangObj^.ralGetKey(ralReplace)),
                      upCase(langObj^.ralGetKey(ralInsert))] then
          if (Pred(CurLine)) = 00 then
            begin
              WriteLn('`A12:');
              WriteLn;
              WriteLn(langObj^.ralGet(ralNoMsgTxt));
              TempCH := #00;
            end; { if }

        if TempCH = UpCase(LangObj^.ralGetKey(ralDelete1)) then
          begin
            WriteLn(LangObj^.ralGetStr(ralDelete1));
            DeleteLines(CurLine);
          end; { DeleteLines }

        if TempCH = UpCase(LangObj^.ralGetKey(ralEditLine2)) then
          begin
            WriteLn(LangObj^.ralGetStr(ralEditLine2));
            ChangeLine(CurLine);
          end; { EditLine }

        if TempCH = UpCase(LangObj^.ralGetKey(ralReplace)) then
          begin
            WriteLn(LangObj^.ralGetStr(ralReplace));
            ReplaceLine(CurLine);
          end; { Replace }

        if TempCH = UpCase(LangObj^.ralGetKey(ralHelp2)) then
          begin
            WriteLn(LangObj^.ralGetStr(ralHelp2));
            DisplayHotFile('EDITHELP', []);
          end; { Help }

        if TempCH = UpCase(LangObj^.ralGetKey(ralContinue)) then
          begin
            WriteLn(LangObj^.ralGetStr(ralContinue));
            EnterMenu := false;
          end; { Continue }

        if TempCH = UpCase(LangObj^.ralGetKey(ralList)) then
          begin
            WriteLn(LangObj^.ralGetStr(ralList));
            ShowAllLines(CurLine);
          end; { List }

        if TempCH = UpCase(LangObj^.ralGetKey(ralInsert)) then
          begin
            WriteLn(LangObj^.ralGetStr(ralInsert));
            InsertText(CurLine);
          end; { Insert }

        if TempCH = UpCase(LangObj^.ralGetKey(ralQuit1)) then
          begin
            WriteLn(LangObj^.ralGetStr(ralQuit1));

            if InputObj^.ralStrYesNoAsk(ralSure) then
              begin
                TotLines := -1; { Aborted }
                CurLine := -1;
                EXIT;
              end; { YesNoAsk }
          end; { Quit }

        if TempCH = UpCase(LangObj^.ralGetKey(ralSave)) then
          begin
            WriteLn(LangObj^.ralGetStr(ralSave));
            AbortEdit := TRUE;
          end; { Quit }
      end; { if EnterMenu }

  UNTIL (AbortEdit) OR (ProgTerminated) OR (NOT contrObj^.CheckAll);

  TotLines := CurLine;
end; { proc. RaAlikeEditor }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit INTEDIT }
