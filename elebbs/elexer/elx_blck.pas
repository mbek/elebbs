UNIT ELX_BLCK;
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
{$I Compiler.inc}
(*
**
** Block (compiler) module for Elexer
**
** Copyright (c) 2000 by Maarten Bekers
**
** Created : 26-Oct-2000
** Last update : 03-Mar-2003
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Elx_Glob;

procedure Block(var elx_Globals: elx_GlobalsType;
                fSys: SymbolSet; IsFunc: Boolean; Level: Longint);
procedure Initialize(var elx_Globals: elx_GlobalsType);
procedure InitVars(var elx_Globals: elx_GlobalsType);
procedure ProcessBlocks(var elx_Globals: elx_GlobalsType);
procedure PrintTables(var elx_Globals: elx_GlobalsType);
procedure ErrorMsg(var elx_Globals: elx_Globalstype);
function ErrorMsgStr(ErrorNr: Longint): String;
procedure AddUserIdent(var elx_Globals: elx_GlobalsType;
                       X0: AlfaType;             { Enter a user defined ID }
                       X1: tObject;
                       X2: tType;
                       X3: Longint;
                       VarList: String);

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF MSDOS}
 uses SysUtils;
{$ENDIF}

{ expression }

procedure Error(var elx_Globals: elx_Globalstype;
                ErrorNr: Longint);
var Counter: Longint;
begin
  {-- If the logging errors to file is turned off, one error is fatal ------}
  if elx_Globals.AbortCompile then EXIT;

  if NOT elx_Globals.LogErrors then
   With elx_Globals.BlockVars^, elx_Globals do
    begin
      AbortCompile := TRUE;           { Abort all compilations }
      Inc(ErrorSet[ErrorNr]);          { Add this error number }

      {-- Now show the error -----------------------------------------------}
      with SourceFile[CurSrcFile] do
        WriteLn(SrcFile^.FileName, '(', CurLineNum, '): Error ', Succ(ErrorNr), ': ', ErrorMsgStr(ErrorNr));

      {-- Show the line the error occured at -------------------------------}
      WriteLn(CurLine);
      for Counter := 01 to Pred(CharCount - ErrorPos) do
        begin
          if CurLine[Counter] = #9 then
            Write(#9)
              else Write(#32);
        end; { if }

      WriteLn('^');
      WriteLn;
    end; { if }

  {-- Log this error to the log file ---------------------------------------}
  if elx_GLobals.LogErrors then
   With elx_Globals.BlockVars^, elx_Globals do
    begin
      if ErrorPos = 0 then
        ErrorFile^.Write(' ****');

      if CharCount > ErrorPos then
        begin
          for Counter := 01 to Pred(CharCount - ErrorPos) do
            begin
              if CurLine[Counter] = #9 then
                ErrorFile^.Write(#9)
                  else ErrorFile^.Write(#32);
            end; { if }

          ErrorFile^.Write('^' + IntStr(ErrorNr, 2));

          ErrorPos := CharCount + 3;
          Inc(ErrorSet[ErrorNr]);
        end; { if }
    end; { if LogErrors }
end; { proc. Error }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetCurSymbol(var elx_Globals: elx_Globalstype): tSymbol;
begin
  GetCurSymbol := elx_Globals.BlockVars^.CurSymbol;
end; { func. GetCurSymbol }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


function ErrorMsgStr(ErrorNr: Longint): String;
var Messages : Array[0..MaxError] of String[40];
begin
  Messages[err_UnkIdent]     := 'Unknown identifier.';
  Messages[err_DupIdent]     := 'Duplicate identifier.';
  Messages[err_VarIdent]     := 'Variable identifier expected.';
  Messages[err_ProgMiss]     := '"program" keyword required';
  Messages[err_RParMiss]     := '")" expected.';
  Messages[err_CollMiss]     := '":" expected';
  Messages[err_Syntax]       := 'Syntax error';
  Messages[err_NotUsed]      := 'NOT USED';
  Messages[err_MissOf]       := '"OF" expected';
  Messages[err_LParMiss]     := '"(" expected.';
  Messages[err_CantPack]     := 'PACKED keyword incorrectly used';
  Messages[err_MissLBrk]     := '"[" expected.';
  Messages[err_MissRBrk]     := '"]" expected.';
  Messages[err_MissDtDt]     := '".." expected.';
  Messages[err_MissSemi]     := '";" expected.';
  Messages[err_FuncResult]   := 'Invalid function result type';
  Messages[err_EqlExpect]    := '"=" expected';
  Messages[err_BoolExpect]   := 'Boolean expression expected';
  Messages[err_ForVar]       := 'Invalid FOR control variable';
  Messages[err_ForMismatch]  := 'Type mismatch.';
  Messages[err_NotUsed2]     := 'NOT USED';
  Messages[err_NumTooLarge]  := 'Number is too large';
  Messages[err_UnexpecEof]   := 'Unexpected end of file.';
  Messages[err_NoCaseType]   := 'Invalid type for case statement';
  Messages[err_InvChar]      := 'Invalid character';
  Messages[err_ConstVar]     := 'Cannot use vars in const expression';
  Messages[err_IndexTyp]     := 'Invalid index type for array';
  Messages[err_IndexBound]   := 'Incorrect parameter in index boundary';
  Messages[err_NoArray]      := 'Identifier is not an array';
  Messages[err_NoType]       := 'Error in type';
  Messages[err_UndefType]    := 'Undefined type';
  Messages[err_NoRecord]     := 'Identifier is no record';
  Messages[err_NoBoolean]    := 'Boolean expression expected';
  Messages[err_NoArithm]     := 'Arithmetic type expected';
  Messages[err_IntExpected]  := 'Integer type expected';
  Messages[err_ExprType]     := 'Type mismatch';
  Messages[err_VarType]      := 'Invalid var parameter type';
  Messages[err_InvVarRef]    := 'Invalid variable reference';
  Messages[err_StrExpected]  := 'String expression expected.';
  Messages[err_NrParams]     := 'Invalid number of parameters';
  Messages[err_WrongNum]     := 'Invalid number';
  Messages[err_NoReadWrite]  := 'Cannot Read or Write variables of this type.';
  Messages[err_MustReal]     := 'Real type expected';
  Messages[err_MustInt]      := 'Integer type expected';
  Messages[err_VarConst]     := 'Type mismatch.';
  Messages[err_VarProc]      := 'Invalid variable reference';
  Messages[err_InvAssign]    := 'Type mismatch in assignment';
  Messages[err_TypeCase]     := 'Constant and CASE types do not match';
  Messages[err_FuncMatch]    := 'Type mismatch.';
  Messages[err_NotUsed4]     := 'NOT USED';
  Messages[err_InvConst]     := 'Invalid const type';
  Messages[err_AsExpected]   := '":=" expected.';
  Messages[err_ThenExpected] := 'THEN expected';
  Messages[err_UntilExpected]:= 'UNTIL expected';
  Messages[err_DoExpected]   := 'DO expected';
  Messages[err_ToExpected]   := 'TO or DOWNTO expected';
  Messages[err_BeginExpected]:= 'BEGIN expected';
  Messages[err_EndExpected]  := 'END expected';
  Messages[err_FacExpected]  := 'Factor expected';
  Messages[err_CommaExpected]:= '"," expected';
  Messages[err_InvStrIdx]    := 'Invalid string index';
  Messages[err_ArTooLarge]   := 'Structure too large';
  Messages[err_ElseExpected] := 'ELSE expected';
  Messages[err_CallTabExceed]:= '"CALL" table exceeded';
  Messages[err_NoNestedRec]  := 'Nested recorsd/arrays not supported';
  Messages[err_DynArExpected]:= 'DynamicArray expected';
  Messages[err_UnkSubstitute]:= 'Unknown substitution variable used';

  {-- Now pass through the actual errorstring ------------------------------}
  ErrorMsgStr := Messages[ErrorNr];
end; { func. ErrorMsgStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ErrorMsg(var elx_Globals: elx_Globalstype);
                             { Writes all errors encountered to the screen }
var Counter: Word;
begin
  if elx_Globals.LogErrors then { Log all errors to the logfile, if enabled }
   With elx_Globals do
    begin
      ErrorFile^.WriteLn('*** Errors encountered during compile: ');

      Counter := 0;
      While (Counter < High(ErrorSet)) do
        begin
          Inc(Counter);

          if ErrorSet[Counter] <> 0 then
            begin
              ErrorFile^.WriteLn('    - Error ' + IntStr(Counter, 0) + ': ' +
                                  ErrorMsgStr(Counter));
              ErrorSet[Counter] := 0;
            end; { if }
        end; { while }
    end; { if LogErrors }
end; { proc. ErrorMsg }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FatalError(var elx_Globals: elx_Globalstype;
                     ErrorNr: Longint);
var Messages: Array[1..11] of String[40];
begin
  Messages[fatal_Idents]     := 'identifier';
  Messages[fatal_procedures] := 'procedures';
  Messages[fatal_reals]      := 'reals';
  Messages[fatal_arrays]     := 'arrays';
  Messages[fatal_levels]     := 'levels';
  Messages[fatal_code]       := 'code';
  Messages[fatal_strings]    := 'strings';
  Messages[fatal_inputline]  := 'input line';
  Messages[fatal_NulString]  := 'nulstring';
  Messages[fatal_RecordSize] := 'this record';
  Messages[fatal_DynArray]   := 'dynamic arrays';

  {-- Log error to the screen ----------------------------------------------}
  if NOT elx_Globals.LogErrors then
   With elx_Globals do
    begin
      AbortCompile := TRUE;                            { Abort the compiler }

      WriteLn;
      with SourceFile[CurSrcFile] do
        WriteLn(SrcFile^.FileName, '(', CurLineNum, '): error ');
      WriteLn('Compiler table for ', Messages[ErrorNr], ' is too small!');
      WriteLn('Compilation aborted');
    end; { if }


  {-- Log all errors to the file, if enabled -------------------------------}
  if elx_Globals.LogErrors then
   with elx_Globals do
    begin
      ErrorFile^.WriteLn('');
      ErrorMsg(elx_Globals);

      ErrorFile^.WriteLn('Compiler table for ' + Messages[ErrorNr] + ' is too small!');
      AbortCompile := TRUE;
    end; { if }

end; { proc. FatalError }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EndSkip(var elx_Globals: elx_Globalstype);{ Underline skipped part of input }
begin
  {-- Underline skipped part of input if interesting -----------------------}
  if elx_Globals.LogErrors then
   With elx_Globals, BlockVars^ do
    begin
      While ErrorPos < CharCount do
        begin
          ErrorFile^.Write('-');
          Inc(ErrorPos);
        end; { while }
    end; { if }

  elx_Globals.BlockVars^.SkipFlag := false;
end; { proc. EndSkip }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure NextCH(var elx_Globals: elx_Globalstype);{ Read next character, process end of line }
begin
  With elx_Globals.BlockVars^, elx_Globals do
  if CharCount = CurLineLen then                   { Process if end of line }
    begin
      {-- Check to make sure were not EOF the file --------------------------}
      if SourceFile[CurSrcFile].SrcFile^.EOF then
        begin
          if CurSrcFile = 0 then
            begin
              Error(elx_Globals, err_UnexpecEof); { Unexpected end of file }

              if NOT LogErrors then                   { Even now its fatal }
                begin
                  ErrorMsg(elx_Globals);
                  AbortCompile := TRUE;
                end; { if }
            end
              else begin
                     Dispose(SourceFile[CurSrcFile].SrcFile, Done);
                     SourceFile[CurSrcFile].SrcName := '';

                     Dec(CurSrcFile);
                   end; { else }

        end; { if }

      {-- If there are any pending errors, show them ------------------------}
      if ErrorPos <> 0 then
        begin
          if SkipFlag then EndSkip(elx_Globals);

          if LogErrors then           { Skip to the next line of processing }
            ErrorFile^.WriteLn('');

          ErrorPos := 0;
        end; { if }

      {-- Show the current line number --------------------------------------}
      if LogErrors then
        ErrorFile^.Write(IntStr(CurCode, 5) + #32 + #32);    { Show current codeline }

      CurLineLen := 0;
      CharCount := 0;

      {-- Read a whole line -------------------------------------------------}
      SourceFile[CurSrcFile].SrcFile^.ReadLn(CurLine);

      {-- Check max line len ------------------------------------------------}
      if Length(CurLine) > (MaxLineLen - 2) then
        FatalError(elx_Globals, fatal_InputLine);

      {-- Log this line to the error log ------------------------------------}
      if LogErrors then
        ErrorFile^.WriteLn(CurLine);

      {-- Remove the #10/#13 characters, and close of the output ------------}
      CurLine := CurLine + #32;
      CurLineLen := Length(CurLine);

      {-- Increase the line counter -----------------------------------------}
      with elx_Globals.SourceFile[CurSrcFile] do
        Inc(CurLineNum);
    end; { if }

  {-- Get current character -------------------------------------------------}
  Inc(elx_Globals.BlockVars^.CharCount);
  With elx_Globals, BlockVars^ do
    LastCH := CurLine[CharCount];
end; { proc. NextCH }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure InSymbol(var elx_Globals: elx_Globalstype);   { Reads next symbol }
const
  DotDot = #31;                                    { Used for .. internally }

  maxStringLen = 4096;

LABEL _1,_2,_3;

var Counter : Longint;                                    { General counter }
    Code    : Integer;                               { Used by VAL routines }
    {$IFDEF MSDOS}
      StrBuff : String;                                   { Temporary storage }
    {$ELSE}
      StrBuff : AnsiString;
    {$ENDIF}
    Exponent: Longint;               { Exponent as used in real numbers etc }
    BufCnt  : Longint;     { Counter used for searching the keywords buffer }
    TempInt : Longint;

  {-- ReadScale (under InSymbol) --------------------------------------------}
  procedure ReadScale;
  begin
    StrBuff := StrBuff + 'E';
    NextCH(elx_Globals);

    With elx_Globals do
      if (BlockVars^.LastCH = '+') OR (BlockVars^.LastCH = '-') then
        begin
          StrBuff := StrBuff + BlockVars^.LastCH;
          NextCH(elx_Globals);
        end; { if }

    With elx_Globals, BlockVars^ do
     if (NOT ((LastCH >= '0') AND (LastCH <= '9'))) then
       Error(elx_Globals, err_WrongNum)
       else begin
               REPEAT
               StrBuff := StrBuff + LastCH;
               NextCH(elx_Globals);
             UNTIL (NOT ((LastCH >= '0') AND (LastCH <= '9'))) OR (AbortCompile) ;
           end; { else }
  end; { proc. ReadScale }

  {-- Options (under InSymbol) ----------------------------------------------}
  procedure Options;
  var TmpStr: String;

    procedure Switch(var TempB: Boolean);
    begin
      TempB := (elx_Globals.BlockVars^.LastCH = '+');

      if NOT TempB then
       With elx_Globals, BlockVars^ do
        begin
          if NOT (LastCH = '-') then             { Invalid switch character }
            begin
              Error(elx_Globals, err_Syntax);

              While (LastCH <> '*') AND (LastCH <> ',') AND (LastCH <> '}')
                     AND (NOT AbortCompile) do
                      NextCH(elx_Globals);
            end { if }
              else NextCH(elx_Globals);
        end
          else NextCH(elx_Globals);
    end; { proc. Switch }

    begin
      REPEAT
        NextCH(elx_Globals);

        With elx_Globals, BlockVars^ do
         if (LastCH <> '*') AND (LastCH <> '}') then
          begin
            if ((LastCH = 't') OR (LastCH = 'T')) then
              begin
                NextCH(elx_Globals);
                Switch(prTables);
              end
                else if ((LastCH = 's') OR (LastCH = 'S')) then
                       begin
                         NextCH(elx_Globals);
                         Switch(StackDump);
                       end { if }
                else if ((LastCH = 'i') OR (LastCH = 'I')) then
                       begin
                         {-- Skip the actual i, and the space -------------}
                         NextCH(elx_Globals); NextCH(elx_Globals);

                         {-- now gather the string to read ----------------}
                         TmpStr := '';
                         while (LastCH <> '*') AND (LastCH <> '}') AND
                                 (NOT elx_Globals.AbortCompile) do
                           begin
                             TmpStr := TmpStr + LastCH;
                             NextCH(elx_Globals);
                           end; { while }

                          {-- Increase the file counter -------------------}
                          Inc(CurSrcFile);
                          SourceFile[CurSrcFile].SrcName := TmpStr;
                          SourceFile[CurSrcFile].CurLineNum := 0;

                          {-- actually open the file ----------------------}
                          New(SourceFile[CurSrcFile].SrcFile, Init);
                          SourceFile[CurSrcFile].SrcFile^.Assign(SourceFile[CurSrcFile].SrcName);
                          SourceFile[CurSrcFile].SrcFile^.FileMode := ReadMode + DenyWrite;
                          if NOT SourceFile[CurSrcFile].SrcFile^.Open(1) then
                            begin
                              WriteLn(#32, #254, #32, 'Unable to open "', SourceFile[CurSrcFile].SrcName, '"');
                              EXIT;
                            end; { if }
                       end; { if }
          end; { if }

      UNTIL (elx_Globals.BlockVars^.LastCH <> ',') OR (elx_Globals.AbortCompile);
    end; { proc. Options }

var sLeng   : Longint;
    strTerm : Char;
begin

{ LABEL } _1: With elx_Globals do
              While (BlockVars^.LastCH in [#32, #9, #10]) AND (NOT AbortCompile) do
                NextCH(elx_Globals);

             if UpCase(elx_Globals.BlockVars^.LastCH) in ['A'..'Z'] then { identifier or wordsymbol }
              With elx_Globals, BlockVars^ do
               begin
                 {-- Initialize variables -----------------------------------}
                 Counter := 0;
                 CurIdent := '                              ';

                 if LastCH in ['A'..'Z'] then
                   LastCH := Chr(Ord(LastCH) + 32);     { Lowercase we want }

                 {-- Try getting the whole identifier -----------------------}
                 REPEAT
                   if Counter < IdLen then
                     begin
                       Inc(Counter);
                       CurIdent[Counter] := LastCH;
                     end; { if }

                   NextCH(elx_Globals);
                   if LastCH in ['A'..'Z'] then
                     LastCH := Chr(Ord(LastCH) + 32);   { Lowercase we want }
                 UNTIL (NOT ((LastCH in ['a'..'z']) OR (LastCH in ['0'..'9'])
                         OR (LastCH = '_'))) OR (AbortCompile);

                 {-- We have the identifier, start a search wether it exists-}
                 TempInt := 01;
                 BufCnt := NrKeyws;

                 REPEAT
                   Counter := (TempInt + BufCnt) DIV 2;{ Start at the middle }

                   if CurIdent <= KwName[Counter] then BufCnt := Counter - 1;
                   if CurIdent >= KwName[Counter] then TempInt := Counter + 1;
                 UNTIL (TempInt > BufCnt) OR (AbortCompile);

                 if (TempInt - 1) > BufCnt then
                   CurSymbol := KwSymbol[Counter]
                     else CurSymbol := sym_ident;
               end { if UpCase(CH) in ['A'..'Z'] }

        else if (elx_Globals.BlockVars^.LastCH in ['+','-','*','/',')','=',',','[',']',';','&','|','~']) then
              With elx_Globals, BlockVars^ do
               begin
                 CurSymbol := SpecialChars[LastCH];
                 NextCH(elx_Globals);

                 {-- make sure not a double slash is typed -------------------}
                 if CurSymbol = sym_rdiv then
                  if LastCH = '/' then
                   begin
                     while CurLineLen > CharCount do
                       NextCH(elx_Globals);

                     {-- get the next character to be processed -------------}
                     NextCH(elx_Globals);

                     Goto _1;
                   end; { while }
               end { if }

        else if (elx_Globals.BlockVars^.LastCH in ['0'..'9']) then{ We''ve found a number }
              With elx_Globals, BlockVars^ do
               begin
                 Counter := 0;
                 StrBuff := '';

                 REPEAT                              { Get the whole number }
                   StrBuff := StrBuff + LastCH;
                   Inc(Counter);
                   NextCH(elx_Globals);
                 UNTIL (NOT ((LastCH >= '0') AND (LastCH <= '9'))) OR (AbortCompile);

                 Val(StrBuff, InSym_Integer, Code); { Convert to an integer }

                 {-- Is the next character a dot ----------------------------}
                 if (LastCH = '.') then
                   begin
                     NextCH(elx_Globals);

                     {-- Is it perhaps twice a dot? -------------------------}
                     if LastCH = '.' then
                       begin
                         LastCH := DotDot;
                         CurSymbol := sym_IntCon;

                         if Code <> 0 then
                           begin
                             Error(elx_Globals, err_NumTooLarge);
                             InSym_integer := 0;
                             Counter := 0;
                           end; { if }
                       end { dot dot }
                        else begin { Number, now we found a dot.. (eg: 12.) }
                               CurSymbol := sym_RealCon;
                               StrBuff := StrBuff + '.';
                               Exponent := 0;

                               While (LastCH >= '0') AND (LastCH <= '9') AND (NOT AbortCompile) do
                                 begin
                                   Dec(Exponent);
                                   StrBuff := StrBuff + LastCH;
                                   NextCH(elx_Globals);
                                 end; { while }

                               if Exponent = 0 then Error(elx_Globals, err_WrongNum);
                               if ((LastCH = 'e') or (Lastch = 'E')) then
                                 ReadScale;

                               Val(StrBuff, insym_Real, Code);
                               if Code <> 0 then Error(elx_Globals, err_NumTooLarge);
                             end; { else }
                   end { dot found }
                 else if ((LastCH = 'e') or (LastCH = 'E')) then
                        begin
                          CurSymbol := sym_RealCon;
                          ReadScale;

                          Val(StrBuff, InSym_Real, Code);
                          if Code <> 0 then Error(elx_Globals, err_NumTooLarge)
                        end
                 else begin                         { Its a regular integer }
                        CurSymbol := sym_IntCon;
                        if Code <> 0 then
                          begin
                            Error(elx_Globals, err_NumTooLarge);
                            InSym_Integer := 0;
                          end; { if }
                      end; { else }

                 if UpCase(LastCH) in ['A'..'Z'] then
                   Error(elx_Globals, err_WrongNum);
               end { if number }

        else Case elx_Globals.BlockVars^.LastCH of
                {-- ":" -----------------------------------------------------}
                ':' : With elx_Globals, BlockVars^ do
                      begin
                        NextCH(elx_Globals);

                        if (LastCH = '=') then
                          begin
                            CurSymbol := sym_Becomes;
                            NextCH(elx_Globals);
                          end
                            else CurSymbol := sym_Colon;
                       end; { ":" }

                {-- "<" -----------------------------------------------------}
                '<' : With elx_Globals, BlockVars^ do
                       begin
                        NextCH(elx_Globals);

                        if (LastCH = '=') then
                          begin
                            CurSymbol := sym_leq;
                            NextCH(elx_Globals);
                          end
                            else begin
                                    if LastCH = '>' then
                                     begin
                                       CurSymbol := sym_neq;
                                       NextCH(elx_Globals);
                                     end
                                       else CurSymbol := sym_Lss;
                                 end; { if }
                      end; { <, <=, <> }

                {-- ">" -----------------------------------------------------}
                '>' : With elx_Globals, BlockVars^ do
                       begin
                        NextCH(elx_Globals);

                        if (LastCH = '=') then
                          begin
                            CurSymbol := sym_geq;
                            NextCH(elx_Globals);
                          end
                            else CurSymbol := sym_gtr;
                      end; { >, >= }

                {-- "." -----------------------------------------------------}
                '.' : With elx_Globals, BlockVars^ do
                       begin
                        NextCH(elx_Globals);

                        if (LastCH = '.') then
                          begin
                            CurSymbol := sym_TwoDots;
                            NextCH(elx_Globals)
                          end
                            else CurSymbol := sym_Period;
                      end; { ., .. }

                {-- ".." ----------------------------------------------------}
             dotdot : With elx_Globals, BlockVars^ do
                      begin
                        CurSymbol := sym_TwoDots;
                        NextCH(elx_Globals);
                      end; { .. }

                {-- "''" ----------------------------------------------------}
               '"',
               '''' : With elx_Globals, BlockVars^ do
                       begin
                        StrBuff := '';
                        StrTerm := elx_Globals.BlockVars^.LastCH;

              { LABEL } _2: NextCH(elx_Globals);

                           if (LastCH = StrTerm) then
                             begin
                               NextCH(elx_Globals);

                               if LastCH = '\' then
                                 begin
                                   NextCH(elx_Globals); { retrieve the next char }

                                   While (BlockVars^.LastCH in [#32, #9]) AND (NOT AbortCompile) do
                                     NextCH(elx_Globals);

                                   if LastCH = StrTerm then
                                     begin
                                       NextCH(elx_Globals);
                                     end
                                       else Error(elx_Globals, err_StrExpected);
                                 end { if }
                                   else if (LastCH <> StrTerm) then Goto _3;
                             end; { if }

                           {-- Only include "normal" characters -------------}
                           if Length(StrBuff) < maxStringLen then
                             StrBuff := StrBuff + LastCH
                               else Error(elx_Globals, err_StrExpected);

                           {-- we dont check for unterminated strings -------}
                           {-- anymore, strings can be multi-lined ----------}
                           {-- however, we do manually add the enters -------}
                           if CharCount = 1 then
                             {$IFDEF ELEUNIX}
                               StrBuff := Copy(StrBuff, 1, Length(StrBuff) - 1)
                                            + #13 +
                                          Copy(StrBuff, Length(StrBuff), 1);
                             {$ELSE}
                               StrBuff := Copy(StrBuff, 1, Length(StrBuff) - 1)
                                            + #13 + #10 +
                                          Copy(StrBuff, Length(StrBuff), 1);
                             {$ENDIF}

                           GOTO _2;
              { LABEL } _3: if Length(StrBuff) = 1 then
                             begin
                               CurSymbol := sym_CharCon;
                               InSym_Integer := Ord(StrBuff[1]);
                             end
                               else begin
                                      CurSymbol := sym_StringCon;
                                      sLeng := Length(StrBuff);

                                      if sLeng = 0 then
                                        StringPtr := NulString
                                          else begin
                                                 StrAllocNew(elx_Globals, StringPtr, sLeng);
                                                 {$IFDEF MSDOS}
                                                   StringTable^[StringPtr]^.Content := StrBuff;
                                                   StringTable^[StringPtr]^.Len := Length(StrBuff);
                                                 {$ELSE}
                                                   StrPCopy(PChar(StringTable^[StringPtr]^.Content), StrBuff);
                                                   StringTable^[StringPtr]^.Len := Length(StrBuff);
                                                 {$ENDIF}

                                                 Counter := StringPtr;
                                               end { else }
                                    end { string, not one char }
                      end; { '''' }

                {-- "(" -----------------------------------------------------}
                '(' : With elx_Globals, BlockVars^ do
                       begin
                        NextCH(elx_Globals);

                        if LastCH <> '*' then
                          CurSymbol := sym_lParent
                            else begin                        { its comment }
                                   NextCH(elx_Globals);

                                   if LastCH ='$' then         { .. $?+ etc }
                                     Options;

                                   REPEAT
                                     While (LastCH <> '*') AND (NOT AbortCompile)  do
                                       NextCH(elx_Globals);

                                     NextCH(elx_Globals);
                                   UNTIL (LastCH = ')') OR (AbortCompile);

                                   NextCH(elx_Globals);
                                   Goto _1;
                                 end; { else }
                      end; { ( }

                {-- "?????" -------------------------------------------------}
                '{' : With elx_Globals, BlockVars^ do              { Comment }
                       begin
                        NextCH(elx_Globals);

                        if (LastCH = '$') then
                          Options;

                        While (LastCH <> '}') AND (NOT AbortCompile) do
                          NextCH(elx_Globals);

                        NextCH(elx_Globals);
                        Goto _1;
                      end; {  }

                {-- "$" -----------------------------------------------------}
                '$' : With elx_Globals, BlockVars^ do    { Hexadecimal stuff ;) }
                       begin
                        NextCH(elx_Globals);

                        Counter := 0;
                        StrBuff := '$';

                        While (UpCase(LastCH) in ['0'..'9', 'A'..'F']) AND (NOT AbortCompile) do
                          begin
                            Inc(Counter);
                            StrBuff := StrBuff + LastCH;
                            NextCH(elx_Globals);
                          end; { while }

                        if (Counter = 0) OR (UpCase(LastCH) in ['G'..'Z']) then
                          begin
                            Error(elx_Globals, err_WrongNum)
                          end
                            else begin
                                   if Counter > 8 then Error(elx_Globals, err_NumTooLarge)
                                    else Val(StrBuff, InSym_Integer, Code);
                                 end; { else }

                        CurSymbol := sym_IntCon;
                      end; { $ }

                {-- "#" -----------------------------------------------------}
                '#' : With elx_Globals, BlockVars^ do       { Char (#) stuff }
                       begin
                        NextCH(elx_Globals);

                        Counter := 0;
                        StrBuff := '';

                        While (UpCase(LastCH) in ['0'..'9']) AND (NOT AbortCompile) do
                          begin
                            Inc(Counter);
                            StrBuff := StrBuff + LastCH;
                            NextCH(elx_Globals);
                          end; { while }

                        if (Counter = 0) then
                          begin
                            Error(elx_Globals, err_WrongNum)
                          end
                            else begin
                                   if Counter > 4 then Error(elx_Globals, err_NumTooLarge)
                                    else Val(StrBuff, InSym_Integer, Code);
                                 end; { else }

                        CurSymbol := sym_CharCon;
                      end; { '#' }

             {-- fall back routine ------------------------------------------}
               else With elx_Globals, BlockVars^ do
                     begin
                      NextCH(elx_Globals);
                      if AbortCompile then EXIT;

                      Error(elx_Globals, err_InvChar);
                      Goto _1
                    end; { else }

             end; { case }
end; { proc. InSymbol }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AddArray(var elx_Globals: elx_GlobalsType;
                   paramType : tType;
                   paramLow  : Longint;
                   paramHigh : Longint);
begin
  if (paramLow > paramHigh) then  { Low bound cant be higher than high bound }
     Error(elx_Globals, err_IndexBound);

  {-- Make sure that the array doesnt exceed our limits ---------------------}
  if (ABS(paramLow) > MaxArBound) OR (ABS(paramHigh) > MaxArBound) then
    begin
      Error(elx_Globals, err_IndexBound);

      paramLow := 0;
      paramHigh := 0;
    end; { if }

  {-- If theres space, add the array ----------------------------------------}
  if elx_Globals.ArrayCount = MaxArray then
    FatalError(elx_Globals, fatal_Arrays)
      else
       with elx_Globals do
           begin
             Inc(ArrayCount);

	     New(ArrayTable^[ArrayCount], Init);
             With ArrayTable^[ArrayCount]^ do
               begin
                 ArrayInf.IdxTyp := paramType;
                 ArrayInf.Low := paramLow;
                 ArrayInf.High := paramHigh;
               end; { with }
           end; { else }
end; { proc. AddArray }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AddDynArray(var elx_Globals: elx_GlobalsType);
begin
  {-- If theres space, add the array ----------------------------------------}
  if elx_Globals.DynArrayCount = MaxDynArray then
    FatalError(elx_Globals, fatal_DynArray)
      else
       with elx_Globals do
           begin
             {-- increase the actual number of dynamic strng arrays ---------}
             Inc(DynArrayCount);

             {-- and setup the table ----------------------------------------}
             {$IFNDEF MSDOS}
             With DynArrayTable^[DynArrayCount] do
               begin
                 LastElementLd := -1;
               end; { with }
             {$ENDIF}
           end; { else }
end; { proc. AddDynArray }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AddBlock(var elx_Globals: elx_GlobalsType);
begin
  with elx_Globals do
    begin
      if BlockCount = maxBlock then
        FatalError(elx_Globals, fatal_Procedures)
          else begin
                 Inc(BlockCount);

                 BlockTable^[BlockCount].Last := 0;
                 BlockTable^[BlockCount].LastPar := 0
               end { else }
    end; { with }
end; { proc. AddBlock }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AddReal(var elx_Globals: elx_GlobalsType;
                  X: Real);
begin
  with elx_Globals do
    begin
      if RealCount2 = (MaxReals - 1) then
        FatalError(elx_Globals, fatal_Reals)
          else begin
                 RealsTable[RealCount2 + 1] := X;
                 RealCount1 := 1;

                 While RealsTable[RealCount1] <> X do
                   Inc(RealCount1);

                 if RealCount1 > RealCount2 then RealCount2 := RealCount1;
               end; { else }
    end; { with }
end; { proc. AddReal }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Emit(var elx_Globals: elx_GlobalsType;
               Funct: Integer);
begin
  with Elx_Globals do
    begin
      if CurCode = MaxCode then
        FatalError(elx_Globals, fatal_Code);

      ProgCode[CurCode].Func := Funct;
      Inc(CurCode);
    end; { with }
end; { proc. Emit }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Emit1(var elx_Globals: elx_GlobalsType;
                Funct, Param: Longint);
begin
  if elx_Globals.CurCode = MaxCode then
    FatalError(elx_Globals, fatal_Code);

  With elx_Globals, ProgCode[CurCode] do
    begin
      Func := Funct;
      Y := Param;
    end; { with }

  Inc(elx_Globals.CurCode);
end; { proc. Emit1 }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Emit2(var elx_Globals: elx_GlobalsType;
                Funct, Param1, Param2: Integer);
begin
  if elx_Globals.CurCode = MaxCode then
    FatalError(elx_Globals, fatal_Code);

  With elx_Globals, ProgCode[CurCode] do
    begin
      Func := Funct;
      X := Param1;
      Y := Param2;
    end; { with }

  Inc(elx_Globals.CurCode);
end; { proc. Emit2 }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure PrintTables(var elx_Globals: elx_GlobalsType);
var Counter: Integer;
    OpCode : OpCodeType;
begin
  {-- Print a list with all identifiers we have -----------------------------}
  if elx_Globals.LogErrors then
   with elx_Globals do
    begin
      ErrorFile^.WriteLn('');
      ErrorFile^.WriteLn('');
      ErrorFile^.WriteLn('');
      ErrorFile^.WriteLn('   identifiers link  obj  typ  ref  nrm  lev  adr msze');
      ErrorFile^.WriteLn('');

      for Counter := 674 to IdentCount do
        With IdentTable^[Counter] do
          ErrorFile^.WriteLn(IntStr(Counter, 0) + ' ' +
                             Name + IntStr(Link, 5) +
                             IntStr(Ord(Obj), 5) +
                             IntStr(Ord(Typ), 5) +
                             IntStr(Ref, 5) +
                             IntStr(Ord(Normal), 5) +
                             IntStr(Lev, 5) +
                             IntStr(Adr, 5) +
                             IntStr(MaxSize, 5));

      {-- Print a list with all blocks that we have -----------------------------}
      ErrorFile^.WriteLn('');
      ErrorFile^.WriteLn('');
      ErrorFile^.WriteLn('');
      ErrorFile^.WriteLn('blocks    last lpar psze vsze');
      ErrorFile^.WriteLn('');

      for Counter := 1 to BlockCount do
        With BlockTable^[Counter] do
          ErrorFile^.WriteLn(IntStr(Counter, 4) +
                             IntStr(Last, 9) +
                             IntStr(Lastpar, 5) +
                             IntStr(pSize, 5) +
                             IntStr(vSize, 5));

      {-- Print a list with all arrays that we have declared --------------------}
      ErrorFile^.WriteLn('');
      ErrorFile^.WriteLn('');
      ErrorFile^.WriteLn('');
      ErrorFile^.WriteLn('arrays    xtyp etyp eref  low high elsz size');
      ErrorFile^.WriteLn('');

      for Counter := 01 to ArrayCount do
        With ArrayTable^[Counter]^ do
          ErrorFile^.WriteLn(IntStr(Counter, 4) +
                             IntStr(Ord(ArrayInf.IdxTyp), 9) +
                             IntStr(Ord(ArrayInf.ElementType), 5) +
                             IntStr(ArrayInf.ElementRef, 5) +
                             IntStr(ArrayInf.Low, 5) +
                             IntStr(ArrayInf.High, 5) +
                             IntStr(ArrayInf.ElementSize, 5) +
                             IntStr(ArrayInf.Totalsize, 5));


      {-- Print a list with all dynarrays that we have declared -----------------}
      ErrorFile^.WriteLn('');
      ErrorFile^.WriteLn('');
      ErrorFile^.WriteLn('');
      ErrorFile^.WriteLn('dynarrays xtyp etyp eref  low high elsz size');
      ErrorFile^.WriteLn('');

      {$IFNDEF MSDOS}
      for Counter := 01 to DynArrayCount do
        With DynArrayTable^[Counter] do
          ErrorFile^.WriteLn(IntStr(Counter, 4) +
                             IntStr(Ord(typ_ints), 9) +
                             IntStr(Ord(ElementType), 5) +
                             IntStr(ElementRef, 5) +
                             IntStr(HighestItem, 5) +   { ?? }
                             IntStr(LowestItem, 5) +    { ?? }
                             IntStr(ElementSize, 5) +
                             IntStr(Totalsize, 5));
      {$ENDIF}

      {-- Print a list with all (op)codes we have in our program ----------------}
      ErrorFile^.WriteLn('');
      ErrorFile^.WriteLn('');
      ErrorFile^.WriteLn('');
      ErrorFile^.WriteLn(' code:');
      ErrorFile^.WriteLn('');
      ErrorFile^.WriteLn(' Code  Func  X    Y');

      for Counter := 0 to (CurCode - 1) do
        begin
          ErrorFile^.Write(IntStr(Counter, 5) + #32);

          OpCode := ProgCode[Counter];
          ErrorFile^.Write(IntStr(OpCode.Func, 5) + #32);

          if OpCode.Func < 100 then
            begin
              if (OpCode.Func in [0, 1, 2, 3, 33]) then
                ErrorFile^.Write(IntStr(OpCode.x, 2) + IntStr(OpCode.y, 5))
                 else ErrorFile^.Write(IntStr(OpCode.y, 7));
            end
              else ErrorFile^.Write('       ');

          ErrorFile^.WriteLn('  (' + OpCodeToStr(OpCode.Func) + '),');
        end; { for }

      ErrorFile^.WriteLn('');
      ErrorFile^.WriteLn('Starting address is ' + IntStr(IdentTable^[BlockTable^[1].Last].Adr, 5))
    end; { if LogErrors }
end; { proc. PrintTables }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Enter(var elx_Globals: elx_GlobalsType;
                ID    : AlfaType;
                IsObj : tObject;
                Level : Integer);
var Counter : Integer;
    OrigLink: Integer;
begin
  if elx_Globals.IdentCount = MaxSyms then
    FatalError(elx_Globals, fatal_Idents)
      else
       with elx_Globals do
         begin
             IdentTable^[0].Name := ID;
             Counter := BlockTable^[Display^[Level]].Last;
             OrigLink := Counter;

             { -- Search for the identifier -------------------------------}
             While IdentTable^[Counter].Name <> ID do
               Counter := IdentTable^[Counter].Link;
                if Counter <> 0 then Error(elx_Globals, err_DupIdent){ If duplicate, throw an error }
                  else begin
                         Inc(IdentCount);

                         if IdentCount > HighIdentCount then
                           HighIdentCount := IdentCount;

                         With IdentTable^[IdentCount] do
                           begin
                             Name := ID;
                             Link := OrigLink;
                             Obj  := IsObj;
                             Typ  := typ_noTyp;
                             Ref  := 00;
                             Lev  := Level;
                             Adr  := 0;
                             MaxSize := -1;
                          end; { with }

                         BlockTable^[Display^[Level]].Last := IdentCount;
                       end; { if not found }
           end; { else begin }
end; { proc. Enter }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Block(var elx_Globals: elx_GlobalsType;
                fSys: SymbolSet; IsFunc: Boolean; Level: Longint);
type ItemType = record
                   Typ  : tType;                            { Type of item }
                   Ref  : IndexType;
                   Temp : Boolean;
                end; { ItemType }

      ConvRec = record                       { Record used to convert vars }
                   Case TP: tType of
                      typ_Ints,
                      typ_Chars,
                      typ_Bools   : (I: Integer);
                      typ_Reals   : (R: Real);
                   end; { case }

var DataAllocd : Integer;                         { Data allocation index }
    IdentIndex : Integer;                 { Ident.index of this procedure }
    BlockIndex : Integer;                 { Block.index of this procedure }

  procedure Skip(fSys: SymbolSet; ErrorNr: Integer);
  { Skip all symbols till we found one symbol that matches the ones }
  { defined in "fSys" }
  begin
    Error(elx_Globals, ErrorNr);
    elx_Globals.BlockVars^.SkipFlag := TRUE;

    With elx_Globals, BlockVars^ do
      While (NOT (CurSymbol in fSys)) AND (NOT AbortCompile) do
        InSymbol(elx_Globals);

    if elx_Globals.BlockVars^.SkipFlag then EndSkip(elx_Globals);
  end; { proc. Skip }


  procedure Test(S1, S2: SymbolSet; ErrorNr: Integer);
  { If the last symbol is not in set "s1" then skip everything till we }
  { encounter a symbol thats in S1 or S2 }
  begin
    if (NOT (GetCurSymbol(elx_Globals) IN S1)) then
      Skip(S1 + S2, ErrorNr);
  end; { proc. test }


  procedure TestSemiColon;
  { Make sure the current symbol is a semi colon, else throw an error }
  begin
    if GetCurSymbol(elx_Globals) = sym_SemiColon then InSymbol(elx_Globals)
      else begin
             Error(elx_Globals, err_MissSemi);

             if GetCurSymbol(elx_Globals) in [sym_comma, sym_colon] then
               InSymbol(elx_Globals);
           end; { else }

    Test([Sym_Ident] + elx_Globals.BlockVars^.BlockBegSys, fSys, err_Syntax);
  end; { proc. TestSemiColon }

  function Loc(ID: AlfaType; ThrowError: Boolean): Integer;
  { This routine starts at the last entered ID, and search it's way backup }
  { to find the correct identifier. It starts with the current level, and }
  { ends at zero. This is to ensure 'local' variables, are actually local }
  var Counter: Integer;
      TempInt: Integer;
  begin
    Counter := Level;
    elx_Globals.IdentTable^[0].Name := ID; { Make sure we always find this ID }

    with elx_Globals do
      REPEAT
        TempInt := BlockTable^[Display^[Counter]].Last;

        While IdentTable^[TempInt].Name <> ID do
          TempInt := IdentTable^[TempInt].Link;

        Dec(Counter);
      UNTIL (Counter < 0) OR (TempInt <> 0) OR (AbortCompile);

    if TempInt = 0 then
     if ThrowError then
      Error(elx_Globals, err_UnkIdent);               { Unknown identifier }
    Loc := TempInt;
  end; { func. Loc }


  procedure EnterVariable;                                { Add a variable }
  begin
    With elx_Globals, BlockVars^ do
      begin
        if GetCurSymbol(elx_Globals) = sym_Ident then
          begin
            Enter(elx_Globals, CurIdent, Obj_Variable, Level);
            InSymbol(elx_Globals);
          end
            else Error(elx_Globals, err_VarIdent);
      end; { with }
  end; { proc. EnterVariable }


  procedure Constant(fSys: SymbolSet; var Conv: ConvRec);
  var Sign: Integer;
      Temp: Integer;
  begin
    Conv.TP := typ_NoTyp;
    Conv.I  := 0;

    With elx_Globals, BlockVars^ do
     begin
       Test(ConstBegSys, fSys, err_InvConst); { Make sure it's a valid constant }

       if GetCurSymbol(elx_Globals) in ConstBegSys then
         begin
            if GetCurSymbol(elx_Globals) = sym_CharCon then
              begin
                Conv.TP := typ_Chars;
                Conv.I := InSym_Integer;
                InSymbol(elx_Globals);
              end
       else if GetCurSymbol(elx_Globals) = sym_StringCon then
              begin
                Conv.Tp := typ_Strngs;
                Conv.I := StringPtr;
                InSymbol(elx_Globals);
              end
                else begin
                       {-- If it's a + or - operator act like it ---------------}
                       Sign := 0;
                       if GetCurSymbol(elx_Globals) in [sym_Plus, sym_Minus] then
                         begin
                           if GetCurSymbol(elx_Globals) = sym_Minus then
                             Sign := -1
                               else Sign := +1;
                           InSymbol(elx_Globals);
                         end; { if }

                       if GetCurSymbol(elx_Globals) = sym_Ident then
                         begin
                           Temp := Loc(CurIdent, true);

                           if Temp <> 0 then
                             if IdentTable^[Temp].Obj <> obj_Constant then
                               Error(elx_Globals, err_ConstVar)
                                else begin
                                       Conv.Tp := IdentTable^[Temp].Typ;
                                       if Conv.Tp in [typ_ints, typ_reals] then
                                         if Sign = 0 then Sign := 1;

                                       if Conv.Tp = typ_reals then
                                         Conv.R := Sign * RealsTable[IdentTable^[Temp].adr]
                                          else begin
                                                 if Conv .Tp = typ_ints then
                                                   Conv.I := Sign * IdentTable^[Temp].Adr
                                                    else begin
                                                           if Sign <> 0 then
                                                             Error(elx_Globals, err_NoArithm);
                                                           Conv.i := IdentTable^[Temp].adr
                                                         end; { else }
                                               end; { else }
                                     end; { else }

                           InSymbol(elx_Globals)
                         end
                           else begin
                                  if Sign = 0 then Sign := 1;

                                  if GetCurSymbol(elx_Globals) = sym_IntCon then
                                    begin
                                      Conv.Tp := typ_Ints;
                                      Conv.I := Sign * InSym_Integer;
                                      InSymbol(elx_Globals);
                                    end
                                      else if GetCurSymbol(elx_Globals) = sym_RealCon then
                                             begin
                                               Conv.tp := typ_reals;
                                               Conv.R := Sign * inSym_Real;
                                               InSymbol(elx_Globals)
                                             end
                                              else Skip(fSys, err_InvConst);
                                end { else }
                     end; { else }

            Test(fSys, [], err_Syntax);
         end; { if }
     end; { with }
  end; { proc. Constant }



  procedure Typ(fSys: SymbolSet; var IsTyp: tType; var IsRef, IsSize, IsMaxSize: Integer);
  var ElementTyp : tType;
      ElementRef : Integer;
      ElementSize: Integer;
      ElementMaxSize: Integer;
      Counter    : Integer;
      OffSet     : Integer;     { FieldNr * Size of each field in a record }
      T0         : Integer;                  { Start integer of IdentCount }
      T1         : Integer;                      { End count of IDentCount }
      Dummy      : ConvRec;
      BlockSize  : Integer;          { Size (in bytes) of this whole block }

    procedure StaticArrayTyp(var ArNr, ArSize: Integer);
    var ElementTyp : tType;
        ElementRef : Integer;
        ElementSize: Integer;
        ElMaxSize  : Integer;
        Low        : ConvRec;
        High       : ConvRec;
    begin
      ElMaxSize := -1;
      Constant([sym_Twodots, sym_rbrack, sym_rparent, sym_of] + fSys, Low);

      if Low.Tp in [typ_reals, typ_strngs] then
        With elx_Globals, BlockVars^ do
        begin
          Error(elx_Globals, err_IndexBound);
          Low.Tp := typ_Ints;
          Low.I := 0;
        end; { if }

      if GetCurSymbol(elx_Globals) = sym_TwoDots then InSymbol(elx_Globals)
        else Error(elx_Globals, err_MissDtDt);              { ".." expected }

      {-- Get the high bound -----------------------------------------------}
      Constant([sym_rbrack, sym_comma, sym_rparent, sym_of] + fSys, High);

      if High.TP <> Low.TP then     { if <X1>..<x2> where x1 is other type }
        begin                                                    { than x2 }
          Error(elx_Globals, err_IndexBound);
          High.I := Low.I;
        end; { if }

      {-- Actually add the array -------------------------------------------}
      AddArray(elx_Globals, Low.TP, Low.I, High.I);
      ArNr := elx_Globals.ArrayCount;

      if GetCurSymbol(elx_Globals) = sym_comma then
        begin
          InSymbol(elx_Globals);
          ElementTyp := typ_Arrays;
          StaticArrayTyp(ElementRef, ElementSize);
        end
          else begin
                 if GetCurSymbol(elx_Globals) = sym_rbrack then
                   InSymbol(elx_Globals)
                     else begin
                            Error(elx_Globals, err_MissRBrk);
                            if GetCurSymbol(elx_Globals) = sym_rparent then
                              InSymbol(elx_Globals);
                          end; { else }

                 {-- Type of array should be defined now ---------------------}
                 if GetCurSymbol(elx_Globals) = sym_of then
                   Insymbol(elx_Globals)
                     else Error(elx_Globals, err_MissOf);

                 typ(fSys, ElementTyp, ElementRef, ElementSize, ElMaxSize);
               end; { else }

      {-- Fill in all the data for this array ------------------------------}
      with elx_Globals do
        ArSize := (ArrayTable^[ArNr]^.ArrayInf.High - ArrayTable^[ArNr]^.ArrayInf.Low + 1) * ElementSize;
      if ArSize > StackSize then                     { Array is too large }
        Error(elx_Globals, err_ArTooLarge);

      {-- correct for strings ----------------------------------------------}
      if ElMaxSize < 0 then
        begin
          Case ElementTyp of
             typ_ints   : ElMaxSize := 2;
             typ_Reals  : ElMaxSize := SizeOf(Real);
             typ_Bools  : ElMaxSize := SizeOf(Boolean);
             typ_Chars  : ElMaxSize := SizeOf(Char);
             typ_Strngs : ElMaxSize := 255 + 1;
           end; { case }
        end
          else begin
                 Case ElementTyp of
                   typ_Strngs : ElMaxSize := ElMaxSize + 1;
                 end; { case }
               end; { else }

      {-- and write the actual values --------------------------------------}
      with elx_Globals do
        begin
          ArrayTable^[ArNr]^.ArrayInf.TotalSize := ArSize; { Size (in bytes) of total array }
          ArrayTable^[ArNr]^.ArrayInf.ElementType := ElementTyp;
          ArrayTable^[ArNr]^.ArrayInf.ElementRef  := ElementRef;
          ArrayTable^[ArNr]^.ArrayInf.ElementSize := ElementSize;
          ArrayTable^[ArNr]^.ArrayInf.ElementMaxSize := ElMaxSize;
        end; { with }
    end; { proc. StaticArrayTyp }


    procedure DynamicArrayTyp(var ArNr, ArSize: Integer);
    var ElementTyp : tType;
        ElementRef : Integer;
        ElementSize: Integer;
        ElMaxSize  : Integer;
        Low        : ConvRec;
        High       : ConvRec;
    begin
      ElMaxSize := -1;

      {-- Actually add the array -------------------------------------------}
      AddDynArray(elx_Globals);
      ArNr := elx_Globals.DynArrayCount;

      {-- Type of array should be defined now ------------------------------}
      if GetCurSymbol(elx_Globals) = sym_of then
        Insymbol(elx_Globals)
          else Error(elx_Globals, err_MissOf);

      {-- get the actual assignment ----------------------------------------}
      typ(fSys, ElementTyp, ElementRef, ElementSize, ElMaxSize);

      {-- Fill in all the data for this array ------------------------------}
      with elx_Globals do
        ArSize := ElementSize;
      if ArSize > StackSize then                     { Array is too large }
        Error(elx_Globals, err_ArTooLarge);

      {-- correct for strings ----------------------------------------------}
      if ElMaxSize < 0 then
        begin
          Case ElementTyp of
             typ_ints   : ElMaxSize := 2;
             typ_Reals  : ElMaxSize := SizeOf(Real);
             typ_Bools  : ElMaxSize := SizeOf(Boolean);
             typ_Chars  : ElMaxSize := SizeOf(Char);
             typ_Strngs : ElMaxSize := 255 + 1;
           end; { case }
        end
          else begin
                 Case ElementTyp of
                   typ_Strngs : ElMaxSize := ElMaxSize + 1;
                 end; { case }
               end; { else }

      {-- and write the actual values --------------------------------------}
      with elx_Globals do
        begin
          {$IFNDEF MSDOS}
            DynArrayTable^[ArNr].TotalSize := ArSize; { Size (in bytes) of total array }
            DynArrayTable^[ArNr].ElementType := ElementTyp;
            DynArrayTable^[ArNr].ElementRef  := ElementRef;
            DynArrayTable^[ArNr].ElementSize := ElementSize;
            DynArrayTable^[ArNr].ElementMaxSize := ElMaxSize;
            DynArrayTable^[ArNr].ChildIsDynamic := (ElementTyp = typ_DynArray);
          {$ENDIF}
        end; { with }
    end; { proc. DynamicArrayTyp }

  begin
    {-- Initialize this type to null by default ----------------------------}
    IsTyp := typ_NoTyp;
    IsRef := 0;
    IsSize:= 0;
    IsMaxSize := -1;

    {-- Is there PACKED before the array of record ?? ----------------------}
    if GetCurSymbol(elx_Globals) = sym_Packed then
     With elx_Globals, BlockVars^ do
      begin
        InSymbol(elx_Globals);

        if (NOT (GetCurSymbol(elx_Globals) = sym_Array) OR (GetCurSymbol(elx_Globals) = sym_Record)) then
          Error(elx_Globals, err_CantPack);                   { "Packed" not applicable }
      end; { if }

    {-- Test wether this is the real definition of the type ----------------}
    test(elx_Globals.BlockVars^.TypeBegSys, fSys, err_CantPack);
    if (GetCurSymbol(elx_Globals) in elx_Globals.BlockVars^.TypeBegSys) then
     With elx_Globals, BlockVars^ do
      begin
        if GetCurSymbol(elx_Globals) = sym_ident then
          begin
            Counter := Loc(CurIdent, true);       { Lookup this identifier }

            if Counter <> 0 then
              begin
                With IdentTable^[Counter] do
                  begin
                    if Obj <> Obj_Type then Error(elx_Globals, err_NoType)
                      else begin
                             IsTyp := Typ;
                             IsRef := Ref;
                             IsSize := Adr;
                             IsMaxSize := MaxSize;

                             if IsTyp = typ_NoTyp then Error(elx_Globals, err_UndefType);
                           end; { if }
                  end; { with }
              end; { if }

        InSymbol(elx_Globals);                                    { Get the next symbol }
        {-- Is it String[xx] -----------------------------------------------}
        if (isTyp = typ_Strngs) AND (GetCurSymbol(elx_Globals) = sym_lBrack) then
          begin
            InSymbol(elx_Globals);
            Constant([Sym_rBrack] + fSys, Dummy);

            if GetCurSymbol(elx_Globals) = sym_rBrack then
              InSymbol(elx_GLobals)
                else Error(elx_Globals, err_MissRBrk);
            IsMaxSize := Dummy.i;
          end; { if }
        {!!}
        {-- Is it Integer[xx] ----------------------------------------------}
        if (isTyp = typ_Ints) AND (GetCurSymbol(elx_Globals) = sym_lBrack) then
          begin
            InSymbol(elx_Globals);
            Constant([Sym_rBrack] + fSys, Dummy);

            if GetCurSymbol(elx_Globals) = sym_rBrack then
              InSymbol(elx_Globals)
                else Error(elx_Globals, err_MissRBrk);
            IsMaxSize := Dummy.i;
          end; { if }
      end
        else if (GetCurSymbol(elx_Globals) = sym_dynArray) then
               begin
                 {-- get the actual symbol ----------------------------------}
                 InSymbol(elx_Globals);

                 {-- its a string array -------------------------------------}
                 IsTyp := typ_DynArray;
                 DynamicArrayTyp(isRef, isSize);
               end { sym_DynArray }
        else if (GetCurSymbol(elx_Globals) = sym_array) then
               begin
                 InSymbol(elx_Globals);

                 if (GetCurSymbol(elx_Globals) = sym_lbrack) then InSymbol(elx_Globals)
                   else begin
                          Error(elx_Globals, err_MissLBrk);
                          if (GetCurSymbol(elx_Globals) = sym_lParent) then
                            InSymbol(elx_Globals);
                        end; { else }

                 IsTyp := typ_arrays;
                 StaticArrayTyp(isRef, isSize);
               end
                 else begin                                      { Records }
                        InSymbol(elx_Globals);
                        AddBlock(elx_Globals);

                        {-- Initialize variables ----------------------------}
                        IsTyp := typ_Records;
                        IsRef := BlockCount;
                        if Level = MaxLevel then
                          FatalError(elx_Globals, fatal_Levels);

                        Inc(Level);
                        Display^[Level] := BlockCount;
                        OffSet := 0;
                        BlockSize := 0;

                        {-- Get all fields in this record -------------------}
                        While (NOT (GetCurSymbol(elx_Globals) in fSys - [sym_semicolon, sym_comma, sym_ident] + [sym_end]))
                               AND (NOT AbortCompile) do
                          begin
                            if (GetCurSymbol(elx_Globals) = sym_ident) then
                              begin
                                T0 := IdentCount;   { Get start ID counter }
                                EnterVariable;

                                While (GetCurSymbol(elx_Globals) = sym_comma) AND (NOT AbortCompile) do
                                  begin
                                    InSymbol(elx_Globals);
                                    EnterVariable;
                                  end; { while }

                                if (GetCurSymbol(elx_Globals) = sym_colon) then
                                  InSymbol(elx_Globals)
                                    else Error(elx_Globals, err_CollMiss);

                                T1 := IdentCount;          { Get the new # }
                                { We now have two numbers. The ID count of }
                                { before the declaraction (T0) and the }
                                { count after the declaration T1 }
                                { If you take (T1 - T0) youll get the }
                                { total number of identifiers in this }
                                { record }
                                Typ(fSys + [sym_semicolon, sym_end,
                                            sym_comma, sym_ident],
                                    ElementTyp, ElementRef, ElementSize, ElementMaxSize);

                                {-- Add all idenitfiers to the ID table -----}
                                While (T0 < T1) AND (NOT AbortCompile) do
                                  begin
                                    Inc(T0);

                                    With IdentTable^[T0] do
                                      begin
                                        Typ := ElementTyp;
                                        Ref := ElementRef;
                                        Normal := true;
                                        Adr := offset;
                                        MaxSize := ElementMaxSize;

                                        Inc(Offset, ElementSize);
                                        Inc(BlockSize, GetFieldSize(elx_Globals, IdentTable^[T0]));
                                      end; { with }
                                  end; { while }
                              end; { if Sym_Ident }

                            if (GetCurSymbol(elx_Globals) <> sym_end) then
                              begin
                                if (GetCurSymbol(elx_Globals) = sym_semicolon) then
                                  InSymbol(elx_Globals)
                                    else begin
                                           Error(elx_Globals, err_MissSemi);
                                           if GetCurSymbol(elx_Globals) = sym_comma then
                                             InSymbol(elx_Globals);
                                         end; { else }

                                Test([sym_ident,sym_end,sym_semicolon], fsys, err_Syntax)
                              end; { if }
                          end; { retrieve all fields }

                        BlockTable^[IsRef].vSize := OffSet;
                        BlockTable^[IsRef].ByteSize := BlockSize;

                        if BlockTable^[IsRef].ByteSize > MaxRecByteSize then
                          FatalError(elx_Globals, fatal_RecordSize);

                        IsMaxSize := BlockSize;
                        IsSize := Offset;
                        if (IsSize > StackSize) then
                          Error(elx_Globals, err_ArTooLarge);

                        BlockTable^[IsRef].pSize := 0;
                        InSymbol(elx_Globals);
                        Dec(Level);
                      end; { records }

        Test(fSys, [], err_Syntax)
      end; { if }
  end; { proc. Typ }


  { ParameterList: Where a procedure/function describes all the parameters }
  { it can use }
  procedure ParameterList;                          { Formal parameter list }
  var Typ     : tType;
      Ref     : Integer;
      Size    : Integer;
      MaxSz   : Longint;
      ValPar  : Boolean;
      Counter : Integer;
      TempInt : Integer;
  begin
    InSymbol(elx_Globals);
    Typ := typ_NoTyp;
    Ref := 0;
    Size:= 0;

    Test([sym_Ident, sym_Var], fSys + [sym_rParent], err_NotUsed);

    While (GetCurSymbol(elx_Globals) in [sym_Ident, sym_Var]) AND (NOT elx_Globals.AbortCompile)  do
     With elx_Globals, BlockVars^ do
      begin
        if GetCurSymbol(elx_Globals) <> sym_var then
          ValPar := TRUE
            else begin
                   InSymbol(elx_Globals);
                   ValPar := FALSE;
                 end; { else }

        TempInt := IdentCount;
        EnterVariable;

        {-- Get all variables on this procedure/function define -------------}
        While (GetCurSymbol(elx_Globals) = sym_Comma) AND (NOT AbortCompile) do
          begin
            InSymbol(elx_Globals);
            EnterVariable;
          end; { while }

        if GetCurSymbol(elx_Globals) = sym_Colon then
          begin
            InSymbol(elx_Globals);

            if GetCurSymbol(elx_Globals) <> sym_Ident then Error(elx_Globals, err_VarIdent)
              else begin
                     Counter := Loc(CurIdent, true);
                     InSymbol(elx_Globals);

                     if Counter <> 0 then
                       if IdentTable^[Counter].obj <> obj_Type then Error(elx_Globals, err_NoType)
                           else begin
                                  Typ := IdentTable^[Counter].Typ;
                                  Ref := IdentTable^[Counter].Ref;
                                  MaxSz := IdentTable^[Counter].MaxSize;

                                  if ValPar then Size := IdentTable^[Counter].Adr
                                    else Size := 01;
                                end; { if }
                   end; { else }

            Test([sym_semicolon, sym_rparent],
                 [sym_comma, sym_ident] + fsys, err_MissSemi)
          end
            else Error(elx_Globals, err_CollMiss);

        {-- Now, register all those variables ------------------------------}
        While (TempInt < IdentCount) AND (NOT AbortCompile) do
          begin
            Inc(TempInt);

            IdentTable^[TempInt].Typ := Typ;
            IdentTable^[TempInt].Ref := Ref;
            IdentTable^[TempInt].Adr := DataAllocd;
            IdentTable^[TempInt].Lev := Level;
            IdentTable^[TempInt].Normal := ValPar;
            IdentTable^[TempInt].MaxSize := MaxSz;

            Inc(DataAllocd, Size);
          end; { while }

        {-- Make sure the parameter is closed correctly --------------------}
        if GetCurSymbol(elx_Globals) <> sym_rparent then
          begin
            if GetCurSymbol(elx_Globals) = sym_SemiColon then InSymbol(elx_Globals)
              else begin
                     Error(elx_Globals, err_MissSemi);
                     if GetCurSymbol(elx_Globals) = sym_Comma then
                       InSymbol(elx_Globals);
                   end; { else }

            Test([sym_Ident, sym_Var], [sym_rparent] + fsys, err_Syntax)
          end; { if }
      end; { while }

    if GetCurSymbol(elx_Globals) = sym_rParent then
      begin
        InSymbol(elx_Globals);
        Test([sym_semicolon, sym_colon], fsys, err_Syntax)
      end
        else Error(elx_Globals, err_RParMiss);
  end; { proc. ParameterList }


  procedure ConstDec;
  var Conv: ConvRec;
  begin
    InSymbol(elx_Globals);
    Test([Sym_Ident], elx_Globals.BlockVars^.BlockBegSys, err_VarIdent);

    While (GetCurSymbol(elx_Globals) = sym_Ident) AND (NOT elx_Globals.AbortCompile) do
     With elx_Globals, BlockVars^ do
      begin
        Enter(elx_Globals, CurIdent, obj_Constant, Level);
        InSymbol(elx_Globals);

        if GetCurSymbol(elx_Globals) = sym_eql then
          InSymbol(elx_Globals)
            else begin
                   Error(elx_Globals, err_EqlExpect);

                   if GetCurSymbol(elx_Globals) = sym_Becomes then InSymbol(elx_Globals);
                 end; { else }

        Constant([Sym_SemiColon, sym_comma, sym_ident] + fSys, Conv);
        IdentTable^[IdentCount].Typ := Conv.tp;
        IdentTable^[IdentCount].Ref := 0;
        IdentTable^[IdentCount].MaxSize := -1;

        if Conv.Tp = typ_Reals then
          begin
            AddReal(elx_Globals, Conv.R);
            IdentTable^[IdentCount].Adr := RealCount1;
          end
            else IdentTable^[IdentCount].Adr := Conv.I;

        TestSemiColon;
      end; { while }
  end; { proc. ConstDec }


  procedure TypeDeclaration;
  var Tp   : tType;
      Rf   : Integer;
      Sz   : Integer;
      MaxSz: Integer;
      T1   : Integer;
  begin
    InSymbol(elx_Globals);
    Test([sym_Ident], elx_Globals.BlockVars^.BlockBegSys, err_VarIdent);

    While (GetCurSymbol(elx_Globals) = sym_Ident) AND (NOT elx_Globals.AbortCompile) do
     With elx_Globals, BlockVars^ do
      begin
        Enter(elx_Globals, CurIdent, obj_Type, Level);

        T1 := IdentCount;
        InSymbol(elx_Globals);

        if GetCurSymbol(elx_Globals) = sym_eql then InSymbol(elx_Globals)
          else begin
                 Error(elx_Globals, err_EqlExpect);
                 if GetCurSymbol(elx_Globals) = sym_Becomes then
                   InSymbol(elx_Globals);
               end; { else }

        Typ([sym_Semicolon, sym_Comma, sym_Ident] + fSys, Tp, rf, sz, MaxSz);
        With IdentTable^[T1] do
          begin
            Typ := TP;
            Ref := Rf;
            Adr := SZ;
            MaxSize := MaxSz;
          end; { with }

        TestSemiColon;
      end; { while }
  end; { proc. TypeDeclaration }


  procedure VariableDeclaration;
  { All variables are declared here (procedure; > var b: integer < )}
  var IsTyp  : tType;
      IsRef  : Integer;
      IsSize : Integer;
      MaxSz  : Integer;
      Count0 : Integer;
      Count1 : Integer;
  begin
    InSymbol(elx_Globals);

    {-- Gather all variables declared --------------------------------------}
    While (GetCurSymbol(elx_Globals) = sym_Ident) AND (NOT elx_Globals.AbortCompile) do
     With elx_Globals, BlockVars^ do
      begin
        Count0 := IdentCount;
        EnterVariable;

        if IsTyp <> typ_DynArray then
          While (GetCurSymbol(elx_Globals) = sym_Comma) AND (NOT AbortCompile) do
            begin
              InSymbol(elx_Globals);
              EnterVariable;

              {-- we need an unique string array table per variable, so we ----}
              {-- prevent reusage of the same type per variable ---------------}
              if IsTyp = typ_DynArray then
                BREAK;
            end; { while }

        if GetCurSymbol(elx_Globals) = sym_Colon then InSymbol(elx_Globals)
          else Error(elx_Globals, err_CollMiss);

        Count1 := IdentCount;
        Typ([sym_semicolon, sym_comma, sym_ident] + fSys, IsTyp, IsRef, IsSize, MaxSz);

        {-- Declare all registers ------------------------------------------}
        While (Count0 < Count1) AND (NOT AbortCompile) do
          begin
            Inc(Count0);

            IdentTable^[Count0].Typ := IsTyp;

            {-- string arrays store their unique strings in their table ----}
            {-- therefore, variables cannot share their type definition ----}
            if IsTyp = typ_DynArray then
              begin
                AddDynArray(elx_Globals);
              end; { if }

            IdentTable^[Count0].Ref := IsRef;
            IdentTable^[Count0].Lev := Level;
            IdentTable^[Count0].Adr := DataAllocd;
            IdentTable^[Count0].Normal := TRUE;
            IdentTable^[Count0].MaxSize := MaxSz;

            Inc(DataAllocd, IsSize);
          end; { while }

        TestSemiColon;
      end; { while }
  end; { proc. VariableDeclaration }


  procedure ProcDeclaration;
  var IsFunc: Boolean;
  begin
    IsFunc := (GetCurSymbol(elx_Globals) = sym_Func);
    InSymbol(elx_Globals);

    {-- Make sure we get our proc/func name ---------------------------------}
    if GetCurSymbol(elx_Globals) <> sym_Ident then
      begin
        Error(elx_Globals, err_VarIdent);                                    { Identifier expected }
        elx_Globals.BlockVars^.CurIdent := '                              ';
      end; { if }

    {-- Add the appropriate type to the tables -----------------------------}
    if IsFunc then
      Enter(elx_Globals, elx_Globals.BlockVars^.CurIdent, obj_function, Level)
        else Enter(elx_Globals, elx_Globals.BlockVars^.CurIdent, obj_procedure, Level);

    with elx_Globals do
      IdentTable^[IdentCount].Normal := TRUE;
    InSymbol(elx_Globals);

    {-- Process the code that's beneath it ---------------------------------}
    with elx_Globals do
      Block(elx_Globals, [sym_semicolon] + fSys, IsFunc, Level + 01);

    {-- Make sure the proc/func is terminated successfully -----------------}
    if GetCurSymbol(elx_Globals) = sym_SemiColon then
      InSymbol(elx_Globals)
        else Error(elx_Globals, err_MissSemi);

    {-- Add an EXIT call ---------------------------------------------------}
    if IsFunc then
      Emit(elx_Globals, op_ExitFunc)
        else Emit(elx_Globals, op_ExitProc);
  end; { proc. ProcDeclaration }


  procedure Statement(fSys: SymbolSet);
   var I : Integer;
       NilType: ItemType;

    procedure Expression(fSys: SymbolSet; var X: ItemType); FORWARD;

    procedure Selector(fSys: SymbolSet; var IsTyp: ItemType;
                       ParentCnt: integer;
                       var FakeVar: Boolean);
    var TempItem: itemtype;
        TempAddr: Integer;
        TmpIdent: String;
        Counter : Integer;
        TmpFunc : Integer;
    begin                    { Symbol in [sym_lparent, sym_lbrack, period] }
      FakeVar := false;

      REPEAT
        if (GetCurSymbol(elx_Globals) = sym_Period) then
         With elx_Globals, BlockVars^ do
          begin                                          { Record selector }
            InSymbol(elx_Globals);                   { Skip Field selector }

            if GetCurSymbol(elx_Globals) <> sym_Ident then
              Error(elx_Globals, err_VarIdent)
                else begin
                       if NOT (IsTyp.typ in [typ_records, typ_Dynarray]) then Error(elx_Globals, err_NoRecord)
                         else begin
                                if IsTyp.Typ = typ_Records then
                                  begin
                                    Counter := BlockTable^[IsTyp.Ref].Last;
                                    IdentTable^[0].Name := CurIdent;

                                    {-- Search for the identifier ----------------}
                                    While (IdentTable^[Counter].Name <> CurIdent) do
                                      Counter := IdentTable^[Counter].Link;
                                    if Counter = 0 then Error(elx_Globals, err_UnkIdent);   { Unkn. identifier }

                                    IsTyp.Typ := IdentTable^[Counter].Typ;
                                    IsTyp.Ref := IdentTable^[Counter].Ref;
                                    TempAddr := IdentTable^[Counter].Adr;

                                    if TempAddr <> 0 then
                                      Emit1(elx_Globals, op_Offset, TempAddr);
                                  end { is a record }
                                 else
                                   if IsTyp.Typ = typ_DynArray then
                                     begin
                                        { code was: DynamicArray.<something> }
                                        { we try to figure out if the node slected was valid }
                                        {$IFNDEF MSDOS}
                                        TmpIdent := CurIdent;
                                        TmpIdent := Trim(TmpIdent);
                                        TempAddr := -1;

                                        if TmpIdent = 'high' then
                                          begin
                                            TempAddr := 0;
                                          end
                                        else if TmpIdent = 'low' then
                                          begin
                                            TempAddr := 1;
                                          end
                                        else
                                          Error(elx_Globals, err_UnkIdent);

                                        {-- now set the result --------------}
                                        isTyp.Typ := typ_Ints;
                                        IsTyp.Ref := IdentTable^[ParentCnt].Ref;
                                        FakeVar := true;

                                        {-- and set the code ----------------}
                                        Emit2(elx_Globals,
                                                 op_DynArCommand,
                                                 IsTyp.Ref,
                                                 TempAddr);
                                        {$ENDIF}
                                     end; { if }

                               InSymbol(elx_Globals);
                             end; { else }
                     end
          end
            else begin                                    { Array selector }
                    if GetCurSymbol(elx_Globals) <> sym_lBrack then Error(elx_Globals, err_MissLBrk);

                    if IsTyp.Typ = typ_Strngs then
                     With elx_Globals, BlockVars^ do
                      begin
                        InSymbol(elx_Globals);

                        Expression(fSys + [sym_rBrack], TempItem);
                        if TempItem.Typ <> typ_Ints then Error(elx_Globals, err_IntExpected)
                          else Emit(elx_Globals, op_StrIndex);

                        IsTyp.Typ := typ_Chars;
                      end else begin
                                 REPEAT
                                   InSymbol(elx_Globals);

                                   Expression(fSys + [sym_comma,sym_rbrack], TempItem);
                                   if IsTyp.Typ = typ_Arrays then
                                     begin
                                       with elx_Globals do
                                         begin
                                            TempAddr := IsTyp.Ref;

                                            if ArrayTable^[TempAddr]^.ArrayInf.Idxtyp <> TempItem.typ then
                                              Error(elx_Globals, err_IndexTyp)
                                                else if ArrayTable^[TempAddr]^.ArrayInf.ElementSize = 1 then
                                                       Emit1(elx_Globals, op_Index1, TempAddr)
                                                        else Emit1(elx_Globals, op_Index, TempAddr);

                                            IsTyp.Typ := ArrayTable^[TempAddr]^.ArrayInf.ElementType;
                                            IsTyp.Ref := ArrayTable^[TempAddr]^.ArrayInf.ElementRef
                                          end; { with }
                                     end else

                                   if IsTyp.Typ = typ_DynArray then
                                     begin
                                       with elx_Globals do
                                         begin
                                            {-- first ensure the index type is integer -}
                                            if (TempItem.Typ <> typ_ints) AND
                                                (TempItem.Typ <> typ_strngs) then
                                                   Error(elx_Globals, err_IndexTyp);

                                            {-- try to determine what index is used ----}
                                            if TempItem.Typ = typ_ints then
                                              TmpFunc := 0
                                                else TmpFunc := 1;

                                            {-- now load the proper index --------------}
                                            TempAddr := IsTyp.Ref;

                                            {$IFNDEF MSDOS}
                                            if DynArrayTable^[TempAddr].ChildIsDynamic then
                                              begin
                                                Emit2(elx_Globals, op_DynArChild,
                                                      TmpFunc, TempAddr);
                                              end
                                                else begin
                                                        Emit2(elx_Globals, op_DynArIndex,
                                                              TmpFunc, TempAddr);
                                                     end; { else }

                                            {-- and show the real type of this array ---}
                                            IsTyp.Typ := DynArrayTable^[TempAddr].ElementType;
                                            IsTyp.Ref := DynArrayTable^[TempAddr].ElementRef
                                            {$ENDIF}
                                          end; { with }
                                     end { if }
                                       else Error(elx_Globals, err_NoArray);

                                 UNTIL (GetCurSymbol(elx_Globals) <> sym_comma) OR (elx_Globals.AbortCompile);
                               end; { else }

                    if (GetCurSymbol(elx_Globals) = sym_rbrack) then InSymbol(elx_Globals)
                      else begin
                             Error(elx_Globals, err_MissRBrk);
                             if (GetCurSymbol(elx_Globals) = sym_rparent) then Insymbol(elx_Globals)
                           end; { else }
                 end; { if }

      UNTIL (NOT (GetCurSymbol(elx_Globals) in [sym_lbrack, sym_lparent, sym_period])) OR (elx_Globals.AbortCompile);

      Test(fSys, [], err_Syntax)
    end; { proc. Selector }


    procedure Call(fSys: SymbolSet; ThisIdent: Integer);
    var LastPar  : Integer;
        CurParam : Integer;
        Counter  : Integer;
        IsTyp    : ItemType;
        FakeVar  : Boolean;
    begin
      with elx_Globals do
        begin
          Emit1(elx_Globals, op_MarkStack, ThisIdent);        { Mark stack }
          LastPar := BlockTable^[IdentTable^[ThisIdent].Ref].LastPar;
          CurParam := ThisIdent;
         end; { with }

      {-- Get all parameters for called func/proc --------------------------}
      if GetCurSymbol(elx_Globals) = sym_lParent then
        begin
          REPEAT
            InSymbol(elx_Globals);
            if CurParam >= LastPar then Error(elx_Globals, err_NrParams)
              else
               with elx_Globals do
                   begin
                     Inc(CurParam);

                     {-- Call by value --------------------------------------}
                     if IdentTable^[CurParam].Normal then
                       begin                             { Value parameter }
                         Expression(fSys + [sym_comma,sym_colon,sym_rparent], IsTyp);

                         if IsTyp.Typ = IdentTable^[CurParam].Typ then
                           begin
                             if IsTyp.Ref <> IdentTable^[CurParam].Ref then
                                 Error(elx_Globals, err_VarType)

                                 else if IsTyp.typ = typ_arrays then
                                   Emit1(elx_Globals, op_LoadBlock, ArrayTable^[IsTyp.ref]^.ArrayInf.Totalsize)

                                 else if IsTyp.typ = typ_records then
                                   Emit1(elx_Globals, op_LoadBlock, BlockTable^[IsTyp.ref].vSize)

                                 else if IsTyp.typ = typ_strngs then
                                        begin
                                          if IsTyp.Temp then emit(elx_Globals, op_StrValParamTmp)
                                            else emit(elx_Globals, op_StrValueParam);
                                        end; { else }
                           end
                             else begin
                                    if (IsTyp.Typ = typ_ints) AND (IdentTable^[CurParam].typ = typ_reals) then
                                          Emit1(elx_Globals, op_LoadFloat, 0)
                                           else if IsTyp.typ <> typ_notyp then
                                             Error(elx_Globals, err_VarType);
                                  end; { else }
                   end
                   {-- VAR parameter ----------------------------------------}
                     else begin                            { VAR parameter }
                            if GetCurSymbol(elx_Globals) <> sym_ident then Error(elx_Globals, err_VarIdent)
                              else begin
                                     Counter := Loc(BlockVars^.CurIdent, true);
                                     InSymbol(elx_Globals);

                                     if Counter <> 0 then
                                       begin
                                         if IdentTable^[Counter].obj <> obj_variable then
                                           Error(elx_Globals, err_InvVarRef);

                                         IsTyp.Typ := IdentTable^[Counter].Typ;
                                         IsTyp.Ref := IdentTable^[Counter].Ref;

                                         if IdentTable^[Counter].normal then
                                           begin
                                             Emit2(elx_Globals, op_LoadAddress, IdentTable^[Counter].Lev,
                                                   IdentTable^[Counter].Adr);

                                             if IdentTable^[Counter].typ = typ_Strngs then
                                               begin
                                                 if IdentTable^[Counter].Lev <> StartBlock then
                                                  if IdentTable^[Counter].Lev = Level then
                                                    begin
                                                      Emit(elx_Globals, op_StringLink);
                                                      {WriteLn('Wheeeeeeeeeee!: ', identtable^[counter].lev, ' / ', level);}
                                                    end; { if }
                                               end; { if }

                                           end
                                            else Emit2(elx_Globals, op_LoadValue, IdentTable^[Counter].lev,
                                                       IdentTable^[Counter].adr);

                                         if GetCurSymbol(elx_Globals) in [sym_lbrack,sym_lparent,sym_period] then
                                           begin
                                             if IsTyp.Typ = typ_strngs then
                                               Error(elx_Globals, err_InvStrIdx);

                                             selector(fsys + [sym_comma, sym_colon, sym_rparent], IsTyp,
                                                      Counter, fakevar);
                                           end; { if }

                                         if (IsTyp.typ <> IdentTable^[CurParam].typ) OR
                                             (IsTyp.ref <> IdentTable^[CurParam].ref) then
                                               Error(elx_Globals, err_VarType);
                                       end; { if }
                                   end; { else }
                          end; { is var parameter }
                   end; { if }

          Test([sym_comma, sym_rparent], fsys, err_Syntax)
        until (GetCurSymbol(elx_Globals) <> sym_comma) OR (elx_Globals.AbortCompile);

        if (GetCurSymbol(elx_Globals) = sym_rparent) then
          InSymbol(elx_Globals)
            else Error(elx_Globals, err_RParMiss);
        end; { if }

      if CurParam < LastPar then
        Error(elx_Globals, err_NrParams);                             { Too few actual parameters }

      with elx_Globals do
        begin
          Emit1(elx_Globals, op_Call, BlockTable^[IdentTable^[ThisIdent].Ref].pSize - 1);
          if IdentTable^[ThisIdent].Lev < Level then
            Emit2(elx_Globals, op_UpdateDisplay, IdentTable^[ThisIdent].Lev, level)
        end; { with }
    end; { proc. Call }


    function ResultType(A, B: tType): tType;

    begin
      if (A > typ_Reals) OR (b > typ_Reals) then
        begin
          Error(elx_Globals, err_NoArithm);
          ResultType := typ_NoTyp;
        end
          else begin
                 if (A = typ_notyp) OR (B = typ_notyp) then
                   begin
                     ResultType := typ_notyp
                   end
                     else if (a = typ_ints) then
                            begin
                              if (b = typ_ints) then
                                begin
                                  ResultType := typ_ints
                                end
                                   else begin
                                          ResultType := typ_reals;
                                          Emit1(elx_Globals, op_LoadFloat, 1)
                                        end; { else }
                            end { if }
                              else begin
                                     ResultType := typ_Reals;
                                     if (b = typ_ints) then
                                       Emit1(elx_Globals, op_LoadFloat, 0)
                                   end; { else }
               end; { else }
    end; { func. ResultType }

    procedure UsersProc(Number: Integer; var RetType: ItemType; IdentPtr: Longint);
    var ParStr  : String;           { String containing the parameter list }
        NrParams: Integer;                { Number of parameters we expect }
        CurParam: Integer;                          { Parameters processed }
        TempType: ItemType;
        TmpIdent: Longint;
        TmpFunc : Longint;
        CallList: String;
        Counter : Longint;
        RecAdr  : Longint;
        RecLevel: Longint;
        LoadVarType: Boolean;


        function RecStr(L, Len: Longint): String;
        var TmpStr: String;
        begin
          Str(L, TmpStr);

          while Length(tmpStr) < Len do
            TmpStr := '0' + TmpStr;

          RecStr := TmpStr;
        end; { func. RecStr }
    begin
      CurParam := 1; { skip F/P identifier }
      LoadVarType := false;
      ParStr := elx_Globals.UserParamList^[Number]^;
      CallList := '';
      {-- ParStr is a string that contains all the required parameters -----}
      {-- for this procedure/function. The format is as follows ------------}
      {-- Len:         Description:                                         }
      {-- ------------------------------------------------------------------}
      {-- 02 chars     Number of parameters specified                       }
      {-- 01 char      (lowercase is copy, uppercase is VAR paramter        }
      {--              2: [i|r|b|c|s|t]:                                    }
      {--                   i = integer                                     }
      {--                   r = real                                        }
      {--                   b = boolean                                     }
      {--                   c = char                                        }
      {--                   s = string                                      }
      {--                   t = dynamicarray                                }
      {-- Now lets first extract the number of parameters we expect --------}
      NrParams := Length(ParStr) - 1; { first char is wether its a f or p }

      if NrParams > 0 then
        begin
          {-- First make sure we have an opening "(" -----------------------}
          if (GetCurSymbol(elx_Globals) = sym_lParent) then
            InSymbol(elx_Globals)
              else Error(elx_Globals, err_LParMiss);

          {-- Now process all parameters -----------------------------------}
          REPEAT
            {-- if we want a var-parameter, make sure we get one -----------}
            if UpCase(ParStr[CurParam + 1]) = ParStr[CurParam + 1] then
              begin
                RecAdr := 0;
                LoadVarType := false;
                RecLevel := -1;

                if (GetCurSymbol(elx_Globals) <> sym_Ident) then
                  begin
                    Error(elx_Globals, err_VarIdent)          { var. identifier expected }
                  end
                    else begin
                           {-- Lookup the specified identifier -------------}
                           TmpIdent := Loc(elx_Globals.BlockVars^.CurIdent, true);

                           {-- skip procedure ------------------------------}
                           if (elx_Globals.IdentTable^[TmpIdent].Typ = typ_Records) then
                            if UpCase(ParStr[CurParam + 1]) <> 'E' then
                             with elx_Globals, BlockVars^ do
                              begin
                                {-- Get next symbol -------------------------}
                                InSymbol(elx_Globals);
                                Test(fSys + [sym_period], [], err_Syntax);
                                InSymbol(elx_Globals);

                                {-- get the blocktable to this record -------}
                                Counter := BlockTable^[IdentTable^[TmpIdent].Ref].Last;
                                IdentTable^[0].Name := CurIdent;
                                RecAdr := IdentTable^[TmpIdent].Adr;
                                RecLevel := IdentTable^[TmpIdent].Lev;

                                {-- Search for the identifier ---------------}
                                While (IdentTable^[Counter].Name <> CurIdent) do
                                  Counter := IdentTable^[Counter].Link;

                                if Counter = 0 then       { Unkn. identifier }
                                  Error(elx_Globals, err_UnkIdent);

                                TmpIdent := Counter; { IdentTable^[Counter].Adr; }
                              end; { if }

                           {-- we dont allow records/arrays in recs/arrays -}
                           {-- this will give problem with user funcs as ---}
                           {-- not enough information is available to ------}
                           {-- determine the offset. If your planning on ---}
                           {-- fixing this, MAKE SURE YOU TEST IT !!!! -----}
                           if RecAdr > 0 then
                            if (elx_Globals.IdentTable^[TmpIdent].Typ in [typ_Records, typ_Arrays]) then
                             begin
                               Error(elx_Globals, err_NoNestedRec);
                             end; { if }

                           {-- and emit the variable -----------------------}
                           if (TmpIdent <> 0) then
                             begin
                               if (elx_Globals.IdentTable^[TmpIdent].obj <> obj_variable) then
                                 begin
                                   {-- if what we specified isnt a variable-}
                                   {-- complain ----------------------------}
                                   Error(elx_Globals, err_InvVarRef)
                                 end; { if } { if }

                               {-- Get the variable we perhaps want --------}
                               TempType.Typ := elx_Globals.IdentTable^[TmpIdent].Typ;
                               TempType.Ref := elx_Globals.IdentTable^[TmpIdent].Ref;
                               TempType.Temp:= FALSE;

                               {-- is this already a ptr to the real variable?}
                               {-- if so, we only pass the ptr -------------}
                               if elx_Globals.IdentTable^[TmpIdent].Normal then
                                 begin
                                   LoadVarType := true;
                                   TmpFunc := op_LoadAddress
                                 end
                                   else begin
                                          TmpFunc := op_LoadValue;
                                          LoadVarType := false;
                                        end; { if }

                               {-- fix level -------------------------------}
                               if RecLevel < 0 then
                                 RecLevel := elx_Globals.IdentTable^[TmpIdent].Lev;

                               {-- and "load by value" or "load by address -}
                               with elx_Globals do
                                 Emit2(elx_Globals, TmpFunc,
                                       RecLevel,
                                       IdentTable^[TmpIdent].adr + RecAdr);

                               {-- get next token --------------------------}
                               InSymbol(elx_Globals);
                             end; { if not found }
                         end; { else }
              end { var parameter }
                else begin
                       {-- handle the expression ---------------------------}
                       Expression(fSys + [sym_Comma, sym_RParent],  TempType);
                     end; { if }

            {-- Make sure we have the correct type of parameter ------------}
            Case UpCase(ParStr[CurParam + 1]) of
              'I' : begin                                        { Integer }
                      if (TempType.typ <> typ_ints) then
                        Error(elx_Globals, err_MustInt);
                    end; { integer }
              'R' : begin
                      if (TempType.typ > typ_reals) then
                        Error(elx_Globals, err_MustReal);
                    end; { real }
              'E' : begin
                      if (TempType.typ <> typ_records) then
                        Error(elx_Globals, err_NoRecord);
                    end; { records }
              'A' : begin
                      if (TempType.typ <> typ_arrays) then
                        Error(elx_Globals, err_NoArray);
                    end; { arrays }
              'B' : begin
                      if (TempType.typ <> typ_Bools) then
                        Error(elx_Globals, err_BoolExpect);
                    end; { boolean }
              'T' : begin
                      if (TempType.typ <> typ_DynArray) then
                        Error(elx_Globals, err_DynArExpected);
                    end; { stringarray }
              'C',
              'S' : begin
                      {-- if not a var parameter, we accept chars, else force -}
                      {-- strings ---------------------------------------------}
                      case ParStr[CurParam + 1] of
                        'S' : if ParStr[CurParam + 1] = 'S' then
                               if (NOT (TempType.Typ in [typ_Strngs])) then
                                  Error(elx_Globals, err_StrExpected);
                        'C' : if ParStr[CurParam + 1] = 'C' then
                               if (NOT (TempType.Typ in [typ_Chars])) then
                                  Error(elx_Globals, err_ExprType);
                      end; { case }
                    end; { String, Char }
            end; { case }

            Inc(CurParam);

            {-- Make sure enough parameters are defined --------------------}
            if CurParam < NrParams then
              if GetCurSymbol(elx_Globals) <> sym_Comma then
                Error(elx_Globals, err_CommaExpected);

            {-- Skip the comma ---------------------------------------------}
            if GetCurSymbol(elx_Globals) = sym_Comma then
              InSymbol(elx_Globals);                              { Get the next symbol }

            {-- We log which exact parameters were used to call this -------}
            case TempType.Typ of
              typ_NoTyp   : CallList := CallList + 'n';
              typ_Ints    : CallList := CallList + 'i';
              typ_Reals   : CallList := CallList + 'r';
              typ_Bools   : CallList := CallList + 'b';
              typ_Chars   : CallList := CallList + 'c';
              typ_Strngs  : if TempType.Temp then
                              CallList := CallList + 'o' { o = temp }
                               else CallList := CallList + 's';
              typ_Arrays  : CallList := CallList + 'a' + RecStr(TmpIdent, 4) + IntStr(Byte(LoadVarType), 1);
              typ_Records : CallList := CallList + 'e' + RecStr(TmpIdent, 4) + IntStr(Byte(LoadVarType), 1);
              typ_DynArray: CallList := CallList + 't' + RecStr(TmpIdent, 4) + IntStr(Byte(LoadVarType), 1);
            end; { case }
          UNTIL (CurParam > NrParams) OR (elx_Globals.AbortCompile);

          {-- And make sure we have a closing ")" as well ------------------}
          if GetCurSymbol(elx_Globals) <> sym_rParent then
            InSymbol(elx_Globals);

          if (GetCurSymbol(elx_Globals) = sym_rParent) then
            InSymbol(elx_Globals)
              else Error(elx_Globals, err_RParMiss);
        end; { if }

       {-- Add a new item to the call array --------------------------------}
       with elx_Globals do
         begin
           Inc(CurCallList);
           if CurCallList > MaxCallList then
             begin
               Error(elx_Globals, err_CallTabExceed);
             end { table overflow }
               else begin
                      GetMem(UserCallList^[CurCallList - 1], Length(CallList) + 1);
                      UserCallList^[CurCallList - 1]^ := CallList;
                    end; { else }
         end; { with }

       {-- Run the user function -------------------------------------------}
       Emit2(elx_Globals, op_RunUserFunc, Number, elx_Globals.CurCallList - 1);

       {-- Set the return function -----------------------------------------}
       RetType.Typ := elx_Globals.IdentTable^[IdentPtr].typ;
    end; { proc. UsersProc }



    procedure Expression(fSys: SymbolSet; var X: ItemType);
    var    IsSym  : tSymbol;
           Counter: Integer;
           TempTyp: ItemType;

      procedure SimpleExpression(fsys:SymbolSet; var x:itemtype);
      var IsSym  : tSymbol;
          TempTyp: ItemType;

        procedure Term(fSys: SymbolSet; var x:itemtype);
        var IsSym  : tSymbol;
            TempTyp: Itemtype;

          procedure Factor(fSys: SymbolSet; var X: ItemType);
          var Func   : Integer;
              Counter: Integer;
              FakeVar: Boolean;

            procedure StandFct(Number: Integer);
            var TypSet: TypeSet;


            begin                         { Standard function no. "number "}
              if (Number in [19, 39, 47, 48, 49]) then
                Emit1(elx_Globals, 8, Number)   { Maxavail, keypressed, readkey, WhereX }
                 else begin                                      { ,WhereY }
           { Random }   if (Number = 40) AND (GetCurSymbol(elx_Globals) <> sym_Lparent) then
                          begin
                             Emit1(elx_Globals, op_RunFunction, Number + 1);
                             X.Typ := typ_Reals;
                          end { if }
                            else begin
                                   if (GetCurSymbol(elx_Globals) = sym_lparent) then
                                     Insymbol(elx_Globals)
                                       else Error(elx_Globals, err_LParMiss);


                                   {-- and handle other cases -----------------}
                                   if ((Number < 17) OR (number > 19)) then
                                     begin
                                       Expression(fSys + [sym_comma,sym_rparent], X);

                                       Case Number of
                           { abs, sqr }   0, 2: begin
                                                  TypSet := [typ_ints, typ_reals];
                                                  elx_Globals.IdentTable^[Counter].Typ := x.typ;

                                                  if x.typ = typ_reals then
                                                    Inc(Number);
                                                end; { abs, sqr }

                           { odd, chr }   4, 5 : TypSet := [typ_ints];
                                { ord }      6 : TypSet := [typ_ints, typ_bools, typ_chars];
                         { succ, pred }   7, 8 : begin
                                                   TypSet := [typ_ints,typ_bools,typ_chars];
                                                   elx_Globals.IdentTable^[Counter].typ := x.typ
                                                 end; { succ, pred }
                       { round, trunc }  9, 10,
                           { sin, cos }  11,12,
                            { exp, ln }  13,14,
                       { sqrt, arctan }  15,16 : begin
                                                   TypSet := [typ_ints, typ_reals];
                                                   if (x.typ = typ_ints) then
                                                     Emit1(elx_Globals, op_LoadFloat, 0);
                                                 end; { .. }
                             { length }     20 : begin
                                                   TypSet := [typ_strngs, typ_chars];

                                                   if x.temp then
                                                     Number := Number + 1;
                                                   if (x.typ = typ_chars) then
                                                     Number := Number + 2
                                                 end; { length }
                               { copy }     23 : begin
                                                   TypSet := [typ_strngs,typ_chars];
                                                   if (x.typ = typ_chars) then
                                                     Number := Number + 2
                                                       else begin
                                                              if X.temp then Number := Number + 1;
                                                            end; { else }
                                                   Test([sym_comma], [sym_comma,sym_rparent] + fSys, err_CommaExpected);

                                                   if (GetCurSymbol(elx_Globals) = sym_comma) then
                                                     begin
                                                       InSymbol(elx_Globals);
                                                       Expression(fSys + [sym_comma,sym_rparent], TempTyp);

                                                       if (TempTyp.typ <> typ_ints) then
                                                         if (TempTyp.typ <> typ_notyp) then
                                                           Error(elx_Globals, err_IntExpected);

                                                       test([sym_comma,sym_rparent], fsys, err_Syntax);

                                                       if (GetCurSymbol(elx_Globals) = sym_comma) then
                                                         begin
                                                           InSymbol(elx_Globals);
                                                           Expression(fSys + [sym_rparent], TempTyp);

                                                           if (TempTyp.typ <> typ_ints) then
                                                             if (TempTyp.typ <> typ_notyp) then
                                                               Error(elx_Globals, err_IntExpected);
                                                         end
                                                           else Emit1(elx_Globals, op_Literal, MaxInteger);
                                                     end; { if }
                                                 end; { copy }

                                { pos }     26 : begin
                                                   TypSet := [typ_strngs, typ_chars];

                                                   if (x.typ = typ_chars) then
                                                     Number := Number + 2
                                                       else begin
                                                              if x.temp then Number := Number + 1;
                                                            end; { else }

                                                   test([sym_comma], [sym_comma]+fsys, err_CommaExpected);
                                                   if (GetCurSymbol(elx_Globals) = sym_comma) then
                                                     begin
                                                       InSymbol(elx_Globals);
                                                       Expression(fSys + [sym_rparent], TempTyp);

                                                       if (TempTyp.typ <> typ_strngs) then
                                                         begin
                                                           if (TempTyp.typ <> typ_notyp) then
                                                             Error(elx_Globals, err_StrExpected);
                                                         end
                                                          else begin
                                                                 if TempTyp.temp then Number := Number + 4;
                                                               end; { else }
                                                     end; { if }
                                                 end; { pos }

                            { expand_vars } 76 : begin
                                                   TypSet := [typ_strngs, typ_chars];
                                                   Emit1(elx_Globals, op_Literal, Level);
                                                 end; { if }

                                { str }     33 : begin
                                                   TypSet := [typ_ints,typ_reals];

                                                   if x.typ = typ_reals then
                                                     Number := Number + 1;
                                                 end; { str }

                                { val }     35,
                               { rval }     37 : begin
                                                   TypSet := [typ_strngs];

                                                   if x.temp then
                                                     Number := Number + 1;
                                                 end; { val, rval }

                             { random }     40 : TypSet := [typ_ints];
                             { upcase }     42 : TypSet := [typ_chars];
                             { fileassign } 54,        { fileassign(c, i) }
                                            55,        { fileassign(s, i) }
                                            56 : begin { fileassign('blah', i) }
                                                   TypSet := [typ_Chars, typ_Strngs];

                                                   if (x.typ = typ_chars) then
                                                     Number := Number + 2
                                                       else begin
                                                              if X.temp then
                                                                Number := Number + 1;
                                                            end; { else }
                                                   Test([sym_comma], [sym_comma,sym_rparent] + fSys, err_CommaExpected);

                                                   if (GetCurSymbol(elx_Globals) = sym_comma) then
                                                     begin
                                                       InSymbol(elx_Globals);
                                                       Expression(fSys + [sym_comma,sym_rparent], TempTyp);

                                                       if (TempTyp.typ <> typ_ints) then
                                                         if (TempTyp.typ <> typ_notyp) then
                                                           Error(elx_Globals, err_IntExpected);

                                                       test([sym_comma,sym_rparent], fsys, err_Syntax);
                                                     end; { if }

                                                   Test([sym_comma], [sym_comma,sym_rparent] + fSys, err_CommaExpected);
                                                   if (GetCurSymbol(elx_Globals) = sym_comma) then
                                                     begin
                                                       InSymbol(elx_Globals);
                                                       Expression(fSys + [sym_comma,sym_rparent], TempTyp);

                                                       if (TempTyp.typ <> typ_ints) then
                                                         if (TempTyp.typ <> typ_notyp) then
                                                           Error(elx_Globals, err_IntExpected);

                                                       test([sym_comma,sym_rparent], fsys, err_Syntax);
                                                     end;

                                                 end; { if }

                             { filegeterr } 57 : TypSet := [typ_ints];
                             { filegetsiz } 61 : TypSet := [typ_ints];
                             { filegetpos } 62 : TypSet := [typ_ints];
                             { fileexist }  64 : TypSet := [typ_ints];
                                       end; { case }

                                       if (x.Typ in TypSet) then
                                         begin
                                           Emit1(elx_Globals, op_RunFunction, Number)
                                         end
                                          else begin
                                                 if (x.Typ <> typ_NoTyp) then
                                                   Error(elx_Globals, err_FuncMatch)
                                               end; { else }
                                     end             { Number is in 17, 18 }
                                       else begin
                                              if GetCurSymbol(elx_Globals) <> sym_ident then
                                                Error(elx_Globals, err_VarIdent)
                                                  else begin
                                                         if elx_Globals.BlockVars^.CurIdent <> 'input     ' then
                                                           Error(elx_Globals, err_UnkIdent)
                                                            else InSymbol(elx_Globals);

                                                         Emit1(elx_Globals, op_RunFunction, Number);
                                                       end; { else }
                                            end; { else }

                                   X.typ  := elx_Globals.IdentTable^[Counter].typ;
                                   X.Temp := true;
                                   if (GetCurSymbol(elx_Globals) = sym_rParent) then
                                     InSymbol(elx_Globals)
                                       else Error(elx_Globals, err_RParMiss);
                                 end; { else }
                      end; { else }
            end; { proc. StandFct }

          begin  { factor }
            x.Typ := typ_NoTyp;
            x.Ref := 00;
            Test(elx_Globals.BlockVars^.facBegSys, fSys, err_FacExpected);

            While (GetCurSymbol(elx_Globals) in elx_Globals.BlockVars^.facBegSys) AND (NOT elx_Globals.AbortCompile) do
             with elx_Globals do
              begin
                Case GetCurSymbol(elx_Globals) of
                   sym_ident: begin
                                Counter := loc(BlockVars^.CurIdent, true);
                                InSymbol(elx_Globals);

                                With IdentTable^[Counter] do
                                  Case obj of
                                    obj_Constant: begin
                                                    x.Typ  := typ;
                                                    x.Ref  := 0;
                                                    x.Temp := false;
                                                    if (x.typ = typ_reals) then emit1(elx_Globals, op_LoadReal, Adr)
                                                      else emit1(elx_Globals, op_Literal, Adr);
                                                  end; { obj_Constant }

                                    obj_variable: begin
                                                    x.Typ  := Typ;
                                                    x.Ref  := ref;
                                                    x.Temp := false;

                                                    if (GetCurSymbol(elx_Globals) in
                                                                    [sym_lbrack, sym_lparent, sym_period]) then
                                                      begin
                                                        if Normal then
                                                          Func := op_LoadAddress
                                                            else Func := op_LoadValue;

                                                        if (x.Typ = typ_strngs) then
                                                          begin
                                                            Emit2(elx_Globals, Func + 1, Lev, Adr);
                                                            Selector(fSys, X, -1, FakeVar);
                                                          end { if }
                                                            else begin
                                                                   Emit2(elx_Globals, Func, Lev, Adr);
                                                                   Selector(fSys, X, Counter, FakeVar);

                                                                   if (x.typ in StandardTypes) then
                                                                     if NOT FakeVar then
                                                                     begin
                                                                       Emit(elx_Globals, op_CopyArray);
                                                                     end; { if }
                                                                 end; { else }
                                                      end { if }
                                                        else begin
                                                               if (x.typ in StandardTypes) then
                                                                 begin
                                                                   if Normal then Func := op_LoadValue
                                                                     else Func := op_LoadIndirect
                                                                 end
                                                                   else begin
                                                                          if normal then Func := 0
                                                                            else Func := 1;
                                                                        end; { else }

                                                               Emit2(elx_Globals, Func, Lev, Adr);
                                                             end; { else }
                                                  end; { obj_variable }
                                  obj_type,
                                  obj_procedure : begin
                                                    Error(elx_Globals, err_VarConst);
                                                  end; { obj_type, obj_procedure }

                                   obj_function : begin
                                                    x.Typ := typ;
                                                    x.Temp := true;

                                                    if (Lev <> 0) AND (Lev <> 1) then
                                                      Call(fSys, Counter)
                                                        else begin
                                                               Case Lev of
                                                                 0 : StandFct(Adr);
                                                                 1 : UsersProc(Adr, X, Counter);
                                                               end; { case }
                                                             end; { else }
                                                  end; { obj_Function }
                                  end; { case obj. }
                              end; { sym_ident }

                 sym_RealCon: begin
                                x.Typ := typ_reals;
                                x.Ref := 0;

                                AddReal(elx_Globals, BlockVars^.inSym_Real);
                                Emit1(elx_Globals, op_LoadReal, RealCount1);
                                InSymbol(elx_Globals);
                              end; { sym_realcon }

                 sym_CharCon: begin
                                x.Typ := typ_chars;
                                x.Ref := 0;
                                x.Temp := false;

                                Emit1(elx_Globals, op_Literal, BlockVars^.InSym_Integer);
                                InSymbol(elx_Globals);
                              end; { sym_CharCon }

                  sym_IntCon: begin
                                x.Typ := typ_ints;
                                x.Ref := 0;
                                Emit1(elx_Globals, op_Literal, BlockVars^.InSym_Integer);
                                InSymbol(elx_Globals);
                              end; { sym_intCon }

               sym_StringCon: begin
                                x.Typ := typ_strngs;
                                x.Ref := 0;
                                x.temp := false;

                                Emit1(elx_Globals, op_Literal, BlockVars^.StringPtr);
                                InSymbol(elx_Globals);
                              end; { sym_StringCon }

                 sym_lparent: begin
                                InSymbol(elx_Globals);
                                Expression(fSys + [sym_rparent], x);

                                if (GetCurSymbol(elx_Globals) = sym_rparent) then
                                  InSymbol(elx_Globals)
                                    else Error(elx_Globals, err_RParMiss);
                              end; { sym_lParent }

                     sym_not: begin
                                InSymbol(elx_Globals);
                                Factor(fSys, X);

                                if (x.typ = typ_bools) then
                                  begin
                                    emit(elx_Globals, op_MakeBoolNot)
                                  end
                                    else begin
                                           if (x.typ <> typ_notyp) then
                                             Error(elx_Globals, err_NoBoolean)
                                         end; { else }
                              end; { sym_NOT }
                end; { case }

                Test(fSys, BlockVars^.FacBegSys, err_Syntax);
              end; { while }
          end; { proc. Factor }


        begin { term }
          Factor(fSys + [sym_times, sym_rdiv, sym_idiv, sym_imod, sym_and], x);

          While (GetCurSymbol(elx_Globals) in [sym_times,sym_rdiv,sym_idiv,sym_imod,sym_and])
                 AND (NOT elx_Globals.AbortCompile) do
            begin
              IsSym := GetCurSymbol(elx_Globals);
              InSymbol(elx_Globals);

              Factor(fSys + [sym_times,sym_rdiv,sym_idiv,sym_imod,sym_and], Temptyp);

              if (IsSym = sym_times) then
                begin
                  x.Typ := ResultType(x.Typ, TempTyp.Typ);

                  Case x.Typ of
                    typ_notyp : ;
                    typ_ints  : emit(elx_Globals, op_MultiplyInt);
                    typ_reals : emit(elx_Globals, op_MultiplyReal);
                  end; { ccase }
                end
                  else begin
                         if (IsSym = sym_rdiv) then
                           begin
                             if (x.typ = typ_ints) then
                               begin
                                 Emit1(elx_Globals, op_LoadFloat, 1);
                                 x.Typ := typ_reals;
                               end; { if }

                             if (TempTyp.typ = typ_ints) then
                               begin
                                 Emit1(elx_Globals, op_LoadFloat, 0);
                                 TempTyp.typ := typ_reals
                               end; { if }

                             if (x.typ = typ_reals) AND (TempTyp.typ = typ_reals) then
                               begin
                                 Emit(elx_Globals, op_DivideReal)
                               end
                                 else begin
                                        if (x.typ <> typ_notyp) AND
                                            (TempTyp.typ <> typ_notyp) then
                                              Error(elx_Globals, err_NoArithm);

                                        x.Typ := typ_notyp;
                                      end; { else }
                           end { if }
                             else begin
                                    if (IsSym = sym_and) then
                                      begin
                                        if (x.Typ = typ_bools) AND
                                            (TempTyp.typ = typ_bools) then
                                              begin
                                                 Emit(elx_Globals, op_MakeBoolAnd)
                                              end
                                                else begin
                                                       if (x.typ <> typ_notyp) AND
                                                           (TempTyp.typ <> typ_notyp) then
                                                             Error(elx_Globals, err_NoBoolean);
                                                       x.Typ := typ_noTyp;
                                                     end; { else }
                                      end { if }
                                        else begin   { op in [idiv, imod] }
                                                if (x.Typ = typ_ints) AND
                                                    (TempTyp.typ = typ_ints) then
                                                      begin
                                                        if (IsSym = sym_idiv) then
                                                          Emit(elx_Globals, op_DivideInt)
                                                            else emit(elx_Globals, op_ModInt);
                                                      end { if }
                                                       else begin
                                                              if (x.typ <> typ_notyp) AND
                                                                  (TempTyp.typ <> typ_notyp) then
                                                                    Error(elx_Globals, err_IntExpected);
                                                              x.Typ := typ_notyp
                                                            end; { else }
                                             end; { else }
                                  end; { else }
                       end; { else }
            end; { while }
        end; { proc. term }


      begin { simpleexpression }
        if (GetCurSymbol(elx_Globals) in [sym_plus, sym_minus]) then
          begin
            isSym := GetCurSymbol(elx_Globals);
            InSymbol(elx_Globals);

            term(fsys+[sym_plus,sym_minus], x);

            if (x.typ > typ_reals) then Error(elx_Globals, err_NoArithm)
              else begin
                     if (IsSym = sym_minus) then
                       begin
                         if (x.typ = typ_reals) then
                           Emit(elx_Globals, op_MakeRealNegative)
                            else emit(elx_Globals, op_MakeIntNegative)
                       end; { if }
                   end; { else }
          end
            else Term(fsys + [sym_plus,sym_minus,sym_or], X);

        While (GetCurSymbol(elx_Globals) in [sym_plus,sym_minus,sym_or]) AND (NOT elx_Globals.AbortCompile) do
          begin
            IsSym := GetCurSymbol(elx_Globals);
            InSymbol(elx_Globals);

            Term(fSys + [sym_plus,sym_minus,sym_or], TempTyp);

            if (IsSym = sym_or) then
              begin
                if (x.typ = typ_bools) AND (TempTyp.typ = typ_bools) then
                  Emit(elx_Globals, op_BoolOr)
                    else begin
                           if (x.typ <> typ_notyp) AND (TempTyp.typ<>typ_notyp) then
                             Error(elx_Globals, err_NoBoolean);

                           x.typ := typ_notyp
                         end; { else }
              end
                else if (x.typ = typ_chars) OR (x.typ = typ_strngs) then
                       begin
                         if NOT ((TempTyp.typ = typ_chars) OR (TempTyp.typ = typ_strngs)) then
                           begin
                             Error(elx_Globals, err_StrExpected);
                             x.typ := typ_notyp;
                           end
                             else begin
                                    if (x.typ = typ_chars) then
                                      Counter := 0
                                        else Counter := 1;

                                    if (TempTyp.typ = typ_strngs) then
                                      Inc(Counter, 2);

                                    if (x.temp) then
                                      Inc(Counter, 4);

                                    if (TempTyp.temp) then
                                      Inc(Counter, 8);

                                    Emit1(elx_Globals, op_Concatenate, Counter);
                                    x.typ := typ_strngs;
                                    x.Temp := TRUE;
                                  end; { else }
                       end { if }
                         else begin
                                x.Typ := ResultType(x.typ, TempTyp.typ);

                                Case X.Typ of
                                  typ_notyp: ;
                                  typ_ints : if (IsSym = sym_plus) then
                                               Emit(elx_Globals, op_AddInt)
                                                 else emit(elx_Globals, op_MinInt);
                                  typ_reals: if (IsSym = sym_plus) then
                                               Emit(elx_Globals, op_AddReal)
                                                 else Emit(elx_Globals, op_MinReal);
                                end; { case }
                              end; { else }
          end; { while }
      end; { proc. SimpleExpression }

    begin { expression }
      SimpleExpression(fSys + [sym_eql, sym_neq, sym_lss, sym_leq, sym_gtr,
                               sym_geq], X);

      if (GetCurSymbol(elx_Globals) in [sym_eql,sym_neq,sym_lss,sym_leq,sym_gtr,sym_geq]) then
        begin
          IsSym := GetCurSymbol(elx_Globals);
          InSymbol(elx_Globals);

          SimpleExpression(fSys, TempTyp);
          if (x.typ in [typ_notyp,typ_ints,typ_bools,typ_chars]) AND
              (x.typ = TempTyp.typ) then
              begin
                Case IsSym of
                  sym_eql: Emit(elx_Globals, op_IntEql);
                  sym_neq: Emit(elx_Globals, op_IntNeq);
                  sym_lss: Emit(elx_Globals, op_IntLss);
                  sym_leq: Emit(elx_Globals, op_IntLeq);
                  sym_gtr: Emit(elx_Globals, op_IntGtr);
                  sym_geq: Emit(elx_Globals, op_IntGeq);
                end; { case }
             end { if }
               else begin
                      if (x.typ = typ_ints) then
                        begin
                          x.Typ := typ_reals;
                          Emit1(elx_Globals, op_LoadFloat, 1)
                        end
                          else begin
                                 if (TempTyp.typ = typ_ints) then
                                   begin
                                     TempTyp.typ := typ_reals;
                                     Emit1(elx_Globals, op_LoadFloat, 0);
                                   end; { if }
                                end; { else }

                      if (x.typ = typ_reals) AND (TempTyp.typ = typ_reals) then
                        begin
                          Case IsSym of
                            sym_eql: Emit(elx_Globals, op_RealEql);
                            sym_neq: Emit(elx_Globals, op_RealNeq);
                            sym_lss: Emit(elx_Globals, op_RealLss);
                            sym_leq: Emit(elx_Globals, op_RealLeq);
                            sym_gtr: Emit(elx_Globals, op_RealGtr);
                            sym_geq: Emit(elx_Globals, op_RealGeq);
                          end; { case }
                        end { if }
                          else begin
                                 if (x.typ in [typ_chars,typ_strngs]) AND
                                     (TempTyp.typ in [typ_chars,typ_strngs]) then
                                       begin
                                         if (x.typ=typ_strngs) then
                                            Counter := 1
                                              else Counter := 0;

                                         if (TempTyp.typ=typ_strngs) then
                                            Counter := Counter + 2;

                                         if (x.temp) then
                                           Counter := Counter + 4;

                                         if (TempTyp.temp) then
                                           Counter := Counter + 8;

                                         if (IsSym in [sym_eql,sym_leq,sym_geq]) then
                                           Counter := Counter + 16;

                                         if (IsSym in [sym_neq,sym_gtr,sym_geq]) then
                                            Counter := Counter + 32;

                                         if (IsSym in [sym_neq,sym_lss,sym_leq]) then
                                            Counter := Counter + 64;

                                         Emit1(elx_Globals, op_StringRel, Counter);
                                       end
                                         else Error(elx_Globals, err_ExprType);
                               end; { else }
                    end; { else }
           x.Typ := typ_bools
        end; { if }
    end; { proc. Expression }

    procedure Assignment(i: integer; IsLev, IsAdr: integer);
    var RunFunc     : Integer;
        CharIndex   : Longint;
        FakeVar     : Boolean;
    var    x,y: itemtype;
    begin              { IdentTable^[i].obj in [vvariable,funktion] }
      x.Typ := elx_Globals.IdentTable^[i].Typ;
      x.Ref := elx_Globals.IdentTable^[i].Ref;
      CharIndex := -1;

      if elx_Globals.IdentTable^[i].Normal then RunFunc := op_LoadAddress
        else RunFunc := op_LoadValue;

      Emit2(elx_Globals, RunFunc, IsLev, IsAdr);

      {-- If its a string then do not allow a selector ----------------------}
      if (GetCurSymbol(elx_Globals) in [sym_lbrack, sym_lparent, sym_period]) then
        begin
          if (x.typ <> typ_strngs) then
             begin
               Selector([sym_becomes, sym_eql] + fSys, x, i, FakeVar);
             end
               else begin
                      {-- Get the next symbol -----------------------------}
                      InSymbol(elx_Globals);

                      {-- Make sure a number was specified ----------------}
                      With elx_Globals, BlockVars^ do
                       if (NOT (GetCurSymbol(elx_Globals) in [sym_IntCon])) OR
                           ((inSym_Integer < 0) OR (inSym_Integer > (1024*65))) then
                             Error(elx_Globals, err_InvStrIdx);

                      {-- save the index ----------------------------------}
                      CharIndex := elx_Globals.BlockVars^.inSym_Integer;

                      {-- and skip the closing bracket --------------------}
                      InSymbol(elx_Globals);
                      if (NOT (GetCurSymbol(elx_Globals) in [sym_rBrack])) then
                        Error(elx_Globals, err_MissRBrk);

                      {-- and make sure "becomes" is on its way -----------}
                      InSymbol(elx_Globals);
                    end; { else }
        end; { if }

      {-- Make sure the "becomes" character is there ------------------------}
      if (GetCurSymbol(elx_Globals) = sym_becomes) then
        InSymbol(elx_Globals)
          else begin
                 Error(elx_Globals, err_AsExpected);
                 if GetCurSymbol(elx_Globals) = sym_eql then InSymbol(elx_Globals)
               end; { else }

      {-- The actual assignment ---------------------------------------------}
      Expression(fSys, Y);
      if x.Typ = y.Typ then
        begin
          if (x.typ in elx_Globals.StandardTypes) then
           begin
             if (x.typ = typ_strngs) then
               begin
                 if (y.temp) then
                   begin
                     Emit(elx_Globals, op_StringTemp);
                   end { if }
                     else begin
                            Emit(elx_Globals, op_StrAndStr);
                          end; { else }
               end { if }
                 else begin
                        Emit(elx_Globals, op_Store)
                      end; { else }
           end
             else begin
                    if (x.ref <> y.ref) then
                      begin
                        Error(elx_Globals, err_InvAssign)
                      end { if }
                        else begin
                               if (x.typ = typ_arrays) then
                                 begin
                                   with elx_Globals do
                                     Emit1(elx_Globals, op_CopyBlock, ArrayTable^[x.ref]^.ArrayInf.TotalSize)
                                 end { if }
                                   else begin
                                          with elx_Globals do
                                            Emit1(elx_Globals, op_CopyBlock, BlockTable^[x.ref].vSize)
                                        end { else }
                             end; { else }
                  end; { else }
        end
          {-- if assignment and assignee dont match (eg: R := I;) ----------}
          else begin
                 if (x.Typ = typ_reals) AND (Y.typ = typ_ints) then
                     begin
                       Emit1(elx_Globals, op_LoadFloat, 0);
                       Emit(elx_Globals, op_Store);
                     end else
          { +----------------^ }
           begin
             if (x.typ = typ_chars) AND (y.typ=typ_strngs)  then
              with elx_Globals do
               begin
                  if (y.temp) then
                    RunFunc := 8
                      else RunFunc := 0;

                  Emit1(elx_Globals, op_CharAndString, RunFunc);
               end { if }
                 else begin
                        if (x.Typ = typ_Strngs) AND (y.typ = typ_chars) then
                          begin
                            Emit1(elx_Globals, op_StringAndChar, CharIndex)
                          end
                            else begin
                                   if (x.typ = typ_strngs) AND (y.typ = typ_arrays) then
                                    with elx_GLobals do
                                     begin
                                       if ArrayTable^[Y.Ref]^.ArrayInf.ElementType = typ_chars then
                                         begin
                                           Emit1(elx_Globals, op_ArToString, Arraytable^[y.ref]^.ArrayInf.Totalsize);
                                           Emit(elx_Globals, op_StringTemp)
                                         end; { if }
                                     end { if }
                                        else begin
                                               if (x.typ = typ_arrays) AND (y.typ = typ_strngs) then
                                                with elx_Globals do
                                                 begin
                                                   if ArrayTable^[X.Ref]^.ArrayInf.ElementType = typ_chars then
                                                     begin
                                                       if Y.Temp then
                                                         begin
                                                           Emit1(elx_Globals, op_CharArAndStrTmp,
                                                                ArrayTable^[X.Ref]^.ArrayInf.TotalSize)
                                                         end
                                                           else begin
                                                                  Emit1(elx_Globals, op_CharArAndStr,
                                                                        ArrayTable^[X.Ref]^.ArrayInf.TotalSize);
                                                                end; { else }
                                                     end; { if }
                                                 end { if }
                                                  else begin
                                                         if (X.Typ <> typ_notyp) AND (Y.typ <> typ_notyp) then
                                                           Error(elx_Globals, err_InvAssign);
                                                       end; { else }
                                             end; { else }
                                 end; { else }
                      end; { else }
           end; { else }
        end; { else }
    end; { proc. Assignment }



    procedure CompoundStatement;
    begin
      InSymbol(elx_Globals);
      Statement([sym_SemiColon, sym_End] + fSys);

      While (GetCurSymbol(elx_Globals) in [sym_Semicolon] + elx_Globals.BlockVars^.Statbegsys) AND
       (NOT elx_Globals.AbortCompile) do
       with elx_Globals do
        begin
          if (GetCurSymbol(elx_Globals) = sym_Semicolon) then InSymbol(elx_Globals)
            else Error(elx_Globals, err_MissSemi);

          Statement([sym_Semicolon,sym_end] + fSys);
        end; { while }

      if (GetCurSymbol(elx_Globals) = sym_end) then
        InSymbol(elx_Globals)
          else Error(elx_Globals, err_EndExpected);
    end; { proc. CompoundStatement }


    procedure IfStatement;
    var IsTyp     : ItemType;
        SaveCode1 : Integer;
        SaveCode2 : Integer;
    begin
      InSymbol(elx_Globals);
      Expression(fSys + [sym_then, sym_do], IsTyp);

      {-- Make sure this returns a boolean ---------------------------------}
      if (NOT (IsTyp.Typ in [typ_Bools, typ_NoTyp])) then Error(elx_Globals, err_BoolExpect);

      {-- Save our current code position -----------------------------------}
      {-- we set the actual position later on, thats why we save it here ---}
      SaveCode1 := elx_Globals.CurCode;
      Emit(elx_Globals, op_ConditionalJmp);                         { jmpc }

      {-- Make sure the next statement is THEN -----------------------------}
      if (GetCurSymbol(elx_Globals) = sym_then) then InSymbol(elx_Globals)
        else begin
               Error(elx_Globals, err_ThenExpected);
               if GetCurSymbol(elx_Globals) = sym_do then InSymbol(elx_Globals);
             end; { else }

      {-- Process this block of code ---------------------------------------}
      Statement(fSys + [sym_else]);

      {-- If else ----------------------------------------------------------}
      if (GetCurSymbol(elx_Globals) = sym_else) then
       with elx_Globals do
        begin
          InSymbol(elx_Globals);
          SaveCode2 := elx_Globals.CurCode;

          Emit(elx_Globals, op_JumpTo);

          {-- Set the actual positions to jump to --------------------------}
          ProgCode[SaveCode1].y := CurCode;

          {-- and set the "else" position to jump to -----------------------}
          Statement(fSys);
          ProgCode[SaveCode2].y := CurCode
        end
           else with elx_Globals do
                begin
                  {-- Set the actual position to jump to -------------------}
                  ProgCode[SaveCode1].y := CurCode
                end; { else }
    end; { proc. IfStatement }


    procedure CaseStatement;
    var CodeCount1: Integer;
        Counter   : Integer;
        LabelCount: Integer;
        CodeCount : Integer;
        IsTyp     : ItemType;
        HasElse   : Boolean;
        CaseTab   : Array[1..MaxCases] of
                      packed record
                        Val : IndexType;                          { Value }
                        LC  : indextype;                  { Index to code }
                      end; { record }
        ExitTab   : Array[1..MaxCases] of Integer;


      procedure CaseLabel(var IsCount: Integer);
      var IsLabel: ConvRec;
          Counter: Integer;
      begin
        Constant(fSys + [sym_Comma, sym_Colon], IsLabel);

        if IsLabel.TP <> IsTyp.Typ then Error(elx_Globals, err_TypeCase)
          else begin
                 if (IsCount = MaxCases) then FatalError(elx_Globals, fatal_code)
                   else begin
                          Inc(IsCount);
                          Counter := 0;

                          CaseTab[IsCount].val := IsLabel.i;
                          CaseTab[IsCount].Lc  := elx_Globals.CurCode;

                          repeat
                            Inc(Counter);
                          until (Casetab[Counter].Val = IsLabel.i) OR (elx_Globals.AbortCompile);

                          if (Counter < IsCount) then
                            Error(elx_Globals, err_DupIdent);               { multiple definition }
                        end; { else }
               end; { else }
      end; { proc. CaseLabel }

      procedure OneCase(var IsCode, IsCount: Integer);
      begin
        if (GetCurSymbol(elx_Globals) in elx_Globals.BlockVars^.ConstBegSys) then
         with elx_Globals do
          begin
            CaseLabel(IsCount);

            While (GetCurSymbol(elx_Globals) = sym_comma) AND (NOT AbortCompile) do
              begin
                InSymbol(elx_Globals);
                CaseLabel(IsCount);
              end; { while }

            if (GetCurSymbol(elx_Globals) = sym_colon) then
              InSymbol(elx_Globals)
                else Error(elx_Globals, err_CollMiss);

            Statement([sym_Semicolon, sym_end] + fSys);
            Inc(IsCode);
            ExitTab[IsCode] := CurCode;
            Emit(elx_Globals, op_JumpTo);
          end; { if }
      end; { proc. Onecase }

    begin
      if elx_Globals.AbortCompile then EXIT;

      InSymbol(elx_Globals);
      LabelCount := 0;
      CodeCount := 0;
      HasElse := FALSE;

      Expression(fSys + [sym_of, sym_comma, sym_colon], IsTyp);
      if (NOT (IsTyp.typ in [typ_ints,typ_bools,typ_chars,typ_notyp])) then
         Error(elx_Globals, err_NoCaseType);

      {-- Save the current code position so we can set the actual jump -----}
      {-- position later in the program ------------------------------------}
      CodeCount1 := elx_Globals.CurCode;
      Emit(elx_Globals, op_Switch);  { jmpx }

      if (GetCurSymbol(elx_Globals) = sym_of) then
        InSymbol(elx_Globals)
          else Error(elx_Globals, err_MissOf);

      {-- Process the first case statement  ---------------------------------}
      OneCase(CodeCount, LabelCount);

      while (GetCurSymbol(elx_Globals) = sym_Semicolon) AND (NOT elx_Globals.AbortCompile) do
        begin
          InSymbol(elx_Globals);
          OneCase(CodeCount, LabelCount);
        end; { while }

      {-- Process an else statement if necessary ---------------------------}
      if (GetCurSymbol(elx_Globals) = sym_Else) then                                { else }
        begin
          InSymbol(elx_Globals);                                    { Skip "else" block }

          {-- We found an else statement -----------------------------------}
          HasElse := TRUE;

          {-- Set the case tabs --------------------------------------------}
          Inc(LabelCount);
          CaseTab[LabelCount].val := -1;
          CaseTab[LabelCount].Lc  := elx_Globals.CurCode;

          {-- process the else code ----------------------------------------}
          Statement([sym_Semicolon, sym_end] + fSys);
          InSymbol(elx_Globals);                       { Read in next symbol we can use }

          {-- Update the code counter --------------------------------------}
          Inc(CodeCount);
          ExitTab[CodeCount] := elx_Globals.CurCode;
          Emit(elx_Globals, op_JumpTo);
        end { else }
          else Error(elx_Globals, err_ElseExpected);

      {-- Set the actual case position to jump to ---------------------------}
      elx_Globals.ProgCode[CodeCount1].Y := elx_Globals.CurCode;
      elx_Globals.ProgCode[CodeCount1].X := Byte(HasElse);{ Default to no else statement }

      {-- Emit all case statements ------------------------------------------}
      for Counter := 1 to LabelCount do
        begin
          Emit1(elx_Globals, 13, CaseTab[Counter].val);
          Emit1(elx_Globals, 13, CaseTab[Counter].lc)
        end; { for }

      Emit1(elx_Globals, op_JumpTo, 0);

      for Counter := 1 to CodeCount do
        elx_Globals.ProgCode[ExitTab[Counter]].y := elx_Globals.CurCode;

      if (GetCurSymbol(elx_Globals) = sym_end) then
        InSymbol(elx_Globals)
          else Error(elx_Globals, err_EndExpected);
    end; { proc. CaseStatement }


    procedure RepeatStatement;
    var Count1  : Integer;
        TempTyp : ItemType;
    begin
      Count1 := elx_Globals.CurCode;
      InSymbol(elx_Globals);

      {-- Get the whole repeat thing ---------------------------------------}
      Statement([sym_Semicolon, sym_until] + fSys);

      While (GetCurSymbol(elx_Globals) in [sym_Semicolon] + elx_Globals.BlockVars^.StatBegSys) AND
            (NOT elx_Globals.AbortCompile) do
        with elx_Globals do
        begin
          if (GetCurSymbol(elx_Globals) = sym_Semicolon) then
            InSymbol(elx_Globals)
              else Error(elx_Globals, err_MissSemi);

          Statement([sym_Semicolon, sym_until] + fSys)
        end; { while }

      {-- Handle the UNTIL statement ---------------------------------------}
      if (GetCurSymbol(elx_Globals) = sym_Until) then
        begin
          InSymbol(elx_Globals);
          Expression(fSys, TempTyp);

          if (NOT (TempTyp.typ in [typ_bools, typ_notyp])) then
            Error(elx_Globals, err_BoolExpect);

          Emit1(elx_Globals, op_ConditionalJmp, Count1)
        end
          else Error(elx_Globals, err_UntilExpected);
    end; { proc. RepeatStatement }


    procedure WhileStatement;
    var TempTyp: ItemType;
        Count1 : Integer;
        Count2 : Integer;
    begin
      InSymbol(elx_Globals);

      {-- Save the current position so we can jump back to it, to actually -}
      {-- create the loop --------------------------------------------------}
      Count1 := elx_Globals.CurCode;

      {-- Make sure its a boolean equation we evaluate ---------------------}
      Expression(fSys + [sym_do], TempTyp);
      if (NOT (TempTyp.Typ in [typ_Bools, typ_noTyp])) then
        Error(elx_Globals, err_BoolExpect);

      {-- Save the current code position in order to actually set the jump -}
      {-- position later on ------------------------------------------------}
      Count2 := elx_Globals.CurCode;
      Emit(elx_Globals, op_ConditionalJmp);

      if (GetCurSymbol(elx_Globals) = sym_do) then
        InSymbol(elx_Globals)
          else Error(elx_Globals, err_DoExpected);

      Statement(fSys);

      {-- Jump back to the original position to keep the loop going --------}
      Emit1(elx_Globals, op_JumpTo, Count1);

      {-- Set the "after if" code to after the while loop so we skip it ----}
      {-- as soon as the "boolean if" doesnt return true anymore -----------}
      elx_Globals.ProgCode[Count2].y := elx_Globals.CurCode
    end; { proc. WhileStatement }


    procedure ForStatement;
    var Conv   : tType;
        Count1 : Integer;
        Count2 : Integer;
        RunFunc: Integer;
        IsTyp  : ItemType;
        Counter: Integer;
    begin
      InSymbol(elx_Globals);

      {-- Get the item we need counting with -------------------------------}
      if (GetCurSymbol(elx_Globals) = sym_ident) then
        begin
          Counter := Loc(elx_Globals.BlockVars^.CurIdent, true);
          InSymbol(elx_Globals);

          if Counter = 0 then Conv := Typ_Ints
            else with elx_Globals do
                  begin
                   if IdentTable^[Counter].obj = obj_variable then
                     begin
                       Conv := IdentTable^[Counter].typ;

                       if IdentTable^[Counter].Normal then
                         RunFunc := op_LoadAddress
                           else RunFunc := op_LoadValue;

                       Emit2(elx_Globals, RunFunc, IdentTable^[Counter].Lev, IdentTable^[Counter].Adr);

                       if (NOT (Conv in [typ_notyp,typ_ints,typ_bools,typ_chars])) then
                         Error(elx_Globals, err_Forvar);
                     end
                       else begin
                              Error(elx_Globals, err_InvVarRef);
                              Conv := typ_ints
                            end; { else }
                 end; { else }
        end { if }
          else Skip([sym_becomes, sym_to, sym_downto, sym_do] + fSys, err_VarIdent);

      {-- Make sure the assigned value matches the count-type ---------------}
      if (GetCurSymbol(elx_Globals) = sym_becomes) then
        begin
          InSymbol(elx_Globals);

          Expression([sym_to, sym_downto, sym_do] + fSys, IsTyp);

          if IsTyp.Typ <> Conv then
            Error(elx_Globals, err_ForMismatch);
        end
          else skip([sym_to,sym_downto,sym_do] + fsys, err_AsExpected);

      {-- TO or DOWNTO ------------------------------------------------------}
      RunFunc := 14;                                      { Default to "TO" }

      if (GetCurSymbol(elx_Globals) in [sym_to, sym_downto]) then
        begin
          if (GetCurSymbol(elx_Globals) = sym_downto) then                  { its "Downto" }
             RunFunc := 16;

          InSymbol(elx_Globals);
          Expression([sym_do] + fSys, IsTyp);

          if IsTyp.typ <> Conv then
            Error(elx_Globals, err_ForMismatch)
        end
          else skip([sym_do] + fsys, err_ToExpected);

      {-- Actually output all code according to the routines ---------------}
      Count1 := elx_Globals.CurCode;      { Save the current code position }
      Emit(elx_Globals, RunFunc);                    { FOR at the beginning }
      if (GetCurSymbol(elx_Globals) = sym_do) then
        InSymbol(elx_Globals)
          else Error(elx_Globals, err_DoExpected);

      {-- Save the actual start of the code --------------------------------}
      {-- to execute in the loop -------------------------------------------}
      Count2 := elx_Globals.CurCode;

      {-- Run the code in the loop -----------------------------------------}
      Statement(fSys);

      {-- Emit a "For2[up|down]" and pass the code segment to jump to ------}
      {-- with it ----------------------------------------------------------}
      Emit1(elx_Globals, RunFunc + 1, Count2);               { Add the end }

      {-- Set the "code-pointer" for the "For1[up|down]" to AFTER the ------}
      {-- the loop. This is necessary to jump out of it --------------------}
      elx_Globals.ProgCode[Count1].Y := elx_Globals.CurCode
    end; { proc. ForStatement }


    procedure StandProc(Number: Integer);
    var    i,f: integer;
           x,y: itemtype;
         TmpIdent: Longint;
         Fakevar : Boolean;
    begin
      Case Number of
         1,
         2   : begin                                                { Read }
                 if (GetCurSymbol(elx_Globals) = sym_lparent) then
                   begin
                     REPEAT
                       InSymbol(elx_Globals);

                       if (GetCurSymbol(elx_Globals) <> sym_ident) then
                         begin
                           Error(elx_Globals, err_VarIdent)
                         end
                           else with elx_Globals do
                                 begin
                                  I := Loc(elx_Globals.BlockVars^.CurIdent, true);
                                  InSymbol(elx_Globals);

                                  if (i <> 0) then
                                    if (IdentTable^[i].obj <> obj_variable) then
                                      begin
                                        Error(elx_Globals, err_InvVarRef)
                                      end { if }
                                        else begin
                                               x.typ := IdentTable^[i].Typ;
                                               x.ref := IdentTable^[i].Ref;

                                               if IdentTable^[i].Normal then
                                                 F := 0
                                                   else F := 1;

                                               Emit2(elx_Globals, f, IdentTable^[i].lev, IdentTable^[i].adr);

                                               if (GetCurSymbol(elx_Globals) in [sym_lbrack, sym_lparent, sym_period]) then
                                                 begin
                                                   if (x.typ=typ_strngs) then
                                                     Error(elx_Globals, err_InvStrIdx);

                                                   Selector(fsys + [sym_comma,sym_rparent], x, i,
                                                            fakevar);
                                                 end; { if }

                                               if (x.typ in [typ_ints,typ_reals,typ_chars,typ_strngs,typ_notyp]) then
                                                 Emit1(elx_Globals, op_Read, Ord(x.Typ))
                                                   else Error(elx_Globals, err_NoReadWrite)
                                             end; { else }
                                end; { else }

                       Test([sym_comma,sym_rparent], fSys, err_Syntax);
                     until (GetCurSymbol(elx_Globals) <> sym_comma) OR (elx_Globals.AbortCompile);


                     if (GetCurSymbol(elx_Globals) = sym_rparent) then
                       InSymbol(elx_Globals)
                         else Error(elx_Globals, err_RParMiss);
                   end; { if }

                 if (Number = 2) then Emit(elx_Globals, op_Read)
               end; { 1, 2 }
         3,
         4   : begin { write }
                 if (GetCurSymbol(elx_Globals) = sym_lparent) then
                  with Elx_Globals do
                   begin
                     REPEAT
                       InSymbol(elx_Globals);
                       Expression(fSys + [sym_comma,sym_colon,sym_rparent], x);

                       if (not (x.typ in StandardTypes)) then
                         Error(elx_Globals, err_NoReadWrite);

                       if (GetCurSymbol(elx_Globals) = sym_colon) then
                         begin
                           InSymbol(elx_Globals);
                           Expression(fSys +[sym_comma,sym_colon,sym_rparent], y);

                           if (y.typ <> typ_ints) then Error(elx_Globals, err_MustInt);

                           if (GetCurSymbol(elx_Globals) = sym_colon) then
                             begin
                               if (x.typ <> typ_reals) then
                                 Error(elx_Globals, err_MustReal);

                               InSymbol(elx_Globals);
                               Expression(fSys + [sym_comma,sym_rparent], y);

                               if (y.typ <> typ_ints) then
                                 Error(elx_Globals, err_MustInt);

                               Emit(elx_Globals, op_DecimalReal);
                             end { if }
                               else begin
                                      if (x.typ = typ_strngs) then
                                        begin
                                          if x.temp then
                                            emit(elx_Globals, op_WriteStr2Tmp)
                                             else emit(elx_Globals, op_WriteStr2)
                                        end { if }
                                          else begin
                                                 Emit1(elx_Globals, op_Write2, Ord(X.Typ));
                                               end; { else }
                                    end; { else }
                         end { if }
                           else begin
                                 if (x.typ = typ_strngs) then
                                   begin
                                     if x.temp then emit(elx_Globals, op_WriteStrTmp)
                                       else emit(elx_Globals, op_WriteStr)
                                   end { if }
                                     else emit1(elx_Globals, op_Write1, ord(x.typ))
                                end; { else }

                     until (GetCurSymbol(elx_Globals) <> sym_comma) OR (AbortCompile);

                     if (GetCurSymbol(elx_Globals) = sym_rparent) then
                       InSymbol(elx_Globals)
                         else Error(elx_Globals, err_RParMiss)
                   end; { if }

                 if (Number = 4) then emit(elx_Globals, op_LineFeed )
               end; { write }

         5   : Emit(elx_Globals, op_Halt);                          { halt }
         6   : Emit1(elx_Globals, op_RunFunction, 43);         { Randomize }
         7   : Emit1(elx_Globals, op_RunFunction, 44);            { ClrScr }
         8,                                                       { gotoxy }
         18  : begin                                            { FileSeek }
                 if (GetCurSymbol(elx_Globals) = sym_lparent) then
                   InSymbol(elx_Globals)
                     else Error(elx_Globals, err_LParMiss);

                 Expression(fSys + [sym_comma,sym_rparent], x);

                 if (x.typ<>typ_ints) then
                   Error(elx_Globals, err_MustInt);

                 Test([sym_comma],fsys+[sym_rparent], err_CommaExpected);

                 if (GetCurSymbol(elx_Globals) = sym_comma) then
                   begin
                     InSymbol(elx_Globals);
                     Expression(fsys+[sym_rparent], y);

                     if (y.typ <> typ_ints) then
                       Error(elx_Globals, err_MustInt);

                     Case Number of
                       8  : Emit1(elx_Globals, op_RunFunction, 45); { GotoXY }
                       18 : Emit1(elx_Globals, op_RunFunction, 65); { FileSeek }
                     end; { case }
                   end; { if }

                 if (GetCurSymbol(elx_Globals) = sym_rparent) then
                   InSymbol(elx_Globals)
                     else Error(elx_Globals, err_RParMiss);
               end; { 8 (GotoXY), 18 (FileSeek) }
         9,
         10,
         11,
         12,
         14,
         15,
         16,                            { Textcolor, delay, textbg, sound, }
         17,                 { fileopen, filecreate, filedelete, fileclose }
         24  : begin                                            { filedone }
                 if (GetCurSymbol(elx_Globals) = sym_lparent) then
                   InSymbol(elx_Globals)
                     else Error(elx_Globals, err_LParMiss);

                 Expression(fSys + [sym_rparent], X);

                 if (x.typ <> typ_ints) then
                   Error(elx_Globals, err_MustInt);

                 Case Number of
                    9  : Emit1(elx_Globals, op_RunFunction, 46); { textcolor }
                    10 : Emit1(elx_Globals, op_RunFunction, 50); { delay }
                    11 : Emit1(elx_Globals, op_RunFunction, 51); { textbackground }
                    12 : Emit1(elx_Globals, op_RunFunction, 52); { sound }
                    14 : Emit1(elx_Globals, op_RunFunction, 58); { FileOpen }
                    15 : Emit1(elx_Globals, op_RunFunction, 59); { FileCreate }
                    16 : Emit1(elx_Globals, op_RunFunction, 60); { FileDelete }
                    17 : Emit1(elx_Globals, op_RunFunction, 63); { fileClose }
                    24 : Emit1(elx_Globals, op_RunFunction, 72); { fileDone }
                 end; { case }

                 if (GetCurSymbol(elx_Globals) = sym_rparent) then
                   InSymbol(elx_Globals)
                     else Error(elx_Globals, err_RParMiss);
               end; { 9, 10, 11, 12 }

         13  : Emit1(elx_Globals, op_RunFunction, 53);           { nosound }

         19  : begin                                          { FileRename }
                 {-- Make sure we have an opening parenthesis --------------}
                 if (GetCurSymbol(elx_Globals) = sym_lparent) then
                   InSymbol(elx_Globals)
                     else Error(elx_Globals, err_LParMiss);

                 {-- We expect the filehandle, act on it -------------------}
                 Expression(fSys + [sym_rparent, sym_comma], X);
                 if (x.typ <> typ_ints) then
                   Error(elx_Globals, err_MustInt);

                 {-- Make sure we get 2 parameters -------------------------}
                 Test([sym_comma] ,fSys + [sym_rParent], err_CommaExpected);

                 if (GetCurSymbol(elx_Globals) = sym_comma) then
                   begin
                     {-- Get the next parameter ----------------------------}
                     InSymbol(elx_Globals);

                     {-- Now we expect either a string or an char ----------}
                     Expression(fSys + [sym_rparent], X);

                     if (NOT (X.Typ in [typ_chars, typ_strngs])) then
                       Error(elx_Globals, err_StrExpected);
                   end; { if }

                 {-- Make sure we have a closing parenthesis ---------------}
                 if (GetCurSymbol(elx_Globals) = sym_rparent) then
                   InSymbol(elx_Globals)
                     else Error(elx_Globals, err_RParMiss);

                 {-- Now emit the actual function number -------------------}
                 Case X.Typ of
                   typ_chars : Emit1(elx_Globals, op_RunFunction, 66); { renamechar }
                   typ_strngs: Emit1(elx_Globals, op_RunFunction, 67); { renamestrng }
                 end; { case }
               end; { FileRename }

         20,                                               { FileReadStrLn }
         21,                                                { FileWriteStr }
         22,                                                    { FileRead }
         23,                                                   { fileWrite }
         25,                                           { FileWriteStringLn }
         26  : begin                                      { FileReadString }
                 {-- Make sure we have an opening parenthesis --------------}
                 if (GetCurSymbol(elx_Globals) = sym_lparent) then
                   InSymbol(elx_Globals)
                     else Error(elx_Globals, err_LParMiss);

                 {-- We expect the filehandle, act on it -------------------}
                 Expression(fSys + [sym_rparent, sym_comma], X);
                 if (x.typ <> typ_ints) then
                   Error(elx_Globals, err_MustInt);

                 {-- Make sure we get 2 parameters -------------------------}
                 Test([sym_comma] ,fSys + [sym_rParent], err_CommaExpected);

                 if (GetCurSymbol(elx_Globals) = sym_comma) then
                   begin
                     {-- Get the next parameter ----------------------------}
                     InSymbol(elx_Globals);

                     {-- Get what we want? ---------------------------------}
                     Expression(fSys + [sym_rparent], X);

                     {-- if this is a fileread -----------------------------}
                     if Number in [20, 26] then
                       begin
                         {-- Lookup the specified identifier ---------------}
                         TmpIdent := Loc(elx_Globals.BlockVars^.CurIdent, true);

                         with elx_Globals do
                           begin
                             if IdentTable^[TmpIdent].Normal then
                               F := 0
                                 else F := 1;

                             Emit2(elx_Globals, f, IdentTable^[TmpIdent].lev,
                                                   IdentTable^[Tmpident].adr);
                           end; { with }
                       end; { if }

                     Case Number of
                       20,
                       21,
                       25,
                       26 : if (NOT (X.Typ in [typ_strngs])) then
                              begin
                                Error(elx_Globals, err_StrExpected);
                              end; { if }
                       22,
                       23 : begin
                              if (NOT (X.Typ in [typ_records])) then
                                 Error(elx_Globals, err_NoRecord);

                              {-- We dont only need the variable to change -}
                              {-- but we also need the type of record it ---}
                              {-- is. Add that here ------------------------}
                              { was: x.ref }

                              {-- Lookup the specified identifier -------------}
                              TmpIdent := Loc(elx_Globals.BlockVars^.CurIdent, true);

                              if elx_Globals.IdentTable^[TmpIdent].Normal then
                                F := 0
                                  else F := 1;

                              Emit1(elx_Globals, op_Literal, TmpIdent);
                              Emit1(elx_Globals, op_Literal, X.Ref);
                              Emit1(elx_Globals, op_Literal, F);
                            end; { if }
                     end; { case }

                     {-- Make we sure its not temp -------------------------}
                     if X.Temp then
                       Error(elx_Globals, err_VarIdent);
                   end; { if }

                 {-- Make sure we have a closing parenthesis ---------------}
                 if (GetCurSymbol(elx_Globals) = sym_rparent) then
                   InSymbol(elx_Globals)
                     else Error(elx_Globals, err_RParMiss);

                 {-- Now emit the actual function number -------------------}
                 Case Number of
                   20 : Emit1(elx_Globals, op_RunFunction, 68); { FileReadStrLn }
                   21 : Emit1(elx_Globals, op_RunFunction, 69); { FileWriteStr }
                   22 : Emit1(elx_Globals, op_RunFunction, 70); { FileRead }
                   23 : Emit1(elx_Globals, op_RunFunction, 71); { FileWrite }
                   25 : Emit1(elx_Globals, op_RunFunction, 73); { FileWriteStrLn }
                   26 : Emit1(elx_Globals, op_RunFunction, 74); { FileReadString }
                 end; { case }

               end; { FileReadStrLn, FileWriteStr }
         27  : begin                                          { Concatenate }
                 {-- Make sure we have an opening parenthesis --------------}
                 if (GetCurSymbol(elx_Globals) = sym_lparent) then
                   InSymbol(elx_Globals)
                     else Error(elx_Globals, err_LParMiss);

                 {-- We expect either a string or an char ------------------}
                 Expression(fSys + [sym_rparent, sym_comma], X);
                 if (NOT (X.Typ in [typ_strngs])) then
                   Error(elx_Globals, err_StrExpected);

                 {-- now ensure this is a variable -------------------------}
                 I := Loc(elx_Globals.BlockVars^.CurIdent, true);
                 if (i <> 0) then
                   if (elx_Globals.IdentTable^[i].obj <> obj_variable) then
                     begin
                       Error(elx_Globals, err_InvVarRef)
                     end; { if }

                 {-- now load the variable ---------------------------------}
                 with elx_Globals do
                   begin
                     if IdentTable^[i].Normal then
                       F := 0
                         else F := 1;

                     Emit2(elx_Globals, f, IdentTable^[i].lev,
                                           IdentTable^[i].adr);
                   end; { with }

                 {-- Make sure we get 2 parameters -------------------------}
                 Test([sym_comma], fSys + [sym_rParent], err_CommaExpected);
                 InSymbol(elx_Globals);

                 {-- We expect either a string or an char ------------------}
                 Expression(fSys + [sym_rparent], X);
                 if (NOT (X.Typ in [typ_chars, typ_strngs])) then
                   Error(elx_Globals, err_StrExpected);

                 {-- Make sure we have a closing parenthesis ---------------}
                 if (GetCurSymbol(elx_Globals) = sym_rparent) then
                   InSymbol(elx_Globals)
                     else Error(elx_Globals, err_RParMiss);

                 {-- are we appending strings or chars ---------------------}
                 Case X.Typ of
                   typ_chars : F := 0;
                   typ_strngs: F := 1;
                 end; { case }

                 {-- free the string we are appending? ---------------------}
                 if X.Temp then
                   Inc(F, 4);

                 {-- Now emit the actual function number -------------------}
                 Emit2(elx_Globals, op_RunFunction, F, 75); { concatstr }
               end; { Concatenate }
      end; { case }
    end; { proc. StandProc }

  begin { statement }
    if elx_Globals.AbortCompile then EXIT;

    if (GetCurSymbol(elx_Globals) in elx_Globals.BlockVars^.statbegsys+[sym_ident]) then
     with elx_Globals do
      begin
        Case GetCurSymbol(elx_Globals) of
           sym_ident : begin
                         i := loc(BlockVars^.CurIdent, true);
                         InSymbol(elx_Globals);

                         if (i <> 0) then
                           begin
                             Case IdentTable^[i].obj of
                               obj_constant,
                               obj_type      : Error(elx_Globals, err_VarProc);
                               obj_variable  : Assignment(i, IdentTable^[i].lev, IdentTable^[i].adr);
                               obj_procedure : begin
                                                 if (IdentTable^[i].lev <> 0) AND (IdentTable^[i].Lev <> 1) then
                                                   Call(fSys, i)
                                                     else begin
                                                            Case IdentTable^[i].Lev of
                                                              0 : StandProc(IdentTable^[i].adr);
                                                              1 : UsersProc(IdentTable^[i].Adr, NilType, i);
                                                            end; { case }
                                                          end; { else }
                                               end; { if }
                               obj_function  : begin
                                                 if IdentTable^[i].Ref = Display^[Level] then
                                                   Assignment(i, IdentTable^[i].lev + 1, 0)
                                                     else Error(elx_Globals, err_VarProc)
                                               end; { if }
                             end; { case }
                           end; { if }
                       end; { sym_ident }
           sym_begin : CompoundStatement;
           sym_if    : IfStatement;
           sym_case  : CaseStatement;
           sym_while : WhileStatement;
           sym_repeat: RepeatStatement;
           sym_for   : ForStatement;
        end; { case }
      end; { if }

    Test(fSys, [], err_MissSemi)
  end; { proc. Statement }

var Counter: Integer;
begin { block }
  DataAllocd := 7;     { It contains the basic 7 variables for the calling }
                      { of a procedure, if there are any local copies of a }
                  { variable (procedure(r: integer), these get added to it }
                                    { real "local" variables are NOT added }
  IdentIndex := elx_Globals.IdentCount;

  with elx_Globals do
    if IdentCount > HighIdentCount then
      HighIdentCount := IdentCount;

  if Level > MaxLevel then
    FatalError(elx_Globals, fatal_levels);

  Test([sym_lparent,sym_colon,sym_Semicolon], fsys, err_MissSemi);

  AddBlock(elx_Globals);

  with elx_Globals do
    begin
      BlockIndex := BlockCount;
      Display^[Level] := BlockCount;

      IdentTable^[IdentIndex].typ := typ_notyp;
      IdentTable^[IdentIndex].ref := BlockIndex;
      IdentTable^[IdentIndex].MaxSize := -1;

      if (GetCurSymbol(elx_Globals) = sym_lparent) and (level > 1) then
        ParameterList;

      BlockTable^[BlockIndex].LastPar := IdentCount;
      BlockTable^[BlockIndex].pSize := DataAllocd; { 7 + all variables that are }
                                             { copies of all passed values }
    end; { if }


  if (isFunc) then
    if (GetCurSymbol(elx_Globals) = sym_colon) then
      begin
        InSymbol(elx_Globals);                                          { function type }

        if (GetCurSymbol(elx_Globals) = sym_ident) then
          begin
            Counter := loc(elx_Globals.BlockVars^.CurIdent, true);
            InSymbol(elx_Globals);

            if (Counter <> 0) then
              begin
                if elx_Globals.IdentTable^[Counter].obj <> obj_type then
                  begin
                    Error(elx_Globals, err_NoType)
                  end { if }
                    else begin
                           if (elx_Globals.IdentTable^[Counter].typ in elx_Globals.StandardTypes) then
                             begin
                               elx_Globals.IdentTable^[IdentIndex].typ := elx_Globals.IdentTable^[Counter].typ
                             end
                               else Error(elx_Globals, err_FuncResult)
                         end; { else }
              end; { if }
          end { if }
            else skip([sym_Semicolon] + fSys, err_VarIdent)
      end { if }
       else Error(elx_Globals, err_CollMiss);


  if (GetCurSymbol(elx_Globals) = sym_Semicolon) then
    inSymbol(elx_Globals)
      else Error(elx_Globals, err_MissSemi);

  REPEAT
(*
 *** OLD ***
 **
 ** The old routine doesnt facilitate declaring variables and types at multiple
 ** places, or more specifically. After a const/type/var block was done,
 ** and a procedure was defined, you couldnt add extra variables. This is
 ** especially not nice when including files
 **
 **
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    REPEAT
      if (GetCurSymbol(elx_Globals) = sym_const) then ConstDec;
      if (GetCurSymbol(elx_Globals) = sym_type) then TypeDeclaration;
      if (GetCurSymbol(elx_Globals) = sym_var) then VariableDeclaration;
    UNTIL (NOT (GetCurSymbol(elx_Globals) in [sym_const, sym_type, sym_var]));

    elx_Globals.BlockTable^[BlockIndex].vSize := DataAllocd;

    while (GetCurSymbol(elx_Globals) in [sym_proc,sym_func]) AND (NOT elx_Globals.AbortCompile) do
      ProcDeclaration;
(*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*)

{---- !!!! NEW !!!! ------}

    while (GetCurSymbol(elx_Globals) in [sym_const, sym_type, sym_var, sym_proc,sym_func]) AND
        (NOT elx_Globals.AbortCompile) do
      begin
        if (GetCurSymbol(elx_Globals) = sym_const) then ConstDec
          else if (GetCurSymbol(elx_Globals) = sym_type) then TypeDeclaration
           else if (GetCurSymbol(elx_Globals) = sym_var) then VariableDeclaration
            else begin
                     ProcDeclaration;
                  end; { else }
      end; { while }

    elx_Globals.BlockTable^[BlockIndex].vSize := DataAllocd;
{---- !!!! NEW !!!! ------}

    Test([sym_begin], elx_Globals.BlockVars^.blockbegsys + elx_Globals.BlockVars^.statbegsys, err_BeginExpected)
  UNTIL (GetCurSymbol(elx_Globals) in elx_Globals.BlockVars^.StatBegSys) OR (elx_Globals.AbortCompile);

  elx_Globals.IdentTable^[IdentIndex].Adr := elx_Globals.CurCode;
  InSymbol(elx_Globals);
  Statement([sym_Semicolon,sym_end]+fsys);

  While (GetCurSymbol(elx_Globals) in [sym_Semicolon] + elx_Globals.BlockVars^.statbegsys)
         AND (NOT elx_Globals.AbortCompile) do
   with elx_Globals do
    begin
      if GetCurSymbol(elx_Globals) = sym_Semicolon then
        InSymbol(elx_Globals)
          else Error(elx_Globals, err_MissSemi);

      Statement([sym_Semicolon,sym_end]+fsys)
    end; { while }

  if (GetCurSymbol(elx_Globals) = sym_end) then
    InSymbol(elx_Globals)
      else Error(elx_Globals, err_EndExpected);

  Test(fSys + [sym_period], [], err_Syntax)
end; { proc. Block }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AddIdentifier(var elx_Globals: elx_GlobalsType;
                        X0: AlfaType;           { Enter standard identifier }
                        X1: tObject;
                        X2: tType;
                        X3: Longint);

begin
  with elx_Globals do
    begin
      Inc(IdentCount);

      if IdentCount > HighIdentCount then
        HighIdentCount := IdentCount;

      if IdentCount >= MaxSyms then
        begin
          FatalError(elx_Globals, fatal_Idents);
          EXIT;
        end; { if }


      With IdentTable^[IdentCount] do
        begin
          Name   := X0;
          Link   := IdentCount - 1;
          Obj    := X1;
          Typ    := X2;
          Ref    := 0;
          Normal := TRUE;
          Lev    := 0;
          Adr    := X3;
          MaxSize:= -1;
        end; { with }
    end; { with }
end; { proc. AddIdentifier }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Initialize(var elx_Globals: elx_GlobalsType);

  procedure Setup;
  begin
    {!! MUST BE IN ALPHABETICAL ORDER FOR IT TO WORK !!}
    With elx_Globals, BlockVars^ do
      begin
        KwName[ 1] := 'and                           ';
        KwName[ 2] := 'array                         ';
        KwName[ 3] := 'begin                         ';
        KwName[ 4] := 'case                          ';
        KwName[ 5] := 'const                         ';
        KwName[ 6] := 'div                           ';
        KwName[ 7] := 'do                            ';
        KwName[ 8] := 'downto                        ';
        KwName[ 9] := 'dynamicarray                  ';
        KwName[10] := 'else                          ';
        KwName[11] := 'end                           ';
        KwName[12] := 'file                          ';
        KwName[13] := 'for                           ';
        KwName[14] := 'function                      ';
        KwName[15] := 'goto                          ';
        KwName[16] := 'if                            ';
        KwName[17] := 'in                            ';
        KwName[18] := 'label                         ';
        KwName[19] := 'mod                           ';
        KwName[20] := 'nil                           ';
        KwName[21] := 'not                           ';
        KwName[22] := 'of                            ';
        KwName[23] := 'or                            ';
        KwName[24] := 'packed                        ';
        KwName[25] := 'procedure                     ';
        KwName[26] := 'program                       ';
        KwName[27] := 'record                        ';
        KwName[28] := 'repeat                        ';
        KwName[29] := 'set                           ';
        KwName[30] := 'then                          ';
        KwName[31] := 'to                            ';
        KwName[32] := 'type                          ';
        KwName[33] := 'until                         ';
        KwName[34] := 'var                           ';
        KwName[35] := 'while                         ';
        KwName[36] := 'with                          ';

        KwSymbol[ 1] := sym_and;        KwSymbol[ 2] := sym_array;
        KwSymbol[ 3] := sym_begin;      KwSymbol[ 4] := sym_case;
        KwSymbol[ 5] := sym_const;      KwSymbol[ 6] := sym_idiv;
        KwSymbol[ 7] := sym_do;         KwSymbol[ 8] := sym_downto;
        KwSymbol[ 9] := sym_dynarray;
        KwSymbol[10] := sym_else;       KwSymbol[11] := sym_end;
        KwSymbol[12] := sym_file;       KwSymbol[13] := sym_for;
        KwSymbol[14] := sym_func;       KwSymbol[15] := sym_goto;
        KwSymbol[16] := sym_if;         KwSymbol[17] := sym_in;
        KwSymbol[18] := sym_label;      KwSymbol[19] := sym_imod;
        KwSymbol[20] := sym_nil;        KwSymbol[21] := sym_not;
        KwSymbol[22] := sym_of;         KwSymbol[23] := sym_or;
        KwSymbol[24] := sym_packed;     KwSymbol[25] := sym_proc;
        KwSymbol[26] := sym_program;    KwSymbol[27] := sym_record;
        KwSymbol[28] := sym_repeat;     KwSymbol[29] := sym_set;
        KwSymbol[30] := sym_then;       KwSymbol[31] := sym_to;
        KwSymbol[32] := sym_type;       KwSymbol[33] := sym_until;
        KwSymbol[34] := sym_var;        KwSymbol[35] := sym_while;
        KwSymbol[36] := sym_with;

        SpecialChars['+'] := sym_plus;        SpecialChars['-'] := sym_minus;
        SpecialChars['*'] := sym_times;       SpecialChars['/'] := sym_rdiv;
        SpecialChars[')'] := sym_rparent;
        SpecialChars['='] := sym_eql;         SpecialChars[','] := sym_comma;
        SpecialChars['['] := sym_lbrack;      SpecialChars[']'] := sym_rbrack;
        SpecialChars['~'] := sym_not;         SpecialChars['&'] := sym_and;
        SpecialChars[';'] := sym_semicolon;   SpecialChars['|'] := sym_or;
      end; { with }
  end; { proc. Setup }


  procedure EnterIDs;
  begin
    AddIdentifier(elx_Globals, '                              ', obj_variable, typ_notyp, 0);  { sentinel }
    AddIdentifier(elx_Globals, 'false                         ', obj_constant, typ_bools, 0);
    AddIdentifier(elx_Globals, 'true                          ', obj_constant, typ_bools, 1);
    AddIdentifier(elx_Globals, 'maxint                        ', obj_constant, typ_ints, 2147483647);
    AddIdentifier(elx_Globals, 'real                          ', obj_type, typ_reals, 1);
    AddIdentifier(elx_Globals, 'char                          ', obj_type, typ_chars, 1);
    AddIdentifier(elx_Globals, 'boolean                       ', obj_type, typ_bools, 1);
    AddIdentifier(elx_Globals, 'integer                       ', obj_type, typ_ints , 1);
    AddIdentifier(elx_Globals, 'string                        ', obj_type, typ_strngs,1);
    AddIdentifier(elx_Globals, 'abs                           ', obj_function, typ_reals,0);
    AddIdentifier(elx_Globals, 'sqr                           ', obj_function, typ_reals,2);
    AddIdentifier(elx_Globals, 'odd                           ', obj_function, typ_bools,4);
    AddIdentifier(elx_Globals, 'chr                           ', obj_function, typ_chars,5);
    AddIdentifier(elx_Globals, 'ord                           ', obj_function, typ_ints, 6);
    AddIdentifier(elx_Globals, 'succ                          ', obj_function, typ_chars,7);
    AddIdentifier(elx_Globals, 'pred                          ', obj_function, typ_chars,8);
    AddIdentifier(elx_Globals, 'round                         ', obj_function, typ_ints, 9);
    AddIdentifier(elx_Globals, 'trunc                         ', obj_function, typ_ints, 10);
    AddIdentifier(elx_Globals, 'sin                           ', obj_function, typ_reals, 11);
    AddIdentifier(elx_Globals, 'cos                           ', obj_function, typ_reals, 12);
    AddIdentifier(elx_Globals, 'exp                           ', obj_function, typ_reals, 13);
    AddIdentifier(elx_Globals, 'ln                            ', obj_function, typ_reals, 14);
    AddIdentifier(elx_Globals, 'sqrt                          ', obj_function, typ_reals, 15);
    AddIdentifier(elx_Globals, 'arctan                        ', obj_function, typ_reals, 16);
    AddIdentifier(elx_Globals, 'eof                           ', obj_function, typ_bools, 17);
    AddIdentifier(elx_Globals, 'eoln                          ', obj_function, typ_bools, 18);
    AddIdentifier(elx_Globals, 'maxavail                      ', obj_function, typ_ints, 19);
    AddIdentifier(elx_Globals, 'length                        ', obj_function, typ_ints, 20);
    AddIdentifier(elx_Globals, 'copy                          ', obj_function, typ_strngs, 23);
    AddIdentifier(elx_Globals, 'pos                           ', obj_function, typ_ints, 26);
    AddIdentifier(elx_Globals, 'str                           ', obj_function, typ_strngs, 33);
    AddIdentifier(elx_Globals, 'val                           ', obj_function, typ_ints, 35);
    AddIdentifier(elx_Globals, 'rval                          ', obj_function, typ_reals, 37);
    AddIdentifier(elx_Globals, 'keypressed                    ', obj_function, typ_bools, 39);
    AddIdentifier(elx_Globals, 'random                        ', obj_function, typ_ints, 40);
    AddIdentifier(elx_Globals, 'upcase                        ', obj_function, typ_chars, 42);
    AddIdentifier(elx_Globals, 'readkey                       ', obj_function, typ_strngs, 47);
    AddIdentifier(elx_Globals, 'wherex                        ', obj_function, typ_ints, 48);
    AddIdentifier(elx_Globals, 'wherey                        ', obj_function, typ_ints, 49);   {next: 54}
    AddIdentifier(elx_Globals, 'fileassign                    ', obj_function, typ_ints, 54); { 55, 56, }
    AddIdentifier(elx_Globals, 'filegeterror                  ', obj_function, typ_ints, 57);
    AddIdentifier(elx_Globals, 'filegetsize                   ', obj_function, typ_ints, 61);
    AddIdentifier(elx_Globals, 'filegetpos                    ', obj_function, typ_ints, 62);
    AddIdentifier(elx_Globals, 'fileexist                     ', obj_function, typ_bools, 64);
    AddIdentifier(elx_Globals, 'read                          ', obj_procedure, typ_notyp, 1);
    AddIdentifier(elx_Globals, 'readln                        ', obj_procedure, typ_notyp, 2);
    AddIdentifier(elx_Globals, 'write                         ', obj_procedure, typ_notyp, 3);
    AddIdentifier(elx_Globals, 'writeln                       ', obj_procedure, typ_notyp, 4);
    AddIdentifier(elx_Globals, 'halt                          ', obj_procedure, typ_notyp, 5);
    AddIdentifier(elx_Globals, 'randomize                     ', obj_procedure, typ_notyp, 6);
    AddIdentifier(elx_Globals, 'clrscr                        ', obj_procedure, typ_notyp, 7);
    AddIdentifier(elx_Globals, 'gotoxy                        ', obj_procedure, typ_notyp, 8);
    AddIdentifier(elx_Globals, 'textcolor                     ', obj_procedure, typ_notyp, 9);
    AddIdentifier(elx_Globals, 'delay                         ', obj_procedure, typ_notyp, 10);
    AddIdentifier(elx_Globals, 'textbackground                ', obj_procedure, typ_notyp, 11);
    AddIdentifier(elx_Globals, 'sound                         ', obj_procedure, typ_notyp, 12);
    AddIdentifier(elx_Globals, 'nosound                       ', obj_procedure, typ_notyp, 13);
    AddIdentifier(elx_Globals, 'fileopen                      ', obj_procedure, typ_notyp, 14);
    AddIdentifier(elx_Globals, 'filecreate                    ', obj_procedure, typ_notyp, 15);
    AddIdentifier(elx_Globals, 'filedelete                    ', obj_procedure, typ_notyp, 16);
    AddIdentifier(elx_Globals, 'fileclose                     ', obj_procedure, typ_notyp, 17);
    AddIdentifier(elx_Globals, 'fileseek                      ', obj_procedure, typ_notyp, 18);
    AddIdentifier(elx_Globals, 'filerename                    ', obj_procedure, typ_notyp, 19); { 66, 67 }
    AddIdentifier(elx_Globals, 'filereadstringln              ', obj_procedure, typ_notyp, 20); { 68 }
    AddIdentifier(elx_Globals, 'filewritestring               ', obj_procedure, typ_notyp, 21); { 69 }
    AddIdentifier(elx_Globals, 'fileread                      ', obj_procedure, typ_notyp, 22); { 70 }
    AddIdentifier(elx_Globals, 'filewrite                     ', obj_procedure, typ_notyp, 23); { 71 }
    AddIdentifier(elx_Globals, 'filedone                      ', obj_procedure, typ_notyp, 24); { 72 }
    AddIdentifier(elx_Globals, 'filewritestringln             ', obj_procedure, typ_notyp, 25); { 73 }
    AddIdentifier(elx_Globals, 'filereadstring                ', obj_procedure, typ_notyp, 26); { 74 }
{!} AddIdentifier(elx_Globals, 'concatenate                   ', obj_procedure, typ_notyp, 27); { 75 }
    AddIdentifier(elx_Globals, 'expand_vars                   ', obj_function,  typ_strngs,76); { 76 }
    AddIdentifier(elx_Globals, '                              ', obj_procedure, typ_notyp, 0);
end;  { AddIdentifierids }

begin
  {-- Initialize the type of symbols that are allowed -----------------------}
  With elx_Globals, BlockVars^ do
    begin
      ConstBegSys   := [sym_plus, sym_minus, sym_intcon, sym_realcon, sym_charcon, sym_stringcon, sym_ident];
      TypeBegSys    := [sym_ident, sym_array, sym_record, sym_dynarray];
      BlockBegSys   := [sym_const, sym_type, sym_var, sym_proc, sym_func, sym_begin];
      FacBegSys     := [sym_intcon, sym_realcon, sym_charcon, sym_stringcon, sym_ident, sym_lparent, sym_not];
      StatBegSys    := [sym_begin, sym_if, sym_while, sym_repeat, sym_for, sym_case];
      StandardTypes := [typ_notyp, typ_ints, typ_reals, typ_bools, typ_chars, typ_strngs];
    end; { with }

  {-- Initialize all keywords -----------------------------------------------}
  Setup;

  {-- Initialize all identifiers --------------------------------------------}
  EnterIDs;
end; { proc. Initialize }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure InitVars(var elx_Globals: elx_GlobalsType);
var Counter: Longint;
begin
  {-- Initialize all sorts of parameters ------------------------------------}
  New(elx_Globals.BlockVars);

  With elx_Globals, BlockVars^ do
    begin
      for Counter := 0 to High(ErrorSet) do
        ErrorSet[Counter] := 0;

      SourceFile[CurSrcFile].CurLineNum := 0;
      CurCode := 0;
      CurLineLen := 0;
      CharCount := 0;
      LastCH := #32;
      ErrorPos := 0;
      StartBlock := 0;                                          { No errors }

      IdentCount := -1;
      ArrayCount := 0;
      BlockCount := 1; { 1 }
      RealCount2 := 0;
{ excluded for win32    Display^[0] := 1; } { 2 }

      SkipFlag := FALSE;
      PrTables := FALSE;   { Print the tables at the end of execution }
      StackDump := FALSE;  { Dump stack after proc/func call }
    end; { with }
end; { proc. InitVars }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AddUserIdent(var elx_Globals: elx_GlobalsType;
                       X0: AlfaType;             { Enter a user defined ID }
                       X1: tObject;
                       X2: tType;
                       X3: Longint;
                       VarList: String);
begin
  if NOT elx_Globals.elx_OnlyParamList then
   with elx_Globals do
    begin
      {-- Level should always be 1 for user defined procs. The compiler ----}
      {-- uses this level to run specially written user-calls for this type-}
      Inc(IdentCount);

      if IdentCount > HighIdentCount then
        HighIdentCount := IdentCount;

      if IdentCount >= MaxSyms then
        begin
          FatalError(elx_Globals, fatal_Idents);
          EXIT;
        end; { if }

      With IdentTable^[IdentCount] do
        begin
          Name   := X0;
          Link   := IdentCount - 1;
          Obj    := X1;
          Typ    := X2;
          Ref    := 0;
          Normal := TRUE;
          Lev    := 1;
          Adr    := X3;
          MaxSize:= -1;
        end; { with }
    end; { elx_OnlyParamList }

  {-- Now setup the parameter list -----------------------------------------}
  GetMem(elx_Globals.UserParamList^[X3], Length(VarList) + 2);

  if X1 = obj_Function then
    begin
      if X2 = typ_strngs then
        elx_Globals.UserParamList^[X3]^ := 'F' + VarList
          else elx_Globals.UserParamList^[X3]^ := 'f' + VarList;
    end
      else elx_Globals.UserParamList^[X3]^ := 'P' + VarList;
end; { proc. AddUserIdent }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ProcessBlocks(var elx_Globals: elx_GlobalsType);
var SaveIdent: Longint;
begin
  {-- Get our first symbol --------------------------------------------------}
  InSymbol(elx_Globals);
  if GetCurSymbol(elx_Globals) <> Sym_program then Error(elx_Globals, err_ProgMiss)         { Make sure its PROGRAM }
    else begin
            InSymbol(elx_Globals);

            if GetCurSymbol(elx_Globals) <> sym_Ident then Error(elx_Globals, err_VarIdent)          { program name }
              else begin
                     elx_Globals.ModuleName := elx_Globals.BlockVars^.CurIdent;

                     InSymbol(elx_Globals);
                     if GetCurSymbol(elx_Globals) = sym_lParent then
                       begin
                         REPEAT
                           InSymbol(elx_Globals);

                           if GetCurSymbol(elx_Globals)<> sym_ident then Error(elx_Globals, err_VarIdent)
                             else InSymbol(elx_Globals);
                         UNTIL (GetCurSymbol(elx_Globals) <> sym_Comma) OR (elx_Globals.AbortCompile);

                         if GetCurSymbol(elx_Globals) = sym_RParent then
                           InSymbol(elx_Globals)
                             else Error(elx_Globals, err_RParMiss);
                       end; { if }
                   end; { if }
         end; { if }

  {-- Save identcounter -----------------------------------------------------}
  SaveIdent := elx_Globals.IdentCount;

  {-- Add a block for all user defined procedures ---------------------------}
  if Assigned(elx_Globals.elx_AddUserFuncsHook) then
    elx_GLobals.elx_AddUserFuncsHook(elx_Globals, false);

  {-- Start block - we set it here ------------------------------------------}
  with elx_Globals do
    begin
      StartBlock := 2; { BlockCount; } { was: 2 }
      BlockCount := 2;                 { was: 2 }
      Display^[0] := BlockCount;
      Display^[1] := BlockCount;
    end; { with }

  {-- Got program name etcetera, identify first block -----------------------}
  With elx_Globals, BlockTable^[1] do { BlockCount }
    begin
      Last := SaveIdent;
      LastPar := 1; { 1 }
      pSize := 0; { 0 }
      vSize := 0; { 0 }
    end; { with }

  {-- Set parameters for the user-defined procedures ------------------------}
  With elx_Globals, BlockTable^[2] do { BlockCount }
    begin
      Last := IdentCount;
      LastPar := 1;
      pSize := 0; { 0 }
      vSize := 0; { 0 }
    end; { with }

  {-- Start processing the first block --------------------------------------}
  with elx_Globals do
    Block(elx_Globals, BlockVars^.BlockBegSys + BlockVars^.StatBegSys, false, StartBlock);

  if GetCurSymbol(elx_Globals) <> sym_Period then    { Make sure the source ends with a dot }
    Error(elx_Globals, err_UnexpecEof);

  {-- Close off the program -------------------------------------------------}
  Emit(elx_Globals, op_halt);                                       { halt }

  {-- dispose of the memory -------------------------------------------------}
  Dispose(elx_Globals.BlockVars);
end; { proc. ProcessBlocks }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { exl_blck }
