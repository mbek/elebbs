program EleXer;
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
{$R+}
{$I Compiler.INC}

{$IFDEF WITH_FULL}
  Dont do - this will include CRT, messing up display of tabs in the error
  reporter
{$ENDIF}
{.$R-,S-,W-}
(*
**
** ELEXER - PascalS compiler with string support.
** Modified sourcecode of FACILIS
**
** Created : 06-Jan-2000
** Last update : 16-Jan-2003
**
**
** Original copyrights:
**   Facilis 0.31                                      file: FACILIS.PAS
**     based on the Pascal-S compiler of Niklaus Wirth,
**     as modified by R.E. Berry
**
**     adapted for the IBMPC by John R. Naleszkiewicz
**
**     extensions by Anthony M. Marcy
**
*)

(*
** Change log:
**   Revised all code to match my formatting styles, changed (almost) all
**   names of all identifiers to match their function etcetera.
**   Made all string handling based upon procedures/functions instead of
**     direct modifying memory locations
**   Changed the interpreter to use opcodes instead of plain numbers.
m*   Split it up in multiple units.
**
**  29-10-2000:
**   * Added support for # in chars
**  19-11-2000:
**   * Added a requirement (and support) for ELSE statements in case
**     statements.
**  xx-Feb-2001:
**   * Added support for hookable i/o (readln, writeln, readkey, etc)
**  xx-Feb-2001:
**   * Added support for runtime-supplied procedures and functions
**  11-Mar-2001:
**   * Added support for reading of records and other stuff.
**
**
**  ToDo:
**   Add support for proc(var char) - proc(s[1])
**   Programmer supplied records
**   using filereadstrln() and stuf in a procedure with a var parameter
**     almost certainly leads to memory corruption
**
**
*)
uses  StrPath,                                       { Extension extraction }
       FileObj,                              { Basic file handling routines }
        Elx_Glob,                            { Global identifiers and stuff }
         Elx_Int,                                     { Interpreter for ELX }
          Elx_Blck;                               { Actual compiler for ELX }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

type
  OptionsType = (elxCompile, elxInterpret, elxShowHelp, elxInfo,
                 elxLogError);

const
  DefaultOptions : Set of OptionsType = [elxShowHelp];
  FinishInit     : Boolean = FALSE;

var
  Options     : Set of OptionsType;
  SaveExitProc: Pointer;
  elx_Globals : elx_GlobalsType;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure elx_UserFuncs(const FuncNr: Longint;
                        var   elx_GlobalsBuf;
                        var   TmpStack: Array of StackType;
                        var   FuncReturn: StackType;
                        var   CallPtr: Pointer;
                        const CallTypes: String);
begin
  WriteLn('User function #', FuncNr, ' - not implemented in EleXer runtime (call list: "', CallTypes,'")');
end; { proc. elx_UserFuncs }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure elx_SystemFuncs(const FuncNr: Longint;
                          var   elx_GlobalsBuf;
                          var   TmpStack: Array of StackType;
                          var   CallPtr: Pointer;
                          var   FuncReturn: StackType);
begin
  {$IFNDEF ISCGI}
  Case FuncNr of
{     39 : FuncReturn.B := KeyPressed;
     44 : ClrScr;
     45 : GotoXY(TmpStack[0].I, TmpStack[1].I);
     46 : TextColor(TmpStack[0].I);}
     47 : begin
            if StrAllocNew(elx_Globals, FuncReturn.S, 1) then
              begin
{                StrSetString(elx_Globals, FuncReturn.S, ReadKey, 1); }
{                StrSetStrLen(elx_Globals, FuncReturn.S, 1); }
              end; { if }
          end; { if }
{     48 : FuncReturn.I := WhereX;
     49 : FuncReturn.I := WhereY;
     50 : Delay(TmpStack[0].I);
     51 : TextBackGround(TmpStack[0].I); }
  end; { case }
  {$ENDIF}
end; { proc. elx_SystemFuncs }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF MSDOS}
procedure elx_UserWrite(const TmpStr: String; DoLn: Boolean);
{$ELSE}
procedure elx_UserWrite(const TmpStr: AnsiString; DoLn: Boolean);
{$ENDIF}
begin
  if TmpStr <> '' then
    Write(TmpStr);

  if DoLn then
    WriteLn;
end; { proc. elx_UserWrite }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure elx_UserRead(var TmpStr; typ: tType; DoLn: Boolean);
begin
  { Read(Ln) is not supported under EleBBS }
end; { proc. elx_UserRead }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowHelp;
const
  WhiteStr = '         ';

begin
  WriteLn(#32, #254, #32, 'Command-line parameters:');
  WriteLn;
  WriteLn(WhiteStr, '-C <filename>     Compile source file');
{  WriteLn(WhiteStr, '-I <filename>     Interpret compiled file'); }
  WriteLn(WhiteStr, '-N <filename>     Display info about compiled file');
  WriteLn(WhiteStr, '-E                Log errors to a file (<filename>.ERR)');
  WriteLn(WhiteStr, '-H                Show this help screen');
  WriteLn;
  WriteLn(#32, #254, #32, 'Please refer to the documentation for a more complete command summary');
  WriteLn;

  Halt(255);
end; { proc. ShowHelp }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FatalError(S: String);
begin
  WriteLn;
  WriteLn(#32, #254, #32, S, ^G);
  WriteLn;

  Halt(255);
end; { proc. FatalError }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CloseDownExitProc;
begin
  ExitProc := SaveExitProc;
end; { proc. CloseDownExitProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowHeader;
begin
  {-- Show the welcome header -----------------------------------------------}
  WriteLn;
  WriteLn('ELEXER; Compiler/interpreter, Version ', CompVersion:4:2);
  WriteLn('        Copyright 1997-2003 Maarten Bekers, All rights reserved.');
  WriteLn;
end; { proc. ShowHeader }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ParseCommandLine;
var Invalid : Boolean;
    InvOpt  : String;
    Counter : Byte;
    Param   : String;
    TempCH  : Char;
begin
  Invalid := true;                     { Default to showing the help screen }
  InvOpt := '';
  Options := [];

  if ParamCount > 00 then
    begin
      Counter := 01;
      While Counter <= ParamCount do
        begin
          Param := ParamStr(Counter);
          TempCH := UpCase(Param[2]);
          Inc(Counter);

          if (NOT (Param[1] in ['/', '-'])) then
            begin
              InvOpt := ParamStr(Counter);
              Invalid := True;
              BREAK;
            end; { Invalid }

         Case UpCase(TempCH) of
            'C' : begin
                     Options := Options + [elxCompile];
                     elx_Globals.SourceFile[elx_globals.CurSrcFile].SrcName := ParamStr(Counter);
                     Inc(Counter);                          { Skip filename }
                     Invalid := false;
                  end; { compile }
            'I' : begin
                     Options := Options + [elxInterpret];
                     elx_Globals.SourceFile[elx_Globals.CurSrcFile].SrcName := ParamStr(Counter);
                     Inc(Counter);                          { Skip filename }
                     Invalid := false;
                  end; { compile }
            'N' : begin
                     Options := Options + [elxInfo];
                     elx_Globals.SourceFile[elx_Globals.CurSrcFile].SrcName := ParamStr(Counter);
                     Inc(Counter);                          { Skip filename }
                     Invalid := false;
                  end; { compile }
            'E' : begin
                    Options := Options + [elxLogError];
                  end; { LogErrors }
              else begin
                     Invalid := True;
                     InvOpt := ParamStr(Counter);
                end; { Invalid option }

         end; { case }
      end; { while }
    end; { if }

  {-- Syntax check selected options -----------------------------------------}
  if elxInfo in Options then
    if (elxCompile in Options) OR (elxInterpret in Options) then
      Invalid := true;

  if (elxCompile in Options) OR (elxInterpret in Options) OR
      (elxInfo in Options) then
        if elx_Globals.SourceFile[elx_Globals.CurSrcFile].SrcName = '' then
          Invalid := true;

  {-- If invalid option, show help ------------------------------------------}
  if Invalid then
    begin
      Options := [elxShowHelp];
    end; { Invalid }

  {-- Now show all selected options -----------------------------------------}
  if (NOT (elxShowHelp in Options)) then
    begin
      Writeln(#32, #254, #32, 'Active options:');

      if elxCompile in Options then WriteLn('   - Compiling source file');
      if elxInterpret in options then WriteLn('   - Interpreting module');
      if elxLogError in Options then WriteLn('   - Log errors and information to file');
      if elxInfo in Options then WriteLn('   - Displaying info about module');
      Writeln;
    end; { if }
end; { proc. ParseCommandLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoOpenFiles;                                { Open up sourcefiles }
begin
  {-- Internal check --------------------------------------------------------}
  FinishInit := TRUE;
  if elx_Globals.SourceFile[elx_Globals.CurSrcFile].SrcName = '' then
    begin
      FatalError('Sourcename empty (program logic error)');
    end; { if }
  elx_Globals.ErrorName := NoExtension(elx_Globals.SourceFile[elx_Globals.CurSrcFile].SrcName)+ '.err';

  with elx_Globals do
    if NoExtension(elx_Globals.SourceFile[elx_Globals.CurSrcFile].SrcName) =
       elx_Globals.SourceFile[elx_Globals.CurSrcFile].SrcName then
         elx_Globals.SourceFile[elx_Globals.CurSrcFile].SrcName:=
           elx_Globals.SourceFile[elx_Globals.CurSrcFile].SrcName+ '.pas';

  {-- Open the source file --------------------------------------------------}
  if (elxCompile in Options) then
   with elx_Globals do
    begin
      New(SourceFile[CurSrcFile].SrcFile, Init);
      SourceFile[CurSrcFile].SrcFile^.Assign(SourceFile[CurSrcFile].SrcName);
      SourceFile[CurSrcFile].SrcFile^.FileMode := ReadMode + DenyWrite;
      if NOT SourceFile[CurSrcFile].SrcFile^.Open(1) then
        begin
          WriteLn(#32, #254, #32, 'Unable to open "', SourceFile[CurSrcFile].SrcName, '" (',
                      SourceFile[CurSrcFile].SrcFile^.IoResult, ')');
          FinishInit := FALSE;
          EXIT;
        end; { if }
    end; { if }

  {-- Open the errors file --------------------------------------------------}
  if elx_Globals.LogErrors then
   with elx_Globals do
    begin
      New(ErrorFile, Init);
      ErrorFile^.Assign(ErrorName);
      ErrorFile^.FileMode := ReadWriteMode + DenyAll;
      if NOT ErrorFile^.Create(1) then
        begin
          WriteLn(#32, #254, #32, 'Unable to create "', ErrorName, '"');
          FinishInit := FALSE;
          EXIT;
        end; { if }
    end; { if }
end; { proc. DoOpenFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowModuleInfo;
const
  WhiteStr = '   ';
begin
  WriteLn;
  WriteLn(WhiteStr, 'Filename . . . . . . . . . . . : ', elx_Globals.SourceFile[elx_Globals.CurSrcFile].SrcName);
  WriteLn(WhiteStr, 'Modulename . . . . . . . . . . : ', elx_Globals.ModHdr.ModName);
  WriteLn(WhiteStr, 'Compiler version . . . . . . . : ', elx_Globals.ModHdr.Version);
  WriteLn(WhiteStr, 'Compile date . . . . . . . . . : ', elx_Globals.ModHdr.DateTime);
  WriteLn;
end; { proc. ShowModuleInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

begin
  {-- Init the local variables ----------------------------------------------}
  Options := DefaultOptions;
  SaveExitProc := ExitProc;
  ExitProc := @CloseDownExitproc;

  {-- Setup some globals ----------------------------------------------------}
  elx_Globals.LogErrors := FALSE;
  elx_Globals.AbortCompile := FALSE;
  elx_Globals.SourceFile[elx_Globals.CurSrcFile].SrcName:= '';
  elx_Globals.ErrorName := '';

  {-- Initialize some global variables --------------------------------------}
  InitVars(elx_Globals);

  {-- Show the Elexer header ------------------------------------------------}
  ShowHeader;

  {-- Parse the commandline options the users have given --------------------}
  ParseCommandLine;

  {-- If error logging is on, turn on worse logging by default --------------}
  if (elxLogError in Options) then
   with elx_Globals do
    begin
      LogErrors := TRUE;                     { Enable default error logging }
      StackDump := TRUE;        { Dump the stack right after running a proc }
      PrTables := TRUE;         { Print arrays, blocks, etc after compiling }
    end; { if LogErrors }

  {-- Run the default initialize routines -----------------------------------}
  if NOT GlobalInitialize(elx_Globals) then
    begin
      FatalError('Unable to initialize compiler/interpreter');
    end; { fatal error }

  {-- Add the user hook procedures ------------------------------------------}
  elx_Globals.elx_AddUserFuncsHook := {$IFDEF FPC}@{$ENDIF}elx_AddUserFuncs;
  elx_Globals.elx_UserFuncsHook := {$IFDEF FPC}@{$ENDIF}elx_UserFuncs;
  elx_Globals.elx_SystemFuncsHook := {$IFDEF FPC}@{$ENDIF}elx_SystemFuncs;
  elx_Globals.elx_UserWriteHook := {$IFDEF FPC}@{$ENDIF}elx_UserWrite;
  elx_Globals.elx_UserReadHook := {$IFDEF FPC}@{$ENDIF}elx_UserRead;

  {-- Now run the options the user has selected -----------------------------}
  if (elxShowHelp in Options) then ShowHelp;

  {-- Open up the source and error files, if necessary ----------------------}
  if (elxCompile in Options) OR (elxInterpret in Options) OR
      (elxInfo in Options) then DoOpenFiles;

  {-- Make sure all is ok, then proceed -------------------------------------}
  if FinishInit then
    begin
      if (elxCompile in Options) then
        begin
          {-- Initialize sourcecode files and keywords and the like ---------}
          Initialize(elx_Globals);

          {-- Compile the actual code ---------------------------------------}
          ProcessBlocks(elx_Globals);

          {-- Print the tables if needed ------------------------------------}
          if elx_Globals.PrTables then Printtables(Elx_Globals);

          {-- Write this to disk --------------------------------------------}
          if NOT HasErrors(elx_Globals) then
           with elx_Globals do
            begin
              HadErrors := FALSE;
              WriteLn(#32, #254, #32, 'Compiled succesfully.');

              if NOT WriteModuleToDisk(elx_Globals, ElxModName(SourceFile[0].SrcName)) then
                FatalError('Unable to write "' + ElxModName(SourceFile[0].SrcName) + '"');
            end
              else with elx_Globals do
                    begin
                      HadErrors := TRUE;
                      WriteLn(#32, #254, #32, 'Compiled with errors.');

                      if LogErrors then
                        begin
                          WriteLn('Please check the log for a more detailed report.');
                          ErrorFile^.WriteLn('');
                          ErrorFile^.WriteLn('Compiled with errors');
                          ErrorFile^.WriteLn('');
                        end; { if }

                      ErrorMsg(elx_Globals);
                    end; { else }

          {-- Dispose of all used memory ------------------------------------}
          GlobalFreeMemory(elx_Globals, false, true);
        end; { if }

      {-- Read the module file back from disk -------------------------------}
      if (elxInfo in Options) OR (elxInterpret in Options) then
        if NOT elx_Globals.HadErrors then
         with elx_Globals do
          begin
            {-- Read module back from disk ----------------------------------}
            if NOT ReadModuleFromDisk(elx_Globals, ElxModName(SourceFile[0].SrcName)) then
              FatalError('Unable to read "' + ElxModName(SourceFile[0].SrcName) + '"');

            if elx_Globals.ModHdr.Version <> elxVersionID then
              FatalError('Incompatible EleXer module version');
          end; { if }

      {-- And interpret it if wanted ----------------------------------------}
      if (elxInterpret in Options) then
       if NOT elx_Globals.HadErrors then
        with elx_Globals do
         begin
           {-- Add a block for all user defined procedures ------------------}
           if Assigned(elx_AddUserFuncsHook) then
             elx_AddUserFuncsHook(elx_Globals, true);

           Interpret(elx_Globals, '', Pointer(elx_Globals.SourceFile[0].SrcFile));
         end; { if }

      {-- Show info if wanted -----------------------------------------------}
      if (elxInfo in Options) then
        begin
          ShowModuleInfo;
        end; { if }
    end; { FinishInit }

  {-- Dispose of all used memory --------------------------------------------}
  GlobalFreeMemory(elx_Globals, true, true);

  {-- Close all files -------------------------------------------------------}
  with elx_Globals do
    begin
       if SourceFile[0].SrcFile <> nil then
        Dispose(SourceFile[0].SrcFile, Done);

      if ErrorFile <> nil then
        Dispose(ErrorFile, Done);
    end; { with }
end. { program ELEXER }
