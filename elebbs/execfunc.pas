unit ExecFunc;
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
** Color routines for EleBBS
**
** Copyright (c) 1996-1998 by Maarten Bekers
**
** Created : 01-Jan-1998
** Last update : 01-Jan-1998
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

{$IFDEF WINGUI}
uses Win_Main, Dialogs;

function WinExecAndWait( Path: Pchar; Visibility: word; KeepWait: Boolean): longint;
{$ENDIF}

{$IFDEF WIN32}
 uses Windows;
{$ENDIF}

procedure RaExec(Cmd: String; IsDOS, ShowMsg, IsMailer, ForceInherit: Boolean; var ExitCode: Word; var Success: Boolean);
 { Performs an RaAlike exec }
{$IFDEF WIN32}
  function GetDuplHandle(Hndl: THandle): THandle;
{$ENDIF}
{$IFDEF WIN32}
procedure ExecuteDosFosProgram(CmdLine: String);
function  DoRunWithFossil(CmdLine: String): Boolean;
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


uses Crt, Global, CfgRec, LongStr, Support, WordStr, Debug_U, InOut_U, Ranges,
     RAL, OutBlock, ElLOG_U, GenDos, Dos, FileRout, StUTils, MultiLn,
     ComUnit, ApFossil, User_U, StrPath, CurTime, Terminal, ObjDec,
     FileObj, CharUnit,

     {$IFDEF WINGUI}
       Messages,
     {$ENDIF}

     {$IFDEF ELEUNIX}
       {$IFDEF VER1_0}
         Linux,
       {$ELSE}
         Unix,
       {$ENDIF}
     {$ENDIF}

     {$IFDEF MSDOS}
       Exec,
       Memory,
     {$ENDIF}
     Cases, TagUnit, GenFile, Main, Control, ExitProg,
     Limit_U, JDates, StatusB, Input_U, SysUtils;


{$IFDEF WIN32}
  {$I GETERR.INC}

procedure WriteGetErr;
var F: File;
begin
  Assign(F, GlobalCfg^.RaConfig^.SysPath + 'GT$RR'+FStr(LineCfg^.RaNodeNr)+'.EXE');
  FileMode := ReadWriteMode + DenyAll;


  {$i-}
    ReWrite(F, 1);
    BlockWrite(F, GetErrorArray, SizeOf(GetErrorArray));
    Close(F);
  {$i+}
  if IOResult > 00 then
   begin
     {$i-}
       Erase(f);
     {$i+}
     if IOresult > 00 then ;
   end; { if }
end; { proc. WriteGetErr }

function GetWinErrorCode: SmallWord;
var F: File;
begin
  Result := 00;

  Assign(F, '$!$EXIT'+FStr(LineCfg^.RaNodeNr)+'.E$$');
  {$i-}
    FileMode := ReadWriteMode + DenyNone;
    Reset(F, 1);
    BlockRead(F, Result, SizeOf(SmallWord));
    Close(F);
    Erase(F);
  {$i+}
  if IOResult > 00 then;

  Assign(F, GlobalCfg^.RaConfig^.SysPath + 'GT$RR'+FStr(LineCfg^.RaNodeNr)+'.EXE');
  {$i-} Erase(f); {$i+}
  if IOResult > 00 then ;
end; { func. GetWinErrorCode }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WIN32}
function GetDuplHandle(Hndl: THandle): THandle;
begin
  if not DuplicateHandle(GetCurrentProcess,
                           Hndl,
                           GetCurrentProcess,
                           @Result,
                           0,
                           true,
                           DUPLICATE_SAME_ACCESS)
                            then GetDuplHandle := THandle(-2);
end; { func. GetDuplHandle }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function IsBatchFile(FName: String): Boolean;
var Ext: String;
begin
  IsBatchFile := false;

  Ext := SupCase(JustExtension(Fname));

  if Ext = 'SH' then IsBatchFile := true;
  if Ext = 'BAT' then IsBatchFile := true;
  if Ext = 'CMD' then IsBatchFile := true;
end; { func. IsBatchFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure RaExec(Cmd: String; IsDOS,ShowMsg, IsMailer, ForceInherit: Boolean; var ExitCode: Word; var Success: Boolean);
{ Performs an RaAlike exec }               { These consts get reinitted below }
Const UseHandle: Boolean = False;                { Use handle in DORINFO1.DEF }
      OldStyle: Boolean = False;         { Generate an old style DORINFO1.DEF }
      LeaveHot: Boolean = False;                  { Leave fossil hot on exit? }
      MemorySwap: Boolean = False;                             { Memory swap? }
      WantsChatOff: Boolean = False;                        { WantsChat off ? }
      BaudRateToUse: LongInt = 00;                          { BaudRate to use }
      FreezeTime: Boolean = False;                         { Freeze the timer }
      WriteTagFile: Boolean = False;                       { Write TAGFILE.RA }
      CloseHandle: Boolean = true;            { Only pass handle, don't close }
      DoClearScreen: Boolean = true;                       { Clear the screen }
      DoInherit: Boolean = false;
      DontDoBatches: Boolean = false;              { Dont run with batchfiles }
      DoPauseCom: Boolean = false;      { call pausecom }

var Memory_To_Use: Integer;
    Return: Integer;
    DoSwap: Word;
    Counter: Longint;
    OldTimeStamp: Longint;
    NewTimeStamp: Longint;
    OldDir: String;
    Path: String;
    Bat_F: Text;
    OldHeapEnd: Pointer;
    TempStr: Array[0..255] of CHar;
    HoldVar: String;
    TempCmd: String;

  {$IFDEF WIN32}
    DupHandle    : THandle;
  {$ENDIF}
    SaveElapsed  : Longint;
    SaveLimit    : Longint;
    ExitinfoName : String;
    TmpStr       : String;
    SaveExitTime : String;
    TempExitinfo : ExitinfoRecord;
    Temp_F       : Text;

    SaveMode     : Longint;

procedure DeductTime(S: String);
var Hours, Mins: Word;
begin
{$IFDEF WITH_FULL}
  Hours := FVal(Copy(S, 1, 2));
  Mins := FVal(Copy(S, 4, 2));

  Inc(LineCfg^.OnlineTime, Hours*60);
  Inc(LineCfg^.OnLineTime, Mins*60);

  ChangeTime(-(Hours * 60), true);   { 1 hour=60 minutes, deduct 60 minutes }
  ChangeTime(-(Mins * 60), true);                       { Deduct 60 minutes }
{$ENDIF}
end; { proc. DeductTime }

procedure Replace(Const Zoek_Voor, Vervang_Door:String; Var Str : String);
var MaxLoop: Byte;
    Counter: Byte;
begin
  Maxloop := 120;
  Counter := 00;

  While (Pos(SUpCase(Zoek_Voor), SUpCase(Str))>00) AND (Counter < MaxLoop) do
    begin
      Inc(Counter);
      WordStr.Replace(Zoek_Voor, Vervang_Door, Str);
    end; { while }
end; { proc. Replace }

var ExtraPath   : String;
    FosBatchPath: String;
begin
 {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logRaExec, 'RaExec');
 {$ENDIF}
 ExitCode := 00;
 {$IFDEF WIN32}
   DupHandle := THandle(-1);
 {$ENDIF}

  {-- Initialize the variables ----------------------------------------------}
  UseHandle := false;
  OldStyle := false;
  LeaveHot := false;
  MemorySwap := false;
  WantsChatOff := false;
  BaudRateToUse := 0;
  FreezeTime := false;
  WriteTagFile := false;
  CloseHandle := true;
  DoClearScreen := true;
  DoInherit := false;
  DontDoBatches := false;
  DoPauseCom := TRUE;

  {-- If handle-inheritance is forced, act  on it ---------------------------}
  if ForceInherit then
    DoInherit := TRUE;

  {-- Done initializing, go on with the rest --------------------------------}
 if Pos('*V', SUpCase(Cmd)) > 00 then
   DoClearScreen := false;

 If RunningBBS then
   If (NOT IsDOS) AND (ShowMsg) AND (DoClearScreen) then
                   begin
                     OutputObj^.ClearScreen;
                     WriteLn('`A14:', LangObj^.ralGet(ralLoading));
                   end; { if }

 OutBlockObj^.DumpBlock;
 termObj^.RaCodeStr(Cmd);
 If NOT IsDos then RaLog('>', 'OS shell : '+cmd);

 if GlobalCfg^.RaConfig^.MultiLine then ExitinfoName := ''
       else ExitinfoName := GlobalCfg^.RaConfig^.SysPath;
 ExitinfoName := ExitinfoName + 'exitinfo.bbs';
 BaudRateToUse := FixBaud(LineCfg^.Exitinfo^.Baud);
 {$IFNDEF WINGUI}
   Savemode := LastMode;
 {$ENDIF}


 {-------------------- Convert all variables to values ---------------------}
 TempCmd := Cmd;
 Cmd := '';
 Counter := 01;
 if LineCfg^.ComNoClose then CloseHandle := false;

 {$IFDEF WITH_FULL}
 While Counter <= Length(TempCmd) do
   begin
      if TempCmd[Counter] = '*' then
        begin
          Inc(Counter, 1);

          Case UpCase(TempCmd[Counter]) of
             '!' : FreezeTime := true;
             '#' : WantsChatOff := true;
             '0' : Cmd := Cmd + LineCfg^.CurFilesRecord^.FilePath;
             '1' : Cmd := Cmd + FStr(LineCfg^.Exitinfo^.Userinfo.MsgArea);
             'A' : UseHandle := true;
             'B' : Cmd := Cmd + FStr(BaudRateToUse);
             'C' : Cmd := Cmd + GetEnv('COMSPEC');
             'D' : OldStyle := true;
             'F' : Cmd := Cmd + FirstNameOnly(LineCfg^.Exitinfo^.Userinfo.Name);
             'G' : Cmd := Cmd + FStr(Byte(LineCfg^.AnsiOn));
             'H' : begin
                     { Dont close comport handle, mainly for FOSSIL apps }
                     { Will not close the handle, but handle is not }
                     { officially inheritable }
                     LeaveHot := true;
                     CloseHandle := FALSE;
                   end; { closehandle }
             'V' : ;
             'L' : Cmd := Cmd + LastNameOnly(LineCfg^.Exitinfo^.Userinfo.Name);
             'M' : MemorySwap := true;
             'N' : Cmd := Cmd + FStr(LineCfg^.RaNodeNr);
             'O' : begin
                     if Pos('*O', Cmd) > 00 then
                        BaudRateToUse := Longint(FVal(GetValue('*O', Cmd, false)));
                     Replace('*O'+GetValue('*O', Cmd, false), '', Cmd);         { Override BPS rate }
                   end; { if }
             'P' : if LineCfg^.Exitinfo^.Baud = 0 then Cmd := Cmd + '1'
                        else Cmd := Cmd + FStr(LineCfg^.Modem^.Comport);
             'R' : Cmd := Cmd + FStr(SearchUser(LineCfg^.Exitinfo^.Userinfo.Name));
             'S' : begin
                     CreateTemplateFile(GetValue('*S', Cmd, false));
                     Replace('*S' + GetValue('*S', Cmd, false), '', Cmd);
                   end; { if }
             'T' : Cmd := Cmd + FStr(Min(LineCfg^.Exitinfo^.TimeLimit, 32767));
             'W' : begin
                     { dont close comport handle. Make the handle we pass }
                     { to the other app, an inheritable handle }
                     { include the new handle as a parameter }
                     DoInherit := true;

                     {$IFDEF WIN32}
                        DupHandle := GetDuplHandle(THandle(Apro_Use_Old_Handle));

                        Cmd := Cmd + IntToStr(DupHandle);
                     {$ENDIF}

                     {$IFDEF OS2}
                        Cmd := Cmd + FStr(Apro_Use_Old_Handle);
                     {$ENDIF}

                     CloseHandle := false;

                   end;
             'X' : WriteTagFile := true;
             'Y' : begin
                     { make the handle inheritable, but dont run any batches }
                     { dont close the handle. Dont change the commandline }

                     {$IFDEF WIN32}
                        DupHandle := GetDuplHandle(THandle(Apro_Use_Old_Handle));
                     {$ENDIF}

                     DoInherit := TRUE;
                     DontDoBatches := TRUE;
                     CloseHandle := FALSE;
                     LeaveHot := TRUE;
                     DoPauseCom := FALSE;
                   end; { *Y }
             'Z' : begin
                     { dont close comport, but dont run batchfiles }
                     DoInherit := TRUE;
                     DontDoBatches := true;
                   end; { *Z }
             ')' : begin
                     {-- make sure we dont start batches ------------------}
                     DoInherit := TRUE;
                     DontDoBatches := TRUE;

                     {-- add the command line -----------------------------}
                     if RemScrnObj <> nil then
                       begin
                         {$IFDEF TCPIP}
                         if NOT RemScrnObj^.PipeHandle.ConnectionAlive then
                           begin
                             RaLog('!', 'Running utility with EleMON function, no EleMON connected, program '+
                                    'execution aborted!');
                             EXIT;
                           end; { if }

                         Cmd := Cmd + FStr(RemScrnObj^.PipeHandle.SockHandle);
                         {$ENDIF}
                       end
                         else begin
                                RaLog('!', 'Running utility with EleMON function, no EleMON connected, program '+
                                      'execution aborted!');
                                EXIT; {!!}
                              end; { else }
                   end; { *( }
            else Cmd := Cmd + '*' + TempCmd[Counter];
          end; { case }
        end
          else Cmd := Cmd + TempCmd[Counter];

        Inc(Counter);
   end; { for }
 {$ENDIF}


 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logRaExec, 'Executing (params removed): '+Cmd);
 {$ENDIF}

{$IFDEF WITH_FULL}
 If NOT IsDOS then
  begin
    CreateDorInfoDef(OldStyle, UseHandle, BaudRateToUse);
    CreateDoorSys(BaudRateToUse);
    CreateDoor32Sys(FixBaud(LineCfg^.Exitinfo^.Baud));

    if GetValue('*U', Cmd, false)<>'' then
      MultiLnObj^.Useron^.StatDesc := Under2Norm(GetValue('*U', Cmd, true));

    MultiLnObj^.WriteUserOn(MultiLnObj^.Useron^.StatDesc, uonExternal);
  end; { if not IsDOS }
{$ENDIF}

  {$IFDEF MSDOS}
     Memory_To_use := HIDE_FILE or CHECK_NET or USE_FILE;
     if GlobalCfg^.RaConfig^.UseXMS then
       Memory_To_Use := Memory_To_Use or USE_XMS;
     if GlobalCfg^.RaConfig^.UseEMS then
       Memory_To_Use := Memory_To_Use or USE_EMS;

     if MemorySwap then DoSwap := $ffff else DoSwap := $0000;
  {$ENDIF}

{$IFDEF WITH_FULL}
 TaggingObj^.CloseTagFile;

 if GetfileSize(TagListFileName, 1) = 00 then
  if NOT WriteTagFile then
    EraseFile(TagListFileName);
{$ENDIF}

 SaveElapsed := LineCfg^.Exitinfo^.Userinfo.Elapsed;
 SaveLimit := LineCfg^.Exitinfo^.TimeLimit;

 if LineCfg^.Exitinfo^.Baud <> 00 then
 {$IFDEF WITH_FULL}
   if ComObj <> nil then
 {$ENDIF}
   if NOT LeaveHot then
    if NOT IsDos then
     if NOT IsMailer then
      begin
        {$IFDEF WITH_DEBUG}
          DebugObj.DebugLog(logRaExec, 'Using FOSSIL (or WIN32), closing comport...');

          if CloseHandle then
            DebugObj.DebugLog(logRaExec, 'ExecFunc: Closing Handle: TRUE')
             else DebugObj.DebugLog(logRaExec, 'ExecFunc: Closeing Handle: FALSE');
        {$ENDIF}

        if CloseHandle then CloseComport;
      end; { if }

 if DoPauseCom then
   PauseCom;

 {$IFDEF WITH_FULL}
 WriteExitInfo(LineCfg^.Exitinfo^);                     { Write Exitinfo.BBS }
 {$ENDIF}

 if NOT GlobalCfg^.RaConfig^.MultiLine then
   OldTimeStamp := GetPackedFileTime(GlobalCfg^.RaConfig^.SysPath + 'exitinfo.bbs')
    else OldTimeStamp := GetPackedFileTime('exitinfo.bbs');

 LineCfg^.CheckInActivity:=false;        { Disable checking of inactivity during shell }

 Path := FirstWord(Cmd, defExtractWord, true);
 GetDir(0, OldDir);

 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logRaExec, 'Now really executing: Path="'+Path+'" Cmd="'+Cmd+'"');
 {$ENDIF}

 if LangObj^.RalFile <> nil then
   begin
     LangObj^.RalFile^.Close;
     if LangObj^.RalFile^.IOResult>00 then ;
   end; { if }

{$IFNDEF WIN32}
{$IFDEF MSDOS}
 {-- MS-DOS ------------------------------------------------------------------}
 If MemorySwap then
  begin
    {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logRaExec, 'Executing with memory swap ...');
    {$ENDIF}
    Return := Do_Exec(Path, Cmd, Memory_To_Use, DoSwap, false);

   ExitCode := Lo(Return);
   Success := false;

   Case Return of
     $0101 : RALog('!', 'SwapError: No space for swapping ...');
     $0102 : RALog('!', 'SwapError: Not enough memory to swap ...');
     $0200 : RALog('!', 'SwapError: Program file not found');
     $0201 : RALog('!', 'SwapError: Invalid drive for program file');
     $0202 : RALog('!', 'SwapError: Invalid path for program file');
     $0203 : RALog('!', 'SwapError: Invalid name for program file');
     $0204 : RALog('!', 'SwapError: Invalid driveletter');
     $0205 : RALog('!', 'SwapError: Path too long..');
     $0206 : RALog('!', 'SwapError: Drive not ready');
     $0207 : RALog('!', 'SwapError: COMMAND.COM not found!');
     $0208 : RALog('!', 'SwapError: Error allocating temporary buffer');
     $0400 : RALog('!', 'SwapError: Error allocating environment buffer');
     $0500 : RALog('!', 'SwapError: Swap requested, but prep_swap not called/returned an error!!');
     $0501 : RALog('!', 'SwapError: MCBS don''t match expected setup!');
     $0502 : RALog('!', 'SwapError: Error while swapping out ...');
     $0600 : RALog('!', 'SwapError: Redirection syntax error');
      else Success := true;
   End; { Case }
  End  { If Swap }
   else
        begin;
          {$IFDEF WITH_DEBUG}
             DebugObj.DebugLog(logRaExec, 'Executing without memory swap!');
          {$ENDIF}
          OldHeapEnd := HeapEnd;
          HeapEnd := HeapPtr;
          SetMemTop(HeapEnd);

          SwapVectors;
          DOS.Exec(Path, Cmd);
          SwapVectors;

          ExitCode := Dos.DosExitCode;

          HeapEnd := OldHeapEnd;
          SetMemTop(HeapEnd);
        end; { Don't swap }
{$ELSE}
  {-- OS/2 -------------------------------------------------------------------}
        begin
          {$IFDEF WITH_DEBUG}
             DebugObj.DebugLog(logRaExec, 'Executing without memory swap! (OS/2)');
             if IsBatchFile(Path) then
               DebugObj.DebugLog(logRaExec, 'Is a batchfile!')
                else DebugObj.DebugLog(logRaExec, 'Not a batch file');
          {$ENDIF}

          {$IFNDEF ELEUNIX}
            {$IFDEF VirtualPascal}
               ExecFlags := efSync;
            {$ENDIF}

            SwapVectors;

            if IsBatchFile(Path) then
              DOS.Exec(GetEnv('COMSPEC'), ' /C ' + Path + #32 + Cmd)
               else Dos.Exec(Path, Cmd);

            ExitCode := Dos.DosExitCode;
            SwapVectors;
          {$ELSE}
            SwapVectors;

            Dos.Exec(Path, Cmd);

            ExitCode := Dos.DosExitCode;
            SwapVectors;
          {$ENDIF}

        end; { OS/2 Exec }
{$ENDIF}
{$ELSE}
 {-- Win32 -------------------------------------------------------------------}
  if FileExist(GlobalCfg^.RaConfig^.SysPath + 'TMP'+FStr(LineCfg^.RaNodeNr)+'.BAT') then
    EraseFile(GlobalCfg^.RaConfig^.SysPath + 'TMP'+FStr(LineCfg^.RaNodeNr)+'.BAT');

  Assign(Bat_F, GlobalCfg^.RaConfig^.SysPath + 'TMP'+FStr(LineCfg^.RaNodeNr)+'.BAT');
  {$i-}
     System.ReWrite(Bat_F);
     WriteLn(Bat_F, '@echo off');
     WriteLn(Bat_F, 'rem ** Batch file automaticly created by '+FullProgName);
     WriteLn(Bat_F, 'rem ** You need to call an FOSSIL driver from this batchfile');
     WriteLn(Bat_F, 'rem ** SFOS.BAT is the start batch, UFOS.BAT is to unload it');
     WriteLn(Bat_F, 'rem ** Both files must reside in the same dir where EXITINFO.BBS is created');
     WriteLn(Bat_F);

     ExtraPath := GlobalCfg^.RaConfig^.SysPath;
     FosBatchPath := GlobalCfg^.RaConfig^.SysPath;
     if GlobalCfg^.RaConfig^.MultiLine then ExtraPath := '';

     if (NOT (DontDoBatches)) then
       begin
         if CloseHandle then
           begin
             if (NOT LineCfg^.LocalLogon) AND (NOT IsDos) then
               begin
                 if NOT FileExist(FosBatchPath + 'SFOS.BAT') then
                   FosBatchPath := '';

                 {-------------- Include the contents of SFOS.BAT in here ---------}
                 Assign(Temp_F, FosBatchPath + 'SFOS.BAT');
                 {$i-}
                   System.Reset(Temp_F);
                   if IoResult = 0 then
                     begin
                       While NOT EOF(Temp_F) do
                         begin
                           ReadLn(Temp_F, TmpStr);
                           WriteLn(Bat_F, TmpStr);
                         end; { while }
                       Close(Temp_F);
                     end; { if }
                 {$i+}
                 if IoResult > 0 then ;
               end; { if }


             WriteLn(Bat_F, GlobalCfg^.RaConfig^.Syspath + 'GT$RR'+FStr(LineCfg^.RaNodeNr)+'.EXE ', LineCfg^.RaNodeNr, #32,
                        Copy(Path+#32+Cmd, 1, 250));

             if (NOT LineCfg^.LocalLogon) AND (NOT IsDos) then
               begin
                 if NOT FileExist(FosBatchPath + 'UFOS.BAT') then
                   FosBatchPath := '';

                 {-------------- Include the contents of UFOS.BAT in here ---------}
                 Assign(Temp_F, FosBatchPath + 'UFOS.BAT');
                 {$i-}
                    System.Reset(Temp_F);
                    if IoResult = 0 then
                      begin
                        While NOT EOF(Temp_F) do
                          begin
                            ReadLn(Temp_F, TmpStr);
                            WriteLn(Bat_F, TmpStr);
                          end; { while }
                        Close(Temp_F);
                      end; { if }
                  {$i+}
                  if IoResult > 0 then ;
               end; { if }
           end { if }
             else WriteLn(Bat_F, Copy(Path + #32 + Cmd, 1, 255));
       end { if NOT DontDoBatches }
         else WriteLn(Bat_F, Copy(Path + #32 + Cmd, 1, 255));

     WriteLn(Bat_F, 'CLS');

     Close(Bat_F);
   {$i+}
   if IOResult>00 then ;

  FillChar(TempStr, SizeOf(TempStr), #00);
  WriteGetErr;

    if NOT DoInherit then
      begin
        HoldVar := ' /C ' + GlobalCfg^.RaConfig^.Syspath + 'TMP' + FStr(LineCfg^.RaNodeNr) + '.BAT';
        Exec(GetEnv('COMSPEC'), HoldVar);
      end
        else begin
               if (DoRunWithFossil(Path)) AND (Leavehot) then
                 ExecuteDosFosProgram(Path + #32 + Cmd)
                   else Exec(Path, Cmd);

               {-- Now close the duplicated handle ----------------------------}
               if DupHandle <> THandle(-1) then
                 Windows.CloseHandle(DupHandle);
             end; { if }

    ExitCode := Dos.DosExitCode;

  ExitCode := GetWinErrorCode;
{$ENDIF}

 {$IFNDEF WINGUI}
   if SaveMode <> LastMode then
    TextMode(Savemode);
 {$ENDIF}

 ChDir(NoBackSlash(OldDir));
 LangObj^.ReadRalFile(langObj^.SaveRalName);        { Just assume it goes correct }


 {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logRaExec, 'RaExec: Returning from DOS / Shell');
 {$ENDIF}

 {$IFDEF WITH_FULL}
   {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logRaExec, 'Using FOSSIL, Ismailer='+Bool2Str(IsMailer));
     DebugObj.DebugLog(logRaExec, 'LeaveHot ='+Bool2Str(LeaveHot));
     DebugObj.DebugLog(logRaExec, 'Exitinfo^.Baud='+FStr(FixBaud(LineCfg^.Exitinfo^.Baud)));
     DebugObj.DebugLog(logRaExec, 'LockModem='+Bool2Str(LineCfg^.Modem^.LockModem));
   {$ENDIF}

   If (LineCfg^.Exitinfo^.Baud<>00) then
    if NOT IsMailer then
     if NOT LeaveHot then
      if NOT IsDos then
       begin
         if CloseHandle then
           InitFossil(LineCfg^.Modem^.LockModem, LineCfg^.Exitinfo^.Baud);
       end; { if }
  ResumeCom;
 {$ENDIF}

{$IFDEF WITH_FULL}
 contrObj^.SetTimeOut;                            { Reset inactivity checks }
 If (NOT LeaveHot) AND (NOT IsMailer) then Support.Delay(500); { Let fossil-port come to sense ;) }

 contrObj^.SetTimeOut;
 LineCfg^.CheckInActivity:=True;           { Check for inactivity during shell? NO! :) }

 if NOT IsMailer then
  if GlobalCfg^.RaConfig^.MultiLine then
   if NOT FileExist('exitinfo.bbs') then
    if NOT IsDos then
        begin
          RaLog('!', 'Unable to open EXITINFO.BBS, terminating call');
          {$IFDEF WITH_DEBUG}
            DebugObj.DebugLog(logRaExec, 'Fatal Error: Cannot open EXITINFO.BBS');
          {$ENDIF}
          HangUp;
        end; { if }

 if NOT IsMailer then
  if NOT GlobalCfg^.RaConfig^.MultiLine then
   if NOT FileExist(GlobalCfg^.RaConfig^.SysPath + 'exitinfo.bbs') then
    if NOT IsDos then
        begin
          RaLog('!', 'Unable to open EXITINFO.BBS, terminating call (not ml)');
          {$IFDEF WITH_DEBUG}
            DebugObj.DebugLog(logRaExec, 'Fatal Error: Cannot open EXITINFO.BBS');
          {$ENDIF}
          HangUp;
        end; { if }

{$IFDEF WITH_FULL}
 if NOT IsMailer then
   begin
     ReadExitinfo(TempExitinfo);                 { Update the exitinfo record }

     {$IFDEF WITH_DEBUG}
     (************
        RaLog('!', '>------------------- RETURNED ------------------------');
        RaLog('!', '(DEBUG): Temp TimeLeft    = '+FStr(TempExitinfo.TimeLimit));
        RaLog('!', '(DEBUG): Secs Remaining   = '+FStr(TimeRemain));
        RaLog('!', '(DEBUG): Current Seconds  = '+FStr(CurrentTime));
        RaLog('!', '(DEBUG): Secs snce logged = '+FStr(TimeInfo.StartTime));
        RaLog('!', '(DEBUG): Mins from time   = '+FStr(MinsFromTime(Exitinfo^.TimeOfCreation)));
        RaLog('!', '(DEBUG): Exitinfo TimeStmp= '+TempExitinfo.TimeOfCreation);
        RaLog('!', '(DEBUG): Calculate time?  = '+Bool2Str(OldTimeStamp = TempExitinfo.TimeOfCreation));
        RaLog('!', '>------------------- RETURNED -----------------------<');
      *************)
    {$ENDIF}

    if NOT GlobalCfg^.RaConfig^.MultiLine then
      NewTimeStamp := GetPackedFileTime(GlobalCfg^.RaConfig^.SysPath + 'exitinfo.bbs')
       else NewTimeStamp := GetPackedFileTime('exitinfo.bbs');

     if OldTimeStamp <> NewTimeStamp then
      with LineCfg^ do
       begin
         ReadExitinfo(Exitinfo^);          { Read Exitinfo^.BBS newrecord }
         GetLevelLimitsInfo(Exitinfo^.Userinfo.Security, false, false);
         ChangeTime(00, true);
         contrObj^.SetStartTime;
       end { if }
        else with LineCfg^ do
             begin
               ReadExitinfo(Exitinfo^);
               ChangeTime(-Max(Pred(MinsFromTime(Exitinfo^.TimeOfCreation)), 0), true);
               GetLevelLimitsInfo(Exitinfo^.Userinfo.Security, false, false);
               contrObj^.SetStartTime;
               ChangeTime(00, true);
             end; { Calculate the new time ourself }

   end; { if }
{$ENDIF}

 if NOT GlobalCfg^.RaConfig^.MultiLine then
     EraseFile(GlobalCfg^.RaConfig^.Syspath + 'exitinfo.bbs')
      else EraseFile('exitinfo.bbs');

{!! If WriteTagFile then DoReadTagFile;}
 TaggingObj^.OpentagFile;

 If WantsChatOff then
  begin
    LineCfg^.Exitinfo^.WantChat := False;
    {$IFDEF WINGUI}
      Form1.WantChatTrayIcon.Active := false;
    {$ENDIF}
  end; { if }

 If FreezeTime then
  begin
    LineCfg^.Exitinfo^.Userinfo.Elapsed := SaveElapsed;
    LineCfg^.Exitinfo^.TimeLimit := SaveLimit;
    contrObj^.SetStartTime;
  end; { if FreezeTime }
 contrObj^.TimeRemain;                                    { Update counter }

 MultiLnObj^.UserOn^.StatDesc := '';
 MultiLnObj^.WriteUserOn(MultiLnObj^.Useron^.StatDesc, uonBrowsing);

 if (NOT IsDos) then
  if DoClearScreen then OutputObj^.ClearScreen;
 StatusDisplay(99, false);                                 { Show new time }
 if IsDOS then InputObj^.PutInBuffer(#255, false);  { Menu item will be done automaticly }

 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logRaExec, 'End of RaExec: '+FStr(ExitCode));
 {$ENDIF}
{$ENDIF}
end; { proc. RaExec }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WINGUI}
{$IFDEF DELPHI16}
function WinExecAndWait( Path: Pchar; Visibility: word; KeepWait: Boolean ): word;
{$ELSE}
function WinExecAndWait( Path: Pchar; Visibility: word; KeepWait: Boolean ): longint;
{$ENDIF}

  var
    InstanceID: THandle;
  {$IFDEF DELPHI16}
    Msg: TMsg;
  {$ELSE}
    Msg: TMsg;
    bStat: BOOL;
    pi: TProcessInformation;
    si: TStartupInfo;
    iExit: DWORD;
    bExit: boolean;
  {$ENDIF}

  begin
  SetLastError(00);

  {$IFDEF DELPHI16}
  InstanceID := WinExec( Path, Visibility );
  {$ELSE}
  FillMemory( @si, sizeof( TStartupInfo ), 0 );

  with si do
    begin
    cb := sizeof( TStartupInfo );
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := Visibility;
    end;

  bStat := CreateProcess
    (
    nil,  // address of module name
    Path, // address of command line
    nil,  // address of process security attributes
    nil,  // address of thread security attributes
    true,  // new process inherits handles
    NORMAL_PRIORITY_CLASS,  // creation flags
    nil,  // address of new environment block
    nil,  // address of current directory name
    si, // address of STARTUPINFO
    pi  // address of PROCESS_INFORMATION
    );
  {$ENDIF}

  {$IFDEF DELPHI16}
  if ( InstanceID < 32 ) then { a value less than 32 indicates an Exec error }
    Result := InstanceID
  else
  {$ENDIF}
    repeat
      while ( PeekMessage( Msg, 0, 0, 0, PM_REMOVE ) ) do
        begin
        if ( Msg.Message = WM_QUIT ) then
          Halt( Msg.wParam );
        TranslateMessage( Msg );
        DispatchMessage( Msg );
        end;
  {$IFDEF DELPHI16}
    until GetModuleUsage( InstanceID ) = 0;
    Result := 0;
  {$ELSE}
      GetExitCodeProcess( pi.hProcess, iExit );
      bExit := iExit <> STILL_ACTIVE;

      if NOT KeepWait then bExit := true;   { Force to abort }
    until bExit;
    CloseHandle( pi.hProcess );
    CloseHandle( pi.hThread );
    Result := iExit;

  {$ENDIF}
  end;
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WIN32}
procedure ExecuteDosFosProgram(CmdLine: String);
{ We only support NT, Win9x should not be used as a server platform }
const
  XTRN_IO_BUF_LEN = 5000;
  SBBSEXEC_MODE_FOSSIL = 0;                          { Only mode we support }
  LoopBeforeYield = 10;                          { should be user definable }

var Temp_F       : pFileObj;
    CharObj      : CharArrayObj;
    CommandParams: String;
    TmpStr       : String;
    TmpBuf       : Array[0..XTRN_IO_BUF_LEN - 1] of Char;
    Hangup_Event : THandle;                          { Event to hangup door }
    ReadSlot     : THandle;                             { Mailslot for data }
    WriteSlot    : THandle;
    ProcessInfo  : TProcessInformation;
    StartupInfo  : TStartupInfo;
    TermStatus   : ULONG;
    InUsed       : Longint;               { used by EleCOMs GetBufferStatus }
    TmpLong      : Longint;               { used by EleCOMs GetBufferStatus }
    Counter      : Longint;
    DosXtrnString: String;
    BufPtr       : Longint;
    SlotCounter  : Longint;
begin
  {-- Initialize DOS xtrn string (Syncs Fossil startkicker) ----------------}
  DosXtrnString := GlobalCfg^.RaConfig^.SysPath + 'dosxtrn.exe';

  {-- Initialise variables -------------------------------------------------}
  ReadSlot := INVALID_HANDLE_VALUE;
  WriteSlot := INVALID_HANDLE_VALUE;
  Hangup_Event := INVALID_HANDLE_VALUE;

  {-- Initialize the string array first ------------------------------------}
  CharObj.Init(1024 * 10);
  { 01 } CharObj.AddStringLn(CmdLine);

  {-- we could add environment variables here, we wont ---------------------}
  {-- First create the temporary file --------------------------------------}
  New(Temp_F, Init);

  Temp_F^.Assign(GlobalCfg^.RaConfig^.SysPath + 'ebsyn' + FStr(LineCfg^.RaNodeNr) + '.env');
  Temp_F^.FileMode := ReadWriteMode + DenyNone;
  Temp_F^.Create(1);
  Temp_F^.BlkWrite(CharObj.TxtArr^, StrLen(CharObj.TxtArr^) + 01);
  Dispose(Temp_F, Done);
  CharObj.Done;

  {-- Create exec_mutex ----------------------------------------------------}
  { Synchronet creates an "exec_mutex" here (unnamed), not sure why.        }
  { Guess it has something to do with not two things executing at the same  }
  { time or something. I dont create it ------------------------------------}
  { ------------------------------------------------------------------------}

  {-- Create an hangup event, we set this if we drop carrier ---------------}
  TmpStr := 'sbbsexec_hungup' + FStr(LineCfg^.RaNodeNr) + #0;
  HangUp_Event := CreateEvent(nil,                    { Security attributes }
                              true,           { Flag for manual-reset event }
                              false,               { flag for initial state }
                              @TmpStr[1]);
  if Hangup_Event = INVALID_HANDLE_VALUE then
    begin
      { fail }
      RaLog('!', 'Unable to create hangup event');
      EXIT;
    end; { if }

  {-- Create a mailslot for reading ----------------------------------------}
  TmpStr := '\\.\mailslot\sbbsexec\rd' + FStr(LineCfg^.RaNodeNr) + #0;
  ReadSlot := CreateMailSlot(@TmpStr[1],
                             SizeOf(TmpBuf),         { Maximum message size }
                             0,                              { read timeout }
                             nil);          { pointer to security structure }
  if ReadSlot = INVALID_HANDLE_VALUE then
    begin
      { fail }
      RaLog('!', 'Unable to create read mailslot');
      CloseHandle(Hangup_Event);
      EXIT;
    end; { if }


  {-- We dont create the write slot until it is signalled that theres data -}
  {-- for us by the FOSSIL driver ------------------------------------------}

  {-- Lets build the actual commandline we are going to pass to dosxtrn ----}
  CommandParams := DosXtrnString + #32 +
                  GlobalCfg^.RaConfig^.SysPath + 'ebsyn' + FStr(LineCfg^.RaNodeNr) + '.env' + #32 +
                  'NT ' + FStr(LineCfg^.RaNodeNR) + #32 +
                   FStr(SBBSEXEC_MODE_FOSSIL) + #32 +
                   FStr(LoopBeforeYield) + #0;

  {-- initialize the startup info ------------------------------------------}
  FillChar(StartupInfo, SizeOf(tStartUpInfo), 0);
  StartupInfo.CB := SizeOf(tStartUpInfo);

  {-- Now actually execute the program -------------------------------------}
  if NOT CreateProcess(nil,                     { pointer to name of module }
                       @CommandParams[1],                  { actual program }
                       nil,                  { ptr to process security attr }
                       nil,                   { ptr to thread security attr }
                       true,                      { handle inheritance flag }
                       CREATE_NEW_CONSOLE AND              { creation flags }
                         CREATE_SEPARATE_WOW_VDM,
                       nil,              { Pointer to new environment block }
                       nil,             { Pointer to current directory name }
                       {$IFDEF FPC}@{$ENDIF}StartupInfo,                { pointer to startupinfo }
                       {$IFDEF FPC}@{$ENDIF}ProcessInfo) then
    begin
      { fail }
      CloseHandle(Hangup_Event);
      CloseHandle(ReadSlot);

      Ralog('!', 'Error while executing: ' + CommandParams);
      EXIT;
    end; { if }

  {-- Loop till something happens ------------------------------------------}
  Counter := 0;
  while (true) do
    begin
      {-- Increase counter -------------------------------------------------}
      Inc(Counter);

      {-- Check carrier ----------------------------------------------------}
      if NOT ComObj^.Com_Carrier then
        begin
          SetEvent(Hangup_Event);
          Sleep(5000);
          { we should wait a few seconds, and then terminate the process }
        end; { if }

      {-- Check if the process is still running ----------------------------}
      if GetExitCodeProcess(ProcessInfo.hProcess,
                            TermStatus) then
        begin
          {-- If the process terminated, we terminate as well --------------}
          if TermStatus <> STILL_ACTIVE then
            BREAK;

          {-- now check if theres data available to write to it ------------}
          if ComObj^.Com_CharAvail then
            begin
              {-- reset the counter ----------------------------------------}
              Counter := 0;

              {-- get the inbound buffer -----------------------------------}
              ComObj^.Com_GetBufferStatus(TmpLong, TmpLong,
                                          Inused, TmpLong);

              {-- make sure we dont request more than we can handle --------}
              if (InUsed + 9) > XTRN_IO_BUF_LEN then
                InUsed := XTRN_IO_BUF_LEN - 10;

              {-- retrieve the data ----------------------------------------}
              ComObj^.Com_ReadBlock(TmpBuf[0], InUsed, TmpLong);

              {-- now check if the write handle is valid, if not create it -}
              if WriteSlot = INVALID_HANDLE_VALUE then
                begin
                  TmpStr := '\\.\mailslot\sbbsexec\wr' +
                              FStr(LineCfg^.RaNodeNr) + #0;

                  WriteSlot := CreateFile(@TmpStr[1],
                                          GENERIC_WRITE,        { Filemode }
                                          FILE_SHARE_READ,  { sharing mode }
                                          nil,
                                          OPEN_EXISTING,
                                          FILE_ATTRIBUTE_NORMAL,
                                          0);
                end; { if }

              {-- now send the data ----------------------------------------}
              {-- if the above call failed, this data get lost -------------}
              if WriteSlot <> INVALID_HANDLE_VALUE then
                begin
                  WriteFile(WriteSlot,                 { handle to write to }
                            TmpBuf,                                { buffer }
                            TmpLong,                       { bytes to write }
                            ULONG(InUsed),     { actual nr of bytes written }
                            nil);                              { overlapped }
                end; { if }
            end; { if }

          {-- Check the number of messages ---------------------------------}
          BufPtr := 0;
          TmpLong := 0;

          if GetMailSlotInfo(ReadSlot,                             { handle }
                             nil,                        { max message size }
                             ULONG(InUsed),          { size of next message }
                             @TmpLong,            { Number of messages left }
                             nil) then                       { read timeout }
            if TmpLong > 0 then
              begin
                {-- if there are more than 200 messages, get the first 200 -}
                if TmpLong > 1500 then
                  TmpLong := 1500;

                {-- Reset usage count --------------------------------------}
                InUsed := 0;

                {-- now check if theres data to read from it ---------------}
                for SlotCounter := 01 to TmpLong do
                 if ReadFile(ReadSlot,                             { handle }
                             TmpBuf[BufPtr],                       { buffer }
                             XTRN_IO_BUF_LEN - BufPtr, { mx data we can get }
                             ULONG(INUSED),{number of bytes we actually got }
                             nil) then                         { overlapped }
                   begin
                     Inc(BufPtr, InUsed);

                     {-- make sure buffer wont overflow --------------------}
                     if BufPtr >= (XTRN_IO_BUF_LEN - 10) then
                       BREAK;
                   end; { if }
              end; { if }


            if Bufptr > 0 then
              begin
                {-- reset the counter --------------------------------------}
                Counter := 0;

                ComObj^.Com_SendBlock(TmpBuf[0],                     { data }
                                      BufPtr,              { bytes to write }
                                      InUsed);           { actually written }

                {-- we try to correct some stupid stuff if this door uses --}
                {-- char per char ------------------------------------------}
                if TmpLong > 40 then
                  Sleep(100);
              end; { if }
        end; { if }

        {-- delay if nothing happens ---------------------------------------}
        if Counter > 0 then Sleep(100);
    end; { while }

  {-- and clean up ---------------------------------------------------------}
  CloseHandle(ProcessInfo.hProcess);
  CloseHandle(ProcessInfo.hThread);
  CloseHandle(Hangup_Event);
  CloseHandle(ReadSlot);
  if WriteSlot <> INVALID_HANDLE_VALUE then
    CloseHandle(WriteSlot);
end; { proc. ExecuteDosFosProgram }
{$ENDIF}
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WIN32}
function DoRunWithFossil(CmdLine: String): Boolean;
var Return: Longint;
begin
  {-- default to native executable -----------------------------------------}
  DoRunWithFossil := FALSE;

  {$IFDEF Win32}
    if LineCfg^.Exitinfo^.Baud = 0 then EXIT;

    if (FileExist(GlobalCfg^.RaConfig^.Syspath + 'sbbsexec.dll')) then
     if (FileExist(GlobalCfg^.RaConfig^.Syspath + 'dosxtrn.exe')) then
       begin
         CmdLine := CmdLine + #0;

         if GetBinaryType(@CmdLine[1], ULONG(Return)) then
          if (Return = SCS_DOS_BINARY) OR
              (Return = SCS_PIF_BINARY) then DoRunWithFossil := TRUE;
       end; { if }
  {$ENDIF}
end; { func. DoRunWithFossil }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
