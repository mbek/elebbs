unit Question;
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
** Question (Q-A) script routines for EleBBS.
**
** Copyright (c) 1996 - 2000 by Maarten Bekers
**
**
** Created : 08-Sep-1996
** Last update : 10-Jul-2000
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses QaSys, CfgRec, QaCfg;


Type QuestObj = Object(QaSysObj)
        qaCfgFileInfo: Array[1..5] of ^CfgFileRecord;
        DoResetLines: Boolean;

        procedure Process(MiscData: String;
                          ResetLines: Boolean;
                          StartLabel: String);   { Load Q-A file and execute }
     end; { QuestObj }

function CommandsHook(var   SplitUpStr  : SplitStringType;
                      var   SplitUpLong : SplitLongType;
                      const CommandStr  : MacroStr;
                      const CommandCount: Longint;
                            QObj        : Pointer): Boolean;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses {$IFDEF WINGUI}
      Win_Main,
     {$ENDIF}
      MultiLn,
       Global,
        Multi,
        {$IFNDEF ISCGI}
         Control,
          LineEd,
           InOut_U,
            Input_U,
             DispAns,
              Support,
               ExecFunc,
                EleMenu,
                 Sound_U,
                  StatusB,
                   Limit_U,
                    Crt,
                     OutBlock,
         {$ENDIF}
          Terminal,
           GenFile,
           ElLog_U,
            Cases,
             Mail,
              LongStr,
               BitWise,
                WordStr,
                 CentrStr,
                  StUtils,
                   Flags,
                    UsrCodes,
                     JDates,
                      StrPath,
                       RAL,
                        FileROut,
                         objDec;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Quest_SliceHook(var SliceCount: longint);
var MaxSlice: Longint;
begin
  Inc(SliceCount);
  if MultiTasker = OS2 then MaxSlice := 250
     else MaxSlice := 155;

  if SliceCount > MaxSlice then
    begin
      DoSlice;
      SliceCount := 00;
    end; { if }
end; { proc. SliceHook }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Quest_AbortFunc: Boolean;
begin
  Quest_AbortFunc := ProgTerminated;

  {$IFNDEF ISCGI}
    if NOT ProgTerminated then
      if NOT contrObj^.CheckAll then
        Quest_AbortFunc := true;
  {$ENDIF}
end; { func. Quest_AbortFunc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Quest_LogProc(const Sev: Char; const OutStr: MacroStr);
begin
  RaLog(Sev, OutStr);
end; { proc. Quest_LogProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function OpenCfgFile(FileName: String): String;
var TempStr: String;
begin
  OpenCfgFile := FileName;

  if Pos('/', FileName) > 0 then EXIT;
  if Pos('\', FileName) > 0 then EXIT;

  TempStr := OpenRaFile(FileName);

  if NOT FileExist(TempStr) then
   if (Pos('\', TempStr) = 0) AND (Pos('/', TempStr) = 0) then
     begin
       if FileExist(GlobalCfg^.RaConfig^.SysPath + TempStr) then
          TempStr := GlobalCfg^.RaConfig^.Syspath + TempStr;

       if FileExist(GlobalCfg^.RaConfig^.FileBasePath + TempStr) then
          TempStr := GlobalCfg^.RaConfig^.FileBasepath + TempStr;

       if FileExist(GlobalCfg^.RaConfig^.MsgBasePath + TempStr) then
          TempStr := GlobalCfg^.RaConfig^.MsgBasepath + TempStr;

       if FileExist(GlobalCfg^.RaConfig^.TempCdFilepath + TempStr) then
          TempStr := GlobalCfg^.RaConfig^.TempCdFilepath + TempStr;

       if FileExist(GlobalCfg^.RaConfig^.SemPath + TempStr) then
          TempStr := GlobalCfg^.RaConfig^.Sempath + TempStr;

       if FileExist(GlobalCfg^.RaConfig^.MenuPath + TempStr) then
          TempStr := GlobalCfg^.RaConfig^.Menupath + TempStr;

       if FileExist(GlobalCfg^.RaConfig^.NodeListPath + TempStr) then
          TempStr := GlobalCfg^.RaConfig^.NodeListpath + TempStr;

       if FileExist(GlobalCfg^.RaConfig^.AttachPath + TempStr) then
          TempStr := GlobalCfg^.RaConfig^.AttachPath + TempStr;

       if FileExist(GlobalCfg^.RaConfig^.TextPath + TempStr) then
          TempStr := GlobalCfg^.RaConfig^.Textpath + TempStr;

       if FileExist(GlobalCfg^.RaConfig^.RipIconpath + TempStr) then
          TempStr := GlobalCfg^.RaConfig^.RipIconpath + TempStr;
    end; { if }

  OpenCfgFile := TempStr;
end; { func. OpenCfgFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CommandsHook(var   SplitUpStr  : SplitStringType;
                      var   SplitUpLong : SplitLongType;
                      const CommandStr  : MacroStr;
                      const CommandCount: Longint;
                            QObj        : Pointer): Boolean;
var WorkStr    : String;
    Command    : String;
    ExitCode   : Word;                               { Needed for "EXEC" }
    SaveLocal  : Boolean;
    TempBool   : Boolean;
    Goodkeys   : CharSet;
    TempCH     : Char;

    Variable1,
    Variable2  : String;

    Counter    : Longint;
begin
  OutputObj^.ResetLines(01);
  CommandsHook := false;

  if (CommandStr[1] in [';', ':']) then EXIT;
  Command := SupCase(ExtractWord(CommandStr, 1, defExtractWord - [';'], false, false));


  With QaSysObj(QObj^).Qinfo^ do
    Case Command[1] of
       {------------------------------- 'A' --------------------------------}
       'A' : Case Command[2] of
               {----------------------------- 'AS' ------------------------------}
               'S' : if Command = 'ASK' then
                       begin
                         { Syntax:   Ask <len> <var num> [YES|NO]}
                         if CommandCount >= 3 then
                            begin
                             if (SplitUpLong[1] in [0..255]) then
                               if (SplitUpLong[2] in [1..qaMaxAnswers]) then
                                 begin
                                   WorkStr := '';

                                   {$IFNDEF ISCGI}
                                     GetString(WorkStr,
                                               SplitUpLong[1],
                                               [#32..#254],
                                               Capitalize,
                                               false, SupCase(SplitUpStr[3]) = 'YES', false);

                                     Answers^.Put(SplitUpLong[2], WorkStr);
                                     OutputProc(CrLf);
                                   {$ENDIF}
                                 end; { if }
                           end; { if }
                       end; { 'ASK' }
             end; { 'A' }

       {------------------------------- 'C' --------------------------------}
       'C' : Case Command[2] of
               {---------------------------- 'CF' -------------------------------}
               'F' : if Command = 'CFG_FILEOPEN' then
                       begin
                         { Syntax: CFG_FILEOPEN <file#> <filename> <type of file> }
                         if CommandCount >= 3 then
                           begin
                             if SplitUpStr[1, 1] = '#' then
                               SplitUpLong[1] := FVal(Answers^.Get(FVal(Copy(SplitUpStr[1], 2, 255))));

                             if SplitUpLong[1] in [1..qaMaxCfgFiles] then
                               begin
                                 New(QuestObj(Qobj^).qaCfgFileInfo[SplitUpLong[1]]);

                                 With QuestObj(Qobj^).qaCfgFileInfo[SplitUpLong[1]]^ do
                                   begin
                                     {-- Convert them if they are var refs --}
                                     if SplitUpStr[2, 1] = '#' then
                                       SplitUpStr[2] := Answers^.Get(FVal(Copy(SplitUpStr[2], 2, 255)));

                                     if SplitUpStr[3, 1] = '#' then
                                       SplitUpStr[3] := Answers^.Get(FVal(Copy(SplitUpStr[3], 2, 255)));

                                     {-- Initializes variables --------------}
                                     FileName := OpenCfgFile(SplitUpStr[2]);
                                     FileHandle := nil;
                                     FileSort := StrToFileSort(SplitUpStr[3]);
                                     ErrorCode := 0;

                                     GetMem(BufData, qaCfgFileSize[FileSort].BufSize);

                                     {-- Open the file ----------------------}
                                     New(FileHandle, Init);
                                     FileHandle^.Assign(FileName);
                                     FileHandle^.FileMode := ReadWriteMode + DenyNone;

                                     if NOT FileHandle^.Open(1) then
                                       ErrorCode := 02;
                                   end; { with }
                               end; { if }
                           end; { if }
                       end { 'CFG_FILEOPEN' }

                else if Command = 'CFG_FILEERROR' then
                       begin
                         { Syntax: CFG_FILEERROR <file#> <variable#> }
                         if CommandCount >= 2 then
                           begin
                             if SplitUpStr[1, 1] = '#' then
                               SplitUpLong[1] := FVal(Answers^.Get(FVal(Copy(SplitUpStr[1], 2, 255))));

                             if SplitUpLong[1] in [1..qaMaxCfgFiles] then
                               begin
                                 if SplitUpLong[2] in [1..qaMaxAnswers] then
                                   begin
                                     With QuestObj(Qobj^).qaCfgFileInfo[SplitUpLong[1]]^ do
                                       begin
                                         Answers^.Put(SplitUpLong[2], FStr(ErrorCode));

                                         ErrorCode := 0;
                                       end; { with }
                                   end; { with }
                               end; { if }
                           end; { if }
                        end { if }

                else if Command = 'CFG_FILESEEK' then
                       begin
                         { Syntax: CFG_FILESEEK <file#> <position> }
                         if CommandCount >= 2 then
                           begin
                             if SplitUpStr[1, 1] = '#' then
                               SplitUpLong[1] := FVal(Answers^.Get(FVal(Copy(SplitUpStr[1], 2, 255))));

                             if SplitUpLong[1] in [1..qaMaxCfgFiles] then
                               begin
                                 With QuestObj(Qobj^).qaCfgFileInfo[SplitUpLong[1]]^ do
                                   begin
                                     if SplitUpStr[2, 1] = '#' then
                                       SplitUpLong[2] := FVal(Answers^.Get(FVal(Copy(SplitUpStr[2], 2, 255))));

                                     FileHandle^.Seek(SplitUpLong[2] * qaCfgFileSize[FileSort].BufSize);
                                  end; { with }
                               end; { if }
                           end; { if }
                        end { if }

                else if Command = 'CFG_FILEGETINFO' then
                       begin
                         { Syntax: Cfg_GetInfo <file#> <field-num> <variable#> }
                         if CommandCount >= 3 then
                           begin
                             if SplitUpStr[1, 1] = '#' then
                               SplitUpLong[1] := FVal(Answers^.Get(FVal(Copy(SplitUpStr[1], 2, 255))));

                             if SplitUpStr[2, 1] = '#' then
                               SplitUpLong[2] := FVal(Answers^.Get(FVal(Copy(SplitUpStr[2], 2, 255))));

                             if SplitUpLong[1] in [1..qaMaxCfgFiles] then
                               begin
                                 if SplitUpLong[3] in [1..qaMaxAnswers] then
                                   begin
                                     With QuestObj(Qobj^).qaCfgFileInfo[SplitUpLong[1]]^ do
                                       begin
                                         WorkStr := '';

                                         Case FileSort of
                                           cfgf_Limits     : CfgFile_MapLimitsRa(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_Language   : CfgFile_MapLanguageRa(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_Lastcall   : CfgFile_MapLastcallBbs(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_FdbHdr     : CfgFile_MapFdbHdr(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_FdbIdx     : CfgFile_MapFdbIdx(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_UsersBbs   : CfgFile_MapUsersBbs(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_UsersIdx   : CfgFile_MapUsersIdx(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_SysInfo    : CfgFile_MapSysInfoBbs(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_Menu       : CfgFile_MapMenuFile(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_Events     : CfgFile_MapEventRa(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_Messages   : CfgFile_MapMessageRa(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_Groups     : CfgFile_MapGroupsRa(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_Files      : CfgFile_MapFilesRa(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_Modem      : CfgFile_MapModemRa(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_Config     : CfgFile_MapConfigRa(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_Protocol   : CfgFile_MapProtocolRa(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_Telnet     : CfgFile_MapTelnetEle(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_Lightbar   : CfgFile_MapLightbarFile(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_NewsServer : CfgFile_MapNewsServerFile(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_NewsGroup  : CfgFile_MapNewsgroupStats(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_ConfigEle  : CfgFile_MapConfigEle(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_MessagesEle: CfgFile_MapMessagesEle(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_FilesEle   : CfgFile_MapFilesEle(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_UseronBbs  : CfgFile_MapUseronBbs(BufData^, SplitUpLong[2], WorkStr);
                                         end; { case }

                                         Answers^.Put(SplitUpLong[3], WorkStr);
                                       end; { with }
                                   end; { with }
                               end; { if }
                           end; { if }
                        end { if }

                else if Command = 'CFG_FILESETINFO' then
                       begin
                         { Syntax: Cfg_GetInfo <file#> <field-num> <variable#> }
                         if CommandCount >= 3 then
                           begin
                             if SplitUpStr[1, 1] = '#' then
                               SplitUpLong[1] := FVal(Answers^.Get(FVal(Copy(SplitUpStr[1], 2, 255))));

                             if SplitUpStr[2, 1] = '#' then
                               SplitUpLong[2] := FVal(Answers^.Get(FVal(Copy(SplitUpStr[2], 2, 255))));

                             if SplitUpLong[1] in [1..qaMaxCfgFiles] then
                               begin
                                 if SplitUpLong[3] in [1..qaMaxAnswers] then
                                   begin
                                     WorkStr := Answers^.Get(SplitUpLong[3]);

                                     With QuestObj(Qobj^).qaCfgFileInfo[SplitUpLong[1]]^ do
                                       begin
                                         Case FileSort of
                                           cfgf_Limits     : CfgFile_MapLimitsRa(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_Language   : CfgFile_MapLanguageRa(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_Lastcall   : CfgFile_MapLastcallBbs(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_FdbHdr     : CfgFile_MapFdbHdr(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_FdbIdx     : CfgFile_MapFdbIdx(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_UsersBbs   : CfgFile_MapUsersBbs(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_UsersIdx   : CfgFile_MapUsersIdx(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_SysInfo    : CfgFile_MapSysInfoBbs(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_Menu       : CfgFile_MapMenuFile(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_Events     : CfgFile_MapEventRa(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_Messages   : CfgFile_MapMessageRa(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_Groups     : CfgFile_MapGroupsRa(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_Files      : CfgFile_MapFilesRa(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_Modem      : CfgFile_MapModemRa(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_Config     : CfgFile_MapConfigRa(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_Protocol   : CfgFile_MapProtocolRa(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_Telnet     : CfgFile_MapTelnetEle(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_Lightbar   : CfgFile_MapLightbarFile(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_NewsServer : CfgFile_MapNewsServerFile(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_NewsGroup  : CfgFile_MapNewsgroupStats(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_ConfigEle  : CfgFile_MapConfigEle(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_MessagesEle: CfgFile_MapMessagesEle(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_FilesEle   : CfgFile_MapFilesEle(BufData^, SplitUpLong[2], WorkStr);
                                           cfgf_UseronBbs  : CfgFile_MapUseronBbs(BufData^, SplitUpLong[2], WorkStr);
                                         end; { case }
                                       end; { with }
                                   end; { with }
                               end; { if }
                           end; { if }
                        end { if }

                else if Command = 'CFG_FILEREAD' then
                       begin
                         { Syntax: CFG_FILEREAD <file#> }
                         if CommandCount >= 1 then
                           begin
                             if SplitUpStr[1, 1] = '#' then
                               SplitUpLong[1] := FVal(Answers^.Get(FVal(Copy(SplitUpStr[1], 2, 255))));

                             if SplitUpLong[1] in [1..qaMaxCfgFiles] then
                               begin
                                 With QuestObj(Qobj^).qaCfgFileInfo[SplitUpLong[1]]^ do
                                   begin
                                     {-- Initializes variables --------------}
                                     FileHandle^.BlkRead(BufData^, qaCfgFileSize[FileSort].BufSize);
                                     ErrorCode := FileHandle^.IoResult;
                                   end; { with }
                               end; { if }
                           end; { if }
                       end { 'CFG_FILEREAD' }

                else if Command = 'CFG_FILEWRITE' then
                       begin
                         { Syntax: CFG_FILEWRITE <file#> }
                         if CommandCount >= 1 then
                           begin
                             if SplitUpStr[1, 1] = '#' then
                               SplitUpLong[1] := FVal(Answers^.Get(FVal(Copy(SplitUpStr[1], 2, 255))));

                             if SplitUpLong[1] in [1..qaMaxCfgFiles] then
                               begin
                                 With QuestObj(Qobj^).qaCfgFileInfo[SplitUpLong[1]]^ do
                                   begin
                                     {-- Initializes variables --------------}
                                     FileHandle^.BlkWrite(BufData^, qaCfgFileSize[FileSort].BufSize);
                                     ErrorCode := FileHandle^.IoResult;
                                   end; { with }
                               end; { if }
                           end; { if }
                       end { 'CFG_FILEREAD' }

                else if Command = 'CFG_FILECLOSE' then
                       begin
                         { Syntax: CFG_FILECLOSE <file#> }
                         if CommandCount >= 1 then
                           begin
                             if SplitUpStr[1, 1] = '#' then
                               SplitUpLong[1] := FVal(Answers^.Get(FVal(Copy(SplitUpStr[1], 2, 255))));

                             if SplitUpLong[1] in [1..qaMaxCfgFiles] then
                               begin
                                 With QuestObj(Qobj^).qaCfgFileInfo[SplitUpLong[1]]^ do
                                   begin
                                     {-- Initializes variables --------------}
                                     FileName := '';
                                     Dispose(FileHandle, Done);
                                     ErrorCode := 0;

                                     FreeMem(BufData, qaCfgFileSize[FileSort].BufSize);
                                   end; { with }

                                 Dispose(QuestObj(Qobj^).qaCfgFileInfo[SplitUpLong[1]]);
                               end; { if }
                           end; { if }
                       end; { 'CFG_FILECLOSE' }

               {---------------------------- 'CH' -------------------------------}
               'H' : if Command = 'CHANGECOLOR' then
                       begin
                         { Syntax: ChangeColor <foreground> <background> }
                         if CommandCount >= 1 then
                            begin
                             {$IFNDEF ISCGI}
                               OutputProc('`F'+ FStr(SplitUpLong[1]) + ':`B' +
                                                FStr(SplitUpLong[2]) + ':');
                             {$ENDIF}
                           end; { if }
                       end; { 'CHANGECOLOR' }

               {----------------------------- 'CU' ------------------------------}
               'U' : if Command = 'CURSOR' then
                     begin
                       { Syntax: Cursor <x-pos> <y-pos> }
                       if CommandCount >= 3 then
                         begin
                           {$IFNDEF ISCGI}
                             OutputProc('`X'+ FStr(SplitUpLong[1]) + ':');
                             OutPutProc('`Y'+ FStr(SplitUpLong[2]) + ':');
                           {$ENDIF}
                         end; { if }
                     end; { 'CURSOR' }

               {----------------------------- 'CO' ------------------------------}
               'O' : if Command = 'CONVERTAREA' then
                     begin
                       { Syntax: ConvertArea [FILES|MESSAGES] [varnum of areanum] }
                       if CommandCount >= 3 then
                         begin
                           if SplitUpLong[2] in [1..qaMaxAnswers] then
                             begin
                               if SUpCase(SplitUpStr[1]) = 'FILES' then
                                 Answers^.Put(SplitUpLong[2], FStr(Ra250MsgArea(FVal(Answers^.Get(SplitUpLong[2])))))
                                   else Answers^.Put(SplitUpLong[2], FStr(Ra250Area(FVal(Answers^.Get(SplitUpLong[2])))));
                             end; { if }
                         end; { if }
                     end; { 'CURSOR' }

               {------------------------------ 'CL' -----------------------------}
               'L' : if Command = 'CLEARSCREEN' then
                       if ReadBit(LineCfg^.Exitinfo^.Userinfo.Attribute, 1) then
                         {$IFNDEF ISCGI}
                           OutputObj^.ClearScreen;
                         {$ELSE}
                           begin end;
                         {$ENDIF}
             end; { 'C' }

       {------------------------------- 'D' --------------------------------}
       'D' : Case Command[2] of
             {---------------------------- 'DE' -------------------------------}
             'E' : if Command = 'DELAY' then
                     begin
                       if CommandCount > 1 then
                         begin
                           {$IFNDEF ISCGI}
                             InputObj^.UpdateScrn;
                             OutBlockObj^.DumpBlock;

                             Delay(SplitUpLong[1]);
                           {$ENDIF}
                         end; { if }
                     end { 'DELAY' }

                   { Syntax: DELETEMSG <areanr> <msgnum> <check-access> }
              else if Command = 'DELETEMSG' then
                     begin
                       if CommandCount > 3 then
                         begin
                           if SplitUpStr[1, 1] = '#' then
                             SplitUpLong[1] := FVal(Answers^.Get(SplitUpLong[1]));

                           if SplitUpStr[2, 1] = '#' then
                             SplitUpLong[2] := FVal(Answers^.Get(SplitUpLong[2]));

                           if SplitUpStr[3, 1] = '#' then
                             SplitUpStr[3] := Answers^.Get(SplitUpLong[3]);

                           SilentDeleteMsg(SplitUpLong[1], SplitUpLong[2], SUpCase(SplitUpStr[3]) = 'YES');
                         end; { if }
                     end; { 'DELETEMSG' }

             {---------------------------- 'DI' -------------------------------}
             'I' : if Command = 'DISPLAYLOCAL' then
                     begin
                       {$IFNDEF ISCGI}
                         WorkStr := Trim(Copy(CommandStr, Length('DISPLAYLOCAL') + 01, 255));
                         WorkStr := QaSysObj(QObj^).MakeDisplayStr(WorkStr, false);

                         While Pos('|', WorkStr) > 0 do
                            Replace('|', #13#10, WorkStr);

                         SaveLocal := LineCfg^.LocalOnly;
                         Flush(Output);
                         LineCfg^.LocalOnly := TRUE;
                         OutputProc(WorkStr);
                         Flush(Output);
                         LineCfg^.LocalOnly := SaveLocal;
                       {$ENDIF}
                     end  { 'DISPLAY' }

                   else if Command = 'DISPLAYFILE' then
                          begin
                            {$IFNDEF ISCGI}
                              DisplayHotFile(SplitUpStr[1], []);
                            {$ENDIF}
                          end; { 'DISPLAYFILE' }

                  {------------------------- 'DO' ---------------------------}
                  'O' : if Command = 'DOCONTINUE' then
                          begin
                            {$IFNDEF ISCGI}
                              if CommandCount >= 2 then
                               if SplitUpLong[1] in [1..qaMaxAnswers] then
                                begin
                                  if OutputObj^.StopMore then
                                    Answers^.Put(SplitUpLong[1], 'NO')
                                     else Answers^.Put(SplitUpLong[1], 'YES');
                                 end; { if }
                            {$ENDIF}
                          end; { if }
             end; { 'D' }

       {------------------------------- 'E' --------------------------------}
       'E' : Case Command[2] of
             {---------------------------- 'EX' -------------------------------}
             {$IFNDEF ISCGI}
             'X' : if Command = 'EXEC' then
                       RaExec(Copy(CommandStr, Length('EXEC') + 01, 250), false, true, false, false, ExitCode, TempBool);
             {$ENDIF}

             {------------------------- 'EM' ----------------------------------}
             'M' : if Command = 'EMULATEINPUT' then
                     begin
                       if CommandCount >= 2 then
                         begin
                           {$IFNDEF ISCGI}
                             InputObj^.PutInBuffer(Trim(Copy(CommandStr,
                                                   Length('EMULATEINPUT') + 1, 255)),
                                                   false);
                           {$ENDIF}
                         end; { if }
                     end { 'EMULATEINPUT' }

              else if Command = 'EMULATEVAR' then
                     begin
                       if CommandCount >= 2 then
                         begin
                           {$IFNDEF ISCGI}
                             if SplitUpLong[1] in [1..qaMaxAnswers] then
                              InputObj^.PutInBuffer(Answers^.Get(SplitUpLong[1]), false);
                           {$ENDIF}
                         end; { if }
                     end { 'EMULATEVAR' }

              else if Command = 'EMULATESYSINPUT' then
                     begin
                       if CommandCount >= 2 then
                         begin
                           {$IFNDEF ISCGI}
                             InputObj^.PutInBuffer(Trim(Copy(CommandStr,
                                                Length('EMULATESYSINPUT') + 1, 255)),
                                                true);
                           {$ENDIF}
                         end; { if }
                     end { 'EMULATESYSINPUT' }

              else if Command = 'EMULATESYSVAR' then
                     begin
                       if CommandCount >= 2 then
                         begin
                           {$IFNDEF ISCGI}
                             if SplitUpLong[1] in [1..qaMaxAnswers] then
                              InputObj^.PutInBuffer(Answers^.Get(SplitUpLong[1]), true);
                           {$ENDIF}
                         end; { if }
                     end; { 'EMULATESYSVAR' }
             end; { 'E' }

       {------------------------------- 'G' --------------------------------}
       'G' : Case Command[2] of
             {---------------------------- 'GE' -------------------------------}
             'E' : if Command = 'GETCHOICE' then
                    begin
                      if CommandCount >= 3 then
                        begin
                          {$IFNDEF ISCGI}
                            if SplitUpLong[2] in [1..qaMaxAnswers] then
                              begin
                                Replace('|', #13, SplitUpStr[1]);
                                Str2Set(SUpCase(SplitUpStr[1]), GoodKeys);

                                repeat
                                  TempCH := UpCase(InputObj^.ReadKey);
                                until (TempCH in GoodKeys) OR (Progterminated);

                                if SPlitUpStr[3] = 'NO' then
                                  OutputProc(TempCH)
                                   else OutputProc(TempCH + CrLf);

                                if TempCH=#13 then TempCH := '|';

                                Answers^.Put(SplitUpLong[2], Trim(TempCH));
                              end; { if }
                          {$ENDIF}

                        end; { if }
                    end { 'GetChoice' }

              else if Command = 'GETARROWKEY' then
                    begin
                      if CommandCount >= 2 then
                        begin
                          {$IFNDEF ISCGI}

                          if SplitUpLong[1] in [1..qaMaxAnswers] then
                            begin
                              Answers^.Put(SplitUpLong[1], '');

                              TempCH := UpCase(InputObj^.ReadKey);
                              if TempCH=#127 then
                                Answers^.Put(SplitUpLong[1], 'DELETE');

                              if TempCH=#22 then
                                begin
                                  TempCH := InputObj^.ReadKey;

                                  Case TempCH of
                                    #09 : Answers^.Put(SplitUpLong[1], 'INSERT');
                                  end; { case }
                                end; { if }


                              if TempCH=#27 then
                                begin
                                  TempCH := GetArrowKeys;

                                  Case TempCH of
                                    'A' : Answers^.Put(SplitUpLong[1], 'UP');
                                    'B' : Answers^.Put(SplitUpLong[1], 'DOWN');
                                    'C' : Answers^.Put(SplitUpLong[1], 'RIGHT');
                                    'D' : Answers^.Put(SplitUpLong[1], 'LEFT');
                                    'H' : Answers^.Put(SplitUpLong[1], 'HOME');
                                    'K' : Answers^.Put(SplitUpLong[1], 'END')
                                      else Answers^.Put(SplitUpLong[1], #27);
                                  end; { case }
                                end; { if escape }

                                if TempCH = #13 then TempCH := '|';
                                if Answers^.Get(SplitUpLong[1]) = '' then
                                    Answers^.Put(SplitUpLong[1], Trim(TempCH));
                            end; { if }
                          {$ENDIF}
                        end; { if }
                    end { 'GETARROWKEY' }

              else if Command = 'GETSYSTEMNAME' then
                    begin
                      if CommandCount > 1 then
                        begin
                          if SplitUpLong[1] in [1..qaMaxAnswers] then
                            begin
                              Answers^.Put(SplitUpLong[1], GlobalCfg^.RaConfig^.SystemName)
                            end; { if }
                        end; { if }

                    end { GetSystemName }

              else if Command = 'GETTELNET' then
                    begin
                      if CommandCount > 1 then
                        begin
                          if SplitUpLong[1] in [1..qaMaxAnswers] then
                            begin
                              if TelnetServ then
                               Answers^.Put(SplitUpLong[1], 'YES')
                                else Answers^.Put(SplitUpLong[1], 'NO');
                            end; { if }
                        end; { if }

                    end { 'GETTELNET' }


              else if Command = 'GETRAWKEY' then
                    begin
                      if CommandCount >= 2 then
                        begin
                          {$IFNDEF ISCGI}
                          if SplitUpLong[1] in [1..qaMaxAnswers] then
                            begin
                              Answers^.Put(SplitUpLong[1], '');

                              TempCH := InputObj^.ReadKey;

                              if TempCH=#127 then
                                Answers^.Put(SplitUpLong[1], 'DELETE');

                              if TempCH = #22 then
                                begin
                                  TempCH := InputObj^.ReadKey;

                                  Case TempCH of
                                    #09 : Answers^.Put(SplitUpLong[1], 'INSERT');
                                  end; { case }
                                end; { if }

                              if TempCH = #27 then
                                begin
                                  TempCH := GetArrowKeys;

                                  Case TempCH of
                                    'A' : Answers^.Put(SplitUpLong[1], 'UP');
                                    'B' : Answers^.Put(SplitUpLong[1], 'DOWN');
                                    'C' : Answers^.Put(SplitUpLong[1], 'RIGHT');
                                    'D' : Answers^.Put(SplitUpLong[1], 'LEFT');
                                    'H' : Answers^.Put(SplitUpLong[1], 'HOME');
                                    'K' : Answers^.Put(SplitUpLong[1], 'END')
                                      else Answers^.Put(SplitUpLong[1], #27);
                                  end; { case }
                                end; { if escape }

                              if Answers^.Get(SplitUpLong[1]) = '' then
                                Answers^.Put(SplitUpLong[1], Copy(TempCH, 1, Length(TempCH)));
                            end; { if }
                          {$ENDIF}
                        end; { if }
                    end { 'GETRAWKEY' }

              else if Command = 'GETGRAPH' then
                    begin
                      if CommandCount >= 2 then
                        begin
                          {$IFNDEF ISCGI}
                          if SplitUpLong[1] in [1..qaMaxAnswers] then
                            begin
                              WorkStr := '';

                              if LineCfg^.AnsiOn then WorkStr := 'ANSI';
                              if LineCfg^.AvatarOn then WorkStr := WorkStr + ',AVATAR';
                              if LineCfg^.RipOn then WorkStr := WorkStr + ',RIP';
                              if WorkStr[1] = ',' then Delete(WorkStr, 1, 1);

                              Answers^.Put(SplitUpLong[1], WorkStr);
                            end; { if }
                          {$ENDIF}
                        end; { if }
                    end { 'GETGRAPH' }

              else if Command = 'GETFLAG' then
                    begin
                      if CommandCount >= 3 then
                       if SplitUpLong[1] in [1..qaMaxAnswers] then
                          begin
                            if RaCheckFlags(LineCfg^.Exitinfo^.Userinfo.Flags, SplitUpStr[2]) then
                              Answers^.Put(SplitUpLong[1], 'YES')
                                else Answers^.Put(SplitUpLong[1], 'NO');
                          end; { if }
                    end { 'GETFLAG' }

              else if Command = 'GETXY' then
                    begin
                      if CommandCount >= 3 then
                        begin
                          {$IFNDEF ISCGI}
                          if SplitUpLong[1] in [1..qaMaxAnswers] then
                            if SplitUpLong[2] in [1..qaMaxAnswers] then
                              begin
                                Answers^.Put(SplitUpLong[1], FStr(OutputObj^.WhereX));
                                Answers^.Put(SplitUpLong[2], FStr(OutputObj^.WhereY));
                              end; { if }
                          {$ENDIF}
                        end; { if }
                    end; { 'GetXY' }
             end; { 'G' }

       {------------------------------- 'K' --------------------------------}
       'K' : begin
               Case Command[2] of
               {------------------------- 'KE' ---------------------------}
               'E' : if Command = 'KEYPRESS' then
                      begin
                        {$IFNDEF ISCGI}
                        if CommandCount > 1 then
                          begin
                            if SplitUpLong[1] in [1..qaMaxAnswers] then
                              begin
                                  InputObj^.UpdateScrn;
                                  OutBlockObj^.DumpBlock;

                                  if InputObj^.Keypressed then
                                    Answers^.Put(SplitUpLong[1], 'YES')
                                      else Answers^.Put(SplitUpLong[1], 'NO');
                              end; { if }
                          end; { if }
                        {$ENDIF}
                       end; { 'KEYPRESS' }
               end; { case }
             end; { 'K' }

       {------------------------------- 'M' --------------------------------}
       'M' : begin
               Case Command[2] of
               {------------------------- 'ME' -----------------------------}
               'E' : if (Command = 'MENUCMD') OR (Command = 'MENUCMND') then
                       begin
                         {$IFNDEF ISCGI}
                          if CommandCount > 1 then
                           begin
                             Variable1 := Copy(CommandStr,
                                               Length(Command) + 01,
                                               255);

                             Counter := FVal(FirstWord(Variable1, defExtractWord, false));

                             if SplitUpStr[2, 1] = '#' then
                               begin
                                 if SplitUpLong[2] in [1..qaMaxAnswers] then
                                   begin
                                     Variable1 := Answers^.Get(SplitUpLong[2]);
                                   end; { if }
                               end; { if }

                               if Counter > 00 then
                                ExecuteMenuFunc(Counter, Variable1, '', TempBool, GoodKeys);
                            end; { if }
                         {$ENDIF}
                       end; { 'MENUCMD', 'MENUCMND' }
               end; { case }
             end; { 'M' }

       {------------------------------- 'P' --------------------------------}
        'P' : begin
                Case Command[2] of
                  {------------------------- 'PO' ---------------------------}
                  'O' : if Command = 'POSTINFO' then
                           QaSysObj(QObj^).AddStr('*** ' + LineCfg^.Exitinfo^.Userinfo.Name + ' completed questionnaire at '+
                                 JDates.TimeStr(false, false)+' on '+ LangObj^.RaFormatDate(JDates.DateStr, 0, y2k_SysDate,
                                 Linecfg^.Exitinfo^.Userinfo.DateFormat) +
                                 ' ***'+#13+#10)

              else if Command = 'POPX' then
                     begin
                       {$IFNDEF ISCGI}
                         if SplitUpLong[1] in [1..20] then
                           OutputProc('`X'+ FStr(SavedX[SplitUpLong[1]]) + ':');
                       {$ENDIF}
                     end { 'POPX' }

              else if Command = 'POPY' then
                     begin
                       {$IFNDEF ISCGI}
                         if SplitUpLong[1] in [1..20] then
                           OutputProc('`Y'+ FStr(SavedY[SplitUpLong[1]]) + ':');
                       {$ENDIF}
                     end { 'POPY' }

              else if Command = 'POPCOLOR' then
                     begin
                       {$IFNDEF ISCGI}
                         if SplitUpLong[1] in [1..20] then
                           OutputObj^.DisplayAttr(SavedAttr[SplitUpLong[1]]);
                       {$ENDIF}
                     end; { 'POPCOLOR' }

             {------------------------- 'PU' ---------------------------}
             'U' : if Command = 'PUSHX' then
                     begin
                       {$IFNDEF ISCGI}
                         if SplitUpLong[1] in [1..20] then
                           SavedX[SplitUpLong[1]] := OutputObj^.WhereX;
                       {$ENDIF}
                      end { 'PUSHX' }

              else if Command = 'PUSHY' then
                     begin
                       {$IFNDEF ISCGI}
                         if SplitUpLong[1] in [1..20] then
                           SavedY[SplitUpLong[1]] := OutputObj^.WhereY;
                       {$ENDIF}
                      end { 'PUSHY' }

              else if Command = 'PUSHCOLOR' then
                     begin
                       {$IFNDEF ISCGI}
                         if SplitUpLong[1] in [1..20] then
                           {$IFDEF WINGUI}
                             SavedAttr[SplitUpLong[1]] := Form1.ColorConsole1.TextAttr;
                           {$ELSE}
                             SavedAttr[SplitUpLong[1]] := TextAttr;
                           {$ENDIF}
                       {$ENDIF}
                     end; { 'PUSHCOLOR' }

             {------------------------- 'PA' ---------------------------}
             'A' : if Command = 'PAGEMODE' then
                    begin
                      {$IFNDEF ISCGI}
                      if CommandCount > 2 then
                        begin
                          if SplitUpLong[1] in [1..qaMaxAnswers] then
                           if SplitUpLong[2] in [1..qaMaxAnswers] then
                             begin
                                if RaYell then
                                  Answers^.Put(SplitUpLong[1], 'ON')
                                    else Answers^.Put(SplitUpLong[1], 'OFF');

                                if ScrollLockOn then
                                  Answers^.Put(SplitUpLong[2], 'ON')
                                   else Answers^.Put(SplitUpLong[2], 'OFF');
                             end; { if }
                        end; { if }
                      {$ENDIF}
                    end; { if }
             end; { 'A' }
          end; { 'P' }

       {------------------------------- 'S' --------------------------------}
        'S' : begin
                Case Command[2] of
                {------------------------- 'SE' ---------------------------}
                'E' : if Command = 'SETFLAG' then
                        begin
                          if CommandCount > 2 then
                            begin
                              SplitUpStr[2] := SupCase(SplitUpStr[2]);

                              if SplitUpStr[2] = 'ON' then
                                RaSetFlags(LineCfg^.Exitinfo^.Userinfo.Flags, SplitUpStr[1])
                                 else RaReSetFlags(LineCfg^.Exitinfo^.Userinfo.Flags, SplitUpStr[1]);
                            end; { if }
                        end { 'SETFLAG' }

              else if Command = 'SETCOMMENT' then
                    begin
                      if CommandCount > 1 then
                        begin
                          if SplitUpLong[1] in [1..qaMaxAnswers] then
                            LineCfg^.Exitinfo^.Userinfo.Comment := Answers^.Get(SplitUpLong[1]);
                        end; { if }
                    end { 'SETCOMMENT' }

              else if Command = 'SETOLM' then
                    begin
                      {$IFNDEF ISCGI}
                      if CommandCount > 1 then
                        begin
                          if SupCase(Trim(SplitUpStr[1])) = 'ON' then
                            contrObj^.TimeInfo.InternodeChecking := true;

                          if SupCase(Trim(SplitUpStr[1])) = 'OFF' then
                            contrObj^.TimeInfo.InternodeChecking := false;
                        end; { if }
                      {$ENDIF}
                    end { 'SETOLM' }

              else if Command = 'SETCHATREASON' then
                    begin
                      if CommandCount > 1 then
                        begin
                          if SplitUpLong[1] in [1..qaMaxAnswers] then
                            LineCfg^.Exitinfo^.PageReason := Copy(Answers^.Get(SplitUpLong[1]), 1, 80);
                        end; { if }
                    end { if }

              else if Command = 'SETCHATWANTED' then
                    begin
                      {$IFNDEF ISCGI}
                      if CommandCount > 1 then
                        begin
                          if SupCase(Trim(SplitUpStr[1])) = 'ON' then
                            LineCfg^.Exitinfo^.WantChat := true;

                          if SupCase(Trim(SplitUpStr[1])) = 'OFF' then
                            LineCfg^.Exitinfo^.WantChat := false;

                          StatusDisplay(99, false);
                        end; { if }
                      {$ENDIF}
                    end { 'SETCHATWANTED' }

              else if Command = 'SETSECURITY' then
                    begin
                      if CommandCount > 1 then
                        begin
                          if SplitUpStr[1, 1] = '#' then
                            LineCfg^.Exitinfo^.Userinfo.Security := FVal(Answers^.Get(SplitUpLong[1]))
                             else LineCfg^.Exitinfo^.Userinfo.Security := SplitUpLong[1];

                          {$IFNDEF ISCGI}
                            GetLevelLimitsInfo(LineCfg^.Exitinfo^.Userinfo.Security, false, false);

                            StatusDisplay(99, false);
                          {$ENDIF}

                          LogHook('!', 'Security level altered to '+FStr(SplitUpLong[1]));
                        end; { if }
                    end { 'SETSECURITY' }

              else if Command = 'SETTIME' then
                    begin
                      {$IFNDEF ISCGI}
                      if CommandCount > 1 then
                        begin
                          if SplitUpStr[1, 1] = '#' then
                            Limit_U.SetTime(FVal(Answers^.Get(SplitUpLong[1])))
                             else Limit_U.SetTime(FVal(SplitUpStr[1]));
                        end; { 'SETTIME' }
                      {$ENDIF}
                    end { 'SETTIME' }

              else if Command = 'SETX' then
                    begin
                      {$IFNDEF ISCGI}
                        if CommandCount > 1 then
                          OutputProc('`X'+ FStr(SplitUpLong[1]) + ':');
                      {$ENDIF}
                    end { 'SETX' }

              else if Command = 'SETY' then
                    begin
                      {$IFNDEF ISCGI}
                        if CommandCount > 1 then
                           OutputProc('`Y'+ FStr(SplitUpLong[1]) + ':');
                      {$ENDIF}
                    end { 'SETY' }

              else if Command = 'SETSTATUSBAR' then
                    begin
                      {$IFNDEF ISCGI}
                        if CommandCount > 1 then
                          StatusDisplay(SplitUpLong[1], false);
                      {$ENDIF}
                    end { 'SETSTATUSBAR' }

              else if Command = 'SETUSERVAR' then
                    begin
                      if CommandCount > 1 then
                        begin
                          WorkStr := Copy(CommandStr, Length('SETUSERVAR') + 04, 255);

                          if WorkStr[01] = '#' then
                            begin
                              Delete(WorkStr, 1, 1);

                              if FVal(WorkStr) in [1..qaMaxAnswers] then
                                WorkStr := Answers^.Get(FVal(WorkStr));
                            end; { if }

                          UserCodeString(SplitUpStr[1, 1],
                                         TempBool,
                                         WorkStr,
                                         true);
                        end; { if }
                    end { 'SETUSERVAR' }

              else if Command = 'SETEDITOR' then
                    begin
                      if CommandCount > 1 then
                        begin
                          if SplitUpStr[1, 1] in ['#'] then
                            SplitUpStr[1] := Answers^.Get(SplitUpLong[1]);

                          GlobalCfg^.RaConfig^.ExternalEdCmd := SplitUpStr[1];
                        end; { if }

                    end { SetEditor }


              else if Command = 'SETRESULTVAR' then
                    begin
                      if CommandCount > 1 then
                        begin
                          if SplitUpLong[1] in [1..qaMaxAnswers] then
                            ReturnResult := Answers^.Get(SplitUpLong[1]);
                        end; { if }
                    end { 'SETRESULTVAR' }

              else if Command = 'SETUSERON' then
                    begin
                      if CommandCount > 1 then
                        begin

                          WorkStr := Trim(Copy(CommandStr, Length('SETUSERON') + 01, 255));
                          termObj^.RaCodeStr(WorkStr);

                          MultiLnObj^.UserOn^.StatDesc := WorkStr;
                          MultiLnObj^.WriteUserOn(MultiLnObj^.Useron^.StatDesc, uonBrowsing);
                        end; { if }
                    end; { 'SETUSERON' }
                end; { case }
             end; { 'S' }

       {------------------------------- 'W' --------------------------------}
       'W' : begin
                Case Command[2] of
                {------------------------- 'WA' ---------------------------}
                'A' : if Command = 'WAITENTER' then
                         begin
                           {$IFNDEF ISCGI}
                             InputObj^.WaitEnter
                           {$ENDIF}
                         end
                   else if Command = 'WASSYSOPKEY' then
                          begin
                            {$IFNDEF ISCGI}
                            if SplitUpLong[1] in [1..qaMaxAnswers] then
                              begin
                                if LineCfg^.SysOpKey then
                                  Answers^.Put(SplitUpLong[1], 'YES')
                                    else Answers^.Put(SplitUpLong[1], 'NO');
                              end; { if }
                            {$ENDIF}
                          end; { 'WasSysOpKey' }
                end; { case }
             end; { 'W' }
    end; { case }
end; { func. CommandsHook }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure QuestObj.Process(MiscData   : String;
                           ResetLines : Boolean;
                           StartLabel : String);
var SaveRadu     : Boolean;
    SaveInterNode: Boolean;
    OutputFile   : String;
    TempData     : String;

    SaveStatus   : Byte;
    SaveStatDesc : String;
begin
  SaveStatus := MultiLnObj^.UserOn^.Status;
  SaveStatDesc := MultiLnObj^.UserOn^.StatDesc;

  MultiLnObj^.WriteUseron('', uonQuestion);

  {---------------------- Define the output filename -----------------------}
  TempData := MiscData;
  Replace('/N', '', TempData);
  QInfo^.Name := NoExtension(WordStr.ExtractWord(TempData, 1, defExtractWord, true, false));
  QInfo^.Name := Slowcase(Trim(QInfo^.Name));
  OutputFile := GlobalCfg^.RaConfig^.SysPath + QInfo^.Name + '.asw';

  SaveRadu := LineCfg^.RaduCodes;
  LineCfg^.RaduCodes := TRUE;
  SaveInterNode := contrObj^.TimeInfo.InterNodeChecking;
  DoResetlines := ResetLines;

  QInfo^.QuesPath := LineCfg^.Language^.QuesPath;
  QInfo^.LogHook := {$IFDEF FPC}@{$ENDIF}Quest_LogProc;
  QInfo^.AbortHook := {$IFDEF FPC}@{$ENDIF}Quest_AbortFunc;
  QInfo^.SliceHook := {$IFDEF FPC}@{$ENDIF}Quest_SliceHook;

  inherited Process(Miscdata, StartLabel, OutputFile, {$IFDEF FPC}@{$ENDIF}CommandsHook);

  MultiLnObj^.WriteUseron(SaveStatDesc, SaveStatus);
  LineCfg^.RaduCodes := SaveRadu;
  contrObj^.TimeInfo.InterNodechecking := SaveInterNode;
end; { proc. process }


end. { unit QUESTION }
