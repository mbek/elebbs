program EleUtil;
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
{$IFNDEF VirtualPascal}
 {$ifdef Win32}
   {$apptype console}
 {$endif}
{$ENDIF}
(*
**
** EleUTIL, several smal utility files for EleBBS systems.
**
** Copyright (c) 1997 by Maarten Bekers
**
** Created : 07-Sep-1997
** Last update : 07-Sep-1997
**
**
*)

uses Crt, Dos, CfgRec, Global, Memman, Cases, CentrStr, Longstr, StrPath,
     GenCfg, GenFile, BitWise, IoRes_Un, Readcfg;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Const CommandOption : String = '';

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FatalError(S: String);
begin
  WriteLn;
  WriteLn(#32, #254, #32, S, ^G);
  WriteLn;
  Halt(255);
end; { proc. FatalError }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowHeader;
begin
  ClrScr;
  TextAttr := LightGray;
  WriteLn('ELEUTIL; EleBBS small maintenance utility, Version '+VersionID);
  WriteLn('         Copyright 1997-2003 Maarten Bekers, All rights reserved.');
  WriteLn;

  if NOT MemMan.AllocMem(raConfig, SizeOf(ConfigRecord), 'ConfigRecord', 'ShowHeader') then
    FatalError('Could not allocate enough memory!');

  if NOT MemMan.AllocMem(ElConfig, SizeOf(ELeConfigRecord), 'EleConfigRecord', 'ShowHeader') then
    FatalError('Could not allocate enough memory!');
end; { proc. ShowHeader }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ParseCommandLine;
var Invalid  : Boolean;
    TempParam: String;
begin
  Invalid := False;

  TempParam := SupCase(Trim(ParamStr(01)));

  If (TempParam='CALLERS') OR (TempParam='RENUMFILE') OR
      (TempParam = 'RENUMMSG') OR (TempParam='REN_MG') OR
       (TempParam = 'REN_FG') OR (TempParam='REIDX_F') OR
        (TempParam = 'REIDX_M') OR (TempParam='CHECKMSG') OR
         (TempParam = 'CHECKFILE') then
           CommandOption := TempParam;


  If Pos('?', ParamStr(1)) > 00 then
    begin
       Writeln('  CALLERS   <number> Specify number of callers called to system');
       WriteLn('  RENUMFILE Renumbers the file-areas in order of the file');
       WriteLn('  RENUMMSG  Renumbers the message areas in order of the file');
       WriteLn('  REN_MG    Renumbers the message groups in order of the file');
       WriteLn('  REN_FG    Renumbers the file groups in order of the file');
       WriteLn('  REIDX_F   Regenerates the FGROUPS.RDX and FILES.RDX files');
       WriteLn('  REIDX_M   Regenerates the MGROUPS.RDX and MESSAGES.RDX files');
       WriteLn('  CHECKMSG  Checks all the (JAM) message areas for missing areafiles');
       WriteLn('  CHECKFILE Checks all the filebase areas for missing paths');
       MemMan.ReleaseMem(RaConfig, SizeOf(ConfigRecord));
       Halt(255);
    end; { if }

  if (ParamCount = 00) OR (CommandOption='') then
    begin
      WriteLn;
      Write(Dup(#205, 80));
      WriteLn(#254, #32, 'Nothing to do! Type ELEUTIL ? for command-line help.');
      WriteLn;
      MemMan.ReleaseMem(RaConfig, SizeOf(ConfigRecord));
      Halt(255);
    end; { Invalid }

  Writeln(CommandOption);
  Write(Dup(#205, 80));
end; { proc. ParseCommandLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function YesNoQuestion(S: String; DefaultChar: Char): Boolean;
var CH     : Char;
begin
  Write(#254, #32, S);
  if DefaultChar='Y' then
    WriteLn(' (Y/n)?')
     else WriteLn(' (y/N)?');

  repeat
    CH := UpCase(Crt.ReadKey);

    if CH=#13 then CH := DefaultChar;
  until (CH in ['Y', 'N']);

  YesNoQuestion := (CH='Y');
end; { func. YesNoQuestion }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoCallers;
var SysInfo_F: File of SysInfoRecord;
    SysInfo  : SysInfoRecord;
    OldCalls : Longint;
begin
  if FVal(ParamStr(2)) < 1 then
      If NOT YesNoQuestion('Reset system calls to none', 'Y') then
        EXIT;

  Assign(SysInfo_F, SysInfoFileName);
  {$i-} System.Reset(Sysinfo_F); {$I+}
  If IOResult>00 then
    FatalError('Unable to open '+JustName(SysInfoFileName));

  {$i-}
    Read(SysInfo_F, SysInfo);
    OldCalls := SysInfo.TotalCalls;
    SysInfo.TotalCalls := FVal(ParamStr(2));
    Seek(SysInfo_F, 00);
    Write(SysInfo_F, SysInfo);
    Close(SysInfo_F);
  {$i+}
  if IOResult>00 then ;

  WriteLn(#254, #32, 'Set total system calls from ', OldCalls, ' callers to ', SysInfo.TotalCalls);
end; { proc. DoCallers }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Renumbase(FName: String; Size: Word);
var Buffer  : Array[1..12000] of Byte;
    AreaNum : Word absolute Buffer;
    Config_F: File;
    OldFPos : Longint;
    NumRead : NumReadType;
begin
  if Size > SizeOf(Buffer) then
    FatalError('Config structure to big for current allocated memory');

  Assign(Config_F, FName);
  FileMode := ReadWriteMode + DenyAll;
  {$i-} Reset(Config_F, 1); {$I+}
  if IOResult>00 then
    FatalError('Unable to open '+FName);

  Write(#254, #32, 'Renumbering ... ');

  While NOT Eof(Config_F) do
    begin
      OldFPos := FilePos(Config_F);
      BlockRead(Config_F, Buffer, Size, NumRead);

      Areanum := FilePos(Config_F) div Size;

      Seek(Config_F, OldFPos);

      if NumRead = Size then
        BlockWrite(Config_F, Buffer, Size);
    end; { while }

  WriteLn;
  WriteLn(#254, #32, 'Done ...');

  {$i-} Close(Config_F); {$I+}
  if IOResult>00 then ;
end; { proc. RenumFileBase }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GenerateRDX(UseFiles: Boolean);
begin
  GenerateRdxFiles(RaConfig^.SysPath, NOT UseFiles, False, false);
  GenerateRdxFiles(RaConfig^.SysPath, NOT UseFiles, True, false);
end; { proc. GenerateRDX }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CheckMessageFiles;
var Area_F  : File;
    MsgInf  : MessageRecord;

    Dest_F  : File;
    DoCopy  : Boolean;
    DelEmpty: Boolean;

function CheckArea(MsgInf: MessageRecord): Boolean;
var Temp: Boolean;
begin
  Temp := true;

  if NOT FileExist(MsgInf.JamBase + '.JDT') then Temp := false;
  if NOT FileExist(MsgInf.JamBase + '.JDX') then Temp := false;
  if NOT FileExist(MsgInf.JamBase + '.JHR') then Temp := false;
  if NOT FileExist(MsgInf.JamBase + '.JLR') then Temp := false;

  CheckArea := temp;
end; { function. CheckArea }

begin
  Writeln;
  Writeln(#32, #254, #32, 'This will check all your JAM messagebases and if');
  Writeln(#32, #254, #32, 'one of the files is missing, it will not write it');
  Writeln(#32, #254, #32, 'to FILTERED.RA');
  Writeln;

  DelEmpty := YesNoQuestion('Also remove areas with no name?', 'Y');

  Assign(Area_F, MessagesFileName);
  FileMode := ReadMode + Denynone;
  {$i-} Reset(Area_F, 1); {$i+}
  if IOResult > 00 then
    FatalError('Unable to open '+MessagesFileName);

  Assign(Dest_F, 'FILTERED.RA');
  FileMode := ReadWriteMode + DenyAll;
  {$i-} Rewrite(Dest_F, 1); {$i+}
  if IOResult > 00 then
    FatalError('Unable to create FILTERED.RA');

  While NOT Eof(Area_F) do
    begin
      BlockRead(Area_F, MsgInf, SizeOf(MessageRecord));

      DoCopy := true;

      if DelEmpty then
       if MsgInf.Name = '' then
         DoCopy := false;

      if ReadBit(MsgInf.Attribute, 7) then
       if MsgInf.Name <> '' then
         begin
           if NOT CheckArea(MsgInf) then
             begin
               WriteLn(#32, #254, #32, 'Not all files of area: ');
               Writeln(#32, #32, #32, MsgInf.Name, ' exist.');

               if YesNoQuestion(' Remove from FILTERED.RA', 'Y') then
                 begin
                   DoCopy := false;
                 end; { if }
             end; { if }
         end; { if }

       if DoCopy then BlockWrite(Dest_F, MsgInf, SizeOf(MessageRecord));
    end; { while }

  WriteLn;
  WriteLn(#32, #254, #32, 'Done.');

  Close(Area_F);
  Close(Dest_F);
end; { proc. CheckMessageFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CheckFilesDirectories;
var Area_F  : File;
    FilesInf: FilesRecord;

    Dest_F  : File;
    DoCopy  : Boolean;
    DelEmpty: Boolean;

function CheckArea(FilesInf: FilesRecord): Boolean;
var Temp: Boolean;
begin
  Temp := true;

  if NOT FileExist(ForceBack(FilesInf.FilePath)) then Temp := false;

  CheckArea := temp;
end; { function. CheckArea }

begin
  Writeln;
  Writeln(#32, #254, #32, 'This will check all your filebase areas and if');
  Writeln(#32, #254, #32, 'one of the paths is missing, it will not write it');
  Writeln(#32, #254, #32, 'to FILTERED.RA');
  Writeln;

  DelEmpty := YesNoQuestion('Also remove areas with no name?', 'Y');

  Assign(Area_F, FilesFileName);
  FileMode := ReadMode + Denynone;
  {$i-} Reset(Area_F, 1); {$i+}
  if IOResult > 00 then
    FatalError('Unable to open '+FilesFileName);

  Assign(Dest_F, 'FILTERED.RA');
  FileMode := ReadWriteMode + DenyAll;
  {$i-} Rewrite(Dest_F, 1); {$i+}
  if IOResult > 00 then
    FatalError('Unable to create FILTERED.RA');

  While NOT Eof(Area_F) do
    begin
      BlockRead(Area_F, FilesInf, SizeOf(FilesRecord));

      DoCopy := true;

      if DelEmpty then
       if FilesInf.Name = '' then
         DoCopy := false;

      if FilesInf.Name <> '' then
        begin
           if NOT CheckArea(FilesInf) then
             begin
               WriteLn(#32, #254, #32, 'The path of area: ');
               Writeln(#32, #32, #32, FilesInf.Name, ' does not exist.');

               if YesNoQuestion(' Remove from FILTERED.RA', 'Y') then
                 begin
                   DoCopy := false;
                 end; { if }
             end; { if }
         end; { if }

       if DoCopy then BlockWrite(Dest_F, FilesInf, SizeOf(FilesRecord));
    end; { while }

  WriteLn;
  WriteLn(#32, #254, #32, 'Done.');

  Close(Area_F);
  Close(Dest_F);
end; { proc. CheckFilesDirectories }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

begin
  RunningBBS := false;
  {$IFDEF OVERLAY}
    InitErrorHandler;
  {$ENDIF}
  ShowHeader;
  ReadConfigRa;
  InitSystemNames;
  ParseCommandline;

  if CommandOption='CALLERS' then DoCallers;
  if CommandOption='RENUMFILE' then RenumBase(MessagesFileName, SizeOf(MessageRecord));
  if CommandOption='RENUMMSG' then RenumBase(FilesFileName, SizeOf(FilesRecord));
  if CommandOption='REN_MG' then RenumBase(MGroupsFileName, SizeOf(GroupRecord));
  if CommandOption='REN_FG' then RenumBase(FGroupsFileName, SizeOf(GroupRecord));
  If CommandOption='REIDX_F' then GenerateRDX(True);
  If CommandOption='REIDX_M' then GenerateRDX(False);
  if CommandOption='CHECKMSG' then CheckMessageFiles;
  if CommandOption='CHECKFILE' then CheckFilesDirectories;

  MemMan.ReleaseMem(RaConfig, SizeOf(ConfigRecord));
end.
