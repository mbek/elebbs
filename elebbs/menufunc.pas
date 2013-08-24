unit MenuFunc;
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
** Menu functions routines for EleBBS
**
** Copyright (c) 1996 by Maarten Bekers
**
** Created : 06-Oct-1996
** Last update : 29-Jul-1997
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 uses CfgRec;
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF WITH_FULL}
  This unit shouldn''t be compiled for an utility program
{$ENDIF}

{$IFDEF WITH_FULL}
{$IFDEF ELEBBS}
procedure DisplayDirect(OptData: String);    { Display direct, with CR at end }
procedure TerminateCall(AskFiles: Boolean);       { RemoteAccess alike logoff }
{$IFDEF WITH_FULL}
procedure ExitWithErrorLevel(OptData: String);                { Menu type -15 }
{$ENDIF}
procedure ShowVersionInformation;                  { Show product information }
procedure ShowUsageGraph(var Exitinfo: ExitinfoRecord);
procedure DisplayRipIcon(MiscData: String);
procedure ShowTimeStatistics;               { Show menu-alike time statistics }
Procedure BullettMenu(MiscData: String);
procedure ToggleProc(Const Active, NotActive:Word; Var Current:Byte; Bit:Byte; OnOff: Boolean);
{$ENDIF}
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
{$IFNDEF WINGUI}
Uses {$IFDEF WITH_FULL} {OoCom, ApFossil,}{$ENDIF} Dos, Global, Support, Mail, Chat, Filerout, FileSys, StUtils, JDates,
      Ral, InOut_U, Crc_Unit, StrPath, Input_U,
       Debug_U, Sound_U, Access_U, AreaSel, ElLog_U, LineEd,
        DispAns, Control, Wfc, RipMisc, BitWise, COlors, Cases,
         LongStr, ComUnit, CentrStr, WordStr, Limit_U, ExitProg, GenFile,
          CfgFile, ApFossil, FileObj, TagUnit, Multi,
           OutBlock, ObjDec;
{$ELSE}
Uses SysUtils, Global, Support, Mail, Chat, Filerout, FileSys, Stutils, JDates, Ral, InOut_u, Crc_Unit,
       Debug_U, Sound_U, Access_U, AreaSel, ElLog_U, LineEd, Input_U,
        DispAns, Control, RipMisc, BitWise, Colors, Use32, Cases,
         CfgRec, ComUnit, CentrStr, WordStr, LongStr, Limit_U, ExitProg, GenFile,
          CfgFile, StrPath, ApFossil, Fileobj, TagUnit,
           Multi, OutBlock, ObjDec;
{$ENDIF}
{$ENDIF}

{$IFDEF WITH_FULL}
procedure ShowUsageGraph(var Exitinfo: ExitinfoRecord);

type ArrType = Array[0..15] of Byte;

function GetLineFor(B: Byte): String;
var Total   : Longint;
    Counter : Byte;
    TempStr : String;

function MakeStr(Orig, Match: Longint): String;
var Perc: Longint;
begin
  if Orig>00 then Perc := Round((Orig / Total) * 100)
    else Perc := 00;

  if Perc > Match then MakeStr := #219 + #219
     else MakeStr := #32#32;
end; { func. MakeStr }

begin
  Total := 00;
  For Counter := 00 to 23 do
    Inc(Total, LineCfg^.Exitinfo^.TimeLogInfo.BusyPerHour[Counter]);

  TempStr := '';
  For Counter := 00 to 23 do
    TempStr := TempStr + #32 + MakeStr(LineCfg^.Exitinfo^.TimeLogInfo.BusyPerHour[Counter], B);

  GetLineFor := TempStr;
end; { func. GetLineFor }


procedure MakeArray(Min, Max: Byte; Length: Word; var Ar: ArrType);
var Counter: Byte;
begin
  Ar[Length] := Max;
  For Counter := Pred(Length) downto 02 do
    Ar[Counter] := Round((Max / Length) * Counter);
  Ar[01] := Min;
end; { proc. MakeArray }

var Perc    : Longint;
    counter : Word;
    total   : Longint;
    Min,
    Max     : Byte;
    NrArr   : ArrType;
begin
  OutputObj^.ClearScreen;
  WriteLn;
  WriteLn('`F14:`B1:', CenterJust(LangObj^.ralGet(ralPercUsage) + #32 +
          LangObj^.RaFormatDate(Exitinfo.TimeLogInfo.StartDate, 0, y2k_SysDate,
          LineCfg^.Exitinfo^.Userinfo.DateFormat) +
          ' - ' + LangObj^.RaFormatDate(DateStr, 0, y2k_SysDate,
          LineCfg^.Exitinfo^.userinfo.DateFormat) + #32 + '(' +
          LangObj^.ralGet(ralLine) + #32 + FStr(LineCfg^.RaNodeNr) + ')', 80, True));
  Write('`A', MakeAttr(GlobalCfg^.RaConfig^.NormFore, GlobalCfg^.RaConfig^.NormBack), ':');

  Min   := 00;
  Max   := 00;
  Total := 00;

  For Counter := 00 to 23 do
      Inc(Total, Exitinfo.TimeLogInfo.BusyPerHour[Counter]);

  For Counter := 00 to 23 do
   With Exitinfo.TimelogInfo do
    begin
      if BusyPerHour[Counter]>00 then Perc := Round((BusyPerHour[Counter] / Total) * 100)
        else Perc := 00;

      if Perc>Max then Max := Perc;
      if Perc<Min then Min := Perc;

    end; { for }
  if Max=0 then Max := 01;

  MakeArray(Min, Max, 15, NrArr);

  For Counter := 15 downto 01 do
    WriteLn('`A10:', MakeLen(FStr(NrArr[Counter]), 02, Zero, True, false), '  `A2:�`A12:', GetLineFor(NrArr[Counter]));

  WriteLn('`A10:    `A2:읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�');
  WriteLn('`A10:      12 01 02 03 04 05 06 07 08 09 10 11 12 01 02 03 04 05 06 07 08 09 10 11');
  WriteLn('`A11:      ', LangObj^.ralGet(ralMidnight), '`X43:', LangObj^.ralGet(ralNoon));

  InputObj^.PressEnter(True, False);
end; { proc. ShowUsageGraph }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DisplayRipIcon(MiscData: String);

Function EscapeString(InString: String): String;
var TempStr: String;
    Counter: Byte;
begin
  TempStr := '';

  for Counter := 01 to Length(InString) do
    begin
      if InString[Counter] in ['!', '\', '|'] then
        TempStr := TempStr + '\';

      TempStr := TempStr + InString[Counter];
    end; { for }

  EscapeString := TempStr;
end; { func. EscapeString }

var X, Y     : Word;
    HotKey   : Char;
    IconName : String;
    LabelStr : String;
    Counter  : Byte;

    Orient,
    Flags,
    BevelSize: Word;
    Width,
    Height   : Word;
    ForeColor: Word;
begin
  X := FVal(Getvalue('/X=', MiscData, True));
  Y := FVal(Getvalue('/Y=', MiscData, True));

  HotKey := LastChar(Getvalue('/HOTKEY=', MiscData, True));
  IconName := Getvalue('/FILE=', MiscData, True);
  LabelStr := Getvalue('LABEL=', MiscData, True);
  ForeColor := FVal(Getvalue('/LABELCOL=', MiscData, True));

  if ForeColor=00 then ForeColor := 04;

  For Counter := 01 to Length(LabelStr) do
    if LabelStr[Counter] = '_' then
        LabelStr[Counter] := #32;

  Orient := 02;
  Flags := 01;
  BevelSize := 496;

  Width := 120;
  Height := 15;

  WriteLn;
  Write('!|1B' + WordToMega(Width) + WordToMega(Height) + WordToMega(Orient) + WordToMega4(Flags) + WordToMega(BevelSize) +
                 WordToMega(ForeColor) + WordToMega(00) + WordToMega(00) + WordToMega(07) +
                 WordToMega(08) + WordToMega(15) + WordToMega(00) + WordToMega(00) + '000000', #13);

  Write('!|1U' + WordToMega(X) + WordToMega(Y) + WordToMega(0) + WordToMega(0) +
                 WordToMega(0) + '0' {flag} + '0' + EscapeString(IconName) +
                 '<>' + EscapeString(LabelStr) + '<>' + EscapeString(HotKey), #13);
end; { proc. DisplayRipIcon }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure BullettMenu(MiscData: String);
var TempStr: String;
{$IFNDEF WINGUI}
    FName, Dir, Ext: String;
{$ELSE}
    FName: String;
{$ENDIF}
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMenuFunc, 'BulettMenu');
  {$ENDIF}

  repeat
    {$IFNDEF WINGUI}
      FSPlit(MiscData, Dir, FName, Ext);
    {$ELSE}
      FName := ExtractFileName(MiscData);
    {$ENDIF}

    if DisplayHotFile(MiscData, [])=#01 then
      begin
        WriteLn;
        Writeln('`A12:',LangObj^.ralGet(ralNoDataFil));
        Writeln;
        EXIT;
      end; { DisplayFile }

    Write(LangObj^.ralGet(ralViewFile));

    TempStr := '';
    GetString(TempStr, 8 - Length(FName), [#32..#254], False, False, ((8-Length(FName))=1), False);
    WriteLn;

    if TempStr<>'' then
      begin
        DisplayHotFile(MiscData+TempStr, []);
        InputObj^.PressEnter(false, true);
      end; { if }

  until (TempStr = '');
end; { proc. BullettMenu }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure DisplayDirect(OptData: String);   { Display direct, with CR at end }
var AreaNr : Word;
    RAInf  : FilesRecord;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMenuFunc, 'DisplayDirect (begin), OptData="'+OptData+'"');
  {$ENDIF}

  AreaNr := FVal(Trim(GetValue('/A=', OptData, True)));
  if AreaNr > 00 then GetFilesRecord(RaInf, AreaNr, True)
    else FillChar(RaInf, SizeOf(FilesRecord), #00);

  if AreaNr <> 00 then DisplayHotFile(RaInf.FilePath + OptData, [])
      else DisplayHotFile(OptData, []);

  WriteLn;
  WriteLn;
  OutputObj^.SetStopMore(False);
  InputObj^.PressEnter(True, True);

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMenuFunc, 'DisplayDirect ( end )');
  {$ENDIF}
end; { proc. DisplayDirect }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TerminateCall(AskFiles: Boolean);
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMenuFunc, 'TerminateCall (begin)');
  {$ENDIF}

  LineCfg^.DispMorePrompt := False;
  OutputObj^.ResetLines(01);
  WriteLn;
  WriteLn;
  WriteLn;

  if AskFiles then
   if GlobalCfg^.RaConfig^.TagLogOffWarning then
    begin
      if TaggingObj^.CurrentTagged > 00 then
       If NOT InputObj^.ralStrYesNoAsk(ralFilesTag) then Exit;
    end; { if }

  contrObj^.TimeInfo.InternodeChecking := false;
  RaLog('!', 'User requested to terminate call');

  DisplayHotFile('GOODBYE', []);
  InputObj^.UpdateScrn;
  OutBlockObj^.DumpBlock;

  if ComObj <> nil then
    ComObj^.Com_FlushOutBuffer({$IFDEF FPC}@{$ENDIF}DoSlice);

  HangUp;
end; { Terminate Call }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
Procedure ExitWithErrorLevel(OptData: String);                { Menu type -15 }
Const  UseHandle: Boolean = False;               { Use handle in DORINFO1.DEF }
       OldStyle: Boolean = False;        { Generate an old style DORINFO1.DEF }
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMenuFunc, 'ExitWithErrorLevel: '+OptData);
  {$ENDIF}

  UseHandle := (Pos('*A', OptData)>00);
  OldStyle := (Pos('*D', OptData)>00);

  Replace('*A', '', OptData);                    { Use handle in DORINFO1.DEF }
  Replace('*D', '', OptData);                        { Old style DORINFO1.DEF }

  CreateDorInfoDef(OldStyle, UseHandle, FixBaud(LineCfg^.Exitinfo^.Baud));
  CreateDoorSys(FixBaud(LineCfg^.Exitinfo^.Baud));
  CreateDoor32Sys(FixBaud(LineCfg^.Exitinfo^.Baud));
  WriteExitinfo(LineCfg^.Exitinfo^);

  RaLog('>', 'Exit to OS at errorlevel '+FStr(FVal(OptData)));
  InExitProcedure := true;    { This will make EleBBS skip the exit procedure }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMenuFunc, 'ExitWithErrorLevel, Optdata="'+OptData+'", ErrorLevel='+FStr(Fval(Trim(OptData))));
  {$ENDIF}

  System.Halt(FVal(Trim(OptData)));
end; { proc. ExitWithErrorLevel }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowVersionInformation;
var TempStr: String;
    AddStr : String;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMenuFunc, 'ShowVersionInformation (begin)');
  {$ENDIF}

  TempStr := '';
  AddStr := '';
  (*
  if GenInfo^.KeyLevel=3 then TempStr := RegAddStrFull;
  if GenInfo^.KeyLevel in [2, 5] then AddStr := '(beta site)';
  *)

  OutputObj^.ClearScreen;
  WriteLn;
  WriteLn;
  WriteLn('`A15:',FullProgName,' version v',VersionID+TempStr);
  WriteLn;
  WriteLn('`A11:Copyright 1996-2003 by Maarten Bekers, All rights reserved.');
  WriteLn;
  {$IFDEF MSDOS}
   {$IFDEF WITH_FULL}
     if ComObj <> nil then
      begin
        WriteLn('`A2:Communications driver: ', ComObj^.Com_GetDriverInfo);
        WriteLn;
      end; { FossilPtr }
   {$ENDIF}
  {$ENDIF}
  if RunsRegistered then
    Writeln('`A14:Registered to: ',GlobalCfg^.RaConfig^.SysOp,', ',GlobalCfg^.RaConfig^.SystemName, ' ', AddStr)
    {!! else WriteLn('`A14:Unregistered evaluation version.')};
  WriteLn;
  WriteLn('`A12:JAM(mbp) - Copyright 1993 Joaquim Homrighausen, Andrew Milner,');
  WriteLn('`X27:Mats Birch, Mats Wallin.');
  WriteLn('`X27:ALL RIGHTS RESERVED.');
  WriteLn;

  InputObj^.PressEnter(True, True);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMenuFunc, 'ShowVersionInformation ( end )');
  {$ENDIF}
end; { proc. ShowVersionInformation }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

Procedure ShowTimeStatistics;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMenuFunc, 'ShowTimeStatistics (begin)');
  {$ENDIF}

  WriteLn;
  WriteLn('`A14:',LangObj^.ralGet(ralCurrTime), '`A11:', TimeStr(False,False));{ Current time         : }
  WriteLn('`A14:',LangObj^.ralGet(ralCuurDate), '`A11:', DateStr);             { Current date         : }
  WriteLn;
  WriteLn('`A14:',LangObj^.ralGet(ralConnTime),'`A11:',LineCfg^.OnlineTime,' ', LangObj^.ralGet(ralMinutes));{ Connect time: }
  WriteLn('`A14:',LangObj^.ralGet(ralUsedTime),'`A11:',LineCfg^.Exitinfo^.Userinfo.Elapsed, { Time used today      : }
                       #32, LangObj^.ralGet(ralMinutes));
  WriteLn('`A14:',LangObj^.ralGet(ralRema2day),'`A11:',LineCfg^.Exitinfo^.TimeLimit,' ',LangObj^.ralGet(ralMinutes));
         { Time remaining today : }
  WriteLn('`A14:',LangObj^.ralGet(ralDailyLm1),'`A11:',LineCfg^.LimitsInfo^.LTime,' ',LangObj^.ralGet(ralMinutes));
        { Daily time limit     : }
  WriteLn;
  InputObj^.PressEnter(True, True);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMenuFunc, 'ShowTimeStatistics ( end )');
  {$ENDIF}
end; { ShowTimeStatistics }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure ToggleProc(Const Active, NotActive:Word; Var Current:Byte; Bit:Byte; OnOff: Boolean);
begin
  {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logSupport, 'ToggleProc');
  {$ENDIF}

  WriteLn;
  WriteLn;
  If NOT (ReadBit(Current, Bit)) then SetBit(Current, Bit)
    else ClearBit(Current, Bit);

  If OnOff then
   If NOT (ReadBit(Current, Bit)) then WriteLn('`A7:',LangObj^.ralGet(notActive),' ', LangObj^.ralGet(ralOff))
     else Writeln('`A10:',LangObj^.ralGet(Active),' ', LangObj^.ralGet(ralON1));

  If NOT OnOff then
   If NOT (ReadBit(Current, Bit)) then WriteLn('`A7:',LangObj^.ralGet(notActive))
     else Writeln('`A10:',LangObj^.ralGet(Active));

  InputObj^.PressEnter(False, True);
end; { proc. ToggleProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

{$ENDIF}


end. { unit MenuFunc }
