program EleNode;
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
{$IFDEF WIN32}
 {$IFNDEF VirtualPascal}
    {$APPTYPE CONSOLE}
 {$ENDIF}
{$ENDIF}
{$I COMPILER.INC}
(*
**
** EleNode, RemoteAccess v2.50 compatible nodelist compiler (for EleBBS)
**
** Copyright (c) 1996-2001 by Maarten Bekers
**
** Created : 26-Mar-1997
** Last update : 26-Mar-1997
**
**
*)
uses {$IFDEF DELCRT}
      Crt32,
     {$ENDIF}
     Global,
      CfgRec,
       MemMan,
        StrPath,
         Cases,
          WordStr,
           LongStr,
            GenFile,
             ReadCfg,
              CentrStr,
               BBSkey,
                GenDos,
                 WinTitle,
                  FileObj,
                   FileRout,
                    SysVars,

{$IFDEF MSDOS}
          Dos,
           Crt;
{$ENDIF}

{$IFDEF OS2}
           Dos,
            Crt;
{$ENDIF}

{$IFDEF WIN32}
           {$IFDEF VirtualPascal}
             SysUtils, 
             Crt, Dos;
           {$ENDIF} 
{$ENDIF}

{$IFDEF FPC}
        SysUtils,
         Dos,
         Crt;
{$ENDIF}


{$IFDEF WITH_FULL}
  To be able to compile EleFile/EleUser/EleNode the WITH_FULL conditional flag has to be disabled
{$ENDIF}

Const IncludeLists : Array[1..11] of String = ('', '', '', '', '',
                                               '', '', '', '', '', '');
      NrNodelists  : Byte = 00;
      DefaultCost  : Word = 00;

      MaxCostRec   = 150;

var CostArray: Array[1..MaxCostRec] of record
                                 NodeType   : Byte;                  { 0=Zone }
                                                                   { 1=Region }
                                                                     { 2=Host }
                                                                     { 3=Node }
                                 Number     : Word;    { Zone, region, net nr }
                                 Node       : Word; { Only with full node nrs }
                                 Cost       : Word;            { # of credits }
                                end; { CostArray }
    SaveAttr : Byte;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


procedure FatalError(S: String);
begin
  WriteLn;
  WriteLn(SystemMsgPrefix, S);
  WriteLn;
  Halt(255);
  TextAttr := SaveAttr;
end; { proc. FatalError }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function WildToName(S: String): String;

function IsNodeListName(S: String): Boolean;
begin
  IsNodeListName := False;

  S := JustExtension(S);
  If (S[1] in ['0'..'9']) AND
      (S[2] in ['0'..'9']) AND
       (S[3] in ['0'..'9']) then IsNodeListName := True;
end; { func. IsNodeListName }

var DirInfo: SearchRec;
    FirstName: String;
begin
  FirstName := '';

  FindFirst(S, anyFile - VolumeID, DirInfo);

  If IsNodeListName(Dirinfo.Name) then FirstName := DirInfo.Name;

  While DosError=00 do
    begin
      FindNext(DirInfo);

      If IsNodeListName(DirInfo.Name) then FirstName := DirInfo.Name;
    end; { while }

  FindClose(DirInfo);

  WildToName := ForceBack(JustPath(S)) + FirstName;
end; { func. WildToName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowHeader;
begin
  SaveAttr := TextAttr;
  TextAttr := LightGray;
  ClrScr;
  WriteLn;
  WriteLn('ELENODE; EleBBS nodelist index compiler, Version '+VersionID);
  WriteLn('         Copyright 1997-2003 Maarten Bekers, All rights reserved.');
  WriteLn;
  if NOT MemMan.AllocMem(GlobalCfg^.RaConfig, SizeOf(ConfigRecord), 'ConfigRecord', 'ShowHeader') then
    FatalError('Could not allocate enough memory!');

  if NOT MemMan.AllocMem(GlobalCfg^.ElConfig, SizeOf(EleConfigRecord), 'EleConfigRecord', 'ShowHeader') then
     FatalError('Could not allocate enough memory!');


  if NOT MemMan.AllocMem(LineCfg^.Exitinfo, SizeOf(ExitinfoRecord), 'ExitinfoRecord', 'ShowHeader') then
    FatalError('Could not allocate enough memory!');
end; { proc. ShowHeader }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GetIncludeList;
var Counter     : Byte;
    NodeInc_F   : pFileObj;
    NodeInc     : NodeIncRecord;
begin
  WriteLn(SystemMsgPrefix, 'Including: ');

  IncludeLists[01] := WildToName(GlobalCfg^.RaConfig^.NodeListPath + 'nodelist.*');

  for Counter := 01 to 10 do
    if ParamStr(Counter) <> '' then
      begin
        if (Pos('.', ParamStr(Counter)) = 0) OR (Pos('*', JustExtension(ParamStr(Counter))) > 0) then
          IncludeLists[Counter+1] := WildToName(GlobalCfg^.RaConfig^.NodeListPath + ParamStr(Counter))
           else IncludeLists[Counter+1] := GlobalCfg^.RaConfig^.NodeListPath + ParamStr(Counter);
      end; { if }

  New(NodeInc_F, Init);
  NodeInc_F^.Assign(GlobalCfg^.RaConfig^.NodeListPath + 'nodeinc.ra');
  NodeInc_F^.FileMode := ReadWriteMode + DenyWrite;
  if NOT NodeInc_F^.Create(1) then
    FatalError('Unable to create nodeinc.ra!');

  For Counter := 01 to 10 do
   If JustName(IncludeLists[Counter]) <> '' then
    begin;
      Writeln('  ', IncludeLists[Counter]);

      FillChar(NodeInc, SizeOf(NodeInc), #00);
      NodeInc := JustName(IncludeLists[Counter]);

      NodeInc_F^.BlkWrite(NodeInc, SizeOf(NodeIncRecord));
      if NodeInc_F^.IoResult > 00 then BREAK;

      Inc(NrNodelists);
    end; { NrNodelists }

  Dispose(NodeInc_F, Done);

  If NrNodelists=00 then
    begin
      MemMan.ReleaseMem(GlobalCfg^.RaConfig, SizeOf(ConfigRecord));
      MemMan.ReleaseMem(LineCfg^.Exitinfo, SizeOf(ExitinfoRecord));
      FatalError('Nothing to process!');
    end; { no nodelists }

end; { proc. GetIncludeList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GetCostTable;
var NodeCost_F: Text;
    NodeCost  : String;
    KeyWord   : String;
    Counter   : Byte;
    TempID    : String;
    NrZones,
    NrRegions,
    NrNets    : Word;
begin
 If NOT FileExist('nodecost.ctl') then
   begin
     WriteLn;
     WriteLn(SystemMsgPrefix, 'Could not find "nodecost.ctl", assuming zero cost for all nodes');
     EXIT;
   end; { No cost file found }

  FillChar(CostArray, SizeOf(CostArray), #00);

  WriteLn;
  WriteLn(SystemMsgPrefix, 'Cost control table:');

  Assign(NodeCost_F, 'nodecost.ctl');
  {$i-} System.Reset(NodeCost_F); {$I+}
  If IOResult>00 then EXIT;

  Counter := 00;

  While NOT Eof(NodeCost_F) do
   begin
    ReadLn(NodeCost_F, NodeCost);

    KeyWord := SUpcase(Trim(ExtractWord(NodeCost, 1, defExtractWord, false, false)));

    If KeyWord='DEFAULT' then DefaultCost := FVal(ExtractWord(NodeCost, 2, defExtractWord, false, false));
    If KeyWord='ZONE' then If Counter < MaxCostRec then
                              begin;
                                Inc(Counter);

                                CostArray[Counter].NodeType := 0;
                                CostArray[Counter].Number := FVal(ExtractWord(NodeCost, 2, defExtractWord, false, false));
                                CostArray[Counter].Cost := FVal(ExtractWord(NodeCost, 3, defExtractWord, false, false));
                              end; { Zone }
    If KeyWord='REGION' then If Counter < MaxCostRec then
                              begin;
                                Inc(Counter);

                                CostArray[Counter].NodeType := 1;
                                CostArray[Counter].Number := FVal(ExtractWord(NodeCost, 2, defExtractWord, false, false));
                                CostArray[Counter].Cost := FVal(ExtractWord(NodeCost, 3, defExtractWord, false, false));
                              end; { Zone }
    If KeyWord='NET' then If Counter < MaxCostRec then
                              begin;
                                Inc(Counter);

                                CostArray[Counter].NodeType := 2;
                                CostArray[Counter].Number := FVal(ExtractWord(NodeCost, 2, defExtractWord, false, false));
                                CostArray[Counter].Cost := FVal(ExtractWord(NodeCost, 3, defExtractWord, false, false));
                              end; { Zone }
    If KeyWord='NODE' then If Counter < MaxCostRec then
                              begin;
                                Inc(Counter);

                                CostArray[Counter].NodeType := 3;

                                TempID := ExtractWord(NodeCost, 2, defExtractWord, false, false);

                                CostArray[Counter].Number := FVal(Copy(TempID, 1, Pos('/', TempId)));
                                CostArray[Counter].Node := FVal(Copy(TempID, Pos('/', TempID), 255));

                                CostArray[Counter].Number := FVal(ExtractWord(NodeCost, 2, defExtractWord, false, false));
                                CostArray[Counter].Cost := FVal(ExtractWord(NodeCost, 3, defExtractWord, false, false));
                              end; { Zone }
  end; { while }

  NrZones := 00;
  NrRegions := 00;
  NrNets := 00;

  For Counter := 01 to MaxCostRec do
   Case CostArray[Counter].NodeType of
      01 : Inc(NrZones);
      02 : Inc(NrRegions);
      03 : Inc(NrNets);
   end; { Case }

  WriteLn('  Default cost : ', DefaultCost);
  WriteLn('  Zones        : ', NrZones);
  WriteLn('  Regions      : ', NrRegions);
  WriteLn('  Nets         : ', NrNets);

  {$i-} Close(NodeCost_F); {$I+}
  If IOResult>00 then;
end; { proc. GetCostTable }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CompileLists;
var ListCounter : Byte;
    NodeIdx_F   : pFileObj;
    NodeIdx     : NodeIdxRecord;
    NodeTXT_F   : Text;
    NodeTXT     : String;
    KeyWord     : String;
    NrZones,
    NrRegions,
    NrNets      : Word;
    CurZone,
    CurRegion,
    CurNet      : Word;
    CurNr       : Word;
    TempStr     : String;
    Counter     : Byte;
    OldFilePos  : LongInt;
    NewEntry    : Boolean;
begin
  WriteLn;
  Write(SystemMsgPrefix, 'Scanning and compiling:');

  New(NodeIdx_F, Init);
  NodeIdx_F^.Assign(GlobalCfg^.RaConfig^.NodeListpath + 'nodeidx.ra');
  NodeIdx_F^.FileMode := ReadWriteMode + DenyAll;
  if NOT NodeIdx_F^.Create(1) then
     FatalError('Unable to create nodeidx.ra! ');

  ListCounter := 00;
  NrZones := 00;
  NrRegions := 00;
  NrNets := 00;
  CurZone := 00;
  CurRegion := 00;
  CurNet := 00;
  NewEntry := False;

  repeat
    Inc(ListCounter);

    If IncludeLists[ListCounter] <> '' then
      begin
        Assign(NodeTXT_F, GlobalCfg^.RaConfig^.NodeListPath + JustName(Includelists[ListCounter]));
        {$i-} System.Reset(NodeTXT_F); {$I+}

        If IOResult=00 then
          begin
            While NOT Eof(NodeTXT_F) do
              begin
               {$IFNDEF __TMT__}
                 OldFilePos := TextFilePos(NodeTxt_F);
               {$ENDIF}

                {$i-} ReadLn(NodeTXT_F, NodeTXT); {$I+}
                if IOResult>00 then break;

                KeyWord := Trim(SUpCase(Copy(NodeTXT, 1, Pos(',', NodeTXT)-1)));

                TempStr := '';
                Counter := Length(KeyWord) + 02;
                While NodeTxt[Counter] <> ',' do
                 begin;
                   TempStr := TempStr + NodeTxt[Counter];
                   Inc(Counter);

                   If Counter > Length(NodeTxt) then Break;
                 end; { While }

                CurNr := FVal(TempStr);

                NewEntry := False;
                If KeyWord='ZONE' then begin
                                          Inc(NrZones);
                                          CurZone := CurNr;

                                          GotoXY(30, WhereY);
                                          Write('Zone ', CurNr:5);
                                          NewEntry := True;
                                       end; { Zone }
                If KeyWord='REGION' then begin
                                           Inc(NrRegions);
                                           CurRegion := CurNr;

                                           GotoXY(45, WhereY);
                                           Write('Region ', CurNr:5);
                                           NewEntry := True;
                                         end; { Region }
                If KeyWord='HOST' then begin
                                          Inc(NrNets);
                                          CurNet := CurNr;

                                          GotoXY(60, WhereY);
                                          Write('Net ', CurNr:5);
                                          NewEntry := True;
                                        end; { Nets }

                If NewEntry then
                  begin
                    FillChar(NodeIdx, SizeOf(NodeIdxRecord), #00);

                    If KeyWord='ZONE' then NodeIdx.NodeType := 00;
                    If KeyWord='REGION' then NodeIdx.NodeType := 01;
                    If KeyWord='HOST' then NodeIdx.NodeType := 02;
                    NodeIdx.Number   := CurNr;
                    NodeIdx.IncEntry := ListCounter;
                    NodeIdx.Pointer  := OldFilePos;

                    For Counter := 01 to MaxCostRec do
                     If CostArray[Counter].NodeType = NodeIdx.NodeType then
                      If CostArray[Counter].Number = NodeIdx.Number then
                       NodeIdx.Cost := CostArray[Counter].Cost;

                    NodeIdx_F^.BlkWrite(NodeIdx, SizeOf(NodeIdxRecord));
                    If IOResult>00 then;
                  end; { NewEntry }

              end; { While not eof }

             {$i-} Close(NodeTXT_F); {$I+}
             If IOResult>00 then;
          end; { File opened }
      end; { IncludeLists }

  Until (ListCounter+1>NrNodelists);

  Dispose(NodeIdx_F, Done);

  GotoXY(1, WhereY);
  ClrEol;

  WriteLn(SystemMsgPrefix, 'Compile summary: ');
  WriteLn('  Zones   : ', NrZones);
  WriteLn('  Regions : ', NrRegions);
  WriteLn('  Nets    : ', NrNets);
  WriteLn;
  Writeln('  Total   : ', NrZones + NrRegions + NrNets);
end; { proc. CompileLists }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowHelpScreen;
begin
  WriteLn(#32, SystemMsgPrefix, 'Command-line parameters:');
  WriteLn(Dup(#32, 09), 'No command-line parameters available');
end; { proc. ShowHelpScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

begin
  SetWindowTitle('EleNODE');

  {$ifdef With_Full}
  {$ifdef Overlay}
  InitErrorHandler;
  {$endif}
  {$endif}
  RunningBBS := False;

  New(GlobalCfg);
  New(LineCfg);
  InitBasicGlobalVars;
  InitBasicLineVars(LineCfg^);

  ShowHeader;
  ReadConfigRa;
  InitSystemNames;
  CheckRegistered;

  if (ParamStr(1) = '?') or (Copy(ParamStr(1), 2, 1) = '?') then
    begin
      ShowHelpScreen
    end
        else begin
               GetIncludeList;
               GetCostTable;
               CompileLists;
             end; { if }

  WriteLn;
  WriteLn(SystemMsgPrefix, 'Processing complete.');


  MemMan.ReleaseMem(GlobalCfg^.RaConfig, SizeOf(ConfigRecord));
  MemMan.ReleaseMem(LineCfg^.Exitinfo, SizeOf(ExitinfoRecord));
  TextAttr := SaveAttr;
end. { EleNode }
