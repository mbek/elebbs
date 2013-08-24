unit NodeLst;
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
** NODELIST routines for EleBBS
**
** Copyright (c) 1997 by Maarten Bekers
**
** Created : 01-Nov-1997
** Last update : 01-Nov-1997
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

type NlTypeList=(nlZones, nlNets, nlNodes);

procedure BrowseNodelist;
procedure HandleNodeListInput(var NodeStr: String; AkaNr: Longint);
procedure MatchNodeList(var NodeStr: String; AkaNr: Longint);

{$IFNDEF WITH_FULL}
  This unit shouldn''t be compiled for an utility program
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
uses Global, Ral, Objdec, StUtils, Support, FileRout, LineEd,
      Colors, Cases, LongStr, CfgRec, CentrStr, WordStr, GenFile,
       Input_U, FileObj;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetIncEntry(IncEntry: Byte): String;
var NodeInc_F: File of NodeIncRecord;
    NodeInc  : NodeIncRecord;
begin
  GetIncEntry := '<>';

  Assign(NodeInc_F, GlobalCfg^.RaConfig^.NodeListPath + 'NODEINC.RA');
  FileMode := ReadMode + DenyNone;
  {$i-} System.Reset(NodeInc_F); {$i+}
  if IOResult>00 then EXIT;

  {$i+}
     Seek(NodeInc_F, Pred(IncEntry));
     Read(NodeInc_F, NodeInc);
     Close(NodeInc_F);
  {$i+}
  if IOResult>00 then ;

  GetIncEntry := NodeInc;
end; { func. GetIncEntry }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GetNodeInfo(TextPTR: Longint; IncEntry: Byte; var TypeName, Number, Name, Location: String);
var Nl_F     : Text;
    TempStr  : String;
    NodeInc  : NodeIncRecord;
begin
  TempStr  := '';
  TypeName := '';
  Number   := '';
  Name     := '';
  Location := '';

  NodeInc := GetIncEntry(IncEntry);

  Assign(NL_F, GlobalCfg^.RaConfig^.NodeListPath + NodeInc);
  {$i-}
    FileMode := ReadMode + DenyNone;
    System.Reset(Nl_F);
    TextSeek(Nl_F, TextPtr);
    ReadLn(Nl_F, TempStr);
    Close(Nl_F);
  {$i+}
  if IOResult>00 then EXIT;

  TypeName := Under2Norm(GetCommaArgument(TempStr, 1));
  Number   := Under2Norm(GetCommaArgument(TempStr, 2));
  Name     := Under2Norm(GetCommaArgument(TempStr, 3));
  Location := Under2Norm(GetCommaArgument(TempStr, 4));
  TypeName := SUpCase(TypeName);

  if TypeName='HOST' then TypeName := 'NET';
end; { proc. GetNodeInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowNodeInfo(TypeName, Number, Name, Location, Cost: String);
begin
  if Name <> '' then
    begin
      Write('`X01:`A10:', MakeLen(TypeName, 6, Space, False, false));
      Write('`X11:`A15:', MakeLen(Number, 08, Space, False, false));
      Write('`X19:`A02:', MakeLen(Name, 34, Space, False, false));
      Write('`X54:`A02:', MakeLen(Location, 15, Space, False, false));
      Write('`X73:`A14:', MakeLen(Cost, 4, Space, False, false));
      WriteLn;
    end; { if }
end; { proc. ShowNodeInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ListAllNodes(NodeIdx: NodeIdxRecord);
var NodeInc  : NodeIncRecord;
    Nl_F     : Text;
    TempStr  : String;
    TypeName : String;
    Number   : String;
    Name     : String;
    Location : String;
begin
  TempStr   := '';
  TypeName  := '';
  Number    := '';
  Name      := '';
  Location  := '';

  NodeInc := GetIncEntry(NodeIdx.IncEntry);

  Assign(NL_F, GlobalCfg^.RaConfig^.NodeListPath + NodeInc);
  {$i-}
    FileMode := ReadMode + DenyNone;
    System.Reset(Nl_F);
  {$i+}
  if IOResult>00 then EXIT;

  WriteLn('`A10:', langObj^.ralGet(ralNrName));
  WriteLn('`A02:', OutputObj^.AvatarDupl(#196, NoColorLength(langObj^.ralGet(ralNrName))));

  TextSeek(Nl_F, NodeIdx.Pointer);
  While (NOT Eof(Nl_F)) AND (NOT OutputObj^.StopMore) do
    begin
      {$i-} ReadLn(Nl_F, TempStr); {$i+}
      if IOResult>00 then BREAK;

      TypeName := SupCase(Under2Norm(GetCommaArgument(TempStr, 1)));
      Number   := Under2Norm(GetCommaArgument(TempStr, 2));
      Name     := Under2Norm(GetCommaArgument(TempStr, 3));
      Location := Under2Norm(GetCommaArgument(TempStr, 4));

      if TypeName='HOST' then TypeName:='NET';

      if TypeName<>'' then
       if (TypeName='NET') OR (TypeName='REGION') OR (TypeName='ZONE') then
        if FVal(Number) <> NodeIdx.Number then BREAK;

      ShowNodeInfo(TypeName, Number, Name, Location, FStr(NodeIdx.Cost));
    end; { while }

  WriteLn;
  WriteLn;

  {$i-} Close(Nl_F); {$i+}
  if IOResult>00 then ;
end; { proc. ListAllNodes }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ListIDX(ListType: nlTypeList; StartIDX: Longint);
var NodeIdx_F: File of NodeIdxRecord;
    NodeIdx  : NodeIdxRecord;

    TypeName : String;
    Number   : String;
    Name     : String;
    Location : String;
    CurZone  : Word;
begin
  Assign(NodeIdx_F, GlobalCfg^.RaConfig^.NodeListPath + 'nodeidx.ra');
  {$i-} System.Reset(NodeIdx_F); {$i+}
  if IOResult>00 then
     begin
       WriteLn('`A12:');
       WriteLn(langObj^.ralGet(ralNoNLhelp));
       InputObj^.PressEnter(False, True);
       EXIT;
     end; { if }

  if listType=nlNets then
    begin
      {$i-}
        Seek(NodeIDX_F, StartIDX - 01);
        Read(NodeIDX_F, NodeIdx);
      {$i+}
      if IOResult>00 then EXIT;
    end; { if }

  CurZone := NodeIdx.Number;

  WriteLn('`A10:', langObj^.ralGet(ralNrName));
  WriteLn('`A02:', OutputObj^.AvatarDupl(#196, NoColorLength(langObj^.ralGet(ralNrName))));

  While (NOT Eof(NodeIdx_F)) AND (NOT OutputObj^.StopMore) do
    begin
     Read(NodeIdx_F, NodeIdx);

     if NodeIdx.NodeType=00 then
      if ListType=nlNets then
       if NodeIdx.Number <> CurZone then EXIT;

     if ListType = nlZones then
      if NodeIdx.NodeType = 00 then                                    { Zone }
       begin
         GetNodeInfo(NodeIdx.Pointer, NodeIdx.IncEntry, TypeName, Number, Name, Location);
         ShowNodeInfo(TypeName, Number, Name, Location, FStr(NodeIdx.Cost));
       end; { if }

     if ListType = nlNets then
      if NodeIdx.NodeType <> 00 then
       begin
         GetNodeInfo(NodeIdx.Pointer, NodeIdx.IncEntry, TypeName, Number, Name, Location);
         ShowNodeInfo(TypeName, Number, Name, Location, FStr(NodeIdx.Cost));
       end; { if }
    end; { while }

  WriteLn;
  WriteLn;

  {$i-} Close(NodeIdx_F); {$I+}
  if IOResult>00 then ;
end; { proc. ListIDX }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function NodeIncSearch(Zone, Net: Word): Longint;
var NodeIdx_F: pFileObj;
    NodeIdx  : NodeIdxRecord;
    CurZone,
    CurNet   : Word;
    Found    : Boolean;
begin
  {-- Initialize some variables -------------------------------------------}
  NodeIncSearch := -1;
  CurZone := 00;
  CurNet := 00;
  Found := False;

  {-- Open the file -------------------------------------------------------}
  New(NodeIdx_F, Init);
  NodeIdx_F^.Assign(GlobalCfg^.RaConfig^.NodeListPath + 'nodeidx.ra');
  NodeIdx_F^.FileMode := ReadMode + DenyNone;
  if NOT NodeIdx_F^.Open(1) then
     begin
       Dispose(NodeIDX_F, Done);
       EXIT;
     end; { if }

  {-- Loop through the file -----------------------------------------------}
  While (NOT (NodeIDX_F^.EOF)) AND (NOT Found) do
    begin
      NodeIDX_F^.BlkRead(NodeIdx, SizeOf(NodeIdx));
      if NodeIDX_F^.IoResult > 0 then BREAK;

      {-- Loop through the INC file ---------------------------------------}
      if NodeIdx.NodeType = 00 then
       CurZone := NodeIdx.Number;

      if NodeIdx.NodeType in [1,2] then
       if CurZone = Zone then
        CurNet := NodeIdx.Number;

      {-- If no net specified, this will suffice --------------------------}
      if Net = 00 then
       if CurZone = zone then
         Found := True;

      {-- else go for a full match ----------------------------------------}
      if Net = CurNet then
       if CurZone = Zone then
         Found := TRUE;
    end; { while }

  {-- Show why it didnt work out ------------------------------------------}
  if NOT Found then
     begin
       WriteLn;
       WriteLn('`A11:', langObj^.ralGet(ralSorryNo), #32,
               langObj^.ralGet(ralNet) + #32, Net, #32,
               langObj^.ralGet(ralListed) + #32,
               langObj^.ralGet(ralZone), #32, 'in ', Zone);

     end { if not found }
      else NodeIncSearch := NodeIDX_F^.FilePos DIV SizeOf(NodeIdxRecord);

  {-- Close the node file -------------------------------------------------}
  Dispose(NodeIDX_F, Done);
end; { func. NodeIncSearch }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GetNodeNumber(const IdxPointer: Longint;
                       const NodeNr: Longint;
                       var Name, Location, Cost: String): Boolean;
var NodeIDX_F  : pFileObj;
    NodeList_F : pFileObj;
    NodeIDX    : NodeIdxRecord;
    NodeInc    : NodeIncRecord;
    CurZone    : Longint;
    CurNet     : Longint;
    Dummy      : String;
    Number     : String;
    TempStr    : String;
    TypeName   : String;
begin
  {-- Initialize some variables -------------------------------------------}
  CurZone := 0;
  CurNet := 0;
  GetNodeNumber := FALSE;

  {-- Open the file -------------------------------------------------------}
  New(NodeIdx_F, Init);
  NodeIdx_F^.Assign(GlobalCfg^.RaConfig^.NodeListPath + 'nodeidx.ra');
  NodeIdx_F^.FileMode := ReadMode + DenyNone;
  if NOT NodeIdx_F^.Open(1) then
     begin
       Dispose(NodeIDX_F, Done);
       EXIT;
     end; { if }


  {-- Seek to the correct position ----------------------------------------}
  NodeIdx_F^.Seek(Pred(IdxPointer) * SizeOf(NodeIdxRecord));

  {-- Read the desired record ---------------------------------------------}
  NodeIDX_F^.BlkRead(NodeIdx, SizeOf(NodeIdx));

  {-- Close the node file -------------------------------------------------}
  Dispose(NodeIDX_F, Done);

  {-- Now we know the position in the nodelist, lets read that one ... ----}
  {-- First get the filename of the actual nodelist -----------------------}
  NodeInc := GetIncEntry(NodeIdx.IncEntry);

  {-- Open te nodelist ----------------------------------------------------}
  New(Nodelist_F, Init);
  NodeList_F^.Assign(GlobalCfg^.RaConfig^.NodeListPath + NodeInc);
  NodeList_F^.FileMode := ReadMode + DenyNone;
  if NOT NodeList_F^.Open(1) then
    begin
      Dispose(NodeList_F, Done);
      EXIT;
    end; { if }

  {-- Seek to the correct position ----------------------------------------}
  NodeList_F^.Seek(NodeIdx.Pointer);

  {-- Now read through till the end (or ...) ------------------------------}
  While (NOT (NodeList_F^.EOF)) do
    begin
      {-- Read the first line in the nodelist -----------------------------}
      NodeList_F^.ReadLn(TempStr);

      {-- Now split up this into different records ------------------------}
      TypeName := SupCase(Under2Norm(GetCommaArgument(TempStr, 1)));
      Number   := Under2Norm(GetCommaArgument(TempStr, 2));
      Name     := Under2Norm(GetCommaArgument(TempStr, 3));
      Location := Under2Norm(GetCommaArgument(TempStr, 4));

      {-- Make sure we dont end up in another region/zone/etc -------------}
      if TypeName = 'HOST' then
        TypeName := 'NET';

      if TypeName <> '' then
       if (TypeName = 'NET') OR (TypeName = 'REGION') OR (TypeName = 'ZONE') then
        if FVal(Number) <> NodeIdx.Number then BREAK;

      {-- Did we found it? ------------------------------------------------}
      if TypeName = '' then
       if FVal(Number) = NodeNr then
         begin
           Cost := FStr(NodeIdx.Cost);
           GetNodeNumber := TRUE;
           BREAK;
         end; { if }
    end; { while }

  {-- Close the nodelist file ---------------------------------------------}
  Dispose(NodeList_F, Done);
end; { func. GetNodeNumber }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ListnodeList(ListType: nlTypeList; Zone, Net: Word);
var IDXPointer: Longint;
    IDX       : NodeIdxRecord;
    IDX_F     : File of NodeIDXrecord;
begin
  if ListType <> nlZones then
    begin
      IDXPointer := NodeIncSearch(Zone, Net);
      if IDXPointer = -1 then EXIT;
    end; { if }

  Case ListType of
    nlZones : ListIDX(nlZones, 01);
    nlNets  : ListIDX(nlNets,  IDXPointer);
    nlNodes : begin
                Assign(Idx_F, GlobalCfg^.RaConfig^.NodeListPath + 'NODEIDX.RA');

                {$i-}
                  System.Reset(Idx_F);
                  Seek(Idx_F, IdxPointer - 01);
                  Read(Idx_F, Idx);
                  Close(Idx_F);
                {$i+}
                if IOResult>00 then ;

                ListAllNodes(IDX);
              end;
  end; { case }

end; { proc. ListNodeList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure HandleNodeListInput(var NodeStr: String; AkaNr: Longint);
var Zone   : Word;
    Nets   : Word;
    TmpZone: Word;
    TmpNets: Word;
    Address: NetAddress;
begin
  if NodeStr <> '' then
   if NodeStr[1]='?' then
      begin
        WriteLn;
        WriteLn;
        WriteLn('`A15:',RaduCenter(langObj^.ralGet(ralAvailZone), 80));
        WriteLn;
        ListNodeList(nlZones, 0, 0);
        NodeStr := '';
      end; { if }

  GetAddress(AkaNr, Address);
  Zone := Address.Zone;
  Nets := Address.Net;

  If NodeStr <> '' then
   if (Pos(':', NodeStr)>00) then
    if (Pos('/', NodeStr)=00) then
     if (Pos('?', NodeStr)>00) then
      begin
        TmpZone := FVal(Copy(NodeStr, 1, Pos(':', NodeStr)-1));
        if TmpZone > 0 then
          Zone := TmpZone;

        WriteLn;
        WriteLn;
        WriteLn('`A15:',RaduCenter(langObj^.ralGet(ralNetsRegs) + #32 +
                FStr(Zone), 80));
        WriteLn;
        ListNodeList(nlNets, Zone, 0);
        NodeStr := '';
     end; { if }

  If NodeStr <> '' then
   if Pos('/', NodeStr)>00 then
      begin
        TmpZone := FVal(Copy(NodeStr, 1, Pos(':', NodeStr)-1));
        TmpNets := FVal(Copy(NodeStr, Pos(':',NodeStr)+1, Pos('/', NodeStr) - (Pos(':', NodeStr) + 01)));

        if TmpZone > 0 then
          Zone := TmpZone;

        if TmpNets > 0 then
          Nets := TmpNets;

        WriteLn;
        WriteLn;
        WriteLn('`A15:',RaduCenter(langObj^.ralGet(ralNodesIn) + #32 +
                 langObj^.ralGet(ralNet) + #32 + FStr(Nets), 80));
        WriteLn;
        ListNodeList(nlNodes, Zone, Nets);
        NodeStr := '';
      end; { if }
end; { proc. HandleInput }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure BrowseNodeList;
var NodeStr: String;
begin
  {-- Clear the screen nicely, and show some headers and instructions -----}
  OutputObj^.ClearScreen;
  Write('`A3:');

  {-- Now show the instructions -------------------------------------------}
  WriteLn(langObj^.ralGet(ralNlBrowse));
  WriteLn(langObj^.ralGet(ralListZones));
  WriteLn(langObj^.ralGet(ralListnets));
  WriteLn(langObj^.ralget(ralListNodes));
  WriteLn;

  {-- Let the user keep looking up new nodelist addresses -----------------}
  repeat
    NodeStr := '';
    Write('`A14:: ');
    GetString(NodeStr, 15, [#32..#254], false, false, false, false);

    {-- Make sure we dont have weird pauses in the midst of the screen ----}
    NodeStr := SUpCase(Trim(NodeStr));
    OutputObj^.ResetLines(1);
    WriteLn;
    WriteLn;
    WriteLn;

    {-- And show all this -------------------------------------------------}
    HandleNodeListInput(NodeStr, 0);
  until (NodeStr = '');
end; { proc. BrowseNodeList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function SearchNodeList(const Zone, Net, Node: Longint;
                        var Name, Location, Cost: String): Boolean;
var IdxPointer: Longint;
begin
  {-- Initialize with pre variables ---------------------------------------}
  Name := 'invalid';
  Location := 'invalid';
  Cost := '-1';
  SearchNodeList := FALSE;

  {-- Lookup the start of this zone/net -----------------------------------}
  IdxPointer := NodeIncSearch(Zone, Net);

  {-- If the zone/net combination is found, lookup the real info ----------}
  if IdxPointer = -1 then EXIT;

  {-- Get the real node-info ----------------------------------------------}
  if NOT GetNodeNumber(IdxPointer, Node, Name, Location, Cost) then EXIT;

  {-- Search nodelist -----------------------------------------------------}
  SearchNodeList := true;
end; { func. SearchNodelist }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure MatchNodeList(var NodeStr: String; AkaNr: Longint);
var Zone       : Word;
    Nets       : Word;
    TmpZone    : Word;
    TmpNets    : Word;
    Address    : NetAddress;
    Name       : String;
    Location   : String;
    Cost       : String;
    OrigNodeStr: String;
begin
  {-- Get the standard zone/net info --------------------------------------}
  OrigNodeStr := NodeStr;
  GetAddress(AkaNr, Address);
  Zone := Address.Zone;
  Nets := Address.Net;

  {-- Now default it ------------------------------------------------------}
  TmpZone := FVal(Copy(NodeStr, 1, Pos(':', NodeStr)-1));
  TmpNets := FVal(Copy(NodeStr, Pos(':',NodeStr)+1, Pos('/', NodeStr) - (Pos(':', NodeStr) + 01)));

  if TmpZone > 0 then
    Zone := TmpZone;

  if TmpNets > 0 then
    Nets := TmpNets;

  {-- Remove the zone number ----------------------------------------------}
  if Pos(':', NodeStr) > 0 then
    Delete(NodeStr, 1, Pos(':', NodeStr));

  {-- Remove the net number -----------------------------------------------}
  if Pos('/', NodeStr) > 0 then
    Delete(NodeStr, 1, Pos('/', NodeStr));

  {-- NodeNumber ----------------------------------------------------------}
  NodeStr := FStr(FVal(NodeStr));

  {-- Write an empty line -------------------------------------------------}
  WriteLn('`A3:');

  {-- Now find a match for this net/zone number ---------------------------}
  if NOT SearchNodeList(Zone, Nets, FVal(NodeStr), Name, Location, Cost) then
    begin
      WriteLn(LangObj^.ralGet(ralNoNode), ' ', OrigNodeStr, '.');
      WriteLn(LangObj^.ralGet(ralNodelHdr));
      NodeStr := '';
    end
      else begin
             {-- Reformat the nodestr -------------------------------------}
             NodeStr := FStr(Zone) + ':' + FStr(Nets) + '/' + NodeStr;

             {-- Ask the user for confirmation ----------------------------}
             WriteLn(NodeStr, ', ', Name, ', ', Location, ', ', Cost,
                     ' credits');

             {-- Ask the user wether its correct --------------------------}
             if NOT InputObj^.RalStrYesNoAsk(ralCorrect) then
               NodeStr := '';
           end; { else (did found it!) }
end; { proc. MatchNodeList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
