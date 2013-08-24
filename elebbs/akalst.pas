unit AKALST;
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
{$S-}
(*
**
** AKALST.TPU, AKA's editor/lister
**
** Copyright (c) 1998-1999 by Maarten Bekers
**
** Created : 08-Mar-1999
** Last update : 30-May-1999
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses CfgRec, Area_lst;

procedure AkaLister(EnterEdit: Boolean);
procedure EditMsgAreaType(var MessageInf: MessageRecord;
                          var EleMsgInfo: EleMessageRecord;
                          var ChangeArray: GlobalChangeArray;
                          GlobalProcessing,
                          DoingEleMSG,
                          GlobChange        : Boolean);

var  AddressEditor_EnterEdit : Boolean;
     AddressEditor_Aborted   : Boolean;
     AddressEditor_Selected  : Longint;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses ListSys, ScrnU, StUtils, GenFile, Global,
     StrPath, AreaDef, LongStr, Cases, FileRout, StrEdit, FileObj,
     MenuSys, MemMan, BitWise;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

 var Akas_File               : pFileObj;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AkaLst_GetInfo(var Info1, Info2, Info3, Info4, Info5, Info6, Info7, Info8, Info9: List_StringType;
                         ItemNr: LongInt; HiLight: LongInt);
var Addr: NetAddress;
begin
  FillChar(Addr, SizeOf(NetAddress), 0);

  Info1 := '';
  Info2 := FStr(ItemNr);
  Info3 := '';
  Info4 := '';
  Info5 := '';
  Info6 := '';
  Info7 := '';
  Info8 := '';
  Info9 := '';

  if ItemNr in [1..10] then
    begin
      Addr := GlobalCfg^.RaConfig^.Address[Itemnr - 1];
    end; { if }

  if ItemNr in [11..250] then
    begin
      Akas_File^.Seek(Pred(ItemNr - 10) * SizeOf(NetAddress));
      Akas_File^.BlkRead(Addr, SizeOf(NetAddress));
    end; { if }

  AddrToString(Info1, Addr.Zone, Addr.Net, Addr.Node, Addr.Point);
  if Info1 = '0:0/0' then Info1 := '[Unused]';
end; { proc. AkaLst_GetInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function AkaLst_Seek(Str: String): LongInt;
var Addr: NetAddress;
    Temp: String;
begin
  AkaLst_Seek := -1;
  Akas_File^.Seek(0);

  While NOT Akas_File^.EOF do
    begin
      Akas_File^.BlkRead(Addr, SizeOf(NetAddress));

      AddrToString(Temp, Addr.Zone, Addr.Net, Addr.Node, Addr.Point);
      if Temp = '0:0/0' then Temp := '[Unused]';

      if SUpcase(Str) = SupCase(Copy(Temp, 1, Length(Str))) then
        begin
          AkaLst_Seek := Akas_File^.FilePos div SizeOf(NetAddress);
          BREAK;
        end; { if }
   end; { While NOT eof }

end; { func. AkaLst_Seek }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function AkaLst_KeyPress(CH: Char; HiLight: LongInt;
                         var HiBarPos, TopOfScrn: Longint;
                         var StartRange, EndRange: LongInt): LongInt;
var Addr: NetAddress;
begin
  AkaLst_KeyPress := -1;

  Case CH of
    #60 : begin
            if HiLight in [1..10] then
              GetAddress(Hilight - 01, Addr)
               else begin
                      Akas_File^.Seek((HiLight - 11) * SizeOf(NetAddress));
                      Akas_File^.BlkRead(Addr, SizeOf(NetAddress));
                    end; { if }

            PartClear(15, HiBarPos + 03, 40, HiBarPos + 03, 07, #32);
            EditAddress(16, HiBarPos + 03, Addr, true);

            if HiLight in [1..10] then
              SetAddress(Hilight - 01, Addr)
               else begin
                      Akas_File^.Seek((HiLight - 11) * SizeOf(NetAddress));
                      Akas_File^.BlkWrite(Addr, SizeOf(NetAddress));
                    end; { if }
          end; { case }
    #83 : begin
            if HiLight in [1..10] then
              GetAddress(Hilight - 01, Addr)
               else begin
                      Akas_File^.Seek((HiLight - 11) * SizeOf(NetAddress));
                      Akas_File^.BlkRead(Addr, SizeOf(NetAddress));
                    end; { if }

            FillChar(Addr, SizeOf(Addr), 0);

            if HiLight in [1..10] then
              SetAddress(Hilight - 01, Addr)
               else begin
                      Akas_File^.Seek((HiLight - 11) * SizeOf(NetAddress));
                      Akas_File^.BlkWrite(Addr, SizeOf(NetAddress));
                    end; { if }
          end; { case }
  end; { case }
end; { func. AkaLst_KeyPress }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function AkaLst_AbortCheck(CH: Char): Boolean;
begin
  if CH = #27 then AkaLst_AbortCheck := True
   else AkaLst_AbortCheck := False;

  if AddressEditor_Aborted then
    AkaLst_AbortCheck := true;
end; { func. AkaLst_AbortCheck }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function AkaLst_GetItems: LongInt;
begin
  AkaLst_GetItems := 250;
end; { func. AkaLst_GetItems }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function AkaLst_Activate(HiLight: LongInt; HiBarPos: LongInt): Longint;
var Temp: Longint;
begin
  AkaLst_Activate := -1;
  Temp := -1;

  if AddressEditor_EnterEdit then
    AkaLst_Activate := AkaLst_KeyPress(#60, HiLight, HiBarPos, TEmp, Temp, Temp)
     else begin
            AddressEditor_Selected := HiLight;
            AddressEditor_Aborted := true;
          end; { if }
end; { func. AkaLst_Activate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AkaLister(EnterEdit: Boolean);
begin
  AddressEditor_EnterEdit := EnterEdit;
  AddressEditor_Aborted := false;
  AddressEditor_Selected := -1;

  If NOT FileExist(AddressFileName) then
    if NOT CreateNewAkasBBS then
     begin
       CantCreate(JustName(AddressFileName), False);
       EXIT;
     end; { if }

  if NOT OpenFile(AddressFileName, ReadWriteMode + DenyWrite) then
      begin
        CantCreate(JustName(AddressFileName), True);
        EXIT;
      end; { if }

  New(Akas_File, Init);
  Akas_File^.Assign(AddressFileName);
  Akas_File^.FileMode := ReadWriteMode + DenyWrite;
  Akas_File^.Open(1);


  DoList(true,                                                  { View Window }
         15,
         21, 4, 0, 0, 0, 0, 0, 0, 0,
         14, 03, 41, mnuScrnLen - 4,                     { Window coordinates }
         ' AKA ',
         'Select your fidonet-style address - F2 to edit',
         {$IFDEF FPC}@{$ENDIF}AkaLst_GetInfo,
         {$IFDEF FPC}@{$ENDIF}AkaLst_Activate,
         {$IFDEF FPC}@{$ENDIF}AkaLst_Seek,
         {$IFDEF FPC}@{$ENDIF}AkaLst_AbortCheck,
         {$IFDEF FPC}@{$ENDIF}AkaLst_Keypress,
         {$IFDEF FPC}@{$ENDIF}AkaLst_GetItems,
         {$IFDEF FPC}@{$ENDIF}CreateNewAkasBBS,
         {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
         false,
         00, 00,
         true, false);

  Dispose(Akas_File, Done);
end; { proc. Akalister }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditMsgAreaType(var MessageInf: MessageRecord;
                          var EleMsgInfo: EleMessageRecord;
                          var ChangeArray: GlobalChangeArray;
                          GlobalProcessing,
                          DoingEleMSG,
                          GlobChange        : Boolean);
var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    SaveDirect: Boolean;
    TmpWord   : SmallWord;
begin
  if GlobalProcessing then
    if NOT DoingEleMSG then EXIT;

  AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'NewsGroupRecord');
  InitPullMenu(Menu);

  AddPullItem(Menu.PullInf^[01], 'JAM    ', 1, #00, 'Use JAM for this messagearea (recommended)', 01);
  AddPullItem(Menu.PullInf^[02], 'Hudson ', 2, #00, 'Use Hudson for this message area (only very small areas)', 01);
  AddPullItem(Menu.PullInf^[03], 'Squish ', 3, #00, 'Use Squish for this message area (compatibility only)', 01);

  Menu.Items   := 3;
  Menu.Width   := 10;
  Menu.Length  := 3;
  Menu.X       := 03;
  Menu.Y       := 6;
  Menu.HiLight := 1;
  Menu.AddSpace:= True;
  Menu.Title   := ' Type ';
  Menu.PosArray[1].XInc := 00;
  Menu.PosArray[1].YDec := 00;

  if NOT GlobalProcessing then
    ShowMenu(Menu, False);

  Choice := 00;

  repeat
    SaveDirect := DirectScrnUpdate;
    DirectScrnUpdate := false;

    if NOT GlobalProcessing then
      begin
        DirectScrnUpdate := SaveDirect;
        Choice := DoPullMenu(Menu, CH, False, False);
      end; { if }

    if GlobalProcessing then
      if Choice < Menu.Items then Inc(Choice)
        else Choice := 00;

    if GlobChange then
     if Choice > 00 then Menu.PullInf^[Choice].mnuCurColor := mnuGlobColor;


    Case Choice of
     1 : begin { jam }
            If (NOT GlobalProcessing)  then
              begin
                ClearBit(EleMsgInfo.Attribute, 2);
                SetBit(MessageInf.Attribute, 7);
              end
               else if DoGlobalChange(100 + Choice) then
                      begin
                        ClearBit(EleMsgInfo.Attribute, 2);
                        SetBit(MessageInf.Attribute, 7);
                      end; { if}
         end; { if }
     2 : begin { hudson }
            If (NOT GlobalProcessing)  then
              begin
                ClearBit(EleMsgInfo.Attribute, 2);
                ClearBit(MessageInf.Attribute, 7);
              end
               else if DoGlobalChange(100 + Choice) then
                      begin
                        ClearBit(EleMsgInfo.Attribute, 2);
                        ClearBit(MessageInf.Attribute, 7);
                      end; { if}
         end; { if }
     3 : begin { squish }
            If (NOT GlobalProcessing)  then
              begin
                SetBit(EleMsgInfo.Attribute, 2);
                ClearBit(MessageInf.Attribute, 7);
              end
               else if DoGlobalChange(100 + Choice) then
                      begin
                        SetBit(EleMsgInfo.Attribute, 2);
                        ClearBit(MessageInf.Attribute, 7);
                      end; { if}
         end; { if }
    end; { Case }

  until (Choice > 0);

  if NOT GlobalProcessing then
   if GlobChange then
     for Choice := 1 to Menu.Items do
       begin
         if Menu.PullInf^[Choice].mnuCurColor = mnuGlobColor then
           ChangeArray[100 + Choice] := true
            else ChangeArray[100 + Choice] := false;
       end; { For }

  RemoveMenu(Menu);
  ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. EditMsgAreaType }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


end. { unit AKALST }
