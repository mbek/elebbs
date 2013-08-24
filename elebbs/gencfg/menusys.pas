unit MenuSys;
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
{$IFDEF WIN32}
  {$H-}
{$ENDIF}
(*
**
** MENUSYS.TPU, MENU system unit for ElCONFIG.
** (have fun with the code :)
**
** Copyright (c) 1998 by Maarten Bekers
**
** Created : 04-Nov-1998
** Last update : 04-Nov-1998
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses CfgRec, Global;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

type
   PullInfRec = record
                  mnuName     : String[35];
                  mnuHotKey   : Char;
                  mnuMenuID   : Word;
                  mnuDescrip  : String[80];
                  mnuSelect   : Boolean;
                  mnuRowNr    : Byte;
                  mnuCurColor : Byte;
                end; { PullInfRec }
   PullInfRecArray = array[1..45] of PullInfRec;

   PullRecord = record
                  Items    : Byte;
                  HiLight  : Integer;
                  X        : Byte;
                  Y        : Byte;
                  PullInf  : ^PullInfRecArray;
                  Width    : Byte;
                  Length   : Byte;
                  TopBar   : Boolean;
                  AddSpace : Boolean;
                  Title    : String[30];
                  SaveScrn : Pointer;
                  PosArray : Array[1..4] of record
                                              XInc: Integer;
                                              YDec: Integer;
                                            end; { PosArray }
                end; { PullRecord }

   MenuRec = record
               mnuName     : String[30];
               mnuHotKey   : Char;
               mnuMenuID   : Word;
               mnuDescrip  : String[80];
               mnuXpos     : Byte;
               mnuPullInf  : PullRecord;
               mnuTopBar   : Boolean;
             end; { MenuRec }

   MenuRecord = record
                  Items   : Byte;
                  HiLight : Byte;
                  MenuInf : Array[1..20] Of MenuRec;
                  Width   : Byte;
                  X,Y     : Byte;
                end; { MenuRecord }

var PullMenu        : ^MenuRecord;

    mnuFileMenu,
    mnuSystemMenu,
    mnuOptionsMenu,
    mnuModemMenu,
    mnuManagerMenu  : ^PullRecord;

procedure EnDisAbleMenu(pull: Pullrecord; Enable: Boolean);
procedure ShowMenuItems(Pull: PullRecord);
procedure InitPullMenu(var Menu: PullRecord);
procedure AddMenuItem(Var Menu:MenuRec; Name:String; HotKey:Char; ID:Word; Help:String; XPos:Byte; PullInf:PullRecord);
procedure AddPullItem(Var Pull:PullInfRec; Name:String; ID: Word; HotKey:Char; Help:String; RowNr: Byte);
procedure ShowMenu(var Pull:PullRecord; Disable: Boolean);
procedure RemoveMenu(var Pull: PullRecord);
procedure Empty(var CH:Char); {$IFDEF MSDOS} FAR; {$ENDIF}
procedure EmptyID(ID:Word); {$IFDEF MSDOS} FAR; {$ENDIF}

function  TopBar(Var Menus:MenuRecord):Word;
function  DoPullMenu(Var Pull:PullRecord; Var CH: Char; Remove, TopBar: Boolean):Word;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses GenCfg, ScrnU, StrEdit, ObjDec
       {$IFDEF WITH_DEBUG}
         ,Debug_U
       {$ENDIF};

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Empty(var CH:Char);
begin
end; { proc. Empty }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EmptyID(ID:Word);
begin
end; { proc. EmptyID }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure InitPullMenu(var Menu: PullRecord);
begin
  Menu.SaveScrn := nil;
end; { proc. InitPullMenu }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AddMenuItem(var Menu: MenuRec; Name:String; HotKey:Char; ID:Word; Help:String; XPos:Byte; PullInf:PullRecord);
begin
  With Menu do
   begin
     mnuName     := Name;
     mnuHotKey   := HotKey;
     mnuMenuID   := ID;
     mnuDescrip  := Help;
     mnuXPos     := XPos;
     mnuPullInf  := PullInf;
     mnuTopBar   := True;
   end; { With Menu }
end; { proc. AddMenu }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AddPullItem(Var Pull:PullInfRec; Name:String; ID:Word; HotKey:Char; Help:String; RowNr: Byte);
begin
  with Pull do
    begin
      mnuName   := Name;
      mnuHotKey := HotKey;
      mnuMenuID := ID;
      mnuDescrip:= Help;
      mnuSelect := True;
      mnuRowNr  := RowNr;
      mnuCurColor := mnuPullColor;
    end; { With }
end; { proc. AddPullItem }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowMenu(var Pull:PullRecord; Disable: Boolean);
var MenuColor : Byte;
    Fill      : Boolean;
    Add       : Byte;
    SaveDirect: Boolean;
begin
  SaveScreen(Pull.SaveScrn);

  If Disable then Menucolor := mnuDisabled
   else MenuColor := mnuBoxColor;

  If Disable then Fill:=False else Fill:=True;

  If Pull.AddSpace then Add := 2 else Add := 00;

  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := False;

  ShadFillBox(Pull.X, Pull.Y, (Pull.Width+Pull.X), (Pull.Length+1) + Pull.Y + Add, MenuColor, mnuStyle, Fill);

  DirectScrnUpdate := SaveDirect;
  WriteAT((Pull.Width+Pull.X - Length(Pull.Title)), Pull.Y, mnuTitleColor, Pull.Title);
end; { ShowMenu }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure RemoveMenu(var Pull: PullRecord);
begin
  RestoreScreen(Pull.SaveScrn);
end; { RemoveMenu }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowMenuItems(Pull: PullRecord);
var Teller    : Byte;
    SaveDirect: Boolean;
begin
  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;

  For Teller:=01 to Pull.Items do
   begin;
    if Teller=Pull.Items then DirectScrnUpdate := SaveDirect;

    If Pull.PullInf^[Teller].mnuSelect then
      If Pull.AddSpace then
        WriteAT(Pull.X+2+Pull.PosArray[Pull.PullInf^[Teller].mnuRowNr].XInc, (Pull.Y+Teller+1)
                -Pull.PosArray[Pull.PullInf^[Teller].mnuRowNr].YDec, Pull.PullInf^[Teller].mnuCurColor,
                Pull.PullInf^[Teller].mnuName)
        else WriteAT(Pull.X+1+Pull.PosArray[Pull.PullInf^[Teller].mnuRowNr].XInc, (Pull.Y+Teller)
               -Pull.PosArray[Pull.PullInf^[Teller].mnuRowNr].YDec, Pull.PullInf^[Teller].mnuCurColor,
               Pull.PullInf^[Teller].mnuName);
   end; { For teller }
end; { proc. ShowMenuItems }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EnDisAbleMenu(pull: Pullrecord; Enable: Boolean);
var MenuColor : Byte;
    Add       : Byte;
    SaveDirect: Boolean;
begin
  If Enable then MenuColor := mnuBoxColor
    else MenuColor := mnuDisabled;

  If Pull.AddSpace then Add := 2 else Add := 00;

  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;
  ShadFillBox(Pull.X, Pull.Y, (Pull.Width+Pull.X), (Pull.Length+1) + Pull.Y + Add, MenuColor, mnuStyle, False);
  DirectScrnUpdate := SaveDirect;
  WriteAT((Pull.Width+Pull.X - Length(Pull.Title)), Pull.Y, mnuTitleColor, Pull.Title);
end; { proc. EnDisAbleMenu }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function DoPullMenu(Var Pull:PullRecord; Var CH: Char; Remove, TopBar: Boolean):Word;
Var Teller  : Byte;

procedure ShowHiLight;
begin
  If Pull.AddSpace then WriteAT(Pull.X+2+Pull.PosArray[Pull.PullInf^[Pull.HiLight].mnuRowNr].XInc, (Pull.Y+Pull.HiLight+1)
                                -Pull.PosArray[Pull.PullInf^[Pull.Hilight].mnuRowNr].YDec,
                                mnuPullHiColor, Pull.PullInf^[Pull.HiLight].MnuName)
    else WriteAT(Pull.X+1+Pull.PosArray[Pull.PullInf^[Pull.HiLight].mnuRowNr].XInc, (Pull.Y+Pull.HiLight)
                 -Pull.PosArray[Pull.PullInf^[Pull.Hilight].mnuRowNr].YDec, mnuPullHiColor,
                 Pull.PullInf^[Pull.HiLight].MnuName);
end; { proc. ShowHiLight }

begin
 repeat
   If Pull.HiLight<01 then Pull.HiLight := Pull.Items;
   If Pull.HiLight>Pull.Items then Pull.HiLight := 01;

   While NOT Pull.PullInf^[Pull.HiLight].mnuSelect do
    begin
      If CH in [#80,#77] then Inc(Pull.HiLight)                      { if 'down' pressed }
       else Dec(Pull.HiLight);

      If Pull.HiLight>Pull.Items then Pull.Hilight := 01;
      If Pull.HiLight<01 then Pull.HiLight := Pull.Items;
      { This can result in an endless loop!! }
    end; { if }

   If Pull.PullInf^[Pull.HiLight].mnuDescrip<>'' then
    begin
      PartClear(mnuMsgXPos, mnuMsgYPos, mnuScrnWidth, mnuMsgYpos, mnuMsgColor, #32);
      WriteAT(mnuMsgXPos, mnuMsgYpos, mnuMsgColor, Pull.PullInf^[Pull.HiLight].mnuDescrip);
    end; { if Descrip<>'' then }

  DirectScrnUpdate := false;
  ShowMenuItems(Pull);
  DirectScrnUpdate := true;
  ShowHiLight;
  curMenuID := Pull.PullInf^[Pull.HiLight].mnuMenuID;

  CH := ReadKey;
  DirectScrnUpdate := false;

  If Remove then
   If Pull.AddSpace then WriteAT(Pull.X+2+Pull.PosArray[Pull.PullInf^[Pull.HiLight].mnuRowNr].XInc, (Pull.Y+Pull.HiLight+1)
                                 -Pull.PosArray[Pull.PullInf^[Pull.Hilight].mnuRowNr].YDec,
                                 Pull.PullInf^[Teller].mnuCurColor, Pull.PullInf^[Pull.HiLight].MnuName)
     else WriteAT(Pull.X+1+Pull.PosArray[Pull.PullInf^[Pull.Hilight].mnuRowNr].XInc, (Pull.Y+Pull.HiLight)
                  -Pull.PosArray[Pull.PullInf^[Pull.Hilight].mnuRowNr].YDec, Pull.PullInf^[Teller].mnuCurColor,
                  Pull.PullInf^[Pull.HiLight].MnuName);

  Case CH of
         #00 : Begin;
                CH := ReadKey;

                Case CH of
                   { Up    } #72 : Dec(Pull.HiLight);
                   { Down  } #80 : Inc(Pull.HiLight);
                   { Home  } #71 : Pull.HiLight := 01;
                   { End   } #79 : Pull.HiLight := Pull.Items;
                   { Left  } #75 : begin;
                                     if NOT TopBar then
                                       Dec(Pull.HiLight);

                                     If TopBar then
                                        begin;
                                          DoPullMenu := 00;
                                          exit;
                                        end; { Left }
                                   end; { if }
                   { Right } #77 : begin;
                                     if NOT TopBar then
                                      Inc(Pull.HiLight);

                                     If TopBar then
                                        begin
                                          DoPullMenu := 00;
                                          Exit;
                                        end; { Right }
                                   end; { if }
                End; { Case }
               End; { #00 }
     #13     : DoPullMenu := Pull.PullInf^[Pull.HiLight].mnuMenuID;
     #27     : DoPullMenu := 00;
  #32..#255  : For Teller := 01 to Pull.Items do
                If UpCase(Pull.PullInf^[Teller].mnuHotKey)=UpCase(CH) then
                    begin;
                      DoPullMenu := Pull.PullInf^[Teller].mnuMenuID;
                      Pull.HiLight := Teller;
                      ShowMenuItems(Pull);
                      ShowHiLight;
                      CH := #13;
                      Break;
                    end; { UpCase }
  End; { Case CH }

 Until CH in [#13, #27];
 DirectScrnUpdate := true;
end; { func. DoPullMenu }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function TopBar(var Menus:MenuRecord): Word;
var Teller    : Byte;
    LastKey   : Char;
    Temp      : Word;
    SaveDirect: Boolean;
    SaveScrn  : pointer;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFastScrn, 'TopBar (begin)');
  {$ENDIF}

  repeat
    SaveDirect := DirectScrnUpdate;
    DirectScrnUpdate := false;

    if Menus.HiLight>Menus.Items then Menus.HiLight := 01;
    if Menus.HiLight<01 then Menus.HiLight := Menus.Items;

    SavePart(SaveScrn, Menus.MenuInf[Menus.HiLight].mnuPullInf.X,
                       Menus.MenuInf[Menus.HiLight].mnuPullInf.Y,
                       Menus.MenuInf[Menus.HiLight].mnuPullInf.X +
                         Menus.MenuInf[Menus.HiLight].mnuPullInf.Width + 02,
                       Menus.MenuInf[Menus.HiLight].mnuPullInf.Y +
                         Menus.MenuInf[Menus.HiLight].mnuPullInf.Length + 02);

    for Teller := 01 to Menus.Items do
      WriteAT(Menus.MenuInf[Teller].mnuXPos, Menus.Y, mnuNormColor, Menus.MenuInf[Teller].mnuName);

    WriteAT(Menus.MenuInf[Menus.HiLight].mnuXPos, Menus.Y, mnuTopHiColor, Menus.MenuInf[Menus.HiLight].mnuName);
    curMenuID := Menus.MenuInf[Menus.HiLight].mnuMenuID;

    ShowMenu(Menus.MenuInf[Menus.HiLight].mnuPullInf, False);
    Temp := DoPullMenu(Menus.MenuInf[Menus.HiLight].mnuPullInf, LastKey, false, True);

    FreeMem(Menus.MenuInf[Menus.HiLight].mnuPullInf.SaveScrn, SizeOf(SaveArrayType));
    ShowMenu(Menus.MenuInf[Menus.HiLight].mnuPullInf, true);
    FreeMem(Menus.MenuInf[Menus.HiLight].mnuPullInf.SaveScrn, SizeOf(SaveArrayType));

    UpdateScreenBuffer(true);

    if Temp=00 then
     case LastKey of
       { Left  } #75 : Dec(Menus.HiLight);
       { Right } #77 : Inc(Menus.HiLight);
       { Escape} #27 : TopBar := 00;
     end; { Case LastKey }

     if LastKey in [#75, #77] then
        begin
           RestorePart(SaveScrn, false);
{          RestoreScreen(MainScreenSaver); }
{          SaveScreen(MainScreenSaver); }
        end { hih }
          else begin
                 FreeMem(SaveScrn, SizeOf(SavePartType));
                 SaveScrn := nil;
               end; { else }

  until LastKey in [#13, #27];

  TopBar := Temp;
  DirectScrnUpdate := SaveDirect;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFastScrn, 'TopBar ( end )');
  {$ENDIF}
end; { func. TopBar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit MENUSYS }
