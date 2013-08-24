unit GenCFG;
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
{$DEFINE WITH_SWAPPER}

{$IFNDEF MSDOS}
  {$UNDEF WITH_SWAPPER}
{$ENDIF}
(*
**
** GENCFG.TPU, Support unit for ElCONFIG / QSetUp.
**
** Copyright (c) 1996 by Maarten Bekers
**
** Created : 16-Oct-1996
** Last update : 19-Sep-1997
**
**
** note: The usage of textcolor and/or textbackground will hang the pc.
** note2: Temporarily removed changing when left/right pressed.
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses
     {$IFDEF DELCRT}
       Crt32,
     {$ELSE}
       Crt,
     {$ENDIF}

     CfgRec, Global, ScrnU, Dos,
     {$IFDEF MSDOS}
       Exec,
       TpEnv,
     {$ENDIF}
     {$IFDEF WIN32}
       Windows,
     {$ENDIF}
     {$IFDEF OS2}
       Os2Def,
       Os2Base,
     {$ENDIF}
     Colors, StUtils, BitWise, StrEdit, Cases, Debug_U, LongStr, Ranges,
     StrPath, GenFile, MenuSys, ObjDec;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

var ParamString     : String;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  Bit2Str(Bits, Nr:Byte):String;
function  AskForPassword(S: String): Boolean;
function  Ask2Str(AskEdit: Byte): String;
function  ProtAvail2Str(B: Byte; Full: Boolean): String;
function  RDXDuplicate(FName: String; Areanr: SmallWord): Boolean;
function  DoSaveChanges(Msg: String; DefaultYes, AllowEsc: Boolean): Boolean;
function  Edit_Color (Editted: Word; var Fore, Back: Byte; Menu: PullRecord; DoMenu: Boolean) : Word;
function  Bool2Str(B: boolean): String;

procedure EditWeeks(var B: Byte);
procedure DoDosShell(ExitMsg: String);
procedure GenerateRdxFiles(AddPath: String; Message: Boolean; Group: Boolean; ShowToUser: Boolean);
procedure CleanString (var S: String);
procedure MakeDirectory(S: String);
procedure EditDirectory(X, Y, Len: Byte; var S: String);
procedure EditFlags(var IsOn,IsOff: Byte; OffSwitch: Boolean; X, Y: Byte; DisMenu: PullRecord);
procedure Color_Example(Temp: String; DefColor: Byte);



Procedure GenCfgInit;


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Edit_Color (Editted: Word; var Fore, Back: Byte; Menu: PullRecord; DoMenu: Boolean) : Word;

{$IFDEF ELEUNIX}
  Const ColChar = #42;
{$ELSE}
  Const ColChar = '*';
{$ENDIF}

var SaveColor  : Byte;
    XPos       : Integer;
    YPos       : Integer;
    CH         : Char;
    SaveScrn   : Pointer;
    SaveUpdate : Boolean;

procedure WriteChar(CONST Ch : Char);   { Uses Global Variables }
begin
  WriteAT(XPos + 60, YPos + 04, ((YPos-1)*16+(XPos-1)), CH);
  GotoXY(XPos + 60, YPos + 04);
end; { Func. WriteChat }

begin
  SaveScreen(SaveScrn);
  If DoMenu then EnDisAbleMenu(Menu, False);

  ShadFillBoxTitle(60, 04, 77, 13, mnuBoxColor, mnuStyle, True, ' Select colour ');

  SaveColor := Editted;
  SaveUpdate := DirectscrnUpdate;
  DirectScrnUpdate := false;
  For YPos:=01 to 08 do
   For XPos:=01 to 16 do
    WriteChar(Colchar);
  DirectScrnUpdate := SaveUpdate;

  YPos := GetBackAttr(Editted)+1;
  XPos := GetForeAttr(Editted)+1;

  if (NOT InRange(XPos, 0, 16)) then XPos := 16;
  if (NOT InRange(YPos, 0, 8)) then YPos := 8;

  repeat
   WriteChar(Chr(8));

   WriteAT(67, 13, mnuTitlecolor, ' ' +  ToHex(((YPos-1)*16+(XPos-1))) + ' ');

   CH := ReadKey;
   If CH=#00 then
    begin
      CH := ReadKey;
      Case Ord(CH) of
       72     : begin
                  WriteChar(ColChar);             { Schrijf een "ColChar" op oude positie }
                  Dec(YPos);                                   { Nieuwe positie }
                end; { Up }
       80     : begin
                  WriteChar(ColChar);
                  Inc(YPos);
                end; { Down }
       75     : begin
                  WriteChar(ColChar);
                  Dec(XPos);
                end; { Left }
       77     : begin
                 WriteChar(ColChar);
                 Inc(XPos);
                end; { Right }
    end; { Case }
   end; { If CH=#00 }

  If XPos=0 then XPos:=16;            { Positie aanpassen, indien "over rand" }
  If XPos=17 then XPos:=1;
  If YPos=0 then YPos:=8;
  If YPos=9 then YPos:=1;

  Until (CH=#13) OR (CH=#27);                 { Stoppen indien karakter [ENTER] }
  If (Ch=#27) then Edit_Color := SaveColor
   else Edit_Color := ((YPos-1)*16+(XPos-1));

  If NOT (Ch=#27) then begin
                         Fore := XPos-1;
                         Back := YPos-1;
                       end; { CH=#27 }

  If DoMenu then EnDisAbleMenu(Menu, True);
  RestoreScreen(SaveScrn);
end; { Func. Edit_Color }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF OS2}
procedure SetKeyBoard_RawMode;
var KBD: KbdInfo;
begin
  kbdGetStatus(kbd, 00);

  kbd.fsMask:=KEYBOARD_MODIFY_STATE or KEYBOARD_BINARY_MODE;

  kbdSetStatus(kbd, 00);
end; { proc. SetKeyboardRawMode }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Bit2Str(Bits, Nr:Byte): String;
begin
  If ReadBit(Bits, Nr) then Bit2Str:='Yes'
    else Bit2Str := 'No ';
end; { func. Bit2Str }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  AskForPassword(S: String): Boolean;
var SaveScrn: Pointer;
    TmpPsw  : String;
begin
  AskForPassword := TRUE;
  if S='' then EXIT;

  AskForPassWord := False;

  SaveScreen(SaveScrn);

  TmpPsw := '';
  ShadFillBoxTitle(31, 11, 49, 15, mnuBoxColor, mnuStyle, True, ' Password ');
  GetString(33, 13, mnuEditColor, TmpPsw, [#32..#255], [#13, #27], 15, 15, true, false, true, true, 0);

  RestoreScreen(SaveScrn);

  AskForPassword := (SUpCase(S)=TmpPsw);
end; { func. AskForPassword }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function RdxDuplicate(FName: String; AreaNr: SmallWord): Boolean;
var Idx_F: File;
    Idx  : SmallWord;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logFastScrn, 'RDXDuplicate: '+FName+', AreaNr='+FStr(Areanr));
  {$ENDIF}

  IDX := 00;
  Assign(Idx_F, FName);
  {$i-}
    FileMode := ReadMode + DenyNone;
    Reset(IDX_F, 1);
    Seek(IDX_F, (AreaNr-1) * 2);

    If (FileSize(Idx_F) / 2) < pred(AreaNr) then Idx := 00
     else BlockRead(Idx_F, Idx, SizeOf(Idx));

    Close(Idx_F);

  {$I+}
  If IOResult>00 then;

  RdxDuplicate := (IDX <> 00);
end; { func. RdxDuplicate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditWeeks(var B: Byte);
var Menu   : PullRecord;
    Choice : Word;
    CH     : Char;
    Counter: Byte;
begin
  New(Menu.PullInf);
  AddPullItem(Menu.PullInf^[01], 'Sunday   ', 01, #00, 'Enter-Toggle whether is active, Esc-quit', 1);
  AddPullItem(Menu.PullInf^[02], 'Monday   ', 02, #00, 'Enter-Toggle whether is active, Esc-quit', 1);
  AddPullItem(Menu.PullInf^[03], 'Tuesday  ', 03, #00, 'Enter-Toggle whether is active, Esc-quit', 1);
  AddPullItem(Menu.PullInf^[04], 'Wednesday', 04, #00, 'Enter-Toggle whether is active, Esc-quit', 1);
  AddPullItem(Menu.PullInf^[05], 'Thursday ', 05, #00, 'Enter-Toggle whether is active, Esc-quit', 1);
  AddPullItem(Menu.PullInf^[06], 'Friday   ', 06, #00, 'Enter-Toggle whether is active, Esc-quit', 1);
  AddPullItem(Menu.PullInf^[07], 'Saturday ', 07, #00, 'Enter-Toggle whether is active, Esc-quit', 1);

  Menu.Items   := 7;
  Menu.Width   := 15;
  Menu.Length  := 7;
  Menu.X       := 50;
  Menu.Y       := 11;
  Menu.HiLight := 01;
  Menu.AddSpace:= True;
  Menu.Title   := ' Days ';
  Menu.PosArray[1].XInc := 00;
  Menu.PosArray[1].YDec := 00;

  ShowMenu(Menu, False);

  Repeat
    for Counter:=0 to 06 do
      begin
        WriteAT(62, 13 + Counter, mnuNormColor, '-');
        If ReadBit(B, Counter) then
          WriteAT(62, 13 + Counter, mnuNormColor, 'X');
      end; { for counter }

    Choice := DoPullMenu(Menu, CH, False, False);

    If Choice>00 then If ReadBit(B, Choice-1) then ClearBit(B, Choice-1)
      else SetBit(B, Choice-1);

  until Choice=0;

  MenuSys.RemoveMenu(Menu);
end; { proc. Editweeks }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GenerateRDXFiles(AddPath   : String;
                           Message   : Boolean;
                           Group     : Boolean;
                           ShowToUser: Boolean);

Const RecordsToRead = 10;

var Temp_F      : File;
    MessageInf  : Array[0..RecordsToRead - 01] of MessageRecord;
    FilesInf    : Array[0..RecordsToRead - 01] of FilesRecord;
    GroupInf    : Array[0..RecordsToRead - 01] of GroupRecord;

    SaveScrn    : Pointer;
    AreaNum     : Array[0..RecordsToRead - 01] of Longint;
    DidRead     : NumReadType;

    IDX_F       : File;
    IDX         : SmallWord;
    Temp        : SmallWord;
    FTime       : LongInt;
    Counter     : Longint;

procedure CreateSeek(SeekPos: Smallword);
var Temp: SmallWord;
begin
  if SeekPos > (FileSize(IDX_F) div SizeOf(SmallWord)) then
   While SeekPos > (FileSize(IDX_F) div SizeOf(SmallWord)) do
     begin
       Temp := 00;                                   { It's an unused area }
       Seek(IDX_F, FileSize(IDX_F));                         { Seek to EOF }

       {$i-} BlockWrite(IDX_F, Temp, SizeOf(SmallWord)); {$i+}
       if IoResult>00 then BREAK;
     end; { while }

  Seek(IDX_F, (SeekPos * SizeOf(SmallWord)));
end; { proc. CreateSeek }

begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileIO, 'CreateSeek: Message/Group: '+Bool2Str(Message)+' / '+Bool2Str(Group));
  {$ENDIF}

  {---------------------------- Notify user --------------------------------}
  if ShowToUser then
    begin
      SaveScreen(SaveScrn);
      ShadFillBox(25, 10, 65, 14, mnuErrorColor, mnuStyle, True);
      WriteAT(33, 12, mnuErrorColor, 'One moment, re-indexing..');
    end; { if }

  {---------------------- Assign the appropriate files ---------------------}
  if (Message) AND (NOT Group) then
   begin
     Assign(Temp_F, ForceBack(AddPath) + 'messages.ra');
     Assign(IDX_F, ForceBack(AddPath) + 'messages.rdx');
   end; { if Message file }

  if (NOT Message) AND (NOT Group) then
   begin
     Assign(Temp_F, ForceBack(AddPath) + 'files.ra');
     Assign(IDX_F, ForceBack(AddPath) + 'files.rdx');
   end; { if FILES file }

  if (Group) then
   begin
     if Message then
      Assign(Temp_F, ForceBack(AddPath) + 'mgroups.ra')
       else Assign(Temp_F, ForceBack(AddPath) + 'fgroups.ra');

     if Message then
      Assign(IDX_F, ForceBack(AddPath) + 'mgroups.rdx')
       else Assign(IDX_F, ForceBack(AddPath) + 'fgroups.rdx');
   end; { if Group }

  {----------------------- Generic open routines ---------------------------}
  FileMode := ReadMode + DenyNone;
  {$i-} Reset(Temp_F, 1); {$i+}
  if IoResult>00 then
    begin
      if ShowToUser then RestoreScreen(SaveScrn);
      EXIT;
    end; { if }

  FileMode := ReadWriteMode + DenyAll;
  {$i-} ReWrite(IDX_F, 1); {$I+}
  if IoResult > 00 then
    begin
      if ShowToUser then RestoreScreen(SaveScrn);
      EXIT;
    end; { if }

  {-------------------------- Generate the RDX files -----------------------}
  While NOT EOF(Temp_F) do
    begin
      for Counter := 0 to (RecordsToRead - 01) do
        AreaNum[Counter] := -1;

      {$i-}
        if (Message) AND (NOT Group) then
          begin
            IDX := FilePos(Temp_F) div SizeOf(MessageRecord);
            BlockRead(Temp_F, MessageInf, SizeOf(MessageInf), DidRead);
            DidRead := DidRead DIV SizeOf(MessageRecord);

            for Counter := 0 to (DidRead - 01) do
              AreaNum[Counter] := MessageInf[Counter].AreaNum;
          end; { if messagerecord }

        if (NOT Message) AND (NOT Group) then
          begin
            IDX := FilePos(Temp_F) DIV SizeOf(FilesRecord);
            BlockRead(Temp_F, FilesInf, SizeOf(FilesInf), DidRead);
            DidRead := DidRead DIV SizeOf(FilesRecord);


            for Counter := 0 to (DidRead - 01) do
              AreaNum[Counter] := FilesInf[Counter].AreaNum;
          end; { filesrecord }

        if (Group) then
          begin
            IDX := FilePos(Temp_F) div SizeOf(GroupRecord);
            BlockRead(Temp_F, GroupInf, SizeOf(GroupInf), DidRead);
            DidRead := DidRead DIV SizeOf(GroupRecord);

            for Counter := 0 to (DidRead - 01) do
              AreaNum[Counter] := GroupInf[Counter].AreaNum;
          end; { if }

      {$i+}
      if IOResult>00 then BREAK;

      for Counter := 0 to (DidRead - 01) do
        if AreaNum[Counter] > 00 then
          begin
            CreateSeek(AreaNum[Counter] - 1);
            Temp := (IDX + Counter) + 01;

            {$i-} BlockWrite(IDX_F, Temp, SizeOf(IDX)); {$I+}
            if IOResult>00 then BREAK;
          end; { for }

    end; { while }

  {------------------------ Close files and update timestamp ----------------}
  {$i-}
    Close(Temp_F);
    Close(IDX_F);
  {$i+}
  if IOResult>00 then ;

  if ShowToUser then RestoreScreen(SaveScrn);
end; { proc. GenerateRdxFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ProtAvail2Str(B: Byte; Full: Boolean): String;
var TempStr: String;
begin
  TempStr := '';

  case B of
    00 : TempStr := 'Not available';
    01 : If Full then TempStr := 'Always Available'
            else TempStr := 'Available';
    02 : TempStr := 'Error free';
  end; { Case }

  If Full then TempStr := MakeLen(TempStr, Length('Always available'), space, false, false)
   else TempStr := MakeLen(TempStr, Length('Not available'), space, false, false);

  ProtAvail2Str := TempStr;
end; { func. ProtAvail2Str }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function DoSaveChanges(Msg: String; DefaultYes, AllowEsc: Boolean): Boolean;
var CH      : Char;
    StartX  : Byte;
    SaveScrn: Pointer;
    GoodSet : CharSet;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileIO, 'DoSaveChanges: '+Msg+ ' (begin)');
  {$ENDIF}

  StartX := (80 div 2) - (Length(Msg) div 2);
  Dec(StartX, 2);                                       { Shadow counting etc }

  SaveScreen(SaveScrn);

  ShadFillBox(StartX, 10, StartX + Length(Msg) + 6, 14, mnuErrorColor, mnuStyle, True);
  WriteAT(StartX + 3, 12, mnuErrorColor, Msg);
  UpdateScreenBuffer(true);

  CursorOn;

  Goodset := ['Y', 'N'];
  if AllowEsc then
    GoodSet := GoodSet + [#27];

  repeat
    GotoXY(StartX + 3 + (Length(msg) -1), 12);
    CH := UpCase(ReadKey);

    If CH=#13 then
     begin
       if DefaultYes then CH := 'Y'
        else CH := 'N';
     end; { if }

  until CH in GoodSet;

  If Ch='Y' then DoSaveChanges := True
   else DoSaveChanges := False;

  RestoreScreen(SaveScrn);

  While Keypressed do ReadKey;

  CursorOff;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileIO, 'DoSaveChanges: '+Msg+ ' ( end )');
  {$ENDIF}
end; { func. DoSaveChanges }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditFlags(var IsOn,IsOff: Byte; OffSwitch: Boolean; X, Y: Byte; DisMenu: PullRecord);
var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    Counter   : Byte;
    SaveDirect: Boolean;
begin
  SaveDirect := DirectScrnUpdate;
  DirectscrnUpdate := false;
  EnDisAbleMenu(DisMenu, False);

  New(Menu.PullInf);
  AddPullItem(Menu.PullInf^[01], '#1', 01, #00, 'Press ENTER to toggle flag setting, ESC when done', 1);
  AddPullItem(Menu.PullInf^[02], '#2', 02, #00, 'Press ENTER to toggle flag setting, ESC when done', 1);
  AddPullItem(Menu.PullInf^[03], '#3', 03, #00, 'Press ENTER to toggle flag setting, ESC when done', 1);
  AddPullItem(Menu.PullInf^[04], '#4', 04, #00, 'Press ENTER to toggle flag setting, ESC when done', 1);
  AddPullItem(Menu.PullInf^[05], '#5', 05, #00, 'Press ENTER to toggle flag setting, ESC when done', 1);
  AddPullItem(Menu.PullInf^[06], '#6', 06, #00, 'Press ENTER to toggle flag setting, ESC when done', 1);
  AddPullItem(Menu.PullInf^[07], '#7', 07, #00, 'Press ENTER to toggle flag setting, ESC when done', 1);
  AddPullItem(Menu.PullInf^[08], '#8', 08, #00, 'Press ENTER to toggle flag setting, ESC when done', 1);

  Menu.Items   := 8;
  Menu.Width   := 9;
  Menu.Length  := 8;
  Menu.X       := X;
  Menu.Y       := Y;
  Menu.HiLight := 01;
  Menu.AddSpace:= True;
  Menu.Title   := ' Flags ';
  Menu.PosArray[1].XInc := 00;
  Menu.PosArray[1].YDec := 00;

  ShowMenu(Menu, False);

  repeat
    for Counter:=0 to 07 do
      begin
        WriteAT(X+6, Y+Counter+2, mnuNormColor, '-');
        If ReadBit(IsOn, Counter) then WriteAT(X+6, Y+Counter+2, mnuNormColor, 'X');

        If OffSwitch then
          If ReadBit(IsOff, Counter) then WriteAT(X+6, Y+Counter+2, mnuNormColor, 'O');
      end; { for counter }

   DirectScrnUpdate := SaveDirect;
   Choice := DoPullMenu(Menu, CH, False, False);

   If NOT OffSwitch then
    If Choice>00 then If ReadBit(IsOn, Choice-1) then ClearBit(IsOn, Choice-1)
      else SetBit(IsOn, Choice-1);

   If OffSwitch then
    If Choice>00 then
     begin;
       IF (NOT ReadBit(IsOn, Choice-1)) AND (NOT ReadBit(IsOff, Choice-1)) then
         SetBit(IsOn, Choice-1)
          else
           If ReadBit(IsOff, Choice-1) then ClearBit(IsOff, Choice-1)
            else
             If ReadBit(IsOn, Choice-1) then begin
                                               ClearBit(IsOn, Choice-1);
                                               SetBit(IsOff, Choice-1);
                                             end; { Off to on }
     end; { Off is also an option }

  until Choice=0;

  RemoveMenu(Menu);
  Dispose(Menu.PullInf);

  EnDisAbleMenu(DisMenu, True);
end; { proc. EditFlags }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Bool2Str(b:boolean):string;
begin
  if B Then Bool2Str := 'Yes'
   else Bool2Str := 'No ';
end; { func. Bool2Str }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function  Ask2Str(AskEdit: Byte): String;
begin
  Ask2Str := '';

  Case Byte(AskEdit) of
     Byte(Yes)  : Ask2Str := 'Yes ';
     Byte(No)   : Ask2Str := 'No  ';
     Byte(Ask)  : Ask2Str := 'Ask ';
     Byte(Only) : Ask2Str := 'Only';
  end; { Case }
end; { func. Ask2Str }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure Color_Example(Temp: String; DefColor: Byte);
var SaveScrn   : Pointer;
    SaveDirect : Boolean;
    OldX,
    OldY       : Byte;
    CH         : Char;
    StartX     : Byte;
    MaxStart   : Byte;
    CurColor   : Byte;
    StrRec     : Array[1..255] of record
                                    CH : Char;
                                    At : Byte;
                                  end; { record }

function NoColorLength(S: String): Byte;
begin
  RemoveRaColors(S);
  NoColorLength := Length(s);
end; { func. NoColorLength }


procedure MakeRecord(S: String);
var Counter  : Byte;
    RecCount : Byte;
    TempCol  : String;
begin
  FillChar(StrRec, SizeOf(StrRec), 00);

  RecCount := 00;
  Counter := 01;

  while Counter <= Length(s) do
   begin
     Case Temp[Counter] of
        ^K : begin
                TempCol := Temp[Counter + 02] + Temp[Counter + 03];
                CurColor := ToDec(TempCol);
                Inc(Counter, 3);
             end; { if }
         else begin
                Inc(RecCount);
                StrRec[RecCount].At := CurColor;
                StrRec[RecCount].CH := Temp[Counter];
              end; { else }
     end; { case }

     Inc(Counter);
   end; { for }

end; { proc. MakeRecord }

procedure ShowString(Start: Byte);
var Counter  : Byte;
begin
  Counter := 00;

  For Counter := Start to Min(Start + 69, Length(Temp)) do
   WriteAT(05 + (Counter - Start), 06, StrRec[Counter].At, StrRec[Counter].CH);
end; { proc. ShowString }


begin
  SaveScreen(SaveScrn);
  OldX := WhereX;
  OldY := WhereY;
  CurColor := DefColor;
  if CurColor=00 then CurColor := 07;

  ShadFillBoxTitle(03, 04, 76, 08, mnuBoxColor, mnuStyle, True, ' Color example ');
  WriteAT(05, 08, mnuMsgColor, ' Press escape to exit, cursor keys to scroll ');

  GotoXY(05, 06);
  CursorOff;
  StartX := 01;

  if NoColorLength(Temp) <= 69 then MaxStart := 01
   else MaxStart := Byte(NoColorLength(Temp) - 69);

  MakeRecord(Temp);

  repeat
    if StartX < 01 then StartX := 01;
    if StartX > MaxStart then StartX := MaxStart;

    SaveDirect := DirectScrnUpdate;
    DirectscrnUpdate := false;
    ShowString(StartX);
    DirectScrnUpdate := SaveDirect;
    UpdateScreenBuffer(true);


    CH := ReadKey;

    Case CH of
      #00 : begin
              CH := ReadKey;
              Case CH of
                #75 : Dec(StartX);
                #77 : Inc(StartX);
              end; { case }
            end; { case }
    end; { case }
  until (CH=#27);

  RestoreScreen(SaveScrn);
  GotoXY(OldX, OldY);
end; { proc. Color_Example }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoDosShell(ExitMsg: String);
var OldHeapEnd   : Pointer;
    SaveScrn     : Pointer;
    DoSwap       : Word;
    Memory_To_Use: Integer;
{$IFDEF MSDOS}
    Env          : EnvRec;
{$ENDIF}
    SavePrompt   : String;
    SaveDir      : String;
begin;
   {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logFastScrn, 'DoDosShell (begin)');
   {$ENDIF}

   {$IFDEF TCPIP}
     if RemScrnObj <> nil then
       begin
         SaveScreen(SaveScrn);

         ShadFillBox(10, 09, 70, 14, 112, mnuStyle, True);
         WriteCenterPos(12, 12, 59, 112, 'OS shell not supported while in EleMON');
         GetInput;

         RestoreScreen(SaveScrn);
         EXIT;
       end; { if }
   {$ENDIF}


   SaveScreen(SaveScrn);

   TextColor(LightGray);
   TextBackGround(Black);
   ScreenClear(White, #32);

   GotoXY(01, 01);
   WriteLn;
   WriteLn(ExitMsg);
   WriteLn;
   WriteLn;

   CursorOn;                                                 { Do the cursor on }

{$IFDEF MSDOS}
   CurrentEnv(Env);
   SavePrompt := GetEnv('PROMPT');
   SetEnvStr(Env, 'PROMPT', 'DOS Shell$_'+GetEnv('PROMPT'));
{$ENDIF}
   GetDir(0, SaveDir);

{$IFNDEF WITH_SWAPPER}
 {$IFDEF MSDOS}
   OldHeapEnd := HeapEnd;                                    { Save old HeapEnd }
   HeapEnd := HeapPtr;                           { Set the HeapEnd to Heap-Used }
   SetMemTop(HeapEnd);
 {$ENDIF}

 {$IFNDEF DELPHI}
   {$IFNDEF ELEUNIX}
     Dos.Exec(GetEnv('COMSPEC'), '');                          { Do Dos Shell }
   {$ELSE}
     Dos.Exec(GetEnv('SHELL'), '-i');
   {$ENDIF}
 {$ENDIF}

 {$IFDEF MSDOS}
   HeapEnd := OldHeapEnd;                                     { Restore heapend }
   SetMemTop(HeapEnd);
 {$ENDIF}
{$ENDIF}

{$IFDEF WITH_SWAPPER}
   Memory_To_use := HIDE_FILE or CHECK_NET or USE_FILE;
   If GlobalCfg^.RaConfig^.UseXMS then
    Memory_To_Use := Memory_To_Use or USE_XMS;
   If GlobalCfg^.RaConfig^.UseEMS then
    Memory_To_Use := Memory_To_Use or USE_EMS;
   DoSwap := $ffff;
   Do_Exec(GetEnv('COMSPEC'), '', Memory_To_Use, DoSwap, false);
{$ENDIF}

{$IFDEF MSDOS}
   SetEnvStr(Env, 'PROMPT', SavePrompt);
{$ENDIF}
   CursorOff;                                               { Do the cursor off }
   ChDir(SaveDir);

   RestoreScreen(SaveScrn);                           { Restore original screen }

   {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logFastScrn, 'DoDosShell ( end )');
   {$ENDIF}
end; { proc. DoDosShell }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


procedure MakeDirectory(S: String);
var Counter   : Byte;
    TempPos   : Byte;
    TempDir   : String;
    AddBefore : String;
    CH        : Char;
    SaveScrn  : Pointer;
    WholeDir  : String;
begin
  if S = '' then EXIT;
  S := ForceBack(S);
  WholeDir := s;

  If (NOT GenFile.FileExist(S)) AND (S<>'') then
      begin
        SaveScreen(SaveScrn);

        ShadFillBox(10, 09, 70, 15, 112, mnuStyle, True);
        WriteCenterPos(11, 11, 59, 112, S);
        WriteCenterPos(11, 13, 59, 112, 'Path does not exist. Create it (Y/n) ? °');
        CursorOn;
        GotoXY(59, 13);                                     { Position cursor }

        repeat
          CH := UpCase(ReadKey);

          If CH=#13 then CH := 'Y';
        until CH in ['Y', 'N'];

        CursorOff;
        RestoreScreen(SaveScrn);

        If UpCase(CH)='N' then Exit;
      end else EXIT; { If NOT FileExist }

  While S[Length(S)]=BsChar do Dec(S[0]);              { Remove trailing slashes }

  Counter := 00;

  AddBefore := '';

  If Copy(S, 2, 2)=':'+BsChar then AddBefore := Copy(S, 1, 3);
  If Copy(S, 2, 2)=':'+BsChar then S := Copy(S, 4, 255);           { Remove C:\ etc }

  if S[1]=BsChar then begin;
                      S := Copy(S, 2, 255);
                      AddBefore := BsChar;
                   end; { Remove leading backslash }

  TempPos := 00;

  TempDir := '';

  REPEAT
    If TempDir<>'' then
     TempDir := TempDir + BsChar;           { Add multiple directory seperator }

    Inc(TempPos);
    While (S[TempPos]<>BsChar) AND (TempPos<=Length(S)) do
                             begin;
                               TempDir := TempDir + S[TempPos];
                               Inc(TempPos);
                             end; { While }

    {$i-} MkDir(AddBefore + TempDir); {$I+}
    If IOResult>00 then ;

    Inc(Counter);
  Until (S='') OR (Counter>250) OR (TempDir=S);

  if IOResult>00 then;

  If (NOT GenFile.FileExist(WholeDir)) then
      begin
        SaveScreen(SaveScrn);

        ShadFillBox((mnuScrnWidth DIV 2) - 15, 09, (mnuScrnWidth DIV 2) + 15, 13, 112, mnuStyle, True);
        WriteAT(((mnuScrnWidth DIV 2) - 15) + 2, 11, 112, 'Unable to create directory');
        GetInput;

        RestoreScreen(SaveScrn);
      end; { if }
end; { proc. MakeDirectory }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditDirectory(X, Y, Len: Byte; var S: String);
var CH: Char;
begin
  GetString(X, Y, mnuEditColor, S, [#32..#255], [#13, #27], Len, Len, false, False, True, false, 0);

  if NOT IsFtpUrl(S) then
    begin
      MakeDirectory(S);
      S := ForceBack(S);
    end; { if }
end; { EditDirectory }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CleanString (var S: String);
begin
 { --- Commented out
  If (Length(S) < 255) Then
   if (SizeOf(S) < Length(S)) then
    FillChar (S[Length(S)+1], SizeOf(S) - Length(S) - 1, #00);
 ----------- }
end; { proc. CleanString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GenCfgInit;
var F           : File;
    CH          : Char;
    Counter     : Byte;
    Mode        : Longint;
begin
  ParamString := '';
  For Counter :=01 to ParamCount do
   If Counter<>ParamCount then ParamString := ParamString + SupCase(ParamStr(Counter)) + ' '
     else ParamString := ParamString + SupCase(ParamStr(Counter));

  if pos('/DEBUGLOG', ParamString)>00 then DebugObj.DebugLogging := True;
  if pos('/DEBUGLOG', ParamString)>00 then DebugObj.DebugLogging := True;
  if (Pos('/ND', ParamString)>00) then                  { NoDouble frames }
    MnuStyle := 01;
  if (Pos('/SIMPLE', ParamString) > 00) OR (Pos('-SIMPLE', ParamString) >0) then
    begin
      mnuStyle := 06;
      mnuBackGrChar := #32;
      mnuBlockChar := #32;
      mnuBackGrCol := 16;
      mnuSngWide := '-';
      mnuDblWide := '=';
      mnuDnArrow := 'v';
      mnuUpArrow := '^';
    end; { if }

  if (NOT ColorOn) OR (Pos('/B', ParamString)>0) OR (Pos('-B', ParamString)>0) then
    begin
      mnuNormColor   := White;
      mnuBoxColor    := Lightgray;
      mnuTitleColor  := 112;
      mnuShadowColor := 00;
      mnuDisabled    := Lightgray;
      mnuTopHiColor  := 112;            { MenuItem color when hilighted }
      mnuPullHiColor := 112;
      mnuMsgColor    := LightGray;
      mnuMsgXPos     := 01;
      mnuTopBar      := Lightgray;
      mnuErrorColor  := 112;
      mnuEditColor   := 112;                            { Blue on green }
      mnuClearColor  := Cyan;
      mnuHiLightColor:= 112;
      mnuGlobColor   := LightGray + Blink;         { Blinking lightgray }
      mnuGlobSelect  := 112;                            { Blue on White }
    end; { coloron }

  {-- Switch to ProBoard mode :-) ------------------------------------------}
  if (Pos('/PB', ParamString) > 0) then
    begin
      mnuStyle := 2;
      mnuBackGrCol := MakeAttr(Black, Lightgray);
      mnuBackGrChar := #176;
      mnuTopHiColor  := MakeAttr(Black, LightGray);
      mnuPullHiColor := MakeAttr(Black, LightGray);
      mnuBoxColor := MakeAttr(White, Blue);
      mnuPullColor := MakeAttr(LightCyan, Blue);
      mnuNormColor := MakeAttr(White, Blue);
    end; { if }

  {-- Switch to ProBoard mode :-) ------------------------------------------}
  if (Pos('/QB', ParamString) > 0) then
    begin
      mnuStyle := 3;
      mnuBackGrCol := MakeAttr(6, Black);
      mnuBackGrChar := #177;
      mnuTopHiColor  := MakeAttr(White, Red);
      mnuPullHiColor := MakeAttr(White, Red);
      mnuBoxColor := MakeAttr(Red, Black);
      mnuPullColor := MakeAttr(White, Black);
      mnuNormColor := MakeAttr(White, Black);
    end; { if }

{$IFDEF WITH_DEBUG}
  if DebugObj.DebugLogging then
   if GenFile.FileExist('\DEBUG.TXT') then
    begin
      writeln('"\DEBUG.TXT" exists. Erase [Y,N]?');
      repeat
          ch := upcase(crt.readkey);
      until ch in ['Y', 'N'];

      if ch='Y' then
        begin
          {$IFNDEF ELEUNIX}
            assign(f,'\debug.txt');
            erase(f);
    {$ENDIF}
        end; { if }
    end; { if }
{$ENDIF}

 {$IFDEF OS2}
  if (Pos('/NORAW', SupCase(ParamString)) = 00) AND (Pos('-NORAW', SUpCase(ParamString)) = 00) then
    begin
      SetKeyboard_RawMode;
    end; { if }
 {$ENDIF}

 {$IFDEF WIN32}
  {$IFDEF VirtualPascal}
    if GetConsoleMode(SysFileStdIn, Mode) then      { Turn the mouse-cursor off }
      SetConsoleMode(SysFileStdIn, Mode AND NOT enable_processed_input
                                   and not enable_mouse_input);
  {$ENDIF}
 {$ENDIF}

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, '(GenCfg): Finished init'+FStr(GetCodePage));
  {$ENDIF}
end; { proc. GenCfgInit }

{$IFNDEF OVERLAY}
begin
  GenCfgInit;
{$ENDIF}
end.
