unit AreaDef;
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
** AREADEF.TPU, AreaDefaults (Message and File groups and areas)
**
** Copyright (c) 1996, 1997 by Maarten Bekers
**
** Created : 23-Nov-1997
** Last update : 23-Nov-1997
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
  This unit should not be compiled in EleBBS
{$ENDIF}

uses Global, Ral, Debug_U, BitWise, CfgRec, StrPath, StUtils, GenCfg,
     LongStr, ObjDec;


function CreateNewProtocolRA: boolean;
function CreateNewRalFile(Name: String; var DefLang: DefaultLangArrayRec; DoDec: Boolean): boolean;
function CreateNewAkasBBS: Boolean;
function CreateNewLanguageRA: boolean;
function CreateNewFileGroupRa: Boolean;
function CreateNewMsgGroupRa: Boolean;
function CreateNewFilesRa: boolean;
function CreateNewMessageRa: Boolean;
function CreateNewLimitsRa: Boolean;
function QuickScanNewMsgArea(AddPath: String; Message, Group: Boolean): Longint;
procedure AskAreaSuggestion(var buf; Message, Group: Boolean; Addpath: String);
procedure LoadMenuDefaults(var MenuInf: mnuRecord);
procedure LoadLightBarDefaults(var LBarInfo: LightBarRecord);
Procedure SetLanguageDefaults(var LangInf: LanguageRecord);
Procedure SetProtocolDefaults(var ProtInf: ProtocolRecord);
Procedure SetMessageDefaults(var MsgInf: MessageRecord);
Procedure SetFilesDefaults(var FilesInf: FilesRecord);
Procedure SetMessageEleDefaults(var EleMsgInf: EleMessageRecord; var MsgInf: MessageRecord);
Procedure SetFilesEleDefaults(var EleFileInf: EleFilesRecord; var FilesInf: FilesRecord);
Procedure SetGroupDefaults(var GroupInf: GroupRecord; Message: Boolean);

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure LoadLightBarDefaults(var LBarInfo: LightBarRecord);
begin
  FillChar(LBarInfo, SizeOf(LightBarRecord), #00);

  LBarInfo.LightX := 01;
  LBarInfo.LightY := 01;
  ClearBit(LBarInfo.Attrib, 00);
end; { proc. LoadLightBarDefaults }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure LoadMenuDefaults(var MenuInf: mnuRecord);
Const Day_End_Time : Word = 1439;

var XCounter,
    YCounter    : Byte;
begin
  FillChar(MenuInf, SizeOf(mnuRecord), #00);

  For XCounter := 1 to 32 do
    For YCounter := 00 to 07 do
      SetBit(MenuInf.Node[XCounter], YCounter);

  For XCounter := 1 to 32 do
    For YCounter := 00 to 07 do
      SetBit(MenuInf.Group[XCounter], YCounter);

  For XCounter := 01 to 07 do
    MenuInf.StopTime[XCounter] := Day_End_time;

  MenuInf.HotKey[1] := #32;
  MenuInf.ForeGround := 3;
end; { proc. LoadMenuDefaults }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure SetLanguageDefaults(var LangInf: LanguageRecord);
begin
{$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, 'Set language default');
{$ENDIF}
  Fillchar(LangInf, SizeOf(LangInf), #00);

  LangInf.Attribute := 01;                                          { Enabled }
  LangInf.Attribute := 00;                                         { Disabled }
  LangInf.DefName := '.ral';
end; { proc. SetLanguageDefaults }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


Procedure SetGroupDefaults(var GroupInf: GroupRecord; Message: Boolean);
begin
{$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, 'Setgroupdefaults');
{$ENDIF}
  FillChar(GroupInf, SizeOf(GroupRecord), #00);

  if Message then
   AskAreaSuggestion(GroupInf, Message, True, JustPath(MGroupsFileName))
    else AskAreaSuggestion(GroupInf, Message, True, JustPath(FGroupsFileName));
end; { SetGroupDefaults }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CreateNewLanguageRA: Boolean;
var Lang_F : File;
    LangInf: LanguageRecord;
    Counter: Byte;
begin
{$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFileIO, 'CreateNewLanguageRa');
{$ENDIF}
  CreateNewLanguageRa := false;

  Assign(Lang_F, JustName(LanguagefileName));
  FileMode := ReadWriteMode + DenyNone;
  {$i-} Rewrite(Lang_F, 1); {$I+}
  If IOResult>00 then EXIT;

  SetLanguageDefaults(LangInf);

  For Counter := 01 to 100 do
    begin;
      {$i-}
        BlockWrite(Lang_F, LangInf, SizeOf(LanguageRecord));
        if IOResult>00 then
          begin
            close(lang_f);
            exit;
          end; { if }
      {$i+}
    end; { for }

  Close(Lang_F);

  CreateNewLanguageRa := True;
end; { func. CreateNewLanguageRA }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


function CreateNewProtocolRA: Boolean;
var Prot_F      : File;
    ProtocolInf : ProtocolRecord;
    Counter     : Byte;
begin
{$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFileIo, 'CreateNewProtocolRA');
{$ENDIF}
  CreateNewProtocolRA := False;

  Assign(Prot_F, JustName(ProtocolFileName));
  FileMode := ReadWriteMode + DenyNone;
  {$i-} Rewrite(Prot_F, 1); {$I+}
  If IOResult>00 then EXIT;

  SetProtocolDefaults(ProtocolInf);

  {$i+}
  For Counter := 01 to 15 do
    BlockWrite(Prot_F, ProtocolInf, SizeOf(ProtocolRecord));
  {$i+}
  CreateNewProtocolRA := (IOResult=00);

  Close(Prot_F);
end; { func. CreateNewProtocolRA }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CreateNewLimitsRa: Boolean;
var Limits_F : File;
    LimitsInf: LimitsRecord;
begin
{$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFileIO, 'CreateNewLimitsRA');
{$ENDIF}
  CreateNewLimitsRA := false;

  Assign(Limits_F, JustName(LimitsFileName));
  FileMode := ReadWriteMode + DenyNone;
  {$i-} Rewrite(Limits_F, 1); {$I+}
  If IOResult>00 then EXIT;

  FillChar(LimitsInf, SizeOf(LimitsRecord), #00);

  {$I-}
    BlockWrite(Limits_F, LimitsInf, SizeOf(LimitsRecord));

    Close(Limits_F);
  {$I+}
  If IOResult=00 then CreateNewLimitsRA := True;
end; { func. CreateNewLimitsRa }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CreateNewMessageRa: Boolean;
var Msg_F : File;
    MsgInf: MessageRecord;
begin
{$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFileIO, 'CreateNewMessageRA');
{$ENDIF}
  CreateNewMessageRa := False;

  Assign(Msg_F, JustName(MessagesFileName));
  FileMode := ReadWriteMode + DenyNone;
  {$i-} Rewrite(Msg_F, 1); {$I+}
  If IOResult>00 then EXIT;

  SetMessageDefaults(MsgInf);

  {$i-}
    BlockWrite(Msg_F, MsgInf, SizeOf(MessageRecord));
    Close(Msg_F);
  {$i+}
  CreateNewMessageRa := (IOresult=00);
end; { func. CreateNewMessageRa }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CreateNewFilesRa: Boolean;
var Files_F : File;
    FilesInf: FilesRecord;
begin
{$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFileIo, 'CreateNewFilesRa');
{$ENDIF}
  CreateNewFilesRA := False;

  Assign(Files_F, JustName(FilesFileName));
  FileMode := ReadWriteMode + DenyNone;
  {$i-} Rewrite(Files_F, 1); {$I+}
  If IOResult>00 then EXIT;

  SetFilesDefaults(FilesInf);

  {$i-}
    BlockWrite(Files_F, FilesInf, SizeOf(FilesRecord));
    Close(Files_F);
  {$i+}
  CreateNewFilesRa := (IOresult=00);
end; { func. CreateNewFilesRa }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CreateNewFileGroupRa: Boolean;
var Group_F : File;
    GroupInf: GroupRecord;
begin
{$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFileIO, 'CreateNewFileGroupRa');
{$ENDIF}
  CreateNewFileGroupRA := False;

  Assign(Group_F, FGroupsFileName);
  FileMode := ReadWriteMode + DenyNone;
  {$i-} Rewrite(Group_F, 1); {$I+}
  If IOResult>00 then EXIT;

  SetGroupDefaults(GroupInf, False);

  {$i-}
    BlockWrite(Group_F, GroupInf, SizeOf(GroupRecord));
    Close(Group_F);
  {$i+}
  CreateNewFileGroupRa := (IOresult=00);
end; { proc. CreateNewFileGroupRa }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CreateNewMsgGroupRa: Boolean;
var Group_F : File;
    GroupInf: GroupRecord;
begin
{$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFileIO, 'CreateNewMsgGroupRa');
{$ENDIF}
  CreateNewMsgGroupRa := False;

  Assign(Group_F, MGroupsFileName);
  FileMode := ReadWriteMode + DenyNone;
  {$i-} Rewrite(Group_F, 1); {$I+}
  If IOResult>00 then EXIT;

  SetGroupDefaults(GroupInf, True);

  {$i-}
    BlockWrite(Group_F, GroupInf, SizeOf(GroupRecord));
    Close(Group_F);
  {$i+}
  CreateNewMsgGroupRa := (IOresult=00);
end; { proc. CreateNewFileGroupRa }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AskAreaSuggestion(var buf; Message, Group: Boolean; Addpath: String);
var AreaNum: Word absolute buf;
    SugArea: Longint;
begin
  if AreaNum <> 00 then EXIT;
  SugArea := QuickScanNewMsgArea(AddPath, Message, Group);
  if SugArea = -1 then EXIT;

  AreaNum := Word(SugArea);
end; { proc. AskAreaSuggestion }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure SetFilesDefaults(var FilesInf: FilesRecord);
begin
{$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, 'Setfilesdefaults');
{$ENDIF}
  FillChar(FilesInf, SizeOf(FilesInf), #00);

  AskAreaSuggestion(FilesInf, False, False, JustPath(FilesFileName));
end; { proc. SetFilesDefaults }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure SetProtocolDefaults(var ProtInf: ProtocolRecord);
begin
{$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, 'Set protocol defaults');
{$ENDIF}
  Fillchar(ProtInf, SizeOf(ProtocolRecord), #00);
end; { proc. SetProtocolDefaults }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure SetMessageDefaults(var MsgInf: MessageRecord);
begin
{$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, 'Set message defaults');
{$ENDIF}
  FillChar(MsgInf, SizeOf(MsgInf), #00);
  AskAreaSuggestion(MsgInf, True, False, JustPath(MessagesFileName));
end; { proc. SetMessageDefaults }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CreateNewRalFile(Name: String; var DefLang: DefaultLangArrayRec; DoDec: Boolean): Boolean;

type WordRec = Record
                 LoB,
                 HiB: Byte;
               end;

function HiFunc(W: SmallWord): Byte;

var B: WordRec ABSOLUTE W;
begin
  HiFunc := B.HiB;
end; { func. HiFunc }

var Temp_F   : File;
    Temp     : SmallWord;
    Counter  : SmallWord;
    SavePos  : SmallWord;
    MakeStr  : String;
    DoKey    : Boolean;
    TempError: Longint;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileIO, 'Createnewralfile (Name='+Name+')');
  {$ENDIF}
  CreateNewRalFile := False;
  DoKey := True;
  if IoResult > 00 then ;

  Assign(Temp_F, Name);
  FileMode := ReadWriteMode + DenyNone;
  {$i-} Rewrite(Temp_F, 01); {$I+}
  TempError := IoResult;
  if TempError > 00 then
    begin
      {$IFDEF WITH_DEBUG}
        DebugObj.DebugLog(logFileIO, 'CreateNewRalFile - TempError = '+FStr(TempError));
      {$ENDIF}

      EXIT;
    end; { if }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileIO, 'Createnewralfile - (Name='+Name+'), file successfully opened');
  {$ENDIF}

  {$i-}
   Temp := defRalSize;

   BlockWrite(Temp_F, Temp, SizeOf(SmallWord));

   Temp := 00;

   For Counter := 01 to defRalSize do
     BlockWrite(Temp_F, Temp, SizeOf(SmallWord));
  {$I+}

  If IOResult>00 then
    begin
      {$i-} Close(Temp_F); {$I+}
      If IOResult>00 then;

      {$IFDEF WITH_DEBUG}
         DebugObj.DebugLog(logFileIO, 'Createnewralfile - (Name='+Name+'), unable to write template');
      {$ENDIF}

      EXIT;
    end; { if }

  BlockWrite(Temp_F, DefLang[00].TextStr^[1], Length(DefLang[00].TextStr^));

  For Counter := 01 to defRalSize do
    begin
       {$i-}
         SavePos := FilePos(Temp_F);
         Seek(Temp_F, (Counter*2));

         Temp := SavePos;
         Inc(Temp);                                             { Skip colour }
         Inc(Temp, Pred(HiFunc(Temp)) + Pred(HiFunc( Temp + Pred(HiFunc(Temp))  )) -    Pred(HiFunc(Temp))   );

         BlockWrite(Temp_F, Temp, SizeOf(Temp));
         Seek(Temp_F, SavePos);
       {$I+}
       If IOResult>00 then break;

       {$i-}
         BlockWrite(Temp_F, DefLang[Counter].TxtColor, SizeOf(Byte));

         FillChar(MakeStr, SizeOf(MakeStr), #00);

        if DefLang[Counter].TextStr <> nil then
          MakeStr := DefLang[Counter].TextStr^
            else MakeStr := '';

         If DefLang[Counter].Keys <> '' then
           MakeStr := DefLang[Counter].Keys + #32 + MakeStr + #255;

         If (DefLang[Counter].HasDefault) then
          If (DefLang[Counter].Defaultkey<>#00) then
            begin
              if DoDec then
                Dec(MakeStr[0]);                            { Remove old key }
              MakeStr := MakeStr + DefLang[Counter].DefaultKey;
            end; { if }

         While (Length(MakeStr) > 00) AND
          (StUtils.LastChar(MakeStr)=#00) do Dec(MakeStr[0]);

         BlockWrite(Temp_F, MakeStr[1], Length(MakeStr));

(***
         Writeln('Writing makestr: "', MakeStr, '"');
         Writeln('StrPas         : "', DefLang[Counter].TextStr^,'"');
         Writeln('Pos(#00)       : "', Pos(#00, makeStr)>00,'"');
         Writeln('LastChar       : "', Ord(LastChar(MakeStr)));
         Writeln('----------------------------------------------------------');
         if (makestr='') OR (Pos(#00, MakeStr)>00) OR
          (counter=63) or (counter=64) or (counter=65) or (counter=62) then
         if dokey then
          if readkey=#27 then dokey:=false;
***)

       {$I+}
       If IOResult>00 then;
    end; { for }

  {$i-} Close(Temp_F); {$I+}
  If IOresult>00 then;

  CreateNewRalFile := True;
end; { proc. CreateNewralFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function QuickScanNewMsgArea(AddPath: String; Message, Group: Boolean): Longint;
var FName   : String;
    F       : File;
    W       : Array[0..399] of SmallWord;
    Reads   : NumReadType;
    Selected: SmallWord;
    Counter : Longint;
begin
  QuickScanNewMsgArea := -1;
  Selected := 0;
  AddPath := ForceBack(AddPath);

  if (Message) AND (Group) then FName := 'mgroups.rdx';
  if (Message) AND (NOT Group) then FName := 'messages.rdx';
  if (NOT Message) AND (Group) then FName := 'fgroups.rdx';
  if (NOT Message) AND (NOT Group) then FName := 'files.rdx';

  Assign(F, AddPath + FName);
  FileMode := ReadMode + DenyNone;
  {$i-} System.Reset(F, SizeOf(SmallWord)); {$i+}
  if IOResult>00 then EXIT;

  While (NOT Eof(F)) AND (Selected = 0) do
    begin
      {$i-} BlockRead(F, W, SizeOf(W) div SizeOf(SmallWord), Reads); {$i+}
      if IOResult>00 then BREAK;

      for Counter := 0 to (Reads - 1) do
        if W[Counter] = 0 then
          begin
            Selected := Succ((FilePos(F) - Reads) + Counter);
            BREAK;
          end; { if }

    end; { while }

  if (Eof(f)) AND (Selected = 0) then
    Selected := FilePos(F) + 1;
  QuickScanNewMsgArea := Selected;

  {$i-} Close(F); {$i+}
  if IOresult>00 then ;
end; { func. QuickScanNewMsgarea }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function CreateNewAkasBBS: Boolean;
var Temp_F : File;
    Addr   : NetAddress;
    Counter: Longint;
begin
  CreateNewAkasBBS := false;

  Assign(Temp_F, AddressFileName);
  FileMode := ReadWriteMode + DenyNone;
  {$i-} ReWrite(Temp_F, 1); {$i+}
  if IoResult > 0 then EXIT;

  FillChar(Addr, SizeOf(NetAddress), 0);

  for Counter := 00 to 250 do
    begin
      {$i-}
        BlockWrite(Temp_F, Addr, SizeOf(NetAddress));
      {$i+}
    end; { for }

  {$i-} Close(Temp_F); {$i+}
  CreateNewAkasBBS := (IoResult = 0);
end; { func. CreateNewAkasBBS }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SetMessageEleDefaults(var EleMsgInf: EleMessageRecord;
                                var MsgInf: MessageRecord);
begin
  FillChar(EleMsgInf, SizeOf(EleMessageRecord), #00);

  ELeMsgInf.AreaNum := MsgInf.AreaNum;
  EleMsgInf.GroupName := Space2Dot(MsgInf.Name);
  EleMsgInf.AccessSettings := nwsPostNever;
end; { proc. SetMessageEleDefaults }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SetFilesEleDefaults(var EleFileInf: EleFilesRecord; var FilesInf: FilesRecord);
begin
  FillChar(EleFileInf, SizeOf(EleFilesRecord), #00);

  EleFileInf.AreaNum := FilesInf.AreaNum;
  EleFileInf.ftpLoginName := 'anonymous';
  EleFileInf.ftpPassword := 'john.doe@elebbs.bbs';
  EleFileInf.ExportUrl := '';
end; { proc. SetFilesEleDefaults }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end.
