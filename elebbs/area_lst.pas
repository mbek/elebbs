unit Area_Lst;
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
** AREA_LST.TPU, AreLister (Message and File groups and areas)
**
** Copyright (c) 1996 by Maarten Bekers
**
** Created : 29-Oct-1996
** Last update : 12-Aug-1997
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
  This unit should not be compiled in EleBBS
{$ENDIF}

{$IFDEF ISCGI}
  This unit should not be compiled in EleWEB
{$ENDIF}

uses {$IFDEF DELCRT}
       Crt32,
     {$ELSE}
       Crt,
     {$ENDIF}
     Dos, CfgRec, RAL, Global, ListSys, Debug_U, ScrnU, StrEdit,
     GenCfg, StrPath, CfgDef, AreaDef, RecDif, GenFile,
     LongStr, Cases, FileRout, Ranges, Strings, SysUtils, BitWise,
     MenuSys, StUtils, CentrStr, RalDef, Colors, FlagStr, MemMan,
     FileObj, ObjDec, MkMsgAbs, MkOpen, Mail;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


Type GlobalChangeArray = Array[1..150] of Boolean;


Const RalHasChanged : Boolean = false;
      IsCombining   : Boolean = false;
      LangSearchStr : String  = '';

Type GlobalUpdateType=(Message, Files, Groups, EleMsg, EleFILE);

var MsgArea_F      : pFileObj;                            { For MessageArea lister }
    EleMSG_F       : pFileObj;
    EleFILE_F      : pFileObj;
    FileArea_F     : pFileObj;
    GenGroup_F     : pFileObj;
    Protocol_F     : pFileObj;
    Language_F     : pFileObj;
    Menu_F         : pFileObj;
    LBar_F         : pFileObj;

    MessageInf     : ^MessageRecord;
    EleMsgInf      : EleMessageRecord;
    MenuInf        : MnuRecord;
    LBarInfo       : LightBarRecord;
    FilesInf       : FilesRecord;
    EleFilesInf    : EleFilesRecord;
    GroupInf       : GroupRecord;
    LanguageInf    : LanguageRecord;
    ProtocolInf    : Protocolrecord;
    EdittingMsgArea: Boolean;
    EdittingGroupRecord: Boolean;
    EdittingFileArea: Boolean;
    CopyPoint      : LongInt;
    MainChangeArray: GlobalChangeArray;
    GroupFileName  : String[128];
    GroupTitle     : String[128];
    GroupMessage   : Boolean;

    CurLang        : DefaultLangArrayRec;
    CurLangPicked  : LanguageRecord;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WIN32}{$H-,F+}{$ENDIF}
Const EdittingAreanr: Boolean = false;

Procedure DoEditAreaNr(X, Y: Byte; DupFile: String; var AreaNum: SmallWord);


Procedure DoFileAreas;
Procedure DoMessageAreas;
Procedure DoGroupList;
Procedure MsgArea_GetInfo(var Info1, Info2, Info3, Info4, Info5, Info6, Info7, Info8, Info9: List_StringType;
                          ItemNr: LongInt; HiLight: LongInt);
Function MsgArea_Seek(Str: String): LongInt;

Procedure EditProtocol(var ProtInf: protocolRecord; ItemNr: String);

Procedure EditMsgArea(var MessageInf: MessageRecord;
                      var EleMsgInfo: EleMessageRecord;
                      GlobChange: Boolean;
                      Template: MessageRecord;
                      NewTemplate: EleMessageRecord;
                      GlobalProcessing: Boolean;
                      var ChangeArray: GlobalChangeArray;
                      DoingEleMSG: Boolean);
Procedure EditGroupRecord(var GroupInf: GroupRecord; GlobChange: Boolean;
                          Template: GroupRecord; GlobalProcessing: Boolean;
                          var ChangeArray: GlobalChangeArray);
Procedure EditFileArea(var FilesInf: FilesRecord;
                       var EleFilesInf: EleFilesRecord;
                       GlobChange: Boolean;
                       Template: FilesRecord;
                       NewTemplate: EleFilesRecord;
                       GlobalProcessing: Boolean;
                       var ChangeArray: GlobalChangeArray;
                       DoingEleFILE: Boolean);
Procedure DeleteRecord(StartRange, EndRange: LongInt;
                       RecordSize: Word; var FileVar: pFileObj;
                       var Buf);
Procedure InsertRecord(StartRange, EndRange: LongInt;
                       RecordSize: Word; var FileVar: pFileObj;
                       var Buf; var DefBuf);
Function DoGlobalChange(Choice: Word): Boolean;
Function Copy_Activate(HiLight: LongInt; HiBarPos: LongInt): Longint;
Function Copy_AbortCheck(CH: Char): Boolean;
Function Copy_KeyPress(CH: Char; HiLight: LongInt; var HiBarPos, TopOfScrn: Longint;
                       var StartRange, EndRange: LongInt): LongInt;
Function General_AbortCheck(CH: Char): Boolean;
Function MsgArea_Activate(HiLight: LongInt; HiBarPos: LongInt): Longint;
Procedure CopyRecord(StartRange, EndRange: LongInt; Move: Boolean;
                     RecordSize: Word; var FileVar: pFileObj;
                     var Buf; NrSkip: Word);
Procedure UpdateGlobalInfo(StartRange, EndRange: LongInt; var GlobInf;
                           var ReadInf; var FileVar: pFileObj;
                           UpdateType: GlobalUpdateType;
                           RecordSize: Word);

Procedure DoCombined;
Procedure Protocol_GetInfo(var Info1, Info2, Info3, Info4, Info5, Info6, Info7, Info8, Info9: List_StringType;
                           ItemNr: LongInt; HiLight: LongInt);
Function Protocol_Activate(HiLight: LongInt; HibarPos: LongInt): Longint;
Function Protocol_Seek(Str: String): LongInt;
Function Protocol_GetItems: LongInt;
procedure MsgBox(S: String; Open: Boolean; var SAveScrn: Pointer);

Function Language_GetItems: LongInt;
Procedure Language_NoEditGetInfo(var Info1, Info2, Info3, Info4, Info5, Info6, Info7, Info8, Info9: List_StringType;
                           ItemNr: LongInt; HiLight: LongInt);
Procedure Language_GetInfo(var Info1, Info2, Info3, Info4, Info5, Info6, Info7, Info8, Info9: List_StringType;
                           ItemNr: LongInt; HiLight: LongInt);
Function Language_Seek(Str: String): LongInt;
Function Language_Activate(HiLight: LongInt; HiBarPos: LongInt): Longint;
Function Language_MenuActivate(HiLight: LongInt; HiBarPos: LongInt): Longint;
Procedure EditLanguage(var LangInf: LanguageRecord; HiLight: LongInt; DoEdit: Boolean; var DoSave: Boolean);

procedure CantCreate(S: String; Open: Boolean);

Procedure EditLanguageText(var LangInf: LanguageRecord);
Function LanguageText_Seek(Str: String): LongInt;
Function LanguageText_GetItems: LongInt;
Procedure LanguageText_GetInfo(var Info1, Info2, Info3, Info4, Info5, Info6, Info7, Info8, Info9: List_StringType;
                               ItemNr: LongInt; HiLight: LongInt);
Function LanguageText_Activate(HiLight: LongInt; HiBarPos: LongInt): Longint;
function LanguageText_CreateNewFile: Boolean;

Function FileArea_Activate(HiLight: LongInt;  HiBarPos: LongInt): Longint;
Function MsgArea_GetItems: LongInt;
Procedure FileArea_GetInfo(var Info1, Info2, Info3, Info4, Info5, Info6, Info7, Info8, Info9: List_StringType;
                           ItemNr: LongInt; HiLight: LongInt);
function MsgArea_KeyPress(CH: Char; HiLight: LongInt;
                          var HiBarPos, TopOfScrn: Longint;
                          var StartRange, EndRange: LongInt): LongInt;
Function FileArea_Seek(Str: String): LongInt;
Function FileArea_GetItems: LongInt;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 uses akaLst;
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ToggleCombined(AreaNr: Word; var Combined: CombinedRecord; IsOn: Boolean);
var Counter: Byte;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFastScrn, 'ToggleCombined');
  {$ENDIF}

  If IsOn then
    For Counter := 01 to 200 do
       If Combined[Counter]=00 then
         begin
           Combined[Counter] := AreaNr;
           break;
         end; { if }

  If NOT IsOn then
    For Counter := 01 to 200 do
       If Combined[Counter]=AreaNr then
         begin
           Combined[Counter] := 00;
           break;
         end; { if }
end; { proc. ToggleCombined }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function StringNew(Lang: String): LangString;
var Temp: LangString;
begin
  Temp := nil;

  if Length(Lang)>0 then
    begin
      GetMem(Temp, Length(Lang)+1);
      If Temp <> nil then
         Temp^ := lang;
    end; { if }

  StringNew := Temp;
end; { proc. StringNew }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure StringDispose(var Lang: LangString);
begin
  If Lang <> nil then Freemem(Lang, Length(Lang^)+1);
  Lang := nil;
end; { proc. StringDispose }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function MsgArea_GetItems: LongInt;
var Add: Byte;
begin
  if NOT IsCombining then Add := 01 else Add:=00;

  MsgArea_GetItems := (MsgArea_F^.FileSize DIV SizeOf(MessageRecord)) + Add;
end; { func. MsgArea_GetItems }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Copy_Activate(HiLight: LongInt; HiBarPos: LongInt): Longint;
begin
  Copy_Activate := -1;

  CopyPoint := HiLight;
end; { proc. Copy_Activate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Copy_KeyPress(CH: Char; HiLight: LongInt; var HiBarPos, TopOfScrn: Longint;
                       var StartRange, EndRange: LongInt): LongInt;
begin
 Copy_KeyPress := -1;
end; { func. Copy_KeyPress }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CantCreate(S: String; Open: Boolean);
var XLen    : Byte;
    XStart  : Byte;
    SaveScrn: Pointer;
begin
  SaveScreen(SaveScrn);

  if Open then S := 'Unable to open '+S else
   S := 'Unable to create '+S;

  if Length(S) > 75 then S[0] := #76;
  XLen := 02 + Length(S) + 02;
  XStart := (mnuScrnWidth div 2) - (XLen div 2);

  ShadFillBoxTitle(XStart, 10, (XStart + Xlen) - 01, 14, mnuBoxColor, mnuStyle, True, ' Fatal error ');
  WriteAT(XStart + 02, 12, Lightgray, S);
  UpdateScreenBuffer(true);

  GetInput;

  RestoreScreen(SaveScrn);
end; { proc. CantCreate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure MsgBox(S: String; Open: Boolean; var SAveScrn: Pointer);
var XLen    : Byte;
    XStart  : Byte;
begin
  if Open then
    begin
      SaveScreen(SaveScrn);

      if Length(S) > 75 then S[0] := #76;
      XLen := 02 + Length(S) + 02;
      XStart := (mnuScrnWidth div 2) - (XLen div 2);

      ShadFillBoxTitle(XStart, 10, (XStart + Xlen) - 01, 14, mnuBoxColor, mnuStyle, True, ' Message ');
      WriteAT(XStart + 02, 12, Lightgray, S);
    end; { if }

  if NOT Open then RestoreScreen(SaveScrn);
end; { proc. CantCreate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function AskConfirmDelete: Char;
var CH: Char;
begin
  ShadFillBox(25, 10, 55, 14, mnuErrorColor, mnuStyle, True);
  WriteAT(28, 12, mnuErrorColor, 'Confirm delete (Y/n)? °');

  CursorOn;

  REPEAT
    GotoXY(50, 12);
    CH := Upcase(ReadKey);

    if CH=#13 then CH := 'Y';
  UNTIL (CH in ['Y', 'N']);

  CursorOff;
  AskConfirmDelete := CH;
end; { func. AskConfirmDelete }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function MsgArea_KeyPress(CH: Char; HiLight: LongInt;
                          var HiBarPos, TopOfScrn: Longint;
                          var StartRange, EndRange: LongInt): LongInt;
var Temp: LongInt;
    TempWord : SmallWord;
    GlobInf: MessageRecord;
    Glob2Inf: EleMessageRecord;
    SaveScrn: Pointer;
begin
  MsgArea_KeyPress := -1;
  if HiLight = MsgArea_GetItems then
   if CH <> #82 then EXIT; { if not inserting, exit }

  Case CH of
    { Delete }  #83 : begin
                        if SemaExist(EleNewsSemaphore) then
                          begin
                            MsgBox('EleNEWS is running - cannot insert or delete areas', true, SaveScrn);
                            GetInput;
                            MsgBox('', false, SaveScrn);
                            EXIT;
                          end; { if }

                        CH := AskConfirmDelete;
                        If Ch='Y' then
                         begin;
                           If StartRange=EndRange then
                            begin;
                              StartRange := HiLight;
                              EndRange := HiLight;
                            end; { StartRange=EndRange }

                           DeleteRecord(StartRange, EndRange,
                                        SizeOf(MessageRecord), MsgArea_F,
                                        MessageInf^);

                           DeleteRecord(StartRange, EndRange,
                                        SizeOf(EleMessageRecord), EleMSG_F,
                                        EleMsgInf);

                           StartRange := -1;
                           EndRange := -1;
                           GenerateRdxFiles(JustPath(MessagesFileName), True, False, true);
                         end; { if True }
                      end; { Delete }
    { Insert }  #82 : begin
                        if SemaExist(EleNewsSemaphore) then
                          begin
                            MsgBox('EleNEWS is running - cannot insert or delete areas', true, SaveScrn);
                            GetInput;
                            MsgBox('', false, SaveScrn);
                            EXIT;
                          end; { if }

                        if StartRange=EndRange then
                          begin
                            StartRange := HiLight;
                            EndRange := HiLight;
                          end; { StartRange=EndRange }

                          SetMessageDefaults(GlobInf);
                          InsertRecord(StartRange, EndRange,
                                       SizeOf(MessageRecord), MsgArea_F,
                                       MessageInf^, GlobInf);

                          SetMessageEleDefaults(Glob2Inf, GlobInf);
                          InsertRecord(StartRange, EndRange,
                                       SizeOf(EleMessageRecord), EleMSG_F,
                                       EleMsgInf, Glob2Inf);

                          StartRange := -1;
                          EndRange := -1;
                          GenerateRdxFiles(JustPath(MessagesFileName), True, False, true);
                          MsgArea_keypress := HiLight + 1;
                      end; { Insert }
    { Move }    #50,
    { Copy }    #46 : begin
                        if SemaExist(EleNewsSemaphore) then
                          begin
                            MsgBox('EleNEWS is running - cannot move or copy areas', true, SaveScrn);
                            GetInput;
                            MsgBox('', false, SaveScrn);
                            EXIT;
                          end; { if }

                        DoList(true,
                                13, 40, 7, 7, 0, 00, 00, 00, 00, 00,           { ShowLen }
                               12, 04, 68, mnuScrnLen - 4,          { Window Coordinates }
                               ' Message areas ',
                               'Select move/copy insertion point',
                               {$IFDEF FPC}@{$ENDIF}MsgArea_GetInfo,
                               {$IFDEF FPC}@{$ENDIF}Copy_Activate,
                               {$IFDEF FPC}@{$ENDIF}MsgArea_Seek,
                               {$IFDEF FPC}@{$ENDIF}Copy_AbortCheck,
                               {$IFDEF FPC}@{$ENDIF}Copy_Keypress,
                               {$IFDEF FPC}@{$ENDIF}MsgArea_GetItems,
                               {$IFDEF FPC}@{$ENDIF}CreateNewMessageRa,
                               {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
                               false,
                               HiBarPos,
                               TopOfScrn,
                               True, True);


                         if Copypoint <> -1 then
                          if CH = #46 then  { only show on real copy }
                           begin
                             MsgBox('WARNING!: The copied area/group will have the same number as original', true, SaveScrn);
                             GetInput;
                             MsgBox('WARNING!: The copied area/group will have the same number as original', false, SaveScrn);
                           end; { if }

                         If StartRange=EndRange then
                          begin;
                            StartRange := HiLight;
                            EndRange := HiLight;
                          end; { StartRange=EndRange }

                          CopyRecord(StartRange, EndRange, (CH = #50),
                                     SizeOf(MessageRecord), MsgArea_F,
                                     GlobInf, 0);

                          CopyRecord(StartRange, EndRange, (CH = #50),
                                     SizeOf(EleMessageRecord), EleMsg_F,
                                     Glob2Inf, 0);

                          StartRange := -1;
                          EndRange := -1;
                          GenerateRdxFiles(JustPath(MessagesFileName), True, False, true);
                      end; { copy }
    { Global }  #34 : begin;
                        SetMessageDefaults(GlobInf);        { Clear the record }
                        SetMessageEleDefaults(Glob2Inf, GlobInf);
                        GlobInf.AreaNum := 0;
                        EditMsgArea(GlobInf, Glob2Inf, True, GlobInf, Glob2Inf, False, MainChangeArray, false);

                        If StartRange=EndRange then
                          begin;
                            StartRange := HiLight;
                            EndRange := HiLight;
                          end; { StartRange=EndRange }

                        ShadFillBox(25, 10, 55, 14, mnuErrorColor, mnuStyle, True);
                        WriteAT(28, 12, mnuErrorColor, 'Apply changes (y/n)? °');

                        CursorOn;
                        Repeat;
                          GotoXY(49, 12);
                          Ch := Upcase(ReadKey);
                        Until CH in ['Y', 'N'];
                        CursorOff;

                        If CH='Y' then
                         begin
                            UpdateGlobalInfo(StartRange, EndRange, GlobInf,
                                             MessageInf^, MsgArea_F,
                                             Message, SizeOf(MessageRecord));
                            UpdateGlobalInfo(StartRange, EndRange, Glob2Inf,
                                             EleMsgInf, EleMSG_F,
                                             EleMSG, SizeOf(EleMessageRecord));

                            StartRange := -1;
                            EndRange := -1;
                            GenerateRdxFiles(JustPath(MessagesFileName), True, False, true);
                         end; { YES, Apply those changes }
                      end; { global Changes }
   { Alt - P }   #25 : begin
                        ShadFillBoxTitle(30, (mnuScrnLen DIV 2) - 2, 50, (mnuScrnLen DIV 2) + 2, mnuBoxColor, mnuStyle, True,
                                         ' Jump to ');
                        WriteAT(32, (mnuScrnLen DIV 2), mnuNormColor, 'Jump to #');

                        GetMessageRecord(GlobInf, HiLight, false);
                        TempWord := GlobInf.AreaNum;

                        EditWord(TempWord, 42, (mnuScrnLen DIV 2), mnuEditColor, true);

                        if RdLastKey <> #27 then
                          begin
                            MsgArea_KeyPress := Ra250MsgArea(TempWord);
                          end; { if }

                      end; { Jump to area }
  End; { Case }
end; { func. MsgArea_KeyPress }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function MsgArea_Activate(HiLight: LongInt; HiBarPos: LongInt): Longint;
var OldMsgInf: MessageRecord;
    OldEleMsg: EleMessageRecord;
begin
  MsgArea_Activate := -1;
  if HiLight=MsgArea_GetItems then EXIT;

  MsgArea_F^.Seek( (HiLight - 1) * SizeOf(MessageRecord));
  MsgArea_F^.BlkRead(MessageInf^, SizeOf(MessageRecord));

  EleMSG_F^.Seek( (HiLight - 1) * SizeOf(EleMessageRecord));
  EleMSG_F^.BlkRead(EleMsgInf, SizeOf(EleMessageRecord));

  OldMsgInf:= MessageInf^;
  OldEleMsg := EleMsgInf;

  EditMsgArea(MessageInf^, EleMsgInf, False, MessageInf^, EleMsgInf, False, MainChangeArray, false);
  EleMsgInf.AreaNum := MessageInf^.AreaNum;

  If (RecordsDifferent(MessageInf^, OldMsgInf, SizeOf(MessageRecord))) OR
       (RecordsDifferent(EleMsgInf, OldEleMsg, SizeOf(EleMessageRecord))) then
       If DoSaveChanges('Save changes (Y/n) ? °', True, false) then
              begin
                MsgArea_F^.Seek( (HiLight - 01) * SizeOf(MessageRecord));
                MsgArea_F^.BlkWrite(MessageInf^, SizeOf(MessageRecord));

                EleMSG_F^.Seek((HiLight-1) * SizeOf(EleMessageRecord));
                EleMSG_F^.BlkWrite(EleMsgInf, SizeOf(EleMessageRecord));
              end { SaveChanges }
                else begin
                       MessageInf^ := OldMsgInf;
                       EleMsgInf := OldEleMsg;
                     end; { else }
end; { proc. MsgArea_Activate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CreateEleMessageFiles;
var EleMsg_F : pFileObj;
    EleMsg   : EleMessageRecord;
    SaveScrn : Pointer;
begin
  if FileExist(MessageEleFileName) then
   begin
     if GetFileSize(MessageEleFileName, SizeOf(EleMessageRecord)) <
         (MsgArea_F^.FileSize div SizeOf(MessageRecord)) then
          begin
            if NOT DoSaveChanges('Newsgroup file is out of date. Regenerate? (Y/n)? °', true, false) then
              begin
                EXIT;
              end; { if }
          end { if }
            else EXIT;
    end; { if }

  New(EleMSG_F, Init);
  EleMSG_F^.Assign(MessageEleFileName);
  EleMSG_F^.FileMode := ReadWriteMode + DenyWrite;
  if NOT EleMSG_F^.Create(1) then
    begin
      MsgBox('Unable to create '+MessageEleFileName, true, SaveScrn);
      Getinput;
      MsgBox('Unable to create '+MessageEleFileName, false, SaveScrn);

      Dispose(EleMSG_F, Done);
      EXIT;
    end; { if }

  MsgBox('Creating newsgroup server information', true, SaveScrn);
  MsgArea_F^.Seek(0);

  While NOT MsgArea_F^.EOF do
    begin
      MsgArea_F^.BlkRead(MessageInf^, SizeOf(MessageRecord));
      if MsgArea_F^.IoResult > 00 then BREAK;

      SetMessageEleDefaults(EleMSG, MessageInf^);

      EleMSG_F^.BlkWrite(EleMSG, SizeOf(EleMessageRecord));
    end; { while }

  MsgBox('Creating newsgroup server information', false, SaveScrn);

  Dispose(EleMSG_F, Done);
end; { proc. CreateEleMessagefiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure DoMessageAreas;
begin
  If NOT FileExist(MessagesFileName) then
    if NOT CreateNewMessageRa then
      begin
        CantCreate(JustName(MessagesFileName), False);
        EXIT;
      end; { if }

  if NOT OpenFile(MessagesFileName, ReadWriteMode + DenyWrite) then
      begin
        CantCreate(JustName(MessagesFileName), True);
        EXIT;
      end; { if }

  IsCombining := false;

  New(MsgArea_F, Init);
  MsgArea_F^.Assign(MessagesFileName);
  MsgArea_F^.FileMode := ReadWriteMode + DenyWrite;
  MsgArea_F^.Open(1);

  AllocMem(MessageInf, SizeOf(MessageRecord), 'MessageRecord', 'DoMessageAreas');

  CreateEleMessageFiles;

  if NOT OpenFile(MessageEleFileName, ReadWriteMode + DenyWrite) then
      begin
        CantCreate(JustName(MessageEleFileName), True);
        EXIT;
      end; { if }

  New(EleMSG_F, Init);
  EleMSG_F^.Assign(MessageEleFileName);
  EleMSG_F^.FileMode := ReadWriteMode + DenyWrite;
  EleMSG_F^.Open(1);

  if (NOT UpToDateFile(MessagesFileName, RdxName(MessagesFileName)))
      then GenerateRdxFiles(JustPath(MessagesFileName), True, False, true);

  DoList(True, 13, 40, 7, 7, 0, 00, 00, 00, 00, 00,                                   { ShowLen }
         12, 04, 68, mnuScrnLen - 4,            { Window Coordinates }
         ' Message areas ',
         'Enter-Edit Space-Tag (INS)ert (DEL)ete  ALT: C-Copy  M-Move  G-Global  P-Goto',
         {$IFDEF FPC}@{$ENDIF}MsgArea_GetInfo,
         {$IFDEF FPC}@{$ENDIF}MsgArea_Activate,
         {$IFDEF FPC}@{$ENDIF}MsgArea_Seek,
         {$IFDEF FPC}@{$ENDIF}General_AbortCheck,
         {$IFDEF FPC}@{$ENDIF}MsgArea_KeyPress,
         {$IFDEF FPC}@{$ENDIF}MsgArea_GetItems,
         {$IFDEF FPC}@{$ENDIF}CreateNewMessageRa,
         {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
         True,
         00, 00,
         true, false);

  Dispose(MsgArea_F, Done);
  Dispose(EleMSG_F, Done);

  if (NOT FileExist(RdxName(MessagesFileName)))
    OR (NOT UpToDateFile(MessagesFileName, RdxName(MessagesFileName)))
      then GenerateRdxFiles(JustPath(MessagesFileName), True, False, true);

  ReleaseMem(MessageInf, SizeOf(MessageRecord));
end; { proc. DoMessageAreas }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Group_GetItems: LongInt;
begin
  Group_GetItems := (GenGroup_F^.FileSize DIV SizeOf(GroupRecord)) + 01;
end; { func. Group_GetItems }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


Procedure Group_GetInfo(var Info1, Info2, Info3, Info4, Info5, Info6, Info7, Info8, Info9: List_StringType;
                        ItemNr: LongInt; Hilight: LongInt);
var GrpInf: GroupRecord;
begin

  if ItemNr <> Group_GetItems then
    begin
      GenGroup_F^.Seek(Pred(ItemNr) * SizeOf(GroupInf));
      GenGroup_F^.BlkRead(GrpInf, SizeOf(GroupInf));

      Info1 := GrpInf.Name;
      Info2 := FStr(GrpInf.AreaNum);
      If Info1='' then Info1 := '[Unused]';
    end else begin
               Info1 := '';
               Info2 := '';
             end; { if }

  Info3 := '';
  Info4 := '';
  Info5 := '';
  Info6 := '';
  Info7 := '';
  Info8 := '';
  Info9 := '';
end; { proc. Group_GetInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Group_Seek(Str: String): LongInt;
var GroupInf: GroupRecord;
begin
  GenGroup_F^.Seek(0);

  Group_Seek := -1;

  While NOT GenGroup_F^.EOF do
   begin
     GenGroup_F^.BlkRead(GroupInf, SizeOf(GroupRecord));
     if GroupInf.Name = '' then GroupInf.Name := '[Unused]';

     If SUpcase(Str) = SupCase(Copy(GroupInf.Name, 1, Length(Str))) then
       begin
         Group_Seek := GenGroup_F^.FilePOS div SizeOf(GroupRecord);
         BREAK;
       end; { Found one }

   end; { While NOT eof }

end; { func. Group_Seek }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Group_Activate(HiLight: LongInt; HiBarPos: LongInt): Longint;
var OldGrpInf: GroupRecord;
    GroupIDX: String;
begin;
  Group_Activate := -1;
  if HiLight = Group_GetItems then EXIT;

  GenGroup_F^.Seek( (HiLight - 1) * SizeOf(GroupRecord));
  GenGroup_F^.BlkRead(GroupInf, SizeOf(GroupRecord));

  OldgrpInf:= GroupInf;

  EditGroupRecord(GroupInf, False, GroupInf, False, MainChangeArray);

  If RecordsDifferent(GroupInf, OldGrpInf, SizeOf(GroupRecord)) then
        If DoSaveChanges('Save changes (Y/n) ? °', TRue, false) then
              begin
                GenGroup_F^.Seek( (HiLight - 1) * SizeOf(GroupRecord));
                GenGroup_F^.BlkWrite(GroupInf, SizeOf(GroupRecord));
              end { SaveChanges }
               else GroupInf := OldGrpInf;

  GroupIDX := RdxName(GroupFileName);
end; { proc. Group_Activate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Group_KeyPress(CH: Char; HiLight: LongInt;
                        var HiBarPos, TopOfScrn: Longint;
                        var StartRange, EndRange: LongInt): LongInt;
var Temp: LongInt;
    GlobInf: GroupRecord;
    SaveScrn: Pointer;
    TempWord : SmallWord;
begin
  Group_Keypress := -1;
  if HiLight=Group_GetItems then
   if CH <> #82 then EXIT;                           { If not inserting, exit }

  Case CH of
    { Delete }  #83 : begin;
                        CH := AskConfirmDelete;
                        If Ch='Y' then
                         begin;
                           If StartRange=EndRange then
                            begin;
                              StartRange := HiLight;
                              EndRange := HiLight;
                            end; { StartRange=EndRange }

                           DeleteRecord(StartRange, EndRange,
                                        SizeOf(GroupRecord), GenGroup_F,
                                        GroupInf);

                           StartRange := -1;
                           EndRange := -1;
                           GenerateRdxFiles(JustPath(GroupFileName), GroupMessage, True, true);
                         end; { if True }
                      end; { Delete }
    { Insert }  #82 : begin;
                        If StartRange=EndRange then
                          begin;
                            StartRange := HiLight;
                            EndRange := HiLight;
                          end; { StartRange=EndRange }


                         SetGroupDefaults(GlobInf, GroupMessage);
                         InsertRecord(StartRange, EndRange,
                                      SizeOf(GroupRecord), GenGroup_F,
                                      GroupInf, GlobInf);

                          StartRange := -1;
                          EndRange := -1;
                          GenerateRdxFiles(Justpath(GroupFileName), GroupMessage, True, true);
                          Group_keypress := HiLight + 1;
                      end; { Insert }
    { Move }    #50,
    { Copy }    #46 : begin
                        If GroupMessage then
                         DoList(True, 16, 41, 6, 0, 0,  00, 00, 00, 00, 00,                                         { ShowLen }
                                15, 04, 64, mnuScrnLen - 4,                                       { Window Coordinates }
                                GroupTitle,
                                'Select move/copy insertion point',
                                {$IFDEF FPC}@{$ENDIF}Group_GetInfo,
                                {$IFDEF FPC}@{$ENDIF}Copy_Activate,
                                {$IFDEF FPC}@{$ENDIF}Group_Seek,
                                {$IFDEF FPC}@{$ENDIF}Copy_AbortCheck,
                                {$IFDEF FPC}@{$ENDIF}Copy_KeyPress,
                                {$IFDEF FPC}@{$ENDIF}Group_GetItems,
                                {$IFDEF FPC}@{$ENDIF}CreateNewMsgGroupRa,
                                {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
                                False,
                                HiBarPos,
                                TopOfScrn,
                                True, True);

                        If NOT GroupMessage then
                         DoList(True, 16, 41, 6, 0, 0, 00, 00, 00, 00, 00,                                          { ShowLen }
                                15, 04, 64, mnuScrnLen - 4,                                { Window Coordinates }
                                GroupTitle,
                                'Select move/copy insertion point',
                                {$IFDEF FPC}@{$ENDIF}Group_GetInfo,
                                {$IFDEF FPC}@{$ENDIF}Copy_Activate,
                                {$IFDEF FPC}@{$ENDIF}Group_Seek,
                                {$IFDEF FPC}@{$ENDIF}Copy_AbortCheck,
                                {$IFDEF FPC}@{$ENDIF}Copy_KeyPress,
                                {$IFDEF FPC}@{$ENDIF}Group_GetItems,
                                {$IFDEF FPC}@{$ENDIF}CreateNewFileGroupRa,
                                {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
                                False,
                                HiBarPos,
                                TopOfScrn,
                                True, True);

                         if Copypoint <> -1 then
                          if CH = #46 then { only show on copy }
                           begin
                             MsgBox('WARNING!: The copied area/group will have the same number as original', true, SaveScrn);
                             GetInput;
                             MsgBox('WARNING!: The copied area/group will have the same number as original', false, SaveScrn);
                           end; { if }

                        If StartRange=EndRange then
                          begin;
                            StartRange := HiLight;
                            EndRange := HiLight;
                          end; { StartRange=EndRange }

                          CopyRecord(StartRange, EndRange, (CH=#50),
                                     SizeOf(GroupRecord), GenGroup_F,
                                     GlobInf, 0);

                          StartRange := -1;
                          EndRange := -1;
                          GenerateRdxFiles(Justpath(GroupFileName), GroupMessage, True, true);
                      end; { copy }
    { Global }  #34 : begin;
                        SetGroupDefaults(GlobInf, GroupMessage);         { Clear the record }
                        GlobInf.AreaNum := 00;
                        EditGroupRecord(GlobInf, True, GlobInf, False, MainChangeArray); { Let SysOp edit the defaults }

                        If StartRange=EndRange then
                          begin;
                            StartRange := HiLight;
                            EndRange := HiLight;
                          end; { StartRange=EndRange }

                        ShadFillBox(25, 10, 55, 14, mnuErrorColor, mnuStyle, True);
                        WriteAT(28, 12, mnuErrorColor, 'Apply changes (y/n)? °');

                        CursorOn;
                        Repeat;
                          GotoXY(49, 12);
                          Ch := Upcase(ReadKey);
                        Until CH in ['Y', 'N'];
                        CursorOff;

                        If CH='Y' then
                         begin;
                            UpdateGlobalInfo(StartRange, EndRange, GlobInf,
                                             GroupInf, GenGroup_F,
                                             Groups, SizeOf(GroupRecord));

                            StartRange := -1;
                            EndRange := -1;
                            GenerateRdxFiles(Justpath(GroupFileName), True, False, true);
                         end; { YES, Apply those changes }

                        StartRange := -1;
                        EndRange := -1;
                        GenerateRdxFiles(Justpath(GroupFileName), GroupMessage, True, true);
                      end; { global Changes }
   { Alt - P }   #25 : begin
                        ShadFillBoxTitle(30, (mnuScrnLen DIV 2) - 2, 50, (mnuScrnLen DIV 2) + 2, mnuBoxColor, mnuStyle, True,
                                         ' Jump to ');
                        WriteAT(32, (mnuScrnLen DIV 2), mnuNormColor, 'Jump to #');

                        GetGroupRecord(HiLight, false, GroupMessage, GlobInf);
                        TempWord := GlobInf.AreaNum;

                        EditWord(TempWord, 42, (mnuScrnLen DIV 2), mnuEditColor, true);

                        if RdLastKey <> #27 then
                          begin
                            Group_KeyPress := Ra250Group(TempWord, Groupmessage);
                          end; { if }

                      end; { Jump to area }
  End; { Case }
end; { func. Group_KeyPress }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure DoGroupList;
var TempBool: Boolean;
begin
  If NOT FileExist(GroupFileName) then
    begin;
      If GroupMessage then TempBool := CreateNewMsgGroupRa
        else TempBool := CreateNewFilegroupRa;

      if NOT TempBool then
        begin
          CantCreate('?GROUP.RA', False);
          EXIT;
       end; { if }
     end; { if }

  if NOT OpenFile(GroupFileName, ReadWriteMode + DenyWrite) then
      begin
        CantCreate(GroupFileName, True);
        EXIT;
      end; { if }

  New(GenGroup_F, Init);
  GenGroup_F^.Assign(GroupFileName);
  GenGroup_F^.FileMode := ReadWriteMode + DenyWrite;
  GenGroup_F^.Open(1);

  if (NOT UpToDateFile(GroupFileName, RdxName(GroupFileName)))
     then GenerateRdxFiles(Justpath(GroupFileName), GroupMessage, True, true);

  If GroupMessage then
   DoList(True, 16, 41, 6, 0, 0,  00, 00, 00, 00, 00,                                  { ShowLen }
          15, 04, 64, mnuScrnLen - 4,                                { Window Coordinates }
          GroupTitle,
          'Enter-Edit Space-Tag (INS)ert (DEL)ete  ALT: C-Copy  M-Move  G-Global  P-Goto',
          {$IFDEF FPC}@{$ENDIF}Group_GetInfo,
          {$IFDEF FPC}@{$ENDIF}Group_Activate,
          {$IFDEF FPC}@{$ENDIF}Group_Seek,
          {$IFDEF FPC}@{$ENDIF}General_AbortCheck,
          {$IFDEF FPC}@{$ENDIF}Group_KeyPress,
          {$IFDEF FPC}@{$ENDIF}Group_GetItems,
          {$IFDEF FPC}@{$ENDIF}CreateNewMsgGroupRa,
          {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
          True,
          00, 00,
          True, False);

  If NOT GroupMessage then
   DoList(True, 16, 41, 6, 0, 0, 00, 00, 00, 00, 00,                                          { ShowLen }
          15, 04, 64, mnuScrnLen - 4,                                { Window Coordinates }
          GroupTitle,
          'Enter-Edit Space-Tag (INS)ert (DEL)ete  ALT: C-Copy  M-Move  G-Global  P-Goto',
          {$IFDEF FPC}@{$ENDIF}Group_GetInfo,
          {$IFDEF FPC}@{$ENDIF}Group_Activate,
          {$IFDEF FPC}@{$ENDIF}Group_Seek,
          {$IFDEF FPC}@{$ENDIF}General_AbortCheck,
          {$IFDEF FPC}@{$ENDIF}Group_KeyPress,
          {$IFDEF FPC}@{$ENDIF}Group_GetItems,
          {$IFDEF FPC}@{$ENDIF}CreateNewFileGroupRa,
          {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
          True,
          00, 00,
          True, False);

  Dispose(GenGroup_F, Done);

  if (NOT FileExist(RdxName(GroupFileName)))
    OR (NOT UpToDateFile(GroupFileName, RdxName(GroupFileName)))
      then GenerateRdxFiles(Justpath(GroupFileName), GroupMessage, True, true);
end; { proc. DoGroupList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FileArea_GetInfo(var Info1, Info2, Info3, Info4, Info5, Info6, Info7, Info8, Info9: List_StringType;
                           ItemNr: LongInt; HiLight: LongInt);
begin
 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFileIO, 'FileArea_GetInfo: ItemNr='+FStr(ItemNr));
 {$ENDIF}

  if ItemNr <> FileArea_GetItems then
    begin
      FileArea_F^.Seek(Pred(ItemNr) * SizeOf(FilesRecord));
      FileArea_F^.BlkRead(FilesInf, SizeOf(FilesRecord));

      Info1 := FilesInf.Name;
      Info2 := FStr(FilesInf.AreaNum);
      If Info1='' then Info1 := '[Unused]';
    end
     else begin
            Info1 := '';
            Info2 := '';
          end; { if }
  Info3 := '';
  Info4 := '';
  Info5 := '';
  Info6 := '';
  Info7 := '';
  Info8 := '';
  Info9 := '';
end; { proc. FileArea_GetInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function FileArea_Seek(Str: String): LongInt;
begin
  FileArea_F^.Seek(0);

  FileArea_Seek := -1;

  While NOT FileArea_F^.EOF do
   begin
     FileArea_F^.BlkREad(FilesInf, SizeOf(FilesInf));
     if FileArea_F^.IoResult > 0 then ;

     if FilesInf.Name = '' then FilesInf.Name := '[Unused]';

     if SUpcase(Str) = SupCase(Copy(FilesInf.Name, 1, Length(Str))) then
       begin
         FileArea_Seek := FileArea_F^.FilePOS div SizeOf(FilesRecord);
         BREAK;
       end; { if }
   end; { While NOT eof }

end; { func. FileArea_Seek }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function FileArea_GetItems: LongInt;
begin
  FileArea_GetItems := (FileArea_F^.FileSize DIV SizeOf(FilesRecord)) + 01;
end; { func. FileArea_GetItems }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function FileArea_KeyPress(CH: Char; HiLight: LongInt;
                           var HiBarPos, TopOfScrn: Longint;
                           var StartRange, EndRange: LongInt): LongInt;

procedure AskToKillbase;
begin
  if FileCount(FilesInf.FilePath)=00 then
    begin
      ShadFillBox(16, 10, 61, 14, mnuErrorColor, mnuStyle, True);
      WriteAT(19, 12, mnuErrorColor, 'Kill the directory as well (y/n) ? °');

      CursorOn;
      repeat;
        GotoXY(54, 12);
        Ch := Upcase(ReadKey);
      Until CH in ['Y', 'N'];

      CursorOff;

      if CH='Y' then
        begin
          EraseDir(ForceBack(FilesInf.FilePath) + '*.*', true);
        end; { if }
    end; { if }

  if (NOT FileExist(GlobalCfg^.RaConfig^.FileBasePath + 'hdr'+BsChar+'fdb'+FStr(FilesInf.AreaNum)+'.hdr'))
      OR (NOT FileExist(GlobalCfg^.RaConfig^.FileBasePath + 'idx'+BsChar+'fdb'+FStr(FilesInf.AreaNum)+'.idx'))
       OR (NOT FileExist(GlobalCfg^.RaConfig^.FileBasePath + 'txt'+BsChar+'fdb'+FStr(FilesInf.AreaNum)+'.txt'))
        then EXIT;

  ShadFillBox(16, 10, 61, 14, mnuErrorColor, mnuStyle, True);
  WriteAT(19, 12, mnuErrorColor, 'Kill the file database as well (y/n) ? °');

  CursorOn;
  repeat;
    GotoXY(58, 12);
    Ch := Upcase(ReadKey);
  Until CH in ['Y', 'N'];

  CursorOff;

  if CH='Y' then
    begin
      GetFilesRecord(FilesInf, HiLight, True);
      EraseFile(GlobalCfg^.RaConfig^.FileBasePath + 'hdr'+BsChar+'fdb'+FStr(FilesInf.AreaNum)+'.hdr');
      EraseFile(GlobalCfg^.RaConfig^.FileBasePath + 'idx'+BsChar+'fdb'+FStr(FilesInf.AreaNum)+'.idx');
      EraseFile(GlobalCfg^.RaConfig^.FileBasePath + 'txt'+BsChar+'fdb'+FStr(FilesInf.AreaNum)+'.txt');
    end; { if }
end; { proc. AskToKillBase }

var Temp: LongInt;
    GlobInf: FilesRecord;
    Glob2Inf: EleFilesRecord;
    SaveScrn: Pointer;
    TempWord : SmallWord;
begin
  FileArea_Keypress := -1;
  if HiLight = FileArea_GetItems then
   if Ch <> #82 then EXIT;

  FileArea_F^.Seek( (HiLight - 1) * SizeOf(FilesRecord));
  FileArea_F^.BlkRead(FilesInf, SizeOf(FilesRecord));
  if FileArea_F^.IoResult > 00 then ;

  Case CH of
    { Delete }  #83 : begin
                        if (SemaExist(EleFileSemaphore)) OR
                            (SemaExist(EleFMgrSemaphore)) then
                          begin
                            MsgBox('EleFILE or EleMGR is running - cannot insert or delete areas', true, SaveScrn);
                            GetInput;
                            MsgBox('', false, SaveScrn);
                            EXIT;
                          end; { if }

                        CH := AskConfirmDelete;
                        If Ch='Y' then
                         begin;
                           If StartRange=EndRange then
                            begin;
                              StartRange := HiLight;
                              EndRange := HiLight;
                            end; { StartRange=EndRange }

                           AskToKillBase;
                           DeleteRecord(StartRange, EndRange,
                                        SizeOf(FilesRecord), FileArea_F,
                                        FilesInf);

                           DeleteRecord(StartRange, EndRange,
                                        SizeOf(EleFilesRecord), EleFILE_F,
                                        EleFilesInf);

                           StartRange := -1;
                           EndRange := -1;
                           GenerateRdxFiles(JustPath(FilesFileName), False, False, true);
                         end; { if True }
                      end; { Delete }
    { Insert }  #82 : begin;
                        if (SemaExist(EleFileSemaphore)) OR
                            (SemaExist(EleFMgrSemaphore)) then
                          begin
                            MsgBox('EleFILE or EleMGR is running - cannot insert or delete areas', true, SaveScrn);
                            GetInput;
                            MsgBox('', false, SaveScrn);
                            EXIT;
                          end; { if }

                        If StartRange=EndRange then
                          begin;
                            StartRange := HiLight;
                            EndRange := HiLight;
                          end; { StartRange=EndRange }


                          SetFilesDefaults(GlobInf);
                          SetFilesEleDefaults(Glob2Inf, GlobInf);

                          InsertRecord(StartRange, EndRange,
                                       SizeOf(FilesRecord), FileArea_F,
                                       FilesInf, GlobInf);

                          InsertRecord(StartRange, EndRange,
                                       SizeOf(EleFilesRecord), EleFILE_F,
                                       EleFilesInf, Glob2Inf);

                          StartRange := -1;
                          EndRange := -1;
                          GenerateRdxFiles(JustPath(FilesFileName), False, False, true);
                          FileArea_keypress := HiLight + 1;
                      end; { Insert }
    { Move }    #50,
    { Copy }    #46 : begin
                        if (SemaExist(EleFileSemaphore)) OR
                            (SemaExist(EleFMgrSemaphore)) then
                          begin
                            MsgBox('EleFILE or EleMGR is running - cannot copy or move areas', true, SaveScrn);
                            GetInput;
                            MsgBox('', false, SaveScrn);
                            EXIT;
                          end; { if }

                          DoList(True, 16, 41, 6, 0, 0, 00, 00, 00, 00, 00,                 { ShowLen }
                                 15, 4, 64, mnuScrnLen - 4,                                  { Window Coordinates }
                                 ' File areas ',
                                 'Select move/copy insertion point',
                                 {$IFDEF FPC}@{$ENDIF}FileArea_GetInfo,
                                 {$IFDEF FPC}@{$ENDIF}Copy_Activate,
                                 {$IFDEF FPC}@{$ENDIF}FileArea_Seek,
                                 {$IFDEF FPC}@{$ENDIF}Copy_AbortCheck,
                                 {$IFDEF FPC}@{$ENDIF}Copy_Keypress,
                                 {$IFDEF FPC}@{$ENDIF}FileArea_GetItems,
                                 {$IFDEF FPC}@{$ENDIF}CreateNewFilesRa,
                                 {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
                                 True,
                                 HiBarPos,
                                 TopOfScrn,
                                 true, true);

                         if Copypoint <> -1 then
                          if CH = #46 then
                           begin
                             MsgBox('WARNING!: The copied area/group will have the same number as original', true, SaveScrn);
                             GetInput;
                             MsgBox('WARNING!: The copied area/group will have the same number as original', false, SaveScrn);
                           end; { if }

                        If StartRange=EndRange then
                          begin;
                            StartRange := HiLight;
                            EndRange := HiLight;
                          end; { StartRange=EndRange }

                          CopyRecord(StartRange, EndRange, (CH=#50),
                                     SizeOf(FilesRecord), FileArea_F,
                                     GlobInf, 0);

                          CopyRecord(StartRange, EndRange, (CH=#50),
                                     SizeOf(EleFilesRecord), EleFILE_F,
                                     Glob2Inf, 0);

                          StartRange := -1;
                          EndRange := -1;
                          GenerateRdxFiles(JustPath(FilesFileName), False, False, true);
                      end; { copy }
   { Alt - P }   #25 : begin
                        ShadFillBoxTitle(30, (mnuScrnLen DIV 2) - 2, 50, (mnuScrnLen DIV 2) + 2, mnuBoxColor, mnuStyle, True,
                                         ' Jump to ');
                        WriteAT(32, (mnuScrnLen DIV 2), mnuNormColor, 'Jump to #');

                        GetFilesRecord(GlobInf, HiLight, false);
                        TempWord := GlobInf.AreaNum;

                        EditWord(TempWord, 42, (mnuScrnLen DIV 2), mnuEditColor, true);

                        if RdLastKey <> #27 then
                          begin
                            FileArea_KeyPress := Ra250Area(TempWord);
                          end; { if }

                      end; { Jump to area }
    { Global }  #34 : begin;
                        SetFilesDefaults(GlobInf);        { Clear the record }
                        SetFilesEleDefaults(Glob2Inf, GlobInf);
                        GlobInf.AreaNum := 00;
                        EditFileArea(GlobInf, Glob2Inf, True, GlobInf, Glob2Inf, False, MainChangeArray, false);

                        If StartRange=EndRange then
                          begin;
                            StartRange := HiLight;
                            EndRange := HiLight;
                          end; { StartRange=EndRange }

                        ShadFillBox(25, 10, 55, 14, mnuErrorColor, mnuStyle, True);
                        WriteAT(28, 12, mnuErrorColor, 'Apply changes (y/n)? °');

                        CursorOn;
                        Repeat;
                          GotoXY(49, 12);
                          Ch := Upcase(ReadKey);
                        Until CH in ['Y', 'N'];
                        CursorOff;

                        If CH='Y' then
                         begin
                            UpdateGlobalInfo(StartRange, EndRange, GlobInf,
                                             FilesInf, FileArea_F,
                                             Files, SizeOf(FilesRecord));

                            UpdateGlobalInfo(StartRange, EndRange, Glob2Inf,
                                             EleFilesInf, EleFILE_F,
                                             EleFile, SizeOf(EleFilesRecord));

                            StartRange := -1;
                            EndRange := -1;
                            GenerateRdxFiles(JustPath(FilesFileName), False, False, true);
                         end; { YES, Apply those changes }
                      end; { global Changes }
  End; { Case }
end; { func. FileArea_KeyPress }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function FileArea_Activate(HiLight: LongInt;  HiBarPos: LongInt): Longint;
var OldFilesInf   : FilesRecord;
    OldEleFilesInf: EleFilesRecord;
begin
  FileArea_Activate := -1;
  if HiLight = FileArea_GetItems then EXIT;

  FileArea_F^.Seek( (HiLight - 1) * SizeOf(FilesRecord));
  FileArea_F^.BlkRead(FilesInf, SizeOf(FilesRecord));

  EleFile_F^.Seek( (HiLight - 1) * SizeOf(EleFilesRecord));
  EleFile_F^.BlkRead(EleFilesInf, SizeOf(EleFilesRecord));

  OldFilesInf:= FilesInf;
  OldEleFilesInf := EleFilesInf;

  EditFileArea(FilesInf, EleFilesInf, False, FilesInf, EleFilesInf, False, MainChangeArray, false);
  EleFilesInf.AreaNum := FilesInf.AreaNum;

  OldFilesInf.AreaNum := FilesInf.AreaNum;
  If (RecordsDifferent(FilesInf, OldFilesInf, SizeOf(FilesRecord))) OR
      (RecordsDifferent(EleFilesInf, OldEleFilesInf, SizeOf(EleFilesRecord))) then
        If DoSaveChanges('Save changes (Y/n) ? °', True, false) then
              begin
                FileArea_F^.Seek( (HiLight - 1) * SizeOf(FilesRecord));
                FileArea_F^.BlkWrite(FilesInf, SizeOf(FilesRecord));

                EleFile_F^.Seek( (HiLight - 1) * SizeOf(EleFilesRecord));
                EleFile_F^.BlkWrite(EleFilesInf, SizeOf(EleFilesRecord));
              end { SaveChanges }
               else begin
                      FilesInf := OldFilesInf;
                      EleFilesInf := OldEleFilesinf;
                    end; { if }
end; { proc. FileArea_Activate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CreateEleFilesFiles;
var EleFile_F : pFileObj;
    EleFile   : EleFilesRecord;
    SaveScrn  : Pointer;
begin
  if FileExist(EleFilesFileName) then
   begin
     if GetFileSize(EleFilesFileName, SizeOf(EleFilesRecord)) <
         (FileArea_F^.FileSize div SizeOf(FilesRecord)) then
          begin
            if NOT DoSaveChanges('FILES.ELE is out of date. Regenerate? (Y/n)? °', true, false) then
              begin
                EXIT;
              end; { if }
          end { if }
            else EXIT;
    end; { if }

  New(EleFile_F, Init);
  EleFile_F^.Assign(EleFilesFileName);
  EleFile_F^.FileMode := ReadWriteMode + DenyWrite;
  if NOT EleFile_F^.Create(1) then
    begin
      MsgBox('Unable to create '+EleFilesFileName, true, SaveScrn);
      Getinput;
      MsgBox('Unable to create '+EleFIlesFileName, false, SaveScrn);

      Dispose(EleFile_F, Done);
      EXIT;
    end; { if }

  MsgBox('Creating information', true, SaveScrn);
  FileArea_F^.Seek(0);

  While NOT FileArea_F^.EOF do
    begin
      FileArea_F^.BlkRead(FilesInf, SizeOf(FilesRecord));
      if FileArea_F^.IoResult > 00 then BREAK;

      SetFilesEleDefaults(EleFile, FilesInf);

      EleFile_F^.BlkWrite(EleFile, SizeOf(EleFilesRecord));
    end; { while }

  MsgBox('Creating information', false, SaveScrn);

  Dispose(EleFile_F, Done);
end; { proc. CreateEleFilesfiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoFileAreas;
begin
  if NOT FileExist(FilesFileName) then
    if NOT CreateNewFilesRa then
        begin
          CantCreate(JustName(FilesFileName), False);
          EXIT;
       end; { if }

  if NOT OpenFile(FilesFileName, ReadWriteMode + DenyWrite) then
      begin
        CantCreate(JustName(FilesFileName), True);
        EXIT;
      end; { if }


  New(FileArea_F, Init);
  FileArea_F^.Assign(FilesFileName);
  FileArea_F^.FileMode := ReadWriteMode + DenyWrite;
  FileArea_F^.Open(1);

  CreateEleFilesFiles;

  if NOT OpenFile(EleFilesFileName, ReadWriteMode + DenyWrite) then
      begin
        CantCreate(JustName(EleFilesFileName), true);
        EXIT;
      end; { if }

  New(EleFile_F, Init);
  EleFile_F^.Assign(EleFilesFileName);
  EleFile_F^.FileMode := ReadWriteMode + DenyWrite;
  EleFile_F^.Open(1);

  if (NOT UpToDateFile(FilesFileName, RdxName(FilesFileName)))
      then GenerateRdxFiles(JustPath(FilesFileName), false, false, true);

  DoList(True, 16, 41, 6, 0, 0, 00, 00, 00, 00, 00,                                          { ShowLen }
         15, 4, 64, mnuScrnLen - 4,                                  { Window Coordinates }
         ' File areas ',
         'Enter-Edit Space-Tag (INS)ert (DEL)ete   (Alt-C)opy (Alt-M)ove (Alt-G)lobal',
         {$IFDEF FPC}@{$ENDIF}FileArea_GetInfo,
         {$IFDEF FPC}@{$ENDIF}FileArea_Activate,
         {$IFDEF FPC}@{$ENDIF}FileArea_Seek,
         {$IFDEF FPC}@{$ENDIF}General_AbortCheck,
         {$IFDEF FPC}@{$ENDIF}FileArea_Keypress,
         {$IFDEF FPC}@{$ENDIF}FileArea_GetItems,
         {$IFDEF FPC}@{$ENDIF}CreateNewFilesRa,
         {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
         True,
         00, 00,
         true, false);

  Dispose(FileArea_F, Done);
  Dispose(EleFILE_F, Done);

  If (NOT FileExist(RdxName(FilesFileName)))
    OR (NOT UpToDateFile(FilesFileName, RdxName(FilesFileName))) OR
     (NOT UpToDateFile(EleFilesFileName, RdxName(FilesFileName)))
      then GenerateRdxFiles(JustPath(FilesFileName), False, False, true);
end; { proc. DoFileAreas }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CopyRecord(StartRange, EndRange: LongInt; Move: Boolean;
                     RecordSize: Word; var FileVar: pFileObj;
                     var Buf; NrSkip: Word);
var LinkBuf_F   : File;
    LinkNew_F   : File;
    Counter     : LongInt;
    AccessToCopy: Boolean;
    DidCopy     : Boolean;
    IntBuf      : Array[0..1024 * 16] of Byte;      { Buffer of 16k, must be enough }
    OldFmode    : Longint;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'CopyRecord () - (BEGIN)');
  {$ENDIF}

  if CopyPoint = -1 then EXIT;
  if CopyPoint = EndRange then EXIT;

  if EndRange < StartRange then
    begin
      Counter := EndRange;
      EndRange := StartRange;
      StartRange := Counter;
    end; { if }

  ShadFillBox(22, 10, 55, 14, mnuErrorColor, mnuStyle, True);

  if Move then WriteCenter(12, mnuErrorColor, 'Moving record(s) at #'+FStr(CopyPoint))
   else WriteCenter(12, mnuErrorColor, 'Copying record(s) at #'+FStr(CopyPoint));
  UpdateScreenBuffer(true);
  DidCopy := FALSE;

  Assign(LinkNew_F, OpenRaFile('linknew.ra'));
  OldFMode := FileMode;
  FileMode := ReadWriteMode + DenyWrite;
  {$i-} Rewrite(LinkNew_F, 01); {$I+}
  If IOResult>00 then Exit;
  FileMode := OldFMode;

  Assign(LinkBuf_F, OpenRaFile('linkbuf.ra'));
  OldFMode := FileMode;
  FileMode := ReadWriteMode + DenyWrite;
  {$i-} Rewrite(LinkBuf_F, 01); {$I+}
  If IOResult>00 then Exit;
  FileMode := OldFMode;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'CopyRecord () - OK, copy the first few records');
  {$ENDIF}

  For Counter:=StartRange to EndRange do
   begin
     FileVar^.Seek( (Counter - 1) * RecordSize);
     FileVar^.BlkRead(Buf, RecordSize);

     {$i-}
       BlockWrite(LinkBuf_F, Buf, RecordSize);
     {$i+}
     if IOResult>00 then ;
   end; { for counter }

  FileVar^.Seek(0);

  Close(LinkBuf_F);
  FileMode := ReadWriteMode + DenyNone;
  Reset(LinkBuf_F, 1);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'CopyRecord () - Startup a loop (1st one) that copies the first part of the records');
  {$ENDIF}

  While NOT FileVar^.EOF do
      begin
        FileVar^.BlkRead(Buf, RecordSize);

        If Move then AccessToCopy := NOT InRange(FileVar^.FilePos div RecordSize, StartRange, EndRange)
         else AccessToCopy := True;

        If (FileVar^.FilePos div RecordSize) <> (CopyPoint) then
         If AccessToCopy then BlockWrite(LinkNew_F, Buf, RecordSize);

        If (FileVar^.FilePos div RecordSize) = (CopyPoint) then
               begin
                 For Counter:=StartRange to EndRange do
                  begin
                    {$IFDEF WITH_DEBUG}
                      DebugObj.DebugLog(logFileSys, 'CopyRecord () - Copy intbuf (begin)');
                      DebugObj.DebugLog(logFileSys, 'CopyRecord () - RecordSize = '+FStr(RecordSize));
                    {$ENDIF}

                    BlockRead(LinkBuf_F, IntBuf, RecordSize);
                    BlockWrite(LinkNew_F, IntBuf, RecordSize);
                    DidCopy := TRUE;
                  end; { for counter }

                 BlockWrite(LinkNew_F, Buf, RecordSize);
               end; { For counter }

      end; { While }

  if NOT DidCopy then
   For Counter:=StartRange to EndRange do
     begin
       BlockRead(LinkBuf_F, IntBuf, RecordSize);
       BlockWrite(LinkNew_F, IntBuf, RecordSize);
       DidCopy := TRUE;
    end; { for counter }

  {$i-}
    Close(LinkBuf_F);
    Erase(LinkBuf_F);
    Close(LinkNew_F);

    FileVar^.Close;
    FileVar^.Erase;

    RenameWithDrive(LinkNew_F, FileVar^.FileName);

    FileVar^.FileMode := ReadWriteMode + DenyWrite;
    FileVar^.Open(1);
  {$I+}
  if IOResult>00 then ;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logFileSys, 'CopyRecord () - ( END )');
  {$ENDIF}
end; { proc. CopyRecord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure InsertRecord(StartRange, EndRange: LongInt;
                       RecordSize: Word; var FileVar: pFileObj;
                       var Buf; var DefBuf);
var LinkNew_F: pFileObj;
    Counter  : LongInt;
    Inserted : Boolean;
begin
  ShadFillBox(22, 10, 55, 14, mnuErrorColor, mnuStyle, True);
  WriteCenter(12, mnuErrorColor, 'Inserting record(s) at #'+FStr(StartRange));
  UpdateScreenBuffer(true);
  Inserted := FALSE;

  New(LinkNew_F, Init);
  LinkNew_F^.Assign(OpenRaFile('linknew.ra'));
  LinkNew_F^.FileMode := ReadWriteMode + DenyWrite;
  if NOT LinkNew_F^.Create(1) then
    begin
      Dispose(LinkNew_F, Done);
      EXIT;
    end; { if }

  FileVar^.Seek(0);
  While NOT FileVar^.EOF do
      begin
        FileVar^.BlkRead(Buf, RecordSize);

        If (FileVar^.FilePos div RecordSize) <> (StartRange) then
          LinkNew_F^.BlkWrite(Buf, RecordSize)
          else begin                                      { Inserting records }
                 for Counter:=StartRange to EndRange do
                   LinkNew_F^.BlkWrite(DefBuf, RecordSize);


                 LinkNew_F^.BlkWrite(Buf, RecordSize);
                 Inserted := TRUE;
               end; { Insert new records }
      end; { While }

  if NOT Inserted then
    For Counter:=StartRange to EndRange do
      LinkNew_F^.BlkWrite(DefBuf, RecordSize);

  {$i-}
    LinkNew_F^.Close;
    FileVar^.Close;
    FileVar^.Erase;

    RenameObjWithDrive(LinkNew_F, FileVar^.FileName);
    Dispose(LinkNew_F, Done);

    FileVar^.FileMode := ReadWriteMode + DenyWrite;
    FileVar^.Open(1);
  {$I+}
  if IOResult>00 then ;
end; { proc. InsertRecord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DeleteRecord(StartRange, EndRange: LongInt;
                       RecordSize: Word; var FileVar: pFileObj;
                       var Buf);
var LinkNew_F: pFileObj;
    Counter  : LongInt;
begin
  ShadFillBox(22, 10, 55, 14, mnuErrorColor, mnuStyle, True);
  WriteCenter(12, mnuErrorColor, 'Deleting record #'+FStr(StartRange)+'-#'+FStr(EndRange));
  UpdateScreenBuffer(true);

  New(LinkNew_F, Init);
  LinkNew_F^.Assign(OpenRaFile('linknew.ra'));
  LinkNew_F^.FileMode := ReadWriteMode + DenyWrite;
  if NOT LinkNew_F^.Create(1) then
    begin
      Dispose(LinkNew_F, Done);
      EXIT;
    end; { if }

  FileVar^.Seek(0);

  While NOT FileVar^.EOF do
      begin
        FileVar^.BlkRead(Buf, RecordSize);

        If NOT InRange(Filevar^.FilePos div RecordSize, StartRange, EndRange) then
          LinkNew_F^.BlkWrite(Buf, RecordSize);
      end; { While }

  LinkNew_F^.Close;
  FileVar^.Close;
  FileVar^.Erase;

  RenameObjWithDrive(LinkNew_F, FileVar^.FileName);
  Dispose(LinkNew_F, Done);

  FileVar^.FileMode := ReadWriteMode + DenyWrite;
  FileVar^.Open(1);
end; { proc. DeleteMsgRecord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure UpdateGlobalInfo(StartRange, EndRange: LongInt; var GlobInf;
                           var ReadInf; var FileVar: pFileObj;
                           UpdateType: GlobalUpdateType;
                           RecordSize: Word);
var LinkNew_F: pFileObj;
    Counter  : LongInt;
    OldFMode : Longint;
    MsgInf   : MessageRecord;
begin
 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFileIO, 'Updateglobalinfo');
 {$ENDIF}

  ShadFillBox(22, 10, 55, 14, mnuErrorColor, mnuStyle, True);
  WriteCenter(12, mnuErrorColor, 'Updating record(s) #'+FStr(StartRange)+'-#'+FStr(EndRange));

 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFileIO, 'Creating linknew.ra');
 {$ENDIF}

  New(LinkNew_F, Init);
  LinkNew_F^.Assign(OpenRaFile('linknew.ra'));
  LinkNew_F^.FileMode := ReadWriteMode + DenyWrite;
  if NOT LinkNew_F^.Create(1) then
    begin
      Dispose(LinkNew_F, Done);
      EXIT;
    end; { if }
  FileVar^.Seek(0);

 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFileIO, 'Changing the mess (UPDATEGLOBALINFO)');
 {$ENDIF}

  While NOT FileVar^.EOF do
      begin
        {$IFDEF WITH_DEBUG}
          DebugObj.DebugLog(logFileIO, 'Reading from filevar: Filepos='+Fstr(Filevar^.FilePos));
        {$ENDIF}

        FileVar^.BlkRead(ReadInf, RecordSize);

        If NOT InRange(FileVar^.FilePos div RecordSize, StartRange, EndRange) then
         LinkNew_F^.BlkWrite(ReadInf, RecordSize)
          else begin
                  Case UpdateType of
                    Message : EditMsgArea(MessageRecord(ReadInf), EleMsgInf,
                                          False,MessageRecord(GlobInf),
                                          EleMsgInf, True,MainChangeArray, false);
                    EleMsg  : EditMsgArea(MsgInf, EleMessageRecord(ReadInf),
                                          False, MsgInf,
                                          EleMessageRecord(GlobInf), True,MainChangeArray, true);
                    Files   : EditFileArea(FilesRecord(ReadInf), EleFilesInf,
                                           false, FilesRecord(GlobInf),
                                           EleFilesInf, True, MainChangeArray, false);
                    EleFILE : EditFileArea(FilesInf, EleFilesRecord(ReadInf),
                                           false, FilesInf, EleFilesRecord(GlobInf),
                                           true, MainChangeArray, true);
                    Groups  : EditGroupRecord(GroupRecord(ReadInf), False, GroupRecord(GlobInf), True, MainChangeArray);
                  End; { Case UpdateType of }

                  LinkNew_F^.BlkWrite(ReadInf, RecordSize);
               end; { Update the info for the record }
      end; { While }

  LinkNew_F^.Close;
  FileVar^.Close;
  FileVar^.Erase;

  RenameObjWithDrive(LinkNew_F, FileVar^.FileName);
  Dispose(LinkNew_F, Done);

  FileVar^.FileMode := ReadWriteMode + DenyNone;
  FileVar^.Open(1);

 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFileIO, 'End of updateglobalinfo');
 {$ENDIF}
end; { proc. UpdateGlobalInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  MessageType2Str(B: Byte): String;
begin
  MessageType2Str := '';

  Case MsgType(b) of
   LocalMail : MessageType2Str := 'Local    ';
   NetMail   : MessageType2Str := 'Netmail  ';
   EchoMail  : MessageType2Str := 'Echomail ';
   InterNet  : MessageType2Str := 'Internet ';
   NewsGroup : MessageType2Str := 'Newsgroup';
   ForumGroup: MessageType2Str := 'Forum    ';
  End; { case b }
end; { func. MessageType2Str }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function GroupType2Str(B: Byte): String;
begin
  GroupType2Str := '';

  Case GroupType(b) of
    group_Normal : GroupType2Str := 'Normal  ';
    group_Forum  : GroupType2Str := 'Forum   ';
  end; { case }
end; { func. GroupType2Str }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function Post2Str(B: Byte): String;
begin
  Post2Str := '';

  Case AccessType(B) of
    nwsPostNever      : Post2Str := 'Never post              ';
    nwsPostAlways     : Post2Str := 'Always post             ';
    nwsPostUseSettings: Post2Str := 'Registered post         ';
    nwsUseBoth        : Post2Str := 'Registered for read/post';
  end; { case }
end; { func. Post2Str }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function  Status2Str(B: Byte): String;
begin
  Status2str := '';

  Case MsgKindsType(B) of
    ElemsgBoth    : Status2Str := 'Pvt/Pub  ';
    ElemsgPrivate : Status2Str := 'Private  ';
    ElemsgPublic  : Status2Str := 'Public   ';
    ElemsgROnly   : Status2Str := 'Read-only';
    ElemsgNoReply : Status2Str := 'No-reply ';
  End; { Case }
end; { func. Status2Str }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


Function  Users2Str(B: Byte): String;
begin
  Users2Str := '';

  If (ReadBit(B, 3)) AND (NOT ReadBit(B, 5)) then
    Users2Str := 'Pick an alias   ';

  If (ReadBit(B, 5))
   then Users2Str := 'Handles only   ';

  If (NOT ReadBit(B, 3)) AND (NOT ReadBit(B, 5)) then
   Users2Str := 'Real names only';
end; { func. Users2Str }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure EditMsgArea(var MessageInf: MessageRecord;
                      var EleMsgInfo: EleMessageRecord;
                      GlobChange: Boolean;
                      Template: MessageRecord;
                      NewTemplate: EleMessageRecord;
                      GlobalProcessing: Boolean;
                      var ChangeArray: GlobalChangeArray;
                      DoingEleMSG: Boolean);

function EditAka(B: Byte; DisMenu: PullRecord): Byte;
var SaveScrn: Pointer;
begin
  EnDisAbleMenu(DisMenu, False);

  SaveScreen(SaveScrn);
    AkaLister(false);
  RestoreScreen(SaveScrn);

  if AddressEditor_Selected > 0 then
    EditAKa := AddressEditor_Selected - 1
     else EditAka := B;

  EnDisAbleMenu(DisMenu, True);
end; { func. EditAka }


procedure EditNewsGroup;
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

  AddPullItem(Menu.PullInf^[01], 'Name            ', 1, #00, 'Name of this group', 01);
  AddPullItem(Menu.PullInf^[02], 'Available       ', 2, #00, 'Is this area available for the newsserver', 01);
  AddPullItem(Menu.PullInf^[03], 'Posting allowed ', 3, #00, 'Are users allowed to post messages from NEWSSRV', 01);
  AddPullItem(Menu.PullInf^[04], 'Extract to area ', 4, #00, 'Extract attachments found to file-area' +
                                                             ' (0 = don''t extract)', 01);
  AddPullItem(Menu.PullInf^[05], 'Override from   ', 5, #00, 'Override the From: address by the users BBS-names?', 01);

  Menu.Items   := 5;
  Menu.Width   := 75;
  Menu.Length  := 5;
  Menu.X       := 03;
  Menu.Y       := 6;
  Menu.HiLight := 1;
  Menu.AddSpace:= True;
  Menu.Title   := ' Newsgroup settings ';
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
        WriteAT(22, 08, mnuNormColor, EleMsgInfo.GroupName);
        WriteAT(22, 09, mnuNormColor, Bit2Str(EleMsgInfo.Attribute, 0));
        WriteAT(22, 10, mnuNormColor, Post2Str(Byte(EleMsgInfo.AccessSettings)));
        WriteAT(22, 11, mnuNormColor, FStr(EleMsgInfo.AttachArea));
        WriteAT(22, 12, mnuNormColor, Bit2Str(EleMsgInfo.Attribute, 1));

        DirectScrnUpdate := SaveDirect;
        Choice := DoPullMenu(Menu, CH, False, False);
      end; { if }

    if GlobalProcessing then
      if Choice < Menu.Items then Inc(Choice)
        else Choice := 00;

    if GlobChange then
     if Choice > 00 then Menu.PullInf^[Choice].mnuCurColor := mnuGlobColor;


    Case Choice of
     1 : begin
            If (NOT GlobalProcessing)  then
              GetString(22, 08, mnuEditColor, EleMsgInfo.GroupName, [#33..#254], [#13, #27],
                        128, 55, false, false, true, false, 0)
               else if DoGlobalChange(100 + Choice) then
                      begin
                        EleMsgInfo.GroupName := NewTemplate.GroupName;
                      end; { if}
         end; { if }
     2 : begin
           If (NOT GlobalProcessing) then
              EleMsgInfo.Attribute := Edit_Bit(EleMsgInfo.Attribute, 0, 22, 09, mnuNormColor)
               else if DoGlobalChange(100 + Choice) then
                 EleMsgInfo.Attribute := NewTemplate.Attribute;
         end; { if }
     3 : begin
           If (NOT GlobalProcessing) then
             begin
               if Byte(EleMsgInfo.AccessSettings) < Byte(nwsUseBoth) then
                 Inc(Byte(EleMsgInfo.AccessSettings))
                   else EleMsgInfo.AccessSettings := nwsPostNever;
             end else if DoGlobalChange(100 + Choice) then
                   EleMsgInfo.AccessSettings := NewTemplate.AccessSettings;
         end; { if }
     4 : begin
            If (NOT GlobalProcessing)  then
              begin
                TmpWord := eleMsgInfo.AttachArea;

                EditWord(TmpWord, 22, 11, mnuEditColor, true);

                EleMsgInfo.AttachArea := TmpWord;
              end
               else if DoGlobalChange(100 + Choice) then
                      begin
                        EleMsgInfo.AttachArea:= NewTemplate.AttachArea;
                      end; { if}
         end; { if }
     5 : begin
           If (NOT GlobalProcessing) then
              EleMsgInfo.Attribute := Edit_Bit(EleMsgInfo.Attribute, 1, 22, 09, mnuNormColor)
               else if DoGlobalChange(100 + Choice) then
                 EleMsgInfo.Attribute := NewTemplate.Attribute;
         end; { if }
    end; { Case }

  until (Choice=00);

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
end; { proc. EditNewsGroup }

var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    Address   : NetAddress;
    TempStr   : String;
    OldMsgInf : MessageRecord;
    Teller    : Byte;
    SaveDirect: Boolean;
    TempMsgPtr: AbsMsgPtr;
begin
  OldMsgInf := MessageInf;
 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logfileIO, 'Start of EditMsgArea');
 {$ENDIF}

  AllocMem(Menu.PullInf, SizeOf(PullInfRecArray),  'PullInfRecArray', 'EditMsgArea');
  InitPullMenu(Menu);
  AddPullItem(Menu.PullInf^[01], 'Name  ', 01, #00, 'Name of this message area', 01);
  AddPullItem(Menu.PullInf^[02], 'Origin', 02, #00, 'Origin line for this message area', 01);
  AddPullItem(Menu.PullInf^[03], '', 03, #00, '', 01);
  AddPullItem(Menu.PullInf^[04], 'Read security', 04, #00, 'Security and flag settings required to read '+
                                                           'messages in this area', 01);
  AddPullItem(Menu.PullInf^[05], 'A flag', 05, #00, 'Security and flag settings required to read messages in this area', 01);
  AddPullItem(Menu.PullInf^[06], 'B flag', 06, #00, 'Security and flag settings required to read messages in this area', 01);
  AddPullItem(Menu.PullInf^[07], 'C flag', 07, #00, 'Security and flag settings required to read messages in this area', 01);
  AddPullItem(Menu.PullInf^[08], 'D flag', 08, #00, 'Security and flag settings required to read messages in this area', 01);
  AddPullItem(Menu.PullInf^[09], '', 09, #00, '', 01);
  AddPullItem(Menu.PullInf^[10], 'Write security', 10, #00, 'Security and flag settings required to write '+
                                                            'messages in this area', 01);
  AddPullItem(Menu.PullInf^[11], 'A flag', 11, #00, 'Security and flag settings required to write messages in this area', 01);
  AddPullItem(Menu.PullInf^[12], 'B flag', 12, #00, 'Security and flag settings required to write messages in this area', 01);
  AddPullItem(Menu.PullInf^[13], 'C flag', 13, #00, 'Security and flag settings required to write messages in this area', 01);
  AddPullItem(Menu.PullInf^[14], 'D flag', 14, #00, 'Security and flag settings required to write messages in this area', 01);
  AddPullItem(Menu.PullInf^[15], '', 15, #00, '', 01);
  AddPullItem(Menu.PullInf^[16], 'Sysop security', 16, #00, 'Security and flag settings required to perform '+
                                                            'maintenance on this area', 01);
  AddPullItem(Menu.PullInf^[17], 'A flag', 17, #00, 'Security and flag settings required to perform '+
                                                    'maintenance on this area', 01);
  AddPullItem(Menu.PullInf^[18], 'B flag', 18, #00, 'Security and flag settings required to perform '+
                                                    'maintenance on this area', 01);
  AddPullItem(Menu.PullInf^[19], 'C flag', 19, #00, 'Security and flag settings required to perform '+
                                                    'maintenance on this area', 01);
  AddPullItem(Menu.PullInf^[20], 'D flag', 20, #00, 'Security and flag settings required to perform '+
                                                    'maintenance on this area', 01);

  AddPullItem(Menu.PullInf^[21], 'Type     ', 21, #00, 'Local, EchoMail, NetMail, Internet, Newsgroup or Forum', 02);
  AddPullItem(Menu.PullInf^[22], 'Status   ', 22, #00, 'Private/public, private only, public only or read-only messages', 02);
  AddPullItem(Menu.PullInf^[23], 'Users    ', 23, #00, 'User names - Real names only, handles only or ask for an alias', 02);
  AddPullItem(Menu.PullInf^[24], 'Days old ', 24, #00, 'Number of days to keep old messages', 02);
  AddPullItem(Menu.PullInf^[25], 'Days rcvd', 25, #00, 'Number of days to keep received messages', 02);
  AddPullItem(Menu.PullInf^[26], 'Max msgs ', 26, #00, 'Maximum number of messages to keep', 02);
  AddPullItem(Menu.PullInf^[27], 'Echoinfo ', 27, #00, 'Append an origin line to outbound messages?', 02);
  AddPullItem(Menu.PullInf^[28], 'Combined ', 28, #00, 'Permit combined access to this message area?', 02);
  AddPullItem(Menu.PullInf^[29], 'Attaches ', 29, #00, 'Permit users to attach files to messages?', 02);
  AddPullItem(Menu.PullInf^[30], 'SoftCRs  ', 30, #00, 'Treat SoftCRs as normal characters in this area?', 02);
  AddPullItem(Menu.PullInf^[31], 'Deletes  ', 31, #00, 'Permit users to delete messages in this area?', 02);
  AddPullItem(Menu.PullInf^[32], 'AKA      ', 32, #00, 'Alias (AKA) address to append to the origin line (EchoMail only)', 02);
  AddPullItem(Menu.PullInf^[33], 'Age      ', 33, #00, 'Minimum age required to access this area', 02);
  AddPullItem(Menu.PullInf^[34], 'Group    ', 34, #00, 'Primary message group this area is in', 02);
  AddPullItem(Menu.PullInf^[35], 'Area type', 35, #00, 'Area type (JAM or Hudson)', 02);
  AddPullItem(Menu.PullInf^[36], 'Board    ', 36, #00, 'Hudson message board number (1-200); Press (F1) to edit', 02);
  AddPullItem(Menu.PullInf^[37], 'Edits    ', 37, #00, 'Allow users to edit their own messages?', 02);

  AddPullItem(Menu.PullInf^[38], 'AltGroup1', 38, #00, 'Secondary group to which this area belongs', 03);
  AddPullItem(Menu.PullInf^[39], 'AltGroup2', 39, #00, 'Secondary group to which this area belongs', 03);
  AddPullItem(Menu.PullInf^[40], 'AltGroup3', 40, #00, 'Secondary group to which this area belongs', 03);
  AddPullItem(Menu.PullInf^[41], 'AllGroups', 41, #00, '"Yes" means this area will appear in all groups', 03);
  AddPullItem(Menu.PullInf^[42], 'Netreply ', 42, #00, 'Area in which to post netmail replies (0=first available)', 03);
  AddPullItem(Menu.PullInf^[43], 'Newsgroup', 43, #00, 'Properties for this area for the newsgroup server', 03);

  Menu.PullInf^[03].mnuSelect := False;
  Menu.PullInf^[09].mnuSelect := False;
  Menu.PullInf^[15].mnuSelect := False;

  Menu.Items   := 43;
  Menu.Width   := 79;
  Menu.Length  := 20;
  Menu.X       := 1;
  Menu.Y       := 1;
  Menu.HiLight := 01;
  Menu.AddSpace:= True;
  Menu.SaveScrn:= nil;
  If NOT GlobChange then
    Menu.Title   := ' Message area '+MakeLen(FStr(MessageInf.AreaNum), 05, space, false, false)+' '
     else Menu.Title := ' Template for global changes ';
  Menu.PosArray[1].XInc := 00;
  Menu.PosArray[1].YDec := 00;
  Menu.PosArray[2].XInc := 23;
  Menu.PosArray[2].YDec := 17;
  Menu.PosArray[3].XInc := 51;
  Menu.PosArray[3].YDec := 34;

  If NOT GlobalProcessing then  ShowMenu(Menu, False);

  Choice := 00;
  Repeat;
   EdittingMsgArea := True;
   SaveDirect := DirectScrnUpdate;
   DirectScrnUpdate := false;


   If (NOT GlobChange) AND (NOT GlobalProcessing) then
     WriteAT(2, 1, mnuTitleColor, ' (F1) to edit area number ');

   If (GlobChange) OR (GLobalProcessing) then EdittingMsgArea := false;                  { Disable F1 }

   If NOT GlobalProcessing then
    begin
       If ReadBit(MessageInf.Attribute, 7) then
        begin
          AddPullItem(Menu.PullInf^[36], 'JAMbase  ', 36, #00, 'Full path and base filename of JAM area', 02)
        end; { if }

       If ReadBit(EleMsgInfo.Attribute, 2) then
        begin
          AddPullItem(Menu.PullInf^[36], 'Squidbase', 36, #00, 'Full path and base filename of Squid area', 02)
        end; { if }

       if (NOT ReadBit(EleMsgInfo.Attribute, 2)) AND
           (NOT ReadBit(MessageInf.Attribute, 7)) then
             begin
               AddPullItem(Menu.PullInf^[36], 'Board    ', 36, #00, 'Hudson message board number (1-200); '+
                                                                    ' Press (F1) to edit', 02);
             end; { if }

       if MessageInf.AkaAddress > 20 then
            MessageInf.AkaAddress := 20;

       GetAddress(MessageInf.AkaAddress, Address);
       With Address do
        StUtils.AddrToString(TempStr, Zone, Net, Node, Point);

       WriteAT(10, 03, mnuNormColor, MessageInf.Name);
       WriteAT(10, 04, mnuNormColor, MessageInf.OriginLine);
       WriteAT(17, 06, mnuNormColor, FStr(MessageInf.ReadSecurity));
       WriteAT(10, 07, mnuNormColor, Byte2FlagsOff(MessageInf.ReadFlags[1], MessageInf.ReadNotFlags[1]));
       WriteAT(10, 08, mnuNormColor, Byte2FlagsOff(MessageInf.ReadFlags[2], MessageInf.ReadNotFlags[2]));
       WriteAT(10, 09, mnuNormColor, Byte2FlagsOff(MessageInf.ReadFlags[3], MessageInf.ReadNotFlags[3]));
       WriteAT(10, 10, mnuNormColor, Byte2FlagsOff(MessageInf.ReadFlags[4], MessageInf.ReadNotFlags[4]));
       WriteAT(18, 12, mnuNormColor, FStr(MessageInf.WriteSecurity));
       WriteAT(10, 13, mnuNormColor, Byte2FlagsOff(MessageInf.WriteFlags[1], MessageInf.WriteNotFlags[1]));
       WriteAT(10, 14, mnuNormColor, Byte2FlagsOff(MessageInf.WriteFlags[2], MessageInf.WriteNotFlags[2]));
       WriteAT(10, 15, mnuNormColor, Byte2FlagsOff(MessageInf.WriteFlags[3], MessageInf.WriteNotFlags[3]));
       WriteAT(10, 16, mnuNormColor, Byte2FlagsOff(MessageInf.WriteFlags[4], MessageInf.WriteNotFlags[4]));
       WriteAT(18, 18, mnuNormColor, FStr(MessageInf.SysOpSecurity));
       WriteAT(10, 19, mnuNormColor, Byte2FlagsOff(MessageInf.SysOpFlags[1], MessageInf.SysOpNotFlags[1]));
       WriteAT(10, 20, mnuNormColor, Byte2FlagsOff(MessageInf.SysOpFlags[2], MessageInf.SysOpNotFlags[2]));
       WriteAT(10, 21, mnuNormColor, Byte2FlagsOff(MessageInf.SysOpFlags[3], MessageInf.SysOpNotFlags[3]));
       WriteAT(10, 22, mnuNormColor, Byte2FlagsOff(MessageInf.SysOpFlags[4], MessageInf.SysOpNotFlags[4]));

       WriteAT(36, 06, mnuNormColor, MessageType2Str(Byte(MessageInf.Typ)));
       WriteAT(36, 07, mnuNormColor, Status2Str(Byte(MessageInf.MsgKinds)));
       WriteAT(36, 08, mnuNormColor, Users2Str(Byte(MessageInf.Attribute)));
       WriteAT(36, 09, mnuNormColor, FStr(MessageInf.DaysKill));
       WriteAT(36, 10, mnuNormColor, FStr(MessageInf.RecvKill));
       WriteAT(36, 11, mnuNormColor, FStr(MessageInf.CountKill));
       WriteAT(36, 12, mnuNormColor, Bit2Str(MessageInf.Attribute, 0));
       WriteAT(36, 13, mnuNormColor, Bit2Str(MessageInf.Attribute, 1));
       WriteAT(36, 14, mnuNormColor, Bit2Str(MessageInf.Attribute, 2));
       WriteAT(36, 15, mnuNormColor, Bit2Str(MessageInf.Attribute, 4));
       WriteAT(36, 16, mnuNormColor, Bit2Str(MessageInf.Attribute, 6));
       WriteAT(36, 17, mnuNormColor, MakeLen(TempStr, 20, space, false, false));
       WriteAT(36, 18, mnuNormColor, FStr(MessageInf.Age));
       WriteAT(36, 19, mnuNormColor, FStr(MessageInf.Group));
       if ReadBit(EleMsgInf.Attribute, 2) then
         begin
           WriteAT(36, 20, mnuNormColor, 'Squish');
           WriteAT(36, 21, mnuNormColor, Copy(MessageInf.JamBase, 1, 43));
         end
          else if ReadBit(MessageInf.Attribute, 7) then
                 begin
                   WriteAT(36, 20, mnuNormColor, 'JAM   ');
                   WriteAT(36, 21, mnuNormColor, Copy(MessageInf.JamBase, 1, 43));
                 end { Is a JAM area }
                  else begin
                         WriteAT(36, 20, mnuNormColor, 'Hudson');
                         WriteAT(36, 21, mnuNormColor, MakeLen('', 43, space, false, false));
                       end; { is a Hudson Area }
       WriteAT(36, 22, mnuNormColor, Bit2Str(EleMsgInf.Attribute, 3));

       WriteAT(64, 06, mnuNormColor, FStr(MessageInf.Altgroup[1]));
       WriteAT(64, 07, mnuNormColor, FStr(MessageInf.Altgroup[2]));
       WriteAT(64, 08, mnuNormColor, FStr(MessageInf.Altgroup[3]));
       WriteAT(64, 09, mnuNormColor, Bit2Str(MessageInf.Attribute2, 0));
       WriteAT(64, 10, mnuNormColor, FStr(MessageInf.NetMailArea));

       DirectScrnUpdate := SaveDirect;
       Choice := DoPullMenu(Menu, CH, False, False);
    End; { GlobalProcessing }

   If GlobalProcessing then
    If Choice<Menu.Items then Inc(Choice)
     else Choice:=00;                                     { Terminate session }

   If GlobChange then
     If Choice>00 then Menu.PullInf^[Choice].mnuCurColor := mnuGlobColor;

   Case Choice of
      01 : begin;
              If NOT GlobalProcessing then
                MessageInf.Name := Edit(Messageinf.Name, 10, 03, mnuEditColor, 40, False, False, True)
                 else If DoGlobalChange(Choice) then MessageInf.Name := Template.Name;
           end;
      02 : begin;
              If NOT GlobalProcessing then
               MessageInf.OriginLine:= Edit(Messageinf.OriginLine, 10, 04, mnuEditColor, 60, False, False, True)
                 else If DoGlobalChange(Choice) then MessageInf.OriginLine:= Template.OriginLine;
           end;
      03 : { Gap } ;
      04 : begin;
              If NOT GlobalProcessing then
                 EditWord(MessageInf.ReadSecurity, 17, 06, mnuEditColor, True)
                   else If DoGlobalChange(Choice) then MessageInf.ReadSecurity:= Template.ReadSecurity;
           end;
      05 : begin;
              If NOT GlobalProcessing then
                 EditFlags(MessageInf.ReadFlags[1], MessageInf.ReadNotFlags[1], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        MessageInf.ReadFlags[1]:= Template.ReadFlags[1];
                                                        MessageInf.ReadNotFlags[1]:= Template.ReadNotFlags[1];
                                                       end; { GlobalChange }
           end;
      06 : begin;
              If NOT GlobalProcessing then
                 EditFlags(MessageInf.ReadFlags[2], MessageInf.ReadNotFlags[2], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        MessageInf.ReadFlags[2]:= Template.ReadFlags[2];
                                                        MessageInf.ReadNotFlags[2]:= Template.ReadNotFlags[2];
                                                       end; { GlobalChange }
           end;
      07 : begin;
              If NOT GlobalProcessing then
                 EditFlags(MessageInf.ReadFlags[3], MessageInf.ReadNotFlags[3], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        MessageInf.ReadFlags[3]:= Template.ReadFlags[3];
                                                        MessageInf.ReadNotFlags[3]:= Template.ReadNotFlags[3];
                                                       end; { GlobalChange }
           end;
      08 : begin;
              If NOT GlobalProcessing then
                 EditFlags(MessageInf.ReadFlags[4], MessageInf.ReadNotFlags[4], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        MessageInf.ReadFlags[4]:= Template.ReadFlags[4];
                                                        MessageInf.ReadNotFlags[4]:= Template.ReadNotFlags[4];
                                                       end; { GlobalChange }
           end;
      09 : { Gap } ;
      10 : begin;
              If NOT GlobalProcessing then
                 EditWord(MessageInf.WriteSecurity, 18, 12, mnuEditColor, True)
                   else If DoGlobalChange(Choice) then MessageInf.WriteSecurity := Template.WriteSecurity;
           end;
      11 : begin;
              If NOT GlobalProcessing then
                 EditFlags(MessageInf.WriteFlags[1], MessageInf.WriteNotFlags[1], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        MessageInf.WriteFlags[1]:= Template.WriteFlags[1];
                                                        MessageInf.WriteNotFlags[1]:= Template.WriteNotFlags[1];
                                                       end; { GlobalChange }
           end;
      12 : begin;
              If NOT GlobalProcessing then
                 EditFlags(MessageInf.WriteFlags[2], MessageInf.WriteNotFlags[2], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        MessageInf.WriteFlags[2]:= Template.WriteFlags[2];
                                                        MessageInf.WriteNotFlags[2]:= Template.WriteNotFlags[2];
                                                       end; { GlobalChange }
           end;
      13 : begin;
              If NOT GlobalProcessing then
                 EditFlags(MessageInf.WriteFlags[3], MessageInf.WriteNotFlags[3], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        MessageInf.WriteFlags[3]:= Template.WriteFlags[3];
                                                        MessageInf.WriteNotFlags[3]:= Template.WriteNotFlags[3];
                                                       end; { GlobalChange }
           end;
      14 : begin;
              If NOT GlobalProcessing then
                 EditFlags(MessageInf.WriteFlags[4], MessageInf.WriteNotFlags[4], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        MessageInf.WriteFlags[4]:= Template.WriteFlags[4];
                                                        MessageInf.WriteNotFlags[4]:= Template.WriteNotFlags[4];
                                                       end; { GlobalChange }
           end;
      15 : { Gap } ;
      16 : begin;
              If NOT GlobalProcessing then
                 EditWord(MessageInf.SysOpSecurity, 18, 18, mnuEditColor, True)
                   else If DoGlobalChange(Choice) then MessageInf.SysOpSecurity := Template.SysOpSecurity;
           end;
      17 : begin;
              If NOT GlobalProcessing then
                 EditFlags(MessageInf.SysOpFlags[1], MessageInf.SysOpNotFlags[1], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        MessageInf.SysOpFlags[1]:= Template.SysOpFlags[1];
                                                        MessageInf.SysOpNotFlags[1]:= Template.SysOpNotFlags[1];
                                                       end; { GlobalChange }
           end;
      18 : begin;
              If NOT GlobalProcessing then
                 EditFlags(MessageInf.SysOpFlags[2], MessageInf.SysOpNotFlags[2], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        MessageInf.SysOpFlags[2]:= Template.SysOpFlags[2];
                                                        MessageInf.SysOpNotFlags[2]:= Template.SysOpNotFlags[2];
                                                       end; { GlobalChange }
           end;
      19 : begin;
              If NOT GlobalProcessing then
                 EditFlags(MessageInf.SysOpFlags[3], MessageInf.SysOpNotFlags[3], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        MessageInf.SysOpFlags[3]:= Template.SysOpFlags[3];
                                                        MessageInf.SysOpNotFlags[3]:= Template.SysOpNotFlags[3];
                                                       end; { GlobalChange }
           end;
      20 : begin;
              If NOT GlobalProcessing then
                 EditFlags(MessageInf.SysOpFlags[4], MessageInf.SysOpNotFlags[4], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        MessageInf.SysOpFlags[4]:= Template.SysOpFlags[4];
                                                        MessageInf.SysOpNotFlags[4]:= Template.SysOpNotFlags[4];
                                                       end; { GlobalChange }
           end;
      21 : begin;
              If NOT GlobalProcessing then
                 If Byte(MessageInf.Typ)<Byte(ForumGroup) then Inc(Byte(MessageInf.Typ))
                  else MessageInf.Typ := LocalMail;

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  MessageInf.Typ := Template.Typ;
           end;
     22 : begin;
              If NOT GlobalProcessing then
                 If Byte(MessageInf.MsgKinds)<Byte(ElemsgNoReply) then Inc(Byte(MessageInf.MsgKinds))
                else MessageInf.MsgKinds:= ElemsgBoth;

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  MessageInf.MsgKinds := Template.MsgKinds;
           end;
      23 : begin;
              If NOT GlobalProcessing then
                If (NOT ReadBit(MessageInf.Attribute, 3)) AND (NOT ReadBit(MessageInf.Attribute, 5)) then
                  SetBit(MessageInf.Attribute, 5) { Handles names only }
                    else If ReadBit(MessageInf.Attribute, 5) then begin;
                                               ClearBit(MessageInf.Attribute, 5);
                                               Setbit(MessageInf.Attribute, 3);
                                              end   { Pick an lias }
                                              else begin;
                                                     ClearBit(MessageInf.Attribute, 3);
                                                     ClearBit(MessageInf.Attribute, 5);
                                                   end; { Real names only }

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  begin;
                    If ReadBit(Template.Attribute, 5) then SetBit(MessageInf.Attribute, 5) else
                        ClearBit(MessageInf.Attribute, 5);
                    If ReadBit(Template.Attribute, 3) then SetBit(MessageInf.Attribute, 3) else
                        ClearBit(MessageInf.Attribute, 3);
                  end; { Handles etc }
           end; { Real Names Only, Handles, Pick an alias }
      24 : begin;
              If NOT GlobalProcessing then
                 EditByte(MessageInf.DaysKill, 36, 09, mnuEditColor, True);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  MessageInf.DaysKill := Template.DaysKill;
           end;
      25 : begin;
              If NOT GlobalProcessing then
                 EditByte(MessageInf.RecvKill, 36, 10, mnuEditColor, True);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  MessageInf.RecvKill := Template.RecvKill;
           end;
      26 : begin;
              If NOT GlobalProcessing then
                 EditWord(MessageInf.CountKill, 36, 11, mnuEditColor, True);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  MessageInf.CountKill := Template.CountKill;
           end;
      27 : begin;
              If NOT GlobalProcessing then
                 MessageInf.Attribute := Edit_Bit(MessageInf.Attribute, 0, 36, 12, mnuNormColor);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  If ReadBit(Template.Attribute, 0) then SetBit(MessageInf.Attribute, 0) else
                    ClearBit(MessageInf.Attribute, 0);
           end;
      28 : begin;
              If NOT GlobalProcessing then
                 MessageInf.Attribute := Edit_Bit(MessageInf.Attribute, 1, 36, 13, mnuNormColor);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  If ReadBit(Template.Attribute, 1) then SetBit(MessageInf.Attribute, 1) else
                    ClearBit(MessageInf.Attribute, 1);
           end;
      29 : begin;
              If NOT GlobalProcessing then
                 MessageInf.Attribute := Edit_Bit(MessageInf.Attribute, 2, 36, 14, mnuNormColor);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  If ReadBit(Template.Attribute, 2) then SetBit(MessageInf.Attribute, 2) else
                    ClearBit(MessageInf.Attribute, 2);
           end;
      30 : begin;
              If NOT GlobalProcessing then
                 MessageInf.Attribute := Edit_Bit(MessageInf.Attribute, 4, 36, 15, mnuNormColor);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  If ReadBit(Template.Attribute, 4) then SetBit(MessageInf.Attribute, 4) else
                    ClearBit(MessageInf.Attribute, 4);
           end;
      31 : begin;
              If NOT GlobalProcessing then
                 MessageInf.Attribute := Edit_Bit(MessageInf.Attribute, 6, 36, 16, mnuNormColor);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  If ReadBit(Template.Attribute, 6) then SetBit(MessageInf.Attribute, 6) else
                    ClearBit(MessageInf.Attribute, 6);
           end;
      32 : begin;
              If NOT GlobalProcessing then
                 MessageInf.AkaAddress := EditAka(MessageInf.AkaAddress, Menu);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  MessageInf.AkaAddress := Template.AkaAddress;
           end;
      33 : begin;
              If NOT GlobalProcessing then
                 EditByte(MessageInf.Age, 36, 18, mnuEditColor, True);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  MessageInf.Age := Template.Age;
           end;
      34 : begin;
              If NOT GlobalProcessing then
                 EditWord(MessageInf.Group, 36, 19, mnuEditColor, True);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  MessageInf.Group := Template.Group;
           end;
      35 : begin
              if NOT GlobalProcessing then
                begin
                  EditMsgAreaType(MessageInf,
                                  EleMsgInfo,
                                  ChangeArray,
                                  GlobalProcessing,
                                  DoingEleMsg,
                                  GlobChange);
                end; { if }

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                begin
                  If ReadBit(Template.Attribute, 7) then SetBit(MessageInf.Attribute, 7) else
                    ClearBit(MessageInf.Attribute, 7);

                  If ReadBit(NewTemplate.Attribute, 2) then SetBit(EleMsgInfo.Attribute, 2) else
                    ClearBit(EleMsgInfo.Attribute, 2);
                end; { if }
           end;
      36 : begin;
              If NOT GlobalProcessing then
                 If (ReadBit(MessageInf.Attribute, 7)) OR
                     (ReadBit(EleMsgInfo.Attribute, 2)) then
                   begin
                     GetString(36, 21, mnuEditColor, Messageinf.JamBase, [#32..#254], [#27, #13], 60, 43, False, False, True,
                         false, 0);

                     MakeDirectory(ForceBack(JustPath(MessageInf.Jambase)));

										 { Create the exact string for the mkmsg factory }
                     TempStr := MakeMsgid(MessageInf.AreaNum, MessageInf.JamBase, MessageInf.Attribute, 
                     							EleMsgInfo.Attribute);

                     TempMsgPtr := nil;
                     if OpenOrCreateMsgArea(TempMsgPtr, TempStr) then
                       CloseMsgArea(TempMsgPtr);
                     
                   end; { if }

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                MessageInf.JamBase := Template.Jambase;
           end;
      37 : begin;
              If NOT GlobalProcessing then
                 EleMsgInf.Attribute := Edit_Bit(ElemsgInf.Attribute, 3, 36, 15, mnuNormColor);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  If ReadBit(NewTemplate.Attribute, 3) then SetBit(EleMsgInf.Attribute, 3) else
                    ClearBit(EleMsgInf.Attribute, 3);
           end;
      38 : begin;
              If NOT GlobalProcessing then
                 EditWord(MessageInf.AltGroup[1], 64, 6, mnuEditColor, TRue);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  MessageInf.AltGroup[1] := Template.AltGroup[1];
           end;
      39 : begin;
              If NOT GlobalProcessing then
                 EditWord(MessageInf.AltGroup[2], 64, 7, mnuEditColor, True);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  MessageInf.AltGroup[2] := Template.AltGroup[2];
           end;
      40 : begin;
              If NOT GlobalProcessing then
                 EditWord(MessageInf.AltGroup[3], 64, 8, mnuEditColor, True);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  MessageInf.AltGroup[3] := Template.AltGroup[3];
           end;
      41 : begin;
              If NOT GlobalProcessing then
                 MessageInf.Attribute2 := Edit_Bit(MessageInf.Attribute2, 0, 64, 09, mnuNormColor);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  If ReadBit(Template.Attribute2, 0) then SetBit(MessageInf.Attribute2, 0) else
                    ClearBit(MessageInf.Attribute2, 0);
           end;
      42 : begin;
              If NOT GlobalProcessing then
                 EditWord(MessageInf.NetMailArea, 64, 10, mnuEditColor, True);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  MessageInf.NetMailArea := Template.NetMailArea;
           end;
      43 : begin
              { !!! 43..46 is taken by EditNewsGroup }
              EditNewsGroup;
           end; { if }
   End; { Case }

   EdittingMsgArea := False;
  Until Choice=0;

  If NOT GlobalProcessing then
    If GlobChange then
     For Choice:=1 to Menu.Items do
      begin;
       If Menu.PullInf^[Choice].mnuCurColor=mnuGlobColor then ChangeArray[Choice]:=True
        else ChangeArray[Choice] := False;
      end; { For }

  RemoveMenu(Menu);
  ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));

 {$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFileIO, 'End of EditMsgArea');
 {$ENDIF}
end; { proc. EditMsgArea }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditGroupRecord(var GroupInf: GroupRecord; GlobChange: Boolean;
                          Template: GroupRecord; GlobalProcessing: Boolean;
                          var ChangeArray: GlobalChangeArray);
var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    SaveDirect: Boolean;
begin
  AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'EditGroupRecord');
  InitPullMenu(Menu);

  AddPullItem(Menu.PullInf^[01], 'Name     ', 01, #00, 'Name of this group', 01);
  AddPullItem(Menu.PullInf^[02], '', 02, #00, '', 01);
  AddPullItem(Menu.PullInf^[03], 'Type     ', 03, #00, 'Type of group, could be used in EleWEB', 1);
  AddPullItem(Menu.PullInf^[04], 'Security ', 04, #00, 'Security and flag settings required to select this group', 01);
  AddPullItem(Menu.PullInf^[05], 'A flag   ', 05, #00, 'Security and flag settings required to select this group', 01);
  AddPullItem(Menu.PullInf^[06], 'B flag   ', 06, #00, 'Security and flag settings required to select this group', 01);
  AddPullItem(Menu.PullInf^[07], 'C flag   ', 07, #00, 'Security and flag settings required to select this group', 01);
  AddPullItem(Menu.PullInf^[08], 'D flag   ', 08, #00, 'Security and flag settings required to select this group', 01);

  Menu.PullInf^[02].mnuSelect := False;

  if NOT GroupMessage then
    Menu.PullInf^[03].mnuSelect := false;

  Menu.Items   := 8;
  Menu.Width   := 55;
  Menu.Length  := 8;
  Menu.X       := 12;
  Menu.Y       := 6;
  Menu.HiLight := 1;
  Menu.AddSpace:= True;
  Menu.Title   := ' Group '+MakeLen(FStr(GroupInf.AreaNum), 5, space, false, false)+ ' ';
  Menu.PosArray[1].XInc := 00;
  Menu.PosArray[1].YDec := 00;

  If NOT GlobalProcessing then ShowMenu(Menu, False);

  Choice := 00;

  Repeat;
    SaveDirect := DirectScrnUpdate;
    DirectScrnUpdate := false;
    EdittingGroupRecord := True;

    If (GlobChange) OR (GlobalProcessing) then EdittingGroupRecord := false;                  { Disable F1 }

    If (NOT GlobChange) AND (NOT GlobalProcessing) then
      WriteAT(13, 6, mnuTitleColor, ' (F1) to edit group number ');

    If GlobalProcessing then
     If Choice<Menu.Items then Inc(Choice)
      else Choice:=00;                                    { Terminate session }

    If GlobChange then
      If Choice>00 then Menu.PullInf^[Choice].mnuCurColor := mnuGlobColor;

    If NOT GlobalProcessing then
      begin
        WriteAT(24, 08, mnuNormColor, GroupInf.Name);

        if GroupMessage then
          WriteAT(24, 10, mnuNormColor, GroupType2Str(Byte(GroupInf.Typ)));

        WriteAT(24, 11, mnuNormColor, FStr(GroupInf.Security));
        WriteAT(24, 12, mnuNormColor, Byte2FlagsOff(GroupInf.Flags[1], GroupInf.NotFlagsMask[1]));
        WriteAT(24, 13, mnuNormColor, Byte2FlagsOff(GroupInf.Flags[2], GroupInf.NotFlagsMask[2]));
        WriteAT(24, 14, mnuNormColor, Byte2FlagsOff(GroupInf.Flags[3], GroupInf.NotFlagsMask[3]));
        WriteAT(24, 15, mnuNormColor, Byte2FlagsOff(GroupInf.Flags[4], GroupInf.NotFlagsMask[4]));


        DirectScrnUpdate := SaveDirect;
        Choice := DoPullMenu(Menu, CH, False, False);
      end; { GlobalProcessing }

    Case Choice of
      01 : begin;
              If NOT GlobalProcessing then
                GroupInf.Name := Edit(GroupInf.Name, 24, 8, mnuEditColor, 40, False, False, True)
                 else If DoGlobalChange(Choice) then GroupInf.Name := Template.Name;
           end;
      03 : if GroupMessage then
             begin;
                If NOT GlobalProcessing then
                   If Byte(GroupInf.Typ)<Byte(group_Forum) then Inc(Byte(GroupInf.Typ))
                    else GroupInf.Typ := group_Normal;

                If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                    GroupInf.Typ := Template.Typ;
             end;
      04 : begin;
              If NOT GlobalProcessing then
                 EditWord(GroupInf.Security, 24, 11, mnuEditColor, True)
                   else If DoGlobalChange(Choice) then GroupInf.Security:= Template.Security;
           end;
      05 : begin;
              If NOT GlobalProcessing then
                 EditFlags(GroupInf.Flags[1], GroupInf.NotFlagsMask[1], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        GroupInf.Flags[1]:= Template.Flags[1];
                                                        GroupInf.NotFlagsMask[1]:= Template.NotFlagsMask[1];
                                                       end; { GlobalChange }
           end;
      06 : begin;
              If NOT GlobalProcessing then
                 EditFlags(GroupInf.Flags[2], GroupInf.NotFlagsMask[2], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        GroupInf.Flags[2]:= Template.Flags[2];
                                                        GroupInf.NotFlagsMask[2]:= Template.NotFlagsMask[2];
                                                       end; { GlobalChange }
           end;
      07 : begin;
              If NOT GlobalProcessing then
                 EditFlags(GroupInf.Flags[3], GroupInf.NotFlagsMask[3], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        GroupInf.Flags[3]:= Template.Flags[3];
                                                        GroupInf.NotFlagsMask[3]:= Template.NotFlagsMask[3];
                                                       end; { GlobalChange }
           end;
      08 : begin;
              If NOT GlobalProcessing then
                 EditFlags(GroupInf.Flags[4], GroupInf.NotFlagsMask[4], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        GroupInf.Flags[4]:= Template.Flags[4];
                                                        GroupInf.NotFlagsMask[4]:= Template.NotFlagsMask[4];
                                                       end; { GlobalChange }
           end;

    End; { Case }

    EdittingGroupRecord := False;
  Until (Choice=00);

  If NOT GlobalProcessing then
    If GlobChange then
     For Choice:=1 to Menu.Items do
      begin;
       If Menu.PullInf^[Choice].mnuCurColor=mnuGlobColor then ChangeArray[Choice]:=True
        else ChangeArray[Choice] := False;
      end; { For }

  RemoveMenu(Menu);
  ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. EditGroupRecord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditFileArea(var FilesInf: FilesRecord;
                       var EleFilesInf: EleFilesRecord;
                       GlobChange: Boolean;
                       Template: FilesRecord;
                       NewTemplate: EleFilesRecord;
                       GlobalProcessing: Boolean;
                       var ChangeArray: GlobalChangeArray;
                       DoingEleFILE: Boolean);

function EditExtension(B: Byte; DisMenu: PullRecord): Byte;
var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    Counter   : Byte;
    TempStr   : String;
    TempStr2  : String;
begin
  EnDisAbleMenu(DisMenu, False);

  AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'EditfileArea');
  InitPullMenu(Menu);

  For Counter:=01 to 10 do
    begin;
      Str(Counter:2, TempStr);

      TempStr2 := ' '+MakeLen(GlobalCfg^.RaConfig^.ArcInfo[Counter].Extension, 4, space, false, false);
      If GlobalCfg^.RaConfig^.ArcInfo[Counter].Extension='' then TempStr2:= '';

      AddPullItem(Menu.PullInf^[Counter], TempStr+TempStr2, Counter, #00,
                  'Select an archive format; ENTER to accept, ESCAPE to clear', 01);
    end; { For Counter }

  Menu.Items   := 10;
  Menu.Width   := 14;
  Menu.Length  := 10;
  Menu.X       := 47;
  Menu.Y       := 6;
  Menu.HiLight := B;
  Menu.AddSpace:= True;
  Menu.Title   := ' Archivers ';
  Menu.PosArray[1].XInc := 00;
  Menu.PosArray[1].YDec := 00;

  ShowMenu(Menu, False);

  Choice := DoPullMenu(Menu, CH, False, False);

  If Choice>0 then EditExtension := Choice
     else EditExtension := 00;

  RemoveMenu(Menu);
  ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));

  EnDisAbleMenu(DisMenu, True);
end; { func. EditAka }


procedure EditFtpArea;
var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    SaveDirect: Boolean;
begin
  if GlobalProcessing then
    if NOT DoingEleFILE then EXIT;

  AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'NewsGroupRecord');
  InitPullMenu(Menu);

  AddPullItem(Menu.PullInf^[01], 'Server + path   ', 1, #00, 'Server + path, eg: ftp://ftp.pcmicro.com/', 01);
  AddPullItem(Menu.PullInf^[02], 'Login name      ', 2, #00, 'Username to logon to this server, eg: anonymous', 01);
  AddPullItem(Menu.PullInf^[03], 'Login password  ', 3, #00, 'Password to send when asked for, eg: john.doe@elebbs.bbs', 01);

  Menu.Items   := 3;
  Menu.Width   := 75;
  Menu.Length  := 3;
  Menu.X       := 03;
  Menu.Y       := 6;
  Menu.HiLight := 1;
  Menu.AddSpace:= True;
  Menu.Title   := ' FTP client settings ';
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
        WriteAT(22, 08, mnuNormColor, EleFilesInf.FtpPath);
        WriteAT(22, 09, mnuNormColor, EleFilesInf.ftpLoginName);
        WriteAT(22, 10, mnuNormColor, EleFilesInf.ftpPassword);

        DirectScrnUpdate := SaveDirect;
        Choice := DoPullMenu(Menu, CH, False, False);
      end; { if }

    if GlobalProcessing then
      if Choice < Menu.Items then Inc(Choice)
        else Choice := 00;

    if GlobChange then
     if Choice > 00 then Menu.PullInf^[Choice].mnuCurColor := mnuGlobColor;


    Case Choice of
     1 : begin
            If (NOT GlobalProcessing)  then
             begin
               GetString(22, 08, mnuEditColor, EleFilesInf.ftpPath, [#32..#254], [#13, #27],
                         250, 55, false, false, true, false, 0);

               if rdLastKey = #13 then
                 FilesInf.FilePath := Copy(EleFilesInf.ftpPath, 1, 40);
             end
               else if DoGlobalChange(100 + Choice) then
                      begin
                        EleFilesInf.ftpPath := NewTemplate.ftpPath;
                        FilesInf.FilePath := Copy(EleFilesInf.ftpPath, 1, 40);
                      end; { if}
         end; { if }
     2 : begin
            If (NOT GlobalProcessing)  then
              GetString(22, 09, mnuEditColor, EleFilesInf.ftpLoginName, [#33..#254], [#13, #27],
                        35, 35, false, false, true, false, 0)
               else if DoGlobalChange(100 + Choice) then
                      begin
                        EleFilesInf.ftpLoginName := NewTemplate.ftpLoginName;
                      end; { if }
         end; { if }
     3 : begin
            If (NOT GlobalProcessing)  then
              GetString(22, 10, mnuEditColor, EleFilesInf.ftpPassword, [#33..#254], [#13, #27],
                        35, 35, false, false, true, false, 0)
               else if DoGlobalChange(100 + Choice) then
                      begin
                        EleFilesInf.ftpPassword := NewTemplate.ftpPassword;
                      end; { if }
         end; { if }
    end; { Case }

  until (Choice=00);

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
end; { proc. EditftpArea }


var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    SaveDirect: Boolean;
begin
  AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'EditFileArea');
  InitPullMenu(Menu);

  AddPullItem(Menu.PullInf^[01], 'Name    ', 01, #00, 'Name of this file area', 01);
  AddPullItem(Menu.PullInf^[02], 'Path    ', 02, #00, 'Full path to where the files are stored', 01);
  AddPullItem(Menu.PullInf^[03], 'URL-path', 03, #00, 'URL used to generate an HTML output of this', 01);

  AddPullItem(Menu.PullInf^[05], 'DL security', 05, #00, 'Security and flag settings required to download from this area', 01);
  AddPullItem(Menu.PullInf^[06], 'A flag', 06, #00, 'Security and flag settings required to download from this area', 01);
  AddPullItem(Menu.PullInf^[07], 'B flag', 07, #00, 'Security and flag settings required to download from this area', 01);
  AddPullItem(Menu.PullInf^[08], 'C flag', 08, #00, 'Security and flag settings required to download from this area', 01);
  AddPullItem(Menu.PullInf^[09], 'D flag', 09, #00, 'Security and flag settings required to download from this area', 01);
  AddPullItem(Menu.PullInf^[10], '', 10, #00, '', 01);
  AddPullItem(Menu.PullInf^[11], 'List security', 11, #00, 'Security and flag settings required '+
                                                           'to list files in this area', 01);
  AddPullItem(Menu.PullInf^[12], 'A flag', 12, #00, 'Security and flag settings required to list files in this area', 01);
  AddPullItem(Menu.PullInf^[13], 'B flag', 13, #00, 'Security and flag settings required to list files in this area', 01);
  AddPullItem(Menu.PullInf^[14], 'C flag', 14, #00, 'Security and flag settings required to list files in this area', 01);
  AddPullItem(Menu.PullInf^[15], 'D flag', 15, #00, 'Security and flag settings required to list files in this area', 01);
  AddPullItem(Menu.PullInf^[16], '', 16, #00, '', 01);
  AddPullItem(Menu.PullInf^[17], 'UL security', 17, #00, 'Security and flag settings required to upload '+
                                                         'files into this area', 01);
  AddPullItem(Menu.PullInf^[18], 'A flag', 18, #00, 'Security and flag settings required to upload files into this area', 01);
  AddPullItem(Menu.PullInf^[19], 'B flag', 19, #00, 'Security and flag settings required to upload files into this area', 01);
  AddPullItem(Menu.PullInf^[20], 'C flag', 20, #00, 'Security and flag settings required to upload files into this area', 01);
  AddPullItem(Menu.PullInf^[21], 'D flag', 21, #00, 'Security and flag settings required to upload files into this area', 01);

  AddPullItem(Menu.PullInf^[22], 'New     ', 22, #00, 'Include this area in a new files check?', 02);
  AddPullItem(Menu.PullInf^[23], 'Dupes   ', 23, #00, 'Check this area for upload duplicates?', 02);

  { 24 is CD-ROM or FTP }

  AddPullItem(Menu.PullInf^[25], 'Free    ', 25, #00, '"Yes" means all files in this area are FREE', 02);
  AddPullItem(Menu.PullInf^[26], 'LongDesc', 26, #00, 'Permit users to enter multi-line upload file descriptions?', 02);
  AddPullItem(Menu.PullInf^[27], 'DirectDL', 27, #00, 'Permit users to download files not in the file database?', 02);
  AddPullItem(Menu.PullInf^[28], 'PwdUL   ', 28, #00, 'Permit users to password-protect their uploads to this area?', 02);
  AddPullItem(Menu.PullInf^[29], 'UL scan ', 29, #00, 'Check uploads for corrupt archives and viruses?', 02);
  AddPullItem(Menu.PullInf^[30], 'ArcType ', 30, #00, 'Archive format to convert uploads to', 02);
  AddPullItem(Menu.PullInf^[31], 'DL days ', 31, #00, 'Number of days to keep files since last download', 02);
  AddPullItem(Menu.PullInf^[32], 'FD days ', 32, #00, 'Number of days to keep files based on filedate', 02);
  AddPullItem(Menu.PullInf^[33], 'MoveArea', 33, #00, 'Area to move old files to (0=kill file)', 02);
  AddPullItem(Menu.PullInf^[34], 'Min age ', 34, #00, 'Minimum age required for list', 02);
  AddPullItem(Menu.PullInf^[35], 'Password', 35, #00, 'Password required to download files '+
                                                      '(does not override individual file setting)', 02);
  AddPullItem(Menu.PullInf^[36], 'Group   ', 36, #00, 'Primary group which this area is in', 02);
  AddPullItem(Menu.PullInf^[37], 'Def.cost', 37, #00, 'Default cost for all files', 02);
  AddPullItem(Menu.PullInf^[38], 'Uploads ', 38, #00, 'Alternate upload area', 02);

  AddPullItem(Menu.PullInf^[39], 'AltGroup1', 39, #00, 'Secondary group to which this area belongs', 03);
  AddPullItem(Menu.PullInf^[40], 'AltGroup2', 40, #00, 'Secondary group to which this area belongs', 03);
  AddPullItem(Menu.PullInf^[41], 'AltGroup3', 41, #00, 'Secondary group to which this area belongs', 03);
  AddPullItem(Menu.PullInf^[42], 'Device   ', 42, #00, 'Fixed-media device on which this area resides', 03);
  AddPullItem(Menu.PullInf^[43], 'AllGroups', 43, #00, '"Yes" means this area will appear in all groups', 03);
  AddPullItem(Menu.PullInf^[44], 'FTP      ', 44, #00, 'Set full path to ftp server, login names etc', 03);

  Menu.PullInf^[04].mnuSelect := False;
  Menu.PullInf^[10].mnuSelect := False;
  Menu.PullInf^[16].mnuSelect := False;

  Menu.Items   := 44;
  Menu.Width   := 55;
  Menu.Length  := 20;
  Menu.X       := 12;
  Menu.Y       := 1;
  Menu.HiLight := 1;
  Menu.AddSpace:= True;
  Menu.Title   := ' File area '+MakeLen(FStr(FilesInf.AreaNum), 5, space, false, false)+' ';
  Menu.PosArray[1].XInc := 00;
  Menu.PosArray[1].YDec := 00;
  Menu.PosArray[2].XInc := 20;
  Menu.PosArray[2].YDec := 17;
  Menu.PosArray[3].XInc := 36;
  Menu.PosArray[3].YDec := 34;

  If NOT GlobalProcessing then ShowMenu(Menu, False);

  Choice := 00;

  REPEAT
    if NOT IsFtpUrl(FilesInf.FilePath)  then
      AddPullItem(Menu.PullInf^[24], 'CD-ROM  ', 24, #00, 'Is this area on a CD-ROM drive?', 02)
       else begin
              AddPullItem(Menu.PullInf^[24], 'FTP     ', 24, #00, 'Is this area on a FTP location?', 02);
              SetBit(FilesInf.Attrib, 3);
            end; { if }

    EdittingFileArea := True;
    SaveDirect := DirectScrnUpdate;
    DirectScrnUpdate := false;

    If (GlobChange) OR (GlobalProcessing) then EdittingFileArea := false;                  { Disable F1 }

    If (NOT GlobChange) AND (NOT GlobalProcessing) then
      WriteAT(14, 1, mnuTitleColor, ' (F1) to edit area number ');

    If GlobalProcessing then
     If Choice<Menu.Items then Inc(Choice)
      else Choice:=00;                                    { Terminate session }

    If GlobChange then
      If Choice>00 then Menu.PullInf^[Choice].mnuCurColor := mnuGlobColor;

    If NOT GlobalProcessing then
      begin
          WriteAT(23, 03, mnuNormColor, FilesInf.Name);
          WriteAT(23, 04, mnuNormColor, FilesInf.FilePath);
          WriteAT(23, 05, mnuNormColor, Copy(EleFilesInf.ExportURL, 1, 40));

          WriteAT(26, 07, mnuNormColor, FStr(FilesInf.Security));
          WriteAT(21, 08, mnuNormColor, Byte2FlagsOff(FilesInf.Flags[1], FilesInf.NotFlags[1]));
          WriteAT(21, 09, mnuNormColor, Byte2FlagsOff(FilesInf.Flags[2], FilesInf.NotFlags[2]));
          WriteAT(21, 10, mnuNormColor, Byte2FlagsOff(FilesInf.Flags[3], FilesInf.NotFlags[3]));
          WriteAT(21, 11, mnuNormColor, Byte2FlagsOff(FilesInf.Flags[4], FilesInf.NotFlags[4]));

          WriteAT(28, 13, mnuNormColor, FStr(FilesInf.ListSecurity));
          WriteAT(21, 14, mnuNormColor, Byte2FlagsOff(FilesInf.ListFlags[1], FilesInf.ListNotFlags[1]));
          WriteAT(21, 15, mnuNormColor, Byte2FlagsOff(FilesInf.ListFlags[2], FilesInf.ListNotFlags[2]));
          WriteAT(21, 16, mnuNormColor, Byte2FlagsOff(FilesInf.ListFlags[3], FilesInf.ListNotFlags[3]));
          WriteAT(21, 17, mnuNormColor, Byte2FlagsOff(FilesInf.ListFlags[4], FilesInf.ListNotFlags[4]));

          WriteAT(26, 19, mnuNormColor, FStr(FilesInf.UploadSecurity));
          WriteAT(21, 20, mnuNormColor, Byte2FlagsOff(FilesInf.UploadFlags[1], FilesInf.UploadNotFlags[1]));
          WriteAT(21, 21, mnuNormColor, Byte2FlagsOff(FilesInf.UploadFlags[2], FilesInf.UploadNotFlags[2]));
          WriteAT(21, 22, mnuNormColor, Byte2FlagsOff(FilesInf.UploadFlags[3], FilesInf.UploadNotFlags[3]));
          WriteAT(21, 23, mnuNormColor, Byte2FlagsOff(FilesInf.UploadFlags[4], FilesInf.UploadNotFlags[4]));

          WriteAT(43, 07, mnuNormColor, Bit2Str(FilesInf.Attrib, 0));
          WriteAT(43, 08, mnuNormColor, Bit2Str(FilesInf.Attrib, 1));
          WriteAT(43, 09, mnuNormColor, Bit2Str(FilesInf.Attrib, 3));
          WriteAT(43, 10, mnuNormColor, Bit2Str(FilesInf.Attrib, 4));
          WriteAT(43, 11, mnuNormColor, Bit2Str(FilesInf.Attrib, 2));
          WriteAT(43, 12, mnuNormColor, Bit2Str(FilesInf.Attrib, 5));
          WriteAT(43, 13, mnuNormColor, Bit2Str(FilesInf.Attrib, 6));
          WriteAT(43, 14, mnuNormColor, Bit2Str(FilesInf.Attrib, 7));

          If (FilesInf.ConvertExt>0) AND (FilesInf.ConvertExt < ((High(GlobalCfg^.RaConfig^.ArcInfo)+01))) then
            WriteAT(43, 15, mnuNormColor, MakeLen(GlobalCfg^.RaConfig^.ArcInfo[FilesInf.ConvertExt].Extension, 3,
                                                  space, false, false))
             else WriteAT(43, 15, mnuNormColor, MakeLen('', 4, space, false, false));

          WriteAT(43, 16, mnuNormColor, FStr(FilesInf.KillDaysDL));
          WriteAT(43, 17, mnuNormColor, FStr(FilesInf.KillDaysFD));
          WriteAT(43, 18, mnuNormColor, FStr(FilesInf.MoveArea));
          WriteAT(43, 19, mnuNormColor, FStr(FilesInf.Age));
          WriteAT(43, 20, mnuNormColor, FilesInf.PassWord);
          WriteAT(43, 21, mnuNormColor, FStr(FilesInf.Group));
          WriteAT(43, 22, mnuNormColor, FStr(FilesInf.DefCost));
          WriteAT(43, 23, mnuNormColor, FStr(FilesInf.UploadArea));

          WriteAT(60, 07, mnuNormColor, FStr(FilesInf.AltGroup[1]));
          WriteAT(60, 08, mnuNormColor, FStr(FilesInf.AltGroup[2]));
          WriteAT(60, 09, mnuNormColor, FStr(FilesInf.AltGroup[3]));
          WriteAT(60, 10, mnuNormColor, FStr(FilesInf.Device));
          WriteAT(60, 11, mnuNormColor, Bit2Str(FilesInf.Attrib2, 0));

          DirectScrnUpdate := SaveDirect;
          Choice := DoPullMenu(Menu, CH, False, False);
      end; { GlobalProcessing }

    Case Choice of
      01 : begin
              If NOT GlobalProcessing then
                FilesInf.Name := Edit(FilesInf.Name, 23, 3, mnuEditColor, 40, False, False, True)
                 else If DoGlobalChange(Choice) then FilesInf.Name := Template.Name;
           end;
      02 : begin
              If NOT GlobalProcessing then
                EditDirectory(23, 4, 40, FilesInf.FilePath)
                 else If DoGlobalChange(Choice) then FilesInf.FilePath := Template.FilePath;
           end;
      03 : begin
              If NOT GlobalProcessing then
               GetString(23, 05, mnuEditColor, EleFilesInf.ExportURL, [#33..#254], [#13, #27],
                         250, 40, false, false, true, false, 0)
                 else If DoGlobalChange(Choice) then
                        if DoingEleFILE then
                          EleFilesInf.ExportURL := NewTemplate.ExportURL;
           end;
      05 : begin;
              If NOT GlobalProcessing then
                 EditWord(FilesInf.Security, 26, 07, mnuEditColor, True)
                   else If DoGlobalChange(Choice) then FilesInf.Security:= Template.Security;
           end;
      06 : begin;
              If NOT GlobalProcessing then
                 EditFlags(FilesInf.Flags[1], FilesInf.NotFlags[1], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        FilesInf.Flags[1]:= Template.Flags[1];
                                                        FilesInf.NotFlags[1]:= Template.NotFlags[1];
                                                       end; { GlobalChange }
           end;
      07 : begin;
              If NOT GlobalProcessing then
                 EditFlags(FilesInf.Flags[2], FilesInf.NotFlags[2], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        FilesInf.Flags[2]:= Template.Flags[2];
                                                        FilesInf.NotFlags[2]:= Template.NotFlags[2];
                                                       end; { GlobalChange }
           end;
      08 : begin;
              If NOT GlobalProcessing then
                 EditFlags(FilesInf.Flags[3], FilesInf.NotFlags[3], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        FilesInf.Flags[3]:= Template.Flags[3];
                                                        FilesInf.NotFlags[3]:= Template.NotFlags[3];
                                                       end; { GlobalChange }
           end;
      09 : begin;
              If NOT GlobalProcessing then
                 EditFlags(FilesInf.Flags[4], FilesInf.NotFlags[4], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        FilesInf.Flags[4]:= Template.Flags[4];
                                                        FilesInf.NotFlags[4]:= Template.NotFlags[4];
                                                       end; { GlobalChange }
           end;
      10 : { gap } ;
      11 : begin;
              If NOT GlobalProcessing then
                 EditWord(FilesInf.ListSecurity, 28, 13, mnuEditColor, True)
                   else If DoGlobalChange(Choice) then FilesInf.ListSecurity:= Template.ListSecurity;
           end;
      12 : begin;
              If NOT GlobalProcessing then
                 EditFlags(FilesInf.ListFlags[1], FilesInf.ListNotFlags[1], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        FilesInf.ListFlags[1]:= Template.ListFlags[1];
                                                        FilesInf.ListNotFlags[1]:= Template.ListNotFlags[1];
                                                       end; { GlobalChange }
           end;
      13 : begin;
              If NOT GlobalProcessing then
                 EditFlags(FilesInf.ListFlags[2], FilesInf.ListNotFlags[2], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        FilesInf.ListFlags[2]:= Template.ListFlags[2];
                                                        FilesInf.ListNotFlags[2]:= Template.ListNotFlags[2];
                                                       end; { GlobalChange }
           end;
      14 : begin;
              If NOT GlobalProcessing then
                 EditFlags(FilesInf.ListFlags[3], FilesInf.ListNotFlags[3], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        FilesInf.ListFlags[3]:= Template.ListFlags[3];
                                                        FilesInf.ListNotFlags[3]:= Template.ListNotFlags[3];
                                                       end; { GlobalChange }
           end;
      15 : begin;
              If NOT GlobalProcessing then
                 EditFlags(FilesInf.ListFlags[4], FilesInf.ListNotFlags[4], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        FilesInf.ListFlags[4]:= Template.ListFlags[4];
                                                        FilesInf.ListNotFlags[4]:= Template.ListNotFlags[4];
                                                       end; { GlobalChange }
           end;
      16 : { gap } ;
      17 : begin;
              If NOT GlobalProcessing then
                 EditWord(FilesInf.UploadSecurity, 26, 19, mnuEditColor, True)
                   else If DoGlobalChange(Choice) then FilesInf.UploadSecurity:= Template.UploadSecurity;
           end;
      18 : begin;
              If NOT GlobalProcessing then
                 EditFlags(FilesInf.UploadFlags[1], FilesInf.UploadNotFlags[1], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        FilesInf.UploadFlags[1]:= Template.UploadFlags[1];
                                                        FilesInf.UploadNotFlags[1]:= Template.UploadNotFlags[1];
                                                       end; { GlobalChange }
           end;
      19 : begin;
              If NOT GlobalProcessing then
                 EditFlags(FilesInf.UploadFlags[2], FilesInf.UploadNotFlags[2], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        FilesInf.UploadFlags[2]:= Template.UploadFlags[2];
                                                        FilesInf.UploadNotFlags[2]:= Template.UploadNotFlags[2];
                                                       end; { GlobalChange }
           end;
      20 : begin;
              If NOT GlobalProcessing then
                 EditFlags(FilesInf.UploadFlags[3], FilesInf.UploadNotFlags[3], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        FilesInf.UploadFlags[3]:= Template.UploadFlags[3];
                                                        FilesInf.UploadNotFlags[3]:= Template.UploadNotFlags[3];
                                                       end; { GlobalChange }
           end;
      21 : begin;
              If NOT GlobalProcessing then
                 EditFlags(FilesInf.UploadFlags[4], FilesInf.UploadNotFlags[4], True, 60, 03, Menu)
                   else If DoGlobalChange(Choice) then begin;
                                                        FilesInf.UploadFlags[4]:= Template.UploadFlags[4];
                                                        FilesInf.UploadNotFlags[4]:= Template.UploadNotFlags[4];
                                                       end; { GlobalChange }
           end;
      22 : begin;
              If NOT GlobalProcessing then
                 FilesInf.Attrib := Edit_Bit(FilesInf.Attrib, 0, 43, 07, mnuNormColor);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  If ReadBit(Template.Attrib, 0) then SetBit(FilesInf.Attrib, 0) else
                    ClearBit(FilesInf.Attrib, 0);
           end;
      23 : begin;
              If NOT GlobalProcessing then
                 FilesInf.Attrib := Edit_Bit(FilesInf.Attrib, 1, 43, 08, mnuNormColor);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  If ReadBit(Template.Attrib, 1) then SetBit(FilesInf.Attrib, 1) else
                    ClearBit(FilesInf.Attrib, 1);
           end;
      24 : begin;
              If NOT GlobalProcessing then
                 FilesInf.Attrib := Edit_Bit(FilesInf.Attrib, 3, 43, 09, mnuNormColor);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  If ReadBit(Template.Attrib, 3) then SetBit(FilesInf.Attrib, 3) else
                    ClearBit(FilesInf.Attrib, 3);
           end;
      25 : begin;
              If NOT GlobalProcessing then
                 FilesInf.Attrib := Edit_Bit(FilesInf.Attrib, 4, 43, 10, mnuNormColor);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  If ReadBit(Template.Attrib, 4) then SetBit(FilesInf.Attrib, 4) else
                    ClearBit(FilesInf.Attrib, 4);
           end;
      26 : begin;
              If NOT GlobalProcessing then
                 FilesInf.Attrib := Edit_Bit(FilesInf.Attrib, 2, 43, 11, mnuNormColor);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  If ReadBit(Template.Attrib, 2) then SetBit(FilesInf.Attrib, 2) else
                    ClearBit(FilesInf.Attrib, 2);
           end;
      27 : begin;
              If NOT GlobalProcessing then
                 FilesInf.Attrib := Edit_Bit(FilesInf.Attrib, 5, 43, 12, mnuNormColor);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  If ReadBit(Template.Attrib, 5) then SetBit(FilesInf.Attrib, 5) else
                    ClearBit(FilesInf.Attrib, 5);
           end;
      28 : begin;
              If NOT GlobalProcessing then
                 FilesInf.Attrib := Edit_Bit(FilesInf.Attrib, 6, 43, 13, mnuNormColor);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  If ReadBit(Template.Attrib, 6) then SetBit(FilesInf.Attrib, 6) else
                    ClearBit(FilesInf.Attrib, 6);
           end;
      29 : begin;
              If NOT GlobalProcessing then
                 FilesInf.Attrib := Edit_Bit(FilesInf.Attrib, 7, 43, 14, mnuNormColor);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  If ReadBit(Template.Attrib, 7) then SetBit(FilesInf.Attrib, 7) else
                    ClearBit(FilesInf.Attrib, 7);
           end;
      30 : begin;
              If NOT GlobalProcessing then
                  FilesInf.ConvertExt := EditExtension(FilesInf.ConvertExt, Menu);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  FilesInf.ConvertExt:= Template.ConvertExt;
           end;
      31 : begin;
              If NOT GlobalProcessing then
                 EditWord(FilesInf.KillDaysDl, 43, 16, mnuEditColor, True);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  FilesInf.KillDaysDL := Template.KillDaysDL;
           end;
      32 : begin;
              If NOT GlobalProcessing then
                 EditWord(FilesInf.KillDaysFD, 43, 17, mnuEditColor, True);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  FilesInf.KillDaysFD := Template.KillDaysFD;
           end;
      33 : begin;
              If NOT GlobalProcessing then
                 EditWord(FilesInf.MoveArea, 43, 18, mnuEditColor, True);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  FilesInf.MoveArea := Template.MoveArea;
           end;
      34 : begin;
              If NOT GlobalProcessing then
                 EditByte(FilesInf.Age, 43, 19, mnuEditColor, True);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  FilesInf.Age := Template.Age;
           end;
      35 : begin;
              If NOT GlobalProcessing then
                 FilesInf.PassWord := Edit(FilesInf.PassWord, 43, 20, mnuEditColor, 15, False, False, True);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  FilesInf.Password := Template.Password;
           end;
      36 : begin;
              If NOT GlobalProcessing then
                 EditWord(FilesInf.Group, 43, 21, mnuEditColor, True);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  FilesInf.Group := Template.Group;
           end;
      37 : begin;
              If NOT GlobalProcessing then
                 EditWord(FilesInf.DefCost, 43, 22, mnuEditColor, True);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  FilesInf.DefCost := Template.DefCost;
           end;
      38 : begin;
              If NOT GlobalProcessing then
                 EditWord(FilesInf.UploadArea, 43, 23, mnuEditColor, True);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  FilesInf.UploadArea := Template.UploadArea;
           end;
      39 : begin;
              If NOT GlobalProcessing then
                 EditWord(FilesInf.AltGroup[1], 60, 7, mnuEditColor, True);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  FilesInf.AltGroup[1] := Template.AltGroup[1];
           end;
      40 : begin;
              If NOT GlobalProcessing then
                 EditWord(FilesInf.AltGroup[2], 60, 8, mnuEditColor, True);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  FilesInf.AltGroup[2] := Template.AltGroup[2];
           end;
      41 : begin;
              If NOT GlobalProcessing then
                 EditWord(FilesInf.AltGroup[3], 60, 9, mnuEditColor, True);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  FilesInf.AltGroup[3] := Template.AltGroup[3];
           end;
      42 : begin;
              If NOT GlobalProcessing then
                 EditByte(FilesInf.Device, 60, 10, mnuEditColor, True);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  FilesInf.Device:= Template.Device;
           end;
      43 : begin;
              If NOT GlobalProcessing then
                 FilesInf.Attrib2 := Edit_Bit(FilesInf.Attrib2, 0, 60, 11, mnuNormColor);

              If (GlobalProcessing) AND (DoGlobalChange(Choice)) then
                  If ReadBit(Template.Attrib2, 0) then SetBit(FilesInf.Attrib2, 0) else
                    ClearBit(FilesInf.Attrib2, 0);
           end;
      44 : begin
             EditFtpArea;
           end; { EditFtpArea }
    End; { Case }

    EdittingFileArea := False;
  Until (Choice=00);

  If NOT GlobalProcessing then
    If GlobChange then
     For Choice:=1 to Menu.Items do
      begin;
       If Menu.PullInf^[Choice].mnuCurColor=mnuGlobColor then ChangeArray[Choice]:=True
        else ChangeArray[Choice] := False;
      end; { For }

  RemoveMenu(Menu);
  ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. EditFileArea }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoEditAreaNr(X, Y: Byte; DupFile: String; var AreaNum: SmallWord);
var OldNr   : Word;
    SaveScrn: Pointer;
    TempBool: Boolean;
    TmpPath : String;
    MsgInf  : MessageRecord;
begin
  OldNr := AreaNum;
  EdittingAreaNr := TRUE;

  EditWord(AreaNum, X, Y, mnuTitleColor, True);
  WriteAT(X, Y, mnuTitleColor, MakeLen(FStr(AreaNum), 5, space, false, false));

  if (OldNr <> AreaNum) then
   if (NOT (Inrange(AreaNum, 1, 200))) then
    if DupFile = RdxName(MessagesFileName) then
      begin
        MsgInf := MessageInf^;

        if NOT ReadBit(MsgInf.Attribute, 7) then     { jam }
         if NOT ReadBit(EleMsgInf.Attribute, 2) then { squish }
         begin
           SaveScreen(SaveScrn);

           ShadFillBox(10, 10, 70, 14, mnuErrorColor, mnuStyle, True);
           WriteAT(14, 12, mnuErrorColor, 'ERROR: HUDSON only allows areanumbers from 1 till 200');
           UpdateScreenBuffer(true);
           Delay(2000);
           GetInput;

           RestoreScreen(SaveScrn);
           AreaNum := OldNr;
         end; { invalid areanumber }

      end; { if }

  If (OldNr<>AreaNum) AND (RdxDuplicate(DupFile, AreaNum)) AND
   (Pos('-N', ParamString) = 00) AND (Pos('/N', ParamString)=00) then
    begin;
      SaveScreen(SaveScrn);

      ShadFillBox(10, 10, 70, 14, mnuErrorColor, mnuStyle, True);
      WriteAT(19, 12, mnuErrorColor, 'ERROR: Area/Group number is already in use!');
      UpdateScreenBuffer(true);
      Delay(2000);
      GetInput;

      RestoreScreen(SaveScrn);

      AreaNum := OldNr;
    end { Duplicate message }
      else
        if (OldNr <> AreaNum) then
         if (SUpCase(DupFile) = SUpCase(RdxName(FilesFileName))) then
           begin
             TmpPath := GlobalCfg^.RaConfig^.FileBasePath;
             if (FileExist(TmpPath + 'hdr'+BsChar+'fdb'+FStr(AreaNum)+'.hdr')) OR
                 (FileExist(TmpPath + 'idx'+BsChar+'fdb'+FStr(AreaNum)+'.idx')) OR
                  (FileExist(TmpPath + 'txt'+BsChar+'fdb'+FStr(AreaNum)+'.txt')) then
                   begin
                     SaveScreen(SaveScrn);
                     ShadFillBox(10, 10, 70, 14, mnuErrorColor, mnuStyle, True);
                     WriteAT(12, 12, mnuErrorColor, 'ERROR: One or more of destination files exist, aborted');
                     UpdateScreenBuffer(true);
                     Delay(2000);
                     RestoreScreen(SaveScrn);

                     AreaNum := OldNr;
                     WriteAT(X, Y, mnuTitleColor, MakeLen(FStr(AreaNum), 5, space, false, false));
                     EdittingAreaNr := FALSE;
                     EXIT;
                   end; { if }

             TempBool := RenameFile(GlobalCfg^.RaConfig^.FileBasePath + 'hdr'+BsChar+'fdb'+FStr(OldNr) + '.hdr',
                         GlobalCfg^.RaConfig^.FileBasePath + 'hdr'+BsChar+'fdb'+FStr(AreaNum) + '.hdr');

             if TempBool then
             TempBool := RenameFile(GlobalCfg^.RaConfig^.FileBasePath + 'txt'+BsChar+'fdb'+FStr(OldNr) + '.txt',
                         GlobalCfg^.RaConfig^.FileBasePath + 'txt'+BsChar+'fdb'+FStr(AreaNum) + '.txt');

             if TempBool then
             TempBool := RenameFile(GlobalCfg^.RaConfig^.FileBasePath + 'idx'+BsChar+'fdb'+FStr(OldNr) + '.idx',
                         GlobalCfg^.RaConfig^.FileBasePath + 'idx'+BsChar+'fdb'+FStr(AreaNum) + '.idx');

             if NOT TempBool then
              begin;
                 SaveScreen(SaveScrn);
                 ShadFillBox(10, 10, 70, 14, mnuErrorColor, mnuStyle, True);
                 WriteAT(19, 12, mnuErrorColor, 'ERROR: Unable to relocate file database files!');
                 UpdateScreenBuffer(true);
                 Delay(2000);
                 RestoreScreen(SaveScrn);
              end; { if }
           end; { if }

  WriteAT(X, Y, mnuTitleColor, MakeLen(FStr(AreaNum), 5, space, false, false));
  EdittingAreaNr := false;
end; { proc. DoEditAreaNr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure MsgArea_GetInfo(var Info1, Info2, Info3, Info4, Info5, Info6, Info7, Info8, Info9: List_StringType;
                          ItemNr: LongInt; HiLight: LongInt);
var MsgInf: MessageRecord;
begin
  if ItemNr <> MsgArea_GetItems then
   begin
     MsgArea_F^.Seek(Pred(ItemNr) * SizeOf(MessageRecord));
     MsgArea_F^.BlkRead(MsgInf, SizeOf(MessageRecord));

     Info1 := MsgInf.Name;
     Case MsgType(MsgInf.Typ) of
        LocalMail : Info2 := 'Local';
        NetMail   : Info2 := 'Net  ';
        EchoMail  : Info2 := 'Echo ';
        InterNet  : Info2 := 'IMail';
        NewsGroup : Info2 := 'News ';
        ForumGroup: Info2 := 'Forum';
     end; { Case }

    Info3 := FStr(MsgInf.AreaNum);
    If Info1='' then Info1 := '[Unused]';
   end
     else begin
            Info1 := '';
            Info2 := '';
            Info3 := '';
          end; { else }

  Info4 := '';
  Info5 := '';
  Info6 := '';
  Info7 := '';
  Info8 := '';
  Info9 := '';
end; { proc. MsgArea_GetInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function DoGlobalChange(Choice: Word): Boolean;
begin
  DoGlobalChange := MainChangeArray[Choice];
end; { func. DoGlobalChange }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function MsgArea_Seek(Str: String): LongInt;
var MessageInf: MessageRecord;
begin
  MsgArea_F^.Seek(0);

  MsgArea_Seek := -1;

  While NOT MsgArea_F^.EOF do
   begin
     MsgArea_F^.BlkRead(MessageInf, SizeOf(MessageRecord));

     if MessageInf.Name = '' then MessageInf.Name := '[Unused]';

     If SUpcase(Str) = SupCase(Copy(MessageInf.Name, 1, Length(Str))) then
       begin
         Msgarea_Seek := MsgArea_F^.FilePos DIV SizeOf(MessageRecord);
         BREAK;
       end; { if }

   end; { While NOT eof }

end; { func. MsgArea_Seek }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function General_AbortCheck(CH: Char): Boolean;
begin
  If Ch=#27 then General_AbortCheck := True
   else General_AbortCheck := False;
end; { func. General_AbortCheck }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Copy_AbortCheck(CH: Char): Boolean;
begin
  If (Ch in [#13, #27]) then Copy_AbortCheck := True
   else Copy_AbortCheck := False;

  If CH=#27 then CopyPoint := -1;
end; { func. Copy_AbortCheck }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure Protocol_GetInfo(var Info1, Info2, Info3, Info4, Info5, Info6, Info7, Info8, Info9: List_StringType;
                           ItemNr: LongInt; HiLight: LongInt);
var ProtInf: ProtocolRecord;
begin
  Protocol_F^.Seek(Pred(ItemNr) * SizeOf(ProtocolRecord));
  Protocol_F^.BlkRead(ProtInf, SizeOf(ProtocolRecord));

  Info1 := ProtInf.Name;
  Info2 := FStr(ItemNr);
  Info3 := '';
  Info4 := '';
  Info5 := '';
  Info6 := '';
  Info7 := '';
  Info8 := '';
  Info9 := '';

  If Info1='' then Info1 := '[Unused]';
end; { proc. Protocol_GetInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure EditProtocol(var ProtInf: ProtocolRecord; Itemnr: String);
Var Menu      : PullRecord;
    Choice    : Word;
    CH        : Char;
    Savedirect: Boolean;
begin;
 AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'EditProtocol');
 InitPullMenu(Menu);
 AddPullItem(Menu.PullInf^[01], 'Name           ', 2701, #00, 'Name of this protocol', 1);
 AddPullItem(Menu.PullInf^[02], 'Key            ', 2702, #00, 'Hotkey to activate from protocol menu', 1);
 AddPullItem(Menu.PullInf^[03], 'Ext ctl file   ', 2703, #00, 'Does the protocol support the standard protocol '+
                                                              'interface file?', 1);
 AddPullItem(Menu.PullInf^[04], 'Batch          ', 2704, #00, 'Does the protocol support batch transfers?', 1);
 AddPullItem(Menu.PullInf^[05], 'Status         ', 2705, #00, 'Available, not available, available on error-free '+
                                                              'connects only', 1);
 AddPullItem(Menu.PullInf^[06], 'Log file       ', 2706, #00, 'Full path and name of the log file this protocol writes', 1);
 AddPullItem(Menu.PullInf^[07], 'Control file   ', 2707, #00, 'Full path and name of the control file this protocol reads', 1);
 AddPullItem(Menu.PullInf^[08], 'DL command line', 2708, #00, 'DOS command-line to activate protocol in download mode', 1);
 AddPullItem(Menu.PullInf^[09], 'UL command line', 2709, #00, 'DOS command-line to activate protocol in upload mode', 1);
 AddPullItem(Menu.PullInf^[10], 'DL ctl string  ', 2710, #00, 'Format (in the control file) of a download filename entry', 1);
 AddPullItem(Menu.PullInf^[11], 'UL ctl string  ', 2711, #00, 'Format (in the control file) of an upload filename entry', 1);
 AddPullItem(Menu.PullInf^[12], 'DL log keyword ', 2712, #00, 'Keyword to look for in the log file which indicates a file '+
                                                              'download', 1);
 AddPullItem(Menu.PullInf^[13], 'UL log keyword ', 2713, #00, 'Keyword to look for in the log file which indicates a file '+
                                                              'upload', 1);
 AddPullItem(Menu.PullInf^[14], 'Log name word  ', 2714, #00, 'Offset (word number) of the filename, from the UL/DL '+
                                                              'keyword', 1);
 AddPullItem(Menu.PullInf^[15], 'Log desc word  ', 2715, #00, 'Offset (word number) of the description, from the UL/DL'+
                                                              ' keyword', 1);

 Menu.Items   := 15;
 Menu.Width   := 77;
 Menu.Length  := 15;
 Menu.X       := 2;
 Menu.Y       := 4;
 Menu.HiLight := 01;
 Menu.AddSpace:= True;
 Menu.Title   := ' External protocol '+ItemNr+' ';
 Menu.PosArray[1].XInc := 00;
 Menu.PosArray[1].YDec := 00;

 ShowMenu(Menu, False);

 Repeat;
  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;
  WriteAT(20, 06, mnuNormColor, ProtInf.Name);
  WriteAT(20, 07, mnuNormColor, ProtInf.ActiveKey);
  WriteAT(20, 08, mnuNormColor, MakeLen(Bool2Str(ProtInf.OpusTypeCtlFile), 3, space, false, false));
  WriteAT(20, 09, mnuNormColor, MakeLen(Bool2Str(ProtInf.BatchAvailable), 3, space, false, false));
  WriteAT(20, 10, mnuNormColor, ProtAvail2Str(ProtInf.Attribute, True));
  WriteAT(20, 11, mnuNormColor, Copy(ProtInf.LogFileName, 1, 58));
  WriteAT(20, 12, mnuNormColor, Copy(ProtInf.CtlFileName, 1, 58));
  WriteAT(20, 13, mnuNormColor, Copy(ProtInf.DnCmdString, 1, 58));
  WriteAT(20, 14, mnuNormColor, Copy(ProtInf.UpCmdString, 1, 58));
  WriteAT(20, 15, mnuNormColor, Copy(ProtInf.DnCtlString, 1, 58));
  WriteAT(20, 16, mnuNormColor, Copy(ProtInf.UpCtlString, 1, 58));
  WriteAT(20, 17, mnuNormColor, ProtInf.DnLogKeyword);
  WriteAT(20, 18, mnuNormColor, ProtInf.UpLogKeyword);
  WriteAT(20, 19, mnuNormColor, FStr(ProtInf.XFerNameWordNum));
  WriteAT(20, 20, mnuNormColor, FStr(ProtInf.XFerDescWordNum));
  DirectScrnUpdate := SaveDirect;

  Choice := DoPullMenu(Menu, CH, False, False);

  Case Choice of
      2701 : ProtInf.Name := Edit(ProtInf.Name, 20, 06, mnuEditColor, 15, False, False, True);
      2702 : ProtInf.ActiveKey := EditKey(ProtInf.ActiveKey, 20, 07, mnuEditColor, True);
      2703 : Edit_Boolean(ProtInf.OpusTypeCtlFile, 20, 08, mnuEditColor);
      2704 : Edit_Boolean(ProtInf.BatchAvailable, 20, 09, mnuEditColor);
      2705 : begin;
                Inc(ProtInf.Attribute);
                If ProtInf.Attribute>2 then ProtInf.Attribute := 00;
             end; { BatchAvailable }
      2706 : GetString(20, 11, mnuEditColor, ProtInf.LogFileName,[#32..#255], [#13, #27], 80, 58, false, False, True, false,0);
      2707 : Getstring(20, 12, mnuEditColor, ProtInf.CtlFileName,[#32..#255], [#13, #27], 80, 58, False, False, True, false,0);
      2708 : Getstring(20, 13, mnuEditColor, ProtInf.DnCmdString,[#32..#255], [#13, #27], 80, 58, False, False, True, false,0);
      2709 : Getstring(20, 14, mnuEditColor, ProtInf.UpCmdString,[#32..#255], [#13, #27], 80, 58, False, False, True, false,0);
      2710 : Getstring(20, 15, mnuEditColor, ProtInf.DnCtlString,[#32..#255], [#13, #27], 80, 58, False, False, True, false,0);
      2711 : Getstring(20, 16, mnuEditColor, ProtInf.UpCtlString,[#32..#255], [#13, #27], 80, 58, False, False, True, false,0);
      2712 : ProtInf.DnLogKeyWord:= Edit(ProtInf.DnLogKeyWord, 20, 17, mnuEditColor, 20, False, False, True);
      2713 : ProtInf.UpLogKeyWord:= Edit(ProtInf.UpLogKeyword, 20, 18, mnuEditColor, 20, False, False, True);
      2714 : EditByte(ProtInf.xFerNameWordNum, 20, 19, mnuEditColor, True);
      2715 : EditByte(ProtInf.xFerDescWordNum, 20, 20, mnuEditColor, True);
  End; { Case }

 Until Choice=0;

 RemoveMenu(Menu);
 ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. EditProtocol }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Protocol_Activate(HiLight: LongInt;  HiBarPos: LongInt): Longint;
var OldProtInf: ProtocolRecord;
begin
  Protocol_Activate := -1;

  Protocol_F^.Seek((HiLight - 1) * SizeOf(ProtocolRecord));
  Protocol_F^.BlkRead(ProtocolInf, SizeOf(ProtocolRecord));

  OldProtInf:= ProtocolInf;

  EditProtocol(ProtocolInf, FStr(HiLight));

  If RecordsDifferent(ProtocolInf, OldProtInf, SizeOf(ProtocolRecord)) then
        If DoSaveChanges('Save changes (Y/n) ? °', True, false) then
              begin
                Protocol_F^.Seek((HiLight - 1) * SizeOf(ProtocolRecord));
                Protocol_F^.BlkWrite(ProtocolInf, SizeOf(ProtocolRecord));
              end { SaveChanges }
               else ProtocolInf:= OldProtInf;

end; { proc. Protocol_Activate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Protocol_Seek(Str: String): LongInt;
var ProtocolInf: ProtocolRecord;
begin
  Protocol_F^.Seek(0);

  Protocol_Seek := -1;

  While NOT Protocol_F^.EOF do
   begin
     Protocol_F^.BlkRead(ProtocolInf, SizeOf(ProtocolRecord));

     If SUpcase(Str) = SupCase(Copy(ProtocolInf.Name, 1, Length(Str))) then
       begin
         Protocol_Seek := Protocol_F^.FilePOS DIV SizeOf(ProtocolRecord);
         BREAK;
       end; { Found one }

   end; { While NOT eof }

end; { func. Protocol_Seek }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Protocol_GetItems: LongInt;
begin
  Protocol_GetItems := Min(Protocol_F^.FileSize div SizeOf(ProtocolRecord), 15);
end; { func. Protocol_GetItems }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Language_GetItems: LongInt;
begin
  Language_GetItems := Language_F^.FileSize DIV SizeOf(LanguageRecord);
end; { func. Language_GetItems }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure Language_GetInfo(var Info1, Info2, Info3, Info4, Info5, Info6, Info7, Info8, Info9: List_StringType;
                           ItemNr: LongInt; HiLight: LongInt);
var LangInf: LanguageRecord;
    Temp   : Boolean;
begin
  Language_F^.Seek(Pred(ItemNr) * SizeOf(LanguageRecord));
  Language_F^.BlkRead(LangInf, SizeOf(LanguageRecord));

  If ItemNr=HiLight then
    begin
      EditLanguage(LangInf, HiLight, False, Temp);
    end; { if }

  Info1 := LangInf.Name;
{!  Info2 := FStr(ItemNr); }
  Info3 := '';
  Info4 := '';
  Info5 := '';
  Info6 := '';
  Info7 := '';
  Info8 := '';
  Info9 := '';

  If Info1='' then Info1 := ''; { [Unused] }
end; { proc. Language_GetInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure Language_NoEditGetInfo(var Info1, Info2, Info3, Info4, Info5, Info6, Info7, Info8, Info9: List_StringType;
                           ItemNr: LongInt; HiLight: LongInt);
var LangInf: LanguageRecord;
begin
  Language_F^.Seek(Pred(ItemNr) * SizeOf(LanguageRecord));
  Language_F^.BlkRead(LangInf, SizeOf(LanguageRecord));

  Info1 := LangInf.Name;
{   Info2 := FStr(ItemNr); }
  Info3 := '';
  Info4 := '';
  Info5 := '';
  Info6 := '';
  Info7 := '';
  Info8 := '';
  Info9 := '';

  If Info1='' then Info1 := '[Default path]'; { [Unused] }
end; { proc. Language_NoEditGetInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Language_Seek(Str: String): LongInt;
var LanguageInf: ProtocolRecord;
begin
  Language_F^.Seek(0);

  Language_Seek := -1;

  While NOT Language_F^.EOF do
   begin
     Language_F^.Blkread(LanguageInf, SizeOf(LanguageRecord));

     if SUpcase(Str) = SupCase(Copy(LanguageInf.Name, 1, Length(Str))) then
       begin
         Language_Seek := Language_F^.FilePOS div SizeOf(LanguageRecord);
         BREAK;
       end; { if }
   end; { While NOT eof }

end; { func. Language_Seek }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure EditLanguage(var LangInf: LanguageRecord; HiLight: LongInt; DoEdit: Boolean; var DoSave: Boolean);
var Menu      : PullRecord;
    FlagsMenu : PullRecord;
    Choice    : Word;
    FlagChoice: Word;
    CH        : Char;
    TempStr   : String;
    WriteColor: Byte;
    TempScrn  : Pointer;
    OldLangInf: LanguageRecord;
    SaveDirect: Boolean;
begin;
 OldLangInf := LangInf;

 AllocMem(Menu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'EditLanguage');
 InitPullMenu(Menu);
 AddPullItem(Menu.PullInf^[01], 'Name         ', 2801, #00, 'Name of this language - use the country '+
                                                            'of origin''s spelling', 1);
 AddPullItem(Menu.PullInf^[02], 'Available    ', 2802, #00, 'Toggle whether this language is available to users', 1);
 AddPullItem(Menu.PullInf^[03], 'Security     ', 2803, #00, 'Minimum security level required to use this language', 1);
 AddPullItem(Menu.PullInf^[04], 'Flags        ', 2804, #00, 'Flag settings required to use this language', 1);
 AddPullItem(Menu.PullInf^[05], 'Language Text', 2805, #00, 'View or modify the language text', 1);
 AddPullItem(Menu.PullInf^[06], 'Menu path    ', 2806, #00, 'Path to this language''s menus', 1);
 AddPullItem(Menu.PullInf^[07], 'Text path    ', 2807, #00, 'Path to this language''s text (.A??) files', 1);
 AddPullItem(Menu.PullInf^[08], 'Script path  ', 2808, #00, 'Path to this language''s script (.ELM and .Q-A) files', 1);
 AddPullItem(Menu.PullInf^[09], 'Filename     ', 2809, #00, 'Full path and filename of the language definition file '+
                                                            '(.RAL)', 1);

 Menu.Items   := 9;
 Menu.Width   := 77;
 Menu.Length  := 9;
 Menu.X       := 2;
 Menu.Y       := 4;
 Menu.HiLight := 01;
 Menu.AddSpace:= False;
 Menu.Title   := '';
 Menu.PosArray[1].XInc := 00;
 Menu.PosArray[1].YDec := 00;


{ ShowMenu(Menu, False);NOT DoEdit); }
 ShowMenu(Menu, NOT DoEdit);

 If NOT DoEdit then
    begin
      Box(02, 04, 79, 14, mnuDisabled, mnuStyle, True);
    end; { Only showing }

 ShowMenuItems(Menu);

 Repeat;
   SaveDirect := DirectScrnUpdate;
   DirectScrnUpdate := false;

   WriteAT(17, 05, mnuNormColor, LangInf.Name);
   WriteAT(17, 06, mnuNormColor, MakeLen(Bool2Str(Boolean(LangInf.Attribute)), 3, space, false, false));
   WriteAT(17, 07, mnuNormColor, FStr(LangInf.Security));
   WriteAT(17, 08, mnuNormColor, 'A '+ Byte2FlagsOff(LangInf.Flags[1], LangInf.NotFlagsMask[1]) +
                                ' B '+ Byte2FlagsOff(LangInf.Flags[2], LangInf.NotFlagsMask[2]) +
                                ' C '+ Byte2FlagsOff(LangInf.Flags[3], LangInf.NotFlagsMask[3]) +
                                ' D '+ Byte2FlagsOff(LangInf.Flags[4], LangInf.NotFlagsMask[4]));
   WriteAT(17, 09, mnuNormColor, '');                         { Language text }
   WriteAT(17, 10, mnuNormColor, LangInf.MenuPath);
   WriteAT(17, 11, mnuNormColor, LangInf.TextPath);
   WriteAT(17, 12, mnuNormColor, LangInf.QuesPath);
   WriteAT(17, 13, mnuNormColor, LangInf.DefName);
   DirectScrnUpdate := SaveDirect;

   If DoEdit then Choice := DoPullMenu(Menu, CH, False, False)
    else Choice := 00;

   Case Choice of
      2801 : LangInf.Name := Edit(LangInf.Name, 17, 05, mnuEditColor, 20, False, False, True);
      2802 : Edit_Boolean(Boolean(LangInf.Attribute), 17, 06, mnuEditColor);
      2803 : EditWord(LangInf.Security, 17, 07, mnuEditColor, True);
      2804 : begin
                 SaveScreen(TempScrn);
                 AllocMem(FlagsMenu.PullInf, SizeOf(PullInfRecArray), 'PullInfRecArray', 'EditLanguage');
                 InitPullMenu(FlagsMenu);
                 AddPullItem(FlagsMenu.PullInf^[01], ' A - Flags ', 01, #00, 'Change "A" flag set', 01);
                 AddPullItem(FlagsMenu.PullInf^[02], ' B - Flags ', 02, #00, 'Change "B" flag set', 01);
                 AddPullItem(FlagsMenu.PullInf^[03], ' C - Flags ', 03, #00, 'Change "C" flag set', 01);
                 AddPullItem(FlagsMenu.PullInf^[04], ' D - Flags ', 04, #00, 'Change "D" flag set', 01);

                 FlagsMenu.Items   := 4;
                 FlagsMenu.Width   := 12;
                 FlagsMenu.Length  := 4;
                 FlagsMenu.X       := 60;
                 FlagsMenu.Y       := 3;
                 FlagsMenu.HiLight := 01;
                 FlagsMenu.AddSpace:= False;
                 FlagsMenu.Title   := '';
                 FlagsMenu.PosArray[1].XInc := 00;
                 FlagsMenu.PosArray[1].YDec := 00;

                 Repeat;
                   ShowMenu(FlagsMenu, NOT DoEdit);
                   FlagChoice := DoPullMenu(FlagsMenu, CH, False, False);

                   If FlagChoice in [1..4] then
                     EditFlags(LangInf.Flags[FlagChoice], LangInf.NotFlagsMask[FlagChoice], True, 60, 03, FlagsMenu);

                 Until FlagChoice=00;

                 RemoveMenu(FlagsMenu);
                 ReleaseMem(FlagsMenu.PullInf, SizeOf(PullInfRecArray));
                 RestoreScreen(TempScrn);
             end; { Menu }
      2805 : begin
                EditLanguageText(LangInf);
             end;
      2806 : EditDirectory(17, 10, 59, LangInf.MenuPath);
      2807 : EditDirectory(17, 11, 59, LangInf.TextPath);
      2808 : EditDirectory(17, 12, 59, LangInf.QuesPath);
      2809 : begin
                LangInf.DefName := NoExtension(LangInf.DefName);

                LangInf.DefName  := Edit(LangInf.Defname, 17, 13, mnuEditColor, 60, false, False, True);

                LangInf.DefName := NoExtension(LangInf.DefName) + '.ral';
             end; { LanguageInf }
   End; { Case }

  Until Choice=0;

  DoSave := false;
  If RecordsDifferent(LangInf, OldLangInf, SizeOf(LanguageRecord)) then
    If DoSaveChanges('Save changes (Y/n) ? °', True, false) then
      DoSave := true;

  if NOT DoSave then LangInf := OldLangInf;

  if NOT DoEdit then FreeMem(Menu.SaveScrn, SizeOf(SaveArrayType)) else RemoveMenu(Menu);
  ReleaseMem(Menu.PullInf, SizeOf(PullInfRecArray));
end; { proc. EditLanguage }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Language_Activate(HiLight: LongInt;  HiBarPos: LongInt): Longint;
var DoSave    : Boolean;
    SaveScrn  : Pointer;
    LangUpdate: pFileObj;
    SaveDirect: Boolean;
begin;
  Language_Activate := -1;

  Language_F^.Seek( (HiLight - 1) * SizeOf(LanguageRecord));
  Language_F^.BlkRead(LanguageInf, SizeOf(LanguageRecord));

  SaveScreen(SaveScrn);
  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;
  Box(02, 16, 79, mnuScrnLen - 03, mnuDisabled, mnuStyle, false);
  DirectScrnUpdate := SaveDirect;

  EditLanguage(LanguageInf, HiLight, True, DoSave);
{(Done by RestoreScrn): Box(02, 16, 79, 22, mnuBoxColor, mnuStyle, false);}

  RestoreScreen(SaveScrn);

  if DoSave then
    begin
      New(LangUpdate, Init);
      LangUpdate^.Assign(LanguageFileName);
      LangUpdate^.FileMode := ReadWriteMode + DenyNone;

      if LangUpdate^.Open(1) then
        begin
          LangUpdate^.Seek((HiLight-1) * SizeOf(LanguageRecord));
          LangUpdate^.BlkWrite(LanguageInf, SizeOf(LanguageRecord));
        end; { if }

      Dispose(LangUpdate, Done);
    end; { SaveChanges }
end; { proc. Language_Activate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Language_MenuActivate(HiLight: LongInt;  HiBarPos: LongInt): Longint;
var OldLangInf: LanguageRecord;
begin;
  Language_MenuActivate := -1;

  Language_F^.Seek( (HiLight - 1) * SizeOf(LanguageRecord));
  Language_F^.BlkRead(LanguageInf, SizeOf(LanguageRecord));

  CurLangPicked := LanguageInf;
  rdLastkey := #13;
end; { proc. Language_MenuActivate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure LanguageText_GetInfo(var Info1, Info2, Info3, Info4, Info5, Info6, Info7, Info8, Info9: List_StringType;
                               ItemNr: LongInt; HiLight: LongInt);
var HasKeys: Boolean;
    AddChar: Char;
    TempStr: String;
begin
  HasKeys := False;
  AddChar := #32;

  TempStr := LangObj^.DefLangArray^[ItemNr].TextStr^;
  If LangObj^.DefLangArray^[ItemNr].TextStr=nil then TempStr := '';

  If CurLang[ItemNr].TextStr = nil then Info1 := ''
    else Info1 := CurLang[ItemNr].TextStr^;
  Info2 := '';
  Info3 := '';
  Info4 := '';
  Info5 := '';
  Info6 := '';
  Info7 := '';
  Info8 := '';
  Info9 := '';

  If LangObj^.DefLangArray^[ItemNr].Keys<>'' then                       { Contains keys }
    begin
      HasKeys := True;
      AddChar := '*';
    end; { Info1 }

  If CurLang[ItemNr].HasDefault then
    begin;
      AddChar := LangObj^.DefLangArray^[ItemNr].DefaultKey;
      if Length(Info1)>1 then Dec(Info1[0]);
    end;

  If ItemNr=HiLight then
   begin
     PartClear(01, 01, mnuScrnWidth, 01, mnuClearColor, #32);
     WriteAT(01, 01, mnuLangTopColor, TrimLeft(LangObj^.DefLangArray^[ItemNr].Keys + ' ') + TempStr);

     WriteRight(79, 01, mnuBoxColor, AddChar+FStr(ItemNr));
   end; { if }
end; { proc. LanguageText_GetInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function LanguageText_Activate(HiLight: LongInt; HiBarPos: LongInt): Longint;
var Temp      : String;
    SaveScrn  : Pointer;
    CH        : Char;
    KeySet    : String;
    TempStr   : String;
    TempChar  : Char;
    LangCH    : Char;
begin
  LanguageText_Activate := -1;

  if (LangObj^.DefLangArray^[HiLight].Keys <> '') then
    begin
      SaveScreen(SaveScrn);

      ShadFillBoxTitle(71, 02, 80, 06, mnuBoxColor, mnuStyle, True, 'Keys');

      TempStr := CurLang[HiLight].Keys;
      repeat
        CurLang[HiLight].Keys := Trim(Edit(CurLang[HiLight].Keys, 73, 04, mnuEditColor, 7, True, False, True));
      until CurLang[HiLight].Keys <> '';

      if TempStr <> CurLang[HiLight].Keys then RalHasChanged := TRUE;

      RestoreScreen(SaveScrn);
    end; { if }

  If LangObj^.DefLangArray^[HiLight].HasDefault then
    begin
      SaveScreen(SaveScrn);

      TempStr := Curlang[01].Keys[1] + '=' + CurLang[01].TextStr^ + ', ' +
                 Curlang[02].Keys[1] + '=' + CurLang[02].TextStr^ + ' : °';
      KeySet :=  UpCase(Curlang[01].Keys[1]) +
                 UpCase(Curlang[02].Keys[1]);

      ShadFillBoxTitle(80 - (Length(TempStr) + 03), 02, 80, 06, mnuBoxColor, mnuStyle, True, 'Default');
      WriteAT(64, 04, mnuEditColor, TempStr);

      TempChar := CurLang[HiLight].DefaultKey;
      repeat
        CursorOn;
        GotoXY(78, 04);

        LangCH := UpCase(Readkey);

        if LangCH in [KeySet[1], KeySet[2]] then
          begin
            if LangCH = KeySet[1] then
              CurLang[HiLight].DefaultKey := 'Y'
                else CurLang[HiLight].DefaultKey := 'N';

            CurLang[HiLight].TextStr^[Length(CurLang[HiLight].TextStr^)] := UpCase(CurLang[HiLight].DefaultKey);
          end; { if }

        CursorOff;
      until (LangCH in [KeySet[1], KeySet[2], #27]);
      if TempChar <> CurLang[HiLight].DefaultKey then RalHasChanged := TRUE;

      RestoreScreen(SaveScrn);
    end; { when has default key }

  PartClear(mnuMsgXPos, mnuMsgYPos, mnuScrnWidth, mnuMsgYPos, LightGray, #32);

  if CurLang[HiLight].TextStr <> nil then
    Temp := CurLang[HiLight].TextStr^
     else Temp := '';

  If CurLang[HiLight].HasDefault then Dec(Temp[0]);
  if CurLang[HiLight].TextStr = nil then Temp := '';

  PartClear(01, HiBarPos + 02, mnuScrnWidth, HibarPos + 02, 07, #32);
  GetString(01, HiBarPos + 2, mnuEditColor, Temp, [#32..#254], [#13, #27], 250, 79, False, True, True, false,
            CurLang[HiLight].Txtcolor);

  if CurLang[HiLight].TextStr <> nil then
   if Temp <> CurLang[HiLight].TextStr^ then
     RalHasChanged := TRUE;

  if CurLang[HiLight].TextStr = nil then
    if Temp <> '' then RalHasChanged := true;

  StringDispose(CurLang[HiLight].TextStr);

  If CurLang[HiLight].HasDefault then
    Temp := Temp + CurLang[HiLight].DefaultKey;

  CurLang[HiLight].TextStr := StringNew(Temp);
end; { proc. LanguageText_Activate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function LanguageText_GetItems: LongInt;
begin
  LanguageText_GetItems := defRalSize;
end; { func. LanguageText_GetItems }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function LanguageText_Seek(Str: String): LongInt;
var Counter: Word;
begin
  LanguageText_Seek:= -1;
  Counter:=00;

(*
  While Counter <= defRalSize do
   begin;
    If SUpcase(Str) = SupCase(Copy(CurLang[Counter].TextStr^, 1, Length(Str)))
      then begin;
              Languagetext_Seek := Counter;
              Break;
           end; { Found one }
     Inc(Counter);
   end; { While Counter }
*)

end; { func. LanguageText_Seek }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function LanguageText_CreateNewFile: Boolean;
begin
  LoadRalDefaults(CurLang);

  LanguageText_CreateNewfile := CreateNewRalFile(LanguageInf.DefName, CurLang, false);
end; { proc. LanguageText_CreateNewFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function LanguageText_Keypress(CH: Char; HiLight: LongInt;
                               var HiBarPos, TopOfScrn: Longint;
                               var StartRange, EndRange: LongInt): LongInt;
var TempFore,
    TempBack    : Byte;
    TempCH      : Char;
    mnuSaveColor: Byte;
    TempStr     : String;
    Counter     : LongInt;
    Menu        : PullRecord;
    GotoNumber  : SmallWord;
begin
  LanguageText_KeyPress := -1;

  Case CH of
 { ALT-D }      #32 : begin;
                        StringDispose(CurLang[HiLight].TextStr);

                        TempStr := '';
                        RalHasChanged := TRUE;
                        if LangObj^.DefLangArray^[HiLight].HasDefault then TempStr := 'Y';

                        with Langobj^ do
                          begin
                            CurLang[HiLight].TextStr   := StringNew(DefLangArray^[HiLight].TextStr^ + TempStr);
                            CurLang[HiLight].HasDefault:= DefLangArray^[HiLight].HasDefault;
                            CurLang[HiLight].DefaultKey:= DefLangArray^[HiLight].DefaultKey;
                            CurLang[HiLight].Keys      := DefLangArray^[HiLight].Keys;
                            CurLang[HiLight].TxtColor  := DefLangArray^[HiLight].TxtColor;
                          end; { with }

                        if HiLight < defRalSize then
                          Inc(HiBarPos);
{!!!                            LanguageText_Keypress := HiLight + 01;}

                      end; { ALT - D (Default) }
 { ALT-C }      #46 : begin
                         TempFore := GetForeAttr(CurLang[HiLight].TxtColor);
                         TempBack := GetBackAttr(CurLang[HiLight].TxtColor);

                         CurLang[HiLight].TxtColor := Byte(Edit_Color(CurLang[HiLight].TxtColor, TempFore, TempBack, Menu,
                                                           False));
                         if MakeAttr(TempFore, TempBack) <> CurLang[HiLight].TxtColor then
                            RalhasChanged := true;
                      end; { ALT - C (Colour) }
 { ALT-I }      #23 : begin
                        Repeat;
                           mnuSaveColor := mnuTitleColor;
                           mnuTitleColor := mnuErrorColor;

                           ShadFillBoxTitle(01, 10, mnuScrnWidth, 14, mnuErrorColor, mnuStyle, True, ' Info ');
                           WriteAT(03, 12, mnuErrorColor, Copy(CurLang[00].TextStr^, 1, 76));

                           mnuTitleColor := mnuSaveColor;

                           TempCH := ReadKey;

                           If TempCH=#13 then
                            begin;
                              TempStr := CurLang[00].TextStr^;
                              If CurLang[00].TextStr = nil then TempStr := '';

                              GetString(03, 12, mnuErrorColor, TempStr, [#32..#254], [#13, #27], 200, 76, False, False, True,
                                        false, 0);
                              if TempStr <> CurLang[00].TextStr^ then
                                RalHasChanged := True;

                              StringDispose(CurLang[00].TextStr);
                              CurLang[00].TextStr := StringNew(TempStr);
                            end; { if }

                        Until TempCH in [#27];

                      end; { ALT - I (Information) }
 { ALT-F }      #33 : begin;
                        WriteAT(01, 01, White, 'Find: ');

                        LangSearchStr := Edit(LangSearchStr, 07, 01, mnuEditColor, mnuScrnWidth - 16, False, False, True);

                        if rdLastKey = #13 then
                          begin
                           If NOT ( HiLight + 01 > defRalSize) then
                            For Counter := Hilight+1 to defRalSize do
                             if CurLang[Counter].TextStr <> nil then
                              If Pos(SupCase(LangSearchStr), SupCase(CurLang[Counter].TextStr^)) <> 00 then
                                begin
                                  LanguageText_Keypress := Counter;
                                  break;
                                end; { if }

                            if Counter > defRalSize then Counter := defRalSize;

                             if Pos(SupCase(LangSearchStr), SupCase(CurLang[Counter].TextStr^)) = 0 then
                              begin
                                WriteAT(01, 01, White + 128, 'Not found');
                                KeyDelay(500, true);
                              end; { if }
                          end; { if }

                      end; { ALT - F (Find) }
 { ALT-G }      #34 : begin
                        PartClear(01, 01, 80, 01, White, #32);
                        WriteAT(01, 01, White, 'Goto: ');

                        GotoNumber := 00;
                        EditWord(GotoNumber, 07, 01, mnuEditColor, true);

                        if rdLastKey = #13 then
                          LanguageText_KeyPress := GotoNumber;
                      end; { if }
 { ALT-N }      #49 : begin
                        If NOT ( HiLight + 01 > defRalSize) then
                         For Counter := Hilight+1 to defRalSize do
                          if CurLang[Counter].TextStr <> nil then
                           If Pos(SupCase(LangSearchStr), SupCase(CurLang[Counter].TextStr^)) <> 00 then
                              begin
                                LanguageText_Keypress := Counter;
                                break;
                              end; { if }

                      end; { ALT - N (Find again) }

  end; { case }
end; { func. LanguageText_Keypress }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure EditLanguageText(var LangInf: LanguageRecord);
var Counter : Word;
    SaveScrn: Pointer;
    TempScrn: Pointer;
    TempStr : String;
begin
   if (LangInf.Defname = '') OR (SUpCase(LangInf.DefName) = '.ral') then
     begin
       MsgBox('Please first define language filename', true, TempScrn);
       GetInput;
       MsgBox('Please first define language filename', false, TempScrn);
       EXIT;
     end; { if }

   If NOT FileExist(LangInf.DefName) then
    begin
      LanguageInf := LangInf;
      if NOT LanguageText_CreateNewfile then
        begin
          CantCreate(LangInf.DefName, False);
          EXIT;
       end; { if }
    end; { if }

   MsgBox('Loading language text ...', true, TempScrn);

   If FileExist(LangInf.DefName) then
     LangObj^.ReadRalFile(LangInf.DefName);

   New(LangObj^.DefLangArray);
   LoadralDefaults(LangObj^.DefLangArray^);
   loadRalDefaults(CurLang);
   LangObj^.RalCodes := False;

   for Counter := 1 to defRalSize do
    begin
        if CurLang[Counter].TextStr <> nil then
         StringDispose(CurLang[Counter].TextStr);

        With CurLang[Counter] do
          begin
            LangObj^.RalGetAll(Counter, TxtColor, TempStr, Keys, DefaultKey, LangObj^.DefLangArray^[Counter].HasDefault,
                               (LangObj^.DefLangArray^[Counter].Keys <> ''));
            TextStr := StringNew(TempStr);
          end; { with }
     end; { If }

   StringDispose(CurLang[00].TextStr);
   CurLang[00].TextStr := StringNew(LangObj^.ralGetTitleInfo);

   MsgBox('Loading language text ...', false, TempScrn);
   SaveScreen(SaveScrn);

   RalHasChanged := FALSE;
    DoList(False, 01, 80, 00, 00, 00, 00, 00, 00, 00, 00,                         { Show Lengths }
           00, 02, 79, mnuScrnLen - 01, '',
           'Enter-Edit  Esc-Exit  ALT: C-Change colour D-Copy default F-Find I-Info G-Goto',
           {$IFDEF FPC}@{$ENDIF}LanguageText_GetInfo,
           {$IFDEF FPC}@{$ENDIF}LanguageText_Activate,
           {$IFDEF FPC}@{$ENDIF}LanguageText_Seek,
           {$IFDEF FPC}@{$ENDIF}General_AbortCheck,
           {$IFDEF FPC}@{$ENDIF}LanguageText_KeyPress,
           {$IFDEF FPC}@{$ENDIF}LanguageText_GetItems,
           {$IFDEF FPC}@{$ENDIF}LanguageText_CreateNewFile,
           {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
           False,
           00, 00,
           False, False);

   Dispose(LangObj^.RalFile, Done);
   LangObj^.RalFile := nil;

   if RalHasChanged then
    If DoSaveChanges('Save changes (Y/n)? °', True, false) then
     begin
       if NOT CreateNewRalFile(LangInf.DefName, CurLang, true) then
         begin
           CantCreate(LangInf.DefName, False);
        end; { if }
     end; { if }

  RestoreScreen(SaveScrn);

   For Counter := 0 to defRalSize do
    begin
       StringDispose(CurLang[Counter].TextStr);
       StringDispose(LangObj^.DefLangArray^[Counter].TextStr);
    end; { If }

   Dispose(LangObj^.DefLangArray);
end; { proc. EditLanguage }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function InCombinedRange(Nr: Word; Combined: CombinedRecord): Boolean;
var Counter: Byte;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logSupport, 'Incombinedrange');
  {$ENDIF}

  InCombinedRange := false;

  For Counter := 01 to 200 do
   If Combined[Counter] = Nr then
        begin
          InCombinedRange := True;
          break;
        end; { if }
end; { func. InCombinedRange }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure Combined_GetInfo(var Info1, Info2, Info3, Info4, Info5, Info6, Info7, Info8, Info9: List_StringType;
                           ItemNr: LongInt; HiLight: LongInt);
var MsgInf: MessageRecord;
begin
  MsgArea_F^.Seek(Pred(ItemNr) * SizeOf(MessageRecord));         { FileSize checking is done bij 'DoList' }
  MsgArea_F^.BlkRead(MsgInf, SizeOf(MessageRecord));

  Info1 := MsgInf.Name;

  If InCombinedRange(MsgArea_F^.FilePos  DIV SizeOf(MessageRecord), GlobalCfg^.RaConfig^.DefaultCombined) then Info2:=Chr(254)
   else Info2 := '';

  Info3 := FStr(MsgArea_F^.FilePos DIV SizeOf(MessageRecord));
  Info4 := '';
  Info5 := '';
  Info6 := '';
  Info7 := '';
  Info8 := '';
  Info9 := '';

  If Info1='' then Info1 := '[Unused]';
end; { proc. MsgArea_GetInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Function Combined_Activate(HiLight: LongInt; HiBarPos: LongInt): Longint;
begin;
  Combined_Activate := -1;

  If InCombinedRange(HiLight, GlobalCfg^.RaConfig^.DefaultCombined) then
      ToggleCombined(HiLight, GlobalCfg^.RaConfig^.DefaultCombined, False)
       else ToggleCombined(HiLight, GlobalCfg^.RaConfig^.DefaultCombined, True);
end; { proc. MsgArea_Activate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure DoCombined;
begin
  If NOT FileExist(MessagesFileName) then
    if NOT CreateNewMessageRa then
     begin
       CantCreate(JustName(MessagesFileName), False);
       EXIT;
     end; { if }

  if NOT OpenFile(MessagesFileName, ReadWriteMode + DenyWrite) then
      begin
        CantCreate(JustName(MessagesFileName), True);
        EXIT;
      end; { if }

  IsCombining := True;

  New(MsgArea_F, Init);
  MsgArea_F^.Assign(MessagesFileName);
  MsgArea_F^.FileMode := ReadWriteMode + DenyWrite;
  MsgArea_F^.Open(01);

  AllocMem(MessageInf, SizeOf(MessageRecord), 'MessageRecord', 'DoMessageAreas');

  CreateEleMessageFiles;

  if NOT OpenFile(MessageEleFileName, ReadWriteMode + DenyWrite) then
      begin
        CantCreate(JustName(MessageEleFileName), True);
        EXIT;
      end; { if }

  New(EleMSG_F, Init);
  EleMSG_F^.Assign(MessageEleFileName);
  EleMSG_F^.FileMode := ReadWriteMode + DenyWrite;
  EleMSG_F^.Open(1);

  if (NOT UpToDateFile(MessagesFileName, RdxName(MessagesFileName)))
      then GenerateRdxFiles(JustPath(MessagesFileName), True, False, true);

  DoList(True, 13, 40, 7, 5, 0, 00, 00, 00, 00, 00,                                   { ShowLen }
         12, 04, 66, mnuScrnLen - 4,                                { Window Coordinates }
         ' Toggle combined default ',
         '',
         {$IFDEF FPC}@{$ENDIF}Combined_GetInfo,
         {$IFDEF FPC}@{$ENDIF}Combined_Activate,
         {$IFDEF FPC}@{$ENDIF}MsgArea_Seek,
         {$IFDEF FPC}@{$ENDIF}General_AbortCheck,
         {$IFDEF FPC}@{$ENDIF}Copy_KeyPress,
         {$IFDEF FPC}@{$ENDIF}MsgArea_GetItems,
         {$IFDEF FPC}@{$ENDIF}CreateNewMessageRa,
         {$IFDEF FPC}@{$ENDIF}EmptyAllCharHook,
         False,
         00, 00,
         true, false);

  Dispose(MsgArea_F, Done);
  Dispose(EleMSG_F, Done);

  ReleaseMem(MessageInf, SizeOf(MessageRecord));
end; { proc. DoCombined }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { proc. Area_Lst }
