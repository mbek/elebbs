unit AreaSel;
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
** Area/Group (message,files) selection routines for EleBBS
**
** Copyright (c) 1996, 1997 by Maarten Bekers
**
** Created : 13-Nov-1997
** Last update : 13-Nov-1997
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF WITH_FULL}
  {$IFNDEF ISCGI}
    This unit shouldn''t be compiled for an utility program
  {$ENDIF}
{$ENDIF}

uses CfgRec;

{$IFDEF WITH_FULL}
function  SearchCombined(AreaNum: Longint): Boolean;
function  ChangeGroup(Message: Boolean; MiscData: String): Boolean;
procedure ChangefileArea(MiscData: String; var Number: System.Word);
procedure ChangeMessageArea(MiscData: String);
procedure ShowMsgAreaNewMail(MiscData: String; DoEnter, ShowAns, IsCombined: Boolean);
procedure ClearCombined(MiscData: String);
procedure SelectCombined(MiscData: String);
procedure ToggleCombinedArea(AreaNum: Longint);
{$ENDIF}


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Debug_U, Global, FileRout, MkFile, Ral, BitWise, ObjDec,

     {$IFDEF WITH_FULL}
       Input_U,
       Support,
       LineEd,
       DispAns,
       Control,
       Question,
     {$ENDIF}

      Access_U, StUtils, MkOpen, MkMsgAbs, MkMsgJam, ElLog_U,
       Cases, StrPath, LongStr, CentrStr,
        WordStr, StrUnit, Mail, FileObj;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

var GlobalGroupCheck : Boolean;
    GlobDoingCombined: Boolean;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
procedure ShowMsgAreaNewMail(MiscData: String; DoEnter, ShowAns, IsCombined: Boolean);
var Area_F      : pFileObj;
    MsgInf      : MessageRecord;

    GroupNr     : Word;
    TempStr     : String;
    OneLiner    : Boolean;
    LineLen     : Byte;
    NumRead     : NumReadtype;
    CheckGroup  : Boolean;
    NextIsEnter : Boolean;

    TotalItems,
    ItemCounter : Longint;
    Dummy       : Longint;
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logAreas, 'ShowMsgAreNewMail');{$endif}

  CheckGroup := Pos('/MG', SUpcase(MiscData)) > 00;
  OneLiner := Pos('/1', MiscData) > 00;
  GroupNr := FVal(GetValue('/MG=', MiscData, True));

  If GroupNr=00 then GroupNr := LineCfg^.Exitinfo^.Userinfo.MsgGroup; { No value? Current group }

  OutputObj^.ClearScreen;
  WriteLn;
  NextIsEnter := False;

  If ShowAns then
   If (NOT CheckGroup) then
    If DisplayHotFile('MSGAREA', [])<>#01
     then EXIT;                                             { File NOT Found }


  If ShowAns then
   If (GroupNr>00) then
    If DisplayHotFile('MGR'+FStr(GroupNr), [])<>#01
     then EXIT;                                              { File NOT Found }

  WriteLn;
  WriteLn('`A15:',LangObj^.ralGet(ralMsgAreas));             { Message Areas: }
  WriteLn;

  New(Area_F, Init);
  Area_F^.Assign(MessagesFileName);
  Area_F^.FileMode := ReadMode + DenyNone;
  if Area_F^.Open(1) then
    begin
      TotalItems := Area_F^.FileSize div SizeOf(MessageRecord);
      ItemCounter := 00;

      While (ItemCounter < TotalItems) AND (NOT OutputObj^.StopMore) do
        begin
          NumRead := Area_F^.BlkRead(MsgInf, SizeOf(MessageRecord));
          Inc(ItemCounter);

          if CheckMsgAreaAccess(MsgInf, CheckGroup, True, GroupNr, LineCfg^.Exitinfo^.userinfo) then
            begin
              If OneLiner then NextIsEnter := True;
              if OneLiner then LineLen := 40
                else LineLen := 30;
              TempStr := Dup(#32, 3);                                  { 3 spaces }

              if NOT IsCombined then
                begin
                  if ((Pos('/NONEW', SUpCase(MiscData)) = 00)) AND (NOT IsCombined) then
                   If HasNewMail(MsgInf) then
                       TempStr := ' `A15:* ';
                end
                 else If SearchCombined(MsgInf.AreaNum) then TempStr := ' `A15:> ';

              If NOT NextIsEnter then
                Write('`A14:', ItemCounter:5, TempStr, '`A3:', NoColorCopy(MsgInf.Name, 1, LineLen), '`X40:')
                 else
                   WriteLn('`A14:', ItemCounter:5, TempStr, '`A3:', NoColorCopy(MsgInf.Name, 1, LineLen));

              NextIsEnter := NOT NextIsEnter;
            end; { If MsgInf.Name <> '' }

        end; { While }

    end; { if Area is opened }
  Dispose(Area_F, Done);

  OutputObj^.ResetLines(01);
  If NextIsEnter then WriteLn;
  WriteLn;

  OutputObj^.SetStopMore(False);
  If DoEnter then InputObj^.PressEnter(True, False);

  {$ifdef With_Debug}
    DebugObj.DebugLog(logAreas, 'End of ShowMsgAreaNewMail');
  {$endif}
end; { proc. ShowMsgAreaNewMail }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure MessageAreaHook(RecordNum: Longint; var Answers: StringArrayObj; StartRecord: Longint;
                          GoingDown: Boolean; var GlobalFile: pFileObj);
var MessageInf : MessageRecord;
    NumRead    : NumreadType;
    AreaNrStr  : String;
    NewMailStr : String;
    EndOfFile  : Boolean;
    Dummy      : Longint;
begin
  GlobalFile^.Seek((Recordnum - 01) * SizeOf(MessageRecord));

  repeat
    if (GlobalFile^.BlkRead(MessageInf, SizeOf(MessageRecord)) = 0) then
      begin
        FillChar(MessageInf, SizeOf(MessageRecord), #00);
        EndOfFile  := TRUE;
      end; { if }

    if (NOT CheckMsgAreaAccess(MessageInf,
                               GlobalGroupCheck,
                               true,
                               LineCfg^.Exitinfo^.Userinfo.MsgGroup,
                               LineCfg^.Exitinfo^.userinfo)) then
                                   MessageInf.Name := '';

    if MessageInf.Name = '' then
     if NOT GoingDown then
       GlobalFile^.Seek(GlobalFile^.FilePos - (SizeOf(MessageRecord) * 2));

    if GoingDown then
      EndOfFile := GlobalFile^.FilePos >= GlobalFile^.FileSize
       else EndOfFile := GlobalFile^.FilePos <= 0;
  until (MessageInf.Name <> '') OR (EndOfFile);

  AreaNrStr := FStr(MessageInf.AreaNum);
  if MessageInf.Name = '' then AreaNrStr := '0';

  if GlobDoingCombined then
    begin
      if SearchCombined(MessageInf.AreaNum) then
        NewMailStr := '>'
         else NewMailStr := ' ';
    end
      else begin
             if HasNewMail(MessageInf) then NewMailStr := '*'
               else NewMailStr := ' ';
           end; { if }

  Answers.Put(StartRecord, FStr(GlobalFile^.FilePos DIV SizeOf(MessageRecord)));
  Answers.Put(StartRecord + 01, AreaNrStr);
  Answers.Put(StartRecord + 02, MessageInf.Name);
  Answers.Put(StartRecord + 03, NewMailStr);
end; { proc. MessageAreaHook }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ScriptMessageAreaChange(var AreaTemp: String; CheckGroup: Boolean): Boolean;
var Quest     : QuestObj;
    TotalAreas: Longint;
    MessageInf: MessageRecord;
    Numread   : NumreadType;
    BeforeRec : Longint;
    GlobalFile: pFileObj;
begin
  ScriptMessageAreaChange := false;
  TotalAreas := 00;

  GlobalGroupCheck := CheckGroup;

  New(GlobalFile, Init);
  GlobalFile^.Assign(MessagesFileName);
  GlobalFile^.FileMode := ReadMode + Denynone;
  if NOT GlobalFile^.Open(1) then
    begin
      Dispose(GlobalFile, Done);
      EXIT;
    end; { if }

  {------------------- Count the number of records in this file -------------}
  repeat
    if (GlobalFile^.BlkRead(MessageInf, SizeOf(MessageRecord)) = 0) then
      FillChar(MessageInf, SizeOf(MessageRecord), #00);

    if (NOT CheckMsgAreaAccess(MessageInf,
                               GlobalGroupCheck,
                               true,
                               LineCfg^.Exitinfo^.Userinfo.MsgGroup,
                               LineCfg^.Exitinfo^.userinfo)) then
                                   MessageInf.Name := '';

    if MessageInf.Name <> '' then
      Inc(TotalAreas);

    if LineCfg^.Exitinfo^.Userinfo.MsgArea = MessageInf.AreaNum then
      BeforeRec := TotalAreas;
  until (GlobalFile^.FilePos >= GlobalFile^.FileSize);
  {------------------- Count the number of records in this file -------------}

  Quest.Init;
  Quest.QInfo^.Answers^.Put(01, FStr(TotalAreas));
  Quest.QInfo^.Answers^.Put(02, FStr(Ra250MsgArea(LineCfg^.Exitinfo^.Userinfo.MsgArea)));
  Quest.QInfo^.Answers^.Put(03, FStr(BeforeRec));
  Quest.QInfo^.GetInfoFile := GlobalFile;
  Quest.QInfo^.GetInfoHook := {$IFDEF FPC}@{$ENDIF}MessageAreaHook;
  Quest.Process('MA-CHNG /N', false, '');

  AreaTemp := Quest.GetReturnResult;

  ScriptMessageAreaChange := (Quest.GetError = 0);

  Quest.Done;

  Dispose(GlobalFile, Done);
end; { func. ScriptMessageAreaChange }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ScriptMessageCombined(var AreaTemp: String; CheckGroup: Boolean): Boolean;
var Quest     : QuestObj;
    TotalAreas: Longint;
    MessageInf: MessageRecord;
    Numread   : NumreadType;
    BeforeRec : Longint;
    GlobalFile: pFileObj;
begin
  ScriptMessageCombined := false;
  TotalAreas := 00;

  GlobalGroupCheck := CheckGroup;
  New(GlobalFile, Init);
  GlobalFile^.Assign(MessagesFileName);
  GlobalFile^.FileMode := ReadMode + Denynone;
  if NOT GlobalFile^.Open(1) then
    begin
      Dispose(GlobalFile, Done);
      EXIT;
    end; { if }

  {------------------- Count the number of records in this file -------------}
  repeat
    if (GlobalFile^.BlkRead(MessageInf, SizeOf(MessageRecord)) = 0) then
      FillChar(MessageInf, SizeOf(MessageRecord), #00);

    if (NOT CheckMsgAreaAccess(MessageInf,
                               GlobalGroupCheck,
                               true,
                               LineCfg^.Exitinfo^.Userinfo.MsgGroup,
                               LineCfg^.Exitinfo^.userinfo)) then
                                   MessageInf.Name := '';

    if MessageInf.Name <> '' then
      Inc(TotalAreas);

    if LineCfg^.Exitinfo^.Userinfo.MsgArea = MessageInf.AreaNum then
      BeforeRec := TotalAreas;
  until (GlobalFile^.FilePos >= GlobalFile^.FileSize);
  {------------------- Count the number of records in this file -------------}

  Quest.Init;
  Quest.QInfo^.Answers^.Put(01, FStr(TotalAreas));
  Quest.QInfo^.Answers^.Put(02, FStr(Ra250MsgArea(LineCfg^.Exitinfo^.Userinfo.MsgArea)));
  Quest.QInfo^.Answers^.Put(03, FStr(BeforeRec));
  Quest.QInfo^.GetInfoFile := GlobalFile;
  Quest.QInfo^.GetInfoHook := {$IFDEF FPC}@{$ENDIF}MessageAreaHook;
  Quest.Process('COMBINED /N', false, '');

  AreaTemp := Quest.GetReturnResult;

  ScriptMessageCombined := (Quest.GetError = 0);

  Quest.Done;

  Dispose(GlobalFile, Done);
end; { func. ScriptMessageCombined }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowFileAreas(MiscData: String; DoEnter, ShowAns: Boolean);
var CheckGroup : Boolean;
    FileArea_F : pFileObj;
    FilesInf   : FilesRecord;
    NextIsEnter: Boolean;
    NumRead    : NumreadType;
    GroupNr    : Word;
    TempStr    : String;

    TotalItems : Longint;
    ItemCounter: Longint;

    OneLiner   : Boolean;
    LineLen    : Byte;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logAreas, 'ShowFileAreas (begin)');
  {$ENDIF}

  CheckGroup := Pos('/FG', SUpcase(MiscData)) > 00;
  OneLiner := Pos('/1', MiscData) > 00;
  GroupNr := FVal(GetValue('/FG=', MiscData, True));

  If GroupNr=00 then GroupNr := LineCfg^.Exitinfo^.Userinfo.FileGroup; { No value? Current group }

  OutputObj^.ClearScreen;
  WriteLn;

  If ShowAns then
   If (NOT CheckGroup) then
    If DisplayHotFile('FILEAREA', [])<>#01
     then EXIT;                                              { File NOT Found }

  If ShowAns then
   If (GroupNr>00) then
    If DisplayHotFile('FGR'+FStr(GroupNr), [])<>#01
     then EXIT;                                              { File NOT Found }

  WriteLn;
  WriteLn;
  WriteLn('`A15:',LangObj^.ralGet(ralFileAreas));               { File Areas: }
  WriteLn;

  New(FileArea_F, Init);
  FileArea_F^.Assign(FilesFileName);
  FileArea_F^.FileMode := ReadMode + DenyNone;
  if NOT FileArea_F^.Open(1) then
    begin
      Dispose(FileArea_F, Done);
      EXIT;
    end; { if }

  ItemCounter:= 00;
  TotalItems := FileArea_F^.FileSize div SizeOf(FilesRecord);
  NextIsEnter:= False;


  While (ItemCounter < TotalItems) AND (Totalitems>00) AND (NOT OutputObj^.StopMore) do
    begin
      NumRead := FileArea_F^.BlkRead(Filesinf, SizeOf(FilesRecord));
      Inc(ItemCounter);
      FilesInf.FilePath := ForceBack(FilesInf.FilePath);

      If OneLiner then NextIsEnter := True;

      if OneLiner then LineLen := 40
        else LineLen := 30;

      if CheckFileAreaAccess(FilesInf, False, CheckGroup, False, True, GroupNr, LineCfg^.Exitinfo^) then
       begin
          TempStr := ' `A7:. ';

          If NOT NextIsEnter then
            Write('`A14:', ItemCounter:5, TempStr, '`A3:', NoColorCopy(FilesInf.Name, 1, LineLen),'`X40:')
             else
              WriteLn('`A14:', ItemCounter:5, TempStr, '`A3:', NoColorCopy(FilesInf.Name, 1, LineLen));

          NextIsEnter := NOT NextIsEnter;
        end; { If FilesInf.Name <> '' }

    end; { while }

  If NextIsEnter then WriteLn;
  OutputObj^.ResetLines(01);
  WriteLn;

  If DoEnter then InputObj^.PressEnter(True, False);

  Dispose(FileArea_F, Done);
  OutputObj^.SetStopMore(False);
  OutputObj^.ResetLines(01);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logAreas, 'ShowFileAreas ( end )');
  {$ENDIF}
end; { proc. ShowFileAreas }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FileAreaHook(RecordNum: Longint; var Answers: StringArrayObj; StartRecord: Longint;
                       GoingDown: Boolean; var GlobalFile: pFileObj);
var FilesInf : FilesRecord;
    NumRead  : NumreadType;
    AreaNrStr: String;
    EndOfFile: Boolean;
begin
  GlobalFile^.Seek((Recordnum - 01) * SizeOf(FilesRecord));

  repeat
    if (GlobalFile^.BlkRead(FilesInf, SizeOf(FilesRecord)) = 0) then
      FillChar(FilesInf, SizeOf(filesRecord), #00);

    if FilesInf.Name <> '' then
     if (NOT CheckFileAreaAccess(Filesinf,
                                 false,               { Need download access? }
                                 GlobalGroupCheck,
                                 false,                 { Need upload access? }
                                 true,                    { Need list access? }
                                 LineCfg^.Exitinfo^.Userinfo.FileGroup,
                                 LineCfg^.Exitinfo^)) then
                                    FilesInf.Name := '';

    if GoingDown then
      EndOfFile := GlobalFile^.FilePos >= GlobalFile^.FileSize
       else EndOfFile := GlobalFile^.FilePos <= 0;

    if FilesInf.Name = '' then
     if NOT GoingDown then
       GlobalFile^.Seek(GlobalFile^.FilePos - (SizeOf(FilesRecord) * 2));

  until (FilesInf.Name <> '') OR (EndOfFile);

  AreaNrStr := fStr(FilesInf.AreaNum);
  if FilesInf.Name = '' then AreaNrStr := '0';

  Answers.Put(StartRecord, FStr(GlobalFile^.FilePos DIV SizeOf(FilesRecord)));
  Answers.Put(StartRecord + 01, AreaNrStr);
  Answers.Put(StartRecord + 02, FilesInf.Name);
end; { proc. FileAreaHook }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ScriptFileAreaChange(var AreaTemp: String; CheckGroup: Boolean): Boolean;
var Quest      : QuestObj;
    TotalAreas : Longint;
    FilesInf   : FilesRecord;
    Numread    : NumreadType;
    BeforeRec  : Longint;
    GlobalFile : pFileObj;
begin
  ScriptFileAreaChange := false;
  TotalAreas := 00;

  GlobalGroupCheck := CheckGroup;
  New(GlobalFile, Init);
  GlobalFile^.Assign(FilesFileName);
  GlobalFile^.FileMode := ReadMode + Denynone;
  if NOT GlobalFile^.Open(1) then
    begin
      Dispose(GlobalFile, Done);
      EXIT;
    end; { if }

  {------------------- Count the number of records in this file -------------}
  repeat
    if (GlobalFile^.BlkRead(FilesInf, SizeOf(FilesRecord)) = 0) then
      FillChar(FilesInf, SizeOf(filesRecord), #00);

    if FilesInf.Name <> '' then
     if (NOT CheckFileAreaAccess(Filesinf,
                                 false,               { Need download access? }
                                 GlobalGroupCheck,
                                 false,                 { Need upload access? }
                                 true,                    { Need list access? }
                                 LineCfg^.Exitinfo^.Userinfo.FileGroup,
                                 LineCfg^.Exitinfo^)) then
                                    FilesInf.Name := '';

    if FilesInf.Name <> '' then
      Inc(TotalAreas);

    if LineCfg^.Exitinfo^.Userinfo.FileArea = FilesInf.AreaNum then
      BeforeRec := TotalAreas;

  until (GlobalFile^.FilePos >= GlobalFile^.FileSize);
  {------------------- Count the number of records in this file -------------}


  Quest.Init;
  Quest.QInfo^.Answers^.Put(01, FStr(TotalAreas));
  Quest.QInfo^.Answers^.Put(02, FStr(Ra250Area(LineCfg^.Exitinfo^.Userinfo.FileArea)));
  Quest.QInfo^.Answers^.Put(03, FStr(BeforeRec));
  Quest.QInfo^.GetInfoFile := GlobalFile;
  Quest.QInfo^.GetInfoHook := {$IFDEF FPC}@{$ENDIF}FileAreaHook;

  Quest.Process('FA-CHNG /N', false, '');
  AreaTemp := Quest.GetReturnResult;

  ScriptFileAreaChange := (Quest.GetError = 0);

  Quest.Done;

  Dispose(GlobalFile, Done);
end; { func. ScriptFileAreaChange }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ChangefileArea(MiscData: String; var Number: System.Word);
var TempBool  : Boolean;
    AreaTemp  : String;
    FilesInf  : FilesRecord;
    CheckGroup: Boolean;
    OnlyGroup : Boolean;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logAreas, 'ChangeFileArea');
  {$ENDIF}

  TempBool := True;
  FilesInf.AreaNum := Number;
  CheckGroup := (Pos('/FG', SUpcase(MiscData)) > 00);
  OnlyGroup := Pos('/OG', SUpCase(MiscData)) > 00;

  if OnlyGroup then
   if Pos('/FG=0', SUpCase(MiscData)) = 0 then MiscData := MiscData + '/FG=0';

  if (Pos('/FG=0', SUpcase(MiscData))>00) OR (LineCfg^.Exitinfo^.Userinfo.FileGroup=0) then
    begin
      TempBool := ChangeGroup(false, MiscData);
    end
     else if (Pos('/FG=', SUpCase(MiscData)) > 00) then
           LineCfg^.Exitinfo^.Userinfo.FileGroup := FVal(GetValue('/FG=', MiscData, false));

  if OnlyGroup then EXIT;

  If TempBool then
    begin
      if NOT ScriptFileAreaChange(AreaTemp, CheckGroup) then
        begin
          ShowFileAreas(MiscData, False, True);

          AreaTemp := '';
          Write('`A15:' , LangObj^.ralGet(ralSelArea));
          GetString(AreaTemp, 5, ['0'..'9'], False, False, False, False);
        end; { if }

      if (FVal(AreaTemp)=00) OR (AreaTemp='') then Exit;

      GetFilesRecord(FilesInf, FVal(AreaTemp), False);
      if (NOT CheckFileAreaAccess(FilesInf, False, CheckGroup, False, True,
                                  LineCfg^.Exitinfo^.Userinfo.FileGroup,
                                  LineCfg^.Exitinfo^)) then
        begin;
          WriteLn;
          WriteLn('`A12:', LangObj^.ralGet(ralInvArea));
          InputObj^.PressEnter(True, False);
          EXIT;
        end; { CheckGroupAccess }
    end; { if }

  Number := FilesInf.AreaNum;

  OutputObj^.ResetLines(01);
  WriteLn;
  WriteLn;

  OutputObj^.SetStopMore(False);
  OutputObj^.ResetLines(01);
end; { proc. ChangefileArea }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ChangeMessageArea(MiscData: String);
var TempBool  : Boolean;
    AreaTemp  : String;
    MessageInf: MessageRecord;
    CheckGroup: Boolean;
    OnlyGroup : Boolean;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logAreas, 'ChangeMessageArea: '+MiscData);
  {$ENDIF}

  TempBool := True;
  MessageInf.AreaNum := LineCfg^.Exitinfo^.Userinfo.Msgarea;
  CheckGroup := (Pos('/MG', SUpcase(MiscData)) > 00);
  OnlyGroup := Pos('/OG', SUpCase(MiscData)) > 00;

  if OnlyGroup then
   if Pos('/MG=0', SUpCase(MiscData)) = 0 then MiscData := MiscData + '/MG=0';

  if (Pos('/MG=0', SUpcase(MiscData))>00) OR (LineCfg^.Exitinfo^.Userinfo.MsgGroup=0) then
    begin
      TempBool := ChangeGroup(True, MiscData);
    end
     else if (Pos('/MG=', SUpCase(MiscData)) > 00) then
            LineCfg^.Exitinfo^.Userinfo.MsgGroup := FVal(GetValue('/MG=', MiscData, false));

  if OnlyGroup then EXIT;

  If TempBool then
    begin
      GlobDoingCombined := false;
      if NOT ScriptMessageAreaChange(AreaTemp, CheckGroup) then
        begin
          ShowMsgAreaNewMail(MiscData, False, True, False);

          AreaTemp := '';
          Write('`A15:' , LangObj^.ralGet(ralSelArea));
          GetString(AreaTemp, 5, ['0'..'9'], False, False, False, False);
        end; { if }

      If FVal(AreaTemp)=00 then Exit;

      GetMessageRecord(Messageinf, FVal(AreaTemp), False);
      If (NOT CheckMsgAreaAccess(MessageInf, CheckGroup, True,
                                 LineCfg^.Exitinfo^.Userinfo.MsgGroup,
                                 LineCfg^.Exitinfo^.userinfo)) then
        begin;
          WriteLn;
          WriteLn('`A12:', LangObj^.ralGet(ralInvArea));
          InputObj^.PressEnter(True, False);
          EXIT;
        end; { CheckGroupAccess }

    end; { if }

  LineCfg^.Exitinfo^.Userinfo.MsgArea := MessageInf.AreaNum;

  OutputObj^.SetStopMore(False);
  OutputObj^.ResetLines(01);
  Writeln;
  WriteLn;
  OutputObj^.ResetLines(01);
end; { proc. ChangeMessageArea }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SelectCombined(MiscData: String);
var TempBool  : Boolean;
    AreaTemp  : String;
    TempNum   : Longint;
    MessageInf: MessageRecord;
    CheckGroup: Boolean;
    DoAbort   : Boolean;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logAreas, 'SelectCombined: '+MiscData+ ' (BEGIN)');
  {$ENDIF}

  TempBool := True;
  DoAbort := false;
  MessageInf.AreaNum := LineCfg^.Exitinfo^.Userinfo.Msgarea;
  CheckGroup := (Pos('/MG', SUpcase(MiscData)) > 00);

  if (Pos('/MG=0', SUpcase(MiscData))>00) OR (LineCfg^.Exitinfo^.Userinfo.MsgGroup=0) then
    begin
      TempBool := ChangeGroup(True, MiscData);
    end
     else if (Pos('/MG=', SUpCase(MiscData)) > 00) then
            LineCfg^.Exitinfo^.Userinfo.MsgGroup := FVal(GetValue('/MG=', MiscData, false));

  If TempBool then
    begin
      REPEAT
        GlobDoingCombined := true;
        if NOT ScriptMessageCombined(AreaTemp, CheckGroup) then
          begin
            ShowMsgAreaNewMail(MiscData, False, False, True);

            AreaTemp := '';
            Write('`A15:' , LangObj^.ralGet(ralTogglArea));

            GetString(AreaTemp, 80, ['0'..'9', #32], false, false, false, false);
          end; { if }

        if Trim(AreaTemp) = '' then DoAbort := true;

        While AreaTemp <> '' do
          begin
            TempNum := FVal(FirstWord(AreaTemp, DefExtractWord, true));

            GetMessageRecord(MessageInf, TempNum, false);
            ToggleCombinedArea(MessageInf.AreaNum);
          end; { while }

        OutputObj^.SetStopMore(False);
        OutputObj^.ResetLines(01);
        LineCfg^.DispMorePrompt := true;
      UNTIL (DoAbort);
    end; { if }

  OutputObj^.SetStopMore(False);
  OutputObj^.ResetLines(01);
  Writeln;
  WriteLn;
  OutputObj^.ResetLines(01);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logAreas, 'SelectCombined: '+MiscData+ ' ( END )');
  {$ENDIF}
end; { proc. SelectCombined }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowGroupList(Message: Boolean; MiscData: String);
var NextIsEnter: Boolean;
    OneLiner   : Boolean;
    Group_F    : pFileObj;
    GroupInf   : GroupRecord;
    FileName   : String;
    TotalItems : Longint;
    Counter    : Longint;
    LineLen    : Byte;
    NumRead    : NumreadType;
begin
  OneLiner := Pos('/1', MiscData)>00;
  If Message then WriteLn('`A15:', LangObj^.ralGet(ralMGroups))
   else Writeln('`A15:', LangObj^.ralGet(ralFleGroup));
  WriteLn;
  WriteLn;

  if Message then FileName := MGroupsFileName
    else FileName := FGroupsFileName;

  New(Group_F, Init);
  Group_F^.Assign(FileName);
  Group_F^.FileMode := ReadMode + DenyNone;
  if NOT Group_F^.Open(1) then
    begin
      Dispose(Group_F, Done);
      EXIT;
    end; { if }

  Counter    := 00;
  TotalItems := Group_F^.FileSize div SizeOf(GroupRecord);
  NextIsEnter:= False;

  While (Counter < TotalItems) AND (Totalitems>00) AND (NOT OutputObj^.StopMore) do
    begin
      If OneLiner then NextIsEnter := True;

      NumRead := Group_F^.BlkRead(GroupInf, SizeOf(GroupRecord));
      Inc(Counter);

      if (CheckGroupAccess(GroupInf, LineCfg^.Exitinfo^)) then
        begin
          if OneLiner then LineLen := 40 else LineLen := 30;

          If NOT NextIsEnter then
           Write('`A14:', Counter:3, '`A7: ... `A3:', NoColorCopy(GroupInf.Name, 1, LineLen), '`X40:')
               else
           WriteLn('`A14:', Counter:3, '`A7: ... `A3:', NoColorCopy(GroupInf.Name, 1, LineLen));

           NextIsEnter := NOT NextIsEnter;
        end; { CheckgroupAccess }

    end; { WHILE }

  If NextIsEnter then WriteLn;

  Dispose(Group_F, Done);
end; { proc. ShowGroupList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GroupHook(RecordNum: Longint; var Answers: StringArrayObj; StartRecord: Longint;
                    GoingDown: Boolean; var GlobalFile: pFileObj);
var GroupInf   : GroupRecord;
    NumRead    : NumreadType;
    GroupNrStr : String;
    EndOfFile  : Boolean;
begin
  GlobalFile^.Seek((Recordnum-1) * SizeOf(GroupRecord));

  repeat
    if (GlobalFile^.BlkRead(GroupInf, SizeOf(GroupRecord)) = 0) then
      FillChar(GroupInf, SizeOf(GroupRecord), #00);

    if (NOT CheckGroupAccess(GroupInf, LineCfg^.Exitinfo^)) then GroupInf.Name := '';

    if GoingDown then
      EndOfFile := GlobalFile^.FilePos >= GlobalFile^.FileSize
       else EndOfFile := GlobalFile^.FilePos <= 0;

    if GroupInf.Name = '' then
    if NOT GoingDown then
      GlobalFile^.Seek(GlobalFile^.FilePos - (SizeOf(GroupRecord) * 2));

  until (GroupInf.Name <> '') OR (EndOfFile);

  GroupNrStr := FStr(GroupInf.AreaNum);
  if GroupInf.Name = '' then GroupNrStr := '0';

  Answers.Put(StartRecord, FStr((GlobalFile^.FilePos DIV SizeOf(GroupRecord))));
  Answers.Put(StartRecord + 01, GroupNrStr);
  Answers.Put(StartRecord + 02, GroupInf.Name);
end; { proc. GroupHook }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ScriptGroupChange(var GroupTemp: String; IsMessage: Boolean): Boolean;
var Quest      : QuestObj;
    FName      : String;
    TotalGroups: Longint;
    GroupInf   : GroupRecord;
    Numread    : NumreadType;
    BeforeRec  : Longint;
    GroupNrStr : String;
    GlobalFile : pFileObj;
begin
  ScriptGroupChange := false;
  TotalGroups := 0;
  BeforeRec := 0;

  if IsMessage then FName := MGroupsFileName else
      FName := FGroupsfileName;

  New(GlobalFile, Init);
  GlobalFile^.Assign(FName);
  GlobalFile^.FileMode := ReadMode + Denynone;
  if NOT GlobalFile^.Open(1) then
    begin
      Dispose(GlobalFile, Done);
      EXIT;
    end; { if }

  {-------------------- Count the number of records available --------------}
  repeat
    if (GlobalFile^.BlkRead(GroupInf, SizeOf(GroupRecord)) = 0) then
      FillChar(GroupInf, SizeOf(GroupRecord), #00);

    if (NOT CheckGroupAccess(GroupInf, LineCfg^.Exitinfo^)) then GroupInf.Name := '';

    if GroupInf.Name <> '' then
      Inc(TotalGroups);

    if IsMessage then
     if LineCfg^.Exitinfo^.Userinfo.MsgGroup = GroupInf.AreaNum then
       BeforeRec := TotalGroups;

    if NOT IsMessage then
     if LineCfg^.Exitinfo^.Userinfo.FileGroup = GroupInf.AreaNum then
       BeforeRec := TotalGroups;

  until (GlobalFile^.FilePos >= GlobalFile^.FileSize);
  {-------------------- Count the number of records available --------------}
  if IsMessage then
    begin
      if LineCfg^.Exitinfo^.Userinfo.MsgGroup = 0 then GroupNrStr := '0'
        else GroupNrStr := FStr(Ra250Group(LineCfg^.Exitinfo^.Userinfo.MsgGroup, true))
    end
      else begin
             if LineCfg^.Exitinfo^.Userinfo.FileGroup = 0 then GroupNrStr := '0'
               else GroupNrStr := FStr(Ra250Group(LineCfg^.Exitinfo^.Userinfo.FileGroup, false));
           end; { else }

  Quest.Init;
  Quest.QInfo^.Answers^.Put(01, FStr(TotalGroups));
  Quest.QInfo^.Answers^.Put(02, GroupNrStr);
  Quest.QInfo^.Answers^.Put(03, FStr(BeforeRec));
  Quest.QInfo^.GetInfoFile := GlobalFile;
  Quest.QInfo^.GetInfoHook := {$IFDEF FPC}@{$ENDIF}GroupHook;

  if IsMessage then
    Quest.Process('MG-CHNG /N', false, '')
     else Quest.Process('FG-CHNG /N', false, '');

  GroupTemp := Quest.GetReturnResult;
  ScriptGroupChange := (Quest.GetError = 0);

  Quest.Done;

  Dispose(GlobalFile, Done);
end; { func. ScriptGroupChange }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ChangeGroup(Message: Boolean; MiscData: String): Boolean;
var ShowList : Boolean;
    GroupTemp: String;
    GroupInf : GroupRecord;
begin;
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logAreas, 'Changegroup');
  {$ENDIF}

  ChangeGroup := False;
  ShowList    := True;

  if NOT ScriptGroupChange(GroupTemp, Message) then
      begin
        if Message then
          if DisplayHotFile('MGROUPS', []) <> #01 then ShowList := False;
        if NOT Message then
          if DisplayHotFile('FGROUPS', []) <> #01 then ShowList := False;

        if ShowList then
          begin
            OutputObj^.ClearScreen;
            WriteLn;
            WriteLn('`A15:');

            ShowGroupList(Message, MiscData);
          end; { if }

          WriteLn;
          GroupTemp := '';
          Write('`A15:' , LangObj^.ralGet(ralSelGroup));
          GetString(GroupTemp, 5, ['0'..'9'], False, False, False, False);
      end; { if }


  OutputObj^.ResetLines(01);
  OutputObj^.SetStopMore(False);

  If FVal(Grouptemp)=00 then Exit;
  GetGroupRecord(FVal(GroupTemp), False, Message, GroupInf);

  If (NOT CheckGroupAccess(GroupInf, LineCfg^.Exitinfo^)) then
        begin;
          WriteLn;
          WriteLn('`A12:', LangObj^.ralGet(ralInvGrNo));
          InputObj^.PressEnter(True, False);
          EXIT;
        end; { CheckGroupAccess }

  If Message then LineCfg^.Exitinfo^.Userinfo.MsgGroup := GroupInf.AreaNum
    else LineCfg^.Exitinfo^.Userinfo.FileGroup := GroupInf.AreaNum;

  if Message then LineCfg^.Exitinfo^.Userinfo.MsgArea := SearchNextMsgArea(GroupInf.AreaNum)
    else LineCfg^.Exitinfo^.Userinfo.FileArea := SearchNextFileArea(GroupInf.AreaNum);

  OutputObj^.ResetLines(01);
  WriteLn;
  WriteLn;
  ChangeGroup := True;
end; { func. ChangeGroup }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
procedure ClearCombined(MiscData: String);
var KeySet : String;
    TempCH : Char;
    Counter: Longint;
begin
  WriteLn;
  Write('`A10:', langObj^.ralGetStr(ralSelDesel));

  KeySet := SUpCase(LangObj^.ralGetKeys(ralSelDesel));

  REPEAT
    TempCH := UpCase(InputObj^.ReadKey);
  UNTIL Pos(TempCH, KeySet) > 0;


  WriteLn;
  if Pos(TempCH, KeySet) in [1, 2] then
    begin
      WriteLn('`A12:');
      if NOT InputObj^.ralStrYesNoAsk(ralVerTurn) then
        TempCH := KeySet[3];
    end; { if }

  Case Pos(TempCH, KeySet) of
   { Select }   1 : SelectAllCombined(LineCfg^.Exitinfo^.Userinfo.CombinedInfo);
   { Deselect } 2 : for Counter := 1 to 200 do
                      LineCfg^.Exitinfo^.Userinfo.CombinedInfo[Counter] := 0;
   { Quit }     3 : ;
  end; { case }
end; { proc. ClearCombined }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function SearchCombined(AreaNum: Longint): Boolean;
var Counter: Longint;
begin
  SearchCombined := false;

  for Counter := 01 to 200 do
    if LineCfg^.Exitinfo^.Userinfo.CombinedInfo[Counter] = AreaNum then
      begin
        SearchCombined := true;
        BREAK;
      end; { if }
end; { func. SearchCombined }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure ToggleCombinedArea(AreaNum: Longint);
var Counter: Longint;
begin
  for Counter := 01 to 200 do
    begin
      if LineCfg^.Exitinfo^.Userinfo.CombinedInfo[Counter] = AreaNum then
        begin
          LineCfg^.Exitinfo^.Userinfo.CombinedInfo[Counter] := 0;
          EXIT;
        end; { if }
    end; { for }

  for Counter := 01 to 200 do
    begin
      if LineCfg^.Exitinfo^.Userinfo.CombinedInfo[Counter] = 0 then
        begin
          LineCfg^.Exitinfo^.Userinfo.CombinedInfo[Counter] := AreaNum;
          EXIT;
        end; { if }
    end; { for }
end; { proc. ToggleCombinedArea }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

end.
