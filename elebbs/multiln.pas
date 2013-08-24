unit MultiLn;
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
** MULTILINE Routines for EleBBS
**
** Copyright (c) 1996 - 1999 by Maarten Bekers
**
** Created : 15-Nov-1997
** Last update : 02-Oct-1999
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Global, CfgRec, FileObj;

type tMultiLnObj = Object
         UserOn     : ^UseronRecord;                  { USERON.BBS contents }

         {-- Constructors and destructors ----------------------------------}
         constructor Init;
         destructor Done;

         {-- Generic routines ----------------------------------------------}
         {$IFDEF WITH_FULL}
           procedure ShowUsersOnline(MiscData: String; AddCR: Boolean);  { Show users online }
           procedure BBSSendMessage(LineNr : Longint; MiscData: String);{ Send message to other node }
           procedure SendInteractiveMsg(LineNr : Longint; Msg: String);
           function  ShareIns : Boolean;                { Is Share Installed? }
         {$ENDIF}

         procedure KillFromUseron;
         procedure WriteUserOn(StatDesc: String; Status: Longint);
         function  CheckOkToSend(LineNr: Longint; var UseronInf:UseronRecord; ShowUser: Boolean): Boolean;
         function  DoubleUseron(Name: String): Boolean;
         function  GetUserOn(Name: String; EleWEBOnly, UpdateGlob: Boolean): Longint;
         function  EmptyNodeNr(IsEleWEB: Boolean): Longint;

         {-- Private routines ----------------------------------------------}
         private
           {$IFDEF WITH_FULL}
           function ScriptWhosOnline(const AddCR: Boolean): Boolean;
           {$ENDIF}
     end; { tMultiLnObj }

type pMultiLnObj = ^tMultiLnObj;


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
 uses Dos, GenDos, FileRout, MkFile, StUtils, Ral, LineEd, IntEdit, ElLog_U,
         InOut_U, Support, BitWise, Colors, Cases, LongStr, CentrStr, UnixDate,
         WordStr, CfgFile, MemMan, Input_U, StrUnit, Question, Ranges, ObjDec
         {$IFDEF WITH_DEBUG}
           ,Debug_U
         {$ENDIF};
{$ELSE}
  uses Debug_U, Jdates, Longstr, Cases, StUtils, Bitwise,
       FileRout, CentrStr, StrPath, CfgFile, Ranges, UnixDate,
       ElLog_U, WordStr, ObjDec, RAL, MemMan;
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function UseronStatusStr(const Useron: UseronRecord): String;
begin
  UseronStatusStr := '';

  case Useron.Status of
      0 : UseronStatusStr := LangObj^.ralGet(ralBrowsing);
      1 : UseronStatusStr := LangObj^.ralGet(ralXFer);
      2 : UseronStatusStr := LangObj^.ralGet(ralMessages1);
      3 : UseronStatusStr := LangObj^.ralGet(ralDoor);
      4 : UseronStatusStr := LangObj^.ralGet(ralSysOpChat);
      5 : UseronStatusStr := LangObj^.ralGet(ralQuestnr);
      6 : UseronStatusStr := LangObj^.ralGet(ralConf);
      7 : UseronStatusStr := LangObj^.ralGet(ralLogingOn);
    255 : UseronStatusStr := Useron.StatDesc
      else UseronStatusStr := LangObj^.ralGet(ralUnknown);
  end; { Case }
end; { func. UseronStatusStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
procedure WhosOnlineHook(RecordNum: Longint; var Answers: StringArrayObj; StartRecord: Longint;
                         GoingDown: Boolean; var GlobalFile: pFileObj);
var UseronInf: UseronRecord;
    EndOfFile: Boolean;
    LineStr  : String;
begin
  GlobalFile^.Seek((Recordnum - 01) * SizeOf(UseronRecord));

  repeat
    if GlobalFile^.BlkRead(UserOnInf, SizeOf(UseronRecord)) = 0 then
      FillChar(UseronInf, SizeOf(UseronRecord), #00);

    if UseronInf.Name <> '' then
     if (ReadBit(UseronInf.Attribute, 00)) OR ((UserOnInf.Line <= 0) AND (UserOnInf.NodeNumber <= 0)) then
       UseronInf.Name := '';

    if GoingDown then
      EndOfFile := GlobalFile^.FilePos >= GlobalFile^.FileSize
       else EndOfFile := GlobalFile^.FilePos <= 0;

    if Useroninf.Name = '' then
      begin
        FillChar(UserOnInf, SizeOf(UserOnInf), #0);

        if NOT GoingDown then
          GlobalFile^.Seek(GlobalFile^.FilePos - (SizeOf(UseronRecord) * 2));
      end; { if }

  until (UseronInf.Name <> '') OR (EndOfFile);

  if UserOnInf.NodeNumber <> 0 then LineStr := FStr(UserOnInf.NodeNumber)
    else LineStr := FStr(UserOnInf.Line);

  Answers.Put(StartRecord, UseronInf.Name);
  Answers.Put(StartRecord + 01, UseronInf.Handle);
  Answers.Put(StartRecord + 02, LineStr);
  Answers.Put(StartRecord + 03, FStr(UseronInf.Baud));
  Answers.Put(StartRecord + 04, UseronInf.City);
  Answers.Put(StartRecord + 05, UseronStatusStr(UserOnInf));
  Answers.Put(StartRecord + 06, FStr(UserOnInf.NoCalls));
  Answers.Put(StartRecord + 07, FStr(GlobalFile^.FilePos DIV SizeOf(UseronRecord)));
end; { proc. WhosOnlineHook }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tMultiLnObj.Init;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMain, 'allocating memory for useron');
  {$ENDIF}
  AllocMem(Useron, SizeOf(UseronRecord), 'Useron', 'InitVariables');

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMain, 'filling useron');
  {$ENDIF}
  FillChar(UserOn^, SizeOf(UserOnRecord), #00);
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tMultiLnObj.Done;
begin
 {!!!  ReleaseMem(UserOn, SizeOf(UseronRecord)); }
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
procedure tMultiLnObj.SendInteractiveMsg(LineNr: Longint; Msg: String);
var MsgPath   : String;
    NodeMsg_F : Text;
begin
  If GlobalCfg^.RaConfig^.SemPath <> '' then MsgPath := GlobalCfg^.RaConfig^.SemPath
    else MsgPath := GlobalCfg^.RaConfig^.SysPath;

  Assign(NodeMsg_F, MsgPath + 'NODE'+FStr(LineNr)+'.RA');
  {$i-} Append(NodeMsg_F); {$I+}
  if IOResult>00 then
    begin
      {$i-} System.ReWrite(NodeMsg_F); {$I+}
      if IOResult>00 then EXIT;
    end; { if }

  {$i-}
    WriteLn(NodeMsg_F);
    WriteLn(NodeMsg_F);
    WriteLn(NodeMsg_F, Msg);
    WriteLn(NodeMsg_F);
    WriteLn(NodeMsg_F, ^K, ']258',^A);
    Close(NodeMsg_F);
  {$i-}
  If IOResult>00 then ;
end; { proc. SendInterActiveMsg }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
procedure tMultiLnObj.BBSSendMessage(LineNr: Longint; MiscData: String);  { Send message to other node }
var LineChosen : String;
    MsgHdr     : String;
    Msglines   : pStringArrayObj;
    TmpUseron  : UseronRecord;
    MsgLinesNr : Longint;
    MsgPath    : String;
    SenderName : String;
    NameToUse  : String;

    Counter    : Longint;
    NodeMsg_F  : Text;
begin
  If LineNr in [0, 255] then
   ShowUsersOnline(MiscData, False)
    else begin
           Writeln;
           Writeln;
           LineChosen := FStr(LineNr);
         end; { else }

  if LineNr = 0 then
    begin
      WriteLn;
      Write('`A11:', LangObj^.ralGet(ralChUser));

      LineChosen := '';
      GetString(LineChosen, 6, ['0'..'9'], false, false, false, false);

      WriteLn;
      if LineChosen = '' then EXIT;
    end; { if }

 If NOT CheckOkToSend(FVal(LineChosen), TmpUserOn, true) then Exit;

 if Pos('/H', SUpCase(MiscData)) > 0 then
   NameToUse := TmpUserOn.Handle
     else NameToUse := TmpUserOn.Name;


 MsgHdr := LangObj^.ralGet(ralMsgTo) + ' ' + NameToUse + ' '+
           LangObj^.ralGet(ralNode2) + ' ' + FStr(FVal(LineChosen));

  New(MsgLines, Init(30));

  MsgLinesNr:=20;
  RaAlikeEditor(MsgHdr, MsgLinesNr, MsgLines^, 00);

 If MsgLinesNr=-1 then WriteLn(#13#10, LangObj^.ralGet(ralAborted), #13#10)
  else begin
         if Pos('/H', SUpCase(MiscData)) > 0 then
           SenderName := LineCfg^.Exitinfo^.Userinfo.Handle
             else Sendername := LineCfg^.Exitinfo^.Userinfo.Name;

         Write(#13#10, LangObj^.ralGet(ralProcess));

         RaLog('>', 'Sent on-line message to '+TmpUseron.Name+' (Line '+FStr(TmpUseron.Line)+')');

         If GlobalCfg^.RaConfig^.SemPath <> '' then MsgPath := GlobalCfg^.RaConfig^.SemPath
            else MsgPath := GlobalCfg^.RaConfig^.SysPath;

         Assign(NodeMsg_F, MsgPath + 'NODE'+FStr(FVal(LineChosen))+'.RA');
         {$i-} Append(NodeMsg_F); {$I+}
         If IOResult>00 then begin
                               {$i-} System.ReWrite(NodeMsg_F); {$I+}
                               If IOResult>00 then
                                 begin
                                   Dispose(MsgLines, Done);
                                   Exit;
                                 end; { if }
                             end; { NodeMsg }

        {$i-}
          if (TmpUserOn.NodeNumber >= WebNodeBase) then
            begin
              WriteLn(NodeMsg_F, ^A, ^A, ^A, '"', SenderName, '"', ' ', LineCfg^.RaNodeNr);
              for Counter := 01 to MsgLinesNr do
                 WriteLn(NodeMsg_F, MsgLines^.Get(Counter));
              WriteLn(NodeMsg_F);
            end
              else begin
                     WriteLn(NodeMsg_F);
                     WriteLn(NodeMsg_F);

                     WriteLn(NodeMsg_F, ^K, ']497 ', SenderName, ' ', ^K, ']496 ', LineCfg^.RaNodeNr,':');
                     for Counter := 01 to MsgLinesNr do
                        WriteLn(NodeMsg_F, MsgLines^.Get(Counter));

                     WriteLn(NodeMsg_F);
                     WriteLn(NodeMsg_F, ^K, ']258',^A);
                   end; { else }

          Close(NodeMsg_F);
        {$i-}
        If IOResult>00 then ;

        WriteLn(LangObj^.ralGet(ralMsgSent), #13#10);
       end;

  InputObj^.PressEnter(False, True);
  Dispose(MsgLines, Done);
end; { proc. SendMessage }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tMultiLnObj.ShareIns : Boolean;
var Comspec_F: pFileObj;
    OldFMode: Longint;
begin
  New(Comspec_F, Init);

  Comspec_F^.Assign(GetEnv('COMSPEC'));
  Comspec_F^.FileMode := ReadWriteMode + DenyNone;
  Comspec_F^.Open(1);

  ShareIns := (shLock(Comspec_F, 1, 1) = 00);
  shUnlock(Comspec_F, 1, 1);

  Dispose(Comspec_F, Done);
end; { func. ShareIns }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tMultiLnObj.ScriptWhosOnline(const AddCR: Boolean): Boolean;
var Quest        : QuestObj;
    TotalRecords : Longint;
    UserOnInf    : UseronRecord;
    GlobalFile   : pFileObj;
begin
  ScriptWhosOnline := false;
  TotalRecords := 00;

  New(GlobalFile, Init);
  GlobalFile^.Assign(GlobalCfg^.RaConfig^.SysPath + 'useron.bbs');
  GlobalFile^.FileMode := ReadMode + DenyNone;
  if NOT GlobalFile^.Open(1) then
    begin
      Dispose(GlobalFile, Done);
      EXIT;
    end; { if }

  {------------------- Count the number of records in this file -------------}
  repeat
    if GlobalFile^.BlkRead(UserOnInf, SizeOf(UseronRecord)) = 0 then
      FillChar(UseronInf, SizeOf(UseronRecord), #00);

    if UseronInf.Name <> '' then
     if (NOT ReadBit(UseronInf.Attribute, 00)) then
      if (UseronInf.Line > 00) OR (UserOnInf.NodeNumber > 0) then
       if UseronInf.Name <> '' then Inc(TotalRecords);

  until (GlobalFile^.FilePos >= GlobalFile^.FileSize);
  {------------------- Count the number of records in this file -------------}


  Quest.Init;
  Quest.QInfo^.Answers^.Put(01, FStr(TotalRecords));
  if AddCR then
    Quest.QInfo^.Answers^.Put(02, 'YES')
     else Quest.QInfo^.Answers^.Put(02, 'NO');

  Quest.QInfo^.GetInfoFile := GlobalFile;
  Quest.QInfo^.GetInfoHook := {$IFDEF FPC}@{$ENDIF}WhosOnlineHook;
  Quest.Process('WHONLINE /N', false, '');
  ScriptWhosOnline := (Quest.GetError = 0);
  Quest.Done;

  Dispose(GlobalFile, Done);
end; { func. ScriptWhosOnline }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tMultiLnObj.ShowUsersOnline(MiscData: String; AddCR: Boolean);
var UseronF    : pFileObj;
    UserOnInf  : UserOnRecord;
    DoView     : Boolean;
    DoIng      : String;
    NameToShow : String;
    LineNr     : String;
begin
  if NOT ScriptWhosOnline(AddCR) then
    begin
      OutputObj^.ClearScreen;
      WriteLn;
      WriteLn('`A15:',CenterJust(LangObj^.ralGet(ralOnline)+' '+GlobalCfg^.RaConfig^.Systemname, 79, False));
      Writeln('`A12:',CenterJust(Dup('Ä', NoColorLength(LangObj^.ralGet(ralOnline)+' '+
               GlobalCfg^.RaConfig^.Systemname)),79,False));
      WriteLn;
      WriteLn('`A10:',LangObj^.ralGet(ralOnlineHdr));
      Writeln('`A02:',OutputObj^.AvatarDupl('Ä', 79));
      LineCfg^.DispMorePrompt := ReadBit(LineCfg^.Exitinfo^.Userinfo.Attribute, 2);

      if NOT FileExist(GlobalCfg^.RaConfig^.Syspath + 'useron.bbs') then
           EXIT;

      if Config_OpenFile(UseronF, GlobalCfg^.RaConfig^.SysPath+'useron.bbs', 01, ReadMode + DenyNone, False, False)>00 then
        begin
          Config_DoneFile(UserOnF);
          EXIT;
        end; { if }

      While NOT UserOnF^.EOF do
       begin
         FillChar(UserOnInf, SizeOf(UseronInf), #00);
         DoIng := '';

         Config_ReadFile(UserOnF, UserOnInf, SizeOf(UserOnRecord));
         Doing := UseronStatusStr(UserOnInf);

        DoView := NOT ReadBit(UseronInf.Attribute, 00);
        if DoView then
          begin
            if UseronInf.NodeNumber > 0 then
              begin
                DoView := UseronInf.NodeNumber > 0;
                LineNr := FStr(UseronInf.NodeNumber);
              end
                else begin
                       DoView := UserOnInf.Line > 00;
                       LineNr := FStr(UseronInf.Line)
                     end; { if }
          end; { if }

        if DoView then DoView := UserOnInf.Name<>'';

        If Pos('/H', SupCase(MiscData)) > 00 then NameToShow := UseronInf.Handle
         else NameToShow := UseronInf.Name;

        if Length(NameToShow) > 30 then
          Delete(NameToShow, 30, 255);

        if Length(UseronInf.City) > 21 then
          Delete(UseronInf.City, 21, 255);

        If DoView then
         With UseronInf do
          WriteLn('`X01:`A11:', NameToShow,
                  '`X31:`A15:', LineNr,
                  '`X38:`A15:', UseronInf.Baud,
                  '`X48:`A15:', Doing,
                  '`X59:`A14:', UseronInf.City);
       end; { While }

      Config_DoneFile(UserOnF);

      WriteLn;
      If AddCR then InputObj^.PressEnter(True, True);
    end; { if }
end; { proc. ShowUsersOnline }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)
{$ENDIF}

function tMultiLnObj.CheckOkToSend(LineNr: Longint; var UseronInf: UseronRecord; ShowUser: Boolean): Boolean;
var UseronF : pFileObj;
    DoView  : Boolean;
{$IFNDEF FPC}
{$IFNDEF WIN32}
 {$IFNDEF OS2}
    Result  : Boolean;
 {$ENDIF}
{$ENDIF}
{$ENDIF}
begin
  FillChar(UseronInf, SizeOf(UseronRecord), #00);
  CheckOkToSend := False;
  Result := false;

  if Config_OpenFile(UserOnF, GlobalCfg^.RaConfig^.Syspath + 'useron.bbs', 01, ReadMode + DenyNone, False, False)>00  then
    begin
      Config_DoneFile(userOnF);
      EXIT;
    end; { if }
  Config_SeekFile(UseronF, Longint(SizeOf(UseronRecord)) * Longint(Pred(LineNr)));
  Config_ReadFile(UseronF, UseronInf, SizeOf(UseronRecord));

  if (UseronInf.Line = LineNr) OR (UseronInf.NodeNumber = LineNr) then
    begin
      Result := true;

      if UseronInf.Name = '' then Result := false else
      if ReadBit(UseronInf.Attribute, 00) then Result := false else
      if ReadBit(UseronInf.Attribute, 03) then
        begin
          {$IFDEF WITH_FULL}
            if ShowUser then
              begin
                Writeln('`A12:',FirstWord(UseronInf.Name, defExtractWord, false), ' ', LangObj^.ralGet(ralNoDisturb));
                Result := False;

                if (SUpCase(LineCfg^.Exitinfo^.Userinfo.Name) = SUpcase(GlobalCfg^.RaConfig^.Sysop)) OR
                    (SUpCase(LineCfg^.Exitinfo^.Userinfo.Handle) = SUpcase(GlobalCfg^.RaConfig^.Sysop)) then
                  begin
                    WriteLn;
                    Result := InputObj^.ralStrYesNoAsk(ralSysOverr);
                  end; { SysOp OverRide }

                if NOT Result then InputObj^.PressEnter(False, True);
              end
                else begin
                       Result := FALSE;
                     end; { else }
          {$ENDIF}

          Config_DoneFile(UserOnF);

          CheckOkToSend := Result;
          EXIT;
        end { if }
         else begin
                Result := true;
              end;
    end; { if }

 {$IFDEF WITH_FULL}
  If NOT Result then
   if ShowUser then
     begin
       Writeln(LangObj^.ralGet(ralNoUser2));
       InputObj^.PressEnter(False, True);
     end; { NOT Result }
 {$ENDIF}

  Config_DoneFile(UserOnF);
{!!  If NOT Result then InputObj^.PressEnter(False, True);}

  CheckOkToSend := Result;
end; { func. CheckOkToSend }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function tMultiLnObj.GetUserOn(Name: String; EleWebOnly, UpdateGlob: Boolean): Longint;
var UserOnF  : pFileObj;
    UserOnInf: UseronRecord;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'GetUserOn');
  {$ENDIF}

  FillChar(UseronInf, SizeOf(UseronInf), #00);
  if UpdateGlob then
    UserOn^ := UseronInf;
  GetUserOn := -1;

  if Config_OpenFile(UserOnF, GlobalCfg^.RaConfig^.Syspath + 'useron.bbs', 01, ReadMode + DenyNone, False, False)>00 then
    begin
      Config_DoneFile(UserOnF);
      EXIT;
    end; { if }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'GetUserOn - Size: '+FStr(UseronF^.FileSize));
    DebugObj.DebugLog(logString, 'GetUserOn - Pos.: '+FStr(UseronF^.FilePos));
  {$ENDIF}

  if EleWebOnly then
    Config_SeekFile(UserOnF, Pred(WebNodeBase) * SizeOf(UserOnRecord));

  While NOT UserOnF^.EOF do
   begin
     Config_ReadFile(UserOnF, UserOnInf, SizeOf(UseronRecord));

     if (SUpCase(Trim(UseronInf.Name))=SUpcase(Name)) OR
         (SUpCase(Trim(UseronInf.Handle))=SUpcase(Name)) then
      if ((NOT ReadBit(UseronInf.Attribute, 00)) OR (EleWEBOnly)) then
       if NOT ReadBit(UseronInf.Attribute, 03) then
          begin
            GetUserOn := UserOnF^.FilePos div Sizeof(UseronRecord);

            if UpdateGlob then
              UserOn^ := UseronInf;
            BREAK;
          end; { if }
   end; { While }

  Config_DoneFile(UserOnF);
end; { func. GetUserOn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

Function tMultiLnObj.DoubleUseron(Name: String): Boolean;
var UseronF  : pFileObj;
    DoView   : Boolean;
{$IFNDEF FPC}
{$IFNDEF WIN32}
 {$IFNDEF OS2}
    Result   : Boolean;
 {$ENDIF}
{$ENDIF}
{$ENDIF}
    UseronInf: UseronRecord;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'DoubleUserOn');
  {$ENDIF}

  FillChar(UseronInf, SizeOf(UseronRecord), #00);
  Result := False;
  DoubleUserOn := Result;

  if Config_OpenFile(UserOnF, GlobalCfg^.RaConfig^.Syspath + 'useron.bbs', 01, ReadMode + DenyNone, False, False)>00 then
    begin
      Config_DoneFile(UserOnF);
      EXIT;
    end; { if }

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'DoubleUserOn - Size: '+FStr(UseronF^.FileSize));
    DebugObj.DebugLog(logString, 'DoubleUserOn - Pos.: '+FStr(UseronF^.FilePos));
  {$ENDIF}

  While NOT UserOnF^.EOF do
   begin
     Config_ReadFile(UserOnF, UserOnInf, SizeOf(UseronRecord));

     if SUpCase(Trim(UseronInf.Name))=SUpcase(Trim(Name)) then
      if (UseronInf.Line <> LineCfg^.RaNodeNr) AND (UseronInf.NodeNumber < WebNodeBase) then
        Result := True;
   end; { While }

  Config_DoneFile(UserOnF);
  DoubleUseron := Result;
end; { func. DoubleUserOn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function tMultiLnObj.EmptyNodeNr(IsEleWEB: Boolean): Longint;
var UserOnF  : pFileObj;
    UserOnInf: UseronRecord;
begin
  FillChar(UseronInf, SizeOf(UseronRecord), #00);
  if IsEleWeb then
    EmptyNodeNr := WebNodeBase
      else EmptyNodeNr := 1;


  if Config_OpenFile(UserOnF, GlobalCfg^.RaConfig^.Syspath + 'useron.bbs', 01, ReadMode + DenyNone, False, False)>00 then
    begin
      Config_DoneFile(UserOnF);
      EXIT;
    end; { if }

  EmptyNodeNr := Succ(UserOnF^.FileSize DIV SizeOf(UseronRecord));
  if IsEleWeb then
    begin
      EmptyNodeNr := Succ(Max(WebNodeBase, UserOnF^.FileSize DIV SizeOf(UseronRecord)));
      Config_SeekFile(UserOnF, Pred(WebNodeBase) * SizeOf(UseronRecord));
    end; { if }

  While NOT UserOnF^.EOF do
   begin
     Config_ReadFile(UserOnF, UserOnInf, SizeOf(UseronRecord));

     if (Trim(UseronInf.Name) = '') OR ((UseronInf.Line = 0) AND (UseronInf.NodeNumber = 0)) then
       begin
         EmptyNodeNr := UserOnF^.FilePos DIV SizeOf(UserOnRecord);
         BREAK;
       end; { if }
   end; { While }

  Config_DoneFile(UserOnF);
end; { func. EmptyNodeNr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure tMultiLnObj.WriteUserOn(StatDesc: String; Status: Longint);
var UseronF     : pFileObj;
    SaveStatDesc: String;

procedure CreateSeekFile(var F: pFileObj; SeekTo: Longint);
var Counter: Longint;
begin
  F^.Seek(F^.FileSize);

  While (F^.FileSize < SeekTo) do
    begin
      {$i-} F^.BlkWrite(UserOn^, SizeOf(UseronRecord)); {$i+}
      if F^.IOResult>00 then BREAK;
    end; { while }
end; { proc. CreateSeekFile }

begin
  if (UserOn = nil) OR (LineCfg^.Exitinfo^.Userinfo.Name='') then EXIT;
  FillChar(UserOn^, SizeOf(UseronRecord), #00);
  SaveStatDesc := UserOn^.StatDesc;

  if Config_OpenFile(UseronF, GlobalCfg^.RaConfig^.SysPath + 'useron.bbs', 01, ReadWriteMode + DenyNone, True, false) > 0 then
    begin
      Config_DoneFile(UserOnF);
      EXIT;
    end; { if }

  CreateSeekFile(UseronF, Longint(SizeOf(UseronRecord)) * Longint(Pred(LineCfg^.RaNodeNr)));
  Config_ReadFile(UseronF, UserOn^, SizeOf(UseronRecord));
  Config_SeekFile(UseronF, Longint(SizeOf(UseronRecord)) * Longint(Pred(LineCfg^.RaNodeNr)));

  FillChar(Useron^, SizeOf(UserOn^), #0);
  UserOn^.Name      := LineCfg^.Exitinfo^.Userinfo.Name;
  UserOn^.Handle    := LineCfg^.Exitinfo^.Userinfo.Handle;

  if LineCfg^.RaNodeNr <= 255 then
    UserOn^.Line      := Byte(LineCfg^.RaNodeNr)
     else Useron^.Line := 0;

  UserOn^.Baud      := LineCfg^.Exitinfo^.Baud;
  UserOn^.City      := LineCfg^.Exitinfo^.Userinfo.Location;
  UserOn^.Attribute := 00;
  UserOn^.Status    := Byte(Status);
  UserOn^.Statdesc  := SaveStatDesc;
  UserOn^.NodeNumber:= LineCfg^.RaNodeNr;
  UserOn^.LastUpdate:= NowAsUnixDate;

  if ReadBit(LineCfg^.Exitinfo^.Userinfo.Attribute2, 3) then SetBit(Useron^.Attribute, 00)     { Hidden }
    else SetBit(Useron^.Attribute, 6);
  if LineCfg^.Exitinfo^.WantChat then SetBit(Useron^.Attribute, 01);                         { Wantchat }
  if ReadBit(LineCfg^.Exitinfo^.Userinfo.Attribute, 7) then SetBit(Useron^.Attribute, 03); { Quiet mode }

  UserOn^.StatDesc := Under2Norm(StatDesc);
  If LineCfg^.Exitinfo^.Userinfo.NoCalls<65535 then
   UserOn^.NoCalls := LineCfg^.Exitinfo^.Userinfo.NoCalls
    else Useron^.NoCalls := 65535;                     { Stupid word.. :-( }

  if UserOn^.StatDesc <> '' then
    UserOn^.Status := 255;

  Config_WriteFile(UseronF, Useron^, SizeOf(UseronRecord));
  Config_DoneFile(UseronF);
end; { proc. WriteUserOn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tMultiLnObj.KillFromUserOn;
var UseronF   : pFileObj;
begin
  if Config_OpenFile(UseronF, GlobalCfg^.RaConfig^.SysPath + 'useron.bbs', 01, ReadWriteMode + DenyNone,
      False, False) > 00 then
        begin
          Config_DoneFile(UserOnF);
          EXIT;
        end; { if }

  Config_SeekFile(UseronF, Longint(SizeOf(UseronRecord)) * Longint(Pred(LineCfg^.RaNodeNr)));
  Config_ReadFile(UseronF, UserOn^, SizeOf(UseronRecord));
  Config_SeekFile(UseronF, Longint(SizeOf(UseronRecord)) * Longint(Pred(LineCfg^.RaNodeNr)));

  if SUpCase(Useron^.Name) = SUpCase(LineCfg^.Exitinfo^.Userinfo.Name) then
    begin
      FillChar(Useron^, SizeOf(UseronRecord), #00);
      Config_WriteFile(UseronF, Useron^, SizeOf(UseronRecord));
    end; { if }

  Config_DoneFile(UseronF);
end; { proc. KillFromUserOn }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit MULTILN }
