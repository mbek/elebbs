unit SCRLIST;
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
** Scriptable listing routines (todays callers etc) for EleBBS
**
** Copyright (c) 1996 - 1999 by Maarten Bekers
**
** Created : 11-Jul-1999
** Last update : 11-Jul-1999
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowTodaysCallers(MiscData: String);        { Show callers of today }
function ScriptLastcallers: Boolean;
procedure ShowUserList(MiscData: String);           { Show a userbase listing }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Global, StrUnit, CfgRec, Cases, LongStr, WordStr, StUtils,
      BitWise, Question, FileObj, Debug_U, ObjDec, CentrStr, RAL,
       Colors, CfgFile, Input_U, LineEd, GenFile, ElLog_u;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure TodaysCallersHook(    RecordNum  : Longint;
                            var Answers    : StringArrayObj;
                                StartRecord: Longint;
                                GoingDown  : Boolean;
                            var GlobalFile : pFileObj);
var LastCallInf: LastcallRecord;
    EndOfFile  : Boolean;
begin
  GlobalFile^.Seek((Recordnum - 01) * SizeOf(LastcallRecord));

  repeat
    if (GlobalFile^.BlkRead(LastcallInf, SizeOf(LastcallRecord)) = 0) then
      FillChar(LastcallInf, SizeOf(Lastcallrecord), #00);

    if GlobalFile^.IoResult > 0 then
      FillChar(LastCallInf, SizeOf(LastCallRecord), #0);

    if ((SUpcase(LastcallInf.Name) = SUpcase(GlobalCfg^.RaConfig^.Sysop)) AND (GlobalCfg^.RaConfig^.ExcludeSysOpfromlist)) then
        LastCallInf.Name := '';
    if ((SUpcase(LastcallInf.Handle) = SUpcase(GlobalCfg^.RaConfig^.Sysop)) AND
         (GlobalCfg^.RaConfig^.ExcludeSysOpfromlist)) then
           LastCallInf.Name := '';
    if (ReadBit(LastCallInf.Attribute, 0)) then LastcallInf.Name := '';

    if GoingDown then
      EndOfFile := GlobalFile^.FilePos >= GlobalFile^.FileSize
       else EndOfFile := GlobalFile^.FilePos <= 0;

    if LastcallInf.Name = '' then
     if NOT GoingDown then
       GlobalFile^.Seek(GlobalFile^.FilePos - (SizeOf(LastcallRecord) * 2));
  until (LastcallInf.Name <> '') OR (EndOfFile);

  if LastcallInf.Name = '' then
    LastCallInf.Line := 0;

  Answers.Put(StartRecord + 00, FStr(GlobalFile^.FilePos DIV SizeOf(LastcallRecord)));
  Answers.Put(StartRecord + 01, LastcallInf.Name);
  Answers.Put(StartRecord + 02, LastcallInf.Handle);
  Answers.Put(StartRecord + 03, FStr(LastcallInf.Line));
  Answers.Put(StartRecord + 04, FStr(LastcallInf.Baud));
  Answers.Put(StartRecord + 05, LastcallInf.City);
  Answers.Put(StartRecord + 06, FStr(LastcallInf.Times));
  Answers.Put(StartRecord + 07, LastcallInf.Logon);
  Answers.Put(StartRecord + 08, LastcallInf.Logoff);
end; { proc. Todayscallershook }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ScriptLastcallers: Boolean;
var Quest        : QuestObj;
    TotalRecords : Longint;
    LastcallInf  : LastcallRecord;
    GlobalFile   : pFileObj;
begin
  ScriptLastcallers := false;
  TotalRecords := 00;

  New(GlobalFile, Init);
  GlobalFile^.Assign(LastCallFileName);
  GlobalFile^.FileMode := ReadMode + DenyNone;
  if NOT GlobalFile^.Open(1) then
    begin
      Dispose(GlobalFile, Done);
      EXIT;
    end; { if }

  {------------------- Count the number of records in this file -------------}
  repeat
    if (GlobalFile^.BlkRead(LastcallInf, SizeOf(LastcallRecord)) = 0) then
      FillChar(LastcallInf, SizeOf(LastcallRecord), #00);

    if ((SUpcase(LastcallInf.Name) = SUpcase(GlobalCfg^.RaConfig^.Sysop)) AND (GlobalCfg^.RaConfig^.ExcludeSysOpfromlist)) then
        LastCallInf.Name := '';
    if ((SUpcase(LastcallInf.Handle) = SUpcase(GlobalCfg^.RaConfig^.Sysop)) AND
         (GlobalCfg^.RaConfig^.ExcludeSysOpfromlist)) then
           LastCallInf.Name := '';
    if (ReadBit(LastCallInf.Attribute, 0)) then LastcallInf.Name := '';

    if (NOT (LastCallInf.Name = '')) then
      if (NOT ReadBit(LastCallInf.Attribute, 0)) then Inc(TotalRecords);

  until (GlobalFile^.FilePos >= GlobalFile^.FileSize);
  {------------------- Count the number of records in this file -------------}

  Quest.Init;
  Quest.QInfo^.Answers^.Put(01, FStr(TotalRecords));
  Quest.QInfo^.GetInfoFile := GlobalFile;
  Quest.QInfo^.GetInfoHook := {$IFDEF FPC}@{$ENDIF}TodaysCallersHook;
  Quest.Process('LASTCALL /N', false, '');
  ScriptLastcallers := (Quest.GetError = 0);
  Quest.Done;

  Dispose(GlobalFile, Done);
end; { func. ScriptLastcallers }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowTodaysCallers(MiscData: String);         { Show todays callers }
var Lastcall_F  : pFileObj;
    Lastcall    : LastcallRecord;
    ShowUsr     : Boolean;
    NameToShow  : String;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMenuFunc, 'ShowTodaysCallers (start)');
  {$ENDIF}

  if NOT ScriptLastCallers then
   begin
     OutputObj^.ClearScreen;
     WriteLn;

     WriteLn('`A15:',CenterJust(LangObj^.ralGet(ralCalls2Day)+' '+GlobalCfg^.RaConfig^.Systemname, 79, False));
     Writeln('`A12:',CenterJust(Dup('Ä',NoColorLength(LangObj^.ralGet(ralCalls2Day)+' '+
                GlobalCfg^.RaConfig^.Systemname)),79,False));
     WriteLn;
     WriteLn('`A10:',LangObj^.ralGet(ralCallsHdr));
     Writeln('`A02:',OutputObj^.AvatarDupl('Ä', 79));

     if Config_OpenFile(LastCall_F, LastCallFileName, 01, ReadMode + DenyNone, False, False)=00 then
        begin
          While (NOT Lastcall_F^.EOF) AND (NOT OutputObj^.StopMore) do
           begin;
             Config_ReadFile(LastCall_F, LastCall, SizeOf(LastCallRecord));

             ShowUsr := True;
             If (SUpcase(Lastcall.Name)=SUpcase(GlobalCfg^.RaConfig^.Sysop)) AND (GlobalCfg^.RaConfig^.ExcludeSysOpfromlist)
               then ShowUsr:= False;
             if ((SUpcase(Lastcall.Handle) = SUpcase(GlobalCfg^.RaConfig^.Sysop))
                 AND (GlobalCfg^.RaConfig^.ExcludeSysOpfromlist))
               then ShowUsr := FALSE;

             if (ReadBit(LastCall.Attribute, 0)) then ShowUsr := false;
             if Lastcall.Name = '' then ShowUsr := false;

             If ShowUsr then
              begin
                If Pos('/H', SUpcase(MiscData))>00 then NameToShow := Lastcall.Handle
                 else NameToShow := Lastcall.Name;

                if Length(NameToShow) > 20 then
                  Delete(NameToShow, 21, 255);

                WriteLn('`X01:`A11:', NameToShow,
                        '`X22:`A15:', Lastcall.Line,
                        '`X29:`A15:', FixBaud(Lastcall.Baud),
                        '`X39:`A15:', Lastcall.Logon,
                        '`X46:`A15:', Lastcall.LogOff,
                        '`X55:`A15:', Lastcall.Times,
                        '`X62:`A14:', Copy(Lastcall.City, 1, 15));
              end; { ShowUsr }
           end; { while }

         Config_DoneFile(LastCall_F);
        end; { IOResult=00 }

      WriteLn;
      OutputObj^.SetStopMore(False);
      InputObj^.PressEnter(True, True);

     {$IFDEF WITH_DEBUG}
        DebugObj.DebugLog(logMenuFunc, 'ShowTodaysCallers ( end )');
     {$ENDIF}
   end; { if }
end; { proc. ShowTodaysCallers }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowUserList(MiscData: String);          { Show a userbase listing }
var User_F      : pFileObj;
    UserInf     : UsersRecord;
    SearchStr   : String;
    DoShow      : Boolean;
    NameToShow  : String;
    GroupShow   : Longint;
    NrItems     : Longint;
    ItemCounter : Longint;
    NumRead     : Numreadtype;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMenuFunc, 'ShowUserList (start)');
  {$ENDIF}

  SearchStr := '';
  MiscData := SupCase(MiscData);
  GroupShow := -1;                              { Show all users in the group }

  GroupShow := FVal(Trim(GetValue('/G=', MiscData, False)));
  If (GroupShow=00) AND (Pos('/G', MiscData)>00) then
      GroupShow := LineCfg^.Exitinfo^.Userinfo.Group
       else GroupShow := -1;
  If (Pos('/G=', MiscData)>00) then
    GroupShow := FVal(GetValue('/G=', MiscData, False));

  {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logMenuFunc, 'ShowUserList: Showing group '+FStr(GroupShow));
  {$ENDIF}

  OutputObj^.ClearScreen;                                      { Clear the screen }
  Writeln;
  Write ('`A14:',LangObj^.ralGet(ralUsrSearch));             { Search-string }
  GetString(SearchStr, 20, [#32..#254], False, False, False, False);
  SearchStr := Trim(SearchStr);
  OutputObj^.ClearScreen;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMenuFunc, 'ShowUserList: Searching for user: "'+SearchStr+'"');
     DebugObj.DebugLog(logMenuFunc, 'ShowUserList: ' + FStr(
        GetFileSize(GlobalCfg^.RaConfig^.MsgBasePath + UserBaseName, SizeOf(UsersRecord))));
  {$ENDIF}

  WriteLn('`A15:',LangObj^.ralGet(ralUsrLstHdr));
  Writeln('`A15:',OutputObj^.AvatarDupl('Ä', 78));

  New(User_F, Init);
  User_F^.Assign(GlobalCfg^.RaConfig^.MsgBasePath + UserBaseName);
  User_F^.FileMode := ReadMode + DenyNone;
  if NOT User_F^.Open(1) then
    begin
      Dispose(User_F, Done);
      EXIT;
    end; { if }

  NrItems := User_F^.FileSize DIV SizeOf(UsersRecord);
  ItemCounter := 00;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMenuFunc, 'ShowUserList: NrItems = '+FStr(NrItems));
     DebugObj.DebugLog(logMenuFunc, 'StopMore    : '+Bool2Str(OutputObj^.StopMore));
  {$ENDIF}

  While (NrItems > ItemCounter) AND (NrItems > 00) AND (NOT OutputObj^.StopMore) do
    begin
      NumRead := User_F^.BlkRead(UserInf, SizeOf(UsersRecord));
      if NumRead <> SizeOf(UsersRecord) then BREAK;

      Inc(ItemCounter);

      If SearchStr='' then DoShow:=True else
        DoShow:= (Pos(SUpcase(SearchStr), SUpcase(UserInf.Name+UserInf.Handle))>0);

      If (SUpcase(UserInf.Name)=SUpcase(GlobalCfg^.RaConfig^.Sysop)) AND (GlobalCfg^.RaConfig^.ExcludeSysOpfromlist)
          then DoShow:= False;
      If (SUpcase(UserInf.Handle)=SUpcase(GlobalCfg^.RaConfig^.Sysop)) AND (GlobalCfg^.RaConfig^.ExcludeSysOpfromlist)
          then DoShow:= False;

      if ReadBit(UserInf.Attribute2, 3) then DoShow := false;   { Hidden userlist }


      {$IFDEF WITH_DEBUG}
        DebugObj.DebugLog(logMenuFunc, 'ShowUserList: User "'+UserInf.Name+'" is in group: '+FStr(Userinf.Group)+
                              ' / Currently show='+Bool2Str(DoShow));
      {$ENDIF}

      If OutputObj^.StopMore then DoShow:= False;
      If GroupShow>-1 then
       If UserInf.Group<>LineCfg^.Exitinfo^.Userinfo.Group then DoShow:= False;

      {$IFDEF WITH_DEBUG}
         DebugObj.DebugLog(logMenuFunc, 'ShowUserList: User "'+UserInf.Name+'" is in group: '+FStr(Userinf.Group));
      {$ENDIF}

      If DoShow then
        begin
          If Pos('/H', SUpcase(MiscData))>00 then NameToShow := UserInf.Handle
            else NameToShow := UserInf.Name;

          WriteLn('`X01:`A11:', NameToShow,
                  '`X31:`A10:', UserInf.Location,
                  '`X57:`A09:', UserInf.LastDate,
                  '`X67:`A14:', UserInf.NoCalls,
                  '`X73:`A12:', LeadingZero(UserInf.Uploads, 2),':',LeadingZero(UserInf.Downloads, 2));
        end; { if DoShow }
    end; { While }

  Dispose(User_F, Done);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMenuFunc, 'ShowUserList ( end )');
  {$ENDIF}

  OutputObj^.SetStopMore(False);
  WriteLn;
  InputObj^.PressEnter(True, False);
end; { proc. ShowUserList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit SCRLIST }
