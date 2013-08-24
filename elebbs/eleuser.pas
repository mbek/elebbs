program EleUSER;
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
  {$IFNDEF VirtualPascal}
    {$APPTYPE CONSOLE}
  {$ENDIF}
{$ENDIF}
(*
**
** EleUSER, RemoteAccess v2.50 userbase maintenance program (for EleBBS)
**
** Copyright (c) 1996-2001 by Maarten Bekers
**
** Created : 26-Mar-1997
** Last update : 26-Mar-1997
**
**
** Note: Creditting system is not implemented yet! So "-R" option is not
**       functional!!
**
*)

uses {$IFDEF DELCRT}
       Crt32,
     {$ENDIF}

     Global,
      CfgRec,
       LongStr,
        CentrStr,
         JDates,
          BitWise,
           Crc_Unit,
            Cases,
             WordStr,
              Sort_Un,
               IORes_un,
                ElLog_U,
                 GenFile,
                   ReadCfg,
                    MemMan,
                     BBSKey,
                      Dos,
                       Crt,
                        WinTitle,
                         FileObj,
                          FileRout,
                           Debug_U,
                            UsrExt,
                             ObjDec,
                              SysVars;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
  To be able to compile EleFile/EleUser/EleNode the WITH_FULL conditional flag has to be disabled
{$ENDIF}

Type OptionsType = (otVerbose, otSort, otPacking, otDays, otSecurity, otReset,
                    otKilltimes, otRebuildExt);

Const DefaultOptions : Set of OptionsType = [otPacking];
      KillDays       : Word = 00;
      KillSecurity   : Word = 00;
      KillTimes      : Word = 00;
      MaxSortings    = (65535 div SizeOf(Integer)) - 2;

      DeleteSema     : Boolean = false;

Type SortListType    = Integer;
     SortListRec     = Array[1..MaxSortings] of SortListType;

var  Options     : Set of OptionsType;
     SortIDX     : ^SortListRec;
     SortUsers_F : pFileObj;
     SaveAttr    : Byte;
     SaveExitProc: Pointer;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure FatalError(S: String);
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logUserBase, '** FatalError (S='+ S + ')');
  {$ENDIF}

  { we cant release raconfig here cause the semaphore directory uses it }
  { MemMan.ReleaseMem(RaConfig, SizeOf(ConfigRecord)); }
  MemMan.ReleaseMem(LineCfg^.Exitinfo, SizeOf(ExitinfoRecord));
  MemMan.ReleaseMem(LineCfg^.UserExtension, SizeOf(UserExtensionRecord));

  WriteLn;
  WriteLn(#32, SystemMsgPrefix, S, ^G);
  WriteLn;

  TextAttr := SaveAttr;
  Halt(255);
end; { proc. FatalError }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ShowHeader;
begin
  SaveAttr := TextAttr;
  TextAttr := LightGray;

  WriteLn;
  WriteLn('ELEUSER; EleBBS User maintenance utility, Version '+VersionID);
  WriteLn('         Copyright 1997-2003 Maarten Bekers, All rights reserved.');
  WriteLn;

  New(LineCfg);
  New(GlobalCfg);
  InitBasicGlobalVars;
  InitBasicLineVars(LineCfg^);


  if NOT MemMan.AllocMem(GlobalCfg^.RaConfig, SizeOf(ConfigRecord), 'ConfigRecord', 'ShowHeader') then
     FatalError('Could not allocate enough memory!');

  if NOT MemMan.AllocMem(GlobalCfg^.ElConfig, SizeOf(EleConfigRecord), 'EleConfigRecord', 'ShowHeader') then
     FatalError('Could not allocate enough memory!');

  if NOT MemMan.AllocMem(LineCfg^.Exitinfo, SizeOf(ExitinfoRecord), 'ExitinfoRecord', 'ShowHeader') then
     FatalError('Could not allocate enough memory!');

  if NOT MemMan.AllocMem(LineCfg^.UserExtension, SizeOf(UserExtensionRecord), 'UserExtensionRecord', 'ShowHeader') then
     FatalError('Could not allocate enough memory!');
end; { proc. ShowHeader }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure CloseDownExitProc;
begin
  ExitProc := SaveExitProc;

  if DeleteSema then
    RemoveSema(EleUserSemaphore);
end; { proc. CloseDownExitProc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure ParseCommandLine;
var Invalid : Boolean;
    InvOpt  : String;
    Counter : Byte;
    Param   : String;
    TempCH  : Char;
begin
  Invalid := False;
  InvOpt := '';

  If ParamCount > 00 then
   For Counter := 01 to ParamCount do
    begin
       Param := ParamStr(Counter);
       TempCH := UpCase(Param[2]);

       If (NOT (Param[1] in ['/', '-'])) OR
           ((Length(ParamStr(Counter)) > 2) AND NOT (TempCH in ['D', 'M', 'T', 'X'])) then
            begin
              InvOpt := ParamStr(Counter);
              Invalid := True;
              Break;
            end; { Invalid }

       Case UpCase(TempCH) of
          'E' : Options := Options + [otRebuildExt];
          'V' : Options := Options + [otVerbose];
          'S' : Options := Options + [otSort];
          'P' : Options := Options + [otPacking];
          'D' : If Length(ParamStr(Counter)) > 2 then
                    begin;
                       KillDays := FVal(Copy(Param, 3, 255));

                       If KillDays > 00 then
                         Options := Options + [otDays];
                    end; { Delete users }
          'T' : If Length(ParamStr(Counter)) > 2 then
                    begin;
                       KillTimes := FVal(Copy(Param, 3, 255));

                       If KillTimes > 00 then
                         Options := Options + [otKillTimes];
                    end; { Purge on # of calls }
          'M' : if Length(ParamStr(Counter)) > 2 then
                    begin
                       KillSecurity := FVal(Copy(Param, 3, 255));

                       If KillSecurity > 00 then
                         Options := Options + [otSecurity];
                    end; { Purging days }
          'R' : Options := Options + [otReset];
          'X' : DebugObj.DebugLogging := TRUE;
           else begin
                  Invalid := True;
                  InvOpt := ParamStr(Counter);
                end; { Invalid option }
       end; { case }

   end; { for Counter }

  if Invalid then
    begin
      if InvOpt <> '?' then
        Writeln(#32, SystemMsgPrefix, 'Invalid option : ', InvOpt);
      WriteLn(#32, SystemMsgPrefix, 'Command-line parameters:');
      WriteLn;
      WriteLn(Dup(#32, 09), '-V      Enable verbose logging');
      WriteLn(Dup(#32, 09), '-S      Sort users in alphabetical order');
      WriteLn(Dup(#32, 09), '-P      Pack user file');
      WriteLn(Dup(#32, 09), '-E      Rebuild "usersele.bbs" file (discard prev data)');
      WriteLn(Dup(#32, 09), '-D[n]   Delete user''s who haven''t called in [n] days');
      WriteLn(Dup(#32, 09), '-M[s]   Only purges users with security level less than [s]');
      WriteLn(Dup(#32, 09), '-T[t]   Only purges users which called less than [t] times');
    {     WriteLn(Dup(#32, 09), '-R      Reset credit balances from LIMITS information'); }
      WriteLn;
      WriteLn(#32, SystemMsgPrefix, 'Please refer to the documentation for a more complete command summary');
      WriteLn;


      MemMan.ReleaseMem(GlobalCfg^.RaConfig, SizeOf(ConfigRecord));
      MemMan.ReleaseMem(LineCfg^.Exitinfo, SizeOf(ExitinfoRecord));
      MemMan.ReleaseMem(LineCfg^.UserExtension, SizeOf(UserExtensionRecord));
      TextAttr := SaveAttr;
      Halt(255);
    end; { Invalid }


  Writeln(#32, SystemMsgPrefix, 'Active options:');
  If otSort in Options then WriteLn('   - Sorting users by surname')
      else WriteLn('   - Not sorting users');

  if otRebuildExt in Options then
    WriteLn('   - Rebuild EleBBS extension file');

  Writeln('   - Packing users file');

  If otDays in Options then WriteLn('   - Killing users who have not called for ', KillDays, ' days')
      else WriteLn('   - Not killing users');
  Writeln;
end; { proc. ParseCommandLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure PackUserFile;
var TotalSize     : LongInt;
    UserExt_F     : pFileObj;
    NewUserExt_F  : pFileObj;
    Users_F       : pFileObj;
    NewUsers_F    : pFileOBj;
    LastRead_F    : pFileObj;
    NewLastRead_F : pFileObj;
    UserInf       : UsersRecord;
    UserExtInf    : UserExtensionRecord;
    LastReadInf   : LastReadRecord;
    OldFMode      : Longint;
    NrDeleted     : Longint;
    TempIO        : Longint;
    DidRead       : Longint;

function MeetsCriteria(Const UserInf: UsersRecord): Boolean;

function Bool2Str(B: Boolean): String;
begin
  if b then Bool2Str := 'Yes'
    else Bool2Str := 'No';
end; { func. Bool2Str }

begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logUserBase, 'MeetsCriteria (Userinf=' + Userinf.Name +
        ') - Sec=#'+ FStr(userInf.Security) + ', Calls#='+FStr(UserInf.NoCalls)+', KillDays = '+
             FStr(DaysAgo(Userinf.LastDate))+ '), NoKill='+Bool2Str(ReadBit(UserInf.Attribute, 4)));
  {$ENDIF}

  MeetsCriteria := True;

  if ((UserInf.Security <= KillSecurity) OR (NOT (otSecurity in options))) then
   if ((Userinf.NoCalls <= KillTimes) OR (NOT (otKillTimes in options))) then
    if KillDays>00 then
     if DaysAgo(UserInf.LastDate) > KillDays then MeetsCriteria := False;

  if ReadBit(UserInf.Attribute, 0) then MeetsCriteria := False;    { Deleted }
  if ReadBit(UserInf.Attribute, 4) then MeetsCriteria := True;     { No-Kill }
end; { func. MeetsCriteria }

begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logUserBase, 'PackUserFile (begin)');
  {$ENDIF}

  NrDeleted := 00;

  TotalSize := GetFileSize(GlobalCfg^.RaConfig^.MsgBasePath + UserBaseName, 01);
  TotalSize := TotalSize + GetFileSize(GlobalCfg^.RaConfig^.MsgBasePath + UserBaseIdxName, 01);
  TotalSize := TotalSize + GetFileSize(GlobalCfg^.RaConfig^.MsgBasePath + UserBaseXiName, 01);
  TotalSize := TotalSize + GetFileSize(GlobalCfg^.RaConfig^.MsgBasePath + UserBaseEleName, 01);

  {$IFDEF WITH_DEBUG}
    if otSecurity in Options then DebugObj.DebugLog(logUserBase, 'Kill on Security is ON')
      else DebugObj.DebugLog(logUserBase, 'Kill on Security is OFF');

    if otKillTimes in Options then DebugObj.DebugLog(logUserBase, 'Kill on times called is ON')
      else DebugObj.DebugLog(logUserBase, 'Kill on times called is OFF');

    DebugObj.DebugLog(logUserBase, 'KillDays is = ' +FStr(KillDays));
  {$ENDIF}

  WriteLn(#32, SystemMsgPrefix, 'Space required: ', CommaStr(TotalSize), '  Space free: ',
           CommaStr(GetDiskFree(GlobalCfg^.RaConfig^.MsgBasePath[1])));

  If TotalSize > GetDiskFree(GlobalCfg^.RaConfig^.MsgBasePath[1]) then
    FatalError('Insufficient diskspace available!');

  WriteLn;
  WriteLn(#32, SystemMsgPrefix, 'Packing user file');
  RaLog('>', 'Packing user file');

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logUserBase, 'PackUserFile - Opening files');
     DebugObj.DebugLog(logUserBase, 'PackUserFile - UserExtensionSize = ' + FStr(LineCfg^.UserExtensionSize));
  {$ENDIF}

  New(NewUsers_F, Init);
  NewUsers_F^.Assign(GlobalCfg^.RaConfig^.MsgBasePath + 'users.new');
  NewUsers_F^.FileMode := WriteMode + DenyNone;
  if NOT NewUsers_F^.Create(1) then
    FatalError('Unable to create users.new');


  New(Users_F, Init);
  Users_F^.Assign(GlobalCfg^.RaConfig^.MsgBasePath+ UserBaseName);
  Users_F^.FileMode := ReadMode + DenyWrite;
  if NOT Users_F^.Open(1) then
    FatalError('Unable to open ' + UserBaseName);

  New(NewUserExt_F, Init);
  NewUserExt_F^.Assign(GlobalCfg^.RaConfig^.MsgBasePath + 'usersele.new');
  NewUserExt_F^.FileMode := WriteMode + DenyNone;
  if NOT NewUserExt_F^.Create(1) then
    FatalError('Unable to create usersele.new');


  New(UserExt_F, Init);
  UserExt_F^.Assign(GlobalCfg^.RaConfig^.MsgBasePath + UserBaseEleName);
  UserExt_F^.FileMode := ReadMode + DenyWrite;
  if NOT UserExt_F^.Open(1) then
    FatalError('Unable to open ' + UserBaseEleName);

  New(NewLastRead_F, Init);
  NewLastRead_F^.Assign(GlobalCfg^.RaConfig^.MsgBasePath + 'lastread.new');
  NewLastRead_F^.FileMode := WriteMode + DenyNone;

  if NOT NewLastRead_F^.Create(1) then
    FatalError('Unable to create lastread.new');


  New(LastRead_F, Init);
  LastRead_F^.Assign(GlobalCfg^.RaConfig^.MsgBasePath + UserBaselastRead);
  LastRead_F^.FileMode := ReadMode + DenyWrite;
  if NOT LastRead_F^.Open(1) then
      FatalError('Unable to open '+UserBaseLastRead);

  While (IOResult = 0) AND (NOT Users_F^.EOF) do
    begin
      {$i-}
        FillChar(UserInf, SizeOf(UserInf), #00);

        DidRead := Users_F^.BlkRead(UserInf, SizeOf(UsersRecord));
        if (Users_F^.IoResult > 00) OR (DidRead <> SizeOf(UsersRecord)) then
          RaLog('>', 'Userbase error (repacking-1-1) err.code: '+FStr(IoResult));

        DidRead := UserExt_F^.BlkRead(UserExtInf, LineCfg^.UserExtensionSize);
        if (UserExt_F^.IoResult > 00) OR (DidRead <> LineCfg^.UserExtensionSize) then
          RaLog('>', 'Userbase error (repacking-1-2) err.code: '+FStr(UserExt_F^.IoResult));

        DidRead := LastRead_F^.BlkRead(LastReadInf, SizeOf(LastReadRecord));
        if (LastRead_F^.IoResult > 00) OR (DidRead <> SizeOf(LastReadRecord)) then
          RaLog('>', 'Userbase error (repacking-1-3) err.code: '+FStr(IoResult));

        if MeetsCriteria(UserInf) then
         begin
           {$IFDEF WITH_DEBUG}
              DebugObj.DebugLog(logUserBase, 'Written: ' + UserInf.Name);
           {$ENDIF}

           NewUsers_F^.BlkWrite(UserInf, SizeOf(UsersRecord));
           NewUserExt_F^.BlkWrite(UserExtInf, LineCfg^.UserExtensionSize);
           NewLastRead_F^.BlkWrite(LastReadInf, SizeOf(LastReadRecord));
         end
           else begin
                  {$IFDEF WITH_DEBUG}
                    DebugObj.DebugLog(logUserBase, 'Deleted: ' + UserInf.Name);
                  {$ENDIF}

                  Inc(NrDeleted);

                  If otVerbose in Options then
                    RaLog('!', 'Removed '+UserInf.Name);
                end; { Don't delete }
      {$I+}
    end; { While }

  If NrDeleted=00 then
       begin
         WriteLn('   (no users deleted)');
         RaLog('>', 'Pack completed (no users deleted)');
       end
        else begin
               RaLog('>', 'Pack completed ('+FStr(NrDeleted)+' users deleted)');
               WriteLn('   (', nrdeleted,' users deleted)');
             end; { Users deleted }

  NewUsers_F^.Close;
  NewUserExt_F^.Close;
  NewLastRead_F^.Close;

  UserExt_F^.Close;
  UserExt_F^.Erase;

  Users_F^.Close;
  Users_F^.Erase;

  LastRead_F^.Close;
  LastRead_F^.Erase;

  RenameObjWithDrive(NewUsers_F, GlobalCfg^.RaConfig^.MsgBasePath+UserBaseName);
  RenameObjWithDrive(NewUserExt_F, GlobalCfg^.RaConfig^.MsgBasePath+UserBaseEleName);
  RenameObjWithDrive(NewLastRead_F, GlobalCfg^.RaConfig^.MsgBasePath+UserBaseLastRead);

  Dispose(Users_F, Done);
  Dispose(UserExt_F, Done);
  Dispose(LastRead_F, Done);
  Dispose(NewUsers_F, Done);
  Dispose(NewUserExt_F, Done);
  Dispose(NewLastRead_F, Done);

  Writeln;

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logUserBase, 'PackUserFile ( end )');
  {$ENDIF}
end; { proc. PackUserFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoBuildFastIndex;
begin
  WriteLn(#32, SystemMsgPrefix, 'Building user fast-index');

  BuildFastIndex;

  WriteLn;
end; { proc. DoBuildFastIndex }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$F+,R-,S-}
Function SortFunc(El1, El2: Pointer):Boolean;
var UsersInf : Array[1..2] of UsersRecord;
    Temp     : Array[1..2] of String;
begin
  {$i-}
    SortUsers_F^.Seek( (Integer(El1^) - 1));
    SortUsers_F^.BlkRead(UsersInf[1], 1);
    SortUsers_F^.Seek( (Integer(El2^) - 1));
    SortUsers_F^.BlkRead(UsersInf[2], 1);
  {$i+}
  If IOResult>00 then;

  Temp[1] := SUpcase(ExtractWord(UsersInf[1].Name, WordCount(UsersInf[1].Name, defExtractWord, false), defExtractWord,
                       false, false));
  Temp[2] := SUpcase(ExtractWord(UsersInf[2].Name, WordCount(UsersInf[2].Name, defExtractWord, false), defExtractWord,
                       false, false));

  SortFunc := UsersInf[1].Security < UsersInf[2].Security;

  If UsersInf[1].Security = UsersInf[2].Security then
   SortFunc := Temp[1] > Temp[2];
end; { func. SortFunc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure SortUserFile;
var UsersInf      : UsersRecord;
    NewUsers_F    : pFileObj;
    LastRead_F    : pFileObj;
    NewLastRead_F : pFileObj;
    UserExt_F     : pFileObj;
    NewUserExt_F  : pFileObj;
    OldFMode      : Longint;
    Counter       : Longint;
    Counter2      : Longint;
    LastReadInf   : LastReadRecord;
    UserExtInf    : UserExtensionRecord;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logUserBase, 'SortUserFile (begin)');
  {$ENDIF}

  Write(#32, SystemMsgPrefix, 'Sorting user-file (wait)...');
  RaLog('>', 'Sorting user file');

  New(SortUsers_F, Init);
  SortUsers_F^.Assign(GlobalCfg^.RaConfig^.MsgBasePath+UserBaseName);
  SortUsers_F^.FileMode := ReadMode + DenyWrite;

  if NOT SortUsers_F^.Open(SizeOf(UsersRecord)) then
    FatalError('Unable to open '+UserBaseName);

  New(NewUserExt_F, Init);
  NewUserExt_F^.Assign(GlobalCfg^.RaConfig^.MsgBasePath + 'usersele.new');
  NewUserExt_F^.FileMode := WriteMode + DenyNone;
  if NOT NewUserExt_F^.Create(1) then
    FatalError('Unable to create usersele.new');


  New(UserExt_F, Init);
  UserExt_F^.Assign(GlobalCfg^.RaConfig^.MsgBasePath + UserBaseEleName);
  UserExt_F^.FileMode := ReadMode + DenyWrite;
  if NOT UserExt_F^.Open(1) then
    FatalError('Unable to open ' + UserBaseEleName);

  New(NewLastRead_F, Init);
  NewLastRead_F^.Assign(GlobalCfg^.RaConfig^.MsgBasePath + 'lastread.new');
  NewLastRead_F^.FileMode := WriteMode + DenyNone;
  if NOT NewLastRead_F^.Create(1) then
    FatalError('Unable to create lastread.new');


  New(LastRead_F, Init);
  LastRead_F^.Assign(GlobalCfg^.RaConfig^.MsgBasePath+UserBaseLastRead);
  LastRead_F^.FileMode := ReadMode + DenyWrite;
  if NOT LastRead_F^.Open(1) then
    FatalError('Unable to open '+UserBaseLastRead);

  If SortUsers_F^.FileSize > MaxSortings then
     begin
        GotoXY(1, WhereY); ClrEol;
        WriteLn(#32, SystemMsgPrefix, 'Userfile is too big to sort!');
        Writeln;

        Dispose(SortUsers_F, Done);
        EXIT;
     end; { Too Big To Sort }

  if NOT MemMan.AllocMem(SortIdx, SizeOf(SortListRec), 'SortListRec', 'SortUsers') then
    FatalError('Not enough memory for sorting buffer!');

  FillChar(SortIdx^, SizeOf(SortListRec), 00);

  for Counter := 01 to SortUsers_F^.FileSize do
    SortIdx^[Counter] := Counter;

  Write(' (reading)');

  QuickSort(SortIdx^,
            SortUsers_F^.FileSize,
            SizeOf(SortListType),
            {$IFDEF FPC}@{$ENDIF}SortFunc);

  New(NewUsers_F, Init);
  NewUsers_F^.Assign(GlobalCfg^.RaConfig^.MsgBasePath + 'users.new');
  NewUsers_F^.FileMode := WriteMode + DenyNone;
  if NOT NewUsers_F^.Create(1) then
    FatalError('Unable to create users.new');

  Write(Dup(#08, Length(' (reading)')), '(writing) ');

  For Counter := 01 to SortUsers_F^.FileSize do
   begin
     {$i-}
       SortUsers_F^.Seek(SortIdx^[Counter]-1);
       UserExt_F^.Seek(SortIdx^[Counter]-1 * LineCfg^.UserExtensionSize);
       LastRead_F^.Seek(Longint(SortIdx^[Counter]-1) * SizeOf(LastReadRecord));

       SortUsers_F^.BlkRead(UsersInf, 1);
       NewUsers_F^.BlkWrite(UsersInf, SizeOf(UsersRecord));

       LastRead_F^.BlkRead(LastReadInf, SizeOf(LastReadRecord));
       NewLastRead_F^.BlkWrite(LastReadInf, SizeOf(LastReadRecord));

       UserExt_F^.BlkRead(UserExtInf, LineCfg^.UserExtensionSize);
       NewUserExt_F^.BlkWrite(UserExtInf, LineCfg^.UserExtensionSize);
     {$I+}
     If IOResult>00 then break;
   end; { for }

  SortUsers_F^.Close;
  SortUsers_F^.Erase;

  UserExt_F^.Close;
  UserExt_F^.Erase;

  LastRead_F^.Close;
  LastRead_F^.Erase;

  NewUsers_F^.Close;
  NewLastRead_F^.Close;
  NewUserExt_F^.Close;

  RenameObjWithDrive(NewUsers_F, GlobalCfg^.RaConfig^.MsgBasePath+UserBaseName);
  RenameObjWithDrive(NewUserExt_F, GlobalCfg^.RaConfig^.MsgBasePath+UserBaseEleName);
  RenameObjWithDrive(NewLastRead_F, GlobalCfg^.RaConfig^.MsgBasePath+UserBaseLastRead);

  Dispose(NewUsers_F, Done);
  Dispose(NewUserExt_F, Done);
  Dispose(LastRead_F, Done);
  Dispose(NewLastRead_F, Done);
  Dispose(SortUsers_F, Done);
  Dispose(UserExt_F, Done);

  MemMan.ReleaseMem(SortIDX, SizeOf(SortListRec));

  GotoXY(1, WhereY); ClrEol;
  WriteLn(#32, SystemMsgPrefix, 'Sorted user file');
  WriteLn;

  RaLog('>', 'Sort completed');

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logUserBase, 'SortUserFile ( end )');
  {$ENDIF}
end; { proc. SortUserFile }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

begin
  SetWindowTitle('EleUSER');

  {$IFNDEF DELCRT}
    AssignCrt(CrtOutput);
    System.Rewrite(Crtoutput);
  {$ENDIF}

  SaveExitProc := ExitProc;
  ExitProc := @CloseDownExitproc;
  RunningBBS := False;
  Options := DefaultOptions;

  {$IFDEF OVERLAY}
    InitErrorHandler;
  {$ENDIF}
  ShowHeader;
  ReadConfigRa;
  InitSystemNames;
  CheckRegistered;
  ParseCommandLine;

  {-- initialize termobject, we need it for EleXer --------------------------}
  New(termobj, Init);

  {-- log some stuff --------------------------------------------------------}
  RaLog(' ', '');
  RaLog('>', 'EleUSER; User maintenance utility fired up');

  if SemaExist(EleUserSemaphore) then
    begin
      RaLog('!', 'EleUSER is already running ('+EleUserSemaPhore + ' exists), aborting.');
      WriteLn(#32, SystemMsgPrefix, 'EleUSER is already running ('+EleUserSemaPhore + ' exists), aborting');
    end { if }
     else begin
            CreateSema(EleUserSemaphore);
            DeleteSema := true;

            {-- Make sure we have the size of the userbase extension file ---}
            LineCfg^.UserExtensionSize := usrext_GetSize;

            {-- rebuild users.ele when so specified -------------------------}
            if otRebuildExt in Options then
              begin
                Write(#32, SystemMsgPrefix, 'Rebuilding user extension file ..');

                BuildUserExt;
                WriteLn;
              end; { if }

            {-- we always pack the userfile ---------------------------------}
            PackUserFile;

            {-- sort the usersfile ------------------------------------------}
            if otSort in Options then
              SortUserFile;

            {-- and always rebuild the useridx.bbs --------------------------}
            DoBuildFastIndex;
          end; { if }

  TextAttr := SaveAttr;
end. { EleUSER }
