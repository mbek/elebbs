unit Limit_U;
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
** Limits (file,message,bpsrate) routines for EleBBS
**
** Copyright (c) 1997,98 by Maarten Bekers
**
** Created : 11-Apr-1998
** Last update : 11-Apr-1998
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
uses Global, CfgRec;


procedure GetLevelLimitsInfo(Level: SmallWord; WarnMsg, IgnoreTime: Boolean);
procedure GetLevelLimitRecord(Level: SmallWord; var LimitsInfo: LimitsRecord);
{$IFDEF WITH_FULL}
procedure LockOutUsers;                { Lock current user out of the system }
procedure SetTime(Left: Word);                            { Time set left }
procedure ChangeTime(Delta: Longint; DoCheck: Boolean);      { DorChangeTime }
procedure PickSecurity;
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses {$IFDEF WINGUI}
        SysUtils,
        JDates,
        Windows,
        Dialogs,
        Controls,
     {$ENDIF}

     {$IFDEF MSDOS}
        Dos,
     {$ENDIF}

     {$IFDEF FPC}
       Dos,
     {$ENDIF}

     {$IFDEF VirtualPascal}
        Dos,
     {$ENDIF}

     {$IFDEF WITH_FULL}
       Debug_U, LongStr, BitWise, CentrStr, LineEd,
       ElLog_U, FastScrn, FileRout, StatusB,
       Control, DispAns, RAL, ExitProg, User_U, Logon,
       CfgFile, InOut_U,
       {$IFNDEF WINGUI}
         Crt,
       {$ENDIF}
     {$ENDIF}

       fileObj,
       ObjDec,
       Ranges;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GetLevelLimitRecord(Level: SmallWord; var LimitsInfo: LimitsRecord);
var Limit_F     : pFileObj;
    NumRead     : NumReadType;
begin
  {-- Initialize the records -----------------------------------------------}
  FillChar(LimitsInfo, SizeOf(LimitsRecord), #00);

  {-- Open the file --------------------------------------------------------}
  New(Limit_F, Init);
  Limit_F^.Assign(LimitsFileName);
  Limit_F^.FileMode := ReadMode + DenyNone;

  if NOT Limit_F^.Open(1) then
    begin
      Dispose(Limit_F, Done);

      {$IFDEF WITH_FULL}
        Config_ShowError('Unable to open file', LimitsfileName, 2);
      {$ELSE}
        EXIT;
      {$ENDIF}
    end; { if }

  {-- Now lookup the security level ----------------------------------------}
  While LimitsInfo.Security <> Level do
    begin
      NumRead := Limit_F^.BlkRead(LimitsInfo, SizeOf(LimitsRecord));

      {-- Were suddenly above the wanted level? Fix this! ------------------}
      if LimitsInfo.Security >= Level then
        begin
          if LimitsInfo.Security > Level then
            begin
              Limit_F^.Seek((Limit_F^.FilePos - (2 * Longint(SizeOf(LimitsRecord)))));
              NumRead := Limit_F^.BlkRead(LimitsInfo, SizeOf(LimitsRecord));
            end; { if }

          BREAK;
        end; { if the correct level doesnt exist in the file }

      {-- Because of the fallback above, prevent looping -------------------}
      if (Limit_F^.FileSize = Limit_F^.FilePos) then BREAK; { eof }
    end; { While }

  {-- Dispose of the objects -----------------------------------------------}
  Dispose(Limit_F, Done);
end; { proc. GetLevelLimitRecord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure GetLevelLimitsInfo(Level: SmallWord; WarnMsg, IgnoreTime: Boolean);
var CH          : Char;
    SaveX,
    SaveY       : Byte;
    SaveScrn    : Pointer;
begin
  {-- Load the limits record -----------------------------------------------}
  GetLevelLimitRecord(Level, LineCfg^.LimitsInfo^);

  {-- OhNo! The user has no time left. Ask the Sysop -----------------------}
  {$IFDEF WITH_FULL}
  if (LongInt(LineCfg^.LimitsInfo^.LTime - LineCfg^.Exitinfo^.Userinfo.Elapsed) < 1) AND (WarnMsg) then
        begin
          SaveScreen(SaveScrn);
          LineCfg^.LocalOnly := True;
          {$IFNDEF WINGUI}
            SaveX := OutputObj^.WhereX;
            SaveY := OutputObj^.WhereY;
          {$ENDIF}

          BoxWindow(23, 11, 57, 17, GlobalCfg^.RaConfig^.BorderFore, GlobalCfg^.RaConfig^.BorderBack, 'Warning!');
          {$ifndef WinGUI}
            TextColor(GlobalCfg^.RaConfig^.WindFore);
            TextBackGround(GlobalCfg^.RaConfig^.WindBack);
            Crt.GotoXY(38, 13);
          {$endif}

          {$IFNDEF WINGUI}
            FastWrite(26, 13, TextAttr, 'User will have no time left.');
            FastWrite(29, 15, TextAttr, 'Reset user''s time (Y,n)?');
          {$ELSE}
           {$IFDEF WITH_FULL}
            Case MessageDlg('User will have no time left'+#10+#13+
                            'Reset user''s time?', mtConfirmation, [mbYes, mbNo], 00) of
                mrYes   : CH := 'Y';
                mrNo    : CH := 'N';
            end;
           {$ENDIF}
          {$ENDIF}

          Repeat;
          {$IFNDEF WINGUI}
            CH := UpCase(Crt.ReadKey);
          {$ENDIF}

            If CH=#13 then CH := 'Y';
          Until CH in ['Y', 'N'];

          If CH='Y' then LineCfg^.Exitinfo^.Userinfo.Elapsed := 00;

          LineCfg^.LocalOnly := False;
          RestoreScreen(SaveScrn);
          {$ifndef WinGUI}
            Crt.GotoXY(SaveX, SaveY);
          {$Endif}
        end; { No time left }
  {$ENDIF}

  {-- Now obay the unlimited values ----------------------------------------}
  With LineCfg^.LimitsInfo^ do
    begin
      Ltime := Min(UnlimitedValue, lTime);
      L300 := Min(UnlimitedValue, l300);
      L1200 := Min(UnlimitedValue, l1200);
      L2400 := Min(UnlimitedValue, l2400);
      L4800 := Min(UnlimitedValue, l4800);
      L7200 := Min(UnlimitedValue, l7200);
      L9600 := Min(UnlimitedValue, l9600);
      L12000 := Min(UnlimitedValue, l12000);
      L14400 := Min(UnlimitedValue, l14400);
      L16800 := Min(UnlimitedValue, l16800);
      L19200 := Min(UnlimitedValue, l19200);
      L38400 := Min(UnlimitedValue, l38400);
      Llocal := Min(UnlimitedValue, lLocal);
      L21600 := Min(UnlimitedValue, l21600);
      L24000 := Min(UnlimitedValue, l24000);
      L26400 := Min(UnlimitedValue, l26400);
      L28800 := Min(UnlimitedValue, l28800);
      L57600 := Min(UnlimitedValue, l57600);
      L64000 := Min(UnlimitedValue, l64000);
      L31200 := Min(UnlimitedValue, l31200);
      L33600 := Min(UnlimitedValue, l33600);
      L115200  := Min(UnlimitedValue, l115200);
    end; { with }

  {-- If the time limit isnt unlimited, deduct time already used -----------}
  if LineCfg^.LimitsInfo^.lTime <> UnlimitedValue then
   with LineCfg^ do
    begin
      if LongInt(Longint(LimitsInfo^.LTime) - Longint(LineCfg^.Exitinfo^.Userinfo.Elapsed))>0 then
        LineCfg^.Exitinfo^.TimeLimit := LimitsInfo^.LTime - LineCfg^.Exitinfo^.Userinfo.Elapsed
         else LineCfg^.Exitinfo^.TimeLimit := 0;
    end; { if }

  {-- Now set the download kbyte limit -------------------------------------}
  Case LineCfg^.Exitinfo^.Baud of
       0..299   : LineCfg^.UsersKBLimit := LineCfg^.LimitsInfo^.LLocal;
     300..1199  : LineCfg^.UsersKbLimit := LineCfg^.LimitsInfo^.L300;
    1200..2399  : LineCfg^.UsersKbLimit := LineCfg^.LimitsInfo^.L1200;
    2400..4799  : LineCfg^.UsersKbLimit := LineCfg^.LimitsInfo^.L2400;
    4800..7199  : LineCfg^.UsersKbLimit := LineCfg^.LimitsInfo^.L4800;
    7200..9599  : LineCfg^.UsersKbLimit := LineCfg^.LimitsInfo^.L7200;
    9600..11999 : LineCfg^.UsersKbLimit := LineCfg^.LimitsInfo^.L9600;
   12000..14399 : LineCfg^.UsersKbLimit := LineCfg^.LimitsInfo^.L12000;
   14400..16799 : LineCfg^.UsersKbLimit := LineCfg^.LimitsInfo^.L14400;
   16800..19199 : LineCfg^.UsersKbLimit := LineCfg^.LimitsInfo^.L16800;
   19200..21599 : LineCfg^.UsersKbLimit := LineCfg^.LimitsInfo^.L19200;
   21600..26399 : LineCfg^.UsersKbLimit := LineCfg^.LimitsInfo^.L21600;
   26400..28799 : LineCfg^.UsersKbLimit := LineCfg^.LimitsInfo^.L26400;
   28800..31199 : LineCfg^.UsersKbLimit := LineCfg^.LimitsInfo^.L28800;
   31200..33599 : LineCfg^.UsersKbLimit := LineCfg^.LimitsInfo^.L31200;
   33600..38399 : LineCfg^.UsersKBLimit := LineCfg^.LimitsInfo^.L33600;
   38400..57599 : LineCfg^.UsersKbLimit := LineCfg^.LimitsInfo^.L38400;
   57600..63999 : LineCfg^.UsersKbLimit := LineCfg^.LimitsInfo^.L57600;
   64000..65535 : LineCfg^.UsersKbLimit := LineCfg^.LimitsInfo^.L64000
     else LineCfg^.UsersKbLimit := LineCfg^.LimitsInfo^.L115200;
  end; { Case }

  if LineCfg^.Exitinfo^.Baud=11520 then
    LineCfg^.UsersKbLimit := LineCfg^.LimitsInfo^.L115200;

  {$IFDEF WITH_FULL}
    if NOT IgnoreTime then
      begin
        ChangeTime(0, true);
        DoEventWarning(LineCfg^.Exitinfo^);
      end; { if }
  {$ENDIF}
end; { proc. GetLevelLimitsInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

{$IFDEF WITH_FULL}
procedure PickSecurity;       { Let SysOp give the user a new security level }
var TempStr  : String;
    SaveScrn : Pointer;
begin
{$IFNDEF WINGUI}
  SaveScreen(SaveScrn);
  LineCfg^.LocalOnly := true;

  repeat
    TempStr := FStr(LineCfg^.Exitinfo^.Userinfo.Security);
    EditBox(TempStr, 25, 11, 55, 15, 'Security', 5, false, false);

    If FVal(TempStr) > 65535 then
      begin
        Write(^G);
        TempStr := '';
      end; { Invalid security level }

    if TempStr <> '' then
      LineCfg^.Exitinfo^.Userinfo.Security := FVal(TempStr);
  until (TempStr <> '');

  GetLevelLimitsInfo(LineCfg^.Exitinfo^.Userinfo.Security, true, false);
  UpdateUserRecord;                                 { Update user information }
  LineCfg^.LocalOnly := false;

  RestoreScreen(SaveScrn);
  {$IFDEF WITH_FULL}
    StatusDisplay(11, false);
    StatusDisplay(99, false);
  {$ENDIF}
{$ENDIF}
end; { proc. PickSecurity }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
procedure SetTime(Left: Word);                            { Time set left }
begin
  if LineCfg^.LimitsInfo^.lTime = UnlimitedValue then
    begin
      LineCfg^.Exitinfo^.Userinfo.Elapsed := 0;
      EXIT;
    end; { if }

  LineCfg^.Exitinfo^.Userinfo.Elapsed := Integer(LineCfg^.LimitsInfo^.LTime - Left);
  LineCfg^.Exitinfo^.TimeLimit := LineCfg^.LimitsInfo^.LTime - LineCfg^.Exitinfo^.Userinfo.Elapsed;

  {$IFDEF WITH_FULL}
    StatusDisplay(99, false);
  {$ENDIF}
end; { proc. SetTime }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

{$IFDEF WITH_FULL}
procedure ChangeTime(Delta: Longint; DoCheck: Boolean);          { ChangeTime }
begin
  if LineCfg^.LimitsInfo^.lTime = UnlimitedValue then
    begin
      LineCfg^.Exitinfo^.Userinfo.Elapsed := LineCfg^.EventDeducted;
      EXIT;
    end; { if }

  LineCfg^.Exitinfo^.Userinfo.Elapsed := SmallInt(LineCfg^.Exitinfo^.Userinfo.Elapsed - Delta);

  if Longint(Longint(LineCfg^.Limitsinfo^.LTime) - Longint(LineCfg^.Exitinfo^.Userinfo.Elapsed)) < 0 then
    begin
      LineCfg^.Exitinfo^.TimeLimit := 00;
      if DoCheck then contrObj^.CheckAll;
      EXIT;
    end; { if }

  LineCfg^.Exitinfo^.TimeLimit := SmallWord(LineCfg^.LimitsInfo^.LTime - LineCfg^.Exitinfo^.UserInfo.Elapsed);

  if DoCheck then contrObj^.CheckAll;
end; { proc. ChangeTime }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

{$IFDEF WITH_FULL}
procedure LockOutUsers;                { Lock current user out of the system }
begin
  LineCfg^.Exitinfo^.Userinfo.Security := 00;
  SetTime(03);
  LineCfg^.DispMorePrompt := False;

  if DisplayHotFile('LOCKOUT', [])=#01 then
    begin
       WriteLn;
       WriteLn;
       Writeln(LangObj^.ralGet(ralLocOut));
       WriteLn(LangObj^.ralGet(ralLocCompl));
       WriteLn;
       HangUp;
       Halt(0);
    end; { LOCKOUT.ANS/ASC/AVT not found }
end; { proc. LockOutUsers }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

end. { unit LIMIT_U }
