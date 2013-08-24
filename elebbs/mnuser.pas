unit MnUser;
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
** Menu functions (change user) routines for EleBBS
**
** Copyright (c) 1996 by Maarten Bekers
**
** Created : 04-Jan-1998
** Last update : 22-Feb-1998
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF WITH_FULL}
  This unit shouldn''t be compiled for an utility program
{$ENDIF}

{$IFDEF WITH_FULL}
function ValidBirthDate(DateInput: String;
                        var DateFmt  : Byte;
                        const RealDOB  : Boolean;
                        var Months,
                            Days,
                            Years    : Word): Boolean;
function  ValidPhone(S: String): Boolean;
function  ValidHandle(TempStr: String): Boolean;
procedure SelectProtocol;               { Change the default protocol setting }
procedure ChangeHandle(MiscData: String);               { Change users handle }
procedure AskForMailAddress(CompleteNew: Boolean);   { Get users mail-address }
procedure ToggleForwarding(OptData: String); { Toggle auto message forwarding }
procedure AskForUsersSex;                                    { Asks users sex }
procedure GetPhoneNumber(var S: String; RalNr: Word);     { Get a phonenumber }
procedure AskDateFormat;                     { Get users preffered dateformat }
procedure GetNewPassword(CompleteNew, PwCheck: Boolean); { Get a new password }
procedure GetDateStr(var DateInput:String; ShowDOB: Boolean; Prompt: String; RealDOB,EmptyAbort: Boolean); { Get a birthdate }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
uses Global, Debug_U, DispAns, Ral, LineEd, StUtils, FileRout, ObjDec,
      Control, MenuFunc, Crc_Unit, Support, Cases, LongStr, CfgRec,
       CentrStr, WordStr, ExitProg, Input_U, User_U, UnixDate,
        Transfer, GenFile, JDates, Question, FileOBj, ScrList,
         elx_Bbs;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ValidPhone(S: String): Boolean;
var NrCount: Longint;
    Counter: Longint;
begin
  NrCount := 0;

  for Counter := 01 to Length(s) do
    if S[Counter] in ['0'..'9'] then
     Inc(NrCount);

  ValidPhone := (NrCount > 2);
end; { func. ValidPhone }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ValidHandle(TempStr: String): Boolean;
begin
  ValidHandle := true;
  TempStr := SupCase(Trim(TempStr));

  if (SearchUser(TempStr) > -1) OR
      (SearchCtlFile('handles.ctl', TempStr, false)) OR
       (TempStr = 'SYSOP') then ValidHandle := false;
end; { func. ValidHandle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function ValidBirthDate(DateInput: String;
                        var DateFmt  : Byte;
                        const RealDOB  : Boolean;
                        var Months,
                            Days,
                            Years    : Word): Boolean;
begin
  Case DateFMT of
    01 : begin
           if Length(DateInput) < 8 then DateInput := '';

           Days := Byte(FVal(Copy(DateInput, 1, 2)));
           Months := Byte(FVal(Copy(DateInput, 4, 2)));
           Years := Byte(FVal(Copy(DateInput, 7, 2)));
         end; { if }
    02 : begin
           if Length(DateInput) < 8 then DateInput := '';

           Months := Byte(FVal(Copy(DateInput, 1, 2)));
           Days := Byte(FVal(Copy(DateInput, 4, 2)));
           Years := Byte(FVal(Copy(DateInput, 7, 2)));
         end; { if }
    03 : begin
           if Length(DateInput) < 8 then DateInput := '';

           Years := Byte(FVal(Copy(DateInput, 1, 2)));
           Months := Byte(FVal(Copy(DateInput, 4, 2)));
           Days := Byte(FVal(Copy(DateInput, 7, 2)));
         end; { if }
    05 : begin
           if Length(DateInput) < 10 then DateInput := '';

           Days := Byte(FVal(Copy(DateInput, 1, 2)));
           Months := Byte(FVal(Copy(DateInput, 4, 2)));
           Years := FVal(Copy(DateInput, 9, 2));
         end; { if }
    06 : begin
           if Length(DateInput) < 10 then DateInput := '';

           Months := Byte(FVal(Copy(DateInput, 1, 2)));
           Days := Byte(FVal(Copy(DateInput, 4, 2)));
           Years := FVal(Copy(DateInput, 9, 2));
         end; { if }
    07 : begin
           if Length(DateInput) < 10 then DateInput := '';

           Years := Byte(FVal(Copy(DateInput, 3, 2)));
           Months := Byte(FVal(Copy(DateInput, 6, 2)));
           Days := FVal(Copy(DateInput, 9, 2));
         end; { if }
  end; { case }

  if (NOT (Months in [1..12])) then DateInput := '';   { Standard month check }
  if (NOT (Days in [1..31])) then DateInput := '';      { Standard days check }
  if (Months = 2) then                     { Special leap-year considerations }
    begin
      if (Days > 29) then DateInput := '';           { This is always invalid }
      if (Days = 29) then
        if NOT IsLeapYear(Years) then DateInput := '';      { Not a leap year }
    end; { if }

  if RealDOB then
    if (Years > (GetYear - 4)) then DateInput := '';

  ValidBirthDate := (DateInput <> '');
end; { func. ValidBirthDate }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure ChangeHandle(MiscData: String);               { Change users handle }
var TempStr: String;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMenuFunc, 'ChangeHandle (Start) (MiscData="'+MiscData+'")');
  {$ENDIF}

  DisplayHotFile('HANDLE', []);

  repeat
    WriteLn;
    Write('`A9:', LangObj^.ralGet(ralAskHandle));

    TempStr := '';
    GetString(TempStr, 30, [#32..#254] - NonUserNameChars, GlobalCfg^.Elconfig^.CapitalizeUsername, False, False, False);
    WriteLn;

    TempStr := Trim(TempStr);
    If TempStr='' then TempStr := LineCfg^.Exitinfo^.Userinfo.Name;

    If SUpCase(TempStr) <> SUpcase(LineCfg^.Exitinfo^.Userinfo.Name) then
     begin
       if NOT ValidHandle(TempStr) then
            begin
              WriteLn;
              Writeln('`A12:', LangObj^.ralGet(ralInvHandle));
              TempStr := '';
            end; { found }
     end; { Not it's own }

  until (TempStr <> '');

  LineCfg^.Exitinfo^.Userinfo.Handle := Trim(TempStr);
  UpdateUserRecord;
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMenuFunc, 'ChangeHandle ( End )');
  {$ENDIF}
end; { proc. ShowHandle }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
Procedure SelectProtocol;
var GoodKeys   : CharSet;
    InternKeys : CharSet;
    CH         : Char;
    Protocol_F : pFileObj;
    ProtInf    : ProtocolRecord;
    TempStr    : String;
    OldFMode   : Longint;
    DontDisplay: Boolean;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMenuFunc, 'SelectProtocol');
  {$ENDIF}

  DontDisplay := (DisplayHotFile('XFERPROT', []) = #01);

  If DontDisplay then
   begin
      OutputObj^.ClearScreen;
      WriteLn('`A3:', LangObj^.ralGet(ralSelProt));
      Writeln;
   end; { DontDisplay }

  TempStr := '';

  New(Protocol_F, Init);
  Protocol_F^.Assign(ProtocolFileName);
  Protocol_F^.FileMode := ReadMode + DenyNone;

  if NOT Protocol_F^.Open(1) then
    begin
      Dispose(Protocol_F, Done);
      EXIT;
    end; { if }

  GetInternKeys(InternKeys);
  If DontDisplay then
   begin;
     If 'X' in InternKeys then Writeln('`A15:(X)  `A12:Xmodem');
     If '1' in InternKeys then Writeln('`A15:(1)  `A12:Xmodem/1K');
     If 'Q' in InternKeys then Writeln('`A15:(Q)  `A12:Xmodem/1K-G');
     If 'Y' in InternKeys then Writeln('`A15:(Y)  `A12:Ymodem       `X24:`A7:(*)');
     If 'G' in InternKeys then Writeln('`A15:(G)  `A12:Ymodem-G     `X24:`A7:(*)');
     If 'Z' in InternKeys then Writeln('`A15:(Z)  `A12:Zmodem       `X24:`A7:(*)');
   end; { DontDisplay }

  While NOT Protocol_F^.EOF do
    begin
      Protocol_F^.BlkRead(ProtInf, SizeOf(ProtocolRecord));
      if Protocol_F^.IoResult > 00 then BREAK;

      If (ProtInf.Name<>'') AND (ProtInf.Attribute in [01, 02]) then
       begin;
        If (ProtInf.Attribute=1) OR                                  { Available }
         ((ProtInf.Attribute=2) AND (LineCfg^.Exitinfo^.ErrorFreeConnect)) then
          begin;
           If DontDisplay then
            begin;
              Write ('`A15:(',ProtInf.ActiveKey,')  `A12:',MakeLen(ProtInf.Name, 15, Space, False, false),' ');
              If ProtInf.BatchAvailable then Writeln('`X24:`A7:(*)')
                else Writeln;
            end; { DontDisplay }

           TempStr := TempStr + ProtInf.ActiveKey;
          end; { Available }
       end; { ProtInf.Name }
    end; { While }

  Dispose(Protocol_F, Done);

  Str2Set(TempStr, GoodKeys);

  if DontDisplay then
    begin
      WriteLn('`A3:');
      WriteLn(LangObj^.ralGet(ralAstrBatch));
    end; { DontDisplay }

  WriteLn('`A3:');
  Write  (LangObj^.ralGet(ralProtocol2));

  OutputObj^.ResetLines(01);

  repeat
    CH := UpCase(InputObj^.ReadKey);
    if ProgTerminated then Hangup;

    if Ch='?' then
      DisplayHotFile('XFERHELP', []);
  until (CH in GoodKeys) OR (CH=#13) OR (CH in InternKeys) OR (ProgTerminated);

  GetProtocolRecord(CH, ProtInf);
  GetInternKeys(InternKeys);

  if (CH in InternKeys) then
     Case UpCase(CH) of
       'X' : WriteLn('Xmodem');
       '1' : WriteLn('Xmodem/1K');
       'Q' : WriteLn('Xmodem/1K-G');
       'Y' : WriteLn('Ymodem');
       'G' : WriteLn('Ymodem-G');
       'Z' : WriteLn('Zmodem');
     end; { case }

  if (NOT (CH in InternKeys)) then
   if CH <> #13 then
     begin
       GetProtocolRecord(CH, ProtInf);
       Writeln(ProtInf.Name);
     end; { if }

  If CH <> #13 then
    begin
      InputObj^.PressEnter(True, False);
      LineCfg^.Exitinfo^.Userinfo.DefaultProtocol := UpCase(CH);
    end; { if }

  if InternKeys=[] then ProtInf.Name := '[No protocols available]';

  WriteLn;
  WriteLn;
end; { proc. SelectProtocol }
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure AskForMailAddress(CompleteNew: Boolean);
begin;
  {$ifdef With_Debug} DebugObj.DebugLog(logMenuFunc, 'AskForMailAddress');{$endif}

  If NOT CompleteNew then
    begin;
      Writeln;
      WriteLn;
      WriteLn;
      If (LineCfg^.Exitinfo^.Userinfo.Address1='') AND
          (LineCfg^.Exitinfo^.Userinfo.Address2='') AND
           (LineCfg^.Exitinfo^.Userinfo.Address3='') then WriteLn('`A14:',LangObj^.ralGet(ralNoMailI))
            else begin
                  Writeln('`A2:',LangObj^.ralGet(ralMaListed));
                  WriteLn;
                  WriteLn('`A3:',LineCfg^.Exitinfo^.Userinfo.Address1);
                  WriteLn('`A3:',LineCfg^.Exitinfo^.Userinfo.Address2);
                  WriteLn('`A3:',LineCfg^.Exitinfo^.Userinfo.Address3);

                  If NOT InputObj^.RalStrYesNoAsk(ralChangeMA) then Exit;
                  Writeln;
                 end; { Mail Address change }
    end; { CompleteNew }

  LineCfg^.Exitinfo^.Userinfo.Address1 := '';
  LineCfg^.Exitinfo^.Userinfo.Address2 := '';
  LineCfg^.Exitinfo^.Userinfo.Address3 := '';

  WriteLn;
  WriteLn('`A3:',LangObj^.ralGet(ralMailAdr));
  Write('`A3:1: '); GetString(LineCfg^.Exitinfo^.Userinfo.Address1, 50, [#32..#254], False, False, False, False);
  Write(#10#13,'`A3:2: '); GetString(LineCfg^.Exitinfo^.Userinfo.Address2, 50, [#32..#254], False, False, False, False);
  Write(#10#13,'`A3:3: '); GetString(LineCfg^.Exitinfo^.Userinfo.Address3, 50, [#32..#254], False, False, False, False);
  Writeln;
end; { AskForMailAddress }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Procedure ToggleForwarding(OptData: String);
var BrowseUserList : Boolean;
    SearchStr: String;
    Ready: Boolean;
begin
  {$ifdef With_Debug} DebugObj.DebugLog(logMenuFunc, 'ToggleForwarding');{$endif}

  If LineCfg^.Exitinfo^.Userinfo.ForwardTo<>'' then
   with LineCfg^ do
    begin
      Exitinfo^.Userinfo.ForwardTo := '';
      WriteLn;
      WriteLn;
      Writeln('`A10:', LangObj^.ralGet(ralForwDis));
      WriteLn;
      InputObj^.PressEnter(False, True);
      Exit;
    end; { ForwardTo }

  If Pos('/U', SUpcase(OptData))>00 then
    BrowseUserList := True
     else BrowseUserList := False;

  WriteLn;
  Repeat;
    Ready:=False;
    Writeln;
    SearchStr:='';

    Write('`A11:', LangObj^.ralGet(ralNominate));
    GetString(SearchStr, 35, [#32..#254], True, False, False, False);

    If SUpcase(SearchStr)=SUpcase(LineCfg^.Exitinfo^.Userinfo.Name) then SearchStr:='';
    If (SearchStr='') then Ready:=True;

    Writeln;
    If SearchUser(SearchStr)>-1 then Ready:=True
      else begin;
             If NOT BrowseUserList then
              begin;
               WriteLn;
               WriteLn('`A11:',LangObj^.ralGet(ralInvUser));
              end
               else begin;
                       WriteLn('`A11:');
                       If InputObj^.ralStrYesNoAsk(ralInvAskBrw) then
                           ShowUserList('');
                       WriteLn;
                    end; { BrowseUserList }
           end; { user not found }
  Until (Ready);

  If SearchStr<>'' then begin;
                          WriteLn;
                          WriteLn('`A10:', LangObj^.ralGet(ralForwEnabl));
                          Writeln;
                          InputObj^.PressEnter(False, True);
                          LineCfg^.Exitinfo^.Userinfo.ForwardTo := SearchStr;
                        end; { SearchStr }

end; { proc. ToggleForwarding }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure AskForUsersSex;
var CH       : Char;
    ValidKeys: CharSet;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMenuFunc, 'AskForUsersSex (begin)');
  {$ENDIF}

  Write('`A3:',LangObj^.ralGetStr(ralYourSex));
  Str2Set(LangObj^.ralGetKeys(ralYourSex), ValidKeys);

  repeat
    CH := UpCase(InputObj^.ReadKey);
    LineCfg^.Exitinfo^.Userinfo.Sex := 00;

    if UpCase(CH)=Copy(LangObj^.ralGetKeys(ralYourSex), 1, 1) then LineCfg^.Exitinfo^.Userinfo.Sex := 01;        { Male }
    if UpCase(CH)=Copy(LangObj^.ralGetKeys(ralYourSex), 2, 1) then LineCfg^.Exitinfo^.Userinfo.Sex := 02;      { FeMale }
  until (CH in ValidKeys) OR (ProgTerminated);

  WriteLn;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMenuFunc, 'AskForUsersSex ( end )');
  {$ENDIF}
end; { proc. AskforUserSex }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure GetPhoneNumber(var S: String; RalNr: Word);
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMenuFunc, 'GetPhoneNumber (begin)');
  {$ENDIF}

  repeat
    S := '';
    WriteLn('`A10:', LangObj^.ralGet(ralNr));
    MustEdit(S, LangObj^.ralGet(RalNumber), 15, False, False, 10, 00, ['0'..'9','-'], 4);

    Writeln;
    Writeln('`A12:',LangObj^.ralGet(RalNumEnter), S);
    If NOT InputObj^.ralStrYesNoAsk(Ralcorrect) then S := '';

    if NOT ValidPhone(S) then S := '';
    WriteLn;
  until (S <> '') OR (ProgTerminated);

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logMenuFunc, 'GetPhoneNumber ( end )');
  {$ENDIF}
end; { proc. GetPhoneNumber }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure AskDateFormat;
var Temp: String;
begin
  {-- Display file DATEFMT, if it doesnt exist, fallback to internal display --}
  if DisplayHotFile('DATEFMT', []) = #1 then
    begin
      WriteLn;
      WriteLn;
      WriteLn('`A10:', LangObj^.ralGet(ralAvailDate));
      WriteLn;
      WriteLn('`A15:1`A2:) ', LangObj^.ralGet(ralDDMMYY));
      WriteLn('`A15:2`A2:) ', LangObj^.ralGet(ralMMDDYY));
      WriteLn('`A15:3`A2:) ', LangObj^.ralGet(ralYYMMDD));
      WriteLn('`A15:4`A2:) ', LangObj^.ralGet(ralDDMmmYY));
      WriteLn('`A15:5`A2:) ', LangObj^.ralGet(ralDDMMYYYY));
      WriteLn('`A15:6`A2:) ', LangObj^.ralGet(ralMMDDYYYY));
      WriteLn('`A15:7`A2:) ', LangObj^.ralGet(ralYYYYMMDD));
      WriteLn('`A15:8`A2:) ', LangObj^.ralGet(ralDDMmmYYYY));
      WriteLn;
    end; { DisplayFile }

  {-- Get the users choice ----------------------------------------------------}
  Temp := '';
  Write(LangObj^.ralGet(RalSelect));
  GetString(Temp, 1, ['1','2','3','4','5','6','7','8'], false, false, false, false);

  {-- Display the choice, if empty we default to "5" --------------------------}
  if Temp = '' then
    Temp := '5';
  WriteLn(Temp);

  {-- Set the actual choice ---------------------------------------------------}
  LineCfg^.Exitinfo^.Userinfo.DateFormat := Byte(FVal(Temp));
end; { proc. AskDateFormat }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure GetDateStr(var DateInput:String; ShowDOB: Boolean; Prompt: String; RealDOB,EmptyAbort: Boolean);
var DateFMT   : Byte;
    Question  : QuestObj;
    RunScript : Boolean;
    DateFmtStr: String;
    TmpArray  : Array[0..2] of String;
    Days,
    Months,
    Years     : Word;
    elxObj    : pElxBbsObj;
begin
  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMenuFunc, 'GetDateStr (begin)');
  {$ENDIF}

  if ShowDOB then
    DisplayHotFile('DOBCHECK', []);

  {-------------------------- Determine the dateformat ------------------------}
  DateFmt := 01;
  Case LineCfg^.Exitinfo^.Userinfo.DateFormat of
    { DD-MM-YY }    01 : DateFMT := 01;
    { MM-DD-YY }    02 : DateFMT := 02;
    { YY-MM-DD }    03 : DateFMT := 03;
    { DD-Mmm-YY }   04 : DateFMT := 01;
    { DD-MM-YYYY }  05 : DateFMT := 05;
    { MM-DD-YYYY }  06 : DateFMT := 06;
    { YYYY-MM-DD }  07 : DateFMT := 07;
    { DD-Mmm-YYYY } 08 : DateFMT := 05;
  end; { case }

  {------------- Repeat asking the date until it's the way we like it! --------}
  REPEAT
    DateInput := '';
    RunScript := FALSE;

    if GetScriptType('DATEDIT') = 'Q-A' then
      begin
        Question.Init;
        Question.Qinfo^.Answers^.Put(01, FStr(DateFmt));
        Question.Qinfo^.Answers^.Put(02, Prompt);

        if DateFMT in [1..4] then
          Question.Qinfo^.Answers^.Put(03, LangObj^.ralGet((ralDDMMYY-01) + DateFmt))
           else Question.Qinfo^.Answers^.Put(03, LangObj^.ralGet((ralDDMMYYYY-01) + (DateFmt - 4)));

        Question.Process('DATEDIT /N', false, '');
        RunScript := (Question.GetError = 0);
        DateInput := Question.GetReturnResult;

        Question.Done;
      end { if }
        else begin
               {-- Extract the exact dateformat ------------------------------}
               if DateFMT in [1..4] then
                 DateFmtStr := LangObj^.ralGet((ralDDMMYY-01) + DateFmt)
                  else DateFmtStr := LangObj^.ralGet((ralDDMMYYYY-01) + (DateFmt - 4));

               {-- Create the textfile ---------------------------------------}
               TmpArray[0] := FStr(DateFmt);
               TmpArray[1] := Prompt;
               TmpArray[2] := DateFmtStr;

               CreateTextFile('datedit.inf', TmpArray);

               {-- run the script --------------------------------------------}
               New(elxObj, Init);
               RunScript := elxObj^.RunElexerScript('datedit', '', false);
               Dispose(elxObj, Done);

               {-- erase the text file ---------------------------------------}
               EraseFile('datedit.inf');
             end; { else }

    if NOT RunScript then
      begin
        WriteLn;

        if DateFMT in [1..4] then
          begin
            Write(Prompt, '(', LangObj^.ralGet((ralDDMMYY-01)+DateFmt),'): ');
            GetString(DateInput, 8, ['0'..'9', '/', '-'], false, false, false, false);
          end
            else begin
                   Write(Prompt, '(', LangObj^.ralGet((ralDDMMYYYY-01)+ (DateFmt - 4)),'): ');
                   GetString(DateInput, 10, ['0'..'9', '/', '-'], false, false, false, false);
                 end; { else }

        WriteLn;
      end; { if }

    if NOT RealDOB then
     if EmptyAbort then
       if DateInput = '' then EXIT;

    if DateFMT <> 7 then                                      { YYYY-MM-DD }
      begin
        DateInput[3] := '-';
        DateInput[6] := '-';
      end
        else begin
               DateInput[5] := '-';
               DateInput[8] := '-';
             end; { *IS* YYYY-MM-DD }

    {---------------------- Convert the date to numbers -----------------------}
    if NOT ValidBirthDate(DateInput, DateFmt, RealDOB,
                          Months, Days, Years) then
      begin
        if RealDOB then
          begin
            DateInput := '';

            WriteLn;
            WriteLn('`A12:', LangObj^.ralGet(ralInvBirth));
          end { if }
            else begin
                   WriteLn;
                   WriteLn;
                   WriteLn('`A14:', LangObj^.ralGet(ralInvDate));
                   DateInput := '';
                 end; { else }
      end; { if }

  UNTIL (DateInput <> '') OR (Progterminated);

  DateInput := LeadingZero(Months, 02) + '-' +
               LeadingZero(Days, 02) + '-' +
               LeadingZero(Years, 02);

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logMenuFunc, 'GetDateStr ( end )');
  {$ENDIF}
end; { proc. GetDateStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure GetNewPassword(CompleteNew, Pwcheck: Boolean);
var TempPassWord: String;
    TempPassWord2: String;
begin
 {$ifdef With_Debug} DebugObj.DebugLog(logMenuFunc, 'GetNewPassword');{$endif}

 if LineCfg^.GuestUser then
   begin
      WriteLn;
      WriteLn('`A12:', LangObj^.ralGet(ralNGSC));
      InputObj^.PressEnter(false, true);
      EXIT;
   end; { if }

 If (NOT CompleteNew) AND (NOT PwCheck) then
  begin;
    WriteLn;
    WriteLn;
    Write('`A14:',LangObj^.ralGet(ralCurrPsw));

    TempPassword := '';
    GetString(TempPassWord, 15, [#32..#254], True, True, False, False);
    WriteLn;
    TempPassWord := SupCase(TempPassWord);

    If RaCrc(TempPassWord, true)<>LineCfg^.Exitinfo^.Userinfo.PassWordCRC then
     begin;
       WriteLn;
       WriteLn(LangObj^.ralGet(ralNoAccess));
       WriteLn;
       InputObj^.PressEnter(True, TRue);
       Exit;
     end; { RaCrc }
  end; { CompleteNew }

 If NOT PwCheck then
  DisplayHotFile('PassWord', []);

 Repeat
    TempPassWord := '';
    WriteLn;
    Write('`A14:', LangObj^.ralGet(ralAskPsw1));

    GetString(TempPassWord, 15, [#32..#254], True, True, False, False);
    WriteLn;
    TempPassWord := SupCase(TempPassWord);

    WriteLn;

    If GlobalCfg^.RaConfig^.StrictPwdChecking then
      begin;
        If SearchCtlFile('pwdtrash.ctl', TempPassword, False) then
            TempPassword := '';

        If SUPcase(TempPassword)=LineCfg^.Exitinfo^.Userinfo.Name then
          TempPassword := '';

        If SUPcase(TempPassword)=ExtractWord(LineCfg^.Exitinfo^.Userinfo.Name, 1, defExtractWord, false, false) then
          TempPassword := '';
        If SUPcase(TempPassword)=ExtractWord(LineCfg^.Exitinfo^.Userinfo.Name, WordCount(LineCfg^.Exitinfo^.Userinfo.Name,
            DefExtractWord, true),
            defExtractWord, false, false) then TempPassword := '';
      end; { Strict Password Checking }

    If TempPassWord='' then WriteLn('`A14:',LangObj^.ralGet(ralInvPsw2));

    If TempPassWord<>'' then
     If Length(TempPassWord)<GlobalCfg^.RaConfig^.MinPwdLen then
         begin;
           WriteLn('`A14:',LangObj^.ralGet(ralPswShort1), ' ', GlobalCfg^.RaConfig^.MinPwdLen, ' ', LangObj^.ralGet(ralChars));
           TempPassWord := '';
         End; { if length TempPassWord }

    If TempPassword<>'' then
      If RaCrc(TempPassword, true) = LineCfg^.Exitinfo^.Userinfo.PasswordCRC then
        begin;
          WriteLn('`A12:', langObj^.ralGet(ralOtherPsw));
          TempPassword := '';
        end; { Can't use the same }

    If (TempPassWord='') AND (NOT CompleteNew) then
        If NOT PwCheck then  begin;
                               InputObj^.PressEnter(True, TRue);
                               Exit;
                             end; { TempPassWord }

    If TempPassWord<>'' then
     begin
        TempPassWord2 := '';
        Write('`A14:', LangObj^.ralGet(ralAskPsw2));
        GetString(TempPassWord2, 15, [#32..#254], True, True, False, false);
        WriteLn;
        TempPassWord2 := SupCase(TempPassWord2);
        If TempPassWord<>TempPassWord2 then
         begin;
          WriteLn;
          WriteLn('`A14:',LangObj^.ralGet(ralInvPsw1));
          TempPassWord := '';

          If (NOT CompleteNew) AND (NOT PWCheck) then
            begin;
              WriteLn;
              WriteLn;
              InputObj^.PressEnter(True, True);
              Exit;
            end; { CompleteNew }
         end; { Do Not Match }
     end; { TempPassWord }
 Until (TempPassWord<>'') OR (ProgTerminated);

 If GlobalCfg^.RaConfig^.SavePassWords then LineCfg^.Exitinfo^.Userinfo.PassWord := TempPassWord
  else LineCfg^.Exitinfo^.Userinfo.Password := '';
 LineCfg^.Exitinfo^.Userinfo.PassWordCRC := RaCrc(TempPassWord, true);

 If NOT CompleteNew then
  begin
     Writeln;
     Writeln;
     WriteLn(LangObj^.ralGet(ralPswChng2));
     Writeln;
     InputObj^.PressEnter(False, True);
  end; { CompleteNew }
end; { proc. GetNewPassWord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)
{$ENDIF}

end.
