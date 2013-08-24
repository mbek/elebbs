unit SysCodes;
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
** EleBBS systemcodes routines for EleBBS
**
** Copyright (c) 1996, 1998 by Maarten Bekers
**
** Created : 11-Oct-1998
** Last update : 11-Oct-1998
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses Global;

function RaSystemCodesStr(C: Char;
                          var WasNumeric: Boolean;
                          var NrOfDots  : Longint): String;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses CfgRec, FileRout,
        Mail,
      {$IFDEF WITH_FULL}
        Crt,
        TagUnit,
        OutBlock,
        InOut_U,
        ExitProg,
      {$ENDIF}

        LongStr, GenFile, JDates, RAL, Stutils, Colors,
         ObjDec;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function RaSystemCodesStr(C: Char;
                          var WasNumeric: Boolean;
                          var NrOfDots  : Longint): String;
var TempAttr   : byte;
    TempFore   : byte;
    TempBack   : byte;

    FirstMsgNum: Longint;
    HighMsgNum : Longint;
    ActiveMsgs : Longint;
begin
  RaSystemCodesStr := '';
  C := UpCase(c);

  WasNumeric := false;
  if C in ['$', '&', '''', '0', '1', '2', 'A', 'D', 'E', 'F', 'H', 'K',
           'L', 'M', 'N', 'O', 'Q', 'R', 'T', 'U', 'W'] then
             WasNumeric := true;

  if UpCase(C) in ['1', 'Y'] then
   begin
     if LineCfg^.CurMessageRecord^.AreaNum <> LineCfg^.Exitinfo^.Userinfo.MsgArea then
      begin
        GetMessageRecord(LineCfg^.CurMessageRecord^, LineCfg^.Exitinfo^.Userinfo.MsgArea, True);
        GetEleMessageRecord(LineCfg^.CurEleMsgRecord^, LineCfg^.Exitinfo^.Userinfo.MsgArea, True);
        LineCfg^.CurMsgNum := Ra250MsgArea(LineCfg^.CurMessageRecord^.AreaNum);
      end; { if }
   end; { if }

  if Upcase(C) in ['2', 'Z'] then
    begin
      if LineCfg^.CurFilesRecord^.AreaNum <> LineCfg^.Exitinfo^.Userinfo.FileArea then
       begin
         GetFilesRecord(LineCfg^.CurFilesRecord^, LineCfg^.Exitinfo^.Userinfo.FileArea, True);
         LineCfg^.CurFileNum := Ra250Area(LineCfg^.CurFilesRecord^.AreaNum);
       end; { if }
    end; { if }

  {.$IFDEF WITH_FULL}
    if C in ['0', 'D', 'E', 'C'] then
      GetMsgAreaStats(LineCfg^.CurMessageRecord^,
                      LineCfg^.CurEleMsgRecord^,
                      ActiveMsgs, FirstMsgnum, HighMsgNum);
  {.$ENDIF}

  Case C of
 { Disp TextFile }  '!' : begin
                            FillChar(LineCfg^.Command_Array, SizeOf(LineCfg^.Command_Array), #00);
                            LineCfg^.Command_Counter := 00;
                            NrOfDots := 00;
                            LineCfg^.Command_Array[LineCfg^.Command_Counter] := C;
                            LineCfg^.ProcessCharType:= chrDisplayText;
                          end; { if DisplayTextFile }
 { Execute Q-A }    '@' : begin
                            FillChar(LineCfg^.Command_Array, SizeOf(LineCfg^.Command_Array), #00);
                            LineCfg^.Command_Counter := 00;
                            NrOfDots := 00;
                            LineCfg^.Command_Array[LineCfg^.Command_Counter] := C;
                            LineCfg^.ProcessCharType:= chrExecQA;
                          end; { Execute Q-A file }
 { Semaphore dir.}  '#' : RaSystemCodesStr := GlobalCfg^.RaConfig^.SemPath;
 { Callers Today }  '$' : RaSystemCodesStr := FStr(GetFileSize(LastCallFileName, SizeOf(LastcallRecord)));
 { Handler lastclr} '%' : RaSystemCodesStr := LineCfg^.Exitinfo^.Sysinfo.LastHandle;
{$IFDEF WITH_FULL}
 { Files Tagged }   '&' : RaSystemCodesStr := FStr(TaggingObj^.CurrentTagged);
 { KB Tagged }     '''' : RaSystemCodesStr := FStr(TaggingObj^.GetTotalKBsTagged);
{$ENDIF}
 { Language }       '(' : RaSystemCodesStr := LineCfg^.Language^.Name;
 { Nr.Msgs in area} '0' : RaSystemCodesStr := FStr(ActiveMsgs);
 { Curr. M-Area }   '1' : RaSystemCodesStr := FStr(LineCfg^.CurMsgNum);
 { Curr. F-Area }   '2' : RaSystemCodesStr := FStr(LineCfg^.CurFileNum);
 { Total Calls }    'A' : RaSystemCodesStr := FStr(LineCfg^.Exitinfo^.SysInfo.Totalcalls);
 { Lastcaller }     'B' : RaSystemCodesStr := LineCfg^.Exitinfo^.Sysinfo.Lastcaller;
 { Number in JAM }  'C' : RaSystemCodesStr := FStr(ActiveMsgs);
 { StartNr msgarea} 'D' : RaSystemCodesStr := FStr(FirstMsgNum);
 { EndNr msgarea }  'E' : RaSystemCodesStr := FStr(HighMsgNum);
 { Nr.Pages SysOp } 'F' : RaSystemCodesStr := FStr(LineCfg^.Exitinfo^.NumberPages);
 { DayOfWeek }      'G' : RaSystemCodesStr := LangObj^.DayOfWeek(True);
 { Number of users} 'H' : RaSystemCodesStr := FStr(GetFileSize(GlobalCfg^.RaConfig^.MsgBasepath + UserBaseName,
                                                   SizeOf(UsersRecord)));
 { Current time }   'I' : RaSystemCodesStr := TimeStr(False, False);
 { Current date }   'J' : RaSystemCodesStr := LangObj^.RaFormatDate(DateStr, 0, y2k_SysDate,
                                                 LineCfg^.Exitinfo^.Userinfo.DateFormat);
 { Mins Connected } 'K' : RaSystemCodesStr := FStr(LineCfg^.OnlineTime);
 { Secs connected } 'L' : RaSystemCodesStr := FStr(00);   { Always returns 00 }
 { Mins Used Today} 'M' : RaSystemCodesStr := FStr(LineCfg^.Exitinfo^.Userinfo.Elapsed);
 { Secs Used Today} 'N' : RaSystemCodesStr := FStr(00);   { Always returns 00 }
 { Mins remaining } 'O' : begin
                            if Longint(Longint(LineCfg^.Limitsinfo^.LTime) -
                               Longint(LineCfg^.Exitinfo^.Userinfo.Elapsed)) = UnlimitedValue then
                              RaSystemCodesStr := langObj^.ralGet(ralUnlimited)
                                else RaSystemCodesStr := FStr(LineCfg^.Exitinfo^.TimeLimit);
                          end; { 'O' }
 { Secs remaining } 'P' : RaSystemCodesStr := FStr(00);   { Always returns 00 }
 { Daily timelimit} 'Q' : begin
                            if Longint(Longint(LineCfg^.Limitsinfo^.LTime) -
                               Longint(LineCfg^.Exitinfo^.Userinfo.Elapsed)) = UnlimitedValue then
                              RaSystemCodesStr := langObj^.ralGet(ralUnlimited)
                                else RaSystemCodesStr := FStr(LineCfg^.LimitsInfo^.LTime);
                          end; { 'Q' }
 { Connect speed }  'R' : RaSystemCodesStr := FStr(FixBaud(LineCfg^.Exitinfo^.Baud));
 { DayOfWeek (ABV)} 'S' : RaSystemCodesStr := LangObj^.DayOfWeek(False);
 { Down-Limit }     'T' : begin
                           if LineCfg^.UsersKbLimit = UnlimitedValue then
                             RaSystemcodesStr := langObj^.ralGet(ralUnlimited)
                              else RaSystemCodesStr := FStr(LineCfg^.UsersKbLimit);
                          end; { 'T' }
 { Next Event }     'U' : RaSystemCodesStr := FStr(MinsTillTime(LineCfg^.Exitinfo^.EventInfo.StartTime));
 { 24 Hr format }   'V' : RaSystemCodesStr := LineCfg^.Exitinfo^.EventInfo.StartTime;
 { Nodenumber }     'W' : RaSystemCodesStr := FStr(LineCfg^.RaNodeNr);
{$IFDEF WITH_FULL}
 { Terminate call } 'X' : HangUp;
{$ENDIF}
 { Name of msgarea} 'Y' : RaSystemCodesStr := LineCfg^.CurMessageRecord^.Name;
 { name of filearea}'Z' : RaSystemCodesStr := LineCfg^.CurFilesRecord^.Name;
{$IFDEF WITH_FULL}
 { Color-Codes }    '[' : If LineCfg^.Command_Counter>=3 then
                          begin
                           TempAttr := ToDec(LineCfg^.Command_Array[2]+LineCfg^.Command_Array[3]);
                           GetColors(TempAttr, TempBack, TempFore);

                           RaSystemCodesStr :=
                             OutputObj^.GetAttrStr(MakeAttr(TempFore, TempBack), True);

                           LineCfg^.Command_Counter := 00;
                           LineCfg^.ProcessCharType := chrDisplay;
                          end { ColorCodes }
                           else begin
                                  { Command_Counter := 00; }
                                  LineCfg^.ProcessCharType:= chrRaSystemCodes;
                                end;
 { ClrEol }         '/' : RaSystemCodesStr := OutputObj^.MakeClrEolStr;
 { ClrEol }         '\' : RaSystemCodesStr := OutputObj^.MakeClrEolStr;
{$ENDIF}
 { Get-RalNr }      ']' : If LineCfg^.Command_Counter=4 then
                          begin
                            {$IFDEF WITH_FULL}
                              with LineCfg^ do
                                RaSystemCodesStr := LangObj^.RalGet(FVal(Command_Array[2]+Command_Array[3]+Command_Array[4]));
                            {$ELSE}
                              RaSystemCodesStr := 'Unsupported';
                            {$ENDIF}
                            LineCfg^.Command_Counter := 00;
                            LineCfg^.ProcessCharType := chrDisplay;
                          end { LangObj^.RalGet }
                           else begin
                                  LineCfg^.ProcessCharType := chrRaSystemCodes;
                                end;
  End; { Case }
end; { func. RaSystemCodesStr }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit SysCodes }
