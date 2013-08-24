unit StatusB;
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
** Statusbar unit for EleBBS
**
** Copyright (c) 1996,97 by Maarten Bekers
**
** Created : 12-Nov-1997
** Last update : 12-Nov-1997
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFNDEF WITH_FULL}
  This unit shouldn''t be compiled for an utility program
{$ENDIF}

{$IFDEF ISCGI}
  This unit should not be compiled into EleWEB
{$ENDIF}

{$IFDEF WITH_FULL}
procedure StatusDisplay(StatusType: Byte; DoneManual: Boolean);
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF WITH_FULL}
uses {$IFNDEF WINGUI}
       Crt,
       Avatar,
       ApTimer,
       ScrnU,
     {$ELSE}
      InOut_U, Win_Main, Avatar, ApTimer,
     {$ENDIF}

      Debug_U, FlagStr, Chat, RemScrn, ObjDec,

      Global, Sound_U, Jdates,
      FileSys, FileRout, RAL, FastScrn, Colors, LongStr,
      CfgRec, CentrStr, Limit_U, TagUnit;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure StatusDisplay(StatusType: Byte; DoneManual: Boolean);
var TempStr    : String;
    OldAttr    : Byte;
    OldX,OldY  : Byte;
    SaveDirect : Boolean;
    TempEvent  : String;
    TempAttr2  : Byte;
    SaveCodes  : Boolean;
    BpsStr     : String;
    WinStr     : Array[1..2] of String;
    OldStatLine: Byte;
{$IFDEF WINGUI}
    TextAttr   : Byte;
{$ENDIF}

procedure FastWrite(X, Y, Attr: Byte; TempStr: String);
var Counter: Byte;
begin
  {$IFNDEF WINGUI}
    if NOT GlobalCfg^.RaConfig^.LimitLocal then
      FastScrn.FastWrite(X, Y, Attr, TempStr);
  {$ENDIF}

  for Counter := X to ((X + Length(TempStr))-1) do
    WinStr[(Y-(mnuScrnLen-2)), Counter] := TempStr[Counter - (X-1)];
end; { proc. FastWrite }


begin
  {$IFDEF NOLINLOCAL}
    EXIT;
  {$ENDIF}

  {$IFDEF WITH_DEBUG}
     DebugObj.DebugLog(logStatus, 'StatusDisplay: '+FStr(Statustype));
  {$ENDIF}



  if DefStatusLine = 10 then                             { Started with -D }
    begin
      if (DoneManual) then defStatusLine := 01
        else EXIT;
    end; { if }

  OldStatLine := lineCfg^.Exitinfo^.StatusLine;
  if StatusType <> 99 then
      begin
        if StatusType>10 then Dec(StatusType, 10);
        if StatusType=lineCfg^.Exitinfo^.StatusLine then
          begin
{            EXIT; }
          end; { if }
      end; { if }
  If StatusType=99 then StatusType := lineCfg^.Exitinfo^.StatusLine;
  if NOT LineCfg^.ShowStatusBar then StatusType := 10;
  lineCfg^.Exitinfo^.StatusLine := StatusType;

  {$IFNDEF WINGUI}
  if StatusType <> 10 then
   if OldStatLine = 10 then
    if WhereY > (mnuScrnLen - 2) then
     begin
       AvtObj^.ScrollScrnRegionUp(01, 01, mnuScrnWidth, mnuScrnLen - 2, 2, true);
     end; { if }
  {$ENDIF}



  if NOT GlobalCfg^.RaConfig^.LimitLocal then
    begin
      if ((StatusType MOD 10) = 0) then
        begin
          LineCfg^.ActScrnLen := mnuScrnLen
        end { if }
         else begin
                LineCfg^.ActScrnLen := mnuScrnLen - 2;
              end; { if }
    end { if }
      else LineCfg^.ActScrnLen := mnuScrnLen;


  WinStr[1] := Dup(#32, 80);                 { Normal clearing for things }
  WinStr[2] := Dup(#32, 80);

  {$IFDEF WINGUI}
    Form1.SpinEdit1.Tag := 1;
    Form1.SpinEdit1.Value := lineCfg^.Exitinfo^.TimeLimit;
    Form1.SpinEdit1.Tag := 0;

    if StatusType in [1..8] then
      begin
        Form1.TabSet1.Tag := 1;
        Form1.TabSet1.TabIndex := (StatusType - 01);
        Form1.TabSet1.Tag := 0;
      end; { if }
  {$ENDIF}


 {$IFNDEF WINGUI}
   if NOT GlobalCfg^.RaConfig^.LimitLocal then
     begin
       OldAttr := TextAttr;
       SaveDirect := DirectScrnUpdate;
       DirectScrnUpdate := false;
       TextAttr := MakeAttr(GlobalCfg^.RaConfig^.StatFore, GlobalCfg^.RaConfig^.StatBack);

       OldX := Crt.WhereX;
       OldY := Crt.WhereY;
       Window(1, 1, mnuScrnWidth, mnuScrnLen);
       if StatusType <> 10 then
         FillArea(01, mnuScrnLen-1, mnuScrnWidth, mnuScrnLen, #32, TextAttr);
     end; { if }
 {$ENDIF}


  SaveCodes := LangObj^.ralCodes;                { Dates like: 12-Jun-1980 are from RAL }
  LangObj^.ralCodes := FALSE;

  with LineCfg^ do
  Case StatusType of
     01 : begin
            if Exitinfo^.ErrorFreeConnect then TempStr := '/'+Modem^.ErrorFreeString
              else TempStr := '';

            BpsStr := FStr(FixBaud(Exitinfo^.Baud)) + TempStr + ' BPS';
            if Exitinfo^.Baud = 65529 then
              BpsStr := 'telnet';

            FastWrite(01, mnuScrnLen- 01, TextAttr, Exitinfo^.Userinfo.Name + ' of '+Exitinfo^.Userinfo.Location +
                              ' at ' + BpsStr);
            FastWrite(70, mnuScrnLen- 01, TextAttr, '(Node '+FStr(RaNodeNr)+')');

            FastWrite(01, mnuScrnLen, TextAttr, 'Security: '+FStr(Exitinfo^.Userinfo.Security));

            if Longint(Longint(Limitsinfo^.LTime) - Longint(Exitinfo^.Userinfo.Elapsed)) = UnlimitedValue then
             FastWrite(18, mnuScrnLen, TextAttr, 'unlimited time')
              else FastWrite(18, mnuScrnLen, TextAttr, 'Time: '+Fstr(Exitinfo^.TimeLimit)+' mins');

            If Exitinfo^.SysOpNext then FastWrite(53, mnuScrnLen, TextAttr, '(SN)');
            if Exitinfo^.EMSI_Session then FastWrite(35, mnuScrnLen, TextAttr, '(IEMSI)');

            If (AnsiOn) AND (NOT RipOn) then FastWrite(42, mnuScrnLen, TextAttr, '(ANSI)');
            If (AvatarOn) AND (NOT RipOn) then FastWrite(48, mnuScrnLen, TextAttr, '(AVT)');
            if (RipOn) then FastWrite(42, mnuScrnLen, TextAttr, '(RIP)');

            If Exitinfo^.WantChat then
              FastWrite(57, mnuScrnLen, (((GlobalCfg^.RaConfig^.WindBack) SHL 4) OR GlobalCfg^.RaConfig^.WindFore) + 128,
                       '(Wants Chat)');

            {$IFNDEF WinGUI}
              FastWrite(70, mnuScrnLen, TextAttr, '(F9)=Help');
            {$ENDIF}

            Case ChatObj^.RaPageStat of
                PagingHours : If RaYell then FastWrite(60, mnuScrnLen - 01, TextAttr, '(PAGE ON)')
                               else FastWrite(60, mnuScrnLen - 01, TextAttr, '(PAGE OFF)');
                   PagingOn : begin;
                                TempAttr2 := TextAttr;
                                TextAttr := (GlobalCfg^.RaConfig^.WindBack SHL 4) OR GlobalCfg^.RaConfig^.WindFore;
                                FastWrite(60, mnuScrnLen - 01, TextAttr, '(PAGE ON)');
                                TextAttr := TempAttr2;
                              end; { PagingOn }
                 PagingOff : begin;
                                TempAttr2 := TextAttr;
                                TextAttr := (GlobalCfg^.RaConfig^.WindBack SHL 4) OR GlobalCfg^.RaConfig^.WindFore;
                                FastWrite(60, mnuScrnLen - 01, TextAttr, '(PAGE OFF)');
                                TextAttr := TempAttr2;
                              end; { PagingOff }
            End; { Case }
          End; { DorStatus=01 }

     02 : begin
            if UserAgeSave = 0 then
              GetUserAge(Exitinfo^.Userinfo.BirthDate, true);

            FastWrite(01, mnuScrnLen - 01, TextAttr, 'Voice#: '+Exitinfo^.Userinfo.VoicePhone);
            With Exitinfo^.Userinfo do
               FastWrite(26, mnuScrnLen - 01, TextAttr, 'Last Call: '+LastTime+' on '+
                  LangObj^.RaFormatDate(LastDate, 8, y2k_SysDate, 0));
            FastWrite(59, mnuScrnLen - 01, TextAttr, 'First Call:'+
                   LangObj^.RaFormatDate(Exitinfo^.Userinfo.FirstDate,8,y2k_SysDate, 0));

            FastWrite(01, mnuScrnLen, TextAttr, 'Data #: '+Exitinfo^.Userinfo.DataPhone);
            FastWrite(23, mnuScrnLen, TextAttr, 'Times Called: '+FStr(Exitinfo^.Userinfo.NoCalls));
            FastWrite(48, mnuScrnLen, TextAttr, 'Age: '+FStr(UserAgeSave));
            FastWrite(60, mnuScrnLen, TextAttr, 'Birthdate:'+
                   LangObj^.RaFormatDate(Exitinfo^.Userinfo.BirthDate, 8, y2k_UserDate, 0));
          End; { StatusBar 02 }

     03 : begin;
            FastWrite(01, mnuScrnLen - 01, TextAttr, 'Uploads: '+FStr(Exitinfo^.Userinfo.UploadsK)+'k ('+
                      FStr(Exitinfo^.Userinfo.Uploads)+')');
            FastWrite(25, mnuScrnLen - 01, TextAttr, 'Downloads: '+FStr(Exitinfo^.Userinfo.DownloadsK)+'k ('+
                      FStr(Exitinfo^.Userinfo.Downloads)+')');
            FastWrite(50, mnuScrnLen - 01, TextAttr, 'Tagged: '+FStr(TaggingObj^.GetTotalKbsTagged)+'k ('+
                      FStr(TaggingObj^.CurrentTagged)+')');
            FastWrite(01, mnuScrnLen, TextAttr, 'Flags: (A):'+Byte2Flags(Exitinfo^.Userinfo.Flags[1])+
                                   '  (B):'+Byte2Flags(Exitinfo^.Userinfo.Flags[2])+
                                   '  (C):'+Byte2Flags(Exitinfo^.Userinfo.Flags[3])+
                                   '  (D):'+Byte2Flags(Exitinfo^.Userinfo.Flags[4]));
          end; { StatusBar 03 }

     04 : Begin;
            FastWrite(01, mnuScrnLen - 01, TextAttr, 'Last Caller: '+Exitinfo^.SysInfo.Lastcaller);
            FastWrite(32, mnuScrnLen - 01, TextAttr, 'Total System Calls: '+fstr(Exitinfo^.SysInfo.TotalCalls));
            FastWrite(68, mnuScrnLen - 01, TextAttr, '(Time:'+JDates.TimeStr(False, False)+')');

            If Exitinfo^.EventInfo.StartTime='00:00' then TempEvent := 'none'
             else TempEvent := Exitinfo^.EventInfo.StartTime;
            With Exitinfo^.EventInfo do
              FastWrite(46, mnuScrnLen, TextAttr, 'Next Event: '+TempEvent+', errorlevel: '+FStr(ErrorLevel));

            If Snooping then FastWrite(20, mnuScrnLen, TextAttr, '(Local screen ON )')
             else FastWrite(20, mnuScrnLen, TextAttr, '(Local screen OFF)');

            { Printer logging not (yet) implemented }
          end; { If OwnStatusTel=04 then begin }

     05 : begin;
            FastWrite(01, mnuScrnLen - 01, TextAttr, 'Msgs posted    : '+FStr(Exitinfo^.Userinfo.MsgsPosted));
            FastWrite(28, mnuScrnLen - 01, TextAttr, 'Highread : '+FStr(Exitinfo^.Userinfo.LastRead));
            FastWrite(56, mnuScrnLen - 01, TextAttr, Language^.Name);
            FastWrite(70, mnuScrnLen - 01, TextAttr, 'Group '+FStr(Exitinfo^.Userinfo.Group));

            FastWrite(01, mnuScrnLen, TextAttr, 'Credit         : '+FStr(Exitinfo^.Userinfo.Credit));
            FastWrite(28, mnuScrnLen, TextAttr, 'Handle   : '+Exitinfo^.Userinfo.Handle);
          End; { If OwnStatusTel=05 then begin }

     06 : begin;
            FastWrite(01, mnuScrnLen - 01, TextAttr, Exitinfo^.Userinfo.Comment);

            if Exitinfo^.PageReason <> '' then
              begin
                FastWrite(01, mnuScrnLen, TextAttr, 'Chat (' + Exitinfo^.Userinfo.Name  + '): '+Exitinfo^.PageReason);
              end;
          end; { If OwnStatusTel=06 then begin }

     07 : If NOT Exitinfo^.Emsi_Session then FastWrite(01,mnuScrnLen - 01, TextAttr, 'No EMSI session established.')
            else begin;
                  FastWrite(01,mnuScrnLen - 01, TextAttr, 'Remote is using : '+Exitinfo^.EMSI_Software);
                  FastWrite(01,mnuScrnLen, TextAttr, Exitinfo^.Emsi_requests);
                 End; { Else }
     08 : begin;
            FastWrite(01, mnuScrnLen - 01, TextAttr, DorStatln[8,1]);
            FastWrite(01, mnuScrnLen, TextAttr, DorStatLn[8,2]);
          end; { StatusBar 08 }

     09 : begin
            FastWrite(01, mnuScrnLen - 01, TextAttr, DorStatLn[9,1]);
            FastWrite(01, mnuScrnLen, TextAttr, DorStatLn[9,2]);
          end; { StatusBar 09 }

     10 : begin
            {$IFNDEF WINGUI}
              if OldStatLine <> StatusType then
                FillArea(01, mnuScrnLen-1, mnuScrnWidth, mnuScrnLen, #32, MakeAttr(Cyan, Black));
            {$ELSE}
              WinStr[1] := '';
              WinStr[2] := '';
            {$ENDIF}
          end; { StatusBar 10 }
  end; { Case }


  if RemScrnObj <> nil then
    begin
      RemScrnObj^.StatRec.Number := StatusType;
      RemScrnObj^.StatRec.Line1 := Winstr[1];
      RemScrnObj^.StatRec.Line2 := WinStr[2];
    end; { if }

  lineCfg^.Exitinfo^.StatusLine := StatusType;
  LangObj^.ralCodes := SaveCodes;

 {$IFDEF WINGUI}
   Form1.Label2.Caption := WinStr[1];
   Form1.Label3.Caption := WinStr[2];
 {$ELSE}
     {$IFDEF WITH_DEBUG}
       DebugObj.DebugLog(logString, 'Setting windows coordinates to full screen');
     {$ENDIF}

     if NOT GlobalCfg^.RaConfig^.LimitLocal then
       begin
         TextAttr := OldAttr;
         Window(1, 1, mnuScrnWidth, LineCfg^.actScrnLen);
         Crt.GotoXY(OldX, OldY);
         DirectScrnUpdate := SaveDirect;

         UpdateScreenBuffer(true);

       end; { if }
 {$ENDIF}

 {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'Statusdisplay (end)');
 {$ENDIF}

end; { proc. StatusDisplay }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
{$ENDIF}

end. { unit StatusB }
