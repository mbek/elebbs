unit IRCUNIT;
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
{&Delphi+}
(*
**
** IRC chatting component
**
** Copyright (c) 1998, 99 by Maarten Bekers
**
** Created : 13-Nov-1998
** Last update : 13-Nov-1998
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses IrcCli, Global, CfgRec, apTimer;

const
  MotdSecs      = 35;
  MaxChannels   = 500;

type tIrcInterfaceObj = Object
         IrcHostName   : String;
         BanFileList   : String;
         WantChannel   : String;
         RealNameStr   : String;
         CurChannel    : Longint;
         IrcPort       : Longint;
         JoinedCount   : Longint;
         MaxJoined     : Longint;
         SaveRadu      : Boolean;
         DoQuit        : Boolean;
         AllowDccs     : Boolean;
         DccMsgBoard   : Longint;
         WaitSecs      : Longint;

         ServMsgColor  : String;
         WarnColor     : String;
         IrcTxtColor   : String;

         IrcClient     : TIrcClient;
         SaveAbortSet  : CharSet;
         MotdTimer     : EventTimer;

         {-- Constructors and destructors ----------------------------------}
         constructor Init;
         destructor Done;

         {-- Generic routines ----------------------------------------------}
         procedure Join_IRC(MiscData: String);

         {-- Private routines ----------------------------------------------}
         private
           function MatchesUserName(TempStr: String): String;
           function ConvertIrcColors(S: AnsiString): AnsiString;
           procedure NextChannel;
           procedure EnableDcc;
           procedure DisableDcc;
           procedure PostDccFiles;
           procedure StartDccSession(Counter: Longint);
           procedure ListDccSessions;
           procedure ProcessIrcInput;
           procedure GetIrcInput(var InputStr: AnsiString; InputLen, ShowLen: Longint);
           procedure GetInput(var InputStr: AnsiString);
           function IrcName(S: String): String;
           procedure AskUserInfo;
           procedure AskIrcServer;
           procedure AskNickName;
           function SetupTCPIP(Port: Word; HostName: String): Boolean;
           function AllowedToJoin(Cmd: String): Boolean;
           procedure DoCommand(var S: AnsiString);
           procedure CloseDownIRC;
           function ConvertToUserName(S: String): String;
     end; { tIrcInterfaceObj. }

type pIrcInterfaceObj = ^tIrcInterfaceObj;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses {$IFDEF VIRTUALPASCAL}
       VpSysLow,
     {$ENDIF}

     {$IFDEF WITH_DEBUG}
       Debug_U,
     {$ENDIF}

     {$IFDEF WIN32}
       Windows,
     {$ENDIF}
         SockFunc, Sockdef, IrcDef, LongStr, InOut_U,
          ElLog_U, LineEd, RAL, Control, Cases, Input_U,
           GenFile, WordStr, CentrStr, SysUtils, Outblock, DispAns,
            EleMenu, StUtils, Ranges, StrUnit, Colors,
             FileRout, Terminal, MultiLn, Mail,
              StrPath, ObjDec, JDates;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tIrcInterfaceObj.Init;
begin
  IrcHostName := '127.0.0.1';
  BanFileList := '';
  Wantchannel := '';
  RealNameStr := ^F + 'A';
  CurChannel := -1;
  IrcPort := 6667;
  JoinedCount := 0;
  MaxJoined := 3000;
  SaveRadu := TRUE;
  DoQuit := FALSE;
  DccMsgBoard := 0;
  WaitSecs := 10;
  ServMsgColor := '`A13:';
  WarnColor := '`A12:';
  IrcTxtColor := '`A3:';
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tIrcInterfaceObj.Done;
begin
  CloseDownIrc;
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcInterfaceObj.MatchesUserName(TempStr: String): String;
var Counter: Longint;
    NickStr: String;
    ChanIDX: Longint;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'MatchesUserName() TempStr="'+TempStr+'"');
  {$ENDIF}

  MatchesUserName := '';
  TempStr := SupCase(Trim(TempStr));
  ChanIDX := IrcClient.SearchChan(IrcClient.fChannels[CurChannel].fChanName);
  if ChanIDX < 0 then EXIT;

  for Counter := 01 to MaxNicks do
    begin
      NickStr := IrcClient.fChannels[ChanIDX].fNickList^.Get(Counter);

      {$IFDEF WITH_DEBUG}
        DebugObj.DebugLog(logString, 'MatchesUserName() NickStr="'+NickStr+'"');
      {$ENDIF}

      if SUpCase(Copy(NickStr, 1, Length(TempStr))) = TempStr then
        begin
          MatchesUserName := NickStr;
          BREAK;
        end; { if }
     end; { for }
end; { func. MatchesUserName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcInterfaceObj.ConvertIrcColors(S: AnsiString): AnsiString;

function NumExtract(S: AnsiString; Second: Boolean): AnsiString;
var TempStr   : AnsiString;
    Counter   : Longint;
    AlreadyHad: Boolean;
begin
  TempStr := '';
  Counter := 01;
  AlreadyHad := false;

  While S[Counter] in ['0'..'9', ','] do
    begin
      if S[Counter] = ',' then
        begin
          Inc(Counter);

          if NOT AlreadyHad then
           begin
            if Second then TempStr := ''
             else BREAK;
            AlreadyHad := true;
           end
            else BREAK;
        end; { if }

      TempStr := TempStr + S[Counter];
      Inc(Counter);
    end; { while }

  Result := TempStr;
end; { func. NumExtract }

var NewStr   : AnsiString;
    ColorStr : AnsiString;
    Counter  : Longint;
begin
  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'ConvertIrcColors - (begin)');
    DebugObj.DebugLog(logString, 'ConvertIrcColors - S = "'+S+'"');
  {$ENDIF}

  NewStr := '';
  Counter := 01;

  While Counter <= Length(s) do
   begin
     {$IFDEF WITH_DEBUG}
       DebugObj.DebugLog(logString, 'ConvertIrcColors - Counter = ' + FStr(Counter));
     {$ENDIF}

     Case S[Counter] of
       #30 : ; { underline }
       #31 : ; { underline }
        ^B : ; { bold }
        ^O : ; { ??? }
        ^R : ; { inverse }
        ^U : ; { underline }
        ^C : begin
               if Length(s) = Counter then
                 begin
                   NewStr := NewStr + IrcTxtColor
                 end
                   else if NOT (S[Counter + 1] in ['0'..'9']) then
                          NewStr := NewStr + IrcTxtColor
                           else begin
                                  ColorStr := Copy(S, Counter + 1, 5);
                                  if Pos(',', ColorStr) = 00 then
                                    begin
                                      Inc(Counter, Length(NumExtract(ColorStr, false)));
                                      NewStr := NewStr + '`F' + IrcToPcColor(NumExtract(ColorStr, false))+ ':';
                                    end { if }
                                      else begin
                                             Inc(Counter, Length(NumExtract(ColorStr, false)) +
                                                          Length(NumExtract(ColorStr, true)) + 1);


                                             NewStr := NewStr + '`F' + IrcToPcColor(NumExtract(ColorStr, false)) + ':';
                                             NewStr := NewStr + '`B' + IrcToPcColor(NumExtract(ColorStr, true)) + ':';
                                           end; { if }
                                end; { if }
             end; { case }
         else NewStr := NewStr + S[counter];
     end; { case }

     Inc(Counter);
   end; { while }

  Result := NewStr + IrcTxtColor;

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'ConvertIrcColors - ( end )');
  {$ENDIF}
end; { func. ConvertIrcColors }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tIrcInterfaceObj.NextChannel;
var Counter: Longint;
begin
  if CurChannel < 0 then CurChannel := 0;

  for Counter := Succ(CurChannel) to MaxChans do
    if IrcClient.fChannels[Counter].fChanName <> '' then
      begin
        CurChannel := Counter;
        EXIT;
      end; { for }

  for Counter := 01 to (CurChannel + 01) do
    if IrcClient.fChannels[Counter].fChanName <> '' then
      begin
        CurChannel := Counter;
        EXIT;
      end; { for }

  CurChannel := -1;
end; { proc. NextChannel }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tIrcInterfaceObj.EnableDcc;
begin
  if DccMsgBoard <> 0 then
    begin
      AllowDccs := TRUE;
      WriteLn('`A3:', langObj^.ralGet(ralDccEnable));
    end
      else begin
             AllowDccs := FALSE;
             WriteLn('`A12:', LangObj^.ralGet(ralDccNoDcc));
           end; { if }
end; { proc. EnableDcc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tIrcInterfaceObj.DisableDcc;
begin
  AllowDccs := FALSE;

  WriteLn('`A3:', langObj^.ralGet(ralDccDisable));
end; { proc. DisableDcc }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tIrcInterfaceObj.PostDccFiles;
var Counter       : Longint;
    EditorInfo    : pStringArrayObj;
    MessageWritten: Longint;
begin
  for Counter := 01 to MaxDCCs do
    begin
      {-- Check if there are finished DCC sessions --------------------------}
      if IrcClient.fDccs[Counter].Active = dcc_Finished then
       if IrcClient.fDccs[Counter].TotalReceived = IrcClient.fDccs[Counter].FileSize then
         begin
           {-- Actually post the message ------------------------------------}
           New(EditorInfo, Init(MaxMsgLines));    { Initialize message text }

           PostMessage(DccMsgBoard,                               { BoardNr }
                       LineCfg^.Exitinfo^.Userinfo.Name,            { ToWho }
                       IrcClient.fDccs[Counter].Originate,        { FromWho }
                       JustPath(IrcClient.fDccs[Counter].FileName), {Subject}
                       false,                              { Return receipt }
                       true,                           { Message is private }
                       false,                               { Reply receipt }
                       false,                                   { Kill/sent }
                       false,                                   { CrashMail }
                       true,                                   { Attachment }
                       false,                                { Mark as sent }
                       0,                         { Number of message lines }
                       EditorInfo^,              { The actual message lines }
                       MessageWritten,             { Message number written }
                       '',                                   { Dest address }
                       '',                                   { From address }
                       false,                            { User interaction }
                       false,                            { Mark as received }
                       DateStr,                                { Datestring }
                       TimeStr(true, false),                   { Timestring }
                       '',                                   { Reply kludge }
                       '',
                       nilAbsMsgPtr);                       { Cached MsgPtr }

           Dispose(EditorInfo, Done);
           IrcClient.ClearDccSession(Counter);
         end; { if }
    end; { for }

end; { proc. PostDccFiles }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tIrcInterfaceObj.StartDccSession(Counter: Longint);
begin
  if (Counter <= 0) then Counter := -1
   else if (Counter > MaxDCCs) then Counter := -1
    else if (IrcClient.fDccs[Counter].DccClient = nil) then Counter := -1
     else if (IrcClient.fDccs[Counter].Active = dcc_NotActive) then Counter := -1;

  if Counter = -1 then                      { No such DCC session pending }
    begin
      WriteLn(WarnColor, langObj^.ralGet(ralDccNoSuch));
      EXIT;
    end; { if }

  With IrcClient.fDccs[Counter] do
    Case Active of
      dcc_Active   : WriteLn(IrcTxtColor, Format(langObj^.ralGet(ralDccStatus), [JustName(FileName), Originate, CommaStr(Filesize - TotalReceived)]));
      dcc_Finished : WriteLn(IrcTxtColor, Format(langObj^.ralGet(ralDccStatus), [JustName(FileName), Originate, CommaStr(Filesize - TotalReceived)]));
      dcc_Pending  : begin
                       {-- Open up the connection to the sender -----------}
                       WriteLn('`A3:', Format(langObj^.ralGet(ralDccConn), [IpAddress, JustName(FileName)]));

                       if DccClient.ConnectToServer(IpAddress,
                                                    PortNr,
                                                    ErrorStr) then
                         begin
                           WriteLn('`A14:', Format(langObj^.ralGet(ralDccDling), [IpAddress, JustName(FileName)]));
                           Active := dcc_Active;
                         end
                           else begin
                                   WriteLn('`A12:', Format(langObj^.ralGet(ralDccNoConn), [IpAddress, JustName(FileName)]));
                                   IrcClient.ClearDccSession(Counter);
                                 end; { else }
                     end; { dcc_Pending }
    end; { case }
end; { proc. StartDccSession }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tIrcInterfaceObj.ListDccSessions;
var Counter: Longint;
begin
  {-- Show the header -----------------------------------------------------}
  WriteLn('`A10:', langObj^.ralGet(ralDccHdr));
  WriteLn('`A02:', Dup('-', NoColorLength(langObj^.ralGet(ralDccHdr))));

  {-- Show all current active DCC sessions --------------------------------}
  for Counter := 01 to MaxDccs do
    With IrcClient.fDccs[Counter] do
      if Active <> dcc_NotActive then
        begin
          Write('`A3:', Originate, '`A7:');

          Case Active of
            dcc_Finished,
            dcc_WaitPerm,
            dcc_Pending  : Write('`X11:', langObj^.ralGet(ralDccWait));
            dcc_Active   : Write('`X11:', langObj^.ralGet(ralDccGetting));
          end; { case }

          if FileSize > 0 then
            Write('`X29:', Round(((TotalReceived) / FileSize) * 100.00):2)
              else Write('`X29:00');

          WriteLn('`X35:', Justname(FileName));
        end; { with }

  {-- Show the closing footer ---------------------------------------------}
  WriteLn('`A02:', Dup('-', NoColorLength(langObj^.ralGet(ralDccHdr))));
  WriteLn;
end; { proc. ListDccSessions }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tIrcInterfaceObj.ProcessIrcInput;
var Counter: Longint;
    TempStr: AnsiString;
begin
  {-- If DCC sessions are disabled, cut off directly --------------------------}
  if NOT AllowDccs then
    for Counter := 01 to MaxDccs do
      if IrcClient.fDccs[Counter].Active <> dcc_Notactive then
        begin
          IrcClient.Notice(IrcClient.fDccs[Counter].Originate, 'DCC sessions are currently disabled for this client');
          IrcClient.ClearDccSession(Counter);
        end; { if }

  {-- Handle all DCC requests -------------------------------------------------}
  TempStr := IrcClient.HandleDCCs;
  if TempStr <> '' then
    begin
      WriteLn(ConvertIrcColors(TempStr));
      OutBlockObj^.DumpBlock;
      InputObj^.UpdateScrn;
    end; { if }

  {-- Post DCC-files as attachments -------------------------------------------}
  PostDccFiles;

  {-- Check to make sure we shouldnt join a channel ---------------------------}
  if (IrcClient.MotdEnd) OR (TimerExpired(MotdTimer)) then
   if WantChannel <> '' then
    if JoinedCount < MaxJoined then
     if NOT IrcClient.DataAvailable then
       begin
         Inc(JoinedCount);
         if IrcClient.Join(WantChannel, '') then
           begin
             JoinedCount := MaxJoined;
             NextChannel;
           end; { if }
       end; { if }

  {-- Receive the info from the remote ----------------------------------------}
  IrcClient.RecvStrLn(TempStr, false);
  OutputObj^.ResetLines(01);

  if TempStr <> '' then
    begin
       Writeln(ConvertIrcColors(IrcClient.MakeUpMessage(TempStr)));
       OutBlockObj^.DumpBlock;
       InputObj^.UpdateScrn;
     end
      else begin
             IrcClient.DoSleep(50);
           end; { if }
end; { proc. ProcessIrcInput }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tIrcInterfaceObj.GetIrcInput(var InputStr: AnsiString; InputLen, ShowLen: Longint);
var ShowSpace   : Boolean;
    TempCH      : Char;
    Startedit   : Longint;
    CurPos      : Longint;
    OrigStartPos: Longint;
    StartPos    : Longint;
    InsStr      : AnsiString;
    TempStr     : AnsiString;
    LastStr     : AnsiString;
    WaitTimer   : EventTimer;
    DidPress    : Boolean;


procedure ClearInputRow;
begin
  Write('`X', StartPos + 1,':');
  Write(OutputObj^.AvatarDupl(#32, ShowLen));
  Write(OutputObj^.AvatarDupl(#08, ShowLen));
end; { proc. ClearInputRow }


procedure ClearThisLine;
begin
  With GlobalCfg^.RaConfig^ do
    Write('`F', NormFore, ':`B', NormBack, ':');
  Write('`X', StartPos,':`E:');
end; { proc. ClearThisline }


procedure RestoreThisLine;
begin
  if (GlobalCfg^.RaConfig^.HiLitePromptFore <> 0) OR (GlobalCfg^.RaConfig^.HiLitePromptBack<>00) then
    begin
      With GlobalCfg^.RaConfig^ do
        Write('`F', HiLitePromptFore, ':`B', HiLitePromptBack, ':');
    end; { if }

  Write('`X', StartPos + 1,':');

  Write(OutputObj^.AvatarDupl(#32, ShowLen), OutputObj^.AvatarDupl(#08, ShowLen));
  Write(Copy(InputStr, StartEdit + 1, ShowLen));
end; { proc. RestoreThisLine }

procedure UpdateLine;
begin
  Write('`X', CurPos + StartPos, ':', Copy(InputStr, StartEdit + CurPos, Min(ShowLen - CurPos, ShowLen)), #32);
end; { proc. UpdateLine }

procedure ScrollToRight;
begin
  Inc(StartEdit, CurPos - 5);
  CurPos := 05;

  ClearInputRow;
  Write('`X', Succ(StartPos), ':', Copy(InputStr, StartEdit + 1, ShowLen), #32);
  UpdateLine;
end; { proc. ScrollToRight }

procedure ScrollToLeft;
begin
  Dec(StartEdit, ShowLen - 5);
  CurPos := ShowLen - 5;

  if StartEdit < 0 then StartEdit := 0;
  if CurPos < 0 then CurPos := 01;

  ClearInputRow;
  Write('`X', Succ(StartPos), ':', Copy(InputStr, StartEdit + 1, ShowLen), #32);
end; { proc. ScrollToLeft }

procedure ShowChannel;
begin
  if CurChannel >= 0 then
   if IrcClient.fChannels[CurChannel].fChanName <> '' then
     begin
        Write(Format(IrcClient.ChanPrefix, [IrcClient.fChannels[CurChannel].fChanName]));
     end; { if }
end; { proc. ShowChannel }

begin
  if CurChannel >= 0 then
   if IrcClient.fChannels[CurChannel].fChanName = '' then
     NextChannel;

  Write('`X1:');
  OrigStartPos := Pred(OutputObj^.WhereX);
  ShowChannel;
  StartPos := Pred(OutputObj^.WhereX);
  ShowLen := (78 - StartPos);

  if (GlobalCfg^.RaConfig^.HiLitePromptFore <> 0) OR (GlobalCfg^.RaConfig^.HiLitePromptBack<>00) then
   begin
     With GlobalCfg^.RaConfig^ do
       Write('`F', HiLitePromptFore, ':`B', HiLitePromptBack, ':');
   end; { set input color }

  ShowSpace := false;
  if (GlobalCfg^.RaConfig^.HiLitePromptBack <> 0) then ShowSpace := true;
  if (NOT LineCfg^.AnsiOn) AND (NOT LineCfg^.AvatarOn) then ShowSpace := false;

  {---------------------- Show the input backgr. color ---------------------}
  if ShowSpace then
    ClearInputRow;


  {-------------------------- Start the input loop -------------------------}
  NewTimer(WaitTimer, Secs2Tics(WaitSecs));
  CurPos := Length(InputStr) + 1;
  StartEdit := 00;
  if CurPos > 0 then
    Write('`X', CurPos + StartPos, ':');             { Set column position }

  REPEAT
    {------- Correct the cursor position ------ }
    if CurPos > Succ(Length(InputStr)) then CurPos := Succ(Length(InputStr));
    if CurPos > ShowLen then ScrollToRight;
    if CurPos < 01 then
      begin
        if StartEdit > 0 then ScrollToLeft
          else CurPos := 01;
      end; { if }

    TempCH := #01;
    Write('`X', CurPos + StartPos, ':');
    InputObj^.UpdateScrn;
    OutBlockObj^.DumpBlock;

    REPEAT
      if TimerExpired(WaitTimer) then
        begin
          StartPos := OrigStartPos;
          ClearThisLine;

          REPEAT
            ProcessIRCinput;
          UNTIL (InputObj^.KeyPressed) OR (NOT ContrObj^.CheckAll) OR (NOT IrcClient.ConnectionAlive);

          ShowChannel;
          StartPos := Pred(OutputObj^.WhereX);
          ShowLen := (78 - StartPos);
          RestoreThisLine;
        end; { if }

      DidPress := InputObj^.KeyPressed;

      if DidPress then
        begin
          TempCH := InputObj^.ReadKey;
          NewTimer(WaitTimer, Secs2Tics(WaitSecs));
        end
          else IrcClient.DoSleep(1);
    UNTIL (DidPress) OR (NOT ContrObj^.CheckAll) OR (NOT IrcClient.ConnectionAlive);


    Case TempCH of
      #08         : if Length(InputStr) > 0 then
                     begin
                       Delete(InputStr, StartEdit + Pred(CurPos), 1);
                       Dec(CurPos);

                       if CurPos = Length(InputStr) then
                         Write(#08, #32, #08)         { Delete char on scrn }
                          else UpdateLine;
                     end; { backspace }
      #09         : begin
                      StartPos := OrigStartPos;
                      ClearThisLine;
                      ShowChannel;
                      StartPos := Pred(OutputObj^.WhereX);
                      RestoreThisline;

                      LastStr := LastWord(Copy(InputStr, 1, Pred(CurPos) + StartEdit), defExtractWord, false);
                      TempStr := MatchesUserName(LastStr);

                      {$IFDEF WITH_DEBUG}
                        DebugObj.DebugLog(logString, 'Tried to match "'+LastStr+'"');
                      {$ENDIF}

                      if (TempStr <> '') AND (LastStr <> '') then
                        begin
                          Delete(InputStr, (CurPos + StartEdit) - Length(LastStr), Length(LastStr));
                          Insert(TempStr, InputStr, ((CurPos + StartEdit) - Length(LastStr)));

                          CurPos := (CurPos - Length(LastStr)) + Length(TempStr);
                          RestoreThisLine;
                        end; { if }
                    end; { if }
      #24         : begin
                      StartPos := OrigStartPos;
                      ClearThisLine;
                      NextChannel;
                      ShowChannel;
                      StartPos := Pred(OutputObj^.WhereX);
                      ShowLen := (78 - StartPos);
                      RestoreThisLine;
                    end; { ctrl-x -> next channel }
      #25         : begin
                     if Length(InputStr) > 0 then
                       begin
                         InputStr := '';

                         StartPos := OrigStartPos;
                         ClearThisLine;
                         ShowChannel;
                         StartPos := Pred(OutputObj^.WhereX);
                         ShowLen := (78 - StartPos);
                         RestoreThisLine;
                       end; { if }
                    end; { ctrl-y }
      #27         : begin
                      TempCH := GetArrowKeys;

                      Case TempCH of
                        { Up }     'A' : ;
                        { Down }   'B' : ;
                        { Right }  'C' : Inc(CurPos);
                        { Left }   'D' : Dec(CurPos);
                        { Home }   'H' : begin
                                           CurPos := 00;
                                           StartEdit := 0;
                                           ScrollToLeft;

                                           CurPos := 00;
                                         end; { home }
                        { End }    'K' : CurPos := Length(InputStr) + 1;
                      end; { case }

                    end; { escape }
      #32..#255   : if Length(InputStr) < InputLen then
                      begin
                        InsStr := '' + TempCH;
                        AnsiFullInsert(InsStr, InputStr, StartEdit + CurPos);

                        if CurPos <> Length(InputStr) then
                          UpdateLine
                            else Write(TempCH);

                        Inc(CurPos);
                      end; { if }
    end; { case }

  UNTIL (TempCH IN [#13]) OR (NOT ContrObj^.CheckAll);
  {-------------------------- End of the input loop ------------------------}

  Write(IrcTxtColor);
end; { proc. GetIrcInput }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tIrcInterfaceObj.GetInput(var InputStr: AnsiString);
begin
  GetIrcInput(InputStr, 250, 78);
end; { proc. GetInput }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcInterfaceObj.IrcName(S: String): String;
begin
  S := Copy(S, 1, 20);
  Result := S;
end; { func. IrcName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tIrcInterfaceObj.AskUserInfo;
begin
  termObj^.RaCodeStr(RealNameStr);

  IrcClient.UseNickName := IrcName(LineCfg^.Exitinfo^.Userinfo.Handle);
  IrcClient.UseRealName := RealNameStr;
  IrcClient.UsePassword := '';
end; { proc. AskUserInfo }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tIrcInterfaceObj.AskIrcServer;
var TempStr: String;
begin
  TempStr := '';

  Write('`A3:', langObj^.ralGet(ralAskIrc));
  GetString(TempStr, 78 - Length(langObj^.ralGet(ralAskIrc)), [#32..#254], false, false, false, false);
  WriteLn;

  IrcHostName := TempStr;
end; { proc. AskIrcServer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tIrcInterfaceObj.AskNickName;
var TempStr: String;
begin
  TempStr := '';

  Write('`A3:', langObj^.ralGet(ralAskIrcNick));
  GetString(TempStr, 78 - Length(langObj^.ralGet(ralAskIrcNick)), [#32..#254], false, false, false, false);
  IrcClient.UseNickName := IrcName(TempStr);
  WriteLn;
end; { proc. AskNickName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcInterfaceObj.SetupTCPIP(Port: Word; HostName: String): Boolean;

procedure FatalError(S: String);
begin
  SetupTCPIP := false;
  RaLog('!', 'IRC-CLIENT: '+S);
end; { proc. FatalError }

var ErrorStr: String;
begin
  Result := IrcClient.ConnectToServer(HostName, Port, ErrorStr);

  if NOT Result then
    FatalError(ErrorStr);
end; { proc. SetupTCPIP }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcInterfaceObj.AllowedToJoin(Cmd: String): Boolean;
var Channel: String;
    Ctl_F  : Text;
    TempStr: String;
begin
  Result := true;
  if BanFileList = '' then EXIT;

  Channel := ExtractWord(Cmd, 2, defExtractWord, false, false);

  Assign(Ctl_F, OpenRaFile(BanFileList));
  {$i-} System.Reset(Ctl_F); {$I+}
  If IOResult>00 then exit;

  While NOT EOF(Ctl_F) do
    begin
      {$i-} ReadLn(Ctl_F, TempStr); {$I+}
      If IOResult>00 then BREAK;

      if TempStr[1] <> ';' then
        begin
          if IsWildCard(Copy(TempStr, 2, 255), Channel) then
            begin
             Case TempStr[1] of
               '-' : Result := FALSE;
               '+' : Result := TRUE;
             end; { case }

             BREAK;
           end; { while }
        end; { if }
    end; { while }

  {$i-} Close(Ctl_F); {$I+}
  If IOResult>00 then ;
end; { func. AllowedToJoin }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tIrcInterfaceObj.DoCommand(var S: AnsiString);
var TempStr: AnsiString;
    DnsStr : String;
    Counter: Longint;
begin
  Delete(S, 1, 1);                                          { Delete the / }

  if (SUpCase(ExtractWord(S, 1, defExtractWord, false, false)) = 'JOIN') OR
      (SUpCase(ExtractWord(S, 1, defExtractWord, false, false)) = 'J') then
    begin
      if NOT AllowedToJoin(s) then
        begin
          WriteLn(langObj^.ralGet(ralChanDeny));
          S := '';
        end
         else
           begin
             if IrcClient.Join(ExtractWord(S, 2, defExtractWord, false, false),
                               ExtractWord(S, 3, defExtractWord, false, false)) then ;
             CurChannel := IrcClient.SearchChan(ExtractWord(S, 2, defExtractWord, false, false));
             if CurChannel < 0 then CurChannel := -1;
             S := '';
           end; { else }
    end; { if }

  if SUpCase(ExtractWord(S, 1, defExtractWord, false, false)) = 'MSG' then
    begin
      TempStr := TrimLeft(Copy(S, Length('/MSG ') + Length(ExtractWord(S, 2, defExtractWord, false, false)), 255));
      IrcClient.PrivMsg(ExtractWord(S, 2, defExtractWord, false, false), TempStr);
      WriteLn(Format('%s-> *%s* %s', [IrcTxtColor, ExtractWord(S, 2, defExtractWord, false, false), TempStr]));
      S := '';
    end; { if }

  if SUpCase(ExtractWord(S, 1, defExtractWord, false, false)) = 'CTCP' then
    begin
      TempStr := ExtractWord(S, 2, defExtractWord, false, false);   { ToWho }
      DnsStr := ExtractWord(S, 3, defExtractWord, false, false);    { Query }
      DnsStr := SUpcase(DnsStr);

      IrcClient.CTCPSend(TempStr, DnsStr);

      WriteLn(Format('`A12:-> [%s] %s', [DnsStr, TempStr]));
      S := '';
    end; { if }

  if SUpCase(ExtractWord(S, 1, defExtractWord, false, false)) = 'ME' then
    begin
      if CurChannel < 0 then
        begin
          WriteLn(WarnColor, Langobj^.ralGet(ralNotOnChan));
          S := '';
        end
          else begin
                 TempStr := Copy(S, Length('/ME '), 255);
                 IrcClient.PrivMsg(IrcClient.fChannels[CurChannel].fChanName, ^A'ACTION ' + TempStr + ^A);

                 WriteLn(Format(IrcClient.ChanPrefix + '%s* %s %s%s', [
                              IrcClient.fChannels[CurChannel].fChanName,
                              ServMsgColor, Trim(IrcClient.UseNickName), TempStr, IrcTxtColor]));
                 S := '';
               end; { if }
    end; { if }

  if SUpCase(ExtractWord(S, 1, defExtractWord, false, false)) = 'DCC' then
    begin
      TempStr := Supcase(ExtractWord(S, 2, defExtractWord, false, false));

      if TempStr = 'LIST' then ListDccSessions
        else if TempStr = 'GET' then StartDccSession(FVal(ExtractWord(S, 3, defExtractWord, false, false)))
         else if TempStr = 'ENABLE' then EnableDcc
           else if TempStr = 'DISABLE' then DisableDcc
             else WriteLn('`A12:', langObj^.ralGet(ralDccUnk));

      S := '';
    end; { if }

  if SUpCase(ExtractWord(S, 1, defExtractWord, false, false)) = 'DNS' then
    begin
      TempStr := Copy(S, Length('/DNS '), 255);

      WriteLn(Format('%s*** Looking up %s', [ServMsgColor, TempStr]));
      OutBlockObj^.DumpBlock;

      if IrcClient.ResolveAddr(TempStr, DnsStr) then
        WriteLn(Format('*** Resolved %s to %s', [ServMsgColor, TempStr, DnsStr]))
         else WriteLn(Format('*** Unable to resolve %s.', [ServMsgColor, TempStr]));

      S := '';
    end; { if }

  if (SUpCase(ExtractWord(S, 1, defExtractWord, false, false)) = 'PART') OR
      (SUpCase(ExtractWord(S, 1, defExtractWord, false, false)) = 'P') then
    begin
      if CurChannel < 0 then
        begin
          WriteLn(WarnColor, Langobj^.ralGet(ralNotOnChan));
          s := '';
        end
          else begin
                 if WordCount(s, defExtractWord, false) = 1 then
                    S := S + #32 + IrcClient.fChannels[CurChannel].fChanName;

                 IrcClient.Part(ExtractWord(S, 2, defExtractWord, false, false));
                 S := '';
                 NextChannel;
               end; { if }
    end; { if }


  if SUpCase(ExtractWord(S, 1, defExtractWord, false, false)) = 'NICK' then
    begin
      TempStr := Copy(S, Length('/NICK '), 255);
      IrcClient.UseNickName := IrcName(TempStr);

      S := 'NICK :'+IrcName(TempStr);
    end; { if }


  if SUpCase(ExtractWord(S, 1, defExtractWord, false, false)) = 'HELP' then
    begin
      if DisplayHotFile('IRCHELP', []) = #00 then S := '';
    end; { if }

  if SUpCase(ExtractWord(S, 1, defExtractWord, false, false)) = 'NAMES' then
    begin

    end; { if }

  if SUpCase(ExtractWord(S, 1, defExtractWord, false, false)) = 'QUIT' then
    begin
      {-- Make sure DCC requests arent pending ----------------------------}
      for Counter := 01 to MaxDccs do
        if IrcClient.fDccs[Counter].Active = dcc_Active then
          begin
            Write('`A12:');
            if NOT InputObj^.RalStrYesNoAsk(ralDccWarn) then
              begin
                EXIT;
              end; { if }

            BREAK;
          end; { if }


      {-- Actually quit ---------------------------------------------------}
      TempStr := Copy(S, Length('/QUIT '), 255);
      if Trim(TempStr) = '' then TempStr := 'Leaving';
      IrcClient.Quit(TempStr);
      IrcClient.DoSleep(100);
      DoQuit := true;
      S := '';
    end; { if }

  if Length(s) >= 1 then
   if S[1] = '/' then Delete(S, 1, 1);    { // commands are executed directly }

  if S <> '' then
   begin
     IrcClient.SendStrLn(s);
   end; { if }
end; { proc. DoCommand }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tIrcInterfaceObj.CloseDownIRC;
begin
  if IrcClient <> nil then
    try
      IrcClient.Quit('Client exited');
      IrcClient.Disconnect;
      IrcClient.Free;
    except
    end;

  IrcClient := nil;
  LineCfg^.RaduCodes := SaveRadu;
  LineCfg^.DispAbortSet := SaveAbortSet;
end; { proc. CloseDownIRC }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tIrcInterfaceObj.ConvertToUserName(S: String): String;
var Counter: Byte;
begin
  Result := '';

  for counter := 01 to Length(s) do
    Case S[Counter] of
      #32 : Result := Result + '_';
       else Result := Result + s[counter];
    end; { case }
end; { func. ConvertToUserName }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tIrcInterfaceObj.Join_IRC(MiscData: String);
var DidLogon : Boolean;
    UseronStr: String;
    LastStr  : AnsiString;
begin
  {------------------- Start initialisation of IRC parameters -----------------}
  SaveRadu := LineCfg^.RaduCodes;
  IrcClient := nil;
  DoQuit := false;
  UseronStr := '';
  UserOnStr := Under2Norm(GetValue('*U', MiscData, True));
  DccMsgBoard := FVal(GetValue('/DCCBOARD=', MiscData, true));
  IrcHostName := GetValue('/SERVER=', MiscData, true);
  WantChannel := GetValue('/CHANNEL=', MiscData, true);
  if WantChannel = '' then
    WantChannel := GetValue('/JOIN=', MiscData, true);
  BanFileList := GetValue('/CHANLIST=', MiscData, true);
  if Pos('/REALNAME=', Supcase(MiscData)) <> 0 then
    RealNameStr := Under2Norm(GetValue('/REALNAME=', MiscData, true));
  SaveAbortSet := LineCfg^.DispAbortSet;
  LineCfg^.DispAbortSet := [];

  WriteLn;
  WriteLn;

  IrcClient := tIrcClient.Create;

  AskUserInfo;
  IrcClient.ChanPrefix := langObj^.ralGet(ralChanPref);
  IrcClient.UserPrefix := langObj^.ralGet(ralUserPref);
  if IrcHostName = '' then
    AskIrcServer;

  if Pos(':', IrcHostName)>0 then
    begin
      Ircport := FVal(Copy(IrcHostName, Pos(':', IrcHostName) + 1, 255));
      IrcHostName := Copy(IrcHostName, 1, Pos(':', IrcHostName) - 1);
    end; { if }

  if IrcHostName = '' then
    begin
      CloseDownIRC;
      EXIT;
    end; { if }

  WriteLn(Format(LangObj^.ralGet(ralConnIRC), [IrcHostName]));
  OutBlockObj^.DumpBlock;
  InputObj^.UpdateScrn;

  {------------ Try to connect to the server -------------------------------}
  if NOT SetupTcpIp(IrcPort, IrchostName) then
    begin
      Write(WarnColor, Format(LangObj^.ralGet(ralNoConnect), [IrcHostName]));
      InputObj^.PressEnter(false, true);
      CloseDownIRC;
      EXIT;
    end; { if }

  {------------ Try to login into the server -------------------------------}
  IrcClient.UseUsername := ConvertToUserName(LineCfg^.Exitinfo^.Userinfo.Handle);
  DidLogon := true;

  if NOT IrcClient.Logon then
    begin
      DidLogon := false;

      if IrcClient.LastError = ERR_NICKNAMEINUSE then
        begin
          While (IrcClient.UseNickName <> '') AND (NOT DidLogon) do
            begin
              AskNickName;

              if IrcClient.UseNickName = '' then
                begin
                  DidLogon := false;
                end
                  else begin
                         if IrcClient.Logon then DidLogon := true;
                       end; { if }
            end; { if }
        end; { if }

      if NOT DidLogon then
        begin
          Write(WarnColor, Format(LangObj^.ralGet(ralIrcNoAcc), [IrcHostName]));
          InputObj^.PressEnter(false, true);
          CloseDownIRC;
          EXIT;
        end; { if }
    end; { if }

  OutBlockObj^.DumpBlock;
  InputObj^.UpdateScrn;
  JoinedCount := 0;
  NewTimer(MotdTimer, Secs2Tics(MotdSecs));

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logString, 'LangObj^.ralGet(ralUserPref): ' + Langobj^.RalGet(ralUserPref));
  {$ENDIF}

  IrcTxtColor := '`A' + FStr(GetLastColor(LangObj^.ralGet(ralUserpref))) + ':';
  MultiLnObj^.WriteUseron('', uonIRC);


  {-- Now write the useron description ----------------------------------------}
  {-- as specified by the sysop -----------------------------------------------}
  if MultiLnobj^.UserOn <> nil then
    begin
      If UserOnStr <> '' then
        MultiLnObj^.Useron^.StatDesc := UserOnStr
          else MultiLnObj^.Useron^.StatDesc := '';
      MultiLnObj^.WriteUserOn(MultiLnObj^.Useron^.StatDesc, uonBrowsing);
    end; { if }

  repeat
    repeat
      ProcessIrcInput;
      if NOT IrcClient.ConnectionAlive then
         BREAK;
    until (InputObj^.Keypressed) OR (NOT ContrObj^.CheckAll);

    if NOT IrcClient.ConnectionAlive then
      begin
        Write(WarnColor, '`X1:`E:', Format(LangObj^.ralGet(ralIrcDiscon), [IrcHostName]));
        InputObj^.PressEnter(false, true);
        BREAK;
      end; { if }

    OutBlockObj^.DumpBlock;
    InputObj^.UpdateScrn;
    if InputObj^.KeyPressed then ; { Update the screen }
    LastStr := '';
    GetInput(LastStr);
    LastStr := Trim(LastStr);
    Write(IrcTxtColor, '`X1:`E:');

    if LastStr <> '' then
     if LastStr[1] = '/' then
      DoCommand(LastStr)
       else begin
              if CurChannel >= 0 then
                begin
                  if IrcClient.fChannels[CurChannel].fChanName<> '' then
                    begin
                      if LastStr <> '' then
                       WriteLn(Format(IrcClient.ChanPrefix + IrcClient.UserPrefix + '%s', [IrcClient.fChannels[CurChannel].fChanName,
                                                           Trim(IrcClient.UseNickName), LastStr]));
                        IrcClient.PrivMsg(IrcClient.fChannels[CurChannel].fChanName, LastStr)
                    end
                     else WriteLn(WarnColor, Langobj^.ralGet(ralNotOnChan));
                end
                  else WriteLn(WarnColor, Langobj^.ralGet(ralNotOnChan));
            end; { if }
    OutBlockObj^.DumpBlock;
    InputObj^.UpdateScrn;
  until (ExtractWord(SUpCase(LastStr), 1, defExtractWord, false, false) = '/QUIT') OR
          (NOT ContrObj^.CheckAll) OR (DoQuit);

  MultiLnObj^.WriteUseron('', uonBrowsing);
  CloseDownIRC;
end; { proc. Join_IRC }


end. { unit IRCUNIT }
