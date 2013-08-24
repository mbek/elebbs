unit LineEd;
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
** EditLine Routines for EleBBS
**
** Copyright (c) 1996,1997 by Maarten Bekers
**
** Created : 15-Nov-1997
** Last update : 15-Nov-1997
**
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

uses StrEdit, Global, CfgRec;

procedure LineEdit(var   InputStr    : String;
                   const InputLen    : Longint;
                         Legals      : CharSet;
                   const Capitalize,
                         PassWord    : Boolean;
                   const FillChar    : Char;
                   const WordWrap,
                         AddShowSpace,
                         IsIEMSI     : Boolean);
procedure MustEdit(var   InputStr   : String;
                   const ShowMsg    : String;
                   const InputLen   : Byte;
                   const Capitalize,
                         AddCR      : Boolean;
                   const MsgFore,
                         MsgBack    : Byte;
                   const Legals     : CharSet;
                   const MinimumLen : Byte);
procedure RangeEdit(var   InputInt : Longint;
                    const ShowMsg  : String;
                    const StartNr,
                          EndNr,
                          DefaultNr: Longint);
procedure GetString(var   InputStr   : String;
                    const InputLen   : Byte;
                    const Legals     : CharSet;
                    const Capitalize,
                          Password,
                          WordWrap,
                          IsIemsi    : Boolean);
function EditBox(var EditS: String;
                 X1, Y1, X2, Y2: Byte;
                 Title     : String;
                 MaxLen    : Byte;
                 IsString,
                 IsPassword: Boolean): Char;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses
     ScrnU,

     {$IFDEF VirtualPascal}
       Crt,
     {$ENDIF}

     {$IFDEF MSDOS}
       Crt,
     {$ENDIF}

     {$IFDEF FPC}
       Crt,
       {$IFDEF ELEUNIX}
         {$IFDEF VER1_0}
            Linux,
         {$ELSE}
            Unix,
         {$ENDIF}
       {$ENDIF}
     {$ENDIF}

      InOut_U, Debug_U, Support, Ral, FastScrn, Control, Colors, Multi,
       Cases, LongStr, CentrStr, Input_U, Ranges, RemScrn, Outblock,
        EleMenu, Stutils, ObjDec;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure MustEdit(var   InputStr   : String;
                   const ShowMsg    : String;
                   const InputLen   : Byte;
                   const Capitalize,
                         AddCR      : Boolean;
                   const MsgFore,
                         MsgBack    : Byte;
                   const Legals     : CharSet;
                   const MinimumLen : Byte);
begin
  repeat
    InputStr := '';
    Write('`F', MsgFore, ':`B', MsgBack, ':');
    Write(ShowMsg);

    GetString(InputStr,
              InputLen,
              Legals,
              Capitalize,
              false, false, false);

    Writeln;
    if AddCR then WriteLn;

    if Length(InputStr) < Minimumlen then InputStr := '';

  until (InputStr <> '') OR (Progterminated);
end; { proc. MustEdit }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure RangeEdit(var   InputInt : Longint;
                    const ShowMsg  : String;
                    const StartNr,
                          EndNr,
                          DefaultNr: Longint);
var MaxLen  : Byte;
    InputStr: String;
begin
  repeat
    InputStr := '';
    Write('`A2:', ShowMsg);

    MaxLen := Length(FStr(EndNr));
    if Length(FStr(StartNr)) > MaxLen then
      MaxLen := Length(Fstr(StartNr));


    GetString(InputStr, MaxLen, ['0'..'9'], false, false, false, false);
    WriteLn;

    if InputStr <> '' then
     if NOT InRange(FVal(InputStr), StartNr, EndNr) then
       begin
         WriteLn;
         WriteLn(LangObj^.ralGet(ralNrInRange), #32, StartNr, '-', EndNr);
       end; { if NOT InRange }

  UNTIL (Inrange(FVal(InputStr), StartNr, EndNr)) OR
         (InputStr = '') OR (ProgTerminated);

  if InputStr = '' then InputStr := FStr(DefaultNr);
  InputInt := FVal(InputStr);
end; { proc. RangeEdit }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

procedure GetString(var   InputStr   : String;
                    const InputLen   : Byte;
                    const Legals     : CharSet;
                    const Capitalize,
                          Password,
                          WordWrap,
                          IsIemsi    : Boolean);
var ShowSpace: Boolean;
begin
  if (GlobalCfg^.RaConfig^.HiLitePromptFore <> 0) OR (GlobalCfg^.RaConfig^.HiLitePromptBack<>00) then
   begin
     With GlobalCfg^.RaConfig^ do
       Write('`F', HiLitePromptFore, ':`B', HiLitePromptBack, ':');
   end; { set input color }

  ShowSpace := false;
  if (GlobalCfg^.RaConfig^.HiLitePromptBack <> 0) then ShowSpace := true;
  if (NOT LineCfg^.AnsiOn) AND (NOT LineCfg^.AvatarOn) then ShowSpace := false;

  LineEdit(InputStr,
           InputLen,
           Legals,
           Capitalize,
           Password,
           #32,
           WordWrap,
           ShowSpace,
           IsIEMSI);


  With GlobalCfg^.RaConfig^ do
    Write('`F', NormFore, ':`B', NormBack, ':');
end; { proc. GetString }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-*)

function EditBox(var EditS: String;
                 X1, Y1, X2, Y2: Byte;
                 Title     : String;
                 MaxLen    : Byte;
                 IsString,
                 IsPassword : Boolean): Char;
var EditPos: Byte;
begin
  EditBox := #00;

  BoxWindow(X1, Y1, X2, Y2,
            GlobalCfg^.RaConfig^.BorderFore, GlobalCfg^.RaConfig^.BorderBack,
            Title);

  EditPos := X1 + (((X2 - X1) - (MaxLen)) DIV 2);

    Case IsString of
      TRUE  : StrEdit.GetString(EditPos, Y1 + 2,
                        MakeAttr(GlobalCfg^.RaConfig^.BorderFore,
                                 GlobalCfg^.RaConfig^.BorderBack), EditS, [#32..#255], [#13, #27], MaxLen, MaxLen, false,
                                 false, false,
                                 IsPassword, 0);
      FALSE : StrEdit.GetString(EditPos, Y1 + 2,
                        MakeAttr(GlobalCfg^.RaConfig^.BorderFore,
                                 GlobalCfg^.RaConfig^.BorderBack), EditS, ['0'..'9'], [#13, #27], MaxLen, MaxLen, false,
                                 false, false,
                                 IsPassword, 0);
    end; { case }

    EditBox := rdLastKey;
    CursorOn;
end; { func. EditBox }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure LineEdit(var   InputStr    : String;
                   const InputLen    : Longint;
                         Legals      : CharSet;
                   const Capitalize,
                         PassWord    : Boolean;
                   const FillChar    : Char;
                   const WordWrap,
                         AddShowSpace,
                         IsIEMSI     : Boolean);
var TempCH      : Char;
    SaveCH      : Char;
    Startedit   : Longint;
    CurPos      : Longint;
    OrigStartPos: Longint;
    StartPos    : Longint;
    ShowLen     : Byte;
    DoUpdateScrn: Boolean;
    DoQuit      : Boolean;

function DoPwdShow(TempStr: String): String;
var Counter: Longint;
    OutStr : String;
begin
  if Password then
    begin
      OutStr :=  '';

      for Counter := 01 to Length(TempStr) do
        OutStr := OutStr + GlobalCfg^.RaConfig^.EchoChar;
    end
      else OutStr := TempStr;

  DoPwdShow := OutStr;
end; { func. DoPwdShow }


procedure ClearInputRow;
begin
  if NOT DoUpdateScrn then EXIT;

  Write('`X', StartPos + 1,':');
  Write(OutputObj^.AvatarDupl(FillChar, ShowLen));
  Write(OutputObj^.AvatarDupl(#08, ShowLen));
end; { proc. ClearInputRow }


procedure ClearThisLine;
begin
  if NOT DoUpdateScrn then EXIT;

  With GlobalCfg^.RaConfig^ do
    Write('`F', NormFore, ':`B', NormBack, ':');
  Write('`X', StartPos,':`E:');
end; { proc. ClearThisline }


procedure RestoreThisLine;
begin
  if NOT DoUpdateScrn then EXIT;

  if (GlobalCfg^.RaConfig^.HiLitePromptFore <> 0) OR (GlobalCfg^.RaConfig^.HiLitePromptBack<>00) then
    begin
      With GlobalCfg^.RaConfig^ do
        Write('`F', HiLitePromptFore, ':`B', HiLitePromptBack, ':');
    end; { if }

  Write('`X', StartPos + 1,':');

  Write(OutputObj^.AvatarDupl(FillChar, ShowLen), OutputObj^.AvatarDupl(#08, ShowLen));
  Write(DoPwdShow(Copy(InputStr, StartEdit + 1, ShowLen)));
end; { proc. RestoreThisLine }

procedure UpdateLine;
begin
  if NOT DoUpdateScrn then EXIT;

  Write('`X', CurPos + StartPos, ':', DoPwdShow(Copy(InputStr, StartEdit + CurPos, Succ(Min(ShowLen - CurPos, ShowLen)))),
        FillChar);
end; { proc. UpdateLine }

procedure ScrollToRight;
begin
  if NOT DoUpdateScrn then EXIT;

  Inc(StartEdit, CurPos - 5);
  CurPos := 05;

  ClearInputRow;
  Write('`X', Succ(StartPos), ':', DoPwdShow(Copy(InputStr, StartEdit + 1, ShowLen)), FillChar);
  UpdateLine;
end; { proc. ScrollToRight }

procedure ScrollToLeft;
begin
  if NOT DoUpdateScrn then EXIT;

  Dec(StartEdit, ShowLen - 5);
  CurPos := ShowLen - 5;

  if StartEdit < 0 then StartEdit := 0;
  if CurPos < 0 then CurPos := 01;

  ClearInputRow;
  Write('`X', Succ(StartPos), ':', DoPwdShow(Copy(InputStr, StartEdit + 1, ShowLen)), FillChar);
end; { proc. ScrollToLeft }

begin
  if isIEMSI then Legals := Legals + ['*', '_'];

  {$IFDEF WITH_DEBUG}
    DebugObj.DebugLog(logInOut, '(LineEdit) - InputStr - "L" in [legals] = ' + Bool2Str('L' in Legals));
  {$ENDIF}

  OrigStartPos := Pred(OutputObj^.WhereX);
  StartPos := Pred(OutputObj^.WhereX);
  ShowLen := Min((78 - StartPos), InputLen);
  DoUpdateScrn := true;
  DoQuit := false;

  if Password then
   if (Length(GlobalCfg^.RaConfig^.EchoChar) = 0) then DoUpdateScrn := false;

  {-------------------------- Start the input loop -------------------------}
  CurPos := Length(InputStr) + 1;
  StartEdit := 00;

  {---------------------- Show the input backgr. color ---------------------}
  if AddShowSpace then
   if DoUpdateScrn then
     begin
       RestoreThisLine;
     end; { if }


  if CurPos > 0 then
    Write('`X', CurPos + StartPos, ':');             { Set column position }

  REPEAT
    {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logInout, '(LineEdit) - WhereX   = ' + FStr(WhereX));
      DebugObj.DebugLog(logInOut, '(LineEdit) - InputStr = "'+InputStr+'"');
      DebugObj.DebugLog(logInOut, '(LineEdit) - CurPos / StartPos = ' + FStr(CurPos) + ' / ' + FStr(StartPos));
      if DoUpdateScrn then
        DebugObj.DebugLog(logInOut, '(LineEdit) - DoUpdateScrn = TRUE');
    {$ENDIF}

    {------- Correct the cursor position ------ }
    if CurPos > Succ(Length(InputStr)) then CurPos := Succ(Length(InputStr));
    if CurPos > Succ(ShowLen) then ScrollToRight;
    if CurPos < 01 then
      begin
        if StartEdit > 0 then ScrollToLeft
          else CurPos := 01;
      end; { if }

    {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logInOut, '(LineEdit) - CursorPos = ' +FStr(CurPos));
    {$ENDIF}

    TempCH := #01;
    if DoUpdateScrn then
      Write('`X', CurPos + StartPos, ':');
    InputObj^.UpdateScrn;
    OutBlockObj^.DumpBlock;

    {------------------------ Get the actual input ----------------------------}
    TempCH := InputObj^.ReadKey;
    if NOT contrObj^.CheckAll then EXIT;

    {$IFDEF WITH_DEBUG}
      DebugObj.DebugLog(logInOut, '(LineEdit) - InputStr - TempCH = "'+TempCH+'"');
      DebugObj.DebugLog(logInOut, '(LineEdit) - InputStr - TempCH in [legals] = ' + Bool2Str(TempCH in Legals));
    {$ENDIF}


    Case TempCH of
      #127, { linux backspace }
      #08         : if Length(InputStr) > 0 then
                     begin
                       Delete(InputStr, StartEdit + Pred(CurPos), 1);
                       Dec(CurPos);

                       if DoUpdateScrn then
                         begin
                           if CurPos = Length(InputStr) then
                             Write(#08, FillChar, #08)         { Delete char on scrn }
                               else UpdateLine;
                         end; { if }
                     end; { backspace }
      #25         : begin
                     if Length(InputStr) > 0 then
                       begin
                         InputStr := '';
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
      else if TempCH in Legals then
             begin
               if Length(InputStr) < InputLen then
                 begin
                   {------- Check if we want this character capitalized ----}
                   if Capitalize then
                     begin
                       SaveCH := TempCH;

                       if (Length(InputStr) = 00) OR (InputStr[Length(InputStr)] in [#32, '-', '.', ',']) then
                         TempCH := UpCase(TempCH)
                           else TempCH := LowCase(TempCH);

                       if InputStr[Length(InputStr)] = 'c' then
                        if (sUpCase(Copy(InputStr, Length(InputStr) - 1, 2)) = 'MC') or
                            (sUpCase(Copy(InputStr, Length(InputStr) - 2, 3)) = 'MAC') then
                              TempCH := SaveCH;
                     end; { if }

                   FullInsert(TempCH, InputStr, StartEdit + CurPos);

                   if DoUpdateScrn then
                     begin
                       {$IFDEF WITH_DEBUG}
                         DebugObj.DebugLog(logString, '(LineEdit) CurPos = ' + FStr(CurPos));
                         DebugObj.DebugLog(logString, '(LineEdit) Length = ' + FStr(Length(InputStr)));
                       {$ENDIF}

                       if CurPos <> Length(InputStr) then
                         UpdateLine
                           else Write(DoPwdShow(TempCH));
                     end; { if }

                   Inc(CurPos);
                 end; { if }
             end; { if }
      end; { case }

    {$IFDEF WITH_DEBUG}
    {$ENDIF}

    if Pos('**EMSI_', sUpCase(InputStr)) > 00 then
      if IsIEMSI then DoQuit := true;

    if WordWrap then
      if Length(InputStr) = InputLen then DoQuit := true;

  UNTIL (TempCH IN [#13]) OR (NOT contrObj^.CheckAll) OR (DoQuit);
  {-------------------------- End of the input loop ------------------------}
end; { proc. LineEdit }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit LINEED }

