unit FSED;
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
** Fullscreen editor routines for EleBBS
**
** Copyright (c) 1999 by Maarten Bekers
**
** Created : 20-Mar-1999
** Last update : 20-Mar-1999
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF ISCGI}
  This unit should not be compiled into EleWEB
{$ENDIF}

uses StrUnit;

const
  MaxEditLines = 400;
  MaxQuoteLines= 200;

type
  EditorMsgInfoRec = record
                       FromWho,
                       ToWho,
                       Subject,
                       AreaName  : String;
                       MsgNumber : Longint;
                       IsPrivate : Boolean;
                     end; { record }

type WinRecRecord = record
                      MinX,
                      MinY    : Byte;
                      MaxX,
                      MaxY    : Byte;
                    end; { record }

     CurCursorRecord = record
                        CurCol,
                        CurRow,
                        PrevCol,
                        PrevRow  : Byte;
                      end; { record }


type tFsedObj = Object
         WinRec      : WinRecRecord;
         CurCursor   : CurCursorRecord;

         TextLines,
         QuoteLines  : PStringArrayObj;

         SaveMsgText : PStringArrayObj;
         SaveWinRec  : WinRecRecord;
         SaveCursor  : CurCursorRecord;
         IsQuoting   : Boolean;
         SaveCurrLine: Longint;
         SaveMajor   : Longint;
         ReadOnly    : Boolean;
         MsgInfo     : EditorMsgInfoRec;

         QuoteColor  : Byte;
         EditColor   : Byte;
         MaxLine     : Longint;

         CurrentLine : Longint;
         InsertMode  : Boolean;

         {-- Constructors and destructors ----------------------------------}
         constructor Init;
         destructor Done;

         {-- Generic routines ----------------------------------------------}
         function FullScreenEditor(var _TextLines,
                                       _QuoteLines : PStringArrayObj;
                                       DoQuotes    : Boolean;
                                   var _MsgInfo    : EditorMsgInfoRec): Longint;

         {-- Private routines ----------------------------------------------}
         private
           function HandleCursors(DoQuotes: Boolean): Boolean;

           procedure ShowMsgHeader;
           procedure ShowMsgFooter;
           procedure ShowTextLine(YPos: Byte; const TempStr: String);
           procedure ShowTextBuffer;
           procedure UpdateCursor(Force: Boolean);
           procedure InsertNewLine(const NewLine: String);
           procedure GotoEndOfLine;
           procedure GotoStartOfLine;
           procedure PageDown;
           procedure PageUp;
           procedure CursorDown;
           procedure CursorUp;
           procedure CursorLeft;
           procedure CursorRight;
           procedure AddChar(const CH: Char);
           procedure DeleteCurrentLine;
           procedure JoinLines(var FirstLine, SecondLine: String);
           procedure BackSpace;
           procedure CenterLine;
           procedure DeleteChar;
           procedure Inserttab;
           procedure DoQuoteLine;
           procedure DoEnter;
           procedure DeleteFirstWord;
           procedure WordLeft;
           procedure WordRight;
           procedure RedrawScreen;
           procedure ShowHelpScreen;
           procedure EditMenu(var InputCH: Char);
           procedure ToggleInsert;
           procedure ShowQuoteWindow;
           procedure CloseQuoteWindow;
     end; { tFsedObj }

type pFsedObj = ^tFsedObj;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 uses ObjDec, Input_U, EleMenu, Global, LongStr, WordStr,
       {$IFDEF WINGUI}
        Win_Main,
      {$ENDIF}
      CentrStr, Crt, Question, StUtils, Support, elx_BBS,
      ElLog_U, FileRout, GenFile;
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.ShowMsgHeader;
var Quest   : QuestObj;
    InsStr  : String;
    PrivStr : String;
    TmpArray: Array[0..6] of String;
    elxObj  : pElxBbsObj;
begin
  if GetScriptType('edithdr') = 'Q-A' then
    begin
      Quest.Init;
      Quest.QInfo^.Answers^.Put(01, MsgInfo.FromWho);
      Quest.QInfo^.Answers^.Put(02, MsgInfo.ToWho);
      Quest.QInfo^.Answers^.Put(03, MsgInfo.Subject);
      Quest.QInfo^.Answers^.Put(04, FStr(MsgInfo.MsgNumber));
      Quest.QInfo^.Answers^.Put(05, MsgInfo.AreaName);

      if MsgInfo.IsPrivate then
        Quest.QInfo^.Answers^.Put(06, 'YES')
         else Quest.QInfo^.Answers^.Put(06, 'NO');

      if InsertMode then
        Quest.QInfo^.Answers^.Put(07, 'YES')
         else Quest.QInfo^.Answers^.Put(07, 'NO');

      Quest.Process('edithdr /N', false, '');
      Quest.Done;
    end { if }
      else begin
             {-- Setup the defaults ---------------------------------------}
             if MsgInfo.IsPrivate then PrivStr := 'YES'
               else PrivStr := 'NO';

             if InsertMode then InsStr := 'YES'
               else InsStr := 'NO';

             {-- Write the text file to pass data -------------------------}
             TmpArray[0] := MsgInfo.FromWho;
             TmpArray[1] := MsgInfo.ToWho;
             TmpArray[2] := MsgInfo.Subject;
             TmpArray[3] := FStr(MsgInfo.MsgNumber);
             TmpArray[4] := MsgInfo.AreaName;
             TmpArray[5] := PrivStr;
             TmpArray[6] := InsStr;
             CreateTextFile('edithdr.inf', TmpArray);

             {-- Now actually run the script ------------------------------}
             New(elxObj, Init);
             elxObj^.RunElexerScript('edithdr', '', true);
             Dispose(elxObj, Done);

             {-- and erase that file --------------------------------------}
             EraseFile('edithdr.inf');
           end; { else }

  Flush(Output);

  EditColor := {$IFDEF WINGUI}Form1.ColorConsole1.{$ENDIF} TextAttr;
  WinRec.MinY := OutputObj^.WhereY;
end; { proc. ShowMsgHeader }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.ShowMsgFooter;
var Quest : QuestObj;
    elxObj: pElxBbsObj;
begin
  if GetScriptType('editftr') = 'Q-A' then
    begin
      Quest.Init;
      Quest.Process('editftr /N', false, '');
      Quest.Done;
    end
      else begin
             New(elxObj, Init);
             elxObj^.RunElexerScript('editftr', '', true);
             Dispose(elxObj, Done);
           end; { else }

  WinRec.MaxY := OutputObj^.WhereY;
end; { proc. ShowMsgFooter }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.ShowTextLine(YPos: Byte; const TempStr: String);
begin
  WriteLn('`G1,', YPos,':', TempStr, '`E:');

  OutputObj^.ResetLines(01);
end; { proc. ShowTextLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


procedure tFsedObj.ShowTextBuffer;
var Counter  : Longint;
    StartPos : Longint;
    TempStr  : String;
begin
  StartPos := CurrentLine - CurCursor.CurRow;
  if (StartPos + WinRec.MinY) < 1 then
     StartPos := -(WinRec.MinY-1);

  for Counter := WinRec.MinY to WinRec.MaxY do
    begin
      TempStr := TextLines^.Get(StartPos + Counter);
      ShowTextLine(Counter, TempStr);
    end; { for }
end; { proc. ShowTextBuffer }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.UpdateCursor(Force: Boolean);
var DoUpdate: Boolean;
begin
  { First, correct the cursor position so we can't get out of bounds }
  if CurCursor.CurCol < WinRec.MinX then CurCursor.CurCol := WinRec.MinX;
  if CurCursor.CurCol > WinRec.MaxX then CurCursor.CurCol := WinRec.MaxX;
  if CurCursor.CurRow < WinRec.MinY then begin
                                           CurCursor.CurRow := WinRec.MinY;
                                           ShowTextBuffer;

                                           CurCursor.PrevCol := CurCursor.CurCol + 01;
                                           CurCursor.PrevRow := CurCursor.CurRow + 01;
                                         end; { if }
  if CurCursor.CurRow > WinRec.MaxY then begin
                                           CurCursor.CurRow := WinRec.MaxY;
                                           ShowTextBuffer;

                                           CurCursor.PrevCol := CurCursor.CurCol + 01;
                                           CurCursor.PrevRow := CurCursor.CurRow + 01;
                                         end; { if }

  { Now, send the minimal change needed }
  DoUpdate := false;
  if (CurCursor.PrevCol <> CurCursor.CurCol) OR (Force) then
    begin
      Write('`X', CurCursor.CurCol, ':');
      DoUpdate := true;
    end; { if }

  if (CurCursor.PrevRow <> CurCursor.CurRow) OR (Force) then
    begin
      Write('`Y', CurCursor.CurRow, ':');
      DoUpdate := true;
    end; { if }

  if DoUpdate then
    InputObj^.UpdateScrn;

  CurCursor.PrevCol := CurCursor.CurCol;
  CurCursor.PrevRow := CurCursor.CurRow;
end; { proc. UpdateCursor }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.InsertNewLine(const NewLine: String);
var Counter : Longint;
    TempStr : String;
begin
  if ReadOnly then EXIT;

  for Counter := (MaxEditLines-01) downto CurrentLine do
    begin
      TempStr := TextLines^.Get(Counter);
      TextLines^.Put(Counter + 01, TempStr);
    end; { for }

  TextLines^.Put(CurrentLine + 01, NewLine);
end; { proc. InsertNewLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.GotoEndOfLine;
var TempStr: String;
begin
  TempStr := TextLines^.Get(CurrentLine);
  CurCursor.CurCol := Length(TrimRight(TempStr)) + 01;

  UpdateCursor(true);
end; { proc. GotoEndOfLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.GotoStartOfLine;
begin
  CurCursor.CurCol := 01;
end; { proc. GotoStartOfLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.PageDown;
begin
  Inc(CurCursor.CurRow, WinRec.MaxY - WinRec.MinY);
  Inc(CurrentLine, WinRec.MaxY - WinRec.MinY);

  if CurrentLine > MaxEditLines then
    begin
      CurrentLine := MaxEditLines;
      CurCursor.CurRow := WinRec.MaxY + 01;
    end; { if }

  UpdateCursor(true);
end; { proc. PageDown }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.PageUp;
begin
  Dec(CurCursor.CurRow, WinRec.MaxY - WinRec.MinY);
  Dec(CurrentLine, WinRec.MaxY - WinRec.MinY);

  if CurrentLine < 01 then
    begin
      CurrentLine := 01;
      CurCursor.CurRow := WinRec.MinY - 01;
    end; { if }

  UpdateCursor(true);
end; { proc. PageUp }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.CursorDown;
begin
  Inc(CurCursor.CurRow);
  Inc(CurrentLine);

  if CurrentLine > MaxEditLines then
    begin
      CurrentLine := MaxEditLines;
      Dec(CurCursor.CurRow);
    end; { if }
end; { proc. CursorDown }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.CursorUp;
begin
  Dec(CurCursor.CurRow);
  Dec(CurrentLine);

  if CurrentLine < 01 then
    begin
      CurrentLine := 01;
    end; { if }
end; { proc. CursorUp }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.CursorLeft;
begin
  Dec(CurCursor.CurCol);

  if CurCursor.CurCol < 1 then
   if Currentline > 1 then
     begin
       CursorUp;
       GotoEndOfline;
     end; { if }
end; { proc. CursorLeft }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.CursorRight;
begin
  Inc(CurCursor.CurCol);

  if CurCursor.CurCol > WinRec.MaxX then
   if Currentline < MaxEditLines then
     begin
       CursorDown;
       GotoStartOfline;
     end; { if }
end; { proc. CursorRight }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.AddChar(const CH: Char);
var TempStr: String;
    WrapStr: String;
    NextStr: String;
    Count  : longint;
    AddIt  : Boolean;
begin
  if ReadOnly then EXIT;
  if CH in [#00, #255] then EXIT;

  TempStr := TextLines^.Get(CurrentLine);

  {--------------- Determine wether we're adding or inserting --------------}
  if CurCursor.CurCol >= Length(TempStr) then
    AddIt := true
      else Addit := false;

  if Insertmode then
    begin
      if Length(TrimRight(TempStr)) >= (WinRec.MaxX - 01) then
        begin
          TempStr := TrimRight(TempStr);

          {------------------ Determine the wrapping length ----------------}
          Count := WordCount(TempStr, defExtractWord, false);
          WrapStr := ExtractWord(TempStr, Count, defExtractWord, false, false);

          if Count = 1 then
            begin
              WrapStr := '';
            end; { if }

          {------------------ Delete this from the original string ---------}
          Delete(TempStr, Succ(Length(TempStr) - Length(WrapStr)), Length(WrapStr));

          if Length(TempStr) >=  (WinRec.MaxX - 1) then
           if NOT AddIt then
            begin
              EXIT;
            end; { if }

          if Addit then
            begin
              TextLines^.Put(CurrentLine, TempStr);

              Nextstr := TextLines^.Get(CurrentLine + 01);
              WrapStr := WrapStr + CH;

              if Length(NextStr + WrapStr + CH) < WinRec.MaxX then
                begin
                  if Trim(NextStr) <> '' then CurCursor.CurCol := Length(WrapStr) + 01
                    else CurCursor.CurCol := Length(WrapStr + NextStr) + 01;

                  NextStr := WrapStr + NextStr;
                  TextLines^.Put(CurrentLine + 01, NextStr);

                  CursorDown;
                end
                  else begin
                         InsertNewLine(WrapStr);
                         CursorDown;
                         GotoEndOfLine;
                       end; { if }
            end
              else begin
                     Insert(CH, TempStr, CurCursor.CurCol);

                     TextLines^.Put(CurrentLine, TempStr);

                     Nextstr := TextLines^.Get(CurrentLine + 01);

                     if Length(NextStr + WrapStr + CH) < WinRec.MaxX then
                       begin
                         NextStr := WrapStr + NextStr;
                         TextLines^.Put(CurrentLine + 01, NextStr);

                         Inc(CurCursor.CurCol);
                       end
                        else begin
                               InsertNewLine(WrapStr);
                               Inc(CurCursor.CurCol);
                             end; { if }

                   end; { if }

          {------------------ Show the buffer contents to the screen -------}
          ShowTextBuffer;
        end
         else begin
                FullInsert(CH, TempStr, CurCursor.CurCol);
                Write(CH, Copy(TempStr, CurCursor.CurCol + 01, 255));
                Inc(CurCursor.CurCol);

                TextLines^.Put(CurrentLine, TempStr);
              end; { if }
    end
      else begin
             if Length(TrimRight(TempStr)) >= (WinRec.MaxX - 01) then
               begin
                 CursorDown;
                 CurCursor.CurCol := 01;

                 Tempstr := TextLines^.Get(CurrentLine);
                 TempStr[CurCursor.CurCol] := CH;
                 if Length(TempStr) < CurCursor.CurCol then
                   TempStr[0] := Chr(CurCursor.CurCol);
                 Inc(CurCursor.CurCol);

                 TextLines^.Put(CurrentLine, TempStr);
                 ShowTextLine(CurCursor.CurRow, TempStr);

                 EXIT;
               end; { if }

             TempStr[CurCursor.CurCol] := CH;
             if Length(TempStr) < CurCursor.CurCol then
               TempStr[0] := Chr(CurCursor.CurCol);

             Write(CH);
             Inc(CurCursor.CurCol);

             TextLines^.Put(CurrentLine, TempStr);
           end; { if }

  UpdateCursor(true);
end; { proc. AddChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.DeleteCurrentLine;
var TempStr: String;
    Counter: Longint;
begin
  if ReadOnly then EXIT;

  if CurrentLine = MaxEditLines then
    begin
      TextLines^.Put(MaxEditLines, '');
    end
      else begin
             for Counter := CurrentLine to MaxEditLines do
               begin
                 TempStr := TextLines^.Get(Counter + 01);
                 TextLines^.Put(Counter, TempStr);
               end; { for }

             TextLines^.Put(MaxEditLines, '');
           end; { if }

  CurCursor.PrevRow := CurCursor.CurRow + 01;
  CurCursor.CurCol := 01;
  ShowTextBuffer;
  UpdateCursor(true);
end; { proc. DeleteCurrentLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.JoinLines(var FirstLine, SecondLine: String);
var Counter: Longint;
    MaxCnt : Longint;
    WordStr: String;
begin
  if ReadOnly then EXIT;

  WordStr := '';
  Counter := 01;
  MaxCnt  := WordCount(SecondLine, defExtractWord, false);

  While (Length(FirstLine) + Length(WordStr)) < 80 do
    begin
      WordStr := WordStr + ExtractWord(SecondLine, Counter, defExtractWord, false, true);
      Inc(Counter);

      if Counter > MaxCnt then BREAK;

      if (Length(FirstLine) + Length(WordStr)) > 78 then
        begin
          RemoveWordNr(WordStr, Counter - 01, defExtractword, false);
          WordStr := TrimLeft(WordStr);
          BREAK;
        end; { if }
    end; { while }

  if WordStr <> '' then
    begin
      FirstLine := FirstLine + WordStr;

      { Remove the old string }
      Delete(SecondLine, 1, Length(WordStr));
    end; { if }
end; { proc. JoinLines }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.BackSpace;
var TempStr: String;
    WordStr: String;
begin
  if ReadOnly then EXIT;
  TempStr := '';

  if CurCursor.CurCol = 01 then
    begin
      if CurrentLine > 01 then
        begin
          TempStr := TextLines^.Get(CurrentLine);

          if Length(TempStr) = 00 then
            begin
              DeleteCurrentLine;
              CursorUp;

              TempStr := TextLines^.Get(CurrentLine);
              CurCursor.CurCol := Length(TempStr) + 01;
            end
              else begin
                     TempStr := TextLines^.Get(CurrentLine - 01);
                     WordStr := TextLines^.Get(CurrentLine);

                     JoinLines(TempStr, WordStr);
                     TextLines^.Put(CurrentLine - 1, TempStr);
                     TextLines^.Put(CurrentLine, WordStr);
                     if WordStr = '' then DeleteCurrentLine;

                     CursorUp;
                     GotoEndOfLine;
                   end; { does the word fit on the prev. line? }
        end; { if }

    end { if }
      else if CurCursor.CurCol > 01 then
             begin
               TempStr := TextLines^.Get(CurrentLine);

               Delete(TempStr, CurCursor.CurCol - 01, 1);
               Write('`X1:`E:', TempStr);
               CursorLeft;

               TextLines^.Put(CurrentLine, TempStr);
            end; { if }

  ShowTextBuffer;
  UpdateCursor(true);
end; { proc. BackSpace }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.CenterLine;
var TempStr: String;
begin
  if ReadOnly then EXIT;

  TempStr := TextLines^.Get(CurrentLine);

  TempStr := CenterJust(Trim(TempStr), WinRec.MaxX - WinRec.MinX, false);

  TextLines^.Put(CurrentLine, TempStr);

  ShowTextLine(CurCursor.CurRow, TempStr);
  UpdateCursor(true);
end; { proc. Centerline }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.DeleteChar;
var TempStr: String;
begin
  if ReadOnly then EXIT;

  TempStr := TextLines^.Get(CurrentLine);

  Delete(TempStr, CurCursor.CurCol, 1);

  TextLines^.Put(CurrentLine, TempStr);

  ShowTextLine(CurCursor.CurRow, TempStr);
  UpdateCursor(true);
end; { proc. DeleteChar }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.Inserttab;
var Counter: Longint;
begin
  if ReadOnly then EXIT;

  for Counter := 01 to 06 do
    AddChar(#32);
end; { proc. InsertTab }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.DoQuoteLine;
var CurrSave: Longint;
    CurCurs : CurCursorRecord;
    WinSav  : WinRecRecord;
    TempStr : String;
begin
  Write('`A', EditColor, ':');
  IsQuoting := false;
  TempStr := TextLines^.Get(CurrentLine);
  TextLines := SaveMsgText;
  CurrSave := CurrentLine;
  CurrentLine := SaveCurrLine;
  ReadOnly := false;
  WinSav := WinRec;
  WinRec := SaveWinRec;
  CurCurs := CurCursor;
  WinRec.MaxY := WinSav.MinY - 2;

  InsertNewLine(TempStr);
  Inc(CurrentLine);
  SaveCurrLine := CurrentLine;
  UpdateCursor(true);

  Write('`A', QuoteColor, ':');
  IsQuoting := true;
  TextLines := QuoteLines;
  CurrentLine := CurrSave + 1;
  CurCursor := CurCurs;
  CurCursor.CurRow := CurCursor.CurRow + 1;
  WinRec := WinSav;
  ReadOnly := true;
  UpdateCursor(true);
end; { proc. DoQuoteLine }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.DoEnter;
var TempStr : String;
    CopyStr : String;
begin
  if IsQuoting then
    begin
      DoQuoteLine;
      EXIT;
    end; { if }

  TempStr := TextLines^.Get(CurrentLine);

  if NOT ReadOnly then
   if InsertMode then
     begin
       if TempStr <> '' then
         begin
           CopyStr := Copy(TempStr, CurCursor.CurCol, Length(TempStr) - Pred(CurCursor.CurCol));
           if Trim(CopyStr) = '' then CopyStr := '';
         end { if }
           else CopyStr := '';

       InsertNewLine(CopyStr);

       if CopyStr = TempStr then TempStr := ''
        else Delete(TempStr, Succ(Length(TempStr) - Length(CopyStr)), Length(TempStr));

       TextLines^.Put(CurrentLine, TempStr);
     end; { if }

  CurCursor.CurCol := 01;
  CursorDown;
  ShowTextBuffer;
end; { proc. DoEnter }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.DeleteFirstWord;
var TempStr: String;
begin
  if ReadOnly then EXIT;

  TempStr := TextLines^.Get(CurrentLine);

  if TempStr[1] <> #32 then
    RemoveWordNr(TempStr, 1, defExtractword, False);
  TempStr := TrimLeft(TempStr);

  TextLines^.Put(CurrentLine, TempStr);

  ShowTextLine(CurCursor.CurRow, TempStr);
  UpdateCursor(true);
end; { proc. DeleteFirstWord }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.WordLeft;
var TempStr: String;
    Counter: Longint;
begin
  TempStr := TextLines^.Get(CurrentLine);

  if CurCursor.CurCol = 1 then
    begin
      if CurrentLine > 01 then
        begin
          CursorUp;
          GotoEndOfLine;
        end; { if }

      EXIT;
    end; { if }

  if CurCursor.CurCol < 02 then
    CurCursor.CurCol := 02;

  for Counter := (CurCursor.CurCol - 02) downto 01 do
    begin

      if TempStr[Counter] in defExtractword then
        begin
          CurCursor.CurCol := Counter + 01;
          BREAK;
        end; { if }

      if Counter = 01 then
        begin
          CurCursor.CurCol := 01;
          BREAK;
        end; { if }
    end; { for }

end; { proc. WordLeft }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.WordRight;
var TempStr: String;
    Counter: Longint;
begin
  TempStr := TextLines^.Get(CurrentLine);

  if CurCursor.CurCol = Length(TempStr) then
    begin
      if CurrentLine < MaxEditLines then
        begin
          CursorDown;
          GotoStartOfLine;
        end; { if }

      EXIT;
    end; { if }

  for Counter := CurCursor.CurCol to Length(TempStr) do
    begin

      if TempStr[Counter] in defExtractword then
        begin
          CurCursor.CurCol := Counter + 01;
          BREAK;
        end; { if }

      if Counter = Length(TempStr) then
        begin
          CurCursor.CurCol := Length(TempStr);
          BREAK;
        end; { if }
    end; { for }

end; { proc. WordRight }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.RedrawScreen;
begin
  ShowMsgHeader;
  ShowMsgFooter;
  ShowTextBuffer;

  UpdateCursor(true);
end; { proc. RedrawScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.ShowHelpScreen;
var Quest  : QuestObj;
    elxObj : pelxBbsObj;
begin
  if GetScriptType('edithlp') = 'Q-A' then
    begin
      Quest.Init;
      Quest.Process('edithlp /N', false, '');
      Quest.Done;
    end
      else begin
             New(elxObj, Init);
             elxObj^.RunElexerScript('edithlp', '', true);
             Dispose(elxObj, Done);
           end; { else }

  RedrawScreen;
end; { proc. ShowHelpScreen }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.EditMenu(var InputCH: Char);
var Quest    : QuestObj;
    elxObj   : pElxBbsObj;
    Result   : String;
begin
  if GetScriptType('editmnu') = 'Q-A' then
    begin
      Quest.Init;
      Quest.Process('editmnu /N', false, '');

      Result := Quest.GetReturnResult;

      Quest.Done;
    end
      else begin
             New(elxObj, Init);
             if NOT elxObj^.RunElexerScript('editmnu', '', false) then
               begin
                 RaLog('!', 'Error occured while executing EDITMNU EleXer script - falling back');
               end; { if }

             Result := elxObj^.GetElxReturnString;
             Dispose(elxObj, Done);
           end; { else }

  InputCH := #00;                                       { Invalidate input }

  if Result = 'ABORT' then
    begin
      InputCH := #27;
    end; { if }

  if Result = 'CONTINUE' then
     begin
       InputCH := #00;
     end; { if }

  if Result = 'HELP' then
     begin
       ShowHelpScreen;
       InputCH := #00;
     end; { if }

  if Result = 'SAVE' then
     begin
       InputCH := #26;
     end; { if }
end; { proc. EditMenu }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.ToggleInsert;
begin
  InsertMode := NOT InsertMode;

  ShowMsgHeader;
  Updatecursor(true);
end; { proc. ToggleInsert }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.ShowQuoteWindow;
var Quest  : QuestObj;
    elxObj : pElxBbsObj;
begin
  if IsQuoting then EXIT;

  {-------------------- Save the original content and settings -------------}
  SaveWinRec := WinRec;
  SaveCursor := CurCursor;
  SaveMsgtext := TextLines;
  TextLines := QuoteLines;
  SaveCurrLine := CurrentLine;
  SaveMajor := Currentline;             { SaveCurrLine is needed modified }
  IsQuoting := true;
  ReadOnly := true;

  {-------------------- Get the last 8 lines of the window and use it ------}
  WinRec.MinY := (WinRec.MaxY - 7);
  CurCursor.CurCol := 1;
  CurCursor.CurRow := 1;
  CurCursor.PrevCol := 0;
  CurCursor.PrevRow := 0;
  CurrentLine := 01;

  {-- Show the quoting header ----------------------------------------------}
  if GetScriptType('editqto') = 'Q-A' then
    begin
      Quest.Init;
      Quest.Process('editqto ' + FStr(Pred(WinRec.MinY)) + ' /N', false, '');
      Quest.Done;
    end
      else begin
             New(elxObj, Init);
             elxObj^.RunElexerScript('editqto', FStr(Pred(WinRec.MinY)), true);
             Dispose(elxObj, Done);
           end; { else }

  Flush(Output);
  QuoteColor := {$IFDEF WINGUI}Form1.ColorConsole1.{$ENDIF} TextAttr;

  {---------------------- Show the actual message text ---------------------}
  ShowTextBuffer;
end; { proc. ShowQuoteWindow }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tFsedObj.CloseQuoteWindow;
begin
  if NOT IsQuoting then EXIT;

  Write('`A', EditColor, ':');
  TextLines := SaveMsgText;
  WinRec := SaveWinRec;
  CurCursor := SaveCursor;
  IsQuoting := false;
  CurrentLine := SaveMajor;
  ShowTextBuffer;
  UpdateCursor(true);

  ReadOnly := false;
end; { proc. CloseQuoteWindow }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFsedObj.HandleCursors(DoQuotes: Boolean): Boolean;
var InputCH: Char;
begin
  HandleCursors := false;

  repeat
    UpdateCursor(false);              { Update the current cursor position }

    InputCH := InputObj^.ReadKey;

    if Trim(TextLines^.Get(CurrentLine)) = '' then
     if InputCH = '/' then
       begin
         EditMenu(InputCH);
         UpdateCursor(true);
       end; { if }

    if InputCH = #27 then
      begin
        InputCH := GetArrowKeys;

        Case InputCH of
        { Escape } #27 : EditMenu(InputCH);
        { Up    }  'A' : CursorUp;
        { Down  }  'B' : CursorDown;
        { Right }  'C' : CursorRight;
        { Left  }  'D' : CursorLeft;
        { Home }   'H' : GotoStartOfLine;
        { End }    'K' : GotoEndOfLine;
        end; { case }

        if InputCH = #26 then
          HandleCursors := true;
      end { if }
        else begin
               Case InputCH of
      { ^A }     #01  : WordLeft;
      { ^B }     #02  : CenterLine;
      { ^C }     #03  : PageDown;
      { ^D }     #04  : CursorRight;
      { ^E }     #05  : CursorUp;
      { ^F }     #06  : WordRight;
      { ^G }     #07  : DeleteChar;
      { ^H }     #08  : BackSpace;
      { ^I }     #09  : InsertTab;
      { ^J }     #10  : CenterLine;
      { ^K }     #11  : ; { if DoQuotes then CloseQuoteWindow;  }
      { ^L }     #12  : RedrawScreen;
      { Enter }  #13  : DoEnter;
      { ^O }     #15  : EditMenu(InputCH);
      { ^P }     #16  : GotoEndOfLine;
      { ^Q }     #17  : if DoQuotes then
                          begin
                            if (NOT IsQuoting) then ShowQuoteWindow
                              else CloseQuoteWindow;
                          end; { if }
      { ^R }     #18  : PageUp;
      { ^S }     #19  : CursorLeft;
      { ^T }     #20  : DeleteFirstWord;
      { ^V }     #22  : begin
                          ToggleInsert;

                          if InputObj^.KeyPressed then
                           if InputObj^.ReadKey = #9 then ;
                        end; { if }
      { ^W }     #23  : GotoStartOfLine;
      { ^X }     #24  : CursorDown;
      { ^Y }     #25  : DeleteCurrentLine;
      { ^Z }     #26  : HandleCursors := true;
      { Delete } #127 : DeleteChar;
                   else AddChar(InputCH);
               end { case }

             end; { else }

  until (InputCH in [#27, #26]);
end; { proc. HandleCursors }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tFsedObj.FullScreenEditor(var _TextLines,
                                       _QuoteLines : PStringArrayObj;
                                       DoQuotes    : Boolean;
                                   var _MsgInfo    : EditorMsgInfoRec): Longint;
var Counter: Longint;
    DoSave : Boolean;
begin
  OutputObj^.ClearScreen;

  WinRec.MinX := 01;
  WinRec.MaxX := 80;
  WinRec.MinY := 04;
  WinRec.MaxY := 20;
  CurCursor.PrevRow := 00;
  CurCursor.PrevCol := 00;
  CurCursor.CurRow := WinRec.MinY;
  CurCursor.CurCol := WinRec.MinX;
  CurrentLine := 01;
  InsertMode := true;
  IsQuoting := false;
  TextLines := _TextLines;
  QuoteLines := _QuoteLines;
  MsgInfo := _MsgInfo;

  ShowMsgHeader;
  ShowMsgFooter;

  DoSave := HandleCursors(DoQuotes);

  if IsQuoting then
    CloseQuoteWindow;

  for Counter := MaxEditLines downto 0 do
    if TextLines^.Get(Counter) <> '' then
      begin
        MaxLine := Counter;
        BREAK;
      end; { for }

  if NOT DoSave then MaxLine := -1;

  FullScreenEditor := MaxLine;
end; { proc. FullScreenEditor }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tFsedObj.Init;
begin
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tFsedObj.Done;
begin
end; { destructor Done }

end. { unit FSED }
