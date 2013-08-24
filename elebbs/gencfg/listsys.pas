unit ListSys;
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
{$IFNDEF VER0_99_13}
  {$I COMPILER.INC}
{$ENDIF}
{$IFDEF WIN32}
  {$H-}
{$ENDIF}
(*
**
** LISTSYS.TPU, LISTs support unit for ELCONFIG
**
** Copyright (c) 1998 by Maarten Bekers
**
** Created : 04-Nov-1998
** Last update : 04-Nov-1998
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

uses CfgRec, Global, MenuSys, ScrnU, Ranges,
      ObjDec
      {$IFDEF WITH_DEBUG}
        ,Debug_U
      {$ENDIF};

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

Type List_StringType            = String[250];

     LF_GetInfo       = Procedure(var Info1, Info2, Info3, Info4, Info5, Info6, Info7, Info8, Info9: List_StringType;
                                            ItemNr: Longint; IsHiLight: Longint);
     LF_Activate      = Function(HiLight: Longint; HiBarPos: Longint): Longint;
     LF_Seek          = Function(Str: String): Longint;
     LF_AbortCheck    = Function(CH:Char): Boolean;
     LF_Keypress      = Function(CH:Char; HiLight: Longint; var HiBarPos, TopOfScrn: Longint;
                                           var StartRange, EndRange: Longint):Longint;
     LF_GetItems      = Function: Longint;
     LF_CreateNew     = Function: Boolean;
     LF_AllCharHook   = Function(CH: Char; HiLight: Longint): Longint;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*)

function  EmptyAllCharHook(CH: Char; HiLight: Longint): Longint; {$IFDEF MSDOS} far; {$ENDIF}
procedure DoList(ViewWindow: Boolean;
                 InfoStart: Byte; Len1, Len2, Len3, Len4, Len5, Len6, Len7, Len8, Len9: Byte;
                 X1, Y1, X2, Y2: Byte; const Title: String;
                 MsgStr: String;
                 GetInfo      : LF_GetInfo;
                 Activate     : LF_Activate;
                 Seek         : LF_Seek;
                 AbortFunc    : LF_AbortCheck;
                 Keypress     : LF_Keypress;
                 GetItems     : LF_GetItems;
                 CreateNewFile: LF_CreateNew;
                 AllCharHook  : LF_AllCharHook;
                 Selectable   : Boolean;
                 StartItem, StartTopOfScreen: Longint;
                 AddSpace,  SetstartOfList: Boolean);

Const DefaultWaitFunc: Function: Boolean = nil;

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 uses GenCfg, CentrStr, StUtils, StrEdit;
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function EmptyAllCharHook(CH: Char; HiLight: Longint): Longint;
begin
  EmptyAllCharHook := -1;
end; { proc. EmptyAllCharHook }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure DoList(ViewWindow: Boolean;
                 InfoStart: Byte; Len1, Len2, Len3, Len4, Len5, Len6, Len7, Len8, Len9: Byte;
                 X1, Y1, X2, Y2: Byte; const Title: String;
                 MsgStr: String;
                 GetInfo: LF_GetInfo;
                 Activate: LF_Activate;
                 Seek: LF_Seek;
                 AbortFunc: LF_AbortCheck;
                 Keypress: LF_Keypress;
                 GetItems: LF_GetItems;
                 CreateNewFile: LF_CreateNew;
                 AllCharHook: LF_AllCharHook;
                 Selectable: Boolean;
                 StartItem, StartTopOfScreen: LongInt;
                 AddSpace, SetStartOfList: Boolean);
var CH: Char;
    TopPosition: Longint;           { ItemNr at top of the screen, zero based }
    HiBarPos: Longint;                                { Bar position, 1 based }
    FitsOnScreen: Byte;             { Number of items that fits on the screen }
    SeekStr: String;
    SeekPos: LongInt;
    StartRange : Longint;
    EndRange: Longint;
    TotalItems: Longint;
    SaveScrn: Pointer;

procedure ShowItemsOnScreen;
var Teller: Byte;
    OnScreen: Longint;
    Info1,
    Info2,
    Info3,
    Info4,
    Info5,
    Info6,
    Info7,
    Info8,
    Info9: List_StringType;
    Color: Byte;
begin
  OnScreen := TotalItems;
  If OnScreen>FitsOnScreen then OnScreen := FitsOnScreen;
  If OnScreen>TotalItems then OnScreen := TotalItems;

  For Teller:=01 to OnScreen do
    begin;
        (**
          WriteAT(1, 1, 15, 'Teller/OnScreen= '+FStr(Teller)+' / '+FStr(OnScreen));
          WriteAT(1, 2, 15, 'TopPosition+Teller='+FStr(TopPosition+Teller));
        **)

      Info1 := ''; Info2 := ''; Info3 := '';
      Info4 := ''; Info5 := ''; Info6 := '';
      Info7 := ''; Info8 := ''; Info9 := '';

      GetInfo(Info1, Info2, Info3, Info4, Info5, Info6, Info7, Info8, Info9, TopPosition + Teller, TopPosition + HiBarPos);

      If (InRange(TopPosition+Teller, StartRange, EndRange)) OR (Teller=HibarPos) then Color := mnuPullHiColor
       else Color := mnuNormColor;

      If AddSpace then
       WriteAT(InfoStart, Y1 + Teller, Color, ' '+
               MakeLen(Info1, Len1, space, false, false)+
               MakeLen(Info2, Len2, space, false, false)+
               MakeLen(Info3, Len3, space, false, false)+
               MakeLen(Info4, Len4, space, false, false)+
               MakeLen(Info5, Len5, space, false, false)+
               MakeLen(Info6, Len6, space, false, false)+
               MakeLen(Info7, Len7, space, false, false)+
               MakeLen(Info8, Len8, space, false, false)+
               MakeLen(Info9, Len9, space, false, false)) else
       WriteAT(InfoStart, Y1 + Teller, Color,
               MakeLen(Info1, Len1, space, false, false)+
               MakeLen(Info2, Len2, space, false, false)+
               MakeLen(Info3, Len3, space, false, false)+
               MakeLen(Info4, Len4, space, false, false)+
               MakeLen(Info5, Len5, space, false, false)+
               MakeLen(Info6, Len6, space, false, false)+
               MakeLen(Info7, Len7, space, false, false)+
               MakeLen(Info8, Len8, space, false, false)+
               MakeLen(Info9, Len9, space, false, false));
    End; { For Teller }

  If (FitsOnScreen>OnScreen) then
   For Teller:=(OnScreen+1) to FitsOnScreen do
       WriteAT(InfoStart, Y1 + Teller, mnuNormColor,
               MakeLen('', Len1, space, false, false)+
               MakeLen('', Len2, space, false, false)+
               MakeLen('', Len3, space, false, false)+
               MakeLen('', Len4, space, false, false)+
               MakeLen('', Len5, space, false, false)+
               MakeLen('', Len6, space, false, false)+
               MakeLen('', Len7, space, false, false)+
               MakeLen('', Len8, space, false, false)+
               MakeLen('', Len9, space, false, false));
end; { proc. ShowItemsOnScreen }

begin
{$IFDEF WITH_DEBUG}
  DebugObj.DebugLog(logFastScrn, 'DoList: '+Title);
{$ENDIF}

  CH := #00;
  If ViewWindow then
   ShadFillBoxTitle(X1, Y1, X2, Y2, mnuBoxColor, mnuStyle, True, Title);

  FitsOnScreen := (Y2 - Y1) - 1;
  TotalItems := GetItems{$IFDEF FPC}(){$ENDIF};
  SeekStr := '';

  (* Was: TopPosition := 00 ; *)
  (* TopPosition := 00; *)
  TopPosition := StartItem - 01;

  HiBarPos := 01;
  StartRange := -1;
  EndRange := -1;

  If StartItem>1 then
   If StartItem <= FitsOnScreen then
     begin
       TopPosition := 00;
       HiBarPos := StartItem;
      end; { Past nog harstikke goed }

  If StartItem>1 then
   If (TopPosition+FitsOnScreen) > TotalItems then
         begin
           Dec(TopPosition, FitsOnScreen-1);
           HiBarPos := FitsOnScreen;
         end; { if }

  if SetStartOfList then
    begin
      if StartItem <= FitsOnScreen then
       begin
         TopPosition := StartTopOfScreen;
         HiBarPos := StartItem;
       end; { if }
    end; { if }

  repeat;
      DirectScrnUpdate := false;
      if NOT ExtraWaiting then WriteDescrip(mnuMsgColor, MsgStr);

      TotalItems := GetItems{$IFDEF FPC}(){$ENDIF};
      If TotalItems=00 then begin;
                              if NOT CreateNewFile{$IFDEF FPC}(){$ENDIF} then
                                 EXIT;
                              TotalItems := 01;
                            end; { CreateNewMessageRa }

      If StartRange>-1 then EndRange := TopPosition + HiBarPos
       else EndRange := -1;


     If ViewWindow then
        WriteAT(X1 + 1, Y2, mnuBoxColor, Dup(StyleStr[mnuStyle, 10], (X2 - X1)-1));

{!!}      if HiBarPos > FitsOnScreen then
         begin
           Inc(TopPosition, (HiBarPos - FitsOnScreen));
           HiBarPos := FitsonScreen;
         end; { if }
      If HiBarPos<01 then HiBarPos := 01;
      While ((TopPosition+FitsOnScreen) > TotalItems)
             AND (TopPosition>=00) do Dec(TopPosition);

      If TopPosition<00 then TopPosition := 00;
      While (TopPosition+HiBarPos) > TotalItems do Dec(HiBarPos);

      If ViewWindow then
       begin;
        If (TopPosition>00) AND (TopPosition<(TotalItems - FitsOnScreen)) then
          WriteRight(X2 - 3, Y2, mnuBoxColor, #32 + mnuUpArrow +
                                                    mnuDnArrow + ' for more ');

        If (TopPosition=00) AND (TotalItems > FitsOnScreen) then
          WriteRight(X2 - 3, Y2, mnuBoxColor, '  ' + mnuDnArrow + ' for more ');

        If (TopPosition=(TotalItems - FitsOnScreen)) AND (TotalItems > FitsOnScreen) then
          WriteRight(X2 - 3, Y2, mnuBoxColor, ' ' + mnuUpArrow + '  for more ');

        If SeekStr<>'' then
          WriteAT(X1 + 1, Y2, mnuBoxColor, ' ' + SeekStr + ' ');
       end; { If ViewWindow }

      If NOT ExtraWaiting then
         ShowItemsOnScreen;               { Show all the items on the screen }

      If NOT (CH in [#33..#255]) then SeekStr := '';

(*
      writeat(1,1,15,'TopPosition/HiBarPos/FitsOnScreen: '+FStr(TopPosition)+' / '+Fstr(HiBarPos)+ ' / '+
      FStr(FitsOnScreen)+']');
*)
      DirectScrnUpdate := True;
      UpdateScreenBuffer(DirectScrnUpdate);
      CH := ReadKey;

      SeekPos := AllCharHook(CH, TopPosition + HiBarPos);
      if SeekPos>-1 then
       If SeekPos<=FitsOnScreen then
               begin;
                 HiBarPos := SeekPos;
                 TopPosition := 00;
               end { the info fits on screen }
        else begin;
               HiBarPos := FitsOnScreen;
               TopPosition := SeekPos - FitsOnScreen;
             end; { Doesn't fit on the screen }

      Case CH of
    #33..#255 : begin;
{$IFDEF WITH_DEBUG}
                  DebugObj.DebugLog(logFastScrn, 'DoList ['+Title+']: SeekStr='+SeekStr);
{$ENDIF}
                  SeekStr := SeekStr + UpCase(CH);
                  SeekPos := Seek(SeekStr);

                  If SeekPos<00 then SeekStr := ''
                   else begin;
                          If SeekPos<=FitsOnScreen then
                              begin;
                                 HiBarPos := SeekPos;
                                 TopPosition := 00;
                              end { the info fits on screen }
                                else begin;
                                       HiBarPos := FitsOnScreen;
                                       TopPosition := SeekPos - FitsOnScreen;
                                     end; { Doesn't fit on the screen }
                        end; { Seek }
                end; { SeekStr }
          #32 : If Selectable then
                 If StartRange=-1 then begin;
                                        StartRange := TopPosition + HiBarPos;
                                        EndRange := TopPosition + HiBarPos;
                                       end else StartRange := -1;
          #13 : begin;
{$IFDEF WITH_DEBUG}
                    DebugObj.DebugLog(logFastScrn, 'DoList ['+Title+']: Enter='+SeekStr);
{$ENDIF}
                    SaveScreen(SaveScrn);

                    SeekPos := Activate(TopPosition + HiBarPos, HiBarPos);
                    if SeekPos>-1 then
                     If SeekPos<=FitsOnScreen then
                          begin;
                             HiBarPos := SeekPos;
                             TopPosition := 00;
                          end { the info fits on screen }
                            else begin;
                                   HiBarPos := FitsOnScreen;
                                    TopPosition := SeekPos - FitsOnScreen;
                                 end; { Doesn't fit on the screen }

                    If NOT ExtraWaiting then
                       RestoreScreen(SaveScrn)
                        else FreeMem(SaveScrn, SizeOf(SaveArrayType));

                    SeekStr := '';
                end; { Activate }
          #00 : begin;
{$IFDEF WITH_DEBUG}
                   DebugObj.DebugLog(logFastScrn, 'DoList ['+Title+']: #00');
{$ENDIF}
                   SeekStr := '';
                   CH := ReadKey;                    { Read Extended key code }

{$IFDEF WITH_DEBUG}
                  DebugObj.DebugLog(logFastScrn, 'DoList ['+Title+']: Before keypress');
{$ENDIF}
                   SeekPos := KeyPress(CH, TopPosition + HiBarPos, HiBarPos, TopPosition, StartRange, EndRange);
{$IFDEF WITH_DEBUG}
                  DebugObj.DebugLog(logFastScrn, 'DoList ['+Title+']: After keypress');
{$ENDIF}

                  if SeekPos>-1 then
                   If SeekPos<=FitsOnScreen then
                     begin;
                      HiBarPos := SeekPos;
                       TopPosition := 00;
                     end { the info fits on screen }
                       else begin;
                              HiBarPos := FitsOnScreen;
                              TopPosition := SeekPos - (FitsOnScreen);
                           end; { Doesn't fit on the screen }


                    Case CH of
          { Down, Right } #77, #80 : If HiBarPos<FitsOnScreen then Inc(HiBarPos)
                                        else Inc(TopPosition);
             { Up, Left } #75, #72 : If HiBarPos>01 then Dec(HiBarPos)
                                        else Dec(TopPosition);
                      { Home } #71 : begin;
                                      HiBarPos := 01;
                                      TopPosition := 00;
                                     end; { Home }
                      { End }  #79 : begin;
                                       TopPosition := TotalItems - FitsOnScreen;
                                       HiBarPos := FitsOnScreen;
                                     end; { End }
                 { PageDown }  #81 : If HiBarPos<FitsOnScreen then
                                       begin;
                                           Inc(TopPosition, HiBarPos);
                                           HiBarPos := FitsOnScreen;
                                       End { HibarPos }
                                         else begin;
                                                 HiBarPos := FitsOnScreen;
                                                 Inc(TopPosition, FitsOnScreen);
                                              end; { HiBarPos }
                    { PageUp } #73 : If HiBarPos=FitsOnScreen then
                                       begin;
                                           Dec(TopPosition, 01);
                                           HiBarPos := 01;
                                       End { HibarPos }
                                         else begin;
                                                 HiBarPos := 01;
                                                 Dec(TopPosition, FitsOnScreen);
                                              end; { HiBarPos }
                   End; { Case }
                end; { Extended key code was pressed }
      End; { Case }

  Until (AbortFunc(CH));
end; { proc. DoList }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

end. { unit LISTSYS }
