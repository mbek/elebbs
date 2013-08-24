unit EditUser;
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
** UserEditor routines for EleBBS
**
** Copyright (c) 1996, 1997 by Maarten Bekers
**
** Created : 21-Sep-1996
** Last update : 23-Aug-1997
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF ISCGI}
  This routine should not be compiled into EleWEB
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 uses CfgRec;

type
  EditRecInfo = record
                   MaxLen, X, Y: Byte;
                   MaxValue,
                   MinValue    : LongInt;
                   Descrip     : String[80];
                end; { EditRecInfo }


type tUserEditObj = Object
         OnlineEditor   : Boolean;  { default: true; }
         ScrnStartpos   : Byte;     { default: 0 }
         AbortKeyPress  : Char;     { default: #0 }
         EditInformation: Array[1..50] of EditRecInfo;
         ExtraAbortSet  : CharSet;
         GlobalCH       : Char;

         {-- Constructors and destructors ----------------------------------}
         constructor Init;
         destructor Done;

         {-- Generic routines ----------------------------------------------}
         procedure UserEdit(RebuildComplete: Boolean);
         function GetLocalkey: Char;

         {-- Private routines ----------------------------------------------}
         private
            procedure ProcessCurField(var CurField: Byte; var CH: Char; EditField: Boolean);
            procedure BoxWindow(X1, Y1, X2, Y2: Byte; Fore, Back: Byte; Title: String);
            procedure DoBeep;
            function  SysOpLineEdit (Var S :String;Len : Byte;Legal,Abort : CharSet;
                      Pad : Char; FirstChar: Boolean) : Char;    (* Editable line input routine (SysOp only!) *)
            procedure FastWrite(X,Y,A: Byte; S: String);
            procedure EditorWrite(Start, Stop,Y, Attr: Byte; Str: String);
            procedure ShowPaddings(Counter: Byte);
            procedure ShowEditLayout(Rebuildcomplete: Boolean);
            procedure MakeEditorsField(CurField: Byte; Info: String; ChangeAttr, EditField: Boolean);
            procedure EditStringField(var Edit: String; var CurField: Byte; var ch:Char; EditField: Boolean);
            procedure GetNewPassword(var CH:Char; var CurField: Byte; EditField: Boolean);
            procedure EditIntegerField(var Edit: LongInt; var CurField: Byte; var ch:Char; EditField: Boolean);
            procedure EditWordField(var Edit: SmallWord; var CurField: Byte; var ch:Char; EditField: Boolean);
            procedure EditByteField(var Edit: Byte; var CurField: Byte; var ch:Char; EditField: Boolean);
            procedure EditFlagField(var Edit: Byte; var CurField: Byte; var ch:Char; EditField: Boolean);
            procedure EditTimeField(var Edit: String; var CurField: Byte; var ch:Char; EditField: Boolean);
            procedure EditDateField(var Edit: String; var CurField: Byte; var ch:Char; EditField: Boolean);
            procedure EditSex(var CurField: Byte; var CH: Char; EditField: Boolean);
            procedure EditDateFormat(var CurField: Byte; var CH: Char; EditField: Boolean);
            procedure EditProtocol(var CurField: Byte; var ch:Char; EditField: Boolean);
            procedure EditCombined(EditField: Boolean; var CH: Char);
            procedure EditUserFlags(EditField: Boolean; var CH: Char);
     end; { tUserEditObj }

type
  pUserEditObj = ^tUserEditObj;


(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
{$IFDEF WITH_FULL}
uses Crt, Global, ScrnU, Colors, StrEdit, WordStr,
     CentrStr, LongStr, Crc_Unit, Ranges, FlagStr, RAL, BitWise,
     Debug_U, Recdif, GenCfg, FastScrn, LineEd, Limit_U, FileRout,
     Main, StatusB, MultiLn, Jdates, Multi, RemScrn,
     ObjDec, StUtils;
{$ENDIF}

{$IFNDEF WITH_FULL}
uses Crt, Global, ScrnU, Colors, StrEdit, StUtils, WordStr,
     CentrStr, LongStr, Crc_Unit, Ranges, FlagStr, RAL, BitWise,
     Debug_U, Recdif, GenCfg, JDates, Multi, RemScrn,
     ObjDec;
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

constructor tUserEditObj.Init;
begin
  OnlineEditor := TRUE;
  ScrnStartPos := 0;
  AbortKeyPress := #0;
end; { constructor Init }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

destructor tUserEditObj.Done;
begin
end; { destructor Done }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tUserEditObj.GetLocalkey: Char;
begin
  repeat
    if Crt.KeyPressed then
      begin
        GetLocalKey := Crt.ReadKey;
        BREAK;
      end; { if }

    {$IFDEF WITH_FULL}
      if RemScrnObj <> nil then
        begin

          if RemScrnObj^.rem_KeyPressed then
            begin
              GetLocalKey := RemScrnObj^.rem_ReadKey;
              BREAK;
            end { if }
              else RemScrnObj^.Rem_GetInputBuffer;
        end; { if }
    {$ENDIF}

    DoSlice;
  until (true=false);

end; { func. GetLocalkey }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)


procedure tUserEditObj.BoxWindow(X1, Y1, X2, Y2: Byte; Fore, Back: Byte; Title: String);
begin
  {$IFNDEF USINGMGR}
    FastScrn.BoxWindow(X1, Y1, X2, Y2, Fore, Back, Title);
  {$ELSE}
    ScrnU.ShadFillBoxTitle(X1, Y1, X2, Y2, MakeAttr(Fore, Back), mnuStyle, True, #32 + Title + #32);
  {$ENDIF}
end; { proc. BoxWindow }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tUserEditObj.DoBeep;
begin
end; { proc. DoBeep }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

function tUserEditObj.SysOpLineEdit (Var S :String;Len : Byte;Legal,Abort : CfgRec.CharSet;
          Pad : Char; FirstChar: Boolean) : Char;    (* Editable line input routine (SysOp only!) *)
var TempCH    : Char;
    OldX, OldY: Byte;
begin
  if #05 in Abort then Abort := Abort + [#77];
  if #04 in Abort then Abort := Abort + [#75];
  if #03 in Abort then Abort := Abort + [#15];
  if #02 in Abort then Abort := Abort + [#72];
  if #01 in Abort then Abort := Abort + [#80];

  OldX := WhereX;
  OldY := WhereY;

  StrEdit.GetString(WhereX, WhereY, TextAttr, S, Legal, Abort + ExtraAbortSet, Len, Len, False, False,False,false,0);
  DirectScrnUpdate := false;
  WriteAT(OldX, OldY, TextAttr, S);

  TempCH := rdLastKey;

  Case TempCH of
    #75 : TempCH := #04;
    #77 : TempCH := #05;
    #72 : TempCH := #02;
    #80 : TempCH := #01;
    #09 : TempCH := #01;
    #15 : TempCH := #02;
  end; (* case *)

  Case TempCH of
    #03 : TempCH := #02;
    #09 : TempCH := #01;
  end; (* case *)

  SysOpLineEdit := TempCH;
end; (* func. SysOpLineEdit *)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tUserEditObj.FastWrite(X,Y,A: Byte; S: String);
begin
 {$IFDEF USINGMGR}
   WriteAT(X,Y,A,S);
 {$ELSE}
   FastScrn.FastWrite(X,Y,A,S)
 {$ENDIF}
end; { proc. FastWrite }

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tUserEditObj.EditorWrite(Start, Stop,Y, Attr: Byte; Str: String);
begin
 {$IFDEF ELEBBS}
    FastWrite(Start, Y, Attr, RightJust(Str, Stop - (Start-1)));
 {$ENDIF}
end; (* proc. EditorWrite *)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

procedure tUserEditObj.ShowPaddings(Counter: Byte);
var CH: Char;
begin
 With EditInformation[Counter] do
  begin;
    If OnlineEditor then
     if Counter <> 4 then { Password }
      FastWrite(X, Y, TextAttr, Dup(mnuBlockChar, MaxLen));

    ProcessCurField(Counter, CH, False);
  end; { With }
end; { proc. ShowPaddings }

{-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-}

procedure tUserEditObj.ShowEditLayout(Rebuildcomplete: Boolean);
var Counter: Byte;
begin
  DirectScrnUpdate := false;

  if Rebuildcomplete then
   begin;
    EditorWrite(02, 11, 02 + ScrnStartPos, TextAttr, 'Name :');
    EditorWrite(02, 11, 03 + ScrnStartPos, TextAttr, 'Handle :');
    EditorWrite(02, 11, 04 + ScrnStartPos, TextAttr, 'Location :');
    EditorWrite(02, 11, 05 + ScrnStartPos, TextAttr, 'Password :');
    EditorWrite(02, 11, 06 + ScrnStartPos, TextAttr, 'Security :');
    EditorWrite(02, 11, 07 + ScrnStartPos, TextAttr, 'Home# :');
    EditorWrite(02, 11, 08 + ScrnStartPos, TextAttr, 'Data# :');
    EditorWrite(02, 11, 09 + ScrnStartPos, TextAttr, 'A flags :');
    EditorWrite(02, 11, 10 + ScrnStartPos, TextAttr, 'B flags :');
    EditorWrite(02, 11, 11 + ScrnStartPos, TextAttr, 'C flags :');
    EditorWrite(02, 11, 12 + ScrnStartPos, TextAttr, 'D flags :');
    EditorWrite(02, 11, 13 + ScrnStartPos, TextAttr, 'Credit :');
    EditorWrite(02, 11, 14 + ScrnStartPos, TextAttr, 'Pending :');
    EditorWrite(02, 11, 15 + ScrnStartPos, TextAttr, 'Group :');
    EditorWrite(02, 11, 16 + ScrnStartPos, TextAttr, 'Sex :');

    EditorWrite(02, 09, 17 + ScrnStartPos, TextAttr, 'Fwd   :');
    EditorWrite(02, 09, 18 + ScrnStartPos, TextAttr, 'Addr1 :');
    EditorWrite(02, 09, 19 + ScrnStartPos, TextAttr, 'Addr2 :');
    EditorWrite(02, 09, 20 + ScrnStartPos, TextAttr, 'Addr3 :');
    EditorWrite(02, 09, 21 + ScrnStartPos, TextAttr, 'Organ.:');
    EditorWrite(02, 09, 22 + ScrnStartPos, TextAttr, 'Comnt :');

    EditorWrite(26, 39, 06 + ScrnStartPos, TextAttr, 'Last time :');
    EditorWrite(26, 39, 07 + ScrnStartPos, TextAttr, 'Last date :');
    EditorWrite(26, 39, 08 + ScrnStartPos, TextAttr, '1st date :');
    EditorWrite(26, 39, 09 + ScrnStartPos, TextAttr, 'Sub date :');
    EditorWrite(26, 39, 10 + ScrnStartPos, TextAttr, 'Birthdate :');

    If OnlineEditor then
       EditorWrite(26, 42, 11 + ScrnStartPos, TextAttr, 'Time remaining :')
        else EditorWrite(26, 42, 11 + ScrnStartPos, TextAttr, 'Time used today :');
    EditorWrite(26, 42, 12 + ScrnStartPos, TextAttr, 'Screen length :');
    EditorWrite(26, 42, 13 + ScrnStartPos, TextAttr, 'Last pwd change :');
    EditorWrite(26, 42, 14 + ScrnStartPos, TextAttr, 'Last DOB check :');
    EditorWrite(26, 42, 15 + ScrnStartPos, TextAttr, 'Date format :');

    EditorWrite(54, 70, 02 + ScrnStartPos, TextAttr, 'Flags  ');
    EditorWrite(54, 70, 03 + ScrnStartPos, TextAttr, 'Combined  ');
    EditorWrite(54, 70, 04 + ScrnStartPos, TextAttr, 'Uploads :');
    EditorWrite(54, 70, 05 + ScrnStartPos, TextAttr, 'Dnloads :');
    EditorWrite(54, 70, 06 + ScrnStartPos, TextAttr, 'UploadK :');
    EditorWrite(54, 70, 07 + ScrnStartPos, TextAttr, 'DnLoadK :');
    EditorWrite(54, 70, 08 + ScrnStartPos, TextAttr, 'TodayK :');
    EditorWrite(54, 70, 09 + ScrnStartPos, TextAttr, 'Messages posted :');
    EditorWrite(54, 70, 10 + ScrnStartPos, TextAttr, 'High msg read :');
    EditorWrite(54, 70, 11 + ScrnStartPos, TextAttr, 'Number of calls :');
    EditorWrite(54, 70, 12 + ScrnStartPos, TextAttr, 'Last msg area :');
    EditorWrite(54, 70, 13 + ScrnStartPos, TextAttr, 'Last file area :');
    EditorWrite(54, 70, 14 + ScrnStartPos, TextAttr, 'Last file group :');
    EditorWrite(54, 70, 15 + ScrnStartPos, TextAttr, 'Last msg gr. :');
    EditorWrite(54, 70, 16 + ScrnStartPos, TextAttr, 'Protocol :');
    EditorWrite(54, 70, 17 + ScrnStartPos, TextAttr, 'Language :');

    If OnlineEditor then
     with GlobalCfg^ do
      begin
        FastWrite(09, mnuScrnLen - 01, MakeAttr(RaConfig^.HiFore, RaConfig^.WindBack), '(TAB) Next field');
        FastWrite(31, mnuScrnLen - 01, MakeAttr(RaConfig^.HiFore, RaConfig^.WindBack), '(SHIFT-TAB) Previous field');
        FastWrite(63, mnuScrnLen - 01, MakeAttr(RaConfig^.HiFore, RaConfig^.WindBack), '(ESC) Exit');
      end
       else begin
              FastWrite(08, mnuScrnLen, TextAttr, '(PgUp) Last user');
              FastWrite(27, mnuScrnLen, TextAttr, '(PgDn) Next user');
              FastWrite(46, mnuScrnLen, TextAttr, '(TAB) Next field');
              FastWrite(64, mnuScrnLen, TextAttr, '(ESC) Exit');
            end; { if }
   end; { if RebuildComplete }

  { User's name }
  EditInformation[01].MaxLen := 35;
  EditInformation[01].X      := 13;
  EditInformation[01].Y      := 02;
  EditInformation[01].Descrip:= 'User''s full name';

  { Handle }
  EditInformation[02].MaxLen := 35;
  EditInformation[02].X      := 13;
  EditInformation[02].Y      := 03;
  EditInformation[02].Descrip:= 'User''s handle/pseudonym';

  { Location }
  EditInformation[03].MaxLen := 25;
  EditInformation[03].X      := 13;
  EditInformation[03].Y      := 04;
  EditInformation[03].Descrip:= 'User''s location (Suburb,City, State)';

  { Password }
  EditInformation[04].MaxLen := 15;
  EditInformation[04].X      := 27;
  EditInformation[04].Y      := 05;
  EditInformation[04].Descrip:= 'Password';

  { Security }
  EditInformation[05].MaxLen  := 5;
  EditInformation[05].X       := 13;
  EditInformation[05].Y       := 06;
  EditInformation[05].MinValue:= 00;
  EditInformation[05].MaxValue:= 65535;
  EditInformation[05].Descrip:= 'Security level (1 to 65,535 or 0 to lock-out user)';

  { Home Number }
  EditInformation[06].MaxLen := 15;
  EditInformation[06].X      := 13;
  EditInformation[06].Y      := 07;
  EditInformation[06].Descrip:= 'Home/voice telephone number';

  { Data Number }
  EditInformation[07].MaxLen := 15;
  EditInformation[07].X      := 13;
  EditInformation[07].Y      := 08;
  EditInformation[07].Descrip:= 'Business/data telephone number';

  { Flags A }
  EditInformation[08].MaxLen := 8;
  EditInformation[08].X      := 13;
  EditInformation[08].Y      := 09;
  EditInformation[08].Descrip:= '"A" flag settings';

  { Flags B }
  EditInformation[09].MaxLen := 8;
  EditInformation[09].X      := 13;
  EditInformation[09].Y      := 10;
  EditInformation[09].Descrip:= '"B" flag settings';

  { Flags C }
  EditInformation[10].MaxLen := 8;
  EditInformation[10].X      := 13;
  EditInformation[10].Y      := 11;
  EditInformation[10].Descrip:= '"C" flag settings';

  { Flags D }
  EditInformation[11].MaxLen := 8;
  EditInformation[11].X      := 13;
  EditInformation[11].Y      := 12;
  EditInformation[11].Descrip:= '"D" flag settings';

  { Credit }
  EditInformation[12].MaxLen  := 7;
  EditInformation[12].X       := 13;
  EditInformation[12].Y       := 13;
  EditInformation[12].MinValue:= 00;
  EditInformation[12].MaxValue:= 00;
  EditInformation[12].Descrip := 'Credit';

  { Pending }
  EditInformation[13].MaxLen  := 7;
  EditInformation[13].X       := 13;
  EditInformation[13].Y       := 14;
  EditInformation[13].MinValue:= 00;
  EditInformation[13].MaxValue:= 00;
  EditInformation[13].Descrip := 'Value of unsent NetMail';

  { Group }
  EditInformation[14].MaxLen  := 5;
  EditInformation[14].X       := 13;
  EditInformation[14].Y       := 15;
  EditInformation[14].MinValue:= 00;
  EditInformation[14].MaxValue:= 65535;
  EditInformation[14].Descrip := 'User''s group number (0 to 65535)';

  { Sex }
  EditInformation[15].MaxLen  := 5;
  EditInformation[15].X       := 13;
  EditInformation[15].Y       := 16;
  EditInformation[15].Descrip := 'User''s sex (SPACE to select)';

  { Forward }
  EditInformation[16].MaxLen  := 35;
  EditInformation[16].X       := 11;
  EditInformation[16].Y       := 17;
  EditInformation[16].Descrip := 'Message-forwarding recipient';

  { Address 1 }
  EditInformation[17].MaxLen  := 50;
  EditInformation[17].X       := 11;
  EditInformation[17].Y       := 18;
  EditInformation[17].Descrip := 'Address line 1';

  { Address 2 }
  EditInformation[18].MaxLen  := 50;
  EditInformation[18].X       := 11;
  EditInformation[18].Y       := 19;
  EditInformation[18].Descrip := 'Address line 2';

  { Address 3 }
  EditInformation[19].MaxLen  := 50;
  EditInformation[19].X       := 11;
  EditInformation[19].Y       := 20;
  EditInformation[19].Descrip := 'Address line 3';

  { Organisation field }
  EditInformation[20].MaxLen  := 50;
  EditInformation[20].X       := 11;
  EditInformation[20].Y       := 21;
  EditInformation[20].Descrip := 'Organisation';

  { Comment }
  EditInformation[21].MaxLen  := 66;
  EditInformation[21].X       := 11;
  EditInformation[21].Y       := 22;
  EditInformation[21].Descrip := 'General comments';

  { Last Time }
  EditInformation[22].MaxLen  := 5;
  EditInformation[22].X       := 41;
  EditInformation[22].Y       := 06;
  EditInformation[22].Descrip := 'Time of user''s last call';

  { Last Date }
  EditInformation[23].MaxLen  := 8;
  EditInformation[23].X       := 41;
  EditInformation[23].Y       := 07;
  EditInformation[23].Descrip := 'Date of user''s last call';

  { 1st Date }
  EditInformation[24].MaxLen  := 8;
  EditInformation[24].X       := 41;
  EditInformation[24].Y       := 08;
  EditInformation[24].Descrip := 'Date of user''s first call';

  { Sub Date }
  EditInformation[25].MaxLen  := 8;
  EditInformation[25].X       := 41;
  EditInformation[25].Y       := 09;
  EditInformation[25].Descrip := 'Subscription expiry date';

  { BirthDate }
  EditInformation[26].MaxLen  := 8;
  EditInformation[26].X       := 41;
  EditInformation[26].Y       := 10;
  EditInformation[26].Descrip := 'User''s date of birth';

  { Time remaining / Minutes used }
  EditInformation[27].MaxLen  := 5;
  EditInformation[27].X       := 44;
  EditInformation[27].Y       := 11;
  EditInformation[27].MinValue:= -32767;
  EditInformation[27].MaxValue:= 32000;
  EditInformation[27].Descrip := 'Minutes of time used today';

  { Screen length }
  EditInformation[28].MaxLen  := 2;
  EditInformation[28].X       := 44;
  EditInformation[28].Y       := 12;
  EditInformation[28].MinValue:= 01;
  EditInformation[28].MaxValue:= 99;
  EditInformation[28].Descrip := 'Screen length';

  { Last pwd change }
  EditInformation[29].MaxLen  := 3;
  EditInformation[29].X       := 44;
  EditInformation[29].Y       := 13;
  EditInformation[29].MinValue:= 00;
  EditInformation[29].MaxValue:= 255;
  EditInformation[29].Descrip := 'Number of calls since the user last changed his/her password';

  { Last dob check }
  EditInformation[30].MaxLen  := 3;
  EditInformation[30].X       := 44;
  EditInformation[30].Y       := 14;
  EditInformation[30].MinValue:= 00;
  EditInformation[30].MaxValue:= 255;
  EditInformation[30].Descrip := 'Number of calls since the last security date of birth check';

  { Date Format }
  EditInformation[31].MaxLen  := 3;
  EditInformation[31].X       := 44;
  EditInformation[31].Y       := 15;
  EditInformation[31].Descrip := 'User''s preferred date format (SPACE to select)';

  { Flags Settings }
  EditInformation[32].MaxLen  := 0;
  EditInformation[32].X       := 72;
  EditInformation[32].Y       := 02;
  EditInformation[32].Descrip := 'User''s flag (ENTER to edit)';

  { Combined area settings }
  EditInformation[33].MaxLen  := 0;
  EditInformation[33].X       := 72;
  EditInformation[33].Y       := 03;
  EditInformation[33].Descrip := 'Combined area settings (ENTER to edit)';

  { Uploads }
  EditInformation[34].MaxLen  := 7;
  EditInformation[34].X       := 72;
  EditInformation[34].Y       := 04;
  EditInformation[34].MinValue:= 00;
  EditInformation[34].MaxValue:= 00;
  EditInformation[34].Descrip := 'Number of files uploaded';

  { Downloads }
  EditInformation[35].MaxLen  := 7;
  EditInformation[35].X       := 72;
  EditInformation[35].Y       := 05;
  EditInformation[35].MinValue:= 00;
  EditInformation[35].MaxValue:= 00;
  EditInformation[35].Descrip := 'Number of files downloaded';

  { UploadsK }
  EditInformation[36].MaxLen  := 7;
  EditInformation[36].X       := 72;
  EditInformation[36].Y       := 06;
  EditInformation[36].MinValue:= 00;
  EditInformation[36].MaxValue:= 00;
  EditInformation[36].Descrip := 'Kilobytes of files uploaded';

  { DownloadK }
  EditInformation[37].MaxLen  := 7;
  EditInformation[37].X       := 72;
  EditInformation[37].Y       := 07;
  EditInformation[37].MinValue:= 00;
  EditInformation[37].MaxValue:= 00;
  EditInformation[37].Descrip := 'Kilobytes of files downloaded';

  { TodayK }
  EditInformation[38].MaxLen  := 7;
  EditInformation[38].X       := 72;
  EditInformation[38].Y       := 08;
  EditInformation[38].MinValue:= 00;
  EditInformation[38].MaxValue:= 00;
  EditInformation[38].Descrip := 'Kilobytes of files downloaded today';

  { Messages posted }
  EditInformation[39].MaxLen  := 5;
  EditInformation[39].X       := 72;
  EditInformation[39].Y       := 09;
  EditInformation[39].MinValue:= 00;
  EditInformation[39].MaxValue:= 65535;
  EditInformation[39].Descrip := 'Number of messages user has posted';

  { High message read }
  EditInformation[40].MaxLen  := 7;
  EditInformation[40].X       := 72;
  EditInformation[40].Y       := 10;
  EditInformation[40].MinValue:= 00;
  EditInformation[40].MaxValue:= 00;
  EditInformation[40].Descrip := 'Highest message read';

  { Number of calls }
  EditInformation[41].MaxLen  := 7;
  EditInformation[41].X       := 72;
  EditInformation[41].Y       := 11;
  EditInformation[41].MinValue:= 00;
  EditInformation[41].MaxValue:= 00;
  EditInformation[41].Descrip := 'Number of times user has called';

  { Last Msg Area }
  EditInformation[42].MaxLen  := 5;
  EditInformation[42].X       := 72;
  EditInformation[42].Y       := 12;
  EditInformation[42].MinValue:= 00;
  EditInformation[42].MaxValue:= 65535;
  EditInformation[42].Descrip := 'Last message area';

  { Last File Area }
  EditInformation[43].MaxLen  := 5;
  EditInformation[43].X       := 72;
  EditInformation[43].Y       := 13;
  EditInformation[43].MinValue:= 00;
  EditInformation[43].MaxValue:= 65535;
  EditInformation[43].Descrip := 'Last file area';

  { Last File Group }
  EditInformation[44].MaxLen  := 5;
  EditInformation[44].X       := 72;
  EditInformation[44].Y       := 14;
  EditInformation[44].MinValue:= 00;
  EditInformation[44].MaxValue:= 65535;
  EditInformation[44].Descrip := 'Last file group selected';

  { Last Message Group }
  EditInformation[45].MaxLen  := 5;
  EditInformation[45].X       := 72;
  EditInformation[45].Y       := 15;
  EditInformation[45].MinValue:= 00;
  EditInformation[45].MaxValue:= 65535;
  EditInformation[45].Descrip := 'Last message group selected';


  { Default protocol }
  EditInformation[46].MaxLen  := 1;
  EditInformation[46].X       := 72;
  EditInformation[46].Y       := 16;
  EditInformation[46].Descrip := 'Activation key for user''s default protocol';

  { Language }
  EditInformation[47].MaxLen  := 3;
  EditInformation[47].X       := 72;
  EditInformation[47].Y       := 17;
  EditInformation[47].MinValue:= 00;
  EditInformation[47].MaxValue:= 255;
  EditInformation[47].Descrip := 'User''s requested language number (0 to 255)';

  For Counter:=01 to 47 do
    begin
      Inc(EditInformation[Counter].Y, ScrnStartPos);

      ShowPaddings(Counter);
    end; { for }

  DirectScrnUpdate := true;
end; { proc. ShowEditLayOut }

{-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-}

procedure tUserEditObj.MakeEditorsField(CurField: Byte; Info: String; ChangeAttr, EditField: Boolean);
var TotalStr   : String;
    SaveDirect : Boolean;
begin
  SaveDirect := DirectScrnUpdate;
  if NOT EditField then
    DirectScrnUpdate := false;

  if (NOT ChangeAttr) OR (NOT EditField) then
   begin
     TextColor(GlobalCfg^.RaConfig^.WindFore);
     TextBackGround(GlobalCfg^.RaConfig^.WindBack);
   end
     else begin;
            TextColor(GlobalCfg^.RaConfig^.HiFore);
            TextBackGround(GlobalCfg^.RaConfig^.WindBack);
          end; { if }

  With EditInformation[CurField] do
   begin
     TotalStr := Info + Dup(mnuBlockChar, Byte(MaxLen - Length(Info)));
     if Length(TotalStr) > MaxLen then
       begin
         {$IFNDEF WIN32}
           TotalStr[0] := Chr(MaxLen);
         {$ELSE}
           SetLength(TotalStr, MaxLen);
         {$ENDIF}
       end; { if }

     FastWrite(X, Y, TextAttr, TotalStr);
     if EditField then GotoXY(X,Y);
   end; { with }

  DirectScrnUpdate := SaveDirect;
end; { proc. MakeEditors }

{-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-}

procedure tUserEditObj.EditStringField(var Edit: String; var CurField: Byte; var ch:Char; EditField: Boolean);
var OldX, OldY: Byte;
    OldAttr   : Byte;
    SaveDirect: Boolean;
begin

 With EditInformation[CurField] do
  begin
    SaveDirect := DirectScrnUpdate;
    DirectscrnUpdate := false;
    OldX := WhereX;
    OldY := WhereY;
    OldAttr := TextAttr;

    MakeEditorsField(CurField, Edit, True, EditField);

    DirectScrnUpdate := SaveDirect;
    If EditField then
      CH := SysOpLineEdit(Edit, MaxLen, [#32..#254], [#02, #01, #03, #06, #09, #13, #15, #27], mnuBlockChar, False);

    MakeEditorsField(CurField, Edit, False, EditField);

    GotoXY(OldX, OldY);
    TextAttr := OldAttr;
  end; { With }
end; { proc. EditStringField }

{-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-}

procedure tUserEditObj.GetNewPassword(var CH:Char; var CurField: Byte; EditField: Boolean);
var TempPassword : String;
    OldX, OldY: Byte;
    SaveDirect   : Boolean;
    SaveScrn     : Pointer;
begin
 FastWrite(13, 5 + ScrnStartPos, TextAttr, 'Not visible');
 If NOT EditField then Exit;

 SaveDirect := DirectScrnUpdate;

 UpdateScreenBuffer(true);
 OldX := WhereX;
 OldY := WhereY;
 GotoXY(13, 05);

 repeat
   CH := GetLocalKey;
 until CH in [#00, #13, #27, #09];

 if CH = #00 then
    CH := GetLocalKey;

 GotoXY(OldX, OldY);
 If CH=#13 then begin;
                  SaveScreen(SaveScrn);

                  if NOT GlobalCfg^.RaConfig^.SavePasswords then
                    begin
                      BoxWindow(11, 03 + ScrnStartPos, 43, 07 + ScrnStartPos, GlobalCfg^.RaConfig^.BorderFore,
                                GlobalCfg^.RaConfig^.BorderBack, 'Password');
                      DirectScrnUpdate := true;
                      FastWrite(13, 05 + ScrnStartPos, MakeAttr(GlobalCfg^.RaConfig^.HiFore, GlobalCfg^.RaConfig^.BorderBack),
                      ' Enter new password? (y,N) °');
                      UpdateScreenBuffer(DirectScrnUpdate);

                      repeat
                         GotoXY(40, 05 + ScrnStartPos);

                         CH := UpCase(GetLocalKey);
                         if CH=#13 then CH := 'N';
                      until ch in ['Y', 'N'];

                      if CH='N' then
                        begin
                          RestoreScreen(SaveScrn);
                          DirectScrnUpdate := SaveDirect;
                          CH := #13;
                          EXIT;
                        end; { if }
                    end; { if }

                  DirectScrnUpdate := false;
                  BoxWindow(11, 03 + ScrnStartpos, 43, 07 + ScrnStartpos, GlobalCfg^.RaConfig^.BorderFore,
                            GlobalCfg^.RaConfig^.BorderBack, 'Password');

                  If GlobalCfg^.RaConfig^.Savepasswords then TempPassword := lineCfg^.Exitinfo^.Userinfo.Password
                    else TempPassword := '';
                  FastWrite(13, 05 + ScrnStartPos, MakeAttr(GlobalCfg^.RaConfig^.HiFore, GlobalCfg^.RaConfig^.BorderBack),
                             'New password: ');
                  DirectScrnUpdate := true;
                  EditStringField(TempPassWord, CurField, CH, EditField);

                  If CH=#27 then CH := #00;

                  If GlobalCfg^.RaConfig^.SavePasswords then lineCfg^.Exitinfo^.Userinfo.Password := TempPassWord;
                  lineCfg^.Exitinfo^.Userinfo.PasswordCRC := RaCrc(TempPassword, true);
                  DirectScrnUpdate := SaveDirect;

                  RestoreScreen(SaveScrn);
                end; { CH=#13 }

end; { proc. GetNewPassword }

{-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-}

procedure tUserEditObj.EditIntegerField(var Edit: LongInt; var CurField: Byte; var ch:Char; EditField: Boolean);
var OldX, OldY: Byte;
    OldAttr   : Byte;
    Temp      : String;
    Correct   : Boolean;
begin
 With EditInformation[CurField] do
  begin;
    OldX := WhereX;
    OldY := WhereY;
    OldAttr := TextAttr;

    MakeEditorsField(CurField, FStr(Edit), True, EditField);

    if EditField then
     REPEAT
      GotoXY(X, Y);
      Temp := FStr(Edit);

      If EditField then
       begin;
        CH := SysOpLineEdit(Temp, MaxLen, ['0'..'9', '-'], [#02, #01, #03, #06, #09, #13, #15, #27], mnuBlockChar, False);
        Edit := Fval(Temp);
       end; { if }

      Correct := True;
      If MinValue>00 then If Edit<MinValue then Correct := False;
      If MaxValue>00 then If Edit>MaxValue then Correct := False;
      If NOT EditField then Correct := True;
      If NOT Correct then DoBeep;
    Until (Correct);

    MakeEditorsField(CurField, FStr(Edit), False, EditField);

    GotoXY(OldX, OldY);
    TextAttr := OldAttr;
  end; { with }

end; { proc. EditIntegerField }

{-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-}

procedure tUserEditObj.EditWordField(var Edit: SmallWord; var CurField: Byte; var ch:Char; EditField: Boolean);
var OldX, OldY: Byte;
    OldAttr   : Byte;
    Temp      : String;
    Correct   : Boolean;
begin
 With EditInformation[CurField] do
  begin;
    OldX := WhereX;
    OldY := WhereY;
    OldAttr := TextAttr;

    MakeEditorsField(CurField, FStr(Edit), True, EditField);

    Repeat;
      GotoXY(x,Y);
      Temp := FStr(Edit);
      If EditField then
        CH := SysOpLineEdit(Temp, MaxLen, ['0'..'9'], [#02, #01, #03, #06, #09, #15, #13, #27], mnuBlockChar, False);
      Edit := Min(Fval(Temp), 65535);

      Correct := True;
      If MinValue>00 then If Edit<MinValue then Correct := False;
      If MaxValue>00 then If Edit>MaxValue then Correct := False;
      If NOT EditField then Correct := True;
      If NOT Correct then DoBeep;
    Until (Correct);

    MakeEditorsField(CurField, FStr(Edit), False, EditField);

    GotoXY(OldX, OldY);
    TextAttr := OldAttr;
  end; { with }

end; { proc. EditWordField }

{-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-}

procedure tUserEditObj.EditByteField(var Edit: Byte; var CurField: Byte; var ch:Char; EditField: Boolean);
var OldX, OldY: Byte;
    OldAttr   : Byte;
    Temp      : String;
    Correct   : Boolean;
begin
 With EditInformation[CurField] do
  begin;
    OldX := WhereX;
    OldY := WhereY;
    OldAttr := TextAttr;

    MakeEditorsField(CurField, FStr(Edit), True, EditField);

    repeat;
      GotoXY(x,Y);
      Temp := FStr(Edit);
      If EditField then
        CH := SysOpLineEdit(Temp, MaxLen, ['0'..'9'], [#02, #01, #03, #06, #09, #13, #15, #27], mnuBlockChar, False);
      Edit := Min(Fval(Temp), 255);

      Correct := True;
      If MinValue>00 then If Edit<MinValue then Correct := False;
      If MaxValue>00 then If Edit>MaxValue then Correct := False;
      If NOT EditField then Correct := True;
      If NOT Correct then DoBeep;
    Until (Correct);

    MakeEditorsField(CurField, FStr(Edit), False, EditField);

    GotoXY(OldX, OldY);
    TextAttr := OldAttr;
  end; { with }

end; { proc. EditByteField }

{-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-}

procedure tUserEditObj.EditFlagField(var Edit: Byte; var CurField: Byte; var ch:Char; EditField: Boolean);
var OldX, OldY: Byte;
    OldAttr   : Byte;
    Temp      : String;
begin
 With EditInformation[CurField] do
  begin;
    OldX := WhereX;
    OldY := WhereY;
    OldAttr := TextAttr;

    MakeEditorsField(CurField, Byte2Flags(Edit), True, EditField);

    GotoXY(x,Y);

    if EditField then UpdateScreenBuffer(true);
    Temp := Byte2Flags(Edit);
    If EditField then
      CH := SysOpLineEdit(Temp, MaxLen, ['-', 'X', 'x'], [#02, #01, #03, #06, #09, #13, #15, #27], mnuBlockChar, False);
    Edit := Str2Flags(Temp);

    MakeEditorsField(CurField, Byte2Flags(Edit), False, EditField);

    GotoXY(OldX, OldY);
    TextAttr := OldAttr;
  end; { with }

end; { proc. EditFlagField }

{-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-}

procedure tUserEditObj.EditTimeField(var Edit: String; var CurField: Byte; var ch:Char; EditField: Boolean);
var OldX, OldY: Byte;
    OldAttr   : Byte;
    First,
    Second    : String;
    Part      : Byte;
begin

 With EditInformation[CurField] do
  begin;
    OldX := WhereX;
    OldY := WhereY;
    OldAttr := TextAttr;

    MakeEditorsField(CurField, Edit, True, EditField);

    GotoXY(x,Y);

    First := Copy(Edit, 1, 2);
    Second := Copy(Edit, 4, 2);

    Part := 01;
    if GlobalCH in [#72, #2] then Part := 02;
    If EditField then
     Repeat;
       { Down = 01 }
       { Up = 02 }

       If (Part=1) AND (CH in [#01, #06, #05]) then Part := 02;
       If (Part=1) AND (CH in [#04, #02]) then Break;
       If (Part=2) AND (CH in [#04, #02]) then Part := 01;       { Must be the last one! }

       If CH=#27 then EXIT;

       Case Part of
         01 : begin;
               GotoXY(x,y);
               CH := SysOpLineEdit(First, 2, ['0'..'9'], [#02, #01, #03, #09, #13, #15, #27], mnuBlockChar, True);
              end; { Case Part 1 }
         02 : begin;
               GotoXY(X+3, Y);
               CH := SysOpLineEdit(Second, 2, ['0'..'9'], [#02, #01, #03, #09, #13, #15, #27], mnuBlockChar, True);
              end; { Case part 2 }
       End; { Case }

      Edit := LeadingZero(FVal(First), 02)+':'+LeadingZero(FVal(Second), 02);
       MakeEditorsField(CurField, Edit, true, EditField);
     Until ((Part=2) AND (CH<>#04)) OR (CH=#13); { Last part, and NOT left pressed }

    Edit := LeadingZero(FVal(First), 02)+':'+LeadingZero(FVal(Second), 02);
    MakeEditorsField(CurField, Edit, false, EditField);
    GotoXY(OldX, OldY);
    TextAttr := OldAttr;
  end; { with }

end; { proc. EditTimeField }

{-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-}

procedure tUserEditObj.EditDateField(var Edit: String; var CurField: Byte; var ch:Char; EditField: Boolean);
var OldX, OldY: Byte;
    OldAttr   : Byte;
    First,
    Second,
    Third     : String;
    Part      : Byte;
    TempEdit  : String;
begin
 With EditInformation[CurField] do
  begin;
    OldX := WhereX;
    OldY := WhereY;
    OldAttr := TextAttr;

    if Edit='' then TempEdit := Dup(#32, 08) else TempEdit := Edit;
    MakeEditorsField(CurField, MakeLen(Copy(TempEdit, 1, 2), 2, Space, false, true) + '-' +
                               MakeLen(Copy(TempEdit, 4, 2), 2, Space, false, true) + '-' +
                               MakeLen(Copy(TempEdit, 7, 2), 2, Space, false, true), true, EditField);

    GotoXY(x,Y);

    First := Copy(Edit, 1, 2);
    Second := Copy(Edit, 4, 2);
    Third := Copy(Edit, 7, 2);

    Part := 01;
    if GlobalCH in [#72, #2] then Part := 03;

    If EditField then
     Repeat;
       { Down = 01 }
       { Up = 02 }

       If (Part=2) AND (CH in [#01, #06, #05]) then Part := 03;
       If (Part=1) AND (CH in [#01, #06, #05]) then Part := 02;
       If (Part=1) AND (CH in [#04, #02]) then Break;
       If (Part=2) AND (CH in [#04, #02]) then Part := 01;
       If (Part=3) AND (CH in [#04, #02]) then Part := 02;       { Must be the last one! }

       If CH=#27 then EXIT;

       Case Part of
         01 : begin;
               GotoXY(x,y);
               CH := SysOpLineEdit(First, 2, ['0'..'9'], [#02, #01, #03, #06, #09, #13, #15, #27], mnuBlockChar, True);
              end; { Case Part 1 }
         02 : begin;
               GotoXY(X+3, Y);
               CH := SysOpLineEdit(Second, 2, ['0'..'9'], [#02, #01, #03, #06, #09, #13, #15, #27], mnuBlockChar, True);
              end; { Case part 2 }
         03 : begin;
               GotoXY(X+6, Y);
               CH := SysOpLineEdit(Third, 2, ['0'..'9'], [#02, #01, #03, #06, #09, #13, #15, #27], mnuBlockChar, True);
              end; { Case part 2 }
       End; { Case }

      FastWrite(X, Y, MakeAttr(GlobalCfg^.RaConfig^.HiFore, GlobalCfg^.RaConfig^.WindBack),
                               MakeLen(First, 2, Space, false, true) + '-' +
                               MakeLen(Second, 2, Space, false, true) + '-' +
                               MakeLen(Third, 2, Space, false, true));

     Until ((Part=3) AND (NOT (CH in [#04,#02]))) OR (CH=#13); { Last part, and NOT left pressed }

    Edit := LeadingZero(FVal(First), 02)+'-'+LeadingZero(FVal(Second), 02)+'-'+
            LeadingZero(FVal(Third), 02);

    if (MakeLen(Trim(First), 2, Zero, True, false)='00') OR
        (MakeLen(Trim(Second), 2, Zero, True, false)='00') then Edit := '';

    if Edit='' then TempEdit := Dup(#32, 08) else TempEdit := Edit;
    MakeEditorsField(CurField, Copy(TempEdit, 1, 2) + '-' + Copy(TempEdit, 4, 2) + '-'+Copy(TempEdit, 7, 2), false, EditField);

    GotoXY(OldX, OldY);
    TextAttr := OldAttr;
  end; { with }

end; { proc. EditDateField }

{-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-}

procedure tUserEditObj.EditSex(var CurField: Byte; var CH: Char; EditField: Boolean);
var OldX, OldY: Byte;
    OldAttr   : Byte;
begin
 CursorOn;

 With EditInformation[CurField] do
  begin;
    OldX := WhereX;
    OldY := WhereY;
    OldAttr := TextAttr;

    Crt.GotoXY(X,Y);
    TextColor(GlobalCfg^.RaConfig^.HiFore);
    TextBackGround(GlobalCfg^.RaConfig^.WindBack);
  end; { with }

 With EditInformation[CurField] do
   FastWrite(X, Y, TextAttr, LangObj^.Sex2Str(lineCfg^.Exitinfo^.Userinfo.Sex, False));

 UpdateScreenBuffer(true);

 If EditField then
  repeat
    CH := GetLocalKey;


    If Ch=#32 then
     If lineCfg^.Exitinfo^.Userinfo.Sex=1 then lineCfg^.Exitinfo^.Userinfo.Sex:=2 else
      lineCfg^.Exitinfo^.Userinfo.Sex := 01;


    With EditInformation[CurField] do
     with lineCfg^ do
     FastWrite(X, Y, TextAttr, LangObj^.Sex2Str(lineCfg^.Exitinfo^.Userinfo.Sex, False));
    UpdateScreenBuffer(true);
  Until (CH<>#32);

 GotoXY(OldX, OldY);
 TextAttr := OldAttr;
 With EditInformation[CurField] do
  with LineCfg^ do
  FastWrite(X, Y, TextAttr, LangObj^.Sex2Str(lineCfg^.Exitinfo^.Userinfo.Sex, False));
end; { proc. EditSex }

{-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-}

procedure tUserEditObj.EditDateFormat(var CurField: Byte; var CH: Char; EditField: Boolean);
var OldX, OldY: Byte;
    Olda: Byte;
begin
 CursorOn;

 OldX := WhereX;
 OldY := WhereY;
 OldA := TextAttr;
 With EditInformation[CurField] do
   GotoXY(X,Y);

 TextColor(GlobalCfg^.RaConfig^.HiFore);
 TextBackGround(GlobalCfg^.RaConfig^.WindBack);
 With EditInformation[CurField] do
   FastWrite(X, Y, TextAttr, LangObj^.Byte2DateFormat(lineCfg^.Exitinfo^.Userinfo.DateFormat, False));

 UpdateScreenBuffer(true);

 If EditField then
  repeat
    CH := GetLocalkey;

    If Ch=#32 then Inc(lineCfg^.Exitinfo^.Userinfo.DateFormat);
    If lineCfg^.Exitinfo^.Userinfo.DateFormat>8 then lineCfg^.Exitinfo^.Userinfo.DateFormat:=01;

    With EditInformation[CurField] do
     FastWrite(X, Y, TextAttr, LangObj^.Byte2DateFormat(lineCfg^.Exitinfo^.Userinfo.DateFormat, False));
    UpdateScreenBuffer(true);
  Until (CH<>#32);

 GotoXY(OldX, OldY);
 TextAttr := OldA;
 With EditInformation[CurField] do
  FastWrite(X, Y, TextAttr, LangObj^.Byte2DateFormat(lineCfg^.Exitinfo^.Userinfo.DateFormat, False));
end; { proc. EditDateFormat }

{-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-}

procedure tUserEditObj.EditProtocol(var CurField: Byte; var ch:Char; EditField: Boolean);
var OldX, OldY: Byte;
    OldAttr   : Byte;
    Temp      : String;
begin
 With EditInformation[CurField] do
  begin;
    OldX := WhereX;
    OldY := WhereY;
    OldAttr := TextAttr;

    MakeEditorsField(CurField, lineCfg^.Exitinfo^.Userinfo.DefaultProtocol, true, EditField);
    GotoXY(x,Y);

    Temp := '';
    Temp[1]:= lineCfg^.Exitinfo^.Userinfo.DefaultProtocol;
    Temp[0]:= Chr(1);

    If EditField then
     CH := SysOpLineEdit(Temp, 1, [#32..#254],
                         [#02, #01, #03, #09, #13, #15, #27], mnuBlockChar, True);
    lineCfg^.Exitinfo^.Userinfo.DefaultProtocol := Temp[1];

    MakeEditorsField(CurField, lineCfg^.Exitinfo^.Userinfo.DefaultProtocol, false, EditField);
    GotoXY(OldX, OldY);
    TextAttr := OldAttr;
  end; { With }

{ RestoreScreen(8); }
end; { proc. EditProtocol }

{-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-}

procedure tUserEditObj.EditCombined(EditField: Boolean; var CH: Char);

procedure RedrawScreen;
var XCounter    : Integer;
    YCounter    : Integer;
    ShowNumber  : Integer;
    SaveDirect  : Boolean;
begin
  SaveDirect := DirectScrnUpdate;
  DirectscrnUpdate := false;

  for XCounter := 01 to 12 do
   for YCounter := 00 to 17 do
     begin
       ShowNumber := Succ(((18 * (XCounter-1)) + YCounter));
       If ShowNumber > 200 then BREAK;

       WriteAT(03 + ((XCounter-1) * 5), 4 + YCounter,
               MakeAttr(GlobalCfg^.RaConfig^.WindFore,
                        GlobalCfg^.RaConfig^.WindBack), #32 + FStr(ShowNumber));
     end; { for }

  DirectScrnUpdate := SaveDirect;
end; { proc. RedrawScreen }


var SaveScrn    : Pointer;
    EditScrn    : Pointer;
    OldRow,
    OldCol      : Longint;
    ShowNumber  : Longint;
    RowCounter  : Longint;
    ColCounter  : Longint;
    TempNr      : String;
begin
  if NOT EditField then EXIT;
  UpdateScreenBuffer(true);
  CursorOn;
  {$IFDEF USINGMGR}
    GotoXY(72, 4);
  {$ELSE}
    GotoXY(72, 3);
  {$ENDIF}
  CH := GetLocalKey;
  if CH <> #13 then EXIT;

  DirectScrnUpdate := FALSE;
  SaveScreen(SaveScrn);

  BoxWindow(02, 03, 63, 22,
            GlobalCfg^.RaConfig^.BorderFore, GlobalCfg^.RaConfig^.BorderBack, ' Edit combined area settings ');
  WriteCenter(22, mnuTitleColor, ' (ENTER) edit, (ESC) to continue ');

  RowCounter := 00;
  ColCounter := 00;
  OldCol := 00;
  OldRow := 00;

  RedrawScreen;
  DirectScrnUpdate := TRUE;

  REPEAT
    ShowNumber := Succ(Word((18 * OldCol) + OldRow));
    WriteAT(03 + (OldCol) * 5, 04 + OldRow,
            MakeAttr(GlobalCfg^.RaConfig^.WindFore, GlobalCfg^.RaConfig^.WindBack), #32 + FStr(ShowNumber));

    if ColCounter < 00 then
      begin
        ColCounter := 10;

        if RowCounter=00 then
          ColCounter := 11;

        Dec(RowCounter);
        if RowCounter < 2 then
          ColCounter := 11;
      end; { if }

    if RowCounter < 00 then
      begin
        RowCounter := 17;
        Dec(ColCounter);

        if ColCounter < 00 then
          begin
            ColCounter := 11;
            RowCounter := 01;
          end; { colcounter }
      end; { if }

    if (ColCounter >= 11) then
     if (RowCounter >= 2) then
       begin
         ColCounter := 00;
         Inc(RowCounter);

         if Ord(rdLastkey)=80 then
           RowCounter := 00;

         if RowCounter = 18 then
           begin
             RowCounter := 00;
             ColCounter := 00;
           end; { last column }
       end; { if }

    if (ColCounter >= 12) then
      begin
        ColCounter := 00;
        Inc(RowCounter);
      end; { if }

    if RowCounter >= 18 then
      begin
        Inc(ColCounter);
        RowCounter := 00;
      end; { if }

    ShowNumber := Succ(((18 * ColCounter) + RowCounter));
    WriteAT(03 + (ColCounter) * 5, 04 + RowCounter, mnuTopHiColor, #32 + FStr(ShowNumber));
    WriteAT(05, 22, MakeAttr(GlobalCfg^.RaConfig^.BorderFore, GlobalCfg^.RaConfig^.BorderBack),
                    StyleStr[mnuStyle, 2] +
                    StyleStr[mnuStyle, 5] +
                    #32 + FStr(LineCfg^.Exitinfo^.Userinfo.CombinedInfo[ShowNumber]) + #32 +
                    StyleStr[mnuStyle, 8] +
                    Dup(StyleStr[mnuStyle, 2], 5));
    UpdateScreenBuffer(true);
    CH := ReadKey;

    OldRow := RowCounter;
    OldCol := ColCounter;

    Case CH of
       #13 : begin
               SaveScreen(EditScrn);

               DirectScrnUpdate := false;
               BoxWindow(11, 04 + ScrnStartpos,
                         19, 07 + ScrnStartpos,
                         GlobalCfg^.RaConfig^.BorderFore,
                         GlobalCfg^.RaConfig^.BorderBack, #32 + FStr(ShowNumber) + #32);
               DirectScrnUpdate := true;

               TempNr := FStr(LineCfg^.Exitinfo^.Userinfo.CombinedInfo[ShowNumber]);
               {$IFDEF USINGMGR}
                 GotoXY(13, 07);
               {$ELSE}
                 GotoXY(13, 06);
               {$ENDIF}
               if SysOpLineEdit(TempNr, 5, ['0'..'9'], [#13, #27], mnuBlockChar, false) = #13 then
                 LineCfg^.Exitinfo^.Userinfo.CombinedInfo[ShowNumber] := FVal(TempNr);

               RestoreScreen(EditScrn);
             end; { toggle (space) }
    end; { case }

    if CH = #00 then
      begin
        CH := ReadKey;

        Case Ord(CH) of
          72 : Dec(RowCounter);
          80 : Inc(RowCounter);

          75 : Dec(ColCounter);
          77 : Inc(ColCounter);

          71 : begin
                 ColCounter := 00;
                 RowCounter := 00;
               end;
          79 : begin
                 RowCounter := 01;
                 ColCounter := 11;
               end;
        end; { case }
      end; { extended key pressed }
  UNTIL (CH in [#27]);

  RestoreScreen(SaveScrn);
  CH := #05;                                                       { One down }
end; { proc. EditCombined }

{-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-}

procedure tUserEditObj.EditUserFlags(EditField: Boolean; var CH: Char);
var HiLight: Byte;

Procedure EditBit(var Bits: Byte; Nr: Byte; EditField: Boolean; var CH: Char);
begin
  GotoXY(74, 2+HiLight);

  TextColor(GlobalCfg^.RaConfig^.HiFore);
  TextBackGround(GlobalCfg^.RaConfig^.WindBack);

  If ReadBit(Bits, Nr) then WriteAT(74, 2 + HiLight, TextAttr, 'Y')
    else WriteAT(74, 2 + HiLight, TextAttr, 'N');

  If EditField then
    CH := Getlocalkey;

  If EditField then
   Case UpCase(CH) of
    'Y' : SetBit(Bits, Nr);
    'N' : ClearBit(Bits, Nr);
   End; { Case }

  TextColor(GlobalCfg^.RaConfig^.WindFore);
  TextBackGround(GlobalCfg^.RaConfig^.WindBack);

  if ReadBit(Bits, Nr) then FastWrite(74, 2 + HiLight, TextAttr, 'Y')
    else FastWrite(74, 2 + HiLight, TextAttr, 'N');
end; { proc. EditBit }


procedure DoEditBits(HiLight: Byte; DoEdit: Boolean; var CH: Char);
var Msg     : String;
    SaveScrn: Pointer;
begin
  CursorOn;

  If NOT OnlineEditor then
    begin;
      Msg := '';

      Case HiLight of
        01 : Msg := 'Delete the user when the user-file is packed?';
        02 : Msg := 'Send screen clear codes?';
        03 : Msg := 'Pause at the end of each screen page?';
        04 : Msg := 'Use ANSI graphics?';
        05 : Msg := 'Use AVATAR graphics?';
        06 : Msg := 'If this field is set to "Y", the user will never be deleted';
        07 : Msg := 'Ignore file ratio and downloads hour restrictions?';
        08 : Msg := 'Use the ANSI full-screen message editor?';
        09 : Msg := 'Set to "Y" If the user doesn''t want to receive on-line messages';
        10 : Msg := '"Y" to use hot-keys, "N" For command stacking';
        11 : Msg := 'Use the fullscreen message viewer?';
        12 : Msg := 'Hidden from the user list?';
        13 : Msg := 'Override paging hours?';
        14 : Msg := 'If set to "Y", only local and netmail messages will shown in a mailbox scan';
        15 : Msg := 'Flag this user as a GUEST? (No password required, time used always set to 0)';
        16 : Msg := 'Allow post-billing (negative credit balance)';
        17 : Msg := 'Scan only combined selected areas in mailbox check';
      End; { case }

    {$ifdef UsingMgr}
      ScrnU.PartClear(01, 01, 80, 01, MakeAttr(GlobalCfg^.RaConfig^.HiFore, GlobalCfg^.RaConfig^.WindBack), ' ');
      ScrnU.WriteCenter(01, MakeAttr(GlobalCfg^.RaConfig^.HiFore, GlobalCfg^.RaConfig^.WindBack),
                             Msg);
    {$endif}
    end; { not online-editor }

    Case HiLight of
      { Deleted }        01 : begin
                                  EditBit(LineCfg^.Exitinfo^.Userinfo.Attribute , 00, DoEdit, CH);

                                  if ReadBit(LineCfg^.Exitinfo^.Userinfo.Attribute, 0) then
                                   if ReadBit(LineCfg^.Exitinfo^.Userinfo.Attribute, 4) then
                                     With GlobalCfg^.RaConfig^ do
                                      begin
                                        SaveScreen(SaveScrn);

                                        BoxWindow(20, 12, 60, 15, WindFore, WindBack, ' WARNING ');
                                        WriteCenter(13, mnuNormColor, ' NOKILL flag is active.');
                                        WriteCenter(14, mnuNormColor, ' Deleted flag will be ignored!');

                                        {!!!!GetInput;}
                                        RestoreScreen(SaveScrn);
                                      end;
                                end; { deleted }
      { ClrScr }         02 : EditBit(LineCfg^.Exitinfo^.Userinfo.Attribute , 01, DoEdit, CH);
      { Page pausing }   03 : EditBit(LineCfg^.Exitinfo^.Userinfo.Attribute , 02, DoEdit, CH);
      { ANSI Graphcs }   04 : EditBit(LineCfg^.Exitinfo^.Userinfo.Attribute , 03, DoEdit, CH);
      { AVT graphcs }    05 : EditBit(LineCfg^.Exitinfo^.Userinfo.Attribute2, 01, DoEdit, CH);
      { NoKill }         06 : EditBit(LineCfg^.Exitinfo^.Userinfo.Attribute , 04, DoEdit, CH);
      { XFer Priority }  07 : EditBit(LineCfg^.Exitinfo^.Userinfo.Attribute , 05, DoEdit, CH);
      { Fullscrn Edit }  08 : EditBit(LineCfg^.Exitinfo^.Userinfo.Attribute , 06, DoEdit, CH);
      { Quiet Mode }     09 : EditBit(LineCfg^.Exitinfo^.Userinfo.Attribute , 07, DoEdit, CH);
      { Hot-Keys }       10 : EditBit(LineCfg^.Exitinfo^.Userinfo.Attribute2, 00, DoEdit, CH);
      { FullScrn vwer }  11 : EditBit(LineCfg^.Exitinfo^.Userinfo.Attribute2, 02, DoEdit, CH);
      { Hidden }         12 : EditBit(LineCfg^.Exitinfo^.Userinfo.Attribute2, 03, DoEdit, CH);
      { Page priority }  13 : EditBit(LineCfg^.Exitinfo^.Userinfo.Attribute2, 04, DoEdit, CH);
      { NoNew Echomail } 14 : EditBit(LineCfg^.Exitinfo^.Userinfo.Attribute2, 05, DoEdit, CH);
      { Guest }          15 : EditBit(LineCfg^.Exitinfo^.Userinfo.Attribute2, 06, DoEdit, CH);
      { Post bill }      16 : EditBit(LineCfg^.Exitinfo^.Userinfo.Attribute2, 07, DoEdit, CH);
      { Selected mail }  17 : EditBit(LineCfg^.Exitinfo^.Userinfo.Attribute3, 00, DoEdit, CH);
    End; { Case }

  CursorOff;
end; { proc. DoEditBits }

var SaveDirect: Boolean;
    SaveScrn  : Pointer;
begin
  If NOT EditField then Exit;
  UpdateScreenBuffer(true);
  CursorOn;
 {$IFDEF USINGMGR}
   GotoXY(72, 3);
 {$ELSE}
   GotoXY(72, 2);
 {$ENDIF}
   CH := GetLocalKey;
  If CH<>#13 then Exit;

  SaveScreen(SaveScrn);

  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := false;
  BoxWindow(51, 02, 76, 20, GlobalCfg^.RaConfig^.BorderFore, GlobalCfg^.RaConfig^.BorderBack, 'Edit flags');

  EditorWrite(53, 72, 03, TextAttr, 'Deleted :');
  EditorWrite(53, 72, 04, TextAttr, 'Clear screen :');
  EditorWrite(53, 72, 05, TextAttr, 'Page pausing :');
  EditorWrite(53, 72, 06, TextAttr, 'ANSI graphics :');
  EditorWrite(53, 72, 07, TextAttr, 'AVATAR graphics :');
  EditorWrite(53, 72, 08, TextAttr, 'No-kill :');
  EditorWrite(53, 72, 09, TextAttr, 'Xfer priority :');
  EditorWrite(53, 72, 10, TextAttr, 'Full screen editor :');
  EditorWrite(53, 72, 11, TextAttr, 'Quiet mode :');
  EditorWrite(53, 72, 12, TextAttr, 'Hot-keys :');
  EditorWrite(53, 72, 13, TextAttr, 'Full screen viewer:');
  EditorWrite(53, 72, 14, TextAttr, 'Hidden :');
  EditorWrite(53, 72, 15, TextAttr, 'Page priority :');
  EditorWrite(53, 72, 16, TextAttr, 'No new echomail :');
  EditorWrite(53, 72, 17, TextAttr, 'Guest :');
  EditorWrite(53, 72, 18, TextAttr, 'Post bill :');
  EditorWrite(53, 72, 19, TextAttr, 'Selected mail only :');

  For HiLight:=01 to 17 do
    begin
      if HiLight = 17 then DirectScrnUpdate := true;
      DoEditBits(HiLight, False, CH);
    end;
  HiLight := 01;
  UpdateScreenBuffer(true);

  repeat
    GotoXY(74, 3);
    If HiLight>17 then HiLight := 01;
    If HiLight<01 then HiLight := 17;

    DoEditBits(HiLight, True, CH);

    Case UpCase(CH) of
       'Y'        : Inc(HiLight);
       'N'        : Inc(HiLight);
       #13        : Inc(HiLight);
    End; { Case }

    If CH=#00 then
     begin
       CH := GetLocalKey;

        Case Ord(CH) of
           72  : Dec(HiLight);
           80  : Inc(HiLight);
           71  : HiLight := 01;
           79 : HiLight := 17;
        End; { Case }
     End; { If CH=#00 }

  Until CH=#27;

  RestoreScreen(SaveScrn);
  DirectScrnUpdate := SaveDirect;
  CH := #05;                                                       { One down }
end; { proc. EditUserFlags }

{-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-}

procedure tUserEditObj.ProcessCurField(var CurField: Byte; var CH: Char; EditField: Boolean);
var TempW     : Longint;
    SaveDirect: Boolean;
begin
  CH := #00;
  SaveDirect := DirectScrnUpdate;
  DirectScrnUpdate := False;

  If EditField then
   If NOT OnlineEditor then
     begin
       {$IFDEF USINGMGR}
          ScrnU.PartClear(01, 01, 80, 01, MakeAttr(GlobalCfg^.RaConfig^.HiFore, GlobalCfg^.RaConfig^.WindBack), ' ');
          ScrnU.WriteCenter(01, MakeAttr(GlobalCfg^.RaConfig^.HiFore, GlobalCfg^.RaConfig^.WindBack),
                             EditInformation[CurField].Descrip);
       {$ENDIF}
     end; { if }

  DirectScrnUpdate := SaveDirect;

  Case CurField of
{ Name }      01 : begin
                       REPEAT
                         EditStringField(LineCfg^.Exitinfo^.Userinfo.Name, CurField, CH, EditField);
                         if EditField then
                          if Trim(LineCfg^.Exitinfo^.Userinfo.Name)='' then
                            DoBeep;
                       UNTIL (Trim(LineCfg^.Exitinfo^.Userinfo.Name)<>'') OR (NOT EditField);
                     end; { name }
{ Handle }    02 : EditStringField(LineCfg^.Exitinfo^.Userinfo.Handle, CurField, CH, EditField);
{ Location }  03 : EditStringField(LineCfg^.Exitinfo^.Userinfo.Location, CurField, CH, EditField);
{ PassWord }  04 : GetNewPassword(CH, CurField, EditField);
{ Security }  05 : EditWordField(LineCfg^.Exitinfo^.Userinfo.Security, CurField, CH, EditField);
{ Voice }     06 : EditStringField(LineCfg^.Exitinfo^.Userinfo.VoicePhone, CurField, CH, EditField);
{ Data }      07 : EditStringField(LineCfg^.Exitinfo^.Userinfo.DataPhone, CurField, CH, EditField);
{ Flags A }   08 : EditFlagField(LineCfg^.Exitinfo^.Userinfo.Flags[1], CurField, CH, EditField);
{ Flags B }   09 : EditFlagField(LineCfg^.Exitinfo^.Userinfo.Flags[2], CurField, CH, EditField);
{ Flags C }   10 : EditFlagField(LineCfg^.Exitinfo^.Userinfo.Flags[3], CurField, CH, EditField);
{ Flags D }   11 : EditFlagField(LineCfg^.Exitinfo^.Userinfo.Flags[4], CurField, CH, EditField);
{ Credit }    12 : EditIntegerField(LineCfg^.Exitinfo^.Userinfo.Credit, CurField, CH, EditField);
{ Pending }   13 : EditIntegerField(LineCfg^.Exitinfo^.Userinfo.Pending, CurField, CH, EditField);
{ Group }     14 : EditWordField(LineCfg^.Exitinfo^.Userinfo.Group, CurField, CH, EditField);
{ Sex }       15 : EditSex(CurField, CH, EditField);
{ Forward }   16 : EditStringField(LineCfg^.Exitinfo^.Userinfo.ForwardTo, CurField, CH, EditField);
{ Address 1 } 17 : EditStringField(LineCfg^.Exitinfo^.Userinfo.Address1, CurField, CH, EditField);
{ Address 2 } 18 : EditStringField(LineCfg^.Exitinfo^.Userinfo.Address2, CurField, CH, EditField);
{ Address 3 } 19 : EditStringField(LineCfg^.Exitinfo^.Userinfo.Address3, CurField, CH, EditField);
{ Organisat } 20 : EditStringField(LineCfg^.Exitinfo^.Userinfo.Organisation, CurField, CH, EditField);
{ Comment }   21 : EditStringField(LineCfg^.Exitinfo^.Userinfo.Comment, CurField, CH, EditField);
{ Last Time } 22 : EditTimeField(LineCfg^.Exitinfo^.Userinfo.LastTime, CurField, CH, EditField);
{ Last Date } 23 : EditDateField(LineCfg^.Exitinfo^.Userinfo.LastDate, CurField, CH, EditField);
{ 1st Date }  24 : EditDateField(LineCfg^.Exitinfo^.Userinfo.FirstDate, CurField, CH, EditField);
{ Sub Date }  25 : EditDateField(LineCfg^.Exitinfo^.Userinfo.SubDate, CurField, CH, EditField);
{ BirthDate}  26 : EditDateField(LineCfg^.Exitinfo^.Userinfo.BirthDate, CurField, CH, EditField);
{ Time Left}  27 : If OnlineEditor then EditWordField(LineCfg^.Exitinfo^.TimeLimit, CurField, CH, EditField)
                    else begin
                           TempW := Integer(LineCfg^.Exitinfo^.Userinfo.Elapsed);
                           EditIntegerField(TempW, CurField, Ch, EditField);
                           LineCfg^.Exitinfo^.UserInfo.Elapsed := Integer(TempW);
                         end; { if }
{ Scrn Lngth} 28 : EditWordField(LineCfg^.Exitinfo^.Userinfo.ScreenLength, CurField, CH, EditField);
{ Lst.Pwd }   29 : EditByteField(LineCfg^.Exitinfo^.Userinfo.LastPwdChange, CurField, CH, EditField);
{ Lst.Dob }   30 : EditByteField(LineCfg^.Exitinfo^.Userinfo.LastDobCheck, CurField, CH, EditField);
{ DateFmt }   31 : EditDateFormat(CurField, CH, EditField);
{ Flags }     32 : EditUserFlags(EditField, CH);
{ Combined }  33 : EditCombined(EditField, CH);
{ Uploads }   34 : EditIntegerField(LineCfg^.Exitinfo^.Userinfo.Uploads, CurField, CH, EditField);
{ Downloads } 35 : EditIntegerField(LineCfg^.Exitinfo^.Userinfo.Downloads, CurField, CH, EditField);
{ UploadsK }  36 : EditIntegerField(LineCfg^.Exitinfo^.Userinfo.UploadsK, CurField, CH, EditField);
{ DownloadK } 37 : EditIntegerField(LineCfg^.Exitinfo^.Userinfo.DownloadsK, CurField, CH, EditField);
{ TodayK }    38 : EditIntegerField(LineCfg^.Exitinfo^.Userinfo.TodayK, CurField, CH, EditField);
{ Msgs Psted} 39 : EditWordField(LineCfg^.Exitinfo^.Userinfo.MsgsPosted, Curfield, CH, EditField);
{ HighMsgRd}  40 : EditIntegerField(LineCfg^.Exitinfo^.Userinfo.LastRead, CurField, CH, EditField);
{ No.Calls }  41 : EditIntegerField(LineCfg^.Exitinfo^.Userinfo.NoCalls, CurField, CH, EditField);
{ Msg Area }  42 : EditWordField(LineCfg^.Exitinfo^.Userinfo.MsgArea, CurField, CH, EditField);
{ FileArea }  43 : EditWordField(LineCfg^.Exitinfo^.Userinfo.FileArea, CurField, CH, EditField);
{ FileGrp  }  44 : EditWordField(LineCfg^.Exitinfo^.Userinfo.FileGroup, CurField, CH, EditField);
{ MsgGrp }    45 : EditWordField(LineCfg^.Exitinfo^.Userinfo.MsgGroup, CurField, CH, EditField);
{ Protocol }  46 : EditProtocol(CurField, CH, EditField);
{ Language }  47 : EditByteField(LineCfg^.Exitinfo^.Userinfo.Language, CurField, CH, EditField);
  End; { Case }

end; { proc. ProcessCurField }

{-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-}

procedure tUserEditObj.UserEdit(RebuildComplete: Boolean);
var CH        : Char;
    CurField  : Byte;
    OldUserInf: UsersRecord;
    DoAbort   : Boolean;
    SaveA,
    SaveX,
    SaveY     : Byte;
    SAveScrn  : Pointer;
begin
 SaveX := WhereX;
 SaveY := WhereY;
 SaveA := TextAttr;

 {$IFDEF WITH_DEBUG}
   DebugObj.DebugLog(logSecurity, 'Entering useredit');
 {$ENDIF}
  if RunningBBS then
   if NOT LineCfg^.LoggedOn then EXIT;

  ExtraAbortSet := [];
  If NOT OnlineEditor then
    ExtraAbortSet := [#81, #73];

  CursorOn;

  If OnlineEditor then
     SaveScreen(SaveScrn);

  {$IFNDEF WIN32}
    Crt.Window(1, 1, 80, mnuScrnLen);
  {$ENDIF}
  LineCfg^.LocalOnly := True;                    { Disable fossil output }

  TextAttr := MakeAttr(GlobalCfg^.RaConfig^.BorderFore, GlobalCfg^.RaConfig^.BorderBack);

  {$IFDEF USINGMGR}
    if RebuildComplete then Partclear(01, 01, 80, mnuScrnLen, White, #32);
  {$ELSE}
    if RebuildCOmplete then Crt.Clrscr;
  {$ENDIF}

  If OnlineEditor then
    BoxWindow(01, 01, 80, mnuScrnLen, GlobalCfg^.RaConfig^.BorderFore, GlobalCfg^.RaConfig^.BorderBack, 'Edit user')
     else begin
             if RebuildComplete then
              begin
                PartClear(01, 02, 80, 02, mnuDisabled, mnuDblWide);
                PartClear(01, mnuScrnLen - 01, 80, mnuScrnLen - 01, mnuDisabled, mnuSngWide);
              end; { if RebuildComplete }

             ScrnStartPos := 01;
          end; { if }

  TextAttr := MakeAttr(GlobalCfg^.RaConfig^.WindFore, GlobalCfg^.RaConfig^.WindBack);
  {$IFNDEF WIN32}
    Crt.TextColor(GlobalCfg^.RaConfig^.WindFore);
    Crt.TextBackGround(GlobalCfg^.RaConfig^.WindBack);
  {$ELSE}
    {$IFNDEF VirtualPascal}
    {$ELSE}
      Crt.TextColor(GlobalCfg^.RaConfig^.WindFore);
      Crt.TextBackGround(GlobalCfg^.RaConfig^.WindBack);
    {$ENDIF}
  {$ENDIF}
  ShowEditLayout(RebuildComplete);

  FillChar(OldUserInf, SizeOf(OldUserInf), #00);
  OldUserInf := LineCfg^.Exitinfo^.UserInfo;

  CurField := 01;

  repeat
    ProcessCurField(CurField, CH, True);

    If CH=#00 then
       {$IFNDEF WIN32}
         CH := GetLocalKey;
       {$ELSE}
         {$IFNDEF VIRTUALPASCAL}
         {$ELSE}
           CH := GetLocalKey;
         {$ENDIF}
       {$ENDIF}

    GlobalCH := CH;

    Case CH of
      { Enter }     #13 : Inc(CurField);
      { Up }        #02 : Dec(CurField);
      { Down }      #01 : Inc(CurField);
      { Left }      #04 : Dec(CurField);
      { Right }     #05 : Inc(CurField);
      { WordWrap }  #06 : Inc(CurField);
      { Tab }       #09 : Inc(CurField);
      { Sh-Tab }    #15 : Dec(CurField);
(*---
      { Left }      #75 : Dec(CurField);
      { Right }     #77 : Inc(CurField);
---*)
      { Down }      #80 : Inc(CurField);
      { Up }        #72 : Dec(CurField);
    End; { Case }

   If CurField<01 then CurField := 47;
   If CurField>47 then Curfield := 01;

   DoAbort := (CH in ExtraAbortSet) OR (CH = #27);
  until (DoAbort);

  AbortKeypress := CH;

  If OnlineEditor then
   begin
    {$IFDEF WITH_FULL}
    Limit_U.SetTime(LineCfg^.Exitinfo^.TimeLimit);                          { Set correct time }

    LineCfg^.AnsiOn := ReadBit(LineCfg^.Exitinfo^.UserInfo.Attribute, 3);
    LineCfg^.AvatarOn := ReadBit(LineCfg^.Exitinfo^.UserInfo.Attribute2, 1);
    LineCfg^.GuestUser := ReadBit(LineCfg^.Exitinfo^.Userinfo.Attribute2, 6);
    if LineCfg^.RipOn then LineCfg^.AnsiOn := True;
    LineCfg^.UserAgeSave := 00;          { Forces EleBBS to recalculate the user's age }
    GetUserAge(LineCfg^.Exitinfo^.Userinfo.Birthdate, true);
    GetLevelLimitsInfo(LineCfg^.Exitinfo^.Userinfo.Security, false, false);

    {$ifndef UsingMgr}
      ReadLanguageRA(Byte(LineCfg^.Exitinfo^.Userinfo.Language - 01));
      if NOT LangObj^.ReadRalFile(LineCfg^.Language^.DefName) then
          LoadDefaultLanguage;
    {$endif}

    RestoreScreen(SaveScrn);
    {$ENDIF}
   end; { if }

  {$Ifdef With_Debug}
    DebugObj.DebugLog(logSecurity, 'Checking for difff...');
  {$endif}
  If RecordsDifferent(LineCfg^.Exitinfo^.Userinfo, OldUserInf, SizeOf(UsersRecord)) then
    begin;
      If Onlineeditor then
         SaveScreen(SaveScrn);

      If OnlineEditor then
       begin;
         BoxWindow(23, 10, 50, 14, Black, Lightgray, '');
         {$IFNDEF WIN32}
           Crt.TextColor(Black);
           Crt.TextBackGround(Lightgray);
         {$ELSE}
           {$IFNDEF VirtualPascal}
           {$ELSE}
             Crt.TextColor(Black);
             Crt.TextBackGround(LightGray);
           {$ENDIF}
         {$ENDIF}

         GotoXY(38, 12);
         DirectscrnUpdate := true;
         FastWrite(26, 12, TextAttr, 'Save changes (Y/n) ? °');

         GotoXY(23 + 24, 12);

         repeat
           CH := UpCase(GetLocalKey);

           If CH=#13 then CH := 'Y';
         Until UpCase(CH) in ['Y', 'N'];
       end;

      DirectScrnUpdate := true;
{$ifdef Usingmgr}
      If NOT OnlineEditor then
        If DoSaveChanges('Save changes (Y/n) ? °', true, false) then CH := 'Y'
         else CH := 'N';
{$endif}

      If CH='N' then LineCfg^.Exitinfo^.Userinfo := OldUserInf;

      If OnlineEditor then
         RestoreScreen(SaveScrn);
    end; { OldUserInf }

  if OnlineEditor then
    begin
      {$IFDEF WITH_FULL}
        StatusDisplay(99, false);
        MultiLnObj^.WriteUserOn('', -1);
     {$ENDIF}
    end; { if }

  LineCfg^.LocalOnly := False;                           { Enable fossil output }

  If NOT OnlineEditor then
    begin
      {$ifdef UsingMgr}
      CursorOff;
      {$endif}
    end { if }
     else CursorOn;

  if RebuildComplete then
    begin
      Crt.GotoXY(SaveX, SaveY);
      TextAttr := SaveA;
    end; { if }
end; { proc. UserEdit }


end. { unit UserEdit. }
