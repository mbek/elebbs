Program RalGen;
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
** RalGen, RalGen, creates default ral-array for ElCONFIG
**
** Copyright (c) 1996 by Maarten Bekers
**
** Created : 27-Dec-1996
** Last update : 19-Apr-1997
**
** note: This program uses a "not terrific" way to detect wheter a prompt has a
**       default key.
**
*)
Uses Crt,
     Dos,
     Global,
     Main,
     Support,
     Ral,
     StUtils,
     Use32,
     MemMan;

Function Norm2Pas(S: String): String;
var TempStr: String;
    Counter: Byte;
begin
  TempStr := '';

  For Counter := 01 to Length(S) do
   begin;
     TempStr := TempStr + S[Counter];
     If S[Counter] = '''' then
      begin
        TempStr := TempStr + '''';
      end;
   end; { for }

  Norm2Pas := TempStr;
end; { func. Norm2Pas }

var Counter      : Integer;
    Text_F       : Text;
    CurProcCount : Byte;
    TempStr      : String;
    TempKeys     : String;
begin
  Writeln('(c)1996,97 by Maarten Bekers, Maarten Bekers.');
  Writeln('All rights reserved. Distribution and use prohibited.');
  WriteLn;
  
  New(langObj, Init);
  ReadRalFile('C:\RA\ENGLISH.RAL');

  Assign(Text_F, 'RALDEF.SUP');
  ReWrite(Text_F);

  Write('Writing ',langObj^.ralentries, ' entries...');

  WriteLn(Text_F, '(*');
  WriteLn(Text_F, '**');
  WriteLn(Text_F, '** RALDEF.SUP, Default RAL values for ElCONFIG');
  WriteLn(Text_F, '**');
  WriteLn(Text_F, '** Copyright (c) 1996 by Maarten Bekers');
  WriteLn(Text_F, '**');
  WriteLn(Text_F, '** Created : 19-Apr-1997');
  WriteLn(Text_F, '** Last update : 19-Apr-1997');
  WriteLn(Text_F, '**');
  WriteLn(Text_F,' ** note: Split up in 6 parts, because TP''s limit.');
  WriteLn(Text_F, '**');
  WriteLn(Text_F, '*)');
  WriteLn(Text_F);
  Writeln(Text_F, '(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)');
  WriteLn(Text_F);
  WriteLn(Text_F);
  WriteLn(Text_F);

  ralCodes := False;                                 { Enable RAL color codes }
  CurProcCount := 00;

  For Counter:=00 to langObj^.ralEntries do
    begin;
      If (Counter=0) OR (Counter=150) OR (Counter=300) OR (Counter=450) OR
        (Counter=600) OR (Counter=750) OR (Counter=900) then
          begin;

            If Counter <> 00 then
             begin;
               WriteLn(Text_F, 'end;');
               WriteLn(Text_F);
             end; { if }

            Writeln(Text_F, '(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)');
            WriteLn(Text_F);
            WriteLn(Text_F,'procedure LoadralDefaults_', Counter div 200, '(var LangArray: DefaultLangArrayRec);');
            WriteLn(Text_F,'begin;');
            Inc(CurProcCount);
          end; { if }

{       WriteLn(Text_F,'   LangArray[', Counter:3,'].TextStr    := StrNew(TempRalStr);'); }


      TempStr := Norm2Pas(LangObj^.RalGet(Counter));
      TempKeys := '';

      If TempStr[Length(TempStr)]=#255 then
          begin
             Dec(TempStr[0]);
             TempKeys := FirstWord(TempStr);
          end;

      If Counter=00 then
        TempStr := LangObj^.ralGetTitleInfo;

      If (LastChar(LangObj^.ralGet(Counter)) in ['Y', 'N']) AND
          (Counter <> 477) AND (Counter <> 478) AND (Counter <> 480) then
        begin
          Dec(TempStr[0]);

          WriteLn(Text_F,'   LangArray[', Counter:3,'].TextStr    := StringNew(''', TempStr, ''');');
          Writeln(Text_F,'   LangArray[', Counter:3,'].HasDefault := True;');
          WriteLn(Text_F,'   LangArray[', Counter:3,'].DefaultKey := ''', LastChar(LangObj^.ralGet(Counter)),''';');
          WriteLn(Text_F,'   LangArray[', Counter:3,'].Keys       := ''', TempKeys, ''';');
          WriteLn(Text_F,'   LangArray[', Counter:3,'].TxtColor   := ', LangObj^.ralGetColor(Counter), ';');
        end
      else
        begin;
           WriteLn(Text_F,'   LangArray[', Counter:3,'].TextStr    := StringNew(''', TempStr, ''');');
           Writeln(Text_F,'   LangArray[', Counter:3,'].HasDefault := False;');
           WriteLn(Text_F,'   LangArray[', Counter:3,'].DefaultKey := #00;');
           WriteLn(Text_F,'   LangArray[', Counter:3,'].Keys       := ''', TempKeys, ''';');
           WriteLn(Text_F,'   LangArray[', Counter:3,'].TxtColor   := ', LangObj^.ralGetColor(Counter), ';');
        end;
      WriteLn(Text_F);
    end; { for }

  WriteLn(Text_F,'end;');

  Writeln(Text_F, '(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)');
  WriteLn(Text_F);
  WriteLn(Text_F,'procedure LoadralDefaults(var LangArray: DefaultLangArrayRec);');
  WriteLn(Text_F,'var Counter: Word;');
  WriteLn(Text_F,'begin;');
  Writeln(Text_F,' FillChar(LangArray, SizeOf(DefaultLangArrayRec), #00);');
  WriteLn(Text_F);
  WriteLn(Text_F,' For Counter := 00 to ',defRalSize,' do ');
  WriteLn(Text_F,'   LangArray[Counter].TextStr := nil;');
  WriteLn(Text_F);

  For Counter := 01 to CurProcCount do
    Writeln(Text_F,' LoadRalDefaults_', Counter - 01, '(LangArray);');

  WriteLn(Text_F,'end;');
  WriteLn(Text_F);

  Writeln;
  Writeln('Done..');

  Close(Text_F);
end. { read-ral }
