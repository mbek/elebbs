unit GenDos;
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
{$I apdefine.inc}
{$I COMPILER.INC}
(*
**
** Generic DOS routines
**
** Created : 29-Dec-1998
** Last update : 23-Jan-1998
**
**
*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF DELPHI}
  uses WinDows, SysUtils;

    type PathStr    = String[255];
         DirStr     = String[255];
         NameStr    = String[255];
         ExtStr     = String[255];

         SearchRec  = TSearchRec;
         FileRec    = TFileRec;
         TextRec    = TTextRec;

         DateTime = record
           Year, Month, Day, Hour, Min, Sec: Word;
         end;

    Const AnyFile    = faAnyFile;
          Hidden     = faHidden;
          ReadOnly   = faReadOnly;
          Archive    = faArchive;
          Directory  = faDirectory;
          VolumeID   = faVolumeID;

          DosError   : Word = 00;

          CheckBreak : Boolean = true;


  procedure GetFtime(var F; var Time: Longint);
  procedure SetFTime(Var F; Time: Longint);
  procedure FSplit(const Path: PathStr; var Dir: DirStr; var Name: NameStr; var Ext: ExtStr);
  procedure PackTime(T: DateTime; var P: LongInt);
  procedure UnpackTime(P: Longint; var T: DateTime);

  procedure FindFirst(const Path: PathStr; Attr: Word; var F: SearchRec);
  procedure FindNext(var F: SearchRec);

  function FExpand(Name: String): String;
  function GetEnv(VarName: String): String;

  procedure GetDate(var Year,Month,Day,Wday: Word);
  procedure GetTime(var Hour,Min,Sec,MSec: Word);

  function EnvCount: Integer;
  function EnvStr(EnvNr: Integer): String;
{$ENDIF (DELPHI) }

{$IFDEF MSDOS}
  uses Dos;

  procedure FindClose(var Dirinfo: SearchRec);
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 IMPLEMENTATION
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF DELPHI}
  procedure GetFtime(var F; var Time: Longint);
  begin
     Time := FileGetDate(TFileRec(F).Handle);
  end; { proc. GextFtime }
  (*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+-*)

  procedure SetFTime(Var F; Time: Longint);
  begin
     FileSetDate(TFileRec(F).Handle, Time);
  end; { proc. SetFtime }
  (*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+-*)

  procedure FSplit(const Path: PathStr; var Dir: DirStr; var Name: NameStr; var Ext: ExtStr);
  var
    I,NamePos,ExtPos: Integer;
  begin
    NamePos := 0;
    ExtPos  := 256;
    for I := 1 to Length(Path) do
    case Path[I] of
      ':','\':
        begin
          NamePos := I;
          ExtPos  := 256;
        end;
      '.': ExtPos := I;
    end;
    Dir  := Copy(Path, 1, NamePos);
    Name := Copy(Path, NamePos+1, ExtPos-NamePos-1);
    Ext  := Copy(Path, ExtPos, 255);
  end; { proc. FSplit }
  (*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+-*)

  procedure PackTime(T: DateTime; var P: LongInt);
  Type  LongRec = record
          Lo, Hi: Word;
        end; { Longrec }
  begin
    if (T.Year < 1980) or (T.Year > 2099) then P := 0 else
     With T do
      begin
        LongRec(P).Lo := (Sec shr 1) or (Min shl 5) or (Hour shl 11);
        LongRec(P).Hi := Day or (Month shl 5) or ((Year - 1980) shl 9);
      end;
  end; { proc. PackTime }
  (*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+-*)

  procedure UnpackTime(P: Longint; var T: DateTime);
  Type WordRec =  Array[1..2] of Word;
  var
    TempRec: WordRec ABSOLUTE P;
  begin
    FillChar(T, SizeOf(DateTime), 00);

    T.Year := (TempRec[2] SHR 9) + 1980;
    T.Month := (TempRec[2] SHR 5) AND 15;
    T.Day := (TempRec[2] AND 31);

    T.Hour := (TempRec[1] SHR 11);
    T.Min  := (TempRec[1] SHR 5) AND 63;
    T.Sec  := (TempRec[1] AND 31) SHL 1;
  end; { proc. UnpackTime }
  (*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+-*)

  procedure FindFirst(const Path: PathStr; Attr: Word; var F: SearchRec);
  begin
    DosError := Sysutils.FindFirst(Path, Attr, F);
  end; { proc. FindFirst }

  (*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+-*)

  procedure FindNext(var F: SearchRec);
  begin
    DosError := Sysutils.FindNext(F);
  end; { proc. FindNext }
  (*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+-*)

  function FExpand(Name: String): String;
  begin
    Result := ExpandFileName(Name);
  end; { func. FExpand }
  (*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+-*)

  function GetEnv(VarName: String): String;
  var
    P: PChar;
    SaveP: PChar;
    L: Word;
    EnvVarBuf: array [0..255] of Char;
  begin
    StrPCopy(EnvVarBuf, Varname);
    L := Length(Varname);
    P := GetEnvironmentStrings;
    SaveP := P;

    while P^ <> #0 do
    begin
      if (StrLIComp(P, EnvVarBuf, L) = 0) and (P[L] = '=') then
      begin
        Result := StrPas(P + L + 1);
        FreeEnvironmentStrings(SaveP);
        Exit;
      end;
      Inc(P, StrLen(P) + 1);
    end;

    FreeEnvironmentStrings(SaveP);
    Result := '';
  end;
  (*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+-*)

  function EnvCount: Integer;
  var
    P: PChar;
    SaveP: PChar;
  begin
    P := GetEnvironmentStrings;
    SaveP := P;
    Result := 0;

    while P^ <> #0 do
    begin
      Inc(P, StrLen(P) + 1);
      Result := Result + 1;
    end;

    FreeEnvironmentStrings(SaveP);
  end;
  (*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+-*)

  function EnvStr(EnvNr: Integer): String;
  var
    P: PChar;
    SaveP: PChar;
    Cntr: Integer;
  begin
    P := GetEnvironmentStrings;
    SaveP := P;
    Cntr := 0;

    while P^ <> #0 do
    begin
      if Cntr = EnvNr then
      begin
        Result := StrPas(P);
        FreeEnvironmentStrings(SaveP);
        Exit;
      end;
      Inc(P, StrLen(P) + 1);
      Cntr := Cntr + 1;
    end;

    FreeEnvironmentStrings(SaveP);
    Result := '';
  end;
  (*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+-*)
  procedure GetDate(var Year,Month,Day,Wday: Word);
  var TD: TDatetime;
  begin
    TD := SysUtils.Date;

    DeCodeDate(TD, Year, Month, Day);
    WDay := SysUtils.DayOfWeek(TD) - 01;
  end; { proc. GetDate }
  (*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+-*)

  procedure GetTime(var Hour,Min,Sec,MSec: Word);
  var TD: TDatetime;
  begin
    TD := Now;
    DecodeTime(TD, Hour, Min, Sec, MSec);
  end; { proc. GetTime }
  (*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+-*)

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$ENDIF (DELPHI) }

{$IFDEF MSDOS}
  procedure FindClose(var Dirinfo: SearchRec);
  begin
  end; { proc. Findclose }
  (*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*-+-*)
{$ENDIF}


end. { unit GENDOS }
