Unit MKFile;
  {$I compiler.inc}
  {$I MKB.Def}

{$X+,V-}

{$UNDEF WITH_FIND}                          { Define if the FIndObj is needed }
{$UNDEF WITH_TFILE}                    { Define if the TFile-Object is needed }

{$IFDEF WIN32}
 {$UNDEF BASMINT}                          { Don't use BASM for file-handling }
 {$H-}                     { Usage of Short strings (<256), for compatibility }
 {$DEFINE Win32}                    { Delphi >=2.00 doesn't do this anymore }
{$ENDIF}
{$i-}
{
     MKFile - Copyright 1993 by Mark May - MK Software
     You are free to use this code in your programs, however
     it may not be included in Source/TPU function libraries
     without my permission.

     Mythical Kingom Tech BBS (513)237-7737 HST/v32
     FidoNet: 1:110/290
     Rime: ->MYTHKING
     You may also reach me at maym@dmapub.dma.org

note: Removed "CreateTempdir", isn't used by this unit ...

     Win32 specific port is (c) by Maarten Bekers.
     OS/2 specific port is (c) by Andre Grueneberg.
}


Interface

{$IFDEF WIN32}
uses Dos, GenDos, Windows, SysUtils, CfgRec, FileObj;
{$ENDIF}

{$IFDEF MSDOS}
Uses Dos, Crt, GenDos, CfgRec, FileObj;
{$ENDIF}

{$IFDEF OS2}
Uses Os2Base, Os2Def, SysUtils, CfgRec, Dos, FileObj;
{$ENDIF}

{$IFDEF GO32V2}
uses Dos, CfgRec, FileObj;
{$ENDIF}

{$IFDEF ELEUNIX}
uses Dos, CfgRec, FileObj;
{$ENDIF}

Const
  fmReadOnly = 0;          {FileMode constants}
  fmWriteOnly = 1;
  fmReadWrite = 2;
  fmDenyAll = 16;
  fmDenyWrite = 32;
  fmDenyRead = 48;
  fmDenyNone = 64;
  fmNoInherit = 128;


Const
  Tries: Word = 20;
  TryDelay: Word = 100;


{$IFDEF WIN32}
Type
  PathStr = String[128];
  DirStr = String[128];
  NameStr = String[13];
  ExtStr = String[4];
{$ENDIF}

Type FindRec = Record
  {$IFDEF WIN32}
  {$IFDEF DELPHI}
     SR: TSearchRec;
  {$ELSE}
     SR: Dos.SearchRec;
  {$ENDIF}
  TStr: Array[0..180] of Char;
  {$ENDIF}
  {$IFDEF MSDOS}
  SR: SearchRec;
  {$ENDIF}
  Dir: DirStr;
  Name: NameStr;
  Ext: ExtStr;
  DError: Longint;
  End;


{$IFDEF WITH_FIND}
Type FindObj = Object
  FI: ^FindRec;
  Procedure Init; {Initialize}
  Procedure Done; {Done}
  Procedure FFirst(FN: String); {Find first}
  Procedure FNext;
  Function  Found: Boolean; {File was found}
  Function  GetName: String; {Get Filename}
  Function  GetFullPath: String; {Get filename with path}
  Function  GetDate: LongInt; {Get file date}
  Function  GetSize: LongInt; {Get file size}
  End;
{$ENDIF}

Type TFileArray = Array[1..$fff0] of Char;

Type TFileRec = Record
  MsgBuffer: ^TFileArray;
  BufferPtr: Word;
  BufferChars: NumReadType;
  BufferStart: LongInt;
  BufferFile: File;
  CurrentStr: String;
  StringFound: Boolean;
  Error: Longint;
  BufferSize: Word;
  End;


{$IFDEF WITH_TFILE}
Type TFile = Object
  TF: ^TFileRec;
  Procedure Init;
  Procedure Done;
  Function  GetString:String;          {Get string from file}
  Function  GetUString: String; {Get LF delimited string}
  Function  OpenTextFile(FilePath: String): Boolean;  {Open file}
  Function  CloseTextFile: Boolean;    {Close file}
  Function  GetChar: Char;             {Internal use}
  Procedure BufferRead;                {Internal use}
  Function  StringFound: Boolean;      {Was a string found}
  Function  SeekTextFile(SeekPos: LongInt): Boolean; {Seek to position}
  Function  GetTextPos: LongInt;       {Get text file position}
  Function  Restart: Boolean;          {Reset to start of file}
  Procedure SetBufferSize(BSize: Word); {Set buffer size}
  End;
{$ENDIF}


function  MkFileError(var F: pFileObj): Word;
Function  FileExist(FName: String): Boolean;
Function  SizeFile(FName: String): LongInt;
Function  DateFile(FName: String): LongInt;
Function  FindPath(FileName: String): String;
Function  LongLo(InNum: LongInt): Word;
Function  LongHi(InNum: LongInt): Word;
Function  LockFile(F: Longint; LockStart: LongInt; LockLength: LongInt): Integer;
Function  UnLockFile(F: Longint; LockStart: LongInt; LockLength: LongInt): Word;
Function  shAssign(Var F: pFileObj; FName: String): Boolean;
Function shUnLock(Var F: pFileObj; LockStart,LockLength: LongInt): Integer;
Function  shLock(Var F: pFileObj; LockStart,LockLength: LongInt): Integer;
Procedure FlushFile(Var F); {Dupe file handle, close dupe handle}
Function  shReset(Var F: pFileObj; RecSize: Word): Boolean;
Function  shRead(Var F: pFileObj; Var Rec; ReadSize: Word; Var NumRead: NumReadType): Boolean;
Function  shWrite(Var F: pFileObj; Var Rec; ReadSize: Word): Boolean;
Function  shOpenFile(Var F: pFileObj; PathName: String): Boolean;
Function  shMakeFile(Var F: pFileObj; PathName: String): Boolean;
Procedure shCloseFile(Var F: pFileObj);
Function  shSeekFile(Var F: pFileObj; FPos: LongInt): Boolean;
Function  shFindFile(Pathname: String; Var Name: String; Var Size, Time: LongInt): Boolean;
Procedure shSetFTime(Var F: pFileObj; Time: LongInt);
Function  GetCurrentPath: String;
{$IFDEF WIN32}
Function  FExpand(Str: String): String;
Procedure FSplit(Path: String; Var Dir: String; Var Name: String; Var Ext: String);
Function  FSearch(Path: String; DirList: String): String;
{$ENDIF}
{$IFDEF MSDOS}
Function  IsDevice(FilePath: String): Boolean;
{$ENDIF}
Function  LoadFilePos(FN: String; Var Rec; FS: Word; FPos: LongInt): Word;
Function  LoadFile(FN: String; Var Rec; FS: Word): Word;
Function  SaveFilePos(FN: String; Var Rec; FS: Word; FPos: LongInt): Word;
Function  SaveFile(FN: String; Var Rec; FS: Word): Word;
Function  ExtendFile(FN: String; ToSize: LongInt): Word;
Function  GetTempName(FN: String): String;
Function  GetTextPos(Var F: Text): LongInt;
Function  FindOnPath(FN: String; Var OutName: String): Boolean;
Function  CopyFile(FN1: String; FN2: String): Boolean;
Function  EraseFile(FN: String): Boolean;
Function  MakePath(FP: String): Boolean;

{$IFDEF WIN32}
procedure GetFtime(var f; var Time: Longint);
procedure SetFTime(var f; Time: Longint);
procedure GetFAttr(var F; var Attr: Word);
procedure SetFAttr(var F; Attr: Word);
{$ENDIF}

Implementation

{$IFDEF MSDOS}
Uses
  StrPath;
{$ELSE}
Uses
  StrPath;
{$ENDIF}


{$IFDEF WIN32}
procedure GetFtime(var f; var Time: Longint);
begin
  {$IFNDEF FPC}
    {$IFDEF DELPHI}
      Time := SysUtils.FileGetDate(System.TFileRec(F).Handle);
    {$ELSE}
      Time := SysUtils.FileGetDate(SysUtils.TFileRec(F).Handle);
    {$ENDIF}
  {$ELSE}
    Time := SysUtils.FileGetDate(DOS.FileRec(F).Handle);
  {$ENDIF}
end; { proc. GetFTime }

procedure SetFTime(var f; Time: Longint);
begin
  {$IFNDEF FPC}
    {$IFDEF DELPHI}
      SysUtils.FileSetDate(System.TFileRec(F).Handle, Time);
    {$ELSE}
      SysUtils.FileSetDate(SysUtils.TFileRec(F).Handle, Time);
    {$ENDIF}
  {$ELSE}
    SysUtils.FileSetDate(Dos.FileRec(F).Handle, Time);
  {$ENDIF}
end; { proc. SetFtime }

procedure GetFAttr(var F; var Attr: Word);
var Temp: Integer;
begin
  {$IFNDEF FPC}
    {$IFDEF DELPHI}
      Temp := FileGetAttr(System.TFileRec(F).Name);
    {$ELSE}
      Temp := FileGetAttr(SysUtils.TFileRec(F).Name);
    {$ENDIF}
  {$ELSE}
    Temp := FileGetAttr(StrPas(Dos.FileRec(F).Name));
  {$ENDIF}

  If Temp<0 then Attr := 00
   else Attr := Temp;
end; { proc. getfAttr }

procedure SetFAttr(var F; Attr: Word);
begin
  {$IFNDEF FPC}
    {$IFDEF DELPHI}
      FileSetAttr(System.TFileRec(F).Name, Attr);
    {$ELSE}
      FileSetAttr(SysUtils.TFileRec(F).Name, Attr);
    {$ENDIF}
  {$ELSE}
    FileSetAttr(StrPas(Dos.FileRec(F).Name), Attr);
  {$ENDIF}
end; { proc. SetfAttr }
{$ENDIF}



{$IFDEF WIN32}
Function FExpand(Str: String): String;
  Var
    IStr: Array[0..128] of Char;
    OStr: Array[0..128] of Char;

  Begin
  {$IFNDEF WIN32}
    StrPCopy(IStr, Str);
    FileExpand(OStr, IStr);
    FExpand := StrPas(OStr);
  {$ELSE}
    FExpand := ExpandFileName(Str);
  {$ENDIF}
  End;
{$ENDIF}

{$IFDEF WIN32}
Procedure FSplit(Path: String; Var Dir: String; Var Name: String; Var Ext: String);
  Var
    FPath: Array[0..129] of Char;
    TD: Array[0..129] of Char;
    TN: Array[0..14] of Char;
    TE: Array[0..5] of Char;

  Begin
  {$IFNDEF WIN32}
    StrPCopy(FPath, Path);
    FileSplit(FPath, TD, TN, TE);
    Dir := StrPas(TD);
    Name := StrPas(TN);
    Ext := StrPas(TE);
  {$ELSE}
    Dir := ExtractFilePath(Path);
    Name := ExtractFileName(Path);
    Ext := ExtractFileExt(Path);
  {$ENDIF}
  End;
{$ENDIF}

{$IFDEF WIN32}
Function  FSearch(Path: String; DirList: String): String;
  Var
    FPath: Array[0..129] of Char;
    DL: Array[0..129] of Char;
    RS: Array[0..129] of Char;

  Begin
  {$IFNDEF WIN32}
    StrPCopy(Fpath, Path);
    StrPCopy(DL, DirList);
    FileSearch(RS, FPath, DL);
    FSearch := StrPas(RS);
  {$ELSE}
    FSearch := FileSearch(Path, DirList);
  {$ENDIF}
  End;
{$ENDIF}


{$IFDEF WITH_FIND}
Procedure FindObj.Init;
  Begin
  New(FI);
  FI^.DError := 1;
  End;


Procedure FindObj.Done;
  Begin
  {$IFNDEF MSDOS}
    FindClose(Fi^.Sr);
  {$ENDIF}
  Dispose(FI);
  End;


Procedure FindObj.FFirst(FN: String);
  Begin
  FN := FExpand(FN);
  FSplit(FN, FI^.Dir, FI^.Name, FI^.Ext);
  {$IFDEF WIN32}
  StrPCopy(FI^.TStr, FN);
  {$IFDEF WIN32}
    Fi^.DError := FindFirst(FI^.TStr, faReadOnly + faArchive, FI^.SR);
  {$ELSE}
    FindFirst(FI^.TStr, faReadOnly + faArchive, FI^.SR);
  {$ENDIF}

  FindClose(FI^.Sr);
  {$ELSE}
  FindFirst(FN, Archive + ReadOnly, FI^.SR);
  {$ENDIF}

  {$IFNDEF WIN32}
    FI^.DError := DosError;
  {$ENDIF}
  End;


Function  FindObj.GetName: String;
  Begin
  If Found Then
    Begin
    {$IFDEF WIN32}
      {$IFNDEF WIN32}
         GetName := StrPas(FI^.SR.Name)
      {$ELSE}
         GetName := Fi^.Sr.Name;
      {$ENDIF}
    {$ELSE}
    GetName := FI^.SR.Name
    {$ENDIF}
    End
  Else
    GetName := '';
  End;



Function FindObj.GetFullPath: String;
  Begin
  GetFullPath := FI^.Dir + GetName;
  End;


Function  FindObj.GetSize: LongInt;
  Begin
  If Found Then
    GetSize := FI^.SR.Size
  Else
    GetSize := 0;
  End;


Function  FindObj.GetDate: LongInt;
  Begin
  If Found Then
    GetDate := FI^.SR.Time
  Else
    GetDate := 0;
  End;


Procedure FindObj.FNext;
  Begin
  {$IFNDEF WIN32}
    FindNext(FI^.SR);
    FI^.DError := DosError;
  {$ELSE}
    Fi^.DError := FindNext(FI^.SR);
  {$ENDIF}
  End;


Function FindObj.Found: Boolean;
  Begin
  Found := (FI^.DError = 0);
  End;
{$ENDIF}

Function shAssign(Var F: pFileObj; FName: String): Boolean;
  Begin
  F^.Assign(Fname);
  shAssign := (f^.ErrorCode = 0);
  End;



Function  shRead(Var F: pFileObj; Var Rec; ReadSize: Word; Var NumRead: NumReadType): Boolean;
  Var
    Count: Word;
    Code: Longint;

  Begin
  Count := Tries;
  Code := 5;
  While ((Count > 0) and (Code = 5)) Do
    Begin
    NumRead := F^.BlkRead(Rec, ReadSize);
    Code := F^.ErrorCode;
    Dec(Count);
    End;
  ShRead := (Code = 0);
  End;


Function shWrite(Var F: pFileObj; Var Rec; ReadSize: Word): Boolean;
  Var
    Count: Word;
    Code: Longint;

  Begin
  Count := Tries;
  Code := 5;
  While ((Count > 0) and (Code = 5)) Do
    Begin
    F^.BlkWrite(Rec, ReadSize);
    Code := F^.ErrorCOde;
    Dec(Count);
    End;
  shWrite := (Code = 0);
  End;



{$IFDEF WIN32}
Function GetCurrentPath: String;
  Var
{$IFNDEF WIN32}
    Path: Array[0..128] of Char;
    CName: Array[0..13] of Char;
    CExt: Array[0..4] of Char;
    TStr: Array[0..128] of Char;
{$ELSE}
    Path: String[128];
    CName: String[255];
    CExt: String[255];
    TStr: AnsiString;
{$ENDIF}

  Begin
  {$IFNDEF WIN32}
    FileExpand('*.*', TStr);
    FileSplit(TStr, Path, CName, CExt);
  {$ELSE}
    TStr := FExpand(StrPas('*.*'));
    FSplit(TStr, Path, CName, CExt);
  {$ENDIF}

  {$IFNDEF WIN32}
    GetCurrentPath := StrPas(Path);
  {$ELSE}
    GetCurrentPath := Path;
  {$ENDIF}
  End;
{$ELSE}
Function GetCurrentPath: String;
  Var
    CName: NameStr;
    Path: DirStr;
    CExt: ExtStr;

  Begin
  FSplit(FExpand('*.*'),Path,CName,CExt);
  GetCurrentPath := Path;
  End;
{$ENDIF}


Function shLock(Var F: pFileObj; LockStart,LockLength: LongInt): Integer;
  Var
    Count: Word;
    Code: Integer;

  Begin
  Count := Tries;
  Code := $21;
  While ((Count > 0) and (Code = $21)) Do
    Begin
    Code := LockFile(F^.FileHandle,LockStart,LockLength);
    Dec(Count);

    {$IFDEF MSDOS}
      If Code = $21 Then
        Delay(TryDelay);
    {$ENDIF}

    End;
  If Code = 1 Then
    Code := 0;
  shLock := Code;
  End;

Function shUnLock(Var F: pFileObj; LockStart,LockLength: LongInt): Integer;
  Var
    Count: Word;
    Code: Integer;

  Begin
  Count := Tries;
  Code := $21;
  While ((Count > 0) and (Code = $21)) Do
    Begin
    Code := UnLockFile(F^.FileHandle,LockStart,LockLength);
    Dec(Count);

    {$IFDEF MSDOS}
      If Code = $21 Then
        Delay(TryDelay);
    {$ENDIF}

    End;
  If Code = 1 Then
    Code := 0;
  shUnLock := Code;
  End;



Function shReset(Var F: pFileObj; RecSize: Word): Boolean;
  Var
    Count: Word;
    Code: Longint;

  Begin
  Count := Tries;
  Code := 5;
  While ((Count > 0) and (Code = 5)) Do
    Begin
    F^.Open(RecSize);
    Code := F^.ErrorCode;
    Dec(Count);
    End;
  ShReset := (Code = 0);
  End;


{$IFDEF MSDOS}
Procedure FlushFile(Var F); {Dupe file handle, close dupe handle}
  Var
    Handle: Word Absolute F;
  {$IFDEF BASMINT}
    Tmp: Word;
  {$ELSE}
    Regs: Registers;
  {$ENDIF}

  Begin
  {$IFDEF BASMINT}
  Tmp := Handle;
  Asm
    Mov ah, $45;
    Mov bx, Tmp;
    Int $21;
    Jc  @JFlush;
    Mov bx, ax;
    Mov ah, $3e;
    Int $21;
    @JFlush:
    End;
  {$ELSE}
  Regs.ah := $45;
  Regs.bx := Handle;
  MsDos(Regs);
  If (Regs.Flags and 1) = 0 Then   {carry}
    Begin
    Regs.bx := Regs.ax;
    Regs.Ah := $3e;
    MsDos(Regs);
    End;
  {$ENDIF}
  End;
{$ENDIF}

{$IFDEF WIN32}
Procedure FlushFile(Var F); {Dupe file handle, close dupe handle}
begin
  { What the flushfile function in BASM does is this: }
  { (01). It duplicates the file-handle (so you have 2 handles of 1 file) }
  { (02). It closes the 2nd. This will make the original file-handle still }
  {       maintained, but because the 2nd handle (the copy) is closed, all }
  {       changes are written to disk. }

  {$IFNDEF FPC}
    {$IFDEF DELPHI}
      Windows.FlushFileBuffers(System.TFileRec(F).Handle);
    {$ELSE}
      Windows.FlushFileBuffers(SysUtils.TFileRec(F).Handle);
    {$ENDIF}
  {$ELSE}
    Windows.FlushFileBuffers(Dos.FileRec(F).Handle);
  {$ENDIF}
end; { proc. FlushFile }
{$ENDIF}

{$IFDEF OS2}
Procedure FlushFile(Var F); {Dupe file handle, close dupe handle}
begin
  DosResetBuffer(FileRec(F).Handle);
end; { proc. FlushFile }
{$ENDIF}

{$IFDEF GO32V2}
Procedure FlushFile(Var F); {Dupe file handle, close dupe handle}
begin
end; { proc. FlushFile }
{$ENDIF}

{$IFDEF ELEUNIX}
Procedure FlushFile(Var F); {Dupe file handle, close dupe handle}
begin
end; { proc. FlushFile }
{$ENDIF}

{$IFDEF MSDOS}
Function LockFile(F: Longint; LockStart: LongInt; LockLength: LongInt): Integer;
  Var
    Handle: Word Absolute F;
    Tmp: Integer;
    StrtHi: Word;
    StrtLo: Word;
    LgHi: Word;
    LgLo: Word;
  {$IFNDEF BASMINT}
    Regs: Registers;
  {$ENDIF}

  Begin
  Tmp := Handle;
  StrtHi := LongHi(LockStart);
  StrtLo := LongLo(LockStart);
  LgHi := LongHi(LockLength);
  LgLo := LongLo(LockLength);

  {$IFDEF BASMINT}
  Asm
    Mov ah, $5c;
    Mov al, $00;
    Mov bx, Tmp;
    Mov cx, StrtHi;
    Mov dx, StrtLo;
    Mov si, LgHi;                 {00h = success           }
    Mov di, LgLo;                 {01h = share not loaded  }
    Int $21;                      {06h = invalid handle    }
    Jc @JLock                     {21h = lock violation    }
    Mov ax, $00;                  {24h = share buffer full }
    @JLock:
    Mov Tmp, ax;
    End;
  {$ELSE}
  Regs.ah := $5c;
  Regs.al := $00;
  Regs.bx := Tmp;
  Regs.cx := StrtHi;
  Regs.dx := StrtLo;
  Regs.si := LgHi;
  Regs.di := LgLo;
  MsDos(Regs);
  If (Regs.Flags and 1) = 0 Then
    Begin
    Regs.ax := 0;
    End;
  Tmp := Regs.ax;
  {$ENDIF}
  If Tmp = 1 Then
    Tmp := 0;
  LockFile := Tmp;
  End;
{$ENDIF}


{$IFDEF WIN32}
Function LockFile(F: Longint; LockStart: LongInt; LockLength: LongInt): Integer;
var Tmp: Word;
    StrtHi: Longint;
    StrtLo: Longint;
    LgHi: Longint;
    LgLo: Longint;
begin
  StrtHi := LongHi(LockStart);
  StrtLo := LongLo(LockStart);
  LgHi := LongHi(LockLength);
  LgLo := LongLo(LockLength);

  if NOT Windows.LockFile(F, StrtLo, StrtHi, LgLo, LgHi) then
      Tmp := GetLastError
       else Tmp := 00;

  LockFile := Tmp;
end; { func. LockFile }
{$ENDIF}

{$IFDEF OS2}
Function LockFile(F: Longint; LockStart: LongInt; LockLength: LongInt): Integer;
var Lock, UnLock: FileLock;
begin
  Lock.lOffset := LockStart;
  Lock.lRange  := LockLength;

  FillChar(UnLock, SizeOf(FileLock), 00);

  LockFile := DosSetFileLocks(F, UnLock, Lock, 1000, 0);
end; { func. LockFile }
{$ENDIF}

{$IFDEF GO32V2}
Function LockFile(F: Longint; LockStart: LongInt; LockLength: LongInt): Integer;
begin
  LockFile := 00;
end; { func. LockFile }
{$ENDIF}

{$IFDEF ELEUNIX}
Function LockFile(f: longint; LockStart: LongInt; LockLength: LongInt): Integer;
begin
  LockFile := 00;
end; { func. LockFile }
{$ENDIF}


{$IFDEF MSDOS}
Function UnLockFile(F: Longint; LockStart: LongInt; LockLength: LongInt): Word;
  Var
    Handle: Word Absolute F;
    Tmp: Word;
    StrtHi: Word;
    StrtLo: Word;
    LgHi: Word;
    LgLo: Word;
  {$IFNDEF BASMINT}
    Regs: Registers;
  {$ENDIF}

  Begin
  Tmp := Handle;
  StrtHi := LongHi(LockStart);
  StrtLo := LongLo(LockStart);
  LgHi := LongHi(LockLength);
  LgLo := LongLo(LockLength);
  {$IFDEF BASMINT}
  Asm
    Mov ah, $5c;
    Mov al, $01;
    Mov bx, Tmp;
    Mov cx, StrtHi;
    Mov dx, StrtLo;
    Mov si, LgHi;                 {00h = success           }
    Mov di, LgLo;                 {01h = share not loaded  }
    Int $21;                      {06h = invalid handle    }
    Jc @JLock                     {21h = lock violation    }
    Mov ax, $00;                  {24h = share buffer full }
    @JLock:
    Mov Tmp, ax;
    End;
  {$ELSE}
  Regs.ah := $5c;
  Regs.al := $01;
  Regs.bx := Tmp;
  Regs.cx := StrtHi;
  Regs.dx := StrtLo;
  Regs.si := LgHi;
  Regs.di := LgLo;
  MsDos(Regs);
  If (Regs.Flags and 1) = 0 Then
    Begin
    Regs.ax := 0;
    End;
  Tmp := Regs.ax;
  {$ENDIF}
  If Tmp = 1 Then
    Tmp := 0;
  UnLockFile := Tmp;
  End;
{$ENDIF}

{$IFDEF WIN32}
Function UnLockFile(F: longint; LockStart: LongInt; LockLength: LongInt): Word;
var Tmp: Word;
    StrtHi: Longint;
    StrtLo: Longint;
    LgHi: Longint;
    LgLo: Longint;
begin
  StrtHi := LongHi(LockStart);
  StrtLo := LongLo(LockStart);
  LgHi := LongHi(LockLength);
  LgLo := LongLo(LockLength);

   if NOT Windows.UnLockFile(F, StrtLo, StrtHi, LgLo, LgHi) then
     Tmp := GetLastError
       else Tmp := 00;

  UnLockFile := Tmp;
end; { func. UnLockFile }
{$ENDIF}

{$IFDEF OS2}
Function UnLockFile(F: Longint; LockStart: LongInt; LockLength: LongInt): Word;
var Lock,
    UnLock : FileLock;
begin
  UnLock.lOffSet := LockStart;
  UnLock.lRange  := LockLength;

  FillChar(Lock, SizeOf(FileLock), 00);
  UnLockFile := DosSetFileLocks(F, UnLock, Lock, 1000, 0);
  DosCancelLockRequest(F, UnLock);
end; { func. UnLockFile }
{$ENDIF}

{$IFDEF GO32V2}
Function UnLockFile(F: Longint; LockStart: LongInt; LockLength: LongInt): Word;
begin
  UnLockFile := 00;
end; { func. UnLockFile }
{$ENDIF}

{$IFDEF ELEUNIX}
Function UnLockFile(F:longint; LockStart: LongInt; LockLength: LongInt): Word;
begin
  UnLockFile := 00;
end; { func. UnLockFile }
{$ENDIF}

Function LongLo(InNum: LongInt): Word;
  Begin
  LongLo := InNum and $FFFF;
  End;


Function LongHi(InNum: LongInt): Word;
  Begin
  LongHi := InNum Shr 16;
  End;


Function SizeFile(FName: String): LongInt;
  Var
    {$IFDEF WIN32}
    {$IFNDEF VIRTUALPASCAL}
      SR: TSearchRec;
    {$ELSE}
      SR: SearchRec;
    {$ENDIF}
    {$ELSE}
    SR: SearchRec;
    {$ENDIF}
    WinTmpError: Word;

  Begin
  {$IFDEF WIN32}
    {$IFNDEF VIRTUALPASCAL}
      WinTmpError := FindFirst(FName, faAnyFile, SR);
      FindClose(Sr);
    {$ELSE}
      Dos.FindFirst(FName, faAnyFile, SR);
      Dos.FindClose(Sr);
    {$ENDIF}
  {$ENDIF}

  {$IFDEF OS2}
    FindFirst(FName, AnyFile, SR);
    FindClose(Sr);
  {$ENDIF}

  {$IFDEF FPC}
    FindFirst(FName, AnyFile, SR);
    FindClose(Sr);
  {$ENDIF}

  {$IFDEF MSDOS}
    FindFirst(FName, AnyFile, SR);
  {$ENDIF}

  {$IFNDEF WIN32}
    WinTmpError := DosError;
  {$ENDIF}

  {$IFDEF VIRTUALPASCAL}
    {$IFDEF WIN32}
      WinTmpError := DosError;
    {$ENDIF}
  {$ENDIF}

  If WinTmpError = 0 Then
    SizeFile := SR.Size
  Else
    SizeFile := -1;
  End;


Function  DateFile(FName: String): LongInt;
  Var
    {$IFDEF WIN32}
    {$IFNDEF VIRTUALPASCAL}
      SR: TSearchRec;
    {$ELSE}
      SR: SearchRec;
    {$ENDIF}
    {$ELSE}
    SR: SearchRec;
    {$ENDIF}
    WinTmpError: Word;

  Begin
  {$IFDEF WIN32}
    {$IFNDEF VIRTUALPASCAL}
      WinTmpError := FindFirst(FName, faAnyFile, SR);
      FindClose(Sr);
    {$ELSE}
      Dos.FindFirst(FName, faAnyFile, SR);
      Dos.FindClose(Sr);
    {$ENDIF}
  {$ENDIF}

  {$IFDEF MSDOS}
    FindFirst(FName, AnyFile, SR);
  {$ENDIF}

  {$IFDEF OS2}
    FindFirst(FName, AnyFile, SR);
    FindClose(Sr);
  {$ENDIF}

  {$IFDEF FPC}
    FindFirst(FName, AnyFile, SR);
    FindClose(Sr);
  {$ENDIF}

  {$IFNDEF WIN32}
    WinTmpError := DosError;
  {$ENDIF}

  {$IFDEF VIRTUALPASCAL}
    {$IFDEF WIN32}
      WinTmpError := DosError;
    {$ENDIF}
  {$ENDIF}

  If WinTmpError = 0 Then
    DateFile := SR.Time
  Else
    DateFile := 0;
  End;


Function FileExist(FName: String): Boolean;
  var Dirinfo: SearchRec;
      WinTmpError: Longint;
  Begin
  If IoResult <> 0 Then;

  {$IFDEF DELPHI}
    WinTmpError := FindFirst(FName, faAnyFile, Dirinfo);
    FindClose(Dirinfo);
  {$ELSE}
    Dos.FindFirst(FName, AnyFile, Dirinfo);
    WinTmpError := DosError;
    {$IFNDEF MSDOS}
      Dos.FindClose(Dirinfo);
    {$ENDIF}
  {$ENDIF}

  FileExist := (WinTmpError = 0);
  End;

{$IFDEF MSDOS}
Function FindPath(FileName: String):String;
  Begin
  FindPath := FileName;
  If FileExist(FileName) Then
    FindPath := FExpand(FileName)
  Else
    FindPath := FExpand(FSearch(FileName,GetEnv('PATH')));
  End;
{$ENDIF}

{$IFDEF OS2}
Function FindPath(FileName: String):String;
  Begin
  FindPath := FileName;
  If FileExist(FileName) Then
    FindPath := FExpand(FileName)
  Else
    FindPath := FExpand(FSearch(FileName,GetEnv('PATH')));
  End;
{$ENDIF}


{$IFDEF WIN32}
Function FindPath(FileName: String): String;
  Var
  {$IFNDEF WIN32}
    TStr: Array[0..128] of Char;
  {$ELSE}
    TStr: String;
  {$ENDIF}
    NStr: Array[0..14] of Char;

  Begin
  FindPath := FileName;
  If FileExist(FileName) Then
    Begin

    {$IFNDEF WIN32}
      FExpand(TStr, StrPCopy(NStr,FileName));
      FindPath := StrPas(TStr);
    {$ELSE}
      TStr := FExpand(StrPas(StrPCopy(NStr,FileName)));
      FindPath := TStr;
    {$ENDIF}
    End
  Else
    Begin
    {$IFNDEF WIN32}
      FileSearch(TStr, StrPCopy(NStr, FileName), GetEnvVar('Path'));
    {$ELSE}
      TStr := SysUtils.FileSearch(StrPas(StrPCopy(NStr, FileName)), GetEnv('Path'));
    {$ENDIF}

    TStr := FExpand(TStr);

    {$IFNDEF WIN32}
    FindPath := StrPas(TStr);
    {$ELSE}
    FIndPath := TStr;
    {$ENDIF}

    End;
  End;
{$ENDIF}

{$IFDEF GO32V2}
Function FindPath(FileName: String):String;
  Begin
  FindPath := FileName;
  If FileExist(FileName) Then
    FindPath := FExpand(FileName)
  Else
    FindPath := FExpand(FSearch(FileName,GetEnv('PATH')));
  End;
{$ENDIF}

{$IFDEF ELEUNIX}
Function FindPath(FileName: String):String;
  Begin
  FindPath := FileName;
  If FileExist(FileName) Then
    FindPath := FExpand(FileName)
  Else
    FindPath := FExpand(FSearch(FileName,GetEnv('PATH')));
  End;
{$ENDIF}


{$IFDEF WITH_TFILE}
Procedure TFile.BufferRead;
  Begin
  TF^.BufferStart := FilePos(TF^.BufferFile);
  if Not shRead (TF^.BufferFile,TF^.MsgBuffer^ , TF^.BufferSize, TF^.BufferChars) Then
    TF^.BufferChars := 0;
  TF^.BufferPtr := 1;
  End;


Function TFile.GetChar: Char;
  Begin
  If TF^.BufferPtr > TF^.BufferChars Then
    BufferRead;
  If TF^.BufferChars > 0 Then
    GetChar := TF^.MsgBuffer^[TF^.BufferPtr]
  Else
    GetChar := #0;
  Inc(TF^.BufferPtr);
  If TF^.BufferPtr > TF^.BufferChars Then
    BufferRead;
  End;


Function TFile.GetString: String;

  Var
    TempStr: String;
    GDone: Boolean;
    Ch: Char;

  Begin
    TempStr := '';
    GDone := False;
    TF^.StringFound := False;
    While Not GDone Do
      Begin
      Ch := GetChar;
      Case Ch Of
        #0:  If TF^.BufferChars = 0 Then
               GDone := True
             Else
               Begin
               Inc(Byte(TempStr[0]));
               TempStr[Ord(TempStr[0])] := Ch;
               TF^.StringFound := True;
               If Length(TempStr) = 255 Then
                 GDone := True;
               End;
        #10:;
        #26:;
        #13: Begin
             GDone := True;
             TF^.StringFound := True;
             End;
        Else
          Begin
            Inc(Byte(TempStr[0]));
            TempStr[Ord(TempStr[0])] := Ch;
            TF^.StringFound := True;
            If Length(TempStr) = 255 Then
              GDone := True;
          End;
        End;
      End;
    GetString := TempStr;
  End;


Function TFile.GetUString: String;

  Var
    TempStr: String;
    GDone: Boolean;
    Ch: Char;

  Begin
  TempStr := '';
  GDone := False;
  TF^.StringFound := False;
  While Not GDone Do
    Begin
    Ch := GetChar;
    Case Ch Of
      #0:  If TF^.BufferChars = 0 Then
             GDone := True
           Else
             Begin
             Inc(Byte(TempStr[0]));
             TempStr[Ord(TempStr[0])] := Ch;
             TF^.StringFound := True;
             If Length(TempStr) = 255 Then
               GDone := True;
             End;
      #13:;
      #26:;
      #10: Begin
           GDone := True;
           TF^.StringFound := True;
           End;
      Else
        Begin
        Inc(Byte(TempStr[0]));
        TempStr[Ord(TempStr[0])] := Ch;
        TF^.StringFound := True;
        If Length(TempStr) = 255 Then
          GDone := True;
        End;
      End;
    End;
  GetUString := TempStr;
  End;


Function TFile.OpenTextFile(FilePath: String): Boolean;
  Begin
  If Not shAssign(TF^.BufferFile, FilePath) Then;
  FileMode := fmReadOnly + fmDenyNone;
  If Not shReset(TF^.BufferFile,1) Then
    OpenTextFile := False
  Else
    Begin
    BufferRead;
    If TF^.BufferChars > 0 Then
      TF^.StringFound := True
    Else
      TF^.StringFound := False;
    OpenTextFile := True;
    End;
  End;


Function TFile.SeekTextFile(SeekPos: LongInt): Boolean;
  Begin
  TF^.Error := 0;
  If ((SeekPos < TF^.BufferStart) Or (SeekPos > TF^.BufferStart + TF^.BufferChars)) Then
    Begin
    Seek(TF^.BufferFile, SeekPos);
    TF^.Error := IoResult;
    BufferRead;
    End
  Else
    Begin
    TF^.BufferPtr := SeekPos + 1 - TF^.BufferStart;
    End;
  SeekTextFile := (TF^.Error = 0);
  End;


Function TFile.GetTextPos: LongInt;       {Get text file position}
  Begin
  GetTextPos := TF^.BufferStart + TF^.BufferPtr - 1;
  End;


Function TFile.Restart: Boolean;
  Begin
  Restart := SeekTextFile(0);
  End;


Function TFile.CloseTextFile: Boolean;
  Begin
  Close(TF^.BufferFile);
  CloseTextFile := (IoResult = 0);
  End;


Procedure TFile.SetBufferSize(BSize: Word);
  Begin
  FreeMem(TF^.MsgBuffer, TF^.BufferSize);
  TF^.BufferSize := BSize;
  GetMem(TF^.MsgBuffer, TF^.BufferSize);
  TF^.BufferChars := 0;
  TF^.BufferStart := 0;
  If SeekTextFile(GetTextPos) Then;
  End;


Procedure TFile.Init;
  Begin
  New(TF);
  TF^.BufferSize := 2048;
  GetMem(TF^.MsgBuffer, TF^.BufferSize);
  End;


Procedure TFile.Done;
  Begin
  Close(TF^.BufferFile);
  If IoResult <> 0 Then;
  FreeMem(TF^.MsgBuffer, TF^.BufferSize);
  Dispose(TF);
  End;


Function TFile.StringFound: Boolean;
  Begin
  StringFound := TF^.StringFound;
  End;
{$ENDIF}


Function  shOpenFile(Var F: pFileObj; PathName: String): Boolean;
  Begin
  F^.Assign(PathName);
  F^.FileMode := fmReadWrite + fmDenyNone;
  shOpenFile := shReset(f,1);
  End;


Function  shMakeFile(Var F: pFileObj; PathName: String): Boolean;
  Begin
  f^.Assign(pathname);
  f^.Create(1);
  shMakeFile := (F^.IOresult = 0);
  END;


Procedure shCloseFile(Var F: pFileObj);
  Begin
  F^.Close;
  If (F^.IOresult <> 0) Then;
  End;


Function  shSeekFile(Var F: pFileObj; FPos: LongInt): Boolean;
  Begin
  F^.Seek(FPos);
  shSeekFile := (IOresult = 0);
  End;


Function  shFindFile(Pathname: String; Var Name: String; Var Size, Time: LongInt): Boolean;
  Var
    {$IFDEF Win32}
     {$IFNDEF VIRTUALPASCAL}
        SR: TSearchRec;
        PStr: Array[0..128] of Char;
     {$ELSE}
        SR: SearchRec;
     {$ENDIF}
    {$ELSE}
      SR: SearchRec;
   {$ENDIF}
    WinTmpError: Word;

  Begin
  FillChar(Sr, SizeOf(SearchRec), #00);

  {$IFDEF Win32}
    {$IFNDEF VIRTUALPASCAL}
       StrPCopy(PStr, PathName);
       WinTmpError := FindFirst(PStr, faArchive, SR);
       FindClose(Sr);
    {$ELSE}
      Dos.FindFirst(PathName, Archive, Sr);
      Dos.FindClose(Sr);
      WinTmpError := DosError;
    {$ENDIF}
  {$ENDIF}

  {$IFDEF MSDOS}
    FindFirst(PathName, Archive, SR);
    WinTmpError := DosError;
  {$ENDIF}

  {$IFDEF GO32V2}
    FindFirst(PathName, Archive, SR);
    WinTmpError := DosError;
  {$ENDIF}

  {$IFDEF OS2}
    FindFirst(PathName, Archive, Sr);
    FindClose(Sr);
    WinTmpError := DosError;
  {$ENDIF}

  If (WinTmpError = 0) Then
    Begin
    shFindFile := True;
    Name := Sr.Name;
    Size := Sr.Size;
    Time := Sr.Time;
    End
  Else
    Begin
    shFindFile := False;
    End;
  End;


Procedure shSetFTime(Var F: pFileObj; Time: LongInt);
  Begin
    F^.SetFileTime(Time);
  End;


{$IFDEF MSDOS}
Function IsDevice(FilePath: String): Boolean;
  Var
    F: File;
    Handle: Word Absolute F;
    Tmp: Word;
  {$IFNDEF BASMINT}
    Regs: Registers;
  {$ENDIF}

  Begin
  Assign(F, FilePath);
  Reset(F);
  If IoResult <> 0 Then
    IsDevice := False
  Else
    Begin
    Tmp := Handle;
{$IFDEF BASMINT}
    Asm
      Mov ah, $44;
      Mov al, $00;
      Mov bx, Tmp;
      Int $21;
      Or  dx, $80;
      Je  @JDev;
      Mov ax, $01;
      @JDev:
      Mov ax, $00;
      Mov @Result, al;
      End;
{$ELSE}
    Regs.ah := $44;
    Regs.al := $00;
    Regs.bx := Tmp;
    MsDos(Regs);
    IsDevice := (Regs.Dx and $80) <> 0;
{$ENDIF}
    End;
  Close(F);
  If IoResult <> 0 Then;
  End;
{$ENDIF}

{$IFDEF OS2}
Function IsDevice(FilePath: String): Boolean;
var F        : File;
    OldFMode : LongInt;
    RC       : ApiRet;
    PType,
    Attr     : LongInt;
begin
  IsDevice := False;

  Assign(f, Filepath);
  OldFMode := FileMode;
  FileMode := fmReadOnly + fmDenyNone;
  Reset(F, 01);
  if IOresult=00 then
    begin
      RC := DosQueryHType(FileRec(F).Handle, pType, Attr);
      if (rc=No_Error) AND (pType AND $FF = 01) then
        IsDevice := True;
      Close(F);
    end; { if Ioresult }

  FileMode := OldFMode;
end; { func. IsDevice }
{$ENDIF}

Function LoadFile(FN: String; Var Rec; FS: Word): Word;
  Begin
  LoadFile := LoadFilePos(FN, Rec, FS, 0);
  End;


Function LoadFilePos(FN: String; Var Rec; FS: Word; FPos: LongInt): Word;
  Var
    F: pFileObj;
    Error: Longint;
    NumRead: NumReadType;

  Begin
  New(F, Init);
  Error := 0;
  If Not FileExist(FN) Then
    Error := 8888;
  If Error = 0 Then
    Begin
    If Not shAssign(F, FN) Then
      Error := MKFileError(F);
    End;
  FileMode := fmReadOnly + fmDenyNone;
  If Not shReset(F,1) Then
    Error := MKFileError(f);
  If Error = 0 Then
    Begin
    F^.Seek(FPos);
    Error := F^.IoResult;
    End;
  If Error = 0 Then
    If Not shRead(F, Rec, FS, NumRead) Then
      Error := MKFileError(f);
  If Error = 0 Then
    Begin
    F^.Close;
    Error := F^.IoResult;
    End;
  LoadFilePos := Error;

  Dispose(F, Done);
  End;


Function SaveFile(FN: String; Var Rec; FS: Word): Word;
   Begin
   SaveFile := SaveFilePos(FN, Rec, FS, 0);
   End;



Function SaveFilePos(FN: String; Var Rec; FS: Word; FPos: LongInt): Word;
  Var
    F: pFileObj;
    Error: Longint;

  Begin
  New(F, Init);
  Error := 0;
  If Not shAssign(F, FN) Then
    Error := MKFileError(f);
  FileMode := fmReadWrite + fmDenyNone;
  If FileExist(FN) Then
    Begin
    If Not shReset(F,1) Then
      Error := MKFileError(f);
    End
  Else
    Begin
    F^.Create(1);
    Error := F^.IoResult;
    End;
  If Error = 0 Then
    Begin
    F^.Seek(Fpos);
    Error := F^.IoResult;
    End;
  If Error = 0 Then
    If FS > 0 Then
      Begin
      If Not shWrite(F, Rec, FS) Then
        Error := MKFileError(f);
      End;
  If Error = 0 Then
    Begin
    F^.Done;
    Error := F^.IoResult;
    End;
  SaveFilePos := Error;
  Dispose(F, Done);
  End;


Function ExtendFile(FN: String; ToSize: LongInt): Word;
{Pads file with nulls to specified size}
  Type
    FillType = Array[1..8000] of Byte;

  Var
    F: pFileObj;
    Error: Longint;
    FillRec: ^FillType;

  Begin
  Error := 0;
  New(FillRec);
  If FillRec = Nil Then
    Error := 10;
  If Error = 0 Then
    Begin
    FillChar(FillRec^, SizeOf(FillRec^), 0);
    If Not shAssign(F, FN) Then
    Error := MKFileError(f);
    FileMode := fmReadWrite + fmDenyNone;
    If FileExist(FN) Then
      Begin
      If Not shReset(F,1) Then
        Error := MKFileError(f);
      End
    Else
      Begin
      F^.Create(1);
      Error := F^.IoResult;
      End;
    End;
  If Error = 0 Then
    Begin
    F^.Seek(F^.FileSize);
    Error := F^.IoResult;
    End;
  If Error = 0 Then
    Begin
    While ((F^.FileSize < (ToSize - SizeOf(FillRec^))) and (Error = 0)) Do
      Begin
      If Not shWrite(F, FillRec^, SizeOf(FillRec^)) Then
        Error := MKFileError(f);
      End;
    End;
  If ((Error = 0) and (F^.FileSize < ToSize)) Then
    Begin
    If Not shWrite(F, FillRec^, ToSize - F^.FileSize) Then
      Error := MKFileError(f);
    End;
  If Error = 0 Then
    Begin
    F^.Close;
    Error := F^.IoResult;
    End;
  Dispose(FillRec);
  ExtendFile := Error;
  End;


Function  GetTempName(FN: String): String;
var Counter  : Byte;
    TempName : String;
    TempCH   : Char;
begin
  If Fn[Length(Fn)] <> BsChar Then
       begin
         FN[00] := Chr(Length(FN) + 01);
         Fn[Length(Fn)] := Bschar;
       end; { Add trailing backslash }

  Randomize;
  TempName[0] := #12;

  For Counter := 01 to 12 do
    begin
      If Counter=09 then TempName[Counter] := '.'
       else begin
              TempCH := #00;

              While NOT (TempCH in ['A'..'Z', 'a'..'z', '0'..'9']) do
                TempCH := Chr(Random(256));

              TempName[Counter] := TempCH;
            end;
    end; { create random name }

  GetTempName := FN + TempName;
end; { func. GetTempName }

Function  GetTextPos(Var f : Text) : LongInt;        { (c) by Arne de Bruijn }

{$IFDEF WIN32}
 {$IFNDEF FPC}
   {$IFDEF DELPHI}
     Type FileRec = System.TFileRec;
          TextRec = System.TTextRec;
   {$ELSE}
     Type FileRec = SysUtils.TFileRec;
          TextRec = SysUtils.TTextRec;
   {$ENDIF}
 {$ELSE}
   Type FileRec = Dos.FileRec;
        TextRec = Dos.TextRec;
 {$ENDIF}
{$ENDIF}


var SaveMode,
    SaveRecSize : Word;
    Temp_F      : File ABSOLUTE F;
begin
 SaveMode := FileRec((@F)^).Mode;
 SaveRecSize := FileRec((@F)^).RecSize;

 FileRec((@F)^).Mode:=fmInOut;                           { Set to binary mode }
        { (The (@F)^ part is to let TP 'forget' the type of the structure, so }
      { you can type-caste it to everything (note that with and without (@X)^ }
       { can give a different value, longint(bytevar) gives the same value as }
                      { bytevar, while longint((@bytevar)^) gives the same as }
         { longint absolute Bytevar (i.e. all 4 bytes in a longint are readed }
                              { from memory instead of 3 filled with zeros))) }

 FileRec((@F)^).RecSize := 01;                { Set record size to 1 (a byte) }

 {$IFNDEF FPK}
   GetTextPos := (FilePos(File((@F)^)) -
 {$ELSE}
   GetTextPos := (FilePos(Temp_F) -
 {$ENDIF}
                  TextRec(F).BufEnd) +
                  TextRec(F).BufPos;
      { Get the fileposition, subtract the already readed buffer, and add the }
                                                    { position in that buffer }

 TextRec(F).Mode := SaveMode;                         { Set back to text mode }
 TextRec(F).BufSize:= SaveRecSize;           { BufSize overwritten by RecSize }
end; { func. GetTextPos }

Function FindOnPath(FN: String; Var OutName: String): Boolean;
  Var
    TmpStr: String;

  Begin
  If FileExist(FN) Then
    Begin
    OutName := FExpand(FN);
    FindOnPath := True;
    End
  Else
    Begin
    TmpStr := FSearch(FN, GetEnv('Path'));
    If FileExist(TmpStr) Then
      Begin
      OutName := TmpStr;
      FindOnPath := True;
      End
    Else
      Begin
      OutName := FN;
      FindOnPath := False;
      End;
    End;
  End;


Function  CopyFile(FN1: String; FN2: String): Boolean;
  Type
    TmpBufType = Array[1..8192] of Byte;

  Var
    F1: File;
    F2: File;
    NumRead: NumReadType;
    Buf: ^TmpBufType;
    Error: Longint;

  Begin
  New(Buf);
  Error := 0;
  Assign(F1, FN1);
  FileMode := fmReadOnly + fmDenyNone;
  Reset(F1, 1);
  Error := IoResult;
  If Error = 0 Then
    Begin
    Assign(F2, FN2);
    FileMode := fmReadWrite + fmDenyNone;
    ReWrite(F2, 1);
    Error := IoResult;
    End;
  If Error = 0 Then
    Begin
    BlockRead(F1, Buf^, SizeOf(Buf^), NumRead);
    Error := IoResult;
    While ((NumRead <> 0) and (Error = 0)) Do
      Begin
      BlockWrite(F2, Buf^, NumRead);
      Error := IoResult;
      If Error = 0 Then
        Begin
        BlockRead(F1, Buf^, SizeOf(Buf^), NumRead);
        Error := IoResult;
        End;
      End;
    End;
  If Error = 0 Then
    Begin
    Close(F1);
    Error := IoResult;
    End;
  If Error = 0 Then
    Begin
    Close(F2);
    Error := IoResult;
    End;
  Dispose(Buf);
  CopyFile := (Error = 0);
  End;


Function  EraseFile(FN: String): Boolean;
  Var
    F: pFileObj;

  Begin
  New(F, Init);
  F^.Assign(fn);
  EraseFile := F^.Erase;
  Dispose(F, done);
  End;


Function  MakePath(FP: String): Boolean;
  Var
    i: Word;

  Begin
  If FP[Length(FP)] <> BsChar Then
    FP := FP + BsChar;
  If Not FileExist(FP + 'nul') Then
    Begin
    i := 2;
    While (i <= Length(FP)) Do
      Begin
      If FP[i] = Bschar Then
        Begin
        If FP[i-1] <> ':' Then
          Begin
          MkDir(Copy(FP, 1, i - 1));
          If IoResult <> 0 Then;
          End;
        End;
      Inc(i);
      End;
    End;
  MakePath := FileExist(FP + 'nul');
  End;

function  MkFileError(var F: pFileObj): Word;
begin
  if F <> nil then MkFileError := F^.IoResult
    else MkFileError := 0;
end; { func. mkFileError }

End.
