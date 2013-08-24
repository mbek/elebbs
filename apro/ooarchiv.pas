{$S-,R-,V-,I-,B-,F+,O-,A-}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{*********************************************************}
{*                  OOARCHIV.PAS 2.03                    *}
{*        Copyright (c) TurboPower Software 1991.        *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OoArchiv;
  {-Objects and routines for working with archived files}

interface

uses
  Dos,
  {$IFDEF UseOpro}
  OpString,
  {$ENDIF}
  {$IFDEF UseTPro}
  TpString,
  {$ENDIF}
  ApMisc;

const
  {+++ archive (LZH and ZIP) option flags +++}
  arCreateDirs       = $0001;
  arReadArcComments  = $0002;
  arReadFileComments = $0004;
  arStripPath        = $0008;
  arCompressing      = $0100;
  arDeleting         = $0200;
  arNoDriveLetter    = $0400;
  arRemoveDots       = $0800;

  {+++ internal option flags +++}
  arReadExtraField   = $8000;

  DefArchiveOptions : Word = arRemoveDots;
  BadArchiveOptions : Word = 0;

type
  FileMaskNodePtr = ^FileMaskNode;
  FileMaskNode =
    object
      fmnNext   : FileMaskNodePtr;
      fmnDirPtr : StringPtr;
      fmnName   : NameStr;
      fmnExt    : ExtStr;

      constructor Init(FM : PathStr);
        {-Initialize the node}
      destructor Done; virtual;
        {-Destroy the node}
      function Match(Dir : DirStr; Name : NameStr; Ext : ExtStr) : Boolean;
        {-Return True if Name matches this file mask}

      {+++ internal methods +++}
    {#Z+}
      constructor fmnInitName(FM : PathStr);
    {#Z-}
    end;

  FileMaskListPtr = ^FileMaskList;
  FileMaskList =
    object
      fmlHead, fmlTail : FileMaskNodePtr;

      constructor Init;
        {-Initialize the list}
      destructor Done; virtual;
        {-Destroy the list}
      function Append(FM : PathStr) : Boolean;
        {-Add FM mask to the list}
      function Match(FName : PathStr) : Boolean;
        {-Return True if FName is in list of file masks}
      procedure SortFileMaskList; virtual;
        {-Sorts FML in ascending order}
      procedure ExpandFileMaskList;
        {-Expand all masks FML into complete file names}

      {+++ internal methods +++}
    {#Z+}
      function fmlAppendPrim(FM : PathStr; Expand : Boolean) : Boolean;
      function fmlAppendName(FM : PathStr) : Boolean;
    {#Z-}
    end;

  ArchivePtr = ^Archive;
  Archive =
    object
      arError   : Word;
      arFile    : File;
      arName    : PathStr;
      arOutPath : PathStr;
      arOptions : Word;

      constructor Init(FName : PathStr);
        {-Initialize the archive and open the input file}
      destructor Done; virtual;
        {-Destroy the archive and close the input file}

      function GetLastError : Word;
        {-Get the code for the last error}
      function GetFileName : PathStr;
        {-Get name of archive}

      procedure arOptionsOn(OptionFlags : Word);
        {-Activate multiple options}
      procedure arOptionsOff(OptionFlags : Word);
        {-Deactivate multiple options}
      function arOptionsAreOn(OptionFlags : Word) : Boolean;
        {-Return True if all specified options are on}

      procedure SetOutputPath(Path : PathStr);
        {-Set path for output files}

      procedure CreateOutputFile(var F : File; FName : PathStr);
        {-Create FName. If FName specifies a directory that doesn't exist,
          and arCreateDirs option is on, try to create it.}

      {+++ internal methods +++}
     {#Z+}
      function arActualName(FName : PathStr) : PathStr;
     {#Z-}
    end;

  function Percentage(V1, V2 : LongInt) : Byte;                        {!!.01}
    {-Returns the ratio of V1 to V2 * 100}                             {!!.01}

  {==========================================================================}

implementation

  constructor FileMaskNode.Init(FM : PathStr);
    {-Initialize the node}
  var
    I, Asterisk : Byte;
    Dir : DirStr;
  begin
    fmnNext := nil;

    {convert to uppercase}
    FM := StUpcase(FM);

    for I := 1 to Length(FM) do
      if FM[I] = '/' then
        FM[I] := '\';

    {split FM into Name, Ext}
    FSplit(FM, Dir, fmnName, fmnExt);
    fmnDirPtr := StringToHeap(Dir);
    if fmnDirPtr = nil then begin
      ArchiveStatus := ecOutOfMemory;
      Fail;
    end;

    {expand to ???????.??? format}
    if fmnName = '' then
      fmnName := '????????';
    if fmnExt = '' then
      fmnExt := '???'
    else if fmnExt = '.' then
      fmnExt := ''
    else
      Delete(fmnExt, 1, 1);

    Asterisk := Pos('*', fmnName);
    if Asterisk <> 0 then begin
      for I := Asterisk to 8 do
        fmnName[I] := '?';
      fmnName[0] := #8;
    end;

    Asterisk := Pos('*', fmnExt);
    if Asterisk <> 0 then begin
      for I := Asterisk to 3 do
        fmnExt[I] := '?';
      fmnExt[0] := #3;
    end;
  end;

  constructor FileMaskNode.fmnInitName(FM : PathStr);
    {-Initialize the name node}
  var
    Dir : DirStr;
    I : Byte;
  begin
    fmnNext := nil;

    {convert to uppercase}
    FM := StUpcase(FM);

    for I := 1 to Length(FM) do
      if FM[I] = '/' then
        FM[I] := '\';

    {split FM into Name, Ext}
    FSplit(FM, Dir, fmnName, fmnExt);
    fmnDirPtr := StringToHeap(Dir);
    if fmnDirPtr = nil then begin
      ArchiveStatus := ecOutOfMemory;
      Fail;
    end;

    if fmnExt <> '' then
      Delete(fmnExt, 1, 1);
  end;

  destructor FileMaskNode.Done;
    {-Destroy the node}
  begin
    DisposeString(fmnDirPtr);
  end;

  function FileMaskNode.Match(Dir : DirStr; Name : NameStr; Ext : ExtStr) : Boolean;
    {-Return True if Name matches this file mask}
  var
    I : Word;
  begin
    Match := False;

    {If Name or Ext is larger than the mask, we know it doesn't match}
    if (Length(Name) > Length(fmnName)) or
       (Length(Ext) > Length(fmnExt)) then
      Exit;

    {Exit if directories are different}
    if (fmnDirPtr^ <> '') and (fmnDirPtr^ <> Dir) then
      Exit;

    {Extend Name and Ext with characters guaranteed not to match}
    Name := PadCh(Name, #0, 8);
    Name[0] := #8;
    Ext := PadCh(Ext, #0, 3);
    Ext[0] := #3;

    {Compare Name to fmnName}
    for I := 1 to Length(fmnName) do
      if (fmnName[I] <> '?') then
        if (Name[I] <> fmnName[I]) then
          Exit;

    {Compare Ext to fmnExt}
    for I := 1 to Length(fmnExt) do
      if (fmnExt[I] <> '?') then
        if (Ext[I] <> fmnExt[I]) then
          Exit;

    {If we get here, we matched}
    Match := True;
  end;

  constructor FileMaskList.Init;
    {-Initialize the list}
  begin
    fmlHead := nil;
    fmlTail := nil;
  end;

  destructor FileMaskList.Done;
    {-Destroy the list}
  var
    FMNP, NP : FileMaskNodePtr;
  begin
    FMNP := fmlHead;
    while (FMNP <> nil) do begin
      NP := FMNP^.fmnNext;
      Dispose(FMNP, Done);
      FMNP := NP;
    end;

    fmlHead := nil;
    fmlTail := nil;
  end;

  function FileMaskList.Append(FM : PathStr) : Boolean;
    {-Add FM mask to the list}
  begin
    Append := fmlAppendPrim(FM, True);
  end;

  function FileMaskList.Match(FName : PathStr) : Boolean;
    {-Return True if Name is in list of file masks}
  var
    Dir : DirStr;
    Name : NameStr;
    Ext : ExtStr;
    FMNP : FileMaskNodePtr;
    I : Byte;
  begin
    {assume success}
    Match := True;
    if fmlHead = nil then
      Exit;

    for I := 1 to Length(FName) do
      if FName[I] = '/' then
        FName[I] := '\';

    {split FName into Name, Ext}
    FSplit(StUpcase(FName), Dir, Name, Ext);
    {if Ext <> '' then}
      {Delete(Ext, 1, 1);}
    if Ext = '.' then
      Ext := ''
    else if Ext <> '' then
      Delete(Ext, 1, 1);

    {search the list}
    FMNP := fmlHead;
    while (FMNP <> nil) do
      if FMNP^.Match(Dir, Name, Ext) then
        Exit
      else
        FMNP := FMNP^.fmnNext;

    {if we get here we failed}
    Match := False;
  end;

  procedure FileMaskList.SortFileMaskList;
    {-Sorts FML in ascending order}
  var
    CurHead : FileMaskNodePtr;
    Node : FileMaskNodePtr;
    Lowest : FileMaskNodePtr;
    LowDir : DirStr;
    LowName : NameStr;
    LowExt : ExtStr;
    Temp : FileMaskNode;
  begin
    CurHead := fmlHead;

    {Loop until all items in place}
    repeat
      Lowest := nil;
      LowDir := #255;
      LowName := #255;
      LowExt := #255;

      {Find the (next) lowest file name}
      Node := CurHead;
      repeat
        if (Node^.fmnDirPtr^ < LowDir) or
           ((Node^.fmnDirPtr^ = LowDir) and (Node^.fmnName < LowName)) or
           ((Node^.fmnDirPtr^ = LowDir) and (Node^.fmnName = LowName) and
           (Node^.fmnExt < LowExt)) then begin
          LowDir := Node^.fmnDirPtr^;
          LowName := Node^.fmnName;
          LowExt := Node^.fmnExt;
          Lowest := Node;
        end;
        Node := Node^.fmnNext;
      until Node = nil;

      {Swap the data in the lowest node with the data in CurHead}
      Move(CurHead^, Temp, SizeOf(FileMaskNode));
      CurHead^.fmnDirPtr := Lowest^.fmnDirPtr;
      CurHead^.fmnName := Lowest^.fmnName;
      CurHead^.fmnExt := Lowest^.fmnExt;
      Lowest^.fmnDirPtr := Temp.fmnDirPtr;
      Lowest^.fmnName := Temp.fmnName;
      Lowest^.fmnExt := Temp.fmnExt;

      {Move up CurHead}
      CurHead := CurHead^.fmnNext;
    until CurHead = nil;
  end;

  {!!.02 revised}
  procedure FileMaskList.ExpandFileMaskList;
    {-Expand all masks FML into complete file names in ExpandedFML}
  const
    AnyFileButDir = AnyFile and
      not (Directory or VolumeID or Hidden or SysFile);
  var
    Node : FileMaskNodePtr;
    NP : FileMaskNodePtr;
    FName : PathStr;
    SRec : SearchRec;
    SaveTail : FileMaskNodePtr;
    Finished : Boolean;
  label
    ErrorExit;
  begin
    ArchiveStatus := ecOk;
    Node := fmlHead;
    SaveTail := fmlTail;

    Finished := False;
    repeat
      with Node^ do begin
        {Setup the file name mask to search for}
        FName := fmnName + '.' + fmnExt;
        if fmnDirPtr^ <> '' then
          FName := AddBackSlash(fmnDirPtr^) + FName;

        {Find the first matching file}
        FindFirst(FName, AnyFileButDir, SRec);
        if DosError <> 0 then begin
          ArchiveStatus := ecFileNotFound;
          Exit;
        end;

        {Append file to new list for each match}
        while DosError = 0 do begin
          if not fmlAppendName(AddBackSlash(fmnDirPtr^)+Srec.Name) then begin
            ArchiveStatus := ecOutOfMemory;
            goto ErrorExit;
          end;
          FindNext(SRec);
        end;

      end;

      {Finished with this mask, continue with next}
      if Node = SaveTail then
        Finished := True
      else
        Node := Node^.fmnNext;

    until Finished;

    {Remove original mask nodes}
    Node := fmlHead;
    fmlHead := SaveTail^.fmnNext;

    Finished := False;
    repeat
      NP := Node^.fmnNext;
      Dispose(Node, Done);
      if NP = fmlHead then
        Finished := True
      else
        Node := NP;
    until Finished;
    Exit;

ErrorExit:
    {Dispose of all nodes after SaveTail}
    Node := SaveTail;
    while (Node <> nil) do begin
      NP := Node^.fmnNext;
      Dispose(Node, Done);
      Node := NP;
    end;
    fmlTail := SaveTail;
  end;

  function FileMaskList.fmlAppendPrim(FM : PathStr; Expand : Boolean) : Boolean;
    {-Add FM to the list}
  var
    FMNP : FileMaskNodePtr;
  begin
    if Expand then
      New(FMNP, Init(FM))
    else
      New(FMNP, fmnInitName(FM));
    if FMNP = nil then
      fmlAppendPrim := False
    else begin
      if fmlHead = nil then begin
        fmlHead := FMNP;
        fmlTail := FMNP;
      end
      else begin
        fmlTail^.fmnNext := FMNP;
        fmlTail := FMNP;
      end;
      fmlAppendPrim := True;
    end;
  end;

  function FileMaskList.fmlAppendName(FM : PathStr) : Boolean;
    {-Add FM name to the list}
  begin
    fmlAppendName := fmlAppendPrim(FM, False);
  end;

  {!!.03 removed FileMode changes that were added in 2.03}
  constructor Archive.Init(FName : PathStr);
    {-Initialize the archive and open the input file}
  begin
    arError := 0;
    arName := FName;
    arOutPath := '';
    arOptions := DefArchiveOptions;

    {try to open the file}
    Assign(arFile, FName);
    Reset(arFile, 1);
    ArchiveStatus := IoResult;
    if ArchiveStatus <> 0 then
      Fail;
  end;

  destructor Archive.Done;
    {-Destroy the archive and close the input file}
  begin
    if FileRec(arFile).Mode = fmInOut then begin
      Close(arFile);
      if IoResult <> 0 then {};
    end;
  end;

  function Archive.GetLastError : Word;
    {-Get the code for the last error}
  begin
    GetLastError := arError;
    arError := 0;
  end;

  function Archive.GetFileName : PathStr;
    {-Get name of archive}
  begin
    GetFileName := arName;
  end;

  procedure Archive.arOptionsOn(OptionFlags : Word);
    {-Activate multiple options}
  begin
    arOptions := arOptions or (OptionFlags and not BadArchiveOptions);
  end;

  procedure Archive.arOptionsOff(OptionFlags : Word);
    {-Deactivate multiple options}
  begin
    arOptions := arOptions and not (OptionFlags and not BadArchiveOptions);
  end;

  function Archive.arOptionsAreOn(OptionFlags : Word) : Boolean;
    {-Return true if all specified options are on}
  begin
    arOptionsAreOn := (arOptions and OptionFlags = OptionFlags);
  end;

  procedure Archive.SetOutputPath(Path : PathStr);
    {-Set path for output files}
  begin
    arOutPath := AddBackSlash(Path);
  end;

  function Archive.arActualName(FName : PathStr) : PathStr;
    {-Return the name that will actually be used for the specified output file}
  var
    I : Word;
  begin
    for I := 1 to Length(FName) do
      if FName[I] = '/' then
        FName[I] := '\';
    if not arOptionsAreOn(arCreateDirs) then
      FName := JustFileName(FName)                                     {!!.01}
    else if FName[1] = '\' then                                        {!!.01}
      System.Delete(FName, 1, 1);                                      {!!.01}
    arActualName := arOutPath+FName;
  end;

  procedure Archive.CreateOutputFile(var F : File; FName : PathStr);
    {-Create FName. If FName specifies a directory that doesn't exist, and
      arCreateDirs option is on, try to create it.}
  var
    I : Word;
    P, N : PathStr;
    PLen : Byte absolute P;
    Finished : Boolean;
  begin
    Finished := False;
    P := '';
    N := FName;
    repeat
      {try to create output file}
      Assign(F, FName);
      Rewrite(F, 1);
      arError := IoResult;
      if (arError <> 3) or not arOptionsAreOn(arCreateDirs) then
        Finished := True
      else repeat
        I := Pos('\', N);
        if I = 0 then
          Finished := True
        else begin
          P := AddBackslash(P)+Copy(N, 1, I-1);
          Delete(N, 1, I);
          if (PLen = 0) or (P[PLen] = ':') then
            P := P+'\'
          else begin
            I := 0;
            MkDir(P);
            arError := IoResult;
            Finished := (arError <> 0) and (arError <> 5);
          end;
        end;
      until (I = 0);
    until Finished;
  end;

  {!!.01}
  function Percentage(V1, V2 : LongInt) : Byte;
    {-Returns the ratio of V1 to V2 * 100}
  var
    Ratio : Byte;
  begin
    if V2 > 16384000 then begin  {Possible LongInt overflow}
      V1 := (V1 + $80) shr 8;  {scale down (div 256)}
      V2 := (V2 + $80) shr 8;  {scale down (div 256)}
    end;
    if V2 <= 0 then
      Ratio := 0
    else
      Ratio := (V1 * 100) div V2;
    if Ratio > 100 then
      Ratio := 100;
    Percentage := Ratio;
  end;

end.
