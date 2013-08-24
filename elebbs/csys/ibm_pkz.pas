{ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป}
{บ      -- THIS FILE IS PART OF THE LIVESYSTEMS COMPRESSOR TOOLBOX. --      บ}
{บ          ALL RIGHTS RESERVED  (C) COPYRIGHTED G. HOOGTERP 1994           บ}
{บ                                                                          บ}
{บ             See the documentation for details on the license.            บ}
{บ                                                                          บ}
{ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

Unit IBM_PKZ;
{$I COMPILER.INC}
Interface
Uses BSC;

Const LocalHeaderSize = 26;
      BegCentrDirSize = 42;
      EndCentrDirSize = 18;
      BufferSize      = 42;

Type  Buffer          = Array[1..42] Of Byte;
      NameBuffer      = Array[0..255] Of Char;
      LocalHeader     = Record
        Version       : SmallWord;
        GenBits       : SmallWord;
        Methode       : SmallWord;
        Time          : Longint;
        CrcLo         : SmallWord;
        CrcHi         : SmallWord;
        CompSize      : LongInt;
        RealSize      : LongInt;
        NameLen       : SmallWord;
        ExtraLen      : SmallWord;
        Fill          : Array[1..16] Of Char;
      End;
      StartCentralDir = Record
        VersionUsed   : SmallWord;
        VersionNeeded : SmallWord;
        GenBits       : SmallWord;
        Meth          : SmallWord;
        Time          : LongInt;
        CRC           : Longint;
        CompSize      : LongInt;
        RealSize      : LongInt;
        NameLen       : SmallWord;
        ExtraLen      : SmallWord;
        CommLen       : SmallWord;
        DiskStart     : SmallWord;
        IntAttr       : SmallWord;
        ExtAttr       : LongInt;
        LocHeadOfs    : LongInt;
      End;
      EndCentralDir   = Record
        DiskNr        : SmallWord;
        SOCDdiskNr    : SmallWord;
        CDDiskCount   : SmallWord;
        CDTotCount    : SmallWord;
        CDSize        : LongInt;
        CDOfs         : LongInt;
        ZipComment    : SmallWord;
        Fill          : Array[1..24] Of Char;
      End;


Type PkZipObject = Object(BasicCompressorObject)
       F           : File;
       Buf         : Buffer;
       AName       : NameBuffer;
       HighVersion : SmallWord;
       ZipMethodes : Array[0..8] of NameString;

       Constructor ZIPInit;
       Procedure FindFirstEntry;   Virtual;
       Procedure FindNextEntry;    Virtual;
       Procedure CheckProtection;  Virtual;
       Function IsThisTypeFile(Var B; Size : Word):Boolean; Virtual;
     End; {PkzipObject}

     PkzipPtr = ^PkzipObject;

procedure InitZIP;

Implementation


Constructor PkzipObject.ZIPInit;
Begin
inherited Init;
Platform:=ID_IBM;
CompressorType:='ZIP2';
CompressorName:='PK(un)zip';
Magic:=ZIP_Type;
ZipMethodes[0] := 'Stored    ';
ZipMethodes[1] := 'Shrunk    ';
ZipMethodes[2] := 'Reduced 1 ';
ZipMethodes[3] := 'Reduced 2 ';
ZipMethodes[4] := 'Reduced 3 ';
ZipMethodes[5] := 'Reduced 4 ';
ZipMethodes[6] := 'Imploded  ';
ZipMethodes[7] := 'Tokenized ';
ZipMethodes[8] := 'DeflateN  ';
End;



Procedure PkzipObject.FindFirstEntry;
Var HeaderID : LongInt;
    LocalHeaderBuf: LocalHeader ABSOLUTE buf;
Begin
HighVersion:=0;
SetFileMode(ReadOnly+ShareDenyNone);
Assign(F,FileName);
Reset(F,1);
Seek(F,WhereInFile);
BlockRead(F,HeaderID,4,RR);
If RR<>4
   Then Begin
        Close(F);
        LastEntry:=True;
        ResetFileMode;
        Exit;
        End;

BlockRead(F,Buf,BufferSize,RR);
If RR<>BufferSize
   Then Begin
        Close(F);
        LastEntry:=True;
        ResetFileMode;
        Exit;
        End;

Case (HeaderID Shr 16) of
 $0403    : Begin
            If Not BeQuick
               Then Begin
                    Seek(F,WhereInFile+LocalHeaderSize+4);
                    FillChar(AName,SizeOf(AName),#00);
                    BlockRead(F,AName,LocalHeaderBuf.NameLen);

                    With IBM(Entry),LocalHeaderBuf Do
                     Begin
                     FileName       := Asciiz2String(AName);
                     ContainsPaths  := Pos('/',FileName)>0;
                     OriginalSize   := RealSize;
                     CompressedSize := CompSize;
                     CompressionName:= ZipMethodes[Methode];

                     FileCRC        := HexWord(CRCHi) + HexWord(CRCLo);

                     FileDate       := TimeStamp(Time);
                     If ProtectedFile
                        Then SaveID := '-AV'
                        Else SaveID := '';
                     End; {With}
                     End;
            If LocalHeaderBuf.Version>HighVersion
               Then HighVersion:=LocalHeaderBuf.Version;
            With LocalHeaderBuf Do
             WhereInFile:=WhereInFile+4+LocalHeaderSize+NameLen+ExtraLen+CompSize;
            End;
 $0201    : LastEntry:=True;
 $0605    : LastEntry:=True;
 Else
End;
Close(F);
ResetFileMode;
End;

Procedure PkzipObject.FindNextEntry;
Var HeaderID : LongInt;
    ExtraTag : SmallWord;
    LocalHeaderBuf: LocalHeader ABSOLUTE buf;
    StartCentralDirBuf: StartCentralDir ABSOLUTE buf;
Begin
SetFileMode(ReadOnly+ShareDenyNone);
Assign(F,FileName);
Reset(F,1);
Seek(F,WhereInFile);
BlockRead(F,HeaderID,4,RR);
If RR<>4
   Then Begin
        Close(F);
        ResetFileMode;
        LastEntry:=True;
        Exit;
        End;

BlockRead(F,Buf,BufferSize,RR);
If RR<>BufferSize
   Then Begin
        Close(F);
        LastEntry:=True;
        ResetFileMode;
        Exit;
        End;

Case (HeaderID Shr 16) of
 $0403    : Begin { Local Header Block }
            If Not BeQuick
               Then Begin
                    Seek(F,WhereInFile+LocalHeaderSize+4);
                    FillChar(AName,SizeOf(AName),#00);
                    BlockRead(F,AName,LocalHeaderBuf.NameLen);
                    With IBM(Entry),LocalHeaderBuf Do
                     Begin
                     FileName       := Asciiz2String(AName);
                     OriginalSize   := RealSize;
                     CompressedSize := CompSize;
                     CompressionName:= ZipMethodes[Methode];
                     FileCRC        := HexWord(CRCHi) + HexWord(CRCLo);
                     FileDate       := TimeStamp(Time);
                     If ProtectedFile
                        Then SaveID := '-AV'
                        Else SaveID := '';
                     Extra          := '';
                     End; {With}
                     End;
            If LocalHeaderBuf.Version>HighVersion
               Then HighVersion:=LocalHeaderBuf.Version;
            With LocalHeaderBuf Do
             WhereInFile:=WhereInFile+4+LocalHeaderSize+NameLen+ExtraLen+CompSize;
            End;
 $0201    : Begin { Central Dir Block }
            With StartCentralDirBuf Do
             Begin
             UnpackVersion:=VersionNeeded;
             CompressorName:='PK(un)zip '+Nr2Str(VersionNeeded,2);
             Insert('.',CompressorName,Length(CompressorName));
             If ExtraLen>0
                Then Begin
                     Seek(F,WhereInFile+BegCentrDirSize+NameLen+4);
                     BlockRead(F,ExtraTag,2,RR);
                     ProtectedFile:=ExtraTag=7;
                     End;
             End;
            LastEntry:=True;
            End;
 Else       LastEntry:=True;
End;
Close(F);
ResetFileMode;
End;

Procedure PkzipObject.CheckProtection;
Var Old : LongInt;
Begin
Old:=WhereInFile;
BeQuick:=True;
FindFirstEntry;
While Not LastEntry Do
 FindNextEntry;
BeQuick:=False;
WhereInFile:=Old;
LastEntry:=False;
If HighVersion=20
   Then CompressorType:='ZIP2'
   Else CompressorType:='ZIP1';
End;


Function PkZipObject.IsThisTypeFile(Var B ;Size : Word):Boolean;
Begin
ZIPInit;
IsThisTypeFile:=True;

If IsExeFile(B)
   Then Begin
        SelfExtractor:=True;
        If SearchBuffer(B,Size,14000,16000,'PK'#03#04,WhereInFile) Then Exit;
        End;
WhereInFile:=0;

If LongInt(B) =$04034B50
   Then Exit;
IsThisTypeFile:=False;
End;

Var CO          : PKZipPtr;

procedure InitZIP;
begin
New(CO,ZIPInit);
AddToList(CO);
end; { proc. InitZIP }

{$IFNDEF OVERLAY}
Begin
 InitZIP;
{$ENDIF}
End.

