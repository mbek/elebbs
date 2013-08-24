{ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป}
{บ      -- THIS FILE IS PART OF THE LIVESYSTEMS COMPRESSOR TOOLBOX. --      บ}
{บ          ALL RIGHTS RESERVED  (C) COPYRIGHTED G. HOOGTERP 1994           บ}
{บ                                                                          บ}
{บ             See the documentation for details on the license.            บ}
{บ                                                                          บ}
{ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

Unit IBM_RAR;
{$I COMPILER.INC}
Interface
Uses BSC;




Type HeaderType   = Record
       CRC        : SmallWord;
       Typ        : Byte;
       Flags      : SmallWord;
       Size       : SmallWord;
     End;

     ArcHeader    = Record
      Res1        : SmallWord;
      Res2        : LongInt;
     End;

     FileHeader   = Record
      PackSize    : LongInt;
      UnpSize     : LongInt;
      HostOS      : Byte;
      CRClo       : SmallWord;
      CRChi       : SmallWord;
      FileDate    : LongInt;
      UnpVer      : Byte;
      Methode     : Byte;
      NameSize    : SmallWord;
      Attr        : LongInt;
     End;

     CommHeader   = Record
      UnpSize     : SmallWord;
      UnpVer      : Byte;
      Methode     : Byte;
      CommCrc     : SmallWord;
     End;
     NameBuffer   = Array[1..255] of Char;


Type RARObject = Object(BasicCompressorObject)
       F        : File;
       Header   : HeaderType;
       ArchHead : ArcHeader;
       CommHead : CommHeader;
       FileHead : FileHeader;
       AName    : NameBuffer;
       RARMethodes: Array[$30..$35] of String[10];

       Constructor RARInit;
       Procedure FindFirstEntry;   Virtual;
       Procedure FindNextEntry;    Virtual;
       Procedure CheckProtection;  Virtual;
       Function IsThisTypeFile(Var B ;Size : Word):Boolean; Virtual;
     End; {Object}

     RARPtr = ^RARObject;

procedure InitRAR;

Implementation

Constructor RARObject.RARInit;
Begin
inherited Init;
Platform:=ID_IBM;
CompressorType:='RAR';
CompressorName:='RAR';
Magic:=RAR_Type;  { A unique number within the toolbox }
RARMethodes[$30] := 'Storing   ';
RARMethodes[$31] := 'Fastest   ';
RARMethodes[$32] := 'Fast      ';
RARMethodes[$33] := 'Normal    ';
RARMethodes[$34] := 'Good      ';
RARMethodes[$35] := 'Best      ';
End;



Procedure RARObject.FindFirstEntry;
Var  Stop     : Boolean;
     AddSize  : LongInt;
Begin

SetFileMode(ReadOnly+ShareDenyNone);
Assign(F,FileName);
Reset(F,1);

Stop:=False;
Repeat
 Seek(F,WhereInFile);
 BlockRead(F,Header,SizeOf(Header),RR);
 If RR<>SizeOf(Header)
    Then Begin
         Close(F);
         LastEntry:=True;
         ResetFileMode;
         Exit;
         End;

 If (Header.Typ=$74)
    Then WhereInFile:=WhereInFile+Header.Size
    Else Begin
         If (Header.Flags And $8000) = 0
            Then WhereInFile:=WhereInFile+Header.Size
            Else Begin
                 BlockRead(F,AddSize,4,RR);
                 WhereInFile:=WhereInFile+Header.Size+AddSize;
                 End;
         End;

 Case Header.Typ of
   $73 : Begin
         SolidArchive  := IsBitSet(Header.Flags,$0008);
         ProtectedFile := IsBitSet(Header.Flags,$0020);
         Locked        := IsBitSet(Header.Flags,$0004);
         If FileExtra<>''
             Then Dec(FileExtra[0],2);
         End;
   $74 : Begin
         BlockRead(F,FileHead,SizeOf(FileHead),RR);
         Stop:=True;
         WhereInFile:=WhereInFile+FileHead.PackSize;
         If Not BeQuick
            Then Begin
                 With IBM(Entry) Do
                  Begin
                  Fillchar(AName,SizeOf(AName),#00);
                  BlockRead(F,AName,FileHead.NameSize,RR);
                  FileName       :=Asciiz2String(AName);
                  ContainsPaths  :=Pos('/',FileName)>0;
                  OriginalSize   :=FileHead.UnpSize;
                  CompressedSize :=FileHead.PackSize;
                  If (FileHead.Attr And $10)=$10
                     Then CompressionName:='<DIR>     '
                     Else CompressionName:=RARMethodes[FileHead.Methode];
                  FileCRC:=HexWord(FileHead.CRChi)+HexWord(FileHead.CRClo);
                  FileDate:=TimeStamp(FileHead.FileDate);
                  If ProtectedFile
                     Then SaveID:='-SE'
                     Else SaveID:='';
                  End;
                 End;
         UnpackVersion:=FileHead.UnpVer;
         HasPassword:=IsBitSet(Header.Flags,$0004);
         End;

End; {Case}

Until Stop;
Close(F);
ResetFileMode;
End;

Procedure RARObject.FindNextEntry;
var Stop     : Boolean;
    AddSize  : LongInt;
Begin
SetFileMode(ReadOnly+ShareDenyNone);
Assign(F,FileName);
Reset(F,1);
Seek(F,WhereInFile);

Stop:=False;
Repeat
 Seek(F,WhereInFile);
 BlockRead(F,Header,SizeOf(Header),RR);
 If RR<>SizeOf(Header)
    Then Begin
         Close(F);
         LastEntry:=True;
         ResetFileMode;
         Exit;
         End;

 If (Header.Typ=$74)
    Then WhereInFile:=WhereInFile+Header.Size
    Else Begin
         If (Header.Flags And $8000) = 0
            Then WhereInFile:=WhereInFile+Header.Size
            Else Begin
                 BlockRead(F,AddSize,4,RR);
                 WhereInFile:=WhereInFile+Header.Size+AddSize;
                 End;
         End;


 If Header.Typ=$74
    Then Begin
         BlockRead(F,FileHead,SizeOf(FileHead),RR);
         Stop:=True;
         WhereInFile:=WhereInFile+FileHead.PackSize;
         If Not BeQuick
            Then Begin
                 With IBM(Entry) Do
                  Begin
                  Fillchar(AName,SizeOf(AName),#00);
                  BlockRead(F,AName,FileHead.NameSize,RR);
                  FileName       :=Asciiz2String(AName);
                  ContainsPaths  :=(Pos('/',FileName)>0) or
                                   (Pos('\',FileName)>0);
                  OriginalSize   :=FileHead.UnpSize;
                  CompressedSize :=FileHead.PackSize;
                  If (FileHead.Attr And $10)=$10
                     Then CompressionName:='<DIR>     '
                     Else CompressionName:=RARMethodes[FileHead.Methode];
                  FileCRC:=HexWord(FileHead.CRChi)+HexWord(FileHead.CRClo);
                  {FileCRC:=HexLong(FileHead.FCRC);}
                  FileDate:=TimeStamp(FileHead.FileDate);
                  If ProtectedFile
                     Then SaveID:='-SE'
                     Else SaveID:='';
                  If FileHead.UnpVer>UnpackVersion
                     Then UnpackVersion:=FileHead.UnpVer;
                  End;
                 End;
         HasPassword:=IsBitSet(Header.Flags,$0004);
         If FileHead.UnpVer>UnpackVersion
            Then UnpackVersion:=FileHead.UnpVer;
         End;
Until Stop;

Close(F);
ResetFileMode;
End;

Procedure RARObject.CheckProtection;
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
End;

Function RARObject.IsThisTypeFile(Var B ;Size : Word):Boolean;
Begin
RARInit;  { Reinit the current object }
IsThisTypeFile:=True;
WhereInFile:=0;

With HeaderType(B) Do
 If (CRC=$6152)   And
    (Typ=$72)     And
    (Flags=$1A21) And
    (Size=$007)
    Then Exit;

If IsExeFile(B)
   Then Begin
        SelfExtractor:=True;
        If SearchBuffer(B,Size,6000,7500,#$52#$61#$72#$21#$1A#$07#$00,WhereInFile)
           Then Exit;
        If SearchBuffer(B,Size,9000,9500,#$52#$61#$72#$21#$1A#$07#$00,WhereInFile)
           Then Exit;
        End;

IsThisTypeFile:=False;
End;



Var CO          : RARPtr;

procedure InitRAR;
begin
New(CO,RARInit);     { Create an instance of this object                 }
AddToList(CO);       { Add it to the list of available compressorobjects }
end; { proc. InitRAR }

{$IFNDEF OVERLAY}
Begin
 InitRAR;
{$ENDIF}
End.

