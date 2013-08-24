{ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป}
{บ      -- THIS FILE IS PART OF THE LIVESYSTEMS COMPRESSOR TOOLBOX. --      บ}
{บ          ALL RIGHTS RESERVED  (C) COPYRIGHTED G. HOOGTERP 1994           บ}
{บ                                                                          บ}
{บ             See the documentation for details on the license.            บ}
{บ                                                                          บ}
{ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

Unit IBM_ARJ;
{$I COMPILER.INC}
Interface
Uses BSC;

Const BufferSize    = 34;

Type Buffer         = Array[1..34] Of Byte;
     NameBuffer     = Array[1..255] Of Char;
     MainHeaderType = Record
        ID           : SmallWord;
        BasSize      : SmallWord;
        FirstSize    : Byte;
        Version      : Byte;
        MinExtr      : Byte;
        HostOS       : Byte;
        ARJflags     : Byte;
        Res1         : Byte;
        FileType     : Byte; { 2=Comment }
        Res2         : Byte;
        Time         : SmallWord;
        Date         : SmallWord;
        Res3         : LongInt;
        Res4         : LongInt;
        Res5         : LongInt;
        SpecPos      : SmallWord;
        NotUsed1     : SmallWord;
        NotUsed2     : SmallWord;
      End;
      LocalHeaderType  = Record
        ID             : SmallWord;
        BasSize        : SmallWord;
        FirstSize      : Byte;
        Version        : Byte;
        MinExtr        : Byte;
        HostOS         : Byte;
        ARJflags       : Byte;
        Methode        : Byte;
        FileType       : Byte; { 2=Comment }
        Res2           : Byte;
        Time           : Longint;
        CompSize       : LongInt;
        RealSize       : LongInt;
        CRCLo          : SmallWord;
        CRCHi          : SmallWord;
        SpecPos        : SmallWord;
        AccMode        : SmallWord;
        HostData       : SmallWord;
      End;


Type ARJObject = Object(BasicCompressorObject)
       F     : File;
       Buf   : Buffer;
       AName : NameBuffer;
       ARJMethodes: Array[0..4] of String[10];

       Constructor ARJInit;
       Procedure FindFirstEntry;   Virtual;
       Procedure FindNextEntry;    Virtual;
       Procedure CheckProtection;  Virtual;
       Function IsThisTypeFile(Var B ;Size : Word):Boolean; Virtual;
     End; {Object}

     ArjPtr = ^ArjObject;

procedure InitARJ;

Implementation



Constructor ArjObject.ARJInit;
Begin
inherited Init;
Platform:=ID_IBM;
CompressorType:='ARJ';
CompressorName:='ARJ';
Magic:=ARJ_Type;
ARJMethodes[0] := 'Stored    ';
ARJMethodes[1] := 'ARJ 1     ';
ARJMethodes[2] := 'ARJ 2     ';
ARJMethodes[3] := 'ARJ 3     ';
ARJMethodes[4] := 'ARJ 4     ';
End;


Procedure ARJObject.FindFirstEntry;
Var Extend : SmallWord;
    Dum    : LongInt;
    MainHeaderTypeBuf: MainHeaderType ABSOLUTE buf;
    LocalheaderTypeBuf: LocalHeaderType ABSOLUTE buf;
Begin
SetFileMode(ReadOnly+ShareDenyNone);
Assign(F,FileName);
Reset(F,1);
Seek(F,WhereInFile);

BlockRead(F,Buf,BufferSize,RR);
If RR<>BufferSize
   Then Begin
        Close(F);
        ResetFileMode;
        Exit;
        End;

With MainHeaderTypeBuf Do
 Begin
 ProtectedFile := IsBitSet(ARJFlags,$02) Or
                  IsBitSet(ARJFlags,$40);
 UnpackVersion  := LocalHeaderTypeBuf.MinExtr;
 WhereInFile:=WhereInFile+BasSize+10;
 Seek(F,WhereInFile);
 End;

BlockRead(F,Buf,BufferSize,RR);
If RR<>BufferSize
   Then Begin
        Close(F);
        ResetFileMode;
        Exit;
        End;

If LocalHeaderTypeBuf.BasSize=0
   Then Begin
        LastEntry:=True;
        ResetFileMode;
        Close(F);
        Exit;
        End;

If Not BeQuick
   Then Begin
        With IBM(Entry),LocalHeaderTypeBuf Do
         Begin
         Fillchar(AName,SizeOf(AName),#00);
         BlockRead(F,AName,BasSize-FirstSize,RR);
         FileName       := Asciiz2String(AName);
         ContainsPaths  := Pos('/',AName)>0;
         OriginalSize   := RealSize;
         CompressedSize := CompSize;
         CompressionName:= ARJMethodes[Methode];
         FileCRC        := HexWord(CRCHi) + HexWord(CRCLo);
         FileDate       := TimeStamp(Time);
         If ProtectedFile
            Then SaveID := '-SE'
            Else SaveID := '';
          End; {With}
         End;

Seek(F,WhereInFile+LocalHeaderTypeBuf.BasSize+4);

BlockRead(F,Dum,4,RR);

BlockRead(F,Extend,2,RR);
If Extend>0
   Then WhereInFile:=WhereInFile+Extend;

WhereInFile:=FilePos(F)+LocalHeaderTypeBuf.CompSize;

Close(F);
ResetFileMode;
End;


Procedure ARJObject.FindNextEntry;
VAR Extend : SmallWord;
    Dum    : LongInt;
    LocalheaderTypeBuf: LocalHeaderType ABSOLUTE buf;
Begin
SetFileMode(ReadOnly+ShareDenyNone);
Assign(F,FileName);
Reset(F,1);
Seek(F,WhereInFile);

BlockRead(F,Buf,BufferSize,RR);
If (RR<>BufferSize) Or
   (LocalHeaderTypeBuf.BasSize=0)
   Then Begin
        LastEntry:=True;
        Close(F);
        ResetFileMode;
        Exit;
        End;

If Not BeQuick
   Then Begin
        With IBM(Entry),LocalHeaderTypeBuf Do
         Begin
         Fillchar(AName,SizeOf(AName),#00);
         BlockRead(F,AName,BasSize-FirstSize,RR);
         FileName       := Asciiz2String(AName);
         ContainsPaths  := Pos('/',AName)>0;
         OriginalSize   := RealSize;
         CompressedSize := CompSize;
         CompressionName:= ARJMethodes[Methode];
         FileCRC        := HexWord(CRCHi) + HexWord(CRCLo);
         FileDate       := TimeStamp(Time);
         If ProtectedFile
            Then SaveID := '-SE'
            Else SaveID := '';
         If LocalHeaderTypeBuf.MinExtr>UnpackVersion
            Then UnpackVersion  := LocalHeaderTypeBuf.MinExtr;
         End; {With}
         End;

Seek(F,WhereInFile+LocalHeaderTypeBuf.BasSize+4);

BlockRead(F,Dum,4,RR);

BlockRead(F,Extend,2,RR);
If Extend>0
   Then WhereInFile:=WhereInFile+Extend;

WhereInFile:=FilePos(F)+LocalHeaderTypeBuf.CompSize;

Close(F);
ResetFileMode;
End;


Procedure ARJObject.CheckProtection;
Var Old : LongInt;
Begin
Old:=WhereInFile;
BeQuick:=True;
FindFirstEntry;
BeQuick:=False;
WhereInFile:=Old;
LastEntry:=False;
End;

Function ARJObject.IsThisTypeFile(Var B ;Size : Word):Boolean;
Type TC = Array[0..$FFF0] of Byte;
Var  Test : Record
       ID : SmallWord;
       B  : SmallWord;
       F  : Byte;
       V  : Byte;
     End;
Begin
ARJInit;
IsThisTypeFile:=True;

If IsExeFile(B)
   Then Begin
        SelfExtractor:=True;
        If Not SearchBuffer(B,Size,6000,6300,#$60#$EA,WhereInFile)
           Then Begin
                If Not SearchBuffer(B,Size,14000,15000,#$60#$EA,WhereInFile)
                   Then Begin
                        If Not SearchBuffer(B,Size,10950,17000,#$60#$EA,WhereInFile)
                           Then WhereInFile:=0;
                        End;
                End;
        If WhereInFile>0
           Then Begin
                Move(TC(B)[WhereInFile],Test,6);
                With Test Do
                  If (ID=$EA60) And
                     (B<2900) {And
                     ((B-F)<256)}
                     Then Exit;
                End;
        End;
WhereInFile:=0;

Move(TC(B)[0],Test,6);
   With Test Do
     If (ID=$EA60) And
        (B<2900) {And
        ((B-F)<256) }
        Then Exit;

IsThisTypeFile:=False;
End;

Var CO          : ARJPtr;

procedure InitArj;
begin
New(CO,ARJInit);
AddToList(CO);
end; { proc. InitArj }

{$IFNDEF OVERLAY}
Begin
 InitArj;
{$ENDIF}
End.

