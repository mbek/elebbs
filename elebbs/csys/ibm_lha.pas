{ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป}
{บ      -- THIS FILE IS PART OF THE LIVESYSTEMS COMPRESSOR TOOLBOX. --      บ}
{บ          ALL RIGHTS RESERVED  (C) COPYRIGHTED G. HOOGTERP 1994           บ}
{บ                                                                          บ}
{บ             See the documentation for details on the license.            บ}
{บ                                                                          บ}
{ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

Unit IBM_LHA;
{$I COMPILER.INC}
Interface
Uses BSC;

Type
     LZHName       = String[120];
     LZHHeader     = Record
       Unk1        : Byte;
       Unk2        : Byte;
       Methode     : Array[1..5] Of Char;
       CompSize    : LongInt;
       RealSize    : LongInt;
       Time        : LongInt;
       Attr        : Byte;
       Update      : Byte;
       Name        : LZHName;
       Crc         : SmallWord;
     End;



Type LHAObject = Object(BasicCompressorObject)
       F           : File;
       Buf         : LZHHeader;

       Constructor LHAInit;
       Procedure FindFirstEntry;   Virtual;
       Procedure FindNextEntry;    Virtual;
       Procedure CheckProtection;  Virtual;
       Function IsThisTypeFile(Var B ;Size : Word):Boolean; Virtual;
     End; {Object}

     LHAPtr = ^LHAObject;

procedure InitLHA;

Implementation

Constructor LHAObject.LHAInit;
Begin
inherited Init;
Platform:=ID_IBM;
CompressorType:='LHA';
CompressorName:='LHArc/LA';
Magic:=LHA_Type;
End;


Procedure LHAObject.FindFirstEntry;
Begin
SetFileMode(ReadOnly+ShareDenyNone);
Assign(F,FileName);
Reset(F,1);
Seek(F,WhereInFile);

BlockRead(F,Buf,22,RR);
If RR<>22
   Then Begin
        Close(F);
        LastEntry:=True;
        ResetFileMode;
        Exit;
        End;

BlockRead(F,Buf.Name[1],Ord(Buf.Name[0]),RR);
BlockRead(F,Buf.CRC,2,RR);

If Not BeQuick
   Then Begin
        With Buf,IBM(Entry) Do
         Begin
         FileName        := Name;
         CompressedSize  := CompSize;
         OriginalSize    := RealSize;
         If (Attr and $10)=$10
            Then CompressionName:='<DIR>     '
            Else CompressionName := Methode;
         FileCRC         := HexWord(CRC)+'    ';
         FileDate        := TimeStamp(Time);
         ProtectedFile   := False;
         ContainsPaths   := (Pos('\',Name)>0) Or (Pos('/',Name)>0);
         SaveID          := '';
         End;
        End;

If Buf.Update>0
   Then Begin
        Inc(WhereInFile,3);
        CompressorName:='LHA';
        End;

WhereInFile:=WhereInFile+Buf.CompSize+(SizeOf(Buf)-120)+Length(Buf.Name);
Close(F);
ResetFileMode;
End;

Procedure LHAObject.FindNextEntry;
Begin
SetFileMode(ReadOnly+ShareDenyNone);
Assign(F,FileName);
Reset(F,1);
Seek(F,WhereInFile);

BlockRead(F,Buf,22,RR);
If RR<>22
   Then Begin
        Close(F);
        LastEntry:=True;
        ResetFileMode;
        Exit;
        End;

BlockRead(F,Buf.Name[1],Ord(Buf.Name[0]),RR);
BlockRead(F,Buf.CRC,2,RR);

If not BeQuick
   Then Begin
        With Buf,IBM(Entry) Do
         Begin
         FileName        := Name;
         CompressedSize  := CompSize;
         OriginalSize    := RealSize;
         If (Attr and $10)=$10
            Then CompressionName:='<DIR>     '
            Else CompressionName := Methode;
         FileCRC         := HexWord(CRC)+'    ';
         FileDate        := TimeStamp(Time);
         ContainsPaths   := (Pos('\',Name)>0) Or (Pos('/',Name)>0);
         ProtectedFile   := False;
         End;
        End;

WhereInFile:=WhereInFile+Buf.CompSize+(SizeOf(Buf)-120)+Length(Buf.Name);

If Buf.Update>0
   Then Begin
        Inc(WhereInFile,3);
        CompressorName:='LHA';
        End;

Close(F);
ResetFileMode;
End;

Procedure LHAObject.CheckProtection;
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


Function LHAObject.IsThisTypeFile(Var B ;Size : Word):Boolean;
Var Dum   : LongInt;
Begin
LHAInit;
IsThisTypeFile:=True;

If IsExeFile(B) or
   SearchBuffer(B,Size,0,1000,'LARC V',Dum)
   Then Begin
        SelfExtractor:=True;
        If SearchBuffer(B,Size,0,1000,'-lz',WhereInFile)
           Then Begin
                Dec(WhereInFile,2);
                Exit;
                End;
        If SearchBuffer(B,Size,0,2000,'-lh',WhereInFile)
           Then Begin
                Dec(WhereInFile,2);
                Exit;
                End;
        End;
WhereInFile:=0;

Move(LZHHeader(B),Buf,SizeOf(Buf));
{
For some reason SpeedPascal 1.5 doesn't like this. It does
work quite nicely with TP and VirtualPascal though. I guess
it's a bug in the compiler since there's no reason why it
shouldn't work.

Buf.Methode[4]:='?';
Buf.Methode[3]:='?';
If Buf.Methode='-l??-'
   Then Exit;
}

With Buf Do
 Begin
 If (Methode[1]='-') And
    (Methode[2]='l') And
    (Methode[5]='-')
    Then Exit;
 End;

IsThisTypeFile:=False;
End;

Var CO          : LHAPtr;

procedure InitlHA;
begin;
New(CO,LHAInit);
AddToList(CO);
end; { InitLha }

{$IFNDEF OVERLAY}
Begin
 InitLHA;
{$ENDIF}
End.


