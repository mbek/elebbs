{ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป}
{บ      -- THIS FILE IS PART OF THE LIVESYSTEMS COMPRESSOR TOOLBOX. --      บ}
{บ          ALL RIGHTS RESERVED  (C) COPYRIGHTED G. HOOGTERP 1994           บ}
{บ                                                                          บ}
{บ             See the documentation for details on the license.            บ}
{บ                                                                          บ}
{ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

Unit IBM_ARC;
{$I COMPILER.INC}

Interface
Uses BSC;

Type LocalHeader = Record
       Mark      : Byte;
       Version   : Byte;
       Name      : Array[1..13] Of Char;
       CompSize  : LongInt;
       Date      : SmallWord;
       Time      : SmallWord;
       Crc       : SmallWord;
       RealSize  : LongInt;
     End;


Type ARCObject = Object(BasicCompressorObject)
       F           : File;
       Buf         : LocalHeader;
       ArcMethodes : Array[1..11] Of String[10];

       Constructor ARCInit;
       Procedure FindFirstEntry;   Virtual;
       Procedure FindNextEntry;    Virtual;
       Procedure CheckProtection;  Virtual;
       Function IsThisTypeFile(Var B ;Size : Word):Boolean; Virtual;
     End; {Object}

     ARCPtr = ^ARCObject;

procedure InitARC;

Implementation


Constructor ARCObject.ARCInit;
Begin
inherited Init;
Platform:=ID_IBM;
CompressorType:='ARC';
CompressorName:='ARC/PAK/ARC7';
Magic:=ARC_Type; { Unique number }
ArcMethodes[1] := 'Stored    ';
ArcMethodes[2] := 'Stored    ';
ArcMethodes[3] := 'Packed    ';
ArcMethodes[4] := 'Squeezed  ';
ArcMethodes[5] := 'Crunched  ';
ArcMethodes[6] := 'Crunched  ';
ArcMethodes[7] := 'Crunched  ';
ArcMethodes[8] := 'Crunched  ';
ArcMethodes[9] := 'Squased   ';
ArcMethodes[10] := 'Crushed   ';
ArcMethodes[11] := 'Distill   ';
End;



Procedure ARCObject.FindFirstEntry;
Begin
SetFileMode(ReadOnly+ShareDenyNone);
Assign(F,FileName);
Reset(F,1);
Seek(F,WhereInFile);

BlockRead(F,Buf,SizeOf(Buf),RR);

If Buf.Mark=0
   Then Begin
        LastEntry:=True;
        Close(F);
        ResetFileMode;
        Exit;
        End;

If Not BeQuick
   Then Begin
        With IBM(Entry),Buf Do
         Begin
         FileName       := Asciiz2String(Name);
         CompressedSize := CompSize;
         OriginalSize   := RealSize;
         If Version<=11
            Then CompressionName:= ArcMethodes[Version]
            Else CompressionName:= 'Unknown   ';
         FileCRC        := HexWord(CRC)+'    ';
         FileDate       := TimeStamp((LongInt(Date) Shl 16)+LongInt(Time));
         SaveID         := '';
         End; {With}
        End;

Case Buf.Version of
 10 : CompressorName:='PAK';    { Cannot be trusted! }
 11 : CompressorName:='ARC7';
End; {Case}

WhereInFile:=WhereInFile+SizeOf(Buf)+Buf.CompSize;
Close(F);
ResetFileMode;
End;

Procedure ARCObject.FindNextEntry;
Begin
SetFileMode(ReadOnly+ShareDenyNone);
Assign(F,FileName);
Reset(F,1);
Seek(F,WhereInFile);

BlockRead(F,Buf,SizeOf(Buf),RR);
If Buf.Version=0
   Then Begin
        LastEntry:=True;
        Close(F);
        ResetFileMode;
        Exit;
        End;

If Not BeQuick
   Then Begin
        With IBM(Entry),Buf Do
         Begin
         FileName       := Asciiz2String(Name);
         CompressedSize := CompSize;
         OriginalSize   := RealSize;
         If Version<=11
            Then CompressionName:= ArcMethodes[Version]
            Else CompressionName:= 'Unknown   ';
         FileCRC        := HexWord(CRC)+'    ';
         FileDate       := TimeStamp((LongInt(Date) Shl 16)+LongInt(Time));
         SaveID         := '';
         End; {With}
        End;

Case Buf.Version of
 10 : CompressorName:='PAK';    { Cannot be trusted! }
 11 : CompressorName:='ARC7';
End; {Case}

WhereInFile:=WhereInFile+SizeOf(Buf)+Buf.CompSize;

Close(F);
ResetFileMode;
End;

Procedure ARCObject.CheckProtection;
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

Function ARCObject.IsThisTypeFile(Var B ;Size : Word):Boolean;
Type Check = Array[0..2] Of Char;
Begin
ARCInit;
IsThisTypeFile:=True;

If IsExeFile(B) and
   SearchBuffer(B,Size,8400,9000,'it?'#00#$1A,WhereInFile)
   Then Begin
        SelfExtractor:=True;
        Inc(WhereInFile,4);
        Exit;
        End;
WhereInFile:=0;


If (Byte(B)=$1A) And
   (Check(B) <> #$1A'HP') And   { Check HYPER! }
   (Check(B) <> #$1A'ST')
   Then Exit;
IsThisTypeFile:=False;
End;

Var CO          : ArcPtr;

procedure InitARC;
begin
New(CO,ArcInit);
AddToList(CO);
end; { proc. InitArc }

{$IFNDEF OVERLAY}
Begin
InitArc;
{$ENDIF}

End.

