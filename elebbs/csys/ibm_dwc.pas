{ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป}
{บ      -- THIS FILE IS PART OF THE LIVESYSTEMS COMPRESSOR TOOLBOX. --      บ}
{บ          ALL RIGHTS RESERVED  (C) COPYRIGHTED G. HOOGTERP 1994           บ}
{บ                                                                          บ}
{บ             See the documentation for details on the license.            บ}
{บ                                                                          บ}
{ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

Unit IBM_DWC;
{$I COMPILER.INC}
Interface
Uses BSC;

Type Buffer           = Array[1..34] Of Byte;
     NameType         = Array[1..13] Of Char;
     ID_Type          = Array[1..3] Of Char;
     MainHeader  = Record
       Fill      : Array[1..7] Of Char;
       Size      : SmallWord;
       Ent_SZ    : Byte;
       Header    : NameType;
       TimeDate  : LongInt;
       Entries   : LongInt;
       ID_3      : ID_Type;
     End;
     LocalHeader    =  RECORD
       Name         : NameType;
       RealSize     : LongInt;
       TimeDate     : LongInt;
       CompSize     : LongInt;
       FPos         : LongInt;
       Method       : Byte;
       SZ_C         : Byte;
       SZ_D         : Byte;
       CRC          : SmallWord;
     END;


Type DWCObject = Object(BasicCompressorObject)
       F           : File;
       Buf         : Buffer;
       DWCMethodes : Array[1..3] Of String[10];

       Constructor DWCInit;
       Procedure FindFirstEntry;   Virtual;
       Procedure FindNextEntry;    Virtual;
       Procedure CheckProtection;  Virtual;
       Function IsThisTypeFile(Var B ;Size : Word):Boolean; Virtual;
     End; {Object}

     DWCPtr = ^DWCObject;

procedure InitDWC;

Implementation

Constructor DWCObject.DWCInit;
Begin
inherited Init;
Platform:=ID_IBM;
CompressorType:='DWC';
CompressorName:='DWC';
Magic:=DWC_Type;
DWCMethodes[1] := 'Meth. 1   ';
DWCMethodes[2] := 'Meth. 2   ';
DWCMethodes[3] := 'Meth. 3   ';
End;


Procedure DWCObject.FindFirstEntry;
var MainHeaderbuf: MainHeader absolute buf;
    LocalHeaderBuf: LocalHeader absolute buf;
Begin
SetFileMode(ReadOnly+ShareDenyNone);
Assign(F,FileName);
Reset(F,1);
WhereInFile:=FileSize(F)-SizeOf(Buf);
Seek(F,WhereInFile);
BlockRead(F,Buf,SizeOf(Buf),RR);
WhereInFile:=FileSize(F)-27-(MainHeaderBuf.Entries*SizeOf(Buf));

Seek(F,WhereInFile);
BlockRead(F,Buf,SizeOf(Buf),RR);

If Not BeQuick
   Then Begin
        With IBM(Entry),LocalHeaderBuf Do
         Begin
         FileName       := Asciiz2String(Name);
         OriginalSize   := RealSize;
         CompressedSize := CompSize;
         CompressionName:= DWCMethodes[Method];
         FileCRC        := HexWord(CRC) + '    ';
         FileDate       := UnixTime(TimeDate);
         SaveID         := '';
         End;
        End;

WhereInFile:=FilePos(F);
Close(F);
ResetFileMode;
End;

Procedure DWCObject.FindNextEntry;
var LocalHeaderBuf: LocalHeader absolute buf;
Begin
SetFileMode(ReadOnly+ShareDenyNone);
Assign(F,FileName);
Reset(F,1);
Seek(F,WhereInFile);
BlockRead(F,Buf,SizeOf(Buf),RR);
If Eof(F)
   Then Begin
        LastEntry:=True;
        Close(F);
        ResetFileMode;
        Exit;
        End;

If Not BeQuick
   Then Begin
        With IBM(Entry),LocalHeaderBuf Do
         Begin
         FileName       := Asciiz2String(Name);
         OriginalSize   := RealSize;
         CompressedSize := CompSize;
         CompressionName:= DWCMethodes[Method];
         FileCRC        := HexWord(CRC) + '    ';
         FileDate       := UnixTime(TimeDate);
         SaveID         := '';
         End;
        End;

WhereInFile:=FilePos(F);

Close(F);
ResetFileMode;
End;

Procedure DWCObject.CheckProtection;
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

Function DWCObject.IsThisTypeFile(Var B ;Size : Word):Boolean;
Var TmpF : File;
    ID: Array[0..2] of Char;
    MZ: Array[0..1] of Char;
Begin
DWCInit;
IsThisTypeFile:=True;
Assign(TmpF,FileName);
Reset(TmpF,1);
BlockRead(TmpF,MZ,2,RR);
SelfExtractor:=MZ='MZ';
Seek(TmpF,FileSize(TmpF)-3);
BlockRead(TmpF,ID,3,RR);
Close(TmpF);
If ID='DWC' Then Exit;
IsThisTypeFile:=False;
End;

Var CO          : DWCPtr;

procedure InitDWC;
begin
New(CO,DWCInit);
AddToList(CO);
end; { proc. InitDWC }

{$IFNDEF OVERLAY}
Begin
 InitDWC;
{$ENDIF}
End.

