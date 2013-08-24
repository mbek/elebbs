{ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป}
{บ      -- THIS FILE IS PART OF THE LIVESYSTEMS COMPRESSOR TOOLBOX. --      บ}
{บ          ALL RIGHTS RESERVED  (C) COPYRIGHTED G. HOOGTERP 1994           บ}
{บ                                                                          บ}
{บ             See the documentation for details on the license.            บ}
{บ                                                                          บ}
{ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

Unit IBM_ZOO;
{$I COMPILER.INC}
Interface
Uses BSC;

Type MainHeader  = Record
       ID        : Array[1..20] Of Char;
       LoTag     : SmallWord;
       HiTag     : SmallWord;
       Start     : LongInt;
       Minus     : Longint;
       MajVers   : Byte;
       MinVers   : Byte;
     End;
     LocalHeader = Record
       LoTag     : SmallWord;
       HiTag     : SmallWord;
       CType     : Char;
       Methode   : Byte;
       Next      : LongInt;
       Offset    : LongInt;
       Date      : SmallWord;
       Time      : SmallWord;
       CRC       : SmallWord;
       RealSize  : LongInt;
       CompSize  : LongInt;
       MajVer    : Byte;
       MinVer    : Byte;
       Del       : Boolean;
       CommPtr   : LongInt;
       CommLen   : SmallWord;
       Name      : Array[0..13] Of Char;
     End;


Type ZOOObject = Object(BasicCompressorObject)
       F           : File;
       Buf         : LocalHeader;
       EXEofs      : LongInt;
       ZooMethodes : Array[0..1] Of String[10];

       Constructor ZOOInit;
       Procedure FindFirstEntry;   Virtual;
       Procedure FindNextEntry;    Virtual;
       Procedure CheckProtection;  Virtual;
       Function IsThisTypeFile(Var B ;Size : Word):Boolean; Virtual;
     End; {Object}

     ZOOPtr = ^ZOOObject;

procedure InitZOO;

Implementation




Constructor ZOOObject.ZOOInit;
Begin
inherited Init;
Platform:=ID_IBM;
CompressorType:='ZOO';
CompressorName:='ZOO';
Magic:=ZOO_Type;
ZooMethodes[0] := 'Stored';
ZooMethodes[0] := 'LZW-compr.';
End;



Procedure ZOOObject.FindFirstEntry;
Var  Main     : MainHeader;
Begin
SetFileMode(ReadOnly+ShareDenyNone);
Assign(F,FileName);
Reset(F,1);


Seek(F,WhereInFile);
BlockRead(F,Main,SizeOf(Main),RR);
CompressorName:=Main.ID;
CompressorName[0]:=#8;

EXEofs:=WhereInFile;
Seek(F,EXEofs+Main.Start);
BlockRead(F,Buf,SizeOf(Buf),RR);

If Not BeQuick
   Then Begin
        With Buf,IBM(Entry) Do
         Begin
         FileName       := Asciiz2String(Name[1]);
         OriginalSize   := RealSize;
         CompressedSize := CompSize;
         CompressionName:= ZooMethodes[Methode];
         FileCRC        := HexWord(CRC) + '    ';
         FileDate       := TimeStamp((LongInt(Date) Shl 16)+LongInt(Time));
         SaveID         := '';
         End; {with}
        End;

WhereInFile:=EXEofs+Buf.Next;
Close(F);
ResetFileMode;
End;

Procedure ZOOObject.FindNextEntry;
Begin
SetFileMode(ReadOnly+ShareDenyNone);
Assign(F,FileName);
Reset(F,1);
Seek(F,WhereInFile);

BlockRead(F,Buf,SizeOf(Buf),RR);
If Buf.CompSize=0
   Then Begin
        LastEntry:=True;
        Close(F);
        ResetFileMode;
        Exit;
        End;


If Not BeQuick
   Then Begin
        With Buf,IBM(Entry) Do
         Begin
         FileName       := Asciiz2String(Name[1]);
         OriginalSize   := RealSize;
         CompressedSize := CompSize;
         CompressionName:= ZooMethodes[Methode];
         FileCRC        := HexWord(CRC) + '    ';
         FileDate       := TimeStamp((LongInt(Date) Shl 16)+LongInt(Time));
         SaveID         := '';
         If Del
            Then Extra  := 'Deleted'
            Else Extra  := '';
         End; {with}
        End;

WhereInFile:=EXEofs+Buf.Next;

Close(F);
ResetFileMode;
End;

Procedure ZOOObject.CheckProtection;
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

Function ZOOObject.IsThisTypeFile(Var B ;Size : Word):Boolean;
Type Check = Array[0..2] of Char;
Begin
ZOOInit;
IsThisTypeFile:=True;

If IsExeFile(B)
   Then Begin
        SelfExtractor:=True;
        If SearchBuffer(B,Size,2400,2500,'ZOO',WhereInFile) Then Exit;
        End;
WhereInFile:=0;

If Check(B)='ZOO'
   Then Exit;
IsThisTypeFile:=False;
End;

Var CO          : ZOOPtr;

procedure InitZOO;
begin
New(CO,ZOOInit);
AddToList(CO);
end; { proc. InitZOO }

{$IFNDEF OVERLAY}
Begin
  InitZOO;
{$ENDIF}
End.

