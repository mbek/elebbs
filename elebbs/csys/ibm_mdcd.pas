{ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป}
{บ      -- THIS FILE IS PART OF THE LIVESYSTEMS COMPRESSOR TOOLBOX. --      บ}
{บ          ALL RIGHTS RESERVED  (C) COPYRIGHTED G. HOOGTERP 1994           บ}
{บ                                                                          บ}
{บ             See the documentation for details on the license.            บ}
{บ                                                                          บ}
{ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

Unit IBM_MDCD;
{$I COMPILER.INC}
Interface
Uses BSC;

Type LocalHeader    = Record                 {header for each compressed file   }
       Signature    : Array[0..3] Of Char;   {file/header signature (MDmd)      }
       ReleaseLevel : Byte;                  {compress version                  }
       HeaderType   : Byte;                  {header type. only type 1 for now  }
       HeaderSize   : SmallWord;                  {size of this header in bytes      }
       UserInfo     : SmallWord;                  {any user info desired             }
       Reserved1    : SmallWord;                  {future use and upward compatablty }
       Reserved2    : LongInt;               {future use and upward compatablty }
       Reserved3    : Array[1..8] of byte;   {future use and upward compatablty }
       CompressType : Byte;                  {type of compression               }
       OrigFileSize : LongInt;               {original file size in bytes       }
       CompFileSize : LongInt;               {compressed file size in bytes     }
       FileAttr     : SmallWord;                  {original file attribute           }
       FileDate     : LongInt;               {original file date/time           }
       FileCRC      : SmallWord;                  {file crc                          }
       FileName     : String[12];            {file name                         }
       PathName     : String[67];            {original drive\path               }
     End;



Type MDCDObject = Object(BasicCompressorObject)
       F           : File;
       Buf         : LocalHeader;
       MCDCMethodes: Array[0..1] of String[10];

       Constructor MDCDInit;
       Procedure FindFirstEntry;   Virtual;
       Procedure FindNextEntry;    Virtual;
       Procedure CheckProtection;  Virtual;
       Function IsThisTypeFile(Var B ;Size : Word):Boolean; Virtual;
     End; {Object}

     MDCDPtr = ^MDCDObject;

procedure InitMDCD;

Implementation


Constructor MDCDObject.MDCDInit;
Begin
inherited Init;
Platform:=ID_IBM;
CompressorType:='MD';
CompressorName:='MDCD';
Magic:=MDCD_Type;
MCDCMethodes[0] := 'Stored    ';
MCDCMethodes[1] := 'LZW13     ';
End;



Procedure MDCDObject.FindFirstEntry;
Begin
SetFileMode(ReadOnly+ShareDenyNone);
Assign(F,FileName);
Reset(F,1);
Seek(F,WhereInFile);

BlockRead(F,Buf,SizeOf(Buf),RR);

If Not BeQuick
   Then Begin
        With IBM(Entry) Do
         Begin
         FileName       := Buf.PathName+Buf.FileName;
         OriginalSize   := Buf.OrigFileSize;
         CompressedSize := Buf.CompFileSize;
         If (Buf.FileAttr And $10)=$10
            Then CompressionName:='<DIR>     '
            Else CompressionName:= MCDCMethodes[Buf.CompressType];
         FileCRC        := HexWord(Buf.FileCRC)+'    ';
         FileDate       := TimeStamp(Buf.FileDate);
         SaveID         := '';
         ContainsPaths  := Buf.PathName<>'';
         Extra          := '';
         End; {With}
        End;
WhereInFile:=WhereInFile+SizeOf(Buf)+Buf.CompFileSize;

Close(F);
ResetFileMode;
End;

Procedure MDCDObject.FindNextEntry;
begin
SetFileMode(ReadOnly+ShareDenyNone);
Assign(F,FileName);
Reset(F,1);
Seek(F,WhereInFile);

BlockRead(F,Buf,SizeOf(Buf),RR);
If RR=0
   Then Begin
        LastEntry:=True;
        Close(F);
        ResetFileMode;
        Exit;
        End;


If Not BeQuick
   Then Begin
        With IBM(Entry) Do
         Begin
         FileName       := Buf.PathName+Buf.FileName;
         OriginalSize   := Buf.OrigFileSize;
         CompressedSize := Buf.CompFileSize;
         If (Buf.FileAttr And $10)=$10
            Then CompressionName:='<DIR>     '
            Else CompressionName:= MCDCMethodes[Buf.CompressType];
         FileCRC        := HexWord(Buf.FileCRC)+'    ';
         FileDate       := TimeStamp(Buf.FileDate);
         ContainsPaths  := Buf.PathName<>'';
         SaveID         := '';
         Extra          := '';
         End; {With}
        End;
WhereInFile:=WhereInFile+SizeOf(Buf)+Buf.CompFileSize;

Close(F);
ResetFileMode;
End;

Procedure MDCDObject.CheckProtection;
Var Old : LongInt;
Begin
Old:=WhereInFile;
BeQuick:=True;

BeQuick:=False;
WhereInFile:=Old;
LastEntry:=False;
End;

Function MDCDObject.IsThisTypeFile(Var B ;Size : Word):Boolean;
Type Check = Array[0..3] Of Char;
Begin
MDCDInit;
IsThisTypeFile:=True;

If Check(B)='MDmd'
   Then Exit;

IsThisTypeFile:=False;
End;


Var CO          : MDCDPtr;

procedure InitMDCD;
begin
New(CO,MDCDInit);
AddToList(CO);
end; { proc. InitMDCD }


{$IFNDEF OVERLAY}
Begin
 InitMDCD;
{$ENDIF}
End.

