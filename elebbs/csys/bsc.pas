{ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»}
{º      -- THIS FILE IS PART OF THE LIVESYSTEMS COMPRESSOR TOOLBOX. --      º}
{º          ALL RIGHTS RESERVED  (C) COPYRIGHTED G. HOOGTERP 1994           º}
{º                                                                          º}
{º             See the documentation for details on the license.            º}
{º                                                                          º}
{ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼}

{ Items marked with * are new in this version }

{ Define UseASM}

Unit BSC; { Basic Compressor Routines }
{$I compiler.inc}
{$IFDEF WIn32} {$H-} {$ENDIF}
Interface

{$IFNDEF WINGUI}
Uses Dos, GenDos;
{$ELSE}
Uses SysUtils, Use32, GenDos;
{$ENDIF}

procedure InitBSC;

{
  VirtualPascal fools around with the Use32 unit so one is never sure which size a type has.
  For this unit and all the units which use the BasicCompressorObject as their root things
  will be forced to the same sizes as TP/BP uses.
}

{$IFDEF VIRTUALPASCAL}
(*
Type Shortint = System.Shortint;
     Integer  = System.SmallInt;
     LongInt  = System.LongInt;
     Byte     = System.Byte;
     Word     = System.SmallWord;
*)
Type
     LongWord = System.Cardinal;
     RR_Type  = LongWord;          { Used as SIZE type in BlockRead/BlockWrite }
{$ENDIF}

{$IFDEF SPEED}
     Type RR_Type  = LongInt;      { Used as SIZE type in BlockRead/BlockWrite }
{$ENDIF}

{$IFDEF FPC}
     Type SmallWord = Word;
     Type RR_Type  = Longint;
{$ENDIF}

{$IFDEF MSDOS}
     Type SmallWord = Word;
     Type RR_Type  = Word;
{$ENDIF}

{$IFDEF WINGUI}
     Type SmallWord = WOrd;
     Type RR_Type  = Longint;
{$ENDIF}

{$IFDEF WIN32}
          PathStr  = String[128];
{$ENDIF}

Type  ComStr   = String[127];


{$I Struct.pas}

Const MaxCompressors     = 16;  { Maximum number of compressors that can be   }
                                { Can be maximal 255 but 16 is enough for now }

Const ReadOnly           = $00;   { Filemode constants }
      WriteOnly          = $01;
      ReadWrite          = $02;

      ShareCompatible    = $00;
      ShareDenyReadWrite = $10;
      ShareDenyWrite     = $20;
      ShareDenyRead      = $30;
      ShareDenyNone      = $40;

      Inheritance        = $80;
      DefaultFileMode    = ReadOnly+ShareCompatible;

Type
     BasicCompressorObject = Object        { Basic compressor object     }
        FileName           : String;       { Current filename            }
        CompressorType     : CompressorID; { Unique short compressor ID  }
        CompressorName     : NameString;   { Full compressor name        }
        Magic              : MagicTypes;   { A unique number             }
        WhereInFile        : LongInt;      { Filepointer                 }

        ProtectedFile      : Boolean;      { Sec. Env. boolean           }
        SelfExtractor      : Boolean;      { SelfExtractor boolean       }
        ContainsPaths      : Boolean;      { Contains paths boolean      }
        HasPassword        : Boolean;      { Password protected          }
        SolidArchive       : Boolean;      { Is solid                    }
        Locked             : Boolean;      { is Locked                   }
        UnpackVersion      : Byte;         { Unpack version. 0 -> unknown}

        HeaderTitle        : String[132];  { Title line for header       }
        HeaderLines        : String[132];  { Second line for header      }
        FileExtra          : String[132];  { Extra info found in the file}
        Entry              : InfoBlock;    { Internal entry buffer       }

        Platform           : PlatformID;   { Compressors platform        }
        LastEntry          : Boolean;      { True if end of file         }
        BeQuick            : Boolean;      { Don't show so don't conv.   }
        PreviouseMode      : Byte;         { Memory byte last filemode   }

        RR,RW              : RR_Type;      { RealRead variable for Blockread/write }

        Constructor Init;

        { Ä Compressor dependend functions ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ }

        Procedure FindFirstEntry;         Virtual;
        Procedure FindNextEntry;          Virtual;
        Procedure CheckProtection;        Virtual;
        Procedure PrintEntry;             Virtual;
        Function IsThisTypeFile(Var B; Size : Word):Boolean; Virtual;
        Procedure ReturnEntry(Var E);     Virtual;

        { Ä Compressor independend functions ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ }

        Function IsProtected:Boolean;            { has Security envelope    }
        Function IsSelfExtractor:Boolean;        { is selfextracting file   }
        Function HasPaths:Boolean;               { Contains dir. structure  }
        Function IsSolidArchive:Boolean;         { Is solid                 }
        Function IsPasswordProtected:Boolean;    { Has passwords            }
        Function IsLocked:Boolean;               { Is Locked                }

        Function WhichType:CompressorID;         { Return Compressor ID     }
        Function WhichPlatform:PlatFormID;       { Return current platform  }
        Function PlatformName:String;            { The name of the platform }
        Procedure WriteHeader;                   { Write a header on screen }

        { Ä Misc. tools ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ }

        Function Asciiz2String(Var A):String;
        Function TimeStamp(Time : LongInt):TimeString;
        Function UnixTime(Time : LongInt):TimeString;
        Function Nr2Str(W : LongInt;Len : Byte):String;
        Function HexWord(Number : Word):String;
        Function HexLong(number : LongInt):String;
        Function ShortFileName(FileSpec : ComStr):ComStr;
        Function StripPath(F : ComStr):PathStr;
        Function IsBitSet(Flag,Bit : Word):Boolean;
        Function SearchBuffer(Var B ;
                                  Size  : Word;
                                  Start : Word;
                                  Stop  : Word;
                                  Check : String;
                              Var InFile: LongInt
                                  ):Boolean;
        Function IsEXEFile(Var B):Boolean;
        Function LongSwap(L : LongInt):LongInt;

        Procedure SetFileMode(Mode : Byte);
        Procedure ResetFileMode;
      End; {Basic Compressor Object}


{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Create an array of pointers to compressionobjects.
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}

Type ObjectList  = Array[1..MaxCompressors] of ^BasicCompressorObject;
Var  OList       : ObjectList;
     OPtr        : Byte;
     ExitSave    : Pointer;

Procedure AddToList(P : Pointer);

Implementation

Constructor BasicCompressorObject.Init;
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Initialize the object, fill all the fields.
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Begin
BeQuick      :=False;
LastEntry    :=False;
SelfExtractor:=False;
ProtectedFile:=False;
ContainsPaths:=False;
HasPassword  :=False;
SolidArchive :=False;
Locked       :=False;
UnpackVersion:=0;

CompressorType:='UNK';
CompressorName:='* Unknown *' ;
Magic         := None;

PlatForm    :=ID_IBM;
HeaderTitle :='Orig   Methode    Comp   Time     Date        CRC      Name             Sec';
HeaderLines :='ÍÍÍÍÍÍ ÍÍÍÍÍÍÍÍÍÍ ÍÍÍÍÍÍ ÍÍÍÍÍÍÍÍ ÍÍÍÍÍÍÍÍÍÍÍ ÍÍÍÍÍÍÍÍ ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ ÍÍÍ';
FileExtra   :='';
End;


{ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
  Virtual procedures and functions
 ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ}

Procedure BasicCompressorObject.FindFirstEntry;
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Find the first entry in a compressed file.   VIRTUAL procedure
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Begin
End;

Procedure BasicCompressorObject.FindNextEntry;
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Find the next entry in a compressed file.    VIRTUAL procedure
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Begin
End;


Procedure BasicCompressorObject.CheckProtection;
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Check a file for protectionflags, paths etc. VIRTUAL procedure
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Begin
End;


Procedure BasicCompressorObject.WriteHeader;
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Write an header to the screen.       VIRTUAL procedure
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Begin
WriteLn('Filename    : ',StripPath(FileName));
  Write('Ã Compressor: ',CompressorName);
If SelfExtractor
   Then Write('  (SelfExtractor)');
If UnpackVersion<>0
   Then WriteLn(' (MinVersion: ',UnpackVersion,')')
   Else WriteLn(' (MinVersion: unknown');
If FileExtra='' Then Write('À ') Else Write('Ã ');
WriteLn('Platform  : ',PlatformName);

If FileExtra<>''
   Then WriteLn('À ',FileExtra);
WriteLn;
WriteLn(HeaderTitle);
WriteLn(HeaderLines);
End;

Procedure BasicCompressorObject.PrintEntry;
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Write an entry to the screen.    VIRTUAL procedure.
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Begin
{
With IBM(Entry) Do
 Begin
 Write(OriginalSize:6,' ');
 Write(CompressionName,' ');
 Write(CompressedSize:6,' ');
 Write(FileDate:20,' ');
 Write(FileCRC,' ');
 Write(ShortFileName(FileName));
 WriteLn(' ',SaveID);
 End;
}
End;

Procedure BasicCompressorObject.ReturnEntry(Var E);
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Return an entry as untyped variable.   VIRTUAL procedure.
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Begin
{Move(IBM(Entry),E,SizeOf(Entry));}
Move(Entry, E, SizeOf(Entry));
End;

Function BasicCompressorObject.IsThisTypeFile(Var B;Size : Word):Boolean;
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Detect if the current file is of this type. VIRTUAL procedure
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Begin
IsThisTypeFile:=False;
End;

{ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
  Non-virtual procedures and functions
 ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ}


Function BasicCompressorObject.IsProtected:Boolean;
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Return the value of the ProtectedFile boolean.
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Begin
IsProtected:=ProtectedFile;
End;

Function BasicCompressorObject.IsSelfExtractor:Boolean;
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Return the value of the SelfExtractor boolean.
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Begin
IsSelfExtractor:=SelfExtractor;
End;

Function BasicCompressorObject.IsLocked:Boolean;
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Return the value of the Locked boolean.
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Begin
IsLocked:=Locked;
End;


Function BasicCompressorObject.HasPaths:Boolean;
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Return the value of the haspaths boolean.
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Begin
HasPaths:=ContainsPaths;
End;

Function BasicCompressorObject.IsPasswordProtected:Boolean;
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Return the value of the HasPassword boolean.
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Begin
IsPasswordProtected:=HasPassword;
End;

Function BasicCompressorObject.IsSolidArchive:Boolean;
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Return the value of the HasPassword boolean.
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Begin
IsSolidArchive:=SolidArchive;
End;



Function BasicCompressorObject.WhichType:CompressorID;
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Return the value of the CompressorType field.
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Begin
WhichType:=CompressorType;
End;

Function BasicCompressorObject.WhichPlatform:PlatFormID;
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Return the value of the Platform field.
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Begin
WhichPlatform:=PlatForm;
End;


Function BasicCompressorObject.PlatformName:String;
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Return a description of the platform
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Begin
Case Platform Of
 ID_IBM      : PlatformName:='IBM or compatible';
 ID_MAC      : PlatformName:='Apple MacIntosh';
 ID_MULTI    : PlatformName:='Platform independend';
 Else          PlatformName:='Unknown platform';
End; {Case}
End;


{ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
  LowLevel utility routines.
 ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ}

Const  Months     : Array[0..12] of String[3]
                  = (
                    '???',
                    'Jan','Feb','Mar','Apr','May','Jun',
                    'Jul','Aug','Sep','Oct','Nov','Dec'
                    );


Function BasicCompressorObject.Asciiz2String(Var A):String;
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Convert an ASCIIZ string to a TP string.
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Type Temp = Array[1..255] of Char;
Var S : String;
Begin
Move(Temp(A),S[1],255);

S[0]:=#01;
While (Length(S)<255) And (S[Length(S)]<>#00) Do
 Inc(S[0]);
Dec(S[0]);
Asciiz2String:=S;
End;

Function BasicCompressorObject.TimeStamp(Time : Longint):TimeString;
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Create a timestamp string from a MSdos timestamp longint.
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Var DateRec : DateTime;
    TempStr : TimeString;
Begin
UnpackTime(Time,DAteRec);
TempStr:='';
With DateRec Do
 TempStr:= Nr2Str(Hour,2)+':'+Nr2Str(Min,2)+':'+Nr2Str(Sec,2)+' '+
           Nr2Str(Day,2)+'-'+Months[Month]+'-'+Nr2Str(Year,4);
TimeStamp:=TempStr;
End;

Function BasicCompressorObject.UnixTime(Time : LongInt):TimeString;
Begin
UnixTime:=' Unsupported format ';
End;


Function BasicCompressorObject.Nr2Str(W : LongInt;Len : Byte):String;
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Convert a number to a string of a certain length.
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Var Tmp : String[10];
    C   : Byte;
Begin
Str(W:Len,Tmp);
For C:=1 To Length(Tmp) Do
 If Tmp[C]=' '
    Then Tmp[C]:='0';
Nr2Str:=Tmp;
End;


Function BasicCompressorObject.HexWord(number : Word):String;
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Convert a word to a HEX value.
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Const HexNum : Array[0..15] Of Char = '0123456789ABCDEF';
Begin
HexWord:=HexNum[(Hi(Number) And $F0) Shr 4] + HexNum[(Hi(Number) And $0F)]+
         HexNum[(Lo(Number) And $F0) Shr 4] + HexNum[(Lo(Number) And $0F)];
End;

Function BasicCompressorObject.HexLong(number : LongInt):String;
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Convert a longint to a HEX value.
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Const HexNum : Array[0..15] Of Char = '0123456789ABCDEF';
Type  tLong = Record
        H,L : Word;
      End;
Begin
HexLong:=HexWord(tLong(Number).H)+HexWord(tLong(Number).L);
End;





Function BasicCompressorObject.ShortFileName(FileSpec : ComStr):ComStr;
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Shorten a full filespecifier to a filename with pathindication
    F.e.: C:\TEST\PROG\BLABLA.PAS becomes
          ...\BLABLA.PAS
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Var Dum  : ComStr;
    Name : String[40];
    Ext  : String[40];
    Count: Byte;
Begin
For Count:=1 To Length(FileSpec) do
 If FileSpec[Count]='/'
    then FileSpec[Count]:='\';

{$IFNDEF WINGUI}
FSplit(FileSpec,Dum,Name,Ext);
{$ELSE}
 Dum := ExtractFilePath(FileSpec);
 Name := ExtractFileName(FileSpec);
 Ext := ExtractFileExt(FileSpec);
{$ENDIF}

If Dum<>''
   Then Dum:='...\'+Name+Ext
   Else Dum:='    '+Name+Ext;
While Length(Dum)<=15 Do
 Dum:=Dum+' ';
ShortFileName:=Dum;
End;

Function BasicCompressorObject.StripPath(F : ComStr):PathStr;
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Strip the path and return only the filename.
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Var Dum   : Byte;
Begin
Dum:=Length(F);
Repeat
 Dec(Dum);
Until (Dum=0) Or (F[Dum] in ['\','/',':']);
If Dum>0
   Then Delete(F,1,Dum);
StripPath:=F;
End;

{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  SearchBuffer searches a buffer of a certain size for a certain string.
  The Start and stop offset can be given to limit the search range.
  InFile returns the position of the string within the buffer if found.
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}

{$IfNDef UseASM}
Function BasicCompressorObject.SearchBuffer(Var B;
                                                Size  : Word;
                                                Start : Word;
                                                Stop  : Word;
                                                Check : String;
                                            Var InFile: LongInt
                                           ):Boolean;

Type TC = Array[0..$FFFE] of Char;
Var BufPtr : Word;
    Found  : Boolean;
    Ok     : Boolean;
    TmpPtr : Word;
Begin
SearchBuffer:=True;
BufPtr:=Start;
Found:=False;
While (Not Found) And (BufPtr<Stop) Do
  Begin
  If Check[1]=TC(B)[BufPtr]
     Then Begin
          Ok:=True;
          TmpPtr:=BufPtr+1;
          While Ok And ((TmpPtr-BufPtr)<Length(Check)) Do
            Begin
            Ok:=TC(B)[TmpPtr]=Check[TmpPtr-BufPtr+1];
            Inc(TmpPtr);
            End;
          Found:=Ok;
          End;

  Inc(BufPtr);
  End;
SearchBuffer:=Found;
InFile:=BufPtr-1;
End;

{$Else}

Function BasicCompressorObject.SearchBuffer(Var B;
                                                Size  : Word;
                                                Start : Word;
                                                Stop  : Word;
                                                Check : String;
                                            Var InFile: LongInt
                                                ):Boolean; External;
{$L .\SEARCH.OBJ}

{$EndIf}


Function BasicCompressorObject.IsEXEFile(Var B):Boolean;
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Check if the file is an exe file.
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Type Check = Array[0..1] of Char;
begin
  isEXEFile:= (Check(B)='MZ');
End;

Function BasicCompressorObject.IsBitSet(Flag,Bit : Word):Boolean;
Begin
IsBitSet:=(Flag and Bit)=Bit;
End;

Function BasicCompressorObject.LongSwap(L : LongInt):LongInt;
Type TC = Record
           W1,W2 : Word;
          End;
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Swap a longint from INTEL to MOTEROLA format or vice versa
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Begin
LongSwap:=(LongInt(SWAP(TC(L).W1)) Shl 16) + LongInt(SWAP(TC(L).W2));
End;

{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
 Store, set and reset the filemode variable
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}

Procedure BasicCompressorObject.SetFileMode(Mode : Byte);
Begin
PreviouseMode:=FileMode;
FileMode:=Mode;
End;

Procedure BasicCompressorObject.ResetFileMode;
Begin
FileMode:=PreviouseMode;
PreviouseMode:=DefaultFileMode;
End;


{ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
  The Object list support
 ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ}

Procedure AddToList(P : Pointer);
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Add an object to the list.
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Begin
If OPtr=MaxCompressors Then Exit;
Inc(OPtr);
OList[OPtr]:=P;
End;


{$F+}
Procedure MyExitProc;
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  Dispose the objects in the list. Clean up!
 ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
Begin
ExitProc:=ExitSave;
While OPtr>0 Do
 Begin
 If OList[OPtr]<>NIL
    Then Begin
         Dispose(OList[OPtr]); {!}
         OLIst[OPtr]:=NIL;
         End;
 Dec(OPtr);
 End;
End;
{$F-}

procedure InitBSC;
Begin
ExitSave:=ExitProc;
ExitProc:=@MyExitProc;   { Install the cleanup procedure in the exitlist }

OPtr:=0;                 { Init the ObjectList                           }
FillChar(OList,SizeOf(OList),#00);

end; { proc. InitBsc }

{$IFNDEF OVERLAY}
begin
 InitBSC;
{$ENDIF}
End.

