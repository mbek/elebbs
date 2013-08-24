{ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป}
{บ      -- THIS FILE IS PART OF THE LIVESYSTEMS COMPRESSOR TOOLBOX. --      บ}
{บ          ALL RIGHTS RESERVED  (C) COPYRIGHTED G. HOOGTERP 1994           บ}
{บ                                                                          บ}
{บ             See the documentation for details on the license.            บ}
{บ                                                                          บ}
{ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}
Unit CompSys;     { Compressor System Main Unit }
{$I compiler.inc}
Interface

Uses
{$IFNDEF WIN32}
     Dos,
{$ELSE}
     SysUtils,
{$ENDIF}
     BSC,         { Basic compressed object     Always first! }

{    MAC_SIT,  } { Macintosh SIT! formaat      }

     IBM_PKZ,     { Pkzip                       }
     IBM_LHA,     { LHA/LZARC/LA                }
     IBM_ARJ,     { Arj                         }
     IBM_SQZ,     { SQZ                         }
     IBM_ARC,     { ARC/PAK/ARC7                }
     IBM_HYP,     { Hyper                       }
     IBM_DWC,     { DWC                         }
     IBM_MDCD,    { MDCD                        }
     IBM_ZOO,     { ZOO                         }
     IBM_RAR;     { RAR                         }


Const CompSys_LowMemory : Boolean = False;

Type CompressorType = ^BasicCompressorObject;

Function DetectCompressor(    _Filename : ComStr;
                          Var _CO       : CompressorType):Boolean;


procedure InitVariables;

Implementation

{$ifdef elebbs}
uses ellog_u, stutils, longstr;
{$else}
uses radu;
{$endif}

Const BufferSize      = 25*1024;  { Make sure there is enough heap! }

Type CheckBuffer = Array[1..BufferSize] of Byte;
Var  Check       : ^CheckBuffer;

Function DetectCompressor(    _Filename : ComStr;
                          Var _CO       : CompressorType):Boolean;
Var F       : File;
    RR      : RR_Type;
    ThisOne : Byte;
    Found   : Boolean;

Begin
CompSys_LowMemory := False;
DetectCompressor:=False;
New(Check);
if Check=NIL
   Then begin
           CompSys_LowMemory := True;

           {$IFDEF ELEBBS}
	           {$IFDEF MSDOS}
    		       	Ralog('!', 'Unable to allocate enough memory for fileviewing operation (need at least '+
            		     	FStr(SizeOf(CheckBuffer)) + ', available '+FStr(MemAvail)+') !');
           		{$endif}
           {$ELSE}
				RaLog('Unable to allocate enough memory in compsys.pas');           
           {$ENDIF}
           
           Exit;
        end;
FillChar(Check^,SizeOf(Check^),#00);

Assign(F,_FileName);
Reset(F,1);
BlockRead(F,Check^,BufferSize,RR);
Close(F);
If (IoResult<>0) or
   (RR=0)
   Then Begin
        Dispose(Check);
        Exit;
        End;

ThisOne:=1;
Found:=False;
While (Not Found) And (ThisOne<=OPtr) Do
 Begin
 OList[ThisOne]^.FileName :=_FileName;
 Found:=OList[ThisOne]^.IsThisTypeFile(Check^,RR);
 If Not Found Then Inc(ThisOne);
 End;

If found
   Then Begin
        _CO:=OList[ThisOne];
        _CO^.Filename:=_FileName;
        End
   Else _CO:=NIL;

Dispose(Check);
DetectCompressor:=Found;
End;

procedure InitVariables;
begin
  InitBSC;

  InitArc;
  InitArj;
  InitDWC;
  InitHyp;
  InitLHA;
  InitMdCD;
  InitZIP;
  InitRAR;
  InitSQZ;
  InitZOO;
end; { proc. InitVariables }

End.

