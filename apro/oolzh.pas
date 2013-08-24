{$S-,R-,V-,I-,B-,F-,O-,A-}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{*********************************************************}
{*                    OOLZH.PAS 2.03                     *}
{*          Copyright (c) TurboPower Software            *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OoLzh;
  {-Objects for working with LZH files}

interface

uses
  Dos,
  {$IFDEF UseOpro}
  OpString,
  OpRoot,
  OpInline,
  OpDos,
  {$ENDIF}
  {$IFDEF UseTPro}
  TpMemChk,
  TpString,
  TpDos,
  {$ENDIF}
  ApMisc,
  OoArchiv;

const
  {compression method codes}
  cmcStored    = '0';             {stored (no compression)}
  cmcFrozen1   = '1';             {shrunk1}
  cmcFrozen2   = '5';             {shrunk2}
  OSID         = 'M';             {for MS-DOS - internal}

var
  NextDisplayInc : Word;                 {Increment value for displays}

type
  CompressionMode  = (cmBestMethod, cmStored, cmFrozen1, cmFrozen2);

  OS               =
    record
      O, S : Word;
    end;

  HeadIdType       = array[1..5] of Char;  {A five charcter string that
                     indicates what version was used to compress the file,
                     such as -lh1-, -lh5-, ...}

  {LZH header}
  LzhHeader =
    record
      HeadSize      : Byte;           {size of header}
      HeadChk       : Byte;           {checksum for header}
      HeadID        : HeadIdType;     {compression type tag}
      NewSize       : LongInt;        {compressed size}
      OrigSize      : LongInt;        {original size}
      Time          : Word;           {packed time}
      Date          : Word;           {packed date}
      Attr          : Byte;           {file attributes}
      Level         : Byte;           {=0 LZH method, =1 LHA method}
      FName         : PathStr;        {filename (variable length)}
      CRC           : Word;           {16-bit CRC (immediately follows FName)}

      OSID          : Char;           {=M for DOS - LHA method}

      PathHdrSize   : Word;           {path extended header size}
      PathHdrID     : Byte;           {=2 "Path" extended header flag}
      ExtFPath      : PathStr;        {pathname (variable length)}

      AttrHdrSize   : Word;
      AttrHdrID     : Byte;
      ExtAttr       : Word;

      FNameHdrSize  : Word;           {filename extended header size}
      FNameHdrID    : Byte;           {=$01 "FileName" extended header}
      ExtFName      : PathStr;        {filename (vriable length)}

      CRCHdrSize    : Word;           {=5 extended CRC Header}
      CRCHdrID      : Byte;           {=0 "CRC" extended header flag}
      ExtCRC        : Word;           {extended Header CRC value}

      NextHdrSize   : Word;           {=0 No more extended headers}
    end;

  LzhNodePtr = ^LzhNode;
  LzhNode =
    object
      lnNext    : LzhNodePtr;
      lnLH      : LzhHeader;
      lnFileOfs : LongInt;
      lnTagged  : Boolean;

      constructor Init(var LH : LzhHeader; FO : LongInt);
        {-Initialize node}
      destructor Done; virtual;
        {-Destroy node}
      procedure SetTag(On : Boolean);
        {-Tag/untag this node}
    end;

  LzhFileListPtr = ^LzhFileList;
  LzhFileList =
    object
      lfHead : LzhNodePtr;
      lfTail : LzhNodePtr;
      lfCount : Word;

      constructor Init;
        {-Initialize list}
      destructor Done; virtual;
        {-Destroy list}
      function Append(var LH : LzhHeader; FO : LongInt) : Boolean;
        {-Add a node to the list}
      procedure Delete(LNP : LzhNodePtr);
        {-Delete the specified node from the LzhFileList}
    end;

  UnLzhPtr = ^UnLzh;
  LzhPtr = ^Lzh;

  OkToWriteFunc = function(LNP : LzhNodePtr; var FName : PathStr; UP : UnLzhPtr) : Boolean;
  ShowNameProc = procedure(UP : UnLzhPtr);
  ShowMethodProc = procedure(LNP : LzhNodePtr; FName : PathStr; UP : UnLzhPtr);
  ExtractSuccessFunc = function(LNP : LzhNodePtr; FName : PathStr; UP : UnLzhPtr) : Boolean;
  ShowProgressFunc = function(UP : UnLzhPtr; BytesWritten, TotalBytes : LongInt) : Boolean;
  CompressSuccessFunc = function(LP : LzhPtr;
                                 LH : LzhHeader) : Boolean;
  OkToCompressFunc = function(LP : LzhPtr; NewFile : PathStr;
                              LH : LzhHeader) : Boolean;

  UnLzh =
    object(Archive)
      ulCrc        : Word;                 {running CRC counter}
      ulStart      : LongInt;
      ulOK         : OkToWriteFunc;
      ulShowLzh    : ShowNameProc;
      ulShowMethod : ShowMethodProc;
      ulSuccess    : ExtractSuccessFunc;
      ulProgress   : ShowProgressFunc;
      ulSaveMode   : Byte;                                             {!!.02}

      constructor Init(FName : PathStr);
        {-Initialize the archive and open the input file}

      procedure Extract(Mask : PathStr);
        {-Extract all files matching Mask}
      procedure ExtractFileMaskList(var FML : FileMaskList);
        {-Extract all files matching one of the masks in specified list}

      procedure BuildLzhFileList(var LFL : LzhFileList; var FML : FileMaskList);
        {-Build a list of files to be unarced}
      procedure ExtractLzhFileList(var LFL : LzhFileList);
        {-Extract all files in the LZH file list}

    {#Z+}
      function OkToWrite(LNP : LzhNodePtr; var FName : PathStr) : Boolean; virtual;
        {-Returns True if OK to write file associated with LNP}
      procedure ShowName; virtual;
        {-Called to display name of LZH file}
      procedure ShowMethod(LNP : LzhNodePtr; FName : PathStr); virtual;
        {-Called to display name of file being unarced and comp. method}
      function ExtractSuccess(LNP : LzhNodePtr; FName : PathStr) : Boolean; virtual;
        {-Called after file assoc. with LNP has been unarced}
      function ShowProgress(BytesWritten, TotalBytes : LongInt) : Boolean; virtual;
        {-Called when flushing output buffer to disk}
    {#Z-}

      procedure SetOkToWriteFunc(OKF : OkToWriteFunc);
        {-Set OK to write function}
      procedure SetShowNameProc(SNP : ShowNameProc);
        {-Set procedure to display LZH file name}
      procedure SetShowMethodProc(SMP : ShowMethodProc);
        {-Set procedure to display file name and compression method}
      procedure SetExtractSuccessFunc(ESF : ExtractSuccessFunc);
        {-Set procedure to call after file has been unarced}
      procedure SetShowProgressFunc(SPF : ShowProgressFunc);
        {-Set procedure to call to show progress}
      function lhaMemRequired(Compressing : Boolean) : LongInt;
        {-Return the amount of heap space required to compress or decompress}

      {+++ internal methods +++}
    {#Z+}
      procedure ulInitTables;
      procedure ulExtractFile(var LN : LzhNode; OutName : PathStr);
      function ulCalcHeaderHeadChk(var LH : LzhHeader) : Byte;
      function ulReadNextHeader(var LH : LzhHeader;
                                var FileOfs, FP : LongInt) : Boolean;
      procedure ulCopyPrim(BytesLeft : LongInt);
      function ulMeltPrim(BytesLeft : LongInt) : Word;
      function ulFlushOutputBuffer(BytesToWrite : Word) : Word;
      function lhaInit(Compressing : Boolean) : Boolean;
      procedure lhaCleanUp(Compressing : Boolean);
      procedure lhaExtractFile(var LN : LzhNode; OutName : PathStr);
    {#Z-}
    end;

  Lzh =
    object(UnLzh)
      lzhEndOfInFile : Boolean;           {True if no more data}
      lzhOrigFileSize : LongInt;          {Size of unfrozen file}
      lzhNewFile : Boolean;               {True if this is a new file}
      lzhProgressWidth : Word;            {Assumed width for DefShowProg}
      lzhOk : OkToCompressFunc;
      lzhSuccess : CompressSuccessFunc;
      lzhCompressMode : CompressionMode;

      constructor Init(FName : PathStr);
        {-Open an archive file FName}
      constructor Create(FName : PathStr);
        {-Create an archive file FName}
      procedure Compress(Mask : PathStr);
        {-Compress files meeting Mask}
      procedure CompressFileMaskList(var FML : FileMaskList);
        {-Compress all files matching masks in FML}
      procedure Delete(Mask : PathStr);
        {-Delete files matching Mask from archive}
      procedure DeleteFileMaskList(var FML : FileMaskList);
        {-Delete files in FML from archive}
      procedure FreshenArchive;
        {-Freshen all files in the current archive (via OkToCompress)}

    {#Z+}
      function OkToCompress(NewFile : PathStr; LH : LzhHeader) : Boolean; virtual;
        {-Returns True if OK to compress file NName}
      function CompressSuccess(LH : LzhHeader) : Boolean; virtual;
        {-Called after file assoc. with LNP has been unarced}
    {#Z-}

      procedure SetOkToCompressFunc(OKC : OkToCompressFunc);
        {-Set OK to compress function}
      procedure SetCompressSuccessFunc(CSF : CompressSuccessFunc);
        {-Set procedure to call after file has been arced}
      procedure SetProgressWidth(Width : Word);
        {-Set assumed width of window for DefShowProgressFunc}
      procedure SetCompressionMode(CM : CompressionMode);
        {-Set which compression mode to use}

      {+++ internal methods +++}
    {#Z+}
      procedure lzhWriteBlock; virtual;
      procedure lzhRestoreArchive;
      procedure lzhPrepareForUpdate;
      procedure lzhDoneUpdate;
      procedure lzhPutC(W : Word);
      procedure lzhPutCode(L : Integer; C : Word);
      function lzhGetC(var C : Byte) : Boolean;
      procedure lzhEncodeChar(C : Integer);
      procedure lzhEncodePosition(C : Integer);
      procedure lzhEncodeEnd;
      procedure lzhStoreFile;
      procedure lzhCreateHeader(var F : File; FileName : PathStr;
                                var LH : LzhHeader);
      procedure lzhFreezeFile(FName : PathStr; var LH : LzhHeader);
      procedure lzhWriteHeader(var LH : LzhHeader; var FO, FP : LongInt);
      procedure lzhCopyFilePrim(var LH : LzhHeader; FO : LongInt);
      procedure lzhCopyFile(FP : LongInt);
      function lzhCompareNodes(Node1 : FileMaskNodePtr;
                               Node2 : LzhNodePtr) : Boolean;
      procedure lhaFreezeFile(FName : PathStr; var LH : LzhHeader);
      function lhaCalcExtendedHeaderCRC(var LH : LzhHeader) : Word;
    {#Z-}
    end;

{#Z-}

procedure DefShowMethodProc(LNP : LzhNodePtr; FName : PathStr; UP : UnLzhPtr);
  {-Default ShowMethod procedure}

function DefExtractSuccessFunc(LNP : LzhNodePtr; FName : PathStr;
                               UP : UnLzhPtr) : Boolean;
  {-Default ExtractSuccess function}

procedure DefShowNameProc(UP : UnLzhPtr);
  {-Default ShowName procedure}

function DefShowProgressFunc(UP : UnLzhPtr; BytesWritten, TotalBytes : LongInt) : Boolean;
  {-Default function to show progress}

function DefOkToWriteFunc(LNP : LzhNodePtr; var FName : PathStr; UP : UnLzhPtr) : Boolean;
  {-Default function to get permission to extract file}

function DefCompressSuccessFunc(LP : LzhPtr;
                                LH : LzhHeader) : Boolean;
  {-Default CompressSuccess function}

function DefOkToCompressFunc(LP : LzhPtr; NewFile : PathStr;
                             LH : LzhHeader) : Boolean;
  {-Default function to say OK to compress NewName}

{#Z+}

  {==========================================================================}

implementation

  {$I OOLZH.PA1}            {Tables and declarations}
  {$I OOLZH.PA2}            {Compression/decompression routines}
  {$I OOLZH.PA3}            {LHA compression method routines}

end.
