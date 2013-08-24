{$S-,R-,V-,I-,B-,F+,O-,A-}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{*********************************************************}
{*                    OOZIP.PAS 2.03                     *}
{*           Copyright (c) TurboPower Software.          *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit OoZip;
  {-Objects for working with ZIP files}

interface

uses
  Dos,
  {$IFDEF UseOpro}
  OpInline,
  OpRoot,
  OpString,
  {$ENDIF}
  {$IFDEF UseTPro}
  TpInline,
  TpMemChk,
  TpString,
  {$ENDIF}
  ApMisc,
  OoArchiv;

type
  LocalHeader =
    record
      {... signature ...}         {= LocalHeaderSig}
      VersionNeeded : Word;       {version needed to extract}
      BitFlag : Word;             {general purpose bit flag}
      Method : Word;              {compression method: 0-8}            {!!.01}
      LastModTime : Word;         {time file was last modified}
      LastModDate : Word;         {date "}
      Crc : LongInt;              {32-bit CRC}
      NewSize : LongInt;          {compressed size of file}
      OrigSize : LongInt;         {uncompressed size of file}
      NameLength : Word;          {length of filename}
      ExtraLength : Word;         {length of extra field}
      {... filename (variable size)     ...}
      {... extra field (variable size)  ...}
      {... compressed data ...}
    end;

  CentralDirHead =
    record
      {... signature ...}         {= CentralDirHeadSig}
      VersionMade : Word;         {version created by}
      VersionNeeded : Word;       {version needed to extract}
      BitFlag : Word;             {general purpose bit flag}
      Method : Word;              {compression method: 0..8}           {!!.01}
      LastModTime : Word;         {time file was last modified}
      LastModDate : Word;         {date "}
      Crc : LongInt;              {32-bit CRC}
      NewSize : LongInt;          {compressed size of file}
      OrigSize : LongInt;         {uncompressed size of file}
      NameLength : Word;          {length of filename}
      ExtraLength : Word;         {length of extra field}
      CommentLength : Word;       {length of comment for file}
      DiskNumberStart : Word;     {number of disk on which file begins}
      InternalAttrs : Word;       {internal file attributes, low bit = Text}
      ExternalAttrs : LongInt;    {external file attributes, DOS dir attr}
      LocalHeaderOfs : LongInt;   {relative offset of local header for file}
      {... filename (variable size)     ...}
      {... extra field (variable size)  ...}
      {... file comment (variable size) ...}
    end;

  CentralDirTail =
    record
      {... signature ...}         {= CentralDirTailSig}
      DiskNumber : Word;          {disk number for this disk}
      CentralDirDisk : Word;      {number of disk with start of central dir}
      EntriesThisDisk : Word;     {total entries in directory on this disk}
      TotalEntries : Word;        {total entries in central directory}
      CentralDirSize : LongInt;   {size of the central directory}
      CentralDirOfs : LongInt;    {offset of start of central dir with respect
                                   to starting disk number}
      CommentLength : Word;       {length of ZIP file comment}
      {... zip file comment (variable size) ...}
    end;

  ZipComment = array[1..65521] of Char; {a comment in a ZIP file}
  CommentPtr = ^ZipComment;
  ExtraPtr = ^ZipComment;

  CompressionMode =                                                    {!!.01}
    (cmBestMethod, cmStore, cmShrink, cmImplode, cmDeflate);           {!!.01}

  {#Z+} {!!.01}{begin}
  ByteArray        = array[0..65535-1] of Byte;
  ByteArrayPtr     = ^ByteArray;
  IntegerArray     = array[0..65535 div SizeOf(Integer)-1] of Integer;
  IntegerArrayPtr  = ^IntegerArray;

  ppHuft           = ^pHuft;
  pHuft            = ^Huft;
  Huft             = record
    ExtraBits      : Byte;   {Number of extra bits}
    NumBits        : Byte;   {Number of bits in this code or subcode}
    case Byte of
      0: (N        : Word);  {Literal, length base, or distance base}
      1: (NextLevel: pHuft); {Pointer to next level of table}
  end;

  FCData = record
    case Byte of
      0 : (Freq : Word);  {frequency count}
      1 : (Code : Word);  {bit string}
  end;

  DLData = record
    case Byte of
      0 : (Dad : Word);  {father node in Huffman tree}
      1 : (Len : Word);  {length of bit string}
  end;

  {Data structure describing a single value and its code string}
  CTData = record
    FC : FCData;
    DL : DLData;
  end;
  CTDataArray = array[0..65535 div SizeOf(CTData) - 1] of CTData;
  CTDataArrayPtr = ^CTDataArray;
  {#Z-} {!!.01}{end}

const
  {compression method codes}
  cmcStored    = 0;               {stored (no compression)}
  cmcShrunk    = 1;               {shrunk}
  cmcReduced1  = 2;               {reduced - factor of 1}
  cmcReduced2  = 3;               {reduced - factor of 2}
  cmcReduced3  = 4;               {reduced - factor of 3}
  cmcReduced4  = 5;               {reduced - factor of 4}
  cmcImploded  = 6;               {imploded}
  cmcTokenized = 7;               {tokenized}                          {!!.01}
  cmcDeflated  = 8;               {deflated}                           {!!.01}

  {#Z+}
  ImpMaxMatchLen = 320;           {Implode max match length}
  {#Z-}

type
  ZipNodePtr = ^ZipNode;
  ZipNode =
    object
      znNext   : ZipNodePtr;
      znCDH    : CentralDirHead;
      znFName  : PathStr;
      znCP     : CommentPtr;
      znEP     : ExtraPtr;
      znTagged : Boolean;

      constructor Init(var CDH   : CentralDirHead;
                       var CP    : CommentPtr;
                       var FName : PathStr);
        {-Initialize node}
      destructor Done; virtual;
        {-Destroy node}
      procedure SetTag(On : Boolean);
        {-Tag/untag this node}
    end;

  ZipFileListPtr = ^ZipFileList;
  ZipFileList =
    object
      zfHead : ZipNodePtr;
      zfTail : ZipNodePtr;
      zfCount : Word;

      constructor Init;
        {-Initialize list}
      destructor Done; virtual;
        {-Destroy list}
      function Append(var CDH   : CentralDirHead;
                      var CP    : CommentPtr;
                      var FName : PathStr) : Boolean;
        {-Add a node to the list}
      procedure Delete(ZNP : ZipNodePtr);
        {-Delete the specified node from the ZipFileList}
    end;

  UnZipPtr = ^UnZip;
  OkToWriteFunc = function(ZNP : ZipNodePtr; var FName : PathStr; UP : UnZipPtr) : Boolean;
  ShowCommentsProc = procedure(CP : CommentPtr; CLen : Word; UP : UnZipPtr);
  ShowMethodProc = procedure(ZNP : ZipNodePtr; FName : PathStr; UP : UnZipPtr);
  ExtractSuccessFunc = function(ZNP : ZipNodePtr; FName : PathStr; UP : UnZipPtr) : Boolean;
  ShowProgressFunc = function(UP : UnZipPtr; BytesWritten, TotalBytes : LongInt) : Boolean;

  UnZip =
    object(Archive)
      uzCrc        : LongInt;              {running CRC counter}
      uzTailPos    : LongInt;
      uzOK         : OkToWriteFunc;
      uzShowZip    : ShowCommentsProc;
      uzShowMethod : ShowMethodProc;
      uzSuccess    : ExtractSuccessFunc;
      uzProgress   : ShowProgressFunc;
      uzSaveMode   : Byte;                                             {!!.02}

      constructor Init(FName : PathStr);
        {-Initialize the archive and open the input file}

      procedure Extract(Mask : PathStr);
        {-Extract all files matching Mask}
      procedure ExtractFileMaskList(var FML : FileMaskList);
        {-Extract all files matching one of the masks in specified list}

      procedure BuildZipFileList(var ZFL : ZipFileList; var FML : FileMaskList);
        {-Build a list of ZIPped files to be unzipped}
      procedure ExtractZipFileList(var ZFL : ZipFileList);
        {-Extract all files in the zip file list}

    {#Z+}
      function OkToWrite(ZNP : ZipNodePtr; var FName : PathStr) : Boolean; virtual;
        {-Returns True if OK to write file associated with ZNP}
      procedure ShowComments(CP : CommentPtr; CLen : Word); virtual;
        {-Called to display name of zip file and zip file comments}
      procedure ShowMethod(ZNP : ZipNodePtr; FName : PathStr); virtual;
        {-Called to display name of file being unzipped and comp. method}
      function ExtractSuccess(ZNP : ZipNodePtr; FName : PathStr) : Boolean; virtual;
        {-Called after file assoc. with ZNP has been unzipped}
      function ShowProgress(BytesWritten, TotalBytes : LongInt) : Boolean; virtual;
        {-Called when flushing output buffer to disk}
    {#Z-}

      procedure SetOkToWriteFunc(OKF : OkToWriteFunc);
        {-Set OK to write function}
      procedure SetShowCommentsProc(SZCP : ShowCommentsProc);
        {-Set procedure to display zip file name and comments}
      procedure SetShowMethodProc(SMP : ShowMethodProc);
        {-Set procedure to display file name and compression method}
      procedure SetExtractSuccessFunc(ESF : ExtractSuccessFunc);
        {-Set procedure to call after file has been unzipped}
      procedure SetShowProgressFunc(SPF : ShowProgressFunc);
        {-Set procedure to call to show progress}

      {+++ internal methods +++}
    {#Z+}
      procedure uzSkipData(N : LongInt);
        {-Skip the next N bytes of data in the ZIP file and return IoResult}
      procedure uzReadSignature(var Sig : LongInt);
        {-Read a signature from the ZIP file}
      procedure uzReadString(L : Word; var S : String; Max : Byte);
        {-Read a string of length L from the ZIP file}
      procedure uzReadLocalHeader(var LH : LocalHeader; var FName : PathStr);
        {-Read a local header from the ZIP file and return IoResult}
      procedure uzReadOneComment(var CP : CommentPtr; Len : Word);
        {-Allocate a CommentPtr and read a Comment into it}
      procedure uzReadOneExtraField(var EP : ExtraPtr; Len : Word);
        {-Allocate an ExtraPtr and read a ExtraField into it}
      procedure uzReadCentralDirHead(var CDH   : CentralDirHead;
                                     var FName : PathStr;
                                     var CP    : CommentPtr;
                                     var EP    : ExtraPtr);
        {-Read a central directory header}
      procedure uzReadCentralDirTail(var CDT : CentralDirTail; var CP : CommentPtr);
        {-Read the "header" that marks the end of the central directory}
      procedure uzFindCentralDir(var CDT : CentralDirTail);
        {-Move file pointer to beginning of central directory}
      procedure uzFindCentralDirTail;
        {-Move file pointer to tail of central directory}
      procedure uzReadNext;
        {-Read next byte from input stream}
      function uzReadBits(Bits : Byte) : Integer;
        {-Read the specified number of bits}
      procedure uzFlushOutputBuffer;
        {-Flush contents of output buffer}
      procedure uzWriteByte(B : Byte);
        {-Write one byte to the output stream}
      procedure uzUnStore;
        {-Extract a stored file}
      procedure uzUnReduce;
        {-Extract a file that was reduced}
      procedure uzUnShrink;
        {-Extract a file that was shrunk}
      procedure uzReadLengths(var T);
        {-Read bit lengths for a tree}
      procedure uzLoadTree(var T; TreeSize : Integer);
        {-Load one Shannon-Fano tree}
      function uzReadTree(var T) : Byte;
        {-Read next byte using a Shannon-Fano tree}
      procedure uzUnImplode;
        {-Extract an imploded file}
      procedure uzExtractFile(var Header : CentralDirHead; OutName : PathStr);
        {-Extract a single file}
      procedure uzInflateFile;                                         {!!.01}
        {-Extract a deflated file}                                     {!!.01}
      procedure uzNeedBits(N : Byte);                                  {!!.01}
      function uzInflateCodes(tl, td: pHuft; bl, bd: Integer) : Boolean;{!!.01}
      function uzInflateStored: Boolean;                               {!!.01}
      function uzInflateFixed: Boolean;                                {!!.01}
      function uzInflateDynamic: Boolean;                              {!!.01}
      function uzInflateBlock (var E : Integer) : Boolean;             {!!.01}
    {#Z-}
    end;
{#Z+}

  ZipPtr = ^Zip;
  OkToCompressFunc = function(NewFile, OldFile: PathStr;
                              var CDH : CentralDirHead;
                              ZP : ZipPtr) : Boolean;
  CompressSuccessFunc = function(ZNode : ZipNodePtr;
                                 ZP : ZipPtr) : Boolean;
  FileCommentFunc = function(ZNode : ZipNodePtr;
                             var CP : CommentPtr;
                             var Len : Word;
                             ZP : ZipPtr) : Boolean;

  Zip =
    object(UnZip)
      zOkToCompress: OkToCompressFunc;
      zSuccess     : CompressSuccessFunc;
      zFileComment : FileCommentFunc;
      zFileIsText  : Boolean;             {True if current file is text}
      zNewFilePending : Boolean;          {True if new file just created}
      zFreshening  : Boolean;             {True when Freshening an archive}

      zCompressMode : CompressionMode;    {Mode requested}
      zCompressionUsed : Integer;         {Mode actualy used}
      zOptionFlags : Integer;  {General pourpose bit flags}            {!!.01}

      {Variables use to adjust implode compression vs speed}
      zMaxStringMatch : Integer;
      zMaxChainLength : Integer;

      zDeflateLevel   : Byte;                                          {!!.01}

      constructor Init(FName : PathStr);
        {-Initialize the archive and open the input file}
      constructor Create(FName : PathStr);
        {-Initialize the archive and create a new ZIP file}

      procedure CompressFileMaskList(var FML : FileMaskList);
        {-Compress all files that match the file mask list}
      procedure Compress(Mask : PathStr);
        {-Compress all files that match Mask}
      procedure FreshenArchive;
        {-Freshen all files in archive}
      procedure DeleteFileMaskList(var FML : FileMaskList);
        {-Delete files in archive that match any mask in FML}
      procedure DeleteFiles(Mask : PathStr);
        {-Delete files in archive that match any mask in FML}
      procedure SetZipComment(var Comment; Len : Word);
        {-Make Comment the current Zip file comment (Len = 0 deletes)}
      procedure UpdateCommentsFileMaskList(var FML : FileMaskList);
        {-Call FileComment hook for every that matches FML}

    {#Z+}
      function OkToCompress(NewFile, OldFile: PathStr;
                            var CDH : CentralDirHead) : Boolean; virtual;
        {-Returns True if OK to compress NewFile}
      function CompressSuccess(ZNode : ZipNodePtr) : Boolean; virtual;
        {-Called after file assoc. with ZNP has been compressed}
      function SetFileComment(ZNode : ZipNodePtr;
                              var CP : CommentPtr;
                              var Len : Word) : Boolean;  virtual;
        {-Called to after compression to add a file comment}
      {#Z-}

      procedure SetOkToCompressFunc(OKC : OkToCompressFunc);
        {-Set an OktoCompress function}
      procedure SetCompressSuccessFunc(CSF : CompressSuccessFunc);
        {-Set a CompressSuccess function}
      procedure SetFileCommentFunc(FCF : FileCommentFunc);
        {-Set a FileComment function}
      procedure SetCompressionMode(Mode : CompressionMode);
        {-Set the mode of compression}
      procedure SetImplodeFactors(MatchLength, Reps : Integer);
        {-Set factors that affect speed and compression}
      procedure SetDeflateLevel(Level : Byte);                         {!!.01}
        {-Set factors that affect speed and compression (deflate)}     {!!.01}

      {+++ internal methods +++}
    {#Z+}
      function SkipNewZip(N : LongInt) : Word;
      function CheckForText : Boolean;
      function WriteZip(var Block; Size : Word) : Boolean;
      procedure RestoreArchive;
      procedure PrepareForUpdate;
      procedure DoneUpdate;
      function AdjustName(FName : String) : String;
      procedure WriteCentralDir(ZFL : ZipFileList);
      procedure PutCode(Code : Integer);
      procedure LzwTableClear;
      procedure AdaptiveReset(Suffix : Word);
      function GetC(var C : Word) : Boolean;
      procedure ShrinkFile;
      procedure StoreFile;
      procedure CompressFile(ZNode : ZipNodePtr);
      function CopyBlockPrim(Len : LongInt) : Boolean;
      procedure CopyFile(ZNode : ZipNodePtr);
      function AddFileToZip(var ZFL : ZipFileList; FNode : FileMaskNodePtr;
                            var ZNode : ZipNodePtr) : Boolean;
      procedure RemoveFileFromZip(var ZFL : ZipFileList; var ZNode : ZipNodePtr);

      procedure CodeTreeAlloc(Size: Integer; var Handle: Integer);
      function TempWrite(Buf: ByteArrayPtr; Bytes: Word): Word;
      procedure ZipCodeTree(Handle: Integer; var Result: ByteArrayPtr);
      procedure WriteCodeTrees;
      procedure MakeCodeTrees;
      procedure BitSRLOut(Value, Length: Integer);
      procedure OutputCode(Value, Tree: Integer);
      procedure BitSDone;

      procedure ImplodeFile;
      procedure ProcessImplode(var B; Size: Word);
      procedure ProcessLongestMatch(Size: Word);
      procedure LongestMatchDone;
      procedure SetMinMatchLength(var B; Size: Word);
      function ImplodedSize: LongInt;
      procedure WriteMatch(MaStart: Word; MaLength: Integer);
      procedure WriteData;
      function TempRead(Buf: ByteArrayPtr; Bytes: Word): Word;
      procedure OutputImplode;

      procedure DeflateFile(Level : Integer);                          {!!.01}
      procedure AllocateDeflateBuffers;                                {!!.01}
      function  dReadBuf(var Buffer; Size : Word) : Word;              {!!.01}
      procedure FillWindow;                                            {!!.01}
      procedure LongestMatchInit(PackLevel : Integer;                  {!!.01}
                                 var Flags : Integer);                 {!!.01}
      function  FlushBlock(Buf : ByteArrayPtr; StoredLen : LongInt;    {!!.01}
                           LastBlock : Boolean) : LongInt;             {!!.01}
      function  Deflate : LongInt;                                     {!!.01}
      procedure dFlushOutbuf(Value, Bytes : Word);                     {!!.01}
      procedure dPutWord(W : Word);                                    {!!.01}
      procedure dPutByte(B : Byte);                                    {!!.01}
      procedure dSendBits(Value, Length : Integer);                    {!!.01}
      procedure dBitsDone;                                             {!!.01}
      procedure dCopyBlock(Buf : ByteArrayPtr; Len : Word;             {!!.01}
                           Header : Boolean);                          {!!.01}
      procedure dCompressBlock(LTree, DTree : CTDataArrayPtr);         {!!.01}
      procedure dSendTree(Tree : CTDataArrayPtr; MaxCode : Integer);   {!!.01}
      procedure dSendAllTrees(LCodes, DCodes, BLCodes : Integer);      {!!.01}
      procedure SetFileType;                                           {!!.01}
   {#Z-}
  end;

procedure DefShowMethodProc(ZNP : ZipNodePtr; FName : PathStr; UP : UnZipPtr);
  {-Default ShowMethod procedure}

function DefExtractSuccessFunc(ZNP : ZipNodePtr; FName : PathStr; UP : UnZipPtr) : Boolean;
  {-Default ExtractSuccess procedure}

procedure DefShowCommentsProc(CP : CommentPtr; CLen : Word; UP : UnZipPtr);
  {-Default ShowComments procedure}

function DefShowProgressFunc(UP : UnZipPtr; BytesWritten, TotalBytes : LongInt) : Boolean;
  {-Default function to show progress}

function DefOkToCompressFunc(NewFile, OldFile: PathStr;
                             var CDH : CentralDirHead;
                             ZP : ZipPtr) : Boolean;
  {-Default OkToCompress function}

function DefCompressSuccessFunc(ZNode : ZipNodePtr;
                                ZP : ZipPtr) : Boolean;
  {-Default CompressSuccess function}

function DefFileCommentFunc(ZNode : ZipNodePtr;
                            var CP : CommentPtr;
                            var Len : Word;
                            ZP : ZipPtr) : Boolean;
  {-Default FileComment procedure }

function DefOkToWriteFunc(ZNP : ZipNodePtr; var FName : PathStr;
                          UP : UnZipPtr) : Boolean;
  {-Default OkToWrite function}

{#Z-}

  {==========================================================================}

implementation

  {$I OOZIP.PA1}      {Declarations, decompression routines}
  {$I OOZIP.PA2}      {Compression routines}
  {$I OOZIP.PA3}      {Implode routines}
  {$I OOZIP.PA4}      {Deflate/Inflate routines}

begin
  UpdateCrcAddr := @UpdateCrc;
end.
