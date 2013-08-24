{$A+,F+,I-,R-,S-,V-}

{$IFDEF BindFaxFont}
{$O-}
{$ELSE}
{$O+}
{$ENDIF}

{$I APDEFINE.INC}

{Include OPro's define file if UseOPro is specified}
{$IFDEF UseOPro}
{$I OPDEFINE.INC}
{$ENDIF}

{******************************************************}
{*                 OOFAXCVT.PAS  2.03                 *}
{*      Copyright (c) TurboPower Software 1993.       *}
{*                All rights reserved.                *}
{******************************************************}

unit OoFaxCvt;
  {-Text/PCX/DCX/TIFF to fax, general fax unpacker}

interface

uses
  Dos,
  {$IFDEF UseOPro}
  OpInline,
  OpRoot,
  OpString,
  {$IFDEF Opro12}
  OpConst,
  {$ENDIF}
  {$ELSE}
  {$IFDEF UseTPro}
  TpInline,
  TpMemChk,
  TpString,
  {$ENDIF}
  {$ENDIF}
  ApMisc;

type
  SigArray = Array[0..5] of Char;        {Fax file signature array}
  ResWidths = (rw1728, rw2048);          {Acceptable fax widths}

const
  {General}
  UpdateInterval : Word = 16;             {Lines per status call}
  DefSig         : SigArray = 'APF10'#26; {Default fax file signature}
  AproFontName   : PathStr = 'APFAX.FNT'; {Name of standard font file}
  DefLeftMargin  : Word = 50;             {Default left margin of 1/4 inch}
  DefTopMargin   : Word = 0;              {Default top margin of zero}
  TabStop        : Word = 4;              {Default tab stops}

  {Fax pixel widths}
  StandardWidth  = 1728;          {Standard width in pixels}
  WideWidth      = 2048;          {Allowed higher resolution}

  {Option flags for FAX page header}
  ffHighRes     = $0001;          {Image stored in high-res mode}
  ffHighWidth   = $0002;          {Image uses option high-width mode}
  ffLengthWords = $0004;          {Set if raster lines include length word}

  {Options for fax conversion}
  fcDoubleWidth = $0001;          {Double the horizontal width in std res}
  fcCenterImage = $0002;          {Center graphics images horizontally}

  {Default fax conversion options}
  DefFaxCvtOptions  : Word = fcDoubleWidth + fcCenterImage;

  {No bad options}
  BadFaxCvtOptions = 0;

  {Options for unpacking}
  ufHalfWidth   = $0001;     {Halve the horizontal width while unpacking}

  {No default options}
  DefUnpackOptions  : Word = 0;

  {No bad options}
  BadUnpackOptions = 0;

  {.Z+}{Private constants}
  {Maximum number of tree records}
  MaxTreeRec = 306;

  {HP soft font limits}
  MaxCharData  = 1024;                  {Max bytes of HP bitmap data}
  MaxFullFont  = 2048;                  {Max bytes of HP full bitmap data}
  MaxTempFont  = 512;                   {Max bytes in temp font array}

  {Max size of decompress buffer}
  MaxData = 2048;

  {Font handles, same value as bytes-per-char}
  SmallFont = 16;
  StandardFont = 48;

  {For checking/setting bits}
  Mask : array[1..8] of Byte = ($80, $40, $20, $10, $08, $04, $02, $01);
  {.Z-}

  {!!.03 - New}
  MaxCodeTable   = 63;
  MaxMUCodeTable = 39;

type
  {.Z+}{Private types}
  Str20 = String[20];

  {Generic byte buffer type}
  PByteBuffer = ^TByteBuffer;
  TByteBuffer = array[0..$FFF0] of Byte;

  PBitBuffer = ^TBitBuffer;
  TBitBuffer = array[1..MaxFullFont] of Byte;

  {Compression code tables}
  CodeRec = record
    Code : Word;
    Sig  : Word;
  end;

  TermCodeArray   = array[0..MaxCodeTable] of CodeRec;    {!!.03}
  MakeUpCodeArray = array[0..MaxMUCodeTable] of CodeRec;  {!!.03}

  {Stores information about our fonts}
  FontRecord = record
    Bytes  : Byte;  {# of bytes per char in font}
    PWidth : Byte;  {width of font in pixels}
    Width  : Byte;  {width of font in bytes (e.g. 16-pixel-wide = 2)}
    Height : Byte;  {height of font in raster lines}
  end;

  {Used for rasterizing text}
  TRasterMatrix = Array[0..1023] of Byte;

  {APRO fax file header record}
  FaxHeaderRec = record
    Signature  : SigArray;               {APRO FAX signature}
    FDateTime  : LongInt;                {Date and time in DOS format}
    SenderID   : String[20];             {Station ID of sender}
    Filler     : Byte;                   {Alignment byte, unused}
    PageCount  : Word;                   {Number of pages in this file}
    PageOfs    : LongInt;                {Offset in file of first page}
    Padding    : Array[39..64] of Byte;  {Expansion room}
  end;

  {APRO fax page header record}
  PageHeaderRec = record
    ImgLength : LongInt;                 {Bytes of image data in this page}
    ImgFlags  : Word;                    {Image flags for width, res, etc}
    Padding   : Array[7..16] of Byte;    {Expansion room}
  end;

  {Pcx header}
  PcxPalArray = Array[0..47] of Byte;
  PcxHeaderRec = record
    Mfgr      : Byte;
    Ver       : Byte;
    Encoding  : Byte;
    BitsPixel : Byte;
    XMin      : Word;
    YMin      : Word;
    XMax      : Word;
    YMax      : Word;
    HRes      : Word;
    VRes      : Word;
    Palette   : PcxPalArray;
    Reserved  : Byte;
    Planes    : Byte;
    BytesLine : Word;
    PalType   : Word;
    Filler    : Array[1..58] of Byte;  {pad to 128 bytes}
  end;

  DcxOfsArray = array[1..1024] of LongInt;

  DcxHeaderRec = record
    ID      : LongInt;
    Offsets : DcxOfsArray;
  end;

  {!!.03 - New}
  {Bmp header}
  BmpHeaderRec = record
    FileType     : Word;
    FileSize     : LongInt;
    Reserved1    : Word;
    Reserved2    : Word;
    ImageDataOfs : LongInt;
  end;

  {!!.03 - New}
  BmpInfoHeaderRec = record
    HeaderSize     : LongInt;
    BmpWidth       : LongInt;
    BmpHeight      : LongInt;
    NumPlanes      : Word;
    BitsPerPixel   : Word;
    CompressMethod : LongInt;
    BitmapSize     : LongInt;
    HorzRes        : LongInt;
    VertRes        : LongInt;
    ColorsUsed     : LongInt;
    NumSigColors   : LongInt;
  end;

  {Portion of HP font file header we care about}
  TFontHdr = record
    FontDescSize : Integer;
    DescFormat   : Byte;
    FontType     : Byte;
    StyleMSB     : Byte;
    Reserve1     : Byte;
    BaseLine     : Integer;
    CellWidth    : Integer;
    CellHeight   : Integer;
    Orientation  : Byte;
    Spacing      : Byte;
  end;

  {HP character header}
  TCharHdr = record
    C4         : Byte;
    Continue   : Byte;
    CSize      : Byte;
    CClass     : Byte;
    Orientation: Byte;
    Reserve1   : Byte;
    LeftOffset : Integer;
    TopOffset  : Integer;
    CharWidth  : Integer;
    CharHeight : Integer;
    DeltaX     : Integer;
  end;

  TreeRec = record
    Next0 : Integer;
    Next1 : Integer;
  end;
  TreeArray = array[0..MaxTreeRec] of TreeRec;
  TreePtr = ^TreeArray;
  {.Z-}

  {Base converter object, used by fax send engine}
  AbstractFaxConverterPtr = ^AbstractFaxConverter;

  {Hook types}
  FaxCvtStatusFunc = function (AF : AbstractFaxConverterPtr;
                                Starting, Ending : Boolean) : Boolean;
  AbstractFaxConverter =
    object(Root)
      CID          : Byte;               {ID of current character}
      FontLoaded   : Boolean;            {False until font loaded}
      DoubleWidth  : Boolean;            {True for PCX files/dbl width lines}
      UseHighRes   : Boolean;            {True if for high-res mode}
      HighResFont  : Boolean;            {True if using high-res font}
      Fill1        : Byte;               {Keep word alignment}
      fcFlags      : Word;               {Option flags}
      ByteOfs      : Word;               {Byte offset in buffer}
      BitOfs       : Word;               {Bit offset in buffer}
      ResWidth     : Word;               {Width of current resolution in pixels}
      BytesPerRow  : Word;               {Bytes per row in fullfont}
      CharDataLen  : Word;               {Number of bytes in bitmap}
      FaxLeftMargin: Word;               {Left margin in pixels}
      FaxTopMargin : Word;               {Right margin in pixels}
      CurrPage     : Integer;            {Current page being processed}
      LineCount    : Integer;            {Number of text lines between page}
      CurrLine     : LongInt;            {Current line of text in file}
      DataLine     : PByteBuffer;        {Buffered line of compressed data}
      TmpBuffer    : PByteBuffer;        {Temp compression buffer}
      FontPtr      : PByteBuffer;        {Pointer to the loaded font table}
      CharData     : PBitBuffer;         {HPCharacter bitmap}
      FullFont     : PBitBuffer;         {Full dimension bitmap}
      LineMatrix   : TRasterMatrix;      {Rasterizing buffer}
      MainHeader   : FaxHeaderRec;       {Record of current fax info}
      PageHeader   : PageHeaderRec;      {Record of current page info}
      FontRec      : FontRecord;         {Holds current font info}
      Font         : File;               {HP font file}
      OutFile      : File;               {Output file}
      FontHdr      : TFontHdr;           {Holds font file header}
      CharHdr      : TCharHdr;           {Holds current character header}
      ScaleCol     : array[1..20] of Word; {For scaling HP fonts}
      ScaleRow     : array[1..32] of Word; {For scaling HP fonts}
      StationID    : Str20;              {FLID of this station}
      FaxPath      : PathStr;            {Path for storage of files}
      OutFileName  : PathStr;            {Name of current output file}
      InFileName   : PathStr;            {Name of current input file}
      StatusP      : FaxCvtStatusFunc;   {Status proc hook}
      SaveMode     : Byte;               {Save FileMode}               {!!.02}

      {Constructors/destructors}
      constructor Init;
      destructor Done; virtual;

      {Option management}
      procedure fcOptionsOn(OptionFlags : Word);
        {-Activate multiple options}
      procedure fcOptionsOff(OptionFlags : Word);
        {-Deactivate multiple options}
      function fcOptionsAreOn(OptionFlags : Word) : Boolean;
        {-Return True if all specified options are on}

      {User control}
      procedure SetStatusFunc(FCF : FaxCvtStatusFunc);
        {-Set our status proc}
      function LoadFont(FontHandle : Byte; HiRes : Boolean) : Boolean; virtual;
        {-Load selected font from APFAX.FNT or memory}
      function LoadHPFont(FontName : PathStr; HiRes : Boolean) : Boolean;
        {-Load font from HP soft font file FontName}
      procedure SetPageSize(PS : Integer);
        {-Set paging size for text faxes}
      procedure SetResolutionMode(HiRes : Boolean);
        {-Select low- or high-resolution mode}
      procedure SetFaxPath(PS : PathStr);
        {-Set path for storage of fax files}
      procedure SetResolutionWidth(RW : ResWidths);
        {-Select resolution width for this fax}
      procedure SetStationID(SID : Str20);
        {-Set the sending Station ID for this fax}
      procedure SetMargins(Left, Top : Word);
        {-Set top and left margins for converted documents}
      procedure GetStatusInfo(var Line : LongInt; var Page : Integer);
        {-Return current line/page}
      function GetFileName : PathStr;
        {-Return the name of the input file}
      procedure ConvertFax(FName : PathStr); virtual;
        {-Convert file to our FAX output format}

      {Virtual method hooks}
      function FaxConvertStatus(Starting, Ending : Boolean) : Boolean; virtual;
        {-Display convert status}

      {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Abstract Load}
      procedure Store(var S : IdStream);
        {-Abstract Store}
      {$ENDIF}

      {.Z+}{Private methods}
      procedure acRawInit;
        {-Init fields}

      {HP font manipulation}
      procedure acConvertChar(C : Byte);
        {-Convert HP character data to full bitmap}
      procedure acScaleChar(C : Byte);
        {-Convert full bitmap to scaled bitmap}
      function acReadCommandNumber : LongInt;
        {-Read a number from a parameterized escape sequence}
      function acReadHPFontHeader : Boolean;
        {-Read header and return True if successful}
      function acReadCharID : Byte;
        {-Read the charcter ID command}
      function acReadCharDef : Boolean;
        {-Read the character header}

      {File management}
      procedure acInitHeaders;
        {-Initialize fax and page headers}
      function acOpenInputFile : Integer; virtual;
        {-Open the input file,  MUST be overridden}
      function acCreateOutputFile(FN : PathStr) : Integer;
        {-Create an output file in our format}
      function acUpdateMainHeader : Integer;
        {-Update the file's header with the contents of the main header record}
      function acUpdatePageHeader(PgNo : Word;
                                var PgInfo : PageHeaderRec) : Integer;
        {-Update the contents of the PgNo-th page header in the file}
      function acFindPage(PgNo : Word) : Integer;
        {-Find the start of the PgNo-th page in the file}

      {Rasterization}
      {!!.03 - added}
      procedure acCompressStringRowPrim(S : String; Row : Byte; LenWord : Boolean);
        {-Compress one raster row in a string of text, not copying to other buffer}
      {!!.01 - used to be CompressString}
      procedure acCompressStringRow(S : String;
                                    Row : Byte;
                                    var Buffer;
                                    var Len : Word;
                                    LenWord : Boolean);
        {-Compress one raster row in a string of text}
      procedure acMakeEndOfPage(var Buffer; var Len : Word);
        {-Generate an end-of-page sequence and place in buffer}
      procedure acInitDataLine;
      procedure acAddCodePrim(Code : Word; SignificantBits : Word);
      procedure acAddCode(RunLen : Word; IsWhite : Boolean);
      procedure acCompressRasterLine(var Buffer);
      function acAddData(var Buffer; Len : Word; DoInc : Boolean) : Boolean;
      function acAddLine(var Buffer; Len : Word) : Boolean;
      function acAddBlankLines(Count : Word) : Boolean;
      {.Z-}
    end;

type
  {Text-to-fax converter engine object}
  TextFaxConverterPtr = ^TextFaxConverter;
  TextFaxConverter =
    object(AbstractFaxConverter)
      ReadBuffer   : PByteBuffer;        {Buffer for reads}
      InFile       : Text;               {Input file}

      {Constructor/destructors}
      constructor Init;
      destructor Done; virtual;

      {User control}
      procedure ConvertFax(FName : PathStr); virtual;
        {-Convert text file to our FAX output format}

      {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Abstract Load}
      {$ENDIF}

      {.Z+}{Private methods}
      function acOpenInputFile : Integer; virtual;
      {.Z-}
    end;

type
  AbstractPcxFaxConverterPtr = ^AbstractPcxFaxConverter;
  AbstractPcxFaxConverter =
    object(AbstractFaxConverter)
      CurrRBSize : Word;
      CurrRBOfs  : Word;
      ActBytesLine : Word;                                             {!!.01}
      ReadBuffer : PByteBuffer;
      PcxHeader  : PcxHeaderRec;
      PcxBytes   : LongInt;
      InFile     : File;

      constructor Init;
      destructor Done; virtual;

      {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Abstract Load}
      {$ENDIF}

      {.Z+} {++ private methods ++}
      function pcValidatePcxHdr : Boolean; virtual;
        {-check the current PCX file header for proper info}
      procedure pcReadRasterLine(var Buffer); virtual;
        {-read next PCX raster line into Buffer}
      procedure pcUnpackImage; virtual;
      {.Z-}
    end;

  {Pcx-to-fax converter engine object}
  PcxFaxConverterPtr = ^PcxFaxConverter;
  PcxFaxConverter =
    object(AbstractPcxFaxConverter)
      procedure ConvertFax(FName : PathStr); virtual;

      {.Z+} {++ private methods ++}
      function acOpenInputFile : Integer; virtual;
      {.Z-}
    end;

  {Dcx-to-fax converter engine object}
  DcxFaxConverterPtr = ^DcxFaxConverter;
  DcxFaxConverter =
    object(AbstractPcxFaxConverter)
      DcxHeader : DcxHeaderRec;
      DcxPgSz   : DcxOfsArray;
      DcxNumPag : Word;

      procedure ConvertFax(FName : PathStr); virtual;

      {.Z+} {++ private methods ++}
      function acOpenInputFile : Integer; virtual;
    end;

  {!!.01 new}
  {Tiff strip information}
  StripRecord = record
    Offset : LongInt;
    Length : LongInt;
  end;
  PStripInfo = ^StripInfo;
  StripInfo = array[1..(65521 div SizeOf(StripRecord))] of StripRecord;

  {Tiff converter}
  TiffFaxConverterPtr = ^TiffFaxConverter;
  TiffFaxConverter =
    object(AbstractFaxConverter)
      Intel        : Boolean;            {True if file is in Intel byte order}
      LastBitMask  : Byte;
      CurrRBSize   : Word;               {amount of data in buffer}
      CurrRBOfs    : Word;               {buffer index}
      tiffVersion  : Word;               {Version number from file preamble}
      tiffSubFile  : Word;               {TIFF tag field values for image:}
      tiffWidth    : Word;               {image width}
      tiffBytes    : Word;               {bytes per raster line}
      tiffLength   : Word;               {image length (height)}
      tiffComp     : Word;               {compression type}
      tiffPhotoMet : Word;               {photometric conversion type}
      tiffRowStrip : LongInt;            {raster lines per image strip}
      tiffStripOfs : LongInt;            {offset in file to first strip}
      tiffStripCnt : LongInt;            {number of strips}
      tiffStripInfo: PStripInfo;         {strip offsets/lengths}       {!!.01}
      tiffByteCntOfs : LongInt;          {offset to byte count list}   {!!.01}
      tiffImgStart : LongInt;            {start of image data in file}
      ReadBuffer   : PByteBuffer;        {buffer for reads}
      InFile       : File;               {input file}

      {Constructor/destructors}
      constructor Init;
      destructor Done; virtual;

      {User control}
      procedure ConvertFax(FName : PathStr); virtual;
        {-Convert a TIFF file to FAX format.  This version only looks
          for the first image in a multi-image file.}

      {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Abstract Load}
      {$ENDIF}

      {.Z+} {Private methods}
      procedure tcReadRasterLine(var Buffer);
      procedure tcLoadStripInfo;                                       {!!.01}
      function acOpenInputFile : Integer; virtual;
      function tcValidTIFF : Boolean;
      function tcDecodeTag : LongInt;
      function tcGetByte : Byte;
      function tcGetWord : Word;
      function tcGetLong : LongInt;
      function tcSeek(NewOfs : LongInt) : Boolean;
      function tcReadTagDir : Boolean;
      {.Z-}
    end;

  {!!.03 - New}
  {Bmp converter}
  BmpFaxConverterPtr = ^BmpFaxConverter;
  BmpFaxConverter =
    object(AbstractFaxConverter)
      BmpByteWidth : Word;
      BytesPerLine : Word;
      Offset       : LongInt;
      InFile       : File;
      FileHeader   : BmpHeaderRec;
      InfoHeader   : BmpInfoHeaderRec;

      {constructors/destructors}
      constructor Init;

      {conversion}
      procedure ConvertFax(FName : PathStr); virtual;

      {+++internal+++}
      function acOpenInputFile : Integer; virtual;
      procedure bcReadRasterLine(var Buffer); virtual;
        {-read next BMP raster line into Buffer}
    end;

type
  UnpackFaxPtr = ^UnpackFax;
  OutputLineFunc = function (UFP : UnpackFaxPtr;
                             Buffer : PByteBuffer;
                             Len : Word;
                             PH : PageHeaderRec) : Boolean;
  UnpackFax =
    object(Root)
      CurCode         : Word;
      CurSig          : Word;
      LineOfs         : Word;            {current offset in line}
      LineBit         : Word;            {current offset in byte}
      CurrPage        : Word;            {current page}
      ufFlags         : Word;            {option flags}
      BadCodes        : Word;
      TreeLast        : Integer;
      TreeNext        : Integer;
      Match           : Integer;
      WhiteTree       : TreePtr;         {tree of White runlength codes}
      BlackTree       : TreePtr;         {tree of black runlength codes}
      LineBuffer      : PByteBuffer;     {buffer for decompression}
      FileBuffer      : PByteBuffer;     {file I/O buffer}
      FaxHeader       : FaxHeaderRec;
      PageHeader      : PageHeaderRec;
      HalfWidth       : Boolean;         {True if halfwidth option set}
      LastOdd         : Boolean;         {Toggle for HalfWidth adjust} {!!.01}
      ufOutputLine    : OutputLineFunc;  {output a decompressed raster line}
      SaveMode        : Byte;            {Save FileMode }               {!!.02}

      {Constructor/destructor}
      constructor Init;
      destructor Done; virtual;

      {Option management}
      procedure ufOptionsOn(OptionFlags : Word);
        {-Activate multiple options}
      procedure ufOptionsOff(OptionFlags : Word);
        {-Deactivate multiple options}
      function ufOptionsAreOn(OptionFlags : Word) : Boolean;
        {-Return True if all specified options are on}

      {User control}
      procedure GetFaxHeader(FName : PathStr; var FH : FaxHeaderRec);
        {-Return header for fax FName}
      procedure GetPageHeader(FName : PathStr; Page : Word;
                              var PH : PageHeaderRec);
        {-Return header for Page in fax FName}
      procedure SetOutputLineFunc(OLF : OutputLineFunc);
        {-Set output-line procedure}
      procedure UnpackPage(FName : PathStr; Page : Word);
        {-Unpack page PgNo of FName, calling OutputLine for each raster line}
      procedure UnpackFile(FName : PathStr);
        {-Unpack all pages in a fax file}

      {Virtual method hooks}
      function OutputLine : Boolean; virtual;
        {-Method to output a line; must either be overridden or a proc assigned
          with SetOutputLineFunc.}

      {$IFDEF UseStreams}
      constructor Load(var S : IdStream);
        {-Abstract Load}
      procedure Store(var S : IdStream);
        {-Abstract Store}
      {$ENDIF}

      {.Z+}{Private methods}
      procedure ufAddCode(T : TreePtr; Code : Word);
      procedure ufSetCode(T : TreePtr; N0, N1 : Integer);
      function ufCodeMatch(var TC : TermCodeArray; var MUC : MakeUpCodeArray) : Integer;
      procedure ufBuildTree(T : TreePtr;
                          var TC : TermCodeArray;
                          var MUC : MakeUpCodeArray);
      procedure ufInitTree(var T : TreePtr);
      procedure ufBuildTrees;
      procedure ufDisposeTrees;
      procedure ufOutputRun(IsWhite : Boolean; Len : Integer);
      function ufValidLineLength(Len : Word) : Boolean;                {!!.01}
      {.Z-}
    end;

type

  AbstractPcxUnpackPtr = ^AbstractPcxUnpack;
  AbstractPcxUnpack =
    object(UnpackFax)
      Lines      : Word;
      PBOfs      : Word;
      PcxF       : File;
      PcxOfs     : LongInt;
      PcxName    : PathStr;
      PackBuffer : Array[0..511] of Byte;

      {.Z+}{Private methods}
      procedure upWriteHeader;
        {-Write the PCX header}
      procedure upFixLineCount;
        {-Fix header YMax value}
      procedure upFlushBuff(Force : Boolean);
        {-See if output buffer needs to be flushed}
      function OutputLine : Boolean; virtual;
        {-Convert one raster line from fax to pcx}
      {.Z-}
    end;

  UnpackToPcxPtr = ^UnpackToPcx;
  UnpackToPcx =
    object(AbstractPcxUnpack)
      procedure UnpackFileToPcx(Fax, Pcx: PathStr);
        {-Unpack fax file FaxName to pcx file PcxName}
    end;

  UnpackToDcxPtr = ^UnpackToDcx;
  UnpackToDcx =
    object(AbstractPcxUnpack)
      procedure UnpackFileToDcx(Fax, Dcx : PathStr);
    end;

  {.Z+} {Private routines}
  {!!.03 Ordering and ifdef changes}
  {Functions/Procedures}
  function TodayString : String;
    {-Return today's date in a "MM/DD/YY" format string}
  function NowString : String;
    {-Return the current time as a "HH:MMpm" string}
  {$IFDEF Standalone}
  function Trim(S : string) : string;
    {-Return a string with leading and trailing white space removed}
  function ForceExtension(Name : string; Ext : ExtStr) : string;
    {-Return a pathname with the specified extension attached}
  {$ENDIF}
  procedure Merge(var S : String; C : Char);
    {-Appends C to S, shifting S if it gets too long}
  function TrimStationID(S : String) : String;
    {-Trim a station ID string}
  procedure StripPrefix(var S : String);
    {-Removes prefix from faxmodem response string}
  function GetPackedDateTime : LongInt;
    {-Return current date/time in packed format}
  {.Z-}

  {$IFDEF UseStreams}
  procedure AbstractFaxConverterStream(SPtr : IdStreamPtr);
    {-Register all types needed for fax converter streams}
  procedure TextFaxConverterStream(SPtr : IdStreamPtr);
    {-Register all types needed for text converter streams}
  procedure PcxFaxConverterStream(SPtr : IdStreamPtr);
    {-Register all types needed for pcx fax converter}
  procedure DcxFaxConverterStream(SPtr : IdStreamPtr);
    {-Register all types needed for dcx fax converter}
  procedure TiffFaxConverterStream(SPtr : IdStreamPtr);
    {-Register all types needed for tiff fax converter}

  procedure UnpackFaxStream(SPtr : IdStreamPtr);
    {-Register all types needed for unpacker}
  procedure UnpackToPcxStream(SPtr : IdStreamPtr);
    {-Register all types needed for unpacker to pcx}
  procedure UnpackToDcxStream(SPtr : IdStreamPtr);
    {-Register all types needed for unpacker to dcx}
  {$ENDIF}

implementation

const
  {General}
  MaxFontBytes : Word = 24576;
  LineBufSize = 512;

  {Sizes for small font used for header line}
  SmallFontRec : FontRecord = (
    Bytes  : 16;
    PWidth : 12;
    Width  : 2;
    Height : 8);

  {Sizes for standard font}
  StandardFontRec : FontRecord = (
    Bytes  : 48;
    PWidth : 20;
    Width  : 3;
    Height : 16);

  {TIFF tag values}
  SubfileType         = 255;
  ImageWidth          = 256;
  ImageLength         = 257;
  BitsPerSample       = 258;
  Compression         = 259;
  PhotometricInterp   = 262;
  StripOffsets        = 273;
  RowsPerStrip        = 278;
  StripByteCounts     = 279;                                           {!!.01}

  {TIFF compression values}
  compNone            = $0001;
  compHuff            = $0002;
  compFAX3            = $0003;
  compFAX4            = $0004;
  compWRDL            = $8003;
  compMPNT            = $8005;

  {TIFF tag integer types}
  tiffByte            = 1;
  tiffASCII           = 2;
  tiffShort           = 3;
  tiffLong            = 4;
  tiffRational        = 5;

  ReadBufferSize   = 8192;        {Max size of file read buffer}
  LinePadSize      = 2;           {Assure this many nulls per line}

  {End of line bit codes for fax images}
  EOLRec : CodeRec = (Code : $0001; Sig : 12);
  LongEOLRec : CodeRec = (Code : $0001; Sig : 16);

  {Raster lines in standard/high resolution}
  RasterLines : array[Boolean] of Byte = (16, 32);

  {Ofset in PCX file of YMax field}
  YMaxOfs = 10;

  {For decoding white runs}
  WhiteTable : TermCodeArray = (
    (Code : $0035; Sig : 8),
    (Code : $0007; Sig : 6),
    (Code : $0007; Sig : 4),
    (Code : $0008; Sig : 4),
    (Code : $000B; Sig : 4),
    (Code : $000C; Sig : 4),
    (Code : $000E; Sig : 4),
    (Code : $000F; Sig : 4),
    (Code : $0013; Sig : 5),
    (Code : $0014; Sig : 5),
    (Code : $0007; Sig : 5),
    (Code : $0008; Sig : 5),
    (Code : $0008; Sig : 6),
    (Code : $0003; Sig : 6),
    (Code : $0034; Sig : 6),
    (Code : $0035; Sig : 6),
    (Code : $002A; Sig : 6),
    (Code : $002B; Sig : 6),
    (Code : $0027; Sig : 7),
    (Code : $000C; Sig : 7),
    (Code : $0008; Sig : 7),
    (Code : $0017; Sig : 7),
    (Code : $0003; Sig : 7),
    (Code : $0004; Sig : 7),
    (Code : $0028; Sig : 7),
    (Code : $002B; Sig : 7),
    (Code : $0013; Sig : 7),
    (Code : $0024; Sig : 7),
    (Code : $0018; Sig : 7),
    (Code : $0002; Sig : 8),
    (Code : $0003; Sig : 8),
    (Code : $001A; Sig : 8),
    (Code : $001B; Sig : 8),
    (Code : $0012; Sig : 8),
    (Code : $0013; Sig : 8),
    (Code : $0014; Sig : 8),
    (Code : $0015; Sig : 8),
    (Code : $0016; Sig : 8),
    (Code : $0017; Sig : 8),
    (Code : $0028; Sig : 8),
    (Code : $0029; Sig : 8),
    (Code : $002A; Sig : 8),
    (Code : $002B; Sig : 8),
    (Code : $002C; Sig : 8),
    (Code : $002D; Sig : 8),
    (Code : $0004; Sig : 8),
    (Code : $0005; Sig : 8),
    (Code : $000A; Sig : 8),
    (Code : $000B; Sig : 8),
    (Code : $0052; Sig : 8),
    (Code : $0053; Sig : 8),
    (Code : $0054; Sig : 8),
    (Code : $0055; Sig : 8),
    (Code : $0024; Sig : 8),
    (Code : $0025; Sig : 8),
    (Code : $0058; Sig : 8),
    (Code : $0059; Sig : 8),
    (Code : $005A; Sig : 8),
    (Code : $005B; Sig : 8),
    (Code : $004A; Sig : 8),
    (Code : $004B; Sig : 8),
    (Code : $0032; Sig : 8),
    (Code : $0033; Sig : 8),
    (Code : $0034; Sig : 8));

  {!!.03 - Updated for wide width faxes}
  WhiteMUTable : MakeUpCodeArray = (
    (Code : $001B; Sig :  5),
    (Code : $0012; Sig :  5),
    (Code : $0017; Sig :  6),
    (Code : $0037; Sig :  7),
    (Code : $0036; Sig :  8),
    (Code : $0037; Sig :  8),
    (Code : $0064; Sig :  8),
    (Code : $0065; Sig :  8),
    (Code : $0068; Sig :  8),
    (Code : $0067; Sig :  8),
    (Code : $00CC; Sig :  9),
    (Code : $00CD; Sig :  9),
    (Code : $00D2; Sig :  9),
    (Code : $00D3; Sig :  9),
    (Code : $00D4; Sig :  9),
    (Code : $00D5; Sig :  9),
    (Code : $00D6; Sig :  9),
    (Code : $00D7; Sig :  9),
    (Code : $00D8; Sig :  9),
    (Code : $00D9; Sig :  9),
    (Code : $00DA; Sig :  9),
    (Code : $00DB; Sig :  9),
    (Code : $0098; Sig :  9),
    (Code : $0099; Sig :  9),
    (Code : $009A; Sig :  9),
    (Code : $0018; Sig :  6),
    (Code : $009B; Sig :  9),
    (Code : $0008; Sig : 11),
    (Code : $000C; Sig : 11),
    (Code : $000D; Sig : 11),
    (Code : $0012; Sig : 12),
    (Code : $0013; Sig : 12),
    (Code : $0014; Sig : 12),
    (Code : $0015; Sig : 12),
    (Code : $0016; Sig : 12),
    (Code : $0017; Sig : 12),
    (Code : $001C; Sig : 12),
    (Code : $001D; Sig : 12),
    (Code : $001E; Sig : 12),
    (Code : $001F; Sig : 12));

  BlackTable : TermCodeArray = (
    (Code : $0037; Sig : 10),
    (Code : $0002; Sig : 3),
    (Code : $0003; Sig : 2),
    (Code : $0002; Sig : 2),
    (Code : $0003; Sig : 3),
    (Code : $0003; Sig : 4),
    (Code : $0002; Sig : 4),
    (Code : $0003; Sig : 5),
    (Code : $0005; Sig : 6),
    (Code : $0004; Sig : 6),
    (Code : $0004; Sig : 7),
    (Code : $0005; Sig : 7),
    (Code : $0007; Sig : 7),
    (Code : $0004; Sig : 8),
    (Code : $0007; Sig : 8),
    (Code : $0018; Sig : 9),
    (Code : $0017; Sig : 10),
    (Code : $0018; Sig : 10),
    (Code : $0008; Sig : 10),
    (Code : $0067; Sig : 11),
    (Code : $0068; Sig : 11),
    (Code : $006C; Sig : 11),
    (Code : $0037; Sig : 11),
    (Code : $0028; Sig : 11),
    (Code : $0017; Sig : 11),
    (Code : $0018; Sig : 11),
    (Code : $00CA; Sig : 12),
    (Code : $00CB; Sig : 12),
    (Code : $00CC; Sig : 12),
    (Code : $00CD; Sig : 12),
    (Code : $0068; Sig : 12),
    (Code : $0069; Sig : 12),
    (Code : $006A; Sig : 12),
    (Code : $006B; Sig : 12),
    (Code : $00D2; Sig : 12),
    (Code : $00D3; Sig : 12),
    (Code : $00D4; Sig : 12),
    (Code : $00D5; Sig : 12),
    (Code : $00D6; Sig : 12),
    (Code : $00D7; Sig : 12),
    (Code : $006C; Sig : 12),
    (Code : $006D; Sig : 12),
    (Code : $00DA; Sig : 12),
    (Code : $00DB; Sig : 12),
    (Code : $0054; Sig : 12),
    (Code : $0055; Sig : 12),
    (Code : $0056; Sig : 12),
    (Code : $0057; Sig : 12),
    (Code : $0064; Sig : 12),
    (Code : $0065; Sig : 12),
    (Code : $0052; Sig : 12),
    (Code : $0053; Sig : 12),
    (Code : $0024; Sig : 12),
    (Code : $0037; Sig : 12),
    (Code : $0038; Sig : 12),
    (Code : $0027; Sig : 12),
    (Code : $0028; Sig : 12),
    (Code : $0058; Sig : 12),
    (Code : $0059; Sig : 12),
    (Code : $002B; Sig : 12),
    (Code : $002C; Sig : 12),
    (Code : $005A; Sig : 12),
    (Code : $0066; Sig : 12),
    (Code : $0067; Sig : 12));

  {!!.03 - Updated for wide width faxes}
  BlackMUTable : MakeUpCodeArray = (
    (Code : $000F; Sig : 10),
    (Code : $00C8; Sig : 12),
    (Code : $00C9; Sig : 12),
    (Code : $005B; Sig : 12),
    (Code : $0033; Sig : 12),
    (Code : $0034; Sig : 12),
    (Code : $0035; Sig : 12),
    (Code : $006C; Sig : 13),
    (Code : $006D; Sig : 13),
    (Code : $004A; Sig : 13),
    (Code : $004B; Sig : 13),
    (Code : $004C; Sig : 13),
    (Code : $004D; Sig : 13),
    (Code : $0072; Sig : 13),
    (Code : $0073; Sig : 13),
    (Code : $0074; Sig : 13),
    (Code : $0075; Sig : 13),
    (Code : $0076; Sig : 13),
    (Code : $0077; Sig : 13),
    (Code : $0052; Sig : 13),
    (Code : $0053; Sig : 13),
    (Code : $0054; Sig : 13),
    (Code : $0055; Sig : 13),
    (Code : $005A; Sig : 13),
    (Code : $005B; Sig : 13),
    (Code : $0064; Sig : 13),
    (Code : $0065; Sig : 13),
    (Code : $0008; Sig : 11),
    (Code : $000C; Sig : 11),
    (Code : $000D; Sig : 11),
    (Code : $0012; Sig : 12),
    (Code : $0013; Sig : 12),
    (Code : $0014; Sig : 12),
    (Code : $0015; Sig : 12),
    (Code : $0016; Sig : 12),
    (Code : $0017; Sig : 12),
    (Code : $001C; Sig : 12),
    (Code : $001D; Sig : 12),
    (Code : $001E; Sig : 12),
    (Code : $001F; Sig : 12));

  {$IFDEF BindFaxFont}
  procedure BoundFont; external;
  {$L APFAX.OBJ}
  {$ENDIF}

  procedure RotateCode(var Code : Word; Sig : Word); assembler;
    {-Flip code MSB for LSB}
  asm
    les di,Code
    mov dx,es:[di]
    xor ax,ax
    mov cx,16
@1: shr dx,1
    rcl ax,1
    loop @1
    mov cx,16
    sub cx,Sig
    shr ax,cl
    mov es:[di],ax
  end;

  procedure RotateCodeGroup(var TC : TermCodeArray; var MUC : MakeUpCodeArray);
    {-Flip bits in white or black groups}
  var
    I : Integer;
  begin
    for I := 0 to MaxCodeTable do   {!!.03}
      with TC[I] do
        RotateCode(Code, Sig);
    for I := 0 to MaxMUCodeTable do {!!.03}
      with MUC[I] do
        RotateCode(Code, Sig);
  end;

  procedure RotateCodes;
    {-Flip bits in all codes}
  begin
    RotateCodeGroup(WhiteTable, WhiteMUTable);
    RotateCodeGroup(BlackTable, BlackMUTable);
    RotateCode(EOLRec.Code, EOLRec.Sig);
    RotateCode(LongEOLRec.Code, LongEOLRec.Sig);
  end;

{General purpose routine}

  {!!.03 - Added}
  procedure FastZero(var Dest; Size : Word); assembler;
    {-Zero fill dest}
  asm
    xor   ax,ax     { store zeros from ax   }
    les   di,Dest   { es:di = @Dest         }
    cld             { go forward            }
    mov   cx,Size   { size of data to zero  }
    shr   cx,1      { divide size by two    }
    rep   stosw     { zero words            }
    adc   cx,cx     { get odd byte (cf)     }
    rep   stosb     { store odd byte if any }
  end;

  function ForceExtension(Name : string; Ext : ExtStr) : string;
    {-Return a pathname with the specified extension attached}
  var
    DotPos : Word;

    function HasExtension(Name : string; var DP : Word) : Boolean;
      {-Return whether and position of extension separator dot in a pathname}
    var
      I : Word;
    begin
      DP := 0;
      for I := Length(Name) downto 1 do
        if (Name[I] = '.') and (DP = 0) then
          DP := I;
      HasExtension := (DP > 0) and (Pos('\', Copy(Name, Succ(DP), 64)) = 0);
    end;

  begin
    if Name = '' then
      ForceExtension := ''
    else if HasExtension(Name, DotPos) then
      ForceExtension := Copy(Name, 1, DotPos)+Ext
    else
      ForceExtension := Name+'.'+Ext;
  end;

  function Trim(S : string) : string;
    {-Return a string with leading and trailing white space removed}
  var
    I : Word;
    SLen : Byte absolute S;
  begin
    while (SLen > 0) and (S[SLen] <= ' ') do
      Dec(SLen);

    I := 1;
    while (I <= SLen) and (S[I] <= ' ') do
      Inc(I);
    Dec(I);
    if I > 0 then
      Delete(S, 1, I);

    Trim := S;
  end;

  function TrimStationID(S : String) : String;
  begin
    S := Trim(S);
    if S[1] = '"' then
      S[1] := ' ';
    while (Length(S) > 0) and (not(Upcase(S[Length(S)]) in ['0'..'9','A'..'Z'])) do
      Dec(S[0]);
    TrimStationID := Trim(S);
  end;

  function LeftPadCh(S : string; Ch : Char; Len : Byte) : string;
    {-Return a string left-padded to length len with ch}
  var
    o : string;
    SLen : Byte absolute S;
  begin
    if Length(S) >= Len then
      LeftPadCh := S
    else if SLen < 255 then begin
      o[0] := Chr(Len);
      Move(S[1], o[Succ(Word(Len))-SLen], SLen);
      FillChar(o[1], Len-SLen, Ch);
      LeftPadCh := o;
    end;
  end;

  function GetPackedDateTime : LongInt;
  var
    L : LongInt;
    DT : DateTime;
    W : Word;
  begin
    with DT do begin
      GetDate(Year, Month, Day, W);
      GetTime(Hour, Min, Sec, W);
      PackTime(DT, L);
    end;
    GetPackedDateTime := L;
  end;

  procedure Merge(var S : String; C : Char);
    {-appends C to S, shifting S if it gets too long}
  var
    B : Byte absolute S;
  begin
    if B > 254 then
      Move(S[2], S[1], B-1)
    else
      Inc(B);
    S[b] := C;
  end;

  procedure StripPrefix(var S : String);
    {-removes prefix from faxmodem response string}
  var
    SepPos : Integer;
  begin
    S := Trim(S);
    SepPos := Pos(':', S);
    if SepPos = 0 then
      SepPos := Pos('=', S);
   if SepPos > 0 then
      Delete(S, 1, SepPos);

    S := Trim(S);
  end;

  function ValidSignature(var TestSig) : Boolean;
  var
    T : SigArray absolute TestSig;
  begin
    ValidSignature := (T = DefSig);
  end;

  function TodayString : String;
    {-return today's date in a "MM/DD/YY" format string}
  var
    Year, Month, Day, DOW : Word;
    S, T : String[12];
  begin
    {get the system date}
    GetDate(Year, Month, Day, DOW);

    {convert the month}
    Str(Month, S);
    S := LeftPadCh(S, ' ', 2);
    if S[1] = ' ' then
      S[1] := '0';
    T := S + '/';

    {convert the day}
    Str(Day, S);
    S := LeftPadCh(S, ' ', 2);
    if S[1] = ' ' then
      S[1] := '0';
    T := T + S;
    T := T + '/';

    {convert the year}
    Year := Year mod 100;  {strip the "19"}
    Str(Year, S);
    S := LeftPadCh(S, ' ', 2);
    if S[1] = ' ' then
      S[1] := '0';
    T := T + S;

    TodayString := T;
  end;

  function NowString : String;
    {-return the current time as a "HH:MMpm" string}
  var
    Hour, Min, Sec, Sec100 : Word;
    S, T : String[12];
    PM : Boolean;
  begin
    {get the system time}
    GetTime(Hour, Min, Sec, Sec100);

    {correct for AM/PM}
    PM := ((Hour >= 12) and (Hour < 24));
    if Hour > 12 then
      Dec(Hour, 12);

    {convert the hours}
    Str(Hour, S);
    if Length(S) = 1 then
      S := '0' + S;
    T := S + ':';

    {convert the minutes}
    Str(Min, S);
    S := LeftPadCh(S, ' ', 2);
    if S[1] = ' ' then
      S[1] := '0';
    T := T + S;

    {add the AM/PM indicator}
    if PM then
      T := T + 'pm'
    else
      T := T + 'am';

    NowString := T;
  end;

{AbstractFaxConverter object}

  procedure AbstractFaxConverter.acRawInit;
  begin
    DataLine := nil;
    TmpBuffer := nil;
    FontPtr := nil;
    FaxPath := '';
    OutFileName := '';
    InFileName  := '';
    StationID := PadCh(' ', ' ', 20);
    @StatusP := nil;
    FastZero(MainHeader, SizeOf(MainHeader)); {!!.03}
    Move(DefSig, MainHeader.Signature, SizeOf(MainHeader.Signature));
    MainHeader.PageOfs := SizeOf(MainHeader);
    FastZero(PageHeader, SizeOf(PageHeader)); {!!.03}
    CurrLine := 0;
    CurrPage := 0;
    LineCount := 0;
    FontLoaded := False;
    DoubleWidth := False;
    fcFlags := DefFaxCvtOptions;
    ResWidth := StandardWidth;                {!!.03}
    UseHighRes := False;
    FaxLeftMargin := DefLeftMargin;
    FaxTopMargin := DefTopMargin;
    AsyncStatus := ecOK;
  end;

  constructor AbstractFaxConverter.Init;
    {-init our object}
  begin
    acRawInit;

    if not Root.Init then
      Fail;

    if not GetMemCheck(DataLine, MaxData) then begin
      Done;
      Fail;
    end;
    acInitDataLine;

    if not GetMemCheck(TmpBuffer, MaxData) then begin
      Done;
      Fail;
    end;

    if not GetMemCheck(FontPtr, MaxFontBytes) then begin
      Done;
      Fail;
    end;
  end;

  destructor AbstractFaxConverter.Done;
    {-destroy object when done}
  begin
    FreeMemCheck(FontPtr, MaxFontBytes);
    FreeMemCheck(TmpBuffer, MaxData);
    FreeMemCheck(DataLine, MaxData);
    Root.Done;
  end;

  procedure AbstractFaxConverter.fcOptionsOn(OptionFlags : Word);
    {-Activate multiple options}
  begin
    fcFlags := fcFlags or (OptionFlags and not BadFaxCvtOptions);
  end;

  procedure AbstractFaxConverter.fcOptionsOff(OptionFlags : Word);
    {-Deactivate multiple options}
  begin
    fcFlags := fcFlags and not (OptionFlags and not BadFaxCvtOptions);
  end;

  function AbstractFaxConverter.fcOptionsAreOn(OptionFlags : Word) : Boolean;
    {-Return True if all specified options are on}
  begin
    fcOptionsAreOn := (fcFlags and OptionFlags = OptionFlags);
  end;

  function AbstractFaxConverter.FaxConvertStatus(Starting, Ending : Boolean) : Boolean;
    {-overridable method to display status of conversion process}
  begin
    if @StatusP <> nil then
      FaxConvertStatus := StatusP(@Self, Starting, Ending)
    else
      FaxConvertStatus := False;
  end;

  procedure AbstractFaxConverter.SetStatusFunc(FCF : FaxCvtStatusFunc);
    {-set our status proc pointer}
  begin
    StatusP := FCF;
  end;

  procedure AbstractFaxConverter.GetStatusInfo(var Line : LongInt;
                                               var Page : Integer);
    {-Return current line/page}
  begin
    Line := CurrLine;
    Page := CurrPage;
  end;

  function AbstractFaxConverter.GetFileName : PathStr;
    {-Return the name of the input file}
  begin
    GetFileName := InFileName;
  end;

  procedure AbstractFaxConverter.SetFaxPath(PS : PathStr);
    {-set path for storage of fax files}
  begin
    FaxPath := PS;
  end;

  procedure AbstractFaxConverter.SetStationID(SID : Str20);
    {-set the sending Station ID for this fax}
  begin
    StationID := PadCh(SID, ' ', 20);
  end;

  procedure AbstractFaxConverter.SetMargins(Left, Top : Word);
    {-Set top and left margins for converted documents}
  begin
    FaxLeftMargin := Left;
    FaxTopMargin := Top;
  end;

  function AbstractFaxConverter.LoadFont(FontHandle : Byte;
                                         HiRes : Boolean) : Boolean;
    {-Load selected font from APFAX.FNT font file or memory}
  {$IFNDEF BindFaxFont}
  label
    Error;
  var
    F : File;
    ToRead  : Word;
    ActRead : Word;
  {$ELSE}
  type
    OS = record
      O : Word;
      S : Word;
    end;
  var
    P : Pointer;
    Len : Word;
  {$ENDIF}
    I, J : Integer;
    Row, NewRow : Word;
    NewBytes : Word;
  begin
    {$IFDEF BindFaxFont}
    P := @BoundFont;
    if FontHandle = StandardFont then begin
      Inc(OS(P).O, 4096);
      Len := 12288;
      FontRec := StandardFontRec;
    end else begin
      Len := 4096;
      FontRec := SmallFontRec;
    end;
    Move(P^, FontPtr^, Len);
    {Scale font up if HiRes requested}
    if HiRes then
      with FontRec do begin
        {Allocate temporary buffer for scaled up font}
        NewBytes := Bytes*2;

        {Double raster lines of font}
        for J := 255 downto 0 do begin
          NewRow := 0;
          Row := 0;
          for I := 1 to Height do begin
            Move(FontPtr^[(J*Bytes)+Row], FontPtr^[(J*NewBytes)+NewRow], Width);
            Move(FontPtr^[(J*Bytes)+Row], FontPtr^[(J*NewBytes)+NewRow+Width], Width);
            Inc(Row, Width);
            Inc(NewRow, Width*2);
          end;
        end;

        {Adjust FontRec}
        Bytes := NewBytes;
        Height := Height*2;
      end;

    LoadFont := True;
    FontLoaded := True;

    {$ELSE}
    LoadFont := False;
    SaveMode := FileMode;                                              {!!.02}
    FileMode := $40;                                                   {!!.02}
    Assign(F, AproFontName);
    Reset(F, 1);
    FileMode := SaveMode;                                              {!!.02}
    AsyncStatus := IoResult;
    if AsyncStatus <> ecOk then
      Exit;

    FastZero(FontPtr^, MaxFontBytes);                                  {!!.03}
    case FontHandle of
      SmallFont:
        FontRec := SmallFontRec;
      StandardFont :
        begin
          FontRec := StandardFontRec;
          {Seek past small font in file}
          Seek(F, (SmallFont*256));
        end;
    end;
    AsyncStatus := IoResult;
    if AsyncStatus <> 0 then
      goto Error;
    ToRead := FontRec.Bytes*256;

    BlockRead(F, FontPtr^, ToRead, ActRead);
    AsyncStatus := IOResult;
    if ActRead <> ToRead then
      AsyncStatus := ecDeviceRead;
    if AsyncStatus <> ecOk then
      goto Error;

    {Scale font up if HiRes requested}
    if HiRes and (AsyncStatus = 0) then
      with FontRec do begin
        {Allocate temporary buffer for scaled up font}
        NewBytes := Bytes*2;

        {Double raster lines of font}
        for J := 255 downto 0 do begin
          NewRow := 0;
          Row := 0;
          for I := 1 to Height do begin
            Move(FontPtr^[(J*Bytes)+Row], FontPtr^[(J*NewBytes)+NewRow], Width);
            Move(FontPtr^[(J*Bytes)+Row], FontPtr^[(J*NewBytes)+NewRow+Width], Width);
            Inc(Row, Width);
            Inc(NewRow, Width*2);
          end;
        end;

        {Adjust FontRec}
        Bytes := NewBytes;
        Height := Height*2;
      end;

    FontLoaded := True;
    LoadFont := True;

Error:
    Close(F);
    if IOResult = 0 then ;
    {$ENDIF}
  end;

  procedure AbstractFaxConverter.acScaleChar(C : Byte);
    {-Scale character from FullFont to ScaleFont}
  var
    OldRow, OldCol : Word;
    Row, Col : Word;
    TempFont : array[1..MaxTempFont] of Byte;
    I : Word;
    Map : Byte;
    FontSize : Word;

    function OldBitSet(Row, Col : Word) : Boolean;
      {-Return True if bit at R,C set in FullFont}
    var
      Index : Word;
      BitIndex : Word;
    begin
      Index := ((Row-1)*BytesPerRow) + ((Col-1) div 8) + 1;
      BitIndex := Col mod 8;
      if BitIndex = 0 then
        BitIndex := 8;
      OldBitSet := (FullFont^[Index] and Mask[BitIndex]) = Mask[BitIndex];
    end;

    procedure NewSetBit(Row, Col : Word);
      {-Set bit at R,C in ScaleFont}
    var
      Index : Word;
      BitIndex : Word;
    begin
      Index := ((Row-1)*3) + ((Col-1) div 8) + 1;
      BitIndex := Col mod 8;
      if BitIndex = 0 then
        BitIndex := 8;
      TempFont[Index] := TempFont[Index] or Mask[BitIndex];
    end;

  begin
    FastZero(TempFont, SizeOf(TempFont)); {!!.03}

    {Scale X}
    with FontHdr do begin
      for Row := 1 to FontHdr.CellHeight do begin
        for Col := 1 to 20 do begin
          if OldBitSet(Row, ScaleCol[Col]) then
            NewSetBit(Row, Col);
        end;
      end;

      {Scale Y}
      if HighResFont then
        FontSize := 96
      else
        FontSize := 48;
      Row := 0;
      for I := 1 to RasterLines[HighResFont] do begin
        OldRow := ScaleRow[I];
        FontPtr^[(C*FontSize)+Row]   := TempFont[OldRow+1];
        FontPtr^[(C*FontSize)+Row+1] := TempFont[OldRow+2];
        FontPtr^[(C*FontSize)+Row+2] := TempFont[OldRow+3];
        Inc(Row, 3);
      end;
    end;
  end;

  procedure AbstractFaxConverter.acConvertChar(C : Byte);
    {-Convert HP character data to scaled bitmap}
  var
    J, Index : Word;
    BitIndex : Word;
    ByteWidth : Word;
    BitWidth : Word;
    Bit : Word;
    Row, Col : Word;
    F1, F2 : Text;

    procedure SetBit(Bit : Word);
      {-Turn on Bit in a FullFont array}
    begin
      Index := (Bit div 8) + 1;
      BitIndex := (Bit mod 8) + 1;
      if Index <= MaxCharData then
        FullFont^[Index] := FullFont^[Index] or Mask[BitIndex];
    end;

    function HPBitSet(Row, Col : Word) : Boolean;
      {-Return True if bit at row/col in HP matrix is set}
    begin
      with CharHdr do begin
        Row := Row - TopOffset;
        Col := Col - LeftOffset - 1;
        Index := ((Row-1)*ByteWidth) + (Col div 8) + 1;
        BitIndex := (Col mod 8) + 1;
        if Index <= MaxCharData then
          HPBitSet := CharData^[Index] and Mask[BitIndex] = Mask[BitIndex]
        else                                                           {!!.01}
          HPBitSet := False;                                           {!!.01}
      end;
    end;

  begin
    with CharHdr, FontHdr do begin
      {Translate TopOffset to mean "raster lines from top"}
      TopOffset := BaseLine-TopOffset;

      {Get byte width of HP character map}
      ByteWidth := (CharWidth+7) shr 3;

      {Get bit width of fullfont}
      BitWidth := (CellWidth + 7) and $F8;

      {Convert from HP format to full bitmap format}
      FastZero(FullFont^, MaxFullFont); {!!.03}
      for Bit := 1 to (BitWidth*CellHeight) do begin
        Row := (Bit div BitWidth) + 1;
        Col := (Bit mod BitWidth) + 1;

        if ((Row > TopOffset) and (Row <= (CharHeight+TopOffset))) and
           ((Col > LeftOffset) and (Col <= (CharWidth+LeftOffset))) then
          if HPBitSet(Row, Col) then
            SetBit(Bit);
      end;
    end;
  end;

  function AbstractFaxConverter.acReadCommandNumber : LongInt;
    {-Read a number from a parameterized escape sequence}
  var
    L : LongInt;
    I : Integer;
    S : String[7];
    C : Char;
  begin
    I := 1;
    repeat
      BlockRead(Font, C, 1);
      S[I] := C;
      Inc(I);
    until (C < '0') or (C > '9');
    I := I-2;
    S[0] := Char(I);
    Val(S, L, I);
    acReadCommandNumber := L;
  end;

  function AbstractFaxConverter.acReadHPFontHeader : Boolean;
    {-Read header and return True if successful}
  var
    W : Word;
    BytesRead : Word;
    S : String[3];
  begin
    acReadHPFontHeader := False;

    BlockRead(Font, S[1], 3, BytesRead);
    S[0] := #3;
    if S <>  ^[')s' then
      Exit;

    {Get size of font descriptor header}
    W := acReadCommandNumber;

    {Read font header}
    BlockRead(Font, FontHdr, SizeOf(FontHdr), BytesRead);
    if (BytesRead <> SizeOf(FontHdr)) or (IoResult <> 0) then
      Exit;

    {Skip past part of header we didn't read}
    Seek(Font, FilePos(Font) + (W-SizeOf(FontHdr)));
    if IoResult <> 0 then
      Exit;

    acReadHPFontHeader := True;
  end;

  function AbstractFaxConverter.acReadCharID : Byte;
    {-Read the charcter ID command}
  var
    S : String[3];
    BytesRead : Word;
  begin
    BlockRead(Font, S[1], 3, BytesRead);
    S[0] := #3;
    if S = ^['*c' then
      acReadCharID := acReadCommandNumber
    else
      acReadCharID := 0;
  end;

  function AbstractFaxConverter.acReadCharDef : Boolean;
    {-Read the character header}
  var
    W : Word;
    BytesRead : Word;
    S : String[3];
  begin
    BlockRead(Font, S[1], 3, BytesRead);
    S[0] := #3;
    if S = ^['(s' then begin
      W := acReadCommandNumber;
      BlockRead(Font, CharHdr, SizeOf(CharHdr), BytesRead);
      CharDataLen := W-16;
      acReadCharDef := (BytesRead = SizeOf(CharHdr)) and (IoResult = 0);
    end else
      acReadCharDef := False;
  end;

  function  AbstractFaxConverter.LoadHPFont(FontName : PathStr; HiRes : Boolean) : Boolean;
    {-Load font from HP soft font file FontName}
  var
    Result : Word;
    BytesRead : Word;
    I : Word;
    R : Real;

    procedure Cleanup;
    begin
      Close(Font);
      if IoResult <> 0 then ;
      FreeMemCheck(CharData, MaxCharData);
      FreeMemCheck(FullFont, MaxFullFont);
    end;

  begin
    LoadHPFont := False;
    HighResFont := HiRes;
    SaveMode := FileMode;                                              {!!.02}
    FileMode := $40;                                                   {!!.02}
    Assign(Font, FontName);
    Reset(Font, 1);
    FileMode := SaveMode;                                              {!!.02}
    Result := IoResult;
    if Result <> 0 then begin
      AsyncStatus := Result;
      Exit;
    end;

    {Allocate bitmap buffers}
    CharData := nil;
    FullFont := nil;
    if not GetMemCheck(CharData, MaxCharData) or
       not GetMemCheck(FullFont, MaxFullFont) then begin
      Cleanup;
      AsyncStatus := ecOutOfMemory;
      Exit;
    end;

    {Assume error}
    AsyncStatus := ecFontNotSupported;

    {Get the font header}
    if not acReadHPFontHeader then begin
      Cleanup;
      Exit;
    end;

    with FontHdr do begin

      {Swap fields we care about}
      BaseLine := Swap(BaseLine);
      CellWidth := Swap(CellWidth);
      CellHeight := Swap(CellHeight);

      {Validate font}
      if (DescFormat <> 0) or
         (Orientation <> 0) or
         ((((CellWidth+7) shr 3) * CellHeight) > MaxFullFont) then begin
        Cleanup;
        Exit;
      end;

      {Clear FontPtr}
      FastZero(FontPtr^, MaxFontBytes); {!!.03}

      {Set scaling}
      with FontHdr do begin
        BytesPerRow := (CellWidth + 7) shr 3;
        R := CellWidth / 20.0;
        for I := 1 to 20 do
          ScaleCol[I] := Round(R * I);

        {Scale Y}
        if HiRes then
          R := CellHeight / 32.0
        else
          R := CellHeight / 16.0;
        for I := 1 to RasterLines[HiRes] do
          ScaleRow[I] := 3 * Round(R * (I-1));
      end;

      {Loop through all characters}
      repeat
        {Read character header}
        CID := acReadCharID;
        if CID <> 0 then begin
          if not acReadCharDef then begin
            Cleanup;
            Exit;
          end;

          with CharHdr do begin
            if FontHdr.DescFormat = 0 then begin
              {Swap fields we care about}
              CharWidth := Swap(CharWidth);
              CharHeight := Swap(CharHeight);
              LeftOffset := Swap(LeftOffset);
              TopOffset := Swap(TopOffset);
            end;

            {Validate character}
            if (CClass > 1) then begin
              Cleanup;
              Exit;
            end;
          end;

          {Read character data}
          if CharDataLen > MaxCharData then
            CharDataLen := MaxCharData;
          BlockRead(Font, CharData^, CharDataLen, BytesRead);
          Result := IoResult;
          if BytesRead <> CharDataLen then
            Result := ecDiskRead;
          if Result <> 0 then begin
            Cleanup;
            AsyncStatus := Result;
            Exit;
          end;

          {Show status}
          AsyncStatus := ecHPFontCvt;
          if FaxConvertStatus(False, False) then begin
            Cleanup;
            AsyncStatus := ecUserAbort;
            Exit;
          end;

          {Convert HP bitmap to full-cell bitmap}
          acConvertChar(CID);

          {Convert full-cell bitmap to scaled bitmap}
          acScaleChar(CID);
        end;
      until Eof(Font);

      Cleanup;

      AsyncStatus := ecOk;
      LoadHPFont := True;
      FontLoaded := True;
      FontRec := StandardFontRec;
      if HiRes then begin
        FontRec.Height := 32;
        FontRec.Bytes := 96
      end;
    end;
  end;

  procedure AbstractFaxConverter.SetPageSize(PS : Integer);
    {-set # of text lines per output page; <= 0 for no pagenation}
  begin
    LineCount := PS;
  end;

  procedure AbstractFaxConverter.SetResolutionMode(HiRes : Boolean);
    {-select std or high resolution mode.  Both modes use the same number of
      pixels per line, but high res doubles the lines-per-inch count.}
  begin
    UseHighRes := HiRes;
  end;

  procedure AbstractFaxConverter.SetResolutionWidth(RW : ResWidths);
    {-select std (1728 pixels) or wide (2048 pixels) width}
  begin
    case RW of
      rw2048 :
        ResWidth := WideWidth;       {!!.03}
      else
        ResWidth := StandardWidth;   {!!.03}
    end;
  end;

  procedure AbstractFaxConverter.acInitHeaders;
    {-Initialize fax and page headers}
  begin
    FastZero(MainHeader, SizeOf(MainHeader));   {!!.03}
    Move(DefSig, MainHeader.Signature, SizeOf(MainHeader.Signature));
    MainHeader.PageOfs := SizeOf(MainHeader);
    FastZero(PageHeader, SizeOf(PageHeader));   {!!.03}
  end;

  function AbstractFaxConverter.acOpenInputFile : Integer;
    {-open the input file, MUST be overridden}
  begin
    RunError(211);
  end;

  function AbstractFaxConverter.acCreateOutputFile(FN : PathStr) : Integer;
    {-create output file FN}
  var
    I : Integer;
    W : Word;
  begin
    if FaxPath = '' then begin                                         {!!.01}
      FN := StUpcase(FN);                                              {!!.01}
      OutFileName := AddBackslash(JustPathName(FN)) +                  {!!.01}
                     ForceExtension(JustFileName(FN), FaxFileExt);     {!!.01}
    end else begin                                                     {!!.01}
      FN := JustFileName(FN);
      OutFileName := AddBackslash(FaxPath)+ForceExtension(StUpcase(FN),
                     FaxFileExt);
    end;                                                               {!!.01}
    Assign(OutFile, OutFileName);
    Rewrite(OutFile, 1);
    I := IOResult;
    if I = 0 then begin
      BlockWrite(OutFile, MainHeader, SizeOf(MainHeader), W);
      I := IOResult;
      if (I = 0) and (W <> SizeOf(MainHeader)) then
        I := ecDeviceWrite;
    end;
    acCreateOutputFile := I;
  end;

  function AbstractFaxConverter.acUpdateMainHeader : Integer;
    {-update the contents of the main header in the file}
  label
    Breakout;
  var
    I : Integer;
    L : LongInt;
    W : Word;
  begin
    {refresh needed fields of MainHeader rec}
    with MainHeader do begin
      SenderID := StationID;
      FDateTime := GetPackedDateTime;
    end;

    {save current file position for later}
    L := FilePos(OutFile);
    I := IOResult;
    if I <> 0 then
      goto Breakout;

    {seek to head of file}
    Seek(OutFile, 0);
    I := IoResult;                                                     {!!.01}
    if I <> 0 then
      goto Breakout;

    {write the header}
    BlockWrite(OutFile, MainHeader, SizeOf(MainHeader), W);
    I := IOResult;
    if (I = 0) and (W <> SizeOf(MainHeader)) then
      I := ecDeviceWrite;
    if I <> 0 then
      goto Breakout;

    {return to original position}
    Seek(OutFile, L);
    I := IOResult;

Breakout:
    acUpdateMainHeader := I;
  end;

  function AbstractFaxConverter.acFindPage(PgNo : Word) : Integer;
    {-find the start of the PgNo-th page in the file}
  label
    Breakout;
  var
    I,N : Integer;
    L,M : LongInt;
    P : PageHeaderRec;
  begin
    {validate passed page no}
    if (PgNo < 1) or (PgNo > MainHeader.PageCount) then begin
      acFindPage := -1;
      Exit;
    end;

    {Find start of file}
    M := MainHeader.PageOfs;
    Seek(OutFile, M);
    I := IOResult;
    if (I <> 0) or (PgNo = 1) then
      goto BreakOut;

    {walk the file finding pages}
    for N := 1 to PgNo-1 do begin
      BlockRead(OutFile, P, SizeOf(P));
      I := IOResult;
      if I <> 0 then
        goto Breakout;
      Inc(M, SizeOf(P)+P.ImgLength);
      Seek(OutFile, M);
      I := IOResult;
      if I <> 0 then
        goto BreakOut;
    end;

Breakout:
    acFindPage := I;
  end;

  function AbstractFaxConverter.acUpdatePageHeader(PgNo : Word;
                                                 var PgInfo : PageHeaderRec) : Integer;
    {-update the contents of the PgNo-th page header in the file}
  label
    Breakout;
  var
    I : Integer;
    W : Word;
    L : LongInt;
  begin
    {save current file position for later}
    L := FilePos(OutFile);
    I := IOResult;
    if I <> 0 then
      goto Breakout;

    {find the page in question}
    I := acFindPage(PgNo);
    if I <> 0 then
      goto Breakout;

    {update the header}
    BlockWrite(Outfile, PgInfo, SizeOf(PageHeaderRec), W);
    I := IOResult;
    if (I = 0) and (W <> SizeOf(PageHeaderRec)) then
      I := ecDeviceWrite;
    if I <> 0 then
      goto Breakout;

    {return to original position}
    Seek(OutFile, L);
    I := IOResult;

Breakout:
    acUpdatePageHeader := I;
  end;

  procedure AbstractFaxConverter.acInitDataLine;
    {-Init the line buffer}
  begin
    FastZero(DataLine^, MaxData); {!!.03}
    ByteOfs := 0;
    BitOfs := 0;
  end;

  procedure AbstractFaxConverter.acAddCodePrim(Code : Word; SignificantBits : Word);
    {-Lowlevel routine to add a runlength code to the line buffer}
  begin
    asm
      les di,Self

      mov ax,Code
      xor dx,dx                {dx:ax = extended code}
      mov cx,es:[di].BitOfs    {cx = bit offset}
      mov si,cx                {save a copy of bit offset}
      jcxz @2
@1:   shl ax,1                 {shift code for bit offset}
      rcl dx,1
      loop @1

@2:   mov bx,es:[di].ByteOfs   {bx = byte offset}
      add si,SignificantBits
      mov cx,si
      shr cx,1
      shr cx,1
      shr cx,1
      add es:[di].ByteOfs,cx   {update ByteOfs}
      and si,7
      mov es:[di].BitOfs,si    {update BitOfs}

      les di,es:[di].DataLine
      add di,bx
      or es:[di],ax            {or new bit pattern in place}
      or es:[di+2],dl
    end;
  end;

  procedure AbstractFaxConverter.acAddCode(RunLen : Word; IsWhite : Boolean);
    {-Adds a code representing RunLen pixels of white (IsWhite=True) or black
      to the current line buffer}
  begin
    asm
      mov al,IsWhite
      mov bx,RunLen

      {Long run?}
      cmp bx,64
      jb  @2

      {Long white run?}
      or al,al
      jz @1

      {Long white run}
      shr bx,1
      shr bx,1
      shr bx,1
      shr bx,1
      shr bx,1
      shr bx,1
      dec bx
      mov si,offset WhiteMUTable
      call @5
      mov bx,RunLen
      and bx,63
      mov si,offset WhiteTable
      jmp @4

      {Long black run}
@1:   shr bx,1
      shr bx,1
      shr bx,1
      shr bx,1
      shr bx,1
      shr bx,1
      dec bx
      mov si,offset BlackMUTable
      call @5
      mov bx,RunLen
      and bx,63
      mov si,offset BlackTable
      jmp @4

      {Short white run?}
@2:   or  al,al
      jz  @3

      {Short white run}
      mov si,offset WhiteTable
      jmp @4

      {Short black run}
@3:   mov si,offset BlackTable

      {Add last code}
@4:   call @5
      jmp @6

      {Routine to add a code}
@5:   shl bx,1
      shl bx,1
      push word ptr [bx+si]
      push word ptr [bx+si+2]
      les di,Self
      push es
      push di
      call acAddCodePrim
      retn
@6:
    end;
  end;

  procedure AbstractFaxConverter.acCompressRasterLine(var Buffer);
    {-compress a raster line of bits into runlength codes}
  var
    RunLen : Word;
    Width : Word;
    P : ^Byte;
    B : Byte;
    IsWhite : Boolean;
    PrevWhite : Boolean;
    DW : Boolean;
    TotalRun : Word;
    TotalRunWidth : Word;
    I : Word;
    Margin : Word;
  begin
    {clear used portion of previous line}
    FastZero(DataLine^, ByteOfs+1);  {!!.03}

    ByteOfs := 0;
    BitOfs := 0;

    {add EOL code}
    acAddCodePrim(LongEOLRec.Code, LongEOLRec.Sig);

    {local references to DoubleWidth}
    DW := DoubleWidth;

    {is the line all white?}
    P := @Buffer;
    Width := ResWidth;
    asm
      les di,P
      xor al,al
      mov cx,Width
      shr cx,1
      shr cx,1
      shr cx,1
      cld
      repe scasb
      mov IsWhite,True
      je  @1
      mov IsWhite,False
@1:
    end;

    if IsWhite then begin
      {yes; add a single code for the all-white line}
      acAddCode(Width, True);
    end

    else begin
      {no; scan and compress:}

      {Add margin}
      TotalRunWidth := ResWidth;                                       {!!.03}
      Margin := FaxLeftMargin;
      if Margin <> 0 then begin
        acAddCode(FaxLeftMargin, True);
        acAddCode(0, False);
        Dec(TotalRunWidth, Margin);                                    {!!.01}
      end;

      TotalRun := 0;
      {TotalRunWidth := 216*8;}                                        {!!.01}
      B := P^;
      PrevWhite := (B and $80 = 0);
      if PrevWhite then
        IsWhite := True
      else begin
        {first pixel is black, add a zero-runlen white code}
        acAddCode(0, True);
        IsWhite := False;
      end;

      {walk the pixel array, counting runlengths and adding codes to match}
      asm
        mov  RunLen,1
        mov  dl,B
        mov  dh,$40
        mov  bh,PrevWhite
        mov  cx,Width
        sub  cx,Margin

        {get NewWhite}
@1:     mov  bl,1
        test dl,dh
        jz   @2
        dec  bl

        {update mask and get new byte if needed}
@2:     mov  al,dh
        shr  al,1
        jnz  @3
        inc  word ptr P
        les  di,P
        mov  dl,es:[di]
        mov  al,$80
@3:     mov  dh,al

        {NewWhite = PrevWhite?}
        cmp  bh,bl
        jne  @4

        {Last pixel?}
        cmp  cx,1
        jne  @5
        test DW,1
        jz   @4
        mov  ax,TotalRunWidth
        sub  ax,TotalRun
        mov  RunLen,ax
        shr  RunLen,1

        {Save registers}
@4:     push bx
        push cx
        push dx

        {Add output code}
        test DW,1
        jz   @41
        shl  RunLen,1
@41:
        {Increment TotalRun}
        mov  ax,TotalRun
        add  ax,RunLen
        mov  TotalRun,ax

        push RunLen
        push word ptr IsWhite
        les di,Self
        push es
        push di
        call acAddCode

        {Restore registers}
        pop dx
        pop cx
        pop bx

        {Update state}
        xor IsWhite,1
        mov RunLen,0
        mov bh,bl

        {Increment RunLen and loop}
@5:     inc RunLen
        loop @1
      end;
    end;

    {Make sure there are at least LinePadSize nulls after the data}
    asm
      les di,Self
      mov ax,es:[di].ByteOfs
      add ax,LinePadSize
      mov es:[di].ByteOfs,ax
    end;
  end;

var
  LByteOfs : Word; {byte offset into LineMatrix}
  LBitOfs : Word;  {bit offset into LineMatrix}
  LPWidth : Word;  {pixel width of current font}

  procedure AddRasterChar(var CharData; var LineMatrix); assembler;
    {-Rasterize one line of one character, adding it to LineMatrix}
  asm
    les  di,LineMatrix
    add  di,LByteOfs
    mov  cx,LBitOfs
    mov  dx,LPWidth
    cld
    push ds
    lds  si,CharData

@1: lodsb                 {get next byte of character data}
    xor  ah,ah
    ror  ax,cl            {rotate by current bit offset}
    or   es:[di],ax       {OR it into position}
    mov  ax,dx            {ax = remaining pixels}
    cmp  ax,8             {at least 8 remaining?}
    jb   @2               {jump if not}
    inc  di               {next output byte}
    sub  dx,8             {update remaining pixels}
    jnz  @1               {loop if more}
    jmp  @3               {get out if not}
@2: sub  dx,ax            {update remaining pixels}
    add  cx,ax            {update bit offset}
    mov  ax,cx
    shr  ax,1
    shr  ax,1
    shr  ax,1
    add  di,ax            {update byte offset}
    and  cx,7             {update bit offset}
    or   dx,dx            {more pixels to merge?}
    jnz  @1               {jump if so}

@3: pop  ds
    mov  si,word ptr LineMatrix
    sub  di,si
    mov  LByteOfs,di      {update global variables}
    mov  LBitOfs,cx
  end;

  {!!.03 - Added}
  procedure AbstractFaxConverter.acCompressStringRowPrim(S : String; Row : Byte; LenWord : Boolean);
    {-Compress one raster row in a string of text, not copying to other buffer}
  var
    SLen   : Byte absolute S;
    MaxLen : Integer;
    I      : Integer;
    Y      : Integer;
    YW     : Integer;
    C      : Integer;
    W      : Integer;
    H      : Integer;
    B      : Integer;

    procedure ExpandTabs;
      {-Expand tabs in S to next tab stop}
    var
      NewS   : String;
      NewLen : Byte absolute NewS;
      I      : Word;

    begin
      FillChar(NewS, SizeOf(News), ' ');
      NewLen := 1;
      for I := 1 to Length(S) do begin
        if S[I] = #09 then begin
          repeat
            Inc(NewLen);
          until ((NewLen mod TabStop) = 1);
        end else begin
          NewS[NewLen] := S[I];
          Inc(NewLen);
        end;
      end;
      Dec(NewLen);
      S := NewS;
    end;

  begin
    {If a font hasn't been loaded the user program has ignored
     a previously reported error; however, we'll exit quitely here
     to avoid catastrophic errors}
    if not FontLoaded then
      Exit;

    with FontRec do begin
      H := Height;
      W := Width;
      B := Bytes;
      LPWidth := PWidth;
    end;

    {Validate Row}
    if Row > H then
      Exit;

    SLen := Length(S);

    {Handle blank lines as special case to improve speed}
    if SLen = 0 then begin
      FastZero(LineMatrix, SizeOf(LineMatrix));
      acCompressRasterLine(LineMatrix);
    end else begin
      ExpandTabs;

      {Truncate line if necessary}
      C := FaxLeftMargin div LPWidth;
      MaxLen := (ResWidth div LPWidth)-C;
      if SLen > MaxLen then
        SLen := MaxLen;

      {Handle non-blank lines}
      YW := (Row-1) * W;
      FastZero(LineMatrix, SizeOf(LineMatrix));

      {Margin will be added later}
      LByteOfs := 0;
      LBitOfs := 0;

      {Rasterize each character}
      for I := 1 to SLen do
        AddRasterChar(FontPtr^[Byte(S[I])*B+YW], LineMatrix);
      acCompressRasterLine(LineMatrix);
    end;
  end;

  {!!.01 - replaces acCompressString, works on one row at a time}
  {!!.03 - reworked to call acCompressStringRowPrim}
  procedure AbstractFaxConverter.acCompressStringRow(S : String;
                                                     Row : Byte;
                                                     var Buffer;
                                                     var Len : Word;
                                                     LenWord : Boolean);
  begin
    acCompressStringRowPrim(S, Row, LenWord);
    Len := 0;
    if LenWord then begin
      TByteBuffer(Buffer)[0] := ByteOfs;
      TByteBuffer(Buffer)[1] := ByteOfs shr 8;
      Inc(Len, 2);
    end;
    Move(DataLine^, TByteBuffer(Buffer)[Len], ByteOfs);
    Inc(Len, ByteOfs);
  end;

  procedure AbstractFaxConverter.acMakeEndOfPage(var Buffer; var Len : Word);
    {-generate an EOP bit run}
  var
    I : Word;
  begin
    acInitDataLine;
    for I := 1 to 6 do
      acAddCodePrim(EOLRec.Code, EOLRec.Sig);
    Move(DataLine^, Buffer, ByteOfs);
    Len := ByteOfs;
  end;

  function AbstractFaxConverter.acAddData(var Buffer; Len : Word;
                                          DoInc : Boolean) : Boolean;
  var
    W : Word;
  begin
    BlockWrite(OutFile, Buffer, Len, W);
    acAddData := (IOResult = 0) and (W = Len);
    if DoInc then
      Inc(PageHeader.ImgLength, Len);
  end;

  function AbstractFaxConverter.acAddLine(var Buffer; Len : Word) : Boolean;
    {-Add a raster with <lengthword><rasterdata> format}
  begin
    acAddLine := acAddData(Len, 2, True) and acAddData(Buffer, Len, True);
  end;

  function AbstractFaxConverter.acAddBlankLines(Count : Word) : Boolean;
    {-Write blank raster lines to disk}
  var
    I : Word;
    Len : Word;
  begin
    acInitDataLine;
    if (ResWidth = StandardWidth) then  {!!.03}
      Len := 4                          {!!.03}
    else                                {!!.03}
      Len := 5;                         {!!.03}

    for I := 1 to Count do begin
      Move(Len, DataLine^[ByteOfs], 2);
      Inc(ByteOfs, 2);
      acAddCodePrim(EOLRec.Code, EOLRec.Sig);
      acAddCode(ResWidth, True);
      Inc(ByteOfs);
      BitOfs := 0;
    end;

    acAddBlankLines := acAddData(DataLine^, Count*(Len+2), True); {!!.03}
  end;

  procedure AbstractFaxConverter.ConvertFax(FName : PathStr);
    {-Convert file to our FAX output format}
  begin
    RunError(211);
  end;

  {$IFDEF UseStreams}
  constructor AbstractFaxConverter.Load(var S : IdStream);
    {-Abstract Load for a converter object}
  begin
    {Init all fields}
    acRawInit;

    {Load the stream data}
    S.Read(UseHighRes, SizeOf(UseHighRes));
    S.Read(fcFlags, SizeOf(fcFlags));
    S.Read(FaxLeftMargin, SizeOf(FaxLeftMargin));
    S.Read(FaxTopMargin, SizeOf(FaxTopMargin));
    S.Read(LineCount, SizeOf(LineCount));
    StationID := S.ReadString;
    FaxPath := S.ReadString;

    {Read the user status pointer}
    @StatusP := S.ReadUserPointer(Nil);

    {Exit if anything failed}
    if S.PeekStatus <> 0 then begin
      Done;
      Fail;
    end;

    {Allocate buffers}
    if not GetMemCheck(DataLine, MaxData) then begin
      Done;
      Fail;
    end;
    acInitDataLine;

    if not GetMemCheck(TmpBuffer, MaxData) then begin
      Done;
      Fail;
    end;

    if not GetMemCheck(FontPtr, MaxFontBytes) then begin
      Done;
      Fail;
    end;
  end;

  procedure AbstractFaxConverter.Store(var S : IdStream);
    {-Abstract Store for a protocol object}
  begin
    {Store various data}
    S.Write(UseHighRes, SizeOf(UseHighRes));
    S.Write(fcFlags, SizeOf(fcFlags));
    S.Write(FaxLeftMargin, SizeOf(FaxLeftMargin));
    S.Write(FaxTopMargin, SizeOf(FaxTopMargin));
    S.Write(LineCount, SizeOf(LineCount));
    S.WriteString(StationID);
    S.WriteString(FaxPath);

    {Store the user status pointer}
    S.WriteUserPointer(@StatusP, ptNil);
  end;
  {$ENDIF}

{TextFaxConverter object}

  constructor TextFaxConverter.Init;
    {-initialize our text-to-fax converter}
  begin
    ReadBuffer := nil;

    if not AbstractFaxConverter.Init then
      Fail;

    {allocate file read buffer}
    if not GetMemCheck(ReadBuffer, ReadBufferSize) then begin
      AsyncStatus := epFatal+ecOutOfMemory;
      Done;
      Fail;
    end;

    {set other variables}
    OutFileName := '';
    AsyncStatus := ecOK;
  end;

  destructor TextFaxConverter.Done;
    {-destroy object when done}
  begin
    FreeMemCheck(ReadBuffer, ReadBufferSize);
    AbstractFaxConverter.Done;
  end;

  function TextFaxConverter.acOpenInputFile : Integer;
    {-open our input file}
  begin
    Assign(InFile, InFileName);
    SetTextBuf(InFile, ReadBuffer^, ReadBufferSize);
    Reset(InFile);
    acOpenInputFile := IOResult;
  end;

  procedure TextFaxConverter.ConvertFax(FName : PathStr);
    {-convert ASCII text file FName to FAX bitmap file}
  label
    Error;
  var
    LC : Integer;
    I : Integer;
    DonePage : Boolean;
    SkipRead : Boolean;                                                {!!.02}
    BufLen : Word;
    S : String;
  begin
    acInitHeaders;

    CurrPage := 0;
    InFileName := StUpcase(FName);
    InFileName := DefaultExtension(InFileName, 'TXT');

    if not FontLoaded then
      if not LoadFont(StandardFont, UseHighRes) then begin
        AsyncStatus := epFatal+ecFaxNoFontFile;
        Exit;
      end;

    {show initial status}
    AsyncStatus := ecOk;
    if FaxConvertStatus(True, False) then begin
      AsyncStatus := ecUserAbort;
      Exit;
    end;

    {open the input file}
    AsyncStatus := acOpenInputFile;
    if AsyncStatus <> ecOK then begin
      if FaxConvertStatus(False, True) then ;
      Exit;
    end;

    {Create the initial output file}
    AsyncStatus := acCreateOutputFile(FName);
    if AsyncStatus <> ecOK then
      goto Error;

    {loop over text, compressing and paginating:}
    SkipRead := False;                                                 {!!.02}
    while True do begin
      Inc(CurrPage);
      CurrLine := 1;

      {Add the page header}
      FastZero(PageHeader, SizeOf(PageHeader));                        {!!.03}
      with PageHeader do begin
        ImgFlags := ffLengthWords;
        if UseHighRes then
          ImgFlags := ImgFlags or ffHighRes;
        if (ResWidth = WideWidth) then                                 {!!.03}
          ImgFlags := ImgFlags or ffHighWidth;                         {!!.03}
      end;
      if not acAddData(PageHeader, SizeOf(PageHeader), False) then begin
        AsyncStatus := ecDeviceWrite;
        goto Error;
      end;

      DonePage := False;
      LC := 0;

      {Add top margin}
      if FaxTopMargin <> 0 then
        if not acAddBlankLines(FaxTopMargin) then begin
          AsyncStatus := ecDeviceWrite;
          goto Error;
        end;

      while (not(EOF(InFile))) and (not(DonePage)) do begin
        if not SkipRead then                                           {!!.02}
          ReadLn(InFile, S);                                           {!!.02}
        SkipRead := False;                                             {!!.02}
        if IOResult <> 0 then begin
          AsyncStatus := ecDeviceRead;
          goto Error;
        end;

        if (Length(S) <> 0) and (S[1] = #12) then begin                {!!.02}
          {Handle form feeds}
          DonePage := True;                                            {!!.02}
          Delete(S, 1, 1);                                             {!!.02}
          SkipRead := True;                                            {!!.02}
        end else begin                                                 {!!.02}
          {!!.01 change start}
          {Rasterize and compress text line}
          for I := 1 to FontRec.Height do begin
            acCompressStringRowPrim(S, I, False);                      {!!.03}
            if not acAddLine(DataLine^, ByteOfs) then begin            {!!.03}
              AsyncStatus := ecDeviceWrite;
              goto Error;
            end;
          end;
          {!!.01 change end}

          {track lines converted}
          Inc(CurrLine);
          Inc(LC);
          if (LineCount > 0) and (LC >= LineCount) then
            DonePage := True;

          {Show the user}
          if FaxConvertStatus(False, False) then begin
            AsyncStatus := ecUserAbort;
            goto Error;
          end;
        end;
      end;

      if (EOF(InFile) or DonePage) and                                 {!!.03}
         (LineCount > 0) and                                           {!!.03}
         (LC < LineCount) then                                         {!!.03}
        {!!.01 change start}
        {pad out the page to our defined line count}
        for I := LC to LineCount-1 do begin                            {!!.02}
          if not acAddBlankLines(FontRec.Height) then begin            {!!.02}
            AsyncStatus := ecDeviceWrite;
            goto Error;
          end;
        end;                                                           {!!.02}
        {!!.01 change end}

      {write the end-of-page marker}
      acMakeEndOfPage(TmpBuffer^, BufLen);
      if not acAddLine(TmpBuffer^, BufLen) then begin
        AsyncStatus := ecDeviceWrite;
        goto Error;
      end;

      {show the user}
      if FaxConvertStatus(False, False) then begin
        AsyncStatus := ecUserAbort;
        goto Error;
      end;

      Inc(MainHeader.PageCount);
      if (acUpdatePageHeader(MainHeader.PageCount, PageHeader) <> 0) then begin
        AsyncStatus := ecDeviceWrite;
        goto Error;
      end;

      if EOF(InFile) then begin
        {all lines converted; update the main header in file}
        if acUpdateMainHeader <> 0 then begin
          AsyncStatus := ecDeviceWrite;
          goto Error;
        end;
        Close(OutFile);
        if IOResult = 0 then ;
        Close(InFile);
        if IOResult = 0 then ;

        AsyncStatus := ecOK;
        if FaxConvertStatus(False, True) then ;
        Exit;
      end;
    end;

Error:
    {if we get here, some error occured}
    Erase(OutFile);
    if IOResult = 0 then ;
    Close(InFile);
    if IOResult = 0 then ;
    if FaxConvertStatus(False, True) then ;
  end;

  {$IFDEF UseStreams}
  constructor TextFaxConverter.Load(var S : IdStream);
    {-Abstract Load for a converter object}
  begin
    ReadBuffer := nil;

    {Use ancestor load}
    AbstractFaxConverter.Load(S);
    if S.PeekStatus <> 0 then begin
      Done;
      Fail;
    end;

    {Allocate buffer}
    if not GetMemCheck(ReadBuffer, ReadBufferSize) then begin
      AsyncStatus := epFatal+ecOutOfMemory;
      Done;
      Fail;
    end;

    {Other inits}
    OutFileName := '';
    AsyncStatus := ecOK;
  end;
  {$ENDIF}

{Pcx/Dcx FaxConverter object}

  constructor AbstractPcxFaxConverter.Init;
  begin
    if not AbstractFaxConverter.Init then
      Fail;
    ReadBuffer := nil;
    if not GetMemCheck(ReadBuffer, ReadBufferSize) then begin
      AsyncStatus := epFatal+ecOutOfMemory;
      Done;
      Fail;
    end;

    FastZero(PCXHeader, SizeOf(PCXHeader)); {!!.03}
    AsyncStatus := ecOK;
  end;

  destructor AbstractPcxFaxConverter.Done;
  begin
    FreeMemCheck(ReadBuffer, ReadBufferSize);
    AbstractFaxConverter.Done;
  end;

  function AbstractPcxFaxConverter.pcValidatePcxHdr : Boolean;
  begin
    pcValidatePcxHdr := False;
    with PcxHeader do begin
      {if Mfgr byte <> $0A or Encoding byte <> $01, it's not a PCX file}
      if (Mfgr <> $0A) or (Encoding <> $01) then
        Exit;

      {we only handle color depth = 1 (monochrome)}
      if (BitsPixel <> $01) then
        Exit;

      {validate image is not too wide}
      if (XMax-XMin) > ResWidth then
        Exit;

      pcValidatePcxHdr := True;
    end;
  end;

  procedure AbstractPcxFaxConverter.pcReadRasterLine(var Buffer);
    {-Read a PCX raster line into Buffer.  Buffer is assumed to be at least
      ResWidth bits in size.}
  var
    T : TByteBuffer absolute Buffer;
    N : Integer;
    B, L : Byte;
    First : Boolean;
    Ok : Boolean;                                                      {!!.01}
  begin
    AsyncStatus := ecOK;
    First := False;

    {unpack the next raster line of PCX data}
    N := 0;
    while (PcxBytes > 0) and (N < PcxHeader.BytesLine) do begin
      if CurrRBOfs >= CurrRBSize then begin
        BlockRead(InFile, ReadBuffer^, ReadBufferSize, CurrRBSize);
        AsyncStatus := IOResult;
        if AsyncStatus <> ecOK then
          Exit;
        if (CurrRBSize > PcxBytes) then
          CurrRBSize := PcxBytes;
        CurrRBOfs := 0;
      end;
      B := ReadBuffer^[CurrRBOfs];
      Inc(CurrRBOfs);

      if First then begin
        B := not B;             {PCX files store in reverse format (1=white)}
        {Make sure it fits}                                            {!!.01}
        if N+L < MaxData then begin                                    {!!.01}
          {Ignore extra bytes for 2.8 and earlier}                     {!!.01}
          if PcxHeader.Ver <= 3 then                                   {!!.01}
            Ok := N+L <= ActBytesLine                                  {!!.01}
          else                                                         {!!.01}
            Ok := True;                                                {!!.01}
          if Ok then                                                   {!!.01}
            FillChar(T[n], L, B);                                      {!!.01}
        end;                                                           {!!.01}
        Inc(N, L);
        First := False;
      end else if B and $C0 = $C0 then begin
        L := B and $3F;
        First := True;
      end else begin
        if (N < MaxData) then
          T[n] := not B;
        Inc(N);
      end;
      Dec(PcxBytes);
    end;
  end;

  {$IFDEF UseStreams}
  constructor AbstractPcxFaxConverter.Load(var S : IdStream);
    {-Load}
  begin
    ReadBuffer := nil;

    {Use ancestor load}
    AbstractFaxConverter.Load(S);
    if S.PeekStatus <> 0 then begin
      Done;
      Fail;
    end;

    {Allocate buffer}
    if not GetMemCheck(ReadBuffer, ReadBufferSize) then begin
      AsyncStatus := epFatal+ecOutOfMemory;
      Done;
      Fail;
    end;
    FastZero(PCXHeader, SizeOf(PCXHeader)); {!!.03}
    AsyncStatus := ecOK;
  end;
  {$ENDIF}

  procedure AbstractPcxFaxConverter.pcUnpackImage;
  var
    LC, X     : Integer;
    PcxWidth  : Word;
    CenterOfs : Word;
    ByteWidth : Word;

  begin
    if not pcValidatePcxHdr then begin
      AsyncStatus := ecBadGraphicsFormat;
      Exit;
    end;

    {need double width scaling?}
    with PcxHeader do begin
      PcxWidth := (XMax - XMin) + 1;
      if FlagIsSet(fcFlags, fcDoubleWidth) then
        DoubleWidth := not UseHighRes and                              {!!.01}
                       ((PcxWidth * 2) <= (ResWidth - FaxLeftMargin)); {!!.01}
    end;

    {center pcx image in fax image}
    CenterOfs := 0;
    if FlagIsSet(fcFlags, fcCenterImage) then
      if (ResWidth - PcxWidth) >= 16 then begin
        {only center if at least one byte on each side}
        if DoubleWidth then
          CenterOfs := (ResWidth - (PcxWidth * 2)) div 32
        else
          CenterOfs := (ResWidth - PcxWidth) div 16;
      end;

    {create page header}
    FastZero(PageHeader, SizeOf(PageHeader)); {!!.03}
    with PageHeader do begin
      ImgFlags := ffLengthWords;
      if UseHighRes then
        ImgFlags := ImgFlags or ffHighRes;
      if (ResWidth = WideWidth) then                                 {!!.03}
        ImgFlags := ImgFlags or ffHighWidth;                         {!!.03}
    end;
    if not acAddData(PageHeader, SizeOf(PageHeader), False) then begin
      AsyncStatus := ecDeviceWrite;
      Exit;
    end;

    {Add top margin}
    if FaxTopMargin <> 0 then
      if not acAddBlankLines(FaxTopMargin) then begin
        AsyncStatus := ecDeviceWrite;
        Exit;
      end;

    {read PCX file, compressing}
    LC := 0;
    CurrRBSize := 1;
    CurrRBOfs := $FFFF;
    while (PcxBytes > 0) and (AsyncStatus = ecOK) and (CurrRBSize <> 0) do begin
      {Get next raster line from PCX}
      FastZero(TmpBuffer^, MaxData); {!!.03}
      pcReadRasterLine(TmpBuffer^);
      if CurrRBSize <> 0 then begin
        ByteWidth := PcxWidth div 8;
        if CenterOfs <> 0 then begin
          if (CenterOfs+ByteWidth) < MaxData then begin
            Move(TmpBuffer^[0], TmpBuffer^[CenterOfs], PcxWidth div 8);
            FastZero(TmpBuffer^[0], CenterOfs);  {!!.03}
          end;
        end;
        acCompressRasterLine(TmpBuffer^);

        if not acAddLine(DataLine^, ByteOfs) then begin
          AsyncStatus := ecDeviceWrite;
          Exit;
        end;

        {track lines converted}
        Inc(LC);
        if LC >= UpdateInterval then begin
          {update the user every UpdInterval lines}
          Inc(CurrLine);
          LC := 0;

          {show the user}
          if FaxConvertStatus(False, False) then begin
            AsyncStatus := ecUserAbort;
            Exit;
          end;
        end;
      end;
    end;
  end;

  function PcxFaxConverter.acOpenInputFile : Integer;
    {-open input file}
  var
    W : Integer;
  begin
    SaveMode := FileMode;                                              {!!.02}
    FileMode := $40;                                                   {!!.02}
    Assign(InFile, InFileName);
    Reset(InFile, 1);
    FileMode := SaveMode;                                              {!!.02}
    W := IOResult;
    if W = 0 then begin
      BlockRead(InFile, PcxHeader, SizeOf(PcxHeader));
      W := IOResult;
    end;
    if (W = ecOK) then
      PcxBytes := FileSize(InFile) - SizeOf(PcxHeader);
    ActBytesLine := (PcxHeader.XMax-PcxHeader.XMin+1) div 8;           {!!.01}
    acOpenInputFile := W;
  end;

  procedure PcxFaxConverter.ConvertFax(FName : PathStr);
  label
    Error;
  var
    W : Word;
  begin
    acInitHeaders;

    CurrPage := 1;
    InFileName := StUpcase(FName);
    InFileName := DefaultExtension(InFileName, 'PCX');

    {show initial status}
    AsyncStatus := ecOk;
    if FaxConvertStatus(True, False) then begin
      AsyncStatus := ecUserAbort;
      Exit;
    end;

    {open the input file}
    AsyncStatus := acOpenInputFile;
    if AsyncStatus <> ecOK then begin
      if FaxConvertStatus(False, True) then ;
      Exit;
    end;

    {create the initial output file}
    AsyncStatus := acCreateOutputFile(FName);
    if AsyncStatus <> ecOK then
      goto Error;

    pcUnpackImage;
    if (AsyncStatus <> 0) then
      goto Error;

    {write the end-of-page marker}
    acMakeEndOfPage(TmpBuffer^, W);
    if not acAddLine(TmpBuffer^, W) then begin
      AsyncStatus := ecDeviceWrite;
      goto Error;
    end;

    {update page and file headers}
    Inc(MainHeader.PageCount);
    if (acUpdatePageHeader(MainHeader.PageCount, PageHeader) <> 0) then begin
      AsyncStatus := ecDeviceWrite;
      goto Error;
    end;
    if acUpdateMainHeader <> 0 then begin
      AsyncStatus := ecDeviceWrite;
      goto Error;
    end;

    {show the user}
    if FaxConvertStatus(False, False) then begin
      AsyncStatus := ecUserAbort;
      goto Error;
    end;

    AsyncStatus := ecOK;
    Close(InFile);
    if IoResult = 0 then ;
    Close(OutFile);
    if IoResult = 0 then ;
    DoubleWidth := False;
    Exit;

Error:
    {if we jumped here, some error occured}
    Close(OutFile);
    Erase(OutFile);
    if IOResult = 0 then ;
    Close(InFile);
    if IOResult = 0 then ;
    if FaxConvertStatus(False, True) then ;
    DoubleWidth := False;
  end;

  function DcxFaxConverter.acOpenInputFile : Integer;
    {- open input file }
  var
    W     : Integer;
    R     : Word;
    I     : Word;
    J     : Word;
    PJ    : Word;
    Pivot : LongInt;
    Sz    : LongInt;

  begin
    SaveMode := FileMode;                                              {!!.02}
    FileMode := $40;                                                   {!!.02}
    Assign(InFile, InFileName);
    Reset(InFile, 1);
    FileMode := SaveMode;                                              {!!.02}
    W := IoResult;
    if (W = 0) then begin
      BlockRead(InFile, DcxHeader, SizeOf(DcxHeader), R);
      Sz := FileSize(InFile);

      W := IoResult;

      if (W = 0) then begin

        { figure out how many pages there are in the index }
        DcxNumPag := 1;
        while (DcxHeader.Offsets[DcxNumPag] <> 0) do
          Inc(DcxNumPag);
        Dec(DcxNumPag);

        { sort the index }
        Move(DcxHeader.Offsets, DcxPgSz, SizeOf(LongInt) * DcxNumPag);
        if (DcxNumPag > 1) then begin
          repeat
            Pivot := DcxPgSz[1];
            for J := 2 to DcxNumPag do begin
              PJ := Pred(J);
              if (DcxPgSz[PJ] > DcxPgSz[J]) then begin
                Pivot := DcxPgSz[PJ];
                DcxPgSz[PJ] := DcxPgSz[J];
                DcxPgSz[J]  := Pivot;
              end;
            end;
          until (Pivot = DcxPgSz[1]);

          for I := 1 to Pred(DcxNumPag) do
            DcxPgSz[I] := DcxPgSz[Succ(I)] - DcxPgSz[I];
          DcxPgSz[DcxNumPag] := Sz - DcxPgSz[DcxNumPag];
        end else
          DcxPgSz[1] := FileSize(InFile)-DcxHeader.Offsets[1]+1;
      end;
    end;
    acOpenInputFile := W;
  end;

  procedure DcxFaxConverter.ConvertFax(FName : PathStr);
  label
    Error;
  var
    W : Word;
  begin
    acInitHeaders;

    InFileName := StUpCase(FName);
    InFileName := DefaultExtension(InFileName, 'DCX');

    {show initial status}
    AsyncStatus := ecOk;
    if FaxConvertStatus(True, False) then begin
      AsyncStatus := ecUserAbort;
      Exit;
    end;

    {open the input file}
    AsyncStatus := acOpenInputFile;
    if AsyncStatus <> ecOK then begin
      if FaxConvertStatus(False, True) then ;
      Exit;
    end;

    if (DcxHeader.ID <> 987654321) then begin
      AsyncStatus := ecBadGraphicsFormat;
      Exit;
    end;

    {create the initial output file}
    AsyncStatus := acCreateOutputFile(FName);
    if AsyncStatus <> ecOK then
      goto Error;

    { loop through all "pages" in the index }
    CurrPage := 1;
    while (DcxHeader.Offsets[CurrPage] <> 0) do begin
      CurrLine := 1;

      if FaxConvertStatus(False, False) then begin
        AsyncStatus := ecUserAbort;
        goto Error;
      end;

      Seek(InFile, DcxHeader.Offsets[CurrPage]);
      { read the pcx header }
      BlockRead(InFile, PcxHeader, SizeOf(PcxHeader));
      AsyncStatus := IoResult;
      if (AsyncStatus <> 0) then
        goto Error;

      PcxBytes := DcxPgSz[CurrPage] - SizeOf(PcxHeader);

      pcUnpackImage;
      if (AsyncStatus <> ecOK) then
        goto Error;

      { make the end-of-page marker }
      acMakeEndOfPage(TmpBuffer^, W);
      if not acAddLine(TmpBuffer^, W) then begin
        AsyncStatus := ecDeviceWrite;
        goto Error;
      end;

      { update page header }
      Inc(MainHeader.PageCount);
      if (acUpdatePageHeader(MainHeader.PageCount, PageHeader) <> 0) then begin
        AsyncStatus := ecDeviceWrite;
        goto Error;
      end;

      Inc(CurrPage);
    end;

    Dec(CurrPage);

    AsyncStatus := acUpdateMainHeader;
    if (AsyncStatus <> 0) then
      goto Error;

    Close(InFile );
    if IoResult = 0 then ;
    Close(OutFile);
    if IoResult = 0 then ;
    AsyncStatus := ecOK;
    DoubleWidth := False;
    if FaxConvertStatus(False, True) then ;
    Exit;

Error:
    Close(InFile );
    if IoResult = 0 then ;
    Close(OutFile);
    if IoResult = 0 then ;
    Erase(OutFile);
    if IoResult = 0 then ;
    DoubleWidth := False;
    if FaxConvertStatus(False, True) then ;
  end;

{TiffFaxConverter object}

  constructor TiffFaxConverter.Init;
    {-initialize our PCX-to-fax converter}
  begin
    ReadBuffer := nil;

    if not AbstractFaxConverter.Init then
      Fail;

    if not GetMemCheck(ReadBuffer, ReadBufferSize) then begin
      AsyncStatus := epFatal+ecOutOfMemory;
      Done;
      Fail;
    end;

    tiffSubFile  := 0;
    tiffWidth    := 0;
    tiffBytes    := 0;
    tiffLength   := 0;
    tiffComp     := 1;
    tiffPhotoMet := 0;
    tiffRowStrip := 0;
    tiffStripOfs := 0;
    tiffStripCnt := 0;
    tiffImgStart := 0;

    AsyncStatus  := ecOK;
  end;

  destructor TiffFaxConverter.Done;
    {-destroy object when done}
  begin
    FreeMemCheck(ReadBuffer, ReadBufferSize);
    AbstractFaxConverter.Done;
  end;

  function TiffFaxConverter.acOpenInputFile : Integer;
    {-open input file}
  var
    W : Integer;
  begin
    CurrRBSize := 0;
    CurrRBOfs := $FFFF;
    SaveMode := FileMode;                                              {!!.02}
    FileMode := $40;                                                   {!!.02}
    Assign(InFile, InFileName);
    Reset(InFile, 1);
    FileMode := SaveMode;                                              {!!.02}
    W := IOResult;
    acOpenInputFile := W;
  end;

  function TiffFaxConverter.tcSeek(NewOfs : LongInt) : Boolean;
  begin
    {seek to location in InFile}
    Seek(InFile, NewOfs);
    AsyncStatus := IOResult;
    tcSeek := (AsyncStatus = 0);

    {force buffer reload}
    CurrRBSize := 0;
    CurrRBOfs := 999;
  end;

  function TiffFaxConverter.tcGetByte : Byte;
    {-Retrieve the next byte from the file}
  begin
    AsyncStatus := 0;
    if CurrRBOfs >= CurrRBSize then begin
      BlockRead(InFile, ReadBuffer^, ReadBufferSize, CurrRBSize);
      AsyncStatus := IOResult;
      CurrRBOfs := 0;
    end;
    tcGetByte := ReadBuffer^[CurrRBOfs];
    Inc(CurrRBOfs);
  end;

  function TiffFaxConverter.tcGetWord : Word;
    {-Get the next word from the file, accounting for byte ordering}
  var                {!!.02}
    B1, B2 : Byte;   {!!.02}

  begin
    B1 := tcGetByte; {!!.02}
    B2 := tcGetByte; {!!.02}

    if Intel then
      tcGetWord := B1 + Word(B2 shl 8)  {!!.02}
    else
      tcGetWord := B2 + Word(B1 shl 8); {!!.02}
  end;

  function TiffFaxConverter.tcGetLong : LongInt;
    {-Get the next longint from the file, accounting for byte ordering}
  var
    B1, B2, B3, B4 : Byte; {!!.02}

  begin
    B1 := tcGetByte;       {!!.02}
    B2 := tcGetByte;       {!!.02}
    B3 := tcGetByte;       {!!.02}
    B4 := tcGetByte;       {!!.02}

    if Intel then
      tcGetLong := LongInt(B4) shl 24 +  {!!.02}
                   LongInt(B3) shl 16 +  {!!.02}
                   LongInt(B2) shl 8  +  {!!.02}
                   LongInt(B1)           {!!.02}
    else
      tcGetLong := LongInt(B1) shl 24 +  {!!.02}
                   LongInt(B2) shl 16 +  {!!.02}
                   LongInt(B3) shl 8  +  {!!.02}
                   LongInt(B4);          {!!.02}
  end;

  function TiffFaxConverter.tcValidTIFF : Boolean;
    {-Validate as TIFF file and set Intel byte-order flag}
  var
    C1, C2 : Char;
    L : LongInt;
  begin
    tcValidTIFF := False;

    {Get Intel or Motorola byte-order flags}
    C1 := Char(tcGetByte);
    C2 := Char(tcGetByte);
    if (C1 = 'I') and (C2 = 'I') then
      Intel := True
    else if (C1 = 'M') and (C2 = 'M') then
      Intel := False
    else
      Exit;

    {Get version flag}
    tiffVersion := tcGetWord;
    if tiffVersion <> 42 then
      {Unsupported version of TIFF}
      Exit;

    {Find start of image  directory}
    L := tcGetLong;
    if (L > 0) and tcSeek(L) then
      tcValidTIFF := True;
  end;

  function TiffFaxConverter.tcDecodeTag : LongInt;
    {-Retrieve and decode the next tag field from the file}
  const
    LastMasks : array[0..7] of Byte =
      ($FF, $80, $C0, $E0, $F0, $F8, $FC, $FE);

  var
    Tag, TagType : Word;
    Len, Offset  : LongInt;

    function Pixels2Bytes(W : Word) : Word;
    begin
      if (W and $0007 <> 0) then
        Pixels2Bytes := (W shr 3)+1
      else
        Pixels2Bytes := (W shr 3);
    end;

  begin
    {abort on pending error}
    if AsyncStatus <> 0 then
      Exit;

    {Get next tag}
    Tag := tcGetWord;
    TagType := tcGetWord;
    case TagType of
      tiffShort : begin
                    Len := tcGetWord;
                    if tcGetWord = 0 then ;
                    Offset := tcGetWord;
                    if tcGetWord = 0 then ;
                  end;
      tiffLong : begin
                   Len := tcGetLong;
                   Offset := tcGetLong;
                 end;
      else begin
        {Unsupported type, just read and discard the data}
        Len := tcGetLong;
        Offset := tcGetLong;
      end;
    end;

    case Tag of
      SubfileType:
        tiffSubFile := Word(Offset);
      ImageWidth:
        begin
          tiffWidth := Word(Offset);
          tiffBytes := Pixels2Bytes(tiffWidth)-1;
          LastBitMask := LastMasks[tiffWidth mod 8];
        end;
      ImageLength:
        tiffLength := Word(Offset);
      RowsPerStrip:
        if TagType = tiffLong then
          tiffRowStrip := Offset
        else
          tiffRowStrip := Offset and $0000FFFF;
      StripOffsets:
        if TagType = tiffLong then begin
          tiffStripOfs := Offset;
          tiffStripCnt := Len;
        end else begin
          tiffStripOfs := Offset and $0000FFFF;
          tiffStripCnt := Len and $0000FFFF;
        end;
      Compression:
        begin                                                          {!!.01}
          tiffComp := Word(Offset);
          if (tiffComp <> compNone) and (tiffComp <> compMPNT) then    {!!.01}
            AsyncStatus := ecBadGraphicsFormat;                        {!!.01}
        end;                                                           {!!.01}
      PhotoMetricInterp:
        tiffPhotoMet := Word(Offset);
      BitsPerSample :
        if Offset <> 1 then
          AsyncStatus := ecBadGraphicsFormat;
      StripByteCounts :
        tiffByteCntOfs := Offset;
      else
        {we ignore other tags} ;
    end;

    tcDecodeTag := Len;
  end;

  function TiffFaxConverter.tcReadTagDir : Boolean;
    {-Read the tags from the "image directory"}
  var
    W, X : Word;
  begin
    {Assume we are at the start of a directory; get the tags count}
    tcReadTagDir := False;
    W := tcGetWord;
    {Read  that many tags and decode}
    for X := 1 to W do begin
      if tcDecodeTag = 0 then ;                                        {!!.01}
      if AsyncStatus <> 0 then                                         {!!.01}
        Exit;                                                          {!!.01}
    end;                                                               {!!.01}

    {Have we picked up the data we need?}
    if (tiffWidth = 0) or (tiffLength = 0) or (tiffStripOfs = 0) then
      Exit;

    tcReadTagDir := True;
  end;

  procedure TiffFaxConverter.tcReadRasterLine(var Buffer);
    {-Read and decompress the next line of raster data from the file}
  var
    T : TByteBuffer absolute Buffer;
    W : Word;
    B : Byte;
    C : Byte;

  begin
    if tiffComp = compNone then begin
      for W := 0 to tiffBytes do
        T[W] := tcGetByte;
    end else if tiffComp = compMPNT then begin
      W := 0;
      while W < tiffBytes do begin
        B := tcGetByte;
        if B > $80 then begin
          B := not(B)+2;
          C := tcGetByte;
          if W+B < MaxData then                                        {!!.01}
            FillChar(T[W], B, C);
          Inc(W, B);
        end else begin
          B := B+1;
          while B > 0 do begin
            if W < MaxData then                                        {!!.01}
              T[W] := tcGetByte;
            Inc(W);
            Dec(B);
          end;
        end;
      end;
    end;
    if tiffPhotoMet > 0 then
      for W := 0 to tiffBytes do begin
        T[W] := not(T[W]);
        if W = tiffBytes then
          T[W] := T[W] and LastBitMask;
      end;
  end;

  {!!.01 new}
  procedure TiffFaxConverter.tcLoadStripInfo;
    {-Load strip offsets/lengths}
  var
    Len : Word;
    I : Word;
  begin
    {Get memory for array}
    Len := tiffStripCnt * SizeOf(StripRecord);
    if not GetMemCheck(tiffStripInfo, Len) then begin
      AsyncStatus := ecOutOfMemory;
      Exit;
    end;

    {Seek to start of strip byte count list}
    if not tcSeek(tiffByteCntOfs) then
      Exit;

    {Load lengths}
    I := 1;
    while (I <= tiffStripCnt) and (AsyncStatus = ecOk) do begin
      tiffStripInfo^[I].Length := tcGetLong;
      Inc(I);
    end;
    if AsyncStatus <> ecOk then
      Exit;

    {Seek to start of strip offset list}
    if not tcSeek(tiffStripOfs) then
      Exit;

    {Load offsets}
    I := 1;
    while (I <= tiffStripCnt) and (AsyncStatus = ecOk) do begin
      tiffStripInfo^[I].Offset := tcGetLong;
      Inc(I);
    end;
  end;

  {!!.01 many changes for multi-strip image support}
  procedure TiffFaxConverter.ConvertFax(FName : PathStr);
    {-Convert FName TIFF file to FAX bitmap file}
  label
    Error;
  var
    W : Word;
    LC : Integer;
    X : Integer;
    L, M : LongInt;
    CenterOfs : Word;
    ByteWidth : Word;
    Len : Word;

    procedure DecodeStrip;
      {-Decode current strip, assume already seeked to image start}
    var
      W : Word;
    begin
      LC := 0;
      for W := 0 to tiffLength-1 do begin
        FastZero(TmpBuffer^, MaxData); {!!.03}
        tcReadRasterLine(TmpBuffer^);
        ByteWidth := (tiffWidth div 8) + Ord((tiffWidth mod 8) <> 0);  {!!.02}
        if CenterOfs <> 0 then begin
          if (CenterOfs+ByteWidth) < MaxData then begin
            Move(TmpBuffer^[0], TmpBuffer^[CenterOfs], ByteWidth);     {!!.02}
            FastZero(TmpBuffer^[0], CenterOfs);                        {!!.03}
          end;
        end;
        acCompressRasterLine(TmpBuffer^);

        if not acAddLine(DataLine^, ByteOfs) then begin
          AsyncStatus := ecDeviceWrite;
          Exit;
        end;

        {track lines converted}
        Inc(LC);
        if LC >= UpdateInterval then begin
          {update the user every UpdInterval lines}
          Inc(CurrLine);
          LC := 0;

          {show the user}
          if FaxConvertStatus(False, False) then begin
            AsyncStatus := ecUserAbort;
            Exit;
          end;
        end;
      end;
    end;

  begin
    acInitHeaders;

    CurrPage := 1;
    InFileName := StUpcase(FName);
    InFileName := DefaultExtension(InFileName, 'TIF');

    {show initial status}
    AsyncStatus := ecOk;
    if FaxConvertStatus(True, False) then begin
      AsyncStatus := ecUserAbort;
      Exit;
    end;

    {open the input file}
    AsyncStatus := acOpenInputFile;
    if AsyncStatus <> ecOK then begin
      if FaxConvertStatus(False, True) then ;
      Exit;
    end;

    {validate TIFF file format, read tags dir}
    if (not tcValidTIFF) or (not tcReadTagDir) then begin
      AsyncStatus := ecBadGraphicsFormat;
      goto Error;
    end;

    {If it's a multi-strip file, load the strip offset/lengths}
    if tiffStripCnt > 1 then begin
      tcLoadStripInfo;
      if AsyncStatus <> ecOk then
        goto Error;
    end;

    {create the initial output file}
    AsyncStatus := acCreateOutputFile(FName);
    if AsyncStatus <> ecOK then
      goto Error;

    {create page header}
    FastZero(PageHeader, SizeOf(PageHeader)); {!!.03}
    with PageHeader do begin
      ImgFlags := ffLengthWords;
      if UseHighRes then
        ImgFlags := ImgFlags or ffHighRes;
      if (ResWidth = WideWidth) then                                 {!!.03}
        ImgFlags := ImgFlags or ffHighWidth;                         {!!.03}
    end;
    if not acAddData(PageHeader, SizeOf(PageHeader), False) then begin
      AsyncStatus := ecDeviceWrite;
      goto Error;
    end;

    {Add top margin}
    if FaxTopMargin <> 0 then
      if not acAddBlankLines(FaxTopMargin) then begin
        AsyncStatus := ecDeviceWrite;
        goto Error;
      end;

    {need double width scaling?}
    if FlagIsSet(fcFlags, fcDoubleWidth) then
      DoubleWidth := not UseHighRes and                                {!!.01}
                     ((tiffWidth * 2) <= (ResWidth - FaxLeftMargin));  {!!.01}

    {center image in fax image}
    CenterOfs := 0;
    if FlagIsSet(fcFlags, fcCenterImage) then
      if (ResWidth - tiffWidth) >= 16 then begin
        {only center if at least one byte on each side}
        if DoubleWidth then
          CenterOfs := (ResWidth - (tiffWidth * 2)) div 32
        else
          CenterOfs := (ResWidth - tiffWidth) div 16;
      end;

    {read TIFF file, compressing}
    LC := 0;
    if tiffStripCnt = 1 then begin

      {Single-strip image, process this strip}
      if not tcSeek(tiffImgStart+tiffStripOfs) then
        goto Error;

      {Decode this strip}
      DecodeStrip;
      if AsyncStatus <> ecOk then
        goto Error;

    end else begin
      for L := 1 to tiffStripCnt do begin
        {Multiple-strip image, seek to next strip}
        if not tcSeek(tiffStripInfo^[L].Offset) then
          goto Error;

        {Set single-strip values}
        tiffLength := tiffStripInfo^[L].Length div tiffBytes;
        if tiffStripInfo^[L].Length mod tiffBytes <> 0 then
          Inc(tiffLength);

        {Decode this strip}
        DecodeStrip;
        if AsyncStatus <> ecOk then
          goto Error;
      end;

      {Release the strip info}
      Len := tiffStripCnt * SizeOf(StripRecord);
      FreeMemCheck(tiffStripInfo, Len);
    end;

    {Write the end-of-page marker}
    acMakeEndOfPage(TmpBuffer^, W);
    if not acAddLine(TmpBuffer^, W) then begin
      AsyncStatus := ecDeviceWrite;
      goto Error;
    end;

    {update page and file headers}
    Inc(MainHeader.PageCount);
    if (acUpdatePageHeader(MainHeader.PageCount, PageHeader) <> 0) then begin
      AsyncStatus := ecDeviceWrite;
      goto Error;
    end;
    if acUpdateMainHeader <> 0 then begin
      AsyncStatus := ecDeviceWrite;
      goto Error;
    end;

    {show the user}
    if FaxConvertStatus(False, False) then begin
      AsyncStatus := ecUserAbort;
      goto Error;
    end;

    AsyncStatus := ecOK;

Error:
    {if we jumped here, some error occured; clean up}
    if AsyncStatus <> ecOk then
      Erase(OutFile)
    else
      Close(OutFile);
    if IOResult = 0 then ;
    Close(InFile);
    if IOResult = 0 then ;
    if FaxConvertStatus(False, True) then ;
  end;

  {$IFDEF UseStreams}
  constructor TiffFaxConverter.Load(var S : IdStream);
    {-Load}
  begin
    ReadBuffer := nil;

    {Use ancestor load}
    AbstractFaxConverter.Load(S);
    if S.PeekStatus <> 0 then begin
      Done;
      Fail;
    end;

    {Allocate buffer}
    if not GetMemCheck(ReadBuffer, ReadBufferSize) then begin
      AsyncStatus := epFatal+ecOutOfMemory;
      Done;
      Fail;
    end;

    tiffSubFile  := 0;
    tiffWidth    := 0;
    tiffBytes    := 0;
    tiffLength   := 0;
    tiffComp     := 1;
    tiffPhotoMet := 0;
    tiffRowStrip := 0;
    tiffStripOfs := 0;
    tiffStripCnt := 0;
    tiffImgStart := 0;
    AsyncStatus  := ecOK;
  end;
  {$ENDIF}

{!!.03 - begin changes}

  constructor BmpFaxConverter.Init;
  begin
    if not AbstractFaxConverter.Init then
      Fail;

    BmpByteWidth := 0;
    BytesPerLine := 0;
    Offset       := 0;

    FillChar(FileHeader, SizeOf(FileHeader), 0);
    FillChar(InfoHeader, SizeOf(InfoHeader), 0);
  end;

  procedure BmpFaxConverter.ConvertFax(FName : PathStr);
  label
    Error;

  var
    CenterOfs : Word;
    W         : Word;
    LC        : Word;

  begin
    acInitHeaders;

    CurrPage   := 1;
    InFileName := DefaultExtension(StUpCase(FName), 'BMP');

    {show initial status}
    AsyncStatus := ecOk;
    if FaxConvertStatus(True, False) then begin
      AsyncStatus := ecUserAbort;
      Exit;
    end;

    {open the input file}
    AsyncStatus := acOpenInputFile;
    if (AsyncStatus <> ecOK) then begin
      if FaxConvertStatus(False, True) then ;
      Exit;
    end;

    {create the initial output file}
    AsyncStatus := acCreateOutputFile(FName);
    if (AsyncStatus <> ecOK) then
      goto Error;

    {create page header}
    FastZero(PageHeader, SizeOf(PageHeader));
    with PageHeader do begin
      ImgFlags := ffLengthWords;
      if UseHighRes then
        ImgFlags := ImgFlags or ffHighRes;
      if (ResWidth = WideWidth) then
        ImgFlags := ImgFlags or ffHighWidth;
    end;
    if not acAddData(PageHeader, SizeOf(PageHeader), False) then begin
      AsyncStatus := ecDeviceWrite;
      goto Error;
    end;

    {Add top margin}
    if (FaxTopMargin <> 0) then
      if not acAddBlankLines(FaxTopMargin) then begin
        AsyncStatus := ecDeviceWrite;
        goto Error;
      end;

    {need double width scaling?}
    if FlagIsSet(fcFlags, fcDoubleWidth) then
      DoubleWidth := not UseHighRes and
                     ((InfoHeader.BmpWidth * 2) <= (ResWidth - FaxLeftMargin));

    {center image in fax image}
    CenterOfs := 0;
    if FlagIsSet(fcFlags, fcCenterImage) then
      if (ResWidth - InfoHeader.BmpWidth) >= 16 then begin
        {only center if at least one byte on each side}
        if DoubleWidth then
          CenterOfs := (ResWidth - (InfoHeader.BmpWidth * 2)) div 32
        else
          CenterOfs := (ResWidth - InfoHeader.BmpWidth) div 16;
      end;

    LC           := 0;
    CurrLine     := 1;

    while (Offset <> FileHeader.ImageDataOfs) do begin
      bcReadRasterLine(TmpBuffer^);
      if (AsyncStatus <> ecOK) then
        goto Error;

      if (CenterOfs <> 0) then
        if ((CenterOfs+BmpByteWidth) < MaxData) then begin
          Move(TmpBuffer^[0], TmpBuffer^[CenterOfs], BmpByteWidth);
          FastZero(TmpBuffer^[0], CenterOfs);
        end;

      acCompressRasterLine(TmpBuffer^);

      if not acAddLine(DataLine^, ByteOfs) then begin
        AsyncStatus := ecDeviceWrite;
        goto Error;
      end;

      Inc(LC);
      if (LC >= UpdateInterval) then begin
        {update the user every UpdInterval lines}
        Inc(CurrLine);
        LC := 0;

        {show the user}
        if FaxConvertStatus(False, False) then begin
          AsyncStatus := ecUserAbort;
          Exit;
        end;
      end;
    end;

    {write the end-of-page marker}
    acMakeEndOfPage(TmpBuffer^, W);
    if not acAddLine(TmpBuffer^, W) then begin
      AsyncStatus := ecDeviceWrite;
      goto Error;
    end;

    {update page and file headers}
    Inc(MainHeader.PageCount);
    if (acUpdatePageHeader(MainHeader.PageCount, PageHeader) <> 0) then begin
      AsyncStatus := ecDeviceWrite;
      goto Error;
    end;
    if acUpdateMainHeader <> 0 then begin
      AsyncStatus := ecDeviceWrite;
      goto Error;
    end;

    {show the user}
    if FaxConvertStatus(False, False) then begin
      AsyncStatus := ecUserAbort;
      goto Error;
    end;

    {close output and input files}
    Close(OutFile);
    if (IoResult <> 0) then begin
      AsyncStatus := ecDeviceWrite;
      goto Error;
    end;
    AsyncStatus := ecOK;
    Close(InFile);
    if (IoResult = 0) then ;
    DoubleWidth := False;
    Exit;

  Error:
    {if we jumped here, some error occured}
    Close(OutFile); if (IoResult = 0) then ;
    Erase(OutFile); if (IoResult = 0) then ;
    Close(InFile) ; if (IoResult = 0) then ;
    if FaxConvertStatus(False, True) then ;
    DoubleWidth := False;
  end;

  function BmpFaxConverter.acOpenInputFile : Integer;
  var
    ErrorCode : Integer;

    function ValidBmp : Boolean;
      {-Dtermine whether a file is a valid bitmap}
    begin
      ValidBmp := (InfoHeader.BmpWidth <= ResWidth) and
                  (Infoheader.BitsPerPixel = 1) and
                  (FileHeader.FileType = $4D42);
    end;

  begin
    {open the bitmap file}
    Assign(InFile, InFileName);
    Reset(InFile, 1);
    ErrorCode := IoResult;
    if (ErrorCode <> ecOK) then begin
      acOpenInputFile := ErrorCode;
      Exit;
    end;

    {read headers}
    BlockRead(InFile, FileHeader, SizeOf(FileHeader));
    BlockRead(InFile, InfoHeader, SizeOf(InfoHeader));
    ErrorCode := IoResult;
    if (ErrorCode <> ecOK) then begin
      acOpenInputFile := ErrorCode;
      Close(InFile); if (IoResult = 0) then ;
      Exit;
    end;

    {validate BMP}
    if not ValidBmp then begin
      acOpenInputFile := ecBadGraphicsFormat;
      Close(InFile); if (IoResult = 0) then ;
      Exit;
    end;

    BmpByteWidth := InfoHeader.BmpWidth div 8;
    BytesPerLine := (BmpByteWidth + 3) and $FFFC;
    Offset       := FileSize(InFile) - BytesPerLine;

    acOpenInputFile := ecOK;
  end;

  procedure NotBuffer(var Buf; Len : Word); assembler;
  asm
    push  ds
    lds   si,Buf                {DS:SI->Buf}
    xor   ax,ax                 {AX=0}
    mov   cx,Len                {CX = Length of buffer}
    shr   cx,1                  {CX = CX / 2}
    adc   ax,ax                 {AX = remainder from division}
    jcxz  @2                    {Jump if there are no words to NOT}
@1: not   word ptr [si]         {NOT next word of buffer}
    inc   si                    {SI = SI + 2}
    inc   si
    loop  @1

@2: or    ax,ax                 {jump if no more data}
    jz    @3
    not   byte ptr [si]         {NOT last byte of buffer}

@3: pop   ds
  end;

  procedure BmpFaxConverter.bcReadRasterLine(var Buffer);
  begin
    FastZero(Buffer, MaxData);
    Seek(InFile, Offset);
    BlockRead(InFile, Buffer, BmpByteWidth);
    if (IoResult <> 0) then
      AsyncStatus := ecDeviceWrite
    else begin
      Dec(Offset, BytesPerLine);
      NotBuffer(Buffer, BytesPerLine);
    end;
  end;

{!!.03 - End changes}

{UnpackFax}

  constructor UnpackFax.Init;
  begin
    AsyncStatus := epFatal+ecOutOfMemory;
    if not Root.Init then
      Fail;

    LineBuffer := nil;
    FileBuffer := nil;
    WhiteTree := nil;
    BlackTree := nil;
    if not GetMemCheck(LineBuffer, LineBufSize) then begin
      Done;
      Fail;
    end;
    if not GetMemCheck(FileBuffer, MaxData) then begin
      Done;
      Fail;
    end;

    ufBuildTrees;
    if AsyncStatus <> ecOk then begin
      Done;
      Fail;
    end;

    @ufOutputLine := nil;
    AsyncStatus := ecOK;
    ufFlags := DefUnpackOptions;
  end;

  destructor UnpackFax.Done;
  begin
    ufDisposeTrees;
    FreeMemCheck(FileBuffer, MaxData);
    FreeMemCheck(LineBuffer, LineBufSize);
    Root.Done;
  end;

  procedure UnpackFax.ufOptionsOn(OptionFlags : Word);
    {-Activate multiple options}
  begin
    ufFlags := ufFlags or (OptionFlags and not BadUnpackOptions);
  end;

  procedure UnpackFax.ufOptionsOff(OptionFlags : Word);
    {-Deactivate multiple options}
  begin
    ufFlags := ufFlags and not (OptionFlags and not BadUnpackOptions);
  end;

  function UnpackFax.ufOptionsAreOn(OptionFlags : Word) : Boolean;
    {-Return True if all specified options are on}
  begin
    ufOptionsAreOn := (ufFlags and OptionFlags = OptionFlags);
  end;

  procedure UnpackFax.GetFaxHeader(FName : PathStr; var FH : FaxHeaderRec);
    {-Return header for fax FName}
  var
    F : File;
  label
    ExitPoint;
  begin
    AsyncStatus := ecOk;

    SaveMode := FileMode;                                              {!!.02}
    FileMode := $40;                                                   {!!.02}
    Assign(F, FName);
    Reset(F, 1);
    FileMode := SaveMode;                                              {!!.02}
    AsyncStatus := IoResult;
    if AsyncStatus <> 0 then
      Exit;

    {Read header}
    BlockRead(F, FH , SizeOf(FH));
    AsyncStatus := IOResult;
    if AsyncStatus <> 0 then
      goto ExitPoint;

    if not ValidSignature(FH) then
      AsyncStatus := ecFaxBadFormat;

ExitPoint:
    Close(F);
    if IoResult <> 0 then ;
  end;

  procedure UnpackFax.GetPageHeader(FName : PathStr; Page : Word;
                                   var PH : PageHeaderRec);
    {-Return header for Page in fax FName}
  var
    F : File;
    Status : Word;
    FH : FaxHeaderRec;
    Offset : LongInt;
    I : Word;
  label
    ExitPoint;
  begin
    AsyncStatus := ecOk;

    SaveMode := FileMode;                                              {!!.02}
    FileMode := $40;                                                   {!!.02}
    Assign(F, FName);
    Reset(F, 1);
    FileMode := SaveMode;                                              {!!.02}
    AsyncStatus := IOResult;
    if AsyncStatus <> 0 then
      Exit;

    {Read header}
    BlockRead(F, FH, SizeOf(FH));
    AsyncStatus := IOResult;
    if AsyncStatus <> 0 then
      goto ExitPoint;

    if not ValidSignature(FH) then begin
      AsyncStatus := ecFaxBadFormat;
      goto ExitPoint;
    end;

    if Page > FH.PageCount then begin
      AsyncStatus := epFatal+ecFaxBadFormat;
      goto ExitPoint;
    end;

    Offset := FH.PageOfs;
    for I := 1 to Page do begin
      Seek(F, Offset);
      AsyncStatus := IOResult;
      if AsyncStatus <> 0 then
        goto ExitPoint;
      BlockRead(F, PH, SizeOf(PH));
      Inc(Offset, SizeOf(PH)+PH.ImgLength);
    end;

ExitPoint:
    Close(F);
    if IoResult <> 0 then ;
  end;

  procedure UnpackFax.SetOutputLineFunc(OLF : OutputLineFunc);
  begin
    ufOutputLine := OLF;
  end;

  procedure UnpackFax.ufAddCode(T : TreePtr; Code : Word);
    {-Add a new code element to a TreeArray}
  begin
    Inc(TreeNext);
    if TreeNext > MaxTreeRec then
      {out of tree space}
      Exit;
    with T^[TreeLast] do
      if Code = 1 then
        Next1 := TreeNext
      else
        Next0 := TreeNext;
  end;

  procedure UnpackFax.ufSetCode(T : TreePtr; N0, N1 : Integer);
    {-Store terminating code}
  begin
    {terminating code}
    with T^[TreeNext] do begin
      Next0 := N0;
      Next1 := N1;
    end;
  end;

  function UnpackFax.ufCodeMatch(var TC : TermCodeArray; var MUC : MakeUpCodeArray) : Integer;
    {-Return run length of matching code, or -1 for no match}
  var
    I : Integer;
    TCode : Word;
  begin
    TCode := CurCode;
    RotateCode(TCode, CurSig);
    for I := 0 to MaxCodeTable do   {!!.03}
      with TC[I] do
        if (TCode = Code) and (CurSig = Sig) then begin
          ufCodeMatch := I;
          Exit;
        end;
    for I := 0 to MaxMUCodeTable do {!!.03}
      with MUC[I] do
        if (TCode = Code) and (CurSig = Sig) then begin
          ufCodeMatch := 64*(I+1);
          Exit;
        end;
    if (TCode = EOLRec.Code) and (CurSig = EOLRec.Sig) then
      ufCodeMatch := -2
    else
      ufCodeMatch := -1;
  end;

  procedure UnpackFax.ufBuildTree(T : TreePtr;
                                var TC : TermCodeArray;
                                var MUC : MakeUpCodeArray);
    {-Recursively build a tree containing all fax codes}
  var
    SaveLast : Integer;
  begin
    if CurSig < 13 then begin
      {add a 0 to the tree}
      ufAddCode(T, 0);
      inc(CurSig);
      CurCode := CurCode shl 1;
      Match := ufCodeMatch(TC, MUC);
      case Match of
        -2 : {end of line}
          ufSetCode(T, -2, 0);
        -1 : {no match}
          begin
            {use recursion to add next bit}
            SaveLast := TreeLast;
            TreeLast := TreeNext;
            ufBuildTree(T, TC, MUC);
            TreeLast := SaveLast;
          end;
        else
          ufSetCode(T, -1, Match);
      end;

      {add a 1 to the tree}
      ufAddCode(T, 1);
      CurCode := CurCode or 1;
      Match := ufCodeMatch(TC, MUC);
      case Match of
        -2 : {end of line}
          ufSetCode(T, -2, 0);
        -1 : {no match}
          begin
            {use recursion to add next bit}
            SaveLast := TreeLast;
            TreeLast := TreeNext;
            ufBuildTree(T, TC, MUC);
            TreeLast := SaveLast;
          end;
        else
          ufSetCode(T, -1, Match);
      end;
      CurCode := CurCode shr 1;
      Dec(CurSig);
    end;
  end;

  procedure UnpackFax.ufInitTree(var T : TreePtr);
    {-Initialize an empty tree}
  begin
    if GetMemCheck(T, SizeOf(TreeArray)) then begin
      FastZero(T^, SizeOf(TreeArray)); {!!.03}
      TreeLast := 0;
      TreeNext := 0;
      CurCode := 0;
      CurSig := 0;
      AsyncStatus := ecOk;
    end else
      AsyncStatus := ecOutOfMemory;
  end;

  procedure UnpackFax.ufBuildTrees;
    {-Build black and white decompression trees}
  begin
    ufInitTree(WhiteTree);
    if AsyncStatus <> ecOk then
      Exit;
    ufBuildTree(WhiteTree, WhiteTable, WhiteMUTable);
    {force a loop into the table for EOL resync}
    WhiteTree^[11].Next0 := 11;

    ufInitTree(BlackTree);
    if AsyncStatus <> ecOk then
      Exit;
    ufBuildTree(BlackTree, BlackTable, BlackMUTable);
    {force a loop into the table for EOL resync}
    BlackTree^[11].Next0 := 11;
  end;

  procedure UnpackFax.ufDisposeTrees;
    {-Dispose of black and white decompression trees}
  begin
    FreeMemCheck(WhiteTree, SizeOf(WhiteTree^));
    FreeMemCheck(BlackTree, SizeOf(BlackTree^));
  end;

  procedure UnpackFax.ufOutputRun(IsWhite : Boolean; Len : Integer);
    {-Output current run}
  var
    Mask : Byte;
    Bytes : Word;
  begin
    if HalfWidth then begin                                            {!!.01}
      if Odd(Len) then begin                                           {!!.01}
        Len := Len shr 1;                                              {!!.01}
        if LastOdd then                                                {!!.01}
          Inc(Len);                                                    {!!.01}
        LastOdd := not LastOdd;                                        {!!.01}
      end else                                                         {!!.01}
        Len := Len shr 1;                                              {!!.01}
    end;                                                               {!!.01}

    if Len > 0 then
      if IsWhite then begin
        {update line position; line already filled with zeros}
        asm
          les di,Self
          mov bx,es:[di].LineBit
          add bx,Len
          mov ax,bx
          shr ax,1
          shr ax,1
          shr ax,1
          add es:[di].LineOfs,ax
          and bx,7
          mov es:[di].LineBit,bx
        end;
      end else begin
        asm
          mov cx,Len
          les di,Self
          mov bx,es:[di].LineBit
          mov ax,es:[di].LineOfs
          les di,es:[di].LineBuffer
          add di,ax
          cmp bx,0
          jz  @3
          mov dx,cx
          mov al,$80
          mov cl,bl
          shr al,cl
          mov cx,dx
  @1:     or es:[di],al
          inc bx
          shr al,1
          dec cx
          cmp bx,8
          jae @2
          jcxz @2
          jmp @1
  @2:     mov ax,bx
          shr ax,1
          shr ax,1
          shr ax,1
          add di,ax
          and bx,7
  @3:     cmp cx,8
          jb @4
          mov dx,cx
          shr cx,1
          shr cx,1
          shr cx,1
          cld
          mov al,$FF
          rep stosb
          and dx,7
          mov cx,dx
  @4:     jcxz @6
          mov dx,cx
          mov al,$80
          mov cl,bl
          shr al,cl
          mov cx,dx
  @5:     or es:[di],al
          inc bx
          shr al,1
          loop @5
  @6:     mov ax,di
          les di,Self
          sub ax,word ptr es:[di].LineBuffer
          mov es:[di].LineOfs,ax
          mov es:[di].LineBit,bx
        end;
      end;
  end;

  {!!.01 new}
  function UnpackFax.ufValidLineLength(Len : Word) : Boolean;
    {-Return True if Len is a valid line length}
  begin
    if HalfWidth then
      ufValidLineLength := (Len = (StandardWidth div 8 div 2)) or   {!!.03}
                           (Len = (WideWidth     div 8 div 2))      {!!.03}
    else
      ufValidLineLength := (Len = (StandardWidth div 8)) or         {!!.03}
                           (Len = (WideWidth     div 8));           {!!.03}
  end;

  function UnpackFax.OutputLine : Boolean;
    {-Output end of line}
  begin
    AsyncStatus := 0;
    if @ufOutputLine <> nil then
      OutputLine := ufOutputLine(@Self, LineBuffer, LineOfs, PageHeader)
    else
      RunError(211);
  end;

  procedure UnpackFax.UnpackPage(FName : PathStr; Page : Word);
    {-decompress FAX image in file}
  label
    ExitPoint;
  var
    CheckZero : Boolean;
    WaitEOL : Boolean;
    CurByte : Byte;
    CurMask : Byte;
    CurOfs : Word;
    ActRead : Word;
    LineCnt  : Word; {count of adjacent empty lines}
    TreeIndex : Integer;
    RunLen : Integer;
    CurTree : TreePtr;
    IsWhite : Boolean;
    F : File;
    X : Word;
    M : LongInt;
    TotRead : LongInt;
    UseLengthWords : Boolean;
    Result : Word;

    function ReadNextLine : Word;
      {-Read the next wordlength raster line}
    var
      Len : Word;

    begin
      BlockRead(F, Len, 2, ActRead);
      if (IoResult <> 0) or (ActRead <> 2) then begin                  {!!.03}
        ReadNextLine := ecDiskRead;
        Exit;
      end;
      if Len > MaxData then
        Len := MaxData;
      BlockRead(F, FileBuffer^, Len, ActRead);
      if (IoResult = 0) and (ActRead = Len) then                       {!!.03}
        ReadNextLine := 0
      else
        ReadNextLine := ecDiskRead;
    end;

  begin
    BadCodes := 0;
    SaveMode := FileMode;                                              {!!.02}
    FileMode := $40;                                                   {!!.02}
    Assign(F, FName);
    Reset(F, 1);
    FileMode := SaveMode;                                              {!!.02}
    AsyncStatus := IoResult;
    if AsyncStatus <> 0 then
      Exit;

    CurrPage := Page;

    {Seek past header}
    BlockRead(F, FaxHeader, SizeOf(FaxHeader));
    AsyncStatus := IoResult;
    if AsyncStatus <> 0 then
      goto ExitPoint;

    if not ValidSignature(FaxHeader) then begin
      AsyncStatus := ecFaxBadFormat;
      goto ExitPoint;
    end;

    if Page > FaxHeader.PageCount then begin
      AsyncStatus := epFatal+ecFaxBadFormat;
      goto ExitPoint;
    end;

    Seek(F, FaxHeader.PageOfs);
    AsyncStatus := IoResult;
    if AsyncStatus <> 0 then
      goto ExitPoint;
    BlockRead(F, PageHeader, SizeOf(PageHeader));
    UseLengthWords := FlagIsSet(PageHeader.ImgFlags, ffLengthWords);

    AsyncStatus := IOResult;
    if AsyncStatus <> 0 then
      goto ExitPoint;
    M := FilePos(F);

    if Page > 1 then
      for X := 1 to Page-1 do begin
        Inc(M, PageHeader.ImgLength);
        Seek(F, M);
        AsyncStatus := IOResult;
        if AsyncStatus <> 0 then
          goto ExitPoint;
        BlockRead(F, PageHeader, SizeOf(PageHeader));
        UseLengthWords := FlagIsSet(PageHeader.ImgFlags, ffLengthWords);
        AsyncStatus := IOResult;
        if AsyncStatus <> 0 then
          goto ExitPoint;
        M := FilePos(F);
      end;

    FastZero(LineBuffer^, LineBufSize); {!!.03}
    IsWhite := True;
    CurTree := WhiteTree;
    TreeIndex := 0;
    LineOfs := 0;
    LineBit := 0;
    LineCnt := 0;
    CurOfs := 0;
    ActRead := 0;
    TotRead := 0;
    HalfWidth := FlagIsSet(ufFlags, ufHalfWidth);

    CheckZero := False;
    WaitEOL := True;
    repeat
      if (LineCnt >= 3) then
        {More than 5 blank EOL's means end of fax}
        goto ExitPoint;

      if CurOfs >= ActRead then begin
        if UseLengthWords then begin
          Result := ReadNextLine;
          if Result <> 0 then begin
            AsyncStatus := Result;
            goto ExitPoint;
          end;
        end else begin
          {have we read the whole image?}
          if (TotRead >= PageHeader.ImgLength) and (TotRead > 0) then
            goto ExitPoint;

          {Get a FileBuffer from the file}
          BlockRead(F, FileBuffer^, MaxData, ActRead);
          AsyncStatus := IoResult;
          if AsyncStatus <> 0 then
            goto ExitPoint;

          if ActRead = 0 then begin
            {End of file without fax terminator}
            AsyncStatus := ecFaxBadFormat;
            goto ExitPoint;
          end;
        end;

        Inc(TotRead, ActRead);
        CurOfs := 0;
      end;
      CurByte := FileBuffer^[CurOfs];
      Inc(CurOfs);

      CurMask := $01;
      while CurMask <> 0 do begin
        if (CurByte and CurMask <> 0) then begin
          if not CheckZero then
            TreeIndex := CurTree^[TreeIndex].Next1;
        end else begin
          CheckZero := False;
          TreeIndex := CurTree^[TreeIndex].Next0;
        end;
        case CurTree^[TreeIndex].Next0 of
          -1 : {complete code}
            begin
              if not WaitEOL then begin
                RunLen := CurTree^[TreeIndex].Next1;
                if (LineOfs+(RunLen shr 3)) < LineBufSize then begin
                  ufOutputRun(IsWhite, RunLen);
                  if RunLen < 64 then begin
                    IsWhite := not IsWhite;
                    if IsWhite then
                      CurTree := WhiteTree
                    else
                      CurTree := BlackTree;
                  end;
                end;
              end;
              TreeIndex := 0;
              LineCnt := 0;
            end;
          -2 : {end of line}
            begin
              {ignore blank line if first line or a terminator}
              if (LineOfs > 0) or (LineBit > 0) then begin
                if ufValidLineLength(LineOfs) then begin               {!!.01}
                  if OutputLine then begin
                    AsyncStatus := ecUserAbort;
                    goto ExitPoint;
                  end;
                  LastOdd := False;                                    {!!.01}
                end;
                FastZero(LineBuffer^, LineOfs);                        {!!.03}
                LineOfs := 0;
                LineBit := 0;
                IsWhite := True;
                CurTree := WhiteTree;
              end;
              TreeIndex := 0;
              Inc(LineCnt);
              WaitEOL := False;
            end;
          +0 : {invalid code, ignore and start over}
            begin
              TreeIndex := 0;
              Inc(BadCodes);
              CheckZero := True;
            end;
        end;
        CurMask := CurMask shl 1;
      end;
    until False;

ExitPoint:
    Close(F);
    if IOResult = 0 then ;
  end;

  procedure UnpackFax.UnpackFile(FName : PathStr);
    {-Decompress FAX image in file}
  var
    I : Word;
    F : File;
    Pages : Word;
  begin
    SaveMode := FileMode;                                              {!!.02}
    FileMode := $40;                                                   {!!.02}
    Assign(F, FName);
    Reset(F, 1);
    FileMode := SaveMode;                                              {!!.02}
    AsyncStatus := IOResult;
    if AsyncStatus <> 0 then
      Exit;

    {Read header}
    BlockRead(F, FaxHeader, SizeOf(FaxHeader));
    AsyncStatus := IOResult;
    if AsyncStatus <> 0 then begin
      Close(F);
      if IoResult <> 0 then ;
      Exit ;
    end;

    {Get page count from fax header}
    Pages := FaxHeader.PageCount;

    {Close the file, UnpackPage will re-open it}
    Close(F);
    if IoResult <> 0 then ;

    {Unpack all pages}
    for I := 1 to Pages do begin
      UnpackPage(FName, I);
      if AsyncStatus <> 0 then
        Exit;
    end;
  end;

  {$IFDEF UseStreams}
  constructor UnpackFax.Load(var S : IdStream);
    {-Abstract Load}
  begin
    LineBuffer := nil;
    FileBuffer := nil;
    WhiteTree := nil;
    BlackTree := nil;
    if not GetMemCheck(LineBuffer, LineBufSize) then begin
      Done;
      Fail;
    end;
    if not GetMemCheck(FileBuffer, MaxData) then begin
      Done;
      Fail;
    end;

    ufBuildTrees;
    if AsyncStatus <> ecOk then begin
      Done;
      Fail;
    end;

    {Load stream data}
    S.Read(ufFlags, SizeOf(ufFlags));
    @ufOutputLine := S.ReadUserPointer(Nil);
    if S.PeekStatus <> 0 then begin
      Done;
      Fail;
    end;
  end;

  procedure UnpackFax.Store(var S : IdStream);
    {-Abstract Store}
  begin
    S.Write(ufFlags, SizeOf(ufFlags));
    S.WriteUserPointer(@ufOutputLine, ptNil);
  end;
  {$ENDIF}

{UnpackToPcx/Dcx}

  procedure AbstractPcxUnpack.upWriteHeader;
    {-Write the PCX header}
  const
    BitLen  : array[Boolean] of Word =                      {!!.03}
      (StandardWidth - 1, (StandardWidth div 2) - 1);       {!!.03}
    ByteLen : array[Boolean] of Word =                      {!!.03}
      (StandardWidth, StandardWidth div 2);                 {!!.03}
  var
    PH : PcxHeaderRec;
    HalveIt : Boolean;
  begin
    HalveIt := FlagIsSet(ufFlags, ufHalfWidth);
    FastZero(PH, SizeOf(PH));                               {!!.03}
    with PH do begin
      Mfgr       := $0A;               {required}
      Ver        := $02;               {CAS requires this version}
      Encoding   := 1;                 {default RLE compression}
      BitsPixel  := 1;                 {1 bit/pixel}
      XMax := BitLen[HalveIt];         {1727 or 864 pixels/line}
      YMax       := 0;                 {to be filled in later...}
      Planes     := 1;                 {single plane}
      BytesLine  := ByteLen[HalveIt];  {216 or 108 bytes/line}
      PalType    := 0;                 {no palette type}
    end;
    BlockWrite(PcxF, PH, SizeOf(PH));
    AsyncStatus := IoResult;
  end;

  procedure AbstractPcxUnpack.upFixLineCount;
    {-go back and fix header YMax value}
  begin
    Seek(PcxF, PcxOfs + YMaxOfs);
    Dec(Lines, 2);
    if IOResult = 0 then
      BlockWrite(PcxF, Lines, 2);
    if IOResult = 0 then ;
  end;

  procedure AbstractPcxUnpack.upFlushBuff(Force : Boolean);
  begin
    if (Force) or (PBOfs > 500) then begin
      BlockWrite(PcxF, PackBuffer, PBOfs);
      AsyncStatus := IoResult;
      PBOfs := 0;
    end;
  end;

  function AbstractPcxUnpack.OutputLine : Boolean;
  var
    I, T : Integer;
  begin
    AsyncStatus := ecOk;
    Inc(Lines);
    T := 0;
    while (T < LineOfs) and (AsyncStatus = ecOk) do begin
      I := 0;
      while (LineBuffer^[T+I] = LineBuffer^[T+I+1]) and (T+I < LineOfs) and (I < 63) do
        Inc(I);
      if I > 0 then begin
        PackBuffer[PBOfs] := Byte(I) or $C0;
        PackBuffer[PBOfs+1] := not(LineBuffer^[T]);
        Inc(PBOfs, 2);
        upFlushBuff(False);
        Inc(T, I);
      end else begin
        if (not(LineBuffer^[T]) and $C0 = $C0) then begin
          PackBuffer[PBOfs] := $C1;
          Inc(PBOfs);
        end;
        PackBuffer[PBOfs] := not(LineBuffer^[T]);
        Inc(PBOfs);
        upFlushBuff(False);
        Inc(T);
      end;
    end;

    {Call user procedure pointer so user can show status or abort}
    if @ufOutputLine <> nil then
       OutputLine := ufOutputLine(@Self, LineBuffer, LineOfs, PageHeader)
    else
      Outputline := False;
  end;

  procedure UnpackToPcx.UnpackFileToPcx(Fax, Pcx: PathStr);
    {-Unpack fax file FaxName to pcx file PcxName}
  begin
    AsyncStatus := ecOk;

    {Open the output pcx file}
    PcxName := Pcx;
    Assign(PcxF, PcxName);
    Rewrite(PcxF, 1);
    AsyncStatus := IoResult;
    if AsyncStatus <> 0 then
      Exit;

    PcxOfs := 0;

    {Write initial pcx header}
    upWriteHeader;
    if AsyncStatus <> ecOk then begin
      Close(PcxF);
      if IoResult <> 0 then ;
      Exit;
    end;

    {Start unpacking}
    Lines := 0;
    PBOfs := 0;
    UnpackFile(Fax);
    if AsyncStatus <> ecOk then begin
      Close(PcxF);
      if IoResult <> 0 then ;
      Exit;
    end;

    {Adjust the YMax header value}
    upFlushBuff(True);
    upFixLineCount;

    {Close up}
    Close(PcxF);
    AsyncStatus := IoResult;
  end;

  procedure UnpackToDcx.UnpackFileToDcx(Fax, Dcx : PathStr);
  label
    Error;

  var
    Header : DcxHeaderRec;
    OnPage : Word;

  begin

    AsyncStatus := ecOk;

    { open the output DCX file }
    PcxName := Dcx;
    Assign(PcxF, PcxName);
    Rewrite(PcxF, 1);
    AsyncStatus := IoResult;
    if (AsyncStatus <> ecOK) then
      Exit;

    { write a blank header }
    FastZero(Header, SizeOf(Header)); {!!.03}
    Header.ID := 987654321;
    BlockWrite(PcxF, Header, SizeOf(Header));
    AsyncStatus := IoResult;
    if (AsyncStatus <> ecOK) then
      goto Error;

    GetFaxHeader(Fax, FaxHeader);
    if (AsyncStatus <> ecOK) then
      goto Error;

    for OnPage := 1 to FaxHeader.PageCount do begin

      { position at the spot for the next pcx file }
      PcxOfs := FileSize(PcxF);
      Header.Offsets[OnPage] := PcxOfs;
      Seek(PcxF, PcxOfs);

      upWriteHeader;
      if (AsyncStatus <> ecOK) then
        goto Error;

      Lines := 0;
      PBOfs := 0;

      UnpackPage(Fax, OnPage);
      if (AsyncStatus <> ecOK) then
        goto Error;

      upFlushBuff(True);
      upFixLineCount;

    end;

    { fix the DCX file header }
    Seek(PcxF, 0);
    BlockWrite(PcxF, Header, SizeOf(Header));
    AsyncStatus := IoResult;
    if (AsyncStatus <> ecOK) then
      goto Error;

    Close(PcxF);
    Exit;

Error:

    Close(PcxF);
    if (IoResult = 0) then ;
    Erase(PcxF);
    if (IoResult = 0) then ;
  end;

{Stream registration procs}

  {$IFDEF UseStreams}
  procedure AbstractFaxConverterStream(SPtr : IdStreamPtr);
    {-Register all types needed for abstract fax converter}
  begin
    SPtr^.RegisterType(otAbstractFaxConverter, veAbstractFaxConverter,
                       TypeOf(AbstractFaxConverter),
                       @AbstractFaxConverter.Store, @AbstractFaxConverter.Load);
  end;

  procedure TextFaxConverterStream(SPtr : IdStreamPtr);
    {-Register all types needed for text fax converter}
  begin
    AbstractFaxConverterStream(SPtr);
    SPtr^.RegisterType(otTextFaxConverter, veTextFaxConverter,
                       TypeOf(TextFaxConverter),
                       @TextFaxConverter.Store, @TextFaxConverter.Load);
  end;

  procedure PcxFaxConverterStream(SPtr : IdStreamPtr);
    {-Register all types needed for pcx fax converter}
  begin
    AbstractFaxConverterStream(SPtr);
    SPtr^.RegisterType(otPcxFaxConverter, vePcxFaxConverter,
                       TypeOf(PcxFaxConverter),
                       @PcxFaxConverter.Store, @PcxFaxConverter.Load);
  end;

  procedure DcxFaxConverterStream(SPtr : IdStreamPtr);
    {-Register all types needed for dcx fax converter}
  begin
    AbstractFaxConverterStream(SPtr);
    SPtr^.RegisterType(otDcxFaxConverter, veDcxFaxConverter,
                       TypeOf(DcxFaxConverter),
                       @DcxFaxConverter.Store, @DcxFaxConverter.Load);
  end;

  procedure TiffFaxConverterStream(SPtr : IdStreamPtr);
    {-Register all types needed for tiff fax converter}
  begin
    AbstractFaxConverterStream(SPtr);
    SPtr^.RegisterType(otTiffFaxConverter, veTiffFaxConverter,
                       TypeOf(TiffFaxConverter),
                       @TiffFaxConverter.Store, @TiffFaxConverter.Load);
  end;

  procedure UnpackFaxStream(SPtr : IdStreamPtr);
    {-Register all types needed for unpacker}
  begin
    SPtr^.RegisterType(otUnpackFax, veUnpackFax,
                       TypeOf(UnpackFax),
                       @UnpackFax.Store, @UnpackFax.Load);
  end;

  procedure UnpackToPcxStream(SPtr : IdStreamPtr);
    {-Register all types needed for unpacker to pcx}
  begin
    SPtr^.RegisterType(otUnpackToPcx, veUnpackToPcx,
                       TypeOf(UnpackToPcx),
                       @UnpackToPcx.Store, @UnpackToPcx.Load);
  end;

  procedure UnpackToDcxStream(SPtr : IdStreamPtr);
    {-Register all types needed for unpacker to dcx}
  begin
    SPtr^.RegisterType(otUnpackToDcx, veUnpackToDcx,
                       TypeOf(UnpackToDcx),
                       @UnpackToDcx.Store, @UnpackToDcx.Load);
  end;
  {$ENDIF}

begin
  RotateCodes;
end.
