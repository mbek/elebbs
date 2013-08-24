
(******************************************************************************
OOFAXPRN.PAS - This unit provides fax printing support for APRO format
fax files using either Epson or HP compatible printers. There are basically
two objects that you need to create: A decendent of AbstractPrinter and a
FaxPrinter object. The basic steps to print a fax are shown below:

 var
   Printer : HP300PrinterPtr;  {HP printer using 300 dpi mode}
   FaxPrn  : FaxPrinterPtr;    {Fax printing object}
  ...
  {Init objects}
  New(Printer, Init);
  New(FaxPrn, Init(Printer));

  {Set options}
  FaxPrn^.SetPcl5Mode(True);           {Uses PCL5 commands to compress}
  FaxPrn^.SetScaling(True);                         {Scale page output}
  FaxPrn^.SetHeader(True);                {Print a header on each page}
  FaxPrn^.SetBlankLineSuppression(True);            {Strip blank areas}

  {Print the fax}
  FaxPrn^.PrintFax('LPT1', 'FAX.APF');

  {Dispose of objects}
  Dispose(FaxPrn, Done);
  Dispose(Printer, Done);

******************************************************************************)

{$R-,S-,A+,F+,I-,V-,B-,O+}

{******************************************************}
{*                 OOFAXPRN.PAS 2.03                  *}
{*      Copyright (c) TurboPower Software 1993.       *}
{*                All rights reserved.                *}
{******************************************************}

{$I APDEFINE.INC}

unit OoFaxPrn;
  {-Print a fax file}

interface

uses
  Dos,
  {$IFDEF UseOpro}
  OpRoot,
  OpString,
  {$ENDIF}
  {$IFDEF UseTpro}
  TpMemChk,
  TpString,
  {$ENDIF}
  ApMisc,
  OoFaxCvt;

const
  {Fax printing options}
  fpScale               = $0001;  {Scale page output}
  fpHeader              = $0002;  {Print a page header}
  fpPcl5Mode            = $0004;  {Use PCL5 commands (HP only)}
  fpSuppressBlanks      = $0008;  {Suppress printing of large blank areas}

  {Setup default options}
  DefPrinterOptions     : Word = fpScale;  {Scale page by default}

  {Setup bad options}
  BadPrinterOptions     : Word = 0;

  BLSSize               : Word = 60;                                   {!!.01}
                            {Blank line suppression size, 60 = 2/10ths inch}
                            {Shrink blank areas to this minimum height}

type
  {.Z+}{Private types}
  {Types for print and pack buffer allocations}
  ByteArray             = array[0..$FFF0] of Byte;
  PByteArray            = ^ByteArray;
  {.Z-}

  {User hook function types - return True to abort}
  LineNotifyType        = function(Lines, LineSize : Word) : Boolean;
  WriteNotifyType       = function(BytesWritten : Word) : Boolean;
  PageNotifyType        = function(ThisPage, TotalPages : Word) : Boolean;

  {Abstract printer object}
  AbstractPrinterPtr    = ^AbstractPrinter;
  AbstractPrinter       = object(Root)
    {.Z+}{Internal}
    PrnF                : file;        {Printer file/device}
    PrnDest             : PathStr;     {Print destination}
    PrintBufCount       : Word;        {Characters in the print buffer}
    PrintedLineCount    : Word;        {Number of raster lines printed}
    MaxLinesPerPage     : Word;        {Max raster lines per page}
    CurrentPage         : Word;        {Page that is printing}
    TotalPages          : Word;        {Number of pages for this document}
    RasterCount         : Word;        {Number of raster lines unpacked}
    FaxName             : PathStr;     {Name of file being printed}
    MinBlankSpace       : Word;        {For blank line compression}
    fpOptions           : Word;        {Print options}

    {Pack and print buffers - dynamically allocated}
    PackBuf             : PByteArray;  {Pack buffer}
    PrintBuf            : PByteArray;  {Print buffer}

    {Notification hooks}
    LineNotify          : LineNotifyType;
    WriteNotify         : WriteNotifyType;
    PageNotify          : PageNotifyType;
    {.Z-}

    {Constructors/destructors}
    constructor Init;
      {-Allocate and initialize the AbstractPrinter object}
    destructor Done; virtual;
      {-Dispose of the AbstractPrinter object}

    {Print routines}
    procedure PrintOpen; virtual;
      {-Open and prepare print device}
    procedure PrintClose; virtual;
      {-Close print device}
    procedure StartNewPage; virtual;
      {-Force a new page}
    procedure FlushPrintBuf; virtual;
      {-Clear the print buffer}
    procedure PrintData(var Data; Size : Word); virtual;
      {-Send data to the printer}
    procedure PrintStr(S : string); virtual;
      {-Print a string}
    function PrintRasterLine(Buffer : pByteBuffer;
                             InputLen : Word;
                             PH : PageHeaderRec) : Boolean; virtual;
      {-Method to output a line must be overridden}
    function MakeHeader : string; virtual;
      {-Returns a page header}

    {User control}
    procedure SetLineNotifyFunc(LNF : LineNotifyType);
      {-Set the procedure that is called for each raster line processed}
    procedure SetWriteNotifyFunc(WNF : WriteNotifyType);
      {-Set the procedure that is called for each data write}
    procedure SetPageNotifyFunc(PNF : PageNotifyType);
      {-Set the procedure that is called before each fax page is printed}
    procedure SetPrnDest(Dest : PathStr);
      {-Set the current print destination file/device name}
    procedure SetPageNum(Page : Word);
      {-Set the current page number}
    procedure SetPageCount(Pages : Word);
      {-Ser the expected total page count}
    procedure SetFaxName(FName : PathStr);
      {-Set the name of the fax file}

    {Option management}
    procedure fpOptionsOn(OptionFlags : Word);
      {-Activate multiple options}
    procedure fpOptionsOff(OptionFlags : Word);
      {-Deactivate multiple options}
    function fpOptionsAreOn(OptionFlags : Word) : Boolean;
      {-Return True if all specified options are on}

    {Information methods}
    function GetPrnDest : PathStr;
      {-Returns the current print destination file/device name}
    function GetPrnBufCount : Word;
      {-Returns the number of characters in the print buffer}
    function GetLineCount : Word;
      {-Returns the number of lines printed on this page}
    function GetMaxLinesPerPage : Word;
      {-Returns the maximum raster lines per page}
    function GetRasterCount : Word;
      {-Returns the number of raster lines unpacked}
    function GetPageNum : Word;
      {-Returns the current page number}
    function GetPageCount : Word;
      {-Returns the expected total page count}
    function GetFaxName : PathStr;
      {-Returns the name of the fax file being printed}

    {.Z+}{Private}
    {Functions to call user hooks}
    function CallWriteNotify(BytesWritten : Word) : Boolean;
      {-Call the write notify hook - return true to abort}
    function CallLineNotify(Lines, LineSize : Word) : Boolean;
      {-Call the line notify hook - return true to abort}
    function CallPageNotify(ThisPage, PageCount : Word) : Boolean;
      {-Call the page notify hook - return true to abort}
    {.Z-}
  end;

  {EpsonPrinter}
  EpsonPrinterPtr       = ^EpsonPrinter;
  EpsonPrinter          = object(AbstractPrinter)
    {.Z+}{Internal}
    RasterCountDown     : Byte;        {For Epson, counts 7..0}
    CombineCount        : Byte;        {For Epson, counts rasters to combine}
    {.Z-}

    {Constructors/destructors}
    constructor Init;
      {-Allocate and initialize the EpsonPrinter object}
    destructor Done; virtual;
      {-Dispose of the EpsonPrinter object}

    {Print routines}
    procedure PrintOpen; virtual;
      {-Open and prepare print device}
    procedure PrintClose; virtual;
      {-Close print device}
    procedure StartNewPage; virtual;
      {-Force a new page}
    procedure PrintGraphics(var Buffer; ByteLen : Word; AdvanceLen : Byte);
      {-Print 8 raster lines of Epson graphics and advance AdvanceLen/216"}
    function PrintRasterLine(Buffer : PByteBuffer;
                             InputLen : Word;
                             PH : PageHeaderRec) : Boolean; virtual;
      {-Method to output a line}
  end;

  {Epson24PinPrinter}
  Epson24PinPrinterPtr       = ^Epson24PinPrinter;
  Epson24PinPrinter          = object(AbstractPrinter)
    {.Z+}{Internal}
    RasterCountDown     : Byte;        {For Epson, counts 7..0}
    ScaleBuf            : PByteArray;  {Scaling buffer}
    {.Z-}

    {Constructors/destructors}
    constructor Init;
      {-Allocate and initialize the Epson24PinPrinter object}
    destructor Done; virtual;
      {-Dispose of the Epson24PinPrinter object}

    {Print routines}
    procedure PrintOpen; virtual;
      {-Open and prepare print device}
    procedure PrintClose; virtual;
      {-Close print device}
    procedure StartNewPage; virtual;
      {-Force a new page}
    procedure PrintGraphics(var Buffer; ByteLen : Word; AdvanceLen : Byte);
      {-Print 24 raster lines of Epson graphics and advance AdvanceLen/180"}
    function PrintRasterLine(Buffer : PByteBuffer;
                             InputLen : Word;
                             PH : PageHeaderRec) : Boolean; virtual;
      {-Method to output a line}
  end;

  {HP150Printer 150x150 resolution}
  HP150PrinterPtr       = ^HP150Printer;
  HP150Printer          = object(AbstractPrinter)
    {.Z+}{Internal}
    {Scale buffer - dynamically allocated}
    ScaleBuf            : PByteArray;  {Scaling buffer}
    BlankLinesToPrint   : Word;        {Pending blank lines to send}
    {.Z-}

    {Constructors/destructors}
    constructor Init;
      {-Allocate and initialize the HP150Printer object}
    destructor Done; virtual;
      {-Dispose of the HP150Printer object}

    {Print routines}
    procedure PrintOpen; virtual;
      {-Open and prepare print device}
    procedure PrintClose; virtual;
      {-Close print device}
    procedure StartNewPage; virtual;
      {-Force a new page}
    procedure PrintData(var Data; Size : Word); virtual;
      {-Send data to the printer}
    function PrintRasterLine(Buffer : pByteBuffer;
                             InputLen : Word;
                             PH : PageHeaderRec) : Boolean; virtual;
      {-Method to output a line}
  end;

  {HP300Printer  300x300 resolution}
  HP300PrinterPtr       = ^HP300Printer;
  HP300Printer          = object(HP150Printer)
    {Constructors/destructors}
    constructor Init;
      {-Allocate and initialize the HP300Printer object}
    destructor Done; virtual;
      {-Dispose of the HP300Printer object}

    {Print routines}
    procedure PrintOpen; virtual;
      {-Open and prepare print device}
    procedure PrintClose; virtual;
      {-Close print device}
    procedure StartNewPage; virtual;
      {-Force a new page}
    function PrintRasterLine(Buffer : pByteBuffer;
                             InputLen : Word;
                             PH : PageHeaderRec) : Boolean; virtual;
      {-Method to output a line}
  end;

  {Fax printer object}
  FaxPrinterPtr         = ^FaxPrinter;
  FaxPrinter            = object(UnpackFax)
    {.Z+}{Internal}
    fpPrinter           : AbstractPrinterPtr;
    {.Z-}

    {Constructors/destructors}
    constructor Init(APrinter : AbstractPrinterPtr);
    destructor Done; virtual;

    {Print routines}
    function OutputLine : Boolean; virtual;
      {-Method to output a line -- called by unpack fax ancestor}
    procedure PrintFax(ADest : PathStr; AFaxName : PathStr);
      {-Prints AFaxFile to ADest}

    {User control}
    procedure SetLineNotifyFunc(LNF : LineNotifyType);
      {-Set the procedure that is called for each raster line processed}
    procedure SetWriteNotifyFunc(WNF : WriteNotifyType);
      {-Set the procedure that is called for each data write}
    procedure SetPageNotifyFunc(PNF : PageNotifyType);
      {-Set the procedure that is called before each fax page is printed}

    procedure SetScaling(On : Boolean);
      {-Enable/disable scaling - default is on}
    procedure SetHeader(On : Boolean);
      {-Enable/disable page headers - default is off}
    procedure SetPCL5Mode(On : Boolean);
      {-Enable/disable use of PCL5 commands - default is off}
    procedure SetBlankLineSuppression(On : Boolean);
      {-Enable/disable blank line suppression - default is off}
  end;


implementation


const
  PackBufSize      = 9216;                     {Pack buffer size}
  {Note: worst case PackBufSize is for Epson 24-pin scaled output.
   For this case, PackBufSize must be at least
     2048*(3/2)*3 = 9216 bytes
   2048 is the maximum number of pixels in the fax raster line.
   3/2 is the horizontal scaling ratio.
   3 is the number of bytes in the pack buffer for each pixel column.}

  PrintBufSize     = 2048;                     {Print buffer size}
  ScaleBufSize     = 2048;                     {Scaling buffer size}

  {PCL printer command strings}
  PclInit          : string[2] = #27'E';       {Init command}
  Pcl300DPI        : string[7] = #27'*t300R';  {Enable 300 dpi mode}
  Pcl150DPI        : string[7] = #27'*t150R';  {Enable 150 dpi mode}
  PclStartGraph    : string[5] = #27'*r0A';    {Start raster graphics}
  PclEndGraph      : string[4] = #27'*rB';     {End raster graphics}
  PclXferS         : string[3] = #27'*b';      {Start data block command}
  PclXferE         : string[1] = 'W';          {End data block command}
  PclTiffMode      : string[5] = #27'*b2M';    {Enable TIFF compression}
  PclDeltaMode     : string[5] = #27'*b3M';    {Enable delta compression}
  PclRepLine       : string[5] = #27'*b0W';    {Repeat previous line}
  PclBlankS        : string[3]  = #27'*b';     {Start blank line command}
  PclBlankE        : string[1]  = 'Y';         {End blank line command}
  PclSetMargin     : string[5] = #27'&l1E';    {Set margin to first raster}

  {TIFF encoded command strings to draw a horizontal line}
  PclLine300       : string[11] = #27'*b6W'#129#255#129#255#213#255;
  PclLine150       : string[9]  = #27'*b4W'#129#255#235#255;

  LinesPerPageLimit= 3135;  {Maximum raster lines per page at 300 dpi}

  Pcl4LineAdj      = 100;   {Heigth of about two text lines for adjusting}
                            {the lines per page for older PCL printers}

  AfterTextSpace   = 14;    {Down 14 raster lines after header}
  ProbationArea    = 60;    {Raster lines above the page bottom to start}
                            {looking for a good place for a page break}

  {Epson printer command strings}
  EpsInit          : string[5]  = #27'@';  {Reset}
  EpsShutdown      : string[3]  = ^L;      {Form feed}

  {Never more than 1920 bytes = 1920 pixels wide = 8.00"}
  EpsMaxWidth      = 1920;


  {---------- General-purpose routines ----------}

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

  procedure FastMove(var Src; var Dest; Size : Word); assembler;
    {-Move that assumes no Src and Dest overlap}
  asm
    push ds         { save ds               }
    mov  cx,Size    { cx = bytes to move    }
    lds  si,Src     { ds:si = @Src          }
    les  di,Dest    { es:di = @Dest         }
    cld             { forward               }
    shr  cx,1       { adjust for words      }
    rep  movsw      { copy cx words         }
    adc  cx,cx      { get odd byte (cf)     }
    rep  movsb      { move last byte if any }
    pop  ds         { restore ds            }
  end;

  function NonZeroLen(var Src; Len : Word) : Word; assembler;
    {-Return number of leading non-zero bytes in Src}
  asm
    mov   cx,Len
    jcxz  @2           {get out for zero length}
    les   di,Src       {ES:DI = buffer}
    add   di,cx
    dec   di           {di -> last valid byte}
    std                {backwards}
    xor   al,al
    repe  scasb
    cld                {in case someone else counts on cld}
    jnz   @1           {jump if any non-zeros found}
    mov   cx,-1        {force return of zero}
@1: inc   cx
@2: mov   ax,cx
  end;

  procedure SetRawMode(var F : file; On : Boolean); assembler;
    {-Set "raw" mode on or off for the specified file (must be a device)}
  asm
    les   di,F
    mov   bx,es:[di]   { bx = file handle    }
    mov   ax,$4400     { get device info     }
    int   $21
    jc    @9           { jump if error       }
    xor   ax,ax        { clear error code    }
    test  dl,$80       { is a device?        }
    jz    @9           { jump if not         }
    and   dx,$00AF     { clear unwanted bits }
    or    dl,$20       { assume raw mode     }
    cmp   On,0         { is On non-zero?     }
    jne   @1           { jump if On = True   }
    and   dl,$DF
@1: mov   ax,$4401     { set device info     }
    int   $21
    jc    @9           { jump if error       }
    xor   ax,ax        { clear error code    }
@9: mov   DosError,ax
  end;

  function Long2Str(L : LongInt) : string;
  var
    S : string;
  begin
    Str(L, S);
    Long2Str := S;
  end;

  { NOTE: This routine assumes that InLen is an even multiple of 3
    and that it is not less than 3 }
  procedure Scale200to300(var InBuf ;     InLen  : Word;
                          var OutBuf; var OutLen : Word); assembler;
  const
    ScaleBufDiv4 = ScaleBufSize div 4;

  asm
    push  ds
    lds   si,InBuf      { DS:SI->InBuf }
    mov   ax,InLen
    les   di,OutBuf     { ES:DI->OutBuf }
    push  di            { Save for later calculation }
    cld                 { go forward }

    xor   dx,dx
    mov   cx,3          { divide length of buffer by three }
    div   cx
    mov   cx,ax

    cmp   cx, ScaleBufDiv4  { test for OutBuf limits }
    jbe   @1
    mov   cx, ScaleBufDiv4  { reduce to upper limit }

    { BX and DX are output registers }

@1: push  cx            { save loop counter }

    lodsw               { get first two bytes of input }
    xchg  al,ah         { reverse the bytes so they're ordered properly }

    mov  bx,ax          {copy into bx}

    {use 12 bits of input from ax}
    shl  ax,1
    rcl  cx,1
    shl  ax,1
    rcl  cx,1
    shl  ax,1
    rcl  cx,1
    shl  cx,1           {skip output bit}
    shl  ax,1
    rcl  cx,1
    shl  ax,1
    rcl  cx,1
    shl  ax,1
    rcl  cx,1
    shl  cx,1           {skip output bit}
    shl  ax,1
    rcl  cx,1
    shl  ax,1
    rcl  cx,1
    shl  ax,1
    rcl  cx,1
    shl  cx,1           {skip output bit}
    shl  ax,1
    rcl  cx,1
    shl  ax,1
    rcl  cx,1
    shl  ax,1
    rcl  cx,1
    shl  cx,1           {skip output bit}

    {use 12 bits of input from bx}
    shl  dx,1           {skip output bit}
    shl  bx,1
    rcl  dx,1
    shl  bx,1
    rcl  dx,1
    shl  bx,1
    rcl  dx,1
    shl  dx,1           {skip output bit}
    shl  bx,1
    rcl  dx,1
    shl  bx,1
    rcl  dx,1
    shl  bx,1
    rcl  dx,1
    shl  dx,1           {skip output bit}
    shl  bx,1
    rcl  dx,1
    shl  bx,1
    rcl  dx,1
    shl  bx,1
    rcl  dx,1
    shl  dx,1           {skip output bit}
    shl  bx,1
    rcl  dx,1
    shl  bx,1
    rcl  dx,1
    shl  bx,1
    rcl  dx,1

    {have two bytes of output, having used 12 bits of input}
    or   cx,dx         {merge output words}
    xchg cl,ch
    mov  es:[di],cx
    inc  di
    inc  di

    {use last 4 bits of ax}
    shl  ax,1
    rcl  cx,1
    shl  ax,1
    rcl  cx,1
    shl  ax,1
    rcl  cx,1
    shl  cx,1           {skip output bit}
    shl  ax,1
    rcl  cx,1

    {use last 4 bits of bx}
    shl  dx,1           {skip output bit}
    shl  bx,1
    rcl  dx,1
    shl  bx,1
    rcl  dx,1
    shl  bx,1
    rcl  dx,1
    shl  dx,1           {skip output bit}
    shl  bx,1
    rcl  dx,1

    lodsb               {another byte of input}
    mov  bl,al          {copy into bl}

    {use 8 bits of al}
    shl  al,1
    rcl  cx,1
    shl  al,1
    rcl  cx,1
    shl  cx,1           {skip output bit}
    shl  al,1
    rcl  cx,1
    shl  al,1
    rcl  cx,1
    shl  al,1
    rcl  cx,1
    shl  cx,1           {skip output bit}
    shl  al,1
    rcl  cx,1
    shl  al,1
    rcl  cx,1
    shl  al,1
    rcl  cx,1
    shl  cx,1           {skip output bit}

    {use 8 bits of bl}
    shl  bl,1
    rcl  dx,1
    shl  bl,1
    rcl  dx,1
    shl  dx,1           {skip output bit}
    shl  bl,1
    rcl  dx,1
    shl  bl,1
    rcl  dx,1
    shl  bl,1
    rcl  dx,1
    shl  dx,1           {skip output bit}
    shl  bl,1
    rcl  dx,1
    shl  bl,1
    rcl  dx,1
    shl  bl,1
    rcl  dx,1

    {have another 2 bytes of output}
    or   cx,dx          {merge output words}
    xchg cl,ch
    mov  es:[di],cx
    inc  di
    inc  di

    pop  cx
    dec  cx             {more groups?}
    jcxz @2             {jump if not}
    jmp  @1             {do it again}

@2: pop  bx             { get initial DI in BX }

    sub   di,bx         { calculate and store length of output }
    mov   bx,di
    les   di,OutLen
    mov   es:[di],bx

    pop   ds            { restore ds }
  end;

  {---------- AbstractPrinter object ----------}

  constructor AbstractPrinter.Init;
  begin
    if not Root.Init then
      Fail;

    {Set defaults}
    PrnDest := 'PRN';
    fpOptions := DefPrinterOptions;

    {General inits}
    PrintBufCount := 0;
    FaxName := '';

    {Allocate the print buffer}
    if not GetMemCheck(PrintBuf, PrintBufSize) then
      Fail;

    {Clear print buffer}
    FastZero(PrintBuf^, PrintBufSize);

    {Allocate the pack buffer}
    if not GetMemCheck(PackBuf, PackBufSize) then begin
      FreeMemCheck(PrintBuf, PrintBufSize);
      Fail;
    end;
    FastZero(PackBuf^, PackBufSize);

    {Unused}
    MaxLinesPerPage := $FFFF;
    MinBlankSpace := 0;

    {Other general inits}
    CurrentPage := 0;
    TotalPages := 0;
    RasterCount := 0;

    {Init notification hook pointers}
    @LineNotify  := nil;
    @WriteNotify := nil;
    @PageNotify  := nil;
  end;

  destructor AbstractPrinter.Done;
  begin
    {Dispose of buffers}
    FreeMemCheck(PackBuf, PackBufSize);
    FreeMemCheck(PrintBuf, PrintBufSize);
    Root.Done;
  end;

  procedure AbstractPrinter.PrintOpen;
  begin
    Assign(PrnF, PrnDest);
    ReWrite(PrnF, 1);
    AsyncStatus := IoResult;
    if AsyncStatus <> ecOk then
      Exit;
    SetRawMode(PrnF, True);
    {Init to show an empty print buffer}
    PrintBufCount := 0;
  end;

  procedure AbstractPrinter.PrintClose;
  begin
    {Flush remaining data in buffer}
    if PrintBufCount > 0 then
      FlushPrintBuf;
    {Close the device/file}
    Close(PrnF);
    if IoResult <> 0 then {};
  end;

  procedure AbstractPrinter.StartNewPage;
  begin
    {Must be overridden}
    RunError(211);
  end;

  procedure AbstractPrinter.FlushPrintBuf;
  begin
    if AsyncStatus <> ecOk then Exit;                                  {!!.02}
    {Send current buffer contents}
    BlockWrite(PrnF, PrintBuf^[0], PrintBufCount);
    AsyncStatus := IoResult;
    if AsyncStatus <> ecOk then Exit;                                  {!!.02}

    {Clear buffer contents counter}
    PrintBufCount := 0;

    {Call user hook}
    if CallWriteNotify(PrintBufCount) then
      AsyncStatus := ecUserAbort;
  end;

  procedure AbstractPrinter.PrintData(var Data; Size : Word);
  var
    BytesToPrint : Word;
    ByteOfs : Word;
  begin
    ByteOfs := 0;
    repeat
      {Limit block to PrintBufSize}
      if Size > PrintBufSize then
        BytesToPrint := PrintBufSize
      else
        BytesToPrint := Size;

      {Dump buffer to output if the block won't fit}
      if PrintBufCount+BytesToPrint > PrintBufSize then
        FlushPrintBuf;
      if AsyncStatus <> ecOK then                                      {!!.03}
        Exit;                                                          {!!.03}

      {Move this data into the print buffer}
      FastMove(ByteArray(Data)[ByteOfs], PrintBuf^[PrintBufCount], BytesToPrint);

      inc(PrintBufCount, BytesToPrint);
      inc(ByteOfs, BytesToPrint);
      dec(Size, BytesToPrint);
    until Size = 0;
  end;

  procedure AbstractPrinter.PrintStr(S : String);
  begin
    PrintData(S[1], Length(S));
  end;

  function AbstractPrinter.PrintRasterLine(Buffer : pByteBuffer;
                                           InputLen : Word;
                                           PH : PageHeaderRec) : Boolean;
  begin
    {Must be overridden}
    RunError(211);
  end;

  function AbstractPrinter.MakeHeader : string;
  var
    S : string[100];
  begin
    S := 'Filename: ' + StUpCase(JustFileName(FaxName)) +
         ' Printed: ' + NowString + '  ' + TodayString +
         '                     Page ' + Long2Str(CurrentPage);

    {Start printing on second line if not using PCL5 mode}
    if not fpOptionsAreOn(fpPCL5Mode) then
      S := ^M^J + S;
    MakeHeader := S;
  end;

  procedure AbstractPrinter.SetLineNotifyFunc(LNF : LineNotifyType);
  begin
    LineNotify := LNF;
  end;

  procedure AbstractPrinter.SetWriteNotifyFunc(WNF : WriteNotifyType);
  begin
    WriteNotify := WNF;
  end;

  procedure AbstractPrinter.SetPageNotifyFunc(PNF : PageNotifyType);
  begin
    PageNotify := PNF;
  end;

  procedure AbstractPrinter.SetPrnDest(Dest : PathStr);
  begin
    PrnDest := Dest;
  end;

  procedure AbstractPrinter.SetPageNum(Page : Word);
  begin
    CurrentPage := Page;
  end;

  procedure AbstractPrinter.SetPageCount(Pages : Word);
  begin
    TotalPages := Pages;
  end;

  procedure AbstractPrinter.SetFaxName(FName : PathStr);
  begin
    FaxName := FName;
  end;

  procedure AbstractPrinter.fpOptionsOn(OptionFlags : Word);
  begin
    fpOptions := fpOptions or (OptionFlags and not BadPrinterOptions);
  end;

  procedure AbstractPrinter.fpOptionsOff(OptionFlags : Word);
  begin
    fpOptions := fpOptions and not (OptionFlags and not BadPrinterOptions);
  end;

  function AbstractPrinter.fpOptionsAreOn(OptionFlags : Word) : Boolean;
  begin
    fpOptionsAreOn := (fpOptions and OptionFlags = OptionFlags);
  end;

  function AbstractPrinter.GetPrnDest : PathStr;
  begin
    GetPrnDest := PrnDest;
  end;

  function AbstractPrinter.GetPrnBufCount : Word;
  begin
    GetPrnBufCount := PrintBufCount;
  end;

  function AbstractPrinter.GetLineCount : Word;
  begin
    GetLineCount := PrintedLineCount;
  end;

  function AbstractPrinter.GetMaxLinesPerPage : Word;
  begin
    GetMaxLinesPerPage := MaxLinesPerPage;
  end;

  function AbstractPrinter.GetRasterCount : Word;
  begin
    GetRasterCount := RasterCount;
  end;

  function AbstractPrinter.GetPageNum : Word;
  begin
    GetPageNum := CurrentPage;
  end;

  function AbstractPrinter.GetPageCount : Word;
  begin
    GetPageCount := TotalPages;
  end;

  function AbstractPrinter.GetFaxName : PathStr;
  begin
    GetFaxName := FaxName;
  end;

  function AbstractPrinter.CallWriteNotify(BytesWritten : Word) : Boolean;
  begin
    if @WriteNotify <> nil then
      CallWriteNotify := WriteNotify(BytesWritten)
    else
      CallWriteNotify := False;  {Don't abort}
    AsyncStatus := ecOk;  {Clear status}
  end;

  function AbstractPrinter.CallLineNotify(Lines, LineSize : Word) : Boolean;
  begin
    if @LineNotify <> nil then
      CallLineNotify := LineNotify(Lines, LineSize)
    else
      CallLineNotify := False;  {Don't abort}
    AsyncStatus := ecOk;  {Clear status}
  end;

  function AbstractPrinter.CallPageNotify(ThisPage, PageCount : Word) : Boolean;
  begin
    if @PageNotify <> nil then
      CallPageNotify := PageNotify(ThisPage, PageCount)
    else
      CallPageNotify := False;  {Don't abort}
    AsyncStatus := ecOk;  {Clear status}
  end;

  {---------- EpsonPrinter object ----------}

  constructor EpsonPrinter.Init;
  begin
    if not AbstractPrinter.Init then
      Fail;

    RasterCountDown  := 7;
    CombineCount := 0;
    PrintedLineCount := 0;
  end;

  destructor EpsonPrinter.Done;
  begin
    {Nothing special to dispose of -- so just call ancestor.done}
    AbstractPrinter.Done;
  end;

  procedure EpsonPrinter.PrintOpen;
  begin
    AbstractPrinter.PrintOpen;
    StartNewPage;
  end;

  procedure EpsonPrinter.PrintClose;
  begin
    {Print last set of raster lines}
    if RasterCountDown < 7 then
      PrintGraphics(PackBuf^, EpsMaxwidth, 24);

    PrintStr(EpsShutdown);
    AbstractPrinter.PrintClose;
  end;

  procedure EpsonPrinter.StartNewPage;
  begin
    {Print last set of raster lines}
    if RasterCountDown < 7 then
      PrintGraphics(PackBuf^, EpsMaxwidth, 24);

    PrintStr(EpsInit);
    Inc(CurrentPage);

    {Print a page header}
    if fpOptionsAreOn(fpHeader) then begin
      PrintStr(MakeHeader);
      PrintStr(^M^J);
      FillChar(PackBuf^, EpsMaxWidth, $80);
      PrintGraphics(PackBuf^, EpsMaxWidth, 8);
    end;

    {Init for next page}
    RasterCountDown  := 7;
    CombineCount := 0;
    PrintedLineCount := 0;
  end;

  procedure EpsonPrinter.PrintGraphics(var Buffer; ByteLen : Word;
                                            AdvanceLen : Byte);
  var
    N1 : Byte;
    N2 : Byte;
  begin
    {Never more than 1920 bytes = 1920 pixels wide = 8.00"}
    if ByteLen > EpsMaxWidth then
      ByteLen := EpsMaxWidth;

    {Trim trailing zeros}
    ByteLen := NonZeroLen(Buffer, ByteLen);

    {Print what's left}
    if ByteLen > 0 then begin
      N1 := ByteLen and $FF;
      N2 := ByteLen shr 8;
      PrintStr(#27'Z'+Char(N1)+Char(N2));
      PrintData(Buffer, ByteLen);
      FastZero(Buffer, EpsMaxWidth);
    end;

    {Vertical advance}
    PrintStr(#27'J'+Char(AdvanceLen)+^M);
  end;

  function EpsonPrinter.PrintRasterLine(Buffer : PByteBuffer;
                                        InputLen : Word;
                                        PH : PageHeaderRec) : Boolean;
  const                 {HighRes, Scaling}
    CombineLimit : array[Boolean, Boolean] of Byte =
      ((0, 1),     {HighRes = False}
       (1, 3));    {HighRes = True}
  var
    Scaling : Boolean;
  begin
    if InputLen = 0 then begin
      PrintRasterLine := False;
      Exit;
    end;
    if InputLen > EpsMaxWidth shr 3 then
      InputLen := EpsMaxWidth shr 3;

    {Update row RasterCountDown of PackBuf}
    asm
      push ds
      les  di,Self
      mov  ch,es:[di].RasterCountdown
      les  di,es:[di].PackBuf
      lds  si,Buffer
      mov  dx,si
      add  dx,InputLen       {dx = input offset terminator}
      jmp  @9
@1:   mov  ah,[si]           {ah = current input byte}
@2:   mov  al,ah
      shr  al,cl
      and  al,1
      xchg ch,cl
      shl  al,cl             {al = ((Input shr BitShift) and $01) shl RasterCountDown}
      xchg ch,cl
      or   es:[di],al        {or result into PackBuf}
      inc  di                {next output byte}
      or   cl,cl             {bit shift = 0?}
      jz   @3                {jump if so}
      dec  cl                {decrement bit shift}
      jmp  @2                {do it again}
@3:   inc  si                {next byte of input}
@9:   mov  cl,7              {reset bit shift}
      cmp  si,dx             {all input used?}
      jb   @1                {jump if not}
      pop  ds
    end;

    Scaling := fpOptionsAreOn(fpScale);
    {Dump PackBuf when 8 raster lines are combined and filled in}
    if CombineCount >=
       CombineLimit[(PH.ImgFlags and ffHighRes <> 0), Scaling] then begin
      CombineCount := 0;
      if RasterCountDown = 0 then begin
        RasterCountDown := 7;
        PrintGraphics(PackBuf^, EpsMaxWidth, 24);
        if AsyncStatus <> ecOk then begin
          PrintRasterLine := True;
          Exit;
        end;
      end else
        Dec(RasterCountDown);
    end else
      inc(CombineCount);

    {Call LineNotify routine}
    Inc(PrintedLineCount);
    if CallLineNotify(PrintedLineCount, InputLen) then begin
      PrintRasterLine := True;
      Exit;
    end;

    {No abort if we get here}
    PrintRasterLine := False;
  end;

  {---------- Epson24PinPrinter object ----------}

  constructor Epson24PinPrinter.Init;
  begin
    if not AbstractPrinter.Init then
      Fail;

    {Allocate scaling buffer}
    if not GetMemCheck(ScaleBuf, ScaleBufSize) then
      Fail;
    FastZero(ScaleBuf^, ScaleBufSize);

    RasterCountDown  := 23;
    PrintedLineCount := 0;
    RasterCount := 0;
  end;

  destructor Epson24PinPrinter.Done;
  begin
    FreeMemCheck(ScaleBuf, ScaleBufSize);
    AbstractPrinter.Done;
  end;

  procedure Epson24PinPrinter.PrintOpen;
  begin
    AbstractPrinter.PrintOpen;
    StartNewPage;
  end;

  procedure Epson24PinPrinter.PrintClose;
  begin
    {Print last set of raster lines}
    if RasterCountDown < 23 then
      PrintGraphics(PackBuf^, PackBufSize, 24);

    PrintStr(EpsShutdown);
    AbstractPrinter.PrintClose;
  end;

  procedure Epson24PinPrinter.StartNewPage;
  var
    I : Word;
  begin
    {Print last set of raster lines}
    if RasterCountDown < 23 then
      PrintGraphics(PackBuf^, PackBufSize, 24);

    PrintStr(EpsInit);
    Inc(CurrentPage);

    {Print a page header}
    if fpOptionsAreOn(fpHeader) then begin
      PrintStr(MakeHeader);
      PrintStr(^M^J);
      FastZero(PackBuf^, PackBufSize);
      for I := 0 to (PackBufSize div 3)-1 do
        PackBuf^[3*I] := $80;
      PrintGraphics(PackBuf^, PackBufSize, 8);
    end;

    {Init for next page}
    RasterCountDown  := 23;
    PrintedLineCount := 0;
    RasterCount := 0;
  end;

  procedure Epson24PinPrinter.PrintGraphics(var Buffer;
                                            ByteLen : Word;
                                            AdvanceLen : Byte);
  var
    N1 : Byte;
    N2 : Byte;
    Cols : Word;
  begin
    {Trim trailing zeros}
    ByteLen := NonZeroLen(Buffer, ByteLen);
    Cols := ByteLen mod 3;
    if Cols <> 0 then
      inc(ByteLen, 3-Cols);

    {Print what's left}
    if ByteLen > 0 then begin
      Cols := ByteLen div 3;
      N1 := Cols and $FF;
      N2 := Cols shr 8;
      PrintStr(#27'*'#40+Char(N1)+Char(N2));
      PrintData(Buffer, ByteLen);
      FastZero(Buffer, PackBufSize);
    end;

    {Vertical advance}
    PrintStr(#27'J'+Char(AdvanceLen)+^M);
  end;

  function Epson24PinPrinter.PrintRasterLine(Buffer : PByteBuffer;
                                             InputLen : Word;
                                             PH : PageHeaderRec) : Boolean;
  var
    OutLen : Word;
    Count : Word;
    ICount : Word;
    Scaling : Boolean;
    HighRes : Boolean;
  begin
    if InputLen = 0 then begin
      PrintRasterLine := False;
      Exit;
    end;

    Scaling := fpOptionsAreOn(fpScale);
    HighRes := (PH.ImgFlags and ffHighRes <> 0);
    inc(RasterCount);

    {Perform horizontal scaling if enabled}
    if Scaling and (InputLen >= 3) then
      Scale200to300(Buffer^, InputLen, ScaleBuf^, OutLen)
    else begin
      FastMove(Buffer^, ScaleBuf^, InputLen);
      OutLen := InputLen;
    end;

    {Perform vertical scaling if appropriate}
    if Scaling and not(HighRes) and Odd(RasterCount) then
      Count := 2
    else
      Count := 1;

    {Prevent pack buffer overflow}
    if OutLen > (PackBufSize shr 3) div 3 then
      OutLen := (PackBufSize shr 3) div 3;

    for ICount := 1 to Count do begin
      {Update row RasterCountDown of PackBuf}
      asm
        push ds
        les  di,Self
        mov  ch,es:[di].RasterCountdown
        lds  si,es:[di].ScaleBuf
        les  di,es:[di].PackBuf

        mov  al,ch
        and  ch,7              {ch = shift count into output byte}
        neg  al
        add  al,23             {al = 23-RasterCountdown}
        xor  ah,ah
        shr  ax,1
        shr  ax,1
        shr  ax,1              {get the byte offset into the output buffer}
        add  di,ax

        mov  dx,si
        add  dx,OutLen         {dx = input offset terminator}
        jmp  @9

@1:     mov  ah,[si]           {ah = current input byte}
@2:     mov  al,ah
        shr  al,cl
        and  al,1
        xchg ch,cl
        shl  al,cl             {al = ((Input shr BitShift) and $01) shl RasterCountDown}
        xchg ch,cl
        or   es:[di],al        {or result into PackBuf}
        add  di,3              {next output byte}
        or   cl,cl             {bit shift = 0?}
        jz   @3                {jump if so}
        dec  cl                {decrement bit shift}
        jmp  @2                {do it again}
@3:     inc  si                {next byte of input}
@9:     mov  cl,7              {reset bit shift}
        cmp  si,dx             {all input used?}
        jb   @1                {jump if not}
        pop  ds
      end;

      {Dump PackBuf when 24 raster lines are combined and filled in}
      if HighRes and ((RasterCount and 3) = 0) then
        {Next raster line will combine with this one}
      else if RasterCountDown = 0 then begin
        RasterCountDown := 23;
        PrintGraphics(PackBuf^, PackBufSize, 24);
        if AsyncStatus <> ecOk then begin
          PrintRasterLine := True;
          Exit;
        end;
      end else
        Dec(RasterCountDown);
    end;

    {Call LineNotify routine}
    Inc(PrintedLineCount);
    if CallLineNotify(PrintedLineCount, InputLen) then begin
      PrintRasterLine := True;
      Exit;
    end;

    {No abort if we get here}
    PrintRasterLine := False;
  end;

  {---------- LaserJet-specific routines ----------}

  procedure TiffEncode( var InBuf ;     InLen  : Word;
                        var OutBuf; var OutLen : Word ); assembler;
  asm
    push ds

    mov  ax,InLen                 {get input length}
    or   ax,ax                    {is it zero?}
    jnz  @I1                      {jump if not}
    xor  di,di                    {return zero output length}
    jmp  @A

@I1:les  di,OutBuf                {di = current output offset}
    mov  dx,di                    {dx = saved starting output offset}
    mov  bx,di                    {bx = control byte offset}
    mov  byte ptr es:[bx],0       {reset initial control byte}

    lds  si,InBuf                 {si = current input offset}

    cmp  ax,1                     {is input length 1?}
    ja   @I2                      {jump if not}

    mov  al,[si]                  {get first input byte}
    mov  es:[bx+1],al             {store it past control byte}
    mov  di,2                     {output length is two}
    jmp  @A                       {exit}

@I2:cld                           {forward}
    mov  cx,si
    add  cx,ax                    {cx = offset just beyond end of input}

    mov  ax,[si]                  {does data start with a run?}
    cmp  ah,al
    je   @I3                      {jump if so}

    inc  di                       {prepare to store first input byte}
    mov  es:[di],al               {store it}
    inc  di                       {prepare to store next input byte}
    inc  si                       {we've used first input byte}

@I3:dec  si                       {first time in, adjust for next inc si}

@1: inc  si                       {next input byte}
    cmp  si,cx
    jae  @9                       {jump out if done}

    mov  ax,[si]                  {get next two bytes}
    cmp  ah,al                    {the same?}
    jne  @5                       {jump if not a run}
    mov  bx,di                    {save OutPos offset}
    mov  byte ptr es:[bx],0       {reset control byte}
    mov  es:[bx+1],al             {store run byte}

@2: inc  si                       {next input byte}
    cmp  si,cx                    {end of input?}
    jae  @3                       {jump if so}
    cmp  [si],al                  {still a run?}
    jne  @3                       {jump if not}
    cmp  byte ptr es:[bx],$81     {max run length?}
    je   @3                       {jump if so}
    dec  byte ptr es:[bx]         {decrement control byte}
    jmp  @2                       {loop}

@3: dec  si                       {back up one input character}
    inc  di                       {step past control and run bytes}
    inc  di
    jmp  @1                       {loop}

@5: cmp  byte ptr es:[bx],$7f     {run already in progress?}
    jb  @6                        {jump if not}
    mov  bx,di                    {start a new control sequence}
    mov  byte ptr es:[bx],0       {reset control byte}
    inc  di                       {next output position}
    jmp  @7
@6: inc  byte ptr es:[bx]         {increase non-run length}
@7: stosb                         {copy input byte to output}
    jmp  @1

@9: pop  ds
    sub  di,dx
@A: les  si,OutLen
    mov  es:[si],di
  end;

  { NOTE: This routine assumes that InLen is an even multiple of 2 }
  procedure NoScale200to150(var InBuf ;     InLen  : Word;
                            var OutBuf; var OutLen : Word); assembler;
  asm
    push  ds
    lds   si,InBuf
    les   di,OutBuf
    mov   cx,InLen
    shr   cx,1

    cmp   cx, ScaleBufSize  { test for OutBuf limits }
    jbe   @0
    mov   cx, ScaleBufSize  { reduce to upper limit }

@0: push  cx
    cld

@1: lodsw
    xchg  al,ah

    shr   ax,1
    rcr   bx,1
    shr   ax,1
    rcr   dx,1

    shr   ax,1
    rcr   bx,1
    shr   ax,1
    rcr   dx,1

    shr   ax,1
    rcr   bx,1
    shr   ax,1
    rcr   dx,1

    shr   ax,1
    rcr   bx,1
    shr   ax,1
    rcr   dx,1

    shr   ax,1
    rcr   bx,1
    shr   ax,1
    rcr   dx,1

    shr   ax,1
    rcr   bx,1
    shr   ax,1
    rcr   dx,1

    shr   ax,1
    rcr   bx,1
    shr   ax,1
    rcr   dx,1

    shr   ax,1
    rcr   bx,1
    shr   ax,1
    rcr   dx,1

    or    bx,dx
    mov   es:[di],bh
    inc   di

    loop  @1

    pop   cx
    les   di,OutLen
    mov   es:[di],cx

    pop   ds
  end;

  { NOTE: This routine assumes that InLen is an integer multiple of 3 }
  procedure Scale200to150(var InBuf ;     InLen  : Word;
                          var OutBuf; var OutLen : Word); assembler;
  const
    ScaleBufDiv2 = ScaleBufSize div 2;

  asm
    push  ds

    lds   si,InBuf      { DS:SI->InBuf               }
    les   di,OutBuf     { ES:DI->OutBuf              }
    push  di            { save for later length calculation }
    cld                 { forward }
    mov   ax,InLen
    xor   dx,dx
    mov   cx,3
    div   cx
    mov   cx,ax         { cx = times through loop below }

    cmp   cx,ScaleBufDiv2  { test for OutBuf limits }
    jbe   @0
    mov   cx,ScaleBufDiv2  { reduce to upper limit }

@0: or    cx,cx         { can't use JCXZ...too much to jump over }
    jnz   @1
    jmp   @9

@1: lodsb
    shl   al,1      {i0..i2 -> o0..o1}
    rcl   bx,1
    shl   al,1
    rcl   dl,1
    shl   al,1
    rcl   dh,1
    or    dl,dh
    shr   dl,1
    rcl   bx,1

    shl   al,1      {i3..i5 -> o2..o3}
    rcl   bx,1
    shl   al,1
    rcl   dl,1
    shl   al,1
    rcl   dh,1
    or    dl,dh
    shr   dl,1
    rcl   bx,1

    shl   al,1      {i6..i8 -> o4..o5}
    rcl   bx,1
    shl   al,1
    rcl   dl,1

    lodsb

    shl   al,1
    rcl   dh,1
    or    dl,dh
    shr   dl,1
    rcl   bx,1

    shl   al,1      {i9..i11 -> o6..o7}
    rcl   bx,1
    shl   al,1
    rcl   dl,1
    shl   al,1
    rcl   dh,1
    or    dl,dh
    shr   dl,1
    rcl   bx,1

    shl   al,1      {i12..i14 -> o8..o9}
    rcl   bx,1
    shl   al,1
    rcl   dl,1
    shl   al,1
    rcl   dh,1
    or    dl,dh
    shr   dl,1
    rcl   bx,1

    shl   al,1      {i15..i17 -> o10..o11}
    rcl   bx,1

    lodsb

    shl   al,1
    rcl   dl,1
    shl   al,1
    rcl   dh,1
    or    dl,dh
    shr   dl,1
    rcl   bx,1

    shl   al,1      {i18..i20 -> o12..o13}
    rcl   bx,1
    shl   al,1
    rcl   dl,1
    shl   al,1
    rcl   dh,1
    or    dl,dh
    shr   dl,1
    rcl   bx,1

    shl   al,1      {i21..i23 -> o14..o15}
    rcl   bx,1
    shl   al,1
    rcl   dl,1
    shl   al,1
    rcl   dh,1
    or    dl,dh
    shr   dl,1
    rcl   bx,1

    mov   ax,bx
    xchg  ah,al
    stosw

    dec   cx
    jcxz  @9
    jmp   @1            { process next block of 3 input bytes }

@9: pop   bx
    sub   di,bx         { di = OutLen }
    les   si,OutLen
    mov   es:[si],di    { return in var parameter }

    pop   ds
  end;


  {---------- HP150Printer object ----------}

  constructor HP150Printer.Init;
  begin
    if not AbstractPrinter.Init then
      Fail;
    {Allocate scaling buffer}
    if not GetMemCheck(ScaleBuf, ScaleBufSize) then
      Fail;
    FastZero(ScaleBuf^, ScaleBufSize);

    {Set maximum raster lines per page}
    MaxLinesPerPage := LinesPerPageLimit div 2;

    {Reduce raster lines per page for older PCL printers}
    if not fpOptionsAreOn(fpPCL5Mode) then
      Dec(MaxLinesPerPage, Pcl4LineAdj div 2);

    MinBlankSpace := BLSSize div 2;
    BlankLinesToPrint := 0;
  end;

  destructor HP150Printer.Done;
  begin
    {Dispose of the scaling buffer}
    FreeMemCheck(ScaleBuf, ScaleBufSize);
    AbstractPrinter.Done;
  end;

  procedure HP150Printer.PrintOpen;
  begin
    AbstractPrinter.PrintOpen;
    StartNewPage;
  end;

  procedure HP150Printer.PrintClose;
  begin
    PrintStr(PclEndGraph + PclInit);
    AbstractPrinter.PrintClose;
  end;

  procedure HP150Printer.StartNewPage;
  begin
    {No sense printing just blank lines if we're starting a new page}
    BlankLinesToPrint := 0;

    PrintStr(PclInit);
    PrintStr(PclSetMargin + Pcl150DPI);

    Inc(CurrentPage);

    {Print the page header}
    if fpOptionsAreOn(fpHeader) then begin
      PrintStr(MakeHeader);

      {Start graphics mode}
      PrintStr(PclStartGraph);

      {Push number of blank lines after header area}
      Inc(BlankLinesToPrint, AfterTextSpace div 2);
    end else
      {Start graphics mode}
      PrintStr(PclStartGraph);

    if fpOptionsAreOn(fpPcl5Mode) then begin
      PrintStr(PclTiffMode);
      {Print a line under the text}
      if fpOptionsAreOn(fpHeader) then
        PrintStr(PclLine150);
    end else begin
      {Print a line under the text}
      if fpOptionsAreOn(fpHeader) then begin
        FillChar(PackBuf^, 150, $FF);
        PrintStr(#27'*b150W');    {print divider line}
        PrintData(PackBuf^, 150);
      end;
    end;

    RasterCount := 0;
    PrintedLineCount := 0;
  end;

  procedure HP150Printer.PrintData(var Data; Size : Word);
  var
    S              : string[21];
    i              : Integer;
  begin
    {Any pending blank lines are printed first}
    if (BlankLinesToPrint > 0) then begin
      if fpOptionsAreOn(fpSuppressBlanks) then begin
        if BlankLinesToPrint > MinBlankSpace then begin

          {Reduce the running line count by this amount}
          Dec(PrintedLineCount, BlankLinesToPrint - MinBlankSpace);
          BlankLinesToPrint := MinBlankSpace;
        end;
      end;

      if fpOptionsAreOn(fpPCL5Mode) then begin
        S := PclBlankS + Long2Str(BlankLinesToPrint) + PclBlankE;
        AbstractPrinter.PrintData(S[1], Length(S));
      end else begin
        S := PclXferS + '0' + PclXferE;
        for i := 1 to BlankLinesToPrint do
          AbstractPrinter.PrintData(S[1], Length(S));
      end;

      {Clear blank line count}
      BlankLinesToPrint := 0;
    end;

    {Call ancestor to do the rest}
    AbstractPrinter.PrintData(Data, Size);
  end;

  function HP150Printer.PrintRasterLine(Buffer : pByteBuffer;
                                        InputLen : Word;
                                        PH : PageHeaderRec) : Boolean;
  var
    NewI      : Word;
    Limit     : Word;
    PackI     : Word;
    Bytes     : string[25];
    RLP       : LongInt;     {Real lines printed}                      {!!.01}

  begin
    PrintRasterLine := False;

    {Check buffer to make sure it has at least 3 bytes}
    while InputLen < 3 do begin
      Buffer^[InputLen] := 0;
      Inc(InputLen);
    end;

    {Check if the page is full}
    if (PrintedLineCount >= MaxLinesPerPage - (ProbationArea div 2)) then begin
      {In the probation area}
      RLP := LongInt(PrintedLineCount) - BlankLinesToPrint;            {!!.01}
      if RLP >= MaxLinesPerPage then begin                             {!!.01}
        {Time to force a new page if this isn't a blank line}
        if NonZeroLen(Buffer^, InputLen) > 0 then begin
          StartNewPage;
          if CallPageNotify(CurrentPage, TotalPages) then begin
            PrintRasterLine := True;
            Exit;
          end;
        end;
      end;
    end;

    Limit := 0;
    Inc(RasterCount);
    if PH.ImgFlags and ffHighRes <> 0 then begin
      if RasterCount and 3 = 0 then
        Exit;
    end else if Odd(RasterCount) then
      Inc(Limit);

    {Handle special case for zero filled lines}
    if NonZeroLen(Buffer^, InputLen) = 0 then begin
      Inc(BlankLinesToPrint, Succ(Limit));
      Inc(PrintedLineCount, Succ(Limit));
      Exit;
    end;

    {Scale down fax raster line to PCL raster line, 200 -> 150}
    if fpOptionsAreOn(fpScale) then
      Scale200to150(Buffer^, InputLen, ScaleBuf^, NewI)
    else
      NoScale200to150(Buffer^, InputLen, ScaleBuf^, NewI);

    {Use TIFF compression in PCL5 mode}
    if fpOptionsAreOn(fpPcl5Mode) then
      TiffEncode(ScaleBuf^, NewI, PackBuf^, PackI)
    else begin
      FastMove(ScaleBuf^, PackBuf^, NewI);
      PackI := NewI;
    end;

    {Send the raster data}
    PrintStr(PclXferS);
    Bytes := Long2Str(PackI) + PclXferE;
    PrintStr(Bytes);
    PrintData(PackBuf^, PackI);
    Inc(PrintedLineCount);

    {Print every other raster line twice in standard resolution}
    if Limit > 0 then begin
      if fpOptionsAreOn(fpPCL5Mode) then begin
        PrintStr(PclDeltaMode);
        PrintStr(PclRepLine);
      end else begin
        PrintStr(PclXferS);
        PrintStr(Bytes);
        PrintData(PackBuf^, PackI);
      end;
      Inc(PrintedLineCount);

      {Switch back to proper mode}
      if fpOptionsAreOn(fpPcl5Mode) then
        PrintStr(PclTiffMode);
    end;

    if AsyncStatus <> ecOk then
      PrintRasterLine := True;

    if CallLineNotify(PrintedLineCount, InputLen) then
      PrintRasterLine := True;
  end;


  {---------- HP300Printer object ----------}

  constructor HP300Printer.Init;
  begin
    if not HP150Printer.Init then
      Fail;
    {Set maximum raster lines per page}
    MaxLinesPerPage := LinesPerPageLimit;

    {Reduce raster lines per page for older PCL printers}
    if not fpOptionsAreOn(fpPCL5Mode) then
      Dec(MaxLinesPerPage, Pcl4LineAdj);

    MinBlankSpace := BLSSize;
  end;

  destructor HP300Printer.Done;
  begin
    HP150Printer.Done;
  end;

  procedure HP300Printer.PrintOpen;
  begin
    AbstractPrinter.PrintOpen;
    StartNewPage;
  end;

  procedure HP300Printer.PrintClose;
  begin
    HP150Printer.PrintClose;
  end;

  procedure HP300Printer.StartNewPage;
  begin
    {No sense printing just blank lines if we're starting a new page}
    BlankLinesToPrint := 0;

    PrintStr(PclInit);
    PrintStr(PclSetMargin + Pcl300DPI);

    Inc(CurrentPage);

    {Print the page header}
    if fpOptionsAreOn(fpHeader) then begin
      PrintStr(MakeHeader);

      {Start graphics mode}
      PrintStr(PclStartGraph);
      Inc(BlankLinesToPrint, AfterTextSpace);
    end else
      {Start graphics mode}
      PrintStr(PclStartGraph);

    if fpOptionsAreOn(fpPcl5Mode) then begin
      PrintStr(PclTiffMode);
      {Print a line under the text}
      if fpOptionsAreOn(fpHeader) then
        PrintStr(PclLine300)
    end else begin
      {Print a line under the text}
      if fpOptionsAreOn(fpHeader) then begin
        FillChar(PackBuf^, 300, $FF);
        PrintStr(#27'*b300W');    {print divider line}
        PrintData(PackBuf^, 300);
      end;
    end;

    RasterCount := 0;
    PrintedLineCount := 0;
    BlankLinesToPrint := 0;
  end;

  function HP300Printer.PrintRasterLine(Buffer : pByteBuffer;
                                        InputLen : Word;
                                        PH : PageHeaderRec) : Boolean;
  var
    NewI      : Word;
    I         : Word;
    Limit     : Word;
    PackI     : Word;
    Bytes     : string[25];
    RLP       : LongInt;     {Real lines printed}                      {!!.01}

  begin
    PrintRasterLine := False;
    {Check buffer to make sure it has at least 3 bytes}
    while InputLen < 3 do begin
      Buffer^[InputLen] := 0;
      Inc(InputLen);
    end;

    {Check if the page is full}
    if (PrintedLineCount >= MaxLinesPerPage - ProbationArea) then begin
      {In the probation area}
      RLP := LongInt(PrintedLineCount) - BlankLinesToPrint;            {!!.01}
      if RLP >= MaxLinesPerPage then begin                             {!!.01}
        {Time to force a new page if this isn't a blank line}
        if NonZeroLen(Buffer^, InputLen) > 0 then begin
          StartNewPage;
          if CallPageNotify(CurrentPage, TotalPages) then begin
            PrintRasterLine := True;
            Exit;
          end;
        end;
      end;
    end;

    {Compute number of times to print this raster line}
    Inc(RasterCount);
    if PH.ImgFlags and ffHighRes <> 0 then
      {Print every other raster line twice}
      if Odd(RasterCount) then
        Limit := 0
      else
        Limit := 1
    else
      {Print each raster line three times}
      Limit := 2;

    {Handle special case for zero lines}
    if NonZeroLen(Buffer^, InputLen) = 0 then begin
      Inc(BlankLinesToPrint, Succ(Limit));
      Inc(PrintedLineCount, Succ(Limit));
      Exit;
    end;

    {Scale up fax raster line to PCL raster line, 200 -> 300}
    if fpOptionsAreOn(fpScale) then
      Scale200to300(Buffer^, InputLen, ScaleBuf^, NewI)
    else begin
      FastMove(Buffer^, ScaleBuf^, InputLen);
      NewI := InputLen;
    end;

    {Use TIFF compression in PCL5 mode}
    if fpOptionsAreOn(fpPcl5Mode) then
      TiffEncode(ScaleBuf^, NewI, PackBuf^, PackI)
    else begin
      FastMove(ScaleBuf^, PackBuf^, NewI);
      PackI := NewI;
    end;

    {Send raster data}
    PrintStr(PclXferS);
    Bytes := Long2Str(PackI) + PclXferE;
    PrintStr(Bytes);
    PrintData(PackBuf^, PackI);
    Inc(PrintedLineCount);

    {Repeat raster line as necessary}
    if Limit > 0 then begin
      if fpOptionsAreOn(fpPCL5Mode) then begin
        PrintStr(PclDeltaMode);
        for I := 1 to Limit do begin
          PrintStr(PclRepLine);
          Inc(PrintedLineCount);
        end;
      end else begin
        for I := 1 to Limit do begin
          PrintStr(PclXferS);
          PrintStr(Bytes);
          PrintData(PackBuf^, PackI);
          Inc(PrintedLineCount);
        end;
      end;

      {Switch back to proper mode}
      if fpOptionsAreOn(fpPcl5Mode) then
        PrintStr(PclTiffMode);
    end;

    if AsyncStatus <> ecOk then
      PrintRasterLine := True;

    if CallLineNotify(PrintedLineCount, InputLen) then
      PrintRasterLine := True;
  end;


  {---------- FaxPrinter object ----------}

  constructor FaxPrinter.Init(APrinter : AbstractPrinterPtr);
  begin
    if not UnpackFax.Init then
      Fail;
    fpPrinter := APrinter;
  end;

  destructor FaxPrinter.Done;
  begin
    UnpackFax.Done;
  end;

  procedure FaxPrinter.SetLineNotifyFunc(LNF : LineNotifyType);
  begin
    fpPrinter^.SetLineNotifyFunc(LNF);
  end;

  procedure FaxPrinter.SetWriteNotifyFunc(WNF : WriteNotifyType);
  begin
    fpPrinter^.SetWriteNotifyFunc(WNF);
  end;

  procedure FaxPrinter.SetPageNotifyFunc(PNF : PageNotifyType);
  begin
    fpPrinter^.SetPageNotifyFunc(PNF);
  end;

  procedure FaxPrinter.SetScaling(On : Boolean);
  begin
    if On then
      fpPrinter^.fpOptionsOn(fpScale)
    else
      fpPrinter^.fpOptionsOff(fpScale);
  end;

  procedure FaxPrinter.SetHeader(On : Boolean);
  begin
    if On then
      fpPrinter^.fpOptionsOn(fpHeader)
    else
      fpPrinter^.fpOptionsOff(fpHeader);
  end;

  procedure FaxPrinter.SetPCL5Mode(On : Boolean);
  begin
    if On then
      fpPrinter^.fpOptionsOn(fpPCL5Mode)
    else
      fpPrinter^.fpOptionsOff(fpPCL5Mode);
  end;

  procedure FaxPrinter.SetBlankLineSuppression(On : Boolean);
  begin
    if On then
      fpPrinter^.fpOptionsOn(fpSuppressBlanks)
    else
      fpPrinter^.fpOptionsOff(fpSuppressBlanks);
  end;

  function FaxPrinter.OutputLine : Boolean;
  begin
    AsyncStatus := ecOk;

    {Call the currently installed printer's PrintRasterLine method}
    OutPutLine := fpPrinter^.PrintRasterLine(LineBuffer, LineOfs, PageHeader);
  end;

  procedure FaxPrinter.PrintFax(ADest : PathStr; AFaxName : PathStr);
  var
    I   : Word;
    FH  : FaxHeaderRec;
  begin
    {Initialize printer variables}
    with fpPrinter^ do begin
      SetFaxName(AFaxName);
      SetPrnDest(ADest);
      SetPageNum(0);
    end;

    GetFaxHeader(AFaxName, FH);
    if AsyncStatus = ecOk then begin
      I := 1;
      fpPrinter^.SetPageCount(FH.PageCount);
      repeat
        fpPrinter^.PrintOpen;
        if AsyncStatus = ecOk then begin
          with fpPrinter^ do
            if CallPageNotify(GetPageNum, GetPageCount) then
              AsyncStatus := ecUserAbort;
          {Call unpacker to unpack the page}
          if AsyncStatus = ecOk then                                   {!!.02}
            UnpackPage(AFaxName, I);                                   {!!.02}
          {Next page}
          Inc(I);
          fpPrinter^.PrintClose;
        end;
      until (I > fpPrinter^.GetPageCount) or (AsyncStatus <> ecOk);
    end;
  end;

end.
