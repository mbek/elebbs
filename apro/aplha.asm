;*********************************************************************;
;                         APLHA.ASM 2.03                              ;
;              ASM routines for APLZH.PAS and OOLZH.PAS               ;
;    Pascal version Copyright (c) 1987, 1993 TurboPower Software.     ;
;        Portions copyright (c) Information Technology 1989,          ;
;           C++ version Copyright (c) 1992 Techmate, INC.             ;
;                      All rights reserved.                           ;
;                                                                     ;
;                 Licensed to TurboPower Software.                    ;
;*********************************************************************;


DosCall         MACRO   DosFuncNum
                MOV     AH,DosFuncNum   ;AH = Function number
                INT     21h             ;Call DOS
                ENDM

SaveAllRegs     MACRO
                PUSH    ES              ;save all basic registers
                PUSH    DS
                PUSH    DI
                PUSH    SI
                PUSH    BP
                PUSH    DX
                PUSH    CX
                PUSH    BX
                PUSH    AX
                ENDM

RestoreAllRegs  MACRO
                POP     AX              ;restore all basic registers
                POP     BX
                POP     CX
                POP     DX
                POP     BP
                POP     SI
                POP     DI
                POP     DS
                POP     ES
                ENDM

PushDWord       MACRO   VALUE
                PUSH    WORD PTR VALUE+2        ;PUSH high word
                PUSH    WORD PTR VALUE          ;PUSH low word
                ENDM

RestoreDS       MACRO
                PUSH    AX
                MOV     AX, DATA
                MOV     DS, AX
                POP     AX
                ASSUME  DS:DATA
                ENDM

SetDS           MACRO   SEGMENTNAME
                PUSH    AX
                MOV     AX, DATA
                MOV     DS, AX
                MOV     AX, DS:SEGMENTNAME
                MOV     DS, AX
                POP     AX
                ASSUME  DS:NOTHING
                ENDM

SetES           MACRO   SEGMENTNAME
                PUSH    AX
                PUSH    DS
                MOV     AX, DATA
                MOV     DS, AX
                MOV     ES, DS:SEGMENTNAME
                POP     DS
                POP     AX
                ASSUME  ES:NOTHING
                ENDM

N1              =       2000h
N2              =       N1 * 2                   ;4000h
N3              =       N1 * 3                   ;6000h

THRESHOLD       =       3
ECUSERABORT     =       2926                     ;from apmisc.pas
ECDISKFULL      =       0101                     ;from apmisc.pas
ECBADFILEFORMAT =       9964                     ;from apmisc.pas

NP              =       14
NC              =       (200h - 2)
NT              =       19
PBIT            =       4
CBIT            =       9
TBIT            =       5
NPT             =       080h
NN1             =       2000h / 64                         ;128
NN2             =       NN1 * 2                            ;256
N_2             =       (256 + 60 - THRESHOLD + 1) * 2     ;628

HASH1           =       13 - 8 + 1                         ;6
MAXMATCH        =       256                                ;0100h
DICTIONARYSIZE  =       2000h                              ;8192
DICSIZE1        =       DICTIONARYSIZE - 1
DICSIZE2        =       DICTIONARYSIZE * 2
DICSIZE4        =       DICTIONARYSIZE * 4

;The zero values below are used as dummy offsets to try to add some level
;of readability while accessing the structures within Seg1, Seg2 and Seg3.
NEXT            EQU     (0)                      ;Seg1
PARENT          EQU     (0)                      ;Seg2
POSITION        EQU     (WORD PTR N2 * 2)
PREV            EQU     (0)                      ;Seg3
LEVEL           EQU     (BYTE PTR N2 * 2)
CHILDCOUNT      EQU     (BYTE PTR N2*2 + 1)


;ExtraSeg (Match String Buffer and other variables)
;8000h + ((NC+1) * 2) + (3*4) + (21*2) + 3 bytes

;The section of data from TextBuffer through the end of Filler is used
;as a general buffer at times and therefore this portion of the structure
;must not be altered.

;UNION's and nested STRUC's removed below for MASM compatibility

ExtraData       STRUC
;
TextBuffer      DB      (N2 + MAXMATCH) dup(?)     ;4100h
Left            DW      2 * NC - 1 dup (?)         ;0400h - 4 - 1
Right           DW      2 * NC - 1 dup (?)         ;0400h - 4 - 1
;;UNION
  CTable        DW      4096 dup (?)               ;1000h
;;  STRUC
;;    CFreq       DW      2 * NC - 1 dup (?)         ;400h - 5 words
;;    PFreq       DW      2 * NP - 1 dup (?)         ;27 words
;;    TFreq       DW      2 * NT - 1 dup (?)         ;37 words
;;  ENDS          ;end of nested structure
;;ENDS            ;end of union
CCode           DW      NC dup (?)                 ;0400h - 4
PTTable         DW      256 dup (?)                ;0200h
PTCode          DW      NPT dup (?)                ;0100h
CLength         DB      NC dup (?)                 ;0200h - 2
PTLength        DB      NPT dup (?)                ;0080h
Filler          DB      8000h - (PTLength + NPT - TextBuffer) dup (?)
;
;;UNION
  Heap          DW      NC + 1 dup (?)
;;  STRUC
;;    LengthCount DW      17 dup (?)
;;    Start       DW      17 dup (?)
;;    Weight      DW      17 dup (?)
;;  ENDS          ;end of nested structure
;;ENDS            ;end of union

OutputPos       DD      (?)
BytesProcessed  DD      (?)

TextEnd         DW      (?)
BlockSize       DW      (?)
CPos            DW      (?)
Unpackable      DW      (?)
MatchPos        DW      (?)
MatchLen        DW      (?)
LastMatchLen    DW      (?)
LastMatchPos    DW      (?)
Avail           DW      (?)
Pos             DW      (?)
Remainder       DW      (?)
AvailableMT     DW      (?)
NChar           DW      (?)
BitLen          DW      (?)

;;UNION
  Freq2         DW      (?)
;;  TableBits     DW      (?)
;;ENDS            ;end of union

;;UNION
  Code2         DW      (?)
;;  TTable        DW      (?)
;;ENDS            ;end of union

LzhN2           DW      (?)
HeapSize        DW      (?)
Depth           DW      (?)
BitBuf          DW      (?)
InitialSP       DW      (?)

OutputMask      DB      (?)
SubBitBuf       DB      (?)
BitCount        DB      (?)

ExtraData       ENDS

;equates instead of UNION's and nested STRUC's, to allow MASM compatibility

CFreq           EQU     (WORD PTR CTable)
PFreq           EQU     (WORD PTR CFreq + ((NC*2 - 1)*2))
TFreq           EQU     (WORD PTR PFreq + ((NP*2 - 1)*2))

LengthCount     EQU     (WORD PTR Heap)
Start           EQU     (WORD PTR LengthCount + (17*2))
Weight          EQU     (WORD PTR Start + (17*2))

TableBits       EQU     (WORD PTR Freq2)
TTable          EQU     (WORD PTR Code2)


DATA    SEGMENT WORD PUBLIC

        EXTRN   glSPF          :DWORD            ;Procedure to show progress
        EXTRN   CRCTable       :WORD             ;Table of CRC values
        EXTRN   ArchiveStatus  :WORD             ;Last LZH error
        EXTRN   zCRC           :WORD             ;file CRC
        EXTRN   OrigSize       :DWORD            ;Original file size
        EXTRN   OrigFileSize   :DWORD            ;Holds OrigSize       !!.01
        EXTRN   CompSize       :DWORD            ;Compressed size

        EXTRN   InFile         :WORD             ;Open input file
        EXTRN   OutF           :WORD             ;Open output file

        EXTRN   EDS            :WORD             ;Extra data segment
        EXTRN   Seg1           :WORD             ;Next structure
        EXTRN   Seg2           :WORD             ;Parent structure
        EXTRN   Seg3           :WORD             ;Prev structure
        EXTRN   Buf            :DWORD            ;Buffer
        EXTRN   BufLimit       :WORD             ;End of buffer

DATA ENDS


CODE    SEGMENT WORD PUBLIC

        ASSUME DS:DATA, CS:CODE

        ;external pascal procedures
        EXTRN   lhaGetC    :FAR
        EXTRN   lhaPutC    :FAR

        ;publics
        PUBLIC  LhaEncode
        PUBLIC  LhaDecode

;
;***********************************************************Abort
;
; On entry, AX contains the error code to return.
; Called to abort the current process, returning the stack to
; its initial state and restoring all registers.
;
Abort           PROC FAR
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        RestoreDS
        MOV             DS:ArchiveStatus, AX     ;set the error status
        MOV             DS, EDS
        ASSUME  DS:NOTHING
StackTest:
        CMP             SP, DS:InitialSP         ;is SP back to the original
        JE              AbortEnd
        POP             AX
        JMP             SHORT StackTest
ABortEnd:
        RestoreAllRegs                           ;restore all registers
        RETF                                     ;return to original caller
Abort              ENDP

;
;***********************************************************ShowProgress2
;
ShowProgress2      PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        ADD        BP, DICTIONARYSIZE
        ;adjust buffer pointer and then fall through to ShowProgress
ShowProgress2   ENDP

;
;***********************************************************ShowProgress
;
ShowProgress    PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        SaveAllRegs
        SetDS EDS
        PushDWord      DS:BytesProcessed         ;push # of bytes written
        RestoreDS
        PushDWord      DS:OrigFileSize           ;push total # of bytes !!.01
        CALL           DWORD PTR DS:glSPF
        OR             AL, AL                    ;did it return False?
        JNZ            dmOK
        MOV            AX, ECUSERABORT                                 ;!!.02
        CALL           Abort                                           ;!!.02
;       MOV            DS:ArchiveStatus, ECUSERABORT                   ;!!.02
dmOK:
        RestoreAllRegs
        RET
ShowProgress    ENDP

;
;***********************************************************InitSlide
;
InitSlide       PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        CLD
        PUSH            BX
        PUSH            CX
        PUSH            DI

        MOV             BX, DICSIZE2             ;BX=4000h

        ;get offset of one past end of buffer
        LEA             AX, DS:TextBuffer[BX]    ;get address of buffer end
        MOV             DS:TextEnd, AX           ;store it

        ;init first 2000h words of segment 1 with 2,4,6,8,...
        SetES Seg1
        XOR             AX, AX
        MOV             DI, AX
        MOV             CX, N1                   ;Init first 2000h words
InitNextLoop:
        INC             AX
        INC             AX                       ;with multiples of 2
        STOSW
        LOOP            InitNextLoop

        ;init rest of segment 1, to zero's
        XOR             AX, AX
        MOV             DI, N2                   ;DI=4000h
        MOV             CX, N2                   ;CX=4000h -- 8000h bytes
        REP             STOSW                    ;C000 bytes

        ;init segment 2, to zero's
        SetES Seg2
        XOR             AX, AX
        MOV             DI, AX
        MOV             CX, N3                   ;CX=6000h
        ADD             CX, 256
        REP             STOSW                    ;C200 bytes

        ;init segment 3, to zero's
        SetES Seg3
        MOV             DI, AX
        MOV             CX, N3                   ;CX=6000h
        REP             STOSW                    ;C000 bytes

        ;fill last 256 words of Seg3 with one's
        MOV             DI, LEVEL                ;Level offset
        ADD             DI, BX                   ;add size of Level
        MOV             CX, 256                  ;256 words to init
        MOV             AX, 1
        REP             STOSW                    ;init 256 words to 1's

        ;misc inits
        MOV             DS:Avail, 2              ;first avaialable tree pos

        POP             DI
        POP             CX
        POP             BX
        RET
InitSlide       ENDP

;
;***********************************************************ReadBuffer
;
ReadBuffer      PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        PUSH            DS
        POP             ES                       ;ES=DS
        PUSH            BX

        ;copy data to start of text buffer
        MOV             DI, OFFSET DS:TextBuffer
        MOV             BX, DICTIONARYSIZE

        LEA             SI, DS:[DI + BX]
        MOV             BP, SI                   ;BP is our buffer offset
        MOV             CX, MAXMATCH
        ADD             CX, BX                   ;Copy DictionarySize +
        SHR             CX, 1                    ;MaxMatch words
        REP             MOVSW

        MOV             AX, DI                   ;AX=offset of buffer
        CALL            ReadCRC                  ;Read and and compute CRC

        ADD             DS:Remainder, AX         ;how much yet to process
        OR              AX, AX                   ;if not remainder then
        JZ              TP1@008
        CALL            ShowProgress             ;show progress
TP1@008:
        POP             BX
        MOV             AX, DICTIONARYSIZE
        SHL             AX, 1
        MOV             DS:Pos, AX               ;dictionary position
        JMP             DeleteInsert
ReadBuffer      ENDP


GetNextMatch    PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        DEC             DS:Remainder             ;reduce amount remaining
        ADD             DS:Pos, 2                ;adjust dictionary pos
        INC             BP                       ;adjust buffer pointer
        CMP             BP, DS:TextEnd           ;have we looked at all chars
        JE              ReadBuffer               ;in buffer -- if so read more
        ;else, fall through to DeleteInsert
GetNextMatch    ENDP

;
;***********************************************************DeleteInsert
;
DeleteInsert    PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        PUSH            DS

        SetDS EDS
        MOV             DI, DS:Pos

        SetDS Seg2
        CMP             WORD PTR DS:PARENT[DI], 0
        JE              ToInsertNode

        RestoreDS
        MOV             ES, Seg1
        MOV             DS, Seg3
        ASSUME  DS:NOTHING
        MOV             SI, DS:PREV[DI]
        MOV             BX, ES:NEXT[DI]
        MOV             ES:NEXT[SI], BX
        MOV             DS:PREV[BX], SI
        XOR             SI, SI

        RestoreDS
        MOV             ES, Seg3
        MOV             DS, Seg2
        ASSUME  DS:NOTHING
        XCHG            SI, DS:PARENT[DI]
        DEC             BYTE PTR ES:CHILDCOUNT[SI]
        CMP             SI, DICSIZE2
        JAE             ToInsertNode

        MOV             AL, ES:CHILDCOUNT[SI]
        CMP             AL, 1
        JBE             NewNode
ToInsertNode:
        JMP             InsertNode
NewNode:
        SetDS Seg2
        MOV             DI, DS:POSITION[SI]
        AND             DI, 7FFFh
        PUSH            DI                       ;save

        SetDS EDS
        CMP             DI, DS:Pos
        JB              TP1@010

        SUB             DI, DICSIZE2
TP1@010:
        MOV             BX, DI
        SetDS Seg2
        MOV             DI, DS:PARENT[SI]
        MOV             CX, DS:POSITION[DI]
        OR              CX, CX
        JNS             TP1@014
        SHL             CX, 1
TP1@011:
        SHR             CX, 1
        SetDS EDS
        CMP             CX, DS:Pos
        JB              TP1@012
        SUB             CX, DICSIZE2
TP1@012:
        CMP             CX, BX
        JBE             TP1@013
        MOV             BX, CX
TP1@013:
        MOV             CX, BX
        OR              CX, DICSIZE2
        SetDS Seg2
        MOV             DS:POSITION[DI], CX
        MOV             DI, DS:PARENT[DI]
        MOV             CX, DS:POSITION[DI]
        SHL             CX, 1
        JC              TP1@011

        SHR             CX, 1
TP1@014:
        CMP             DI, DICSIZE2
        JAE             TP1@017

        SetDS EDS
        CMP             CX, DS:Pos
        JB              TP1@015
        SUB             CX, DICSIZE2
TP1@015:
        CMP             CX, BX
        JNA             TP1@016
        MOV             BX, CX
TP1@016:
        OR              BX, DICSIZE2
        OR              BX, 8000h
        SetDS Seg2
        MOV             DS:POSITION[DI], BX
TP1@017:
        POP             DI                       ;restore
        MOV             DX, DI

        RestoreDS
        MOV             ES, Seg1
        MOV             DS, Seg2
        ASSUME  DS:NOTHING
        CMP             WORD PTR DS:PARENT[DI], 0
        JNE             TP1@019
TP1@018:
        MOV             DI, ES:NEXT[DI]
        CMP             WORD PTR DS:PARENT[DI], 0
        JE              TP1@018
TP1@019:
        MOV             BX, DI
        MOV             DI, DS:PARENT[BX]
        CMP             DI, SI
        JNE             TP1@019

        SetDS Seg3
        MOV             DX, SI
        MOV             SI, DS:PREV[BX]
        MOV             DI, ES:NEXT[BX]
        MOV             ES:NEXT[SI], DI
        MOV             DS:PREV[DI], SI
        MOV             SI, DX

        MOV             DI, DS:PREV[SI]
        MOV             ES:NEXT[DI], BX
        MOV             DS:PREV[BX], DI

        MOV             DI, ES:NEXT[SI]
        MOV             DS:PREV[DI], BX
        MOV             ES:NEXT[BX], DI

        SetDS Seg2
        MOV             DI, DS:PARENT[SI]
        MOV             DS:PARENT[BX], DI
        MOV             WORD PTR DS:PARENT[SI], 0

        SetDS EDS
        MOV             DX, DS:Avail
        MOV             ES:NEXT[SI], DX
        MOV             DS:Avail, SI
InsertNode:
        SetDS EDS
        MOV             DL, DS:[BP]              ;get a character
        XOR             DH, DH
        SHL             DX, 1
        OR              DX, DICSIZE2
        MOV             AX, DS:MatchLen
        CMP             AX, 4
        JNL             LimitLength
        JMP             ShortMatch
LimitLength:
        DEC             AX                       ;match length - 1
        MOV             SI, DS:MatchPos
        SUB             SI, OFFSET DS:TextBuffer - 1
        SHL             SI, 1
        OR              SI, DICSIZE2             ;mask with 4000h

        SetDS Seg2
        MOV             DI, DS:PARENT[SI]
        OR              DI, DI                   ;DI is offset of parent string
        JNZ             TP1@021
        SetES Seg1
TP1@020:
        MOV             SI, ES:NEXT[SI]
        MOV             DI, DS:PARENT[SI]
        OR              DI, DI
        JZ              TP1@020
TP1@021:
        SetES Seg3
        CMP             AL, BYTE PTR ES:LEVEL[DI];compare match len to
        JA              TP1@023                  ;parents length
TP1@022:
        MOV             SI, DI
        MOV             DI, DS:PARENT[SI]
        CMP             AL, BYTE PTR ES:LEVEL[DI]
        JNA             TP1@022

TP1@023:
        MOV             DX, DI                   ;DI=parents offset into string table
        MOV             BX, DI
        SetDS EDS
        MOV             CX, DS:Pos
        SetDS Seg2
        CMP             WORD PTR DS:POSITION[BX], 0
        JNL             TP1@025
TP1@024:
        MOV             DS:POSITION[BX], CX
        MOV             BX, DS:[BX]
        CMP             WORD PTR DS:POSITION[BX], 0
        JL              TP1@024
TP1@025:
        CMP             BX, DICSIZE2
        JNB             TP1@026
        OR              CX, 8000h
        SetDS Seg2
        MOV             DS:POSITION[BX], CX
TP1@026:
        JMP             MatchNext
ShortMatch:
        MOV             AX, 1
        SetDS EDS
        MOV             DS:MatchLen, AX
InsertLoop:
        SetDS EDS
        MOV             SI, AX
        MOV             BL, DS:[BP + SI]         ;get next character
        MOV             CL, HASH1
        SHL             BX, CL
        ADD             BX, DX
        AND             BX, N2 - 2
        OR              BX, DICSIZE4             ;BX=hash

        RestoreDS
        MOV             ES, Seg1
        MOV             DS, Seg2
        ASSUME  DS:NOTHING
        MOV             DS:PARENT, DX
        MOV             SI, BX
TP1@028:
        MOV             SI, ES:NEXT[SI]
        CMP             DS:PARENT[SI], DX
        JNE             TP1@028

        OR              SI, SI                   ;end of list?
        JNZ             TP1@029

        SetDS EDS
        MOV             DI, DS:Pos
        SetDS Seg2
        MOV             DS:PARENT[DI], DX        ;string dictionary offset
        MOV             SI, DI
        XCHG            SI, ES:NEXT[BX]
        MOV             ES:NEXT[DI], SI

        SetES Seg3
        MOV             ES:PREV[DI], BX
        MOV             ES:PREV[SI], DI
        MOV             DI, DX
        INC             BYTE PTR ES:CHILDCOUNT[DI]

        POP             DS
        RET
TP1@029:
        INC             AX
MatchNext:
        PUSH            SI
        MOV             CX, MAXMATCH
        CMP             SI, DICSIZE2
        JNB             TP1@030

        RestoreDS
        MOV             ES, Seg3
        MOV             DS, Seg2
        ASSUME  DS:NOTHING
        MOV             CL, ES:LEVEL[SI]
        XOR             CH, CH
        MOV             SI, DS:POSITION[SI]
        AND             SI, 7FFFh
TP1@030:                                         ;SI=MatchPos AX=MatchLen
        SHR             SI, 1
        SetDS EDS
        ADD             SI, OFFSET DS:TextBuffer
        CMP             SI, BP
        JB              TP1@031
        SUB             SI, DICTIONARYSIZE
TP1@031:
        MOV             DS:MatchPos, SI
        SUB             CX, AX
        JE              TP1@032

        PUSH            DS
        POP             ES                       ;ES=EDS
        MOV             DI, BP
        ADD             DI, AX
        ADD             SI, AX
        ADD             AX, CX
        REPE            CMPSB
TP1@032:
        POP             SI
        JE              TP1@033

        INC             CX
        SUB             AX, CX
TP1@033:
        MOV             DS:MatchLen, AX
        JNE             DivNode

        CMP             AX, MAXMATCH
        JE              TP1@034

        MOV             CX, DS:Pos
        SetDS Seg2
        MOV             DS:POSITION[SI], CX
        MOV             DX, SI
        JMP             InsertLoop
TP1@034:
        RestoreDS
        MOV             ES, Seg3
        MOV             DS, EDS
        ASSUME  DS:NOTHING

        MOV             DI, DS:Pos
        MOV             BX, ES:PREV[SI]
        MOV             ES:PREV[DI], BX
        SetDS Seg1
        MOV             DS:NEXT[BX], DI
        MOV             BX, DS:NEXT[SI]
        MOV             DS:NEXT[DI], BX
        MOV             ES:PREV[BX], DI

        SetES Seg2
        MOV             ES:PARENT[DI], DX
        MOV             WORD PTR ES:PARENT[SI], 0
        MOV             DS:NEXT[SI], DI
        POP             DS
        RET
DivNode:
        SetDS EDS
        MOV             DI, DS:Avail
        MOV             CX, DS:Pos

        SetES Seg1
        MOV             BX, ES:NEXT[DI]
        MOV             DS:Avail, BX             ;next available tree pos

        SetDS Seg2
        MOV             DS:POSITION[DI], CX
        MOV             DS:PARENT[DI], DX
        MOV             BL, AL
        MOV             BH, 2

        SetDS Seg3
        MOV             WORD PTR DS:LEVEL[DI], BX
        MOV             BX, DS:PREV[SI]
        MOV             DS:PREV[DI], BX
        MOV             ES:NEXT[BX], DI

        MOV             BX, ES:NEXT[SI]
        MOV             ES:NEXT[DI], BX
        MOV             DS:PREV[BX], DI

        SetDS EDS
        MOV             BX, DS:MatchPos
        ADD             BX, AX
        MOV             BL, DS:[BX]
        MOV             CL, HASH1
        SHL             BX, CL
        ADD             BX, DI
        AND             BX, N2 - 2
        OR              BX, DICSIZE4

        PUSH            DI
        SetDS Seg3
        MOV             DI, ES:NEXT[BX]
        MOV             ES:NEXT[BX], SI
        MOV             ES:NEXT[SI], DI
        MOV             DS:PREV[SI], BX
        MOV             DS:PREV[DI], SI
        POP             DI

        SetDS Seg2
        MOV             DS:PARENT[SI], DI
        MOV             SI, AX

        SetDS EDS
        MOV             BL, DS:[BP + SI]
        MOV             CL, HASH1
        SHL             BX, CL
        ADD             BX, DI
        AND             BX, N2 - 2
        OR              BX, DICSIZE4
        MOV             SI, DS:Pos

        RestoreDS
        MOV             ES, Seg1
        MOV             DS, Seg2
        ASSUME  DS:NOTHING
        MOV             DS:PARENT[SI], DI
        MOV             DX, DI

        SetDS Seg3
        MOV             DI, SI
        XCHG            DI, ES:NEXT[BX]
        MOV             ES:NEXT[SI], DI

        MOV             DS:PREV[SI], BX
        MOV             DS:PREV[DI], SI

        POP             DS
        RET
DeleteInsert    ENDP

;
;***********************************************************LhaEncode
;
; Call Tree:
;
; LhaEncode
; ÃÄInitSlide
; ÃÄnlFreezeStart
; ³ ÀÄInitPutBits
; ÃÄReadCRC
; ³ ÀÄCalculateCRC
; ÃÄShowProgress
; ÃÄDeleteInsert
; ÃÄGetNextMatchÄÄReadBufferÄÄDeleteInsert
; ³               ÃÄReadCRC
; ³               ÀÄShowProgress
; ÃÄnlFreezeOutput
; ³ ÀÄSendBlock
; ³   ÃÄMakeTree
; ³   ³ ÃÄDownHeap
; ³   ³ ÀÄCountLength
; ³   ³   ÀÄCountLength
; ³   ÃÄPutBitsÄÄPutCode
; ³   ³          ÀÄlhaPutC
; ³   ÃÄCountTFreq
; ³   ÃÄWritePTLength
; ³   ³ ÀÄPutBitsÄÄPutCode
; ³   ³            ÀÄlhaPutC
; ³   ÃÄWriteCLength
; ³   ³   PutBitsÄÄPutCode
; ³   ³            ÀÄlhaPutC
; ³   ÀÄPuteCode
; ³     ÀÄlhaPutC
; ÀÄnlFreezeEnd
;   ÃÄSendBlock
;   ³ ÀÄSame as SendBlock called by nlFreezeOutput
;   ÀÄPutCode
;
;
LhaEncode       PROC FAR
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        SaveAllRegs
        SetDS EDS

        MOV             DS:InitialSP, SP  ;save the stack pointer
        XOR             AX, AX
        MOV             WORD PTR DS:BytesProcessed, AX
        MOV             WORD PTR DS:BytesProcessed + 2, AX
        MOV             DS:UnPackable, AX
        RestoreDS
        MOV             DS:zCRC, AX
        MOV             DS, EDS
        ASSUME  DS:NOTHING

        CALL            InitSlide
        CALL            nlFreezeStart

        MOV             CX, DICTIONARYSIZE
        MOV             AX, CX
        ADD             AX, MAXMATCH
        MOV             BP, OFFSET DS:TextBuffer
        ADD             BP, AX
        SHL             AX, 1
        MOV             DS:Pos, AX

        PUSH            DS
        POP             ES
        MOV             BX, CX                   ;2000h
        MOV             DI, BP                   ;
        SHR             CX, 1                    ;adjust for word access
        MOV             AX, '  '
        REP             STOSW                    ;fill with spaces

        MOV             AX, BP
        CALL            ReadCRC

        MOV             DS:Remainder, AX
        OR              AX, AX
        JZ              TP1@035
        CALL            ShowProgress
TP1@035:
        MOV             DS:MatchLen, 0
        CALL            DeleteInsert

        MOV             AX, DS:Remainder
        CMP             DS:MatchLen, AX
        JNA             TP1@036
        MOV             DS:MatchLen, AX
TP1@036:
        CMP             DS:Remainder,0
        JE              TP1@042

        MOV             AX, DS:MatchLen
        MOV             DS:LastMatchLen, AX
        MOV             AX, DS:MatchPos
        MOV             DS:LastMatchPos, AX
        CALL            GetNextMatch

        MOV             AX, DS:Remainder
        CMP             DS:MatchLen, AX
        JNA             TP1@037

        MOV             DS:MatchLen, AX
TP1@037:
        MOV             AX, DS:LastMatchLen
        CMP             DS:MatchLen, AX
        JA              TP1@038

        CMP             AX, THRESHOLD
        JNB             TP1@039
TP1@038:
        MOV             AL, DS:[BP - 1]
        XOR             AH, AH
        CALL            nlFreezeOutput

        JMP             SHORT TP1@041
TP1@039:
        MOV             BX, BP
        SUB             BX, DS:LastMatchPos
        SUB             BX, 2
        AND             BX, DICSIZE1
        ADD             AX, 256 - THRESHOLD
        CALL            nlFreezeOutput
        DEC             DS:LastMatchLen
MatchLoop:
        CALL            GetNextMatch
        DEC             DS:LastMatchLen
        JNZ             MatchLoop

        MOV             AX, DS:Remainder
        CMP             DS:MatchLen, AX
        JNA             TP1@041

        MOV             DS:MatchLen, AX
TP1@041:
        CMP             DS:UnPackable, 0
        JE              TP1@036
TP1@042:
        CALL            nlFreezeEnd

        RestoreAllRegs
        RET
LhaEncode       ENDP

;
;***********************************************************LhaDecode
;
; Call Tree:
;
; LhaDecode
; ÃÄnlMeltStartÄÄInitGetBitsÄÄFillBuffer
; ³                           ÀÄlhaGetC
; ÃÄnlMeltCharacter
; ³ ÃÄFillBuffer
; ³ ³ ÀÄlhaGetC
; ³ ÃÄGetBits
; ³ ³ ÀÄFillBuffer
; ³ ³   ÀÄlhaGetC
; ³ ÃÄReadPTLength
; ³ ³ ÃÄGetBits
; ³ ³ ³ ÀÄFillBuffer
; ³ ³ ³   ÀÄlhaGetC
; ³ ³ ÃÄFillBuffer
; ³ ³ ³ ÀÄlhaGetC
; ³ ³ ÀÄMakeTable
; ³ ÀÄReadCLength
; ³   ÃÄGetBits
; ³   ³ ÀÄFillBuffer
; ³   ³   ÀÄlhaGetC
; ³   ÃÄFillBuffer
; ³   ³ ÀÄlhaGetC
; ³   ÀÄMakeTable
; ÃÄnlMeltPosition
; ³ ÃÄGetBits
; ³ ³ ÀÄFillBuffer
; ³ ³   ÀÄlhaGetC
; ³ ÀÄFillBuffer
; ³   ÀÄlhaGetC
; ÃÄWriteCRC
; ³ ÀÄCalculateCRC
; À ShowProgress2ÄÄShowProgress
;
;
LhaDecode       PROC FAR
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        SaveAllRegs
        SetDS EDS
        MOV             DS:InitialSP, SP  ;save the stack pointer

        ;initialize BytesProcessed and zCRC values
        XOR             AX, AX
        MOV             WORD PTR DS:BytesProcessed, AX
        MOV             WORD PTR DS:BytesProcessed + 2, AX
        RestoreDS
        MOV             DS:zCRC, AX
        MOV             DS, EDS
        ASSUME  DS:NOTHING

        PUSH            DS
        POP             ES                       ;ES=DS

        MOV             DX, 100h - 3             ;?

        ;initialize TextBuffer to all spaces
        MOV             DI, OFFSET DS:TextBuffer
        MOV             CX, N1
        MOV             AX, '  '
        REP             STOSW

        ;initialize bit buffers and get first character
        CALL            nlMeltStart

        MOV             DI, OFFSET DS:TextBuffer
        MOV             BP, DICTIONARYSIZE
        JMP             TP@Entry
TP1@044:
        ;decode the character
        CALL            nlMeltCharacter

        ;high byte must indicate further processing needed
        OR              AH, AH
        JNZ             TP1@046
        STOSB

        ;is the output buffer full
        CMP             DI, OFFSET DS:TextBuffer[N2]
        JNE             TP@Entry

        ;yes, buffer is full - write it
        MOV             DI, OFFSET DS:TextBuffer
        MOV             AX, DI
        MOV             BX, N2
        CALL            WriteCRC
TP@Entry:
        ;decrease the original size
        RestoreDS
        SUB             WORD PTR DS:OrigSize, 1
        SBB             WORD PTR DS:OrigSize + 2, 0
        MOV             DS, EDS
        ASSUME  DS:NOTHING

        DEC             BP
        JC              TP@EndLoop      ;if OrigSize < 0 then were done
        JNS             TP1@044

        CALL            ShowProgress2
        JMP             TP1@044
TP1@046:
        MOV             CX, AX
        SUB             CX, DX
        MOV             AX, DI
        ;
        SUB             AX, OFFSET DS:TextBuffer
        CALL            nlMeltPosition
        MOV             SI, DI
        SUB             SI, AX
        DEC             SI
        SUB             BP, CX
        JNS             TP1@047
        CALL            ShowProgress2
TP1@047:
        MOV             AX, DI
        ADD             AX, CX

        PUSH            CX
        SUB             AX, OFFSET DS:TextBuffer[N2]
        JB              TP1@048
        SUB             CX, AX
        REP             MOVSB

        PUSH            AX
        MOV             DI, OFFSET DS:TextBuffer
        MOV             AX, DI
        MOV             BX, N2
        CALL            WriteCRC
        POP             CX

        JMP             SHORT TP1@049
TP1@048:
        CMP             SI, OFFSET DS:TextBuffer
        JNS             TP1@049
        ADD             SI, N2
TP1@049:
        MOV             AX, SI
        ADD             AX, CX
        SUB             AX, OFFSET DS:TextBuffer[N2]
        JB              TP1@050
        SUB             CX, AX
        REP             MOVSB

        MOV             CX, AX
        MOV             SI, OFFSET DS:TextBuffer
TP1@050:
        REP             MOVSB
        POP             CX

        ;decrease the original size
        RestoreDS
        SUB             WORD PTR DS:OrigSize, CX
        SBB             WORD PTR DS:OrigSize + 2, 0
        JC              TP@EndLoop
        MOV             DS, EDS
        ASSUME  DS:NOTHING
TP1@051:
        JMP             TP1@044
TP@EndLoop:
        SetDS EDS

        ;write the remaining bytes
        MOV             AX, OFFSET DS:TextBuffer
        MOV             BX, DI
        SUB             BX, AX
        CALL            WriteCRC

        INC             BP
        CMP             BP, DICTIONARYSIZE
        JE              TP1@052
        CALL            ShowProgress2
TP1@052:
        RestoreAllRegs
        RET
LhaDecode       ENDP

;
;***********************************************************CountTFreq
;
CountTFreq      PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        PUSH            CX
        PUSH            SI
        PUSH            DI
        CLD

        PUSH            DS
        POP             ES
        MOV             CX, NT
        MOV             DI, OFFSET DS:TFreq
        XOR             AX, AX
        REP             STOSW
        MOV             CX, NC
        MOV             DI, OFFSET DS:CLength[NC - 1]
        STD
        REPE            SCASB
        CLD
        INC             DI
        MOV             SI, DI
        MOV             DI, OFFSET DS:CLength
TP2@001:
        MOV             BL, DS:[DI]
        INC             DI
        OR              BL, BL
        JNZ             TP2@006

        MOV             CX, -1
        REPE            SCASB
        DEC             DI
        NOT             CX

        CMP             CX, 2
        JA              TP2@002

        ADD             DS:TFreq[0 * 2], CX
        JMP             TP2@005
TP2@002:
        CMP             CX, 19
        JA              TP2@004

        JNE             TP2@003
        INC             DS:TFreq[0 * 2]
TP2@003:
        INC             DS:TFreq[1 * 2]
        JMP             TP2@005
TP2@004:
        INC             DS:TFreq[2 * 2]
TP2@005:
        JMP             TP2@007
TP2@006:
        XOR             BH, BH
        SHL             BX, 1
        INC             DS:TFreq[BX + 2 * 2]
TP2@007:
        CMP             DI, SI
        JNA             TP2@001

        POP             DI
        POP             SI
        POP             CX
        RET
CountTFreq      ENDP

;
;***********************************************************WritePTLength
;
WritePTLength   PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        PUSH            CX
        PUSH            SI
        PUSH            DI

        PUSH            CX
        PUSH            BX
        MOV             SI, OFFSET DS:PTLength
        LEA             DI, DS:[SI - 1]
        ADD             DI, AX
        MOV             CX, AX
        INC             CX
        XOR             AL, AL
        STD
        REPE            SCASB

        CLD
        MOV             BX, CX
        POP             AX
        CALL            PutBits

        INC             DI
        POP             CX
        ADD             CX, SI
TP2@008:
        XOR             BH, BH
        MOV             BL, DS:[SI]
        INC             SI
        CMP             BL, 6
        JA              TP2@009

        MOV             AL, 3
        JMP             TP2@010
TP2@009:
        MOV             AX, BX
        SUB             AX, 3
        MOV             BX, 0FFFEh
TP2@010:
        CALL            PutBits

        CMP             SI, CX
        JNE             TP2@013
TP2@011:
        CMP             SI, OFFSET DS:PTLength + 6
        JNB             TP2@012

        CMP             BYTE PTR DS:[SI], 0
        JNE             TP2@012

        INC             SI
        JMP             TP2@011
TP2@012:
        MOV             AL, 2
        MOV             BX, SI
        SUB             BX, OFFSET DS:PTLength + 3
        CALL            PutBits
TP2@013:
        CMP             SI, DI
        JNA             TP2@008

        POP             DI
        POP             SI
        POP             CX
        RET
WritePTLength   ENDP

;
;***********************************************************WriteCLength
;
WriteCLength    PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        PUSH            CX
        PUSH            SI
        PUSH            DI
        MOV             DI, OFFSET DS:CLength + NC - 1
        MOV             CX, NC + 1
        XOR             AL, AL
        STD
        REPE            SCASB

        CLD
        INC             DI
        MOV             SI, DI
        MOV             DI, OFFSET DS:CLength
        MOV             BX, CX
        MOV             AL, CBIT
        CALL            PutBits
TP2@014:
        MOV             BL, DS:[DI]
        INC             DI
        OR              BL, BL
        JNZ             TP2@019

        XOR             AL, AL
        MOV             CX, -1
        REPE            SCASB

        DEC             DI
        NOT             CX

        CMP             CX, 2
        JA              TP2@016
TP2@015:
        MOV             AL, DS:PTLength[0]
        MOV             BX, DS:PTCode[0 * 2]
        CALL            PutCode

        LOOP            TP2@015

        JMP             TP2@020
TP2@016:
        CMP             CX, 19
        JA              TP2@018
        JNE             TP2@017
        MOV             AL, DS:PTLength[0]
        MOV             BX, DS:PTCode[0 * 2]
        CALL            PutCode

        DEC             CX
TP2@017:
        MOV             AL, DS:PTLength[1]
        MOV             BX, DS:PTCode[1 * 2]
        CALL            PutCode

        MOV             AL, 4
        MOV             BX, CX
        SUB             BX, 3
        CALL            PutBits

        JMP             TP2@020
TP2@018:
        MOV             AL, DS:PTLength[2]
        MOV             BX, DS:PTCode[2 * 2]
        CALL            PutCode

        MOV             AL, CBIT
        MOV             BX, CX
        SUB             BX, 20
        CALL            PutBits

        JMP             TP2@020
TP2@019:
        XOR             BH, BH
        MOV             AL, DS:PTLength[BX + 2]
        SHL             BX, 1
        MOV             BX, DS:PTCode[BX + 4]
        CALL            PutCode
TP2@020:
        CMP             DI, SI
        JA              TP2@021
        JMP             TP2@014
TP2@021:
        POP             DI
        POP             SI
        POP             CX
        RET
WriteCLength    ENDP

;
;***********************************************************SendBlock
;
SendBlock       PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        PUSH            CX
        PUSH            DX
        PUSH            SI
        PUSH            DI
        PUSH            BP

        MOV             AX, NC
        MOV             BX, OFFSET DS:CFreq
        MOV             CX, OFFSET DS:CLength
        MOV             DX, OFFSET DS:CCode
        CALL            MakeTree

        MOV             BX, AX
        MOV             CX, AX
        SHL             BX, 1
        MOV             BX, DS:CFreq[BX]
        MOV             AL, 16
        CALL            PutBits

        CMP             CX, NC
        JB              TP2@024

        CALL            CountTFreq

        MOV             AX, NT
        MOV             BX, OFFSET DS:TFreq
        MOV             CX, OFFSET DS:PTLength
        MOV             DX, OFFSET DS:PTCode
        CALL            MakeTree

        CMP             AX, NT
        JB              TP2@022

        MOV             AX, NT
        MOV             BX, TBIT
        MOV             CX, 3
        CALL            WritePTLength

        JMP             TP2@023
TP2@022:
        MOV             BX, AX
        MOV             AL, TBIT * 2
        CALL            PutBits
TP2@023:
        CALL            WriteCLength
        JMP             TP2@025
TP2@024:
        XOR             BX, BX
        MOV             AL, CBIT + TBIT
        CALL            PutBits

        MOV             BX, CX
        MOV             AL, CBIT + TBIT
        CALL            PutBits
TP2@025:
        MOV             AX, NP
        MOV             BX, OFFSET DS:PFreq
        MOV             CX, OFFSET DS:PTLength
        MOV             DX, OFFSET DS:PTCode
        CALL            MakeTree

        CMP             AX, NP
        JB              TP2@026

        MOV             AX, NP
        MOV             BX, PBIT
        MOV             CX, -1
        CALL            WritePTLength

        JMP             TP2@027
TP2@026:
        MOV             BX, AX
        MOV             AL, PBIT * 2
        CALL            PutBits
TP2@027:
        RestoreDS
        LES             SI, DS:Buf               ;ES:SI=Buf
        MOV             DS, EDS
        ASSUME  DS:NOTHING

        XOR             CX, CX
        MOV             BP, WORD PTR DS:OutPutPos ;get current offset pos
TP2@028:
        AND             CL, 07h
        JNZ             TP2@029
        LODS            BYTE PTR ES:[SI]
        MOV             CH, AL
TP2@029:
        XOR             BH, BH
        LODS            BYTE PTR ES:[SI]

        MOV             BL, AL
        ADD             CH, 80h
        RCL             BH, 1
        MOV             AL, DS:CLength[BX]
        SHL             BX, 1
        MOV             BX, DS:CCode[BX]
        CALL            PutCode

        SHL             CH, 1
        JC              TP2@030

        LODS            WORD PTR ES:[SI]
        MOV             DX, AX
        AND             AX, 0000Fh
        MOV             DI, AX
        MOV             AL, DS:PTLength[DI]
        SHL             DI, 1
        MOV             BX, DS:PTCode[DI]
        CALL            PutCode
        SHR             DI, 1

        DEC             DI
        JNG             TP2@030
        MOV             AX, DI
        MOV             BX, DX
        AND             BL, 0f0h
        CALL            PutCode
TP2@030:
        INC             CX
        CMP             SI, BP
        JB              TP2@028

        PUSH            DS
        POP             ES
        XOR             AX, AX
        MOV             CX, NC
        MOV             DI, OFFSET DS:CFreq
        REP             STOSW

        MOV             CX, NP
        MOV             DI, OFFSET DS:PFreq
        REP             STOSW

        POP             BP
        POP             DI
        POP             SI
        POP             DX
        POP             CX
        RET
SendBlock       ENDP

;
;***********************************************************nlFreezeOutput
;
nlFreezeOutPut  PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        PUSH            DI
        LES             DI, DS:OutPutPos         ;ES:DI=current buffer pos

        ROR             DS:OutPutMask, 1
        JNC             TP2@032
        RestoreDS
        CMP             DI, DS:BufLimit
        MOV             DS, EDS
        ASSUME  DS:NOTHING

        JB              TP2@031
        PUSH            AX
        PUSH            BX
        PUSH            ES
        CALL            SendBlock
        POP             ES
        POP             BX
        POP             AX
        CMP             DS:UnPackable, 0
        JNE             OS1_End
        XOR             DI, DI
TP2@031:
        MOV             DS:CPos, DI
        MOV             BYTE PTR ES:[DI], 0
        INC             DI
TP2@032:
        STOSB
        XCHG            BX, AX
        SHL             BX, 1
        INC             DS:CFreq[BX]
        SHR             BH, 1
        JZ              TP2@033

        XOR             BX, BX
        SHL             AX, 1
        JZ              Keta1
        SHL             AX, 1
        SHL             AX, 1
        MOV             BX, 13 + 1
Keta0:
        DEC             BX
        SHL             AX, 1
        JC              keta1

        DEC             BX
        SHL             AX, 1
        JC              keta1

        DEC             BX
        SHL             AX, 1
        JC              keta1

        DEC             BX
        SHL             AX, 1
        JC              keta1

        DEC             BX
        SHL             AX, 1
        JNC             keta0
Keta1:
        OR              AX, BX
        STOSW

        SHL             BX, 1
        INC             DS:PFreq[BX]
        MOV             AL, DS:OutPutMask
        MOV             BX, DS:CPos
        OR              ES:[BX], AL
TP2@033:
        MOV             WORD PTR DS:OutPutPos, DI
OS1_End:
        POP    DI
        RET
nlFreezeOutPut  ENDP

;
;***********************************************************nlFreezeStart
;
nlFreezeStart   PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        PUSH            CX
        PUSH            DI

        PUSH            DS
        POP             ES
        XOR             AX, AX
        MOV             CX, NC
        MOV             DI, OFFSET DS:CFreq
        REP             STOSW

        MOV             CX, NP
        MOV             DI, OFFSET DS:PFreq
        REP             STOSW

        RestoreDS
        LES             BX, DS:Buf
        MOV             DS, EDS
        ASSUME  DS:NOTHING

        MOV             WORD PTR DS:OutPutPos + 2, ES
        MOV             WORD PTR DS:OutPutPos, BX
        MOV             BYTE PTR ES:[BX], 0
        MOV             DS:OutPutMask, 01h
        CALL            InitPutBits

        POP             DI
        POP             CX
        RET
nlFreezeStart   ENDP

;
;***********************************************************nlFreezeEnd
;
nlFreezeEnd     PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        CMP             DS:UnPackable, 0
        JNE             TP2@034
        CALL            SendBlock
        MOV             AX, 7
        XOR             BX, BX
        CALL            PutCode
TP2@034:
        RET
nlFreezeEnd     ENDP

;
;***********************************************************ReadPTLength
;
ReadPTLength    PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        PUSH            CX
        PUSH            DX
        PUSH            SI
        PUSH            DI

        PUSH            AX
        MOV             SI, AX
        MOV             DX, BX
        MOV             AX, BX
        CALL            GetBits
        CMP             AX, SI
        JNA             TP2@035

        MOV             AX, ECBADFILEFORMAT
        JMP             Abort                    ;exit with error
TP2@035:
        MOV             DI, OFFSET DS:PTLength
        OR              AX, AX
        JNZ             TP2@036

        MOV             DI, OFFSET DS:PTLength
        POP             CX                       ;CX=AX
        REP             STOSB

        MOV             AX, DX
        CALL            GetBits
        MOV             CX, 256
        MOV             DI, OFFSET DS:PTTable
        REP             STOSW
        JMP             TP2@040
TP2@036:
        MOV             DX, CX
        ADD             DX, DI
        MOV             SI, DI
        ADD             SI, AX
TP2@037:
        MOV             AX, 3
        CALL            GetBits

        CMP             AL, 7
        JNE             TP2@038

        MOV             BX, DS:[BitBuf]
DoWhile:
        SHL             BX, 1
        JNC             OutW

        INC             AX
        JMP             DoWhile
OutW:
        PUSH            AX
        SUB             AX, 6
        CALL            FillBuffer
        POP             AX
TP2@038:
        STOSB
        CMP             DI, DX
        JNE             TP2@039

        MOV             AX, 2
        CALL            GetBits

        MOV             CX, AX
        XOR             AL, AL
        REP             STOSB
TP2@039:
        CMP             DI, SI
        JB              TP2@037

        POP             CX
        MOV             SI, CX
        ADD             CX, OFFSET DS:PTLength
        SUB             CX, DI

        JAE             PTLOk1
        MOV             AX, ECBADFILEFORMAT
        JMP             Abort                    ;exit with error
PTLOk1:
        XOR             AL, AL
        REP             STOSB

        MOV             AX, SI
        MOV             BX, OFFSET DS:PTLength
        MOV             CX, 8
        MOV             DX, OFFSET DS:PTTable
        CALL            MakeTable
TP2@040:
        POP             DI
        POP             SI
        POP             DX
        POP             CX
        RET
ReadPTLength    ENDP

;
;***********************************************************ReadCLength
;
ReadCLength     PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        PUSH            CX
        PUSH            DX
        PUSH            DI

        MOV             AX, CBIT
        CALL            GetBits
        CMP             AX, NC

        JNA             RCLOk1
        MOV             AX, ECBADFILEFORMAT
        JMP             Abort                    ;exit with error
RCLOk1:
        MOV             DI, OFFSET DS:CLength
        OR              AX, AX
        JNZ             TP2@041

        MOV             CX, NC
        REP             STOSB

        MOV             AX, CBIT
        CALL            GetBits

        MOV             CX, 4096
        MOV             DI, OFFSET DS:CTable
        REP             STOSW

        JMP             TP2@052
TP2@041:
        MOV             DX, DI
        ADD             DX, AX
TP2@042:
        MOV             AX, DS:BitBuf
        MOV             BL, AH
        XOR             BH, BH
        SHL             BX, 1
        MOV             BX, DS:PTTable[BX]
TP2@043:
        CMP             BX, NT
        JB              TP2@046

        SHL             AL, 1
        JNC             TP2@044

        MOV             BX, DS:Right[BX]
        JMP             TP2@045
TP2@044:
        MOV             BX, DS:Left[BX]
TP2@045:
        JMP             TP2@043
TP2@046:
        PUSH            BX
        MOV             AL, DS:PTLength[BX]
        CALL            FillBuffer
        POP             AX
        SUB             AX, 2
        JA              TP2@050
        JNZ             TP2@047

        MOV             AX, CBIT
        CALL            GetBits

        ADD             AX, 20
        MOV             CX, AX
        JMP             TP2@049
TP2@047:
        INC             AX
        JNZ             TP2@048

        MOV             AX, 4
        CALL            GetBits

        ADD             AX,3
        MOV             CX, AX
        JMP             TP2@049
TP2@048:
        MOV             CX, 1
TP2@049:
        XOR             AL, AL
        REP             STOSB
        JMP            TP2@051
TP2@050:
        STOSB
TP2@051:
        CMP             DI, DX
        JB              TP2@042
        MOV             CX, OFFSET DS:CLength + NC
        SUB             CX, DI
        JNC             AfterTE
        MOV             AX, ECBADFILEFORMAT
        JMP             Abort                    ;exit with error
AfterTE:
        XOR             AL, AL
        REP             STOSB

        MOV             AX, NC
        MOV             BX, OFFSET DS:CLength
        MOV             CX, 12
        MOV             DX, OFFSET DS:CTable
        CALL            MakeTable
TP2@052:
        POP             DI
        POP             DX
        POP             CX
        RET
ReadCLength     ENDP

;
;***********************************************************nlMeltCharacter
;
nlMeltCharacter PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        PUSH            CX
        SUB             DS:[BlockSize], 1
        JC              nlMeltCharacter2
nlMeltCharacter3:
        MOV             BX, DS:[BitBuf]
        MOV             CL, 4
        SHR             BX, CL
        SHL             BX, 1
        MOV             BX, DS:CTable[BX]
        CMP             BX, NC
        JNB             TP2@053
nlMeltCharacter1:
        PUSH            BX
        MOV             AL, DS:CLength[BX]
        CALL            FillBuffer

        POP             AX
        POP             CX
        RET
TP2@053:
        MOV             AX, DS:[BitBuf]
        SHL             AL, CL
TP2@054:
        SHL             AL, 1
        JNC             TP2@055

        MOV             BX, DS:Right[BX]
        JMP             TP2@056
TP2@055:
        MOV             BX, DS:Left[BX]
TP2@056:
        CMP             BX, NC
        JNB             TP2@054

        JMP             nlMeltCharacter1
nlMeltCharacter2:
        PUSH            AX
        PUSH            BX
        PUSH            CX
        MOV             AX, 16
        CALL            GetBits
        DEC             AX
        MOV             DS:[BlockSize], AX
        MOV             AX, NT
        MOV             BX, TBIT
        MOV             CX, 3
        CALL            ReadPTLength
        CALL            ReadCLength
        MOV             AX, NP
        MOV             BX, PBIT
        MOV             CX, -1
        CALL            ReadPTLength
        POP             CX
        POP             BX
        POP             AX
        JMP             nlMeltCharacter3
nlMeltCharacter ENDP

;
;***********************************************************nlMeltPosition
;
nlMeltPosition  PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        PUSH            CX
        XOR             BH, BH
        MOV             BL, BYTE PTR DS:[BitBuf + 1]
        SHL             BX, 1
        MOV             BX, DS:PTTable[BX]
        CMP             BX, NP
        JNB             TP2@058
nlMeltPosition1:
        PUSH            BX
        MOV             AL, DS:PTLength[BX]
        CALL            FillBuffer
        POP             AX
        OR              AX, AX
        JZ              TP2@057
        DEC             AX
        MOV             CX, AX
        CALL            GetBits
        MOV             BX, 1
        SHL             BX, CL
        OR              AX, BX
TP2@057:
        POP             CX
        RET
TP2@058:
        MOV             AL, BYTE PTR DS:BitBuf
TP2@059:
        SHL             AL, 1
        JNC             TP2@060
        MOV             BX, DS:Right[BX]
        JMP             TP2@061
TP2@060:
        MOV             BX, DS:Left[BX]
TP2@061:
        CMP             BX, NP
        JNB             TP2@059
        JMP             nlMeltPosition1
nlMeltPosition  ENDP

;
;***********************************************************nlMeltStart
;
nlMeltStart     PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        MOV             DS:BlockSize, 0
        JMP             InitGetBits
nlMeltStart     ENDP

;
;***********************************************************CountLength
;
CountLength     PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        CMP             BX, DS:LzhN2
        JNB             TP4@002

        MOV             BX, DS:Depth
        CMP             BX, 16 * 2
        JNA             TP4@001

        MOV             BX, 16 * 2
TP4@001:
        INC             DS:LengthCount[BX]
        RET
TP4@002:
        ADD             DS:Depth, 2
        PUSH            BX
        MOV             BX, DS:Left[BX]
        CALL            CountLength

        POP             BX
        PUSH            BX
        MOV             BX, DS:Right[BX]
        CALL            CountLength

        POP             BX
        SHR             DS:Left[BX], 1
        SHR             DS:Right[BX], 1
        SUB             DS:Depth, 2
        RET
CountLength     ENDP

;
;***********************************************************DownHeap
;
DownHeap        PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        PUSH            CX
        PUSH            DX
        PUSH            SI
        PUSH            DI
        PUSH            BP

        MOV             SI, DS:Heap[BX]
        PUSH            SI
        MOV             BP, DS:Freq2
        MOV             CX, DS:[BP + SI]
        MOV             SI, BX
TP4@003:
        SHL             SI, 1
        CMP             SI, DS:HeapSize
        JA              TP4@005

        MOV             DI, DS:Heap[SI]
        MOV             AX, DS:[BP + DI]
        JNB             TP4@004

        MOV             DI, DS:Heap[SI + 2]
        MOV             DX, DS:[BP + DI]
        CMP             AX, DX
        JNA             TP4@004

        MOV             AX, DX
        ADD             SI, 2
TP4@004:
        CMP             CX, AX
        JBE             TP4@005

        MOV             DI, DS:Heap[SI]
        MOV             DS:Heap[BX], DI
        MOV             BX, SI
        JMP             TP4@003
TP4@005:
        POP             DS:Heap[BX]

        POP             BP
        POP             DI
        POP             SI
        POP             DX
        POP             CX
        RET
DownHeap        ENDP

;
;***********************************************************MakeTree
;
MakeTree        PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        PUSH            CX
        PUSH            DX
        PUSH            SI
        PUSH            DI
        PUSH            BP

        MOV             DS:NChar, AX
        MOV             DS:Freq2, BX
        MOV             DS:BitLen, CX
        MOV             DS:Code2, DX
        SHL             AX, 1
        MOV             DS:LzhN2, AX
        MOV             DS:AvailableMT, AX
        XOR             AX, AX
        CLD
        PUSH            DS
        POP             ES
        MOV             DI, CX
        MOV             CX, DS:NChar
        REP             STOSB

        MOV             CX, DS:NChar
        MOV             DI, DS:Freq2
        LEA             DX, DS:2[DI]
        MOV             BX, OFFSET DS:Heap
        JMP             MakeTree1
TP4@006:
        ADD             BX, 2
        MOV             DS:[BX], DI
        SUB             DS:[BX], DX
MakeTree1:
        XOR             AX, AX                   ;set zero flag
        REPZ            SCASW
        JNZ             TP4@006

        SUB             BX, OFFSET DS:Heap
        MOV             DS:HeapSize, BX
        CMP             BX, 4
        JNB             TP4@007

        MOV             AX, DS:Heap[2]
        MOV             DI, AX
        ADD             DI, DS:Code2
        MOV             WORD PTR DS:[DI], 0
        JMP             Return
TP4@007:
        SHR             BX, 1
        AND             BX, 0FFFEh
TP4@008:
        PUSH            BX
        CALL            DownHeap
        POP             BX
        SUB             BX, 2
        JNZ             TP4@008

        MOV             DI, DS:Code2
TP4@009:
        MOV             SI, DS:Heap[2]
        CMP             SI, DS:LzhN2
        JNB             TP4@010

        MOV             DS:[DI], SI
        ADD             DI, 2
TP4@010:
        MOV             BX, DS:HeapSize
        SUB             DS:HeapSize, 2
        MOV             AX, DS:Heap[BX]
        MOV             DS:Heap[2], AX
        MOV             BX, 2
        CALL            DownHeap

        MOV             DX, DS:Heap[2]
        CMP             DX, DS:LzhN2
        JNB             TP4@011

        MOV             DS:[DI], DX
        ADD             DI, 2
TP4@011:
        MOV             BX, DS:AvailableMT
        ADD             DS:AvailableMT, 2
        MOV             DS:Left[BX], SI
        MOV             DS:Right[BX], DX
        MOV             DS:Heap[2], BX
        MOV             BP, DS:Freq2
        MOV             AX, DS:[SI + BP]
        MOV             SI, DX
        ADD             AX, DS:[SI + BP]
        MOV             DX, BX
        ADD             BX, BP
        MOV             DS:[BX], AX
        MOV             BX, 2
        CALL            DownHeap

        CMP             DS:HeapSize, 2
        JA              TP4@009

        MOV             BX, DX
        PUSH            DX
        XOR             AX, AX
        MOV             DI, OFFSET DS:LengthCount
        MOV             CX, 17
        REP             STOSW


        MOV             DS:Depth, AX
        CALL            CountLength

        XOR             DX, DX
        MOV             CX, 15
        MOV             SI, OFFSET DS:LengthCount + 2
TP4@012:
        LODSW
        SHL             AX, CL
        ADD             DX, AX
        LOOP            TP4@012
        ADD             DX, DS:[SI]
        JZ              TP4@016

        SUB             DS:LengthCount[16 * 2], DX
        MOV             CX, DX
TP4@013:
        MOV             DI, OFFSET DS:LengthCount + 15 * 2
TP4@014:
        CMP             WORD PTR DS:[DI], 0
        JNE             TP4@015
        SUB             DI, 2
        JMP             TP4@014
TP4@015:
        DEC             WORD PTR DS:[DI]
        ADD             WORD PTR DS:[DI + 2], 2
        LOOP            TP4@013
TP4@016:
        MOV             DI, OFFSET DS:LengthCount + 16 * 2
        MOV             SI, DS:Code2
        MOV             AL, 16
TP4@017:
        MOV             CX, DS:[DI]
        JCXZ            MakeLength1
TP4@018:
        MOV             BX, DS:[SI]
        ADD             SI, 2
        SHR             BX, 1
        ADD             BX, DS:BitLen
        MOV             DS:[BX], AL
        LOOP            TP4@018
MakeLength1:
        SUB             DI, 2
        DEC             AL
        JNZ             TP4@017

        XOR             AX, AX

        MOV             DS:Start, AX
        MOV             DS:Weight, AX
        MOV             SI, OFFSET DS:LengthCount + 2
        MOV             DI, OFFSET DS:Start + 2
        MOV             BX, OFFSET DS:Weight + 2
        MOV             CX, 16
TP4@019:
        STOSW
        MOV             DX, DS:[SI]
        ADD             SI, 2
        DEC             CX
        SHL             DX, CL
        ADD             AX, DX
        MOV             DX, 1
        SHL             DX, CL
        INC             CX
        MOV             DS:[BX], DX
        ADD             BX, 2
        LOOP            TP4@019

        XOR             BX, BX
        MOV             CX, DS:NChar
        MOV             SI, DS:BitLen
        MOV             DI, DS:Code2
TP4@020:
        LODSB
        SHL             AL, 1
        MOV             BL, AL
        MOV             AX, DS:Start[BX]
        STOSW
        MOV             AX, DS:Weight[BX]
        ADD             DS:Start[BX], AX
        LOOP            TP4@020
        POP             AX
Return:
        SHR             AX, 1
        POP             BP
        POP             DI
        POP             SI
        POP             DX
        POP             CX
        RET
MakeTree        ENDP

;
;***********************************************************MakeTable
;
MakeTable       PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        PUSH            CX
        PUSH            DX
        PUSH            SI
        PUSH            DI
        PUSH            BP

        MOV             DS:[NChar], AX
        SHL             AX, 1
        MOV             DS:[AvailableMT], AX
        MOV             DS:[BitLen], BX
        MOV             DS:[TableBits], CX
        MOV             DS:[TTable], DX

        PUSH            DS
        POP             ES
        CLD
        ;clear LengthCount
        XOR             AX, AX
        MOV             CX, 16
        MOV             DI, OFFSET DS:LengthCount + 2
        REP             STOSW

        ;count BitLen into LengthCount
        XOR             BX, BX
        MOV             CX, DS:[NChar]
        MOV             SI, DS:[BitLen]
TP5@001:
        MOV             BL, DS:[SI]
        INC             SI
        SHL             BX, 1
        INC             DS:LengthCount[BX]
        LOOP            TP5@001

        ;initialize Weight, Start, Table
        MOV             CX, DS:[TableBits]
        MOV             BP, 1
        SHL             BP, CL
        MOV             BX, OFFSET DS:LengthCount + 2
        MOV             SI, OFFSET DS:Weight + 2
        MOV             DI, OFFSET DS:Start + 2
        MOV             AX, DS:[TTable]
TP5@002:
        STOSW
        MOV             DX, DS:[BX]
        ADD             BX, 2
        SHL             DX, CL
        ADD             AX, DX
        SHR             BP, 1
        MOV             DS:[SI], BP
        ADD             SI, 2
        LOOP            TP5@002

        PUSH            AX
        SUB             AX, DS:[TTable]
        MOV             CL, 15
        SUB             CX, DS:[TableBits]
        SHL             AX, CL
        SHL             BP, CL
TP5@003:
        STOSW
        MOV             DX, DS:[BX]
        ADD             BX, 2
        SHL             DX, CL
        ADD             AX, DX
        MOV             DS:[SI], BP
        ADD             SI, 2
        DEC             CX
        SHR             BP, 1
        JNZ             TP5@003

        OR              AX, AX
        JZ              TP5@004

        MOV             AX, ECBADFILEFORMAT
        JMP             Abort                    ;exit with error
TP5@004:
        POP             DI
        MOV             AX, DI
        SUB             AX, DS:[TTable]
        SHR             AX, 1
        MOV             BX, 1
        MOV             CX, DS:[TableBits]
        SHL             BX, CL
        MOV             CX, BX
        SUB             CX, AX
        XOR             AX, AX
        REP             STOSW

        ;make table and additional tree
        XOR             AX, AX
TP5@005:
        MOV             SI, AX
        ADD             SI, DS:[BitLen]
        MOV             BL, DS:[SI]
        OR              BL, BL
        JZ              MakeTable1

        XOR             BH, BH
        MOV             DX, BX
        SHL             BX, 1
        MOV             DI, DS:Start[BX]
        MOV             CX, DS:Weight[BX]
        CMP             DX, DS:[TableBits]
        JA              TP5@006
        REP             STOSW
        MOV             DS:Start[BX], DI
        JMP             SHORT TP5@011
TP5@006:
        ADD             DS:Start[BX], CX
        MOV             SI, DI
        MOV             BX, DS:[TableBits]
        MOV             CL, 16
        SUB             CL, BL
        SHR             DI, CL
        SHL             DI, 1
        ADD             DI, DS:[TTable]
        MOV             CL, BL
        SHL             SI, CL
        XOR             CX, CX
        MOV             CL, DL
        SUB             CX, DS:[TableBits]
TP5@007:
        CMP             WORD PTR DS:[DI], 0
        JNE             TP5@008
        MOV             BX, DS:[AvailableMT]
        MOV             DS:Right[BX], 0
        MOV             DS:Left[BX], 0
        MOV             DS:[DI], BX
        ADD             DS:[AvailableMT], 2
TP5@008:
        MOV             DI, DS:[DI]
        SHL             SI, 1
        JNC             TP5@009
        ADD             DI, OFFSET DS:Right
        JMP             SHORT TP5@010
TP5@009:
        ADD             DI, OFFSET DS:Left
TP5@010:
        LOOP            TP5@007

        MOV             DS:[DI], AX
TP5@011:
MakeTable1:
        INC             AX
        CMP             AX, DS:[NChar]
        JNB             TP5@012
        JMP             TP5@005
TP5@012:
        POP             BP
        POP             DI
        POP             SI
        POP             DX
        POP             CX
        RET
MakeTable       ENDP

;
;***********************************************************CalculateCRC
;
; On entry:
;    AX=offset to the buffer to process
;    BX=number of bytes to process
;
CalculateCRC    PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        PUSH            CX
        PUSH            SI

        MOV             SI, AX
        MOV             CX, BX

        RestoreDS
        MOV             BX, DS:[zCRC]            ;get previous crc value

        ;exit if we don't have anything to process
        JCXZ            ExitCalcCRC

        MOV             DS, EDS
        ASSUME  DS:NOTHING

        XOR             AH, AH
        CLD
CRCLoop1:
        LODSB
        XOR             BL, AL
        MOV             AL, BH
        MOV             BH, AH
        SHL             BX, 1
        RestoreDS
        MOV             BX, DS:CRCTable[BX]
        MOV             DS, EDS
        ASSUME  DS:NOTHING
        XOR             BX, AX
        LOOP            CRCLoop1
ExitCalcCRC:
        MOV             AX, BX
        RestoreDS
        MOV             DS:[zCRC], AX
        MOV             DS, EDS
        ASSUME  DS:NOTHING

        POP             SI
        POP             CX
        RET
CalculateCRC    ENDP

;
;****************************************************************FillBuffer
;
FillBuffer      PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        PUSH            CX
        PUSH            DX

        MOV             CH, AL
        MOV             CL, DS:[BitCount]
        MOV             DX, DS:[BitBuf]
        MOV             AL, DS:[SubBitBuf]
        CMP             CH, CL
        JNA             TP6@003
        SUB             CH, CL
        SHL             DX, CL
        ROL             AL, CL
        ADD             DL, AL
        MOV             CL, 8
TP6@001:
        XOR             AX, AX
        RestoreDS
        SUB             WORD PTR DS:[CompSize], 1
        SBB             WORD PTR DS:[CompSize] + 2, AX
        JS              TP6@002

        PUSH            ES
        PUSH            SI
        PUSH            DI
        PUSH            DX
        PUSH            CX
        PUSH            BX
        CALL            lhaGetC
        POP             BX
        POP             CX
        POP             DX
        POP             DI
        POP             SI
        POP             ES

        ASSUME  DS:NOTHING
        OR              AX, AX
        JNS             TP6@002
        MOV             AX, ECBADFILEFORMAT
        JMP             Abort                    ;exit with error
TP6@002:
        SetDS EDS
        CMP             CH, CL
        JNA             TP6@003
        SUB             CH, CL
        MOV             DH, DL
        MOV             DL, AL
        JMP             TP6@001
TP6@003:
        SUB             CL, CH
        MOV             DS:[BitCount], CL
        MOV             CL, CH
        XOR             AH, AH
        SHL             DX, CL
        SHL             AX, CL
        ADD             DL, AH
        MOV             DS:[BitBuf], DX
        MOV             DS:[SubBitBuf], AL
        POP             DX
        POP             CX
        RET
FillBuffer      ENDP

;
;*******************************************************************GetBits
;
GetBits         PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        PUSH            CX
        MOV             CL, 16
        SUB             CL, AL
        PUSH            DS:[BitBuf]
        CALL            FillBuffer
        POP             AX
        SHR             AX, CL
        POP             CX
        RET
GetBits         ENDP

;
;*******************************************************************PutBits
;
PutBits         PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        PUSH            CX
        MOV             CL, 16
        SUB             CL, AL
        SHL             BX, CL
        POP             CX
        ;On to PutCode
PutBits         ENDP

;
;*******************************************************************PutCode
;
;Also entered from PutBits (fall through)
;
PutCode         PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        PUSH            CX
        PUSH            DX

        MOV             CH, AL
        MOV             CL, DS:[BitCount]
        MOV             AL, DS:[SubBitBuf]

        CMP             CH, CL                   ;do we have enough bits to
        JNB             TP6@004                  ;write

        ROL             BH, CL
        OR              AL, BH
        MOV             DS:[SubBitBuf], AL
        SUB             CL, CH
        MOV             DS:[BitCount], CL
        POP             DX
        POP             CX
        RET
TP6@004:
        SUB             CH, CL
        XOR             DH, DH
        MOV             DL, BH
        SHL             DX, CL
        SHL             BX, CL
        OR              AL, DH
        MOV             DX, BX
        MOV             CL, 8
TP6@005:
        RestoreDS
        SUB             WORD PTR DS:[CompSize], 1
        SBB             WORD PTR DS:[CompSize + 2], 0
        MOV             DS, EDS
        ASSUME  DS:NOTHING
        JB              Disable

        RestoreDS
        PUSH            ES
        PUSH            SI
        PUSH            DI
        PUSH            DX
        PUSH            CX
        PUSH            BX

        PUSH            AX                       ;byte to write in AX
        CALL            lhaPutC

        POP             BX
        POP             CX
        POP             DX
        POP             DI
        POP             SI
        POP             ES
        MOV             DS, EDS
        ASSUME  DS:NOTHING

        CMP             CH, CL
        JB              TP6@006

        SUB             CH, CL
        MOV             AL, DH
        MOV             DH, DL
        JMP             TP6@005
TP6@006:
        MOV             DS:[SubBitBuf], DH
        SUB             CL, CH
        MOV             DS:[BitCount], CL
        POP             DX
        POP             CX
        RET

Disable:
        MOV             DS:[UnPackable], 1
        RestoreDS
        INC             WORD PTR DS:[CompSize]   ;adjust compressed size
        INC             WORD PTR DS:[CompSize + 2]
        MOV             DS, EDS
        ASSUME  DS:NOTHING

        POP             DX
        POP             CX
        RET
PutCode         ENDP

;
;***********************************************************ReadCRC
;
; On entry:
;
;    AX=the offset of the buffer
;
ReadCRC         PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        PUSH            CX
        PUSH            DX

        RestoreDS
        MOV             CX, DS:[InFile]          ;CX=file handle
        MOV             DS, EDS
        ASSUME  DS:NOTHING

        MOV             DX, AX                   ;DS:DX=buffer
        XCHG            BX, CX                   ;BX=file handle
        DosCall         3Fh                      ;Read file or device call
        MOV             BX, AX                   ;BX=bytes read
        ADD             WORD PTR DS:[BytesProcessed], AX
        ADC             WORD PTR DS:[BytesProcessed + 2], 0
        XCHG            AX, DX
        OR              BX, BX                   ;did we read anything?
        JZ              TP6@007                  ;if not don't do crc
        CALL            CalculateCRC
TP6@007:
        MOV             AX, DX

        POP             DX
        POP             CX
        RET
ReadCRC         ENDP

;
;******************************************************************WriteCRC
;
; On entry:
;
;    AX=is offset of buffer
;    BX=number of bytes to write
;
WriteCRC        PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        PUSH            CX
        PUSH            DX

        RestoreDS
        MOV             CX, DS:[OutF]            ;CX=open file handle
        MOV             DS, EDS
        ASSUME  DS:NOTHING

        MOV             DX, AX                   ;DX=offset to buffer
        PUSH            BX                       ;save byte count
        CALL            CalculateCRC
        POP             BX                       ;restore byte count

        ;exit if we don't have a valid file handle
        ;no error because this could be used for testing the archive
        JCXZ            TP6@008
                                                 ;DS:DX buffer to write
        XCHG            BX, CX                   ;CX=number of bytes to write
        DosCall         40h                      ;write file or device call

        ADD             WORD PTR DS:[BytesProcessed], AX
        ADC             WORD PTR DS:[BytesProcessed + 2], 0
        CMP             AX, CX                   ;did we write CX bytes?
        JE              TP6@008                  ;contnue

        MOV             AX, BX
        OR              AX, AX
        JNZ             TP6@008
        ;else, must have an error
        MOV             AX, ECDISKFULL
        JMP             Abort                    ;exit with error
TP6@008:
        POP             DX
        POP             CX
        RET
WriteCRC        ENDP

;
;***************************************************************InitGetBits
;
InitGetBits     PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        XOR             AX, AX
        MOV             WORD PTR DS:[BitBuf], AX
        MOV             BYTE PTR DS:[SubBitBuf], AL
        MOV             BYTE PTR DS:[BitCount], AL
        MOV             AL, 16
        JMP             FillBuffer
InitGetBits     ENDP

;
;***************************************************************InitPutBits
;
InitPutBits     PROC
        ASSUME  DS:NOTHING, ES:NOTHING, SS:NOTHING
        MOV             BYTE PTR DS:[BitCount], 8
        MOV             BYTE PTR DS:[SubBitBuf], 0
        RET
InitPutBits     ENDP

CODE            ENDS

                END
