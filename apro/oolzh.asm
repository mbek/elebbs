;******************************************************
;                     OOLZH.ASM 2.03
;                 ASM routines for OOLZH
;      Copyright (c) TurboPower Software 1990, 1993.
;                  All rights reserved.
;******************************************************

;****************************************************** Macros

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

;Restore all registers saved in order of SaveAllRegs
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

FlushBuffer     MACRO
                SaveAllRegs                     ;Save registers
                PUSH    OutBufPos               ;Push bytes to write
                PUSH    WORD PTR SaveSelf+2     ;Push Self parameter
                PUSH    WORD PTR SaveSelf
                CALL    Unlzh@ulFlushOutputBuffer
                MOV     InErr,AX                ;Save error code
                MOV     OutBufPos,0             ;Reset counter
                RestoreAllRegs                  ;Restore registers
                ENDM

;****************************************************** Data

DATA    SEGMENT WORD PUBLIC

        EXTRN   CodeTable : BYTE                ;Lookup table for code
        EXTRN   LengthTable : BYTE              ;Lookup table for length
        EXTRN   Son : WORD                      ;Frequency binary tree
        EXTRN   Parent : WORD                   ; "
        EXTRN   Freq : WORD                     ; "
        EXTRN   LzhHandle : WORD                ;File handle for input
        EXTRN   InBuffer : DWORD                ;Input buffer
        EXTRN   OutBuffer : DWORD               ;Output buffer
        EXTRN   TextBuffer : DWORD              ;Sliding dictionary
        EXTRN   EDPtr : DWORD                   ;Pointer to extra seg

; LZSS constants
RingBufSize     =       4096                    ;= 10000h
RingBufSizeMask =       0FFFh                   ;= RingBufSize - 1
Threshold       =       2
LookAheadSize   =       60

; Huffman constants
NumChar         =       (256 - Threshold + LookAheadSize)
TableSize       =       (NumChar * 2 - 1)
Root            =       TableSize-1
MaxFreq         =       08000h

; LZSS RingBuffer
RingBuffer      DB      RingBufSize DUP(?)
RingBufPos      DW      (?)

; Input/Output buffering
InBufSize       =       4096
InBufPos        DW      (?)
OutBufSize      =       4096
OutBufLimit     =       OutBufSize + LookAheadSize
OutBufPos       DW      (?)
BitBuffer       DW      (?)                     ;Input bit buffer
BitBufferLen    DW      (?)
BytesLeft       DD      (?)                     ;Bytes left to make file
BytesRead       DW      (?)                     ;Bytes read from LZH file
EmptyNode       =       8192                    ;Denotes empty tree node
Zero            =       0                       ;Zero constant

;Extra data segment structure
ExtraData       STRUC
LeftSon         DW      InBufSize+1 DUP (?)
RightSon        DW      InBufSize+1+256 DUP (?)
Dad             DW      InBufSize+1 DUP (?)
MatchLen        DW      (?)
MatchPos        DW      (?)
                ENDS

; other
SaveSelf        DD      (?)                     ;Self parameter to ulMeltPrim
InErr           DW      (?)                     ;Last LZH error

DATA    ENDS

;****************************************************** Code

CODE    SEGMENT WORD PUBLIC

        ASSUME  CS:CODE,DS:DATA

        PUBLIC  Unlzh@ulMeltPrim
        PUBLIC  UpdateCRC
        PUBLIC  UpdateBinaryTree
        PUBLIC  InitTree
        PUBLIC  InsertNode
        PUBLIC  DeleteNode

        EXTRN   Unlzh@ulFlushOutputBuffer : FAR
        EXTRN   ReconstructTree : NEAR

Crc16   DW      00000h, 0C0C1h, 0C181h, 00140h, 0C301h, 003C0h, 00280h, 0C241h
        DW      0C601h, 006C0h, 00780h, 0C741h, 00500h, 0C5C1h, 0C481h, 00440h
        DW      0CC01h, 00CC0h, 00D80h, 0CD41h, 00F00h, 0CFC1h, 0CE81h, 00E40h
        DW      00A00h, 0CAC1h, 0CB81h, 00B40h, 0C901h, 009C0h, 00880h, 0C841h
        DW      0D801h, 018C0h, 01980h, 0D941h, 01B00h, 0DBC1h, 0DA81h, 01A40h
        DW      01E00h, 0DEC1h, 0DF81h, 01F40h, 0DD01h, 01DC0h, 01C80h, 0DC41h
        DW      01400h, 0D4C1h, 0D581h, 01540h, 0D701h, 017C0h, 01680h, 0D641h
        DW      0D201h, 012C0h, 01380h, 0D341h, 01100h, 0D1C1h, 0D081h, 01040h
        DW      0F001h, 030C0h, 03180h, 0F141h, 03300h, 0F3C1h, 0F281h, 03240h
        DW      03600h, 0F6C1h, 0F781h, 03740h, 0F501h, 035C0h, 03480h, 0F441h
        DW      03C00h, 0FCC1h, 0FD81h, 03D40h, 0FF01h, 03FC0h, 03E80h, 0FE41h
        DW      0FA01h, 03AC0h, 03B80h, 0FB41h, 03900h, 0F9C1h, 0F881h, 03840h
        DW      02800h, 0E8C1h, 0E981h, 02940h, 0EB01h, 02BC0h, 02A80h, 0EA41h
        DW      0EE01h, 02EC0h, 02F80h, 0EF41h, 02D00h, 0EDC1h, 0EC81h, 02C40h
        DW      0E401h, 024C0h, 02580h, 0E541h, 02700h, 0E7C1h, 0E681h, 02640h
        DW      02200h, 0E2C1h, 0E381h, 02340h, 0E101h, 021C0h, 02080h, 0E041h
        DW      0A001h, 060C0h, 06180h, 0A141h, 06300h, 0A3C1h, 0A281h, 06240h
        DW      06600h, 0A6C1h, 0A781h, 06740h, 0A501h, 065C0h, 06480h, 0A441h
        DW      06C00h, 0ACC1h, 0AD81h, 06D40h, 0AF01h, 06FC0h, 06E80h, 0AE41h
        DW      0AA01h, 06AC0h, 06B80h, 0AB41h, 06900h, 0A9C1h, 0A881h, 06840h
        DW      07800h, 0B8C1h, 0B981h, 07940h, 0BB01h, 07BC0h, 07A80h, 0BA41h
        DW      0BE01h, 07EC0h, 07F80h, 0BF41h, 07D00h, 0BDC1h, 0BC81h, 07C40h
        DW      0B401h, 074C0h, 07580h, 0B541h, 07700h, 0B7C1h, 0B681h, 07640h
        DW      07200h, 0B2C1h, 0B381h, 07340h, 0B101h, 071C0h, 07080h, 0B041h
        DW      05000h, 090C1h, 09181h, 05140h, 09301h, 053C0h, 05280h, 09241h
        DW      09601h, 056C0h, 05780h, 09741h, 05500h, 095C1h, 09481h, 05440h
        DW      09C01h, 05CC0h, 05D80h, 09D41h, 05F00h, 09FC1h, 09E81h, 05E40h
        DW      05A00h, 09AC1h, 09B81h, 05B40h, 09901h, 059C0h, 05880h, 09841h
        DW      08801h, 048C0h, 04980h, 08941h, 04B00h, 08BC1h, 08A81h, 04A40h
        DW      04E00h, 08EC1h, 08F81h, 04F40h, 08D01h, 04DC0h, 04C80h, 08C41h
        DW      04400h, 084C1h, 08581h, 04540h, 08701h, 047C0h, 04680h, 08641h
        DW      08201h, 042C0h, 04380h, 08341h, 04100h, 081C1h, 08081h, 04040h

;****************************************************** UpdateCRC

;procedure UpdateCRC(var CRC : Word; var Buffer; Len : Word);
;Update 16-bit CRC based on first Len bytes in Buffer

CRC             EQU     DWORD PTR [BP+12]
Buffer          EQU     DWORD PTR [BP+8]
Len             EQU     WORD PTR [BP+6]

UpdateCRC       PROC FAR

        PUSH    BP                      ;set up stack frame
        MOV     BP,SP

        MOV     CX,Len                  ;CX = Len
        JCXZ    ucDone                  ;nothing to do if Len = 0

        PUSH    DS                      ;save DS
        LDS     SI,CRC                  ;DS:SI => CRC
        LODSW                           ;AX = CRC
        MOV     DX,AX                   ;DX = CRC
        LDS     SI,Buffer               ;DS:SI => Buffer
        CLD                             ;go forward

ucNext: LODSB                           ;Get next character from buffer
        XOR     DL,AL
        XOR     BX,BX                   ;Zero out BX
        MOV     BL,DL                   ;Use as index...
        SHL     BX,1                    ;into array of words
        MOV     DL,DH                   ;Move hi(Crc) to lo
        XOR     DH,DH                   ;Clear hi
        XOR     DX,Crc16[BX]            ;Lookup new CRC value
        LOOP    ucNext                  ;Repeat

        MOV     AX,DX                   ;AX = new CRC
        LES     DI,CRC                  ;ES:DI => CRC
        STOSW                           ;store new CRC
        POP     DS                      ;restore DS

ucDone: POP     BP                      ;clean up stack and return
        RET     10

UpdateCRC       ENDP

;****************************************************** InitTree

;InitTree
;  -Initializes the ecoding tree
;
;   Destroys AX,BX,CX,DI,ES
;
InitTree        PROC NEAR
         CLD
         MOV    AX,WORD PTR EDPtr+2             ;ES = tree segment
         MOV    ES,AX                           ;
         MOV    CX,256                          ;Fill 256 words
         MOV    AX,EmptyNode                    ;EmptyNode value
         MOV    BX,(InBufSize+1)*2              ;+1
         LEA    DI,RightSon[BX]                 ;ES:DI-> RightSon[InBufSize+1]
         REP    STOSW                           ;
         MOV    CX,4097                         ;Fill 4097 words
         MOV    DI, OFFSET Dad                  ;ES:DI -> RightSon[0]
         REP    STOSW                           ;
         RET                                    ;
InitTree        ENDP

;****************************************************** InsertNode

;InsertNode(R : Word);
;  -Inserts node R into binary tree
;
;        Destroys AX,BX,CX,DX,SI,DI,ES
;
InsertNode      PROC NEAR

;Stack setup and register saves
         PUSH   BP                              ;
         MOV    BP,SP                           ;Stack pointer
         MOV    BX,[BP+4]                       ;BX = R

;Load ES:DI, then set DS=extra data segment
         LES    DI,TextBuffer                   ;ES:DI -> TextBuffer^
         PUSH   DS                              ;Set DS = extra data segment
         MOV    AX,WORD PTR EDPtr+2             ;
         MOV    DS,AX                           ;

;Init RightSon/LeftSon of R
         MOV    SI,BX                           ;Save R in SI
         SHL    BX,1                            ;Make it an index
         MOV    AX, EmptyNode                   ;
         MOV    RightSon[BX],AX                 ;RightSon[R] := EmptyNode
         MOV    LeftSon[BX],AX                  ;LeftSon[R] := EmptyNode

;Get the current char and calculate the RightSon root slot
         ADD    DI,SI                           ;P := TextBuffer^ +
         MOV    BL,ES:[DI]                      ;  InBufSize
         XOR    BH,BH                           ;
         ADD    BX,InBufSize+1                  ;
         SHL    BX,1                            ;BX = P (an index)
         MOV    DS:MatchLen,Zero                ;MatchLen := 0

;If we've seen this char before then start comparing old strings
; else make a new node and exit
         CMP    RightSon[BX],EmptyNode          ;RightSon[BX] = EmptyNode?
         JE     IN1                             ;Yes, go insert node
         MOV    BX,RightSon[BX]                 ;No, BX := RightSon[P]
         JMP    IN2                             ;
IN1:     SHL    SI,1                            ;
         MOV    RightSon[BX],SI                 ;RightSon[P] := R
         MOV    [Dad+SI],BX                     ;Dad[R] := P
         JMP    IN99                            ;Exit

;Compare the current and old string
IN2:     CLD                                    ;
IN3:     MOV    SI,BX                           ;Get old string node
         SHR    SI,1                            ;Make a TextBuffer^ index
         MOV    DX,SI                           ;Save orignal index
         MOV    CX,LookAheadSize                ;Compare no more than 60 chars
         PUSH   DS                              ;Save DS
         MOV    AX,ES                           ;DS=ES
         MOV    DS,AX                           ;
         REPE   CMPS BYTE PTR [SI], BYTE PTR [DI] ;Compare old and new string
         POP    DS                              ;Restore DS
         LAHF                                   ;Save results for later
         JZ     IN10                            ;All equal, go replace node

;Update MatchLen and MatchPos if this was the longest compare yet
IN5:     SUB    SI,DX                           ;SI = equal char count
         DEC    SI                              ;Account for first char
         CMP    SI,DS:MatchLen                  ;Greater than previous?
         JLE    IN51                            ;No, skip
         MOV    DS:MatchLen,SI                  ;Yes, save length
         MOV    CX,DI                           ;Get ending R
         SUB    CX,SI                           ;  subtract out equal count
         DEC    CX                              ;  account for 1st char
         MOV    DX,BX                           ;Get P index
         SHR    DX,1                            ;Make a value
         SUB    CX,DX                           ;MatchPos := ((R - P) and
         AND    CX,InBufSize-1                  ;  InBufSize) - 1
         DEC    CX                              ;
         MOV    DS:MatchPos,CX                  ;

;If this MatchLen is the same as a previous, save the lower MatchPos
IN51:    CMP    SI,DS:MatchLen                  ;Same MatchLen?
         JNE    IN6                             ;No, skip
         MOV    CX,DI                           ;Yes, compute new MatchPos
         SUB    CX,SI                           ;  subtract out equal count
         DEC    CX                              ;  account for 1st char
         MOV    DX,BX                           ;Get P index
         SHR    DX,1                            ;Make a value
         SUB    CX,DX                           ;MatchPos := ((R - P) and
         AND    CX,InBufSize-1                  ;  InBufSize) - 1
         DEC    CX                              ;
         CMP    CX,DS:MatchPos                  ;New MatchPos < old
         JGE    IN6                             ;No, skip it
         MOV    DS:MatchPos,CX                  ;Yes, store it

;Restore ES:DI to TextBuffer^[R]. Go process CMPS results
IN6:     SUB    DI,SI                           ;Restore ES:DI
         DEC    DI                              ;Account for first char
         SAHF                                   ;Restore the compare results
         JA     IN7                             ;Lower string, go follow

;New string was higher sort, look for higher string to match
         CMP    RightSon[BX],EmptyNode          ;if RightSon[P] <> Empty
         JE     IN8                             ;
         MOV    BX,RightSon[BX]                 ;  P := RightSon[P]
         JMP    IN3                             ;Go compare this node

;No more higher strings, insert new right son node here
IN8:     SHL    DI,1                            ;Make an index
         MOV    RightSon[BX],DI                 ;RightSon[P] := R
         MOV    Dad[DI],BX                      ;Dad[R] := P
         JMP    IN99                            ;Exit

;New string was lower sort, look for lower node to match
IN7:     CMP    LeftSon[BX],EmptyNode           ;if LeftSon[P] = Empty
         JE     IN9                             ;
         MOV    BX,LeftSon[BX]                  ;  P := LeftSon[P]
         JMP    IN3                             ;Go compare this node

;No more lower strings, insert new left son node here
IN9:     SHL    DI,1                            ;Make an index
         MOV    LeftSon[BX],DI                  ;LeftSon[P] := R
         MOV    Dad[DI],BX                      ;Dad[R] := P
         JMP    IN99                            ;Exit

;Found exact match of LookAhead length
;Replace old node with current node (to speed subsequent searches)
IN10:    SUB    SI,DX                           ;SI = equal char count
         DEC    SI                              ;Account for first char
         MOV    CX,DI                           ;Get ending R
         SUB    CX,SI                           ;  subtract out equal count
         DEC    CX                              ;  account for 1st char
         MOV    DX,BX                           ;Get P index
         SHR    DX,1                            ;Make a value
         SUB    CX,DX                           ;MatchPos := ((R - P) and
         AND    CX,InBufSize-1                  ;  InBufSize) - 1
         DEC    CX                              ;
         MOV    DS:MatchPos,CX                  ;
         MOV    DS:MatchLen,LookAheadSize       ;
         SUB    DI,LookAheadSize                ;DI = R
         SHL    DI,1                            ;Make an index

;Set Dad, LeftSon and RightSon pointers of current node
         MOV    AX,[Dad+BX]                     ;Dad[R] := Dad[P]
         MOV    [Dad+DI],AX
         MOV    AX,[RightSon+BX]                ;RightSon[R] := RightSon[P]
         MOV    [RightSon+DI],AX
         MOV    AX,[LeftSon+BX]                 ;LeftSon[R] := LeftSon[P]
         MOV    [LeftSon+DI],AX

;Set new Dad pointers for children nodes
         MOV    SI,[LeftSon+BX]                 ;Dad[LeftSon[P]] := R;
         MOV    [Dad+SI],DI
         MOV    SI,[RightSon+BX]                ;Dad[RightSon[P]] : R
         MOV    [Dad+SI],DI

;Set child pointer (left or right) of parent node
         MOV    SI,[Dad+BX]                     ;if RightSon[Dad[P]] = P
         CMP    [RightSon+SI],BX
         JNE    IN11
         MOV    SI,[Dad+BX]                     ;RightSon[Dad[P]] := R
         MOV    [RightSon+SI],DI
         JMP    IN12
IN11:    MOV    SI,[Dad+BX]                     ;LeftSon[Dad[P]] := R
         MOV    [LeftSon+SI],DI

;Clear old node
IN12:    MOV    [Dad+BX],EmptyNode              ;Dad[P] := EmptyNode

;Clean up and exit
IN99:    POP    DS
         POP    BP
         RET    2
InsertNode      ENDP

;***************************************************** DeleteNode
;DeleteNode(P : Word);
;  -Deletes node P from binary tree
;
DeleteNode      PROC NEAR

;Stack setup and register saves
         PUSH   BP                              ;
         MOV    BP,SP                           ;Stack pointer
         MOV    BX,[BP+4]                       ;BX = P
         PUSH   DS                              ;Set DS=extra data segment
         MOV    AX,WORD PTR EDPtr+2             ;
         MOV    DS,AX                           ;

;If this node is empty just exit
         SHL    BX,1                            ;Make an index
         MOV    SI,BX                           ;Save P in SI
         CMP    [Dad+BX],EmptyNode              ;if Dad[P] = EmptyNode
         JNE    DN0                             ;No, keep going
         JMP    DN99                            ;Yes, finished

;if RightSon is empty then pull up left son
DN0:     CMP    [RightSon+BX], EmptyNode        ;if RightSon[P] = EmptyNode
         JNE    DN1                             ;
         MOV    BX,[LeftSon+BX]                 ;  Q := LeftSon[BX]
         JMP    DN2                             ;

;If LeftSon is empty then pull up right son
DN1:     CMP    [LeftSon+BX],EmptyNode          ;else if LeftSon[BX] = EmptyNode
         JNE    DN3
         MOV    BX,[RightSon+BX]                ;  Q := RightSon[BX]
         JMP    DN2                             ;

;Both children are active, find leftmost son of right tree
DN3:     MOV    BX,[LeftSon+BX]                 ;else Q := LeftSon[BX]
         CMP    [RightSon+BX], EmptyNode        ;if RightSon[Q] <> EmptyNode
         JE     DN5                             ;

;Traverse down right sons until right son is empty
DN6:     MOV    BX,RightSon[BX]                 ;repeat
         CMP    RightSon[BX],EmptyNode          ;  Q := RightSon[Q]
         JNE    DN6                             ;until RightSon[Q] = EmptyNode

;Attach Q's left son to P's left son
         MOV    DI,[Dad+BX]                     ;RightSon[Dad[Q]] := LeftSon[Q]
         MOV    AX,LeftSon[BX]                  ;
         MOV    [RightSon+DI],AX                ;
         MOV    DI,[LeftSon+BX]                 ;Dad[LeftSon[Q]] := Dad[Q]
         MOV    AX,[Dad+BX]                     ;
         MOV    [Dad+DI],AX                     ;

;Replace P with Q
         MOV    AX,[LeftSon+SI]                 ;LeftSon[Q] := LeftSon[P]
         MOV    [LeftSon+BX], AX                ;
         MOV    DI,[LeftSon+SI]                 ;Dad[LeftSon[P]] := Q
         MOV    [Dad+DI],BX                     ;

;Move it up
DN5:     MOV    AX,[RightSon+SI]                ;RightSon[Q] := RightSon[P]
         MOV    [RightSon+BX],AX                ;
         MOV    DI,[RightSon+SI]                ;Dad[RightSon[P]] := Q
         MOV    [Dad+DI],BX                     ;

;Make P's parent Q's parent
DN2:     MOV    AX,[Dad+SI]                     ;Dad[Q] := Dad[P]
         MOV    [Dad+BX],AX                     ;
         MOV    DI,[Dad+SI]                     ;RightSon[Dad[P]] := Q
         CMP    [RightSon+DI],SI                ;if RightSon[Dad[P]] = P then
         JNE    DN9                             ;
         MOV    [RightSon+DI],BX                ;  RightSon[Dad[P]] := Q
         JMP    DN10                            ;
DN9:     MOV    DI,[Dad+SI]                     ;  else LeftSon[Dad[P]] := Q
         MOV    [LeftSon+DI],BX                 ;

;Say this node is empty
DN10:    MOV    [Dad+SI],EmptyNode              ;Dad[P] := EmptyNode

;Clean up and return
DN99:    POP   DS
         POP   BP
         RET   2
DeleteNode      ENDP

;****************************************************** UpdateBinaryTree

;UpdateBinaryTree
;  -Updates the freq of this code and rearranges the binary tree
;        SI = Code (passed)
;        CX = NewFreq
;        DI = Index of last, lower freq
;        BX,DX = various temp values
;
;        Destroys AX,BX,CX,DX,SI,DI
;
UpdateBinaryTree        PROC NEAR

;Check for reconstruct
        OR      Freq[Root * 2],0                ;At max frequency?
        JNS     UBT1                            ;No, continue
        PUSH    SI
        PUSH    ES
        CALL    ReconstructTree                 ;Yes, reconstruct tree
        POP     ES
        POP     SI

;Increment this code's frequency
UBT1:   CLD                                     ;Clear the direction flag
        MOV     SI,Parent[SI]                   ;SI = Parent[Code]
UBT2:   MOV     AX,Freq[SI]                     ;AX = current freq
        LEA     DI,Freq[SI]                     ;ES:DI = @Freq[SI]
        ADD     DI,2                            ;Next
        SCASW                                   ;Search for greater
        JNE     UBT6                            ;No swap, go get parent

;Need swap, find first greater
        MOV     CX,0FFFFh                       ;Search all
        REPE    SCASW                           ; for first greater
        SUB     DI,4                            ;Back up to last lower
        SUB     DI,OFFSET Freq                  ;DI = index of last lower

;Found code with higher frequency, swap with the parent and son pointers
        MOV     BX,Son[SI]                      ;Get old Son index
        MOV     Parent[BX],DI                   ;Update with new index
        CMP     BX,TableSize*2                  ;Index < TableSize ?
        JAE     UBT4                            ;No, continue
        MOV     Parent[BX+2],DI                 ;Parent[BX+1] = new index

UBT4:   XCHG    BX,Son[DI]                      ;get old index, stuff new
        MOV     Parent[BX],SI                   ;Stuff old index
        CMP     BX,TableSize*2                  ;BX > TableSize ?
        JGE     UBT5                            ;No, continue
        MOV     Parent[BX+2],SI                 ;Stuff old index

UBT5:   MOV     Son[SI],BX                      ;Son[old index] := new ptr
        MOV     SI,DI                           ;Code := Next code

UBT6:   INC     Freq[SI]                        ;Increment freq of this code
        MOV     SI,Parent[SI]                   ;Get parent index
        OR      SI,SI                           ;Repeat up to Root
        JNZ     UBT2
        RET

UpdateBinaryTree        ENDP

;****************************************************** GetNextByte

;procedure GetNextByte;
;  {-Returns next byte from input stream in AL}

;  Destroys AX,BX

GetNextByte     PROC    NEAR

        PUSH    SI                      ;Save SI (for GetNextPosition)
        INC     InBufPos                ;Increment InBufPos
        MOV     BX,InBufPos             ;BX = InBufPos
        CMP     BX,BytesRead            ;InBufPos >= BytesRead ?
        JL      GNB1                    ;No, get next byte

;Replenish byte buffer
        PUSH    CX                      ;Save CX (for GetNextPosition)
        PUSH    DX                      ;Save DX (for GetNextCode)
        MOV     BX,LzhHandle            ;BX = file handle
        MOV     CX,InBufSize            ;CX = bytes to read
        PUSH    DS                      ;Save DS
        LDS     DX,InBuffer             ;DS:DX = InBuffer
        DosCall 03Fh                    ;Read file
        POP     DS                      ;Restore DS
        JC      GNB2                    ;Exit on error
        MOV     BytesRead,AX            ;Save BytesRead
        XOR     BX,BX                   ;Reset index
        MOV     InBufPos,BX             ;"
        POP     DX                      ;Restore DX (for GetNextCode)
        POP     CX                      ;Restore CX (for GetNextPosition)

;Return next byte in buffer
GNB1:   PUSH    DS                      ;Save DS
        LDS     SI,InBuffer             ;DS:SI = InBuffer
        MOV     AL,[SI+BX]              ;AL = next byte
        POP     DS                      ;Resore DS
        POP     SI                      ;Restore SI (for GetNextPosition)
        RET

;Error exit
GNB2:   MOV     InErr,AX                ;Save the error code
        MOV     AL,00h                  ;Just return a nul
        MOV     BytesRead,CX            ;Prevent additional retries
        POP     DX                      ;Restore the stack
        POP     CX
        POP     SI
        RET                             ;MeltPrim will abort shortly

GetNextByte     ENDP

;****************************************************** GetNextPosition

;procedure GetNextPosition;
;  {-Returns ring buffer index of current <pos, len> code}

;       Destroys AX,BX,CX,DX,SI

GetNextPosition PROC NEAR

        CMP     BitBufferLen,8          ;Do we have at least 8 bits?
        JGE     GNP1                    ;Yes, continue

;Refill BitBuffer
        CALL    GetNextByte             ;Get next byte (in AL)
        XOR     AH,AH                   ;Zero out AH
        MOV     CX,8                    ;New bits
        SUB     CX,BitBufferLen         ;8 - BitBufferLen = shift count
        SHL     AX,CL                   ;Shift new byte over
        ADD     BitBuffer,AX            ;Merge old and new
        ADD     BitBufferLen,8          ;Set new bit count

GNP1:   MOV     SI,BitBuffer            ;Get next 8 bits
        MOV     CL,8                    ;Shift these bits to lower 8
        SHR     SI,CL                   ;Move to AL
        SHL     BitBuffer,CL            ;Adjust BitBuffer
        SUB     BitBufferLen,8          ;Calc new BitBuffer length
        JNZ     GNP2                    ;Continue

;Refill BitBuffer
        CALL    GetNextByte             ;Get next byte (in AL)
        XOR     AH,AH                   ;Zero out AH
        MOV     CX,8                    ;New bits
        SUB     CX,BitBufferLen         ;8 - BitBufferLen = shift count
        SHL     AX,CL                   ;Shift new byte over
        ADD     BitBuffer,AX            ;Merge old and new
        ADD     BitBufferLen,8          ;Set new bit count

;Recover upper 6 bits from the CodeTable
GNP2:   XOR     AX,AX                   ;Zero AX
        MOV     AL,CodeTable[SI]        ;Lookup code
        MOV     CL,6                    ;
        SHL     AX,CL                   ;Move bits over
        PUSH    AX                      ;Save this (Code)

;Prepare to read the rest of the bits
        MOV     DX,BitBuffer            ;DX = BitBuffer
        MOV     BX,BitBufferLen         ;BX = BitBuffer length
        XOR     CX,CX                   ;Zero out CX
        MOV     CL,LengthTable[SI]      ;Get length of position

;Shift new bits (left) into Position
GNP3:   SHL     SI,1                    ;Move existing bits over
        SHL     DX,1                    ;Shift next bit into Carry flag
        ADC     SI,0                    ;Add next bit to Position
        DEC     BX                      ;Decrement length of bit buffer
        JNZ     GNP4                    ;Skip if bits left in BitBuffer

;Replenish the BitBuffer
        CALL    GetNextByte             ;Get next byte (in AL)
        MOV     DH,AL                   ;Store high byte
        CALL    GetNextByte             ;Get next byte (in AL)
        MOV     DL,AL                   ;Store low byte
        MOV     BX,16                   ;16 fresh bits
GNP4:   LOOP    GNP3

;Clean up and exit
GNP5:   MOV     BitBuffer,DX            ;Update BitBuffer
        MOV     BitBufferLen,BX         ;Update BitBufferLen
        AND     SI,03Fh                 ;Mask
        POP     AX                      ;Restore Code
        OR      AX,SI                   ;Merge with index
        RET

GetNextPosition ENDP

;****************************************************** GetNextCode

;procedure GetNextCode;
;  {-Gets next Huffman code from input stream}

;       Destroys AX,BX,CX,DX,SI
;       Returns AX (Code)

GetNextCode     PROC NEAR

        MOV     SI,Son[Root * 2]        ;Start at Root
        MOV     DI,TableSize * 2        ;Use register for faster compares

;Move BitBuffer into registers
GNC0:   MOV     DX,BitBuffer            ;DX = current bit buffer
        MOV     CX,BitBufferLen         ;CX = length of bit buffer
        JMP     SHORT GNC2              ;Go compare

;Get next bit, 1 means go to right son, 0 means left
GNC1:
        SHR     SI,1                    ;Change from index to value
        SHL     DX,1                    ;Shift next bit into Carry flag
        ADC     SI,0                    ;Add next bit to current index
        SHL     SI,1                    ;Change back to index
        MOV     SI,Son[SI]              ;Get next Son index
        DEC     CX                      ;Decrement length of bit buffer
        JNZ     GNC2                    ;Skip if bits left

;See if the BitBuffer needs to be replenished
        CALL    GetNextByte             ;Get next byte (in AL)
        MOV     DH,AL                   ;Store high byte
        CALL    GetNextByte             ;Get next byte (in AL)
        MOV     DL,AL                   ;Store low byte
        MOV     CX,16                   ;16 fresh bits

;Continue traversing son indices until we get to a leaf
GNC2:   CMP     SI,DI                   ;Into leaf yet? (SI > TableSize*2)
        JB      GNC1                    ;No, get next index

;Store BitBuffer, Update binary tree, return function value
        MOV     BitBuffer,DX            ;Store new bit buffer
        MOV     BitBufferLen,CX         ;Store new bit buffer length
        PUSH    SI                      ;Save Code
        CALL    UpdateBinaryTree        ;Update the binary tree (Code in SI)
        POP     AX                      ;Restore Code to AX
        SUB     AX,TableSize*2          ;Code := Code - TableSize
        SHR     AX,1                    ;Change from index to value
        RET

GetNextCode     ENDP

;****************************************************** Unlzh.ulMeltPrim

;function Unlzh.ulMeltPrim(BytesLeft : LongInt) : Word;
;  {-Loop through frozen file, write melted file}

uBytesLeft      EQU     DWORD PTR [BP+10]
uSelf           EQU     DWORD PTR [BP+6]

Unlzh@ulMeltPrim        PROC FAR

;Get BytesLeft
        PUSH    BP                      ;Set up stack frame
        MOV     BP,SP
        LES     DI,uSelf                ;ES:DI = uSelf
        MOV     WORD PTR SaveSelf,DI    ;SaveSelf = ES:DI
        MOV     WORD PTR SaveSelf+2,ES
        MOV     AX,WORD PTR uBytesLeft  ;AX = uBytesLeft lo
        MOV     WORD PTR BytesLeft,AX   ;
        MOV     AX,WORD PTR uBytesLeft+2;AX = uBytesLeft hi
        MOV     WORD PTR BytesLeft+2,AX ;

;Misc  initializations
        XOR     AX,AX
        MOV     InBufPos,AX             ;InBuffer index
        MOV     OutBufPos,AX            ;OutBuffer index
        MOV     BitBuffer,AX            ;Bits move from right to left
        MOV     BitBufferLen,AX         ;Number of valid bits in BitBuffer
        MOV     RingBufPos,RingBufSize - LookAheadSize  ;RingBuffer index
        MOV     BytesRead,AX            ;Number of bytes in buffer
        MOV     InErr,AX                ;Error code

;FillChar(RingBuffer, RingBufSize-LookAheadSize, ' ');
        CLD
        PUSH    DS                      ;ES = DS
        POP     ES
        MOV     DI,OFFSET RingBuffer    ;ES:DI => RingBuffer
        MOV     CX,RingBufSize-LookAheadSize    ;CX = RingBufSize-LookAheadSize
        MOV     AL,' '                  ;AL = space
        REP     STOSB                   ;Init RingBuffer to spaces

;Refill BitBuffer
        CALL    GetNextByte             ;Get next byte (in AL)
        XOR     AH,AH                   ;Zero out AH
        MOV     CX,8                    ;New bits
        SUB     CX,BitBufferLen         ;8 - BitBufferLen = shift count
        SHL     AX,CL                   ;Shift new byte over
        ADD     BitBuffer,AX            ;Merge old and new
        ADD     BitBufferLen,8          ;Set new bit count

;Major uncompress loop
MP1:    CALL    GetNextCode             ;AX = next code
        OR      AH,AH                   ;Code > 255?
        JNZ     MP3                     ;Yes, go decode <pos, len> code

;It's a single character, stuff it in the output buffer
        MOV     BX,OutBufPos            ;BX = OutBufPos
        PUSH    ES                      ;Save ES
        LES     DI,OutBuffer            ;ES:DI = OutBuffer
        MOV     ES:[DI+BX],AL           ;OutBuffer^[OutBufPos] := char
        POP     ES                      ;Restore ES
        INC     OutBufPos               ;Update OutBufPos
        MOV     BX,RingBufPos           ;BX = RingBufPos
        MOV     RingBuffer[BX],AL       ;RingBuffer[RingBufPos] := char
        INC     BX                      ;Next position in ring buffer
        AND     BX,RingBufSizeMask      ;Wrap around ring buffer
        MOV     RingBufPos,BX           ;Update RingBufPos var
        SUB     WORD PTR BytesLeft,1    ;Bytes left to go
        SBB     WORD PTR BytesLeft+2,0  ; (longint)
        JMP     SHORT MP5

;It's a position code, get the position in AX
MP3:    PUSH    AX                      ;Save Code
        CALL    GetNextPosition         ;Get position into AX
        MOV     BX,RingBufPos           ;BX = position in RingBuffer
        SUB     BX,AX                   ;Backwards from current position
        SUB     BX,2                    ;Two more

;Get the length
        POP     CX                      ;CX = Code
        SUB     CX,255 - Threshold      ;Convert to length

;Loop for length, retrieving chars from ring buffer
        CLD                             ;Go forward
        PUSH    ES                      ;Save ES
        LES     DI,OutBuffer            ;ES:DI = OutBuffer
        ADD     DI,OutBufPos            ;ES:DI = @OutBuffer[OutBufPos]
        MOV     SI,RingBufPos           ;SI = RingBuffer index

;Go ahead pre-update some variables now
        ADD     OutBufPos,CX            ;Update OutBufPos
        SUB     WORD PTR BytesLeft,CX   ;Update BytesLeft
        SBB     WORD PTR BytesLeft+2,0  ; (longint)

MP4:    INC     BX                      ;Increment Position
        AND     BX,RingBufSizeMask      ;Wrap around ring buffer
        MOV     AL,RingBuffer[BX]       ;AX = character code
        STOSB                           ;OutBuffer[OutBufPos] := char code
        MOV     RingBuffer[SI],AL       ;RingBuffer[RingBufPos] := char code
        INC     SI                      ;Next position in ring buffer
        AND     SI,RingBufSizeMask      ;Wrap around ring buffer
        LOOP    MP4                     ;Finished?

        POP     ES                      ;Restore ES

;Finished with repeat, update RingBufPos
        MOV     RingBufPos,SI           ;Update RingBufPos

;Found any errors?
        CMP     InErr,0                 ;any errors?
        JNE     MP10                    ;leave if so

;Output buffer full?
MP5:    CMP     OutBufPos,OutBufSize    ;OutBufPos >= OutBufSize
        JB      MP7                     ;No, continue
        FlushBuffer                     ;Yes, go flush buffer
        CMP     InErr,0                 ;Get error?
        JNE     MP10                    ;If so, exit

;Finished with extraction?
MP7:    MOV     AX,WORD PTR BytesLeft+2 ;Get high word of BytesLeft
        OR      AX,AX                   ;Finished?
        JS      MP9                     ;Yes, leave
        OR      AX,WORD PTR BytesLeft   ;Low word zero?
        JZ      MP9                     ;Yes, leave
        JMP     MP1                     ;No, get another code

;Finished, flush last buffer, calculate CRC
MP9:    FlushBuffer                     ;write last buffer (if any)

MP10:   MOV     AX,InErr
        POP     BP
        RET     8

Unlzh@ulMeltPrim        ENDP

CODE    ENDS
        END
