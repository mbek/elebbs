;*******************************************************************
;                       APUART.ASM 2.03
;        Copyright (c) TurboPower Software 1987, 1989.
;     Portions copyright (c) Information Technology 1989,
;        and used under license to TurboPower Software
;                   All rights reserved.
;*******************************************************************

;================================================================== Data
Data    SEGMENT WORD PUBLIC

;Include configuration data set by APDEFINE.INC
INCLUDE APCONFIG.ASM

;"Permanent" configuration options (not affected by APDEFINE.INC)
TASMOn          EQU 0           ;Use TASM
UseToggle       EQU 0           ;Toggle ints when exiting ISR
;ShareIrqOn     EQU 0           ;Enable IRQ line sharing
;Debugging      EQU 0           ;Don't allow ISR to be re-entered
;HandleTPort    EQU 0           ;Works better with TPort boards

;Pointers to Ports (a fixed array of pointers)
        EXTRN   ActiveComPort:DWORD

;Port definition (same as in PORTDEF.PAS)
PortRec         STRUC           ;Abstract port definition
BaseAddr        DW      ?       ;Base IO addr of UART
Flags           DW      ?       ;Port options word
InBuffLen       DW      ?       ;Length of input buffer
InBuffCount     DW      ?       ;Current # chars in buffer
OutBuffLen      DW      ?       ;Length of output buffer
OutBuffCount    DW      ?       ;Current # of chars in buffer
LostCharCount   DW      ?       ;Total lost bytes since last check
SWFFull         DW      ?       ;Hi-water mark for xoff
SWFResume       DW      ?       ;Lo-water mark for xon
HWFFull         DW      ?       ;Hi-water mark for auto-handshaking off}
HWFResume       DW      ?       ;Lo-water mark for auto-handshaking on}
CurBaud         DD      ?       ;Baud rate
InBuff          DD      ?       ;Addr of input buffer
InHead          DD      ?       ;Addr of current head
InTail          DD      ?       ;Addr of current tail
InBuffEnd       DD      ?       ;Addr of end-of-buffer
OutBuff         DD      ?       ;Addr of output buffer
OutHead         DD      ?       ;Addr of current head
OutTail         DD      ?       ;Addr of current tail
OutBuffEnd      DD      ?       ;Addr of end-of-buffer
StatBuff        DD      ?       ;Addr of status buffer
StatHead        DD      ?       ;Addr of current status head
StatTail        DD      ?       ;Addr of current status tail
StatBuffEnd     DD      ?       ;Addr of end of status buffer
PortName        DB      ?       ;"Standard" com name (i.e. COM1, COM2...)
Vector          DB      ?       ;Vector number of UART interrupt
IrqNumber       DB      ?       ;Irq number used by this port
IntMask         DB      ?       ;Current UART interrupt enable mask
CurrentPort     DB      ?       ;Current active port number
ISREntryPoint   DB      ?       ;Entry point number into APUART.ASM
ModemStatus     DB      ?       ;Current modem status
ModemControl    DB      ?       ;Current modem control value
LineStatus      DB      ?       ;Current line status
LineControl     DB      ?       ;Current line control value
SWFState        DB      ?       ;Software flow control options
SWFGotXoff      DB      ?       ;True if Xoff char received
SWFSentXoff     DB      ?       ;True if Xoff char sent
SWFOnChar       DB      ?       ;SW flow on character (def = $11, xon)
SWFOffChar      DB      ?       ;SW flow off character (def = $13, xoff)
BreakReceived   DB      ?       ;True if break detected
TxReady         DB      ?       ;True if transmitter is available
TxInts          DB      ?       ;True if using transmit interrupts
TxIntsActive    DB      ?       ;True if transmit ints are active
Buffered        DB      ?       ;True if using buffered serial I/O
UseStatusBuffer DB      ?       ;True if using status buffer
OldUart         DB      ?       ;True if UART is 8250 or 8250B
CurParity       DB      ?       ;Parity
CurDataBits     DB      ?       ;Data bits
CurStopBits     DB      ?       ;Stop bits
SaveChar        DB      ?       ;Last known char (used internally only)
LastXmitError   DB      ?       ;Last transmit error code
HWFTransMask    DB      ?       ;Mask to XOR modem status bits to zero
HWFTransHonor   DB      ?       ;Mask of required modem status bits
HWFRecMask      DB      ?       ;Mask of "on" modem status bits
HWFRecHonor     DB      ?       ;Mask of modem status bits we care about
HWFRemoteOff    DB      ?       ;True if we have turned off the remote
ISRActive       DB      ?       ;True if ISR is already active
ProtocolActive  DB      ?       ;True if this port is doing a protocol
FaxActive       DB      ?       ;True if the port is doing a fax         !!.02
CloseProc       DD      ?       ;ClosePort proc for this port
ErrorProc       DD      ?       ;Pointer to error procedure
ErrorData       DD      ?       ;Pointer passed to error routine
UserAbort       DD      ?       ;Hook for user (keyboard) abort
OrigPortState   DB      12 DUP (?) ;Record for saving init port config
PortRec         ENDS

;Transmitter failure codes
XmitFailedHW    EQU     1       ;Transmit failed for lack of DSR and/or CTS
XmitFailedSW    EQU     2       ;Transmit failed due to Xoff

;Masks
DSROnMask       EQU     020h    ;DSR bit only
CTSOnMask       EQU     010h    ;CTS bit only
FIFOMask        EQU     006h    ;Interrupt bits only (in IID reg)
FIFOOnMask      EQU     0C0h    ;Fifo bits only (in IID reg)
DTRMask         EQU     001h    ;DTR bit only
RTSMask         EQU     002h    ;RTS bit only
HWSignals       EQU     DTRMask + RTSMask ;Both hdw flow signals
StartSig        EQU     0A5h    ;Start signature for event logging
LEMask          EQU     00Eh    ;Error bits only of LineStatus
BreakMask       EQU     010h    ;Break bit in LSR
DeltaBits       EQU     00Bh    ;Mask off all but delta modem status bits
AllIntsOff      EQU     000h    ;All UART interrupts off
AllIntsOn       EQU     00Fh    ;All UART interrupts on
AllIntsButTrans EQU     00Dh    ;All UART ints on except transmit
THREMask        EQU     020h    ;Mask of just THRE in LSR
THREandTEMask   EQU     060h    ;Mask of both THRE and TE bits
ptHiIrq         EQU     1000h   ;True if IRQ > 7

;Register offsets
IER             EQU     01      ;Interrupt enable register
IIR             EQU     02      ;Interupt ID register
FCR             EQU     02      ;FIFO control register
LCR             EQU     03      ;Line control register
MCR             EQU     04      ;Modem control register
LSR             EQU     05      ;Line status register
MSR             EQU     06      ;Modem status register
SCR             EQU     07      ;Scratch register

;EOI masks
EOIMask         EQU     020h    ;Nonspecific end-of-interrupt mask
EOIPort         EQU     020h    ;Interrupt controller port
EOIPortHi       EQU     0A0h    ;Interrupt controller port for slave 8259

IFDEF EventLoggingOn
        EXTRN   EventQueue : DWORD
        EXTRN   EventIndex : WORD
        EXTRN   MaxEventIndex : WORD
        EXTRN   EventsWrapped : BYTE
        EXTRN   EventLogging : BYTE
ENDIF

Data           ENDS

;========================================================== Macros
;---------------------------------------------------------- StoreTime Macro
IFDEF EventLoggingOn
;Creates a timestamped event record
StoreTime MACRO Number, Code, Signature
        LOCAL   ST1,ST2

;Restore DS
        PUSH    AX
        PUSH    DS
        MOV     AX, Data
        MOV     DS, AX

;See if logging is turned on
        CMP     EventLogging, 1
        JNE     ST2

;Save used registers
        PUSH    BX
        PUSH    DX
        PUSH    ES
        PUSH    SI

;Get the current time in milliseconds (LongInt result in AX,DX)
        CALL    FAR PTR ReadTimer

;Create a timer record in the next slot in EventQueue
        MOV     SI, EventIndex          ;SI = index
        LES     BX, EventQueue          ;ES:BX => EventQueue
        MOV     WORD PTR ES:[BX+SI],Code ;Store event type
        MOV     BYTE PTR ES:1[BX+SI],Signature ;Embed signature
        MOV     ES:2[BX+SI], AX         ;Store high word of time
        MOV     ES:4[BX+SI], DX         ;Store low word of time

;Check for buffer wrap
        ADD     SI, 6                   ;Point to next slot
        CMP     SI, MaxEventIndex       ;Buffer wrap?
        JB      ST1                     ;No, skip it
        XOR     SI, SI                  ;Yes, wrap around
        MOV     EventsWrapped, 1        ; and say wrapped at least once

;Store EventIndex and restore used regs
ST1:    MOV     EventIndex, SI
        POP     SI
        POP     ES
        POP     DX
        POP     BX
ST2:    POP     DS
        POP     AX
ENDM

;---------------------------------------------------------- StoreEvent Macro
;Creates a data event record
StoreEvent MACRO EventType, DataWord
        LOCAL   SE1,SE2

;Restore DS
        PUSH    BX
        PUSH    DS
        MOV     BX, Data
        MOV     DS, BX

;See if logging is turned on
        CMP     EventLogging, 1
        JNE     SE2

;Save used registers
        PUSH    ES
        PUSH    SI

;Create an event record of EventType and DataWord
        MOV     SI, EventIndex          ;SI = index
        LES     BX, EventQueue          ;ES:BX => EventQueue
        MOV     WORD PTR ES:[BX+SI], EventType ;Store event type
        MOV     ES:2[BX+SI], AX         ;Store DataWord

;Check for buffer wrap
        ADD     SI, 6                   ;Point to next slot
        CMP     SI, MaxEventIndex       ;Buffer wrap?
        JB      SE1                     ;No, skip it
        XOR     SI, SI                  ;Yes, wrap around
        MOV     EventsWrapped, 1        ; and say wrapped at least once

;Store EventIndex and Restore used regs
SE1:    MOV     EventIndex, SI
        POP     SI
        POP     ES
SE2:    POP     DS
        POP     BX
ENDM
ENDIF

;----------------------------------------------------------- CommISR Macro
;Standard async ISR
CommISR       MACRO Number
CommInt&Number& PROC FAR

;Save used registers
        PUSH    AX
        PUSH    BX
        PUSH    CX
        PUSH    DX
        PUSH    DS
        PUSH    ES
        PUSH    DI

;Make DS:BX -> ActiveComPort[Number]
        MOV     AX,Data
        MOV     DS,AX
        LDS     BX, DWORD PTR Data:ActiveComPort+((Number-1)*4)

IFDEF EventLoggingOn
;Get the current millisecond time for the start of the interrupt
        StoreTime &Number&, &Number&, StartSig
ENDIF

;Go process the interrupt
        JMP     CommDisp

CommInt&Number& ENDP
ENDM

;================================================================ Code
CODE    SEGMENT WORD PUBLIC
        ASSUME CS:CODE, DS:DATA

;Pascal externals
        PUBLIC  CommInt1
        PUBLIC  CommInt2
        PUBLIC  CommInt3
        PUBLIC  CommInt4
        PUBLIC  CommInt5                                               ;!!.02
        PUBLIC  CommInt6                                               ;!!.02
        PUBLIC  CommInt7                                               ;!!.02
        PUBLIC  CommInt8                                               ;!!.02
        PUBLIC  CommTx
        PUBLIC  SendByte

IFDEF   EventLoggingOn
        PUBLIC  ReadTimer
ENDIF

;Setup the eight ISR stubs
        CommISR 1                       ;Creates CommInt1
        CommISR 2                       ;Creates CommInt2
        CommISR 3                       ;Creates CommInt3
        CommISR 4                       ;Creates CommInt4
        CommISR 5                       ;Creates CommInt5              !!.02
        CommISR 6                       ;Creates CommInt6              !!.02
        CommISR 7                       ;Creates CommInt7              !!.02
        CommISR 8                       ;Creates CommInt8              !!.02

IFDEF ShareIrqOn
;---------------------------------------------------------------- ChainToISR
;Chain to the previous ISR (sharing this IRQ)
ChainToISR      PROC    NEAR
;Finagle stack directly
        PUSH    BP                      ;Save the current BP
        MOV     BP,SP                   ;Prepare to access stack frame
        MOV     AX,[BP+6]               ;Get Seg of old ISR
        MOV     BX,[BP+4]               ;Get Ofs of old ISR
        XCHG    AX,WORD PTR[BP+014h]    ;Restore AX, set old seg
        XCHG    BX,WORD PTR[BP+012h]    ;Restore BX, set old ofs
        POP     BP                      ;Restore old BP
        ADD     SP,6                    ;Pop rtn addr and old seg,ofs

;Restore remaining regs
        POP     DI
        POP     ES
        POP     DS
        POP     DX
        POP     CX

;Go process old ISR (it will return directly to the interrupted process)
        RETF
ChainToISR      ENDP
ENDIF

;------------------------------------------------------------------ CommDisp
CommDisp        PROC    NEAR
;DS:BX already points to ActiveComPort[N]
        MOV     DX,[BX+BaseAddr]        ;DX = UART BaseAddr
        MOV     CX,DX                   ;Save in cx
        CLD                             ;For later STOBS's

IFDEF Debugging
;Prevent re-entrancy (during Periscope debugging)
        CMP     [BX+ISRActive],1        ;Is ISR already active?
        JNE     NotActive               ;No, say it is and keep going
        JMP     NoChain                 ;Yes, just leave
NotActive:
        MOV     [BX+ISRActive],1        ;Flag as active
ENDIF

;Get interrupt type
        ADD     DX,IIR
        IN      AL,DX                   ;Read IIR register into AL

;See if there is something to service
        SHR     AX,1                    ;Interrupt pending?
        JC      FalseInt                ;No, just leave

;Top of interrupt loop
IntLoop:
        RCL     AX,1                    ;Restore interrupt ID

IFDEF EventLoggingOn
;Store the interrupt mask
        StoreEvent 1, AX
ENDIF

;Jump to proper interrupt routine
        PUSH    AX                      ;Save IID reg for later FIFO check
        AND     AX,FifoMask             ;Mask off possible FIFO bits
        MOV     DI,AX                   ;Use int id as index to jump table
        JMP     WORD PTR CS:DispTbl[DI] ;Go process interrupt type
DispTbl LABEL   WORD
        DW      IntModemStatus
        DW      IntTransmit
        DW      IntReceive
        DW      IntBreak

;Not an interrupt, just leave
FalseInt:
        JMP     ToggleInts

;---------------------------------------------------------- Line/Break Ints
;Handle break/error interrupt
IntBreak:
        MOV     DX,CX                   ;DX = UART BaseAddr
        ADD     DX,LSR                  ;DX = UART line status register
        IN      AL,DX                   ;Read in LineStatus
        OR      [BX+LineStatus],AL      ;Or in the newly set bits
        TEST    AL,BreakMask            ;Break bit on?
        JE      NoBreak                 ;No
        MOV     [BX+BreakReceived],1    ;Set break flag in Port record
NoBreak:

IFDEF EventLoggingOn
;Store the new line status
        StoreEvent 2,AX
ENDIF

        JMP     EndCase

;----------------------------------------------------------Receive Ints
;Handle receiver interrupts
IntReceive:
        MOV     DX,CX                   ;DX = UART BaseAddr
        IN      AL,DX                   ;Get received character
        MOV     [BX+SaveChar],AL        ;Save char temp

IFDEF EventLoggingOn
;Store the received character
        StoreEvent 3, AX
ENDIF

IFDEF SWFlowOn
;Check transmitter software (xon/xoff) flow control
        TEST    [BX+SWFState],2         ;Checking for Xoff?
IFDEF TASMOn
JUMPS
        JZ      CheckBuffer             ;No, check for rcv flow control
NOJUMPS
ELSE
        JNZ     XFlowCheck              ;Yes, go check it
        JMP     CheckHdw                ;No, go check for hw flow control
ENDIF

;Did we get an Xoff?
XFlowCheck:
        CMP     AL,[BX+SWFOffChar]      ;Is it Xoff?
        JNE     CheckXon                ;No, check for Xon
        MOV     [BX+SWFGotXoff],1       ;Indicate Xoff received
        JMP     EndReceive              ;Finished

;Did we get an Xon?
CheckXon:
        CMP     AL,[BX+SWFOnChar]       ;Is it Xon?
        JNE     CheckBuffer             ;No, go check buffer hi-water mark
        MOV     [BX+SWFGotXoff],0       ;Clear Xoff in Port record
        CMP     [BX+OutBuffCount],0     ;Is OutBuff empty?
IFDEF TASMOn
JUMPS
        JE      EndReceive              ;Yes, finished
NOJUMPS
ELSE
        JNE     KickTransmit            ;No, go transmit
        JMP     EndReceive              ;Yes, finished
ENDIF

;Call the transmitter after receiving an Xon
KickTransmit:
        PUSH    DS                      ;Segment of Port record
        PUSH    BX                      ;Offset of Port record
        CALL    NEAR PTR CommTx         ;Call transmitter logic
        JMP     EndReceive              ;Finished

;See if the receive buffer has hit the xoff high water mark
CheckBuffer:
        TEST    [BX+SWFState],1         ;Checking for Xoff?
IFDEF TASMOn
JUMPS
        JZ      CheckHdw                ;No, check for hw flow control
NOJUMPS
ELSE
        JNZ     XFlowCheck1             ;Yes, go check it
        JMP     CheckHdw                ;No, check for hw flow control
ENDIF
XFlowCheck1:
        MOV     AX,[BX+InBuffCount]     ;AX = Current count
        CMP     AX,[BX+SWFFull]         ;AX > High-water mark
        JL      CheckHdw                ;No, continue
        CMP     [BX+SWFSentXoff],0      ;Already sent Xoff?
        JNE     CheckHdw                ;Yes, continue
        MOV     [BX+SWFSentXoff],1      ;Say we sent Xoff
        MOV     AH,[BX+SWFOffChar]      ;Xoff char
        MOV     DI,DS                   ;ES = DS
        MOV     ES,DI                   ;
        CALL    SendBytePrim            ;Send the Xoff (in AL)
ENDIF ;SWFlowOn

;Check for hardware flow control
CheckHdw:
IFDEF HWFlowOn
        TEST    [BX+HWFRecHonor],HWSignals ;Any HWFlow control to do?
        JZ      CheckRoom               ;No, continue

;Hardware flow control is on - do we need to turn remote off?
        MOV     AX,[BX+InBuffCount]     ;AX = Current count
        CMP     AX,[BX+HWFFull]         ;AX > Auto turn-off point
        JB      CheckRoom               ;No, go check buffer

        TEST    [BX+HWFRemoteOff],1     ;Already off?
        JNZ     CheckRoom               ;Yes, go check buffer
        MOV     AL,[BX+HWFRecHonor]     ;Get bits to honor
        XOR     [BX+ModemControl],AL    ;Flip them
        MOV     [BX+HWFRemoteOff],1     ;Note that we've turned remote off

;Set a new modem control value
        MOV     AL,[BX+ModemControl]    ;AL = new modem control value
        MOV     DX,CX                   ;DX = BaseAddr
        ADD     DX,MCR                  ;DX = modem control register
        OUT     DX,AL                   ;Set new value
ENDIF ;HWFlowOn

;Check for physical room in buffer
CheckRoom:
        MOV     AX,[BX+InBuffLen]       ;AX = Buffer length
        CMP     AX,[BX+InBuffCount]     ;AX > current count?
        JNA     ErrorReceive            ;No room, go report error

;Store this character to the head of the input buffer
GotRoom:
        MOV     AL,[BX+SaveChar]        ;Get the character back
        LES     DI,DWORD PTR [BX+InHead] ;ES:DI -> InHead
        STOSB                           ;Store the char to InHead
        INC     [BX+InBuffCount]        ;Inc buffer count
        MOV     WORD PTR [BX+InHead], DI ;Store the new InHead

IFDEF StatusBufferOn
;If using a status buffer, insert possible line errors into status buffer
        CMP     [BX+UseStatusBuffer], 1 ;Using status buffer?
        JNE     CheckWrap               ;No, continue
        TEST    [BX+LineStatus],LEMask  ;Check for error bits
        JZ      NoStatus                ;No errors, continue
        MOV     AL,[BX+LineStatus]      ;Get current line status
        AND     AL,LEMask               ;Only the error bits
        PUSH    DI                      ;Leave InHead in DI
        LES     DI,DWORD PTR [BX+StatHead] ;ES:DI -> StatHead
        STOSB                           ;Store the status
        POP     DI                      ;Restore DI to InHead
        AND     [BX+LineStatus],NOT LEMask ;Remove the error bits from LS
NoStatus:
        INC     WORD PTR [BX+StatHead]  ;Inc buffer head
ENDIF ;StatusBufferOn

;Check for buffer wrap
CheckWrap:
        CMP     DI,WORD PTR [BX+InBuffEnd] ;AX < ofs(InBuffEnd)?
        JB      EndReceive              ;Yes, continue
BufferWrap:
        MOV     AX,WORD PTR [BX+InBuff] ;AX = offset of start of buffer
        MOV     WORD PTR [BX+InHead],AX ;Set InHead to start of buffer

IFDEF StatusBufferOn
;If the input buffer wrapped, so does the status buffer
        CMP     [BX+UseStatusBuffer], 1 ;Using status buffer?
        JNE     EndReceive              ;No, continue
        MOV     AX,WORD PTR [BX+StatBuff] ;AX = offset of start of buffer
        MOV     WORD PTR [BX+StatHead],AX ;Set StatHead to start of buffer
ENDIF ;StatusBufferOn

;End of receive logic, if using a 16550 and FIFO is on, check line status
EndReceive:
        POP     AX                      ;AX = original IIR value
        TEST    AX, FifoOnMask          ;Are FIFO bits on?
IFDEF TASMOn
JUMPS
        JZ      EndCase1                ;No, go leave
NOJUMPS
ELSE
        JNZ     End1                    ;Yes...
        JMP     EndCase1                ;No, go leave
End1:
ENDIF
        PUSH    AX                      ;Yes, save IIR again
        MOV     DX,CX                   ;Get new line status value
        ADD     DX,LSR                  ;DX = line status reg
        IN      AL,DX                   ;AL = line status value
        OR      [BX+LineStatus],AL      ;Or in the newly set bits
        TEST    AX,1                    ;Is LSR0 bit set?
IFDEF TASMOn
JUMPS
        JNZ     IntReceive              ;Yes, go get more chars
NOJUMPS
ELSE
        JZ      End2                    ;No...
        JMP     IntReceive              ;Yes, go get more chars
End2:
ENDIF
        JMP     EndCase                 ;No, just leave

ErrorReceive:
        INC     [BX+LostCharCount]      ;No room, inc lost char count
        JMP     EndCase

;----------------------------------------------------------Transmit Ints
;Handle transmitter interrupts
IntTransmit:
        MOV     [BX+TxReady],1          ;Transmitter is available

;Update LineStatus on transmit interrupts
        MOV     DX,CX                   ;DX = UART BaseAddr
        ADD     DX,LSR                  ;DX = UART line status register
        IN      AL,DX                   ;Read in LineStatus
        OR      [BX+LineStatus],AL      ;Store in Port record

;Check for more data to transmit
        CMP     [BX+OutBuffCount],0     ;Tx buffer empty?
        JE      NoTransmit              ;Yes, go turn off transmit ints

;But skip attempt if transmits are blocked
        TEST    [BX+LastXmitError], XmitFailedHW+XmitFailedSW ;Check xmit flow
        JNZ     NoTransmit              ;Skip if xmit blocked

;Got data to transmit and transmits aren't blocked, go do it
        PUSH    DS                      ;Segment of PortRec
        PUSH    BX                      ;Offset of PortRec
        CALL    NEAR PTR CommTx         ;Try to transmit next char
        JMP     EndCase

;No more chars to transmit or transmit blocked - turn off transmit interrupts
NoTransmit:
        SUB     DX,4                    ;DX = UART interrupt enable register
        MOV     AL,AllIntsButTrans      ;AX = int mask for all but trans
        OUT     DX,AL                   ;Turns off transmit interrupts
        MOV     [BX+TxIntsActive],0     ;Say that they're off
        JMP     EndCase

;----------------------------------------------------------Modem Status Ints
;Handle modem status interrupts
IntModemStatus:
        MOV     DX,CX                   ;DX = BaseAddr
        ADD     DX,MSR                  ;DX = Modem status register
        IN      AL,DX                   ;Get the new modem status
        MOV     AH,[BX+ModemStatus]     ;Get the old modem status
        AND     AH,DeltaBits            ;Mask off all but old delta bits
        OR      AL,AH                   ;Merge old delta bits with new
        MOV     [BX+ModemStatus],AL     ;Save new status in Port record

IFDEF EventLoggingOn
;Store the new modem status
        StoreEvent 5, AX
ENDIF

;Try to start transmitting again
        CMP     [BX+OutBuffCount],0     ;Is OutBuff empty?
        JZ      EndCase                 ;Yes, we're finished
        PUSH    DS                      ;Segment of PortRec
        PUSH    BX                      ;Offset of PortRec
        CALL    NEAR PTR CommTx         ;Restart transmitter

;--------------------------------------------------------------EndCase
;Interrupt handled, see if any more
EndCase:
        POP     AX                      ;Discard old saved IIR value
EndCase1:
        MOV     DX,CX                   ;DX = BaseAddr
        ADD     DX,IIR                  ;DX = IIR reg
        IN      AL,DX                   ;AL = interrupt flags
        SHR     AX,1                    ;Shift bit 1 into carry flag
IFDEF TASMOn
JUMPS
        JNC     IntLoop                 ;Keep going if carry clear
NOJUMPS
ELSE
        JC      ToggleInts              ;No more to do if carry set
        JMP     IntLoop
ENDIF

ToggleInts:

IFDEF EventLoggingOn
;Get the current millisecond time for the end of the interrupt
        StoreTime &Number, 0, 0
ENDIF


;Toggle ints off/on (possibly generates another interrupt to the PIC)
        MOV     AL,EOIMask              ;Load non-specific EOI
        TEST    [BX+Flags],ptHiIrq      ;Is this ISR attached to a high IRQ?
        JZ      NoHiEOI                 ;No, skip the slave PIC
        OUT     EOIPortHi,AL            ;Yes, do it
NoHiEOI:
        OUT     EOIPort,AL              ;Always do EOI to master PIC

;Toggled interrupts off/on when exiting the ISR.
IFDEF UseToggle
        CMP     [BX+OldUart],1          ;Is this an old UART?
        JZ      NoToggle                ;Yes, skip this process
        MOV     DX,CX                   ;DX = BaseAddr of UART
        INC     DX                      ;DX = Interrupt Enable register
        IN      AL,DX                   ;Get current interrupt mask
        MOV     AH,AL                   ;Save it in AH
        XOR     AL,AL                   ;Zero means no interrupts
        OUT     DX,AL                   ;Do it
        MOV     AL,AH                   ;AL = original interrupt mask
        OUT     DX,AL                   ;Restore it (generates edge to PIC)
NoToggle:
ENDIF

IFDEF ShareIrqOn
;Check for a chained interrupt
        MOV     AX,CS                   ;Note current code segment
        CMP     AX, WORD PTR [BX+OrigPortState+0Ah] ;Current CS = CS of old ISR?
        JNE     NoChain                 ;No, don't chain
        PUSH    WORD PTR [BX+OrigPortState+0Ah] ;Push old seg
        PUSH    WORD PTR [BX+OrigPortState+08h] ;Push old ofs
        CALL    ChainToISR              ;Chain to previous ISR
        ;This will return directly to the interrupted program
ENDIF

IFDEF Debugging
;Flag kernel as no longer active
        MOV     [BX+ISRActive],0
ENDIF

;Restore the registers
NoChain:
        POP     DI
        POP     ES
        POP     DS
        POP     DX
        POP     CX
        POP     BX
        POP     AX
        IRET
CommDisp        ENDP

;-------------------------------------------------------------------CommTx
;Try to transmit one character (callable from a Pascal unit)
;procedure CommTx(PPtr : PortRecPtr);
;
CommTx  PROC    NEAR
PortOfs EQU     [BP+4]
PortSeg EQU     [BP+6]

;Set stack frame
        PUSH    BP
        MOV     BP,SP

;Establish Port addressability
        PUSH    DS
        MOV     DS,PortSeg              ;DS:BX -> Port
        MOV     BX,PortOfs

IFDEF HWFlowOn
;Check HWFlow requirements
        MOV     AH,[BX+ModemStatus]     ;Get latest modem status word
        XOR     AH,[BX+HWFTransMask]    ;Set bits to zero if signal is "on"
        MOV     AL,[BX+HWFTransHonor]   ;Get mask to honor
        AND     AL,AH                   ;Compare current mask to honor mask
        JZ      CheckSWFlow             ;Ok to trans, go check SWFlow
        MOV     [BX+LastXmitError], XmitFailedHW ;Blocked, set error and exit
        JMP     ExitCommTx
ENDIF

;Check SFLow requirements
CheckSWFlow:
IFDEF SWFlowOn
        TEST    [BX+SWFState],2         ;Tracking Xoff?
        JZ      SkipXoff                ;No, skip it
        CMP     [BX+SWFGotXoff],1       ;Yes, got an Xoff?
        MOV     [BX+LastXmitError], XmitFailedSW  ;Yes, set error and exit
        JE      ExitCommTx
SkipXoff:
ENDIF

;Turn on transmit interrupts
        MOV     DX,[BX+BaseAddr]        ;DX = BaseAddr
        INC     DX                      ;DX = interrupt enable register
        MOV     AL,AllIntsOn            ;AL = mask to enable all interrupts
        OUT     DX,AL                   ;Turn all ints on
        MOV     [BX+TxIntsActive],1     ;Say they are on

;Transmit the character
        LES     DI,DWORD PTR [BX+OutTail]
        MOV     AH,ES:[DI]              ;Fetch next char
        CALL    SendBytePrim            ;Use common send (char in AH)
        MOV     [BX+LastXmitError],0    ;No xmit errors
        DEC     [BX+OutBuffCount]       ;Dec xmit count
        INC     WORD PTR [BX+OutTail]   ;Bump pointer

;Check for buffer wraparound
        MOV     AX,WORD PTR [BX+OutTail]
        CMP     AX,WORD PTR [BX+OutBuffEnd]
        JNE     ExitCommTx

;Wrap around occurred, reset the tail pointer
        MOV     AX,WORD PTR [BX+OutBuff]
        MOV     WORD PTR [BX+OutTail],AX

ExitCommTx:
        POP     DS
        POP     BP
        RET     4
CommTx  ENDP

;----------------------------------------------------------------SendBytePrim
;Transmits one byte
;Parms passed by register:
;  DS:BX -> PortRec
;  AH       Char to transmit
;
SendBytePrim PROC NEAR
;TxInts might be off when ISR sends xon/xoff. If so, skip TxReady logic
        CMP     [BX+TxIntsActive],1             ;Are transmit ints on?
        JNE     NoTxInt                         ;No, skip TxReady flag set
        MOV     [BX+TxReady],0                  ;Say Xmitter not available
NoTxInt:

;Wait for THRE (only needed in a few cases)
        MOV     DX,[BX+BaseAddr]                ;DX = Baseaddr
        ADD     DX,LSR                          ;DX = line status register
Check:  IN      AL,DX                           ;AL = line status
IFDEF HandleTPort
        TEST    AL,THREandTEMask                ;Is transmitter empty?
ELSE
        TEST    AL,THREMask                     ;Is transmitter empty?
ENDIF
        JZ      Check

;Send the character
        MOV     AL,AH                           ;AL = char to send
        MOV     DX,WORD PTR [BX+BaseAddr]       ;DX = BaseAddr
        OUT     DX,AL                           ;Send it

IFDEF EventLoggingOn
;Store the just transmitted char
        StoreEvent 4, AX
ENDIF

        RET
SendBytePrim ENDP

;----------------------------------------------------------------SendByte
;Transmits one byte (callable from a Pascal unit)
;procedure SendByte(Ch : Char; PPtr : PortRecPtr);
;
SendByte PROC NEAR
ChB     EQU     [BP+4]
PortOfs EQU     [BP+6]
PortSeg EQU     [BP+8]

        PUSH    BP
        MOV     BP,SP
        PUSH    DS

        MOV     DS,PortSeg              ;DS:BX -> Port
        MOV     BX,PortOfs
        MOV     AH, ChB
        CALL    SendBytePrim            ;Go output the character
        POP     DS
        POP     BP
        RET     6
SendByte ENDP

IFDEF   EventLoggingOn
;----------------------------------------------------------------ReadTimer;
;Returns a LongInt timer value (assumes reprogrammed timer)
;function ReadTimer : LongInt;
;
ReadTimer PROC FAR
        PUSH    BP
        MOV     BP,SP
        SUB     SP, 4

        MOV     DX,20h                  ;Address PIC ocw3
        MOV     AL,0Ah                  ;Ask to read irr
        OUT     DX,AL
        MOV     AL,0                    ;Latch timer 0
        OUT     43h,AL
        IN      AL,DX                   ;Read irr
        MOV     DI,AX                   ;Save it in DI
        IN      AL,40h                  ;Counter --> bx
        MOV     BL,AL                   ;LSB in BL
        IN      AL,40h
        MOV     BH,AL                   ;MSB in BH
        NOT     BX                      ;Need ascending counter
        IN      AL,21h                  ;Read PIC imr
        MOV     SI,AX                   ;Save it in SI
        MOV     AL,0FFh                 ;Mask all interrupts
        OUT     21h,AL
        MOV     AX,40h                  ;read low word of time
        MOV     ES,AX                   ;from BIOS data area
        MOV     DX,ES:[6Ch]
        MOV     AX,SI                   ;Restore imr from SI
        OUT     21h,AL
        MOV     AX,DI                   ;Retrieve old irr
        TEST    AL,1                    ;Counter hit 0?
        JZ      Done                    ;Jump if not
        CMP     BX,0FFh                 ;Counter > $FF?
        JA      Done                    ;Done if so
        INC     DX                      ;Else count int req.

Done:
        MOV     AX,BX                   ;AX,DX = LongInt result
        MOV     SP,BP
        POP     BP
        RET
ReadTimer ENDP
ENDIF

Code    ENDS
        END
