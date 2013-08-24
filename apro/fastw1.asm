
;       FASTW1.ASM
;       Fast screen writing routines
;       By Brian Foley
;       Last update: 6/7/90


;****************************************************** Equates

        Mono    = 0
        CGA     = 1
        EGA     = 2
        MCGA    = 3
        VGA     = 4

;****************************************************** Data

DATA    SEGMENT BYTE PUBLIC

        EXTRN   BaseOfScreen : WORD     ;Pascal variables
        EXTRN   WaitForRetrace : BYTE

        CurrentMode     DB      ?       ;local variables
        Display         DB      ?

DATA    ENDS

;****************************************************** Code

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE,DS:DATA

        PUBLIC  FastWrite, FastWriteNA, ChangeAttribute, MoveFromScreen
        PUBLIC  MoveToScreen, CurrentDisplay, CurrentVideoMode

;****************************************************** CalcOffset

;Calculates offset in video memory corresponding to Row,Column
;On entry, CH has Row, BX has Column (both 1-based)
;On exit, ES:DI points to proper address in video memory, AX = 0

CalcOffset      PROC NEAR

        XOR     AX,AX                   ;AX = 0
        MOV     CL,AL                   ;CL = 0
        MOV     BH,AL                   ;BH = 0
        DEC     CH                      ;Row (in CH) to 0..24 range
        SHR     CX,1                    ;CX = Row * 128
        MOV     DI,CX                   ;Store in DI
        SHR     DI,1                    ;DI = Row * 64
        SHR     DI,1                    ;DI = Row * 32
        ADD     DI,CX                   ;DI = (Row * 160)
        DEC     BX                      ;Col (in BX) to 0..79 range
        SHL     BX,1                    ;Account for attribute bytes
        ADD     DI,BX                   ;DI = (Row * 160) + (Col * 2)
        MOV     ES,BaseOfScreen         ;ES:DI points to BaseOfScreen:Row,Col
        RET                             ;Return

CalcOffset      ENDP

;****************************************************** FastWrite

;procedure FastWrite(St : String; Row, Col, Attr : Byte);
;Write St at Row,Col in Attr (video attribute) without snow

;equates for parameters:
FWAttr          EQU     BYTE PTR [BP+6]
FWCol           EQU     BYTE PTR [BP+8]
FWRow           EQU     BYTE PTR [BP+10]
FWSt            EQU     DWORD PTR [BP+12]

FastWrite       PROC FAR

        PUSH    BP                      ;Save BP
        MOV     BP,SP                   ;Set up stack frame
        PUSH    DS                      ;Save DS
        MOV     CH,FWRow                ;CH = Row
        MOV     BL,FWCol                ;BL = Column
        CALL    CalcOffset              ;Call routine to calculate offset
        MOV     CL,WaitForRetrace       ;Grab this before changing DS
        LDS     SI,FWSt                 ;DS:SI points to St[0]
        CLD                             ;Set direction to forward
        LODSB                           ;AX = Length(St); DS:SI -> St[1]
        XCHG    AX,CX                   ;CX = Length; AL = WaitForRetrace
        JCXZ    FWExit                  ;If string empty, exit
        MOV     AH,FWAttr               ;AH = Attribute
        RCR     AL,1                    ;If WaitForRetrace is False...
        JNC     FWMono                  ; use "FWMono" routine
        MOV     DX,03DAh                ;Point DX to CGA status port
FWGetNext:
        LODSB                           ;Load next character into AL
                                        ; AH already has Attr
        MOV     BX,AX                   ;Store video word in BX
;        CLI                             ;No interrupts now
FWWaitNoH:
        IN      AL,DX                   ;Get 6845 status
        TEST    AL,8                    ;Vertical retrace in progress?
        JNZ     FWStore                 ;If so, go
        RCR     AL,1                    ;Else, wait for end of
        JC      FWWaitNoH               ; horizontal retrace
FWWaitH:
        IN      AL,DX                   ;Get 6845 status again
        RCR     AL,1                    ;Wait for horizontal
        JNC     FWWaitH                 ; retrace
FWStore:
        MOV     AX,BX                   ;Move word back to AX...
        STOSW                           ; and then to screen
;        STI                             ;Allow interrupts!
        LOOP    FWGetNext               ;Get next character
        JMP     FWExit                  ;Done
FWMono:
        LODSB                           ;Load next character into AL
                                        ; AH already has Attr
        STOSW                           ;Move video word into place
        LOOP    FWMono                  ;Get next character
FWExit:
        POP     DS                      ;Restore DS
        MOV     SP,BP                   ;Restore SP
        POP     BP                      ;Restore BP
        RET     10                      ;Remove parameters and return

FastWrite       ENDP

;****************************************************** FastWriteNA

;procedure FastWriteNA(St : String; Row, Col : Byte);
;Write St at Row,Col without snow, and don't change video attributes

;equates for parameters:
FNCol           EQU     BYTE PTR [BP+6]
FNRow           EQU     BYTE PTR [BP+8]
FNSt            EQU     DWORD PTR [BP+10]

FastWriteNA     PROC FAR

        PUSH    BP                      ;Save BP
        MOV     BP,SP                   ;Set up stack frame
        PUSH    DS                      ;Save DS
        MOV     CH,FNRow                ;CH = Row
        MOV     BL,FNCol                ;BL = Column
        CALL    CalcOffset              ;Call routine to calculate offset
        MOV     CL,WaitForRetrace       ;Grab this before changing DS
        LDS     SI,FNSt                 ;DS:SI points to St[0]
        CLD                             ;Set direction to forward
        LODSB                           ;AX = Length(St); DS:SI -> St[1]
        XCHG    AX,CX                   ;CX = Length; AL = Wait
        JCXZ    FNExit                  ;If string empty, exit
        RCR     AL,1                    ;If WaitForRetrace is False...
        JNC     FNNoWait                ; use FNNoWait routine
        MOV     DX,03DAh                ;Point DX to CGA status port
FNGetNext:
        LODSB                           ;Load next character into AL
        MOV     AH,AL                   ;Store char in AH
;        CLI                             ;No interrupts now
FNWaitNoH:
        IN      AL,DX                   ;Get 6845 status
        TEST    AL,8                    ;Check for vertical retrace
        JNZ     FNStore                 ; In progress? go
        RCR     AL,1                    ;Else, wait for end of
        JC      FNWaitNoH               ; horizontal retrace
FNWaitH:
        IN      AL,DX                   ;Get 6845 status again
        RCR     AL,1                    ;Wait for horizontal
        JNC     FNWaitH                 ; retrace
FNStore:
        MOV     AL,AH                   ;Move char back to AL...
        STOSB                           ; and then to screen
        STI                             ;Allow interrupts
        INC     DI                      ;Skip attribute bytes
        LOOP    FNGetNext               ;Get next character
        JMP     FNExit                  ;Done
FNNoWait:
        MOVSB                           ;Move character to screen
        INC     DI                      ;Skip attribute bytes
        LOOP    FNNoWait                ;Get next character
FNExit:
        POP     DS                      ;Restore DS
        MOV     SP,BP                   ;Restore SP
        POP     BP                      ;Restore BP
        RET     8                       ;Remove parameters and return

FastWriteNA     ENDP

;****************************************************** ChangeAttribute

;procedure ChangeAttribute(Number : Word; Row, Col, Attr : Byte);
;Change Number video attributes to Attr starting at Row,Col

;equates for parameters:
CAAttr          EQU     BYTE PTR [BP+6]
CACol           EQU     BYTE PTR [BP+8]
CARow           EQU     BYTE PTR [BP+10]
CANumber        EQU     WORD PTR [BP+12]

ChangeAttribute PROC FAR

        PUSH    BP                      ;Save BP
        MOV     BP,SP                   ;Set up stack frame
        MOV     CH,CARow                ;CH = Row
        MOV     BL,CACol                ;BL = Column
        CALL    CalcOffset              ;Call routine to calculate offset
        INC     DI                      ;Skip character
        CLD                             ;Set direction to forward
        MOV     CX,CANumber             ;CX = Number to change
        JCXZ    CAExit                  ;If zero, exit
        MOV     AL,CAAttr               ;AL = Attribute
        CMP     WaitForRetrace,1        ;Get wait state
        JNE     CANoWait                ;If WaitForRetrace is False
                                        ; use CANoWait routine
        MOV     AH,AL                   ;Store attribute in AH
        MOV     DX,03DAh                ;Point DX to CGA status port
CAGetNext:
;        CLI                             ;No interrupts now
CAWaitNoH:
        IN      AL,DX                   ;Get 6845 status
        TEST    AL,8                    ;Check for vert. retrace
        JNZ     CAGo                    ;In progress? Go
        RCR     AL,1                    ;Wait for end of horizontal
        JC      CAWaitNoH               ; retrace
CAWaitH:
        IN      AL,DX                   ;Get 6845 status again
        RCR     AL,1                    ;Wait for horizontal
        JNC     CAWaitH                 ; retrace
CAGo:
        MOV     AL,AH                   ;Move Attr back to AL...
        STOSB                           ; and then to screen
        STI                             ;Allow interrupts
        INC     DI                      ;Skip characters
        LOOP    CAGetNext               ;Look for next opportunity
        JMP     CAExit                  ;Done
CANoWait:
        STOSB                           ;Change the attribute
        INC     DI                      ;Skip characters
        LOOP    CANoWait                ;Get next character
CAExit:                                 ;Next instruction
        MOV     SP,BP                   ;Restore SP
        POP     BP                      ;Restore BP
        RET     8                       ;Remove parameters and return

ChangeAttribute ENDP

;****************************************************** MoveFromScreen

;procedure MoveFromScreen(var Source, Dest; Length : Word);
;Move Length words from Source (video memory) to Dest without snow

;equates for parameters:
MFLength        EQU     WORD PTR [BP+6]
MFDest          EQU     DWORD PTR [BP+8]
MFSource        EQU     DWORD PTR [BP+12]

MoveFromScreen  PROC FAR

        PUSH    BP                      ;Save BP
        MOV     BP,SP                   ;Set up stack frame
        MOV     BX,DS                   ;Save DS in BX
        MOV     AL,WaitForRetrace       ;Grab before changing DS
        LES     DI,MFDest               ;ES:DI points to Dest
        LDS     SI,MFSource             ;DS:SI points to Source
        MOV     CX,MFLength             ;CX = Length
        CLD                             ;Set direction to forward
        RCR     AL,1                    ;Check WaitForRetrace
        JNC     MFNoWait                ;False? Use MFNoWait routine
        MOV     DX,03DAh                ;Point DX to CGA status port
MFNext:
;        CLI                             ;No interrupts now
MFWaitNoH:
        IN      AL,DX                   ;Get 6845 status
        TEST    AL,8                    ;Check for vertical retrace
        JNZ     MFGo                    ;In progress? go
        RCR     AL,1                    ;Wait for end of horizontal
        JC      MFWaitNoH               ; retrace
MFWaitH:
        IN      AL,DX                   ;Get 6845 status again
        RCR     AL,1                    ;Wait for horizontal
        JNC     MFWaitH                 ; retrace
MFGo:
        LODSW                           ;Load next video word into AX
        STI                             ;Allow interrupts
        STOSW                           ;Store video word in Dest
        LOOP    MFNext                  ;Get next video word
        JMP     MFExit                  ;All Done
MFNoWait:
        REP     MOVSW                   ;That's it!
MFExit:
        MOV     DS,BX                   ;Restore DS
        MOV     SP,BP                   ;Restore SP
        POP     BP                      ;Restore BP
        RET     10                      ;Remove parameters and return

MoveFromScreen  ENDP

;****************************************************** MoveToScreen

;procedure MoveToScreen(var Source, Dest; Length : Word);
;Move Length words from Source to Dest (video memory) without snow

;equates for parameters:
MTLength        EQU     WORD PTR [BP+6]
MTDest          EQU     DWORD PTR [BP+8]
MTSource        EQU     DWORD PTR [BP+12]

MoveToScreen    PROC FAR

        PUSH    BP                      ;Save BP
        MOV     BP,SP                   ;Set up stack frame
        PUSH    DS                      ;Save DS
        MOV     AL,WaitForRetrace       ;Grab before changing DS
        LES     DI,MTDest               ;ES:DI points to Dest
        LDS     SI,MTSource             ;DS:SI points to Source
        MOV     CX,MTLength             ;CX = Length
        CLD                             ;Set direction to forward
        RCR     AL,1                    ;Check WaitForRetrace
        JNC     MTNoWait                ;False? Use MTNoWait routine
        MOV     DX,03DAh                ;Point DX to CGA status port
MTGetNext:
        LODSW                           ;Load next video word into AX
        MOV     BX,AX                   ;Store video word in BX
;        CLI                             ;No interrupts now
MTWaitNoH:
        IN      AL,DX                   ;Get 6845 status
        TEST    AL,8                    ;Check for vertical retrace
        JNZ     MTGo                    ;In progress? Go
        RCR     AL,1                    ;Wait for end of horizontal
        JC      MTWaitNoH               ; retrace
MTWaitH:
        IN      AL,DX                   ;Get 6845 status again
        RCR     AL,1                    ;Wait for horizontal
        JNC     MTWaitH                 ; retrace
MTGo:
        MOV     AX,BX                   ;Move word back to AX...
        STOSW                           ; and then to screen
        STI                             ;Allow interrupts
        LOOP    MTGetNext               ;Get next video word
        JMP     MTExit                  ;All done
MTNoWait:
        REP     MOVSW                   ;That's all!
MTExit:
        POP     DS                      ;Restore DS
        MOV     SP,BP                   ;Restore SP
        POP     BP                      ;Restore BP
        RET     10                      ;Remove parameters and return

MoveToScreen    ENDP

;****************************************************** CurrentModePrim

;Returns current video mode in AL

CurrentModePrim PROC NEAR

        MOV     AH,0Fh                  ;Get video mode function
        INT     10h                     ;Call BIOS
        MOV     CurrentMode,AL          ;Save it
        RET                             ;Return

CurrentModePrim ENDP

;****************************************************** CurrentDisplay

;function CurrentDisplay : DisplayType;
;Returns type of the currently active display.

JunkValue       = 0FFFFh

CurrentDisplay  PROC FAR

        ;get the current video mode
        CALL     CurrentModePrim        ;Call primitive routine

        ;test for VGA
        MOV     Display,VGA             ;Assume VGA
        MOV     CX,JunkValue            ;Load CX with junk value
        MOV     AX,1C00h                ;Save/Restore video state
        INT     10h
        CMP     AL,1Ch                  ;AL = $1C signals valid call
        JE      CDexit                  ;Adapter type known

        ;test for MCGA
        MOV     Display,MCGA            ;Assume MCGA
        MOV     BL,32h                  ;Choose Enable
        MOV     AX,1200h                ;Enable/Disable video addressing
        INT     10h
        CMP     AL,12h                  ;AL = $12 signals valid call
        JE      CDexit                  ;Adapter type known

        ;test for EGA
        MOV     Display,EGA             ;Assume EGA
        MOV     BX,0FF10h               ;Return EGA information
        MOV     CX,JunkValue            ;Load CX with junk value
        MOV     AX,1200h                ;Alternate function select
        INT     10h
        XOR     AL,AL                   ;AL = 0
        CMP     CX,JunkValue            ;CX unchanged?
        JE      CDplain                 ;If so, not an EGA
        CMP     BH,1                    ;BH should be 0 or 1
        JA      CDplain                 ;Mono or CGA if it isn't

        ;See if EGA is the active display
        CMP     BH,1                    ;mono display?
        JE      CDegaMono               ;If so, use mono check
        CMP     CurrentMode,7           ;In mono mode?
        JE      CDplain                 ;If so, we're not on the EGA
        JMP     SHORT CDexit            ;else, it's an EGA

CDegaMono:
        CMP     CurrentMode,7           ;Current mode = 7?
        JNE     CDplain                 ;Exit if not

CDexit:
        MOV     AL,Display              ;set return value
        RET                             ;Return

CDplain:
        MOV     Display,CGA             ;Assume CGA
        CMP     CurrentMode,7           ;Mono mode
        JNE     CDexit                  ;Done if not
        MOV     Display,Mono            ;Else, Mono
        JMP     SHORT CDexit            ;Done

CurrentDisplay  ENDP

;****************************************************** CurrentVideoMode

;function CurrentVideoMode : Byte;
;Returns current video mode in AL

CurrentVideoMode        PROC FAR

        CALL     CurrentModePrim        ;Call primitive routine
        RET                             ;Return

CurrentVideoMode        ENDP

CODE    ENDS

        END
