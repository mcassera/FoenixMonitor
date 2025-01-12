.cpu "w65c02"	                                                                ; Tells 64TASS to enable the extra 65c02 assembly commands

cursor      =   $e8                                                             ; cureent screen PO (2 bytes)
dLine       =   $ea                                                             ; current disassembly line (2 bytes)
tempII      =   $ec                                                             ; used for indirect indexing
tempII2     =   $ee                                                             ; used for indorect indexing 2
MULT_A      =   $de00                                                           ; 2 bytes
MULT_B      =   $de02                                                           ; 2 bytes
MULT_result =   $de10                                                           ; 4 bytes
ADD_A       =   $de08                                                           ; 4 bytes
ADD_B       =   $de0c                                                           ; 4 bytes
ADD_result  =   $de18                                                           ; 4 bytes
MMU_CTRL    =   $01
BCURSOR     =   $d010
BASIC_WARM  =   $8908

PCH             =       $fd00                                                   ; program counter high byte
PCL             =       $fd01                                                   ; program counter low byte
SR              =       $fd02                                                   ; status register
ACC             =       $fd03                                                   ; accumulator
XR              =       $fd04                                                   ; X register
YR              =       $fd05                                                   ; Y register
SP              =       $fd06                                                   ; stack pointer
buff            =       $fd07
NMIFlag         =       $fd08                                                   ; disable if already activated.



* = $7f00
Start:



Call:
        inc NMIFlag
        lda #<Call
        sta PCL
        lda #>Call
        sta PCH
        stx XR
        sty YR
        php
        pla
        sta SR
        tsx 
        stx SP
        jsr SetMonitor
        jsr $a025
        jsr ReturnfromMonitor
        jsr insertRTS
        stz NMIFlag
        rts
NMI:
        pha
        lda NMIFlag
        beq contNMI
        pla
        cli
        rti
contNMI:
        pla
        sta ACC
        stx XR
        sty YR
        pla
        sta SR
        pla 
        sta PCL
        pla 
        sta PCH
        tsx
        stx SP
        inc NMIFlag
        jsr SetMonitor
        jsr $a025
        jsr ReturnfromMonitor
        jsr insertRTS
        sei
        stz NMIFlag
        lda PCH
        pha 
        lda PCL
        pha 
        lda SR
        pha 
        lda ACC
        ldx XR
        ldy YR
        cli
        rti
Break:
        sta ACC
        stx XR
        sty YR  
        pla
        sta SR
        pla 
        sta PCL 
        pla 
        sta PCH
        tsx
        stx SP
        inc NMIFlag
        inc NMIFlag
        jsr SetMonitor
        jsr $a025
        jsr ReturnfromMonitor
        jsr insertRTS
        sei
        stz NMIFlag
        cli
        jmp BASIC_WARM

SetMonitor:   
        sei
        lda $00
        sta buff
        lda #$b3
        sta $00

        lda #$1e 
        sta $0d

        lda buff
        sta $00

        cli
        rts

ReturnfromMonitor:
        sei
        lda $00
        sta buff
        lda #$b3
        sta $00

        lda #$43 
        sta $0d

        lda buff
        sta $00      
        cli
        rts

insertRTS:
        
        lda #$0d
        sta $0678
        lda #$01
        sta $0680
        rts


