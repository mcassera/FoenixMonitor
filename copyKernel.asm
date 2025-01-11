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



* = $7000

start:
copyKernel:
        stz $01
        lda #$e0
        sta tempII+1
        lda #$50
        sta tempII2+1
        stz tempII
        stz tempII2
    
        ldy #$00
cpyLoop:
        lda (tempII),y
        sta (tempII2),Y
        iny
        bne cpyLoop
        inc tempII+1
        inc tempII2+1
        lda tempII+1
        bne cpyLoop

        sei 
        lda $00
        sta buff

        lda #$b3
        sta $00
        lda #$1f                        ; memory $3:e000
        sta $0f                         ; slot 7

        lda buff
        sta $00

        lda #$e0
        sta tempII+1
        lda #$50
        sta tempII2+1
cpyBack:
        lda (tempII2),y
        sta (tempII),Y
        iny
        bne cpyBack
        inc tempII+1
        inc tempII2+1
        lda tempII+1
        bne cpyBack


        lda #$2c
        sta $fffa
 
        lda #$7e
        sta $fffb

        ;lda #$79
        ;sta $fffe
        ;lda #$7e
        ;sta $ffff

        cli
        rts

buff:   .byte $00