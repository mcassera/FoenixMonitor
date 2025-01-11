; mnuemonic table for 65c02 assembly

opcode:

;         x0    x1    x2    x3    x4    x5    x6    x7    x8    x9    xa    xb    xc    xd    xe    xf   
.text   "brk","ora","???","???","tsb","ora","asl","???","php","ora","asl","???","tsb","ora","asl","???"     ;0x
.text   "bpl","ora","ora","???","trb","ora","asl","???","clc","ora","inc","???","trb","ora","asl","???"     ;1x
.text   "jsr","and","???","???","bit","and","rol","???","plp","and","rol","???","bit","and","rol","???"     ;2x
.text   "bmi","and","and","???","bit","and","rol","???","sec","and","dec","???","bit","and","rol","???"     ;3x
.text   "rti","eor","???","???","???","eor","lsr","???","pha","sec","lsr","???","jmp","eor","lsr","???"     ;4x
.text   "bvc","eor","eor","???","???","eor","lsr","???","cli","eor","phy","???","???","eor","lsr","???"     ;5x
.text   "rts","adc","???","???","stz","adc","ror","???","pla","adc","ror","???","jmp","adc","ror","???"     ;6x
.text   "bvs","adc","adc","???","stz","adc","ror","???","sei","adc","ply","???","jmp","adc","tot","???"     ;7x
.text   "bra","sta","???","???","sty","sta","stx","???","dey","bit","txa","???","sty","sta","stx","???"     ;8x
.text   "bcc","sta","sta","???","sty","sta","stx","???","txa","sta","txs","???","stz","sta","stz","???"     ;9x
.text   "ldy","lda","ldx","???","ldy","lda","ldx","???","tay","lda","tax","???","ldy","lda","ldx","???"     ;ax
.text   "bcs","lda","lda","???","ldy","lda","ldx","???","clv","lda","tsx","???","ldy","lda","ldx","???"     ;bx
.text   "cpy","cmp","???","???","cpy","cmp","dec","???","iny","cmp","dex","???","cpy","cmp","dec","???"     ;cx
.text   "bne","cmp","cmp","???","???","cmp","dec","???","cld","cmp","phx","???","???","cmp","dec","???"     ;dx
.text   "cpx","sbc","???","???","cpx","sbc","inc","???","inx","sbc","nop","???","cpx","sbc","inc","???"     ;ex
.text   "beq","sbc","sbc","???","???","sbc","inc","???","sed","sbc","plx","???","???","sbc","inc","???"     ;fx

;addressing modes
; $00 - implied
; $06 - a
; $0c - #$--
; $12 - $----
; $18 - $--
; $1e - $----,x
; $24 - $----,y
; $2a - $--,x
; $30 - $--,y
; $36 - relative addressing
; $3c - ($--,x)
; $42 - ($----,x)
; $48 - ($--),y
; $4e - ($--)
; $54 - ($----)

addressMode:

;        x0  x1  x2  x3  x4  x5  x6  x7  x8  x9  xa  xb  xc  xd  xe  xf        
.byte   $00,$3c,$00,$00,$18,$18,$18,$00,$00,$0c,$06,$00,$12,$12,$12,$00     ;0x
.byte   $36,$48,$4e,$00,$18,$2a,$2a,$00,$00,$24,$06,$00,$12,$1e,$1e,$00     ;1x
.byte   $12,$3c,$00,$00,$18,$18,$18,$00,$00,$0c,$06,$00,$12,$12,$12,$00     ;2x
.byte   $36,$48,$4e,$00,$2a,$2a,$2a,$00,$00,$24,$06,$00,$1e,$1e,$1e,$00     ;3x
.byte   $00,$3c,$00,$00,$00,$18,$18,$00,$00,$0c,$06,$00,$12,$12,$12,$00     ;4x
.byte   $36,$48,$4e,$00,$00,$2a,$2a,$00,$00,$24,$00,$00,$00,$1e,$1e,$00     ;5x
.byte   $00,$3c,$00,$00,$18,$18,$18,$00,$00,$0c,$06,$00,$54,$12,$12,$00     ;6x
.byte   $36,$48,$4e,$00,$2a,$2a,$2a,$00,$00,$24,$00,$00,$42,$1e,$1e,$00     ;7x
.byte   $36,$3c,$00,$00,$18,$18,$18,$00,$00,$0c,$00,$00,$12,$12,$12,$00     ;8x
.byte   $36,$48,$4e,$00,$2a,$2a,$30,$00,$00,$24,$00,$00,$12,$1e,$1e,$00     ;9x
.byte   $0c,$3c,$0c,$00,$18,$18,$18,$00,$00,$0c,$00,$00,$12,$12,$12,$00     ;ax
.byte   $36,$48,$4e,$00,$2a,$2a,$30,$00,$00,$24,$00,$00,$1e,$1e,$24,$00     ;bx
.byte   $0c,$3c,$00,$00,$18,$18,$18,$00,$00,$0c,$00,$00,$12,$12,$12,$00     ;cx
.byte   $36,$48,$4e,$00,$00,$18,$2a,$00,$00,$24,$00,$00,$00,$1e,$1e,$00     ;dx
.byte   $0c,$3c,$00,$00,$18,$18,$18,$00,$00,$0c,$00,$00,$12,$12,$12,$00     ;ex
.byte   $36,$48,$4e,$00,$00,$2a,$2a,$00,$00,$24,$00,$00,$00,$1e,$1e,$00     ;fx

addOut:
.text   "      "
.text   "a     "
.text   "#$B   "
.text   "$W    "
.text   "$B    "
.text   "$W,x  "
.text   "$W,y  "
.text   "$B,x  "
.text   "$B,y  "
.text   "$R    "
.text   "($B,x)"
.text   "($W,x)"
.text   "($B),y"
.text   "($B)  "
.text   "($W)  "