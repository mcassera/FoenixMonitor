; T2 - Intro to the MicroKERNEL - Monitor/Debugger
; 2024 mcassera




.cpu "w65c02"	                                                                ; Tells 64TASS to enable the extra 65c02 assembly commands
.include "api.asm"	                                                        ; This is the Kernel API for communication



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

PCH             =       $fd00                             ; program counter high byte
PCL             =       $fd01                              ; program counter low byte
SR              =       $fd02                             ; status register
ACC             =       $fd03                              ; accumulator
XR              =       $fd04                            ; X register
YR              =       $fd05                              ; Y register
SP              =       $fd06                             ; stack pointer
NMIFlag         =       $fd08                                                   ; disable if already activated.

* = $a000
Start:

*=$e0								                ; Set up buffer for Kernel communication
	.dsection zp						                ; Define position for zp (zero page)
	.cerror * > $e7, "Too many Zero page variables for the Kernel"

* = $a000
    .dsection code 
	.cerror * > $bfff, "code too long, encroaching on I/O Screen Ram"

        
        	                                

SetupKernel:							                ; Set up the API to work with

	.section zp						                ; Zero page section $c0 to $c6
event:	.dstruct	kernel.event.event_t
        .send


        .section code
       ;.include "copyKernel.asm"
        
                .text"FOENIX MONITOR - 2025 MICHAEL CASSERA"
nmiEntrance:
                cli
                stz $01
                stz buff
                lda #$04 
                sta BCURSOR                                                     ; turns off the flashing Basic Cursor
textColor:
		ldx #$00
_tcLoop:
		lda textColorLut,x
		sta $d800,x
		sta $d840,x
		inx
		cpx #$41
		bne _tcLoop

                ;jsr clearScreen
                ;lda #$00                                                        ; We'll start by setting the X and Y position of the cursor
                lda $07eb
                sta xPOS                                                        ; to 0,3
                ;lda #$03
                lda $07ea
                sta yPOS
                jsr getScreenPos                                                ; Translate the X,Y position into a memory location on the text/color matrix
                lda #$03                                                        ; First, we need to set the I/O control to 3 so we can address the color matrix
                sta $01                                                         ;
                lda #$26                                                        ; Then we'll set the color to inverse what the characters will be to 
                sta (cursor)                                                    ; create a cursor to show where our x,y position is on the screen
                stz $01                                                         ; Remember to resore the I/O control back to zero
                lda NMIFlag
                cmp #$02
                bne skipB
                lda #"B"
                sta RHeaders+1
skipB:
                jsr DisplayRegisters
                lda #"R"
                sta RHeaders+1
init_events:                                                                    ; we're setting up the kernel telling it where our memory will be
                lda #<event                                                     ; this was setup above is th ZP section - we're using $c0 to $c6
                sta kernel.args.events                                          ; as SuperBASIC does not use this memory
                lda #>event
                sta kernel.args.events+1
                
Loop:                                                                           ; This is our loop that the program will continue to run unless we exit using 
                jsr handle_events                                               ; the F1 key. The CMP command is checking our buffer to see if the F1 key has been
                lda buff                                                        ; hit - $81 is the ascii value for F1 -
                cmp #$81                                                        ; if the CMP is true we jump out of the loop and return to SuperBasic
                beq returnBASIC
                jmp Loop

returnBASIC:
                ;jsr clearScreen
                jsr cReturn
                jsr clearCursor
                lda xPOS
                sta $07eb
                lda yPOS
                sta $07ea
                lda cursor
                sta $40 
                lda cursor+1
                sta $41
                lda #$05 
                sta BCURSOR                                                     ; Turns on the blinking BASIC cursor
                rts                                                             ; This returns us to SuperBASIC

handle_events:                                                                  ; This is where we query the kernel looking for events
                lda kernel.args.events.pending		                        ; Peek at the queue to see if anything is pending
                bpl _done       			                        ; Nothing to do
                jsr kernel.NextEvent			                        ; Get the next event.
                bcs _done       		                                ; If there are no pending events we return to the mail loop.
                jsr _dispatch                                                   ; If there is an event, we jump down to dispatch to see what it is and act on it
                lda buff                                                        ; When we return from dispatch we'll check the buffer for F1 and skip any other 
                cmp #$81                                                        ; events that might be pending in the handler, otherwise, we check for more events.
                bne handle_events
_done:
                rts             			                        ; go back to the main loop        

_dispatch:
                lda event.type				                        ; Here we check what event has triggered in the kernel. We are only checking
                cmp #kernel.event.key.PRESSED		                        ; for keyboard presses. Any other event will be ignored and return us to the
                beq keypress			                                ; event handler.
                rts

keypress:
                ;lda event.key.flags                                            ; Once a key is pressed, we can get the ascii value by loading the byte from the
                lda event.key.ascii                                             ; event.key.ascii location assigned by the kernel. We then check to see if it's a
                cmp #$20                                                        ; regular printable character by seeing if it's a value between $20 and $7f.
                bcc checkNonChar                                                ; if not, we'll jump down to check for specific key presses that do other work
                cmp #$80                                                        ; otherwise we'll pass through to the print character routine below.
                bcs checkNonChar
 

printChar:                                                                      ; The accumulator carries the ascii value of the key that was pressed so we'll
                sta buff                                                        ; stor that in a buffer.  First we'll clear the cursor from the location. Next
                jsr clearCursor                                                 ; we need to change the I/O control to 2 so we can put the character on the screen
                lda #$02                                                        ; 
                sta $01
                lda buff                                                        ; once that is done, we'll use the cursor position calculated earlier to place the
                sta (cursor)                                                    ; character on the screen using indirect addressing, Y has not changed from zero.
                stz $01                                                         ; Next, we retore I/O control to zero.

setCursor:
                inc xPOS                                                        ; Once the character is placed, we'll increment the x position, and check to see if
                lda xPOS                                                        ; we've reached the end of the screen. If not, we'll bypass the new line and set the
                cmp #80                                                         ; cursor in its new position.
                bcc setCursorScreen

newLine:                                                                        ; If x has reached the end of the line, we'll reset it to zero and increment the
                lda #$00                                                        ; y position by 1 putting it on a new line.
                sta xPOS
                inc yPOS
                lda yPos                                                        ; We'll also check to see if y has reached the bottom of the screen. If it has, we'll
                cmp #60                                                         ; reset it to the top of the screen since this is just a demo.
                beq setY
                cmp #$ff                                                        ; if not, we jump down to setting the cursor position.
                bne setCursorScreen 
setY:
                lda #$00
                sta yPos

setCursorScreen:                                                                ; This sets the new cursor position by calculating the new memory location
                jsr getScreenPos                                                ; on the text matrix and setting the color to be inverse the text colors.
                lda #$03                                                        ; Remember we need to set I/O control to 3 to address the color matrix, and
                sta $01                                                         ; to reset it back to 0 when we're done.
                lda #$26 
                sta (cursor)
                stz $01
                rts

clearCursor:                                                                    ; This is the clear cursor routine. We are simply changing the color matrix back to
                lda #$03                                                        ; the normal text/background colors. I/O control to 3 and then back to 0.
                sta $01
                ;ldy #$00
                lda #$62
                sta (cursor)
                stz $01
                rts

moveCursor:
                jsr clearCursor                                                ; When hitting the routines below, we'll branch here to clear and reset the cursor
                bra setCursorScreen



checkNonChar:                                                                   ; Here's the branching section if we've hit non printable ascii characters. We check for 
                cmp #$0d                                                        ; return, delete, cursor up, down, left , and right as well as F1 for exit.
                beq return
        ;        cmp #16
        ;        beq up
                cmp #14
                beq down
                cmp #$02
                beq left
                cmp #$06
                beq right
                cmp #$08 
                beq delete
                cmp #$81 
                beq exit
                rts
return:                                                                         ; When hitting return we want to return x to zero and increase y by 1
                jsr saveBuffer
                jsr evalCommand
cReturn:
                jsr clearCursor
                stz xPOS                                                        ; if y hits the bottom of the screen, we'll just reset it to the top. 
                inc yPOS
                lda yPOS
                cmp #59
                bcc moveCursor
                jsr scroll
                dec yPOS
                bra moveCursor
up:                                                                             ; up decreases y by 1, not change to x. if we go above the top line,   
                dec yPOS                                                        ; we'll reset to the bottom
                lda yPOS
                cmp #60
                bcc moveCursor
                lda #59
                sta yPOS
                bra moveCursor
down:                                                                           ; down increases y by 1, no change to x, if we hit the bottom of the 
                inc yPOS                                                        ; screen, we'll reset to the top.
                lda yPOS
                cmp #59
                bcc moveCursor
                jsr scroll
                dec yPOS
                bra moveCursor
left:                                                                           ; left decreases x by 1 no change to y even if we go left of the screan. In
                dec xPOS                                                        ; that case, we'll just reset to the right side of the screen on the same line.
                lda xPOS
                cmp #79
                bcc moveCursor
                lda #79
                sta xPOS
                bra moveCursor
right:                                                                          ; Right increases x by one, no change to y even if we go past the right side
                inc xPOS                                                        ; the screen. We'll just put x on the left side of the same line.
                lda xPOS 
                cmp #79
                bcc moveCursor
                stz xPOS
                bra moveCursor
delete:                                                                         ; delete wil decrease x by one while also remove the character that the cursor was 
                lda #$02                                                        ; over prior to hitting delete. This will cause the character that was to the left
                sta $01                                                         ; to be highlighted.
                lda #$20
                sta (cursor)
                stz $01
                bra left
exit:                                                                           ; exit will store the ascii code for F1 in the buffer which will trigger a branch
                lda #$81
                sta buff                                                        ; in the main loop to exit the program and give controll back to SuperBASIC.
                ;jmp $e020                                                      ; reboot the system
                rts

saveBuffer:                                                                     ; grab the whole line of characters from the screen
                sec                                                             
                lda cursor                                                      ; get the true cursor position on the video matrix
                sbc xPOS                                                        ; subtract xPOS to get to the start of the line
                sta tempII
                lda cursor+1
                sbc #$00
                sta tempII+1
                lda #$02
                sta MMU_CTRL                                                    ; switch the MMU to the graphics screen 
                ldy #$00                                                        ; set y to zero for indirect indexing loop
bufferLoop:
                lda (tempII),y                                                  ; load a character from the screen
                sta lBuffer,y                                                   ; and store it on our working buffer to parse
                iny
                cpy #80                                                         ; have we done the entire line
                bcc bufferLoop                                                  ; not yet, keep looping
                ldy xPOS                                                        ; now reload the original XPOS
                lda #$00                                                        ; and set that character to zero in our buffer to the parser knows where we stopped.
                sta lBuffer,y
                stz MMU_CTRL                                                    ; restore IO back to 0
                rts

memoryDump:                                                                     ; probably a temp solution for the command structor
                jmp memoryDisplay                                               ; there are a series of jumps here to the various commmands
disassemblyDump:                                                                ; should come up with a better way to mange this.
                jmp disassemble
exitToBasic:
                lda #$81
                sta buff
                rts
        

evalCommand:                                                                    ; this is the command evaluator. This also needs to be changed to be
                lda lBuffer                                                     ; more flexible. right nowm just a collection of camparisons with branches
                ldx #$00
cLoop:                                                                          ; Run through the list of commands to find the command if any
                cmp commandList,X                                               ; once we find the command, branch to eval 2 to set up the jump
                beq eval2
                inx 
                cpx #$04                                                        ; current number of supported commands
                bne cLoop
                rts                                                             ; no command, return to the regular screen routine
eval2:
                txa                                                             ; transfer x to a so we can double it for the command lookup table
                asl
                tax                                                             ; transfer back to x so we can load the address into a temp location
                jmp (commandTable,x)


getWord:                                                                        ; This gets the address from the command converting ascii characters into a location
                lda lBuffer,X                                                   ; in WORD format.
                jsr getValue
                asl 
                asl 
                asl 
                asl 
                sta dLine+1
                inx 
                lda lBuffer,X
                jsr getValue
                ora dLine+1
                sta dLine+1
                inx 
                lda lBuffer,X
                jsr getValue
                asl 
                asl 
                asl 
                asl
                sta dLine
                inx
                lda lBuffer,X
                jsr getValue
                ora dLine
                sta dLine
                rts

getValue:                                                                       ; Just a quick routine to turn ascii numbers to actual numbers (hex included)
                sec
                sbc #$30 
                cmp #$10 
                bcc vDone
                sbc #$27
vDone:
                rts

memoryDisplay:
                jsr cReturn
                ldx #$01
                lda lBuffer,X
                beq memC
                inx
                jsr getWord
        
memC:
                stz dcount
memLoop:
                lda dLine
                sta tempII
                lda dLine+1
                sta tempII+1
                lda #">"
                jsr printChar
                lda #" "
                jsr printChar
                lda dLine+1
                jsr outputByte
                lda dLine
                jsr outputByte
                lda #" "
                jsr printChar
                lda #" "
                jsr printChar
                lda (dLine)
                jsr outputByte
                lda #" "
                jsr printChar
                ldx #$00
mByteLoop:
                jsr writeByte
                lda #" "
                jsr printChar
                inx
                cpx #$07
                bne noExtraLine
                lda #" "
                jsr printChar
noExtraLine:
                cpx #$0f
                bcc mByteLoop
                lda #" "
                jsr printChar
                ldy #$00
chrLoop
                lda (tempII),y
                jsr printChar
                cpy #$07
                bne blankSpace
                lda #" "
                jsr printChar
blankSpace:
                iny
                cpy #$10
                bcc chrLoop

                jsr cReturn
                jsr moveCount
                inc dcount
                lda dcount
                cmp #$10
                bcc memLoop
                rts

DisplayRegisters:
                jsr cReturn
                ldx #$00
regLoop1:
                lda RHeaders,X
                jsr printChar
                inx
                cpx #$23
                bcc regLoop1

                jsr cReturn
                ldx #$03
                jsr spaces
                lda PCH
                jsr outputByte
                lda PCL
                jsr outputByte
                ldx #$02
                jsr spaces
                lda SR
                jsr outputByte
                ldx #$02
                jsr spaces
                lda ACC
                jsr outputByte
                ldx #$02
                jsr spaces
                lda XR
                jsr outputByte
                ldx #$02
                jsr spaces
                lda YR
                jsr outputByte
                ldx #$02
                jsr spaces
                lda SP
                jsr outputByte
                jsr cReturn
                jsr cReturn
                rts


spaces:
                lda #" "
                jsr printChar
                dex
                bne spaces
                rts
disassemble:
                jsr cReturn
                ldx #$01
                lda lBuffer,x
                beq dis
                ldx #$02        
                jsr getWord                                               
dis:
                stz dcount
testLoop:
                stz codeFlag
                lda (dLine)
                sta code1
                inc codeFlag
                jsr getOpCode
                lda (dLine)
                jsr getAddressing
        

                lda #$07 
                sta xPOS
                lda #" "
                jsr printChar

                ldx #$00
codeLoop:
                lda code1,x 
                jsr outputByte
                lda #" "
                jsr printChar
                inx
                cpx codeFlag
                bne codeLoop

                jsr cReturn
                inc dLine
                lda dLine
                bne noRoll
                inc dLine+1
noRoll:
                inc dcount
                lda dcount
                cmp #$20                                                         ;line dissassembled
                bcc testLoop
                rts

getOpCode:
                sta MULT_B
                stz MULT_B+1
                lda #$03
                sta MULT_A
                stz MULT_A+1
                lda MULT_result
                sta ADD_A
                lda MULT_result+1
                sta ADD_A+1
                stz ADD_A+2
                stz ADD_A+3
                lda #<opcode
                sta ADD_B
                lda #>opcode
                sta ADD_B+1
                stz ADD_B+2
                stz ADD_B+3
                lda ADD_result
                sta tempII2 
                lda ADD_result+1
                sta tempII2+1
                jsr getScreenPos
                lda #";"
                jsr printChar
                lda #" "
                jsr printChar
                lda dLine+1
                jsr outputByte
                lda dLine
                jsr outputByte
                lda #$11 
                sta xPOS
                lda #" "
                jsr printChar
                ldy #$00

opcLoop:
                lda (tempII2),y
                sta buff
                sty opPOS
                lda buff
                jsr printChar
                ldy opPOS
    
                iny
                cpy #$03
                bne opcLoop
                lda #$20
                jsr printChar
                rts

getAddressing:
                tax
                lda addressMode,X
                tax
                clc
                adc #$06
                sta tempII2+1
adLoop:
                lda addOut,X
                cmp #"R"
                bne notRelative
                jsr writeRelative
                bra next
notRelative:
                cmp #"B"
                bne notByte
                jsr writeByte
                bra next
notByte:
                cmp #"W"
                bne notWord
                jsr writeWord
                bra next
notWord:
                cmp #" "
                beq doneLine
                jsr printChar
next:
                inx
                cpx tempII2+1
                bne adLoop
doneLine:

                rts

writeByte:
                jsr moveCount
                lda (dLine)
                sta code2
                inc codeFlag
                jmp outputByte

writeWord:
                jsr moveCount
                lda (dLine)
                sta LO
                sta code2
                inc codeFlag
                jsr moveCount
                lda (dLine)
                sta code3
                inc codeFlag
                jsr outputByte
                lda LO
                jmp outputByte

writeRelative:
                lda dLine
                sta LO
                lda dLine+1
                sta HI
                jsr moveCount
                inc codeFlag
                lda (dLine)
                sta code2
                bpl branchForward
                sec
                lda #$ff
                sbc code2
                sbc #$01
                sta code3
                sec
                lda LO
                sbc code3
                sta LO
                lda HI
                sbc #$00
                jsr outputByte
                lda LO
                jmp outputByte
branchForward:
                clc
                adc LO
                adc #$02
                sta LO
                lda HI
                adc #$00
                jsr outputByte
                lda LO
                jmp outputByte

                

outputByte:
                sta HI
                lsr
                lsr
                lsr
                lsr 
                tay
                lda hex,y
                jsr printChar
                lda HI       
                and #%00001111
                tay
                lda hex,y
                jsr printChar
                rts

moveCount:
                inc dLine
                lda dLine
                bne notZero
                inc dLine+1
notZero:
                rts

getScreenPos:                                                                   ; We need to get the matrix byte location when storing a character to the screen. t
                lda yPOS                                                        ; This is done with the simple formula (Y * 80 + X) + $C000. We'll use the F256xx math
                sta MULT_A                                                      ; coprocessor to get this done quickly. There ar 4 entry bytes for the multiplication,
                stz MULT_A+1                                                    ; MULT_A, MULT_A+1, MULT_B, MULT_B+1. MULT_result - MULT_result+3 will give us the 4 byte 
                lda #80                                                         ; result immediately. Our inputs are only 1 byte each so we make sure to do STZ for the
                sta MULT_B                                                      ; two.
                stz MULT_B+1
                lda MULT_result                                                 ; We next use the coprocessor for addition. Addition has 8 inputs total,
                sta ADD_A                                                       ; ADD_A - ADD_A+3, and ADD_B - ADD_B+3. We are only using 2 bytes for each value
                lda MULT_result+1                                               ; so we'll be sure to STZ out the rest. Results are returned at ADD_Result - AddResult+3.
                sta ADD_A+1
                stz ADD_A+2                                                     ; We've set these labels up at the top of the program.
                stz ADD_A+3
                lda xPOS
                sta ADD_B
                lda #$c0
                sta ADD_B+1
                stz ADD_B+2
                stz ADD_B+3
                lda ADD_result                                                  ; The result is stored in two bytes called cursor.
                sta cursor
                lda ADD_result+1
                sta cursor+1
                rts

clearScreen:
                lda #$c0
                sta tempII+1
                stz tempII
                ldx #$14
                ldy #$00
                lda #$20
blockLoop:
                lda #$02
                sta $01
                lda #$20
                sta (tempII),y
                lda #$03
                sta $01
                lda #$62
                sta (tempII),y
                iny
                bne blockLoop
                inc tempII+1
                dex
                bne blockLoop
                stz $01
                rts

scroll:
                lda #$c0
                sta tempII2+1
                sta tempII+1
                stz tempII2
                lda #$50
                sta tempII
                ldx #$13
                ldy #$00
scrollLoop:
                lda #$02
                sta $01
                lda (tempII),y
                sta (tempII2),y
                lda #$03
                sta $01
                lda (tempII),y
                sta (tempII2),y
                iny
                bne scrollLoop
                inc tempII+1
                inc tempII2+1
                dex
                bne scrollLoop


lastLine:

                dec $01
                lda #$20
                sta $d270,x 
                inc $01
                lda #$62
                sta $d270,x 
                inx
                cpx #80
                bcc lastLine

                stz $01

                rts


                                                                                ; Working Memory
xPOS:           .byte $00                                                       ; X cursor position
yPOS:           .byte $00                                                       ; y cursor position
buff:           .byte $00                                                       ; 1 byte buffer holding the asccii code of the key we pressed
opPOS:          .byte $00
colorbuf:       .fill 28,$62                                                    ; The color buffer for the title text
LO:             .byte $00
HI:             .byte $00
code1           .byte $00
code2           .byte $00
code3           .byte $00
codeFlag        .byte $00
RHeaders:       .text "*R  PC   SR  AC  XR  YR  SP   v0.1a"                            ; Headers for Registers

hex:            .text "0123456789abcdef"
lBuffer:        .fill 80,$20
dcount:         .byte $00
exitFlag        .byte $00

commandList:    .text "dmrx"
commandTable:
                .word disassemble,memoryDisplay,DisplayRegisters,exitToBasic

textColorLut:
		.byte 0,0,0,0
		.byte 102,102,102,0
		.byte 170,0,0,0
		.byte 0,170,0,0
		.byte 234,65,192,0
		.byte 0,72,135,0
		.byte 0,156,255,0
		.byte 255,219,255,0
		.byte 40,63,63,0
		.byte 138,170,170,0
		.byte 255,85,85,0
		.byte 85,255,85,0
		.byte 237,141,255,0
		.byte 0,0,255,0
		.byte 85,255,255,0
		.byte 255,255,255,0

.include "table1.s"

    .endsection