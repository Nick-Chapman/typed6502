;;; Frame count example

osasci = &ffe3
oswrch = &ffee
osbyte = &fff4

macro copy16i I,V
    lda #LO(I) : sta V
    lda #HI(I) : sta V+1
endmacro

macro Puts S
    copy16i msg, msgPtr
    jmp after
.msg: EQUS S, 0
.after:
    jsr printMessage
endmacro

macro Position X,Y
    lda #31 : jsr osasci
    lda #X : jsr osasci
    lda #Y : jsr osasci
endmacro

ORG &70

.msgPtr SKIP 2
;.frameCount SKIP 1 ;; TODO

ORG &2000

.start:
    jmp main

.frameCount SKIP 1

.mos_syncVB: lda #19 : jmp osbyte

.mode1:
    lda #22 : jsr oswrch
    lda #1 : jsr oswrch
    rts

.cursorOff:
    lda #23 : jsr oswrch
    lda #1 : jsr oswrch
    lda #0 : jsr oswrch
    lda #0 : jsr oswrch
    lda #0 : jsr oswrch
    lda #0 : jsr oswrch
    lda #0 : jsr oswrch
    lda #0 : jsr oswrch
    lda #0 : jsr oswrch
    lda #0 : jsr oswrch
    rts

.printMessage: {
    ldy #0
.loop
    lda (msgPtr),y
    beq done
    jsr osasci
    iny
    bne loop
.done:
    rts }

.main: {
    jsr mode1
    jsr cursorOff
.loop:
    jsr mos_syncVB
    Position 1,1 : Puts "Frame : " : lda frameCount  : jsr printHexA
    inc frameCount
    jmp loop }

.printHexA: {
    pha : lda #'[' : jsr osasci : pla
    pha
    and #&f0 : lsr a : lsr a : lsr a : lsr a : tax
    lda digits,x
    jsr osasci
    pla
    and #&f : tax
    lda digits,x
    jsr osasci
    pha : lda #']' : jsr osasci : pla
    rts
.digits EQUS "0123456789abcdef" }

.end:
SAVE start, end
