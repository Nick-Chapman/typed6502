
osasci = &ffe3

org &2000

addr = &70

.start:
    jmp main
.mytext EQUS "Hello world!", 13, 0
.main:
    ;;lda #LO(mytext) : sta addr
    ;;lda #HI(mytext) : sta addr+1
    ldy #0
.loop
    ;;lda (addr),y
    lda mytext,y
    beq finished
    jsr osasci
    iny
    bne loop
.finished
.spin:
    jmp spin
.end:

save start, end
