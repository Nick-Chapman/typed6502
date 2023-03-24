
osasci = &ffe3

org &2000

ptr = &70

.start:
    jmp main

.hello EQUS "Hello!", 13, 0
.goodbye EQUS "Goodbye!", 13, 0

.main:
    lda #LO(hello) : sta ptr
    lda #HI(hello) : sta ptr+1
    jsr outputMessage
    lda #LO(goodbye) : sta ptr
    lda #HI(goodbye) : sta ptr+1
    jsr outputMessage

.spin:
    jmp spin

.outputMessage:
    ldy #0
.loop
    lda (ptr),y
    beq finished
    jsr osasci
    iny
    bne loop
.finished:
    rts

.end:
save start, end
