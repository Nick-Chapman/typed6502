
osasci = &ffe3

org &2000

.start:
    jmp main
.mytext:
    equs "Goodbye!", 13, 0

.main:
    ldy #0

.loop
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
