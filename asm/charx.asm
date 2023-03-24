
osasci = &ffe3

org &2000

.start:
    lda #'X' : jsr osasci
.spin:
    jmp spin
.end:

save start, end
