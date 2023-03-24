
osasci = &ffe3

org &2000

.start:
    jmp main
.main:
    lda #'H' : jsr osasci
    lda #'e' : jsr osasci
    lda #'l' : jsr osasci : jsr osasci
    lda #'o' : jsr osasci
    lda #'!' : jsr osasci
    lda #13 : jsr osasci
.spin:
    jmp spin
.end:

save start, end
