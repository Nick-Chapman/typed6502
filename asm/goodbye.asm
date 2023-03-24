
osasci = &ffe3

org &2000

.start:
    jmp main
.main:
    lda #'g' : jsr osasci
    lda #'o' : jsr osasci : jsr osasci
    lda #'d' : jsr osasci
    lda #'b' : jsr osasci
    lda #'y' : jsr osasci
    lda #'e' : jsr osasci
    lda #'!' : jsr osasci
    lda #13 : jsr osasci
.spin:
    jmp spin
.end:

save start, end
