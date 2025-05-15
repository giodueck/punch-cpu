;add x2 x0 #1
;nop
;shl x1 x2 #31   ; 0x80000000
;shls x2 x2 #31  ; 0x80000000
ldh x1 #0x8000
ldh x2 #0x8000

; if negative, set x4 = 1
addng x4 x0 #1
xorpz x4 x4

adds x3 x1 x2
nop

; if overflowed (it should), set x5 = 1
addvs x5 x0 #1
xorvc x5 x5

brk
b #0
