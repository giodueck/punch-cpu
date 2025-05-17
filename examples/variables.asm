@var x 0
@var y 1

; Copy variables into RAM
add x1 x0 _datalen
add x2 x0 _datastart
add x3 x0 _ramstart
.initram
ldria x4 x2
stria x4 x3
subs x1 #1
bne initram

ldr x1 x

nop
nop
nop

brk
b x0
