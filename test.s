@const test 123
@array arr1 8 [0]
@var var1 0xdeaddead

add x1 #0b1011
adds x4 #0o2700
.label addle x4 #0x2000
ldria x4 x4
; comment
strib x1 x4   ; inline comment
bl #0
blt #1

.last
bllt #2

@macro do_something 1
nop
b $0
@endm

@macro adder 2
add $0 $1 test
@endm

do_something last
