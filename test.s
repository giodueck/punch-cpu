@const test 123
@array arr1 8 [0]
.label @var var1 0xdeaddead

add x1 #0b1011
adds x4 #0o2700
addle x4 #0x2000
ldria x4 #002000
; comment
strib x1 x4   ; inline comment
bl #0
blt #0
bllt #0

@macro do_something #0
nops
@endm

@macro add 2
add $1 $2
@endm
