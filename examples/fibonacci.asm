; Calculate the largest fibonacci number that fits in a 32 bit signed integer

; Starts with 0 and 1 as the first numbers
    mov x1 #0
    mov x2 #1
    mov x4 #1 ; counter

.loop
    adds x3 x2 x1
    mov x1 x2
    ; Once a fibonacci number overflows, end and keep previous number in x1
    bvs end
    mov x2 x3
    add x4 #1
    b loop

.end
    brk
    b #0
