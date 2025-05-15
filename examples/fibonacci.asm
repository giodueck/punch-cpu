
@macro mov 2
add $0 x0 $1
@endm

; Calculate the largest fibonacci number that fits in a 32 bit signed integer

    mov x1 #1
    mov x2 #1
    nop

.loop
    adds x1 x2
    bvs end

.loop_alt
    adds x2 x1
    addvs x1 x0 x2
    bvc loop

.end
    brk
    b #0
