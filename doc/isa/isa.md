# Punch ISA

## Overview
Punch is a 32-bit ARM and RISC-V inspired ISA, with several features tuned to Factorio's Circuit Network system in which it is designed to be built.

The ISA is designed to be simple to decode and yet provide enough versatile and powerful instructions so that the restraints of encoding do not limit what programs can do.

### Design
This ISA is designed as a first dive into pipelined designs and takes heavy inspiration from the ARM ISA, particularly ARMv4 which features a simple prefetch-decode-execute pipeline.

Also inspired by ARM is the conditional execution of every instruction, which can limit the time spent filling the pipeline after a branch, and the use of condition flags which enable this behavior.

The design is also inspired by the RISC-V RV32I ISA, which prioritizes a simple base ISA to allow for extensibility, and provides only a few instruction formats, unlike ARMv4 for example.

The memory space is best constrained to 16 bits, as addresses in this range would be the most convenient to use, but larger address ranges are of course supported as well. In these cases, a similar approach to RV32I is taken where two instructions are needed to create a 32-bit immediate value for an address.

### General specifications and features
The base ISA contains 22 instructions and 4 instruction formats, which are independent of the instructions.

There is also a debugging and utility instruction in the form of a halt or wait, which stops the clock until manual action is taken or the timer specified for the wait instruction runs down.

There are a total of 13 possible conditions to execute instructions conditionally, with 4 flags: Zero, Negative, Overflow and Error.

There are a total of 16 registers, one of which is hardwired to zero. This allows several simplifications in the instruction set, as several explicit instructions can be instead mapped to an instruction making use of the zero register. Some examples are NOP (adding the zero register to itself), MOV (adding some register or immediate value with the zero register and storing in a different register) and CMP (subtracting and setting flags, but writing to the zero register, which voids the result).

The registers are:
- x0: hardwired zero: reading results in 0 and writing has no effect.
- x1-x10: general purpose registers.
- x11-x12 (also t1-t2): general purpose timers which count down at 60Hz until reaching zero.
- x13 (also SP): stack pointer, general purpose register used to interact with the in-memory stack.
- x14 (also LR): link register, general purpose register used for the branch with link instruction to enable use of subroutines.
- x15 (also PC): program counter, points to the next instruction to fetch from the program memory. Always ahead of the current instruction by a few instructions, pointing to the instruction currently in the fetch stage.

There is also a special internal register holding the flags, which cannot be read but can be used to execute instructions conditionally and can be written by certain instructions.

#### Implementation: First revision
The first revision of the Punch CPU implements all the base instructions and features a five stage pipeline:
1. Fetch
2. Decode
3. Read registers
4. Execute/Memory access
5. Writeback

> **Note** The third and fifth stages can run simultaneously without data hazards, but the registers written to by an instruction will take one cycle in the writeback stage before they can be read from, i.e. a register written to by an instruction in the execute stage and read from in the read stage is subject to a data hazard.
> This does not apply to the branch instruction, but it does if branching via an ALU instruction or a memory load.

It features a small memory range: 4K words of program ROM and 8K words of RAM, where a word is a 32-bit value.

## Instruction encoding
All instructions are one 32-bit word long and have one of several similar types of formats. These were designed to be simple to decode and immediates embedded in them were placed to be easily sign extended, as all immediates are signed two's complement.

There are four different types of instruction:
- R-type: `eeee eeee ssss rrrr dddd oooo o00f cccc`
- I-type: `iiii iiii iiii rrrr dddd oooo o01f cccc`
- A-type: `aaaa aaaa aaaa aaaa dddd oooo o10f cccc`
- L-type: `llll llll llll llll llll oooo o11f cccc`

Here one letter simbolizes one bit of a group:
- e: extra/function specific
- i/a/l: I/A/L immediate, of 12, 16 and 20 bits respectively
- s: second register operand
- r: first register operand
- d: destination/source register
- o: opcode
- f: flags enable or link enable
- c: condition code

The immediates were placed such that their sign bit is always at bit 31 of the instruction, and common fields were reused across formats to simplify decoding.

Operand positioning is designed to make instructions usable in more than one format, and so they follow a few simple rules:
1. Immediate values are always the second operand, in R-type instructions it will instead be the `s` register.
2. The `r` register is always the first operand, even when the instruction omits it. In the latter case, the first operand will instead be the zero register.
3. The `d` register will always be the destination register, i.e. the register to which the result of an instruction should be written. The only exception to this is with the store instruction, where it is the source register instead. If it is omitted, the destination will be the zero register.

> **Note** Some instructions make use of the second operand even though they do not use the first.

> **Note** Most instructions can use most formats. However, many of the instructions are effectively NOPs in many cases, e.g. any ALU operation with the L-type format with the `f` flag unset will have no effect on the state of the processor, and even with the flag bit are barely useful. Some format-instruction pairings may be reserved for future expansions.

## Instructions

## Memory
