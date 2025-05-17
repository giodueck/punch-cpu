# Punch ISA

## Overview
Punch is a 32-bit ARM and RISC-V inspired ISA, with several features tuned to Factorio's Circuit Network system in which it is designed to be built.

The ISA is designed to be simple to decode and yet provide enough versatile and powerful instructions so that the restraints of encoding do not limit what programs can do.

### Design
This ISA is designed as a first dive into pipelined designs and takes heavy inspiration from the ARM ISA, particularly ARMv4 which features a simple prefetch-decode-execute pipeline.

Also inspired by ARM is the conditional execution of every instruction, which can limit the time spent filling the pipeline after a branch, and the use of condition flags which enable this behavior.

The design is also inspired by the RISC-V RV32I ISA, which prioritizes a simple base ISA to allow for extensibility, and provides only a few instruction formats, unlike ARMv4 for example.

The memory space is best constrained to 16 bits, as addresses in this range would be the most convenient to use, but larger address ranges are of course supported as well. In these cases, a similar approach to RV32I can be taken where several instructions are needed to create a 32-bit immediate value for an address.

### General specifications and features
The base ISA contains 18 instructions and 4 instruction formats, which are independent of the instructions.

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
> This does not apply to the branch instruction with immediate offset, but it does if branching via an ALU instruction or a memory load.

It features a small memory range: 4K words of program ROM and 8K words of RAM, where a word is a 32-bit value.

#### Implementation: Second revision
An instruction which sets a register which is immediately needed as an operand by the following instruction caused a data hazard in the first revision. Since it is only a 1 cycle delay between when an instruction executes and when the result is written to the destination, this value could also be forwarded to the execution stage without much complex wiring.

As such, if a register is needed as either operand in an operation, and its value is updated by the previous instruction, the value produced by this previous instruction replaces the value read in the read registers stage.

This is the only unhandled hazard of the first revision, as control hazards are handled by flushing the pipeline if the branch is taken.

> **Note** The third revision fixes some problems in the second, like improper writeback checking for the result to be forwarded and an exception for the store operation, which uses the destination as the operand instead.

#### Implementation: Third revision
Implement Data ROM, 4096 words of constant memory to be written by the compiler and copied to RAM to initialize variables.

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

### Condition codes
All instructions can be executed conditionally, making use of one of several combinations of flags. To execute an instruction conditionally, append the corresponding postfix. These are:

Postfix | Code | Description                     | Flags
------- | ---- | ------------------------------- | -----
--      | 0000 | Always                          | none
`eq`    | 0001 | Equal or equal to zero          | (Z)
`ne`    | 0010 | Not equal or not zero           | (!Z)
`ng`    | 0011 | Negative                        | (N)
`pz`    | 0100 | Positive or zero                | (!N)
`lt`    | 0101 | (signed) Less than              | (N != V)
`le`    | 0110 | (signed) Less than or equal     | (Z or N != V)
`gt`    | 0111 | (signed) Greater than           | (!Z and N = V)
`ge`    | 1000 | (signed) Greater than or equal  | (N = V)
`vs`    | 1001 | Overflow set                    | (V)
`vc`    | 1010 | Overflow clear                  | (!V)
`es`    | 1011 | Error set                       | (E)
`ec`    | 1100 | Error clear                     | (!E)

All other codes are reserved, except for 15 (1111), which should result in false and may stand for Never.

For instructions that update the flags, they update all flags even if they may not be able to set every flag. The meaning of the flags is as follows:
- Z: the result is zero
- N: the result is negative
- V: the operation resulted in signed overflow, only set by addition and subtraction instructions
- E: some error occurred, for example a division by 0

### Arithmetic and logic instructions

#### First class instructions
There are 12 first class instructions in this category. They are implemented using the arithmetic combinator, which has these same functions.

Mnemonic | Opcode | Description
-------- | ------ | -----------
`add`    | 00000  | Add two operands
`sub`    | 00001  | Subtract the second operand from the first
`sbn`    | 00010  | Subtract the first operand from the second
`mul`    | 00011  | Multiply two operands
`div`    | 00100  | Divide the first operand by the second, results in the integer quotient
`mod`    | 00101  | Divide the first operand by the second, results in the modulo
`exp`    | 00110  | Raise the first operand to the second
`shl`    | 00111  | Shift the first operand left by the second. It the shift amount is 32 or more, rotate instead
`shr`    | 01000  | Shift the first operand right, extending the sign, by the second. It the shift amount is 32 or more, rotate instead
`and`    | 01001  | Logical AND between two operands
`orr`    | 01010  | Logical OR between two operands
`xor`    | 01011  | Logical XOR between two operands

All instructions take two operands, but may be encoded as R-type, I-type or A-type (it may be useful to use just the second operand, to encode in A-type set operand 1 to x0). All instructions store the result in the destination register.

> **Note** The use of L-type instructions with these opcodes is reserved for future extensions.

All instructions in this section can update the flags. To do this, append `s` to the instruction before the optional condition code.

All instructions may set the Z and N flags. Only add, sub and sbn may set the V flag. Only div and mod may set the E flag.

##### Using PC
Note that the PC will always be 3 instructions ahead of the current one due to it pointing to the instruction currently in the fetch stage.

When using PC as a destination register, the pipeline will be flushed and the instruction pointed to by the new PC will be loaded. This makes ALU operations behave as a kind of branch instruction.

#### Second class instructions
There are many instructions in other architectures that can be easily represented with the first class instructions already, but which make code easier to reason about. Here are some instructions and their translations.

Mnemonic | Operands    | Translation           | Description
-------- | ----------- | --------------------- | -----------
`mov<s>` | `xd xs/imm` | `add<s> xd x0 xs/imm` | Copy a register or immediate into a register.
`cmp`    | `xr xs/imm` | `subs x0 xr xs/imm`   | Compare two values and update flags without storing a result
`nop`    |             | `add x0 x0 x0`        | Do nothing. This instruction encodes as 0, so an invalid fetch or unset program memory do nothing.
`not<s>` | `xd xr`     | `xor<s> xd xr -1`     | Logical NOT to first operand.

### Load and store instructions

#### First class instructions
There are 3 first class instructions in this category.

Mnemonic | Opcode | Operands    | Description
-------- | ------ | ----------- | -----------
`ldr<s>` |  10000 | `xd xs/imm` | Load a word from memory at address pointed to by the second operand.
`str`    |  10001 | `xd xs/imm` | Store a word from the source register (xd) to memory at address pointed to by the second operand.
`ldh<s>` |  10010 | `xd imm`    | Load a 16-bit immediate into the upper 16 bits of the destination, clearing the lower bits. Can be used to construct 32-bit immediates with 3 instructions.

> **Note** The first operand stays unused in all of these instructions. Encodings with the first operand different from 0 are reserved for future extensions.

##### Address register modification
When using an R-type ldr or str instruction, a pre/post-increment/decrement is supported. It is encoded in the `e` bitfield and is interpreted as follows:

`vvvv vvvu`

Which represents:
- v: a 7-bit two's complement value, i.e. a number in the range of -64 to 63.
- u: pre/post bit:
  - 0: post: add value after memory access
  - 1: pre: add value before memory access

These instructions are accessed by appending the corresponding postfix, before the `s` postfix in the case of the load instruction, and before the conditional postfix:
- ib: For pre-increment
- ia: For post-increment
- db: For pre-decrement
- da: For post-decrement

Normally the value to be added is 0, when adding either `ib` or `ia`, it is 1, and when adding either `db` or `da` it is -1. This value may also be specified as an immediate third operand when it should be different.

##### Using PC
When using PC as a destination register, the pipeline will be flushed and the instruction pointed to by the new PC will be loaded. This makes load instructions behave as a kind of branch instruction.

#### Second class instructions
Stack instructions are translated to load and store instructions with an address writeback.

Mnemonic | Operands  | Translation         | Description
-------- | --------- | ------------------- | -----------
`push`   | `xd`      | `strdb xd sp`       | Push the register xd to the top of the stack, updating the stack pointer.
`pop<s>` | `xd`      | `ldria<s> xd sp`    | Pop the top of the stack to the register xd, updating the stack pointer.

> **Note** These operations are designed for a full-descending stack, i.e. the bottom of the stack is the highest memory address.

### Branch instruction
There is one explicit branch instruction, which works by adding an offset to PC in the execution phase.

The branch instruction immediately fetches the next desired instruction in the next cycle, which causes one less cycle of latency from flushing the pipeline than when using an instruction which makes use of the writeback stage. The branch instruction is also the only way to update LR and branch in the same instruction.

Mnemonic | Opcode | Operands | Description
-------- | ------ | -------- | -----------
`b<l>`   |  11100 | `imm`    | Adds operand 2 as an offset to PC. If `l` is appended, also stores the address of the following instruction in LR.
`b`      |  11100 | `xs`     | Sets PC to contents of operand 2. Takes one cycle longer to flush pipeline compared to branches to immediates.

Although using a register's content as the offset is possible, it is be impractical because it is an offset from the current instruction, which is calculated by an assembler if it is an immediate. In these cases, an ALU instruction may be better suited, like when branching to LR. For this reason, **a branch to a register is compiled as an add with the zero register and PC as the destination, which is 1 cycle slower than a branch to immediate**.

> **Note** The PC is is ahead of the current instruction by 3 at any point in time due to it pointing to the instruction currently in the fetch stage. The offset should account for this.

### Special control instructions
There are 2 control instructions, meant to provide ways to manipulate otherwise hidden aspects of the CPU.

Mnemonic | Opcode | Operands | Description
-------- | ------ | -------- | -----------
`setf`   |  11101 | `xs/imm` | Sets flags to the lower 4 bits of operand 2, corresponding from MSB to LSB to Z, N, V and E respectively. E.g. 0xc = 0b1100, which sets the Z and N flags and unsets the V and E flags.
`brk`    |  11110 |          | Stop clock and resume only manually.
`wait`   |  11110 | `t1/t2`  | Same instruction as brk, but clock restarts when the given timer reaches 0.

## Memory
The memory map is split into several different areas mapped onto different components. Notably, the lowest addresses are dedicated program ROM and as such are only executable and not readable, unlike other areas which are not executable.

Memory is addressed by 32-bit word, which simplifies the memory interface signifficantly. Byte addressing can be simulated with additional instructions.

The addressable range is, of course, much larger than the 20K words, or 80KB used. The main drivers of a small memory space are the KISS principle and trying to fit everything into 15 bits or less, so A-type instructions can address the entire memory map without going into negative values.

```
0x4FFF  |-------------|
0x4C00  | Unused      | 1K words
0x4BFF  |-------------|
0x4400  | VRAM        | 2K words
0x43FF  |-------------|
0x4000  | GPIO        | 1K words
0x3FFF  |-------------|
0x2000  | RAM         | 8K words
0x1FFF  |-------------|
0x1000  | Data ROM    | 4K words
0x0FFF  |-------------|
0x0000  | Program ROM | 4K words
        |-------------|
```

### Programming with variables
Memory addresses defined in assembly language using `@var` and `@array` refer to RAM addresses, but must be initialized to a value. This value, however, is stored in Data ROM. To initialize the variables in RAM, a constant `_datalen` is defined to provide the length of the Data ROM occupied by variables, which can then be used to copy data over to RAM.

The constants defined by the assembler relating to the memory map are:
- `_datastart`: 0x1000
- `_datalen`: length of Data ROM occupied by data variables and arrays
- `_ramstart`: 0x2000
