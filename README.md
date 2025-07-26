# Punch CPU
A Factorio computer inspired by ARM and RISC-V

***Screenshot***

***Annotated screenshot***

The design makes heavy use of the textplates and nixie tubes mods, but they are not mandatory.

# Specs
- 18 versatile instructions with 4 different possible encodings
- 5 stage pipeline with flushing for taken branches and forwarding of values to mitigate data hazards
- 10Hz clock rate, i.e. 6 ticks per pipeline cycle and 10 instructions per second in the best case
- 4K 32-bit words (16KB) of program ROM, 4K words of data ROM, and 8K words (32KB) of RAM
- Instructions to stop and delay clock

## Planned
- General purpose memory mapped I/O ports
- Picture processing unit and memory mapped VRAM

# Executing programs
Aside from generating blueprint strings to paste over the program and data ROMs in-game, this tool comes with
and emulator to run programs in the terminal directly.

# See also

- [FC-tools](https://github.com/giodueck/FC-tools): assembler and emulator for a previous architecture, Horizon. Meant to be extensible but really isn't well organized, though it has some interesting code (so I took some into this project as well).
- [factorio-computing](https://github.com/giodueck/factorio-computing): blueprints and assemblers for three previous architectures: Overture, based on the CPU of the same name in the game "Turing Complete", Everest, my first homebrew design and build, and Horizon, my last CPU meant to improve upon and simplify the ISA of Everest.
