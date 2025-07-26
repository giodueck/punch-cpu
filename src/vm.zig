//! The Virtual Machine is an emulator of the Punch architecture. It aims to mimic the exact behavior of the
//! Factorio implementation, down to any quirks in the pipelining and, eventually, all the graphics and
//! I/O devices.

const std = @import("std");
const util = @import("util.zig");

// Todo: add PPU, when the implementation is done (in game)
// Todo: add GPIO, when the implementation is done

const X0 = 0;
const X1 = 1;
const X2 = 2;
const X3 = 3;
const X4 = 4;
const X5 = 5;
const X6 = 6;
const X7 = 7;
const X8 = 8;
const X9 = 9;
const X10 = 10;
const T1 = 11;
const T2 = 12;
const SP = 13;
const LR = 14;
const PC = 15;

pub const VM = struct {
    // Registers
    registers: [16]i32 = [_]i32{0} ** 16,
    flags: Flags = .{},

    // Memory map
    program_rom: [4096]i32 = [_]i32{0} ** 4096,
    data_rom: [4096]i32 = [_]i32{0} ** 4096,
    ram: [8192]i32 = [_]i32{0} ** 8192,

    // Pipeline buffers
    // The writeback stage forwards no buffer, as there is no next stage after it
    pipeline_buffers: [4]PipelineBuffer = [_]PipelineBuffer{
        PipelineBuffer{},
    } ** 4,
    forward_reg: u4 = 0,
    forward_value: i32 = 0,

    // State
    do_halt: bool = false,
    halted: bool = false,
    wait_reg: WaitCandidate = @enumFromInt(0),
    flush_cycles: u32 = 0,
    pc_inc: i32 = 1,

    /// Loads the given program into program ROM, its data, if any, goes into the data ROM.
    /// If program or data are empty, nothing changes in the respective ROM.
    /// If the input slices are too long, the portion which fits is loaded and the rest discarded without error.
    pub fn loadProgram(self: *VM, program: []const i32, data: []const i32) void {
        for (program, 0..) |word, i| {
            if (i >= self.program_rom.len) break;
            self.program_rom[i] = word;
        }
        for (data, 0..) |word, i| {
            if (i >= self.data_rom.len) break;
            self.data_rom[i] = word;
        }
    }

    /// Only sets PC to 0, does not clear any registers or memory positions or clear the flags register
    pub fn reset(self: *VM) void {
        self.registers[PC] = 0;
    }

    /// Write any register, X0 is a discarded write
    fn writeReg(self: *VM, reg: u4, value: i32) void {
        if (reg != X0) self.registers[reg] = value;
    }

    /// Read any register, X0 is always 0
    fn readReg(self: *VM, reg: u4) i32 {
        return if (reg == 0) 0 else self.registers[reg];
    }

    /// Runs the VM for one cycle, advancing the pipeline one step or 6 ticks
    /// Returns false if the clock is halted without a wait condition
    pub fn step(self: *VM) bool {
        // Clock high
        //  Fetch
        //  Decode
        //  Execute and memory access
        //  Writeback
        //
        // Clock low
        //  Read registers
        //
        // Pipeline looks like
        // F |D | R|E |W
        //   |F |D | R|E |W
        //   |  |F |D | R|E |W
        //   |  |  |F |D | R|E |W
        //   |  |  |  |F |D | R|E W
        //             ~~
        // The underlined column shows how writeback and read stages can run simultaneously: writeback occurs
        // first, which means the read in the second half of the stage takes to written values if the same
        // register is written and read.
        // It also illustrates that a result from the execute stage can be used in the read stage, however it
        // is instead forwarded before the value is actually written to the register.
        // These two details prevent data hazards.

        const fetch = PipelineBuffer{
            .instruction = self.fetchInstruction(),
            .next_ins_addr = self.registers[PC] + 1,
        };
        var decode = self.pipeline_buffers[0];
        var read = self.pipeline_buffers[1];
        var execute = self.pipeline_buffers[2];
        var writeback = self.pipeline_buffers[3];

        if (!self.halted) {
            // Clock high
            decode = self.decodeStage(&decode);
            execute = self.executeStage(&execute);
            self.writebackStage(&writeback);

            // Clock low
            read = self.readStage(&read);

            self.pipeline_buffers = .{ fetch, decode, read, execute };
        }

        // Count timers down one cycle
        var continue_from_halted = false;
        if (self.registers[T1] != 0) {
            if (self.registers[T1] <= 6) {
                continue_from_halted = (self.wait_reg == .T1);
                self.registers[T1] = 0;
            } else {
                self.registers[T1] -= 6;
            }
        }
        if (self.registers[T2] != 0) {
            if (self.registers[T2] <= 6) {
                continue_from_halted = (self.wait_reg == .T2);
                self.registers[T2] = 0;
            } else {
                self.registers[T2] -= 6;
            }
        }

        if (continue_from_halted) {
            self.halted = false;
            self.wait_reg = .None;
        }

        const ret = !self.halted;
        self.halted = self.halted or self.do_halt;
        self.do_halt = false;

        // return !(self.halted and self.wait_reg == .None);
        return ret;
    }

    fn decodeStage(self: *VM, buffer: *PipelineBuffer) PipelineBuffer {
        _ = self;
        buffer.type = @enumFromInt(buffer.rType().t);
        return buffer.*;
    }

    fn readStage(self: *VM, buffer: *PipelineBuffer) PipelineBuffer {
        var r: u4 = 0;
        var s: u4 = 0;
        var d: u4 = 0;
        switch (buffer.type) {
            .R => {
                const ins = buffer.rType();
                buffer.op1 = self.readReg(ins.r);
                buffer.op2 = self.readReg(ins.s);
                // store instructions use the destination register as the source instead
                buffer.result = self.readReg(ins.d);
                r = ins.r;
                s = ins.s;
                d = ins.d;
            },
            .I => {
                const ins = buffer.iType();
                buffer.op1 = self.readReg(ins.r);
                buffer.op2 = ins.i;
                // store instructions use the destination register as the source instead
                buffer.result = self.readReg(ins.d);
                r = ins.r;
                d = ins.d;
            },
            .A => {
                const ins = buffer.aType();
                buffer.op1 = 0;
                buffer.op2 = ins.a;
                // store instructions use the destination register as the source instead
                buffer.result = self.readReg(ins.d);
                d = ins.d;
            },
            .L => {
                const ins = buffer.lType();
                buffer.op1 = 0;
                buffer.op2 = ins.l;
                buffer.result = 0;
            },
        }

        // When a load/store instruction (opcodes 16/17) is encoded in R-type, address register modification is
        // supported
        if (buffer.type == .R and (buffer.rType().o & 0b11110 == 16)) {
            // Special purpose immediate
            const e = buffer.rType().e;
            // Pre/post bit: 1 is pre, 0 is post
            buffer.v = @intCast(e & 1);
            // Offset: the amount to add to the address register. Range is [-64,63]
            buffer.u = @intCast(e >> 1);
        }

        buffer.writeback = switch (buffer.rType().o) {
            0...16, 18 => true,
            else => false,
        };

        buffer.res_reg = d;
        buffer.op2_reg = s;

        // Forwarding: for this to work, executeStage must be called before this stage
        if (r == self.forward_reg) buffer.op1 = self.forward_value;
        if (s == self.forward_reg) buffer.op2 = self.forward_value;
        if (d == self.forward_reg) buffer.result = self.forward_value;

        return buffer.*;
    }

    fn executeStage(self: *VM, buffer: *PipelineBuffer) PipelineBuffer {
        self.pc_inc = 1;
        if (self.flush_cycles > 0) {
            self.flush_cycles -= 1;
            buffer.* = .{};
            return buffer.*;
        }
        std.debug.print("{any}\n{any}\n", .{ buffer, buffer.rType() });

        if (!self.cond(buffer.rType().c)) {
            buffer.* = .{};
            return buffer.*;
        }

        if (buffer.rType().c != 0) {
            std.debug.print("DEBUGPRINT[4]: vm.zig:253 (after if (buffer.rType().c != 0) )\n", .{});
        }

        var flags: Flags = .{};
        switch (buffer.rType().o) {
            // ALU
            0 => {
                buffer.result = buffer.op1 +% buffer.op2;
                flags.V = @addWithOverflow(buffer.op1, buffer.op2)[1] > 0;
            },
            1 => {
                buffer.result = buffer.op1 -% buffer.op2;
                flags.V = @subWithOverflow(buffer.op1, buffer.op2)[1] > 0;
            },
            2 => {
                buffer.result = buffer.op2 -% buffer.op1;
                flags.V = @subWithOverflow(buffer.op2, buffer.op1)[1] > 0;
            },
            3 => {
                buffer.result = buffer.op1 *% buffer.op2;
            },
            4 => {
                // Factorio treats a 0 signal as if it is not there at all, so a division by it results in
                // no result at all, i.e. 0
                buffer.result = if (buffer.op2 != 0) @divTrunc(buffer.op1, buffer.op2) else 0;
                flags.E = if (buffer.op2 == 0) true else false;
            },
            5 => {
                buffer.result = if (buffer.op2 != 0) @mod(buffer.op1, buffer.op2) else 0;
                flags.E = (buffer.op2 == 0);
            },
            6 => {
                buffer.result = if (buffer.op2 >= 0) util.powi(i32, buffer.op1, buffer.op2) else 0;
            },
            7 => {
                buffer.result = buffer.op1 << @intCast(@mod(buffer.op2, 32));
            },
            8 => {
                buffer.result = buffer.op1 >> @intCast(@mod(buffer.op2, 32));
            },
            9 => {
                buffer.result = buffer.op1 & buffer.op2;
            },
            10 => {
                buffer.result = buffer.op1 | buffer.op2;
            },
            11 => {
                buffer.result = buffer.op1 ^ buffer.op2;
            },
            // Load
            16 => {
                if (buffer.v == 1) buffer.op2 +%= buffer.u;
                buffer.result = self.load(buffer.op2);
                if (buffer.v != 1) buffer.op2 +%= buffer.u;
            },
            // Store
            17 => {
                if (buffer.v == 1) buffer.op2 +%= buffer.u;
                self.store(buffer.op2, buffer.result);
                if (buffer.v != 1) buffer.op2 +%= buffer.u;
            },
            // Load high HW
            18 => {
                buffer.result = buffer.op2 << 16;
            },
            // Branch (proper)
            28 => {
                self.pc_inc = buffer.op2;
                self.flush_cycles = 3;
            },
            // Set flags
            29 => {
                flags.Z = buffer.op2 & 1 > 0;
                flags.N = buffer.op2 & 2 > 0;
                flags.V = buffer.op2 & 4 > 0;
                flags.E = buffer.op2 & 8 > 0;
            },
            // Break/Wait
            30 => {
                self.do_halt = true;
                std.debug.print("DEBUGPRINT[2]: vm.zig:321 (after self.do_halt = true;)\n", .{});
                if (buffer.rType().e <= 2) self.wait_reg = @enumFromInt(buffer.rType().e) else self.wait_reg = .None;
            },
            else => {
                buffer.result = 0;
            },
        }

        // Branch by write to PC
        if (buffer.writeback and buffer.res_reg == PC) self.flush_cycles = 4;

        flags.Z = (buffer.result == 0);
        flags.N = (buffer.result < 0);

        self.forward_reg = buffer.res_reg;
        self.forward_value = buffer.result;

        if (buffer.rType().f) self.flags = flags;

        return buffer.*;
    }

    fn writebackStage(self: *VM, buffer: *PipelineBuffer) void {
        // PC is modified here. If it is not a destination, it is incremented by 1 or the offset set by a branch.
        // If it is, PC is set to a value instead.
        if (buffer.writeback) {
            self.writeReg(buffer.res_reg, buffer.result);
            if (buffer.res_reg != PC) self.registers[PC] +%= self.pc_inc;
        } else {
            self.registers[PC] +%= self.pc_inc;
        }
        // Link
        if (buffer.rType().f and buffer.rType().o == 28) {
            self.registers[LR] = buffer.next_ins_addr;
        }
        // Address source register modification
        if (buffer.u != 0) {
            if (buffer.v == 0) buffer.op2 +%= buffer.u;
            self.writeReg(buffer.op2_reg, buffer.op2);
        }
    }

    fn cond(self: *VM, condition_code: u4) bool {
        const Z = self.flags.Z;
        const N = self.flags.N;
        const V = self.flags.V;
        const E = self.flags.E;
        return switch (condition_code) {
            0 => true,
            1 => Z,
            2 => !Z,
            3 => N,
            4 => !N,
            5 => N != V,
            6 => Z or N != V,
            7 => !Z and N == V,
            8 => N == V,
            9 => V,
            10 => !V,
            11 => E,
            12 => !E,
            // 13 and 14 undefined and by default false. 15 defined as never
            else => false,
        };
    }

    /// Reads from program memory. Only supposed to be used by the fetch stage.
    /// If attempting to read general purpose ROM or RAM, returns 0.
    fn fetchInstruction(self: *VM) i32 {
        const addr = self.registers[PC];
        return switch (addr) {
            0x0000...0x0FFF => self.program_rom[@intCast(addr)],
            else => 0,
        };
    }

    /// Reads from a memory address accessible by instructions. If attempting to read from program ROM or any
    /// address not implemented, returns 0.
    fn load(self: *VM, addr: i32) i32 {
        return switch (addr) {
            0x1000...0x1FFF => self.data_rom[@intCast(addr)],
            0x2000...0x3FFF => self.ram[@intCast(addr)],
            else => 0,
        };
    }

    /// Writes to a memory address accessible by instructions. If attempting to write to any ROM or any
    /// address not implemented, this is a NOOP.
    fn store(self: *VM, addr: i32, value: i32) void {
        switch (addr) {
            0x2000...0x3FFF => {
                self.ram[@intCast(addr)] = value;
            },
            else => {},
        }
    }
};

const Flags = struct {
    Z: bool = false,
    N: bool = false,
    V: bool = false,
    E: bool = false,
};

/// Registers which the clock can wait to hit 0 to resume from the halted state
const WaitCandidate = enum(u2) {
    None = 0,
    T1 = 1,
    T2 = 2,
};

// Pipeline:
//  Fetch:
//      out: instruction
//  Decode:
//      out: type, immediate arguments, register arguments, condition code, flags enable, opcode, address of
//      next instruction (forwarded and only used by writeback)
//  Read registers:
//      out: operand 1 and 2 (select immediate or reg), read register or forwarded result, address source
//      register pre modification, calculate address source register modification, determine read or write to
//      memory, determine writeback enable
//  Execute or read memory:
//      All output is conditional. If the condition is false, all data described below is zero and nothing is
//          forwarded. If the CPU is flushing due to a recent branch, the condition is set to false regardless.
//      out: result of operation, forward result, read or write memory, branch, flush
//  Writeback:
//      Writes the result if writeback enabled, writes to the link register if branching and flags enabled,
//          address source register modification

const PipelineBuffer = struct {
    // Inputs
    instruction: i32 = 0,
    type: InstructionType = @enumFromInt(0),
    op1: i32 = 0,
    op2: i32 = 0,
    op2_reg: u4 = 0,
    result: i32 = 0,
    res_reg: u4 = 0,
    // Writeback enabled
    writeback: bool = false,
    /// The address of the following instruction, used by the `bl` instruction
    next_ins_addr: i32 = 0,
    // Address register modification for load/store
    u: i7 = 0,
    v: u1 = 0,

    pub fn rType(self: *const PipelineBuffer) RType {
        return @bitCast(self.instruction);
    }

    pub fn iType(self: *const PipelineBuffer) IType {
        return @bitCast(self.instruction);
    }

    pub fn aType(self: *const PipelineBuffer) AType {
        return @bitCast(self.instruction);
    }

    pub fn lType(self: *const PipelineBuffer) LType {
        return @bitCast(self.instruction);
    }
};

const InstructionType = enum(u2) {
    R,
    I,
    A,
    L,
};

// LS bits first
const RType = packed struct(i32) {
    c: u4,
    f: bool,
    t: u2 = 0,
    o: u5,
    d: u4,
    r: u4,
    s: u4,
    e: i8,
};

const IType = packed struct(i32) {
    c: u4,
    f: bool,
    t: u2 = 1,
    o: u5,
    d: u4,
    r: u4,
    i: i12,
};

const AType = packed struct(i32) {
    c: u4,
    f: bool,
    t: u2 = 2,
    o: u5,
    d: u4,
    a: i16,
};

const LType = packed struct(i32) {
    c: u4,
    f: bool,
    t: u2 = 3,
    o: u5,
    l: i20,
};

const Instruction = union(InstructionType) {
    R: RType,
    I: IType,
    A: AType,
    L: LType,
};
