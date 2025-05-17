const std = @import("std");

const c = @cImport({
    @cInclude("stdlib.h");
    @cInclude("helpers.h");
    @cInclude("bp_creator.h");
    @cInclude("rom_bp_strings.h");
});

/// 4096 words
const program_rom = @embedFile("program_rom");
const program_rom_len = 0x1000;
/// 4096 words
const data_rom = @embedFile("data_rom");
const data_rom_len = 0x1000;

/// Allocates a string in which the program blueprint string is stored. Must be freed with freeRom()
pub fn genProgramRom(allocator: std.mem.Allocator, program: []i32) !?[*:0]u8 {
    const padded_program: []i32 = try allocator.alloc(i32, program_rom_len);
    defer allocator.free(padded_program);
    std.mem.copyForwards(i32, padded_program, program);
    for (program.len..program_rom_len) |i| {
        padded_program[i] = 0;
    }
    return c.bp_replace(program_rom, c.is_rom_12_placeholder, @ptrCast(padded_program), program.len);
}

/// Allocates a string in which the data blueprint string is stored. Must be freed with freeRom()
pub fn genDataRom(allocator: std.mem.Allocator, data: []i32) !?[*:0]u8 {
    const padded_data: []i32 = try allocator.alloc(i32, data_rom_len);
    defer allocator.free(padded_data);
    std.mem.copyForwards(i32, padded_data, data);
    for (data.len..data_rom_len) |i| {
        padded_data[i] = 0;
    }
    return c.bp_replace(data_rom, c.is_rom_12_placeholder, @ptrCast(padded_data), data.len);
}

pub fn freeRom(rom: [*c]u8) void {
    c.free(rom);
}
