//! Basic functions not in the std library

const std = @import("std");
const math = std.math;

/// A version of std.math.powi that will silently overflow and wraparound
pub fn powi(comptime T: type, x: T, y: T) T {
    const bit_size = @typeInfo(T).int.bits;

    // `y & 1 == 0` won't compile when `does_one_overflow`.
    const does_one_overflow = math.maxInt(T) < 1;
    const is_y_even = !does_one_overflow and y & 1 == 0;

    if (x == 1 or y == 0 or (x == -1 and is_y_even)) {
        if (does_one_overflow) {
            // return error.Overflow;
        } else {
            return 1;
        }
    }

    if (x == -1) {
        return -1;
    }

    if (x == 0) {
        if (y > 0) {
            return 0;
        } else {
            // Infinity/NaN, not overflow in strict sense
            // return error.Overflow;
            return 0;
        }
    }
    // x >= 2 or x <= -2 from this point
    if (y >= bit_size) {
        // return error.Overflow;
    }
    if (y < 0) {
        return 0;
    }

    // invariant :
    // return value = powi(T, base, exp) * acc;

    var base = x;
    var exp = y;
    var acc: T = if (does_one_overflow) unreachable else 1;

    while (exp > 1) {
        if (exp & 1 == 1) {
            // const ov = @mulWithOverflow(acc, base);
            // if (ov[1] != 0) return error.Overflow;
            // acc = ov[0];
            acc = acc *% base;
        }

        exp >>= 1;

        // const ov = @mulWithOverflow(base, base);
        // if (ov[1] != 0) return error.Overflow;
        // base = ov[0];
        base = base *% base;
    }

    if (exp == 1) {
        // const ov = @mulWithOverflow(acc, base);
        // if (ov[1] != 0) return error.Overflow;
        // acc = ov[0];
        acc = acc *% base;
    }

    return acc;
}
