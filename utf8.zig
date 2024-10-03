const std = @import("std");

pub fn main() !void {
    var bits: u21 = 0;
    std.debug.print("{}\n", .{bits});

    const s = "aα🚴‍♂️";

    for (s) |c| {
        std.debug.print("{b} ", .{c});
    }
    std.debug.print("\n", .{});

    const len: usize = s.len;
    var i: usize = 0;
    while (i < len) {
        if (s[i] >> 7 == 0) {
            bits = s[i] & 0b1111111;
            i += 1;
        } else if (s[i] >> 5 == 0b110) {
            bits = ((s[i] & 0b11111) << 6) | (s[i + 1] & 0b111111);
            i += 2;
        } else if (s[i] >> 4 == 0b1110) {
            bits = ((s[i] & 0b1111) << 12) | ((s[i + 1] & 0b111111) << 6) | (s[i + 2] & 0b111111);
            i += 3;
        } else if (s[i] >> 3 == 0b11110) {
            bits = ((s[i] & 0b111) << 18) | ((s[i + 1] & 0b111111) << 12) | ((s[i + 2] & 0b111111) << 6) | (s[i + 3] & 0b111111);
            i += 4;
        } else {
            @panic("what this");
        }

        std.debug.print("{}\n", .{bits});
    }
}
