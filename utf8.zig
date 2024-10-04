const std = @import("std");

fn decode(s: []const u8) void {
    var unicode: u21 = 0;

    var i: usize = 0;
    while (i < s.len) {
        if (s[i] >> 7 == 0) {
            unicode = s[i];
            i += 1;
        } else if ((s[i] >> 5) == 0b110) {
            unicode = @as(u21, s[i] & 0b11111) << 6;
            unicode |= s[i + 1] & 0b111111;
            i += 2;
        } else if ((s[i] >> 4) == 0b1110) {
            unicode = @as(u21, s[i] & 0b1111) << 12;
            unicode |= @as(u21, s[i + 1] & 0b111111) << 6;
            unicode |= s[i + 2] & 0b111111;
            i += 3;
        } else if ((s[i] >> 3) == 0b11110) {
            unicode = @as(u21, s[i] & 0b111) << 18;
            unicode |= @as(u21, s[i + 1] & 0b111111) << 12;
            unicode |= @as(u21, s[i + 2] & 0b111111) << 6;
            unicode |= s[i + 3] & 0b111111;
            i += 4;
        } else {
            @panic("malformed input");
        }

        std.debug.print("{x}\n", .{unicode});
    }
}

pub fn main() !void {
    const s = "aα🧙‍♂️";
    decode(s);
}
