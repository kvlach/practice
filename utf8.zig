const std = @import("std");

fn encode(unicode: u21) void {
    var b1: u8 = 0;
    var b2: u8 = 0;
    var b3: u8 = 0;
    var b4: u8 = 0;

    if (unicode <= 0x7f) {
        b1 = @intCast(unicode & 0b1111111);
    } else if (unicode <= 0x7ff) {
        b1 = @intCast(unicode >> 6);
        b1 |= 0b11000000;
        b2 = @intCast(unicode & 0b111111);
        b2 |= 0b10000000;
    } else if (unicode <= 0xffff) {
        b1 = @intCast(unicode >> 12);
        b1 |= 0b11100000;
        b2 = @intCast((unicode >> 6) & 0b111111);
        b2 |= 0b10000000;
        b3 = @intCast(unicode & 0b111111);
        b3 |= 0b10000000;
    } else if (unicode <= 0x10ffff) {
        b1 = @intCast(unicode >> 18);
        b1 |= 0b11100000;
        b2 = @intCast((unicode >> 12) & 0b111111);
        b2 |= 0b10000000;
        b3 = @intCast((unicode >> 6) & 0b111111);
        b3 |= 0b10000000;
        b4 = @intCast(unicode & 0b111111);
        b4 |= 0b10000000;
    } else {
        @panic("malformed input");
    }

    std.log.debug("{b} {b} {b} {b}\n", .{ b1, b2, b3, b4 });
}

fn decode(s: []const u8) void {
    var unicode: u21 = 0;

    var i: usize = 0;
    while (i < s.len) {
        if (s[i] >> 7 == 0) {
            unicode = s[i] & 0b1111111;
            i += 1;
        } else if (s[i] >> 5 == 0b110) {
            unicode = @as(u21, s[i] & 0b11111) << 6;
            unicode |= s[i + 1] & 0b111111;
            i += 2;
        } else if (s[i] >> 4 == 0b1110) {
            unicode = @as(u21, s[i] & 0b1111) << 12;
            unicode |= @as(u21, s[i + 1] & 0b111111) << 6;
            unicode |= s[i + 2] & 0b111111;
            i += 3;
        } else if (s[i] >> 3 == 0b11110) {
            unicode = @as(u21, s[i] & 0b111) << 18;
            unicode |= @as(u21, s[i + 1] & 0b111111) << 12;
            unicode |= @as(u21, s[i + 2] & 0b111111) << 6;
            unicode |= s[i + 3] & 0b111111;
            std.log.debug("{b} {b} {b} {b}", .{ s[i], s[i + 1], s[i + 2], s[i + 3] });
            i += 4;
        } else {
            @panic("malformed input");
        }

        std.debug.print("{x}\n", .{unicode});
    }
}

pub fn main() !void {
    const s = "aα🕵️‍♀️";
    decode(s);
    encode(0x1f575);
}
