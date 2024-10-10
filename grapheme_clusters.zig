const std = @import("std");

fn decode(s: []const u8) [1024]u21 {
    var codepoint: u21 = 0;
    var codepoints: [1024]u21 = undefined;
    @memset(&codepoints, 0);

    var i: usize = 0;
    var j: usize = 0;
    while (i < s.len) {
        if (s[i] >> 7 == 0) {
            codepoint = s[i];
            i += 1;
        } else if (s[i] >> 5 == 0b110) {
            codepoint = @as(u21, s[i] & 0b11111) << 6;
            codepoint |= s[i + 1] & 0b111111;
            i += 2;
        } else if (s[i] >> 4 == 0b1110) {
            codepoint = @as(u21, s[i] & 0b1111) << 12;
            codepoint |= @as(u21, s[i + 1] & 0b111111) << 6;
            codepoint |= s[i + 2] & 0b111111;
            i += 3;
        } else if (s[i] >> 3 == 0b11110) {
            codepoint = @as(u21, s[i] & 0b111) << 18;
            codepoint |= @as(u21, s[i + 1] & 0b111111) << 12;
            codepoint |= @as(u21, s[i + 2] & 0b111111) << 6;
            codepoint |= s[i + 3] & 0b111111;
            i += 4;
        } else {
            @panic("malformed input");
        }

        codepoints[j] = codepoint;
        j += 1;
    }

    return codepoints;
}

pub fn main() !void {
    const s = "aα🧟‍♀️";
    const codepoints = decode(s);

    for (codepoints) |codepoint| {
        std.debug.print("{x} ", .{codepoint});
        if (codepoint == 0) {
            break;
        }

        if (codepoint == 0x200D) {
            // ZWJ
        }
    }
}
