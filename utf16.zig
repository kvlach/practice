const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("utf16.txt", .{});
    defer file.close();

    var buf: [1024]u8 = undefined;
    @memset(&buf, 0);
    const n = try file.readAll(&buf);

    var unicode: u20 = 0;

    var w1: u16 = 0;
    var w2: u16 = 0;

    var i: usize = 0;

    while (i < n) {
        w1 = @as(u16, buf[i]) << 8 | buf[i + 1];
        w2 = @as(u16, buf[i + 2]) << 8 | buf[i + 3];

        if (w1 < 0xD800 or w1 > 0xDFFF) {
            unicode = w1;
        } else if (!(0xD800 < w1 and w1 < 0xDBFF)) {
            @panic("malformed input w1");
        } else if (!(0xDC00 < w2 and w2 < 0xDFFF)) {
            @panic("malformed input w2");
        } else {
            unicode = @as(u20, (w1 & 0b1111111111) << 10 | (w2 & 0b1111111111)) + 0x10000;
        }

        i += 4;

        std.log.info("{}", .{unicode});
    }
}
