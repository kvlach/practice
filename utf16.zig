const std = @import("std");

fn decode(path: []const u8) !void {
    var buf: [1024]u8 = undefined;
    @memset(&buf, 0);
    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const n = try file.readAll(&buf);

    std.debug.print("{} {x}\n", .{ n, buf[0..n] });

    var i: usize = 0;
    var b1: u20 = 0;
    var b2: u20 = 0;
    var w1: u20 = 0;
    var w2: u20 = 0;
    var unicode: u20 = 0;
    while (i < n) {
        defer std.debug.print("{x}\n", .{unicode});

        b1 = @as(u20, buf[i]);
        b2 = @as(u20, buf[i + 1]);
        w1 = (b1 << 8) | b2;

        if (w1 < 0xD800 or w1 > 0xDFFF) {
            unicode = w1;
            i += 2;
            continue;
        }

        if (!(w1 >= 0xD800 and w1 <= 0xDBFF)) {
            @panic("malformed w1");
        }

        b1 = @as(u20, buf[i + 2]);
        b2 = @as(u20, buf[i + 3]);
        w2 = (b1 << 8) | b2;

        if (!(w2 >= 0xDC00 or w2 <= 0xDFFF)) {
            @panic("malformed w2");
        }

        unicode = (w1 << 10) | (w2 & 0b1111111111) + 0x10000;
        i += 4;
    }
}

pub fn main() !void {
    try decode("utf16.txt");
}
