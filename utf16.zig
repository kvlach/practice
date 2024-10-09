const std = @import("std");

fn decode(buf: [1024]u8, n: usize) void {
    var w1: u20 = 0;
    var w2: u20 = 0;
    var unicode: u20 = 0;
    var i: usize = 0;

    while (i < n) {
        defer std.log.info("{x}", .{unicode});

        w1 = @as(u20, buf[i]) << 8 | @as(u20, buf[i + 1]);

        if (w1 < 0xD800 or w1 > 0xDFFF) {
            unicode = w1;
            i += 2;
            continue;
        }

        if (!(0xD800 <= w1 and w1 <= 0xDBFF)) {
            @panic("malformed w1");
        }
        if (i + 4 > n) {
            @panic("expected to find w2, but couldn't");
        }

        w2 = @as(u20, buf[i + 2]) << 8 | @as(u20, buf[i + 3]);
        if (!(0xDC00 <= w2 and w2 <= 0xDFFF)) {
            @panic("malformed w2");
        }

        unicode = ((w1 & 0b1111111111) << 10) | (w2 & 0b1111111111) + 0x10000;
        i += 4;
    }
}

fn encode(unicode: u20) u32 {
    if (unicode < 0x10000) {
        return unicode;
    }
    const unicode_prime = unicode - 0x10000;
    var w1: u16 = @truncate(unicode_prime >> 10);
    w1 |= 0xD800;
    var w2: u16 = @truncate(unicode_prime & 0b1111111111);
    w2 |= 0xDC00;
    return (@as(u32, w1) << 16) | w2;
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("utf16.txt", .{});
    defer file.close();

    std.debug.print("ENCODING\n", .{});
    var buf: [1024]u8 = undefined;
    const n = try file.readAll(&buf);
    std.log.debug("{any}", .{buf[0..n]});
    decode(buf, n);

    std.debug.print("DECODING\n", .{});
    std.log.info("{x}", .{encode(0x3b1)});
    const two_wide = encode(0x1f9dc);
    const b1 = two_wide >> 24;
    const b2 = (two_wide >> 16) & 0xff;
    const b3 = (two_wide >> 8) & 0xff;
    const b4 = two_wide & 0xff;
    std.log.info("{} {} {} {}", .{ b1, b2, b3, b4 });
}
