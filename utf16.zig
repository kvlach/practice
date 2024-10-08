const std = @import("std");

fn decode(buf: [1024]u8, n: usize) void {
    std.log.debug("{any}", .{buf[0..n]});

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

        if (!(0xD800 <= w1 or w1 <= 0xDBFF)) {
            @panic("malformed w1");
        }
        if (i + 4 > n) {
            @panic("expected to find second byte");
        }

        w2 = @as(u20, buf[i + 2]) << 8 | @as(u20, buf[i + 3]);

        if (!(0xDC00 <= w2 or w2 <= 0xDFFF)) {
            @panic("malformed w2");
        }

        unicode = (w1 << 10) | (w2 & 0b1111111111) + 0x10000;
        i += 4;
    }
}

fn encode(unicode: u20) void {
    if (unicode > 0x10FFFF) {
        @panic("number too big");
    }

    if (unicode < 0x10000) {
        std.log.info("{}", .{unicode});
        return;
    }

    const unicode2 = unicode - 0x10000;
    const w1: u16 = 0xD800 | (@as(u16, unicode2) >> 10);
    const w2: u16 = 0xDC00 | (unicode2 & 0b1111111111);

    std.log.info("{} {}", w1, w2);
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("utf16.txt", .{});
    defer file.close();

    var buf: [1024]u8 = undefined;
    const n = try file.readAll(&buf);

    decode(buf, n);
    encode(0x3b1);
    encode(0x61);
}
