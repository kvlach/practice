const std = @import("std");

const Mn_Nonspacing_Mark = []u32{ 1, 2, 3 };

pub fn main() !void {
    const s = "aα🧚‍♂️";

    var iter = (try std.unicode.Utf8View.init(s)).iterator();

    while (iter.nextCodepoint()) |codepoint| {
        std.debug.print("{x}\n", .{codepoint});
    }

    // curl -O ftp://ftp.unicode.org/Public/UNIDATA/Scripts.txt
    var file = try std.fs.cwd().openFile("Scripts.txt", .{});
    defer file.close();

    var buf: [1024]u8 = undefined;
    @memset(&buf, 0);

    // const n = try file.read(&buf);
    // std.debug.print("{} {any}\n", .{ n, buf });

    while (file.reader().readUntilDelimiterOrEof(&buf, '\n')) |maybe_line| {
        if (maybe_line) |line| {
            std.debug.print("{s}\n", .{line});
        } else {
            break;
        }
    } else |err| {
        std.log.err("{}", .{err});
    }

    // while (true) {
    //     const n = try file.read(&buf);
    //     if (n == 0) {
    //         break;
    //     }
    // }
}
