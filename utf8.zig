const std = @import("std");

pub fn main() !void {
    const s = "aα";
    std.debug.print("aα\n", .{});
    // std.debug.print("{} {} {}\n", .{ s[0], s[1], s[2] });

    var last_code_point: usize = 0;

    const len = s.len;
    var i: usize = 0;
    while (i < len) {
        if (last_code_point <= 0x7f) {
            std.debug.print("{b}\n", .{s[i]});
            i += 1;
        } else if (last_code_point <= 0x7ff) {
            std.debug.print("{} {}\n", .{ s[i], s[i + 1] });
            i += 2;
        }
        last_code_point = s[i - 1];
        std.debug.print("last_code_point = {}\n", .{last_code_point});
    }

    // for (s) |c| {
    //     std.debug.print("{}\n", .{c});
    // }
}
