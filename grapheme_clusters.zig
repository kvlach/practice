const std = @import("std");

pub fn main() !void {
    // var iter = std.unicode.Utf8Iterator{ .bytes = "aα", .i = 0 };
    var iter = (try std.unicode.Utf8View.init("aα👩‍👧‍👦👩‍🦯")).iterator();

    const graphemeExtend = true;
    const generalCategory = "Spacing_Mark";
    const emojiModifier = true;

    var extend = false;
    var zwj = false;
    var spacingMark = false;

    while (iter.nextCodepoint()) |codepoint| {
        if (codepoint == 0x200D) {
            zwj = true;
        }
        defer if (zwj == true and codepoint != 0x200D) {
            zwj = false;
        };

        extend = graphemeExtend == true or generalCategory == "Spacing_Mark" or emojiModifier == true and !zwj;

        spacingMark = switch (codepoint) {
            0x0E33, 0x0EB3 => true,
            else => false,
        };

        if (zwj == true) {
            std.debug.print(" ", .{});
        } else {
            std.debug.print("\n", .{});
        }

        std.debug.print("{x}", .{codepoint});
    }
}
