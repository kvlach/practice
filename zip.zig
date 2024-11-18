const std = @import("std");

const fixed_code_literals_match_arr = [_]?u5{
	0b00000,
	0b00001,
	0b00010,
	0b00011,
	0b00100,
	0b00101,

	0b00110,
	0b10111,

	0b11000,

	0b11001,
	0b11010,
	0b11011,
	0b11100,
	0b11101,
	0b11110,
	0b11111,
};

fn match_literal(bits [9]u1) u9 {
	const prefix = @as(u5, bits[0]) << 4
                 | @as(u5, bits[1]) << 3
                 | @as(u5, bits[2]) << 2
                 | @as(u5, bits[3]) << 1
                 | @as(u5, bits[4]) << 0;

    const n = switch(prefix) {
    	0b00110, 0b10111 => {
    		const actual = @as(u9, prefix)
    		             | @as(u9, bits[5]) << 3
    		             | @as(u9, bits[6]) << 2
    		             | @as(u9, bits[7]) << 1;
    		actual - 48
		},
    	0b11000 => {
    		const actual = @as(u9, prefix)
    		             | @as(u9, bits[5]) << 3
    		             | @as(u9, bits[6]) << 2
    		             | @as(u9, bits[7]) << 1;
    		actual + 78
		},
    	else => std.debug.panic("unexpected prefix", .{});
    }
}

pub fn main() !void {
	const data = [_]u8{ 51, 52, 50, 54, 28, 124, 136, 11, 0 };
	std.debug.print("{b:8}\n", .{data[3]});
}
