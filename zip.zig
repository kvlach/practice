const std = @import("std");

const Bits = struct {
	bits: std.ArrayList(u1),
	index: usize,

	fn byte_to_bits(byte: u8) [8]u1 {
		const a: u1 = @intCast(byte >> 7);
		const b: u1 = @intCast((byte >> 6) & 0b1);
		const c: u1 = @intCast((byte >> 5) & 0b1);
		const d: u1 = @intCast((byte >> 4) & 0b1);
		const e: u1 = @intCast((byte >> 3) & 0b1);
		const f: u1 = @intCast((byte >> 2) & 0b1);
		const g: u1 = @intCast((byte >> 1) & 0b1);
		const h: u1 = @intCast(byte & 0b1);
		return [8]u1{ a, b, c, d, e, f, g, h };
	}

	fn init(allocator: std.mem.Allocator, bytes: []u8) !Bits {
		var bits = std.ArrayList(u1).init(allocator);

		for (bytes) |byte| {
			const part = byte_to_bits(byte);
			try bits.append(part[0]);
			try bits.append(part[1]);
			try bits.append(part[2]);
			try bits.append(part[3]);
			try bits.append(part[4]);
			try bits.append(part[5]);
			try bits.append(part[6]);
			try bits.append(part[7]);
		}

		return Bits{
			.bits = bits,
			.index = 0,
		};
	}

	fn deinit(self: *Bits) void {
		self.bits.deinit();
	}

	fn peek(self: *Bits, comptime T: type) T {
		const size = @bitSizeOf(T);
		if (self.index+size > self.bits.items.len) {
			std.debug.panic("overflow", .{});
		}
		var n: T = 0;
		for (0..size) |i| {
			n |= @as(T, self.bits.items[self.index+i]) << @intCast(size-i-1);
		}
		return n;
	}

	fn read(self: *Bits, comptime T: type) T {
		const n = self.peek(T);
		self.index += @bitSizeOf(T);
		return n;
	}

	fn number_of_bits_to_read(self: *Bits) usize {
		const peek5 = self.peek(u5);
		if (0b00000 <= peek5 and peek5 <= 0b00101) {
			return 7;
		}
		if (0b00110 <= peek5 and peek5 <= 0b10111) {
			return 8;
		}
		if (peek5 == 0b11000) {
			return 8;
		}
		if (0b11001 <= peek5 and peek5 <= 0b11111) {
			return 9;
		}
		std.debug.panic("unreachable". {});
	}

	fn next_literal_value(self: *Bits) i10 {
		const num_of_bits = self.number_of_bits_to_read();
	}
};

pub fn main() !void {
	var gpa = std.heap.GeneralPurposeAllocator(.{}){};
	const allocator = gpa.allocator();
	const data = [_]u8{ 51, 52, 50, 54, 28, 124, 136, 11, 0 };
	var bits = try Bits.init(allocator, @constCast(&data));
	std.debug.print("{any}\n", .{bits.bits.items});
	std.debug.print("{b:5}\n", .{bits.read(u5)});
	std.debug.print("{b:5}\n", .{bits.read(u5)});
	std.debug.print("{b:5}\n", .{bits.read(u5)});
	std.debug.print("{b:5}\n", .{bits.read(u5)});
}
