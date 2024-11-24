const std = @import("std");

const Bits = struct{
	bits: std.ArrayList(u1),
	index: usize,
	expect_distance: bool,

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
			const part = Bits.byte_to_bits(byte);
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
		std.debug.panic("unreachable", .{});
	}

	fn number_to_add(self: *Bits) i10 {
		const peek5 = self.peek(u5);
		if (0b00000 <= peek5 and peek5 <= 0b00101) {
			return 256;
		}
		if (0b00110 <= peek5 and peek5 <= 0b10111) {
			return -48;
		}
		if (peek5 == 0b11000) {
			return 78;
		}
		if (0b11001 <= peek5 and peek5 <= 0b11111) {
			return -256;
		}
		std.debug.panic("unreachable", .{});
	}

	fn next_literal_or_length(self: *Bits) i10 {
		const num_of_bits = self.number_of_bits_to_read();
		const add = self.number_to_add();

		var n: i10 = 0;
		switch (num_of_bits) {
			7 => n = @as(i10, self.read(u7)),
			8 => n = @as(i10, self.read(u8)),
			9 => n = @as(i10, self.read(u9)),
			else => std.debug.panic("unreachable", .{}),
		}

		return n + add;
	}

	fn next_distance(self: *Bits) u5 {
		const distance_code = self.read(u5);
		return distance_code;
	}

	fn next(self: *Bits) Code {
		const lit_or_len = self.next_literal_or_length();
		const code = switch (lit_or_len) {
			256 => Code.EndOfBlock,
			257...285 => Code.Length
			else => Code.Literal,
		}

		if (code == Code.Length) {
			self.expect_distance = true;
		}
	}
};

const Code = enum {
	Literal,
	Length,
	EndOfBlock,
	Distance,
}

pub fn main() !void {
	var gpa = std.heap.GeneralPurposeAllocator(.{}){};
	const allocator = gpa.allocator();

	const data = [_]u8{ 51, 52, 50, 54, 28, 124, 136, 11, 0 };

	var bits = try Bits.init(allocator, @constCast(&data));
	std.debug.print("{any}\n", .{bits.bits.items});

	std.debug.print("{}\n", .{bits.next_literal()});
	std.debug.print("{}\n", .{bits.next_literal()});
	std.debug.print("{}\n", .{bits.next_literal()});
	std.debug.print("{}\n", .{bits.next_literal()});
	std.debug.print("{}\n", .{bits.next_literal()});
	std.debug.print("{}\n", .{bits.next_literal()});
	std.debug.print("{}\n", .{bits.next_literal()});
	std.debug.print("{}\n", .{bits.next_literal()});
	// std.debug.print("{}\n", .{bits.next_distance()});
}
