const std = @import("std");

const Bits = struct{
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
		const size: T = @intCast(@bitSizeOf(T));
		std.debug.print("{} {}\n", .{@TypeOf(size), size});
		if (self.index+size > self.bits.items.len) {
			std.debug.panic("overflow", .{});
		}
		const n: T = 0;
		for (0..size) |i| {
			const cast_i: T = @intCast(i);
			n |= @as(T, self.bits.items[self.index+i]) << size-cast_i-1;
		}
		return n;
	}

	fn read(self: *Bits, comptime T: type) T {
		const n = self.peek(T);
		self.index += @bitSizeOf(T);
		return n;
	}
};

pub fn main() !void {
	var gpa = std.heap.GeneralPurposeAllocator(.{}){};
	const allocator = gpa.allocator();

	const data = [_]u8{ 51, 52, 50, 54, 28, 124, 136, 11, 0 };

	var bits = try Bits.init(allocator, @constCast(&data));
	std.debug.print("{}\n", .{bits.peek(u5)});
}
