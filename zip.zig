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
		try bits.ensureTotalCapacity(bytes.len*8);

		for (bytes) |byte| {
			const part = Bits.byte_to_bits(byte);
			std.debug.print("{b:8} {any}\n", .{byte, part});
			try bits.appendSlice(@constCast(&part));
		}

		return Bits{
			.bits = bits,
			.index = 0,
		};
	}

	fn u8le(self: *Bits) u8 {
		if (self.index+8 > self.bits.items.len) {
			std.debug.panic("overflow", .{});
		}
		const n: u8 = @as(u8, self.bits.items[self.index+7]) << 7
		            | @as(u8, self.bits.items[self.index+6]) << 6
		            | @as(u8, self.bits.items[self.index+5]) << 5
		            | @as(u8, self.bits.items[self.index+4]) << 4
		            | @as(u8, self.bits.items[self.index+3]) << 3
		            | @as(u8, self.bits.items[self.index+2]) << 2
		            | @as(u8, self.bits.items[self.index+1]) << 1
		            | @as(u8, self.bits.items[self.index+0]) << 0;
		std.debug.print("n={b:8}\n", .{n});
		self.index += 8;
		return n;
	}
};

pub fn main() !void {
	var gpa = std.heap.GeneralPurposeAllocator(.{}){};
	const allocator = gpa.allocator();
	const bytes = [_]u8{ 51, 52, 50, 54, 28, 124, 136, 11, 0 };
	var bits = try Bits.init(allocator, @constCast(&bytes));
	std.debug.print("{any}\n", .{bits.bits.items});
	std.debug.print("{}\n", .{bits.u8le()});
}
