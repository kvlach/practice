const std = @import("std");

const Bits = struct {
	bits: std.ArrayList(u1),
	index: usize,

	fn byte_to_bits(byte: u8) [8]u1 {
		const b1: u1 = @intCast(byte >> 7);
		const b2: u1 = @intCast((byte >> 6) & 0b1);
		const b3: u1 = @intCast((byte >> 5) & 0b1);
		const b4: u1 = @intCast((byte >> 4) & 0b1);
		const b5: u1 = @intCast((byte >> 3) & 0b1);
		const b6: u1 = @intCast((byte >> 2) & 0b1);
		const b7: u1 = @intCast((byte >> 1) & 0b1);
		const b8: u1 = @intCast(byte & 0b1);
		return [8]u1{ b1, b2, b3, b4, b5, b6, b7, b8 };
	}

	fn u5peek(self: *Bits) u5 {
		if (self.index+5 > self.bits.items.len) {
			std.debug.panic("overflow", .{});
		}
		const n = @as(u5, self.bits.items[self.index+0]) << 4
		        | @as(u5, self.bits.items[self.index+1]) << 3
		        | @as(u5, self.bits.items[self.index+2]) << 2
		        | @as(u5, self.bits.items[self.index+3]) << 1
		        | @as(u5, self.bits.items[self.index+4]) << 0;
		return n;
	}

	fn u7read(self: *Bits) u7 {
		if (self.index+7 > self.bits.items.len) {
			std.debug.panic("overflow", .{});
		}
		const n = @as(u7, self.bits.items[self.index+0]) << 6
		        | @as(u7, self.bits.items[self.index+1]) << 5
		        | @as(u7, self.bits.items[self.index+2]) << 4
		        | @as(u7, self.bits.items[self.index+3]) << 3
		        | @as(u7, self.bits.items[self.index+4]) << 2
		        | @as(u7, self.bits.items[self.index+5]) << 1
		        | @as(u7, self.bits.items[self.index+6]) << 0;
		self.index += 7;
		return n;
	}

	fn u8read(self: *Bits) u8 {
		if (self.index+8 > self.bits.items.len) {
			std.debug.panic("overflow", .{});
		}
		const n = @as(u8, self.bits.items[self.index+0]) << 7
		        | @as(u8, self.bits.items[self.index+1]) << 6
		        | @as(u8, self.bits.items[self.index+2]) << 5
		        | @as(u8, self.bits.items[self.index+3]) << 4
		        | @as(u8, self.bits.items[self.index+4]) << 3
		        | @as(u8, self.bits.items[self.index+5]) << 2
		        | @as(u8, self.bits.items[self.index+6]) << 1
		        | @as(u8, self.bits.items[self.index+7]) << 0;
		self.index += 8;
		return n;
	}

	fn u9read(self: *Bits) u9 {
		if (self.index+9 > self.bits.items.len) {
			std.debug.panic("overflow", .{});
		}
		const n = @as(u9, self.bits.items[self.index+0]) << 8
		        | @as(u9, self.bits.items[self.index+1]) << 7
		        | @as(u9, self.bits.items[self.index+2]) << 6
		        | @as(u9, self.bits.items[self.index+3]) << 5
		        | @as(u9, self.bits.items[self.index+4]) << 4
		        | @as(u9, self.bits.items[self.index+5]) << 3
		        | @as(u9, self.bits.items[self.index+6]) << 2
		        | @as(u9, self.bits.items[self.index+7]) << 1
		        | @as(u9, self.bits.items[self.index+8]) << 0;
		self.index += 9;
		return n;
	}

	fn num_of_bits_to_read(self: *Bits) usize {
		const n = self.u5peek();
		if (0b00000 <= n and n <= 0b00101) {
			return 7;
		}
		if (0b00110 <= n and n <= 0b10111) {
			return 8;
		}
		if (n == 0b11000) {
			return 8;
		}
		if (0b11001 <= n and n <= 0b11111) {
			return 9;
		}
		std.debug.panic("unreachable", .{});
	}

	fn read_next(self: *Bits) u9 {
		const bits_to_read = self.num_of_bits_to_read();
		return switch (bits_to_read) {
			7 => @as(u9, self.u7read()),
			8 => @as(u9, self.u8read()),
			9 => self.u9read(),
			else => std.debug.panic("unreachable", .{}),
		};
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

		return Bits {
			.bits = bits,
			.index = 0,
		};
	}
};

pub fn main() !void {
	var gpa = std.heap.GeneralPurposeAllocator(.{}){};
	const allocator = gpa.allocator();
	const data = [_]u8{ 51, 52, 50, 54, 28, 124, 136, 11, 0 };
	var bits = try Bits.init(allocator, @constCast(&data));
	std.debug.print("{any}\n", .{bits.bits.items});
	std.debug.print("{b:9}\n", .{bits.read_next()});
	std.debug.print("{b:9}\n", .{bits.read_next()});
	std.debug.print("{b:9}\n", .{bits.read_next()});
	std.debug.print("{b:9}\n", .{bits.read_next()});
	std.debug.print("{b:9}\n", .{bits.read_next()});
	std.debug.print("{b:9}\n", .{bits.read_next()});
	std.debug.print("{b:9}\n", .{bits.read_next()});
	std.debug.print("{b:9}\n", .{bits.read_next()});
	std.debug.print("{b:9}\n", .{bits.read_next()});
	std.debug.print("{b:9}\n", .{bits.read_next()});
}
