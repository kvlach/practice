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

	fn deinit(self: *Bits) void {
		self.bits.deinit();
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
		        | @as(u9, self.bits.items[self.index+7]) << 0;
		self.index += 9;
		return n;
	}

	fn number_of_bits_to_read(self: *Bits) usize {
		const n = self.u5peek();
		if (0b00000 <= n and n <= 0b00101) {
			return 7;
		}
		if (0b00110 <= n and n <= 0b10111 or n == 0b11000) {
			return 8;
		}
		if (0b11001 <= n and n <= 0b11111) {
			return 9;
		}
		std.debug.panic("unreachable", .{});
	}

	fn read_next(self: *Bits) i10 {
		return switch (self.number_of_bits_to_read()) {
			7 => @as(i10, self.u7read()),
			8 => @as(i10, self.u8read()),
			9 => @as(i10, self.u9read()),
			else => std.debug.panic("unreachable", .{}),
		};
	}

	fn literal_to_add(self: *Bits) i10 {
		const n = self.u5peek();
		if (0b00000 <= n and n <= 0b00101) {
			return 256;
		}
		if (0b00110 <= n and n <= 0b10111 or n == 0b11000) {
			return -48;
		}
		if (n == 0b11000) {
			return 78;
		}
		if (0b11001 <= n and n <= 0b11111) {
			return -256;
		}
		std.debug.panic("unreachable", .{});
	}

	pub fn next_literal_value(self: *Bits) i10 {
		const add = self.literal_to_add();
		const bits = self.read_next();
		return bits + add;
	}

	fn decode_length_distance(self: *Bits) void {
		const length = self.next_literal_value();
		const distance = self.u5peek();
		std.debug.print("length={} distance={}\n", .{length, distance});
	}
};

pub fn main() !void {
	var gpa = std.heap.GeneralPurposeAllocator(.{}){};
	const allocator = gpa.allocator();

	const data = [_]u8{ 51, 52, 50, 54, 28, 124, 136, 11, 0 };

	var bits = try Bits.init(allocator, @constCast(&data));
	defer bits.deinit();
	bits.decode_length_distance();
	// std.debug.print("{}\n", .{bits.next_literal_value()});
}
