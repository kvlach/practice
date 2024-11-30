const std = @import("std");

const BlockParts = enum {
	Literal,
	Length,
	Distance,
    EndOfBlock,
};

const BlockPart = struct {
	kind: BlockParts,
	value: i16,

	fn init(kind: BlockParts, value: i16) BlockPart {
		return BlockPart {
			.kind = kind,
			.value = value,
		};
	}
};

const Bits = struct {
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

		return Bits {
			.bits = bits,
			.index = 0,
			.expect_distance = false,
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

	fn number_to_add(self: *Bits) i16 {
		const peek5 = self.peek(u5);
		if (0b00000 <= peek5 and peek5 <= 0b00101) {
			return 256;
		}
		if (0b00110 <= peek5 and peek5 <= 0b10111) {
			return -48;
		}
		if (peek5 == 0b11000) {
			return 88;
		}
		if (0b11001 <= peek5 and peek5 <= 0b11111) {
			return -256;
		}
		std.debug.panic("unreachable", .{});
	}

	fn next(self: *Bits) ?BlockPart {
		if (self.expect_distance) {
			self.expect_distance = false;
			const n = @as(i16, self.read(u5));
			std.log.debug("distance without extra bits = {}", .{n});
			return switch (n) {
				0...3 => BlockPart.init(BlockParts.Distance, n+1),
				4 => BlockPart.init(BlockParts.Distance, n+1+self.read(u1)),
				5 => BlockPart.init(BlockParts.Distance, n+2+self.read(u1)),
				6 => BlockPart.init(BlockParts.Distance, n+3+self.read(u2)),
				7 => BlockPart.init(BlockParts.Distance, n+6+self.read(u2)),
				8 => BlockPart.init(BlockParts.Distance, n+9+self.read(u3)),
				9 => BlockPart.init(BlockParts.Distance, n+16+self.read(u3)),
				10 => BlockPart.init(BlockParts.Distance, n+23+self.read(u4)),
				11 => BlockPart.init(BlockParts.Distance, n+38+self.read(u4)),
				12 => BlockPart.init(BlockParts.Distance, n+53+self.read(u5)),
				13 => BlockPart.init(BlockParts.Distance, n+84+self.read(u5)),
				else => std.debug.panic("unreachable", .{}),
			};
		}

		const num_of_bits = self.number_of_bits_to_read();
		const num_to_add = self.number_to_add();

		const n = switch (num_of_bits) {
			7 => @as(i16, self.read(u7)),
			8 => @as(i16, self.read(u8)),
			9 => @as(i16, self.read(u9)),
			else => std.debug.panic("unreachable", .{}),
		} + num_to_add;

		return switch (n) {
			256 => return null, // BlockPart.init(BlockParts.EndOfBlock, n),
			257...285 => {
				self.expect_distance = true;
				return switch (n) {
					257...264 => BlockPart.init(BlockParts.Length, n-254),
					265...268 => BlockPart.init(BlockParts.Length, n-254+self.read(u1)),
					269...272 => BlockPart.init(BlockParts.Length, n-250+self.read(u2)),
					273...276 => BlockPart.init(BlockParts.Length, n-238+self.read(u3)),
					277...280 => BlockPart.init(BlockParts.Length, n-210+self.read(u4)),
					281...284 => BlockPart.init(BlockParts.Length, n-150+self.read(u5)),
					285 => BlockPart.init(BlockParts.Length, 0),
					else => std.debug.panic("unreachable", .{}),
				};
			},
			else => BlockPart.init(BlockParts.Literal, n),
		};
	}
};

pub fn main() !void {
	var gpa = std.heap.GeneralPurposeAllocator(.{}){};
	const allocator = gpa.allocator();
	const data = [_]u8{ 51, 52, 50, 54, 28, 124, 136, 11, 0 };
	var bits = try Bits.init(allocator, @constCast(&data));
	std.debug.print("{any}\n", .{bits.bits.items});
	while (bits.next()) |part| {
		std.debug.print("{}\n", .{part});
	}
}
