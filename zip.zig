const std = @import("std");

fn u8_to_bits(byte: u8) []u1 {
	const a: u1 = @intCast(byte >> 7);
	const b: u1 = @intCast((byte >> 6) & 0b1);
	const c: u1 = @intCast((byte >> 5) & 0b1);
	const d: u1 = @intCast((byte >> 4) & 0b1);
	const e: u1 = @intCast((byte >> 3) & 0b1);
	const f: u1 = @intCast((byte >> 2) & 0b1);
	const g: u1 = @intCast((byte >> 1) & 0b1);
	const h: u1 = @intCast(byte & 0b1);

	// const a = @intFromBool(byte >> 7 == 0b1);
	// const b = @intFromBool((byte >> 6) & 0b1 == 0b1);
	// const c = @intFromBool((byte >> 5) & 0b1 == 0b1);
	// const d = @intFromBool((byte >> 4) & 0b1 == 0b1);
	// const e = @intFromBool((byte >> 3) & 0b1 == 0b1);
	// const f = @intFromBool((byte >> 2) & 0b1 == 0b1);
	// const g = @intFromBool((byte >> 1) & 0b1 == 0b1);
	// const h = @intFromBool(byte & 0b1 == 0b1);

	return @constCast(&[8]u1{ a, b, c, d, e, f, g, h });
}

const Bits = struct {
	bits: []u1,

	fn init(bytes: []u8) Bits {
		var bits: [bytes.len*8]u1 = undefined;

		for (0.., bytes) |i, byte| {
			const chunk = u8_to_bits(byte);
			bits[i*8 + 0] = chunk[0];
			bits[i*8 + 1] = chunk[1];
			bits[i*8 + 2] = chunk[2];
			bits[i*8 + 3] = chunk[3];
			bits[i*8 + 4] = chunk[4];
			bits[i*8 + 5] = chunk[5];
			bits[i*8 + 6] = chunk[6];
			bits[i*8 + 7] = chunk[7];
		}

		return Bits {
			.bits = bits,
		};
	}
};

pub fn main() !void {
	const sample = [_]u8{ 51, 52, 50, 54, 28, 124, 136, 11, 0 };
	std.debug.print("{b:8}, {any}\n", .{sample[0], u8_to_bits(sample[0])});
	const bits = Bits.init(@constCast(&sample));
	std.debug.print("{any}\n", .{bits});
}
