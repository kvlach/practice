const std = @import("std");

fn parseU16(arr_ptr: *[*]u8) u16 {
	const arr = arr_ptr.*;
	const n = (@as(u16, arr[1]) << 8) | arr[0];
	arr_ptr.* = arr_ptr[2..];
	return n;
}

fn parseU32(arr_ptr: *[]u8) u32 {
	var arr = arr_ptr.*;

	const n = (@as(u32, arr[3]) << 24) | (@as(u32, arr[2]) << 16) | (@as(u32, arr[1]) << 8) | arr[0];

	arr = arr[4..];
	return n;
}


const LocalFileHeader = struct {
	version: u16,
	flags: u16,
	compressionMethod: u16,
	modTime: u16,
	modDate: u16,
	crc32: u32,
	compressedSize: u32,
	uncompressedSize: u32,
	// fileNameLen: u16,
	// extraFieldLen: u16,
	// fileName: []u8,
	// extraField: []u8,

	fn signature() u32 {
		return 0x04034b50;
	}

	fn init(raw: []u8) LocalFileHeader {
		const arr_ptr = @constCast(&raw);
		const unknown_arr: *[*]u8 = @ptrCast(arr_ptr);

		if (parseU32(arr_ptr) != LocalFileHeader.signature()) {
			std.debug.panic("invalid signature", .{});
		}

		std.debug.print("{any}\n", .{arr_ptr.*});
		const version = parseU16(unknown_arr);
		std.debug.print("{any}\n", .{arr_ptr.*});
		const flags = parseU16(unknown_arr);
		const compressionMethod = parseU16(unknown_arr);
		const modTime = parseU16(unknown_arr);
		const modDate = parseU16(unknown_arr);
		const crc32 = parseU32(arr_ptr);
		const compressedSize = parseU32(arr_ptr);
		const uncompressedSize = parseU32(arr_ptr);

		return LocalFileHeader {
			.version = version,
			.flags = flags,
			.compressionMethod = compressionMethod,
			.modTime = modTime,
			.modDate = modDate,
			.crc32 = crc32,
			.compressedSize = compressedSize,
			.uncompressedSize = uncompressedSize,
		};
	}
};

pub fn main() !void {
	var file = try std.fs.cwd().openFile("test.zip", .{});
	defer file.close();

	var raw: [1024]u8 = undefined;
	const n = try file.readAll(&raw);
	const lfh = LocalFileHeader.init(raw[0..n]);
	std.debug.print("{}\n", .{lfh});
}
