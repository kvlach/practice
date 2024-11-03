const std = @import("std");

const Buffer = struct {
	buffer: []u8,
	index: usize,
	allocator: std.mem.Allocator,

	fn init(allocator: std.mem.Allocator, raw: []u8) !*Buffer {
		const ptr = try allocator.create(Buffer);
		ptr.* = .{
			.buffer = raw,
			.index = 0,
			.allocator = allocator,
		};
		return ptr;
	}

	fn deinit(self: *Buffer) void {
		self.allocator.destroy(self);
	}

	fn next(self: *Buffer) u8 {
		const it = self.buffer[self.index];
		self.index += 1;
		return it;
	}

	fn u16le(self: *Buffer) u16 {
		return self.next() | (@as(u16, self.next()) << 8);
	}

	fn u32le(self: *Buffer) u32 {
		return self.next() | (@as(u32, self.next()) << 8) | (@as(u32, self.next()) << 16) | (@as(u32, self.next()) << 24);
	}
};

const LocalFileHeader = struct {
	version: u16,
	flags: u16,
	compressionMethod: u16,
	modTime: u16,
	modDate: u16,
	crc32: u32,
	compressedSize: u32,
	uncompressedSize: u32,
	fileNameLen: u16,
	extraFieldLen: u16,
	fileName: []u8,

	fn signature() u32 {
		return 0x04034b50;
	}

	fn init(allocator: std.mem.Allocator, raw: []u8) !LocalFileHeader {
		const buf = try Buffer.init(allocator, raw);
		defer buf.deinit();

		if (LocalFileHeader.signature() != buf.u32le()) {
			std.debug.panic("invalid signature", .{});
		}

		const version = buf.u16le();
		const flags = buf.u16le();
		const compressionMethod = buf.u16le();
		const modTime = buf.u16le();
		const modDate = buf.u16le();
		const crc32 = buf.u32le();
		const compressedSize = buf.u32le();
		const uncompressedSize = buf.u32le();
		const fileNameLen = buf.u16le();
		const extraFieldLen = buf.u16le();
		const fileName = buf.buffer[buf.index..buf.index+fileNameLen];
		// const extraField = ;

		return LocalFileHeader{
			.version = version,
			.flags = flags,
			.compressionMethod = compressionMethod,
			.modTime = modTime,
			.modDate = modDate,
			.crc32 = crc32,
			.compressedSize = compressedSize,
			.uncompressedSize = uncompressedSize,
			.fileNameLen = fileNameLen,
			.extraFieldLen = extraFieldLen,
			.fileName = fileName,
		};
	}
};

pub fn main() !void {
	var file = try std.fs.cwd().openFile("test.zip", .{});
	defer file.close();

	var buf: [1024]u8 = undefined;
	const n = try file.readAll(&buf);

	var gpa = std.heap.GeneralPurposeAllocator(.{}){};
	const allocator = gpa.allocator();

	const lfh = try LocalFileHeader.init(allocator, buf[0..n]);
	std.debug.print("fname={s} lfh={}\n", .{lfh.fileName, lfh});
}
