const std = @import("std");

const Buffer = struct {
	index: usize,
	buffer: []u8,

	fn init(buffer: []u8, len: usize) *Buffer {
		const buf = &Buffer {
			.index = 0,
			.buffer = buffer[0..len],
		};
		std.debug.print("buf={}\n", .{buf});
		return @constCast(buf);
	}

	fn next(self: *Buffer) u8 {
		std.debug.print("buf={}\n", .{self});
		const item = self.buffer[self.index];
		self.index += 1;
		return item;
	}
};

fn readU16(buf: *Buffer) u16 {
	return buf.next() | (@as(u16, buf.next()) << 8);
}

fn readU32(buf: *Buffer) u32 {
	return buf.next() | (@as(u32, buf.next()) << 8) | (@as(u32, buf.next()) << 16) | (@as(u32, buf.next()) << 24);
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
	fileName: []u8,
	extraField: []u8,

	fn signature() u32 {
		return 0x04034b50;
	}

	fn init(buf: *Buffer) LocalFileHeader {
		if (readU32(buf) != LocalFileHeader.signature()) {
			std.debug.panic("invalid signature", .{});
		}

		const version = readU16(buf);
		const flags = readU16(buf);
		const compressionMethod = readU16(buf);
		const modTime = readU16(buf);
		const modDate = readU16(buf);
		const crc32 = readU32(buf);
		const compressedSize = readU32(buf);
		const uncompressedSize = readU32(buf);
		const fileNameLen = readU16(buf);
		const extraFieldLen = readU16(buf);
		const fileName = buf.buffer[buf.index..buf.index+fileNameLen];
		const extraField = buf.buffer[buf.index+fileNameLen..buf.index+fileNameLen+extraFieldLen];

		return LocalFileHeader{
			.version = version,
			.flags = flags,
			.compressionMethod = compressionMethod,
			.modTime = modTime,
			.modDate = modDate,
			.crc32 = crc32,
			.compressedSize = compressedSize,
			.uncompressedSize = uncompressedSize,
			// .fileNameLen = fileNameLen,
			// .extraFieldLen = extraFieldLen,
			.fileName = fileName,
			.extraField = extraField,
		};
	}
};

pub fn main() !void {
	var file = try std.fs.cwd().openFile("test.zip", .{});
	defer file.close();

	var raw: [1024]u8 = undefined;
	const n = try file.readAll(&raw);

	const buf = Buffer.init(&raw, n);
	// std.debug.print("raw={any}\n", .{raw[0..n]});
	const lfh = LocalFileHeader.init(buf);
	std.debug.print("{}\n", .{lfh});
}
