const std = @import("std");

fn readU16(raw: []u8) u16 {
	return (@as(u16, raw[1]) << 8) | raw[0];
}
fn readU32(raw: []u8) u32 {
	return (@as(u32, raw[3]) << 24) | (@as(u32, raw[2]) << 16) | (@as(u32, raw[1]) << 8) | raw[0];
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
	fileNameLen: u16,
	extraFieldLen: u16,
	fileName: []u8,
	extraField: []u8,

	fn signature() u32 {
		return 0x04034b50;
	}

	fn init(raw: []u8) LocalFileHeader {
		if (LocalFileHeader.signature() != readU32(raw)) {
			std.debug.panic("incorrect signature", .{});
		}

		const version = readU16(raw[0..2]);
		const flags = readU16(raw[2..4]);
		const compressionMethod = readU16(raw[4..6]);
		const modTime = readU16(raw[6..8]);
		const modDate = readU16(raw[8..10]);
		const crc32 = readU32(raw[10..14]);
		const compressedSize = readU32(raw[14..18]);
		const uncompressedSize = readU32(raw[18..22]);
		const fileNameLen = readU16(raw[22..24]);
		const extraFieldLen = readU16(raw[24..26]);
		const fileName = raw[26..fileNameLen+26];
		const extraField = raw[26+fileNameLen..26+fileNameLen+extraFieldLen];

		return LocalFileHeader {
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
			.extraField = extraField,
		};
	}
};

pub fn main() !void {
	var file = try std.fs.cwd().openFile("test.zip", .{});
	defer file.close();

	var buf: [1024]u8 = undefined;
	const n = try file.readAll(&buf);
	const lfh = LocalFileHeader.init(buf[0..n]);
	std.debug.print("{}\n", .{lfh});
	std.debug.print("{x}\n", .{lfh.fileName});
}
