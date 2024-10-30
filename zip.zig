const std = @import("std");

fn readSignature(raw: []u8) u32 {
	return (@as(u32, raw[3]) << 24) | (@as(u32, raw[2]) << 16) | (@as(u32, raw[1]) << 8) | raw[0];
}

const LocalFileHeader = packed struct {
	extractionVersion: u16,
	flags: u16,
	compressionMethod: u16,
	modTime: u16,
	modDate: u16,
	crc32: u32,
	compressedSize: u32,
	uncompressedSize: u32,
	fileNameLen: u16,
	extraFieldLen: u16,
	// fileName: []u8,
	// extraField: []u8,

	fn signature() u32 {
		return 0x04034b50;
	}

	fn verify(raw: []u8) void {
		const sig = readSignature(raw);
		if (sig != LocalFileHeader.signature()) {
			std.debug.panic("unexpected signautre '{x}'", .{sig});
		}
	}
};

// const File = struct {
// 	lfh: LocalFileHeader,
// 	eh: EncryptionHeader,
// 	data: []u8,
// 	dd: DataDescriptor,
// };

// const Zip = struct {
// 	files: []File,
// 	adh: ArchiveDecryptionHeader,
// 	aedr: ArchiveExtraDataRecord,
// 	cdrs: []CentralDirectoryHeader,
// 	z64EndCdRecord: Zip64EndOfCentralDirectoryRecord,
// 	z64EndCdLocator: Zip64EndOfCentralDirectoryLocator,
// 	endCdLocator: EndOfCentralDirectoryRecord,
// };

pub fn main() !void {
	var file = try std.fs.cwd().openFile("test.zip", .{});
	defer file.close();

	var buf: [1024]u8 = undefined;
	@memset(&buf, 0);

	_ = try file.readAll(&buf);

	LocalFileHeader.verify(buf[0..4]);
	const lfh = try file.reader().readStruct(LocalFileHeader);
	std.debug.print("LocalFileHeader={any}\n", .{lfh});
}
