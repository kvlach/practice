const std = @import("std");

const localFileHeaderSignature = 0x04034b50;

const LocalFileHeader = struct{
	version: u16,
	flags: u16,
	compressionMethod: u16,
	modTime: u16,
	modDate: u16,
	crc32: u32,
	sizeCompressed: u32,
	sizeUncompressed: u32,
	fileNameLen: u16,
	extraFieldLen: u16,
	fileName: []u8,
	extraField: []u8,

	fn parse(raw: []u8) LocalFileHeader {
		if (raw.len < @sizeOf(LocalFileHeader)) {
			std.debug.panic("not enough data");
		}

		const signature = (raw[0] << 12) | (raw[1] << 8) | (raw[2] << 4) | raw[3];
		if (signature != localFileHeaderSignature) {
			std.debug.panic("expected signature {x} got {x}", .{localFileHeaderSignature, signature});
		}
	}
};

const DataDescriptor = struct {
	crc32: u32,
	sizeCompressed: u32,
	sizeUncompressed: u32,
};

const Data = struct {
	localFileHeader: LocalFileHeader,
	encryptionHeader: u32, // TODO
	fileData: []u8,
	dataDescriptor: DataDescriptor,
};

const ArchiveExtraDataRecord = struct{
	signature: u32, // 0x08064b50
	extraFieldLen: u32,
	extraField: []u8,
};

const CentralDirectoryHeader = struct {
	signature: u32, // 0x02014b50
	versionMadeBy: u16,
	versionNeededToExtract: u16,
	flags: u16,
	compressionMethod: u16,
	modTime: u16,
	modDate: u16,
	crc32: u32,
	sizeCompressed: u32,
	sizeUncompressed: u32,
	fileNameLen: u16,
	extraFieldLen: u16,
	fileCommentLen: u16,
	diskNumberStart: u16,
	internalFileAttributes: u16,
	externalFileAttributes: u32,
	relativeOffsetOfLocalHeader: u32,
	fileName: []u8,
	extraField: []u8,
	fileComment: []u8,
};

const DigitalSignature = struct{
	headerSignature: u32, // 0x05054b50
	size: u16,
	data: []u8,
};

const Zip64EndOfCentralDirectoryRecord = struct {
	signature: u32, // 0x06064b50
	size: u64,
	versionMadeBy: u16,
	versionNeededToExtract: u16,
	numberOfThisDisk: u32,
	numberOfTheDiskWithTheStartOfTheCentralDirectory: u32,
	totalEntriesOnThisDisk: u64,
	totalEntriesInCentralDirectory: u64,
	sizeCentralDirectory: u64,
	offsetStartCentralDirectoryWithStartingDiskNumber: u64,
	extensibleDataSector: []u8,
};

const Zip64EndOfCentralDirectoryLocator = struct {
	signature: u32, // 0x07064b50
	numberOfTheDiskWithTheStartOfEndOfCentralDirectory: u32,
	relativeOffsetOfCentralDirectoryRecord: u64,
	numberOfDisks: u32,
};

const EndOfCentralDirectoryRecord = struct {
	signature: u32, // 0x06054b50
	numberOfThisDisk: u16,
	numberOfTheDiskWithTheStartOfTheCentralDirectory: u16,
	totalEntriesInCentralDirectoryOnThisDisk: u16,
	centralDirectorySize: u32,
	offsetStartCentralDirectoryWithStartingDiskNumber: u32,
	zipFileCommentLen: u16,
	zipFileComment: []u8,
};

const Zip = struct {
	data: []Data,
	archiveDecryptionHeader: bool, // TODO
	archiveExtraDataRecord: ArchiveExtraDataRecord,
	centralDirectoryHeaders: []CentralDirectoryHeader,
	digitalSignature: DigitalSignature,
	zip64EndOfCentralDirectoryRecord: Zip64EndOfCentralDirectoryRecord,
	zip64EndOfCentralDirectoryLocator: Zip64EndOfCentralDirectoryLocator,
	endOfCentralDirectoryRecord: EndOfCentralDirectoryRecord,
};

pub fn main() !void {
	var file = try std.fs.cwd().openFile("test.zip", .{});
	defer file.close();

	var buf: [1024*1024]u8 = undefined;
	@memset(&buf, 0);

	const n = try file.reader().readAll(&buf);
	std.debug.print("read {} bytes\n", .{n});
}
