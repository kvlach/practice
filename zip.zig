const std = @import("std");

const Bytes = struct {
	bytes: []u8,
	index: usize,
	allocator: std.mem.Allocator,

	fn init(allocator: std.mem.Allocator, bytes: []u8) !*Bytes {
		const bs = try allocator.create(Bytes);
		bs.* = Bytes{
			.bytes = bytes,
			.index = 0,
			.allocator = allocator,
		};
		return bs;
	}

	fn deinit(self: *Bytes) void {
		self.allocator.destroy(self);
	}

	fn u16le(self: *Bytes) u16 {
		var n = @as(u16, self.bytes[self.index+1]) << 8;
		n |= self.bytes[self.index];
		self.index += 2;
		return n;
	}

	fn u32le(self: *Bytes) u32 {
		var n = @as(u32, self.bytes[self.index+3]) << 24;
		n |= @as(u32, self.bytes[self.index+2]) << 16;
		n |= @as(u32, self.bytes[self.index+1]) << 8;
		n |= self.bytes[self.index];
		self.index += 4;
		return n;
	}

	fn slice(self: *Bytes, amount: usize) []u8 {
		const part = self.bytes[self.index..self.index+amount];
		self.index += self.index+amount;
		return part;
	}
};

const LocalFileHeader = struct {
	version: u16,
	flags: u16,
	compressionMethod: u16,
	modTime: u16,
	modDate: u16,
	crc32: u32,
	compressizedSize: u32,
	uncompressizedSize: u32,
	fileName: []u8,
	extraField: []u8,

	fn signature() u32 {
		return 0x04034b50;
	}

	fn init(bs: *Bytes) LocalFileHeader {
		if (LocalFileHeader.signature() != bs.u32le()) {
			std.debug.panic("invalid signature", .{});
		}

		const version = bs.u16le();
		const flags = bs.u16le();
		const compressionMethod = bs.u16le();
		const modTime = bs.u16le();
		const modDate = bs.u16le();
		const crc32 = bs.u32le();
		const compressizedSize = bs.u32le();
		const uncompressizedSize = bs.u32le();
		const fileNameLen = bs.u16le();
		const extraFieldLen = bs.u16le();
		const fileName = bs.slice(fileNameLen);
		const extraField = bs.slice(extraFieldLen);

		return LocalFileHeader{
			.version = version,
			.flags = flags,
			.compressionMethod = compressionMethod,
			.modTime = modTime,
			.modDate = modDate,
			.crc32 = crc32,
			.compressizedSize = compressizedSize,
			.uncompressizedSize = uncompressizedSize,
			.fileName = fileName,
			.extraField = extraField,
		};
	}
};

const File = struct {
	lfh: LocalFileHeader,
	// eh: ?EncryptionHeader,
	// data: []u8,

};

const Zip = struct {
	lfh: LocalFileHeader,

	fn init(allocator: std.mem.Allocator, raw: []u8) !Zip {
		const bs = try Bytes.init(allocator, raw);
		defer bs.deinit();

		return Zip{
			.lfh = LocalFileHeader.init(bs),
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

	const zip = try Zip.init(allocator, buf[0..n]);
	std.debug.print("{any}\n", .{zip});
}
