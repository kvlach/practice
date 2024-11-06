const std = @import("std");

const Bytes = struct {
	bytes: []u8,
	index: usize,

	fn u16le(self: *Bytes) u16 {
		if (self.index+2 > self.bytes.len) {
			std.debug.panic("overflow", .{});
		}
		var n = @as(u16, self.bytes[self.index+1]) << 8;
		n |= self.bytes[self.index];
		self.index += 2;
		return n;
	}

	fn u32le(self: *Bytes) u32 {
		if (self.index+4 > self.bytes.len) {
			std.debug.panic("overflow", .{});
		}
		var n = @as(u32, self.bytes[self.index+3]) << 24;
		n |= @as(u32, self.bytes[self.index+2]) << 16;
		n |= @as(u32, self.bytes[self.index+1]) << 8;
		n |= self.bytes[self.index];
		self.index += 4;
		return n;
	}

	fn slice(self: *Bytes, amount: usize) []u8 {
		if (self.index+amount > self.bytes.len) {
			std.debug.panic("overflow", .{});
		}
		const part = self.bytes[self.index..self.index+amount];
		self.index += amount;
		return part;
	}
};

const LocalFileHeader = struct {
	version_needed_to_extract: u16,
	general_purpose_bit_flag: u16,
	compression_method: u16,
	last_mod_file_time: u16,
	last_mod_file_date: u16,
	crc32: u32,
	compressed_size: u32,
	uncompressed_size: u32,
	file_name: []u8,
	extra_field: []u8,

	fn signature() u32 {
		return 0x04034b50;
	}

	fn init(bs: *Bytes) LocalFileHeader {
		if (LocalFileHeader.signature() != bs.u32le()) {
			std.debug.panic("invalid signature", .{});
		}

		const version_needed_to_extract = bs.u16le();
		const general_purpose_bit_flag = bs.u16le();
		const compression_method = bs.u16le();
		const last_mod_file_time = bs.u16le();
		const last_mod_file_date = bs.u16le();
		const crc32 = bs.u32le();
		const compressed_size = bs.u32le();
		const uncompressed_size = bs.u32le();
		const file_name_length = bs.u16le();
		const extra_field_length = bs.u16le();
		const file_name = bs.slice(file_name_length);
		const extra_field = bs.slice(extra_field_length);

		return LocalFileHeader{
			.version_needed_to_extract = version_needed_to_extract,
			.general_purpose_bit_flag = general_purpose_bit_flag,
			.compression_method = compression_method,
			.last_mod_file_time = last_mod_file_time,
			.last_mod_file_date = last_mod_file_date,
			.crc32 = crc32,
			.compressed_size = compressed_size,
			.uncompressed_size = uncompressed_size,
			.file_name = file_name,
			.extra_field = extra_field,
		};
	}
};

const DataDescriptor = struct {
	crc32: u32,
	compressed_size: u32,
	uncompressed_size: u32,

	fn init(bs: *Bytes) DataDescriptor {
		const crc32 = bs.u32le();
		const compressed_size = bs.u32le();
		const uncompressed_size = bs.u32le();

		return DataDescriptor {
			.crc32 = crc32,
			.compressed_size = compressed_size,
			.uncompressed_size = uncompressed_size,
		};
	}
};

const File = struct {
	local_file_header: LocalFileHeader,
	// encryption_header: ?EncryptionHeader,
	file_data: []u8,
	data_descriptor: ?DataDescriptor,

	fn init(bs: *Bytes) File {
		const lfh = LocalFileHeader.init(bs);
		const file_data = bs.slice(lfh.compressed_size);

		var data_descriptor: ?DataDescriptor = null;
		if ((lfh.general_purpose_bit_flag & 0b100) >> 3 == 1) {
			data_descriptor = DataDescriptor.init(bs);
		}

		return File {
			.local_file_header = lfh,
			.file_data = file_data,
			.data_descriptor = data_descriptor,
		};
	}
};

const ArchiveExtraDataRecord = struct {
    extra_field_data: []u8,

    fn signature() u32 {
    	return 0x08064b50;
    }

    fn init(bs: *Bytes) ArchiveExtraDataRecord {
		if (LocalFileHeader.signature() != bs.u32le()) {
			std.debug.panic("invalid signature", .{});
		}

    	const extra_field_length = bs.u32le();

    	return ArchiveExtraDataRecord {
    		.extra_field_data = bs.slice(extra_field_length),
    	};
    }
};

const Zip = struct {
	files: [1]File,
	// archive_decryption_header: ArchiveDecryptionHeader,
	archive_extra_data_record: ArchiveExtraDataRecord,

	pub fn init(raw: []u8) Zip {
		const bs = Bytes{
			.bytes = raw,
			.index = 0,
		};

		const files = [1]File{ File.init(@constCast(&bs)) };
		const archive_extra_data_record = ArchiveExtraDataRecord.init(@constCast(&bs));

		return Zip{
			.files = files,
			.archive_extra_data_record = archive_extra_data_record,
		};
	}
};

pub fn main() !void {
	var file = try std.fs.cwd().openFile("test.zip", .{});
	defer file.close();

	var buf: [1024]u8 = undefined;
	const n = try file.readAll(&buf);
	std.debug.print("raw={any}\n", .{buf[0..n]});

	const zip = Zip.init(buf[0..n]);
	std.debug.print("\n{}\n", .{zip.files[0]});
	std.debug.print("\n{b}\n", .{zip.files[0].local_file_header.general_purpose_bit_flag});
}
