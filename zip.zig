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

		return LocalFileHeader {
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

const File = struct {
	local_file_header: LocalFileHeader,
	file_data: []u8,

	fn init(bs: *Bytes) File {
		const lfh = LocalFileHeader.init(bs);
		const file_data = bs.slice(lfh.compressed_size);

		return File {
			.local_file_header = lfh,
			.file_data = file_data,
		};
	}
};

const CentralDirectoryHeader = struct {
	version_made_by: u16,
	version_needed_to_extract: u16,
	general_purpose_bit_flag: u16,
	compression_method: u16,
	last_mod_file_time: u16,
	last_mod_file_date: u16,
	crc32: u32,
	compressed_size: u32,
	uncompressed_size: u32,
	disk_number_start: u16,
	internal_file_attributes: u16,
	external_file_attributes: u32,
	relative_offset_of_local_header: u32,
	file_name: []u8,
	extra_field: []u8,
	file_comment: []u8,

	fn signature() u32 {
		return 0x02014b50;
	}

	fn init(bs: *Bytes) CentralDirectoryHeader {
		if (CentralDirectoryHeader.signature() != bs.u32le()) {
			std.debug.panic("invalid signature", .{});
		}

		const version_made_by = bs.u16le();
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
		const file_comment_length = bs.u16le();
		const disk_number_start = bs.u16le();
		const internal_file_attributes = bs.u16le();
		const external_file_attributes = bs.u32le();
		const relative_offset_of_local_header = bs.u32le();
		const file_name = bs.slice(file_name_length);
		const extra_field = bs.slice(extra_field_length);
		const file_comment = bs.slice(file_comment_length);

		return CentralDirectoryHeader {
			.version_made_by = version_made_by,
			.version_needed_to_extract = version_needed_to_extract,
			.general_purpose_bit_flag = general_purpose_bit_flag,
			.compression_method = compression_method,
			.last_mod_file_time = last_mod_file_time,
			.last_mod_file_date = last_mod_file_date,
			.crc32 = crc32,
			.compressed_size = compressed_size,
			.uncompressed_size = uncompressed_size,
			.disk_number_start = disk_number_start,
			.internal_file_attributes = internal_file_attributes,
			.external_file_attributes = external_file_attributes,
			.relative_offset_of_local_header = relative_offset_of_local_header,
			.file_name = file_name,
			.extra_field = extra_field,
			.file_comment = file_comment,
		};
	}
};

const EndOfCentralDirectoryRecord = struct {
	number_of_this_disk: u32,
	number_of_the_disk_with_the_start_of_the_central_directory: u16,
	total_number_of_entries_in_the_central_directory_on_this_disk: u16,
	total_number_of_entries_in_the_central_directory: u16,
	size_of_the_central_directory: u16,
	offset_of_start_of_central_directory_with_respect_to_the_starting_disk_number: u32,
	zip_file_comment: []u8,

	fn signature() u32 {
		return 0x06054b50;
	}

	fn init(bs: *Bytes) EndOfCentralDirectoryRecord {
		if (EndOfCentralDirectoryRecord.signature() != bs.u32le()) {
			std.debug.panic("invalid signature", .{});
		}

		const number_of_this_disk = bs.u32le();
		const number_of_the_disk_with_the_start_of_the_central_directory = bs.u16le();
		const total_number_of_entries_in_the_central_directory_on_this_disk = bs.u16le();
		const total_number_of_entries_in_the_central_directory = bs.u16le();
		const size_of_the_central_directory = bs.u16le();
		const offset_of_start_of_central_directory_with_respect_to_the_starting_disk_number = bs.u32le();
		const zip_file_comment_length = bs.u16le();
		const zip_file_comment = bs.slice(zip_file_comment_length);

		return EndOfCentralDirectoryRecord {
			.number_of_this_disk = number_of_this_disk,
			.number_of_the_disk_with_the_start_of_the_central_directory = number_of_the_disk_with_the_start_of_the_central_directory,
			.total_number_of_entries_in_the_central_directory_on_this_disk = total_number_of_entries_in_the_central_directory_on_this_disk,
			.total_number_of_entries_in_the_central_directory = total_number_of_entries_in_the_central_directory,
			.size_of_the_central_directory = size_of_the_central_directory,
			.offset_of_start_of_central_directory_with_respect_to_the_starting_disk_number = offset_of_start_of_central_directory_with_respect_to_the_starting_disk_number,
			.zip_file_comment = zip_file_comment,
		};
	}
};

const Zip = struct {
	files: [1]File,
	central_directory_headers: [1]CentralDirectoryHeader,
	end_of_central_directory_record: EndOfCentralDirectoryRecord,

	fn init(raw: []u8) Zip {
		const bs = Bytes {
			.bytes = raw,
			.index = 0,
		};

		const files = [1]File{ File.init(@constCast(&bs)) };
		const central_directory_headers = [1]CentralDirectoryHeader{ CentralDirectoryHeader.init(@constCast(&bs)) };
		const end_of_central_directory_record = EndOfCentralDirectoryRecord.init(@constCast(&bs));

		return Zip {
			.files = files,
			.central_directory_headers = central_directory_headers,
			.end_of_central_directory_record = end_of_central_directory_record,
		};
	}
};

const DeflateBlock = struct {
	last_block_bit: u1,
	block_type: u2,
};

pub fn main() !void {
	var file = try std.fs.cwd().openFile("test.zip", .{});
	defer file.close();

	var buf: [1024]u8 = undefined;
	const n = try file.readAll(&buf);

	const zip = Zip.init(buf[0..n]);
	std.debug.print("{any}\n", .{zip.files[0].local_file_header});

	const zf = zip.files[0];

	if (zf.local_file_header.compression_method == 8) {
		std.debug.print("The file is Deflated\n", .{});
	}
}
