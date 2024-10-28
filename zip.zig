const LocalFileHeader = struct {
	signature: u32,
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
};

const DataDescriptor = struct {
	crc32: u32,
	compressedSize: u32,
	uncompressedSize: u32,
};

const Data = struct {
	lfh: LocalFileHeader,
	eh: ?EncryptionHeader,
	fd: []u8,
	dd: DataDescriptor,
};


/////////////////////////////////////////////


const ArchiveExtraDataRecord = struct {
	signature: u32,
	extraFieldLen: u32,
	extraFieldData: []u8,
};

const CentralDirectoryFileHeader = struct {
	signature: u32,
	version: u16,
	versNeeded: u16,
	flags: u16,
	compression: u16,
	modTime: u16,
	modDate: u16,
	crc32: u32,
	compressedSize: u32,
	uncompressedSize: u32,
	fileNameLen: u16,
	extraFieldLen: u16,
	fileCommLen: u16,
	diskStart: u16,
	internalAttr: u16,
	externalAttr: u32,
	offsetOfLocalHeader: u32,
	fileName: []u8,
	extraField: []u8,
	fileComment: []u8,
};

const DigitalSignature = struct{
	headerSignature: u32,
	sizeOfData: u16,
	signatureData: []u8,
};

const Zip64EndOfCentralDirecotryRecord = struct {
	signature: u32,
	size: u64,
	versionMadeBy, u16,
	versionMadeToExtract: u16,
	diskNumber: u32,
	disckNumberStartCentralDir: u32,
	totalEntriesDisk: u64,
	totalEntriesCentralDirectory: u64,
	sizeCentralDir: u64,
	offsetCentralDirWithStartingDiskNumber: u64,
	extensibleDataSector: []u8,
};

const Zip64EndOfCentralDirectoryLocator = struct{
	signature: u32,
	numDiskWithStartOfZip64EndOfCentralDir: u32,
	endOfDirectoryRecord
};

const Zip = struct {
	data: []Data,
	adh: ArchiveDecriptionHeader,
	aedr: ArchiveExtraDataRecord,
	cdh: []CentralDirectoryHeader,
	zip64EndOfCentralDirectoryRecord: Zip64EndOfCentralDirecotryRecord,
	zip64EndOfCentralDirectoryLocator:
	endOfCentralDirectoryRecord:
};
