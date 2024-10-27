const std = @import("std");

const Script = enum {
    Adlam,
    Ahom,
    Anatolian_Hieroglyphs,
    Arabic,
    Armenian,
    Avestan,
    Balinese,
    Bamum,
    Bassa_Vah,
    Batak,
    Bengali,
    Bhaiksuki,
    Bopomofo,
    Brahmi,
    Braille,
    Buginese,
    Buhid,
    Canadian_Aboriginal,
    Carian,
    Caucasian_Albanian,
    Chakma,
    Cham,
    Cherokee,
    Chorasmian,
    Common,
    Coptic,
    Cuneiform,
    Cypriot,
    Cypro_Minoan,
    Cyrillic,
    Deseret,
    Devanagari,
    Dives_Akuru,
    Dogra,
    Duployan,
    Egyptian_Hieroglyphs,
    Elbasan,
    Elymaic,
    Ethiopic,
    Garay,
    Georgian,
    Glagolitic,
    Gothic,
    Grantha,
    Greek,
    Gujarati,
    Gunjala_Gondi,
    Gurmukhi,
    Gurung_Khema,
    Han,
    Hangul,
    Hanifi_Rohingya,
    Hanunoo,
    Hatran,
    Hebrew,
    Hiragana,
    Imperial_Aramaic,
    Inherited,
    Inscriptional_Pahlavi,
    Inscriptional_Parthian,
    Javanese,
    Kaithi,
    Kannada,
    Katakana,
    Kawi,
    Kayah_Li,
    Kharoshthi,
    Khitan_Small_Script,
    Khmer,
    Khojki,
    Khudawadi,
    Kirat_Rai,
    Lao,
    Latin,
    Lepcha,
    Limbu,
    Linear_A,
    Linear_B,
    Lisu,
    Lycian,
    Lydian,
    Mahajani,
    Makasar,
    Malayalam,
    Mandaic,
    Manichaean,
    Marchen,
    Masaram_Gondi,
    Medefaidrin,
    Meetei_Mayek,
    Mende_Kikakui,
    Meroitic_Cursive,
    Meroitic_Hieroglyphs,
    Miao,
    Modi,
    Mongolian,
    Mro,
    Multani,
    Myanmar,
    Nabataean,
    Nag_Mundari,
    Nandinagari,
    Newa,
    New_Tai_Lue,
    Nko,
    Nushu,
    Nyiakeng_Puachue_Hmong,
    Ogham,
    Ol_Chiki,
    Old_Hungarian,
    Old_Italic,
    Old_North_Arabian,
    Old_Permic,
    Old_Persian,
    Old_Sogdian,
    Old_South_Arabian,
    Old_Turkic,
    Old_Uyghur,
    Ol_Onal,
    Oriya,
    Osage,
    Osmanya,
    Pahawh_Hmong,
    Palmyrene,
    Pau_Cin_Hau,
    Phags_Pa,
    Phoenician,
    Psalter_Pahlavi,
    Rejang,
    Runic,
    Samaritan,
    Saurashtra,
    Sharada,
    Shavian,
    Siddham,
    SignWriting,
    Sinhala,
    Sogdian,
    Sora_Sompeng,
    Soyombo,
    Sundanese,
    Sunuwar,
    Syloti_Nagri,
    Syriac,
    Tagalog,
    Tagbanwa,
    Tai_Le,
    Tai_Tham,
    Tai_Viet,
    Takri,
    Tamil,
    Tangsa,
    Tangut,
    Telugu,
    Thaana,
    Thai,
    Tibetan,
    Tifinagh,
    Tirhuta,
    Todhri,
    Toto,
    Tulu_Tigalari,
    Ugaritic,
    Vai,
    Vithkuqi,
    Wancho,
    Warang_Citi,
    Yezidi,
    Yi,
    Zanabazar_Square,
};

const scriptStr = [_][]const u8{
    "Adlam",
    "Ahom",
    "Anatolian_Hieroglyphs",
    "Arabic",
    "Armenian",
    "Avestan",
    "Balinese",
    "Bamum",
    "Bassa_Vah",
    "Batak",
    "Bengali",
    "Bhaiksuki",
    "Bopomofo",
    "Brahmi",
    "Braille",
    "Buginese",
    "Buhid",
    "Canadian_Aboriginal",
    "Carian",
    "Caucasian_Albanian",
    "Chakma",
    "Cham",
    "Cherokee",
    "Chorasmian",
    "Common",
    "Coptic",
    "Cuneiform",
    "Cypriot",
    "Cypro_Minoan",
    "Cyrillic",
    "Deseret",
    "Devanagari",
    "Dives_Akuru",
    "Dogra",
    "Duployan",
    "Egyptian_Hieroglyphs",
    "Elbasan",
    "Elymaic",
    "Ethiopic",
    "Garay",
    "Georgian",
    "Glagolitic",
    "Gothic",
    "Grantha",
    "Greek",
    "Gujarati",
    "Gunjala_Gondi",
    "Gurmukhi",
    "Gurung_Khema",
    "Han",
    "Hangul",
    "Hanifi_Rohingya",
    "Hanunoo",
    "Hatran",
    "Hebrew",
    "Hiragana",
    "Imperial_Aramaic",
    "Inherited",
    "Inscriptional_Pahlavi",
    "Inscriptional_Parthian",
    "Javanese",
    "Kaithi",
    "Kannada",
    "Katakana",
    "Kawi",
    "Kayah_Li",
    "Kharoshthi",
    "Khitan_Small_Script",
    "Khmer",
    "Khojki",
    "Khudawadi",
    "Kirat_Rai",
    "Lao",
    "Latin",
    "Lepcha",
    "Limbu",
    "Linear_A",
    "Linear_B",
    "Lisu",
    "Lycian",
    "Lydian",
    "Mahajani",
    "Makasar",
    "Malayalam",
    "Mandaic",
    "Manichaean",
    "Marchen",
    "Masaram_Gondi",
    "Medefaidrin",
    "Meetei_Mayek",
    "Mende_Kikakui",
    "Meroitic_Cursive",
    "Meroitic_Hieroglyphs",
    "Miao",
    "Modi",
    "Mongolian",
    "Mro",
    "Multani",
    "Myanmar",
    "Nabataean",
    "Nag_Mundari",
    "Nandinagari",
    "Newa",
    "New_Tai_Lue",
    "Nko",
    "Nushu",
    "Nyiakeng_Puachue_Hmong",
    "Ogham",
    "Ol_Chiki",
    "Old_Hungarian",
    "Old_Italic",
    "Old_North_Arabian",
    "Old_Permic",
    "Old_Persian",
    "Old_Sogdian",
    "Old_South_Arabian",
    "Old_Turkic",
    "Old_Uyghur",
    "Ol_Onal",
    "Oriya",
    "Osage",
    "Osmanya",
    "Pahawh_Hmong",
    "Palmyrene",
    "Pau_Cin_Hau",
    "Phags_Pa",
    "Phoenician",
    "Psalter_Pahlavi",
    "Rejang",
    "Runic",
    "Samaritan",
    "Saurashtra",
    "Sharada",
    "Shavian",
    "Siddham",
    "SignWriting",
    "Sinhala",
    "Sogdian",
    "Sora_Sompeng",
    "Soyombo",
    "Sundanese",
    "Sunuwar",
    "Syloti_Nagri",
    "Syriac",
    "Tagalog",
    "Tagbanwa",
    "Tai_Le",
    "Tai_Tham",
    "Tai_Viet",
    "Takri",
    "Tamil",
    "Tangsa",
    "Tangut",
    "Telugu",
    "Thaana",
    "Thai",
    "Tibetan",
    "Tifinagh",
    "Tirhuta",
    "Todhri",
    "Toto",
    "Tulu_Tigalari",
    "Ugaritic",
    "Vai",
    "Vithkuqi",
    "Wancho",
    "Warang_Citi",
    "Yezidi",
    "Yi",
    "Zanabazar_Square",
};

const scriptArr = [_]Script{
    Script.Adlam,
    Script.Ahom,
    Script.Anatolian_Hieroglyphs,
    Script.Arabic,
    Script.Armenian,
    Script.Avestan,
    Script.Balinese,
    Script.Bamum,
    Script.Bassa_Vah,
    Script.Batak,
    Script.Bengali,
    Script.Bhaiksuki,
    Script.Bopomofo,
    Script.Brahmi,
    Script.Braille,
    Script.Buginese,
    Script.Buhid,
    Script.Canadian_Aboriginal,
    Script.Carian,
    Script.Caucasian_Albanian,
    Script.Chakma,
    Script.Cham,
    Script.Cherokee,
    Script.Chorasmian,
    Script.Common,
    Script.Coptic,
    Script.Cuneiform,
    Script.Cypriot,
    Script.Cypro_Minoan,
    Script.Cyrillic,
    Script.Deseret,
    Script.Devanagari,
    Script.Dives_Akuru,
    Script.Dogra,
    Script.Duployan,
    Script.Egyptian_Hieroglyphs,
    Script.Elbasan,
    Script.Elymaic,
    Script.Ethiopic,
    Script.Garay,
    Script.Georgian,
    Script.Glagolitic,
    Script.Gothic,
    Script.Grantha,
    Script.Greek,
    Script.Gujarati,
    Script.Gunjala_Gondi,
    Script.Gurmukhi,
    Script.Gurung_Khema,
    Script.Han,
    Script.Hangul,
    Script.Hanifi_Rohingya,
    Script.Hanunoo,
    Script.Hatran,
    Script.Hebrew,
    Script.Hiragana,
    Script.Imperial_Aramaic,
    Script.Inherited,
    Script.Inscriptional_Pahlavi,
    Script.Inscriptional_Parthian,
    Script.Javanese,
    Script.Kaithi,
    Script.Kannada,
    Script.Katakana,
    Script.Kawi,
    Script.Kayah_Li,
    Script.Kharoshthi,
    Script.Khitan_Small_Script,
    Script.Khmer,
    Script.Khojki,
    Script.Khudawadi,
    Script.Kirat_Rai,
    Script.Lao,
    Script.Latin,
    Script.Lepcha,
    Script.Limbu,
    Script.Linear_A,
    Script.Linear_B,
    Script.Lisu,
    Script.Lycian,
    Script.Lydian,
    Script.Mahajani,
    Script.Makasar,
    Script.Malayalam,
    Script.Mandaic,
    Script.Manichaean,
    Script.Marchen,
    Script.Masaram_Gondi,
    Script.Medefaidrin,
    Script.Meetei_Mayek,
    Script.Mende_Kikakui,
    Script.Meroitic_Cursive,
    Script.Meroitic_Hieroglyphs,
    Script.Miao,
    Script.Modi,
    Script.Mongolian,
    Script.Mro,
    Script.Multani,
    Script.Myanmar,
    Script.Nabataean,
    Script.Nag_Mundari,
    Script.Nandinagari,
    Script.Newa,
    Script.New_Tai_Lue,
    Script.Nko,
    Script.Nushu,
    Script.Nyiakeng_Puachue_Hmong,
    Script.Ogham,
    Script.Ol_Chiki,
    Script.Old_Hungarian,
    Script.Old_Italic,
    Script.Old_North_Arabian,
    Script.Old_Permic,
    Script.Old_Persian,
    Script.Old_Sogdian,
    Script.Old_South_Arabian,
    Script.Old_Turkic,
    Script.Old_Uyghur,
    Script.Ol_Onal,
    Script.Oriya,
    Script.Osage,
    Script.Osmanya,
    Script.Pahawh_Hmong,
    Script.Palmyrene,
    Script.Pau_Cin_Hau,
    Script.Phags_Pa,
    Script.Phoenician,
    Script.Psalter_Pahlavi,
    Script.Rejang,
    Script.Runic,
    Script.Samaritan,
    Script.Saurashtra,
    Script.Sharada,
    Script.Shavian,
    Script.Siddham,
    Script.SignWriting,
    Script.Sinhala,
    Script.Sogdian,
    Script.Sora_Sompeng,
    Script.Soyombo,
    Script.Sundanese,
    Script.Sunuwar,
    Script.Syloti_Nagri,
    Script.Syriac,
    Script.Tagalog,
    Script.Tagbanwa,
    Script.Tai_Le,
    Script.Tai_Tham,
    Script.Tai_Viet,
    Script.Takri,
    Script.Tamil,
    Script.Tangsa,
    Script.Tangut,
    Script.Telugu,
    Script.Thaana,
    Script.Thai,
    Script.Tibetan,
    Script.Tifinagh,
    Script.Tirhuta,
    Script.Todhri,
    Script.Toto,
    Script.Tulu_Tigalari,
    Script.Ugaritic,
    Script.Vai,
    Script.Vithkuqi,
    Script.Wancho,
    Script.Warang_Citi,
    Script.Yezidi,
    Script.Yi,
    Script.Zanabazar_Square,
};

const GeneralCategory = enum {
    Uppercase_Letter,
    Lowercase_Letter,
    Titlecase_Letter,
    Cased_Letter,
    Modifier_Letter,
    Other_Letter,
    Letter,
    Nonspacing_Mark,
    Spacing_Mark,
    Enclosing_Mark,
    Mark,
    Decimal_Number,
    Letter_Number,
    Other_Number,
    Number,
    Connector_Punctuation,
    Dash_Punctuation,
    Open_Punctuation,
    Close_Punctuation,
    Initial_Punctuation,
    Final_Punctuation,
    Other_Punctuation,
    Punctuation,
    Math_Symbol,
    Currency_Symbol,
    Modifier_Symbol,
    Other_Symbol,
    Symbol,
    Space_Separator,
    Line_Separator,
    Paragraph_Separator,
    Separator,
    Control,
    Format,
    Surrogate,
    Private_Use,
    Unassigned,
    Other,
};

const generalCategoryStr = [_][]const u8{
    "Lu",
    "Ll",
    "Lt",
    "LC",
    "L&", // LC alias
    "Lm",
    "Lo",
    "L",
    "Mn",
    "Mc",
    "Me",
    "M",
    "Nd",
    "Nl",
    "No",
    "N",
    "Pc",
    "Pd",
    "Ps",
    "Pe",
    "Pi",
    "Pf",
    "Po",
    "P",
    "Sm",
    "Sc",
    "Sk",
    "So",
    "S",
    "Zs",
    "Zl",
    "Zp",
    "Z",
    "Cc",
    "Cf",
    "Cs",
    "Co",
    "Cn",
    "C",
};

const generalCategoryArr = [_]GeneralCategory{
    GeneralCategory.Uppercase_Letter,
    GeneralCategory.Lowercase_Letter,
    GeneralCategory.Titlecase_Letter,
    GeneralCategory.Cased_Letter,
    GeneralCategory.Cased_Letter, // L&
    GeneralCategory.Modifier_Letter,
    GeneralCategory.Other_Letter,
    GeneralCategory.Letter,
    GeneralCategory.Nonspacing_Mark,
    GeneralCategory.Spacing_Mark,
    GeneralCategory.Enclosing_Mark,
    GeneralCategory.Mark,
    GeneralCategory.Decimal_Number,
    GeneralCategory.Letter_Number,
    GeneralCategory.Other_Number,
    GeneralCategory.Number,
    GeneralCategory.Connector_Punctuation,
    GeneralCategory.Dash_Punctuation,
    GeneralCategory.Open_Punctuation,
    GeneralCategory.Close_Punctuation,
    GeneralCategory.Initial_Punctuation,
    GeneralCategory.Final_Punctuation,
    GeneralCategory.Other_Punctuation,
    GeneralCategory.Punctuation,
    GeneralCategory.Math_Symbol,
    GeneralCategory.Currency_Symbol,
    GeneralCategory.Modifier_Symbol,
    GeneralCategory.Other_Symbol,
    GeneralCategory.Symbol,
    GeneralCategory.Space_Separator,
    GeneralCategory.Line_Separator,
    GeneralCategory.Paragraph_Separator,
    GeneralCategory.Separator,
    GeneralCategory.Control,
    GeneralCategory.Format,
    GeneralCategory.Surrogate,
    GeneralCategory.Private_Use,
    GeneralCategory.Unassigned,
    GeneralCategory.Other,
};

const ScriptEntry = struct {
    rangeStart: u21,
    rangeEnd: ?u21,
    script: Script,
    generalCategory: GeneralCategory,
};

fn parseScript(s: []u8) Script {
	var len: usize = 0;
	while (s[len] != ' ') {
		len += 1;
	}
	const candidate = s[0..len];

	for (0.., scriptStr) |i, fmt| {
		if (std.mem.eql(u8, candidate, fmt)) {
			return scriptArr[i];
		}
	}
	std.debug.panic("unknown script '{s}'", .{candidate});
}

fn parseGeneralCategory(s: []u8) GeneralCategory {
	var len: usize = 0;
	while (s[len] != ' ') {
		len += 1;
	}
	const candidate = s[0..len];

	for (0.., generalCategoryStr) |i, fmt| {
		if (std.mem.eql(u8, candidate, fmt)) {
			return generalCategoryArr[i];
		}
	}
	std.debug.panic("unknown general category '{s}'", .{candidate});
}

fn parseLine(line: []u8) !ScriptEntry {
	var rangeStart: u21 = 0;
	var rangeStartFound = false;

	var rangeEnd: ?u21 = null;
	var rangeEndIndex: usize = 0;

	var script: Script = undefined;
	var scriptFound = false;

	var generalCategory: GeneralCategory = undefined;
	var generalCategoryFound = false;

	for (0.., line) |i, c| {
		if (!rangeStartFound and c == ' ') {
			rangeStart = try std.fmt.parseInt(u21, line[0..i], 16);
			rangeStartFound = true;
		} else if (!rangeStartFound and c == '.') {
			rangeStart = try std.fmt.parseInt(u21, line[0..i], 16);
			rangeStartFound = true;
			rangeEndIndex = i+2;
		} else if (rangeEndIndex != 0 and c == ' ') {
			rangeEnd = try std.fmt.parseInt(u21, line[rangeEndIndex..i], 16);
			rangeEndIndex = 0;
		} else if (!scriptFound and c == ';') {
			script = parseScript(line[i+2..]);
			scriptFound = true;
		} else if (!generalCategoryFound and c == '#') {
			generalCategory = parseGeneralCategory(line[i+2..]);
			generalCategoryFound = true;
			break;
		}
	}

	return ScriptEntry{
		.rangeStart = rangeStart,
		.rangeEnd = rangeEnd,
		.script = script,
		.generalCategory = generalCategory,
	};
}

fn matchScript(scripts: std.ArrayList(ScriptEntry), codepoint: u21) ScriptEntry {
	for (scripts.items) |script| {
		if (script.rangeEnd) |rangeEnd| {
			if (script.rangeStart <= codepoint and codepoint <= rangeEnd) {
				return script;
			}
		} else if (script.rangeStart == codepoint) {
			return script;
		}
	}
	std.debug.panic("failed to match codepoint '{x}' to script", .{codepoint});
}

fn isPostcore(codepoint: u21, script: ScriptEntry) bool {
	const cat = script.generalCategory;

	const graphemeExtend = cat == GeneralCategory.Enclosing_Mark or cat == GeneralCategory.Nonspacing_Mark; // + Other_Grapheme_Extend
	const emojiModifier = false; // Present in a different file?
	const extend = graphemeExtend or emojiModifier;

	const zwj = codepoint == 0x200D;

	const spacingMark = cat == GeneralCategory.Spacing_Mark or codepoint == 0x0E33 or codepoint == 0x0EB3; // + Grapheme_Cluster_Break ≠ Extend + explicit codepoint exceptions

	return extend or zwj or spacingMark;
}

pub fn main() !void {
	var file = try std.fs.cwd().openFile("Scripts.txt", .{});
	defer file.close();

	var buf: [256]u8 = undefined;

	var gpa = std.heap.GeneralPurposeAllocator(.{}){};
	const allocator = gpa.allocator();
	var scripts = std.ArrayList(ScriptEntry).init(allocator);
	defer scripts.deinit();

	while (file.reader().readUntilDelimiterOrEof(&buf, '\n')) |maybe_line| {
		if (maybe_line) |line| {
			if (line.len == 0 or line[0] == '#') {
				continue;
			}
			const script = try parseLine(line);
			try scripts.append(script);
		} else {
			break; // EOF
		}
	} else |err| {
		std.debug.panic("failed to read file: {}", .{err});
	}

	const s = "aα👨‍👨‍👧‍👦👩‍👩‍👦‍👦👨‍🏭🫎👯‍♂️";
	var codepointIter = (try std.unicode.Utf8View.init(s)).iterator();
	var codepoints = std.ArrayList(u21).init(allocator);
	while (codepointIter.nextCodepoint()) |codepoint| {
		std.debug.print("{x}\n", .{codepoint});
		try codepoints.append(codepoint);
	}

	std.debug.print("==========\n", .{});

	var i: usize = 0;
	while (i < codepoints.items.len) {
		const codepoint = codepoints.items[i];
		// std.debug.print("{x} {}\n", .{codepoint, script});

		if (i+1 >= codepoints.items.len) {
			std.debug.print("{x}\n", .{codepoint});
			break;
		}
		const peek = codepoints.items[i+1];
		const peekScript = matchScript(scripts, peek);
		if (isPostcore(peek, peekScript)) {
			std.debug.print("{x} ", .{codepoint});
			i += 1;
		} else {
			std.debug.print("{x}\n", .{codepoint});
		}

		i += 1;
	}
}
