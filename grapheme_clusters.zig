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

const ScriptEntry = struct {
    rangeStart: u21,
    rangeEnd: u21,
    script: Script,
    generalCategory: GeneralCategory,
};

fn parseScript(_: []u8) Script {}

fn parseGeneralCategory(s: [2]u8) GeneralCategory {
    return switch (s[0]) {
        'L' => switch (s[1]) {
            'u' => GeneralCategory.Uppercase_Letter,
            'l' => GeneralCategory.Lowercase_Letter,
            't' => GeneralCategory.Titlecase_Letter,
            'C' => GeneralCategory.Cased_Letter,
            'm' => GeneralCategory.Modifier_Letter,
            'o' => GeneralCategory.Other_Letter,
            ' ' => GeneralCategory.Letter,
            else => std.debug.panic("unxpected general category character {s}", .{s}),
        },
        'M' => switch (s[1]) {
            'n' => GeneralCategory.Nonspacing_Mark,
            'c' => GeneralCategory.Spacing_Mark,
            'e' => GeneralCategory.Enclosing_Mark,
            ' ' => GeneralCategory.Mark,
            else => std.debug.panic("unxpected general category character {s}", .{s}),
        },
        'n' => switch (s[1]) {
            'd' => GeneralCategory.Decimal_Number,
            'l' => GeneralCategory.Letter_Number,
            'o' => GeneralCategory.Other_Number,
            ' ' => GeneralCategory.Number,
            else => std.debug.panic("unxpected general category character {s}", .{s}),
        },
        'n' => switch (s[1]) {
            'c' => GeneralCategory.Connector_Punctuation,
            'd' => GeneralCategory.Dash_Punctuation,
            's' => GeneralCategory.Open_Punctuation,
            'e' => GeneralCategory.Close_Punctuation,
            'i' => GeneralCategory.Initial_Punctuation,
            'f' => GeneralCategory.Final_Punctuation,
            'o' => GeneralCategory.Other_Punctuation,
            ' ' => GeneralCategory.Punctuation,
            else => std.debug.panic("unxpected general category character {s}", .{s}),
        },
        'S' => switch (s[1]) {
            'm' => GeneralCategory.Math_Symbol,
            'c' => GeneralCategory.Currency_Symbol,
            'k' => GeneralCategory.Modifier_Symbol,
            'o' => GeneralCategory.Other_Symbol,
            ' ' => GeneralCategory.Symbol,
            else => std.debug.panic("unxpected general category character {s}", .{s}),
        },
    };
}

fn parseLine(line: []u8) ScriptEntry {
    var rangeStart: u21 = undefined;
    var rangeEnd: u21 = undefined;
    var rangeEndIndex: usize = undefined;
    var script: Script = undefined;
    var generalCategory: GeneralCategory = undefined;

    for (0.., line) |i, c| {
        if (rangeStart == undefined and c == ' ') {
            rangeStart = std.fmt.parseInt(u21, line[0..i], 16);
        } else if (rangeStart == undefined and c == '.') {
            rangeStart = std.fmt.parseInt(u21, line[0..i], 16);
            rangeEndIndex = i + 1;
        } else if (rangeEndIndex != undefined and c == ' ') {
            rangeEnd = std.fmt.parseInt(u21, line[rangeEndIndex..i], 16);
            rangeEndIndex = undefined;
        } else if (script == undefined and c == ';') {
            script = parseScript(line[i + 2 ..]);
        } else if (generalCategory == undefined and c == '#') {
            generalCategory = parseGeneralCategory(line[i + 2 .. i + 4]);
            break;
        } else {
            std.debug.panic("unexpected character '{}'", .{c});
        }
    }

    return ScriptEntry{
        .rangeStart = rangeStart,
        .rangeEnd = rangeEnd,
        .script = script,
        .generalCategory = generalCategory,
    };
}

pub fn main() void {
    // https://www.unicode.org/Public/16.0.0/ucd/Scripts.txt
    var file = try std.fs.cwd().openFile("Scripts.txt", .{});
    defer file.close();

    var buf: [256]u8 = undefined;
    while (file.reader().readUntilDelimiterOrEof(&buf, '\n')) |maybe_line| {
        if (maybe_line) |line| {
            if (line.len == 0 or line[0] == '#') {
                continue;
            }
            std.debug.print("{}\n", .{parseLine(line)});
        } else {
            break; // EOF
        }
    } else |err| {
        std.debug.panic("failed to read file: {}", .{err});
    }
}
