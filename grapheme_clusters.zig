const std = @import("std");

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

const ScriptEntry = struct {
    rangeStart: u21,
    rangeEnd: u21,
    script: Script,
    generalCategory: GeneralCategory,
};

pub fn main() !void {
    // https://www.unicode.org/Public/16.0.0/ucd/Scripts.txt
    var file = try std.fs.cwd().openFile("Scripts.txt", .{});
    defer file.close();

    var buf: [256]u8 = undefined;
    @memset(&buf, 0);

    var entries = std.ArrayList(ScriptEntry).init(std.testing.allocator);
    defer entries.deinit();

    while (file.reader().readUntilDelimiterOrEof(&buf, '\n')) |maybe_line| {
        if (maybe_line) |line| {
            if (line.len == 0 or line[0] == '#') {
                continue;
            }
            // std.debug.print("{s}\n", .{line});
            var i: usize = 0;
            while (i < line.len) {
                if (line[i] == '.' or line[i] == ' ') {
                    break;
                }
                i += 1;
            }

            const rangeStart = try std.fmt.parseInt(u21, buf[0..i], 16);

            var rangeEnd: u21 = 0;
            var j = i + 1;
            if (line[j] == '.') {
                while (j < line.len) {
                    if (line[j] == ' ') {
                        break;
                    }
                    j += 1;
                }
                // std.debug.print("{s}\n", .{line[i + 2 .. j]});
                rangeEnd = try std.fmt.parseInt(u21, line[i + 2 .. j], 16);
            }

            std.debug.print("{x}..{x}\n", .{ rangeStart, rangeEnd });

            const entry = ScriptEntry{
                .rangeStart = rangeStart,
                .rangeEnd = rangeEnd,
            };
        } else {
            break; // EOF
        }
    } else |err| {
        std.debug.panic("failed to read file: {}", .{err});
    }
}
