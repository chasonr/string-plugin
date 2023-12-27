// Convert a clang::StringLiteral to a UTF-8 representation as a std::string

#define __STDC_LIMIT_MACROS 1
#define __STDC_CONSTANT_MACROS 1
#include <string>
#include <cstring>
#include <limits>
#include <clang/Basic/Version.h>
#include <clang/AST/Expr.h>
#include "utf8string.h"

namespace {

template <typename chtype, bool is_utf32>
std::string stringFromBytes(clang::StringLiteral const *str)
{
    std::string b = str->getBytes().str();
    std::string out_str;

    std::size_t i = 0;
    while (i < b.size()) {
        chtype in_ch;
        char32_t utf32;
        char utf8[4];
        unsigned len;

        std::memcpy(&in_ch, b.c_str()+i, sizeof(chtype));
        i += sizeof(chtype);
        utf32 = in_ch;
        if (!is_utf32) {
            // Convert from UTF-16
            if ((in_ch & ~(chtype)0x3FF) == 0xD800 && i < b.size()) {
                // Possible surrogate pair
                chtype in_ch2;

                std::memcpy(&in_ch2, b.c_str()+i, sizeof(chtype));
                if ((in_ch2 & ~(chtype)0x3FF) == 0xDC00) {
                    i += sizeof(chtype);
                    utf32 = 0x10000 + ((in_ch & 0x3FF) << 10) + (in_ch2 & 0x3FF);
                }
            }
        }
        // Reject invalid code points
        if (utf32 > 0x10FFFF || (utf32 & ~(char32_t)0x7FF) == 0xD800) {
            utf32 = 0xFFFD;
        }
        // Convert to UTF-8
        if (utf32 < 0x80) {
            utf8[0] = (char)utf32;
            len = 1;
        } else if (utf32 < 0x800) {
            utf8[0] = (char)(0xC0 + (utf32 >> 6));
            utf8[1] = (char)(0x80 + (utf32 & 0x3F));
            len = 2;
        } else if (utf32 < 0x10000) {
            utf8[0] = (char)(0xC0 + (utf32 >> 12));
            utf8[1] = (char)(0x80 + ((utf32 >> 6) & 0x3F));
            utf8[2] = (char)(0x80 + (utf32 & 0x3F));
            len = 3;
        } else {
            utf8[0] = (char)(0xC0 + (utf32 >> 18));
            utf8[1] = (char)(0x80 + ((utf32 >> 12) & 0x3F));
            utf8[2] = (char)(0x80 + ((utf32 >> 6) & 0x3F));
            utf8[3] = (char)(0x80 + (utf32 & 0x3F));
            len = 4;
        }
        out_str += std::string(utf8, len);
    }

    return out_str;
}

}

std::string stringToUTF8(clang::StringLiteral const *str)
{
    switch (str->getCharByteWidth()) {
    case 1:
        return str->getString().str();

    case 2:
        return stringFromBytes<char16_t, false>(str);

    case 4:
        return stringFromBytes<char32_t, true>(str);

    default:
        return "";
    }
}
