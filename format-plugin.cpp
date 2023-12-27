// Use with:
// -Xclang -load -Xclang path/to/string-plugin.so -Xclang -add-plugin -Xclang format-plugin
// and zero or more of
// -Xclang -plugin-arg-format-plugin -Xclang -list=functions.txt

#include <algorithm>
#include <fstream>
#include <regex>
#include <string>
#include <vector>
#include <cstdlib>
#include <cstring>

#define __STDC_LIMIT_MACROS 1
#define __STDC_CONSTANT_MACROS 1
#include <clang/AST/AST.h>
#include <clang/AST/ASTConsumer.h>
#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/FrontendPluginRegistry.h>
#include <clang/Sema/Sema.h>
#include <clang/Sema/SemaDiagnostic.h>
#include <llvm/ADT/SmallBitVector.h>

#include "split.h"
#include "utf8string.h"
#include "compat.h"

namespace {

enum FormatStringType {
    printf,
    printf_s,
    scanf,
    scanf_s,
    NSString,
    strftime,
    strfmon,
    kprintf
};

static const char *
formatTypeName(FormatStringType type)
{
    switch (type) {
    case FormatStringType::printf:
        return "printf";

    case FormatStringType::printf_s:
        return "printf_s";

    case FormatStringType::scanf:
        return "scanf";

    case FormatStringType::scanf_s:
        return "scanf_s";

    case FormatStringType::NSString:
        return "NSString";

    case FormatStringType::strftime:
        return "strftime";

    case FormatStringType::strfmon:
        return "strfmon";

    case FormatStringType::kprintf:
        return "kprintf";

    default:
        return "unknown";
    }
}

struct FunctionDesc {
    std::string name;
    FormatStringType type;
    unsigned format_arg;
    unsigned var_arg;
};
FunctionDesc const default_function_list[] = {
    // Wide string formatting
    { "wprintf",      FormatStringType::printf,   1, 2 },
    { "fwprintf",     FormatStringType::printf,   2, 3 },
    { "swprintf",     FormatStringType::printf,   3, 4 },
    { "vwprintf",     FormatStringType::printf,   1, 0 },
    { "vfwprintf",    FormatStringType::printf,   2, 0 },
    { "vswprintf",    FormatStringType::printf,   3, 0 },
    { "wscanf",       FormatStringType::scanf,    1, 2 },
    { "fwscanf",      FormatStringType::scanf,    2, 3 },
    { "swscanf",      FormatStringType::scanf,    2, 3 },
    { "vwscanf",      FormatStringType::scanf,    1, 0 },
    { "vfwscanf",     FormatStringType::scanf,    2, 0 },
    { "vswscanf",     FormatStringType::scanf,    2, 0 },
    // Annex K
    { "printf_s",     FormatStringType::printf_s, 1, 2 },
    { "fprintf_s",    FormatStringType::printf_s, 2, 3 },
    { "sprintf_s",    FormatStringType::printf_s, 3, 4 },
    { "snprintf_s",   FormatStringType::printf_s, 3, 4 },
    { "vprintf_s",    FormatStringType::printf_s, 1, 0 },
    { "vfprintf_s",   FormatStringType::printf_s, 2, 0 },
    { "vsprintf_s",   FormatStringType::printf_s, 3, 0 },
    { "vsnprintf_s",  FormatStringType::printf_s, 3, 0 },
    { "wprintf_s",    FormatStringType::printf_s, 1, 2 },
    { "fwprintf_s",   FormatStringType::printf_s, 2, 3 },
    { "swprintf_s",   FormatStringType::printf_s, 3, 4 },
    { "snwprintf_s",  FormatStringType::printf_s, 3, 4 },
    { "vwprintf_s",   FormatStringType::printf_s, 1, 0 },
    { "vfwprintf_s",  FormatStringType::printf_s, 2, 0 },
    { "vswprintf_s",  FormatStringType::printf_s, 3, 0 },
    { "vsnwprintf_s", FormatStringType::printf_s, 3, 0 },
    { "scanf_s",      FormatStringType::scanf_s,  1, 2 },
    { "fscanf_s",     FormatStringType::scanf_s,  2, 3 },
    { "sscanf_s",     FormatStringType::scanf_s,  2, 3 },
    { "vscanf_s",     FormatStringType::scanf_s,  1, 0 },
    { "vfscanf_s",    FormatStringType::scanf_s,  2, 0 },
    { "vsscanf_s",    FormatStringType::scanf_s,  2, 0 },
    { "wscanf_s",     FormatStringType::scanf_s,  1, 2 },
    { "fwscanf_s",    FormatStringType::scanf_s,  2, 3 },
    { "swscanf_s",    FormatStringType::scanf_s,  2, 3 },
    { "vwscanf_s",    FormatStringType::scanf_s,  1, 0 },
    { "vfwscanf_s",   FormatStringType::scanf_s,  2, 0 },
    { "vswscanf_s",   FormatStringType::scanf_s,  2, 0 },
    // Wide string time
    { "wcsftime",     FormatStringType::strftime, 3, 0 },
};

struct GettextDesc {
    std::string name;
    unsigned format_arg;
};

static std::string
typeName(clang::BuiltinType::Kind kind)
{
    switch (kind) {
    case clang::BuiltinType::Char_S:
    case clang::BuiltinType::Char_U:
        return "char";

    case clang::BuiltinType::Double:
        return "double";

    case clang::BuiltinType::Float:
        return "float";

    case clang::BuiltinType::Int:
        return "int";

    case clang::BuiltinType::Long:
        return "long";

    case clang::BuiltinType::LongDouble:
        return "long double";

    case clang::BuiltinType::LongLong:
        return "long long";

    case clang::BuiltinType::SChar:
        return "signed char";

    case clang::BuiltinType::Short:
        return "short";

    case clang::BuiltinType::UInt:
        return "unsigned int";

    case clang::BuiltinType::ULong:
        return "unsigned long";

    case clang::BuiltinType::ULongLong:
        return "unsigned long long";

    case clang::BuiltinType::Void:
        return "void";

    default:
        return "unknown";
    }
}

static clang::BuiltinType::Kind
getSignedKind(clang::BuiltinType::Kind u_kind)
{
    if (clang::BuiltinType::Kind::Char_S <= u_kind
            && u_kind <= clang::BuiltinType::Kind::Int128) {
        return u_kind;
    }

    switch (u_kind) {
    case clang::BuiltinType::Kind::Char_U:
        return clang::BuiltinType::Kind::Char_S;

    case clang::BuiltinType::Kind::UChar:
        return clang::BuiltinType::Kind::SChar;

    case clang::BuiltinType::Kind::WChar_U:
        return clang::BuiltinType::Kind::WChar_S;

    case clang::BuiltinType::Kind::UShort:
        return clang::BuiltinType::Kind::Short;

    case clang::BuiltinType::Kind::UInt:
        return clang::BuiltinType::Kind::Int;

    case clang::BuiltinType::Kind::ULong:
        return clang::BuiltinType::Kind::Long;

    case clang::BuiltinType::Kind::ULongLong:
        return clang::BuiltinType::Kind::LongLong;

    case clang::BuiltinType::Kind::UInt128:
        return clang::BuiltinType::Kind::Int128;

    default:
        return clang::BuiltinType::Void;
    }
}

static clang::BuiltinType::Kind
getUnsignedKind(clang::BuiltinType::Kind s_kind)
{
    if (clang::BuiltinType::Kind::Char_U <= s_kind
            && s_kind <= clang::BuiltinType::Kind::UInt128) {
        return s_kind;
    }

    switch (s_kind) {
    case clang::BuiltinType::Kind::Char_S:
        return clang::BuiltinType::Kind::Char_U;

    case clang::BuiltinType::Kind::SChar:
        return clang::BuiltinType::Kind::UChar;

    case clang::BuiltinType::Kind::WChar_S:
        return clang::BuiltinType::Kind::WChar_U;

    case clang::BuiltinType::Kind::Short:
        return clang::BuiltinType::Kind::UShort;

    case clang::BuiltinType::Kind::Int:
        return clang::BuiltinType::Kind::UInt;

    case clang::BuiltinType::Kind::Long:
        return clang::BuiltinType::Kind::ULong;

    case clang::BuiltinType::Kind::LongLong:
        return clang::BuiltinType::Kind::ULongLong;

    case clang::BuiltinType::Kind::Int128:
        return clang::BuiltinType::Kind::UInt128;

    default:
        return clang::BuiltinType::Void;
    }
}

struct FormatString
{
    std::string string;
    clang::StringLiteral const *expr;
};

class FormatVisitor : public clang::RecursiveASTVisitor<FormatVisitor> {
public:
    explicit FormatVisitor(
            clang::CompilerInstance& inst,
            std::vector<FunctionDesc> const & functions,
            std::vector<GettextDesc> const & gettexts) :
    instance(inst),
    diagnostics(inst.getDiagnostics())
    {
        warn_format_nonliteral = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                "format string is not a string literal");

        warn_printf_ignored_flag = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                "flag '%0' is ignored when flag '%1' is present");

        warn_format_zero_positional_specifier = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
            "position arguments in format strings start counting at 1 (not 0)");

        warn_printf_asterisk_missing_arg = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
            "'%select{*|.*}0' specified field %select{width|precision}0 is missing a matching 'int' argument");

        warn_printf_asterisk_wrong_type = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
            "field %select{width|precision}0 should have type %1, but argument has type %2");

        warn_format_nonsensical_length = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
            "length modifier '%0' results in undefined behavior or no effect with '%1' conversion specifier");

        warn_format_invalid_conversion = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
            "invalid conversion specifier '%0'");

        warn_printf_insufficient_data_args = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
            "more '%%' conversions than data arguments");

        warn_printf_positional_arg_exceeds_data_args = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
            "data argument position '%0' exceeds the number of data arguments (%1)");

        warn_format_conversion_argument_type_mismatch = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
            "format specifies type %0 but the argument has %select{type|underlying type}2 %1");

        warn_format_mix_positional_nonpositional_args = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
            "cannot mix positional and non-positional arguments in format string");

        warn_printf_nonsensical_flag = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
            "flag '%0' results in undefined behavior with '%1' conversion specifier");

        warn_printf_data_arg_not_used = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
            "data argument not used by format string");

        warn_scanf_nonzero_width = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
            "zero field width in scanf format string is unused");

        plugin_warn_duplicate_flag = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                "flag '%0' used more than once");

        plugin_warn_bad_percent = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                "'%%' specifier should contain nothing but '%%'");

        plugin_warn_zero_and_precision = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                "'0' flag ignored with precision and '%%%0' %1 format");

        plugin_warn_n_format = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                "printf_s does not allow the 'n' conversion specifier");

        plugin_warn_need_width = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                "'%%%0' without width is dangerous");

        for (auto fs = functions.begin(); fs != functions.end(); ++fs) {
            function_list.push_back(*fs);
        }
        for (auto gs = gettexts.begin(); gs != gettexts.end(); ++gs) {
            gettext_list.push_back(*gs);
        }

        // Get the type kinds for various types defined in the environment
        auto size_type = instance.getASTContext().getSizeType();
        auto bi_type = clang::dyn_cast<clang::BuiltinType>(size_type);
        size_type_kind = bi_type->getKind();
        ssize_type_kind = getSignedKind(size_type_kind);

        auto intmax_type = instance.getASTContext().getIntMaxType();
        bi_type = clang::dyn_cast<clang::BuiltinType>(intmax_type);
        intmax_type_kind = bi_type->getKind();

        auto uintmax_type = instance.getASTContext().getIntMaxType();
        bi_type = clang::dyn_cast<clang::BuiltinType>(uintmax_type);
        uintmax_type_kind = bi_type->getKind();

        auto wchar_type = instance.getASTContext().getWideCharType();
        bi_type = clang::dyn_cast<clang::BuiltinType>(wchar_type);
        wchar_type_kind = bi_type->getKind();

        auto ptrdiff_type = instance.getASTContext().getWideCharType();
        bi_type = clang::dyn_cast<clang::BuiltinType>(ptrdiff_type);
        ptrdiff_type_kind = bi_type->getKind();
        uptrdiff_type_kind = getUnsignedKind(ptrdiff_type_kind);

        // No function to get wint_t. Assume that it is the promoted version
        // of wchar_t.
        if (clang::BuiltinType::Kind::Char_S <= wchar_type_kind
                && wchar_type_kind <= clang::BuiltinType::Kind::Int) {
            wint_type_kind = clang::BuiltinType::Int;
            uwint_type_kind = clang::BuiltinType::UInt;
        } else if (clang::BuiltinType::Kind::Bool <= wchar_type_kind
                && wchar_type_kind <= clang::BuiltinType::Kind::UInt) {
            wint_type_kind = clang::BuiltinType::Int;
            uwint_type_kind = clang::BuiltinType::UInt;
        } else {
            wint_type_kind = getSignedKind(wchar_type_kind);
            uwint_type_kind = getUnsignedKind(wchar_type_kind);
        }
    }

    bool VisitCallExpr(clang::CallExpr *exp)
    {
        clang::FunctionDecl *d = exp->getDirectCallee();
        if (d == nullptr) { return true; }

        // Assume that any function with a format attribute is checked in the
        // core; don't duplicate the warning
        if (!d->hasAttr<clang::FormatAttr>()) {
            // Determine whether the function is one we're looking for
            auto ident = d->getIdentifier();
            if (ident == nullptr) { return true; }
            llvm::StringRef const name = ident->getName();
            for (auto fs = function_list.begin(); fs != function_list.end(); ++fs) {
                if (name == fs->name) {
                    processFormat(exp, *fs);
                    break;
                }
            }
        }

        return true;
    }

private:
    void processFormat(clang::CallExpr *expr, FunctionDesc const & desc) const
    {
        std::vector<FormatString> formats;

        getFormatStrings(formats, expr->getArg(desc.format_arg-1));
        llvm::ArrayRef<const clang::Expr *> args(
                expr->getArgs(),
                expr->getNumArgs());

        for (auto i = formats.begin(); i != formats.end(); ++i) {
            clang::StringLiteral *fexpr = nullptr;
#if CLANG_VERSION_MAJOR < 15
            if (!i->expr->isAscii() && !i->expr->isUTF8())
#else
            if (!i->expr->isOrdinary() && !i->expr->isUTF8())
#endif
            {
                clang::QualType type = instance.getASTContext().getConstantArrayType(
                        instance.getASTContext().CharTy.withConst(),
                        llvm::APInt(32, i->string.size()+1),
                        nullptr,
                        clang::ArrayType::Normal, 0);
                clang::SourceLocation loc =
                        // Offset 1 for the L, u or U prefix
                        i->expr->getExprLoc().getLocWithOffset(1);
                fexpr = clang::StringLiteral::Create(
                        instance.getASTContext(),
                        clang::StringRef(i->string.c_str(), i->string.size()),
                        clang::StringLiteral::UTF8,
                        false,
                        type,
                        loc);
            }
            checkFormatString(fexpr ? fexpr : i->expr, args, desc);
        }
    }

    void checkFormatString(
            clang::StringLiteral const *fexpr,
            llvm::ArrayRef<const clang::Expr *> const & args,
            FunctionDesc const & desc) const
    {
        switch (desc.type) {
        case FormatStringType::printf:
        case FormatStringType::printf_s:
        case FormatStringType::kprintf:
            // printf and printf_s differ only in that printf_s does not accept %n
            checkPrintf(fexpr, args, desc);
            break;

        case FormatStringType::scanf:
        case FormatStringType::scanf_s:
            // scanf and scanf_s differ only in that scanf_s accepts a second
            // argument after any string received by %s, %c or %[; this argument
            // is an rsize_t that is the number of characters in the buffer
            checkScanf(fexpr, args, desc);
            break;

        case FormatStringType::NSString:
        case FormatStringType::strftime:
        case FormatStringType::strfmon:
            // TODO Not implemented
            break;
        }
    }

    void checkPrintf(
            clang::StringLiteral const *fexpr,
            llvm::ArrayRef<const clang::Expr *> const & args,
            FunctionDesc const & desc) const
    {
        if (desc.type == FormatStringType::kprintf) return; // TODO

        static const std::regex format_rx(
                "%([0-9]+\\$|)"                       // Introduction
                "([-'+ #0]*)"                         // Flags
                "([0-9]+|\\*[0-9]+\\$|\\*|)"          // Width
                "(\\.\\*[0-9]+\\$|\\.[0-9]*|\\.\\*|)" // Precision
                "(hh|ll|[hljztL]|)"                   // Type modifier
                "(.)");                               // Type

        // Format
        std::string format = fexpr->getBytes().str();

        // Look for mixed positional and non-positional arguments
        bool positional = false;
        bool nonpositional = false;

        // Give the "too many conversions" warning only once
        bool long_warned = false;

        // For non-positional arguments, this is the offset to the next argument
        unsigned position = 0U;

        // This will catch positional specifiers that skip arguments
        std::vector<unsigned> positions_used;

        std::smatch match;
        for (auto where = format.cbegin(); // Start next match from this offset
             std::regex_search(where, format.cend(), match, format_rx);
             where += match.position() + match.length()) {
            clang::SourceLocation warn_here = fexpr->getExprLoc().getLocWithOffset(
                    where - format.cbegin() + match.position() + 1);

            // One format specifier
            std::string specifier(match[0].str());
            std::string param_str(match[1].str());
            std::string flags(match[2].str());
            std::string width_str(match[3].str());
            std::string precision_str(match[4].str());
            std::string modifier(match[5].str());
            std::string type(match[6].str());

            // Parse the flags
            int thousands = 0;
            int minus = 0;
            int plus = 0;
            int space = 0;
            int alt = 0;
            int zero = 0;
            for (auto i = flags.cbegin(); i != flags.cend(); ++i) {
                switch (*i) {
                case '\'':
                    if (thousands == 1) {
                        // Duplicate flag
                        diagnostics.Report(warn_here,
                                plugin_warn_duplicate_flag)
                            << std::string(1, *i);
                    }
                    if (thousands < 2) {
                        ++thousands;
                    }
                    break;

                case '-':
                    if (minus == 1) {
                        // Duplicate flag
                        diagnostics.Report(warn_here,
                                plugin_warn_duplicate_flag)
                            << std::string(1, *i);
                    }
                    if (minus < 2) {
                        minus = true;
                    }
                    break;

                case '+':
                    if (plus == 1) {
                        // Duplicate flag
                        diagnostics.Report(warn_here,
                                plugin_warn_duplicate_flag)
                            << std::string(1, *i);
                    }
                    if (plus < 2) {
                        plus = true;
                    }
                    break;

                case ' ':
                    if (space == 1) {
                        // Duplicate flag
                        diagnostics.Report(warn_here,
                                plugin_warn_duplicate_flag)
                            << std::string(1, *i);
                    }
                    if (space < 2) {
                        space = true;
                    }
                    break;

                case '#':
                    if (alt == 1) {
                        // Duplicate flag
                        diagnostics.Report(warn_here,
                                plugin_warn_duplicate_flag)
                            << std::string(1, *i);
                    }
                    if (alt < 2) {
                        alt = true;
                    }
                    break;

                case '0':
                    if (zero == 1) {
                        // Duplicate flag
                        diagnostics.Report(warn_here,
                                plugin_warn_duplicate_flag)
                            << std::string(1, *i);
                    }
                    if (zero < 2) {
                        zero = true;
                    }
                    break;
                }
            }
            if (space && plus) {
                // ' ' ignored if '+' is present
                diagnostics.Report(warn_here,
                        warn_printf_ignored_flag)
                    << " " << "+";
            }
            if (zero && minus) {
                // '0' ignored if '-' is present
                diagnostics.Report(warn_here,
                        warn_printf_ignored_flag)
                    << "0" << "-";
            }

            // If width and precision are parameters, they must be int.
            // We don't otherwise care what they are.
            unsigned width = 0U;
            if (width_str == "*") {
                width = ++position;
                nonpositional = true;
            } else if (!width_str.empty() && width_str[0] == '*') {
                width = std::strtoul(width_str.c_str()+1, nullptr, 10);
                if (width == 0U) {
                    // Positional specifier is 0
                    diagnostics.Report(warn_here,
                            warn_format_zero_positional_specifier);
                } else {
                    positions_used.push_back(width);
                }
                positional = true;
            }
            if (width != 0U && desc.var_arg != 0U) {
                width = width + desc.var_arg - 2U;
                if (width >= args.size()) {
                    // Positional specifier exceeds number of arguments
                    diagnostics.Report(warn_here,
                            warn_printf_asterisk_missing_arg)
                        << 0;
                } else {
                    const clang::Expr *arg = args[width];
                    const clang::Type *type = arg->getType().getTypePtrOrNull();
                    // Don't strip off implicit conversions
                    const clang::BuiltinType *bi_type = clang::dyn_cast<clang::BuiltinType>(type);
                    if (bi_type == nullptr
                            || (bi_type->getKind() != clang::BuiltinType::Int
                                && bi_type->getKind() != clang::BuiltinType::UInt)) {
                        // Width parameter is not int
                        diagnostics.Report(warn_here,
                                warn_printf_asterisk_wrong_type)
                            << 0
                            << "'int'"
                            << "'" + arg->getType().getAsString() + "'";
                    }
                }
            }

            unsigned precision = 0U;
            if (precision_str == ".*") {
                precision = ++position;
                nonpositional = true;
            } else if (!precision_str.empty() && precision_str[1] == '*') {
                precision = std::strtoul(precision_str.c_str()+2, nullptr, 10);
                if (precision == 0U) {
                    // Positional specifier is 0
                    diagnostics.Report(warn_here,
                            warn_format_zero_positional_specifier);
                } else {
                    positions_used.push_back(precision);
                }
                positional = true;
            }
            if (precision != 0U && desc.var_arg != 0U) {
                precision = precision + desc.var_arg - 2U;
                if (precision >= args.size()) {
                    // Positional specifier exceeds number of arguments
                    diagnostics.Report(warn_here,
                            warn_printf_asterisk_missing_arg)
                        << 1;
                } else {
                    const clang::Expr *arg = args[precision];
                    const clang::Type *type = arg->getType().getTypePtrOrNull();
                    // Don't strip off implicit conversions
                    const clang::BuiltinType *bi_type = clang::dyn_cast<clang::BuiltinType>(type);
                    if (bi_type == nullptr
                            || (bi_type->getKind() != clang::BuiltinType::Int
                                && bi_type->getKind() != clang::BuiltinType::UInt)) {
                        // Precision parameter is not int
                        diagnostics.Report(warn_here,
                                warn_printf_asterisk_wrong_type)
                            << 1
                            << "'int'"
                            << "'" + arg->getType().getAsString() + "'";
                    }
                }
            }

            // Check for %%
            if (type == "%") {
                // "A % character is written. No argument is converted. The
                // complete conversion specification shall be %%."
                // -- ISO/IEC 9899:2011, 7.21.6.1 "The fprintf function",
                //    paragraph 8
                if (specifier != "%%") {
                    // Invalid % specifier
                    diagnostics.Report(warn_here,
                            plugin_warn_bad_percent);
                }
                continue;
            }

            // Identify the parameter that matches this specifier
            unsigned param;
            if (param_str.empty()) {
                param = ++position;
                nonpositional = true;
                if (!long_warned && param > args.size() - (desc.var_arg - 1U)) {
                    // Too many % specifiers
                    diagnostics.Report(warn_here,
                            warn_printf_insufficient_data_args);
                    long_warned = true;
                }
            } else {
                param = std::strtoul(param_str.c_str(), nullptr, 10);
                positional = true;
                if (param == 0) {
                    diagnostics.Report(warn_here,
                            warn_format_zero_positional_specifier);
                    param = 1;
                } else {
                    positions_used.push_back(param);
                }
                if (param > args.size() - (desc.var_arg - 1U)) {
                    // Arg number exceeds the number of arguments
                    diagnostics.Report(warn_here,
                            warn_printf_positional_arg_exceeds_data_args)
                        << param
                        << static_cast<unsigned>(args.size() - (desc.var_arg - 1U));
                }
            }
            param = param + desc.var_arg - 2U;

            // Identify the desired type of the parameter
            clang::BuiltinType::Kind kind1 = clang::BuiltinType::Void;
            clang::BuiltinType::Kind kind2 = clang::BuiltinType::Void;
            bool pointer = false;
            bool const_pointer = false;
            switch (type[0]) {
            case 'd':
            case 'i':
            case 'o':
            case 'u':
            case 'x':
            case 'X':
                if (zero && !precision_str.empty()) {
                    // Zero and precision
                    diagnostics.Report(warn_here,
                            plugin_warn_zero_and_precision)
                        << type
                        << formatTypeName(desc.type);
                }
                if (modifier.empty()) {
                    kind1 = clang::BuiltinType::Int;
                    kind2 = clang::BuiltinType::UInt;
                } else if (modifier == "hh") {
                    kind1 = clang::BuiltinType::Int;
                    kind2 = clang::BuiltinType::UInt;
                } else if (modifier == "h") {
                    kind1 = clang::BuiltinType::Int;
                    kind2 = clang::BuiltinType::UInt;
                } else if (modifier == "l") {
                    kind1 = clang::BuiltinType::Long;
                    kind2 = clang::BuiltinType::ULong;
                } else if (modifier == "ll") {
                    kind1 = clang::BuiltinType::LongLong;
                    kind2 = clang::BuiltinType::ULongLong;
                } else if (modifier == "j") { // ptrdiff_t
                    kind1 = ptrdiff_type_kind;
                    kind2 = uptrdiff_type_kind;
                } else if (modifier == "z") { // size_t
                    kind1 = ssize_type_kind;
                    kind2 = size_type_kind;
                } else if (modifier == "t") { // intmax_t
                    kind1 = intmax_type_kind;
                    kind2 = uintmax_type_kind;
                } else {
                    // Modifier not compatible
                    diagnostics.Report(warn_here,
                            warn_format_nonsensical_length)
                        << modifier << type;
                    kind1 = clang::BuiltinType::Int;
                    kind2 = clang::BuiltinType::UInt;
                }
                break;

            case 'f':
            case 'F':
            case 'e':
            case 'E':
            case 'g':
            case 'G':
            case 'a':
            case 'A':
                if (modifier.empty()) {
                    kind1 = clang::BuiltinType::Double;
                } else if (modifier == "l") {
                    kind1 = clang::BuiltinType::Double;
                } else if (modifier == "ll") {
                    kind1 = clang::BuiltinType::LongDouble;
                } else {
                    // Modifier not compatible
                    diagnostics.Report(warn_here,
                            warn_format_nonsensical_length)
                        << modifier << type;
                    kind1 = clang::BuiltinType::Double;
                }
                kind2 = kind1;
                break;

            case 'c':
                if (modifier.empty()) {
                    kind1 = clang::BuiltinType::Int;
                    kind2 = kind1;
                } else if (modifier == "l") { // wint_t
                    kind1 = wint_type_kind;
                    kind2 = uwint_type_kind;
                } else {
                    // Modifier not compatible
                    diagnostics.Report(warn_here,
                            warn_format_nonsensical_length)
                        << modifier << type;
                    kind1 = clang::BuiltinType::Int;
                    kind2 = kind1;
                }
                break;

            case 's':
                if (modifier.empty()) {
                    kind1 = clang::BuiltinType::Char_S;
                    kind2 = clang::BuiltinType::Char_U;
                } else if (modifier == "l") { // wchar_t
                    kind1 = wchar_type_kind;
                    kind2 = kind1;
                } else {
                    // Modifier not compatible
                    diagnostics.Report(warn_here,
                            warn_format_nonsensical_length)
                        << modifier << type;
                    kind1 = clang::BuiltinType::Char_S;
                    kind2 = clang::BuiltinType::Char_U;
                }
                pointer = true;
                const_pointer = true;
                break;

            case 'p':
                if (!modifier.empty()) {
                    // Modifier not compatible
                    diagnostics.Report(warn_here,
                            warn_format_nonsensical_length)
                        << modifier << type;
                }
                pointer = true;
                const_pointer = true;
                break;

            case 'n':
                if (desc.type == FormatStringType::printf_s) {
                    // printf_s does not support %n
                    diagnostics.Report(warn_here,
                            plugin_warn_n_format);
                }
                if (modifier.empty()) {
                    kind1 = clang::BuiltinType::Int;
                } else if (modifier == "hh") {
                    kind1 = clang::BuiltinType::SChar;
                } else if (modifier == "h") {
                    kind1 = clang::BuiltinType::Short;
                } else if (modifier == "l") {
                    kind1 = clang::BuiltinType::Long;
                } else if (modifier == "ll") {
                    kind1 = clang::BuiltinType::LongLong;
                } else if (modifier == "j") { // ptrdiff_t
                    kind1 = ptrdiff_type_kind;
                } else if (modifier == "z") { // size_t
                    kind1 = ssize_type_kind;
                } else if (modifier == "t") { // intmax_t
                    kind1 = intmax_type_kind;
                } else {
                    // Modifier not compatible
                    diagnostics.Report(warn_here,
                            warn_format_nonsensical_length)
                        << modifier << type;
                    kind1 = clang::BuiltinType::Int;
                }
                kind2 = kind1;
                pointer = true;
                const_pointer = false;
                break;

            case 'C': // wint_t
                if (!modifier.empty()) {
                    // Modifier not compatible
                    diagnostics.Report(warn_here,
                            warn_format_nonsensical_length)
                        << modifier << type;
                }
                kind1 = wint_type_kind;
                kind2 = uwint_type_kind;
                break;

            case 'S': // wchar_t
                if (!modifier.empty()) {
                    // Modifier not compatible
                    diagnostics.Report(warn_here,
                            warn_format_nonsensical_length)
                        << modifier << type;
                }
                kind1 = wchar_type_kind;
                kind2 = kind1;
                pointer = true;
                const_pointer = true;
                break;

            default:
                // Unknown type specifier
                diagnostics.Report(warn_here,
                        warn_format_invalid_conversion)
                    << type;
                break;
            }
            bool warn_thousands = std::strchr("ACESXacenopsx", type[0]) != nullptr;
            bool warn_minus     = type[0] == 'n';
            bool warn_plus      = std::strchr("CSXcnopsux",    type[0]) != nullptr;
            bool warn_space     = std::strchr("CSXcnopsux",    type[0]) != nullptr;
            bool warn_alt       = std::strchr("CScdinpsu",     type[0]) != nullptr;
            bool warn_zero      = std::strchr("CScnps",        type[0]) != nullptr;
            if (warn_thousands && thousands) {
                diagnostics.Report(warn_here,
                        warn_printf_nonsensical_flag)
                    << "\'"
                    << type;
            }
            if (warn_minus && minus) {
                diagnostics.Report(warn_here,
                        warn_printf_nonsensical_flag)
                    << "-"
                    << type;
            }
            if (warn_plus && plus) {
                diagnostics.Report(warn_here,
                        warn_printf_nonsensical_flag)
                    << "+"
                    << type;
            }
            if (warn_space && space) {
                diagnostics.Report(warn_here,
                        warn_printf_nonsensical_flag)
                    << " "
                    << type;
            }
            if (warn_alt && alt) {
                diagnostics.Report(warn_here,
                        warn_printf_nonsensical_flag)
                    << "#"
                    << type;
            }
            if (warn_zero && zero) {
                diagnostics.Report(warn_here,
                        warn_printf_nonsensical_flag)
                    << "0"
                    << type;
            }

            if (param < args.size() && desc.var_arg != 0U) {
                std::string expected;
                bool warn_match = false;
                const clang::Expr *arg = args[param];
                clang::QualType argtype = arg->getType();

                if (type == "p") {
                    // Accept any pointer type
                    expected = "void *";
                    warn_match = !argtype->isPointerType();
                } else if (kind1 != clang::BuiltinType::Void) {
                    const clang::BuiltinType *bi_type = nullptr;

                    if (pointer) { // %s, %n
                        expected = typeName(kind1) + " *";
                        if (!argtype->isPointerType()) {
                            // Expected a pointer type but the type is not a pointer
                            warn_match = true;
                        } else if (!const_pointer && argtype->getPointeeType().isConstQualified()) {
                            // The pointer points to a const type but we can't accept const (%n)
                            warn_match = true;
                        } else {
                            const clang::Type *ptype = argtype->getPointeeType().getTypePtrOrNull();
                            if (ptype != nullptr) {
                                bi_type = clang::dyn_cast<clang::BuiltinType>(ptype);
                            }
                        }
                    } else {
                        expected = typeName(kind1);
                        bi_type = clang::dyn_cast<clang::BuiltinType>(argtype);
                    }
                    if (!warn_match && bi_type != nullptr) {
                        clang::BuiltinType::Kind arg_kind = bi_type->getKind();
                        if (arg_kind != kind1 && arg_kind != kind2) {
                            warn_match = true;
                        }
                    }
                }
                if (warn_match) {
                    // Type does not match
                    diagnostics.Report(warn_here,
                            warn_format_conversion_argument_type_mismatch)
                        << "'" + expected + "'"
                        << "'" + arg->getType().getAsString() + "'"
                        << 0;
                }
            }
        }

        // Warn if positional and nonpositional parameters are mixed
        if (positional && nonpositional) {
            // Positional and nonpositional parameters are mixed
            diagnostics.Report(fexpr->getExprLoc(),
                warn_format_mix_positional_nonpositional_args);
        }

        // Warn if positional parameters are used and any are skipped
        if (positional) {
            std::sort(positions_used.begin(), positions_used.end());
            unsigned last = 0U;
            for (auto i = positions_used.cbegin();
                    i != positions_used.cend(); ++i) {
                // A position may be used more than once
                for (unsigned j = last + 1U; j < *i; ++j) {
                    unsigned pos = desc.var_arg + j - 2U;
                    if (pos >= args.size()) break;
                    diagnostics.Report(args[pos]->getExprLoc(),
                            warn_printf_data_arg_not_used);
                }
                last = *i;
            }
            position = positions_used.empty() ? 0U : positions_used.back();
        }
        // Warn if arguments at the end are unused
        for (unsigned pos = desc.var_arg + position - 1U;
                pos < args.size(); ++pos) {
            diagnostics.Report(args[pos]->getExprLoc(),
                    warn_printf_data_arg_not_used);
        }
    }

    void checkScanf(
            clang::StringLiteral const *fexpr,
            llvm::ArrayRef<const clang::Expr *> const & args,
            FunctionDesc const & desc) const
    {
        static const std::regex format_rx(
                "%([0-9]+\\$|)"                  // Introduction
                "(\\*?)"                         // Flags
                "([0-9]+|)"                      // Width
                "(hh|ll|[hljztL]|)"              // Type modifier
                "(\\[\\^.+?\\]|\\[[^^].*?\\]|.)"); // Type

        // Format
        std::string format = fexpr->getBytes().str();

        // Look for mixed positional and non-positional arguments
        bool positional = false;
        bool nonpositional = false;

        // Give the "too many conversions" warning only once
        bool long_warned = false;

        // For non-positional arguments, this is the offset to the next argument
        unsigned position = 0U;

        // This will catch positional specifiers that skip arguments
        std::vector<unsigned> positions_used;

        std::smatch match;
        for (auto where = format.cbegin(); // Start next match from this offset
             std::regex_search(where, format.cend(), match, format_rx);
             where += match.position() + match.length()) {
            clang::SourceLocation warn_here = fexpr->getExprLoc().getLocWithOffset(
                    where - format.cbegin() + match.position() + 1);

            // One format specifier
            std::string specifier(match[0].str());
            std::string param_str(match[1].str());
            std::string suppress(match[2].str());
            std::string width_str(match[3].str());
            std::string modifier(match[4].str());
            std::string type(match[5].str());

            // If width is present, it must not be zero.
            unsigned width = 0U;
            if (!width_str.empty()) {
                width = std::strtoul(width_str.c_str(), nullptr, 10);
                if (width == 0U) {
                    diagnostics.Report(warn_here,
                            warn_scanf_nonzero_width);
                }
            }

            // Check for %%
            if (type == "%") {
                // "Matches a single % character; no conversion or assignment
                // occurs. The complete conversion specification shall be %%."
                // -- ISO/IEC 9899:2011, 7.21.6.2 "The fscanf function",
                //    paragraph 12
                if (specifier != "%%") {
                    // Invalid % specifier
                    diagnostics.Report(warn_here,
                            plugin_warn_bad_percent);
                }
                continue;
            }

            // If no argument is used, stop here
            if (!suppress.empty()) {
                continue;
            }

            // Identify the parameter that matches this specifier
            unsigned param;
            if (param_str.empty()) {
                param = ++position;
                nonpositional = true;
                if (!long_warned && param > args.size() - (desc.var_arg - 1U)) {
                    // Too many % specifiers
                    diagnostics.Report(warn_here,
                            warn_printf_insufficient_data_args);
                    long_warned = true;
                }
            } else {
                param = std::strtoul(param_str.c_str(), nullptr, 10);
                positional = true;
                if (param == 0) {
                    diagnostics.Report(warn_here,
                            warn_format_zero_positional_specifier);
                    param = 1;
                } else {
                    positions_used.push_back(param);
                }
                if (param > args.size() - (desc.var_arg - 1U)) {
                    // Arg number exceeds the number of arguments
                    diagnostics.Report(warn_here,
                            warn_printf_positional_arg_exceeds_data_args)
                        << param
                        << static_cast<unsigned>(args.size() - (desc.var_arg - 1U));
                }
            }
            param = param + desc.var_arg - 2U;

            // Identify the desired type of the parameter
            clang::BuiltinType::Kind kind1 = clang::BuiltinType::Void;
            clang::BuiltinType::Kind kind2 = clang::BuiltinType::Void;
            std::string expected;
            switch (type[0]) {
            case 'd':
            case 'i':
            case 'n':
                if (modifier.empty()) {
                    expected = "int *";
                    kind1 = clang::BuiltinType::Int;
                    kind2 = clang::BuiltinType::UInt;
                } else if (modifier == "hh") {
                    expected = "signed char *";
                    kind1 = clang::BuiltinType::SChar;
                    kind2 = clang::BuiltinType::UChar;
                } else if (modifier == "h") {
                    expected = "short *";
                    kind1 = clang::BuiltinType::Short;
                    kind2 = clang::BuiltinType::UShort;
                } else if (modifier == "l") {
                    expected = "long *";
                    kind1 = clang::BuiltinType::Long;
                    kind2 = clang::BuiltinType::ULong;
                } else if (modifier == "ll") {
                    expected = "long long *";
                    kind1 = clang::BuiltinType::LongLong;
                    kind2 = clang::BuiltinType::ULongLong;
                } else if (modifier == "j") { // ptrdiff_t
                    expected = "ptrdiff_t *";
                    kind1 = ptrdiff_type_kind;
                    kind2 = uptrdiff_type_kind;
                } else if (modifier == "z") { // size_t
                    expected = "size_t *";
                    kind1 = ssize_type_kind;
                    kind2 = size_type_kind;
                } else if (modifier == "t") { // intmax_t
                    expected = "intmax_t *";
                    kind1 = intmax_type_kind;
                    kind2 = uintmax_type_kind;
                } else {
                    // Modifier not compatible
                    diagnostics.Report(warn_here,
                            warn_format_nonsensical_length)
                        << modifier << type;
                    expected = "int *";
                    kind1 = clang::BuiltinType::Int;
                    kind2 = clang::BuiltinType::UInt;
                }
                break;

            case 'o':
            case 'u':
            case 'x':
            case 'X':
                if (modifier.empty()) {
                    expected = "unsigned int *";
                    kind1 = clang::BuiltinType::Int;
                    kind2 = clang::BuiltinType::UInt;
                } else if (modifier == "hh") {
                    expected = "unsigned char *";
                    kind1 = clang::BuiltinType::SChar;
                    kind2 = clang::BuiltinType::UChar;
                } else if (modifier == "h") {
                    expected = "unsigned short *";
                    kind1 = clang::BuiltinType::Short;
                    kind2 = clang::BuiltinType::UShort;
                } else if (modifier == "l") {
                    expected = "unsigned long *";
                    kind1 = clang::BuiltinType::Long;
                    kind2 = clang::BuiltinType::ULong;
                } else if (modifier == "ll") {
                    expected = "unsigned long long *";
                    kind1 = clang::BuiltinType::LongLong;
                    kind2 = clang::BuiltinType::ULongLong;
                } else if (modifier == "j") { // ptrdiff_t
                    expected = "ptrdiff_t *";
                    kind1 = ptrdiff_type_kind;
                    kind2 = uptrdiff_type_kind;
                } else if (modifier == "z") { // size_t
                    expected = "size_t *";
                    kind1 = ssize_type_kind;
                    kind2 = size_type_kind;
                } else if (modifier == "t") { // intmax_t
                    expected = "uintmax_t *";
                    kind1 = intmax_type_kind;
                    kind2 = uintmax_type_kind;
                } else {
                    // Modifier not compatible
                    diagnostics.Report(warn_here,
                            warn_format_nonsensical_length)
                        << modifier << type;
                    expected = "unsigned int *";
                    kind1 = clang::BuiltinType::Int;
                    kind2 = clang::BuiltinType::UInt;
                }
                break;

            case 'f':
            case 'F':
            case 'e':
            case 'E':
            case 'g':
            case 'G':
            case 'a':
            case 'A':
                if (modifier.empty()) {
                    expected = "float *";
                    kind1 = clang::BuiltinType::Float;
                } else if (modifier == "l") {
                    kind1 = clang::BuiltinType::Double;
                    expected = "double *";
                } else if (modifier == "ll") {
                    expected = "long double *";
                    kind1 = clang::BuiltinType::LongDouble;
                } else {
                    // Modifier not compatible
                    diagnostics.Report(warn_here,
                            warn_format_nonsensical_length)
                        << modifier << type;
                    expected = "float *";
                    kind1 = clang::BuiltinType::Double;
                }
                kind2 = kind1;
                break;

            case 'c':
            case 's':
            case '[':
                if (width == 0U && desc.type != FormatStringType::scanf_s) {
                    diagnostics.Report(warn_here,
                            plugin_warn_need_width)
                        << type.substr(0, 1);
                }
                if (modifier.empty()) {
                    expected = "char *";
                    kind1 = clang::BuiltinType::Char_S;
                    kind2 = clang::BuiltinType::Char_U;
                } else if (modifier == "l") { // wchar_t
                    expected = "wchar_t *";
                    kind1 = wchar_type_kind;
                    kind2 = kind1;
                } else {
                    // Modifier not compatible
                    diagnostics.Report(warn_here,
                            warn_format_nonsensical_length)
                        << modifier << type;
                    expected = "char *";
                    kind1 = clang::BuiltinType::Char_S;
                    kind2 = clang::BuiltinType::Char_U;
                }
                break;

            case 'p':
                if (!modifier.empty()) {
                    // Modifier not compatible
                    diagnostics.Report(warn_here,
                            warn_format_nonsensical_length)
                        << modifier << type;
                }
                expected = "void **";
                break;

            case 'C':
            case 'S': // wchar_t
                if (width == 0U && desc.type != FormatStringType::scanf_s) {
                    diagnostics.Report(warn_here,
                            plugin_warn_need_width)
                        << type.substr(0, 1);
                }
                if (!modifier.empty()) {
                    // Modifier not compatible
                    diagnostics.Report(warn_here,
                            warn_format_nonsensical_length)
                        << modifier << type;
                }
                expected = "wchar_t *";
                kind1 = wchar_type_kind;
                kind2 = kind1;
                break;

            default:
                // Unknown type specifier
                diagnostics.Report(warn_here,
                        warn_format_invalid_conversion)
                    << type;
                break;
            }

            if (!expected.empty() && param < args.size() && desc.var_arg != 0U) {
                bool warn_match = false;
                const clang::Expr *arg = args[param];
                clang::QualType argtype = arg->getType();

                // Expected type is always a non-const pointer; kind1 and
                // kind2 indicate the type of the dereference
                if (!argtype->isPointerType()
                        || argtype->getPointeeType().isConstQualified()) {
                    warn_match = true;
                } else if (type == "p") {
                    // Accept any pointer type
                    warn_match = !argtype->getPointeeType()->isPointerType();
                } else {
                    const clang::Type *ptype = argtype->getPointeeType().getTypePtrOrNull();
                    const clang::BuiltinType *bi_type = nullptr;

                    if (ptype != nullptr) {
                        bi_type = clang::dyn_cast<clang::BuiltinType>(ptype);
                    }
                    if (bi_type == nullptr) {
                        warn_match = true;
                    } else {
                        clang::BuiltinType::Kind arg_kind = bi_type->getKind();
                        warn_match = (arg_kind != kind1 && arg_kind != kind2);
                    }
                }
                if (warn_match) {
                    // Type does not match
                    diagnostics.Report(warn_here,
                            warn_format_conversion_argument_type_mismatch)
                        << "'" + expected + "'"
                        << "'" + arg->getType().getAsString() + "'"
                        << 0;
                }
            }

            // For scanf_s, the string conversions accept two arguments, and
            // the second one is size_t.
            // POSIX does not specify scanf_s, and no POSIX-like implementation
            // of scanf_s is known. Assume that the size follows the array;
            // that is, %1$s accepts a char * as argument 1, and a size_t as
            // argument 2.
            if (desc.type == FormatStringType::scanf_s
                    && std::strchr("cs[CS", type[0]) != nullptr) {
                unsigned param2 = param + 1U;
                if (!param_str.empty()) {
                    positions_used.push_back(positions_used.back() + 1);
                } else {
                    ++position;
                }
                if (param2 >= args.size()) {
                    if (param_str.empty()) {
                        if (!long_warned) {
                            diagnostics.Report(warn_here,
                                    warn_printf_insufficient_data_args);
                            long_warned = true;
                        }
                    } else {
                        diagnostics.Report(warn_here,
                                warn_printf_positional_arg_exceeds_data_args)
                            << param2
                            << static_cast<unsigned>(args.size() - (desc.var_arg - 1U));
                    }
                } else {
                    const clang::Expr *arg = args[param2];
                    clang::QualType argtype = arg->getType();
                    const clang::BuiltinType *bi_type = clang::dyn_cast<clang::BuiltinType>(argtype);
                    bool warn_match;
                    if (bi_type == nullptr) {
                        warn_match = true;
                    } else {
                        clang::BuiltinType::Kind kind = bi_type->getKind();
                        warn_match = (kind != size_type_kind && kind != ssize_type_kind);
                    }
                    if (warn_match) {
                        // Type does not match
                        diagnostics.Report(warn_here,
                                warn_format_conversion_argument_type_mismatch)
                            << "'rsize_t'"
                            << "'" + argtype.getAsString() + "'"
                            << 0;
                    }
                }
            }
        }

        // Warn if positional and nonpositional parameters are mixed
        if (positional && nonpositional) {
            // Positional and nonpositional parameters are mixed
            diagnostics.Report(fexpr->getExprLoc(),
                warn_format_mix_positional_nonpositional_args);
        }

        // Warn if positional parameters are used and any are skipped
        if (positional) {
            std::sort(positions_used.begin(), positions_used.end());
            unsigned last = 0U;
            for (auto i = positions_used.cbegin();
                    i != positions_used.cend(); ++i) {
                // A position may be used more than once
                for (unsigned j = last + 1U; j < *i; ++j) {
                    unsigned pos = desc.var_arg + j - 2U;
                    if (pos >= args.size()) break;
                    diagnostics.Report(args[pos]->getExprLoc(),
                            warn_printf_data_arg_not_used);
                }
                last = *i;
            }
            position = positions_used.empty() ? 0U : positions_used.back();
        }
        // Warn if arguments at the end are unused
        for (unsigned pos = desc.var_arg + position - 1U;
                pos < args.size(); ++pos) {
            diagnostics.Report(args[pos]->getExprLoc(),
                    warn_printf_data_arg_not_used);
        }
    }

    void getFormatStrings(
            std::vector<FormatString>& formats,
            clang::Expr const *arg) const
    {
        if (arg == nullptr) { return; }

        // Pass any implicit type conversions
        arg = arg->IgnoreImplicit();

        // If function call, look for gettext-like functions
        {
            clang::CallExpr const *call = llvm::dyn_cast_or_null<clang::CallExpr>(arg);
            if (call != nullptr) {
                clang::FunctionDecl const *d = call->getDirectCallee();
                if (d == nullptr) { goto warn; }
                if (d->hasAttr<clang::FormatArgAttr>()) {
                    for (auto a = d->specific_attr_begin<clang::FormatArgAttr>();
                         a != d->specific_attr_end<clang::FormatArgAttr>(); ++a) {
                        // The function may have any number of format_arg attributes;
                        // this allows the function to accept multiple format strings
                        getFormatStrings(formats, call->getArg((*a)->getFormatIdx().getLLVMIndex()));
                    }
                } else {
                    auto ident = d->getIdentifier();
                    if (ident == nullptr) { goto warn; }
                    llvm::StringRef const name = ident->getName();
                    bool found = false;
                    for (auto a = gettext_list.begin();
                        a != gettext_list.end(); ++a) {
                        // The function appear any number of times in getext_list;
                        // this allows the function to accept multiple format strings
                        if (name == a->name) {
                            getFormatStrings(formats, call->getArg(a->format_arg-1));
                            found = true;
                        }
                    }
                    if (!found) { goto warn; }
                }
                return;
            }
        }
        // If conditional operator, process the two possible format strings
        {
            clang::AbstractConditionalOperator const *cond = llvm::dyn_cast_or_null<clang::AbstractConditionalOperator>(arg);
            if (cond != nullptr) {
                getFormatStrings(formats, cond->getTrueExpr());
                getFormatStrings(formats, cond->getFalseExpr());
                return;
            }
        }
        {
            // If string literal, add it to the list
            clang::StringLiteral const *str = llvm::dyn_cast_or_null<clang::StringLiteral>(arg);
            if (str != nullptr) {
                // Get the text of the string literal
                FormatString fmt;
                fmt.string = stringToUTF8(str);
                fmt.expr = str;
                formats.push_back(fmt);
                return;
            }
        }

    warn:
        // Can't process this format; raise a warning
        diagnostics.Report(arg->getExprLoc(), warn_format_nonliteral);
    }

    clang::CompilerInstance& instance;
    clang::DiagnosticsEngine& diagnostics;
    unsigned warn_format_nonliteral;
    unsigned warn_printf_ignored_flag;
    unsigned warn_format_zero_positional_specifier;
    unsigned warn_printf_asterisk_missing_arg;
    unsigned warn_printf_asterisk_wrong_type;
    unsigned warn_format_nonsensical_length;
    unsigned warn_format_invalid_conversion;
    unsigned warn_printf_insufficient_data_args;
    unsigned warn_printf_positional_arg_exceeds_data_args;
    unsigned warn_format_conversion_argument_type_mismatch;
    unsigned warn_format_mix_positional_nonpositional_args;
    unsigned warn_printf_nonsensical_flag;
    unsigned warn_printf_data_arg_not_used;
    unsigned warn_scanf_nonzero_width;
    unsigned plugin_warn_duplicate_flag;
    unsigned plugin_warn_bad_percent;
    unsigned plugin_warn_zero_and_precision;
    unsigned plugin_warn_n_format;
    unsigned plugin_warn_need_width;
    clang::BuiltinType::Kind size_type_kind;
    clang::BuiltinType::Kind ssize_type_kind;
    clang::BuiltinType::Kind intmax_type_kind;
    clang::BuiltinType::Kind uintmax_type_kind;
    clang::BuiltinType::Kind wchar_type_kind;
    clang::BuiltinType::Kind ptrdiff_type_kind;
    clang::BuiltinType::Kind uptrdiff_type_kind;
    clang::BuiltinType::Kind wint_type_kind;
    clang::BuiltinType::Kind uwint_type_kind;
    std::vector<FunctionDesc> function_list;
    std::vector<GettextDesc> gettext_list;
};

struct FormatASTConsumer : public clang::ASTConsumer {
public:
    explicit FormatASTConsumer(
            clang::CompilerInstance& inst,
            std::vector<FunctionDesc> const & functions,
            std::vector<GettextDesc> const & gettexts)
    : Visitor(inst, functions, gettexts) {}

    virtual void HandleTranslationUnit(clang::ASTContext &Context)
    {
        Visitor.TraverseDecl(Context.getTranslationUnitDecl());
    }
private:
    FormatVisitor Visitor;
};

class FormatAction : public clang::PluginASTAction {
public:
	clang_compat::ASTConsumerPtr CreateASTConsumer(clang::CompilerInstance& inst, llvm::StringRef str) override
    {
        if (function_list.empty()) {
            for (std::size_t i = 0; i < sizeof(default_function_list)/sizeof(default_function_list[0]); ++i) {
                function_list.push_back(default_function_list[i]);
            }
        }
        return clang_compat::ASTConsumerPtr(new FormatASTConsumer(inst, function_list, gettext_list));
    }

    bool ParseArgs(const clang::CompilerInstance& inst,
                   const std::vector<std::string>& args) override
    {
        bool ok = true;
        clang::DiagnosticsEngine *diagnostics = &inst.getDiagnostics();
        unsigned bad_file = diagnostics->getCustomDiagID(clang::DiagnosticsEngine::Error, "cannot open file %0: %1");
        unsigned bad_option = diagnostics->getCustomDiagID(clang::DiagnosticsEngine::Error, "unknown option:  %0");

        for (auto i = args.begin(); i != args.end(); ++i) {
            std::string arg = *i;
            if (arg.substr(0, 6) == "-list=") {
                std::string filename(arg.substr(6));
                std::ifstream file(filename);
                while (file.good()) {
                    std::string line;
                    std::getline(file, line);
                    std::vector<std::string> vec = Split(line);
                    if (vec.size() < 3) { continue; }

                    if (vec[1] == "gettext") {
                        GettextDesc desc;
                        desc.name = vec[0];
                        desc.format_arg = std::strtoull(vec[2].c_str(), nullptr, 10);
                        gettext_list.push_back(desc);
                    } else {
                        FunctionDesc desc;
                        desc.name = vec[0];
                        if (vec[1] == "printf") {
                            desc.type = FormatStringType::printf;
                        } else if (vec[1] == "printf_s") {
                            desc.type = FormatStringType::printf_s;
                        } else if (vec[1] == "scanf") {
                            desc.type = FormatStringType::scanf;
                        } else if (vec[1] == "scanf_s") {
                            desc.type = FormatStringType::scanf_s;
                        } else if (vec[1] == "NSString") {
                            desc.type = FormatStringType::NSString;
                        } else if (vec[1] == "strftime") {
                            desc.type = FormatStringType::strftime;
                        } else if (vec[1] == "strfmon") {
                            desc.type = FormatStringType::strfmon;
                        } else if (vec[1] == "kprintf") {
                            desc.type = FormatStringType::kprintf;
                        } else {
                            continue;
                        }
                        desc.format_arg = std::strtoull(vec[2].c_str(), nullptr, 10);
                        desc.var_arg = vec.size() > 3 ? std::strtoull(vec[3].c_str(), nullptr, 10) : 0;
                        function_list.push_back(desc);
                    }
                }
                if (!file.eof()) {
                    // failed to open the file, or error while reading it
                    diagnostics->Report(bad_file) << filename << std::strerror(errno);
                    ok = false;
                }
            } else {
                diagnostics->Report(bad_option) << arg;
                ok = false;
            }
        }
        return ok;
    }

private:
    std::vector<FunctionDesc> function_list;
    std::vector<GettextDesc> gettext_list;
};

} // end anonymous namespace

static clang::FrontendPluginRegistry::Add<FormatAction>
X("format-plugin", "provide format checking for wide string functions");
