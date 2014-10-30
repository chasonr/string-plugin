// Use with:
// -Xclang -load -Xclang path/to/string-plugin.so -Xclang -add-plugin -Xclang format-plugin
// and zero or more of
// -Xclang -plugin-arg-format-plugin -Xclang -list=functions.txt

#include <fstream>
#include <string>
#include <vector>
#include <cstdlib>

#define __STDC_LIMIT_MACROS 1
#define __STDC_CONSTANT_MACROS 1
#include <clang/AST/AST.h>
#include <clang/AST/ASTConsumer.h>
#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/FrontendPluginRegistry.h>
#include <clang/Sema/Sema.h>
#include <llvm/ADT/SmallBitVector.h>

#include "split.h"
#include "utf8string.h"

namespace {

struct FunctionDesc {
    std::string name;
    clang::Sema::FormatStringType type;
    unsigned format_arg;
    unsigned var_arg;
};
FunctionDesc const default_function_list[] = {
    // Wide string formatting
    { "wprintf",      clang::Sema::FST_Printf,   1, 2 },
    { "fwprintf",     clang::Sema::FST_Printf,   2, 3 },
    { "swprintf",     clang::Sema::FST_Printf,   3, 4 },
    { "vwprintf",     clang::Sema::FST_Printf,   1, 0 },
    { "vfwprintf",    clang::Sema::FST_Printf,   2, 0 },
    { "vswprintf",    clang::Sema::FST_Printf,   3, 0 },
    { "wscanf",       clang::Sema::FST_Scanf,    1, 2 },
    { "fwscanf",      clang::Sema::FST_Scanf,    2, 3 },
    { "swscanf",      clang::Sema::FST_Scanf,    2, 3 },
    { "vwscanf",      clang::Sema::FST_Scanf,    1, 0 },
    { "vfwscanf",     clang::Sema::FST_Scanf,    2, 0 },
    { "vswscanf",     clang::Sema::FST_Scanf,    2, 0 },
    // Annex K
    { "wprintf_s",    clang::Sema::FST_Printf,   1, 2 },
    { "fwprintf_s",   clang::Sema::FST_Printf,   2, 3 },
    { "swprintf_s",   clang::Sema::FST_Printf,   3, 4 },
    { "snwprintf_s",  clang::Sema::FST_Printf,   3, 4 },
    { "vwprintf_s",   clang::Sema::FST_Printf,   1, 0 },
    { "vfwprintf_s",  clang::Sema::FST_Printf,   2, 0 },
    { "vswprintf_s",  clang::Sema::FST_Printf,   3, 0 },
    { "vsnwprintf_s", clang::Sema::FST_Printf,   3, 0 },
    // Wide string time
    { "wcsftime",     clang::Sema::FST_Strftime, 3, 0 },
};

struct GettextDesc {
    std::string name;
    unsigned format_arg;
};

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
        not_literal = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning, "format is not a string literal");
        for (auto fs = functions.begin(); fs != functions.end(); ++fs) {
            function_list.push_back(*fs);
        }
        for (auto gs = gettexts.begin(); gs != gettexts.end(); ++gs) {
            gettext_list.push_back(*gs);
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
            if (!i->expr->isAscii() && !i->expr->isUTF8()) {
                clang::QualType type = instance.getASTContext().getConstantArrayType(
                        instance.getASTContext().CharTy.withConst(),
                        llvm::APInt(32, i->string.size()+1),
                        clang::ArrayType::Normal, 0);
                clang::SourceLocation loc =
                        // Offset 1 for the L, u or U prefix
                        i->expr->getLocStart().getLocWithOffset(1);
                fexpr = clang::StringLiteral::Create(
                        instance.getASTContext(),
                        clang::StringRef(i->string.c_str(), i->string.size()),
                        clang::StringLiteral::UTF8,
                        false,
                        type,
                        loc);
            }
            llvm::SmallBitVector checked_args;
            instance.getSema().CheckFormatString(
                    fexpr ? fexpr : i->expr,
                    i->expr,
                    args,
                    desc.var_arg == 0,
                    desc.format_arg - 1,
                    desc.var_arg - 1,
                    desc.type,
                    true,
                    clang::Sema::VariadicFunction,
                    checked_args);
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
                        getFormatStrings(formats, call->getArg((*a)->getFormatIdx()-1));
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
        diagnostics.Report(arg->getLocStart(), not_literal);
    }

    clang::CompilerInstance& instance;
    clang::DiagnosticsEngine& diagnostics;
    unsigned not_literal;
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
    clang::ASTConsumer* CreateASTConsumer(clang::CompilerInstance& inst, llvm::StringRef str) override
    {
        if (function_list.empty()) {
            for (std::size_t i = 0; i < sizeof(default_function_list)/sizeof(default_function_list[0]); ++i) {
                function_list.push_back(default_function_list[i]);
            }
        }
        return new FormatASTConsumer(inst, function_list, gettext_list);
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
                            desc.type = clang::Sema::FST_Printf;
                        } else if (vec[1] == "scanf") {
                            desc.type = clang::Sema::FST_Scanf;
                        } else if (vec[1] == "NSString") {
                            desc.type = clang::Sema::FST_NSString;
                        } else if (vec[1] == "strftime") {
                            desc.type = clang::Sema::FST_Strftime;
                        } else if (vec[1] == "strfmon") {
                            desc.type = clang::Sema::FST_Strfmon;
                        } else if (vec[1] == "kprintf") {
                            desc.type = clang::Sema::FST_Kprintf;
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
