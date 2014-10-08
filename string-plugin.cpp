// Use with:
// -Xclang -load -Xclang path/to/string-plugin.so -Xclang -add-plugin -Xclang string-plugin
// and zero or more of
// -Xclang -plugin-arg-string-plugin -Xclang -list=functions.txt

#include <fstream>
#include <string>
#include <vector>
#include <cstdlib>

#define __STDC_LIMIT_MACROS 1
#define __STDC_CONSTANT_MACROS 1
#include <clang/AST/AST.h>
#include <clang/AST/ASTConsumer.h>
#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/Basic/Version.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/FrontendPluginRegistry.h>

#include "split.h"

namespace {

struct FunctionSubst {
    std::string function;
    unsigned arg;
    std::string alternative;
};
FunctionSubst const default_function_list[] = {
    { "sprintf", 1, "snprintf" },
    { "vsprintf", 1, "vsnprintf" },
    { "strcpy", 1, "" },
    { "strcat", 1, "" },
    { "wcscpy", 1, "" },
    { "wcscat", 1, "" },
    { "gets",   1, "fgets" },
};

class StringVisitor : public clang::RecursiveASTVisitor<StringVisitor> {
public:
    explicit StringVisitor(clang::CompilerInstance& inst, std::vector<FunctionSubst> const & functions) :
    diagnostics(inst.getDiagnostics())
    {
#if CLANG_VERSION_MAJOR < 3 || (CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR < 5)
        auto const arg_msg_level = clang::DiagnosticsEngine::Warning;
#else
        auto const arg_msg_level = clang::DiagnosticsEngine::Remark;
#endif
        arg_is_array = diagnostics.getCustomDiagID(arg_msg_level, "argument %0 to '%1' is an array");
        arg_is_array_with_alt = diagnostics.getCustomDiagID(arg_msg_level, "argument %0 to '%1' is an array; suggest conversion to %2");
        arg_is_pointer = diagnostics.getCustomDiagID(arg_msg_level, "argument %0 to '%1' is something other than an array");
        suspicious_sizeof = diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning, "suspicious use of sizeof(x)/sizeof(y)");
        for (auto fs = functions.begin(); fs != functions.end(); ++fs) {
            function_list.push_back(*fs);
        }
    }

    bool VisitCallExpr(clang::CallExpr *exp)
    {
        clang::FunctionDecl *d = exp->getDirectCallee();
        if (d == nullptr) { return true; }

        // Determine whether the function is one we're looking for
        auto ident = d->getIdentifier();
        if (ident == nullptr) { return true; }
        llvm::StringRef const name = ident->getName();
        for (auto fs = function_list.begin(); fs != function_list.end(); ++fs) {
            // A function may appear more than once on the list, to check
            // more than one argument
            if (name == fs->function) {
                clang::Expr *arg = exp->getArg(fs->arg-1);
                if (arg == nullptr) { continue; }
                arg = arg->IgnoreImplicit();
                const clang::Type *type = arg->getType().getTypePtrOrNull();
                if (type != nullptr && type->isArrayType()) {
                    if (!fs->alternative.empty()) {
                        diagnostics.Report(arg->getLocStart(), arg_is_array_with_alt) << fs->arg << name << fs->alternative;
                    } else {
                        diagnostics.Report(arg->getLocStart(), arg_is_array) << fs->arg << name;
                    }
                } else {
                    diagnostics.Report(arg->getLocStart(), arg_is_pointer) << fs->arg << name;
                }
            }
        }

        return true;
    }

    bool VisitBinaryOperator(clang::BinaryOperator *op)
    {
        // Looking for divide operator
        if (op->getOpcode() != clang::BO_Div) { return true; }

        // Looking for sizeof(x) / sizeof(y)
        clang::Expr *lhs = op->getLHS();
        clang::Expr *rhs = op->getRHS();
        clang::UnaryExprOrTypeTraitExpr *lhs_u = clang::dyn_cast_or_null<clang::UnaryExprOrTypeTraitExpr>(lhs);
        clang::UnaryExprOrTypeTraitExpr *rhs_u = clang::dyn_cast_or_null<clang::UnaryExprOrTypeTraitExpr>(rhs);
        if (lhs_u == nullptr) { return true; }
        if (rhs_u == nullptr) { return true; }
        if (lhs_u->getKind() != clang::UETT_SizeOf) { return true; }
        if (rhs_u->getKind() != clang::UETT_SizeOf) { return true; }

        // Report this expression if x is a pointer
        const clang::Type *x_type = lhs_u->getTypeOfArgument().getTypePtrOrNull();
        if (x_type == nullptr) { return true; }
        if (!x_type->isPointerType()) { return true; }

        diagnostics.Report(op->getLocStart(), suspicious_sizeof);
        return true;
    }

private:
    clang::DiagnosticsEngine& diagnostics;
    unsigned arg_is_array;
    unsigned arg_is_array_with_alt;
    unsigned arg_is_pointer;
    unsigned suspicious_sizeof;
    std::vector<FunctionSubst> function_list;
};

struct StringASTConsumer : public clang::ASTConsumer {
public:
    explicit StringASTConsumer(clang::CompilerInstance& inst, std::vector<FunctionSubst> const & functions)
    : Visitor(inst, functions) {}

    virtual void HandleTranslationUnit(clang::ASTContext &Context)
    {
        Visitor.TraverseDecl(Context.getTranslationUnitDecl());
    }
private:
    StringVisitor Visitor;
};

class StringAction : public clang::PluginASTAction {
public:
    clang::ASTConsumer* CreateASTConsumer(clang::CompilerInstance& inst, llvm::StringRef str) override
    {
        if (function_list.empty()) {
            for (std::size_t i = 0; i < sizeof(default_function_list)/sizeof(default_function_list[0]); ++i) {
                function_list.push_back(default_function_list[i]);
            }
        }
        return new StringASTConsumer(inst, function_list);
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
                    if (vec.size() < 2) { continue; }

                    FunctionSubst subst;
                    subst.function = vec[0];
                    subst.arg = std::strtoul(vec[1].c_str(), nullptr, 10);
                    if (vec.size() > 2) { subst.alternative = vec[2]; }

                    function_list.push_back(subst);
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
    std::vector<FunctionSubst> function_list;
};

}

static clang::FrontendPluginRegistry::Add<StringAction>
X("string-plugin", "assistance for converting calls to unbounded string functions");
