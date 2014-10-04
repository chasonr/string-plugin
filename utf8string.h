// Convert a clang::StringLiteral to a UTF-8 representation as a std::string

#ifndef UTF8STRING_H
#define UTF8STRING_H

#include <string>
#include <clang/AST/Expr.h>

std::string stringToUTF8(clang::StringLiteral const *str);

#endif
