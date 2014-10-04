#include <string>
#include <vector>
#include <cctype>
#include "split.h"

std::vector<std::string>
Split(std::string const & str)
{
    std::string str2;
    std::vector<std::string> vec;
    std::size_t i, j;

    // Recognize '#' as beginning a comment
    i = str.find('#');
    str2 = (i == std::string::npos) ? str : str.substr(0, i);

    i = 0;
    while (i < str2.size()) {
        // Skip whitespace
        while (i < str2.size() && std::isspace(str2[i])) { ++i; }
        if (i >= str2.size()) { break; }
        // Advance j past current non-whitespace
        j = i + 1;
        while (j < str2.size() && !std::isspace(str2[j])) { ++j; }
        // Add substring to vec
        vec.push_back(str2.substr(i, j - i));
        i = j;
    }

    return vec;
}
