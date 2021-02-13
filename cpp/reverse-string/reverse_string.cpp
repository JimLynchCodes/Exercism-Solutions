#include "reverse_string.h"
#include <algorithm>

namespace reverse_string
{

using std::reverse;
using std::string;

string reverse_string(string str)
{
    reverse(str.begin(), str.end());
    return str;
}

} // namespace reverse_string
