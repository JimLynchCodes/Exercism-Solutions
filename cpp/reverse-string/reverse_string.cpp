#include "reverse_string.h"
#include <algorithm>

using std::reverse;

namespace reverse_string
{

string reverse_string(string str)
{
    reverse(str.begin(), str.end());
    return str;
}

} // namespace reverse_string
