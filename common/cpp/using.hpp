#include <stddef.h>

using u32 = unsigned int;
using uS  = size_t;

#ifdef _STL_VECTOR_H

template <class T>
using Vec = std::vector<T>;

#endif

#ifdef _BASIC_STRING_H

using Str = std::string;

#endif
