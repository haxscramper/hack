#!/usr/bin/env python

import sys
import clang.cindex

f = open("/tmp/file.cpp", "a")
f.write("""
#ifndef __HEADER_FOO
#define __HEADER_FOO

//+reflect
class Foo
{
    public:
    private:
        int m_int; //+reflect
};

#endif
""")


def main():
    index = clang.cindex.Index.create()
    tu = index.parse("/tmp/file.cpp", args=['-x', 'c++'])

    for x in tu.cursor.get_tokens():
        print(x.kind, str(x.spelling))

if __name__ == '__main__':
    main()
