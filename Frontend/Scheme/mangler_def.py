#!/usr/bin/env python

""" Just like haskell's z-encoding...

    Usage: this -> output the table
           this str -> mangled string
"""

import sys
from itertools import chain

def char_range(a, z):
    return map(chr, range(ord(a), ord(z) + 1))

def char_ranges(*a_to_zs):
    res = []
    for (a, _, z) in a_to_zs:
        res.append(char_range(a, z))
    return list(chain(*res))

# [Char]
extra_chars = list("!#$%&|+-*/:<=>?@^~.'`()[]")

# [Char]
normal_chars = char_ranges('a-z', 'A-Z', '0-9')

# [(Char, Char)]
extra_table = zip(extra_chars, normal_chars)

# [Char?]
mangle_table = [None] * 128

# Delete those unviewable chars
for c in extra_chars + normal_chars:
    mangle_table[ord(c)] = c

for (from_c, to_c) in extra_table:
    mangle_table[ord(from_c)] = 'z' + to_c

mangle_table[ord('_')] = '_'
mangle_table[ord('z')] = 'zz'

demangle_table = range(128)
for fr_c, to_c in enumerate(mangle_table):
    if to_c is not None and len(to_c) == 2:
        demangle_table[ord(to_c[1])] = fr_c

if __name__ == '__main__':
    if len(sys.argv) == 1: # print table
        print '''module Frontend.Scheme.MangleTable (
  prefix,
  table,
) where

prefix :: Char
prefix = 'z'

table :: [(Int, String)]
table = ['''

        # [(Int, Char)]
        mangle_table = [(i, s) for (i, s) in enumerate(mangle_table) if s is not None]

        print ",\n".join('  (%d, "%s")' % (i, s) for (i, s) in mangle_table)

        print '  ]'
    else:
        s = sys.argv[1]
        print '"%s"' % ''.join(mangle_table[ord(c)] for c in s)

