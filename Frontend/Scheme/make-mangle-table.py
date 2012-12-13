""" Just like haskell's z-encoding...
"""

from itertools import chain

def char_range(a, z):
    return map(chr, range(ord(a), ord(z) + 1))

def char_ranges(*a_to_zs):
    res = []
    for (a, _, z) in a_to_zs:
        res.append(char_range(a, z))
    return list(chain(*res))

# [Char]
extra_chars = list("!#$%&|+-*/:<=>?@^~")

# [Char]
normal_chars = char_ranges('a-z', 'A-Z', '0-9')

# [(Char, Char)]
extra_table = zip(extra_chars, normal_chars)

# [Char?]
whole_table = [None] * 128

# Delete those unviewable chars
for c in extra_chars + normal_chars:
    whole_table[ord(c)] = c

for (from_c, to_c) in extra_table:
    whole_table[ord(from_c)] = 'z' + to_c

whole_table[ord('_')] = '_'
whole_table[ord('z')] = 'zz'

print '''module Frontend.Scheme.MangleTable (
  prefix,
  table,
) where

prefix :: Char
prefix = 'z'

table :: [(Int, String)]
table = ['''

# [(Int, Char)]
whole_table = [(i, s) for (i, s) in enumerate(whole_table) if s is not None]

print ",\n".join('  (%d, "%s")' % (i, s) for (i, s) in whole_table)

print '  ]'

