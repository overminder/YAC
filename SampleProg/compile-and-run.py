#!/usr/bin/env python

import re
import sys
import time
from contextlib import contextmanager
from subprocess import Popen, PIPE

@contextmanager
def timeit(name):
    t0 = time.time()
    yield
    dt = time.time() - t0
    print '%s uses %.4f sec' % (name, dt)

if len(sys.argv) != 2:
    print 'usage: %s [source-file]' % sys.argv[0]
    sys.exit(1)

with open(sys.argv[1]) as f:
    src = f.read()

with timeit('hs-compile'):
    p = Popen(['../Main'], stdin=PIPE, stdout=PIPE)
    out, _ = p.communicate(src)

with open('test.S', 'w') as f:
    f.write(out)

p = Popen(['gcc', 'main.c', 'test.S'])
p.communicate()

with timeit('native-run'):
    p = Popen(['./a.out'])
    p.communicate()

with timeit('csi-run'):
    p = Popen(['csi'], stdin=PIPE, stdout=PIPE)
    out, _ = p.communicate('''
    (display "csi-result => ")
    (display (begin %s))
    (newline)
    ''' % src)

for line in out.split('\n'):
    if line.startswith('csi-result'):
        print line

