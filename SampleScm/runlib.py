import os
import time
from subprocess import Popen, PIPE
from contextlib import contextmanager

HERE = os.path.dirname(os.path.abspath(__file__))
def path(p):
    return os.path.join(HERE, p)

class CompileError(Exception):
    pass

@contextmanager
def timeit(name):
    t0 = time.time()
    yield
    dt = time.time() - t0
    print '%s uses %.4f sec' % (name, dt)

def hs_compile(name_in, name_out):
    with open(name_in) as f:
        src = f.read()

    p = Popen([path('../bin/ScmToIR')], stdin=PIPE, stdout=PIPE, stderr=PIPE)
    out, err = p.communicate(src)
    if err:
        raise CompileError(err)
    with open(name_out, 'w') as f:
        f.write(out)

def gcc_compile(files_in, name_out, cflags=[], ldflags=[]):
    p = Popen(['gcc'] + ['-o', name_out] + cflags + files_in + ldflags,
              stderr=PIPE)
    _, err = p.communicate()
    if err:
        raise CompileError(err)

def native_run(name):
    Popen([name]).communicate()

def csi_run(file_name):
    with open(file_name) as f:
        src = f.read()

    p = Popen(['csi', '-q'], stdin=PIPE, stdout=PIPE)
    out, _ = p.communicate(src)
    return out

def newer_than(a, b):
    if os.path.isfile(b):
        return time.ctime(os.path.getctime(a)) > time.ctime(os.path.getctime(b))
    else:
        return True # doesn't exist anyway


