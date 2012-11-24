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

    p = Popen([path('../bin/IRToAsm')], stdin=PIPE, stdout=PIPE, stderr=PIPE)
    out, err = p.communicate(src)
    if err:
        raise CompileError(err)
    with open(name_out, 'w') as f:
        f.write(out)

def gcc_compile(files_in, name_out, flags=[]):
    p = Popen(['gcc', '-o', name_out] + flags + files_in)
    _, err = p.communicate()
    if err:
        raise CompileError(err)

def native_run(name):
    p = Popen([name], stdout=PIPE, stderr=PIPE)
    out, err = p.communicate()
    if err:
        raise RuntimeError(err)
    return out

def csi_run(file_name):
    with open(file_name) as f:
        src = f.read()

    p = Popen(['csi', '-q'], stdin=PIPE, stdout=PIPE)
    out, _ = p.communicate('''
    (define (%%funcall proc . args)
      (apply proc args))
    (define %%funcall/t %%funcall)
    (define (putInt i)
      (display i)
      (newline))
    (define (get7thArg a b c d e f g) g)
    (define (get8thArg a b c d e f g h) h)
    %s
    (main)
    ''' % src)
    return out


