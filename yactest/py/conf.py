import os
import sys
import atexit
import ctypes
import tempfile
from subprocess import Popen, PIPE
from contextlib import contextmanager

class CompileError(Exception):
    pass

HERE = os.path.dirname(os.path.abspath(__file__))
BASEDIR = os.path.normpath(os.path.join(HERE, '../../'))

def localpath(p):
    return os.path.join(BASEDIR, p)

def sexpr_to_asm(src):
    ''' param src: ir source to compile
        returns: assembler
    '''
    p = Popen([localpath('bin/SExprToAsm')],
              stdin=PIPE, stdout=PIPE, stderr=PIPE)
    out, err = p.communicate(src)
    if err:
        raise CompileError('SExpr->ASM: %s' % err)
    return out

def gen_tmp_path():
    f = tempfile.NamedTemporaryFile(delete=False)
    path = f.name
    f.close()
    @atexit.register
    def close_that():
        try:
            os.unlink(path)
        except OSError:
            pass
    return path

def run_asm(src, cflags, entrypoint, args):
    ''' param src: asm source to link
        param entrypoint: str, asm func name to run
        param args: [int], args to pass
        returns: int, function result
    '''
    tmp_path = gen_tmp_path()
    p = Popen(['gcc', '-x', 'assembler', '-fPIC', '-shared', '-', '-o',
               tmp_path] + cflags,
              stdin=PIPE, stdout=PIPE, stderr=PIPE)
    out, err = p.communicate(src)
    if err:
        raise CompileError('ASM->BIN: %s' % err)
    lib = ctypes.CDLL(tmp_path)
    func = getattr(lib, entrypoint)
    func.restype = ctypes.c_int64
    func.argtypes = [ctypes.c_int64] * len(args)
    return func(*args)

