from yactest.py.conf import run_asm, sexpr_to_asm

def test_run_asm_noarg():
    src = '''
    .global foo
    foo:
        mov $123, %rax
        ret
    '''
    assert run_asm(src, [], 'foo', []) == 123

def test_run_asm_args():
    src = '''
    .global foo
    foo:
        lea (%rsi,%rdi), %rax
        ret
    '''
    assert run_asm(src, [], 'foo', [123, 456]) == 123 + 456

def test_run_ir_and_asm_noarg():
    ir = '''
    (extern foo)
    (define foo
      (lambda ()
        123))'''
    asm = sexpr_to_asm(ir)
    assert run_asm(asm, [], 'foo', []) == 123

def test_run_ir_and_asm_args():
    ir = '''
    (extern foo)
    (define foo
      (lambda (a b)
        (+ a b)))
    '''
    asm = sexpr_to_asm(ir)
    assert run_asm(asm, [], 'foo', [123, 456]) == 123 + 456

