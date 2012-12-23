from yactest.py.conf import run_asm, sexpr_to_asm

def test_fibo():
    ir = '''
    (define fibo
      (lambda (n)
        (if (< n 2)
            n
            (+ (%funcall fibo (- n 1))
               (%funcall fibo (- n 2))))))

    (extern run_fibo)
    (define run_fibo
      (lambda (n)
        (%funcall fibo n)))
    '''
    asm = sexpr_to_asm(ir)
    assert run_asm(asm, [], 'run_fibo', [10]) == 55

def test_get_quad():
    ir = '''
    (define some_sym 5)

    (extern get_val)
    (define get_val
      (lambda ()
        (%get-quad (%symbol-addr some_sym) 0)))
    '''
    asm = sexpr_to_asm(ir)
    assert run_asm(asm, [], 'get_val', []) == 5

def test_set_quad():
    ir = '''
    (define some_sym 5)

    (extern get_val)
    (define get_val
      (lambda ()
        (%set-quad! (%symbol-addr some_sym) 0 123)
        (%symbol-val some_sym)))
    '''
    asm = sexpr_to_asm(ir)
    assert run_asm(asm, [], 'get_val', []) == 123

