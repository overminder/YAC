#!/usr/bin/env python

import random

MAX_DEPTH = 15

def rand_nat(depth):
    return random.randint(1, 65535)

def rand_expr(depth):
    depth += 1
    if depth > MAX_DEPTH:
        f = rand_nat
    else:
        f = random.choice([rand_if, rand_add, rand_nat])
    return f(depth)

def rand_if(depth):
    return '(if %s %s %s)' % (rand_expr(depth), rand_expr(depth),
            rand_expr(depth))

def rand_add(depth):
    return '(+ %s %s)' % (rand_expr(depth), rand_expr(depth))

print rand_expr(0)
