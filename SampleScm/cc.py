#!/usr/bin/env python

from argparse import ArgumentParser
from runlib import *

parser = ArgumentParser(description='compile and run .ss file')
parser.add_argument(
        'input_file', metavar='FILE',
        help='compile the content of FILE')
parser.add_argument(
        '-o', '--output_file',
        metavar='FILE', default=path('a.out'),
        help='output to FILE')
parser.add_argument(
        '--gc',
        metavar='FLAVOR', default='moving',
        help='set gc FLAVOR (moving/generational/boehm, default=moving)')
parser.add_argument(
        '--gc_stat', action='store_true',
        help='enable gc statistic')
parser.add_argument(
        '--run_with_csi', action='store_true',
        help='use csi to run the program')
parser.add_argument(
        '-r', '--run', action='store_true',
        help='will run the program after finishing compilation')

args = parser.parse_args()

macro_defs = {}
ldflags = []
src_name = args.input_file
rts_names = map(path, ['scm_main.c', 'scm_runtime.c', 'scm_%s_gc.c' % args.gc])
cgen_name = path('test.c')
bin_name = args.output_file

macro_defs['SCM_GC_FLAVOR_%s' % args.gc.upper()] = ''
if args.gc_stat:
    macro_defs['SCM_GC_STAT'] = ''
if args.gc == 'boehm':
    ldflags += ['-lgc']

macro_defs = ['-D%s%s' % (k, '=%s' % v if v else '')
              for (k, v) in macro_defs.iteritems()]

if newer_than(src_name, cgen_name):
    with timeit('hs-compile'):
        hs_compile(src_name, cgen_name)

with timeit('gcc-compile'):
    gcc_compile(rts_names + [cgen_name], bin_name,
                cflags=['-O3', '-pg', '--std=c99'] + macro_defs,
                ldflags=ldflags)

if args.run:
    with timeit('native-run'):
        native_run(bin_name)

if args.run_with_csi:
    with timeit('csi-run'):
        out = csi_run(src_name).strip()
    print out

