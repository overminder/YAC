#!/usr/bin/env python

from argparse import ArgumentParser
from runlib import *

parser = ArgumentParser(description='compile and run .ss file')
parser.add_argument(
        'input_file', metavar='FILE',
        help='compile the content of FILE')
parser.add_argument(
        '-o', '--output_file',
        metavar='FILE', default='a.out',
        help='output to FILE')
parser.add_argument(
        '--run_with_csi', action='store_true',
        help='use csi to run the program')
parser.add_argument(
        '-r', '--run', action='store_true',
        help='will run the program after finishing compilation')

args = parser.parse_args()

src_name = args.input_file
lib_name = 'lib.c'
asm_name = 'test.S'
bin_name = args.output_file

with timeit('hs-compile'):
    hs_compile(src_name, asm_name)

gcc_compile([lib_name, asm_name], bin_name)

if args.run:
    with timeit('native-run'):
        out = native_run(bin_name).strip()
    print out

if args.run_with_csi:
    with timeit('csi-run'):
        out = csi_run(src_name).strip()
    print out

