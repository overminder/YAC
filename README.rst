Yet Another Compiler
--------------------

- Written in Haskell
- For learning compiler backend stuffs (insn sel, reg alloc etc..).
- Mobai lunba

Usage
-----

  - Compile
  ``make``

  - See assembly result
  ``echo "(define main (lambda (argc argv) 0))" | ./bin/IRToAsm``

  - Compile and run assembly
  ``./SampleProg/cc.py ./SampleProg/test-oop.ss -r``
