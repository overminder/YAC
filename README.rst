Yet Another Compiler
--------------------

- Written in Haskell
- For learning compiler backend stuffs (insn sel, reg alloc etc..).
- Mobai lunba

Usage
-----

  - Compile
  ``ghc --make Main``

  - See assembly result
  ``echo "(define main (lambda (argc argv) 0))" | ./Main``

  - Compile and run assembly
  ``./SampleProg/cc.py ./SampleProg/test-if.ss -r``
