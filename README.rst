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
  ``echo "(define a 1) (define b 2) (define c (+ a b))" | ./Main``

  - Run assembly (require csi)
  ``./SampleProg/compile-and-run.py ./SampleProg/test-if.ss``
