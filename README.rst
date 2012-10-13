Yet Another Compiler
--------------------

- Written in Haskell
- For learning compiler backend stuffs (insn sel, reg alloc etc..).
- Mobai lunba

Usage
-----

.. code:: bash
  ghc --make Main
  ./Main "(define a 1) (define b 2) (define c (+ a b))"
