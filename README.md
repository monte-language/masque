# Masque

The Masque is a simple interpreter, written in Haskell, for running the Monte
programming language.

The Masque uses the same abstract Monte AST format as Typhon.

To build and run:

```
    $ cabal configure
    $ cabal build
    $ path/to/your/monte/bin/monte -c prelude.mt > prelude.ty
    $ dist/build/masque/masque someMonte.ty
```

Patches are welcome. This is a GPLv3 project.
