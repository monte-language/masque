# Masque

The Masque is a simple interpreter, written in Haskell, for running the Monte
programming language.

The Masque uses the same abstract Monte AST format as Typhon.

To build and run:

```
    $ cabal sandbox init
    $ cabal configure
    $ cabal build
```

Or use Nix:

```
    $ cabal2nix --shell . > default.nix
    $ nix-build
```

Patches are welcome. This is a GPLv3 project.
