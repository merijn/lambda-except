# Building

## With Cabal

```
cabal sandbox init
cabal install
```

## With Stack

```
stack build
```

# Running the interpreter/REPL

So the binary is in your path:

```
stack install
lambda-except test.lambda
```

# Exiting the interpreter

Ctrl-C

# References

- Introduction to Generalized Type Systems; by Henk Barendregt. http://www.diku.dk/hjemmesider/ansatte/henglein/papers/barendregt1991.pdf

- Proofs and Types; Girard, Lafont, and Taylor. http://www.paultaylor.eu/stable/prot.pdf
