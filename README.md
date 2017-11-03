# LVR SAT SOLVER

Simple SAT solver implemented in Haskell.

## Building and running

Bulding this project requires Cabal. Build it by running `cabal build` in the
root directory. This creates an executable called `LVR-SAT-solver` in
`dist/build/LVR-SAT-solver/`.

To run the program, run `./LVR-SAT-solver infile outfile`. This will read the
`infile` in DIMACS cnf format, solve it and write the result to `outfile`.

This repository also includes a testing module `src/TestValuation.hs` that
checks if a valuation satisfies a formula. It is intended to be run by running
`runhaskell TestValuation.hs formula result` from the `src` directory or by
running the `runtests.sh` scripts, that tests the program against all files in
the `benchmark` directory.

## Implementation details

A formula is represented by a list of clauses represented by sets of literals. I
decided to use sets because assignments can be done quickly with set operations.

The program uses a variant of the DPLL algorithm without heuristics or pure
literal elimination. I tried implementing these optimizations, but it turned out
that searching for pure literals or the right literal to assign made the program
run slower on large test files.

Unit clause assignments are done in a single pass, so the solver should work
best with files that contain lots of short clauses.
