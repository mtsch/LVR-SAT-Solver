#!/bin/bash
# Run the program on all files found in ./benchmark and check if it is satisfied.
cabal build

pushd src

SOLVE="../dist/build/LVR-SAT-solver/LVR-SAT-solver"
CHECK="runhaskell TestValuation.hs"
DIR="../benchmark"

for f in $(ls $DIR); do
    echo $f:
    time $SOLVE $DIR/$f $DIR/out.tmp
    $CHECK $DIR/$f $DIR/out.tmp
    echo
done

rm $DIR/out.tmp
popd
