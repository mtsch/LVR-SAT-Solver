#!/bin/bash
# Run the program on all files found in ./benchmark and check if it is satisfied.
cabal build

pushd src

PROGRAM="../dist/build/LVR-SAT-solver/LVR-SAT-solver"
CHECK="runhaskell TestValuation.hs"
DIR="../benchmark"

for f in $(ls $DIR); do
    echo $f:
    $PROGRAM $DIR/$f $DIR/out.tmp
    echo -n "  "
    $CHECK $DIR/$f $DIR/out.tmp
done

rm $DIR/out.tmp
popd
