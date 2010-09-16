

# A haskell investigation into Nonograms

## Solving

Solver.hs contains a combination of constraint propagation. For some problems, that runs out of steam, and then it falls back on simple search.

## Test boards

benchmark.hs contains a series of example boards gathered from the interwebs, and a Criterion benchmark to put together all the solvable ones.
