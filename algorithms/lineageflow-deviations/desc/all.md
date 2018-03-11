This algorithm calculates relative displacements and relative increments between pairs of cells.
In this case, this calculation is done for pairs of cells that are within a certain distance of each other.

Besides displacements and increments, means squared relative displacements and relative increment autocorrelations are calculated.
These quantities give indications on the nature of the process of deviation between cells.

The first parameter is the radius within which pairs of cells are going to be chosen.
We recommend a value similar to the distance between neighboring cells.

The second parameter is the inverse proportion of cells to be used for the calculation. Larger values reduce the running time of the algorithm, but give worse statistics. As an example, a value of 2 uses half of cells for statistics.
