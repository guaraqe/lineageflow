This algorithm calculates derivatives using a polynomial interpolation of the data points, at each time step.
It is recommended for measurements whose noise is independent between time steps.

The first parameter is the size of the filter used for the derivation.
Larger filter sizes improve temporal resolution but reduce statistics.

The second one is the order of the derivative to calculate.
For example, for the calculation of a regular derivative, one chooses the value 1.

The last one is the order of the polynomials used for smoothing.
The larger is the order of the polynomials, the more details one is able to grasp.
A recommended value is 2.
