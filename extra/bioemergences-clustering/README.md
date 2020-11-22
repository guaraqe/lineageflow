# Bioemergences Clustering

A simplified executable for performing clustering of cell trajectories. Inputs
must be a CSV file with the following columns:

```
id_center
id_mother
x
y
z
timestep
```

the output with contain the calculated selections (clusters) corresponding to
each cell.

## Building

The executable can be built from the root directory:

```
$ make bioemergences-clustering
```

and the Docker image containing the executable can be built with:

```
$ make bioemergences-clustering-docker
```
