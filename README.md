![](documentation/logo.png)

`LineageFlow` is a suite of libraries, executables and tools for the manipulation and exploration of spatio-temporal cell lineages, and measurements over them.
More details on the ideas underlying this system can be found [here](https://pastel.archives-ouvertes.fr/tel-01689773).

- [Installation](#installation)
- [Structure](#structure)
- [Algorithms](#algorithms)
- [Tools](#tools)
- [Acknowledgements](#acknowledgements)

# Installation

# Usage

# Structure

The packages of the suite are organized as follows:

![](documentation/dependencies.png)

Each package has a well determined function:

- [`types`](infrastructure/lineageflow-types): types related to temporal cell lineages and measurements over it;
- [`prelude`](infrastructure/lineageflow-prelude): custom prelude for a safer manipulation of measurements;
- [`io`](infrastructure/lineageflow-io): interface for reading and writing measurements from files;
- [`io-*`](infrastructure/lineageflow-database-cbor): particular implementation of IO method;
- [`base`](infrastructure/lineageflow-base): types for the manipulation of algorithms;
- [`declaration`](infrastructure/lineageflow-declaration): types for the declaration of algorithms;
- [`algorithm`](infrastructure/lineageflow-algorithm): generation of executables from algorithm declarations;
- [`query`](infrastructure/lineageflow-query): types for querying measurements from a database;
- [`database`](infrastructure/lineageflow-database): interface for measurement database interactions;
- [`database-*`](infrastructure/lineageflow-database-sqlite): particular implementation of a measurement database;
- [`server-api`](tools/lineageflow-server-api): API for server-client interaction;
- [`server`](tools/lineageflow-server): server for querying databases, launching algorithms and running exploration tools;
- [`client`](tools/lineageflow-client): graphical interface for the server API.

# Algorithms

The following packages define algorithms both in library and executable form:

- [`tracking`](algorithms/lineageflow-tracking): generation of spatio-temporal lineages from cell trackings;
- [`statistics`](algorithms/lineageflow-statistics): global statistics for measurements;
- [`derivatives`](algorithms/lineageflow-derivatives): discrete derivatives for cell trajectories;
- [`homogenization`](algorithms/lineageflow-homogenization): homogenization of measurements on time, space, etc.;
- [`triangulations`](algorithms/lineageflow-triangulations): generation and manipulation of triangulations;
- [`trajectories`](algorithms/lineageflow-trajectories): trajectories, genealogical trajectories and path integrals;
- [`clustering`](algorithms/lineageflow-clustering): clustering of cell trajectories;
- [`deviations`](algorithms/lineageflow-deviations): statistics of deviations of neighboring cells;
- [`forces`](algorithms/lineageflow-forces): estimation of forces using least-squares methods;

# Tools

The following tools are also included in the suite:

- [`import`](tools/lineageflow-import): importing cell trackings from other file formats;
- [`export`](tools/lineageflow-export): exporting cell selections to other file formats;
- [`viewer`](tools/lineageflow-viewer): visualization of measurements in 3D+time;
- [`plot`](tools/lineageflow-plot): plot generation for standard statistics on measurements;
- [`playground`](tools/lineageflow-playground): utilities for interactive calculation on GHCi.

with the following interface packages:

- [`viewer-interface`](tools/lineageflow-viewer-interface): interface for the `lf-viewer` executable;
- [`plot-interface`](tools/lineageflow-plot-interface): interface for the `lf-plot` executable;

# Acknowledgements

This project has been developed by Juan Raphael Diaz Simões during a PhD thesis, in collaboration between the [Condensed Matter Physics Laboratory](https://pmc.polytechnique.fr/) at [École Polytechnique](http://www.polytechnique.edu/) and [CNRS](http://www.dr4.cnrs.fr/), and the [BioEmergences Laboratory](http://bioemergences.eu/bioemergences/index.php) at [CNRS](http://www.dr4.cnrs.fr/).
The project has been directed by [Denis Grebenkov](https://pmc.polytechnique.fr/pagesperso/dg/), [Paul Bourgine](https://fr.linkedin.com/in/paul-bourgine-84a4383) and [Nadine Peyriéras](http://bioemergences.eu/bioemergences/people.php).
