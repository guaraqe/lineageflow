# lineageflow-tracking

This package defines a type for cell trackings and include facilities for the
generation of spatio-temporal cell lineages from them.

## Executable

This defines the executable `lf-tracking` with the following subcommands:

- `tracking`: generates temporal lineage from tracking data;
- `restrict`: restricts cell trackings to intervals of time.

## Library

The library defines two types in the module `LineageFlow.Tracking.Types`:

- `Tracking`, representing the relation between cells at consecutive time steps;
- `LineageSet`, which is a record with four components:
    - `mothers`, relating each cell with its eventual mother;
    - `children`, relating each cell with its eventual chidren;
    - `tc`, which relates the temporal point of view to the cellular point of view of the temporal lineage;
    - `ct`, which relates the cellular point of view to the temporal point of view of the temporal lineage;

and a function:

```
lineageSetFromTracking :: DSumMap Time Cell Tracking -> LineageSet
```

which transforms the cell tracking into the more complete description.
