# lineageflow-derivatives

This package implements different differentiation methods for discrete cell trajectories.

## Executable

This defines the executable `lf-derivatives` with the following subcommands:

- `simple`: naive derivatives;
- `holoborodko`: derivatives suppressing high frequencies;
- `lanczos`: derivatives with polynomial smoothing.

## Library

This library defines in the module `LineageFlow.Derivatives.Types` the `Deriver` type, together with multiple helper functions:

```
deriveS :: Deriver -> Array t Scalar -> Array t Scalar
deriveV :: Deriver -> Array t Vector -> Array t Vector

tcDeriveS :: TC -> CT -> Deriver -> TCMap Scalar -> TCMap Scalar
tcDeriveV :: TC -> CT -> Deriver -> TCMap Vector -> TCMap Vector
```

In the other `LineageFlow.Derivatives.*` modules, particular derivers are defined:

- `simpleFw :: Int -> Deriver`
- `holo :: Int -> Deriver`
- `lanczos :: Int -> Int -> Int -> Int -> Deriver`

whose documentation can be found in their respective modules.
