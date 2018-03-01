# lineageflow-declaration

This package defines the `Decl` type, which is used for the declaration of parameters, measurements and algorithms.

```haskell
data Decl a = Decl
  { decl_name :: Text
  , decl_desc :: Text
  , decl_type :: a
  }
```

It is used by the executables in order to declare what are the appropriate inputs for them.
It is also used by the client in order to generate the appropriate graphical interface for the algorithm.
