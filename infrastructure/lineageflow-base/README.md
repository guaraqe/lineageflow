# lineageflow-base

This package contains the base types defining *parameters*, *measurements* and *algorithms*.

## Parameters

The type for parameters is defined as follows:

```haskell
data PType = PType
  { ptype :: Text}
```

It is defined by its identifier.

## Measurements

The type for measurements is defined as follows:

```haskell
data MType = MType
  { mtype_domain :: Text
  , mtype_codomain :: Text
  }
```

It is defined by the identifiers of the *domain* and *codomain* of the measurement.

## Algorithms

The type for measurements is defined as follows:

```haskell
data AType f = AType
  { atype_parameters :: Assoc (CardF (f PType))
  , atype_inputs     :: Assoc (CardF (f MType))
  , atype_outputs    :: Assoc (CardF (f MType))
  } deriving Generic
```

As one can see, `AType` has kind `(Type -> Type) -> Type`.
This is done so that one can both *define* and *query* (i.e. execute) algorithms using the same structure.

The type `Assoc a` denotes an *association* between names and elements of type `a`.

The type `CardF a` tells if the *cardinality* of the element:

```haskell
data CardF a
  = SingleF a
  | OptionalF (Maybe a)
  | ManyF [a]
```

Putting everything together, the type of an algorithm is defined by its parameters, inputs and outputs.
In each case, the elements have both a name and a cardinality.
