# lineageflow-query

This package defines the `Query` type, which is used for the query of parameters, measurements and algorithms.

It is used by the database in order to transform queries into inputs for algorithms.
This is the also input given by the user when launching an algorithm from the graphical interface.

The shape of the type is the following:

```haskell
data QueryWith a b = Query
  { query_fields :: Assoc a
  , query_type :: b
```

The `fields` part corresponds to a row in a database, where the names in `Assoc` correspond to the column names.
