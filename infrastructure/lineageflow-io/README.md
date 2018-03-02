# lineageflow-io

This package defines an interface for reading and writing measurements from/to files.
The interface is done through a constraint-polymorphic record:

```haskell
data IOMethod kg kp = IOMethod
  { io_get :: forall f a . kg f a => FilePath -> IO (f a)
  , io_put :: forall f a . kp f a => FilePath -> f a -> IO ()
  }
```

The classes `kg` and `kp` are classes that allows a particular way of encoding and decoding of measurements.
For an example, see the `lineageflow-io-cbor` package.
