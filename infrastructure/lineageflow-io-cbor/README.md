# lineageflow-io-cbor

This package defines an IO method for LineageFlow measurements using CBOR files.
It is a thin wrapper around the `serialise` package.
The database has type:

```haskell
cborDatabase :: IOMethod CBORGet CBORPut
```
