module LineageFlow.IO.CBOR.Implementation
  ( cborDatabase
  ) where

import LineageFlow.IO
import LineageFlow.IO.CBOR.Classes

--------------------------------------------------------------------------------

cborDatabase :: IOMethod CBORGet CBORPut
cborDatabase = IOMethod cborGetMeasurement cborPutMeasurement
