module LineageFlow.IO.CBOR.Implementation
  ( cbor
  ) where

import LineageFlow.IO
import LineageFlow.IO.CBOR.Classes

--------------------------------------------------------------------------------

cbor :: IOMethod CBORGet CBORPut
cbor = IOMethod cborGetMeasurement cborPutMeasurement
