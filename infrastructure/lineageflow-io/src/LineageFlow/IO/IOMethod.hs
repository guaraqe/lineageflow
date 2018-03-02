module LineageFlow.IO.IOMethod
  (
  -- * IO
    IOMethod (..)
  ) where

-- | Database interface definition. The constraint @kg@ and @kp@ represent the
-- particular method of encoding and decoding the measurement.
data IOMethod kg kp = IOMethod
  { io_get :: forall f a . kg f a => FilePath -> IO (f a)
  , io_put :: forall f a . kp f a => FilePath -> f a -> IO ()
  }
