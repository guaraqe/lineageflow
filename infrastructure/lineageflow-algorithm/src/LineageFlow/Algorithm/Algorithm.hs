module LineageFlow.Algorithm.Algorithm
  (
  -- * Algorithm
    Algorithm (..)
  , makeAlgorithm
  , runAlgorithm
  ) where

import LineageFlow.IO

import LineageFlow.Algorithm.Imports
import LineageFlow.Algorithm.Classes.PD
import LineageFlow.Algorithm.Classes.MD
import LineageFlow.Algorithm.Classes.P
import LineageFlow.Algorithm.Classes.I
import LineageFlow.Algorithm.Classes.O

import LineageFlow.Declaration

import Data.Proxy

data Algorithm p i o = Algorithm
  { algorithm_decl :: ADecl
  , algorithm_operation :: p -> i -> o
  }

aDecl ::
  forall p i o .
  (PD p, MD i, MD o) =>
  Text -> Text -> (p -> i -> o) -> ADecl
aDecl name desc _ =
  Decl name desc $
    AType
      (pDecl (Proxy @ p))
      (mDecl (Proxy @ i))
      (mDecl (Proxy @ o))

makeAlgorithm ::
  forall p i o .
  (PD p, MD i, MD o) =>
  Text -> Text -> (p -> i -> o) -> Algorithm p i o
makeAlgorithm name desc alg = Algorithm (aDecl name desc alg) alg

unConst :: Assoc (CardF (Const String a)) -> Assoc (CardF String)
unConst = fmap (fmap getConst)

runAlgorithm ::
  (P p, I kg i, O kp o, PD p, MD i, MD o) =>
  IOMethod kg kp -> APath -> Algorithm p i o -> IO ()
runAlgorithm
  iomethod
  (AType par inp out)
  (Algorithm _ alg) = do

  input <- iGet iomethod (unConst inp)
  let
    parameter = pGet (unConst par)
    output = alg parameter input

  oPut iomethod (unConst out) output
