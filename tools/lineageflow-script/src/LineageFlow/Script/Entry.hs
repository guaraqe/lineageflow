module LineageFlow.Script.Entry
  (
    Entry (..)
  , _EntryFixed
  , _EntryPrefixed
  , _EntryVar
  , readEntry
  , withCard
  ) where

import LineageFlow.Declaration

import LineageFlow.Script.Var

import qualified Data.Text as Text

import Control.Lens
import Data.Aeson.Types
import Data.Text (Text)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

data Entry =
  EntryFixed Text |
  EntryPrefixed Text |
  EntryVar Var
  deriving (Eq, Show, Generic)

$(makePrisms ''Entry)

extra :: String -> String
extra = tail . dropWhile (/= '-')

instance ToJSON Entry where
  toJSON = genericToJSON $
    defaultOptions { constructorTagModifier = extra . convertConstructor }

instance FromJSON Entry where
  parseJSON = genericParseJSON $
    defaultOptions { constructorTagModifier = extra . convertConstructor }

instance HasVars Entry where
  getVars (EntryFixed _) = []
  getVars (EntryPrefixed _) = []
  getVars (EntryVar v) = [v]

readEntry :: Card -> Text -> Entry
readEntry card t =
  case Text.uncons t of
    Nothing -> EntryFixed ""
    Just ('_',var) -> EntryPrefixed var
    Just ('$',var) ->
      case Text.splitOn " : " var of
        [v,c] -> EntryVar $ Var v $ case c of
          "single" -> Single
          "optional" -> Optional
          "list" -> Many
          _ -> Single
        _ -> EntryVar (Var var card)
    Just _ -> EntryFixed t

withCard :: (Card -> a -> b) -> CardF a -> CardF b
withCard f (SingleF a) = SingleF (f Single a)
withCard f (OptionalF a) = OptionalF (fmap (f Optional) a)
withCard f (ManyF a) = ManyF (fmap (f Many) a)

