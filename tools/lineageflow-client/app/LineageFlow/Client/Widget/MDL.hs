{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module LineageFlow.Client.Widget.MDL
  ( materialTextInput
  , materialButton
  , materialDropdown
  , mdlConf
  ) where

import Reflex.Dom
import GHCJS.DOM.Element (toElement)

import qualified GHCJS.DOM.Types as GDT
import qualified GHCJS.Types as GT
import qualified Data.Map.Lazy as Map
import qualified Data.Bimap as Bimap
import qualified Data.Text as T
import qualified Text.Read as T
import Data.Text (Text)
import Control.Monad.IO.Class
import Data.Maybe
import Data.Monoid
import Control.Monad
import Data.Functor

import Control.Lens hiding ((#))

mdlConf :: (Reflex t, Ord k, Show k, Read k) => Text -> DropdownConfig t k
mdlConf name = DropdownConfig
  never
  (constDyn . Map.fromList $
    [ ("class","mdl-selectfield__select")
    , ("id",name)
    ]
  )

foreign import javascript unsafe "componentHandler.upgradeElement($1);"
  materialInitJS :: GT.JSVal -> IO ()

materialInitialize el = do
  let jsel = GDT.unElement $ toElement $ el
  pb <- getPostBuild
  performEvent_ $ (liftIO $ materialInitJS jsel) <$ pb

materialTextInput domId initial label = do
  (container, t) <- elAttr' "div" (Map.singleton "class" "mdl-textfield mdl-js-textfield mdl-textfield--floating-label") $ do
    let attrMap = Map.fromList [("class", "mdl-textfield__input"), ("id", domId)] :: Map.Map Text Text
    t <- textInput $ def & textInputConfig_attributes .~ (constDyn attrMap) & textInputConfig_initialValue .~ initial
    elAttr "label" (Map.fromList [("class", "mdl-textfield__label"), ("for", domId)]) $ text label
    return t

  materialInitialize $ _element_raw container
  return t


materialButton s = do
  (e, _) <- elAttr' "button" (Map.singleton "class" "mdl-button mdl-js-button mdl-button--raised") $ text s
  materialInitialize $ _element_raw e
  return $ domEvent Click e

materialDropdown name k0 options (DropdownConfig setK attrs) = do
  (ell,ret) <- elAttr' "div" (Map.singleton "class"  "mdl-selectfield mdl-js-selectfield mdl-selectfield--floating-label") $ do
    optionsWithAddedKeys <- fmap (zipDynWith Map.union options) $ foldDyn Map.union (k0 =: "") $ fmap (=: "") setK
    defaultKey <- holdDyn k0 setK
    let (indexedOptions, ixKeys) = splitDynPure $ ffor optionsWithAddedKeys $ \os ->
          let xs = fmap (\(ix, (k, v)) -> ((ix, k), ((ix, k), v))) $ zip [0::Int ..] $ Map.toList os
          in (Map.fromList $ map snd xs, Bimap.fromList $ map fst xs)
    modifyAttrs <- dynamicAttributesToModifyAttributes attrs
    let cfg = def
          & selectElementConfig_elementConfig . elementConfig_modifyAttributes .~ fmap mapKeysToAttributeName modifyAttrs
          & selectElementConfig_setValue .~ fmap (T.pack . show) (attachPromptlyDynWithMaybe (flip Bimap.lookupR) ixKeys setK)
    (eRaw, _) <- selectElement cfg $ listWithKey indexedOptions $ \(ix, k) v -> do
       let optionAttrs = fmap (\dk -> "value" =: T.pack (show ix) <> if dk == k then "selected" =: "selected" else mempty) defaultKey
       elDynAttr "option" optionAttrs $ dynText v
    let lookupSelected ks v = do
          key <- T.readMaybe $ T.unpack v
          Bimap.lookup key ks
    let eChange = attachPromptlyDynWith lookupSelected ixKeys $ _selectElement_change eRaw
    let readKey keys mk = fromMaybe k0 $ do
          k <- mk
          guard $ Bimap.memberR k keys
          return k
    dValue <- fmap (zipDynWith readKey ixKeys) $ holdDyn (Just k0) $ leftmost [eChange, fmap Just setK]
    (ee, _) <- elAttr' "label" (Map.fromList [("class","mdl-selectfield__label"),("for",name)]) $ text name
    materialInitialize $ _selectElement_raw eRaw
    materialInitialize $ _element_raw ee
    return $ Dropdown dValue (attachPromptlyDynWith readKey ixKeys eChange)
  materialInitialize $ _element_raw ell
  return ret

