{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module LineageFlow.Client.ScriptRun.View
  ( scriptNameView
  , scriptVarsView
  , scriptEnvView
  ) where

import LineageFlow.Client.Prelude
import LineageFlow.Client.Types
import LineageFlow.Client.ScriptRun.Model
import LineageFlow.Client.Widget.Common
import LineageFlow.Client.Widget.Dropdown
import LineageFlow.Client.Widget.MDL

scriptNameView :: MonadWidget t m => View t (App t m) ScriptRunModel
scriptNameView = simpleView $ \model -> elClass "div" "script-choice" $ do
  scriptList <- getScriptList
  scriptChoice <- dropdownType Fixed "" "script" scriptList

  return $ fmap (set scriptRun_choice) scriptChoice

scriptVarsView :: MonadWidget t m => View t (App t m) ScriptRunModel
scriptVarsView = simpleView $ \model -> do
  scriptName <- fmap (fmap $ fromMaybe "") $ uniqView scriptRun_choice model
  vars <- getScript scriptName =<< changed scriptName
  return $ fmap (set scriptRun_vars) vars

scriptEnvView :: MonadWidget t m => View t (App t m) ScriptRunModel
scriptEnvView = simpleView $ \model -> elClass "div" "env-choice" $ do
  input <- uniqView scriptRun_vars model
  prefix <- view textInput_value <$> materialTextInput "prefix" "" "prefix"
  envVars <- traverseDyn varChoice $ fmap (view scriptInput_vars) $ input
  envLoop <- traverseDyn loopChoice $ fmap (view scriptInput_loop) $ input

  let env = Env <$> prefix <*> (fmap Assoc envVars) <*> (fmap Assoc envLoop)

  return $ fmap (set scriptRun_env) (updated env)

--------------------------------------------------------------------------------

varChoice :: MonadWidget t m => Var -> m (Dynamic t (Text,CardF Text))
varChoice (Var name card) = do
  let
    f Single = SingleF ()
    f Many = ManyF [()]
    f Optional = OptionalF (Just ())

    txt = view textInput_value <$> materialTextInput name "" name

  cardf <- someContainer (const txt) (const name) (const "") (f card)

  return $ fmap (\x -> (name, x)) cardf

loopChoice :: MonadWidget t m => Var -> m (Dynamic t (Text, [CardF Text]))
loopChoice (Var name card) = do
  let
    f Single = SingleF ()
    f Many = ManyF [()]
    f Optional = OptionalF (Just ())

    txt = view textInput_value <$> materialTextInput name "" name

  cardf <- containerList $
    someContainer (const txt) (const name) (const "") (f card)

  return $ fmap (\x -> (name,x)) cardf

--------------------------------------------------------------------------------

changed :: (MonadWidget t m, Eq a) => Dynamic t a -> m (Event t ())
changed x = fmap (const ()) . updated <$> holdUniqDyn x

uniqView ::
  (Eq a, MonadWidget t m) =>
  Lens' b a -> Dynamic t b -> m (Dynamic t a)
uniqView l = holdUniqDyn . fmap (view l)
