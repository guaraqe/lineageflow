{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module LineageFlow.Client.ScriptRun.Elm
  ( scriptRun
  ) where

import LineageFlow.Client.Prelude
import LineageFlow.Client.Types
import LineageFlow.Client.Global.Model
import LineageFlow.Client.ScriptRun.Model
import LineageFlow.Client.ScriptRun.View
import LineageFlow.Client.Widget.MDL
import qualified Data.Map.Strict as Map

scriptRun :: MonadWidget t m => View t (App t m) Global
scriptRun = mconcat
  [ changeViewUniq global_scriptRun scriptNameView
  , buttonView
  , changeViewUniq global_scriptRun scriptVarsView
  , changeViewUniq global_scriptRun scriptEnvView
  ]


buttonView :: MonadWidget t m => View t (App t m) Global
buttonView = simpleView $ \model -> do
  env <- uniqView (global_scriptRun . scriptRun_env) model
  name <- uniqView (global_scriptRun . scriptRun_choice) model
  button <- materialButton "Run"
  nums <- may (\n -> runScriptEnv n env button) name

  let
    f = Map.fromList . fmap (\(n,q) -> (n,(q,StatusWaiting)))

  return $ fmap (over global_algorithmRunning . Map.union . f) nums

may ::
  MonadWidget t m =>
  (Dynamic t a -> m (Event t b)) -> Dynamic t (Maybe a) -> m (Event t b)
may f m =
  fmap switchPromptlyDyn $
  widgetHold (pure never) $
  fmap (f . pure) $
  fmapMaybe id (updated m)

changed :: (MonadWidget t m, Eq a) => Dynamic t a -> m (Event t ())
changed x = fmap (const ()) . updated <$> holdUniqDyn x

uniqView ::
  (Eq a, MonadWidget t m) =>
  Lens' b a -> Dynamic t b -> m (Dynamic t a)
uniqView l = holdUniqDyn . fmap (view l)
