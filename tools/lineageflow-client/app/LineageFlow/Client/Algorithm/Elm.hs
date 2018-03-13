{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module LineageFlow.Client.Algorithm.Elm
  ( algorithm
  ) where

import LineageFlow.Client.Prelude
import LineageFlow.Client.Types
import LineageFlow.Client.Widget.MDL
import LineageFlow.Client.Widget.Dropdown
import LineageFlow.Client.Algorithm.View
import LineageFlow.Client.Global.Model
import LineageFlow.Client.Algorithm.Model
import qualified Data.Map.Strict as Map

algorithm ::
  MonadWidget t m => View t (App t m) Global
algorithm =
  decorateView
   (runnerView <> queryView <> changeViewUniq global_algorithmModel algorithmView) $
   elClass "div" "algorithm-container"

queryView ::
  MonadWidget t m =>
  View t (App t m) Global
queryView = simpleView $ \model -> do
  amodel <- uniqView global_algorithmModel model

  signal <- delay 0.5 $ fmap getAQuery $ updated amodel

  return $ fmap (set global_algorithm) signal

uniqView ::
  (Eq a, MonadWidget t m) =>
  Lens' b a -> Dynamic t b -> m (Dynamic t a)
uniqView l = holdUniqDyn . fmap (view l)

runnerView ::
  MonadWidget t m =>
  View t (App t m) Global
runnerView = simpleView $ \model -> elClass "div" "algorithm-button" $ do
  let
    executable = fmap (view (global_algorithmModel . amodel_executable)) model
    command = fmap (view (global_algorithmModel . amodel_command)) model
    query = fmap (view global_algorithm) model

  runnerButton <- materialButton "Run"

  runnerDyn <- (delay 0.5 =<<) $
    attachPromptlyDyn query <$>
      may (\t -> runAlgorithm t runnerButton) query

  return $
    ffor runnerDyn $
      \(qu,n) ->
        case qu of
          Nothing -> id
          Just q -> over global_algorithmRunning (Map.insert n (q, StatusRunning))

may ::
  MonadWidget t m =>
  (Dynamic t a -> m (Event t b)) -> Dynamic t (Maybe a) -> m (Event t b)
may f m =
  fmap switchPromptlyDyn $
  widgetHold (pure never) $
  fmap (f . pure) $
  fmapMaybe id (updated m)

defDecl = Decl "" "" (AType mempty mempty mempty)

defQuery = Query mempty (AType mempty mempty mempty)

toButton :: Reflex t => Dynamic t a -> Event t ()
toButton = fmap (const ()) . updated

mayList :: Maybe [a] -> [a]
mayList Nothing = []
mayList (Just l) = l
