{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module LineageFlow.Client.Running.Elm
  ( running
  ) where

import LineageFlow.Client.Prelude
import LineageFlow.Client.Types

import LineageFlow.Client.Global.Model

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Text (Text)

import Data.Time.Clock

running ::
  MonadWidget t m =>
  View t (App t m) Global
running =
  decorateView (runningView <> updateView) $
    elClass "div" "running-container"

updateView ::
  MonadWidget t m =>
  View t (App t m) Global
updateView = simpleView $ \global -> do
  runn <- uniqView global_algorithmRunning global

  cur <- liftIO getCurrentTime
  tick <- fmap (const ()) <$> tickLossy 10 cur

  x <- dyn $ fmap mergeMap . Map.traverseWithKey (\n -> getUpdate tick n . pure) <$> runn

  y <- switchPromptly never x

  return $ fmap (over global_algorithmRunning . Map.unionWith const) y

getUpdate ::
  MonadWidget t m =>
  Event t () -> Int -> Dynamic t (AQuery, Status) -> App t m (Event t (AQuery, Status))
getUpdate ev n qs = do
  let
    ev2 =
      attachPromptlyDynWithMaybe
        (\(_,s) b -> if s == StatusRunning || s == StatusWaiting then Just b else Nothing)
        qs ev
  val <- runCheck (pure n) ev2
  return $ attachPromptlyDynWith (\(q,_) s -> (q,s)) qs val


runningView ::
  MonadWidget t m =>
  View t (App t m) Global
runningView = simpleView $ \model -> do
  runn <- uniqView global_algorithmRunning model

  elClass "table" "mdl-data-table mdl-js-data-table mdl-data-table--selectable mdl-shadow--2dp" $ do
    el "thead" $ showTopRow
    el "tbody" $ traverseDyn_ showStatus (fmap Map.elems runn)

  return never

--------------------------------------------------------------------------------

showTopRow ::
  MonadWidget t m => m ()
showTopRow =
  el "tr" $ do
    tdMDL "th" $ text "Executable"
    tdMDL "th" $ text "Command"
    tdMDL "th" $ text "Status"

showStatus ::
  MonadWidget t m =>
  Dynamic t (AQuery, Status) -> m ()
showStatus pair =
  let
    status = fmap (textStatus . view _2) pair
    fields = fmap (view (_1 . query_fields)) pair
    executable = fromJust . lookupAssoc "executable" <$> fields
    command = fromJust . lookupAssoc "command" <$> fields
  in
    el "tr" $ do
      tdMDL "td" $ dynText executable
      tdMDL "td" $ dynText command
      tdMDL "td" $ dynText status

tdMDL :: MonadWidget t m => Text -> m a -> m a
tdMDL x = elClass x "mdl-data-table__cell--non-numeric"


--------------------------------------------------------------------------------

textStatus :: Status -> Text
textStatus StatusNever = "Not found, should not happen"
textStatus StatusWaiting = "Waiting"
textStatus StatusRunning = "Running"
textStatus StatusSuccess = "Finished"
textStatus StatusFailed = "Failed"

traverseDyn_ ::
  MonadWidget t m =>
  (Dynamic t a -> m ()) -> Dynamic t [a] -> m ()
traverseDyn_ f x = do
  void $ dyn $ fmap (mapM_ (f . pure)) x

uniqView ::
  (Eq a, MonadWidget t m) =>
  Lens' b a -> Dynamic t b -> m (Dynamic t a)
uniqView l = holdUniqDyn . fmap (view l)
