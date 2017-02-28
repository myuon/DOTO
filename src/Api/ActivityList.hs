{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings, FlexibleContexts, TypeApplications #-}
module Api.ActivityList where

import Control.Monad.Trans.Class (lift)
import Database.Persist.Sqlite
import Data.Aeson
import Data.Time
import qualified Data.Text as T
import Servant

import Models
import Config

type ActivityListAPI =
       "activities" :> Get '[JSON] [ActivityList]
  :<|> "activity" :> Capture "activity_id" ActivityListId :> Get '[JSON] (Maybe (Entity ActivityList))
  :<|> "activity" :> "new" :> ReqBody '[JSON] Value :> Post '[JSON] ()

selectActivityLists :: IO [ActivityList]
selectActivityLists = do
  lists <- runDB $ selectList [] []
  return $ (\(Entity _ u) -> u) <$> lists

insertActivityList :: ActivityList -> IO ()
insertActivityList = runDB . insert_

newtype ActivityListDTO = ActivityListDTO (Action, [TodoItemId], UserId)

instance FromJSON ActivityListDTO where
  parseJSON (Object v) =
    ActivityListDTO <$> ((,,)
      <$> (read @Action <$> (v .: "action"))
      <*> v .: "entity"
      <*> v .: "userid")
  parseJSON _ = mempty

serverActivityListAPI :: Server ActivityListAPI
serverActivityListAPI = getAll :<|> getById :<|> postNew where
  getAll = lift selectActivityLists
  getById tid = lift $ runDB $ selectFirst [ActivityListId ==. tid] []
  postNew json = case fromJSON @ActivityListDTO json of
    Error err -> lift $ print err
    Success (ActivityListDTO (action, items, userid)) -> do
      time <- lift getCurrentTime
      lift $ insertActivityList (ActivityList (T.pack $ show action) time items userid)
