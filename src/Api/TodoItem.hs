{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings, FlexibleContexts, TypeApplications #-}
module Api.TodoItem where

import Control.Monad.Trans.Class (lift)
import Database.Persist.Sqlite
import Data.Aeson
import Data.Time
import qualified Data.Text as T
import Servant

import Models
import Config

type TodoItemAPI =
       "items" :> Get '[JSON] [TodoItem]
  :<|> "item" :> Capture "item_id" TodoItemId :> Get '[JSON] (Maybe (Entity TodoItem))
  :<|> "item" :> "new" :> ReqBody '[JSON] Value :> Post '[JSON] (Maybe (Key TodoItem))
  :<|> "item" :> "update" :> Capture "item_id" TodoItemId :> ReqBody '[JSON] Value :> Post '[JSON] ()

selectTodoItems :: IO [TodoItem]
selectTodoItems = do
  todos <- runDB $ selectList [] []
  return $ (\(Entity _ u) -> u) <$> todos

insertTodoItem :: TodoItem -> IO (Key TodoItem)
insertTodoItem = runDB . insert

newtype ItemDTO = ItemDTO (T.Text, T.Text, T.Text, T.Text, [T.Text], TodoListId)

instance FromJSON ItemDTO where
  parseJSON (Object v) =
    ItemDTO <$> ((,,,,,)
      <$> v .: "name"
      <*> v .: "icon"
      <*> v .: "color"
      <*> v .: "message"
      <*> v .: "tags"
      <*> v .: "listid")
  parseJSON _ = mempty

newtype ItemIsoDTO = ItemIsoDTO TodoItem

instance FromJSON ItemIsoDTO where
  parseJSON (Object v) =
    ItemIsoDTO <$> (TodoItem
      <$> v .: "name"
      <*> v .: "createdTime"
      <*> v .: "icon"
      <*> v .: "color"
      <*> v .: "message"
      <*> v .: "tags"
      <*> v .: "listid")
  parseJSON _ = mempty

serverTodoItemAPI :: Server TodoItemAPI
serverTodoItemAPI = getAll :<|> getById :<|> postNew :<|> updateItem where
  getAll = lift selectTodoItems
  getById tid = lift $ runDB $ selectFirst [TodoItemId ==. tid] []
  postNew json = case fromJSON @ItemDTO json of
    Error err -> lift $ print err >> return Nothing
    Success (ItemDTO (name, icon, color, msg, tgs, listid)) -> do
      time <- lift getCurrentTime
      fmap Just $ lift $ insertTodoItem (TodoItem name time icon color msg tgs listid)
  updateItem tid json = case fromJSON @ItemIsoDTO json of
    Error err -> lift $ print err
    Success (ItemIsoDTO d) -> lift $ runDB $ replace tid d
