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
  :<|> "item" :> Capture "todoitem_id" TodoItemId :> Get '[JSON] (Maybe (Entity TodoItem))
  :<|> "item" :> "new" :> ReqBody '[JSON] Value :> Post '[JSON] ()

selectTodoItems :: IO [TodoItem]
selectTodoItems = do
  todos <- runDB connInfo $ selectList [] []
  return $ (\(Entity _ u) -> u) <$> todos

insertTodoItem :: TodoItem -> IO ()
insertTodoItem = runDB connInfo . insert_

newtype ItemDTO = ItemDTO (T.Text, T.Text)

instance FromJSON ItemDTO where
  parseJSON (Object v) = ItemDTO <$> ((,) <$> v .: "name" <*> v .: "message")
  parseJSON _ = mempty

serverTodoItemAPI :: Server TodoItemAPI
serverTodoItemAPI = getAll :<|> getById :<|> postNew where
  getAll = lift selectTodoItems
  getById tid = lift $ runDB connInfo $ selectFirst [TodoItemId ==. tid] []
  postNew json = case fromJSON @ItemDTO json of
    Error err -> lift $ print err
    Success (ItemDTO (n,m)) -> do
      time <- lift getCurrentTime
      lift $ insertTodoItem (TodoItem n time "" "" m [])
