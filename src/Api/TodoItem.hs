{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings, FlexibleContexts, TypeApplications #-}
module Api.TodoItem where

import Control.Lens
import Control.Monad.Trans.Class (lift)
import Database.Persist.Sqlite
import Data.Aeson
import Data.Time
import qualified Data.Text as T
import Servant

import Models
import Config

type TodoItemAPI =
       "item" :> "get" :> Get '[JSON] [TodoItem]
  :<|> "item" :> "new" :> ReqBody '[JSON] Value :> Post '[JSON] ()

selectTodoItems :: IO [TodoItem]
selectTodoItems = do
  todos <- runDB connInfo $ selectList [] []
  return $ (\(Entity _ u) -> u) <$> todos

insertTodoItem :: TodoItem -> IO ()
insertTodoItem = runDB connInfo . insert_

newtype ItemDTO = ItemDTO (T.Text , T.Text)

instance FromJSON ItemDTO where
  parseJSON (Object v) = ItemDTO <$> ((,) <$> v .: "name" <*> v .: "message")
  parseJSON _ = mempty

serverTodoItemAPI :: Server TodoItemAPI
serverTodoItemAPI = getTodoItems :<|> postTodoItem where
  getTodoItems = lift selectTodoItems
  postTodoItem json = case fromJSON @ItemDTO json of
    Error err -> lift $ print err
    Success (ItemDTO (n,m)) -> do
      time <- lift getCurrentTime
      lift $ insertTodoItem (TodoItem n time "" "" m [])
