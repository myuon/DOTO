{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings, FlexibleContexts, TypeApplications #-}
module Api.TodoList where

import Control.Monad.Trans.Class (lift)
import Database.Persist.Sqlite
import Data.Aeson
import Data.Time
import qualified Data.Text as T
import Servant

import Models
import Config

type TodoListAPI =
       "lists" :> Get '[JSON] [TodoList]
  :<|> "list" :> Capture "todolist_id" TodoListId :> Get '[JSON] (Maybe (Entity TodoList))
  :<|> "list" :> "new" :> ReqBody '[JSON] Value :> Post '[JSON] ()

selectTodoLists :: IO [TodoList]
selectTodoLists = do
  lists <- runDB connInfo $ selectList [] []
  return $ (\(Entity _ u) -> u) <$> lists

insertTodoList :: TodoList -> IO ()
insertTodoList = runDB connInfo . insert_

newtype ListDTO = ListDTO [TodoItemId]

instance FromJSON ListDTO where
  parseJSON (Object v) = ListDTO <$> (v .: "items")
  parseJSON _ = mempty

serverTodoListAPI :: Server TodoListAPI
serverTodoListAPI = getAll :<|> getById :<|> postNew where
  getAll = lift selectTodoLists
  getById tid = lift $ runDB connInfo $ selectFirst [TodoListId ==. tid] []
  postNew json = case fromJSON @ListDTO json of
    Error err -> lift $ print err
    Success (ListDTO items) -> lift $ insertTodoList (TodoList items)
