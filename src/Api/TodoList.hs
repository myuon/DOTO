{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings, FlexibleContexts, TypeApplications #-}
module Api.TodoList where

import Control.Monad.Trans.Class (lift)
import Database.Persist.Sqlite
import Data.Aeson
import Data.Time
import qualified Data.Text as T
import qualified Data.List as L
import Servant

import Models
import Config

type TodoListAPI =
       "lists" :> Get '[JSON] [TodoList]
  :<|> "list" :> Capture "list_id" TodoListId :> Get '[JSON] (Maybe (Entity TodoList))
  :<|> "list" :> "new" :> ReqBody '[JSON] Value :> Post '[JSON] (Maybe TodoListId)
  :<|> "list" :> "undone" :> "add" :> Capture "list_id" TodoListId :> Capture "item_id" TodoItemId :> Post '[JSON] ()
  :<|> "list" :> "checkDone" :> Capture "list_id" TodoListId :> Capture "item_id" TodoItemId :> Post '[JSON] ()
  :<|> "list" :> "checkDelete" :> Capture "list_id" TodoListId :> Capture "item_id" TodoItemId :> Post '[JSON] ()

selectTodoLists :: IO [TodoList]
selectTodoLists = do
  lists <- runDB $ selectList [] []
  return $ (\(Entity _ u) -> u) <$> lists

insertTodoList :: TodoList -> IO ()
insertTodoList = runDB . insert_

newtype TodoListDTO = TodoListDTO UserId

instance FromJSON TodoListDTO where
  parseJSON (Object v) = TodoListDTO <$> (v .: "userid")
  parseJSON _ = mempty

serverTodoListAPI :: Server TodoListAPI
serverTodoListAPI = getAll :<|> getById :<|> postNew :<|> addUndone :<|> checkDone :<|> checkDelete where
  getAll = lift selectTodoLists
  getById tid = lift $ runDB $ selectFirst [TodoListId ==. tid] []
  postNew json = case fromJSON @TodoListDTO json of
    Error err -> lift $ print err >> return Nothing
    Success (TodoListDTO userid) -> fmap Just $ lift $ newTodoList userid
  addUndone lid tid = do
    TodoList _ und _ _ <- lift $ (\(Entity _ u) -> u) . head <$> runDB (selectList [TodoListId ==. lid] [])
    lift $ runDB $ update lid [TodoListUndone =. (tid : und)]
  checkDone lid tid = do
    TodoList ds uds _ _ <- lift $ (\(Entity _ u) -> u) . head <$> runDB (selectList [TodoListId ==. lid] [LimitTo 1])
    lift $ runDB $ update lid [TodoListUndone =. L.delete tid uds, TodoListDone =. tid : ds]
  checkDelete lid tid = do
    TodoList _ uds ds _ <- lift $ (\(Entity _ u) -> u) . head <$> runDB (selectList [TodoListId ==. lid] [LimitTo 1])
    lift $ runDB $ update lid [TodoListUndone =. L.delete tid uds, TodoListDeleted =. tid : ds]
