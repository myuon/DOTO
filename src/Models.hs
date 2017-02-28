{-# LANGUAGE QuasiQuotes, TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, ExistentialQuantification, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Models where

import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson
import Database.Persist.TH
import Database.Persist.Sql
import Database.Persist.Sqlite
import qualified Data.Text as T
import Data.Time
import Config

data Action = New | Done | Delete deriving (Eq, Ord, Read, Show)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
TodoItem json
  name T.Text
  createdTime UTCTime
  due UTCTime Maybe
  icon T.Text
  color T.Text
  message T.Text
  tags [T.Text]
  listid TodoListId
  deriving Show

ActivityList json
  action T.Text
  actionTime UTCTime
  entity [TodoItemId]
  userid UserId
  deriving Show

TodoList json
  done [TodoItemId]
  undone [TodoItemId]
  deleted [TodoItemId]
  userid UserId
  deriving Show

User json
  name T.Text
  email T.Text
  list [TodoListId]
  activities [ActivityListId]
  deriving Show
|]

runDB :: SqlPersistT (NoLoggingT (ResourceT IO)) a -> IO a
runDB = runSqlite connInfo

doMigration :: IO ()
doMigration = runSqlite connInfo $ runMigration migrateAll

newTodoList :: UserId -> IO TodoListId
newTodoList uid = runDB $ insert $ TodoList [] [] [] uid

newUser :: T.Text -> T.Text -> IO UserId
newUser name email = do
  uid <- runDB $ insert $ User name email [] []
  tid <- newTodoList uid
  runDB $ update uid [UserList =. [tid]]
  return uid
