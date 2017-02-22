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
  icon T.Text
  color T.Text
  message T.Text
  tags [T.Text]
  deriving Show

TodoList json
  items [TodoItemId]
  deriving Show

ActivityList json
  action T.Text
  actionTime UTCTime
  entity [TodoItemId]
  deriving Show
|]

runDB :: ConnectInfo -> SqlPersistT (NoLoggingT (ResourceT IO)) a -> IO a
runDB = runSqlite

doMigration :: ConnectInfo -> IO ()
doMigration conn = runSqlite conn $ runMigration migrateAll
