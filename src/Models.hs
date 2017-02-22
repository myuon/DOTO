{-# LANGUAGE QuasiQuotes, TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, ExistentialQuantification, GeneralizedNewtypeDeriving #-}
module Models where

import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Control.Monad.Reader
import Database.Persist.TH
import Database.Persist.Sql
import Database.Persist.Sqlite
import qualified Data.Text as T
import Data.Time
import Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
TodoItem json
    name T.Text
    createdTime UTCTime
    icon T.Text
    color T.Text
    message T.Text
    tags [T.Text]
    deriving Show
|]

runDB :: ConnectInfo -> SqlPersistT (NoLoggingT (ResourceT IO)) a -> IO a
runDB = runSqlite

doMigration :: ConnectInfo -> IO ()
doMigration conn = runSqlite conn $ runMigration migrateAll
