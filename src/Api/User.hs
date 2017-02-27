{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings, FlexibleContexts, TypeApplications #-}
module Api.User where

import Control.Monad.Trans.Class (lift)
import Database.Persist.Sqlite
import Data.Aeson
import Data.Time
import qualified Data.Text as T
import qualified Data.List as L
import Servant

import Models
import Config

-- TODO: authとかそのへん
type UserAPI =
       "users" :> Get '[JSON] [User]
  :<|> "user" :> Capture "user_id" UserId :> Get '[JSON] (Maybe (Entity User))
  :<|> "user" :> QueryParam "user_name" T.Text :> Get '[JSON] (Maybe (Entity User))
  :<|> "user" :> "new" :> ReqBody '[JSON] Value :> Post '[JSON] (Maybe (Key User))
  :<|> "user" :> "update" :> Capture "user_id" UserId :> ReqBody '[JSON] Value :> Post '[JSON] ()

selectUsers :: IO [User]
selectUsers = do
  users <- runDB $ selectList [] []
  return $ (\(Entity _ u) -> u) <$> users

newtype UserDTO = UserDTO (T.Text, T.Text)

instance FromJSON UserDTO where
  parseJSON (Object v) = UserDTO <$> ((,) <$> v .: "name" <*> v .: "email")
  parseJSON _ = mempty

newtype UserIsoDTO = UserIsoDTO User

instance FromJSON UserIsoDTO where
  parseJSON (Object v) = UserIsoDTO <$> (User
    <$> v .: "name"
    <*> v .: "email"
    <*> v .: "list"
    <*> v .: "activities")
  parseJSON _ = mempty

serverUserAPI :: Server UserAPI
serverUserAPI = getAll :<|> getById :<|> getByName :<|> postNew :<|> postUpdate where
  getAll = lift selectUsers
  getById uid = lift $ runDB $ selectFirst [UserId ==. uid] []
  getByName name = case name of
    Just name' -> lift $ runDB $ selectFirst [UserName ==. name'] []
    Nothing -> return Nothing
  postNew json = case fromJSON @UserDTO json of
    Error err -> lift $ print err >> return Nothing
    Success (UserDTO (name, email)) -> fmap Just $ lift $ newUser name email
  postUpdate uid json = case fromJSON @UserIsoDTO json of
    Error err -> lift $ print err
    Success (UserIsoDTO user) -> lift $ runDB $ replace uid user
