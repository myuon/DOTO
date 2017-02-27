{-# LANGUAGE DataKinds, TypeOperators, TypeApplications, OverloadedStrings #-}
import Control.Monad
import Control.Monad.Except
import Control.Monad.Logger
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder', defConfig)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Internal.Builder as T
import qualified Data.Text.IO as T
import Network.Wai
import Network.Wai.Handler.Warp
import System.IO
import System.Environment (getArgs)
import Servant
import Servant.EDE
import Servant.Docs (markdown, docs, ToSample(..), singleSample)
import Servant.JS

import Api.TodoItem (TodoItemAPI, serverTodoItemAPI)
import Api.TodoList (TodoListAPI, serverTodoListAPI)
import Api.ActivityList (ActivityListAPI, serverActivityListAPI)
import Api.User (UserAPI, serverUserAPI)
import Models (doMigration)
import Config (connInfo)

type API =
  "static" :> Raw
  :<|> Get '[HTML "index.html"] Object
  :<|> "save" :> Capture "todo_id" String :> Capture "filename" String :> ReqBody '[JSON] Value :> Post '[JSON] ()
  :<|> "todo" :> Capture "todo_id" String :> Capture "filename" String :> Get '[JSON] Value
  :<|> TodoItemAPI
  :<|> TodoListAPI
  :<|> ActivityListAPI
  :<|> UserAPI

app :: Application
app = serve @API Proxy server

server :: Server API
server =
  serveDirectory "static"
  :<|> return mempty
  :<|> save
  :<|> todo
  :<|> serverTodoItemAPI
  :<|> serverTodoListAPI
  :<|> serverActivityListAPI
  :<|> serverUserAPI

  where
    save tid fn json = liftIO
      $ writeFile ("todo/" ++ tid ++ "/" ++ fn) $ T.unpack $ T.toLazyText
      $ encodePrettyToTextBuilder' defConfig $ toJSON json

    todo tid fn = do
      Just j <- decode <$> liftIO (BS.readFile $ "todo/" ++ tid ++ "/" ++ fn)
      return j

startApp :: IO ()
startApp = do
  loadTemplates @API Proxy [] "static/templates"
  let port = 8080
  putStrLn $ "http://localhost:" ++ show port ++ "/"
  run port app

main :: IO ()
main = do
  let jquery' = jqueryWith $ defCommonGeneratorOptions { moduleName = "api" }
  writeJSForAPI @TodoItemAPI Proxy jquery' "static/js/api/todoitem.js"
  writeJSForAPI @ActivityListAPI Proxy jquery' "static/js/api/activitylist.js"
  writeJSForAPI @TodoListAPI Proxy jquery' "static/js/api/todolist.js"
  writeJSForAPI @UserAPI Proxy jquery' "static/js/api/user.js"

  args <- getArgs
  case args of
    ("migrate" : _) -> doMigration
    _ -> startApp
