{-# LANGUAGE DataKinds, TypeOperators, DeriveGeneric #-}
import Control.Monad
import Control.Monad.Except
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Encode
import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder', Config(..))
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Internal.Builder as T
import qualified Data.Text.IO as T
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import System.IO
import Servant
import Servant.EDE
import Servant.Docs (markdown, docs, ToSample(..), singleSample)

type API =
  "static" :> Raw
  :<|> Get '[HTML "index.html"] Object
  :<|> "update" :> ReqBody '[JSON] (Object, Object) :> Post '[JSON] ()
  :<|> "todo" :> Capture "todo_file" String :> Get '[JSON] (Value, Value)

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =
  serveDirectory "static"
  :<|> return mempty
  :<|> update
  :<|> todo

  where
    update (j,hj) = do
      liftIO
        $ writeFile "todo/20160628192500.json" $ T.unpack $ T.toLazyText
        $ encodePrettyToTextBuilder' (Config 2 mempty) $ toJSON j

      liftIO
        $ writeFile "todo/20160628192500_history.json" $ T.unpack $ T.toLazyText
        $ encodePrettyToTextBuilder' (Config 2 mempty) $ toJSON hj
      return ()

    todo fn = do
      let (bn,_) = break (== '.') fn
      Just j <- decode <$> liftIO (BS.readFile $ "todo/" ++ fn)
      Just hj <- decode <$> liftIO (BS.readFile $ "todo/" ++ bn ++ "_history.json")
      return (j, hj)

startApp :: IO ()
startApp = do
  loadTemplates api "static/templates"
  let port = 8080
  putStrLn $ "http://localhost:" ++ show port ++ "/"
  run port app

main :: IO ()
main = startApp
