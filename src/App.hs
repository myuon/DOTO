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
  :<|> "save" :> Capture "todo_id" String :> Capture "filename" String :> ReqBody '[JSON] Value :> Post '[JSON] ()
  :<|> "todo" :> Capture "todo_id" String :> Capture "filename" String :> Get '[JSON] Value

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =
  serveDirectory "static"
  :<|> return mempty
  :<|> save
  :<|> todo

  where
    save tid fn json = do
      liftIO
        $ writeFile ("todo/" ++ tid ++ "/" ++ fn) $ T.unpack $ T.toLazyText
        $ encodePrettyToTextBuilder' (Config 2 mempty) $ toJSON json

    todo tid fn = do
      Just j <- decode <$> liftIO (BS.readFile $ "todo/" ++ tid ++ "/" ++ fn)
      return j

startApp :: IO ()
startApp = do
  loadTemplates api "static/templates"
  let port = 8080
  putStrLn $ "http://localhost:" ++ show port ++ "/"
  run port app

main :: IO ()
main = startApp
