module Main where

import qualified Reflex.Server as Server
import Network.Wai
import Network.HTTP.Types.Status
import Data.ByteString.Lazy
import Data.Binary.Builder
import Data.Text.Encoding
import Data.Text

main :: IO ()
main =
  Server.host Server.defaultSettings $ do
    requestEvt <- Server.getRequest
    Server.writeResponse $ (\(token, request) -> (token,
                                                  case toLazyByteString . encodeUtf8Builder <$> responseMap request of
                                                    Nothing -> (responseLBS notFound404 [] "not found")
                                                    Just resp -> responseLBS ok200 [] resp
                                                 )
                           ) <$> requestEvt

responseMap :: Request -> Maybe Text
responseMap req = case rawPathInfo req of
  "/jappie" -> Just "hello jappie!"
  "/luci" -> Just "hi lucy ❤️"
  "/duck" -> Just "duck duck duck."
  "/eru" -> Just "ewwwuuu~"
  _ -> Nothing
