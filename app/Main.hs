module Main where

import qualified Reflex.Server as Server
import Network.Wai
import Network.HTTP.Types.Status

main :: IO ()
main =
  Server.host Server.defaultSettings $ do
    requestEvt <- Server.getRequest
    Server.writeResponse $ (\(token, _request) -> (token, responseLBS ok200 [] "hello world!")) <$> requestEvt
