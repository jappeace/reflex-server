module Main where

import Control.Monad.Except
import Data.Binary.Builder
import Data.Text
import Data.Text.Encoding
import Network.HTTP.Types.Status
import Network.Wai
import qualified Reflex.Server as Server

main :: IO ()
main =
  Server.host Server.defaultSettings $ do
    requestEvt <- Server.getRequest
    let mRequests =
              ( \(token, request) -> do
                  res <- runExceptT $ responseMap request
                  liftIO $ putStrLn "request"
                  case res of
                    Left NotFound -> pure (token, responseLBS notFound404 [] "not found")
                    Right resp -> pure (token, responseLBS ok200 [] $ toLazyByteString $ encodeUtf8Builder resp)
              )
                <$> requestEvt
    responses <- Server.holdRequest mRequests
    Server.writeResponse responses

data ResponseErrors = NotFound

responseMap :: MonadError ResponseErrors m => Request -> m Text
responseMap req = case pathInfo req of
  ["jappie"] -> pure "hello jappie!"
  ["luci"] -> pure "hi lucy ❤️"
  ["duck"] -> pure "duck duck duck."
  ["eru"] -> pure "ewwwuuu~"
  _ -> throwError NotFound
