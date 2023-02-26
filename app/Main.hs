module Main where

import qualified Reflex.Server as Server
import Network.Wai
import Network.HTTP.Types.Status
import Data.Binary.Builder
import Data.Text.Encoding
import Data.Text
import Control.Monad.Except

main :: IO ()
main =
  Server.host Server.defaultSettings $ do
    requestEvt <- Server.getRequest
    Server.writeResponse $ (\(token, request) -> do
                                                 res  <- runExceptT $ responseMap request
                                                 case res of
                                                    Left NotFound -> pure (token ,responseLBS notFound404 [] "not found")
                                                    Right resp -> pure (token, responseLBS ok200 [] $ toLazyByteString $ encodeUtf8Builder resp)
                           ) <$> requestEvt

data ResponseErrors = NotFound

responseMap :: MonadError ResponseErrors m => Request -> m Text
responseMap req = case pathInfo req of
  ["jappie"] -> pure "hello jappie!"
  ["luci"] -> pure "hi lucy ❤️"
  ["duck"] -> pure "duck duck duck."
  ["eru"] -> pure "ewwwuuu~"
  _ -> throwError NotFound
