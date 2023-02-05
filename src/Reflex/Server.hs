{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}

module Reflex.Server
  ( host
  )
where

import Control.Concurrent.Async
import Control.Concurrent.STM.TQueue
import Control.Monad.STM
import           Data.Dependent.Sum       (DSum ((:=>)))
import           Control.Monad.Identity   (Identity (..))
import Control.Monad.Reader
import Control.Monad.Ref
import Data.Kind
import Reflex(Spider, Event, SpiderHost, Global, PostBuildT, runPostBuildT, runSpiderHost)
import Data.UUID
import qualified Data.UUID.V4 as V4
import Network.Wai
import Reflex.TriggerEvent.Base
import Reflex.PerformEvent.Base
import Reflex.Host.Class
import Network.Socket
import qualified Network.Wai.Handler.Warp as Warp
import System.Random

-- | Provides an implementation of the 'HasServerEvents' type class.
newtype ReflexServerT t (m :: Type -> Type) a =
  MReflexServer { unReflexServerT :: (ReaderT (ServerEvents t) m a) }

type ConcreteReflexServer = ReflexServerT Spider (PostBuildT Spider (PerformEventT Spider (SpiderHost Global)))

runReflexServerT :: ReflexServerT t m a -> ServerEvents t -> m a
runReflexServerT = runReaderT . unReflexServerT

newtype RequestToken = MkRequestToken { untoken :: UUID }
  deriving Eq

data ServerEvents t = ServerEvents
  { serverEventsPostBuildEvent  :: Event t ()
  , serverEventsRequest         :: Event t (RequestToken, Request)
  }

data ServerSettings = MkSettings
  { serverSettingsWarpSettings :: Warp.Settings
  , serverSettingsWarpSocket :: Socket
  }


-- | I can't spell this
type Que = TQueue

-- defaultSettings :: ServerSettings
-- defaultSettings = MkSettings
--   { serverSettingsWarpSettings = Warp.defaultSettings
--   , serverSettingsWarpSocket :: Socket
--   }

host :: ServerSettings -> ConcreteReflexServer () -> IO ()
host MkSettings{..} server = do
  outRequests <- newTQueueIO
  inResponses <- newTQueueIO
  withAsync (Warp.runSettingsSocket serverSettingsWarpSettings serverSettingsWarpSocket
             $ aRequestThread outRequests inResponses) $ \_serverHandle -> runSpiderHost $ do
    (serverEventsPostBuildEvent, trPostBuildRef) <- newEventWithTriggerRef
    (serverEventsRequest,        trNewRequest) <- newEventWithTriggerRef

    -- (this is async proly)
    ((), FireCommand fire) <-
      hostPerformEventT $ flip runPostBuildT serverEventsPostBuildEvent
                        $ runReflexServerT server $ ServerEvents{..}

    -- Trigger the post build event.
    (readRef trPostBuildRef >>=) . mapM_ $ \tr ->
      fire [tr :=> Identity ()] $ return ()


aRequestThread ::  Que (RequestToken, Request) -> Que (RequestToken, Response) -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
aRequestThread outRequests inResponses request responseFun = do
  reqId <- MkRequestToken <$> V4.nextRandom
  atomically $ writeTQueue outRequests (reqId, request)

  response <- fix $ \loop -> do
    mresponse <-
      atomically $ do
        (reqToken, response) <- peekTQueue inResponses
        if (reqToken == reqId) then
          Just . snd <$> readTQueue inResponses
        else pure Nothing
    case mresponse of
      Just x -> pure x
      Nothing -> loop
  responseFun response
