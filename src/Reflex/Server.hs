{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Reflex.Server
  ( host,
    writeResponse,
    HasServerEvents (..),
  )
where

import Control.Concurrent.Async
import Control.Concurrent.STM.TQueue
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.STM
import Data.Dependent.Sum (DSum ((:=>)))
import Data.Kind
import Network.Socket
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Reflex hiding (Request, Response)
import Reflex.Host.Class
import Reflex.Server.WaiApp

-- | Provides an implementation of the 'HasServerEvents' type class.
newtype ReflexServerT t (m :: Type -> Type) a = MReflexServer {unReflexServerT :: (ReaderT (ServerEvents t) m a)}

type ConcreteReflexServer =
  ReflexServerT Spider (PostBuildT Spider (PerformEventT Spider (SpiderHost Global)))

runReflexServerT :: ReflexServerT t m a -> ServerEvents t -> m a
runReflexServerT = runReaderT . unReflexServerT

data ServerEvents t = ServerEvents
  { serverEventsPostBuildEvent :: Event t (),
    serverEventsRequest :: Event t (RequestToken, Request),
    serverEventsResponseQue :: Que (RequestToken, Response)
  }

class HasServerEvents t m | m -> t where
  getPostBuildEvent :: m (Event t ())
  getRequest :: m (Event t (RequestToken, Request))
  getResponseQue :: m (Que (RequestToken, Response))

data ServerSettings = MkSettings
  { serverSettingsWarpSettings :: Warp.Settings,
    serverSettingsWarpSocket :: Socket
  }

-- defaultSettings :: ServerSettings
-- defaultSettings = MkSettings
--   { serverSettingsWarpSettings = Warp.defaultSettings
--   , serverSettingsWarpSocket :: Socket
--   }

host :: ServerSettings -> ConcreteReflexServer () -> IO ()
host MkSettings {..} server = do
  outRequests <- newTQueueIO
  serverEventsResponseQue <- newTQueueIO
  withAsync
    ( Warp.runSettingsSocket serverSettingsWarpSettings serverSettingsWarpSocket $
        aRequestThread outRequests serverEventsResponseQue
    )
    $ \_serverHandle -> runSpiderHost $ do
      (serverEventsPostBuildEvent, trPostBuildRef) <- newEventWithTriggerRef
      (serverEventsRequest, trNewRequest) <- newEventWithTriggerRef

      -- (this is async proly)
      ((), FireCommand fire) <-
        hostPerformEventT $
          flip runPostBuildT serverEventsPostBuildEvent $
            runReflexServerT server $ ServerEvents {..}

      -- Trigger the post build event.
      (readRef trPostBuildRef >>=) . mapM_ $ \tr ->
        fire [tr :=> Identity ()] $ return ()

      fix $ \loop -> do
        (readRef trNewRequest >>=) . mapM_ $ \tr -> do
          request <- liftIO $ atomically $ readTQueue outRequests
          fire [tr :=> Identity request] $ return ()
        loop

writeResponse ::
  (PerformEvent t m, MonadIO (Performable m), HasServerEvents t m) =>
  Event t (RequestToken, Response) ->
  m ()
writeResponse ev = do
  que <- getResponseQue
  performEvent_ $
    (\req -> liftIO $ atomically $ writeTQueue que req)
      <$> ev
