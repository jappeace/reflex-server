{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE DerivingStrategies #-}

module Reflex.Server
  ( host,
    defaultSettings,
    writeResponse,
    HasServerEvents(..),
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
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Reflex hiding (Request, Response)
import Reflex.Host.Class
import Reflex.Server.WaiApp

-- | Provides an implementation of the 'HasServerEvents' type class.
newtype ReflexServerT t (m :: Type -> Type) a = MkReflexServerT {unReflexServerT :: (ReaderT (ServerEvents t) m a)}
  deriving newtype (MonadReader (ServerEvents t))

deriving instance (ReflexHost t, Functor m)        => Functor (ReflexServerT t m)
deriving instance (ReflexHost t, Applicative m)    => Applicative (ReflexServerT t m)
deriving instance (ReflexHost t, Monad m)          => Monad (ReflexServerT t m)
deriving instance (ReflexHost t, MonadFix m)       => MonadFix (ReflexServerT t m)
deriving instance (ReflexHost t, MonadIO m)        => MonadIO (ReflexServerT t m)
deriving instance ReflexHost t                     => MonadTrans (ReflexServerT t)
deriving instance (ReflexHost t, TriggerEvent t m) => TriggerEvent t (ReflexServerT t m)

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


instance (Monad m, ReflexHost t) => HasServerEvents t (ReflexServerT t m) where
  getPostBuildEvent = serverEventsPostBuildEvent <$> ask
  getRequest = serverEventsRequest <$> ask
  getResponseQue = serverEventsResponseQue <$> ask

instance (ReflexHost t, PerformEvent t m) => PerformEvent t (ReflexServerT t m) where
  type Performable (ReflexServerT t m) = ReflexServerT t (Performable m)
  performEvent_ = MkReflexServerT . performEvent_ . fmap unReflexServerT
  performEvent  = MkReflexServerT . performEvent  . fmap unReflexServerT

data ServerSettings = MkSettings
  { serverSettingsWarpSettings :: Warp.Settings
  }

defaultSettings :: ServerSettings
defaultSettings = MkSettings
  { serverSettingsWarpSettings = Warp.defaultSettings
  }

host :: ServerSettings -> ConcreteReflexServer () -> IO ()
host MkSettings {..} server = do
  outRequests <- newTQueueIO
  serverEventsResponseQue <- newTQueueIO
  withAsync
    ( Warp.runSettings serverSettingsWarpSettings $
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
