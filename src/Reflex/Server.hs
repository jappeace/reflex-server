module Reflex.Server
  ( main
  )
where

import qualified Network.Wai.Handler.Warp as Warp

newtype ConcreteServer t a = MkConcreteServer (ReaderT (SystemEvents t)  a)


data SystemEvents t = SystemEvents
  { sysPostBuildEvent                 :: Event t ()
  }

data ServerSettings = MkSettings
  { warpSettings :: Warp.Settings
  }

host :: ServerSettings -> ConcreteServer () -> IO ()
host settings server = runSpiderHost $ do
  (sysPostBuildEvent, trPostBuildRef) <- newEventWithTriggerRef

  -- (this is async proly)
  runConcreteServer...

  -- Trigger the post build event.
  (readRef trPostBuildRef >>=) . mapM_ $ \tr ->
    fire [tr :=> Identity ()] $ return ()


  -- do main warp loop
