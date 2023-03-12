{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Reflex.Server.WaiApp
  ( aRequestThread,
    Que,
    RequestToken,
  )
where

import Control.Concurrent.STM.TQueue
import Control.Monad.Reader
import Control.Monad.STM
import qualified Data.UUID.V4 as V4
import Network.Wai
import Reflex.Server.RequestToken

-- | I can't spell this
type Que = TQueue

aRequestThread ::
  Que (RequestToken, Request) ->
  Que (RequestToken, Response) ->
  Request ->
  (Response -> IO ResponseReceived) ->
  IO ResponseReceived
aRequestThread outRequests inResponses request responseFun = do
  reqId <- MkRequestToken <$> V4.nextRandom
  atomically $ writeTQueue outRequests (reqId, request)

  response <- fix $ \loop -> do
    -- we need to be outside of atomically to loop
    -- to give other threads a chance
    mresponse <-
      atomically $ do
        (reqToken, _response) <- peekTQueue inResponses
        if (reqToken == reqId)
          then Just . snd <$> readTQueue inResponses
          else pure Nothing
    case mresponse of
      Just x -> pure x
      Nothing -> loop
  responseFun response
