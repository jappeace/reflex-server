{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}

-- | privite module so that the constructor gets hidden
--   but we can still use it for various reflex events
module Reflex.Server.RequestToken
  (RequestToken(..))
where

import Data.UUID

-- | the request token is used to associate a request with a response.
--   this is required because reflex does it's own thread management.
newtype RequestToken = MkRequestToken { _untoken :: UUID}
  deriving (Eq)

