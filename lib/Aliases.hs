{-# LANGUAGE RankNTypes #-}

module Aliases
( Error
, EIO
) where

import MonadImports

-- | Internal error type
type Error = String

-- | Shorthand
type EIO a = forall m. (MonadIO m) => EitherT Error m a

