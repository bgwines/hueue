{-# LANGUAGE RankNTypes #-}

module QueueStore.Types.Aliases
( Error
, EIO
) where

import Control.Monad.Trans.Either
import Control.Monad.IO.Class

-- | Internal error type
type Error = String

-- | Shorthand
type EIO a = (MonadIO m) => EitherT Error m a

