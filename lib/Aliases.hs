{-# LANGUAGE Rank2Types #-}

module Aliases
( Error
, EIO
) where

import MonadImports

type Error = String

type EIO a = forall m. (MonadIO m) => EitherT Error m a
