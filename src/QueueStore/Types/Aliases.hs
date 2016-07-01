module QueueStore.Types.Aliases
( Error
, EIO
) where

import Control.Monad.Trans.Either

-- | Internal error type
type Error = String

-- | Shorthand
type EIO a = EitherT Error IO a

