{-# LANGUAGE OverloadedStrings #-}

module QueueStore.Constants
( queueStoreDBPath
) where

import qualified Data.ByteString.Internal as BSI

queueStoreDBPath :: String
queueStoreDBPath = "queueStoreDBPath"