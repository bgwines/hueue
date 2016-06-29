{-# LANGUAGE DeriveGeneric #-}

module GithubWebhook.Types.SmallUser
( SmallUser(..)
) where

import qualified Data.Text as T
import qualified Data.Aeson as A

import GHC.Generics

data SmallUser = SmallUser
    { name :: T.Text
    , email :: T.Text
    , username :: Maybe T.Text } deriving (Eq, Generic, Show)

instance A.ToJSON SmallUser
instance A.FromJSON SmallUser
