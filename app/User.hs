{-# LANGUAGE DeriveGeneric #-}

module User
( User
) where

import qualified Data.Text as T
import qualified Data.Aeson as A

import GHC.Generics

data User = User
    { name :: T.Text
    , email :: T.Text
    , username :: Maybe T.Text } deriving (Eq, Generic, Show)

instance A.ToJSON User
instance A.FromJSON User