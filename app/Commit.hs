{-# LANGUAGE DeriveGeneric #-}

module Commit
( Commit
) where

import qualified Data.Text as T
import qualified Data.Aeson as A

import GHC.Generics

import qualified User as U

data Commit = Commit
    { id :: T.Text
    , tree_id :: T.Text
    , distinct :: Bool
    , message :: T.Text
    , timestamp :: T.Text
    , url :: T.Text
    , author :: U.User
    , committer :: U.User
    , added :: [T.Text]
    , removed :: [T.Text]
    , modified :: [T.Text] } deriving (Eq, Generic, Show)

instance A.ToJSON Commit
instance A.FromJSON Commit