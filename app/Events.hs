{-# LANGUAGE DeriveGeneric #-}

module Events
( PushEvent
) where

import qualified Data.Text as T
import qualified Data.Aeson as A

import qualified User as U
import qualified Repo as R
import qualified Commit as C
import qualified Sender as S

import GHC.Generics

data PushEvent = PushEvent
    { ref :: T.Text
    , before :: T.Text
    , after :: T.Text
    , created :: Bool
    , deleted :: Bool
    , forced :: Bool
    , base_ref :: Maybe T.Text
    , compare :: T.Text
    , commits :: [C.Commit]
    , head_commit :: C.Commit
    , repository :: R.Repo
    , pusher :: U.User
    , sender :: S.Sender } deriving (Eq, Generic, Show)

instance A.ToJSON PushEvent
instance A.FromJSON PushEvent