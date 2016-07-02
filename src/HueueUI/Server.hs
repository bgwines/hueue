{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HueueUI.Server (main) where

import qualified Web.Scotty as Scotty
import qualified Network.Wai as Wai

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy.Internal as BSLI

import GHC.Generics

import Data.Maybe
import Data.Monoid
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Either

import GithubWebhook.Types.Error (Error)
import GithubWebhook.Constants (githubEvent)

import qualified Data.Aeson as A

import qualified Utils as U

import qualified QueueStore.Store

main :: IO ()
main = serve 4568

serve :: Int -> IO ()
serve port = Scotty.scotty port $ do
    Scotty.get "/loadQueue" $ do
        U.putStrLnIO "in here"
        --repoID <- Scotty.param "repoID" :: Scotty.ActionM Integer
        equeue <- liftIO . runEitherT $ QueueStore.Store.loadQueueDEBUG 61999075
        U.printIO equeue
        Scotty.text . TL.pack . show $ equeue
        --U.printIO fullRepoName
        return ()
        --handleGithubWebrequest <$> Scotty.request <*> Scotty.headers <*> Scotty.body
        --    >>= eitherT U.putStrLnIO return

    Scotty.notFound $ do
        U.putStrLnIO "in here..."
        Scotty.text "There is no such route."

--handleGithubWebrequest
--    :: Wai.Request
--    -> [(TL.Text, TL.Text)]
--    -> BSLI.ByteString
--    -> EitherT String Scotty.ActionM ()
--handleGithubWebrequest _request headers body = do
--    event <- hoistEither $ U.note "Could not extract event from Github POST header" maybeEvent
--    liftIOEitherT . join . hoistEither $ case event of
--        -- Can't use constants here :/
--        -- http://stackoverflow.com/questions/9336385/why-do-these-pattern-matches-overlap
--        "push"          -> Producer.handlePush         <$> A.eitherDecode body
--        "issue_comment" -> Producer.handleIssueComment <$> A.eitherDecode body
--        _ -> Left $ "Cannot handle Github event " ++ (show event)
--    where
--        maybeEvent :: Maybe TL.Text
--        maybeEvent = fmap snd . L.find ((==) githubEvent . fst) $ headers

--        liftIOEitherT :: (MonadIO m) => EitherT e IO a -> EitherT e m a
--        liftIOEitherT = EitherT . liftIO . runEitherT
