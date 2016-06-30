{-# LANGUAGE OverloadedStrings #-}

module GithubWebhook.Server (main) where

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

import GithubWebhook.Constants (Error)

import qualified Data.Aeson as A

import GithubWebhook.Types.Events.PushEvent
import GithubWebhook.Types.Events.IssueCommentEvent

import qualified GithubWebhook.Producer as Producer

import qualified Utils as U

main :: IO ()
main = serve 4567

serve :: Int -> IO ()
serve port = Scotty.scotty port $ do
    Scotty.post "/payload" $ do
        request <- Scotty.request
        headers <- Scotty.headers
        body <- Scotty.body
        eitherT U.putStrLnIO return $ handleGithubWebrequest request headers body

    Scotty.notFound $ do
        Scotty.text "There is no such route."

handleGithubWebrequest :: Wai.Request -> [(TL.Text, TL.Text)] -> BSLI.ByteString -> EitherT String Scotty.ActionM ()
handleGithubWebrequest _request headers body = do
    event <- hoistEither $ U.note "Could not extract event from Github POST header" maybeEvent
    liftIOEitherT . join . hoistEither $ case event of
        "push"          -> Producer.handlePush         <$> (A.eitherDecode body :: Either String PushEvent)
        "issue_comment" -> Producer.handleIssueComment <$> (A.eitherDecode body :: Either String IssueCommentEvent)
        _ -> Left $ "Cannot handle Github event " ++ (show event)
    where
        maybeEvent :: Maybe TL.Text
        maybeEvent = fmap snd . L.find ((==) "X-GitHub-Event" . fst) $ headers

        liftIOEitherT :: (MonadIO m) => EitherT e IO a -> EitherT e m a
        liftIOEitherT = EitherT . liftIO . runEitherT
