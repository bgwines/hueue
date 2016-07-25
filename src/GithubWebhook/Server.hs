{-# LANGUAGE OverloadedStrings #-}

module GithubWebhook.Server (main) where

import qualified Web.Scotty as Scotty
import qualified Network.Wai as Wai

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy.Internal as BSLI

import GHC.Generics

import MonadImports

import GithubWebhook.Types.Error (Error)
import GithubWebhook.Constants (githubEvent)

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
        handleGithubWebrequest <$> Scotty.request <*> Scotty.headers <*> Scotty.body
            >>= eitherT U.putStrLnIO return

    Scotty.notFound $ do
        Scotty.text "There is no such route."

handleGithubWebrequest
    :: Wai.Request
    -> [(TL.Text, TL.Text)]
    -> BSLI.ByteString
    -> EitherT String Scotty.ActionM ()
handleGithubWebrequest _request headers body = do
    event <- hoistEither $ U.note "Could not extract event from Github POST header" maybeEvent
    case event of
        -- Can't use constants here :/
        -- http://stackoverflow.com/questions/9336385/why-do-these-pattern-matches-overlap
        "issue_comment" -> (hoistEither . A.eitherDecode $ body) >>= Producer.handleIssueComment
        _ -> left $ "Cannot handle Github event " ++ (show event)
    where
        maybeEvent :: Maybe TL.Text
        maybeEvent = fmap snd . L.find ((==) githubEvent . fst) $ headers
