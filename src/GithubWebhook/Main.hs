{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

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

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Monad.Logger (runStderrLoggingT)

import qualified Utils as U

import Job

main :: IO ()
main = runStderrLoggingT $ do
    connectionPool <- createSqlitePool "commonPool" 10
    runSqlPool (runMigration migrateAll) connectionPool
    liftIO $ serve 4567 connectionPool

serve :: Int -> ConnectionPool -> IO ()
serve port connectionPool = Scotty.scotty port $ do
    Scotty.post "/payload" $
        handleGithubWebrequest connectionPool <$> Scotty.request <*> Scotty.headers <*> Scotty.body
            >>= eitherT U.putStrLnIO return

    Scotty.notFound $ Scotty.text "There is no such route."

handleGithubWebrequest
    :: ConnectionPool
    -> Wai.Request
    -> [(TL.Text, TL.Text)]
    -> BSLI.ByteString
    -> EitherT String Scotty.ActionM ()
handleGithubWebrequest connectionPool _request headers body = do
    U.putStrLnIO "handling a Github web request!"
    event <- hoistEither $ U.note "Could not extract event from Github POST header" maybeEvent
    case event of
        -- Can't use constants here :/
        -- http://stackoverflow.com/questions/9336385/why-do-these-pattern-matches-overlap
        "issue_comment" -> (hoistEither . A.eitherDecode $ body)
            >>= Producer.handleIssueComment connectionPool
        _ -> left $ "Cannot handle Github event " ++ show event
    where
        maybeEvent :: Maybe TL.Text
        maybeEvent = fmap snd . L.find ((==) githubEvent . fst) $ headers
