{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import GithubWebhook.Constants (githubEvent)
import MonadImports
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Internal as BSLI
import qualified Data.List as L
import qualified Data.Text.Lazy as TL
import qualified Database.Persist.Sqlite as P
import qualified DataStore.Job as Job
import qualified DataStore.Repo as Repo
import qualified GithubWebhook.Producer as Producer
import qualified Network.Wai as Wai
import qualified Executor
import qualified Utils as U
import qualified Web.Scotty as Scotty

main :: IO ()
main = Executor.serve $ serverAction 4567

serverAction :: Int -> P.ConnectionPool -> IO ()
serverAction port connectionPool = Scotty.scotty port $ do
    Scotty.post "/payload" $
        handleGithubWebrequest connectionPool <$> Scotty.request <*> Scotty.headers <*> Scotty.body
            >>= eitherT U.putStrLnIO return

    Scotty.notFound $ Scotty.text "There is no such route."

handleGithubWebrequest
    :: P.ConnectionPool
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
