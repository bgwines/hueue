{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Web.Scotty as Scotty

import Data.Monoid
import Control.Monad
import Control.Monad.IO.Class

main :: IO ()
main = Scotty.scotty 4567 $ do
    Scotty.post "/payload" $ do
        liftIO . putStrLn $ "got payloaded"
        Scotty.html $ "<h1>IN THE PAYLOAD CASE</h1>"

    Scotty.post "/:word" $ do
        beam <- Scotty.param "word"
        liftIO $ print $ mconcat ["get; word = ", beam]
        Scotty.html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

    Scotty.matchAny "/all" $ do
        Scotty.text "matches all methods"

    Scotty.notFound $ do
        Scotty.text "there is no such route."