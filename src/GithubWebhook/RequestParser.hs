{-# LANGUAGE OverloadedStrings #-}

module GithubWebhook.RequestParser
( parse
, ParsedRequest(..)
) where

import GithubWebhook.Types.Error (Error)

import Text.Parsec.Text (Parser)

import qualified Data.Text as T

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as Parsec

import Debug.Trace

data ParsedRequest
    = ParsedRequest
    { task :: T.Text
    , srcBranch :: T.Text
    , dstBranch :: T.Text
    } deriving (Eq, Show)

requestParser :: Parser ParsedRequest
requestParser = do
    hueue <- token
    if hueue /= "hueue"
        then (fail "Parsing failed")
        else ParsedRequest <$> token <*> token <*> token
    where
        token :: Parser T.Text
        token = (T.pack <$> Parsec.many1 Parsec.letter) <* Parsec.spaces

parse :: T.Text -> Either Error ParsedRequest
parse text
    = mapLeft (concatMap Parsec.messageString . Parsec.errorMessages)
    . Parsec.parse requestParser ""
    $ text

mapLeft f (Left a) = Left $ f a
mapLeft _ (Right b) = Right b
