module Utils
( bigUserJSONFieldModifier
, camelCaseToSnakeCase
, printIO
, putStrLnIO
, hushT
, note
, hush
, replaceFirst
) where

import qualified Data.Char as Ch

import MonadImports

type Error = String

camelCaseToSnakeCase :: String -> String
camelCaseToSnakeCase [] = []
camelCaseToSnakeCase (ch:s)
    | Ch.isUpper ch = '_' : Ch.toLower ch : camelCaseToSnakeCase s
    | otherwise = ch : camelCaseToSnakeCase s

bigUserJSONFieldModifier :: String -> String
bigUserJSONFieldModifier fieldName =
    case fieldName of
        "userType" -> "type"
        _ -> camelCaseToSnakeCase fieldName

printIO :: (MonadIO m, Show a) => a -> m ()
printIO = liftIO . print

putStrLnIO :: (MonadIO m) => String -> m ()
putStrLnIO = liftIO . putStrLn

-- 'hush' for the respective monad transformers.
hushT :: (Monad m) => EitherT l m r -> MaybeT m r
hushT = MaybeT . liftM hush . runEitherT

-- | Convert, with a error message to be used if the `Maybe` is `Nothing`.
note :: Error -> Maybe b -> Either Error b
note error Nothing = Left error
note _ (Just x) = Right x

-- | Convert, supressing the error message.
hush :: Either l r -> Maybe r
hush (Left _) = Nothing
hush (Right r) = Just r

replaceFirst :: (Eq a) => a -> a -> [a] -> [a]
replaceFirst _ _ [] = []
replaceFirst a b (x:xs)
    | a == x = b:xs
    | otherwise = x : replaceFirst a b xs
