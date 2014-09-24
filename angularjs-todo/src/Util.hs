{-# LANGUAGE OverloadedStrings #-}

module Util (
    reader
  , logFail
  , logRunEitherT
  , getJSON
  , writeJSON
  ) where

import           Control.Monad.Trans.Either
import qualified Data.Aeson as A
import           Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T

import           Snap.Core
import           Snap.Snaplet
import           Application

type H = Handler App App

-------------------------------------------------------------------------------
-- | Mark response as 'application/json'
jsonResponse :: MonadSnap m => m ()
jsonResponse = modifyResponse $ setHeader "Content-Type" "application/json"

-------------------------------------------------------------------------------
-- | Set MIME to 'application/json' and write given object into
-- 'Response' body.
writeJSON :: (MonadSnap m, A.ToJSON a) => a -> m ()
writeJSON a = do
  jsonResponse
  writeLBS . A.encode $ a

-------------------------------------------------------------------------------
-- | Try to parse request body as JSON with a default max size of
-- 50000.
getJSON :: (MonadSnap m, A.FromJSON a) => m (Either String a)
getJSON = getBoundedJSON 50000


-------------------------------------------------------------------------------
-- | Parse request body into JSON or return an error string.
getBoundedJSON
    :: (MonadSnap m, A.FromJSON a)
    => Int64
    -- ^ Maximum size in bytes
    -> m (Either String a)
getBoundedJSON n = do
  bodyVal <- A.decode `fmap` readRequestBody n
  return $ case bodyVal of
    Nothing -> Left "Can't find JSON data in POST body"
    Just v -> case A.fromJSON v of
                A.Error e -> Left e
                A.Success a -> Right a

reader :: T.Reader a -> T.Text -> Either String a
reader p s =
  case p s of
    Right (a, "") -> return a
    Right (_, _) -> Left "readParser: input not exhausted"
    Left e -> Left e

-- | Log Either Left values or run the Handler action.  To be used in
-- situations where to user shouldn't see an error (either due to it
-- being irrelevant or due to security) but we want to leave a trace
-- of the error case anyway.
logFail :: Either String (H ()) -> H ()
logFail = either (logError . T.encodeUtf8 . T.pack) id

logRunEitherT :: EitherT String H (H ()) -> H ()
logRunEitherT e = runEitherT e >>= logFail
