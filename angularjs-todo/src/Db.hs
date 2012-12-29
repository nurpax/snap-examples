{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Db (
    User(..)
  , Comment(..)
  , createTables
  , saveComment
  , listComments) where

import           Control.Applicative
import           Control.Monad
import qualified Data.Text as T
import           Data.Time (UTCTime)
import qualified Database.SQLite.Simple as S
import           Snap.Snaplet
import           Snap.Snaplet.SqliteSimple
------------------------------------------------------------------------------
import           Application

data User = User Int T.Text

data Comment = Comment
  {
    commentId :: Int
  , commentSavedOn :: UTCTime
  , commentText :: T.Text
  } deriving (Show)

instance FromRow Comment where
  fromRow = Comment <$> field <*> field <*> field

tableExists :: S.Connection -> String -> IO Bool
tableExists conn tblName = do
  r <- S.query conn "SELECT name FROM sqlite_master WHERE type='table' AND name=?" (Only tblName)
  case r of
    [Only (_ :: String)] -> return True
    _ -> return False

-- | Create the necessary database tables, if not already initialized.
createTables :: S.Connection -> IO ()
createTables conn = do
  -- Note: for a bigger app, you probably want to create a 'version'
  -- table too and use it to keep track of schema version and
  -- implement your schema upgrade procedure here.
  schemaCreated <- tableExists conn "comments"
  unless schemaCreated $
    S.execute_ conn
      (S.Query $
       T.concat [ "CREATE TABLE comments ("
                , "id INTEGER PRIMARY KEY, "
                , "user_id INTEGER NOT NULL, "
                , "saved_on TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL, "
                , "comment TEXT)"])

-- | Retrieve a user's list of comments
listComments :: User -> Handler App Sqlite [Comment]
listComments (User uid _) =
  query "SELECT id,saved_on,comment FROM comments WHERE user_id = ?" (Only uid)

-- | Save a new comment for a user
saveComment :: User -> T.Text -> Handler App Sqlite ()
saveComment (User uid _) c =
  execute "INSERT INTO comments (user_id,comment) VALUES (?,?)" (uid, c)
