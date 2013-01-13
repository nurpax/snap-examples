{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Db (
    User(..)
  , Todo(..)
  , createTables
  , saveTodo
  , listTodos) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Int (Int64)
import           Data.Maybe
import qualified Data.Text as T
import           Database.SQLite.Simple

data User = User Int T.Text

data Todo =
  Todo
  { todoId :: Maybe Int64
  , todoText :: T.Text
  , todoDone :: Bool
  } deriving (Show)

instance FromJSON Todo where
  parseJSON (Object v) =
    Todo <$> optional (v .: "id")
         <*> v .: "text"
         <*> v .: "done"
  parseJSON _ = mzero

instance ToJSON Todo where
  toJSON (Todo i text done) =
    object [ "id" .= fromJust i
           , "text" .= text
           , "done" .= done
           ]

instance FromRow Todo where
  fromRow = Todo <$> field <*> field <*> field

tableExists :: Connection -> String -> IO Bool
tableExists conn tblName = do
  r <- query conn "SELECT name FROM sqlite_master WHERE type='table' AND name=?" (Only tblName)
  case r of
    [Only (_ :: String)] -> return True
    _ -> return False

-- | Create the necessary database tables, if not already initialized.
createTables :: Connection -> IO ()
createTables conn = do
  -- Note: for a bigger app, you probably want to create a 'version'
  -- table too and use it to keep track of schema version and
  -- implement your schema upgrade procedure here.
  schemaCreated <- tableExists conn "todos"
  unless schemaCreated $
    execute_ conn
      (Query $
       T.concat [ "CREATE TABLE todos ("
                , "id INTEGER PRIMARY KEY, "
                , "user_id INTEGER NOT NULL, "
                , "saved_on TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL, "
                , "text TEXT, "
                , "done BOOLEAN)"])

-- | Retrieve a user's list of comments
listTodos :: Connection -> User -> IO [Todo]
listTodos conn (User uid _) =
  query conn "SELECT id,text,done FROM todos WHERE user_id = ?" (Only uid)

-- | Save or update a todo
saveTodo :: Connection -> User -> Todo -> IO Todo
saveTodo conn (User uid _) t =
  maybe newTodo updateTodo (todoId t)
  where
    newTodo = do
      execute conn "INSERT INTO todos (user_id,text,done) VALUES (?,?,?)"
        (uid, todoText t, todoDone t)
      rowId <- lastInsertRowId conn
      return $ t { todoId = Just rowId }

    updateTodo tid = do
      execute conn "UPDATE todos SET text = ?, done = ? WHERE (user_id = ? AND id = ?)"
        (todoText t, todoDone t, uid, tid)
      return t
