{-# LANGUAGE OverloadedStrings #-}
module Level05.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)
import qualified Data.Bifunctor                     as Bf

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Bifunctor                     (first)
import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection,
                                                     Query (Query, fromQuery),
                                                     close, execute, execute_,
                                                     open, query, query_)
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level05.Types                      (Comment, CommentText,
                                                     Error (DBError), Topic,
                                                     fromDbComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

import           Level05.AppM                       (AppM (..), liftEither)

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
newtype FirstAppDB = FirstAppDB
  { dbConn  :: Connection
  }

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: FilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open fp
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

mapDbError :: Sql.DatabaseResponse a -> Either Error a
mapDbError dbResult = Bf.first DBError dbResult

runDB
  :: (a -> Either Error b)
  -> IO a
  -> AppM b
runDB f q =
  AppM $ (>>= f) <$> mapDbError <$> Sql.runDBAction q

getComments
  :: FirstAppDB
  -> Topic
  -> AppM [Comment]
getComments (FirstAppDB conn) topic =
 let
    sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
    topic' = getTopic topic
    query' = query conn sql (Sql.Only topic')
    transform = traverse fromDbComment
  in
    runDB transform query'

addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> AppM ()
addCommentToTopic (FirstAppDB conn) topic commentText =
  let
    sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
    topic' = getTopic topic
    commentText' = getCommentText commentText
    query' = getCurrentTime >>= \time -> (execute conn sql (topic', commentText', time))
    transform = pure
  in
    runDB transform query'

getTopics
  :: FirstAppDB
  -> AppM [Topic]
getTopics (FirstAppDB conn) =
  let
    sql = "SELECT DISTINCT topic FROM comments"
    query' = query_ conn sql
    transform = traverse $ mkTopic . Sql.fromOnly
  in
    runDB transform query'

deleteTopic
  :: FirstAppDB
  -> Topic
  -> AppM ()
deleteTopic (FirstAppDB conn) topic =
  let
    sql = "DELETE FROM comments WHERE topic = ?"
    topic' = getTopic topic
    query' = execute conn sql (Sql.Only topic')
    transform = pure
  in
    runDB transform query'
