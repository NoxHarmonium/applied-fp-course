{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Level04.DB.Types where

import           Data.Text                      (Text)
import           Data.Time                      (UTCTime)

import           Database.SQLite.Simple.FromRow (FromRow (fromRow), field)
import           Database.SQLite.Simple.ToRow (ToRow (toRow))

data DBComment = DBComment
  { dbCommentId    :: Int
  , dbCommentTopic :: Text
  , dbCommentBody  :: Text
  , dbCommentTime  :: UTCTime
  }
  deriving ( Show )

instance FromRow DBComment where
  fromRow = DBComment <$> field <*> field <*> field <*> field

instance ToRow DBComment where
  toRow (DBComment _ topic body time) = toRow(topic, body, time)

-- Now move to ``src/Level04/Types.hs``
