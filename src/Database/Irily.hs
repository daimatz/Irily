module Database.Irily
    where

import           Control.Monad.State
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Text           (Text)

data Relation = Relation
    { relationColumns :: [Column]
    , relationTuples  :: [Tuple]
    }
  deriving (Show, Read, Eq, Ord)

data Table = Table
    { tableName     :: Text
    , tableRelation :: Relation
    }
  deriving (Show, Read, Eq, Ord)

data Tuple = Tuple
    { tupleValues :: [Text]
    }
  deriving (Show, Read, Eq, Ord)

data Column = Column
    { columnParent :: Text
    , columnName   :: Text
    }
  deriving (Show, Read, Eq, Ord)
