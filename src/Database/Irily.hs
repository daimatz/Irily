module Database.Irily
    where

-- import           Control.Monad.State
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Text           (Text)

type Relation = ([Text], [Tuple])
type Table = Relation
type Database = Map Text Table
type Tuple = [Value]
-- type Column = (Text, Text)

data Value
    = VInt  Int
    | VText Text
    | VNull
  deriving (Show, Read, Eq, Ord)

newDB :: Map Text Table
newDB = Map.fromList []

create :: Database -> Text -> [Text] -> Database
create db tableName columnNames =
    Map.insert tableName (columnNames, []) db

from :: Database -> Text -> Maybe Relation
from db name = Map.lookup name db

select :: [Text] -> Relation -> Relation
select columns relation =
    unzip $ filter (\r -> fst r `elem` columns) $ uncurry zip relation

insert :: Database -> Text -> Tuple -> Database
insert db name tuple =
    Map.update (\table -> Just (fst table, snd table ++ [tuple])) name db
