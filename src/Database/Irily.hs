module Database.Irily
    where

import           Control.Monad.State
import           Data.List           (elemIndex)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Maybe          (fromJust, mapMaybe)
import           Data.Text           (Text)

type Relation = ([Text], [Tuple])
type Table = Relation
type Database = Map Text Table
type Tuple = [Value]

data Value
    = VInt  Int
    | VText Text
    | VNull
  deriving (Show, Read, Eq, Ord)

newDB :: Database
newDB = Map.fromList []

create :: Text -> [Text] -> State Database ()
create tableName columnNames = state $ \db ->
    ( ()
    , Map.insert tableName (columnNames, []) db
    )

from :: Database -> Text -> Maybe Relation
from db name = Map.lookup name db

select :: [Text] -> Relation -> Relation
select columns relation =
    let idxs = mapMaybe (flip elemIndex $ fst relation) columns
    in  ( columns
        , f idxs $ snd relation
        )
  where
    f :: [Int] -> [Tuple] -> [Tuple]
    f idxs = map $ \tuple -> map (g tuple) idxs
    g :: Tuple -> Int -> Value
    g tuple idx = tuple !! idx

lessThan :: Text -> Int -> Relation -> Relation
lessThan column value relation =
    let idx = fromJust $ elemIndex column $ fst relation
    in  ( fst relation
        , filter (\ts -> int (ts !! idx) < value) $ snd relation
        )
  where
    int (VInt x) = x
    int _        = error "not int"

insert :: Text -> Tuple -> State Database ()
insert name tuple = state $ \db ->
    ( ()
    , Map.update (\table -> Just (fst table, snd table ++ [tuple])) name db
    )
