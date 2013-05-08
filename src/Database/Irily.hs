module Database.Irily
    where

import           Control.Applicative ((<$>))
import           Control.Monad.State
import           Data.List           (elemIndex)
import           Data.Map            (Map, (!))
import qualified Data.Map            as Map
import           Data.Maybe          (fromJust, mapMaybe)
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           Prelude             hiding (print)

type Relation = ([Text], [Tuple])
type Table = Relation
type Database = Map Text Table
type Tuple = [Value]
type Filter = Relation -> Relation

data Value
    = VInt  Int
    | VText Text
    | VNull
  deriving (Show, Read, Eq, Ord)

type DBAccess a = StateT Database IO a

runDB :: DBAccess a -> Database -> IO Database
runDB action db = snd <$> runStateT action db

newDB :: Database
newDB = Map.fromList []

create :: Text -> [Text] -> DBAccess ()
create tableName columnNames = do
    db <- get
    put $ Map.insert tableName (columnNames, []) db

from :: Text -> DBAccess Relation
from name = (! name) <$> get

innerJoin :: Text -> Relation -> Relation -> Relation
innerJoin column left right =
    let newColumns = fst left ++ fst right
        li         = fromJust $ elemIndex column $ fst left
        ri         = fromJust $ elemIndex column $ fst right
        newTuples  = [ l ++ r
            | l <- snd left, r <- snd right,
                l !! li == r !! ri
            ]
    in
        ( newColumns, newTuples )

leftJoin :: Text -> Relation -> Relation -> Relation
leftJoin column left right =
    let newColumns = fst left ++ fst right
    in
        case elemIndex column $ fst right of
        Nothing ->
            ( newColumns
            , map (++ replicate (length $ fst right) VNull) (snd left)
            )
        Just ri ->
            let newTuples = map (f ri) (snd left)
            in
                ( newColumns, newTuples )
  where
    li = fromJust $ elemIndex column $ fst left
    f ri l =
        if any (\r -> l !! li == r !! ri) (snd right) then
            join [ l ++ r | r <- snd right, l !! li == r !! ri ]
        else
            l ++ replicate (length $ fst right) VNull


selectAll :: Relation -> Relation
selectAll = id

select :: [Text] -> Relation -> Relation
select columns relation =
    let idxs = mapMaybe (flip elemIndex $ fst relation) columns
    in
        ( columns
        , f idxs $ snd relation
        )
  where
    f :: [Int] -> [Tuple] -> [Tuple]
    f idxs = map $ \tuple -> map (tuple !!) idxs

insert :: Text -> Tuple -> DBAccess ()
insert name tuple = do
    db <- get
    put $ Map.update (\table -> Just (fst table, snd table ++ [tuple])) name db

mkFilter :: Text -> (Value -> Bool) -> Filter
mkFilter column p relation =
    let idx = fromJust $ elemIndex column $ fst relation
    in
        ( fst relation
        , filter (\tuple -> p (tuple !! idx)) $ snd relation
        )

(.<.) :: Text -> Value -> Filter
column .<. value = mkFilter column $ \v -> int v < int value
  where
    int (VInt x) = x
    int _        = error "not int"

(.=.) :: Text -> Value -> Filter
column .=. value = mkFilter column $ \v -> v == value

print :: Relation -> IO ()
print relation =
    let h = foldl1 (concatWith "|") $ fst relation
        t = foldl1 (concatWith "\n") $
            map (foldl1 (concatWith "|") . map value) $
            snd relation
    in
        TIO.putStrLn $ h <> "\n" <> t <> "\n"
  where
    concatWith sep x y = x <> sep <> y
    value (VInt x)  = textShow x
    value (VText x) = x
    value VNull     = "null"
    textShow = T.pack . show
