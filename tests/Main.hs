module Main where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.State
import           Prelude             hiding (print)

import           Database.Irily

main :: IO ()
main = do
    runStateT database newDB
    return ()

database :: DBAccess ()
database = do
    create "shohin" [
        "shohin_id", "shohin_name", "kubun_id", "price"
        ]
    create "kubun" [
        "kubun_id", "kubun_name"
        ]
    insert "shohin"
        [ VInt 1
        , VText "apple"
        , VInt 1
        , VInt 300
        ]
    insert "shohin"
        [ VInt 2
        , VText "orange"
        , VInt 1
        , VInt 130
        ]
    insert "shohin"
        [ VInt 3
        , VText "cabbage"
        , VInt 2
        , VInt 200
        ]
    insert "kubun"
        [ VInt 1
        , VText "fruits"
        ]
    insert "kubun"
        [ VInt 2
        , VText "vegetables"
        ]

    liftIO . print
        =<< selectAll
        <$> from "shohin"

    liftIO . print
        =<< selectAll <$> from "kubun"

    liftIO . print
        =<< select ["shohin_name", "price"]
        <$> from "shohin"

    liftIO . print
        =<< selectAll
        <$> "price" .<. VInt 250
        <$> from "shohin"

    liftIO . print
        =<< selectAll
        <$> (innerJoin "kubun_id"
            <$> (selectAll <$> from "shohin")
            <*> (selectAll <$> from "kubun")
            )
