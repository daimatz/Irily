module Main where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.State
import           Prelude             hiding (print)

import           Database.Irily

main :: IO ()
main = do
    runDB action newDB
    return ()

action :: DBAccess ()
action = do
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
    insert "shohin"
        [ VInt 4
        , VText "lemon"
        , VInt 1
        , VInt 100
        ]
    insert "shohin"
        [ VInt 5
        , VText "pencil"
        , VNull
        , VInt 60
        ]
    insert "shohin"
        [ VInt 6
        , VText "USB memory"
        , VInt 3
        , VInt 500
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

    liftIO . print
        =<< selectAll
        <$> (leftJoin "kubun_id"
            <$> (selectAll <$> from "shohin")
            <*> (selectAll <$> from "kubun")
            )
