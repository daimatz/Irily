{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative ((<$>))
import           Control.Monad.State

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

    r1 <- selectAll
        <$> from "shohin"
    liftIO $ print r1

    r2 <- select ["shohin_name"]
        <$> "price" .<. VInt 250
        <$> from "shohin"
    liftIO $ print r2

    r3 <- select ["shohin_name"]
        <$> "kubun_id" .=. VInt 1
        <$> (selectAll
            <$> from "shohin"
            )
    liftIO $ print r3
