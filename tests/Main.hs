{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative ((<$>))
import           Control.Monad.State

import           Database.Irily

main :: IO ()
main = do
    let db = snd $ runState database newDB
    print $ select ["shohin_name", "kubun_id"]
        <$> from db "shohin"
    print $ select ["shohin_name"]
        <$> lessThan "price" 250
        <$> from db "shohin"

database :: State Database ()
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
