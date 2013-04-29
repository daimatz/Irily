{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative ((<$>))

import           Database.Irily

main :: IO ()
main = do
    let db01 = newDB
        db02 = create db01 "shohin" [
            "shohin_id", "shohin_name", "kubun_id", "price"
            ]
        db03 = create db02 "kubun" [
            "kubun_id", "kubun_name"
            ]
        db04 = insert db03 "shohin"
            [ VInt 1
            , VText "apple"
            , VInt 1
            , VInt 300
            ]
        db05 = insert db04 "shohin"
            [ VInt 2
            , VText "orange"
            , VInt 1
            , VInt 130
            ]
        db06 = insert db05 "shohin"
            [ VInt 3
            , VText "cabbage"
            , VInt 2
            , VInt 200
            ]
        db07 = insert db06 "kubun"
            [ VInt 1
            , VText "fruits"
            ]
        db08 = insert db07 "kubun"
            [ VInt 2
            , VText "vegetables"
            ]
    print db08
    print $ select ["shohin_name", "kubun_id"] <$> from db08 "shohin"
    print $ select ["shohin_name"]
        <$> lessThan "price" 250
        <$> from db08 "shohin"
