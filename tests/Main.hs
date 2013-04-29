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
            [ Just "1"
            , Just "apple"
            , Just "1"
            , Just"300"
            ]
        db05 = insert db04 "shohin"
            [ Just "2"
            , Just "orange"
            , Just "1"
            , Just "130"
            ]
        db06 = insert db05 "shohin"
            [ Just "3"
            , Just "cabbage"
            , Just "2"
            , Just "200"
            ]
        db07 = insert db06 "kubun"
            [ Just "1"
            , Just "fruits"
            ]
        db08 = insert db07 "kubun"
            [ Just "2"
            , Just "vegetables"
            ]
    print db08
    print $ select ["shohin_name", "kubun_id"] <$> from db08 "shohin"
