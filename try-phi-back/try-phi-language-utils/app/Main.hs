{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Main(main) where

import Test(test, test')

main :: IO ()
main = do 
    test'
    test "./test/data/snippet.eo"
    print "ok"
