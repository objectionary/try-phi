{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Main(main) where

import EO.Test(test, test')

main :: IO ()
main = do 
    test'
    test "./src/EO/snippet.eo"
    print "ok"
