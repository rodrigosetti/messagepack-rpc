{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.MessagePack
import Network.Simple.TCP
import qualified Data.Map as M

-- | Echo server
main :: IO ()
main =
    withSocketsDo $ runRPC methods HostAny "3000"
  where
    methods = M.singleton "echo" echo
    echo = return . Right

