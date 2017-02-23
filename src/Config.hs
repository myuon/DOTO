{-# LANGUAGE OverloadedStrings #-}
module Config where

import qualified Data.Text as T

type ConnectInfo = T.Text

connInfo :: ConnectInfo
connInfo = "dotoDB.sqlite"
