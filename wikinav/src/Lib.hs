{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( someFunc
    ) where


import Prelude ()
import Prelude.Compat
import Data.Map.Lazy as Map
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html


type WordAPI1 = "word" :> Get '[JSON] [Frequency]

type FreqMap = Map String Float

data Frequency = Frequency 
    { wordFreqMap ::  FreqMap
    } deriving (Eq, Show, Generic)

instance ToJSON Frequency

freqmap :: FreqMap 
freqmap = Map.singleton "and" 1

words1 :: [Frequency]
words1 = [ Frequency freqmap]

server1 :: Server WordAPI1
server1 = return words1

wordAPI :: Proxy WordAPI1
wordAPI = Proxy

app1 :: Application
app1 = serve wordAPI server1

someFunc :: IO ()
someFunc = run 8081 app1
