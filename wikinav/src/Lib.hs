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

type NotifyAPI = "wiki" :> Capture "url" :> POST `[JSON] Frequency

type WordAPI1 = "word" :> Get '[JSON] [Frequency]

type API = NotifyAPI | WordAPI1

type FreqMap = Map String Float

data Frequency = Frequency 
    { wordFreqMap ::  FreqMap
    } deriving (Eq, Show, Generic)

instance ToJSON Frequency

freqmap :: FreqMap 
freqmap = Map.singleton "and" 1

words1 :: [Frequency]
words1 = [ Frequency freqmap]

server1 :: Handler WordAPI1
server1 = return words1

server2 :: String -> Handler NotifyAPI
server2 url = lift $ writeFile "current.txt" url

wordAPI :: Proxy WordAPI1
wordAPI = Proxy

server = server1 :<|> server2

app1 :: Application
app1 = serve wordAPI server1

someFunc :: IO ()
someFunc = run 8081 app1
