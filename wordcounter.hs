{-# LANGUAGE OverloadedStrings #-}

import Hakyll.Web.Html
import Data.List.Split (splitOn)
import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson
import Data.Either
import Data.Char
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8

url = "https://en.wikipedia.org/w/api.php?action=parse&prop=text&format=json&page=Google"

getJSON :: IO B8.ByteString
getJSON = simpleHttp url

newtype Content = Content { content :: String }

instance FromJSON Content where
  parseJSON = withObject ""  $ \o -> do parse <- o .: "parse"
                                        text <- parse .: "text"
                                        Content <$> text .: "*"

processArticle :: B8.ByteString -> Map.Map String Int
processArticle = frequencies . words . removePunctuation . removeBrackets . stripTags . preRefs . either (const "") id . fmap content . eitherDecode

preRefs :: String -> String
preRefs = head . splitOn "id=\"References\""

removePunctuation :: String -> String
removePunctuation = filter (`notElem` ("(),.?!:;\'\"" :: String))

removeBrackets :: String -> String
removeBrackets ('[':str) = skipToClose str
removeBrackets (c:str) = c : removeBrackets str
removeBrackets [] = []

skipToClose :: String -> String
skipToClose (']':str) = removeBrackets str
skipToClose (c:str) = skipToClose str
skipToClose [] = []

frequencies :: [String] -> Map.Map String Int
frequencies = foldr (\word -> Map.insertWith (+) (toLower <$> word) 1) Map.empty
