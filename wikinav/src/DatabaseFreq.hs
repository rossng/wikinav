{-# LANGUAGE OverloadedStrings #-}

module DatabaseFreq 
where

import           Control.Applicative
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import Hakyll.Web.Html
import Data.List.Split (splitOn)
import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson
import Data.Either
import Data.Char
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8

data WordField = WordField T.Text Int deriving (Show)

instance FromRow WordField where
  fromRow = WordField <$> field <*> field

instance ToRow WordField where
  toRow (WordField word freq) = toRow (word, freq)

createTable :: IO ()
createTable = do
  conn <- open "words.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS words (word TEXT PRIMARY KEY, freq INTEGER)"
  close conn

insertTestData :: IO ()
insertTestData = do conn <- open "words.db"
                    addWordFreq "and" 5 conn
                    close conn

printAllData :: IO ()
printAllData = do conn <- open "words.db"
                  r <- query_ conn "SELECT * from words" :: IO [WordField]
                  mapM_ print r
                  close conn

deleteWord :: String -> IO ()
deleteWord s = do conn <- open "words.db"
                  execute conn "DELETE FROM words WHERE word = ?" (Only (s :: String))
                  close conn

addWordFreq :: T.Text -> Int  -> Connection -> IO ()
addWordFreq word val conn = do
  execute conn "INSERT OR IGNORE INTO words (word, freq) VALUES (?, 0)" (Only (word :: T.Text))
  executeNamed conn "UPDATE words SET freq = freq + :freq WHERE word = :word" [":freq" := val, ":word" := (word :: T.Text)]

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
