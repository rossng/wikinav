{-# LANGUAGE OverloadedStrings #-}

module DatabaseFreq 
where

import Prelude hiding ((!!))
import           Control.Applicative
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

import Hakyll.Web.Html
import Data.List.Split (splitOn)
import Data.List.Safe ((!!))
import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson
import Data.Key (forWithKeyM_)
import Data.Either (either)
import Data.Maybe (mapMaybe)
import Data.Char
import Control.Monad (forM, forM_)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.Map.Strict as Map
import Text.Regex.PCRE


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

clearDB :: IO ()
clearDB = do
  conn <- open "words.db"
  execute_ conn "DELETE FROM words"
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

url id = "https://en.wikipedia.org/w/api.php?action=parse&prop=text&format=json&pageid=" ++ show id

getJSON :: Int -> IO B8.ByteString
getJSON id = simpleHttp (url id)

newtype Content = Content { content :: String }

instance FromJSON Content where
  parseJSON = withObject ""  $ \o -> do parse <- o .: "parse"
                                        text <- parse .: "text"
                                        Content <$> text .: "*"

articleWords :: B8.ByteString -> [String]
articleWords = words . removePunctuation . removeBrackets . stripTags . preRefs . either (const "") id . fmap content . eitherDecode

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

pattern :: String
pattern = "\"pageid\":\\s*(\\d+)"

links :: String -> [Int]
links str = read <$> mapMaybe (!! 1) (str =~ pattern :: [[String]])

getArticleList :: IO [Int]
getArticleList = getArticleListFrom "-||"
  where
    getArticleListFrom :: String -> IO [Int]
    getArticleListFrom cmcontinue = do
      page <- fmap B8.unpack . simpleHttp $ "https://en.wikipedia.org/w/api.php?format=json&action=query&list=categorymembers&cmtype=page&cmlimit=500&cmtitle=Category:Featured_articles&cmcontinue=" ++ cmcontinue
      let contPattern = "\"cmcontinue\":\\s*\"([^\"]+)\"" :: String
      laterPages <- case (page =~ contPattern :: [[String]]) of
        [[_,cont]] -> getArticleListFrom cont
        _ -> return []
      return $ links page ++ laterPages

loadArticleWords :: Int -> IO [String]
loadArticleWords =  fmap articleWords . getJSON

main :: IO ()
main = loadFrom 0

loadFrom :: Int -> IO ()
loadFrom from = do
  articleList <- getArticleList
  drop from (zip [1..] articleList) `forM_` \(i, article) -> do
    freqs <- frequencies <$> loadArticleWords article
    putStr ":"
    conn <- open "words.db"
    withTransaction conn $ do
      freqs `forWithKeyM_` \word freq -> do
        addWordFreq (T.pack word) freq conn
    close conn
    print i
