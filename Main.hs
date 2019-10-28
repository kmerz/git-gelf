{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import           Control.Applicative
import           Control.Monad
import           Data.Dates
import           Data.Either                (rights)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec            hiding (State)
import qualified Text.Megaparsec            as Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void Text

data GitLogEntry = GitLogEntry
  { gitHash :: String
  , authorName :: String
  , authorEmail :: String
  , authorDate :: DateTime 
  , subject :: String
  } deriving (Show)

data GelfGitLog = GelfGitLog
  { version :: String,
    host :: String,
    short_message :: String,
    timestamp :: 
  }

parseEmail :: Parser String
parseEmail = Megaparsec.some (alphaNumChar <|> char '@' <|> char '.')

parseNameOrEmail :: Parser String
parseNameOrEmail = Megaparsec.some (alphaNumChar <|> char ' ') <|> parseEmail

-- strict ISO 8601 Time: 2017-06-18T12:08:41+01:00
parseISOTime :: Parser DateTime
parseISOTime = do
    year <- Megaparsec.some digitChar
    void (char '-')
    month <- Megaparsec.some digitChar
    void (char '-')
    day <- Megaparsec.some digitChar
    void (char 'T')
    hour <- Megaparsec.some digitChar
    void (char ':')
    minute <- Megaparsec.some digitChar
    void (char ':')
    second <- Megaparsec.some digitChar
    void (char '+')
    _ <- Megaparsec.some digitChar
    void (char ':')
    _ <- Megaparsec.some digitChar
    return DateTime
      { year = read year
      , month = read month
      , day = read day
      , hour = read hour
      , minute = read minute
      , second = read second
      }

-- git log --pretty=format:'%H%%%an%%%ae%%%aI%%%s'
parseGitLog :: Parser GitLogEntry
parseGitLog = do
  gitHash <- Megaparsec.some alphaNumChar
  void (char '%')
  authorName <- parseNameOrEmail
  void (char '%')
  authorEmail <- parseEmail
  void (char '%')
  authorDate <- parseISOTime --Megaparsec.some alphaNumChar
  void (char '%')
  subject <- Megaparsec.some printChar
  return GitLogEntry{..}

emptyDateTime :: DateTime
emptyDateTime = DateTime
  { year = 0
  , month = 0
  , day = 0
  , hour = 0
  , minute = 0
  , second = 0
  }

emptyGitLogEntry :: GitLogEntry
emptyGitLogEntry = GitLogEntry
  { gitHash = "empty"
  , authorName = "empty"
  , authorDate = emptyDateTime
  , authorEmail = "empty"
  , subject = ""
  }

fromRight :: b -> Either a b -> b
fromRight defaultValue either = case either of
    Left _ -> defaultValue
    Right val -> val

parseContents :: String -> [GitLogEntry]
parseContents gitLogs = rights $ map (parse parseGitLog "" . T.pack) (lines gitLogs)

main = do
    gitLogs <- getContents
    let y = parseContents gitLogs
    mapM (print . show)  y
