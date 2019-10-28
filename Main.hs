{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.Dates
import           Data.Either                (rights)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void
import           GHC.Generics
import           Network.HostName
import           Network.Socket
import           Network.Socket.ByteString.Lazy (send)
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
  , authorTimestamp :: String
  , subject :: String
  } deriving (Show)

data GelfGitLog = GelfGitLog
  { version :: String
  , host :: String
  , short_message :: String
  , timestamp :: String
  , _hash :: String
  , _author :: String
  , _email :: String
  } deriving (Show, Generic)

instance ToJSON GelfGitLog

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

-- git log --pretty=format:'%H%%%an%%%ae%%%aI%%%at%%%s'
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
  authorTimestamp <- Megaparsec.some digitChar
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
  , authorTimestamp = "empty"
  , subject = ""
  }

fromRight :: b -> Either a b -> b
fromRight defaultValue either = case either of
    Left _ -> defaultValue
    Right val -> val

parseContents :: String -> [GitLogEntry]
parseContents gitLogs = rights $ map (parse parseGitLog "" . T.pack) (lines gitLogs)

convertToGelf :: String -> GitLogEntry -> GelfGitLog
convertToGelf hostName gitLogEntry = GelfGitLog
  { version = "1.1"
  , host = hostName
  , short_message = subject gitLogEntry
  , timestamp = authorTimestamp gitLogEntry
  , _hash = gitHash gitLogEntry
  , _author = authorName gitLogEntry
  , _email = authorEmail gitLogEntry }

main = do
    gitLogs <- getContents
    hostName <- getHostName
    let gitLogEntries = parseContents gitLogs
--    mapM (print) $ (map encode) (map (convertToGelf hostName) y)
    withSocketsDo $ do
      (server:_) <- getAddrInfo Nothing (Just "172.30.200.208") (Just "5555")
      s <-socket (addrFamily server) Datagram defaultProtocol
      connect s (addrAddress server)
      mapM_ ((send s . encode) . convertToGelf hostName) gitLogEntries
      close s
