module Main where

import Control.Monad
import Control.Applicative hiding((<|>),many,optional)
import Text.ParserCombinators.Parsec
import System.Environment
import Data.Default
import Data.Time.Clock
import Data.Time.Calendar

newtype Email   = Email String deriving(Eq, Show)
newtype LogPath = LogPath String deriving(Eq, Show)

getEmail :: Email -> String
getEmail (Email e) = e
getLogPath :: LogPath -> String
getLogPath (LogPath p) = p
instance Default Email where
    def = Email ""
instance Default LogPath where 
    def = LogPath "~/activity"
data Flags = Flags {
    email   :: Email
  , logPath :: LogPath 
} deriving(Eq, Show)

secondOfDay :: VimLogTime -> Integer
secondOfDay t = let h = toInteger $ vimLogHour   t
                    m = toInteger $ vimLogMinute t
                    s = toInteger $ vimLogSecond t
                 in h * 3600 + m * 60 + s

pairFold :: [a] -> [(a,a)]
pairFold []  = []
pairFold [x] = []
pairFold (x:y:xs) = (x,y) : pairFold xs

defMaybe :: (Default a) => Maybe a -> a
defMaybe Nothing  = def
defMaybe (Just v) = v

getFlags :: [(String,String)] -> Flags
getFlags args = let email = defMaybe $ lookup "-e" args
                    path  = defMaybe $ lookup "-d" args
                 in Flags (Email email) (LogPath path)

date :: IO String -- (year, month, day)
date = liftM (map replace . showGregorian . utctDay) getCurrentTime
    where replace '-' = '/'
          replace  c   = c

data VimLogTime = VimLogTime {
    vimLogYear   :: Integer
  , vimLogMonth  :: Int
  , vimLogDay    :: Int
  , vimLogHour   :: Int
  , vimLogMinute :: Int
  , vimLogSecond :: Int
} deriving(Eq, Show)

data VimLogAction = Create | Open | Write deriving(Eq, Show)
type VimLogGitBranch = String

data VimLog = VimLog {
    time       :: VimLogTime
  , action     :: VimLogAction
  , editedfile :: FilePath
  , gitbranch  :: Maybe VimLogGitBranch
} deriving(Eq, Show)

delimiter :: Parser Char
delimiter = char ';'

logParser :: Parser VimLog
logParser = VimLog <$> (logTimeParser  <* delimiter)
                   <*> (actionParser   <* delimiter)
                   <*> (filePathParser <* delimiter)
                   <*> branchParser
logTimeParser :: Parser VimLogTime
logTimeParser = VimLogTime <$> (read <$> many1   digit <* char '-')     -- year
                           <*> (read <$> count 2 digit <* char '-')     -- month
                           <*> (read <$> count 2 digit <* spaces)       -- day
                           <*> (read <$> count 2 digit <* char ':')     -- hour
                           <*> (read <$> count 2 digit <* char ':')     -- minute
                           <*> (read <$> count 2 digit)                 -- second

actionParser :: Parser VimLogAction
actionParser = (Create <$ string "create")
           <|> (Open   <$ string "open")
           <|> (Write  <$ string "write")

filePathParser :: Parser FilePath
filePathParser = try quotedPath <|> many1 (noneOf ";\n\r")

quotedPath :: Parser FilePath
quotedPath = do
    char '"'
    path <- many1 quotedChar
    char '"'
    return path

quotedChar :: Parser Char
quotedChar = try ('$' <$ string "\\\"") <|>  noneOf "\"" <?> "fuck"


branchParser :: Parser (Maybe VimLogGitBranch)
branchParser = (Just <$> filePathParser) <|> (Nothing <$ string "")


eol :: Parser String 
eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\r"
  <|> string "\n"
  <?> "end of line"

logFileParser = (logParser `endBy` eol) 

main = do
    args       <- liftM pairFold getArgs
    date       <- date
    let flags = getFlags args
        logRootDir = (getLogPath . logPath) flags
        path       = logRootDir ++ "/" ++ date ++ ".log"
    parsed <- parseFromFile logFileParser path 
    case parsed of
      Left errMsg -> putStrLn $ show errMsg
      Right val   -> mapM_ (putStrLn . show) val
