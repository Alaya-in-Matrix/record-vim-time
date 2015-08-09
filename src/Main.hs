module Main where
-- Author: lvwenlong_lambda@qq.com
-- Last Modified:CST 2015-08-09 13:35:07 星期日
import Control.Monad
import Text.ParserCombinators.Parsec
import System.Environment
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.List
import Data.List.Split hiding(endBy)
import qualified Data.Map as Map

data VimLogTime = VimLogTime {
    vimLogYear   :: Integer
  , vimLogMonth  :: Int
  , vimLogDay    :: Int
  , vimLogHour   :: Int
  , vimLogMinute :: Int
  , vimLogSecond :: Int
} deriving(Eq, Show)
data VimLogAction    = Create | Open | Write deriving(Eq, Show)
type VimLogGitBranch = String
type VimFileType     = String
data VimLog = VimLog {
    time       :: VimLogTime
  , action     :: VimLogAction
  , editedfile :: FilePath
  , filetype   :: VimFileType
  , gitbranch  :: Maybe VimLogGitBranch
} deriving(Eq)
instance Show VimLog where 
    show (VimLog t a e ft g) = "VimLog {\n    " ++ show t ++ "\n    " ++ show a ++ "\n    " ++ e ++ "\n    " ++ ft ++ "\n    " ++ show g ++ "\n}"

secondOfDay :: VimLogTime -> Integer
secondOfDay t = let h = toInteger $ vimLogHour   t
                    m = toInteger $ vimLogMinute t
                    s = toInteger $ vimLogSecond t
                 in h * 3600 + m * 60 + s

dateToday :: IO String -- (year, month, day)
dateToday = liftM (map replace . showGregorian . utctDay) getCurrentTime
    where replace '-' = '/'
          replace  c   = c

logParser :: Parser VimLog
logParser = VimLog <$> (logTimeParser  <* delimiter)
                   <*> (actionParser   <* delimiter)
                   <*> (filePathParser <* delimiter)
                   <*> (filePathParser <* delimiter)
                   <*> branchParser
   where delimiter = char ';'

logTimeParser :: Parser VimLogTime
logTimeParser = VimLogTime <$> (read <$> many1   digit <* char '-')     -- year
                           <*> (read <$> count 2 digit <* char '-')     -- month
                           <*> (read <$> count 2 digit <* spaces)       -- day
                           <*> (read <$> count 2 digit <* char ':')     -- hour
                           <*> (read <$> count 2 digit <* char ':')     -- minute
                           <*> (read <$> count 2 digit)                 -- second

fileTypeParser :: Parser VimFileType 
fileTypeParser = many1 (noneOf ";") 

actionParser :: Parser VimLogAction
actionParser = (Create <$ string "create")
           <|> (Open   <$ string "open")
           <|> (Write  <$ string "write")

filePathParser :: Parser FilePath
filePathParser = try quotedPath <|> many1 (noneOf ";\n\r")

quotedPath :: Parser FilePath
quotedPath = char '"' *> many1 quotedChar <* char '"'

quotedChar :: Parser Char
quotedChar = try ('"' <$ string "\\\"") <|>  noneOf "\"" <?> "fuck"

branchParser :: Parser (Maybe VimLogGitBranch)
branchParser = (Just <$> filePathParser) <|> (Nothing <$ string "")

eol :: Parser String 
eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\r"
  <|> string "\n"
  <?> "end of line"

logFileParser :: Parser [VimLog]
logFileParser = (logParser `endBy` eol) 


logMap :: [VimLog] -> Map.Map FilePath [(Integer, VimLogAction)]
logMap logs = logMap' (Map.empty) logs
    where logMap' m [] = m
          logMap' m (x:xs) = logMap' m' xs
              where m' = let t = (secondOfDay.time) x
                             a = action x
                             f = editedfile x
                             pair = (t,a)
                          in case (Map.lookup f m) of
                               Nothing  -> Map.insert f (pair:[]) m
                               Just rec -> Map.update (\_ -> Just (pair:rec)) f m


trans2 :: (a->a->b)->(c->a)->(c->c->b)
trans2 func cvt c1 c2 = func (cvt c1) (cvt c2)

duration :: [(Integer,VimLogAction)] -> Integer
duration logs = let sorted  = sortBy (compare `trans2` fst) logs
                    splited = splitWhen ((/= Write) . snd) sorted
               in sum $ map calcDura splited
                   where calcDura [] = 0
                         calcDura xs = (fst $ last xs) - (fst $ head xs)

main :: IO ()
main = do
    path   <- (foldr (++) "") <$> (sequence [liftM head getArgs , pure "/" ,dateToday ,pure ".log"])
    parsed <- parseFromFile logFileParser path 
    case parsed of
      Left errMsg -> putStrLn $ show errMsg
      Right val   -> let lm = logMap $ reverse val
                         dm = Map.map duration lm
                         vimtime = timeToTimeOfDay $ secondsToDiffTime $ foldr (+) 0 $  map snd (Map.toList dm)
                      in putStrLn $ "Time you spent on VIM today: " ++ show vimtime

